module DynamicStrategicInvestment

using LinearAlgebra

export Calibration, LossWeights, Model,
       model_matrices, stage_loss,
       solve_feedback, solve_cooperation, solve_open_loop,
       solve_stationary_multistart, simulate_feedback, simulate_open_loop,
       evaluate_path, two_stage_diagnostics

"""Estimated/calibrated recursive Australian macroeconomic block."""
Base.@kwdef struct Calibration
    rho_x::Float64 = 0.6287494507727328
    rho_pi::Float64 = 0.5625497798601921
    sigma::Float64 = 0.036948002726257784
    fiscal_multiplier::Float64 = 0.051421355523204385
    kappa::Float64 = 0.15510594039123907
    rho_b::Float64 = 0.995
    debt_from_interest::Float64 = 0.015
    debt_from_fiscal::Float64 = 0.060
    debt_from_inflation::Float64 = 0.100
    rho_d::Float64 = 0.0
    rho_u::Float64 = 0.0
end

"""
Primitive one-period loss weights.  These never change across equilibrium
concepts.  This is important: lagged policy choices are the strategic
investments; the adjustment-cost coefficients are not choice variables.
"""
Base.@kwdef struct LossWeights
    inflation::Float64
    output::Float64
    debt::Float64
    monetary_level::Float64
    fiscal_level::Float64
    monetary_change::Float64
    fiscal_change::Float64
    discount::Float64 = 0.99
end

Base.@kwdef struct Model
    calibration::Calibration
    monetary::LossWeights
    fiscal::LossWeights
    social::LossWeights
end

"""
Construct

    s[t+1] = A*s[t] + Bm*m[t] + Bf*f[t]

for `s = [x_lag, pi_lag, debt, m_lag, f_lag, demand_shock,
cost_push_shock]`.  Positive `m` is monetary tightening; positive `f` is a
fiscal expansion.  The first three entries of `s[t+1]` are current output,
inflation and end-of-period debt.
"""
function model_matrices(c::Calibration)
    n = 7
    outcomes = zeros(3, n + 2)
    mi, fi = n + 1, n + 2

    # Hybrid IS relation.
    outcomes[1, 1] = c.rho_x
    outcomes[1, 2] = c.sigma
    outcomes[1, 6] = 1.0
    outcomes[1, mi] = -c.sigma
    outcomes[1, fi] = c.fiscal_multiplier

    # Hybrid Phillips relation.
    outcomes[2, :] .= c.kappa .* outcomes[1, :]
    outcomes[2, 2] += c.rho_pi
    outcomes[2, 7] += 1.0

    # Debt accumulation.
    outcomes[3, 3] = c.rho_b
    outcomes[3, mi] += c.debt_from_interest
    outcomes[3, fi] += c.debt_from_fiscal
    outcomes[3, :] .-= c.debt_from_inflation .* outcomes[2, :]

    transition = zeros(n, n + 2)
    transition[1:3, :] .= outcomes
    transition[4, mi] = 1.0       # today's monetary choice is tomorrow's lag
    transition[5, fi] = 1.0       # today's fiscal choice is tomorrow's lag
    transition[6, 6] = c.rho_d
    transition[7, 7] = c.rho_u
    return transition[:, 1:n], transition[:, mi], transition[:, fi], outcomes
end

"""Quadratic stage loss in `[state; monetary control; fiscal control]`."""
function stage_loss(w::LossWeights, c::Calibration)
    _, _, _, outcomes = model_matrices(c)
    n = 7
    mi, fi = n + 1, n + 2
    map = zeros(7, n + 2)
    map[1:3, :] .= outcomes
    map[4, mi] = 1.0
    map[5, fi] = 1.0
    map[6, 4] = -1.0
    map[6, mi] = 1.0
    map[7, 5] = -1.0
    map[7, fi] = 1.0
    weights = Diagonal([
        w.output, w.inflation, w.debt,
        w.monetary_level, w.fiscal_level,
        w.monetary_change, w.fiscal_change,
    ])
    return Matrix(Symmetric(map' * weights * map))
end

"""Finite-horizon subgame-perfect feedback Nash equilibrium by backward induction."""
function solve_feedback(model::Model, horizon::Int)
    horizon >= 1 || error("horizon must be positive")
    A, bm, bf, _ = model_matrices(model.calibration)
    Bm, Bf = reshape(bm, :, 1), reshape(bf, :, 1)
    Qm = stage_loss(model.monetary, model.calibration)
    Qf = stage_loss(model.fiscal, model.calibration)
    n = size(A, 1)
    z, mi, fi = 1:n, n + 1, n + 2
    Pm_next, Pf_next = zeros(n, n), zeros(n, n)
    km = Vector{Vector{Float64}}(undef, horizon)
    kf = Vector{Vector{Float64}}(undef, horizon)
    pm = Vector{Matrix{Float64}}(undef, horizon)
    pf = Vector{Matrix{Float64}}(undef, horizon)
    transition = Vector{Matrix{Float64}}(undef, horizon)

    for t in horizon:-1:1
        response = [
            Qm[mi, mi] + model.monetary.discount * dot(Bm, Pm_next * Bm)  Qm[mi, fi] + model.monetary.discount * dot(Bm, Pm_next * Bf)
            Qf[fi, mi] + model.fiscal.discount * dot(Bf, Pf_next * Bm)     Qf[fi, fi] + model.fiscal.discount * dot(Bf, Pf_next * Bf)
        ]
        abs(det(response)) > 1e-12 || error("singular stage game at t=$t")
        state_term = vcat(
            reshape(Qm[mi, z] + model.monetary.discount * vec(Bm' * Pm_next * A), 1, :),
            reshape(Qf[fi, z] + model.fiscal.discount * vec(Bf' * Pf_next * A), 1, :),
        )
        rules = -(response \ state_term)
        km[t], kf[t] = vec(rules[1, :]), vec(rules[2, :])
        acl = A + Bm * km[t]' + Bf * kf[t]'
        selector = vcat(Matrix{Float64}(I, n, n), rules)
        pm[t] = Matrix(Symmetric(selector' * Qm * selector + model.monetary.discount * acl' * Pm_next * acl))
        pf[t] = Matrix(Symmetric(selector' * Qf * selector + model.fiscal.discount * acl' * Pf_next * acl))
        transition[t] = Matrix(acl)
        Pm_next, Pf_next = pm[t], pf[t]
    end
    return (monetary_rule=km, fiscal_rule=kf, monetary_value=pm,
            fiscal_value=pf, transition=transition, horizon=horizon)
end

"""Finite-horizon cooperative feedback solution under the fixed social loss."""
function solve_cooperation(model::Model, horizon::Int)
    A, bm, bf, _ = model_matrices(model.calibration)
    B = hcat(bm, bf)
    Q = stage_loss(model.social, model.calibration)
    n = size(A, 1)
    z, u = 1:n, (n + 1):(n + 2)
    pnext = zeros(n, n)
    km = Vector{Vector{Float64}}(undef, horizon)
    kf = Vector{Vector{Float64}}(undef, horizon)
    pv = Vector{Matrix{Float64}}(undef, horizon)
    transition = Vector{Matrix{Float64}}(undef, horizon)
    for t in horizon:-1:1
        rules = -((Q[u, u] + model.social.discount * B' * pnext * B) \
                  (Q[u, z] + model.social.discount * B' * pnext * A))
        km[t], kf[t] = vec(rules[1, :]), vec(rules[2, :])
        acl = A + B * rules
        selector = vcat(Matrix{Float64}(I, n, n), rules)
        pv[t] = Matrix(Symmetric(selector' * Q * selector + model.social.discount * acl' * pnext * acl))
        transition[t] = Matrix(acl)
        pnext = pv[t]
    end
    return (monetary_rule=km, fiscal_rule=kf, value=pv,
            transition=transition, horizon=horizon)
end

"""Build a complete-path quadratic objective for the recursive state system."""
function path_objective(Q::Matrix{Float64}, discount::Float64,
                        A, bm, bf, initial_state, horizon::Int)
    n = length(initial_state)
    controls = 2 * horizon
    base = Float64.(initial_state)
    loading = zeros(n, controls)
    H = zeros(controls, controls)
    h = zeros(controls)
    constant = 0.0
    for t in 1:horizon
        selector = zeros(n + 2, controls)
        selector[1:n, :] .= loading
        selector[n + 1, t] = 1.0
        selector[n + 2, horizon + t] = 1.0
        intercept = vcat(base, 0.0, 0.0)
        weight = discount^(t - 1)
        H .+= weight .* (selector' * Q * selector)
        h .+= weight .* (selector' * Q * intercept)
        constant += weight * dot(intercept, Q * intercept)
        base = A * base
        loading = A * loading
        loading[:, t] .+= bm
        loading[:, horizon + t] .+= bf
    end
    return Matrix(Symmetric(H)), h, constant
end

"""
Complete-path open-loop Nash equilibrium on exactly the same equations and
with exactly the same primitive losses as the feedback game.
"""
function solve_open_loop(model::Model, initial_state, horizon::Int)
    A, bm, bf, _ = model_matrices(model.calibration)
    Qm = stage_loss(model.monetary, model.calibration)
    Qf = stage_loss(model.fiscal, model.calibration)
    Hm, hm, _ = path_objective(Qm, model.monetary.discount, A, bm, bf, initial_state, horizon)
    Hf, hf, _ = path_objective(Qf, model.fiscal.discount, A, bm, bf, initial_state, horizon)
    rows = vcat(Hm[1:horizon, :], Hf[(horizon + 1):(2horizon), :])
    rhs = -vcat(hm[1:horizon], hf[(horizon + 1):(2horizon)])
    choices = rows \ rhs
    return (monetary=choices[1:horizon], fiscal=choices[(horizon + 1):(2horizon)],
            condition_number=cond(rows), horizon=horizon)
end

"""Simulate time-varying feedback rules from an initial state."""
function simulate_feedback(solution, model::Model, initial_state)
    A, bm, bf, _ = model_matrices(model.calibration)
    horizon = solution.horizon
    state = Float64.(initial_state)
    states = zeros(length(state), horizon + 1)
    states[:, 1] .= state
    monetary, fiscal = zeros(horizon), zeros(horizon)
    for t in 1:horizon
        monetary[t] = dot(solution.monetary_rule[t], state)
        fiscal[t] = dot(solution.fiscal_rule[t], state)
        state = A * state + bm * monetary[t] + bf * fiscal[t]
        states[:, t + 1] .= state
    end
    return (states=states, output=vec(states[1, 2:end]), inflation=vec(states[2, 2:end]),
            debt=vec(states[3, 2:end]), monetary=monetary, fiscal=fiscal)
end

"""Simulate a precomputed complete policy path."""
function simulate_open_loop(solution, model::Model, initial_state)
    A, bm, bf, _ = model_matrices(model.calibration)
    state = Float64.(initial_state)
    states = zeros(length(state), solution.horizon + 1)
    states[:, 1] .= state
    for t in 1:solution.horizon
        state = A * state + bm * solution.monetary[t] + bf * solution.fiscal[t]
        states[:, t + 1] .= state
    end
    return (states=states, output=vec(states[1, 2:end]), inflation=vec(states[2, 2:end]),
            debt=vec(states[3, 2:end]), monetary=solution.monetary, fiscal=solution.fiscal)
end

"""Evaluate primitive monetary, fiscal and social losses along a path."""
function evaluate_path(path, model::Model)
    losses = Float64[]
    for w in (model.monetary, model.fiscal, model.social)
        Q = stage_loss(w, model.calibration)
        value = 0.0
        for t in eachindex(path.monetary)
            joint = vcat(path.states[:, t], path.monetary[t], path.fiscal[t])
            value += w.discount^(t - 1) * dot(joint, Q * joint)
        end
        push!(losses, value)
    end
    return (monetary=losses[1], fiscal=losses[2], social=losses[3])
end

"""
Exact two-stage strategic-investment diagnostics.  The cross responses show
how a first-stage instrument changes the rival's second-stage action.  The
reported wedges are the rival-response pieces of the stage-one half-gradient.
"""
function two_stage_diagnostics(model::Model, initial_state)
    solution = solve_feedback(model, 2)
    path = simulate_feedback(solution, model, initial_state)
    A, bm, bf, _ = model_matrices(model.calibration)
    Qm = stage_loss(model.monetary, model.calibration)
    Qf = stage_loss(model.fiscal, model.calibration)
    n = length(initial_state)
    z, mi, fi = 1:n, n + 1, n + 2
    km2, kf2 = solution.monetary_rule[2], solution.fiscal_rule[2]
    s2 = path.states[:, 2]

    fiscal_marginal_in_monetary_loss =
        vec(Qm[fi, z]) + Qm[fi, mi] .* km2 + Qm[fi, fi] .* kf2
    monetary_marginal_in_fiscal_loss =
        vec(Qf[mi, z]) + Qf[mi, mi] .* km2 + Qf[mi, fi] .* kf2

    fiscal_response_to_monetary_investment = dot(kf2, bm)
    monetary_response_to_fiscal_investment = dot(km2, bf)
    monetary_strategic_wedge = model.monetary.discount *
        fiscal_response_to_monetary_investment *
        dot(fiscal_marginal_in_monetary_loss, s2)
    fiscal_strategic_wedge = model.fiscal.discount *
        monetary_response_to_fiscal_investment *
        dot(monetary_marginal_in_fiscal_loss, s2)
    monetary_continuation = model.monetary.discount *
        dot(bm, solution.monetary_value[2] * s2)
    fiscal_continuation = model.fiscal.discount *
        dot(bf, solution.fiscal_value[2] * s2)

    return (
        solution=solution,
        path=path,
        fiscal_response_to_monetary_investment=fiscal_response_to_monetary_investment,
        monetary_response_to_fiscal_investment=monetary_response_to_fiscal_investment,
        monetary_strategic_wedge=monetary_strategic_wedge,
        fiscal_strategic_wedge=fiscal_strategic_wedge,
        monetary_continuation=monetary_continuation,
        fiscal_continuation=fiscal_continuation,
    )
end

"""One stationary coupled-Riccati fixed point from a chosen initial value scale."""
function stationary_from_seed(model::Model, seed_scale::Float64;
                              tolerance=1e-11, max_iterations=50_000)
    A, bm, bf, _ = model_matrices(model.calibration)
    Bm, Bf = reshape(bm, :, 1), reshape(bf, :, 1)
    Qm, Qf = stage_loss(model.monetary, model.calibration), stage_loss(model.fiscal, model.calibration)
    n = size(A, 1)
    z, mi, fi = 1:n, n + 1, n + 2
    Pm, Pf = seed_scale .* Matrix(Qm[z, z]), seed_scale .* Matrix(Qf[z, z])
    km, kf = zeros(n), zeros(n)
    for iteration in 1:max_iterations
        response = [
            Qm[mi, mi] + model.monetary.discount * dot(Bm, Pm * Bm)  Qm[mi, fi] + model.monetary.discount * dot(Bm, Pm * Bf)
            Qf[fi, mi] + model.fiscal.discount * dot(Bf, Pf * Bm)     Qf[fi, fi] + model.fiscal.discount * dot(Bf, Pf * Bf)
        ]
        state_term = vcat(
            reshape(Qm[mi, z] + model.monetary.discount * vec(Bm' * Pm * A), 1, :),
            reshape(Qf[fi, z] + model.fiscal.discount * vec(Bf' * Pf * A), 1, :),
        )
        rules = -(response \ state_term)
        km_new, kf_new = vec(rules[1, :]), vec(rules[2, :])
        acl = A + Bm * km_new' + Bf * kf_new'
        selector = vcat(Matrix{Float64}(I, n, n), rules)
        Pm_new = Matrix(Symmetric(selector' * Qm * selector + model.monetary.discount * acl' * Pm * acl))
        Pf_new = Matrix(Symmetric(selector' * Qf * selector + model.fiscal.discount * acl' * Pf * acl))
        err = maximum((maximum(abs, Pm_new - Pm), maximum(abs, Pf_new - Pf),
                       maximum(abs, km_new - km), maximum(abs, kf_new - kf)))
        Pm, Pf, km, kf = Pm_new, Pf_new, km_new, kf_new
        if err < tolerance
            residual = err
            return (converged=true, iterations=iteration, monetary_rule=km,
                    fiscal_rule=kf, monetary_value=Pm, fiscal_value=Pf,
                    transition=Matrix(acl), spectral_radius=maximum(abs, eigvals(acl)),
                    residual=residual, seed_scale=seed_scale)
        end
    end
    acl = A + Bm * km' + Bf * kf'
    return (converged=false, iterations=max_iterations, monetary_rule=km,
            fiscal_rule=kf, monetary_value=Pm, fiscal_value=Pf,
            transition=Matrix(acl), spectral_radius=maximum(abs, eigvals(acl)),
            residual=Inf, seed_scale=seed_scale)
end

"""Search for stationary feedback equilibria from several Riccati initialisations."""
function solve_stationary_multistart(model::Model;
                                     seeds=[0.0, 0.01, 0.1, 1.0, 10.0, 100.0])
    attempts = [stationary_from_seed(model, Float64(seed)) for seed in seeds]
    solutions = NamedTuple[]
    for attempt in attempts
        attempt.converged && attempt.spectral_radius < 1.0 || continue
        duplicate = any(maximum(abs, attempt.monetary_rule - item.monetary_rule) < 1e-7 &&
                        maximum(abs, attempt.fiscal_rule - item.fiscal_rule) < 1e-7
                        for item in solutions)
        duplicate || push!(solutions, attempt)
    end
    return (attempts=attempts, solutions=solutions)
end

end # module

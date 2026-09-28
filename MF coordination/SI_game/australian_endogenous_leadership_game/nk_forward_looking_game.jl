module ForwardLookingNK

using LinearAlgebra
using ..EndogenousLeadership: Mandate, SocialWeights, Regime

export NKCalibration, NKModel, private_reduction, nk_loss,
       solve_nk_nash, solve_nk_cooperation, search_nk_nash,
       simulate_nk, evaluate_nk_path, nk_strategic_wedge

"""Hybrid-to-New-Keynesian structural block."""
Base.@kwdef struct NKCalibration
    rho_x::Float64 = 0.6287494507727328
    rho_pi::Float64 = 0.5625497798601921
    sigma::Float64 = 0.036948002726257784
    fiscal_multiplier::Float64 = 0.051421355523204385
    kappa::Float64 = 0.15510594039123907
    private_discount::Float64 = 0.99
    rho_b::Float64 = 0.995
    debt_from_interest::Float64 = 0.015
    debt_from_fiscal::Float64 = 0.060
    debt_from_inflation::Float64 = 0.100
    rho_d::Float64 = 0.0
    rho_u::Float64 = 0.0
end

Base.@kwdef struct NKModel
    calibration::NKCalibration = NKCalibration()
    monetary::Mandate = Mandate(inflation=1.5, output=0.25, debt=0.15)
    fiscal::Mandate = Mandate(inflation=0.5, output=0.75, debt=0.15)
    social::SocialWeights = SocialWeights()
end

"""
Reduce the private-sector block conditional on expected future equilibrium
outcomes `expectations * s[t+1]`.

`theta=0` exactly reproduces the backward-looking model.  `theta=1` gives

    x[t]  = E x[t+1] - sigma * (m[t] - E pi[t+1]) + psi*f[t] + d[t]
    pi[t] = beta_p * E pi[t+1] + kappa*x[t] + u[t].

The predetermined state remains
`[x_lag, pi_lag, debt, m_lag, f_lag, demand, cost_push]` so the homotopy is
directly comparable across values of `theta`.
"""
function private_reduction(c::NKCalibration, expectations::AbstractMatrix,
                           theta::Real)
    size(expectations) == (2, 7) || error("expectations must be 2x7")
    0.0 <= theta <= 1.0 || error("theta must lie in [0,1]")
    n, q = 7, 9

    # s[t+1] = N*[s[t];m[t];f[t]] + E*[x[t];pi[t]].
    N = zeros(n, q)
    N[3, 3] = c.rho_b
    N[3, 8] = c.debt_from_interest
    N[3, 9] = c.debt_from_fiscal
    N[4, 8] = 1.0
    N[5, 9] = 1.0
    N[6, 6] = c.rho_d
    N[7, 7] = c.rho_u
    E = zeros(n, 2)
    E[1, 1] = 1.0
    E[2, 2] = 1.0
    E[3, 2] = -c.debt_from_inflation

    # Backward-looking parts of the IS curve and Phillips curve.
    R = zeros(2, q)
    R[1, 1] = (1.0 - theta) * c.rho_x
    R[1, 2] = (1.0 - theta) * c.sigma
    R[1, 6] = 1.0
    R[1, 8] = -c.sigma
    R[1, 9] = c.fiscal_multiplier
    R[2, 2] = (1.0 - theta) * c.rho_pi
    R[2, 7] = 1.0

    # Expected output and inflation are linear in s[t+1].
    real_activity_forecast = vec(expectations[1, :] +
                                 c.sigma .* expectations[2, :])
    inflation_forecast = c.private_discount .* vec(expectations[2, :])
    M = [1.0 0.0; -c.kappa 1.0]
    M[1, :] .-= theta .* vec(real_activity_forecast' * E)
    M[2, :] .-= theta .* vec(inflation_forecast' * E)
    R[1, :] .+= theta .* vec(real_activity_forecast' * N)
    R[2, :] .+= theta .* vec(inflation_forecast' * N)
    abs(det(M)) > 1e-10 || error("singular private-sector expectational block")

    outcomes = M \ R
    transition = N + E * outcomes
    return (A=transition[:, 1:n], bm=vec(transition[:, 8]),
            bf=vec(transition[:, 9]), outcomes=outcomes,
            transition=transition, private_matrix=M)
end

"""Quadratic stage-loss matrix for an authority or the social planner."""
function nk_loss(model::NKModel, regime::Regime, reduction, authority::Symbol;
                 include_adjustment=true)
    map = zeros(7, 9)
    map[1:2, :] .= reduction.outcomes
    map[3, :] .= reduction.transition[3, :]
    map[4, 8] = 1.0
    map[5, 9] = 1.0
    map[6, 4] = -1.0
    map[6, 8] = 1.0
    map[7, 5] = -1.0
    map[7, 9] = 1.0

    if authority == :monetary
        w = model.monetary
        diagonal = [w.output, w.inflation, w.debt, w.own_level, 0.0,
                    regime.monetary_adjustment, 0.0]
    elseif authority == :fiscal
        w = model.fiscal
        diagonal = [w.output, w.inflation, w.debt, 0.0, w.own_level,
                    0.0, regime.fiscal_adjustment]
    elseif authority == :social
        w = model.social
        lm = include_adjustment ? regime.monetary_adjustment : 0.0
        lf = include_adjustment ? regime.fiscal_adjustment : 0.0
        diagonal = [w.output, w.inflation, w.debt,
                    w.monetary_level, w.fiscal_level, lm, lf]
    else
        error("authority must be :monetary, :fiscal, or :social")
    end
    return Matrix(Symmetric(map' * Diagonal(diagonal) * map))
end

function make_rule(name, km, kf, reduction; metadata=NamedTuple())
    acl = reduction.A + reduction.bm * km' + reduction.bf * kf'
    return (name=name, monetary_rule=Vector(km), fiscal_rule=Vector(kf),
            transition=Matrix(acl), spectral_radius=maximum(abs, eigvals(acl)),
            metadata=metadata)
end

"""Solve the LQ feedback Nash game for a fixed private-expectations map."""
function solve_fixed_nash(model, regime, reduction;
                          tolerance=1e-11, max_iterations=50_000)
    A, bm, bf = reduction.A, reduction.bm, reduction.bf
    Bm, Bf = reshape(bm, :, 1), reshape(bf, :, 1)
    Qm = nk_loss(model, regime, reduction, :monetary)
    Qf = nk_loss(model, regime, reduction, :fiscal)
    n, z, mi, fi = 7, 1:7, 8, 9
    Pm, Pf = Matrix(Qm[z, z]), Matrix(Qf[z, z])
    km, kf = zeros(n), zeros(n)
    residual = Inf
    for iteration in 1:max_iterations
        response = [
            Qm[mi, mi] + model.monetary.discount * dot(Bm, Pm * Bm)  Qm[mi, fi] + model.monetary.discount * dot(Bm, Pm * Bf)
            Qf[fi, mi] + model.fiscal.discount * dot(Bf, Pf * Bm)    Qf[fi, fi] + model.fiscal.discount * dot(Bf, Pf * Bf)
        ]
        abs(det(response)) > 1e-12 || error("singular policy response block")
        state_terms = vcat(
            reshape(Qm[mi, z] + model.monetary.discount .* vec(Bm' * Pm * A), 1, :),
            reshape(Qf[fi, z] + model.fiscal.discount .* vec(Bf' * Pf * A), 1, :),
        )
        rules = -(response \ state_terms)
        km_new, kf_new = vec(rules[1, :]), vec(rules[2, :])
        acl = A + Bm * km_new' + Bf * kf_new'
        selector = vcat(Matrix{Float64}(I, n, n), rules)
        Pm_new = Matrix(Symmetric(selector' * Qm * selector +
                    model.monetary.discount * acl' * Pm * acl))
        Pf_new = Matrix(Symmetric(selector' * Qf * selector +
                    model.fiscal.discount * acl' * Pf * acl))
        residual = maximum((maximum(abs, Pm_new - Pm), maximum(abs, Pf_new - Pf),
                            maximum(abs, km_new - km), maximum(abs, kf_new - kf)))
        Pm, Pf, km, kf = Pm_new, Pf_new, km_new, kf_new
        if residual < tolerance
            return make_rule(regime.name, km, kf, reduction;
                metadata=(monetary_value=Pm, fiscal_value=Pf, Qm=Qm, Qf=Qf,
                          inner_iterations=iteration, inner_residual=residual,
                          inner_converged=true))
        end
    end
    return make_rule(regime.name, km, kf, reduction;
        metadata=(monetary_value=Pm, fiscal_value=Pf, Qm=Qm, Qf=Qf,
                  inner_iterations=max_iterations, inner_residual=residual,
                  inner_converged=false))
end

"""Solve the cooperative LQ regulator for a fixed expectations map."""
function solve_fixed_cooperation(model, regime, reduction;
                                 tolerance=1e-11, max_iterations=50_000)
    A, B = reduction.A, hcat(reduction.bm, reduction.bf)
    Q = nk_loss(model, regime, reduction, :social)
    z, u = 1:7, 8:9
    P, rules = Matrix(Q[z, z]), zeros(2, 7)
    residual = Inf
    for iteration in 1:max_iterations
        rules_new = -((Q[u, u] + model.social.discount * B' * P * B) \
                      (Q[u, z] + model.social.discount * B' * P * A))
        acl = A + B * rules_new
        selector = vcat(Matrix{Float64}(I, 7, 7), rules_new)
        Pnew = Matrix(Symmetric(selector' * Q * selector +
                      model.social.discount * acl' * P * acl))
        residual = max(maximum(abs, Pnew - P), maximum(abs, rules_new - rules))
        P, rules = Pnew, rules_new
        if residual < tolerance
            return make_rule(regime.name, vec(rules[1, :]), vec(rules[2, :]), reduction;
                metadata=(value=P, Qs=Q, inner_iterations=iteration,
                          inner_residual=residual, inner_converged=true))
        end
    end
    return make_rule(regime.name, vec(rules[1, :]), vec(rules[2, :]), reduction;
        metadata=(value=P, Qs=Q, inner_iterations=max_iterations,
                  inner_residual=residual, inner_converged=false))
end

function equilibrium_expectations(reduction, rule)
    return reduction.outcomes[:, 1:7] +
           reduction.outcomes[:, 8] * rule.monetary_rule' +
           reduction.outcomes[:, 9] * rule.fiscal_rule'
end

"""Rational-expectations stationary Markov-perfect Nash equilibrium."""
function solve_nk_nash(model::NKModel, regime::Regime, theta::Real;
                       initial_expectations=zeros(2, 7), damping=0.25,
                       tolerance=2e-10, max_iterations=5_000)
    expectations = Matrix{Float64}(initial_expectations)
    residual = Inf
    last_rule = nothing
    for iteration in 1:max_iterations
        reduction = private_reduction(model.calibration, expectations, theta)
        rule = solve_fixed_nash(model, regime, reduction)
        rule.metadata.inner_converged || error("inner Nash iteration failed")
        candidate = equilibrium_expectations(reduction, rule)
        residual = maximum(abs, candidate - expectations)
        expectations .= (1.0 - damping) .* expectations .+ damping .* candidate
        last_rule = rule
        if residual < tolerance
            final_reduction = private_reduction(model.calibration, candidate, theta)
            final_rule = solve_fixed_nash(model, regime, final_reduction)
            final_candidate = equilibrium_expectations(final_reduction, final_rule)
            final_residual = maximum(abs, final_candidate - candidate)
            metadata = merge(final_rule.metadata,
                (theta=Float64(theta), expectations=final_candidate,
                 reduction=final_reduction, outer_iterations=iteration,
                 outer_residual=final_residual, outer_converged=final_residual < 20tolerance,
                 regime=regime))
            return make_rule(regime.name, final_rule.monetary_rule,
                             final_rule.fiscal_rule, final_reduction; metadata=metadata)
        end
    end
    reduction = private_reduction(model.calibration, expectations, theta)
    metadata = merge(last_rule.metadata,
        (theta=Float64(theta), expectations=expectations, reduction=reduction,
         outer_iterations=max_iterations, outer_residual=residual,
         outer_converged=false, regime=regime))
    return make_rule(regime.name, last_rule.monetary_rule,
                     last_rule.fiscal_rule, reduction; metadata=metadata)
end

"""Rational-expectations cooperative equilibrium."""
function solve_nk_cooperation(model::NKModel, regime::Regime, theta::Real;
                              initial_expectations=zeros(2, 7), damping=0.25,
                              tolerance=2e-10, max_iterations=5_000)
    expectations = Matrix{Float64}(initial_expectations)
    residual = Inf
    last_rule = nothing
    for iteration in 1:max_iterations
        reduction = private_reduction(model.calibration, expectations, theta)
        rule = solve_fixed_cooperation(model, regime, reduction)
        rule.metadata.inner_converged || error("inner cooperative iteration failed")
        candidate = equilibrium_expectations(reduction, rule)
        residual = maximum(abs, candidate - expectations)
        expectations .= (1.0 - damping) .* expectations .+ damping .* candidate
        last_rule = rule
        if residual < tolerance
            final_reduction = private_reduction(model.calibration, candidate, theta)
            final_rule = solve_fixed_cooperation(model, regime, final_reduction)
            final_candidate = equilibrium_expectations(final_reduction, final_rule)
            final_residual = maximum(abs, final_candidate - candidate)
            metadata = merge(final_rule.metadata,
                (theta=Float64(theta), expectations=final_candidate,
                 reduction=final_reduction, outer_iterations=iteration,
                 outer_residual=final_residual, outer_converged=final_residual < 20tolerance,
                 regime=regime))
            return make_rule(regime.name, final_rule.monetary_rule,
                             final_rule.fiscal_rule, final_reduction; metadata=metadata)
        end
    end
    reduction = private_reduction(model.calibration, expectations, theta)
    metadata = merge(last_rule.metadata,
        (theta=Float64(theta), expectations=expectations, reduction=reduction,
         outer_iterations=max_iterations, outer_residual=residual,
         outer_converged=false, regime=regime))
    return make_rule(regime.name, last_rule.monetary_rule,
                     last_rule.fiscal_rule, reduction; metadata=metadata)
end

"""Search for distinct fixed points from scaled expectations maps."""
function search_nk_nash(model::NKModel, regime::Regime, theta::Real,
                        reference_expectations;
                        scales=[-2.0, -0.5, 0.0, 0.5, 1.0, 2.0, 5.0])
    attempts = NamedTuple[]
    solutions = NamedTuple[]
    for scale in scales
        try
            solution = solve_nk_nash(model, regime, theta;
                initial_expectations=scale .* reference_expectations)
            push!(attempts, solution)
            stable = solution.metadata.outer_converged && solution.spectral_radius < 1.0
            duplicate = any(norm(vcat(solution.monetary_rule - item.monetary_rule,
                                       solution.fiscal_rule - item.fiscal_rule,
                                       vec(solution.metadata.expectations - item.metadata.expectations))) < 1e-6
                            for item in solutions)
            stable && !duplicate && push!(solutions, solution)
        catch
            # A failed or singular seed is recorded by its absence from attempts.
        end
    end
    return (attempts=attempts, solutions=solutions, requested=length(scales))
end

function simulate_nk(rule, initial_state; horizon=40)
    state = Float64.(initial_state)
    states = zeros(7, horizon + 1)
    states[:, 1] .= state
    monetary, fiscal = zeros(horizon), zeros(horizon)
    for t in 1:horizon
        monetary[t] = dot(rule.monetary_rule, state)
        fiscal[t] = dot(rule.fiscal_rule, state)
        state = rule.transition * state
        states[:, t + 1] .= state
    end
    return (states=states, output=vec(states[1, 2:end]),
            inflation=vec(states[2, 2:end]), debt=vec(states[3, 2:end]),
            monetary=monetary, fiscal=fiscal)
end

function evaluate_nk_path(path, model::NKModel, regime::Regime, rule)
    reduction = rule.metadata.reduction
    matrices = (nk_loss(model, regime, reduction, :monetary),
                nk_loss(model, regime, reduction, :fiscal),
                nk_loss(model, regime, reduction, :social),
                nk_loss(model, regime, reduction, :social; include_adjustment=false))
    discounts = (model.monetary.discount, model.fiscal.discount,
                 model.social.discount, model.social.discount)
    values = zeros(4)
    for t in eachindex(path.monetary)
        joint = vcat(path.states[:, t], path.monetary[t], path.fiscal[t])
        for k in 1:4
            values[k] += discounts[k]^(t - 1) * dot(joint, matrices[k] * joint)
        end
    end
    return (monetary=values[1], fiscal=values[2], social=values[3],
            common_macro=values[4])
end

"""Rival-response component of the continuation first-order condition."""
function nk_strategic_wedge(rule, model::NKModel, initial_state, authority::Symbol)
    A = rule.metadata.reduction.A
    bm, bf = rule.metadata.reduction.bm, rule.metadata.reduction.bf
    state0 = Float64.(initial_state)
    m0, f0 = dot(rule.monetary_rule, state0), dot(rule.fiscal_rule, state0)
    state1 = A * state0 + bm * m0 + bf * f0
    m1, f1 = dot(rule.monetary_rule, state1), dot(rule.fiscal_rule, state1)
    state2 = A * state1 + bm * m1 + bf * f1
    joint1 = vcat(state1, m1, f1)
    if authority == :monetary
        Q, P, beta = rule.metadata.Qm, rule.metadata.monetary_value, model.monetary.discount
        rival_h = 2.0 * (dot(Q[9, :], joint1) + beta * dot(bf, P * state2))
        components = beta .* bm .* rule.fiscal_rule .* rival_h
        strategic = sum(components)
        adjustment_state = components[4]
        macro_states = sum(components[1:3])
        total = 2.0 * beta * dot(bm, P * state1)
    elseif authority == :fiscal
        Q, P, beta = rule.metadata.Qf, rule.metadata.fiscal_value, model.fiscal.discount
        rival_h = 2.0 * (dot(Q[8, :], joint1) + beta * dot(bm, P * state2))
        components = beta .* bf .* rule.monetary_rule .* rival_h
        strategic = sum(components)
        adjustment_state = components[5]
        macro_states = sum(components[1:3])
        total = 2.0 * beta * dot(bf, P * state1)
    else
        error("authority must be :monetary or :fiscal")
    end
    nonstrategic = total - strategic
    share = abs(strategic) / (abs(strategic) + abs(nonstrategic) + eps(Float64))
    return (strategic=strategic, nonstrategic=nonstrategic,
            total_continuation=total, bounded_share=share,
            adjustment_state=adjustment_state, macro_states=macro_states,
            channel_components=components)
end

end # module

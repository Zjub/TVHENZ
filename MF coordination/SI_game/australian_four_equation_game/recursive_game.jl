# -----------------------------------------------------------------------------
# Recursive monetary--fiscal game and the ex ante commitment-technology game
# -----------------------------------------------------------------------------

"""
Calibration of the recursive Australian state-space model.

The observed state is `s = [x_lag, pi_lag, b, i_lag, g_lag, d, u]`.  The first
two coefficients are the persistence estimates from the Australian IS and
Phillips equations, while `d` and `u` are the currently observed demand and
cost-push shocks.  The remaining transmission coefficients are shared with the
open-loop model through `economy`.
"""
Base.@kwdef struct RecursiveCalibration
    economy::MacroCalibration
    output_persistence::Float64
    inflation_persistence::Float64
    demand_persistence::Float64 = 0.0
    cost_push_persistence::Float64 = 0.0
end


"""A stationary linear feedback Nash solution, with controls `u = rule*s`."""
struct FeedbackGameResult
    monetary_rule::Vector{Float64}
    fiscal_rule::Vector{Float64}
    transition::Matrix{Float64}
    monetary_value::Matrix{Float64}
    fiscal_value::Matrix{Float64}
    converged::Bool
    iterations::Int
    spectral_radius::Float64
end


"""
Return the recursive state transition and current-outcome map

    s[t+1] = A*s[t] + Bm*i[t] + Bf*g[t] + innovation[t+1].

The estimated Australian persistence coefficients are used in a deliberately
transparent semi-structural recursive closure.  Current policy affects current
output; current output enters inflation; and the resulting inflation affects
end-of-period debt.  This differs from the perfect-foresight private block in
the open-loop benchmark and makes that modelling choice auditable rather than
hiding an expectations approximation in the solver.
"""
function recursive_matrices(c::RecursiveCalibration)
    p = c.economy
    state_count = 7
    joint_count = state_count + 2
    monetary_index = state_count + 1
    fiscal_index = state_count + 2

    # Map [state; i; g] into current x, current pi and end-of-period debt.
    outcomes = zeros(3, joint_count)
    outcomes[1, 1] = c.output_persistence
    outcomes[1, 2] = p.sigma             # lagged inflation lowers the real rate
    outcomes[1, 6] = 1.0                 # observed demand shock
    outcomes[1, monetary_index] = -p.sigma
    outcomes[1, fiscal_index] = p.fiscal_multiplier
    outcomes[2, :] .= p.kappa .* outcomes[1, :]
    outcomes[2, 2] += c.inflation_persistence
    outcomes[2, 7] += 1.0                # observed cost-push shock
    outcomes[3, 3] = p.rho_b
    outcomes[3, monetary_index] += p.debt_from_interest
    outcomes[3, fiscal_index] += p.debt_from_fiscal
    outcomes[3, :] .-= p.debt_from_inflation .* outcomes[2, :]

    transition = zeros(state_count, joint_count)
    transition[1:3, :] .= outcomes
    transition[4, monetary_index] = 1.0
    transition[5, fiscal_index] = 1.0
    transition[6, 6] = c.demand_persistence
    transition[7, 7] = c.cost_push_persistence
    A = Matrix(transition[:, 1:state_count])
    Bm = Vector(transition[:, monetary_index])
    Bf = Vector(transition[:, fiscal_index])
    return A, Bm, Bf, outcomes
end


"""Quadratic one-period loss in `[state; monetary; fiscal]`."""
function recursive_stage_loss(weights::LossWeights, c::RecursiveCalibration)
    _, _, _, outcomes = recursive_matrices(c)
    state_count = 7
    monetary_index = state_count + 1
    fiscal_index = state_count + 2
    # Rows map [s; i; g] into current x, pi, end debt, i, g, Delta-i, Delta-g.
    map = zeros(7, state_count + 2)
    map[1:3, :] .= outcomes
    map[4, monetary_index] = 1.0
    map[5, fiscal_index] = 1.0
    map[6, 4] = -1.0
    map[6, monetary_index] = 1.0
    map[7, 5] = -1.0
    map[7, fiscal_index] = 1.0
    diagonal = Diagonal([
        weights.output,
        weights.inflation,
        weights.debt,
        weights.monetary_level,
        weights.fiscal_level,
        weights.monetary_change,
        weights.fiscal_change,
    ])
    return Matrix(map' * diagonal * map)
end


"""Copy loss weights and replace selected operational adjustment costs."""
function operational_weights(
    weights::LossWeights;
    monetary_adjustment::Union{Nothing, Float64} = nothing,
    fiscal_adjustment::Union{Nothing, Float64} = nothing,
)
    monetary_adjustment === nothing || monetary_adjustment >= 0.0 ||
        error("monetary adjustment cost must be non-negative")
    fiscal_adjustment === nothing || fiscal_adjustment >= 0.0 ||
        error("fiscal adjustment cost must be non-negative")
    return LossWeights(
        inflation = weights.inflation,
        output = weights.output,
        debt = weights.debt,
        monetary_level = weights.monetary_level,
        fiscal_level = weights.fiscal_level,
        monetary_change = something(monetary_adjustment, weights.monetary_change),
        fiscal_change = something(fiscal_adjustment, weights.fiscal_change),
        discount = weights.discount,
    )
end


"""
Solve the stationary Markov-perfect linear--quadratic Nash game.

`monetary_adjustment` and `fiscal_adjustment` are adjustment coefficients
delegated to future policymakers.  They change operational policy rules but do
not replace the primitive mandate used to evaluate the first-stage choice.  A
value of `nothing` retains the corresponding primitive adjustment cost.
"""
function solve_feedback_nash(
    primitives::ModelPrimitives,
    calibration::RecursiveCalibration;
    monetary_adjustment::Union{Nothing, Float64} = nothing,
    fiscal_adjustment::Union{Nothing, Float64} = nothing,
    tolerance::Float64 = 1e-11,
    max_iterations::Int = 20_000,
)
    A, Bm_vector, Bf_vector, _ = recursive_matrices(calibration)
    Bm = reshape(Bm_vector, :, 1)
    Bf = reshape(Bf_vector, :, 1)
    monetary_operational = operational_weights(
        primitives.monetary; monetary_adjustment = monetary_adjustment,
    )
    fiscal_operational = operational_weights(
        primitives.fiscal; fiscal_adjustment = fiscal_adjustment,
    )
    Qm = recursive_stage_loss(monetary_operational, calibration)
    Qf = recursive_stage_loss(fiscal_operational, calibration)
    state_count = size(A, 1)
    z = 1:state_count
    mi = state_count + 1
    fi = state_count + 2
    beta_m = primitives.monetary.discount
    beta_f = primitives.fiscal.discount

    Pm = Matrix(Qm[z, z])
    Pf = Matrix(Qf[z, z])
    monetary_rule = zeros(state_count)
    fiscal_rule = zeros(state_count)
    converged = false
    iterations = max_iterations

    for iteration in 1:max_iterations
        # Each row is one authority's first-order condition.  Off-diagonal
        # elements come from the same-period policy mix and the continuation
        # value of debt and lagged instruments.
        response_system = [
            Qm[mi, mi] + beta_m * dot(Bm, Pm * Bm)  Qm[mi, fi] + beta_m * dot(Bm, Pm * Bf)
            Qf[fi, mi] + beta_f * dot(Bf, Pf * Bm)  Qf[fi, fi] + beta_f * dot(Bf, Pf * Bf)
        ]
        state_terms = vcat(
            reshape(Qm[mi, z] + beta_m * vec(Bm' * Pm * A), 1, :),
            reshape(Qf[fi, z] + beta_f * vec(Bf' * Pf * A), 1, :),
        )
        rules = -stable_solve(response_system, state_terms)
        monetary_new = vec(rules[1, :])
        fiscal_new = vec(rules[2, :])
        closed_loop = A + Bm * monetary_new' + Bf * fiscal_new'
        selector = vcat(Matrix{Float64}(I, state_count, state_count), rules)
        Pm_new = selector' * Qm * selector + beta_m * closed_loop' * Pm * closed_loop
        Pf_new = selector' * Qf * selector + beta_f * closed_loop' * Pf * closed_loop
        Pm_new = Matrix(Symmetric(Pm_new))
        Pf_new = Matrix(Symmetric(Pf_new))

        error = maximum((
            maximum(abs, Pm_new - Pm),
            maximum(abs, Pf_new - Pf),
            maximum(abs, monetary_new - monetary_rule),
            maximum(abs, fiscal_new - fiscal_rule),
        ))
        Pm, Pf = Pm_new, Pf_new
        monetary_rule, fiscal_rule = monetary_new, fiscal_new
        if error < tolerance
            converged = true
            iterations = iteration
            break
        end
    end

    closed_loop = A + Bm * monetary_rule' + Bf * fiscal_rule'
    radius = maximum(abs, eigvals(closed_loop))
    return FeedbackGameResult(
        monetary_rule,
        fiscal_rule,
        Matrix(closed_loop),
        Pm,
        Pf,
        converged,
        iterations,
        real(radius),
    )
end


"""Solve the recursive cooperative regulator using the fixed social loss."""
function solve_feedback_cooperation(
    primitives::ModelPrimitives,
    calibration::RecursiveCalibration;
    tolerance::Float64 = 1e-11,
    max_iterations::Int = 20_000,
)
    A, Bm, Bf, _ = recursive_matrices(calibration)
    B = hcat(Bm, Bf)
    Q = recursive_stage_loss(primitives.social, calibration)
    state_count = size(A, 1)
    z = 1:state_count
    u = (state_count + 1):(state_count + 2)
    beta = primitives.social.discount
    P = Matrix(Q[z, z])
    rules = zeros(2, state_count)
    converged = false
    iterations = max_iterations
    for iteration in 1:max_iterations
        control_cost = Q[u, u] + beta * B' * P * B
        state_cost = Q[u, z] + beta * B' * P * A
        rules_new = -stable_solve(control_cost, state_cost)
        closed_loop = A + B * rules_new
        selector = vcat(Matrix{Float64}(I, state_count, state_count), rules_new)
        P_new = selector' * Q * selector + beta * closed_loop' * P * closed_loop
        P_new = Matrix(Symmetric(P_new))
        error = max(maximum(abs, P_new - P), maximum(abs, rules_new - rules))
        P, rules = P_new, rules_new
        if error < tolerance
            converged = true
            iterations = iteration
            break
        end
    end
    closed_loop = A + B * rules
    return (
        monetary_rule = vec(rules[1, :]),
        fiscal_rule = vec(rules[2, :]),
        transition = Matrix(closed_loop),
        value = P,
        converged = converged,
        iterations = iterations,
        spectral_radius = real(maximum(abs, eigvals(closed_loop))),
    )
end


"""Simulate a deterministic impulse response under stationary feedback rules."""
function simulate_feedback(
    result,
    initial_state::AbstractVector;
    horizon::Int = 40,
)
    length(initial_state) == length(result.monetary_rule) ||
        error("initial state has the wrong dimension")
    state = Float64.(initial_state)
    output = zeros(horizon)
    inflation = zeros(horizon)
    debt = zeros(horizon)
    monetary = zeros(horizon)
    fiscal = zeros(horizon)
    for t in 1:horizon
        monetary[t] = dot(result.monetary_rule, state)
        fiscal[t] = dot(result.fiscal_rule, state)
        next_state = result.transition * state
        output[t], inflation[t], debt[t] = next_state[1], next_state[2], next_state[3]
        state = next_state
    end
    return (
        output = output,
        inflation = inflation,
        debt = debt,
        monetary = monetary,
        fiscal = fiscal,
    )
end


"""Discounted value matrix for a fixed feedback policy and evaluation loss."""
function fixed_policy_value(
    weights::LossWeights,
    result,
    calibration::RecursiveCalibration;
    tolerance::Float64 = 1e-12,
    max_iterations::Int = 50_000,
)
    Q = recursive_stage_loss(weights, calibration)
    state_count = length(result.monetary_rule)
    rules = vcat(result.monetary_rule', result.fiscal_rule')
    selector = vcat(Matrix{Float64}(I, state_count, state_count), rules)
    period = selector' * Q * selector
    transition = result.transition
    P = zeros(state_count, state_count)
    for _ in 1:max_iterations
        P_new = period + weights.discount * transition' * P * transition
        if maximum(abs, P_new - P) < tolerance
            return Matrix(Symmetric(P_new))
        end
        P = P_new
    end
    error("fixed-policy value iteration did not converge")
end


"""
Solve the first-stage strategic investment game on an explicit commitment grid.

Future policymakers play the feedback Nash game with additional own-instrument
adjustment costs `(lambda_M, lambda_F)`.  The first-stage authorities evaluate
the resulting rules with their original mandate losses, plus a quadratic cost
of moving the institution away from its primitive adjustment coefficient.  A
constitutional planner evaluates the same continuation equilibrium with the
fixed social loss.  Centering the implementation cost at the primitive cleanly
isolates strategic delegation from an arbitrary preference for zero inertia.
"""
function solve_commitment_game(
    primitives::ModelPrimitives,
    calibration::RecursiveCalibration,
    demand_shock_sd::Float64,
    supply_shock_sd::Float64;
    commitment_grid = collect(0.0:0.025:0.50),
    investment_cost::Float64 = 0.10,
)
    grid = Float64.(commitment_grid)
    issorted(grid) || error("commitment grid must be sorted")
    first(grid) >= 0.0 || error("commitment grid must be non-negative")
    count = length(grid)
    monetary_objective = fill(Inf, count, count)
    fiscal_objective = fill(Inf, count, count)
    social_objective = fill(Inf, count, count)
    stability = fill(NaN, count, count)
    iterations = fill(0, count, count)
    initial_demand = [0.0, 0.0, 0.0, 0.0, 0.0, -demand_shock_sd, 0.0]
    initial_supply = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, supply_shock_sd]

    for (mi, lambda_m) in enumerate(grid), (fi, lambda_f) in enumerate(grid)
        result = solve_feedback_nash(
            primitives,
            calibration;
            monetary_adjustment = lambda_m,
            fiscal_adjustment = lambda_f,
        )
        stability[mi, fi] = result.spectral_radius
        iterations[mi, fi] = result.iterations
        if !result.converged || result.spectral_radius >= 1.0
            continue
        end
        Pm = fixed_policy_value(primitives.monetary, result, calibration)
        Pf = fixed_policy_value(primitives.fiscal, result, calibration)
        Ps = fixed_policy_value(primitives.social, result, calibration)
        expected_value(P) = 0.5 * (
            dot(initial_demand, P * initial_demand) +
            dot(initial_supply, P * initial_supply)
        )
        monetary_deviation = lambda_m - primitives.monetary.monetary_change
        fiscal_deviation = lambda_f - primitives.fiscal.fiscal_change
        monetary_objective[mi, fi] = expected_value(Pm) + investment_cost * monetary_deviation^2
        fiscal_objective[mi, fi] = expected_value(Pf) + investment_cost * fiscal_deviation^2
        social_objective[mi, fi] = expected_value(Ps) + investment_cost * (
            monetary_deviation^2 + fiscal_deviation^2
        )
    end

    # Monetary best responses are computed down each fiscal-choice column;
    # fiscal best responses are computed across each monetary-choice row.
    monetary_best_index = [argmin(monetary_objective[:, fi]) for fi in 1:count]
    fiscal_best_index = [argmin(fiscal_objective[mi, :]) for mi in 1:count]
    nash_candidates = Tuple{Int, Int}[]
    for mi in 1:count, fi in 1:count
        if monetary_best_index[fi] == mi && fiscal_best_index[mi] == fi
            push!(nash_candidates, (mi, fi))
        end
    end
    if isempty(nash_candidates)
        # A finite grid need not contain the exact crossing.  Select the cell
        # with the smallest joint distance from both discrete best responses.
        distances = [
            abs(mi - monetary_best_index[fi]) + abs(fi - fiscal_best_index[mi])
            for mi in 1:count, fi in 1:count
        ]
        approximate = argmin(distances)
        nash_index = (approximate[1], approximate[2])
        exact_grid_nash = false
    else
        candidate_scores = [
            monetary_objective[index...] + fiscal_objective[index...]
            for index in nash_candidates
        ]
        nash_index = nash_candidates[argmin(candidate_scores)]
        exact_grid_nash = true
    end
    social_cartesian = argmin(social_objective)
    social_index = (social_cartesian[1], social_cartesian[2])
    return (
        grid = grid,
        monetary_objective = monetary_objective,
        fiscal_objective = fiscal_objective,
        social_objective = social_objective,
        stability = stability,
        iterations = iterations,
        monetary_best_index = monetary_best_index,
        fiscal_best_index = fiscal_best_index,
        nash_index = nash_index,
        social_index = social_index,
        exact_grid_nash = exact_grid_nash,
        investment_cost = investment_cost,
    )
end

module PolicyGame

using LinearAlgebra

export MacroParameters,
       Preferences,
       ModelPrimitives,
       GameResult,
       RuleRegime,
       transition_matrices,
       simulate,
       solve_games,
       realised_loss,
       simulate_policy_rules,
       first_action_sensitivities


# -----------------------------------------------------------------------------
# Model primitives
# -----------------------------------------------------------------------------

"""
Parameters of the macroeconomic transition equations.

Sign conventions:
* `u > 0` is a tighter real cash-rate setting, in percentage points.
* `g > 0` is a looser public-demand setting, in per cent from trend.
* `b > 0` is public debt above its steady-state debt-to-GDP ratio.
"""
Base.@kwdef struct MacroParameters
    rho_x::Float64
    sigma_i::Float64
    chi_g::Float64
    rho_pi::Float64
    kappa::Float64

    # The debt block is calibrated rather than estimated in this example.
    rho_b::Float64 = 0.995
    psi_g::Float64 = 0.060
    psi_i::Float64 = 0.015
    psi_pi::Float64 = 0.100
end


"""
One authority's fixed objective-function primitives.

The same preferences are used in every strategic solution concept.  In
particular, the fiscal-dominance exercise does *not* alter these weights.
"""
Base.@kwdef struct Preferences
    inflation::Float64
    output::Float64
    debt::Float64
    adjustment::Float64
    level::Float64 = 1.0e-4
    discount::Float64 = 0.99
end


"""All primitives needed to solve the monetary-fiscal game."""
struct ModelPrimitives
    economy::MacroParameters
    monetary_preferences::Preferences
    fiscal_preferences::Preferences
end


"""A solved policy path and its resulting macroeconomic outcomes."""
struct GameResult
    monetary::Vector{Float64}
    fiscal::Vector{Float64}
    states::Matrix{Float64}
    monetary_loss::Float64
    fiscal_loss::Float64
end


"""
Coefficients of an explicit monetary-fiscal policy-rule regime.

`nominal_inflation_response` is the coefficient in a nominal Taylor rule.  Its
real-rate counterpart is `nominal_inflation_response - 1`.  Thus a value above
one is active monetary policy and a value below one is passive monetary policy.

`fiscal_debt_response` is the response of fiscal tightening (the primary
surplus) to debt.  Since `g` is fiscal expansion, it enters its rule as `-gamma_b
* b`.  A positive value is passive/debt-stabilising fiscal policy.
"""
Base.@kwdef struct RuleRegime
    name::String
    nominal_inflation_response::Float64
    monetary_output_response::Float64
    monetary_smoothing::Float64
    monetary_debt_accommodation::Float64
    fiscal_debt_response::Float64
    fiscal_output_response::Float64
    fiscal_smoothing::Float64
end


# -----------------------------------------------------------------------------
# Three-equation-plus-debt transition system
# -----------------------------------------------------------------------------

"""
Return matrices for

    s[t+1] = A*s[t] + B_u*u[t] + B_g*g[t] + shock[t].

The state is ordered as

    [output gap, inflation gap, debt gap,
     lagged output gap, previous monetary setting, previous fiscal setting].

The lagged output state preserves the two-quarter activity-to-inflation lag in
the Australian Phillips-curve estimate.  Previous instruments are states
because both authorities dislike abrupt instrument changes.
"""
function transition_matrices(parameters::MacroParameters)
    p = parameters
    A = zeros(6, 6)

    # Dynamic IS equation.
    A[1, 1] = p.rho_x

    # Phillips equation. Inflation responds to the older output gap.
    A[2, 2] = p.rho_pi
    A[2, 4] = p.kappa

    # Linearised government debt accumulation equation.
    A[3, 2] = -p.psi_pi
    A[3, 3] = p.rho_b

    # Store today's output gap for the Phillips curve next period.
    A[4, 1] = 1.0

    B_u = zeros(6)
    B_u[1] = -p.sigma_i  # monetary tightening lowers demand
    B_u[3] = p.psi_i     # higher rates raise debt-service costs
    B_u[5] = 1.0         # store the current monetary instrument

    B_g = zeros(6)
    B_g[1] = p.chi_g     # fiscal expansion raises demand
    B_g[3] = p.psi_g     # fiscal expansion raises debt
    B_g[6] = 1.0         # store the current fiscal instrument

    return A, B_u, B_g
end


"""Simulate a supplied pair of monetary and fiscal instrument paths."""
function simulate(
    parameters::MacroParameters,
    initial_state::AbstractVector,
    monetary::AbstractVector,
    fiscal::AbstractVector,
    shocks::AbstractMatrix,
)
    horizon = length(monetary)
    @assert length(fiscal) == horizon
    @assert size(shocks) == (horizon, length(initial_state))

    A, B_u, B_g = transition_matrices(parameters)
    states = zeros(horizon + 1, length(initial_state))
    states[1, :] .= initial_state

    for t in 1:horizon
        states[t + 1, :] .= (
            A * view(states, t, :) +
            B_u * monetary[t] +
            B_g * fiscal[t] +
            view(shocks, t, :)
        )
    end
    return states
end


# Julia stores arrays column by column.  This helper deliberately stacks all
# states for period 1, then all states for period 2, and so on.  That ordering
# matches the block-diagonal loss matrix constructed below.
stack_postdecision_states(states::AbstractMatrix) =
    vec(permutedims(states[2:end, :]))


"""Construct the affine mapping from full control paths to full state paths."""
function state_mapping(
    parameters::MacroParameters,
    initial_state::AbstractVector,
    shocks::AbstractMatrix,
)
    horizon = size(shocks, 1)
    state_count = length(initial_state)
    zeros_path = zeros(horizon)

    base_states = stack_postdecision_states(
        simulate(parameters, initial_state, zeros_path, zeros_path, shocks),
    )
    mapping = zeros(horizon * state_count, 2 * horizon)
    no_shocks = zeros(size(shocks))

    # Each column is the state response to a unit movement in one instrument in
    # one period.  This is slower than a hand-built companion matrix but much
    # easier to audit in an initial research model.
    for column in 1:(2 * horizon)
        monetary = zeros(horizon)
        fiscal = zeros(horizon)
        if column <= horizon
            monetary[column] = 1.0
        else
            fiscal[column - horizon] = 1.0
        end
        response = simulate(
            parameters,
            zeros(state_count),
            monetary,
            fiscal,
            no_shocks,
        )
        mapping[:, column] .= stack_postdecision_states(response)
    end
    return base_states, mapping
end


# -----------------------------------------------------------------------------
# Quadratic objectives and strategic solutions
# -----------------------------------------------------------------------------

struct QuadraticLoss
    hessian::Matrix{Float64}
    linear::Vector{Float64}
    constant::Float64
end

value(loss::QuadraticLoss, controls::AbstractVector) =
    dot(controls, loss.hessian * controls) +
    2.0 * dot(loss.linear, controls) +
    loss.constant


function difference_matrix(horizon::Int)
    D = Matrix{Float64}(I, horizon, horizon)
    for t in 2:horizon
        D[t, t - 1] = -1.0
    end
    return D
end


"""Write one authority's discounted loss as w'Hw + 2h'w + c."""
function loss_quadratic(
    preferences::Preferences,
    base_states::AbstractVector,
    state_map::AbstractMatrix,
    initial_state::AbstractVector,
    authority::Symbol,
)
    horizon = size(state_map, 2) ÷ 2
    Q = Diagonal([
        preferences.output,
        preferences.inflation,
        preferences.debt,
        0.0,
        0.0,
        0.0,
    ])
    period_weights = preferences.discount .^ (0:(horizon - 1))
    W = Diagonal(period_weights)
    Q_bar = kron(W, Q)

    H = Matrix(state_map' * Q_bar * state_map)
    h = Vector(state_map' * Q_bar * base_states)
    c = dot(base_states, Q_bar * base_states)

    selector = zeros(horizon, 2 * horizon)
    previous_instrument = 0.0
    if authority == :monetary
        selector[:, 1:horizon] .= Matrix{Float64}(I, horizon, horizon)
        previous_instrument = initial_state[5]
    elseif authority == :fiscal
        selector[:, (horizon + 1):(2 * horizon)] .=
            Matrix{Float64}(I, horizon, horizon)
        previous_instrument = initial_state[6]
    else
        error("authority must be :monetary or :fiscal")
    end

    # Delta maps an instrument path into period-to-period changes.  The first
    # change is measured from the instrument inherited in the initial state.
    delta_map = difference_matrix(horizon) * selector
    delta_base = zeros(horizon)
    delta_base[1] = -previous_instrument

    H .+= preferences.adjustment .* (delta_map' * W * delta_map)
    h .+= preferences.adjustment .* (delta_map' * W * delta_base)
    c += preferences.adjustment * dot(delta_base, W * delta_base)
    H .+= preferences.level .* (selector' * W * selector)

    return QuadraticLoss(H, h, c)
end


# Fall back to a Moore-Penrose solution only if numerical singularity occurs.
function stable_solve(A::AbstractMatrix, b::AbstractVecOrMat)
    try
        return A \ b
    catch exception
        if exception isa SingularException || exception isa PosDefException
            return pinv(Matrix(A)) * b
        end
        rethrow()
    end
end


"""
Solve four games using exactly the same macro and preference primitives:

1. simultaneous open-loop Nash;
2. equal-weighted cooperation;
3. central-bank Stackelberg leadership; and
4. fiscal Stackelberg leadership.
"""
function solve_games(
    primitives::ModelPrimitives,
    initial_state::AbstractVector,
    shocks::AbstractMatrix;
    cooperative_monetary_weight::Float64 = 0.5,
)
    base, mapping = state_mapping(primitives.economy, initial_state, shocks)
    monetary_loss = loss_quadratic(
        primitives.monetary_preferences,
        base,
        mapping,
        initial_state,
        :monetary,
    )
    fiscal_loss = loss_quadratic(
        primitives.fiscal_preferences,
        base,
        mapping,
        initial_state,
        :fiscal,
    )

    horizon = size(shocks, 1)
    u = 1:horizon
    g = (horizon + 1):(2 * horizon)

    # Nash equilibrium: combine the central bank's FOCs for u with the fiscal
    # authority's FOCs for g.
    nash_matrix = [
        monetary_loss.hessian[u, u] monetary_loss.hessian[u, g]
        fiscal_loss.hessian[g, u] fiscal_loss.hessian[g, g]
    ]
    nash_rhs = -vcat(monetary_loss.linear[u], fiscal_loss.linear[g])
    nash_controls = stable_solve(nash_matrix, nash_rhs)

    # Cooperative benchmark: the solution concept changes, not the underlying
    # agency objectives.  The planner puts equal weight on them by default.
    omega = cooperative_monetary_weight
    cooperative_H = (
        omega .* monetary_loss.hessian .+
        (1.0 - omega) .* fiscal_loss.hessian
    )
    cooperative_h = (
        omega .* monetary_loss.linear .+
        (1.0 - omega) .* fiscal_loss.linear
    )
    cooperative_controls = stable_solve(cooperative_H, -cooperative_h)

    # Central-bank leadership.  First derive the fiscal authority's complete
    # best-response path g(u), then substitute it into the central-bank loss.
    Hgg = fiscal_loss.hessian[g, g]
    fiscal_slope = -stable_solve(Hgg, fiscal_loss.hessian[g, u])
    fiscal_intercept = -stable_solve(Hgg, fiscal_loss.linear[g])
    cb_leader_map = vcat(Matrix{Float64}(I, horizon, horizon), fiscal_slope)
    cb_leader_offset = vcat(zeros(horizon), fiscal_intercept)
    cb_reduced_H = cb_leader_map' * monetary_loss.hessian * cb_leader_map
    cb_reduced_h = cb_leader_map' * (
        monetary_loss.hessian * cb_leader_offset + monetary_loss.linear
    )
    cb_choice = stable_solve(cb_reduced_H, -cb_reduced_h)
    cb_leader_controls = cb_leader_map * cb_choice + cb_leader_offset

    # Fiscal leadership is the symmetric calculation.
    Huu = monetary_loss.hessian[u, u]
    monetary_slope = -stable_solve(Huu, monetary_loss.hessian[u, g])
    monetary_intercept = -stable_solve(Huu, monetary_loss.linear[u])
    fiscal_leader_map = vcat(monetary_slope, Matrix{Float64}(I, horizon, horizon))
    fiscal_leader_offset = vcat(monetary_intercept, zeros(horizon))
    fiscal_reduced_H = fiscal_leader_map' * fiscal_loss.hessian * fiscal_leader_map
    fiscal_reduced_h = fiscal_leader_map' * (
        fiscal_loss.hessian * fiscal_leader_offset + fiscal_loss.linear
    )
    fiscal_choice = stable_solve(fiscal_reduced_H, -fiscal_reduced_h)
    fiscal_leader_controls = fiscal_leader_map * fiscal_choice + fiscal_leader_offset

    control_paths = Dict(
        "nash" => nash_controls,
        "cooperative" => cooperative_controls,
        "central_bank_leader" => cb_leader_controls,
        "fiscal_leader" => fiscal_leader_controls,
    )

    results = Dict{String, GameResult}()
    for (name, controls) in control_paths
        monetary = Vector(controls[u])
        fiscal = Vector(controls[g])
        states = simulate(primitives.economy, initial_state, monetary, fiscal, shocks)
        results[name] = GameResult(
            monetary,
            fiscal,
            states,
            value(monetary_loss, controls),
            value(fiscal_loss, controls),
        )
    end
    return results
end


"""Evaluate a realised path using one authority's fixed preferences."""
function realised_loss(
    preferences::Preferences,
    states::AbstractMatrix,
    instrument::AbstractVector,
    authority::Symbol,
)
    horizon = length(instrument)
    @assert size(states, 1) == horizon + 1
    previous_index = authority == :monetary ? 5 : 6
    previous = states[1, previous_index]
    total = 0.0

    for t in 1:horizon
        # Policy chosen at t affects states at t+1, so score the post-decision
        # state and the change from the inherited instrument.
        x, pi, b = states[t + 1, 1:3]
        change = instrument[t] - previous
        period_loss = (
            preferences.output * x^2 +
            preferences.inflation * pi^2 +
            preferences.debt * b^2 +
            preferences.adjustment * change^2 +
            preferences.level * instrument[t]^2
        )
        total += preferences.discount^(t - 1) * period_loss
        previous = instrument[t]
    end
    return total
end


# -----------------------------------------------------------------------------
# Leeper-style active/passive rule regimes
# -----------------------------------------------------------------------------

"""
Simulate explicit policy rules while leaving both loss functions unchanged.

This separates policy institutions from preferences.  It is a reduced-form
active/passive experiment, not a rational-expectations determinacy proof.
"""
function simulate_policy_rules(
    primitives::ModelPrimitives,
    regime::RuleRegime,
    initial_state::AbstractVector,
    shocks::AbstractMatrix,
)
    horizon = size(shocks, 1)
    states = zeros(horizon + 1, length(initial_state))
    states[1, :] .= initial_state
    monetary = zeros(horizon)
    fiscal = zeros(horizon)
    A, B_u, B_g = transition_matrices(primitives.economy)

    # Subtracting one converts the nominal Taylor coefficient into the response
    # of the ex-ante real rate to inflation.
    real_inflation_response = regime.nominal_inflation_response - 1.0

    for t in 1:horizon
        state = view(states, t, :)
        monetary[t] = (
            real_inflation_response * state[2] +
            regime.monetary_output_response * state[1] +
            regime.monetary_smoothing * state[5] -
            regime.monetary_debt_accommodation * state[3]
        )
        fiscal[t] = (
            -regime.fiscal_debt_response * state[3] -
            regime.fiscal_output_response * state[1] +
            regime.fiscal_smoothing * state[6]
        )
        states[t + 1, :] .= (
            A * state +
            B_u * monetary[t] +
            B_g * fiscal[t] +
            view(shocks, t, :)
        )
    end

    return GameResult(
        monetary,
        fiscal,
        states,
        realised_loss(
            primitives.monetary_preferences,
            states,
            monetary,
            :monetary,
        ),
        realised_loss(
            primitives.fiscal_preferences,
            states,
            fiscal,
            :fiscal,
        ),
    )
end


"""Recover the first Nash actions' derivatives with respect to initial states."""
function first_action_sensitivities(primitives::ModelPrimitives, horizon::Int = 20)
    state_count = 6
    sensitivities = zeros(2, state_count)
    shocks = zeros(horizon, state_count)

    for state_index in 1:state_count
        initial_state = zeros(state_count)
        initial_state[state_index] = 1.0
        equilibrium = solve_games(primitives, initial_state, shocks)["nash"]
        sensitivities[1, state_index] = equilibrium.monetary[1]
        sensitivities[2, state_index] = equilibrium.fiscal[1]
    end
    return sensitivities
end

end # module PolicyGame

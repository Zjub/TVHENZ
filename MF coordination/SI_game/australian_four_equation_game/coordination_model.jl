module CoordinationModel

using LinearAlgebra

export MacroCalibration,
       LossWeights,
       ModelPrimitives,
       ShockPaths,
       GameResult,
       RuleCoefficients,
       macro_outcomes,
       solve_games,
       strategic_diagnostics,
       solve_rule_system,
       social_loss_value,
       mandate_weights,
       RecursiveCalibration,
       FeedbackGameResult,
       recursive_matrices,
       solve_feedback_nash,
       solve_feedback_cooperation,
       simulate_feedback,
       solve_commitment_game


# -----------------------------------------------------------------------------
# Model primitives
# -----------------------------------------------------------------------------

"""
Parameters of the forward-looking macroeconomic block.

Sign conventions:
* `i > 0` is a tighter nominal cash-rate gap, in percentage points;
* `g > 0` is a fiscal expansion/public-demand gap, in per cent;
* `b > 0` is debt above its steady-state debt-to-GDP ratio, in percentage points.
"""
Base.@kwdef struct MacroCalibration
    beta::Float64 = 0.99
    sigma::Float64
    fiscal_multiplier::Float64
    kappa::Float64
    rho_b::Float64 = 0.995
    debt_from_fiscal::Float64 = 0.060
    debt_from_interest::Float64 = 0.015
    debt_from_inflation::Float64 = 0.100
end


"""
Quadratic loss weights.

The social, monetary and fiscal losses all use this same type.  They are kept
as three separate objects because a mandate is not a welfare criterion.
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


struct ModelPrimitives
    economy::MacroCalibration
    social::LossWeights
    monetary::LossWeights
    fiscal::LossWeights
end


"""Exogenous paths entering the IS curve, Phillips curve and debt equation."""
struct ShockPaths
    demand::Vector{Float64}
    cost_push::Vector{Float64}
    debt::Vector{Float64}

    function ShockPaths(demand, cost_push, debt)
        length(demand) == length(cost_push) == length(debt) ||
            error("All shock paths must have the same length")
        new(Float64.(demand), Float64.(cost_push), Float64.(debt))
    end
end


struct GameResult
    monetary::Vector{Float64}
    fiscal::Vector{Float64}
    output::Vector{Float64}
    inflation::Vector{Float64}
    debt::Vector{Float64}
    monetary_loss::Float64
    fiscal_loss::Float64
    social_loss::Float64
end


"""
Coefficients for the alternative, rule-based five-equation closure.

The monetary rule is a smoothed Taylor-type rule.  The fiscal rule makes the
expansion variable `g` fall when debt or the output gap rises.  This closure is
reported separately from the optimizing policy game.
"""
Base.@kwdef struct RuleCoefficients
    name::String
    monetary_smoothing::Float64
    inflation_response::Float64
    output_response::Float64
    debt_accommodation::Float64
    fiscal_smoothing::Float64
    debt_response::Float64
    output_response_fiscal::Float64
end


# -----------------------------------------------------------------------------
# Forward-looking IS and Phillips curves plus government debt
# -----------------------------------------------------------------------------

"""
Solve the private-sector block for supplied monetary and fiscal paths.

Under perfect foresight, expectations equal the model-consistent future path:

    x[t]  = x[t+1] - sigma*(i[t] - pi[t+1]) + chi*g[t] + demand[t]
    pi[t] = beta*pi[t+1] + kappa*x[t] + cost_push[t]
    b[t+1]= rho_b*b[t] + eta_g*g[t] + eta_i*i[t]
             - eta_pi*pi[t] + debt_shock[t].

The finite-horizon terminal conditions are x[T+1] = pi[T+1] = 0.  The returned
debt path is end-of-period debt b[t+1], so the final policy choice is disciplined
by the final debt observation in every objective.
"""
function macro_outcomes(
    p::MacroCalibration,
    monetary::AbstractVector,
    fiscal::AbstractVector,
    shocks::ShockPaths;
    initial_debt::Float64 = 0.0,
)
    horizon = length(monetary)
    length(fiscal) == horizon == length(shocks.demand) ||
        error("Controls and shocks must share a horizon")

    # Solve the stacked rational-expectations IS/Phillips block.  Unknowns are
    # [x[1:T]; pi[1:T]].
    system = zeros(2 * horizon, 2 * horizon)
    rhs = zeros(2 * horizon)
    x = 1:horizon
    pi = (horizon + 1):(2 * horizon)

    for t in 1:horizon
        # x_t - x_{t+1} - sigma*pi_{t+1}
        system[t, x[t]] = 1.0
        if t < horizon
            system[t, x[t + 1]] = -1.0
            system[t, pi[t + 1]] = -p.sigma
        end
        rhs[t] = (
            -p.sigma * monetary[t] +
            p.fiscal_multiplier * fiscal[t] +
            shocks.demand[t]
        )

        # pi_t - beta*pi_{t+1} - kappa*x_t
        row = horizon + t
        system[row, pi[t]] = 1.0
        system[row, x[t]] = -p.kappa
        if t < horizon
            system[row, pi[t + 1]] = -p.beta
        end
        rhs[row] = shocks.cost_push[t]
    end

    solution = system \ rhs
    output_path = Vector(solution[x])
    inflation_path = Vector(solution[pi])

    debt_path = zeros(horizon)
    inherited_debt = initial_debt
    for t in 1:horizon
        debt_path[t] = (
            p.rho_b * inherited_debt +
            p.debt_from_fiscal * fiscal[t] +
            p.debt_from_interest * monetary[t] -
            p.debt_from_inflation * inflation_path[t] +
            shocks.debt[t]
        )
        inherited_debt = debt_path[t]
    end
    return output_path, inflation_path, debt_path
end


"""Return the affine mapping from complete control paths to macro outcomes."""
function outcome_mapping(
    p::MacroCalibration,
    shocks::ShockPaths;
    initial_debt::Float64 = 0.0,
)
    horizon = length(shocks.demand)
    zero_controls = zeros(horizon)
    base = vcat(macro_outcomes(
        p, zero_controls, zero_controls, shocks; initial_debt = initial_debt,
    )...)

    zero_shocks = ShockPaths(zeros(horizon), zeros(horizon), zeros(horizon))
    mapping = zeros(3 * horizon, 2 * horizon)
    for column in 1:(2 * horizon)
        monetary = zeros(horizon)
        fiscal = zeros(horizon)
        if column <= horizon
            monetary[column] = 1.0
        else
            fiscal[column - horizon] = 1.0
        end
        response = macro_outcomes(p, monetary, fiscal, zero_shocks)
        mapping[:, column] .= vcat(response...)
    end
    return base, mapping
end


# -----------------------------------------------------------------------------
# Objectives and policy games
# -----------------------------------------------------------------------------

struct QuadraticLoss
    hessian::Matrix{Float64}
    linear::Vector{Float64}
    constant::Float64
end

value(loss::QuadraticLoss, controls::AbstractVector) =
    dot(controls, loss.hessian * controls) +
    2.0 * dot(loss.linear, controls) + loss.constant


function difference_matrix(horizon::Int)
    # Include both entry from the inherited steady-state instrument (zero) and
    # the terminal return to that steady state.  Without the final row, a
    # finite-horizon game can exploit an artificial end-of-sample jump.
    differences = zeros(horizon + 1, horizon)
    differences[1, 1] = 1.0
    for t in 2:horizon
        differences[t, t] = 1.0
        differences[t, t - 1] = -1.0
    end
    differences[end, end] = -1.0
    return differences
end


"""Write one loss as w'Hw + 2h'w + c for w = [i; g]."""
function loss_quadratic(
    weights::LossWeights,
    base::AbstractVector,
    mapping::AbstractMatrix,
)
    horizon = size(mapping, 2) ÷ 2
    discount = weights.discount .^ (0:(horizon - 1))
    state_weights = Diagonal(vcat(
        weights.output .* discount,
        weights.inflation .* discount,
        weights.debt .* discount,
    ))

    hessian = Matrix(mapping' * state_weights * mapping)
    linear = Vector(mapping' * state_weights * base)
    constant = dot(base, state_weights * base)

    monetary_selector = hcat(
        Matrix{Float64}(I, horizon, horizon), zeros(horizon, horizon),
    )
    fiscal_selector = hcat(
        zeros(horizon, horizon), Matrix{Float64}(I, horizon, horizon),
    )
    period_discount = Diagonal(discount)
    adjustment_discount = Diagonal(weights.discount .^ (0:horizon))
    differences = difference_matrix(horizon)

    for (level_weight, change_weight, selector) in [
        (weights.monetary_level, weights.monetary_change, monetary_selector),
        (weights.fiscal_level, weights.fiscal_change, fiscal_selector),
    ]
        hessian .+= level_weight .* (selector' * period_discount * selector)
        change_map = differences * selector
        hessian .+= change_weight .* (
            change_map' * adjustment_discount * change_map
        )
    end
    return QuadraticLoss(hessian, linear, constant)
end


stable_solve(matrix::AbstractMatrix, rhs::AbstractVecOrMat) = try
    matrix \ rhs
catch exception
    if exception isa SingularException || exception isa PosDefException
        pinv(Matrix(matrix)) * rhs
    else
        rethrow()
    end
end


"""
Solve cooperation, simultaneous open-loop Nash, and both Stackelberg games.

Cooperation minimizes the separate social-welfare loss.  Nash and leadership
use the two mandate losses.  Every outcome is finally evaluated using the same
social loss, which makes welfare comparisons coherent.
"""
function solve_games(
    primitives::ModelPrimitives,
    shocks::ShockPaths;
    initial_debt::Float64 = 0.0,
)
    base, mapping = outcome_mapping(
        primitives.economy, shocks; initial_debt = initial_debt,
    )
    social = loss_quadratic(primitives.social, base, mapping)
    monetary_loss = loss_quadratic(primitives.monetary, base, mapping)
    fiscal_loss = loss_quadratic(primitives.fiscal, base, mapping)

    horizon = length(shocks.demand)
    i = 1:horizon
    g = (horizon + 1):(2 * horizon)

    # Simultaneous open-loop Nash equilibrium.
    nash_matrix = [
        monetary_loss.hessian[i, i] monetary_loss.hessian[i, g]
        fiscal_loss.hessian[g, i] fiscal_loss.hessian[g, g]
    ]
    nash_rhs = -vcat(monetary_loss.linear[i], fiscal_loss.linear[g])
    nash = stable_solve(nash_matrix, nash_rhs)

    # Centralized policy chooses both instruments using true social welfare.
    cooperation = stable_solve(social.hessian, -social.linear)

    # Monetary leadership: fiscal authority follows its complete best response.
    fiscal_slope = -stable_solve(
        fiscal_loss.hessian[g, g], fiscal_loss.hessian[g, i],
    )
    fiscal_intercept = -stable_solve(
        fiscal_loss.hessian[g, g], fiscal_loss.linear[g],
    )
    monetary_map = vcat(Matrix{Float64}(I, horizon, horizon), fiscal_slope)
    monetary_offset = vcat(zeros(horizon), fiscal_intercept)
    monetary_reduced_H = monetary_map' * monetary_loss.hessian * monetary_map
    monetary_reduced_h = monetary_map' * (
        monetary_loss.hessian * monetary_offset + monetary_loss.linear
    )
    monetary_choice = stable_solve(monetary_reduced_H, -monetary_reduced_h)
    monetary_leader = monetary_map * monetary_choice + monetary_offset

    # Fiscal leadership is symmetric.
    monetary_slope = -stable_solve(
        monetary_loss.hessian[i, i], monetary_loss.hessian[i, g],
    )
    monetary_intercept = -stable_solve(
        monetary_loss.hessian[i, i], monetary_loss.linear[i],
    )
    fiscal_map = vcat(monetary_slope, Matrix{Float64}(I, horizon, horizon))
    fiscal_offset = vcat(monetary_intercept, zeros(horizon))
    fiscal_reduced_H = fiscal_map' * fiscal_loss.hessian * fiscal_map
    fiscal_reduced_h = fiscal_map' * (
        fiscal_loss.hessian * fiscal_offset + fiscal_loss.linear
    )
    fiscal_choice = stable_solve(fiscal_reduced_H, -fiscal_reduced_h)
    fiscal_leader = fiscal_map * fiscal_choice + fiscal_offset

    controls = Dict(
        "cooperation" => cooperation,
        "nash" => nash,
        "monetary_leader" => monetary_leader,
        "fiscal_leader" => fiscal_leader,
    )

    results = Dict{String, GameResult}()
    for (name, path) in controls
        monetary_path = Vector(path[i])
        fiscal_path = Vector(path[g])
        output, inflation, debt = macro_outcomes(
            primitives.economy,
            monetary_path,
            fiscal_path,
            shocks;
            initial_debt = initial_debt,
        )
        results[name] = GameResult(
            monetary_path,
            fiscal_path,
            output,
            inflation,
            debt,
            value(monetary_loss, path),
            value(fiscal_loss, path),
            value(social, path),
        )
    end
    return results
end


"""Return best-response matrices and their round-trip spectral radius."""
function strategic_diagnostics(primitives::ModelPrimitives, horizon::Int)
    zero_shocks = ShockPaths(zeros(horizon), zeros(horizon), zeros(horizon))
    base, mapping = outcome_mapping(primitives.economy, zero_shocks)
    monetary_loss = loss_quadratic(primitives.monetary, base, mapping)
    fiscal_loss = loss_quadratic(primitives.fiscal, base, mapping)
    i = 1:horizon
    g = (horizon + 1):(2 * horizon)

    monetary_response = -stable_solve(
        monetary_loss.hessian[i, i], monetary_loss.hessian[i, g],
    )
    fiscal_response = -stable_solve(
        fiscal_loss.hessian[g, g], fiscal_loss.hessian[g, i],
    )
    loop = monetary_response * fiscal_response
    radius = maximum(abs, eigvals(loop))
    return (
        monetary_response = monetary_response,
        fiscal_response = fiscal_response,
        spectral_radius = real(radius),
        monetary_norm = opnorm(monetary_response),
        fiscal_norm = opnorm(fiscal_response),
    )
end


"""
Construct mandate tilts around a fixed social loss.

Only inflation and output weights change.  Targets, macro parameters, debt
weights and all instrument costs remain identical, isolating the mechanism in
the user's research question.
"""
function mandate_weights(social::LossWeights, divergence::Float64)
    0.0 <= divergence < 1.0 || error("divergence must lie in [0,1)")
    monetary = LossWeights(
        inflation = social.inflation * (1.0 + divergence),
        output = social.output * (1.0 - divergence),
        debt = social.debt,
        monetary_level = social.monetary_level,
        fiscal_level = social.fiscal_level,
        monetary_change = social.monetary_change,
        fiscal_change = social.fiscal_change,
        discount = social.discount,
    )
    fiscal = LossWeights(
        inflation = social.inflation * (1.0 - divergence),
        output = social.output * (1.0 + divergence),
        debt = social.debt,
        monetary_level = social.monetary_level,
        fiscal_level = social.fiscal_level,
        monetary_change = social.monetary_change,
        fiscal_change = social.fiscal_change,
        discount = social.discount,
    )
    return monetary, fiscal
end


# -----------------------------------------------------------------------------
# Alternative five-equation policy-rule closure
# -----------------------------------------------------------------------------

"""
Solve IS, Phillips, debt, monetary-rule and fiscal-rule equations jointly.

This provides a transparent active/passive-policy comparison.  It is not used
to calculate the strategic games, whose closures are optimizing first-order
conditions rather than imposed rules.
"""
function solve_rule_system(
    p::MacroCalibration,
    rule::RuleCoefficients,
    shocks::ShockPaths;
    initial_debt::Float64 = 0.0,
)
    horizon = length(shocks.demand)
    block = horizon
    x = 1:block
    pi = (block + 1):(2 * block)
    b = (2 * block + 1):(3 * block)
    i = (3 * block + 1):(4 * block)
    g = (4 * block + 1):(5 * block)
    matrix = zeros(5 * horizon, 5 * horizon)
    rhs = zeros(5 * horizon)

    for t in 1:horizon
        # Forward-looking IS curve.
        row = t
        matrix[row, x[t]] = 1.0
        matrix[row, i[t]] = p.sigma
        matrix[row, g[t]] = -p.fiscal_multiplier
        if t < horizon
            matrix[row, x[t + 1]] = -1.0
            matrix[row, pi[t + 1]] = -p.sigma
        end
        rhs[row] = shocks.demand[t]

        # New Keynesian Phillips curve.
        row = horizon + t
        matrix[row, pi[t]] = 1.0
        matrix[row, x[t]] = -p.kappa
        if t < horizon
            matrix[row, pi[t + 1]] = -p.beta
        end
        rhs[row] = shocks.cost_push[t]

        # Beginning-of-period debt. The first observation is predetermined;
        # later observations follow the previous period's budget identity.
        row = 2 * horizon + t
        matrix[row, b[t]] = 1.0
        if t == 1
            rhs[row] = initial_debt
        else
            matrix[row, b[t - 1]] = -p.rho_b
            matrix[row, g[t - 1]] = -p.debt_from_fiscal
            matrix[row, i[t - 1]] = -p.debt_from_interest
            matrix[row, pi[t - 1]] = p.debt_from_inflation
            rhs[row] = shocks.debt[t - 1]
        end

        # Smoothed monetary reaction function.
        row = 3 * horizon + t
        matrix[row, i[t]] = 1.0
        if t > 1
            matrix[row, i[t - 1]] = -rule.monetary_smoothing
        end
        scale_i = 1.0 - rule.monetary_smoothing
        matrix[row, pi[t]] = -scale_i * rule.inflation_response
        matrix[row, x[t]] = -scale_i * rule.output_response
        matrix[row, b[t]] = -scale_i * rule.debt_accommodation

        # Fiscal expansion falls with debt and a positive output gap.
        row = 4 * horizon + t
        matrix[row, g[t]] = 1.0
        if t > 1
            matrix[row, g[t - 1]] = -rule.fiscal_smoothing
        end
        scale_g = 1.0 - rule.fiscal_smoothing
        matrix[row, b[t]] = scale_g * rule.debt_response
        matrix[row, x[t]] = scale_g * rule.output_response_fiscal
    end

    solution = matrix \ rhs
    # Convert beginning-of-period debt to the same end-of-period convention as
    # the optimizing model before evaluating the result.
    end_debt = zeros(horizon)
    for t in 1:horizon
        end_debt[t] = (
            p.rho_b * solution[b[t]] +
            p.debt_from_fiscal * solution[g[t]] +
            p.debt_from_interest * solution[i[t]] -
            p.debt_from_inflation * solution[pi[t]] +
            shocks.debt[t]
        )
    end
    return (
        output = Vector(solution[x]),
        inflation = Vector(solution[pi]),
        debt = end_debt,
        monetary = Vector(solution[i]),
        fiscal = Vector(solution[g]),
        condition_number = cond(matrix),
    )
end


"""Evaluate arbitrary outcome and control paths using social welfare."""
function social_loss_value(
    weights::LossWeights,
    output,
    inflation,
    debt,
    monetary,
    fiscal,
)
    horizon = length(output)
    differences_i = vcat(monetary[1], diff(monetary), -monetary[end])
    differences_g = vcat(fiscal[1], diff(fiscal), -fiscal[end])
    total = 0.0
    for t in 1:horizon
        total += weights.discount^(t - 1) * (
            weights.output * output[t]^2 +
            weights.inflation * inflation[t]^2 +
            weights.debt * debt[t]^2 +
            weights.monetary_level * monetary[t]^2 +
            weights.fiscal_level * fiscal[t]^2 +
            weights.monetary_change * differences_i[t]^2 +
            weights.fiscal_change * differences_g[t]^2
        )
    end
    total += weights.discount^horizon * (
        weights.monetary_change * differences_i[end]^2 +
        weights.fiscal_change * differences_g[end]^2
    )
    return total
end


# The recursive game is kept in a separate source file because it implements a
# distinct equilibrium concept from the complete-path, open-loop game above.
include("recursive_game.jl")

end # module

using Test
using LinearAlgebra

include(joinpath(@__DIR__, "..", "coordination_model.jl"))
using .CoordinationModel

function test_primitives(divergence = 0.5)
    economy = MacroCalibration(
        sigma = 0.04,
        fiscal_multiplier = 0.05,
        kappa = 0.14,
    )
    social = LossWeights(
        inflation = 1.0,
        output = 0.5,
        debt = 0.15,
        monetary_level = 0.02,
        fiscal_level = 0.02,
        monetary_change = 0.05,
        fiscal_change = 0.04,
    )
    monetary, fiscal = mandate_weights(social, divergence)
    return ModelPrimitives(economy, social, monetary, fiscal)
end

@testset "Forward-looking macro block" begin
    model = test_primitives()
    horizon = 12
    zero = zeros(horizon)
    output, inflation, debt = macro_outcomes(
        model.economy, zero, zero, ShockPaths(zero, zero, zero),
    )
    @test output == zero
    @test inflation == zero
    @test debt == zero

    demand = copy(zero)
    demand[1] = -1.0
    output, inflation, debt = macro_outcomes(
        model.economy, zero, zero, ShockPaths(demand, zero, zero),
    )
    @test output[1] < 0
    @test inflation[1] < 0
    @test all(isfinite, vcat(output, inflation, debt))
end

@testset "Strategic games and welfare" begin
    model = test_primitives()
    horizon = 16
    demand = zeros(horizon)
    demand[1] = -0.5
    shocks = ShockPaths(demand, zeros(horizon), zeros(horizon))
    results = solve_games(model, shocks)
    @test Set(keys(results)) == Set([
        "cooperation", "nash", "monetary_leader", "fiscal_leader",
    ])
    @test all(length(results[name].monetary) == horizon for name in keys(results))
    @test all(isfinite(results[name].social_loss) for name in keys(results))
    @test results["cooperation"].social_loss <= results["nash"].social_loss + 1e-9
    @test results["cooperation"].social_loss <= results["monetary_leader"].social_loss + 1e-9
    @test results["cooperation"].social_loss <= results["fiscal_leader"].social_loss + 1e-9

    diagnostic = strategic_diagnostics(model, horizon)
    @test diagnostic.spectral_radius >= 0.0
    @test diagnostic.spectral_radius < 1.0
    @test diagnostic.monetary_norm > 0.0
    @test diagnostic.fiscal_norm > 0.0
end

@testset "Only mandate weights vary" begin
    model = test_primitives(0.6)
    @test model.monetary.inflation > model.social.inflation
    @test model.monetary.output < model.social.output
    @test model.fiscal.inflation < model.social.inflation
    @test model.fiscal.output > model.social.output
    for field in [
        :debt, :monetary_level, :fiscal_level,
        :monetary_change, :fiscal_change, :discount,
    ]
        @test getfield(model.monetary, field) == getfield(model.social, field)
        @test getfield(model.fiscal, field) == getfield(model.social, field)
    end
end

@testset "Five-equation rule closure" begin
    model = test_primitives()
    horizon = 12
    supply = zeros(horizon)
    supply[1] = 0.5
    shocks = ShockPaths(zeros(horizon), supply, zeros(horizon))
    rule = RuleCoefficients(
        name = "test",
        monetary_smoothing = 0.6,
        inflation_response = 1.5,
        output_response = 0.25,
        debt_accommodation = 0.0,
        fiscal_smoothing = 0.4,
        debt_response = 0.3,
        output_response_fiscal = 0.5,
    )
    result = solve_rule_system(model.economy, rule, shocks)
    @test length(result.output) == horizon
    @test all(isfinite, vcat(
        result.output, result.inflation, result.debt,
        result.monetary, result.fiscal,
    ))
    @test result.condition_number > 0.0
end

@testset "Recursive feedback and commitment game" begin
    model = test_primitives()
    calibration = RecursiveCalibration(
        economy = model.economy,
        output_persistence = 0.63,
        inflation_persistence = 0.56,
    )
    A, Bm, Bf, outcomes = recursive_matrices(calibration)
    @test size(A) == (7, 7)
    @test length(Bm) == length(Bf) == 7
    @test size(outcomes) == (3, 9)

    nash = solve_feedback_nash(model, calibration)
    cooperation = solve_feedback_cooperation(model, calibration)
    @test nash.converged
    @test cooperation.converged
    @test nash.spectral_radius < 1.0
    @test cooperation.spectral_radius < 1.0
    @test all(isfinite, vcat(nash.monetary_rule, nash.fiscal_rule))

    zero_path = simulate_feedback(nash, zeros(7); horizon = 12)
    @test all(iszero, vcat(
        zero_path.output, zero_path.inflation, zero_path.debt,
        zero_path.monetary, zero_path.fiscal,
    ))

    # Without delegated adjustment costs, lagged instruments cease to enter
    # either feedback rule.  This is the policy-history channel of the model.
    flexible = solve_feedback_nash(
        model,
        calibration;
        monetary_adjustment = 0.0,
        fiscal_adjustment = 0.0,
    )
    @test abs(flexible.monetary_rule[4]) < 1e-10
    @test abs(flexible.fiscal_rule[5]) < 1e-10

    shock = [0.0, 0.0, 0.0, 0.0, 0.0, -0.5, 0.0]
    nash_value = dot(
        shock,
        CoordinationModel.fixed_policy_value(model.social, nash, calibration) * shock,
    )
    cooperative_value = dot(shock, cooperation.value * shock)
    @test cooperative_value <= nash_value + 1e-8

    institutional = solve_commitment_game(
        model,
        calibration,
        0.5,
        0.7;
        commitment_grid = collect(0.0:0.02:0.10),
        investment_cost = 0.005,
    )
    @test institutional.exact_grid_nash
    @test all(isfinite, institutional.social_objective)
    @test institutional.nash_index[1] in eachindex(institutional.grid)
    @test institutional.social_index[2] in eachindex(institutional.grid)
end

println("Australian monetary-fiscal coordination tests passed")

using AustralianMonetaryFiscalGame
using AustralianMonetaryFiscalGame.AustralianData
using AustralianMonetaryFiscalGame.PolicyGame
using Test


function test_primitives()
    economy = MacroParameters(
        rho_x = 0.63,
        sigma_i = 0.04,
        chi_g = 0.05,
        rho_pi = 0.56,
        kappa = 0.15,
    )
    monetary = Preferences(
        inflation = 1.00,
        output = 0.25,
        debt = 0.00,
        adjustment = 0.12,
    )
    fiscal = Preferences(
        inflation = 0.25,
        output = 1.00,
        debt = 0.45,
        adjustment = 0.08,
    )
    return ModelPrimitives(economy, monetary, fiscal)
end


@testset "state transition" begin
    primitives = test_primitives()
    horizon = 8
    states = simulate(
        primitives.economy,
        zeros(6),
        zeros(horizon),
        zeros(horizon),
        zeros(horizon, 6),
    )
    @test states == zeros(horizon + 1, 6)
end


@testset "strategic equilibria" begin
    primitives = test_primitives()
    horizon = 10
    initial_state = zeros(6)
    initial_state[2] = 0.7
    shocks = zeros(horizon, 6)
    games = solve_games(primitives, initial_state, shocks)

    @test Set(keys(games)) == Set([
        "nash",
        "cooperative",
        "central_bank_leader",
        "fiscal_leader",
    ])
    @test size(games["nash"].states) == (horizon + 1, 6)
    @test all(isfinite, games["nash"].monetary)
    @test all(isfinite, games["nash"].fiscal)

    # The cooperative solution minimises one half of each primitive loss, so it
    # must weakly reduce their unweighted sum relative to Nash.
    nash_joint = games["nash"].monetary_loss + games["nash"].fiscal_loss
    cooperative_joint = (
        games["cooperative"].monetary_loss +
        games["cooperative"].fiscal_loss
    )
    @test cooperative_joint <= nash_joint + 1.0e-10

    # Check unilateral local optimality directly around the Nash paths.
    base, mapping = PolicyGame.state_mapping(
        primitives.economy,
        initial_state,
        shocks,
    )
    monetary_loss = PolicyGame.loss_quadratic(
        primitives.monetary_preferences,
        base,
        mapping,
        initial_state,
        :monetary,
    )
    fiscal_loss = PolicyGame.loss_quadratic(
        primitives.fiscal_preferences,
        base,
        mapping,
        initial_state,
        :fiscal,
    )
    controls = vcat(games["nash"].monetary, games["nash"].fiscal)
    for (index, loss) in [(1, monetary_loss), (horizon + 1, fiscal_loss)]
        perturbation = zeros(2 * horizon)
        perturbation[index] = 1.0e-4
        baseline = PolicyGame.value(loss, controls)
        @test PolicyGame.value(loss, controls + perturbation) >= baseline - 1.0e-10
        @test PolicyGame.value(loss, controls - perturbation) >= baseline - 1.0e-10
    end
end


@testset "fixed objectives across policy-rule regimes" begin
    primitives = test_primitives()
    horizon = 12
    initial_state = zeros(6)
    initial_state[2] = 0.7
    initial_state[3] = 1.0
    shocks = zeros(horizon, 6)

    monetary_dominance = RuleRegime(
        name = "monetary_dominance",
        nominal_inflation_response = 1.5,
        monetary_output_response = 0.25,
        monetary_smoothing = 0.6,
        monetary_debt_accommodation = 0.0,
        fiscal_debt_response = 0.35,
        fiscal_output_response = 0.8,
        fiscal_smoothing = 0.4,
    )
    fiscal_dominance = RuleRegime(
        name = "fiscal_dominance",
        nominal_inflation_response = 0.8,
        monetary_output_response = 0.1,
        monetary_smoothing = 0.6,
        monetary_debt_accommodation = 0.1,
        fiscal_debt_response = 0.0,
        fiscal_output_response = 0.8,
        fiscal_smoothing = 0.4,
    )
    md = simulate_policy_rules(primitives, monetary_dominance, initial_state, shocks)
    fd = simulate_policy_rules(primitives, fiscal_dominance, initial_state, shocks)

    # This checks the regime change operates through rules: fiscal dominance
    # accommodates debt with easier money and lacks fiscal debt feedback.
    @test fd.monetary[1] < md.monetary[1]
    @test abs(fd.fiscal[1]) < abs(md.fiscal[1])
    @test isfinite(md.monetary_loss)
    @test isfinite(fd.monetary_loss)
end


@testset "Australian data snapshot" begin
    raw = joinpath(dirname(@__DIR__), "data", "raw")
    data, neutral = prepare_quarterly_data(raw)
    estimates, _ = estimate_macro_block(data)
    @test size(data, 1) > 100
    @test neutral > 0.0
    @test estimates["sigma_i"] > 0.0
    @test estimates["chi_g"] > 0.0
    @test estimates["kappa"] > 0.0
end

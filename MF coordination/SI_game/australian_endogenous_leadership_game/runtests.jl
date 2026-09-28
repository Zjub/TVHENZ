using LinearAlgebra
using Test

include(joinpath(@__DIR__, "endogenous_leadership.jl"))
using .EndogenousLeadership

model = GameModel()
static = solve_static_games(model)
flexible = Regime(name="flexible")
monetary_cost = Regime(name="monetary", monetary_adjustment=0.05)
fiscal_cost = Regime(name="fiscal", fiscal_adjustment=0.04)
bilateral = Regime(name="bilateral", monetary_adjustment=0.05,
                   fiscal_adjustment=0.04)

@testset "static games" begin
    Qm = authority_loss(model, flexible, :monetary)
    Qf = authority_loss(model, flexible, :fiscal)
    state = [0.2, -0.1, 0.3, 0.0, 0.0, -0.4, 0.25]

    # Simultaneous Nash first-order conditions hold for an arbitrary state.
    nash = static["Static Nash"]
    joint = vcat(state, dot(nash.monetary_rule, state),
                 dot(nash.fiscal_rule, state))
    @test abs(dot(Qm[8, :], joint)) < 1e-10
    @test abs(dot(Qf[9, :], joint)) < 1e-10

    # In each Stackelberg game, the announced follower really is on its
    # within-period best-response function.
    monetary_leader = static["Monetary leader"]
    joint_m = vcat(state, dot(monetary_leader.monetary_rule, state),
                   dot(monetary_leader.fiscal_rule, state))
    @test abs(dot(Qf[9, :], joint_m)) < 1e-10

    fiscal_leader = static["Fiscal leader"]
    joint_f = vcat(state, dot(fiscal_leader.monetary_rule, state),
                   dot(fiscal_leader.fiscal_rule, state))
    @test abs(dot(Qm[8, :], joint_f)) < 1e-10
end

@testset "fixed primitives and ownership of costs" begin
    Qm0 = authority_loss(model, flexible, :monetary)
    Qf0 = authority_loss(model, flexible, :fiscal)
    Qmm = authority_loss(model, monetary_cost, :monetary)
    Qfm = authority_loss(model, monetary_cost, :fiscal)
    Qmf = authority_loss(model, fiscal_cost, :monetary)
    Qff = authority_loss(model, fiscal_cost, :fiscal)

    # A monetary adjustment cost changes only the monetary authority's loss;
    # a fiscal adjustment cost changes only the fiscal authority's loss.
    @test norm(Qfm - Qf0) < 1e-12
    @test norm(Qmf - Qm0) < 1e-12
    @test norm(Qmm - Qm0) > 0.0
    @test norm(Qff - Qf0) > 0.0

    # Mandate and social weights are model primitives and are not altered by
    # the regime definitions.
    @test model.monetary.inflation == 1.5
    @test model.fiscal.output == 0.75
    @test model.social.inflation == 1.0
end

@testset "stationary Markov-perfect equilibrium" begin
    solutions = Dict(
        "flexible" => solve_stationary_nash(model, flexible),
        "monetary" => solve_stationary_nash(model, monetary_cost),
        "fiscal" => solve_stationary_nash(model, fiscal_cost),
        "bilateral" => solve_stationary_nash(model, bilateral),
    )
    for solution in values(solutions)
        @test solution.metadata.converged
        @test solution.metadata.residual < 1e-9
        @test solution.spectral_radius < 1.0
    end

    # With no adjustment costs, inherited instrument levels are irrelevant.
    @test maximum(abs, solutions["flexible"].monetary_rule[4:5]) < 1e-9
    @test maximum(abs, solutions["flexible"].fiscal_rule[4:5]) < 1e-9

    # Once an instrument is costly to adjust, its inherited value becomes a
    # genuine state and changes equilibrium feedback behaviour.
    @test abs(solutions["monetary"].monetary_rule[4]) > 1e-4
    @test abs(solutions["fiscal"].fiscal_rule[5]) > 1e-4

    search = search_stationary_nash(model, bilateral)
    @test length(search.solutions) == 1
    @test all(attempt.metadata.converged for attempt in search.attempts)
    @test maximum(rule_distance(attempt, first(search.solutions))
                  for attempt in search.attempts) < 1e-7

    state = [0.0, 0.0, 0.0, 0.0, 0.0, -0.46387449164984773, 0.0]
    monetary_wedge = strategic_wedge_share(solutions["bilateral"], model,
                                           bilateral, state, :monetary)
    fiscal_wedge = strategic_wedge_share(solutions["bilateral"], model,
                                         bilateral, state, :fiscal)
    @test 0.0 <= monetary_wedge.bounded_share <= 1.0
    @test 0.0 <= fiscal_wedge.bounded_share <= 1.0
    @test isapprox(monetary_wedge.total_continuation,
                   monetary_wedge.strategic + monetary_wedge.nonstrategic;
                   atol=1e-12)
    @test isapprox(fiscal_wedge.total_continuation,
                   fiscal_wedge.strategic + fiscal_wedge.nonstrategic;
                   atol=1e-12)
end

@testset "cooperation and simulation" begin
    cooperative_regime = Regime(name="coordination")
    cooperative = solve_stationary_cooperation(model, cooperative_regime)
    @test cooperative.metadata.converged
    @test cooperative.spectral_radius < 1.0
    state = [0.0, 0.0, 0.0, 0.0, 0.0, -0.46387449164984773, 0.0]
    path = simulate_rule(cooperative, model, state; horizon=40)
    values = evaluate_path(path, model, cooperative_regime)
    @test length(path.monetary) == 40
    @test all(isfinite, path.states)
    @test values.social >= values.common_macro >= 0.0
end

println("All endogenous-leadership game tests passed.")

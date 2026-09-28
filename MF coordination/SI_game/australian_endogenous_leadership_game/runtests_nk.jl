using LinearAlgebra
using Test

include(joinpath(@__DIR__, "endogenous_leadership.jl"))
using .EndogenousLeadership
include(joinpath(@__DIR__, "nk_forward_looking_game.jl"))
using .ForwardLookingNK

model = NKModel()
flexible = Regime(name="Flexible Nash")
bilateral = Regime(name="Bilateral commitment",
                   monetary_adjustment=0.05, fiscal_adjustment=0.04)

@testset "homotopy endpoint" begin
    old = solve_stationary_nash(GameModel(), flexible)
    new = solve_nk_nash(model, flexible, 0.0)
    @test new.metadata.outer_converged
    @test norm(old.monetary_rule - new.monetary_rule) < 1e-10
    @test norm(old.fiscal_rule - new.fiscal_rule) < 1e-10
    @test norm(old.transition - new.transition) < 1e-10
end

@testset "private-sector rational expectations" begin
    solution = solve_nk_nash(model, bilateral, 1.0)
    reduction = solution.metadata.reduction
    implied = reduction.outcomes[:, 1:7] +
              reduction.outcomes[:, 8] * solution.monetary_rule' +
              reduction.outcomes[:, 9] * solution.fiscal_rule'
    @test solution.metadata.outer_converged
    @test maximum(abs, implied - solution.metadata.expectations) < 1e-8
    @test solution.spectral_radius < 1.0

    # Verify the two forward-looking private equations at an arbitrary state.
    state = [0.1, -0.2, 0.3, 0.05, -0.04, -0.25, 0.15]
    m = dot(solution.monetary_rule, state)
    f = dot(solution.fiscal_rule, state)
    next_state = solution.transition * state
    x, pi = next_state[1], next_state[2]
    expected = solution.metadata.expectations * next_state
    c = model.calibration
    @test isapprox(x, expected[1] - c.sigma * (m - expected[2]) +
                      c.fiscal_multiplier * f + state[6]; atol=1e-9)
    @test isapprox(pi, c.private_discount * expected[2] + c.kappa * x +
                       state[7]; atol=1e-9)
end

@testset "policy regimes and strategic decomposition" begin
    regimes = [flexible,
        Regime(name="M", monetary_adjustment=0.05),
        Regime(name="F", fiscal_adjustment=0.04), bilateral]
    state = [0.0, 0.0, 0.0, 0.0, 0.0, -0.46387449164984773, 0.0]
    for regime in regimes
        solution = solve_nk_nash(model, regime, 1.0)
        @test solution.metadata.outer_converged
        @test solution.metadata.inner_converged
        @test solution.spectral_radius < 1.0
        for authority in (:monetary, :fiscal)
            wedge = nk_strategic_wedge(solution, model, state, authority)
            @test isapprox(wedge.strategic,
                           sum(wedge.channel_components); atol=1e-12)
            @test isapprox(wedge.total_continuation,
                           wedge.strategic + wedge.nonstrategic; atol=1e-12)
            @test 0.0 <= wedge.bounded_share <= 1.0
        end
    end

    cooperative = solve_nk_cooperation(model, Regime(name="Coordination"), 1.0)
    @test cooperative.metadata.outer_converged
    @test cooperative.spectral_radius < 1.0
    path = simulate_nk(cooperative, state; horizon=40)
    value = evaluate_nk_path(path, model, Regime(name="Coordination"), cooperative)
    @test all(isfinite, path.states)
    @test value.social >= 0.0
end

@testset "equilibrium search" begin
    reference = solve_nk_nash(model, bilateral, 0.0).metadata.expectations
    search = search_nk_nash(model, bilateral, 1.0, reference;
                            scales=[-0.5, 0.0, 1.0, 2.0])
    @test length(search.attempts) == 4
    @test length(search.solutions) == 1
    @test all(solution.metadata.outer_converged for solution in search.attempts)
end

println("All forward-looking NK extension tests passed.")

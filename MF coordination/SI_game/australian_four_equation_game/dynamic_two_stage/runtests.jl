using Test
using LinearAlgebra

include(joinpath(@__DIR__, "dynamic_game.jl"))
using .DynamicStrategicInvestment

function test_model()
    c = Calibration()
    social = LossWeights(inflation=1.0, output=0.5, debt=0.15,
        monetary_level=0.1, fiscal_level=0.1,
        monetary_change=0.05, fiscal_change=0.04)
    monetary = LossWeights(inflation=1.5, output=0.25, debt=0.15,
        monetary_level=0.1, fiscal_level=0.1,
        monetary_change=0.05, fiscal_change=0.04)
    fiscal = LossWeights(inflation=0.5, output=0.75, debt=0.15,
        monetary_level=0.1, fiscal_level=0.1,
        monetary_change=0.05, fiscal_change=0.04)
    Model(calibration=c, monetary=monetary, fiscal=fiscal, social=social)
end

@testset "Dynamic strategic-investment game" begin
    model = test_model()
    demand = [0.0, 0.0, 0.0, 0.0, 0.0, -0.46387449164984773, 0.0]
    A, bm, bf, _ = model_matrices(model.calibration)
    @test size(A) == (7, 7)
    @test bm[4] == 1.0
    @test bf[5] == 1.0

    two = solve_feedback(model, 2)
    @test two.horizon == 2
    @test all(isfinite, two.monetary_rule[1])
    @test all(isfinite, two.fiscal_rule[1])

    # The terminal rules satisfy both simultaneous stage first-order conditions.
    Qm = stage_loss(model.monetary, model.calibration)
    Qf = stage_loss(model.fiscal, model.calibration)
    joint_rule = vcat(Matrix{Float64}(I, 7, 7),
                      reshape(two.monetary_rule[2], 1, :),
                      reshape(two.fiscal_rule[2], 1, :))
    @test maximum(abs, Qm[8, :]' * joint_rule) < 1e-10
    @test maximum(abs, Qf[9, :]' * joint_rule) < 1e-10

    diag = two_stage_diagnostics(model, demand)
    @test diag.fiscal_response_to_monetary_investment > 0
    @test diag.monetary_response_to_fiscal_investment > 0
    @test abs(diag.monetary_strategic_wedge) > 0
    @test abs(diag.fiscal_strategic_wedge) > 0

    feedback_path = simulate_feedback(two, model, demand)
    open_solution = solve_open_loop(model, demand, 2)
    open_path = simulate_open_loop(open_solution, model, demand)
    @test norm([feedback_path.monetary[1] - open_path.monetary[1],
                feedback_path.fiscal[1] - open_path.fiscal[1]]) > 1e-6

    search = solve_stationary_multistart(model)
    @test length(search.solutions) == 1
    @test all(attempt.converged for attempt in search.attempts)
    stationary = first(search.solutions)
    @test stationary.spectral_radius < 1

    long = solve_feedback(model, 100)
    @test norm(vcat(long.monetary_rule[1] - stationary.monetary_rule,
                    long.fiscal_rule[1] - stationary.fiscal_rule)) < 1e-4

    cooperative = solve_cooperation(model, 40)
    fb40 = simulate_feedback(solve_feedback(model, 40), model, demand)
    coop40 = simulate_feedback(cooperative, model, demand)
    @test evaluate_path(coop40, model).social <= evaluate_path(fb40, model).social + 1e-10
end

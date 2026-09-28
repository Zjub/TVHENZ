#!/usr/bin/env julia

# Reproduce the Australian calibration, policy games, active/passive rule
# comparison, tables and figures used in the accompanying paper.

using CSV
using DataFrames
using Dates
using LinearAlgebra
using Plots
using Printf
using Statistics
using TOML

include("coordination_model.jl")
using .CoordinationModel

# Reuse the auditable RBA downloader/parser from the preceding project.  The
# strategic model is independent; only the common data engineering is shared.
include(joinpath(@__DIR__, "..", "australian_three_equation_game", "data_pipeline.jl"))
using .AustralianData


const ROOT = @__DIR__
const SHARED_DATA = normpath(joinpath(ROOT, "..", "australian_three_equation_game", "data"))
const RAW = joinpath(SHARED_DATA, "raw")
const OUTPUT = joinpath(ROOT, "output")
const HORIZON = 24
const BASELINE_DIVERGENCE = 0.50

const STRATEGIES = ["cooperation", "nash", "monetary_leader", "fiscal_leader"]
const LABELS = Dict(
    "cooperation" => "Social cooperation",
    "nash" => "Nash",
    "monetary_leader" => "Monetary leader",
    "fiscal_leader" => "Fiscal leader",
)
const COLOURS = Dict(
    "cooperation" => :seagreen,
    "nash" => :black,
    "monetary_leader" => :darkorange,
    "fiscal_leader" => :midnightblue,
)
const STYLES = Dict(
    "cooperation" => :solid,
    "nash" => :dash,
    "monetary_leader" => :dot,
    "fiscal_leader" => :dashdot,
)


"""Create one-period innovations at Australian empirical shock scales."""
function scenarios(estimates)
    demand = zeros(HORIZON)
    demand[1] = -estimates["demand_shock_sd"]
    supply = zeros(HORIZON)
    supply[1] = estimates["supply_shock_sd"]
    debt = zeros(HORIZON)
    debt[1] = 1.0
    zeros_path = zeros(HORIZON)
    return Dict(
        "demand_contraction" => ShockPaths(demand, zeros_path, zeros_path),
        "cost_push_inflation" => ShockPaths(zeros_path, supply, zeros_path),
        "debt_stress" => ShockPaths(zeros_path, zeros_path, debt),
        "inflation_and_debt" => ShockPaths(zeros_path, supply, debt),
    )
end


"""Plot the public Australian series used for the semi-structural calibration."""
function plot_calibration_data(data::DataFrame)
    rows = (data.date .>= Date(1993, 3, 31)) .& (data.date .<= Date(2019, 12, 31))
    sample = data[rows, :]
    panels = [
        plot(sample.date, sample.output_gap; title = "Output gap", ylabel = "%"),
        plot(sample.date, sample.inflation_gap; title = "Trimmed-mean inflation gap", ylabel = "ppt"),
        plot(sample.date, sample.real_rate_gap; title = "Real cash-rate gap", ylabel = "ppt"),
        plot(sample.date, sample.public_demand_gap; title = "Public-demand gap", ylabel = "%"),
    ]
    for panel in panels
        hline!(panel, [0.0]; color = :grey70, linewidth = 0.8, label = false)
        plot!(panel; legend = false, grid = :y, framestyle = :box)
    end
    figure = plot(
        panels...;
        layout = (2, 2),
        size = (1100, 720),
        plot_title = "Australian calibration data, 1993Q1–2019Q4",
        left_margin = 3Plots.mm,
    )
    savefig(figure, joinpath(OUTPUT, "australian_calibration_data.png"))
end


"""Plot impulse responses for the four strategic solution concepts."""
function plot_game(results::Dict{String, GameResult}, title::String, filename::String)
    quarters = 0:(HORIZON - 1)
    panels = [
        plot(title = "Output gap", ylabel = "%"),
        plot(title = "Inflation gap", ylabel = "ppt annualised"),
        plot(title = "Debt gap", ylabel = "ppt of GDP"),
        plot(title = "Cash-rate gap", ylabel = "ppt"),
        plot(title = "Fiscal expansion", ylabel = "% from trend"),
    ]
    fields = [:output, :inflation, :debt, :monetary, :fiscal]
    for strategy in STRATEGIES
        result = results[strategy]
        for (panel, field) in zip(panels, fields)
            plot!(
                panel,
                quarters,
                getfield(result, field);
                label = LABELS[strategy],
                color = COLOURS[strategy],
                linestyle = STYLES[strategy],
                linewidth = strategy == "cooperation" ? 2.8 : 2.0,
            )
        end
    end
    for panel in panels
        hline!(panel, [0.0]; color = :grey75, linewidth = 0.7, label = false)
        plot!(panel; grid = :y, framestyle = :box)
    end

    welfare = [results[s].social_loss for s in STRATEGIES]
    welfare_panel = bar(
        [LABELS[s] for s in STRATEGIES],
        welfare;
        title = "Social-welfare loss",
        ylabel = "discounted quadratic loss",
        color = [COLOURS[s] for s in STRATEGIES],
        label = false,
        legend = false,
        xrotation = 22,
        framestyle = :box,
    )
    plot!(panels[5]; xlabel = "quarters")
    figure = plot(
        panels[1], panels[2], panels[3], panels[4], panels[5], welfare_panel;
        layout = (3, 2),
        size = (1200, 1050),
        plot_title = title,
        legend = :topright,
        left_margin = 4Plots.mm,
        bottom_margin = 3Plots.mm,
    )
    savefig(figure, joinpath(OUTPUT, filename))
end


"""Plot how mandate divergence changes complementarity and welfare."""
function plot_complementarity_sweep(table::DataFrame)
    p1 = plot(
        table.divergence,
        table.spectral_radius;
        color = :purple4,
        linewidth = 2.5,
        marker = :circle,
        label = "best-response loop",
        title = "Strategic complementarity",
        xlabel = "mandate divergence δ",
        ylabel = "spectral radius",
        framestyle = :box,
    )
    hline!(p1, [1.0]; color = :firebrick, linestyle = :dash, label = "selection boundary")

    p2 = plot(
        table.divergence,
        log10.(1.0 .+ table.demand_nash_welfare_gap);
        color = :navy,
        linewidth = 2.5,
        marker = :diamond,
        label = "demand shock",
        title = "Cost of non-cooperation",
        xlabel = "mandate divergence δ",
        ylabel = "log10(1 + welfare gap %)",
        framestyle = :box,
    )
    plot!(
        p2,
        table.divergence,
        log10.(1.0 .+ table.supply_nash_welfare_gap);
        color = :darkorange,
        linewidth = 2.5,
        marker = :square,
        label = "cost-push shock",
    )

    p3 = plot(
        table.divergence,
        table.demand_nash_monetary_peak;
        color = :black,
        linewidth = 2.2,
        label = "Nash",
        title = "Monetary response to demand shock",
        xlabel = "mandate divergence δ",
        ylabel = "peak absolute cash-rate gap",
        yscale = :log10,
        framestyle = :box,
    )
    plot!(
        p3,
        table.divergence,
        table.demand_cooperative_monetary_peak;
        color = :seagreen,
        linewidth = 2.2,
        linestyle = :dash,
        label = "cooperation",
    )

    p4 = plot(
        table.divergence,
        table.supply_nash_fiscal_peak;
        color = :black,
        linewidth = 2.2,
        label = "Nash",
        title = "Fiscal response to cost-push shock",
        xlabel = "mandate divergence δ",
        ylabel = "peak absolute fiscal gap",
        yscale = :log10,
        framestyle = :box,
    )
    plot!(
        p4,
        table.divergence,
        table.supply_cooperative_fiscal_peak;
        color = :seagreen,
        linewidth = 2.2,
        linestyle = :dash,
        label = "cooperation",
    )

    figure = plot(
        p1, p2, p3, p4;
        layout = (2, 2),
        size = (1150, 800),
        plot_title = "Preference disagreement and the policy game",
        left_margin = 4Plots.mm,
        bottom_margin = 3Plots.mm,
    )
    savefig(figure, joinpath(OUTPUT, "strategic_complementarity.png"))
end


"""Map the coordination boundary over mandate divergence and policy costs."""
function plot_coordination_boundary(grid::DataFrame)
    divergences = sort(unique(grid.divergence))
    costs = sort(unique(grid.instrument_level_cost))
    radius = [
        only(grid.spectral_radius[
            (grid.divergence .== divergence) .&
            (grid.instrument_level_cost .== cost)
        ])
        for cost in costs, divergence in divergences
    ]
    figure = heatmap(
        divergences,
        costs,
        radius;
        xlabel = "mandate divergence δ",
        ylabel = "common instrument-level cost",
        title = "Best-response amplification and the coordination boundary",
        colorbar_title = "spectral radius",
        color = :viridis,
        size = (900, 650),
        framestyle = :box,
    )
    contour!(
        figure,
        divergences,
        costs,
        radius;
        levels = [1.0],
        color = :white,
        linewidth = 3,
        label = "ρ = 1",
    )
    scatter!(
        figure,
        [BASELINE_DIVERGENCE],
        [0.10];
        marker = :star5,
        markersize = 10,
        color = :red,
        markerstrokecolor = :white,
        label = "Australian baseline",
    )
    savefig(figure, joinpath(OUTPUT, "coordination_boundary.png"))
end


"""Show how shocks activate an otherwise dormant strategic interaction."""
function plot_shock_activation(table::DataFrame)
    demand = table[table.shock_type .== "demand", :]
    supply = table[table.shock_type .== "cost_push", :]
    p1 = plot(
        demand.scale,
        demand.excess_social_loss;
        color = :navy,
        linewidth = 2.5,
        marker = :circle,
        label = "demand contraction",
        title = "Absolute welfare cost of Nash",
        xlabel = "shock size / empirical standard deviation",
        ylabel = "Nash loss minus cooperation",
        framestyle = :box,
    )
    plot!(
        p1,
        supply.scale,
        supply.excess_social_loss;
        color = :darkorange,
        linewidth = 2.5,
        marker = :square,
        label = "cost-push shock",
    )
    vline!(p1, [1.0]; color = :grey45, linestyle = :dash, label = "baseline shock")

    p2 = plot(
        demand.scale,
        demand.policy_mix_distance;
        color = :navy,
        linewidth = 2.5,
        marker = :circle,
        label = "demand contraction",
        title = "Distance between Nash and social policy mixes",
        xlabel = "shock size / empirical standard deviation",
        ylabel = "Euclidean path distance",
        framestyle = :box,
    )
    plot!(
        p2,
        supply.scale,
        supply.policy_mix_distance;
        color = :darkorange,
        linewidth = 2.5,
        marker = :square,
        label = "cost-push shock",
    )
    vline!(p2, [1.0]; color = :grey45, linestyle = :dash, label = "baseline shock")
    figure = plot(
        p1, p2;
        layout = (1, 2),
        size = (1150, 470),
        plot_title = "Shocks activate the policy game",
        left_margin = 4Plots.mm,
        bottom_margin = 3Plots.mm,
    )
    savefig(figure, joinpath(OUTPUT, "shock_activation.png"))
end


"""Plot the active/passive policy-rule comparison."""
function plot_rule_regimes(rule_results)
    quarters = 0:(HORIZON - 1)
    fields = [:output, :inflation, :debt, :monetary, :fiscal]
    titles = ["Output gap", "Inflation gap", "Debt gap", "Cash-rate gap", "Fiscal expansion"]
    panels = [plot(title = title) for title in titles]
    colours = Dict("monetary_dominance" => :seagreen, "fiscal_dominance" => :firebrick)
    labels = Dict("monetary_dominance" => "Active M / passive F", "fiscal_dominance" => "Passive M / active F")
    for regime in ["monetary_dominance", "fiscal_dominance"]
        result = rule_results[regime]
        for (panel, field) in zip(panels, fields)
            plot!(
                panel,
                quarters,
                getfield(result, field);
                color = colours[regime],
                linewidth = 2.5,
                linestyle = regime == "monetary_dominance" ? :solid : :dash,
                label = labels[regime],
            )
        end
    end
    for panel in panels
        hline!(panel, [0.0]; color = :grey75, linewidth = 0.7, label = false)
        plot!(panel; framestyle = :box, grid = :y)
    end
    plot!(panels[5]; xlabel = "quarters")
    figure = plot(
        panels[1], panels[2], panels[3], panels[4], panels[5],
        plot(; axis = false, grid = false, framestyle = :none);
        layout = (3, 2),
        size = (1200, 1000),
        plot_title = "Rule-based regimes after an inflation and debt shock",
        legend = :topright,
        left_margin = 4Plots.mm,
    )
    savefig(figure, joinpath(OUTPUT, "active_passive_regimes.png"))
end


"""Plot the first-stage commitment game and its two best-response schedules."""
function plot_commitment_game(game)
    grid = game.grid
    # Display welfare relative to the best institutional design so the contour
    # retains detail even when the level of expected loss is large.
    excess_social = game.social_objective .- minimum(game.social_objective)
    p1 = heatmap(
        grid,
        grid,
        excess_social';
        xlabel = "monetary adjustment coefficient lambda_M",
        ylabel = "fiscal adjustment coefficient lambda_F",
        title = "Social cost of delegated policy inertia",
        colorbar_title = "loss above minimum",
        color = :viridis,
        framestyle = :box,
    )
    monetary_br = grid[game.monetary_best_index]
    fiscal_br = grid[game.fiscal_best_index]
    plot!(
        p1,
        monetary_br,
        grid;
        color = :dodgerblue3,
        linewidth = 3,
        label = "monetary best response",
    )
    plot!(
        p1,
        grid,
        fiscal_br;
        color = :firebrick3,
        linewidth = 3,
        linestyle = :dash,
        label = "fiscal best response",
    )
    nash_m = grid[game.nash_index[1]]
    nash_f = grid[game.nash_index[2]]
    social_m = grid[game.social_index[1]]
    social_f = grid[game.social_index[2]]
    scatter!(
        p1,
        [nash_m],
        [nash_f];
        marker = :star5,
        markersize = 11,
        color = :white,
        markerstrokecolor = :black,
        label = "institutional Nash",
    )
    scatter!(
        p1,
        [social_m],
        [social_f];
        marker = :diamond,
        markersize = 8,
        color = :gold,
        markerstrokecolor = :black,
        label = "social design",
    )

    fiscal_column = game.nash_index[2]
    monetary_row = game.nash_index[1]
    p2 = plot(
        grid,
        game.monetary_objective[:, fiscal_column] .-
            minimum(game.monetary_objective[:, fiscal_column]);
        color = :dodgerblue3,
        linewidth = 2.8,
        label = "monetary authority",
        xlabel = "own delegated adjustment coefficient",
        ylabel = "loss above own minimum",
        title = "Unilateral institutional deviations at Nash",
        framestyle = :box,
    )
    plot!(
        p2,
        grid,
        game.fiscal_objective[monetary_row, :] .-
            minimum(game.fiscal_objective[monetary_row, :]);
        color = :firebrick3,
        linewidth = 2.8,
        linestyle = :dash,
        label = "fiscal authority",
    )
    vline!(p2, [nash_m]; color = :dodgerblue3, linestyle = :dot, label = false)
    vline!(p2, [nash_f]; color = :firebrick3, linestyle = :dot, label = false)
    figure = plot(
        p1,
        p2;
        layout = (1, 2),
        size = (1200, 500),
        plot_title = "Two-stage strategic investment in policy inertia",
        left_margin = 4Plots.mm,
        bottom_margin = 3Plots.mm,
    )
    savefig(figure, joinpath(OUTPUT, "commitment_game.png"))
end


"""Plot recursive impulse responses under alternative institutional designs."""
function plot_recursive_irfs(paths, losses, title, filename)
    quarters = 0:(HORIZON - 1)
    order = [
        "cooperation",
        "flexible_nash",
        "feedback_nash",
        "institutional_nash",
        "social_commitment",
    ]
    labels = Dict(
        "cooperation" => "Social regulator",
        "flexible_nash" => "Nash, no inertia",
        "feedback_nash" => "MPE, primitive inertia",
        "institutional_nash" => "Two-stage Nash",
        "social_commitment" => "Socially chosen inertia",
    )
    colours = Dict(
        "cooperation" => :seagreen,
        "flexible_nash" => :grey45,
        "feedback_nash" => :black,
        "institutional_nash" => :purple4,
        "social_commitment" => :darkorange,
    )
    styles = Dict(
        "cooperation" => :solid,
        "flexible_nash" => :dot,
        "feedback_nash" => :dash,
        "institutional_nash" => :dashdot,
        "social_commitment" => :solid,
    )
    panels = [
        plot(title = "Output gap", ylabel = "%"),
        plot(title = "Inflation gap", ylabel = "ppt annualised"),
        plot(title = "Debt gap", ylabel = "ppt of GDP"),
        plot(title = "Cash-rate gap", ylabel = "ppt"),
        plot(title = "Fiscal expansion", ylabel = "% from trend"),
    ]
    fields = [:output, :inflation, :debt, :monetary, :fiscal]
    for regime in order
        for (panel, field) in zip(panels, fields)
            plot!(
                panel,
                quarters,
                getfield(paths[regime], field);
                color = colours[regime],
                linestyle = styles[regime],
                linewidth = regime in ["cooperation", "social_commitment"] ? 2.8 : 2.0,
                label = labels[regime],
            )
        end
    end
    for panel in panels
        hline!(panel, [0.0]; color = :grey75, linewidth = 0.7, label = false)
        plot!(panel; framestyle = :box, grid = :y)
    end
    loss_panel = bar(
        [labels[name] for name in order],
        [losses[name] for name in order];
        title = "Expected social loss",
        ylabel = "discounted quadratic loss",
        color = [colours[name] for name in order],
        label = false,
        xrotation = 24,
        framestyle = :box,
    )
    figure = plot(
        panels[1], panels[2], panels[3], panels[4], panels[5], loss_panel;
        layout = (3, 2),
        size = (1200, 1050),
        plot_title = title,
        legend = :topright,
        left_margin = 4Plots.mm,
        bottom_margin = 3Plots.mm,
    )
    savefig(figure, joinpath(OUTPUT, filename))
end


function main()
    mkpath(OUTPUT)
    refresh = "--refresh-data" in ARGS
    obtain_raw_data(RAW; refresh = refresh)
    data, neutral_real_rate = prepare_quarterly_data(RAW)
    estimates, estimate_table = estimate_macro_block(data)
    CSV.write(joinpath(OUTPUT, "estimated_coefficients.csv"), estimate_table)

    economy = MacroCalibration(
        beta = 0.99,
        sigma = estimates["sigma_i"],
        fiscal_multiplier = estimates["chi_g"],
        kappa = estimates["kappa"],
        rho_b = 0.995,
        debt_from_fiscal = 0.060,
        debt_from_interest = 0.015,
        debt_from_inflation = 0.100,
    )

    # This social loss is the common welfare yardstick.  Mandate divergence
    # changes only the relative inflation/output weights of the two agencies.
    social = LossWeights(
        inflation = 1.00,
        output = 0.50,
        debt = 0.15,
        # Level costs prevent offsetting monetary/fiscal instruments from
        # becoming a nearly costless policy mix.  A sensitivity map below
        # reports what happens as these costs approach zero.
        monetary_level = 0.100,
        fiscal_level = 0.100,
        monetary_change = 0.050,
        fiscal_change = 0.040,
    )
    monetary, fiscal = mandate_weights(social, BASELINE_DIVERGENCE)
    primitives = ModelPrimitives(economy, social, monetary, fiscal)
    shocks = scenarios(estimates)

    # ------------------------------------------------------------------
    # Recursive MPE and the first-stage commitment-technology game.
    # ------------------------------------------------------------------
    recursive_calibration = RecursiveCalibration(
        economy = economy,
        output_persistence = estimates["rho_x"],
        inflation_persistence = estimates["rho_pi"],
    )
    feedback_nash = solve_feedback_nash(primitives, recursive_calibration)
    feedback_cooperation = solve_feedback_cooperation(primitives, recursive_calibration)
    feedback_nash.converged || error("Feedback Nash solution did not converge")
    feedback_cooperation.converged || error("Cooperative feedback solution did not converge")

    # The chosen coefficient is delegated to future policymakers.  True
    # adjustment costs in the ex ante mandates and social welfare remain fixed.
    commitment = solve_commitment_game(
        primitives,
        recursive_calibration,
        estimates["demand_shock_sd"],
        estimates["supply_shock_sd"];
        commitment_grid = collect(0.0:0.005:0.15),
        investment_cost = 0.005,
    )
    lambda_m_nash = commitment.grid[commitment.nash_index[1]]
    lambda_f_nash = commitment.grid[commitment.nash_index[2]]
    lambda_m_social = commitment.grid[commitment.social_index[1]]
    lambda_f_social = commitment.grid[commitment.social_index[2]]
    institutional_nash = solve_feedback_nash(
        primitives,
        recursive_calibration;
        monetary_adjustment = lambda_m_nash,
        fiscal_adjustment = lambda_f_nash,
    )
    social_commitment = solve_feedback_nash(
        primitives,
        recursive_calibration;
        monetary_adjustment = lambda_m_social,
        fiscal_adjustment = lambda_f_social,
    )
    flexible_nash = solve_feedback_nash(
        primitives,
        recursive_calibration;
        monetary_adjustment = 0.0,
        fiscal_adjustment = 0.0,
    )

    recursive_regimes = Dict(
        "cooperation" => feedback_cooperation,
        "flexible_nash" => flexible_nash,
        "feedback_nash" => feedback_nash,
        "institutional_nash" => institutional_nash,
        "social_commitment" => social_commitment,
    )
    initial_recursive_shocks = Dict(
        "demand_contraction" => [0.0, 0.0, 0.0, 0.0, 0.0, -estimates["demand_shock_sd"], 0.0],
        "cost_push_inflation" => [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, estimates["supply_shock_sd"]],
    )
    recursive_paths = Dict{String, Dict{String, NamedTuple}}()
    recursive_losses = Dict{String, Dict{String, Float64}}()
    recursive_summary = DataFrame(
        scenario = String[], regime = String[], social_loss = Float64[],
        spectral_radius = Float64[], peak_output = Float64[],
        peak_inflation = Float64[], peak_monetary = Float64[], peak_fiscal = Float64[],
    )
    for scenario in ["demand_contraction", "cost_push_inflation"]
        state = initial_recursive_shocks[scenario]
        paths = Dict{String, NamedTuple}()
        losses = Dict{String, Float64}()
        for (name, result) in recursive_regimes
            path = simulate_feedback(result, state; horizon = HORIZON)
            value_matrix = CoordinationModel.fixed_policy_value(
                social, result, recursive_calibration,
            )
            social_value = dot(state, value_matrix * state)
            paths[name] = path
            losses[name] = social_value
            push!(recursive_summary, (
                scenario, name, social_value, result.spectral_radius,
                maximum(abs, path.output), maximum(abs, path.inflation),
                maximum(abs, path.monetary), maximum(abs, path.fiscal),
            ))
        end
        recursive_paths[scenario] = paths
        recursive_losses[scenario] = losses
    end
    CSV.write(joinpath(OUTPUT, "recursive_game_summary.csv"), recursive_summary)

    rule_summary_recursive = DataFrame(
        regime = String[], policy = String[], state = String[], coefficient = Float64[],
    )
    state_names = [
        "lagged_output", "lagged_inflation", "debt", "lagged_monetary",
        "lagged_fiscal", "demand_shock", "cost_push_shock",
    ]
    for (name, result) in recursive_regimes
        for (policy, rule) in [
            ("monetary", result.monetary_rule),
            ("fiscal", result.fiscal_rule),
        ], (state_name, coefficient) in zip(state_names, rule)
            push!(rule_summary_recursive, (name, policy, state_name, coefficient))
        end
    end
    CSV.write(joinpath(OUTPUT, "recursive_policy_rules.csv"), rule_summary_recursive)

    commitment_table = DataFrame(
        monetary_adjustment = Float64[], fiscal_adjustment = Float64[],
        monetary_objective = Float64[], fiscal_objective = Float64[],
        social_objective = Float64[], spectral_radius = Float64[],
    )
    for (mi, lambda_m) in enumerate(commitment.grid),
        (fi, lambda_f) in enumerate(commitment.grid)
        push!(commitment_table, (
            lambda_m,
            lambda_f,
            commitment.monetary_objective[mi, fi],
            commitment.fiscal_objective[mi, fi],
            commitment.social_objective[mi, fi],
            commitment.stability[mi, fi],
        ))
    end
    CSV.write(joinpath(OUTPUT, "commitment_game.csv"), commitment_table)

    # Main strategic solutions and a tidy results table.
    all_results = Dict{String, Dict{String, GameResult}}()
    summary = DataFrame(
        scenario = String[], strategy = String[],
        monetary_loss = Float64[], fiscal_loss = Float64[], social_loss = Float64[],
        peak_output = Float64[], peak_inflation = Float64[], peak_debt = Float64[],
        peak_monetary = Float64[], peak_fiscal = Float64[],
    )
    for scenario in ["demand_contraction", "cost_push_inflation", "debt_stress"]
        results = solve_games(primitives, shocks[scenario])
        all_results[scenario] = results
        for strategy in STRATEGIES
            r = results[strategy]
            push!(summary, (
                scenario, strategy, r.monetary_loss, r.fiscal_loss, r.social_loss,
                maximum(abs, r.output), maximum(abs, r.inflation), maximum(abs, r.debt),
                maximum(abs, r.monetary), maximum(abs, r.fiscal),
            ))
        end
    end
    CSV.write(joinpath(OUTPUT, "game_summary.csv"), summary)

    # With aligned targets and no shock, every policy arrangement has the same
    # zero steady state.  The reaction slopes remain latent, but no intercept
    # activates them.  This table makes that distinction explicit.
    zero_path = zeros(HORIZON)
    steady_results = solve_games(
        primitives, ShockPaths(zero_path, zero_path, zero_path),
    )
    steady_summary = DataFrame(
        strategy = String[], social_loss = Float64[],
        max_abs_output = Float64[], max_abs_inflation = Float64[],
        max_abs_monetary = Float64[], max_abs_fiscal = Float64[],
    )
    for strategy in STRATEGIES
        result = steady_results[strategy]
        push!(steady_summary, (
            strategy, result.social_loss,
            maximum(abs, result.output), maximum(abs, result.inflation),
            maximum(abs, result.monetary), maximum(abs, result.fiscal),
        ))
    end
    CSV.write(joinpath(OUTPUT, "steady_state_summary.csv"), steady_summary)

    # Scale each empirical shock from zero.  In this linear-quadratic model,
    # policy paths scale linearly and absolute welfare gaps scale quadratically.
    # The graph separates a peaceful steady state from conflict conditional on
    # a disturbance.
    activation = DataFrame(
        shock_type = String[], scale = Float64[],
        cooperative_social_loss = Float64[], nash_social_loss = Float64[],
        excess_social_loss = Float64[], policy_mix_distance = Float64[],
    )
    for scale in 0.0:0.1:1.5
        for shock_type in ["demand", "cost_push"]
            demand_path = zeros(HORIZON)
            supply_path = zeros(HORIZON)
            if shock_type == "demand"
                demand_path[1] = -scale * estimates["demand_shock_sd"]
            else
                supply_path[1] = scale * estimates["supply_shock_sd"]
            end
            scaled_results = solve_games(
                primitives, ShockPaths(demand_path, supply_path, zeros(HORIZON)),
            )
            cooperative = scaled_results["cooperation"]
            nash_result = scaled_results["nash"]
            distance = sqrt(
                sum(abs2, nash_result.monetary - cooperative.monetary) +
                sum(abs2, nash_result.fiscal - cooperative.fiscal)
            )
            push!(activation, (
                shock_type, scale, cooperative.social_loss,
                nash_result.social_loss,
                nash_result.social_loss - cooperative.social_loss,
                distance,
            ))
        end
    end
    CSV.write(joinpath(OUTPUT, "shock_activation.csv"), activation)

    # Sweep only the mandate-weight wedge; all macro and welfare primitives are
    # held fixed.  This is the clean experiment for strategic complementarity.
    sweep = DataFrame(
        divergence = Float64[], spectral_radius = Float64[],
        monetary_response_norm = Float64[], fiscal_response_norm = Float64[],
        demand_nash_welfare_gap = Float64[], supply_nash_welfare_gap = Float64[],
        demand_nash_monetary_peak = Float64[], demand_cooperative_monetary_peak = Float64[],
        supply_nash_fiscal_peak = Float64[], supply_cooperative_fiscal_peak = Float64[],
    )
    for divergence in 0.0:0.05:0.90
        monetary_d, fiscal_d = mandate_weights(social, divergence)
        model_d = ModelPrimitives(economy, social, monetary_d, fiscal_d)
        diagnostic = strategic_diagnostics(model_d, HORIZON)
        demand_results = solve_games(model_d, shocks["demand_contraction"])
        supply_results = solve_games(model_d, shocks["cost_push_inflation"])
        demand_gap = 100.0 * (
            demand_results["nash"].social_loss /
            demand_results["cooperation"].social_loss - 1.0
        )
        supply_gap = 100.0 * (
            supply_results["nash"].social_loss /
            supply_results["cooperation"].social_loss - 1.0
        )
        push!(sweep, (
            divergence, diagnostic.spectral_radius,
            diagnostic.monetary_norm, diagnostic.fiscal_norm,
            demand_gap, supply_gap,
            maximum(abs, demand_results["nash"].monetary),
            maximum(abs, demand_results["cooperation"].monetary),
            maximum(abs, supply_results["nash"].fiscal),
            maximum(abs, supply_results["cooperation"].fiscal),
        ))
    end
    CSV.write(joinpath(OUTPUT, "complementarity_sweep.csv"), sweep)

    # Two-dimensional sensitivity analysis.  Low instrument costs make the
    # opposing instruments close substitutes and can push the best-response
    # loop through the unit spectral-radius boundary.
    coordination_grid = DataFrame(
        divergence = Float64[], instrument_level_cost = Float64[],
        spectral_radius = Float64[],
    )
    for level_cost in 0.005:0.005:0.200, divergence in 0.0:0.05:0.90
        social_grid = LossWeights(
            inflation = social.inflation,
            output = social.output,
            debt = social.debt,
            monetary_level = level_cost,
            fiscal_level = level_cost,
            monetary_change = social.monetary_change,
            fiscal_change = social.fiscal_change,
            discount = social.discount,
        )
        monetary_grid, fiscal_grid = mandate_weights(social_grid, divergence)
        model_grid = ModelPrimitives(economy, social_grid, monetary_grid, fiscal_grid)
        diagnostic_grid = strategic_diagnostics(model_grid, HORIZON)
        push!(coordination_grid, (divergence, level_cost, diagnostic_grid.spectral_radius))
    end
    CSV.write(joinpath(OUTPUT, "coordination_grid.csv"), coordination_grid)

    # Explicit rules provide a distinct active/passive fiscal-dominance check.
    regimes = Dict(
        "monetary_dominance" => RuleCoefficients(
            name = "monetary_dominance",
            monetary_smoothing = 0.60,
            inflation_response = 1.50,
            output_response = 0.25,
            debt_accommodation = 0.00,
            fiscal_smoothing = 0.40,
            debt_response = 0.35,
            output_response_fiscal = 0.50,
        ),
        "fiscal_dominance" => RuleCoefficients(
            name = "fiscal_dominance",
            monetary_smoothing = 0.60,
            inflation_response = 0.80,
            output_response = 0.10,
            debt_accommodation = -0.10,
            fiscal_smoothing = 0.40,
            debt_response = 0.00,
            output_response_fiscal = 0.50,
        ),
    )
    rule_results = Dict{String, NamedTuple}()
    rule_summary = DataFrame(
        regime = String[], social_loss = Float64[], terminal_debt = Float64[],
        peak_inflation = Float64[], peak_monetary = Float64[],
        condition_number = Float64[],
    )
    for name in ["monetary_dominance", "fiscal_dominance"]
        result = solve_rule_system(economy, regimes[name], shocks["inflation_and_debt"])
        rule_results[name] = result
        welfare = social_loss_value(
            social, result.output, result.inflation, result.debt,
            result.monetary, result.fiscal,
        )
        push!(rule_summary, (
            name, welfare, result.debt[end], maximum(abs, result.inflation),
            maximum(abs, result.monetary), result.condition_number,
        ))
    end
    CSV.write(joinpath(OUTPUT, "rule_summary.csv"), rule_summary)

    diagnostic = strategic_diagnostics(primitives, HORIZON)
    baseline_demand = all_results["demand_contraction"]
    baseline_supply = all_results["cost_push_inflation"]
    report_values = Dict{String, Any}(
        "sample_start" => estimates["sample_start"],
        "sample_end" => estimates["sample_end"],
        "neutral_real_rate" => neutral_real_rate,
        "sigma" => economy.sigma,
        "fiscal_multiplier" => economy.fiscal_multiplier,
        "kappa" => economy.kappa,
        "beta" => economy.beta,
        "demand_shock_sd" => estimates["demand_shock_sd"],
        "supply_shock_sd" => estimates["supply_shock_sd"],
        "mandate_divergence" => BASELINE_DIVERGENCE,
        "spectral_radius" => diagnostic.spectral_radius,
        "monetary_response_norm" => diagnostic.monetary_norm,
        "fiscal_response_norm" => diagnostic.fiscal_norm,
        "demand_nash_welfare_gap_percent" => 100.0 * (
            baseline_demand["nash"].social_loss /
            baseline_demand["cooperation"].social_loss - 1.0
        ),
        "supply_nash_welfare_gap_percent" => 100.0 * (
            baseline_supply["nash"].social_loss /
            baseline_supply["cooperation"].social_loss - 1.0
        ),
        "demand_cooperative_peak_monetary" => maximum(abs, baseline_demand["cooperation"].monetary),
        "demand_nash_peak_monetary" => maximum(abs, baseline_demand["nash"].monetary),
        "supply_cooperative_peak_fiscal" => maximum(abs, baseline_supply["cooperation"].fiscal),
        "supply_nash_peak_fiscal" => maximum(abs, baseline_supply["nash"].fiscal),
        "feedback_nash_spectral_radius" => feedback_nash.spectral_radius,
        "feedback_nash_iterations" => feedback_nash.iterations,
        "institutional_nash_monetary_adjustment" => lambda_m_nash,
        "institutional_nash_fiscal_adjustment" => lambda_f_nash,
        "social_monetary_adjustment" => lambda_m_social,
        "social_fiscal_adjustment" => lambda_f_social,
        "recursive_demand_feedback_nash_loss" => recursive_losses["demand_contraction"]["feedback_nash"],
        "recursive_demand_cooperative_loss" => recursive_losses["demand_contraction"]["cooperation"],
        "recursive_supply_feedback_nash_loss" => recursive_losses["cost_push_inflation"]["feedback_nash"],
        "recursive_supply_cooperative_loss" => recursive_losses["cost_push_inflation"]["cooperation"],
        "recursive_supply_social_commitment_loss" => recursive_losses["cost_push_inflation"]["social_commitment"],
    )
    open(joinpath(OUTPUT, "paper_results.toml"), "w") do io
        TOML.print(io, report_values)
    end
    open(joinpath(OUTPUT, "calibration.toml"), "w") do io
        TOML.print(io, Dict(
            "macro" => Dict(string(field) => getfield(economy, field) for field in fieldnames(MacroCalibration)),
            "social" => Dict(string(field) => getfield(social, field) for field in fieldnames(LossWeights)),
            "monetary_mandate" => Dict(string(field) => getfield(monetary, field) for field in fieldnames(LossWeights)),
            "fiscal_mandate" => Dict(string(field) => getfield(fiscal, field) for field in fieldnames(LossWeights)),
            "recursive" => Dict(
                "output_persistence" => recursive_calibration.output_persistence,
                "inflation_persistence" => recursive_calibration.inflation_persistence,
                "investment_cost" => commitment.investment_cost,
                "institutional_nash_monetary_adjustment" => lambda_m_nash,
                "institutional_nash_fiscal_adjustment" => lambda_f_nash,
                "social_monetary_adjustment" => lambda_m_social,
                "social_fiscal_adjustment" => lambda_f_social,
            ),
        ))
    end

    plot_calibration_data(data)
    plot_game(
        all_results["demand_contraction"],
        "One-standard-deviation Australian demand contraction",
        "demand_shock_game.png",
    )
    plot_game(
        all_results["cost_push_inflation"],
        "One-standard-deviation Australian cost-push shock",
        "cost_push_game.png",
    )
    plot_complementarity_sweep(sweep)
    plot_coordination_boundary(coordination_grid)
    plot_shock_activation(activation)
    plot_rule_regimes(rule_results)
    plot_commitment_game(commitment)
    plot_recursive_irfs(
        recursive_paths["demand_contraction"],
        recursive_losses["demand_contraction"],
        "Recursive responses to an Australian demand contraction",
        "recursive_demand_game.png",
    )
    plot_recursive_irfs(
        recursive_paths["cost_push_inflation"],
        recursive_losses["cost_push_inflation"],
        "Recursive responses to an Australian cost-push shock",
        "recursive_cost_push_game.png",
    )

    @printf("Baseline best-response spectral radius: %.4f\n", diagnostic.spectral_radius)
    @printf("Demand-shock Nash welfare gap: %.2f%%\n", report_values["demand_nash_welfare_gap_percent"])
    @printf("Supply-shock Nash welfare gap: %.2f%%\n", report_values["supply_nash_welfare_gap_percent"])
    @printf(
        "Feedback Nash stability radius: %.4f (%d Riccati iterations)\n",
        feedback_nash.spectral_radius,
        feedback_nash.iterations,
    )
    @printf(
        "Institutional Nash adjustment costs: (%.3f, %.3f); social design: (%.3f, %.3f)\n",
        lambda_m_nash,
        lambda_f_nash,
        lambda_m_social,
        lambda_f_social,
    )
    println("Results written to $OUTPUT")
end


main()

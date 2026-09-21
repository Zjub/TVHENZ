#!/usr/bin/env julia

# Reproduce the complete Australian monetary-fiscal example.
#
# Run from this directory with:
#
#     julia --project=. run_model.jl
#
# Add `--refresh-data` to redownload the current RBA statistical tables.

ENV["GKSwstype"] = "100"  # headless GR output: write files without opening windows

using AustralianMonetaryFiscalGame
using AustralianMonetaryFiscalGame.AustralianData
using AustralianMonetaryFiscalGame.PolicyGame
using CSV
using DataFrames
using Dates
using Plots
using Printf
using TOML


const ROOT = @__DIR__
const RAW = joinpath(ROOT, "data", "raw")
const PROCESSED = joinpath(ROOT, "data", "processed")
const OUTPUT = joinpath(ROOT, "output")
const HORIZON = 20

const STATE_NAMES = [
    "output_gap",
    "inflation_gap",
    "debt_gap",
    "lagged_output_gap",
    "previous_monetary",
    "previous_fiscal",
]

const STRATEGY_ORDER = [
    "nash",
    "cooperative",
    "central_bank_leader",
    "fiscal_leader",
]

const STRATEGY_LABELS = Dict(
    "nash" => "Nash",
    "cooperative" => "Cooperative",
    "central_bank_leader" => "CB leader",
    "fiscal_leader" => "Fiscal leader",
)

const STRATEGY_COLOURS = Dict(
    "nash" => :firebrick,
    "cooperative" => :seagreen,
    "central_bank_leader" => :darkorange,
    "fiscal_leader" => :midnightblue,
)


"""Create initial-state disturbances, all measured in Australian data units."""
function make_scenarios(estimates::Dict{String, Any})
    demand = zeros(6)
    demand[1] = -estimates["demand_shock_sd"]

    supply = zeros(6)
    supply[2] = estimates["supply_shock_sd"]

    fiscal_stress = zeros(6)
    fiscal_stress[2] = estimates["supply_shock_sd"]
    fiscal_stress[3] = 1.0  # debt-to-GDP ratio begins 1 percentage point above target

    return Dict(
        "demand_contraction" => demand,
        "supply_inflation" => supply,
        "fiscal_stress" => fiscal_stress,
    )
end


"""Write all estimated and calibrated primitives to an auditable TOML file."""
function write_primitives(
    path::AbstractString,
    primitives::ModelPrimitives,
    neutral_real_rate::Float64,
)
    p = primitives.economy
    m = primitives.monetary_preferences
    f = primitives.fiscal_preferences
    document = Dict(
        "macro" => Dict(
            "rho_x" => p.rho_x,
            "sigma_i" => p.sigma_i,
            "chi_g" => p.chi_g,
            "rho_pi" => p.rho_pi,
            "kappa" => p.kappa,
            "rho_b" => p.rho_b,
            "psi_g" => p.psi_g,
            "psi_i" => p.psi_i,
            "psi_pi" => p.psi_pi,
            "neutral_real_rate" => neutral_real_rate,
        ),
        "monetary_preferences" => Dict(
            "inflation" => m.inflation,
            "output" => m.output,
            "debt" => m.debt,
            "adjustment" => m.adjustment,
            "level" => m.level,
            "discount" => m.discount,
        ),
        "fiscal_preferences" => Dict(
            "inflation" => f.inflation,
            "output" => f.output,
            "debt" => f.debt,
            "adjustment" => f.adjustment,
            "level" => f.level,
            "discount" => f.discount,
        ),
        "important_note" => (
            "Preference weights are fixed across all games and active/passive " *
            "policy-rule regimes."
        ),
    )
    open(path, "w") do io
        TOML.print(io, document)
    end
end


"""Save a compact four-panel chart of the Australian model inputs."""
function plot_australian_inputs(data::DataFrame, neutral_real_rate::Float64)
    selected = data[data.date .>= Date(1993, 3, 31), :]
    common = (
        linewidth = 1.7,
        colour = :steelblue4,
        legend = false,
        gridalpha = 0.2,
        framestyle = :box,
    )

    p1 = plot(
        selected.date,
        selected.output_gap;
        title = "Output gap",
        ylabel = "% of trend",
        common...,
    )
    p2 = plot(
        selected.date,
        selected.inflation_gap;
        title = "Trimmed-mean inflation gap",
        ylabel = "ppt from 2.5%",
        common...,
    )
    p3 = plot(
        selected.date,
        selected.real_rate_gap;
        title = "Ex-post real cash-rate gap",
        ylabel = "ppt",
        common...,
    )
    p4 = plot(
        selected.date,
        selected.public_demand_gap;
        title = "Public-demand gap",
        ylabel = "% of trend",
        common...,
    )
    for panel in (p1, p2, p3, p4)
        hline!(panel, [0.0]; colour = :black, linewidth = 0.7)
    end
    figure = plot(
        p1,
        p2,
        p3,
        p4;
        layout = (2, 2),
        size = (1200, 760),
        plot_title = @sprintf(
            "Australian inputs (neutral real rate centred at %.2f%%)",
            neutral_real_rate,
        ),
    )
    savefig(figure, joinpath(OUTPUT, "australian_inputs.png"))
end


function add_strategy_paths!(
    panel,
    results::Dict{String, GameResult},
    extractor::Function,
)
    for strategy in STRATEGY_ORDER
        plot!(
            panel,
            0:(HORIZON - 1),
            extractor(results[strategy]);
            label = STRATEGY_LABELS[strategy],
            colour = STRATEGY_COLOURS[strategy],
            linewidth = 1.8,
        )
    end
    hline!(panel, [0.0]; colour = :black, linewidth = 0.7, label = "")
    return panel
end


"""Plot all four strategic solution concepts for one shock."""
function plot_game(results::Dict{String, GameResult}, scenario::String, filename::String)
    panels = [
        plot(title = "Output gap", ylabel = "ppt", legend = false),
        plot(title = "Inflation gap", ylabel = "annualised ppt", legend = false),
        plot(title = "Real cash-rate gap", ylabel = "ppt", legend = false),
        plot(title = "Public-demand gap", ylabel = "% of trend", legend = false),
        plot(title = "Debt gap", ylabel = "ppt of GDP", legend = false),
        plot(title = "Solution concepts", legend = :left, grid = false, axis = false),
    ]
    extractors = [
        result -> result.states[1:HORIZON, 1],
        result -> result.states[1:HORIZON, 2],
        result -> result.monetary,
        result -> result.fiscal,
        result -> result.states[1:HORIZON, 3],
    ]
    for index in 1:5
        plot!(panels[index]; gridalpha = 0.2, framestyle = :box)
        add_strategy_paths!(panels[index], results, extractors[index])
    end

    # Put a clean common legend in the unused sixth panel.
    for strategy in STRATEGY_ORDER
        plot!(
            panels[6],
            [NaN],
            [NaN];
            label = STRATEGY_LABELS[strategy],
            colour = STRATEGY_COLOURS[strategy],
            linewidth = 2,
        )
    end
    plot!(panels[5]; xlabel = "quarters after shock")
    figure = plot(
        panels...;
        layout = (3, 2),
        size = (1200, 1050),
        plot_title = "Fixed preferences: $(replace(scenario, '_' => ' '))",
    )
    savefig(figure, joinpath(OUTPUT, filename))
end


"""Compare active-monetary/passive-fiscal and the reverse policy-rule mix."""
function plot_rule_regimes(rule_results::Dict{String, GameResult})
    regime_order = ["monetary_dominance", "fiscal_dominance"]
    labels = Dict(
        "monetary_dominance" => "Monetary dominance (AM/PF)",
        "fiscal_dominance" => "Fiscal dominance (PM/AF)",
    )
    colours = Dict("monetary_dominance" => :seagreen, "fiscal_dominance" => :firebrick)
    panels = [
        plot(title = "Output gap", legend = false),
        plot(title = "Inflation gap", legend = false),
        plot(title = "Real cash-rate gap", legend = false),
        plot(title = "Public-demand gap", legend = false),
        plot(title = "Debt gap", legend = false),
        plot(title = "Policy-rule regimes", legend = :left, grid = false, axis = false),
    ]
    extractors = [
        result -> result.states[1:HORIZON, 1],
        result -> result.states[1:HORIZON, 2],
        result -> result.monetary,
        result -> result.fiscal,
        result -> result.states[1:HORIZON, 3],
    ]

    for panel_index in 1:5
        for regime in regime_order
            plot!(
                panels[panel_index],
                0:(HORIZON - 1),
                extractors[panel_index](rule_results[regime]);
                colour = colours[regime],
                linewidth = 1.8,
                label = labels[regime],
            )
        end
        hline!(panels[panel_index], [0.0]; colour = :black, linewidth = 0.7, label = "")
        plot!(panels[panel_index]; gridalpha = 0.2, framestyle = :box)
    end
    for regime in regime_order
        plot!(
            panels[6],
            [NaN],
            [NaN];
            label = labels[regime],
            colour = colours[regime],
            linewidth = 2,
        )
    end
    plot!(panels[5]; xlabel = "quarters after shock")
    figure = plot(
        panels...;
        layout = (3, 2),
        size = (1200, 1050),
        plot_title = "Fixed preferences: inflation plus 1 ppt debt stress",
    )
    savefig(figure, joinpath(OUTPUT, "fiscal_dominance_comparison.png"))
end


function main()
    mkpath(RAW)
    mkpath(PROCESSED)
    mkpath(OUTPUT)
    refresh = "--refresh-data" in ARGS
    obtain_raw_data(RAW; refresh = refresh)

    data, neutral_real_rate = prepare_quarterly_data(RAW)
    estimates, estimate_table = estimate_macro_block(data)
    CSV.write(joinpath(PROCESSED, "australia_quarterly.csv"), data)
    CSV.write(joinpath(OUTPUT, "estimated_coefficients.csv"), estimate_table)
    open(joinpath(OUTPUT, "estimation_summary.toml"), "w") do io
        TOML.print(io, estimates)
    end

    economy = MacroParameters(
        rho_x = estimates["rho_x"],
        sigma_i = estimates["sigma_i"],
        chi_g = estimates["chi_g"],
        rho_pi = estimates["rho_pi"],
        kappa = estimates["kappa"],
    )

    # These objective weights are model primitives.  They are defined once and
    # passed unchanged to every game and every active/passive policy-rule run.
    monetary_preferences = Preferences(
        inflation = 1.00,
        output = 0.25,
        debt = 0.00,
        adjustment = 0.12,
    )
    fiscal_preferences = Preferences(
        inflation = 0.25,
        output = 1.00,
        debt = 0.45,
        adjustment = 0.08,
    )
    primitives = ModelPrimitives(economy, monetary_preferences, fiscal_preferences)
    write_primitives(
        joinpath(OUTPUT, "model_primitives.toml"),
        primitives,
        neutral_real_rate,
    )

    scenarios = make_scenarios(estimates)
    no_future_shocks = zeros(HORIZON, 6)
    all_games = Dict{String, Dict{String, GameResult}}()
    game_summary = DataFrame(
        scenario = String[],
        strategy = String[],
        monetary_loss = Float64[],
        fiscal_loss = Float64[],
        joint_loss = Float64[],
        peak_abs_output_gap = Float64[],
        peak_abs_inflation_gap = Float64[],
        peak_abs_debt_gap = Float64[],
        peak_abs_monetary = Float64[],
        peak_abs_fiscal = Float64[],
    )

    for scenario_name in sort(collect(keys(scenarios)))
        results = solve_games(
            primitives,
            scenarios[scenario_name],
            no_future_shocks,
        )
        all_games[scenario_name] = results
        for strategy in STRATEGY_ORDER
            result = results[strategy]
            push!(game_summary, (
                scenario_name,
                strategy,
                result.monetary_loss,
                result.fiscal_loss,
                result.monetary_loss + result.fiscal_loss,
                maximum(abs, result.states[:, 1]),
                maximum(abs, result.states[:, 2]),
                maximum(abs, result.states[:, 3]),
                maximum(abs, result.monetary),
                maximum(abs, result.fiscal),
            ))
        end
    end
    CSV.write(joinpath(OUTPUT, "game_summary.csv"), game_summary)

    sensitivities = first_action_sensitivities(primitives, HORIZON)
    sensitivity_table = DataFrame(
        action = repeat(["monetary_u", "fiscal_g"], inner = length(STATE_NAMES)),
        state = repeat(STATE_NAMES, outer = 2),
        derivative = vec(permutedims(sensitivities)),
    )
    CSV.write(joinpath(OUTPUT, "policy_sensitivities.csv"), sensitivity_table)

    # Leeper-style regimes change rule coefficients, never preferences.
    rule_regimes = Dict(
        "monetary_dominance" => RuleRegime(
            name = "monetary_dominance",
            nominal_inflation_response = 1.50, # active money: Taylor principle
            monetary_output_response = 0.25,
            monetary_smoothing = 0.60,
            monetary_debt_accommodation = 0.00,
            fiscal_debt_response = 0.35,       # passive fiscal: stabilises debt
            fiscal_output_response = 0.80,
            fiscal_smoothing = 0.40,
        ),
        "fiscal_dominance" => RuleRegime(
            name = "fiscal_dominance",
            nominal_inflation_response = 0.80, # passive money
            monetary_output_response = 0.10,
            monetary_smoothing = 0.60,
            monetary_debt_accommodation = 0.10,
            fiscal_debt_response = 0.00,       # active fiscal: no debt feedback
            fiscal_output_response = 0.80,
            fiscal_smoothing = 0.40,
        ),
    )
    rule_results = Dict{String, GameResult}()
    rule_summary = DataFrame(
        regime = String[],
        nominal_inflation_response = Float64[],
        fiscal_debt_response = Float64[],
        monetary_loss = Float64[],
        fiscal_loss = Float64[],
        joint_loss = Float64[],
        peak_abs_monetary = Float64[],
        peak_abs_fiscal = Float64[],
        terminal_debt_gap = Float64[],
    )
    for regime_name in ["monetary_dominance", "fiscal_dominance"]
        regime = rule_regimes[regime_name]
        result = simulate_policy_rules(
            primitives,
            regime,
            scenarios["fiscal_stress"],
            no_future_shocks,
        )
        rule_results[regime_name] = result
        push!(rule_summary, (
            regime_name,
            regime.nominal_inflation_response,
            regime.fiscal_debt_response,
            result.monetary_loss,
            result.fiscal_loss,
            result.monetary_loss + result.fiscal_loss,
            maximum(abs, result.monetary),
            maximum(abs, result.fiscal),
            result.states[end, 3],
        ))
    end
    CSV.write(joinpath(OUTPUT, "rule_regime_summary.csv"), rule_summary)

    plot_australian_inputs(data, neutral_real_rate)
    plot_game(
        all_games["demand_contraction"],
        "demand_contraction",
        "demand_shock_game.png",
    )
    plot_game(
        all_games["supply_inflation"],
        "supply_inflation",
        "supply_shock_game.png",
    )
    plot_rule_regimes(rule_results)

    println("Julia model completed. Outputs written to: $OUTPUT")
end


main()

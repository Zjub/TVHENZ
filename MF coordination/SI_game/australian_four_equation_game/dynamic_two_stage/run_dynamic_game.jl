using CSV
using DataFrames
using LinearAlgebra
using Plots
using Printf
using TOML

include(joinpath(@__DIR__, "dynamic_game.jl"))
using .DynamicStrategicInvestment

const ROOT = @__DIR__
const OUTPUT = joinpath(ROOT, "output")
const HORIZON = 40
const DEMAND_SD = 0.46387449164984773
const SUPPLY_SD = 0.6699569103763775

default(fontfamily="Computer Modern", linewidth=2, framestyle=:box, gridalpha=0.22)

function baseline_model()
    calibration = Calibration()
    social = LossWeights(
        inflation=1.0, output=0.5, debt=0.15,
        monetary_level=0.10, fiscal_level=0.10,
        monetary_change=0.05, fiscal_change=0.04,
    )
    monetary = LossWeights(
        inflation=1.5, output=0.25, debt=0.15,
        monetary_level=0.10, fiscal_level=0.10,
        monetary_change=0.05, fiscal_change=0.04,
    )
    fiscal = LossWeights(
        inflation=0.5, output=0.75, debt=0.15,
        monetary_level=0.10, fiscal_level=0.10,
        monetary_change=0.05, fiscal_change=0.04,
    )
    return Model(calibration=calibration, monetary=monetary,
                 fiscal=fiscal, social=social)
end

"""Scale only the primitive adjustment costs, leaving all mandate weights fixed."""
function adjustment_model(scale)
    base = baseline_model()
    copy_scaled(w) = LossWeights(
        inflation=w.inflation, output=w.output, debt=w.debt,
        monetary_level=w.monetary_level, fiscal_level=w.fiscal_level,
        monetary_change=scale * w.monetary_change,
        fiscal_change=scale * w.fiscal_change, discount=w.discount,
    )
    return Model(calibration=base.calibration, monetary=copy_scaled(base.monetary),
                 fiscal=copy_scaled(base.fiscal), social=copy_scaled(base.social))
end

initial_states() = Dict(
    "Demand contraction" => [0.0, 0.0, 0.0, 0.0, 0.0, -DEMAND_SD, 0.0],
    "Cost-push inflation" => [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, SUPPLY_SD],
)

function stationary_as_finite(stationary, horizon)
    return (
        monetary_rule=[copy(stationary.monetary_rule) for _ in 1:horizon],
        fiscal_rule=[copy(stationary.fiscal_rule) for _ in 1:horizon],
        horizon=horizon,
    )
end

function save_irf_plot(paths, scenario, filename)
    variables = [
        (:output, "Output gap"), (:inflation, "Inflation gap"),
        (:debt, "Debt gap"), (:monetary, "Monetary instrument"),
        (:fiscal, "Fiscal instrument"),
    ]
    colors = Dict("Feedback Nash"=>:navy, "Open-loop Nash"=>:firebrick,
                  "Cooperation"=>:darkgreen, "Stationary MPE"=>:darkorange)
    styles = Dict("Feedback Nash"=>:solid, "Open-loop Nash"=>:dash,
                  "Cooperation"=>:dot, "Stationary MPE"=>:dashdot)
    panels = Any[]
    shown = 16
    for (field, label) in variables
        p = plot(title=label, xlabel="quarter", ylabel="gap",
                 legend=field == :output ? :topright : false)
        for regime in ["Feedback Nash", "Open-loop Nash", "Cooperation", "Stationary MPE"]
            plot!(p, 1:shown, getfield(paths[regime], field)[1:shown],
                  label=regime, color=colors[regime], linestyle=styles[regime])
        end
        push!(panels, p)
    end
    push!(panels, plot(framestyle=:none, axis=false, ticks=false,
                       annotations=(0.5, 0.5, text("Fixed primitive adjustment costs\npolicy positions are the investments", 11))))
    fig = plot(panels..., layout=(3, 2), size=(1050, 900),
               plot_title=scenario, left_margin=3Plots.mm, bottom_margin=2Plots.mm)
    savefig(fig, joinpath(OUTPUT, filename))
end

function save_two_stage_plot(model, states)
    panels = Any[]
    for (scenario, state) in states
        fb = simulate_feedback(solve_feedback(model, 2), model, state)
        ol = simulate_open_loop(solve_open_loop(model, state, 2), model, state)
        p1 = plot(1:2, fb.monetary, marker=:circle, label="feedback",
                  color=:navy, title="$scenario: monetary", xlabel="stage", ylabel="instrument")
        plot!(p1, 1:2, ol.monetary, marker=:diamond, label="open loop",
              color=:firebrick, linestyle=:dash)
        p2 = plot(1:2, fb.fiscal, marker=:circle, label="feedback",
                  color=:navy, title="$scenario: fiscal", xlabel="stage", ylabel="instrument")
        plot!(p2, 1:2, ol.fiscal, marker=:diamond, label="open loop",
              color=:firebrick, linestyle=:dash)
        push!(panels, p1, p2)
    end
    savefig(plot(panels..., layout=(2, 2), size=(1000, 720),
                 plot_title="Two-stage strategic-investment experiment"),
            joinpath(OUTPUT, "two_stage_policies.png"))
end

function save_horizon_plot(table)
    p1 = plot(table.horizon, table.rule_distance, color=:navy, marker=:circle,
              yscale=:log10, xlabel="horizon", ylabel="distance to stationary rule",
              title="Feedback-rule convergence", label=false)
    demand = table[table.scenario .== "Demand contraction", :]
    supply = table[table.scenario .== "Cost-push inflation", :]
    p2 = plot(demand.horizon, demand.feedback_openloop_policy_gap, color=:navy,
              marker=:circle, label="demand contraction", xlabel="horizon",
              ylabel="norm of initial policy difference", title="Feedback versus open loop")
    plot!(p2, supply.horizon, supply.feedback_openloop_policy_gap, color=:firebrick,
          marker=:diamond, label="cost-push inflation")
    savefig(plot(p1, p2, layout=(1, 2), size=(1050, 450),
                 left_margin=7Plots.mm, bottom_margin=5Plots.mm),
            joinpath(OUTPUT, "horizon_convergence.png"))
end

function save_wedge_plot(table)
    labels = ["M: demand", "F: demand", "M: cost push", "F: cost push"]
    values = [
        table[table.scenario .== "Demand contraction", :monetary_strategic_wedge][1],
        table[table.scenario .== "Demand contraction", :fiscal_strategic_wedge][1],
        table[table.scenario .== "Cost-push inflation", :monetary_strategic_wedge][1],
        table[table.scenario .== "Cost-push inflation", :fiscal_strategic_wedge][1],
    ]
    p = bar(labels, values, color=[:navy, :darkgreen, :navy, :darkgreen],
            label=false, ylabel="strategic component of stage-1 half-gradient",
            title="Rival-response component of strategic investment", xrotation=15,
            left_margin=7Plots.mm, bottom_margin=5Plots.mm)
    hline!(p, [0.0], color=:black, linewidth=1, label=false)
    savefig(p, joinpath(OUTPUT, "strategic_wedges.png"))
end

function save_adjustment_sensitivity(table)
    p1 = plot(table.adjustment_scale, table.fiscal_response_to_monetary_investment,
              marker=:circle, color=:navy, label="d f2 / d m1",
              xlabel="multiple of baseline adjustment costs", ylabel="cross-stage response",
              title="Inherited-policy response")
    plot!(p1, table.adjustment_scale, table.monetary_response_to_fiscal_investment,
          marker=:diamond, color=:darkgreen, label="d m2 / d f1")
    p2 = plot(table.adjustment_scale, table.feedback_openloop_policy_gap,
              marker=:circle, color=:firebrick, label=false,
              xlabel="multiple of baseline adjustment costs",
              ylabel="norm of stage-1 policy difference",
              title="Strategic feedback--open-loop gap")
    vline!(p1, [1.0], color=:grey40, linestyle=:dash, label="baseline")
    vline!(p2, [1.0], color=:grey40, linestyle=:dash, label=false)
    savefig(plot(p1, p2, layout=(1, 2), size=(1050, 450),
                 left_margin=7Plots.mm, bottom_margin=5Plots.mm),
            joinpath(OUTPUT, "adjustment_cost_sensitivity.png"))
end

function write_tex_macros(summary, diagnostics, stationary)
    demand = summary[(summary.scenario .== "Demand contraction") .&
                     (summary.horizon .== HORIZON), :]
    supply = summary[(summary.scenario .== "Cost-push inflation") .&
                     (summary.horizon .== HORIZON), :]
    value(df, regime, field) = df[df.regime .== regime, field][1]
    diag_d = diagnostics[diagnostics.scenario .== "Demand contraction", :][1, :]
    diag_s = diagnostics[diagnostics.scenario .== "Cost-push inflation", :][1, :]
    file = joinpath(OUTPUT, "results.tex")
    open(file, "w") do io
        @printf(io, "\\newcommand{\\DemandSD}{%.3f}\n", DEMAND_SD)
        @printf(io, "\\newcommand{\\SupplySD}{%.3f}\n", SUPPLY_SD)
        @printf(io, "\\newcommand{\\FiscalResponseMonetary}{%.4f}\n", diag_d.fiscal_response_to_monetary_investment)
        @printf(io, "\\newcommand{\\MonetaryResponseFiscal}{%.4f}\n", diag_d.monetary_response_to_fiscal_investment)
        @printf(io, "\\newcommand{\\DemandMonetaryWedge}{%.5f}\n", diag_d.monetary_strategic_wedge)
        @printf(io, "\\newcommand{\\DemandFiscalWedge}{%.5f}\n", diag_d.fiscal_strategic_wedge)
        @printf(io, "\\newcommand{\\SupplyMonetaryWedge}{%.5f}\n", diag_s.monetary_strategic_wedge)
        @printf(io, "\\newcommand{\\SupplyFiscalWedge}{%.5f}\n", diag_s.fiscal_strategic_wedge)
        @printf(io, "\\newcommand{\\DemandFeedbackLoss}{%.4f}\n", value(demand, "Feedback Nash", :social_loss))
        @printf(io, "\\newcommand{\\DemandOpenLoss}{%.4f}\n", value(demand, "Open-loop Nash", :social_loss))
        @printf(io, "\\newcommand{\\DemandCoopLoss}{%.4f}\n", value(demand, "Cooperation", :social_loss))
        @printf(io, "\\newcommand{\\SupplyFeedbackLoss}{%.4f}\n", value(supply, "Feedback Nash", :social_loss))
        @printf(io, "\\newcommand{\\SupplyOpenLoss}{%.4f}\n", value(supply, "Open-loop Nash", :social_loss))
        @printf(io, "\\newcommand{\\SupplyCoopLoss}{%.4f}\n", value(supply, "Cooperation", :social_loss))
        @printf(io, "\\newcommand{\\StationaryRadius}{%.4f}\n", stationary.spectral_radius)
        @printf(io, "\\newcommand{\\DistinctStationary}{%d}\n", 1)
    end
end

function main()
    mkpath(OUTPUT)
    model = baseline_model()
    states = initial_states()

    # Search from dispersed value-function initialisations.  This is a
    # numerical diagnostic, not a proof of global uniqueness.
    search = solve_stationary_multistart(model)
    isempty(search.solutions) && error("no stable stationary feedback equilibrium found")
    stationary = first(search.solutions)
    stationary_rows = DataFrame(seed=Float64[], converged=Bool[], iterations=Int[],
                                spectral_radius=Float64[], residual=Float64[],
                                distance_to_selected=Float64[])
    for attempt in search.attempts
        distance = maximum(abs, vcat(attempt.monetary_rule - stationary.monetary_rule,
                                    attempt.fiscal_rule - stationary.fiscal_rule))
        push!(stationary_rows, (attempt.seed_scale, attempt.converged, attempt.iterations,
                               real(attempt.spectral_radius), attempt.residual, distance))
    end
    CSV.write(joinpath(OUTPUT, "stationary_equilibrium_search.csv"), stationary_rows)

    feedback = solve_feedback(model, HORIZON)
    cooperation = solve_cooperation(model, HORIZON)
    stationary_finite = stationary_as_finite(stationary, HORIZON)
    summary = DataFrame(scenario=String[], horizon=Int[], regime=String[],
                        monetary_loss=Float64[], fiscal_loss=Float64[], social_loss=Float64[],
                        initial_monetary=Float64[], initial_fiscal=Float64[],
                        peak_output=Float64[], peak_inflation=Float64[], peak_debt=Float64[])
    plotted_paths = Dict{String, Dict{String, Any}}()
    for (scenario, state) in states
        open_loop = solve_open_loop(model, state, HORIZON)
        paths = Dict(
            "Feedback Nash" => simulate_feedback(feedback, model, state),
            "Open-loop Nash" => simulate_open_loop(open_loop, model, state),
            "Cooperation" => simulate_feedback(cooperation, model, state),
            "Stationary MPE" => simulate_feedback(stationary_finite, model, state),
        )
        plotted_paths[scenario] = paths
        for regime in ["Feedback Nash", "Open-loop Nash", "Cooperation", "Stationary MPE"]
            path = paths[regime]
            loss = evaluate_path(path, model)
            push!(summary, (scenario, HORIZON, regime, loss.monetary, loss.fiscal, loss.social,
                            path.monetary[1], path.fiscal[1], maximum(abs, path.output),
                            maximum(abs, path.inflation), maximum(abs, path.debt)))
        end
    end

    # Exact T=2 experiment and its rival-response decomposition.
    diagnostics = DataFrame(scenario=String[],
        fiscal_response_to_monetary_investment=Float64[],
        monetary_response_to_fiscal_investment=Float64[],
        monetary_strategic_wedge=Float64[], fiscal_strategic_wedge=Float64[],
        monetary_continuation=Float64[], fiscal_continuation=Float64[])
    for (scenario, state) in states
        diag = two_stage_diagnostics(model, state)
        push!(diagnostics, (scenario,
            diag.fiscal_response_to_monetary_investment,
            diag.monetary_response_to_fiscal_investment,
            diag.monetary_strategic_wedge, diag.fiscal_strategic_wedge,
            diag.monetary_continuation, diag.fiscal_continuation))
        for (regime, path) in [
            ("Feedback Nash", diag.path),
            ("Open-loop Nash", simulate_open_loop(solve_open_loop(model, state, 2), model, state)),
            ("Cooperation", simulate_feedback(solve_cooperation(model, 2), model, state)),
        ]
            loss = evaluate_path(path, model)
            push!(summary, (scenario, 2, regime, loss.monetary, loss.fiscal, loss.social,
                            path.monetary[1], path.fiscal[1], maximum(abs, path.output),
                            maximum(abs, path.inflation), maximum(abs, path.debt)))
        end
    end
    CSV.write(joinpath(OUTPUT, "equilibrium_summary.csv"), summary)
    CSV.write(joinpath(OUTPUT, "two_stage_diagnostics.csv"), diagnostics)

    # The Australian adjustment costs are modest.  This sweep shows whether
    # the investment mechanism strengthens when inherited policy is more
    # durable, without ever treating the coefficients as strategic choices.
    sensitivity = DataFrame(adjustment_scale=Float64[],
        fiscal_response_to_monetary_investment=Float64[],
        monetary_response_to_fiscal_investment=Float64[],
        monetary_strategic_wedge=Float64[], fiscal_strategic_wedge=Float64[],
        feedback_openloop_policy_gap=Float64[])
    demand_state = states["Demand contraction"]
    for scale in [0.0, 0.25, 0.5, 1.0, 2.0, 4.0, 8.0, 12.0]
        scaled = adjustment_model(scale)
        diag = two_stage_diagnostics(scaled, demand_state)
        fb_path = diag.path
        ol_path = simulate_open_loop(solve_open_loop(scaled, demand_state, 2), scaled, demand_state)
        gap = norm([fb_path.monetary[1] - ol_path.monetary[1],
                    fb_path.fiscal[1] - ol_path.fiscal[1]])
        push!(sensitivity, (scale, diag.fiscal_response_to_monetary_investment,
                            diag.monetary_response_to_fiscal_investment,
                            diag.monetary_strategic_wedge, diag.fiscal_strategic_wedge, gap))
    end
    CSV.write(joinpath(OUTPUT, "adjustment_cost_sensitivity.csv"), sensitivity)

    # Convergence of finite backward induction to the stationary feedback rule.
    horizon_rows = DataFrame(horizon=Int[], scenario=String[], rule_distance=Float64[],
                             feedback_openloop_policy_gap=Float64[], feedback_social_loss=Float64[],
                             openloop_social_loss=Float64[])
    for horizon in [2, 3, 4, 6, 8, 12, 20, 30, 40, 60, 80]
        fb = solve_feedback(model, horizon)
        rule_distance = norm(vcat(fb.monetary_rule[1] - stationary.monetary_rule,
                                  fb.fiscal_rule[1] - stationary.fiscal_rule))
        for (scenario, state) in states
            fb_path = simulate_feedback(fb, model, state)
            ol_path = simulate_open_loop(solve_open_loop(model, state, horizon), model, state)
            gap = norm([fb_path.monetary[1] - ol_path.monetary[1],
                        fb_path.fiscal[1] - ol_path.fiscal[1]])
            push!(horizon_rows, (horizon, scenario, rule_distance, gap,
                                 evaluate_path(fb_path, model).social,
                                 evaluate_path(ol_path, model).social))
        end
    end
    CSV.write(joinpath(OUTPUT, "horizon_convergence.csv"), horizon_rows)

    # Stationary feedback coefficients make the dynamic strategic channel auditable.
    state_names = ["lagged_output", "lagged_inflation", "debt", "lagged_monetary",
                   "lagged_fiscal", "demand_shock", "cost_push_shock"]
    rule_table = DataFrame(authority=String[], state=String[], coefficient=Float64[])
    for (authority, rule) in [("Monetary", stationary.monetary_rule),
                              ("Fiscal", stationary.fiscal_rule)]
        for (state, coefficient) in zip(state_names, rule)
            push!(rule_table, (authority, state, coefficient))
        end
    end
    CSV.write(joinpath(OUTPUT, "stationary_policy_rules.csv"), rule_table)

    calibration = Dict(
        "estimated" => Dict("rho_x"=>model.calibration.rho_x,
                            "rho_pi"=>model.calibration.rho_pi,
                            "sigma"=>model.calibration.sigma,
                            "fiscal_multiplier"=>model.calibration.fiscal_multiplier,
                            "kappa"=>model.calibration.kappa,
                            "demand_shock_sd"=>DEMAND_SD,
                            "supply_shock_sd"=>SUPPLY_SD),
        "calibrated" => Dict("rho_b"=>model.calibration.rho_b,
                             "debt_from_interest"=>model.calibration.debt_from_interest,
                             "debt_from_fiscal"=>model.calibration.debt_from_fiscal,
                             "debt_from_inflation"=>model.calibration.debt_from_inflation),
    )
    open(joinpath(OUTPUT, "calibration.toml"), "w") do io
        TOML.print(io, calibration)
    end

    save_irf_plot(plotted_paths["Demand contraction"], "One-s.d. Australian demand contraction",
                  "dynamic_demand_irfs.png")
    save_irf_plot(plotted_paths["Cost-push inflation"], "One-s.d. Australian cost-push shock",
                  "dynamic_cost_push_irfs.png")
    save_two_stage_plot(model, states)
    save_horizon_plot(horizon_rows)
    save_wedge_plot(diagnostics)
    save_adjustment_sensitivity(sensitivity)
    write_tex_macros(summary, diagnostics, stationary)

    @printf("Stable stationary solutions found: %d\n", length(search.solutions))
    @printf("Selected stationary spectral radius: %.6f\n", real(stationary.spectral_radius))
    for row in eachrow(diagnostics)
        @printf("%s: strategic wedges M=% .6f, F=% .6f\n",
                row.scenario, row.monetary_strategic_wedge, row.fiscal_strategic_wedge)
    end
    println("Wrote results to $OUTPUT")
end

main()

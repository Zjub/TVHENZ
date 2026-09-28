using CSV
using DataFrames
using LinearAlgebra
using Plots
using Printf

include(joinpath(@__DIR__, "endogenous_leadership.jl"))
using .EndogenousLeadership
include(joinpath(@__DIR__, "nk_forward_looking_game.jl"))
using .ForwardLookingNK

const ROOT = @__DIR__
const OUTPUT = joinpath(ROOT, "output")
const HORIZON = 40
const DEMAND_SD = 0.46387449164984773
const SUPPLY_SD = 0.6699569103763775
const LAMBDA_M = 0.05
const LAMBDA_F = 0.04

default(fontfamily="Computer Modern", linewidth=2, framestyle=:box,
        gridalpha=0.22, legendfontsize=8)

regimes() = Dict(
    "Flexible Nash" => Regime(name="Flexible Nash"),
    "Monetary commitment" => Regime(name="Monetary commitment",
                                      monetary_adjustment=LAMBDA_M),
    "Fiscal commitment" => Regime(name="Fiscal commitment",
                                    fiscal_adjustment=LAMBDA_F),
    "Bilateral commitment" => Regime(name="Bilateral commitment",
                                       monetary_adjustment=LAMBDA_M,
                                       fiscal_adjustment=LAMBDA_F),
    "Coordination" => Regime(name="Coordination"),
)

states() = Dict(
    "Demand contraction" => [0.0, 0.0, 0.0, 0.0, 0.0, -DEMAND_SD, 0.0],
    "Cost-push inflation" => [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, SUPPLY_SD],
)

function solve_homotopy(model, theta_grid, regime_map)
    solutions = Dict{Tuple{Float64, String}, Any}()
    for name in ["Flexible Nash", "Monetary commitment", "Fiscal commitment",
                 "Bilateral commitment", "Coordination"]
        expectations = zeros(2, 7)
        for theta in theta_grid
            solution = if name == "Coordination"
                solve_nk_cooperation(model, regime_map[name], theta;
                    initial_expectations=expectations)
            else
                solve_nk_nash(model, regime_map[name], theta;
                    initial_expectations=expectations)
            end
            solution.metadata.outer_converged || error("NK fixed point failed: $name, theta=$theta")
            solution.spectral_radius < 1.0 || error("unstable NK solution: $name, theta=$theta")
            solutions[(theta, name)] = solution
            expectations = solution.metadata.expectations
        end
    end
    return solutions
end

bounded_component_share(component, total) =
    abs(component) / (abs(component) + abs(total - component) + eps(Float64))

function save_theta_plot(summary)
    demand = summary[summary.scenario .== "Demand contraction", :]
    names = ["Flexible Nash", "Monetary commitment", "Fiscal commitment",
             "Bilateral commitment", "Coordination"]
    colors = Dict("Flexible Nash"=>:black, "Monetary commitment"=>:navy,
                  "Fiscal commitment"=>:darkgreen, "Bilateral commitment"=>:firebrick,
                  "Coordination"=>:darkorange)
    styles = Dict("Flexible Nash"=>:dash, "Monetary commitment"=>:solid,
                  "Fiscal commitment"=>:solid, "Bilateral commitment"=>:dashdot,
                  "Coordination"=>:dot)
    fields = [(:initial_monetary, "Initial monetary action"),
              (:initial_fiscal, "Initial fiscal action"),
              (:common_macro_loss, "Forty-quarter macro loss"),
              (:spectral_radius, "Closed-loop spectral radius")]
    panels = Any[]
    for (field, title) in fields
        p = plot(title=title, xlabel="forward-looking weight theta",
                 ylabel=field in (:initial_monetary, :initial_fiscal) ? "policy gap" : "value")
        for name in names
            rows = demand[demand.regime .== name, :]
            plot!(p, rows.theta, rows[!, field], color=colors[name],
                  linestyle=styles[name], marker=:circle, markersize=3,
                  label=field == :initial_monetary ? name : false)
        end
        push!(panels, p)
    end
    savefig(plot(panels..., layout=(2, 2), size=(1100, 800),
                 plot_title="From backward-looking behaviour to a forward-looking NK economy",
                 left_margin=6Plots.mm, bottom_margin=5Plots.mm),
            joinpath(OUTPUT, "nk_forwardness_sensitivity.png"))
end

function save_channel_plot(channels, summary)
    monetary = channels[(channels.regime .== "Monetary commitment") .&
                        (channels.authority .== "Monetary"), :]
    fiscal = channels[(channels.regime .== "Fiscal commitment") .&
                      (channels.authority .== "Fiscal"), :]
    p1 = plot(monetary.theta, abs.(monetary.adjustment_state_wedge),
              marker=:circle, color=:navy, label="monetary persistence",
              xlabel="forward-looking weight theta", ylabel="absolute FOC wedge",
              title="Rival response through inherited policy")
    plot!(p1, fiscal.theta, abs.(fiscal.adjustment_state_wedge),
          marker=:diamond, color=:darkgreen, label="fiscal persistence")
    p2 = plot(monetary.theta, monetary.adjustment_state_share,
              marker=:circle, color=:navy, label="monetary persistence",
              xlabel="forward-looking weight theta", ylabel="bounded continuation share",
              title="Adjustment-state strategic share")
    plot!(p2, fiscal.theta, fiscal.adjustment_state_share,
          marker=:diamond, color=:darkgreen, label="fiscal persistence")

    demand = summary[summary.scenario .== "Demand contraction", :]
    flex = demand[demand.regime .== "Flexible Nash", :]
    mon = demand[demand.regime .== "Monetary commitment", :]
    fis = demand[demand.regime .== "Fiscal commitment", :]
    both = demand[demand.regime .== "Bilateral commitment", :]
    displacement(df) = [norm([df.initial_monetary[i] - flex.initial_monetary[i],
                              df.initial_fiscal[i] - flex.initial_fiscal[i]])
                        for i in 1:nrow(df)]
    p3 = plot(mon.theta, displacement(mon), marker=:circle, color=:navy,
              label="monetary persistence", xlabel="forward-looking weight theta",
              ylabel="distance from flexible Nash", title="Commitment-induced policy displacement")
    plot!(p3, fis.theta, displacement(fis), marker=:diamond, color=:darkgreen,
          label="fiscal persistence")
    plot!(p3, both.theta, displacement(both), marker=:star5, color=:firebrick,
          label="bilateral persistence")

    impacts = channels[(channels.regime .== "Bilateral commitment") .&
                       (channels.authority .== "Monetary"), :]
    p4 = plot(impacts.theta, impacts.monetary_private_impact,
              marker=:circle, color=:navy, label="monetary instrument",
              xlabel="forward-looking weight theta", ylabel="norm of current x-pi impact",
              title="Private-sector policy transmission")
    plot!(p4, impacts.theta, impacts.fiscal_private_impact,
          marker=:diamond, color=:darkgreen, label="fiscal instrument")
    savefig(plot(p1, p2, p3, p4, layout=(2, 2), size=(1100, 800),
                 plot_title="Forward-looking behaviour and strategic investment",
                 left_margin=7Plots.mm, bottom_margin=5Plots.mm),
            joinpath(OUTPUT, "nk_strategic_investment_channels.png"))
end

function save_irfs(paths, scenario, filename)
    fields = [(:output, "Output gap"), (:inflation, "Inflation gap"),
              (:debt, "Debt gap"), (:monetary, "Monetary instrument"),
              (:fiscal, "Fiscal instrument")]
    names = ["Flexible Nash", "Monetary commitment", "Fiscal commitment",
             "Bilateral commitment", "Coordination"]
    colors = Dict("Flexible Nash"=>:black, "Monetary commitment"=>:navy,
                  "Fiscal commitment"=>:darkgreen, "Bilateral commitment"=>:firebrick,
                  "Coordination"=>:darkorange)
    styles = Dict("Flexible Nash"=>:dash, "Monetary commitment"=>:solid,
                  "Fiscal commitment"=>:solid, "Bilateral commitment"=>:dashdot,
                  "Coordination"=>:dot)
    panels = Any[]
    shown = 16
    for (field, title) in fields
        p = plot(title=title, xlabel="quarter", ylabel="gap",
                 legend=field == :output ? :topright : false)
        for name in names
            plot!(p, 1:shown, getfield(paths[name], field)[1:shown],
                  color=colors[name], linestyle=styles[name], label=name)
        end
        push!(panels, p)
    end
    push!(panels, plot(framestyle=:none, axis=false, ticks=false,
        annotations=(0.5, 0.55, text("Private expectations transmit\nfuture policy into current outcomes", 11))))
    savefig(plot(panels..., layout=(3, 2), size=(1100, 1020),
                 plot_title=scenario, left_margin=5Plots.mm, right_margin=3Plots.mm,
                 top_margin=5Plots.mm, bottom_margin=6Plots.mm),
            joinpath(OUTPUT, filename))
end

function write_tex(summary, channels, search, backward_match)
    demand = summary[(summary.scenario .== "Demand contraction") .&
                     (summary.theta .== 1.0), :]
    getv(regime, column) = demand[demand.regime .== regime, column][1]
    mon = channels[(channels.theta .== 1.0) .&
                   (channels.regime .== "Monetary commitment") .&
                   (channels.authority .== "Monetary"), :]
    fis = channels[(channels.theta .== 1.0) .&
                   (channels.regime .== "Fiscal commitment") .&
                   (channels.authority .== "Fiscal"), :]
    mon_all = channels[(channels.regime .== "Monetary commitment") .&
                       (channels.authority .== "Monetary"), :]
    fis_all = channels[(channels.regime .== "Fiscal commitment") .&
                       (channels.authority .== "Fiscal"), :]
    mon_peak = mon_all[argmax(abs.(mon_all.adjustment_state_wedge)), :]
    fis_peak = fis_all[argmax(abs.(fis_all.adjustment_state_wedge)), :]
    open(joinpath(OUTPUT, "nk_results.tex"), "w") do io
        @printf(io, "\\newcommand{\\NKBackwardMatch}{%.2e}\n", backward_match)
        @printf(io, "\\newcommand{\\NKDistinctSolutions}{%d}\n", length(search.solutions))
        @printf(io, "\\newcommand{\\NKSearchAttempts}{%d}\n", length(search.attempts))
        @printf(io, "\\newcommand{\\NKFlexibleM}{%.4f}\n", getv("Flexible Nash", :initial_monetary))
        @printf(io, "\\newcommand{\\NKFlexibleF}{%.4f}\n", getv("Flexible Nash", :initial_fiscal))
        @printf(io, "\\newcommand{\\NKBilateralM}{%.4f}\n", getv("Bilateral commitment", :initial_monetary))
        @printf(io, "\\newcommand{\\NKBilateralF}{%.4f}\n", getv("Bilateral commitment", :initial_fiscal))
        @printf(io, "\\newcommand{\\NKCoordM}{%.4f}\n", getv("Coordination", :initial_monetary))
        @printf(io, "\\newcommand{\\NKCoordF}{%.4f}\n", getv("Coordination", :initial_fiscal))
        @printf(io, "\\newcommand{\\NKFlexibleLoss}{%.4f}\n", getv("Flexible Nash", :common_macro_loss))
        @printf(io, "\\newcommand{\\NKBilateralLoss}{%.4f}\n", getv("Bilateral commitment", :common_macro_loss))
        @printf(io, "\\newcommand{\\NKCoordLoss}{%.4f}\n", getv("Coordination", :common_macro_loss))
        @printf(io, "\\newcommand{\\NKMonInvestmentShare}{%.3f}\n", mon.adjustment_state_share[1])
        @printf(io, "\\newcommand{\\NKFisInvestmentShare}{%.3f}\n", fis.adjustment_state_share[1])
        @printf(io, "\\newcommand{\\NKMonPeakTheta}{%.2f}\n", mon_peak.theta)
        @printf(io, "\\newcommand{\\NKFisPeakTheta}{%.2f}\n", fis_peak.theta)
        @printf(io, "\\newcommand{\\NKMonPeakWedge}{%.2e}\n", abs(mon_peak.adjustment_state_wedge))
        @printf(io, "\\newcommand{\\NKFisPeakWedge}{%.2e}\n", abs(fis_peak.adjustment_state_wedge))
    end
end

function main()
    mkpath(OUTPUT)
    model = NKModel()
    regime_map = regimes()
    theta_grid = collect(0.0:0.1:1.0)
    solutions = solve_homotopy(model, theta_grid, regime_map)
    initial = states()

    summary = DataFrame(scenario=String[], theta=Float64[], regime=String[],
        initial_monetary=Float64[], initial_fiscal=Float64[], common_macro_loss=Float64[],
        actual_social_loss=Float64[], spectral_radius=Float64[], outer_iterations=Int[],
        peak_output=Float64[], peak_inflation=Float64[], peak_debt=Float64[])
    channels = DataFrame(theta=Float64[], regime=String[], authority=String[],
        strategic_wedge=Float64[], adjustment_state_wedge=Float64[],
        macro_state_wedge=Float64[], strategic_share=Float64[],
        adjustment_state_share=Float64[], monetary_private_impact=Float64[],
        fiscal_private_impact=Float64[])
    names = ["Flexible Nash", "Monetary commitment", "Fiscal commitment",
             "Bilateral commitment", "Coordination"]
    for theta in theta_grid, name in names
        rule = solutions[(theta, name)]
        regime = regime_map[name]
        for scenario in ["Demand contraction", "Cost-push inflation"]
            path = simulate_nk(rule, initial[scenario]; horizon=HORIZON)
            loss = evaluate_nk_path(path, model, regime, rule)
            push!(summary, (scenario, theta, name, path.monetary[1], path.fiscal[1],
                loss.common_macro, loss.social, real(rule.spectral_radius),
                rule.metadata.outer_iterations, maximum(abs, path.output),
                maximum(abs, path.inflation), maximum(abs, path.debt)))
        end
        if name != "Coordination"
            for authority in (:monetary, :fiscal)
                wedge = nk_strategic_wedge(rule, model, initial["Demand contraction"], authority)
                adjustment_share = bounded_component_share(wedge.adjustment_state,
                                                            wedge.total_continuation)
                reduction = rule.metadata.reduction
                push!(channels, (theta, name, authority == :monetary ? "Monetary" : "Fiscal",
                    wedge.strategic, wedge.adjustment_state, wedge.macro_states,
                    wedge.bounded_share, adjustment_share,
                    norm(reduction.outcomes[:, 8]), norm(reduction.outcomes[:, 9])))
            end
        end
    end
    CSV.write(joinpath(OUTPUT, "nk_theta_summary.csv"), summary)
    CSV.write(joinpath(OUTPUT, "nk_strategic_channels.csv"), channels)

    # Exact backward-looking replication is a required validation of the homotopy.
    old = solve_stationary_nash(GameModel(), Regime(name="old flexible"))
    new = solutions[(0.0, "Flexible Nash")]
    backward_match = norm(vcat(old.monetary_rule - new.monetary_rule,
                               old.fiscal_rule - new.fiscal_rule))

    reference = solutions[(0.0, "Bilateral commitment")].metadata.expectations
    search = search_nk_nash(model, regime_map["Bilateral commitment"], 1.0, reference)
    search_table = DataFrame(seed_index=Int[], converged=Bool[], stable=Bool[],
                             spectral_radius=Float64[], outer_residual=Float64[])
    for (index, solution) in enumerate(search.attempts)
        push!(search_table, (index, solution.metadata.outer_converged,
            solution.spectral_radius < 1.0, real(solution.spectral_radius),
            solution.metadata.outer_residual))
    end
    CSV.write(joinpath(OUTPUT, "nk_equilibrium_search.csv"), search_table)

    save_theta_plot(summary)
    save_channel_plot(channels, summary)
    pure_nk_paths = Dict{String, Dict{String, Any}}()
    for scenario in ["Demand contraction", "Cost-push inflation"]
        paths = Dict{String, Any}()
        for name in names
            paths[name] = simulate_nk(solutions[(1.0, name)], initial[scenario]; horizon=HORIZON)
        end
        pure_nk_paths[scenario] = paths
    end
    save_irfs(pure_nk_paths["Demand contraction"],
              "Forward-looking NK demand contraction", "nk_demand_irfs.png")
    save_irfs(pure_nk_paths["Cost-push inflation"],
              "Forward-looking NK cost-push inflation", "nk_cost_push_irfs.png")
    write_tex(summary, channels, search, backward_match)

    @printf("Backward-looking rule replication error: %.3e\n", backward_match)
    @printf("Pure-NK bilateral search: %d stable distinct solution(s) from %d converged starts\n",
            length(search.solutions), length(search.attempts))
    println("Wrote forward-looking NK results to $OUTPUT")
end

main()

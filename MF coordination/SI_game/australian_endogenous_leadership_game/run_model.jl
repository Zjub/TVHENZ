using CSV
using DataFrames
using LinearAlgebra
using Plots
using Printf
using TOML

include(joinpath(@__DIR__, "endogenous_leadership.jl"))
using .EndogenousLeadership

const ROOT = @__DIR__
const OUTPUT = joinpath(ROOT, "output")
const HORIZON = 40
const DEMAND_SD = 0.46387449164984773
const SUPPLY_SD = 0.6699569103763775
const LAMBDA_M = 0.05
const LAMBDA_F = 0.04

default(fontfamily="Computer Modern", linewidth=2, framestyle=:box,
        gridalpha=0.22, legendfontsize=8)

baseline_model() = GameModel()

"""Change only the two own-instrument level penalties."""
function instrument_penalty_model(scale)
    base = baseline_model()
    monetary = Mandate(inflation=base.monetary.inflation,
        output=base.monetary.output, debt=base.monetary.debt,
        own_level=scale * base.monetary.own_level,
        discount=base.monetary.discount)
    fiscal = Mandate(inflation=base.fiscal.inflation,
        output=base.fiscal.output, debt=base.fiscal.debt,
        own_level=scale * base.fiscal.own_level,
        discount=base.fiscal.discount)
    return GameModel(calibration=base.calibration, monetary=monetary,
                     fiscal=fiscal, social=base.social)
end

"""Static best-response curvatures and their dimensionless slope product."""
function interaction_statistics(model)
    zero = Regime(name="analytical zero-cost benchmark")
    Qm = authority_loss(model, zero, :monetary)
    Qf = authority_loss(model, zero, :fiscal)
    dm, df = Qm[8, 8], Qf[9, 9]
    cm, cf = Qm[8, 9], Qf[9, 8]
    return (dm=dm, df=df, cm=cm, cf=cf,
            monetary_response=-cm / dm, fiscal_response=-cf / df,
            gamma=(-cm / dm) * (-cf / df))
end

function regimes()
    return Dict(
        "Flexible Nash" => Regime(name="Flexible Nash"),
        "Monetary commitment" => Regime(name="Monetary commitment", monetary_adjustment=LAMBDA_M),
        "Fiscal commitment" => Regime(name="Fiscal commitment", fiscal_adjustment=LAMBDA_F),
        "Bilateral commitment" => Regime(name="Bilateral commitment",
                                           monetary_adjustment=LAMBDA_M,
                                           fiscal_adjustment=LAMBDA_F),
    )
end

initial_states() = Dict(
    "Demand contraction" => [0.0, 0.0, 0.0, 0.0, 0.0, -DEMAND_SD, 0.0],
    "Cost-push inflation" => [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, SUPPLY_SD],
)

function select_solution(search, name)
    isempty(search.solutions) && error("no stable stationary solution for $name")
    return first(search.solutions)
end

function save_irfs(paths, scenario, filename)
    fields = [(:output, "Output gap"), (:inflation, "Inflation gap"),
              (:debt, "Debt gap"), (:monetary, "Monetary instrument"),
              (:fiscal, "Fiscal instrument")]
    order = ["Flexible Nash", "Monetary commitment", "Fiscal commitment",
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
        for name in order
            plot!(p, 1:shown, getfield(paths[name], field)[1:shown], label=name,
                  color=colors[name], linestyle=styles[name])
        end
        push!(panels, p)
    end
    push!(panels, plot(framestyle=:none, axis=false, ticks=false,
        annotations=(0.5, 0.55, text("Adjustment costs create\nendogenous commitment capacity", 11))))
    fig = plot(panels..., layout=(3, 2), size=(1100, 900), plot_title=scenario,
               left_margin=4Plots.mm, bottom_margin=3Plots.mm)
    savefig(fig, joinpath(OUTPUT, filename))
end

function save_policy_map(static, dynamic, model, states)
    labels = ["Static Nash", "Monetary leader", "Fiscal leader", "Static cooperation",
              "Flexible Nash", "Monetary commitment", "Fiscal commitment",
              "Bilateral commitment", "Coordination"]
    colors = Dict("Static Nash"=>:grey35, "Monetary leader"=>:navy,
                  "Fiscal leader"=>:darkgreen, "Static cooperation"=>:darkorange,
                  "Flexible Nash"=>:black, "Monetary commitment"=>:blue,
                  "Fiscal commitment"=>:green4, "Bilateral commitment"=>:firebrick,
                  "Coordination"=>:orange)
    markers = Dict(name => (startswith(name, "Static") || occursin("leader", name) ? :diamond : :circle)
                   for name in labels)
    panels = Any[]
    for scenario in ["Demand contraction", "Cost-push inflation"]
        state = states[scenario]
        p = plot(title=scenario, xlabel="monetary instrument", ylabel="fiscal instrument",
                 legend=:outerright)
        for name in labels
            rule = haskey(static, name) ? static[name] : dynamic[name]
            m, f = dot(rule.monetary_rule, state), dot(rule.fiscal_rule, state)
            scatter!(p, [m], [f], label=name, color=colors[name], marker=markers[name],
                     markersize=6)
        end
        push!(panels, p)
    end
    savefig(plot(panels..., layout=(2, 1), size=(950, 850),
                 plot_title="Static leadership benchmarks and dynamic commitment regimes",
                 left_margin=5Plots.mm, bottom_margin=3Plots.mm),
            joinpath(OUTPUT, "policy_benchmark_map.png"))
end

function save_leadership_bars(table)
    names = table.regime
    positions = collect(1:length(names))
    p = bar(positions .- 0.18, table.monetary_leadership_index,
        label="toward monetary leader", color=:navy, bar_width=0.34,
        ylabel="fraction of flexible-Nash distance closed",
        title="Rule-space proximity to static leadership benchmarks",
        xticks=(positions, names), xrotation=18,
        left_margin=8Plots.mm, bottom_margin=8Plots.mm, legend=:topright)
    bar!(p, positions .+ 0.18, table.fiscal_leadership_index,
         label="toward fiscal leader", color=:darkgreen, bar_width=0.34)
    hline!(p, [0.0], color=:black, linewidth=1, label=false)
    savefig(p, joinpath(OUTPUT, "leadership_indices.png"))
end

function save_sensitivity(table)
    p1 = plot(table.scale, table.monetary_cost_leadership, marker=:circle,
              color=:navy, label="monetary cost -> monetary leader",
              xlabel="multiple of baseline own adjustment cost",
              ylabel="leader-proximity index", title="Unilateral commitment capacity")
    plot!(p1, table.scale, table.fiscal_cost_leadership, marker=:diamond,
          color=:darkgreen, label="fiscal cost -> fiscal leader")
    hline!(p1, [0.0], color=:black, linewidth=1, label=false)
    vline!(p1, [1.0], color=:grey45, linestyle=:dash, label="baseline")
    p2 = plot(table.scale, table.monetary_cost_common_loss, marker=:circle,
              color=:navy, label="monetary commitment", xlabel="cost multiple",
              ylabel="common macro loss", title="Demand-shock macro loss")
    plot!(p2, table.scale, table.fiscal_cost_common_loss, marker=:diamond,
          color=:darkgreen, label="fiscal commitment")
    savefig(plot(p1, p2, layout=(1, 2), size=(1100, 450),
                 left_margin=7Plots.mm, bottom_margin=5Plots.mm),
            joinpath(OUTPUT, "unilateral_commitment_sensitivity.png"))
end

function save_heatmaps(grid, flexible_loss)
    xs = sort(unique(grid.lambda_m))
    ys = sort(unique(grid.lambda_f))
    welfare = [grid[(grid.lambda_m .== x) .& (grid.lambda_f .== y), :common_macro_loss][1] - flexible_loss
               for y in ys, x in xs]
    bound = maximum(abs, welfare)
    p = heatmap(xs, ys, welfare, color=:balance, clims=(-bound, bound),
                xlabel="monetary adjustment cost", ylabel="fiscal adjustment cost",
                title="Demand-shock macro loss relative to flexible Nash",
                colorbar_title="loss difference", left_margin=6Plots.mm,
                right_margin=12Plots.mm, bottom_margin=4Plots.mm, size=(850, 540))
    scatter!(p, [LAMBDA_M], [LAMBDA_F], color=:black, marker=:star5,
             markersize=8, label="baseline bilateral game")
    savefig(p, joinpath(OUTPUT, "meta_nash_welfare_map.png"))

    monetary = [grid[(grid.lambda_m .== x) .& (grid.lambda_f .== y), :initial_monetary][1]
                for y in ys, x in xs]
    fiscal = [grid[(grid.lambda_m .== x) .& (grid.lambda_f .== y), :initial_fiscal][1]
              for y in ys, x in xs]
    p1 = heatmap(xs, ys, monetary, color=:viridis,
                 xlabel="monetary adjustment cost", ylabel="fiscal adjustment cost",
                 title="Initial monetary action", colorbar_title="m(0)")
    p2 = heatmap(xs, ys, fiscal, color=:viridis,
                 xlabel="monetary adjustment cost", ylabel="fiscal adjustment cost",
                 title="Initial fiscal action", colorbar_title="f(0)")
    for panel in (p1, p2)
        scatter!(panel, [LAMBDA_M], [LAMBDA_F], color=:white, marker=:star5,
                 markerstrokecolor=:black, markersize=8, label=false)
    end
    savefig(plot(p1, p2, layout=(1, 2), size=(1100, 450),
                 plot_title="Bilateral commitment-capacity game: demand contraction",
                 left_margin=6Plots.mm, bottom_margin=4Plots.mm),
            joinpath(OUTPUT, "meta_nash_policy_map.png"))
end

"""
Create a pre-calibration parameter map.

For macro curvature `(hM,hF,cM,cF)`, a common instrument-penalty multiplier
`a` and common transmission multiplier `t` imply

    Gamma = t^4 cM cF / ((a rM + t^2 hM)(a rF + t^2 hF)).

Gamma is the product of the two static best-response slopes.  The second map
solves the full recursive game and reports the envelope-based strategic share.
"""
function save_analytical_maps(model, demand_state)
    stats = interaction_statistics(model)
    rm, rf = model.monetary.own_level, model.fiscal.own_level
    hm, hf = stats.dm - rm, stats.df - rf
    cross_product = stats.cm * stats.cf

    penalty_scales = 10.0 .^ range(-4.0, 0.0; length=81)
    transmission_scales = 10.0 .^ range(log10(0.5), log10(30.0); length=81)
    gamma_surface = [
        t^4 * cross_product /
        ((a * rm + t^2 * hm) * (a * rf + t^2 * hf))
        for t in transmission_scales, a in penalty_scales
    ]
    p = heatmap(penalty_scales, transmission_scales, gamma_surface,
        xscale=:log10, yscale=:log10, color=:viridis,
        xlabel="common instrument-level penalty multiplier",
        ylabel="common policy-transmission multiplier",
        title="Static strategic interaction: response product Gamma",
        colorbar_title="Gamma", left_margin=7Plots.mm, bottom_margin=5Plots.mm,
        right_margin=10Plots.mm, size=(850, 560))
    contour!(p, penalty_scales, transmission_scales, gamma_surface,
             levels=[0.01, 0.10, 0.25, 0.40], color=:white,
             linewidth=1.2, labels=true)
    scatter!(p, [1.0], [1.0], marker=:star5, markersize=9,
             color=:red, markerstrokecolor=:white, label="Australian baseline")
    savefig(p, joinpath(OUTPUT, "analytical_interaction_map.png"))

    # Exact thresholds holding either macro transmission or the common level
    # penalty fixed.  These formulas follow directly from the expression above.
    thresholds = DataFrame(gamma=Float64[], level_penalty_multiplier=Float64[],
                           transmission_multiplier=Float64[])
    for target in [0.01, 0.05, 0.10, 0.25, 0.40]
        aa = rm * rf
        bb = hm * rf + hf * rm
        cc = hm * hf - cross_product / target
        penalty_root = (-bb + sqrt(bb^2 - 4aa * cc)) / (2aa)

        # Let y=t^2 and solve the resulting quadratic for its positive root.
        ay = target * hm * hf - cross_product
        by = target * (rm * hf + rf * hm)
        cy = target * rm * rf
        discriminant = by^2 - 4ay * cy
        roots = [(-by + sqrt(discriminant)) / (2ay),
                 (-by - sqrt(discriminant)) / (2ay)]
        y = minimum(filter(>(0.0), roots))
        push!(thresholds, (target, penalty_root, sqrt(y)))
    end
    CSV.write(joinpath(OUTPUT, "analytical_thresholds.csv"), thresholds)

    # The dimensionless persistence share p=lambda/(D+lambda) is the fraction
    # of one-period own-action curvature due to adjustment.  This grid combines
    # p with the static interaction strength and solves the full MPE.
    level_scales = [1.0, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001]
    persistence_shares = [0.05, 0.15, 0.30, 0.50, 0.70]
    dynamic_map = DataFrame(level_penalty_scale=Float64[], gamma=Float64[],
        persistence_share=Float64[], monetary_wedge_share=Float64[],
        fiscal_wedge_share=Float64[], stable=Bool[])
    for a in level_scales
        counterfactual = instrument_penalty_model(a)
        local_stats = interaction_statistics(counterfactual)
        for persistence in persistence_shares
            lm = persistence / (1.0 - persistence) * local_stats.dm
            lf = persistence / (1.0 - persistence) * local_stats.df
            regime_m = Regime(name="analytical M-only", monetary_adjustment=lm)
            regime_f = Regime(name="analytical F-only", fiscal_adjustment=lf)
            solution_m = solve_stationary_nash(counterfactual, regime_m)
            solution_f = solve_stationary_nash(counterfactual, regime_f)
            wedge_m = strategic_wedge_share(solution_m, counterfactual,
                                             regime_m, demand_state, :monetary)
            wedge_f = strategic_wedge_share(solution_f, counterfactual,
                                             regime_f, demand_state, :fiscal)
            stable = solution_m.metadata.converged && solution_f.metadata.converged &&
                     solution_m.spectral_radius < 1.0 && solution_f.spectral_radius < 1.0
            push!(dynamic_map, (a, local_stats.gamma, persistence,
                                wedge_m.bounded_share, wedge_f.bounded_share, stable))
        end
    end
    CSV.write(joinpath(OUTPUT, "dynamic_strategic_ranges.csv"), dynamic_map)

    gammas = [interaction_statistics(instrument_penalty_model(a)).gamma
              for a in level_scales]
    x = log10.(gammas)
    monetary_matrix = [dynamic_map[
        (dynamic_map.gamma .== gamma) .&
        (dynamic_map.persistence_share .== persistence), :monetary_wedge_share][1]
        for persistence in persistence_shares, gamma in gammas]
    fiscal_matrix = [dynamic_map[
        (dynamic_map.gamma .== gamma) .&
        (dynamic_map.persistence_share .== persistence), :fiscal_wedge_share][1]
        for persistence in persistence_shares, gamma in gammas]
    tick_locations = x[[1, 3, 5, 7]]
    tick_labels = [@sprintf("%.3g", g) for g in gammas[[1, 3, 5, 7]]]
    p1 = heatmap(x, persistence_shares, monetary_matrix, clims=(0, 0.5),
        color=:thermal, xlabel="static response product Gamma (log scale)",
        ylabel="persistence share p", title="Monetary strategic share",
        xticks=(tick_locations, tick_labels), colorbar_title="FOC share")
    p2 = heatmap(x, persistence_shares, fiscal_matrix, clims=(0, 0.5),
        color=:thermal, xlabel="static response product Gamma (log scale)",
        ylabel="persistence share p", title="Fiscal strategic share",
        xticks=(tick_locations, tick_labels), colorbar_title="FOC share")
    savefig(plot(p1, p2, layout=(1, 2), size=(1100, 450),
                 plot_title="When does strategic investment matter?",
                 left_margin=7Plots.mm, bottom_margin=6Plots.mm,
                 right_margin=7Plots.mm),
            joinpath(OUTPUT, "dynamic_strategic_ranges.png"))

    return (stats=stats, thresholds=thresholds, dynamic_map=dynamic_map,
            persistence_m=LAMBDA_M / (stats.dm + LAMBDA_M),
            persistence_f=LAMBDA_F / (stats.df + LAMBDA_F))
end

function write_results_tex(summary, proximity, action_proximity, search_rows,
                           static, states, analytics, dynamic, model)
    demand = summary[summary.scenario .== "Demand contraction", :]
    supply = summary[summary.scenario .== "Cost-push inflation", :]
    getv(df, regime, col) = df[df.regime .== regime, col][1]
    prox(name, col) = proximity[proximity.regime .== name, col][1]
    m_follow = static["Monetary leader"].metadata.follower_leader_slope
    f_follow = static["Fiscal leader"].metadata.follower_leader_slope
    actionp(scenario, regime, col) = action_proximity[
        (action_proximity.scenario .== scenario) .& (action_proximity.regime .== regime), col][1]
    function leader_separation(scenario)
        state = states[scenario]
        ml = static["Monetary leader"]
        fl = static["Fiscal leader"]
        return norm([
            dot(ml.monetary_rule - fl.monetary_rule, state),
            dot(ml.fiscal_rule - fl.fiscal_rule, state),
        ])
    end
    file = joinpath(OUTPUT, "results.tex")
    open(file, "w") do io
        @printf(io, "\\newcommand{\\DemandSD}{%.3f}\n", DEMAND_SD)
        @printf(io, "\\newcommand{\\SupplySD}{%.3f}\n", SUPPLY_SD)
        @printf(io, "\\newcommand{\\MonetaryFollowerSlope}{%.4f}\n", m_follow)
        @printf(io, "\\newcommand{\\FiscalFollowerSlope}{%.4f}\n", f_follow)
        @printf(io, "\\newcommand{\\MonCostMIndex}{%.3f}\n", prox("Monetary commitment", :monetary_leadership_index))
        @printf(io, "\\newcommand{\\FisCostFIndex}{%.3f}\n", prox("Fiscal commitment", :fiscal_leadership_index))
        @printf(io, "\\newcommand{\\BothMIndex}{%.3f}\n", prox("Bilateral commitment", :monetary_leadership_index))
        @printf(io, "\\newcommand{\\BothFIndex}{%.3f}\n", prox("Bilateral commitment", :fiscal_leadership_index))
        @printf(io, "\\newcommand{\\DemandMonActionIndex}{%.3f}\n",
                actionp("Demand contraction", "Monetary commitment", :monetary_leadership_index))
        @printf(io, "\\newcommand{\\DemandFisActionIndex}{%.3f}\n",
                actionp("Demand contraction", "Fiscal commitment", :fiscal_leadership_index))
        @printf(io, "\\newcommand{\\SupplyMonActionIndex}{%.3f}\n",
                actionp("Cost-push inflation", "Monetary commitment", :monetary_leadership_index))
        @printf(io, "\\newcommand{\\SupplyFisActionIndex}{%.3f}\n",
                actionp("Cost-push inflation", "Fiscal commitment", :fiscal_leadership_index))
        @printf(io, "\\newcommand{\\DemandLeaderSeparation}{%.4f}\n",
                leader_separation("Demand contraction"))
        @printf(io, "\\newcommand{\\SupplyLeaderSeparation}{%.4f}\n",
                leader_separation("Cost-push inflation"))
        @printf(io, "\\newcommand{\\BaselineGamma}{%.2e}\n", analytics.stats.gamma)
        @printf(io, "\\newcommand{\\BaselinePersistenceM}{%.3f}\n", analytics.persistence_m)
        @printf(io, "\\newcommand{\\BaselinePersistenceF}{%.3f}\n", analytics.persistence_f)
        gamma_ten = analytics.thresholds[analytics.thresholds.gamma .== 0.10, :]
        @printf(io, "\\newcommand{\\GammaTenPenaltyScale}{%.3f}\n",
                gamma_ten.level_penalty_multiplier[1])
        @printf(io, "\\newcommand{\\GammaTenTransmissionScale}{%.2f}\n",
                gamma_ten.transmission_multiplier[1])
        bilateral_regime = regimes()["Bilateral commitment"]
        demand_state = states["Demand contraction"]
        wedge_m = strategic_wedge_share(dynamic["Bilateral commitment"], model,
            bilateral_regime, demand_state, :monetary)
        wedge_f = strategic_wedge_share(dynamic["Bilateral commitment"], model,
            bilateral_regime, demand_state, :fiscal)
        @printf(io, "\\newcommand{\\BaselineMonetaryWedgeShare}{%.3f}\n",
                wedge_m.bounded_share)
        @printf(io, "\\newcommand{\\BaselineFiscalWedgeShare}{%.3f}\n",
                wedge_f.bounded_share)
        for (prefix, df) in [("Demand", demand), ("Supply", supply)]
            for (tag, regime) in [("Flexible", "Flexible Nash"),
                                    ("Monetary", "Monetary commitment"),
                                    ("Fiscal", "Fiscal commitment"),
                                    ("Both", "Bilateral commitment"),
                                    ("Coord", "Coordination")]
                @printf(io, "\\newcommand{\\%s%sLoss}{%.4f}\n", prefix, tag,
                        getv(df, regime, :common_macro_loss))
                @printf(io, "\\newcommand{\\%s%sM}{%.4f}\n", prefix, tag,
                        getv(df, regime, :initial_monetary))
                @printf(io, "\\newcommand{\\%s%sF}{%.4f}\n", prefix, tag,
                        getv(df, regime, :initial_fiscal))
            end
        end
        bilateral = search_rows[search_rows.regime .== "Bilateral commitment", :]
        @printf(io, "\\newcommand{\\BilateralRadius}{%.4f}\n",
                getv(summary, "Bilateral commitment", :spectral_radius))
        @printf(io, "\\newcommand{\\SearchStarts}{%d}\n", nrow(bilateral))
        @printf(io, "\\newcommand{\\SearchMaxDistance}{%.2e}\n", maximum(bilateral.distance_to_selected))
    end
end

function main()
    mkpath(OUTPUT)
    model = baseline_model()
    static = solve_static_games(model)
    regime_map = regimes()

    dynamic = Dict{String, Any}()
    search_rows = DataFrame(regime=String[], seed=Float64[], converged=Bool[],
                            spectral_radius=Float64[], residual=Float64[],
                            distance_to_selected=Float64[])
    for name in ["Flexible Nash", "Monetary commitment", "Fiscal commitment", "Bilateral commitment"]
        search = search_stationary_nash(model, regime_map[name])
        selected = select_solution(search, name)
        dynamic[name] = selected
        for attempt in search.attempts
            push!(search_rows, (name, attempt.metadata.seed_scale,
                attempt.metadata.converged, real(attempt.spectral_radius),
                attempt.metadata.residual, rule_distance(attempt, selected)))
        end
    end
    # The requested coordination benchmark has the true social objective and
    # flexible instruments.  It is therefore compared with flexible Nash, not
    # with a planner facing the bilateral adjustment-cost technology.
    coordination_regime = Regime(name="Coordination")
    dynamic["Coordination"] = solve_stationary_cooperation(model, coordination_regime)
    CSV.write(joinpath(OUTPUT, "equilibrium_search.csv"), search_rows)

    # Rule-level proximity to the two original Stackelberg benchmarks.
    proximity = DataFrame(regime=String[], monetary_leadership_index=Float64[],
                          fiscal_leadership_index=Float64[], distance_to_static_nash=Float64[],
                          distance_to_monetary_leader=Float64[], distance_to_fiscal_leader=Float64[])
    flexible = dynamic["Flexible Nash"]
    for name in ["Flexible Nash", "Monetary commitment", "Fiscal commitment",
                 "Bilateral commitment", "Coordination"]
        rule = dynamic[name]
        push!(proximity, (name,
            leadership_index(rule, flexible, static["Monetary leader"]),
            leadership_index(rule, flexible, static["Fiscal leader"]),
            rule_distance(rule, static["Static Nash"]),
            rule_distance(rule, static["Monetary leader"]),
            rule_distance(rule, static["Fiscal leader"])))
    end
    CSV.write(joinpath(OUTPUT, "leader_proximity.csv"), proximity)

    states = initial_states()
    analytics = save_analytical_maps(model, states["Demand contraction"])
    summary = DataFrame(scenario=String[], regime=String[], lambda_m=Float64[], lambda_f=Float64[],
                        monetary_loss=Float64[], fiscal_loss=Float64[], social_loss=Float64[],
                        common_macro_loss=Float64[], initial_monetary=Float64[], initial_fiscal=Float64[],
                        peak_output=Float64[], peak_inflation=Float64[], peak_debt=Float64[],
                        spectral_radius=Float64[])
    all_paths = Dict{String, Dict{String, Any}}()
    for scenario in ["Demand contraction", "Cost-push inflation"]
        state = states[scenario]
        paths = Dict{String, Any}()
        for name in ["Flexible Nash", "Monetary commitment", "Fiscal commitment", "Bilateral commitment"]
            rule, regime = dynamic[name], regime_map[name]
            path = simulate_rule(rule, model, state; horizon=HORIZON)
            loss = evaluate_path(path, model, regime)
            paths[name] = path
            push!(summary, (scenario, name, regime.monetary_adjustment,
                regime.fiscal_adjustment, loss.monetary, loss.fiscal, loss.social,
                loss.common_macro, path.monetary[1], path.fiscal[1],
                maximum(abs, path.output), maximum(abs, path.inflation),
                maximum(abs, path.debt), real(rule.spectral_radius)))
        end
        coord_regime = coordination_regime
        coord_path = simulate_rule(dynamic["Coordination"], model, state; horizon=HORIZON)
        coord_loss = evaluate_path(coord_path, model, coord_regime)
        paths["Coordination"] = coord_path
        push!(summary, (scenario, "Coordination", coord_regime.monetary_adjustment,
            coord_regime.fiscal_adjustment, NaN, NaN, coord_loss.social,
            coord_loss.common_macro, coord_path.monetary[1], coord_path.fiscal[1],
            maximum(abs, coord_path.output), maximum(abs, coord_path.inflation),
            maximum(abs, coord_path.debt), real(dynamic["Coordination"].spectral_radius)))
        all_paths[scenario] = paths
    end
    CSV.write(joinpath(OUTPUT, "regime_summary.csv"), summary)

    # Shock-specific action distances are easier to interpret than distances
    # between full feedback-rule coefficient vectors.  They remain diagnostics,
    # not proofs of endogenous leadership.
    action_proximity = DataFrame(scenario=String[], regime=String[],
        monetary_leadership_index=Float64[], fiscal_leadership_index=Float64[],
        distance_to_monetary_leader=Float64[], distance_to_fiscal_leader=Float64[])
    action(rule, state) = [dot(rule.monetary_rule, state), dot(rule.fiscal_rule, state)]
    for scenario in ["Demand contraction", "Cost-push inflation"]
        state = states[scenario]
        flexible_action = action(dynamic["Flexible Nash"], state)
        monetary_leader = action(static["Monetary leader"], state)
        fiscal_leader = action(static["Fiscal leader"], state)
        for name in ["Flexible Nash", "Monetary commitment", "Fiscal commitment",
                     "Bilateral commitment", "Coordination"]
            candidate = action(dynamic[name], state)
            dm0 = norm(flexible_action - monetary_leader)
            df0 = norm(flexible_action - fiscal_leader)
            push!(action_proximity, (scenario, name,
                1.0 - norm(candidate - monetary_leader) / dm0,
                1.0 - norm(candidate - fiscal_leader) / df0,
                norm(candidate - monetary_leader), norm(candidate - fiscal_leader)))
        end
    end
    CSV.write(joinpath(OUTPUT, "shock_action_proximity.csv"), action_proximity)

    static_table = DataFrame(scenario=String[], equilibrium=String[],
                             monetary=Float64[], fiscal=Float64[])
    for scenario in ["Demand contraction", "Cost-push inflation"],
        name in ["Static Nash", "Monetary leader", "Fiscal leader", "Static cooperation"]
        candidate = action(static[name], states[scenario])
        push!(static_table, (scenario, name, candidate[1], candidate[2]))
    end
    CSV.write(joinpath(OUTPUT, "static_benchmarks.csv"), static_table)

    state_names = ["lagged_output", "lagged_inflation", "debt", "lagged_monetary",
                   "lagged_fiscal", "demand_shock", "cost_push_shock"]
    rules_table = DataFrame(regime=String[], authority=String[], state=String[], coefficient=Float64[])
    for name in keys(dynamic), (authority, rule) in
        [("Monetary", dynamic[name].monetary_rule), ("Fiscal", dynamic[name].fiscal_rule)]
        for (state_name, coefficient) in zip(state_names, rule)
            push!(rules_table, (name, authority, state_name, coefficient))
        end
    end
    CSV.write(joinpath(OUTPUT, "policy_rules.csv"), rules_table)

    # Unilateral-cost continuation: does the MPE move toward the relevant leader?
    sensitivity = DataFrame(scale=Float64[], monetary_cost_leadership=Float64[],
                            fiscal_cost_leadership=Float64[], monetary_cost_common_loss=Float64[],
                            fiscal_cost_common_loss=Float64[])
    demand_state = states["Demand contraction"]
    for scale in collect(0.0:0.25:5.0)
        rm = Regime(name="M-only", monetary_adjustment=scale * LAMBDA_M)
        rf = Regime(name="F-only", fiscal_adjustment=scale * LAMBDA_F)
        sol_m = solve_stationary_nash(model, rm)
        sol_f = solve_stationary_nash(model, rf)
        path_m = simulate_rule(sol_m, model, demand_state; horizon=HORIZON)
        path_f = simulate_rule(sol_f, model, demand_state; horizon=HORIZON)
        push!(sensitivity, (scale,
            leadership_index(sol_m, flexible, static["Monetary leader"]),
            leadership_index(sol_f, flexible, static["Fiscal leader"]),
            evaluate_path(path_m, model, rm).common_macro,
            evaluate_path(path_f, model, rf).common_macro))
    end
    CSV.write(joinpath(OUTPUT, "unilateral_commitment_sensitivity.csv"), sensitivity)

    # Both commitment capacities: the MPE is the endogenous leadership/meta-Nash game.
    grid = DataFrame(lambda_m=Float64[], lambda_f=Float64[], stable=Bool[],
                     distance_m_leader=Float64[], distance_f_leader=Float64[],
                     leader_balance=Float64[], common_macro_loss=Float64[],
                     initial_monetary=Float64[], initial_fiscal=Float64[])
    for lm in collect(0.0:0.0125:0.10), lf in collect(0.0:0.01:0.08)
        regime = Regime(name="grid", monetary_adjustment=lm, fiscal_adjustment=lf)
        solution = solve_stationary_nash(model, regime)
        stable = solution.metadata.converged && solution.spectral_radius < 1.0
        dm = rule_distance(solution, static["Monetary leader"])
        df = rule_distance(solution, static["Fiscal leader"])
        balance = (df - dm) / (df + dm)
        path = simulate_rule(solution, model, demand_state; horizon=HORIZON)
        loss = evaluate_path(path, model, regime).common_macro
        push!(grid, (lm, lf, stable, dm, df, balance, loss,
                     path.monetary[1], path.fiscal[1]))
    end
    CSV.write(joinpath(OUTPUT, "meta_nash_grid.csv"), grid)

    calibration = Dict(
        "estimated" => Dict("rho_x"=>model.calibration.rho_x,
            "rho_pi"=>model.calibration.rho_pi, "sigma"=>model.calibration.sigma,
            "fiscal_multiplier"=>model.calibration.fiscal_multiplier,
            "kappa"=>model.calibration.kappa, "demand_shock_sd"=>DEMAND_SD,
            "supply_shock_sd"=>SUPPLY_SD),
        "commitment" => Dict("monetary_adjustment"=>LAMBDA_M,
                             "fiscal_adjustment"=>LAMBDA_F),
    )
    open(joinpath(OUTPUT, "calibration.toml"), "w") do io
        TOML.print(io, calibration)
    end

    save_irfs(all_paths["Demand contraction"], "Australian demand contraction",
              "demand_commitment_paths.png")
    save_irfs(all_paths["Cost-push inflation"], "Australian cost-push inflation",
              "cost_push_commitment_paths.png")
    save_policy_map(static, dynamic, model, states)
    save_leadership_bars(proximity)
    save_sensitivity(sensitivity)
    flexible_demand_loss = summary[(summary.scenario .== "Demand contraction") .&
                                   (summary.regime .== "Flexible Nash"), :common_macro_loss][1]
    save_heatmaps(grid, flexible_demand_loss)
    write_results_tex(summary, proximity, action_proximity, search_rows,
                      static, states, analytics, dynamic, model)

    println(proximity)
    @printf("Bilateral MPE spectral radius: %.6f\n", dynamic["Bilateral commitment"].spectral_radius)
    println("Wrote model results and figures to $OUTPUT")
end

main()

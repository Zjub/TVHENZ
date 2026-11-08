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
const DEMAND_STATE = [0.0, 0.0, 0.0, 0.0, 0.0, -0.46387449164984773, 0.0]
const BASE_LAMBDA_M = 0.05
const BASE_LAMBDA_F = 0.04

default(fontfamily="Computer Modern", linewidth=2, framestyle=:box,
        gridalpha=0.22, legendfontsize=8)

"""Copy the calibration while changing selected forward-looking parameters."""
function recalibrate(c::NKCalibration;
                     sigma=c.sigma,
                     fiscal_multiplier=c.fiscal_multiplier,
                     kappa=c.kappa,
                     private_discount=c.private_discount,
                     rho_b=c.rho_b)
    return NKCalibration(
        rho_x=c.rho_x,
        rho_pi=c.rho_pi,
        sigma=sigma,
        fiscal_multiplier=fiscal_multiplier,
        kappa=kappa,
        private_discount=private_discount,
        rho_b=rho_b,
        debt_from_interest=c.debt_from_interest,
        debt_from_fiscal=c.debt_from_fiscal,
        debt_from_inflation=c.debt_from_inflation,
        rho_d=c.rho_d,
        rho_u=c.rho_u,
    )
end

"""The inherited-policy term as a bounded share of the full continuation FOC."""
investment_share(wedge) =
    abs(wedge.adjustment_state) /
    (abs(wedge.adjustment_state) +
     abs(wedge.total_continuation - wedge.adjustment_state) + eps(Float64))

policy_at(rule, state) =
    [dot(rule.monetary_rule, state), dot(rule.fiscal_rule, state)]

function matrix_from(df, xname, yname, zname, xs, ys)
    return [begin
        row = df[(df[!, xname] .== x) .& (df[!, yname] .== y), :]
        nrow(row) == 0 ? NaN : row[1, zname]
    end for y in ys, x in xs]
end

function mark_baseline!(p, x, y; label="Australian baseline")
    scatter!(p, [x], [y], marker=:star5, markersize=8,
             markercolor=:white, markerstrokecolor=:black,
             markerstrokewidth=1.2, label=label)
end

"""
Map adjustment persistence against the strength of direct monetary and fiscal
transmission.  The persistence share p=lambda/(D+lambda) is dimensionless;
D is the authority's one-period own-action curvature in flexible Nash for the
same transmission calibration.
"""
function transmission_persistence_map(base_model::NKModel)
    transmission_scales = [0.50, 0.75, 1.00, 1.50, 2.00, 3.00]
    persistence_shares = [0.05, 0.15, 0.30, 0.50, 0.70, 0.85]
    results = DataFrame(
        transmission_scale=Float64[], persistence_share=Float64[],
        lambda_m=Float64[], lambda_f=Float64[],
        monetary_investment_share=Float64[], fiscal_investment_share=Float64[],
        monetary_total_strategic_share=Float64[], fiscal_total_strategic_share=Float64[],
        monetary_rival_slope=Float64[], fiscal_rival_slope=Float64[],
        monetary_policy_displacement=Float64[], fiscal_policy_displacement=Float64[],
        stability_margin=Float64[], converged=Bool[])

    baseline_dm = NaN
    baseline_df = NaN
    for scale in transmission_scales
        calibration = recalibrate(base_model.calibration;
            sigma=scale * base_model.calibration.sigma,
            fiscal_multiplier=scale * base_model.calibration.fiscal_multiplier)
        model = NKModel(calibration=calibration, monetary=base_model.monetary,
                        fiscal=base_model.fiscal, social=base_model.social)
        flexible = solve_nk_nash(model, Regime(name="Flexible Nash"), 1.0)
        flexible.metadata.outer_converged || error("flexible NK map failed at scale=$scale")
        direct_m = flexible.metadata.Qm[8, 8]
        direct_f = flexible.metadata.Qf[9, 9]
        if scale == 1.0
            baseline_dm, baseline_df = direct_m, direct_f
        end
        flexible_policy = policy_at(flexible, DEMAND_STATE)

        for persistence in persistence_shares
            lambda_m = persistence / (1.0 - persistence) * direct_m
            lambda_f = persistence / (1.0 - persistence) * direct_f
            monetary_regime = Regime(name="Monetary persistence",
                                     monetary_adjustment=lambda_m)
            fiscal_regime = Regime(name="Fiscal persistence",
                                   fiscal_adjustment=lambda_f)
            try
                monetary = solve_nk_nash(model, monetary_regime, 1.0;
                    initial_expectations=flexible.metadata.expectations)
                fiscal = solve_nk_nash(model, fiscal_regime, 1.0;
                    initial_expectations=flexible.metadata.expectations)
                converged = monetary.metadata.outer_converged &&
                            fiscal.metadata.outer_converged
                wm = nk_strategic_wedge(monetary, model, DEMAND_STATE, :monetary)
                wf = nk_strategic_wedge(fiscal, model, DEMAND_STATE, :fiscal)
                displacement_m = norm(policy_at(monetary, DEMAND_STATE) - flexible_policy)
                displacement_f = norm(policy_at(fiscal, DEMAND_STATE) - flexible_policy)
                margin = 1.0 - max(monetary.spectral_radius, fiscal.spectral_radius)
                push!(results, (scale, persistence, lambda_m, lambda_f,
                    investment_share(wm), investment_share(wf),
                    wm.bounded_share, wf.bounded_share,
                    monetary.fiscal_rule[4], fiscal.monetary_rule[5],
                    displacement_m, displacement_f, margin, converged))
            catch error
                @warn "parameter-region solve failed" scale persistence error
                push!(results, (scale, persistence, lambda_m, lambda_f,
                    NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, false))
            end
        end
    end
    baseline_p_m = BASE_LAMBDA_M / (baseline_dm + BASE_LAMBDA_M)
    baseline_p_f = BASE_LAMBDA_F / (baseline_df + BASE_LAMBDA_F)
    return results, transmission_scales, persistence_shares,
           baseline_p_m, baseline_p_f
end

function save_transmission_persistence_map(df, xs, ys, baseline_p_m, baseline_p_f)
    monetary = 100 .* matrix_from(df, :transmission_scale, :persistence_share,
                                  :monetary_investment_share, xs, ys)
    fiscal = 100 .* matrix_from(df, :transmission_scale, :persistence_share,
                                :fiscal_investment_share, xs, ys)
    displacement = matrix_from(df, :transmission_scale, :persistence_share,
        :monetary_policy_displacement, xs, ys) .+
        matrix_from(df, :transmission_scale, :persistence_share,
        :fiscal_policy_displacement, xs, ys)
    margin = matrix_from(df, :transmission_scale, :persistence_share,
                         :stability_margin, xs, ys)

    monetary_max = maximum(filter(isfinite, vec(monetary)))
    fiscal_max = maximum(filter(isfinite, vec(fiscal)))
    p1 = heatmap(xs, ys, monetary, color=:thermal, clims=(0, monetary_max),
        xlabel="policy-transmission scale", ylabel="persistence share p",
        title="Monetary inherited-policy share (%)", colorbar_title="percent")
    mark_baseline!(p1, 1.0, baseline_p_m)
    p2 = heatmap(xs, ys, fiscal, color=:thermal, clims=(0, fiscal_max),
        xlabel="policy-transmission scale", ylabel="persistence share p",
        title="Fiscal inherited-policy share (%)", colorbar_title="percent")
    mark_baseline!(p2, 1.0, baseline_p_f)
    p3 = heatmap(xs, ys, displacement, color=:viridis,
        xlabel="policy-transmission scale", ylabel="persistence share p",
        title="Sum of policy displacements", colorbar_title="distance")
    mark_baseline!(p3, 1.0, (baseline_p_m + baseline_p_f) / 2)
    p4 = heatmap(xs, ys, margin, color=:dense,
        xlabel="policy-transmission scale", ylabel="persistence share p",
        title="Closed-loop stability margin", colorbar_title="1-rho")
    mark_baseline!(p4, 1.0, (baseline_p_m + baseline_p_f) / 2)
    savefig(plot(p1, p2, p3, p4, layout=(2, 2), size=(1150, 850),
                 plot_title="Where strategic investment matters in the forward-looking game",
                 left_margin=7Plots.mm, bottom_margin=6Plots.mm),
            joinpath(OUTPUT, "nk_strategic_regions_transmission.png"))
end

"""Map the private discount factor against debt persistence in bilateral Nash."""
function expectations_persistence_map(base_model::NKModel)
    private_discounts = [0.85, 0.90, 0.95, 0.975, 0.99, 0.999]
    debt_persistence = [0.80, 0.90, 0.95, 0.975, 0.99, 0.995]
    results = DataFrame(
        private_discount=Float64[], debt_persistence=Float64[],
        maximum_investment_share=Float64[], maximum_total_strategic_share=Float64[],
        expectations_multiplier=Float64[], policy_displacement=Float64[],
        stability_margin=Float64[], outer_iterations=Int[], converged=Bool[])

    for rho_b in debt_persistence, beta_p in private_discounts
        calibration = recalibrate(base_model.calibration;
            private_discount=beta_p, rho_b=rho_b)
        model = NKModel(calibration=calibration, monetary=base_model.monetary,
                        fiscal=base_model.fiscal, social=base_model.social)
        flexible = solve_nk_nash(model, Regime(name="Flexible Nash"), 1.0)
        bilateral_regime = Regime(name="Bilateral persistence",
            monetary_adjustment=BASE_LAMBDA_M,
            fiscal_adjustment=BASE_LAMBDA_F)
        try
            bilateral = solve_nk_nash(model, bilateral_regime, 1.0;
                initial_expectations=flexible.metadata.expectations)
            wm = nk_strategic_wedge(bilateral, model, DEMAND_STATE, :monetary)
            wf = nk_strategic_wedge(bilateral, model, DEMAND_STATE, :fiscal)
            reduction = bilateral.metadata.reduction
            H = [1.0 calibration.sigma; 0.0 calibration.private_discount]
            expectations_multiplier = opnorm(reduction.private_matrix \ H)
            displacement = norm(policy_at(bilateral, DEMAND_STATE) -
                                policy_at(flexible, DEMAND_STATE))
            push!(results, (beta_p, rho_b,
                max(investment_share(wm), investment_share(wf)),
                max(wm.bounded_share, wf.bounded_share),
                expectations_multiplier, displacement,
                1.0 - bilateral.spectral_radius,
                bilateral.metadata.outer_iterations,
                bilateral.metadata.outer_converged))
        catch error
            @warn "expectations-region solve failed" beta_p rho_b error
            push!(results, (beta_p, rho_b, NaN, NaN, NaN, NaN, NaN, 0, false))
        end
    end
    return results, private_discounts, debt_persistence
end

function save_expectations_persistence_map(df, xs, ys)
    investment = 100 .* matrix_from(df, :private_discount, :debt_persistence,
                                    :maximum_investment_share, xs, ys)
    multiplier = matrix_from(df, :private_discount, :debt_persistence,
                             :expectations_multiplier, xs, ys)
    displacement = matrix_from(df, :private_discount, :debt_persistence,
                               :policy_displacement, xs, ys)
    margin = matrix_from(df, :private_discount, :debt_persistence,
                         :stability_margin, xs, ys)
    panels = [
        heatmap(xs, ys, investment, color=:thermal,
            xlabel="private discount beta_p", ylabel="debt persistence rho_b",
            title="Maximum inherited-policy share (%)", colorbar_title="percent"),
        heatmap(xs, ys, displacement, color=:viridis,
            xlabel="private discount beta_p", ylabel="debt persistence rho_b",
            title="Bilateral policy displacement", colorbar_title="distance"),
        heatmap(xs, ys, multiplier, color=:viridis,
            xlabel="private discount beta_p", ylabel="debt persistence rho_b",
            title="Private expectations multiplier", colorbar_title="norm"),
        heatmap(xs, ys, margin, color=:dense,
            xlabel="private discount beta_p", ylabel="debt persistence rho_b",
            title="Closed-loop stability margin", colorbar_title="1-rho"),
    ]
    for p in panels
        mark_baseline!(p, 0.99, 0.995)
    end
    savefig(plot(panels..., layout=(2, 2), size=(1150, 850),
                 plot_title="Expectations, state persistence, and strategic amplification",
                 left_margin=8Plots.mm, bottom_margin=6Plots.mm),
            joinpath(OUTPUT, "nk_strategic_regions_expectations.png"))
end

function write_region_tex(first_map, second_map, baseline_p_m, baseline_p_f)
    valid_first = first_map[first_map.converged .&
                            isfinite.(first_map.monetary_investment_share) .&
                            isfinite.(first_map.fiscal_investment_share), :]
    valid_second = second_map[second_map.converged .&
                              isfinite.(second_map.maximum_investment_share), :]
    max_m = valid_first[argmax(valid_first.monetary_investment_share), :]
    max_f = valid_first[argmax(valid_first.fiscal_investment_share), :]
    first_displacement = valid_first.monetary_policy_displacement .+
                         valid_first.fiscal_policy_displacement
    max_displacement = valid_first[argmax(first_displacement), :]
    max_expect = valid_second[argmax(valid_second.maximum_investment_share), :]
    max_expect_displacement = valid_second[argmax(valid_second.policy_displacement), :]
    max_multiplier = valid_second[argmax(valid_second.expectations_multiplier), :]
    min_margin = minimum(valid_second.stability_margin)
    open(joinpath(OUTPUT, "nk_region_results.tex"), "w") do io
        @printf(io, "\\newcommand{\\NKBaselinePersistenceM}{%.3f}\n", baseline_p_m)
        @printf(io, "\\newcommand{\\NKBaselinePersistenceF}{%.3f}\n", baseline_p_f)
        @printf(io, "\\newcommand{\\NKRegionMaxMShare}{%.2f}\n", 100 * max_m.monetary_investment_share)
        @printf(io, "\\newcommand{\\NKRegionMaxMTransmission}{%.2f}\n", max_m.transmission_scale)
        @printf(io, "\\newcommand{\\NKRegionMaxMPersistence}{%.2f}\n", max_m.persistence_share)
        @printf(io, "\\newcommand{\\NKRegionMaxFShare}{%.2f}\n", 100 * max_f.fiscal_investment_share)
        @printf(io, "\\newcommand{\\NKRegionMaxFTransmission}{%.2f}\n", max_f.transmission_scale)
        @printf(io, "\\newcommand{\\NKRegionMaxFPersistence}{%.2f}\n", max_f.persistence_share)
        @printf(io, "\\newcommand{\\NKRegionMaxFCellDisplacement}{%.3f}\n",
                max_f.monetary_policy_displacement + max_f.fiscal_policy_displacement)
        @printf(io, "\\newcommand{\\NKRegionMaxFCellMargin}{%.3f}\n",
                max_f.stability_margin)
        @printf(io, "\\newcommand{\\NKRegionMaxDisplacement}{%.3f}\n",
                max_displacement.monetary_policy_displacement +
                max_displacement.fiscal_policy_displacement)
        @printf(io, "\\newcommand{\\NKRegionMaxDisplacementTransmission}{%.2f}\n",
                max_displacement.transmission_scale)
        @printf(io, "\\newcommand{\\NKRegionMaxDisplacementPersistence}{%.2f}\n",
                max_displacement.persistence_share)
        @printf(io, "\\newcommand{\\NKRegionMaxDisplacementMargin}{%.3f}\n",
                max_displacement.stability_margin)
        @printf(io, "\\newcommand{\\NKExpectRegionMaxShare}{%.2f}\n", 100 * max_expect.maximum_investment_share)
        @printf(io, "\\newcommand{\\NKExpectRegionMaxBeta}{%.3f}\n", max_expect.private_discount)
        @printf(io, "\\newcommand{\\NKExpectRegionMaxDebt}{%.3f}\n", max_expect.debt_persistence)
        @printf(io, "\\newcommand{\\NKExpectRegionPeakShareDisplacement}{%.3f}\n",
                max_expect.policy_displacement)
        @printf(io, "\\newcommand{\\NKExpectRegionMaxDisplacement}{%.3f}\n",
                max_expect_displacement.policy_displacement)
        @printf(io, "\\newcommand{\\NKExpectRegionMaxDisplacementBeta}{%.3f}\n",
                max_expect_displacement.private_discount)
        @printf(io, "\\newcommand{\\NKExpectRegionMaxDisplacementDebt}{%.3f}\n",
                max_expect_displacement.debt_persistence)
        @printf(io, "\\newcommand{\\NKExpectRegionMaxDisplacementShare}{%.2f}\n",
                100 * max_expect_displacement.maximum_investment_share)
        @printf(io, "\\newcommand{\\NKExpectRegionMaxMultiplier}{%.3f}\n",
                max_multiplier.expectations_multiplier)
        @printf(io, "\\newcommand{\\NKExpectRegionMinMargin}{%.3f}\n", min_margin)
        @printf(io, "\\newcommand{\\NKRegionFirstConverged}{%d}\n", nrow(valid_first))
        @printf(io, "\\newcommand{\\NKRegionFirstTotal}{%d}\n", nrow(first_map))
        @printf(io, "\\newcommand{\\NKRegionSecondConverged}{%d}\n", nrow(valid_second))
        @printf(io, "\\newcommand{\\NKRegionSecondTotal}{%d}\n", nrow(second_map))
    end
end

function main()
    mkpath(OUTPUT)
    model = NKModel()
    println("Solving forward-looking transmission-persistence region map...")
    first_map, transmission, persistence, baseline_p_m, baseline_p_f =
        transmission_persistence_map(model)
    CSV.write(joinpath(OUTPUT, "nk_strategic_regions_transmission.csv"), first_map)
    save_transmission_persistence_map(first_map, transmission, persistence,
                                      baseline_p_m, baseline_p_f)

    println("Solving forward-looking expectations-persistence region map...")
    second_map, private_discounts, debt_persistence =
        expectations_persistence_map(model)
    CSV.write(joinpath(OUTPUT, "nk_strategic_regions_expectations.csv"), second_map)
    save_expectations_persistence_map(second_map, private_discounts, debt_persistence)
    write_region_tex(first_map, second_map, baseline_p_m, baseline_p_f)
    println("Wrote forward-looking parameter-region results to $OUTPUT")
end

"""Rebuild the figures from saved CSV files without resolving the grid."""
function replot_saved()
    first_map = CSV.read(joinpath(OUTPUT, "nk_strategic_regions_transmission.csv"),
                         DataFrame)
    second_map = CSV.read(joinpath(OUTPUT, "nk_strategic_regions_expectations.csv"),
                          DataFrame)
    transmission = sort(unique(first_map.transmission_scale))
    persistence = sort(unique(first_map.persistence_share))
    private_discounts = sort(unique(second_map.private_discount))
    debt_persistence = sort(unique(second_map.debt_persistence))
    save_transmission_persistence_map(first_map, transmission, persistence,
                                      0.330, 0.282)
    save_expectations_persistence_map(second_map, private_discounts,
                                      debt_persistence)
    write_region_tex(first_map, second_map, 0.330, 0.282)
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end

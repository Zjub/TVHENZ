# 16_make_plots.jl
#
# Publication- and slide-ready plots for the version-16 three-agent model.
# Reads the CSV outputs created by 16_three_agent_toy_model.jl and saves both
# high-resolution PNG and vector PDF versions in model16_figures/.

using CSV
using DataFrames
using Plots
using Printf
using Statistics

gr()

const FIGURE_DIR_16 = joinpath(@__DIR__, "model16_figures")
mkpath(FIGURE_DIR_16)

# e61 colours used in the research team's other figure scripts.
const E61_LIGHT_TEAL_16 = colorant"#66CED6"
const E61_TEAL_16 = colorant"#0D8982"
const E61_ORANGE_16 = colorant"#ED7F00"
const E61_RED_16 = colorant"#B40000"
const E61_NAVY_16 = colorant"#152B3C"
const E61_GREY_16 = colorant"#6B747C"
const E61_LIGHT_GREY_16 = colorant"#DCE3E6"

default(
    foreground_color_subplot=E61_NAVY_16,
    foreground_color_axis=E61_NAVY_16,
    foreground_color_text=E61_NAVY_16,
    background_color=:white,
    background_color_subplot=:white,
    grid=true,
    gridcolor=E61_LIGHT_GREY_16,
    gridalpha=0.55,
    linewidth=2.4,
    markersize=6,
    markerstrokewidth=1.5,
    legendfontsize=8,
    guidefontsize=10,
    tickfontsize=9,
    titlefontsize=12,
    titlefontcolor=E61_NAVY_16,
    dpi=300,
)

function require_inputs16(files)
    missing_files = filter(f -> !isfile(joinpath(@__DIR__, f)), files)
    isempty(missing_files) || error(
        "Missing model-16 outputs: $(join(missing_files, ", ")). " *
        "Run 16_three_agent_toy_model.jl first.")
end

require_inputs16([
    "16_three_agent_regression_moments.csv",
    "16_three_agent_cells.csv",
    "16_three_agent_path.csv",
    "16_three_agent_scenarios.csv",
    "16_three_agent_shapley.csv",
    "16_channel_comparison.csv",
])

regression16 = CSV.read(joinpath(@__DIR__,
                                "16_three_agent_regression_moments.csv"),
                        DataFrame)
cells16 = CSV.read(joinpath(@__DIR__, "16_three_agent_cells.csv"), DataFrame)
path16 = CSV.read(joinpath(@__DIR__, "16_three_agent_path.csv"), DataFrame)
scenarios16 = CSV.read(joinpath(@__DIR__, "16_three_agent_scenarios.csv"),
                       DataFrame)
shapley16 = CSV.read(joinpath(@__DIR__, "16_three_agent_shapley.csv"),
                     DataFrame)
channels16 = CSV.read(joinpath(@__DIR__, "16_channel_comparison.csv"),
                      DataFrame)

function save_both16(fig, stem::String)
    png_path = joinpath(FIGURE_DIR_16, stem * ".png")
    pdf_path = joinpath(FIGURE_DIR_16, stem * ".pdf")
    savefig(fig, png_path)
    savefig(fig, pdf_path)
    println("Saved $(basename(png_path)) and $(basename(pdf_path))")
end

pp16(x) = 100.0 .* x

function shock_span16!(p; label="High Supplement / pandemic period")
    vspan!(p, [11.5, 36.5], color=E61_GREY_16, alpha=0.12,
           linewidth=0, label=label)
    vline!(p, [12.0, 36.0], color=E61_GREY_16, alpha=0.7,
           linestyle=:dot, linewidth=1.2, label="")
    return p
end

function coefficient_panel16(outcome::String, panel::String;
                             show_legend::Bool=false)
    sub = regression16[(regression16.outcome .== outcome) .&
                       (regression16.panel .== panel), :]
    order = Dict("intercept" => 1, "australian" => 2,
                 "post" => 3, "interaction" => 4)
    sub.order = [order[t] for t in sub.term]
    sort!(sub, :order)
    x = collect(1:nrow(sub))
    labels = ["NZ pre", "Australian\npre gap", "NZ Post",
              "Australian ×\nPost"]
    title_outcome = outcome == "finding" ? "Job finding" : "Separation"
    title_panel = panel == "main" ? "recipient comparison" :
                                     "non-recipient placebo"
    p = plot(
        xlim=(0.45, 4.55), xticks=(x, labels),
        ylabel="Percentage points", xlabel="",
        title="$title_outcome: $title_panel",
        legend=show_legend ? :topright : false,
        size=(660, 450), margin=5Plots.mm,
    )
    hline!(p, [0.0], color=E61_GREY_16, linewidth=1.0, label="")
    scatter!(p, x .- 0.09, pp16(sub.data),
             yerror=1.96 .* pp16(sub.standard_error),
             color=E61_TEAL_16, marker=:circle, markersize=6,
             label=show_legend ? "Data (95% CI)" : "")
    scatter!(p, x .+ 0.09, pp16(sub.model),
             color=E61_ORANGE_16, marker=:diamond, markersize=7,
             label=show_legend ? "Model 16" : "")
    return p
end

function make_regression_fit16()
    panels = [
        coefficient_panel16("finding", "main"; show_legend=true),
        coefficient_panel16("finding", "placebo"),
        coefficient_panel16("separation", "main"),
        coefficient_panel16("separation", "placebo"),
    ]
    fig = plot(panels..., layout=(2, 2), size=(1320, 900),
               plot_title="Model 16 fit to main and placebo regressions",
               plot_titlefontsize=15, plot_titlefontcolor=E61_NAVY_16,
               margin=7Plots.mm)
    save_both16(fig, "fig16_01_regression_fit")
    return fig
end

const CELL_SERIES_16 = [
    (panel="main", group="AB", label="Australian recipient",
     colour=E61_ORANGE_16, linestyle=:solid),
    (panel="placebo", group="A0", label="Australian non-recipient",
     colour=E61_TEAL_16, linestyle=:solid),
    (panel="main", group="NZ", label="NZ matched to recipients",
     colour=E61_LIGHT_TEAL_16, linestyle=:dash),
    (panel="placebo", group="NZ", label="NZ matched to non-recipients",
     colour=E61_GREY_16, linestyle=:dash),
]

function cell_panel16(outcome::String; show_legend::Bool=false)
    p = plot(
        xlim=(0.75, 2.25), xticks=([1, 2], ["Pre", "Post"]),
        ylabel="Weekly rate (%)", xlabel="",
        title=outcome == "finding" ? "Job-finding rate" : "Separation rate",
        legend=show_legend ? :outerbottom : false,
        size=(680, 480), margin=5Plots.mm,
    )
    for (series_index, s) in enumerate(CELL_SERIES_16)
        sub = cells16[(cells16.outcome .== outcome) .&
                      (cells16.empirical_panel .== s.panel) .&
                      (cells16.group .== s.group), :]
        period_order = Dict("pre" => 1, "post" => 2)
        sub.order = [period_order[t] for t in sub.period]
        sort!(sub, :order)
        plot!(p, [1, 2], pp16(sub.model), color=s.colour,
              linestyle=s.linestyle, marker=:diamond, markersize=5.5,
              label=show_legend ? s.label : "")
        scatter!(p, [1, 2], pp16(sub.data), marker=:circle,
                 markercolor=:white, markerstrokecolor=s.colour,
                 markerstrokewidth=2.0, markersize=6.5,
                 label=show_legend && series_index == 1 ?
                       "Hollow circles: data" : "")
    end
    return p
end

function make_transition_cells16()
    p1 = cell_panel16("finding"; show_legend=true)
    p2 = cell_panel16("separation")
    fig = plot(p1, p2, layout=(1, 2), size=(1400, 560),
               plot_title="Observed and model-implied transitions",
               plot_titlefontsize=15, plot_titlefontcolor=E61_NAVY_16,
               margin=7Plots.mm)
    save_both16(fig, "fig16_02_transition_cells")
    return fig
end

function weekly_transition_panel16(outcome::Symbol; show_legend::Bool=false)
    if outcome == :finding
        columns = (:find_AB, :find_A0, :find_NZ)
        title = "Job-finding probability"
    else
        columns = (:sep_AB, :sep_A0, :sep_NZ)
        title = "Separation probability"
    end
    labels = ("Australian recipient", "Australian non-recipient", "New Zealand")
    colours = (E61_ORANGE_16, E61_TEAL_16, E61_LIGHT_TEAL_16)
    styles = (:solid, :solid, :dash)
    p = plot(xlabel="Model week", ylabel="Weekly probability (%)",
             title=title, legend=show_legend ? :outerbottom : false,
             xlim=(1, maximum(path16.week)), margin=5Plots.mm,
             bottom_margin=15Plots.mm)
    shock_span16!(p; label=show_legend ? "High Supplement / pandemic" : "")
    for i in 1:3
        plot!(p, path16.week, pp16(path16[!, columns[i]]),
              color=colours[i], linestyle=styles[i], linewidth=2.7,
              label=show_legend ? labels[i] : "")
    end
    return p
end

function make_weekly_transitions16()
    p1 = weekly_transition_panel16(:finding; show_legend=true)
    p2 = weekly_transition_panel16(:separation)
    fig = plot(p1, p2, layout=(1, 2), size=(1400, 620),
               plot_title="Version-16 transition dynamics",
               plot_titlefontsize=15, plot_titlefontcolor=E61_NAVY_16,
               margin=7Plots.mm)
    save_both16(fig, "fig16_03_weekly_transitions")
    return fig
end

function behaviour_panel16(margin::Symbol; show_legend::Bool=false)
    if margin == :search
        columns = (:search_AB, :search_A0, :search_NZ)
        title = "Optimal search"
        ylabel = "Search choice"
    else
        columns = (:accept_AB, :accept_A0, :accept_NZ)
        title = "Offer acceptance"
        ylabel = "Acceptance probability"
    end
    labels = ("Australian recipient", "Australian non-recipient", "New Zealand")
    colours = (E61_ORANGE_16, E61_TEAL_16, E61_LIGHT_TEAL_16)
    styles = (:solid, :solid, :dash)
    p = plot(xlabel="Model week", ylabel=ylabel, title=title,
             legend=show_legend ? :outertop : false,
             xlim=(1, maximum(path16.week)), margin=5Plots.mm)
    shock_span16!(p; label=show_legend ? "High Supplement / pandemic" : "")
    for i in 1:3
        plot!(p, path16.week, path16[!, columns[i]], color=colours[i],
              linestyle=styles[i], linewidth=2.7,
              label=show_legend ? labels[i] : "")
    end
    return p
end

function make_behavioural_margins16()
    p1 = behaviour_panel16(:search; show_legend=true)
    p2 = behaviour_panel16(:acceptance)
    fig = plot(p1, p2, layout=(1, 2), size=(1400, 620),
               plot_title="Behavioural margins in the combined model",
               plot_titlefontsize=15, plot_titlefontcolor=E61_NAVY_16,
               margin=7Plots.mm)
    save_both16(fig, "fig16_04_behavioural_margins")
    return fig
end

function shapley_panel16(outcome::String)
    sub = shapley16[shapley16.outcome .== outcome, :]
    order = Dict("benefit" => 1, "common_shocks" => 2,
                 "nz_differential" => 3)
    sub.order = [order[f] for f in sub.factor]
    sort!(sub, :order, rev=true)
    labels = replace.(sub.factor,
                      "benefit" => "Benefit",
                      "common_shocks" => "Common shocks",
                      "nz_differential" => "NZ differential")
    colours = [f == "benefit" ? E61_ORANGE_16 :
               f == "common_shocks" ? E61_TEAL_16 : E61_LIGHT_TEAL_16
               for f in sub.factor]
    vals = sub.shapley_contribution_pp
    title = outcome == "finding" ? "Job-finding DiD" : "Separation DiD"
    y = collect(1:length(labels))
    p = bar(y, vals, orientation=:h, yticks=(y, labels),
            color=colours, linecolor=:white,
            legend=false, xlabel="Contribution (percentage points)",
            title=title, margin=6Plots.mm)
    vline!(p, [0.0], color=E61_NAVY_16, linewidth=1.0, label="")
    for (i, val) in enumerate(vals)
        scale = max(maximum(abs.(vals)), 0.5)
        if abs(val) < 0.03
            xpos, align, text_colour = 0.04 * scale, :left, E61_NAVY_16
        elseif val < -0.50
            xpos, align, text_colour = val + 0.05 * scale, :left, :white
        elseif val < 0.0
            xpos, align, text_colour = val - 0.025 * scale, :right, E61_NAVY_16
        else
            xpos, align, text_colour = val + 0.025 * scale, :left, E61_NAVY_16
        end
        annotate!(p, xpos, y[i], text(@sprintf("%+.2f", val), 9,
                                      text_colour, align))
    end
    return p
end

function make_shapley16()
    p1 = shapley_panel16("finding")
    p2 = shapley_panel16("separation")
    fig = plot(p1, p2, layout=(1, 2), size=(1400, 520),
               plot_title="Shapley decomposition of the main recipient DiD",
               plot_titlefontsize=15, plot_titlefontcolor=E61_NAVY_16,
               margin=7Plots.mm)
    save_both16(fig, "fig16_05_shapley_decomposition")
    return fig
end

function recipient_level_shapley16()
    permutations = ((1,2,3), (1,3,2), (2,1,3),
                    (2,3,1), (3,1,2), (3,2,1))
    factor_names = ("benefit", "common_shocks", "nz_differential")
    rows = NamedTuple[]

    function scenario_value(state::NTuple{3,Bool}, outcome::String)
        sub = scenarios16[(scenarios16.benefit_on .== state[1]) .&
                          (scenarios16.common_shocks_on .== state[2]) .&
                          (scenarios16.nz_differential_on .== state[3]), :]
        nrow(sub) == 1 || error("Expected one factorial scenario")
        return 100.0 * (outcome == "finding" ? sub.find_AB[1] :
                                                  sub.sep_AB[1])
    end

    function data_change(outcome::String)
        sub = cells16[(cells16.outcome .== outcome) .&
                      (cells16.empirical_panel .== "main") .&
                      (cells16.group .== "AB"), :]
        pre = sub.data[sub.period .== "pre"][1]
        post = sub.data[sub.period .== "post"][1]
        return 100.0 * (post - pre)
    end

    for outcome in ("finding", "separation")
        contributions = zeros(3)
        for perm in permutations
            state = (false, false, false)
            old_value = scenario_value(state, outcome)
            for factor in perm
                next_state = collect(state)
                next_state[factor] = true
                new_state = Tuple(next_state)
                new_value = scenario_value(new_state, outcome)
                contributions[factor] += new_value - old_value
                state = new_state
                old_value = new_value
            end
        end
        contributions ./= length(permutations)
        model_total = scenario_value((true,true,true), outcome) -
                      scenario_value((false,false,false), outcome)
        observed_total = data_change(outcome)
        for i in 1:3
            push!(rows, (outcome=outcome, factor=factor_names[i],
                         contribution_pp=contributions[i],
                         model_total_change_pp=model_total,
                         data_total_change_pp=observed_total))
        end
    end
    return DataFrame(rows)
end

function value_label16!(p, value::Float64, y::Int, scale::Float64;
                        force_inside::Bool=false)
    if force_inside || abs(value) > 0.45 * scale
        xpos = value < 0.0 ? value + 0.04 * scale : value - 0.04 * scale
        align = value < 0.0 ? :left : :right
        colour = :white
    elseif abs(value) < 0.02
        xpos, align, colour = 0.035 * scale, :left, E61_NAVY_16
    else
        xpos = value < 0.0 ? value - 0.025 * scale : value + 0.025 * scale
        align = value < 0.0 ? :right : :left
        colour = E61_NAVY_16
    end
    annotate!(p, xpos, y,
              text(@sprintf("%+.2f", value), 9, colour, align))
end

function level_decomposition_panel16(level_values::DataFrame,
                                     outcome::String)
    sub = level_values[level_values.outcome .== outcome, :]
    factor_order = Dict("benefit" => 1, "common_shocks" => 2,
                        "nz_differential" => 3)
    sub.order = [factor_order[f] for f in sub.factor]
    sort!(sub, :order)
    vals = vcat(sub.contribution_pp,
                sub.model_total_change_pp[1], sub.data_total_change_pp[1])
    labels = ["Benefit", "Common shocks", "NZ differential",
              "Model total", "Data total"]
    colours = [E61_ORANGE_16, E61_TEAL_16, E61_LIGHT_TEAL_16,
               E61_NAVY_16, E61_GREY_16]
    y = reverse(collect(1:length(labels)))
    p = bar(y, vals, orientation=:h, yticks=(y, labels),
            color=colours, linecolor=:white, legend=false,
            xlabel="Change in recipient level (percentage points)",
            title=outcome == "finding" ? "Job-finding level" :
                                         "Separation level",
            left_margin=8Plots.mm, right_margin=8Plots.mm)
    vline!(p, [0.0], color=E61_NAVY_16, linewidth=1.0, label="")
    scale = maximum(abs.(vals))
    for i in eachindex(vals)
        value_label16!(p, vals[i], y[i], scale;
                       force_inside=i >= 4)
    end
    return p
end

function make_recipient_level_decomposition16(level_values::DataFrame)
    p1 = level_decomposition_panel16(level_values, "finding")
    p2 = level_decomposition_panel16(level_values, "separation")
    fig = plot(p1, p2, layout=(1, 2), size=(1450, 570),
               plot_title="Contributions to the Australian recipient's own pre–post change",
               plot_titlefontsize=15, plot_titlefontcolor=E61_NAVY_16,
               margin=7Plots.mm)
    save_both16(fig, "fig16_08_recipient_level_decomposition")
    return fig
end

function level_vs_did_panel16(level_values::DataFrame, outcome::String)
    factors = ["benefit", "common_shocks", "nz_differential"]
    labels = ["Benefit", "Common shocks", "NZ differential"]
    level = Float64[]
    did = Float64[]
    for factor in factors
        lrow = level_values[(level_values.outcome .== outcome) .&
                            (level_values.factor .== factor), :]
        drow = shapley16[(shapley16.outcome .== outcome) .&
                         (shapley16.factor .== factor), :]
        push!(level, lrow.contribution_pp[1])
        push!(did, drow.shapley_contribution_pp[1])
    end
    x = collect(1:3)
    p = bar(x .- 0.18, level, bar_width=0.34,
            color=E61_LIGHT_TEAL_16, linecolor=:white,
            label="Recipient pre–post level",
            xticks=(x, labels), ylabel="Contribution (percentage points)",
            title=outcome == "finding" ? "Job finding" : "Separation",
            legend=:outerbottom, bottom_margin=14Plots.mm)
    bar!(p, x .+ 0.18, did, bar_width=0.34,
         color=E61_TEAL_16, linecolor=:white,
         label="Recipient-minus-NZ DiD")
    hline!(p, [0.0], color=E61_NAVY_16, linewidth=0.9, label="")
    return p
end

function make_level_vs_did16(level_values::DataFrame)
    p1 = level_vs_did_panel16(level_values, "finding")
    p2 = level_vs_did_panel16(level_values, "separation")
    fig = plot(p1, p2, layout=(1, 2), size=(1450, 590),
               plot_title="Why a common shock can have the opposite sign in the DiD",
               plot_titlefontsize=15, plot_titlefontcolor=E61_NAVY_16,
               margin=7Plots.mm)
    save_both16(fig, "fig16_09_level_vs_did_contributions")
    return fig
end

function channel_panel16(outcome::Symbol)
    order = Dict("combined" => 1, "search_only" => 2,
                 "acceptance_only" => 3)
    sub = copy(channels16)
    sub.order = [order[c] for c in sub.channel]
    sort!(sub, :order)
    labels = ["Combined\nloss=$(round(r.weighted_loss, digits=2))" for r in eachrow(sub)]
    labels[2] = "Search only\nloss=$(round(sub.weighted_loss[2], digits=2))"
    labels[3] = "Acceptance only\nloss=$(round(sub.weighted_loss[3], digits=2))"
    x = collect(1:3)
    if outcome == :finding
        full = sub.full_main_find_did_pp
        benefit = sub.benefit_only_main_find_did_pp
        target = sub.data_main_find_did_pp[1]
        title = "Job-finding DiD"
    else
        full = sub.full_main_sep_did_pp
        benefit = sub.benefit_only_main_sep_did_pp
        target = sub.data_main_sep_did_pp[1]
        title = "Separation DiD"
    end
    p = bar(x .- 0.18, benefit, bar_width=0.34, color=E61_LIGHT_TEAL_16,
            linecolor=:white, label="Benefit-only scenario",
            xticks=(x, labels), ylabel="Percentage points", title=title,
            legend=:outerbottom, margin=6Plots.mm)
    bar!(p, x .+ 0.18, full, bar_width=0.34, color=E61_TEAL_16,
         linecolor=:white, label="Full model")
    hline!(p, [target], color=E61_ORANGE_16, linestyle=:dash,
           linewidth=2.5, label="Data")
    hline!(p, [0.0], color=E61_NAVY_16, linewidth=0.8, label="")
    return p
end

function make_channel_comparison16()
    p1 = channel_panel16(:finding)
    p2 = channel_panel16(:separation)
    fig = plot(p1, p2, layout=(1, 2), size=(1400, 570),
               plot_title="Search and acceptance mechanism comparison",
               plot_titlefontsize=15, plot_titlefontcolor=E61_NAVY_16,
               margin=7Plots.mm)
    save_both16(fig, "fig16_06_channel_comparison")
    return fig
end

function scenario_label16(row)
    factors = String[]
    row.benefit_on && push!(factors, "Benefit")
    row.common_shocks_on && push!(factors, "Common")
    row.nz_differential_on && push!(factors, "NZ diff.")
    return isempty(factors) ? "None" : join(factors, " + ")
end

function scenario_panel16(outcome::Symbol)
    sub = copy(scenarios16)
    sub.order = 4 .* Int.(sub.benefit_on) .+
                2 .* Int.(sub.common_shocks_on) .+
                Int.(sub.nz_differential_on)
    sort!(sub, :order)
    labels = scenario_label16.(eachrow(sub))
    x = collect(1:nrow(sub))
    if outcome == :finding
        vals = sub.main_find_did_pp
        target = -1.71
        title = "Job-finding DiD"
    else
        vals = sub.main_sep_did_pp
        target = 3.72
        title = "Separation DiD"
    end
    colours = [r.benefit_on ? E61_ORANGE_16 :
               r.common_shocks_on ? E61_TEAL_16 : E61_LIGHT_TEAL_16
               for r in eachrow(sub)]
    p = bar(x, vals, color=colours, linecolor=:white, legend=false,
            xticks=(x, labels), xrotation=35,
            ylabel="Percentage points", title=title,
            bottom_margin=30Plots.mm, left_margin=6Plots.mm)
    hline!(p, [target], color=E61_RED_16, linestyle=:dash,
           linewidth=2.2, label="")
    hline!(p, [0.0], color=E61_NAVY_16, linewidth=0.8, label="")
    return p
end

function make_factorial_scenarios16()
    p1 = scenario_panel16(:finding)
    p2 = scenario_panel16(:separation)
    fig = plot(p1, p2, layout=(1, 2), size=(1600, 740),
               plot_title="Main DiD under all combinations of model shocks",
               plot_titlefontsize=15, plot_titlefontcolor=E61_NAVY_16,
               margin=7Plots.mm)
    save_both16(fig, "fig16_07_factorial_scenarios")
    return fig
end

function write_figure_index16()
    path = joinpath(FIGURE_DIR_16, "figure_index.txt")
    open(path, "w") do io
        println(io, "Version-16 model figures")
        println(io, "========================")
        println(io, "fig16_01_regression_fit: model versus all 16 regression coefficients; data show 95% CIs.")
        println(io, "fig16_02_transition_cells: pre/post selected-sample rates; hollow circles are data and diamonds/lines are model.")
        println(io, "fig16_03_weekly_transitions: structural weekly job finding and separation for AB, A0 and one NZ agent.")
        println(io, "fig16_04_behavioural_margins: optimal search and offer acceptance in the combined specification.")
        println(io, "fig16_05_shapley_decomposition: order-invariant contributions to the main recipient DiD.")
        println(io, "fig16_06_channel_comparison: recalibrated combined, search-only and acceptance-only specifications.")
        println(io, "fig16_07_factorial_scenarios: all eight benefit/common/NZ-differential combinations; red dashed line is the data DiD.")
        println(io, "fig16_08_recipient_level_decomposition: benefit and common-shock contributions to the recipient's own pre-post level change, with model and data totals.")
        println(io, "fig16_09_level_vs_did_contributions: direct comparison of contributions to recipient levels and to the recipient-minus-NZ DiD.")
        println(io, "")
        println(io, "All rates and DiDs are for the selected empirical population, not the whole economy.")
        println(io, "The two NZ empirical panels are measurements of one structural NZ agent.")
    end
    println("Saved figure_index.txt")
end

function run_plots16()
    level_values = recipient_level_shapley16()
    CSV.write(joinpath(FIGURE_DIR_16, "level_decomposition_values.csv"),
              level_values)
    figures = [
        make_regression_fit16(),
        make_transition_cells16(),
        make_weekly_transitions16(),
        make_behavioural_margins16(),
        make_shapley16(),
        make_channel_comparison16(),
        make_factorial_scenarios16(),
        make_recipient_level_decomposition16(level_values),
        make_level_vs_did16(level_values),
    ]
    write_figure_index16()
    println("\nCreated $(length(figures)) model-16 figures in:")
    println("  $FIGURE_DIR_16")
    return figures
end

if abspath(PROGRAM_FILE) == @__FILE__
    run_plots16()
end

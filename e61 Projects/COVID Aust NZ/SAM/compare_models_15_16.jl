# compare_models_15_16.jl
#
# Runs model 15 and model 16 in separate Julia processes (avoiding name clashes)
# and creates aligned moment and parameter comparison tables.

using CSV
using DataFrames
using Printf

function run_script(script::String)
    isfile(script) || error("Missing model script: $script")
    cmd = `$(Base.julia_cmd()) --startup-file=no $script`
    println("Running $script")
    run(cmd)
end

function get15(summary::DataFrame, moment::String)
    rows = summary[summary.moment .== moment, :]
    nrow(rows) == 1 || return missing
    return parse(Float64, string(rows.model[1]))
end

function get15data(summary::DataFrame, moment::String)
    rows = summary[summary.moment .== moment, :]
    nrow(rows) == 1 || return missing
    return parse(Float64, string(rows.data[1]))
end

function get16cell(cells::DataFrame, outcome::String, panel::String,
                   group::String, period::String, column::Symbol)
    rows = cells[(cells.outcome .== outcome) .&
                 (cells.empirical_panel .== panel) .&
                 (cells.group .== group) .&
                 (cells.period .== period), :]
    nrow(rows) == 1 || return missing
    return parse(Float64, string(rows[1, column]))
end

function get16param(params::DataFrame, parameter::String)
    rows = params[params.parameter .== parameter, :]
    nrow(rows) == 1 || return missing
    return parse(Float64, string(rows.value[1]))
end

function build_moment_comparison(summary15::DataFrame, cells16::DataFrame)
    rows = NamedTuple[]
    mapping = [
        (outcome="finding", panel="main", group="NZ", period="pre",
         m15="f_N_pre"),
        (outcome="finding", panel="main", group="AB", period="pre",
         m15="f_R_pre"),
        (outcome="finding", panel="main", group="NZ", period="post",
         m15="f_N_post"),
        (outcome="finding", panel="main", group="AB", period="post",
         m15="f_R_post"),
        (outcome="separation", panel="main", group="NZ", period="pre",
         m15="sep_N_pre"),
        (outcome="separation", panel="main", group="AB", period="pre",
         m15="sep_R_pre"),
        (outcome="separation", panel="main", group="NZ", period="post",
         m15="sep_N_post"),
        (outcome="separation", panel="main", group="AB", period="post",
         m15="sep_R_post"),
    ]
    for m in mapping
        data = get16cell(cells16, m.outcome, m.panel, m.group,
                         m.period, :data)
        model16 = get16cell(cells16, m.outcome, m.panel, m.group,
                            m.period, :model)
        push!(rows, (
            outcome=m.outcome, empirical_panel=m.panel, group=m.group,
            period=m.period, data=data,
            model15=get15(summary15, m.m15), model16=model16,
            model15_gap=get15(summary15, m.m15)-data,
            model16_gap=model16-data,
            note=m.group == "NZ" ?
                 "Version 16 has one NZ agent plus a main-panel measurement offset" :
                 "AB is the version-16 recipient agent",
        ))
    end

    # Placebo-panel cells are new in version 16 and have no model-15 analogue.
    for outcome in ("finding", "separation"), group in ("A0", "NZ"),
        period in ("pre", "post")
        data = get16cell(cells16, outcome, "placebo", group, period, :data)
        model16 = get16cell(cells16, outcome, "placebo", group, period, :model)
        push!(rows, (
            outcome=outcome, empirical_panel="placebo", group=group,
            period=period, data=data, model15=missing, model16=model16,
            model15_gap=missing, model16_gap=model16-data,
            note="New version-16 placebo discipline; not present in model 15",
        ))
    end
    return DataFrame(rows)
end

function build_parameter_comparison(summary15::DataFrame,
                                    params16::DataFrame)
    return DataFrame([
        (concept="weekly discount factor", model15=0.99,
         model16=get16param(params16, "beta_weekly"),
         note="Model 15 script default is 0.99; model 16 converts a 0.96 annual factor"),
        (concept="pre matching shifter", model15=get15(summary15, "mu_pre"),
         model16=get16param(params16, "mu_pre"),
         note="Not directly comparable because beta and three-agent calibration change"),
        (concept="common search cost", model15=get15(summary15, "kappa_pre"),
         model16=get16param(params16, "kappa"),
         note="Scale depends on weekly discounting and channel specification"),
        (concept="post matching shifter", model15=get15(summary15, "mu_post"),
         model16=get16param(params16, "mu_post"),
         note="Version 16 is disciplined by untreated Australian and both NZ panels"),
        (concept="common separation shock",
         model15=get15(summary15, "common_sep_shock"),
         model16=get16param(params16, "common_sep_shock"),
         note="Version 16 uses a competing-risks form"),
        (concept="differential nonpecuniary work cost",
         model15=get15(summary15, "health_R"),
         model16=get16param(params16, "nz_work_cost_diff"),
         note="Signs/references differ: model 15 fits a recipient wedge; version 16 estimates an NZ differential from placebo DiDs"),
        (concept="separation base", model15=get15(summary15, "sep_base"),
         model16=get16param(params16, "sep_base"),
         note="Related but version 16 combines risks multiplicatively"),
        (concept="separation amplitude", model15=get15(summary15, "sep_amp"),
         model16=get16param(params16, "sep_amp"),
         note="Benefit-sensitive continuation mechanism"),
    ])
end

function run_comparison()
    run_script("15_structural_separation_model.jl")
    run_script("16_three_agent_toy_model.jl")

    summary15 = CSV.read("15_structural_separation_summary.csv", DataFrame)
    cells16 = CSV.read("16_three_agent_cells.csv", DataFrame)
    params16 = CSV.read("16_three_agent_parameters.csv", DataFrame)

    moments = build_moment_comparison(summary15, cells16)
    parameters = build_parameter_comparison(summary15, params16)
    CSV.write("15_16_moment_comparison.csv", moments)
    CSV.write("15_16_parameter_comparison.csv", parameters)

    println("\nModel 15 versus model 16 main-panel moments")
    println("--------------------------------------------")
    for row in eachrow(moments[moments.empirical_panel .== "main", :])
        @printf("%-10s %-3s %-4s data=% .4f m15=% .4f m16=% .4f\n",
                row.outcome, row.group, row.period, row.data,
                row.model15, row.model16)
    end
    println("\nSaved:")
    println("  15_16_moment_comparison.csv")
    println("  15_16_parameter_comparison.csv")
    return moments, parameters
end

if abspath(PROGRAM_FILE) == @__FILE__
    run_comparison()
end

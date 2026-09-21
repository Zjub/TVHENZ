module AustralianData

using CSV
using DataFrames
using Dates
using Downloads
using LinearAlgebra
using SHA
using Statistics
using TOML

export obtain_raw_data, prepare_quarterly_data, estimate_macro_block


# Official RBA statistical tables.  The exact series IDs selected from each
# table are listed separately below, so the data choices are easy to audit.
const RBA_URLS = Dict(
    "g1" => "https://www.rba.gov.au/statistics/tables/csv/g1-data.csv",
    "h1" => "https://www.rba.gov.au/statistics/tables/csv/h1-data.csv",
    "h2" => "https://www.rba.gov.au/statistics/tables/csv/h2-data.csv",
    "f1_1" => "https://www.rba.gov.au/statistics/tables/csv/f1.1-data.csv",
)

const SERIES = Dict(
    "g1" => Dict(
        "trimmed_mean_qoq" => "GCPIOCPMTMQP",
        "trimmed_mean_yoy" => "GCPIOCPMTMYP",
    ),
    "h1" => Dict("real_gdp" => "GGDPCVGDP"),
    "h2" => Dict("public_demand" => "GGDPECCVPD"),
    "f1_1" => Dict("cash_rate" => "FIRMMCRT"),
)


"""Download the RBA tables when missing (or when `refresh=true`)."""
function obtain_raw_data(raw_directory::AbstractString; refresh::Bool = false)
    mkpath(raw_directory)
    manifest = Dict{String, Any}()

    for table in sort(collect(keys(RBA_URLS)))
        url = RBA_URLS[table]
        path = joinpath(raw_directory, "rba_$(table).csv")
        if refresh || !isfile(path)
            Downloads.download(url, path)
        end
        payload = read(path)
        manifest[table] = Dict(
            "url" => url,
            "file" => basename(path),
            "sha256" => bytes2hex(sha256(payload)),
            "bytes" => length(payload),
        )
    end
    manifest["manifest_updated_utc"] = string(now(UTC))
    open(joinpath(raw_directory, "manifest.toml"), "w") do io
        TOML.print(io, manifest)
    end
    return manifest
end


"""
Read selected series from one RBA CSV table.

RBA files contain human-readable metadata above the observations.  The parser
finds columns from the `Series ID` row rather than relying on display titles,
which may change punctuation or wording over time.
"""
function read_rba_table(path::AbstractString, wanted::Dict{String, String})
    raw = CSV.read(
        path,
        DataFrame;
        header = 2,
        normalizenames = false,
        silencewarnings = true,
    )
    first_column = names(raw)[1]
    series_row = findfirst(
        value -> !ismissing(value) && string(value) == "Series ID",
        raw[!, first_column],
    )
    isnothing(series_row) && error("No Series ID metadata row found in $path")

    selected_columns = Dict{String, Symbol}()
    for (output_name, series_id) in wanted
        matches = findall(
            column -> begin
                value = raw[series_row, column]
                !ismissing(value) && string(value) == series_id
            end,
            names(raw),
        )
        length(matches) == 1 || error("Series $series_id missing or duplicated in $path")
        selected_columns[output_name] = Symbol(names(raw)[only(matches)])
    end

    dates = Date[]
    values = Dict(name => Float64[] for name in keys(wanted))
    for row in (series_row + 1):nrow(raw)
        parsed_date = tryparse(Date, string(raw[row, first_column]), dateformat"d/m/y")
        isnothing(parsed_date) && continue

        row_values = Dict{String, Float64}()
        complete_row = true
        for (output_name, column) in selected_columns
            value = raw[row, column]
            if ismissing(value)
                complete_row = false
                break
            end
            parsed_value = tryparse(Float64, string(value))
            if isnothing(parsed_value)
                complete_row = false
                break
            end
            row_values[output_name] = parsed_value
        end
        complete_row || continue

        push!(dates, parsed_date)
        for output_name in keys(wanted)
            push!(values[output_name], row_values[output_name])
        end
    end

    output = DataFrame(date = dates)
    for output_name in sort(collect(keys(wanted)))
        output[!, Symbol(output_name)] = values[output_name]
    end
    sort!(output, :date)
    return output
end


"""Return the quarter-end date containing `date`."""
function quarter_end(date::Date)
    final_month = 3 * cld(month(date), 3)
    return lastdayofmonth(Date(year(date), final_month, 1))
end


"""
Two-sided Hodrick-Prescott trend.

For observations y, the trend solves

    min_tau sum((y - tau)^2) + lambda * sum((Delta^2 tau)^2).

The implementation is included directly so the transformation is visible and
does not require a separate filtering package.
"""
function hp_trend(series::AbstractVector; lambda::Float64 = 1600.0)
    y = Float64.(series)
    n = length(y)
    n >= 3 || error("HP filter requires at least three observations")

    second_difference = zeros(n - 2, n)
    for row in 1:(n - 2)
        second_difference[row, row] = 1.0
        second_difference[row, row + 1] = -2.0
        second_difference[row, row + 2] = 1.0
    end
    return (
        Matrix{Float64}(I, n, n) +
        lambda .* (second_difference' * second_difference)
    ) \ y
end


"""Construct the quarterly Australian dataset used by the model."""
function prepare_quarterly_data(raw_directory::AbstractString)
    inflation = read_rba_table(joinpath(raw_directory, "rba_g1.csv"), SERIES["g1"])
    output = read_rba_table(joinpath(raw_directory, "rba_h1.csv"), SERIES["h1"])
    fiscal = read_rba_table(joinpath(raw_directory, "rba_h2.csv"), SERIES["h2"])
    monthly_rates = read_rba_table(
        joinpath(raw_directory, "rba_f1_1.csv"),
        SERIES["f1_1"],
    )

    # The model is quarterly, so convert the monthly cash-rate target to a
    # quarterly mean before joining it to the other series.
    monthly_rates.quarter = quarter_end.(monthly_rates.date)
    quarterly_rates = combine(
        groupby(monthly_rates, :quarter),
        :cash_rate => mean => :cash_rate,
    )
    rename!(quarterly_rates, :quarter => :date)

    data = innerjoin(output, fiscal, inflation, quarterly_rates; on = :date)
    sort!(data, :date)

    log_gdp = log.(data.real_gdp)
    log_public_demand = log.(data.public_demand)
    data.output_gap = 100.0 .* (log_gdp .- hp_trend(log_gdp))
    data.public_demand_gap = 100.0 .* (
        log_public_demand .- hp_trend(log_public_demand)
    )
    data.inflation_gap = 4.0 .* data.trimmed_mean_qoq .- 2.5

    estimation_rows = (
        (data.date .>= Date(1993, 3, 31)) .&
        (data.date .<= Date(2019, 12, 31))
    )
    neutral_real_rate = mean(
        data.cash_rate[estimation_rows] .-
        data.trimmed_mean_yoy[estimation_rows],
    )
    data.real_rate_gap = (
        data.cash_rate .- data.trimmed_mean_yoy .- neutral_real_rate
    )
    return data, neutral_real_rate
end


lag(series::AbstractVector, periods::Int) =
    vcat(fill(missing, periods), series[1:(end - periods)])


"""Ordinary least squares with explicit design-matrix construction."""
function ols(
    frame::DataFrame,
    dependent::Symbol,
    regressors::Vector{Symbol},
)
    columns = vcat([dependent], regressors)
    clean = dropmissing(frame[:, columns])
    X = hcat(ones(nrow(clean)), [Float64.(clean[!, column]) for column in regressors]...)
    y = Float64.(clean[!, dependent])
    coefficients = X \ y
    residuals = y - X * coefficients
    degrees_of_freedom = length(y) - size(X, 2)
    residual_sd = sqrt(sum(abs2, residuals) / degrees_of_freedom)
    rmse = sqrt(mean(residuals .^ 2))
    return coefficients, residuals, residual_sd, rmse, length(y)
end


"""
Estimate the compact Australian IS and Phillips equations over 1993Q1–2019Q4.

These are descriptive reduced-form estimates, not causal policy multipliers.
After estimation, sign checks prevent the strategic model from silently using
an economically inverted transmission mechanism.
"""
function estimate_macro_block(data::DataFrame)
    rows = (
        (data.date .>= Date(1993, 3, 31)) .&
        (data.date .<= Date(2019, 12, 31))
    )
    sample = copy(data[rows, :])
    sample.output_gap_lag1 = lag(sample.output_gap, 1)
    sample.inflation_gap_lag1 = lag(sample.inflation_gap, 1)
    sample.output_gap_lag2 = lag(sample.output_gap, 2)

    is_coefficients, is_residuals, demand_sd, is_rmse, is_n = ols(
        sample,
        :output_gap,
        [:output_gap_lag1, :real_rate_gap, :public_demand_gap],
    )
    pc_coefficients, pc_residuals, supply_sd, pc_rmse, pc_n = ols(
        sample,
        :inflation_gap,
        [:inflation_gap_lag1, :output_gap_lag2],
    )

    # The data currently deliver these signs without binding constraints.  If a
    # future data vintage does not, the example stops and asks the researcher to
    # reconsider the specification rather than clipping an estimate silently.
    0.0 <= is_coefficients[2] < 1.0 || error("IS persistence is outside [0,1)")
    is_coefficients[3] < 0.0 || error("Estimated real-rate effect is not negative")
    is_coefficients[4] > 0.0 || error("Estimated public-demand effect is not positive")
    0.0 <= pc_coefficients[2] < 1.0 || error("Inflation persistence is outside [0,1)")
    pc_coefficients[3] > 0.0 || error("Estimated Phillips slope is not positive")

    estimates = Dict{String, Any}(
        "rho_x" => is_coefficients[2],
        "sigma_i" => -is_coefficients[3],
        "chi_g" => is_coefficients[4],
        "rho_pi" => pc_coefficients[2],
        "kappa" => pc_coefficients[3],
        "demand_shock_sd" => demand_sd,
        "supply_shock_sd" => supply_sd,
        "is_intercept" => is_coefficients[1],
        "pc_intercept" => pc_coefficients[1],
        "is_rmse" => is_rmse,
        "pc_rmse" => pc_rmse,
        "sample_start" => string(minimum(sample.date)),
        "sample_end" => string(maximum(sample.date)),
        "available_quarters" => nrow(sample),
        "is_observations" => is_n,
        "pc_observations" => pc_n,
    )

    estimate_table = DataFrame(
        equation = ["IS", "IS", "IS", "IS", "Phillips", "Phillips", "Phillips"],
        term = [
            "intercept",
            "lagged output gap",
            "real rate gap",
            "public demand gap",
            "intercept",
            "lagged inflation gap",
            "output gap, lag 2",
        ],
        estimate = vcat(is_coefficients, pc_coefficients),
        method = fill("ordinary least squares; post-estimation sign check", 7),
    )
    return estimates, estimate_table
end

end # module AustralianData

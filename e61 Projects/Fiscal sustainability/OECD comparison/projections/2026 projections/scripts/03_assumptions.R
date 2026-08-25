source(file.path("scripts", "00_config.R"))

historical <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
population <- fread(file.path(processed_dir, "population_projection_age_shares.csv"))
scaling <- fread(file.path(processed_dir, "standardisation_parameters.csv"))
official <- fread(file.path(processed_dir, "official_pbo_nfo_wide.csv"))

years <- projection_start:projection_end
drivers <- population[year %in% years]

last_hist <- historical[year == max(year)]
transition <- function(start, end, start_year, end_year, years) {
  approx(c(start_year, end_year), c(start, end), xout = years, rule = 2)$y
}

drivers[, unemployment := approx(
  x = c(2025, 2026, 2027, 2028, 2032, projection_end),
  y = c(last_hist$unemployment, 4.4, 4.6, 4.7, 4.5, 4.5),
  xout = year, rule = 2
)$y]
drivers[, tot_z := transition(last_hist$tot_z, -0.4, 2025, 2032, year)]
drivers[, rp_z := transition(last_hist$rp_z, 0, 2025, 2032, year)]
drivers[, covid := 0L]
drivers[, inflation := approx(
  c(2025, 2026, 2027, 2028, 2030, projection_end),
  c(0.028, 0.035, 0.030, 0.0275, 0.025, 0.025),
  xout = year, rule = 2
)$y]

macro_scenarios <- rbindlist(list(
  copy(drivers)[, `:=`(scenario = "central", nominal_gdp_growth_long_run = 0.044, effective_interest_rate_long_run = 0.044)],
  copy(drivers)[, `:=`(
    scenario = "pressure",
    unemployment = pmin(unemployment + transition(0, 0.7, 2026, 2032, year), 6),
    inflation = inflation + 0.0025,
    nominal_gdp_growth_long_run = 0.037,
    effective_interest_rate_long_run = 0.052
  )],
  copy(drivers)[, `:=`(
    scenario = "restraint",
    unemployment = pmax(unemployment - 0.2, 3.5),
    nominal_gdp_growth_long_run = 0.050,
    effective_interest_rate_long_run = 0.038
  )]
), use.names = TRUE, fill = TRUE)

# Construct a coherent real-GDP-per-capita path for the new top-down driver.
# The PBO nominal-GDP denominator is used through the official forecast period;
# thereafter nominal GDP grows at the scenario assumption. Inflation converts
# nominal growth to real growth, and the published population projection gives
# the per-capita transformation. These columns are named specifically for the
# top-down equations so they do not collide with the bottom-up nominal-GDP path.
add_scale_income_paths <- function(dt) {
  dt <- copy(dt)
  setorder(dt, year)
  dt <- merge(
    dt,
    official[, .(year, pbo_nominal_gdp = nominal_gdp_billion * 1000)],
    by = "year", all.x = TRUE
  )
  setorder(dt, year)
  dt[, topdown_nominal_gdp := pbo_nominal_gdp]

  previous_nominal <- official[year == 2025, nominal_gdp_billion * 1000]
  previous_real <- historical[year == 2025, gdp_real]
  if (!length(previous_nominal) || !is.finite(previous_nominal)) {
    previous_nominal <- historical[year == 2025, gdp_nom]
  }
  if (!length(previous_real) || !is.finite(previous_real)) {
    stop("A 2024-25 real GDP observation is required to project real GDP per capita.")
  }

  dt[, `:=`(topdown_real_gdp = NA_real_, real_gdp_growth = NA_real_)]
  for (i in seq_len(nrow(dt))) {
    if (!is.finite(dt$topdown_nominal_gdp[i])) {
      dt$topdown_nominal_gdp[i] <- previous_nominal *
        (1 + dt$nominal_gdp_growth_long_run[i])
    }
    nominal_growth <- dt$topdown_nominal_gdp[i] / previous_nominal - 1
    real_growth <- (1 + nominal_growth) / (1 + dt$inflation[i]) - 1
    dt$real_gdp_growth[i] <- real_growth
    dt$topdown_real_gdp[i] <- previous_real * (1 + real_growth)
    previous_nominal <- dt$topdown_nominal_gdp[i]
    previous_real <- dt$topdown_real_gdp[i]
  }

  dt[, `:=`(
    log_population = log(pop_total),
    log_real_gdp_per_capita = log(topdown_real_gdp / pop_total),
    pbo_nominal_gdp = NULL
  )]
  dt
}

macro_scenarios <- rbindlist(lapply(
  split(macro_scenarios, by = "scenario", keep.by = TRUE),
  add_scale_income_paths
), use.names = TRUE, fill = TRUE)

category_assumptions <- data.table(
  category = c(
    "General public services excl interest", "Defence", "Public order and safety",
    "Economic affairs", "Environmental Protection", "Housing and community amenities",
    "Health", "Recreation, culture and religion", "Education", "Social Protection", "Transport"
  ),
  exposure = c(
    "total_population", "gdp_target", "total_population", "total_population",
    "total_population", "total_population", "health_age_weight", "total_population",
    "education_age_weight", "social_age_weight", "total_population"
  ),
  central_excess_cost = c(0, NA, 0.0025, 0, 0, 0, 0.0075, 0, 0.0025, 0.0025, 0),
  pressure_excess_cost = c(0.0025, NA, 0.005, 0.0025, 0.0025, 0.0025, 0.0125, 0.0025, 0.005, 0.0075, 0.0025),
  restraint_excess_cost = c(-0.0025, NA, 0, -0.0025, -0.0025, -0.0025, 0.0025, -0.0025, 0, 0, -0.0025)
)

scenario_parameters <- data.table(
  scenario = c("central", "pressure", "restraint"),
  defence_target_gdp = c(0.025, 0.030, 0.022),
  defence_target_year = c(2035L, 2035L, 2035L),
  effective_interest_rate_long_run = c(0.044, 0.052, 0.038),
  net_capital_investment_gdp = c(0.015, 0.018, 0.012),
  post_forecast_revenue_increase = c(0.012, 0.006, 0.012),
  stock_flow_adjustment = c(0, 0.0025, -0.001)
)

fwrite(macro_scenarios, file.path(processed_dir, "macro_scenario_assumptions.csv"))
fwrite(category_assumptions, file.path(processed_dir, "bottom_up_category_assumptions.csv"))
fwrite(scenario_parameters, file.path(processed_dir, "scenario_parameters.csv"))

message("Scenario assumptions written to: ", processed_dir)

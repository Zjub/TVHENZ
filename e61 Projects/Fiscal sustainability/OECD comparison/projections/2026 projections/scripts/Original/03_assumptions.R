source(file.path("scripts", "00_config.R"))

historical <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
population <- fread(file.path(processed_dir, "population_projection_age_shares.csv"))
scaling <- fread(file.path(processed_dir, "standardisation_parameters.csv"))

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

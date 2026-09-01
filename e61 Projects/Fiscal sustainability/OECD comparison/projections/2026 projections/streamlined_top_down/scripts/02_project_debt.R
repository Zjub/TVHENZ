source("config.R")
source(file.path("R", "model_functions.R"))

inputs <- load_streamlined_inputs()
primary_paths <- fread(file.path(table_dir, "primary_spending_paths.csv"))
actual_primary_paths <- fread(file.path(
  table_dir, "actual_anchored_primary_spending_paths.csv"
))
historical_2019_primary_paths <- fread(file.path(
  table_dir, "historical_2019_anchored_primary_spending_paths.csv"
))
contributions <- fread(file.path(table_dir, "structural_driver_contributions.csv"))
actual_contributions <- fread(file.path(
  table_dir, "structural_actual_anchor_driver_contributions.csv"
))
historical_2019_contributions <- fread(file.path(
  table_dir, "structural_2019_anchor_driver_contributions.csv"
))
income_assumptions <- fread(file.path(
  table_dir, "real_gdp_per_capita_scenarios.csv"
))

central_parameters <- inputs$parameters[scenario == "central"]
official <- inputs$official
history_2025 <- inputs$history[year == 2025L, .(year, nominal_gdp = gdp_nom)]

revenue <- data.table(year = 2025:projection_end)
revenue <- merge(
  revenue,
  official[, .(year, official_revenue = revenue_ratio_gdp)],
  by = "year", all.x = TRUE
)
revenue[, revenue_ratio := official_revenue]
revenue_anchor <- official[year == official_forecast_end, revenue_ratio_gdp]
revenue[year > official_forecast_end, revenue_ratio := approx(
  c(official_forecast_end, 2037L),
  c(revenue_anchor, revenue_anchor + central_parameters$post_forecast_revenue_increase),
  xout = year, rule = 2
)$y]
revenue[, official_revenue := NULL]

simulate_debt <- function(primary_path, model, model_label,
                          anchor_type = c("pbo", "actual")) {
  anchor_type <- match.arg(anchor_type)
  nominal_case <- if (model %in% c("midpoint_gdp", "pressure_gdp")) {
    model
  } else "central_gdp"
  macro <- income_assumptions[income_case == nominal_case, .(
    year, nominal_gdp = scenario_nominal_gdp
  )]
  nominal_gdp <- rbindlist(list(history_2025, macro))[year <= projection_end]
  setorder(nominal_gdp, year)
  nominal_gdp[, nominal_gdp_growth := nominal_gdp / shift(nominal_gdp) - 1]
  dt <- merge(
    data.table(year = 2025:projection_end),
    primary_path[, .(year, primary_fiscal_ratio = primary_fiscal_pp / 100)],
    by = "year", all.x = TRUE
  )
  dt <- merge(dt, nominal_gdp, by = "year", all.x = TRUE)
  dt <- merge(dt, revenue, by = "year", all.x = TRUE)
  dt <- merge(dt, official[, .(
    year,
    official_net_debt = net_debt_ratio_gdp,
    official_gross_debt = gross_debt_ratio_gdp,
    official_interest = public_debt_interest_ratio_gdp
  )], by = "year", all.x = TRUE)
  dt[, `:=`(
    net_debt_ratio = NA_real_,
    gross_debt_ratio = NA_real_,
    interest_ratio = NA_real_,
    total_fiscal_expenditure_ratio = NA_real_,
    fiscal_deficit_ratio = NA_real_
  )]

  gross_net_wedge <- official[year == official_forecast_end,
                              gross_debt_ratio_gdp - net_debt_ratio_gdp]
  implied_rate_2030 <- official[year == official_forecast_end,
    public_debt_interest_ratio_gdp] /
    (official[year == official_forecast_end - 1L, gross_debt_ratio_gdp] /
       (1 + dt[year == official_forecast_end, nominal_gdp_growth]))
  dt[, effective_interest_rate := NA_real_]
  for (rate_year in 2026:official_forecast_end) {
    prior_gross <- official[year == rate_year - 1L, gross_debt_ratio_gdp]
    current_interest <- official[year == rate_year, public_debt_interest_ratio_gdp]
    current_growth <- dt[year == rate_year, nominal_gdp_growth]
    dt[year == rate_year, effective_interest_rate :=
         current_interest / (prior_gross / (1 + current_growth))]
  }
  dt[year > official_forecast_end, effective_interest_rate := approx(
    c(official_forecast_end, 2037L),
    c(implied_rate_2030, central_parameters$effective_interest_rate_long_run),
    xout = year[year > official_forecast_end], rule = 2
  )$y]
  dt[year == 2025L, effective_interest_rate :=
       official[year == 2025L, public_debt_interest_ratio_gdp] /
         official[year == 2025L, gross_debt_ratio_gdp]]

  opening_net <- official[year == 2025L, net_debt_ratio_gdp]
  opening_gross <- official[year == 2025L, gross_debt_ratio_gdp]
  official_cutoff <- if (anchor_type == "pbo") official_forecast_end else 2025L
  for (i in seq_len(nrow(dt))) {
    current_year <- dt$year[i]
    if (current_year <= official_cutoff) {
      dt$net_debt_ratio[i] <- dt$official_net_debt[i]
      dt$gross_debt_ratio[i] <- dt$official_gross_debt[i]
      dt$interest_ratio[i] <- dt$official_interest[i]
      if (is.finite(dt$primary_fiscal_ratio[i])) {
        dt$total_fiscal_expenditure_ratio[i] <-
          dt$primary_fiscal_ratio[i] + dt$interest_ratio[i]
        dt$fiscal_deficit_ratio[i] <-
          dt$total_fiscal_expenditure_ratio[i] - dt$revenue_ratio[i]
      }
      opening_net <- dt$net_debt_ratio[i]
      opening_gross <- dt$gross_debt_ratio[i]
      next
    }
    growth <- dt$nominal_gdp_growth[i]
    opening_net_current_gdp <- opening_net / (1 + growth)
    opening_gross_current_gdp <- max(opening_gross / (1 + growth), 0)
    dt$interest_ratio[i] <- dt$effective_interest_rate[i] * opening_gross_current_gdp
    dt$total_fiscal_expenditure_ratio[i] <-
      dt$primary_fiscal_ratio[i] + dt$interest_ratio[i]
    dt$fiscal_deficit_ratio[i] <-
      dt$total_fiscal_expenditure_ratio[i] - dt$revenue_ratio[i]
    dt$net_debt_ratio[i] <- opening_net_current_gdp +
      dt$fiscal_deficit_ratio[i] + central_parameters$stock_flow_adjustment
    dt$gross_debt_ratio[i] <- max(dt$net_debt_ratio[i] + gross_net_wedge, 0)
    opening_net <- dt$net_debt_ratio[i]
    opening_gross <- dt$gross_debt_ratio[i]
  }
  dt[, `:=`(
    model = model,
    model_label = model_label,
    anchor_type = anchor_type,
    anchor_label = if (anchor_type == "pbo") {
      "PBO forecast through 2029-30"
    } else "Latest National Accounts actual (2024-25)"
  )]
  dt
}

debt_paths_pbo <- rbindlist(lapply(unique(primary_paths$model), function(model_name) {
  path <- primary_paths[model == model_name]
  simulate_debt(path, model_name, unique(path$model_label), "pbo")
}), fill = TRUE)
debt_paths_actual <- rbindlist(lapply(unique(actual_primary_paths$model), function(model_name) {
  path <- actual_primary_paths[model == model_name]
  simulate_debt(path, model_name, unique(path$model_label), "actual")
}), fill = TRUE)
debt_paths <- rbindlist(list(debt_paths_pbo, debt_paths_actual), fill = TRUE)

# Translate each higher-growth GDP-PC spending contribution into its debt effect. The
# counterfactual removes one contribution while leaving all other assumptions
# and the 2030 anchor unchanged.
central_primary <- copy(primary_paths[model == "central_gdp"])
counterfactual_debt <- list()
groups <- unique(contributions$group)
for (group_name in groups) {
  group_path <- contributions[group == group_name,
                              .(year, cumulative_contribution_pp)]
  counterfactual <- merge(central_primary, group_path, by = "year", all.x = TRUE)
  counterfactual[is.na(cumulative_contribution_pp), cumulative_contribution_pp := 0]
  counterfactual[, primary_fiscal_pp :=
                   primary_fiscal_pp - cumulative_contribution_pp]
  counterfactual_debt[[group_name]] <- simulate_debt(
    counterfactual,
    paste0("without_", make.names(tolower(group_name))),
    paste0("Counterfactual without ", group_name),
    "pbo"
  )[, contribution_group := group_name]
}
counterfactual_debt <- rbindlist(counterfactual_debt, fill = TRUE)

central_debt_endpoint <- debt_paths[
  model == "central_gdp" & anchor_type == "pbo" & year == projection_end,
  net_debt_ratio
]
driver_debt_effects <- counterfactual_debt[year == projection_end, .(
  counterfactual_net_debt_2066_pp = 100 * net_debt_ratio,
  driver_effect_on_net_debt_2066_pp =
    100 * (central_debt_endpoint - net_debt_ratio)
), by = contribution_group]

debt_endpoints <- debt_paths[year == projection_end, .(
  model,
  model_label,
  anchor_type,
  anchor_label,
  primary_fiscal_expenditure_2066_pp = 100 * primary_fiscal_ratio,
  public_debt_interest_2066_pp = 100 * interest_ratio,
  total_fiscal_expenditure_2066_pp = 100 * total_fiscal_expenditure_ratio,
  net_debt_2066_pp = 100 * net_debt_ratio,
  gross_debt_2066_pp = 100 * gross_debt_ratio
)]

fwrite(revenue, file.path(table_dir, "central_revenue_path.csv"))
fwrite(debt_paths, file.path(table_dir, "debt_paths.csv"))
fwrite(counterfactual_debt, file.path(table_dir, "driver_debt_counterfactuals.csv"))
fwrite(driver_debt_effects, file.path(table_dir, "structural_driver_debt_effects.csv"))
fwrite(debt_endpoints, file.path(table_dir, "debt_endpoints.csv"))

fit_metrics <- fread(file.path(table_dir, "in_sample_fit.csv"))
rolling_metrics <- fread(file.path(table_dir, "rolling_fit_summary.csv"))
collinearity <- fread(file.path(table_dir, "collinearity.csv"))
residual_dynamics <- fread(file.path(
  table_dir, "residual_dynamics_sensitivity.csv"
))
central_endpoint <- primary_paths[
  model == "central_gdp" & year == projection_end, primary_fiscal_pp
]
anchor_value <- primary_paths[
  model == "central_gdp" & year == official_forecast_end, primary_fiscal_pp
]
contribution_sum <- contributions[year == projection_end,
                                  sum(cumulative_contribution_pp)]
actual_central_endpoint <- actual_primary_paths[
  model == "central_gdp" & year == projection_end, primary_fiscal_pp
]
actual_anchor_value <- actual_primary_paths[
  model == "central_gdp" & year == 2025L, primary_fiscal_pp
]
actual_contribution_sum <- actual_contributions[
  year == projection_end, sum(cumulative_contribution_pp)
]
historical_2019_endpoint <- historical_2019_primary_paths[
  model == "central_gdp" & year == projection_end, primary_fiscal_pp
]
historical_2019_anchor_value <- historical_2019_primary_paths[
  model == "central_gdp" & year == 2019L, primary_fiscal_pp
]
historical_2019_contribution_sum <- historical_2019_contributions[
  year == projection_end, sum(cumulative_contribution_pp)
]
all_primary_paths <- rbindlist(list(
  primary_paths, actual_primary_paths, historical_2019_primary_paths
), fill = TRUE)
validation <- data.table(
  check = c(
    "GDP-PC model driver decomposition reconciles",
    "Latest-actual driver decomposition reconciles",
    "FY2018-19 driver decomposition reconciles",
    "All reported primary paths are finite",
    "All projected debt paths are finite",
    "GDP-PC model long-run transitory contribution is zero",
    "Income-model maximum VIF is below 10",
    "Income-model residuals pass Ljung-Box at 5%",
    "Residual-dynamics alternatives include a white-noise specification",
    "Every rolling model has the expected observations"
  ),
  passed = c(
    abs(anchor_value + contribution_sum - central_endpoint) < 1e-8,
    abs(actual_anchor_value + actual_contribution_sum -
          actual_central_endpoint) < 1e-8,
    abs(historical_2019_anchor_value + historical_2019_contribution_sum -
          historical_2019_endpoint) < 1e-8,
    all(is.finite(all_primary_paths$primary_fiscal_pp)),
    all(is.finite(debt_paths[year >= projection_start, net_debt_ratio])),
    abs(contributions[group == "Transitory macro bridge" & year == projection_end,
                      cumulative_contribution_pp]) < 1e-10,
    max(collinearity[model == "with_income", vif]) < 10,
    fit_metrics[model == "with_income", residual_ljung_box_p] > 0.05,
    any(residual_dynamics$residual_ljung_box_p > 0.05),
    uniqueN(rolling_metrics$observations) == 1L &&
      unique(rolling_metrics$observations) ==
        (rolling_end - rolling_start + 1L) * rolling_horizon
  ),
  detail = c(
    paste0("Gap = ", format(anchor_value + contribution_sum - central_endpoint,
                            scientific = TRUE)),
    paste0("Gap = ", format(
      actual_anchor_value + actual_contribution_sum - actual_central_endpoint,
      scientific = TRUE
    )),
    paste0("Gap = ", format(
      historical_2019_anchor_value + historical_2019_contribution_sum -
        historical_2019_endpoint,
      scientific = TRUE
    )),
    paste0(nrow(all_primary_paths), " model-year rows across three spending anchors"),
    paste0(nrow(debt_paths[year >= projection_start]), " model-year rows"),
    paste0("2066 contribution = ",
           contributions[group == "Transitory macro bridge" & year == projection_end,
                         cumulative_contribution_pp]),
    paste0("Maximum VIF = ", round(max(collinearity[model == "with_income", vif]), 2)),
    paste0("p = ", round(fit_metrics[model == "with_income", residual_ljung_box_p], 3)),
    paste0(
      paste(
        residual_dynamics[residual_ljung_box_p > 0.05, error_specification],
        collapse = ", "
      ),
      "; endpoint range = ",
      round(min(residual_dynamics$pbo_anchored_primary_fiscal_2066_pp), 2),
      "-",
      round(max(residual_dynamics$pbo_anchored_primary_fiscal_2066_pp), 2),
      "%"
    ),
    paste0("Observations per model = ", unique(rolling_metrics$observations))
  ),
  required_for_output = c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE
  )
)
fwrite(validation, file.path(table_dir, "validation_checks.csv"))
if (any(!validation$passed & validation$required_for_output)) {
  stop("One or more required workflow integrity checks failed.")
}
message("Interest and debt paths generated from the streamlined primary-spending models.")

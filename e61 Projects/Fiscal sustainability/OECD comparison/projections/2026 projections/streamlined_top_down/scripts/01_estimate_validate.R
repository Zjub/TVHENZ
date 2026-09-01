source("config.R")
source(file.path("R", "model_functions.R"))

required <- c("forecast", "urca", "strucchange")
missing <- required[!vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))

inputs <- load_streamlined_inputs()
central_future <- copy(inputs$future_all[scenario == "central"])
pressure_income <- inputs$future_all[scenario == "pressure", .(
  year, pressure_log_real_gdp_per_capita = log_real_gdp_per_capita
)]
income_paths <- merge(
  central_future,
  pressure_income,
  by = "year", all.x = TRUE
)
income_paths[, central_log_real_gdp_per_capita := log_real_gdp_per_capita]

# These are pure real-income sensitivities. Demography, prices, unemployment,
# terms of trade and all fiscal/debt settings remain at their central values.
future_by_income_case <- list(
  central_gdp = copy(income_paths)[,
    log_real_gdp_per_capita := central_log_real_gdp_per_capita],
  midpoint_gdp = copy(income_paths)[,
    log_real_gdp_per_capita := 0.5 * (
      central_log_real_gdp_per_capita + pressure_log_real_gdp_per_capita
    )],
  pressure_gdp = copy(income_paths)[,
    log_real_gdp_per_capita := pressure_log_real_gdp_per_capita]
)
future_by_income_case <- lapply(future_by_income_case, function(x) {
  x[, c(
    "pressure_log_real_gdp_per_capita",
    "central_log_real_gdp_per_capita"
  ) := NULL]
  x
})

# Reconstruct nominal GDP consistently from each real-GDP path using the common
# central inflation assumption. This changes GDP growth, not inflation or any
# other macro/fiscal assumption, and gives the debt denominator a coherent path.
opening_real_gdp <- inputs$history[year == 2025L, gdp_real]
opening_nominal_gdp <- inputs$history[year == 2025L, gdp_nom]
future_by_income_case <- lapply(future_by_income_case, function(x) {
  setorder(x, year)
  x[, scenario_real_gdp := exp(log_real_gdp_per_capita) * pop_total]
  x[, scenario_real_gdp_growth := scenario_real_gdp /
      shift(scenario_real_gdp, fill = opening_real_gdp) - 1]
  x[, scenario_nominal_gdp_growth :=
      (1 + scenario_real_gdp_growth) * (1 + inflation) - 1]
  x[, scenario_nominal_gdp := NA_real_]
  previous_nominal <- opening_nominal_gdp
  for (i in seq_len(nrow(x))) {
    x$scenario_nominal_gdp[i] <- previous_nominal *
      (1 + x$scenario_nominal_gdp_growth[i])
    previous_nominal <- x$scenario_nominal_gdp[i]
  }
  x[, `:=`(
    topdown_real_gdp = scenario_real_gdp,
    real_gdp_growth = scenario_real_gdp_growth,
    topdown_nominal_gdp = scenario_nominal_gdp
  )]
  x
})
bridge_tables <- lapply(
  future_by_income_case,
  function(future_case) prepare_bridge_table(
    inputs$history, future_case, inputs$standardisation
  )
)
bridge_table <- bridge_tables$central_gdp
variants <- reported_variants
fits <- setNames(lapply(variants, function(variant) {
  fit_bridge(bridge_table, estimation_end, variant)
}), variants)

fit_metrics <- rbindlist(lapply(fits, in_sample_metrics))
coefficients <- rbindlist(lapply(fits, coefficient_table), fill = TRUE)
collinearity <- rbindlist(lapply(fits, design_diagnostics), fill = TRUE)

residual_tests <- rbindlist(lapply(fits, function(object) {
  residual <- residuals(object$fit)
  adf <- urca::ur.df(residual, type = "none", lags = 2L, selectlags = "BIC")
  ols_data <- as.data.frame(object$estimation[, c("dy", object$terms), with = FALSE])
  formula <- reformulate(object$terms, response = "dy", intercept = FALSE)
  cusum_p <- tryCatch(
    strucchange::sctest(strucchange::efp(formula, data = ols_data, type = "Rec-CUSUM"))$p.value,
    error = function(e) NA_real_
  )
  data.table(
    model = object$variant,
    model_label = unname(model_labels[object$variant]),
    adf_residual_statistic = adf@teststat[1L],
    adf_residual_5pct_critical = adf@cval[1L, "5pct"],
    residual_stationary_at_5pct = adf@teststat[1L] < adf@cval[1L, "5pct"],
    recursive_cusum_p = cusum_p
  )
}))

# Check whether the long-run structural estimates depend on the short-run
# residual process. The main fit remains BIC-selected; fixed low-order ARMA
# alternatives are diagnostics rather than additional projection cases.
fixed_error_orders <- list(
  `Fixed ARMA(0,0)` = c(0L, 0L, 0L),
  `Fixed AR(1)` = c(1L, 0L, 0L),
  `Fixed MA(1)` = c(0L, 0L, 1L),
  `Fixed ARMA(1,1)` = c(1L, 0L, 1L),
  `Fixed AR(2)` = c(2L, 0L, 0L),
  `Fixed MA(2)` = c(0L, 0L, 2L)
)
fixed_error_fits <- lapply(fixed_error_orders, function(order) {
  tryCatch(
    fit_bridge(
      bridge_table, estimation_end, "with_income",
      fixed_order = order
    ),
    error = function(e) NULL
  )
})
selected_order <- forecast::arimaorder(fits$with_income$fit)
error_fits <- c(
  setNames(
    list(fits$with_income),
    paste0(
      "BIC-selected ARMA(", selected_order["p"], ",", selected_order["q"], ")"
    )
  ),
  Filter(Negate(is.null), fixed_error_fits)
)
residual_dynamics_sensitivity <- rbindlist(lapply(
  names(error_fits),
  function(label) {
    object <- error_fits[[label]]
    metric <- in_sample_metrics(object)
    order <- forecast::arimaorder(object$fit)
    estimates <- coef(object$fit)
    residual <- residuals(object$fit)
    residual_acf <- as.numeric(stats::acf(
      residual, lag.max = min(5L, length(residual) - 1L),
      plot = FALSE, na.action = na.pass
    )$acf)[-1L]
    coefficient_value <- function(term) {
      if (term %in% names(estimates)) unname(estimates[term]) else NA_real_
    }
    data.table(
      error_specification = label,
      is_bic_selected = identical(object, fits$with_income),
      ar_order = unname(order["p"]),
      ma_order = unname(order["q"]),
      bic = metric$bic,
      aicc = metric$aicc,
      residual_ljung_box_p = metric$residual_ljung_box_p,
      max_abs_residual_acf_lags_1_5 = max(abs(residual_acf), na.rm = TRUE),
      age_65_74_coefficient = coefficient_value("d_age_65_74_pp"),
      age_75p_coefficient = coefficient_value("d_age_75p_pp"),
      real_gdp_per_capita_coefficient = coefficient_value(
        "d_log_real_gdp_per_capita"
      ),
      pbo_anchored_primary_fiscal_2066_pp = deterministic_projection(
        object, bridge_table, inputs$official
      )[year == projection_end, primary_fiscal_pp]
    )
  }
), fill = TRUE)

# Expanding-window conditional forecasts. The realised driver values are used,
# so this evaluates the spending equation rather than the quality of the
# demographic and macro projections.
rolling_predictions <- list()
counter <- 0L
for (origin in rolling_start:rolling_end) {
  for (variant in variants) {
    object <- tryCatch(
      fit_bridge(bridge_table, origin, variant),
      error = function(e) NULL
    )
    if (is.null(object)) next
    end_year <- min(origin + rolling_horizon, estimation_end)
    changes <- forecast_bridge_changes(object, bridge_table, origin + 1L, end_year)
    start_value <- bridge_table[year == origin, spending_pp]
    prediction <- changes[, .(
      year,
      predicted_spending_pp = start_value + cumsum(predicted_change_pp)
    )]
    prediction <- merge(
      prediction,
      bridge_table[, .(year, actual_spending_pp = spending_pp)],
      by = "year"
    )
    counter <- counter + 1L
    rolling_predictions[[counter]] <- prediction[, `:=`(
      origin = origin,
      horizon = year - origin,
      model = variant,
      model_label = unname(model_labels[variant]),
      error_pp = predicted_spending_pp - actual_spending_pp
    )]
  }
}
rolling_predictions <- rbindlist(rolling_predictions, fill = TRUE)
rolling_by_horizon <- rolling_predictions[, .(
  observations = .N,
  mean_error_pp = mean(error_pp),
  mae_pp = mean(abs(error_pp)),
  rmse_pp = sqrt(mean(error_pp^2))
), by = .(model, model_label, horizon)]
rolling_overall <- rolling_predictions[, .(
  observations = .N,
  average_mae_pp = mean(abs(error_pp)),
  average_rmse_pp = sqrt(mean(error_pp^2)),
  rmse_1y_pp = sqrt(mean(error_pp[horizon == 1L]^2)),
  rmse_5y_pp = sqrt(mean(error_pp[horizon == 5L]^2))
), by = .(model, model_label)]

# Long-horizon GDP sensitivities. The first three cases use the same estimated
# structural equation and differ only in projected real GDP per capita. The
# fourth is a separately estimated equation without the income regressor.
growth_2060 <- vapply(bridge_tables, function(x) {
  100 * (exp(x[year == 2060L, d_log_real_gdp_per_capita]) - 1)
}, numeric(1))
case_labels <- c(
  setNames(
    sprintf("GDP PC model: %.2f%% growth", growth_2060),
    names(growth_2060)
  ),
  no_income = "Model excluding GDP PC"
)
case_specs <- list(
  central_gdp = list(object = fits$with_income, table = bridge_tables$central_gdp),
  midpoint_gdp = list(object = fits$with_income, table = bridge_tables$midpoint_gdp),
  pressure_gdp = list(object = fits$with_income, table = bridge_tables$pressure_gdp),
  no_income = list(object = fits$no_income, table = bridge_tables$central_gdp)
)
build_case_paths <- function(anchor = c("pbo", "actual", "historical_2019")) {
  anchor <- match.arg(anchor)
  rbindlist(lapply(names(case_specs), function(case_id) {
    spec <- case_specs[[case_id]]
    path <- if (anchor == "actual") {
      deterministic_projection_from_actual(spec$object, spec$table)
    } else if (anchor == "historical_2019") {
      deterministic_projection_from_historical_anchor(
        spec$object, spec$table, 2019L,
        path_type = paste0(
          "FY2018-19 actual; deterministic structural changes thereafter"
        )
      )
    } else {
      deterministic_projection(spec$object, spec$table, inputs$official)
    }
    path[, `:=`(
      underlying_model = model,
      model = case_id,
      model_label = unname(case_labels[case_id]),
      income_case = case_id,
      income_case_label = unname(case_labels[case_id])
    )]
    path
  }), fill = TRUE)
}
anchored_paths <- build_case_paths("pbo")
actual_anchored_paths <- build_case_paths("actual")
historical_2019_paths <- build_case_paths("historical_2019")
model_only_paths <- rbindlist(lapply(fits, model_only_projection, table = bridge_table))
structural_contributions <- projection_contributions(
  fits$with_income, bridge_table
)
structural_actual_contributions <- projection_contributions(
  fits$with_income, bridge_table,
  anchor_year = max(inputs$history$year)
)
structural_2019_contributions <- projection_contributions(
  fits$with_income, bridge_table, anchor_year = 2019L
)
central_interval <- parameter_interval(
  fits$with_income, bridge_table, inputs$official
)
central_actual_interval <- parameter_interval_from_actual(
  fits$with_income, bridge_table
)
central_2019_interval <- parameter_interval_from_historical_anchor(
  fits$with_income, bridge_table, 2019L,
  path_type = "FY2018-19 actual; deterministic structural changes thereafter"
)

income_assumptions <- rbindlist(lapply(names(bridge_tables), function(case_id) {
  merge(
    bridge_tables[[case_id]][sample == "projection", .(
      year,
      log_real_gdp_per_capita,
      real_gdp_per_capita_growth = exp(d_log_real_gdp_per_capita) - 1
    )],
    future_by_income_case[[case_id]][, .(
      year, scenario_real_gdp, scenario_real_gdp_growth,
      scenario_nominal_gdp, scenario_nominal_gdp_growth
    )],
    by = "year"
  )[, `:=`(
    income_case = case_id,
    income_case_label = unname(case_labels[case_id])
  )]
}))

endpoints <- merge(
  anchored_paths[year == projection_end, .(
    model, model_label, anchored_primary_fiscal_2066_pp = primary_fiscal_pp
  )],
  actual_anchored_paths[year == projection_end, .(
    model, actual_anchored_primary_fiscal_2066_pp = primary_fiscal_pp
  )], by = "model"
)
endpoints <- merge(
  endpoints,
  historical_2019_paths[year == projection_end, .(
    model,
    historical_2019_anchored_primary_fiscal_2066_pp = primary_fiscal_pp
  )], by = "model"
)
endpoints[, conditional_model_only_primary_fiscal_2066_pp := NA_real_]
endpoints[model == "central_gdp",
  conditional_model_only_primary_fiscal_2066_pp := model_only_paths[
    model == "with_income" & year == projection_end, primary_fiscal_pp
  ]
]
endpoints[model == "no_income",
  conditional_model_only_primary_fiscal_2066_pp := model_only_paths[
    model == "no_income" & year == projection_end, primary_fiscal_pp
  ]
]
endpoint_interval <- central_interval[year == projection_end, .(
  central_lower_80_pp = lower_80_pp,
  central_upper_80_pp = upper_80_pp,
  central_lower_95_pp = lower_95_pp,
  central_upper_95_pp = upper_95_pp
)]
endpoints[model == "central_gdp", names(endpoint_interval) := endpoint_interval]
actual_endpoint_interval <- central_actual_interval[year == projection_end, .(
  actual_anchor_lower_80_pp = lower_80_pp,
  actual_anchor_upper_80_pp = upper_80_pp,
  actual_anchor_lower_95_pp = lower_95_pp,
  actual_anchor_upper_95_pp = upper_95_pp
)]
endpoints[model == "central_gdp", names(actual_endpoint_interval) := actual_endpoint_interval]
historical_2019_endpoint_interval <- central_2019_interval[
  year == projection_end, .(
    historical_2019_lower_80_pp = lower_80_pp,
    historical_2019_upper_80_pp = upper_80_pp,
    historical_2019_lower_95_pp = lower_95_pp,
    historical_2019_upper_95_pp = upper_95_pp
  )
]
endpoints[model == "central_gdp",
  names(historical_2019_endpoint_interval) := historical_2019_endpoint_interval
]

# Estimation-window and COVID-observation sensitivities for the income model.
baseline_window <- paste0("Full window 1980-", estimation_end)
window_specs <- list()
window_specs[[baseline_window]] <- list(
  first = 1980L, last = estimation_end, table = bridge_table
)
window_specs[[paste0("Starts in 1990; through ", estimation_end)]] <- list(
  first = 1990L, last = estimation_end, table = bridge_table
)
if (!workflow_is_pre_2020) {
  window_specs[["Estimated through 2019"]] <- list(
    first = 1980L, last = 2019L, table = bridge_table
  )
}
window_sensitivity <- rbindlist(lapply(names(window_specs), function(label) {
  spec <- window_specs[[label]]
  object <- fit_bridge(spec$table, spec$last, "with_income", spec$first)
  endpoint <- deterministic_projection(object, bridge_table, inputs$official)[
    year == projection_end, primary_fiscal_pp
  ]
  data.table(
    window = label,
    first_estimation_year = object$first_year,
    last_estimation_year = object$last_year,
    endpoint_2066_pp = endpoint
  )
}))
age_65p_fit <- fit_bridge(bridge_table, estimation_end, "with_income_65p")
window_sensitivity <- rbindlist(list(
  window_sensitivity,
  data.table(
    window = "Combined population share aged 65+ instead of separate 65-74 and 75+ shares",
    first_estimation_year = age_65p_fit$first_year,
    last_estimation_year = age_65p_fit$last_year,
    endpoint_2066_pp = deterministic_projection(
      age_65p_fit, bridge_table, inputs$official
    )[year == projection_end, primary_fiscal_pp]
  )
), use.names = TRUE)
baseline_endpoint <- window_sensitivity[window == baseline_window, endpoint_2066_pp]
window_sensitivity[, difference_from_full_sample_pp := endpoint_2066_pp - baseline_endpoint]

covid_exclusion_table <- copy(bridge_table)
covid_exclusion_table[year %in% 2020:2023, dy := NA_real_]
covid_excluded_fit <- fit_bridge(covid_exclusion_table, estimation_end, "with_income")
covid_treatment <- data.table(
  treatment = c(
    "Differenced FY2020-22 level interventions",
    "Exclude annual changes FY2020-23"
  ),
  endpoint_2066_pp = c(
    endpoints[model == "central_gdp", anchored_primary_fiscal_2066_pp],
    deterministic_projection(covid_excluded_fit, bridge_table, inputs$official)[
      year == projection_end, primary_fiscal_pp
    ]
  )
)
covid_treatment[, difference_from_central_pp := endpoint_2066_pp - endpoint_2066_pp[1L]]

# Retain the decisive rejection evidence without regenerating the old model grid.
legacy_assessment_file <- file.path(project_dir, "outputs", "tables", "model_selection_assessment.csv")
legacy_ecm_file <- file.path(project_dir, "outputs", "tables", "ecm_validation_summary.csv")
legacy_assessment <- if (file.exists(legacy_assessment_file)) fread(legacy_assessment_file) else data.table()
legacy_ecm <- if (file.exists(legacy_ecm_file)) fread(legacy_ecm_file) else data.table()

saveRDS(fits, file.path(model_dir, "streamlined_bridge_fits.rds"))
fwrite(bridge_table, file.path(table_dir, "model_input_table.csv"))
fwrite(fit_metrics, file.path(table_dir, "in_sample_fit.csv"))
fwrite(coefficients, file.path(table_dir, "coefficients.csv"))
fwrite(collinearity, file.path(table_dir, "collinearity.csv"))
fwrite(residual_tests, file.path(table_dir, "residual_and_stability_tests.csv"))
fwrite(residual_dynamics_sensitivity,
       file.path(table_dir, "residual_dynamics_sensitivity.csv"))
fwrite(rolling_predictions, file.path(table_dir, "rolling_predictions.csv"))
fwrite(rolling_by_horizon, file.path(table_dir, "rolling_fit_by_horizon.csv"))
fwrite(rolling_overall, file.path(table_dir, "rolling_fit_summary.csv"))
fwrite(anchored_paths, file.path(table_dir, "primary_spending_paths.csv"))
fwrite(actual_anchored_paths, file.path(table_dir, "actual_anchored_primary_spending_paths.csv"))
fwrite(historical_2019_paths,
       file.path(table_dir, "historical_2019_anchored_primary_spending_paths.csv"))
fwrite(rbindlist(list(
  copy(anchored_paths)[, anchor_type := "PBO forecast anchor"],
  copy(actual_anchored_paths)[, anchor_type := "Latest actual anchor"],
  copy(historical_2019_paths)[, anchor_type := "FY2018-19 actual anchor"]
), fill = TRUE), file.path(table_dir, "all_anchor_primary_spending_paths.csv"))
fwrite(model_only_paths, file.path(table_dir, "model_only_paths.csv"))
fwrite(income_assumptions, file.path(table_dir, "real_gdp_per_capita_scenarios.csv"))
fwrite(structural_contributions, file.path(table_dir, "structural_driver_contributions.csv"))
fwrite(structural_actual_contributions,
       file.path(table_dir, "structural_actual_anchor_driver_contributions.csv"))
fwrite(structural_2019_contributions,
       file.path(table_dir, "structural_2019_anchor_driver_contributions.csv"))
fwrite(central_interval, file.path(
  table_dir, "gdp_pc_model_high_growth_parameter_interval.csv"
))
fwrite(central_actual_interval,
       file.path(table_dir, "gdp_pc_model_actual_anchor_parameter_interval.csv"))
fwrite(central_2019_interval,
       file.path(table_dir, "gdp_pc_model_2019_anchor_parameter_interval.csv"))
fwrite(endpoints, file.path(table_dir, "model_endpoints.csv"))
fwrite(window_sensitivity, file.path(table_dir, "structural_window_sensitivity.csv"))
fwrite(covid_treatment, file.path(table_dir, "structural_covid_sensitivity.csv"))
if (nrow(legacy_assessment)) fwrite(legacy_assessment, file.path(table_dir, "excluded_model_evidence.csv"))
if (nrow(legacy_ecm)) fwrite(legacy_ecm, file.path(table_dir, "excluded_ecm_evidence.csv"))

message("Streamlined structural-change models estimated and validated.")

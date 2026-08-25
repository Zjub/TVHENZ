source(file.path("scripts", "00_config.R"))
source(file.path("R", "model_functions_2.R"))

historical <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
future <- fread(file.path(processed_dir, "macro_scenario_assumptions.csv"))[scenario == "central"]
official <- fread(file.path(processed_dir, "official_pbo_nfo_wide.csv"))

driver_variants <- list(
  baseline = character(),
  population_only = "population",
  real_gdp_per_capita_only = "real_gdp_per_capita",
  population_and_real_gdp_per_capita = c("population", "real_gdp_per_capita")
)
variant_labels <- c(
  baseline = "Current controls only",
  population_only = "Add population",
  real_gdp_per_capita_only = "Add real GDP per capita",
  population_and_real_gdp_per_capita = "Add population and real GDP per capita"
)

arma_degrees <- function(fit) {
  order <- forecast::arimaorder(fit)
  sum(order[intersect(c("p", "q", "P", "Q"), names(order))])
}

metric_row <- function(model, actual, fitted, residuals, observations) {
  keep <- is.finite(actual) & is.finite(fitted)
  actual <- actual[keep]
  fitted <- fitted[keep]
  residuals <- residuals[is.finite(residuals)]
  data.table(
    model = model,
    observations = observations,
    r_squared = 1 - sum((actual - fitted)^2) /
      sum((actual - mean(actual))^2),
    in_sample_mae_pp = mean(abs(actual - fitted)) * 100,
    in_sample_rmse_pp = sqrt(mean((actual - fitted)^2)) * 100,
    residual_sd_pp = sd(residuals) * 100
  )
}

fit_level_metrics <- function(fit, train, drivers) {
  y <- train$broad_expenditure_gdp
  dy <- diff(y)
  train_dyn <- build_topdown_table(
    train, include_y = TRUE, scale_drivers = drivers
  )

  ols_fitted <- as.numeric(fitted(fit$fits$structural_ols))
  level_fitted <- as.numeric(fitted(fit$fits$arimax_level))
  diff_change <- as.numeric(fitted(fit$fits$arimax_diff))
  diff_level <- y[-length(y)] + diff_change

  dynamic_terms <- colnames(fit$fits$dynamic_diff$xreg)
  dynamic_rows <- complete.cases(
    train_dyn[, c("dy", dynamic_terms), with = FALSE]
  )
  dynamic_change <- as.numeric(fitted(fit$fits$dynamic_diff))
  dynamic_level <- train_dyn$y_lag[dynamic_rows] + dynamic_change

  structural <- structural_feature_frame(train, drivers)
  structural_fitted <- as.numeric(predict(
    fit$fits$hybrid_structural_ols, newdata = structural
  ))
  structural_change <- c(NA_real_, diff(structural_fitted))
  hybrid_terms <- colnames(fit$fits$hybrid_macro$xreg)
  hybrid_rows <- complete.cases(
    train_dyn[, c("dy", hybrid_terms), with = FALSE]
  )
  hybrid_macro_change <- as.numeric(fitted(fit$fits$hybrid_macro))
  hybrid_change <- structural_change[hybrid_rows] + hybrid_macro_change
  hybrid_level <- train_dyn$y_lag[hybrid_rows] + hybrid_change

  ecm_terms <- setdiff(all.vars(formula(fit$fits$ardl_ecm)), "dy")
  ecm_rows <- complete.cases(train_dyn[, c("dy", ecm_terms), with = FALSE])
  ecm_change <- as.numeric(fitted(fit$fits$ardl_ecm))
  ecm_level <- train_dyn$y_lag[ecm_rows] + ecm_change

  univariate_fitted <- as.numeric(fitted(fit$fits$univariate_arima))

  rbindlist(list(
    metric_row(
      "structural_ols", y, ols_fitted,
      residuals(fit$fits$structural_ols), length(ols_fitted)
    ),
    metric_row(
      "arimax_level", y, level_fitted,
      residuals(fit$fits$arimax_level), length(level_fitted)
    ),
    metric_row(
      "arimax_diff", y[-1L], diff_level,
      dy - diff_change, length(diff_change)
    ),
    metric_row(
      "dynamic_diff", train_dyn$y[dynamic_rows], dynamic_level,
      train_dyn$dy[dynamic_rows] - dynamic_change, length(dynamic_change)
    ),
    metric_row(
      "hybrid", train_dyn$y[hybrid_rows], hybrid_level,
      train_dyn$dy[hybrid_rows] - hybrid_change, length(hybrid_change)
    ),
    metric_row(
      "ardl_ecm", train_dyn$y[ecm_rows], ecm_level,
      residuals(fit$fits$ardl_ecm), length(ecm_change)
    ),
    metric_row(
      "univariate_arima", y, univariate_fitted,
      residuals(fit$fits$univariate_arima), length(univariate_fitted)
    )
  ))
}

coefficient_rows <- function(fit, variant) {
  components <- list(
    structural_ols = fit$fits$structural_ols,
    arimax_level = fit$fits$arimax_level,
    arimax_diff = fit$fits$arimax_diff,
    dynamic_diff = fit$fits$dynamic_diff,
    hybrid_structural = fit$fits$hybrid_structural_ols,
    hybrid_macro = fit$fits$hybrid_macro,
    ardl_ecm = fit$fits$ardl_ecm,
    univariate_arima = fit$fits$univariate_arima
  )
  rbindlist(lapply(names(components), function(component) {
    object <- components[[component]]
    estimates <- coef(object)
    covariance <- tryCatch(vcov(object), error = function(e) NULL)
    standard_errors <- if (is.null(covariance)) {
      rep(NA_real_, length(estimates))
    } else {
      sqrt(diag(covariance))
    }
    keep <- grepl("log_population|log_real_gdp_per_capita", names(estimates))
    if (!any(keep)) return(NULL)
    data.table(
      variant = variant,
      component = component,
      term = names(estimates)[keep],
      estimate = as.numeric(estimates[keep]),
      standard_error = standard_errors[keep],
      p_value = 2 * pnorm(-abs(as.numeric(estimates[keep]) / standard_errors[keep]))
    )
  }), fill = TRUE)
}

full_results <- list()
path_results <- list()
coefficient_results <- list()
fits_by_variant <- list()

anchor_value <- official[
  year == official_forecast_end,
  official_topdown_expenditure(.SD)
]

for (variant in names(driver_variants)) {
  drivers <- driver_variants[[variant]]
  fit <- fit_predict_topdown(
    historical, future, additional_drivers = drivers
  )
  fits_by_variant[[variant]] <- fit$fits

  metrics <- fit_level_metrics(fit, historical, drivers)
  metadata <- copy(fit$metadata)
  metadata[, variant := variant]
  metrics[, variant := variant]
  summary <- merge(metadata, metrics, by = c("variant", "model"), all = TRUE)

  official_comparison <- merge(
    fit$paths[year <= official_forecast_end],
    official[, .(
      year,
      official_value = official_topdown_expenditure(.SD)
    )],
    by = "year"
  )
  official_metrics <- official_comparison[, .(
    official_period_mae_pp = mean(abs(value - official_value)) * 100,
    official_period_rmse_pp = sqrt(mean((value - official_value)^2)) * 100
  ), by = model]
  summary <- merge(summary, official_metrics, by = "model", all.x = TRUE)

  endpoints <- fit$paths[, .(
    model_only_join = value[year == official_forecast_end],
    model_only_2066 = value[year == projection_end],
    anchored_2066 = anchor_value + value[year == projection_end] -
      value[year == official_forecast_end]
  ), by = model]
  summary <- merge(summary, endpoints, by = "model", all.x = TRUE)
  full_results[[variant]] <- summary

  variant_paths <- copy(fit$paths)
  variant_paths[, `:=`(
    variant = variant,
    variant_label = unname(variant_labels[variant]),
    model_label = unname(model_labels[model])
  )]
  path_results[[variant]] <- variant_paths
  coefficient_results[[variant]] <- coefficient_rows(fit, variant)
}

full_summary <- rbindlist(full_results, fill = TRUE)
paths_all <- rbindlist(path_results, fill = TRUE)
coefficients_all <- rbindlist(coefficient_results, fill = TRUE)

baseline_ic <- full_summary[variant == "baseline", .(
  model, baseline_aic = aic, baseline_bic = bic
)]
full_summary <- merge(full_summary, baseline_ic, by = "model", all.x = TRUE)
full_summary[, `:=`(
  delta_aic_from_baseline = aic - baseline_aic,
  delta_bic_from_baseline = bic - baseline_bic,
  model_label = unname(model_labels[model]),
  variant_label = unname(variant_labels[variant])
)]

# Expanding-window, one-to-five-year conditional forecast evaluation for every
# driver variant. Each origin uses only observations available at that origin.
rolling_results <- list()
counter <- 0L
for (variant in names(driver_variants)) {
  drivers <- driver_variants[[variant]]
  for (origin in forecast_check_start:forecast_check_end) {
    train <- historical[year <= origin]
    validation <- historical[
      year > origin & year <= origin + forecast_check_horizon
    ]
    if (nrow(train) < 25L || !nrow(validation)) next
    fit <- tryCatch(
      fit_predict_topdown(train, validation, additional_drivers = drivers),
      error = function(e) NULL
    )
    if (is.null(fit)) next
    counter <- counter + 1L
    rolling_results[[counter]] <- merge(
      fit$paths,
      validation[, .(year, actual = broad_expenditure_gdp)],
      by = "year"
    )[, `:=`(
      variant = variant,
      origin = origin,
      horizon = year - origin,
      error = value - actual
    )]
  }
}
rolling <- rbindlist(rolling_results, fill = TRUE)
rolling_summary <- rolling[, .(
  n = .N,
  mean_error_pp = mean(error) * 100,
  mae_pp = mean(abs(error)) * 100,
  rmse_pp = sqrt(mean(error^2)) * 100
), by = .(variant, model, horizon)]
rolling_overall <- rolling[, .(
  rolling_rmse_all_pp = sqrt(mean(error^2)) * 100,
  rolling_mae_all_pp = mean(abs(error)) * 100
), by = .(variant, model)]
rolling_wide <- dcast(
  rolling_summary, variant + model ~ horizon,
  value.var = "rmse_pp"
)
horizon_columns <- intersect(
  as.character(1:forecast_check_horizon), names(rolling_wide)
)
setnames(
  rolling_wide,
  horizon_columns,
  paste0("rolling_rmse_", horizon_columns, "y_pp")
)
full_summary <- merge(full_summary, rolling_overall, by = c("variant", "model"), all.x = TRUE)
full_summary <- merge(full_summary, rolling_wide, by = c("variant", "model"), all.x = TRUE)
full_summary[, `:=`(
  rolling_rank_all = frank(rolling_rmse_all_pp, ties.method = "min"),
  rolling_rank_5y = frank(rolling_rmse_5y_pp, ties.method = "min")
), by = variant]

saveRDS(fits_by_variant, file.path(model_dir, "top_down_scale_income_variant_fits.rds"))
fwrite(full_summary, file.path(table_dir, "top_down_scale_income_model_comparison.csv"))
fwrite(rolling, file.path(table_dir, "top_down_scale_income_rolling_predictions.csv"))
fwrite(rolling_summary, file.path(table_dir, "top_down_scale_income_rolling_summary.csv"))
fwrite(paths_all, file.path(table_dir, "top_down_scale_income_projection_paths.csv"))
fwrite(coefficients_all, file.path(table_dir, "top_down_scale_income_coefficients.csv"))

message("Population and real-GDP-per-capita variants evaluated across all top-down specifications.")

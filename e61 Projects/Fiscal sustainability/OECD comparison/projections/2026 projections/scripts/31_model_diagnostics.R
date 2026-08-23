source(file.path("scripts", "00_config.R"))
source(file.path("R", "model_functions_2.R"))

historical <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
future <- fread(file.path(processed_dir, "macro_scenario_assumptions.csv"))[scenario == "central"]
official <- fread(file.path(processed_dir, "official_pbo_nfo_wide.csv"))
fits <- readRDS(file.path(model_dir, "top_down_fitted_models.rds"))
rolling_checks <- fread(file.path(table_dir, "forecast_check_rolling_summary.csv"))
official_checks <- fread(file.path(table_dir, "forecast_check_against_official_summary.csv"))

y <- historical$broad_expenditure_gdp
dy <- diff(y)
years <- historical$year

metric_row <- function(model, actual, fitted, residuals, first_year, scale = "Spending/GDP level",
                       box_fitdf = 0L) {
  keep <- is.finite(actual) & is.finite(fitted)
  actual <- actual[keep]
  fitted <- fitted[keep]
  residuals <- residuals[is.finite(residuals)]
  lag <- min(length(residuals) - 1L, max(10L, box_fitdf + 3L))
  box <- Box.test(residuals, lag = lag, type = "Ljung-Box", fitdf = box_fitdf)
  data.table(
    model = model,
    model_label = unname(model_labels[model]),
    metric_scale = scale,
    observations = length(actual),
    first_fitted_year = first_year,
    last_fitted_year = max(years),
    r_squared = 1 - sum((actual - fitted)^2) / sum((actual - mean(actual))^2),
    mae_pp = mean(abs(actual - fitted)) * 100,
    rmse_pp = sqrt(mean((actual - fitted)^2)) * 100,
    residual_sd_pp = sd(residuals) * 100,
    ljung_box_lag = lag,
    ljung_box_fit_degrees = box_fitdf,
    ljung_box_p_value = unname(box$p.value),
    shapiro_wilk_p_value = if (length(residuals) >= 3L && length(residuals) <= 5000L) {
      unname(shapiro.test(residuals)$p.value)
    } else NA_real_
  )
}

ols_fitted <- as.numeric(fitted(fits$structural_ols))
level_fitted <- as.numeric(fitted(fits$arimax_level))
diff_change_fitted <- as.numeric(fitted(fits$arimax_diff))
diff_level_fitted <- y[-length(y)] + diff_change_fitted

train_dyn <- build_topdown_table(historical, include_y = TRUE)

dynamic_terms <- colnames(fits$dynamic_diff$xreg)
dynamic_rows <- complete.cases(train_dyn[, c("dy", dynamic_terms), with = FALSE])
dynamic_change_fitted <- as.numeric(fitted(fits$dynamic_diff))
dynamic_level_fitted <- train_dyn$y_lag[dynamic_rows] + dynamic_change_fitted

hybrid_structural_history <- structural_feature_frame(historical)
hybrid_structural_fitted <- as.numeric(predict(
  fits$hybrid_structural_ols,
  newdata = hybrid_structural_history
))
hybrid_structural_change <- c(NA_real_, diff(hybrid_structural_fitted))
hybrid_macro_terms <- colnames(fits$hybrid_macro$xreg)
hybrid_rows <- complete.cases(train_dyn[, c("dy", hybrid_macro_terms), with = FALSE])
hybrid_macro_fitted <- as.numeric(fitted(fits$hybrid_macro))
hybrid_change_fitted <- hybrid_structural_change[hybrid_rows] + hybrid_macro_fitted
hybrid_level_fitted <- train_dyn$y_lag[hybrid_rows] + hybrid_change_fitted

ecm_terms <- setdiff(all.vars(formula(fits$ardl_ecm)), "dy")
ecm_rows <- complete.cases(train_dyn[, c("dy", ecm_terms), with = FALSE])
ecm_change_fitted <- as.numeric(fitted(fits$ardl_ecm))
ecm_level_fitted <- train_dyn$y_lag[ecm_rows] + ecm_change_fitted

univariate_fitted <- as.numeric(fitted(fits$univariate_arima))
arma_fitdf <- function(fit) {
  order <- arimaorder(fit)
  sum(order[intersect(c("p", "q", "P", "Q"), names(order))])
}

fit_diagnostics <- rbindlist(list(
  metric_row("structural_ols", y, ols_fitted, residuals(fits$structural_ols), min(years)),
  metric_row("arimax_level", y, level_fitted, residuals(fits$arimax_level), min(years),
             box_fitdf = arma_fitdf(fits$arimax_level)),
  metric_row("arimax_diff", y[-1L], diff_level_fitted, dy - diff_change_fitted, years[2L],
             box_fitdf = arma_fitdf(fits$arimax_diff)),
  metric_row("dynamic_diff", train_dyn$y[dynamic_rows], dynamic_level_fitted,
             train_dyn$dy[dynamic_rows] - dynamic_change_fitted,
             min(train_dyn$year[dynamic_rows]), box_fitdf = arma_fitdf(fits$dynamic_diff)),
  metric_row("hybrid", train_dyn$y[hybrid_rows], hybrid_level_fitted,
             train_dyn$dy[hybrid_rows] - hybrid_change_fitted,
             min(train_dyn$year[hybrid_rows]),
             box_fitdf = arma_fitdf(fits$hybrid_macro)),
  metric_row("ardl_ecm", train_dyn$y[ecm_rows], ecm_level_fitted,
             residuals(fits$ardl_ecm), min(train_dyn$year[ecm_rows])),
  metric_row("univariate_arima", y, univariate_fitted, residuals(fits$univariate_arima), min(years),
             box_fitdf = arma_fitdf(fits$univariate_arima))
))

lm_coefficient_table <- function(fit, model, component) {
  sm <- summary(fit)$coefficients
  x <- model.matrix(fit)
  e <- residuals(fit)
  n <- nrow(x)
  k <- ncol(x)
  bread <- solve(crossprod(x))
  meat <- crossprod(x, x * as.numeric(e^2))
  vcov_hc1 <- (n / (n - k)) * bread %*% meat %*% bread
  hc1_se <- sqrt(diag(vcov_hc1))
  data.table(
    model = model, model_label = unname(model_labels[model]), component = component,
    term = rownames(sm), estimate = sm[, 1L], conventional_se = sm[, 2L],
    robust_hc1_se = hc1_se,
    conventional_p_value = sm[, 4L],
    robust_hc1_p_value = 2 * pnorm(-abs(sm[, 1L] / hc1_se)),
    standard_error_scope = "Coefficient uncertainty conditional on the specification"
  )
}

arima_coefficient_table <- function(fit, model, component) {
  estimates <- coef(fit)
  covariance <- tryCatch(vcov(fit), error = function(e) NULL)
  se <- if (is.null(covariance)) rep(NA_real_, length(estimates)) else sqrt(diag(covariance))
  data.table(
    model = model, model_label = unname(model_labels[model]), component = component,
    term = names(estimates), estimate = as.numeric(estimates), conventional_se = se,
    robust_hc1_se = NA_real_,
    conventional_p_value = 2 * pnorm(-abs(as.numeric(estimates) / se)),
    robust_hc1_p_value = NA_real_,
    standard_error_scope = "ARIMA maximum-likelihood coefficient uncertainty conditional on the specification"
  )
}

coefficient_uncertainty <- rbindlist(list(
  lm_coefficient_table(fits$structural_ols, "structural_ols", "Structural equation"),
  arima_coefficient_table(fits$arimax_level, "arimax_level", "Level ARIMAX"),
  arima_coefficient_table(fits$arimax_diff, "arimax_diff", "Difference ARIMAX"),
  arima_coefficient_table(fits$dynamic_diff, "dynamic_diff", "Dynamic difference ARIMAX"),
  lm_coefficient_table(fits$hybrid_structural_ols, "hybrid", "Structural level equation"),
  arima_coefficient_table(fits$hybrid_macro, "hybrid", "Macro difference ARIMAX"),
  lm_coefficient_table(fits$ardl_ecm, "ardl_ecm", "Error-correction equation"),
  arima_coefficient_table(fits$univariate_arima, "univariate_arima", "ARIMA with COVID controls")
), fill = TRUE)

# Conditional forecast intervals hold the supplied driver path fixed. ARIMA
# paths include future innovation uncertainty but not parameter or driver-path
# uncertainty. OLS uses its conventional prediction interval.
set.seed(20260715)
n_sim <- 2000L
h <- nrow(future)
x_future_all <- model_feature_frame(future)
x_history_all <- model_feature_frame(historical)

summarise_simulations <- function(paths, model) {
  data.table(
    year = future$year,
    model = model,
    model_label = unname(model_labels[model]),
    mean = rowMeans(paths),
    standard_error = apply(paths, 1L, sd),
    lower_95 = apply(paths, 1L, quantile, probs = 0.025),
    upper_95 = apply(paths, 1L, quantile, probs = 0.975),
    interval_scope = "Conditional model-only interval; fixed economic and demographic drivers"
  )
}

ols_pred <- predict(fits$structural_ols, newdata = x_future_all, interval = "prediction", level = 0.95)
ols_interval <- data.table(
  year = future$year, model = "structural_ols", model_label = unname(model_labels["structural_ols"]),
  mean = ols_pred[, "fit"],
  standard_error = (ols_pred[, "upr"] - ols_pred[, "lwr"]) / (2 * qnorm(0.975)),
  lower_95 = ols_pred[, "lwr"], upper_95 = ols_pred[, "upr"],
  interval_scope = "Conditional model-only OLS prediction interval; fixed drivers"
)

level_x <- x_future_all[, colnames(fits$arimax_level$xreg), drop = FALSE]
level_sim <- replicate(n_sim, as.numeric(simulate(
  fits$arimax_level, nsim = h, xreg = as.matrix(level_x), future = TRUE
)))

diff_columns <- colnames(fits$arimax_diff$xreg)
diff_history_base <- model_feature_frame(historical, include_covid = FALSE)
diff_future_base <- model_feature_frame(future, include_covid = FALSE)
diff_combined <- rbind(diff_history_base[nrow(diff_history_base), , drop = FALSE], diff_future_base)
diff_x <- cbind(
  as.data.frame(apply(diff_combined, 2L, diff)),
  covid_feature_frame(future)
)[, diff_columns, drop = FALSE]
diff_sim_change <- replicate(n_sim, as.numeric(simulate(
  fits$arimax_diff, nsim = h, xreg = as.matrix(diff_x), future = TRUE
)))
diff_sim <- apply(diff_sim_change, 2L, cumsum) + tail(y, 1L)

future_dyn <- build_future_feature_table(historical, future)

dynamic_x <- as.data.frame(future_dyn[, colnames(fits$dynamic_diff$xreg), with = FALSE])
dynamic_sim_change <- replicate(n_sim, as.numeric(simulate(
  fits$dynamic_diff, nsim = h, xreg = as.matrix(dynamic_x), future = TRUE
)))
dynamic_sim <- apply(dynamic_sim_change, 2L, cumsum) + tail(y, 1L)

hybrid_structural_future <- structural_feature_frame(future)
hybrid_structural_projected <- as.numeric(predict(
  fits$hybrid_structural_ols,
  newdata = hybrid_structural_future
))
hybrid_structural_future_change <- diff(c(
  tail(hybrid_structural_fitted, 1L),
  hybrid_structural_projected
))
hybrid_macro_x <- as.data.frame(future_dyn[, colnames(fits$hybrid_macro$xreg), with = FALSE])
hybrid_macro_sim <- replicate(n_sim, as.numeric(simulate(
  fits$hybrid_macro, nsim = h, xreg = as.matrix(hybrid_macro_x), future = TRUE
)))
hybrid_sim <- apply(
  hybrid_macro_sim + hybrid_structural_future_change,
  2L,
  cumsum
) + tail(y, 1L)

simulate_ecm_path <- function() {
  path <- numeric(h)
  y_previous <- tail(train_dyn$y, 1L)
  dy_previous <- tail(na.omit(train_dyn$dy), 1L)
  innovations <- rnorm(h, mean = 0, sd = summary(fits$ardl_ecm)$sigma)

  for (i in seq_len(h)) {
    row <- as.data.frame(future_dyn[i])
    row$y_lag <- y_previous
    row$dy_lag <- dy_previous
    dy_forecast <- as.numeric(predict(fits$ardl_ecm, newdata = row)) + innovations[i]
    path[i] <- y_previous + dy_forecast
    y_previous <- path[i]
    dy_previous <- dy_forecast
  }

  path
}
ecm_sim <- replicate(n_sim, simulate_ecm_path())

univariate_x <- covid_feature_frame(future)
univariate_x <- univariate_x[, colnames(fits$univariate_arima$xreg), drop = FALSE]
univariate_sim <- replicate(n_sim, as.numeric(simulate(
  fits$univariate_arima, nsim = h, xreg = as.matrix(univariate_x), future = TRUE
)))

forecast_uncertainty <- rbindlist(list(
  ols_interval,
  summarise_simulations(level_sim, "arimax_level"),
  summarise_simulations(diff_sim, "arimax_diff"),
  summarise_simulations(dynamic_sim, "dynamic_diff"),
  summarise_simulations(hybrid_sim, "hybrid"),
  summarise_simulations(ecm_sim, "ardl_ecm"),
  summarise_simulations(univariate_sim, "univariate_arima")
), fill = TRUE)

anchor_value <- official[
  year == official_forecast_end,
  expenses_ratio_gdp + net_capital_investment_ratio_gdp
]

fit_window_sensitivity <- function(label, train, future_data) {
  result <- fit_predict_topdown(train, future_data)$paths
  result[, .(
    estimation_window = label,
    estimation_first_year = min(train$year),
    estimation_last_year = max(train$year),
    endpoint_unanchored = value[year == projection_end],
    endpoint_anchored = anchor_value + value[year == projection_end] - value[year == official_forecast_end]
  ), by = model]
}

window_sensitivity <- rbindlist(list(
  fit_window_sensitivity("Full sample", historical, future),
  fit_window_sensitivity("Starts 1990", historical[year >= 1990], future),
  fit_window_sensitivity(
    "Estimated through 2019",
    historical[year <= 2019],
    rbindlist(list(historical[year >= 2020], future), use.names = TRUE, fill = TRUE)
  )
))
window_sensitivity[, model_label := unname(model_labels[model])]
baseline_end <- window_sensitivity[estimation_window == "Full sample",
                                   .(model, baseline_endpoint = endpoint_anchored)]
window_sensitivity <- merge(window_sensitivity, baseline_end, by = "model")
window_sensitivity[, endpoint_difference_from_full_sample_pp :=
                     (endpoint_anchored - baseline_endpoint) * 100]

driver_scenarios <- list(
  Baseline = copy(future),
  `Older population (+1 pp aged 65+)` = copy(future),
  `Unemployment +1 pp` = copy(future),
  `Terms of trade +1 SD` = copy(future),
  `Relative government prices +1 SD` = copy(future)
)
ramp <- pmin(pmax((future$year - 2029) / 6, 0), 1)
driver_scenarios[["Older population (+1 pp aged 65+)"]][, `:=`(
  `65p` = `65p` + 0.01 * ramp,
  `35_54` = `35_54` - 0.01 * ramp
)]
driver_scenarios[["Unemployment +1 pp"]][, unemployment := unemployment + ramp]
driver_scenarios[["Terms of trade +1 SD"]][, tot_z := tot_z + ramp]
driver_scenarios[["Relative government prices +1 SD"]][, rp_z := rp_z + ramp]

driver_sensitivity <- rbindlist(lapply(names(driver_scenarios), function(label) {
  result <- fit_predict_topdown(historical, driver_scenarios[[label]])$paths
  result[, .(
    driver_scenario = label,
    endpoint_unanchored = value[year == projection_end],
    endpoint_anchored = anchor_value + value[year == projection_end] - value[year == official_forecast_end]
  ), by = model]
}))
driver_sensitivity[, model_label := unname(model_labels[model])]
driver_baseline <- driver_sensitivity[driver_scenario == "Baseline",
                                     .(model, baseline_endpoint = endpoint_anchored)]
driver_sensitivity <- merge(driver_sensitivity, driver_baseline, by = "model")
driver_sensitivity[, endpoint_effect_pp := (endpoint_anchored - baseline_endpoint) * 100]

diagnostic_scope <- data.table(
  approach = c("Top-down estimated models", "Bottom-up calibrated projection"),
  model_fit = c(
    "In-sample level fit, residual autocorrelation and normality diagnostics; supplemented by rolling forecast errors",
    "No conventional in-sample statistical fit: categories are calibrated to GFS and scaled to the official forecast; raw forecast-period differences are reported"
  ),
  sensitivity = c(
    "Alternative estimation windows and four controlled driver shocks",
    "Central, pressure and restraint assumption scenarios plus revenue sensitivities"
  ),
  standard_errors = c(
    "Coefficient standard errors and conditional model-only forecast intervals",
    "Not applicable without estimated category equations; scenario spread is not a confidence interval"
  )
)

rolling_selection <- rolling_checks[, .(
  rolling_rmse_1y_pp = rmse_pp[horizon == 1L],
  rolling_rmse_5y_pp = rmse_pp[horizon == 5L],
  rolling_rmse_average_pp = mean(rmse_pp)
), by = model]
official_selection <- official_checks[approach == "Top-down model-only", .(
  model, official_period_mae_pp = mae_difference_pp
)]
window_selection <- window_sensitivity[estimation_window != "Full sample", .(
  maximum_endpoint_window_shift_pp = max(abs(endpoint_difference_from_full_sample_pp))
), by = model]
interval_selection <- forecast_uncertainty[year == projection_end, .(
  model, conditional_95_interval_width_pp = (upper_95 - lower_95) * 100
)]

model_selection_assessment <- Reduce(
  function(x, y) merge(x, y, by = "model", all = TRUE),
  list(
    fit_diagnostics[, .(model, model_label, r_squared, in_sample_rmse_pp = rmse_pp,
                        ljung_box_p_value)],
    rolling_selection, official_selection, window_selection, interval_selection
  )
)
model_selection_assessment[, `:=`(
  preferred_role = fcase(
    model == "structural_ols", "Transparent structural interpretation and Shapley attribution",
    model == "arimax_level", "Short-to-medium-run statistical and residual-dynamics cross-check",
    model == "arimax_diff", "Difference-based sensitivity check where permanent level relationships are doubtful",
    model == "dynamic_diff", "Variable-specific difference-model cross-check",
    model == "hybrid", "Structural/macro top-down long-run cross-check",
    model == "ardl_ecm", "Experimental long-run adjustment sensitivity",
    model == "univariate_arima", "Near-term COVID-adjusted benchmark only"
  ),
  selection_comment = fcase(
    model == "structural_ols", "Interpretable but residual autocorrelation and high estimation-window sensitivity weaken it as a standalone forecast.",
    model == "arimax_level", "Strong in-sample dynamic fit and well-behaved residual autocorrelation, but weaker rolling forecasts and material endpoint sensitivity caution against long-run primacy.",
    model == "arimax_diff", "Reasonable fit and residual behaviour, but accumulated changes create widening long-horizon uncertainty and high sample sensitivity.",
    model == "dynamic_diff", "Uses economically tailored transformations and intervention pulses; accumulated changes can still widen long-horizon uncertainty.",
    model == "hybrid", "Separates slow-moving structural pressures from shorter-run macro impulses; accumulated-error uncertainty remains relevant.",
    model == "ardl_ecm", "Separates long-run levels from short-run dynamics, but requires supporting integration, bounds, residual and stability tests before it can be treated as a valid long-run model.",
    model == "univariate_arima", "Best short-horizon benchmark, but its flat path cannot respond to ageing, prices, unemployment or policy and is unsuitable as the central long-run projection."
  )
)]

approach_recommendation <- data.table(
  horizon_or_use = c(
    "Official forecast period", "Central long-run fiscal projection",
    "Primary top-down long-run cross-check", "Near-term statistical benchmark",
    "Uncertainty and stress testing"
  ),
  recommended_approach = c(
    "Published PBO consolidated forecast",
    "Bottom-up purpose model with endogenous interest and explicit revenue scenarios",
    "Dynamic-difference model, reviewed against the full structural-model ensemble",
    "ARIMA with COVID controls",
    "Bottom-up scenarios plus the spread and sensitivities across all top-down specifications"
  ),
  rationale = c(
    "Contains current policy, budget measures, near-term macro judgement and consolidated fiscal information absent from parsimonious equations.",
    "Maps ageing, service demand, excess costs, defence, revenue and interest feedback into auditable category paths; its assumptions can be replaced directly as policy information improves.",
    "The dynamic-difference model has the strongest five-year rolling result among the driver-based models and the lowest tested endpoint-window sensitivity. The ECM remains an experimental sensitivity because a valid long-run level relationship must be established separately.",
    "Provides the strongest short-horizon statistical benchmark, but no structural response to demographic, economic or policy changes.",
    "No single model captures parameter, specification, policy and macro uncertainty; scenario and model spread are complementary diagnostics rather than probability intervals."
  )
)

fwrite(fit_diagnostics, file.path(table_dir, "model_fit_diagnostics.csv"))
fwrite(coefficient_uncertainty, file.path(table_dir, "model_coefficient_standard_errors.csv"))
fwrite(forecast_uncertainty, file.path(table_dir, "model_conditional_forecast_intervals.csv"))
fwrite(window_sensitivity, file.path(table_dir, "model_estimation_window_sensitivity.csv"))
fwrite(driver_sensitivity, file.path(table_dir, "model_driver_sensitivity.csv"))
fwrite(diagnostic_scope, file.path(table_dir, "model_diagnostic_scope.csv"))
fwrite(model_selection_assessment, file.path(table_dir, "model_selection_assessment.csv"))
fwrite(approach_recommendation, file.path(table_dir, "approach_recommendation.csv"))

message("Model diagnostics written: fit, residual tests, standard errors, conditional intervals and sensitivities.")

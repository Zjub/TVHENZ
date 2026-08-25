source(file.path("scripts", "00_config.R"))
source(file.path("R", "model_functions_2.R"))

historical <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
future <- fread(file.path(processed_dir, "macro_scenario_assumptions.csv"))[scenario == "central"]
official <- fread(file.path(processed_dir, "official_pbo_nfo_wide.csv"))
fits <- readRDS(file.path(model_dir, "top_down_fitted_models.rds"))
dyn <- build_topdown_table(historical, include_y = TRUE)
y <- historical$broad_expenditure_gdp

metric <- function(model, years, actual, fitted, residual, fitdf = 0L) {
  keep <- is.finite(actual) & is.finite(fitted)
  residual <- residual[is.finite(residual)]
  lag <- max(1L, min(5L, length(residual) - 1L))
  data.table(
    model, model_label = unname(model_labels[model]), observations = sum(keep),
    first_fitted_year = min(years[keep]), last_fitted_year = max(years[keep]),
    r_squared = 1 - sum((actual[keep] - fitted[keep])^2) /
      sum((actual[keep] - mean(actual[keep]))^2),
    mae_pp = mean(abs(actual[keep] - fitted[keep])) * 100,
    rmse_pp = sqrt(mean((actual[keep] - fitted[keep])^2)) * 100,
    residual_sd_pp = sd(residual) * 100,
    ljung_box_p_value = Box.test(residual, lag = lag, type = "Ljung-Box", fitdf = fitdf)$p.value
  )
}
arma_df <- function(fit) sum(arimaorder(fit)[c("p", "q", "P", "Q")])

ols_hat <- as.numeric(fitted(fits$structural_ols))
level_hat <- as.numeric(fitted(fits$arimax_level))

diff_terms <- colnames(fits$arimax_diff$xreg)
diff_rows <- complete.cases(dyn[, c("dy", diff_terms), with = FALSE])
diff_dy_hat <- as.numeric(fitted(fits$arimax_diff))
diff_y_hat <- dyn$y_lag[diff_rows] + diff_dy_hat

structural_x <- structural_feature_frame(historical, character())
structural_x <- structural_x[, names(coef(fits$hybrid_structural_ols))[-1L], drop = FALSE]
structural_hat <- as.numeric(predict(fits$hybrid_structural_ols, structural_x))
d_structural <- c(NA_real_, diff(structural_hat))
hybrid_terms <- colnames(fits$hybrid_macro$xreg)
hybrid_rows <- complete.cases(dyn[, c("dy", hybrid_terms), with = FALSE])
hybrid_change_hat <- d_structural[hybrid_rows] + as.numeric(fitted(fits$hybrid_macro))
hybrid_y_hat <- dyn$y_lag[hybrid_rows] + hybrid_change_hat

ecm_terms <- setdiff(all.vars(formula(fits$ardl_ecm)), "dw")
ecm_rows <- complete.cases(dyn[, c("dw", ecm_terms), with = FALSE])
ecm_dw_hat <- as.numeric(fitted(fits$ardl_ecm))
ecm_y_hat <- exp(dyn$w_lag[ecm_rows] + ecm_dw_hat)

fit_diagnostics <- rbindlist(list(
  metric("structural_ols", historical$year, y, ols_hat, residuals(fits$structural_ols)),
  metric("arimax_level", historical$year, y, level_hat, residuals(fits$arimax_level), arma_df(fits$arimax_level)),
  metric("arimax_diff", dyn$year[diff_rows], dyn$y[diff_rows], diff_y_hat,
         dyn$dy[diff_rows] - diff_dy_hat, arma_df(fits$arimax_diff)),
  metric("hybrid", dyn$year[hybrid_rows], dyn$y[hybrid_rows], hybrid_y_hat,
         dyn$dy[hybrid_rows] - hybrid_change_hat, arma_df(fits$hybrid_macro)),
  metric("ardl_ecm", dyn$year[ecm_rows], dyn$y[ecm_rows], ecm_y_hat,
         residuals(fits$ardl_ecm))
))

anchor <- official[year == official_forecast_end,
                   official_topdown_expenditure(.SD)]
window_specs <- list(
  `Full sample` = list(train = historical, future = future),
  `Starts 1990` = list(train = historical[year >= 1990], future = future),
  `Estimated through 2019` = list(
    train = historical[year <= 2019],
    future = rbindlist(list(historical[year >= 2020], future), fill = TRUE)
  )
)
window_sensitivity <- rbindlist(lapply(names(window_specs), function(label) {
  item <- window_specs[[label]]
  result <- fit_predict_topdown(item$train, item$future)$paths
  result[, .(
    estimation_window = label,
    estimation_first_year = min(item$train$year),
    estimation_last_year = max(item$train$year),
    endpoint_unanchored = value[year == projection_end],
    endpoint_anchored = anchor + value[year == projection_end] - value[year == official_forecast_end]
  ), by = model]
}))
window_sensitivity[, model_label := unname(model_labels[model])]
baseline <- window_sensitivity[estimation_window == "Full sample",
                               .(model, baseline_endpoint = endpoint_anchored)]
window_sensitivity <- merge(window_sensitivity, baseline, by = "model")
window_sensitivity[, endpoint_difference_from_full_sample_pp :=
                     (endpoint_anchored - baseline_endpoint) * 100]

# Compatibility form used by the general graph suite.
four_window <- window_sensitivity[, .(
  window = estimation_window, model, model_label,
  endpoint_percent_gdp = endpoint_unanchored * 100
)]

design_check <- function(x, model, component) {
  x <- as.data.frame(x)
  x <- x[, vapply(x, function(z) sd(z, na.rm = TRUE) > 0, logical(1)), drop = FALSE]
  x <- x[complete.cases(x), , drop = FALSE]
  if (ncol(x) < 2L) return(data.table(model, component, maximum_vif = 1, condition_number = 1))
  vifs <- vapply(seq_len(ncol(x)), function(j) {
    1 / (1 - summary(lm(x[[j]] ~ ., data = x[, -j, drop = FALSE]))$r.squared)
  }, numeric(1))
  data.table(model, component, maximum_vif = max(vifs),
             condition_number = kappa(scale(as.matrix(x)), exact = TRUE))
}
collinearity <- rbindlist(list(
  design_check(model.matrix(fits$structural_ols)[, -1L, drop = FALSE], "structural_ols", "level"),
  design_check(fits$arimax_level$xreg, "arimax_level", "level xreg"),
  design_check(fits$arimax_diff$xreg, "arimax_diff", "difference xreg"),
  design_check(model.matrix(fits$hybrid_structural_ols)[, -1L, drop = FALSE], "hybrid", "structural level"),
  design_check(fits$hybrid_macro$xreg, "hybrid", "macro changes"),
  design_check(model.matrix(fits$ardl_ecm)[, -1L, drop = FALSE], "ardl_ecm", "UECM")
))
collinearity[, model_label := unname(model_labels[model])]

fwrite(fit_diagnostics, file.path(table_dir, "model_fit_diagnostics.csv"))
fwrite(window_sensitivity, file.path(table_dir, "model_estimation_window_sensitivity.csv"))
fwrite(four_window, file.path(table_dir, "four_model_window_sensitivity.csv"))
fwrite(collinearity, file.path(table_dir, "model_collinearity_diagnostics.csv"))
message("Diagnostics written for the five reported top-down models.")

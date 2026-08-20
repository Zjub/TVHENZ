source(file.path("scripts", "00_config.R"))
source(file.path("R", "model_functions.R"))

historical <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
future <- fread(file.path(processed_dir, "macro_scenario_assumptions.csv"))[scenario == "central"]
official <- fread(file.path(processed_dir, "official_pbo_nfo_wide.csv"))

fit <- fit_predict_topdown(historical, future)
paths <- fit$paths
paths[, model_label := unname(model_labels[model])]
paths[, path_type := "Unanchored model projection"]

official_path <- official[year >= projection_start & year <= official_forecast_end,
                          .(year, official_value = expenses_ratio_gdp)]

anchored <- paths[, {
  model_at_join <- value[year == official_forecast_end]
  if (!length(model_at_join)) model_at_join <- value[which.min(abs(year - official_forecast_end))]
  join_value <- official_path[year == official_forecast_end, official_value]
  data.table(
    year = year,
    value = fifelse(
      year <= official_forecast_end,
      official_path$official_value[match(year, official_path$year)],
      join_value + value - model_at_join
    )
  )
}, by = .(model, model_label)]
anchored[, path_type := "Official forecast then model projection"]

historical_out <- historical[, .(
  year, model = "historical", model_label = "Historical national-accounts concept",
  value = broad_expenditure_gdp, path_type = "Historical"
)]
official_out <- official_path[, .(
  year, model = "official", model_label = "Official PBO forecast",
  value = official_value, path_type = "Official forecast"
)]

all_paths <- rbindlist(list(historical_out, official_out, paths, anchored), fill = TRUE)

coefficients <- rbindlist(list(
  data.table(model = "structural_ols", term = names(coef(fit$fits$structural_ols)), estimate = coef(fit$fits$structural_ols)),
  data.table(model = "arimax_level", term = names(coef(fit$fits$arimax_level)), estimate = coef(fit$fits$arimax_level)),
  data.table(model = "arimax_diff", term = names(coef(fit$fits$arimax_diff)), estimate = coef(fit$fits$arimax_diff)),
  data.table(model = "hybrid_demographic", term = names(coef(fit$fits$demographic_ols)), estimate = coef(fit$fits$demographic_ols)),
  data.table(model = "hybrid_macro", term = names(coef(fit$fits$hybrid_macro)), estimate = coef(fit$fits$hybrid_macro)),
  data.table(model = "univariate_arima", term = names(coef(fit$fits$univariate_arima)), estimate = coef(fit$fits$univariate_arima))
), fill = TRUE)

saveRDS(fit$fits, file.path(model_dir, "top_down_fitted_models.rds"))
fwrite(all_paths, file.path(table_dir, "top_down_projection_paths.csv"))
fwrite(fit$metadata, file.path(table_dir, "top_down_model_summary.csv"))
fwrite(coefficients, file.path(table_dir, "top_down_model_coefficients.csv"))
message("Five top-down model paths written, with unanchored and official-anchored variants.")

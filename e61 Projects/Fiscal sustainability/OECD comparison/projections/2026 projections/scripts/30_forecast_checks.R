source(file.path("scripts", "00_config.R"))
source(file.path("R", "model_functions_2.R"))

historical <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
official <- fread(file.path(processed_dir, "official_pbo_nfo_wide.csv"))
topdown_paths <- fread(file.path(table_dir, "top_down_projection_paths.csv"))
bottomup_totals <- fread(file.path(table_dir, "bottom_up_total_projections.csv"))

rolling <- list()
counter <- 0L
for (origin in forecast_check_start:forecast_check_end) {
  train <- historical[year <= origin]
  future <- historical[year > origin & year <= origin + forecast_check_horizon]
  if (nrow(train) < 25 || !nrow(future)) next
  fit <- tryCatch(fit_predict_topdown(train, future), error = function(e) NULL)
  if (is.null(fit)) next
  counter <- counter + 1L
  rolling[[counter]] <- merge(
    fit$paths,
    future[, .(year, actual = broad_expenditure_gdp)],
    by = "year"
  )[, `:=`(
    origin = origin,
    horizon = year - origin,
    error = value - actual
  )]
}
rolling <- rbindlist(rolling, fill = TRUE)
rolling[, model_label := unname(model_labels[model])]

rolling_summary <- rolling[, .(
  n = .N,
  mean_error_pp = mean(error) * 100,
  mae_pp = mean(abs(error)) * 100,
  rmse_pp = sqrt(mean(error^2)) * 100
), by = .(model, model_label, horizon)]

official_compare <- topdown_paths[
  path_type == "Unanchored model projection" & year <= official_forecast_end
]
official_compare <- merge(
  official_compare,
  official[, .(
    year,
    official_value = official_topdown_expenditure(.SD)
  )],
  by = "year"
)
official_compare[, `:=`(
  difference_pp = (value - official_value) * 100,
  approach = "Top-down model-only"
)]

bottom_compare <- merge(
  bottomup_totals[scenario == "central" & year >= projection_start & year <= official_forecast_end],
  official[, .(
    year,
    official_value = expenses_ratio_gdp,
    official_interest = public_debt_interest_ratio_gdp
  )],
  by = "year"
)
bottom_compare[, `:=`(
  value = raw_primary_expense_ratio + official_interest,
  difference_pp = (raw_primary_expense_ratio + official_interest - official_value) * 100,
  model = "bottom_up_unanchored",
  model_label = "Bottom-up model-only",
  approach = "Bottom-up model-only",
  path_type = "Unanchored model projection"
)]

official_compare_all <- rbindlist(list(
  official_compare[, .(year, model, model_label, value, official_value, difference_pp, approach)],
  bottom_compare[, .(year, model, model_label, value, official_value, difference_pp, approach)]
), fill = TRUE)

official_summary <- official_compare_all[, .(
  forecast_years = .N,
  mean_difference_pp = mean(difference_pp),
  mae_difference_pp = mean(abs(difference_pp)),
  max_abs_difference_pp = max(abs(difference_pp))
), by = .(model, model_label, approach)]

fwrite(rolling, file.path(table_dir, "forecast_check_rolling_predictions.csv"))
fwrite(rolling_summary, file.path(table_dir, "forecast_check_rolling_summary.csv"))
fwrite(official_compare_all, file.path(table_dir, "forecast_check_against_official_paths.csv"))
fwrite(official_summary, file.path(table_dir, "forecast_check_against_official_summary.csv"))

message("Forecast checks written: rolling historical tests and model-only versus official forecast comparisons.")

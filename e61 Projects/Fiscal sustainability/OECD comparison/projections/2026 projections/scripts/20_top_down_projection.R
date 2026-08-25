source(file.path("scripts", "00_config.R"))
source(file.path("R", "model_functions_2.R"))

historical <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
future <- fread(file.path(processed_dir, "macro_scenario_assumptions.csv"))[scenario == "central"]
official <- fread(file.path(processed_dir, "official_pbo_nfo_wide.csv"))

fit <- fit_predict_topdown(historical, future)
paths <- copy(fit$paths)
paths[, model_label := unname(model_labels[model])]
paths[, path_type := "Unanchored model projection"]

official_path <- official[
  year >= projection_start & year <= official_forecast_end,
  .(year, official_value = official_topdown_expenditure(.SD))
]
anchored <- paths[, {
  model_join <- value[year == official_forecast_end]
  official_join <- official_path[year == official_forecast_end, official_value]
  data.table(
    year,
    value = fifelse(
      year <= official_forecast_end,
      official_path$official_value[match(year, official_path$year)],
      official_join + value - model_join
    )
  )
}, by = .(model, model_label)]
anchored[, path_type := "Official forecast then model projection"]

historical_out <- historical[, .(
  year, model = "historical", model_label = "Historical national-accounts concept",
  value = broad_expenditure_gdp, path_type = "Historical",
  interest_treatment = topdown_interest_treatment,
  interest_treatment_label = topdown_interest_treatment_label()
)]
official_out <- official_path[, .(
  year, model = "official", model_label = paste0(
    "PBO concept: ", topdown_interest_treatment_label()
  ),
  value = official_value, path_type = "Official forecast"
)]
all_paths <- rbindlist(list(historical_out, official_out, paths, anchored), fill = TRUE)

anchor_comparison <- merge(
  paths[year == official_forecast_end, .(model, model_label, model_only_join_value = value)],
  paths[year == projection_end, .(model, model_label, model_only_endpoint = value)], by = c("model", "model_label")
)
anchor_comparison <- merge(
  anchor_comparison,
  anchored[year == projection_end, .(model, pbo_anchored_endpoint = value)], by = "model"
)
anchor_comparison[, `:=`(
  pbo_join_year = official_forecast_end,
  pbo_join_concept_aligned_value = official_path[year == official_forecast_end, official_value],
  additive_splice_adjustment_pp =
    (official_path[year == official_forecast_end, official_value] - model_only_join_value) * 100,
  endpoint_difference_pp = (pbo_anchored_endpoint - model_only_endpoint) * 100
)]

coefficients <- rbindlist(lapply(names(fit$fits), function(component) {
  values <- coef(fit$fits[[component]])
  data.table(component, term = names(values), estimate = as.numeric(values))
}), fill = TRUE)
summary_table <- copy(fit$metadata)
summary_table[, model_label := unname(model_labels[model])]
ecm_b <- coef(fit$fits$ardl_ecm)
ecm_lambda <- unname(ecm_b["w_lag"])
ecm_long_run <- data.table(
  term = c("intercept", "log_real_gdp_per_capita", "log_relative_government_price"),
  estimate = -unname(ecm_b[c("(Intercept)", "log_real_gdp_per_capita_l1",
                            "log_relative_gov_price_l1")]) / ecm_lambda,
  adjustment = ecm_lambda
)

fwrite(all_paths, file.path(table_dir, "top_down_projection_paths.csv"))
fwrite(anchor_comparison, file.path(table_dir, "top_down_anchor_comparison.csv"))
fwrite(coefficients, file.path(table_dir, "top_down_model_coefficients.csv"))
fwrite(summary_table, file.path(table_dir, "top_down_model_summary.csv"))
fwrite(ecm_long_run, file.path(table_dir, "top_down_ecm_long_run.csv"))
saveRDS(fit$fits, file.path(model_dir, "top_down_fitted_models.rds"))

message("Top-down projection written for the five reported model families.")

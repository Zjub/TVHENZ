source(file.path("scripts", "00_config.R"))
source(file.path("R", "model_functions_2.R"))

historical <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
future <- fread(file.path(processed_dir, "macro_scenario_assumptions.csv"))[scenario == "central"]
official <- fread(file.path(processed_dir, "official_pbo_nfo_wide.csv"))

fit <- fit_predict_topdown(historical, future)
paths <- fit$paths
paths[, model_label := unname(model_labels[model])]
paths[, path_type := "Unanchored model projection"]

official_path <- official[
  year >= projection_start & year <= official_forecast_end,
  .(
    year,
    official_expenses_value = expenses_ratio_gdp,
    official_net_capital_value = net_capital_investment_ratio_gdp,
    official_value = expenses_ratio_gdp + net_capital_investment_ratio_gdp
  )
]

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
  year, model = "official", model_label = "PBO expenses plus net capital investment",
  value = official_value, path_type = "Official forecast"
)]

all_paths <- rbindlist(list(historical_out, official_out, paths, anchored), fill = TRUE)

# Reconciliation table for the alternative model-only path and the additive
# PBO splice. A negative splice adjustment means the PBO join level is below
# the model-only level in the join year.
latest_common_year <- max(intersect(historical$year, official$year))
latest_na_value <- historical[year == latest_common_year, broad_expenditure_gdp]
latest_pbo_expenses_value <- official[year == latest_common_year, expenses_ratio_gdp]
latest_pbo_net_capital_value <- official[
  year == latest_common_year, net_capital_investment_ratio_gdp
]
latest_pbo_value <- latest_pbo_expenses_value + latest_pbo_net_capital_value
pbo_join_expenses_value <- official[
  year == official_forecast_end, expenses_ratio_gdp
]
pbo_join_net_capital_value <- official[
  year == official_forecast_end, net_capital_investment_ratio_gdp
]
pbo_join_value <- pbo_join_expenses_value + pbo_join_net_capital_value

anchor_comparison <- merge(
  paths[year == official_forecast_end, .(
    model, model_label, model_only_join_value = value
  )],
  paths[year == projection_end, .(
    model, model_only_endpoint = value
  )],
  by = "model"
)
anchor_comparison <- merge(
  anchor_comparison,
  anchored[year == projection_end, .(
    model, pbo_anchored_endpoint = value
  )],
  by = "model"
)
anchor_comparison[, `:=`(
  latest_common_year = latest_common_year,
  latest_national_accounts_value = latest_na_value,
  latest_pbo_expenses_value = latest_pbo_expenses_value,
  latest_pbo_net_capital_value = latest_pbo_net_capital_value,
  latest_pbo_concept_aligned_value = latest_pbo_value,
  latest_concept_gap_pp = (latest_na_value - latest_pbo_value) * 100,
  pbo_join_year = official_forecast_end,
  pbo_join_expenses_value = pbo_join_expenses_value,
  pbo_join_net_capital_value = pbo_join_net_capital_value,
  pbo_join_concept_aligned_value = pbo_join_value,
  additive_splice_adjustment_pp = (pbo_join_value - model_only_join_value) * 100,
  endpoint_difference_pp = (pbo_anchored_endpoint - model_only_endpoint) * 100
)]

coefficients <- rbindlist(list(
  data.table(model = "structural_ols", term = names(coef(fit$fits$structural_ols)), estimate = coef(fit$fits$structural_ols)),
  data.table(model = "arimax_level", term = names(coef(fit$fits$arimax_level)), estimate = coef(fit$fits$arimax_level)),
  data.table(model = "arimax_diff", term = names(coef(fit$fits$arimax_diff)), estimate = coef(fit$fits$arimax_diff)),
  data.table(model = "dynamic_diff", term = names(coef(fit$fits$dynamic_diff)), estimate = coef(fit$fits$dynamic_diff)),
  data.table(model = "hybrid_structural", term = names(coef(fit$fits$hybrid_structural_ols)), estimate = coef(fit$fits$hybrid_structural_ols)),
  data.table(model = "hybrid_macro", term = names(coef(fit$fits$hybrid_macro)), estimate = coef(fit$fits$hybrid_macro)),
  data.table(model = "ardl_ecm", term = names(coef(fit$fits$ardl_ecm)), estimate = coef(fit$fits$ardl_ecm)),
  data.table(model = "univariate_arima", term = names(coef(fit$fits$univariate_arima)), estimate = coef(fit$fits$univariate_arima))
), fill = TRUE)

saveRDS(fit$fits, file.path(model_dir, "top_down_fitted_models.rds"))
fwrite(all_paths, file.path(table_dir, "top_down_projection_paths.csv"))
fwrite(fit$metadata, file.path(table_dir, "top_down_model_summary.csv"))
fwrite(coefficients, file.path(table_dir, "top_down_model_coefficients.csv"))
fwrite(fit$diagnostics$ecm_long_run, file.path(table_dir, "top_down_ecm_long_run.csv"))
fwrite(anchor_comparison, file.path(table_dir, "top_down_anchor_comparison.csv"))
message("Seven top-down model paths written, with unanchored and official-anchored variants.")

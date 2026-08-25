# Consolidate the deliberately limited five-model evidence set. The historical
# filename is retained so existing automation does not need another rename.
source(file.path("scripts", "00_config.R"))

fit <- fread(file.path(table_dir, "model_fit_diagnostics.csv"))
rolling <- fread(file.path(table_dir, "forecast_check_rolling_summary.csv"))
windows <- fread(file.path(table_dir, "model_estimation_window_sensitivity.csv"))
paths <- fread(file.path(table_dir, "top_down_projection_paths.csv"))
anchors <- fread(file.path(table_dir, "top_down_anchor_comparison.csv"))

rolling_overall <- rolling[, .(
  average_rolling_rmse_pp = mean(rmse_pp),
  rolling_rmse_1y_pp = rmse_pp[horizon == 1L],
  rolling_rmse_5y_pp = rmse_pp[horizon == 5L]
), by = .(model, model_label)]
assessment <- merge(
  fit[, .(model, model_label, in_sample_rmse_pp = rmse_pp, in_sample_r_squared = r_squared,
          residual_ljung_box_p = ljung_box_p_value)],
  rolling_overall, by = c("model", "model_label"), all = TRUE
)
assessment <- merge(
  assessment,
  anchors[, .(model, model_only_endpoint = model_only_endpoint * 100,
              pbo_anchored_endpoint = pbo_anchored_endpoint * 100)], by = "model"
)
window_range <- windows[, .(
  maximum_window_shift_pp = max(abs(endpoint_difference_from_full_sample_pp), na.rm = TRUE)
), by = model]
assessment <- merge(assessment, window_range, by = "model")

endpoint_table <- paths[year == projection_end & path_type %in%
                          c("Unanchored model projection", "Official forecast then model projection")]
endpoint_table[, endpoint_percent_gdp := value * 100]

fwrite(assessment, file.path(table_dir, "model_selection_assessment.csv"))
fwrite(endpoint_table, file.path(table_dir, "five_model_endpoints.csv"))
# Compatibility outputs consumed by older report code.
fwrite(fit, file.path(table_dir, "four_model_in_sample_fit.csv"))
fwrite(rolling, file.path(table_dir, "four_model_rolling_fit.csv"))
message("Five-model assessment table written.")

source(file.path("scripts", "00_config.R"))
suppressPackageStartupMessages({library(ggplot2); library(data.table)})

paths <- fread(file.path(table_dir, "top_down_projection_paths.csv"))
rolling <- fread(file.path(table_dir, "forecast_check_rolling_summary.csv"))
historical <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
out_dir <- file.path(figure_dir, "five_model")
make_dirs(c(out_dir, file.path(documentation_dir, "figures")))
theme_note <- theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

projection_plot <- function(selected_path_type, title, subtitle) {
  ggplot() +
    geom_line(data = historical[year >= 2000],
              aes(year, broad_expenditure_gdp * 100), colour = "black", linewidth = 0.9) +
    geom_line(data = paths[path_type == selected_path_type],
              aes(year, value * 100, colour = model_label), linewidth = 0.9) +
    scale_colour_manual(values = palette_models) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "% of GDP", colour = NULL) + theme_note
}
p_model <- projection_plot(
  "Unanchored model projection", "Five top-down projections from the latest actual",
  "Common demographic and macro assumptions; no PBO spending anchor"
)
p_anchor <- projection_plot(
  "Official forecast then model projection", "Five top-down projections with the PBO near-term anchor",
  paste0("Matching PBO treatment through 2029-30: ", topdown_interest_treatment_label())
)
p_rolling <- ggplot(rolling, aes(horizon, rmse_pp, colour = model_label)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  scale_colour_manual(values = palette_models) + scale_x_continuous(breaks = 1:5) +
  labs(title = "Rolling conditional forecast performance",
       subtitle = "Origins 2009-10 to 2019-20; lower RMSE is better",
       x = "Forecast horizon (years)", y = "RMSE (percentage points)", colour = NULL) + theme_note

save_one <- function(plot, name) {
  ggsave(file.path(out_dir, paste0(name, ".png")), plot, width = 9, height = 5.5, dpi = 300)
  ggsave(file.path(documentation_dir, "figures", paste0(name, ".png")),
         plot, width = 9, height = 5.5, dpi = 300)
}
save_one(p_model, "five_model_paths_model_only")
save_one(p_anchor, "five_model_paths_pbo_anchor")
save_one(p_rolling, "five_model_rolling_rmse")
message("Five-model figures written.")

source(file.path("scripts", "00_config.R"))

comparison <- fread(file.path(table_dir, "bottom_up_method_comparison.csv"))
category_comparison <- fread(file.path(table_dir, "bottom_up_method_category_comparison.csv"))
calibration <- fread(file.path(table_dir, "bottom_up_data_enhanced_calibration.csv"))

paths <- rbindlist(list(
  comparison[, .(year, scenario, method = "Baseline fixed demographic rates", ratio = baseline_ratio)],
  comparison[, .(year, scenario, method = "Data-enhanced demographic-price", ratio = data_enhanced_ratio)]
))
method_colours <- c(
  "Baseline fixed demographic rates" = "#0072B2",
  "Data-enhanced demographic-price" = "#D55E00"
)

p1 <- ggplot(paths[scenario == "central"], aes(year, ratio * 100, colour = method)) +
  geom_line(linewidth = 1.05) +
  geom_vline(xintercept = official_forecast_end + 0.5, linetype = "dashed", colour = "grey45") +
  scale_colour_manual(values = method_colours) +
  labs(
    title = "Central bottom-up primary-spending estimates",
    subtitle = "Both methods use the official aggregate anchor through 2029-30",
    x = NULL, y = "% of GDP", colour = NULL,
    caption = paste0(
      "Baseline uses fixed demographic exposures and calibrated excess costs. Enhanced uses the same exposures plus ABS public-sector wage-price evidence\n",
      "and shrunk historical residual intensity."
    )
  ) + theme_fiscal()
save_plot_pair(p1, file.path(figure_dir, "bottom_up_comparison", "01_central_paths"), 9.5, 6)

p2 <- ggplot(paths, aes(year, ratio * 100, colour = method)) +
  geom_line(linewidth = 0.95) +
  facet_wrap(~scenario, ncol = 1, scales = "free_y") +
  geom_vline(xintercept = official_forecast_end + 0.5, linetype = "dashed", colour = "grey45") +
  scale_colour_manual(values = method_colours) +
  labs(
    title = "Baseline and data-enhanced estimates across scenarios",
    x = NULL, y = "% of GDP", colour = NULL,
    caption = "Scenario spreads retain the baseline pressure/restraint offsets.\nThe enhanced central calibration is common to all scenarios."
  ) + theme_fiscal()
save_plot_pair(p2, file.path(figure_dir, "bottom_up_comparison", "02_all_scenarios"), 9.5, 8.5)

endpoint <- category_comparison[year == projection_end & scenario == "central"]
p3 <- ggplot(endpoint, aes(reorder(category, difference_pp), difference_pp, fill = difference_pp >= 0)) +
  geom_col(width = 0.72) +
  geom_hline(yintercept = 0, colour = "grey25") +
  coord_flip() +
  scale_fill_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "#0072B2"), guide = "none") +
  labs(
    title = "Category effect of the data-enhanced method in 2065-66",
    subtitle = "Data-enhanced estimate minus baseline, central scenario",
    x = NULL, y = "Percentage points of GDP",
    caption = "Positive bars raise the enhanced aggregate relative to baseline; negative bars lower it."
  ) + theme_fiscal()
save_plot_pair(p3, file.path(figure_dir, "bottom_up_comparison", "03_endpoint_category_differences"), 9.5, 6.5)

cal_long <- melt(
  calibration,
  id.vars = "category",
  measure.vars = c("baseline_central_excess", "enhanced_central_excess"),
  variable.name = "calibration_type", value.name = "annual_rate"
)
cal_long[, calibration_type := fcase(
  calibration_type == "baseline_central_excess", "Baseline",
  default = "Data-enhanced"
)]
p4 <- ggplot(cal_long, aes(annual_rate * 100, reorder(category, annual_rate), colour = calibration_type)) +
  geom_point(size = 2.4, position = position_dodge(width = 0.45)) +
  geom_vline(xintercept = 0, colour = "grey45") +
  scale_colour_manual(values = c(Baseline = "#0072B2", `Data-enhanced` = "#D55E00")) +
  labs(
    title = "Annual category excess-cost calibration",
    subtitle = "Enhanced estimates are shrunk toward the baseline because only six non-COVID changes are available",
    x = "% per year above common economy-wide unit-cost growth", y = NULL, colour = NULL,
    caption = "Defence is excluded because it follows an exogenous target share of GDP."
  ) + theme_fiscal()
save_plot_pair(p4, file.path(figure_dir, "bottom_up_comparison", "04_excess_cost_calibration"), 9.5, 6.5)

message("Bottom-up comparison figures written to: ", file.path(figure_dir, "bottom_up_comparison"))

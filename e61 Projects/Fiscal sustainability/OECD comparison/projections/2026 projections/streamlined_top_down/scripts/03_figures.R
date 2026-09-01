source("config.R")
suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
  library(data.table)
})

theme_set(theme_minimal(base_size = 11) + theme(
  panel.grid.minor = element_blank(),
  legend.position = "bottom",
  plot.title.position = "plot",
  plot.caption = element_text(hjust = 0, colour = "grey35")
))

save_plot <- function(plot, name, width = 9, height = 5.5) {
  ggsave(file.path(figure_dir, paste0(name, ".png")), plot,
         width = width, height = height, dpi = 320)
  ggsave(file.path(figure_dir, paste0(name, ".svg")), plot,
         width = width, height = height)
}

estimation_context <- function(subtitle) {
  if (workflow_is_pre_2020) {
    paste0(subtitle, "\nCoefficients estimated only through FY2018-19")
  } else subtitle
}

inputs <- fread(file.path(table_dir, "model_input_table.csv"))
paths <- fread(file.path(table_dir, "primary_spending_paths.csv"))
interval <- fread(file.path(
  table_dir, "gdp_pc_model_high_growth_parameter_interval.csv"
))
interval_2019 <- fread(file.path(
  table_dir, "gdp_pc_model_2019_anchor_parameter_interval.csv"
))
contributions <- fread(file.path(table_dir, "structural_driver_contributions.csv"))
rolling <- fread(file.path(table_dir, "rolling_fit_by_horizon.csv"))
debt <- fread(file.path(table_dir, "debt_paths.csv"))
debt_effects <- fread(file.path(table_dir, "structural_driver_debt_effects.csv"))
all_anchor_paths <- fread(file.path(table_dir, "all_anchor_primary_spending_paths.csv"))
income_assumptions <- fread(file.path(table_dir, "real_gdp_per_capita_scenarios.csv"))

reported_cases <- c("central_gdp", "midpoint_gdp", "pressure_gdp", "no_income")
case_colours <- c(
  central_gdp = "#0072B2",
  midpoint_gdp = "#009E73",
  pressure_gdp = "#D55E00",
  no_income = "#7A5195"
)
case_linetypes <- c(
  central_gdp = "solid", midpoint_gdp = "longdash",
  pressure_gdp = "dotdash", no_income = "twodash"
)
case_label_table <- unique(paths[, .(model, model_label)])
case_labels <- setNames(case_label_table$model_label, case_label_table$model)
case_labels <- case_labels[reported_cases]
short_labels <- c(
  with_income = "Structural model with GDP per capita",
  no_income = "No GDP-per-capita effect",
  structural_only = "No transitory controls",
  arima_benchmark = "ARIMA benchmark"
)

p1 <- ggplot() +
  geom_line(
    data = inputs[sample == "history"],
    aes(year, spending_pp), colour = "#222222", linewidth = 0.8
  ) +
  geom_ribbon(
    data = interval[year > official_forecast_end],
    aes(year, ymin = lower_80_pp, ymax = upper_80_pp),
    fill = unname(case_colours["central_gdp"]), alpha = 0.14
  ) +
  geom_line(
    data = paths[model %in% reported_cases],
    aes(year, primary_fiscal_pp, colour = model, linetype = model), linewidth = 0.95
  ) +
  geom_vline(xintercept = official_forecast_end, linetype = "dashed", colour = "grey45") +
  scale_colour_manual(
    values = case_colours,
    labels = case_labels
  ) +
  scale_linetype_manual(values = case_linetypes, labels = case_labels) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Primary fiscal expenditure under four real-GDP treatments",
    subtitle = estimation_context(
      "PBO anchor through FY2029-30; all non-GDP projection assumptions held constant"
    ),
    x = NULL, y = "% of GDP",
    colour = NULL, linetype = NULL,
    caption = paste0(
      "Shading is the ", case_labels[["central_gdp"]],
      " case's 80% coefficient-uncertainty interval; it excludes uncertainty in future drivers and policy."
    )
  )
save_plot(p1, "01_gdp_sensitivity_primary_spending")

group_colours <- c(
  "Age composition" = "#0072B2",
  "Relative government prices" = "#D55E00",
  "Real GDP per capita" = "#009E73",
  "Transitory macro bridge" = "#CC79A7",
  "COVID interventions" = "#777777"
)
p2 <- ggplot(
  contributions,
  aes(year, cumulative_contribution_pp, colour = group)
) +
  geom_hline(yintercept = 0, colour = "grey65") +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = group_colours) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = paste0("Structural driver contributions under ", case_labels[["central_gdp"]]),
    subtitle = estimation_context(
      "Cumulative contribution relative to the FY2029-30 PBO anchor"
    ),
    x = NULL, y = "Percentage-point contribution to spending/GDP", colour = NULL
  )
save_plot(p2, "02_structural_driver_contributions")

p3 <- ggplot(
  rolling,
  aes(horizon, rmse_pp, colour = model, group = model)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_colour_manual(values = model_colours, labels = short_labels) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_x_continuous(breaks = 1:rolling_horizon) +
  labs(
    title = "Conditional rolling forecast performance",
    subtitle = estimation_context(paste0(
      "Expanding-window origins ", rolling_start, "-", rolling_end,
      "; realised drivers supplied to each model"
    )),
    x = "Forecast horizon (years)", y = "RMSE (percentage points of GDP)", colour = NULL
  )
save_plot(p3, "03_rolling_rmse")

debt_plot_data <- debt[model %in% reported_cases & anchor_type == "pbo", .(
  year, model, model_label,
  `Primary fiscal expenditure` = 100 * primary_fiscal_ratio,
  `Total fiscal expenditure including PDI` = 100 * total_fiscal_expenditure_ratio,
  `Net debt` = 100 * net_debt_ratio
)]
debt_plot_data <- melt(
  debt_plot_data,
  id.vars = c("year", "model", "model_label"),
  variable.name = "measure", value.name = "value"
)
p4 <- ggplot(
  debt_plot_data[year >= 2026],
  aes(year, value, colour = model)
) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_line(linewidth = 0.9) +
  facet_wrap(~measure, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = case_colours, labels = case_labels) +
  labs(
    title = "GDP sensitivity in spending flows mechanically into public debt",
    subtitle = estimation_context(
      "Central inflation, revenue and interest assumptions; nominal GDP follows each real-GDP path"
    ),
    x = NULL, y = "% of GDP", colour = NULL
  )
save_plot(p4, "04_gdp_sensitivity_spending_and_debt", width = 9, height = 9)

income_plot <- income_assumptions[year >= official_forecast_end]
income_plot[, index_2030 := 100 * exp(
  log_real_gdp_per_capita - log_real_gdp_per_capita[year == official_forecast_end]
), by = income_case]
p5 <- ggplot(income_plot, aes(year, index_2030, colour = income_case)) +
  geom_hline(yintercept = 100, colour = "grey70") +
  geom_line(linewidth = 1) +
  scale_colour_manual(
    values = case_colours[names(case_colours) != "no_income"],
    labels = case_labels[names(case_labels) != "no_income"]
  ) +
  labs(
    title = "Real GDP per capita paths used in the sensitivity cases",
    subtitle = estimation_context(
      "Index: FY2029-30 = 100; the middle path is halfway in log levels"
    ),
    x = NULL, y = "Real GDP per capita index", colour = NULL
  )
save_plot(p5, "05_structural_driver_paths")

p6 <- ggplot(
  debt_effects,
  aes(reorder(contribution_group, driver_effect_on_net_debt_2066_pp),
      driver_effect_on_net_debt_2066_pp,
      fill = contribution_group)
) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = group_colours) +
  labs(
    title = "Contribution of each driver to net debt in 2065-66",
    subtitle = estimation_context(paste0(
      case_labels[["central_gdp"]],
      " minus a counterfactual holding that contribution at the FY2029-30 anchor"
    )),
    x = NULL, y = "Percentage-point effect on net debt/GDP"
  )
save_plot(p6, "06_structural_driver_effects_on_debt")

anchor_colours <- c(
  "PBO forecast anchor" = "#0072B2",
  "Latest actual anchor" = "#D55E00",
  "FY2018-19 actual anchor" = "#009E73"
)
p7 <- ggplot() +
  geom_line(
    data = inputs[sample == "history"],
    aes(year, spending_pp), colour = "#333333", linewidth = 0.75
  ) +
  geom_line(
    data = all_anchor_paths[model == "central_gdp"],
    aes(year, primary_fiscal_pp, colour = anchor_type), linewidth = 1
  ) +
  geom_vline(xintercept = official_forecast_end, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = anchor_colours) +
  labs(
    title = paste0(case_labels[["central_gdp"]], " under three starting anchors"),
    subtitle = estimation_context(
      "Identical coefficients and long-run driver treatment"
    ),
    x = NULL, y = "% of GDP", colour = NULL,
    caption = "The FY2018-19 and latest-actual paths start from their National Accounts observations; the PBO path uses official estimates through FY2029-30."
  )
save_plot(p7, "07_gdp_pc_model_anchor_comparison")

anchor_debt <- debt[model == "central_gdp", .(
  year, anchor_label,
  `Total fiscal expenditure including PDI` = 100 * total_fiscal_expenditure_ratio,
  `Net debt` = 100 * net_debt_ratio
)]
anchor_debt <- melt(
  anchor_debt,
  id.vars = c("year", "anchor_label"),
  variable.name = "measure", value.name = "value"
)
anchor_debt <- anchor_debt[is.finite(value)]
p8 <- ggplot(anchor_debt[year >= 2025], aes(year, value, colour = anchor_label)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_line(linewidth = 1) +
  facet_wrap(~measure, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = c(
    "PBO forecast through 2029-30" = unname(anchor_colours["PBO forecast anchor"]),
    "Latest National Accounts actual (2024-25)" = unname(anchor_colours["Latest actual anchor"])
  )) +
  labs(
    title = paste0(case_labels[["central_gdp"]], " spending and debt under two anchors"),
    subtitle = estimation_context(
      "Common revenue, nominal-GDP and interest-rate assumptions"
    ),
    x = NULL, y = "% of GDP", colour = NULL
  )
save_plot(p8, "08_gdp_pc_model_anchor_spending_and_debt", height = 7)

actual_model_paths <- all_anchor_paths[
  anchor_type == "Latest actual anchor" & model %in% reported_cases
]
p9 <- ggplot() +
  geom_line(
    data = inputs[sample == "history"],
    aes(year, spending_pp), colour = "#333333", linewidth = 0.75
  ) +
  geom_line(
    data = actual_model_paths,
    aes(year, primary_fiscal_pp, colour = model, linetype = model),
    linewidth = 1
  ) +
  scale_colour_manual(
    values = case_colours,
    labels = case_labels
  ) +
  scale_linetype_manual(
    values = case_linetypes,
    labels = case_labels
  ) +
  labs(
    title = "Four GDP treatments from the latest National Accounts actual",
    subtitle = estimation_context(
      "Common FY2024-25 anchor and non-GDP assumptions"
    ),
    x = NULL, y = "% of GDP", colour = NULL, linetype = NULL,
    caption = "Neither path uses the PBO expenditure projection."
  )
save_plot(p9, "09_latest_actual_gdp_sensitivity")

historical_2019_paths <- all_anchor_paths[
  anchor_type == "FY2018-19 actual anchor" & model %in% reported_cases
]
p10 <- ggplot() +
  geom_line(
    data = inputs[sample == "history"],
    aes(year, spending_pp), colour = "#333333", linewidth = 0.75
  ) +
  geom_ribbon(
    data = interval_2019[year > 2019L],
    aes(year, ymin = lower_80_pp, ymax = upper_80_pp),
    fill = unname(case_colours["central_gdp"]), alpha = 0.14
  ) +
  geom_line(
    data = historical_2019_paths,
    aes(year, primary_fiscal_pp, colour = model, linetype = model),
    linewidth = 1
  ) +
  geom_vline(xintercept = 2019L, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = case_colours, labels = case_labels) +
  scale_linetype_manual(values = case_linetypes, labels = case_labels) +
  labs(
    title = "Four GDP treatments from the FY2018-19 actual",
    subtitle = if (workflow_is_pre_2020) {
      "Coefficients estimated only through FY2018-19; only slow structural changes accumulated thereafter"
    } else {
      "Full-sample coefficients; only slow structural changes accumulated after 2019"
    },
    x = NULL, y = "% of GDP", colour = NULL, linetype = NULL,
    caption = paste0(
      "The paths are anchored before COVID; shading is the ",
      case_labels[["central_gdp"]], " case's 80% coefficient interval."
    )
  )
save_plot(p10, "10_historical_2019_gdp_sensitivity")

expected_figure_stems <- sprintf(
  "%02d_%s",
  1:10,
  c(
    "gdp_sensitivity_primary_spending",
    "structural_driver_contributions",
    "rolling_rmse",
    "gdp_sensitivity_spending_and_debt",
    "structural_driver_paths",
    "structural_driver_effects_on_debt",
    "gdp_pc_model_anchor_comparison",
    "gdp_pc_model_anchor_spending_and_debt",
    "latest_actual_gdp_sensitivity",
    "historical_2019_gdp_sensitivity"
  )
)
expected_figure_files <- unlist(lapply(
  expected_figure_stems,
  function(stem) file.path(figure_dir, paste0(stem, c(".png", ".svg")))
))
if (!all(file.exists(expected_figure_files))) {
  stop("The complete ten-figure PNG/SVG suite was not generated.")
}

message("Streamlined figures written.")

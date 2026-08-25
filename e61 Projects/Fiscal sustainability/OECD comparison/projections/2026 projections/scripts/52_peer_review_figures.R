source(file.path("scripts", "00_config.R"))
check_packages(c("ggplot2", "data.table", "patchwork"))

suppressPackageStartupMessages(library(patchwork))

peer_dir <- file.path(figure_dir, "peer_review")
dir.create(peer_dir, recursive = TRUE, showWarnings = FALSE)

selection <- fread(file.path(table_dir, "model_selection_assessment.csv"))
paths <- fread(file.path(table_dir, "top_down_projection_paths.csv"))
bounds <- fread(file.path(table_dir, "ecm_bounds_tests.csv"))
spec_tests <- fread(file.path(table_dir, "ecm_specification_tests.csv"))
historical <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
official <- fread(file.path(processed_dir, "official_pbo_nfo_wide.csv"))

# The mechanically differenced ARIMAX remains an internal nested benchmark.
# It is not presented as an independent model in the colleague-facing note;
# the dynamic specification is shown as one candidate within the differenced family.
selection <- selection[model != "arimax_diff"]
paths <- paths[model != "arimax_diff"]
selection[model == "dynamic_diff", model_label := "Dynamic differences"]
paths[model == "dynamic_diff", model_label := "Dynamic differences"]
peer_palette <- c(palette_models, `Dynamic differences` = "#56B4E9")

model_order <- selection[order(rolling_rmse_5y_pp), model_label]
rmse_long <- melt(
  selection,
  id.vars = c("model", "model_label"),
  measure.vars = c("rolling_rmse_1y_pp", "rolling_rmse_5y_pp"),
  variable.name = "horizon", value.name = "rmse_pp"
)
rmse_long[, horizon := fcase(
  horizon == "rolling_rmse_1y_pp", "1-year horizon",
  default = "5-year horizon"
)]
rmse_long[, model_label := factor(model_label, levels = rev(model_order))]

p_rmse <- ggplot(rmse_long, aes(rmse_pp, model_label, colour = horizon)) +
  geom_line(aes(group = model_label), colour = "grey75", linewidth = 0.7) +
  geom_point(size = 2.8) +
  scale_colour_manual(values = c("1-year horizon" = "#0072B2", "5-year horizon" = "#D55E00")) +
  labs(
    title = "Rolling conditional forecast RMSE",
    subtitle = "Models are compared without selecting a preferred dynamic specification",
    x = "RMSE (percentage points of GDP)", y = NULL, colour = NULL
  ) + theme_fiscal() +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 8.5))

endpoint <- paths[
  path_type == "Official forecast then model projection" & year == projection_end,
  .(model, model_label, endpoint_pct = value * 100)
]
endpoint <- merge(
  endpoint,
  selection[, .(model, window_shift_pp = maximum_endpoint_window_shift_pp)],
  by = "model"
)
endpoint[, role := fcase(
  model == "dynamic_diff", "Dynamic candidate",
  model == "univariate_arima", "Statistical benchmark",
  model == "ardl_ecm", "Rejected ECM",
  default = "Other driver"
)]
endpoint[, label_hjust := fcase(
  window_shift_pp < 1, 0,
  window_shift_pp > 0.85 * max(window_shift_pp), 1,
  default = 0.5
)]
endpoint_x_max <- max(endpoint$window_shift_pp) * 1.08

p_tradeoff <- ggplot(endpoint, aes(window_shift_pp, endpoint_pct, colour = role)) +
  geom_point(size = 3) +
  geom_text(aes(label = model_label, hjust = label_hjust), size = 2.7,
            check_overlap = TRUE, nudge_y = 0.22, show.legend = FALSE) +
  scale_colour_manual(values = c(
    "Dynamic candidate" = "#009E73",
    "Statistical benchmark" = "#E69F00",
    "Rejected ECM" = "#CC3311",
    "Other driver" = "grey45"
  )) +
  coord_cartesian(xlim = c(-0.55, endpoint_x_max), clip = "off") +
  labs(
    title = "Long-run result versus sample sensitivity",
    subtitle = paste0("Anchored ", projection_end - 1L, "-", substr(projection_end, 3, 4), " endpoint"),
    x = "Maximum endpoint shift across estimation windows (pp)",
    y = "Projected spending (% of GDP)", colour = NULL
  ) + theme_fiscal() +
  theme(legend.position = "bottom", legend.text = element_text(size = 7.5))

p_selection <- p_rmse + p_tradeoff +
  plot_layout(widths = c(1.05, 1)) +
  plot_annotation(
    title = "Evidence for comparing the top-down specifications",
    caption = "Rolling origins 2010--2020 use subsequently observed driver paths. Endpoint sensitivity compares the full sample, a 1990 start, and estimation through 2019."
  )
save_plot_pair(p_selection, file.path(peer_dir, "01_top_down_selection_evidence"), 12, 6.5)

reported_paths <- paths[path_type == "Official forecast then model projection"]
p_paths <- ggplot(reported_paths, aes(year, value * 100, colour = model_label)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = official_forecast_end + 0.5, linetype = "dashed", colour = "grey45") +
  scale_colour_manual(values = peer_palette) +
  labs(
    title = "Reported top-down projections after the PBO anchor",
    subtitle = "Population and real GDP per capita are included in every specification",
    x = NULL, y = "% of GDP", colour = NULL,
    caption = "The mechanically differenced nested benchmark is retained internally but not displayed as an independent model."
  ) + theme_fiscal() + theme(legend.text = element_text(size = 8))
save_plot_pair(p_paths, file.path(peer_dir, "03_top_down_reported_paths"), 10, 6.5)

# Illustrative dynamic projection with and without the PBO spending anchor. The
# model-only path begins at the latest National Accounts outturn and never uses
# the PBO spending-ratio path. Both variants use the same future driver inputs.
latest_year <- max(historical$year)
latest_na <- historical[year == latest_year, broad_expenditure_gdp]
latest_pbo_expenses <- official[year == latest_year, expenses_ratio_gdp]
latest_pbo_net_capital <- official[
  year == latest_year, net_capital_investment_ratio_gdp
]
latest_pbo <- latest_pbo_expenses + latest_pbo_net_capital

history_anchor_plot <- historical[year >= 2019, .(
  year, value = broad_expenditure_gdp,
  series = "National Accounts history"
)]
dynamic_model_only <- rbindlist(list(
  data.table(
    year = latest_year, value = latest_na,
    series = "Dynamic model from latest outturn"
  ),
  paths[
    model == "dynamic_diff" & path_type == "Unanchored model projection",
    .(year, value, series = "Dynamic model from latest outturn")
  ]
))
dynamic_pbo_splice <- rbindlist(list(
  official[year >= 2019 & year <= official_forecast_end, .(
    year, value = expenses_ratio_gdp + net_capital_investment_ratio_gdp,
    series = "Concept-aligned PBO level, then model changes"
  )],
  paths[
    model == "dynamic_diff" &
      path_type == "Official forecast then model projection" &
      year > official_forecast_end,
    .(year, value, series = "Concept-aligned PBO level, then model changes")
  ]
))
pbo_expenses_only <- official[year >= 2019 & year <= official_forecast_end, .(
  year, value = expenses_ratio_gdp,
  series = "PBO operating expenses only"
)]
anchor_plot_data <- rbindlist(list(
  history_anchor_plot, dynamic_model_only, dynamic_pbo_splice,
  pbo_expenses_only
))
anchor_colours <- c(
  "National Accounts history" = "#222222",
  "Dynamic model from latest outturn" = "#0072B2",
  "Concept-aligned PBO level, then model changes" = "#009E73",
  "PBO operating expenses only" = "grey55"
)

p_anchor_comparison <- ggplot(
  anchor_plot_data,
  aes(year, value * 100, colour = series, linewidth = series, linetype = series)
) +
  geom_line() +
  geom_segment(
    data = data.table(
      x = latest_year, xend = latest_year,
      y = latest_pbo_expenses * 100, yend = latest_na * 100
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE, colour = "#CC3311", linewidth = 0.8,
    arrow = grid::arrow(ends = "both", length = grid::unit(0.12, "cm"))
  ) +
  annotate(
    "text", x = latest_year + 0.8,
    y = 100 * mean(c(latest_na, latest_pbo_expenses)),
    label = sprintf(
      "%.2f pp raw expenses gap\nPBO broad proxy is %.2f pp %s NA",
      100 * (latest_na - latest_pbo_expenses),
      100 * abs(latest_pbo - latest_na),
      ifelse(latest_pbo >= latest_na, "above", "below")
    ),
    colour = "#CC3311", hjust = 0, size = 3.1
  ) +
  geom_vline(
    xintercept = official_forecast_end + 0.5,
    linetype = "dashed", colour = "grey45"
  ) +
  scale_colour_manual(values = anchor_colours) +
  scale_linetype_manual(values = c(
    "National Accounts history" = "solid",
    "Dynamic model from latest outturn" = "solid",
    "Concept-aligned PBO level, then model changes" = "solid",
    "PBO operating expenses only" = "dotted"
  ), guide = "none") +
  scale_linewidth_manual(values = c(
    "National Accounts history" = 1.0,
    "Dynamic model from latest outturn" = 1.05,
    "Concept-aligned PBO level, then model changes" = 1.05,
    "PBO operating expenses only" = 0.8
  ), guide = "none") +
  labs(
    title = "Dynamic-difference projection with and without the PBO spending anchor",
    subtitle = "The model-only path starts from the latest National Accounts expenditure ratio",
    x = NULL, y = "% of GDP", colour = NULL,
    caption = paste0(
      "The concept-aligned splice uses PBO expenses plus net capital investment through ",
      official_forecast_end - 1L, "-", substr(official_forecast_end, 3, 4),
      " and the model's subsequent changes. The model-only path does not use the PBO spending projection."
    )
  ) + theme_fiscal() +
  theme(legend.position = "bottom", legend.text = element_text(size = 8))
save_plot_pair(
  p_anchor_comparison,
  file.path(peer_dir, "05_top_down_anchor_comparison"),
  10.5, 6.5
)

# All reported specifications without a PBO spending anchor. Each line is
# seeded with the common latest National Accounts observation, then follows the
# model's own forecast from the next financial year.
model_only_paths <- paths[
  path_type == "Unanchored model projection",
  .(year, value, model, model_label)
]
model_only_seeds <- unique(model_only_paths[, .(model, model_label)])[, .(
  year = latest_year, value = latest_na, model, model_label
)]
model_only_all <- rbindlist(list(model_only_seeds, model_only_paths))

p_model_only_all <- ggplot(
  model_only_all,
  aes(year, value * 100, colour = model_label)
) +
  geom_line(linewidth = 0.95) +
  geom_point(
    data = unique(model_only_seeds[, .(year, value)]),
    colour = "#222222", size = 2.5, inherit.aes = FALSE,
    aes(year, value * 100)
  ) +
  scale_colour_manual(values = peer_palette) +
  labs(
    title = "Top-down projections without a PBO spending anchor",
    subtitle = "Every path starts at the latest National Accounts outturn",
    x = NULL, y = "% of GDP", colour = NULL,
    caption = "Future demographic and macroeconomic assumptions are unchanged; only the PBO spending-level splice is omitted."
  ) + theme_fiscal() +
  theme(legend.text = element_text(size = 8))
save_plot_pair(
  p_model_only_all,
  file.path(peer_dir, "06_top_down_model_only_all"),
  10.5, 6.5
)

projection_bounds <- bounds[specification == "Projection ECM as fitted"]
frow <- projection_bounds[grepl("F-test", test)]
trow <- projection_bounds[grepl("t-test", test)]

p_f <- ggplot() +
  annotate("rect", xmin = 0, xmax = frow$lower_bound_I0_5pct, ymin = -Inf, ymax = Inf,
           fill = "#D9D9D9", alpha = 0.8) +
  annotate("rect", xmin = frow$lower_bound_I0_5pct, xmax = frow$upper_bound_I1_5pct,
           ymin = -Inf, ymax = Inf, fill = "#F0E442", alpha = 0.35) +
  annotate("rect", xmin = frow$upper_bound_I1_5pct, xmax = 5.2, ymin = -Inf, ymax = Inf,
           fill = "#56B4E9", alpha = 0.25) +
  geom_point(aes(frow$statistic, 1), colour = "#CC3311", size = 4) +
  geom_vline(xintercept = c(frow$lower_bound_I0_5pct, frow$upper_bound_I1_5pct), linetype = "dashed") +
  annotate("text", x = frow$statistic, y = 1.12, label = sprintf("F = %.3f", frow$statistic), colour = "#CC3311") +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = c(0, 5.2), ylim = c(0.75, 1.25)) +
  labs(title = "Bounds F-test", subtitle = "Grey: no relationship; yellow: inconclusive; blue: level relationship", x = "F statistic") +
  theme_fiscal() + theme(plot.subtitle = element_text(size = 8))

p_t <- ggplot() +
  annotate("rect", xmin = -5.2, xmax = trow$upper_bound_I1_5pct, ymin = -Inf, ymax = Inf,
           fill = "#56B4E9", alpha = 0.25) +
  annotate("rect", xmin = trow$upper_bound_I1_5pct, xmax = trow$lower_bound_I0_5pct,
           ymin = -Inf, ymax = Inf, fill = "#F0E442", alpha = 0.35) +
  annotate("rect", xmin = trow$lower_bound_I0_5pct, xmax = -1.2, ymin = -Inf, ymax = Inf,
           fill = "#D9D9D9", alpha = 0.8) +
  geom_point(aes(trow$statistic, 1), colour = "#CC3311", size = 4) +
  geom_vline(xintercept = c(trow$lower_bound_I0_5pct, trow$upper_bound_I1_5pct), linetype = "dashed") +
  annotate("text", x = trow$statistic, y = 1.12, label = sprintf("t = %.3f", trow$statistic), colour = "#CC3311") +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = c(-5.2, -1.2), ylim = c(0.75, 1.25)) +
  labs(title = "Bounds t-test", subtitle = "More-negative values are stronger evidence of adjustment", x = "t statistic") +
  theme_fiscal() + theme(plot.subtitle = element_text(size = 8))

test_plot <- spec_tests[
  specification == "Projection ECM as fitted" &
    test %in% c(
      "Breusch-Godfrey serial correlation (order 2)",
      "Ramsey RESET functional form",
      "Recursive CUSUM parameter stability"
    )
]
test_plot[, short_test := fcase(
  grepl("Breusch-Godfrey", test), "Serial correlation",
  grepl("RESET", test), "Functional form",
  default = "Parameter stability"
)]
test_plot[, outcome := fifelse(p_value < 0.05, "Reject null at 5%", "Do not reject null")]

p_tests <- ggplot(test_plot, aes(p_value, reorder(short_test, p_value), colour = outcome)) +
  geom_vline(xintercept = 0.05, linetype = "dashed", colour = "grey35") +
  geom_segment(aes(x = 0, xend = p_value, yend = reorder(short_test, p_value)),
               colour = "grey75", show.legend = FALSE) +
  geom_point(size = 3.2) +
  geom_text(aes(label = sprintf("p = %.4f", p_value)), nudge_x = 0.025,
            hjust = 0, size = 3, show.legend = FALSE) +
  scale_colour_manual(values = c("Reject null at 5%" = "#CC3311", "Do not reject null" = "#009E73")) +
  coord_cartesian(xlim = c(0, 0.58)) +
  labs(title = "Residual and stability checks", x = "p-value", y = NULL, colour = NULL) +
  theme_fiscal() + theme(legend.position = "bottom")

p_ardl <- (p_f | p_t) / p_tests +
  plot_layout(heights = c(0.85, 1.15)) +
  plot_annotation(
    title = "Why the fitted ARDL/ECM is not accepted as a long-run model",
    caption = paste0(
      "Finite-sample 5% bounds use 20,000 simulations for Case III with ",
      length(topdown_age_groups) + length(topdown_scale_drivers) + 2L,
      " long-run regressors. Passing residual and stability checks does not establish the required level relationship."
    )
  )
save_plot_pair(p_ardl, file.path(peer_dir, "02_ardl_rejection_evidence"), 11, 8)

debt_bottom <- fread(file.path(table_dir, "debt_paths_bottom_up.csv"))
bottom_central <- debt_bottom[
  spending_model == "central" & revenue_scenario == "central",
  .(year, value = total_expense_ratio, model_label = "Bottom-up central")
]
combined <- rbindlist(list(
  reported_paths[, .(year, value, model_label)],
  bottom_central
))
combined_colours <- c(peer_palette, `Bottom-up central` = "#000000")
p_all <- ggplot(combined, aes(year, value * 100, colour = model_label)) +
  geom_line(aes(linewidth = model_label == "Bottom-up central")) +
  geom_vline(xintercept = official_forecast_end + 0.5, linetype = "dashed", colour = "grey45") +
  scale_colour_manual(values = combined_colours) +
  scale_linewidth_manual(values = c(`TRUE` = 1.25, `FALSE` = 0.8), guide = "none") +
  labs(
    title = "Reported top-down specifications and bottom-up central path",
    subtitle = "Bottom-up includes endogenous interest; top-down broad expenditure embeds interest",
    x = NULL, y = "% of GDP", colour = NULL,
    caption = "Concepts are close rather than identical. The comparison is triangulation, not a formal forecast combination."
  ) + theme_fiscal() + theme(legend.text = element_text(size = 8))
save_plot_pair(p_all, file.path(peer_dir, "04_all_reported_models"), 10.5, 7)

# Population and real-income sensitivity.  The mechanically differenced nested
# benchmark remains in the machine-readable tables but is omitted here, in line
# with the colleague-facing presentation of the differenced family.
scale_income <- fread(file.path(table_dir, "top_down_scale_income_model_comparison.csv"))[
  model != "arimax_diff"
]
scale_income[model == "dynamic_diff", model_label := "Dynamic differences"]
scale_income[, variant_label := factor(
  variant_label,
  levels = c(
    "Current controls only", "Add population", "Add real GDP per capita",
    "Add population and real GDP per capita"
  )
)]
scale_order <- scale_income[variant == "real_gdp_per_capita_only"][
  order(rolling_rmse_5y_pp), model_label
]
scale_income[, model_label := factor(model_label, levels = rev(scale_order))]

fit_long <- melt(
  scale_income,
  id.vars = c("model", "model_label", "variant", "variant_label"),
  measure.vars = c("in_sample_rmse_pp", "rolling_rmse_5y_pp"),
  variable.name = "metric", value.name = "rmse_pp"
)
fit_long[, metric := fcase(
  metric == "in_sample_rmse_pp", "In-sample level RMSE",
  default = "Five-year rolling RMSE"
)]
p_scale_fit <- ggplot(
  fit_long,
  aes(rmse_pp, model_label, colour = variant_label, group = variant_label)
) +
  geom_point(size = 2.4, position = position_dodge(width = 0.55)) +
  facet_wrap(~metric, scales = "free_x") +
  scale_colour_manual(values = c(
    "Current controls only" = "grey50",
    "Add population" = "#0072B2",
    "Add real GDP per capita" = "#D55E00",
    "Add population and real GDP per capita" = "#009E73"
  )) +
  labs(
    title = "How population and real GDP per capita affect model fit",
    subtitle = "Lower RMSE is better; rolling forecasts use subsequently observed driver paths",
    x = "RMSE (percentage points of GDP)", y = NULL, colour = NULL
  ) + theme_fiscal() +
  theme(legend.position = "bottom", legend.text = element_text(size = 7.5))
save_plot_pair(p_scale_fit, file.path(peer_dir, "07_scale_income_fit"), 12, 7)

endpoint_scale <- scale_income[, .(
  model, model_label, variant, variant_label,
  endpoint_pct = anchored_2066 * 100
)]
p_scale_endpoint <- ggplot(
  endpoint_scale,
  aes(endpoint_pct, model_label, colour = variant_label, group = variant_label)
) +
  geom_point(size = 2.4, position = position_dodge(width = 0.55)) +
  scale_colour_manual(values = c(
    "Current controls only" = "grey50",
    "Add population" = "#0072B2",
    "Add real GDP per capita" = "#D55E00",
    "Add population and real GDP per capita" = "#009E73"
  )) +
  labs(
    title = "Population and real income materially change long-run endpoints",
    subtitle = paste0("PBO-anchored spending ratio in ", projection_end - 1L, "-", substr(projection_end, 3, 4)),
    x = "Projected spending (% of GDP)", y = NULL, colour = NULL,
    caption = "Endpoint dispersion is a specification diagnostic, not a probability interval."
  ) + theme_fiscal() +
  theme(legend.position = "bottom", legend.text = element_text(size = 7.5))
save_plot_pair(p_scale_endpoint, file.path(peer_dir, "08_scale_income_endpoints"), 12, 7)

# Export a flat figure folder matching the Overleaf project layout.
overleaf_figure_dir <- file.path(documentation_dir, "figures")
dir.create(overleaf_figure_dir, recursive = TRUE, showWarnings = FALSE)
figure_exports <- c(
  "top_down_selection_evidence.png" = file.path(peer_dir, "01_top_down_selection_evidence.png"),
  "ardl_rejection_evidence.png" = file.path(peer_dir, "02_ardl_rejection_evidence.png"),
  "top_down_reported_paths.png" = file.path(peer_dir, "03_top_down_reported_paths.png"),
  "top_down_anchor_comparison.png" = file.path(peer_dir, "05_top_down_anchor_comparison.png"),
  "top_down_model_only_all.png" = file.path(peer_dir, "06_top_down_model_only_all.png"),
  "top_down_scale_income_fit.png" = file.path(peer_dir, "07_scale_income_fit.png"),
  "top_down_scale_income_endpoints.png" = file.path(peer_dir, "08_scale_income_endpoints.png"),
  "all_reported_models.png" = file.path(peer_dir, "04_all_reported_models.png"),
  "bottom_up_central_paths.png" = file.path(figure_dir, "bottom_up_comparison", "01_central_paths.png"),
  "bottom_up_category_differences.png" = file.path(figure_dir, "bottom_up_comparison", "03_endpoint_category_differences.png")
)
for (destination in names(figure_exports)) {
  ok <- file.copy(
    figure_exports[[destination]],
    file.path(overleaf_figure_dir, destination),
    overwrite = TRUE
  )
  if (!ok) stop("Could not export Overleaf figure: ", destination, call. = FALSE)
}

message("Peer-review figures written to: ", peer_dir, " and ", overleaf_figure_dir)

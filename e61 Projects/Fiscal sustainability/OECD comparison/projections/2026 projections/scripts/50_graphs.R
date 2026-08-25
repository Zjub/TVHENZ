source(file.path("scripts", "00_config.R"))

historical <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
population_hist <- fread(file.path(processed_dir, "historical_age_shares.csv"))
population_proj <- fread(file.path(processed_dir, "population_projection_age_shares.csv"))
official <- fread(file.path(processed_dir, "official_pbo_nfo_wide.csv"))
gfs_purpose <- fread(file.path(processed_dir, "historical_gfs_expenses_by_purpose.csv"))
bottom <- fread(file.path(table_dir, "bottom_up_category_projections.csv"))
bottom_totals <- fread(file.path(table_dir, "bottom_up_total_projections.csv"))
top <- fread(file.path(table_dir, "top_down_projection_paths.csv"))
shapley <- fread(file.path(table_dir, "shapley_change_decomposition.csv"))
rolling_summary <- fread(file.path(table_dir, "forecast_check_rolling_summary.csv"))
official_compare <- fread(file.path(table_dir, "forecast_check_against_official_paths.csv"))
revenue <- fread(file.path(table_dir, "revenue_scenario_paths.csv"))
debt_bottom <- fread(file.path(table_dir, "debt_paths_bottom_up.csv"))
debt_top <- fread(file.path(table_dir, "debt_paths_top_down.csv"))
debt_official <- fread(file.path(table_dir, "debt_path_official_pbo.csv"))
fit_diagnostics <- fread(file.path(table_dir, "model_fit_diagnostics.csv"))
window_sensitivity <- fread(file.path(table_dir, "four_model_window_sensitivity.csv"))
window_sensitivity[, endpoint_difference_from_full_sample_pp :=
  endpoint_percent_gdp - endpoint_percent_gdp[window == "Full sample"], by = model]
setnames(window_sensitivity, "window", "estimation_window")

official_hist <- official[, .(
  year, expenses_ratio_gdp, revenue_ratio_gdp,
  topdown_value = official_topdown_expenditure(.SD)
)]
p1 <- ggplot() +
  geom_line(data = historical, aes(year, broad_expenditure_gdp * 100), colour = "#0072B2", linewidth = 0.9) +
  geom_line(data = official_hist, aes(year, topdown_value * 100), colour = "#D55E00", linewidth = 0.8) +
  labs(
    title = "Two aggregate spending concepts used in the workflow",
    subtitle = paste0("Configured treatment: ", topdown_interest_treatment_label()),
    x = NULL, y = "% of GDP",
    caption = "Sources: ABS National Accounts; Parliamentary Budget Office National Fiscal Outlook. Concepts differ; no level splice is hidden."
  ) + theme_fiscal()
save_plot_pair(p1, file.path(figure_dir, "data", "01_historical_spending_concepts"))

age_cols <- c("0_14", "15_34", "35_54", "55_64", "65p")
age_all <- rbindlist(list(
  population_hist[, c("year", age_cols), with = FALSE][, source := "Historical ABS"],
  population_proj[year >= 2025, c("year", age_cols), with = FALSE][, source := "Centre for Population projection"]
), fill = TRUE)
age_long <- melt(age_all, id.vars = c("year", "source"), variable.name = "age_group", value.name = "share")
p2 <- ggplot(age_long, aes(year, share * 100, colour = age_group, linetype = source)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Population age shares", x = NULL, y = "% of population", colour = "Age group", linetype = NULL,
       caption = "Sources: ABS; Centre for Population 2025 Population Statement. Missing years after 2035 are interpolated to the published 2065-66 point.") +
  theme_fiscal()
save_plot_pair(p2, file.path(figure_dir, "data", "02_population_age_shares"))

official_flows <- melt(
  official[year >= 2015, .(year, Revenue = revenue_ratio_gdp, Expenses = expenses_ratio_gdp)],
  id.vars = "year", variable.name = "series", value.name = "value"
)
p3 <- ggplot(official_flows, aes(year, value * 100, colour = series)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 2025.5, linetype = "dashed", colour = "grey45") +
  labs(title = "Official consolidated national fiscal anchor", subtitle = "Dashed line separates history/estimate from forecast",
       x = NULL, y = "% of GDP", colour = NULL, caption = "Source: PBO 2026-27 National Fiscal Outlook.") +
  theme_fiscal()
save_plot_pair(p3, file.path(figure_dir, "data", "03_official_revenue_expenses"))

bottom_central <- bottom[scenario == "central"]
p4 <- ggplot(bottom_central, aes(year, share_gdp * 100, fill = category)) +
  geom_area(position = "stack", colour = NA) +
  geom_vline(xintercept = official_forecast_end + 0.5, linetype = "dashed", colour = "white") +
  labs(title = "Bottom-up primary spending by purpose: central scenario",
       subtitle = "Official total primary spending is imposed through 2029-30; purpose drivers determine the later path",
       x = NULL, y = "% of GDP", fill = NULL,
       caption = "Sources: ABS GFS purpose categories; PBO forecast anchor; Centre for Population; e61 assumptions. Debt interest is added in the debt module.") +
  theme_fiscal() + theme(legend.text = element_text(size = 8))
save_plot_pair(p4, file.path(figure_dir, "bottom_up", "01_central_category_stack"), 10.5, 7)

p5 <- ggplot(bottom_totals, aes(year, primary_expense_ratio * 100, colour = scenario)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = official_forecast_end + 0.5, linetype = "dashed", colour = "grey45") +
  scale_colour_manual(values = palette_scenarios) +
  labs(title = "Bottom-up primary spending scenarios", subtitle = "Paths are identical during the official anchor and diverge under purpose-specific assumptions afterwards",
       x = NULL, y = "% of GDP", colour = NULL, caption = "Source: e61 bottom-up projection workflow.") + theme_fiscal()
save_plot_pair(p5, file.path(figure_dir, "bottom_up", "02_total_scenarios"))

contrib <- merge(
  bottom_central[, .(year, category, share_gdp)],
  bottom_central[year == official_forecast_end, .(category, base_share = share_gdp)],
  by = "category"
)
contrib[, change_pp := (share_gdp - base_share) * 100]
p6 <- ggplot(contrib[year >= official_forecast_end], aes(year, change_pp, fill = category)) +
  geom_area(position = "stack") +
  geom_hline(yintercept = 0, colour = "grey30") +
  labs(title = "Purpose contributions to the central primary-spending change since 2029-30",
       x = NULL, y = "Percentage points of GDP", fill = NULL,
       caption = "Contributions are changes in category shares, not causal Shapley estimates.") +
  theme_fiscal() + theme(legend.text = element_text(size = 8))
save_plot_pair(p6, file.path(figure_dir, "bottom_up", "03_category_contributions"), 10.5, 7)

top_anchor <- top[path_type == "Official forecast then model projection"]
p7 <- ggplot(top_anchor, aes(year, value * 100, colour = model_label)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = palette_models) +
  geom_vline(xintercept = official_forecast_end + 0.5, linetype = "dashed", colour = "grey45") +
  labs(title = "Top-down projections after PBO estimates",
       subtitle = "",
       x = NULL, y = "% of GDP", colour = NULL, caption = "Source: e61 estimates using ABS history and PBO anchor.") + theme_fiscal()
save_plot_pair(p7, file.path(figure_dir, "top_down", "01_anchored_long_run_models"), 10, 6.5)

top_unanchored <- top[path_type == "Unanchored model projection" & year <= official_forecast_end]
official_short <- official[year >= projection_start & year <= official_forecast_end]
official_short[, topdown_value := official_topdown_expenditure(.SD)]
p8 <- ggplot(top_unanchored, aes(year, value * 100, colour = model_label)) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.8) +
  geom_line(data = official_short, aes(year, topdown_value * 100), inherit.aes = FALSE, colour = "black", linewidth = 1.2) +
  geom_point(data = official_short, aes(year, topdown_value * 100), inherit.aes = FALSE, colour = "black", size = 2) +
  scale_colour_manual(values = palette_models) +
  labs(title = "Model-only top-down paths during the official forecast period",
       subtitle = paste0("Black is the matching PBO concept: ", topdown_interest_treatment_label()),
       x = NULL, y = "% of GDP", colour = NULL, caption = "This comparison separates projection mechanics from announced policy forecasts.") + theme_fiscal()
save_plot_pair(p8, file.path(figure_dir, "top_down", "02_unanchored_forecast_comparison"), 10, 6.5)

spread <- dcast(top_anchor, year ~ model, value.var = "value")
spread[, model_range_pp := (do.call(pmax, c(.SD, na.rm = TRUE)) - do.call(pmin, c(.SD, na.rm = TRUE))) * 100,
       .SDcols = reported_topdown_models]
p9 <- ggplot(spread, aes(year, model_range_pp)) +
  geom_area(fill = "#56B4E9", alpha = 0.7) +
  labs(title = "Top-down model uncertainty", subtitle = "Range between the highest and lowest anchored model path",
       x = NULL, y = "Percentage points of GDP", caption = "This is model spread, not a statistical confidence interval.") + theme_fiscal()
save_plot_pair(p9, file.path(figure_dir, "top_down", "03_model_spread"))

p_shapley <- ggplot(shapley, aes(reorder(component, contribution_pp), contribution_pp,
                                 fill = contribution_pp >= 0)) +
  geom_col(width = 0.72) +
  geom_hline(yintercept = 0, colour = "grey25") +
  coord_flip() +
  scale_fill_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "#0072B2"), guide = "none") +
  labs(
    title = paste0("Shapley attribution of the spending-ratio change, ",
                   unique(shapley$base_year), " to ", unique(shapley$comparison_year)),
    subtitle = "Factor contributions average incremental fitted effects over every ordering; the residual completes the observed change",
    x = NULL, y = "Percentage points of GDP",
    caption = "Attribution from re-fitted structural regressions. This is not a causal decomposition or a forecasting model."
  ) + theme_fiscal()
save_plot_pair(p_shapley, file.path(figure_dir, "top_down", "04_shapley_attribution"), 10, 6.5)

p10 <- ggplot(rolling_summary, aes(horizon, rmse_pp, colour = model_label)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  scale_colour_manual(values = palette_models) +
  scale_x_continuous(breaks = 1:forecast_check_horizon) +
  labs(title = "Rolling historical forecast errors", subtitle = "Conditional forecasts use the subsequently observed driver paths",
       x = "Forecast horizon (years)", y = "RMSE, percentage points of GDP", colour = NULL,
       caption = "Origins 2010-2020. COVID years are retained and identified by an intervention variable when available.") + theme_fiscal()
save_plot_pair(p10, file.path(figure_dir, "forecast_checks", "01_rmse_by_horizon"), 10, 6.5)

p11 <- ggplot(official_compare, aes(year, difference_pp, colour = model_label)) +
  geom_hline(yintercept = 0, colour = "black") + geom_line(linewidth = 0.9) + geom_point(size = 2) +
  labs(title = "Model-only difference from the official expense forecast",
       x = NULL, y = "Percentage points of GDP", colour = NULL,
       caption = "Positive values mean the projection method is above the official forecast. Bottom-up is shown before official scaling.") + theme_fiscal()
save_plot_pair(p11, file.path(figure_dir, "forecast_checks", "02_model_minus_official"), 10, 6.5)

p12 <- ggplot(revenue, aes(year, revenue_ratio * 100, colour = scenario)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = official_forecast_end + 0.5, linetype = "dashed", colour = "grey45") +
  scale_colour_manual(values = palette_scenarios) +
  labs(title = "External revenue anchor and long-run revenue scenarios", x = NULL, y = "% of GDP", colour = NULL,
       caption = "PBO consolidated revenue through 2029-30. Indexed thresholds are represented by holding revenue/GDP at the endpoint.") + theme_fiscal()
save_plot_pair(p12, file.path(figure_dir, "revenue_debt", "01_revenue_scenarios"))

debt_bottom[, scenario_label := fifelse(
  revenue_scenario == "indexed_thresholds", "Central spending + indexed thresholds",
  paste0(toupper(substr(spending_model, 1, 1)), substr(spending_model, 2, nchar(spending_model)))
)]
p13 <- ggplot(debt_bottom, aes(year, net_debt_ratio * 100, colour = scenario_label)) +
  geom_line(linewidth = 1) +
  geom_line(data = debt_official, aes(year, net_debt_ratio * 100), inherit.aes = FALSE, colour = "black", linewidth = 1.1, linetype = "dashed") +
  labs(title = "Net debt under bottom-up spending and revenue scenarios",
       subtitle = "Dashed black line is the published PBO forecast; projected interest responds to debt",
       x = NULL, y = "% of GDP", colour = NULL,
       caption = "Debt arithmetic includes primary expenses, endogenous interest, net capital investment and an explicit stock-flow adjustment assumption.") + theme_fiscal()
save_plot_pair(p13, file.path(figure_dir, "revenue_debt", "02_bottom_up_debt"), 10, 6.5)

p14 <- ggplot(debt_top, aes(year, net_debt_ratio * 100, colour = model_label)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = palette_models) +
  labs(title = "Net debt sensitivity to the top-down spending model",
       subtitle = "Top-down primary fiscal spending receives an endogenous debt-interest path",
       x = NULL, y = "% of GDP", colour = NULL,
       caption = "Published PBO debt and interest are used through FY2029-30; subsequent interest responds to the preceding net-debt stock.") + theme_fiscal()
save_plot_pair(p14, file.path(figure_dir, "revenue_debt", "03_top_down_debt_model_spread"), 10, 6.5)

bottom_central_total <- debt_bottom[
  spending_model == "central" & revenue_scenario == "central",
  .(year, value = total_expense_ratio, model_label = "Bottom-up central")
]
top_central_total <- debt_top[, .(
  year, value = total_expense_ratio, model_label
)]
comparison_paths <- rbindlist(list(
  top_central_total,
  bottom_central_total
))
comparison_colours <- c(palette_models, `Bottom-up central` = "#000000")
p15 <- ggplot(comparison_paths, aes(year, value * 100, colour = model_label)) +
  geom_line(aes(linewidth = model_label == "Bottom-up central")) +
  geom_vline(xintercept = official_forecast_end + 0.5, linetype = "dashed", colour = "grey45") +
  scale_colour_manual(values = comparison_colours) +
  scale_linewidth_manual(values = c(`TRUE` = 1.25, `FALSE` = 0.8), guide = "none") +
  labs(
    title = "Bottom-up and top-down spending projections on a common expense basis",
    subtitle = "Both approaches add debt interest to primary spending after the official forecast period",
    x = NULL, y = "% of GDP", colour = NULL,
    caption = "Top-down primary fiscal expenditure includes capital investment; debt interest is added once in the debt module."
  ) + theme_fiscal()
save_plot_pair(p15, file.path(figure_dir, "model_comparison", "01_bottom_up_vs_top_down"), 10.5, 6.8)

bottom_range <- debt_bottom[revenue_scenario %in% c("central", "pressure", "restraint"), .(
  low = min(total_expense_ratio), high = max(total_expense_ratio)
), by = year]
top_range <- top_central_total[, .(low = min(value), high = max(value)), by = year]
p16 <- ggplot() +
  geom_ribbon(data = bottom_range, aes(year, ymin = low * 100, ymax = high * 100, fill = "Bottom-up scenarios"), alpha = 0.25) +
  geom_ribbon(data = top_range, aes(year, ymin = low * 100, ymax = high * 100, fill = "Top-down models"), alpha = 0.35) +
  geom_line(data = bottom_central_total, aes(year, value * 100), colour = "black", linewidth = 1) +
  scale_fill_manual(values = c(`Bottom-up scenarios` = "#D55E00", `Top-down models` = "#0072B2")) +
  labs(
    title = "Range of bottom-up scenarios and top-down model estimates",
    subtitle = "Black line is the central bottom-up path; ribbons are specification or assumption ranges, not confidence intervals",
    x = NULL, y = "% of GDP", fill = NULL,
    caption = "The widening bottom-up range reflects explicit cost, defence and financing assumptions."
  ) + theme_fiscal()
save_plot_pair(p16, file.path(figure_dir, "model_comparison", "02_bottom_up_top_down_ranges"), 10.5, 6.8)

p17 <- ggplot(fit_diagnostics, aes(reorder(model_label, rmse_pp), rmse_pp, fill = model_label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0("R2 = ", sprintf("%.2f", r_squared))), hjust = -0.08, size = 3.5) +
  coord_flip() +
  expand_limits(y = max(fit_diagnostics$rmse_pp) * 1.22) +
  labs(
    title = "In-sample fit of the top-down models",
    subtitle = "RMSE is calculated on the spending/GDP level; labels show in-sample R-squared",
    x = NULL, y = "RMSE, percentage points of GDP",
    caption = "In-sample fit should be considered alongside rolling forecast performance and residual tests."
  ) + theme_fiscal()
save_plot_pair(p17, file.path(figure_dir, "diagnostics", "01_in_sample_fit"), 9.5, 6.2)

p19 <- ggplot(window_sensitivity, aes(estimation_window, endpoint_difference_from_full_sample_pp,
                                      colour = model_label, group = model_label)) +
  geom_hline(yintercept = 0, colour = "grey35") +
  geom_line(linewidth = 0.8) + geom_point(size = 2.2) +
  scale_colour_manual(values = palette_models) +
  labs(
    title = "Sensitivity to the estimation sample",
    subtitle = "Difference in the anchored 2065-66 spending endpoint relative to the full 1980-2025 sample",
    x = NULL, y = "Percentage points of GDP", colour = NULL,
    caption = "The pre-COVID test estimates through 2019 and then projects using observed 2020-25 drivers before the long-run assumptions."
  ) + theme_fiscal()
save_plot_pair(p19, file.path(figure_dir, "diagnostics", "03_estimation_window_sensitivity"), 10.5, 6.5)

message("Figure suite written to: ", figure_dir)

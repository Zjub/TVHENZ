source(file.path("scripts", "00_config.R"))
source(file.path("R", "model_functions_2.R"))
suppressPackageStartupMessages({library(ggplot2); library(data.table); library(forecast)})

historical_raw <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
future <- fread(file.path(processed_dir, "macro_scenario_assumptions.csv"))[scenario == "central"]
official <- fread(file.path(processed_dir, "official_pbo_nfo_wide.csv"))
treatments <- c("include_interest", "exclude_other_interest", "exclude_total_interest")
treatment_labels <- setNames(vapply(
  treatments, topdown_interest_treatment_label, character(1)
), treatments)

fit_metrics <- function(result, historical) {
  fits <- result$fits
  dyn <- build_topdown_table(historical, include_y = TRUE)
  y <- historical$broad_expenditure_gdp
  metric <- function(model, actual, fitted) {
    keep <- is.finite(actual) & is.finite(fitted)
    data.table(
      model,
      in_sample_observations = sum(keep),
      in_sample_rmse_pp = sqrt(mean((actual[keep] - fitted[keep])^2)) * 100,
      in_sample_mae_pp = mean(abs(actual[keep] - fitted[keep])) * 100,
      in_sample_r_squared = 1 - sum((actual[keep] - fitted[keep])^2) /
        sum((actual[keep] - mean(actual[keep]))^2)
    )
  }
  ols_hat <- as.numeric(fitted(fits$structural_ols))
  level_hat <- as.numeric(fitted(fits$arimax_level))
  diff_terms <- colnames(fits$arimax_diff$xreg)
  diff_rows <- complete.cases(dyn[, c("dy", diff_terms), with = FALSE])
  diff_hat <- dyn$y_lag[diff_rows] + as.numeric(fitted(fits$arimax_diff))

  structural_x <- structural_feature_frame(historical, character())
  structural_x <- structural_x[, names(coef(fits$hybrid_structural_ols))[-1L], drop = FALSE]
  structural_hat <- as.numeric(predict(fits$hybrid_structural_ols, structural_x))
  d_structural <- c(NA_real_, diff(structural_hat))
  hybrid_terms <- colnames(fits$hybrid_macro$xreg)
  hybrid_rows <- complete.cases(dyn[, c("dy", hybrid_terms), with = FALSE])
  hybrid_hat <- dyn$y_lag[hybrid_rows] + d_structural[hybrid_rows] +
    as.numeric(fitted(fits$hybrid_macro))

  ecm_terms <- setdiff(all.vars(formula(fits$ardl_ecm)), "dw")
  ecm_rows <- complete.cases(dyn[, c("dw", ecm_terms), with = FALSE])
  ecm_hat <- exp(dyn$w_lag[ecm_rows] + as.numeric(fitted(fits$ardl_ecm)))

  rbindlist(list(
    metric("structural_ols", y, ols_hat),
    metric("arimax_level", y, level_hat),
    metric("arimax_diff", dyn$y[diff_rows], diff_hat),
    metric("hybrid", dyn$y[hybrid_rows], hybrid_hat),
    metric("ardl_ecm", dyn$y[ecm_rows], ecm_hat)
  ))
}

fit_results <- vector("list", length(treatments))
names(fit_results) <- treatments
path_results <- list()
metric_results <- list()
coefficient_results <- list()

for (treatment in treatments) {
  historical <- apply_topdown_interest_treatment(historical_raw, treatment)
  fit <- fit_predict_topdown(
    historical, future, interest_treatment = treatment
  )
  fit_results[[treatment]] <- fit
  model_paths <- copy(fit$paths)
  model_paths[, `:=`(
    model_label = unname(model_labels[model]),
    path_type = "Model only"
  )]
  official_path <- official[
    year >= projection_start & year <= official_forecast_end,
    .(year, official_value = official_topdown_expenditure(.SD, treatment))
  ]
  official_join <- official_path[year == official_forecast_end, official_value]
  anchored <- model_paths[, {
    model_join <- value[year == official_forecast_end]
    data.table(
      year,
      value = fifelse(
        year <= official_forecast_end,
        official_path$official_value[match(year, official_path$year)],
        official_join + value - model_join
      )
    )
  }, by = .(model, model_label, interest_treatment, interest_treatment_label)]
  anchored[, path_type := "Matching PBO anchor"]
  path_results[[treatment]] <- rbindlist(list(model_paths, anchored), fill = TRUE)

  metrics <- fit_metrics(fit, historical)
  metrics[, `:=`(
    interest_treatment = treatment,
    interest_treatment_label = treatment_labels[[treatment]]
  )]
  metric_results[[treatment]] <- metrics

  coefficient_results[[treatment]] <- rbindlist(lapply(names(fit$fits), function(component) {
    values <- coef(fit$fits[[component]])
    data.table(
      interest_treatment = treatment,
      interest_treatment_label = treatment_labels[[treatment]],
      component,
      term = names(values),
      estimate = as.numeric(values)
    )
  }), fill = TRUE)
}

paths <- rbindlist(path_results, fill = TRUE)
in_sample <- rbindlist(metric_results, fill = TRUE)
coefficients <- rbindlist(coefficient_results, fill = TRUE)

rolling <- rbindlist(lapply(treatments, function(treatment) {
  historical <- apply_topdown_interest_treatment(historical_raw, treatment)
  rbindlist(lapply(forecast_check_start:forecast_check_end, function(origin) {
    train <- historical[year <= origin]
    validation <- historical[year > origin & year <= origin + forecast_check_horizon]
    if (!nrow(validation)) return(NULL)
    fit <- tryCatch(
      fit_predict_topdown(train, validation, interest_treatment = treatment),
      error = function(e) NULL
    )
    if (is.null(fit)) return(NULL)
    merge(
      fit$paths[, .(year, model, forecast = value)],
      validation[, .(year, actual = broad_expenditure_gdp)],
      by = "year"
    )[, `:=`(
      origin = origin,
      horizon = year - origin,
      error = forecast - actual,
      interest_treatment = treatment,
      interest_treatment_label = treatment_labels[[treatment]]
    )]
  }), fill = TRUE)
}), fill = TRUE)
rolling[, model_label := unname(model_labels[model])]
rolling_summary <- rolling[, .(
  observations = .N,
  rolling_rmse_pp = sqrt(mean(error^2)) * 100,
  rolling_mae_pp = mean(abs(error)) * 100,
  rolling_bias_pp = mean(error) * 100
), by = .(model, model_label, interest_treatment, interest_treatment_label, horizon)]

rolling_overall <- rolling_summary[, .(
  average_rolling_rmse_pp = mean(rolling_rmse_pp),
  rolling_rmse_1y_pp = rolling_rmse_pp[horizon == 1L],
  rolling_rmse_5y_pp = rolling_rmse_pp[horizon == 5L]
), by = .(model, model_label, interest_treatment, interest_treatment_label)]

endpoints <- paths[year == projection_end, .(
  endpoint_percent_gdp = value * 100
), by = .(model, model_label, interest_treatment, interest_treatment_label, path_type)]
endpoint_wide <- dcast(
  endpoints,
  model + model_label + interest_treatment + interest_treatment_label ~ path_type,
  value.var = "endpoint_percent_gdp"
)
comparison <- merge(
  merge(in_sample, rolling_overall,
        by = c("model", "interest_treatment", "interest_treatment_label")),
  endpoint_wide,
  by = c("model", "model_label", "interest_treatment", "interest_treatment_label")
)

baseline <- comparison[interest_treatment == "include_interest", .(
  model,
  included_interest_in_sample_rmse_pp = in_sample_rmse_pp,
  included_interest_average_rolling_rmse_pp = average_rolling_rmse_pp,
  included_interest_anchored_endpoint = `Matching PBO anchor`
)]
comparison <- merge(comparison, baseline, by = "model")
comparison[, `:=`(
  in_sample_rmse_change_pp = in_sample_rmse_pp - included_interest_in_sample_rmse_pp,
  average_rolling_rmse_change_pp =
    average_rolling_rmse_pp - included_interest_average_rolling_rmse_pp,
  anchored_endpoint_change_pp =
    `Matching PBO anchor` - included_interest_anchored_endpoint
)]

history_long <- melt(
  historical_raw,
  id.vars = "year",
  measure.vars = c(
    "broad_expenditure_including_interest_gdp",
    "broad_expenditure_excluding_other_interest_gdp",
    "broad_expenditure_excluding_total_interest_gdp"
  ),
  variable.name = "outcome", value.name = "value"
)
history_long[, interest_treatment_label := fcase(
  outcome == "broad_expenditure_including_interest_gdp", treatment_labels[["include_interest"]],
  outcome == "broad_expenditure_excluding_other_interest_gdp", treatment_labels[["exclude_other_interest"]],
  default = treatment_labels[["exclude_total_interest"]]
)]

colours <- c(
  "Include all interest" = "#0072B2",
  "Exclude conventional other interest" = "#D55E00",
  "Exclude total interest payable" = "#009E73"
)
theme_interest <- theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

p_history <- ggplot(history_long, aes(year, value * 100, colour = interest_treatment_label)) +
  geom_line(linewidth = 0.9) + scale_colour_manual(values = colours) +
  labs(
    title = "National Accounts spending under three interest treatments",
    subtitle = "Government consumption + investment + income payable, with the stated interest deduction",
    x = NULL, y = "% of GDP", colour = NULL
  ) + theme_interest

p_paths <- ggplot(
  paths[path_type == "Matching PBO anchor"],
  aes(year, value * 100, colour = interest_treatment_label)
) + geom_line(linewidth = 0.85) + facet_wrap(~model_label, scales = "free_y", ncol = 2) +
  geom_vline(xintercept = official_forecast_end + 0.5, linetype = "dashed", colour = "grey45") +
  scale_colour_manual(values = colours) +
  labs(
    title = "Top-down projections under alternative interest treatments",
    subtitle = "Each treatment uses its matching PBO anchor through FY2029-30",
    x = NULL, y = "% of GDP", colour = NULL
  ) + theme_interest

change_paths <- paths[path_type == "Matching PBO anchor" & year >= official_forecast_end]
change_paths[, change_since_anchor_pp :=
               (value - value[year == official_forecast_end]) * 100,
             by = .(model, interest_treatment)]
p_changes <- ggplot(
  change_paths, aes(year, change_since_anchor_pp, colour = interest_treatment_label)
) + geom_line(linewidth = 0.85) + facet_wrap(~model_label, scales = "free_y", ncol = 2) +
  geom_hline(yintercept = 0, colour = "grey45") + scale_colour_manual(values = colours) +
  labs(
    title = "Model-implied change after the common anchor year",
    subtitle = "Changes from FY2029-30 isolate model dynamics from concept-level differences",
    x = NULL, y = "Change from FY2029-30 (percentage points of GDP)", colour = NULL
  ) + theme_interest

p_rolling <- ggplot(
  rolling_summary,
  aes(horizon, rolling_rmse_pp, colour = interest_treatment_label)
) + geom_line(linewidth = 0.85) + geom_point(size = 1.7) +
  facet_wrap(~model_label, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = colours) + scale_x_continuous(breaks = 1:5) +
  labs(
    title = "Rolling forecast fit under alternative interest treatments",
    subtitle = "Lower RMSE is better; outcomes differ by the interest component removed",
    x = "Forecast horizon", y = "RMSE (percentage points)", colour = NULL
  ) + theme_interest

debt_active <- fread(file.path(table_dir, "debt_paths_top_down.csv"))
total_equivalent_paths <- rbindlist(list(
  paths[
    path_type == "Matching PBO anchor" & interest_treatment == "include_interest",
    .(year, model, model_label, value,
      series = "Previous treatment: interest embedded")
  ],
  debt_active[, .(
    year, model, model_label, value = total_expense_ratio,
    series = "Preferred treatment: primary plus endogenous interest"
  )]
))
total_colours <- c(
  "Previous treatment: interest embedded" = "#0072B2",
  "Preferred treatment: primary plus endogenous interest" = "#D55E00"
)
p_total <- ggplot(
  total_equivalent_paths, aes(year, value * 100, colour = series)
) + geom_line(linewidth = 0.9) + facet_wrap(~model_label, scales = "free_y", ncol = 2) +
  geom_vline(xintercept = official_forecast_end + 0.5,
             linetype = "dashed", colour = "grey45") +
  scale_colour_manual(values = total_colours) +
  labs(
    title = "Total expenditure before and after separating debt interest",
    subtitle = "The preferred treatment adds modelled PDI to primary fiscal expenditure after FY2029-30",
    x = NULL, y = "% of GDP", colour = NULL
  ) + theme_interest

out_dir <- file.path(figure_dir, "interest_treatment_sensitivity")
doc_fig_dir <- file.path(documentation_dir, "figures")
make_dirs(c(out_dir, doc_fig_dir))
save_interest <- function(plot, name, height = 6) {
  ggsave(file.path(out_dir, paste0(name, ".png")), plot,
         width = 9.5, height = height, dpi = 300)
  ggsave(file.path(out_dir, paste0(name, ".svg")), plot,
         width = 9.5, height = height)
  ggsave(file.path(doc_fig_dir, paste0(name, ".png")), plot,
         width = 9.5, height = height, dpi = 300)
}
save_interest(p_history, "interest_treatment_historical_comparison", 5.5)
save_interest(p_paths, "interest_treatment_projection_comparison", 7.2)
save_interest(p_changes, "interest_treatment_projection_changes", 7.2)
save_interest(p_rolling, "interest_treatment_rolling_fit", 7.2)
save_interest(p_total, "interest_treatment_total_expenditure_comparison", 7.2)

fwrite(paths, file.path(table_dir, "interest_treatment_projection_paths.csv"))
fwrite(in_sample, file.path(table_dir, "interest_treatment_in_sample_fit.csv"))
fwrite(rolling, file.path(table_dir, "interest_treatment_rolling_predictions.csv"))
fwrite(rolling_summary, file.path(table_dir, "interest_treatment_rolling_fit.csv"))
fwrite(comparison, file.path(table_dir, "interest_treatment_model_comparison.csv"))
fwrite(coefficients, file.path(table_dir, "interest_treatment_coefficients.csv"))
fwrite(total_equivalent_paths,
       file.path(table_dir, "interest_treatment_total_expenditure_paths.csv"))
message("Interest-treatment sensitivity tables and figures written.")

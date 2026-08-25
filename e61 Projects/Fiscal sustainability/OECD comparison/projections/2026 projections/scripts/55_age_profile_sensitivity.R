source(file.path("scripts", "00_config.R"))
source(file.path("R", "model_functions_2.R"))
suppressPackageStartupMessages({library(ggplot2); library(data.table)})

historical <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
future <- fread(file.path(processed_dir, "macro_scenario_assumptions.csv"))[scenario == "central"]
official <- fread(file.path(processed_dir, "official_pbo_nfo_wide.csv"))
specifications <- c(age_shares = "Separate 0-14 and 65+ shares",
                    expenditure_profile = "Age-expenditure profile index")

fit_one <- function(specification, train = historical, projection = future) {
  fit_predict_topdown(
    train, projection, demographic_specification = specification
  )
}
fits <- lapply(names(specifications), fit_one)
names(fits) <- names(specifications)

model_paths <- rbindlist(lapply(names(fits), function(specification) {
  copy(fits[[specification]]$paths)[, `:=`(
    demographic_specification = specification,
    demographic_label = specifications[[specification]],
    model_label = unname(model_labels[model]),
    path_type = "Model only"
  )]
}))
official_path <- official[
  year >= projection_start & year <= official_forecast_end,
  .(year, official_value = official_topdown_expenditure(.SD))
]
anchor_value <- official_path[year == official_forecast_end, official_value]
anchored <- model_paths[, {
  join <- value[year == official_forecast_end]
  data.table(
    year,
    value = fifelse(
      year <= official_forecast_end,
      official_path$official_value[match(year, official_path$year)],
      anchor_value + value - join
    )
  )
}, by = .(model, model_label, demographic_specification, demographic_label)]
anchored[, path_type := "Common PBO anchor"]
all_paths <- rbindlist(list(model_paths, anchored), fill = TRUE)

rolling <- rbindlist(lapply(names(specifications), function(specification) {
  rbindlist(lapply(forecast_check_start:forecast_check_end, function(origin) {
    train <- historical[year <= origin]
    validation <- historical[year > origin & year <= origin + forecast_check_horizon]
    if (!nrow(validation)) return(NULL)
    result <- tryCatch(fit_one(specification, train, validation), error = function(e) NULL)
    if (is.null(result)) return(NULL)
    merge(
      result$paths[, .(year, model, forecast = value)],
      validation[, .(year, actual = broad_expenditure_gdp)], by = "year"
    )[, `:=`(
      origin = origin, horizon = year - origin, error = forecast - actual,
      demographic_specification = specification,
      demographic_label = specifications[[specification]]
    )]
  }), fill = TRUE)
}), fill = TRUE)
rolling[, model_label := unname(model_labels[model])]
rolling_summary <- rolling[, .(
  observations = .N, rmse_pp = sqrt(mean(error^2)) * 100,
  mae_pp = mean(abs(error)) * 100, bias_pp = mean(error) * 100
), by = .(model, model_label, demographic_specification, demographic_label, horizon)]

comparison <- rolling_summary[, .(
  average_rolling_rmse_pp = mean(rmse_pp),
  rolling_rmse_1y_pp = rmse_pp[horizon == 1L],
  rolling_rmse_5y_pp = rmse_pp[horizon == 5L]
), by = .(model, model_label, demographic_specification, demographic_label)]
endpoints <- all_paths[year == projection_end, .(
  endpoint_percent_gdp = value * 100
), by = .(model, model_label, demographic_specification, demographic_label, path_type)]
comparison <- merge(
  comparison,
  dcast(endpoints, model + model_label + demographic_specification + demographic_label ~ path_type,
        value.var = "endpoint_percent_gdp"),
  by = c("model", "model_label", "demographic_specification", "demographic_label")
)

coefficient_rows <- function(fit, specification) {
  pieces <- list(
    structural_ols = fit$fits$structural_ols,
    arimax_level = fit$fits$arimax_level,
    arimax_diff = fit$fits$arimax_diff,
    hybrid_structural = fit$fits$hybrid_structural_ols
  )
  rbindlist(lapply(names(pieces), function(component) {
    object <- pieces[[component]]
    values <- coef(object)
    terms <- grep("age_|age_expenditure", names(values), value = TRUE)
    data.table(
      demographic_specification = specification, component,
      term = terms, estimate = as.numeric(values[terms])
    )
  }))
}
coefficients <- rbindlist(lapply(names(fits), function(specification) {
  coefficient_rows(fits[[specification]], specification)
}))

category_weights <- age_profile_category_table()
age_groups <- c("0_14", "15_34", "35_54", "55_64", "65p")
category_profiles <- dcast(
  category_weights,
  category + exposure + category_expenditure_weight + base_expenditure ~ age_group,
  value.var = "relative_weight"
)
base_population <- fread(file.path(processed_dir, "historical_age_shares.csv"))[year == 2025L]
base_exposure <- as.numeric(
  as.matrix(category_profiles[, ..age_groups]) %*% as.numeric(base_population[, ..age_groups])
)
effective_age_weights <- data.table(
  age_group = age_groups,
  effective_relative_weight = vapply(age_groups, function(group) {
    sum(category_profiles$category_expenditure_weight *
          category_profiles[[group]] / base_exposure)
  }, numeric(1))
)
effective_age_weights[, base_population_share :=
                        as.numeric(base_population[, age_groups, with = FALSE])]
effective_age_weights[, base_index_contribution :=
                        effective_relative_weight * base_population_share]

index_path <- rbindlist(list(
  historical[, .(year, period = "Historical")],
  future[, .(year, period = "Projection")]
))
index_inputs <- rbindlist(list(historical, future), fill = TRUE)
index_path[, index := age_expenditure_profile_index(index_inputs)]

out_dir <- file.path(figure_dir, "age_profile_sensitivity")
make_dirs(c(out_dir, file.path(documentation_dir, "figures")))
profile_colours <- c(
  "Separate 0-14 and 65+ shares" = "#0072B2",
  "Age-expenditure profile index" = "#D55E00"
)
theme_profile <- theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

p_index <- ggplot(index_path, aes(year, index, colour = period)) +
  geom_line(linewidth = 0.9) + geom_hline(yintercept = 1, colour = "grey45", linetype = "dashed") +
  scale_colour_manual(values = c(Historical = "#0072B2", Projection = "#D55E00")) +
  labs(
    title = "Aggregate age-expenditure pressure index",
    subtitle = "Fixed 2024-25 category shares and grouped lifecycle relativities; index equals one in 2024-25",
    x = NULL, y = "Index (FY2024-25 = 1)", colour = NULL
  ) + theme_profile

p_weights <- ggplot(effective_age_weights,
                    aes(age_group, effective_relative_weight, fill = age_group)) +
  geom_col(show.legend = FALSE) + geom_hline(yintercept = 1, linetype = "dashed") +
  geom_text(aes(label = sprintf("%.2f", effective_relative_weight)),
            vjust = -0.35, size = 3.6) +
  labs(
    title = "Effective aggregate lifecycle weights",
    subtitle = "A value above one means that age group receives more than the population-average primary-spending exposure",
    x = "Age group", y = "Relative weight"
  ) + theme_profile

affected_models <- setdiff(names(model_labels), "ardl_ecm")
p_paths <- ggplot(
  anchored[model %in% affected_models],
  aes(year, value * 100, colour = demographic_label)
) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = official_forecast_end + 0.5,
             linetype = "dashed", colour = "grey45") +
  facet_wrap(~model_label, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = profile_colours) +
  labs(
    title = "Effect of replacing age shares with the expenditure-profile index",
    subtitle = "Common PBO anchor through FY2029-30; all non-demographic assumptions are unchanged",
    x = NULL, y = "% of GDP", colour = NULL
  ) + theme_profile

p_rolling <- ggplot(
  rolling_summary[model %in% affected_models],
  aes(horizon, rmse_pp, colour = demographic_label)
) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.8) +
  facet_wrap(~model_label, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = profile_colours) + scale_x_continuous(breaks = 1:5) +
  labs(
    title = "Rolling forecast fit under the two demographic specifications",
    subtitle = "Lower RMSE is better; conditional forecasts use subsequently observed drivers",
    x = "Forecast horizon", y = "RMSE (percentage points)", colour = NULL
  ) + theme_profile

save_profile <- function(plot, filename, height = 6) {
  ggsave(file.path(out_dir, paste0(filename, ".png")), plot,
         width = 9.5, height = height, dpi = 300)
  ggsave(file.path(out_dir, paste0(filename, ".svg")), plot,
         width = 9.5, height = height)
  ggsave(file.path(documentation_dir, "figures", paste0(filename, ".png")), plot,
         width = 9.5, height = height, dpi = 300)
}
save_profile(p_index, "age_expenditure_profile_index", 5.5)
save_profile(p_weights, "age_expenditure_effective_weights", 5.5)
save_profile(p_paths, "age_profile_projection_comparison", 7)
save_profile(p_rolling, "age_profile_rolling_comparison", 7)

fwrite(category_weights, file.path(table_dir, "age_profile_category_weights.csv"))
fwrite(effective_age_weights, file.path(table_dir, "age_profile_effective_age_weights.csv"))
fwrite(index_path, file.path(table_dir, "age_expenditure_profile_index.csv"))
fwrite(all_paths, file.path(table_dir, "age_profile_projection_paths.csv"))
fwrite(rolling_summary, file.path(table_dir, "age_profile_rolling_fit.csv"))
fwrite(comparison, file.path(table_dir, "age_profile_model_comparison.csv"))
fwrite(coefficients, file.path(table_dir, "age_profile_model_coefficients.csv"))
message("Age-expenditure-profile sensitivity tables and figures written.")

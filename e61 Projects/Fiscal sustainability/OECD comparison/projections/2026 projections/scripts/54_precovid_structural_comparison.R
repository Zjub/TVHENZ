source(file.path("scripts", "00_config.R"))
source(file.path("R", "model_functions_2.R"))
suppressPackageStartupMessages({library(ggplot2); library(data.table)})

historical <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
future <- fread(file.path(processed_dir, "macro_scenario_assumptions.csv"))[scenario == "central"]
official <- fread(file.path(processed_dir, "official_pbo_nfo_wide.csv"))
models <- c("structural_ols", "arimax_level", "hybrid", "ardl_ecm")

full_fit <- fit_predict_topdown(historical, future)$paths[model %in% models]
full_fit[, estimation_sample := "Current fit: through FY2024-25"]

pre_train <- historical[year <= 2019L]
pre_future <- rbindlist(list(historical[year >= 2020L], future),
                        use.names = TRUE, fill = TRUE)
pre_fit <- fit_predict_topdown(pre_train, pre_future)$paths[model %in% models]
pre_fit[, estimation_sample := "Pre-COVID fit: through FY2018-19"]

comparison <- rbindlist(list(full_fit, pre_fit), use.names = TRUE)
comparison[, model_label := unname(model_labels[model])]
comparison[, path_type := "Model only"]

official_path <- official[
  year >= projection_start & year <= official_forecast_end,
  .(year, official_value = official_topdown_expenditure(.SD))
]
anchor_value <- official_path[year == official_forecast_end, official_value]
anchored <- comparison[year >= projection_start, {
  model_join <- value[year == official_forecast_end]
  data.table(
    year,
    value = fifelse(
      year <= official_forecast_end,
      official_path$official_value[match(year, official_path$year)],
      anchor_value + value - model_join
    )
  )
}, by = .(model, model_label, estimation_sample)]
anchored[, path_type := "Common PBO anchor"]

all_paths <- rbindlist(list(comparison, anchored), fill = TRUE)
endpoints <- dcast(
  all_paths[year == projection_end,
            .(model, model_label, estimation_sample, path_type,
              endpoint_percent_gdp = value * 100)],
  model + model_label + path_type ~ estimation_sample,
  value.var = "endpoint_percent_gdp"
)
endpoints[, difference_pre_minus_current_pp :=
            `Pre-COVID fit: through FY2018-19` - `Current fit: through FY2024-25`]

out_dir <- file.path(figure_dir, "structural_precovid_comparison")
make_dirs(c(out_dir, file.path(documentation_dir, "figures")))
sample_colours <- c(
  "Current fit: through FY2024-25" = "#0072B2",
  "Pre-COVID fit: through FY2018-19" = "#D55E00"
)
theme_compare <- theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

p_validation <- ggplot() +
  geom_line(
    data = historical[year >= 2010L],
    aes(year, broad_expenditure_gdp * 100), colour = "black", linewidth = 0.8
  ) +
  geom_line(
    data = comparison[year <= 2035L],
    aes(year, value * 100, colour = estimation_sample), linewidth = 0.9
  ) +
  geom_vline(xintercept = 2019.5, linetype = "dashed", colour = "grey45") +
  facet_wrap(~model_label, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = sample_colours) +
  labs(
    title = "Structural models estimated before and after COVID",
    subtitle = "Black is the National Accounts outcome; the pre-COVID equations are estimated only through FY2018-19",
    x = NULL, y = "% of GDP", colour = NULL
  ) + theme_compare

p_model_only <- ggplot() +
  geom_line(
    data = historical[year >= 2000L],
    aes(year, broad_expenditure_gdp * 100), colour = "black", linewidth = 0.75
  ) +
  geom_line(
    data = comparison[year >= 2020L],
    aes(year, value * 100, colour = estimation_sample), linewidth = 0.9
  ) +
  facet_wrap(~model_label, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = sample_colours) +
  labs(
    title = "Model-only structural projections by estimation sample",
    subtitle = "Pre-COVID paths use observed driver values for FY2019-20 to FY2024-25, but no post-2019 spending observations",
    x = NULL, y = "% of GDP", colour = NULL
  ) + theme_compare

p_anchored <- ggplot(
  anchored,
  aes(year, value * 100, colour = estimation_sample)
) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = official_forecast_end + 0.5,
             linetype = "dashed", colour = "grey45") +
  facet_wrap(~model_label, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = sample_colours) +
  labs(
    title = "Structural projections after a common PBO anchor",
    subtitle = paste0("Both fits use the matching PBO anchor through FY2029-30: ",
                      topdown_interest_treatment_label()),
    x = NULL, y = "% of GDP", colour = NULL
  ) + theme_compare

save_comparison <- function(plot, filename, height = 7) {
  ggsave(file.path(out_dir, paste0(filename, ".png")), plot,
         width = 10, height = height, dpi = 300)
  ggsave(file.path(out_dir, paste0(filename, ".svg")), plot,
         width = 10, height = height)
  ggsave(file.path(documentation_dir, "figures", paste0(filename, ".png")), plot,
         width = 10, height = height, dpi = 300)
}

save_comparison(p_validation, "structural_precovid_validation")
save_comparison(p_model_only, "structural_precovid_model_only")
save_comparison(p_anchored, "structural_precovid_pbo_anchored")

fwrite(all_paths, file.path(table_dir, "structural_precovid_projection_comparison.csv"))
fwrite(endpoints, file.path(table_dir, "structural_precovid_endpoint_comparison.csv"))
message("Pre-COVID structural-model comparison tables and figures written.")

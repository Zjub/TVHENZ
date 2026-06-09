## Projection graphs for the bracket creep research note.
##
## Input:
##   ATO 1p results/tax_distribution_results.csv
##
## Outputs:
##   projection_graphs/figure_3_etr_change_by_percentile.{png,pdf,csv}
##   projection_graphs/figure_4_real_income_gain_clawback.{png,pdf,csv}
##   projection_graphs/appendix_scenario_etr_change.{png,pdf,csv}

required_packages <- c("dplyr", "ggplot2", "readr", "scales", "tidyr","theme61","data.table")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Install missing packages before running this script: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(tidyr)
library(theme61)

fallback_palette <- c("#2f5f8f", "#d55e00", "#6b6b6b", "#009e73", "#7f3c8d")
has_theme61 <- FALSE
e61_palette <- fallback_palette

data_path <- file.path("ATO 1p results", "tax_distribution_results.csv")
output_dir <- "projection_graphs"

dir.create(output_dir, showWarnings = FALSE)

real_income_growth_annual <- 0.012

pretty_fy <- function(fy) {
  parts <- gsub("^FY", "", fy)
  parts <- strsplit(parts, "_", fixed = TRUE)
  vapply(
    parts,
    function(x) paste0(x[1], "/", substr(x[2], 3, 4)),
    character(1)
  )
}

plot_labs <- function(..., footnotes = NULL, sources = NULL) {
  if (has_theme61) {
    tryCatch(
      theme61::labs_e61(..., footnotes = footnotes, sources = sources),
      error = function(e) {
        caption <- paste(
          c(
            if (!is.null(sources)) paste0("Source: ", sources),
            if (!is.null(footnotes)) paste0("Note: ", paste(footnotes, collapse = " "))
          ),
          collapse = "\n"
        )

        labs(..., caption = caption)
      }
    )
  } else {
    caption <- paste(
      c(
        if (!is.null(sources)) paste0("Source: ", sources),
        if (!is.null(footnotes)) paste0("Note: ", paste(footnotes, collapse = " "))
      ),
      collapse = "\n"
    )

    labs(..., caption = caption)
  }
}

plot_theme <- function() {
  if (has_theme61 && exists("theme_e61", asNamespace("theme61"))) {
    tryCatch(
      getFromNamespace("theme_e61", "theme61")(),
      error = function(e) {
        theme_minimal(base_size = 11) +
          theme(
            panel.grid.minor = element_blank(),
            legend.position = "bottom",
            plot.title = element_text(face = "bold"),
            plot.caption = element_text(hjust = 0, colour = "grey35")
          )
      }
    )
  } else {
    theme_minimal(base_size = 11) +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, colour = "grey35")
      )
  }
}

save_outputs <- function(plot, basename, data) {
  png_path <- file.path(output_dir, paste0(basename, ".png"))
  pdf_path <- file.path(output_dir, paste0(basename, ".pdf"))
  csv_path <- file.path(output_dir, paste0(basename, ".csv"))

  ggsave(png_path, plot, width = 8, height = 4.8, dpi = 300)
  ggsave(pdf_path, plot, width = 8, height = 4.8)
  write_csv(data, csv_path)
}

tax_results <- read_csv(data_path, show_col_types = FALSE) %>%
  mutate(
    percentile = as.numeric(percentile),
    percentile_label = percentile * 100,
    fy_sort = as.integer(gsub("^FY([0-9]{4})_.*$", "\\1", fy))
  ) %>%
  arrange(fy_sort, percentile)

required_columns <- c("percentile", "fy", "baseline", "high_inflation", "no_policy", "indexed_brackets")
missing_columns <- setdiff(required_columns, names(tax_results))

if (length(missing_columns) > 0) {
  stop(
    "Input file is missing required columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

financial_years <- tax_results %>%
  distinct(fy, fy_sort) %>%
  arrange(fy_sort)

base_year <- financial_years$fy[1]
final_year <- financial_years$fy[nrow(financial_years)]
middle_years <- financial_years$fy[c(
  min(3, nrow(financial_years)),
  min(6, nrow(financial_years)),
  nrow(financial_years)
)] %>%
  unique()

projection_years <- c(base_year, middle_years) %>% unique()
years_elapsed <- financial_years$fy_sort[nrow(financial_years)] - financial_years$fy_sort[1]
real_income_growth_total <- (1 + real_income_growth_annual)^years_elapsed - 1

base_rates <- tax_results %>%
  filter(fy == base_year) %>%
  select(percentile, base_etr = baseline)

final_rates <- tax_results %>%
  filter(fy == final_year) %>%
  select(
    percentile,
    final_baseline_etr = baseline,
    final_indexed_etr = indexed_brackets
  )

figure_3_data <- tax_results %>%
  filter(fy %in% projection_years) %>%
  left_join(base_rates, by = "percentile") %>%
  mutate(
    etr_change_pp = (baseline - base_etr) * 100,
    year_label = pretty_fy(fy)
  )

setDT(figure_3_data)

figure_3_plot <- ggplot(
  figure_3_data[fy != "FY2025_26" & percentile <= 0.99],
  aes(x = percentile_label, y = etr_change_pp, colour = year_label)
) +
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 0.9) +
  #scale_colour_manual(values = e61_palette[seq_along(unique(figure_3_data$year_label))]) +
  scale_x_continuous(
    breaks = seq(0, 100, 10),
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(labels = label_number(suffix = " pp", accuracy = 0.5)) +
  labs_e61(
    title = "Bracket Creep across the income distribution",
    x = "Earnings percentile",
    y = "Percentage point change from 2025/26",
    colour = "Financial year",
    sources = c("ATO Individual Sample file","e61"),
    footnotes = c("Uses the 2022/23 file. Earnings are inflated to 2025/26 level and the 2025/26 nominal tax system is applied to establish base line.","Assumes income growth of 3.7%pa, in line with PBO projections.")
  ) +
  plab(c("2027/28","2030/31","2034/35"),x=c(60,60,60),y=c(-0.5,1.5,3.5))

figure_3_plot

unique(figure_3_data$fy)

unique(figure_3_data$percentile)

figure_3_data[fy == "FY2034_35"]

figure_3_data[fy == "FY2034_35" & percentile_label >= 99]

save_e61(paste0("Bracket_creep_ATO1_F3","_newBudg.png"),res=2,save_data = TRUE,chart_type = "wide",dim = list(width = 15))

figure_4_data <- base_rates %>%
  left_join(final_rates, by = "percentile") %>%
  mutate(
    percentile_label = percentile * 100,
    after_tax_gain_baseline = (1 + real_income_growth_total) * (1 - final_baseline_etr) -
      (1 - base_etr),
    after_tax_gain_indexed = (1 + real_income_growth_total) * (1 - final_indexed_etr) -
      (1 - base_etr),
    income_gain_clawback = if_else(
      after_tax_gain_indexed > 0,
      (after_tax_gain_indexed - after_tax_gain_baseline) / after_tax_gain_indexed,
      NA_real_
    )
  )

figure_4_plot <- ggplot(
  figure_4_data,
  aes(x = percentile_label, y = income_gain_clawback * 100)
) +
  geom_hline(yintercept = 0) +
  geom_line() +
  scale_x_continuous(
    breaks = seq(0, 100, 10),
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  labs_e61(
    title = "Share of real income gains lost to bracket creep",
    x = "Earnings percentile",
    y = "Share of real post-tax income gain",
    sources = "ATO 1p results file",
    footnotes = c("Uses the 2022/23 file. Earnings are inflated to 2025/26 level and the 2025/26 nominal tax system is applied to establish base line.","Assumes income growth of 3.7%pa, in line with PBO projections.")
  )

figure_4_plot

save_e61(paste0("Bracket_creep_ATO1_F4","_newBudg.png"),res=2,save_data = TRUE,chart_type = "wide",dim = list(width = 15))

# scenario_labels <- c(
#   baseline = "Baseline",
#   high_inflation = "High inflation",
#   no_policy = "No policy change",
#   indexed_brackets = "Indexed brackets"
# )
#
# appendix_data <- tax_results %>%
#   filter(fy == final_year) %>%
#   left_join(base_rates, by = "percentile") %>%
#   pivot_longer(
#     cols = all_of(names(scenario_labels)),
#     names_to = "scenario",
#     values_to = "etr"
#   ) %>%
#   mutate(
#     scenario = factor(scenario_labels[scenario], levels = scenario_labels),
#     etr_change_pp = (etr - base_etr) * 100,
#     percentile_label = percentile * 100
#   )
#
# appendix_plot <- ggplot(
#   appendix_data,
#   aes(x = percentile_label, y = etr_change_pp, colour = scenario)
# ) +
#   geom_hline(yintercept = 0, colour = "grey65", linewidth = 0.4) +
#   geom_line(linewidth = 0.85) +
#   scale_colour_manual(values = e61_palette[seq_along(scenario_labels)]) +
#   scale_x_continuous(
#     breaks = seq(0, 100, 10),
#     limits = c(0, 100),
#     expand = expansion(mult = c(0, 0.01))
#   ) +
#   scale_y_continuous(labels = label_number(suffix = " pp", accuracy = 0.5)) +
#   plot_labs(
#     title = paste0("Effective tax rate changes by scenario in ", pretty_fy(final_year)),
#     x = "Earnings percentile",
#     y = paste0("Percentage point change from ", pretty_fy(base_year)),
#     colour = NULL,
#     sources = "ATO 1p results file"
#   ) +
#   plot_theme()
#
# save_outputs(appendix_plot, "appendix_scenario_etr_change", appendix_data)
#
# message("Projection graph outputs written to: ", normalizePath(output_dir))

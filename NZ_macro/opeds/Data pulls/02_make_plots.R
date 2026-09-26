# Make op-ed charts from the outputs of 01_pull_data.R.
# Run from any working directory with:
#   Rscript "Data pulls/02_make_plots.R"

required_packages <- c("dplyr", "ggplot2", "readr", "scales", "theme61", "tidyr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop(
    "Install the required packages first: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(scales)
  library(theme61)
  library(tidyr)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE))
} else {
  normalizePath(getwd(), mustWork = TRUE)
}

processed_dir <- file.path(script_dir, "processed")
figure_dir <- file.path(script_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

needed <- file.path(
  processed_dir,
  c(
    "befu_2026_fiscal.csv",
    "befu_2026_economic.csv",
    "ltfs_2025_selected_figures.csv",
    "stats_nz_cofog_history.csv",
    "stats_nz_cofog_growth_contributions.csv",
    "stats_nz_cofog_2010_2025_decomposition.csv",
    "treasury_ltfm_functional_projections.csv",
    "treasury_functional_subcategories_endpoint_decomposition.csv",
    "treasury_welfare_benefits_2010_2025_decomposition.csv",
    "treasury_welfare_recipient_counts_2010_2025.csv",
    "stats_nz_economic_affairs_2010_2025_by_expense_type.csv",
    "treasury_economic_affairs_subcategories_2010_2025.csv",
    "world_bank_macro_comparison.csv"
  )
)
if (any(!file.exists(needed))) {
  stop("Run 01_pull_data.R first. Missing: ", paste(basename(needed[!file.exists(needed)]), collapse = ", "), call. = FALSE)
}

befu_fiscal <- read_csv(file.path(processed_dir, "befu_2026_fiscal.csv"), show_col_types = FALSE)
befu_economic <- read_csv(file.path(processed_dir, "befu_2026_economic.csv"), show_col_types = FALSE)
ltfs <- read_csv(file.path(processed_dir, "ltfs_2025_selected_figures.csv"), show_col_types = FALSE)
cofog <- read_csv(file.path(processed_dir, "stats_nz_cofog_history.csv"), show_col_types = FALSE)
cofog_growth <- read_csv(file.path(processed_dir, "stats_nz_cofog_growth_contributions.csv"), show_col_types = FALSE)
cofog_endpoints <- read_csv(file.path(processed_dir, "stats_nz_cofog_2010_2025_decomposition.csv"), show_col_types = FALSE)
ltfm_functional <- read_csv(file.path(processed_dir, "treasury_ltfm_functional_projections.csv"), show_col_types = FALSE)
treasury_subcategories <- read_csv(file.path(processed_dir, "treasury_functional_subcategories_endpoint_decomposition.csv"), show_col_types = FALSE)
welfare_benefits <- read_csv(file.path(processed_dir, "treasury_welfare_benefits_2010_2025_decomposition.csv"), show_col_types = FALSE)
welfare_recipients <- read_csv(file.path(processed_dir, "treasury_welfare_recipient_counts_2010_2025.csv"), show_col_types = FALSE)
cofog_economic_type <- read_csv(file.path(processed_dir, "stats_nz_economic_affairs_2010_2025_by_expense_type.csv"), show_col_types = FALSE)
treasury_economic_detail <- read_csv(file.path(processed_dir, "treasury_economic_affairs_subcategories_2010_2025.csv"), show_col_types = FALSE)
world_bank <- read_csv(file.path(processed_dir, "world_bank_macro_comparison.csv"), show_col_types = FALSE)

e61_colours <- palette_e61(6)

save_chart <- function(plot, filename, chart_type = "normal") {
  message("Saving with save_e61: ", filename)
  # Let save_e61 calculate its native canvas and type scale. Supplying custom
  # dimensions here would cause the package to rescale text a second time.
  theme61::save_e61(
    filename = file.path(figure_dir, filename),
    plot = plot,
    format = c("png", "svg"),
    chart_type = chart_type,
    res = 2,
    spell_check = FALSE,
    print_info = FALSE
  )
}

# 1. The near-term consolidation task: OBEGALx and net core Crown debt.
near_term <- befu_fiscal |>
  filter(
    unit == "% of GDP",
    measure %in% c("OBEGAL (excluding ACC)", "Net core Crown debt2")
  ) |>
  mutate(
    measure = recode(
      measure,
      "OBEGAL (excluding ACC)" = "OBEGALx",
      "Net core Crown debt2" = "Net core Crown debt"
    ),
    status = factor(status, levels = c("Actual", "Forecast"))
  )

p1 <- ggplot(near_term, aes(year, value, colour = status, group = status)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.4) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  facet_wrap(~measure, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = c("Actual" = e61_colours[1], "Forecast" = e61_colours[2]), drop = FALSE) +
  scale_x_continuous(breaks = pretty_breaks(7)) +
  scale_y_continuous(labels = label_number(suffix = "%", accuracy = 1)) +
  labs_e61(
    title = "Returning to surplus does not immediately reverse the debt rise",
    subtitle = "Budget 2026 actuals and forecasts, years ending June",
    x = NULL,
    y = NULL,
    sources = "New Zealand Treasury, Budget Economic and Fiscal Update 2026"
  ) +
  theme_e61(legend = "bottom")
save_chart(p1, "01_near_term_balance_and_debt")

# 2. The structural deficit is not simply a cyclical story.
structural <- ltfs |>
  filter(topic == "structural fiscal balance") |>
  mutate(series = recode(series, "Structural balance" = "Structural balance", "OBEGALx" = "OBEGALx"))

p2 <- ggplot(structural, aes(year, value, colour = series)) +
  geom_hline(yintercept = 0, colour = "grey65", linewidth = 0.5) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = c("OBEGALx" = e61_colours[1], "Structural balance" = e61_colours[3])) +
  scale_x_continuous(breaks = pretty_breaks(7)) +
  scale_y_continuous(labels = label_number(suffix = "%", accuracy = 0.5)) +
  labs_e61(
    title = "New Zealand has been running a structural deficit",
    subtitle = "Operating balance and Treasury estimate of the structural balance, % of GDP",
    x = NULL,
    y = NULL,
    sources = "New Zealand Treasury, He Tirohanga Mokopuna 2025"
  ) +
  theme_e61(legend = "bottom")
save_chart(p2, "02_structural_balance", chart_type = "wide")

# 3. Ageing pressure: fewer working-age people for each person aged 65+.
support_ratio <- ltfs |>
  filter(topic == "old-age support ratio") |>
  mutate(series = recode(series, "LTFS 2025" = "Latest projection", "LTFS 2006" = "Projection made in 2006"))

p3 <- ggplot(support_ratio, aes(year, value, colour = series)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("Latest projection" = e61_colours[1], "Projection made in 2006" = e61_colours[4])) +
  scale_x_continuous(breaks = pretty_breaks(7)) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.5),
    limits = c(0, ceiling(max(support_ratio$value, na.rm = TRUE)))
  ) +
  labs_e61(
    title = "The old-age support ratio is set to halve",
    subtitle = "People aged 15-64 for each person aged 65 and over",
    x = NULL,
    y = NULL,
    sources = "Stats NZ and New Zealand Treasury, He Tirohanga Mokopuna 2025"
  ) +
  theme_e61(legend = "bottom")
save_chart(p3, "03_old_age_support_ratio", chart_type = "wide")

# 4. Health and NZ Superannuation under the unchanged-policy baseline.
age_spending <- bind_rows(
  ltfs |>
    filter(topic == "health expenditure and productivity", grepl("baseline", series, ignore.case = TRUE)) |>
    transmute(year, component = "Health", value),
  ltfs |>
    filter(topic == "superannuation expenditure and productivity", grepl("baseline", series, ignore.case = TRUE)) |>
    transmute(year, component = "New Zealand Superannuation", value)
)

p4 <- ggplot(age_spending, aes(year, value, colour = component)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("Health" = e61_colours[1], "New Zealand Superannuation" = e61_colours[2])) +
  scale_x_continuous(breaks = pretty_breaks(7)) +
  scale_y_continuous(labels = label_number(suffix = "%", accuracy = 1)) +
  labs_e61(
    title = "Age-related spending pressures build steadily",
    subtitle = "Unchanged-policy baseline expenditure, % of GDP",
    x = NULL,
    y = NULL,
    footnotes = "Long-term projections illustrate policy pressure; they are not forecasts.",
    sources = "New Zealand Treasury OLG model, He Tirohanga Mokopuna 2025"
  ) +
  theme_e61(legend = "bottom")
save_chart(p4, "04_age_related_spending", chart_type = "wide")

# 5. Passive policy eventually produces an explosive debt path.
long_debt <- ltfs |>
  filter(topic == "OLG and LTFM net debt projections") |>
  mutate(series = recode(series, "OLG" = "Overlapping-generations model", "LTFM" = "Long-term fiscal model"))

p5 <- ggplot(long_debt, aes(year, value, colour = series)) +
  geom_hline(yintercept = 100, colour = "grey70", linetype = "dashed", linewidth = 0.5) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("Overlapping-generations model" = e61_colours[1], "Long-term fiscal model" = e61_colours[3])) +
  scale_x_continuous(breaks = pretty_breaks(7)) +
  scale_y_continuous(
    labels = label_number(suffix = "%", accuracy = 25),
    limits = c(0, ceiling(max(long_debt$value, na.rm = TRUE) / 25) * 25)
  ) +
  labs_e61(
    title = "Without policy change, debt does not stabilise",
    subtitle = "Net core Crown debt under unchanged-policy projections, % of GDP",
    x = NULL,
    y = NULL,
    footnotes = "These scenarios show the implications of unchanged policy; they are not predictions.",
    sources = "New Zealand Treasury, He Tirohanga Mokopuna 2025"
  ) +
  theme_e61(legend = "bottom")
save_chart(p5, "05_unchanged_policy_debt", chart_type = "wide")

# 6. Macro context: weak per-capita growth relative to Australia and the OECD.
gdp_pc <- world_bank |>
  filter(indicator_name == "Real GDP per capita growth", year >= 2000) |>
  mutate(country = recode(country_code, "NZL" = "New Zealand", "AUS" = "Australia", "OED" = "OECD members"))

p6 <- ggplot(gdp_pc, aes(year, value, colour = country)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.5) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c("New Zealand" = e61_colours[1], "Australia" = e61_colours[2], "OECD members" = e61_colours[4])) +
  scale_x_continuous(breaks = pretty_breaks(7)) +
  scale_y_continuous(labels = label_number(suffix = "%", accuracy = 1)) +
  labs_e61(
    title = "Weak per-capita growth makes fiscal adjustment harder",
    subtitle = "Annual change in real GDP per person",
    x = NULL,
    y = NULL,
    sources = "World Bank, World Development Indicators"
  ) +
  theme_e61(legend = "bottom")
save_chart(p6, "06_real_gdp_per_capita_growth", chart_type = "wide")

# 7. True COFOG history from Stats NZ: general government, including capital.
cofog_latest <- cofog |>
  filter(expense_measure == "Total expenses", year == max(year), function_name != "Total") |>
  arrange(percent_gdp) |>
  mutate(function_name = factor(function_name, levels = function_name))

p7 <- ggplot(cofog_latest, aes(percent_gdp, function_name)) +
  geom_col(fill = e61_colours[1], width = 0.72) +
  scale_x_continuous(labels = label_number(suffix = "%", accuracy = 0.5), expand = expansion(mult = c(0, 0.08))) +
  labs_e61(
    title = "Social protection, health and education dominate government spending",
    subtitle = "General-government expenditure by COFOG function, year ending June 2025, % of GDP",
    x = NULL,
    y = NULL,
    footnotes = "Total expenses include operating expenses and net acquisition of non-financial assets. General government consolidates central and local government.",
    sources = "Stats NZ, Government Finance Statistics; GDP denominator from New Zealand Treasury"
  ) +
  theme_e61()
save_chart(p7, "07_cofog_spending_by_function")

# 8. Treasury function-level projections. These are not labelled COFOG because
# the institutional scope and classification differ from the Stats NZ series.
ltfm_components <- ltfm_functional |>
  filter(year >= 2025) |>
  select(year, status, functional_class, percent_gdp) |>
  pivot_wider(names_from = functional_class, values_from = percent_gdp) |>
  transmute(
    year,
    status,
    Health,
    `New Zealand Superannuation`,
    `Other social security and welfare` = `Social security and welfare` - `New Zealand Superannuation`,
    Education,
    `Finance costs`,
    `Other functions` = `Total Crown expenses` - Health - `Social security and welfare` - Education - `Finance costs`
  ) |>
  pivot_longer(
    cols = -c(year, status),
    names_to = "component",
    values_to = "percent_gdp"
  ) |>
  mutate(
    component = factor(
      component,
      levels = c("Other functions", "Education", "Health", "Other social security and welfare", "New Zealand Superannuation", "Finance costs")
    )
  )

p8 <- ggplot(ltfm_components, aes(year, percent_gdp, fill = component)) +
  geom_area(colour = "white", linewidth = 0.15) +
  geom_vline(xintercept = 2029, colour = "grey35", linetype = "dashed", linewidth = 0.5) +
  scale_fill_manual(values = e61_colours[c(5, 4, 1, 6, 2, 3)]) +
  guides(fill = guide_legend(ncol = 2)) +
  scale_x_continuous(breaks = pretty_breaks(7)) +
  scale_y_continuous(labels = label_number(suffix = "%", accuracy = 5), expand = expansion(mult = c(0, 0.02))) +
  labs_e61(
    title = "Ageing and debt servicing reshape the spending mix",
    subtitle = "Treasury unchanged-policy total-Crown expense projections, % of GDP",
    x = NULL,
    y = NULL,
    footnotes = c(
      "Dashed line marks the end of the BEFU 2025 forecast period; later values are projections, not forecasts.",
      "Treasury functional classes are related to, but are not identical to, COFOG and exclude local government."
    ),
    sources = "New Zealand Treasury, 2025 Long-term Fiscal Model"
  ) +
  theme_e61(legend = "bottom")
save_chart(p8, "08_long_term_spending_by_function", chart_type = "wide")

# Common six-part grouping keeps the historical charts legible while the
# processed CSV retains all ten first-level COFOG functions.
cofog_group <- function(x) {
  ifelse(
    x %in% c("Social protection", "Health", "Education", "General public services", "Economic affairs"),
    x,
    "Other functions"
  )
}

# 9. COFOG history: composition and overall scale relative to GDP.
cofog_history_grouped <- cofog |>
  filter(expense_measure == "Total expenses", function_name != "Total") |>
  mutate(component = cofog_group(function_name)) |>
  group_by(year, component) |>
  summarise(percent_gdp = sum(percent_gdp), .groups = "drop") |>
  mutate(
    component = factor(
      component,
      levels = c("Other functions", "Economic affairs", "General public services", "Education", "Health", "Social protection")
    )
  )

p9 <- ggplot(cofog_history_grouped, aes(year, percent_gdp, fill = component)) +
  geom_area(colour = "white", linewidth = 0.15) +
  scale_fill_manual(values = e61_colours[c(5, 4, 6, 3, 1, 2)]) +
  guides(fill = guide_legend(ncol = 2)) +
  scale_x_continuous(breaks = pretty_breaks(8)) +
  scale_y_continuous(labels = label_number(suffix = "%", accuracy = 5), expand = expansion(mult = c(0, 0.02))) +
  labs_e61(
    title = "Government spending rose sharply through the pandemic",
    subtitle = "General-government expenditure by COFOG function, % of GDP",
    x = NULL,
    y = NULL,
    footnotes = c(
      "Total expenses include operating expenses and net acquisition of non-financial assets.",
      "Other functions comprise defence, public order and safety, environmental protection, housing and community amenities, and recreation, culture and religion."
    ),
    sources = "Stats NZ, Government Finance Statistics; GDP denominator from New Zealand Treasury"
  ) +
  theme_e61(legend = "bottom")
save_chart(p9, "09_cofog_spending_history", chart_type = "wide")

# 10. Contributions sum to annual nominal total-expenditure growth, subject to
# the source data's small rounding differences.
cofog_growth_grouped <- cofog_growth |>
  mutate(component = cofog_group(function_name)) |>
  group_by(year, component) |>
  summarise(contribution_pp = sum(contribution_to_growth_pp), .groups = "drop") |>
  mutate(
    component = factor(
      component,
      levels = c("Other functions", "Economic affairs", "General public services", "Education", "Health", "Social protection")
    )
  )

p10 <- ggplot(cofog_growth_grouped, aes(year, contribution_pp, fill = component)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.45) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = e61_colours[c(5, 4, 6, 3, 1, 2)]) +
  guides(fill = guide_legend(ncol = 2)) +
  scale_x_continuous(breaks = pretty_breaks(8)) +
  scale_y_continuous(labels = label_number(suffix = "pp", accuracy = 2)) +
  labs_e61(
    title = "Social protection drove much of the recent spending growth",
    subtitle = "Contribution to annual nominal growth in general-government expenditure, percentage points",
    x = NULL,
    y = NULL,
    footnotes = c(
      "Each contribution is the function's annual dollar change divided by total expenditure in the previous year.",
      "Contributions sum to nominal expenditure growth; they include inflation and do not measure service-volume growth."
    ),
    sources = "Stats NZ, Government Finance Statistics; e61 calculations"
  ) +
  theme_e61(legend = "bottom")
save_chart(p10, "10_cofog_contributions_to_spending_growth", chart_type = "wide")

# 11. Long-change attribution is often clearer than a sequence of annual
# contributions. Shares sum to 100% of the 2010-2025 nominal dollar increase.
cofog_endpoint_plot <- cofog_endpoints |>
  mutate(function_name = reorder(function_name, contribution_to_total_increase_percent))

p11 <- ggplot(cofog_endpoint_plot, aes(contribution_to_total_increase_percent, function_name)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.4) +
  geom_col(fill = e61_colours[1], width = 0.72) +
  geom_text(
    aes(
      label = label_number(suffix = "%", accuracy = 0.1)(contribution_to_total_increase_percent),
      hjust = if_else(contribution_to_total_increase_percent >= 15, 1.05, -0.1)
    ),
    size = 3.5
  ) +
  scale_x_continuous(
    labels = label_number(suffix = "%", accuracy = 10),
    expand = expansion(mult = c(0.02, 0.18))
  ) +
  labs_e61(
    title = "Social protection led the 2010-2025 spending increase",
    subtitle = "Share of the nominal increase in general-government expenditure",
    x = "Share of total dollar increase",
    y = NULL,
    footnotes = c(
      "Each bar is the function's dollar increase divided by the increase in total expenditure; bars sum to 100%.",
      "This is a nominal decomposition and includes inflation; 2025 data are provisional."
    ),
    sources = "Stats NZ, Government Finance Statistics; e61 calculations"
  ) +
  theme_e61()
save_chart(p11, "11_cofog_contribution_to_2010_2025_spending_increase")

# 12. The official NZ COFOG release stops at the ten divisions. Treasury's
# related core-Crown expense tables provide an endpoint breakdown for nine
# divisions, shown as a separate, explicitly non-COFOG decomposition.
treasury_detail_plot <- treasury_subcategories |>
  mutate(
    closest_cofog_division = recode(
      closest_cofog_division,
      "Housing and community amenities" = "Housing and community"
    ),
    subcategory = recode(
      subcategory,
      "Health-service, disability and pharmaceutical purchasing" = "Health, disability and pharmaceutical purchasing",
      "Economic and industrial services" = "Economic and industrial services",
      "Primary and secondary schools" = "Primary and secondary schools",
      "Tax receivable write-downs" = "Tax receivable write-downs"
    )
  )

make_subcategory_plot <- function(groups, title) {
  plot_data <- treasury_detail_plot |>
    filter(closest_cofog_division %in% groups) |>
    mutate(subcategory = reorder(subcategory, contribution_to_mapped_group_increase_percent))

  ggplot(plot_data, aes(contribution_to_mapped_group_increase_percent, subcategory)) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.35) +
    geom_col(fill = e61_colours[2], width = 0.7) +
    geom_text(
      aes(label = label_number(suffix = "%", accuracy = 1)(contribution_to_mapped_group_increase_percent)),
      hjust = -0.1,
      size = 3
    ) +
    facet_wrap(~closest_cofog_division, scales = "free", ncol = 1) +
    scale_x_continuous(
      breaks = pretty_breaks(4),
      labels = label_number(suffix = "%", accuracy = 10),
      expand = expansion(mult = c(0.08, 0.3))
    ) +
    labs_e61(
      title = title,
      subtitle = "Share of each related Treasury core-Crown functional group's nominal increase, 2010 to 2025",
      x = NULL,
      y = NULL,
      footnotes = c(
        "Treasury classes are the closest available detailed lens; they are not second-level COFOG and exclude local government.",
        "Residuals make groups exhaustive. 2010 is actual; 2025 is a Budget 2025 forecast."
      ),
      sources = "New Zealand Treasury, HYEFU 2010 and BEFU 2025; e61 calculations"
    ) +
    theme_e61() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
}

p12a <- make_subcategory_plot(
  c("Social protection", "Health", "Education"),
  "What drove growth in the three largest spending functions"
)
save_chart(p12a, "12a_treasury_subcategories_social_health_education")

p12b <- make_subcategory_plot(
  c("General public services", "Public order and safety", "Defence"),
  "What drove growth in government services, public safety and defence"
)
save_chart(p12b, "12b_treasury_subcategories_government_safety_defence")

p12c <- make_subcategory_plot(
  c("Economic affairs", "Housing and community", "Recreation, culture and religion"),
  "What drove growth in economic, housing and cultural functions"
)
save_chart(p12c, "12c_treasury_subcategories_economic_housing_culture")

# 13. Decompose the growth in welfare benefits using harmonised groupings that
# bridge the 2013 benefit reforms and subsequent naming changes.
welfare_benefit_plot <- welfare_benefits |>
  mutate(benefit_group = reorder(benefit_group, contribution_to_benefit_increase_percent))

p13 <- ggplot(welfare_benefit_plot, aes(contribution_to_benefit_increase_percent, benefit_group)) +
  geom_col(fill = e61_colours[2], width = 0.72) +
  geom_text(
    aes(
      label = label_number(suffix = "%", accuracy = 0.1)(contribution_to_benefit_increase_percent),
      hjust = if_else(contribution_to_benefit_increase_percent >= 20, 1.05, -0.1)
    ),
    size = 3.5
  ) +
  scale_x_continuous(
    labels = label_number(suffix = "%", accuracy = 10),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs_e61(
    title = "New Zealand Superannuation drove most benefit-spending growth",
    subtitle = "Contribution to the nominal increase in core-Crown welfare-benefit expenses, 2010 to 2025",
    x = NULL,
    y = NULL,
    footnotes = c(
      "Categories bridge benefit reforms: 2010 Jobseeker-related support combines Unemployment and Sickness Benefits; disability combines Invalids Benefit and Disability Allowance.",
      "2010 is actual; 2025 is a Budget 2025 forecast. Contributions include indexation and policy changes as well as recipient growth."
    ),
    sources = "New Zealand Treasury, HYEFU 2010 and BEFU 2025; e61 calculations"
  ) +
  theme_e61()
save_chart(p13, "13_welfare_benefit_contribution_2010_2025")

# 14. Recipient counts help distinguish demographic/caseload growth from the
# nominal spending increase, although they do not isolate rate or policy effects.
welfare_recipient_plot <- welfare_recipients |>
  select(recipient_group, recipients_thousands_2010, recipients_thousands_2025) |>
  pivot_longer(
    starts_with("recipients_thousands_"),
    names_to = "year",
    values_to = "recipients_thousands"
  ) |>
  mutate(
    year = recode(year, "recipients_thousands_2010" = "2010", "recipients_thousands_2025" = "2025"),
    recipient_group = reorder(recipient_group, recipients_thousands)
  )

p14 <- ggplot(welfare_recipient_plot, aes(recipients_thousands, recipient_group, group = recipient_group)) +
  geom_line(colour = "grey65", linewidth = 1.2) +
  geom_point(aes(colour = year), size = 3.2) +
  scale_colour_manual(values = c("2010" = e61_colours[4], "2025" = e61_colours[2])) +
  scale_x_continuous(labels = label_number(suffix = "k", accuracy = 100), expand = expansion(mult = c(0.02, 0.08))) +
  labs_e61(
    title = "NZ Superannuation had the largest recipient increase",
    subtitle = "Recipients of selected benefits, thousands, 2010 actual and 2025 forecast",
    x = NULL,
    y = NULL,
    footnotes = c(
      "Jobseeker-related combines Unemployment and Sickness Benefits in 2010; disability compares Invalids Benefit with Supported Living Payment.",
      "Recipient counts explain only part of expenditure growth; payment rates, indexation and policy changes also matter."
    ),
    sources = "New Zealand Treasury, HYEFU 2010 and BEFU 2025"
  ) +
  theme_e61(legend = "bottom")
save_chart(p14, "14_welfare_recipient_counts_2010_2025")

# 15. The closest available detailed explanation for economic affairs. The
# denominator is the increase across these three core-Crown classes, not the
# broader general-government COFOG total.
economic_detail_plot <- treasury_economic_detail |>
  mutate(
    subcategory = recode(
      subcategory,
      "Non-departmental outputs and employment initiatives" = "Non-departmental outputs/employment",
      "Other transport programmes and one-offs" = "Other programmes and one-offs",
      "North Island weather-event support" = "Weather-event support",
      "Other primary-services expenses" = "Other expenses"
    )
  )

make_economic_detail_plot <- function(class_name, title) {
  plot_data <- economic_detail_plot |>
    filter(functional_class == class_name) |>
    mutate(subcategory = reorder(subcategory, contribution_to_mapped_economic_affairs_increase_percent))

  ggplot(plot_data, aes(contribution_to_mapped_economic_affairs_increase_percent, subcategory)) +
    geom_col(fill = e61_colours[2], width = 0.7) +
    geom_text(
      aes(label = label_number(suffix = "%", accuracy = 0.1)(contribution_to_mapped_economic_affairs_increase_percent)),
      hjust = -0.1,
      size = 3.5
    ) +
    scale_x_continuous(
      breaks = breaks_extended(5),
      labels = label_number(suffix = "%", accuracy = 1),
      expand = expansion(mult = c(0, 0.35))
    ) +
    labs_e61(
      title = title,
      subtitle = "Contribution to growth across related core-Crown economic-affairs classes, 2010 to 2025",
      x = NULL,
      y = NULL,
      footnotes = c(
        "The broader Stats NZ COFOG increase was 76% operating expenditure and 24% net acquisition of non-financial assets.",
        "Treasury classes exclude local government and are not second-level COFOG. 2010 is actual; 2025 is a Budget 2025 forecast."
      ),
      sources = "Stats NZ, Government Finance Statistics; New Zealand Treasury, HYEFU 2010 and BEFU 2025; e61 calculations"
    ) +
    theme_e61()
}

p15a <- make_economic_detail_plot("Transport and communications", "Road transport was the largest identifiable contributor")
save_chart(p15a, "15a_economic_affairs_transport_contribution")

p15b <- make_economic_detail_plot("Economic and industrial services", "Industry-support growth was spread across broad programmes")
save_chart(p15b, "15b_economic_affairs_industry_contribution")

p15c <- make_economic_detail_plot("Primary services", "Departmental spending drove primary-services growth")
save_chart(p15c, "15c_economic_affairs_primary_services_contribution")

message("Charts written to: ", figure_dir)

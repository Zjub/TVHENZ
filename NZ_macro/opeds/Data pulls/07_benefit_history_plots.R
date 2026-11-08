# Additional charts loaded by 04_make_topic_plots.R before its gallery export.
# Uses that script's save_chart(), paths, styles and source manifest.
stopifnot(exists("save_chart"), exists("load_data"))
long_counts <- load_data("main_benefit_history")
long_js <- load_data("jobseeker_history")
long_nzs <- load_data("nzs_history")
long_groups <- load_data("benefit_history_groups")
long_totals <- load_data("benefit_expense_history_totals")
archival_counts <- load_data("archival_benefit_counts")
archival_expenses <- load_data("archival_benefit_expenses_deflated")
history_macro <- load_data("benefit_history_prices_population")
count_sources <- c("msd_national_2013", "msd_national_2018", "msd_national_2021", "msd_benefits")
expense_sources <- c("treasury_hyefu_2005", "treasury_hyefu_2010", "treasury_hyefu_2015", "treasury_hyefu_2020", "treasury_expenses")
pension_sources <- c("msd_statistical_2012", "msd_national_2013", "msd_nzs_2018", "msd_nzs_2021", "msd_nzs_2026")
reform_date <- as.Date("2013-07-15")
reform_line <- function() geom_vline(xintercept = reform_date, linetype = "dashed", colour = "grey40")
brief_expense_note <- "Actuals from five Treasury vintages; accounting, classification and programme coverage change. The main basket is approximate across the 2013 reform."

d <- long_counts |> filter(programme != "Total")
save_chart("B11_long_main_benefit_counts", d,
  ggplot(d, aes(date, recipients / 1000, colour = programme)) + geom_line(linewidth = .9) + reform_line() +
    ggplot2::labs(x = NULL, y = "Working-age recipients (thousands)", colour = NULL),
  "Main benefit receipt over eighteen years", "72 consecutive quarters, September 2008-June 2026; dashed line marks July 2013 reform",
  "Pre-July 2013 figures are MSD estimates mapping old benefits into current categories. Each overlap quarter uses its newest downloaded vintage. Benefit receipt is not unemployment.",
  count_sources, "Observed counts / official backcast")
d <- long_js
save_chart("B12_long_jobseeker_subgroups", d,
  ggplot(d, aes(date, recipients / 1000, colour = subgroup)) + geom_line(linewidth = 1) + reform_line() +
    ggplot2::labs(x = NULL, y = "Working-age Jobseeker recipients (thousands)", colour = NULL),
  "Work-ready and health-related receipt since 2008", "MSD current-category counts and official pre-reform backcasts",
  "The health-related group is not a count of people unemployed and ready for work. Some recipients work. Pre-reform figures are backcasts; subgroup counts do not identify subgroup expenditure.",
  count_sources, "Observed counts / official backcast")
d <- long_js |> group_by(date) |> mutate(share_percent = 100 * recipients / sum(recipients)) |> ungroup() |>
  filter(subgroup == "Health condition or disability")
save_chart("B13_long_health_related_share", d,
  ggplot(d, aes(date, share_percent)) + geom_line(linewidth = 1, colour = colours[1]) + reform_line() +
    scale_y_continuous(labels = percent) + ggplot2::labs(x = NULL, y = "Health-related share of Jobseeker recipients"),
  "The changing mix of Jobseeker recipients", "Health condition or disability subgroup, September 2008-June 2026",
  "This is a recipient share, not a spending share. Pre-July 2013 categories are estimated by MSD. The series alone cannot explain changes in health, eligibility or administration.",
  count_sources, "Derived measure / official backcast")
d <- long_counts |> filter(programme != "Total") |> group_by(programme) |> arrange(date, .by_group = TRUE) |>
  mutate(index = 100 * recipients / first(recipients)) |> ungroup()
save_chart("B14_long_caseload_indices", d,
  ggplot(d, aes(date, index, colour = programme)) + geom_line(linewidth = .9) + reform_line() +
    geom_hline(yintercept = 100, linetype = "dotted", colour = "grey50") +
    ggplot2::labs(x = NULL, y = "Recipient index (September 2008 = 100)", colour = NULL),
  "How caseloads changed from the same starting point", "Working-age main benefits, indexed to September 2008",
  "Indexing compares proportional changes, not fiscal importance. The starting observation is a backcast and reflects its own economic conditions; small categories can show large percentage swings.",
  count_sources, "Derived measure / official backcast")
d <- long_counts |> filter(programme == "Total", format(date, "%m-%d") == "06-30") |>
  mutate(year = as.integer(format(date, "%Y"))) |> left_join(history_macro, by = "year") |>
  filter(!is.na(population)) |> mutate(per_1000_residents = 1000 * recipients / population)
save_chart("B15_long_receipt_population_ratio", d,
  ggplot(d, aes(year, per_1000_residents)) + geom_line(linewidth = 1, colour = colours[1]) + geom_point(size = 2) +
    geom_vline(xintercept = 2013.54, linetype = "dashed", colour = "grey50") +
    ggplot2::labs(x = "June stock year", y = "Working-age main-benefit recipients per 1,000 residents"),
  "Benefit receipt relative to population", "June 2009-June 2025; denominator is all residents, not the working-age population",
  "This is not an unemployment rate or the percentage of working-age people on benefits. June stocks and annual population do not align exactly; 2026 has no population observation in this API snapshot.",
  c(count_sources, "wb_history_population"), "Derived measure / official backcast")
d <- long_counts |> filter(programme == "Other Main Benefits") |>
  mutate(quarter = factor(as.integer(format(date, "%m")) / 3, levels = 1:4, labels = c("March", "June", "September", "December")))
save_chart("B16_other_benefit_seasonality", d,
  ggplot(d, aes(quarter, recipients / 1000, fill = quarter)) + geom_boxplot(show.legend = FALSE) +
    geom_jitter(width = .1, alpha = .45, size = 1) + ggplot2::labs(x = "Quarter-end month", y = "Other main-benefit recipients (thousands)"),
  "Why quarter-end comparisons can mislead", "Distribution of other main-benefit counts across quarters, 2008-2026",
  "Student hardship and other seasonal programmes affect this category. Boxes mix years with different policies and economic conditions; they describe seasonality, not a causal seasonal effect.",
  count_sources, "Descriptive distribution / official backcast")

d <- long_groups
save_chart("B17_long_expense_composition", d,
  ggplot(d, aes(year, value_millions / 1000, fill = group)) + geom_area() +
    geom_vline(xintercept = 2013.5, linetype = "dashed", colour = "grey40") +
    ggplot2::labs(x = "Fiscal year ending June", y = "Gross benefit expenses (NZ$ billions)", fill = NULL),
  "The benefit budget from 2001 to 2025", "Annual actual expenses; temporary COVID and cost-of-living programmes separated",
  paste(brief_expense_note, "The residual is shown rather than silently excluded. These are nominal gross expenses, not payments solely to people without jobs."),
  expense_sources, "Actual expense history / approximate grouping")
d <- long_groups
save_chart("B18_long_expenses_gdp", d,
  ggplot(d, aes(year, percent_gdp, fill = group)) + geom_area() +
    scale_y_continuous(labels = percent) + geom_vline(xintercept = 2013.5, linetype = "dashed", colour = "grey40") +
    ggplot2::labs(x = "Fiscal year ending June", y = "Gross benefit expenses / GDP", fill = NULL),
  "Benefit spending relative to the economy", "Actual annual expenses, 2001-2025; matching Treasury fiscal-year GDP denominators",
  paste(brief_expense_note, "Recent GDP comes from BEFU 2026; earlier GDP from the fiscal-history workbook. A COVID spike includes employer wage subsidies."),
  c(expense_sources, "treasury_macro", "treasury_fiscal_history"), "Derived expense history / approximate grouping")
d <- long_groups |> filter(group %in% c("NZ Superannuation", "Main income-replacement basket", "Family tax credits and payments", "Housing assistance"))
save_chart("B19_long_budget_shares", d,
  ggplot(d, aes(year, share_benefits_percent, colour = group)) + geom_line(linewidth = 1) +
    geom_vline(xintercept = 2013.5, linetype = "dashed", colour = "grey40") +
    scale_y_continuous(labels = percent) + ggplot2::labs(x = "Fiscal year ending June", y = "Share of total gross benefit expenses", colour = NULL),
  "Which parts of the benefit budget grew in share?", "Four selected baskets as shares of all benefit expenses, 2001-2025",
  paste(brief_expense_note, "Selected shares need not sum to 100%. Temporary programmes enlarge the denominator; Jobseeker work-ready spending is not separately observed."),
  expense_sources, "Derived expense history / approximate grouping")
save_chart("B20_long_real_expenses", d,
  ggplot(d, aes(year, real_2025_millions / 1000, colour = group)) + geom_line(linewidth = 1) +
    geom_vline(xintercept = 2013.5, linetype = "dashed", colour = "grey40") +
    ggplot2::labs(x = "Fiscal year ending June", y = "Expense in 2025 purchasing power (NZ$ billions)", colour = NULL),
  "Long-run benefit spending after inflation", "Selected baskets, 2001-2025; calendar annual-average CPI deflator",
  paste(brief_expense_note, "Calendar-year CPI approximates fiscal-year purchasing power. Real spending changes can reflect caseload, rates and composition."),
  c(expense_sources, "wb_history_cpi"), "Derived expense history / approximate grouping")
d <- long_groups |> filter(group %in% c("NZ Superannuation", "Main income-replacement basket"))
save_chart("B21_long_real_expense_per_resident", d,
  ggplot(d, aes(year, per_resident_real_2025, colour = group)) + geom_line(linewidth = 1) +
    geom_vline(xintercept = 2013.5, linetype = "dashed", colour = "grey40") +
    ggplot2::labs(x = "Fiscal year ending June", y = "Annual expense per resident (2025 NZ$)", colour = NULL),
  "Spending adjusted for prices and population", "NZ Super and an approximate main income-replacement basket, 2001-2025",
  "The denominator is all residents, not recipients. The main basket combines Community Wage/legacy benefits with Jobseeker, Supported Living and Sole Parent Support; reform transfers and coverage prevent exact equivalence.",
  c(expense_sources, "wb_history_cpi", "wb_history_population"), "Derived expense history / approximate grouping")
d <- long_groups |> filter(year %in% c(2001, 2025)) |>
  select(year, group, value_millions, real_2025_millions) |>
  pivot_longer(c(value_millions, real_2025_millions), names_to = "basis", values_to = "value") |>
  pivot_wider(names_from = year, values_from = value) |>
  mutate(change_millions = coalesce(`2025`, 0) - coalesce(`2001`, 0),
         basis = recode(basis, value_millions = "Nominal change", real_2025_millions = "Change in 2025 purchasing power"))
save_chart("B22_long_growth_decomposition", d,
  ggplot(d, aes(reorder(wrap(group, 24), change_millions), change_millions / 1000, fill = group)) +
    geom_col(show.legend = FALSE) + coord_flip() + facet_wrap(~basis) +
    ggplot2::labs(x = NULL, y = "Change in annual expenses (NZ$ billions)"),
  "Contributions to the 2001-2025 expense increase", "Endpoint changes in nominal and CPI-adjusted expenses",
  "This is an accounting decomposition, not a policy attribution. COVID programmes did not exist in 2001; basket definitions and accounting differ across vintages. The full component mapping is exported.",
  c(expense_sources, "wb_history_cpi"), "Accounting decomposition / approximate grouping")
d <- load_data("nzs_expense_caseload_decomposition") |> arrange(year) |>
  mutate(expense_index = 100 * real_2025_millions / first(real_2025_millions),
         caseload_index = 100 * average_recipients / first(average_recipients),
         per_recipient_index = 100 * real_expense_per_average_recipient / first(real_expense_per_average_recipient)) |>
  pivot_longer(c(expense_index, caseload_index, per_recipient_index), names_to = "component", values_to = "index") |>
  mutate(component = recode(component, expense_index = "Total real NZ Super expense", caseload_index = "Average recipient stock",
                            per_recipient_index = "Real expense per average recipient"))
save_chart("B23_nzs_caseload_expense_decomposition", d,
  ggplot(d, aes(year, index, colour = component)) + geom_line(linewidth = 1) +
    ggplot2::labs(x = "Fiscal year ending June", y = "Index (2009 = 100)", colour = NULL),
  "NZ Super spending: caseload and spending per person", "2009-2025; caseload approximated by the mean of four quarter-end stocks",
  "Total real expense equals average stock times real expense per average recipient. The latter is a flow/stock proxy, not a statutory benefit rate; eligibility, recipient composition, timing and revisions matter.",
  c(expense_sources, pension_sources, "wb_history_cpi"), "Accounting decomposition / caseload proxy")

# The archival tables are shown on their own coverage. No historical Family
# Benefit figures of uncertain accuracy are used in these plots.
d <- archival_counts |> filter(programme %in% c("Unemployment-related benefits", "Sickness Benefit", "Invalids Benefit", "Domestic Purposes Benefit", "Widows Benefit"), !is.na(recipients))
save_chart("B24_archival_recipient_counts", d,
  ggplot(d, aes(year, recipients / 1000, colour = programme)) + geom_line(linewidth = .8) + geom_point(size = 1.6) +
    ggplot2::labs(x = "Reported stock year", y = "Recipients (thousands)", colour = NULL),
  "Benefit receipt in the longer historical record", "Selected published observations, 1940-2012; original categories and age coverage",
  "Points are observed; missing years are not imputed. Before 1990 stocks refer to March, then June. Definitions, age coverage and eligibility changed; these are not modern working-age backcasts or unemployment counts.",
  "msd_statistical_2012", "Archival statistics with definition breaks")
d <- load_data("pension_recipient_long_history")
save_chart("B25_pension_recipients_1940_2026", d,
  ggplot(d, aes(year, recipients / 1000, colour = regime, group = interaction(regime, counting_regime))) +
    geom_line(linewidth = .9) + geom_point(size = 1.5) +
    geom_vline(xintercept = 1996, linetype = "dashed", colour = "grey50") +
    ggplot2::labs(x = "Reported stock year", y = "Pension / NZ Super recipients (thousands)", colour = NULL),
  "Pension recipients from 1940 to 2026", "Archival pension series through 2008; modern June NZ Super counts from 2009",
  "Earlier observations are sparse; pre-1990 stocks are March. Non-qualified spouses are included from 1996 (dashed line). Pension schemes and eligibility ages changed; gaps in lines mark source/counting breaks.",
  pension_sources, "Archival / modern series splice with breaks")
d <- load_data("pension_recipient_long_history") |> filter(!is.na(per_1000_residents))
save_chart("B26_pension_recipients_population", d,
  ggplot(d, aes(year, per_1000_residents, colour = regime, group = interaction(regime, counting_regime))) +
    geom_line(linewidth = .9) + geom_point(size = 1.6) +
    geom_vline(xintercept = 1996, linetype = "dashed", colour = "grey50") +
    ggplot2::labs(x = "Reported stock year", y = "Pension recipients per 1,000 residents", colour = NULL),
  "Pension receipt relative to population since 1960", "Available recipient observations divided by annual total population, through 2025",
  "Not an eligibility or coverage rate for older people. Eligibility-age changes and non-qualified spouse counting affect the ratio. Annual population approximates March/June stock dates; source breaks remain visible.",
  c(pension_sources, "wb_history_population"), "Derived archival / modern series splice")
d <- archival_expenses |> filter(programme %in% c("Unemployment Benefit", "Sickness Benefit", "Invalids Benefit", "Domestic Purposes Benefit", "NZ Superannuation / predecessor pensions"), !is.na(value_millions))
save_chart("B27_archival_nominal_expenses", d,
  ggplot(d, aes(year, value_millions / 1000, colour = programme, group = interaction(programme, accounting_basis))) +
    geom_line(linewidth = .9) + geom_point(size = 1.6) + geom_vline(xintercept = 1994.5, linetype = "dashed", colour = "grey50") +
    ggplot2::labs(x = "Reported expenditure year", y = "NZ$ billions, as published", colour = NULL),
  "The archival expenditure record", "Selected observations from MSD Table 7.2, 1940-2002; original coverage",
  "These amounts include supplementary assistance attributed to benefit categories and are not spliced into Treasury's modern expenses. March years before 1990; cash through 1994 and accrual thereafter. Missing cells are not zero.",
  "msd_statistical_2002", "Archival expenses with coverage/accounting breaks")
d <- d |> filter(!is.na(real_2025_millions))
save_chart("B28_archival_real_expenses", d,
  ggplot(d, aes(year, real_2025_millions / 1000, colour = programme, group = interaction(programme, accounting_basis))) +
    geom_line(linewidth = .9) + geom_point(size = 1.6) + geom_vline(xintercept = 1994.5, linetype = "dashed", colour = "grey50") +
    ggplot2::labs(x = "Reported expenditure year", y = "Expense in 2025 purchasing power (NZ$ billions)", colour = NULL),
  "Archival benefit spending after inflation", "Available observations, 1960-2002; calendar CPI used as an approximate deflator",
  "Supplementary assistance is included within categories. Inflation adjustment does not fix eligibility, tax, reporting-period or cash/accrual changes. These are separate from modern Treasury expense totals.",
  c("msd_statistical_2002", "wb_history_cpi"), "Derived archival expenses with breaks")
d <- load_data("social_welfare_gsf_history")
save_chart("B29_social_welfare_gsf_long_gdp", d,
  ggplot(d, aes(year, percent_gdp, colour = accounting_basis, group = accounting_basis)) +
    geom_line(linewidth = .9) + geom_point(size = 1.4) + scale_y_continuous(labels = percent) +
    ggplot2::labs(x = "Reported fiscal year", y = "Social security, welfare and GSF / GDP", colour = NULL),
  "The broader welfare function over fifty years", "Treasury fiscal-history series, 1972-2025; accounting regimes kept separate",
  "Includes the Government Superannuation Fund and wider welfare expenses, not just benefits or unemployment support. Cash/March, cash/June and accrual regimes differ; early GST treatment and valuation changes affect comparisons.",
  "treasury_fiscal_history", "Broad functional history with accounting breaks")

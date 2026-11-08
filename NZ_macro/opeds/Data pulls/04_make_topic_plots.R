# Make a browsable evidence pack for five NZ op-ed topics.
# Run 03_pull_topic_data.R first. Every chart exports its own plotting data.
required <- c("dplyr", "ggplot2", "purrr", "readr", "scales", "tidyr", "svglite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing R packages: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(purrr); library(readr); library(tidyr)})
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[1]))) else normalizePath("Data pulls")
data_dir <- file.path(root, "processed", "op_ed_topics")
figure_dir <- file.path(root, "figures", "op_ed_topics")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
load_data <- function(name) read_csv(file.path(data_dir, paste0(name, ".csv")), show_col_types = FALSE)
if (!file.exists(file.path(data_dir, "validation.csv"))) stop("Run 03_pull_topic_data.R first.")
wb <- load_data("world_bank")
macro <- load_data("fiscal_denominators")
benefits <- load_data("benefit_expenses")
benefit_total <- load_data("benefit_total")
counts <- load_data("benefit_recipients")
js <- load_data("jobseeker_subgroups")
duration <- load_data("jobseeker_duration")
fte <- load_data("public_service_fte")
agencies <- load_data("public_service_agencies")
sectors <- load_data("workforce_sectors")
regions <- load_data("workforce_regions")
cc <- load_data("contractors")
cc_share <- load_data("contractors_share")
oia <- load_data("oia_service_metrics")
rd <- load_data("rd_expenditure")
register <- load_data("tax_expenditure_register")
tax_system <- load_data("tax_system_spending")
games <- load_data("game_rebate_allocations")
params <- load_data("parameters")
sources_manifest <- load_data("source_manifest")
if (file.exists(file.path(data_dir, "benefit_history_source_manifest.csv"))) {
  sources_manifest <- bind_rows(sources_manifest, load_data("benefit_history_source_manifest"))
}
catalogue <- list()
topic_names <- c(D = "Digital services taxes", T = "Tax streamlining and business support",
                 I = "Taxation of inflation", P = "Public servants and service value", B = "Benefit spending")
colours <- c("#148B8B", "#DB7B00", "#54BFC7", "#B40000", "#5875A4", "#6C635A")
percent <- scales::label_number(suffix = "%", accuracy = .1)
comma <- scales::label_number(big.mark = ",")
wrap <- function(x, width = 40) vapply(x, function(t) paste(strwrap(t, width = width), collapse = "\n"), character(1))

save_chart <- function(id, data, plot, title, subtitle, note, source_ids, kind = "Direct measure") {
  message("Saving ", id, ": ", title)
  official_sources <- sources_manifest |> filter(source_id %in% source_ids)
  stopifnot(nrow(official_sources) > 0, nrow(data) > 0)
  source_caption <- if (length(source_ids) > 4) {
    paste0(length(source_ids), " official releases; full URLs and vintages in chart catalogue and gallery")
  } else paste(unique(official_sources$description), collapse = "; ")
  caption <- paste0(paste(strwrap(paste(kind, "|", note), width = 130), collapse = "\n"), "\nSources: ",
                    paste(strwrap(source_caption, width = 125), collapse = "\n"))
  plot <- plot + ggplot2::labs(title = title, subtitle = paste(strwrap(subtitle, 110), collapse = "\n"), caption = caption) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE),
           fill = guide_legend(nrow = 2, byrow = TRUE), shape = guide_legend(nrow = 1)) +
    theme_minimal(base_size = 12, base_family = "sans") +
    theme(plot.title = element_text(size = 18, face = "bold", hjust = 0),
          plot.subtitle = element_text(size = 12, hjust = 0),
          plot.caption = element_text(size = 9, hjust = 0, lineheight = 1.15),
          plot.title.position = "plot", plot.caption.position = "plot",
          axis.title.y = element_text(size = 12, margin = margin(r = 12)),
          axis.title.x = element_text(size = 12), axis.text = element_text(size = 10),
          legend.position = "bottom", legend.box = "vertical", legend.title = element_text(size = 10),
          legend.text = element_text(size = 9), strip.text = element_text(size = 11),
          plot.margin = margin(14, 18, 14, 14))
  # Standard ggplot export preserves explicit axis units and analytical scales.
  height <- if (id %in% c("T03_estimated_tax_expenditures", "P09_agency_staffing", "B01_benefit_expense_composition")) 10 else 8
  ggplot2::ggsave(file.path(figure_dir, paste0(id, ".png")), plot, width = 12, height = height, dpi = 180, bg = "white")
  ggplot2::ggsave(file.path(figure_dir, paste0(id, ".svg")), plot, width = 12, height = height, bg = "white")
  write_csv(data, file.path(figure_dir, paste0(id, "_data.csv")), na = "")
  catalogue[[length(catalogue) + 1]] <<- tibble(
    id = id, topic = unname(topic_names[substr(id, 1, 1)]), title = title,
    subtitle = subtitle, measurement_type = kind, interpretation = note,
    sources = paste(official_sources$url, collapse = " | "),
    png = paste0(id, ".png"), svg = paste0(id, ".svg"), data_csv = paste0(id, "_data.csv")
  )
}

# DIGITAL SERVICES: official economic context is kept distinct from the tax base.
d <- wb |> filter(iso3 %in% c("NZL", "AUS"), measure == "ict_exports_usd")
save_chart("D01_ict_exports", d,
  ggplot(d, aes(year, value / 1e9, colour = country)) + geom_line(linewidth = .8) +
    geom_point(size = 1.5) + ggplot2::labs(x = NULL, y = "Current US$ billions"),
  "ICT services exports", "New Zealand and Australia; annual balance-of-payments data",
  "Exports are outward sales, not the inbound revenue a NZ DST would tax. Current US dollars are affected by exchange rates.",
  "wb_ict_exports_usd", "Context proxy")
d <- wb |> filter(measure == "ict_exports_share")
save_chart("D02_ict_export_share", d,
  ggplot(d, aes(year, value, colour = country)) + geom_line(linewidth = .8) +
    scale_y_continuous(labels = percent) + ggplot2::labs(x = NULL, y = "Share of service exports"),
  "ICT in the services export mix", "Share of all service exports; each series ends at its last available observation",
  "ICT services are not identical to the targeted advertising, marketplace, social-media and user-data services in the withdrawn bill.",
  "wb_ict_exports_share", "Context proxy")
d <- wb |> filter(measure == "internet_users")
save_chart("D03_internet_use", d,
  ggplot(d, aes(year, value, colour = country)) + geom_line(linewidth = .8) +
    scale_y_continuous(labels = percent, limits = c(0, 100)) + ggplot2::labs(x = NULL, y = "Individuals using the internet"),
  "How widespread is internet use?", "Population share; New Zealand, Australia and the OECD aggregate",
  "Internet use measures reach. It gives neither platform revenue nor a corporate tax gap.", "wb_internet_users", "Context proxy")
d <- load_data("dst_revenue_scenarios") |> mutate(rate_label = paste0(100 * rate, "%"))
save_chart("D04_dst_revenue_scenarios", d,
  ggplot(d, aes(revenue_billion, revenue_millions, colour = rate_label)) + geom_line(linewidth = .9) +
    ggplot2::labs(x = "Assumed eligible NZ revenue (NZ$ billions)", y = "Gross annual tax (NZ$ millions)", colour = NULL),
  "DST revenue depends on its measured base", "Hypothetical gross revenue tax; the withdrawn bill proposed 3%",
  "NZ has no enacted DST in this snapshot. The base is assumed, not measured; no avoidance, incidence, compliance or trade response is modelled.",
  c("dst_status", "dst_ris"), "Scenario")
d <- load_data("dst_margin_scenarios") |> filter(pass_through == 0)
save_chart("D05_dst_profit_burden", d,
  ggplot(d, aes(100 * profit_margin, dst_as_percent_pre_dst_profit)) + geom_line(colour = colours[1], linewidth = 1) +
    geom_hline(yintercept = 100, linetype = "dashed", colour = colours[2]) +
    ggplot2::labs(x = "Pre-DST profit margin (%)", y = "DST as % of pre-DST profit"),
  "A revenue tax varies with profit margins", "3% of eligible revenue; no pass-through to customers",
  "Illustrative margins, not company accounts. This is a standalone DST burden, not total tax or a corporate effective tax rate.",
  "dst_ris", "Scenario")
d <- load_data("dst_margin_scenarios") |> filter(profit_margin <= .25) |>
  mutate(pass_through_label = paste0(100 * pass_through, "% passed through"))
save_chart("D06_dst_incidence", d,
  ggplot(d, aes(100 * profit_margin, dst_as_percent_pre_dst_profit, colour = pass_through_label)) + geom_line(linewidth = .9) +
    ggplot2::labs(x = "Pre-DST profit margin (%)", y = "Firm's retained burden / pre-DST profit (%)", colour = NULL),
  "Who bears the DST changes the result", "A 3% tax with assumed customer pass-through of 0%, 50% or 100%",
  "The assumed passed-through fraction is borne by customers; demand and the eligible revenue base are held fixed. These are not incidence estimates.",
  "dst_ris", "Scenario")
d <- tibble(historical_estimate_millions = c(86, 90),
            implied_base_billion = historical_estimate_millions / .03 / 1000,
            percent_2025_tax_revenue = 100 * historical_estimate_millions / macro$tax_millions[macro$year == 2025])
save_chart("D07_historical_dst_scale", d,
  ggplot(d, aes(historical_estimate_millions, percent_2025_tax_revenue)) +
    geom_line(colour = colours[1], linewidth = 1) + geom_point(size = 3, colour = colours[1]) +
    scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = .001), limits = c(0, NA)) +
    ggplot2::labs(x = "Historical 2023 RIS annual revenue estimate (NZ$ millions)", y = "Share of 2025 core Crown tax revenue"),
  "Putting the historic DST estimate in scale", "The 2023 RIS estimated roughly NZ$86-90 million per assessment year",
  "A scale comparison against 2025 actual tax revenue, not an updated forecast. The implied 3% tax base is in the exported data; it is not observed.",
  c("dst_ris", "treasury_macro"), "Historical estimate / scale comparison")

# TAX STREAMLINING: tax expenditures, appropriated credits and rebates are
# reported separately. No estimate of the gains from repeal is manufactured.
d <- register |> count(estimated) |> mutate(coverage = if_else(estimated, "Annual dollar estimate published", "No annual dollar estimate extracted"))
save_chart("T01_cost_disclosure", d,
  ggplot(d, aes(coverage, n, fill = coverage)) + geom_col(show.legend = FALSE) +
    geom_text(aes(label = n), vjust = -.3) + scale_x_discrete(labels = wrap) +
    ggplot2::labs(x = NULL, y = "Number of listed provisions"),
  "How much tax expenditure is costed?", "52 provisions listed in Tables 4 and 5 of the 2026 statement",
  "An unestimated cost is not zero. This is disclosure coverage, not an exhaustive count of concessions, complexity or repeal savings.",
  "treasury_tax_expenditures")
d <- register |> count(type, estimated) |> mutate(coverage = if_else(estimated, "Annual estimate", "Unestimated"))
save_chart("T02_provision_types", d,
  ggplot(d, aes(type, n, fill = coverage)) + geom_col() + coord_flip() + ggplot2::labs(x = NULL, y = "Listed provisions", fill = NULL),
  "What kinds of tax provisions are listed?", "Published classification and availability of annual cost estimates",
  "Classification follows Treasury's benchmark. A timing deduction differs from a permanent tax reduction; provision counts do not measure welfare costs.",
  "treasury_tax_expenditures")
d <- register |> filter(estimated) |> arrange(desc(annual_estimate_millions)) |> slice_head(n = 12) |>
  mutate(label = paste0(wrap(provision, 38), "\n(review ", last_reviewed, ")"))
save_chart("T03_estimated_tax_expenditures", d,
  ggplot(d, aes(reorder(label, annual_estimate_millions), annual_estimate_millions)) +
    geom_col(fill = colours[1]) + coord_flip() + ggplot2::labs(x = NULL, y = "Published NZ$ millions per annum"),
  "Larger published tax-expenditure estimates", "Estimates have different review vintages; they are not a common-year total",
  "Revenue-forgone estimates hold behaviour fixed and can overlap. The statement's provision estimates can differ from its newer Table 3 series. Do not sum them as repeal savings.",
  "treasury_tax_expenditures")
d <- tax_system |> filter(year_basis == "income")
save_chart("T04_measured_tax_credits", d,
  ggplot(d, aes(year_end, value_millions, colour = programme, group = programme)) + geom_line(linewidth = .8) +
    geom_point(aes(shape = status), size = 2) + ggplot2::labs(x = "Income tax year ending March", y = "NZ$ millions", colour = NULL),
  "Regularly measured credits and deductions", "Treasury Table 3; actuals, estimated actuals and forecast marked by points",
  "Income-tax years differ from fiscal years. This selection omits appropriated spending through the tax system, such as the R&D incentive.",
  "treasury_tax_expenditures")
d <- tax_system |> filter(programme == "R&D tax incentive")
save_chart("T05_rdti_expense", d,
  ggplot(d, aes(year_end, value_millions)) + geom_line(colour = colours[1], linewidth = .9) +
    geom_point(aes(shape = status), size = 3, colour = colours[1]) +
    ggplot2::labs(x = "Fiscal year ending June", y = "NZ$ millions"),
  "R&D tax incentive expense", "Appropriated spending through the tax system, as recorded in the 2026 statement",
  "Claims, accrual timing and revisions affect yearly costs. 2025 is an estimate and 2026 a forecast; this is not the additional R&D generated by the credit.",
  "treasury_tax_expenditures")
d <- rd |> filter(breakdown == "Sector", !is.na(value_millions))
save_chart("T06_rd_by_sector", d,
  ggplot(d, aes(year, value_millions / 1000, colour = category)) + geom_line(linewidth = .9) + geom_point(size = 2) +
    ggplot2::labs(x = NULL, y = "NZ$ billions", colour = NULL),
  "Research spending by sector", "Stats NZ total internal R&D expenditure; available survey years",
  "Business is surveyed annually; government and higher education less frequently. Lines connect available observations, not annual interpolated values. No causal effect is inferred.",
  "stats_rd")
size_year <- max(rd$year[rd$breakdown == "Size" & !is.na(rd$value_millions)])
d <- rd |> filter(breakdown == "Size", year == size_year, !is.na(value_millions))
save_chart("T07_rd_by_firm_size", d,
  ggplot(d, aes(reorder(category, value_millions), value_millions)) + geom_col(fill = colours[1]) + coord_flip() +
    ggplot2::labs(x = NULL, y = "Internal R&D (NZ$ millions)"),
  "Which sizes of organisation undertake R&D?", paste("All-sector R&D expenditure by employee-size band,", size_year),
  "Includes business, government and higher education. The size breakdown is unavailable in 2025; spending concentration is not subsidy additionality. Missing cells are not zero.",
  "stats_rd")
d <- rd |> filter(breakdown == "Published_Industry", year == 2025, !is.na(value_millions)) |>
  arrange(desc(value_millions)) |> slice_head(n = 10)
save_chart("T08_rd_by_industry", d,
  ggplot(d, aes(reorder(wrap(category, 32), value_millions), value_millions)) + geom_col(fill = colours[1]) + coord_flip() +
    ggplot2::labs(x = NULL, y = "Internal R&D (NZ$ millions)"),
  "Where business research takes place", "Ten largest available published industry categories, 2025",
  "Survey categories and sampling uncertainty are retained in the chart data. Suppression is not zero. R&D credit eligibility is narrower than the survey definition.",
  "stats_rd")
d <- wb |> filter(measure == "rd_gdp")
save_chart("T09_rd_international", d,
  ggplot(d, aes(year, value, colour = country)) + geom_line(linewidth = .9) + geom_point(size = 1.4) +
    scale_y_continuous(labels = percent) + ggplot2::labs(x = NULL, y = "R&D expenditure / GDP"),
  "Research spending in international context", "New Zealand, Australia and OECD aggregate; available years",
  "R&D intensity is a descriptive comparison. It does not isolate the effects of credits, industry structure, institutions or income differences.", "wb_rd_gdp")
d <- load_data("screen_budget_2027")
save_chart("T10_screen_budget", d,
  ggplot(d, aes(programme, value_millions, fill = programme)) + geom_col(show.legend = FALSE) +
    geom_text(aes(label = value_millions), vjust = -.3) + ggplot2::labs(x = NULL, y = "NZ$ millions"),
  "Domestic and international screen support", "2026/27 domestic expense forecast and international rebate appropriation",
  "These are spending programmes, not corporate tax credits. A forecast expense and an authorised appropriation are different concepts; neither is an actual payment or net fiscal cost.",
  c("treasury_expenses", "treasury_vote_business"), "Budget forecast / appropriation")
d <- load_data("screen_domestic")
save_chart("T11_domestic_screen_history", d,
  ggplot(d, aes(year, value)) + geom_line(colour = colours[1], linewidth = .9) +
    geom_point(aes(shape = status), size = 2.5) + ggplot2::labs(x = "Fiscal year ending June", y = "NZ$ millions"),
  "Domestic screen grants over time", "Treasury heritage, culture and recreation expense table",
  "This line covers domestic grants only, not the international screen rebate. 2021-2025 are actual expenses and 2026 onward are forecasts.", "treasury_expenses")
d <- games |> pivot_longer(c(allocated_millions, studios), names_to = "measure", values_to = "value") |>
  mutate(measure = recode(measure, allocated_millions = "Allocated NZ$ millions", studios = "Recipient studios"))
save_chart("T12_game_rebate_rounds", d,
  ggplot(d, aes(factor(year), value, fill = factor(year))) + geom_col(show.legend = FALSE) +
    geom_text(aes(label = value), vjust = -.3) + facet_wrap(~measure, scales = "free_y") + ggplot2::labs(x = "Allocation round", y = NULL),
  "Game rebate allocations and recipients", "NZ On Air announced rounds in 2025 and 2026",
  "Announced allocations are not fiscal-year expenses. Different recipient cohorts and no counterfactual mean these numbers cannot show whether the rebate pays for itself.",
  c("games_2025", "games_2026"))
d <- load_data("expensing_deductions")
save_chart("T13_expensing_timing", d,
  ggplot(d, aes(factor(year), deduction / 1000, fill = method)) + geom_col(position = "dodge") +
    ggplot2::labs(x = "Year since purchase (first use = year 0)", y = "Deduction (NZ$ thousands)", fill = NULL),
  "Expensing changes deduction timing", "A NZ$100,000 asset; 20% straight-line depreciation; full tax capacity assumed",
  "All schedules deduct NZ$100,000 in total. Investment Boost deducts 20% immediately and depreciates the remaining 80%; full expensing is hypothetical.",
  "investment_boost", "Scenario")
d <- load_data("expensing_present_values")
save_chart("T14_expensing_present_value", d,
  ggplot(d, aes(discount_rate * 100, pv_percent_cost, colour = method)) + geom_line(linewidth = 1) +
    ggplot2::labs(x = "Nominal discount rate (%)", y = "Present value of tax shield / asset cost (%)", colour = NULL),
  "The value of earlier deductions", "Same asset and deductions; NZ 28% company tax rate; first-use deduction at time zero",
  "The gain is timing value, not extra lifetime deductions. Discount rates and depreciation are assumed; firms unable to use losses immediately have different benefits.",
  "investment_boost", "Scenario")

# INFLATION: distinct bracket and nominal-return channels, with exact Fisher
# real-return arithmetic rather than simply subtracting inflation.
d <- wb |> filter(measure == "inflation", iso3 == "NZL")
save_chart("I01_inflation", d,
  ggplot(d, aes(year, value)) + geom_col(fill = colours[1]) + geom_hline(yintercept = 0) +
    scale_y_continuous(labels = percent) + ggplot2::labs(x = NULL, y = "Annual-average CPI inflation"),
  "The inflation entering nominal tax bases", "New Zealand annual-average consumer price inflation",
  "Annual-average inflation differs from June-quarter annual inflation. CPI is a household price index, not a universal deflator for taxable gains.", "wb_inflation")
d <- load_data("tax_brackets_real") |> filter(bracket <= 3) |> mutate(bracket = factor(bracket, labels = c("First threshold", "Second threshold", "Third threshold")))
save_chart("I02_real_brackets", d,
  ggplot(d, aes(year, threshold_real_2010, colour = bracket)) + geom_line(linewidth = 1) +
    scale_y_continuous(labels = scales::label_dollar(prefix = "$", big.mark = ",")) + ggplot2::labs(x = NULL, y = "Threshold in 2010 purchasing power", colour = NULL),
  "Tax thresholds in real dollars", "First three thresholds; schedule at 30 June and annual-average CPI",
  "The July 2024 threshold increases first appear in the June 2025 snapshot. This is a purchasing-power measure, not an aggregate revenue estimate.",
  c("tax_rates", "wb_cpi"), "Derived measure")
d <- load_data("constant_real_income_tax") |> mutate(income = paste0("$", comma(real_income_2010), " in 2010 dollars"))
save_chart("I03_constant_real_income_tax", d,
  ggplot(d, aes(year, average_tax_percent, colour = income)) + geom_line(linewidth = 1) +
    scale_y_continuous(labels = percent) + ggplot2::labs(x = NULL, y = "Income tax / gross income", colour = NULL),
  "Tax rates when real earnings stay constant", "Four illustrative earners; nominal income rises only with observed CPI",
  "June 2011 onward tax-scale snapshots; earnings in 2010 purchasing power. No ACC, credits or transfers. The 2021 39% rate is a policy change, not inflation.",
  c("tax_rates", "tax_rates_2010", "wb_cpi"), "Scenario using observed CPI")
save_chart("I04_2010_indexation_comparison", d,
  ggplot(d, aes(year, additional_tax_real_2010, colour = income)) + geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, colour = "grey50") + ggplot2::labs(x = NULL, y = "Extra annual tax (2010 NZ$)", colour = NULL),
  "Comparing tax with a 2010 indexed scale", "Actual June schedules versus fully indexing the 2010 tax schedule",
  "Includes both bracket erosion and subsequent policy changes, including the new 39% rate. A negative value would mean tax below this counterfactual. No taxpayer population is assumed.",
  c("tax_rates", "tax_rates_2010", "wb_cpi"), "Counterfactual")
d <- load_data("fiscal_drag_cross_section")
save_chart("I05_2021_indexation_comparison", d,
  ggplot(d, aes(income_2025 / 1000, additional_tax)) + geom_line(linewidth = 1, colour = colours[1]) +
    geom_hline(yintercept = 0, colour = "grey50") + ggplot2::labs(x = "2025 annual gross income (NZ$ thousands)", y = "Difference in annual income tax (NZ$)"),
  "How far did threshold changes offset inflation?", "2025 full-year scale versus CPI-indexing every 2021 threshold",
  "Both schedules retain the 2021 rates, including 39%, to isolate threshold differences. The graph is static and excludes ACC, credits and transfers.",
  c("tax_rates", "wb_cpi"), "Counterfactual")
d <- load_data("interest_tax_illustration") |> filter(tax_rate == .33) |>
  select(year, tax_nominal, hypothetical_indexed_tax, tax_on_inflation_component) |>
  pivot_longer(-year, names_to = "component", values_to = "value") |>
  mutate(component = recode(component, tax_nominal = "Tax on nominal interest", hypothetical_indexed_tax = "Tax if only positive real interest taxed", tax_on_inflation_component = "Tax on inflation compensation (capped)"))
save_chart("I06_interest_tax_components", d,
  ggplot(d, aes(year, value, colour = component)) + geom_line(linewidth = 1) +
    ggplot2::labs(x = NULL, y = "Annual tax (NZ$)", colour = NULL),
  "Tax on nominal interest and inflation", "Illustrative NZ$10,000 balance earning 5%; 33% tax rate; observed inflation",
  "The deposit rate and balance are assumed. Tax on inflation compensation is capped at total interest tax; the indexed example gives no refund when real interest is negative.",
  c("wb_inflation", "tax_rates"), "Scenario using observed CPI")
d <- load_data("interest_tax_illustration") |> filter(tax_rate == .33) |>
  select(year, real_pre_tax_percent, real_after_tax_percent, indexed_real_after_tax_percent) |>
  pivot_longer(-year, names_to = "return", values_to = "value") |>
  mutate(return = recode(return, real_pre_tax_percent = "Before tax", real_after_tax_percent = "Tax on nominal interest", indexed_real_after_tax_percent = "Tax on positive real interest"))
save_chart("I07_real_saving_returns", d,
  ggplot(d, aes(year, value, colour = return)) + geom_line(linewidth = 1) + geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(labels = percent) + ggplot2::labs(x = NULL, y = "Real annual return", colour = NULL),
  "Saving returns after inflation and tax", "Fixed illustrative 5% nominal return; 33% tax; exact real-return calculation",
  "These are not realised returns on actual NZ deposits. The real-return-only tax counterfactual assumes no refund for losses; nominal tax payments are deflated with CPI.",
  c("wb_inflation", "tax_rates"), "Scenario using observed CPI")
d <- crossing(inflation = seq(0, .10, .001), tax_rate = c(.175, .28, .33, .39)) |>
  mutate(real_return = 100 * ((1 + .05 * (1 - tax_rate)) / (1 + inflation) - 1), rate = paste0(100 * tax_rate, "% tax"))
save_chart("I08_interest_inflation_sensitivity", d,
  ggplot(d, aes(inflation * 100, real_return, colour = rate)) + geom_line(linewidth = 1) + geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(x = "Assumed annual inflation (%)", y = "After-tax real return (%)", colour = NULL),
  "When nominal interest fails to protect savings", "A 5% nominal return taxed at alternative marginal rates",
  "All inflation rates and returns in this sensitivity chart are assumptions. The 28% line also illustrates a PIE rate; taxable income, losses and product features are not modelled.",
  "tax_rates", "Scenario")

# PUBLIC SERVANTS: departmental FTEs, public-sector headcounts and contractors
# have distinct populations. Source aggregates are not added to their components.
d <- fte
save_chart("P01_public_service_fte", d,
  ggplot(d, aes(date, fte / 1000)) + geom_line(linewidth = 1, colour = colours[1]) +
    ggplot2::labs(x = NULL, y = "Public Service FTEs (thousands)"),
  "The size of the Public Service", "Published aggregate: annual June snapshots and recent quarterly observations",
  "Public Service departments are narrower than the public sector. FTEs exclude contractors; agency creation and transfers can change the series independently of service volume.", "psc_fte")
d <- fte |> filter(format(date, "%m-%d") == "06-30") |> mutate(year = as.integer(format(date, "%Y"))) |>
  left_join(wb |> filter(iso3 == "NZL", measure == "population") |> select(year, population = value), by = "year") |>
  filter(!is.na(population)) |> mutate(per_1000 = fte / population * 1000)
save_chart("P02_fte_per_resident", d,
  ggplot(d, aes(year, per_1000)) + geom_line(linewidth = 1, colour = colours[1]) + geom_point(size = 1.5) +
    ggplot2::labs(x = NULL, y = "Departmental FTEs per 1,000 residents"),
  "Public Service staffing relative to population", "June departmental FTEs divided by World Bank annual population",
  "A workload proxy, not a measure of efficiency. Annual population and June staffing dates differ slightly; health, school and local-government workers are outside departmental coverage.",
  c("psc_fte", "wb_population"), "Derived measure")
last_sector_year <- max(sectors$year)
d <- sectors |> filter(year == last_sector_year)
save_chart("P03_sector_composition", d,
  ggplot(d, aes(reorder(sector, headcount), headcount / 1000, fill = broad_sector)) + geom_col() + coord_flip() +
    ggplot2::labs(x = NULL, y = "Headcount (thousands)", fill = NULL),
  "Which workers are in the public sector?", paste("Sector headcounts in", last_sector_year, "- latest year in the linked downloadable CSV"),
  "The CSV linked from the 2025 page currently ends in 2023. This is headcount, not FTE; SOEs, health, education and local government have different roles.", "psc_sectors")
d <- sectors |> filter(sector != "Private Sector")
save_chart("P04_sector_employment_shares", d,
  ggplot(d, aes(year, share_total_percent, fill = sector)) + geom_area() +
    scale_y_continuous(labels = percent) + ggplot2::labs(x = NULL, y = "Share of workforce headcount", fill = NULL),
  "Public employment as a share of the workforce", paste("Downloadable sector series, 2000-", last_sector_year, sep = ""),
  "The denominator is the workforce represented in Stats NZ Business Demography, not HLFS total employment. The download stops in 2023 despite being linked from a 2025 page.", "psc_sectors")
d <- sectors |> group_by(year) |>
  summarise(public = sum(headcount[sector != "Private Sector"]), private = sum(headcount[sector == "Private Sector"]),
            population = first(population), .groups = "drop") |>
  mutate(public_per_1000 = public / population * 1000, private_per_1000 = private / population * 1000) |>
  pivot_longer(c(public_per_1000, private_per_1000), names_to = "sector", values_to = "per_1000") |>
  mutate(sector = recode(sector, public_per_1000 = "Public sector", private_per_1000 = "Private sector"))
save_chart("P05_employment_per_resident", d,
  ggplot(d, aes(year, per_1000, colour = sector)) + geom_line(linewidth = 1) +
    ggplot2::labs(x = NULL, y = "Employees per 1,000 residents", colour = NULL),
  "Employment relative to the population", "Wider public sector and private sector in the downloadable Business Demography series",
  "Demography, labour participation and coverage affect these ratios. They are not measures of output per worker or international rankings.",
  c("psc_sectors", "wb_population"), "Derived measure")
d <- regions |> filter(year == last_sector_year, !region %in% c("Unknown", "Overseas", "Area Outside Regions")) |>
  group_by(region) |> summarise(public_share = 100 * sum(headcount[sector != "Private Sector"]) / sum(headcount), .groups = "drop")
save_chart("P06_regional_public_employment", d,
  ggplot(d, aes(reorder(region, public_share), public_share)) + geom_col(fill = colours[1]) + coord_flip() +
    scale_y_continuous(labels = percent) + ggplot2::labs(x = NULL, y = "Public-sector share of regional workforce"),
  "Public employment differs across regions", paste("Workplace-region headcounts,", last_sector_year),
  "Health and education are included. A regional employment concentration does not establish excess staffing; workplace location differs from residence.", "psc_sectors")
d <- cc |> filter(period_basis == "Full fiscal year", type == "Public Service Departments") |>
  group_by(date) |> summarise(opex = sum(opex_millions), capex = sum(capex_millions), .groups = "drop") |>
  pivot_longer(c(opex, capex), names_to = "expense", values_to = "value")
save_chart("P07_departmental_contractors", d,
  ggplot(d, aes(as.integer(format(date, "%Y")), value, fill = expense)) + geom_col(position = "dodge") +
    ggplot2::labs(x = "Fiscal year ending June", y = "NZ$ millions", fill = NULL),
  "Departmental contractors and consultants", "Operating and capital expenditure; full fiscal years only",
  "Contractors are outside departmental FTE. Coverage and classifications change over time; capital expenditure may deliver multi-year assets. Partial-year 2026 data are excluded.", "psc_contractors")
d <- cc_share |> filter(period_basis == "Full fiscal year")
save_chart("P08_contractor_workforce_share", d,
  ggplot(d, aes(date, share_percent)) + geom_line(linewidth = 1, colour = colours[1]) + geom_point(size = 2) +
    scale_y_continuous(labels = percent) + ggplot2::labs(x = NULL, y = "Contractor OPEX / total workforce spending"),
  "Contractors in workforce spending", "PSC published OPEX share; full-year observations",
  "The share is a spending measure, not a headcount share. Follow PSC's coverage notes; the 2026 fiscal-year-to-date observation is excluded.", "psc_contractors_share")
d <- agencies |> filter(date == max(date)) |> arrange(desc(fte)) |> slice_head(n = 12)
save_chart("P09_agency_staffing", d,
  ggplot(d, aes(reorder(wrap(agency, 35), fte), fte / 1000)) + geom_col(fill = colours[1]) + coord_flip() +
    ggplot2::labs(x = NULL, y = "FTEs (thousands)"),
  "Larger Public Service departments", paste("Twelve largest departmental FTE counts at", max(d$date)),
  "Departmental staff include service delivery, such as corrections and benefit administration. Agency size alone is not a value-for-money measure.", "psc_fte")
d <- oia
save_chart("P10_service_workload_timeliness", d,
  ggplot(d, aes(requests, timeliness_percent)) + geom_point(alpha = .7, colour = colours[1]) + scale_x_log10(labels = comma) +
    scale_y_continuous(labels = percent, limits = c(0, 100)) + ggplot2::labs(x = "OIA requests completed (log scale)", y = "Completed within statutory timeframe"),
  "A service workload and performance measure", "OIA reporting agencies, January-June 2026",
  "A single service dimension, not agency productivity. Timeliness includes permitted extensions; case mix, quality, other outputs and OIA staffing are unobserved. Each point is an agency.", "psc_oia", "Service-output proxy")
d <- oia |> filter(requests >= 100, !is.na(average_days))
save_chart("P11_service_response_times", d,
  ggplot(d, aes(average_days, timeliness_percent, size = requests)) + geom_point(alpha = .5, colour = colours[1]) +
    scale_y_continuous(labels = percent) + scale_size_area(max_size = 11, labels = comma) +
    ggplot2::labs(x = "Average working days to respond", y = "Within statutory timeframe", size = "Requests"),
  "Service speed and legal timeliness", "OIA reporting agencies completing at least 100 requests, January-June 2026",
  "Request volume is not FTE. Different deadlines and permitted extensions mean average response time and timeliness can move differently. Costs and response quality are missing.", "psc_oia", "Service-output proxy")
d <- load_data("workforce_headlines_2025") |>
  transmute(`Central government` = central_headcount, `Local government` = local_headcount,
            `Private sector` = total_workforce - public_headcount) |>
  pivot_longer(everything(), names_to = "sector", values_to = "headcount") |>
  mutate(share_percent = 100 * headcount / sum(headcount))
save_chart("P12_workforce_headline_2025", d,
  ggplot(d, aes(sector, share_percent, fill = sector)) + geom_col(show.legend = FALSE) +
    geom_text(aes(label = paste0(round(share_percent, 1), "%")), vjust = -.3) +
    labs(x = NULL, y = "Share of Business Demography workforce (%)"),
  "The wider public sector in 2025", "PSC's published 2025 headline, extracted separately from its older downloadable CSV",
  "Includes health, education, Crown entities, SOEs and local government. This headcount denominator is not HLFS employment. It does not update the detailed 2000-2023 sector series.",
  "psc_workforce_page")

# BENEFITS: expenses are fiscal-year flows; quarterly counts are stocks.
benefit_labels <- function(x) recode(x, `New Zealand Superannuation` = "NZ Superannuation", `Jobseeker Support and Emergency Benefit` = "Jobseeker + Emergency Benefit", `Other Working for Families tax credits` = "Other Working for Families", .default = x)
d <- benefits |> filter(year == 2025) |> mutate(label = wrap(benefit_labels(measure), 34))
save_chart("B01_benefit_expense_composition", d,
  ggplot(d, aes(reorder(label, value), value / 1000)) + geom_col(fill = colours[1]) + coord_flip() +
    ggplot2::labs(x = NULL, y = "NZ$ billions"),
  "What benefit spending actually includes", "2024/25 actual gross core Crown benefit expenses",
  "Includes NZ Super, family credits and supplementary support. Benefits can go to people in work. Jobseeker includes health-related recipients and the Treasury row also includes Emergency Benefit.", "treasury_expenses")
d <- benefits |> filter(year == 2025) |>
  mutate(group = case_when(measure == "New Zealand Superannuation" ~ "NZ Superannuation",
                          measure == "Jobseeker Support and Emergency Benefit" ~ "Jobseeker + Emergency Benefit", TRUE ~ "All other benefits")) |>
  group_by(group) |> summarise(value_millions = sum(value), .groups = "drop") |>
  mutate(share_percent = 100 * value_millions / sum(value_millions))
save_chart("B02_benefit_spending_shares", d,
  ggplot(d, aes(group, share_percent, fill = group)) + geom_col(show.legend = FALSE) +
    geom_text(aes(label = paste0(round(share_percent, 1), "%")), vjust = -.3) +
    scale_x_discrete(labels = wrap) + ggplot2::labs(x = NULL, y = "Share of gross benefit expenses (%)"),
  "Jobseeker within the benefit budget", "2024/25 actual expenses, with a broad residual shown explicitly",
  "Neither Jobseeker nor the residual is equivalent to spending on unemployed people. These are gross taxable transfers; benefit counts do not identify employment status.", "treasury_expenses", "Derived measure")
d <- benefits |> filter(year == 2025, measure %in% c("New Zealand Superannuation", "Jobseeker Support and Emergency Benefit", "Supported Living Payment", "Sole Parent Support")) |>
  mutate(label = benefit_labels(measure)) |> select(label, percent_gdp, percent_core_expenses) |>
  pivot_longer(-label, names_to = "denominator", values_to = "share") |>
  mutate(denominator = recode(denominator, percent_gdp = "% of GDP", percent_core_expenses = "% of core Crown expenses"))
save_chart("B03_benefits_in_fiscal_scale", d,
  ggplot(d, aes(label, share, fill = label)) + geom_col(show.legend = FALSE) + facet_wrap(~denominator, scales = "free_y") +
    scale_x_discrete(labels = function(x) wrap(x, 16)) + ggplot2::labs(x = NULL, y = "Percent"),
  "Main benefit programmes in fiscal scale", "2024/25 actual gross expenses using matching fiscal-year denominators",
  "Different denominators answer different questions. None of the programmes is identical to the HLFS unemployed population; spending is before income tax collected back.",
  c("treasury_expenses", "treasury_macro"), "Derived measure")
d <- benefits |> filter(status == "Actual") |>
  mutate(group = case_when(measure == "New Zealand Superannuation" ~ "NZ Superannuation",
                           measure == "Jobseeker Support and Emergency Benefit" ~ "Jobseeker + Emergency Benefit",
                           measure %in% c("Wage Subsidy Scheme", "Covid leave support", "Cost of living payment") ~ "Temporary COVID / cost-of-living programmes",
                           TRUE ~ "Other benefits")) |>
  group_by(year, group) |> summarise(value = sum(value), .groups = "drop")
save_chart("B04_benefit_expenses_history", d,
  ggplot(d, aes(year, value / 1000, fill = group)) + geom_area() +
    ggplot2::labs(x = "Fiscal year ending June", y = "Gross expense (NZ$ billions)", fill = NULL),
  "Benefit spending through the pandemic years", "2021-2025 actuals; temporary programmes separated",
  "Nominal expenses; classifications and COVID programmes affect comparisons. NZ Super belongs in the benefit budget but is not a working-age unemployment benefit.", "treasury_expenses")
d <- benefits |> filter(status == "Actual", measure %in% c("New Zealand Superannuation", "Jobseeker Support and Emergency Benefit", "Supported Living Payment", "Sole Parent Support")) |>
  mutate(label = benefit_labels(measure)) |> select(year, label, value, real_2025_millions) |>
  pivot_longer(c(value, real_2025_millions), names_to = "basis", values_to = "expense") |>
  mutate(basis = recode(basis, value = "Nominal", real_2025_millions = "2025 purchasing power"))
save_chart("B05_real_nominal_expenses", d,
  ggplot(d, aes(year, expense / 1000, colour = label)) + geom_line(linewidth = .9) + facet_wrap(~basis) +
    ggplot2::labs(x = "Fiscal year ending June", y = "NZ$ billions", colour = NULL),
  "Benefit expense growth after inflation", "Major programmes in nominal and CPI-deflated dollars, 2021-2025",
  "Calendar annual-average CPI approximately deflates June fiscal-year expenses. Real growth can reflect recipient numbers, rates and composition; it is not proof of generosity or dependency.",
  c("treasury_expenses", "wb_cpi"), "Derived measure")
d <- js
save_chart("B06_jobseeker_subgroups", d,
  ggplot(d, aes(date, recipients / 1000, fill = subgroup)) + geom_area() +
    ggplot2::labs(x = NULL, y = "Working-age recipients (thousands)", fill = NULL),
  "Jobseeker is not just work-ready recipients", "MSD quarter-end stocks, June 2021-June 2026",
  "Includes health condition/disability recipients. Some recipients work; these counts differ from HLFS unemployment. MSD rounds counts and subgroup sums can differ slightly from headline totals.", "msd_benefits")
d <- counts |> filter(programme != "Total")
save_chart("B07_main_benefit_counts", d,
  ggplot(d, aes(date, recipients / 1000, colour = programme)) + geom_line(linewidth = 1) +
    ggplot2::labs(x = NULL, y = "Working-age recipients (thousands)", colour = NULL),
  "Working-age main benefit receipt", "Quarter-end counts by main programme, June 2021-June 2026",
  "Recipient stocks are not expenditure flows. Supplementary benefits overlap with main benefits and should not be added to these counts as extra people.", "msd_benefits")
d <- duration |> group_by(date) |> mutate(share_percent = 100 * recipients / sum(recipients)) |> ungroup()
save_chart("B08_jobseeker_duration", d,
  ggplot(d, aes(date, share_percent, fill = programme)) + geom_area() +
    scale_y_continuous(labels = percent) + ggplot2::labs(x = NULL, y = "Share of Jobseeker recipients", fill = NULL),
  "How long recipients have received support", "Continuous benefit duration for working-age Jobseeker recipients",
  "MSD continuous-duration definitions can include benefit-type changes. Long duration need not mean continuous unemployment; health limitations and part-time work matter.", "msd_benefits")
d <- load_data("benefit_cut_scenarios") |> group_by(cut_percent, target) |>
  summarise(low = min(gross_savings_millions), high = max(gross_savings_millions), central = median(gross_savings_millions), .groups = "drop")
save_chart("B09_gross_cut_savings", d,
  ggplot(d, aes(cut_percent, central, colour = target, fill = target)) + geom_ribbon(aes(ymin = low, ymax = high), alpha = .15, colour = NA) +
    geom_line(linewidth = 1) + ggplot2::labs(x = "Assumed expense reduction (%)", y = "Gross annual saving (NZ$ millions)", colour = NULL, fill = NULL),
  "The arithmetic of hypothetical benefit cuts", "2025 actual Jobseeker + Emergency expense; work-ready base estimated from claimant shares",
  "The work-ready band assumes average payment 0.75-1.25 times the health-related payment. Quarter-end counts approximate annual composition; Emergency Benefit is not separately allocated. These are not net savings.",
  c("treasury_expenses", "msd_benefits"), "Scenario")
d <- load_data("benefit_cut_scenarios") |> group_by(cut_percent, target) |>
  summarise(low = min(percent_core_expenses), high = max(percent_core_expenses), central = median(percent_core_expenses), .groups = "drop")
save_chart("B10_cut_savings_fiscal_scale", d,
  ggplot(d, aes(cut_percent, central, colour = target, fill = target)) + geom_ribbon(aes(ymin = low, ymax = high), alpha = .15, colour = NA) +
    geom_line(linewidth = 1) + scale_y_continuous(labels = percent) +
    ggplot2::labs(x = "Assumed expense reduction (%)", y = "Gross saving / core Crown expenses", colour = NULL, fill = NULL),
  "Benefit cuts against the total spending budget", "Same static gross-saving scenarios, scaled by 2025 core Crown expenses",
  "Lost income tax, replacement assistance, administration and behaviour can reduce net savings. The graph neither costs a specific proposal nor assumes any cut is desirable.",
  c("treasury_expenses", "msd_benefits", "treasury_macro"), "Scenario")

if (file.exists(file.path(data_dir, "benefit_history_groups.csv"))) {
  source(file.path(root, "07_benefit_history_plots.R"), local = TRUE)
}

# A local, searchable gallery with chart data and source links. No external
# JavaScript or web server is needed, and source caveats remain visible.
catalogue <- bind_rows(catalogue)
stopifnot(!anyDuplicated(catalogue$id), nrow(catalogue) >= 40,
          all(file.exists(file.path(figure_dir, catalogue$png))),
          all(file.exists(file.path(figure_dir, catalogue$svg))))
write_csv(catalogue, file.path(figure_dir, "chart_catalogue.csv"))
escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}
cards <- map_chr(seq_len(nrow(catalogue)), function(i) {
  c <- catalogue[i, ]
  urls <- strsplit(c$sources, " | ", fixed = TRUE)[[1]]
  source_links <- paste0('<a href="', escape(urls), '" target="_blank" rel="noopener">Source ', seq_along(urls), '</a>', collapse = " · ")
  paste0('<article data-topic="', escape(c$topic), '" data-kind="', escape(c$measurement_type), '">',
         '<div class="meta">', escape(c$id), ' · ', escape(c$measurement_type), '</div>',
         '<h2>', escape(c$title), '</h2><p>', escape(c$subtitle), '</p>',
         '<a href="', c$png, '"><img loading="lazy" src="', c$png, '" alt="', escape(c$title), '"></a>',
         '<p class="note">', escape(c$interpretation), '</p>',
         '<div class="links"><a href="', c$png, '">PNG</a> · <a href="', c$svg, '">SVG</a> · <a href="', c$data_csv, '">Chart data</a> · ', source_links, '</div></article>')
})
options <- paste0('<option>', escape(unique(catalogue$topic)), '</option>', collapse = "")
kind_options <- paste0('<option>', escape(unique(catalogue$measurement_type)), '</option>', collapse = "")
html <- paste0('<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
 '<title>NZ op-ed evidence charts</title><style>body{margin:0;background:#f5f7fa;color:#172a3b;font:16px/1.5 system-ui}header,main{max-width:1400px;margin:auto;padding:24px}h1{margin:0}h2{font-size:20px;margin:8px 0}header p{max-width:950px}.filters{display:flex;gap:12px;flex-wrap:wrap;position:sticky;top:0;background:#f5f7fa;padding:12px 0;z-index:2}select,input{font:inherit;padding:8px;border:1px solid #bccbd5;border-radius:5px}.grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(440px,1fr));gap:22px}article{background:white;border:1px solid #d9e1e8;border-radius:9px;padding:18px}img{width:100%;height:auto}.meta{font-size:12px;color:#526579}.note{font-size:14px}.links{font-size:13px}a{color:#075b9b}[hidden]{display:none!important}@media(max-width:600px){.grid{grid-template-columns:1fr}header,main{padding:14px}}@media print{.filters{display:none}article{break-inside:avoid}.grid{display:block}}</style>',
 '<header><h1>New Zealand: five op-ed evidence packs</h1><p>', nrow(catalogue), ' charts from public data, with downloadable plotting data and source links. Snapshot cutoff: 14 October 2026. Observed statistics, context proxies and scenarios are labelled separately.</p>',
 '<p>These charts test measurements; they do not make a policy argument. Read each caveat before comparing series. The <a href="../../processed/op_ed_topics/measurement_notes.csv">measurement notes</a> identify missing data for stronger claims. <a href="chart_catalogue.csv">Download the catalogue</a>.</p>',
 '<p><strong>Coverage note:</strong> the workforce-sector CSV linked from the PSC 2025 page ends in 2023. Departmental FTEs extend to March 2026. Tax on savings uses an assumed 5% return because RBNZ automated downloads were inaccessible; it is not a measured deposit-return series.</p>',
 '<p><strong>Benefit history:</strong> additional B11-B29 charts cover quarterly counts from 2008, actual Treasury expenses from 2001, and selected archival observations from 1940. The <a href="../../BENEFIT_HISTORY.md">history notes</a> explain backcasts, overlaps and accounting/coverage breaks.</p>',
 '<div class="filters"><select id="topic"><option value="">All topics</option>', options,
 '</select><select id="kind"><option value="">All measure types</option>', kind_options,
 '</select><input id="search" placeholder="Search titles and notes" aria-label="Search charts"><span id="count"></span></div></header>',
 '<main class="grid">', paste(cards, collapse = "\n"), '</main><script>const cards=[...document.querySelectorAll("article")];function filter(){let n=0;for(const c of cards){c.hidden=!!((topic.value&&c.dataset.topic!==topic.value)||(kind.value&&c.dataset.kind!==kind.value)||(search.value&&!c.textContent.toLowerCase().includes(search.value.toLowerCase())));if(!c.hidden)n++}document.getElementById("count").textContent=n+" charts"}const topic=document.getElementById("topic"),kind=document.getElementById("kind"),search=document.getElementById("search");[topic,kind,search].forEach(x=>x.addEventListener("input",filter));filter();</script></html>')
writeLines(html, file.path(figure_dir, "index.html"), useBytes = TRUE)
message("Saved ", nrow(catalogue), " charts and gallery: ", file.path(figure_dir, "index.html"))

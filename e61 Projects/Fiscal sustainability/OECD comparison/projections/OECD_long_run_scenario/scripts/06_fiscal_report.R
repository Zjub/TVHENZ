# Generate standalone HTML, a Word note, CSV source documentation and figures.
labels <- c(health_ratio = "Health and long-term care", pension_ratio = "Public pensions",
            other_ratio = "Other primary expenses", capital_ratio = "Net capital investment",
            primary_ratio = "Total primary spending")
case_labels <- c(documented_equations = "Documented equations; available inputs",
  fixed_older_employment = "Older employment rates held fixed",
  price_indexed_pensions = "Pensions indexed to prices",
  no_health_baumol = "No health Baumol effect",
  other_spending_fixed_gdp = "Other spending fixed/GDP",
  higher_health_income_elasticity = "Health income elasticity = 1",
  september_2026_other_rule_only = "2026 residual-spending rule only")
theme_set(theme_minimal(base_size = 12) + theme(legend.position = "bottom",
  panel.grid.minor = element_blank(), plot.title.position = "plot"))
save_figure <- function(p, name, width = 10, height = 6) {
  ggsave(file.path(fig, paste0(name, ".png")), p, width = width, height = height, dpi = 160)
  ggsave(file.path(fig, paste0(name, ".pdf")), p, width = width, height = height)
}
plot_d <- copy(spending_long[scenario == "BAU1" & case == "documented_equations"])
plot_d[, component_label := labels[as.character(component)]]
save_figure(ggplot(plot_d, aes(year, ratio * 100)) + geom_line(colour = "#245d83", linewidth = 0.9) +
  facet_wrap(~component_label, scales = "free_y", ncol = 2) +
  labs(title = "Australian spending under the documented OECD equations",
       subtitle = "Independent implementation with available inputs; partial numerical replication",
       x = NULL, y = "% of GDP"), "01_spending_components", height = 8)
bp <- benchmark_comparison[case == "documented_equations"]
bp <- melt(bp[, .(component, model_change_pp, published_change_pp)], id.vars = "component")
bp[, `:=`(component = labels[component],
           series = fifelse(variable == "model_change_pp", "Independent calculation", "OECD Australia survey chart"))]
save_figure(ggplot(bp, aes(component, value, fill = series)) +
  geom_col(position = "dodge", width = 0.7) + geom_hline(yintercept = 0, colour = "grey40") +
  scale_fill_manual(values = c("Independent calculation" = "#245d83", "OECD Australia survey chart" = "#d79338")) +
  labs(title = "The numerical match is incomplete", subtitle = "Change from 2024 to 2060; benchmark parameters were not fitted",
       x = NULL, y = "Percentage points of GDP", fill = NULL), "02_published_benchmark")
sp <- copy(spending_long[scenario == "BAU1" & component == "primary_ratio"])
sp[, case_label := case_labels[case]]
save_figure(ggplot(sp, aes(year, change_pp, colour = case_label)) + geom_line(linewidth = 0.9) +
  labs(title = "Spending assumptions have material long-run effects", x = NULL,
       y = "Change in primary spending since 2024, pp of GDP", colour = NULL) +
  guides(colour = guide_legend(ncol = 2)), "03_spending_sensitivities", height = 7)
dp <- copy(debt[case == "documented_equations" & year <= 2060])
dp[, closure_label := fifelse(closure == "oecd_rule", "Revenue adjusts under documented fiscal rule", "Primary revenue held fixed/GDP")]
save_figure(ggplot(dp, aes(year, gross_liabilities_ratio * 100, colour = closure_label)) +
  geom_line(linewidth = 1) + labs(title = "Debt outcomes depend on the fiscal closure",
    subtitle = "Partial-equilibrium illustrations; these are not the published OECD debt projections",
    x = NULL, y = "Gross financial liabilities, % of GDP", colour = NULL), "04_debt_closure")
gp <- melt(growth_effects[year == 2060], id.vars = "case",
  measure.vars = paste0(growth_components, "_effect_pp"))
gp[, `:=`(component = labels[sub("_effect_pp$", "", variable)],
           channel = fifelse(case == "employment_10pct", "10% more output from employment", "10% more output from productivity"))]
save_figure(ggplot(gp, aes(component, value, fill = channel)) +
  geom_col(position = "dodge") + geom_hline(yintercept = 0, colour = "grey40") +
  labs(title = "The source of growth matters", subtitle = "Effect on spending shares in 2060 relative to the same baseline",
       x = NULL, y = "Percentage points of GDP", fill = NULL), "05_growth_source")
save_figure(ggplot(endpoint_comparison, aes(reorder(method, change_pp), change_pp, fill = source_type)) +
  geom_col(width = 0.7) + coord_flip() +
  scale_fill_manual(values = c("Published OECD benchmark" = "#d79338",
    "Independent equation implementation" = "#245d83", "Existing Australian model" = "#8b9aa5")) +
  labs(title = "Published OECD spending pressures alongside our projections",
       subtitle = "2024-2060 changes; Australian models use a PBO FY2023-24 historical anchor",
       caption = "OECD: sum of three published spending components. Definitions and macro assumptions differ.",
       x = NULL, y = "Change in spending, percentage points of GDP", fill = NULL) +
  guides(fill = guide_legend(ncol = 1)), "06_existing_models", height = 7)
save_figure(ggplot(comparison, aes(year, change_since_2030_pp, colour = method)) + geom_line(linewidth = 0.9) +
  labs(title = "Existing Australian projections and the OECD-equation illustration",
       subtitle = "Rebased in 2030; macro assumptions and expenditure definitions differ",
       x = NULL, y = "Change in spending since 2030, pp of GDP", colour = NULL) +
  guides(colour = guide_legend(ncol = 2)), "07_post_2030_model_paths", height = 7)
save_figure(ggplot(imf_comparison, aes(year, total_expenditure_pct, colour = method)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = imf_comparison[method == "IMF Fiscal Monitor (live snapshot)"], size = 2) +
  labs(title = "IMF medium-term expenditure projections and our saved paths",
       subtitle = "Total expenditure includes interest and net capital investment; IMF snapshot 23 October 2026",
       x = NULL, y = "% of GDP", colour = NULL,
       caption = "IMF calendar years and Australian financial years are paired by ending year. Definitions and vintages differ.") +
  scale_x_continuous(breaks = 2025:2031) + guides(colour = guide_legend(ncol = 2)),
  "08_imf_medium_term", width = 12, height = 8)
save_figure(ggplot(component_comparison, aes(component, change_pp, fill = method)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, colour = "grey40") +
  labs(title = "IMF health and pension pressures versus available model components",
       subtitle = "2025-2030 changes; published IMF April 2026 structural indicators",
       x = NULL, y = "Percentage points of GDP", fill = NULL,
       caption = "Health coverage differs. Existing Australian bottom-up models do not identify pensions separately.") +
  guides(fill = guide_legend(ncol = 2)), "09_imf_components", width = 11, height = 6)

sources <- data.table(
  source = c("OECD EO117 long-run scenarios", "UN World Population Prospects 2024",
    "OECD Health Expenditure and Financing", "OECD Social Expenditure (SOCX)",
    "OECD Labour Force Statistics", "PBO consolidated fiscal history",
    "OECD Australia 2026 Figure 1.14", "OECD fiscal framework 2017",
    "OECD fiscal revision 2019", "OECD fiscal revision September 2026"),
  used_for = c("Real GDP, population, employment, deflator, rates, liabilities and balances",
    "Annual population age shares; estimates through 2023, medium projection from 2024",
    "Observed 2024 public health share, including health-related long-term care",
    "2022 public old-age, early retirement, survivor and disability pension shares",
    "Observed employment rates aged 55-64 and 65-74 through 2024",
    "FY2023-24 expenses and net capital investment; residual expense anchor",
    "Independent validation targets; never inputs to model parameter estimation",
    "Residual primary spending and fiscal reaction function",
    "Health and pension projection equations and calibration",
    "Residual-spending rule sensitivity and assessment of later methodological changes"),
  file = c("eo117_all_australia_bau1.json plus inherited oecd_lts_australia_long.csv",
    "wpp2024_age_groups.xlsx", "sha_australia.json", "socx_australia.json",
    "lfs_Y55T64.json; lfs_Y65T74.json", "pbo_historical_and_forecast_snapshot.csv",
    "australia_2026_figure_1_14.xlsx", "oecd_fiscal_method_2017.pdf",
    "oecd_fiscal_method_2019.pdf", "oecd_fiscal_method_2026.pdf"),
  url = c("https://data-explorer.oecd.org/vis?df[ag]=OECD.ECO.MAD&df[id]=DSD_EO_LTB@DF_EO_LTB",
    "https://population.un.org/wpp/Download/Standard/Population/",
    "https://db.nomics.world/OECD/DSD_SHA%40DF_SHA",
    "https://db.nomics.world/OECD/DSD_SOCX_AGG%40DF_SOCX_AGG",
    "https://db.nomics.world/OECD/DSD_LFS%40DF_LFS_INDIC",
    "https://www.pbo.gov.au/publications-and-data/data-and-tools/data-portal/national-fiscal-outlook",
    "https://stat.link/gdxr47", "https://doi.org/10.1787/8eddfa18-en",
    "https://doi.org/10.1787/4f07fb8d-en", "https://doi.org/10.1787/421bf86f-en"),
  qualification = c("OECD series accessed via DBnomics; baseline vintage fixed at EO117",
    "Age shares scaled to the OECD population total; unscaled UN totals retained",
    "HF1, total functions and providers; Australia has no compulsory contributory health-insurance schemes",
    "Early-retirement TP112 has no Australian observations: explicit zero assumption; 2022 share extrapolated",
    "Future rates are approximated with aggregate employment-odds changes; no OECD cohort-model replication",
    "Copied from existing project extract; financial-year/calendar pairing and expense-definition bridge",
    "January 2026 chart; exact fiscal model input vintage is not supplied in the workbook",
    "Implemented conditional on exogenous macro paths, with constant financial assets/GDP",
    "Documented older framework; does not claim to reproduce every EO117 implementation detail",
    "EO119 paper; only its Table 3 residual-spending rule transferred, not its full fiscal model"))
sources <- rbind(sources, imf_sources[, .(source,
  used_for = coverage,
  file = c("../imf/*_AUS.json", "../imf/fiscal_monitor_april_2026_australia_structural.csv", "Official IMF publication; contextual discussion"),
  url, qualification = status)])
fwrite(sources, file.path(processed, "data_sources_and_limitations.csv"))
dictionary <- data.table(
  variable = c("year", "pop", "employment", "real_gdp", "nominal_gdp", "employment_rate",
    "share65p", "share55_64", "share65_74", "share75p", "share5_24", "er55_64", "er65_74",
    "interest_rate", "health_ratio", "pension_ratio", "other_ratio", "capital_ratio",
    "gross_liabilities_ratio", "assets_ratio", "primary_revenue_ratio"),
  definition = c("Calendar year; PBO anchor paired to financial year ending in this year",
    "OECD trend total population (POPS)", "OECD potential employment (ETPT)",
    "OECD potential output (GDPVTRD), 2021 PPP USD level",
    "Base-2024 nominal GDP multiplied by potential real output and BAU1 GDP-deflator growth",
    "OECD trend employment rate 15-74 (ERS1574)/100",
    "UN population share aged 65+", "UN population share aged 55-64",
    "UN population share aged 65-74", "UN population share aged 75+", "UN population share aged 5-24",
    "Age 55-64 employment rate: observed 2024, then specified projection rule",
    "Age 65-74 employment rate: observed 2024, then specified projection rule",
    "OECD implicit interest rate on gross government liabilities/100",
    "Public health and health-related long-term care spending/GDP",
    "Four public cash pension categories from the 2019 definition/GDP",
    "PBO expenses less OECD-derived interest, health and pensions; mixed-concept residual",
    "PBO net capital investment/GDP held at the initial ratio",
    "OECD general-government gross financial liabilities/GDP, not PBO gross debt",
    "OECD gross minus net financial liabilities/GDP; includes non-debt financial assets",
    "Revenue excluding interest receipts/GDP, initially inferred from OECD primary balance"),
  unit = c("year", "persons", "persons", "2021 PPP USD", "nominal index/level; growth rates used",
           rep("decimal fraction", 16)))
fwrite(dictionary, file.path(processed, "data_dictionary.csv"))

esc <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
html_table <- function(d) {
  d <- as.data.frame(d)
  d[] <- lapply(d, function(x) if (is.numeric(x)) format(round(x, 3), trim = TRUE) else x)
  paste0("<table><thead><tr>", paste0("<th>", esc(names(d)), "</th>", collapse = ""),
    "</tr></thead><tbody>", paste(apply(d, 1, function(row) paste0("<tr>",
      paste0("<td>", esc(row), "</td>", collapse = ""), "</tr>")), collapse = ""), "</tbody></table>")
}
html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Australian fiscal projections: OECD equations</title>',
  '<style>body{font:16px/1.6 system-ui,sans-serif;max-width:1100px;margin:40px auto;padding:0 25px;color:#182b3a}h1,h2{line-height:1.25}h2{margin-top:2.2em}table{border-collapse:collapse;width:100%;font-size:14px}th,td{padding:9px;border-bottom:1px solid #cbd5de;text-align:left;vertical-align:top}th{background:#eef3f7}img{max-width:100%;height:auto}pre{background:#eef3f7;padding:16px;white-space:pre-wrap}a{color:#245d83}.note{border-left:4px solid #d79338;padding:10px 18px;background:#fff8eb}</style></head><body>')
word <- if (requireNamespace("officer", quietly = TRUE)) officer::read_docx() else NULL
heading <- function(x, level = 2) {
  html <<- c(html, paste0("<h", level, ">", esc(x), "</h", level, ">"))
  if (!is.null(word)) word <<- officer::body_add_par(word, x, style = paste("heading", level))
}
paragraph <- function(x, note = FALSE) {
  html <<- c(html, paste0(if (note) '<p class="note">' else '<p>', esc(x), "</p>"))
  if (!is.null(word)) word <<- officer::body_add_par(word, x)
}
table_out <- function(d) {
  html <<- c(html, html_table(d))
  if (!is.null(word)) {
    t <- as.data.frame(d)
    t[] <- lapply(t, function(x) if (is.numeric(x)) round(x, 3) else as.character(x))
    word <<- officer::body_add_table(word, t)
  }
}
figure_out <- function(name, caption) {
  path <- file.path(fig, paste0(name, ".png"))
  html <<- c(html, paste0('<figure><img alt="', esc(caption), '" src="data:image/png;base64,',
    jsonlite::base64_enc(readBin(path, "raw", file.info(path)$size)), '"><figcaption>', esc(caption), '</figcaption></figure>'))
  if (!is.null(word)) {
    word <<- officer::body_add_img(word, path, width = 6, height = if (name == "01_spending_components") 4.8 else 3.6)
    word <<- officer::body_add_par(word, caption)
  }
}
heading("Australian fiscal projections using the OECD approach", 1)
paragraph("Updated for the projection project, 23 October 2026. This note accompanies a reproducible R workflow, cached source data, input tables, numerical validation and figures, including an IMF comparison.")
paragraph("Status: this is an implementation of selected published OECD equations using available and substituted inputs, not a validated numerical replication of the OECD Australian projection. Health is closely approximated, but the combined spending increase is materially understated because pensions and residual primary spending do not match. Published benchmark values do not determine the independent model parameters. Debt results are conditional illustrations rather than a reproduction of OECD Australia's published debt paths.", TRUE)
heading("Results and the published benchmark")
summary <- benchmark_comparison[case == "documented_equations", .(
  Component = labels[component], `R calculation, pp` = model_change_pp,
  `Published OECD, pp` = published_change_pp, `Difference, pp` = difference_pp)]
table_out(summary)
paragraph(sprintf("Between 2024 and 2060, calculated primary spending rises by %.2f percentage points of GDP, compared with %.2f points for the three published spending components combined. The chart's total fiscal-pressure increase is %.2f points: it also contains net-interest and other fiscal factors, so that total must not be interpreted as a spending increase.",
  spending_long[scenario == "BAU1" & case == "documented_equations" & component == "primary_ratio" & year == 2060, change_pp],
  sum(benchmark_spending[component %in% c("health_ratio", "pension_ratio", "other_ratio"), published_change_pp]),
  benchmark_spending[component == "total_pressure", published_change_pp]))
figure_out("02_published_benchmark", "Figure 1. Independent calculations and the Australia 2026 survey benchmark, 2024-2060.")
heading("Why the numerical replication falls short")
table_out(gap_bridge[, .(Component = labels[component], `Published less calculated, pp` = shortfall_pp,
                         `Share of total shortfall, %` = share_of_total_shortfall_pct)])
paragraph(sprintf("The shortfall is %.2f percentage points, or %.1f%% of the published three-component spending increase. The table identifies where the discrepancy occurs; it does not identify every underlying cause. Passing the accounting and equation checks confirms that the chosen rules were implemented consistently, not that the original OECD Australian specification and inputs have been recovered.",
  sum(gap_bridge$shortfall_pp), 100 * sum(gap_bridge$shortfall_pp) / sum(gap_bridge$published_change_pp)))
nl <- driver_gap[quantity == "Population per worker"]
paragraph(sprintf("Other primary spending accounts for most of the shortfall. Our older documented rule makes its GDP share proportional to population per worker. In the EO117 inputs this ratio falls from %.3f in 2024 to %.3f in 2060, a %.2f%% decline, mechanically producing a spending decline of %.2f points. To obtain the published +%.2f points with the same initial spending share and this same rule would instead require population per worker to rise to %.3f, a %.2f%% increase. This is an inverse diagnostic, not a recovered OECD projection. The sign discrepancy cannot be repaired by changing a positive starting spending level.",
 nl$initial, nl$calculated_2060, 100 * (nl$calculated_2060 / nl$initial - 1),
 gap_bridge[component == "other_ratio", model_change_pp], bench_o * 100,
 nl$benchmark_implied_2060, 100 * (nl$benchmark_implied_2060 / nl$initial - 1)))
paragraph("What is established is the incompatibility between our available macro inputs, chosen residual-spending rule and the published residual-spending increase. What is not established is which difference in the original exercise explains it. The published other-primary category has not been fully reconciled with our mixed-concept current-expense residual and separate fixed capital block. Different demographic/employment inputs, initial forecast-to-long-run transitions, expenditure coverage or implementation details remain possibilities. Describing the gap simply as a data-vintage effect would overstate what has been verified.")
ep <- driver_gap[quantity == "Employment rate aged 55-74"]
paragraph(sprintf("Pensions are the second main source. We substituted a rule that transfers aggregate employment-odds changes to two older age bands; we did not recreate the OECD age/sex cohort model. Our population-weighted 55-74 employment rate moves from %.1f%% to %.1f%%. Holding all other pension inputs fixed, the published pension increase would imply an end rate of %.1f%%. That does not establish that the OECD assumes this rate; it shows the sensitivity to the missing cohort information. Holding individual older-age employment rates fixed increases our pension rise from %.2f to %.2f points, narrowing the gap by %.2f points but leaving %.2f points unexplained.",
 ep$initial * 100, ep$calculated_2060 * 100, ep$benchmark_implied_2060 * 100,
 gap_bridge[component == "pension_ratio", model_change_pp],
 benchmark_comparison[case == "fixed_older_employment" & component == "pension_ratio", model_change_pp],
 benchmark_comparison[case == "fixed_older_employment" & component == "pension_ratio", model_change_pp] -
   gap_bridge[component == "pension_ratio", model_change_pp],
 benchmark_comparison[case == "fixed_older_employment" & component == "pension_ratio", published_change_pp - model_change_pp]))
paragraph(sprintf("Starting levels also matter. The pension anchor is %.2f%% of GDP, derived from 2022 SOCX data rolled to 2024, rather than the exact OECD fiscal starting level. Keeping our pension multiplier unchanged would require an initial share of %.2f%% to match the published increase. The health mismatch is much smaller: %.2f points, or %.1f%% of the total shortfall. Keeping our health multiplier unchanged would require an initial health share of %.2f%% instead of %.2f%%. These are alternative inverse diagnostics, not parameter estimates, and changing one anchor while keeping total expenses fixed would also change the residual-spending anchor.",
 anchor$pension_ratio * 100, gap_bridge[component == "pension_ratio", implied_initial_share_pct],
 gap_bridge[component == "health_ratio", shortfall_pp],
 gap_bridge[component == "health_ratio", share_of_total_shortfall_pct],
 gap_bridge[component == "health_ratio", implied_initial_share_pct], anchor$health_ratio * 100))
paragraph("The practical conclusion is that our equation implementation should be labelled an OECD-method illustration until the fiscal definitions, initial levels, transition rules and cohort employment inputs have been reconciled. The September 2026 rule sensitivity is not a remedy for the January 2026 survey discrepancy: it belongs to a later model vintage. Exact reproduction should precede using our independent calculation as a substitute for the published OECD result in comparisons.", TRUE)

heading("Equations implemented in R")
paragraph("The older documented framework is WP1440 (2017) plus WP1581 (2019). For health, h is spending/GDP, y is real GDP/person, s is the population share aged 65+, and q is real GDP/worker. The share equation below follows by cancelling population and the GDP deflator from the nominal per-capita equation. Parameters are calibration assumptions, not estimates from Australian aggregate spending.")
equations <- c("Health: h(t)/h(0) = [y(t)/y(0)]^(-0.3) * [s(t)/s(0)]^0.4 * [q(t)/q(0)]^0.5",
 "Retiree proxy: R = N(55-64)*(1-e55_64) + N(65-74)*(1-e65_74) + N(75+)",
 "Pensions: p(t)/p(0) = [(R/L)(t)/(R/L)(0)] * [q(t)/q(0)]^(-(1-phi)); baseline phi=1",
 "Other current primary: o(t)/o(0) = [(N/L)(t)/(N/L)(0)]",
 "Primary spending = health + pensions + other primary expenses + net capital investment")
html <- c(html, paste0("<pre>", esc(paste(equations, collapse = "\n")), "</pre>"))
if (!is.null(word)) for (e in equations) word <- officer::body_add_par(word, e)
paragraph("The 2019 pension scope includes disability as well as old-age, early-retirement and survivor pensions. These are public cash benefits; private superannuation payouts are excluded. Health-related long-term care is already included in the SHA health total, so it is not added a second time. Net capital investment is a separate fixed-share assumption.")
figure_out("01_spending_components", "Figure 2. Components and total primary spending, 2024-2100; vertical scales differ.")

heading("Data and the boundary between observations and assumptions")
table_out(sources[, .(Source = source[1:7], Purpose = used_for[1:7], Qualification = qualification[1:7])])
paragraph("Every input is inspectable in data/processed/fiscal. data_sources_and_limitations.csv gives source links and definitions; download_manifest.csv records exact request URLs; source_file_checksums.csv identifies cached files. The inherited six-scenario EO117 extract remains separate from the new full BAU1 download. Public OECD SDMX requests returned HTTP 403 during acquisition, so OECD series were obtained through the DBnomics mirror and retained as raw JSON.")
table_out(anchor_table[, .(Variable = variable, `Initial value (fraction)` = value, Status = status)])
paragraph("The 2022 SOCX pension share is rolled to 2024 using observed 2022/2024 older-age employment rates and the documented pension equation. Australia's early-retirement series contains no observations; it is treated explicitly as zero, with that assumption recorded by programme. Missing values are not silently treated as zeros elsewhere. Age shares come from annual UN WPP 2024 values; population levels are scaled to OECD totals, with the original UN levels retained for audit.")
paragraph("Future age-specific employment is the main approximation. Changes in the OECD aggregate 15-74 employment odds are applied to observed 2024 rates for 55-64 and 65-74, allowing within-band population composition to evolve. The age rates are not jointly re-estimated to reproduce the aggregate labour force and do not reproduce the OECD cohort model or its retirement-law effects. A fixed-age-rate alternative is provided. The mixed data vintage and broad PBO expense anchor also limit exact matching.")
paragraph("GDP ratios begin at observed calendar-2024 GDP. For the illustrative nominal denominator, subsequent growth combines OECD potential real output growth with BAU1 GDP-deflator growth; the initial output gap is not modelled separately. Other climate scenarios use their own real-output growth and the same BAU1 deflator. PBO FY2023-24 is paired with calendar 2024; fiscal years are not interpolated or silently treated as identical concepts. The PBO expense residual includes items that do not exactly equal OECD current primary spending.")

heading("Sensitivities and why growth's source matters")
figure_out("03_spending_sensitivities", "Figure 3. One-at-a-time spending-rule changes, using the same anchors and macro inputs.")
figure_out("05_growth_source", "Figure 4. Same output gain, different spending effects: controlled 10% shocks by 2060.")
g <- growth_effects[year == 2060]
paragraph(sprintf("A 10%% output gain introduced gradually by 2060 changes primary spending by %+.2f points of GDP when it comes from productivity, and %+.2f points when it comes from employment. Both shocks raise real output and the illustrative nominal denominator by exactly 10%%. In the productivity case, wage-linked pensions and residual services grow with the denominator; health's income and cost channels also respond. More employment instead improves beneficiary-to-worker ratios. This is a model mechanism, not an estimated causal effect for Australia.",
 g[case == "productivity_10pct", primary_ratio_effect_pp], g[case == "employment_10pct", primary_ratio_effect_pp]))
table_out(contrib[year == 2060, .(Component = component, Driver = driver, `Contribution, pp` = contribution_pp)])
paragraph("Health's negative income contribution in this table is an effect on spending/GDP: an income elasticity below one makes the denominator grow faster than the associated spending volume. It does not imply falling real health spending. Contributions use an exact allocation of multiplicative log changes and reconcile to each component's level change.")

heading("Debt and revenue: a conditional illustration")
paragraph("Two closures use identical primary spending and exogenous OECD interest rates. One holds primary revenue/GDP fixed. The other implements the 2017 revenue reaction function (target-gap coefficient 0.4, debt-gap coefficient 0.05, annual upward revenue cap 1 percentage point of GDP) with a ten-year forward average of nominal growth. Gross financial assets/GDP remain constant; their nominal acquisition is financed and is included in the gross-liability recursion. Interest receipts are modelled separately from interest payments.")
figure_out("04_debt_closure", "Figure 5. Conditional debt paths under fixed revenue and the documented fiscal adjustment rule.")
paragraph("These paths start from the EO117 2024 balance sheet and begin the rule in 2025, targeting the initial gross-liability ratio. EO117's published global baseline instead begins adjustment in 2027 and targets its 2026 ratio. The survey chart uses its own fiscal assumptions. The simulation does not solve the global model, endogenise interest rates, include asset revaluations, or reproduce Australian tax-bracket policy. OECD net financial liabilities include a broader asset concept than PBO net debt. The published_debt_comparison.csv file is a diagnostic comparison with an explicit comparability warning, not a validation claim.")

heading("Lessons for our Australian projection approaches")
figure_out("06_existing_models", "Figure 6. Published OECD spending-component changes and our model changes over 2024-2060.")
table_out(endpoint_comparison[, .(Method = method, Period = paste0(start_year, "-", end_year),
                                  `Change in spending, pp` = change_pp)])
paragraph(sprintf("The published OECD comparator is +%.2f points of GDP, the sum of health, pensions and other primary spending in Figure 1.14. It is shown separately from our independent +%.2f-point calculation. The +%.2f-point total fiscal-pressure indicator includes interest and other fiscal factors and is not used as a spending comparator. The workbook supplies endpoint spending changes only, so no annual OECD spending path or 2030-to-2060 change has been invented.",
 endpoint_comparison[source_type == "Published OECD benchmark", change_pp],
 endpoint_comparison[source_type == "Independent equation implementation", change_pp],
 benchmark_spending[component == "total_pressure", published_change_pp]))
paragraph("To use the published 2024-2060 period, each saved Australian model's 2060 endpoint is compared with the PBO FY2023-24 historical anchor for its expenditure concept. Bottom-up spending is primary expenses, while streamlined spending adds net capital investment. The historical anchors are added for comparison only; none of the saved forecasts is changed or re-estimated. This aligns the comparison period but does not harmonise expenditure coverage, macro inputs or data vintages. The published OECD sum does not separately identify capital spending.")
if (all(c("Bottom-up fixed exposure", "Bottom-up data enhanced") %in% endpoint_comparison$method)) {
  paragraph(sprintf("On this common-period comparison, the existing fixed-exposure bottom-up model rises by %.2f points and the data-enhanced model by %.2f points, both much closer to the published OECD +%.2f points than our independent equation calculation. This proximity is useful context, but does not validate their component mechanisms: different inputs or offsetting component errors can give similar aggregate changes. The published result should remain the OECD comparator until the replication discrepancy is resolved.",
    endpoint_comparison[method == "Bottom-up fixed exposure", change_pp],
    endpoint_comparison[method == "Bottom-up data enhanced", change_pp],
    endpoint_comparison[source_type == "Published OECD benchmark", change_pp]))
}
figure_out("07_post_2030_model_paths", "Figure 7. Supplementary comparison of calculated paths after the common 2030 forecast anchor.")
table_out(comparison[year == 2060, .(Method = method, `Change since 2030, pp` = change_since_2030_pp)])
paragraph("This supplementary chart retains the earlier 2030 rebase, which isolates changes after the official forecast period. An actual OECD series cannot be placed on that chart from the available endpoint data. Differences between the curves reflect both modelling choices and macro assumptions and do not rank forecast accuracy.")
paragraph("First, separate health, pensions, remaining spending and investment before judging the aggregate spending path. The current Australian bottom-up social-protection age weighting pools wage-linked pensions with programmes that have different eligibility and policy drivers. An explicit pension block would make the retirement and indexation assumptions reviewable. Disability and aged-care classification must be reconciled before adding blocks together.")
paragraph("Second, distinguish service volumes, income-related demand and relative service costs. The Australian bottom-up cost base rises with real income per person and then adds category-specific excess growth. The OECD health equation uses separate income, ageing and productivity-linked cost terms. Importing its Baumol term on top of an existing full productivity allowance could double-count cost growth. A common GDP denominator, population path and cost definition should be used before comparing excess-growth parameters.")
paragraph("Third, the top-down GDP-per-capita coefficient cannot by itself identify how faster growth changes fiscal pressure. The controlled shocks show that equal output gains can have very different effects depending on employment, wages, benefit indexation and service costs. The streamlined model's explicit primary-spending and interest separation is useful, but the long-run income coefficient should be checked against these mechanisms rather than interpreted as a universal growth dividend.")
paragraph("Fourth, keep a service-standard scenario distinct from a current-legislation scenario. Constant pension benefits relative to wages represent a policy assumption; price indexation produces very different fiscal outcomes while reducing benefits relative to earnings. Similarly, a debt-stabilising revenue rule measures required fiscal adjustment, while a fixed-revenue debt simulation measures the consequences of leaving that adjustment undone. Both should be reported alongside spending pressures.")
paragraph("Fifth, retain aggregate time-series models as empirical cross-checks and use structural blocks to explain long-run assumptions. This OECD implementation is calibrated rather than estimated as an Australian aggregate regression. It does not resolve the identification, collinearity or extrapolation issues in the Shapley, ARIMA and streamlined models, nor does its proximity on health validate all its assumptions.")

heading("Recent IMF projections: a comparison over the available horizons")
paragraph("Two IMF sources are included: the live Fiscal Monitor database snapshot retrieved on 23 October 2026, and the April 2026 Fiscal Monitor structural fiscal indicators (Table A23). The IMF portal advertises an October 2026 database, but the JSON response does not embed a release label; the cached series is therefore identified by retrieval date. The structural table is explicitly dated April 2026. The January 2026 Australia Article IV report is a separate country-policy source, not the assumed vintage of either series.")
table_out(data.table(Source = c("Published OECD Australia survey", "IMF Fiscal Monitor live snapshot",
  "IMF April 2026 structural indicators", "IMF April 2026 structural indicators"),
  Measure = c("Health + pensions + other primary spending change", "Annual general-government total expenditure",
    "Annual health and pension spending-share changes", "Discounted cumulative health and pension spending changes"),
  Period = c("2024-2060", "Through 2031; matched comparison 2025-2030", "2025-2030", "2025-2050 net present values")))
paragraph("The IMF total-expenditure series covers general government, which is appropriate for a consolidated Australian comparison. It includes interest and net capital investment. Comparing it directly with our primary-expense paths would understate our spending by omitting those items. The new comparison adds the saved interest and capital inputs to the bottom-up models and uses the streamlined total fiscal-expenditure outputs. The enhanced bottom-up case borrows the baseline interest path for this short-horizon comparison only.")
figure_out("08_imf_medium_term", "Figure 8. IMF and project total-expenditure paths over the available medium-term horizon.")
table_out(imf_endpoints[, .(Method = method, `2025, % GDP` = initial_spending_pct,
                           `2030, % GDP` = end_spending_pct, `Change, pp` = change_pp)])
imf_e <- imf_endpoints[method == "IMF Fiscal Monitor (live snapshot)"]
paragraph(sprintf("The live IMF series shows total spending of %.2f%% of GDP in 2025 and %.2f%% in 2030, a change of %+.2f percentage points. Its 2031 endpoint is %.2f%% of GDP. This is a medium-term policy forecast; a declining spending share over these years is compatible with increasing ageing pressures over subsequent decades. Our Australian primary-spending models are anchored to PBO forecasts through 2030, so their near-term comparison with the IMF mainly tests the official forecast anchor, accounting coverage and macro assumptions, rather than their different long-run estimation methods.",
  imf_e$initial_spending_pct, imf_e$end_spending_pct, imf_e$change_pp,
  imf_exp[year == 2031, value_pct]))
paragraph("The bottom-up totals also reflect the saved debt module's interest calculation: in 2030 they are 36.41% of GDP, compared with the PBO total of 37.10%, despite sharing the PBO primary-spending anchor. This 0.69-point difference comes from interest, rather than the category spending equations. The enhanced case uses the same interest path in this comparison. These totals should therefore not be read as independent forecasts of primary spending over the PBO anchor period.")
paragraph("Fiscal Monitor documentation bases Australian projections on Commonwealth and state/territory budgets with IMF staff estimates and adjustments. The country Article IV report describes consolidation through operating-expense restraint, care-sector reforms and infrastructure normalisation. These policy assumptions differ from a long-run exercise that maintains public service standards and benefit generosity. Calendar-year IMF data are paired here with Australian fiscal years labelled by ending year; the report does not claim exact temporal or accounting equivalence.")
figure_out("09_imf_components", "Figure 9. Published IMF health and pension pressures compared over 2025-2030.")
table_out(component_comparison[, .(Method = method, Component = component,
                                  Period = paste0(start_year, "-", end_year), `Change, pp` = change_pp)])
paragraph("The April 2026 IMF structural table projects a 0.1-point decline in Australian public pension spending/GDP and a 0.8-point increase in health spending/GDP from 2025 to 2030. These are component pressure estimates, not a decomposition that must sum to the live database's total-expenditure forecast: the two sources have different vintages and policy treatments. Social Protection in our bottom-up model is not a pension series, and is therefore not used as a pension comparator. Health definitions also differ across IMF, SHA health-related long-term care and the ABS GFS Health function.")
paragraph("Over this period our independent OECD-equation calculation increases health spending by 0.30 points and pensions by 0.25 points. The two saved bottom-up health paths instead fall by 0.14 and 0.09 points. Both bottom-up scripts rescale every spending category each year to match the aggregate PBO primary-spending forecast through 2030. Their health changes therefore combine category drivers with aggregate calibration; they are not separately sourced official health forecasts. Comparing these calibrated shares directly with IMF structural health pressures does not isolate the health cost-growth assumption.")
paragraph("This comparison makes two model questions particularly visible. The OECD-equation pension illustration rises while the IMF pension estimate falls, reinforcing the need for Australian pension eligibility, means testing, private-superannuation maturation and indexation assumptions to be reconciled with the OECD's constant-benefit-ratio treatment. The IMF health approach uses demographic profiles and excess cost growth; its documentation assumes convergence to a 0.6% advanced-economy excess-cost-growth rate by 2050. That parameter is not interchangeable with a productivity-linked Baumol coefficient or a cost premium applied above an already wage-linked base.")
table_out(imf_structural[measure_type == "discounted_cumulative_change", .(
  Indicator = indicator, Period = paste0(start_year, "-", end_year), Value = value, Unit = unit)])
paragraph("The long-horizon IMF entries are -2.7% of GDP for pensions and 43.1% for health in net present value over 2025-2050, using a discount rate one percentage point above GDP growth. They aggregate a sequence of spending changes over many years. In particular, 43.1 is not the annual health spending share or a 43.1-point increase in 2050. The table does not reveal the unique underlying annual path or the 2050 annual endpoint. These values are preserved as NPVs and excluded from the 2024-2060 spending-change chart; no IMF 2060 spending estimate is extrapolated.", TRUE)
paragraph("For the project, the IMF therefore supplies a useful medium-term total-spending benchmark and a separate check on pension and health assumptions. It does not supply, in the sources retrieved here, a directly comparable Australian total-spending increase to 2060. A close match to either institution on an aggregate alone cannot resolve differences in benefit rules, service costs or fiscal policy assumptions. Exact requests, raw JSON, the manual structural-table transcription and source limitations are saved under data/raw/imf and data/processed/imf.")

heading("The September 2026 revision")
paragraph("WP1877, published 30 September 2026, uses EO119 rather than EO117, separately identifies defence, refines residual spending, changes the pension scope to old-age and survivors, and incorporates financial-asset revaluation effects. It should not be spliced into an older benchmark and described as an exact replication. The paper is cached alongside the earlier methodology.")
paragraph("A rule-only sensitivity transfers its Table 3 residual-spending weights: education 22%, other social protection 31%, housing 2%, and other services 45%. Education follows population aged 5-24 and two-thirds of real-wage growth; social protection follows population and real wages; housing follows population and general prices; the remaining share stays constant/GDP. It keeps our older pension scope, capital and defence treatment unchanged. It is a sensitivity to the residual rule, not the complete EO119 model.")
paragraph(sprintf("With the same EO117 macro inputs and anchors, this rule-only change makes residual spending change by %+.2f points through 2060, versus %+.2f under the older rule. This reinforces the need to specify which OECD vintage is being replicated before drawing conclusions from a comparison.",
 spending_long[scenario == "BAU1" & case == "september_2026_other_rule_only" & component == "other_ratio" & year == 2060, change_pp],
 spending_long[scenario == "BAU1" & case == "documented_equations" & component == "other_ratio" & year == 2060, change_pp]))

heading("Reproduction, checks and remaining work")
paragraph("Run from the projections directory: & 'C:\\Program Files\\R\\R-4.5.1\\bin\\Rscript.exe' --vanilla OECD_long_run_scenario\\run_fiscal_replication.R. This uses cached files and works without network access. Add --download to acquire missing public source files. The Word report is generated when officer is installed; HTML and CSVs are always generated. R package versions are recorded in session_info.txt.")
paragraph(sprintf("All %d numerical validation checks passed, including scale and inflation invariance, component reconciliation, debt and asset accounting, the revenue cap, and convergence of the fiscal rule in a stationary economy. These are implementation checks; the unresolved published-benchmark gaps are reported separately and are not counted as successful numerical replication.", nrow(checks)))
table_out(checks)
paragraph("For an exact numerical replication, the remaining needs are the fiscal starting-level and transition adjustments used for the Australia survey chart, the matching age/sex employment cohort paths, and a reconciliation of expenditure, investment and liability definitions. For our own Australian model, the next useful experiment is to rerun the OECD blocks and our bottom-up rules on identical Australian demographic and macro paths with the same fiscal anchor, followed by explicit pension, health and residual-service assumption comparisons.")
heading("Sources and files")
for (j in seq_len(nrow(sources))) {
  html <- c(html, paste0('<p><a href="', gsub('&', '&amp;', sources$url[j], fixed = TRUE), '">', esc(sources$source[j]), '</a>: ', esc(sources$file[j]), '</p>'))
  if (!is.null(word)) word <- officer::body_add_par(word, paste(sources$source[j], sources$url[j], sources$file[j], sep = " | "))
}
html <- c(html, "</body></html>")
writeLines(html, file.path(out, "fiscal_replication_report.html"), useBytes = TRUE)
if (!is.null(word)) print(word, target = file.path(out, "fiscal_replication_report.docx"))
capture.output(sessionInfo(), file = file.path(out, "session_info.txt"))
message("Wrote HTML/Word assessment, data documentation and nine figure pairs.")

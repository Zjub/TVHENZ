# IMF is an additional benchmark. Its available horizons/metrics are preserved.
imf_raw <- file.path(root, "data", "raw", "imf")
imf_processed <- file.path(root, "data", "processed", "imf")
dir.create(imf_processed, recursive = TRUE, showWarnings = FALSE)
imf_indicator_codes <- c("G_X_G01_GDP_PT", "GGXCNL_G01_GDP_PT", "GGXONLB_G01_GDP_PT", "G_XWDG_G01_GDP_PT")
imf_long <- rbindlist(lapply(imf_indicator_codes, function(code) {
  path <- file.path(imf_raw, paste0(code, "_AUS.json"))
  if (!file.exists(path)) stop("Missing IMF source: ", path, "; run scripts/03b_download_imf_sources.R")
  x <- jsonlite::fromJSON(path, simplifyVector = FALSE)$values[[code]][["AUS"]]
  if (!length(x)) stop("Australian IMF series is empty: ", code)
  values <- unlist(x)
  data.table(indicator = code, country = "AUS", year = as.integer(names(values)), value_pct = as.numeric(values))
}))
imf_long[, `:=`(retrieval_date = "2026-10-23",
  edition = "Live Fiscal Monitor snapshot; API response does not embed edition",
  unit = "Percent of GDP", source_url = paste0("https://www.imf.org/external/datamapper/api/v1/", indicator, "/AUS"))]
fwrite(imf_long, file.path(imf_processed, "fiscal_monitor_australia_snapshot.csv"))
imf_exp <- imf_long[indicator == "G_X_G01_GDP_PT"]
stopifnot(!anyDuplicated(imf_exp$year), all(2024:2031 %in% imf_exp$year))
imf_structural <- fread(file.path(imf_raw, "fiscal_monitor_april_2026_australia_structural.csv"))
fwrite(imf_structural, file.path(imf_processed, "fiscal_monitor_structural_indicators.csv"))

# All total-expenditure comparators include interest and net capital investment.
# PBO fiscal years are paired by ending year to IMF calendar years; no claim of
# exact calendar alignment or identical accounting concepts is made.
imf_comparison <- list(imf_exp[year %between% c(2025L, 2031L), .(
  year, method = "IMF Fiscal Monitor (live snapshot)", total_expenditure_pct = value_pct,
  coverage = "General government total expenditure", source = "IMF API snapshot 23 October 2026")])
imf_comparison[[2]] <- official[year %between% c(2025L, 2030L), .(
  year, method = "PBO official forecast", total_expenditure_pct = 100 * (expenses_ratio_gdp + net_capital_investment_ratio_gdp),
  coverage = "Consolidated expenses plus net capital investment", source = "Cached PBO extract")]
imf_comparison[[3]] <- debt[case == "documented_equations" & closure == "oecd_rule" & year %between% c(2025L, 2031L), .(
  year, method = "OECD equations: revenue adjustment rule", total_expenditure_pct = 100 * total_outlays_ratio,
  coverage = "Independent primary expenditure plus endogenous interest", source = "Existing independent calculation")]
imf_comparison[[4]] <- debt[case == "documented_equations" & closure == "fixed_revenue" & year %between% c(2025L, 2031L), .(
  year, method = "OECD equations: fixed revenue", total_expenditure_pct = 100 * total_outlays_ratio,
  coverage = "Independent primary expenditure plus endogenous interest", source = "Existing independent calculation")]

# Read the project's saved bottom-up debt module for interest and capital costs.
bd_path <- file.path(parent_tables, "debt_paths_bottom_up.csv")
if (file.exists(bd_path)) {
  saved_bottom <- fread(bd_path)[scenario == "central" & revenue_scenario == "central"]
  stopifnot(!anyDuplicated(saved_bottom$year))
  imf_comparison[[length(imf_comparison) + 1L]] <- saved_bottom[year %between% c(2025L, 2031L), .(
    year, method = "Bottom-up fixed exposure + interest + capital",
    total_expenditure_pct = 100 * (total_expense_ratio + net_capital_investment_ratio),
    coverage = "Primary expenses plus saved interest and net capital investment", source = "Saved bottom-up debt module")]
  enhanced_path <- file.path(parent_tables, "bottom_up_data_enhanced_total_projections.csv")
  if (file.exists(enhanced_path)) {
    enhanced <- merge(fread(enhanced_path)[scenario == "central", .(year, primary_expense_ratio)],
      saved_bottom[, .(year, interest_ratio, net_capital_investment_ratio)], by = "year")
    imf_comparison[[length(imf_comparison) + 1L]] <- enhanced[year %between% c(2025L, 2031L), .(
      year, method = "Bottom-up enhanced + common interest + capital",
      total_expenditure_pct = 100 * (primary_expense_ratio + interest_ratio + net_capital_investment_ratio),
      coverage = "Enhanced primary expenses plus baseline bottom-up interest and capital",
      source = "Saved enhanced spending; baseline interest borrowed for comparison only")]
  }
}
stream_debt <- file.path(dirname(root), "2026 projections", "streamlined_top_down", "outputs", "tables", "debt_paths.csv")
if (file.exists(stream_debt)) {
  sd <- fread(stream_debt)[anchor_type == "pbo" & year %between% c(2025L, 2031L)]
  sd[year == 2025L & !is.finite(total_fiscal_expenditure_ratio),
     total_fiscal_expenditure_ratio := official[year == 2025, expenses_ratio_gdp + net_capital_investment_ratio_gdp]]
  imf_comparison[[length(imf_comparison) + 1L]] <- sd[, .(
    year, method = paste0("Streamlined: ", model), total_expenditure_pct = total_fiscal_expenditure_ratio * 100,
    coverage = "Primary fiscal expenditure plus saved endogenous interest", source = "Saved streamlined PBO-anchored debt paths")]
}
imf_comparison <- rbindlist(imf_comparison)
stopifnot(!anyDuplicated(imf_comparison[, .(year, method)]), all(is.finite(imf_comparison$total_expenditure_pct)))
imf_comparison[, change_since_2025_pp := total_expenditure_pct - total_expenditure_pct[year == 2025L], by = method]
imf_comparison[, limitation := "Interest and capital included; accounting coverage, data vintage and calendar/fiscal years differ"]
fwrite(imf_comparison, file.path(out, "imf_medium_term_comparison.csv"))
imf_endpoints <- imf_comparison[, .(start_year = 2025L, end_year = 2030L,
  initial_spending_pct = total_expenditure_pct[year == 2025L],
  end_spending_pct = total_expenditure_pct[year == 2030L],
  change_pp = total_expenditure_pct[year == 2030L] - total_expenditure_pct[year == 2025L]), by = method]
fwrite(imf_endpoints, file.path(out, "imf_medium_term_endpoints.csv"))

# Only genuinely comparable component horizons are used; no IMF 2060 path is
# extrapolated. IMF public health is not identical to ABS GFS Health or SHA LTC.
component_comparison <- rbindlist(list(
  imf_structural[measure_type == "annual_flow_change", .(method = "IMF Fiscal Monitor April 2026",
    component = fifelse(grepl("pension", indicator), "Public pensions", "Health"),
    start_year, end_year, change_pp = value,
    qualification = "Published IMF flow change; health/pension coverage may differ from other models")],
  data.table(method = "OECD equations with available inputs", component = c("Health", "Public pensions"),
    start_year = 2025L, end_year = 2030L,
    change_pp = c(100 * (central[year == 2030, health_ratio] - central[year == 2025, health_ratio]),
                  100 * (central[year == 2030, pension_ratio] - central[year == 2025, pension_ratio])),
    qualification = "Independent calculation, not published OECD; health includes health-related LTC")
))
for (spec in list(c("bottom_up_category_projections.csv", "Bottom-up fixed exposure"),
                  c("bottom_up_data_enhanced_category_projections.csv", "Bottom-up data enhanced"))) {
  path <- file.path(parent_tables, spec[1])
  if (file.exists(path)) {
    z <- fread(path)[scenario == "central" & category == "Health"]
    component_comparison <- rbind(component_comparison, data.table(method = spec[2], component = "Health",
      start_year = 2025L, end_year = 2030L,
      change_pp = 100 * (z[year == 2030, share_gdp] - z[year == 2025, share_gdp]),
      qualification = "ABS GFS Health function; pensions not separately projected in existing Social Protection block"))
  }
}
fwrite(component_comparison, file.path(out, "imf_component_comparison.csv"))
imf_sources <- data.table(
  source = c("IMF Fiscal Monitor live database", "IMF Fiscal Monitor April 2026, Table A23", "IMF Australia 2025 Article IV, published January 2026"),
  url = c("https://www.imf.org/external/datamapper/datasets/FM",
    "https://www.imf.org/-/media/files/publications/fiscal-monitor/2026/april/english/msa.pdf",
    "https://www.imf.org/-/media/files/publications/cr/2026/english/1ausea2026001-source-pdf.pdf"),
  coverage = c("General government; annual fiscal indicators to 2031", "Public pension and health pressures; 2025-2030 flows and 2025-2050 NPVs",
    "Country-specific fiscal and policy discussion; not used as the live database vintage"),
  status = c("Raw official API JSON cached 23 October 2026; API does not embed release label",
    "Australian row manually transcribed from primary PDF via web access; direct shell PDF download returned 403",
    "Read via official IMF publication; separate vintage; no values attributed to it in the live series"))
fwrite(imf_sources, file.path(imf_processed, "imf_sources_and_limitations.csv"))
files <- list.files(imf_raw, full.names = TRUE)
files <- files[!grepl("\\.part$", files)]
fwrite(data.table(file = basename(files), bytes = file.info(files)$size,
                 md5 = unname(tools::md5sum(files))), file.path(imf_processed, "source_file_checksums.csv"))
message("Built IMF total-expenditure and component comparisons; preserved flow/NPV distinctions.")

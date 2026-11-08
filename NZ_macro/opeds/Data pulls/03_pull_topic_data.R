# Public data for five NZ op-ed topics. No argument is assumed.
# Rscript "Data pulls/03_pull_topic_data.R"
# NZ_DATA_REFRESH=false reuses the source snapshots. NZ_OPED_PYTHON selects Python.
# R packages are checked, never installed. PDF extraction uses the small helper
# in helpers/ and Python with PyMuPDF; it does not use transcribed estimates.

required <- c("dplyr", "httr2", "jsonlite", "purrr", "readr", "readxl", "rvest", "tidyr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing R packages: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages({library(dplyr); library(purrr); library(readr); library(tidyr)})
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[1]))) else normalizePath("Data pulls")
raw_dir <- file.path(root, "raw", "op_ed_topics")
out_dir <- file.path(root, "processed", "op_ed_topics")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
refresh <- tolower(Sys.getenv("NZ_DATA_REFRESH", "true")) %in% c("true", "1", "yes")
as_of <- as.Date("2026-10-14") # snapshot cutoff; do not silently incorporate later releases
manifest <- list()
number <- function(x) suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
write_data <- function(x, name) write_csv(x, file.path(out_dir, paste0(name, ".csv")), na = "")
excel <- function(path, sheet) readxl::read_excel(path, sheet = sheet, col_names = FALSE,
  range = readxl::cell_limits(c(1, 1), c(NA, NA)), .name_repair = "minimal")

pull <- function(id, url, filename, description, kind = "Official data") {
  path <- file.path(raw_dir, filename)
  cached <- !refresh && file.exists(path) && file.info(path)$size > 0
  if (!cached) {
    message("Pulling ", id)
    temp <- tempfile(tmpdir = raw_dir)
    on.exit(unlink(temp), add = TRUE)
    response <- httr2::request(url) |>
      httr2::req_user_agent("NZ op-ed public-data research (R httr2)") |>
      httr2::req_timeout(120) |> httr2::req_retry(max_tries = 3) |>
      httr2::req_perform(path = temp)
    if (httr2::resp_status(response) != 200 || file.info(temp)$size < 20) stop("Failed source: ", id)
    # Reject an HTML error page masquerading as a spreadsheet, CSV or PDF.
    content_type <- httr2::resp_header(response, "content-type")
    if (!grepl("\\.html$", filename) && !is.null(content_type) && grepl("text/html", content_type)) stop("HTML returned for ", id)
    if (!file.copy(temp, path, overwrite = TRUE)) stop("Cannot save ", path)
  }
  manifest[[length(manifest) + 1]] <<- tibble(
    source_id = id, description = description, url = url, kind = kind,
    snapshot_cutoff = as.character(as_of), cached_this_run = cached,
    file_saved_utc = format(file.info(path)$mtime, tz = "UTC", usetz = TRUE),
    md5 = unname(tools::md5sum(path)), local_file = filename
  )
  path
}

sources <- tibble::tribble(
  ~id, ~file, ~url, ~description,
  "psc_fte", "psc_fte.csv", "https://www.publicservice.govt.nz/assets/Five-year-trend.csv", "PSC departmental FTEs, 2000 to March 2026",
  "psc_sectors", "psc_sectors.csv", "https://www.publicservice.govt.nz/assets/Census/Workforce-Data-CSV-files-2025/NZ-workforce-by-sectorin-1.csv", "PSC/Stats NZ workforce headcount by sector and region, 2000-2023 (download linked from 2025 page)",
  "psc_contractors", "psc_contractors.csv", "https://www.publicservice.govt.nz/assets/Census/Workforce-Data-CSV-files-2026/CC-agency-quarterly.csv", "PSC annual and year-to-date contractor OPEX and CAPEX",
  "psc_contractors_share", "psc_contractors_share.csv", "https://www.publicservice.govt.nz/assets/Census/Workforce-Data-CSV-files-2026/CC-share-quarterly.csv", "PSC contractor OPEX share of total workforce spending",
  "psc_oia", "psc_oia_2026.xlsx", "https://www.publicservice.govt.nz/assets/OIA-Statistics-Jan-Jun-2026-Final.xlsx", "PSC OIA requests and service timeliness, Jan-Jun 2026",
  "msd_benefits", "msd_june_2026.xlsx", "https://www.msd.govt.nz/documents/about-msd-and-our-work/publications-resources/statistics/benefit/2026/quarterly-benefit-fact-sheets-national-benefit-tables-june-2026.xlsx", "MSD working-age benefit and Jobseeker subgroup quarterly counts, June 2021-June 2026",
  "treasury_expenses", "expense_tables_2026.xlsx", "https://www.treasury.govt.nz/sites/default/files/2026-05/befu26-data-expense-tables.xlsx", "Treasury BEFU 2026 detailed expenses: 2021-2025 actual, 2026-2030 forecasts",
  "treasury_macro", "befu_2026.xlsx", "https://www.treasury.govt.nz/sites/default/files/2026-05/befu26-charts-data.xlsx", "Treasury BEFU 2026 economic and fiscal time series",
  "treasury_tax_expenditures", "tax_expenditures_2026.pdf", "https://www.treasury.govt.nz/sites/default/files/2026-05/b26-tax-expenditure-statement.pdf", "Treasury 2026 tax expenditure statement; Table 3 and provisions in Tables 4-5",
  "treasury_vote_business", "vote_business_2026.pdf", "https://www.treasury.govt.nz/sites/default/files/2026-05/est26-v1-buscin.pdf", "Vote Business, Science and Innovation 2026/27; international screen appropriation",
  "stats_rd", "stats_rd_2025.csv", "https://www.stats.govt.nz/assets/Uploads/Research-and-development-survey/Research-and-development-survey-2025/Download-data/research-and-development-survey-2025.csv", "Stats NZ R&D survey 2025, expenditure, sector, size and industry; suppressed cells retained as missing",
  "stats_rd_notes", "stats_rd_notes.csv", "https://www.stats.govt.nz/assets/Uploads/Research-and-development-survey/Research-and-development-survey-2025/Download-data/research-and-development-survey-2025-csv-notes.csv", "Stats NZ R&D survey notes and definitions"
)
files <- setNames(map(seq_len(nrow(sources)), function(i) {
  s <- sources[i, ]; pull(s$id, s$url, s$file, s$description)
}), sources$id)

# Policy pages are saved as well as statistical downloads: scenario parameters
# can therefore be checked against the actual source used in this run.
policy <- tibble::tribble(
  ~id, ~file, ~url, ~description,
  "dst_status", "dst_status.html", "https://www.taxpolicy.ird.govt.nz/bills/53-dst-23", "IRD DST Bill: withdrawn 21 May 2025",
  "dst_ris", "dst_ris_2023.pdf", "https://www.regulation.govt.nz/assets/RIS-Documents/ria-ird-dst-aug23.pdf", "IRD 2023 DST regulatory impact statement; historical revenue estimate",
  "tax_rates", "tax_rates.html", "https://www.ird.govt.nz/income-tax/income-tax-for-individuals/tax-codes-and-tax-rates-for-individuals/tax-rates-for-individuals", "IRD full-year personal tax scale from April 2025",
  "tax_rates_2010", "tax_rates_2010.html", "https://www.taxtechnical.ird.govt.nz/new-legislation/act-articles/taxation-budget-measures-act-2010/personal-tax-cuts", "IRD personal tax scale introduced October 2010; full-year rates from 2011/12",
  "company_tax", "company_tax.html", "https://www.ird.govt.nz/income-tax/income-tax-for-businesses-and-organisations/tax-rates-for-businesses", "IRD company tax rate",
  "psc_workforce_page", "psc_workforce.html", "https://www.publicservice.govt.nz/data/workforce-data/public-sector-composition/workforce-size", "PSC workforce page with 2025 sector headlines, distinct from the older linked CSV",
  "investment_boost", "investment_boost.html", "https://www.ird.govt.nz/investment-boost", "IRD 20% immediate deduction then depreciation on remaining 80%",
  "games_2025", "games_2025.html", "https://www.nzonair.govt.nz/news/nz-on-air-announces-second-year-gdsr-recipients-showing-improved-economic-value-and-job-creation/", "NZ On Air 2025 GDSR allocations and recipients",
  "games_2026", "games_2026.html", "https://www.nzonair.govt.nz/news/our-year-in-review-2025-2026/game-development-sector-rebate1/", "NZ On Air 2026 GDSR allocations and recipients"
)
policy_files <- setNames(map(seq_len(nrow(policy)), function(i) {
  s <- policy[i, ]; pull(s$id, s$url, s$file, s$description, "Policy / programme source")
}), policy$id)

# World Bank API caches raw responses; no imputation of missing indicator years.
indicators <- c(
  population = "SP.POP.TOTL", cpi = "FP.CPI.TOTL", inflation = "FP.CPI.TOTL.ZG",
  ict_exports_usd = "BX.GSR.CCIS.CD", ict_exports_share = "BX.GSR.CCIS.ZS",
  internet_users = "IT.NET.USER.ZS", rd_gdp = "GB.XPD.RSDV.GD.ZS"
)
wb <- imap_dfr(indicators, function(code, label) {
  url <- paste0("https://api.worldbank.org/v2/country/NZL;AUS;OED/indicator/", code,
                "?format=json&per_page=20000&date=2000:2026")
  path <- pull(paste0("wb_", label), url, paste0("wb_", label, ".json"), paste("World Bank", code), "International official statistics")
  obj <- jsonlite::fromJSON(path, flatten = TRUE)
  if (!is.data.frame(obj[[2]])) stop("Invalid World Bank response for ", code)
  if (as.numeric(obj[[1]]$pages) != 1) stop("World Bank response needs pagination")
  obj[[2]] |> transmute(country = country.value, iso3 = countryiso3code,
                       year = as.integer(date), measure = label, indicator = code, value) |>
    filter(!is.na(value), year <= 2025)
})
write_data(wb, "world_bank")
cpi <- wb |> filter(iso3 == "NZL", measure == "cpi") |> select(year, cpi = value) |> arrange(year)
stopifnot(all(c(2010, 2021, 2024, 2025) %in% cpi$year), !anyDuplicated(cpi$year))
cpi_2010 <- cpi$cpi[cpi$year == 2010]
cpi_2025 <- cpi$cpi[cpi$year == 2025]

# Parse Treasury workbooks directly: their economic, fiscal, and detailed
# expense tables have different label and year columns.
wide_series <- function(path, sheet, label_col, year_row, status_row, value_cols, rows) {
  z <- excel(path, sheet)
  years <- number(unlist(z[year_row, value_cols], use.names = FALSE))
  status <- trimws(as.character(unlist(z[status_row, value_cols], use.names = FALSE)))
  map_dfr(rows, function(i) {
    label <- trimws(as.character(z[[label_col]][i]))
    if (is.na(label)) return(NULL)
    tibble(measure = label, year = years, status = status,
           value = number(unlist(z[i, value_cols], use.names = FALSE)), source_row = i)
  }) |> filter(!is.na(year), !is.na(value))
}
econ_raw <- excel(files$treasury_macro, "Economic Time Series")
econ <- wide_series(files$treasury_macro, "Economic Time Series", 2, 4, 5, 3:ncol(econ_raw), 6:nrow(econ_raw))
fiscal_raw <- excel(files$treasury_macro, "Fiscal Time Series")
fiscal <- wide_series(files$treasury_macro, "Fiscal Time Series", 3, 4, 5, 4:ncol(fiscal_raw), 10:30)
write_data(econ, "treasury_economic")
write_data(fiscal, "treasury_fiscal")
gdp_row <- which(grepl("Nominal expenditure GDP", as.character(fiscal_raw[[2]])))
stopifnot(length(gdp_row) == 1)
gdp <- wide_series(files$treasury_macro, "Fiscal Time Series", 2, 4, 5, 4:ncol(fiscal_raw), gdp_row) |>
  transmute(year, gdp_millions = value)
core <- fiscal |> filter(measure == "Core Crown expenses") |> transmute(year, core_expenses_millions = value)
tax <- fiscal |> filter(measure == "Core Crown tax revenue") |> transmute(year, tax_millions = value)
macro <- gdp |> left_join(core, by = "year") |> left_join(tax, by = "year")
stopifnot(nrow(macro) >= 10, !anyDuplicated(macro$year), all(macro$gdp_millions > 100000))
write_data(macro, "fiscal_denominators")

benefits <- wide_series(files$treasury_expenses, "Core Crown Expense Tables", 4, 53, 54, 6:15, 56:77) |>
  mutate(measure = sub("[0-9,]+$", "", measure)) |>
  left_join(macro, by = "year") |> left_join(cpi, by = "year") |>
  mutate(percent_gdp = 100 * value / gdp_millions,
         percent_core_expenses = 100 * value / core_expenses_millions,
         real_2025_millions = value * cpi_2025 / .data$cpi)
benefit_total <- wide_series(files$treasury_expenses, "Core Crown Expense Tables", 4, 53, 54, 6:15, 79)
reconciled <- benefits |> group_by(year) |> summarise(value = sum(value), .groups = "drop") |>
  left_join(benefit_total |> select(year, published_total = value), by = "year")
stopifnot(all(abs(reconciled$value - reconciled$published_total) <= 2))
write_data(benefits, "benefit_expenses")
write_data(benefit_total, "benefit_total")
screen_domestic <- wide_series(files$treasury_expenses, "Core Crown Expense Tables", 4, 312, 313, 6:15, 317)
# Row positions are guarded by the label; find the proper header if it moves.
stopifnot(all(screen_domestic$measure == "Screen Production Grants"))
write_data(screen_domestic, "screen_domestic")

quarter_date <- function(x) {
  # MSD uses Jun-21 etc. Return quarter end rather than the first of month.
  m <- match(substr(x, 1, 3), month.abb)
  y <- 2000 + number(substr(x, 5, 6))
  as.Date(sprintf("%04d-%02d-01", y + (m == 12), ifelse(m == 12, 1, m + 1))) - 1
}
msd_table <- function(sheet, label_col, value_start, rows) {
  z <- excel(files$msd_benefits, sheet)
  columns <- value_start:ncol(z)
  dates <- quarter_date(as.character(unlist(z[4, columns], use.names = FALSE)))
  map_dfr(rows, function(i) tibble(
    programme = trimws(as.character(z[[label_col]][i])), date = dates,
    recipients = number(unlist(z[i, columns], use.names = FALSE))
  )) |> filter(!is.na(date), date <= as_of, !is.na(recipients))
}
counts <- msd_table("Summary table - last 5 years", 2, 3, 5:9)
js <- msd_table("JS - last 5 years", 3, 4, 5:6) |>
  mutate(subgroup = if_else(grepl("Work Ready", programme), "Work ready", "Health condition or disability"))
js_duration <- msd_table("JS - last 5 years", 3, 4, 13:14)
stopifnot(n_distinct(counts$date) == 21, n_distinct(js$date) == 21,
          all(c("Work ready", "Health condition or disability") %in% js$subgroup))
# MSD randomly rounds to multiples of three. Published subgroup sums may differ
# from the headline total, so check a rounding tolerance and retain both.
check <- js |> group_by(date) |> summarise(n = sum(recipients), .groups = "drop") |>
  left_join(counts |> filter(programme == "Jobseeker Support"), by = "date")
stopifnot(all(abs(check$n - check$recipients) <= 9))
write_data(counts, "benefit_recipients")
write_data(js, "jobseeker_subgroups")
write_data(js_duration, "jobseeker_duration")

fte <- read_csv(files$psc_fte, show_col_types = FALSE) |>
  transmute(agency = `Org Name`, date = as.Date(SurveyDate, "%d/%m/%Y"), fte = number(FTEs)) |>
  filter(date <= as_of)
fte_total <- fte |> filter(agency == "Public Service") |> select(date, fte) |> arrange(date)
fte <- fte |> filter(agency != "Public Service")
stopifnot(!anyDuplicated(fte_total$date))
stopifnot(abs(fte_total$fte[fte_total$date == as.Date("2025-06-30")] - 62654) < 2)
sectors <- read_csv(files$psc_sectors, show_col_types = FALSE) |>
  transmute(year = as.integer(format(as.Date(sub(" .*", "", Date), "%m/%d/%Y"), "%Y")),
            region = RegionalCouncil, sector = Sector, broad_sector = BroadSector,
            headcount = number(Headcount))
sector_totals <- sectors |> group_by(year, sector, broad_sector) |>
  summarise(headcount = sum(headcount), .groups = "drop") |>
  group_by(year) |> mutate(share_total_percent = 100 * headcount / sum(headcount)) |> ungroup() |>
  left_join(wb |> filter(iso3 == "NZL", measure == "population") |> select(year, population = value), by = "year") |>
  mutate(per_1000_residents = 1000 * headcount / population)
stopifnot(n_distinct(sector_totals$sector) == 7, max(sector_totals$year) >= 2023)
write_data(fte, "public_service_agencies")
write_data(fte_total, "public_service_fte")
write_data(sectors, "workforce_regions")
write_data(sector_totals, "workforce_sectors")

cc <- read_csv(files$psc_contractors, col_types = cols(.default = col_character())) |>
  transmute(date = as.Date(PERIOD, "%d/%m/%Y"), agency = AGENCY, type = TYPE,
            opex_millions = number(OPEX) / 1e6, capex_millions = number(CAPEX) / 1e6,
            period_basis = if_else(format(date, "%m-%d") == "06-30", "Full fiscal year", "Fiscal year to date")) |>
  filter(date <= as_of)
cc_share <- read_csv(files$psc_contractors_share, show_col_types = FALSE) |>
  transmute(date = as.Date(sub(" .*", "", `Period ending`), "%d/%m/%Y"), share_percent = Share * 100,
            period_basis = if_else(format(date, "%m-%d") == "06-30", "Full fiscal year", "Fiscal year to date"))
write_data(cc, "contractors")
write_data(cc_share, "contractors_share")

oia_raw <- excel(files$psc_oia, "Timeliness & publication")
oia <- tibble(agency_id = number(oia_raw[[1]]), agency = as.character(oia_raw[[2]]),
              requests = number(oia_raw[[3]]), within_timeframe = number(oia_raw[[4]]),
              published_share = number(oia_raw[[6]]) / number(oia_raw[[3]]), extensions = number(oia_raw[[7]]),
              average_days = number(oia_raw[[9]]), median_days = number(oia_raw[[10]])) |>
  filter(!is.na(agency_id), !is.na(agency), !is.na(requests), requests > 0) |>
  mutate(timeliness_percent = 100 * within_timeframe / requests,
         period = "January-June 2026")
stopifnot(nrow(oia) > 50, all(oia$timeliness_percent <= 100.01))
write_data(oia, "oia_service_metrics")

rd_raw <- read_csv(files$stats_rd, col_types = cols(.default = col_character()))
rd <- rd_raw |> filter(Variable == "_01_Total_RD_Expenditure", Breakdown %in% c("Sector", "Published_Industry", "Size", "Total")) |>
  transmute(breakdown = Breakdown, category = sub("^[0-9]+_", "", Breakdown_category),
            year = as.integer(Year), value_millions = number(RD_Value),
            status = Status, unit = Unit, footnotes = Footnotes,
            relative_sampling_error_percent = number(Relative_Sampling_Error))
stopifnot(nrow(rd) > 100, sum(rd$category == "Business Sector" & rd$year == 2025 & !is.na(rd$value_millions)) == 1)
write_data(rd, "rd_expenditure")
invisible(file.copy(files$stats_rd_notes, file.path(out_dir, "rd_survey_notes.csv"), overwrite = TRUE))

# The PDF parser retains the source wording, page number, confidence statements,
# and review vintage rather than treating unavailable costs as zero.
python <- Sys.getenv("NZ_OPED_PYTHON", Sys.which("python"))
if (!nzchar(python)) stop("Python with PyMuPDF is required. Set NZ_OPED_PYTHON to its executable.")
json_path <- file.path(raw_dir, "pdf_tables.json")
result <- system2(python, vapply(c(file.path(root, "helpers", "topic_pdf_extract.py"),
                   files$treasury_tax_expenditures, files$treasury_vote_business, json_path), shQuote, character(1)))
if (result != 0L) stop("PDF extraction failed. Python requires PyMuPDF (import fitz).")
pdf <- jsonlite::fromJSON(json_path)
registry <- as_tibble(pdf$registry) |>
  mutate(estimated = !is.na(annual_estimate_millions),
         type = case_when(grepl("timing", sections_and_type, ignore.case = TRUE) ~ "Timing deduction",
                          grepl("permanent", sections_and_type, ignore.case = TRUE) ~ "Permanent deduction",
                          grepl("credit", sections_and_type, ignore.case = TRUE) ~ "Tax credit",
                          grepl("Deferral", sections_and_type, ignore.case = TRUE) ~ "Deferral",
                          grepl("Exemption", sections_and_type, ignore.case = TRUE) ~ "Exemption",
                          TRUE ~ "Other / inspect source"))
stopifnot(nrow(registry) == 52, !anyDuplicated(registry$provision),
          any(registry$provision == "Partial expensing (Investment Boost)"))
tax_series <- as_tibble(pdf$series)
stopifnot(tax_series$value_millions[tax_series$programme == "R&D tax incentive" & tax_series$year_end == 2025] == 532)
write_data(registry, "tax_expenditure_register")
write_data(tax_series, "tax_system_spending")
screen <- bind_rows(
  screen_domestic |> filter(year == 2027) |> transmute(programme = "Domestic screen grants", year, value_millions = value, status = "Budget forecast"),
  tibble(programme = "International screen rebate", year = 2027,
         value_millions = pdf$international_screen_budget_2027_millions, status = "Budget appropriation")
)
stopifnot(screen$value_millions[screen$programme == "International screen rebate"] == 210)
write_data(screen, "screen_budget_2027")

page_text <- function(path) rvest::read_html(path) |> rvest::html_element("body") |> rvest::html_text2()
games_text_2025 <- page_text(policy_files$games_2025)
games_text_2026 <- page_text(policy_files$games_2026)
capture <- function(text, pattern) {
  match <- regmatches(text, regexec(pattern, text, perl = TRUE))[[1]]
  if (length(match) < 2) stop("Cannot extract programme statistic: ", pattern)
  number(match[2])
}
games <- tibble(
  year = c(2025, 2026),
  allocated_millions = c(capture(games_text_2025, "share of \\$([0-9.]+)m"),
                        capture(games_text_2026, "combined \\$([0-9.]+) million")),
  studios = c(40, capture(games_text_2026, "([0-9]+) studios received"))
)
# The 2025 release spells 'Forty' in prose. Check the source instead of passing
# a word to numeric conversion. The 2026 recipient count is extracted below.
stopifnot(grepl("Forty game development studios", games_text_2025))
games$studios <- c(40, capture(games_text_2026, "([0-9]+) studios received"))
games <- games |> mutate(basis = "Announced allocation round; not fiscal-year expense", source_id = c("games_2025", "games_2026"))
write_data(games, "game_rebate_allocations")
workforce_page <- page_text(policy_files$psc_workforce_page)
headlines <- tibble(
  year = 2025L,
  public_headcount = capture(workforce_page, "public sector employed around\\s+([0-9,]+)\\s+people in\\s+2025"),
  total_workforce = capture(workforce_page, "total workforce\\s*\\(([0-9,]+)\\)"),
  central_headcount = capture(workforce_page, "central government\\s*\\(([0-9,]+)\\)"),
  local_headcount = capture(workforce_page, "local government\\s*\\(([0-9,]+)\\)"),
  basis = "PSC page 2025 Business Demography headline; detailed download ends in 2023"
)
stopifnot(headlines$central_headcount + headlines$local_headcount == headlines$public_headcount)
write_data(headlines, "workforce_headlines_2025")

# Auditable constants. A value sourced from a historic proposal is still a
# historic proposal; user-selected rates and margins are scenario assumptions.
parameters <- tibble::tribble(
  ~parameter, ~value, ~unit, ~basis, ~source,
  "dst_rate", 0.03, "fraction", "Withdrawn 2023 proposal", "https://www.legislation.govt.nz/bill/government/2023/0288/latest/096be8ed81f46d88.pdf",
  "dst_historical_revenue_low", 86, "NZD millions/year", "2023 RIS historical estimate, NOT a current forecast", policy$url[policy$id == "dst_ris"],
  "dst_historical_revenue_high", 90, "NZD millions/year", "2023 RIS historical estimate, NOT a current forecast", policy$url[policy$id == "dst_ris"],
  "company_tax_rate", 0.28, "fraction", "NZ standard company tax rate", "https://www.ird.govt.nz/income-tax/income-tax-for-businesses-and-organisations/tax-rates-for-businesses",
  "investment_boost_immediate_fraction", 0.20, "fraction", "IRD published Investment Boost treatment", policy$url[policy$id == "investment_boost"],
  "illustrative_interest_rate", 0.05, "fraction/year", "Scenario assumption; not a measured deposit rate", "User-adjustable in script",
  "illustrative_saving_balance", 10000, "NZD", "Scenario assumption", "User-adjustable in script",
  "illustrative_asset_cost", 100000, "NZD", "Scenario assumption", "User-adjustable in script"
)
write_data(parameters, "parameters")

# Tax brackets: use full-year IRD schedules, not the 2024/25 blended schedule.
# Calculations below are snapshots of the schedule at 30 June of each year.
# ACC, credits, transfers and behavioural changes are deliberately excluded.
rates_page <- page_text(policy_files$tax_rates)
stopifnot(all(vapply(c("15,600", "53,500", "78,100", "180,000", "39%"), function(x) grepl(x, rates_page, fixed = TRUE), logical(1))))
tax_fn <- function(income, thresholds, rates = c(.105, .175, .30, .33, .39)) {
  lower <- c(0, thresholds)
  upper <- c(thresholds, Inf)
  sum(pmax(pmin(income, upper) - lower, 0) * rates)
}
old <- c(14000, 48000, 70000, 180000)
current <- c(15600, 53500, 78100, 180000)
stopifnot(tax_fn(0, current) == 0, abs(tax_fn(15600, current) - 1638) < 1e-8,
          abs(tax_fn(53500, current) - 8270.5) < 1e-8)
brackets <- crossing(year = cpi$year[cpi$year >= 2010], bracket = 1:4) |>
  left_join(cpi, by = "year") |>
  mutate(threshold = if_else(year >= 2025, current[bracket], old[bracket]),
         introduced = if_else(bracket == 4, 2021L, 2010L),
         threshold_real_2010 = threshold * cpi_2010 / .data$cpi) |>
  filter(year >= introduced)
write_data(brackets, "tax_brackets_real")
drag <- crossing(year = cpi$year[cpi$year >= 2011], real_income_2010 = c(30000, 50000, 70000, 100000)) |>
  left_join(cpi, by = "year") |>
  mutate(price_factor = .data$cpi / cpi_2010, nominal_income = real_income_2010 * price_factor) |>
  rowwise() |> mutate(
    tax_actual_scale = tax_fn(nominal_income, if (year >= 2025) current else old,
                             if (year >= 2021) c(.105, .175, .30, .33, .39) else c(.105, .175, .30, .33, .33)),
    tax_fully_indexed_2010 = tax_fn(real_income_2010, old, c(.105, .175, .30, .33, .33)) * price_factor,
    average_tax_percent = 100 * tax_actual_scale / nominal_income,
    additional_tax_real_2010 = (tax_actual_scale - tax_fully_indexed_2010) / price_factor
  ) |> ungroup()
write_data(drag, "constant_real_income_tax")
# Isolate inflation since 2021 from the new 39% top rate. The counterfactual
# indexes all 2021 thresholds and retains all 2021 rates.
latest_factor <- cpi$cpi[cpi$year == 2025] / cpi$cpi[cpi$year == 2021]
cross_section <- tibble(income_2025 = seq(10000, 250000, 1000)) |> rowwise() |>
  mutate(tax_actual = tax_fn(income_2025, current), tax_indexed_2021 = tax_fn(income_2025, old * latest_factor),
         additional_tax = tax_actual - tax_indexed_2021) |> ungroup()
write_data(cross_section, "fiscal_drag_cross_section")

interest <- crossing(year = wb$year[wb$iso3 == "NZL" & wb$measure == "inflation"], tax_rate = c(.175, .28, .33, .39)) |>
  left_join(wb |> filter(iso3 == "NZL", measure == "inflation") |> select(year, inflation_percent = value), by = "year") |>
  mutate(nominal_interest_rate = .05, nominal_interest = 10000 * .05,
         tax_nominal = nominal_interest * tax_rate,
         inflation_compensation = 10000 * inflation_percent / 100,
         tax_on_inflation_component = tax_rate * pmin(pmax(inflation_compensation, 0), nominal_interest),
         real_pre_tax_percent = 100 * ((1 + nominal_interest_rate) / (1 + inflation_percent / 100) - 1),
         real_after_tax_percent = 100 * ((1 + nominal_interest_rate * (1 - tax_rate)) / (1 + inflation_percent / 100) - 1),
         hypothetical_indexed_tax = pmax(nominal_interest - inflation_compensation, 0) * tax_rate,
         indexed_real_after_tax_percent = 100 * ((1 + nominal_interest_rate - hypothetical_indexed_tax / 10000) / (1 + inflation_percent / 100) - 1))
write_data(interest, "interest_tax_illustration")

dst <- crossing(revenue_billion = seq(.5, 6, .1), rate = c(.01, .02, .03, .05)) |>
  mutate(revenue_millions = 1000 * revenue_billion * rate)
dst_margins <- crossing(profit_margin = seq(.01, .50, .01), pass_through = c(0, .5, 1)) |>
  mutate(dst_as_percent_pre_dst_profit = 100 * .03 * (1 - pass_through) / profit_margin,
         corporate_tax_as_percent_revenue = 100 * .28 * profit_margin)
write_data(dst, "dst_revenue_scenarios")
write_data(dst_margins, "dst_margin_scenarios")

# Expensing compares the present value of equal nominal deductions. All three
# schedules deduct the same asset cost. Year 0 is the purchase/first-use year;
# this matches IRD's example of first-year depreciation alongside the boost.
deductions <- crossing(method = c("Standard depreciation", "20% Investment Boost", "Full expensing"), year = 0:4) |>
  mutate(deduction = case_when(method == "Standard depreciation" ~ 20000,
                              method == "20% Investment Boost" & year == 0 ~ 36000,
                              method == "20% Investment Boost" ~ 16000,
                              method == "Full expensing" & year == 0 ~ 100000,
                              TRUE ~ 0), tax_shield = deduction * .28)
stopifnot(all((deductions |> group_by(method) |> summarise(total = sum(deduction)))$total == 100000))
pv <- crossing(discount_rate = seq(0, .15, .005), method = unique(deductions$method)) |>
  left_join(deductions, by = "method", relationship = "many-to-many") |>
  group_by(discount_rate, method) |>
  summarise(pv_tax_shield = sum(tax_shield / (1 + discount_rate)^year), .groups = "drop") |>
  mutate(pv_percent_cost = 100 * pv_tax_shield / 100000)
write_data(deductions, "expensing_deductions")
write_data(pv, "expensing_present_values")

# Fiscal arithmetic, not a costing of any particular announced cut. A rate cut,
# recipient reduction, and benefit cancellation have different effects. Gross
# benefits include taxable transfers; lost income tax and replacement support
# mean the net Crown saving can be smaller.
js_expense <- benefits$value[benefits$year == 2025 & benefits$measure == "Jobseeker Support and Emergency Benefit"]
js_june <- js |> filter(date == as.Date("2025-06-30"))
work_share <- js_june$recipients[js_june$subgroup == "Work ready"] / sum(js_june$recipients)
cuts <- crossing(cut_percent = seq(0, 50, 5), target = c("All Jobseeker + Emergency Benefit", "Work-ready proxy"),
                 work_ready_relative_payment = c(.75, 1, 1.25)) |>
  mutate(allocated_base = if_else(target == "All Jobseeker + Emergency Benefit", js_expense,
             js_expense * work_share * work_ready_relative_payment / (work_share * work_ready_relative_payment + 1 - work_share)),
         gross_savings_millions = allocated_base * cut_percent / 100,
         percent_core_expenses = 100 * gross_savings_millions / macro$core_expenses_millions[macro$year == 2025],
         percent_gdp = 100 * gross_savings_millions / macro$gdp_millions[macro$year == 2025],
         basis = "2025 actual gross expense; June 2025 claimant share proxy; no behaviour, tax or replacement benefits")
write_data(cuts, "benefit_cut_scenarios")

measurement_notes <- tibble::tribble(
  ~topic, ~measure, ~interpretation, ~missing_for_stronger_measure,
  "Digital services taxes", "ICT services exports / internet use", "Economic context proxies; neither is inbound platform revenue or the statutory DST base", "NZ user-linked revenue by service, platform and residence; confidential company tax data",
  "Digital services taxes", "DST revenue / profit-margin simulations", "Gross revenue arithmetic with declared base and pass-through assumptions; 2023 RIS estimates are historical", "Current eligible base, avoidance, incidence, trade responses, compliance costs and interaction with corporation tax",
  "Tax streamlining", "Tax expenditure provision counts and estimated cost coverage", "Disclosure coverage measures; counts do not measure compliance complexity or make provisions undesirable", "Compliance hours by tax and firm size, IRD administrative cost by function, distribution and behavioural responses",
  "Tax streamlining", "R&D expenditure and RDTI expenses", "Survey R&D and appropriated credits have different time bases; neither is additional R&D caused by policy", "Evaluation counterfactual and spillovers; firm-level eligible expenditure and claim cohorts",
  "Tax streamlining", "Screen and games support", "Screen and game rebates are spending programmes, not all tax credits. Announced allocations are distinct from fiscal expenses", "Incremental productions, displacement, leakage, opportunity cost, cultural benefits and net fiscal return",
  "Inflation taxation", "Fiscal drag on constant real income", "Illustrative individuals; annual-average CPI with June schedule snapshots; top-rate change is separately noted", "Taxpayer microdata, credits, ACC, distribution and an aggregate revenue model",
  "Inflation taxation", "Tax on nominal interest", "Fixed 5% illustrative return and observed inflation; no real-return deduction or negative-tax refund assumed", "Actual taxable savings balances and rates, borrowing deductions, financial arrangements and loss treatment",
  "Public servants", "Public Service FTE / wider public-sector headcount", "Different coverage and units; contractors excluded from FTE; wide sector includes health, education, SOEs and local government", "Comparable international general-government employment definitions and labour-force denominators",
  "Public servants", "OIA timeliness, workload, response times", "One service output dimension; quality and case mix omitted; legal timeliness includes permitted extensions", "Agency service outputs and outcomes, unit costs, quality, case mix, capital and contractor substitution",
  "Benefits", "Jobseeker work-ready vs health condition/disability", "Working-age recipients; some work while receiving benefits. Jobseeker is not the HLFS unemployed population", "Actual expenditure by subgroup, earnings and household structure",
  "Benefits", "Gross cut scenarios", "Fiscal scale illustration; work-ready allocation depends on assumed relative payment; not a policy-specific net saving", "Precisely specified cuts, taxable-transfer feedback, replacement assistance, administration and behavioural response"
)
write_data(measurement_notes, "measurement_notes")
write_data(bind_rows(manifest), "source_manifest")
write_data(tibble(check = c("Benefit expense reconciliation", "June 2025 Public Service FTE anchor", "MSD subgroup rounding tolerance", "52 distinct tax provisions", "Expensing lifetime deductions"),
                  result = "PASS"), "validation")
message("Topic data saved to ", out_dir)

# Election evidence extension. Public observations and explicit counterfactuals.
# Run with Rscript; --cached reuses pinned downloads. No packages are installed.
required <- c("dplyr", "tidyr", "purrr", "readr", "readxl", "httr2", "jsonlite", "rvest")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(purrr); library(readr)})
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[1]))) else normalizePath("Data pulls")
raw_dir <- file.path(root, "raw", "election_2026")
out_dir <- file.path(root, "processed", "election_2026")
old_dir <- file.path(root, "processed", "op_ed_topics")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
refresh <- !"--cached" %in% commandArgs(TRUE) && tolower(Sys.getenv("NZ_DATA_REFRESH", "true")) != "false"
manifest <- list(); checks <- list()
num <- function(x) suppressWarnings(as.numeric(gsub(",", "", trimws(as.character(x)))))
write_data <- function(x, name) write_csv(x, file.path(out_dir, paste0(name, ".csv")), na = "")
check <- function(name, ok, detail = "") {
  if (!isTRUE(ok)) stop("Validation failed: ", name, " ", detail)
  checks[[length(checks) + 1L]] <<- tibble(check = name, passed = TRUE, detail = detail)
}
fetch <- function(id, url, filename, description) {
  dest <- file.path(raw_dir, filename)
  cached <- !refresh && file.exists(dest) && file.info(dest)$size > 20
  if (!cached) {
    message("Pulling ", id)
    tmp <- tempfile(tmpdir = raw_dir); on.exit(unlink(tmp), add = TRUE)
    request <- httr2::request(url) |> httr2::req_user_agent("NZ economic evidence research; R httr2") |>
      httr2::req_timeout(90) |> httr2::req_retry(max_tries = 3)
    response <- tryCatch(httr2::req_perform(request, path = tmp), error = function(e) {
      if (!grepl("SSL|schannel|TLS", conditionMessage(e), ignore.case = TRUE)) stop(e)
      message("Using TLS-verified Python fallback for ", id)
      python <- Sys.getenv("NZ_OPED_PYTHON", Sys.which("python"))
      if (!nzchar(python)) stop("Set NZ_OPED_PYTHON to a Python executable for the TLS fallback.")
      result <- system2(python, vapply(c(file.path(root,"helpers","election_download.py"),url,tmp,filename),shQuote,character(1)), stdout=TRUE)
      if (!is.null(attr(result,"status")) && attr(result,"status") != 0L) stop("Fallback download failed: ",id)
      NULL
    })
    if (file.info(tmp)$size < 20) stop("Empty download: ", id)
    ct <- if (is.null(response)) NULL else httr2::resp_header(response, "content-type")
    if (!grepl("html$", filename) && !is.null(ct) && grepl("text/html", ct)) stop("HTML instead of data: ", id)
    if (!file.copy(tmp, dest, overwrite = TRUE)) stop("Cannot save ", dest)
  }
  manifest[[length(manifest) + 1L]] <<- tibble(source_id = id, description = description, url = url,
    snapshot_cutoff = "2026-11-02", downloaded_utc = format(file.info(dest)$mtime, tz = "UTC", usetz = TRUE),
    cached_this_run = cached, local_file = filename, md5 = unname(tools::md5sum(dest)))
  dest
}
stats <- "https://www.stats.govt.nz/assets/Uploads/"
hlpi_base <- paste0(stats, "Household-living-costs-price-indexes/Household-living-costs-price-indexes-September-2026-quarter/Download-data/household-living-costs-price-indexes-september-2026-quarter-")
files <- list(
  pref = fetch("prefu", "https://www.treasury.govt.nz/sites/default/files/2026-09/prefu26-charts-data.xlsx", "prefu.xlsx", "Treasury PREFU September 2026 charts and time series"),
  exp = fetch("prefu_exp", "https://www.treasury.govt.nz/sites/default/files/2026-09/prefu26-data-expense-tables.xlsx", "prefu_expenses.xlsx", "Treasury PREFU 2026 expense and recipient tables"),
  hlpi = fetch("hlpi", paste0(hlpi_base, "time-series-indexes.csv"), "hlpi_september.csv", "Stats NZ September 2026 household living-cost indexes, 2008-2026"),
  facts = fetch("hlpi_facts", paste0(hlpi_base, "group-facts.csv"), "hlpi_september_facts.csv", "Stats NZ HLPI household profiles; historical survey benchmark years"),
  cpi = fetch("cpi", paste0(stats, "Consumers-price-index/Consumers-price-index-September-2026-quarter/Download-data/consumers-price-index-september-2026-quarter-index-numbers.csv"), "cpi_indices.csv", "Stats NZ CPI index histories, September 2026 vintage"),
  labour = fetch("labour", paste0(stats, "Labour-market-statistics/Labour-market-statistics-June-2026-quarter/Download-data/labour-market-statistics-june-2026-quarter.zip"), "labour.zip", "Stats NZ June 2026 HLFS, QES and LCI CSV release"),
  health = fetch("health", "https://minhealthnz.shinyapps.io/nz-health-survey-2024-25-annual-data-explorer/data/nz-health-survey-2024-25-prevalences.csv", "health_prevalences.csv", "Ministry of Health NZ Health Survey 2024/25 prevalences, confidence intervals and flags"),
  life = fetch("life", paste0(stats, "National-and-subnational-period-life-tables/National-and-subnational-period-life-tables-2022-2024/Download-data/new-zealand-period-life-tables-2022-2024.csv"), "life.csv", "Stats NZ complete 2022-2024 period life tables, including 105+ open interval"),
  kiwi = fetch("kiwi", "https://www.ird.govt.nz/-/media/project/ir/home/documents/about-us/tax-statistics---current/kiwisaver-statistics/annual-kiwisaver-statistics/annual-kiwisaver-stats.xlsx?modified=20260929221522", "kiwisaver.xlsx", "IRD annual KiwiSaver tables through June 2026; live workbook URL pinned by query"),
  nzsf = fetch("nzsf", "https://www.nzsuperfund.nz/performance/investment-performance/", "nzsf.html", "NZ Super Fund official cumulative return indices; provisional, before NZ tax, after costs"),
  cgt = fetch("labour_cgt", "https://www.labour.org.nz/capitalgainstax", "labour_cgt.html", "Labour proposed property CGT; party policy, not enacted law"),
  medicard = fetch("labour_medicard", "https://www.labour.org.nz/medicard", "labour_medicard.html", "Labour Medicard and capacity proposals; party estimates"),
  sme = fetch("nzfirst_sme", "https://www.nzfirst.nz/nzfirst_to_campaign_on_20_tax_rate_for_smes", "nzfirst_sme.html", "NZ First proposed 20% company rate below $30m turnover; party policy"),
  kiwi_rules = fetch("kiwi_rules", "https://www.ird.govt.nz/kiwisaver-changes", "ird_changes.html", "IRD current KiwiSaver defaults, ESCT caveat, government contribution changes"),
  compulsory = fetch("national_kiwi", "https://www.national.org.nz/news/260621-national-to-further-boost-kiwis-financia", "national_kiwi.html", "National proposed compulsory KiwiSaver; party policy, not enacted law")
)
files$tax_rates <- fetch("tax_rates", "https://www.ird.govt.nz/income-tax/income-tax-for-individuals/tax-codes-and-tax-rates-for-individuals/tax-rates-for-individuals", "tax_rates.html", "IRD individual income-tax brackets from April 2025; interest included in taxable income")
files$boost <- fetch("investment_boost", "https://www.ird.govt.nz/investment-boost", "investment_boost.html", "IRD Investment Boost: 20% deduction and depreciation on remaining 80%")
files$cgt_design <- fetch("cgt_design", "https://www.interest.co.nz/sites/default/files/2025-10/Labour%20CGT-oct25.pdf", "labour_cgt_design_2025.pdf", "Labour original detailed CGT proposal, October 2025: no inflation indexation; archival party design")
ex <- function(path, sheet) readxl::read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text", range = readxl::cell_limits(c(1, 1), c(NA, NA)), .name_repair = "minimal")
wide <- function(path, sheet, label_col, year_row, status_row, cols, rows, block) {
  x <- ex(path, sheet)
  map_dfr(rows, function(i) tibble(measure = trimws(x[[label_col]][i]), year = num(unlist(x[year_row, cols])),
    status = trimws(as.character(unlist(x[status_row, cols]))), value = num(unlist(x[i, cols])),
    source_sheet = sheet, source_row = i, block = block)) |> filter(!is.na(year), !is.na(value), !is.na(measure))
}
fiscal <- bind_rows(
  wide(files$pref, "Fiscal Time Series", 3, 4, 5, 4:18, 8:28, "NZD millions"),
  wide(files$pref, "Fiscal Time Series", 3, 4, 5, 4:18, 34:54, "Share GDP"),
  wide(files$pref, "Fiscal Time Series", 2, 4, 5, 4:18, 30, "NZD millions"))
econ <- wide(files$pref, "Economic Time Series", 2, 4, 5, 3:17, 6:44, "Source units; see workbook")
write_data(fiscal, "prefu_fiscal"); write_data(econ, "prefu_economic")
gdp <- fiscal |> filter(grepl("Nominal expenditure GDP", measure)) |> transmute(year, gdp_millions = value)
check("PREFU nominal GDP one value per year", nrow(gdp) == 15 && !anyDuplicated(gdp$year))
expenses <- wide(files$exp, "Core Crown Expense Tables", 4, 4, 5, 6:15, 7:23, "Functional expense")
benefits <- wide(files$exp, "Core Crown Expense Tables", 4, 51, 52, 6:15, 54:75, "Benefit expense")
bt <- wide(files$exp, "Core Crown Expense Tables", 4, 51, 52, 6:15, 77, "Benefit total")
reconcile <- benefits |> group_by(year) |> summarise(total = sum(value), .groups = "drop") |> left_join(bt |> select(year, published = value), by = "year")
check("PREFU benefits reconcile", all(abs(reconcile$total - reconcile$published) <= 2))
bn <- wide(files$exp, "Core Crown Expense Tables", 4, 85, 86, 6:15, 88:92, "Recipients thousands")
write_data(expenses |> left_join(gdp, by = "year"), "prefu_expenses")
write_data(benefits |> left_join(gdp, by = "year"), "prefu_benefits")
write_data(bn, "prefu_recipients")
# Retain the published source's actual/unaudited/forecast status throughout.
check("PREFU OBEGALx anchor", fiscal |> filter(block == "NZD millions", measure == "OBEGAL (excluding ACC)", year == 2029) |> dplyr::pull(value) |> identical(4016))

# Household costs, actual prices and labour measures.
hlpi <- read_csv(files$hlpi, show_col_types = FALSE) |> mutate(year = as.integer(substr(quarter, 1, 4)),
  q = as.integer(substr(quarter, 6, 6)), date = as.Date(sprintf("%d-%02d-01", year, q * 3)))
check("Latest HLPI quarter", max(hlpi$quarter) == "2026Q3")
write_data(hlpi, "hlpi"); write_data(read_csv(files$facts, show_col_types = FALSE), "household_profiles")
cpi <- read_csv(files$cpi, show_col_types = FALSE) |> filter(Series_reference == "CPIQ.SE9A", Data_value > 0) |>
  transmute(period = Period, year = floor(Period), month = round(100 * (Period - floor(Period))),
    date = as.Date(sprintf("%d-%02d-01", year, month)), cpi = Data_value)
check("CPI historical coverage", min(cpi$year) <= 2010 && max(cpi$date) == as.Date("2026-09-01") && !anyDuplicated(cpi$date))
write_data(cpi, "cpi")
zip_files <- utils::unzip(files$labour, list = TRUE)$Name
read_zip <- function(ending) {
  member <- zip_files[endsWith(zip_files, ending)]
  check(paste("ZIP member", ending), length(member) == 1L)
  tmp <- tempfile(); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(files$labour, files = member, exdir = tmp)
  read_csv(file.path(tmp, member), show_col_types = FALSE, col_types = cols(.default = col_character()))
}
lms <- read_zip("lms-jun26qtr-tables.csv")
labour <- lms |> filter(Group == "Labour Force Status by Sex: Seasonally Adjusted", Series_title_2 == "Total Both Sexes") |>
  transmute(series = Series_reference, period = num(Period), value = num(Data_value), units = UNITS,
    magnitude = num(MAGNTUDE), measure = Series_title_1, status = STATUS)
qes <- read_zip("qes-jun26qtr-csv.csv") |> filter(Series_reference %in% c("QEMQ.SAAZ9A", "QEMQ.SCAZ9AS", "QEMQ.SDAZ9AS")) |>
  transmute(series = Series_reference, period = num(Period), value = num(Data_value), units = UNITS,
    magnitude = num(MAGNTUDE), measure = paste(Series_title_1, Series_title_3), status = STATUS)
lci <- read_zip("lci-jun26qtr-csv.csv") |> filter(Series_reference == "LCIQ.SD53Z9") |>
  transmute(series = Series_reference, period = num(Period), value = num(Data_value), units = UNITS,
    magnitude = num(MAGNTUDE), measure = paste(Series_title_1, Series_title_2), status = STATUS)
labour <- bind_rows(labour, qes, lci) |> mutate(year = floor(period), month = round(100 * (period - floor(period))),
  date = as.Date(sprintf("%d-%02d-01", year, month))) |> arrange(series, date)
check("June unemployment anchor", labour |> filter(series == "HLFQ.S1F3S", date == as.Date("2026-06-01")) |> dplyr::pull(value) |> identical(5.6))
write_data(labour, "labour")

# Entire health CSV retained; suppressions and precision flags remain explicit.
health <- read_csv(files$health, show_col_types = FALSE) |> rename(indicator = short.description,
  estimate = total, low = total.low.CI, high = total.high.CI, flag = flag_for_publishing) |>
  mutate(survey_period = sprintf("%d/%02d", year, (year + 1) %% 100),
         estimate = if_else(!is.na(flag) & flag == "S", NA_real_, estimate))
check("Health survey year means starting year", max(health$year) == 2024)
check("Health cost barrier anchor", health |> filter(population == "adults", group == "Total", year == 2024, indicator == "Unmet need for GP due to cost") |> pull(estimate) |> identical(14.9))
write_data(health, "health_survey")

# KiwiSaver administrative data. Date headers are Excel serial dates read as text.
kiwi_wide <- function(sheet, rows, first_year, last_year, unit) {
  x <- ex(files$kiwi, sheet); cols <- 2:(last_year - first_year + 2)
  header <- num(unlist(x[3, cols])); header_year <- as.integer(format(as.Date(header, origin = "1899-12-30"), "%Y"))
  check(paste("KiwiSaver dates", sheet), identical(header_year, first_year:last_year))
  map_dfr(rows, function(i) tibble(measure = trimws(x[[1]][i]), year = header_year,
    value = num(unlist(x[i, cols])), unit = unit, source_sheet = sheet, source_row = i)) |> filter(!is.na(value))
}
kiwi <- bind_rows(kiwi_wide("Payments to providers 1", 4:13, 2008, 2026, "NZD millions"),
  kiwi_wide("Payments to providers 2", 4:10, 2020, 2026, "Members"),
  kiwi_wide("Amount withdrawn 1", 4:6, 2012, 2026, "NZD"),
  kiwi_wide("Number of withdrawals 1", 4:5, 2012, 2026, "Withdrawals"))
x <- ex(files$kiwi, "Payments to Providers 3")
participation <- tibble(year = num(x[[1]][4:9]), members_any_time = num(x[[2]][4:9]),
  contributed = num(x[[3]][4:9]), no_contribution = num(x[[5]][4:9]), published_share = num(x[[4]][4:9]))
check("KiwiSaver participation reconciles", all(participation$contributed + participation$no_contribution == participation$members_any_time))
write_data(kiwi, "kiwisaver"); write_data(participation, "kiwisaver_participation")
# Preserve all workbook explanatory text for source-definition review.
notes <- map_dfr(readxl::excel_sheets(files$kiwi), function(sh) {
  z <- ex(files$kiwi, sh)
  tibble(sheet = sh, row = seq_len(nrow(z)), text = apply(z, 1, function(a) paste(na.omit(a), collapse = " | ")))
})
write_data(notes, "kiwisaver_workbook_notes")
life <- read_csv(files$life, show_col_types = FALSE, col_types = cols(age = col_character())) |>
  mutate(age = as.integer(sub("+", "", age, fixed = TRUE)), open_interval = age == 105)
check("Life table open interval", max(life$age) == 105 && all(c("lx", "Lx", "ex") %in% life$measure))
write_data(life, "period_life_tables")
lt <- life |> filter(quantile == "50%", ethnic == "NZ total") |> select(sex, age, measure, value) |>
  pivot_wider(names_from = measure, values_from = value) |> group_by(sex) |>
  mutate(survival_from_65 = lx / lx[age == 65]) |> ungroup()
check("Survival is monotone", all(vapply(split(lt, lt$sex), function(z) all(diff(z$lx[order(z$age)]) <= 0), logical(1))))
write_data(lt, "life_table_nz")
# Extract machine-readable official Chart.js configuration, not values from an image.
html <- paste(readLines(files$nzsf, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
m <- regexpr("var chartData_[0-9]+ = \\{[^\n]+\\};", html, perl = TRUE)
check("NZSF chart JSON present", m[1] > 0)
js <- regmatches(html, m); js <- sub("^var chartData_[0-9]+ = ", "", js); js <- sub(";$", "", js)
config <- jsonlite::fromJSON(js, simplifyVector = FALSE)$data
nzsf <- map_dfr(config$datasets, function(s) {
  if (!nzchar(s$label) || length(s$data) != length(config$labels)) return(NULL)
  tibble(date = as.Date(unlist(config$labels), "%d/%m/%Y"), series = s$label, return_index = num(unlist(s$data)))
}) |> filter(date <= as.Date("2026-11-02"))
check("NZSF index valid", nrow(nzsf) > 900 && all(nzsf$return_index > 0))
write_data(nzsf, "nzsf_return_indices")

# Model assumptions are exported for audit; edit this code and matching captions
# together when changing scenarios. The exported CSV is not a configuration file.
parameters <- tibble::tribble(~parameter, ~value, ~unit, ~basis,
  "cgt_rate", .28, "fraction", "Labour proposed rate; not enacted",
  "interest_nominal_rate", .05, "fraction/year", "Assumption; not a deposit-rate observation",
  "saving_tax_rate", .33, "fraction", "Illustrative marginal income-tax rate",
  "saving_balance", 100000, "NZD", "Illustrative starting balance",
  "retirement_age", 65, "years", "Scenario; current NZ Super age context",
  "start_age", 25, "years", "Scenario; 40 year accumulation",
  "real_salary", 70000, "NZD/year", "Constant real gross earnings assumption",
  "real_annuity", 30000, "NZD/year", "Illustrative CPI-level pension, NOT a statutory NZ Super rate",
  "accumulation_real_return", .03, "fraction/year", "Net of tax and fees; scenario, not forecast",
  "annuity_real_discount", .02, "fraction/year", "Liability discount assumption, not expected risky asset return",
  "annuity_expense_loading", .05, "fraction of actuarial cost", "Sensitivity assumption, not market quote",
  "employer_esct", .30, "fraction", "Illustrative tax on employer contribution; not all workers' rate",
  "business_asset_cost", 100000, "NZD", "Scenario",
  "business_depreciation", .20, "diminishing value fraction/year", "Assumed eligible asset; not universal statutory depreciation",
  "business_discount", .05, "fraction/year", "Scenario")
write_data(parameters, "parameters")
p <- setNames(parameters$value, parameters$parameter)
tax <- function(income, thresholds = c(15600, 53500, 78100, 180000)) {
  rate <- c(.105, .175, .30, .33, .39); lo <- c(0, thresholds); hi <- c(thresholds, Inf)
  vapply(income, function(y) sum(pmax(0, pmin(y, hi) - lo) * rate), numeric(1))
}
check("Income tax anchor", abs(tax(70000) - 13220.5) < .01)
# Constant-real-income comparison: both arms start with exactly the same tax scale.
drag <- crossing(real_income = c(40000, 70000, 100000, 150000), inflation = c(.01, .02, .03, .04), years = 0:20) |>
  mutate(nominal_income = real_income * (1 + inflation)^years, tax_fixed = tax(nominal_income),
    tax_indexed = tax(real_income) * (1 + inflation)^years,
    extra_real_tax = (tax_fixed - tax_indexed) / (1 + inflation)^years,
    real_net_fixed = (nominal_income - tax_fixed) / (1 + inflation)^years,
    real_net_indexed = real_income - tax(real_income))
write_data(drag, "inflation_bracket_scenarios")
cgt <- crossing(inflation = c(.01, .02, .03, .04), real_growth = c(0, .01, .02, .04), years = 1:40) |>
  mutate(initial = 500000, price_factor = (1 + inflation)^years,
    sale = initial * ((1 + inflation) * (1 + real_growth))^years,
    nominal_gain = sale - initial, real_gain_nominal_dollars = sale - initial * price_factor,
    nominal_tax = p[["cgt_rate"]] * pmax(nominal_gain, 0),
    indexed_tax = p[["cgt_rate"]] * pmax(real_gain_nominal_dollars, 0),
    tax_on_inflation_real = (nominal_tax - indexed_tax) / price_factor,
    effective_tax_real_gain = if_else(real_gain_nominal_dollars > .01, nominal_tax / real_gain_nominal_dollars, NA_real_))
write_data(cgt, "cgt_scenarios")
interest <- crossing(inflation = seq(0, .08, .001), tax_rate = c(.105, .175, .28, .33, .39)) |>
  mutate(nominal_interest = p[["interest_nominal_rate"]], real_before = (1 + nominal_interest)/(1 + inflation) - 1,
    real_after_nominal_tax = (1 + nominal_interest * (1 - tax_rate))/(1 + inflation) - 1,
    real_after_indexed_tax = (1 + nominal_interest - tax_rate * pmax(nominal_interest - inflation, 0))/(1 + inflation) - 1)
write_data(interest, "interest_indexation")
wealth <- crossing(inflation = c(.01, .02, .03, .04), years = 0:40) |>
  mutate(nominal_interest = p[["interest_nominal_rate"]], starting_balance = p[["saving_balance"]],
    nominal_tax_real_balance = starting_balance * ((1 + nominal_interest * (1 - p[["saving_tax_rate"]]))/(1 + inflation))^years,
    indexed_tax_real_balance = starting_balance * ((1 + nominal_interest - p[["saving_tax_rate"]] * pmax(nominal_interest - inflation, 0))/(1 + inflation))^years)
write_data(wealth, "saving_wealth_scenarios")

# Retirement: CPI-level, single-life, no refund or survivor benefit. NOT a quote.
# Lx/l65 approximates expected pension-years; payments centred in each year.
# 105+ Lx retained, discounted at its approximate mean payment time.
annuity_factor <- function(discount, real_payment_growth = 0, sex = "Equal-weight pool") {
  z <- lt |> filter(age >= 65)
  factors <- z |> group_by(sex) |> summarise(factor = sum(Lx / lx[age == 65] *
    ((1 + real_payment_growth)/(1 + discount))^(age - 65 + if_else(age == 105, ex / 2, .5))), .groups = "drop")
  if (sex == "Equal-weight pool") mean(factors$factor) else factors$factor[factors$sex == sex]
}
annuity <- crossing(discount = seq(0, .05, .0025), sex = c("Male", "Female", "Equal-weight pool"), real_payment_growth = c(0, .01)) |>
  mutate(factor = pmap_dbl(list(discount, real_payment_growth, sex), annuity_factor),
    capital_needed = p[["real_annuity"]] * factor,
    loaded_capital = capital_needed * (1 + p[["annuity_expense_loading"]]))
check("Annuity capital decreases with discount rate", all(vapply(split(annuity, interaction(annuity$sex, annuity$real_payment_growth)), function(z) all(diff(z$capital_needed[order(z$discount)]) < 0), logical(1))))
check("Annuity factor at zero matches life expectancy", abs(annuity_factor(0, 0, "Female") - lt$ex[lt$age == 65 & lt$sex == "Female"]) < .05)
write_data(annuity, "annuity_capital")
factor <- annuity_factor(p[["annuity_real_discount"]]) * (1 + p[["annuity_expense_loading"]])
accum <- crossing(net_contribution = c(.04, .06, .08, .12), real_return = c(.01, .03, .05), years = 0:40) |>
  mutate(age = p[["start_age"]] + years, annual_deposit = p[["real_salary"]] * net_contribution,
    balance = annual_deposit * if_else(real_return == 0, years, ((1 + real_return)^years - 1)/real_return),
    implied_real_annuity = balance / factor)
write_data(accum, "retirement_accumulation")
careers <- crossing(career = c("Continuous", "Five-year break at 30", "Five-year break at 50", "Half-time throughout"), age = 25:64) |>
  mutate(earnings_fraction = case_when(career == "Half-time throughout" ~ .5,
    career == "Five-year break at 30" & age %in% 30:34 ~ 0,
    career == "Five-year break at 50" & age %in% 50:54 ~ 0, TRUE ~ 1),
    contribution = p[["real_salary"]] * .08 * earnings_fraction,
    retirement_value = contribution * (1 + p[["accumulation_real_return"]])^(64 - age)) |>
  group_by(career) |> summarise(balance = sum(retirement_value), implied_real_annuity = balance/factor, .groups = "drop")
write_data(careers, "retirement_career_gaps")
fees <- crossing(fee = seq(0, .015, .0005), years = 0:40) |>
  mutate(gross_real_after_tax = .04, net_real_return = (1 + gross_real_after_tax) * (1 - fee) - 1,
    balance = p[["real_salary"]] * .08 * ((1 + net_real_return)^years - 1)/net_real_return,
    implied_real_annuity = balance/factor)
write_data(fees, "retirement_fees")
drawdown <- crossing(horizon = c(20, 25, 30, 35, 40), discount = c(0, .01, .02, .03, .04)) |>
  mutate(self_insured_capital = p[["real_annuity"]] * vapply(seq_len(n()), function(i) sum((1 + discount[i])^-(seq_len(horizon[i]) - .5)), numeric(1)),
    pooled_capital = p[["real_annuity"]] * map_dbl(discount, annuity_factor),
    outlive_probability = map_dbl(horizon, function(h) mean(lt$survival_from_65[lt$age == 65 + h])))
write_data(drawdown, "retirement_longevity")
required_contrib <- crossing(start_age = 20:55, real_return = c(.01, .03, .05), target_real_pension = c(15000, 30000)) |>
  mutate(years = 65 - start_age, annuity_factor = factor,
    annual_deposit = target_real_pension * factor / (((1 + real_return)^years - 1)/real_return),
    net_contribution_rate = annual_deposit / p[["real_salary"]])
write_data(required_contrib, "retirement_required_contributions")
# Same deposit pool, alternative pension allocation rules: redistribution is explicit.
distribution <- crossing(salary = seq(20000, 150000, 5000), net_contribution = .08) |>
  mutate(balance = salary * net_contribution * ((1.03^40 - 1)/.03),
    own_account_annuity = balance/factor,
    common_pension = mean(own_account_annuity))
write_data(distribution, "retirement_distribution")
transition <- crossing(year = 0:50, extra_prefunding_percent_gdp = c(1, 2, 3), real_return = c(.01, .03, .05)) |>
  mutate(real_gdp_growth = .02, relative_return = (1 + real_return)/(1 + real_gdp_growth) - 1,
    assets_percent_gdp = map2_dbl(year, relative_return, function(n, r) if(n == 0) 0 else sum((1+r)^(0:(n-1)))) * extra_prefunding_percent_gdp)
write_data(transition, "prefunding_transition")
national_saving <- crossing(displaced_other_saving = seq(0, 1, .05), public_cost_share = c(0, .10, .20)) |>
  mutate(additional_account_deposits_billion = 5,
    net_national_saving_billion = additional_account_deposits_billion * (1 - displaced_other_saving - public_cost_share))
write_data(national_saving, "national_saving_sensitivity")
# Contributions: headline employee+employer rates differ from invested deposits.
pay <- crossing(employee_rate = c(.035, .04, .06), remuneration = c("Gross salary held at $70,000", "Employer cost held at $72,450")) |>
  mutate(employer_rate = employee_rate, salary = if_else(grepl("Gross salary", remuneration), 70000, 72450/(1+employer_rate)),
    employer_cost = salary * (1 + employer_rate), employee_deposit = salary * employee_rate,
    employer_deposit_after_esct = salary * employer_rate * (1 - p[["employer_esct"]]),
    net_deposit = employee_deposit + employer_deposit_after_esct,
    take_home_before_acc = salary - tax(salary) - employee_deposit)
write_data(pay, "retirement_pay_incidence")

# Business comparison: distinguish deduction timing, permanent rate and losses.
business <- crossing(profit_before_investment = seq(0, 250000, 5000), method = c("28% + standard depreciation", "20% + standard depreciation", "28% + Investment Boost", "28% + full expensing")) |>
  mutate(cost = p[["business_asset_cost"]], rate = if_else(grepl("^20%", method), .20, .28),
    deduction = case_when(grepl("full expensing", method) ~ cost,
      grepl("Investment Boost", method) ~ cost * (.2 + .8 * p[["business_depreciation"]]),
      TRUE ~ cost * p[["business_depreciation"]]),
    tax_due = pmax(profit_before_investment - deduction, 0) * rate,
    unused_loss = pmax(deduction - profit_before_investment, 0))
write_data(business, "business_first_year")
deductions <- crossing(year = 0:100, method = c("28% + standard depreciation", "20% + standard depreciation", "28% + Investment Boost", "28% + full expensing")) |>
  mutate(cost = p[["business_asset_cost"]], rate = if_else(grepl("^20%", method), .20, .28),
    deduction = case_when(grepl("full expensing", method) ~ if_else(year == 0, cost, 0),
      grepl("Investment Boost", method) ~ if_else(year == 0, cost * .2, cost * .8 * .2 * .8^pmax(year-1, 0)),
      TRUE ~ if_else(year == 0, 0, cost * .2 * .8^pmax(year-1, 0))))
check("Deduction totals equal asset cost", all(abs((deductions |> group_by(method) |> summarise(total = sum(deduction)))$total - p[["business_asset_cost"]]) < .01))
pv <- crossing(discount = seq(0, .12, .005), method = unique(deductions$method)) |>
  mutate(pv_tax_shield = map2_dbl(discount, method, function(r, m) {
    z <- deductions |> filter(method == m); sum(z$rate * z$deduction/(1+r)^z$year)
  }), net_capital_cost = p[["business_asset_cost"]] - pv_tax_shield)
write_data(deductions, "business_deductions"); write_data(pv, "business_pv")
# Stylised marginal investment: perpetual pre-tax cash flow, no sale, leverage,
# inflation or loss constraints. Effective marginal wedge follows the cash flows.
emtr <- crossing(real_required_return = seq(.01, .10, .0025), method = unique(deductions$method)) |>
  mutate(rate = if_else(grepl("^20%", method), .20, .28),
    pv_shield_per_dollar = map2_dbl(real_required_return, method, function(r, m) {
      z <- deductions |> filter(method == m); sum(z$rate*z$deduction/(1+r)^z$year)/p[["business_asset_cost"]]
    }), required_pretax_yield = real_required_return * (1 - pv_shield_per_dollar)/(1-rate),
    effective_marginal_wedge = (required_pretax_yield-real_required_return)/required_pretax_yield)
write_data(emtr, "business_marginal_wedge")
check("Full expensing zero marginal wedge in stylised model", max(abs(emtr$effective_marginal_wedge[grepl("full expensing", emtr$method)])) < 1e-10)

# Numeric research notes for writing without charts. Every number retains its
# observed/forecast/scenario status, period, units and source, rather than prose
# that could inadvertently turn a model output into a measured fact.
fact <- function(topic, measure, value, unit, period, basis, caveat, source_id) {
  tibble(topic, measure, value, unit, period, basis, caveat, source_id)
}
health_fact <- function(indicator) health |> filter(population=="adults",group=="Total",year==2024,.data$indicator==.env$indicator) |> dplyr::pull(estimate)
fiscal_fact <- function(m, y) fiscal |> filter(block=="Share GDP",measure==m,year==y) |> dplyr::pull(value) |> (`*`)(100)
ben2026 <- benefits |> filter(year==2026)
js2026 <- ben2026$value[ben2026$measure=="Jobseeker Support and Emergency Benefit"]
core2026 <- fiscal |> filter(block=="NZD millions",measure=="Core Crown expenses",year==2026) |> dplyr::pull(value)
key_findings <- bind_rows(
  fact("Health","Adults with GP cost barrier",health_fact("Unmet need for GP due to cost"),"percent","2024/25","Observed survey estimate","95% CI in health_survey.csv; barriers overlap","health"),
  fact("Health","Adults with GP waiting-time barrier",health_fact("Unmet need for GP due to wait time"),"percent","2024/25","Observed survey estimate","Does not establish which intervention creates appointments","health"),
  fact("Household costs","All-household annual living-cost inflation",hlpi$change.a[hlpi$quarter=="2026Q3" & hlpi$hlpi=="allhh" & hlpi$nzhec=="A"],"percent","September 2026","Observed index change","HLPI includes interest; differs from CPI","hlpi"),
  fact("Household costs","Superannuitant annual living-cost inflation",hlpi$change.a[hlpi$quarter=="2026Q3" & hlpi$hlpi=="super" & hlpi$nzhec=="A"],"percent","September 2026","Observed index change","Group basket, not each retiree's inflation","hlpi"),
  fact("Jobs","Seasonally adjusted unemployment rate",5.6,"percent of labour force","June 2026","Observed HLFS estimate","Different concept from Jobseeker receipt","labour"),
  fact("Fiscal","Core Crown tax revenue / GDP",fiscal_fact("Core Crown tax revenue",2026),"percent","2025/26","Unaudited actual","Published PREFU denominator; revisions noted in original release","prefu"),
  fact("Fiscal","Core Crown tax revenue / GDP",fiscal_fact("Core Crown tax revenue",2031),"percent","2030/31","Official forecast","Not a guarantee or independent election-policy costing","prefu"),
  fact("Benefits","NZ Super / total benefit expense",100*ben2026$value[ben2026$measure=="New Zealand Superannuation"]/sum(ben2026$value),"percent","2025/26","Derived from unaudited actual","Benefit expense denominator excludes administration and other social services","prefu_exp"),
  fact("Benefits","Jobseeker + Emergency / core Crown expenses",100*js2026/core2026,"percent","2025/26","Derived from unaudited actual","Whole programme includes health-related recipients; not work-ready spending","prefu_exp"),
  fact("Inflation","Extra real annual income tax after ten years",drag$extra_real_tax[drag$real_income==70000 & drag$inflation==.02 & drag$years==10],"Starting-year NZD/year","Ten-year scenario","Scenario","Constant $70k real earnings, 2% inflation, frozen versus indexed current thresholds; no credits/ACC/transfers","tax_rates"),
  fact("Inflation","CGT on inflation alone after ten years",cgt$tax_on_inflation_real[cgt$inflation==.02 & cgt$real_growth==0 & cgt$years==10],"Starting-year NZD","Ten-year scenario","Proposed-policy scenario","$500k property, 2% inflation, zero real gain, 28% tax, no costs; begins at valuation/acquisition date","labour_cgt"),
  fact("Retirement","Capital for $30k CPI-level lifetime income",p[["real_annuity"]]*factor,"NZD at age 65","Model retirement at 65","Actuarial scenario","2% liability discount, equal male/female period mortality, 5% loading; no survivor/refund; not a quote","life"),
  fact("Retirement","Net invested contribution to fund $30k target",100*required_contrib$net_contribution_rate[required_contrib$start_age==25 & required_contrib$real_return==.03 & required_contrib$target_real_pension==30000],"percent of assumed gross earnings","Ages 25-64","Scenario","$70k constant real earnings, 3% net return, fixed 2% annuity discount; not a universal pension costing","life"),
  fact("Retirement","Lifetime income after 8% invested for 40 years",accum$implied_real_annuity[accum$net_contribution==.08 & accum$real_return==.03 & accum$years==40],"NZD/year in constant purchasing power","Ages 25-64","Scenario","8% is net deposited, not combined headline employee/employer rate; $70k real earnings, 3% net return","life"),
  fact("Retirement","Share of members with no recorded annual contribution",100*participation$no_contribution[participation$year==2026]/participation$members_any_time[participation$year==2026],"percent","Year ending June 2026","Administrative statistic","Includes children/retirees/overseas members; not non-saving employed adults","kiwi")
)
check("Key findings finite", all(is.finite(key_findings$value)))
write_data(key_findings,"key_findings")

# Snapshot provenance for inputs re-used from the existing, separately dated pack.
reused <- c("benefit_history_groups", "jobseeker_history", "main_benefit_history", "rd_expenditure", "tax_system_spending", "tax_expenditure_register", "world_bank", "treasury_fiscal")
walk(reused, function(n) {
  f <- file.path(old_dir, paste0(n, ".csv")); if (!file.exists(f)) stop("Run 05_run_topic_pack.R first; missing ", n)
  manifest[[length(manifest)+1L]] <<- tibble(source_id = paste0("existing_", n), description = paste("Existing October 2026 evidence snapshot:", n),
    url = "../../processed/op_ed_topics/source_manifest.csv", snapshot_cutoff = "2026-10-14", downloaded_utc = format(file.info(f)$mtime, tz="UTC", usetz=TRUE),
    cached_this_run = TRUE, local_file = paste0("../../processed/op_ed_topics/", n, ".csv"), md5 = unname(tools::md5sum(f)))
})
write_data(bind_rows(manifest), "source_manifest"); write_data(bind_rows(checks), "validation")
message("Election data and scenarios saved in ", out_dir)

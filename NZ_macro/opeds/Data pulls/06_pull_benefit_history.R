# Extend the benefit evidence pack using archived official releases.
# Run directly or through 05_run_topic_pack.R. NZ_DATA_REFRESH=false uses cache.
# Modern backcasts, archival counts, archival expenses and Treasury expenses
# remain separate datasets: their coverage is not interchangeable.
required <- c("dplyr", "httr2", "jsonlite", "purrr", "readr", "readxl", "tidyr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing R packages: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages({library(dplyr); library(purrr); library(readr); library(tidyr)})
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[1]))) else normalizePath("Data pulls")
raw_dir <- file.path(root, "raw", "op_ed_topics")
out_dir <- file.path(root, "processed", "op_ed_topics")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!file.exists(file.path(out_dir, "benefit_expenses.csv"))) stop("Run 03_pull_topic_data.R first.")
refresh <- tolower(Sys.getenv("NZ_DATA_REFRESH", "true")) %in% c("true", "1", "yes")
number <- function(x) suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
excel <- function(path, sheet) readxl::read_excel(path, sheet = sheet, col_names = FALSE,
  range = readxl::cell_limits(c(1, 1), c(NA, NA)), .name_repair = "minimal", col_types = "text")
write_data <- function(x, name) write_csv(x, file.path(out_dir, paste0(name, ".csv")), na = "")
read_data <- function(name) read_csv(file.path(out_dir, paste0(name, ".csv")), show_col_types = FALSE)
manifest <- list()
pull <- function(id, url, filename, description) {
  path <- file.path(raw_dir, filename)
  cached <- !refresh && file.exists(path) && file.info(path)$size > 0
  if (!cached) {
    message("Pulling benefit history: ", id)
    temp <- tempfile(tmpdir = raw_dir)
    on.exit(unlink(temp), add = TRUE)
    response <- httr2::request(url) |> httr2::req_user_agent("NZ op-ed benefit-history research; R httr2") |>
      httr2::req_timeout(120) |> httr2::req_retry(max_tries = 3) |> httr2::req_perform(path = temp)
    type <- httr2::resp_header(response, "content-type")
    if (httr2::resp_status(response) != 200 || file.info(temp)$size < 20 ||
        (!is.null(type) && grepl("text/html", type))) stop("Invalid historical source: ", id)
    if (!file.copy(temp, path, overwrite = TRUE)) stop("Cannot save: ", path)
  }
  manifest[[length(manifest) + 1]] <<- tibble(source_id = id, description = description, url = url,
    kind = "Official historical data", snapshot_cutoff = "2026-10-14", cached_this_run = cached,
    file_saved_utc = format(file.info(path)$mtime, tz = "UTC", usetz = TRUE),
    md5 = unname(tools::md5sum(path)), local_file = filename)
  path
}
sources <- tibble::tribble(
  ~id, ~file, ~url, ~description,
  "msd_national_2013", "msd_national_2013.xlsx", "https://www.msd.govt.nz/documents/about-msd-and-our-work/publications-resources/statistics/benefit/2013/september/quarterly-benefit-fact-sheets-national-benefit-tables1.xlsx", "MSD September 2013 benefit tables, including official pre-reform backcasts to September 2008",
  "msd_national_2018", "msd_national_2018.xlsx", "https://www.msd.govt.nz/documents/about-msd-and-our-work/publications-resources/statistics/benefit/2018/quarterly-benefit-fact-sheets-national-benefit-tables-dec-2018.xlsx", "MSD December 2018 benefit tables; December 2013-December 2018",
  "msd_national_2021", "msd_national_2021.xlsx", "https://www.msd.govt.nz/documents/about-msd-and-our-work/publications-resources/statistics/benefit/2021/quarterly-benefit-fact-sheets-national-benefit-tables-december-2021.xlsx", "MSD December 2021 benefit tables; December 2016-December 2021",
  "msd_nzs_2018", "msd_nzs_2018.xlsx", "https://www.msd.govt.nz/documents/about-msd-and-our-work/publications-resources/statistics/benefit/2018/quarterly-benefit-fact-sheets-nzs-and-vp-tables-dec-2018.xlsx", "MSD December 2018 NZ Superannuation and Veterans Pension counts",
  "msd_nzs_2021", "msd_nzs_2021.xlsx", "https://www.msd.govt.nz/documents/about-msd-and-our-work/publications-resources/statistics/benefit/2021/quarterly-benefit-fact-sheets-nzs-and-vp-tables-december-2021.xlsx", "MSD December 2021 NZ Superannuation and Veterans Pension counts",
  "msd_nzs_2026", "msd_nzs_2026.xlsx", "https://www.msd.govt.nz/documents/about-msd-and-our-work/publications-resources/statistics/benefit/2026/quarterly-benefit-fact-sheets-nzs-and-vp-tables-june-2026.xlsx", "MSD June 2026 NZ Superannuation and Veterans Pension counts",
  "msd_statistical_2012", "msd_statistical_2012.pdf", "https://www.msd.govt.nz/documents/about-msd-and-our-work/publications-resources/statistics/statistical-report-2012.pdf", "MSD Statistical Report 2012, Table OT.3: selected recipient observations 1940-2012",
  "msd_statistical_2002", "msd_statistical_2002.pdf", "https://www.msd.govt.nz/documents/about-msd-and-our-work/publications-resources/statistics/statistical-report-2002.pdf", "MSD Statistical Report 2002, Table 7.2: selected core-benefit/pension expenses 1940-2002, including supplementary assistance",
  "treasury_hyefu_2005", "hyefu_2005_expenses.pdf", "https://www.treasury.govt.nz/sites/default/files/2007-10/hyefu05-6.pdf", "Treasury HYEFU 2005 expense tables: 2001-2005 actuals",
  "treasury_hyefu_2010", "hyefu_2010.pdf", "https://www.treasury.govt.nz/sites/default/files/2010-12/hyefu10.pdf", "Treasury HYEFU 2010 benefit expense table: 2006-2010 actuals",
  "treasury_hyefu_2015", "hyefu_2015.pdf", "https://www.treasury.govt.nz/sites/default/files/2015-12/hyefu15.pdf", "Treasury HYEFU 2015 benefit expense table: 2011-2015 actuals",
  "treasury_hyefu_2020", "hyefu_2020.pdf", "https://www.treasury.govt.nz/sites/default/files/2020-12/hyefu20.pdf", "Treasury HYEFU 2020 benefit expense table: 2016-2020 actuals",
  "treasury_fiscal_history", "treasury_fiscal_history.xlsx", "https://www.treasury.govt.nz/sites/default/files/2026-01/fiscaltimeseries1972-2025-year-end25.xlsx", "Treasury Fiscal Time Series 1972-2025: historical social security/welfare/GSF, fiscal denominators and accounting breaks"
)
files <- setNames(map(seq_len(nrow(sources)), function(i) {
  s <- sources[i, ]; pull(s$id, s$url, s$file, s$description)
}), sources$id)

wb <- imap_dfr(c(population = "SP.POP.TOTL", cpi = "FP.CPI.TOTL"), function(code, name) {
  path <- pull(paste0("wb_history_", name), paste0("https://api.worldbank.org/v2/country/NZL/indicator/", code,
       "?format=json&per_page=20000&date=1960:2025"), paste0("wb_history_", name, ".json"),
       paste("World Bank", code, "New Zealand annual history, 1960-2025"))
  obj <- jsonlite::fromJSON(path, flatten = TRUE)
  stopifnot(as.numeric(obj[[1]]$pages) == 1)
  obj[[2]] |> transmute(year = as.integer(date), measure = name, value) |> filter(!is.na(value), year <= 2025)
})
wb_wide <- wb |> pivot_wider(names_from = measure, values_from = value)
stopifnot(min(wb_wide$year) == 1960, max(wb_wide$year) == 2025)
write_data(wb_wide, "benefit_history_prices_population")

# MSD header layouts change across releases. Infer date columns and find rows
# by labels rather than giving the same positional parser to every workbook.
date_header <- function(x) {
  x <- trimws(as.character(x))
  if (is.na(x) || !nzchar(x)) return(as.Date(NA))
  if (grepl("^[A-Za-z]{3}-[0-9]{2}$", x)) {
    date <- as.Date(sprintf("%04d-%02d-01", 2000 + number(substr(x, 5, 6)), match(substr(x, 1, 3), month.abb)))
  } else if (grepl("^[0-9]{5}(\\.[0-9]+)?$", x) && number(x) > 30000 && number(x) < 50000) {
    date <- as.Date(number(x), origin = "1899-12-30")
  } else if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)) {
    date <- as.Date(substr(x, 1, 10))
  } else return(as.Date(NA))
  # Old headers use the first of the quarter's last month. These are stocks at
  # quarter end, so convert consistently to the last day of that month.
  first <- as.Date(format(date, "%Y-%m-01"))
  as.Date(format(first + 32, "%Y-%m-01")) - 1
}
read_msd_row <- function(path, sheet, label_pattern, vintage, source_id, first_match = FALSE) {
  z <- excel(path, sheet)
  header_rows <- map(seq_len(min(8, nrow(z))), function(i) {
    as.Date(vapply(as.character(unlist(z[i, ], use.names = FALSE)), function(x) as.numeric(date_header(x)), numeric(1)), origin = "1970-01-01")
  })
  # A sheet can contain more than one table. For the 2013 NZS table its own
  # date header is later, while dates/coverage are identical to the first table.
  header <- header_rows[[which.max(map_int(header_rows, ~sum(!is.na(.x))))]]
  columns <- which(!is.na(header))
  stopifnot(length(columns) >= 20)
  labels <- apply(as.matrix(z[, seq_len(min(columns) - 1), drop = FALSE]), 1, function(row) {
    paste(trimws(row[!is.na(row) & nzchar(trimws(row))]), collapse = " ")
  })
  matches <- which(grepl(label_pattern, labels, ignore.case = TRUE, perl = TRUE))
  if (first_match) matches <- head(matches, 1)
  if (length(matches) != 1) stop("Ambiguous MSD row: ", basename(path), " / ", sheet, " / ", label_pattern)
  tibble(date = header[columns], recipients = number(unlist(z[matches, columns], use.names = FALSE)),
         source_id = source_id, source_vintage = vintage, source_sheet = sheet, source_row = matches) |>
    filter(!is.na(recipients))
}

old_sheets <- c(`Jobseeker Support` = "JS 2008-2013", `Sole Parent Support` = "SPS 2008-2013",
                `Supported Living Payment` = "SLP 2008-2013", `Other Main Benefits` = "Other 2008-2013",
                Total = "All main benefits 2008-2013")
counts_old <- imap_dfr(old_sheets, function(sheet, programme) {
  read_msd_row(files$msd_national_2013, sheet, "^Total$", 2013, "msd_national_2013", first_match = TRUE) |>
    mutate(programme = programme)
})
parse_summary <- function(path, vintage, source_id) {
  map_dfr(names(old_sheets), function(programme) {
    read_msd_row(path, "Summary table - last 5 years", paste0("^", programme, "$"), vintage, source_id) |>
      mutate(programme = programme)
  })
}
counts_all <- bind_rows(counts_old,
  parse_summary(files$msd_national_2018, 2018, "msd_national_2018"),
  parse_summary(files$msd_national_2021, 2021, "msd_national_2021"),
  read_data("benefit_recipients") |> mutate(source_id = "msd_benefits", source_vintage = 2026L,
      source_sheet = "Summary table - last 5 years", source_row = NA_integer_))
parse_js <- function(path, sheet, vintage, source_id) {
  map_dfr(c("Work ready", "Health condition or disability"), function(subgroup) {
    read_msd_row(path, sheet, if (subgroup == "Work ready") "Work Ready" else "Health Condition or Disability", vintage, source_id) |>
      mutate(subgroup = subgroup)
  })
}
js_all <- bind_rows(
  parse_js(files$msd_national_2013, "JS 2008-2013", 2013, "msd_national_2013"),
  parse_js(files$msd_national_2018, "JS- last 5 years", 2018, "msd_national_2018"),
  parse_js(files$msd_national_2021, "JS - last 5 years", 2021, "msd_national_2021"),
  read_data("jobseeker_subgroups") |> select(date, recipients, subgroup) |>
    mutate(source_id = "msd_benefits", source_vintage = 2026L, source_sheet = "JS - last 5 years", source_row = NA_integer_))
nzs_all <- bind_rows(
  read_msd_row(files$msd_national_2013, "Other 2008-2013", "^New Zealand Superannuation \\(NZS\\)$", 2013, "msd_national_2013"),
  map_dfr(c(2018, 2021, 2026), function(y) read_msd_row(files[[paste0("msd_nzs_", y)]], "Benefit type - over time",
                 "^New Zealand Superannuation$", y, paste0("msd_nzs_", y)))
)

# Keep overlapping releases for audit and select the newest available release
# for each exact date/category. Never add duplicated overlap quarters.
deduplicate <- function(data, key, name) {
  audit <- data |> group_by(across(all_of(c("date", key)))) |>
    summarise(releases = n(), lowest = min(recipients), highest = max(recipients),
              revision_range = highest - lowest, .groups = "drop") |> filter(releases > 1)
  write_data(audit, paste0(name, "_overlap_audit"))
  data |> arrange(desc(source_vintage)) |> distinct(across(all_of(c("date", key))), .keep_all = TRUE) |>
    mutate(estimate_basis = if_else(date < as.Date("2013-07-15"), "MSD pre-reform backcast", "Observed current-category count")) |>
    arrange(date)
}
counts <- deduplicate(counts_all, "programme", "main_benefit_history")
js <- deduplicate(js_all, "subgroup", "jobseeker_history")
nzs <- deduplicate(nzs_all |> mutate(programme = "NZ Superannuation"), "programme", "nzs_history") |>
  mutate(estimate_basis = "Published all-age NZ Superannuation count")
expected <- seq(as.Date("2008-09-01"), as.Date("2026-06-01"), by = "3 months")
expected <- as.Date(format(expected + 32, "%Y-%m-01")) - 1
stopifnot(n_distinct(counts$date) == 72, n_distinct(js$date) == 72, n_distinct(nzs$date) == 72,
          all(sort(unique(counts$date)) == expected), nrow(counts) == 72 * 5, nrow(js) == 72 * 2)
reconcile <- counts |> group_by(date) |>
  summarise(total = recipients[programme == "Total"], components = sum(recipients[programme != "Total"]), .groups = "drop")
stopifnot(all(abs(reconcile$total - reconcile$components) <= 12))
js_check <- js |> group_by(date) |> summarise(components = sum(recipients), .groups = "drop") |>
  left_join(counts |> filter(programme == "Jobseeker Support") |> select(date, total = recipients), by = "date")
stopifnot(all(abs(js_check$total - js_check$components) <= 9))
write_data(counts, "main_benefit_history")
write_data(js, "jobseeker_history")
write_data(nzs, "nzs_history")
write_data(counts_all, "main_benefit_history_all_vintages")

python <- Sys.getenv("NZ_OPED_PYTHON", Sys.which("python"))
if (!nzchar(python)) stop("Python with PyMuPDF is required; set NZ_OPED_PYTHON.")
status <- system2(python, vapply(c(file.path(root, "helpers", "benefit_history_pdf_extract.py"), raw_dir, out_dir), shQuote, character(1)))
if (status != 0) stop("Historical PDF extraction failed.")
historic <- read_data("treasury_benefit_history_extracted")
is_total <- grepl("^(Welfare )?Benefit Expenses$", historic$measure, ignore.case = TRUE)
old_total <- historic |> filter(is_total) |> select(year, value_millions, source_id, source_vintage, pdf_page)
old_components <- historic |> filter(!is_total)
checks <- old_components |> group_by(year) |> summarise(components = sum(value_millions, na.rm = TRUE), .groups = "drop") |>
  left_join(old_total |> select(year, total = value_millions), by = "year")
stopifnot(nrow(checks) == 20, all(abs(checks$components - checks$total) < .01))
new_components <- read_data("benefit_expenses") |> filter(status == "Actual") |>
  transmute(year, measure, value_millions = value, original_cell = as.character(value), status,
            source_id = "treasury_expenses", source_vintage = 2026L, pdf_page = NA_integer_)
components <- bind_rows(old_components, new_components) |>
  mutate(key = tolower(gsub("[^a-z]", "", tolower(measure))),
         group = case_when(
           key == "newzealandsuperannuation" ~ "NZ Superannuation",
           key %in% c("domesticpurposesbenefit", "unemploymentbenefit", "communitywage", "invalidsbenefit", "sicknessbenefit",
                      "jobseekersupportandemergencybenefit", "supportedlivingpayment", "soleparentsupport") ~ "Main income-replacement basket",
           key %in% c("familysupport", "childtaxcredit", "inworkpayment", "familytaxcredit", "inworktaxcredit",
                      "otherworkingforfamiliestaxcredits", "beststart", "familyboosttaxcredit") ~ "Family tax credits and payments",
           key %in% c("accommodationsupplement", "accommodationassistance", "incomerelatedrents") ~ "Housing assistance",
           key %in% c("wagesubsidyscheme", "covidleavesupport", "costoflivingpayment") ~ "Temporary COVID / cost-of-living",
           TRUE ~ "Other benefits and assistance"))
totals <- bind_rows(old_total,
  read_data("benefit_total") |> filter(status == "Actual") |>
    transmute(year, value_millions = value, source_id = "treasury_expenses", source_vintage = 2026L, pdf_page = NA_integer_))
stopifnot(nrow(totals) == 25, !anyDuplicated(totals$year), all(totals$year == 2001:2025))

# Dollar block only: the fiscal-history workbook also repeats the series as
# GDP percentages. Selecting the first observation of each year prevents a
# dollar/percentage mix. Stars and other annotations are removed from years.
spend <- excel(files$treasury_fiscal_history, "Spending")
hist_fiscal <- tibble(year = number(substr(spend[[2]], 1, 4)),
                     accounting_basis = spend[[1]],
                     core_expenses_millions = number(spend[[4]]),
                     social_welfare_gsf_millions = number(spend[[7]])) |>
  fill(accounting_basis) |> filter(year >= 1972, year <= 2025) |> distinct(year, .keep_all = TRUE)
gdp_raw <- excel(files$treasury_fiscal_history, "Nominal GDP")
gdp <- tibble(year = number(gdp_raw[[2]]), gdp_millions = number(gdp_raw[[3]])) |> filter(!is.na(year))
hist_fiscal <- hist_fiscal |> left_join(gdp, by = "year")
stopifnot(nrow(hist_fiscal) == 54, hist_fiscal$social_welfare_gsf_millions[hist_fiscal$year == 1972] == 391.9)
write_data(hist_fiscal |> mutate(percent_gdp = 100 * social_welfare_gsf_millions / gdp_millions), "social_welfare_gsf_history")
denominators <- bind_rows(read_data("fiscal_denominators") |> filter(year <= 2025) |>
  select(year, gdp_millions, core_expenses_millions),
  hist_fiscal |> select(year, gdp_millions, core_expenses_millions)) |> distinct(year, .keep_all = TRUE)
cpi_2025 <- wb_wide$cpi[wb_wide$year == 2025]
groups <- components |> group_by(year, group) |> summarise(value_millions = sum(value_millions, na.rm = TRUE), .groups = "drop") |>
  # Complete category totals, not original source cells. A basket with no
  # programmes in a given year (e.g. COVID support before its introduction)
  # contributes zero to the exhaustive, reconciled expense decomposition.
  complete(year = 2001:2025, group, fill = list(value_millions = 0)) |>
  left_join(totals |> select(year, total_millions = value_millions), by = "year") |>
  left_join(denominators, by = "year") |> left_join(wb_wide, by = "year") |>
  mutate(share_benefits_percent = 100 * value_millions / total_millions,
         percent_gdp = 100 * value_millions / gdp_millions,
         real_2025_millions = value_millions * cpi_2025 / cpi,
         per_resident_real_2025 = 1e6 * real_2025_millions / population)
stopifnot(all(abs((groups |> group_by(year) |> summarise(check = sum(value_millions) - first(total_millions)))$check) < .01))
write_data(components, "benefit_history_components")
write_data(groups, "benefit_history_groups")
write_data(totals |> left_join(denominators, by = "year") |> left_join(wb_wide, by = "year") |>
  mutate(real_2025_millions = value_millions * cpi_2025 / cpi, percent_gdp = 100 * value_millions / gdp_millions), "benefit_expense_history_totals")

archival_counts <- read_data("archival_benefit_counts")
archival_expenses <- read_data("archival_benefit_expenses")
stopifnot(min(archival_counts$year) == 1940, max(archival_counts$year) == 2012,
          archival_counts$recipients[archival_counts$year == 2012 & archival_counts$programme == "NZ Superannuation / predecessor pensions"] == 598933,
          archival_expenses$value_millions[archival_expenses$year == 2002 & archival_expenses$programme == "NZ Superannuation / predecessor pensions"] == 5600.488)
write_data(archival_expenses |> left_join(wb_wide, by = "year") |>
  mutate(real_2025_millions = value_millions * cpi_2025 / cpi), "archival_benefit_expenses_deflated")
# Annual pension series uses old published stocks to 2008, then the modern
# June-quarter all-age series. This is a documented source/cadence splice;
# eligibility and spouse-counting changes prevent a constant-coverage claim.
pension <- bind_rows(
  archival_counts |> filter(programme == "NZ Superannuation / predecessor pensions", year <= 2008) |>
    transmute(year, recipients, source_id, regime = "Archival pension series"),
  nzs |> filter(format(date, "%m-%d") == "06-30") |> mutate(year = as.integer(format(date, "%Y"))) |>
    transmute(year, recipients, source_id, regime = "Modern June NZ Super series")) |>
  arrange(year) |> left_join(wb_wide, by = "year") |> mutate(per_1000_residents = recipients / population * 1000,
    counting_regime = if_else(year < 1996, "Before non-qualified spouses included", "Includes non-qualified spouses"))
stopifnot(!anyDuplicated(pension$year), max(pension$year) == 2026)
write_data(pension, "pension_recipient_long_history")
# Approximate average caseload over each June fiscal year uses four quarter-end
# stocks, not the single June stock; excludes years with incomplete quarters.
nzs_years <- nzs |> mutate(year = as.integer(format(date, "%Y")) + (as.integer(format(date, "%m")) > 6)) |>
  group_by(year) |> summarise(quarters = n(), average_recipients = mean(recipients), .groups = "drop") |> filter(quarters == 4, year <= 2025)
nzs_decomposition <- groups |> filter(group == "NZ Superannuation") |>
  inner_join(nzs_years, by = "year") |>
  mutate(real_expense_per_average_recipient = real_2025_millions * 1e6 / average_recipients)
write_data(nzs_decomposition, "nzs_expense_caseload_decomposition")
write_data(bind_rows(manifest), "benefit_history_source_manifest")
write_data(tibble(check = c("72 consecutive benefit quarters", "Main benefit components reconcile within rounding", "Jobseeker subgroups reconcile within rounding", "2001-2020 PDF expenses reconcile", "2001-2025 grouped expenses reconcile", "Archival pension anchors", "No duplicate years in pension splice"), result = "PASS"), "benefit_history_validation")
message("Long benefit histories saved: quarterly 2008-2026; Treasury expenses 2001-2025; archival observations from 1940.")

# New Zealand fiscal-risk data pulls
#
# Sources:
#   * NZ Treasury fiscal time series (history to June 2025)
#   * NZ Treasury Budget Economic and Fiscal Update 2026 (history + forecast)
#   * NZ Treasury Long-term Fiscal Statement 2025 (long-run scenarios)
#   * Stats NZ general-government GFS (COFOG history)
#   * NZ Treasury 2025 Long-term Fiscal Model (functional projections)
#   * World Bank Indicators API (international macro context)
#
# Run from any working directory with:
#   Rscript "Data pulls/01_pull_data.R"
# Set NZ_DATA_REFRESH=false to reuse already-downloaded source workbooks.

required_packages <- c("dplyr", "httr2", "jsonlite", "pdftools", "purrr", "readr", "readxl", "tidyr")
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
  library(purrr)
  library(readr)
  library(readxl)
  library(tidyr)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE))
} else {
  normalizePath(getwd(), mustWork = TRUE)
}

raw_dir <- file.path(script_dir, "raw")
processed_dir <- file.path(script_dir, "processed")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

refresh <- tolower(Sys.getenv("NZ_DATA_REFRESH", "true")) %in% c("1", "true", "yes")

sources <- tibble::tribble(
  ~source_id, ~description, ~url, ~local_file,
  "treasury_history",
  "Fiscal Time Series Historical Fiscal Indicators 1972-2025",
  "https://www.treasury.govt.nz/sites/default/files/2026-01/fiscaltimeseries1972-2025-year-end25.xlsx",
  "treasury_fiscal_history_1972_2025.xlsx",
  "befu_2026",
  "Budget Economic and Fiscal Update 2026 - Charts and Data",
  "https://www.treasury.govt.nz/sites/default/files/2026-05/befu26-charts-data.xlsx",
  "treasury_befu_2026_charts_data.xlsx",
  "ltfs_2025",
  "He Tirohanga Mokopuna 2025 - Charts and Data",
  "https://www.treasury.govt.nz/sites/default/files/2025-09/ltfs-2025-charts-data.xlsx",
  "treasury_ltfs_2025_charts_data.xlsx",
  "stats_nz_gfs_2025",
  "Government Finance Statistics (general government), year ended June 2025",
  "https://www.stats.govt.nz/assets/Uploads/Government-finance-statistics-general-government/Government-finance-statistics-general-government-Year-ended-June-2025/Download-data/government-finance-statistics-general-government-year-ended-june-2025.CSV",
  "stats_nz_gfs_general_government_2025.csv",
  "treasury_ltfm_2025",
  "Long-term Fiscal Model for He Tirohanga Mokopuna 2025",
  "https://www.treasury.govt.nz/sites/default/files/2025-09/ltfm-htm-sep25.xlsx",
  "treasury_ltfm_2025.xlsx",
  "treasury_hyefu_2010",
  "Half Year Economic and Fiscal Update 2010",
  "https://www.treasury.govt.nz/sites/default/files/2010-12/hyefu10.pdf",
  "treasury_hyefu_2010.pdf",
  "treasury_befu_2025",
  "Budget Economic and Fiscal Update 2025",
  "https://www.treasury.govt.nz/sites/default/files/2025-05/befu25-v2.pdf",
  "treasury_befu_2025.pdf"
)

download_source <- function(url, destination) {
  if (!refresh && file.exists(destination) && file.info(destination)$size > 0) {
    message("Using cached file: ", basename(destination))
    return(invisible(destination))
  }

  message("Downloading: ", url)
  request <- httr2::request(url) |>
    httr2::req_user_agent("NZ fiscal risks research; R/httr2") |>
    httr2::req_retry(max_tries = 3)
  response <- httr2::req_perform(request, path = destination)
  if (httr2::resp_status(response) >= 300) stop("Download failed: ", url, call. = FALSE)
  invisible(destination)
}

walk2(sources$url, file.path(raw_dir, sources$local_file), download_source)

history_file <- file.path(raw_dir, sources$local_file[sources$source_id == "treasury_history"])
befu_file <- file.path(raw_dir, sources$local_file[sources$source_id == "befu_2026"])
ltfs_file <- file.path(raw_dir, sources$local_file[sources$source_id == "ltfs_2025"])
gfs_file <- file.path(raw_dir, sources$local_file[sources$source_id == "stats_nz_gfs_2025"])
ltfm_file <- file.path(raw_dir, sources$local_file[sources$source_id == "treasury_ltfm_2025"])
hyefu_2010_file <- file.path(raw_dir, sources$local_file[sources$source_id == "treasury_hyefu_2010"])
befu_2025_file <- file.path(raw_dir, sources$local_file[sources$source_id == "treasury_befu_2025"])

as_number <- function(x) suppressWarnings(as.numeric(as.character(x)))

# The historical workbook uses row 4 as its series header and column 2 as year.
parse_history_sheet <- function(sheet) {
  raw <- read_excel(history_file, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  series <- as.character(unlist(raw[4, -c(1, 2)], use.names = FALSE))
  dat <- raw[-seq_len(4), , drop = FALSE]
  years <- as_number(dat[[2]])

  map_dfr(seq_along(series), function(j) {
    if (is.na(series[[j]]) || !nzchar(trimws(series[[j]]))) return(NULL)
    tibble(
      category = sheet,
      year = years,
      series = trimws(series[[j]]),
      value_millions = as_number(dat[[j + 2]])
    )
  }) |>
    filter(!is.na(year), !is.na(value_millions))
}

history_long <- map_dfr(
  c("Spending", "Revenue, Surplus measures", "Debt, Net Worth", "NZS Fund series"),
  parse_history_sheet
)

gdp_raw <- read_excel(history_file, sheet = "Nominal GDP", col_names = FALSE, .name_repair = "minimal")
gdp <- tibble(
  year = as_number(gdp_raw[[2]][-seq_len(4)]),
  nominal_gdp_millions = as_number(gdp_raw[[3]][-seq_len(4)])
) |>
  filter(!is.na(year), !is.na(nominal_gdp_millions))

history_long <- history_long |>
  left_join(gdp, by = "year") |>
  mutate(percent_gdp = 100 * value_millions / nominal_gdp_millions) |>
  arrange(category, series, year)

# BEFU fiscal sheet: values are in $m in rows 10-30 and % of GDP in rows 38-58.
parse_befu_fiscal <- function() {
  raw <- read_excel(befu_file, sheet = "Fiscal Time Series", col_names = FALSE, .name_repair = "minimal")
  years <- as_number(unlist(raw[4, 3:ncol(raw)], use.names = FALSE))
  status <- as.character(unlist(raw[5, 3:ncol(raw)], use.names = FALSE))

  parse_rows <- function(rows, unit) {
    map_dfr(rows, function(i) {
      measure <- as.character(raw[[2]][i])
      if (is.na(measure) || !nzchar(trimws(measure))) return(NULL)
      tibble(
        year = years,
        status = status,
        measure = trimws(measure),
        unit = unit,
        value = as_number(unlist(raw[i, 3:ncol(raw)], use.names = FALSE))
      )
    })
  }

  bind_rows(parse_rows(10:30, "$ millions"), parse_rows(38:58, "% of GDP")) |>
    mutate(value = if_else(unit == "% of GDP", 100 * value, value)) |>
    filter(!is.na(year), !is.na(value)) |>
    arrange(unit, measure, year)
}

# BEFU economic sheet: labels are in column 1 and years begin in column 2.
parse_befu_economic <- function() {
  raw <- read_excel(befu_file, sheet = "Economic Time Series", col_names = FALSE, .name_repair = "minimal")
  years <- as_number(unlist(raw[4, 2:ncol(raw)], use.names = FALSE))
  status <- as.character(unlist(raw[5, 2:ncol(raw)], use.names = FALSE))

  map_dfr(6:nrow(raw), function(i) {
    measure <- as.character(raw[[1]][i])
    values <- as_number(unlist(raw[i, 2:ncol(raw)], use.names = FALSE))
    if (is.na(measure) || all(is.na(values))) return(NULL)
    tibble(year = years, status = status, measure = trimws(measure), value = values)
  }) |>
    filter(!is.na(year), !is.na(value)) |>
    arrange(measure, year)
}

befu_fiscal <- parse_befu_fiscal()
befu_economic <- parse_befu_economic()

# Generic parser for the LTFS "Fig n data" sheets. It detects the first data
# year, then uses the earliest nearby row containing the maximum number of
# series labels. This accommodates the one-row header offset in some figures.
parse_ltfs_figure <- function(sheet, topic) {
  raw <- read_excel(ltfs_file, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  first_col <- as_number(raw[[1]])
  data_start <- which(first_col >= 1900 & first_col <= 2100)[1]
  if (is.na(data_start)) stop("Could not find year data in ", sheet, call. = FALSE)

  candidates <- seq.int(max(1, data_start - 4), data_start - 1)
  filled <- vapply(candidates, function(i) {
    sum(!is.na(unlist(raw[i, -1, drop = FALSE], use.names = FALSE)))
  }, numeric(1))
  header_row <- candidates[which.max(filled)]
  labels <- as.character(unlist(raw[header_row, -1, drop = FALSE], use.names = FALSE))
  unit_row <- min(data_start - 1, header_row + 1)
  units <- as.character(unlist(raw[unit_row, -1, drop = FALSE], use.names = FALSE))
  title <- trimws(as.character(raw[[1]][1]))
  source <- trimws(as.character(raw[[1]][2]))

  map_dfr(seq_along(labels), function(j) {
    label <- labels[[j]]
    if (is.na(label) || !nzchar(trimws(label)) || tolower(trimws(label)) == "line") return(NULL)
    tibble(
      figure = sub(" data$", "", sheet),
      topic = topic,
      title = title,
      source = source,
      year = first_col[data_start:nrow(raw)],
      series = trimws(label),
      unit = ifelse(is.na(units[[j]]), "", trimws(units[[j]])),
      value = as_number(raw[[j + 1]][data_start:nrow(raw)])
    )
  }) |>
    filter(!is.na(year), !is.na(value))
}

ltfs_figures <- tibble::tribble(
  ~sheet, ~topic,
  "Fig 5 data", "10-year government bond rate",
  "Fig 6 data", "productivity growth",
  "Fig 8 data", "historical debt versus earlier projections",
  "Fig 13 data", "structural fiscal balance",
  "Fig 15 data", "old-age support ratio",
  "Fig 18 data", "unchanged-policy revenue and expenditure",
  "Fig 19 data", "unchanged-policy net debt",
  "Fig 20 data", "OLG and LTFM net debt projections",
  "Fig 21 data", "interest-rate sensitivity",
  "Fig 22 data", "health expenditure and productivity",
  "Fig 23 data", "superannuation expenditure and productivity",
  "Fig 26 data", "demographic sensitivity of expenditure",
  "Fig 28 data", "favourable and unfavourable fiscal scenarios"
)

ltfs_long <- map2_dfr(ltfs_figures$sheet, ltfs_figures$topic, parse_ltfs_figure) |>
  arrange(figure, series, year)

# Stats NZ's GFS file provides genuine COFOG data for general government
# (central plus local government). Total expenses include operating expenses
# plus net acquisition of non-financial assets.
gfs_raw <- read_csv(
  gfs_file,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

cofog_history <- gfs_raw |>
  filter(grepl("^General Government, Expenses by Function", Group)) |>
  mutate(
    expense_measure = case_when(
      grepl("Total Expenses$", Group) ~ "Total expenses",
      grepl("Operating Expenses$", Group) ~ "Operating expenses",
      grepl("Net Acquisition", Group) ~ "Net acquisition of non-financial assets",
      TRUE ~ NA_character_
    ),
    function_name = case_when(
      expense_measure == "Total expenses" ~ Series_title_2,
      TRUE ~ Series_title_1
    ),
    year = as.integer(substr(Period, 1, 4)),
    value_millions = as.numeric(Data_value) * 10^as.numeric(MAGNTUDE) / 1e6
  ) |>
  filter(!is.na(expense_measure), !is.na(function_name), !is.na(value_millions)) |>
  select(year, status = STATUS, expense_measure, function_name, value_millions) |>
  left_join(gdp, by = "year") |>
  mutate(percent_gdp = 100 * value_millions / nominal_gdp_millions) |>
  group_by(year, expense_measure) |>
  mutate(
    total_for_measure_millions = value_millions[match("Total", function_name)],
    share_of_total = value_millions / total_for_measure_millions
  ) |>
  ungroup() |>
  arrange(expense_measure, function_name, year)

# Treasury's LTFM categories are not strict COFOG: they cover the total Crown
# rather than general government and use Treasury functional expense classes.
# They are nevertheless the best published function-level long-run paths.
ltfm_raw <- read_excel(
  ltfm_file,
  sheet = "Long-Term Fiscal Model",
  col_names = FALSE,
  .name_repair = "minimal"
)
ltfm_years <- as_number(unlist(ltfm_raw[6, 4:ncol(ltfm_raw)], use.names = FALSE))
ltfm_gdp <- as_number(unlist(ltfm_raw[13, 4:ncol(ltfm_raw)], use.names = FALSE))

ltfm_rows <- tibble::tribble(
  ~row, ~functional_class, ~scope,
  35, "Total Crown expenses", "Total Crown",
  180, "New Zealand Superannuation", "Core Crown; also included in total Crown social security and welfare",
  198, "Social security and welfare", "Total Crown",
  232, "Finance costs", "Total Crown",
  256, "Health", "Total Crown",
  269, "Education", "Total Crown",
  277, "Core government services", "Total Crown",
  281, "Law and order", "Total Crown",
  288, "Transport and communications", "Total Crown",
  295, "Economic and industrial services", "Total Crown",
  299, "Defence", "Total Crown",
  303, "Heritage, culture and recreation", "Total Crown",
  307, "Primary services", "Total Crown",
  311, "Housing and community development", "Total Crown",
  317, "Environmental protection", "Total Crown",
  321, "Other", "Total Crown"
)

ltfm_functional <- pmap_dfr(ltfm_rows, function(row, functional_class, scope) {
  value_billions <- as_number(unlist(ltfm_raw[row, 4:ncol(ltfm_raw)], use.names = FALSE))
  tibble(
    year = ltfm_years,
    status = case_when(
      ltfm_years <= 2024 ~ "Outturn",
      ltfm_years <= 2029 ~ "BEFU 2025 forecast",
      TRUE ~ "Projection"
    ),
    functional_class = functional_class,
    scope = scope,
    value_billions = value_billions,
    nominal_gdp_billions = ltfm_gdp,
    percent_gdp = 100 * value_billions / ltfm_gdp
  )
}) |>
  filter(!is.na(year), !is.na(value_billions)) |>
  arrange(functional_class, year)

# A cautious bridge for readers. It identifies the closest COFOG division but
# is not used to relabel or aggregate the LTFM data as COFOG.
functional_concordance <- tibble::tribble(
  ~treasury_functional_class, ~closest_cofog_division, ~mapping_note,
  "Core government services", "General public services", "Closest broad match; COFOG also places public-debt transactions in general public services.",
  "Finance costs", "General public services", "Related to public-debt transactions, but retained separately in Treasury reporting.",
  "Defence", "Defence", "Close conceptual match.",
  "Law and order", "Public order and safety", "Close conceptual match.",
  "Transport and communications", "Economic affairs", "Partial match; economic affairs is broader.",
  "Economic and industrial services", "Economic affairs", "Partial match; overlaps with other Treasury classes.",
  "Primary services", "Economic affairs", "Partial match; mainly agriculture, forestry and fishing functions.",
  "Environmental protection", "Environmental protection", "Close conceptual match.",
  "Housing and community development", "Housing and community amenities", "Close conceptual match.",
  "Health", "Health", "Close conceptual match.",
  "Heritage, culture and recreation", "Recreation, culture and religion", "Close conceptual match.",
  "Education", "Education", "Close conceptual match.",
  "Social security and welfare", "Social protection", "Close conceptual match; includes New Zealand Superannuation.",
  "Other", NA_character_, "No unique COFOG match."
)

# Contributions to annual nominal growth in total COFOG expenditure. A
# function's contribution is its dollar change divided by the previous year's
# total expenditure, so contributions sum to the annual growth rate (apart
# from the source data's small published-rounding differences).
cofog_growth_contributions <- cofog_history |>
  filter(expense_measure == "Total expenses", function_name != "Total") |>
  group_by(function_name) |>
  arrange(year, .by_group = TRUE) |>
  mutate(change_millions = value_millions - lag(value_millions)) |>
  ungroup() |>
  left_join(
    cofog_history |>
      filter(expense_measure == "Total expenses", function_name == "Total") |>
      transmute(
        year,
        total_expenses_millions = value_millions,
        previous_total_expenses_millions = lag(value_millions),
        total_nominal_growth_percent = 100 * (value_millions / lag(value_millions) - 1)
      ),
    by = "year"
  ) |>
  mutate(contribution_to_growth_pp = 100 * change_millions / previous_total_expenses_millions) |>
  filter(!is.na(contribution_to_growth_pp)) |>
  arrange(year, function_name)

# A deliberately endpoint-based decomposition for op-ed comparisons. The
# contribution measures allocate the total nominal dollar increase between
# 2010 and 2025 across the ten COFOG divisions; they therefore sum to 100%.
cofog_2010_2025 <- cofog_history |>
  filter(
    expense_measure == "Total expenses",
    function_name != "Total",
    year %in% c(2010, 2025)
  ) |>
  select(function_name, year, value_millions, percent_gdp, share_of_total) |>
  pivot_wider(
    names_from = year,
    values_from = c(value_millions, percent_gdp, share_of_total),
    names_glue = "{.value}_{year}"
  ) |>
  mutate(
    change_millions = value_millions_2025 - value_millions_2010,
    own_nominal_growth_percent = 100 * (value_millions_2025 / value_millions_2010 - 1)
  )

cofog_total_increase <- cofog_2010_2025 |>
  summarise(total = sum(change_millions)) |>
  pull(total)
cofog_total_2010 <- cofog_history |>
  filter(expense_measure == "Total expenses", function_name == "Total", year == 2010) |>
  pull(value_millions)

cofog_2010_2025 <- cofog_2010_2025 |>
  mutate(
    contribution_to_total_increase_percent = 100 * change_millions / cofog_total_increase,
    contribution_to_cumulative_growth_pp = 100 * change_millions / cofog_total_2010,
    share_of_total_2010_percent = 100 * share_of_total_2010,
    share_of_total_2025_percent = 100 * share_of_total_2025,
    change_in_spending_share_pp = share_of_total_2025_percent - share_of_total_2010_percent
  ) |>
  select(-share_of_total_2010, -share_of_total_2025) |>
  arrange(desc(contribution_to_total_increase_percent))

# Stats NZ publishes only first-level COFOG for New Zealand. The Treasury's
# core-Crown expense tables provide a related subcategory lens at the two
# requested endpoints. They are not second-level COFOG, exclude local
# government, and the 2025 endpoint is a Budget 2025 forecast.
hyefu_2010_pages <- pdftools::pdf_text(hyefu_2010_file)
befu_2025_pages <- pdftools::pdf_text(befu_2025_file)

pdf_table_block <- function(page_text, table_number) {
  marker <- paste0("Table ", table_number)
  start <- regexpr(marker, page_text, fixed = TRUE)[[1]]
  if (start < 0) stop("Could not find ", marker, " in Treasury PDF.", call. = FALSE)
  remainder <- substring(page_text, start)
  next_table <- regexpr("\nTable ", remainder, fixed = TRUE)[[1]]
  if (next_table > 1) substring(remainder, 1, next_table - 1) else remainder
}

pdf_endpoint_value <- function(block, label, column, match_number = 1) {
  lines <- strsplit(block, "\n", fixed = TRUE)[[1]]
  hits <- grep(label, lines, fixed = TRUE)
  if (!length(hits)) stop("Could not find PDF row: ", label, call. = FALSE)
  valid_values <- list()
  for (hit in hits) {
    line <- lines[[hit]]
    label_start <- regexpr(label, line, fixed = TRUE)[[1]]
    remainder <- substring(line, label_start + nchar(label))
    tokens <- regmatches(
      remainder,
      gregexpr("\\(?\\s*[0-9][0-9,]*\\s*\\)?|\\.\\.", remainder, perl = TRUE)
    )[[1]]
    values <- ifelse(
      tokens == "..",
      0,
      as_number(gsub("[(),[:space:]]", "", tokens)) * ifelse(grepl("\\(", tokens), -1, 1)
    )
    if (length(values) >= column) valid_values[[length(valid_values) + 1]] <- values[[column]]
  }
  if (length(valid_values) >= match_number) return(valid_values[[match_number]])
  stop("Too few matching PDF rows or values in row: ", label, call. = FALSE)
}

v10 <- function(table, label, page) {
  pdf_endpoint_value(pdf_table_block(hyefu_2010_pages[[page]], table), label, 5)
}
v25 <- function(table, label, page) {
  pdf_endpoint_value(pdf_table_block(befu_2025_pages[[page]], table), label, 6)
}

make_endpoint_group <- function(group, total_2010, total_2025, components) {
  components |>
    bind_rows(tibble(
      closest_cofog_division = group,
      subcategory = "Other / residual",
      value_millions_2010 = total_2010 - sum(components$value_millions_2010),
      value_millions_2025 = total_2025 - sum(components$value_millions_2025)
    )) |>
    mutate(
      mapped_group_millions_2010 = total_2010,
      mapped_group_millions_2025 = total_2025
    )
}

component <- function(group, name, value_2010, value_2025) {
  tibble(
    closest_cofog_division = group,
    subcategory = name,
    value_millions_2010 = value_2010,
    value_millions_2025 = value_2025
  )
}

social_10 <- v10("4.1", "Social security and welfare expenses", 99)
social_25 <- v25("5.1", "Social security and welfare expenses", 153)
health_10 <- v10("4.5", "Health expenses", 100)
health_25 <- v25("5.3", "Health expenses", 154)
education_10 <- v10("4.7", "Education expenses", 101)
education_25 <- v25("5.4", "Education expenses", 154)
core_gov_10 <- v10("4.10", "Core Government service expenses", 101)
core_gov_25 <- v25("5.7", "Core government service expenses", 155)
finance_10 <- 2311
finance_25 <- v25("5.16", "Finance costs expenses", 157)

treasury_subcategories_2010_2025 <- bind_rows(
  make_endpoint_group(
    "Social protection", social_10, social_25,
    bind_rows(
      component("Social protection", "Welfare benefits", v10("4.1", "Welfare benefits", 99), v25("5.1", "Welfare benefits", 153)),
      component("Social protection", "Departmental expenses", v10("4.1", "Departmental expenses", 99), v25("5.1", "Departmental expenses", 153))
    )
  ),
  make_endpoint_group(
    "Health", health_10, health_25,
    bind_rows(
      component(
        "Health", "Health-service, disability and pharmaceutical purchasing",
        v10("4.5", "Health service purchasing", 100),
        v25("5.3", "Purchasing of health services", 154) +
          v25("5.3", "National disability support services", 154) +
          v25("5.3", "National Pharmaceuticals Purchasing", 154)
      ),
      component("Health", "Departmental outputs", v10("4.5", "Departmental outputs", 100), v25("5.3", "Departmental outputs", 154)),
      component("Health", "Health payments to ACC", v10("4.5", "Health payments to ACC", 100), v25("5.3", "Health payments to ACC", 154))
    )
  ),
  make_endpoint_group(
    "Education", education_10, education_25,
    bind_rows(
      component("Education", "Early childhood education", v10("4.7", "Early childhood education", 101), v25("5.4", "Early childhood education", 154)),
      component("Education", "Primary and secondary schools", v10("4.7", "Primary and secondary schools", 101), v25("5.4", "Primary and secondary schools", 154)),
      component("Education", "Tertiary funding", v10("4.7", "Tertiary funding", 101), v25("5.4", "Tertiary funding", 154)),
      component("Education", "Departmental expenses", v10("4.7", "Departmental expenses", 101), v25("5.4", "Departmental expenses", 154))
    )
  ),
  make_endpoint_group(
    "General public services", core_gov_10 + finance_10, core_gov_25 + finance_25,
    bind_rows(
      component("General public services", "Finance costs", finance_10, finance_25),
      component("General public services", "Departmental expenses", v10("4.10", "Departmental expenses", 101), v25("5.7", "Departmental expenses", 155)),
      component("General public services", "International development", v10("4.10", "Official development assistance", 101), v25("5.7", "International Development Cooperation", 155)),
      component("General public services", "Tax receivable write-downs", v10("4.10", "Tax receivable write-down and impairments", 101), v25("5.7", "Tax receivable write-down and impairments", 155)),
      component("General public services", "Science expenses", v10("4.10", "Science expenses", 101), v25("5.7", "Science expenses", 155))
    )
  ),
  make_endpoint_group(
    "Public order and safety", v10("4.11", "Law and order expenses", 102), v25("5.8", "Law and order expenses", 156),
    bind_rows(
      component("Public order and safety", "Police", v10("4.11", "Police", 102), v25("5.8", "Police", 156)),
      component("Public order and safety", "Corrections", v10("4.11", "Department of Corrections", 102), v25("5.8", "Department of Corrections", 156)),
      component("Public order and safety", "Justice", v10("4.11", "Ministry of Justice", 102), v25("5.8", "Ministry of Justice", 156)),
      component("Public order and safety", "Customs", v10("4.11", "Customs", 102), v25("5.8", "NZ Customs Service", 156))
    )
  ),
  make_endpoint_group(
    "Defence", v10("4.12", "Defence expenses", 102), v25("5.11", "Defence expenses", 156),
    component("Defence", "Defence Force", v10("4.12", "NZDF Core expenses", 102), v25("5.11", "New Zealand Defence Force expenses", 156))
  ),
  make_endpoint_group(
    "Economic affairs",
    v10("4.13", "Transport and communication expenses", 102) + v10("4.14", "Economic and industrial services expenses", 102) + v10("4.16", "Primary service expenses", 103),
    v25("5.9", "Transport and communication expenses", 156) + v25("5.10", "Economic and industrial services expenses", 156) + v25("5.13", "Primary services expenses", 157),
    bind_rows(
      component("Economic affairs", "Transport and communications", v10("4.13", "Transport and communication expenses", 102), v25("5.9", "Transport and communication expenses", 156)),
      component("Economic affairs", "Economic and industrial services", v10("4.14", "Economic and industrial services expenses", 102), v25("5.10", "Economic and industrial services expenses", 156)),
      component("Economic affairs", "Primary services", v10("4.16", "Primary service expenses", 103), v25("5.13", "Primary services expenses", 157))
    )
  ),
  make_endpoint_group(
    "Recreation, culture and religion", v10("4.17", "Heritage, culture and recreation expenses", 103), v25("5.12", "Heritage, culture and recreation expenses", 157),
    bind_rows(
      component("Recreation, culture and religion", "Departmental outputs", v10("4.17", "Departmental outputs", 103), v25("5.12", "Departmental outputs", 157)),
      component("Recreation, culture and religion", "Non-departmental outputs", v10("4.17", "Non-departmental outputs", 103), v25("5.12", "Non-departmental outputs", 157))
    )
  ),
  make_endpoint_group(
    "Housing and community amenities", v10("4.18", "Housing and community development", 103), v25("5.14", "Housing and community development expenses", 157),
    component("Housing and community amenities", "Departmental outputs", v10("4.18", "Departmental outputs", 103), v25("5.14", "Departmental outputs", 157))
  )
) |>
  filter(subcategory != "Other / residual" | value_millions_2010 != 0 | value_millions_2025 != 0) |>
  mutate(
    change_millions = value_millions_2025 - value_millions_2010,
    own_nominal_growth_percent = 100 * (value_millions_2025 / value_millions_2010 - 1),
    share_of_mapped_group_percent_2010 = 100 * value_millions_2010 / mapped_group_millions_2010,
    share_of_mapped_group_percent_2025 = 100 * value_millions_2025 / mapped_group_millions_2025,
    mapped_group_increase_millions = mapped_group_millions_2025 - mapped_group_millions_2010,
    contribution_to_mapped_group_increase_percent = 100 * change_millions / mapped_group_increase_millions,
    change_in_group_share_pp = share_of_mapped_group_percent_2025 - share_of_mapped_group_percent_2010,
    scope = "Treasury core Crown; closest functional match, not second-level COFOG",
    endpoint_status = "2010 actual; 2025 Budget 2025 forecast"
  ) |>
  arrange(closest_cofog_division, desc(contribution_to_mapped_group_increase_percent))

# Harmonise the major benefit types across the 2013 welfare reforms and other
# naming changes. Residual benefits include the smaller programmes that cannot
# be matched cleanly across the two publication vintages.
benefit_2010 <- pdf_table_block(hyefu_2010_pages[[100]], "4.2")
benefit_2025 <- pdf_table_block(befu_2025_pages[[154]], "5.2")
benefit_value <- function(block, label, column) pdf_endpoint_value(block, label, column)

welfare_benefit_decomposition <- tibble::tribble(
  ~benefit_group, ~value_millions_2010, ~value_millions_2025,
  "New Zealand Superannuation", benefit_value(benefit_2010, "New Zealand Superannuation", 5), benefit_value(benefit_2025, "New Zealand Superannuation", 6),
  "Jobseeker-related support", benefit_value(benefit_2010, "Unemployment Benefit", 5) + benefit_value(benefit_2010, "Sickness Benefit", 5), benefit_value(benefit_2025, "Jobseeker Support and Emergency Benefit", 6),
  "Disability-related support", benefit_value(benefit_2010, "Invalids Benefit", 5) + benefit_value(benefit_2010, "Disability Allowance", 5), benefit_value(benefit_2025, "Supported Living Payment", 6) + benefit_value(benefit_2025, "Disability Assistance", 6),
  "Sole-parent support", benefit_value(benefit_2010, "Domestic Purposes Benefit", 5), benefit_value(benefit_2025, "Sole Parent Support", 6),
  "Family tax credits", benefit_value(benefit_2010, "Family Tax Credit", 5) + benefit_value(benefit_2010, "In Work Tax Credit", 5) + benefit_value(benefit_2010, "Child Tax Credit", 5), benefit_value(benefit_2025, "Family Tax Credit", 6) + benefit_value(benefit_2025, "Other Working for Families tax credits", 6) + benefit_value(benefit_2025, "FamilyBoost tax credit", 6),
  "Housing assistance", benefit_value(benefit_2010, "Accommodation Supplement", 5) + benefit_value(benefit_2010, "Income Related Rents", 5), benefit_value(benefit_2025, "Accommodation Assistance", 6) + benefit_value(benefit_2025, "Income-Related Rents", 6)
)

benefit_total_2010 <- benefit_value(benefit_2010, "Benefit expenses", 5)
benefit_total_2025 <- benefit_value(benefit_2025, "Benefit expenses", 6)
welfare_benefit_decomposition <- welfare_benefit_decomposition |>
  bind_rows(tibble(
    benefit_group = "Other benefits",
    value_millions_2010 = benefit_total_2010 - sum(welfare_benefit_decomposition$value_millions_2010),
    value_millions_2025 = benefit_total_2025 - sum(welfare_benefit_decomposition$value_millions_2025)
  )) |>
  mutate(
    change_millions = value_millions_2025 - value_millions_2010,
    own_nominal_growth_percent = 100 * (value_millions_2025 / value_millions_2010 - 1),
    contribution_to_benefit_increase_percent = 100 * change_millions / (benefit_total_2025 - benefit_total_2010),
    share_of_benefit_expenses_2010_percent = 100 * value_millions_2010 / benefit_total_2010,
    share_of_benefit_expenses_2025_percent = 100 * value_millions_2025 / benefit_total_2025,
    endpoint_status = "2010 actual; 2025 Budget 2025 forecast"
  ) |>
  arrange(desc(contribution_to_benefit_increase_percent))

beneficiary_2010 <- pdf_table_block(hyefu_2010_pages[[100]], "4.3")
welfare_recipient_counts <- tibble::tribble(
  ~recipient_group, ~recipients_thousands_2010, ~recipients_thousands_2025, ~comparability_note,
  "New Zealand Superannuation", pdf_endpoint_value(beneficiary_2010, "New Zealand Superannuation", 5), pdf_endpoint_value(benefit_2025, "New Zealand Superannuation", 6, 2), "Directly comparable broad programme.",
  "Jobseeker-related", pdf_endpoint_value(beneficiary_2010, "Unemployment Benefit", 5) + pdf_endpoint_value(beneficiary_2010, "Sickness Benefit", 5), pdf_endpoint_value(benefit_2025, "Jobseeker Support and Emergency Benefit", 6, 2), "2010 combines Unemployment and Sickness Benefits; 2025 uses Jobseeker Support and Emergency Benefit.",
  "Disability main benefit", pdf_endpoint_value(beneficiary_2010, "Invalids Benefit", 5), pdf_endpoint_value(benefit_2025, "Supported living payment", 6), "Invalids Benefit was replaced by Supported Living Payment; excludes supplementary disability assistance.",
  "Sole-parent main benefit", pdf_endpoint_value(beneficiary_2010, "Domestic Purposes Benefit", 5), pdf_endpoint_value(benefit_2025, "Sole parent support", 6), "Domestic Purposes Benefit is used as the closest 2010 predecessor to Sole Parent Support.",
  "Accommodation support", pdf_endpoint_value(beneficiary_2010, "Accommodation Supplement", 5), pdf_endpoint_value(benefit_2025, "Accommodation Supplement", 6, 1), "Counts refer to Accommodation Supplement recipients; expenditure group also includes income-related rents."
) |>
  mutate(
    change_thousands = recipients_thousands_2025 - recipients_thousands_2010,
    recipient_growth_percent = 100 * (recipients_thousands_2025 / recipients_thousands_2010 - 1),
    endpoint_status = "2010 actual; 2025 Budget 2025 forecast"
  ) |>
  arrange(desc(change_thousands))

# Economic affairs: first separate the strict Stats NZ COFOG increase into
# operating and capital-like expenditure, then provide a more detailed but
# narrower core-Crown decomposition from the Treasury expense tables.
cofog_economic_affairs_expense_type <- cofog_history |>
  filter(
    function_name == "Economic affairs",
    expense_measure != "Total expenses",
    year %in% c(2010, 2025)
  ) |>
  select(expense_measure, year, value_millions, percent_gdp) |>
  pivot_wider(
    names_from = year,
    values_from = c(value_millions, percent_gdp),
    names_glue = "{.value}_{year}"
  ) |>
  mutate(
    change_millions = value_millions_2025 - value_millions_2010,
    contribution_to_economic_affairs_increase_percent = 100 * change_millions / sum(change_millions),
    own_nominal_growth_percent = 100 * (value_millions_2025 / value_millions_2010 - 1)
  ) |>
  arrange(desc(contribution_to_economic_affairs_increase_percent))

make_economic_class <- function(functional_class, total_2010, total_2025, components) {
  components |>
    bind_rows(tibble(
      functional_class = functional_class,
      subcategory = "Other / residual",
      value_millions_2010 = total_2010 - sum(components$value_millions_2010),
      value_millions_2025 = total_2025 - sum(components$value_millions_2025)
    ))
}

economic_component <- function(functional_class, subcategory, value_2010, value_2025) {
  tibble(
    functional_class = functional_class,
    subcategory = subcategory,
    value_millions_2010 = value_2010,
    value_millions_2025 = value_2025
  )
}

transport_2010 <- v10("4.13", "Transport and communication expenses", 102)
transport_2025 <- v25("5.9", "Transport and communication expenses", 156)
industry_2010 <- v10("4.14", "Economic and industrial services expenses", 102)
industry_2025 <- v25("5.10", "Economic and industrial services expenses", 156)
primary_2010 <- v10("4.16", "Primary service expenses", 103)
primary_2025 <- v25("5.13", "Primary services expenses", 157)

treasury_economic_affairs_detail <- bind_rows(
  make_economic_class(
    "Transport and communications", transport_2010, transport_2025,
    bind_rows(
      economic_component("Transport and communications", "Road transport agency", v10("4.13", "New Zealand Transport Agency", 102), v25("5.9", "Waka Kotahi NZ Transport Agency", 156)),
      economic_component("Transport and communications", "Rail funding", v10("4.13", "Rail funding", 102), v25("5.9", "Rail funding", 156)),
      economic_component("Transport and communications", "Departmental outputs", v10("4.13", "Departmental outputs", 102), v25("5.9", "Departmental outputs", 156)),
      economic_component("Transport and communications", "North Island weather-event support", 0, v25("5.9", "North Island weather events", 156))
    )
  ),
  make_economic_class(
    "Economic and industrial services", industry_2010, industry_2025,
    bind_rows(
      economic_component("Economic and industrial services", "Departmental outputs", v10("4.14", "Departmental outputs", 102), v25("5.10", "Departmental outputs", 156)),
      economic_component("Economic and industrial services", "Non-departmental outputs and employment initiatives", v10("4.14", "Non-departmental outputs", 102) + v10("4.14", "Employment initiatives", 102), v25("5.10", "Non-departmental outputs", 156)),
      economic_component("Economic and industrial services", "KiwiSaver and HomeStart", v10("4.14", "KiwiSaver", 102), v25("5.10", "KiwiSaver", 156))
    )
  ),
  make_economic_class(
    "Primary services", primary_2010, primary_2025,
    bind_rows(
      economic_component("Primary services", "Departmental expenses", v10("4.16", "Departmental expenses", 103), v25("5.13", "Departmental expenses", 157)),
      economic_component("Primary services", "Non-departmental outputs", v10("4.16", "Non-departmental outputs", 103), v25("5.13", "Non-departmental outputs", 157))
    )
  )
) |>
  mutate(
    subcategory = case_when(
      subcategory == "Other / residual" & functional_class == "Transport and communications" ~ "Other transport programmes and one-offs",
      subcategory == "Other / residual" & functional_class == "Economic and industrial services" ~ "Other industry services and support",
      subcategory == "Other / residual" & functional_class == "Primary services" ~ "Other primary-services expenses",
      TRUE ~ subcategory
    ),
    change_millions = value_millions_2025 - value_millions_2010,
    mapped_economic_affairs_increase_millions = transport_2025 + industry_2025 + primary_2025 - transport_2010 - industry_2010 - primary_2010,
    contribution_to_mapped_economic_affairs_increase_percent = 100 * change_millions / mapped_economic_affairs_increase_millions,
    own_nominal_growth_percent = if_else(
      value_millions_2010 == 0,
      NA_real_,
      100 * (value_millions_2025 / value_millions_2010 - 1)
    ),
    scope = "Treasury core Crown; related functional classes, not second-level COFOG",
    endpoint_status = "2010 actual; 2025 Budget 2025 forecast"
  ) |>
  arrange(desc(contribution_to_mapped_economic_affairs_increase_percent))

stopifnot(
  abs(sum(cofog_2010_2025$contribution_to_total_increase_percent) - 100) < 0.01,
  abs(sum(welfare_benefit_decomposition$contribution_to_benefit_increase_percent) - 100) < 0.01,
  abs(sum(cofog_economic_affairs_expense_type$contribution_to_economic_affairs_increase_percent) - 100) < 0.01,
  abs(sum(treasury_economic_affairs_detail$contribution_to_mapped_economic_affairs_increase_percent) - 100) < 0.01,
  treasury_subcategories_2010_2025 |>
    group_by(closest_cofog_division) |>
    summarise(total = sum(contribution_to_mapped_group_increase_percent), .groups = "drop") |>
    summarise(ok = all(abs(total - 100) < 0.01)) |>
    pull(ok)
)

treasury_subcategory_availability <- tibble::tribble(
  ~closest_cofog_division, ~availability_note,
  "Environmental protection", "No comparable core-Crown subcategory decomposition is available for 2010: ETS expenses were then reported within heritage, culture and recreation.",
  "All divisions", "Treasury categories are related functional classes, not second-level COFOG; they exclude local government and may reflect classification changes between publication vintages."
)

# A small, transparent international panel for the macroeconomic backdrop.
wb_indicators <- tibble::tribble(
  ~indicator, ~indicator_name,
  "NY.GDP.MKTP.KD.ZG", "Real GDP growth",
  "NY.GDP.PCAP.KD.ZG", "Real GDP per capita growth",
  "NY.GDP.PCAP.PP.KD", "Real GDP per capita (PPP)",
  "NE.GDI.FTOT.ZS", "Gross fixed capital formation",
  "SP.POP.65UP.TO.ZS", "Population aged 65 and over",
  "BN.CAB.XOKA.GD.ZS", "Current account balance",
  "NE.TRD.GNFS.ZS", "Trade openness",
  "SL.UEM.TOTL.ZS", "Unemployment rate"
)

pull_world_bank <- function(indicator, indicator_name) {
  url <- paste0(
    "https://api.worldbank.org/v2/country/NZL;AUS;OED/indicator/", indicator,
    "?format=json&per_page=20000&date=1990:2026"
  )
  response <- httr2::request(url) |>
    httr2::req_user_agent("NZ fiscal risks research; R/httr2") |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()
  payload <- jsonlite::fromJSON(httr2::resp_body_string(response), flatten = TRUE)
  observations <- payload[[2]]
  if (is.null(observations) || !nrow(observations)) return(tibble())

  tibble(
    country = observations$country.value,
    country_code = observations$countryiso3code,
    year = as.integer(observations$date),
    indicator = indicator,
    indicator_name = indicator_name,
    value = as.numeric(observations$value)
  ) |>
    filter(!is.na(value))
}

world_bank_cache <- file.path(processed_dir, "world_bank_macro_comparison.csv")
world_bank <- if (!refresh && file.exists(world_bank_cache)) {
  message("Using cached file: ", basename(world_bank_cache))
  read_csv(world_bank_cache, show_col_types = FALSE)
} else {
  map2_dfr(
    wb_indicators$indicator,
    wb_indicators$indicator_name,
    pull_world_bank
  ) |>
    arrange(indicator_name, country_code, year)
}

write_csv(history_long, file.path(processed_dir, "treasury_fiscal_history_long.csv"), na = "")
write_csv(befu_fiscal, file.path(processed_dir, "befu_2026_fiscal.csv"), na = "")
write_csv(befu_economic, file.path(processed_dir, "befu_2026_economic.csv"), na = "")
write_csv(ltfs_long, file.path(processed_dir, "ltfs_2025_selected_figures.csv"), na = "")
write_csv(cofog_history, file.path(processed_dir, "stats_nz_cofog_history.csv"), na = "")
write_csv(cofog_growth_contributions, file.path(processed_dir, "stats_nz_cofog_growth_contributions.csv"), na = "")
write_csv(cofog_2010_2025, file.path(processed_dir, "stats_nz_cofog_2010_2025_decomposition.csv"), na = "")
write_csv(ltfm_functional, file.path(processed_dir, "treasury_ltfm_functional_projections.csv"), na = "")
write_csv(treasury_subcategories_2010_2025, file.path(processed_dir, "treasury_functional_subcategories_endpoint_decomposition.csv"), na = "")
write_csv(treasury_subcategory_availability, file.path(processed_dir, "treasury_functional_subcategory_notes.csv"), na = "")
write_csv(welfare_benefit_decomposition, file.path(processed_dir, "treasury_welfare_benefits_2010_2025_decomposition.csv"), na = "")
write_csv(welfare_recipient_counts, file.path(processed_dir, "treasury_welfare_recipient_counts_2010_2025.csv"), na = "")
write_csv(cofog_economic_affairs_expense_type, file.path(processed_dir, "stats_nz_economic_affairs_2010_2025_by_expense_type.csv"), na = "")
write_csv(treasury_economic_affairs_detail, file.path(processed_dir, "treasury_economic_affairs_subcategories_2010_2025.csv"), na = "")
write_csv(functional_concordance, file.path(processed_dir, "treasury_to_cofog_concordance.csv"), na = "")
write_csv(world_bank, file.path(processed_dir, "world_bank_macro_comparison.csv"), na = "")
write_csv(
  sources |>
    transmute(
      source_id,
      description,
      url,
      downloaded_file = file.path("raw", local_file),
      retrieved_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
    ),
  file.path(processed_dir, "source_manifest.csv")
)

message("Data pull complete. Tidy files written to: ", processed_dir)

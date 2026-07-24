suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- args[startsWith(args, "--file=")]
  if (length(hit)) return(dirname(normalizePath(sub("--file=", "", hit[[1]]))))
  normalizePath(getwd())
}

find_project_root <- function() {
  candidates <- unique(normalizePath(c(
    getwd(),
    file.path(getwd(), "2026 projections"),
    dirname(get_script_dir()),
    dirname(dirname(get_script_dir()))
  ), mustWork = FALSE))
  hit <- candidates[file.exists(file.path(candidates, "run_all.R"))]
  if (!length(hit)) stop("Could not locate the 2026 projections project root.", call. = FALSE)
  hit[[1]]
}

check_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Install required R packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

make_dirs <- function(paths) {
  invisible(lapply(paths, dir.create, recursive = TRUE, showWarnings = FALSE))
}

download_if_needed <- function(url, destination, refresh = FALSE) {
  if (file.exists(destination) && !isTRUE(refresh)) return(invisible(destination))
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  message("Downloading ", url)
  curl::curl_download(url, destination, quiet = FALSE, mode = "wb")
  if (!file.exists(destination) || file.info(destination)$size == 0) {
    stop("Download failed: ", url, call. = FALSE)
  }
  invisible(destination)
}

parse_fy_end <- function(x) {
  x <- as.character(x)
  first <- suppressWarnings(as.integer(substr(x, 1, 4)))
  ifelse(grepl("^[0-9]{4}-[0-9]{2}$", x), first + 1L, NA_integer_)
}

fy_end_year <- function(date) {
  date <- as.Date(date)
  as.integer(format(date, "%Y")) + as.integer(as.integer(format(date, "%m")) >= 7L)
}

annualise_complete <- function(dt, value_name, mode = c("sum", "mean"),
                               expected_periods, date_col = "date") {
  mode <- match.arg(mode)
  x <- copy(dt)
  x[, year := fy_end_year(get(date_col))]
  out <- x[, .(
    value = if (mode == "sum") sum(as.numeric(value), na.rm = TRUE) else mean(as.numeric(value), na.rm = TRUE),
    periods = uniqueN(as.Date(get(date_col)))
  ), by = year]
  out <- out[periods >= expected_periods]
  out[, periods := NULL]
  setnames(out, "value", value_name)
  out[order(year)]
}

prefer_sa <- function(dt) {
  x <- copy(dt)
  if (!nrow(x) || !"series_type" %in% names(x)) return(x)
  if (any(grepl("Seasonally Adjusted", x$series_type, ignore.case = TRUE))) {
    x <- x[grepl("Seasonally Adjusted", series_type, ignore.case = TRUE)]
  }
  x
}

dedup_series <- function(dt) {
  x <- copy(dt)
  if (!nrow(x)) return(x)
  x[, .(value = mean(as.numeric(value), na.rm = TRUE)), by = date]
}

zscore_with <- function(x, center, scale) {
  scale <- ifelse(is.finite(scale) && scale > 0, scale, 1)
  (as.numeric(x) - center) / scale
}

safe_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

read_abs_rds <- function(raw_dir, catalogue) {
  readRDS(file.path(raw_dir, paste0("abs_", gsub("\\.", "", catalogue), ".rds")))
}

extract_abs_national_accounts <- function(na_raw) {
  na <- as.data.table(na_raw)
  na[, date := as.Date(date)]
  na <- na[grepl("\\$\\s*Million", unit, ignore.case = TRUE)]
  text_cols <- intersect(c("series", "table_title", "unit", "data_type", "series_type"), names(na))
  na[, text := do.call(paste, c(.SD, sep = " | ")), .SDcols = text_cols]

  is_total_gg <- grepl("^\\s*General government\\s*[-;]", na$series, ignore.case = TRUE) &
    !grepl("National|State and local|Defence|Non[- ]?defence", na$series, ignore.case = TRUE)
  is_nom_table <- grepl("^\\s*Table\\s*3\\b", na$table_title, ignore.case = TRUE)
  is_real_table <- grepl("^\\s*Table\\s*2\\b", na$table_title, ignore.case = TRUE)
  is_gfce <- grepl("Final\\s+consumption\\s+expenditure\\s*;", na$series, ignore.case = TRUE)
  is_gfcf <- grepl("Gross\\s+fixed\\s+capital\\s+formation\\s*;", na$series, ignore.case = TRUE)
  is_gdp <- grepl("Gross domestic product|\\bGDP\\b", na$series, ignore.case = TRUE)
  not_change <- !grepl("Percentage changes|Index|Revisions", na$series, ignore.case = TRUE)

  get_flow <- function(mask, name, real = FALSE) {
    x <- prefer_sa(na[mask & not_change])
    x <- dedup_series(x)
    annualise_complete(x, name, "sum", 4L)
  }

  gfce_nom <- get_flow(is_nom_table & is_total_gg & is_gfce, "gfce_nom")
  gfcf_nom <- get_flow(is_nom_table & is_total_gg & is_gfcf, "gfcf_nom")
  gfce_real <- get_flow(is_real_table & is_total_gg & is_gfce, "gfce_real", TRUE)
  gdp_nom <- get_flow(is_nom_table & is_gdp, "gdp_nom")
  gdp_real <- get_flow(is_real_table & is_gdp, "gdp_real", TRUE)

  tip <- prefer_sa(na[
    table_no == "5206017_gen_govt_income_account" &
      series == "Total income payable ;"
  ])
  tip <- annualise_complete(dedup_series(tip), "income_payable_nom", "sum", 4L)

  out <- Reduce(function(x, y) merge(x, y, by = "year", all = TRUE),
                list(gfce_nom, gfcf_nom, gfce_real, gdp_nom, gdp_real, tip))
  setorder(out, year)
  out[, `:=`(
    broad_expenditure_nom = gfce_nom + gfcf_nom + income_payable_nom,
    broad_expenditure_gdp = (gfce_nom + gfcf_nom + income_payable_nom) / gdp_nom,
    gov_consumption_price = gfce_nom / gfce_real,
    gdp_price = gdp_nom / gdp_real
  )]
  out
}

extract_abs_age_shares <- function(pop_raw) {
  x <- as.data.table(pop_raw)
  x[, date := as.Date(date)]
  # Table 59 embeds sex in the series name. Using Persons only avoids summing
  # male + female + persons, which doubles the population level.
  x <- x[grepl(";\\s*Persons\\s*;", series, ignore.case = TRUE)]
  x[, age := suppressWarnings(as.integer(sub(".*;\\s*([0-9]+)( and over)?\\s*;\\s*$", "\\1", series)))]
  x <- x[!is.na(age)]
  x[, age_group := fcase(
    age <= 14, "0_14",
    age <= 34, "15_34",
    age <= 54, "35_54",
    age <= 64, "55_64",
    default = "65p"
  )]
  grouped <- x[, .(population = sum(as.numeric(value), na.rm = TRUE)),
               by = .(year = as.integer(format(date, "%Y")), age_group)]
  totals <- grouped[, .(pop_total = sum(population)), by = year]
  grouped <- merge(grouped, totals, by = "year")
  grouped[, share := population / pop_total]
  wide <- dcast(grouped, year + pop_total ~ age_group, value.var = "share")
  setcolorder(wide, c("year", "pop_total", "0_14", "15_34", "35_54", "55_64", "65p"))
  wide[order(year)]
}

extract_abs_terms_of_trade <- function(bop_raw) {
  x <- as.data.table(bop_raw)
  x[, date := as.Date(date)]
  x <- x[grepl("Terms of Trade", series, ignore.case = TRUE) &
           grepl("Goods and Services", series, ignore.case = TRUE)]
  x <- prefer_sa(x)
  annualise_complete(dedup_series(x), "tot_index", "mean", 4L)
}

extract_abs_unemployment <- function(lf_raw) {
  x <- as.data.table(lf_raw)
  x[, date := as.Date(date)]
  x <- x[series == "Unemployment rate ;  Persons ;"]
  x <- prefer_sa(x)
  annualise_complete(dedup_series(x), "unemployment", "mean", 12L)
}

extract_lf_unemployment_xlsx <- function(path) {
  raw <- as.data.table(readxl::read_excel(path, sheet = "Data1", col_names = FALSE))
  descriptions <- trimws(as.character(unlist(raw[1, -1, with = FALSE], use.names = FALSE)))
  types <- trimws(as.character(unlist(raw[3, -1, with = FALSE], use.names = FALSE)))
  hit <- which(descriptions == "Unemployment rate ;  Persons ;" & types == "Seasonally Adjusted")
  if (length(hit) != 1) stop("Could not identify seasonally adjusted persons unemployment rate.", call. = FALSE)
  excel_dates <- safe_num(raw[[1]][11:nrow(raw)])
  values <- safe_num(raw[[hit + 1L]][11:nrow(raw)])
  x <- data.table(
    date = as.Date(excel_dates, origin = "1899-12-30"),
    value = values
  )[!is.na(date) & !is.na(value)]
  annualise_complete(x, "unemployment", "mean", 12L)
}

read_gfs_sheet_long <- function(path, sheet) {
  raw <- as.data.table(readxl::read_excel(path, sheet = sheet, col_names = FALSE))
  years <- parse_fy_end(unlist(raw[5, -1, with = FALSE], use.names = FALSE))
  values <- raw[7:nrow(raw)]
  names(values) <- c("item", paste0("y", years))
  long <- melt(values, id.vars = "item", variable.name = "year_key", value.name = "value")
  long[, year := as.integer(sub("y", "", year_key))]
  long[, value := safe_num(value)]
  long <- long[!is.na(item) & !is.na(value)]
  long[, year_key := NULL]
  long[]
}

extract_pbo_metric <- function(raw, metric, level = "National total") {
  labels <- trimws(as.character(raw[[1]]))
  i <- which(labels == metric)
  if (length(i) != 1) stop("Could not uniquely find PBO metric: ", metric, call. = FALSE)
  window <- seq.int(i + 1L, min(i + 14L, nrow(raw)))
  j <- window[which(labels[window] == level)]
  if (length(j) != 1) stop("Could not find ", level, " below ", metric, call. = FALSE)
  years <- parse_fy_end(unlist(raw[i, -1, drop = FALSE], use.names = FALSE))
  values <- safe_num(unlist(raw[j, -1, drop = FALSE], use.names = FALSE))
  data.table(year = years, value = values)[!is.na(year) & !is.na(value)]
}

parse_population_projection <- function(path) {
  raw <- as.data.table(readxl::read_excel(path, sheet = "PROJECTIONS"))
  setnames(raw, janitor_names(names(raw)))
  year_col <- names(raw)[grepl("financial_year", names(raw))][1]
  geo_col <- names(raw)[grepl("geography", names(raw))][1]
  sex_col <- names(raw)[grepl("^sex$", names(raw))][1]
  age_col <- names(raw)[grepl("^age$", names(raw))][1]
  value_col <- names(raw)[grepl("^value$", names(raw))][1]
  x <- raw[get(geo_col) == "Australia" & get(sex_col) == "Persons"]
  x[, `:=`(
    year = parse_fy_end(get(year_col)),
    age = safe_num(get(age_col)),
    population = safe_num(get(value_col))
  )]
  x <- x[!is.na(year) & !is.na(age) & !is.na(population)]
  x[, age_group := fcase(
    age <= 14, "0_14", age <= 34, "15_34", age <= 54, "35_54",
    age <= 64, "55_64", default = "65p"
  )]
  grouped <- x[, .(population = sum(population)), by = .(year, age_group)]
  totals <- grouped[, .(pop_total = sum(population)), by = year]
  grouped <- merge(grouped, totals, by = "year")
  grouped[, share := population / pop_total]
  dcast(grouped, year + pop_total ~ age_group, value.var = "share")[order(year)]
}

interpolate_projection <- function(dt, years) {
  out <- data.table(year = years)
  for (col in setdiff(names(dt), "year")) {
    out[, (col) := approx(dt$year, dt[[col]], xout = years, rule = 2)$y]
  }
  out
}

janitor_names <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  gsub("^_|_$", "", x)
}

save_plot_pair <- function(plot, path_without_extension, width = 9, height = 6) {
  dir.create(dirname(path_without_extension), recursive = TRUE, showWarnings = FALSE)
  ggsave(paste0(path_without_extension, ".png"), plot, width = width, height = height,
         units = "in", dpi = 240, bg = "white")
  ggsave(paste0(path_without_extension, ".svg"), plot, width = width, height = height,
         units = "in", bg = "white")
}

theme_fiscal <- function() {
  theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title.position = "plot",
      plot.caption = element_text(hjust = 0, colour = "grey35")
    )
}

write_validation <- function(check, passed, detail, output_file) {
  row <- data.table(check = check, passed = isTRUE(passed), detail = as.character(detail))
  if (file.exists(output_file)) {
    old <- fread(output_file)
    row <- rbind(old[check != row$check], row, fill = TRUE)
  }
  fwrite(row, output_file)
  if (!isTRUE(passed)) warning("Validation failed: ", check, " - ", detail, call. = FALSE)
  invisible(row)
}

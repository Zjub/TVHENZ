source(file.path("scripts", "00_config.R"))
check_packages(c("curl", "readabs", "data.table", "jsonlite"))

files <- c(
  gfs_annual = file.path(raw_dir, "abs_gfs_annual_2024_25.xlsx"),
  population_projection = file.path(raw_dir, "population_statement_2025_age_sex.xlsx"),
  pbo_nfo = file.path(raw_dir, "pbo_national_fiscal_outlook_2026_27.xlsx"),
  budget_nominal_gdp = file.path(raw_dir, "budget_2026_27_nominal_gdp.xlsx"),
  budget_receipts_cash = file.path(raw_dir, "budget_2026_27_cash_receipts.csv"),
  budget_receipts_gdp = file.path(raw_dir, "budget_2026_27_receipts_gdp.csv"),
  labour_force_table1 = file.path(raw_dir, "abs_labour_force_may_2026_table1.xlsx")
)

for (name in names(files)) {
  download_if_needed(source_urls[[name]], files[[name]], refresh_data)
}

abs_cache <- file.path(raw_dir, "abs_cache")
dir.create(abs_cache, recursive = TRUE, showWarnings = FALSE)

download_abs <- function(catalogue, tables = "all") {
  destination <- file.path(raw_dir, paste0("abs_", gsub("\\.", "", catalogue), ".rds"))
  if (file.exists(destination) && !refresh_data) return(invisible(destination))
  message("Downloading ABS catalogue ", catalogue, ", tables ", paste(tables, collapse = ", "))
  value <- readabs::read_abs(
    catalogue,
    tables = tables,
    path = abs_cache,
    check_local = !refresh_data
  )
  saveRDS(value, destination)
  invisible(destination)
}

download_abs("5206.0", "all")
download_abs("3101.0", "59")
download_abs("5302.0", "5")

manifest <- rbindlist(lapply(names(files), function(name) {
  info <- file.info(files[[name]])
  data.table(
    source = name,
    url = source_urls[[name]],
    local_file = normalizePath(files[[name]], winslash = "/"),
    bytes = info$size,
    downloaded_or_checked = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
}), fill = TRUE)

manifest <- rbind(
  manifest,
  data.table(
    source = c("abs_5206", "abs_3101_table59", "abs_5302_table5"),
    url = c(
      "ABS Australian National Accounts 5206.0",
      "ABS National, state and territory population 3101.0 Table 59",
      "ABS Balance of Payments 5302.0 Table 5"
    ),
    local_file = normalizePath(file.path(raw_dir, c("abs_52060.rds", "abs_31010.rds", "abs_53020.rds")), winslash = "/"),
    bytes = file.info(file.path(raw_dir, c("abs_52060.rds", "abs_31010.rds", "abs_53020.rds")))$size,
    downloaded_or_checked = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  ), fill = TRUE
)

fwrite(manifest, file.path(raw_dir, "source_manifest.csv"))
message("Raw data and source manifest written to: ", raw_dir)

source(file.path("scripts", "00_config.R"))
check_packages(c("readabs", "data.table"))

# The enhanced bottom-up model uses official public-sector wage-price series.
# Table 5a contains financial-year indexes by industry and is downloaded from
# the ABS latest-release endpoint by readabs. The raw extract is cached so the
# model remains reproducible when REFRESH_DATA is false.
wpi_rds <- file.path(raw_dir, "abs_wpi_public_industry.rds")
if (!file.exists(wpi_rds) || refresh_data) {
  message("Downloading ABS Wage Price Index catalogue 6345.0, table 5a")
  wpi_raw <- readabs::read_abs(
    "6345.0", tables = "5a", path = file.path(raw_dir, "abs_cache"),
    check_local = !refresh_data
  )
  saveRDS(wpi_raw, wpi_rds)
} else {
  wpi_raw <- readRDS(wpi_rds)
}

wpi <- as.data.table(wpi_raw)
wpi[, date := as.Date(date)]
wpi <- wpi[
  frequency == "Annual" &
    grepl("^Financial Year Index", series) &
    grepl("Australia ;\\s+Public ;", series)
]
wpi[, driver := fcase(
  grepl("Health care and social assistance", series), "public_health_wpi",
  grepl("Education and training", series), "public_education_wpi",
  grepl("Public administration and safety", series), "public_admin_wpi",
  grepl("All industries", series), "public_all_wpi",
  default = NA_character_
)]
wpi[, year := as.integer(format(date, "%Y"))]
wpi <- wpi[!is.na(driver), .(
  index = mean(as.numeric(value), na.rm = TRUE),
  abs_series_id = paste(unique(series_id), collapse = ";")
), by = .(driver, year)]
setorder(wpi, driver, year)
wpi[, growth := index / shift(index) - 1, by = driver]
wpi[, `:=`(
  source = "ABS Wage Price Index, Australia, catalogue 6345.0 table 5a",
  source_url = "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/wage-price-index-australia/latest-release",
  downloaded = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
)]

required <- c("public_health_wpi", "public_education_wpi", "public_admin_wpi", "public_all_wpi")
missing <- setdiff(required, unique(wpi$driver))
if (length(missing)) stop("Missing required WPI drivers: ", paste(missing, collapse = ", "), call. = FALSE)
if (wpi[, max(year)] < 2025L) stop("The downloaded WPI series does not cover 2024-25.", call. = FALSE)

fwrite(wpi, file.path(processed_dir, "bottom_up_external_price_drivers.csv"))

manifest_file <- file.path(raw_dir, "source_manifest.csv")
manifest <- if (file.exists(manifest_file)) fread(manifest_file) else data.table()
manifest <- manifest[source != "abs_wpi_public_industry"]
manifest <- rbind(manifest, data.table(
  source = "abs_wpi_public_industry",
  url = unique(wpi$source_url),
  local_file = normalizePath(wpi_rds, winslash = "/"),
  bytes = file.info(wpi_rds)$size,
  downloaded_or_checked = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
), fill = TRUE)
fwrite(manifest, manifest_file)
message("Online bottom-up price drivers written to: ", processed_dir)

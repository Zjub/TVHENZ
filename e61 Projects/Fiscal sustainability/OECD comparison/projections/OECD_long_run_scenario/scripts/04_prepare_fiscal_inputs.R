suppressPackageStartupMessages({library(data.table); library(jsonlite); library(readxl); library(ggplot2)})
root <- Sys.getenv("OECD_LTS_ROOT")
stopifnot(nzchar(root))
source(file.path(root, "R", "fiscal_functions.R"))
raw <- file.path(root, "data", "raw", "fiscal")
processed <- file.path(root, "data", "processed", "fiscal")
out <- file.path(root, "outputs", "fiscal")
fig <- file.path(out, "figures")
for (p in c(processed, out, fig)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
parent_data <- file.path(dirname(root), "2026 projections", "data", "processed")
base_year <- 2024L
end_year <- 2100L

# WPP 2024 is annual despite its five-year age bands. The cached Australian
# extract avoids repeatedly parsing the large worldwide workbook.
wpp_path <- file.path(processed, "un_wpp2024_australia_age_groups.csv")
if (!file.exists(wpp_path)) {
  wpp <- rbindlist(lapply(c("Estimates", "Medium variant"), function(sheet) {
    x <- as.data.table(read_excel(file.path(raw, "wpp2024_age_groups.xlsx"),
                                 sheet = sheet, skip = 16, col_types = "text", .name_repair = "unique_quiet"))
    # Header row varies in WPP exports; locate via the actual column names.
    if (!"Year" %in% names(x) || !"ISO3 Alpha-code" %in% names(x)) {
      x <- as.data.table(read_excel(file.path(raw, "wpp2024_age_groups.xlsx"),
                                   sheet = sheet, skip = 12, col_types = "text", .name_repair = "unique_quiet"))
    }
    x <- x[get("ISO3 Alpha-code") == "AUS"]
    stopifnot(nrow(x) > 0)
    age <- grep("^([0-9]+-[0-9]+|100\\+)$", names(x), value = TRUE)
    stopifnot(length(age) == 21L)
    x <- x[, c("Year", age), with = FALSE]
    setnames(x, "Year", "year")
    x[, year := as.integer(year)]
    x[, (age) := lapply(.SD, function(z) as.numeric(z) * 1000), .SDcols = age]
    x[, variant := sheet]
    x
  }))
  setorder(wpp, year)
  stopifnot(!anyDuplicated(wpp$year))
  fwrite(wpp, wpp_path)
} else wpp <- fread(wpp_path)
age <- grep("^([0-9]+-[0-9]+|100\\+)$", names(wpp), value = TRUE)
wpp[, un_population := rowSums(.SD), .SDcols = age]
sum_age <- function(minimum, maximum = 110) {
  starts <- as.numeric(sub("[-+].*$", "", age))
  rowSums(wpp[, .SD, .SDcols = age[starts >= minimum & starts < maximum]])
}
wpp[, `:=`(share55_64 = sum_age(55, 65) / un_population,
           share65_74 = sum_age(65, 75) / un_population,
           share75p = sum_age(75) / un_population,
           share65p = sum_age(65) / un_population,
           share5_24 = sum_age(5, 25) / un_population,
           share15_74 = sum_age(15, 75) / un_population)]

macro <- fread(file.path(root, "data", "processed", "oecd_lts_australia_long.csv"))
extra <- json_series(file.path(raw, "eo117_all_australia_bau1.json"))
extra <- extra[, .(scenario = SCENARIO, measure = MEASURE, year, value)]
macro <- unique(rbindlist(list(macro[, .(scenario, measure, year, value)], extra)),
                by = c("scenario", "measure", "year"), fromLast = TRUE)
fwrite(extra, file.path(processed, "eo117_additional_bau1_series.csv"))
fwrite(data.table(available_measure = sort(unique(extra$measure))),
       file.path(processed, "eo117_available_bau1_measures.csv"))

# Freeze observed age-specific employment at the last pre-anchor observation.
rates <- rbindlist(lapply(c("Y55T64", "Y65T74"), function(a) {
  d <- json_series(file.path(raw, paste0("lfs_", a, ".json")))
  stopifnot(uniqueN(d$series_code) == 1L)
  d[, .(year, age = a, employment_rate = value / 100)]
}))
fwrite(rates, file.path(processed, "oecd_older_employment_history.csv"))
er55 <- rates[age == "Y55T64" & year == base_year, employment_rate]
er65 <- rates[age == "Y65T74" & year == base_year, employment_rate]
stopifnot(length(er55) == 1, length(er65) == 1)
drivers <- dcast(macro[measure %in% c("GDPVTRD", "GDPVTRD_CAP", "ETPT", "POPS", "ERS1574",
                                     "RATE_GGINTP", "GDP", "PGDP", "GGFLQ", "GNFLQ", "NLGQ", "NLGXQ")],
                 scenario + year ~ measure, value.var = "value")
drivers <- merge(drivers, wpp[, .(year, un_population, share55_64, share65_74, share75p, share65p, share15_74, share5_24)],
                 by = "year", all.x = TRUE)
drivers <- drivers[year >= base_year & year <= end_year]
setorder(drivers, scenario, year)
drivers[, `:=`(pop = POPS, employment = ETPT, real_gdp = GDPVTRD,
                employment_rate = ERS1574 / 100, er55_64 = er55, er65_74 = er65)]
# Non-BAU1 nominal paths use each scenario's real output and the BAU1 deflator
# growth. This is a stated partial-equilibrium assumption, not OECD nominal data.
nom <- drivers[scenario == "BAU1", .(year, nominal_bau1 = GDP[year == base_year] *
  real_gdp / real_gdp[year == base_year] * PGDP / PGDP[year == base_year], real_bau1 = real_gdp)]
drivers <- merge(drivers, nom, by = "year")
drivers[, nominal_gdp := nominal_bau1 * real_gdp / real_bau1]
drivers[, interest_rate := RATE_GGINTP / 100]
stopifnot(all(is.finite(drivers$real_gdp)), all(is.finite(drivers$share75p)),
          all(drivers$employment_rate > 0 & drivers$employment_rate < 1))
fwrite(drivers, file.path(processed, "fiscal_projection_drivers.csv"))

# SHA HF1 equals public financing for Australia (no compulsory contributory
# insurance). Select total functions, total providers; do not sum overlapping
# functions and provider subtotals or add HC3 a second time.
sha <- json_series(file.path(raw, "sha_australia.json"))
health <- sha[FUNCTION == "_T" & PROVIDER == "_T" & MODE_PROVISION == "_T" &
                year <= base_year & is.finite(value)]
stopifnot(uniqueN(health$series_code) == 1L)
health <- health[which.max(year)]

socx <- json_series(file.path(raw, "socx_australia.json"))
pension_codes <- c("TP111", "TP112", "TP211", "TP311")
pensions <- socx[SPENDING_TYPE == "C" & PROGRAMME_TYPE %in% pension_codes]
latest <- pensions[is.finite(value), max(year)]
pension_parts <- pensions[year == latest, .(programme = PROGRAMME_TYPE, year, ratio = value / 100)]
# TP112 exists but has no observations for Australia; explicit structural-zero
# treatment is recorded instead of silently dropping NA from a general sum.
pension_parts[, missing_treated_zero := !is.finite(ratio)]
pension_parts[missing_treated_zero == TRUE, ratio := 0]
stopifnot(nrow(pension_parts) == 4L)
fwrite(pension_parts, file.path(processed, "pension_anchor_components.csv"))
# Roll the 2022 SOCX share forward with the same pension equation, using actual
# 2022/2024 LFS rates. This is an extrapolation, not an observed 2024 pension share.
get_retired <- function(y) {
  a <- wpp[year == y]
  m <- macro[scenario == "BAU1" & year == y]
  n <- m[measure == "POPS", value]
  e <- m[measure == "ETPT", value]
  n * (a$share55_64 * (1 - rates[age == "Y55T64" & year == y, employment_rate]) +
       a$share65_74 * (1 - rates[age == "Y65T74" & year == y, employment_rate]) + a$share75p) / e
}
pension_ratio <- sum(pension_parts$ratio) * get_retired(base_year) / get_retired(latest)

# Public consolidated levels from the local PBO historical extract. They are
# financial-year observations labelled by ending year. Calendar 2024 is paired
# with FY2023-24 as an explicit annual approximation, not an exact vintage match.
pbo_cache <- file.path(raw, "pbo_historical_and_forecast_snapshot.csv")
if (!file.exists(pbo_cache)) {
  stopifnot(file.copy(file.path(parent_data, "official_pbo_nfo_wide.csv"), pbo_cache))
}
official <- fread(pbo_cache)
a <- official[year == base_year]
oecd_b <- drivers[scenario == "BAU1" & year == base_year]
prior_gdp <- macro[scenario == "BAU1" & measure == "GDP" & year == base_year - 1L, value]
g0 <- oecd_b$nominal_gdp / prior_gdp - 1
prior_debt <- macro[scenario == "BAU1" & measure == "GGFLQ" & year == base_year - 1L, value] / 100
anchor <- list(health_ratio = health$value / 100, pension_ratio = pension_ratio,
  capital_ratio = a$net_capital_investment_ratio_gdp,
  gross_liabilities_ratio = oecd_b$GGFLQ / 100,
  assets_ratio = (oecd_b$GGFLQ - oecd_b$GNFLQ) / 100,
  interest_ratio = oecd_b$interest_rate * prior_debt / (1 + g0))
# Keep the OECD actual net-lending identity in the initial balance. Do not force
# the PBO revenue concept to close an OECD financial-liability balance sheet.
anchor$interest_receipts_ratio <- (oecd_b$NLGQ - oecd_b$NLGXQ) / 100 + anchor$interest_ratio
anchor$other_ratio <- a$expenses_ratio_gdp - anchor$interest_ratio - anchor$health_ratio - anchor$pension_ratio
anchor$primary_revenue_ratio <- oecd_b$NLGXQ / 100 +
  anchor$health_ratio + anchor$pension_ratio + anchor$other_ratio + anchor$capital_ratio
anchor$asset_return <- anchor$interest_receipts_ratio * (1 + g0) / anchor$assets_ratio
stopifnot(all(is.finite(unlist(anchor))), anchor$other_ratio > 0, anchor$assets_ratio >= 0)
anchor_table <- data.table(variable = names(anchor), value = unlist(anchor))
anchor_table[, status := fifelse(variable == "health_ratio", "Observed OECD SHA 2024",
  fifelse(variable == "pension_ratio", "OECD SOCX 2022 rolled forward to 2024",
  fifelse(variable %in% c("gross_liabilities_ratio", "assets_ratio"), "Published EO117 2024",
  fifelse(variable == "capital_ratio", "PBO FY2023-24 historical extract",
  fifelse(variable == "interest_ratio", "EO117 implicit rate times lagged gross liabilities/current GDP",
          "Accounting residual or implied parameter; see report")))))]
fwrite(anchor_table, file.path(processed, "fiscal_anchor.csv"))

benchmark <- as.data.table(read_excel(file.path(raw, "australia_2026_figure_1_14.xlsx"),
                                    sheet = "fig1-14", range = "A31:M63", col_names = FALSE,
                                    .name_repair = "unique_quiet"))
z <- benchmark[benchmark[[2]] == "AUS"]
benchmark_spending <- data.table(component = c("health_ratio", "pension_ratio", "other_ratio",
                   "net_interest", "other_factors", "total_pressure"),
                  published_change_pp = as.numeric(unlist(z[1, 3:8])), start_year = 2024L, end_year = 2060L)
fwrite(benchmark_spending, file.path(processed, "published_spending_benchmark.csv"))
bd <- benchmark[, .(year = suppressWarnings(as.integer(.SD[[10]])),
  published_no_adjustment = suppressWarnings(as.numeric(.SD[[12]])),
  published_adjustment = suppressWarnings(as.numeric(.SD[[13]])))]
bd <- bd[is.finite(year)]
fwrite(bd, file.path(processed, "published_debt_benchmark.csv"))

source_files <- list.files(raw, full.names = TRUE)
manifest <- data.table(file = basename(source_files), bytes = file.info(source_files)$size,
                       md5 = unname(tools::md5sum(source_files)))
fwrite(manifest, file.path(processed, "source_file_checksums.csv"))
message("Prepared independent fiscal inputs and extracted published benchmarks.")

source(file.path("scripts", "00_config.R"))

gfs_purpose <- fread(file.path(processed_dir, "historical_gfs_expenses_by_purpose.csv"))
gfs_operating <- fread(file.path(processed_dir, "historical_gfs_operating_statement.csv"))
macro <- fread(file.path(processed_dir, "macro_scenario_assumptions.csv"))
category_assumptions <- fread(file.path(processed_dir, "bottom_up_category_assumptions.csv"))
scenario_parameters <- fread(file.path(processed_dir, "scenario_parameters.csv"))
official <- fread(file.path(processed_dir, "official_pbo_nfo_wide.csv"))
wpi <- fread(file.path(processed_dir, "bottom_up_external_price_drivers.csv"))
pop_hist <- fread(file.path(processed_dir, "historical_age_shares.csv"))
pop_proj <- fread(file.path(processed_dir, "population_projection_age_shares.csv"))
baseline_categories <- fread(file.path(table_dir, "bottom_up_category_projections.csv"))
baseline_totals <- fread(file.path(table_dir, "bottom_up_total_projections.csv"))

base_year <- 2025L
`%||%` <- function(x, y) if (length(x) && !is.na(x)) x else y

build_exposure <- function(dt, exposure) {
  if (exposure == "total_population") return(dt$pop_total)
  if (exposure == "health_age_weight") return(dt$pop_total * (
    0.65 * dt$`0_14` + 0.75 * dt$`15_34` + 0.90 * dt$`35_54` +
      1.30 * dt$`55_64` + 3.00 * dt$`65p`))
  if (exposure == "education_age_weight") return(dt$pop_total * (
    dt$`0_14` + 0.35 * dt$`15_34` + 0.05 * dt$`35_54`))
  if (exposure == "social_age_weight") return(dt$pop_total * (
    0.60 * dt$`0_14` + 0.50 * dt$`15_34` + 0.40 * dt$`35_54` +
      0.80 * dt$`55_64` + 2.20 * dt$`65p`))
  rep(NA_real_, nrow(dt))
}

# Observed age shares are used through 2024 and the published projection from
# 2025. Duplicate years deliberately prefer the projection.
age_all <- rbindlist(list(pop_hist, pop_proj), fill = TRUE)
setorder(age_all, year)
age_all <- age_all[, .SD[.N], by = year]

driver_map <- data.table(
  category = category_assumptions$category,
  price_driver = c(
    "public_admin_wpi", NA, "public_admin_wpi", "public_all_wpi",
    "public_all_wpi", "public_all_wpi", "public_health_wpi",
    "public_all_wpi", "public_education_wpi", "public_health_wpi",
    "public_all_wpi"
  )
)

# Reconstruct general public services excluding interest in every historical
# year, matching the baseline's primary-spending concept.
hist <- copy(gfs_purpose)
interest <- gfs_operating[item == "Interest expenses n.e.c.", .(year, interest = value)]
hist <- merge(hist, interest, by = "year", all.x = TRUE)
hist[item == "General public services", `:=`(
  item = "General public services excl interest",
  value = value - fifelse(is.na(interest), 0, interest)
)]
hist <- merge(hist, category_assumptions[, .(category, exposure)], by.x = "item", by.y = "category")
hist <- merge(hist, age_all, by = "year", all.x = TRUE)
hist[, exposure_value := build_exposure(.SD, exposure), by = .(item, exposure)]
hist <- merge(hist, driver_map, by.x = "item", by.y = "category", all.x = TRUE)
price_hist <- wpi[, .(year, price_driver = driver, price_index = index, price_growth = growth)]
hist <- merge(hist, price_hist, by = c("year", "price_driver"), all.x = TRUE)
all_wpi <- wpi[driver == "public_all_wpi", .(year, all_wpi_growth = growth)]
hist <- merge(hist, all_wpi, by = "year", all.x = TRUE)
setorder(hist, item, year)
hist[, `:=`(
  spending_growth = value / shift(value) - 1,
  exposure_growth = exposure_value / shift(exposure_value) - 1
), by = item]
hist[, `:=`(
  relative_price_premium = price_growth - all_wpi_growth,
  residual_intensity_growth = spending_growth - exposure_growth - price_growth,
  observed_excess_over_common_cost = spending_growth - exposure_growth - all_wpi_growth
)]

# Exclude 2019-20 to 2021-22 from the central calibration. COVID and temporary
# programs otherwise dominate a sample that contains only ten GFS observations.
calibration <- hist[year >= 2017 & !year %in% 2020:2022 & !is.na(observed_excess_over_common_cost), .(
  observations = .N,
  historical_relative_price_premium = median(relative_price_premium, na.rm = TRUE),
  historical_residual_intensity_growth = median(residual_intensity_growth, na.rm = TRUE),
  historical_total_excess = median(observed_excess_over_common_cost, na.rm = TRUE)
), by = .(category = item, price_driver)]
calibration[!is.finite(historical_relative_price_premium), historical_relative_price_premium := 0]
calibration[!is.finite(historical_residual_intensity_growth), historical_residual_intensity_growth := 0]
calibration[!is.finite(historical_total_excess), historical_total_excess := 0]
calibration <- merge(calibration, category_assumptions[, .(category, baseline_central_excess = central_excess_cost)], by = "category")

# A 35 per cent evidence weight recognises the short, break-prone GFS panel.
# The empirical target is winsorised to +/-2 percentage points per year.
calibration[, `:=`(
  evidence_weight = 0.35,
  capped_historical_excess = pmax(pmin(historical_total_excess, 0.02), -0.02)
)]
calibration[, enhanced_central_excess :=
  (1 - evidence_weight) * baseline_central_excess + evidence_weight * capped_historical_excess]
calibration[, long_run_central_excess :=
  0.5 * enhanced_central_excess + 0.5 * baseline_central_excess]
calibration[, method := "35% shrinkage of non-COVID median historical excess toward baseline; adjustment half-damped by 2040"]
fwrite(calibration, file.path(table_dir, "bottom_up_data_enhanced_calibration.csv"))
fwrite(hist[, .(
  year, category = item, exposure, price_driver, spending_growth,
  exposure_growth, price_growth, all_wpi_growth, relative_price_premium,
  residual_intensity_growth, observed_excess_over_common_cost
)], file.path(table_dir, "bottom_up_data_enhanced_historical_decomposition.csv"))

base <- gfs_purpose[year == base_year, .(category = item, value)]
interest_base <- gfs_operating[year == base_year & item == "Interest expenses n.e.c.", value]
base[category == "General public services", `:=`(
  category = "General public services excl interest", value = value - interest_base
)]
base <- merge(base, category_assumptions, by = "category", all.x = TRUE)
base <- merge(base, calibration[, .(category, enhanced_central_excess, long_run_central_excess)], by = "category", all.x = TRUE)

official_anchor <- official[year >= base_year & year <= official_forecast_end, .(
  year, primary_expense_ratio = expenses_ratio_gdp - public_debt_interest_ratio_gdp,
  nominal_gdp = nominal_gdp_billion * 1000
)]

project_enhanced <- function(scenario_name) {
  drv <- macro[scenario == scenario_name]
  params <- scenario_parameters[scenario == scenario_name]
  gdp <- merge(data.table(year = base_year:projection_end), official_anchor[, .(year, nominal_gdp)], by = "year", all.x = TRUE)
  gdp[year == base_year & is.na(nominal_gdp), nominal_gdp := gfs_purpose[year == base_year, unique(gdp_nom)]]
  for (i in 2:nrow(gdp)) if (is.na(gdp$nominal_gdp[i])) {
    growth <- params$nominal_gdp_growth_long_run %||% drv[year == gdp$year[i], nominal_gdp_growth_long_run]
    gdp$nominal_gdp[i] <- gdp$nominal_gdp[i - 1] * (1 + growth)
  }

  rows <- vector("list", nrow(base))
  for (j in seq_len(nrow(base))) {
    b <- base[j]
    path <- merge(data.table(year = base_year:projection_end), drv, by = "year", all.x = TRUE)
    path <- merge(path[, !c("pop_total", "0_14", "15_34", "35_54", "55_64", "65p"), with = FALSE], pop_proj, by = "year", all.x = TRUE)
    path <- merge(path, gdp, by = "year", all.x = TRUE)
    path[, exposure_value := build_exposure(.SD, b$exposure)]
    path[, `:=`(value = NA_real_, applied_excess_cost = NA_real_)]
    path[year == base_year, value := b$value]

    if (b$exposure == "gdp_target") {
      base_share <- b$value / gdp[year == base_year, nominal_gdp]
      path[, target_share := approx(
        c(base_year, params$defence_target_year), c(base_share, params$defence_target_gdp),
        xout = year, rule = 2
      )$y]
      path[, value := nominal_gdp * target_share]
    } else {
      baseline_scenario <- b[[paste0(scenario_name, "_excess_cost")]]
      scenario_offset <- baseline_scenario - b$central_excess_cost
      path[, empirical_central := approx(
        c(base_year, 2040L, projection_end),
        c(b$enhanced_central_excess, b$long_run_central_excess, b$long_run_central_excess),
        xout = year, rule = 2
      )$y]
      path[, applied_excess_cost := empirical_central + scenario_offset]
      for (i in 2:nrow(path)) {
        exposure_growth <- path$exposure_value[i] / path$exposure_value[i - 1] - 1
        population_growth <- path$pop_total[i] / path$pop_total[i - 1] - 1
        gdp_growth <- path$nominal_gdp[i] / path$nominal_gdp[i - 1] - 1
        productivity_growth <- max(gdp_growth - path$inflation[i] - population_growth, 0)
        unit_cost_growth <- path$inflation[i] + productivity_growth + path$applied_excess_cost[i]
        path$value[i] <- path$value[i - 1] * (1 + exposure_growth) * (1 + unit_cost_growth)
      }
    }
    rows[[j]] <- path[, .(
      year, scenario = scenario_name, category = b$category, value,
      nominal_gdp, applied_excess_cost
    )]
  }

  out <- rbindlist(rows)
  totals <- out[, .(raw_primary = sum(value), nominal_gdp = unique(nominal_gdp)), by = year]
  totals <- merge(totals, official_anchor[, .(year, primary_expense_ratio)], by = "year", all.x = TRUE)
  totals[, scale_factor := fifelse(!is.na(primary_expense_ratio), primary_expense_ratio * nominal_gdp / raw_primary, 1)]
  last_scale <- totals[year == official_forecast_end, scale_factor]
  totals[year > official_forecast_end, scale_factor := last_scale]
  out <- merge(out, totals[, .(year, scale_factor)], by = "year")
  out[, `:=`(
    raw_value = value, raw_share_gdp = value / nominal_gdp,
    value = value * scale_factor, share_gdp = value * scale_factor / nominal_gdp
  )]
  out[, scale_factor := NULL]
  out
}

enhanced <- rbindlist(lapply(c("central", "pressure", "restraint"), project_enhanced))
enhanced_totals <- enhanced[, .(
  primary_expense_value = sum(value), primary_expense_ratio = sum(share_gdp),
  raw_primary_expense_value = sum(raw_value), raw_primary_expense_ratio = sum(raw_share_gdp),
  nominal_gdp = unique(nominal_gdp)
), by = .(year, scenario)]

comparison <- merge(
  baseline_totals[, .(year, scenario, baseline_ratio = primary_expense_ratio)],
  enhanced_totals[, .(year, scenario, data_enhanced_ratio = primary_expense_ratio)],
  by = c("year", "scenario")
)
comparison[, difference_pp := (data_enhanced_ratio - baseline_ratio) * 100]
category_comparison <- merge(
  baseline_categories[, .(year, scenario, category, baseline_share_gdp = share_gdp)],
  enhanced[, .(year, scenario, category, data_enhanced_share_gdp = share_gdp)],
  by = c("year", "scenario", "category")
)
category_comparison[, difference_pp := (data_enhanced_share_gdp - baseline_share_gdp) * 100]

fwrite(enhanced, file.path(table_dir, "bottom_up_data_enhanced_category_projections.csv"))
fwrite(enhanced_totals, file.path(table_dir, "bottom_up_data_enhanced_total_projections.csv"))
fwrite(comparison, file.path(table_dir, "bottom_up_method_comparison.csv"))
fwrite(category_comparison, file.path(table_dir, "bottom_up_method_category_comparison.csv"))
message("Data-enhanced bottom-up projections and baseline comparisons written.")

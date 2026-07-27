source(file.path("scripts", "00_config.R"))

gfs_purpose <- fread(file.path(processed_dir, "historical_gfs_expenses_by_purpose.csv"))
gfs_operating <- fread(file.path(processed_dir, "historical_gfs_operating_statement.csv"))
macro <- fread(file.path(processed_dir, "macro_scenario_assumptions.csv"))
category_assumptions <- fread(file.path(processed_dir, "bottom_up_category_assumptions.csv"))
scenario_parameters <- fread(file.path(processed_dir, "scenario_parameters.csv"))
official <- fread(file.path(processed_dir, "official_pbo_nfo_wide.csv"))

base_year <- 2025L
base <- gfs_purpose[year == base_year, .(category = item, value)]
interest_base <- gfs_operating[year == base_year & item == "Interest expenses n.e.c.", value]
base[category == "General public services", `:=`(
  category = "General public services excl interest",
  value = value - interest_base
)]
base <- merge(base, category_assumptions, by = "category", all.x = TRUE)
if (anyNA(base$exposure)) stop("Missing bottom-up assumptions for: ", paste(base[is.na(exposure), category], collapse = ", "))

official_anchor <- official[year >= 2025 & year <= official_forecast_end, .(
  year,
  total_expense_ratio = expenses_ratio_gdp,
  interest_ratio = public_debt_interest_ratio_gdp,
  primary_expense_ratio = expenses_ratio_gdp - public_debt_interest_ratio_gdp,
  nominal_gdp = nominal_gdp_billion * 1000
)]

build_exposure <- function(dt, exposure) {
  if (exposure == "total_population") return(dt$pop_total)
  if (exposure == "health_age_weight") {
    return(dt$pop_total * (0.65 * dt$`0_14` + 0.75 * dt$`15_34` +
      0.90 * dt$`35_54` + 1.30 * dt$`55_64` + 3.00 * dt$`65p`))
  }
  if (exposure == "education_age_weight") {
    return(dt$pop_total * (dt$`0_14` + 0.35 * dt$`15_34` + 0.05 * dt$`35_54`))
  }
  if (exposure == "social_age_weight") {
    return(dt$pop_total * (0.60 * dt$`0_14` + 0.50 * dt$`15_34` +
      0.40 * dt$`35_54` + 0.80 * dt$`55_64` + 2.20 * dt$`65p`))
  }
  rep(NA_real_, nrow(dt))
}

project_scenario <- function(scenario) {
  scenario_name <- scenario
  drv <- macro[scenario == scenario_name]
  params <- scenario_parameters[scenario == scenario_name]
  cost_col <- paste0(scenario, "_excess_cost")
  gdp <- data.table(year = base_year:projection_end)
  gdp <- merge(gdp, official_anchor[, .(year, nominal_gdp)], by = "year", all.x = TRUE)
  gdp[year == base_year & is.na(nominal_gdp), nominal_gdp := gfs_purpose[year == base_year, unique(gdp_nom)]]
  for (i in 2:nrow(gdp)) {
    if (is.na(gdp$nominal_gdp[i])) {
      growth <- params$nominal_gdp_growth_long_run %||% drv[year == gdp$year[i], nominal_gdp_growth_long_run]
      if (!length(growth) || is.na(growth)) growth <- drv[1, nominal_gdp_growth_long_run]
      gdp$nominal_gdp[i] <- gdp$nominal_gdp[i - 1] * (1 + growth)
    }
  }

  rows <- list()
  for (j in seq_len(nrow(base))) {
    b <- base[j]
    path <- merge(data.table(year = base_year:projection_end), drv, by = "year", all.x = TRUE)
    path[year == base_year, `:=`(
      pop_total = macro[scenario == scenario_name & year == projection_start, pop_total] /
        (1 + macro[scenario == scenario_name & year == projection_start, pop_total] /
          macro[scenario == scenario_name & year == projection_start + 1, pop_total] - 1)
    )]
    # Replace the artificial base population with the published 2025 point.
    pop_all <- fread(file.path(processed_dir, "population_projection_age_shares.csv"))
    path <- merge(path[, !c("pop_total", "0_14", "15_34", "35_54", "55_64", "65p"), with = FALSE],
                  pop_all, by = "year", all.x = TRUE)
    path <- merge(path, gdp, by = "year", all.x = TRUE)
    path[, scenario := scenario_name]
    path[, exposure_value := build_exposure(.SD, b$exposure)]
    path[, value := NA_real_]
    path[year == base_year, value := b$value]

    if (b$exposure == "gdp_target") {
      base_share <- b$value / gdp[year == base_year, nominal_gdp]
      path[, target_share := approx(
        c(base_year, params$defence_target_year), c(base_share, params$defence_target_gdp),
        xout = year, rule = 2
      )$y]
      path[, value := nominal_gdp * target_share]
    } else {
      excess <- b[[cost_col]]
      for (i in 2:nrow(path)) {
        exposure_growth <- path$exposure_value[i] / path$exposure_value[i - 1] - 1
        inflation <- path$inflation[i]
        population_growth <- path$pop_total[i] / path$pop_total[i - 1] - 1
        gdp_growth <- path$nominal_gdp[i] / path$nominal_gdp[i - 1] - 1
        productivity_growth <- max(gdp_growth - inflation - population_growth, 0)
        # Baseline real spending per exposure unit follows economy-wide real
        # income/productivity. The excess-cost parameter is therefore an
        # explicit addition to, or subtraction from, that common baseline.
        unit_cost_growth <- inflation + productivity_growth + excess
        path$value[i] <- path$value[i - 1] * (1 + exposure_growth) * (1 + unit_cost_growth)
      }
    }
    rows[[j]] <- path[, .(year, scenario, category = b$category, value, nominal_gdp)]
  }

  out <- rbindlist(rows)
  totals <- out[, .(raw_primary = sum(value), nominal_gdp = unique(nominal_gdp)), by = year]
  totals <- merge(totals, official_anchor[, .(year, primary_expense_ratio)], by = "year", all.x = TRUE)
  totals[, scale_factor := fifelse(
    !is.na(primary_expense_ratio), primary_expense_ratio * nominal_gdp / raw_primary, 1
  )]
  # Apply the last official calibration factor after the anchor; subsequent
  # category growth, rather than repeated rescaling, drives the long run.
  last_scale <- totals[year == official_forecast_end, scale_factor]
  totals[year > official_forecast_end, scale_factor := last_scale]
  out <- merge(out, totals[, .(year, scale_factor)], by = "year")
  out[, `:=`(raw_value = value, raw_share_gdp = value / nominal_gdp)]
  out[, `:=`(
    value = value * scale_factor,
    share_gdp = value * scale_factor / nominal_gdp
  )]
  out[, scale_factor := NULL]
  out
}

`%||%` <- function(x, y) if (length(x) && !is.na(x)) x else y

bottom_up <- rbindlist(lapply(c("central", "pressure", "restraint"), project_scenario))
bottom_up_totals <- bottom_up[, .(
  primary_expense_value = sum(value),
  primary_expense_ratio = sum(share_gdp),
  raw_primary_expense_value = sum(raw_value),
  raw_primary_expense_ratio = sum(raw_share_gdp),
  nominal_gdp = unique(nominal_gdp)
), by = .(year, scenario)]

fwrite(bottom_up, file.path(table_dir, "bottom_up_category_projections.csv"))
fwrite(bottom_up_totals, file.path(table_dir, "bottom_up_total_projections.csv"))
message("Bottom-up projections written for central, pressure and restraint scenarios.")

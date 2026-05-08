# Build Australia projection tables and plots from the OECD long-run scenario
# data downloaded by 01_download_oecd_lts_data.R.

if (!exists("oecd_lts_config")) {
  source(file.path(Sys.getenv("OECD_LTS_ROOT", unset = getwd()), "scripts", "00_config.R"))
}

read_oecd_lts_australia <- function() {
  path <- file.path(oecd_lts_config$processed_dir, "oecd_lts_australia_long.csv")

  if (!file.exists(path)) {
    if (!exists("download_oecd_lts_australia")) {
      source(file.path(oecd_lts_config$root_dir, "scripts", "01_download_oecd_lts_data.R"))
    }
    download_oecd_lts_australia()
  }

  fread(path)
}

add_derived_oecd_lts_series <- function(dt) {
  out <- copy(dt)

  # The OECD long-run scenario database has the potential GDP level for
  # Australia, but not always the ready-made annual growth series. Deriving it
  # here keeps the debt-bridge input usable while preserving the OECD level path
  # as the source of truth.
  if (!("GDPVTR_ANNPCT" %in% unique(out$measure)) && "GDPVTRD" %in% unique(out$measure)) {
    gdp_level <- out[measure == "GDPVTRD"]
    setorder(gdp_level, scenario, year)
    gdp_growth <- copy(gdp_level)
    gdp_growth[
      ,
      value := 100 * (value / shift(value) - 1),
      by = scenario
    ]
    gdp_growth[, measure := "GDPVTR_ANNPCT"]
    gdp_growth[, measure_label := "Potential output growth"]
    gdp_growth[, oecd_measure_label := "Derived from OECD potential GDP level"]
    gdp_growth[, category := "Output and population"]
    gdp_growth[, unit_hint := "Percent per annum"]
    gdp_growth[, series_code := paste(country, measure, scenario, frequency, sep = ".")]
    gdp_growth[, series_name := paste(country_name, measure_label, scenario_label, sep = " - ")]
    gdp_growth[, source_url := "Derived from GDPVTRD"]
    out <- rbindlist(list(out, gdp_growth), use.names = TRUE, fill = TRUE)
  }

  out[]
}

make_summary_table <- function(dt, summary_years = c(2025, 2030, 2050, 2071, 2100)) {
  available_summary_years <- sort(intersect(summary_years, unique(dt$year)))
  base_year <- oecd_lts_config$base_year

  base_values <- dt[year == base_year, .(scenario, measure, base_value = value)]
  summary_dt <- dt[year %in% available_summary_years]
  summary_dt <- merge(summary_dt, base_values, by = c("scenario", "measure"), all.x = TRUE)
  summary_dt[
    ,
    annualised_growth_from_base_pct := fifelse(
      year > base_year & base_value > 0 & value > 0,
      100 * ((value / base_value)^(1 / (year - base_year)) - 1),
      NA_real_
    )
  ]
  summary_dt[
    ,
    change_from_base_pct := fifelse(
      base_value > 0 & value > 0,
      100 * (value / base_value - 1),
      NA_real_
    )
  ]
  summary_dt[]
}

make_bau_differences <- function(dt, reference_scenario = "BAU1") {
  reference <- dt[
    scenario == reference_scenario,
    .(measure, year, reference_value = value)
  ]
  out <- merge(dt, reference, by = c("measure", "year"), all.x = TRUE)
  out[
    ,
    absolute_difference_from_bau1 := value - reference_value
  ]
  out[
    ,
    percent_difference_from_bau1 := fifelse(
      reference_value != 0,
      100 * (value / reference_value - 1),
      NA_real_
    )
  ]
  out[]
}

make_debt_bridge_inputs <- function(dt) {
  bridge_measures <- c("GDPVTR_ANNPCT", "GDPVTRD", "GDPVTRD_CAP", "POPS", "POP")
  bridge <- dt[measure %in% bridge_measures]

  if (!nrow(bridge)) {
    return(data.table())
  }

  bridge_wide <- dcast(
    bridge,
    scenario + scenario_short + scenario_label + year ~ measure,
    value.var = "value"
  )

  if ("GDPVTR_ANNPCT" %in% names(bridge_wide)) {
    setnames(bridge_wide, "GDPVTR_ANNPCT", "potential_gdp_growth_pct")
    bridge_wide[, potential_gdp_growth_decimal := potential_gdp_growth_pct / 100]
  }
  if ("GDPVTRD" %in% names(bridge_wide)) {
    setnames(bridge_wide, "GDPVTRD", "potential_gdp_volume_usd_2021_ppp")
  }
  if ("GDPVTRD_CAP" %in% names(bridge_wide)) {
    setnames(bridge_wide, "GDPVTRD_CAP", "potential_gdp_per_capita_usd_2021_ppp")
  }
  if ("POPS" %in% names(bridge_wide)) {
    setnames(bridge_wide, "POPS", "trend_total_population")
  }
  if ("POP" %in% names(bridge_wide)) {
    setnames(bridge_wide, "POP", "total_population")
  }

  bridge_wide[
    ,
    series := paste0("OECD EO117 LTS - ", scenario_short)
  ]
  setorder(bridge_wide, scenario, year)
  bridge_wide[]
}

make_log_accounting <- function(dt, identity_measure, component_measures,
                                base_year, output_name) {
  accounting_dt <- dt[measure %in% c(identity_measure, component_measures)]

  if (uniqueN(accounting_dt$measure) < length(c(identity_measure, component_measures))) {
    return(data.table())
  }

  wide <- dcast(
    accounting_dt,
    scenario + scenario_short + scenario_label + year ~ measure,
    value.var = "value"
  )
  needed <- c(identity_measure, component_measures)
  wide <- wide[complete.cases(wide[, ..needed])]
  wide <- wide[rowSums(wide[, lapply(.SD, function(x) x <= 0), .SDcols = needed]) == 0]

  base <- wide[
    year == base_year,
    c("scenario", needed),
    with = FALSE
  ]
  setnames(base, needed, paste0("base_", needed))
  wide <- merge(wide, base, by = "scenario", all.x = TRUE)

  wide[, total_log_change_pp := 100 * (log(get(identity_measure)) - log(get(paste0("base_", identity_measure))))]

  component_cols <- character()
  for (component in component_measures) {
    contribution_col <- paste0("contribution_", component)
    wide[
      ,
      (contribution_col) := 100 * (log(get(component)) - log(get(paste0("base_", component))))
    ]
    component_cols <- c(component_cols, contribution_col)
  }

  wide[
    ,
    residual_contribution := total_log_change_pp - rowSums(.SD),
    .SDcols = component_cols
  ]

  long <- melt(
    wide,
    id.vars = c("scenario", "scenario_short", "scenario_label", "year", "total_log_change_pp"),
    measure.vars = c(component_cols, "residual_contribution"),
    variable.name = "component",
    value.name = "contribution_pp"
  )
  long[, accounting_block := output_name]
  long[, component := sub("^contribution_", "", component)]
  long[, component_label := fifelse(
    component == "residual_contribution",
    "Residual",
    measure_lookup[match(component, measure), measure_label]
  )]
  long[]
}

plot_line_measure <- function(dt, measure_code, basename, y_title, indexed = FALSE) {
  plot_dt <- dt[measure == measure_code & scenario %in% plot_scenarios]

  if (!nrow(plot_dt)) {
    return(invisible(NULL))
  }

  if (indexed) {
    base <- plot_dt[year == oecd_lts_config$base_year, .(scenario, base_value = value)]
    plot_dt <- merge(plot_dt, base, by = "scenario", all.x = TRUE)
    plot_dt[, plot_value := 100 * value / base_value]
    y_title <- paste0(y_title, ", 2025 = 100")
  } else {
    plot_dt[, plot_value := value]
  }
  plot_dt <- plot_dt[!is.na(plot_value)]

  p <- ggplot(plot_dt, aes(x = year, y = plot_value, colour = scenario)) +
    geom_line(linewidth = 0.9) +
    scale_colour_manual(values = scenario_colour, labels = scenario_labels) +
    labs(
      title = paste0(oecd_lts_config$country_name, ": ", plot_dt$measure_label[[1]]),
      subtitle = "OECD Economic Outlook 117 long-run scenarios",
      x = NULL,
      y = y_title,
      colour = NULL,
      caption = "Source: OECD Economic Outlook 117 long-term scenarios via DBnomics."
    ) +
    oecd_lts_theme()

  save_plot_pair(p, basename)
}

plot_scenario_differences <- function(diff_dt) {
  plot_dt <- diff_dt[
    measure == "GDPVTRD_CAP" &
      scenario != "BAU1" &
      scenario %in% plot_scenarios
  ]

  if (!nrow(plot_dt)) {
    return(invisible(NULL))
  }

  p <- ggplot(plot_dt, aes(x = year, y = percent_difference_from_bau1, colour = scenario)) +
    geom_hline(yintercept = 0, colour = "grey35", linewidth = 0.45) +
    geom_line(linewidth = 0.9) +
    scale_colour_manual(values = scenario_colour, labels = scenario_labels) +
    labs(
      title = "Potential GDP per capita relative to BAU1",
      subtitle = "Difference from the business-as-usual median-damage path",
      x = NULL,
      y = "Percent difference",
      colour = NULL,
      caption = "Source: OECD Economic Outlook 117 long-term scenarios via DBnomics."
    ) +
    oecd_lts_theme()

  save_plot_pair(p, "oecd_lts_australia_gdp_per_capita_difference_from_bau1")
}

plot_accounting <- function(accounting_dt, block_name, basename, title) {
  plot_dt <- accounting_dt[
    accounting_block == block_name &
      scenario %in% c("BAU1", "ET2", "ET4") &
      year %in% seq(oecd_lts_config$base_year, oecd_lts_config$end_year, by = 5)
  ]

  if (!nrow(plot_dt)) {
    return(invisible(NULL))
  }

  p <- ggplot(plot_dt, aes(x = year, y = contribution_pp, fill = component_label)) +
    geom_col(width = 4.2, colour = "white", linewidth = 0.15) +
    geom_line(
      aes(x = year, y = total_log_change_pp, group = scenario),
      inherit.aes = FALSE,
      colour = "black",
      linewidth = 0.6,
      data = unique(plot_dt[, .(scenario, scenario_short, year, total_log_change_pp)])
    ) +
    facet_wrap(~ scenario_short, ncol = 1) +
    labs(
      title = title,
      subtitle = paste0("Log-point change since ", oecd_lts_config$base_year, "; black line is total"),
      x = NULL,
      y = "Log points",
      fill = NULL,
      caption = "Source: OECD Economic Outlook 117 long-term scenarios via DBnomics. Residual captures measurement/identity mismatch."
    ) +
    oecd_lts_theme()

  save_plot_pair(p, basename, height = 7.5)
}

plot_energy_mix <- function(dt) {
  energy_dt <- dt[
    measure %in% energy_share_measures &
      scenario %in% c("BAU1", "ET2", "ET4") &
      year %in% c(2025, 2050, 2071, 2100)
  ]

  if (!nrow(energy_dt)) {
    return(invisible(NULL))
  }

  energy_dt[, share_pct := 100 * value]

  p <- ggplot(energy_dt, aes(x = factor(year), y = share_pct, fill = measure_label)) +
    geom_col(width = 0.75, colour = "white", linewidth = 0.15) +
    facet_wrap(~ scenario_short, ncol = 1) +
    labs(
      title = "Australia primary energy mix",
      subtitle = "Selected OECD long-run scenarios",
      x = NULL,
      y = "Share of primary energy (%)",
      fill = NULL,
      caption = "Source: OECD Economic Outlook 117 long-term scenarios via DBnomics."
    ) +
    oecd_lts_theme()

  save_plot_pair(p, "oecd_lts_australia_energy_mix_selected_years", height = 7.5)
}

plot_emissions_and_energy <- function(dt) {
  plot_dt <- dt[
    measure %in% c("CO2", "TES") &
      scenario %in% plot_scenarios
  ]

  if (!nrow(plot_dt)) {
    return(invisible(NULL))
  }

  base <- plot_dt[year == oecd_lts_config$base_year, .(scenario, measure, base_value = value)]
  plot_dt <- merge(plot_dt, base, by = c("scenario", "measure"), all.x = TRUE)
  plot_dt[, index_2025 := 100 * value / base_value]

  p <- ggplot(plot_dt, aes(x = year, y = index_2025, colour = scenario)) +
    geom_line(linewidth = 0.9) +
    facet_wrap(~ measure_label, scales = "free_y") +
    scale_colour_manual(values = scenario_colour, labels = scenario_labels) +
    labs(
      title = "Emissions and primary energy supply",
      subtitle = "Australia, indexed to 2025",
      x = NULL,
      y = "2025 = 100",
      colour = NULL,
      caption = "Source: OECD Economic Outlook 117 long-term scenarios via DBnomics."
    ) +
    oecd_lts_theme()

  save_plot_pair(p, "oecd_lts_australia_emissions_and_energy_index")
}

build_oecd_lts_australia_outputs <- function() {
  dt <- read_oecd_lts_australia()
  dt <- add_derived_oecd_lts_series(dt)
  dt <- dt[year <= oecd_lts_config$end_year]

  projection_paths <- copy(dt)
  setorder(projection_paths, scenario, measure, year)
  fwrite(
    projection_paths,
    file.path(oecd_lts_config$output_dir, "oecd_lts_australia_projection_paths.csv")
  )

  summary_dt <- make_summary_table(projection_paths)
  fwrite(
    summary_dt,
    file.path(oecd_lts_config$output_dir, "oecd_lts_australia_scenario_summary.csv")
  )

  diff_dt <- make_bau_differences(projection_paths)
  fwrite(
    diff_dt,
    file.path(oecd_lts_config$output_dir, "oecd_lts_australia_differences_from_bau1.csv")
  )

  debt_bridge <- make_debt_bridge_inputs(projection_paths)
  fwrite(
    debt_bridge,
    file.path(oecd_lts_config$output_dir, "oecd_lts_australia_debt_bridge_inputs.csv")
  )

  output_accounting <- make_log_accounting(
    dt = projection_paths,
    identity_measure = "GDPVTRD",
    component_measures = c("EFFLABS", "ETPT"),
    base_year = oecd_lts_config$base_year,
    output_name = "potential_output"
  )
  labour_accounting <- make_log_accounting(
    dt = projection_paths,
    identity_measure = "ETPT",
    component_measures = c("POPS_1574", "ERS1574"),
    base_year = oecd_lts_config$base_year,
    output_name = "potential_employment"
  )
  accounting_dt <- rbindlist(list(output_accounting, labour_accounting), use.names = TRUE, fill = TRUE)
  fwrite(
    accounting_dt,
    file.path(oecd_lts_config$output_dir, "oecd_lts_australia_growth_accounting.csv")
  )

  plot_line_measure(
    projection_paths,
    "GDPVTRD_CAP",
    "oecd_lts_australia_potential_gdp_per_capita",
    "USD at 2021 PPPs"
  )
  plot_line_measure(
    projection_paths,
    "GDPVTRD",
    "oecd_lts_australia_potential_gdp_index",
    "Potential GDP",
    indexed = TRUE
  )
  plot_line_measure(
    projection_paths,
    "GDPVTR_ANNPCT",
    "oecd_lts_australia_potential_output_growth",
    "Percent per annum"
  )
  plot_line_measure(
    projection_paths,
    "POPS",
    "oecd_lts_australia_trend_population",
    "Trend total population",
    indexed = TRUE
  )
  plot_scenario_differences(diff_dt)
  plot_accounting(
    accounting_dt,
    "potential_output",
    "oecd_lts_australia_potential_output_accounting",
    "Potential output growth accounting"
  )
  plot_accounting(
    accounting_dt,
    "potential_employment",
    "oecd_lts_australia_potential_employment_accounting",
    "Potential employment accounting"
  )
  plot_emissions_and_energy(projection_paths)
  plot_energy_mix(projection_paths)

  message(
    "Built Australia OECD LTS projection outputs: ",
    nrow(projection_paths),
    " observations."
  )

  invisible(list(
    projection_paths = projection_paths,
    summary = summary_dt,
    differences = diff_dt,
    debt_bridge = debt_bridge,
    accounting = accounting_dt
  ))
}

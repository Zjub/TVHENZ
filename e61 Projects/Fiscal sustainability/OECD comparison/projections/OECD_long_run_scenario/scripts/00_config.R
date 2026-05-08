# Shared configuration for the Australia OECD long-run scenario workflow.

required_packages <- c("data.table", "ggplot2", "jsonlite")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required. Please install it before running this workflow.", call. = FALSE)
  }
}

library(data.table)
library(ggplot2)
library(jsonlite)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

detect_oecd_lts_root <- function() {
  env_root <- Sys.getenv("OECD_LTS_ROOT", unset = "")

  if (nzchar(env_root) && file.exists(file.path(env_root, "scripts", "00_config.R"))) {
    return(normalizePath(env_root, mustWork = TRUE))
  }

  wd <- normalizePath(getwd(), mustWork = TRUE)
  candidates <- c(
    wd,
    file.path(wd, "OECD_long_run_scenario"),
    file.path(wd, "projections", "OECD_long_run_scenario")
  )

  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "scripts", "00_config.R"))) {
      return(normalizePath(candidate, mustWork = TRUE))
    }
  }

  stop("Could not locate the OECD_long_run_scenario workflow root.", call. = FALSE)
}

oecd_lts_root <- detect_oecd_lts_root()

oecd_lts_config <- list(
  root_dir = oecd_lts_root,
  raw_dir = file.path(oecd_lts_root, "data", "raw"),
  processed_dir = file.path(oecd_lts_root, "data", "processed"),
  output_dir = file.path(oecd_lts_root, "outputs"),
  figure_dir = file.path(oecd_lts_root, "outputs", "figures"),
  source_provider = "OECD",
  source_dataset = "DSD_EO_LTB@DF_EO_LTB",
  source_dataset_name = "OECD Economic Outlook 117 long-term scenarios",
  dbnomics_dataset_url = "https://api.db.nomics.world/v22/series/OECD/DSD_EO_LTB%40DF_EO_LTB",
  oecd_data_explorer_url = "https://data-explorer.oecd.org/vis?df%5Bag%5D=OECD.ECO.MAD&df%5Bds%5D=DisseminateFinalDMZ&df%5Bid%5D=DSD_EO_LTB%40DF_EO_LTB",
  country_code = "AUS",
  country_name = "Australia",
  frequency = "A",
  base_year = 2025,
  end_year = 2100
)

dir.create(oecd_lts_config$raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(oecd_lts_config$processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(oecd_lts_config$output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(oecd_lts_config$figure_dir, recursive = TRUE, showWarnings = FALSE)

scenario_lookup <- data.table(
  scenario = c("BAU1", "BAU2", "ET1", "ET2", "ET3", "ET4"),
  scenario_short = c("BAU median damage", "BAU high damage", "ET slow costs", "ET fast costs", "ET high damage slow costs", "ET high damage fast costs"),
  scenario_label = c(
    "Business as usual, median damage",
    "Business as usual, high damage",
    "Accelerated transition, median damage, slow cost reduction",
    "Accelerated transition, median damage, fast cost reduction",
    "Accelerated transition, high damage, slow cost reduction",
    "Accelerated transition, high damage, fast cost reduction"
  ),
  transition = c("Business as usual", "Business as usual", rep("Accelerated energy transition", 4)),
  damage_curve = c("Median", "High", "Median", "Median", "High", "High"),
  mitigation_cost = c("None", "None", "Slow reduction", "Fast reduction", "Slow reduction", "Fast reduction")
)

measure_lookup <- data.table(
  measure = c(
    "GDPVTRD",
    "GDPVTRD_CAP",
    "GDPVTR_ANNPCT",
    "POPS",
    "POPS_1574",
    "ETPT",
    "ERS1574",
    "EFFLABS",
    "KTPV",
    "KTPV_ANNPCT",
    "CO2",
    "TES",
    "SHARE_COAL",
    "SHARE_GAS",
    "SHARE_OIL",
    "SHARE_HYDRO",
    "SHARE_NUCLEAR",
    "SHARE_SOLAR",
    "SHARE_WIND",
    "SHARE_ORENEW",
    "UNR",
    "TTRADE",
    "GGFLQ",
    "NLGQ",
    "YPGTQ",
    "YRGTQ",
    "RATE_GGINTP"
  ),
  measure_label = c(
    "Potential GDP, volume",
    "Potential GDP per capita",
    "Potential output growth",
    "Trend total population",
    "Trend working-age population, 15-74",
    "Potential employment",
    "Trend employment rate, 15-74",
    "Trend labour efficiency",
    "Productive capital stock",
    "Productive capital stock growth",
    "Gross CO2 emissions",
    "Total primary energy supply",
    "Coal share of primary energy",
    "Gas share of primary energy",
    "Oil share of primary energy",
    "Hydro share of primary energy",
    "Nuclear share of primary energy",
    "Solar share of primary energy",
    "Wind share of primary energy",
    "Other renewables share of primary energy",
    "Unemployment rate",
    "Terms of trade",
    "General government gross financial liabilities",
    "General government net lending",
    "General government outlays",
    "General government receipts",
    "Implicit interest rate on gross government liabilities"
  ),
  unit_hint = c(
    "USD at 2021 PPPs",
    "USD at 2021 PPPs per person",
    "Percent per annum",
    "Persons",
    "Persons",
    "Persons",
    "Percent",
    "Index/level",
    "Volume",
    "Percent per annum",
    "Million tonnes",
    "Terajoules",
    rep("Share, 0 to 1", 8),
    "Percent",
    "Index/level",
    "Percent of GDP",
    "Percent of GDP",
    "Percent of GDP",
    "Percent of GDP",
    "Percent"
  ),
  category = c(
    rep("Output and population", 6),
    rep("Supply-side drivers", 4),
    rep("Energy and emissions", 10),
    rep("Macro and fiscal", 7)
  )
)

scenario_colour <- c(
  "BAU1" = "#2E5E8C",
  "BAU2" = "#78A7CF",
  "ET1" = "#5F8C4A",
  "ET2" = "#2F6F5E",
  "ET3" = "#C9822B",
  "ET4" = "#A23E48"
)

scenario_labels <- setNames(scenario_lookup$scenario_short, scenario_lookup$scenario)
plot_scenarios <- c("BAU1", "BAU2", "ET2", "ET4")
energy_share_measures <- c(
  "SHARE_COAL",
  "SHARE_GAS",
  "SHARE_OIL",
  "SHARE_HYDRO",
  "SHARE_NUCLEAR",
  "SHARE_SOLAR",
  "SHARE_WIND",
  "SHARE_ORENEW"
)

oecd_lts_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.caption = element_text(hjust = 0)
    )
}

save_plot_pair <- function(plot, basename, width = 8.5, height = 5.8) {
  ggplot2::ggsave(
    file.path(oecd_lts_config$figure_dir, paste0(basename, ".png")),
    plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300
  )
  ggplot2::ggsave(
    file.path(oecd_lts_config$figure_dir, paste0(basename, ".svg")),
    plot,
    width = width,
    height = height,
    units = "in"
  )
}

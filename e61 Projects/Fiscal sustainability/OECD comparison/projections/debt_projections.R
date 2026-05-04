# Topic: Consolidated debt projections
# Author: Matt Nolan
# Created: 4/5/2026

rm(list = ls())

required_packages <- c(
  "data.table",
  "ggplot2",
  "readr",
  "readxl"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install required packages before running this script: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(readr)
  library(readxl)
})

get_script_dir <- function() {
  file_arg <- "--file="
  cmd_args <- commandArgs(trailingOnly = FALSE)
  script_arg <- cmd_args[startsWith(cmd_args, file_arg)]

  if (length(script_arg) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", script_arg[[1]]))))
  }

  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }

  normalizePath(getwd())
}

find_data_dir <- function(script_dir) {
  candidates <- c(
    file.path(script_dir, "data"),
    file.path(getwd(), "data"),
    file.path(getwd(), "projections", "data")
  )

  data_dir <- candidates[dir.exists(candidates)][1]

  if (is.na(data_dir)) {
    stop("Could not find the projections data folder.", call. = FALSE)
  }

  normalizePath(data_dir)
}

script_dir <- get_script_dir()
data_dir <- find_data_dir(script_dir)
project_dir <- dirname(data_dir)
output_dir <- file.path(project_dir, "outputs")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

read_projection_data <- function(data_dir) {
  OECD_debt <- read_csv(
    file.path(data_dir, "Updated OECD gross general debt.csv"),
    show_col_types = FALSE
  )
  setDT(OECD_debt)

  exp_proj <- read_excel(
    file.path(data_dir, "Graph_data.xlsx"),
    sheet = "Figure_26"
  )
  setDT(exp_proj)
  exp_proj <- exp_proj[, .(year, series, Consolidated)]
  exp_proj[, `:=`(
    year = as.numeric(year),
    series = as.character(series),
    Consolidated = as.numeric(Consolidated)
  )]

  OECD_for_debt <- OECD_debt[
    `Reference area` == "Australia",
    .(year = as.numeric(TIME_PERIOD), debt = as.numeric(OBS_VALUE))
  ]

  OECD_exp_GDP <- read_excel(
    file.path(data_dir, "table4_gov_exp-gdp.xlsx"),
    sheet = "exp_%_gpd",
    range = "B2:BC88"
  )
  setDT(OECD_exp_GDP)
  setnames(OECD_exp_GDP, c("...1", "...2"), c("country", "level"))

  exp_dt <- OECD_exp_GDP[
    country == "Australia" & level != "Local"
  ][, country := NULL]

  exp <- melt(
    exp_dt,
    id.vars = "level",
    value.name = "Expenses"
  )

  OECD_rev_GDP <- read_excel(
    file.path(data_dir, "table6_gov_rev-gdp.xlsx"),
    sheet = "gtr_%_gdp",
    range = "B2:BJ88"
  )
  setDT(OECD_rev_GDP)
  setnames(OECD_rev_GDP, c("...1", "...2"), c("country", "level"))

  rev_dt <- OECD_rev_GDP[
    country == "Australia" & level != "Local"
  ][, country := NULL]

  rev <- melt(
    rev_dt,
    id.vars = "level",
    value.name = "Revenue"
  )

  flow_dt <- exp[rev, on = .(level, variable)]

  # Shift forward by an extra year due to OECD years, matching OECD_consolidated.R.
  flow_dt[, year := as.numeric(variable) + 1965]

  flow_dt <- flow_dt[
    ,
    .(
      Expenses = sum(as.numeric(Expenses), na.rm = TRUE),
      Revenue = sum(as.numeric(Revenue), na.rm = TRUE)
    ),
    by = .(year)
  ]

  flow_dt <- flow_dt[exp_proj, on = .(year)]
  flow_dt[, exp_gap := Expenses - Consolidated * 100]

  list(
    flow_dt = flow_dt,
    OECD_for_debt = OECD_for_debt
  )
}

project_debt_path <- function(flow_dt, OECD_for_debt,
                              ramp_revenue = TRUE,
                              target_increase = 2,
                              n_ramp = 20,
                              g_nom_assump = 0.045) {
  dt <- copy(flow_dt)
  setorder(dt, year)

  debt_hist <- copy(OECD_for_debt)
  setorder(debt_hist, year)

  if (dt[series == "Historical", .N] == 0) {
    stop("No rows with series == 'Historical' found in flow_dt.", call. = FALSE)
  }

  if (dt[series == "Projection (base)", .N] == 0) {
    stop(
      "No rows with series == 'Projection (base)' found in flow_dt.",
      call. = FALSE
    )
  }

  last_hist <- dt[series == "Historical"][.N]
  last_gap <- last_hist$exp_gap
  last_rev <- last_hist$Revenue

  dt[
    series == "Projection (base)",
    Expenses := Consolidated * 100 + last_gap
  ]

  dt[
    series == "Projection (base)",
    exp_gap := Expenses - Consolidated * 100
  ]

  proj_idx <- dt[series == "Projection (base)", which = TRUE]
  n_proj <- length(proj_idx)

  if (ramp_revenue) {
    n_ramp_use <- min(n_ramp, n_proj)
    step <- target_increase / n_ramp_use

    dt[
      proj_idx,
      Revenue := last_rev + pmin(seq_len(.N), n_ramp_use) * step
    ]
  } else {
    dt[proj_idx, Revenue := last_rev]
  }

  dt[, deficit_pct := Expenses - Revenue]

  base_year <- max(debt_hist$year, na.rm = TRUE)

  if (!(base_year %in% dt$year)) {
    common_years <- intersect(dt$year, debt_hist$year)

    if (length(common_years) == 0) {
      stop("No overlapping years between flow_dt and OECD_for_debt.", call. = FALSE)
    }

    base_year <- max(common_years, na.rm = TRUE)
  }

  base_debt <- debt_hist[year == base_year, debt]

  if (length(base_debt) != 1 || is.na(base_debt)) {
    stop("Could not identify a unique base-year debt observation.", call. = FALSE)
  }

  dt[year >= base_year, g_nom := g_nom_assump]

  dt[, debt_pct := NA_real_]
  dt[year == base_year, debt_pct := base_debt]

  idx_base <- which(dt$year == base_year)

  for (i in idx_base:(nrow(dt) - 1)) {
    d_t <- dt$debt_pct[i] / 100
    B_t <- dt$deficit_pct[i] / 100
    g_t <- dt$g_nom[i]

    if (is.na(g_t) || is.na(d_t) || is.na(B_t)) next

    d_next <- (d_t + B_t) / (1 + g_t)
    dt$debt_pct[i + 1] <- d_next * 100
  }

  hist_part <- debt_hist[year <= base_year, .(year, debt)]
  proj_part <- dt[year > base_year & !is.na(debt_pct), .(year, debt = debt_pct)]

  rbind(hist_part, proj_part, use.names = TRUE)
}

inputs <- read_projection_data(data_dir)
flow_dt <- inputs$flow_dt
OECD_for_debt <- inputs$OECD_for_debt

debt_with_rev <- project_debt_path(
  flow_dt,
  OECD_for_debt,
  ramp_revenue = TRUE,
  target_increase = 2,
  n_ramp = 20,
  g_nom_assump = 0.045
)
debt_with_rev[, scenario := "Revenue +2ppts"]

debt_no_rev <- project_debt_path(
  flow_dt,
  OECD_for_debt,
  ramp_revenue = FALSE,
  target_increase = 2,
  n_ramp = 20,
  g_nom_assump = 0.045
)
debt_no_rev[, scenario := "Revenue flat"]

debt_both <- rbindlist(
  list(debt_with_rev, debt_no_rev),
  use.names = TRUE
)

projection_labels <- data.frame(
  label = c("Bracket Creep", "No Bracket Creep"),
  x = c(2030, 2030),
  y = c(40, 100),
  scenario = c("Revenue +2ppts", "Revenue flat")
)

scenario_colours <- c(
  "Revenue +2ppts" = "#0B4F6C",
  "Revenue flat" = "#C14953"
)

projection_plot <- ggplot(debt_both, aes(x = year, y = debt, colour = scenario)) +
  geom_line() +
  geom_vline(xintercept = 2024, linetype = "dashed") +
  geom_text(
    data = projection_labels,
    aes(x = x, y = y, label = label, colour = scenario),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = scenario_colours) +
  ggplot2::labs(
    x = "Year",
    y = "Gross debt (% of GDP)",
    colour = NULL,
    caption = paste(
      "Sources: ABS, OECD, e61.",
      "Note: Bracket Creep is a two-percentage point increase in tax to GDP over 20 years."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

write_csv(debt_both, file.path(output_dir, "debt_projection_paths.csv"))
write_csv(flow_dt, file.path(output_dir, "debt_projection_flow_inputs.csv"))

ggplot2::ggsave(
  filename = file.path(output_dir, "Debt_projections_creep.svg"),
  plot = projection_plot,
  width = 8,
  height = 6,
  units = "in"
)

ggplot2::ggsave(
  filename = file.path(output_dir, "Debt_projections_creep.pdf"),
  plot = projection_plot,
  width = 8,
  height = 6,
  units = "in"
)

message("Projection outputs written to: ", normalizePath(output_dir))

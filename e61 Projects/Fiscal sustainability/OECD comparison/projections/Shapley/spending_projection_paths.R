# Topic: Spending projections from the Shapley driver model
# Author: Matt Nolan
# Created: 4/5/2026
#
# This is a focused extraction from "2e Shapley outliers.R". It only builds the
# government spending-to-GDP projection path used downstream in debt projections.

rm(list = ls())

required_packages <- c(
  "data.table",
  "ggplot2",
  "readabs",
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
  library(readabs)
  library(readxl)
})

# -------------------- Settings --------------------

freq <- "FY"
measure <- "Expenditure"       # "GFCE", "GFCE_plus_GFCF", or "Expenditure" - this is consumption, final government demand, and total expenditure including transfers to households
share_basis <- "nominal"       # "nominal" or "real"
abs_check_local <- TRUE
start_year <- 1980
final_year_opt <- NA
outlier_years <- c(2020, 2021)
govt_level <- "total"          # "total", "Federal", or "State"
level <- "National"

features <- c(
  "0_14",
  "15_34",
  "35_54",
  "55_64",
  "65p",
  "tot",
  "rp_g",
  "unemp"
)

# -------------------- Paths --------------------

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

find_shapley_dir <- function(start_dir) {
  candidates <- unique(normalizePath(
    c(
      start_dir,
      file.path(start_dir, "Shapley"),
      getwd(),
      file.path(getwd(), "Shapley")
    ),
    winslash = "\\",
    mustWork = FALSE
  ))

  matches <- candidates[file.exists(file.path(candidates, "For_shapely_projections.xlsx"))]

  if (!length(matches)) {
    stop(
      "Could not find For_shapely_projections.xlsx. ",
      "Run from the projections folder or the projections/Shapley folder, ",
      "or keep the workbook beside this script.",
      call. = FALSE
    )
  }

  normalizePath(matches[[1]], winslash = "\\")
}

script_dir <- get_script_dir()
shapley_dir <- find_shapley_dir(script_dir)

projection_workbook <- file.path(shapley_dir, "For_shapely_projections.xlsx")
output_dir <- file.path(shapley_dir, "outputs")
abs_cache_dir <- file.path(shapley_dir, "abs-cache")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(abs_cache_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(projection_workbook)) {
  stop(
    "Could not find For_shapely_projections.xlsx in ",
    shapley_dir,
    call. = FALSE
  )
}

# -------------------- Helpers --------------------

fy_end_year <- function(date) {
  d <- as.Date(date)
  y <- as.integer(format(d, "%Y"))
  m <- as.integer(format(d, "%m"))
  y + (m >= 7L)
}

calendar_year <- function(date) {
  as.integer(format(as.Date(date), "%Y"))
}

annualise <- function(dt, out_name, mode = c("mean", "sum"), freq = c("FY", "CY")) {
  mode <- match.arg(mode)
  freq <- match.arg(freq)
  dt <- as.data.table(copy(dt))

  if (!"date" %in% names(dt)) {
    stop("annualise(): 'date' column missing.", call. = FALSE)
  }

  if (!inherits(dt$date, "Date")) {
    dt[, date := as.Date(date)]
  }

  year_fun <- if (freq == "FY") fy_end_year else calendar_year

  if (mode == "mean") {
    out <- dt[, .(val = mean(value, na.rm = TRUE)), by = .(year = year_fun(date))]
  } else {
    out <- dt[, .(val = sum(value, na.rm = TRUE)), by = .(year = year_fun(date))]
  }

  setnames(out, "val", out_name)
  setorder(out, year)
  out
}

prefer_sa <- function(dt) {
  if (!nrow(dt)) return(dt)
  text_col <- if ("series_type" %in% names(dt)) dt$series_type else dt$.__txt

  sa <- dt[grepl("Seasonally adjusted", text_col, ignore.case = TRUE)]
  if (nrow(sa)) return(sa)

  trend <- dt[grepl("\\bTrend\\b", text_col, ignore.case = TRUE)]
  if (nrow(trend)) return(trend)

  dt[grepl("\\bOriginal\\b", text_col, ignore.case = TRUE)]
}

dedup_by_date_value <- function(dt) {
  if (!nrow(dt)) return(dt)
  unique(dt[, .(date, value)])[order(date)]
}

ann <- function(dt, out_name) {
  if (!inherits(dt, "data.table")) dt <- as.data.table(dt)

  if (!nrow(dt)) {
    out <- data.table(year = integer(), val = numeric())
    setnames(out, "val", out_name)
    return(out)
  }

  annualise(dt, out_name = out_name, mode = "mean", freq = freq)
}

read_abs_dt <- function(cat_no, tables = "all") {
  as.data.table(read_abs(
    cat_no,
    tables = tables,
    path = abs_cache_dir,
    check_local = abs_check_local
  ))
}

# -------------------- Historical Data --------------------

get_gfce_gdp <- function(freq = c("FY", "CY"),
                         govt_level = c("total", "Federal", "State"),
                         verbose = TRUE) {
  freq <- match.arg(freq)
  govt_level <- match.arg(govt_level)

  na <- read_abs_dt("5206.0")

  if (!nrow(na)) {
    stop("read_abs('5206.0') returned 0 rows.", call. = FALSE)
  }

  if (!inherits(na$date, "Date")) {
    na[, date := as.Date(date)]
  }

  na <- na[grepl("\\$\\s*Million(s)?\\b", unit, ignore.case = TRUE)]

  if (!"table_no" %in% names(na)) {
    stop("Expected column 'table_no' not found in ABS 5206.0.", call. = FALSE)
  }

  if (!is.character(na$table_no)) {
    na[, table_no := as.character(table_no)]
  }

  txt_cols <- intersect(
    c("series", "table_title", "unit", "data_type", "series_type"),
    names(na)
  )
  if (!length(txt_cols)) txt_cols <- "series"

  for (col in txt_cols) {
    if (!is.character(na[[col]])) {
      set(na, j = col, value = as.character(na[[col]]))
    }
  }

  na[, .__txt := do.call(paste, c(.SD, list(sep = " | "))), .SDcols = txt_cols]

  is_tbl13 <- function(x) grepl("^\\s*Table\\s*(3)\\b", x, ignore.case = TRUE)
  is_tbl24 <- function(x) grepl("^\\s*Table\\s*(2)\\b", x, ignore.case = TRUE)
  is_rev <- function(x) grepl("Revisions", x, ignore.case = TRUE)

  is_gen_gov_level <- function(x) {
    if (govt_level == "Federal") {
      grepl("^\\s*General\\s+government\\s*[-;]\\s*National", x, ignore.case = TRUE) &&
        !grepl("Defence|Non[-\\s]?defence", x, ignore.case = TRUE)
    } else if (govt_level == "State") {
      grepl(
        "^\\s*General\\s+government\\s*[-;]\\s*State\\s+and\\s+local",
        x,
        ignore.case = TRUE
      )
    } else {
      grepl("^\\s*General\\s+government\\s*[-;]\\s*", x, ignore.case = TRUE) &&
        !grepl("National|State and local", x, ignore.case = TRUE)
    }
  }

  is_gfce <- function(x) grepl("Final\\s+consumption\\s+expenditure\\s*;", x, TRUE)
  is_gfcf <- function(x) grepl("Gross\\s+fixed\\s+capital\\s+formation\\s*;", x, TRUE)
  is_bad_var <- function(x) grepl("Percentage\\s+changes|Index", x, TRUE)
  is_nom <- function(x) grepl("Current\\s*prices?", x, TRUE) || grepl("\\bCP\\b", x, TRUE)
  is_real <- function(x) {
    grepl("Chain\\s*volume|Volume\\s*chain|Chain-?volume|Volume\\s*measures", x, TRUE) ||
      grepl("\\bCVM\\b|\\bvolume\\b", x, TRUE)
  }
  is_gdp <- function(x) grepl("\\bGross\\s+domestic\\s+product\\b|\\bGDP\\b", x, TRUE)

  na_nom <- na[is_tbl13(table_title) & !is_rev(series)]
  na_real <- na[is_tbl24(table_title) & !is_rev(series)]

  gg_nom <- na_nom[
    vapply(series, is_gen_gov_level, logical(1)) &
      !vapply(series, is_bad_var, logical(1))
  ]
  gg_real <- na_real[
    vapply(series, is_gen_gov_level, logical(1)) &
      !vapply(series, is_bad_var, logical(1))
  ]

  gfce_nom_q <- dedup_by_date_value(prefer_sa(
    gg_nom[vapply(.__txt, is_nom, logical(1)) & vapply(series, is_gfce, logical(1))]
  ))
  gfcf_nom_q <- dedup_by_date_value(prefer_sa(
    gg_nom[vapply(.__txt, is_nom, logical(1)) & vapply(series, is_gfcf, logical(1))]
  ))
  gfce_real_q <- dedup_by_date_value(prefer_sa(
    gg_real[vapply(.__txt, is_real, logical(1)) & vapply(series, is_gfce, logical(1))]
  ))
  gfcf_real_q <- dedup_by_date_value(prefer_sa(
    gg_real[vapply(.__txt, is_real, logical(1)) & vapply(series, is_gfcf, logical(1))]
  ))

  gdp_nom_q <- dedup_by_date_value(prefer_sa(
    na_nom[vapply(series, is_gdp, logical(1)) & vapply(.__txt, is_nom, logical(1))]
  ))
  gdp_real_q <- dedup_by_date_value(prefer_sa(
    na_real[vapply(series, is_gdp, logical(1)) & vapply(.__txt, is_real, logical(1))]
  ))

  tip_q <- dedup_by_date_value(prefer_sa(
    na[
      table_no == "5206017_gen_govt_income_account" &
        !is_rev(series) &
        series == "Total income payable ;"
    ]
  ))

  if (!nrow(gfce_nom_q) && !nrow(gfce_real_q) && !nrow(gfcf_nom_q) && !nrow(gfcf_real_q)) {
    stop("No General government GFCE/GFCF rows after filters.", call. = FALSE)
  }

  if (!nrow(gdp_nom_q) && !nrow(gdp_real_q)) {
    stop("No GDP rows after filters.", call. = FALSE)
  }

  gfce_nom_a <- ann(gfce_nom_q, "gfce_nom")
  gfce_real_a <- ann(gfce_real_q, "gfce_real")
  gfcf_nom_a <- ann(gfcf_nom_q, "gfcf_nom")
  gfcf_real_a <- ann(gfcf_real_q, "gfcf_real")
  gdp_nom_a <- ann(gdp_nom_q, "gdp_nom")
  gdp_real_a <- ann(gdp_real_q, "gdp_real")

  tip_nom_a <- if (nrow(tip_q) && govt_level != "State") {
    ann(tip_q, "total_income_payable_nom")
  } else {
    data.table(year = integer(), total_income_payable_nom = double())
  }

  out <- Reduce(
    function(x, y) merge(x, y, by = "year", all = TRUE),
    list(gfce_nom_a, gfce_real_a, gfcf_nom_a, gfcf_real_a, gdp_nom_a, gdp_real_a, tip_nom_a)
  )

  setorder(out, year)

  if (isTRUE(verbose)) {
    message("Built fiscal aggregates from ABS 5206.0 for ", govt_level, ".")
  }

  out
}

get_erp_total_and_shares <- function(freq = c("FY", "CY"), table_hint = "59") {
  freq <- match.arg(freq)
  erp_raw <- read_abs_dt("3101.0", tables = table_hint)

  if (!"date" %in% names(erp_raw)) {
    stop("ABS 3101.0 pull missing 'date'.", call. = FALSE)
  }

  if (!inherits(erp_raw$date, "Date")) {
    erp_raw[, date := as.Date(date)]
  }

  age_text_col <- if ("item" %in% names(erp_raw)) "item" else "series"

  if ("sex" %in% names(erp_raw) && any(grepl("^Persons$", erp_raw$sex, TRUE))) {
    erp_raw <- erp_raw[grepl("^Persons$", sex, TRUE)]
  }

  parse_age_bounds <- function(x) {
    x <- tolower(x)
    nums <- regmatches(x, gregexpr("\\d+", x))[[1]]
    if (!length(nums)) return(c(NA_integer_, NA_integer_))

    lo <- as.integer(nums[1])
    hi <- if (grepl("over|and over|\\+", x)) 120L else lo
    c(lo, hi)
  }

  bounds <- do.call(rbind, lapply(erp_raw[[age_text_col]], parse_age_bounds))
  erp_raw[, age_lo := bounds[, 1]]
  erp_raw[, age_hi := bounds[, 2]]

  ages <- erp_raw[!is.na(age_lo) & !is.na(age_hi)]

  map_bin <- function(lo, hi) {
    if (hi <= 14) return("0_14")
    if (lo >= 15 && hi <= 34) return("15_34")
    if (lo >= 35 && hi <= 54) return("35_54")
    if (lo >= 55 && hi <= 64) return("55_64")
    if (lo >= 65) return("65p")
    NA_character_
  }

  ages[, group := mapply(map_bin, age_lo, age_hi)]
  ages <- ages[!is.na(group)]

  ages_grp <- ages[, .(pop_grp = sum(value, na.rm = TRUE)), by = .(date, group)]
  erp_tot <- ages_grp[, .(pop_total = sum(pop_grp, na.rm = TRUE)), by = .(date)]

  erp_m <- merge(ages_grp, erp_tot, by = "date", all.x = TRUE)
  erp_m[, share := pop_grp / pop_total]

  year_fun <- if (freq == "FY") fy_end_year else calendar_year
  shares_a <- erp_m[, .(share = mean(share, na.rm = TRUE)), by = .(year = year_fun(date), group)]
  erp_tot_a <- erp_tot[, .(pop_total = mean(pop_total, na.rm = TRUE)), by = .(year = year_fun(date))]

  shares_w <- dcast(shares_a, year ~ group, value.var = "share")

  for (group in c("0_14", "15_34", "35_54", "55_64", "65p")) {
    if (!group %in% names(shares_w)) shares_w[, (group) := NA_real_]
  }

  setcolorder(shares_w, c("year", "0_14", "15_34", "35_54", "55_64", "65p"))
  merge(erp_tot_a, shares_w, by = "year", all = TRUE)[order(year)]
}

get_terms_of_trade <- function(freq = c("FY", "CY")) {
  freq <- match.arg(freq)
  bo <- read_abs_dt("5302.0")

  if (!inherits(bo$date, "Date")) {
    bo[, date := as.Date(date)]
  }

  tot_q <- bo[
    table_no == "530205" &
      series == "Terms of Trade ;  Goods and Services ;" &
      series_type == "Seasonally Adjusted"
  ]

  if (!nrow(tot_q)) {
    stop("Could not locate terms of trade index in ABS 5302.0.", call. = FALSE)
  }

  annualise(tot_q, out_name = "tot_index", mode = "mean", freq = freq)[order(year)]
}

get_unemployment <- function(freq = c("FY", "CY")) {
  freq <- match.arg(freq)
  dt_unemp <- read_abs_dt("6202.0", tables = "1")

  dt_unemp <- dt_unemp[
    series == "Unemployment rate ;  Persons ;" &
      series_type == "Seasonally Adjusted"
  ]

  if (!nrow(dt_unemp)) {
    stop("Could not locate unemployment rate in ABS 6202.0.", call. = FALSE)
  }

  annualise(dt_unemp, out_name = "unemp", mode = "mean", freq = freq)
}

build_share_dataset <- function(freq = c("FY", "CY"),
                                measure = c("GFCE", "GFCE_plus_GFCF", "Expenditure"),
                                govt_level = c("total", "Federal", "State"),
                                share_basis = c("nominal", "real")) {
  freq <- match.arg(freq)
  measure <- match.arg(measure)
  govt_level <- match.arg(govt_level)
  share_basis <- match.arg(share_basis)

  fiscal <- get_gfce_gdp(freq = freq, govt_level = govt_level)
  erp <- get_erp_total_and_shares(freq = freq)

  dt <- merge(fiscal, erp, by = "year", all = FALSE)

  if (!nrow(dt)) {
    stop("No overlapping years between ABS 5206.0 and ABS 3101.0.", call. = FALSE)
  }

  gov_level <- switch(
    measure,
    "GFCE" = if (share_basis == "nominal") dt$gfce_nom else dt$gfce_real,
    "GFCE_plus_GFCF" = {
      if (share_basis == "nominal") dt$gfce_nom + dt$gfcf_nom else dt$gfce_real + dt$gfcf_real
    },
    "Expenditure" = {
      if (share_basis == "real") {
        stop(
          "Transfers are nominal-only. Use share_basis = 'nominal' or a different measure.",
          call. = FALSE
        )
      }

      tip <- if (govt_level == "State") {
        0
      } else {
        if (!"total_income_payable_nom" %in% names(dt)) {
          stop("Column 'total_income_payable_nom' not found.", call. = FALSE)
        }
        dt$total_income_payable_nom
      }

      dt$gfce_nom + dt$gfcf_nom + tip
    }
  )

  gdp_level <- if (share_basis == "nominal") dt$gdp_nom else dt$gdp_real

  if (anyNA(gov_level) || anyNA(gdp_level)) {
    stop("Missing series for chosen share_basis/measure in ABS 5206.0.", call. = FALSE)
  }

  dt[, gov_gdp := gov_level / gdp_level]
  dt[, p_gfce := gfce_nom / gfce_real]
  dt[, p_gdp := gdp_nom / gdp_real]
  dt[, rp_g := as.numeric(scale(p_gfce / p_gdp, center = TRUE, scale = TRUE))]

  if (!"pop_total" %in% names(dt)) {
    stop("pop_total not found after ERP merge.", call. = FALSE)
  }

  setorder(dt, year)
  dt[, dln_pop := c(NA_real_, diff(log(pop_total)))]

  need_cols <- c(
    "year",
    "gov_gdp",
    "0_14",
    "15_34",
    "35_54",
    "55_64",
    "65p",
    "pop_total",
    "rp_g",
    "gfce_nom",
    "gfce_real",
    "gfcf_nom",
    "gfcf_real",
    "gdp_nom",
    "gdp_real"
  )

  missing_cols <- setdiff(need_cols, names(dt))
  if (length(missing_cols)) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  dt <- dt[, ..need_cols]
  dt <- dt[complete.cases(dt)]
  setorder(dt, year)

  dt[, age_sum := `0_14` + `15_34` + `35_54` + `55_64` + `65p`]
  if (any(abs(dt$age_sum - 1) > 0.03, na.rm = TRUE)) {
    warning("Some years' age shares deviate from 1 by more than 3%.")
  }
  dt[, age_sum := NULL]

  dt[]
}

# -------------------- Model and Projections --------------------

coef_full <- function(dt, features, y_col = "gov_gdp") {
  x_df <- as.data.frame(dt[, ..features])

  for (col in seq_along(x_df)) {
    x_df[[col]] <- as.numeric(x_df[[col]])
  }

  x_mat <- as.matrix(x_df)
  y <- as.numeric(dt[[y_col]])

  beta <- as.numeric(coef(lm.fit(x = x_mat, y = y)))
  names(beta) <- colnames(x_mat)
  beta
}

predict_no_intercept <- function(dt, features, beta) {
  x_mat <- as.matrix(as.data.frame(dt[, ..features]))
  as.vector(x_mat %*% beta)
}

prepare_historical_model_data <- function() {
  tot_a <- get_terms_of_trade(freq = freq)
  unemp_a <- get_unemployment(freq = freq)

  share_dt <- build_share_dataset(
    freq = freq,
    measure = measure,
    govt_level = govt_level,
    share_basis = share_basis
  )

  share_dt <- merge(share_dt, tot_a, by = "year", all.x = TRUE)
  share_dt[, tot := as.numeric(scale(tot_index, center = TRUE, scale = TRUE))]

  share_dt <- merge(share_dt, unemp_a, by = "year", all.x = TRUE)

  share_dt[, p_gfce := gfce_nom / gfce_real]
  share_dt[, p_gdp := gdp_nom / gdp_real]
  share_dt[, rp_g := as.numeric(scale(p_gfce / p_gdp, center = TRUE, scale = TRUE))]
  share_dt[, dln_pop := c(0.032, diff(log(pop_total)))]
  share_dt[, is_outlier := year %in% outlier_years]

  if (is.finite(start_year)) share_dt <- share_dt[year >= start_year]
  if (is.finite(final_year_opt)) share_dt <- share_dt[year <= final_year_opt]

  need <- c("gov_gdp", features)
  incomplete <- share_dt[!complete.cases(share_dt[, ..need])]

  if (nrow(incomplete)) {
    warning(
      "Dropping rows with incomplete model data: ",
      paste(incomplete$year, collapse = ", ")
    )
    share_dt <- share_dt[complete.cases(share_dt[, ..need])]
  }

  if (nrow(share_dt) < 2) {
    stop("Not enough complete historical observations to estimate the model.", call. = FALSE)
  }

  share_dt
}

prepare_projection_drivers <- function(share_dt) {
  proj_raw <- as.data.table(readxl::read_excel(projection_workbook, sheet = "Sheet1"))
  setnames(proj_raw, tolower(names(proj_raw)))
  proj_raw <- proj_raw[year >= 2025]

  name_map <- c(
    "year" = "year",
    "0_14" = "0_14",
    "15_34" = "15_34",
    "35_54" = "35_54",
    "55_64" = "55_64",
    "65p" = "65p",
    "rp_g" = "rp_g",
    "rp_g_z" = "rp_g_z",
    "pop_total" = "pop_total",
    "dln_pop" = "dln_pop",
    "unemp" = "unemp",
    "tot" = "tot",
    "tot_z" = "tot_z",
    "tot_index" = "tot_index"
  )

  keep_cols <- intersect(names(proj_raw), names(name_map))
  setnames(proj_raw, keep_cols, name_map[keep_cols])
  proj <- copy(proj_raw)

  if (!"dln_pop" %in% names(proj) && "pop_total" %in% names(proj)) {
    setorder(proj, year)
    proj[, dln_pop := c(NA_real_, diff(log(pop_total)))]
    proj <- proj[!is.na(dln_pop)]
  }

  if ("rp_g_z" %in% names(proj)) {
    proj[, rp_g := as.numeric(rp_g_z)]
  } else if ("rp_g" %in% names(proj)) {
    proj[, rp_g := as.numeric(rp_g)]
  } else {
    stop("Projection file must include either 'rp_g' or 'rp_g_z'.", call. = FALSE)
  }

  if (!"tot_index" %in% names(share_dt)) {
    stop("Expected 'tot_index' in historical data.", call. = FALSE)
  }

  mu_tot_raw <- mean(share_dt$tot_index, na.rm = TRUE)
  sd_tot_raw <- sd(share_dt$tot_index, na.rm = TRUE)
  sd_tot_raw <- ifelse(is.finite(sd_tot_raw) && sd_tot_raw > 0, sd_tot_raw, 1)

  if ("tot_z" %in% names(proj)) {
    proj[, tot := as.numeric(tot_z)]
  } else if ("tot_index" %in% names(proj)) {
    proj[, tot := as.numeric((tot_index - mu_tot_raw) / sd_tot_raw)]
  } else if ("tot" %in% names(proj)) {
    proj[, tot := as.numeric(tot)]
  } else {
    stop("Projection file must include 'tot_z', 'tot_index', or 'tot'.", call. = FALSE)
  }

  age_vars <- intersect(c("0_14", "15_34", "35_54", "55_64", "65p"), names(proj))

  if (length(age_vars)) {
    proj[, age_sum := rowSums(.SD), .SDcols = age_vars]

    if (any(abs(proj$age_sum - 1) > 0.03, na.rm = TRUE)) {
      warning("Some projection age shares do not sum to 1. Renormalising.")
      proj[, (age_vars) := lapply(.SD, function(x) x / age_sum), .SDcols = age_vars]
    }

    proj[, age_sum := NULL]
  }

  last_hist_year <- max(share_dt$year, na.rm = TRUE)
  proj <- proj[year > last_hist_year]

  if (!nrow(proj)) {
    stop("Projection workbook has no years beyond the historical sample.", call. = FALSE)
  }

  missing_feats <- setdiff(features, names(proj))

  if (length(missing_feats)) {
    stop(
      "Projection file missing required feature columns: ",
      paste(missing_feats, collapse = ", "),
      call. = FALSE
    )
  }

  proj <- proj[complete.cases(proj[, ..features])]

  if (!nrow(proj)) {
    stop("Projection workbook has no complete rows for the selected features.", call. = FALSE)
  }

  setorder(proj, year)
  proj
}

share_dt <- prepare_historical_model_data()
dt_est <- share_dt[!(year %in% outlier_years)]

if (nrow(dt_est) < 2) {
  stop("Not enough observations after excluding outlier years.", call. = FALSE)
}

beta <- coef_full(dt_est, features)

hist_dt <- data.table(
  year = share_dt$year,
  series = "Historical",
  value = share_dt$gov_gdp
)

fitted_dt <- data.table(
  year = dt_est$year,
  series = "Fitted (in-sample)",
  value = predict_no_intercept(dt_est, features, beta)
)

proj <- prepare_projection_drivers(share_dt)

base_proj <- copy(proj[, c("year", features), with = FALSE])
base_proj[, value := predict_no_intercept(base_proj, features, beta)]
base_proj[, series := "Projection (base)"]

plot_dt <- rbindlist(
  list(
    hist_dt,
    fitted_dt,
    base_proj[, .(year, series, value)]
  ),
  use.names = TRUE
)

debt_path_input <- plot_dt[
  series != "Fitted (in-sample)",
  .(year, series, Consolidated = value)
]

model_coefficients <- data.table(
  feature = names(beta),
  beta = as.numeric(beta)
)

output_stub <- paste0(measure, "_", level)

fwrite(
  plot_dt,
  file.path(output_dir, paste0("spending_projection_history_fit_projection_", output_stub, ".csv"))
)
fwrite(
  debt_path_input,
  file.path(output_dir, paste0("spending_path_for_debt_projection_", output_stub, ".csv"))
)
fwrite(
  model_coefficients,
  file.path(output_dir, paste0("spending_projection_coefficients_", output_stub, ".csv"))
)
fwrite(
  share_dt,
  file.path(output_dir, paste0("spending_projection_historical_model_data_", output_stub, ".csv"))
)
fwrite(
  proj,
  file.path(output_dir, paste0("spending_projection_forward_drivers_", output_stub, ".csv"))
)

projection_plot <- ggplot(plot_dt, aes(x = year, y = value * 100, colour = series)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(
    values = c(
      "Historical" = "black",
      "Fitted (in-sample)" = "#3777B8",
      "Projection (base)" = "#C14953"
    )
  ) +
  labs(
    title = paste0("Government ", measure, " as % of GDP"),
    subtitle = paste0("No-intercept OLS; outlier years excluded: ", paste(outlier_years, collapse = ", ")),
    x = NULL,
    y = "% of GDP",
    colour = NULL,
    caption = "Sources: ABS, e61 projections."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

ggplot2::ggsave(
  file.path(output_dir, paste0("spending_projection_", output_stub, ".png")),
  projection_plot,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

ggplot2::ggsave(
  file.path(output_dir, paste0("spending_projection_", output_stub, ".svg")),
  projection_plot,
  width = 8,
  height = 6,
  units = "in"
)

message("Spending projection outputs written to: ", normalizePath(output_dir))

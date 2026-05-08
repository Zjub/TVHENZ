# Topic: Spending projections from an ARIMAX driver model
# Author: Matt Nolan
# Created: 4/5/2026
#
# This mirrors the Shapley spending projection workflow, but replaces the
# no-intercept OLS projection model with a basic ARIMA model using the same
# explanatory variables as external regressors.

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
decomposition_base_year <- 2000

# Projection model form.
#
# "level" keeps the current ARIMAX-in-levels specification:
#   spending_share_t ~ age shares_t + macro drivers_t + ARIMA errors
#
# "diff" estimates annual changes and then cumulates the forecast changes from
# the final observed spending/GDP value:
#   d(spending_share_t) ~ d(age shares_t) + d(macro drivers_t) + ARIMA errors
#
# "ECM" first estimates a long-run level relationship, then estimates annual
# changes with the lagged long-run gap included as an error-correction term:
#   d(spending_share_t) ~ d(drivers_t) + lag(long_run_gap_t) + ARIMA errors
#
# The ECM option is only a sensible forecasting model if the cointegration
# diagnostic supports a stationary long-run gap. The script saves that test in
# outputs/diagnostics so this can be checked before relying on the ECM path.
model_form <- "diff"
model_form <- match.arg(model_form, c("level", "ECM", "diff"))

# Age shares sum to one. That creates a specification choice.
#
# TRUE reproduces the Shapley-style coding: include every age group and omit the
# intercept. This treats age groups symmetrically and is useful when the model is
# mainly a decomposition device.
#
# FALSE uses the more standard regression/ARIMAX coding: include an intercept
# and drop one age group as the reference category. The coefficients on included
# age groups are then interpreted relative to the omitted group.
no_intercept <- TRUE # Set to true when using difference or ECM approaches - as otherwise projections are sensitive to misspecification of the drift term.
reference_age_group <- "35_54"

age_features_all <- c(
  "0_14",
  "15_34",
  "35_54",
  "55_64",
  "65p"
)

economic_features <- c(
  "tot",
  "rp_g",
  "unemp"
)

features <- if (isTRUE(no_intercept)) {
  age_features_all
} else {
  setdiff(age_features_all, reference_age_group)
}

features <- c(features, economic_features)
include_mean <- !isTRUE(no_intercept)

model_variant <- if (isTRUE(no_intercept) && model_form == "level") {
  "no_intercept_all_ages"
} else if (isTRUE(no_intercept)) {
  paste0("no_drift_ref_d_", reference_age_group)
} else {
  paste0("intercept_ref_", reference_age_group)
}

model_variant <- paste(model_form, model_variant, sep = "_")

specification_note <- if (isTRUE(no_intercept) && model_form == "level") {
  "no intercept, all age groups"
} else if (isTRUE(no_intercept)) {
  paste0("no drift, differenced reference age group = ", reference_age_group)
} else {
  paste0("intercept, reference age group = ", reference_age_group)
}

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

find_arima_dir <- function(start_dir) {
  candidates <- unique(normalizePath(
    c(
      start_dir,
      file.path(start_dir, "ARIMA"),
      getwd(),
      file.path(getwd(), "ARIMA")
    ),
    winslash = "\\",
    mustWork = FALSE
  ))

  matches <- candidates[file.exists(file.path(candidates, "For_shapely_projections.xlsx"))]

  if (!length(matches)) {
    stop(
      "Could not find For_shapely_projections.xlsx. ",
      "Run from the projections folder or the projections/ARIMA folder, ",
      "or keep the workbook beside this script.",
      call. = FALSE
    )
  }

  normalizePath(matches[[1]], winslash = "\\")
}

script_dir <- get_script_dir()
arima_dir <- find_arima_dir(script_dir)

projection_workbook <- file.path(arima_dir, "For_shapely_projections.xlsx")
output_dir <- file.path(arima_dir, "outputs")
abs_cache_dir <- file.path(arima_dir, "abs-cache")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(abs_cache_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(projection_workbook)) {
  stop(
    "Could not find For_shapely_projections.xlsx in ",
    arima_dir,
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

make_xreg <- function(dt, features) {
  x_df <- as.data.frame(dt[, ..features])

  for (col in seq_along(x_df)) {
    x_df[[col]] <- as.numeric(x_df[[col]])
  }

  out <- as.matrix(x_df)
  colnames(out) <- features
  out
}

fit_arimax_grid <- function(y, xreg,
                            include_mean = FALSE,
                            p_grid = 0:2,
                            d_grid = 0:1,
                            q_grid = 0:2) {
  fits <- list()
  fit_index <- 0L

  for (p in p_grid) {
    for (d in d_grid) {
      for (q in q_grid) {
        fit <- tryCatch(
          suppressWarnings(stats::arima(
            y,
            order = c(p, d, q),
            xreg = xreg,
            # include.mean is the base-R ARIMA intercept/constant option for
            # non-differenced models. We set this from no_intercept so the
            # ARIMAX specification mirrors the chosen age-share treatment.
            include.mean = include_mean,
            method = "ML"
          )),
          error = function(e) NULL
        )

        if (!is.null(fit) && is.finite(fit$aic)) {
          fit_index <- fit_index + 1L
          fits[[fit_index]] <- list(
            order = c(p, d, q),
            fit = fit,
            aic = fit$aic
          )
        }
      }
    }
  }

  if (!length(fits)) {
    stop("No ARIMAX model could be estimated for the selected grid.", call. = FALSE)
  }

  fits[[which.min(vapply(fits, `[[`, numeric(1), "aic"))]]
}

arimax_fitted_values <- function(y, fit) {
  as.numeric(y - residuals(fit))
}

forecast_arimax <- function(fit, newxreg) {
  xreg <- matrix(NA_real_, nrow = 1L, ncol = ncol(newxreg))
  colnames(xreg) <- colnames(newxreg)
  forecast <- predict(fit, n.ahead = nrow(newxreg), newxreg = newxreg)
  as.numeric(forecast$pred)
}

quote_formula_name <- function(x) {
  paste0("`", gsub("`", "\\\\`", x), "`")
}

estimate_long_run_relationship <- function(dt, features, include_mean) {
  rhs <- paste(quote_formula_name(features), collapse = " + ")
  formula_text <- if (isTRUE(include_mean)) {
    paste("gov_gdp ~", rhs)
  } else {
    paste("gov_gdp ~ 0 +", rhs)
  }

  lm(as.formula(formula_text), data = as.data.frame(dt))
}

cointegration_diagnostics <- function(dt, features, include_mean) {
  long_run_fit <- estimate_long_run_relationship(dt, features, include_mean)
  residual_dt <- data.table(
    year = dt$year,
    cointegration_residual = as.numeric(residuals(long_run_fit))
  )

  if (!requireNamespace("tseries", quietly = TRUE)) {
    test_dt <- data.table(
      test = "Engle-Granger residual ADF",
      statistic = NA_real_,
      p_value = NA_real_,
      conclusion = "not run",
      note = "Install the tseries package to run the residual ADF test."
    )
  } else {
    adf <- tryCatch(
      suppressWarnings(tseries::adf.test(residual_dt$cointegration_residual)),
      error = function(e) NULL
    )

    if (is.null(adf)) {
      test_dt <- data.table(
        test = "Engle-Granger residual ADF",
        statistic = NA_real_,
        p_value = NA_real_,
        conclusion = "test failed",
        note = "The residual ADF test failed."
      )
    } else {
      p_value <- as.numeric(adf$p.value)
      test_dt <- data.table(
        test = "Engle-Granger residual ADF",
        statistic = as.numeric(adf$statistic),
        p_value = p_value,
        conclusion = if (is.finite(p_value) && p_value < 0.05) {
          "evidence of a stationary long-run gap"
        } else {
          "no strong evidence of cointegration"
        },
        note = paste(
          "This is an Engle-Granger residual ADF diagnostic.",
          "The p-value uses the tseries ADF approximation, so treat it as a guide rather than a formal critical value."
        )
      )
    }
  }

  list(
    fit = long_run_fit,
    test = test_dt,
    residuals = residual_dt
  )
}

add_model_differences <- function(dt, features) {
  dt <- copy(dt)
  setorder(dt, year)

  dt[, previous_year := shift(year)]
  dt[, lag_gov_gdp := shift(gov_gdp)]
  dt[, d_gov_gdp := gov_gdp - lag_gov_gdp]

  for (feature in features) {
    dt[, paste0("d_", feature) := as.numeric(get(feature)) - shift(as.numeric(get(feature)))]
  }

  dt[, consecutive_year := year == previous_year + 1L]
  dt
}

add_long_run_gap <- function(dt, long_run_fit) {
  dt <- copy(dt)
  dt[, long_run_fitted := as.numeric(predict(long_run_fit, newdata = as.data.frame(dt)))]
  dt[, long_run_gap := gov_gdp - long_run_fitted]
  dt[, ecm_lag := shift(long_run_gap)]
  dt
}

prepare_projection_difference_drivers <- function(share_dt, proj, features) {
  last_hist <- share_dt[year == max(year, na.rm = TRUE)]

  combined <- rbindlist(
    list(
      last_hist[, c("year", features), with = FALSE],
      proj[, c("year", features), with = FALSE]
    ),
    use.names = TRUE
  )

  setorder(combined, year)

  for (feature in features) {
    combined[, paste0("d_", feature) := as.numeric(get(feature)) - shift(as.numeric(get(feature)))]
  }

  combined[year %in% proj$year]
}

build_model_inputs <- function(share_dt, dt_est_level, proj, features,
                               model_form, include_mean, cointegration,
                               age_features_all, reference_age_group) {
  if (model_form == "level") {
    return(list(
      estimation_dt = copy(dt_est_level),
      projection_dt = copy(proj),
      y_col = "gov_gdp",
      x_features = features,
      long_run_fit = NULL
    ))
  }

  diff_features <- paste0("d_", features)

  # Differenced age shares are exactly collinear when all age groups are
  # included: because the level shares sum to one, the first differences sum to
  # zero. Dropping one differenced age group is an identification choice, not a
  # drift term. This lets no_intercept = TRUE mean "no annual drift" for the
  # diff/ECM models while keeping the design matrix estimable.
  diff_age_features <- paste0("d_", intersect(features, age_features_all))
  reference_diff_age <- paste0("d_", reference_age_group)

  if (length(diff_age_features) == length(age_features_all) && reference_diff_age %in% diff_features) {
    diff_features <- setdiff(diff_features, reference_diff_age)
  }

  diff_dt <- add_model_differences(share_dt, features)
  diff_proj <- prepare_projection_difference_drivers(share_dt, proj, features)

  if (model_form == "ECM") {
    diff_dt <- add_long_run_gap(diff_dt, cointegration$fit)
    diff_proj[, ecm_lag := NA_real_]
    model_features <- c(diff_features, "ecm_lag")
  } else {
    model_features <- diff_features
  }

  need <- c("year", "previous_year", "lag_gov_gdp", "d_gov_gdp", model_features)
  estimation_dt <- diff_dt[
    !(year %in% outlier_years) &
      !(previous_year %in% outlier_years) &
      consecutive_year == TRUE,
    ..need
  ]
  estimation_dt <- estimation_dt[complete.cases(estimation_dt)]

  projection_dt <- diff_proj[, c("year", model_features), with = FALSE]
  projection_dt <- projection_dt[complete.cases(projection_dt[, setdiff(model_features, "ecm_lag"), with = FALSE])]

  list(
    estimation_dt = estimation_dt,
    projection_dt = projection_dt,
    y_col = "d_gov_gdp",
    x_features = model_features,
    long_run_fit = if (model_form == "ECM") cointegration$fit else NULL
  )
}

forecast_ecm_path <- function(arimax_fit, projection_dt, share_dt, proj,
                              features, model_features, long_run_fit,
                              max_iterations = 25L, tolerance = 1e-10) {
  out <- copy(projection_dt)
  last_hist <- share_dt[year == max(year, na.rm = TRUE)]
  previous_y <- last_hist$gov_gdp[[1]]
  previous_driver <- as.data.frame(last_hist[, ..features])

  # Start the fixed-point iteration from the last observed level. The ECM lag
  # for later projection years is then updated from the model-implied level path.
  out[, ecm_lag := NA_real_]
  out[1L, ecm_lag := previous_y - as.numeric(predict(long_run_fit, newdata = previous_driver))]
  if (nrow(out) > 1L) {
    out[2:nrow(out), ecm_lag := out[1L, ecm_lag]]
  }

  level_path <- rep(previous_y, nrow(out))

  for (iteration in seq_len(max_iterations)) {
    old_level_path <- level_path
    xreg <- make_xreg(out, model_features)
    delta_path <- forecast_arimax(arimax_fit, xreg)
    level_path <- previous_y + cumsum(delta_path)

    if (nrow(out) > 1L) {
      for (i in 2:nrow(out)) {
        prior_driver <- as.data.frame(proj[year == out$year[[i - 1L]], ..features])
        out[i, ecm_lag := level_path[[i - 1L]] - as.numeric(predict(long_run_fit, newdata = prior_driver))]
      }
    }

    if (max(abs(level_path - old_level_path), na.rm = TRUE) < tolerance) {
      break
    }
  }

  data.table(
    year = out$year,
    delta = c(level_path[[1]] - previous_y, diff(level_path)),
    value = level_path,
    ecm_lag = out$ecm_lag
  )
}

build_level_paths <- function(model_form, share_dt, dt_est_model, proj,
                              model_inputs, arimax_fit, y_est) {
  if (model_form == "level") {
    fitted_dt <- data.table(
      year = dt_est_model$year,
      series = "Fitted (in-sample)",
      value = arimax_fitted_values(y_est, arimax_fit)
    )

    xreg_proj <- make_xreg(model_inputs$projection_dt, model_inputs$x_features)
    base_proj <- copy(proj[, c("year", features), with = FALSE])
    base_proj[, value := forecast_arimax(arimax_fit, xreg_proj)]
    base_proj[, series := "Projection (base)"]

    return(list(fitted_dt = fitted_dt, base_proj = base_proj, projection_model_dt = model_inputs$projection_dt))
  }

  fitted_delta <- arimax_fitted_values(y_est, arimax_fit)
  fitted_dt <- data.table(
    year = dt_est_model$year,
    series = "Fitted (in-sample)",
    value = dt_est_model$lag_gov_gdp + fitted_delta
  )

  if (model_form == "ECM") {
    ecm_forecast <- forecast_ecm_path(
      arimax_fit = arimax_fit,
      projection_dt = model_inputs$projection_dt,
      share_dt = share_dt,
      proj = proj,
      features = features,
      model_features = model_inputs$x_features,
      long_run_fit = model_inputs$long_run_fit
    )

    projection_model_dt <- copy(model_inputs$projection_dt)
    projection_model_dt[, ecm_lag := ecm_forecast$ecm_lag]

    base_proj <- copy(proj[, c("year", features), with = FALSE])
    base_proj[, value := ecm_forecast$value]
    base_proj[, series := "Projection (base)"]

    return(list(fitted_dt = fitted_dt, base_proj = base_proj, projection_model_dt = projection_model_dt))
  }

  xreg_proj <- make_xreg(model_inputs$projection_dt, model_inputs$x_features)
  delta_path <- forecast_arimax(arimax_fit, xreg_proj)
  last_hist_value <- share_dt[year == max(year, na.rm = TRUE), gov_gdp][[1]]

  base_proj <- copy(proj[, c("year", features), with = FALSE])
  base_proj[, value := last_hist_value + cumsum(delta_path)]
  base_proj[, series := "Projection (base)"]

  list(fitted_dt = fitted_dt, base_proj = base_proj, projection_model_dt = model_inputs$projection_dt)
}

build_arimax_decomposition <- function(share_dt, hist_dt, fitted_dt,
                                       proj, base_proj, features,
                                       arimax_fit, base_year,
                                       include_mean = FALSE) {
  beta <- coef(arimax_fit)[features]

  if (anyNA(beta)) {
    missing_beta <- features[is.na(beta)]
    stop(
      "Could not find ARIMAX coefficients for: ",
      paste(missing_beta, collapse = ", "),
      call. = FALSE
    )
  }

  driver_dt <- rbindlist(
    list(
      share_dt[, c("year", features), with = FALSE],
      proj[, c("year", features), with = FALSE]
    ),
    use.names = TRUE,
    fill = TRUE
  )

  setorder(driver_dt, year)

  if (!base_year %in% driver_dt$year) {
    stop("Decomposition base year is not available in the driver data.", call. = FALSE)
  }

  model_path <- rbindlist(
    list(
      fitted_dt[, .(year, model_value = value, model_series = "Fitted (in-sample)")],
      base_proj[, .(year, model_value = value, model_series = "Projection (base)")]
    ),
    use.names = TRUE
  )

  if (!base_year %in% model_path$year) {
    stop("Decomposition base year is not available in the fitted model path.", call. = FALSE)
  }

  base_x <- driver_dt[year == base_year, ..features]
  base_model <- model_path[year == base_year, model_value][[1]]

  decomp_dt <- merge(model_path, driver_dt, by = "year", all.x = TRUE)
  decomp_dt <- decomp_dt[year >= base_year]

  contrib_cols <- paste0("contrib_", features)

  for (feature in features) {
    decomp_dt[
      ,
      paste0("contrib_", feature) :=
        (as.numeric(get(feature)) - as.numeric(base_x[[feature]])) * as.numeric(beta[[feature]])
    ]
  }

  decomp_dt[, regression_component := rowSums(.SD), .SDcols = contrib_cols]
  decomp_dt[, model_change := model_value - base_model]
  decomp_dt[, arima_component := model_change - regression_component]

  # If the model includes a constant/intercept, that constant does not
  # contribute to changes since the base year: it is the same in every year.
  # We still record the chosen treatment in the output so the decomposition is
  # easy to interpret later.
  decomp_dt[, includes_constant := include_mean]

  decomp_long <- melt(
    decomp_dt[
      ,
      c("year", "model_series", "includes_constant", contrib_cols, "arima_component"),
      with = FALSE
    ],
    id.vars = c("year", "model_series", "includes_constant"),
    variable.name = "driver",
    value.name = "contribution"
  )

  decomp_long[
    ,
    driver := sub("^contrib_", "", driver)
  ]

  driver_labels <- c(
    "0_14" = "Age 0-14",
    "15_34" = "Age 15-34",
    "35_54" = "Age 35-54",
    "55_64" = "Age 55-64",
    "65p" = "Age 65+",
    "tot" = "Terms of trade",
    "rp_g" = "Relative govt prices",
    "unemp" = "Unemployment",
    "arima_component" = "ARIMA dynamics"
  )

  decomp_long[, driver_label := driver_labels[driver]]
  decomp_long[is.na(driver_label), driver_label := driver]

  decomp_totals <- decomp_dt[
    ,
    .(year, model_series, includes_constant, model_change, regression_component, arima_component)
  ]

  actual_base <- hist_dt[year == base_year, value][[1]]
  actual_change <- hist_dt[
    year >= base_year,
    .(year, actual_change = value - actual_base)
  ]

  list(
    long = decomp_long,
    totals = decomp_totals,
    actual = actual_change
  )
}

build_transformed_decomposition <- function(model_dt, projection_model_dt,
                                            hist_dt, fitted_dt, base_proj,
                                            model_features, arimax_fit,
                                            base_year, include_mean = FALSE) {
  beta <- coef(arimax_fit)[model_features]

  if (anyNA(beta)) {
    missing_beta <- model_features[is.na(beta)]
    stop(
      "Could not find ARIMAX coefficients for: ",
      paste(missing_beta, collapse = ", "),
      call. = FALSE
    )
  }

  xreg_path <- rbindlist(
    list(
      model_dt[, c("year", model_features), with = FALSE],
      projection_model_dt[, c("year", model_features), with = FALSE]
    ),
    use.names = TRUE,
    fill = TRUE
  )
  setorder(xreg_path, year)

  model_path <- rbindlist(
    list(
      fitted_dt[, .(year, model_value = value, model_series = "Fitted (in-sample)")],
      base_proj[, .(year, model_value = value, model_series = "Projection (base)")]
    ),
    use.names = TRUE
  )
  setorder(model_path, year)

  if (!base_year %in% model_path$year) {
    stop("Decomposition base year is not available in the fitted model path.", call. = FALSE)
  }

  decomp_dt <- merge(model_path, xreg_path, by = "year", all.x = TRUE)
  decomp_dt <- decomp_dt[year >= base_year]
  base_model <- model_path[year == base_year, model_value][[1]]

  contrib_cols <- paste0("contrib_", model_features)

  for (feature in model_features) {
    decomp_dt[, paste0("contrib_", feature) := as.numeric(get(feature)) * as.numeric(beta[[feature]])]
    decomp_dt[year <= base_year, paste0("contrib_", feature) := 0]
  }

  # The transformed models explain annual changes. Contributions to the level
  # change since the base year are therefore cumulative sums of annual
  # contribution terms.
  setorder(decomp_dt, year)
  for (contrib_col in contrib_cols) {
    decomp_dt[, (contrib_col) := cumsum(fifelse(is.na(get(contrib_col)), 0, get(contrib_col)))]
  }

  intercept_name <- intersect(c("intercept", "mean"), names(coef(arimax_fit)))
  if (isTRUE(include_mean) && length(intercept_name)) {
    intercept_value <- as.numeric(coef(arimax_fit)[intercept_name[[1]]])
    decomp_dt[, constant_component := intercept_value * seq(0, .N - 1L)]
  } else {
    decomp_dt[, constant_component := 0]
  }

  decomp_dt[, regression_component := rowSums(.SD), .SDcols = contrib_cols]
  decomp_dt[, model_change := model_value - base_model]
  decomp_dt[, arima_component := model_change - regression_component - constant_component]
  decomp_dt[, includes_constant := include_mean]

  decomp_long <- melt(
    decomp_dt[
      ,
      c("year", "model_series", "includes_constant", contrib_cols, "constant_component", "arima_component"),
      with = FALSE
    ],
    id.vars = c("year", "model_series", "includes_constant"),
    variable.name = "driver",
    value.name = "contribution"
  )

  decomp_long[, driver := sub("^contrib_", "", driver)]

  driver_labels <- c(
    "d_0_14" = "Change in age 0-14",
    "d_15_34" = "Change in age 15-34",
    "d_35_54" = "Change in age 35-54",
    "d_55_64" = "Change in age 55-64",
    "d_65p" = "Change in age 65+",
    "d_tot" = "Change in terms of trade",
    "d_rp_g" = "Change in relative govt prices",
    "d_unemp" = "Change in unemployment",
    "ecm_lag" = "Lagged long-run gap",
    "constant_component" = "Constant",
    "arima_component" = "ARIMA dynamics"
  )

  decomp_long[, driver_label := driver_labels[driver]]
  decomp_long[is.na(driver_label), driver_label := driver]

  decomp_totals <- decomp_dt[
    ,
    .(
      year,
      model_series,
      includes_constant,
      model_change,
      regression_component,
      constant_component,
      arima_component
    )
  ]

  actual_base <- hist_dt[year == base_year, value][[1]]
  actual_change <- hist_dt[
    year >= base_year,
    .(year, actual_change = value - actual_base)
  ]

  list(
    long = decomp_long,
    totals = decomp_totals,
    actual = actual_change
  )
}

calculate_vif <- function(dt, features) {
  # VIFs diagnose whether the regressors are close to linear combinations of
  # each other. This is especially important for age shares, which move slowly
  # and are mechanically connected because they sum to one.
  rbindlist(lapply(features, function(feature) {
    other_features <- setdiff(features, feature)

    if (!length(other_features)) {
      return(data.table(feature = feature, r_squared = NA_real_, vif = NA_real_))
    }

    model_dt <- as.data.frame(dt[, c(feature, other_features), with = FALSE])
    names(model_dt)[names(model_dt) == feature] <- "target"

    fit <- tryCatch(
      lm(target ~ ., data = model_dt),
      error = function(e) NULL
    )

    if (is.null(fit)) {
      return(data.table(feature = feature, r_squared = NA_real_, vif = NA_real_))
    }

    r2 <- summary(fit)$r.squared
    vif <- if (is.finite(r2) && r2 < 1) 1 / (1 - r2) else Inf

    data.table(feature = feature, r_squared = r2, vif = vif)
  }))
}

coefficient_diagnostics <- function(arimax_fit) {
  estimates <- coef(arimax_fit)
  se <- rep(NA_real_, length(estimates))

  if (!is.null(arimax_fit$var.coef)) {
    se <- sqrt(diag(arimax_fit$var.coef))
  }

  out <- data.table(
    term = names(estimates),
    estimate = as.numeric(estimates),
    std_error = as.numeric(se)
  )

  out[, t_stat := estimate / std_error]
  out
}

root_diagnostics <- function(arimax_fit) {
  coefs <- coef(arimax_fit)
  ar_coefs <- coefs[grepl("^ar", names(coefs))]
  ma_coefs <- coefs[grepl("^ma", names(coefs))]

  ar_roots <- if (length(ar_coefs)) {
    data.table(
      component = "AR",
      root = seq_along(ar_coefs),
      modulus = Mod(polyroot(c(1, -as.numeric(ar_coefs))))
    )
  } else {
    data.table(component = "AR", root = integer(), modulus = numeric())
  }

  ma_roots <- if (length(ma_coefs)) {
    data.table(
      component = "MA",
      root = seq_along(ma_coefs),
      modulus = Mod(polyroot(c(1, as.numeric(ma_coefs))))
    )
  } else {
    data.table(component = "MA", root = integer(), modulus = numeric())
  }

  out <- rbindlist(list(ar_roots, ma_roots), use.names = TRUE)
  out[, near_unit_circle := modulus < 1.1]
  out
}

residual_diagnostics <- function(arimax_fit, p, q) {
  residuals_dt <- data.table(
    index = seq_along(residuals(arimax_fit)),
    residual = as.numeric(residuals(arimax_fit))
  )

  summary_dt <- residuals_dt[
    ,
    .(
      mean = mean(residual, na.rm = TRUE),
      sd = sd(residual, na.rm = TRUE),
      min = min(residual, na.rm = TRUE),
      max = max(residual, na.rm = TRUE)
    )
  ]

  fitdf <- p + q
  box_dt <- rbindlist(lapply(c(4, 8, 12), function(lag_value) {
    test <- tryCatch(
      Box.test(residuals_dt$residual, lag = lag_value, type = "Ljung-Box", fitdf = fitdf),
      error = function(e) NULL
    )

    if (is.null(test)) {
      return(data.table(lag = lag_value, statistic = NA_real_, p_value = NA_real_))
    }

    data.table(
      lag = lag_value,
      statistic = as.numeric(test$statistic),
      p_value = as.numeric(test$p.value)
    )
  }))

  acf_values <- as.numeric(acf(residuals_dt$residual, plot = FALSE, na.action = na.pass)$acf)
  acf_dt <- data.table(
    lag = seq_along(acf_values) - 1L,
    acf = acf_values
  )

  list(
    residuals = residuals_dt,
    summary = summary_dt,
    ljung_box = box_dt,
    acf = acf_dt
  )
}

stationarity_diagnostics <- function(dt, variables) {
  # ADF/KPSS tests are useful here, but they require the optional tseries
  # package. If it is not installed, write a clear diagnostic row instead of
  # making the whole workflow fail.
  if (!requireNamespace("tseries", quietly = TRUE)) {
    return(data.table(
      variable = variables,
      test = "not run",
      statistic = NA_real_,
      p_value = NA_real_,
      note = "Install the tseries package to run ADF and KPSS tests."
    ))
  }

  rbindlist(lapply(variables, function(variable) {
    x <- as.numeric(dt[[variable]])
    x <- x[is.finite(x)]

    adf <- tryCatch(
      suppressWarnings(tseries::adf.test(x)),
      error = function(e) NULL
    )
    kpss <- tryCatch(
      suppressWarnings(tseries::kpss.test(x)),
      error = function(e) NULL
    )

    rbindlist(list(
      if (is.null(adf)) {
        data.table(variable = variable, test = "ADF", statistic = NA_real_, p_value = NA_real_, note = "test failed")
      } else {
        data.table(variable = variable, test = "ADF", statistic = as.numeric(adf$statistic), p_value = adf$p.value, note = "")
      },
      if (is.null(kpss)) {
        data.table(variable = variable, test = "KPSS", statistic = NA_real_, p_value = NA_real_, note = "test failed")
      } else {
        data.table(variable = variable, test = "KPSS", statistic = as.numeric(kpss$statistic), p_value = kpss$p.value, note = "")
      }
    ), use.names = TRUE)
  }), use.names = TRUE)
}

driver_range_diagnostics <- function(dt_est, proj, features) {
  rbindlist(lapply(features, function(feature) {
    hist_min <- min(dt_est[[feature]], na.rm = TRUE)
    hist_max <- max(dt_est[[feature]], na.rm = TRUE)
    proj_min <- min(proj[[feature]], na.rm = TRUE)
    proj_max <- max(proj[[feature]], na.rm = TRUE)

    data.table(
      feature = feature,
      historical_min = hist_min,
      historical_max = hist_max,
      projection_min = proj_min,
      projection_max = proj_max,
      projection_below_history = proj_min < hist_min,
      projection_above_history = proj_max > hist_max
    )
  }))
}

rolling_forecast_diagnostics <- function(dt_est, features, include_mean,
                                         y_col = "gov_gdp",
                                         d_grid = 0:1,
                                         min_origin = 2010,
                                         horizon = 5) {
  max_year <- max(dt_est$year, na.rm = TRUE)
  origins <- min_origin:(max_year - 1L)

  out <- rbindlist(lapply(origins, function(origin_year) {
    train <- dt_est[year <= origin_year]
    test <- dt_est[year > origin_year][1:horizon]
    test <- test[!is.na(year)]

    if (nrow(train) < 20 || !nrow(test)) {
      return(NULL)
    }

    fit_result <- tryCatch(
      fit_arimax_grid(
        y = as.numeric(train[[y_col]]),
        xreg = make_xreg(train, features),
        include_mean = include_mean,
        d_grid = d_grid
      ),
      error = function(e) NULL
    )

    if (is.null(fit_result)) {
      return(NULL)
    }

    pred <- tryCatch(
      forecast_arimax(fit_result$fit, make_xreg(test, features)),
      error = function(e) rep(NA_real_, nrow(test))
    )

    data.table(
      origin_year = origin_year,
      forecast_year = test$year,
      horizon = seq_len(nrow(test)),
      actual = test[[y_col]],
      forecast = pred,
      error = pred - test[[y_col]],
      p = fit_result$order[[1]],
      d = fit_result$order[[2]],
      q = fit_result$order[[3]],
      aic = fit_result$fit$aic
    )
  }), use.names = TRUE, fill = TRUE)

  if (!nrow(out)) {
    return(data.table())
  }

  out[, abs_error := abs(error)]
  out[, squared_error := error^2]
  out
}

model_sensitivity_diagnostics <- function(dt_est, proj, features,
                                          age_features_all, economic_features,
                                          include_mean,
                                          y_col = "gov_gdp",
                                          d_grid = 0:1) {
  candidate_sets <- list(
    full = features,
    age_only = intersect(features, c(age_features_all, paste0("d_", age_features_all))),
    economic_only = intersect(features, c(economic_features, paste0("d_", economic_features))),
    no_terms_of_trade = setdiff(features, c("tot", "d_tot")),
    no_relative_prices = setdiff(features, c("rp_g", "d_rp_g")),
    no_unemployment = setdiff(features, c("unemp", "d_unemp")),
    no_error_correction = setdiff(features, "ecm_lag")
  )

  rbindlist(lapply(names(candidate_sets), function(model_name) {
    feature_set <- candidate_sets[[model_name]]

    if (!length(feature_set)) {
      return(NULL)
    }

    fit_result <- tryCatch(
      fit_arimax_grid(
        y = as.numeric(dt_est[[y_col]]),
        xreg = make_xreg(dt_est, feature_set),
        include_mean = include_mean,
        d_grid = d_grid
      ),
      error = function(e) NULL
    )

    if (is.null(fit_result)) {
      return(data.table(
        model_name = model_name,
        status = "failed",
        features = paste(feature_set, collapse = ", ")
      ))
    }

    forecast_path <- tryCatch(
      forecast_arimax(fit_result$fit, make_xreg(proj, feature_set)),
      error = function(e) rep(NA_real_, nrow(proj))
    )

    data.table(
      model_name = model_name,
      status = "ok",
      features = paste(feature_set, collapse = ", "),
      p = fit_result$order[[1]],
      d = fit_result$order[[2]],
      q = fit_result$order[[3]],
      aic = fit_result$fit$aic,
      first_projection_year = proj$year[[1]],
      first_projection_value = forecast_path[[1]],
      final_projection_year = proj$year[[nrow(proj)]],
      final_projection_value = forecast_path[[length(forecast_path)]]
    )
  }), use.names = TRUE, fill = TRUE)
}

write_diagnostics <- function(diagnostics_dir, output_stub,
                              dt_est, share_dt, proj, features,
                              arimax_fit, arimax_order,
                              include_mean, age_features_all,
                              economic_features,
                              y_col = "gov_gdp",
                              d_grid = 0:1,
                              cointegration = NULL) {
  dir.create(diagnostics_dir, showWarnings = FALSE, recursive = TRUE)

  coefficient_dt <- coefficient_diagnostics(arimax_fit)
  vif_dt <- calculate_vif(dt_est, features)
  correlation_dt <- as.data.table(cor(dt_est[, ..features], use = "pairwise.complete.obs"), keep.rownames = "feature")
  root_dt <- root_diagnostics(arimax_fit)
  residuals_out <- residual_diagnostics(arimax_fit, p = arimax_order[[1]], q = arimax_order[[3]])
  stationarity_dt <- stationarity_diagnostics(dt_est, c(y_col, features))
  range_dt <- driver_range_diagnostics(dt_est, proj, features)
  rolling_dt <- rolling_forecast_diagnostics(
    dt_est,
    features,
    include_mean = include_mean,
    y_col = y_col,
    d_grid = d_grid
  )
  sensitivity_dt <- model_sensitivity_diagnostics(
    dt_est = dt_est,
    proj = proj,
    features = features,
    age_features_all = age_features_all,
    economic_features = economic_features,
    include_mean = include_mean,
    y_col = y_col,
    d_grid = d_grid
  )

  cointegration_test_dt <- if (is.null(cointegration)) {
    data.table(
      test = "Engle-Granger residual ADF",
      statistic = NA_real_,
      p_value = NA_real_,
      conclusion = "not available",
      note = "Cointegration diagnostic was not supplied."
    )
  } else {
    cointegration$test
  }

  cointegration_residual_dt <- if (is.null(cointegration)) {
    data.table(year = integer(), cointegration_residual = numeric())
  } else {
    cointegration$residuals
  }

  # Write an index first so the diagnostics folder is navigable from RStudio.
  # The index is deliberately plain CSV: it can be opened directly or joined
  # back to other diagnostic tables if we later add automated flags.
  diagnostic_index <- data.table(
    file = c(
      paste0("diagnostic_coefficients_", output_stub, ".csv"),
      paste0("diagnostic_vif_", output_stub, ".csv"),
      paste0("diagnostic_correlations_", output_stub, ".csv"),
      paste0("diagnostic_arima_roots_", output_stub, ".csv"),
      paste0("diagnostic_residuals_", output_stub, ".csv"),
      paste0("diagnostic_residual_summary_", output_stub, ".csv"),
      paste0("diagnostic_ljung_box_", output_stub, ".csv"),
      paste0("diagnostic_residual_acf_", output_stub, ".csv"),
      paste0("diagnostic_stationarity_", output_stub, ".csv"),
      paste0("diagnostic_driver_ranges_", output_stub, ".csv"),
      paste0("diagnostic_rolling_forecasts_", output_stub, ".csv"),
      paste0("diagnostic_model_sensitivity_", output_stub, ".csv"),
      paste0("diagnostic_cointegration_test_", output_stub, ".csv"),
      paste0("diagnostic_cointegration_residuals_", output_stub, ".csv"),
      paste0("diagnostic_coefficients_", output_stub, ".png"),
      paste0("diagnostic_rolling_forecasts_", output_stub, ".png"),
      paste0("diagnostic_driver_ranges_", output_stub, ".png")
    ),
    purpose = c(
      "Coefficient estimates, standard errors, t statistics, and approximate p-values.",
      "Variance inflation factors for the external regressors; large values point to multicollinearity.",
      "Pairwise correlations among the external regressors.",
      "AR and MA root moduli; values close to 1 suggest near-unit-root or near-noninvertible dynamics.",
      "Year-by-year ARIMAX residuals from the estimation sample.",
      "Residual mean, standard deviation, RMSE, MAE, and extrema.",
      "Ljung-Box residual autocorrelation tests at several lags.",
      "Residual autocorrelation function by lag.",
      "ADF and KPSS stationarity checks if the tseries package is installed; otherwise records that they were skipped.",
      "Historical and projection ranges for each driver, with out-of-range flags.",
      "Rolling holdout forecasts using actual future driver values.",
      "Projection sensitivity to alternative regressor sets.",
      "Engle-Granger residual ADF diagnostic for a long-run levels relationship.",
      "Residuals from the estimated long-run levels relationship used by the cointegration diagnostic and ECM.",
      "Coefficient plot with approximate 95 percent intervals.",
      "Rolling forecast plot against the historical spending/GDP series.",
      "Driver range plot comparing estimation and projection ranges."
    ),
    first_place_to_look = c(
      "Check signs, magnitudes, and whether uncertainty intervals are wide.",
      "Check whether age shares or macro variables are too collinear to interpret separately.",
      "Use alongside VIFs to identify which variables move together.",
      "Check the near_unit_circle flag and root_modulus values.",
      "Plot or inspect for large one-off errors and clustered misses.",
      "Use RMSE and MAE as compact fit summaries.",
      "Small p-values indicate residual autocorrelation remains after the ARIMA component.",
      "Look for persistent autocorrelation at low lags.",
      "Non-stationary regressors can create unstable coefficient estimates and spurious relationships.",
      "Out-of-range projection drivers are extrapolation warnings.",
      "Compare forecast and actual values by origin year and horizon.",
      "Compare first and final projection values across model variants.",
      "If this does not show evidence of a stationary long-run gap, be cautious about the ECM option.",
      "Plot these residuals to see whether the long-run gap looks mean reverting.",
      "Visual companion to the coefficient CSV.",
      "Visual companion to the rolling forecast CSV.",
      "Visual companion to the driver range CSV."
    )
  )

  fwrite(diagnostic_index, file.path(diagnostics_dir, paste0("diagnostic_index_", output_stub, ".csv")))
  fwrite(coefficient_dt, file.path(diagnostics_dir, paste0("diagnostic_coefficients_", output_stub, ".csv")))
  fwrite(vif_dt, file.path(diagnostics_dir, paste0("diagnostic_vif_", output_stub, ".csv")))
  fwrite(correlation_dt, file.path(diagnostics_dir, paste0("diagnostic_correlations_", output_stub, ".csv")))
  fwrite(root_dt, file.path(diagnostics_dir, paste0("diagnostic_arima_roots_", output_stub, ".csv")))
  fwrite(residuals_out$residuals, file.path(diagnostics_dir, paste0("diagnostic_residuals_", output_stub, ".csv")))
  fwrite(residuals_out$summary, file.path(diagnostics_dir, paste0("diagnostic_residual_summary_", output_stub, ".csv")))
  fwrite(residuals_out$ljung_box, file.path(diagnostics_dir, paste0("diagnostic_ljung_box_", output_stub, ".csv")))
  fwrite(residuals_out$acf, file.path(diagnostics_dir, paste0("diagnostic_residual_acf_", output_stub, ".csv")))
  fwrite(stationarity_dt, file.path(diagnostics_dir, paste0("diagnostic_stationarity_", output_stub, ".csv")))
  fwrite(range_dt, file.path(diagnostics_dir, paste0("diagnostic_driver_ranges_", output_stub, ".csv")))
  fwrite(rolling_dt, file.path(diagnostics_dir, paste0("diagnostic_rolling_forecasts_", output_stub, ".csv")))
  fwrite(sensitivity_dt, file.path(diagnostics_dir, paste0("diagnostic_model_sensitivity_", output_stub, ".csv")))
  fwrite(cointegration_test_dt, file.path(diagnostics_dir, paste0("diagnostic_cointegration_test_", output_stub, ".csv")))
  fwrite(cointegration_residual_dt, file.path(diagnostics_dir, paste0("diagnostic_cointegration_residuals_", output_stub, ".csv")))

  coefficient_plot <- ggplot(
    coefficient_dt[!grepl("^ar|^ma|intercept", term)],
    aes(x = reorder(term, estimate), y = estimate)
  ) +
    geom_col(fill = "#3777B8") +
    geom_errorbar(aes(ymin = estimate - 1.96 * std_error, ymax = estimate + 1.96 * std_error), width = 0.2) +
    coord_flip() +
    labs(
      title = "ARIMAX coefficient diagnostics",
      subtitle = "Bars are estimates; whiskers are approximate 95% intervals",
      x = NULL,
      y = "Coefficient"
    ) +
    theme_minimal(base_size = 11)

  ggplot2::ggsave(
    file.path(diagnostics_dir, paste0("diagnostic_coefficients_", output_stub, ".png")),
    coefficient_plot,
    width = 8,
    height = 5.5,
    units = "in",
    dpi = 300
  )

  rolling_actual_dt <- dt_est[, .(year, actual_value = as.numeric(get(y_col)))]
  rolling_y_label <- if (y_col == "gov_gdp") "% of GDP" else "Annual change in spending/GDP, percentage points"

  rolling_plot <- if (nrow(rolling_dt)) {
    ggplot() +
      geom_line(
        data = rolling_actual_dt,
        aes(x = year, y = actual_value * 100),
        colour = "black",
        linewidth = 0.9
      ) +
      geom_line(
        data = rolling_dt,
        aes(x = forecast_year, y = forecast * 100, group = origin_year),
        colour = "#C14953",
        alpha = 0.35
      ) +
      labs(
        title = "Rolling ARIMAX forecast diagnostics",
        subtitle = "Black line is actual model outcome; red lines are rolling holdout forecasts",
        x = NULL,
        y = rolling_y_label
      ) +
      theme_minimal(base_size = 11)
  } else {
    ggplot() +
      annotate("text", x = 0, y = 0, label = "Rolling forecasts were not available.") +
      theme_void()
  }

  ggplot2::ggsave(
    file.path(diagnostics_dir, paste0("diagnostic_rolling_forecasts_", output_stub, ".png")),
    rolling_plot,
    width = 8,
    height = 5.5,
    units = "in",
    dpi = 300
  )

  range_plot <- ggplot(range_dt, aes(x = feature)) +
    geom_linerange(aes(ymin = historical_min, ymax = historical_max), linewidth = 1.1, colour = "black") +
    geom_linerange(aes(ymin = projection_min, ymax = projection_max), linewidth = 0.8, colour = "#C14953") +
    coord_flip() +
    labs(
      title = "Historical and projected driver ranges",
      subtitle = "Black = estimation sample range; red = projection range",
      x = NULL,
      y = "Driver value"
    ) +
    theme_minimal(base_size = 11)

  ggplot2::ggsave(
    file.path(diagnostics_dir, paste0("diagnostic_driver_ranges_", output_stub, ".png")),
    range_plot,
    width = 8,
    height = 5.5,
    units = "in",
    dpi = 300
  )

  invisible(list(
    coefficients = coefficient_dt,
    vif = vif_dt,
    correlations = correlation_dt,
    roots = root_dt,
    residuals = residuals_out,
    stationarity = stationarity_dt,
    ranges = range_dt,
    rolling = rolling_dt,
    sensitivity = sensitivity_dt,
    cointegration_test = cointegration_test_dt,
    cointegration_residuals = cointegration_residual_dt,
    index = diagnostic_index
  ))
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

  # Only the selected model features must be complete. If no_intercept = FALSE,
  # the omitted age group can still be present in the workbook, but it is not
  # used directly in xreg because it is absorbed into the intercept/reference
  # category interpretation.
  proj <- proj[complete.cases(proj[, ..features])]

  if (!nrow(proj)) {
    stop("Projection workbook has no complete rows for the selected features.", call. = FALSE)
  }

  setorder(proj, year)
  proj
}

share_dt <- prepare_historical_model_data()
dt_est_level <- share_dt[!(year %in% outlier_years)]

if (nrow(dt_est_level) < 10) {
  stop("Not enough observations after excluding outlier years.", call. = FALSE)
}

proj <- prepare_projection_drivers(share_dt)
cointegration <- cointegration_diagnostics(
  dt = dt_est_level,
  features = features,
  include_mean = include_mean
)

model_inputs <- build_model_inputs(
  share_dt = share_dt,
  dt_est_level = dt_est_level,
  proj = proj,
  features = features,
  model_form = model_form,
  include_mean = include_mean,
  cointegration = cointegration,
  age_features_all = age_features_all,
  reference_age_group = reference_age_group
)

dt_est <- model_inputs$estimation_dt
model_features <- model_inputs$x_features
y_col <- model_inputs$y_col

if (nrow(dt_est) < 10) {
  stop("Not enough observations to estimate the selected model form.", call. = FALSE)
}

y_est <- as.numeric(dt_est[[y_col]])
xreg_est <- make_xreg(dt_est, model_features)

arima_d_grid <- if (model_form == "level") 0:1 else 0

arimax_result <- fit_arimax_grid(
  y = y_est,
  xreg = xreg_est,
  include_mean = include_mean,
  d_grid = arima_d_grid
)
arimax_fit <- arimax_result$fit
arimax_order <- arimax_result$order

hist_dt <- data.table(
  year = share_dt$year,
  series = "Historical",
  value = share_dt$gov_gdp
)

level_paths <- build_level_paths(
  model_form = model_form,
  share_dt = share_dt,
  dt_est_model = dt_est,
  proj = proj,
  model_inputs = model_inputs,
  arimax_fit = arimax_fit,
  y_est = y_est
)

fitted_dt <- level_paths$fitted_dt
base_proj <- level_paths$base_proj
projection_model_dt <- level_paths$projection_model_dt

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
  term = names(coef(arimax_fit)),
  estimate = as.numeric(coef(arimax_fit))
)

model_summary <- data.table(
  model = "ARIMAX",
  model_form = model_form,
  model_variant = model_variant,
  no_intercept = no_intercept,
  include_mean = include_mean,
  reference_age_group = if (isTRUE(no_intercept) && model_form == "level") NA_character_ else reference_age_group,
  specification_note = specification_note,
  level_features = paste(features, collapse = ", "),
  model_features = paste(model_features, collapse = ", "),
  outcome = y_col,
  p = arimax_order[[1]],
  d = arimax_order[[2]],
  q = arimax_order[[3]],
  aic = arimax_fit$aic,
  sigma2 = arimax_fit$sigma2,
  n_estimation_years = length(y_est),
  excluded_years = paste(outlier_years, collapse = ", "),
  cointegration_test = cointegration$test$test[[1]],
  cointegration_p_value = cointegration$test$p_value[[1]],
  cointegration_conclusion = cointegration$test$conclusion[[1]]
)

decomposition <- if (model_form == "level") {
  build_arimax_decomposition(
    share_dt = share_dt,
    hist_dt = hist_dt,
    fitted_dt = fitted_dt,
    proj = proj,
    base_proj = base_proj,
    features = features,
    arimax_fit = arimax_fit,
    base_year = decomposition_base_year,
    include_mean = include_mean
  )
} else {
  build_transformed_decomposition(
    model_dt = dt_est,
    projection_model_dt = projection_model_dt,
    hist_dt = hist_dt,
    fitted_dt = fitted_dt,
    base_proj = base_proj,
    model_features = model_features,
    arimax_fit = arimax_fit,
    base_year = decomposition_base_year,
    include_mean = include_mean
  )
}

output_stub <- paste0(measure, "_", level, "_", model_variant)
diagnostics_dir <- file.path(output_dir, "diagnostics")

fwrite(
  plot_dt,
  file.path(output_dir, paste0("arimax_spending_projection_history_fit_projection_", output_stub, ".csv"))
)
fwrite(
  debt_path_input,
  file.path(output_dir, paste0("arimax_spending_path_for_debt_projection_", output_stub, ".csv"))
)
fwrite(
  model_coefficients,
  file.path(output_dir, paste0("arimax_spending_projection_coefficients_", output_stub, ".csv"))
)
fwrite(
  model_summary,
  file.path(output_dir, paste0("arimax_spending_projection_model_summary_", output_stub, ".csv"))
)
fwrite(
  share_dt,
  file.path(output_dir, paste0("arimax_spending_projection_historical_model_data_", output_stub, ".csv"))
)
fwrite(
  dt_est,
  file.path(output_dir, paste0("arimax_spending_projection_estimation_data_", output_stub, ".csv"))
)
fwrite(
  proj,
  file.path(output_dir, paste0("arimax_spending_projection_forward_drivers_", output_stub, ".csv"))
)
fwrite(
  projection_model_dt,
  file.path(output_dir, paste0("arimax_spending_projection_forward_model_data_", output_stub, ".csv"))
)
fwrite(
  decomposition$long,
  file.path(
    output_dir,
    paste0(
      "arimax_spending_change_decomposition_since_",
      decomposition_base_year,
      "_",
      output_stub,
      ".csv"
    )
  )
)
fwrite(
  decomposition$totals,
  file.path(
    output_dir,
    paste0(
      "arimax_spending_change_decomposition_totals_since_",
      decomposition_base_year,
      "_",
      output_stub,
      ".csv"
    )
  )
)

diagnostics <- write_diagnostics(
  diagnostics_dir = diagnostics_dir,
  output_stub = output_stub,
  dt_est = dt_est,
  share_dt = share_dt,
  proj = projection_model_dt,
  features = model_features,
  arimax_fit = arimax_fit,
  arimax_order = arimax_order,
  include_mean = include_mean,
  age_features_all = age_features_all,
  economic_features = economic_features,
  y_col = y_col,
  d_grid = arima_d_grid,
  cointegration = cointegration
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
    subtitle = paste0(
      "ARIMAX(",
      paste(arimax_order, collapse = ", "),
      "), ",
      specification_note,
      "; outlier years excluded: ",
      paste(outlier_years, collapse = ", ")
    ),
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
  file.path(output_dir, paste0("arimax_spending_projection_", output_stub, ".png")),
  projection_plot,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

ggplot2::ggsave(
  file.path(output_dir, paste0("arimax_spending_projection_", output_stub, ".svg")),
  projection_plot,
  width = 8,
  height = 6,
  units = "in"
)

decomposition_plot <- ggplot(
  decomposition$long,
  aes(x = year, y = contribution * 100, fill = driver_label)
) +
  geom_col(width = 0.85) +
  geom_line(
    data = decomposition$totals,
    aes(x = year, y = model_change * 100, group = 1),
    inherit.aes = FALSE,
    linewidth = 0.9,
    colour = "black"
  ) +
  geom_line(
    data = decomposition$actual,
    aes(x = year, y = actual_change * 100, group = 1),
    inherit.aes = FALSE,
    linewidth = 0.8,
    linetype = "dashed",
    colour = "grey30"
  ) +
  geom_vline(xintercept = max(share_dt$year), linetype = "dotted", colour = "grey45") +
  labs(
    title = paste0("Drivers of change in government ", measure, " since ", decomposition_base_year),
    subtitle = paste0(
      "Bars decompose the ARIMAX fitted/projection change; solid line is total model change; dashed line is actual historical change. ",
      specification_note,
      "."
    ),
    x = NULL,
    y = "Percentage points of GDP",
    fill = NULL,
    caption = "Sources: ABS, e61 projections."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

ggplot2::ggsave(
  file.path(
    output_dir,
    paste0(
      "arimax_spending_change_decomposition_since_",
      decomposition_base_year,
      "_",
      output_stub,
      ".png"
    )
  ),
  decomposition_plot,
  width = 9,
  height = 6.5,
  units = "in",
  dpi = 300
)

ggplot2::ggsave(
  file.path(
    output_dir,
    paste0(
      "arimax_spending_change_decomposition_since_",
      decomposition_base_year,
      "_",
      output_stub,
      ".svg"
    )
  ),
  decomposition_plot,
  width = 9,
  height = 6.5,
  units = "in"
)

message("ARIMAX spending projection outputs written to: ", normalizePath(output_dir))

# -----------------------------------------------------------------------------
# Top-down government spending model functions
# Updated version: adds variable-specific dynamics and an explicit ECM
# -----------------------------------------------------------------------------
#
# Reported model families
# -----------------------
# 1. Structural OLS in levels.
# 2. Levels ARIMAX with stationary ARMA errors.
# 3. A single differenced ARIMAX using variable-appropriate transformations:
#      - the selected age shares enter as annual changes;
#      - relative government prices enter as annual changes;
#      - unemployment enters as current and lagged CHANGES. In a pure
#        differenced model, putting the unemployment level directly on the RHS
#        would imply repeated spending growth for every year unemployment stays
#        high, producing an undesirable permanent drift;
#      - the terms of trade enter as current and lagged annual changes;
#      - COVID is represented by separate annual intervention indicators.
#
# 4. A hybrid structural-level/macro-change model.
# 5. A parsimonious augmented-Mann ECM. Its candidate long-run vector is log
#    spending/GDP, log real GDP per capita and the log relative government
#    price; unemployment and terms of trade enter as short-run changes.
#
#    The coefficient on lagged spending (`y_lag`) is the adjustment parameter.
#    For a stable error-correction mechanism it should normally be negative.
#    Long-run coefficients are approximately -beta_level / beta_y_lag.
#
# AIC/AICc/BIC are recorded where available. These should NOT be
#    used to rank models that have different dependent-variable transformations
#    (for example, a levels model versus a differenced model). Use rolling
#    pseudo-out-of-sample forecast errors for cross-model comparison.
#
# Dependencies are intentionally unchanged: only data.table and forecast.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(forecast)
})


# -----------------------------------------------------------------------------
# Feature helpers
# -----------------------------------------------------------------------------

active_age_groups <- function() {
  if (exists("topdown_age_groups", inherits = TRUE)) {
    get("topdown_age_groups", inherits = TRUE)
  } else {
    c("0_14", "15_34", "55_64", "65p")
  }
}


covid_intervention_years <- function() {
  if (exists("topdown_covid_years", inherits = TRUE)) {
    as.integer(get("topdown_covid_years", inherits = TRUE))
  } else {
    2020:2022
  }
}


age_level_terms <- function(suffix = "") {
  paste0("age_", active_age_groups(), suffix)
}


covid_dummy_terms <- function() {
  paste0("covid_fy", covid_intervention_years())
}


scale_driver_columns <- c(
  population = "log_population",
  real_gdp_per_capita = "log_real_gdp_per_capita"
)


active_scale_drivers <- function(drivers = NULL) {
  if (is.null(drivers)) {
    drivers <- if (exists("topdown_scale_drivers", inherits = TRUE)) {
      get("topdown_scale_drivers", inherits = TRUE)
    } else {
      character()
    }
  }
  drivers <- unique(as.character(drivers))
  unknown <- setdiff(drivers, names(scale_driver_columns))
  if (length(unknown)) {
    stop("Unknown top-down scale/income drivers: ", paste(unknown, collapse = ", "))
  }
  drivers
}


scale_level_terms <- function(suffix = "", drivers = NULL) {
  selected <- active_scale_drivers(drivers)
  if (!length(selected)) return(character())
  paste0(unname(scale_driver_columns[selected]), suffix)
}


scale_difference_terms <- function(drivers = NULL) {
  terms <- scale_level_terms(drivers = drivers)
  if (!length(terms)) return(character())
  paste0("d_", terms)
}


scale_feature_frame <- function(dt, drivers = NULL) {
  terms <- scale_level_terms(drivers = drivers)
  if (!length(terms)) return(data.frame(row.names = seq_len(nrow(dt))))
  missing_terms <- setdiff(terms, names(dt))
  if (length(missing_terms)) {
    stop("Missing top-down scale/income variables: ", paste(missing_terms, collapse = ", "))
  }
  out <- lapply(terms, function(term) as.numeric(dt[[term]]))
  names(out) <- terms
  as.data.frame(out, check.names = FALSE)
}


age_feature_frame <- function(dt) {
  out <- lapply(active_age_groups(), function(group) as.numeric(dt[[group]]))
  names(out) <- age_level_terms()
  as.data.frame(out, check.names = FALSE)
}


active_demographic_specification <- function(specification = NULL) {
  if (is.null(specification)) {
    specification <- if (exists("topdown_demographic_specification", inherits = TRUE)) {
      get("topdown_demographic_specification", inherits = TRUE)
    } else "age_shares"
  }
  specification <- match.arg(specification, c("age_shares", "expenditure_profile"))
  specification
}


age_profile_relative_weights <- function() {
  # These grouped relativities are the transparent baseline judgements already
  # used by the bottom-up model. They are not estimates from ABS microdata.
  data.table(
    exposure = rep(c("total_population", "health_age_weight",
                     "education_age_weight", "social_age_weight"), each = 5L),
    age_group = rep(c("0_14", "15_34", "35_54", "55_64", "65p"), 4L),
    relative_weight = c(
      rep(1, 5),
      0.65, 0.75, 0.90, 1.30, 3.00,
      1.00, 0.35, 0.05, 0.00, 0.00,
      0.60, 0.50, 0.40, 0.80, 2.20
    ),
    weight_status = c(
      rep("Flat allocation assumption", 5),
      rep("Judgemental grouped proxy pending administrative age-cost data", 15)
    )
  )
}


age_profile_category_table <- function() {
  purpose <- fread(file.path(processed_dir, "historical_gfs_expenses_by_purpose.csv"))
  operating <- fread(file.path(processed_dir, "historical_gfs_operating_statement.csv"))
  assumptions <- fread(file.path(processed_dir, "bottom_up_category_assumptions.csv"))
  base_year <- 2025L
  base <- purpose[year == base_year, .(category = item, base_expenditure = value)]
  interest <- operating[year == base_year & item == "Interest expenses n.e.c.", value]
  base[category == "General public services", `:=`(
    category = "General public services excl interest",
    base_expenditure = base_expenditure - interest
  )]
  base <- merge(base, assumptions[, .(category, exposure)], by = "category", all.x = TRUE)
  base[exposure == "gdp_target", exposure := "total_population"]
  base[, category_expenditure_weight := base_expenditure / sum(base_expenditure)]
  profiles <- age_profile_relative_weights()
  merge(base, profiles, by = "exposure", allow.cartesian = TRUE)
}


age_expenditure_profile_index <- function(dt) {
  dt <- as.data.table(copy(dt))
  age_groups <- c("0_14", "15_34", "35_54", "55_64", "65p")
  missing <- setdiff(age_groups, names(dt))
  if (length(missing)) stop("Missing age shares for expenditure profile: ", paste(missing, collapse = ", "))

  base_population <- fread(file.path(processed_dir, "historical_age_shares.csv"))[
    year == 2025L
  ]
  categories <- age_profile_category_table()
  category_profiles <- dcast(
    categories, category + exposure + category_expenditure_weight ~ age_group,
    value.var = "relative_weight"
  )
  out <- rep(0, nrow(dt))
  for (i in seq_len(nrow(category_profiles))) {
    profile <- as.numeric(category_profiles[i, ..age_groups])
    base_exposure <- sum(profile * as.numeric(base_population[1L, ..age_groups]))
    current_exposure <- as.matrix(dt[, ..age_groups]) %*% profile
    out <- out + category_profiles$category_expenditure_weight[i] *
      as.numeric(current_exposure / base_exposure)
  }
  out
}


demographic_level_terms <- function(specification = NULL, suffix = "") {
  specification <- active_demographic_specification(specification)
  if (specification == "age_shares") age_level_terms(suffix) else {
    paste0("age_expenditure_profile", suffix)
  }
}


demographic_feature_frame <- function(dt, specification = NULL) {
  specification <- active_demographic_specification(specification)
  if (specification == "age_shares") return(age_feature_frame(dt))
  data.frame(age_expenditure_profile = age_expenditure_profile_index(dt))
}


covid_feature_frame <- function(dt) {
  years <- as.numeric(dt$year)
  out <- lapply(covid_intervention_years(), function(year) as.integer(years == year))
  names(out) <- covid_dummy_terms()
  as.data.frame(out, check.names = FALSE)
}

model_feature_frame <- function(dt, include_covid = TRUE, scale_drivers = NULL,
                                demographic_specification = NULL) {
  # Full set of contemporaneous regressors used by the simple level models.
  out <- cbind(
    demographic_feature_frame(dt, demographic_specification),
    data.frame(
    tot_z = as.numeric(dt$tot_z),
    rp_z = as.numeric(dt$rp_z),
      unemployment = as.numeric(dt$unemployment)
    ),
    scale_feature_frame(dt, scale_drivers)
  )
  if (include_covid) out <- cbind(out, covid_feature_frame(dt))
  out
}


demo_feature_frame <- function(dt) {
  # Retained as a convenience helper for diagnostics / alternative variants.
  age_feature_frame(dt)
}


structural_feature_frame <- function(dt, scale_drivers = NULL,
                                     demographic_specification = NULL) {
  # Slow-moving variables that we are willing to interpret as structural
  # determinants of the long-run spending share.
  #
  # Relative government prices (`rp_z`) are included here, rather than in the
  # short-run macro block, because persistent changes in the cost of producing
  # government services can create a long-run pressure on nominal spending/GDP.
  cbind(
    demographic_feature_frame(dt, demographic_specification),
    data.frame(rp_z = as.numeric(dt$rp_z)),
    scale_feature_frame(dt, scale_drivers),
    covid_feature_frame(dt)
  )
}


safe_auto_arima <- function(y, xreg = NULL, stationary = TRUE, include_mean = TRUE) {
  # Small ARMA search because annual fiscal samples are typically short.
  forecast::auto.arima(
    y,
    xreg = xreg,
    d = if (stationary) 0 else NA,
    max.p = 2, max.q = 2,
    max.P = 0, max.Q = 0,
    seasonal = FALSE,
    stationary = stationary,
    allowdrift = include_mean,
    allowmean = include_mean,
    stepwise = TRUE,
    approximation = FALSE
  )
}


varying_columns <- function(df, tol = 1e-10) {
  # Drop regressors that are effectively constant in the estimation sample.
  # This is especially useful for intervention dummies in subsamples that do
  # not include COVID.
  vapply(
    df,
    function(v) {
      s <- sd(v, na.rm = TRUE)
      is.finite(s) && s > tol
    },
    logical(1)
  )
}


as_xreg_matrix <- function(df) {
  if (is.null(df) || ncol(df) == 0L) return(NULL)
  as.matrix(df)
}


# -----------------------------------------------------------------------------
# Dynamic feature construction
# -----------------------------------------------------------------------------

build_topdown_table <- function(dt, include_y = TRUE, scale_drivers = NULL,
                                demographic_specification = NULL) {
  # Build one table containing both levels and the transformations needed by
  # `dynamic_diff` and `ardl_ecm`.
  #
  # `shift()` is preferable here to repeated calls to diff() because it keeps
  # every transformed variable explicitly aligned with calendar year t.

  dt <- as.data.table(copy(dt))
  setorder(dt, year)

  out <- data.table(
    year = as.numeric(dt$year),
    age_0_14 = as.numeric(dt[["0_14"]]),
    age_15_34 = as.numeric(dt[["15_34"]]),
    age_55_64 = as.numeric(dt[["55_64"]]),
    age_65p = as.numeric(dt[["65p"]]),
    tot_z = as.numeric(dt$tot_z),
    rp_z = as.numeric(dt$rp_z),
    unemployment = as.numeric(dt$unemployment),
    covid = as.numeric(dt$covid)
  )
  out[, age_expenditure_profile := age_expenditure_profile_index(dt)]

  # The relative-price index is available directly in history and is recovered
  # from the standardised projection assumption out of sample.
  standardisation <- fread(file.path(processed_dir, "standardisation_parameters.csv"))
  rp <- standardisation[variable == "relative_gov_price"]
  relative_price_from_z <- as.numeric(dt$rp_z) * rp$scale + rp$center
  relative_price <- if ("relative_gov_price" %in% names(dt)) {
    fifelse(is.finite(as.numeric(dt$relative_gov_price)),
            as.numeric(dt$relative_gov_price), relative_price_from_z)
  } else relative_price_from_z
  out[, log_relative_gov_price := log(relative_price)]
  if ("log_real_gdp_per_capita" %in% names(dt)) {
    out[, log_real_gdp_per_capita := as.numeric(dt$log_real_gdp_per_capita)]
  }

  for (term in scale_level_terms(drivers = scale_drivers)) {
    if (!term %in% names(dt)) stop("Missing top-down driver: ", term)
    out[, (term) := as.numeric(dt[[term]])]
  }

  # Annual changes in slow-moving structural variables.
  for (v in c("age_0_14", "age_15_34", "age_55_64", "age_65p",
              "age_expenditure_profile", "rp_z")) {
    out[, (paste0("d_", v)) := get(v) - shift(get(v))]
    out[, (paste0(v, "_l1")) := shift(get(v))]
  }

  # Terms of trade: current change and one lag of the change.
  # The lag allows fiscal effects to arrive with a budget / implementation delay.
  out[, d_tot_z := tot_z - shift(tot_z)]
  out[, d_tot_z_l1 := shift(d_tot_z)]

  # Unemployment: keep both its level and its annual change.
  # The stand-alone differenced / hybrid models use changes, so a one-off rise
  # in unemployment creates a level shift in projected spending rather than a
  # permanently repeated annual increase. The ECM can safely use unemployment
  # levels because its error-correction term pins down a finite steady state.
  out[, unemployment_l1 := shift(unemployment)]
  out[, d_unemployment := unemployment - shift(unemployment)]
  out[, d_unemployment_l1 := shift(d_unemployment)]

  # Separate annual interventions absorb the spending changes recorded in each
  # COVID-affected financial year without imposing a ramp or symmetric unwind.
  for (covid_year in covid_intervention_years()) {
    out[, (paste0("covid_fy", covid_year)) := as.integer(get("year") == covid_year)]
  }
  for (v in scale_level_terms(drivers = scale_drivers)) {
    out[, (paste0("d_", v)) := get(v) - shift(get(v))]
    out[, (paste0(v, "_l1")) := shift(get(v))]
  }

  for (v in intersect(c("log_relative_gov_price", "log_real_gdp_per_capita"), names(out))) {
    out[, (paste0("d_", v)) := get(v) - shift(get(v))]
    out[, (paste0(v, "_l1")) := shift(get(v))]
    out[, (paste0("d_", v, "_l1")) := shift(get(paste0("d_", v)))]
  }

  if (include_y) {
    if (!"broad_expenditure_gdp" %in% names(dt)) {
      stop("`broad_expenditure_gdp` is required when include_y = TRUE.")
    }

    out[, y := as.numeric(dt$broad_expenditure_gdp)]
    out[, y_lag := shift(y)]
    out[, dy := y - y_lag]
    out[, dy_lag := shift(dy)]
    out[, w := log(y)]
    out[, w_lag := shift(w)]
    out[, dw := w - w_lag]
    out[, dw_lag := shift(dw)]
  }

  out
}


build_future_feature_table <- function(train, future, scale_drivers = NULL,
                                       demographic_specification = NULL) {
  # Derive future changes and lags using the end of the historical sample.
  # For example, the first projected d_tot_z is future TOT minus the final
  # historical TOT, rather than future TOT minus an unavailable future lag.

  train <- as.data.table(copy(train))
  future <- as.data.table(copy(future))
  setorder(train, year)
  setorder(future, year)

  if (nrow(train) < 2L) stop("At least two historical observations are required.")
  if (nrow(future) < 1L) stop("`future` must contain at least one row.")
  if (min(future$year) <= max(train$year)) {
    stop("Future years must begin strictly after the final historical year.")
  }

  # Two historical rows are enough to seed all one-period lags and lagged
  # first-differences required by the new dynamic models.
  seed <- tail(train, 2L)
  combined <- rbindlist(list(seed, future), fill = TRUE, use.names = TRUE)
  derived <- build_topdown_table(
    combined, include_y = FALSE, scale_drivers = scale_drivers,
    demographic_specification = demographic_specification
  )

  derived[year %in% future$year]
}


# -----------------------------------------------------------------------------
# ECM helpers
# -----------------------------------------------------------------------------

extract_ecm_long_run <- function(ecm_fit, scale_drivers = NULL) {
  # Convert coefficients from the unrestricted ECM parameterisation into
  # implied long-run coefficients.
  #
  # If the model is:
  #   dy_t = a + lambda*y_{t-1} + beta*x_{t-1} + ...
  # then the long-run relationship is approximately:
  #   y* = -a/lambda - (beta/lambda) * x
  #
  # This table is a diagnostic, not a separate fitted model.

  b <- coef(ecm_fit)
  lambda <- unname(b["y_lag"])

  if (!length(lambda) || is.na(lambda) || abs(lambda) < 1e-8) {
    return(data.table(
      term = character(),
      estimate = numeric(),
      adjustment = numeric()
    ))
  }

  level_terms <- intersect(
    c(
      "(Intercept)", age_level_terms("_l1"), "rp_z_l1",
      scale_level_terms("_l1", scale_drivers), "unemployment_l1"
    ),
    names(b)
  )

  estimates <- -b[level_terms] / lambda

  data.table(
    term = sub("_l1$", "", names(estimates)),
    estimate = as.numeric(estimates),
    adjustment = lambda
  )
}


recursive_ecm_forecast <- function(ecm_fit, train_table, future_table) {
  # The ECM contains lagged spending and lagged spending growth, so future
  # forecasts must be generated recursively: the forecast for year t becomes
  # the lagged dependent variable used in forecasting year t+1.

  h <- nrow(future_table)
  out <- numeric(h)

  w_prev <- tail(train_table$w, 1L)
  dw_prev <- tail(na.omit(train_table$dw), 1L)

  # Terms with no historical variation are dropped before estimation.

  for (i in seq_len(h)) {
    row <- as.data.frame(future_table[i])

    row$w_lag <- w_prev
    row$dw_lag <- dw_prev

    # predict.lm matches regressors by name; extra columns in `row` are harmless.
    dw_hat <- as.numeric(predict(ecm_fit, newdata = row))

    w_prev <- w_prev + dw_hat
    out[i] <- exp(w_prev)
    dw_prev <- dw_hat
  }

  out
}


# -----------------------------------------------------------------------------
# Main fitting and projection function
# -----------------------------------------------------------------------------

fit_predict_topdown <- function(train, future, additional_drivers = NULL,
                                demographic_specification = NULL,
                                interest_treatment = NULL) {
  train <- as.data.table(copy(train))
  future <- as.data.table(copy(future))
  interest_treatment <- active_topdown_interest_treatment(interest_treatment)
  train <- apply_topdown_interest_treatment(train, interest_treatment)
  setorder(train, year)
  setorder(future, year)

  scale_drivers <- active_scale_drivers(additional_drivers)
  demographic_specification <- active_demographic_specification(demographic_specification)
  driver_variant <- if (!length(scale_drivers)) "baseline" else {
    paste(scale_drivers, collapse = "+")
  }

  y <- as.numeric(train$broad_expenditure_gdp)
  h <- nrow(future)

  results <- list()
  metadata <- list()

  # ---------------------------------------------------------------------------
  # 1. Structural OLS in levels
  # ---------------------------------------------------------------------------
  # Simple descriptive benchmark: spending/GDP as a contemporaneous function
  # of age composition, TOT, relative prices, unemployment and COVID.

  x <- model_feature_frame(train, scale_drivers = scale_drivers,
                           demographic_specification = demographic_specification)
  x_future <- model_feature_frame(future, scale_drivers = scale_drivers,
                                  demographic_specification = demographic_specification)

  keep_x <- varying_columns(x)
  x <- x[, keep_x, drop = FALSE]
  x_future <- x_future[, names(x), drop = FALSE]

  ols_fit <- lm(y ~ ., data = x)
  results$structural_ols <- as.numeric(predict(ols_fit, newdata = x_future))

  metadata$structural_ols <- data.table(
    model = "structural_ols",
    specification = paste(deparse(formula(ols_fit)), collapse = " "),
    arima_order = NA_character_,
    aic = AIC(ols_fit),
    aicc = NA_real_,
    bic = BIC(ols_fit),
    observations = nobs(ols_fit),
    driver_variant = driver_variant,
    demographic_specification = demographic_specification
  )

  # ---------------------------------------------------------------------------
  # 2. Regression in levels with stationary ARMA errors
  # ---------------------------------------------------------------------------
  # This can already behave somewhat like a restricted error-correction model:
  # if the regression residual is persistent but stationary, spending is pulled
  # back toward the fitted level relationship over time.

  level_fit <- safe_auto_arima(
    y,
    as_xreg_matrix(x),
    stationary = TRUE,
    include_mean = TRUE
  )

  results$arimax_level <- as.numeric(
    forecast(level_fit, xreg = as_xreg_matrix(x_future), h = h)$mean
  )

  metadata$arimax_level <- data.table(
    model = "arimax_level",
    specification = "Spending/GDP in levels with age and macro xreg; stationary ARMA errors",
    arima_order = paste(arimaorder(level_fit)[1:3], collapse = ","),
    aic = AIC(level_fit),
    aicc = level_fit$aicc,
    bic = level_fit$bic,
    observations = length(y),
    driver_variant = driver_variant,
    demographic_specification = demographic_specification
  )

  # ---------------------------------------------------------------------------
  # 3. Differenced ARIMAX with variable-appropriate transformations
  # ---------------------------------------------------------------------------
  # Build aligned dynamic tables once for all change and ECM specifications.
  train_dyn <- build_topdown_table(
    train, include_y = TRUE, scale_drivers = scale_drivers,
    demographic_specification = demographic_specification
  )
  future_dyn <- build_future_feature_table(
    train, future, scale_drivers = scale_drivers,
    demographic_specification = demographic_specification
  )

  # ---------------------------------------------------------------------------
  # Age and relative-price changes represent gradual structural movement;
  # current and lagged changes in unemployment and the terms of trade capture
  # cyclical effects. Separate COVID-year indicators absorb exceptional years.
  # ---------------------------------------------------------------------------
  # Economic interpretation:
  #   - selected demographics: changes matter for annual spending growth;
  #   - relative government prices: current change affects annual spending;
  #   - unemployment: current and lagged CHANGES matter;
  #   - terms of trade: current and lagged CHANGES matter;
  #   - COVID: separate FY2020, FY2021 and FY2022 interventions;
  #   - remaining serial correlation is handled by stationary ARMA errors.
  #
  # We deliberately do not add lagged dy as an xreg here because ARMA errors
  # already absorb persistence. The explicit lagged dependent variable appears
  # in the ECM below, where recursive forecasting is economically important.

  dynamic_terms <- c(
    paste0("d_", demographic_level_terms(demographic_specification)),
    scale_difference_terms(scale_drivers),
    "d_rp_z",
    "d_unemployment", "d_unemployment_l1",
    "d_tot_z", "d_tot_z_l1",
    covid_dummy_terms()
  )

  dyn_sample <- complete.cases(train_dyn[, c("dy", dynamic_terms), with = FALSE])
  dyn_x <- as.data.frame(train_dyn[dyn_sample, ..dynamic_terms])
  dyn_y <- train_dyn$dy[dyn_sample]

  keep_dyn <- varying_columns(dyn_x)
  dyn_x <- dyn_x[, keep_dyn, drop = FALSE]
  dyn_future_x <- as.data.frame(future_dyn[, names(dyn_x), with = FALSE])

  diff_fit <- safe_auto_arima(
    dyn_y,
    as_xreg_matrix(dyn_x),
    stationary = TRUE,
    include_mean = FALSE
  )

  diff_forecast <- as.numeric(
    forecast(diff_fit, xreg = as_xreg_matrix(dyn_future_x), h = h)$mean
  )

  results$arimax_diff <- tail(y, 1L) + cumsum(diff_forecast)

  metadata$arimax_diff <- data.table(
    model = "arimax_diff",
    specification = paste(
      "Change in spending/GDP with selected d(age shares), d(scale/income drivers), d(relative prices),",
      "d(unemployment) + lag, d(TOT) + lag, FY2020-22 interventions; ARMA errors"
    ),
    arima_order = paste(arimaorder(diff_fit)[1:3], collapse = ","),
    aic = AIC(diff_fit),
    aicc = diff_fit$aicc,
    bic = diff_fit$bic,
    observations = length(dyn_y),
    driver_variant = driver_variant,
    demographic_specification = demographic_specification
  )

  # ---------------------------------------------------------------------------
  # 5. UPDATED hybrid structural + macro model
  # ---------------------------------------------------------------------------
  # Long-run / structural component:
  #     age composition + relative government prices in levels.
  #
  # The structural level equation also contains the FY2020-22 interventions.
  # Their fitted first differences absorb the corresponding pandemic movements.
  #
  # Short-run macro component:
  #     unemployment changes and terms-of-trade changes. COVID terms are not
  #     repeated here because they already enter through the structural block.
  #
  # The hybrid still works in annual changes when combining the two components:
  # observed dy minus structural predicted dy is the macro residual change.

  structural <- structural_feature_frame(
    train, scale_drivers, demographic_specification
  )
  structural_future <- structural_feature_frame(
    future, scale_drivers, demographic_specification
  )

  keep_structural <- varying_columns(structural)
  structural <- structural[, keep_structural, drop = FALSE]
  structural_future <- structural_future[, names(structural), drop = FALSE]

  hybrid_structural_fit <- lm(y ~ ., data = structural)

  structural_hist <- as.numeric(predict(hybrid_structural_fit, newdata = structural))
  structural_proj <- as.numeric(predict(hybrid_structural_fit, newdata = structural_future))

  d_structural_train <- diff(structural_hist)
  d_structural_future <- diff(c(tail(structural_hist, 1L), structural_proj))

  # Align the macro residual with calendar rows t = 2,...,T.
  residual_change <- diff(y) - d_structural_train
  residual_table <- copy(train_dyn[-1L])
  residual_table[, macro_residual_change := residual_change]

  hybrid_macro_terms <- c(
    "d_unemployment", "d_unemployment_l1",
    "d_tot_z", "d_tot_z_l1"
  )

  hybrid_sample <- complete.cases(
    residual_table[, c("macro_residual_change", hybrid_macro_terms), with = FALSE]
  )

  hybrid_macro_x <- as.data.frame(residual_table[hybrid_sample, ..hybrid_macro_terms])
  hybrid_macro_y <- residual_table$macro_residual_change[hybrid_sample]

  keep_hybrid_macro <- varying_columns(hybrid_macro_x)
  hybrid_macro_x <- hybrid_macro_x[, keep_hybrid_macro, drop = FALSE]
  hybrid_macro_future_x <- as.data.frame(
    future_dyn[, names(hybrid_macro_x), with = FALSE]
  )

  hybrid_macro_fit <- safe_auto_arima(
    hybrid_macro_y,
    as_xreg_matrix(hybrid_macro_x),
    stationary = TRUE,
    include_mean = FALSE
  )

  hybrid_macro_forecast <- as.numeric(
    forecast(
      hybrid_macro_fit,
      xreg = as_xreg_matrix(hybrid_macro_future_x),
      h = h
    )$mean
  )

  results$hybrid <- tail(y, 1L) + cumsum(
    d_structural_future + hybrid_macro_forecast
  )

  metadata$hybrid <- data.table(
    model = "hybrid",
    specification = paste(
      "Structural level OLS using selected age shares + scale/income drivers + relative prices + FY2020-22 interventions;",
      "macro residual change uses d(unemployment) + lag and d(TOT) + lag; ARMA errors"
    ),
    arima_order = paste(arimaorder(hybrid_macro_fit)[1:3], collapse = ","),
    aic = AIC(hybrid_macro_fit),
    aicc = hybrid_macro_fit$aicc,
    bic = hybrid_macro_fit$bic,
    observations = length(hybrid_macro_y),
    driver_variant = driver_variant,
    demographic_specification = demographic_specification
  )

  # ---------------------------------------------------------------------------
  # 5. Parsimonious augmented-Mann unrestricted ECM / ARDL-style model
  # ---------------------------------------------------------------------------
  # The ECM is estimated directly as a regression for annual spending growth:
  #
  #   dy_t = a
  #        + lambda * y_{t-1}
  #        + theta' * structural_levels_{t-1}
  #        + short-run dynamics
  #        + error_t
  #
  # Long-run block:
  #   lagged age shares + lagged relative government prices.
  #
  # Dynamic / conditioning block:
  #   d(relative prices), lagged unemployment level + d(unemployment),
  #   d(TOT) current + lag, lagged spending growth, and annual COVID interventions.
  #
  # Unemployment is treated slightly differently from the structural variables:
  # it is plausibly stationary, so `unemployment_l1` is best thought of as a
  # conditioning variable for the spending equilibrium rather than evidence of
  # a cointegrating trend. `d_unemployment` captures the immediate cyclical move.
  #
  # We intentionally omit short-run changes in every individual age share here.
  # Age shares are slow-moving and already consume several long-run degrees of
  # freedom. Adding both their level and change coefficients can overfit a short
  # annual sample. That is an easy robustness variant to add later if needed.

  # Long run: log spending/GDP, log real GDP per capita and log relative price.
  # Age shares are deliberately omitted from the bounds vector because their
  # smooth integration properties make the I(0)/I(1) bounds assumptions unsafe.
  ecm_core <- c("w_lag", "log_real_gdp_per_capita_l1", "log_relative_gov_price_l1")
  ecm_short_1 <- c(
    "d_log_real_gdp_per_capita", "d_log_relative_gov_price",
    "d_unemployment", "d_tot_z", covid_dummy_terms()
  )
  ecm_short_2 <- c(
    "dw_lag", "d_log_real_gdp_per_capita_l1", "d_log_relative_gov_price_l1",
    "d_unemployment_l1", "d_tot_z_l1"
  )
  fit_ecm_candidate <- function(order) {
    terms <- c(ecm_core, ecm_short_1, if (order == 2L) ecm_short_2)
    sample <- complete.cases(train_dyn[, c("dw", terms), with = FALSE])
    df <- as.data.frame(train_dyn[sample, c("dw", terms), with = FALSE])
    keep <- varying_columns(df[, terms, drop = FALSE])
    terms <- terms[keep]
    fit <- lm(reformulate(terms, response = "dw"), data = df)
    list(fit = fit, order = order, sample = sample, bic = BIC(fit))
  }
  ecm_candidates <- lapply(1:2, fit_ecm_candidate)
  ecm_selected <- ecm_candidates[[which.min(vapply(ecm_candidates, `[[`, numeric(1), "bic"))]]
  ecm_fit <- ecm_selected$fit
  ecm_formula <- formula(ecm_fit)

  results$ardl_ecm <- recursive_ecm_forecast(
    ecm_fit,
    train_table = train_dyn,
    future_table = future_dyn
  )

  ecm_adjustment <- unname(coef(ecm_fit)["w_lag"])

  metadata$ardl_ecm <- data.table(
    model = "ardl_ecm",
    specification = paste(deparse(ecm_formula), collapse = " "),
    arima_order = NA_character_,
    aic = AIC(ecm_fit),
    aicc = NA_real_,
    bic = BIC(ecm_fit),
    observations = nobs(ecm_fit),
    ecm_adjustment = ifelse(length(ecm_adjustment), ecm_adjustment, NA_real_),
    selected_lag_order = ecm_selected$order,
    driver_variant = driver_variant,
    demographic_specification = demographic_specification
  )

  # ---------------------------------------------------------------------------
  # Collect projection paths and fitted objects
  # ---------------------------------------------------------------------------

  paths <- rbindlist(lapply(names(results), function(model) {
    data.table(
      year = future$year,
      model = model,
      value = results[[model]]
    )
  }))
  paths[, `:=`(
    interest_treatment = interest_treatment,
    interest_treatment_label = topdown_interest_treatment_label(interest_treatment)
  )]
  metadata_out <- rbindlist(metadata, fill = TRUE)
  metadata_out[, `:=`(
    interest_treatment = interest_treatment,
    interest_treatment_label = topdown_interest_treatment_label(interest_treatment)
  )]

  list(
    paths = paths,
    metadata = metadata_out,
    fits = list(
      structural_ols = ols_fit,
      arimax_level = level_fit,
      arimax_diff = diff_fit,
      hybrid_structural_ols = hybrid_structural_fit,
      hybrid_macro = hybrid_macro_fit,
      ardl_ecm = ecm_fit
    ),
    diagnostics = list(
      # Useful first checks for the ECM:
      #   * ecm_adjustment should normally be negative;
      #   * long-run coefficients should be economically plausible and stable.
      ecm_lag_order = ecm_selected$order,
      ecm_candidate_bic = vapply(ecm_candidates, `[[`, numeric(1), "bic")
    )
  )
}

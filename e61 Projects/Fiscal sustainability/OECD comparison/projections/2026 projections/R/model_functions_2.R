# -----------------------------------------------------------------------------
# Top-down government spending model functions
# Updated version: adds variable-specific dynamics and an explicit ECM
# -----------------------------------------------------------------------------
#
# Main changes from the earlier version
# -------------------------------------
# 1. Keeps the original benchmark models:
#      - structural_ols
#      - arimax_level
#      - arimax_diff
#      - hybrid
#      - univariate_arima
#
# 2. Adds `dynamic_diff`.
#    This still forecasts annual changes in spending/GDP, but no longer assumes
#    that every explanatory variable should enter in the same transformation.
#    In particular:
#      - the selected age shares enter as annual changes;
#      - relative government prices enter as annual changes;
#      - unemployment enters as current and lagged CHANGES. In a pure
#        differenced model, putting the unemployment level directly on the RHS
#        would imply repeated spending growth for every year unemployment stays
#        high, producing an undesirable permanent drift;
#      - the terms of trade enter as current and lagged annual changes;
#      - COVID is represented by separate annual intervention indicators.
#
# 3. Adds `ardl_ecm`, an unrestricted error-correction model (ECM).
#    The ECM separates:
#      - a LONG-RUN relationship in the levels of spending/GDP, age shares and
#        relative government prices; from
#      - SHORT-RUN dynamics in unemployment, terms of trade, relative prices,
#        lagged spending growth and COVID interventions.
#
#    The coefficient on lagged spending (`y_lag`) is the adjustment parameter.
#    For a stable error-correction mechanism it should normally be negative.
#    Long-run coefficients are approximately -beta_level / beta_y_lag.
#
# 4. Changes the hybrid model so that relative government prices sit with the
#    demographic variables in the structural level component. This reflects
#    the idea that demographics and relative government costs are slow-moving
#    structural pressures, while unemployment and terms-of-trade movements are
#    shorter-run macro influences.
#
# 5. Adds metadata fields for AIC/AICc/BIC where available. These should NOT be
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


age_feature_frame <- function(dt) {
  out <- lapply(active_age_groups(), function(group) as.numeric(dt[[group]]))
  names(out) <- age_level_terms()
  as.data.frame(out, check.names = FALSE)
}


covid_feature_frame <- function(dt) {
  years <- as.numeric(dt$year)
  out <- lapply(covid_intervention_years(), function(year) as.integer(years == year))
  names(out) <- covid_dummy_terms()
  as.data.frame(out, check.names = FALSE)
}

model_feature_frame <- function(dt, include_covid = TRUE) {
  # Full set of contemporaneous regressors used by the simple level models.
  out <- cbind(
    age_feature_frame(dt),
    data.frame(
    tot_z = as.numeric(dt$tot_z),
    rp_z = as.numeric(dt$rp_z),
      unemployment = as.numeric(dt$unemployment)
    )
  )
  if (include_covid) out <- cbind(out, covid_feature_frame(dt))
  out
}


demo_feature_frame <- function(dt) {
  # Retained as a convenience helper for diagnostics / alternative variants.
  age_feature_frame(dt)
}


structural_feature_frame <- function(dt) {
  # Slow-moving variables that we are willing to interpret as structural
  # determinants of the long-run spending share.
  #
  # Relative government prices (`rp_z`) are included here, rather than in the
  # short-run macro block, because persistent changes in the cost of producing
  # government services can create a long-run pressure on nominal spending/GDP.
  cbind(
    age_feature_frame(dt),
    data.frame(rp_z = as.numeric(dt$rp_z)),
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

build_topdown_table <- function(dt, include_y = TRUE) {
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

  # Annual changes in slow-moving structural variables.
  for (v in c("age_0_14", "age_15_34", "age_55_64", "age_65p", "rp_z")) {
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

  if (include_y) {
    if (!"broad_expenditure_gdp" %in% names(dt)) {
      stop("`broad_expenditure_gdp` is required when include_y = TRUE.")
    }

    out[, y := as.numeric(dt$broad_expenditure_gdp)]
    out[, y_lag := shift(y)]
    out[, dy := y - y_lag]
    out[, dy_lag := shift(dy)]
  }

  out
}


build_future_feature_table <- function(train, future) {
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
  derived <- build_topdown_table(combined, include_y = FALSE)

  derived[year %in% future$year]
}


# -----------------------------------------------------------------------------
# ECM helpers
# -----------------------------------------------------------------------------

extract_ecm_long_run <- function(ecm_fit) {
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
    c("(Intercept)", age_level_terms("_l1"), "rp_z_l1", "unemployment_l1"),
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

  y_prev <- tail(train_table$y, 1L)
  dy_prev <- tail(na.omit(train_table$dy), 1L)

  # Terms with no historical variation are dropped before estimation.

  for (i in seq_len(h)) {
    row <- as.data.frame(future_table[i])

    row$y_lag <- y_prev
    row$dy_lag <- dy_prev

    # predict.lm matches regressors by name; extra columns in `row` are harmless.
    dy_hat <- as.numeric(predict(ecm_fit, newdata = row))

    out[i] <- y_prev + dy_hat
    y_prev <- out[i]
    dy_prev <- dy_hat
  }

  out
}


# -----------------------------------------------------------------------------
# Main fitting and projection function
# -----------------------------------------------------------------------------

fit_predict_topdown <- function(train, future) {
  train <- as.data.table(copy(train))
  future <- as.data.table(copy(future))
  setorder(train, year)
  setorder(future, year)

  y <- as.numeric(train$broad_expenditure_gdp)
  h <- nrow(future)

  results <- list()
  metadata <- list()

  # ---------------------------------------------------------------------------
  # 1. Structural OLS in levels
  # ---------------------------------------------------------------------------
  # Simple descriptive benchmark: spending/GDP as a contemporaneous function
  # of age composition, TOT, relative prices, unemployment and COVID.

  x <- model_feature_frame(train)
  x_future <- model_feature_frame(future)

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
    observations = nobs(ols_fit)
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
    observations = length(y)
  )

  # ---------------------------------------------------------------------------
  # 3. Original differenced ARIMAX benchmark
  # ---------------------------------------------------------------------------
  # This is intentionally retained as the simple benchmark. It mechanically
  # differences every original regressor, including unemployment and COVID.
  # That is NOT our preferred economic specification; `dynamic_diff` below is
  # designed to show whether variable-specific transformations improve on it.

  x_diff_base <- model_feature_frame(train, include_covid = FALSE)
  x_diff_future_base <- model_feature_frame(future, include_covid = FALSE)
  combined_x <- rbind(x_diff_base[nrow(x_diff_base), , drop = FALSE], x_diff_future_base)
  dx_future <- as.data.frame(apply(combined_x, 2, diff))
  dx_train <- as.data.frame(apply(x_diff_base, 2, diff))
  dx_train <- cbind(dx_train, covid_feature_frame(train[-1L]))
  dx_future <- cbind(dx_future, covid_feature_frame(future))
  dy <- diff(y)

  keep_dx <- varying_columns(dx_train)
  dx_train <- dx_train[, keep_dx, drop = FALSE]
  dx_future <- dx_future[, names(dx_train), drop = FALSE]

  diff_fit <- safe_auto_arima(
    dy,
    as_xreg_matrix(dx_train),
    stationary = TRUE,
    include_mean = FALSE
  )

  diff_forecast <- as.numeric(
    forecast(diff_fit, xreg = as_xreg_matrix(dx_future), h = h)$mean
  )

  results$arimax_diff <- y[length(y)] + cumsum(diff_forecast)

  metadata$arimax_diff <- data.table(
    model = "arimax_diff",
    specification = "Benchmark: annual change in spending/GDP with differenced drivers and FY2020-22 interventions; no drift",
    arima_order = paste(arimaorder(diff_fit)[1:3], collapse = ","),
    aic = AIC(diff_fit),
    aicc = diff_fit$aicc,
    bic = diff_fit$bic,
    observations = length(dy)
  )

  # Build the aligned dynamic tables once for the remaining models.
  train_dyn <- build_topdown_table(train, include_y = TRUE)
  future_dyn <- build_future_feature_table(train, future)

  # ---------------------------------------------------------------------------
  # 4. NEW: dynamic differenced model with variable-specific transformations
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
    paste0("d_", age_level_terms()),
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

  dynamic_fit <- safe_auto_arima(
    dyn_y,
    as_xreg_matrix(dyn_x),
    stationary = TRUE,
    include_mean = FALSE
  )

  dynamic_dy_forecast <- as.numeric(
    forecast(dynamic_fit, xreg = as_xreg_matrix(dyn_future_x), h = h)$mean
  )

  results$dynamic_diff <- tail(y, 1L) + cumsum(dynamic_dy_forecast)

  metadata$dynamic_diff <- data.table(
    model = "dynamic_diff",
    specification = paste(
      "Change in spending/GDP with selected d(age shares), d(relative prices),",
      "d(unemployment) + lag, d(TOT) + lag, FY2020-22 interventions; ARMA errors"
    ),
    arima_order = paste(arimaorder(dynamic_fit)[1:3], collapse = ","),
    aic = AIC(dynamic_fit),
    aicc = dynamic_fit$aicc,
    bic = dynamic_fit$bic,
    observations = length(dyn_y)
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

  structural <- structural_feature_frame(train)
  structural_future <- structural_feature_frame(future)

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
      "Structural level OLS using selected age shares + relative prices + FY2020-22 interventions;",
      "macro residual change uses d(unemployment) + lag and d(TOT) + lag; ARMA errors"
    ),
    arima_order = paste(arimaorder(hybrid_macro_fit)[1:3], collapse = ","),
    aic = AIC(hybrid_macro_fit),
    aicc = hybrid_macro_fit$aicc,
    bic = hybrid_macro_fit$bic,
    observations = length(hybrid_macro_y)
  )

  # ---------------------------------------------------------------------------
  # 6. NEW: parsimonious unrestricted ECM / ARDL-style model
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

  ecm_terms <- c(
    "y_lag",
    age_level_terms("_l1"),
    "rp_z_l1",
    "d_rp_z",
    "unemployment_l1", "d_unemployment",
    "d_tot_z", "d_tot_z_l1",
    "dy_lag",
    covid_dummy_terms()
  )

  ecm_sample <- complete.cases(train_dyn[, c("dy", ecm_terms), with = FALSE])
  ecm_df <- as.data.frame(train_dyn[ecm_sample, c("dy", ecm_terms), with = FALSE])

  # Drop terms with no historical variation. The dependent variable is kept.
  ecm_keep <- varying_columns(ecm_df[, ecm_terms, drop = FALSE])
  ecm_terms_kept <- ecm_terms[ecm_keep]

  ecm_formula <- as.formula(
    paste("dy ~", paste(ecm_terms_kept, collapse = " + "))
  )

  ecm_fit <- lm(ecm_formula, data = ecm_df)

  results$ardl_ecm <- recursive_ecm_forecast(
    ecm_fit,
    train_table = train_dyn,
    future_table = future_dyn
  )

  ecm_adjustment <- unname(coef(ecm_fit)["y_lag"])

  metadata$ardl_ecm <- data.table(
    model = "ardl_ecm",
    specification = paste(deparse(ecm_formula), collapse = " "),
    arima_order = NA_character_,
    aic = AIC(ecm_fit),
    aicc = NA_real_,
    bic = BIC(ecm_fit),
    observations = nobs(ecm_fit),
    ecm_adjustment = ifelse(length(ecm_adjustment), ecm_adjustment, NA_real_)
  )

  # ---------------------------------------------------------------------------
  # 7. ARIMA benchmark with COVID interventions
  # ---------------------------------------------------------------------------

  uni_x <- covid_feature_frame(train)
  uni_future_x <- covid_feature_frame(future)
  keep_uni <- varying_columns(uni_x)
  uni_x <- uni_x[, keep_uni, drop = FALSE]
  uni_future_x <- uni_future_x[, names(uni_x), drop = FALSE]

  uni_fit <- safe_auto_arima(
    y, as_xreg_matrix(uni_x),
    stationary = FALSE,
    include_mean = TRUE
  )

  results$univariate_arima <- as.numeric(
    forecast(uni_fit, xreg = as_xreg_matrix(uni_future_x), h = h)$mean
  )

  metadata$univariate_arima <- data.table(
    model = "univariate_arima",
    specification = "Automatic ARIMA benchmark with FY2020-22 interventions",
    arima_order = paste(arimaorder(uni_fit)[1:3], collapse = ","),
    aic = AIC(uni_fit),
    aicc = uni_fit$aicc,
    bic = uni_fit$bic,
    observations = length(y)
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

  list(
    paths = paths,
    metadata = rbindlist(metadata, fill = TRUE),
    fits = list(
      structural_ols = ols_fit,
      arimax_level = level_fit,
      arimax_diff = diff_fit,
      dynamic_diff = dynamic_fit,
      hybrid_structural_ols = hybrid_structural_fit,
      hybrid_macro = hybrid_macro_fit,
      ardl_ecm = ecm_fit,
      univariate_arima = uni_fit
    ),
    diagnostics = list(
      # Useful first checks for the ECM:
      #   * ecm_adjustment should normally be negative;
      #   * long-run coefficients should be economically plausible and stable.
      ecm_long_run = extract_ecm_long_run(ecm_fit)
    )
  )
}

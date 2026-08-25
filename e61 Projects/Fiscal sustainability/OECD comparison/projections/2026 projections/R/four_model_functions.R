suppressPackageStartupMessages({
  library(data.table)
  library(forecast)
})

# Four deliberately distinct models for colleague-facing comparison.  The
# broader experimental model grid remains available in the research outputs,
# but is not part of this selected set.

old_shapley_feature_frame <- function(dt) {
  model_feature_frame(dt, include_covid = TRUE, scale_drivers = character())
}

hybrid_level_feature_frame <- function(dt) {
  structural_feature_frame(dt, scale_drivers = character())
}

real_spending_per_capita_log <- function(dt) {
  log(as.numeric(dt$broad_expenditure_gdp)) +
    as.numeric(dt$log_real_gdp_per_capita)
}

fit_predict_four_models <- function(train, future) {
  train <- as.data.table(copy(train))
  future <- as.data.table(copy(future))
  setorder(train, year)
  setorder(future, year)

  y <- as.numeric(train$broad_expenditure_gdp)
  h <- nrow(future)
  results <- list()
  metadata <- list()

  # 1. Old-Shapley structural OLS comparison.  This deliberately contains the
  # established age, terms-of-trade, relative-price, unemployment and COVID
  # controls, but neither population nor GDP per capita.
  old_x <- old_shapley_feature_frame(train)
  old_future_x <- old_shapley_feature_frame(future)
  keep_old <- varying_columns(old_x)
  old_x <- old_x[, keep_old, drop = FALSE]
  old_future_x <- old_future_x[, names(old_x), drop = FALSE]
  old_fit <- lm(y ~ ., data = old_x)
  results$structural_ols <- as.numeric(predict(old_fit, newdata = old_future_x))
  metadata$structural_ols <- data.table(
    model = "structural_ols",
    specification = paste(deparse(formula(old_fit)), collapse = " "),
    observations = nobs(old_fit)
  )

  # Common change table, including real-GDP-per-capita growth for the two
  # models where income belongs in changes rather than in a spending-ratio level.
  train_dyn <- build_topdown_table(
    train, include_y = TRUE, scale_drivers = "real_gdp_per_capita"
  )
  future_dyn <- build_future_feature_table(
    train, future, scale_drivers = "real_gdp_per_capita"
  )

  # 2. Revised hybrid.  Age composition and relative government prices form
  # the slow level; unemployment, terms of trade and GDP per capita enter only
  # through annual changes around that level.
  hybrid_level_x <- hybrid_level_feature_frame(train)
  hybrid_level_future_x <- hybrid_level_feature_frame(future)
  keep_hybrid_level <- varying_columns(hybrid_level_x)
  hybrid_level_x <- hybrid_level_x[, keep_hybrid_level, drop = FALSE]
  hybrid_level_future_x <- hybrid_level_future_x[, names(hybrid_level_x), drop = FALSE]
  hybrid_level_fit <- lm(y ~ ., data = hybrid_level_x)
  hybrid_level_history <- as.numeric(predict(hybrid_level_fit, newdata = hybrid_level_x))
  hybrid_level_projection <- as.numeric(predict(
    hybrid_level_fit, newdata = hybrid_level_future_x
  ))
  d_hybrid_history <- c(NA_real_, diff(hybrid_level_history))
  d_hybrid_future <- diff(c(tail(hybrid_level_history, 1L), hybrid_level_projection))
  hybrid_residual_change <- train_dyn$dy - d_hybrid_history
  hybrid_terms <- c(
    "d_unemployment", "d_unemployment_l1",
    "d_tot_z", "d_tot_z_l1", "d_log_real_gdp_per_capita"
  )
  hybrid_sample <- complete.cases(
    data.frame(hybrid_residual_change, train_dyn[, ..hybrid_terms])
  )
  hybrid_macro_x <- as.data.frame(train_dyn[hybrid_sample, ..hybrid_terms])
  hybrid_macro_y <- hybrid_residual_change[hybrid_sample]
  keep_hybrid_macro <- varying_columns(hybrid_macro_x)
  hybrid_macro_x <- hybrid_macro_x[, keep_hybrid_macro, drop = FALSE]
  hybrid_macro_future_x <- as.data.frame(
    future_dyn[, names(hybrid_macro_x), with = FALSE]
  )
  hybrid_macro_fit <- safe_auto_arima(
    hybrid_macro_y, as_xreg_matrix(hybrid_macro_x),
    stationary = TRUE, include_mean = FALSE
  )
  hybrid_macro_forecast <- as.numeric(forecast(
    hybrid_macro_fit,
    xreg = as_xreg_matrix(hybrid_macro_future_x), h = h
  )$mean)
  results$hybrid <- tail(y, 1L) + cumsum(d_hybrid_future + hybrid_macro_forecast)
  metadata$hybrid <- data.table(
    model = "hybrid",
    specification = paste0(
      "Age shares, relative prices and FY2020-22 indicators in the structural level; ",
      "changes in unemployment, terms of trade and real GDP per capita in the macro block"
    ),
    observations = length(hybrid_macro_y)
  )

  # 3. Real spending per capita.  Bounds tests are run separately.  Because the
  # full-sample tests do not establish cointegration, the projection equation is
  # estimated in differences and makes no long-run elasticity claim.
  z <- real_spending_per_capita_log(train)
  dz <- c(NA_real_, diff(z))
  per_capita_terms <- c(
    "d_log_real_gdp_per_capita", "d_age_0_14", "d_age_65p", "d_rp_z",
    covid_dummy_terms()
  )
  per_capita_sample <- complete.cases(
    data.frame(dz, train_dyn[, ..per_capita_terms])
  )
  per_capita_x <- as.data.frame(train_dyn[per_capita_sample, ..per_capita_terms])
  per_capita_y <- dz[per_capita_sample]
  keep_per_capita <- varying_columns(per_capita_x)
  per_capita_x <- per_capita_x[, keep_per_capita, drop = FALSE]
  per_capita_future_x <- as.data.frame(
    future_dyn[, names(per_capita_x), with = FALSE]
  )
  per_capita_fit <- safe_auto_arima(
    per_capita_y, as_xreg_matrix(per_capita_x),
    stationary = TRUE, include_mean = FALSE
  )
  dz_future <- as.numeric(forecast(
    per_capita_fit,
    xreg = as_xreg_matrix(per_capita_future_x), h = h
  )$mean)
  z_future <- tail(z, 1L) + cumsum(dz_future)
  results$per_capita_income <- exp(
    z_future - as.numeric(future$log_real_gdp_per_capita)
  )
  metadata$per_capita_income <- data.table(
    model = "per_capita_income",
    specification = paste0(
      "Change in log real spending per capita on changes in real GDP per capita, ",
      "age shares and relative prices, with FY2020-22 indicators"
    ),
    observations = length(per_capita_y)
  )

  # 4. Simple statistical benchmark: outcome history plus the three separate
  # COVID level interventions, with no economic drivers.
  covid_x <- covid_feature_frame(train)
  covid_future_x <- covid_feature_frame(future)
  keep_covid <- varying_columns(covid_x)
  covid_x <- covid_x[, keep_covid, drop = FALSE]
  covid_future_x <- covid_future_x[, names(covid_x), drop = FALSE]
  arima_fit <- safe_auto_arima(
    y, as_xreg_matrix(covid_x), stationary = FALSE, include_mean = TRUE
  )
  results$univariate_arima <- as.numeric(forecast(
    arima_fit, xreg = as_xreg_matrix(covid_future_x), h = h
  )$mean)
  metadata$univariate_arima <- data.table(
    model = "univariate_arima",
    specification = "Automatic ARIMA with separate FY2020-22 level interventions",
    observations = length(y)
  )

  paths <- rbindlist(lapply(names(results), function(model) {
    data.table(year = future$year, model = model, value = results[[model]])
  }))

  list(
    paths = paths,
    metadata = rbindlist(metadata, fill = TRUE),
    fits = list(
      structural_ols = old_fit,
      hybrid_level_ols = hybrid_level_fit,
      hybrid_macro = hybrid_macro_fit,
      per_capita_income = per_capita_fit,
      univariate_arima = arima_fit
    ),
    training = list(
      y = y,
      z = z,
      train_dyn = train_dyn,
      hybrid_level_history = hybrid_level_history,
      hybrid_sample = hybrid_sample,
      hybrid_macro_y = hybrid_macro_y,
      per_capita_sample = per_capita_sample,
      per_capita_y = per_capita_y
    )
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(forecast)
})

model_feature_frame <- function(dt) {
  data.frame(
    age_0_14 = as.numeric(dt[["0_14"]]),
    age_15_34 = as.numeric(dt[["15_34"]]),
    age_55_64 = as.numeric(dt[["55_64"]]),
    age_65p = as.numeric(dt[["65p"]]),
    tot_z = as.numeric(dt$tot_z),
    rp_z = as.numeric(dt$rp_z),
    unemployment = as.numeric(dt$unemployment),
    covid = as.numeric(dt$covid)
  )
}

demo_feature_frame <- function(dt) {
  data.frame(
    age_0_14 = as.numeric(dt[["0_14"]]),
    age_15_34 = as.numeric(dt[["15_34"]]),
    age_55_64 = as.numeric(dt[["55_64"]]),
    age_65p = as.numeric(dt[["65p"]])
  )
}

macro_feature_frame <- function(dt) {
  data.frame(
    tot_z = as.numeric(dt$tot_z),
    rp_z = as.numeric(dt$rp_z),
    unemployment = as.numeric(dt$unemployment)
  )
}

safe_auto_arima <- function(y, xreg = NULL, stationary = TRUE, include_mean = TRUE) {
  forecast::auto.arima(
    y,
    xreg = xreg,
    d = if (stationary) 0 else NA,
    max.p = 2, max.q = 2, max.P = 0, max.Q = 0,
    seasonal = FALSE,
    stationary = stationary,
    allowdrift = include_mean,
    allowmean = include_mean,
    stepwise = TRUE,
    approximation = FALSE
  )
}

fit_predict_topdown <- function(train, future) {
  train <- as.data.table(copy(train))
  future <- as.data.table(copy(future))
  y <- as.numeric(train$broad_expenditure_gdp)
  x <- model_feature_frame(train)
  x_future <- model_feature_frame(future)
  keep_x <- vapply(x, function(v) sd(v, na.rm = TRUE) > 1e-10, logical(1))
  x <- x[, keep_x, drop = FALSE]
  x_future <- x_future[, names(x), drop = FALSE]
  h <- nrow(future)

  results <- list()
  metadata <- list()

  ols_fit <- lm(y ~ ., data = x)
  results$structural_ols <- as.numeric(predict(ols_fit, newdata = x_future))
  metadata$structural_ols <- data.table(
    model = "structural_ols", specification = paste(deparse(formula(ols_fit)), collapse = " "),
    arima_order = NA_character_, aic = AIC(ols_fit), observations = length(y)
  )

  level_fit <- safe_auto_arima(y, as.matrix(x), stationary = TRUE, include_mean = TRUE)
  results$arimax_level <- as.numeric(forecast(level_fit, xreg = as.matrix(x_future), h = h)$mean)
  metadata$arimax_level <- data.table(
    model = "arimax_level", specification = "Spending/GDP in levels with age and macro xreg",
    arima_order = paste(arimaorder(level_fit)[1:3], collapse = ","),
    aic = AIC(level_fit), observations = length(y)
  )

  combined_x <- rbind(x[nrow(x), , drop = FALSE], x_future)
  dx_future <- as.data.frame(apply(combined_x, 2, diff))
  dx_train <- as.data.frame(apply(x, 2, diff))
  dy <- diff(y)
  diff_fit <- safe_auto_arima(dy, as.matrix(dx_train), stationary = TRUE, include_mean = FALSE)
  diff_forecast <- as.numeric(forecast(diff_fit, xreg = as.matrix(dx_future), h = h)$mean)
  results$arimax_diff <- y[length(y)] + cumsum(diff_forecast)
  metadata$arimax_diff <- data.table(
    model = "arimax_diff", specification = "Annual change in spending/GDP with differenced xreg; no drift",
    arima_order = paste(arimaorder(diff_fit)[1:3], collapse = ","),
    aic = AIC(diff_fit), observations = length(dy)
  )

  demo <- demo_feature_frame(train)
  demo_future <- demo_feature_frame(future)
  demo_fit <- lm(y ~ ., data = demo)
  demo_hist <- as.numeric(predict(demo_fit, newdata = demo))
  demo_proj <- as.numeric(predict(demo_fit, newdata = demo_future))
  d_demo_train <- diff(demo_hist)
  d_demo_future <- diff(c(tail(demo_hist, 1), demo_proj))
  macro <- macro_feature_frame(train)
  macro_future <- macro_feature_frame(future)
  d_macro_train <- as.data.frame(apply(macro, 2, diff))
  d_macro_future <- as.data.frame(apply(rbind(macro[nrow(macro), , drop = FALSE], macro_future), 2, diff))
  macro_residual_change <- diff(y) - d_demo_train
  hybrid_fit <- safe_auto_arima(
    macro_residual_change, as.matrix(d_macro_train), stationary = TRUE, include_mean = FALSE
  )
  hybrid_macro <- as.numeric(forecast(
    hybrid_fit, xreg = as.matrix(d_macro_future), h = h
  )$mean)
  results$hybrid <- y[length(y)] + cumsum(d_demo_future + hybrid_macro)
  metadata$hybrid <- data.table(
    model = "hybrid", specification = "Demographic level OLS plus macro difference ARIMAX; no drift",
    arima_order = paste(arimaorder(hybrid_fit)[1:3], collapse = ","),
    aic = AIC(hybrid_fit), observations = length(macro_residual_change)
  )

  uni_fit <- safe_auto_arima(y, stationary = FALSE, include_mean = TRUE)
  results$univariate_arima <- as.numeric(forecast(uni_fit, h = h)$mean)
  metadata$univariate_arima <- data.table(
    model = "univariate_arima", specification = "Automatic univariate ARIMA benchmark",
    arima_order = paste(arimaorder(uni_fit)[1:3], collapse = ","),
    aic = AIC(uni_fit), observations = length(y)
  )

  paths <- rbindlist(lapply(names(results), function(model) {
    data.table(year = future$year, model = model, value = results[[model]])
  }))

  list(
    paths = paths,
    metadata = rbindlist(metadata, fill = TRUE),
    fits = list(
      structural_ols = ols_fit, arimax_level = level_fit, arimax_diff = diff_fit,
      demographic_ols = demo_fit, hybrid_macro = hybrid_fit, univariate_arima = uni_fit
    )
  )
}

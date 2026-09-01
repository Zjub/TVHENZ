suppressPackageStartupMessages({
  library(data.table)
  library(forecast)
})

load_streamlined_inputs <- function() {
  history <- fread(file.path(input_dir, "historical_top_down_model_data.csv"))[
    year >= estimation_start
  ]
  future_all <- fread(file.path(input_dir, "macro_scenario_assumptions.csv"))[
    year >= projection_start & year <= projection_end
  ]
  future <- future_all[scenario == "central"]
  official <- fread(file.path(input_dir, "official_pbo_nfo_wide.csv"))
  standardisation <- fread(file.path(input_dir, "standardisation_parameters.csv"))
  parameters <- fread(file.path(input_dir, "scenario_parameters.csv"))
  list(
    history = history,
    future = future,
    future_all = future_all,
    official = official,
    standardisation = standardisation,
    parameters = parameters
  )
}

recover_index <- function(z, variable_name, standardisation) {
  p <- standardisation[standardisation[["variable"]] == variable_name]
  if (nrow(p) != 1L) stop("Missing standardisation parameters for ", variable_name)
  as.numeric(z) * p$scale + p$center
}

prepare_bridge_table <- function(history, future, standardisation) {
  h <- copy(history)
  f <- copy(future)

  h[, `:=`(
    sample = "history",
    spending_pp = 100 * broad_expenditure_excluding_other_interest_gdp
  )]
  f[, `:=`(sample = "projection", spending_pp = NA_real_)]

  keep <- c(
    "year", "sample", "spending_pp", "0_14", "65_74", "75p", "65p", "unemployment",
    "tot_z", "rp_z", "log_real_gdp_per_capita"
  )
  dt <- rbindlist(list(h[, ..keep], f[, ..keep]), use.names = TRUE)
  setorder(dt, year)
  if (anyDuplicated(dt$year)) stop("History and projection years overlap.")

  dt[, `:=`(
    age_0_14_pp = 100 * as.numeric(`0_14`),
    age_65_74_pp = 100 * as.numeric(`65_74`),
    age_75p_pp = 100 * as.numeric(`75p`),
    age_65p_pp = 100 * as.numeric(`65p`),
    relative_price = recover_index(rp_z, "relative_gov_price", standardisation),
    terms_of_trade = recover_index(tot_z, "tot_index", standardisation)
  )]
  dt[, `:=`(
    log_relative_price = log(relative_price),
    log_terms_of_trade = log(terms_of_trade)
  )]

  for (covid_year in 2020:2022) {
    level_name <- paste0("covid_fy", covid_year)
    change_name <- paste0("d_", level_name)
    dt[, (level_name) := as.integer(get("year") == covid_year)]
    dt[, (change_name) := get(level_name) - shift(get(level_name))]
  }

  change_map <- c(
    spending_pp = "dy",
    age_0_14_pp = "d_age_0_14_pp",
    age_65_74_pp = "d_age_65_74_pp",
    age_75p_pp = "d_age_75p_pp",
    age_65p_pp = "d_age_65p_pp",
    log_relative_price = "d_log_relative_price",
    log_real_gdp_per_capita = "d_log_real_gdp_per_capita",
    unemployment = "d_unemployment",
    log_terms_of_trade = "d_log_terms_of_trade"
  )
  for (source in names(change_map)) {
    dt[, (change_map[[source]]) := get(source) - shift(get(source))]
  }
  for (term in c("d_unemployment", "d_log_terms_of_trade")) {
    dt[, (paste0(term, "_l1")) := shift(get(term))]
  }
  dt[, spending_lag_pp := shift(spending_pp)]
  dt
}

slow_terms <- c(
  "d_age_0_14_pp", "d_age_65_74_pp", "d_age_75p_pp", "d_log_relative_price",
  "d_log_real_gdp_per_capita"
)
slow_terms_65p <- c(
  "d_age_0_14_pp", "d_age_65p_pp", "d_log_relative_price",
  "d_log_real_gdp_per_capita"
)
transitory_terms <- c(
  "d_unemployment", "d_unemployment_l1",
  "d_log_terms_of_trade", "d_log_terms_of_trade_l1"
)
covid_terms <- paste0("d_covid_fy", 2020:2022)

terms_for_variant <- function(variant) {
  switch(
    variant,
    with_income = c(slow_terms, transitory_terms, covid_terms),
    with_income_65p = c(slow_terms_65p, transitory_terms, covid_terms),
    no_income = c(setdiff(slow_terms, "d_log_real_gdp_per_capita"),
                  transitory_terms, covid_terms),
    structural_only = c(slow_terms, covid_terms),
    arima_benchmark = covid_terms,
    stop("Unknown model variant: ", variant)
  )
}

varying_terms <- function(dt, terms, tolerance = 1e-10) {
  terms[vapply(terms, function(term) {
    value <- sd(dt[[term]], na.rm = TRUE)
    is.finite(value) && value > tolerance
  }, logical(1))]
}

fit_bridge <- function(table, last_year, variant = "with_income",
                       first_year = estimation_start, fixed_order = NULL) {
  terms <- terms_for_variant(variant)
  sample <- table[
    sample == "history" & year >= first_year & year <= last_year
  ]
  sample <- sample[is.finite(dy)]
  terms <- varying_terms(sample, terms)
  complete <- complete.cases(sample[, c("dy", terms), with = FALSE])
  estimation <- sample[complete]
  x <- if (length(terms)) as.matrix(estimation[, ..terms]) else NULL
  # Explicit small ARMA search is more stable than auto.arima in short annual
  # samples with intervention regressors. There is deliberately no drift.
  orders <- if (is.null(fixed_order)) {
    CJ(p = 0:2, q = 0:2)[p + q <= 2L]
  } else {
    if (length(fixed_order) != 3L || fixed_order[2L] != 0L) {
      stop("fixed_order must be an ARMA order written as c(p, 0, q).")
    }
    data.table(p = as.integer(fixed_order[1L]), q = as.integer(fixed_order[3L]))
  }
  candidates <- lapply(seq_len(nrow(orders)), function(i) {
    tryCatch(
      if (is.null(x)) {
        forecast::Arima(
          estimation$dy,
          order = c(orders$p[i], 0L, orders$q[i]),
          include.mean = FALSE, method = "ML"
        )
      } else {
        forecast::Arima(
          estimation$dy,
          xreg = x,
          order = c(orders$p[i], 0L, orders$q[i]),
          include.mean = FALSE, method = "ML"
        )
      },
      error = function(e) NULL
    )
  })
  candidates <- Filter(Negate(is.null), candidates)
  if (!length(candidates)) stop("No estimable ARMA-error model for ", variant)
  fit <- candidates[[which.min(vapply(candidates, BIC, numeric(1)))]]
  list(
    fit = fit,
    variant = variant,
    terms = terms,
    estimation = estimation,
    first_year = min(estimation$year),
    last_year = max(estimation$year)
  )
}

fit_xreg_coefficients <- function(object) {
  values <- coef(object$fit)
  out <- setNames(rep(0, length(object$terms)), object$terms)
  common <- intersect(names(out), names(values))
  out[common] <- values[common]
  out
}

long_run_terms_for_variant <- function(variant) {
  switch(
    variant,
    with_income = slow_terms,
    with_income_65p = slow_terms_65p,
    no_income = setdiff(slow_terms, "d_log_real_gdp_per_capita"),
    structural_only = slow_terms,
    arima_benchmark = character(),
    stop("Unknown model variant: ", variant)
  )
}

forecast_bridge_changes <- function(object, table, start_year, end_year) {
  rows <- table[year >= start_year & year <= end_year]
  x <- if (length(object$terms)) {
    as.matrix(rows[, object$terms, with = FALSE])
  } else NULL
  prediction <- if (is.null(x)) {
    forecast::forecast(object$fit, h = nrow(rows))
  } else {
    forecast::forecast(object$fit, xreg = x, h = nrow(rows))
  }
  data.table(year = rows$year, predicted_change_pp = as.numeric(prediction$mean))
}

official_primary_fiscal_pp <- function(official) {
  official[, .(
    year,
    primary_fiscal_pp = 100 * (
      expenses_ratio_gdp - public_debt_interest_ratio_gdp +
        net_capital_investment_ratio_gdp
    )
  )]
}

deterministic_projection <- function(object, table, official,
                                     anchor_year = official_forecast_end,
                                     end_year = projection_end) {
  official_path <- official_primary_fiscal_pp(official)[
    year >= projection_start & year <= anchor_year
  ]
  anchor <- official_path[year == anchor_year, primary_fiscal_pp]
  rows <- table[year > anchor_year & year <= end_year]
  beta <- fit_xreg_coefficients(object)
  projection_terms <- intersect(object$terms, long_run_terms_for_variant(object$variant))
  x <- if (length(projection_terms)) {
    as.matrix(rows[, projection_terms, with = FALSE])
  } else NULL
  deterministic_change <- if (!is.null(x)) {
    as.numeric(x %*% beta[projection_terms])
  } else rep(0, nrow(rows))
  projected <- data.table(
    year = rows$year,
    primary_fiscal_pp = anchor + cumsum(deterministic_change)
  )
  rbindlist(list(official_path, projected))[, `:=`(
    model = object$variant,
    model_label = unname(model_labels[object$variant]),
    path_type = "PBO through 2029-30; deterministic model changes thereafter"
  )]
}

deterministic_projection_from_historical_anchor <- function(
    object, table, anchor_year, end_year = projection_end,
    path_type = paste0(
      "Historical actual in ", anchor_year,
      "; deterministic model changes thereafter"
    )) {
  if (!anchor_year %in% table[sample == "history", year]) {
    stop("Historical anchor year is unavailable: ", anchor_year)
  }
  anchor <- table[year == anchor_year, spending_pp]
  if (length(anchor) != 1L || !is.finite(anchor)) {
    stop("Historical spending anchor is unavailable for ", anchor_year)
  }
  rows <- table[year > anchor_year & year <= end_year]
  beta <- fit_xreg_coefficients(object)
  projection_terms <- intersect(object$terms, long_run_terms_for_variant(object$variant))
  x <- if (length(projection_terms)) {
    as.matrix(rows[, projection_terms, with = FALSE])
  } else NULL
  deterministic_change <- if (!is.null(x)) {
    as.numeric(x %*% beta[projection_terms])
  } else rep(0, nrow(rows))
  rbindlist(list(
    data.table(year = anchor_year, primary_fiscal_pp = anchor),
    data.table(
      year = rows$year,
      primary_fiscal_pp = anchor + cumsum(deterministic_change)
    )
  ))[, `:=`(
    model = object$variant,
    model_label = unname(model_labels[object$variant]),
    path_type = path_type
  )]
}

deterministic_projection_from_actual <- function(object, table,
                                                 end_year = projection_end) {
  anchor_year <- max(table[sample == "history", year])
  deterministic_projection_from_historical_anchor(
    object, table, anchor_year, end_year,
    "Latest National Accounts actual; deterministic model changes thereafter"
  )
}

model_only_projection <- function(object, table, start_year = projection_start,
                                  end_year = projection_end) {
  initial <- table[year == start_year - 1L, spending_pp]
  changes <- forecast_bridge_changes(object, table, start_year, end_year)
  changes[, `:=`(
    primary_fiscal_pp = initial + cumsum(predicted_change_pp),
    model = object$variant,
    model_label = unname(model_labels[object$variant]),
    path_type = "Conditional model-only projection from latest actual"
  )]
  changes
}

term_group <- function(term) {
  if (grepl("age_", term)) return("Age composition")
  if (term == "d_log_relative_price") return("Relative government prices")
  if (term == "d_log_real_gdp_per_capita") return("Real GDP per capita")
  if (grepl("unemployment|terms_of_trade", term)) return("Transitory macro bridge")
  if (grepl("covid", term)) return("COVID interventions")
  "Other"
}

projection_contributions <- function(object, table,
                                     anchor_year = official_forecast_end,
                                     end_year = projection_end) {
  rows <- table[year > anchor_year & year <= end_year]
  beta <- fit_xreg_coefficients(object)
  long <- rbindlist(lapply(object$terms, function(term) {
    projected <- term %in% long_run_terms_for_variant(object$variant)
    data.table(
      year = rows$year,
      term = term,
      contribution_change_pp = if (projected) rows[[term]] * beta[[term]] else 0
    )
  }))
  long[, group := vapply(term, term_group, character(1))]
  grouped <- long[, .(contribution_change_pp = sum(contribution_change_pp)),
                  by = .(year, group)]
  grouped[, cumulative_contribution_pp := cumsum(contribution_change_pp), by = group]
  anchor_rows <- unique(grouped[, .(group)])[, `:=`(
    year = anchor_year, contribution_change_pp = 0,
    cumulative_contribution_pp = 0
  )]
  rbindlist(list(anchor_rows, grouped), use.names = TRUE)[order(group, year)]
}

parameter_interval <- function(object, table, official,
                               anchor_year = official_forecast_end,
                               end_year = projection_end) {
  path <- deterministic_projection(object, table, official, anchor_year, end_year)
  rows <- table[year > anchor_year & year <= end_year]
  variance <- object$fit$var.coef
  if (is.null(variance) || !length(object$terms)) {
    path[, `:=`(standard_error_pp = NA_real_, lower_80_pp = NA_real_, upper_80_pp = NA_real_,
                lower_95_pp = NA_real_, upper_95_pp = NA_real_)]
    return(path)
  }
  common <- intersect(
    intersect(object$terms, long_run_terms_for_variant(object$variant)),
    rownames(variance)
  )
  cumulative_x <- apply(as.matrix(rows[, ..common]), 2, cumsum)
  if (is.null(dim(cumulative_x))) cumulative_x <- matrix(cumulative_x, ncol = 1L)
  v <- variance[common, common, drop = FALSE]
  se <- sqrt(pmax(0, rowSums((cumulative_x %*% v) * cumulative_x)))
  uncertainty <- data.table(year = rows$year, standard_error_pp = se)
  out <- merge(path, uncertainty, by = "year", all.x = TRUE)
  out[year <= anchor_year, standard_error_pp := 0]
  out[, `:=`(
    lower_80_pp = primary_fiscal_pp - qnorm(0.90) * standard_error_pp,
    upper_80_pp = primary_fiscal_pp + qnorm(0.90) * standard_error_pp,
    lower_95_pp = primary_fiscal_pp - qnorm(0.975) * standard_error_pp,
    upper_95_pp = primary_fiscal_pp + qnorm(0.975) * standard_error_pp
  )]
  out
}

parameter_interval_from_historical_anchor <- function(
    object, table, anchor_year, end_year = projection_end,
    path_type = paste0(
      "Historical actual in ", anchor_year,
      "; deterministic model changes thereafter"
    )) {
  path <- deterministic_projection_from_historical_anchor(
    object, table, anchor_year, end_year, path_type
  )
  rows <- table[year > anchor_year & year <= end_year]
  variance <- object$fit$var.coef
  common <- if (is.null(variance)) character() else intersect(
    intersect(object$terms, long_run_terms_for_variant(object$variant)),
    rownames(variance)
  )
  if (!length(common)) {
    path[, `:=`(standard_error_pp = NA_real_, lower_80_pp = NA_real_, upper_80_pp = NA_real_,
                lower_95_pp = NA_real_, upper_95_pp = NA_real_)]
    return(path)
  }
  cumulative_x <- apply(as.matrix(rows[, ..common]), 2, cumsum)
  if (is.null(dim(cumulative_x))) cumulative_x <- matrix(cumulative_x, ncol = 1L)
  v <- variance[common, common, drop = FALSE]
  se <- sqrt(pmax(0, rowSums((cumulative_x %*% v) * cumulative_x)))
  out <- merge(path, data.table(year = rows$year, standard_error_pp = se),
               by = "year", all.x = TRUE)
  out[year == anchor_year, standard_error_pp := 0]
  out[, `:=`(
    lower_80_pp = primary_fiscal_pp - qnorm(0.90) * standard_error_pp,
    upper_80_pp = primary_fiscal_pp + qnorm(0.90) * standard_error_pp,
    lower_95_pp = primary_fiscal_pp - qnorm(0.975) * standard_error_pp,
    upper_95_pp = primary_fiscal_pp + qnorm(0.975) * standard_error_pp
  )]
  out
}

parameter_interval_from_actual <- function(object, table,
                                           end_year = projection_end) {
  anchor_year <- max(table[sample == "history", year])
  parameter_interval_from_historical_anchor(
    object, table, anchor_year, end_year,
    "Latest National Accounts actual; deterministic model changes thereafter"
  )
}

in_sample_metrics <- function(object) {
  estimation <- object$estimation
  fitted_change <- as.numeric(fitted(object$fit))
  actual_level <- estimation$spending_pp
  fitted_level <- estimation$spending_lag_pp + fitted_change
  residual <- estimation$dy - fitted_change
  lag <- max(1L, min(5L, length(residual) - 1L))
  data.table(
    model = object$variant,
    model_label = unname(model_labels[object$variant]),
    observations = nrow(estimation),
    first_year = min(estimation$year),
    last_year = max(estimation$year),
    arima_order = paste(arimaorder(object$fit)[1:3], collapse = ","),
    change_r_squared = 1 - sum(residual^2) /
      sum((estimation$dy - mean(estimation$dy))^2),
    one_step_rmse_pp = sqrt(mean((actual_level - fitted_level)^2)),
    one_step_mae_pp = mean(abs(actual_level - fitted_level)),
    residual_ljung_box_p = Box.test(
      residual, lag = lag, type = "Ljung-Box",
      fitdf = sum(arimaorder(object$fit)[c("p", "q")])
    )$p.value,
    aicc = object$fit$aicc,
    bic = object$fit$bic
  )
}

coefficient_table <- function(object) {
  estimates <- coef(object$fit)
  variance <- object$fit$var.coef
  terms <- intersect(object$terms, names(estimates))
  se <- sqrt(diag(variance))[terms]
  data.table(
    model = object$variant,
    model_label = unname(model_labels[object$variant]),
    term = terms,
    group = vapply(terms, term_group, character(1)),
    estimate = as.numeric(estimates[terms]),
    standard_error = as.numeric(se),
    p_value = 2 * pnorm(-abs(as.numeric(estimates[terms]) / as.numeric(se))),
    lower_95 = as.numeric(estimates[terms]) - qnorm(0.975) * as.numeric(se),
    upper_95 = as.numeric(estimates[terms]) + qnorm(0.975) * as.numeric(se)
  )
}

design_diagnostics <- function(object) {
  x <- as.data.frame(object$estimation[, object$terms, with = FALSE])
  x <- x[, vapply(x, function(z) sd(z) > 1e-10, logical(1)), drop = FALSE]
  if (ncol(x) < 2L) return(data.table(
    model = object$variant, term = names(x), vif = 1,
    condition_number = 1
  ))
  vifs <- vapply(seq_len(ncol(x)), function(j) {
    fit <- lm(x[[j]] ~ ., data = x[, -j, drop = FALSE])
    1 / (1 - summary(fit)$r.squared)
  }, numeric(1))
  data.table(
    model = object$variant,
    term = names(x),
    vif = vifs,
    condition_number = kappa(scale(as.matrix(x)), exact = TRUE)
  )
}

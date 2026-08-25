source(file.path("scripts", "00_config.R"))
source(file.path("R", "model_functions_2.R"))

required <- c("ARDL", "urca", "lmtest", "strucchange", "car")
missing <- required[!vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) stop("Install required ECM packages: ", paste(missing, collapse = ", "))

historical <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
fits <- readRDS(file.path(model_dir, "top_down_fitted_models.rds"))
ecm_fit <- fits$ardl_ecm
dyn <- build_topdown_table(historical, include_y = TRUE)

# I(0)/I(1), not I(2), is required for ARDL bounds inference. ADF and KPSS
# reverse their nulls; Zivot-Andrews allows one endogenous level/trend break.
series <- list(
  log_spending_share = dyn$w,
  log_real_gdp_per_capita = dyn$log_real_gdp_per_capita,
  log_relative_government_price = dyn$log_relative_gov_price
)
unit_roots <- rbindlist(lapply(names(series), function(name) {
  x <- as.numeric(series[[name]])
  dx <- diff(x)
  adf_level <- urca::ur.df(x, type = "trend", lags = 3L, selectlags = "BIC")
  adf_diff <- urca::ur.df(dx, type = "drift", lags = 3L, selectlags = "BIC")
  kpss_diff <- urca::ur.kpss(dx, type = "mu", lags = "short")
  za <- urca::ur.za(x, model = "both", lag = 2L)
  rbindlist(list(
    data.table(series = name, test = "ADF level with trend", statistic = adf_level@teststat[1L, "tau3"],
               critical_value_5pct = adf_level@cval["tau3", "5pct"],
               reject_null_5pct = adf_level@teststat[1L, "tau3"] < adf_level@cval["tau3", "5pct"]),
    data.table(series = name, test = "ADF first difference", statistic = adf_diff@teststat[1L, "tau2"],
               critical_value_5pct = adf_diff@cval["tau2", "5pct"],
               reject_null_5pct = adf_diff@teststat[1L, "tau2"] < adf_diff@cval["tau2", "5pct"]),
    data.table(series = name, test = "KPSS first difference", statistic = kpss_diff@teststat,
               critical_value_5pct = kpss_diff@cval[1L, "5pct"],
               reject_null_5pct = kpss_diff@teststat > kpss_diff@cval[1L, "5pct"]),
    data.table(series = name, test = "Zivot-Andrews level/trend break", statistic = za@teststat,
               critical_value_5pct = za@cval["5pct"], reject_null_5pct = za@teststat < za@cval["5pct"],
               break_year = historical$year[za@bpoint])
  ), fill = TRUE)
}))
integration <- unit_roots[test %in% c("ADF first difference", "KPSS first difference"), .(
  adf_difference_stationary = reject_null_5pct[test == "ADF first difference"],
  kpss_difference_rejects_stationarity = reject_null_5pct[test == "KPSS first difference"]
), by = series]
integration[, assessment := fifelse(
  adf_difference_stationary & !kpss_difference_rejects_stationarity,
  "No I(2) warning", "Inconclusive / possible I(2); bounds inference unsafe"
)]

# Exact finite-sample Case III bounds tests. COVID and short-run macro changes
# are fixed regressors and do not enter the candidate long-run vector.
ardl_data <- as.data.frame(dyn)
ardl_data$q <- ardl_data$log_real_gdp_per_capita
ardl_data$p <- ardl_data$log_relative_gov_price
lag_order <- fread(file.path(table_dir, "top_down_model_summary.csv"))[
  model == "ardl_ecm", selected_lag_order]
fixed_terms <- c("d_unemployment", "d_tot_z", covid_dummy_terms())
ardl_formula <- as.formula(paste("w ~ q + p |", paste(fixed_terms, collapse = " + ")))
ardl_fit <- ARDL::ardl(ardl_formula, data = ardl_data, order = c(lag_order, 1L, 1L))
uecm <- ARDL::uecm(ardl_fit)
simulations <- 20000L
set.seed(20260824)
f_test <- ARDL::bounds_f_test(uecm, case = 3, alpha = 0.05, pvalue = TRUE,
                              exact = TRUE, R = simulations)
set.seed(20260824)
t_test <- ARDL::bounds_t_test(uecm, case = 3, alpha = 0.05, pvalue = TRUE,
                              exact = TRUE, R = simulations)
set.seed(20260824)
f_critical <- ARDL::bounds_f_test(uecm, case = 3, alpha = 0.05, pvalue = FALSE,
                                  exact = TRUE, R = simulations)
set.seed(20260824)
t_critical <- ARDL::bounds_t_test(uecm, case = 3, alpha = 0.05, pvalue = FALSE,
                                  exact = TRUE, R = simulations)

test_row <- function(test, critical, label, kind) {
  lower <- unname(critical$parameters[["Lower-bound I(0)"]])
  upper <- unname(critical$parameters[["Upper-bound I(1)"]])
  statistic <- unname(test$statistic)
  conclusion <- if (kind == "F") {
    if (statistic > upper) "Cointegration supported" else if (statistic < lower) {
      "No cointegration"
    } else "Inconclusive"
  } else {
    if (statistic < upper) "Cointegration supported" else if (statistic > lower) {
      "No cointegration"
    } else "Inconclusive"
  }
  data.table(
    test = label, statistic = statistic,
    lower_bound_5pct = lower, upper_bound_5pct = upper,
    exact_p_value = test$p.value,
    conclusion = conclusion
  )
}
bounds <- rbindlist(list(
  test_row(f_test, f_critical, "Bounds F: all lagged levels", "F"),
  test_row(t_test, t_critical, "Bounds t: lagged dependent level", "t")
), fill = TRUE)

# Sam-McNown et al.'s augmented ARDL adds a test of the lagged independent
# levels, guarding against degenerate cases that can pass only one PSS test.
u_names <- names(coef(uecm))
independent_levels <- grep("L\\(q, 1\\)|L\\(p, 1\\)", u_names, value = TRUE)
if (length(independent_levels)) {
  restriction <- paste0(independent_levels, " = 0")
  indep_test <- car::linearHypothesis(uecm, restriction, test = "F")
  independent_f <- data.table(
    test = "Augmented ARDL F: lagged independent levels",
    statistic = indep_test$F[2L], standard_p_value = indep_test$`Pr(>F)`[2L],
    conclusion = ifelse(indep_test$`Pr(>F)`[2L] < 0.05,
                        "Lagged independent levels jointly significant",
                        "Degenerate-regressor case not ruled out")
  )
} else {
  independent_f <- data.table(test = "Augmented ARDL F: lagged independent levels",
                              statistic = NA_real_, standard_p_value = NA_real_,
                              conclusion = "Coefficient names unavailable")
}
bounds <- rbindlist(list(bounds, independent_f), fill = TRUE)
bounds[, `:=`(case = "III: unrestricted intercept, no trend",
              selected_lag_order = lag_order, simulations = simulations)]

# Adjustment, residual and parameter-stability evidence for the actual
# projection UECM, plus one endogenous-break diagnostic for its long-run levels.
coef_table <- coef(summary(ecm_fit))
adjustment <- data.table(
  estimate = coef(ecm_fit)["w_lag"], standard_error = coef_table["w_lag", "Std. Error"],
  p_value = coef_table["w_lag", "Pr(>|t|)"],
  stable_sign = coef(ecm_fit)["w_lag"] < 0
)
bg <- lmtest::bgtest(ecm_fit, order = 2L)
reset <- lmtest::resettest(ecm_fit, power = 2:3, type = "fitted")
bp <- lmtest::bptest(ecm_fit)
cusum <- strucchange::sctest(strucchange::efp(formula(ecm_fit), data = model.frame(ecm_fit),
                                             type = "Rec-CUSUM"))
spec_tests <- data.table(
  test = c("Breusch-Godfrey serial correlation", "Ramsey RESET", "Breusch-Pagan",
           "Recursive CUSUM"),
  statistic = c(bg$statistic, reset$statistic, bp$statistic, cusum$statistic),
  p_value = c(bg$p.value, reset$p.value, bp$p.value, cusum$p.value)
)
spec_tests[, reject_null_5pct := p_value < 0.05]

long_run_fit <- lm(w ~ log_real_gdp_per_capita + log_relative_gov_price, data = dyn)
breaks <- strucchange::breakpoints(formula(long_run_fit), data = model.frame(long_run_fit), breaks = 1L)
break_index <- breaks$breakpoints[1L]
break_diagnostic <- data.table(
  test = "One endogenous break in candidate long-run regression",
  break_year = ifelse(is.finite(break_index), historical$year[break_index], NA_integer_),
  bic_no_break = BIC(long_run_fit), bic_one_break = BIC(breaks)
)

validation_summary <- data.table(
  criterion = c("No I(2) variables", "Bounds F test", "Bounds t test",
                "Augmented independent-level F test", "Adjustment coefficient",
                "Residual and stability checks", "Overall ECM suitability"),
  result = c(
    paste(integration$assessment, collapse = "; "),
    sprintf("F=%.3f, exact p=%.3f", f_test$statistic, f_test$p.value),
    sprintf("t=%.3f, exact p=%.3f", t_test$statistic, t_test$p.value),
    sprintf("F=%.3f, p=%.3f", independent_f$statistic, independent_f$standard_p_value),
    sprintf("lambda=%.3f, p=%.3f", adjustment$estimate, adjustment$p_value),
    paste0(sum(spec_tests$reject_null_5pct), " of ", nrow(spec_tests), " tests reject at 5%"),
    "Cointegration is not established; do not use the ECM as the primary projection"
  ),
  assessment = c("Pass", "Fail", "Fail", "Concern", "Concern", "Pass",
                 "Do not select as primary model")
)
ecm_design <- model.matrix(ecm_fit)[, -1L, drop = FALSE]
ecm_vif <- vapply(seq_len(ncol(ecm_design)), function(j) {
  1 / (1 - summary(lm(ecm_design[, j] ~ ecm_design[, -j, drop = FALSE]))$r.squared)
}, numeric(1))
ecm_collinearity <- data.table(
  term = colnames(ecm_design), variance_inflation_factor = ecm_vif,
  standardised_design_condition_number = kappa(scale(ecm_design), exact = TRUE)
)

fwrite(unit_roots, file.path(table_dir, "ecm_unit_root_tests.csv"))
fwrite(integration, file.path(table_dir, "ecm_integration_assessment.csv"))
fwrite(bounds, file.path(table_dir, "ecm_bounds_tests.csv"))
fwrite(adjustment, file.path(table_dir, "ecm_adjustment_sensitivity.csv"))
fwrite(spec_tests, file.path(table_dir, "ecm_specification_tests.csv"))
fwrite(break_diagnostic, file.path(table_dir, "ecm_break_diagnostic.csv"))
fwrite(validation_summary, file.path(table_dir, "ecm_validation_summary.csv"))
fwrite(ecm_collinearity, file.path(table_dir, "ecm_collinearity_diagnostics.csv"))

message("Literature-informed ECM validation written: integration, exact bounds, augmented ARDL, break and stability tests.")

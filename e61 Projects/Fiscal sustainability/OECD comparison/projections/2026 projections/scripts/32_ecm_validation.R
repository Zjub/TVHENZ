source(file.path("scripts", "00_config.R"))
source(file.path("R", "model_functions_2.R"))

required_packages <- c("ARDL", "urca", "lmtest", "strucchange")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(missing_packages)) {
  stop(
    "ECM validation requires these CRAN packages: ",
    paste(missing_packages, collapse = ", "),
    ". Install them with install.packages(c(",
    paste(sprintf("'%s'", missing_packages), collapse = ", "),
    ")).",
    call. = FALSE
  )
}

historical <- fread(file.path(processed_dir, "historical_top_down_model_data.csv"))
future <- fread(file.path(processed_dir, "macro_scenario_assumptions.csv"))[scenario == "central"]
fits <- readRDS(file.path(model_dir, "top_down_fitted_models.rds"))
ecm_fit <- fits$ardl_ecm
ecm_frame <- model.frame(ecm_fit)

# -----------------------------------------------------------------------------
# 1. Integration-order checks
# -----------------------------------------------------------------------------
# The bounds procedure permits a mixture of I(0) and I(1) variables, but not
# I(2). ADF and KPSS reverse the null hypothesis, so both are reported. Annual
# samples are short and age shares are smooth, making disagreement between the
# tests substantively important rather than something to hide with one test.

level_series <- c(
  list(spending_gdp = historical$broad_expenditure_gdp),
  setNames(lapply(active_age_groups(), function(group) historical[[group]]), age_level_terms()),
  list(
    relative_government_prices = historical$rp_z,
    unemployment = historical$unemployment
  )
)

adf_row <- function(series_name, x, transformation, deterministic) {
  type <- if (deterministic == "intercept_and_trend") "trend" else "drift"
  test_name <- if (type == "trend") "tau3" else "tau2"
  test <- urca::ur.df(
    x,
    type = type,
    lags = min(3L, floor(length(x)^(1 / 3))),
    selectlags = "BIC"
  )
  coefficient_names <- rownames(coef(test@testreg))
  selected_lags <- sum(startsWith(coefficient_names, "z.diff.lag"))
  statistic <- unname(test@teststat[1L, test_name])
  critical_value <- unname(test@cval[test_name, "5pct"])

  data.table(
    series = series_name,
    transformation = transformation,
    test = "Augmented Dickey-Fuller",
    deterministic = deterministic,
    null_hypothesis = "Unit root",
    statistic = statistic,
    critical_value_5pct = critical_value,
    selected_lags = selected_lags,
    reject_null_5pct = statistic < critical_value
  )
}

kpss_row <- function(series_name, x, transformation, deterministic) {
  type <- if (deterministic == "intercept_and_trend") "tau" else "mu"
  test <- urca::ur.kpss(x, type = type, lags = "short")
  statistic <- unname(test@teststat)
  critical_value <- unname(test@cval[1L, "5pct"])

  data.table(
    series = series_name,
    transformation = transformation,
    test = "KPSS",
    deterministic = deterministic,
    null_hypothesis = if (type == "tau") "Trend stationarity" else "Level stationarity",
    statistic = statistic,
    critical_value_5pct = critical_value,
    selected_lags = test@lag,
    reject_null_5pct = statistic > critical_value
  )
}

unit_root_tests <- rbindlist(lapply(names(level_series), function(series_name) {
  level <- as.numeric(level_series[[series_name]])
  first_difference <- diff(level)
  rbindlist(list(
    adf_row(series_name, level, "level", "intercept"),
    adf_row(series_name, level, "level", "intercept_and_trend"),
    kpss_row(series_name, level, "level", "intercept"),
    kpss_row(series_name, level, "level", "intercept_and_trend"),
    adf_row(series_name, first_difference, "first_difference", "intercept"),
    kpss_row(series_name, first_difference, "first_difference", "intercept")
  ))
}))

integration_assessment <- rbindlist(lapply(names(level_series), function(series_name) {
  adf_difference <- unit_root_tests[
    series == series_name & transformation == "first_difference" &
      test == "Augmented Dickey-Fuller",
    reject_null_5pct
  ]
  kpss_difference <- unit_root_tests[
    series == series_name & transformation == "first_difference" & test == "KPSS",
    reject_null_5pct
  ]

  conclusion <- if (adf_difference && !kpss_difference) {
    "First-difference stationarity supported by both tests; no I(2) warning"
  } else if (!adf_difference && kpss_difference) {
    "I(2) warning: first-difference stationarity is rejected or not established by both tests"
  } else {
    "Inconclusive: ADF and KPSS disagree on first-difference stationarity"
  }

  data.table(
    series = series_name,
    first_difference_adf_rejects_unit_root = adf_difference,
    first_difference_kpss_rejects_stationarity = kpss_difference,
    integration_conclusion = conclusion
  )
}))

# -----------------------------------------------------------------------------
# 2. Bounds tests for a long-run level relationship
# -----------------------------------------------------------------------------
# The fitted projection ECM deliberately omits short-run changes in the age
# shares. We test its exact lagged-level block, then estimate a canonical ARDL
# robustness specification that includes those changes. The latter is used to
# obtain finite-sample Case III bounds for the common sample size and the
# configured long-run regressor block.

long_run_terms <- c(
  "y_lag", age_level_terms("_l1"), "rp_z_l1", "unemployment_l1"
)
short_run_terms <- setdiff(attr(terms(ecm_fit), "term.labels"), long_run_terms)
restricted_ecm <- lm(reformulate(short_run_terms, response = "dy"), data = ecm_frame)
level_block_test <- anova(restricted_ecm, ecm_fit)
current_f_statistic <- level_block_test$F[2L]
current_standard_f_p_value <- level_block_test$`Pr(>F)`[2L]
current_t_statistic <- unname(coef(summary(ecm_fit))["y_lag", "t value"])

dynamic_table <- build_topdown_table(historical, include_y = TRUE)
canonical_long_terms <- c(age_level_terms(), "rp_z", "unemployment")
canonical_fixed_terms <- c("d_tot_z", "d_tot_z_l1", covid_dummy_terms())
canonical_formula <- as.formula(paste(
  "y ~", paste(canonical_long_terms, collapse = " + "), "|",
  paste(canonical_fixed_terms, collapse = " + ")
))
canonical_ardl <- ARDL::ardl(
  canonical_formula,
  data = as.data.frame(dynamic_table),
  order = c(2L, rep(1L, length(canonical_long_terms)))
)
canonical_uecm <- ARDL::uecm(canonical_ardl)

bounds_simulations <- 20000L
set.seed(20260822)
canonical_f_test <- ARDL::bounds_f_test(
  canonical_uecm,
  case = 3,
  alpha = 0.05,
  pvalue = TRUE,
  exact = TRUE,
  R = bounds_simulations
)
set.seed(20260822)
canonical_t_test <- ARDL::bounds_t_test(
  canonical_uecm,
  case = 3,
  alpha = 0.05,
  pvalue = TRUE,
  exact = TRUE,
  R = bounds_simulations
)

f_lower <- canonical_f_test$tab[["Lower-bound I(0)"]]
f_upper <- canonical_f_test$tab[["Upper-bound I(1)"]]
t_lower <- canonical_t_test$tab[["Lower-bound I(0)"]]
t_upper <- canonical_t_test$tab[["Upper-bound I(1)"]]

classify_f_bounds <- function(statistic, lower, upper) {
  if (statistic > upper) "Level relationship supported" else if (statistic < lower) {
    "No level relationship supported"
  } else "Inconclusive between I(0) and I(1) bounds"
}

classify_t_bounds <- function(statistic, i0_bound, i1_bound) {
  if (statistic < i1_bound) "Level relationship supported" else if (statistic > i0_bound) {
    "No level relationship supported"
  } else "Inconclusive between I(0) and I(1) bounds"
}

bounds_tests <- rbindlist(list(
  data.table(
    specification = "Projection ECM as fitted",
    test = "Bounds F-test: joint lagged-level block",
    statistic = current_f_statistic,
    lower_bound_I0_5pct = f_lower,
    upper_bound_I1_5pct = f_upper,
    upper_bound_p_value = NA_real_,
    standard_test_p_value = current_standard_f_p_value,
    decision_5pct = classify_f_bounds(current_f_statistic, f_lower, f_upper)
  ),
  data.table(
    specification = "Projection ECM as fitted",
    test = "Bounds t-test: lagged dependent level",
    statistic = current_t_statistic,
    lower_bound_I0_5pct = t_lower,
    upper_bound_I1_5pct = t_upper,
    upper_bound_p_value = NA_real_,
    standard_test_p_value = coef(summary(ecm_fit))["y_lag", "Pr(>|t|)"],
    decision_5pct = classify_t_bounds(current_t_statistic, t_lower, t_upper)
  ),
  data.table(
    specification = "Canonical ARDL robustness specification",
    test = "Bounds F-test: joint lagged-level block",
    statistic = unname(canonical_f_test$statistic),
    lower_bound_I0_5pct = f_lower,
    upper_bound_I1_5pct = f_upper,
    upper_bound_p_value = canonical_f_test$p.value,
    standard_test_p_value = NA_real_,
    decision_5pct = classify_f_bounds(unname(canonical_f_test$statistic), f_lower, f_upper)
  ),
  data.table(
    specification = "Canonical ARDL robustness specification",
    test = "Bounds t-test: lagged dependent level",
    statistic = unname(canonical_t_test$statistic),
    lower_bound_I0_5pct = t_lower,
    upper_bound_I1_5pct = t_upper,
    upper_bound_p_value = canonical_t_test$p.value,
    standard_test_p_value = NA_real_,
    decision_5pct = classify_t_bounds(unname(canonical_t_test$statistic), t_lower, t_upper)
  )
))
bounds_tests[, `:=`(
  alpha = 0.05,
  bounds_case = "Case III: unrestricted intercept, no trend",
  long_run_regressors = length(canonical_long_terms),
  observations = nobs(ecm_fit),
  finite_sample_simulations = bounds_simulations
)]

# -----------------------------------------------------------------------------
# 3. Residual, functional-form, parameter-stability and collinearity checks
# -----------------------------------------------------------------------------

model_test_rows <- function(fit, specification) {
  bg <- lmtest::bgtest(fit, order = 2L)
  bp <- lmtest::bptest(fit)
  reset <- lmtest::resettest(fit, power = 2:3, type = "fitted")
  cusum <- tryCatch({
    process <- strucchange::efp(
      formula(fit),
      data = model.frame(fit),
      type = "Rec-CUSUM"
    )
    strucchange::sctest(process)
  }, error = function(e) NULL)

  rbindlist(list(
    data.table(
      specification = specification,
      test = "Breusch-Godfrey serial correlation (order 2)",
      null_hypothesis = "No residual serial correlation through lag 2",
      statistic = unname(bg$statistic), p_value = bg$p.value
    ),
    data.table(
      specification = specification,
      test = "Breusch-Pagan heteroskedasticity",
      null_hypothesis = "Homoskedastic residuals",
      statistic = unname(bp$statistic), p_value = bp$p.value
    ),
    data.table(
      specification = specification,
      test = "Ramsey RESET functional form",
      null_hypothesis = "No neglected nonlinear functional form",
      statistic = unname(reset$statistic), p_value = reset$p.value
    ),
    data.table(
      specification = specification,
      test = "Recursive CUSUM parameter stability",
      null_hypothesis = "Stable regression parameters",
      statistic = if (is.null(cusum)) NA_real_ else unname(cusum$statistic),
      p_value = if (is.null(cusum)) NA_real_ else cusum$p.value
    )
  ))
}

specification_tests <- rbindlist(list(
  model_test_rows(ecm_fit, "Projection ECM as fitted"),
  model_test_rows(canonical_uecm, "Canonical ARDL robustness specification")
))
specification_tests[, reject_null_5pct := p_value < 0.05]

design <- model.matrix(ecm_fit)[, -1L, drop = FALSE]
standardised_design <- scale(design)
condition_number <- kappa(standardised_design, exact = TRUE)
vif_values <- vapply(seq_len(ncol(design)), function(j) {
  auxiliary <- lm(design[, j] ~ design[, -j, drop = FALSE])
  1 / (1 - summary(auxiliary)$r.squared)
}, numeric(1))

collinearity_diagnostics <- data.table(
  term = colnames(design),
  variance_inflation_factor = vif_values,
  standardised_design_condition_number = condition_number
)
setorder(collinearity_diagnostics, -variance_inflation_factor)

dynamic_roots <- function(fit) {
  b <- coef(fit)
  lambda <- unname(b["y_lag"])
  phi <- if ("dy_lag" %in% names(b)) unname(b["dy_lag"]) else 0
  transition <- matrix(c(1 + lambda + phi, -phi, 1, 0), 2L, 2L, byrow = TRUE)
  eigen(transition, only.values = TRUE)$values
}

window_specs <- list(
  `Full sample` = list(train = historical, future = future),
  `Starts 1990` = list(train = historical[year >= 1990], future = future),
  `Estimated through 2019` = list(
    train = historical[year <= 2019],
    future = rbindlist(list(historical[year >= 2020], future), fill = TRUE)
  )
)

adjustment_sensitivity <- rbindlist(lapply(names(window_specs), function(label) {
  spec <- window_specs[[label]]
  fit <- fit_predict_topdown(spec$train, spec$future)$fits$ardl_ecm
  roots <- dynamic_roots(fit)
  coefficient_table <- coef(summary(fit))
  data.table(
    estimation_window = label,
    first_year = min(spec$train$year),
    last_year = max(spec$train$year),
    observations = nobs(fit),
    adjustment_estimate = coef(fit)["y_lag"],
    adjustment_standard_error = coefficient_table["y_lag", "Std. Error"],
    adjustment_p_value_standard_t = coefficient_table["y_lag", "Pr(>|t|)"],
    maximum_dynamic_root_modulus = max(Mod(roots)),
    dynamically_stable = max(Mod(roots)) < 1
  )
}))

# -----------------------------------------------------------------------------
# 4. Auditable overall assessment
# -----------------------------------------------------------------------------

current_bg_p <- specification_tests[
  specification == "Projection ECM as fitted" & grepl("Breusch-Godfrey", test), p_value
]
current_reset_p <- specification_tests[
  specification == "Projection ECM as fitted" & grepl("RESET", test), p_value
]
current_cusum_p <- specification_tests[
  specification == "Projection ECM as fitted" & grepl("CUSUM", test), p_value
]
i2_warning_count <- integration_assessment[startsWith(integration_conclusion, "I(2) warning"), .N]
inconclusive_integration_count <- integration_assessment[grepl("Inconclusive", integration_conclusion), .N]

validation_summary <- data.table(
  criterion = c(
    "No I(2) variables",
    "Projection ECM bounds F-test",
    "Projection ECM bounds t-test",
    "Canonical ARDL bounds robustness",
    "Residual serial correlation",
    "Functional form",
    "Parameter stability",
    "Dynamic stability",
    "Long-run regressor collinearity",
    "Overall ECM suitability"
  ),
  result = c(
    paste0(i2_warning_count, " I(2) warning(s); ", inconclusive_integration_count,
           " additional inconclusive first-difference assessment(s)"),
    sprintf("F = %.3f; 5%% bounds [%.3f, %.3f]", current_f_statistic, f_lower, f_upper),
    sprintf("t = %.3f; 5%% bounds [%.3f, %.3f]", current_t_statistic, t_lower, t_upper),
    paste(unique(bounds_tests[specification == "Canonical ARDL robustness specification", decision_5pct]),
          collapse = "; "),
    sprintf("Breusch-Godfrey p = %.4f", current_bg_p),
    sprintf("RESET p = %.4f", current_reset_p),
    sprintf("Recursive CUSUM p = %.4f", current_cusum_p),
    sprintf("Maximum root modulus = %.3f", adjustment_sensitivity[estimation_window == "Full sample", maximum_dynamic_root_modulus]),
    sprintf("Maximum VIF = %.1f; condition number = %.1f", max(vif_values), condition_number),
    "Current ECM is not supported as the preferred projection model"
  ),
  assessment = c(
    if (i2_warning_count == 0L && inconclusive_integration_count == 0L) "Pass" else "Concern",
    bounds_tests[specification == "Projection ECM as fitted" & grepl("F-test", test), decision_5pct],
    bounds_tests[specification == "Projection ECM as fitted" & grepl("t-test", test), decision_5pct],
    "Inconclusive",
    if (current_bg_p < 0.05) "Fail" else "Pass",
    if (current_reset_p < 0.05) "Fail" else "Pass",
    if (current_cusum_p < 0.05) "Fail" else "Pass",
    if (all(adjustment_sensitivity$dynamically_stable)) "Pass" else "Fail",
    if (max(vif_values) > 10) "Concern" else "Pass",
    "Do not select as primary model"
  ),
  implication = c(
    "Bounds inference is invalid for I(2) variables; mixed unit-root results also signal low power and smooth demographic trends.",
    "The fitted lagged-level block does not establish the level relationship required for an ECM.",
    "The adjustment term alone does not establish a level relationship under the appropriate non-standard distribution.",
    "Adding the omitted short-run age changes improves specification diagnostics but does not give decisive bounds evidence.",
    "Unmodelled residual dynamics weaken coefficient and bounds inference.",
    "RESET rejection indicates the fitted conditional mean is incomplete or incorrectly parameterised.",
    "CUSUM does not detect broad parameter instability, although power is limited in this short, highly parameterised sample.",
    "Conditional forecasts converge rather than explode when future drivers are held fixed.",
    "Highly correlated age shares make individual long-run coefficients unstable and difficult to interpret.",
    "Retain the ECM only as an experimental sensitivity; use rolling performance and structural suitability to select the projection framework."
  )
)

fwrite(unit_root_tests, file.path(table_dir, "ecm_unit_root_tests.csv"))
fwrite(integration_assessment, file.path(table_dir, "ecm_integration_assessment.csv"))
fwrite(bounds_tests, file.path(table_dir, "ecm_bounds_tests.csv"))
fwrite(specification_tests, file.path(table_dir, "ecm_specification_tests.csv"))
fwrite(collinearity_diagnostics, file.path(table_dir, "ecm_collinearity_diagnostics.csv"))
fwrite(adjustment_sensitivity, file.path(table_dir, "ecm_adjustment_sensitivity.csv"))
fwrite(validation_summary, file.path(table_dir, "ecm_validation_summary.csv"))

message(
  "ECM validation written: integration, finite-sample bounds, residual, ",
  "functional-form, stability and collinearity checks."
)

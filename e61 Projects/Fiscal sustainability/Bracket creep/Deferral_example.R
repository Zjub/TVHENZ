# ============================================================
# Flat/progressive tax, deferral, and risky business choice
# Australian-style progressive tax scale
# ============================================================

library(data.table)
library(ggplot2)
library(scales)

# ============================================================
# Parameters
# ============================================================

# Main tax switch:
# TRUE  = flat tax
# FALSE = Australian progressive resident income tax scale
use_flat_tax <- TRUE

annual_labour_income <- 100000
risk_free_return <- 0.05

T_max <- 10
focus_T <- 10

failure_prob <- 0.50
risk_aversion <- 1 # Is a low level of risk aversion, but currently breaking on the values estimated for Australian households (1.3-1.5).
background_wealth <- 100000 # Set to only a year of income - this income is not at risk

# If use_flat_tax == TRUE, the flat tax rate is set equal to the
# marginal Australian tax rate applying at this reference income.
# With annual_labour_income = 100000, this is 30%.
flat_rate_reference_income <- annual_labour_income

# For progressive tax on a final capital gain, this allows you to assume
# the person has other taxable income in the sale year.
# Set to 0 to isolate the capital-gain sale.
sale_year_other_income <- 0

# ============================================================
# Australian resident tax scale
# Excludes Medicare levy and offsets.
# ============================================================

australian_income_tax <- function(income) {
  income <- pmax(income, 0)

  tax <-
    pmax(pmin(income, 45000) - 18200, 0) * 0.16 +
    pmax(pmin(income, 135000) - 45000, 0) * 0.30 +
    pmax(pmin(income, 190000) - 135000, 0) * 0.37 +
    pmax(income - 190000, 0) * 0.45

  tax
}

australian_marginal_tax_rate <- function(income) {
  fifelse(
    income <= 18200, 0,
    fifelse(
      income <= 45000, 0.16,
      fifelse(
        income <= 135000, 0.30,
        fifelse(
          income <= 190000, 0.37,
          0.45
        )
      )
    )
  )
}

flat_tax_rate <- australian_marginal_tax_rate(flat_rate_reference_income)

tax_payable <- function(income,
                        use_flat_tax = TRUE,
                        flat_tax_rate = 0.30) {

  income <- pmax(income, 0)

  if (use_flat_tax) {
    return(flat_tax_rate * income)
  } else {
    return(australian_income_tax(income))
  }
}

tax_on_increment <- function(base_income,
                             increment,
                             use_flat_tax = TRUE,
                             flat_tax_rate = 0.30) {

  tax_payable(
    base_income + increment,
    use_flat_tax = use_flat_tax,
    flat_tax_rate = flat_tax_rate
  ) -
    tax_payable(
      base_income,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate
    )
}

tax_system_label <- function(use_flat_tax, flat_tax_rate) {
  if (use_flat_tax) {
    paste0("Flat tax: ", percent(flat_tax_rate, accuracy = 1))
  } else {
    "Australian progressive scale"
  }
}

# ============================================================
# Utility functions
# ============================================================

utility <- function(w, gamma) {
  if (any(w <= 0)) {
    stop("CRRA utility requires strictly positive total wealth.")
  }

  if (gamma == 1) {
    log(w)
  } else {
    w^(1 - gamma) / (1 - gamma)
  }
}

certainty_equivalent <- function(outcomes, probs, gamma) {
  eu <- sum(probs * utility(outcomes, gamma))

  if (gamma == 1) {
    exp(eu)
  } else {
    ((1 - gamma) * eu)^(1 / (1 - gamma))
  }
}

find_success_payoff <- function(safe_incremental_wealth,
                                failure_prob,
                                background_wealth,
                                risk_aversion) {

  target_total_wealth <- background_wealth + safe_incremental_wealth

  objective <- function(success_payoff) {
    ce <- certainty_equivalent(
      outcomes = c(
        background_wealth,
        background_wealth + success_payoff
      ),
      probs = c(failure_prob, 1 - failure_prob),
      gamma = risk_aversion
    )

    ce - target_total_wealth
  }

  lower <- 0
  upper <- max(safe_incremental_wealth * 2, 1)

  max_iter <- 100
  iter <- 0

  while (objective(upper) < 0 && iter < max_iter) {
    upper <- upper * 2
    iter <- iter + 1
  }

  if (iter == max_iter) {
    return(NA_real_)
  }

  uniroot(objective, lower = lower, upper = upper)$root
}

# ============================================================
# Safe work / bank account paths
# ============================================================

simulate_safe_work <- function(T,
                               annual_labour_income,
                               risk_free_return,
                               use_flat_tax,
                               flat_tax_rate,
                               tax_system = c("no_tax", "income_tax")) {

  tax_system <- match.arg(tax_system)

  bank_balance <- 0

  for (year in 1:T) {

    if (tax_system == "no_tax") {
      bank_balance <- bank_balance * (1 + risk_free_return)
      bank_balance <- bank_balance + annual_labour_income
    }

    if (tax_system == "income_tax") {
      interest_income <- bank_balance * risk_free_return
      taxable_income <- annual_labour_income + interest_income

      annual_tax <- tax_payable(
        taxable_income,
        use_flat_tax = use_flat_tax,
        flat_tax_rate = flat_tax_rate
      )

      bank_balance <- bank_balance + interest_income + annual_labour_income - annual_tax
    }
  }

  bank_balance
}

# ============================================================
# Part 1: Hidden labour income as a deferred capital gain
# ============================================================

simulate_hidden_labour_case <- function(T_max,
                                        annual_labour_income,
                                        risk_free_return,
                                        use_flat_tax,
                                        flat_tax_rate,
                                        sale_year_other_income = 0) {

  out <- vector("list", T_max)

  for (T in 1:T_max) {

    # Safe worker: labour income and bank interest taxed each year.
    wage_bank_after_tax <- simulate_safe_work(
      T = T,
      annual_labour_income = annual_labour_income,
      risk_free_return = risk_free_return,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      tax_system = "income_tax"
    )

    # Business builder: labour contribution embedded in business pre-tax,
    # compounding pre-tax, then taxed only at sale.
    business_before_tax <- simulate_safe_work(
      T = T,
      annual_labour_income = annual_labour_income,
      risk_free_return = risk_free_return,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      tax_system = "no_tax"
    )

    # Founder / sweat-equity assumption: zero cost base.
    business_tax_at_sale <- tax_on_increment(
      base_income = sale_year_other_income,
      increment = business_before_tax,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate
    )

    business_after_tax_sale <- business_before_tax - business_tax_at_sale

    # Tax at sale needed to exactly neutralise the deferral advantage.
    neutralising_tax <- business_before_tax - wage_bank_after_tax

    out[[T]] <- data.table(
      year = T,
      wage_bank_after_tax = wage_bank_after_tax,
      business_before_tax = business_before_tax,
      business_after_tax_sale = business_after_tax_sale,
      business_tax_at_sale = business_tax_at_sale,
      neutralising_tax = neutralising_tax,
      deferral_benefit = business_after_tax_sale - wage_bank_after_tax,
      deferral_benefit_pct = business_after_tax_sale / wage_bank_after_tax - 1
    )
  }

  rbindlist(out)
}

# ============================================================
# Part 2: Risky business calibrated to no-tax indifference
# ============================================================

simulate_risky_indifference_case <- function(T_max,
                                             annual_labour_income,
                                             risk_free_return,
                                             use_flat_tax,
                                             flat_tax_rate,
                                             failure_prob,
                                             background_wealth,
                                             risk_aversion,
                                             sale_year_other_income = 0) {

  out <- vector("list", T_max)

  for (T in 1:T_max) {

    # Safe work in the absence of tax.
    safe_work_no_tax <- simulate_safe_work(
      T = T,
      annual_labour_income = annual_labour_income,
      risk_free_return = risk_free_return,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      tax_system = "no_tax"
    )

    # Safe work under the selected tax system.
    safe_work_after_tax <- simulate_safe_work(
      T = T,
      annual_labour_income = annual_labour_income,
      risk_free_return = risk_free_return,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      tax_system = "income_tax"
    )

    # Choose business success payoff so the person is indifferent
    # between safe work and risky business before tax.
    success_payoff_no_tax <- find_success_payoff(
      safe_incremental_wealth = safe_work_no_tax,
      failure_prob = failure_prob,
      background_wealth = background_wealth,
      risk_aversion = risk_aversion
    )

    if (is.na(success_payoff_no_tax)) {
      out[[T]] <- data.table(
        year = T,
        safe_work_no_tax = safe_work_no_tax,
        safe_work_after_tax = safe_work_after_tax,
        business_failure_no_tax = 0,
        business_success_no_tax = NA_real_,
        business_expected_no_tax = NA_real_,
        business_ce_no_tax = NA_real_,
        business_failure_after_tax = 0,
        business_success_after_tax = NA_real_,
        business_expected_after_tax = NA_real_,
        business_ce_after_tax = NA_real_,
        tax_induced_gap = NA_real_,
        expected_tax_induced_gap = NA_real_,
        required_success_multiple = NA_real_
      )
      next
    }

    failure_payoff_no_tax <- 0

    ce_business_no_tax_total <- certainty_equivalent(
      outcomes = c(
        background_wealth + failure_payoff_no_tax,
        background_wealth + success_payoff_no_tax
      ),
      probs = c(failure_prob, 1 - failure_prob),
      gamma = risk_aversion
    )

    ce_business_no_tax_incremental <- ce_business_no_tax_total - background_wealth

    # Tax applied only in success state at sale.
    # Failure has zero business value and no refundable tax credit.
    success_tax <- tax_on_increment(
      base_income = sale_year_other_income,
      increment = success_payoff_no_tax,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate
    )

    success_payoff_after_tax <- success_payoff_no_tax - success_tax
    failure_payoff_after_tax <- 0

    ce_business_after_tax_total <- certainty_equivalent(
      outcomes = c(
        background_wealth + failure_payoff_after_tax,
        background_wealth + success_payoff_after_tax
      ),
      probs = c(failure_prob, 1 - failure_prob),
      gamma = risk_aversion
    )

    ce_business_after_tax_incremental <- ce_business_after_tax_total - background_wealth

    expected_business_no_tax <-
      failure_prob * failure_payoff_no_tax +
      (1 - failure_prob) * success_payoff_no_tax

    expected_business_after_tax <-
      failure_prob * failure_payoff_after_tax +
      (1 - failure_prob) * success_payoff_after_tax

    out[[T]] <- data.table(
      year = T,

      safe_work_no_tax = safe_work_no_tax,
      safe_work_after_tax = safe_work_after_tax,

      business_failure_no_tax = failure_payoff_no_tax,
      business_success_no_tax = success_payoff_no_tax,
      business_expected_no_tax = expected_business_no_tax,
      business_ce_no_tax = ce_business_no_tax_incremental,

      business_failure_after_tax = failure_payoff_after_tax,
      business_success_after_tax = success_payoff_after_tax,
      business_expected_after_tax = expected_business_after_tax,
      business_ce_after_tax = ce_business_after_tax_incremental,

      tax_induced_gap = ce_business_after_tax_incremental - safe_work_after_tax,
      expected_tax_induced_gap = expected_business_after_tax - safe_work_after_tax,

      required_success_multiple = success_payoff_no_tax / safe_work_no_tax
    )
  }

  rbindlist(out)
}

# ============================================================
# Run model under selected tax system
# ============================================================

selected_tax_label <- tax_system_label(use_flat_tax, flat_tax_rate)

hidden <- simulate_hidden_labour_case(
  T_max = T_max,
  annual_labour_income = annual_labour_income,
  risk_free_return = risk_free_return,
  use_flat_tax = use_flat_tax,
  flat_tax_rate = flat_tax_rate,
  sale_year_other_income = sale_year_other_income
)

risky <- simulate_risky_indifference_case(
  T_max = T_max,
  annual_labour_income = annual_labour_income,
  risk_free_return = risk_free_return,
  use_flat_tax = use_flat_tax,
  flat_tax_rate = flat_tax_rate,
  failure_prob = failure_prob,
  background_wealth = background_wealth,
  risk_aversion = risk_aversion,
  sale_year_other_income = sale_year_other_income
)

print(hidden[year == focus_T])
print(risky[year == focus_T])

# ============================================================
# Plots for selected tax system
# ============================================================

# -----------------------------
# Plot 1: hidden labour case
# -----------------------------

hidden_plot_data <- melt(
  hidden,
  id.vars = "year",
  measure.vars = c(
    "wage_bank_after_tax",
    "business_after_tax_sale"
  ),
  variable.name = "scenario",
  value.name = "after_tax_wealth"
)

hidden_plot_data[, scenario := fifelse(
  scenario == "wage_bank_after_tax",
  "Safe work: labour and interest taxed annually",
  "Hidden labour as capital gain: taxed only at sale"
)]

p_hidden <- ggplot(
  hidden_plot_data,
  aes(x = year, y = after_tax_wealth, colour = scenario)
) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Case 1: Labour income hidden as a deferred capital gain",
    subtitle = selected_tax_label,
    x = "Years",
    y = "After-tax wealth",
    colour = NULL
  )

print(p_hidden)

# -----------------------------
# Plot 2: hidden labour deferral benefit
# -----------------------------

p_hidden_gap <- ggplot(hidden, aes(x = year, y = deferral_benefit)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Deferral benefit from taxing hidden labour only at sale",
    subtitle = selected_tax_label,
    x = "Years",
    y = "Business after-tax wealth minus wage/bank wealth"
  )

print(p_hidden_gap)

# -----------------------------
# Plot 3: no-tax calibration
# -----------------------------

pretax_plot_data <- melt(
  risky,
  id.vars = "year",
  measure.vars = c(
    "safe_work_no_tax",
    "business_ce_no_tax",
    "business_expected_no_tax",
    "business_success_no_tax",
    "business_failure_no_tax"
  ),
  variable.name = "scenario",
  value.name = "wealth"
)

pretax_plot_data[, scenario := fifelse(
  scenario == "safe_work_no_tax",
  "Safe work: no tax",
  fifelse(
    scenario == "business_ce_no_tax",
    "Risky business: certainty equivalent, no tax",
    fifelse(
      scenario == "business_expected_no_tax",
      "Risky business: expected value, no tax",
      fifelse(
        scenario == "business_success_no_tax",
        "Risky business: success payoff, no tax",
        "Risky business: failure payoff, no tax"
      )
    )
  )
)]

p_pretax <- ggplot(
  pretax_plot_data,
  aes(x = year, y = wealth, colour = scenario)
) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Case 2: Risky business calibrated to no-tax indifference",
    subtitle = paste0(
      "Failure probability = ", percent(failure_prob),
      ", risk aversion = ", risk_aversion,
      ", background wealth = ", dollar(background_wealth)
    ),
    x = "Years",
    y = "Incremental wealth",
    colour = NULL
  )

print(p_pretax)

# -----------------------------
# Plot 4: after-tax risky comparison
# -----------------------------

aftertax_plot_data <- melt(
  risky,
  id.vars = "year",
  measure.vars = c(
    "safe_work_after_tax",
    "business_ce_after_tax",
    "business_expected_after_tax"
  ),
  variable.name = "scenario",
  value.name = "wealth"
)

aftertax_plot_data[, scenario := fifelse(
  scenario == "safe_work_after_tax",
  "Safe work: taxed annually",
  fifelse(
    scenario == "business_ce_after_tax",
    "Risky business: certainty equivalent after tax",
    "Risky business: expected value after tax"
  )
)]

p_aftertax <- ggplot(
  aftertax_plot_data,
  aes(x = year, y = wealth, colour = scenario)
) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "How the selected tax system changes the risky-business choice",
    subtitle = selected_tax_label,
    x = "Years",
    y = "After-tax incremental wealth",
    colour = NULL
  )

print(p_aftertax)

# -----------------------------
# Plot 5: tax-induced incentive
# -----------------------------

p_tax_gap <- ggplot(risky, aes(x = year, y = tax_induced_gap)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Tax-induced incentive toward the risky business",
    subtitle = "Positive values mean tax makes the risky business more attractive than safe work",
    x = "Years",
    y = "Risky business certainty equivalent minus safe work wealth"
  )

print(p_tax_gap)

# ============================================================
# Summary table for selected tax system
# ============================================================

summary_table <- risky[year == focus_T, .(
  tax_system = selected_tax_label,
  year,
  safe_work_no_tax,
  risky_business_ce_no_tax = business_ce_no_tax,
  risky_business_expected_no_tax = business_expected_no_tax,
  risky_business_success_payoff_no_tax = business_success_no_tax,
  required_success_multiple,
  safe_work_after_tax,
  risky_business_ce_after_tax = business_ce_after_tax,
  risky_business_expected_after_tax = business_expected_after_tax,
  tax_induced_gap,
  expected_tax_induced_gap
)]

print(summary_table)

# ============================================================
# Final section: compare flat vs progressive tax scales
# ============================================================

run_model_for_tax_system <- function(use_flat_tax_input) {

  label <- tax_system_label(use_flat_tax_input, flat_tax_rate)

  hidden_out <- simulate_hidden_labour_case(
    T_max = T_max,
    annual_labour_income = annual_labour_income,
    risk_free_return = risk_free_return,
    use_flat_tax = use_flat_tax_input,
    flat_tax_rate = flat_tax_rate,
    sale_year_other_income = sale_year_other_income
  )

  risky_out <- simulate_risky_indifference_case(
    T_max = T_max,
    annual_labour_income = annual_labour_income,
    risk_free_return = risk_free_return,
    use_flat_tax = use_flat_tax_input,
    flat_tax_rate = flat_tax_rate,
    failure_prob = failure_prob,
    background_wealth = background_wealth,
    risk_aversion = risk_aversion,
    sale_year_other_income = sale_year_other_income
  )

  hidden_out[, tax_system := label]
  risky_out[, tax_system := label]

  list(
    hidden = hidden_out,
    risky = risky_out
  )
}

flat_results <- run_model_for_tax_system(TRUE)
progressive_results <- run_model_for_tax_system(FALSE)

comparison_hidden <- rbindlist(
  list(
    flat_results$hidden,
    progressive_results$hidden
  ),
  use.names = TRUE
)

comparison_risky <- rbindlist(
  list(
    flat_results$risky,
    progressive_results$risky
  ),
  use.names = TRUE
)

# -----------------------------
# Comparison plot 1:
# Hidden-labour deferral benefit
# -----------------------------

p_compare_hidden <- ggplot(
  comparison_hidden,
  aes(x = year, y = deferral_benefit, colour = tax_system)
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Hidden labour: deferral benefit under flat vs progressive tax",
    subtitle = "Business after-tax sale wealth minus safe wage/bank wealth",
    x = "Years",
    y = "Deferral benefit",
    colour = NULL
  )

print(p_compare_hidden)

# -----------------------------
# Comparison plot 2:
# Risky-business tax-induced incentive
# -----------------------------

p_compare_risky <- ggplot(
  comparison_risky,
  aes(x = year, y = tax_induced_gap, colour = tax_system)
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Risky business: tax-induced incentive under flat vs progressive tax",
    subtitle = "Positive values mean tax makes the risky business more attractive than safe work",
    x = "Years",
    y = "Risky business certainty equivalent minus safe work wealth",
    colour = NULL
  )

print(p_compare_risky)

# -----------------------------
# Comparison table at focus_T
# -----------------------------

comparison_table <- merge(
  comparison_hidden[year == focus_T, .(
    tax_system,
    hidden_labour_deferral_benefit = deferral_benefit,
    hidden_labour_deferral_benefit_pct = deferral_benefit_pct
  )],
  comparison_risky[year == focus_T, .(
    tax_system,
    safe_work_after_tax,
    risky_business_ce_after_tax = business_ce_after_tax,
    risky_business_expected_after_tax = business_expected_after_tax,
    risky_business_tax_induced_gap = tax_induced_gap,
    risky_business_expected_gap = expected_tax_induced_gap
  )],
  by = "tax_system"
)

print(comparison_table)

# ============================================================
# Extra section: income averaging for realised capital gains
# ============================================================

# ------------------------------------------------------------
# What this does
# ------------------------------------------------------------
# Normal realisation:
#   The full capital gain is taxed in the sale year.
#
# Income averaging:
#   The capital gain is divided equally across the prior T years.
#   Tax is calculated as if gain/T was earned in each year.
#   The total tax is assessed at the end.
#
# Important:
#   This removes progressive-rate bunching.
#   It does NOT remove the time-value deferral benefit unless
#   apply_interest_to_averaged_tax is set to TRUE.
# ------------------------------------------------------------

apply_interest_to_averaged_tax <- FALSE

# This controls what other income the taxpayer is assumed to have
# in the years over which the capital gain is averaged.
#
# For a pure founder/sweat-equity case, use 0.
# For a case where the person also has other annual taxable income,
# set this to annual_labour_income or another amount.
averaging_base_income_per_year <- 0

tax_on_averaged_increment <- function(total_increment,
                                      T,
                                      base_income_per_year = 0,
                                      use_flat_tax = TRUE,
                                      flat_tax_rate = 0.30,
                                      apply_interest_to_averaged_tax = FALSE,
                                      risk_free_return = 0.05) {

  if (T <= 0) {
    stop("T must be positive.")
  }

  annual_increment <- total_increment / T

  annual_incremental_tax <- tax_on_increment(
    base_income = base_income_per_year,
    increment = annual_increment,
    use_flat_tax = use_flat_tax,
    flat_tax_rate = flat_tax_rate
  )

  if (!apply_interest_to_averaged_tax) {
    return(T * annual_incremental_tax)
  }

  # Optional version: grow each year's notional tax to the sale date.
  # This removes both bunching and the time-value deferral benefit.
  years_until_sale <- (T - 1):0

  sum(annual_incremental_tax * (1 + risk_free_return)^years_until_sale)
}

# ============================================================
# Hidden labour case with income averaging
# ============================================================

simulate_hidden_labour_case_with_averaging <- function(T_max,
                                                       annual_labour_income,
                                                       risk_free_return,
                                                       use_flat_tax,
                                                       flat_tax_rate,
                                                       sale_year_other_income = 0,
                                                       averaging_base_income_per_year = 0,
                                                       apply_interest_to_averaged_tax = FALSE) {

  out <- vector("list", T_max)

  for (T in 1:T_max) {

    wage_bank_after_tax <- simulate_safe_work(
      T = T,
      annual_labour_income = annual_labour_income,
      risk_free_return = risk_free_return,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      tax_system = "income_tax"
    )

    business_before_tax <- simulate_safe_work(
      T = T,
      annual_labour_income = annual_labour_income,
      risk_free_return = risk_free_return,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      tax_system = "no_tax"
    )

    # Normal tax: full gain taxed in sale year.
    normal_tax_at_sale <- tax_on_increment(
      base_income = sale_year_other_income,
      increment = business_before_tax,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate
    )

    business_after_normal_tax <- business_before_tax - normal_tax_at_sale

    # Averaging tax: gain split equally across prior years.
    averaged_tax_at_sale <- tax_on_averaged_increment(
      total_increment = business_before_tax,
      T = T,
      base_income_per_year = averaging_base_income_per_year,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      apply_interest_to_averaged_tax = apply_interest_to_averaged_tax,
      risk_free_return = risk_free_return
    )

    business_after_averaging_tax <- business_before_tax - averaged_tax_at_sale

    out[[T]] <- data.table(
      year = T,
      wage_bank_after_tax = wage_bank_after_tax,
      business_before_tax = business_before_tax,

      normal_tax_at_sale = normal_tax_at_sale,
      business_after_normal_tax = business_after_normal_tax,
      normal_deferral_benefit = business_after_normal_tax - wage_bank_after_tax,

      averaged_tax_at_sale = averaged_tax_at_sale,
      business_after_averaging_tax = business_after_averaging_tax,
      averaging_deferral_benefit = business_after_averaging_tax - wage_bank_after_tax,

      averaging_tax_saving = normal_tax_at_sale - averaged_tax_at_sale,
      averaging_extra_incentive =
        business_after_averaging_tax - business_after_normal_tax
    )
  }

  rbindlist(out)
}

# ============================================================
# Risky business case with income averaging
# ============================================================

simulate_risky_indifference_case_with_averaging <- function(T_max,
                                                            annual_labour_income,
                                                            risk_free_return,
                                                            use_flat_tax,
                                                            flat_tax_rate,
                                                            failure_prob,
                                                            background_wealth,
                                                            risk_aversion,
                                                            sale_year_other_income = 0,
                                                            averaging_base_income_per_year = 0,
                                                            apply_interest_to_averaged_tax = FALSE) {

  out <- vector("list", T_max)

  for (T in 1:T_max) {

    safe_work_no_tax <- simulate_safe_work(
      T = T,
      annual_labour_income = annual_labour_income,
      risk_free_return = risk_free_return,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      tax_system = "no_tax"
    )

    safe_work_after_tax <- simulate_safe_work(
      T = T,
      annual_labour_income = annual_labour_income,
      risk_free_return = risk_free_return,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      tax_system = "income_tax"
    )

    success_payoff_no_tax <- find_success_payoff(
      safe_incremental_wealth = safe_work_no_tax,
      failure_prob = failure_prob,
      background_wealth = background_wealth,
      risk_aversion = risk_aversion
    )

    if (is.na(success_payoff_no_tax)) {
      out[[T]] <- data.table(
        year = T,
        safe_work_no_tax = safe_work_no_tax,
        safe_work_after_tax = safe_work_after_tax,
        business_success_no_tax = NA_real_,
        business_expected_no_tax = NA_real_,
        business_ce_no_tax = NA_real_,
        business_success_after_normal_tax = NA_real_,
        business_ce_after_normal_tax = NA_real_,
        business_success_after_averaging_tax = NA_real_,
        business_ce_after_averaging_tax = NA_real_,
        normal_tax_induced_gap = NA_real_,
        averaging_tax_induced_gap = NA_real_,
        averaging_change_in_gap = NA_real_
      )
      next
    }

    failure_payoff_no_tax <- 0

    ce_business_no_tax_total <- certainty_equivalent(
      outcomes = c(
        background_wealth + failure_payoff_no_tax,
        background_wealth + success_payoff_no_tax
      ),
      probs = c(failure_prob, 1 - failure_prob),
      gamma = risk_aversion
    )

    ce_business_no_tax_incremental <- ce_business_no_tax_total - background_wealth

    expected_business_no_tax <-
      failure_prob * failure_payoff_no_tax +
      (1 - failure_prob) * success_payoff_no_tax

    # ------------------------------------------------------------
    # Normal sale-year taxation
    # ------------------------------------------------------------

    normal_success_tax <- tax_on_increment(
      base_income = sale_year_other_income,
      increment = success_payoff_no_tax,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate
    )

    business_success_after_normal_tax <-
      success_payoff_no_tax - normal_success_tax

    business_failure_after_tax <- 0

    ce_business_after_normal_tax_total <- certainty_equivalent(
      outcomes = c(
        background_wealth + business_failure_after_tax,
        background_wealth + business_success_after_normal_tax
      ),
      probs = c(failure_prob, 1 - failure_prob),
      gamma = risk_aversion
    )

    ce_business_after_normal_tax_incremental <-
      ce_business_after_normal_tax_total - background_wealth

    expected_business_after_normal_tax <-
      failure_prob * business_failure_after_tax +
      (1 - failure_prob) * business_success_after_normal_tax

    # ------------------------------------------------------------
    # Income averaging treatment for the successful sale
    # ------------------------------------------------------------

    averaged_success_tax <- tax_on_averaged_increment(
      total_increment = success_payoff_no_tax,
      T = T,
      base_income_per_year = averaging_base_income_per_year,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      apply_interest_to_averaged_tax = apply_interest_to_averaged_tax,
      risk_free_return = risk_free_return
    )

    business_success_after_averaging_tax <-
      success_payoff_no_tax - averaged_success_tax

    ce_business_after_averaging_tax_total <- certainty_equivalent(
      outcomes = c(
        background_wealth + business_failure_after_tax,
        background_wealth + business_success_after_averaging_tax
      ),
      probs = c(failure_prob, 1 - failure_prob),
      gamma = risk_aversion
    )

    ce_business_after_averaging_tax_incremental <-
      ce_business_after_averaging_tax_total - background_wealth

    expected_business_after_averaging_tax <-
      failure_prob * business_failure_after_tax +
      (1 - failure_prob) * business_success_after_averaging_tax

    out[[T]] <- data.table(
      year = T,

      safe_work_no_tax = safe_work_no_tax,
      safe_work_after_tax = safe_work_after_tax,

      business_failure_no_tax = failure_payoff_no_tax,
      business_success_no_tax = success_payoff_no_tax,
      business_expected_no_tax = expected_business_no_tax,
      business_ce_no_tax = ce_business_no_tax_incremental,

      normal_success_tax = normal_success_tax,
      business_success_after_normal_tax = business_success_after_normal_tax,
      business_expected_after_normal_tax = expected_business_after_normal_tax,
      business_ce_after_normal_tax = ce_business_after_normal_tax_incremental,
      normal_tax_induced_gap =
        ce_business_after_normal_tax_incremental - safe_work_after_tax,

      averaged_success_tax = averaged_success_tax,
      business_success_after_averaging_tax = business_success_after_averaging_tax,
      business_expected_after_averaging_tax = expected_business_after_averaging_tax,
      business_ce_after_averaging_tax = ce_business_after_averaging_tax_incremental,
      averaging_tax_induced_gap =
        ce_business_after_averaging_tax_incremental - safe_work_after_tax,

      averaging_tax_saving_success_state =
        normal_success_tax - averaged_success_tax,

      averaging_change_in_gap =
        (ce_business_after_averaging_tax_incremental - safe_work_after_tax) -
        (ce_business_after_normal_tax_incremental - safe_work_after_tax),

      required_success_multiple = success_payoff_no_tax / safe_work_no_tax
    )
  }

  rbindlist(out)
}

# ============================================================
# Run averaging comparison: flat and progressive tax systems
# ============================================================

run_averaging_model_for_tax_system <- function(use_flat_tax_input) {

  label <- tax_system_label(use_flat_tax_input, flat_tax_rate)

  hidden_avg_out <- simulate_hidden_labour_case_with_averaging(
    T_max = T_max,
    annual_labour_income = annual_labour_income,
    risk_free_return = risk_free_return,
    use_flat_tax = use_flat_tax_input,
    flat_tax_rate = flat_tax_rate,
    sale_year_other_income = sale_year_other_income,
    averaging_base_income_per_year = averaging_base_income_per_year,
    apply_interest_to_averaged_tax = apply_interest_to_averaged_tax
  )

  risky_avg_out <- simulate_risky_indifference_case_with_averaging(
    T_max = T_max,
    annual_labour_income = annual_labour_income,
    risk_free_return = risk_free_return,
    use_flat_tax = use_flat_tax_input,
    flat_tax_rate = flat_tax_rate,
    failure_prob = failure_prob,
    background_wealth = background_wealth,
    risk_aversion = risk_aversion,
    sale_year_other_income = sale_year_other_income,
    averaging_base_income_per_year = averaging_base_income_per_year,
    apply_interest_to_averaged_tax = apply_interest_to_averaged_tax
  )

  hidden_avg_out[, tax_system := label]
  risky_avg_out[, tax_system := label]

  list(
    hidden = hidden_avg_out,
    risky = risky_avg_out
  )
}

flat_avg_results <- run_averaging_model_for_tax_system(TRUE)
progressive_avg_results <- run_averaging_model_for_tax_system(FALSE)

comparison_hidden_avg <- rbindlist(
  list(
    flat_avg_results$hidden,
    progressive_avg_results$hidden
  ),
  use.names = TRUE
)

comparison_risky_avg <- rbindlist(
  list(
    flat_avg_results$risky,
    progressive_avg_results$risky
  ),
  use.names = TRUE
)

# ============================================================
# Plot A: hidden labour, normal sale tax vs income averaging
# ============================================================

hidden_avg_plot_data <- melt(
  comparison_hidden_avg,
  id.vars = c("year", "tax_system"),
  measure.vars = c(
    "normal_deferral_benefit",
    "averaging_deferral_benefit"
  ),
  variable.name = "tax_treatment",
  value.name = "deferral_benefit"
)

hidden_avg_plot_data[, tax_treatment := fifelse(
  tax_treatment == "normal_deferral_benefit",
  "Normal realisation tax",
  "Income averaging at sale"
)]

p_hidden_avg <- ggplot(
  hidden_avg_plot_data,
  aes(
    x = year,
    y = deferral_benefit,
    colour = tax_treatment
  )
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ tax_system) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Hidden labour: effect of income averaging on deferral benefit",
    subtitle = "Income averaging reduces progressive bunching but does not remove payment deferral",
    x = "Years",
    y = "Business advantage over wage/bank benchmark",
    colour = NULL
  )

print(p_hidden_avg)

# ============================================================
# Plot B: risky business, normal sale tax vs income averaging
# ============================================================

risky_avg_plot_data <- melt(
  comparison_risky_avg,
  id.vars = c("year", "tax_system"),
  measure.vars = c(
    "normal_tax_induced_gap",
    "averaging_tax_induced_gap"
  ),
  variable.name = "tax_treatment",
  value.name = "tax_induced_gap"
)

risky_avg_plot_data[, tax_treatment := fifelse(
  tax_treatment == "normal_tax_induced_gap",
  "Normal realisation tax",
  "Income averaging at sale"
)]

p_risky_avg <- ggplot(
  risky_avg_plot_data,
  aes(
    x = year,
    y = tax_induced_gap,
    colour = tax_treatment
  )
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ tax_system) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Risky business: does income averaging deal with the risk issue?",
    subtitle = "Positive values mean the tax system makes the risky business more attractive than safe work",
    x = "Years",
    y = "Risky business certainty-equivalent advantage",
    colour = NULL
  )

print(p_risky_avg)

# ============================================================
# Plot C: how much income averaging changes the risky incentive
# ============================================================

p_averaging_change <- ggplot(
  comparison_risky_avg,
  aes(
    x = year,
    y = averaging_change_in_gap,
    colour = tax_system
  )
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Change in risky-business incentive caused by income averaging",
    subtitle = "Positive values mean income averaging makes the risky business more attractive",
    x = "Years",
    y = "Change in certainty-equivalent advantage",
    colour = NULL
  )

print(p_averaging_change)

# ============================================================
# Summary table at focus_T
# ============================================================

averaging_summary_table <- comparison_risky_avg[year == focus_T, .(
  tax_system,
  year,
  safe_work_after_tax,
  business_success_no_tax,
  normal_success_tax,
  averaged_success_tax,
  averaging_tax_saving_success_state,
  business_ce_after_normal_tax,
  business_ce_after_averaging_tax,
  normal_tax_induced_gap,
  averaging_tax_induced_gap,
  averaging_change_in_gap
)]

print(averaging_summary_table)

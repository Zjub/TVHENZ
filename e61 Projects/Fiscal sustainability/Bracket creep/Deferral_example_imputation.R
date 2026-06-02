# ============================================================
# Flat/progressive tax, deferral, risky business choice,
# and Australian-style dividend imputation
# ============================================================

library(data.table)
library(ggplot2)
library(scales)

# ============================================================
# Parameters
# ============================================================

# Main tax switch:
# TRUE  = flat personal tax
# FALSE = Australian progressive resident income tax scale
use_flat_tax <- TRUE

annual_labour_income <- 100000
risk_free_return <- 0.05

T_max <- 10
focus_T <- 10

failure_prob <- 0.50
same_income_prob <- 0.40
risk_aversion <- 1
background_wealth <- 100000

# If use_flat_tax == TRUE, the flat tax rate is set equal to the
# marginal Australian tax rate applying at this reference income.
flat_rate_reference_income <- annual_labour_income

# Company tax paid by the business before franked profits are extracted.
# This is separate from the personal tax system being compared.
company_tax_rate <- 0.30

# Other personal taxable income in the terminal extraction year.
sale_year_other_income <- 0

# Australian resident individuals can generally receive refundable
# franking credit offsets. Set FALSE to examine non-refundable credits.
franking_credits_refundable <- TRUE

# Share-sale/no-dividend treatment:
# 0 = retained franking credits are not reflected in the share sale price.
# 1 = the full franking account is capitalised into the share sale price.
# Intermediate values allow partial capitalisation.
franking_credit_capitalisation_rate <- 1

plot_output_dir <- "Deferral_example_imputation_plots"
plot_dpi <- 320

imputation_treatment_footnote <- paste0(
  "Note: 'Company/imputation' assumes retained after-company-tax profits ",
  "are extracted as a fully franked terminal dividend. The separate ",
  "'Corporate share sale' treatment assumes no dividend is paid; franking ",
  "credits are not claimed directly and only affect proceeds through the ",
  "capitalisation parameter."
)

save_plot_png <- function(plot,
                          filename,
                          width = 11,
                          height = 7,
                          dpi = plot_dpi) {

  if (!dir.exists(plot_output_dir)) {
    dir.create(plot_output_dir, recursive = TRUE)
  }

  ggsave(
    filename = file.path(plot_output_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    units = "in",
    bg = "white"
  )
}

show_plot <- function(plot) {
  if (interactive()) {
    print(plot)
  }
}

# ============================================================
# Australian resident tax scale
# Excludes Medicare levy and offsets.
# ============================================================

australian_income_tax <- function(income) {
  income <- pmax(income, 0)

  pmax(pmin(income, 45000) - 18200, 0) * 0.16 +
    pmax(pmin(income, 135000) - 45000, 0) * 0.30 +
    pmax(pmin(income, 190000) - 135000, 0) * 0.37 +
    pmax(income - 190000, 0) * 0.45
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
  }

  australian_income_tax(income)
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
    paste0("Flat personal tax: ", percent(flat_tax_rate, accuracy = 1))
  } else {
    "Australian progressive personal scale"
  }
}

imputation_label <- function(company_tax_rate, franking_credits_refundable) {
  paste0(
    "Company tax = ", percent(company_tax_rate, accuracy = 1),
    ", franking credits ",
    ifelse(franking_credits_refundable, "refundable", "non-refundable")
  )
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
                                same_income_prob = 0,
                                background_wealth,
                                risk_aversion) {

  if (failure_prob < 0 || same_income_prob < 0 ||
      failure_prob + same_income_prob >= 1) {
    stop(
      "failure_prob and same_income_prob must be non-negative, ",
      "and their sum must be less than 1."
    )
  }

  target_total_wealth <- background_wealth + safe_incremental_wealth
  same_payoff <- safe_incremental_wealth
  success_prob <- 1 - failure_prob - same_income_prob

  objective <- function(success_payoff) {
    ce <- certainty_equivalent(
      outcomes = c(
        background_wealth,
        background_wealth + same_payoff,
        background_wealth + success_payoff
      ),
      probs = c(failure_prob, same_income_prob, success_prob),
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

      bank_balance <- bank_balance + interest_income +
        annual_labour_income - annual_tax
    }
  }

  bank_balance
}

# ============================================================
# Company and imputation paths
# ============================================================

simulate_company_retained_profits <- function(T,
                                              annual_business_profit,
                                              risk_free_return,
                                              company_tax_rate) {

  company_cash <- 0
  franking_account <- 0
  total_company_tax <- 0

  for (year in 1:T) {
    interest_income <- company_cash * risk_free_return
    company_taxable_income <- annual_business_profit + interest_income
    company_tax <- company_tax_rate * pmax(company_taxable_income, 0)
    after_company_tax_profit <- company_taxable_income - company_tax

    company_cash <- company_cash + after_company_tax_profit
    franking_account <- franking_account + company_tax
    total_company_tax <- total_company_tax + company_tax
  }

  data.table(
    company_cash = company_cash,
    franking_credit = franking_account,
    total_company_tax = total_company_tax
  )
}

tax_on_franked_distribution <- function(cash_dividend,
                                        franking_credit,
                                        base_income,
                                        use_flat_tax,
                                        flat_tax_rate,
                                        franking_credits_refundable = TRUE) {

  grossed_up_dividend <- cash_dividend + franking_credit

  personal_tax_before_credit <- tax_on_increment(
    base_income = base_income,
    increment = grossed_up_dividend,
    use_flat_tax = use_flat_tax,
    flat_tax_rate = flat_tax_rate
  )

  net_shareholder_tax <- personal_tax_before_credit - franking_credit

  if (!franking_credits_refundable) {
    net_shareholder_tax <- pmax(net_shareholder_tax, 0)
  }

  data.table(
    cash_dividend = cash_dividend,
    franking_credit = franking_credit,
    grossed_up_dividend = grossed_up_dividend,
    personal_tax_before_credit = personal_tax_before_credit,
    net_shareholder_tax = net_shareholder_tax,
    after_personal_tax_distribution = cash_dividend - net_shareholder_tax
  )
}

simulate_imputation_business <- function(T,
                                         annual_business_profit,
                                         risk_free_return,
                                         company_tax_rate,
                                         use_flat_tax,
                                         flat_tax_rate,
                                         sale_year_other_income = 0,
                                         franking_credits_refundable = TRUE) {

  company <- simulate_company_retained_profits(
    T = T,
    annual_business_profit = annual_business_profit,
    risk_free_return = risk_free_return,
    company_tax_rate = company_tax_rate
  )

  distribution <- tax_on_franked_distribution(
    cash_dividend = company$company_cash,
    franking_credit = company$franking_credit,
    base_income = sale_year_other_income,
    use_flat_tax = use_flat_tax,
    flat_tax_rate = flat_tax_rate,
    franking_credits_refundable = franking_credits_refundable
  )

  cbind(company, distribution)
}

# ============================================================
# Part 1: Hidden labour income as a deferred capital gain,
# with a company/imputation comparison
# ============================================================

simulate_hidden_labour_case <- function(T_max,
                                        annual_labour_income,
                                        risk_free_return,
                                        use_flat_tax,
                                        flat_tax_rate,
                                        company_tax_rate,
                                        sale_year_other_income = 0,
                                        franking_credits_refundable = TRUE) {

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

    # Pure deferral benchmark: labour contribution embedded in business
    # pre-tax, compounding pre-tax, then taxed only at sale.
    business_before_tax <- simulate_safe_work(
      T = T,
      annual_labour_income = annual_labour_income,
      risk_free_return = risk_free_return,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      tax_system = "no_tax"
    )

    capital_gain_tax_at_sale <- tax_on_increment(
      base_income = sale_year_other_income,
      increment = business_before_tax,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate
    )

    business_after_capital_gain_tax <- business_before_tax -
      capital_gain_tax_at_sale

    # Imputation path: the company pays company tax as profit arises,
    # retains after-company-tax cash, then distributes a fully franked
    # terminal dividend to the resident shareholder.
    imputation <- simulate_imputation_business(
      T = T,
      annual_business_profit = annual_labour_income,
      risk_free_return = risk_free_return,
      company_tax_rate = company_tax_rate,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      sale_year_other_income = sale_year_other_income,
      franking_credits_refundable = franking_credits_refundable
    )

    out[[T]] <- data.table(
      year = T,
      wage_bank_after_tax = wage_bank_after_tax,

      business_before_tax = business_before_tax,
      capital_gain_tax_at_sale = capital_gain_tax_at_sale,
      business_after_capital_gain_tax = business_after_capital_gain_tax,
      capital_gain_deferral_benefit =
        business_after_capital_gain_tax - wage_bank_after_tax,
      capital_gain_deferral_benefit_pct =
        business_after_capital_gain_tax / wage_bank_after_tax - 1,

      company_cash_before_distribution = imputation$company_cash,
      company_tax_paid = imputation$total_company_tax,
      franking_credit = imputation$franking_credit,
      grossed_up_dividend = imputation$grossed_up_dividend,
      personal_tax_before_franking_credit =
        imputation$personal_tax_before_credit,
      net_shareholder_topup_tax = imputation$net_shareholder_tax,
      business_after_imputation = imputation$after_personal_tax_distribution,
      imputation_deferral_benefit =
        imputation$after_personal_tax_distribution - wage_bank_after_tax,
      imputation_deferral_benefit_pct =
        imputation$after_personal_tax_distribution / wage_bank_after_tax - 1,

      imputation_change_in_benefit =
        (imputation$after_personal_tax_distribution - wage_bank_after_tax) -
        (business_after_capital_gain_tax - wage_bank_after_tax)
    )
  }

  rbindlist(out)
}

# ============================================================
# Part 2: Risky business calibrated to no-tax indifference,
# with pure CGT and imputation after-tax treatments
# ============================================================

simulate_risky_indifference_case <- function(T_max,
                                             annual_labour_income,
                                             risk_free_return,
                                             use_flat_tax,
                                             flat_tax_rate,
                                             company_tax_rate,
                                             failure_prob,
                                             same_income_prob = 0,
                                             background_wealth,
                                             risk_aversion,
                                             sale_year_other_income = 0,
                                             franking_credits_refundable = TRUE) {

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
      same_income_prob = same_income_prob,
      background_wealth = background_wealth,
      risk_aversion = risk_aversion
    )

    success_prob <- 1 - failure_prob - same_income_prob

    if (is.na(success_payoff_no_tax)) {
      out[[T]] <- data.table(
        year = T,
        safe_work_no_tax = safe_work_no_tax,
        safe_work_after_tax = safe_work_after_tax,
        business_failure_no_tax = 0,
        business_same_no_tax = safe_work_no_tax,
        business_success_no_tax = NA_real_,
        business_expected_no_tax = NA_real_,
        business_ce_no_tax = NA_real_,
        business_same_after_capital_gain_tax = NA_real_,
        business_success_after_capital_gain_tax = NA_real_,
        business_ce_after_capital_gain_tax = NA_real_,
        company_cash_same_state = NA_real_,
        company_cash_success_state = NA_real_,
        franking_credit_same_state = NA_real_,
        business_same_after_imputation = NA_real_,
        business_success_after_imputation = NA_real_,
        business_ce_after_imputation = NA_real_,
        capital_gain_tax_induced_gap = NA_real_,
        imputation_tax_induced_gap = NA_real_,
        required_success_multiple = NA_real_
      )
      next
    }

    failure_payoff_no_tax <- 0
    same_payoff_no_tax <- safe_work_no_tax

    ce_business_no_tax_total <- certainty_equivalent(
      outcomes = c(
        background_wealth + failure_payoff_no_tax,
        background_wealth + same_payoff_no_tax,
        background_wealth + success_payoff_no_tax
      ),
      probs = c(failure_prob, same_income_prob, success_prob),
      gamma = risk_aversion
    )

    ce_business_no_tax_incremental <- ce_business_no_tax_total -
      background_wealth

    expected_business_no_tax <-
      failure_prob * failure_payoff_no_tax +
      same_income_prob * same_payoff_no_tax +
      success_prob * success_payoff_no_tax

    # Pure capital-gain treatment: no tax until success-state sale.
    same_capital_gain_tax <- tax_on_increment(
      base_income = sale_year_other_income,
      increment = same_payoff_no_tax,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate
    )

    success_capital_gain_tax <- tax_on_increment(
      base_income = sale_year_other_income,
      increment = success_payoff_no_tax,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate
    )

    business_success_after_capital_gain_tax <- success_payoff_no_tax -
      success_capital_gain_tax

    business_failure_after_tax <- 0
    business_same_after_capital_gain_tax <- same_payoff_no_tax -
      same_capital_gain_tax

    ce_business_after_capital_gain_tax_total <- certainty_equivalent(
      outcomes = c(
        background_wealth + business_failure_after_tax,
        background_wealth + business_same_after_capital_gain_tax,
        background_wealth + business_success_after_capital_gain_tax
      ),
      probs = c(failure_prob, same_income_prob, success_prob),
      gamma = risk_aversion
    )

    business_ce_after_capital_gain_tax <-
      ce_business_after_capital_gain_tax_total - background_wealth

    expected_business_after_capital_gain_tax <-
      failure_prob * business_failure_after_tax +
      same_income_prob * business_same_after_capital_gain_tax +
      success_prob * business_success_after_capital_gain_tax

    # Imputation treatment. Scale annual business profits so that, absent
    # tax, the success-state terminal payoff equals success_payoff_no_tax.
    success_multiple <- success_payoff_no_tax / safe_work_no_tax

    same_imputation <- simulate_imputation_business(
      T = T,
      annual_business_profit = annual_labour_income,
      risk_free_return = risk_free_return,
      company_tax_rate = company_tax_rate,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      sale_year_other_income = sale_year_other_income,
      franking_credits_refundable = franking_credits_refundable
    )

    success_imputation <- simulate_imputation_business(
      T = T,
      annual_business_profit = annual_labour_income * success_multiple,
      risk_free_return = risk_free_return,
      company_tax_rate = company_tax_rate,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      sale_year_other_income = sale_year_other_income,
      franking_credits_refundable = franking_credits_refundable
    )

    business_same_after_imputation <-
      same_imputation$after_personal_tax_distribution

    business_success_after_imputation <-
      success_imputation$after_personal_tax_distribution

    ce_business_after_imputation_total <- certainty_equivalent(
      outcomes = c(
        background_wealth + business_failure_after_tax,
        background_wealth + business_same_after_imputation,
        background_wealth + business_success_after_imputation
      ),
      probs = c(failure_prob, same_income_prob, success_prob),
      gamma = risk_aversion
    )

    business_ce_after_imputation <-
      ce_business_after_imputation_total - background_wealth

    expected_business_after_imputation <-
      failure_prob * business_failure_after_tax +
      same_income_prob * business_same_after_imputation +
      success_prob * business_success_after_imputation

    out[[T]] <- data.table(
      year = T,

      safe_work_no_tax = safe_work_no_tax,
      safe_work_after_tax = safe_work_after_tax,
      failure_prob = failure_prob,
      same_income_prob = same_income_prob,
      success_prob = success_prob,

      business_failure_no_tax = failure_payoff_no_tax,
      business_same_no_tax = same_payoff_no_tax,
      business_success_no_tax = success_payoff_no_tax,
      business_expected_no_tax = expected_business_no_tax,
      business_ce_no_tax = ce_business_no_tax_incremental,

      same_capital_gain_tax = same_capital_gain_tax,
      success_capital_gain_tax = success_capital_gain_tax,
      business_same_after_capital_gain_tax =
        business_same_after_capital_gain_tax,
      business_success_after_capital_gain_tax =
        business_success_after_capital_gain_tax,
      business_expected_after_capital_gain_tax =
        expected_business_after_capital_gain_tax,
      business_ce_after_capital_gain_tax =
        business_ce_after_capital_gain_tax,
      capital_gain_tax_induced_gap =
        business_ce_after_capital_gain_tax - safe_work_after_tax,

      company_tax_paid_same_state = same_imputation$total_company_tax,
      company_cash_same_state = same_imputation$company_cash,
      franking_credit_same_state = same_imputation$franking_credit,
      grossed_up_dividend_same_state =
        same_imputation$grossed_up_dividend,
      net_shareholder_topup_tax_same_state =
        same_imputation$net_shareholder_tax,
      company_tax_paid_success_state = success_imputation$total_company_tax,
      company_cash_success_state = success_imputation$company_cash,
      franking_credit_success_state = success_imputation$franking_credit,
      grossed_up_dividend_success_state =
        success_imputation$grossed_up_dividend,
      net_shareholder_topup_tax_success_state =
        success_imputation$net_shareholder_tax,
      business_same_after_imputation =
        business_same_after_imputation,
      business_success_after_imputation =
        business_success_after_imputation,
      business_expected_after_imputation =
        expected_business_after_imputation,
      business_ce_after_imputation = business_ce_after_imputation,
      imputation_tax_induced_gap =
        business_ce_after_imputation - safe_work_after_tax,

      imputation_change_in_gap =
        (business_ce_after_imputation - safe_work_after_tax) -
        (business_ce_after_capital_gain_tax - safe_work_after_tax),

      required_success_multiple = success_multiple
    )
  }

  rbindlist(out)
}

# ============================================================
# Run model under selected tax system
# ============================================================

selected_tax_label <- tax_system_label(use_flat_tax, flat_tax_rate)
selected_imputation_label <- imputation_label(
  company_tax_rate,
  franking_credits_refundable
)

hidden <- simulate_hidden_labour_case(
  T_max = T_max,
  annual_labour_income = annual_labour_income,
  risk_free_return = risk_free_return,
  use_flat_tax = use_flat_tax,
  flat_tax_rate = flat_tax_rate,
  company_tax_rate = company_tax_rate,
  sale_year_other_income = sale_year_other_income,
  franking_credits_refundable = franking_credits_refundable
)

risky <- simulate_risky_indifference_case(
  T_max = T_max,
  annual_labour_income = annual_labour_income,
  risk_free_return = risk_free_return,
  use_flat_tax = use_flat_tax,
  flat_tax_rate = flat_tax_rate,
  company_tax_rate = company_tax_rate,
  failure_prob = failure_prob,
  same_income_prob = same_income_prob,
  background_wealth = background_wealth,
  risk_aversion = risk_aversion,
  sale_year_other_income = sale_year_other_income,
  franking_credits_refundable = franking_credits_refundable
)

print(hidden[year == focus_T])
print(risky[year == focus_T])

# ============================================================
# Plots for selected tax system
# ============================================================

hidden_plot_data <- melt(
  hidden,
  id.vars = "year",
  measure.vars = c(
    "wage_bank_after_tax",
    "business_after_capital_gain_tax",
    "business_after_imputation"
  ),
  variable.name = "scenario",
  value.name = "after_tax_wealth"
)

hidden_plot_data[, scenario := fifelse(
  scenario == "wage_bank_after_tax",
  "Safe work: labour and interest taxed annually",
  fifelse(
    scenario == "business_after_capital_gain_tax",
    "Pure deferred capital gain: tax only at sale",
    "Company/imputation: company tax annually, franked dividend at end"
  )
)]

p_hidden <- ggplot(
  hidden_plot_data,
  aes(x = year, y = after_tax_wealth, colour = scenario)
) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Case 1: Hidden labour with and without imputation",
    subtitle = paste(selected_tax_label, selected_imputation_label, sep = " | "),
    x = "Years",
    y = "After-tax wealth",
    colour = NULL,
    caption = imputation_treatment_footnote
  )

show_plot(p_hidden)
save_plot_png(p_hidden, "01_hidden_labour_after_tax_wealth.png")

hidden_gap_plot_data <- melt(
  hidden,
  id.vars = "year",
  measure.vars = c(
    "capital_gain_deferral_benefit",
    "imputation_deferral_benefit"
  ),
  variable.name = "treatment",
  value.name = "deferral_benefit"
)

hidden_gap_plot_data[, treatment := fifelse(
  treatment == "capital_gain_deferral_benefit",
  "Pure deferred capital gain",
  "Company/imputation"
)]

p_hidden_gap <- ggplot(
  hidden_gap_plot_data,
  aes(x = year, y = deferral_benefit, colour = treatment)
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Deferral benefit after including franking credits",
    subtitle = "Business after-tax wealth minus wage/bank wealth",
    x = "Years",
    y = "Deferral benefit",
    colour = NULL,
    caption = imputation_treatment_footnote
  )

show_plot(p_hidden_gap)
save_plot_png(p_hidden_gap, "02_hidden_labour_deferral_benefit.png")

pretax_plot_data <- melt(
  risky,
  id.vars = "year",
  measure.vars = c(
    "safe_work_no_tax",
    "business_ce_no_tax",
    "business_expected_no_tax",
    "business_same_no_tax",
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
        scenario == "business_same_no_tax",
        "Risky business: same-as-safe payoff, no tax",
        fifelse(
        scenario == "business_success_no_tax",
        "Risky business: success payoff, no tax",
        "Risky business: failure payoff, no tax"
        )
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
      ", same-as-safe probability = ", percent(same_income_prob),
      ", success probability = ",
      percent(1 - failure_prob - same_income_prob),
      ", risk aversion = ", risk_aversion,
      ", background wealth = ", dollar(background_wealth)
    ),
    x = "Years",
    y = "Incremental wealth",
    colour = NULL
  )

show_plot(p_pretax)
save_plot_png(p_pretax, "03_risky_business_no_tax_calibration.png")

aftertax_plot_data <- melt(
  risky,
  id.vars = "year",
  measure.vars = c(
    "safe_work_after_tax",
    "business_ce_after_capital_gain_tax",
    "business_expected_after_capital_gain_tax",
    "business_ce_after_imputation",
    "business_expected_after_imputation"
  ),
  variable.name = "scenario",
  value.name = "wealth"
)

aftertax_plot_data[, scenario := fifelse(
  scenario == "safe_work_after_tax",
  "Safe work: taxed annually",
  fifelse(
    scenario == "business_ce_after_capital_gain_tax",
    "Risky business CE: pure deferred capital gain",
    fifelse(
      scenario == "business_expected_after_capital_gain_tax",
      "Risky business expected value: pure deferred capital gain",
      fifelse(
        scenario == "business_ce_after_imputation",
        "Risky business CE: company/imputation",
        "Risky business expected value: company/imputation"
      )
    )
  )
)]

p_aftertax <- ggplot(
  aftertax_plot_data,
  aes(x = year, y = wealth, colour = scenario)
) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "How tax treatment changes the risky-business choice",
    subtitle = paste(selected_tax_label, selected_imputation_label, sep = " | "),
    x = "Years",
    y = "After-tax incremental wealth",
    colour = NULL,
    caption = imputation_treatment_footnote
  )

show_plot(p_aftertax)
save_plot_png(p_aftertax, "04_risky_business_after_tax_comparison.png")

tax_gap_plot_data <- melt(
  risky,
  id.vars = "year",
  measure.vars = c(
    "capital_gain_tax_induced_gap",
    "imputation_tax_induced_gap"
  ),
  variable.name = "treatment",
  value.name = "tax_induced_gap"
)

tax_gap_plot_data[, treatment := fifelse(
  treatment == "capital_gain_tax_induced_gap",
  "Pure deferred capital gain",
  "Company/imputation"
)]

p_tax_gap <- ggplot(
  tax_gap_plot_data,
  aes(x = year, y = tax_induced_gap, colour = treatment)
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Tax-induced incentive toward the risky business",
    subtitle = "Positive values mean tax makes the risky business more attractive than safe work",
    x = "Years",
    y = "Risky business certainty equivalent minus safe work wealth",
    colour = NULL,
    caption = imputation_treatment_footnote
  )

show_plot(p_tax_gap)
save_plot_png(p_tax_gap, "05_risky_business_tax_induced_gap.png")

# ============================================================
# Summary table for selected tax system
# ============================================================

summary_table <- merge(
  hidden[year == focus_T, .(
    year,
    wage_bank_after_tax,
    business_after_capital_gain_tax,
    business_after_imputation,
    capital_gain_deferral_benefit,
    imputation_deferral_benefit,
    imputation_change_in_benefit,
    company_tax_paid,
    franking_credit,
    net_shareholder_topup_tax
  )],
  risky[year == focus_T, .(
    year,
    safe_work_after_tax,
    failure_prob,
    same_income_prob,
    success_prob,
    risky_business_same_payoff_no_tax = business_same_no_tax,
    risky_business_success_payoff_no_tax = business_success_no_tax,
    risky_business_ce_capital_gain = business_ce_after_capital_gain_tax,
    risky_business_ce_imputation = business_ce_after_imputation,
    capital_gain_tax_induced_gap,
    imputation_tax_induced_gap,
    imputation_change_in_gap
  )],
  by = "year"
)

summary_table[, tax_system := selected_tax_label]
summary_table[, imputation_system := selected_imputation_label]

print(summary_table)
cat("\n", imputation_treatment_footnote, "\n", sep = "")

# ============================================================
# Compare flat vs progressive personal tax scales
# ============================================================

run_model_for_tax_system <- function(use_flat_tax_input) {

  label <- tax_system_label(use_flat_tax_input, flat_tax_rate)

  hidden_out <- simulate_hidden_labour_case(
    T_max = T_max,
    annual_labour_income = annual_labour_income,
    risk_free_return = risk_free_return,
    use_flat_tax = use_flat_tax_input,
    flat_tax_rate = flat_tax_rate,
    company_tax_rate = company_tax_rate,
    sale_year_other_income = sale_year_other_income,
    franking_credits_refundable = franking_credits_refundable
  )

  risky_out <- simulate_risky_indifference_case(
    T_max = T_max,
    annual_labour_income = annual_labour_income,
    risk_free_return = risk_free_return,
    use_flat_tax = use_flat_tax_input,
    flat_tax_rate = flat_tax_rate,
    company_tax_rate = company_tax_rate,
    failure_prob = failure_prob,
    same_income_prob = same_income_prob,
    background_wealth = background_wealth,
    risk_aversion = risk_aversion,
    sale_year_other_income = sale_year_other_income,
    franking_credits_refundable = franking_credits_refundable
  )

  hidden_out[, tax_system := label]
  risky_out[, tax_system := label]

  list(hidden = hidden_out, risky = risky_out)
}

flat_results <- run_model_for_tax_system(TRUE)
progressive_results <- run_model_for_tax_system(FALSE)

comparison_hidden <- rbindlist(
  list(flat_results$hidden, progressive_results$hidden),
  use.names = TRUE
)

comparison_risky <- rbindlist(
  list(flat_results$risky, progressive_results$risky),
  use.names = TRUE
)

comparison_hidden_plot_data <- melt(
  comparison_hidden,
  id.vars = c("year", "tax_system"),
  measure.vars = c(
    "capital_gain_deferral_benefit",
    "imputation_deferral_benefit"
  ),
  variable.name = "treatment",
  value.name = "deferral_benefit"
)

comparison_hidden_plot_data[, treatment := fifelse(
  treatment == "capital_gain_deferral_benefit",
  "Pure deferred capital gain",
  "Company/imputation"
)]

p_compare_hidden <- ggplot(
  comparison_hidden_plot_data,
  aes(x = year, y = deferral_benefit, colour = treatment)
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ tax_system) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Hidden labour: deferral benefit under flat vs progressive tax",
    subtitle = selected_imputation_label,
    x = "Years",
    y = "Deferral benefit",
    colour = NULL,
    caption = imputation_treatment_footnote
  )

show_plot(p_compare_hidden)
save_plot_png(p_compare_hidden, "06_hidden_labour_flat_vs_progressive.png")

comparison_risky_plot_data <- melt(
  comparison_risky,
  id.vars = c("year", "tax_system"),
  measure.vars = c(
    "capital_gain_tax_induced_gap",
    "imputation_tax_induced_gap"
  ),
  variable.name = "treatment",
  value.name = "tax_induced_gap"
)

comparison_risky_plot_data[, treatment := fifelse(
  treatment == "capital_gain_tax_induced_gap",
  "Pure deferred capital gain",
  "Company/imputation"
)]

p_compare_risky <- ggplot(
  comparison_risky_plot_data,
  aes(x = year, y = tax_induced_gap, colour = treatment)
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ tax_system) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Risky business: incentive under flat vs progressive tax",
    subtitle = selected_imputation_label,
    x = "Years",
    y = "Risky business certainty-equivalent advantage",
    colour = NULL,
    caption = imputation_treatment_footnote
  )

show_plot(p_compare_risky)
save_plot_png(p_compare_risky, "07_risky_business_flat_vs_progressive.png")

comparison_table <- merge(
  comparison_hidden[year == focus_T, .(
    tax_system,
    hidden_labour_capital_gain_benefit = capital_gain_deferral_benefit,
    hidden_labour_imputation_benefit = imputation_deferral_benefit,
    imputation_change_in_hidden_benefit = imputation_change_in_benefit,
    company_tax_paid,
    franking_credit,
    net_shareholder_topup_tax
  )],
  comparison_risky[year == focus_T, .(
    tax_system,
    safe_work_after_tax,
    failure_prob,
    same_income_prob,
    success_prob,
    risky_business_same_payoff_no_tax = business_same_no_tax,
    risky_business_success_payoff_no_tax = business_success_no_tax,
    risky_business_ce_capital_gain = business_ce_after_capital_gain_tax,
    risky_business_ce_imputation = business_ce_after_imputation,
    risky_business_capital_gain_gap = capital_gain_tax_induced_gap,
    risky_business_imputation_gap = imputation_tax_induced_gap,
    imputation_change_in_risky_gap = imputation_change_in_gap
  )],
  by = "tax_system"
)

print(comparison_table)
cat("\n", imputation_treatment_footnote, "\n", sep = "")

# ============================================================
# Extra section: income averaging for realised capital gains
# ============================================================

# Normal realisation:
#   The full capital gain is taxed in the sale year.
#
# Income averaging:
#   The capital gain is divided equally across the prior T years.
#   Tax is calculated as if gain/T was earned in each year.
#   The total tax is assessed at the end.
#
# Company/imputation:
#   Company tax is paid as profits arise and franking credits are
#   attached to a terminal dividend. This is not capital-gain averaging,
#   but is included here as a third comparator.
#
# Corporate share sale:
#   Company tax is paid as profits arise, no dividend is paid, and the
#   shareholder realises a capital gain on the shares. Income averaging
#   can apply to this capital gain. Franking credits are not directly
#   claimed; any value reflected in the sale price is controlled by
#   franking_credit_capitalisation_rate.

apply_interest_to_averaged_tax <- FALSE
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

  years_until_sale <- (T - 1):0

  sum(annual_incremental_tax * (1 + risk_free_return)^years_until_sale)
}

simulate_hidden_labour_case_with_averaging <- function(T_max,
                                                       annual_labour_income,
                                                       risk_free_return,
                                                       use_flat_tax,
                                                       flat_tax_rate,
                                                       company_tax_rate,
                                                       sale_year_other_income = 0,
                                                       averaging_base_income_per_year = 0,
                                                       apply_interest_to_averaged_tax = FALSE,
                                                       franking_credits_refundable = TRUE,
                                                       franking_credit_capitalisation_rate = 0) {

  out <- vector("list", T_max)

  for (T in 1:T_max) {

    base_case <- simulate_hidden_labour_case(
      T_max = T,
      annual_labour_income = annual_labour_income,
      risk_free_return = risk_free_return,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      company_tax_rate = company_tax_rate,
      sale_year_other_income = sale_year_other_income,
      franking_credits_refundable = franking_credits_refundable
    )[year == T]

    averaged_tax_at_sale <- tax_on_averaged_increment(
      total_increment = base_case$business_before_tax,
      T = T,
      base_income_per_year = averaging_base_income_per_year,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      apply_interest_to_averaged_tax = apply_interest_to_averaged_tax,
      risk_free_return = risk_free_return
    )

    business_after_averaging_tax <- base_case$business_before_tax -
      averaged_tax_at_sale

    corporate_share_sale_proceeds <-
      base_case$company_cash_before_distribution +
      franking_credit_capitalisation_rate * base_case$franking_credit

    corporate_share_sale_tax <- tax_on_increment(
      base_income = sale_year_other_income,
      increment = corporate_share_sale_proceeds,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate
    )

    business_after_corporate_share_sale <-
      corporate_share_sale_proceeds - corporate_share_sale_tax

    corporate_share_sale_averaged_tax <- tax_on_averaged_increment(
      total_increment = corporate_share_sale_proceeds,
      T = T,
      base_income_per_year = averaging_base_income_per_year,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      apply_interest_to_averaged_tax = apply_interest_to_averaged_tax,
      risk_free_return = risk_free_return
    )

    business_after_corporate_share_sale_averaging <-
      corporate_share_sale_proceeds - corporate_share_sale_averaged_tax

    out[[T]] <- data.table(
      year = T,
      wage_bank_after_tax = base_case$wage_bank_after_tax,
      business_before_tax = base_case$business_before_tax,

      normal_tax_at_sale = base_case$capital_gain_tax_at_sale,
      business_after_normal_tax =
        base_case$business_after_capital_gain_tax,
      normal_deferral_benefit =
        base_case$capital_gain_deferral_benefit,

      averaged_tax_at_sale = averaged_tax_at_sale,
      business_after_averaging_tax = business_after_averaging_tax,
      averaging_deferral_benefit =
        business_after_averaging_tax - base_case$wage_bank_after_tax,

      company_tax_paid = base_case$company_tax_paid,
      franking_credit = base_case$franking_credit,
      franking_credit_capitalised =
        franking_credit_capitalisation_rate * base_case$franking_credit,
      net_shareholder_topup_tax = base_case$net_shareholder_topup_tax,
      business_after_imputation = base_case$business_after_imputation,
      imputation_deferral_benefit =
        base_case$imputation_deferral_benefit,

      corporate_share_sale_proceeds = corporate_share_sale_proceeds,
      corporate_share_sale_tax = corporate_share_sale_tax,
      business_after_corporate_share_sale =
        business_after_corporate_share_sale,
      corporate_share_sale_deferral_benefit =
        business_after_corporate_share_sale - base_case$wage_bank_after_tax,

      corporate_share_sale_averaged_tax =
        corporate_share_sale_averaged_tax,
      business_after_corporate_share_sale_averaging =
        business_after_corporate_share_sale_averaging,
      corporate_share_sale_averaging_deferral_benefit =
        business_after_corporate_share_sale_averaging -
        base_case$wage_bank_after_tax
    )
  }

  rbindlist(out)
}

simulate_risky_indifference_case_with_averaging <- function(T_max,
                                                            annual_labour_income,
                                                            risk_free_return,
                                                            use_flat_tax,
                                                            flat_tax_rate,
                                                            company_tax_rate,
                                                            failure_prob,
                                                            same_income_prob = 0,
                                                            background_wealth,
                                                            risk_aversion,
                                                            sale_year_other_income = 0,
                                                            averaging_base_income_per_year = 0,
                                                            apply_interest_to_averaged_tax = FALSE,
                                                            franking_credits_refundable = TRUE,
                                                            franking_credit_capitalisation_rate = 0) {

  out <- vector("list", T_max)

  for (T in 1:T_max) {

    base_case <- simulate_risky_indifference_case(
      T_max = T,
      annual_labour_income = annual_labour_income,
      risk_free_return = risk_free_return,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      company_tax_rate = company_tax_rate,
      failure_prob = failure_prob,
      same_income_prob = same_income_prob,
      background_wealth = background_wealth,
      risk_aversion = risk_aversion,
      sale_year_other_income = sale_year_other_income,
      franking_credits_refundable = franking_credits_refundable
    )[year == T]

    if (is.na(base_case$business_success_no_tax)) {
      out[[T]] <- data.table(
        year = T,
        safe_work_after_tax = base_case$safe_work_after_tax,
        normal_tax_induced_gap = NA_real_,
        averaging_tax_induced_gap = NA_real_,
        imputation_tax_induced_gap = NA_real_,
        corporate_share_sale_tax_induced_gap = NA_real_,
        corporate_share_sale_averaging_tax_induced_gap = NA_real_
      )
      next
    }

    success_prob <- 1 - failure_prob - same_income_prob

    averaged_same_tax <- tax_on_averaged_increment(
      total_increment = base_case$business_same_no_tax,
      T = T,
      base_income_per_year = averaging_base_income_per_year,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      apply_interest_to_averaged_tax = apply_interest_to_averaged_tax,
      risk_free_return = risk_free_return
    )

    averaged_success_tax <- tax_on_averaged_increment(
      total_increment = base_case$business_success_no_tax,
      T = T,
      base_income_per_year = averaging_base_income_per_year,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      apply_interest_to_averaged_tax = apply_interest_to_averaged_tax,
      risk_free_return = risk_free_return
    )

    business_success_after_averaging_tax <-
      base_case$business_success_no_tax - averaged_success_tax

    business_failure_after_tax <- 0
    business_same_after_averaging_tax <-
      base_case$business_same_no_tax - averaged_same_tax

    ce_business_after_averaging_tax_total <- certainty_equivalent(
      outcomes = c(
        background_wealth + business_failure_after_tax,
        background_wealth + business_same_after_averaging_tax,
        background_wealth + business_success_after_averaging_tax
      ),
      probs = c(failure_prob, same_income_prob, success_prob),
      gamma = risk_aversion
    )

    business_ce_after_averaging_tax <-
      ce_business_after_averaging_tax_total - background_wealth

    corporate_share_sale_success_proceeds <-
      base_case$company_cash_success_state +
      franking_credit_capitalisation_rate *
        base_case$franking_credit_success_state

    corporate_share_sale_same_proceeds <-
      base_case$company_cash_same_state +
      franking_credit_capitalisation_rate *
        base_case$franking_credit_same_state

    corporate_share_sale_same_tax <- tax_on_increment(
      base_income = sale_year_other_income,
      increment = corporate_share_sale_same_proceeds,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate
    )

    corporate_share_sale_success_tax <- tax_on_increment(
      base_income = sale_year_other_income,
      increment = corporate_share_sale_success_proceeds,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate
    )

    business_success_after_corporate_share_sale <-
      corporate_share_sale_success_proceeds - corporate_share_sale_success_tax

    business_same_after_corporate_share_sale <-
      corporate_share_sale_same_proceeds - corporate_share_sale_same_tax

    corporate_share_sale_averaged_same_tax <- tax_on_averaged_increment(
      total_increment = corporate_share_sale_same_proceeds,
      T = T,
      base_income_per_year = averaging_base_income_per_year,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      apply_interest_to_averaged_tax = apply_interest_to_averaged_tax,
      risk_free_return = risk_free_return
    )

    corporate_share_sale_averaged_success_tax <- tax_on_averaged_increment(
      total_increment = corporate_share_sale_success_proceeds,
      T = T,
      base_income_per_year = averaging_base_income_per_year,
      use_flat_tax = use_flat_tax,
      flat_tax_rate = flat_tax_rate,
      apply_interest_to_averaged_tax = apply_interest_to_averaged_tax,
      risk_free_return = risk_free_return
    )

    business_success_after_corporate_share_sale_averaging <-
      corporate_share_sale_success_proceeds -
      corporate_share_sale_averaged_success_tax

    business_same_after_corporate_share_sale_averaging <-
      corporate_share_sale_same_proceeds -
      corporate_share_sale_averaged_same_tax

    ce_business_after_corporate_share_sale_total <- certainty_equivalent(
      outcomes = c(
        background_wealth + business_failure_after_tax,
        background_wealth + business_same_after_corporate_share_sale,
        background_wealth + business_success_after_corporate_share_sale
      ),
      probs = c(failure_prob, same_income_prob, success_prob),
      gamma = risk_aversion
    )

    business_ce_after_corporate_share_sale <-
      ce_business_after_corporate_share_sale_total - background_wealth

    ce_business_after_corporate_share_sale_averaging_total <-
      certainty_equivalent(
        outcomes = c(
          background_wealth + business_failure_after_tax,
          background_wealth +
            business_same_after_corporate_share_sale_averaging,
          background_wealth +
            business_success_after_corporate_share_sale_averaging
        ),
        probs = c(failure_prob, same_income_prob, success_prob),
        gamma = risk_aversion
      )

    business_ce_after_corporate_share_sale_averaging <-
      ce_business_after_corporate_share_sale_averaging_total -
      background_wealth

    out[[T]] <- data.table(
      year = T,
      safe_work_after_tax = base_case$safe_work_after_tax,
      failure_prob = failure_prob,
      same_income_prob = same_income_prob,
      success_prob = success_prob,
      business_same_no_tax = base_case$business_same_no_tax,
      business_success_no_tax = base_case$business_success_no_tax,

      normal_same_tax = base_case$same_capital_gain_tax,
      normal_success_tax = base_case$success_capital_gain_tax,
      business_ce_after_normal_tax =
        base_case$business_ce_after_capital_gain_tax,
      normal_tax_induced_gap =
        base_case$capital_gain_tax_induced_gap,

      averaged_same_tax = averaged_same_tax,
      averaged_success_tax = averaged_success_tax,
      business_ce_after_averaging_tax =
        business_ce_after_averaging_tax,
      averaging_tax_induced_gap =
        business_ce_after_averaging_tax - base_case$safe_work_after_tax,

      company_tax_paid_success_state =
        base_case$company_tax_paid_success_state,
      company_cash_same_state =
        base_case$company_cash_same_state,
      company_cash_success_state =
        base_case$company_cash_success_state,
      franking_credit_same_state =
        base_case$franking_credit_same_state,
      franking_credit_success_state =
        base_case$franking_credit_success_state,
      net_shareholder_topup_tax_same_state =
        base_case$net_shareholder_topup_tax_same_state,
      net_shareholder_topup_tax_success_state =
        base_case$net_shareholder_topup_tax_success_state,
      business_ce_after_imputation =
        base_case$business_ce_after_imputation,
      imputation_tax_induced_gap =
        base_case$imputation_tax_induced_gap,

      imputation_change_in_gap =
        base_case$imputation_tax_induced_gap -
        base_case$capital_gain_tax_induced_gap,

      franking_credit_capitalised_success_state =
        franking_credit_capitalisation_rate *
        base_case$franking_credit_success_state,
      franking_credit_capitalised_same_state =
        franking_credit_capitalisation_rate *
        base_case$franking_credit_same_state,
      corporate_share_sale_same_proceeds =
        corporate_share_sale_same_proceeds,
      corporate_share_sale_success_proceeds =
        corporate_share_sale_success_proceeds,
      corporate_share_sale_same_tax =
        corporate_share_sale_same_tax,
      corporate_share_sale_success_tax =
        corporate_share_sale_success_tax,
      business_ce_after_corporate_share_sale =
        business_ce_after_corporate_share_sale,
      corporate_share_sale_tax_induced_gap =
        business_ce_after_corporate_share_sale -
        base_case$safe_work_after_tax,

      corporate_share_sale_averaged_same_tax =
        corporate_share_sale_averaged_same_tax,
      corporate_share_sale_averaged_success_tax =
        corporate_share_sale_averaged_success_tax,
      business_ce_after_corporate_share_sale_averaging =
        business_ce_after_corporate_share_sale_averaging,
      corporate_share_sale_averaging_tax_induced_gap =
        business_ce_after_corporate_share_sale_averaging -
        base_case$safe_work_after_tax
    )
  }

  rbindlist(out)
}

run_averaging_model_for_tax_system <- function(use_flat_tax_input) {

  label <- tax_system_label(use_flat_tax_input, flat_tax_rate)

  hidden_avg_out <- simulate_hidden_labour_case_with_averaging(
    T_max = T_max,
    annual_labour_income = annual_labour_income,
    risk_free_return = risk_free_return,
    use_flat_tax = use_flat_tax_input,
    flat_tax_rate = flat_tax_rate,
    company_tax_rate = company_tax_rate,
    sale_year_other_income = sale_year_other_income,
    averaging_base_income_per_year = averaging_base_income_per_year,
    apply_interest_to_averaged_tax = apply_interest_to_averaged_tax,
    franking_credits_refundable = franking_credits_refundable,
    franking_credit_capitalisation_rate =
      franking_credit_capitalisation_rate
  )

  risky_avg_out <- simulate_risky_indifference_case_with_averaging(
    T_max = T_max,
    annual_labour_income = annual_labour_income,
    risk_free_return = risk_free_return,
    use_flat_tax = use_flat_tax_input,
    flat_tax_rate = flat_tax_rate,
    company_tax_rate = company_tax_rate,
    failure_prob = failure_prob,
    same_income_prob = same_income_prob,
    background_wealth = background_wealth,
    risk_aversion = risk_aversion,
    sale_year_other_income = sale_year_other_income,
    averaging_base_income_per_year = averaging_base_income_per_year,
    apply_interest_to_averaged_tax = apply_interest_to_averaged_tax,
    franking_credits_refundable = franking_credits_refundable,
    franking_credit_capitalisation_rate =
      franking_credit_capitalisation_rate
  )

  hidden_avg_out[, tax_system := label]
  risky_avg_out[, tax_system := label]

  list(hidden = hidden_avg_out, risky = risky_avg_out)
}

flat_avg_results <- run_averaging_model_for_tax_system(TRUE)
progressive_avg_results <- run_averaging_model_for_tax_system(FALSE)

comparison_hidden_avg <- rbindlist(
  list(flat_avg_results$hidden, progressive_avg_results$hidden),
  use.names = TRUE
)

comparison_risky_avg <- rbindlist(
  list(flat_avg_results$risky, progressive_avg_results$risky),
  use.names = TRUE
)

hidden_avg_plot_data <- melt(
  comparison_hidden_avg,
  id.vars = c("year", "tax_system"),
  measure.vars = c(
    "normal_deferral_benefit",
    "averaging_deferral_benefit",
    "imputation_deferral_benefit",
    "corporate_share_sale_deferral_benefit",
    "corporate_share_sale_averaging_deferral_benefit"
  ),
  variable.name = "tax_treatment",
  value.name = "deferral_benefit"
)

hidden_avg_plot_data[, tax_treatment := fifelse(
  tax_treatment == "normal_deferral_benefit",
  "Normal capital-gain realisation",
  fifelse(
    tax_treatment == "averaging_deferral_benefit",
    "Capital-gain income averaging",
    fifelse(
      tax_treatment == "imputation_deferral_benefit",
      "Company/imputation: franked dividend",
      fifelse(
        tax_treatment == "corporate_share_sale_deferral_benefit",
        "Corporate share sale: normal CGT",
        "Corporate share sale: averaged CGT"
      )
    )
  )
)]

p_hidden_avg <- ggplot(
  hidden_avg_plot_data,
  aes(x = year, y = deferral_benefit, colour = tax_treatment)
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ tax_system) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Hidden labour: capital-gain averaging vs imputation",
    subtitle = selected_imputation_label,
    x = "Years",
    y = "Business advantage over wage/bank benchmark",
    colour = NULL,
    caption = imputation_treatment_footnote
  )

show_plot(p_hidden_avg)
save_plot_png(
  p_hidden_avg,
  "08_hidden_labour_averaging_vs_imputation.png",
  width = 12,
  height = 7.5
)

risky_avg_plot_data <- melt(
  comparison_risky_avg,
  id.vars = c("year", "tax_system"),
  measure.vars = c(
    "normal_tax_induced_gap",
    "averaging_tax_induced_gap",
    "imputation_tax_induced_gap",
    "corporate_share_sale_tax_induced_gap",
    "corporate_share_sale_averaging_tax_induced_gap"
  ),
  variable.name = "tax_treatment",
  value.name = "tax_induced_gap"
)

risky_avg_plot_data[, tax_treatment := fifelse(
  tax_treatment == "normal_tax_induced_gap",
  "Normal capital-gain realisation",
  fifelse(
    tax_treatment == "averaging_tax_induced_gap",
    "Capital-gain income averaging",
    fifelse(
      tax_treatment == "imputation_tax_induced_gap",
      "Company/imputation: franked dividend",
      fifelse(
        tax_treatment == "corporate_share_sale_tax_induced_gap",
        "Corporate share sale: normal CGT",
        "Corporate share sale: averaged CGT"
      )
    )
  )
)]

p_risky_avg <- ggplot(
  risky_avg_plot_data,
  aes(x = year, y = tax_induced_gap, colour = tax_treatment)
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ tax_system) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Risky business: capital-gain averaging vs imputation",
    subtitle = "Positive values mean the tax system makes the risky business more attractive",
    x = "Years",
    y = "Risky business certainty-equivalent advantage",
    colour = NULL,
    caption = imputation_treatment_footnote
  )

show_plot(p_risky_avg)
save_plot_png(
  p_risky_avg,
  "09_risky_business_averaging_vs_imputation.png",
  width = 12,
  height = 7.5
)

averaging_summary_table <- comparison_risky_avg[year == focus_T, .(
  tax_system,
  year,
  safe_work_after_tax,
  failure_prob,
  same_income_prob,
  success_prob,
  business_same_no_tax,
  business_success_no_tax,
  normal_same_tax,
  normal_success_tax,
  averaged_same_tax,
  averaged_success_tax,
  company_cash_same_state,
  company_tax_paid_success_state,
  company_cash_success_state,
  franking_credit_same_state,
  franking_credit_success_state,
  franking_credit_capitalised_same_state,
  franking_credit_capitalised_success_state,
  net_shareholder_topup_tax_same_state,
  net_shareholder_topup_tax_success_state,
  business_ce_after_normal_tax,
  business_ce_after_averaging_tax,
  business_ce_after_imputation,
  business_ce_after_corporate_share_sale,
  business_ce_after_corporate_share_sale_averaging,
  normal_tax_induced_gap,
  averaging_tax_induced_gap,
  imputation_tax_induced_gap,
  corporate_share_sale_tax_induced_gap,
  corporate_share_sale_averaging_tax_induced_gap,
  imputation_change_in_gap
)]

print(averaging_summary_table)
cat("\n", imputation_treatment_footnote, "\n", sep = "")

# ============================================================
# Capital gains tax explainer graphs
# ============================================================
#
# Main explainer plots:
#   A like-for-like asset comparison. Each plot uses the same asset with
#   a cost base, a holding period, inflation, and varying real returns.
#
# Founder/deferral plots:
#   A separate stylised founder case where labour-like value is built
#   inside an asset/company and tax is paid only at exit. These figures
#   show the deferral benefit through final after-tax wealth and the
#   present value of tax.
#
# Outputs are saved to:
#   CGT_explainer_graphs/

required_packages <- c("dplyr", "ggplot2", "scales", "tidyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install missing packages before running this script: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

# ============================================================
# Parameters
# ============================================================

output_dir <- "CGT_explainer_graphs"
dir.create(output_dir, showWarnings = FALSE)
unlink(
  list.files(
    output_dir,
    pattern = "\\.(png|pdf|csv)$",
    full.names = TRUE
  )
)

asset_cost_base <- 1000000
holding_period_years <- 10
inflation_rate <- 0.025
min_nominal_capital_gain_return <- 0
max_nominal_capital_gain_return <- 0.10

# A flat 30% rate is used for the core asset plots to isolate the
# treatment of the capital gain itself, rather than progressivity.
tax_rate <- 0.30

# The lumpiness plot uses the personal scale because the point of that
# chart is how progressive tax interacts with one-off realisation.
sale_year_other_income <- 0

# Founder case.
founder_annual_value_creation <- 100000
founder_real_reinvestment_return <- 0.025
founder_discount_rate <- (1 + inflation_rate) *
  (1 + founder_real_reinvestment_return) - 1
founder_max_years <- 20
normal_return_allowance_real <- 0.025
lumpiness_annual_wage_income <- 180000
lumpiness_annual_base_wage_with_gain <- 80000
lumpiness_terminal_capital_gain <- 1000000
income_type_annual_real_income <- 100000
income_type_real_reinvestment_return <- normal_return_allowance_real
pure_deferral_real_return <- 0.05
debt_example_real_interest_rate <- normal_return_allowance_real
debt_example_real_capital_gain_return <- debt_example_real_interest_rate
risk_success_gain <- 200000
risk_failure_loss <- -100000
risk_success_probability <- 0.50
delayed_loss_credit_pv_share <- 0.50

palette_explainer <- c(
  navy = "#214E7A",
  orange = "#D55E00",
  teal = "#1B8A7A",
  grey = "#636363",
  purple = "#6F4E9B",
  red = "#B3403A",
  gold = "#B88A00"
)

col <- function(name) {
  unname(palette_explainer[name])
}

# ============================================================
# Helpers
# ============================================================

money_k <- function(x) {
  sign <- if_else(x < 0, "-", "")
  paste0(
    sign,
    "$",
    format(round(abs(x) / 1000), big.mark = ",", trim = TRUE),
    "k"
  )
}

money_full <- function(x) {
  sign <- if_else(x < 0, "-", "")
  paste0(
    sign,
    "$",
    format(
      round(abs(x)),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  )
}

percent_plain <- function(x) {
  paste0(round(100 * x, 1), "%")
}

theme_explainer <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(colour = "grey30"),
      plot.caption = element_text(hjust = 0, colour = "grey35", size = 9),
      axis.title = element_text(colour = "grey20")
    )
}

save_plot <- function(plot, name, width = 9, height = 5.6) {
  ggsave(
    filename = file.path(output_dir, paste0(name, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 320,
    units = "in",
    bg = "white"
  )

  ggsave(
    filename = file.path(output_dir, paste0(name, ".pdf")),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    bg = "white"
  )
}

save_data <- function(data, name) {
  write.csv(
    data,
    file = file.path(output_dir, paste0(name, ".csv")),
    row.names = FALSE
  )
}

# ============================================================
# Australian resident personal income tax scale
# Excludes Medicare levy and offsets.
# ============================================================

australian_income_tax <- function(income) {
  income <- pmax(income, 0)

  pmax(pmin(income, 45000) - 18200, 0) * 0.16 +
    pmax(pmin(income, 135000) - 45000, 0) * 0.30 +
    pmax(pmin(income, 190000) - 135000, 0) * 0.37 +
    pmax(income - 190000, 0) * 0.45
}

tax_on_increment <- function(base_income, increment) {
  australian_income_tax(base_income + pmax(increment, 0)) -
    australian_income_tax(base_income)
}

tax_or_credit_on_increment <- function(base_income, increment) {
  if (increment >= 0) {
    return(tax_on_increment(base_income, increment))
  }

  -tax_on_increment(base_income, abs(increment))
}

# ============================================================
# Core asset model
# ============================================================

asset_return_grid <- data.frame(
  nominal_return = seq(
    min_nominal_capital_gain_return,
    max_nominal_capital_gain_return,
    by = 0.001
  )
) %>%
  mutate(
    annual_real_return = (1 + nominal_return) / (1 + inflation_rate) - 1,
    final_asset_value = asset_cost_base * (1 + nominal_return)^holding_period_years,
    indexed_cost_base = asset_cost_base * (1 + inflation_rate)^holding_period_years,
    normal_return_allowance_cost_base = asset_cost_base *
      ((1 + inflation_rate) * (1 + normal_return_allowance_real))^holding_period_years,
    nominal_gain = final_asset_value - asset_cost_base,
    real_gain = final_asset_value - indexed_cost_base,
    above_normal_gain = final_asset_value - normal_return_allowance_cost_base,
    inflation_gain = indexed_cost_base - asset_cost_base,

    taxable_gain_discount = if_else(
      nominal_gain >= 0,
      0.50 * nominal_gain,
      nominal_gain
    ),
    taxable_gain_indexation = if_else(
      real_gain >= 0,
      real_gain,
      pmin(nominal_gain, 0)
    ),
    taxable_gain_normal_return_allowance = if_else(
      above_normal_gain >= 0,
      above_normal_gain,
      pmin(nominal_gain, 0)
    ),
    taxable_gain_full_nominal = nominal_gain,

    tax_discount = tax_rate * taxable_gain_discount,
    tax_indexation = tax_rate * taxable_gain_indexation,
    tax_normal_return_allowance =
      tax_rate * taxable_gain_normal_return_allowance,
    tax_full_nominal = tax_rate * taxable_gain_full_nominal,

    after_tax_wealth_discount = final_asset_value - tax_discount,
    after_tax_wealth_indexation = final_asset_value - tax_indexation,
    after_tax_wealth_full_nominal = final_asset_value - tax_full_nominal,

    after_tax_real_gain_discount = real_gain - tax_discount,
    after_tax_real_gain_indexation_nominal_loss_only =
      real_gain - tax_indexation,
    after_tax_real_gain_indexation_full_loss_offset =
      real_gain * (1 - tax_rate),

    tax_discount_as_share_real_gain = if_else(
      real_gain > 0,
      tax_discount / real_gain,
      NA_real_
    ),
    tax_indexation_as_share_real_gain = if_else(
      real_gain > 0,
      tax_indexation / real_gain,
      NA_real_
    ),
    tax_full_nominal_as_share_real_gain = if_else(
      real_gain > 0,
      tax_full_nominal / real_gain,
      NA_real_
    )
  )

tax_base_data <- asset_return_grid %>%
  select(
    nominal_return,
    annual_real_return,
    `50% discount` = tax_discount_as_share_real_gain,
    `Inflation indexation` = tax_indexation_as_share_real_gain,
    `Full nominal gain taxed` = tax_full_nominal_as_share_real_gain
  ) %>%
  pivot_longer(
    cols = c(`50% discount`, `Inflation indexation`, `Full nominal gain taxed`),
    names_to = "treatment",
    values_to = "tax_as_share_real_gain"
  )

p_tax_base <- ggplot(
  tax_base_data,
  aes(
    x = nominal_return * 100,
    y = tax_as_share_real_gain * 100,
    colour = treatment
  )
) +
  geom_hline(yintercept = 30, colour = "grey65", linetype = "dashed") +
  geom_line(linewidth = 1, na.rm = TRUE) +
  scale_colour_manual(
    values = c(
      "50% discount" = col("orange"),
      "Inflation indexation" = col("navy"),
      "Full nominal gain taxed" = col("grey")
    )
  ) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "How much of the real gain is taxed?",
    subtitle = "The return shown is the capital-gain component, not the investor's total return",
    x = "Annual nominal capital-gain return",
    y = "Tax paid as a share of the real gain",
    caption = paste0(
      "Asset cost base: ", money_full(asset_cost_base),
      ". Held for ", holding_period_years, " years. Inflation: ",
      percent_plain(inflation_rate), " per year. Tax rate: ",
      percent_plain(tax_rate),
      ". X-axis excludes dividends, rent, and other non-CGT returns."
    )
  ) +
  theme_explainer()

save_plot(p_tax_base, "discount_vs_indexation_tax_share_of_real_gain_flat_30pct")
save_data(tax_base_data, "discount_vs_indexation_tax_share_of_real_gain_flat_30pct")

allowance_data <- asset_return_grid %>%
  select(
    nominal_return,
    annual_real_return,
    `50% discount` = tax_discount,
    `CPI indexation` = tax_indexation,
    `CPI plus normal return allowance` = tax_normal_return_allowance
  ) %>%
  pivot_longer(
    cols = c(
      `50% discount`,
      `CPI indexation`,
      `CPI plus normal return allowance`
    ),
    names_to = "allowance",
    values_to = "tax_paid"
  )

p_allowance <- ggplot(
  allowance_data,
  aes(x = nominal_return * 100, y = tax_paid, colour = allowance)
) +
  geom_line(linewidth = 1) +
  scale_colour_manual(
    values = c(
      "50% discount" = col("orange"),
      "CPI indexation" = col("navy"),
      "CPI plus normal return allowance" = col("purple")
    )
  ) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  scale_y_continuous(labels = money_k) +
  labs(
    title = "What return should be excluded from tax?",
    subtitle = "CPI indexation removes inflation; a normal-return allowance removes more",
    x = "Annual nominal capital-gain return",
    y = "Tax paid at realisation",
    caption = paste0(
      "Asset cost base: ", money_full(asset_cost_base),
      ". Held for ", holding_period_years, " years. Inflation: ",
      percent_plain(inflation_rate), " per year. Normal real return allowance: ",
      percent_plain(normal_return_allowance_real), ". Tax rate: ",
      percent_plain(tax_rate), "."
    )
  ) +
  theme_explainer()

save_plot(p_allowance, "indexation_choice_cpi_vs_normal_return_allowance_flat_30pct")
save_data(allowance_data, "indexation_choice_cpi_vs_normal_return_allowance_flat_30pct")

debt_example_nominal_asset_return <- (1 + inflation_rate) *
  (1 + debt_example_real_capital_gain_return) - 1
debt_example_nominal_interest_rate <- (1 + inflation_rate) *
  (1 + debt_example_real_interest_rate) - 1
debt_example_final_asset_value <- asset_cost_base *
  (1 + debt_example_nominal_asset_return)
debt_example_indexed_cost_base <- asset_cost_base * (1 + inflation_rate)
debt_example_real_capital_gain <- debt_example_final_asset_value -
  debt_example_indexed_cost_base
debt_example_nominal_interest <- asset_cost_base *
  debt_example_nominal_interest_rate
debt_example_taxable_position_nominal_interest <-
  debt_example_real_capital_gain - debt_example_nominal_interest
debt_example_tax_saving <- -tax_rate *
  debt_example_taxable_position_nominal_interest

debt_example_data <- data.frame(
  component = c(
    "Real capital gain taxed",
    "Nominal interest deducted",
    "Net taxable loss",
    "Tax saving at 30%"
  ),
  amount = c(
    debt_example_real_capital_gain,
    -debt_example_nominal_interest,
    debt_example_taxable_position_nominal_interest,
    debt_example_tax_saving
  ),
  type = c(
    "Taxable income",
    "Deduction",
    "Net tax base",
    "Tax saving"
  )
) %>%
  mutate(
    component = factor(component, levels = component)
  )

p_debt <- ggplot(
  debt_example_data,
  aes(x = component, y = amount, fill = type)
) +
  geom_hline(yintercept = 0, colour = "grey55") +
  geom_col(width = 0.68) +
  geom_text(
    aes(label = money_k(amount)),
    vjust = if_else(debt_example_data$amount >= 0, -0.25, 1.15),
    size = 3.4
  ) +
  scale_fill_manual(
    values = c(
      "Taxable income" = col("teal"),
      "Deduction" = col("red"),
      "Net tax base" = col("navy"),
      "Tax saving" = col("orange")
    )
  ) +
  scale_x_discrete(labels = label_wrap(16)) +
  scale_y_continuous(labels = money_k, expand = expansion(mult = c(0.15, 0.15))) +
  labs(
    title = "Nominal interest deductions can create a debt-financing bias",
    subtitle = "A break-even debt-funded asset can generate a tax loss when gains are indexed",
    x = NULL,
    y = "One-year amount",
    caption = paste0(
      "Asset cost base and debt: ", money_full(asset_cost_base),
      ". Inflation: ", percent_plain(inflation_rate),
      ". Real capital gain and real interest rate: ",
      percent_plain(debt_example_real_interest_rate),
      ". The pre-tax economic profit is zero."
    )
  ) +
  theme_explainer() +
  theme(
    axis.text.x = element_text(size = 9),
    legend.position = "none"
  )

save_plot(
  p_debt,
  "debt_financing_nominal_interest_deduction_with_indexation_flat_30pct",
  width = 9,
  height = 5.6
)
save_data(
  debt_example_data,
  "debt_financing_nominal_interest_deduction_with_indexation_flat_30pct"
)

tax_paid_data <- asset_return_grid %>%
  select(
    nominal_return,
    annual_real_return,
    `50% discount` = tax_discount,
    `Inflation indexation` = tax_indexation,
    `Full nominal gain taxed` = tax_full_nominal
  ) %>%
  pivot_longer(
    cols = c(`50% discount`, `Inflation indexation`, `Full nominal gain taxed`),
    names_to = "treatment",
    values_to = "tax_paid"
  )

p_tax_paid <- ggplot(
  tax_paid_data,
  aes(x = nominal_return * 100, y = tax_paid, colour = treatment)
) +
  geom_line(linewidth = 1) +
  scale_colour_manual(
    values = c(
      "50% discount" = col("orange"),
      "Inflation indexation" = col("navy"),
      "Full nominal gain taxed" = col("grey")
    )
  ) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  scale_y_continuous(labels = money_k) +
  labs(
    title = "Tax paid on the same asset",
    subtitle = "Indexation taxes only the real gain; the discount taxes half the nominal gain",
    x = "Annual nominal capital-gain return",
    y = "Tax paid at realisation",
    caption = paste0(
      "Asset cost base: ", money_full(asset_cost_base),
      ". Held for ", holding_period_years, " years. Inflation: ",
      percent_plain(inflation_rate), " per year. Tax rate: ",
      percent_plain(tax_rate), "."
    )
  ) +
  theme_explainer()

save_plot(p_tax_paid, "discount_vs_indexation_tax_paid_flat_30pct")
save_data(tax_paid_data, "discount_vs_indexation_tax_paid_flat_30pct")

wealth_data <- asset_return_grid %>%
  select(
    nominal_return,
    annual_real_return,
    `No capital gains tax` = final_asset_value,
    `50% discount` = after_tax_wealth_discount,
    `Inflation indexation` = after_tax_wealth_indexation,
    `Full nominal gain taxed` = after_tax_wealth_full_nominal
  ) %>%
  pivot_longer(
    cols = c(
      `No capital gains tax`,
      `50% discount`,
      `Inflation indexation`,
      `Full nominal gain taxed`
    ),
    names_to = "treatment",
    values_to = "final_wealth"
  )

p_wealth <- ggplot(
  wealth_data,
  aes(x = nominal_return * 100, y = final_wealth, colour = treatment)
) +
  geom_line(linewidth = 1) +
  scale_colour_manual(
    values = c(
      "No capital gains tax" = col("teal"),
      "50% discount" = col("orange"),
      "Inflation indexation" = col("navy"),
      "Full nominal gain taxed" = col("grey")
    )
  ) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  scale_y_continuous(labels = money_k) +
  labs(
    title = "Final after-tax wealth from the same asset",
    subtitle = "The gain is the same before tax; only the tax treatment changes",
    x = "Annual nominal capital-gain return",
    y = "Final wealth after tax",
    caption = paste0(
      "Asset cost base: ", money_full(asset_cost_base),
      ". Held for ", holding_period_years, " years. Inflation: ",
      percent_plain(inflation_rate), " per year. Tax rate: ",
      percent_plain(tax_rate), "."
    )
  ) +
  theme_explainer()

save_plot(p_wealth, "discount_vs_indexation_final_after_tax_wealth_flat_30pct")
save_data(wealth_data, "discount_vs_indexation_final_after_tax_wealth_flat_30pct")

loss_data <- asset_return_grid %>%
  select(
    annual_real_return,
    nominal_return,
    `50% discount; nominal losses recognised` =
      after_tax_real_gain_discount,
    `Real gains taxed; nominal losses recognised` =
      after_tax_real_gain_indexation_nominal_loss_only,
    `Real gains taxed; full loss offset` =
      after_tax_real_gain_indexation_full_loss_offset
  ) %>%
  pivot_longer(
    cols = c(
      `50% discount; nominal losses recognised`,
      `Real gains taxed; nominal losses recognised`,
      `Real gains taxed; full loss offset`
    ),
    names_to = "treatment",
    values_to = "after_tax_real_gain"
  )

p_loss <- ggplot(
  loss_data,
  aes(x = nominal_return * 100, y = after_tax_real_gain, colour = treatment)
) +
  geom_hline(yintercept = 0, colour = "grey65") +
  geom_vline(xintercept = 0, colour = "grey80", linetype = "dashed") +
  geom_line(linewidth = 1) +
  scale_colour_manual(
    values = c(
      "50% discount; nominal losses recognised" = col("orange"),
      "Real gains taxed; nominal losses recognised" = col("red"),
      "Real gains taxed; full loss offset" = col("navy")
    )
  ) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  scale_y_continuous(labels = money_k) +
  labs(
    title = "Nominal losses are recognised, but inflationary real losses are not",
    subtitle = "For a real loss, the loss credit is calculated using the unindexed cost base",
    x = "Annual nominal capital-gain return",
    y = "After-tax real gain or loss",
    caption = paste0(
      "Asset cost base: ", money_full(asset_cost_base),
      ". Held for ", holding_period_years, " years. Inflation: ",
      percent_plain(inflation_rate), " per year. Tax rate: ",
      percent_plain(tax_rate),
      ". Full real loss offset is shown as a comparator."
    )
  ) +
  theme_explainer()

save_plot(p_loss, "loss_rules_nominal_vs_real_loss_credit_flat_30pct")
save_data(loss_data, "loss_rules_nominal_vs_real_loss_credit_flat_30pct")

# ============================================================
# Lumpiness and income averaging
# ============================================================

lumpiness_data <- asset_return_grid %>%
  mutate(
    tax_indexation_sale_year = vapply(
      taxable_gain_indexation,
      function(x) tax_or_credit_on_increment(sale_year_other_income, x),
      numeric(1)
    ),
    tax_indexation_averaged = holding_period_years * vapply(
      taxable_gain_indexation / holding_period_years,
      function(x) tax_or_credit_on_increment(sale_year_other_income, x),
      numeric(1)
    ),
    tax_discount_progressive = vapply(
      taxable_gain_discount,
      function(x) tax_or_credit_on_increment(sale_year_other_income, x),
      numeric(1)
    )
  ) %>%
  select(
    nominal_return,
    annual_real_return,
    `Indexed gain taxed in sale year` = tax_indexation_sale_year,
    `Indexed gain with income averaging` = tax_indexation_averaged,
    `50% discount in sale year` = tax_discount_progressive
  ) %>%
  pivot_longer(
    cols = c(
      `Indexed gain taxed in sale year`,
      `Indexed gain with income averaging`,
      `50% discount in sale year`
    ),
    names_to = "treatment",
    values_to = "tax_paid"
  )

p_lumpiness <- ggplot(
  lumpiness_data,
  aes(x = nominal_return * 100, y = tax_paid, colour = treatment)
) +
  geom_line(linewidth = 1) +
  scale_colour_manual(
    values = c(
      "Indexed gain taxed in sale year" = col("red"),
      "Indexed gain with income averaging" = col("navy"),
      "50% discount in sale year" = col("orange")
    )
  ) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  scale_y_continuous(labels = money_k) +
  labs(
    title = "Lumpiness matters under a progressive tax scale",
    subtitle = "The same asset gain can face different tax if it is all assessed in one year",
    x = "Annual nominal capital-gain return",
    y = "Tax paid at realisation",
    caption = paste0(
      "Asset cost base: ", money_full(asset_cost_base),
      ". Held for ", holding_period_years, " years. Inflation: ",
      percent_plain(inflation_rate),
      " per year. Uses the resident personal scale, excluding Medicare levy and offsets."
    )
  ) +
  theme_explainer()

save_plot(p_lumpiness, "lumpiness_tax_by_return_progressive_scale")
save_data(lumpiness_data, "lumpiness_tax_by_return_progressive_scale")

lumpiness_example_data <- data.frame(
  scenario = c(
    "$180k wages each year",
    "$80k wages plus $1m gain in final year",
    "$80k wages plus $1m gain averaged"
  ),
  total_income = c(
    holding_period_years * lumpiness_annual_wage_income,
    holding_period_years * lumpiness_annual_base_wage_with_gain +
      lumpiness_terminal_capital_gain,
    holding_period_years * lumpiness_annual_base_wage_with_gain +
      lumpiness_terminal_capital_gain
  ),
  tax_paid = c(
    holding_period_years *
      australian_income_tax(lumpiness_annual_wage_income),
    (holding_period_years - 1) *
      australian_income_tax(lumpiness_annual_base_wage_with_gain) +
      australian_income_tax(
        lumpiness_annual_base_wage_with_gain +
          lumpiness_terminal_capital_gain
      ),
    holding_period_years *
      australian_income_tax(
        lumpiness_annual_base_wage_with_gain +
          lumpiness_terminal_capital_gain / holding_period_years
      )
  )
) %>%
  mutate(
    scenario = factor(scenario, levels = scenario),
    effective_tax_rate = tax_paid / total_income
  )

p_lumpiness_example <- ggplot(
  lumpiness_example_data,
  aes(x = scenario, y = effective_tax_rate * 100, fill = scenario)
) +
  geom_col(width = 0.68) +
  geom_text(
    aes(
      label = paste0(
        label_percent(scale = 1, accuracy = 0.1)(effective_tax_rate * 100),
        "\n",
        money_k(tax_paid)
      )
    ),
    vjust = -0.25,
    size = 3.4,
    lineheight = 0.9
  ) +
  scale_fill_manual(
    values = c(
      "$180k wages each year" = col("navy"),
      "$80k wages plus $1m gain in final year" = col("red"),
      "$80k wages plus $1m gain averaged" = col("teal")
    )
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Lumpy gains can face higher tax under a progressive scale",
    subtitle = "The same $1.8m of income over 10 years is taxed differently depending on timing",
    x = NULL,
    y = "Tax paid as a share of total income",
    caption = "Uses the resident personal scale, excluding Medicare levy and offsets."
  ) +
  theme_explainer() +
  theme(
    axis.text.x = element_text(angle = 16, hjust = 1),
    legend.position = "none"
  )

save_plot(p_lumpiness_example, "lumpiness_wage_income_vs_final_year_capital_gain_example")
save_data(lumpiness_example_data, "lumpiness_wage_income_vs_final_year_capital_gain_example")

income_type_data <- data.frame(
  income_type = c(
    "Wages earned each year",
    "Real capital income earned each year",
    "Real capital gain realised in final year",
    "Real capital gain averaged over 10 years"
  ),
  timing = c(
    "Annual income",
    "Annual income",
    "One-year realisation",
    "Averaged gain"
  ),
  total_real_income = income_type_annual_real_income * holding_period_years,
  tax_paid = c(
    holding_period_years *
      australian_income_tax(income_type_annual_real_income),
    holding_period_years *
      australian_income_tax(income_type_annual_real_income),
    australian_income_tax(
      income_type_annual_real_income * holding_period_years
    ),
    holding_period_years *
      australian_income_tax(income_type_annual_real_income)
  )
) %>%
  mutate(
    income_type = factor(income_type, levels = income_type),
    effective_tax_rate = tax_paid / total_real_income
  )

p_income_type <- ggplot(
  income_type_data,
  aes(x = income_type, y = effective_tax_rate * 100, fill = timing)
) +
  geom_col(width = 0.68) +
  geom_text(
    aes(
      label = paste0(
        label_percent(scale = 1, accuracy = 0.1)(effective_tax_rate * 100),
        "\n",
        money_k(tax_paid)
      )
    ),
    vjust = -0.25,
    size = 3.4,
    lineheight = 0.9
  ) +
  scale_fill_manual(
    values = c(
      "Annual income" = col("navy"),
      "One-year realisation" = col("red"),
      "Averaged gain" = col("teal")
    )
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Wages, capital income, and capital gains can be taxed differently",
    subtitle = "The same $1m of real income over 10 years; only timing and income form change",
    x = NULL,
    y = "Tax paid as a share of total real income",
    caption = "Resident personal scale; excludes Medicare levy, offsets, and deferral PV effects."
  ) +
  theme_explainer() +
  theme(
    axis.text.x = element_text(angle = 16, hjust = 1),
    legend.position = "none"
  )

save_plot(p_income_type, "income_type_wages_capital_income_vs_capital_gains_progressive_scale")
save_data(income_type_data, "income_type_wages_capital_income_vs_capital_gains_progressive_scale")

income_type_years <- seq_len(holding_period_years)
income_type_annual_tax <- australian_income_tax(income_type_annual_real_income)
income_type_annual_after_tax_income <-
  income_type_annual_real_income - income_type_annual_tax
income_type_accumulation_factors <-
  (1 + income_type_real_reinvestment_return)^(holding_period_years - income_type_years)
income_type_discount_factors <-
  (1 + income_type_real_reinvestment_return)^income_type_years

income_type_annual_pv_tax <- sum(
  income_type_annual_tax / income_type_discount_factors
)
income_type_annual_final_wealth <- sum(
  income_type_annual_after_tax_income * income_type_accumulation_factors
)
income_type_deferred_pre_tax_wealth <- sum(
  income_type_annual_real_income * income_type_accumulation_factors
)

income_type_deferred_full_tax <- australian_income_tax(
  income_type_deferred_pre_tax_wealth
)
income_type_deferred_discount_tax <- australian_income_tax(
  0.50 * income_type_deferred_pre_tax_wealth
)

income_type_deferral_data <- data.frame(
  scenario = c(
    "Wages taxed each year",
    "Real capital income taxed each year",
    "Deferred real capital gain: full taxation",
    "Deferred real capital gain: 50% discount"
  ),
  tax_timing = c(
    "Annual tax",
    "Annual tax",
    "Deferred tax at exit",
    "Deferred tax at exit, 50% discount"
  ),
  pre_tax_final_wealth = c(
    income_type_deferred_pre_tax_wealth,
    income_type_deferred_pre_tax_wealth,
    income_type_deferred_pre_tax_wealth,
    income_type_deferred_pre_tax_wealth
  ),
  tax_paid = c(
    holding_period_years * income_type_annual_tax,
    holding_period_years * income_type_annual_tax,
    income_type_deferred_full_tax,
    income_type_deferred_discount_tax
  ),
  present_value_tax = c(
    income_type_annual_pv_tax,
    income_type_annual_pv_tax,
    income_type_deferred_full_tax /
      (1 + income_type_real_reinvestment_return)^holding_period_years,
    income_type_deferred_discount_tax /
      (1 + income_type_real_reinvestment_return)^holding_period_years
  ),
  final_after_tax_wealth = c(
    income_type_annual_final_wealth,
    income_type_annual_final_wealth,
    income_type_deferred_pre_tax_wealth - income_type_deferred_full_tax,
    income_type_deferred_pre_tax_wealth - income_type_deferred_discount_tax
  )
) %>%
  mutate(
    scenario = factor(scenario, levels = scenario)
  )

income_type_deferral_plot_data <- income_type_deferral_data %>%
  select(
    scenario,
    tax_timing,
    `Present value of tax` = present_value_tax,
    `Final after-tax wealth` = final_after_tax_wealth
  ) %>%
  pivot_longer(
    cols = c(`Present value of tax`, `Final after-tax wealth`),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(
      metric,
      levels = c("Present value of tax", "Final after-tax wealth")
    )
  )

p_income_type_deferral <- ggplot(
  income_type_deferral_plot_data,
  aes(x = scenario, y = value, fill = tax_timing)
) +
  geom_col(width = 0.68) +
  geom_text(
    aes(label = money_k(value)),
    vjust = -0.25,
    size = 3.2
  ) +
  facet_wrap(~metric, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Annual tax" = col("navy"),
      "Deferred tax at exit" = col("red"),
      "Deferred tax at exit, 50% discount" = col("orange")
    )
  ) +
  scale_x_discrete(labels = label_wrap(18)) +
  scale_y_continuous(
    labels = money_k,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Deferral changes both the timing of tax and final wealth",
    subtitle = paste0(
      money_full(income_type_annual_real_income),
      " of real income accrues each year and is reinvested over ",
      holding_period_years,
      " years"
    ),
    x = NULL,
    y = NULL,
    caption = paste0(
      "Resident personal scale; excludes Medicare levy and offsets. Real reinvestment/",
      "discount rate: ", percent_plain(income_type_real_reinvestment_return),
      ". Reinvestment returns are used to value timing, not separately taxed."
    )
  ) +
  theme_explainer() +
  theme(
    axis.text.x = element_text(size = 9),
    legend.position = "bottom"
  )

save_plot(
  p_income_type_deferral,
  "income_type_deferral_present_value_tax_and_final_wealth_progressive_scale",
  width = 10,
  height = 6
)
save_data(
  income_type_deferral_data,
  "income_type_deferral_present_value_tax_and_final_wealth_progressive_scale"
)

# ============================================================
# Deferral and withholding examples for explainer text
# ============================================================

pure_deferral_years <- seq_len(holding_period_years)
pure_deferral_annual_tax_paid <- numeric(holding_period_years)
pure_deferral_annual_tax_wealth <- asset_cost_base

for (year in pure_deferral_years) {
  annual_real_income <- pure_deferral_annual_tax_wealth *
    pure_deferral_real_return
  pure_deferral_annual_tax_paid[year] <- tax_rate * annual_real_income
  pure_deferral_annual_tax_wealth <- pure_deferral_annual_tax_wealth +
    annual_real_income - pure_deferral_annual_tax_paid[year]
}

pure_deferral_pre_tax_wealth <- asset_cost_base *
  (1 + pure_deferral_real_return)^holding_period_years
pure_deferral_real_gain <- pure_deferral_pre_tax_wealth - asset_cost_base
pure_deferral_full_tax <- tax_rate * pure_deferral_real_gain
pure_deferral_discount_tax <- tax_rate * 0.50 * pure_deferral_real_gain

pure_deferral_data <- data.frame(
  scenario = c(
    "Real return taxed each year",
    "Real capital gain taxed at exit",
    "Real capital gain taxed at exit with 50% discount"
  ),
  tax_timing = c(
    "Annual tax",
    "Deferred tax at exit",
    "Deferred tax at exit, 50% discount"
  ),
  present_value_tax = c(
    sum(
      pure_deferral_annual_tax_paid /
        (1 + pure_deferral_real_return)^pure_deferral_years
    ),
    pure_deferral_full_tax /
      (1 + pure_deferral_real_return)^holding_period_years,
    pure_deferral_discount_tax /
      (1 + pure_deferral_real_return)^holding_period_years
  ),
  final_after_tax_wealth = c(
    pure_deferral_annual_tax_wealth,
    pure_deferral_pre_tax_wealth - pure_deferral_full_tax,
    pure_deferral_pre_tax_wealth - pure_deferral_discount_tax
  )
) %>%
  mutate(scenario = factor(scenario, levels = scenario))

pure_deferral_plot_data <- pure_deferral_data %>%
  select(
    scenario,
    tax_timing,
    `Present value of tax` = present_value_tax,
    `Final after-tax wealth` = final_after_tax_wealth
  ) %>%
  pivot_longer(
    cols = c(`Present value of tax`, `Final after-tax wealth`),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(
      metric,
      levels = c("Present value of tax", "Final after-tax wealth")
    )
  )

p_pure_deferral <- ggplot(
  pure_deferral_plot_data,
  aes(x = scenario, y = value, fill = tax_timing)
) +
  geom_col(width = 0.68) +
  geom_text(aes(label = money_k(value)), vjust = -0.25, size = 3.2) +
  facet_wrap(~metric, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Annual tax" = col("navy"),
      "Deferred tax at exit" = col("red"),
      "Deferred tax at exit, 50% discount" = col("orange")
    )
  ) +
  scale_x_discrete(labels = label_wrap(18)) +
  scale_y_continuous(labels = money_k, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Deferral is valuable even when the real gain is eventually taxed",
    subtitle = paste0(
      money_full(asset_cost_base),
      " asset earning a ",
      percent_plain(pure_deferral_real_return),
      " annual real capital gain over ",
      holding_period_years,
      " years"
    ),
    x = NULL,
    y = NULL,
    caption = paste0(
      "Flat tax rate: ", percent_plain(tax_rate),
      ". The same real return is taxed annually or only when the gain is realised."
    )
  ) +
  theme_explainer() +
  theme(axis.text.x = element_text(size = 9))

save_plot(
  p_pure_deferral,
  "pure_capital_gain_deferral_pv_tax_and_final_wealth_flat_30pct",
  width = 10,
  height = 6
)
save_data(
  pure_deferral_data,
  "pure_capital_gain_deferral_pv_tax_and_final_wealth_flat_30pct"
)

imputation_company_tax_paid <- numeric(holding_period_years)
imputation_company_wealth <- asset_cost_base

for (year in pure_deferral_years) {
  company_profit <- imputation_company_wealth * pure_deferral_real_return
  imputation_company_tax_paid[year] <- tax_rate * company_profit
  imputation_company_wealth <- imputation_company_wealth +
    company_profit - imputation_company_tax_paid[year]
}

imputation_franking_credits <- sum(imputation_company_tax_paid)
imputation_share_sale_price_with_credits <-
  imputation_company_wealth + imputation_franking_credits
imputation_share_sale_cgt <- tax_rate *
  (imputation_share_sale_price_with_credits - asset_cost_base)
imputation_capitalised_credit_value <- imputation_franking_credits

imputation_data <- data.frame(
  scenario = c(
    "Real return taxed personally each year",
    "Company tax paid each year; credits capitalised at sale",
    "Untaxed asset, CGT paid only at exit"
  ),
  tax_timing = c(
    "Annual personal tax",
    "Company tax plus capitalised credits",
    "Deferred personal CGT"
  ),
  company_tax_paid = c(
    0,
    sum(imputation_company_tax_paid),
    0
  ),
  franking_credits_capitalised_at_sale = c(
    0,
    imputation_capitalised_credit_value,
    0
  ),
  exit_personal_cgt = c(
    0,
    imputation_share_sale_cgt,
    pure_deferral_full_tax
  ),
  share_sale_price = c(
    NA_real_,
    imputation_share_sale_price_with_credits,
    pure_deferral_pre_tax_wealth
  ),
  present_value_tax = c(
    sum(
      pure_deferral_annual_tax_paid /
        (1 + pure_deferral_real_return)^pure_deferral_years
    ),
    sum(
      imputation_company_tax_paid /
        (1 + pure_deferral_real_return)^pure_deferral_years
    ) +
      imputation_share_sale_cgt /
        (1 + pure_deferral_real_return)^holding_period_years -
      imputation_capitalised_credit_value /
        (1 + pure_deferral_real_return)^holding_period_years,
    pure_deferral_full_tax /
      (1 + pure_deferral_real_return)^holding_period_years
  ),
  final_after_tax_wealth = c(
    pure_deferral_annual_tax_wealth,
    imputation_share_sale_price_with_credits - imputation_share_sale_cgt,
    pure_deferral_pre_tax_wealth - pure_deferral_full_tax
  )
) %>%
  mutate(scenario = factor(scenario, levels = scenario))

imputation_plot_data <- imputation_data %>%
  select(
    scenario,
    tax_timing,
    `Present value of net tax burden` = present_value_tax,
    `Final after-tax wealth` = final_after_tax_wealth
  ) %>%
  pivot_longer(
    cols = c(`Present value of net tax burden`, `Final after-tax wealth`),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(
      metric,
      levels = c("Present value of net tax burden", "Final after-tax wealth")
    )
  )

p_imputation <- ggplot(
  imputation_plot_data,
  aes(x = scenario, y = value, fill = tax_timing)
) +
  geom_col(width = 0.68) +
  geom_text(aes(label = money_k(value)), vjust = -0.25, size = 3.2) +
  facet_wrap(~metric, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Annual personal tax" = col("navy"),
      "Company tax plus capitalised credits" = col("teal"),
      "Deferred personal CGT" = col("red")
    )
  ) +
  scale_x_discrete(labels = label_wrap(18)) +
  scale_y_continuous(labels = money_k, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Company tax can act like withholding for shareholder income",
    subtitle = "Franking credits are capitalised into the final share sale value",
    x = NULL,
    y = NULL,
    caption = paste0(
      "Stylised flat-rate example. Company tax and personal tax rate: ",
      percent_plain(tax_rate),
      ". Net tax burden subtracts the value of franking credits capitalised at sale."
    )
  ) +
  theme_explainer() +
  theme(axis.text.x = element_text(size = 9))

save_plot(
  p_imputation,
  "imputation_company_tax_as_withholding_flat_30pct",
  width = 10,
  height = 6
)
save_data(imputation_data, "imputation_company_tax_as_withholding_flat_30pct")

risk_data <- data.frame(
  policy = c(
    "No-tax benchmark",
    "Symmetric tax, same investment",
    "Symmetric tax, scaled up",
    "Asymmetric tax, scaled to match upside"
  ),
  investment_scale = c(
    1,
    1,
    1 / (1 - tax_rate),
    1 / (1 - tax_rate)
  ),
  success_payoff = c(
    risk_success_gain,
    risk_success_gain * (1 - tax_rate),
    risk_success_gain,
    risk_success_gain
  ),
  failure_payoff = c(
    risk_failure_loss,
    risk_failure_loss * (1 - tax_rate),
    risk_failure_loss,
    risk_failure_loss / (1 - tax_rate)
  )
) %>%
  mutate(
    expected_payoff = risk_success_probability * success_payoff +
      (1 - risk_success_probability) * failure_payoff,
    policy = factor(policy, levels = policy)
  )

risk_plot_data <- risk_data %>%
  select(
    policy,
    `Success outcome` = success_payoff,
    `Failure outcome` = failure_payoff
  ) %>%
  pivot_longer(
    cols = c(`Success outcome`, `Failure outcome`),
    names_to = "outcome",
    values_to = "payoff"
  ) %>%
  mutate(
    outcome = factor(
      outcome,
      levels = c("Success outcome", "Failure outcome")
    )
  )

p_risk <- ggplot(
  risk_plot_data,
  aes(x = policy, y = payoff, fill = outcome)
) +
  geom_hline(yintercept = 0, colour = "grey55") +
  geom_hline(
    yintercept = c(risk_success_gain, risk_failure_loss),
    colour = "grey65",
    linetype = "dashed"
  ) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  geom_text(
    aes(label = money_k(payoff)),
    position = position_dodge(width = 0.75),
    vjust = if_else(risk_plot_data$payoff >= 0, -0.25, 1.15),
    size = 3
  ) +
  scale_fill_manual(
    values = c(
      "Success outcome" = col("teal"),
      "Failure outcome" = col("red")
    )
  ) +
  scale_x_discrete(labels = label_wrap(16)) +
  scale_y_continuous(labels = money_k, expand = expansion(mult = c(0.15, 0.15))) +
  labs(
    title = "Symmetric tax lets investors scale up to restore the same risk",
    subtitle = "A project with a $200k gain if successful and a $100k loss if unsuccessful",
    x = NULL,
    y = "Project payoff after tax",
    caption = paste0(
      "Flat tax rate: ", percent_plain(tax_rate),
      ". Scaling by ", round(1 / (1 - tax_rate), 2),
      "x restores both upside and downside under symmetric treatment, but not when losses are denied."
    )
  ) +
  theme_explainer() +
  theme(axis.text.x = element_text(size = 9))

save_plot(
  p_risk,
  "risk_symmetric_vs_asymmetric_loss_treatment_flat_30pct",
  width = 10,
  height = 6
)
save_data(risk_data, "risk_symmetric_vs_asymmetric_loss_treatment_flat_30pct")

# ============================================================
# Core asset model, b set: Australian progressive income tax scale
# ============================================================

asset_return_grid_progressive <- asset_return_grid %>%
  mutate(
    tax_discount_progressive = vapply(
      taxable_gain_discount,
      function(x) tax_or_credit_on_increment(sale_year_other_income, x),
      numeric(1)
    ),
    tax_indexation_progressive = vapply(
      taxable_gain_indexation,
      function(x) tax_or_credit_on_increment(sale_year_other_income, x),
      numeric(1)
    ),
    tax_normal_return_allowance_progressive = vapply(
      taxable_gain_normal_return_allowance,
      function(x) tax_or_credit_on_increment(sale_year_other_income, x),
      numeric(1)
    ),
    tax_full_nominal_progressive = vapply(
      taxable_gain_full_nominal,
      function(x) tax_or_credit_on_increment(sale_year_other_income, x),
      numeric(1)
    ),

    after_tax_wealth_discount_progressive =
      final_asset_value - tax_discount_progressive,
    after_tax_wealth_indexation_progressive =
      final_asset_value - tax_indexation_progressive,
    after_tax_wealth_full_nominal_progressive =
      final_asset_value - tax_full_nominal_progressive,

    after_tax_real_gain_discount_progressive =
      real_gain - tax_discount_progressive,
    after_tax_real_gain_indexation_nominal_loss_only_progressive =
      real_gain - tax_indexation_progressive,
    after_tax_real_gain_indexation_full_loss_offset_progressive =
      real_gain - vapply(
        real_gain,
        function(x) tax_or_credit_on_increment(sale_year_other_income, x),
        numeric(1)
      ),

    tax_discount_progressive_as_share_real_gain = if_else(
      real_gain > 0,
      tax_discount_progressive / real_gain,
      NA_real_
    ),
    tax_indexation_progressive_as_share_real_gain = if_else(
      real_gain > 0,
      tax_indexation_progressive / real_gain,
      NA_real_
    ),
    tax_full_nominal_progressive_as_share_real_gain = if_else(
      real_gain > 0,
      tax_full_nominal_progressive / real_gain,
      NA_real_
    )
  )

tax_base_data_b <- asset_return_grid_progressive %>%
  select(
    nominal_return,
    annual_real_return,
    `50% discount` = tax_discount_progressive_as_share_real_gain,
    `Inflation indexation` = tax_indexation_progressive_as_share_real_gain,
    `Full nominal gain taxed` =
      tax_full_nominal_progressive_as_share_real_gain
  ) %>%
  pivot_longer(
    cols = c(`50% discount`, `Inflation indexation`, `Full nominal gain taxed`),
    names_to = "treatment",
    values_to = "tax_as_share_real_gain"
  )

p_tax_base_b <- ggplot(
  tax_base_data_b,
  aes(
    x = nominal_return * 100,
    y = tax_as_share_real_gain * 100,
    colour = treatment
  )
) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  scale_colour_manual(
    values = c(
      "50% discount" = col("orange"),
      "Inflation indexation" = col("navy"),
      "Full nominal gain taxed" = col("grey")
    )
  ) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "How much of the real gain is taxed?",
    subtitle = "Australian income tax scale; return shown is only the capital-gain component",
    x = "Annual nominal capital-gain return",
    y = "Tax paid as a share of the real gain",
    caption = paste0(
      "Asset cost base: ", money_full(asset_cost_base),
      ". Held for ", holding_period_years, " years. Inflation: ",
      percent_plain(inflation_rate),
      " per year. Excludes Medicare levy, offsets, and non-CGT returns."
    )
  ) +
  theme_explainer()

save_plot(p_tax_base_b, "discount_vs_indexation_tax_share_of_real_gain_progressive_scale")
save_data(tax_base_data_b, "discount_vs_indexation_tax_share_of_real_gain_progressive_scale")

allowance_data_b <- asset_return_grid_progressive %>%
  select(
    nominal_return,
    annual_real_return,
    `50% discount` = tax_discount_progressive,
    `CPI indexation` = tax_indexation_progressive,
    `CPI plus normal return allowance` =
      tax_normal_return_allowance_progressive
  ) %>%
  pivot_longer(
    cols = c(
      `50% discount`,
      `CPI indexation`,
      `CPI plus normal return allowance`
    ),
    names_to = "allowance",
    values_to = "tax_paid"
  )

p_allowance_b <- ggplot(
  allowance_data_b,
  aes(x = nominal_return * 100, y = tax_paid, colour = allowance)
) +
  geom_line(linewidth = 1) +
  scale_colour_manual(
    values = c(
      "50% discount" = col("orange"),
      "CPI indexation" = col("navy"),
      "CPI plus normal return allowance" = col("purple")
    )
  ) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  scale_y_continuous(labels = money_k) +
  labs(
    title = "What return should be excluded from tax?",
    subtitle = "Australian income tax scale",
    x = "Annual nominal capital-gain return",
    y = "Tax paid at realisation",
    caption = paste0(
      "Asset cost base: ", money_full(asset_cost_base),
      ". Held for ", holding_period_years, " years. Inflation: ",
      percent_plain(inflation_rate), " per year. Normal real return allowance: ",
      percent_plain(normal_return_allowance_real),
      ". Excludes Medicare levy and offsets."
    )
  ) +
  theme_explainer()

save_plot(p_allowance_b, "indexation_choice_cpi_vs_normal_return_allowance_progressive_scale")
save_data(allowance_data_b, "indexation_choice_cpi_vs_normal_return_allowance_progressive_scale")

tax_paid_data_b <- asset_return_grid_progressive %>%
  select(
    nominal_return,
    annual_real_return,
    `50% discount` = tax_discount_progressive,
    `Inflation indexation` = tax_indexation_progressive,
    `Full nominal gain taxed` = tax_full_nominal_progressive
  ) %>%
  pivot_longer(
    cols = c(`50% discount`, `Inflation indexation`, `Full nominal gain taxed`),
    names_to = "treatment",
    values_to = "tax_paid"
  )

p_tax_paid_b <- ggplot(
  tax_paid_data_b,
  aes(x = nominal_return * 100, y = tax_paid, colour = treatment)
) +
  geom_line(linewidth = 1) +
  scale_colour_manual(
    values = c(
      "50% discount" = col("orange"),
      "Inflation indexation" = col("navy"),
      "Full nominal gain taxed" = col("grey")
    )
  ) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  scale_y_continuous(labels = money_k) +
  labs(
    title = "Tax paid on the same asset",
    subtitle = "Australian income tax scale",
    x = "Annual nominal capital-gain return",
    y = "Tax paid at realisation",
    caption = paste0(
      "Asset cost base: ", money_full(asset_cost_base),
      ". Held for ", holding_period_years, " years. Inflation: ",
      percent_plain(inflation_rate),
      " per year. Excludes Medicare levy and offsets."
    )
  ) +
  theme_explainer()

save_plot(p_tax_paid_b, "discount_vs_indexation_tax_paid_progressive_scale")
save_data(tax_paid_data_b, "discount_vs_indexation_tax_paid_progressive_scale")

wealth_data_b <- asset_return_grid_progressive %>%
  select(
    nominal_return,
    annual_real_return,
    `No capital gains tax` = final_asset_value,
    `50% discount` = after_tax_wealth_discount_progressive,
    `Inflation indexation` = after_tax_wealth_indexation_progressive,
    `Full nominal gain taxed` = after_tax_wealth_full_nominal_progressive
  ) %>%
  pivot_longer(
    cols = c(
      `No capital gains tax`,
      `50% discount`,
      `Inflation indexation`,
      `Full nominal gain taxed`
    ),
    names_to = "treatment",
    values_to = "final_wealth"
  )

p_wealth_b <- ggplot(
  wealth_data_b,
  aes(x = nominal_return * 100, y = final_wealth, colour = treatment)
) +
  geom_line(linewidth = 1) +
  scale_colour_manual(
    values = c(
      "No capital gains tax" = col("teal"),
      "50% discount" = col("orange"),
      "Inflation indexation" = col("navy"),
      "Full nominal gain taxed" = col("grey")
    )
  ) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  scale_y_continuous(labels = money_k) +
  labs(
    title = "Final after-tax wealth from the same asset",
    subtitle = "Australian income tax scale",
    x = "Annual nominal capital-gain return",
    y = "Final wealth after tax",
    caption = paste0(
      "Asset cost base: ", money_full(asset_cost_base),
      ". Held for ", holding_period_years, " years. Inflation: ",
      percent_plain(inflation_rate),
      " per year. Excludes Medicare levy and offsets."
    )
  ) +
  theme_explainer()

save_plot(p_wealth_b, "discount_vs_indexation_final_after_tax_wealth_progressive_scale")
save_data(wealth_data_b, "discount_vs_indexation_final_after_tax_wealth_progressive_scale")

loss_data_b <- asset_return_grid_progressive %>%
  select(
    annual_real_return,
    nominal_return,
    `50% discount; nominal losses recognised` =
      after_tax_real_gain_discount_progressive,
    `Real gains taxed; nominal losses recognised` =
      after_tax_real_gain_indexation_nominal_loss_only_progressive,
    `Real gains taxed; full loss offset` =
      after_tax_real_gain_indexation_full_loss_offset_progressive
  ) %>%
  pivot_longer(
    cols = c(
      `50% discount; nominal losses recognised`,
      `Real gains taxed; nominal losses recognised`,
      `Real gains taxed; full loss offset`
    ),
    names_to = "treatment",
    values_to = "after_tax_real_gain"
  )

p_loss_b <- ggplot(
  loss_data_b,
  aes(x = nominal_return * 100, y = after_tax_real_gain, colour = treatment)
) +
  geom_hline(yintercept = 0, colour = "grey65") +
  geom_vline(xintercept = 0, colour = "grey80", linetype = "dashed") +
  geom_line(linewidth = 1) +
  scale_colour_manual(
    values = c(
      "50% discount; nominal losses recognised" = col("orange"),
      "Real gains taxed; nominal losses recognised" = col("red"),
      "Real gains taxed; full loss offset" = col("navy")
    )
  ) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  scale_y_continuous(labels = money_k) +
  labs(
    title = "Nominal losses are recognised, but inflationary real losses are not",
    subtitle = "Australian income tax scale",
    x = "Annual nominal capital-gain return",
    y = "After-tax real gain or loss",
    caption = paste0(
      "Asset cost base: ", money_full(asset_cost_base),
      ". Held for ", holding_period_years, " years. Inflation: ",
      percent_plain(inflation_rate),
      " per year. Excludes Medicare levy and offsets. Full real loss offset is shown as a comparator."
    )
  ) +
  theme_explainer()

save_plot(p_loss_b, "loss_rules_nominal_vs_real_loss_credit_progressive_scale")
save_data(loss_data_b, "loss_rules_nominal_vs_real_loss_credit_progressive_scale")

# Plot `lumpiness_tax_by_return_progressive_scale` already uses the
# Australian progressive scale because lumpiness is about the interaction
# between realisation timing and progressivity.

# ============================================================
# Founder case: labour-like value built inside an asset/company
# ============================================================

simulate_founder_year <- function(T) {
  value_at_exit <- 0

  for (year in seq_len(T)) {
    annual_value <- founder_annual_value_creation *
      (1 + inflation_rate)^(year - 1)

    value_at_exit <- value_at_exit * (1 + founder_discount_rate) +
      annual_value
  }

  annual_tax_final_wealth <- 0
  annual_tax_paid <- numeric(T)

  for (year in seq_len(T)) {
    annual_value <- founder_annual_value_creation *
      (1 + inflation_rate)^(year - 1)
    taxable_return <- annual_tax_final_wealth * founder_discount_rate
    annual_tax <- tax_rate * (annual_value + taxable_return)
    annual_tax_paid[year] <- annual_tax

    annual_tax_final_wealth <- annual_tax_final_wealth + taxable_return +
      annual_value - annual_tax
  }

  deferred_full_tax <- tax_rate * value_at_exit
  deferred_discount_tax <- tax_rate * 0.50 * value_at_exit

  data.frame(
    years_to_exit = T,
    scenario = c(
      "Tax paid each year",
      "Deferred tax at exit",
      "Deferred tax at exit: 50% discount"
    ),
    final_after_tax_wealth = c(
      annual_tax_final_wealth,
      value_at_exit - deferred_full_tax,
      value_at_exit - deferred_discount_tax
    ),
    pv_tax = c(
      sum(annual_tax_paid / (1 + founder_discount_rate)^(seq_len(T) - 1)),
      deferred_full_tax / (1 + founder_discount_rate)^T,
      deferred_discount_tax / (1 + founder_discount_rate)^T
    ),
    tax_paid_nominal = c(
      sum(annual_tax_paid),
      deferred_full_tax,
      deferred_discount_tax
    ),
    pre_tax_exit_value = value_at_exit
  )
}

founder_data <- bind_rows(lapply(seq_len(founder_max_years), simulate_founder_year)) %>%
  mutate(
    scenario = factor(
      scenario,
      levels = c(
        "Tax paid each year",
        "Deferred tax at exit",
        "Deferred tax at exit: 50% discount"
      )
    )
  )

p_founder_wealth <- ggplot(
  founder_data,
  aes(x = years_to_exit, y = final_after_tax_wealth, colour = scenario)
) +
  geom_line(linewidth = 1) +
  scale_colour_manual(
    values = c(
      "Tax paid each year" = col("navy"),
      "Deferred tax at exit" = col("teal"),
      "Deferred tax at exit: 50% discount" = col("orange")
    )
  ) +
  scale_x_continuous(breaks = seq(0, founder_max_years, 5)) +
  scale_y_continuous(labels = money_k) +
  labs(
    title = "Founder case: deferral increases final after-tax wealth",
    subtitle = "Tax delayed until exit leaves more value compounding before tax is paid",
    x = "Years to exit",
    y = "Final after-tax wealth",
    caption = paste0(
      "Founder creates ", money_full(founder_annual_value_creation),
      " of real value each year. Tax rate: ", percent_plain(tax_rate),
      ". Reinvestment/discount rate: ", percent_plain(founder_discount_rate),
      " nominal."
    )
  ) +
  theme_explainer()

save_plot(p_founder_wealth, "founder_deferral_final_after_tax_wealth")
save_data(founder_data, "founder_deferral_model_data")

p_founder_pv_tax <- ggplot(
  founder_data,
  aes(x = years_to_exit, y = pv_tax, colour = scenario)
) +
  geom_line(linewidth = 1) +
  scale_colour_manual(
    values = c(
      "Tax paid each year" = col("navy"),
      "Deferred tax at exit" = col("teal"),
      "Deferred tax at exit: 50% discount" = col("orange")
    )
  ) +
  scale_x_continuous(breaks = seq(0, founder_max_years, 5)) +
  scale_y_continuous(labels = money_k) +
  labs(
    title = "Founder case: deferral lowers the present value of tax",
    subtitle = "Even when the same full gain is eventually taxed, later tax is worth less today",
    x = "Years to exit",
    y = "Present value of tax",
    caption = paste0(
      "Founder creates ", money_full(founder_annual_value_creation),
      " of real value each year. Tax rate: ", percent_plain(tax_rate),
      ". Reinvestment/discount rate: ", percent_plain(founder_discount_rate),
      " nominal."
    )
  ) +
  theme_explainer()

save_plot(p_founder_pv_tax, "founder_deferral_present_value_of_tax")

message("CGT explainer graph outputs written to: ", normalizePath(output_dir))

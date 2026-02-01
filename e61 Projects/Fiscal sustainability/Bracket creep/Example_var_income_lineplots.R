# Extend out the variable income example for CGT to allow us to look at a variety of income levels and realisation events.

#remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

# dir.create("C:/Rlibs", showWarnings = FALSE)
# .libPaths(c("C:/Rlibs", .libPaths()))
# install.packages("xfun")

# --- Libraries ---
library(dplyr)
library(ggplot2)
library(theme61)
library(data.table)
library(readxl)

# --- Options you can adjust ---
levy      <- TRUE   # Include the medicare levy
surcharge <- FALSE  # Not used below, but you can add later if needed
disc_rate <- 0.03   # Discount rate for PV
horizon   <- 60     # Number of periods

# Vectors over which you'll loop
#income_targets <- c(50000, 100000, 150000, 200000)  # adjust as you like
income_targets <- seq(00000,200000,by=500)
#regularities   <- c(2, 5, 10, 20)                   # e.g. how often CG happens
regularities   <- seq(1,20,1)
discount = 0.5

# -----------------------------
# 1. Tax function (unchanged)
# -----------------------------
tax_function <- function(income, include_levy = FALSE) {

  medicare_levy <- if (include_levy) {
    ifelse(
      income <= 45000,
      pmax(0, pmin((income - 27222) * 0.1, income * 0.02)),
      income * 0.02
    )
  } else {
    0
  }

  base_tax <- case_when(
    income <= 18200 ~ 0,
    income <= 45000 ~ (income - 18200) * 0.16,
    income <= 135000 ~ (45000 - 18200) * 0.16 +
      (income - 45000) * 0.30,
    income <= 190000 ~ (45000 - 18200) * 0.16 +
      (135000 - 45000) * 0.30 +
      (income - 135000) * 0.37,
    TRUE ~ (45000 - 18200) * 0.16 +
      (135000 - 45000) * 0.30 +
      (190000 - 135000) * 0.37 +
      (income - 190000) * 0.45
  )

  base_tax + medicare_levy
}

# ------------------------------------------
# 2. Helper to simulate one (income, reg) pair
# ------------------------------------------
simulate_pattern <- function(income_target, regularity,
                             levy = TRUE,
                             disc_rate = 0.03,
                             horizon = 60,
                             discount = 0.5) {

  time <- 1:horizon

  # Stable: same income each year
  incomes_stable <- rep(income_target, horizon)

  # Volatile: 0 for regularity-1 years, then regularity * income_target
  pattern_vol <- c(rep(0, regularity - 1), income_target * regularity)
  incomes_volatile <- rep(pattern_vol, length.out = horizon)

  # Volatile with discount
  non_taxable_disc <- incomes_volatile / (1/discount)
  taxable_disc    <- incomes_volatile - non_taxable_disc

  # Taxes
  tax_stable         <- tax_function(incomes_stable, include_levy = levy)
  tax_volatile       <- tax_function(incomes_volatile, include_levy = levy)
  tax_volatile_disc  <- tax_function(taxable_disc, include_levy = levy)

  # Discount factors
  df <- 1 / (1 + disc_rate)^(time - 1)

  # Total and PV income for each pattern
  total_income_stable        <- sum(incomes_stable)
  total_income_volatile      <- sum(incomes_volatile)
  total_income_volatile_disc <- sum(taxable_disc + non_taxable_disc)

  PV_income_stable        <- sum(incomes_stable * df)
  PV_income_volatile      <- sum(incomes_volatile * df)
  PV_income_volatile_disc <- sum((taxable_disc + non_taxable_disc) * df)

  # Total and PV tax
  total_tax_stable        <- sum(tax_stable)
  total_tax_volatile      <- sum(tax_volatile)
  total_tax_volatile_disc <- sum(tax_volatile_disc)

  PV_tax_stable        <- sum(tax_stable * df)
  PV_tax_volatile      <- sum(tax_volatile * df)
  PV_tax_volatile_disc <- sum(tax_volatile_disc * df)

  # Build summary table for this (income_target, regularity) combo
  out <- rbindlist(list(
    data.table(
      pattern       = "Stable",
      income_target = income_target,
      regularity    = regularity,
      total_income  = total_income_stable,
      total_tax     = total_tax_stable,
      PV_income     = PV_income_stable,
      PV_tax        = PV_tax_stable
    ),
    data.table(
      pattern       = "Volatile",
      income_target = income_target,
      regularity    = regularity,
      total_income  = total_income_volatile,
      total_tax     = total_tax_volatile,
      PV_income     = PV_income_volatile,
      PV_tax        = PV_tax_volatile
    ),
    data.table(
      pattern       = "Volatile with discount",
      income_target = income_target,
      regularity    = regularity,
      total_income  = total_income_volatile_disc,
      total_tax     = total_tax_volatile_disc,
      PV_income     = PV_income_volatile_disc,
      PV_tax        = PV_tax_volatile_disc
    )
  ))

  # Effective tax rates
  out[, ETR_overall := total_tax / total_income]
  out[, ETR_PV      := PV_tax / PV_income]

  out
}

# ------------------------------------------
# 3. Loop over all income_targets & regularities
# ------------------------------------------

all_results <- rbindlist(
  lapply(income_targets, function(inc) {
    rbindlist(
      lapply(regularities, function(reg) {
        simulate_pattern(
          income_target = inc,
          regularity    = reg,
          levy          = levy,
          disc_rate     = disc_rate,
          horizon       = horizon,
          discount      = discount
        )
      })
    )
  })
)

# Quick check
all_results[]

# ------------------------------------------
# 4. Line plots
# ------------------------------------------

# (A) Overall ETR vs income_target, coloured by pattern, faceted by regularity
ggplot(all_results,
       aes(x = income_target / 1000, y = ETR_overall * 100, colour = pattern)) +
  geom_line() +
  facet_wrap(~ regularity, labeller = label_bquote(reg == .(regularity))) +
  labs_e61(
    title = "Overall effective tax rate by income and regularity",
    x     = "Annual annual income ($'000)",
    y     = "Effective tax rate (%)",
    sources   = c("e61"),
    footnotes = c("ETR is total tax over 60 years divided by total income over 60 years.")
  ) + theme_e61(legend = "bottom")

all_results[total_tax == 0,ETR_overall := 0]

ggplot(all_results[regularity == 10],
       aes(x = income_target / 1000, y = ETR_overall * 100, colour = pattern)) +
  geom_line() +
  labs_e61(
    title = "Overall effective tax rate by average income",
    x     = "Average annual income ($'000)",
    subtitle     = "Effective tax rate (%)",
    sources   = c("e61"),
    footnotes = c("ETR is total tax over 60 years divided by total income over 60 years.",paste0("Capital gain is realised every ","10 ","years. No real income growth occurs."))
  ) +
  plab(c("Regular Wages","Irregular Gains","Discounted Irregular Gains"),x=c(50,50,80),y=c(5,35,15))

save_e61("ETR_comparison_income_CGT.pdf")
save_e61("ETR_comparison_income_CGT.png",res=2,title = "Overall effective tax rate by regularity")

ggplot(all_results[income_target/1000 == 100],
       aes(x = regularity, y = ETR_overall * 100, colour = pattern)) +
  geom_line() +
  labs_e61(
    #title = "Overall effective tax rate by regularity",
    x     = "Years before each realisation",
    subtitle     = "Effective tax rate (%)",
    sources   = c("e61"),
    footnotes = c("ETR is total tax over 60 years divided by total income over 60 years.",paste0("Average annual income is $100,000 in all cases. No real income growth occurs."))
  ) +
  plab(c("Regular Wages","Irregular Gains","Discounted Irregular Gains"),x=c(5,5,8),y=c(25,33,15))


save_e61("ETR_comparison_regularity_CGT.pdf")
save_e61("ETR_comparison_regularity_CGT.svg",title = "Overall effective tax rate by regularity")



# (B) PV of tax vs income_target, coloured by pattern, faceted by regularity
# ggplot(all_results,
#        aes(x = income_target / 1000, y = PV_tax / 1e6, colour = pattern)) +
#   geom_line() +
#   facet_wrap(~ regularity, labeller = label_bquote(reg == .(regularity))) +
#   labs_e61(
#     title = paste0("Present value of tax by income and regularity (r = ", disc_rate * 100, "%)"),
#     x     = "Annual income target ($'000)",
#     y     = "PV of tax ($m)",
#     sources   = c("e61"),
#     footnotes = c("PV over 60 years of tax payments.")
#   ) + theme_e61(legend = "bottom")
#
# # (C) If you’d rather see how things change with regularity for a fixed income,
# #     you can flip the axes:
# ggplot(all_results,
#        aes(x = regularity, y = ETR_overall * 100, colour = pattern, group = pattern)) +
#   geom_line() +
#   facet_wrap(~ income_target, labeller = label_bquote(Income == .(income_target))) +
#   labs_e61(
#     title = "Overall effective tax rate by regularity",
#     x     = "Regularity of capital gain",
#     y     = "Effective tax rate (%)",
#     sources = c("e61")
#   ) + theme_e61(legend = "bottom")



### Check some of the related data

taxpercentiles <- read_excel("taxpercentiles.xlsx")
setDT(taxpercentiles)

taxpercentiles[,":=" (tax_share = total_tax/sum(total_tax),income_share = total_income/sum(total_income))]

share_dt <- melt(taxpercentiles[,.(percentile,tax_share,income_share)],id.vars = "percentile")

ggplot(share_dt,aes(x=percentile,y=value,colour=variable)) + geom_line()

# shapley <- read_csv("Shapley.csv")
# setDT(shapley)
#
# shapley[,Type := fifelse(variable %in% c("medicare","TAX_OFSTS_TOTL_AMT"),"no_HI","HI")]
#
# shapley[,.(sum(share)),by=.(percentile, Type)][Type == "HI"]
# shapley[variable == "Relevant_D"][order(percentile)]
# shapley[variable == "CG"][order(percentile)]





## Main example file for the ETR of bracket creep
## Budget 2026 version for wage income: LITO and WATO are included.
## The $1,000 instant tax deduction is deliberately excluded.

#install.packages("remotes")
#remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

## Note script isn't automated - so when you change dates make sure it is saving the correct plots.

# Load required libraries
library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(tidyverse)
library(data.table)
library(Hmisc)

# Options

levy = FALSE # Include the medicare levy
surcharge = FALSE # Include the medicare surcharge
real_income_growth <- 0.12
avg_earnings_base <- 100000
initial_scale <- "27-28 Budget 2026" # Budget 2026 ongoing wage-income scale: 14% second rate plus WATO.
future_scale <- "27-28 Budget 2026"
pre_budget_2026_scale <- "24-25" # Treatment as at 2026, before Budget 2026-27 changes: 16% second rate and no WATO.
max_inc <- 600000 # Change this for different graphs
inflator_type <- "CPI" # Use CPI if doing an inflation base, use "Income" to do the income distribution comparisons
base_year_label <- "FY26"
budget_year_label <- "FY28"
future_year_label <- "FY38"

# Define the tax function

tax_scale_settings <- function(tax_scale) {
  dplyr::case_when(
    tax_scale == "23-24" ~ "23-24",
    tax_scale == "24-25" ~ "24-25",
    tax_scale == "26-27 Budget 2026" ~ "26-27 Budget 2026",
    tax_scale == "27-28 Budget 2026" ~ "27-28 Budget 2026",
    TRUE ~ NA_character_
  ) -> scale_name

  if (is.na(scale_name)) {
    stop("Unknown tax_scale: ", tax_scale)
  }

  list(
    second_rate = dplyr::case_when(
      scale_name == "23-24" ~ 0.19,
      scale_name == "24-25" ~ 0.16,
      scale_name == "26-27 Budget 2026" ~ 0.15,
      scale_name == "27-28 Budget 2026" ~ 0.14
    ),
    third_rate = ifelse(scale_name == "23-24", 0.325, 0.30),
    third_threshold = ifelse(scale_name == "23-24", 120000, 135000),
    fourth_threshold = ifelse(scale_name == "23-24", 180000, 190000),
    include_wato = scale_name == "27-28 Budget 2026"
  )
}

tax_function <- function(income, include_levy = levy, include_surcharge = surcharge, tax_scale = future_scale) {

  settings <- tax_scale_settings(tax_scale)

  # --- LITO (Low Income Tax Offset) ---
  # Applies to income tax payable (base tax), not Medicare levy / MLS.
  lito <- dplyr::case_when(
    income <= 37500 ~ 700,
    income <= 45000 ~ 700 - 0.05 * (income - 37500),
    income <= 66667 ~ 325 - 0.015 * (income - 45000),
    TRUE            ~ 0
  )

  # Guardrails (numerical safety)
  lito <- pmax(0, lito)

  # --- WATO (Working Australians Tax Offset) ---
  # Budget 2026: non-refundable offset of up to $250 from 2027-28 for work income.
  # This script treats all modelled income as wage income and excludes the $1,000 instant deduction.
  wato <- if (settings$include_wato) {
    ifelse(income > 0, 250, 0)
  } else {
    0
  }

  # --- Medicare Levy phase-in and full ---
  medicare_levy <- if (include_levy) {
    ifelse(
      income <= 45000,
      pmax(0, pmin((income - 27222) * 0.1, income * 0.02)),
      income * 0.02
    )
  } else {
    0
  }

  # --- Medicare Levy Surcharge (MLS) --- Note: Surcharge income base is wrong, so do not use for now
  mls_rate <- if (include_surcharge) dplyr::case_when(
    income <= 101000 ~ 0,
    income <= 118000 ~ 0.01,
    income <= 158000 ~ 0.0125,
    TRUE             ~ 0.015
  ) else 0

  mls_surcharge <- income * mls_rate

  # --- Base tax brackets ---

  if(tax_scale == "23-24"){
  # 2023/24 scale
  base_tax <- dplyr::case_when(
    income <= 18200 ~ 0,
    income <= 45000 ~ (income - 18200) * 0.19,
    income <= 120000 ~ (45000 - 18200) * 0.19 +
      (income - 45000) * 0.325,
    income <= 180000 ~ (45000 - 18200) * 0.19 +
      (120000 - 45000) * 0.325 +
      (income - 120000) * 0.37,
    TRUE             ~ (45000 - 18200) * 0.19 +
      (120000 - 45000) * 0.325 +
      (180000 - 120000) * 0.37 +
      (income - 180000) * 0.45
  )
  } else {
    base_tax <- dplyr::case_when(
      income <= 18200 ~ 0,
      income <= 45000 ~ (income - 18200) * settings$second_rate,
      income <= settings$third_threshold ~ (45000 - 18200) * settings$second_rate +
        (income - 45000) * settings$third_rate,
      income <= settings$fourth_threshold ~ (45000 - 18200) * settings$second_rate +
        (settings$third_threshold - 45000) * settings$third_rate +
        (income - settings$third_threshold) * 0.37,
      TRUE             ~ (45000 - 18200) * settings$second_rate +
        (settings$third_threshold - 45000) * settings$third_rate +
        (settings$fourth_threshold - settings$third_threshold) * 0.37 +
        (income - settings$fourth_threshold) * 0.45
    )
  }

  # --- Apply offsets to base tax only (non-refundable) ---
  income_tax_after_offsets <- pmax(0, base_tax - lito - wato)

  # --- Total tax ---
  total_tax <- income_tax_after_offsets + medicare_levy + mls_surcharge
  return(total_tax)
}


# Define effective tax rate function
etr_function <- function(income, tax_scale = future_scale) {
  tax_function(income, include_levy = levy, include_surcharge = surcharge, tax_scale = tax_scale) / income
}

# Create income sequence
incomes <- seq(1000, max_inc, by = 500)

# Define income inflators from the FY26 base year
if (inflator_type == "CPI"){
  inflator_to_budget_year <- (1 + 0.025)^(2)
  inflator_to_future_year <- (1 + 0.025)^(12)
} else {
  inflator_to_budget_year <- (1 + 0.037)^(2)
  inflator_to_future_year <- (1 + 0.037)^(12) # Use for Figure 2 - the "difference" between the distributions
  }

# Create main data frame
df <- tibble(
  income = incomes,
  income_budget_year = income * inflator_to_budget_year,
  income_future_year = income * inflator_to_future_year,
  etr_pre_budget_2026 = etr_function(income, tax_scale = pre_budget_2026_scale),
  etr_budget_year = etr_function(income_budget_year, tax_scale = initial_scale),
  etr_future_year = etr_function(income_future_year, tax_scale = future_scale)
)

setDT(df)



# Create reference points to annotate
annotate_points <- tibble(
  income = c(avg_earnings_base, avg_earnings_base, avg_earnings_base*(1+real_income_growth)),
  etr = c(
    etr_function(avg_earnings_base * inflator_to_budget_year, tax_scale = initial_scale),
    etr_function(avg_earnings_base * inflator_to_future_year, tax_scale = future_scale),
    etr_function(avg_earnings_base*(1+real_income_growth) * inflator_to_future_year, tax_scale = future_scale)
  ),
  type = c("Budget 2026", "Inflation only", "Nominal income growth")
)

annotate_points <- annotate_points %>%
  mutate(point_color = case_when(
    income == avg_earnings_base & type == "Budget 2026" ~ "red",       # Base-year average earnings in FY28
    income == avg_earnings_base*(1+real_income_growth) ~ "black",     # Future-year average earnings with nominal income growth
    TRUE ~ "blue"                   # Same real income in FY38 from inflation only
  ))
annotate_points <- annotate_points %>%
  mutate(vjust = ifelse(income == 100000 & type == "Budget 2026", 2.5, -2.5))

# footnotes_vec <- c(
#   paste0("Markers show a ", budget_year_label, " worker in ", budget_year_label, " (red), the same real income in ", future_year_label, " from inflation only (blue), and a ", future_year_label, " worker in ", future_year_label, " with nominal income growth (black)."),
#   paste0("All series are plotted against ", base_year_label, "-dollar incomes; labels compare ", budget_year_label, " and ", future_year_label, ". Later-year tax is calculated on nominal incomes inflated from ", base_year_label, "."),
#   "Budget 2026 settings include the 14% second rate and WATO from 2027-28; the $1,000 instant deduction is excluded.",
#   "Dashed line shows the treatment as at 2026, before Budget 2026-27 changes: 16% second rate, LITO, and no WATO."
# )

footnotes_vec <- c(
  paste0("All series are plotted against ", base_year_label, "-dollar incomes; labels compare ", budget_year_label, " and ", future_year_label, ". Assumed inflation rate of 2.5%, and nominal income growth of 3.7%."),
  "Dashed line shows the treatment as at 2026, before Budget 2026-27 changes to the second rate and introduction of WATO."
)

if (levy && surcharge) {
  footnotes_vec <- c(footnotes_vec, "Effective Tax Rate includes Single Medicare Levy and Medicare Levy Surcharge.")
} else if (levy && !surcharge) {
  footnotes_vec <- c(footnotes_vec, "Effective Tax Rate includes Single Medicare Levy but excludes the Medicare Levy Surcharge.")
} else if (!levy && surcharge) {
  footnotes_vec <- c(footnotes_vec, "Effective Tax Rate includes Single Medicare Levy Surcharge but excludes the standard Medicare Levy.")
} else {
  footnotes_vec <- c(footnotes_vec, "Effective Tax Rate excludes Single Medicare Levy and Medicare Levy Surcharge.")
}

if (levy && surcharge) {
  title_vec <- "ETR by Gross Taxable Income - including Medicare Levy"
} else if (levy && !surcharge) {
  title_vec <- "ETR by Gross Taxable Income - including Medicare Levy (ex surcharge)"
} else if (!levy && surcharge) {
  title_vec <- "ETR by Gross Taxable Income - nonsense"
} else {
  title_vec <- "ETR by Gross Taxable Income - excluding Medicare Levy"
}

if (levy && surcharge) {
  save_vec <- "ETR_levy_surcharge"
} else if (levy && !surcharge) {
  save_vec <- "ETR_levy"
} else if (!levy && surcharge) {
  save_vec <- "ETR_surcharge"
} else {
  save_vec <- "ETR"
}


# Generate the plot
ggplot(df, aes(x = income/1000)) +
  geom_line(aes(y = etr_pre_budget_2026*100, colour = "Pre-Budget 2026 treatment", linetype = "Pre-Budget 2026 treatment")) +
  geom_line(aes(y = etr_budget_year*100, colour = "FY28 Budget 2026", linetype = "FY28 Budget 2026")) +
  geom_line(aes(y = etr_future_year*100, colour = "FY38 inflation only", linetype = "FY38 inflation only")) +
  geom_point(data = annotate_points,
             aes(x = income/1000, y = etr*100, shape = type, colour = point_color),
             size = 2.5) +
  geom_text(data = annotate_points,
            aes(x = income / 1000, y = etr * 100,
                label = paste0(round(etr * 100, 1), "%"),
                colour = point_color,
                vjust = vjust),
            size = 2) +
  scale_colour_manual(values = c(
    "Pre-Budget 2026 treatment" = "grey40",
    "FY28 Budget 2026" = palette_e61(3)[1],
    "FY38 inflation only" = palette_e61(3)[3],
    "red" = "red",
    "black" = "black",
    "blue" = "blue"  # Add more if needed
  )) +
  scale_linetype_manual(values = c(
    "Pre-Budget 2026 treatment" = "dashed",
    "FY28 Budget 2026" = "solid",
    "FY38 inflation only" = "solid"
  )) +
  scale_shape_manual(values = c("Budget 2026" = 16, "Inflation only" = 17, "Nominal income growth" = 17)) +
  labs_e61(
    title = title_vec,
    footnotes = footnotes_vec,
    x = paste0("Gross Taxable Income (", base_year_label, "$)"),
    y = "%",
    colour = "Tax Scale",
    linetype = "Tax Scale",
    shape = "Tax Scale"
  ) +
  plab(#c("Effective tax rates in FY25","Effective tax rates in FY35","Average FY25 worker in FY25","Average FY25 worker in FY35","Average FY35 worker in FY35"),
    c(paste0("Effective tax rates in ", budget_year_label), paste0("Effective tax rates in ", future_year_label), paste0(budget_year_label, " worker in ", budget_year_label), paste0(budget_year_label, " worker in ", future_year_label, " (inflation only)"), paste0(future_year_label, " worker in ", future_year_label)),
       x=c(1,1,100,100,100),
       y=c(0.28*100,0.33*100,0.16*100,0.13*100,0.10*100),
       colour=c(palette_e61(3)[1],palette_e61(3)[3],"red","blue","black"),
    size = 2)

save_e61(paste0("Bracket_creep_",save_vec,"_newBudg.png"),res=2,save_data = TRUE,chart_type = "wide",dim = list(width = 15))
save_e61(paste0("Bracket_creep_",save_vec,"_newBudg.svg"),chart_type = "wide")

ggplot(df, aes(x = income/1000)) +
  geom_line(aes(y = etr_pre_budget_2026*100, colour = "Pre-Budget 2026 treatment", linetype = "Pre-Budget 2026 treatment")) +
  geom_line(aes(y = etr_budget_year*100, colour = "FY28 Budget 2026", linetype = "FY28 Budget 2026")) +
  geom_line(aes(y = etr_future_year*100, colour = "FY38 inflation only", linetype = "FY38 inflation only")) +
  geom_point(data = annotate_points,
             aes(x = income/1000, y = etr*100, shape = type, colour = point_color),
             size = 2.5) +
  geom_text(data = annotate_points,
            aes(x = income / 1000, y = etr * 100,
                label = paste0(round(etr * 100, 1), "%"),
                colour = point_color,
                vjust = vjust),
            size = 2) +
  scale_colour_manual(values = c(
    "Pre-Budget 2026 treatment" = "grey40",
    "FY28 Budget 2026" = palette_e61(3)[1],
    "FY38 inflation only" = palette_e61(3)[3],
    "red" = "red",
    "black" = "black",
    "blue" = "blue"  # Add more if needed
  )) +
  scale_linetype_manual(values = c(
    "Pre-Budget 2026 treatment" = "dashed",
    "FY28 Budget 2026" = "solid",
    "FY38 inflation only" = "solid"
  )) +
  scale_shape_manual(values = c("Budget 2026" = 16, "Inflation only" = 17, "Nominal income growth" = 17)) +
  labs_e61(
    #title = title_vec,
    footnotes = footnotes_vec,
    x = paste0("Gross Taxable Income (", base_year_label, "$)"),
    y = "%",
    colour = "Tax Scale",
    linetype = "Tax Scale",
    shape = "Tax Scale"
  ) +
  plab(#c("Effective tax rates in FY25","Effective tax rates in FY35","Average FY25 worker in FY25","Average FY25 worker in FY35","Average FY35 worker in FY35"),
    c(paste0("Effective tax rates in ", budget_year_label), paste0("Effective tax rates in ", future_year_label), paste0(budget_year_label, " worker in ", budget_year_label), paste0(budget_year_label, " worker in ", future_year_label, " (inflation only)"), paste0(future_year_label, " worker in ", future_year_label)),
    x=c(1,1,100,100,100),
    y=c(0.28*100,0.33*100,0.16*100,0.13*100,0.10*100),
    colour=c(palette_e61(3)[1],palette_e61(3)[3],"red","blue","black"),
    size = 2)


save_e61(paste0("Bracket_creep_",save_vec,"_newBudg.pdf"),chart_type = "wide")


df[,diff := etr_future_year - etr_budget_year]
df[,diff_doll := diff*income]


ggplot(df,aes(x=income/1000,y=diff*100)) + geom_line() +
  labs_e61(
    title = paste0("Simulated change in tax rates ", budget_year_label, "-", future_year_label),
    footnotes = footnotes_vec[3],
    x = paste0("Gross Taxable Income (", base_year_label, "$)"),
    y = "%",
    colour = "Tax Scale",
    shape = "Tax Scale"
  )

save_e61(paste0("Bracket_creep_diff_",save_vec,"_newBudg.png"),res=2,save_data = TRUE,chart_type = "wide",dim = list(width = 15))
save_e61(paste0("Bracket_creep_diff_",save_vec,"_newBudg.pdf"),chart_type = "wide")


df[diff < df[income == 100000]$diff & income > 200000]

annotate_points$income[3]

df[income == 112000]

df[diff == max(diff)]

df[income == 100000]

df[income > 120000 & diff < 0.02082169]

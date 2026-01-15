## Main example file for the ETR of bracket creep

#install.packages("remotes")
#remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

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
second_rate = 0.16

# Define the tax function

tax_function <- function(income, include_levy = TRUE, include_surcharge = TRUE) {

  # --- Medicare Levy phase-in and full ---
  medicare_levy <- if (include_levy) {
    ifelse(
      income <= 45000,
      # Phase-in below ~45k
      pmax(0, pmin((income - 27222) * 0.1, income * 0.02)),
      # Full 2% above ~45k
      income * 0.02
    )
  } else {
    0
  }

  # --- Medicare Levy Surcharge (MLS) --- Note: Surcharge income base is wrong, so do not use for now
  mls_rate <- if (include_surcharge) case_when(
    income <= 101000 ~ 0,
    income <= 118000 ~ 0.01,
    income <= 158000 ~ 0.0125,
    TRUE ~ 0.015
  ) else 0

  mls_surcharge <- income * mls_rate

  # --- Base tax brackets ---
  base_tax <- case_when(
    income <= 18200 ~ 0,
    income <= 45000 ~ (income - 18200) * second_rate,
    income <= 135000 ~ (45000 - 18200) * second_rate +
      (income - 45000) * 0.30,
    income <= 190000 ~ (45000 - 18200) * second_rate +
      (135000 - 45000) * 0.30 +
      (income - 135000) * 0.37,
    TRUE             ~ (45000 - 18200) * second_rate +
      (135000 - 45000) * 0.30 +
      (190000 - 135000) * 0.37 +
      (income - 190000) * 0.45
  )

  # --- Total tax ---
  total_tax <- base_tax + medicare_levy + mls_surcharge

  return(total_tax)
}


# Define effective tax rate function
etr_function <- function(income) {
  tax_function(income, include_levy = levy, include_surcharge = surcharge) / income
}

# Create income sequence
incomes <- seq(1000, 200000, by = 500)

# Define deflator (1 + 0.025)^10
deflator <- (1 + 0.286)

# Create main data frame
df <- tibble(
  income = incomes,
  etr_nominal = etr_function(income),
  etr_deflated = etr_function(income * deflator)
)

setDT(df)

real_income_growth <- 0.11
avg_earnings_25 <- 100000

# Create reference points to annotate
annotate_points <- tibble(
  income = c(avg_earnings_25, avg_earnings_25, avg_earnings_25*(1+real_income_growth)),
  etr = c(
    etr_function(avg_earnings_25),
    etr_function(avg_earnings_25 * deflator),
    etr_function(avg_earnings_25*(1+real_income_growth) * deflator)
  ),
  type = c("Nominal", "Deflated", "Deflated")
)

annotate_points <- annotate_points %>%
  mutate(point_color = case_when(
    income == avg_earnings_25 & type == "Deflated" ~ "red",       # FY25 average earnings, deflated
    income == avg_earnings_25*(1+real_income_growth) ~ "black",     # FY35 average earnings
    TRUE ~ "blue"                   # FY25 at that year
  ))
annotate_points <- annotate_points %>%
  mutate(vjust = ifelse(income == 100000 & type == "Nominal", 2.5, -2.5))

footnotes_vec <- c(
  "Markers for Avg Earnings in FY25 (red), FY35 (black) and extra (blue)",
  "Deflated line assumes 2.5% inflation for 10 years"
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
  geom_line(aes(y = etr_nominal*100, colour = "Nominal")) +
  geom_line(aes(y = etr_deflated*100, colour = "Deflated")) +
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
    "Nominal" = palette_e61(3)[1],
    "Deflated" = palette_e61(3)[3],
    "red" = "red",
    "black" = "black",
    "blue" = "blue"  # Add more if needed
  )) +
  scale_shape_manual(values = c("Nominal" = 16, "Deflated" = 17)) +
  labs_e61(
    title = title_vec,
    footnotes = footnotes_vec,
    x = "Gross Taxable Income (FY25$)",
    y = "%",
    colour = "Tax Scale",
    shape = "Tax Scale"
  ) +
  plab(#c("Effective tax rates in FY25","Effective tax rates in FY35","Average FY25 worker in FY25","Average FY25 worker in FY35","Average FY35 worker in FY35"),
    c("Effective tax rates in FY25","Effective tax rates in FY35","FY25 worker in FY25","FY25 worker in FY35","FY35 worker in FY35"),
       x=c(1,1,100,100,100),
       y=c(0.28*100,0.33*100,0.16*100,0.13*100,0.10*100),
       colour=c(palette_e61(3)[1],palette_e61(3)[3],"blue","red","black"),
    size = 2)

#save_e61(paste0("Bracket_creep_",save_vec,".png"),res=2,save_data = TRUE)
#save_e61(paste0("Bracket_creep_",save_vec,".svg"))


df[,diff := etr_deflated - etr_nominal]
df[,diff_doll := diff*income]


ggplot(df,aes(x=income/1000,y=diff)) + geom_line()

ggplot(df,aes(x=income/1000,y=diff_doll)) + geom_line()

# change_dt <- melt(df[,.(income,diff,diff_doll)],id.vars = "income")
#
# ggplot(change_dt,aes(x=income/1000,y=value)) + geom_line() + facet_wrap(~variable)




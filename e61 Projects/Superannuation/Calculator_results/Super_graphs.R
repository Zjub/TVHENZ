## Plot from the superannuation calculator for note: https://e61-institute.shinyapps.io/retirement-tester/

# Load required libraries
library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(tidyverse)
library(data.table)
library(Hmisc)
library(data.table)


# Import the datasets we're after

main_dt_c12 <- read_csv("data_full_profile.csv")
setDT(main_dt_c12)

main_dt_c12[chart == "Average cash available"]
main_dt_c12[chart == "Lifecycle income" & series == "Spendable income"]

shock_dt_c12 <- read_csv("data_full_profile_shock.csv")
setDT(shock_dt_c12)

shock_dt_c12[chart == "Average cash available"]
shock_dt_c12[chart == "Lifecycle income" & series == "Spendable income"]


main_dt_c9 <- read_csv("data_full_profile_9.csv")
setDT(main_dt_c9)

main_dt_c9[chart == "Average cash available"]
main_dt_c9[chart == "Lifecycle income" & series == "Spendable income"]

shock_dt_c9 <- read_csv("data_full_profile_shock_9.csv")
setDT(shock_dt_c9)

shock_dt_c9[chart == "Average cash available"]
shock_dt_c9[chart == "Lifecycle income" & series == "Spendable income"]

ggplot(
  data.table(rbind(
    shock_dt_c9[chart == "Lifecycle income" & series == "Spendable income",
                .(age, value_dollars, series = "9%")],
    shock_dt_c12[chart == "Lifecycle income" & series == "Spendable income",
                 .(age, value_dollars, series = "12%")]
  )),
  aes(x = age, y = value_dollars/1000, colour = series)
) +
  geom_line() +
  scale_y_continuous_e61(limits = c(0,120,30)) +
  labs_e61(title = "Earnings profile with an early life shock",
           y="$",
           sources = "e61",
           footnotes = "Single individual with four years out of work.")

save_e61("Shock.png",res=2)

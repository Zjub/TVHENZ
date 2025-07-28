### Exploring SIH RR data (addition to Matt M's work).
## Data not stored centrally for security - will need a synthetic version for final paper.
# Author: Matt Nolan
# Date made: 31/12/2024
# Last update: 3/2/2025


library(tidyverse)
library(data.table)
library(theme61)
library(readxl)
library(Hmisc)
library(flextable)
library(officer)
library(webshot2)

rm(list=ls())
gc()

#webshot2::install_phantomjs()

# Replace with dataset
#RR_dt <- read_csv("C:/Users/OEM/Downloads/RRs_csv 1.csv")
RR_dt <- read_csv("C:/Users/MattNolan/Downloads/RRs_csv 3.csv")
setDT(RR_dt)

# Set restrictions

RR_dt <- RR_dt[AGEEC > 21 & AGEEC < 54 & hours > 5] # Note, the core note used hours greater than 5, not greater than or equal to.

RR_dt[,":=" (net_inc_ratio = hours0_net_income/current_net_income,grs_inc_ratio = hours0_gross_income/current_gross_income)]

# Here, if there is not capital income just do the straight RR. If gross_income is zero then there is no "taxable benefits" so also set to zero.
# RR_dt[,":=" (grs_RR = (hours0_gross_income-NonWageIncome)/(current_gross_income-NonWageIncome),
#              net_RR = fifelse(NonWageIncome == 0, hours0_net_income/current_net_income,
#                               fifelse(hours0_gross_income == 0,0,
#                          ((hours0_net_income-(NonWageIncome - (NonWageIncome/hours0_gross_income)*hours0_income_tax))/(current_net_income-(NonWageIncome - (NonWageIncome/current_gross_income)*current_income_tax))))))]



RR_dt[,":=" (grs_RR = (hours0_gross_income-NonWageIncome)/(current_gross_income-NonWageIncome),
             net_RR = fifelse(NonWageIncome == 0, hours0_net_income/current_net_income,
                              fifelse(hours0_gross_income == 0,0,
                         ((hours0_net_income + hours0_HECS_payment -(NonWageIncome - (NonWageIncome/(hours0_gross_income))*hours0_income_tax))/(current_net_income + current_HECS_payment -(NonWageIncome - (NonWageIncome/current_gross_income)*current_income_tax))))))]

RR_dt[net_RR == min(RR_dt$net_RR,na.rm=TRUE)]
RR_dt[net_RR < 0]
RR_dt[net_RR < -0.01]

RR_dt[net_RR < 0]$hours0_taxable_benefit

RR_dt[net_RR <0, net_RR := 0] # As the remaining negatives are rounding errors

RR_dt[is.na(net_RR)]

RR_dt[is.na(net_RR) & hours0_gross_income > 0] # Confirms the NA only happens when gross income is negative.

RR_dt <- RR_dt[!is.na(net_RR)]

RR_dt_net <- RR_dt[,.(net_inc_ratio,net_RR)]
RR_dt_grs <- RR_dt[,.(grs_inc_ratio,grs_RR)]


ggplot(melt(RR_dt_net),aes(x=value,colour=variable)) + geom_density() + theme_e61(legend = "bottom")
ggplot(melt(RR_dt_grs),aes(x=value,colour=variable)) + geom_density() + theme_e61(legend = "bottom")

ggplot(melt(RR_dt_net), aes(x=variable, y=value, fill=variable)) + 
  geom_boxplot() + 
  coord_flip() + 
  theme_e61(legend = "bottom")

## Further cuts

# Create weighted quartiles of RRs
# Make equivalised income - we will count all dependents, not just those 14 and below.
RR_dt[, net_RR_forq := net_RR + runif(.N, -1e-6, 1e-6)]

RR_dt[, RR_quartile := cut(
  net_RR_forq,
  breaks = Hmisc::wtd.quantile(net_RR_forq, weights = SIHPSWT, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
  labels = c("1st Quartile", "2nd Quartile", "3rd Quartile", "4th Quartile"),
  include.lowest = TRUE
)]

RR_dt[,.(income = mean(current_gross_income),cap_inc = mean(NonWageIncome),existing_benefits = mean(current_taxable_benefit)),by=.(RR_quartile)][order(RR_quartile)]

# Make the RR groups.

RR_dt[,hhld_inc := current_gross_income + partner_earnings/52.2][,OECD_scale := 1 + 0.5*partnered + 0.3*Numb_dep][,eqv_hhld_gross_inc := hhld_inc/OECD_scale]

RR_dt[,group := fcase(net_RR == 0,1,
                      net_RR < 0.25, 2,
                      net_RR < 0.5, 3,
                      net_RR < 0.75, 4,
                      default = 5)]
# Summary stats
RR_dt[,.(income = mean(current_gross_income),cap_inc = mean(NonWageIncome),existing_benefits = mean(current_taxable_benefit),age=mean(AGEEC),hours_worked = mean(hours),kids = mean(Numb_dep),hhld_eq_ginc = mean(eqv_hhld_gross_inc),L_Asset = mean(LiquidAssets_Household),.N),by=.(group)][order(group)]

RR_dt[, .(
  income = weighted.mean(current_gross_income, weights = SIHPSWT, na.rm = TRUE),
  cap_inc = weighted.mean(NonWageIncome, weights = SIHPSWT, na.rm = TRUE),
  existing_benefits = weighted.mean(current_taxable_benefit, weights = SIHPSWT, na.rm = TRUE),
  age = weighted.mean(AGEEC, weights = SIHPSWT, na.rm = TRUE),
  hours_worked = weighted.mean(hours, weights = SIHPSWT, na.rm = TRUE),
  kids = weighted.mean(Numb_dep, weights = SIHPSWT, na.rm = TRUE),
  hhld_eq_ginc = weighted.mean(eqv_hhld_gross_inc, weights = SIHPSWT, na.rm = TRUE),
  L_Asset = weighted.mean(LiquidAssets_Household, weights = SIHPSWT, na.rm = TRUE),
  .N,
  pop = sum(SIHPSWT)
), by = .(group)][order(group)]

table_all <- RR_dt[, .(
  income = weighted.mean(current_gross_income, weights = SIHPSWT, na.rm = TRUE),
  cap_inc = weighted.mean(NonWageIncome, weights = SIHPSWT, na.rm = TRUE),
  existing_benefits = weighted.mean(current_taxable_benefit, weights = SIHPSWT, na.rm = TRUE),
  age = weighted.mean(AGEEC, weights = SIHPSWT, na.rm = TRUE),
  hours_worked = weighted.mean(hours, weights = SIHPSWT, na.rm = TRUE),
  kids = weighted.mean(Numb_dep, weights = SIHPSWT, na.rm = TRUE),
  hhld_eq_ginc = weighted.mean(eqv_hhld_gross_inc, weights = SIHPSWT, na.rm = TRUE),
  L_Asset = weighted.mean(LiquidAssets_Household, weights = SIHPSWT, na.rm = TRUE),
  .N,
  pop = sum(SIHPSWT)
), by = .(group)][order(group)]

table_all[, group := factor(group, levels = 1:5, labels = c(
  "Ineligible",
  ">0% - <25% RR",
  ">=25% - <50% RR",
  ">=50% - <75% RR",
  "75%+ RR"
))]

table_all_formated <- flextable(table_all) %>%
  set_header_labels(
    group = "Group",
    income = "Income ($)",
    cap_inc = "Capital Income ($)",
    existing_benefits = "Benefits ($)",
    age = "Age (Years)",
    hours_worked = "Hours Worked",
    kids = "Number of Kids",
    hhld_eq_ginc = "Equivalised Gross Income ($)",
    L_Asset = "Liquid Assets ($)",
    N = "Sample Size",
    pop = "Population"
  ) %>%
  colformat_double(j = "income", digits = 2) %>%  
  colformat_double(j = "cap_inc", digits = 2) %>%  
  colformat_double(j = "existing_benefits", digits = 2) %>%  
  colformat_double(j = "age", digits = 1) %>%  
  colformat_double(j = "hours_worked", digits = 1) %>%  
  colformat_double(j = "kids", digits = 1) %>%  
  colformat_double(j = "hhld_eq_ginc", digits = 2) %>%  
  colformat_double(j = "L_Asset", digits = 0) %>%  
  colformat_double(j = "N", digits = 0) %>%  
  colformat_double(j = "pop", digits = 0) %>%  
  autofit() %>%  
  bold(part = "header") %>%
  theme_vanilla() %>%
  add_header_lines("Replacement Rate Groups (all workers)") %>%  # Adds a large, customizable title
  fontsize(i = 1, part = "header", size = 16) %>%  # Increases font size for the first line of the header
  bold(i = 1, part = "header") %>%  # Make the title bold
  align(i = 1, part = "header", align = "center") %>%  # Center the title
  autofit()

table_all_formated

############# Tables to export ----

## Single without Children

table_all_sing <- RR_dt[partnered == 0 & Numb_dep == 0, .(
  income = weighted.mean(current_gross_income, weights = SIHPSWT, na.rm = TRUE),
  cap_inc = weighted.mean(NonWageIncome, weights = SIHPSWT, na.rm = TRUE),
  existing_benefits = weighted.mean(current_taxable_benefit, weights = SIHPSWT, na.rm = TRUE),
  age = weighted.mean(AGEEC, weights = SIHPSWT, na.rm = TRUE),
  hours_worked = weighted.mean(hours, weights = SIHPSWT, na.rm = TRUE),
  #kids = weighted.mean(Numb_dep, weights = SIHPSWT, na.rm = TRUE),
  hhld_eq_ginc = weighted.mean(eqv_hhld_gross_inc, weights = SIHPSWT, na.rm = TRUE),
  L_Asset = weighted.mean(LiquidAssets_Household, weights = SIHPSWT, na.rm = TRUE),
  #.N,
  pop = sum(SIHPSWT)
), by = .(group)][order(group)]

table_all_sing[, group := factor(group, levels = 1:5, labels = c(
  "Ineligible",
  ">0% - <25% RR",
  ">=25% - <50% RR",
  ">=50% - <75% RR",
  "75%+ RR"
))]

table_all_sing_formated <- flextable(table_all_sing) %>%
  set_header_labels(
    group = "Group",
    income = "Prior Income ($)",
    cap_inc = "Capital Income ($)",
    existing_benefits = "Benefits ($)",
    age = "Age (Years)",
    hours_worked = "Prior Hours Worked",
    #kids = "Number of Kids",
    hhld_eq_ginc = "Equivalised Gross Income ($)",
    L_Asset = "Liquid Assets ($)",
    #N = "Sample Size",
    pop = "Population"
  ) %>%
  colformat_double(j = "income", digits = 2) %>%  
  colformat_double(j = "cap_inc", digits = 2) %>%  
  colformat_double(j = "existing_benefits", digits = 2) %>%  
  colformat_double(j = "age", digits = 1) %>%  
  colformat_double(j = "hours_worked", digits = 1) %>%  
  #colformat_double(j = "kids", digits = 1) %>%  
  colformat_double(j = "hhld_eq_ginc", digits = 2) %>%  
  colformat_double(j = "L_Asset", digits = 0) %>%  
  #colformat_double(j = "N", digits = 0) %>%  
  colformat_double(j = "pop", digits = 0) %>%  
  autofit() %>%  
  bold(part = "header") %>%
  theme_vanilla() %>%
  add_header_lines("Replacement Rate Groups (Single no Dependents)") %>%  # Adds a large, customizable title
  fontsize(i = 1, part = "header", size = 16) %>%  # Increases font size for the first line of the header
  bold(i = 1, part = "header") %>%  # Make the title bold
  align(i = 1, part = "header", align = "center") %>%  # Center the title
  autofit()

table_all_sing_formated

# Save the flextable to an HTML file
save_as_html(table_all_sing_formated, path = "table_all_sing.html")

# Convert the HTML to an image
webshot("table_all_sing.html", file = "table_all_sing.png", zoom = 2, vwidth = 1200, vheight = 800)

## Single with Children

table_all_sing_dep <- RR_dt[partnered == 0 & Numb_dep > 0, .(
  income = weighted.mean(current_gross_income, weights = SIHPSWT, na.rm = TRUE),
  cap_inc = weighted.mean(NonWageIncome, weights = SIHPSWT, na.rm = TRUE),
  existing_benefits = weighted.mean(current_taxable_benefit, weights = SIHPSWT, na.rm = TRUE),
  age = weighted.mean(AGEEC, weights = SIHPSWT, na.rm = TRUE),
  hours_worked = weighted.mean(hours, weights = SIHPSWT, na.rm = TRUE),
  kids = weighted.mean(Numb_dep, weights = SIHPSWT, na.rm = TRUE),
  hhld_eq_ginc = weighted.mean(eqv_hhld_gross_inc, weights = SIHPSWT, na.rm = TRUE),
  L_Asset = weighted.mean(LiquidAssets_Household, weights = SIHPSWT, na.rm = TRUE),
  #.N,
  pop = sum(SIHPSWT)
), by = .(group)][order(group)]

table_all_sing_dep[, group := factor(group, levels = 1:5, labels = c(
  "Ineligible",
  ">0% - <25% RR",
  ">=25% - <50% RR",
  ">=50% - <75% RR",
  "75%+ RR"
))]

table_all_sing_dep_formated <- flextable(table_all_sing_dep) %>%
  set_header_labels(
    group = "Group",
    income = "Prior Income ($)",
    cap_inc = "Capital Income ($)",
    existing_benefits = "Benefits ($)",
    age = "Age (Years)",
    hours_worked = "Prior Hours Worked",
    kids = "Number of Kids",
    hhld_eq_ginc = "Equivalised Gross Income ($)",
    L_Asset = "Liquid Assets ($)",
    #N = "Sample Size",
    pop = "Population"
  ) %>%
  colformat_double(j = "income", digits = 2) %>%  
  colformat_double(j = "cap_inc", digits = 2) %>%  
  colformat_double(j = "existing_benefits", digits = 2) %>%  
  colformat_double(j = "age", digits = 1) %>%  
  colformat_double(j = "hours_worked", digits = 1) %>%  
  colformat_double(j = "kids", digits = 1) %>%  
  colformat_double(j = "hhld_eq_ginc", digits = 2) %>%  
  colformat_double(j = "L_Asset", digits = 0) %>%  
  #colformat_double(j = "N", digits = 0) %>%  
  colformat_double(j = "pop", digits = 0) %>%  
  autofit() %>%  
  bold(part = "header") %>%
  theme_vanilla() %>%
  add_header_lines("Replacement Rate Groups (Single with Dependents)") %>%  # Adds a large, customizable title
  fontsize(i = 1, part = "header", size = 16) %>%  # Increases font size for the first line of the header
  bold(i = 1, part = "header") %>%  # Make the title bold
  align(i = 1, part = "header", align = "center") %>%  # Center the title
  autofit()

table_all_sing_dep_formated

# Save the flextable to an HTML file
save_as_html(table_all_sing_dep_formated, path = "table_all_sing_dep.html")

# Convert the HTML to an image
webshot("table_all_sing_dep.html", file = "table_all_sing_dep.png", zoom = 2, vwidth = 1200, vheight = 800)

## Couple no children

table_all_coup <- RR_dt[partnered == 1 & Numb_dep == 0, .(
  income = weighted.mean(current_gross_income, weights = SIHPSWT, na.rm = TRUE),
  cap_inc = weighted.mean(NonWageIncome, weights = SIHPSWT, na.rm = TRUE),
  existing_benefits = weighted.mean(current_taxable_benefit, weights = SIHPSWT, na.rm = TRUE),
  age = weighted.mean(AGEEC, weights = SIHPSWT, na.rm = TRUE),
  hours_worked = weighted.mean(hours, weights = SIHPSWT, na.rm = TRUE),
  #kids = weighted.mean(Numb_dep, weights = SIHPSWT, na.rm = TRUE),
  hhld_eq_ginc = weighted.mean(eqv_hhld_gross_inc, weights = SIHPSWT, na.rm = TRUE),
  L_Asset = weighted.mean(LiquidAssets_Household, weights = SIHPSWT, na.rm = TRUE),
  #.N,
  pop = sum(SIHPSWT)
), by = .(group)][order(group)]

table_all_coup[, group := factor(group, levels = 1:5, labels = c(
  "Ineligible",
  ">0% - <25% RR",
  ">=25% - <50% RR",
  ">=50% - <75% RR",
  "75%+ RR"
))]

table_all_coup_formated <- flextable(table_all_coup) %>%
  set_header_labels(
    group = "Group",
    income = "Prior Income ($)",
    cap_inc = "Capital Income ($)",
    existing_benefits = "Benefits ($)",
    age = "Age (Years)",
    hours_worked = "Prior Hours Worked",
    #kids = "Number of Kids",
    hhld_eq_ginc = "Equivalised Gross Income ($)",
    L_Asset = "Liquid Assets ($)",
    #N = "Sample Size",
    pop = "Population"
  ) %>%
  colformat_double(j = "income", digits = 2) %>%  
  colformat_double(j = "cap_inc", digits = 2) %>%  
  colformat_double(j = "existing_benefits", digits = 2) %>%  
  colformat_double(j = "age", digits = 1) %>%  
  colformat_double(j = "hours_worked", digits = 1) %>%  
  #colformat_double(j = "kids", digits = 1) %>%  
  colformat_double(j = "hhld_eq_ginc", digits = 2) %>%  
  colformat_double(j = "L_Asset", digits = 0) %>%  
  #colformat_double(j = "N", digits = 0) %>%  
  colformat_double(j = "pop", digits = 0) %>%  
  autofit() %>%  
  bold(part = "header") %>%
  theme_vanilla() %>%
  add_header_lines("Replacement Rate Groups (Couple no Dependents)") %>%  # Adds a large, customizable title
  fontsize(i = 1, part = "header", size = 16) %>%  # Increases font size for the first line of the header
  bold(i = 1, part = "header") %>%  # Make the title bold
  align(i = 1, part = "header", align = "center") %>%  # Center the title
  autofit()

table_all_coup_formated

# Save the flextable to an HTML file
save_as_html(table_all_coup_formated, path = "table_all_coup.html")

# Convert the HTML to an image
webshot("table_all_coup.html", file = "table_all_coup.png", zoom = 2, vwidth = 1200, vheight = 800)

## Couple with Children

table_all_coup_dep <- RR_dt[partnered == 1 & Numb_dep > 0, .(
  income = weighted.mean(current_gross_income, weights = SIHPSWT, na.rm = TRUE),
  cap_inc = weighted.mean(NonWageIncome, weights = SIHPSWT, na.rm = TRUE),
  existing_benefits = weighted.mean(current_taxable_benefit, weights = SIHPSWT, na.rm = TRUE),
  age = weighted.mean(AGEEC, weights = SIHPSWT, na.rm = TRUE),
  hours_worked = weighted.mean(hours, weights = SIHPSWT, na.rm = TRUE),
  kids = weighted.mean(Numb_dep, weights = SIHPSWT, na.rm = TRUE),
  hhld_eq_ginc = weighted.mean(eqv_hhld_gross_inc, weights = SIHPSWT, na.rm = TRUE),
  L_Asset = weighted.mean(LiquidAssets_Household, weights = SIHPSWT, na.rm = TRUE),
  #.N,
  pop = sum(SIHPSWT)
), by = .(group)][order(group)]

table_all_coup_dep[, group := factor(group, levels = 1:5, labels = c(
  "Ineligible",
  ">0% - <25% RR",
  ">=25% - <50% RR",
  ">=50% - <75% RR",
  "75%+ RR"
))]

table_all_coup_dep_formated <- flextable(table_all_coup_dep) %>%
  set_header_labels(
    group = "Group",
    income = "Prior Income ($)",
    cap_inc = "Capital Income ($)",
    existing_benefits = "Benefits ($)",
    age = "Age (Years)",
    hours_worked = "Prior Hours Worked",
    kids = "Number of Kids",
    hhld_eq_ginc = "Equivalised Gross Income ($)",
    L_Asset = "Liquid Assets ($)",
    #N = "Sample Size",
    pop = "Population"
  ) %>%
  colformat_double(j = "income", digits = 2) %>%  
  colformat_double(j = "cap_inc", digits = 2) %>%  
  colformat_double(j = "existing_benefits", digits = 2) %>%  
  colformat_double(j = "age", digits = 1) %>%  
  colformat_double(j = "hours_worked", digits = 1) %>%  
  colformat_double(j = "kids", digits = 1) %>%  
  colformat_double(j = "hhld_eq_ginc", digits = 2) %>%  
  colformat_double(j = "L_Asset", digits = 0) %>%  
  #colformat_double(j = "N", digits = 0) %>%  
  colformat_double(j = "pop", digits = 0) %>%  
  autofit() %>%  
  bold(part = "header") %>%
  theme_vanilla() %>%
  add_header_lines("Replacement Rate Groups (Couple with Dependents)") %>%  # Adds a large, customizable title
  fontsize(i = 1, part = "header", size = 16) %>%  # Increases font size for the first line of the header
  bold(i = 1, part = "header") %>%  # Make the title bold
  align(i = 1, part = "header", align = "center") %>%  # Center the title
  autofit()

table_all_coup_dep_formated

# Save the flextable to an HTML file
save_as_html(table_all_coup_dep_formated, path = "table_all_coup_dep.html")

# Convert the HTML to an image
webshot("table_all_coup_dep.html", file = "table_all_coup_dep.png", zoom = 2, vwidth = 1200, vheight = 800)


############## Densities ----

RR_dt[,log_eq := log(eqv_hhld_gross_inc)]
RR_dt[,kid_group := fcase(Numb_dep == 0,"None",
                          Numb_dep == 1,"One",
                          Numb_dep > 1,"Two or more",
                          default = "Error")]

RR_dt[,.N,by=.(kid_group)]

# Unweighted household income
ggplot(RR_dt,aes(x=log(eqv_hhld_gross_inc),colour=as.factor(group))) + geom_density() +
  scale_colour_manual(values = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) +
  scale_y_continuous_e61(limits=c(0,2,0.4)) + 
  labs_e61(title="Density of household income by replacement rate*",subtitle = "All workers, log income",y="",x="",footnotes = c("Equivalised gross income, using the modified OECD equivalence scale")) +
  plab(c("Ineligible","0-25%","25-50%","50-75%","75%+"),x=rep(3.8,times=5),y=c(1.66,1.56,1.46,1.36,1.26),colour=c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))

#Weighted household income
ggplot(RR_dt,aes(x=log(eqv_hhld_gross_inc),colour=as.factor(group))) + 
  #stat_density(aes(weight = SIHPSWT,fill=as.factor(group)), geom = "area", alpha = 0.4) +
  stat_density(aes(weight = SIHPSWT,fill=as.factor(group)), geom = "line", alpha = 0.4) +
  scale_colour_manual(values = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) +
  scale_fill_manual(values = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) +
  scale_y_continuous_e61(limits=c(0,4,0.4)) + 
  labs_e61(title="Density of household income by replacement rate*",subtitle = "All workers, log income",y="",x="",footnotes = c("Equivalised gross income, using the modified OECD equivalence scale")) +
  plab(c("Ineligible","0-25%","25-50%","50-75%","75%+"),x=rep(3.8,times=5),y=c(2.6,2.2,1.8,1.4,1.0),colour=c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))

# Normalised densities

normalized_densities_HI <- RR_dt[!is.na(log_eq)][, {
  d <- density(log_eq, weights = SIHPSWT)
  list(x = d$x, y = d$y / max(d$y))
}, by = group]

# Plot using the pre-calculated normalized densities
ggplot(normalized_densities_HI, aes(x = x, y = y, colour = as.factor(group))) +
  geom_line(size = 1) +
  scale_colour_manual(values = c(palette_e61(5)[1], palette_e61(5)[2], palette_e61(5)[3], palette_e61(5)[4], palette_e61(5)[5])) +
  scale_y_continuous_e61(limits = c(0, 1,0.2)) +  
  labs_e61(
    title = "Density of Household Income by Replacement Rate*",
    subtitle = "All workers, log income",
    y = "",
    x = "",
    footnotes = c("Equivalised gross income, using the modified OECD equivalence scale","Density normalised to show proportional values.")
  ) +
  plab(c("Ineligible", "0-25%", "25-50%", "50-75%", "75%+"), 
       x = rep(9, times = 5), 
       y = c(0.9, 0.7, 0.5, 0.3, 0.1), 
       colour = c(palette_e61(5)[1], palette_e61(5)[2], palette_e61(5)[3], palette_e61(5)[4], palette_e61(5)[5])) +
  scale_x_continuous_e61(limits = c(5,10,1))


save_e61("HHLD_inc_RR.png",res=2,pad_width = 1)

# Unweighted capital income
ggplot(RR_dt,aes(x=log(NonWageIncome),colour=as.factor(group))) + geom_density() +
  scale_colour_manual(values = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) +
  scale_y_continuous_e61(limits=c(0,2,0.4)) + 
  labs_e61(title="Density of capital income by replacement rate*",subtitle = "All workers, log income",y="",x="",footnotes = c("Equivalised gross income, using the modified OECD equivalence scale")) +
  plab(c("Ineligible","0-25%","25-50%","50-75%","75%+"),x=rep(3.8,times=5),y=c(1.66,1.56,1.46,1.36,1.26),colour=c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) +
  scale_x_continuous_e61(limits =c(-2,12,2))

# Weighted capital income
ggplot(RR_dt,aes(x=log(NonWageIncome),colour=as.factor(group))) + 
  #stat_density(aes(weight = SIHPSWT,fill=as.factor(group)), geom = "area", alpha = 0.4) +
  stat_density(aes(weight = SIHPSWT,fill=as.factor(group)), geom = "line", alpha = 0.4) +
  scale_colour_manual(values = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) +
  scale_fill_manual(values = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) +
  scale_y_continuous_e61(limits=c(0,3.2,0.4)) + 
  labs_e61(title="Density of capital income by replacement rate*",subtitle = "All workers, log income",y="",x="",footnotes = c("Equivalised gross income, using the modified OECD equivalence scale")) +
  plab(c("Ineligible","0-25%","25-50%","50-75%","75%+"),x=rep(3.8,times=5),y=c(1.66,1.56,1.46,1.36,1.26),colour=c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) +
  scale_x_continuous_e61(limits =c(-2,12,2))

# Unweighted age
ggplot(RR_dt,aes(x=AGEEC,colour=as.factor(group))) + geom_density() +
  scale_colour_manual(values = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) +
  scale_y_continuous_e61(limits=c(0,0.2,0.04)) + 
  labs_e61(title="Density of household income by replacement rate*",subtitle = "All workers, log income",y="",x="",footnotes = c("Equivalised gross income, using the modified OECD equivalence scale")) +
  plab(c("Ineligible","0-25%","25-50%","50-75%","75%+"),x=rep(3.8,times=5),y=c(0.166,0.156,0.146,0.136,0.126),colour=c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))

# Weighted age
ggplot(RR_dt,aes(x=AGEEC,colour=as.factor(group))) + 
  #stat_density(aes(weight = SIHPSWT,fill=as.factor(group)), geom = "area", alpha = 0.4) +
  stat_density(aes(weight = SIHPSWT,fill=as.factor(group)), geom = "line", alpha = 0.4) +
  scale_colour_manual(values = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) +
  scale_fill_manual(values = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) +
  scale_y_continuous_e61(limits=c(0,0.3,0.04)) + 
  labs_e61(title="Density of age by replacement rate*",subtitle = "All workers, log income",y="",x="",footnotes = c("Equivalised gross income, using the modified OECD equivalence scale")) +
  plab(c("Ineligible","0-25%","25-50%","50-75%","75%+"),x=rep(3.8,times=5),y=c(0.26,0.22,0.18,0.14,0.10),colour=c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))

# normalised age
normalized_densities_age <- RR_dt[, {
  d <- density(AGEEC, weights = SIHPSWT)
  list(x = d$x, y = d$y / max(d$y))
}, by = group]

# Plot using the pre-calculated normalized densities
ggplot(normalized_densities_age, aes(x = x, y = y, colour = as.factor(group))) +
  geom_line(size = 1) +
  scale_colour_manual(values = c(palette_e61(5)[1], palette_e61(5)[2], palette_e61(5)[3], palette_e61(5)[4], palette_e61(5)[5])) +
  scale_y_continuous_e61(limits = c(0, 1,0.2)) +  
  labs_e61(
    title = "Density of Age by Replacement Rate*",
    subtitle = "All workers",
    y = "",
    x = "",
    footnotes = c("Equivalised gross income, using the modified OECD equivalence scale","Density normalised to show proportional values.")
  ) +
  plab(c("Ineligible", "0-25%", "25-50%", "50-75%", "75%+"), 
       x = rep(30, times = 5), 
       y = c(0.31, 0.24, 0.17, 0.10, 0.03), 
       colour = c(palette_e61(5)[1], palette_e61(5)[2], palette_e61(5)[3], palette_e61(5)[4], palette_e61(5)[5])) +
  scale_x_continuous_e61(limits = c(15,55,5))

save_e61("AGE_RR.png",res=2,pad_width = 1)

# Kids histogram

RR_dt[, group2 := factor(group, levels = 1:5, labels = c(
  "Ineligible",
  ">0% - <25% RR",
  ">=25% - <50% RR",
  ">=50% - <75% RR",
  "75%+ RR"
))]

weighted_kids <- RR_dt[,.(pop = sum(SIHPSWT)),by=.(kid_group,group2)]

ggplot(weighted_kids,aes(x=kid_group,y=pop/1000,fill=as.factor(group2))) + geom_col(position="dodge") +
  labs_e61(title = "Replacement Rate by Number of Children",subtitle = "All workers",y="",x="Dependents",sources = c("ABS","e61")) +
  plab(c("Ineligible","0-25%","25-50%","50-75%","75%+"),x=rep(2,times=5),y=c(1450,1350,1250,1150,1050),colour=c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))

save_e61("RR_children.png",res=2,pad_width = 1)

#weighted_kids[, group2b := factor(group2, levels = c(levels(factor(group2)), " "))]

# Make this proportional
ggplot(weighted_kids,aes(x=group2,y=pop/1000,fill=kid_group)) + geom_col(position="fill") +
  labs_e61(title = "Replacement Rate by Number of Children",subtitle = "All workers",y="",x="",sources = c("ABS","e61")) +
  plab(c("No children","One child","Two or more children"),x=c(6.2,6.2,6.2),y=c(0.77,0.52,0.02)) + 
  coord_flip() +   scale_x_discrete(
    limits = c(levels(weighted_kids$group2), " ")  # Add an empty category
  ) + format_flip() +
  scale_y_continuous(labels = scales::percent_format())


# + theme(axis.text.x = element_text(angle = 0, size = 12),axis.ticks.length = unit(0.5, "cm")) + scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 2 == 0, paste0("\n", x), x))

save_e61("RR_children_share.png",res=2,pad_width = 1,auto_scale = FALSE)





## FT

# Summary stats "full time workers"
RR_dt[hours > 30,.(income = mean(current_gross_income),cap_inc = mean(NonWageIncome),existing_benefits = mean(current_taxable_benefit),age=mean(AGEEC),hours_worked = mean(hours),kids = mean(Numb_dep),hhld_eq_ginc = mean(eqv_hhld_gross_inc),L_Asset = mean(LiquidAssets_Household),.N),by=.(group)][order(group)]

table_FT <- RR_dt[hours > 30, .(
  income = weighted.mean(current_gross_income, weights = SIHPSWT, na.rm = TRUE),
  cap_inc = weighted.mean(NonWageIncome, weights = SIHPSWT, na.rm = TRUE),
  existing_benefits = weighted.mean(current_taxable_benefit, weights = SIHPSWT, na.rm = TRUE),
  age = weighted.mean(AGEEC, weights = SIHPSWT, na.rm = TRUE),
  hours_worked = weighted.mean(hours, weights = SIHPSWT, na.rm = TRUE),
  kids = weighted.mean(Numb_dep, weights = SIHPSWT, na.rm = TRUE),
  hhld_eq_ginc = weighted.mean(eqv_hhld_gross_inc, weights = SIHPSWT, na.rm = TRUE),
  L_Asset = weighted.mean(LiquidAssets_Household, weights = SIHPSWT, na.rm = TRUE),
  .N,
  pop = sum(SIHPSWT)
), by = .(group)][order(group)]

table_FT[, group := factor(group, levels = 1:5, labels = c(
  "Ineligible",
  ">0% - <25% RR",
  ">=25% - <50% RR",
  ">=50% - <75% RR",
  "75%+ RR"
))]

table_FT_formated <- flextable(table_FT) %>%
  set_header_labels(
    group = "Group",
    income = "Income ($)",
    cap_inc = "Capital Income ($)",
    existing_benefits = "Benefits ($)",
    age = "Age (Years)",
    hours_worked = "Hours Worked",
    kids = "Number of Kids",
    hhld_eq_ginc = "Equivalised Gross Income ($)",
    L_Asset = "Liquid Assets ($)",
    N = "Sample Size",
    pop = "Population"
  ) %>%
  colformat_double(j = "income", digits = 2) %>%  
  colformat_double(j = "cap_inc", digits = 2) %>%  
  colformat_double(j = "existing_benefits", digits = 2) %>%  
  colformat_double(j = "age", digits = 1) %>%  
  colformat_double(j = "hours_worked", digits = 1) %>%  
  colformat_double(j = "kids", digits = 1) %>%  
  colformat_double(j = "hhld_eq_ginc", digits = 2) %>%  
  colformat_double(j = "L_Asset", digits = 0) %>%  
  colformat_double(j = "N", digits = 0) %>%  
  colformat_double(j = "pop", digits = 0) %>%  
  autofit() %>%  
  bold(part = "header") %>%
  theme_vanilla() %>%
  add_header_lines("Replacement Rate Groups (FT (32+ hours) workers)") %>%  # Adds a large, customizable title
  fontsize(i = 1, part = "header", size = 16) %>%  # Increases font size for the first line of the header
  bold(i = 1, part = "header") %>%  # Make the title bold
  align(i = 1, part = "header", align = "center") %>%  # Center the title
  autofit()

table_FT_formated

# Unweighted household income
ggplot(RR_dt[hours > 30],aes(x=log(eqv_hhld_gross_inc),colour=as.factor(group))) + geom_density() +
  scale_colour_manual(values = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) +
  scale_y_continuous_e61(limits=c(0,2,0.4)) + 
  labs_e61(title="Density of household income by replacement rate*",subtitle = "FT workers, log income",y="",x="",footnotes = c("Equivalised gross income, using the modified OECD equivalence scale")) +
  plab(c("Ineligible","0-25%","25-50%","50-75%","75%+"),x=rep(3.8,times=5),y=c(1.66,1.56,1.46,1.36,1.26),colour=c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))

#Weighted household income
ggplot(RR_dt[hours > 30],aes(x=log(eqv_hhld_gross_inc),colour=as.factor(group))) + 
  #stat_density(aes(weight = SIHPSWT,fill=as.factor(group)), geom = "area", alpha = 0.4) +
  stat_density(aes(weight = SIHPSWT,fill=as.factor(group)), geom = "line", alpha = 0.4) +
  scale_colour_manual(values = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) +
  scale_fill_manual(values = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) +
  scale_y_continuous_e61(limits=c(0,4,0.4)) + 
  labs_e61(title="Density of household income by replacement rate*",subtitle = "FT workers, log income",y="",x="",footnotes = c("Equivalised gross income, using the modified OECD equivalence scale")) +
  plab(c("Ineligible","0-25%","25-50%","50-75%","75%+"),x=rep(3.8,times=5),y=c(2.6,2.2,1.8,1.4,1.0),colour=c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))

# Normalised densities

normalized_densities_FT_HI <- RR_dt[hours > 30 & !is.na(log_eq)][, {
  d <- density(log_eq, weights = SIHPSWT)
  list(x = d$x, y = d$y / max(d$y))
}, by = group]

# Plot using the pre-calculated normalized densities
ggplot(normalized_densities_FT_HI, aes(x = x, y = y, colour = as.factor(group))) +
  geom_line(size = 1) +
  scale_colour_manual(values = c(palette_e61(5)[1], palette_e61(5)[2], palette_e61(5)[3], palette_e61(5)[4], palette_e61(5)[5])) +
  scale_y_continuous_e61(limits = c(0, 1,0.2)) +  
  labs_e61(
    title = "Density of Household Income by Replacement Rate*",
    subtitle = "FT workers, log income",
    y = "",
    x = "",
    footnotes = c("Equivalised gross income, using the modified OECD equivalence scale","Density normalised to show proportional values.")
  ) +
  plab(c("Ineligible", "0-25%", "25-50%", "50-75%", "75%+"), 
       x = rep(9, times = 5), 
       y = c(0.9, 0.7, 0.5, 0.3, 0.1), 
       colour = c(palette_e61(5)[1], palette_e61(5)[2], palette_e61(5)[3], palette_e61(5)[4], palette_e61(5)[5])) +
  scale_x_continuous_e61(limits = c(5,10,1))


save_e61("HHLD_inc_RR_FT.png",res=2,pad_width = 1)

# normalised age
normalized_densities_age_FT <- RR_dt[hours > 30, {
  d <- density(AGEEC, weights = SIHPSWT)
  list(x = d$x, y = d$y / max(d$y))
}, by = group]

# Plot using the pre-calculated normalized densities
ggplot(normalized_densities_age_FT, aes(x = x, y = y, colour = as.factor(group))) +
  geom_line(size = 1) +
  scale_colour_manual(values = c(palette_e61(5)[1], palette_e61(5)[2], palette_e61(5)[3], palette_e61(5)[4], palette_e61(5)[5])) +
  scale_y_continuous_e61(limits = c(0, 1,0.2)) +  
  labs_e61(
    title = "Density of Age by Replacement Rate*",
    subtitle = "FT workers",
    y = "",
    x = "",
    footnotes = c("Equivalised gross income, using the modified OECD equivalence scale","Density normalised to show proportional values.")
  ) +
  plab(c("Ineligible", "0-25%", "25-50%", "50-75%", "75%+"), 
       x = rep(35, times = 5), 
       y = c(0.31, 0.24, 0.17, 0.10, 0.03), 
       colour = c(palette_e61(5)[1], palette_e61(5)[2], palette_e61(5)[3], palette_e61(5)[4], palette_e61(5)[5])) +
  scale_x_continuous_e61(limits = c(15,55,5))

save_e61("AGE_RR_FT.png",res=2,pad_width = 1)

weighted_kids_FT <- RR_dt[hours > 30,.(pop = sum(SIHPSWT)),by=.(kid_group,group2)]

ggplot(weighted_kids_FT,aes(x=kid_group,y=pop/1000,fill=as.factor(group2))) + geom_col(position="dodge") +
  labs_e61(title = "Replacement Rate by Number of Children",subtitle = "FT workers",y="",x="Dependents",sources = c("ABS","e61")) +
  plab(c("Ineligible","0-25%","25-50%","50-75%","75%+"),x=rep(2,times=5),y=c(1450,1350,1250,1150,1050),colour=c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))
  
  save_e61("RR_children_FT.png",res=2,pad_width = 1)

#weighted_kids[, group2b := factor(group2, levels = c(levels(factor(group2)), " "))]

# Make this proportional
ggplot(weighted_kids_FT,aes(x=group2,y=pop/1000,fill=kid_group)) + geom_col(position="fill") +
  labs_e61(title = "Replacement Rate by Number of Children",subtitle = "FT workers",y="",x="",sources = c("ABS","e61")) +
  plab(c("No children","One child","Two or more children"),x=c(6.2,6.2,6.2),y=c(0.77,0.52,0.02)) + 
  coord_flip() +   scale_x_discrete(
    limits = c(levels(weighted_kids$group2), " ")  # Add an empty category
  ) + format_flip() +
  scale_y_continuous(labels = scales::percent_format())


# + theme(axis.text.x = element_text(angle = 0, size = 12),axis.ticks.length = unit(0.5, "cm")) + scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 2 == 0, paste0("\n", x), x))

save_e61("RR_children_share_FT.png",res=2,pad_width = 1,auto_scale = FALSE)


### Example

hourly_wage <- 25  # Wage per hour
benefit_levels <- c(200, 400)  # Two benefit levels ($200 and $400)
hours_to_abate <- 20  # Full abatement after 30 hours of work

abatement_rates <- benefit_levels / hours_to_abate

example_dt <- CJ(hours_worked = 0:40, benefit = benefit_levels)

example_dt[, abatement_rate := abatement_rates[match(benefit, benefit_levels)]]
example_dt[, take_home_pay := (hours_worked * hourly_wage) + pmax(0, benefit - (abatement_rate * hours_worked))]
example_dt[, benefit_label := factor(benefit, labels = c("$200 benefit", "$400 benefit"))]

ggplot(example_dt, aes(x = hours_worked, y = take_home_pay, colour = benefit_label)) +
  geom_line() +
  geom_point() +
  labs(title = "Take-Home Pay in different systems",
       x = "Hours Worked",
       y = "",
       colour = "Benefit Level") +
  scale_y_continuous_e61(labels = scales::dollar_format(),limits=c(0,1000,200)) +
  plab(c("$200 benefit (40% RR and PTR)","$400 benefit (80% RR and PTR)","$500 earnings at 20 hours"),x=c(1,1,1),y=c(660,740,820),size = 4) +
  geom_vline(xintercept = hours_to_abate) +
  plab(c("EMTR higher with benefit","EMTR unchanged"),x=c(0,25),y=c(100,100),colour = c("red","forestgreen"),size = 4)+
  #plab(c("EMTR higher with benefit","EMTR unchanged","Fiscal cost higher","PTR higher","RR higher"),x=c(0,25,25,25,25),y=c(100,100,310,370,430),colour = c("red","forestgreen","red","red","forestgreen")) +
  plab(c("Fiscal cost higher","PTR higher","RR higher"),x=c(25,25,25),y=c(310,370,430),colour = c("red","red","forestgreen"),geom="label",size = 4) +   annotate("label", x = 26.5, y = 490, label = "At all hours", colour = "black", size = 4.5) +theme(plot.title = element_text(size =20))

#save_e61("example.png",res=20,pad_width = 1)



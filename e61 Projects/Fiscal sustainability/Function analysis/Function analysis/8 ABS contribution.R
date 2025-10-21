## Last update:  29/09/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan
# Calculate the sector contributions across all categories with ABS data - comparison to "OECD_comparison" file.

## Setup ----

library(cli)
library(tidyverse)
library(data.table)
library(theme61)
library(tidyr)
library(readxl)
library(gghighlight)
library(readabs)
library(OECD)
library(jsonlite)
library(httr)
library(Synth)
library(mFilter)
library(dendextend)
library(FactoMineR)
library(factoextra)
library(ggalluvial)
library(pheatmap)
library(gridExtra)
library(TSclust)
library(dtwclust)
library(moments)

rm(list=ls())
gc()

## Import data ----

work = TRUE
expense_only = FALSE
comp_year <- 1999

if (work == TRUE){
  consolidate_dt <- read_csv("C:/Users/MattNolan/Git/TVHENZ/e61 Projects/Fiscal sustainability/Function analysis/Data/abs_gfs_data_clean.csv")
} else{
  consolidate_dt <- read_csv("~/GitHub/TVHENZ/e61 Projects/Fiscal sustainability/Function analysis/Data/abs_gfs_data_clean.csv")
}

unique(consolidate_dt$etf_type_name)

if (expense_only == TRUE){
  consolidated_expenses_dt <- consolidate_dt[etf_type_name == "Revenue and expenses"]
} else {
  consolidated_expenses_dt <- consolidate_dt
}

setDT(consolidated_expenses_dt)

unique(consolidated_expenses_dt$cofog_group_name)

dt <- consolidated_expenses_dt[fin_year %in% c(comp_year,2024)][,.(nom_spend = sum(gov_expenses_mn,na.rm = TRUE)),by=.(cofog_div_name,fin_year)]

totals <- dt[, .(nom_spend = sum(nom_spend)), by = fin_year][
  , cofog_div_name := "Total"]

dt <- rbind(dt, totals)

wide <- dcast(dt, cofog_div_name ~ fin_year, value.var = "nom_spend")
colnames(wide) <- c("cofog_div_name",paste0("a",comp_year),"a2024")


wide[,change := a2024 - get(paste0("a",comp_year))]

wide[,contribution := change/wide[cofog_div_name == "Total"]$change]
wide[, size := get(paste0("a", comp_year)) /
       wide[cofog_div_name == "Total", get(paste0("a", comp_year))]]

wide

ggplot(melt(wide[cofog_div_name != "Total",.(cofog_div_name,contribution = contribution*100,size = size*100)],id.vars = "cofog_div_name"),aes(x=cofog_div_name,y=value,fill=variable)) + geom_col(position = "dodge") + 
  coord_flip() +
  scale_y_continuous_e61(limits = c(0,50,10)) + 
  plab(c(paste0("Contribution to growth in ",comp_year),"Size in "),y=c(20,20),x=c(1.5,2.5)) +
  labs_e61(title = "Contributions to spending growth",
           y="%",
           x="",
           sources = c("e61","ABS"))

save_e61(paste0("ABS_contribution_growth_",comp_year,".png"),res=2)

## Check the post 1999 figure

dt2 <- consolidated_expenses_dt[fin_year %in% c(1999,2002,2024)][,.(nom_spend = sum(gov_expenses_mn,na.rm = TRUE)),by=.(cofog_div_name,fin_year)]

totals2 <- dt2[, .(nom_spend = sum(nom_spend)), by = fin_year][
  , cofog_div_name := "Total"]

dt2 <- rbind(dt2, totals2)
# 
# wide2 <- dcast(dt2, cofog_div_name ~ fin_year, value.var = "nom_spend")
# colnames(wide2) <- c("cofog_div_name","a2014","a2024")
# 
# wide2[,change := a2024 - a2014]
# 
# wide2[,contribution := change/wide2[cofog_div_name == "Total"]$change]
# wide2[,size := a2024/wide2[cofog_div_name == "Total"]$a2024]
# 
# wide2

# Manually make a %GDP

GDP <- read_abs("5206.0")
setDT(GDP)

GDP <- GDP[table_no == "5206001_key_aggregates"]

unique(GDP$series)

# Filter for the required series: GDP, Terms of Trade, and RNGDI
GDP_dt <- GDP[
  date >= as.Date("1980-01-01") & 
    series %in% c(
      "Gross domestic product: Current prices ;"
    ) & 
    series_type == "Original"
]

GDP_dt <- GDP_dt[,.(date,month = as.numeric(month(date)),year = as.numeric(year(date)),value)]
GDP_dt[,fin_year := fcase(month < 7, year,
                          default = year + 1)]

GDP_dt[,.N,by=.(fin_year)]

GDP_fin_year <- GDP_dt[,.(GDP = sum(value)),by=.(fin_year)]

dt2_GDP <- GDP_fin_year[dt2,on=.(fin_year)][,prop := nom_spend/GDP]

dt2_GDP

wideGDP <- dcast(dt2_GDP, cofog_div_name ~ fin_year, value.var = "prop")
colnames(wideGDP) <- c("cofog_div_name","p1999","p2002","p2024")

wideGDP[,change := p2024 - p1999]

wideGDP[,contribution := change/wideGDP[cofog_div_name == "Total"]$change]
wideGDP[,size := p2024/wideGDP[cofog_div_name == "Total"]$p2024]
wideGDP[,size_99 := p1999/wideGDP[cofog_div_name == "Total"]$p1999]

wide[order(contribution)]
wideGDP[order(contribution)]


## Suspect that people have added transactions in non-financial assets into expenses.
# 
# dt_check <- consolidate_dt[fin_year %in% c(1999,2024)][,.(nom_spend = sum(gov_expenses_mn,na.rm = TRUE)),by=.(cofog_div_name,fin_year)]
# 
# totals_check <- dt_check[, .(nom_spend = sum(nom_spend)), by = fin_year][
#   , cofog_div_name := "Total"]
# 
# dt_check <- rbind(dt_check, totals_check)
# 
# wide_check <- dcast(dt_check, cofog_div_name ~ fin_year, value.var = "nom_spend")
# 
# wide_check[,change := `2024` - `1999`]
# 
# wide_check[,contribution := change/wide_check[cofog_div_name == "Total"]$change]
# 
# wide_check

# --- Shapley-style decomposition of Δ(total/GDP) across functions ---
# Build a tidy table with endpoints for each function
# Build endpoints (exclude "Total" row)
endpts <- merge(
  dt2[fin_year == 1999, .(cofog_div_name, a0 = nom_spend)],
  dt2[fin_year == 2024, .(cofog_div_name, a1 = nom_spend)],
  by = "cofog_div_name", all = TRUE
)[cofog_div_name != "Total"]

# Treat missing categories as zeros
endpts[is.na(a0), a0 := 0]
endpts[is.na(a1), a1 := 0]

G0 <- GDP_fin_year[fin_year == 1999, GDP]
G1 <- GDP_fin_year[fin_year == 2024, GDP]
dG <- G1 - G0

endpts[, `:=`(da = a1 - a0,
              abar = 0.5*(a0 + a1))]

# Exact Shapley contribution for each function
endpts[, shapley_contrib :=
         0.5*(da/G0 + da/G1) - abar * (dG/(G0*G1))]

# Sanity check: sums EXACTLY to Δ(A/G)
delta_ratio_total <- endpts[, sum(shapley_contrib)]
A0 <- endpts[, sum(a0)]
A1 <- endpts[, sum(a1)]
delta_ratio_check <- (A1/G1) - (A0/G0)

stopifnot(all.equal(delta_ratio_total, delta_ratio_check, tolerance = 1e-12))

# Percent contributions
endpts[, contribution := shapley_contrib / delta_ratio_total]
endpts[order(-contribution)]

sum(endpts$shapley_contrib)

unique(consolidate_dt$cofog_group_name)

consolidate_dt[cofog_group_name == "100 Sickness and disability"]

### DSP

DSP_cost <- data.table(fin_year=c(2015,2025),DSP_value=c(16.3,23.3))

DSP_cost <- GDP_fin_year[DSP_cost,on=.(fin_year)]

DSP_cost[,ratio := DSP_value/(GDP/1000)]


wideGDP[order(contribution)]

### Understand more about non-financial assets
# 
# GFS_dt <- read_abs(cat_no = "5512.0")


total_exp_nfa <- consolidated_expenses_dt[etf_type_name == "Transactions in non-financial assets"][,.(nom_spend = sum(gov_nfa_mn,na.rm=TRUE)),by=.(fin_year)]

total_expNFA_prop <- GDP_fin_year[total_exp_nfa,on=.(fin_year)][,prop := nom_spend/GDP]

ggplot(total_expNFA_prop,aes(x=fin_year,y=prop)) + geom_line()

## Now I want to add both types of expenses together.

total_all <- consolidated_expenses_dt[,.(nom_spend_total = sum(gov_nfa_mn,na.rm=TRUE) + sum(gov_expenses_mn,na.rm=TRUE),nom_spend_exp = sum(gov_expenses_mn,na.rm=TRUE), nom_spend_cap = sum(gov_nfa_mn,na.rm=TRUE)),by=.(fin_year)]

total_all <- melt(total_all,id.vars = "fin_year")

total_all_prop <- GDP_fin_year[total_all,on=.(fin_year)][,prop := value/GDP]

ggplot(total_all_prop,aes(x=fin_year,y=prop,colour=variable)) + geom_line()


ggplot(total_all_prop[variable != "nom_spend_cap"],aes(x=fin_year,y=prop*100,colour=variable)) + geom_line() +
  scale_y_continuous_e61(limits = c(32,48,4)) +
  labs_e61(title = "Consolidated government expenditure",
           y = "% NGDP",
           sources = c("ABS","e61"),
           footnotes = c("Expenditure includes current expenses and net acquisition of non-financial assets")) + 
  plab(c("Total expenditure","Current expenses"),x=c(2008,2010),y=c(41,33))

save_e61("Base_spending_plot.png",res=2)

(total_all_prop[variable == "nom_spend_exp" & fin_year == 2024]$prop - total_all_prop[variable == "nom_spend_exp" & fin_year == 2004]$prop)/(total_all_prop[variable == "nom_spend_total" & fin_year == 2024]$prop - total_all_prop[variable == "nom_spend_total" & fin_year == 2004]$prop)

## And the front matter plot





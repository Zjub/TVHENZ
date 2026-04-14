# Topic: Fomalising debt projections
# Author: Matt Nolan
# Created: 14/4/2026
# Last edit: 14/4/2026
# Last editor: Matt Nolan

rm(list=ls())

#remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(readxl)
library(tidyverse)
library(data.table)
library(Hmisc)
library(tidysynth)
library(readabs)

### Prior code


#### For debt plots

OECD_debt <- read_csv("Updated OECD gross general debt.csv")
setDT(OECD_debt)


ggplot(OECD_debt[
  `Reference area` == "Australia"
],aes(x= TIME_PERIOD, y= OBS_VALUE)) + geom_line()

exp_proj <- Graph_data <- read_excel("~/GitHub/TVHENZ/e61 Projects/Fiscal sustainability/Report work/Graphs/Graph_data.xlsx", sheet = "Figure_26")

#exp_proj <- read_excel("C:/Users/MattNolan/Git/TVHENZ/e61 Projects/Fiscal sustainability/Report work/Graphs/Graph_data.xlsx",sheet = "Figure_26")

setDT(exp_proj)

exp_proj <- exp_proj[,.(year,series,Consolidated)]


OECD_for_debt <- OECD_debt[`Reference area` == "Australia",.(year = TIME_PERIOD,debt = OBS_VALUE)]

OECD_exp_GDP <- read_excel("table4_gov_exp-gdp.xlsx", 
                           sheet = "exp_%_gpd", range = "B2:BC88")

setDT(OECD_exp_GDP)
setnames(OECD_exp_GDP, c("...1", "...2"), c("country", "level"))

# Collapse by country
# OECD_dt <- OECD_exp_GDP[, lapply(.SD, sum,na.rm=TRUE), by = country, .SDcols = patterns("^\\d{4}$")]
# setDT(OECD_dt)
# 
# OECD_dt[,.(country,`2022`)][order(`2022`)]

exp_dt <- OECD_exp_GDP[country == "Australia" & level != "Local"][,country := NULL]

exp <- melt(exp_dt,id.vars = "level",value.name = "Expenses")

OECD_rev_GDP <- read_excel("table6_gov_rev-gdp.xlsx", 
                           sheet = "gtr_%_gdp", range = "B2:BJ88")

setDT(OECD_rev_GDP)
setnames(OECD_rev_GDP, c("...1", "...2"), c("country", "level"))

rev_dt <- OECD_rev_GDP[country == "Australia" & level != "Local"][,country := NULL]

rev <- melt(rev_dt,id.vars = "level",value.name = "Revenue")

flow_dt <- exp[rev,on=.(level,variable)]

flow_dt[,year := as.numeric(variable) + 1965] # Shift forward by an extra year due to OECD years - replace with six month averaging if you put in report

flow_dt <- flow_dt[,.(Expenses = sum(Expenses,na.rm=TRUE),Revenue = sum(Revenue,na.rm=TRUE)),by=.(year)]

flow_dt <- flow_dt[exp_proj,on=.(year)][,exp_gap := Expenses - Consolidated*100]

flow_dt

####### Added OECD plots 


##------------------------------------------------------##
## Function to project debt for a given revenue rule    ##
##------------------------------------------------------##

project_debt_path <- function(flow_dt, OECD_for_debt,
                              ramp_revenue = TRUE,
                              target_increase = 2,
                              n_ramp = 20,
                              g_nom_assump = 0.045) {
  
  dt <- copy(flow_dt)
  setorder(dt, year)
  setDT(OECD_for_debt)
  setorder(OECD_for_debt, year)
  
  ## --- 2. Adjust projections for Expenses & Revenue ---
  
  last_hist <- dt[series == "Historical"][.N]
  last_gap  <- last_hist$exp_gap
  last_rev  <- last_hist$Revenue
  
  # (i) EXPENSES: Consolidated * 100 + last_gap
  dt[series == "Projection (base)",
     Expenses := Consolidated * 100 + last_gap]
  
  # recompute exp_gap for projections (optional)
  dt[series == "Projection (base)",
     exp_gap := Expenses - Consolidated * 100]
  
  # (ii) REVENUE:
  proj_idx <- dt[series == "Projection (base)", which = TRUE]
  n_proj   <- length(proj_idx)
  
  if (ramp_revenue) {
    # ramp evenly to +target_increase, then flat
    n_ramp_use <- min(n_ramp, n_proj)
    step       <- target_increase / n_ramp_use
    
    dt[proj_idx,
       Revenue := last_rev + pmin(seq_len(.N), n_ramp_use) * step]
  } else {
    # keep revenue flat at last historical level
    dt[proj_idx, Revenue := last_rev]
  }
  
  ## --- 3. Deficit (% of GDP) & nominal growth ---
  
  dt[, deficit_pct := Expenses - Revenue]
  
  base_year <- max(OECD_for_debt$year)
  base_debt <- OECD_for_debt[year == base_year, debt]
  
  dt[year >= base_year, g_nom := g_nom_assump]
  
  ## --- 4. Project debt-to-GDP from base_year onward ---
  
  dt[, debt_pct := NA_real_]
  dt[year == base_year, debt_pct := base_debt]
  
  idx_base <- which(dt$year == base_year)
  
  for (i in idx_base:(nrow(dt) - 1)) {
    d_t <- dt$debt_pct[i]    / 100
    B_t <- dt$deficit_pct[i] / 100
    g_t <- dt$g_nom[i]
    
    if (is.na(g_t) || is.na(d_t) || is.na(B_t)) next
    
    d_next <- (d_t + B_t) / (1 + g_t)
    dt$debt_pct[i + 1] <- d_next * 100
  }
  
  ## --- 5. Append projections to OECD_for_debt ---
  
  proj_debt <- dt[year > base_year,
                  .(year, debt = debt_pct)]
  
  debt_full <- rbind(OECD_for_debt[order(year)],
                     proj_debt,
                     use.names = TRUE)
  
  return(debt_full)
}

##------------------------------------------------------##
## Run two scenarios                                    ##
##------------------------------------------------------##

# 1) With 2ppt revenue increase
debt_with_rev <- project_debt_path(flow_dt, OECD_for_debt,
                                   ramp_revenue = TRUE)
debt_with_rev[, scenario := "Revenue +2ppts"]

# 2) Without revenue increase (flat at last level)
debt_no_rev <- project_debt_path(flow_dt, OECD_for_debt,
                                 ramp_revenue = FALSE)
debt_no_rev[, scenario := "Revenue flat"]

# Combine
debt_both <- rbindlist(list(debt_with_rev, debt_no_rev), use.names = TRUE)

##------------------------------------------------------##
## Plot both paths                                      ##
##------------------------------------------------------##

ggplot(debt_both, aes(x = year, y = debt, colour = scenario)) +
  geom_line() +
  labs(title = "Consolidated debt projections",
       x = "Year",
       y = "Gross debt (% of GDP)",
       sources = c("ABS","OECD","e61"),
       footnotes = c("Bracket Creep is a two-percentage point increase in tax to GDP over 20 years.")) +
  geom_vline(xintercept = 2024,linetype = "dashed") +
  plab(c("Bracket Creep","No Bracket Creep"),x=c(2030,2030),y=c(40,100))

#save_e61("Debt_projections_creep2.svg")

ggplot(debt_both, aes(x = year, y = debt, colour = scenario)) +
  geom_line() +
  labs(#title = "Consolidated debt projections",
    x = "Year",
    y = "Gross debt (% of GDP)",
    sources = c("ABS","OECD","e61"),
    footnotes = c("Bracket Creep is a two-percentage point increase in tax to GDP over 20 years.")) +
  geom_vline(xintercept = 2024,linetype = "dashed") +
  plab(c("Bracket Creep","No Bracket Creep"),x=c(2030,2030),y=c(40,100))

#save_e61("Debt_projections_creep2.pdf")
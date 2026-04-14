# Topic: Fomalising debt projections - used chatgpt to clean up variable definitions, but results unchanged.
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

OECD_debt <- read_csv("Updated OECD gross general debt.csv", show_col_types = FALSE)
setDT(OECD_debt)

ggplot(
  OECD_debt[`Reference area` == "Australia"],
  aes(x = TIME_PERIOD, y = OBS_VALUE)
) +
  geom_line()

exp_proj <- read_excel(
  "~/GitHub/TVHENZ/e61 Projects/Fiscal sustainability/Report work/Graphs/Graph_data.xlsx",
  sheet = "Figure_26"
)

#exp_proj <- read_excel("C:/Users/MattNolan/Git/TVHENZ/e61 Projects/Fiscal sustainability/Report work/Graphs/Graph_data.xlsx",sheet = "Figure_26")

setDT(exp_proj)

exp_proj <- exp_proj[, .(year, series, Consolidated)]
exp_proj[, year := as.numeric(year)]
exp_proj[, Consolidated := as.numeric(Consolidated)]
exp_proj[, series := as.character(series)]

OECD_for_debt <- OECD_debt[
  `Reference area` == "Australia",
  .(year = as.numeric(TIME_PERIOD), debt = as.numeric(OBS_VALUE))
]

OECD_exp_GDP <- read_excel(
  "table4_gov_exp-gdp.xlsx", 
  sheet = "exp_%_gpd",
  range = "B2:BC88"
)

setDT(OECD_exp_GDP)
setnames(OECD_exp_GDP, c("...1", "...2"), c("country", "level"))

exp_dt <- OECD_exp_GDP[
  country == "Australia" & level != "Local"
][, country := NULL]

exp <- melt(
  exp_dt,
  id.vars = "level",
  value.name = "Expenses"
)

OECD_rev_GDP <- read_excel(
  "table6_gov_rev-gdp.xlsx", 
  sheet = "gtr_%_gdp",
  range = "B2:BJ88"
)

setDT(OECD_rev_GDP)
setnames(OECD_rev_GDP, c("...1", "...2"), c("country", "level"))

rev_dt <- OECD_rev_GDP[
  country == "Australia" & level != "Local"
][, country := NULL]

rev <- melt(
  rev_dt,
  id.vars = "level",
  value.name = "Revenue"
)

flow_dt <- exp[rev, on = .(level, variable)]

# Keep the original year logic
flow_dt[, year := as.numeric(variable) + 1965]

flow_dt <- flow_dt[
  ,
  .(
    Expenses = sum(as.numeric(Expenses), na.rm = TRUE),
    Revenue  = sum(as.numeric(Revenue), na.rm = TRUE)
  ),
  by = .(year)
]

flow_dt <- flow_dt[exp_proj, on = .(year)]
flow_dt[, exp_gap := Expenses - Consolidated * 100]

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
  
  debt_hist <- copy(OECD_for_debt)
  setorder(debt_hist, year)
  
  ## Basic checks
  if (dt[series == "Historical", .N] == 0) {
    stop("No rows with series == 'Historical' found in flow_dt.")
  }
  
  if (dt[series == "Projection (base)", .N] == 0) {
    stop("No rows with series == 'Projection (base)' found in flow_dt.")
  }
  
  ## --- 2. Adjust projections for Expenses & Revenue ---
  
  last_hist <- dt[series == "Historical"][.N]
  last_gap  <- last_hist$exp_gap
  last_rev  <- last_hist$Revenue
  
  # (i) EXPENSES: Consolidated * 100 + last_gap
  dt[
    series == "Projection (base)",
    Expenses := Consolidated * 100 + last_gap
  ]
  
  # recompute exp_gap for projections
  dt[
    series == "Projection (base)",
    exp_gap := Expenses - Consolidated * 100
  ]
  
  # (ii) REVENUE:
  proj_idx <- dt[series == "Projection (base)", which = TRUE]
  n_proj   <- length(proj_idx)
  
  if (n_proj == 0) {
    stop("No projection rows found.")
  }
  
  if (ramp_revenue) {
    # ramp evenly to +target_increase, then flat
    n_ramp_use <- min(n_ramp, n_proj)
    step       <- target_increase / n_ramp_use
    
    dt[
      proj_idx,
      Revenue := last_rev + pmin(seq_len(.N), n_ramp_use) * step
    ]
  } else {
    # keep revenue flat at last historical level
    dt[proj_idx, Revenue := last_rev]
  }
  
  ## --- 3. Deficit (% of GDP) & nominal growth ---
  
  dt[, deficit_pct := Expenses - Revenue]
  
  # Preferred anchor: latest debt year
  base_year <- max(debt_hist$year, na.rm = TRUE)
  
  # If latest debt year is not in dt, fall back to latest common year
  if (!(base_year %in% dt$year)) {
    common_years <- intersect(dt$year, debt_hist$year)
    
    if (length(common_years) == 0) {
      stop("No overlapping years between flow_dt and OECD_for_debt.")
    }
    
    base_year <- max(common_years, na.rm = TRUE)
  }
  
  base_debt <- debt_hist[year == base_year, debt]
  
  if (length(base_debt) != 1 || is.na(base_debt)) {
    stop("Could not identify a unique base-year debt observation.")
  }
  
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
  
  ## --- 5. Append projections to historical debt ---
  ## Important: if we had to fall back to an earlier common base year,
  ## splice the historical debt series at that year so projections appear.
  
  hist_part <- debt_hist[year <= base_year, .(year, debt)]
  proj_part <- dt[year > base_year & !is.na(debt_pct), .(year, debt = debt_pct)]
  
  debt_full <- rbind(hist_part, proj_part, use.names = TRUE)
  
  return(debt_full)
}

##------------------------------------------------------##
## Run two scenarios                                    ##
##------------------------------------------------------##

# 1) With 2ppt revenue increase
debt_with_rev <- project_debt_path(
  flow_dt,
  OECD_for_debt,
  ramp_revenue = TRUE,
  target_increase = 2,
  n_ramp = 20,
  g_nom_assump = 0.045
)
debt_with_rev[, scenario := "Revenue +2ppts"]

# 2) Without revenue increase (flat at last level)
debt_no_rev <- project_debt_path(
  flow_dt,
  OECD_for_debt,
  ramp_revenue = FALSE,
  target_increase = 2,
  n_ramp = 20,
  g_nom_assump = 0.045
)
debt_no_rev[, scenario := "Revenue flat"]

# Combine
debt_both <- rbindlist(list(debt_with_rev, debt_no_rev), use.names = TRUE)

##------------------------------------------------------##
## Plot both paths                                      ##
##------------------------------------------------------##

ggplot(debt_both, aes(x = year, y = debt, colour = scenario)) +
  geom_line() +
  labs(
    title = "Consolidated debt projections",
    x = "Year",
    y = "Gross debt (% of GDP)",
    sources = c("ABS","OECD","e61"),
    footnotes = c("Bracket Creep is a two-percentage point increase in tax to GDP over 20 years.")
  ) +
  geom_vline(xintercept = 2024, linetype = "dashed") +
  plab(c("Bracket Creep","No Bracket Creep"), x = c(2030,2030), y = c(40,100))

#save_e61("Debt_projections_creep2.svg")

ggplot(debt_both, aes(x = year, y = debt, colour = scenario)) +
  geom_line() +
  labs(
    #title = "Consolidated debt projections",
    x = "Year",
    y = "Gross debt (% of GDP)",
    sources = c("ABS","OECD","e61"),
    footnotes = c("Bracket Creep is a two-percentage point increase in tax to GDP over 20 years.")
  ) +
  geom_vline(xintercept = 2024, linetype = "dashed") +
  plab(c("Bracket Creep","No Bracket Creep"), x = c(2030,2030), y = c(40,100))

#save_e61("Debt_projections_creep2.pdf")

########### Add in other projections to allow comparison

## OECD 2026 Country Report

OECD_2026_country <- read_excel("OECD_2026_country.xlsx", 
                                sheet = "OECD_proj")

setDT(OECD_2026_country)

proj_all <- OECD_2026_country[debt_no_rev,on=.(year)][,':=' (scenario = NULL,OECD_adj_proj = NULL)]

## Add PBO projections

PBO_NFO <- read_excel("OECD_2026_country.xlsx", 
                                sheet = "PBO")

setDT(PBO_NFO)

graph_proj_all <- melt(PBO_NFO[proj_all,on=.(year)],id.vars = "year")[year <= 2050]

ggplot(graph_proj_all,aes(x=year,y=value,colour=variable)) + geom_line()+
  geom_vline(xintercept = 2024, linetype = "dashed") +
  scale_y_continuous_e61(limits = c(0,120,20)) +
  plab(c("OECD","PBO","e61"),x=c(2026,2026,2026),y=c(90,40,75),colour = c(palette_e61(3)[2],palette_e61(3)[1],palette_e61(3)[3])) +
  labs_e61(title = "Projections of gross debt",
           subtitle = "General government gross debt, % of NGDP",
           sources = c("ABS", "e61", "PBO National Fiscal Outlook", "OECD"))

save_e61("All_debt_projection.svg")





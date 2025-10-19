## Last update:  29/09/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan
# File for some demographic plots

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
library(lubridate)

## Import data

pop_dt <- read_abs(cat_no = "3101.0")
setDT(pop_dt)

unique(pop_dt$table_title)

pop_dt[table_no == "310104"]

pop <- pop_dt[series == "Estimated Resident Population ;  Persons ;  Australia ;",.(date = date, pop = value/1000000)]

ggplot(pop,aes(x=date,y=pop)) + geom_line()

# ---------- 1) Year-ended growth (quarterly) ----------
setorder(pop, date)
pop[, yoy := (pop / shift(pop, 4) - 1) * 100]   # % y/y

# ---------- 2) Plot 1: time series of y/y growth ----------
p_yoy <- ggplot(pop, aes(x = date, y = yoy)) +
  geom_hline(yintercept = 0, linewidth = 0.25) +
  geom_line(linewidth = 0.6) +
  labs_e61(
    title = "Australia: Population Growth (year-ended)",
    subtitle = "Estimated Resident Population (ERP), Persons",
    x = NULL, y = "Percent"
  ) 

print(p_yoy)

# ---------- 3) Annual (Dec) y/y and decade averages ----------
# Use December observations for calendar-year y/y growth
annual_dec <- pop[month(date) == 12L & !is.na(yoy),
                  .(year = year(date), yoy_dec = yoy)]

annual_dec[, decade := paste0(floor(year/10)*10, "s")]
decade_avg <- annual_dec[, .(avg_yoy = mean(yoy_dec, na.rm = TRUE)), by = decade]
decade_avg[, decade := factor(decade, levels = sort(unique(decade)))]

# ---------- 4) Plot 2: decade-average growth (column plot) ----------
p_decade <- ggplot(decade_avg, aes(x = decade, y = avg_yoy)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", avg_yoy)), vjust = -0.2, size = 3.3) +
  labs(
    title = "Australia: Population Growth — Decade Averages",
    subtitle = "Average of December year-ended growth rates within each decade",
    x = NULL, y = "Percent"
  ) +
  ylim(0, max(decade_avg$avg_yoy, na.rm = TRUE) * 1.12)

print(p_decade)

## Include population projections - note these are "June year" projections

projection <- read_csv("Projected population, Australia.csv")
setDT(projection)

projection <- projection[,.(year=YEAR,proj_pop = `Medium series`/1000000)]

pop[,month := month(date)]
June_pop <- pop[month == 6,.(year = year(date),pop = pop)]

data_projected <- merge(June_pop, projection[, .(year, proj_pop)],
      by = "year", all = TRUE)

ggplot(melt(data_projected,id.vars = c("year")),aes(x=year,y=value,colour=variable)) + geom_line()

# data_projected has: year, pop (Actual, in millions), proj_pop (Projection, in millions)
setDT(data_projected)
setorder(data_projected, year)

# Last actual year (for a helpful dashed divider on plots)
last_actual_year <- data_projected[!is.na(pop), max(year)]

# --- 1) Compute growth rates (year-ended, %), separately for Actual & Projection ---
data_projected[
  ,
  `:=`(
    g_act  = (pop      / shift(pop,      1) - 1) * 100,
    g_proj = (proj_pop / shift(proj_pop, 1) - 1) * 100
  )
]

(data_projected[year %in% c(2024)]$pop/data_projected[year %in% c(1994)]$pop)^(1/30)-1

(data_projected[year %in% c(2024)]$pop/data_projected[year %in% c(2014)]$pop)^(1/10)-1

# Long form for plotting growth
growth_long <- melt(
  data_projected[, .(year, `Actual` = g_act, `Projection` = g_proj)],
  id.vars = "year", variable.name = "series", value.name = "growth_pct"
)

# --- 2) Rolling 10-year geometric average (%), separately for Actual & Projection ---
data_projected[
  ,
  `:=`(
    g10_act  = ((pop      / shift(pop,      10))^(1/10) - 1) * 100,
    g10_proj = ((proj_pop / shift(proj_pop, 10))^(1/10) - 1) * 100
  )
]

g10_long <- melt(
  data_projected[, .(year, `Actual` = g10_act, `Projection` = g10_proj)],
  id.vars = "year", variable.name = "series", value.name = "g10_pct"
)

# --- 3) Plot 1: Year-ended growth (line) ---
p_growth <- ggplot(growth_long[!is.na(growth_pct)],
                   aes(x = year, y = growth_pct, colour = series)) +
  geom_hline(yintercept = 0, linewidth = 0.25) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = last_actual_year, linetype = "dashed", linewidth = 0.3) +
  labs_e61(
    title = "Population growth (June year-ended)",
    x = NULL, y = "%",
    sources = c("ABS","e61")
  ) +
  scale_x_continuous_e61(limits = c(1980,2070,15)) +
  plab(c("Actual","Projection"),x=c(1980,2040),y=c(2.2,2.2))
  

print(p_growth)

save_e61("Population_growth.png",res=2)

# --- 4) Plot 2: Rolling 10-year average (columns, dodged) ---
p_g10 <- ggplot(g10_long[!is.na(g10_pct)],
                aes(x = factor(year), y = g10_pct, fill = series)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_hline(yintercept = 0, linewidth = 0.25) +
  labs(
    title = "Population growth — rolling 10-year geometric average",
    subtitle = "Bars show 10-year avg ending in each year (June to June).",
    x = NULL, y = "Percent"
  ) +
  # If there are many years, optionally thin x labels:
  scale_x_discrete(breaks = function(v) v[as.integer(v) %% 2 == 0])

print(p_g10)


#### Add the participation and GDP plots here - they are currently saved with the other work I've done that used them.
# GDP, NNDI - this has already been saved in a separate file in the OECD folder.


### Income tax plot

Income_tax <- read_excel("Income_tax.xlsx")
setDT(Income_tax)

ggplot(melt(Income_tax[year >= 1971,.(year,Ratio_cash_total,Ratio_cash_per)],id.vars = "year"),aes(x=year,y=value*100,colour=variable)) + 
  geom_line() + 
  geom_vline(xintercept = 2025,linetype = "dashed") +
  plab(c("Total","Personal"),x=c(1970,1970),y=c(17,15.5)) +
  labs_e61(title = "Record high income tax",
           y = "% NGDP",
           x= "",
           sources = c("PBO","e61"),
           footnotes = c("Tax receipts from cash accounts."))

save_e61("Income_tax_GDP.png",res=2,auto_scale = FALSE)

## Add plot of demographic adjustment based on Matt M's look into aged care and family benefits

Dem_adj_SP <- read_excel("Dem_adj_SP.xlsx")
setDT(Dem_adj_SP)

ggplot(Dem_adj_SP,aes(x=Year,y=Family)) + geom_line() +
  geom_hline(yintercept = 1) + 
  scale_y_continuous_e61(limits = c(0.9,1.4)) +
  labs_e61(title = "Payments stable")


ggplot(melt(Dem_adj_SP,id.vars = "Year"),aes(x=Year,y=value,colour=variable)) + geom_line() +
  geom_hline(yintercept = 1) + 
  scale_y_continuous_e61(limits = c(0.8,1.6)) +
  labs_e61(title = "Aged care spending not just about ageing",
           y= "Relative to age adjusted GDP share",
           sources = c("ABS","e61"),
           footnotes = c("Plot illustrates the ratio of spending compared to a counterfactual where spending by demographic group remained fixed as a % of GDP.")) +
  plab(c("Aged Care","Family Support"),x=c(2011,1999),y=c(0.9,1.3))

save_e61("Dem_socialpay.png",res=2)

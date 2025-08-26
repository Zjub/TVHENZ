# Topic: Replicate TTPI plot and deflate by income growth
# Author: Matt Nolan
# Created: 25/8/2025
# Last edit: 25/8/2025
# Last editor: Matt Nolan

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

dt <- read_excel("TTPI main table results.xlsx")
setDT(dt)

dt[,net_trans := sum_transfers - sum_tax]

# --- bring in GDP per capita series from ABS ---
# ABS cat. 5206.0 Table 1 has GDP and population
gdp_data <- read_abs(cat_no = "5206.0", tables = "1")
setDT(gdp_data)

gdp_data <- gdp_data[series == "GDP per capita: Chain volume measures ;" & series_type == "Original"][,year := year(date)]

gdp <- gdp_data[,.(gdp_pc = sum(value)),by=.(year)]

g_93 <- gdp[year == 1993]$gdp_pc

gdp[,base := g_93]

gdp[,gdp_index := gdp_pc/base]

dt <- gdp[dt,on=.(year)]

# deflate
dt[, net_trans_real := net_trans / gdp_index]

ggplot(dt[year %in% c(1996,2006,2016,2021)],
       aes(x = age, y = net_trans, colour = factor(year), group = year)) +
  geom_line() +
  labs_e61(
    title = "TTPI plot",
    y = "Net transfers (real)",
    colour = "Year"
  ) + theme_e61(legend = "bottom")


# --- plot ---
ggplot(dt[year %in% c(1996,2006,2016,2021)],
       aes(x = age, y = net_trans_real, colour = factor(year), group = year)) +
  geom_line() +
  labs_e61(
    title = "Relative to real GDP",
    y = "Net transfers (1993 $ GDP-pc deflated)",
    colour = "Year"
  ) + theme_e61(legend = "bottom")


## Switch the deflator - inflate up by CPI and deflate by GDPD.
cpi_raw <- read_abs(cat_no = "6401.0", tables = "1")
setDT(cpi_raw)

cpi_au <- cpi_raw[grepl("Index Numbers ;  All groups CPI ;  Australia ;", series, ignore.case = TRUE)]

cpi_au[,year := year(date)]

cpi <- cpi_au[,.(cpi_ann = mean(value)/100),by=.(year)]

ipd_raw <- read_abs(cat_no = "5206.0", tables = "5")
setDT(ipd_raw)

gdp_ipd <- ipd_raw[series == "GROSS DOMESTIC PRODUCT ;"][,year := year(date)][,.(GDPD = mean(value)/100),by=.(year)]

gdp_ipd

dt <- dt[cpi,on=.(year)][gdp_ipd,on=.(year)][!is.na(net_trans)]

dt[,net_trans_GDPD_PC := net_trans_real*cpi_ann/GDPD]

ggplot(dt[year %in% c(1996,2006,2016,2021)],
       aes(x = age, y = net_trans_GDPD_PC, colour = factor(year), group = year)) +
  geom_line() +
  labs_e61(
    title = "Deflated by GDPD",
    y = "Net transfers (1993 $ nominal GDP-pc deflated)",
    colour = "Year"
  ) + theme_e61(legend = "bottom")

# Use GDI deflator


### Create 4 year blocks

dt_group <- dt[,.(net_trans = weighted.mean(net_trans,population),net_trans_real = weighted.mean(net_trans,population),net_trans_GDPD_PC=weighted.mean(net_trans_GDPD_PC,population)),by=.(grouped_year,age)]

dt_group_unweight <- dt[,.(net_trans = mean(net_trans),net_trans_real = mean(net_trans),net_trans_GDPD_PC=mean(net_trans_GDPD_PC),pop=mean(population)),by=.(grouped_year,age)]


ggplot(dt_group_unweight[grouped_year %in% c("1993/1994-1997/1998","2018/2019-2022/2023")],aes(x=age,y=net_trans,colour=grouped_year)) + geom_line() +theme_e61(legend = "bottom")

ggplot(dt_group[grouped_year %in% c("1993/1994-1997/1998","2018/2019-2022/2023")],aes(x=age,y=net_trans,colour=grouped_year)) + geom_line() +theme_e61(legend = "bottom") + labs_e61(title = "TTPI plot")

ggplot(dt_group[grouped_year %in% c("1993/1994-1997/1998","2018/2019-2022/2023")],aes(x=age,y=net_trans_GDPD_PC,colour=grouped_year)) + geom_line() +theme_e61(legend = "bottom") + labs_e61(title = "Deflated by NGDP_PC")

ggplot(dt_group,aes(x=age,y=net_trans_GDPD_PC,colour=grouped_year)) + geom_line() +theme_e61(legend = "bottom")

ggplot(dt_group_unweight,aes(x=age,y=pop,colour=grouped_year)) + geom_line()  +theme_e61(legend = "bottom")

sel_years <- c("1993/1994-1997/1998","2018/2019-2022/2023")

diff_dt <- melt(
  dt_group[grouped_year %in% sel_years],
  id.vars = c("age","grouped_year"),
  measure.vars = c("net_trans","net_trans_GDPD_PC"),
  variable.name = "measure",
  value.name   = "value"
)[
  , dcast(.SD, age + measure ~ grouped_year, value.var = "value")
][
  , diff := `2018/2019-2022/2023` - `1993/1994-1997/1998`
][]

ggplot(diff_dt[age >= 20 & age <= 80], aes(x = age, y = diff, colour = measure)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(size = 1) +
  labs(
    x = "Age",
    y = "Difference (late − early)",
    colour = "Measure",
    title = "Difference in Net Transfers by Age"
  ) +
  theme_e61(legend = "bottom")

ggplot(diff_dt[age >= 20 & age <= 80], 
       aes(x = age, y = diff, colour = measure)) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "loess", se = FALSE, span = 0.2, size = 1) +
  labs(
    x = "Age",
    y = "Difference (2019/23 − 1994/98)",
    colour = "Measure",
    title = "Difference in Net Transfers by Age (Smoothed)"
  ) +
  theme_e61(legend = "bottom") + 
  geom_vline(xintercept = 65,linetype="dotted")

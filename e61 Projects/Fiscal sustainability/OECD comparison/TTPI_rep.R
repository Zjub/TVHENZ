# Topic: Replicate TTPI plot and deflate by income growth
# Author: Matt Nolan
# Created: 25/8/2025
# Last edit: 1/3/2026
# Last editor: Matt Nolan

# Library management --------------------------------------------

# ## Update packages 
.libPaths()
update.packages(lib.loc = .libPaths()[1], ask = FALSE, checkBuilt = TRUE)

# Libraries
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(tidyverse)
library(data.table)
library(Hmisc)
library(tidysynth)
library(readabs)
library(theme61)

# ------------------------------------------------

dt <- read_excel("TTPI main table results.xlsx")
setDT(dt)

dt[,net_trans := sum_transfers - sum_tax]

# --- bring in GDP per capita series from ABS ---
# ABS cat. 5206.0 Table 1 has GDP and population
gdp_data <- read_abs(cat_no = "5206.0", tables = "1")
setDT(gdp_data)

gdp_data <- gdp_data[series == "GDP per capita: Chain volume measures ;" & series_type == "Original"][,year := year(date)]

gdp <- gdp_data[,.(gdp_pc = sum(value)),by=.(year)]

g_93 <- gdp[year == 2023]$gdp_pc

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
# Table 1 (the old reference) has been changed to monthly
cpi_raw <- read_abs(cat_no = "6401.0", tables = "17")
setDT(cpi_raw)

cpi_au <- cpi_raw[grepl("Index Numbers ;  All groups CPI ;  Australia ;", series, ignore.case = TRUE)]

cpi_au[,year := year(date)]

gdp[,base := g_93]

gdp[,gdp_index := gdp_pc/base]

cpi <- cpi_au[,.(cpi_ann = mean(value)/100),by=.(year)]

cpi_23 <- cpi[year == 2023]$cpi_ann
cpi[,base := cpi_23]
cpi[,cpi_value := cpi_ann/base]

ipd_raw <- read_abs(cat_no = "5206.0", tables = "5")
setDT(ipd_raw)

gdp_ipd <- ipd_raw[series == "GROSS DOMESTIC PRODUCT ;"][,year := year(date)][,.(GDPD = mean(value)/100),by=.(year)]
gdpd_base_year <- if (2023 %in% gdp_ipd$year) 2023 else max(gdp_ipd$year, na.rm = TRUE)
gdpd_base <- gdp_ipd[year == gdpd_base_year]$GDPD
gdp_ipd[,GDPD_index := GDPD/gdpd_base]

gdp_ipd

dt <- dt[cpi,on=.(year)][gdp_ipd,on=.(year)][!is.na(net_trans)]

dt[,ngdp_pc_index := gdp_index*GDPD_index]
dt[,net_trans_GDPD_PC := net_trans*cpi_value/ngdp_pc_index]

ggplot(dt[year %in% c(1996,2006,2016,2021)],
       aes(x = age, y = net_trans_GDPD_PC, colour = factor(year), group = year)) +
  geom_line() +
  labs_e61(
    title = "Deflated by NGDP per capita",
    y = "Net transfers (2023$ rebased by NGDP-pc growth)",
    colour = "Year"
  ) + theme_e61(legend = "bottom")

# Use GDI deflator


### Create 4 year blocks

dt_group <- dt[,.(net_trans = weighted.mean(net_trans,population),net_trans_real = weighted.mean(net_trans_real,population),net_trans_GDPD_PC=weighted.mean(net_trans_GDPD_PC,population)),by=.(grouped_year,age)]

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

ggplot(diff_dt[age >= 20 & age <= 90], 
       aes(x = age, y = diff/1000, colour = measure)) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "loess", se = FALSE, span = 0.2, size = 1) +
  labs_e61(
    x = "Age",
    y = "$(000)",
    colour = "Measure",
    title = "Change in fiscal transfers by Age",
    subtitle = "Intergenerational compact by transfer concept",
    sources = c("Varela, Breunig, and Smith (2025)","TTPI","e61"),
    footnotes = c("Smoothed via LOESS regression using a span of 0.2.","Difference reflects the change between 1994/98 and 2019/23, in FY2023 prices.")
  ) +
  geom_vline(xintercept = 65,linetype="dotted",colour = palette_e61(3)[3]) +
  plab(c("Real Transfers","Relative to national income","Retirement Age"),x=c(20,20,36),y=c(17,12,27)) +
  scale_y_continuous_e61(limits = c(-20,30,10))

save_e61("IG_compact_comparison.svg",auto_scale = FALSE)


#### Add income 

dt[,adj_HS_inc := Haig_simon_income*cpi_value/(GDPD*gdp_index)]

ggplot(dt[year %in% c(1996,2006,2016,2021)],
       aes(x = age, y = Haig_simon_income, colour = factor(year), group = year)) +
  geom_line() +
  labs_e61(
    title = "TTPI plot",
    y = "HS inc (real)",
    colour = "Year"
  ) + theme_e61(legend = "bottom")

ggplot(dt[year %in% c(1996,2006,2016,2021)],
       aes(x = age, y = adj_HS_inc, colour = factor(year), group = year)) +
  geom_line() +
  labs_e61(
    title = "TTPI plot",
    y = "HS inc (GDP adj)",
    colour = "Year"
  ) + theme_e61(legend = "bottom")

dt_norm <- dt[, .(
  age,
  adj_HS_inc_norm = Haig_simon_income / weighted.mean(Haig_simon_income,population)
), by = year]

ggplot(dt_norm[year %in% c(1996,2006,2016,2021)],
       aes(x = age, y = adj_HS_inc_norm, colour = factor(year), group = year)) +
  geom_line() +
  labs_e61(
    title = "HS income (normalised by year mean)",
    y = "HS inc / mean(HS inc, year)",
    colour = "Year"
  ) +
  theme_e61(legend = "bottom")

# This highlights that HS income has risen much strongly across the distribution than nominal GDP. As a result, lets also replicate transfers relative to HS income per person.

dt[,avg_HS := weighted.mean(Haig_simon_income,population),by=.(year)]
dt[,avg_HS_nom := avg_HS*cpi_value]
hs_base_year <- if (2023 %in% dt$year) 2023 else max(dt$year, na.rm = TRUE)
hs_base <- unique(dt[year == hs_base_year,.(avg_HS_nom)])$avg_HS_nom
dt[,net_trans_HS := net_trans*cpi_value*hs_base/avg_HS_nom]

ggplot(dt[year %in% c(1996,2006,2016,2021)],
       aes(x = age, y = net_trans_HS, colour = factor(year), group = year)) +
  geom_line() +
  labs_e61(
    title = "Deflated by HS income",
    y = "Net transfers (2023$ rebased by HS-income growth)",
    colour = "Year"
  ) + theme_e61(legend = "bottom")

dt_group2 <- dt[,.(net_trans = weighted.mean(net_trans,population),net_trans_HS = weighted.mean(net_trans_HS,population),net_trans_GDPD_PC=weighted.mean(net_trans_GDPD_PC,population)),by=.(grouped_year,age)]

sel_years <- c("1993/1994-1997/1998","2018/2019-2022/2023")

diff_dt2 <- melt(
  dt_group2[grouped_year %in% sel_years],
  id.vars = c("age","grouped_year"),
  measure.vars = c("net_trans","net_trans_HS","net_trans_GDPD_PC"),
  variable.name = "measure",
  value.name   = "value"
)[
  , dcast(.SD, age + measure ~ grouped_year, value.var = "value")
][
  , diff := `2018/2019-2022/2023` - `1993/1994-1997/1998`
][]

ggplot(diff_dt2[age >= 20 & age <= 90], 
       aes(x = age, y = diff/1000, colour = measure)) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "loess", se = FALSE, span = 0.2, size = 1) +
  labs_e61(
    x = "Age",
    y = "$(000)",
    colour = "Measure",
    title = "Change in fiscal transfers by Age",
    subtitle = "Intergenerational compact by transfer concept",
    sources = c("Varela, Breunig, and Smith (2025)","TTPI","e61"),
    footnotes = c("Smoothed via LOESS regression using a span of 0.2.","Difference reflects the change between 1994/98 and 2019/23, in FY2023 prices.")
  ) +
  geom_vline(xintercept = 65,linetype="dotted",colour = palette_e61(4)[4]) +
  plab(c("Real Transfers","Relative to HS income","Relative to national income","Retirement Age"),x=c(20,20,20,36),y=c(22,17,12,27)) +
  scale_y_continuous_e61(limits = c(-40,30,10))

save_e61("IG_compact_comparison2.svg",auto_scale = FALSE)
save_e61("IG_compact_comparison2.png",res=2,auto_scale = FALSE)

### Do the same exercise on "final income" as that is the key plot in the TPPI report. The above is looking at both of the sides

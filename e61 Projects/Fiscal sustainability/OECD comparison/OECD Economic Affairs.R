####### Economic Affairs breakdown.
# Add time series, the State vs Federal breakdown - we will want subcategories but this will be done in a separate file with ABS data
# Author: Matt Nolan
# Created: 15/9/2025
# Last edit: 15/9/2025
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

### Conditions

shift_au <- "HALF"   # "NONE", "FORWARD", or "HALF" - Australia's FY ends half way through the given year, so none keeps the data as is, forward shifts it forward a full year, the half splits the years and averages.


### Data


Consol_toG <- read_excel("table22-consolidated-cofog-expenditure-spent-by-approach.xlsx", 
                         sheet = "COFOGexp_%oftotal", skip = 1)

Consol_toGDP <- read_excel("table22-consolidated-cofog-expenditure-spent-by-approach.xlsx", 
                           sheet = "COFOGexp_%GDP", skip = 1)

setDT(Consol_toG)
setDT(Consol_toGDP)

Aus_toGDP <- Consol_toGDP[Country == "Australia"][,':=' (Country = NULL,ISO = NULL, `COFOG Code` = NULL, `Government level code` = NULL,`1995` = NULL,`1996` = NULL,`1997` = NULL)]

Aus_toGDP <- melt(Aus_toGDP, id.vars = c(colnames(Aus_toGDP)[1],colnames(Aus_toGDP)[2]),variable.name = "Year",value.name = "value")

colnames(Aus_toGDP)[1] <- "COFOG_Area"
colnames(Aus_toGDP)[2] <- "Government_level"

Aus_toGDP[, Government_level := fifelse(Government_level %in% c("Local", "State"), 
                                        "Non-Federal", "Federal")]

Aus_toGDP <- Aus_toGDP[, .(value = sum(value,na.rm=TRUE)), 
                       by = .(COFOG_Area, Government_level, Year)]

as.numeric(Aus_toGDP$Year) + 1997

Aus_toGDP[,Government_level := factor(Government_level,levels=c("Federal","Non-Federal"))]

ggplot(Aus_toGDP[COFOG_Area == "Economic Affairs"],aes(x=as.numeric(Year)+1997, y=value*100,fill = as.factor(Government_level))) + scale_fill_manual(values = c(palette_e61(2)[2],palette_e61(2)[1])) +
  geom_col() +
  labs_e61(title = "Economic Affairs ",y="% GDP",
           sources = c("ABS","e61")) +
  scale_x_continuous_e61(limits=c(1997,2023,4),expand_left = 0.02,expand_right = 0.02,hide_first_last = FALSE) +
  plab(c("Federal","Non-Federal"),y=c(8.5,6.5),x=c(1998,1998), colour = c(palette_e61(2)[2],palette_e61(2)[1]))

save_e61("Economic_affairs_type.png",res=2)
save_e61("Economic_affairs_type.svg")

### Cross country comparison

EA_dt <- Consol_toGDP[`COFOG Area` == "Economic Affairs"][,':=' (ISO = NULL, `COFOG Code` = NULL, `Government level code` = NULL,`1995` = NULL,`1996` = NULL,`1997` = NULL)]

EA_CC_dt <- melt(EA_dt,id.vars = c("Country",colnames(EA_dt[,2]),colnames(EA_dt[,3])),variable.name = "Year",value.name = "value")

EA_CC_dt[,Year := as.numeric(Year)+1997]

EA_CC_dt <- EA_CC_dt[,.(value = sum(value,na.rm=TRUE)),by=.(Country,Year)]

if (identical(shift_au, "FORWARD")) {
  # Treat AU FY(t-1/t) as calendar year t  --> shift AU +1 year
  EA_CC_dt[Country == "Australia", year := year + 1L]
  # Optional: trim any AU years now beyond others' max
  max_year <- EA_CC_dt[Country != "Australia", max(year, na.rm = TRUE)]
  EA_CC_dt <- EA_CC_dt[!(Country == "Australia" & year > max_year)]
  
} else if (identical(shift_au, "HALF")) {
  # Calendar-year AU(Y) = 0.5*FY(Y) + 0.5*FY(Y+1)
  EA_CC_dt[Country == "Australia",
               value := 0.5*value + 0.5*shift(value, type = "lag"),
               by = Country]
  # Drop trailing AU year that lacks a lead
  EA_CC_dt <- EA_CC_dt[!(Country == "Australia" & is.na(value))]
  # (Years stay the same; values are blended)
}


ggplot(EA_CC_dt,aes(x=Year,y=value,group=Country))+geom_line()

# Generate average based on position in 1999 quartiles.

min_years <- 5  # require at least 5 years of 1990s data to classify (tweak if needed)

class_base <- EA_CC_dt[Country != "Australia" & Year %between% c(1998, 2008)]
class_stats <- class_base[
  , .(value_mean_90s = mean(value, na.rm = TRUE),
      n_years = sum(!is.na(value))),
  by = Country
][n_years >= min_years]

q1 <- quantile(class_stats$value_mean_90s, 0.25, na.rm = TRUE)
q3 <- quantile(class_stats$value_mean_90s, 0.75, na.rm = TRUE)

class_stats[, tier := fifelse(value_mean_90s <= q1, "low",
                              fifelse(value_mean_90s >= q3, "high", NA_character_))]
classified <- class_stats[!is.na(tier)]

high_countries <- classified[tier == "high", Country]
low_countries  <- classified[tier == "low",  Country]

period_99p <- EA_CC_dt[Year >= 1999]
value_1999 <- period_99p[Year == 1999][,.(value_99 = value,Country)]

period_99p <- period_99p[value_1999,on=.(Country)][,index := value/value_99]

avg_index_by_group <- function(countries) {
  period_99p[Country %in% countries, mean(index, na.rm = TRUE)]
}

avg_high_index <- avg_index_by_group(high_countries)
avg_low_index  <- avg_index_by_group(low_countries)
avg_aus_index  <- period_99p[Country == "Australia", mean(index, na.rm = TRUE)]

cat("High spenders (top quartile, 1990s):\n  ",
    paste(sort(high_countries), collapse = ", "), "\n",
    "Average index (1999+): ", round(avg_high_index, 3), "\n\n", sep = "")

cat("Low spenders (bottom quartile, 1990s):\n  ",
    paste(sort(low_countries), collapse = ", "), "\n",
    "Average index (1999+): ", round(avg_low_index, 3), "\n\n", sep = "")

cat("Australia average index (1999+): ", round(avg_aus_index, 3), "\n\n", sep = "")

# --- 3) Build 1999+ time series for plot: mean 'index' by year --------------------
series_high <- period_99p[Country %in% high_countries,
                          .(index = mean(value, na.rm = TRUE)), by = Year][, series := "High"]
series_low  <- period_99p[Country %in% low_countries,
                          .(index = mean(value, na.rm = TRUE)), by = Year][, series := "Low"]
series_aus  <- period_99p[Country == "Australia",
                          .(index = mean(value, na.rm = TRUE)), by = Year][, series := "Australia"]

plot_df <- rbindlist(list(series_high, series_low, series_aus), use.names = TRUE)
setorder(plot_df, series, Year)

# --- 4) Plot ----------------------------------------------------------------------
p_comp <- ggplot(plot_df, aes(Year, index*100, colour = series)) +
  # shaded areas first so lines plot on top
  annotate("rect", xmin = 2008.5, xmax = 2010.5, ymin = 2, ymax = 9,
           alpha = 0.2, fill = "grey") +
  annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = 2, ymax = 9,
           alpha = 0.2, fill = "grey") +
  geom_line() +
  labs_e61(title = "Economic Affairs across countries",
           x = NULL, y = "% NGDP", sources = c("e61","OECD"),
           footnotes = c("High and low spending countries are based on the top and bottom quartile of spenders on this function in the decade to 2008.","As Australia has a different financial year all values are an average of two consecutive years.")) +
  plab(c("Australia","High","Low"),x=c(1999,1999,1999),y=c(5.2,7,3.2))

print(p_comp)

save_e61("CC_Econ_affairs.png",res=2,auto_scale = FALSE)
save_e61("CC_Econ_affairs.svg",auto_scale = FALSE)



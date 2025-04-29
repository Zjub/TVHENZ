## Last update:  28/04/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan
# Fiscal sustainability plots from OECD

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

rm(list=ls())
gc()

## OECD debt comparisons

data_url <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_FIN_DASH@DF_FIN_DASH_S13,/A.AUS+AUT+BEL+CAN+CHL+COL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA.LES13_FD4+LES1311_FD4.?startPeriod=1996&endPeriod=2023"

# Send the GET request
response <- GET(data_url)

# Check if the request was successful (important due to site changes)
if (response$status_code == 200) {
  # Parse the JSON content
  raw_content <- content(response, "text")
  data <- fromJSON(raw_content, flatten = TRUE)
} else {
  stop("Failed to fetch data from the OECD API")
}

# If successful we extract the data we are after as "result"
data_sets <- data$dataSets
result <- data.frame()

for (var_name in names(data_sets)) {
  if (grepl("series", var_name) && grepl("observations", var_name)) {
    country_index <- sub("series\\.[0-9]+:([0-9]+):.*", "\\1", var_name)
    obs_number <- sub(".*\\.observations\\.(\\d+)", "\\1", var_name)
    debt_index <- sub("series\\.[0-9]+:[0-9]+:([0-9]+):.*", "\\1", var_name)
    series_id <- sub("series\\.\\d+:(.*)\\.observations\\.\\d+", "\\1", var_name)
    observations <- data_sets[[var_name]]
    value <- observations[[1]][1]
    time <- as.numeric(obs_number)
    result <- rbind(result, data.frame(Country = as.numeric(country_index), debt_type = as.numeric(debt_index) ,Time = as.numeric(time), Value = value, SeriesID = series_id))
  }
}

setDT(result)

names(data[[2]])

data[[3]]$dimensions$series$values[[2]] # This is the country index

unique(result$SeriesID)

result[SeriesID == "0:0:0"] # The first number is the country code, the second number if the debt type
result[SeriesID == "0:1:0"]

# result[,year := 2023 - max(Time) + Time,by=.(SeriesID)] # Although intuitive this is wrong - 0 refers to 2009, a 14 refers to 2023, but 15 onwards reflects 1996 onwards!

result[,year := fifelse(Time <= 14, 2009 + Time, 1981 + Time),by=.(SeriesID)]

a <- data[[3]]$dimensions$series$values[[2]]

a <- cbind(a,Country = as.numeric(rownames(a))-1)

setDT(a)

result <- a[result,on=.(Country)]

unique(result$id)

result[id == "AUS"]
result[id == "CHL"]

ggplot(result, aes(x = year, y = Value, group = id)) +
  geom_line(aes(
    colour = ifelse(id == "AUS", "AUS", "Other"),
    size = ifelse(id == "AUS", "AUS", "Other"),
    alpha = ifelse(id == "AUS", 1, 0.4)
  )) +
  scale_colour_manual(values = c("AUS" = "darkgreen", "Other" = "grey")) +
  scale_size_manual(values = c("AUS" = 1, "Other" = 0.2)) +
  scale_alpha_identity() +
  facet_wrap(~debt_type) +
  theme_e61(legend = "bottom")

ggplot(result[debt_type == 0], aes(x = year, y = Value, group = id)) +
  geom_line(aes(
    colour = ifelse(id == "AUS", "AUS", "Other"),
    size = ifelse(id == "AUS", "AUS", "Other"),
    alpha = ifelse(id == "AUS", 1, 0.4)
  )) +
  scale_y_continuous_e61(y_top = FALSE,limits = c(0,310,40)) +
  scale_colour_manual(values = c("AUS" = "darkgreen", "Other" = "grey")) +
  scale_size_manual(values = c("AUS" = 1, "Other" = 0.2)) +
  scale_alpha_identity() +
  labs_e61(title = "Gross debt burden",
           subtitle = "General Government, % of GDP",
           y = "(%)",
           x = "",
           sources = c("ABS","e61"))

ggplot(result[debt_type == 1], aes(x = year, y = Value, group = id)) +
  geom_line(aes(
    colour = ifelse(id == "AUS", "AUS", "Other"),
    size = ifelse(id == "AUS", "AUS", "Other"),
    alpha = ifelse(id == "AUS", 1, 0.4)
  )) +
  scale_y_continuous_e61(y_top = FALSE,limits = c(0,270,40)) +
  scale_colour_manual(values = c("AUS" = "darkgreen", "Other" = "grey")) +
  scale_size_manual(values = c("AUS" = 1, "Other" = 0.2)) +
  scale_alpha_identity() +
  labs_e61(title = "Net debt burden",
           subtitle = "General Government, % of GDP",
           y = "(%)",
           x = "",
           sources = c("ABS","e61"))

ggplot(result[debt_type == 1 & !id %in% c("JPN","GRC")], aes(x = year, y = Value, group = id)) +
  geom_line(aes(
    colour = ifelse(id == "AUS", "AUS", "Other"),
    size = ifelse(id == "AUS", "AUS", "Other"),
    alpha = ifelse(id == "AUS", 1, 0.4)
  )) +
  scale_y_continuous_e61(y_top = FALSE,limits = c(0,180,40)) +
  scale_colour_manual(values = c("AUS" = palette_e61(2)[2], "Other" = "grey")) +
  scale_size_manual(values = c("AUS" = 1, "Other" = 0.2)) +
  scale_alpha_identity() +
  labs_e61(title = "Net debt burden",
           subtitle = "General Government, % of GDP",
           y = "%",
           x = "",
           sources = c("OECD","e61"),
           footnotes = c("Excluding Japan and Greece, who had rates in excess of 200%","General Government includes both Federal, State, and Local governments across countries"))

save_e61("Net_debt.png",res = 2)

result[debt_type == 1 & year == 2023][order(Value)]

## General and central government comparisons
#
# data_url <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_FIN_DASH@DF_FIN_DASH_S13,/A.AUS.LES1311_FD4+LES13_FD4.?startPeriod=1995&endPeriod=2024&dimensionAtObservation=AllDimensions"
#
# # Send the GET request
# response <- GET(data_url)
#
# # Check if the request was successful (important due to site changes)
# if (response$status_code == 200) {
#   # Parse the JSON content
#   raw_content <- content(response, "text")
#   data <- fromJSON(raw_content, flatten = TRUE)
# } else {
#   stop("Failed to fetch data from the OECD API")
# }
#
# # If successful we extract the data we are after as "result"
# data_sets <- data$dataSets
# result_AUS <- data.frame()
#
# for (var_name in names(data_sets)) {
#   if (grepl("series", var_name) && grepl("observations", var_name)) {
#     country_index <- sub("series\\.[0-9]+:([0-9]+):.*", "\\1", var_name)
#     obs_number <- sub(".*\\.observations\\.(\\d+)", "\\1", var_name)
#     debt_index <- sub("series\\.[0-9]+:[0-9]+:([0-9]+):.*", "\\1", var_name)
#     series_id <- sub("series\\.\\d+:(.*)\\.observations\\.\\d+", "\\1", var_name)
#     observations <- data_sets[[var_name]]
#     value <- observations[[1]][1]
#     time <- as.numeric(obs_number)
#     result_AUS <- rbind(result_AUS, data.frame(Country = as.numeric(country_index), debt_type = as.numeric(debt_index) ,Time = as.numeric(time), Value = value, SeriesID = series_id))
#   }
# }


## Add labour force participation graph

data_url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.SAE,DSD_LFS@DF_LFS_INDIC,/ESP+PRT+NOR+NLD+LUX+KOR+JPN+ITA+ISR+IRL+GRC+DEU+FRA+FIN+EST+DNK+CRI+BEL+SWE+USA+GBR+NZL+CAN+AUS.LF_RATE.PT_POP_SUB._T.Y15T64.?startPeriod=1960"

# Send the GET request
response <- GET(data_url)

# Check if the request was successful (important due to site changes)
if (response$status_code == 200) {
  # Parse the JSON content
  raw_content <- content(response, "text")
  data <- fromJSON(raw_content, flatten = TRUE)
} else {
  stop("Failed to fetch data from the OECD API")
}

# If successful we extract the data we are after as "result"
data_sets <- data$dataSets
result_LFP <- data.frame()

for (var_name in names(data_sets)) {
  if (grepl("series", var_name) && grepl("observations", var_name)) {
    series_index <- sub("series\\.(\\d+):.*", "\\1", var_name)
    obs_number <- sub(".*\\.observations\\.(\\d+)", "\\1", var_name)
    series_id <- sub("series\\.\\d+:(.*)\\.observations\\.\\d+", "\\1", var_name)
    observations <- data_sets[[var_name]]
    value <- observations[[1]][1]
    time <- as.numeric(obs_number)
    result_LFP <- rbind(result_LFP, data.frame(Series = as.numeric(series_index), Time = time, Value = value, SeriesID = series_id))
  }
}

setDT(result_LFP)

time_id <- as.data.table(data$structure[[3]]$observation$values)[,Time := 0:63][,.(Time,year = id)]

result_LFP <- time_id[result_LFP,on=.(Time)]

country_id <- data.table(country = data$structure$dimensions$series$values[[1]]$name,Series = 0:23)

result_LFP <- country_id[result_LFP,on=.(Series)]
#result_LFP[, line_size := ifelse(country %in% c("Australia","United States"), 1.5, 0.5)]
#result_LFP[, line_color := ifelse(country == "United States", "red", ifelse(country == "Australia", "blue", "lightgrey"))]

result_LFP[, line_size := ifelse(country %in% c("Australia"), 1, 0.25)]
result_LFP[, line_color := fifelse(country == "Australia", "darkgreen", "lightgrey")]


ggplot(result_LFP[year >= 1990], aes(x = as.numeric(year), y = Value/100, group = country)) +
  geom_line(aes(size = line_size, color = line_color)) +
  scale_size_identity() +
  scale_color_identity() +
  theme_e61() +
  labs_e61(title = "Participation Rates", y = "", x = "")+ scale_y_continuous_e61(labels=scales::percent_format(1)) +
  plot_label(x = c(2000,2000),y=c(0.87,0.83),c("United States","Australia"),colour=c("red","blue")) +
  scale_y_continuous_e61(limits = c(0.5,0.9,0.1),labels=scales::percent_format())

# Now make these rates "PP change from 1990"
initial <- result_LFP[year == 2000][,.(initial = Value,country)]

result2_LFP <- initial[result_LFP,on=.(country)][,change := Value - initial]

ggplot(result2_LFP[year >= 2000], aes(x = as.numeric(year), y = change/100, group = country)) +
  geom_line(aes(size = line_size, color = line_color)) +
  scale_size_identity() +
  scale_color_identity() +
  theme_e61() +
  labs_e61(title = "Participation Rates",subtitle = "Change from 1990", y = "", x = "") +
  geom_hline(yintercept = 0) +
  scale_y_continuous_e61(labels=scales::percent_format(1),limits=c(-0.1,0.2,0.05))

result2_LFP[year == 2023][order(change)]

### OECD consolidated expenditure and revenue

data_url <- "https://sdmx.oecd.org/public/rest/data/OECD.GOV.GIP,DSD_GOV@DF_GOV_PF_YU,1.0/A.AUS.GE+GR.PT_B1GQ...?startPeriod=2007&endPeriod=2023&dimensionAtObservation=AllDimensions"

# Send the GET request
response <- GET(data_url)

# Check if the request was successful (important due to site changes)
if (response$status_code == 200) {
  # Parse the JSON content
  raw_content <- content(response, "text")
  data <- fromJSON(raw_content, flatten = TRUE)
} else {
  stop("Failed to fetch data from the OECD API")
}

# If successful we extract the data we are after as "result"
data_sets <- data$dataSets # Third number determines spending or revenue, eighth number the time period
result_flows <- data.frame()

for (var_name in names(data_sets)) {
  if (grepl("observations", var_name)) {
    obs_code <- sub(".*\\.observations\\.(\\d+)", "\\1", var_name)
    observations <- data_sets[[var_name]]
    value <- observations[[1]][1]
    indicators <- as.numeric(strsplit(sub("observations\\.", "", obs_code), ":")[[1]])
    result_flows <- rbind(result_flows, data.frame(Time = indicators[8], type = indicators[3], Value = value, Series = obs_code))
  }
}

setDT(result_flows)

result_flows

data$structure


result_flows[, year := fcase(Time == 0, 2013,
                              Time == 1, 2014,
                              Time == 2, 2008,
                              Time == 3, 2009,
                              Time == 4, 2010,
                              Time == 5, 2011,
                              Time == 6, 2015,
                              Time == 7, 2016,
                              Time == 8, 2017,
                              Time == 9, 2018,
                              Time == 10, 2019,
                              Time == 11, 2020,
                              Time == 12, 2021,
                              Time == 13, 2022,
                              Time == 14, 2023,
                              Time == 15, 2007,
                              Time == 16, 2012,
                              default = NA)]


result_flows[,type_char := fifelse(type == 0, "Expenditure","Revenue")]

ggplot(result_flows,aes(x=year,y=Value,colour=type_char)) + geom_line() +
  labs_e61(title = "General Government Spending remains high",
           subtitle = "Share of GDP",
           y = "%",
           x = "",
           sources = c("OECD,e61")) +
  scale_y_continuous_e61(limits = c(30,45,3)) +
  plab(c("Revenue","Expenditure"),x=c(2008,2008),y=c(43,40.5))

save_e61("General_govt_x_R.png",res=2)





### Historic tax receipts and forecasts - from PBO


Budget_forecast <- read_excel("Historical budget forecasts - 2024-25 Budget.xlsx",
                                                         sheet = "Table 2", skip = 2)

unique(Budget_forecast$Unit)

tax_data <- Budget_forecast %>%
  filter(Aggregate == "Tax receipts")

tax_data_long <- tax_data %>%
  pivot_longer(
    cols = `1998-99`:`2027-28`,   # all your year columns
    names_to = "year",
    values_to = "value"
  )

setDT(tax_data_long)

tax_data_long$`Budget update`

tax_data_long[, year_end := as.integer(
  ifelse(
    as.integer(substr(year, 6, 7)) < 50,
    2000 + as.integer(substr(year, 6, 7)),
    1900 + as.integer(substr(year, 6, 7))
  )
)]

tax_data_long[, Series := fifelse(`Budget update` == "Historical actuals", "Historical", "Projection")]

b <- tax_data_long[!is.na(value)]

b <- b[!`Budget update` %in%c("2006-07 Budget","2007-08 Budget","2008-09 Budget")]

ggplot(b[year_end >= 2010 & Unit == "% of GDP" & Accounting == "Cash"],aes(x=year_end,y=value, group = `Budget update`, colour = Series)) + geom_line() + scale_colour_manual(values = c(
  "Historical" = "blue",    # or whatever color you want
  "Projection" = "grey70"
))

ggplot(b[year_end >= 2010 & year_end <= 2023 & Unit != "% of GDP" & Accounting == "Cash"],aes(x=year_end,y=value/1000, group = `Budget update`, colour = Series)) + geom_line() + scale_colour_manual(values = c(
  "Historical" = "blue",    # or whatever color you want
  "Projection" = "grey70"
)) + scale_y_continuous_e61(limits = c(200,700,100)) +
  labs_e61(y= "",x="",title = "One off tax take surprise")

save_e61("Tax_projection_level.png",res=2)

ggplot(b[year_end >= 2010 & year_end <= 2023 & Unit == "% of GDP" & Accounting == "Cash"],aes(x=year_end,y=value, group = `Budget update`, colour = Series)) + geom_line() + scale_colour_manual(values = c(
  "Historical" = "blue",    # or whatever color you want
  "Projection" = "grey70"
)) + scale_y_continuous_e61() +
  labs_e61(y= "",x="",title = "One off tax take surprise")


ggplot(b[year_end >= 2010 & Unit == "% of GDP" & Accounting != "Cash"],aes(x=year_end,y=value, group = `Budget update`, colour = Series)) + geom_line() + scale_colour_manual(values = c(
  "Historical" = "blue",    # or whatever color you want
  "Projection" = "grey70"
))


ggplot(b[year_end >= 2010 & Unit != "% of GDP" & Accounting != "Cash"],aes(x=year_end,y=value, group = `Budget update`, colour = Series)) + geom_line() + scale_colour_manual(values = c(
  "Historical" = "blue",    # or whatever color you want
  "Projection" = "grey70"
))


b[year_end == 2010]

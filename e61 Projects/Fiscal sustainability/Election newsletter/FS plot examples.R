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
  scale_colour_manual(values = c("AUS" = "darkgreen", "Other" = "grey")) +
  scale_size_manual(values = c("AUS" = 1, "Other" = 0.2)) +
  scale_alpha_identity() +
  labs_e61(title = "Net debt burden",
           subtitle = "General Government, % of GDP",
           y = "(%)",
           x = "",
           sources = c("ABS","e61"),
           footnotes = "Excluding Japan and Greece, who had rates in excess of 200%")

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

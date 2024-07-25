## Last edit: 25/07/2024
# Version of the SC exercise on participation rates focused on Australia, and conditioning on dependency ratios.

#library(devtools)

# devtools::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

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

## Pull in data ----

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
result <- data.frame()

for (var_name in names(data_sets)) {
  if (grepl("series", var_name) && grepl("observations", var_name)) {
    series_index <- sub("series\\.(\\d+):.*", "\\1", var_name)
    obs_number <- sub(".*\\.observations\\.(\\d+)", "\\1", var_name)
    series_id <- sub("series\\.\\d+:(.*)\\.observations\\.\\d+", "\\1", var_name)
    observations <- data_sets[[var_name]]
    value <- observations[[1]][1]
    time <- as.numeric(obs_number)
    result <- rbind(result, data.frame(Series = as.numeric(series_index), Time = time, Value = value, SeriesID = series_id))
  }
}

setDT(result)

time_id <- as.data.table(data$structure[[3]]$observation$values)[,Time := 0:63][,.(Time,year = id)]

result <- time_id[result,on=.(Time)]

country_id <- data.table(country = data$structure$dimensions$series$values[[1]]$name,Series = 0:23)

result <- country_id[result,on=.(Series)]
result[, line_size := ifelse(country %in% c("Australia","United States"), 1.5, 0.5)]
result[, line_color := ifelse(country == "United States", "red", ifelse(country == "Australia", "blue", "lightgrey"))]

ggplot(result[year >= 1990], aes(x = as.numeric(year), y = Value/100, group = country)) +
  geom_line(aes(size = line_size, color = line_color)) +
  scale_size_identity() +
  scale_color_identity() +
  theme_e61() +
  labs_e61(title = "Participation Rates", y = "", x = "")+ scale_y_continuous_e61(labels=scales::percent_format(1)) +
  plot_label(x = c(2000,2000),y=c(0.87,0.83),c("United States","Australia"),colour=c("red","blue")) +
  scale_y_continuous_e61(limits = c(0.5,0.9,0.1),labels=scales::percent_format())

# Now make these rates "PP change from 1990"
initial <- result[year == 1990][,.(initial = Value,country)]

result2 <- initial[result,on=.(country)][,change := Value - initial]

ggplot(result2[year >= 1990], aes(x = as.numeric(year), y = change/100, group = country)) +
  geom_line(aes(size = line_size, color = line_color)) +
  scale_size_identity() +
  scale_color_identity() +
  theme_e61() +
  labs_e61(title = "Participation Rates",subtitle = "Change from 1990", y = "", x = "") + 
  geom_hline(yintercept = 0) + 
  scale_y_continuous_e61(labels=scales::percent_format(1),limits=c(-0.1,0.2,0.05))


# There are some year gaps in the data, which on the website look imputed by the average of the prior two values. So set this here.

syn_data <- result[,.(Unit = country,Time = as.numeric(year),Outcome = Value,Series = Series)][Time >= 1990 & Time <= 2023]

syn_data[,.N,by=.(Unit)]

syn_data[Unit == "Finland"][order(Time)]
syn_data[Unit == "Australia"][order(Time)]
syn_data[Time == 1992][order(Time)]

units <- unique(syn_data$Unit)
time_range <- seq(min(syn_data$Time), max(syn_data$Time))
complete_panel <- CJ(Unit = units, Time = time_range)

# Merge with the original data to get a balanced panel
syn_data <- merge(complete_panel, syn_data, by = c("Unit", "Time"), all.x = TRUE)

# Find gaps and replace with values from website

syn_data[is.na(Series)]

replacement <- syn_data[is.na(Series)]

replacement[Unit == "Australia"]$Outcome <- 72.8
replacement[Unit == "Australia"]$Series <- syn_data[Unit == "Australia"]$Series[1]

replacement[Unit == "Belgium"]$Outcome <- 66.9
replacement[Unit == "Belgium"]$Series <- syn_data[Unit == "Belgium"]$Series[1]

replacement[Unit == "Canada"]$Outcome <- 77.4
replacement[Unit == "Canada"]$Series <- syn_data[Unit == "Canada"]$Series[1]

replacement[Unit == "Costa Rica"]$Outcome <- 68.2
replacement[Unit == "Costa Rica"]$Series <- syn_data[Unit == "Costa Rica"]$Series[1]

replacement[Unit == "Denmark"]$Outcome <- 78.7
replacement[Unit == "Denmark"]$Series <- syn_data[Unit == "Denmark"]$Series[1]

replacement[Unit == "Estonia"]$Outcome <- 77.4
replacement[Unit == "Estonia"]$Series <- syn_data[Unit == "Estonia"]$Series[1]

replacement[Unit == "France"]$Outcome <- 68.7
replacement[Unit == "France"]$Series <- syn_data[Unit == "France"]$Series[1]

replacement[Unit == "Germany"]$Outcome <- 70.6
replacement[Unit == "Germany"]$Series <- syn_data[Unit == "Germany"]$Series[1]

replacement[Unit == "Greece"]$Outcome <- 67.4
replacement[Unit == "Greece"]$Series <- syn_data[Unit == "Greece"]$Series[1]

replacement[Unit == "Ireland"]$Outcome <- 68.5
replacement[Unit == "Ireland"]$Series <- syn_data[Unit == "Ireland"]$Series[1]

replacement[Unit == "Israel"]$Outcome <- 71
replacement[Unit == "Israel"]$Series <- syn_data[Unit == "Israel"]$Series[1]

replacement[Unit == "Italy"]$Outcome <- 58.2
replacement[Unit == "Italy"]$Series <- syn_data[Unit == "Italy"]$Series[1]

replacement[Unit == "Japan"]$Outcome <- 71.5
replacement[Unit == "Japan"]$Series <- syn_data[Unit == "Japan"]$Series[1]

replacement[Unit == "Korea"]$Outcome <- 66.4
replacement[Unit == "Korea"]$Series <- syn_data[Unit == "Korea"]$Series[1]

replacement[Unit == "Luxembourg"]$Outcome <- 68.7
replacement[Unit == "Luxembourg"]$Series <- syn_data[Unit == "Luxembourg"]$Series[1]

replacement[Unit == "Netherlands"]$Outcome <- 71.8
replacement[Unit == "Netherlands"]$Series <- syn_data[Unit == "Netherlands"]$Series[1]

replacement[Unit == "New Zealand"]$Outcome <- 77.6
replacement[Unit == "New Zealand"]$Series <- syn_data[Unit == "New Zealand"]$Series[1]

replacement[Unit == "Norway"]$Outcome <- 80.9
replacement[Unit == "Norway"]$Series <- syn_data[Unit == "Norway"]$Series[1]

replacement[Unit == "Portugal"]$Outcome <- 71.2
replacement[Unit == "Portugal"]$Series <- syn_data[Unit == "Portugal"]$Series[1]

replacement[Unit == "Spain"]$Outcome <- 64.5
replacement[Unit == "Spain"]$Series <- syn_data[Unit == "Spain"]$Series[1]

replacement[Unit == "United Kingdom"]$Outcome <- 76
replacement[Unit == "United Kingdom"]$Series <- syn_data[Unit == "United Kingdom"]$Series[1]

replacement[Unit == "United States"]$Outcome <- 74.7
replacement[Unit == "United States"]$Series <- syn_data[Unit == "United States"]$Series[1]

syn_data <- syn_data[!is.na(Series)]

syn_data <- rbind(syn_data,replacement)

### Undertake synthetic control exercise.
# Set treatment year

treat_year <- 2008

pre_treatment_period <- seq(1990,treat_year,by=1)
post_treatment_period <- seq(treat_year + 1,2023,by=1)

treated_unit <- 23 # The number referring to "Australia"

dataprep.out <- dataprep(
  foo = syn_data,
  predictors = c("Outcome"),  # Variables to use as predictors
  predictors.op = "mean",
  time.predictors.prior = pre_treatment_period,
  special.predictors = list(
    list("Outcome", pre_treatment_period, "mean")
  ),
  dependent = "Outcome",
  unit.variable = "Series",
  time.variable = "Time",
  treatment.identifier = treated_unit,
  controls.identifier = setdiff(unique(syn_data$Series), treated_unit),
  time.optimize.ssr = pre_treatment_period,
  time.plot = c(pre_treatment_period, post_treatment_period)
)

synth.out <- synth(dataprep.out)

synth.tab <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)
print(synth.tab)

weights <- synth.tab$tab.w
setDT(weights)
weights[w.weights == max(synth.tab$tab.w$w.weights)]$unit.names

weights[order(w.weights)]

syn_data[Series == weights[w.weights == max(synth.tab$tab.w$w.weights)]$unit.names]

# Ensure synth.out$solution.w is a numeric vector
Y0plot_numeric <- as.matrix(dataprep.out$Y0plot)
solution_w_numeric <- as.numeric(synth.out$solution.w)

# Perform the matrix multiplication
synthetic <- Y0plot_numeric %*% solution_w_numeric

# Extract treated unit data
treated <- as.numeric(dataprep.out$Y1plot)

path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "",
          Xlab = "year",
          Ylim = c(70,80),
          Legend = c("Aussie LFP","Synthetic Aus"),
          Legend.position = "bottomright"
)


plot_data <- data.frame(
  Time = as.numeric(rownames(Y0plot_numeric)),
  Treated = treated,
  Synthetic = synthetic
)

### Pull in dependency ratios for additional controls
# Add dependence information

data_url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.SPD,DSD_PAG@DF_PAG,/JPN+USA+NZL+LUX+KOR+AUS.A.OAWAR....?startPeriod=2002&endPeriod=2022"

response <- GET(data_url)

if (response$status_code == 200) {
  # Parse the JSON content
  raw_content <- content(response, "text")
  data2 <- fromJSON(raw_content, flatten = TRUE)
} else {
  stop("Failed to fetch data from the OECD API")
}

# Print the structure of the response
str(data2)

data2$dataSets

data2$structure$dimensions$series

# Extract the data we are after as "result"
data_sets2 <- data2$dataSets
result_dependence <- data.frame()

for (var_name in names(data_sets2)) {
  if (grepl("series", var_name) && grepl("observations", var_name)) {
    series_index <- sub("series\\.(\\d+):.*", "\\1", var_name)
    obs_number <- sub(".*\\.observations\\.(\\d+)", "\\1", var_name)
    series_id <- sub("series\\.\\d+:(.*)\\.observations\\.\\d+", "\\1", var_name)
    observations <- data_sets2[[var_name]]
    value <- observations[[1]][1]
    time <- as.numeric(obs_number)
    result_dependence <- rbind(result_dependence, data.frame(Series = as.numeric(series_index), Time = time, Value = value, SeriesID = series_id))
  }
}

result_dependence

setDT(result_dependence)

time_id2 <- as.data.table(data2$structure[[3]]$observation$values)[,Time := 0:3][,.(Time,year = id)]

result_dependence <- time_id2[result_dependence,on=.(Time)]

country_id2 <- data.table(country = data2$structure$dimensions$series$values[[1]]$name,Series = 0:5)
result_dependence <- country_id2[result_dependence,on=.(Series)]

result_plot <- result_dependence[year %in% c(2002,2022)]

ggplot(result_dependence[year %in% c(2002,2022)],aes(x=country,y=Value/100,fill=year)) + geom_col(position="dodge") + scale_y_continuous_e61(labels=scales::percent_format(),limits=c(0,0.62,0.1),y_top=FALSE) + labs_e61(title = "Old age dependency ratio",subtitle = "Population over 65 relative to WAP",y="",x="",)+theme_e61_alt() + coord_flip()

## Our world in data makes up for the gaps in the OECD data.  The website is here: https://ourworldindata.org/age-structure

dep_ratio <- fread("age-dependency-ratio-of-working-age-population.csv")
setDT(dep_ratio)

countries <- unique(syn_data$Unit)

length(countries)

length(unique(dep_ratio[V1 %in% countries]$V1)) # One country will be missed - South Korea

length(unique(dep_ratio[V1 %in% c(countries,"South Korea")]$V1))

dep_ratio_OECD <- dep_ratio[V1 %in% c(countries,"South Korea")]
colnames(dep_ratio_OECD) <- c("Unit","Code","Time","DR")
dep_ratio_OECD[Unit == "South Korea",Unit := "Korea"]

# Merge dependency ratios with our main dataset
syn_data <- dep_ratio_OECD[syn_data,on=.(Unit,Time)]

## New SC exercise
# Set treatment year

treat_year <- 2014

pre_treatment_period <- seq(1990,treat_year,by=1)
post_treatment_period <- seq(treat_year + 1,2023,by=1)

treated_unit <- 23 # The number referring to "Australia"

dataprep.out <- dataprep(
  foo = syn_data,
  predictors = c("Outcome", "DR"),  # Change here is to include "DR" as a predictor
  predictors.op = "mean",
  time.predictors.prior = pre_treatment_period,
  special.predictors = list(
    list("Outcome", pre_treatment_period, "mean"),
    list("DR", pre_treatment_period, "mean")  # Also need to include "DR" to special predictors
  ),
  dependent = "Outcome",
  unit.variable = "Series",
  time.variable = "Time",
  treatment.identifier = treated_unit,
  controls.identifier = setdiff(unique(syn_data$Series), treated_unit),
  time.optimize.ssr = pre_treatment_period,
  time.plot = c(pre_treatment_period, post_treatment_period)
)

synth.out <- synth(dataprep.out)

synth.tab <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)
print(synth.tab)

weights <- synth.tab$tab.w
setDT(weights)
weights[w.weights == max(synth.tab$tab.w$w.weights)]$unit.names

weights[order(w.weights)]

syn_data[Series == weights[w.weights == max(synth.tab$tab.w$w.weights)]$unit.names]

# Ensure synth.out$solution.w is a numeric vector
Y0plot_numeric <- as.matrix(dataprep.out$Y0plot)
solution_w_numeric <- as.numeric(synth.out$solution.w)

# Perform the matrix multiplication
synthetic <- Y0plot_numeric %*% solution_w_numeric

# Extract treated unit data
treated <- as.numeric(dataprep.out$Y1plot)

path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "",
          Xlab = "year",
          Ylim = c(70,82),
          Legend = c("Aussie LFP","Synthetic Aus"),
          Legend.position = "bottomright"
)


plot_data <- data.frame(
  Time = as.numeric(rownames(Y0plot_numeric)),
  Treated = treated,
  Synthetic = synthetic
)


syn_data[is.na(DR)]

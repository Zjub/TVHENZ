rm(list=ls())

.libPaths(new = 'C:/Rpackage')

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

## After the switch from stat to data the R package is broken. So need to grab information directly.
# # Grab the participation data
# 
# dataset_id <- "DSD_LFS"
# filter <- list(
#   LOCATION = "AUS",
#   SUBJECT = "TOT",
#   MEASURE = "T"
# )
# 
# # Query the data
# data <- get_dataset(dataset_id, filter = filter, start_time = 2010)

## Grab data

# Define the endpoint URL for the data query
#data_url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.SAE,DSD_LFS@DF_LFS_INDIC,/TUR+CHE+ESP+SVN+SVK+PRT+POL+NOR+NLD+MEX+LUX+LTU+LVA+KOR+JPN+ITA+ISR+IRL+ISL+HUN+GRC+DEU+FRA+FIN+EST+DNK+CZE+CRI+COL+CHL+BEL+AUT+OECD+SWE+USA+GBR+NZL+CAN+AUS.LF_RATE.PT_POP_SUB._T.Y15T64.?startPeriod=1960"

data_url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.SAE,DSD_LFS@DF_LFS_INDIC,/ESP+PRT+NOR+NLD+LUX+KOR+JPN+ITA+ISR+IRL+GRC+DEU+FRA+FIN+EST+DNK+CRI+BEL+SWE+USA+GBR+NZL+CAN+AUS.LF_RATE.PT_POP_SUB._T.Y15T64.?startPeriod=1960"

# Send the GET request
response <- GET(data_url)

# Check if the request was successful
if (response$status_code == 200) {
  # Parse the JSON content
  raw_content <- content(response, "text")
  data <- fromJSON(raw_content, flatten = TRUE)
} else {
  stop("Failed to fetch data from the OECD API")
}

str(data)

data$dataSets

data$structure$dimensions$series

# Extract the data we are after as "result"
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

# Insert the year and country correspondence

setDT(result)
#result[,year := 2023 - max(Time) + Time,by=.(Series)]

time_id <- as.data.table(data$structure[[3]]$observation$values)[,Time := 0:63][,.(Time,year = id)]

result <- time_id[result,on=.(Time)]

country_id <- data.table(country = data$structure$dimensions$series$values[[1]]$name,Series = 0:23)

result <- country_id[result,on=.(Series)]
result[, line_size := ifelse(country == "United States", 1.5, 0.5)]
result[, line_color := ifelse(country == "United States", "red", "lightgrey")]

# Plot with ggplot2
ggplot(result[year >= 1990], aes(x = as.numeric(year), y = Value/100, group = country)) +
  geom_line(aes(size = line_size, color = line_color)) +
  scale_size_identity() +
  scale_color_identity() +
  theme_e61_alt() +
  labs_e61(title = "Participation Rates", y = "", x = "")+ scale_y_continuous_e61(labels=scales::percent_format(1))

save_e61("plot1.png",res=2,auto_scale = FALSE)

ggplot(result[year >= 1990 & country %in% c("United States","Japan")], aes(x = as.numeric(year), y = Value/100, group = country)) +
  geom_line(aes(color = line_color),size=1.5) +
  scale_color_identity() +
  theme_e61_alt() +
  labs_e61(title = "Participation Rates: US and Japan", y = "", x = "")+ scale_y_continuous_e61(labels=scales::percent_format(1))

save_e61("plot1b.png",res=2,auto_scale = FALSE)


initial <- result[year == 1990][,.(initial = Value,country)]

result2 <- initial[result,on=.(country)][,change := Value - initial]

ggplot(result2[year >= 1990], aes(x = as.numeric(year), y = change/100, group = country)) +
  geom_line(aes(size = line_size, color = line_color)) +
  scale_size_identity() +
  scale_color_identity() +
  theme_e61_alt() +
  labs_e61(title = "Participation Rates",subtitle = "Change from 1990", y = "", x = "") + geom_hline(yintercept = 0) + scale_y_continuous_e61(labels=scales::percent_format(1),limits=c(-0.1,0.2,0.05))

save_e61("plot2.png",res=2,auto_scale = FALSE)

result[year == 1990]

result[year == 2000 & !country %in% result[year == 1990]$country]

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

pre_treatment_period <- seq(1990,2008,by=1)
post_treatment_period <- seq(2009,2023,by=1)

treated_unit <- 22 # The number referring to "United States"

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
          Legend = c("US LFP","Synthetic US"),
          Legend.position = "bottomright"
)


plot_data <- data.frame(
  Time = as.numeric(rownames(Y0plot_numeric)),
  Treated = treated,
  Synthetic = synthetic
)

# Melt the data for ggplot2
plot_data_melted <- melt(plot_data, id.vars = "Time", variable.name = "Type", value.name = "Outcome")

# Plot using ggplot2
ggplot(plot_data_melted, aes(x = Time, y = Outcome, color = Type)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "Synthetic Control Method",
       x = "Time",
       y = "Outcome",
       color = "Series") + geom_vline(xintercept = 2009)


#Relative to initial

initial <- syn_data[Time == 1990][,.(initial = Outcome,Series)]

syn_data <- initial[syn_data,on=.(Series)][,trend_1990 := Outcome/initial]

dataprep.out <- dataprep(
  foo = syn_data,
  predictors = c("trend_1990"),  # Variables to use as predictors
  predictors.op = "mean",
  time.predictors.prior = pre_treatment_period,
  special.predictors = list(
    list("trend_1990", pre_treatment_period, "mean")
  ),
  dependent = "trend_1990",
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

syn_data[Series == weights[w.weights == max(synth.tab$tab.w$w.weights)]$unit.names]

weights

# Ensure synth.out$solution.w is a numeric vector
Y0plot_numeric <- as.matrix(dataprep.out$Y0plot)
solution_w_numeric <- as.numeric(synth.out$solution.w)

# Perform the matrix multiplication
synthetic <- Y0plot_numeric %*% solution_w_numeric

# Extract treated unit data
treated <- as.numeric(dataprep.out$Y1plot)


path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "XXX",
          Xlab = "year",
          Legend = c("XXX"),
          Legend.position = "bottomright"
)



# Prepare data for ggplot2
plot_data <- data.frame(
  Time = as.numeric(rownames(Y0plot_numeric)),
  Treated = treated,
  Synthetic = synthetic
)

# Melt the data for ggplot2
plot_data_melted <- melt(plot_data, id.vars = "Time", variable.name = "Type", value.name = "trend_1990")
setDT(plot_data_melted)
plot_data_melted[,Outcome := trend_1990*initial[Series == 21]$initial]

# Plot using ggplot2
ggplot(plot_data_melted, aes(x = Time, y = trend_1990, color = Type)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "Synthetic Control Method",
       x = "Time",
       y = "trend_1990",
       color = "Series") + geom_vline(xintercept = 2009)


ggplot(plot_data_melted, aes(x = Time, y = Outcome/100, color = Type)) +
  geom_line(size = 1) +
  theme_e61_alt() +
  labs_e61(title = "Synthetic United States",
       x = "",
       y = "",
       colour = "Series") + geom_vline(xintercept = 2009,linetype="dashed") + scale_y_continuous_e61(labels=scales::percent_format(),limits = c(0.7,0.87,0.05),y_top=FALSE)


save_e61("plot3.png",res=2,auto_scale = FALSE)


ggplot(country_id[weights,on=.(Series=unit.names)][order(-w.weights)],aes(x=country,y=w.weights)) + geom_col() + coord_flip() + labs_e61(title = "Weights of varying nations",y="",x="") + scale_y_continuous_e61(labels=scales::percent_format(),limits=c(0,0.15,0.05)) + theme_e61_alt()

save_e61("plot4.png",res=2,auto_scale = FALSE)



### Check doing as FDs
# 
# # Calculate the first differences of the outcome variable
# setkey(syn_data,Unit, Time)
# 
# syn_data[, Outcome_diff := Outcome - shift(Outcome, type = "lag"), by = Unit]
# 
# # Remove the first period since it will have NA for the first difference
# syn_data <- syn_data[!is.na(Outcome_diff)]
# 
# # Define the pre-treatment and post-treatment periods
# pre_treatment_period <- 1991:2009  # Replace with actual pre-treatment periods
# post_treatment_period <- 2010:2023  # Replace with actual post-treatment periods
# treated_unit <- 21
# 
# # Convert treated_unit to numeric if necessary
# treated_unit_num <- as.numeric(factor(treated_unit, levels = levels(factor(syn_data$Series))))
# 
# # Prepare the data for the Synth package
# dataprep.out <- dataprep(
#   foo = syn_data,
#   predictors = c("Outcome_diff"),  # Use first differences as predictors
#   predictors.op = "mean",
#   time.predictors.prior = pre_treatment_period,
#   special.predictors = list(
#     list("Outcome_diff", pre_treatment_period, "mean")
#   ),
#   dependent = "Outcome_diff",  # Use first differences as the dependent variable
#   unit.variable = "Series",
#   time.variable = "Time",
#   treatment.identifier = treated_unit_num,
#   controls.identifier = setdiff(unique(syn_data$Series), treated_unit_num),
#   time.optimize.ssr = pre_treatment_period,
#   time.plot = c(pre_treatment_period, post_treatment_period)
# )
# 
# # Run the synthetic control method
# synth.out <- synth(dataprep.out)
# 
# # Generate results and plots
# synth.tab <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)
# print(synth.tab)
# 
# # Ensure synth.out$solution.w is a numeric vector
# Y0plot_numeric <- as.matrix(dataprep.out$Y0plot)
# solution_w_numeric <- as.numeric(synth.out$solution.w)
# 
# # Perform the matrix multiplication
# synthetic <- Y0plot_numeric %*% solution_w_numeric
# 
# # Extract treated unit data
# treated <- as.numeric(dataprep.out$Y1plot)
# 
# # Prepare data for ggplot2
# plot_data <- data.frame(
#   Time = as.numeric(rownames(Y0plot_numeric)),
#   Treated = treated,
#   Synthetic = synthetic
# )
# 
# # Melt the data for ggplot2
# plot_data_melted <- melt(plot_data, id.vars = "Time", variable.name = "Type", value.name = "Outcome")
# 
# # Plot using ggplot2
# ggplot(plot_data_melted, aes(x = Time, y = Outcome, color = Type)) +
#   geom_line(size = 1) +
#   theme_minimal() +
#   labs(title = "Synthetic Control Method (First Differences)",
#        x = "Time",
#        y = "Outcome Difference",
#        color = "Series")

## Add dependence information

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

save_e61("plot4.png",res=2,auto_scale = FALSE)

############### 30 July: Add version conditional on dependency ratio for Zach


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

plots <- list()

years <- c(2000,2008,2014)

for(i in 1:3){
  
  treat_year <- years[i]
  
  pre_treatment_period <- seq(1990,treat_year,by=1)
  post_treatment_period <- seq(treat_year + 1,2023,by=1)
  
  treated_unit <- 22 # The number referring to "Australia"
  
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
            Legend = c("US LFP","Synthetic US"),
            Legend.position = "bottomright"
  )
  
  
  plot_data <- data.frame(
    Time = as.numeric(rownames(Y0plot_numeric)),
    Treated = treated,
    Synthetic = synthetic
  )
  setDT(plot_data)
  
  
  syn_data[is.na(DR)]
  
  ggplot(country_id[weights,on=.(Series=unit.names)][order(-w.weights)],aes(x=country,y=w.weights)) + geom_col() + coord_flip() + labs_e61(title = "Weights of varying nations",y="",x="") + scale_y_continuous_e61(labels=scales::percent_format(),limits=c(0,0.06,0.02)) + theme_e61_alt()
  
  graph_withcontrols <- data.table::melt(plot_data,id.vars = "Time")
  
  p <- ggplot(graph_withcontrols,aes(x=Time,y=value,colour=variable)) + geom_line() + labs_e61(title = "Synthetic United States",subtitle=paste0("Treatment year = ",years[i]),sources = c("ABS","e61"),y="%") + scale_y_continuous_e61(labels=scales::percent_format(scale=1),limits=c(70,82,2)) + geom_vline(xintercept = treat_year,linetype="dashed")

  plots[[i]] <- p
}

save_e61(plotlist = plots,"SC_US.svg")

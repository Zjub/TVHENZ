# Topic: Looking at consolidated amounts by function
# Author: Matt Nolan
# Created: 5/7/2025
# Last edit: 7/7/2025
# Last editor: Matt Nolan

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



Consol_toG <- read_excel("table22-consolidated-cofog-expenditure-spent-by-approach.xlsx", 
                                                                       sheet = "COFOGexp_%oftotal", skip = 1)

Consol_toGDP <- read_excel("table22-consolidated-cofog-expenditure-spent-by-approach.xlsx", 
                           sheet = "COFOGexp_%GDP", skip = 1)

setDT(Consol_toG)
setDT(Consol_toGDP)

### Set up data ----

Aus_toG <- Consol_toG[Country == "Australia"][,':=' (Country = NULL,ISO = NULL, `COFOG Code` = NULL, `Government level code` = NULL,`1995` = NULL,`1996` = NULL,`1997` = NULL)]

Aus_toG <- melt(Aus_toG, id.vars = c(colnames(Aus_toG)[1],colnames(Aus_toG)[2]),variable.name = "Year",value.name = "value")

colnames(Aus_toG)[1] <- "COFOG_Area"
colnames(Aus_toG)[2] <- "Government_level"

Aus_toG[, Government_level := fifelse(Government_level %in% c("Local", "State"), 
                                      "Non-Federal", "Federal")]

Aus_toG <- Aus_toG[, .(value = sum(value)), 
                   by = .(COFOG_Area, Government_level, Year)]

Aus_toG

Aus_toGDP <- Consol_toGDP[Country == "Australia"][,':=' (Country = NULL,ISO = NULL, `COFOG Code` = NULL, `Government level code` = NULL,`1995` = NULL,`1996` = NULL,`1997` = NULL)]

Aus_toGDP <- melt(Aus_toGDP, id.vars = c(colnames(Aus_toGDP)[1],colnames(Aus_toGDP)[2]),variable.name = "Year",value.name = "value")

colnames(Aus_toGDP)[1] <- "COFOG_Area"
colnames(Aus_toGDP)[2] <- "Government_level"

Aus_toGDP[, Government_level := fifelse(Government_level %in% c("Local", "State"), 
                                      "Non-Federal", "Federal")]

Aus_toGDP <- Aus_toGDP[, .(value = sum(value)), 
                   by = .(COFOG_Area, Government_level, Year)]

Aus_toGDP


## Make Australia plots ----

unique(Aus_toG$COFOG_Area)

ggplot(Aus_toG[COFOG_Area == "Defence"],aes(x=Year, y=value,fill = as.factor(Government_level))) + geom_col() +
  theme_e61(legend = "bottom")

ggplot(Aus_toG[COFOG_Area == "Enviromental Protection"],aes(x=Year, y=value,fill = as.factor(Government_level))) + geom_col() +
  theme_e61(legend = "bottom")

ggplot(Aus_toGDP[COFOG_Area == "Enviromental Protection"],aes(x=as.numeric(Year), y=value,colour = as.factor(Government_level))) + geom_line() +
  theme_e61(legend = "bottom")

ggplot(Aus_toG[COFOG_Area == "Social Protection"],aes(x=Year, y=value,fill = as.factor(Government_level))) + geom_col() +
  theme_e61(legend = "bottom")

ggplot(Aus_toGDP[COFOG_Area == "Social Protection"],aes(x=as.numeric(Year), y=value,colour = as.factor(Government_level))) + geom_line() +
  theme_e61(legend = "bottom")



## Make international comparison data ----
# To GDP comparisons

toGDP <- Consol_toGDP[][, `:=`(
  ISO = NULL,
  `COFOG Code` = NULL,
  `Government level code` = NULL,
  `1995` = NULL,
  `1996` = NULL,
  `1997` = NULL
)]

toGDP <- melt(
  toGDP, 
  id.vars = c("Country", "COFOG Area", "Government level"),
  variable.name = "Year",
  value.name = "value"
)

setnames(toGDP, c("COFOG Area", "Government level"), c("COFOG_Area", "Government_level"))

toGDP[, Government_level := fifelse(
  Government_level %in% c("Local", "State"), "Non-Federal", "Federal"
)]

toGDP <- toGDP[, .(value = if (all(is.na(value))) NA_real_ else sum(value, na.rm=TRUE)),
               by = .(Country, COFOG_Area, Government_level, Year)]

totalGDP <- toGDP[, .(value = if (all(is.na(value))) NA_real_ else sum(value, na.rm=TRUE)),
                  by = .(Country, COFOG_Area, Year)]
totalGDP[, Government_level := "Total"]

toGDP <- rbind(toGDP, totalGDP)

toGDP[, Aus_flag := fifelse(Country == "Australia", "Australia",
                            fifelse(Country %in% c("Ireland","United Kingdom","United States"),"Anglo","Other"))]

ggplot(toGDP[COFOG_Area == "Defence" & Government_level == "Total"], 
       aes(x = as.numeric(Year), y = value, 
           group = Country, colour = Aus_flag, size = Aus_flag)) +
  geom_line() +
  scale_colour_manual(values = c("Australia" = palette_e61(3)[1],"Anglo" = palette_e61(3)[3], "Other" = "lightgrey")) +
  scale_size_manual(values = c("Australia" = 1.2, "Anglo" = 0.5, "Other" = 0.5)) +
  labs_e61(colour = "Country", size = "Country",title = "Defence")

unique(toGDP$COFOG_Area)

split_plot_data <- toGDP[COFOG_Area == "Total" & Government_level != "Total" & Year == "2022"]
split_plot_data[, Country := factor(Country, 
                              levels = split_plot_data[Government_level == "Federal"][order(value)]$Country)]
split_plot_data[, Government_level := factor(Government_level, 
                                       levels = c("Non-Federal","Federal"))]

## France is far too small in this plot
ggplot(split_plot_data, aes(x = Country, y = value, fill = Government_level)) +
  geom_col() +
  coord_flip() +
  labs_e61(title = "Role of consolidation",
           footnotes = c("Dark blue is spending by Federal Govt % GDP. Light blue is additional spending attributed to non-Federal entities."))

save_e61("Consolidation_cc.png",res=2)

split_plot_data2 <- toGDP[COFOG_Area == "Total" & Government_level != "Total" & Country == "Australia"]
split_plot_data2[, Government_level := factor(Government_level, 
                                             levels = c("Non-Federal","Federal"))]

ggplot(split_plot_data2,aes(x=Year,y=value*100,fill=Government_level)) +
  geom_col() +
  labs_e61(title = "Consolidated spending",
           subtitle = "Activity attributed to authority who \"spent\" funds.",
           y="%")

ggplot(split_plot_data2,aes(x=Year,y=value*100,fill=Government_level)) +
  geom_col(position = "dodge") +
  labs_e61(title = "Consolidated spending",
           subtitle = "Activity attributed to authority who \"spent\" funds.",
           y="%")



## Do for all types

areas <- unique(toGDP$COFOG_Area)

plots <- lapply(areas, function(area) {
  p <- ggplot(toGDP[COFOG_Area == area & Government_level == "Total"], 
              aes(x = as.numeric(Year), y = value, 
                  group = Country, colour = Aus_flag, size = Aus_flag)) +
    geom_line() +
    scale_colour_manual(values = c("Australia" = palette_e61(3)[1], "Anglo" = palette_e61(3)[3], "Other" = "lightgrey")) +
    scale_size_manual(values = c("Australia" = 1.2, "Anglo" = 0.5, "Other" = 0.5)) +
    labs_e61(colour = "Country", size = "Country", title = area,subtitle = "%GDP")
  
  save_e61(filename = paste0("Consolidated Spending in ",area,".png"),plot = p,res=2)
  
  print(toGDP[Aus_flag == "Anglo" & COFOG_Area == area & Year == "2022" & Government_level == "Total"])
  
  return(p)
})

names(plots) <- areas

plots

toGDP[Aus_flag == "Anglo" & COFOG_Area == "Health" & Year == "2022" & Government_level == "Total"]


# Double check the totals

totals_ext <- table4_gov_exp_gdp <- read_excel("table4_gov_exp-gdp.xlsx", 
                                               sheet = "exp_%_gpd", skip = 1)

setDT(totals_ext)

colnames(totals_ext)[1] <- "cc"
colnames(totals_ext)[2] <- "Country"
colnames(totals_ext)[3] <- "Level"

totals_ext <- totals_ext[,":=" (cc = NULL)]

totals_ext[,.(spend_GDP = sum(`2022`,na.rm=TRUE)),by=.(Country)][order(spend_GDP)]

toGDP[Aus_flag == "Anglo" & COFOG_Area == "Total" & Year == "2022" & Government_level == "Total"]

ggplot(
  totals_ext[, .(spend_GDP = sum(`2022`, na.rm=TRUE)), by = .(Country)][spend_GDP > 0][,":=" (flag = fifelse(Country == "Australia","Australia","Other"))][order(spend_GDP)],
  aes(x = factor(Country, levels = Country), y = spend_GDP,fill=flag)
) +
  scale_fill_manual(values = c("Australia" = palette_e61(3)[3], "Other" = palette_e61(3)[1])) +
  geom_col() +
  coord_flip()



## Share comparisons

toG <- Consol_toG[
  #!`Government level` == "Social Security Funds"
][, `:=`(
  ISO = NULL,
  `COFOG Code` = NULL,
  `Government level code` = NULL,
  `1995` = NULL,
  `1996` = NULL,
  `1997` = NULL
)]

toG <- melt(
  toG, 
  id.vars = c("Country", "COFOG Area", "Government level"),
  variable.name = "Year",
  value.name = "value"
)

setnames(toG, c("COFOG Area", "Government level"), c("COFOG_Area", "Government_level"))

toG[, Government_level := fifelse(
  Government_level %in% c("Local", "State"), "Non-Federal", Government_level
)]

toG <- toG[, .(value = if (all(is.na(value))) NA_real_ else sum(value, na.rm=TRUE)),
               by = .(Country, COFOG_Area, Government_level, Year)]

toG[, Aus_flag := fifelse(Country == "Australia", "Australia", "Other")]








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

data_url <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.SAE,DSD_LFS@DF_LFS_INDIC,/ESP+PRT+NOR+NLD+LUX+KOR+JPN+ITA+ISR+IRL+GRC+DEU+FRA+FIN+EST+DNK+CRI+BEL+SWE+USA+GBR+NZL+CAN+AUS.LF_RATE.PT_POP_SUB._T.Y15T64.?startPeriod=1960"
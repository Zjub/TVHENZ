## Last update:  18/07/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan
# Looking at the initial consolidated data to consider i) defence and interest, ii) capital spending, iii) clustering.

## Setup ----

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

rm(list=ls())
gc()

## Import data ----

work = TRUE

if (work == TRUE){
  consolidate_dt <- read_csv("C:/Users/MattNolan/Git/TVHENZ/e61 Projects/Fiscal sustainability/Function analysis/Data/abs_gfs_data_clean.csv")
} else{print("Na bro")}


setDT(consolidate_dt)

head(consolidate_dt)

unique(consolidate_dt$sector_desc)

### Defence and interest ----



### Assets by function ----


### Cluster analysis test ----
# Note clustering is fairly detailed work, so put most of this in another script - just set up some basic NN clustering and PCA to give an example of looking at i) what is similar ii) what drives similarity.


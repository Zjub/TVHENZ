# Pulling in data on New Zealand arrival dates (from datalab) and New Zealand departures (from ABS website)
# Author: Matt Nolan
# Date made: 09/12/2024
# Last update: 09/12/2024

library(tidyverse)
library(data.table)
library(theme61)
library(readxl)
library(fixest)
library(readabs)

### New Zealand departures

dt <- read_abs("3401.0")

colnames(dt)

unique(dt$table_no)
unique(dt$series)

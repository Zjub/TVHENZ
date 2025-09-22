## Last update:  22/09/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan
# Update debt scenarios for external report using latest PBO tool (the finalised FY26 tool, rather than the interim one used in the internal report)

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
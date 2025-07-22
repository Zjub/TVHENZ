## Last update:  21/07/2025
## Author:  Matt Nolan
## Last update person:  Matt Nolan
# Adding some high level plots for Greg's speech to the tax roundtable.

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

taxpercentiles <- read_excel("taxpercentiles.xlsx")
setDT(taxpercentiles)

taxpercentiles[,":=" (tax_share = total_tax/sum(total_tax),income_share = total_income/sum(total_income))]

share_dt <- melt(taxpercentiles[,.(percentile,tax_share,income_share)],id.vars = "percentile")

ggplot(share_dt,aes(x=percentile,y=value,colour=variable)) + geom_line()

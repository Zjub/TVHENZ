# Pulling in aggregate data from US and Australia to compare.

rm(list=ls())
gc()

.libPaths(new = 'C:/Rpackage')

library(tidyverse)
library(theme61)
library(tidyr)
library(readxl)
library(gghighlight)
library(readabs)
library(seasonal)
library(fredr)

# Just downloaded manually rather than bothering with fredr, given the constant updating of API keys required.
# Focus is unemployment rate, vacancies to unemployment, and participation rates [all seasonally adjusted]

USPR <- read.csv("USPR.csv") %>% filter(DATE >= "2003-01-01")
USUNEMPLOY <- read.csv("USUNEMPLOY.csv") %>% filter(DATE >= "2003-01-01")
USUNRATE <- read.csv("USUNRATE.csv") %>% filter(DATE >= "2003-01-01")
USVAC <- read.csv("USVAC.csv") %>% filter(DATE >= "2003-01-01")

USVUR <- inner_join(USUNEMPLOY,USVAC,by="DATE") 
USVUR <- USVUR %>% mutate(VU = LMJVTTUVUSM647S/(UNEMPLOY*1000))

plot(USVUR$VU)

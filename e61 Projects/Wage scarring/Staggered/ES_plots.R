## Create the staggered ES wage scarring plots

## Setup ----

rm(list=ls())

library(pacman)

p_load(
  tidyverse,
  data.table,
  collapse,
  readabs,
  readr,
  readxl,
  mFilter,
  zoo
)

library(theme61)

## Graphs

Industry_ES <- read_csv("Data/Industry_ES.csv")
setDT(Industry_ES)

Industry_ES[,prop := coefficient/avg_inc]

Industry_ES[rel_year == 1]

Industry_ES[rel_year == 5]

ggplot(Industry_ES,aes(x=rel_year,y=coefficient,colour=Industry)) + geom_line()
ggplot(Industry_ES,aes(x=rel_year,y=prop*100,colour=Industry)) + geom_line() +
  labs_e61(title = "Wage Scarring by Industry",
           y = "%",
           x = "",
           sources = c("ABS","e61"),
           footnotes = c("Reference period is three years prior to job loss")) +
  geom_vline(xintercept = -1,linetype="dashed") +
  plab(c("Primary","Secondary","Sales","Private Services","Public Services"),x=c(-4,-4,-4,-4,-4),y=c(-62,-52,-42,-32,-22),colour = c(palette_e61(5)[1],palette_e61(5)[5],palette_e61(5)[4],palette_e61(5)[2],palette_e61(5)[3])) +
  scale_y_continuous_e61(limits = c(-100,10,20)) + geom_hline(yintercept = 0) +
  scale_x_continuous_e61(limits = c(-5,5,1))


save_e61("Wage_scar_industry.png",res=2)

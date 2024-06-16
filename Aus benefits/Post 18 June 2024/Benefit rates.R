# Purpose: Illustrates relative "real" beneift rates with different deflators for a blog post.
# Author: Matt Nolan
# Date: 16/06/2024

rm(list=ls())

ptm <- proc.time()

.libPaths(new = 'C:/Rpackage')

# install and load packages 
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  data.table,
  magrittr,
  fst,
  datapasta,
  readxl, 
  janitor,
  lubridate,
  ggthemes,
  viridis,
  tictoc,
  readtext,
  broom,
  scales,
  crayon,
  hildareadR,
  collapse,
  DescTools,
  MASS,
  readabs,
  readrba,
  readxl,
  rvest
)

library(theme61)

## Construct nominal single individual benefit rates. ----

legislation_url <- "https://guides.dss.gov.au/social-security-guide/5/2/1/20"
webpage <- read_html(legislation_url)

tables <- webpage %>% html_nodes("table") %>% html_table()

#early_ben_table <- tables[[8]]
benefit_table <- tables[[9]]
#setDT(early_ben_table)
setDT(benefit_table)

benefit_table <- benefit_table[,`Rate ($ pw)` := as.numeric(gsub(" .*", "", `Rate ($ pw)`))]

benefit_table <- benefit_table[,Rate := as.numeric(`Rate ($ pw)`)][!is.na(Rate)][,Date := as.Date(Date, format = "%d/%m/%Y")][,`Rate ($ pw)` := NULL][Date == 2000-07-01,Date := Date + 19]

# Rates switch from weekly to fortnightly in 2021-03-20, so adjust them to all be weekly.

benefit_table[Date >= as.Date("2021-03-20"),Rate := Rate/2]
#benefit_table[Date >= 1987-06-13,Rate := Rate/2] # Also early values were fortnightly

#colnames(early_ben_table) <- c("Date","Rate")
#early_ben_table[,Date := as.Date(Date, format = "%d/%m/%Y")]

#benefit_dt <- rbind(early_ben_table,benefit_table) # Commenting out the early data for now.
benefit_dt <- benefit_table

# Adjust the months to make it match the given quarter - could automate but want to see the specific dates for now.

benefit_dt <- benefit_dt[Date != as.Date("2021-03-20")] # As rate is increased a fortnight later we will just drop this.

benefit_dt[, Date := fifelse(Date == as.Date("1989-11-15"), as.Date("1989-12-20"),
                             fifelse(Date == as.Date("1990-04-18"), as.Date("1990-03-20"),
                                     fifelse(Date == as.Date("2000-07-01"), as.Date("2000-06-20"),
                                             fifelse(Date == as.Date("2021-04-01"), as.Date("2021-03-20"),
                                                     Date))))]

## Now bring in the different deflators ----

Living_CI <- read_abs("6467.0")
setDT(Living_CI)

ben_LCI <- Living_CI[data_type == "INDEX" & series == "Index Numbers ;  Other government transfer recipient households ;  All groups ;" & table_title == "TABLE 1. All Groups, Index Numbers and Percentage Changes, by Household Type"][,.(Date = date + 19,ben_LCI = value)]

benefit_dt <- ben_LCI[benefit_dt,on=.(Date)]
benefit_dt[, Date := fcase(day(benefit_dt$Date) == 1,Date,
                           day(benefit_dt$Date) == 13, Date - 12,
                           day(benefit_dt$Date) == 20, Date - 19,
                           default = NA)]

CPI <- read_abs("6401.0")
setDT(CPI)

dt <- CPI[table_title == "TABLES 1 and 2. CPI: All Groups, Index Numbers and Percentage Changes" & unit == "Index Numbers" & series == "Index Numbers ;  All groups CPI ;  Australia ;"][,.(Date = date,CPI = value)][benefit_dt,on=.(Date)][,Date := as.Date(Date)]


### Construct "real" benefit rates and plot ----

dt <- dt[,':=' (Real_Rate = Rate/CPI*100, LCI_adj_Rate = Rate/ben_LCI*100)][,.(Date,Rate,Real_Rate,LCI_adj_Rate)]

graph_dt <- melt(dt,id="Date")

ggplot(graph_dt,aes(x=Date,y=value,colour=variable)) + geom_line() + scale_y_continuous_e61(labels=scales::dollar_format(),limits=c(100,700,100)) + labs_e61(title = "Single Working-Age Adult Unemployment Benefit",subtitle = "Weekly Gross Payment",y="") + theme_e61_alt() + plot_label("Weekly Payment",x=as.Date("1990-03-01"),y=310,colour=palette_e61(3)[1],size=4.5) + plot_label("Real Payment (CPI)",x=as.Date("1990-03-01"),y=350,colour=palette_e61(3)[2],size=4.5) + plot_label("Real Payment (LCI)",x=as.Date("1990-03-01"),y=390,colour=palette_e61(3)[3],size=4.5) + theme(legend.position = "none")

save_e61("Long-term benefits.png",chart_type = "PPT",res=2)


ggplot(graph_dt[Date > as.Date("2011-12-30")],aes(x=Date,y=value,colour=variable)) + geom_line() + scale_y_continuous_e61(labels=scales::dollar_format(),limits=c(100,700,100)) + labs_e61(title = "Single Working-Age Adult Unemployment Benefit",subtitle = "Weekly Gross Payment",y="") + theme_e61_alt() + plot_label("Weekly Payment",x=as.Date("2012-03-01"),y=310,colour=palette_e61(3)[1],size=4.5) + plot_label("Real Payment (CPI)",x=as.Date("2012-03-01"),y=350,colour=palette_e61(3)[2],size=4.5) + plot_label("Real Payment (LCI)",x=as.Date("2012-03-01"),y=390,colour=palette_e61(3)[3],size=4.5) + scale_x_date(date_breaks = "2 years", date_labels = "%Y") + theme(legend.position = "none")

save_e61("Real benefits.png",chart_type = "PPT",res=2)

### Gross replacement rates ----
# Create percent of full-time and percent of average replacement rates

AWE <- read_abs("6302.0")
setDT(AWE)

AWE_dt <- AWE[table_title == "TABLE 2. Average Weekly Earnings, Australia (Dollars) - Seasonally Adjusted" & series %in% c("Earnings; Persons; Full Time; Adult; Ordinary time earnings ;","Earnings; Persons; Total earnings ;" )][,Date := as.Date(date) %m-% months(2) -14][,.(Date,series,value)]

AWE_dt <- dcast(AWE_dt,Date~series)
colnames(AWE_dt) <- c("Date","FT","Average")

ben_base <- benefit_dt[Date > as.Date("2011-12-30")]$ben_LCI[1]
FT_base <- AWE_dt$FT[1]
Avg_base <- AWE_dt$Average[1]

AWE_graph_dt <- AWE_dt[benefit_dt,on=.(Date)][!is.na(FT)][,":=" (Real_Rate = Rate/ben_LCI*ben_base,FT_deflate = Rate/FT*FT_base,Avg_deflate = Rate/Average*Avg_base,FT_RR = Rate/FT,Avg_RR = Rate/Average)]

AWE_RR_dt <- melt(AWE_graph_dt[,.(Date,FT_RR,Avg_RR)],id="Date")

AWE_graph_dt <- melt(AWE_graph_dt[,.(Date,Real_Rate,FT_deflate,Avg_deflate)],id="Date")

ggplot(AWE_graph_dt,aes(x=Date,y=value,colour=variable)) + geom_line() + theme_e61_alt() + scale_y_continuous_e61(labels=scales::dollar_format(),limits=c(100,700,100)) + labs_e61(title = "Single Working-Age Adult Unemployment Benefit",subtitle = "Weekly Gross Payment, deflated in various ways",y="") + plot_label("Real Payment (LCI)",x=as.Date("2012-03-01"),y=310,colour=palette_e61(3)[1],size=4.5) + plot_label("Relative to FT earners",x=as.Date("2012-03-01"),y=350,colour=palette_e61(3)[2],size=4.5) + plot_label("Relative to the Average Earner",x=as.Date("2012-03-01"),y=390,colour=palette_e61(3)[3],size=4.5) + theme(legend.position = "none") + scale_x_date(date_breaks = "2 years", date_labels = "%Y")

save_e61("Benefits relative to earnings.png",chart_type = "PPT",res=2)

ggplot(AWE_RR_dt,aes(x=Date,y=value,colour=variable)) + geom_line() + theme_e61_alt() + scale_y_continuous_e61(labels=scales::percent_format(),limits=c(0.1,0.5,0.1)) + labs_e61(title = "Single Working-Age Adult Unemployment Benefit",subtitle = "Income Replacement Rate (gross)",y="") + plot_label("Relative to FT earners",x=as.Date("2012-03-01"),y=0.33,colour=palette_e61(3)[1],size=4.5) + plot_label("Relative to the Average Earner",x=as.Date("2012-03-01"),y=0.38,colour=palette_e61(3)[2],size=4.5) + theme(legend.position = "none") + scale_x_date(date_breaks = "2 years", date_labels = "%Y")

save_e61("Income Replacement Rates.png",chart_type = "PPT",res=2)
# Topic: Looking at consolidated amounts by function
# Author: Matt Nolan
# Created: 5/7/2025
# Last edit: 9/7/2025
# Last editor: Matt Nolan

rm(list=ls())

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
library(readabs)


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
total_order <- split_plot_data[, .(total_value = sum(value, na.rm=TRUE)), by = Country][order(total_value)]$Country
split_plot_data[, Country := factor(Country, 
                              levels = total_order)]
split_plot_data[, Government_level := factor(Government_level, 
                                       levels = c("Non-Federal","Federal"))]

## Overall plot
ggplot(split_plot_data[!Country %in% c("Italy","Czech Republic","Hungary","Slovenia","Spain","Iceland","Estonia")], aes(x = Country, y = value*100, fill = Government_level)) +
  geom_col() +
  coord_flip() +
  labs_e61(title = "Role of consolidation",
           subtitle = "",
           footnotes = c("Dark blue is spending by Federal Govt % GDP. Light blue is additional spending attributed to non-Federal entities."),
           y="")

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

## And make comparison graphs

ggplot(toGDP[Government_level == "Total" & Country == "Australia"],aes(x=as.numeric(as.character(Year)),y=value,colour=COFOG_Area)) + geom_line() + theme_e61(legend = "bottom")

toGDP[Government_level == "Total" & Country == "Australia" & !COFOG_Area == "Total"]

toGDP[Government_level == "Total" & Country == "Australia" & COFOG_Area == "Total"]

cons_share <- toGDP[Government_level == "Total" & Country == "Australia" & COFOG_Area == "Total"][,.(Year,value)][toGDP[Government_level == "Total" & Country == "Australia" & !COFOG_Area == "Total"],on=.(Year)][,share := i.value/value][,Year := as.numeric(as.character(Year))]

ggplot(cons_share[Year >= 2008],aes(x=Year,y=share*100,colour=COFOG_Area)) + geom_line() + theme_e61(legend = "bottom") + labs_e61(title = "Consolidated spending share",y="%")

cons_share[COFOG_Area == "Economic Affairs"]

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


toGDP[Year == "2022" & COFOG_Area == "Total"]

### Work out contributions ----

dollar_spend <- table22_consolidated_cofog_expenditure_spent_by_approach <- read_excel("table22-consolidated-cofog-expenditure-spent-by-approach.xlsx", sheet = "COFOGexp", skip = 1)
setDT(dollar_spend)

Aus_spend <- dollar_spend[Country == "Australia"][,":=" (`COFOG Code` = NULL,ISO=NULL,`Government level code` = NULL, `1995` = NULL, `1996` = NULL, `1997` = NULL,Country = NULL)][`Government level` != "Social Security Funds"]

colnames(Aus_spend)[1] <- "COFOG_Area"
colnames(Aus_spend)[2] <- "Government_level"

Aus_spend[, Government_level := fifelse(Government_level %in% c("Local", "State"), 
                                      "Non-Federal", "Federal")]

Aus_spend_long <- melt(Aus_spend,id.vars = c("COFOG_Area","Government_level"),variable.name = "Year",value.name = "value")

Aus_spend_long <- Aus_spend_long[, .(value = sum(value)), 
                   by = .(COFOG_Area, Government_level, Year)][,Year := as.numeric(as.character(Year))]

Aus_spend_long_total <- Aus_spend_long[, .(value = sum(value)), 
                                       by = .(COFOG_Area, Year)]

a<-ggplot(Aus_spend_long[Government_level == "Federal" & !COFOG_Area == "Total"],aes(x=Year,y=value,colour=COFOG_Area)) + geom_line() + theme_e61(legend = "bottom")

b<-ggplot(Aus_spend_long_total[!COFOG_Area == "Total"],aes(x=Year,y=value,colour=COFOG_Area)) + geom_line() + theme_e61(legend = "bottom")

#save_e61(a,b,filename ="spending_nominal_incconsolidated.png",res=2)

# Contributions plot - Federal

total_federal_spend <- Aus_spend_long[Government_level == "Federal" & !COFOG_Area == "Total"][, .(total_value = sum(value)), by = Year]

federal_spend <- merge(Aus_spend_long[Government_level == "Federal" & !COFOG_Area == "Total"], total_federal_spend, by = "Year")
federal_spend[, share := value / total_value]

shares_2012_2022 <- federal_spend[Year %in% c(2012, 2022),
                            .(share_2012 = share[Year == 2012],
                              share_2022 = share[Year == 2022]),
                            by = COFOG_Area]

# Contribution to total growth
# (change in spending for area / total spending change) from 2012 to 2022
growth_fed <- federal_spend[Year %in% c(2012, 2022),
                  dcast(.SD, COFOG_Area ~ Year, value.var = "value")]

growth_fed[, total_change := `2022` - `2012`]

total_change_federal_spend <- total_federal_spend[Year == 2022, total_value] -
  total_federal_spend[Year == 2012, total_value]

growth_fed[, contribution := total_change / total_change_federal_spend]

# Merge results
federal_summary <- merge(shares_2012_2022, growth_fed[, .(COFOG_Area, contribution)], by = "COFOG_Area")

ggplot(melt(federal_summary,id.vars = "COFOG_Area",variable.name = "variable",value.name = "value"),aes(x=COFOG_Area,y=value,fill=variable)) + geom_col(position = "dodge") + coord_flip() +
  plab(c("2012 share","2022 share","Contribution"),y=c(0.2,0.2,0.2),x=c(3.5,4.5,5.5))

# Contributions plot - State

total_consolidated_spend <- Aus_spend_long_total[!COFOG_Area == "Total"][, .(total_value = sum(value)), by = Year]

consolidated_spend <- merge(Aus_spend_long_total[!COFOG_Area == "Total"], total_consolidated_spend, by = "Year")
consolidated_spend[, share := value / total_value]

consolidated_shares_2012_2022 <- consolidated_spend[Year %in% c(2012, 2022),
                                  .(share_2012 = share[Year == 2012],
                                    share_2022 = share[Year == 2022]),
                                  by = COFOG_Area]

# Contribution to total growth
# (change in spending for area / total spending change) from 2012 to 2022
growth_consolidated <- consolidated_spend[Year %in% c(2012, 2022),
                            dcast(.SD, COFOG_Area ~ Year, value.var = "value")]

growth_consolidated[, total_change := `2022` - `2012`]

total_change_consolidated_spend <- total_consolidated_spend[Year == 2022, total_value] -
  total_consolidated_spend[Year == 2012, total_value]

growth_consolidated[, contribution := total_change / total_change_consolidated_spend]

# Merge results
consolidated_summary <- merge(consolidated_shares_2012_2022, growth_consolidated[, .(COFOG_Area, contribution)], by = "COFOG_Area")

ggplot(melt(consolidated_summary,id.vars = "COFOG_Area",variable.name = "variable",value.name = "value"),aes(x=COFOG_Area,y=value,fill=variable)) + geom_col(position = "dodge") + coord_flip() +
  plab(c("2012 share","2022 share","Contribution"),y=c(0.2,0.2,0.2),x=c(3.5,4.5,5.5)) + 
  labs_e61(title = "Consolidated Government Expenditure")

save_e61("Consolidated_shares_contribution.png",res=2)

contributions_both <-growth_consolidated[,.(COFOG_Area,cons_contribution = contribution)][growth_fed[,.(COFOG_Area,contribution)],on=.(COFOG_Area)]

ggplot(melt(contributions_both,id.vars = "COFOG_Area"),aes(x=COFOG_Area,y=value*100,fill=variable)) + geom_col(position="dodge") + coord_flip() +
  labs_e61(title = "Contribution to Expenditure growth 2012-2022",
           y="%") +
  plab(c("Consolidated","Federal"),x=c(3.5,4.5),y=c(20,20)) +
  scale_y_continuous_e61(limits = c(0,50,10))

save_e61("Budget_growth_cont.png",res=2)

a

b


### Generate Figure 8 from internal report, but with consolidated spending.

## Fiscal things (habits)

pay_ngdp <- read_excel("Expenditure plots.xlsx",
                       sheet = "Sheet1", range = "A1:D27")
setDT(pay_ngdp)

colnames(pay_ngdp) <- c("FY","Payments","NGDP","GDPD")

# Change things to real
pay_ngdp[,RPayments := Payments/GDPD]
pay_ngdp[,RGDP := NGDP/GDPD]

base_year <- "2000"
base_row <- pay_ngdp[FY == base_year]
pay_ngdp[, `:=`(
  Payments_norm = RPayments / base_row$RPayments,
  RGDP_norm = RGDP / base_row$RGDP
)]

pay_ngdp[, Year := as.integer(FY)]
trend_data <- pay_ngdp[Year <= 2014 & Year != 2009]
trend_model <- lm(log(RGDP_norm) ~ Year, data = trend_data)
pay_ngdp[, RGDP_trend := exp(predict(trend_model, newdata = .SD))]

## Prior plot
# ggplot(pay_ngdp, aes(x = Year)) +
#   geom_line(aes(y = Payments_norm, color = "Payments")) +
#   geom_line(aes(y = RGDP_norm, color = "NGDP")) +
#   geom_line(aes(y = RGDP_trend, color = "Pre-2014 NGDP Trend"), linetype = "dashed") +
#   scale_color_manual(values = c("Payments" = palette_e61(3)[1], "NGDP" = palette_e61(3)[2], "Pre-2014 NGDP Trend" = "black")) +
#   plab(c("Real Govt Payments","GDP","2000-2014 GDP trend"),x=c(2000.5,2000.5,2000.5),y=c(2.2,1.85,1.7),colour = c(palette_e61(3)[1],palette_e61(3)[2],"black")) +
#   labs_e61(subtitle = "Deflated by GDPD, indexed to 1 in FY99/20",
#            y="",
#            x="")

# Now include the information we have in this script

Aus_spend_long_total[COFOG_Area == "Total"]

# Pull in population numbers

#LS_full <- read_abs(cat_no = "6291.0.55.001") %>% filter(table_title ==  "Table 01. Labour force status by Age, Social marital status, and Sex" )
pop_full <- read_abs(cat_no = "3101.0")
setDT(pop_full)

unique(pop_full$series)

# The annual data reflects fiscal years
pop <- pop_full[startsWith(series, "Estimated Resident Population ;  Persons ;") & table_no == 3101059]

unique(pop$series)

pop_long <- pop[,.(pop = sum(value)/1000000),by=.(date)][,Year := year(date)]

pop_long <- pop_long[Aus_spend_long_total[COFOG_Area == "Total"],on=.(Year)][,nom_pc_spend := value/pop]

ggplot(pop_long,aes(x=Year,y=nom_pc_spend)) + geom_col()

# Deflate

cpi <- read_abs(cat_no = "6401.0")
setDT(cpi)

cpi <- cpi[startsWith(series, "Index Numbers ;  All groups CPI ;  Aust") & table_no == 640101]

max(cpi[,.N,by=.(date)]$N)

cpi[, Year := year(date) + (month(date) >= 7)]
cpi_annual <- cpi[, .(cpi_avg = mean(value, na.rm = TRUE)), by = Year]

pop_real <- cpi_annual[pop_long,on=.(Year)][,real_pc_spend := nom_pc_spend*100/cpi_avg]


ggplot(pop_real,aes(x=Year,y=real_pc_spend/1000)) + geom_col() +
  labs_e61(title = "Real Consolidated Expenditure per person",
           y="$(000s)")


save_e61("Real_consolidated_PC.png",res=2)


# Find consolidated plots for short-termism in "Fiscal habits"


## Consolidated borrowing

borrow_dt <- read_excel("table18_balances_gdp.xlsx", 
                        sheet = "balances_%_gdp", skip = 1)
setDT(borrow_dt)

colnames(borrow_dt)[2] <- "Country"

borrowing_data <- borrow_dt[Country %in% split_plot_data$Country & !Country %in% c("Italy","Czech Republic","Hungary","Slovenia","Spain","Iceland","Estonia")][!is.na(`2023`)][,.(Country, `2023`)][,.(borrowing =sum(`2023`,na.rm=TRUE)),by=.(Country)]

borrowing_data[, Country := factor(Country, levels = borrowing_data[order(-borrowing)]$Country)]
borrowing_data[, highlight := ifelse(Country == "Australia", "Australia", "Other")]

# Plot with custom colors
ggplot(borrowing_data, aes(x = Country, y = borrowing, fill = highlight)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Australia" = "gold", "Other" = palette_e61(3)[2])) +
  labs_e61(
    title = "Consolidated Government Borrowing",
    subtitle = "% of GDP, 2023",
    sources = c("OECD", "e61")
  ) +
  scale_y_continuous_e61(limits =c(-10,20,5))

save_e61("Borrowing_consolidate.png",res=2)

borrow_dt[Country == "Australia"]


### GFS information

GFS_ABS <- read_excel("GFS_ABS.xlsx", sheet = "For_plot", 
                      skip = 17)
setDT(GFS_ABS)


GFS_dt <- GFS_ABS[,.(Expense,nom_change = `2024`-`2015`)]

GFS_ABS_real <- melt(GFS_ABS,id.vars="Expense",variable.name = "Year")[,Year := as.integer(as.character(Year))]

GFS_ABS_real <- GFS_ABS_real[cpi_annual,on=.(Year)][!is.na(value)][,real_spend := value/cpi_avg]

GFS_ABS_real <- GFS_ABS_real[Year == 2024][GFS_ABS_real[Year == 2015],on=.(Expense)][,.(Expense,real_spend,i.real_spend)][,real_diff := real_spend - i.real_spend]

ggplot(GFS_ABS_real[,.(Expense,real_prop = real_diff/sum(real_diff))],aes(x=1,y=real_prop*100,fill=Expense)) + geom_col() +
  theme_e61(legend = "right") +
  labs_e61(title = "Share of real expense growth",
           y = "%") +
  scale_y_continuous_e61(limits = c(0,100,20))+
  theme(
    legend.text = element_text(size = 5),      # Smaller legend text
    axis.text.x = element_blank(),             # Remove x-axis numbers
    axis.ticks.x = element_blank()             # Optional: remove x-axis ticks too
  )

save_e61("Real_expense_growth_GFS.png",res=2,auto_scale = FALSE)



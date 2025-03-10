## Grabbing some trade data for a post
# Initial: 2/03/2025
# Author: Matt Nolan
# Last update: 2/03/2025

rm(list=ls())

library(remotes)
library(tidyverse)
library(data.table)
library(collapse)
library(readabs)
library(readr)
library(readxl)
library(theme61)
library(lubridate)
library(mFilter)
library(zoo)
library(Hmisc)
library(seasonal)

## Data ----

trade_full <- read_abs("5368.0") # This is broad trade, not just merchandise trade, set of series.
setDT(trade_full)

trade_full

unique(trade_full$table_title) # Only have merchandise trade by country

### Exports
data_export_2024 <- trade_full[table_title == "TABLE 14a. MERCHANDISE EXPORTS, by Country, FOB Value" & date >= as.Date("2024-01-01") & date < as.Date("2025-01-01"),.(value = sum(value)),by=.(series)][order(-value)][,prop := value/value[1]]

top_export_2024 <- data_export_2024[2:11]

Other_2024 <- data.table(series = "Other ;", value = data_export_2024[1]$value - sum(data_export_2024[2:11]$value), prop = 1 - sum(data_export_2024[2:11]$prop))

top_export_2024 <- rbind(top_export_2024,Other_2024)

top_export_2024[,country := sub("[ ;]+$", "", series)][,country := factor(country,levels=country)]

# Top here is the top in 2024, not the top in the given year.
data_export_2019 <- trade_full[table_title == "TABLE 14a. MERCHANDISE EXPORTS, by Country, FOB Value" & date >= as.Date("2019-01-01") & date < as.Date("2020-01-01"),.(value = sum(value)),by=.(series)][order(-value)][,prop_2019 := value/value[1]]

top_export_2019 <- data_export_2019[series %in% top_export_2024$series]

Other_2019 <- data.table(series = "Other ;", value = data_export_2019[1]$value - sum(top_export_2019$value), prop_2019 = 1 - sum(top_export_2019$prop))

top_export_2019 <- rbind(top_export_2019,Other_2019)

data_export_2014 <- trade_full[table_title == "TABLE 14a. MERCHANDISE EXPORTS, by Country, FOB Value" & date >= as.Date("2014-01-01") & date < as.Date("2015-01-01"),.(value = sum(value)),by=.(series)][order(-value)][,prop_2014 := value/value[1]]

top_export_2014 <- data_export_2014[series %in% top_export_2024$series]

Other_2014 <- data.table(series = "Other ;", value = data_export_2014[1]$value - sum(top_export_2014$value), prop_2014 = 1 - sum(top_export_2014$prop))

top_export_2014 <- rbind(top_export_2014,Other_2014)

data_export_2004 <- trade_full[table_title == "TABLE 14a. MERCHANDISE EXPORTS, by Country, FOB Value" & date >= as.Date("2004-01-01") & date < as.Date("2005-01-01"),.(value = sum(value)),by=.(series)][order(-value)][,prop_2004 := value/value[1]]

top_export_2004 <- data_export_2004[series %in% top_export_2024$series]

Other_2004 <- data.table(series = "Other ;", value = data_export_2004[1]$value - sum(top_export_2004$value), prop_2004 = 1 - sum(top_export_2004$prop))

top_export_2004 <- rbind(top_export_2004,Other_2004)

#ggplot(top_export_2024,aes(x=country,y=prop)) + geom_col()

decade_compare <- top_export_2014[top_export_2024,on=.(series)]
decade_compare <- top_export_2004[decade_compare,on=.(series)]

ggplot(melt(decade_compare[,.(country,prop,prop_2014,prop_2004)],id.vars = "country"),aes(x=country,y=value,fill=variable)) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits=c(0,0.4,0.1),labels=scales::percent_format()) +
  labs_e61(title="Proportion of total exports",
           x="",
           y="") + coord_flip() +
  plab(label = c("2024","2014","2004"),y=c(0.2,0.2,0.2),x=c(7.5,8.5,9.5))

save_e61("Export.png",res=2,pad_width = 2)




### Imports
data_import_2024 <- trade_full[table_title == "TABLE 14b. MERCHANDISE IMPORTS, by Country, Customs Value" & date >= as.Date("2024-01-01") & date < as.Date("2025-01-01"),.(value = sum(value)),by=.(series)][order(-value)][,prop := value/value[1]]

top_import_2024 <- data_import_2024[2:11]

Other_2024 <- data.table(series = "Other ;", value = data_import_2024[1]$value - sum(data_import_2024[2:11]$value), prop = 1 - sum(data_import_2024[2:11]$prop))

top_import_2024 <- rbind(top_import_2024,Other_2024)

top_import_2024[,country := sub("[ ;]+$", "", series)][,country := factor(country,levels=country)]

data_import_2019 <- trade_full[table_title == "TABLE 14b. MERCHANDISE IMPORTS, by Country, Customs Value" & date >= as.Date("2019-01-01") & date < as.Date("2020-01-01"),.(value = sum(value)),by=.(series)][order(-value)][,prop_2019 := value/value[1]]

top_import_2019 <- data_import_2019[series %in% top_import_2024$series]

Other_2019 <- data.table(series = "Other ;", value = data_import_2019[1]$value - sum(top_import_2019$value), prop_2019 = 1 - sum(top_import_2019$prop))

top_import_2019 <- rbind(top_import_2019,Other_2019)

data_import_2014 <- trade_full[table_title == "TABLE 14b. MERCHANDISE IMPORTS, by Country, Customs Value" & date >= as.Date("2014-01-01") & date < as.Date("2015-01-01"),.(value = sum(value)),by=.(series)][order(-value)][,prop_2014 := value/value[1]]

top_import_2014 <- data_import_2014[series %in% top_import_2024$series]

Other_2014 <- data.table(series = "Other ;", value = data_import_2014[1]$value - sum(top_import_2014$value), prop_2014 = 1 - sum(top_import_2014$prop))

top_import_2014 <- rbind(top_import_2014,Other_2014)

data_import_2004 <- trade_full[table_title == "TABLE 14b. MERCHANDISE IMPORTS, by Country, Customs Value" & date >= as.Date("2004-01-01") & date < as.Date("2005-01-01"),.(value = sum(value)),by=.(series)][order(-value)][,prop_2004 := value/value[1]]

top_import_2004 <- data_import_2004[series %in% top_import_2024$series]

Other_2004 <- data.table(series = "Other ;", value = data_import_2004[1]$value - sum(top_import_2004$value), prop_2004 = 1 - sum(top_import_2004$prop))

top_import_2004 <- rbind(top_import_2004,Other_2004)

#ggplot(top_import_2024,aes(x=country,y=prop)) + geom_col()

decade_compare <- top_import_2014[top_import_2024,on=.(series)]
decade_compare <- top_import_2004[decade_compare,on=.(series)]

ggplot(melt(decade_compare[,.(country,prop,prop_2014,prop_2004)],id.vars = "country"),aes(x=country,y=value,fill=variable)) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits=c(0,0.4,0.1),labels=scales::percent_format()) +
  labs_e61(title="Proportion of total imports",
           x="",
           y="") + coord_flip() +
  plab(label = c("2024","2014","2004"),y=c(0.2,0.2,0.2),x=c(7.5,8.5,9.5))

save_e61("Import.png",res=2,pad_width = 2)


### Net exports
data_netx_2024 <- data_import_2024[data_export_2024,on=.(series)][!is.na(value)][,net_ex := i.value - value][order(-net_ex)]

top_netx_2024 <- data_netx_2024[series %in% top_export_2024$series][,.(series,net_ex)][,country := sub("[ ;]+$", "", series)][,country := factor(country,levels=country)]

ggplot(top_netx_2024,aes(x=country,y=net_ex/1000)) + geom_col() +
  coord_flip() +
  scale_y_continuous_e61(limits = c(-40,80,20)) + 
  add_baseline() +
  labs_e61(title = "Net Export",subtitle = "Top 10 Export countries",
           y="$bn",
           x="")

save_e61("Net_export.png",res=2,pad_width = 2)

#"TABLE 14a. MERCHANDISE EXPORTS, by Country, FOB Value"                                                       
#[14] "TABLE 14b. MERCHANDISE IMPORTS, by Country, Customs Value"   
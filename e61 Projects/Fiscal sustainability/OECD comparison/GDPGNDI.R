## Graphing GDP per capita and GNDI per capita



rm(list = ls())
gc()

# Load required libraries
library(tidyverse)
library(data.table)
library(vars)       # For VAR models
library(tseries)    # For stationarity tests
library(ggplot2)
library(readabs)
library(theme61)

# Load and prepare GDP data
a <- read_abs("5206.0")
setDT(a)

a <- a[table_no == "5206001_key_aggregates"]

unique(a$series)

# Filter for the required series: GDP, Terms of Trade, and RNGDI
data_dt <- a[
  date >= as.Date("1980-01-01") & 
    series %in% c(
      #"GDP per hour worked: Index ;",
      #"Gross domestic product: Chain volume measures ;",
      "GDP per capita: Chain volume measures ;",
      #"Terms of trade: Index ;",
      "Real net national disposable income per capita: Chain volume measures ;"
    ) & 
    series_type == "Seasonally Adjusted"
]

##### Graphs used for fiscal work ----

unique(data_dt$series)
unique(data_dt$series_type)

base_yr <- data_dt[date == as.Date("2000-03-01"),.(series,value)]

dt <- base_yr[,.(series,total = value)][data_dt,on=.(series)][,index := value/total]

ggplot(dt[date >= as.Date("2000-01-01")],aes(x=date,y=index,colour=series)) + geom_line() +
  labs_e61(title = "High export prices support incomes",
           y = "Index = March 2000",
           sources = c("ABS","e61"),
           footnotes = c("Quarterly, seasonally adjusted, per capita data.")) + scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  plab(c("GDP per capita","RNNDI per capita"),x=c(as.Date("2000-03-01"),as.Date("2000-03-01")),y=c(1.35,1.45)) +
  scale_y_continuous_e61(limits = c(0.9,1.6,0.1))

save_e61("RNNDIvGDP.png",res=2,auto_scale = FALSE) 

fwrite(dt[date >= as.Date("2000-01-01"),.(series,date,index)],"GDPNNDI.csv")

dt[date %in% c("2025-06-01"),.(index25 = index,series)]

compare_dt <- dt[date %in% c("2025-06-01"),.(index25 = index,series)][dt[date %in% c("2019-12-01"),.(index19 = index,series)],on=.(series)]
compare_dt[,":=" (growth = index25/index19-1,annualised = (index25/index19)^(1/5.5)-1)]

compare_dt

data_dt2 <- a[
  date >= as.Date("1979-01-01") & 
    series %in% c(
      "GDP per hour worked: Index ;"
      #"Gross domestic product: Chain volume measures ;",
      #"GDP per capita: Chain volume measures ;",
      #"Terms of trade: Index ;",
      #"Real net national disposable income per capita: Chain volume measures ;"
    ) & 
    series_type == "Seasonally Adjusted"
]

data_dt2[, `:=`(
  yr  = year(date),
  qtr = quarter(date),
  dec_start = floor(year(date) / 10) * 10,
  decade = paste0(sprintf("%02d", (floor(year(date)/10)*10) %% 100), "s")
)]

# keep the decades of interest
data_dt2 <- data_dt2[dec_start %in% c(1980, 1990, 2000, 2010, 2020)]

# compute growth metrics per decade
growth_by_decade <- data_dt2[order(date), {
  # endpoints
  first_row <- .SD[1]
  last_row  <- .SD[.N]
  
  # exact span in years using quarters
  qdiff  <- (year(last_row$date) - year(first_row$date)) * 4 +
    (quarter(last_row$date) - quarter(first_row$date))
  yrs    <- qdiff / 4
  
  # CAGR (annualised)
  cagr   <- if (yrs > 0) (last_row$value / first_row$value)^(1/yrs) - 1 else NA_real_
  
  # Average annual log growth using all quarterly obs (annualised)
  ann_log_mean <- if (.N > 1) 4 * mean(diff(log(value))) else NA_real_
  ann_log_rate <- if (!is.na(ann_log_mean)) exp(ann_log_mean) - 1 else NA_real_
  
  .(start_date   = first_row$date,
    end_date     = last_row$date,
    start_index  = first_row$value,
    end_index    = last_row$value,
    years_span   = yrs,
    CAGR         = cagr,
    AvgAnnLog    = ann_log_rate)
}, by = .(dec_start, decade)][order(dec_start)]

growth_by_decade

ggplot(data_dt2[date >= as.Date("2000-01-01")],aes(x=date,y=value,colour=series)) + geom_line() +
  labs_e61(title = "High export prices support incomes",
           y = "Index = March 2000",
           sources = c("ABS","e61"),
           footnotes = c("Quarterly, seasonally adjusted, per capita data.")) + scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  plab(c("GDP per capita","RNNDI per capita"),x=c(as.Date("2000-03-01"),as.Date("2000-03-01")),y=c(1.35,1.45))


growth_by_decade$decade <- factor(growth_by_decade$decade,levels = c("80s","90s","00s","10s","20s"))

ggplot(growth_by_decade,aes(x=decade,y=AvgAnnLog*100)) + geom_col() +
  labs_e61(title = "Decade productivity growth",
           y="Average annual growth",
           footnotes = c("The 2020s decade is only a partial decade.","Labour productivity definded as GDP per hour worked."),
           sources = c("ABS","e61"))

ggplot(growth_by_decade,aes(x=decade,y=CAGR*100)) + geom_col() +
  labs_e61(title = "Decade productivity growth",
           y="Average annual growth",
           footnotes = c("The 2020s decade is only a partial decade.","Labour productivity definded as GDP per hour worked."),
           sources = c("ABS","e61"))

save_e61("Prod_decade.png",res=2)

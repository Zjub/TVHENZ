## Christmas post for blog on retail sales
# Initial: 23/12/2024
# Author: Matt Nolan
# Last update: 23/12/2024

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

# Import retail sales data

retail_full <- read_abs("8501.0")
setDT(retail_full)

unique(retail_full$table_title)

retail_monthly <- retail_full[table_title == "TABLE 1. Retail Turnover, By Industry Group"]

unique(retail_monthly$series)

ggplot(retail_monthly[series_type == "Original" & series == "Turnover ;  Total (State) ;  Total (Industry) ;"],aes(x=date,y=value/1000)) + geom_line() +
  labs_e61(y="",x="",title = "Retail sales",subtitle = "$bn, nominal") +
  scale_y_continuous_e61(limits = c(0,50,10))

save_e61("total_retail.png",res=2)

retail_monthly_total <- retail_monthly[series_type == "Original" & series == "Turnover ;  Total (State) ;  Total (Industry) ;"]

retail_monthly_total[,":=" (year = year(date),month=month(date))]
retail_monthly_total[,":=" (mvalue = max(value), avg = mean(value)),by=.(year)]

table(retail_monthly_total[value == mvalue & year !=2024]$month)

retail_monthly_total[month != 12,":=" (sndvalue = max(value),avg_nonD = mean(value)),by=.(year)]

seconddt <- unique(retail_monthly_total[month != 12,.(sndvalue,avg_nonD,year)]) 
setDT(seconddt)

retail_monthly_total <- seconddt[retail_monthly_total,on=.(year)]

table(retail_monthly_total[value == sndvalue & year !=2024 & month != 12]$month)
retail_monthly_total[value == sndvalue & year !=2024 & month != 12]

retail_monthly_total[,ratio_max_2nd := mvalue/sndvalue]
retail_monthly_total[,ratio_max_avg := mvalue/avg_nonD]
retail_monthly_total[,ratio_top2 := (mvalue+sndvalue)/(avg*2)]

ggplot(unique(retail_monthly_total[year != 2024,.(ratio_max_2nd,year)]),aes(x=year,y=ratio_max_2nd)) + geom_line() +
  labs_e61(title = "December becomes less important",subtitle="Sales relative to second largest month",y="",x="") +
  scale_y_continuous_e61(limits=c(1,1.5,0.1))

save_e61("December_retail.png",res=2)

ggplot(unique(retail_monthly_total[year != 2024,.(ratio_max_avg,year)]),aes(x=year,y=ratio_max_avg)) + geom_line() +
  labs_e61(title = "December becomes less important",subtitle="Sales relative to average non-December",y="",x="")

ggplot(unique(retail_monthly_total[year != 2024,.(ratio_top2,year)]),aes(x=year,y=ratio_top2)) + geom_line() +
  labs_e61(title = "Shifting to November?",subtitle="Sales of top two months relative to average",y="",x="")+
  scale_y_continuous_e61(limits=c(1,1.5,0.1))

save_e61("Christmas.png",res=2)

### What are the seasonal products?
## Same as above but by product.

unique(retail_monthly$series)

retail_monthly[, series2 := str_match(series, "^[^;]+;[^;]+;\\s*([^;]+)\\s*;")[, 2]]

sectors <- unique(retail_monthly$series2)[1:6]

for (i in 1:length(sectors)){
  temp <- retail_monthly[series_type == "Original" & series2 == sectors[i]]
  temp[,":=" (year = year(date),month=month(date))]
  temp[,":=" (mvalue = max(value), avg = mean(value)),by=.(year)]
  
  print(table(temp[value == mvalue & year !=2024]$month)) # Check December is the peak for each category, otherwise rewrite to use December instead of max.
  
  temp[month != 12,":=" (sndvalue = max(value),avg_nonD = mean(value)),by=.(year)]
  
  temp2 <- unique(temp[month != 12,.(sndvalue,avg_nonD,year)]) 
  setDT(temp2)
  
  temp <- temp2[temp,on=.(year)]
  temp[,ratio_max_2nd := mvalue/sndvalue]
  temp[,ratio_max_avg := mvalue/avg_nonD]
  temp[,ratio_top2 := (mvalue+sndvalue)/(avg*2)]
  
  a <- ggplot(unique(temp[year != 2024,.(ratio_max_2nd,year)]),aes(x=year,y=ratio_max_2nd)) + geom_line() +
    labs_e61(title = paste0("December ",sectors[i]),subtitle="Sales relative to second largest month",y="",x="") + scale_y_continuous_e61(limits = c(1,2,0.2))
  
  print(a)
  
  save_e61(plot = a,filename = paste0("December_",sectors[i],".png"),res=2)

  b <- ggplot(unique(temp[year != 2024,.(ratio_max_avg,year)]),aes(x=year,y=ratio_max_avg)) + geom_line() +
    labs_e61(title = paste0("To average ",sectors[i]),subtitle="December Sales to average of rest of year",y="",x="") + scale_y_continuous_e61(limits = c(1,2.4,0.2))
  
  save_e61(plot = b,filename = paste0("December_toavg_",sectors[i],".png"),res=2)
  

  print(b)
}


## Do we have more detail quarterly

retail_ind_subgroup <- retail_full[table_title == "TABLE 11. Retail Turnover, State by Industry Subgroup, Original"]
retail_ind_subgroup <- retail_ind_subgroup[grepl("Total \\(State\\)", series)]  # Remove the state level data
retail_ind_subgroup[, series2 := str_match(series, "^[^;]+;[^;]+;\\s*([^;]+)\\s*;")[, 2]]
retail_ind_subgroup[,":=" (year = year(date),month=month(date))]

unique(retail_ind_subgroup$series2)
sectors

retail_sub_only <- retail_ind_subgroup[!(series2 %in% sectors[c(1:6)]) & series2 != "Total (Industry) "]

subsales <- retail_sub_only[,sum(value),by=.(year)]

totalsales <- retail_ind_subgroup[series2 == "Total (Industry) ",sum(value),by=.(year)]

sub1sales <- retail_ind_subgroup[series2 %in% sectors,sum(value),by=.(year)]

subsales - totalsales
sub1sales - totalsales # Suggests we've dropped some big categories

subsectors <- unique(retail_sub_only$series2)

for (i in 1:length(subsectors)){
  temp <- retail_sub_only[series_type == "Original" & series2 == subsectors[i]]
  temp[,":=" (year = year(date),month=month(date))]
  
  temp[month != 12,":=" (avg_nonD = mean(value)),by=.(year)]
  temp[month == 12,":=" (Dec_value = value),by=.(year)]
  
  temp2 <- unique(temp[month != 12,.(avg_nonD,year)]) 
  setDT(temp2)
  
  temp3 <- unique(temp[month == 12,.(Dec_value,year)]) 
  setDT(temp2)
  
  temp <- temp2[temp,on=.(year)]
  temp <- temp3[temp,on=.(year)]
  temp[,ratio_Dec_avg := Dec_value/avg_nonD]
  
  print(temp[year == 2023,.(series2[1],mean(ratio_Dec_avg))])
}

# The 2023 comparison for the post
sectors_wsub <- unique(retail_ind_subgroup$series2)

result_2023 <- data.table(series2 = character(), mean_ratio_Dec_avg = numeric())

for (i in 1:length(sectors_wsub)) {
  temp <- retail_ind_subgroup[series_type == "Original" & series2 == sectors_wsub[i]]
  temp[, ":=" (year = year(date), month = month(date))]
  
  temp[month != 12, ":=" (avg_nonD = mean(value)), by = .(year)]
  temp[month == 12, ":=" (Dec_value = value), by = .(year)]
  
  temp2 <- unique(temp[month != 12, .(avg_nonD, year)]) 
  setDT(temp2)
  
  temp3 <- unique(temp[month == 12, .(Dec_value, year)]) 
  setDT(temp3)
  
  temp <- temp2[temp, on = .(year)]
  temp <- temp3[temp, on = .(year)]
  temp[, ratio_Dec_avg := Dec_value / avg_nonD]
  
  a <- ggplot(unique(temp[year != 2024,.(ratio_Dec_avg,year)]),aes(x=year,y=ratio_Dec_avg)) + geom_line() +
    labs_e61(title = paste0("To average ",sectors_wsub[i]),subtitle="December Sales to average of rest of year",y="",x="") + scale_y_continuous_e61(limits = c(1,2.4,0.2))
  
  print(a)
  
  save_e61(plot = a,filename = paste0("subsector_",sectors_wsub[i],".png"),res=2)
  
  
  temp_result <- temp[year == 2023, .(series2 = sectors_wsub[i], mean_ratio_Dec_avg = mean(ratio_Dec_avg, na.rm = TRUE))]
  
  result_2023 <- rbind(result_2023, temp_result, fill = TRUE)
}

total_fig <- result_2023[series2 == "Total (Industry) "]$mean_ratio_Dec_avg

ggplot(result_2023,aes(x=series2,y=mean_ratio_Dec_avg)) + geom_col() + coord_flip() +
  labs_e61(title = "2023 sales ratio",y="",x="") + format_flip() +
  geom_hline(yintercept = total_fig,linetype = "dashed",colour="red") +
  scale_y_continuous_e61(limits = c(0,2,0.5))
  
save_e61("categories.png",res=2,pad_width = 1)



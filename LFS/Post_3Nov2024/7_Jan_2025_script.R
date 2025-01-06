## Just look at gender gaps in participation by age and region
# Date started: 6/01/2025
# Author: Matt Nolan

library(tidyverse)
library(data.table)
library(theme61)
library(readabs)
library(seasonal)
library(readxl)

rm(list = ls())

PR_OECD <- read_excel("PR_OECD.xlsx", sheet = "R_gap")
setDT(PR_OECD)

PR_OECD[, country := factor(country, levels = country[order(`2022`)])]
PR_OECD[, highlight := ifelse(country == "(AUS) Australia", "(AUS) Australia", "Other")]

# Create a custom color scale
ggplot(PR_OECD, aes(x = factor(country, levels = country[order(`2022`)]), y = `2022`, fill = highlight)) + 
  geom_col() +
  labs_e61(title = "Participation rate gap", subtitle = "Male PR - Female PR", y = "", x = "") +
  scale_y_continuous_e61(limits = c(0, 40, 10)) +
  scale_fill_manual(values = c("(AUS) Australia" = "red", "Other" = "grey")) +  
  coord_flip()  

save_e61("PR_gap.png",res=2,pad_width = 1)

# ABS

LFS <- read_abs(cat_no = "6202.0") 
setDT(LFS)
## Gender

LFS_gender <- LFS %>% filter(series_type == "Seasonally Adjusted") %>% filter(table_title == "Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original")  %>% filter(series == "Participation rate ;  Persons ;" | series == "Participation rate ;  > Males ;"  | series == "Participation rate ;  > Females ;") %>% select("date","series","value")

LFS_detailed <- read_abs(cat_no = "6291.0.55.001") %>% filter(table_title == "Table 01. Labour force status by Age, Social marital status, and Sex")
setDT(LFS_detailed)

unique(LFS_detailed$series)

age_groups <- c("15-24", "25-34", "35-44", "45-54", "55-64","65 years and over")

list_of_tables_female <- list()
list_of_tables_male <- list()

for (age_group in age_groups) {
  
  if (age_group != "65 years and over"){
    dt <- LFS_detailed[series == paste0("> ", age_group, " years ;  Participation rate ;  > Females ;")][, .(date, value)]
  } else {
    dt <- LFS_detailed[series == paste0(age_group, " ;  Participation rate ;  > Females ;")][, .(date, value)]
  }
  
  dt <- dt[order(date)]
  
  # Check if there are any NA values in 'value' and exclude them (interpolating might be better, check to see the gaps manually)
  if (any(is.na(dt$value))) {
    dt <- dt[!is.na(value)]
  }
  
  value_ts <- ts(dt$value, start = c(year(min(dt$date)), month(min(dt$date))), frequency = 12)
  seas_result <- try(seas(value_ts), silent = TRUE)
  
  # Check if seasonal adjustment succeeded and if '$series$s11' is available from that objecvt
  if (!inherits(seas_result, "try-error") && !is.null(seas_result$series$s11)) {
    value_sa <- seas_result$series$s11
    dt[, value_sa := as.numeric(value_sa)]
  } else {
    dt[, value_sa := NA]  # Set to NA if seasonal adjustment fails
    warning(paste("Seasonal adjustment failed for age group:", age_group))
  }
  
  list_of_tables_female[[age_group]] <- dt
}

age_PR_female_dt <- rbindlist(list_of_tables_female, idcol = "age_group")

avg_age_1990 <- age_PR_female_dt[date >= as.Date("1990-01-01") & date <= as.Date("1990-12-31"),
                          .(avg_value = mean(value_sa)), by = age_group]

ggplot(age_PR_female_dt[date >= as.Date("1990-01-01")],aes(x=date,y=value_sa,colour=age_group)) + geom_line() +
  labs_e61(title = "Female Labour Force Participation by Age",y="",source = "ABS") +
  scale_y_continuous_e61(limits=c(0,100,by=20)) +
  plab(label = c("15-24","25-34","35-44","45-54","55-64","65+"),x = rep(as.Date("2010-01-01"),times=6),y=c(47,42,37,32,27,22)) +
  geom_hline(data = avg_age_1990[age_group == "15-24"], aes(yintercept = avg_value), linetype = "dotted", colour = palette_e61(6)[1]) +
  geom_hline(data = avg_age_1990[age_group == "25-34"], aes(yintercept = avg_value), linetype = "dotted", colour = palette_e61(6)[2]) +
  geom_hline(data = avg_age_1990[age_group == "35-44"], aes(yintercept = avg_value), linetype = "dotted", colour = palette_e61(6)[3]) +
  geom_hline(data = avg_age_1990[age_group == "45-54"], aes(yintercept = avg_value), linetype = "dotted", colour = palette_e61(6)[4]) +
  geom_hline(data = avg_age_1990[age_group == "55-64"], aes(yintercept = avg_value), linetype = "dotted", colour = palette_e61(6)[5]) +
  geom_hline(data = avg_age_1990[age_group == "65 years and over"], aes(yintercept = avg_value), linetype = "dotted", colour = palette_e61(6)[6])

save_e61("age_fem_PR.png",res=2,pad_width = 1)

for (age_group in age_groups) {
  
  if (age_group != "65 years and over"){
    dt <- LFS_detailed[series == paste0("> ", age_group, " years ;  Participation rate ;  > Males ;")][, .(date, value)]
  } else {
    dt <- LFS_detailed[series == paste0(age_group, " ;  Participation rate ;  > Males ;")][, .(date, value)]
  }
  
  dt <- dt[order(date)]
  
  # Check if there are any NA values in 'value' and exclude them (interpolating might be better, check to see the gaps manually)
  if (any(is.na(dt$value))) {
    dt <- dt[!is.na(value)]
  }
  
  value_ts <- ts(dt$value, start = c(year(min(dt$date)), month(min(dt$date))), frequency = 12)
  seas_result <- try(seas(value_ts), silent = TRUE)
  
  # Check if seasonal adjustment succeeded and if '$series$s11' is available from that objecvt
  if (!inherits(seas_result, "try-error") && !is.null(seas_result$series$s11)) {
    value_sa <- seas_result$series$s11
    dt[, value_sa := as.numeric(value_sa)]
  } else {
    dt[, value_sa := NA]  # Set to NA if seasonal adjustment fails
    warning(paste("Seasonal adjustment failed for age group:", age_group))
  }
  
  list_of_tables_male[[age_group]] <- dt
}

age_PR_male_dt <- rbindlist(list_of_tables_male, idcol = "age_group")
colnames(age_PR_male_dt) <- c("age_group","date","value_male","value_sa_male")

age_PR <- age_PR_male_dt[age_PR_female_dt,on=.(age_group,date)]

age_PR[,PR_gap := value_sa_male - value_sa]

ggplot(age_PR,aes(x=date,y=PR_gap,colour=age_group)) + geom_line() +
  plab(label = c("15-24","25-34","35-44","45-54","55-64","65+"),x = c(rep(as.Date("2010-01-01"),times=3),rep(as.Date("2017-01-01"),times=3)),y=c(47,42,37,47,42,37)) +
  scale_y_continuous_e61(limits = c(-10,60,10)) + geom_hline(yintercept = 0) +
  labs_e61(title = "Participation gap by age",subtitle="Male PR - Female PR",y="ppt",x="")
  
save_e61("age_PR_gap.png",res=2,pad_width = 1)

# Also add "cohort" to follow through a few cohorts participation gaps

#ten_year_data <-age_PR_female_dt[date %in% c(as.Date("1980-01-01"),as.Date("1990-01-01"),as.Date("2000-01-01"),as.Date("2010-01-01"),as.Date("2020-01-01"))]

age_PR_female_dt[, year := year(date)]

ten_year_data <- age_PR_female_dt[
  year %in% c(1982, 1992, 2002, 2012, 2022),  
  .(value_sa = mean(value_sa, na.rm = TRUE),  
    value = mean(value, na.rm = TRUE)),       
  by = .(age_group, year)                     
]

ten_year_data[, date := as.Date(paste0(year, "-01-01"))]
ten_year_data[, year := NULL]  

data_15_24 <- ten_year_data[age_group == "15-24"]
age_groups_future <- c("25-34", "35-44", "45-54", "55-64", "65 years and over")
shifts <- c(10, 20, 30, 40, 50)  

for (i in seq_along(age_groups_future)) {
  shift_years <- shifts[i]
  future_age_group <- age_groups_future[i]
  col_name <- paste0("value_sa_", shift_years)  
  
  future_data <- ten_year_data[age_group == future_age_group, .(date = date - years(shift_years), value_sa_future = value_sa)]
  
  # Merge with data_15_24 on date
  data_15_24 <- merge(
    data_15_24, 
    future_data, 
    by = "date", 
    all.x = TRUE
  )
  
  # Rename the merged column
  setnames(data_15_24, "value_sa_future", col_name)
}

data_15_24[,group := c("1982","1992","2002","2012","2022")]
colnames(data_15_24) <- c("date","age_group","15-24","value","25-34","35-44","45-54","55-64","65+","group")
data_15_24[,":=" (date = NULL,value = NULL,`65+` = NULL,age_group=NULL)]


plot_data <- melt(data_15_24,id.vars = "group")
plot_data <- plot_data[!is.na(value)]  

ggplot(plot_data, aes(x = variable, y = value, colour = (group), group = group)) +
  geom_line() + 
  geom_point() +
  plab(c("1982", "1992", "2002", "2012", "2022"), x = rep(3.5, times = 5), y = c(71, 68.5, 66, 64, 61.5)) +
  scale_y_continuous_e61(limits = c(60, 100, 5)) +
  labs_e61(title = "Cohort Female Participation", y = "%", x = "")+scale_colour_manual(values = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))

save_e61("female_cohort.png",res=2,pad_width = 1)

# And for men

age_PR_male_dt[, year := year(date)]
colnames(age_PR_male_dt) <- c("age_group","date","value","value_sa","year")

ten_year_data <- age_PR_male_dt[
  year %in% c(1982, 1992, 2002, 2012, 2022),  
  .(value_sa = mean(value_sa, na.rm = TRUE),  
    value = mean(value, na.rm = TRUE)),       
  by = .(age_group, year)                     
]

ten_year_data[, date := as.Date(paste0(year, "-01-01"))]
ten_year_data[, year := NULL]  # Remove the temporary year column

data_15_24 <- ten_year_data[age_group == "15-24"]
age_groups_future <- c("25-34", "35-44", "45-54", "55-64", "65 years and over")
shifts <- c(10, 20, 30, 40, 50)  

for (i in seq_along(age_groups_future)) {
  shift_years <- shifts[i]
  future_age_group <- age_groups_future[i]
  col_name <- paste0("value_sa_", shift_years)  # e.g., "value_sa_10", "value_sa_20"
  
  # Create the future dataset to merge
  future_data <- ten_year_data[age_group == future_age_group, .(date = date - years(shift_years), value_sa_future = value_sa)]
  
  # Merge with data_15_24 on date
  data_15_24 <- merge(
    data_15_24, 
    future_data, 
    by = "date", 
    all.x = TRUE
  )
  
  # Rename the merged column
  setnames(data_15_24, "value_sa_future", col_name)
}

data_15_24[,group := c("1982","1992","2002","2012","2022")]
colnames(data_15_24) <- c("date","age_group","15-24","value","25-34","35-44","45-54","55-64","65+","group")
data_15_24[,":=" (date = NULL,value = NULL,`65+` = NULL,age_group=NULL)]


plot_data <- melt(data_15_24,id.vars = "group")
plot_data <- plot_data[!is.na(value)]  

ggplot(plot_data, aes(x = variable, y = value, colour = (group), group = group)) +
  geom_line() + 
  geom_point() +
  plab(c("1982", "1992", "2002", "2012", "2022"), x = rep(3.5, times = 5), y = c(71, 68.5, 66, 64, 61.5)) +
  scale_y_continuous_e61(limits = c(60, 100, 5)) +
  labs_e61(title = "Cohort Male Participation", y = "%", x = "")+scale_colour_manual(values = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5]))

save_e61("male_cohort.png",res=2,pad_width = 1)

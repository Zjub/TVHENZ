

library(tidyverse)
library(data.table)
library(theme61)
library(readxl)
library(fixest)
library(readabs)

counts <- read_csv("definition_counts.csv")
setDT(counts)

dt <- counts[variable == "count_twoplus",.(date,NSA_age,JSP_DSP_age)][,other := JSP_DSP_age - NSA_age] # Construct the "core" NSA-JSP ex YA, and the overall ex YA. This makes the target those aged 22-60 (as excluding all aged pension too and those aged over 60)

# Get a population number by month - can use LFS to do this

LS_full <- read_abs(cat_no = "6291.0.55.001") %>% filter(table_title ==  "Table 01. Labour force status by Age, Social marital status, and Sex" )
setDT(LS_full)

colnames(LS_full)

unique(LS_full$series)

filtered_dt <- LS_full[series %like% "Civilian population"]
filtered_dt <- filtered_dt[series %like% "Persons"]
filtered_dt <- filtered_dt[series %like% ">>"]
filtered_dt <- filtered_dt[!series %like% "Married|married"]

filtered_dt <- filtered_dt[,.(date,series,value)][order(series)]
population <- filtered_dt[,.(pop = sum(value)),by=.(date)]

dt2 <- population[dt,on=.(date)][,":=" (NSA_per = NSA_age/(pop*1000),Other_per = other/(pop*1000))]

graph_dt <- melt(dt2[,.(date,NSA_per,Other_per)],id.vars = "date")

ggplot(graph_dt,aes(x=date,y=value,colour=variable)) + geom_line() +
  labs_e61(title = "Long-term Benefit Receipt",subtitle="Percent of population*",footnotes = c("Aged 22-60"),y="") +
  scale_x_date(date_breaks = "3 year",date_labels = "%Y") +
  scale_y_continuous_e61(labels=scales::percent_format(),limits=c(0,0.09,0.02),y_top = FALSE) +
  plab(x=as.Date("2016-01-01"),y=c(0.03,0.07),label = c("NSA-JSP","On other benefits"))

save_e61("Long-term_dep.png",res=2,pad_width = 1)

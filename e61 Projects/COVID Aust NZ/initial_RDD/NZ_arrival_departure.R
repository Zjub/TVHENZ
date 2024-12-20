# Pulling in data on New Zealand arrival dates (from datalab) and New Zealand departures (from ABS website)
# Author: Matt Nolan
# Date made: 09/12/2024
# Last update: 09/12/2024

library(tidyverse)
library(data.table)
library(theme61)
library(readxl)
library(fixest)
library(readabs)
library(seasonal)

### New Zealand departures

dt <- read_abs("3401.0")

colnames(dt)

unique(dt$table_no)
unique(dt$series)

nz_series <- unique(dt$series[grep("New Zealand", dt$series, ignore.case = TRUE)])

## This series is only visitors. We have to rely on the annual release, which doesn't have a catalogue number.


nz_birth_annual <- read_excel("34070DO003_202223 (birth).xlsx", 
                                        sheet = "Table 3.1", range = "A15:U20")


# NZ Citizen permanent arrivals from Australia.
Arrivals_NZ <- read_csv("Arrivals_NZ.csv", 
                        skip = 5)

colnames(Arrivals_NZ) <- c("date","arrivals")

setDT(Arrivals_NZ)

Arrivals_NZ[, date := as.Date(paste0(substr(date, 1, 4), "-", substr(date, 6, 7), "-01"))]
Arrivals_NZ <- Arrivals_NZ[!is.na(date)]

ggplot(Arrivals_NZ,aes(x=date,y=arrivals)) + geom_line()

Arrivals_NZ[, arrivals_ts := ts(arrivals, start = c(year(min(date)), month(min(date))), frequency = 12)]

Arrivals_NZ[, arrivals_sa := final(seas(arrivals_ts))][,type := "arrival"]

Arrivals_NZ[, arrivals_ts := NULL]

avg_line <- mean(Arrivals_NZ[date >= as.Date("2014-01-01") & date < as.Date("2019-10-01")]$arrivals_sa)

print(Arrivals_NZ)

ggplot(Arrivals_NZ[date >= as.Date("2018-01-01") & date < as.Date("2022-01-01")],aes(x=date,y=arrivals_sa,colour=type)) + geom_line() +
  labs_e61(subtitle = "Monthly, seasonally adjusted",sources = c("Stats NZ","e61"),y="",x="",footnotes = c("Arrivals from Australia of New Zealand citizens","Average until September 2019")) +
  annotate("rect", xmin = as.Date("2020-03-20"), xmax = as.Date("2020-06-20"), 
           ymin = -Inf, ymax = Inf, alpha = 0.2, fill = palette_e61(3)[2]) +
  annotate("rect", xmin = as.Date("2020-06-20"), xmax = as.Date("2020-10-20"), 
           ymin = -Inf, ymax = Inf, alpha = 0.2, fill = palette_e61(3)[3]) +
  plab(label=c("First announcement","Second","2014-2019 avg**"),x=c(as.Date("2018-01-01"),as.Date("2021-01-20"),as.Date("2018-01-01")),y=c(800,2200,1800),colour = c(palette_e61(3)[2],palette_e61(3)[3],"black")) +
  scale_y_continuous_e61(limits=c(0,2500,500)) + 
  scale_color_manual(values = palette_e61(3)[1]) + geom_hline(yintercept = avg_line,linetype = "dashed") 

save_e61("NZ_leaving.pdf",pad_width = 1)


### Datalab arrival date distributions

NZ_Arrival_dist <- read_csv("NZ_Arrival_dist.csv")
setDT(NZ_Arrival_dist)

#NZ_Arrival_dist[, arrive_date := as.Date(paste0(year, "-", sprintf("%02d", month), "-01"))]

ggplot(NZ_Arrival_dist,aes(x=arrive_date,y=N)) + geom_col() +
  labs_e61(subtitle = "Arrival month of New Zealanders in Australia",y="",x="",sources=c("ABS","e61"),footnotes=c("Dates for New Zealanders who are in Australia for analysis")) + 
  scale_y_continuous_e61(limits=c(0,800,100))

save_e61("NZ_arr_date.pdf",pad_width = 1)

sum(NZ_Arrival_dist$N)

### Add some more annex stuff on HILDA wages

wage <- read_excel("HIDLA JobSeeker Prev Wage Income.xlsx")
setDT(wage)

# Calculate the weighted average of wages for each group
result <- wage[, .(
  Weighted_Avg_Wage = sum(`Prev. Weekly Wage (2017$)` * Count) / sum(Count)
), by = .(`Single/ Partnered`, `Children?`, JobSeeker)]

# Display the result
print(result)

# Generate LaTeX table using xtable
library(xtable)
latex_table <- xtable(result, digits = 2)
print(latex_table, type = "latex")

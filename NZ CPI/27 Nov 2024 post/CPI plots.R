# Producing plots for blog post of 27 November. 
# Purpose: Showing NZ and Australia inflation
# Date of last edit: 26/11/2024
# Author: Matt Nolan


rm(list=ls())

.libPaths(new = 'C:/Rpackage')


# install and load packages 
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  data.table,
  fst,
  readxl, 
  janitor,
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
  readr,
  readxl,
  rvest,
  collapse,
  remotes,
  mFilter,
  zoo,
  Hmisc,
  seasonal
)

#remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

library(theme61)


#### New Zealand graphs first
# 
# cpi_nz <- read_csv("cpi_nz.csv", skip = 5)
# colnames(cpi_nz) <- c("date","Annual CPI Inflation")
# setDT(cpi_nz)
# cpi_nz <- cpi_nz[1:393] # 393 is Sep 2024
# 
# cpi_nz[, date := as.Date(paste0(sub("Q[1-4]", "", date), "-", 
#                             sprintf("%02d", (as.numeric(sub(".*Q", "", date)) - 1) * 3 + 1), 
#                             "-01"))]
# 
# trim_sep_2024_nz <- read_csv("trim_sep_2024_nz.csv",skip=4)
# colnames(trim_sep_2024_nz) <- c("date","Trim 10%","Trim 20%","Trim 30%")
# setDT(trim_sep_2024_nz)
# 
# trim_sep_2024_nz[1:100]


rbnz_nz <- read_excel("rbnz_nz.xlsx",skip=4)
setDT(rbnz_nz)

# Grab indicies we want

cpi_nz <- rbnz_nz[,1:2]
colnames(cpi_nz) <- c("date","CPI")
setkey(cpi_nz, date)

cpi_nz[, CPI_annual_change := (CPI / data.table::shift(CPI, n = 4) - 1) * 100]

cpi_nz_extend <- rbnz_nz[,c(1,26,30:31)]
colnames(cpi_nz_extend) <- c("date","trimmed_qpc","FM_annual","SFM_annual")

cpi_nz <- cpi_nz_extend[cpi_nz,on=.(date)][date >= as.Date("2001-03-01")]

cpi_nz[, trimmed := 1] 
cpi_nz[, trimmed := cumsum(c(1, head(trimmed_qpc, -1)/100)), by = NULL]

cpi_nz[, trimmed_annual_change := (trimmed / data.table::shift(trimmed, n = 4) - 1) * 100]

cpi_nz$date <- as.Date(cpi_nz$date)

ggplot(cpi_nz,aes(x=date,y=CPI_annual_change)) + geom_line() +
  geom_hline(yintercept = c(1, 3), linetype = "dashed", color = "black") +
  annotate("rect", xmin = min(cpi_nz$date), xmax = max(cpi_nz$date),
           ymin = 1, ymax = 3, alpha = 0.2, fill = "orange") +
  scale_x_date(expand = c(0,0)) + scale_y_continuous_e61(limits = c(0,8,2)) +
  labs_e61(title = "New Zealand Inflation",y="%",x="") + plab(x=c(as.Date("2013-01-01"),as.Date("2016-01-01")),y=c(3.5,7),label=c("Target Range","Article Date"),colour=c("orange","black")) + 
  geom_vline(linetype="dotted",xintercept = as.Date("2022-03-01"))

save_e61("NZ Inflation.png",res=2)

ggplot(melt(cpi_nz[,.(date,CPI_annual_change,trimmed_annual_change,FM_annual,SFM_annual)],id.vars = "date"),aes(x=date,y=value,colour=variable))+geom_line()+
  geom_hline(yintercept = c(1, 3), linetype = "dashed", color = "black") +
  annotate("rect", xmin = min(cpi_nz$date), xmax = max(cpi_nz$date),
           ymin = 1, ymax = 3, alpha = 0.2, fill = "orange") +
  scale_x_date(expand = c(0,0)) + scale_y_continuous_e61(limits = c(0,8,2)) +
  labs_e61(title = "New Zealand Inflation",y="%",x="") + plab(x=c(as.Date("2013-01-01"),as.Date("2016-01-01")),y=c(3.5,7),label=c("Target Range","Article Date"),colour=c("orange","black")) + 
  geom_vline(linetype="dotted",xintercept = as.Date("2022-03-01")) + plab(x=c(as.Date("2002-01-01"),as.Date("2002-01-01"),as.Date("2002-01-01"),as.Date("2002-01-01")),y=c(7,6.4,5.8,5.2),label=c("CPI","Trimmed","Factor Model","Sectoral Model"),colour=c(palette_e61(4)[1],palette_e61(4)[2],palette_e61(4)[3],palette_e61(4)[4]))


save_e61("NZ Inflation Detailed.png",res=2)


#### Australia

cpi_aus <- read_abs(cat_no = "6401.0")
setDT(cpi_aus)
unique(cpi_aus$table_title)

cpi_aus[, table_name := sub("^(TABLES? \\d+).*", "\\1", table_title)]

unique(cpi_aus[table_name == "TABLE 8"]$series)

cpi_aus <- cpi_aus[table_name == "TABLE 8" & series %in% c("Percentage Change from Corresponding Quarter of Previous Year ;  All groups CPI ;  Australia ;" ,"Percentage Change from Corresponding Quarter of Previous Year ;  Trimmed Mean ;  Australia ;" ) & date >= as.Date("2001-01-01")] # As trimmed mean is seasonally adjusted, using SA CPI:  "Percentage Change from Corresponding Quarter of Previous Year ;  All groups CPI, seasonally adjusted ;  Australia ;" 

cpi_aus[, category := ifelse(
  grepl("All groups CPI", series), "CPI",
  ifelse(grepl("Trimmed Mean", series), "Trimmed", NA)
)]

ggplot(cpi_aus,aes(x=date,y=value,colour=category)) + geom_line()+
  geom_hline(yintercept = c(2, 3), linetype = "dashed", color = "black") +
  annotate("rect", xmin = min(cpi_aus$date), xmax = max(cpi_aus$date),
           ymin = 2, ymax = 3, alpha = 0.2, fill = "orange") +
  scale_x_date(expand = c(0,0)) + scale_y_continuous_e61(limits = c(-1,9,2)) + geom_hline(yintercept = 0) +
  labs_e61(title = "Australian Inflation",y="%",x="") + plab(x=c(as.Date("2013-01-01"),as.Date("2002-01-01"),as.Date("2002-01-01")),y=c(3.5,8.5,7.5),label=c("Target Range","CPI","Trimmed"),colour=c("orange",palette_e61(2)[1],palette_e61(2)[2])) 

save_e61("Aus Inflation.png",res=2)


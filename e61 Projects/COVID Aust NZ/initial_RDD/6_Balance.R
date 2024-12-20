# Script to pull together the balance statistics for our exercise
# Author: Matt Nolan
# Date made: 29/11/2024
# Last update: 3/12/2024

library(tidyverse)
library(data.table)
library(theme61)
library(readxl)
library(readr)
library(fixest)

## Age balance

age_balance <- read_csv("age_balance.csv")
setDT(age_balance)

ggplot(age_balance,aes(x=age_bin,y=result_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,10000,2000)) + plab(c("Australian","New Zealander"),x=c("35-44","35-44"),y=c(8800,7200)) +
  labs_e61(subtitle = "Matched JSP dataset",y="",x="",sources=c("ABS","e61")) # title = "Age distribution",

save_e61("Age.pdf", pad_width = 1)

sum(age_balance$result_data)/2
sum(age_balance$matched_data)/2

ggplot(age_balance,aes(x=age_bin,y=matched_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,30000,6000)) + plab(c("Australian","New Zealander"),x=c("35-44","35-44"),y=c(27000,23000)) +
  labs_e61(,subtitle = "Full Matched dataset",y="",x="",sources=c("ABS","e61")) # title = "Age distribution"

save_e61("Age_full.pdf", pad_width = 1)

# Age shares

age_shares <- age_balance[, .(
  total_result_data = sum(result_data),
  total_matched_data = sum(matched_data),
  total_all_data = sum(all_data)
), by = age_bin]

age_shares[, `:=`(
  share_result_data = total_result_data / sum(total_result_data),
  share_matched_data = total_matched_data / sum(total_matched_data),
  share_all_data = total_all_data / sum(total_all_data)
)]

age_plot <- melt(age_shares[,.(age_bin,share_result_data,share_matched_data,share_all_data)],id.vars = "age_bin")

ggplot(age_plot,aes(x=age_bin,y=value,fill=variable)) + geom_col(position="dodge") + 
  plab(c("Matched JSP","Matched Total","Total Aus-NZ"),x = c("45-54","45-54","45-54"),y=c(0.28,0.25,0.22)) +
  scale_y_continuous_e61(labels=scales::percent_format(),limits = c(0,0.39,0.1),y_top = FALSE) +
  labs_e61(subtitle = "Proportion of each sample",x="",y="",sources=c("ABS","e61")) # title = "Proportion of sample by age"

save_e61("Age_prop.pdf", pad_width = 1)

## Industry balance

ind_balance <- read_csv("ind_balance.csv")
setDT(ind_balance)

ggplot(ind_balance,aes(x=industry,y=result_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,8000,2000)) + plab(c("Australian","New Zealander"),x=c("A","A"),y=c(5400,4600)) +
  labs_e61(subtitle = "Matched JSP dataset",y="",x="",sources=c("ABS","e61")) # title = "Industry distribution",

save_e61("Industry.pdf", pad_width = 1)

ggplot(ind_balance,aes(x=industry,y=matched_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,20000,4000)) + plab(c("Australian","New Zealander"),x=c("A","A"),y=c(17000,14000)) +
  labs_e61(subtitle = "Full Matched dataset",y="",x="",sources=c("ABS","e61")) # title = "Industry distribution",

save_e61("Industry_full.pdf", pad_width = 1)

# Industry shares

industry_shares <- ind_balance[, .(
  total_result_data = sum(result_data),
  total_matched_data = sum(matched_data),
  total_all_data = sum(all_data)
), by = industry]

industry_shares[, `:=`(
  share_result_data = total_result_data / sum(total_result_data),
  share_matched_data = total_matched_data / sum(total_matched_data),
  share_all_data = total_all_data / sum(total_all_data)
)]

industry_plot <- melt(industry_shares[,.(industry,share_result_data,share_matched_data,share_all_data)],id.vars = "industry")

ggplot(industry_plot,aes(x=industry,y=value,fill=variable)) + geom_col(position="dodge") + 
  plab(c("Matched JSP","Matched Total","Total Aus-NZ"),x = c("A","A","A"),y=c(0.28,0.25,0.22)) +
  scale_y_continuous_e61(labels=scales::percent_format(),limits = c(0,0.32,0.1),y_top = FALSE) +
  labs_e61(subtitle = "Proportion of sample",x="",y="",sources=c("ABS","e61"))

save_e61("Industry_share.pdf", pad_width = 1)

## Occupation balance

occ_balance <- read_csv("occ_balance.csv")
setDT(occ_balance)

ggplot(occ_balance,aes(x=occ,y=result_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,7000,2000),y_top = FALSE) + plab(c("Australian","New Zealander"),x=c(0,0),y=c(5400,4600)) +
  labs_e61(subtitle = "Matched JSP dataset",y="",x="",sources=c("ABS","e61")) +
  scale_x_continuous(breaks = seq(0, 9, 1)) # title = "Occupation distribution",

save_e61("Occupation.pdf", pad_width = 1)

ggplot(occ_balance,aes(x=occ,y=matched_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,22000,4000),y_top = FALSE) + plab(c("Australian","New Zealander"),x=c(0,0),y=c(19000,17000)) +
  labs_e61(subtitle = "Full Matched dataset",y="",x="",sources=c("ABS","e61")) +
  scale_x_continuous(breaks = seq(0, 9, 1)) # title = "Occupation distribution",

save_e61("Occupation_full.pdf", pad_width = 1)

# Occupation shares

occ_shares <- occ_balance[, .(
  total_result_data = sum(result_data),
  total_matched_data = sum(matched_data),
  total_all_data = sum(all_data)
), by = occ]

occ_shares[, `:=`(
  share_result_data = total_result_data / sum(total_result_data),
  share_matched_data = total_matched_data / sum(total_matched_data),
  share_all_data = total_all_data / sum(total_all_data)
)]

occ_plot <- melt(occ_shares[,.(occ,share_result_data,share_matched_data,share_all_data)],id.vars = "occ")

ggplot(occ_plot,aes(x=occ,y=value,fill=variable)) + geom_col(position="dodge") + 
  plab(c("Matched JSP","Matched Total","Total Aus-NZ"),x = c(4,4,4),y=c(0.28,0.25,0.22)) +
  scale_y_continuous_e61(labels=scales::percent_format(),limits = c(0,0.38,0.1),y_top = FALSE) +
  labs_e61(subtitle = "Proportion of sample",x="",y="",sources=c("ABS","e61")) +
  scale_x_continuous(breaks = c(0:9))

save_e61("Occupation_share.pdf", pad_width = 1)

## region balance

region_balance <- read_csv("region_balance.csv")
setDT(region_balance)

ggplot(region_balance,aes(x=region,y=result_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,9000,2000),y_top = FALSE) + plab(c("Australian","New Zealander"),x=c(4,4),y=c(7400,6600)) +
  labs_e61(subtitle = "Matched JSP dataset",y="",x="",sources=c("ABS","e61")) # title = "Region distribution",

save_e61("Region.pdf", pad_width = 1)

ggplot(region_balance,aes(x=region,y=matched_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,30000,7000),y_top = FALSE) + plab(c("Australian","New Zealander"),x=c(4,4),y=c(20000,17000)) +
  labs_e61(subtitle = "Full Matched dataset",y="",x="",sources=c("ABS","e61")) # title = "Region distribution",

save_e61("Region_full.pdf", pad_width = 1)

# Also add relative size of each region in each of the samples. [will want to do this for each balance - so there are three balance plots, two comparing the balance of the samples, and one comparing it to the aggregate data]

region_shares <- region_balance[, .(
  total_result_data = sum(result_data),
  total_matched_data = sum(matched_data),
  total_all_data = sum(all_data)
), by = region]

region_shares[, `:=`(
  share_result_data = total_result_data / sum(total_result_data),
  share_matched_data = total_matched_data / sum(total_matched_data),
  share_all_data = total_all_data / sum(total_all_data)
)]

region_plot <- melt(region_shares[,.(region,share_result_data,share_matched_data,share_all_data)],id.vars = "region")

ggplot(region_plot,aes(x=region,y=value,fill=variable)) + geom_col(position="dodge") + 
  plab(c("Matched JSP","Matched Total","Total Aus-NZ"),x = c("4","4","4"),y=c(0.28,0.25,0.22)) +
  scale_y_continuous_e61(labels=scales::percent_format(),limits = c(0,0.38,0.1),y_top = FALSE) +
  labs_e61(subtitle = "Proportion of sample",x="",y="",sources=c("ABS","e61"))

save_e61("Region_share.pdf", pad_width = 1)

## sex balance

sex_balance <- read_csv("sex_balance.csv")
setDT(sex_balance)

ggplot(sex_balance,aes(x=combined_gender,y=result_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,19000,2000),y_top = FALSE) + plab(c("Australian","New Zealander"),x=c(1,1),y=c(17000,15000)) +
  labs_e61(subtitle = "Matched JSP dataset",y="",x="",sources=c("ABS","e61")) +
  scale_x_continuous(breaks = c(1:2)) # title = "Sex distribution",

save_e61("gender.pdf", pad_width = 1)

ggplot(sex_balance,aes(x=combined_gender,y=matched_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,48000,10000),y_top = FALSE) + plab(c("Australian","New Zealander"),x=c(1.5,1.5),y=c(45000,42000)) +
  labs_e61(subtitle = "Full Matched dataset",y="",x="",sources=c("ABS","e61")) +
  scale_x_continuous(breaks = c(1:2)) # title = "Sex distribution",

save_e61("gender_full.pdf", pad_width = 1)

# Sex shares

sex_shares <- sex_balance[, .(
  total_result_data = sum(result_data),
  total_matched_data = sum(matched_data),
  total_all_data = sum(all_data)
), by = combined_gender]

sex_shares[, `:=`(
  share_result_data = total_result_data / sum(total_result_data),
  share_matched_data = total_matched_data / sum(total_matched_data),
  share_all_data = total_all_data / sum(total_all_data)
)]

sex_plot <- melt(sex_shares[,.(combined_gender,share_result_data,share_matched_data,share_all_data)],id.vars = "combined_gender")

ggplot(sex_plot[combined_gender == 1],aes(x=combined_gender,y=value,fill=variable)) + geom_col(position="dodge") + 
  plab(c("Matched JSP","Matched Total","Total Aus-NZ"),x = c(0.65,0.95,1.25),y=c(0.68,0.68,0.68)) +
  scale_y_continuous_e61(labels=scales::percent_format(),limits = c(0,1,0.1),y_top = FALSE) +
  labs_e61(subtitle = "Proportion of sample who is male",x="",y="",sources=c("ABS","e61")) + 
  theme(axis.text.x = element_blank()) + theme(axis.ticks.x = element_blank())

save_e61("gender_share.pdf", pad_width = 1)

## Spouse balance

spouse_balance <- read_csv("spouse_balance.csv")
setDT(spouse_balance)

# ggplot(spouse_balance,aes(x=spouse,y=result_data,fill=as.factor(nz))) + geom_col(position="dodge") +
#   scale_y_continuous_e61(limits = c(0,08000,2000),y_top = FALSE) + plab(c("Australian","New Zealander"),x=c(4,4),y=c(7400,6600)) +
#   labs_e61(title = "spouse distribution",subtitle = "Matched JSP dataset",y="",x="",sources=c("ABS","e61"))
# 
# ggplot(spouse_balance,aes(x=spouse,y=matched_data,fill=as.factor(nz))) + geom_col(position="dodge") +
#   scale_y_continuous_e61(limits = c(0,30000,7000),y_top = FALSE) + plab(c("Australian","New Zealander"),x=c(1.6,1.8),y=c(7000,7000)) +
#   labs_e61(title = "spouse distribution",subtitle = "Full Matched dataset",y="",x="",sources=c("ABS","e61")) + coord_flip()

# spouse shares - for spouse do the Aus and NZ comparisons as shares as well

spouse_type_shares <- spouse_balance[, .(
  total_result_data = sum(result_data),
  total_matched_data = sum(matched_data),
  total_all_data = sum(all_data)
), by = .(spouse,nz)]

spouse_type_shares[, `:=`(
  `Matched JSP` = total_result_data / sum(total_result_data),
  share_matched_data = total_matched_data / sum(total_matched_data),
  share_all_data = total_all_data / sum(total_all_data)
),by=nz]

spouse_type_plot <- melt(spouse_type_shares[,.(spouse,nz,`Matched JSP`,`Matched Total` = share_matched_data, `Total Aus-NZ` = share_all_data)],id.vars = c("spouse","nz"))

ggplot(spouse_type_plot[spouse == 1],aes(x=variable,y=value,fill=as.factor(nz))) + geom_col(position="dodge") + 
  plab(c("Australian","New Zealander"),x = c(1,1),y=c(0.58,0.48)) +
  scale_y_continuous_e61(labels=scales::percent_format(),limits = c(0,0.68,0.1),y_top = FALSE) +
  labs_e61(subtitle = "Proportion of sample who report a spouse",x="",y="",sources=c("ABS","e61"))

save_e61("spouse_share.pdf", pad_width = 1)


# spouse_shares <- spouse_balance[, .(
#   total_result_data = sum(result_data),
#   total_matched_data = sum(matched_data),
#   total_all_data = sum(all_data)
# ), by = spouse]
# 
# spouse_shares[, `:=`(
#   share_result_data = total_result_data / sum(total_result_data),
#   share_matched_data = total_matched_data / sum(total_matched_data),
#   share_all_data = total_all_data / sum(total_all_data)
# )]
# 
# spouse_plot <- melt(spouse_shares[,.(spouse,share_result_data,share_matched_data,share_all_data)],id.vars = "spouse")
# 
# ggplot(spouse_plot[spouse == 1],aes(x=spouse,y=value,fill=variable)) + geom_col(position="dodge") + 
#   plab(c("Matched JSP","Matched Total","Total Aus-NZ"),x = c(1,1,1),y=c(0.78,0.68,0.58)) +
#   scale_y_continuous_e61(labels=scales::percent_format(),limits = c(0,1,0.1),y_top = FALSE) +
#   labs_e61(title = "Proportion of sample who report a spouse",x="",y="",sources=c("ABS","e61")) + 
#   scale_x_continuous()

## Income ventile

ventile_pre_balance <- read_csv("ventile_pre_balance.csv")

setDT(ventile_pre_balance)

ggplot(ventile_pre_balance,aes(x=ventile_pre_inc,y=result_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,2400,500),y_top = FALSE) + plab(c("Australian","New Zealander"),x=c(4,4),y=c(1900,1700)) +
  labs_e61(subtitle = "Matched JSP dataset",y="",x="",sources=c("ABS","e61")) # title = "Pre-income distribution",

save_e61("Incomedist.pdf", pad_width = 1)

ggplot(ventile_pre_balance,aes(x=ventile_pre_inc,y=matched_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,7800,1000),y_top = FALSE) + plab(c("Australian","New Zealander"),x=c(5,5),y=c(6400,5800)) +
  labs_e61(subtitle = "Full Matched dataset",y="",x="",sources=c("ABS","e61")) # title = "Pre-income distribution",

save_e61("Incomedist_full.pdf", pad_width = 1)

## Spouse income ventile

ventile_spouse_pre_balance <- read_csv("ventile_spouse_pre_balance.csv")

setDT(ventile_spouse_pre_balance)

ggplot(ventile_spouse_pre_balance,aes(x=spouse_ventile_pre_inc,y=result_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,2400,500),y_top = FALSE) + plab(c("Australian","New Zealander"),x=c(4,4),y=c(1900,1700)) +
  labs_e61(subtitle = "Matched JSP dataset",y="",x="",sources=c("ABS","e61")) # title = "Spouse pre-income distribution",

save_e61("Incomespouse.pdf", pad_width = 1)

ggplot(ventile_spouse_pre_balance,aes(x=spouse_ventile_pre_inc,y=matched_data,fill=as.factor(nz))) + geom_col(position="dodge") +
  scale_y_continuous_e61(limits = c(0,7800,1000),y_top = FALSE) + plab(c("Australian","New Zealander"),x=c(5,5),y=c(6400,5800)) +
  labs_e61(subtitle = "Full Matched dataset",y="",x="",sources=c("ABS","e61"))  # title = "Spouse pre-income distribution",

save_e61("Incomespouse_full.pdf", pad_width = 1)


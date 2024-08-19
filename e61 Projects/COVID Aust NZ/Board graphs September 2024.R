## A script that creates the plot for the board
# Author: Matt Nolan
# Last edit: 19/08/2024

### Libraries and import data ----

library(tidyverse)
library(data.table)
library(theme61)

rm(list=ls())

data_loc <- "Cleared data/2021-097_o_0216_cleared"
file_list <- list.files(data_loc, pattern = "*.csv", full.names = TRUE)

combined_data <- rbindlist(lapply(file_list, function(file) {
  
  data <- fread(file)
  
  # Add a column with names
  data[, file_name := gsub("^.*\\/|\\.csv$", "", file)][, month := sub(".*_(\\d{2})$", "\\1", file_name)]
  
  return(data)
}), use.names = TRUE, fill = TRUE)
setDT(combined_data)

### Create graphs ----

ggplot(combined_data[time > 30 & time <= 180],aes(x=time,y=survival_prob.1,colour=file_name)) + geom_line()

ggplot(combined_data[time > 30 & time <= 180],aes(x=time,y=survival_prob.2,colour=file_name)) + geom_line()

times <- seq(28,98,by=7)

df <- combined_data[time %in% times]

df[,month:=as.numeric(month)]

df[,category := fifelse(month <= 02,"pre-CVDS",
                        fifelse(month <= 06,"pre-announce_ext","post-announce_ext"))]

## This is for the combined data
# df <- df[,.(initial = n.risk[1]),by=.(month)][df,on=.(month)]
# 
# df <- df[,.(initial = sum(initial),remain = sum(n.risk) - sum(n.event)),by=.(category,time)][,surv := remain/initial]
## As we don't have the group separated, we will just take the "average of monthly survival rates

# df <- df[,.(avg_NZ = mean(survival_prob.1),avg_Aus = mean(survival_prob.2)),by=.(time,category)]
# 
# df <- melt(df,id.vars = c("time","category"))
# 
# ggplot(df,aes(x=time,y=value,colour=variable)) + geom_line() + facet_wrap(~category) + theme_e61(legend="bottom")

df <- df[,.(avg_NZ = mean(survival_prob.1),avg_Aus = mean(survival_prob.2)),by=.(time,category)][,diff := avg_Aus - avg_NZ]

ggplot(df,aes(x=time,y=diff*100,colour = category)) + 
  geom_line() + 
  plot_label(label = c("Pre-COVID Supplement","Before extension","After extension"),x=c(60,60,60),y=c(-0.7,-1.2,-1.7),colour = c(palette_e61(3)[3],palette_e61(3)[2],palette_e61(3)[1])) +
  scale_y_continuous_e61(limits=c(-6,0.00,by=1)) +
  labs_e61(title = "Supplement entrants less likely to reenter work",subtitle = "Difference in Job Finding Rates*",footnotes = c("Australian minus Kiwi joblessness exit rates","The groups reflect the date of entry into joblessness relative to the annoucement date of the policy"),sources = c("ABS","e61"),y="%",x="Days from job exit")

save_e61("Survival_diff.png",res=2,pad_width = 1)




# Topic: Takes background data and produces plots
# Author: Matt Nolan
# Created: 19/10/2025
# Last edit: 14/11/2025
# Last editor: Matt Nolan
## Double check the plots are edited, consistent, as had backup issues


rm(list=ls())

options(pkgType = "win.binary")
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

## Figure 1: Government spending

f1_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "Figure_1")

setDT(f1_dt)

ggplot(f1_dt,aes(x=fin_year,y=prop*100,colour=variable)) + geom_line() +
  scale_y_continuous_e61(limits = c(32,48,4)) +
  labs_e61(title = "Consolidated government expenditure",
           y = "% GDP",
           sources = c("ABS","e61"),
           footnotes = c("Expenditure includes current expenses and net acquisition of non-financial assets")) + 
  plab(c("Current expenses","Total expenditure"),x=c(2010,2008),y=c(33,41))

save_e61("Figure_1.png",res=2)
save_e61("Report_graph/Figure_1.svg")


## Figure 2: Fiscal balance - also Figure 1 in short report

f2_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "Figure_2")

setDT(f2_dt)

f2_dt[,variable := factor(variable,levels = c("States","Commonwealth","National"))]

ggplot(f2_dt,aes(x=year,y=value,colour=variable)) + geom_line() + geom_line() +
  labs_e61(title = "Persistent Fiscal Imbalance",
           y = "Fiscal balance, % GDP",
           sources =c("PBO","Budget 2026","e61"),
           footnotes = c("Uses the 2025 National Fiscal Outlook by the PBO, updated with Budget 2026 estimates.","Plot to the right of the dashed line reflect Budget/PBO estimates.")) + # , with estimates updated for the 2026 Final Budget Outcome - checked and this isn't in FBO, which is more cut down.
  geom_hline(yintercept = 0) +
  plab(c("States","Federal","National"),y=c(-3,-5,-7),x=c(2003,2003,2003)) + 
  geom_vline(xintercept = 2024,linetype = "dashed") +
  scale_y_continuous_e61(limits = c(-12,4,4)) +
  scale_x_continuous_e61(limits = c(2003,2028,5))

save_e61("Figure_2.png",res=2)
save_e61("Report_graph/Figure_2.svg")

## Update Figure 2: Fiscal balance - also Figure 1 in short report

f2_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "Figure_2 (Feb version)")

setDT(f2_dt)

f2_dt[,variable := factor(variable,levels = c("States","Commonwealth","National"))]

ggplot(f2_dt,aes(x=year,y=value,colour=variable)) + geom_line() + geom_line() +
  labs_e61(title = "Persistent Fiscal Imbalance",
           y = "Fiscal balance, % GDP",
           sources =c("PBO","Budget 2026","e61"),
           footnotes = c("Uses the 2026 National Fiscal Outlook by the PBO.","Plot to the right of the dashed line reflect Budget/PBO estimates.")) + # , with estimates updated for the 2026 Final Budget Outcome - checked and this isn't in FBO, which is more cut down.
  geom_hline(yintercept = 0) +
  plab(c("States","Federal","National"),y=c(-3,-6,-9),x=c(2003,2003,2003)) + 
  geom_vline(xintercept = 2025,linetype = "dashed") +
  scale_y_continuous_e61(limits = c(-12,4,4)) +
  scale_x_continuous_e61(limits = c(2003,2029,5))

save_e61("Figure_2_new.png",res=2)
save_e61("Report_graph/Figure_2_new.svg",auto_scale = FALSE)

## Figure 3: Net debt

f3_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "Figure_3")

setDT(f3_dt)

ggplot(melt(f3_dt,id.vars = "Year"),aes(x=Year,y=value,colour =variable)) + geom_line() +
  scale_y_continuous_e61(limits = c(-10,40,10)) + geom_hline(yintercept = 0) +
  labs_e61(title = "Net debt projected to keep rising",
           y = "% GDP",
           sources = c("PBO","e61"),
           footnotes = c("Actuals and projections come from the 2024/25 National Fiscal Outlook","Plot to the right of the dashed line reflect Budget/PBO estimates.")) +
  geom_vline(xintercept = 2023,linetype = "dashed") +
  plab(c("State","Federal","National"),x=c(2013,2005,2005),y=c(-5,25,35))

save_e61("Figure_3.png",res=2)
save_e61("Report_graph/Figure_3.svg")

ggplot(melt(f3_dt,id.vars = "Year"),aes(x=Year,y=value,colour =variable)) + geom_line() +
  scale_y_continuous_e61(limits = c(-10,40,10)) + geom_hline(yintercept = 0) +
  labs_e61(title = "Net debt projected to keep rising",
           y = "% GDP",
           sources =c("PBO","Budget 2026","e61"),
           footnotes = c("Actuals and projections come from the 2024/25 National Fiscal Outlook","Plot to the right of the dashed line reflect Budget/PBO estimates.")) +
  geom_vline(xintercept = 2023,linetype = "dashed") +
  plab(c("State","Federal","National"),x=c(2013,2005,2005),y=c(-5,25,35)) +
  scale_x_continuous_e61(limits = c(2003,2028,5))

save_e61("new_Figure_3.png",res=2)
save_e61("Report_graph/new_Figure_3.svg")

## Update Figure 3: Net debt

f3_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "Figure_3 (Feb version)")

setDT(f3_dt)

ggplot(melt(f3_dt,id.vars = "Year"),aes(x=Year,y=value,colour =variable)) + geom_line() +
  scale_y_continuous_e61(limits = c(-10,40,10)) + geom_hline(yintercept = 0) +
  labs_e61(title = "Net debt projected to keep rising",
           y = "% GDP",
           sources = c("PBO","e61"),
           footnotes = c("Actuals and projections come from the 2024/25 National Fiscal Outlook","Plot to the right of the dashed line reflect Budget/PBO estimates.")) +
  geom_vline(xintercept = 2025,linetype = "dashed") +
  plab(c("State","Federal","National"),x=c(2013,2005,2005),y=c(-5,25,35))

save_e61("Figure_3_new.png",res=2)
save_e61("Report_graph/Figure_3_new.svg")

ggplot(melt(f3_dt,id.vars = "Year"),aes(x=Year,y=value,colour =variable)) + geom_line() +
  scale_y_continuous_e61(limits = c(-10,40,10)) + geom_hline(yintercept = 0) +
  labs_e61(title = "Net debt projected to keep rising",
           y = "% GDP",
           sources =c("PBO","Budget 2026","e61"),
           footnotes = c("Actuals and projections come from the 2026 National Fiscal Outlook","Plot to the right of the dashed line reflect Budget/PBO estimates.")) +
  geom_vline(xintercept = 2025,linetype = "dashed") +
  plab(c("State","Federal","National"),x=c(2013,2005,2005),y=c(-5,25,35)) +
  scale_x_continuous_e61(limits = c(2003,2029,5))

save_e61("new_Figure_3_new.png",res=2)
save_e61("Report_graph/new_Figure_3_new.svg")


## Figure 4: General Government Debt across OECD

f4_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "Figure_4")

setDT(f4_dt)

f4_dt[, `Reference area` := factor(`Reference area`, levels = `Reference area`)]

ggplot(f4_dt, aes(x = `Reference area`, y = OBS_VALUE, fill = colour_flag)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Australia" = "gold", "Other" = palette_e61(2)[2])) +
  labs_e61(x = NULL, y = "% GDP",title = "General Government Gross Debt (2023)",
           sources = c("e61","OECD"))

save_e61("Figure_4.png",res=2)
save_e61("Report_graph/Figure_4.svg")

ggplot(f4_dt, aes(x = `Reference area`, y = OBS_VALUE, fill = colour_flag)) +
  geom_col(width = 0.7) +
  coord_flip() +
  format_flip() +
  scale_fill_manual(values = c("Australia" = "gold", "Other" = palette_e61(2)[2])) +
  labs_e61(x = NULL, y = "% GDP",title = "General government gross debt (2023)",
           sources = c("e61","OECD"))

save_e61("Report_graph/new_Figure_4.svg")


## Figure 5a: Expenditure by government level

f5a_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "Figure_5a")

setDT(f5a_dt)
f5a_dt[,Level := factor(Level,levels = c("Non-Federal","Federal"))]

ggplot(f5a_dt,aes(x=Year,y=value,colour=Level)) + geom_line() +
  labs_e61(
    title = "Expenditure by Government level",
    y = "% GDP",
    sources = c("e61","OECD"),
    footnotes = c("Spending shares based on OECD standard consolidation.","FY has been shifted forward by one relative to OECD reporting - due to the Australian financial year starting six months later than other countries.")
  ) + scale_y_continuous_e61(limits = c(15,27.5,2.5)) +
  plab(c("Non-Federal","Federal"),x=c(1999,1999),y=c(18,21.5))

save_e61("Figure_5a.png",res=2)
save_e61("Report_graph/Figure_5a.svg")

## Figure 5b: Contribution growth by government level

f5b_dt <- read_excel("Graph_data.xlsx", 
                     sheet = "Figure_5b")

setDT(f5b_dt)
f5b_dt[,Level := factor(Level,levels = c("Non-Federal","Federal"))]

ggplot(f5b_dt, aes(x = Year, y = change_pp, fill = Level)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) +
  labs_e61(
    title = "Government Levels contribution to spending growth",
    subtitle = paste0("Change (% of GDP) relative to 1999"),
    y = "",
    sources = c("e61","OECD"),
    footnotes = c("Spending shares based on OECD standard consolidation.","FY has been shifted forward by one relative to OECD reporting - due to the Australian financial year starting six months later than other countries.")
  ) +
  scale_y_continuous_e61() +
  plab(c("Non-Federal","Federal"),x=c(1998,1998),y=c(3.5,5))

save_e61("Figure_5b.png",res=2)
save_e61("Report_graph/Figure_5b.svg")


## New Figure 5:

p5a <- ggplot(f5a_dt,aes(x=Year,y=value,colour=Level)) + geom_line() +
  labs_e61(
    title = "A. Expenditure by Government level",
    y = "% GDP"
  ) + scale_y_continuous_e61(limits = c(15,27.5,2.5)) +
  plab(c("Non-Federal","Federal"),x=c(1999,1999),y=c(18,21.5))

p5b <- ggplot(f5b_dt, aes(x = Year, y = change_pp, fill = Level)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) +
  labs_e61(
    title = "B. Contribution to growth",
    subtitle = paste0("Change (% of GDP) relative to 1999"),
    y = "") +
  scale_y_continuous_e61() +
  plab(c("Non-Federal","Federal"),x=c(1998,1998),y=c(3.5,5))

save_e61("Figure_5.png",p5a,p5b,res=2,sources = c("e61","OECD"), title = "Consolidated Expenditure",
         footnotes = c("Spending shares based on OECD standard consolidation.","FY has been shifted forward by one relative to OECD reporting - due to the Australian financial year starting six months later than other countries."))
save_e61("Report_graph/Figure_5.svg",p5a,p5b,sources = c("e61","OECD"), title = "Consolidated Expenditure",
         footnotes = c("Spending shares based on OECD standard consolidation.","FY has been shifted forward by one relative to OECD reporting - due to the Australian financial year starting six months later than other countries."))

## Figure 6: Untied data ratio

f6_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "Figure_6")

setDT(f6_dt)

ggplot(f6_dt,aes(x=Year,y=Untied_Ratio*100)) + geom_line() + scale_y_continuous_e61(limits = c(0,80,20)) +
  labs_e61(title = "Share of Federal funding that is untied.",
           y= "% of total funding",
           sources = c("Budget Papers 3","e61"),
           footnotes = c("General funding includes both GST revenue and general revenue assistance at the state and local level."))

save_e61("Figure_6.png",res=2)
save_e61("Report_graph/Figure_6.svg")


## Figure 6: Revenue by level

f7_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "Figure_7")

setDT(f7_dt)

f7_dt[,level := factor(level,levels = c("State","Federal"))]

ggplot(f7_dt,aes(x=year,y=Revenue,colour=level)) + geom_line() + 
  labs_e61(title = "Revenue by Government level",
           y= "% GDP",
           sources = c("OECD","e61")) +
  scale_y_continuous_e61(limits=c(10,26,4)) +
  plab(c("Non-Federal","Federal"),x=c(2009,2009),y=c(13,21))

save_e61("Figure_7.png",res=2)
save_e61("Report_graph/Figure_7.svg")

## Figure 8a: Cross-country expenditure

f8a_dt <- read_excel("Graph_data.xlsx", 
                     sheet = "Figure_8a")

setDT(f8a_dt)

total_order <- f8a_dt[, .(total_value = sum(value, na.rm=TRUE)), by = Country][order(total_value)]$Country
f8a_dt[, Country := factor(Country, 
                                    levels = total_order)]
f8a_dt[, Government_level := factor(Government_level, 
                                             levels = c("Non-Federal","Federal"))]


ggplot(f8a_dt, aes(x = Country, y = value*100, fill = Government_level)) +
  geom_col() +
  geom_col(
    data = f8a_dt[Country == "Australia"],
    aes(x = Country, y = value*100, group = Government_level),
    fill = NA, colour = "gold", linewidth = 2, inherit.aes = TRUE
  ) +
  coord_flip() +
  labs_e61(title = "Expenditure: Cross-country (2022)",
           y = "% GDP",
           subtitle = "",
           sources = c("e61","OECD"),
           footnotes = c("Dark blue is spending by Federal Govt % GDP. Light blue is additional spending attributed to non-Federal entities.","FY has been shifted forward by one relative to OECD reporting - due to the Australian financial year starting six months later than other countries.")) +
  plab(c("Non-Federal","Federal"),x=c(1.5,3.5),y=c(40,40))

save_e61("Figure_8a.png",res=2)
save_e61("Report_graph/Figure_8a.svg")

## Figure 8b: Cross-country revenue

f8b_dt <- read_excel("Graph_data.xlsx", 
                     sheet = "Figure_8b")


setDT(f8b_dt)
f8b_dt[,level := factor(level,levels = c("Non-Federal","Federal"))]

total_order <- f8b_dt[, .(total_value = sum(value, na.rm=TRUE)), by = country][order(total_value)]$country
f8b_dt[, country := factor(country,
                           levels = total_order)]


ggplot(f8b_dt, aes(x = country, y = value, fill = level)) +
  geom_col() +
  geom_col(
    data = f8b_dt[country == "Australia"],
    aes(x = country, y = value, group = level),
    fill = NA, color = "gold", linewidth = 2, inherit.aes = TRUE
  ) +
  coord_flip() +
  labs_e61(title = "Revenue: Cross-country (2022)",
           y = "% GDP",
           subtitle = "",
           sources = c("e61","OECD"),
           footnotes = c("Dark blue is spending by Federal Govt % GDP. Light blue is additional spending attributed to non-Federal entities.","FY has been shifted forward by one relative to OECD reporting - due to the Australian financial year starting six months later than other countries.")) +
  plab(c("Non-Federal","Federal"),x=c(1.5,3.5),y=c(40,40))

save_e61("Figure_8b.png",res=2)
save_e61("Report_graph/Figure_8b.svg")

## New Figure 8:

p8a <- ggplot(f8a_dt, aes(x = Country, y = value*100, fill = Government_level)) +
  geom_col() +
  geom_col(
    data = f8a_dt[Country == "Australia"],
    aes(x = Country, y = value*100, group = Government_level),
    fill = NA, colour = "gold", linewidth = 1, inherit.aes = TRUE
  ) +
  coord_flip() +
  labs_e61(title = "A. Expenditure",
           y = "% GDP",
           subtitle = "") +
  plab(c("Non-Federal","Federal"),x=c(1.5,3.5),y=c(40,40))+
  theme(axis.text.y = element_text(size = 6))

p8b <- ggplot(f8b_dt, aes(x = country, y = value, fill = level)) +
  geom_col() +
  geom_col(
    data = f8b_dt[country == "Australia"],
    aes(x = country, y = value, group = level),
    fill = NA, color = "gold", linewidth = 1, inherit.aes = TRUE
  ) +
  coord_flip() +
  labs_e61(title = "B. Revenue",
           y = "% GDP",
           subtitle = ""
           ) +
  plab(c("Non-Federal","Federal"),x=c(1.5,3.5),y=c(40,40))+
  theme(axis.text.y = element_text(size = 6))

save_e61("Figure_8.png",p8a,p8b,res=2,sources = c("e61","OECD"), title = "Cross-country comparison (2022)",
         footnotes = c("Dark blue is spending by Federal Govt % GDP. Light blue is additional spending attributed to non-Federal entities.","FY has been shifted forward by one relative to OECD reporting - due to the Australian financial year starting six months later than other countries."))
save_e61("Report_graph/Figure_8.svg",p8a,p8b,sources = c("e61","OECD"), title = "Consolidated Expenditure",
         footnotes = c("Dark blue is spending by Federal Govt % GDP. Light blue is additional spending attributed to non-Federal entities.","FY has been shifted forward by one relative to OECD reporting - due to the Australian financial year starting six months later than other countries."))

p8a_new <- ggplot(f8a_dt, aes(x = Country, y = value*100, fill = Government_level)) +
  geom_col(width = 0.7) +
  geom_col(width = 0.7,
    data = f8a_dt[Country == "Australia"],
    aes(x = Country, y = value*100, group = Government_level),
    fill = NA, colour = "gold", linewidth = 1, inherit.aes = TRUE
  ) +
  coord_flip() +
  format_flip() +
  labs_e61(title = "A. Expenditure",
           y = "% GDP",
           subtitle = "") +
  plab(c("Non-Federal","Federal"),x=c(1.5,3.5),y=c(41,41))+
  theme(axis.text.y = element_text(size = 6))

p8a_new 

p8b_new <- ggplot(f8b_dt, aes(x = country, y = value, fill = level)) +
  geom_col(width = 0.7) +
  geom_col(width = 0.7,
    data = f8b_dt[country == "Australia"],
    aes(x = country, y = value, group = level),
    fill = NA, color = "gold", linewidth = 1, inherit.aes = TRUE
  ) +
  coord_flip() +
  format_flip() +
  labs_e61(title = "B. Revenue",
           y = "% GDP",
           subtitle = ""
  ) +
  plab(c("Non-Federal","Federal"),x=c(1.5,3.5),y=c(41,41))+
  theme(axis.text.y = element_text(size = 6))

save_e61("Report_graph/new_Figure_8.svg",p8a_new,p8b_new,sources = c("e61","OECD"), title = "International consolidated accounts",
         footnotes = c("Dark blue is spending by Federal Govt % GDP. Light blue is additional spending attributed to non-Federal entities.","FY has been shifted forward by one relative to OECD reporting - due to the Australian financial year starting six months later than other countries."))


## Figure 9: Low vs high

f9_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "Figure_9")


setDT(f9_dt)

ggplot(f9_dt, aes(year, index, colour = series)) +
  geom_line() +
  labs_e61(title = "Low spending countries catchup",
           x = NULL, y = "1999 expenditure %GDP = 1", sources = c("e61","OECD"),
           footnotes = c("Countries classified by average spending as a % of GDP in the 1999s.","Low countries are the bottom quartile: United Kingdom, United States,Estonia, Latvia, Lithuania, Luxembourg, Switzerland.","High countries are the top quartile: Finland, Australia, Belgium, Denmark, France, Israel, Sweden.")) +
  plab(c("Australia","High Spenders","Low Spenders"),y=c(1.22,1.12,1.17),x = c(1999,1999,1999)) +
  scale_y_continuous_e61(limits=c(0.9,1.3,0.1))

save_e61("Figure_9.png",res=2)
save_e61("Report_graph/Figure_9.svg")


# Figure 10: Synthetic control exercise aggregate

f10_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "Figure_10")


setDT(f10_dt)


ggplot(f10_dt, aes(x = year, y = value, colour = series)) +
  geom_vline(xintercept = 2007, linetype = "dashed") +
  geom_line() +
  labs_e61(title = "Australian Fiscal Expenditure remains high",
           x = "Year", y = "Expenditure/GDP Index", linetype = "",
           sources = c("OECD","e61"),
           footnotes = c("An index of nominal government expenditure to nominal GDP, relative to its 1999 level.",
                         "Australia's Fiscal Year ends in June rather than December. For this reason the Australian data is averaged across consecutive years.","Five main donor countries are United States, Israel, Norway, Iceland, and New Zealand. Weights are provided in the Online Appendix.")) +
  geom_hline(yintercept = 1)  +
  plab(c("Observed","Synthetic"),x=c(2000,2000),y=c(1.13,1.07)) +
  scale_y_continuous_e61(limits = c(0.9,1.3,0.1))

save_e61("Figure_10.png",res=2)
save_e61("Report_graph/Figure_10.svg")

ggplot(f10_dt, aes(x = year, y = value, colour = series)) +
  geom_vline(xintercept = 2007, linetype = "dashed") +
  geom_line() +
  labs_e61(title = "Australian fiscal expenditure remains high",
           x = "Year", y = "Expenditure/GDP Index", linetype = "",
           sources = c("OECD","e61"),
           footnotes = c("An index of nominal government expenditure to nominal GDP, relative to its 1999 level.",
                         "Australia's Fiscal Year ends in June rather than December. For this reason the Australian data is averaged across consecutive years.","Five main donor countries are United States, Israel, Norway, Iceland, and New Zealand. Weights are provided in the Online Appendix.")) +
  geom_hline(yintercept = 1)  +
  plab(c("Observed","Synthetic"),x=c(2000,2000),y=c(1.13,1.07)) +
  scale_y_continuous_e61(limits = c(0.9,1.3,0.1))

save_e61("Report_graph/new_Figure_10.svg")

# Figure 11: OECD relative size

f11_dt <- read_excel("Graph_data.xlsx", 
                     sheet = "Figure_11")
setDT(f11_dt)

ggplot(f11_dt,aes(x=COFOG_Area,y=value*100,fill=variable)) + geom_col(position="dodge") + coord_flip() +
  labs_e61(title = "Relative size of functions by Government level",
           subtitle = "2022 Expenditure",
           y="%",
           sources = c("e61","OECD")) +
  plab(c("Consolidated","Federal"),x=c(3.5,4.5),y=c(20,20)) +
  scale_y_continuous_e61(limits = c(0,50,10))

save_e61("Figure_11.png",res=2)
save_e61("Report_graph/Figure_11.svg")




# Figure 12: ABS contributions COFOG

f12_dt <- read_excel("Graph_data.xlsx", 
                     sheet = "Figure_12")
setDT(f12_dt)

ggplot(f12_dt,aes(x=cofog_div_name,y=value,fill=variable)) + geom_col(position = "dodge") + 
  coord_flip() +
  scale_y_continuous_e61(limits = c(0,50,10)) + 
  plab(c("Contribution to growth","Size in 1999*"),y=c(20,20),x=c(1.5,3.5)) +
  labs_e61(title = "Health drives growth since 1999",
           y="%",
           x="",
           sources = c("e61","ABS"),
           footnotes = c("Size reflects the share of total government spending that is on that government function."))

save_e61("Figure_12.png",res=2)
save_e61("Report_graph/Figure_12.svg")


# Figure 13: Expense breakdown

f13_dt <- read_excel("Graph_data.xlsx", 
                     sheet = "Figure_13")
setDT(f13_dt)

f13_dt$etf_broad <- factor(f13_dt$etf_broad,levels = c("Depreciation","Interest expenses","Employees","Non-employee expenses","Transfers"))

ggplot(f13_dt,aes(x=fin_year,y=share,fill=etf_broad)) + 
  geom_col() + 
  labs_e61(title = "Total expenses",
           sources = c("ABS","e61"),
           y="Proportion of total expenditure") +
  scale_y_continuous_e61(limits = c(0,1.2,0.25),expand_top = 0.05)+
  plab(c("Depreciation","Interest","Employee","Non-employee","Transfers"),x=c(1999,1999,2008,2008,2018),y=c(1.1,1.2,1.1,1.2,1.2))

save_e61("Figure_13.png",res=2)
save_e61("Report_graph/Figure_13.svg",auto_scale = FALSE)

ggplot(f13_dt,aes(x=fin_year,y=share*100,fill=etf_broad)) + 
  geom_area() + 
  labs_e61(title = "Total expenses",
           sources = c("ABS","e61"),
           y="Proportion of total expenditure") +
  scale_y_continuous_e61(limits = c(0,120,25),expand_top = 0.05)+
  plab(c("Depreciation","Interest","Employee","Non-employee","Transfers"),x=c(1999,1999,2008,2008,2018),y=c(110,120,110,120,120))

save_e61("Report_graph/new_Figure_13.svg",auto_scale = FALSE)


# Figure 14: Type of spending

f14_dt <- read_excel("Graph_data.xlsx", 
                     sheet = "Figure_14")
setDT(f14_dt)
f14_dt[,Type := factor(Type2,levels = c(3,2,1,4))]

ggplot(f14_dt,aes(x=fin_year,y=prop,fill=as.factor(Type2))) + geom_col() +
  scale_x_continuous(limits = c(1984,2024)) + 
  scale_y_continuous_e61(limits = c(0,1.0001,0.2)) +
  plab(c("Other + Admin","Public Goods","Transfers","Direct Payment"),x=rep(1984,4),y=c(0.8,0.6,0.4,0.2)) +
  labs_e61(title = "Purpose of spending",
           y = "Proportion of total",
           sources = c("ABS","e61"),
           footnotes = c("The classification of functions into purpose is given in Appendix A."))

save_e61("Figure_14.png",res=2)
save_e61("Report_graph/Figure_14.svg",auto_scale = FALSE)

ggplot(f14_dt,aes(x=fin_year,y=prop*100,fill=as.factor(Type2))) + geom_area() +
  scale_y_continuous_e61(limits = c(0,120,25),expand_top = 0.05)+
  plab(c("Other + Admin","Public Goods","Transfers","Direct Payment"),x=c(1999,1999,2012,2012),y=c(110,120,110,120)) +
  labs_e61(title = "Purpose of spending",
           y = "Proportion of total",
           sources = c("ABS","e61"),
           footnotes = c("The classification of functions into purpose is given in Online Appendix."))

save_e61("Report_graph/new_Figure_14.svg",auto_scale = FALSE)

# Figure 15: Cluster

f15_dt <- read_excel("Graph_data.xlsx", 
                     sheet = "Figure_15")
setDT(f15_dt)

f15_dt[,cluster := as.factor(cluster)]

ggplot(f15_dt, aes(PC1, PC2, colour = cluster)) +
  geom_point(size = 2, alpha = 0.9) +
  ggrepel::geom_text_repel(aes(label = name), size = 1.2, show.legend = FALSE) +
  labs_e61(
    title = "Hierarchical clusters",
    x = "PC1", #sprintf("PC1 (%.1f%%)", 100 * pca_res$eig[1, 2]),
    y = "PC2", #sprintf("PC2 (%.1f%%)", 100 * pca_res$eig[2, 2])
    sources = c("ABS","e61"),
    footnotes = c("Figure shows the first two principal components of time-series features for each expenditure category, with points coloured by their hierarchical cluster membership. Labels indicate expenditure categories. This visual is for illustration only: clusters were estimated on the full feature set, not just the two dimensions shown here.")
  ) + scale_y_continuous_e61(limits = c(-3,2,1)) + theme_e61()

# Code for this is not doing the colours correctly. Take directly from main file
save_e61("Figure_15.png",res=2)
save_e61("Report_graph/Figure_15.svg",res=2)

ggplot(f15_dt, aes(PC1, PC2, colour = cluster)) +
  geom_point(size = 2, alpha = 0.9) +
  ggrepel::geom_text_repel(aes(label = name), size = 1.2, show.legend = FALSE) +
  labs_e61(
    title = "Hierarchical clusters",
    x = "", #sprintf("PC1 (%.1f%%)", 100 * pca_res$eig[1, 2]),
    y = "", #sprintf("PC2 (%.1f%%)", 100 * pca_res$eig[2, 2])
    sources = c("ABS","e61"),
    footnotes = c("Figure shows the first two principal components (PCs) of time-series features for each expenditure category, with the first PC on the x-axis and the second on the y-axis.", "Points are coloured by their hierarchical cluster membership. Labels indicate expenditure categories. This visual is for illustration only: clusters were estimated on the full feature set, not just the two dimensions shown here.")
  ) + scale_y_continuous_e61(limits = c(-3,2,1))

save_e61("Report_graph/new_Figure_15.svg",res=2)

# Figure 16: Cluster time series

f16_dt <- read_excel("Graph_data.xlsx", 
                     sheet = "Figure_16")
setDT(f16_dt)

ggplot(f16_dt, aes(year, avg, colour = cluster, group = cluster)) +
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = cluster), alpha = 0.15, colour = NA, show.legend = FALSE) +
  geom_line() +
  labs_e61(
    title = "Spending paths for different clusters",
    y = "Average spending in cluster 1999 = 1",
    x = "Year", 
    sources = c("ABS","e61"),
    footnotes = c("Each function is normalised to 1 in 1999, line represents the unweighted average of expenses within each cluster. The shaded regions represent the range of values within the cluster.")
  ) +
  plab(c("Infrastructure and hospitals","Public order and old age","Cyclical social protection","Youth and medical investments","Housing transfers"),x=rep(1999,5),y=c(8.5,7.5,6.5,5.5,4.5)) +
  scale_y_continuous_e61(limits=c(0,9,3))

# Code for this is not doing the colours correctly. Take directly from main file


# Figure 17: Population growth

f17_dt <- read_excel("Graph_data.xlsx", 
                     sheet = "Figure_17")
setDT(f17_dt)

ggplot(f17_dt,
       aes(x = year, y = growth_pct, colour = series)) +
  geom_hline(yintercept = 0, linewidth = 0.25) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = 2024, linetype = "dashed", linewidth = 0.3) +
  labs_e61(
    title = "Population growth (June year-ended)",
    x = NULL, y = "%",
    sources = c("ABS","e61"),
    footnotes = c("Projection used is the Medium scenario from the ABS Population Projections")
  ) +
  scale_x_continuous_e61(limits = c(1980,2070,15)) +
  plab(c("Actual","Projection"),x=c(1980,2040),y=c(2.2,2.2))

save_e61("Figure_17.png",res=2)
save_e61("Report_graph/Figure_17.svg",auto_scale = FALSE)


# Figure 18: Population pyramid

# Figure 19: Participation contribution

f19_dt <- read_excel("Graph_data.xlsx", 
                     sheet = "Figure_19")
setDT(f19_dt)

ggplot(f19_dt, aes(x = age_group, y = contribution*100, fill = component)) +
  geom_bar(stat = "identity",position="dodge") +
  labs(title = "Drivers of the change in participation rates",
       subtitle = "Change between June 1990 to June 2025",
       y = "Percentage point contribution",
       fill = "Component",
       sources = c("ABS","e61"),
       footnotes = c("Population refers to the change in participation that would be expected due to the change in the population share in that age group at 1990 participation rates. Participation reflects the contribution of the change in participation rates for that age group between 1990 and 2025.")) + 
  scale_y_continuous_e61(limits = c(-5,5,1)) + coord_flip() +
  plab(c("Participation","Population"),x=c(1,2),y=c(-4,-4))

save_e61("Figure_19.png",res=2)
save_e61("Report_graph/Figure_19.svg",auto_scale = FALSE)

ggplot(f19_dt, aes(x = age_group, y = contribution*100, fill = component)) +
  geom_bar(stat = "identity",position="dodge") +
  labs(title = "Drivers of the change in participation rates",
       subtitle = "Percentage point contribution between June 1990 to June 2025",
       fill = "Component",
       sources = c("ABS","e61"),
       footnotes = c("Population refers to the change in participation that would be expected due to the change in the population share in that age group at 1990 participation rates. Participation reflects the contribution of the change in participation rates for that age group between 1990 and 2025.")) + 
  scale_y_continuous_e61(limits = c(-5,5,1)) + coord_flip() + format_flip() + geom_hline(yintercept = 0) +
  plab(c("Participation","Population"),x=c(1,2),y=c(-3.95,-3.95))

save_e61("Report_graph/new_Figure_19.svg",auto_scale = FALSE)

# Figure 20: Participation projection

f20_dt <- read_excel("Graph_data.xlsx", 
                     sheet = "Figure_20")
setDT(f20_dt)

ggplot(f20_dt,aes(x=TIME_PERIOD,y=value*100,colour=variable)) + geom_line() +
  plab(c("At current participation","Doubling in participation among over 65s"),x=c(2025,2025),y=c(61.5,67.5)) +
  labs_e61(title = "Older workers needed to maintain labour force",y="%",x="",
           sources = c("ABS","e61"),
           footnotes = c("In high participation scenario over 65 participation rate rises from 16% to 32% between 2025 and 2071.","ABS population projections used, based on the medium scenarios.")) +
  scale_y_continuous_e61(limits=c(60,70,2))

save_e61("Figure_20.png",res=2)
save_e61("Report_graph/Figure_20.svg",auto_scale = FALSE)


# Figure 21: Productivity growth

f21_dt <- read_excel("Graph_data.xlsx", 
                     sheet = "Figure_21")
setDT(f21_dt)
f21_dt[,decade := factor(decade,levels = c("80s","90s","00s","10s","20s"))]


ggplot(f21_dt,aes(x=decade,y=CAGR*100)) + geom_col() +
  labs_e61(title = "Decade productivity growth",
           y="Average annual growth",
           footnotes = c("The 2020s decade is only a partial decade.","Labour productivity definded as GDP per hour worked."),
           sources = c("ABS","e61"))

save_e61("Figure_21.png",res=2)
save_e61("Report_graph/Figure_21.svg",auto_scale = FALSE)

ggplot(f21_dt,aes(x=decade,y=CAGR*100)) + geom_col() +
  labs_e61(title = "Decade productivity growth",
           subtitle="Average annual growth",
           footnotes = c("The 2020s decade is only a partial decade.","Labour productivity definded as GDP per hour worked."),
           sources = c("ABS","e61")) + geom_hline(yintercept = 0)

save_e61("Report_graph/new_Figure_21.svg",auto_scale = FALSE)

# Figure 22: GDP v RNNDI

f22_dt <- read_excel("Graph_data.xlsx", 
                     sheet = "Figure_22")
setDT(f22_dt)
f22_dt[,date := as.Date(date)]

ggplot(f22_dt,aes(x=date,y=index,colour=series)) + geom_line() +
  labs_e61(title = "High export prices support incomes",
           y = "Index = March 2000",
           sources = c("ABS","e61"),
           footnotes = c("Quarterly, seasonally adjusted, per capita data.")) + scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  plab(c("GDP per capita","RNNDI per capita"),x=c(as.Date("2000-03-01"),as.Date("2000-03-01")),y=c(1.35,1.45)) +
  scale_y_continuous_e61(limits = c(0.9,1.6,0.1))

save_e61("Figure_22.png",res=2)
save_e61("Report_graph/Figure_22.svg",auto_scale = FALSE)


# Figure 23: Stickiness

f23_dt <- read_excel("Graph_data.xlsx", 
                     sheet = "Figure_23")

setDT(f23_dt)

ggplot(f23_dt, aes(x = fin_year)) +
  geom_line(aes(y = Payments_norm, color = "Payments")) +
  geom_line(aes(y = RGDP_norm, color = "NGDP")) +
  geom_line(aes(y = RGDP_trend, color = "Pre-2014 NGDP Trend"), linetype = "dashed") +
  scale_color_manual(values = c("Payments" = palette_e61(3)[1], "NGDP" = palette_e61(3)[2], "Pre-2014 NGDP Trend" = "black")) +
  labs(y = "Indexed to FY2000 = 1", x = "Financial Year", color = "Series") +
  plab(c("Real Govt Payments","GDP","2000-2014 GDP trend"),x=c(2000.5,2000.5,2000.5),y=c(2.1,1.85,1.7),colour = c(palette_e61(3)[1],palette_e61(3)[2],"black")) +
  labs_e61(title = "Spending follows old GDP trends",
           subtitle = "Deflated by GDPD, indexed to 1 in FY99/20",
           y="",
           x="",
           sources = c("ABS","e61")) +
  scale_y_continuous_e61(limits = c(1,2.2,0.5))

save_e61("Figure_23.png",res=2)
save_e61("Report_graph/Figure_23.svg",auto_scale = FALSE)

f23_dt[fin_year %in% c(2000,2011,2024)]

(1.382947/1)^(1/11)-1
(1.385045/1)^(1/11)-1

(1.997286/1.382947)^(1/13)-1
(1.908311/1.385045)^(1/13)-1

ggplot(f23_dt, aes(x = fin_year)) +
  geom_line(aes(y = Payments_norm, color = "Payments")) +
  geom_line(aes(y = RGDP_norm, color = "NGDP")) +
  geom_line(aes(y = RGDP_trend, color = "Pre-2014 NGDP Trend"), linetype = "dashed") +
  scale_color_manual(values = c("Payments" = palette_e61(3)[1], "NGDP" = palette_e61(3)[2], "Pre-2014 NGDP Trend" = "black")) +
  labs(y = "Indexed to FY2000 = 1", x = "Financial Year", color = "Series") +
  plab(c("Real Govt Payments","GDP","2000-2014 GDP trend"),x=c(2000.5,2000.5,2000.5),y=c(2.1,1.85,1.7),colour = c(palette_e61(3)[1],palette_e61(3)[2],"black")) +
  labs_e61(title = "Spending follows old GDP trends",
           subtitle = "Deflated by GDPD, indexed to 1 in FY99/00",
           y="",
           x="",
           sources = c("ABS","e61")) +
  scale_y_continuous_e61(limits = c(1,2.2,0.5))

save_e61("Report_graph/new_Figure_23.svg",auto_scale = FALSE)


# Figure 24: Shapley

four_bin <- read_excel("Graph_data.xlsx", 
                       sheet = "Figure_24")

setDT(four_bin)

features <- c("0_14","15_34","35_54","55_64","65p","tot",
              "rp_g","unemp")
measure         <- "GFCE"



if (exists("four_bin") && nrow(four_bin)) {
  measure_vars <- intersect(features, names(four_bin))
  four_m <- melt(
    four_bin,
    id.vars       = c("y0","y1","d_actual","d_hat","unexplained"),
    measure.vars  = measure_vars,
    variable.name = "driver",
    value.name    = "contrib"
  )
  
  # Collapse into groups
  age_bins <- c("0_14","15_34","35_54","55_64")
  four_m[, group := fcase(
    driver == "65p",              "65+",
    driver %in% age_bins,         "Other ages",
    default =                     "Economic effects"
  )]
  
  # Aggregate explained contributions within groups
  four_g <- four_m[, .(contrib = sum(contrib, na.rm = TRUE)),
                   by = .(y0, y1, d_actual, d_hat, unexplained, group)]
  
  # Add Residual
  residual_dt <- unique(four_m[, .(y0, y1, d_actual, d_hat, unexplained)])
  residual_dt[, `:=`(group = "Residual", contrib = unexplained)]
  four_g <- rbind(four_g, residual_dt[, .(y0, y1, d_actual, d_hat, unexplained, group, contrib)], use.names = TRUE)
  
  # Optional filter
  four_g <- four_g[y0 != 1972]
  
  # Segment labels & ordering
  four_g[, Segment := paste0(y0, "–", y1)]
  seg_levels <- four_bin[y0 != 1972, paste0(y0, "–", y1)]
  four_g[, Segment := factor(Segment, levels = seg_levels)]
  
  # ----- Key: force Residual to extremes -----
  four_g[, stack_order := {
    if (all(contrib[group=="Residual"] <= 0)) {
      factor(group, levels = c("Residual","65+","Other ages","Economic effects"))
    } else {
      factor(group, levels = c("65+","Other ages","Economic effects","Residual"))
    }
  }, by = Segment]
  
  # Build the plot
  p4c <- ggplot(four_g, aes(x = Segment, y = contrib*100, fill = stack_order)) +
    geom_col() +
    # Big solid dot = actual Δ
    geom_point(
      data = unique(four_g[, .(Segment, d_actual)]),
      aes(x = Segment, y = d_actual*100),
      inherit.aes = FALSE,
      shape = 16, size = 5, color = "black"
    ) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    coord_flip() +
    labs_e61(
      title = "Demographic trends dominate lift in spending",
      x = NULL, y = "Contribution (level points)", fill = "Component",
      footnotes = c("Regression based Shapely decomposition, explained in the Online Appendix.",paste0("Black dot reflects the change in ",measure," to GDP."),"Effects represent association between the change in the category and changes in spending to GDP.","Economic Effects reflect variation explained by changes in unemployment, relative government costs, and terms of trade."),
      sources = c("e61","ABS")
    ) +
    plab(c("65+","Other ages","Economic effects***","Residual"),y=rep(3,4),x=c(2.2,1.7,1.2,0.7))
  
  print(p4c)
}

save_e61("Figure_24.png",res=2)
save_e61("Report_graph/Figure_24.svg",auto_scale = FALSE)

# Figure 25: Shapley counterfactual


# Figure 26: Shapley projection

# Figure 27a: Revenue risks

# Figure 27b: Expenditure risks

# Figure 28: Bond market shock projection

### Health
# Figure 28: 


### Social
# Figure 32
f32_dt <- read_excel("Graph_data.xlsx", 
                        sheet = "Figure_32")


ggplot(f32_dt, aes(x = Year, y = Total)) +
  geom_line() +
  labs(
    title = "Consolidated Social Protection Expenditure",
    subtitle = "% of GDP",
    sources = c("ABS", "e61")
  ) + scale_y_continuous_e61(limits = c(0, 15, 5))

save_e61("Figure_32.png", res=2, chart_type = "wide")
save_e61("Report_graph/Figure_32.svg", chart_type = "wide")


### Figure 33


f33_dt <- read_excel("Graph_data.xlsx", 
                   sheet = "Figure_33")

# Define Anglo countries and custom colors
anglos <- c("Australia", "Canada", "New Zealand", "United Kingdom", "United States")
anglo_colors <- c(
  "Australia" = "gold",
  "Canada" = e61_teallight,
  "New Zealand" = e61_orangedark,
  "United Kingdom" = e61_coraldark,
  "United States" = e61_bluedark
)

# Reshape and flag Anglo countries
f33_dt <- f33_dt %>%
  pivot_longer(-Year, names_to = "Country", values_to = "Value") %>%
  mutate(
    Anglo = Country %in% anglos,
    Year = as.numeric(Year)
  )

# Plot
ggplot(f33_dt, aes(x = Year, y = Value, group = Country)) +
  geom_line(data = filter(f33_dt, !Anglo),
            color = "lightgrey") +
  geom_line(data = filter(f33_dt, Anglo),
            aes(color = Country), size = 1.1) +
  scale_color_manual(values = anglo_colors) +
  labs_e61(title = "Social Expenditures by Country",
           x = "", y = "Share of GDP (%)", footnotes = "Social Expenditure includes Expenditure on the Elderly, 
       Families, Unemployment, Disability/Incapacity, and a small `other' category", 
           color = "Anglo Countries", sources = c("OECD Social Expenditure Database", "e61")) + 
  plot_label(c("Aus", "Can", "NZ", "UK", 
               "US"), 
             c(2018.5, 2005, 2010, 2010, 2005), 
             c(3, 5, 5, 20, 12), 
             c("gold", e61_teallight, e61_orangedark, e61_coraldark, e61_bluedark), size = 4) + 
  xlim(2002, 2021)

save_e61("Figure_33.png", res = 2, chart_type = "wide", dim = list(width = 13))
save_e61("Report_graph/Figure_33.svg", chart_type = "wide", dim = list(width = 13))


## Figure 34

f34_dt <- read_excel("Graph_data.xlsx", 
                 sheet = "Figure_34")

setDT(f34_dt)

f34_dt <- melt(f34_dt,id.vars = "Year")


# Plot
ggplot(f34_dt, aes(x = Year, y = value, color = variable)) +
  geom_line() +
  labs(
    title = "Social Protection Expenditure by Type",
    subtitle = "% of GDP",
    sources = c("e61", "ABS"),
    color = NULL
  ) + plot_label(c("Age", "Disability",  "Family", "Other", "Unemployment"), 
                 c(2020, 2020, 2007, 2020, 2012), 
                 c(4.8, 3.2, 3, 1.7, 1.6),
                 colour = c(palette_e61(5)[2],palette_e61(5)[1],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) + scale_y_continuous_e61(limits = c(0, 5, 1))

save_e61("Figure_34.png", res=2, chart_type = "wide", dim = list(width = 14))
save_e61("Report_graph/Figure_34.svg", chart_type = "wide", dim = list(width = 14))

# Figure 35:

f35_dt <- read_excel("Graph_data.xlsx", 
                     sheet = "Figure_35")

setDT(f35_dt)


ggplot(melt(f35_dt,id.vars = "Year"),aes(x=Year,y=value,colour=variable)) + geom_line() +
  geom_hline(yintercept = 1) + 
  scale_y_continuous_e61(limits = c(0.8,1.6)) +
  labs_e61(title = "Aged care spending not just about ageing",
           y= "Relative to age adjusted GDP share",
           sources = c("ABS","e61"),
           footnotes = c("Plot illustrates the ratio of spending compared to a counterfactual where spending by demographic group remained fixed as a % of GDP.")) +
  plab(c("Aged Care","Family Support"),x=c(2011,1999),y=c(0.9,1.3))

save_e61("Figure_35.png", res=2, chart_type = "wide", dim = list(width = 14))
save_e61("Report_graph/Figure_35.svg", chart_type = "wide", dim = list(width = 14))


# Figure 36

f36_dt <- read_excel("Graph_data.xlsx", 
                 sheet = "Figure_36")

setDT(f36_dt)

f36_dt <- melt(f36_dt,id.vars = "Year")

# Plot
ggplot(f36_dt, aes(x = Year, y = value, color = variable)) +
  geom_line() +
  labs_e61(
    title = "Types of Social Protection Support",
    subtitle = "% of GDP",
    color = NULL, 
    footnotes = c("In-Kind includes Social benefits to households in goods and services, and Use of goods and services.","Other reflects the costs of administration."), 
    sources = c("ABS", "e61"),
  ) + plot_label(c( "In-kind", "Cash payments", "Other"), c(2020, 2015, 2020), 
                 c(4.2, 7.5, 2))

save_e61("Figure_36.png", res=2, chart_type = "wide", dim = list(width = 13))
save_e61("Report_graph/Figure_36.svg", chart_type = "wide", dim = list(width = 13))


# Figure 37a

f37a_dt <- read_excel("Graph_data.xlsx", 
                 sheet = "Figure_37_a")

f37a_dt <- f37a_dt %>% mutate(Category = factor(Category, levels = unique(Category)))

# net sum (positives + negatives) per Category
totals <- f37a_dt %>%
  group_by(Category) %>%
  summarise(Net = sum(ChangeSince2000), .groups = "drop")

setDT(f37a_dt)
setDT(totals)


ggplot(f37a_dt[Category != "Other"], aes(x = Category, y = ChangeSince2000, fill = Type)) +
  geom_col() +
  geom_hline(yintercept = 0, colour = "black") +
  # dot for net total — don't inherit the global 'fill = Type'
  geom_point(data = totals[Category != "Other"],
             aes(x = Category, y = Net),
             inherit.aes = FALSE,
             size = 1, colour = "black") +
  labs_e61(
    title = "Change in Social Protection Expenditure, Since 2000",
    y = "Change in ppt of GDP",
    footnotes = c("In-Kind includes Social benefits to households in goods and services, and Use of goods and services.","Other reflects the costs of administration."),
    sources = c("ABS","e61"),
  ) + scale_y_continuous_e61(limits = c(-1.5, 2, 0.5)) +
  plab(c("In-kind","Cash","Other"),x=c(3,3,3),y=c(1.8,1.3,0.8))

save_e61("Figure_37a.png", res = 2, chart_type = "wide")
save_e61("Report_graph/Figure_37a.svg", chart_type = "wide")


### Figure 37b

f37b_dt <- read_excel("Graph_data.xlsx", 
                 sheet = "Figure_37_b")

setDT(f37b_dt)

f37b_dt[,Category := factor(Category,levels = c("NDIS","Aged Care","Other Cash","JSP","Family","Pension","Total"))]

# compute starts/ends for floating bars (exclude Total while accumulating)
parts <- f37b_dt %>%
  filter(Category != "Total") %>%
  mutate(
    start = c(0, head(cumsum(Projected_Change), -1)),
    end   = start + Projected_Change
  )

# build the Total bar explicitly from 0 → sum(parts)
total_val <- sum(parts$Projected_Change)
total_row <- f37b_dt %>% filter(Category == "Total") %>%
  transmute(Category, Change = total_val, Projected_Change,
            start = 0, end = total_val)

wf <- bind_rows(parts, total_row) %>%
  mutate(id = row_number())  # numeric x for geom_rect

setDT(wf)

wf[,colour := fifelse(Category == "Total", "blue",fifelse(Projected_Change < 0,"forestgreen","darkred"))]

# plot
ggplot(wf) +
  geom_rect(aes(xmin = id - 0.45, xmax = id + 0.45,
                ymin = pmin(start, end), ymax = pmax(start, end),
                fill = colour)) +
  scale_fill_identity() +  
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = wf$id, labels = wf$Category, expand = c(0.01, 0.01)) +
  labs_e61(title = "Projected Change In Social Protection Payments to 2063",
       x = NULL, y = "Change in Share of GDP (ppt)",
       sources = c("Treasury IGR 2023"))

# Compute label positions
labs_dt <- wf[, .(id, Category)]
labs_dt[, y := -0.05]                     # base offset below axis
labs_dt[seq(2, .N, 2), y := -0.5]       # lower every second label

ggplot(wf) +
  geom_rect(aes(xmin = id - 0.45, xmax = id + 0.45,
                ymin = pmin(start, end), ymax = pmax(start, end),
                fill = colour)) +
  scale_fill_identity() +
  geom_hline(yintercept = 0) +
  # hide built-in x labels
  scale_x_continuous(breaks = NULL, expand = c(0.01, 0.01)) +
  geom_text(data = labs_dt, aes(x = id, y = y, label = Category), vjust = 1, size = 3.5) +
  labs_e61(
    title = "Projected Change In Social Protection Payments to 2063",
    x = "",
    y = "Change in Share of GDP (ppt)",
    footnotes = c("Other cash reflects working age payments not included in other categories."),
    sources = c("Treasury IGR 2023")
  ) +
  theme_e61(legend = "none")


save_e61("Figure_37b.png",res=2)
save_e61("Report_graph/Figure_37b.svg")

## 37 full

plot_37a <- ggplot(f37a_dt[Category != "Other"], aes(x = Category, y = ChangeSince2000, fill = Type)) +
  geom_col() +
  geom_hline(yintercept = 0, colour = "black") +
  # dot for net total — don't inherit the global 'fill = Type'
  geom_point(data = totals[Category != "Other"],
             aes(x = Category, y = Net),
             inherit.aes = FALSE,
             size = 1, colour = "black") +
  labs_e61(
    title = "A. Historical, Since 2000",
    y = "Change in ppt of GDP",
    footnotes = c("In-Kind includes Social benefits to households in goods and services, and Use of goods and services.","Other reflects the costs of administration."),
    sources = c("ABS","e61"),
  ) + scale_y_continuous_e61(limits = c(-1.5, 2, 0.5)) +
  plab(c("In-kind","Cash","Other"),x=c(3,3,3),y=c(1.8,1.3,0.8))

plot_37b <- ggplot(wf) +
  geom_rect(aes(xmin = id - 0.45, xmax = id + 0.45,
                ymin = pmin(start, end), ymax = pmax(start, end),
                fill = colour)) +
  scale_fill_identity() +
  geom_hline(yintercept = 0) +
  # hide built-in x labels
  scale_x_continuous(breaks = NULL, expand = c(0.01, 0.01)) +
  geom_text(data = labs_dt, aes(x = id, y = y, label = Category), vjust = 1, size = 3.5) +
  labs_e61(
    title = "B. Projected to 2063",
    x = "",
    y = "Change in Share of GDP (ppt)",
    footnotes = c("Other cash reflects working age payments not included in other categories."),
    sources = c("Treasury IGR 2023")
  ) +
  scale_y_continuous_e61(limits = c(-1,3,1))


save_e61("Figure_37.png",plot_37a,plot_37b,res=2,title = "Change in Social Protection Payments")
save_e61("Report_graph/Figure_37.svg",plot_37a,plot_37b,title = "Change in Social Protection Payments")

new_plot_37b <- ggplot(wf) +
  geom_rect(aes(xmin = id - 0.45, xmax = id + 0.45,
                ymin = pmin(start, end), ymax = pmax(start, end),
                fill = colour)) +
  scale_fill_identity() +
  geom_hline(yintercept = 0) +
  # hide built-in x labels
  scale_x_continuous(breaks = NULL, expand = c(0.01, 0.01)) +
  geom_text(data = labs_dt, aes(x = id, y = y, label = Category), vjust = 1, size = 3.00) +
  labs_e61(
    title = "B. Projected to 2063",
    x = "",
    y = "Change in Share of GDP (ppt)",
    footnotes = c("Other cash reflects working age payments not included in other categories."),
    sources = c("Treasury IGR 2023")
  ) +
  scale_y_continuous_e61(limits = c(-1,3,1))

save_e61("Report_graph/new_Figure_37.svg",plot_37a,new_plot_37b,title = "Change in Social Protection Payments")

### Economic affairs

# Figure 38: Economic affairs outline


## Short Figure six (combines 36 and 37a into a two-panel)

plot_36 <- ggplot(f36_dt, aes(x = Year, y = value, color = variable)) +
  geom_line() +
  labs_e61(
    title = "Types of support",
    subtitle = "% of GDP",
    color = NULL, 
    ) + plot_label(c( "In-kind", "Cash payments", "Other"), c(2018, 2013, 2018), 
                 c(4.2, 7.5, 2))


plot_37a2 <- ggplot(f37a_dt[Category != "Other"], aes(x = Category, y = ChangeSince2000, fill = Type)) +
  geom_col() +
  geom_hline(yintercept = 0, colour = "black") +
  # dot for net total — don't inherit the global 'fill = Type'
  geom_point(data = totals[Category != "Other"],
             aes(x = Category, y = Net),
             inherit.aes = FALSE,
             size = 1, colour = "black") +
  labs_e61(
    title = "Change since 2000",
    y = "Change in ppt of GDP"
  ) + scale_y_continuous_e61(limits = c(-1.5, 2, 0.5)) +
  plab(c("In-kind","Cash","Other"),x=c(3,3,3),y=c(1.8,1.3,0.8))


save_e61("Short_Figure_6.png",plot_36,plot_37a2,res=2,title ="Social Protection Expenditure", 
         sources = c("ABS", "e61"),
         footnotes = c("In-Kind includes Social benefits to households in goods and services, and Use of goods and services.","Other reflects the costs of administration."))

save_e61("Report_graph/Short_Figure_6.svg",plot_36,plot_37a2,res=2,title ="Social Protection Expenditure", 
         sources = c("ABS", "e61"),
         footnotes = c("In-Kind includes Social benefits to households in goods and services, and Use of goods and services.","Other reflects the costs of administration."))

### Add plots for slides

# Slide 2, spending and revenue - add our projection line

s2_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "2 Spend rev (Feb version)")

setDT(s2_dt)

s2_dt <- melt(s2_dt,id.vars = "Year")

ggplot(s2_dt,aes(x=Year,y=value,colour=variable,linetype = variable)) + geom_line() +
  #scale_y_continuous_e61(limits = c(32,48,4)) +
  labs_e61(title = "Persistent spending pressure",
           y = "% GDP",
           sources = c("PBO","e61"),
           footnotes = c("Consolidated Expenses and Revenue for the 2026 National Fiscal Outlook by the PBO.","Demographic projection based on e61 demographic modelling of spending trends.")) + 
  plab(c("Expenses","Revenue","Demographic projection"),x=c(2003,2003,2003),y=c(43,38,27)) +
  scale_x_continuous_e61(limits = c(2003,2029,5)) +
  geom_vline(xintercept = 2025,linetype = "dashed") +
  scale_linetype_manual(values = c("solid","solid","longdash"))

save_e61("Slide 2.svg")

# Slide 3, interest

s3_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "3 Interest_plot (Feb version)")

setDT(s3_dt)

s3_dt <- melt(s3_dt,id.vars = "Year")

ggplot(s3_dt,aes(x=Year,y=value,colour=variable)) + geom_line() +
  #scale_y_continuous_e61(limits = c(32,48,4)) +
  labs_e61(title = "Interest expenditure",
           y = "% GDP",
           sources = c("PBO","e61"),
           footnotes = c("Public debt interest from the 2026 National Fiscal Outlook by the PBO")) + 
  plab(c("State","Federal","National"),x=c(2003,2003,2003),y=c(1.2,1.7,2.2)) +
  scale_x_continuous_e61(limits = c(2003,2029,5)) +
  geom_vline(xintercept = 2025,linetype = "dashed")

save_e61("Slide 3.svg")


##### Additional plots
## Tax
# 
# tax_dt <- read_excel("Graph_data.xlsx", 
#                       sheet = "Tax_plot")
# setDT(tax_dt)
# 
# ggplot(melt(tax_dt,id.vars = "year"),aes(x=year,y=value*100,colour=variable)) + geom_line() + geom_vline(xintercept = 2025,linetype = "dashed") +
#   scale_y_continuous_e61(limits = c(0,25,5)) +
#   labs_e61(title = "Income tax burdens to rise",
#            y = "% GDP",
#            sources = c("e61","PBO"),
#            footnotes = c("Until 2000, cash accounting concepts are used. Post 2000 income tax is accrual based.","Projections sourced from PBO Medium Term Budget Outlook, with an allowance for non-individual or corporate income tax.","Total income tax includes individual, corporate, fringe benefit, resource rent, and superannuation taxes."))
# 
# save_e61("Income_tax.png",res=2)
# save_e61("Income_tax.svg")






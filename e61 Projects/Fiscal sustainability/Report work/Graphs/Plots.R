# Topic: Takes background data and produces plots
# Author: Matt Nolan
# Created: 19/10/2025
# Last edit: 26/10/2025
# Last editor: Matt Nolan


rm(list=ls())

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
           y = "% NGDP",
           sources = c("ABS","e61"),
           footnotes = c("Expenditure includes current expenses and net acquisition of non-financial assets")) + 
  plab(c("Current expenses","Total expenditure"),x=c(2010,2008),y=c(33,41))

save_e61("Figure_1.png",res=2)
save_e61("Report_graph/Figure_1.svg")


## Figure 2: Fiscal balance - also Figure 1 in short report

f2_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "Figure_2")

setDT(f2_dt)

f2_dt[,variable := factor(variable,levels = c("Commonwealth","States","National"))]

ggplot(f2_dt,aes(x=year,y=value,colour=variable)) + geom_line() + geom_line() +
  labs_e61(title = "Persistent Fiscal Imbalance",
           y = "Fiscal balance, % NGDP",
           sources =c("PBO","Budget 2026","e61"),
           footnotes = c("Uses the 2025 National Fiscal Outlook by the PBO, updated with Budget 2026 estimates.","Plot to the right of the dashed line reflect Budget/PBO estimates.")) + # , with estimates updated for the 2026 Final Budget Outcome - checked and this isn't in FBO, which is more cut down.
  geom_hline(yintercept = 0) +
  plab(c("States","Federal","National"),y=c(-3,-5,-7),x=c(2003,2003,2003)) + 
  geom_vline(xintercept = 2024,linetype = "dashed") +
  scale_y_continuous_e61(limits = c(-12,4,4)) +
  scale_x_continuous_e61(limits = c(2003,2028,5))

save_e61("Figure_2.png",res=2)
save_e61("Report_graph/Figure_2.svg")

## Figure 3: Net debt

f3_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "Figure_3")

setDT(f3_dt)

ggplot(melt(f3_dt,id.vars = "Year"),aes(x=Year,y=value,colour =variable)) + geom_line() +
  scale_y_continuous_e61(limits = c(-10,40,10)) + geom_hline(yintercept = 0) +
  labs_e61(title = "Net debt projected to keep rising",
           y = "% NGDP",
           sources = c("PBO","e61"),
           footnotes = c("Actuals and projections come from the 2024/25 National Fiscal Outlook")) +
  geom_vline(xintercept = 2023,linetype = "dashed") +
  plab(c("State","Federal","National"),x=c(2013,2005,2005),y=c(-5,25,35))

save_e61("Figure_3.png",res=2)
save_e61("Report_graph/Figure_3.svg")


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


## Figure 5a: Expenditure by government level

f5a_dt <- read_excel("Graph_data.xlsx", 
                    sheet = "Figure_5a")

setDT(f5a_dt)
f5a_dt[,Level := factor(Level,levels = c("Non-Federal","Federal"))]

ggplot(f5a_dt,aes(x=Year,y=value,colour=Level)) + geom_line() +
  labs_e61(
    title = "Expenditure by Government level",
    y = "% NGDP",
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

ggplot(f7_dt,aes(x=year,y=Revenue,colour=level)) + geom_line() + 
  labs_e61(title = "Revenue by Government level",
           y= "% NGDP",
           sources = c("OECD","e61")) +
  scale_y_continuous_e61(limits=c(10,26,4)) +
  plab(c("Federal","Non-Federal"),x=c(2009,2009),y=c(21,13))

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
           y = "% NGDP",
           subtitle = "",
           sources = c("e61","OECD"),
           footnotes = c("Dark blue is spending by Federal Govt % GDP. Light blue is additional spending attributed to non-Federal entities.","FY has been shifted forward by one relative to OECD reporting - due to the Australian financial year starting six months later than other countries.")) +
  plab(c("Non-Federal","Federal"),x=c(1.5,3.5),y=c(40,40))


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
           y = "% NGDP",
           subtitle = "",
           sources = c("e61","OECD"),
           footnotes = c("Dark blue is spending by Federal Govt % GDP. Light blue is additional spending attributed to non-Federal entities.","FY has been shifted forward by one relative to OECD reporting - due to the Australian financial year starting six months later than other countries.")) +
  plab(c("Non-Federal","Federal"),x=c(1.5,3.5),y=c(40,40))

save_e61("Figure_8b.png",res=2)
save_e61("Report_graph/Figure_8b.svg")


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
                         "Australia's Fiscal Year ends in June rather than December. For this reason the Australian data is averaged across consecutive years.","Five main donor countries are United States, Israel, Norway, Iceland, and New Zealand. Weights are provided in Appendix A.")) +
  geom_hline(yintercept = 1)  +
  plab(c("Observed","Synthetic"),x=c(2000,2000),y=c(1.13,1.07)) +
  scale_y_continuous_e61(limits = c(0.9,1.3,0.1))

save_e61("Figure_10.png",res=2)
save_e61("Report_graph/Figure_10.svg")

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
  plab(c("Contribution to growth","Size in 2024*"),y=c(20,20),x=c(1.5,3.5)) +
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

# Figure 15: Cluster

f15_dt <- read_excel("Graph_data.xlsx", 
                     sheet = "Figure_15")
setDT(f15_dt)

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
#save_e61("Figure_15.png",res=2)

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



# Figure 24: Shapley


# Figure 25: Shapley counterfactual


# Figure 26: Shapley projection



# Figure 35:

ggplot(melt(Dem_adj_SP,id.vars = "Year"),aes(x=Year,y=value,colour=variable)) + geom_line() +
  geom_hline(yintercept = 1) + 
  scale_y_continuous_e61(limits = c(0.8,1.6)) +
  labs_e61(title = "Aged care spending not just about ageing",
           y= "Relative to age adjusted GDP share",
           sources = c("ABS","e61"),
           footnotes = c("Plot illustrates the ratio of spending compared to a counterfactual where spending by demographic group remained fixed as a % of GDP.")) +
  plab(c("Aged Care","Family Support"),x=c(2011,1999),y=c(0.9,1.3))




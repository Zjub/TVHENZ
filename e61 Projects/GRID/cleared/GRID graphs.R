## Replicating the graphs for the presentation

library(tidyverse)
library(data.table)
library(theme61)
library(readabs)


GDP <- read_abs(cat_no = "5206.0") #%>% filter(series_type == "Seasonally Adjusted") %>% filter(series == "Gross domestic product: Chain volume measures ;") %>% filter(table_title == "Table 1. Key National Accounts Aggregates") %>% select("date","series","value")

setDT(GDP)

GDP_total <- GDP[date >= as.Date("2001-01-01") & unit == "$ Millions" & series == "Gross domestic product: Chain volume measures ;"]

GDP_PC <- GDP[date >= as.Date("2001-01-01") & unit == "$" & series == "GDP per capita: Chain volume measures ;"] # & unit == "$ Millions",  & unit == "Index Numbers"

# unique(GDP$unit)
# unique(GDP$series)

unique(GDP_PC$series)

### Plot GDP PC

plot_PC <- GDP_PC[frequency == "Quarter" & series_type == "Seasonally Adjusted"][,.(date,value)]
plot_PC[, growth := value / shift(value) - 1]

# Identify negative growth quarters
plot_PC[, neg_growth := growth < 0]

# Identify periods of two consecutive negative quarters
plot_PC[, run := rleid(neg_growth)]
plot_PC[, two_neg_quarters := .N >= 2 & all(neg_growth), by = run]

# Identify quarters that are below all previous values
plot_PC[, cumulative_max := cummax(value)]
plot_PC[, below_previous := value < shift(cumulative_max, fill = Inf)]

# Combine both conditions for shading
plot_PC <- plot_PC[date >= as.Date("2002-03-01"), shade_period := two_neg_quarters | below_previous]

plot_PC[, period_id := rleid(shade_period)]
shading_periods <- plot_PC[shade_period == TRUE, .(start = min(date), end = max(date)), by = period_id]

ggplot(plot_PC[date >= as.Date("2002-03-01")], aes(x = date, y = value / 1000)) +
  geom_line() +
  geom_rect(data = shading_periods, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  labs_e61(title = "Real GDP per capita ($AUD)",y = "($000s)", x = "Date",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","Seasonally adjusted, chain-volume GDP"),source = c("ABS","e61", "GRID")) + scale_y_continuous_e61(labels = scales::dollar_format(),limits = c(16,24,by=1))

# The above is quarterly GDP. For the plot we want to generate annual figures.

plot_annual_PC <- GDP_PC[frequency == "Quarter" & series_type == "Original"][,.(date,value)]
plot_annual_PC[, annual_GDP := frollsum(value, n = 4)]

ggplot(plot_annual_PC[date >= as.Date("2002-03-01")], aes(x = date, y = annual_GDP / 1000)) +
  geom_line() +
  geom_rect(data = shading_periods, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  labs_e61(title = "Real GDP per capita ($AUD)",y = "($000s)", x = "Date",footnotes = c("Recession defined as period with two quarters of negative quarterly GDP per capita growth with GDP per capita below its prior peak","Rolling annual, chain-volume GDP"),source = c("ABS","e61", "GRID")) + scale_y_continuous_e61(labels = scales::dollar_format(),limits = c(60,100,by=10))


save_e61("GDP_PC.png",res=2,pad_width = 1,auto_scale = FALSE)

### Plot for total GDP
# 
# plot_total <- GDP_total[frequency == "Quarter" & series_type == "Seasonally Adjusted"][,.(date,value)]
# plot_total[, growth := value / shift(value) - 1]
# 
# # Identify negative growth quarters
# plot_total[, neg_growth := growth < 0]
# 
# # Identify periods of two consecutive negative quarters
# plot_total[, run := rleid(neg_growth)]
# plot_total[, two_neg_quarters := .N >= 2 & all(neg_growth), by = run]
# 
# # Identify quarters where the value is below all previous values
# plot_total[, cumulative_max := cummax(value)]
# plot_total[, below_previous := value < shift(cumulative_max, fill = Inf)]
# 
# # Combine both conditions for shading, starting from a specific date
# plot_total[date >= as.Date("2002-03-01"), shade_period := two_neg_quarters | below_previous]
# 
# # Identify contiguous shading periods
# plot_total[, period_id := rleid(shade_period)]
# shading_periods <- plot_total[shade_period == TRUE, .(start = min(date), end = max(date)), by = period_id]
# 
# # Plot with shaded regions
# ggplot(plot_total, aes(x = date, y = value / 1000)) +
#   geom_line() +
#   geom_rect(data = shading_periods, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
#             fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
#   labs(y = "Value (in thousands)", x = "Date")


### Make the core PC graph with the recessions based on total values
# 
# ggplot(plot_PC[date >= as.Date("2002-03-01")], aes(x = date, y = value / 1000)) +
#   geom_line() +
#   geom_rect(data = shading_periods, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
#             fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
#   labs(y = "Value (in thousands)", x = "Date")

### Employment data

LS <- read_abs(cat_no = "6202.0") %>% filter(series_type == "Seasonally Adjusted") %>% filter(table_title == "Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original") %>% filter(series == "Employment to population ratio ;  Persons ;" | series == "Unemployment rate ;  Persons ;" | series == "Employment to population ratio ;  > Males ;" | series == "Unemployment rate ;  > Males ;" | series == "Employment to population ratio ;  > Females ;" | series == "Unemployment rate ;  > Females ;") %>% select("date","series","value")

LS2 <- pivot_wider(LS,id_cols = "date",names_from = "series",values_from = "value")
setDT(LS2)
LS2a <- LS2[,1:4]
colnames(LS2a) <- c("date","Total","Men","Women")
LS2a <- melt(LS2a,id.vars = "date")

LS2b <- LS2[,c(1,5:7)]
colnames(LS2b) <- c("date","Total","Men","Women")
LS2b <- melt(LS2b,id.vars = "date")


ggplot(LS2a[date >= as.Date("2002-01-01")],aes(x=date,y=value,colour=variable)) + geom_line() +
  scale_y_continuous_e61(limits = c(50,75,by=5)) +
  plab(label=c("Total","Men","Women"),x=c(as.Date("2011-01-01"),as.Date("2011-01-01"),as.Date("2011-01-01")),y=c(64,72,57)) +
  geom_rect(data = shading_periods, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  labs_e61(title = "Employment Rates",y = "%", x = "Date",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","Seasonally adjusted, for those aged 15+"),source = c("ABS","e61"))

save_e61("Emp_rate.png",res=2,pad_width = 1,auto_scale = FALSE)


ggplot(LS2b[date >= as.Date("2002-01-01")],aes(x=date,y=value,colour=variable)) + geom_line() +
  scale_y_continuous_e61(limits = c(0,10,by=1)) +
  plab(label=c("Total","Men","Women"),x=c(as.Date("2011-01-01"),as.Date("2011-01-01"),as.Date("2011-01-01")),y=c(3.8,6.8,7.8)) +
  geom_rect(data = shading_periods, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  labs_e61(title = "Unemployment Rates",y = "%", x = "Date",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","Seasonally adjusted, for those aged 15+"),source = c("ABS","e61")) #+ theme_e61(legend = "bottom")

save_e61("Unemp_rate.png",res=2,pad_width = 1,auto_scale = FALSE)


### Real wage graph
# Add both WPI and Average Weekly Earnings

AWE <- read_abs(cat_no = "6302.0")
setDT(AWE)

ggplot(AWE[series %in% c("Earnings; Persons; Full Time; Adult; Ordinary time earnings ;",
                   "Earnings; Persons; Full Time; Adult; Total earnings ;",
                   "Earnings; Persons; Total earnings ;") & series_type == "Seasonally Adjusted"],aes(x=date,y=value*52.2/1000,colour=series))+geom_line() +
  plab(label=c("Ordinary Full Time","Total Full Time","Actual Earnings"),x=c(as.Date("2003-01-01"),as.Date("2003-01-01"),as.Date("2003-01-01")),y=c(85,92,48)) +
  geom_rect(data = shading_periods, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  labs_e61(title = "Annual Earnings",y = "($000s)", x = "Date",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","Seasonally adjusted, annualised weekly earnings"),source = c("ABS","e61")) #+ theme_e61(legend = "bottom")

# Adjust by CPI

# Just use OECD data instead, as can't find the long time series

OECD_earnings <- read_csv("OECD_earnings.csv")
setDT(OECD_earnings)

OECD_earnings[,date := as.Date(paste0(TIME_PERIOD,"-07-01"))]

ggplot(OECD_earnings[TIME_PERIOD >= 2002],aes(x=date,y=OBS_VALUE/1000)) + geom_line() +
  geom_rect(data = shading_periods, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  scale_y_continuous_e61(limits = c(50,75,5)) +
  labs_e61(title = "Average FTE Annual Earnings",y = "($000s)", x = "Date",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","Annual FTE wages, $USD, 2023 PPP"),source = c("OECD","e61")) #+ theme_e61(legend = "bottom")

save_e61("Earnings.png",res=2,pad_width = 1,auto_scale = FALSE)

## Terms of trade

IIP <- read_abs(cat_no = "5302.0")
setDT(IIP)

TOT <- IIP[series == "Terms of Trade ;  Goods and Services ;" & series_type == "Seasonally Adjusted"]

ggplot(TOT[date >= as.Date("2002-01-01")],aes(x=date,y=value)) + geom_line()+
  geom_rect(data = shading_periods, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  scale_y_continuous_e61(limits = c(40,120,20)) +
  labs_e61(title = "Terms of trade",y = "", x = "Date",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","Terms of Trade index"),source = c("ABS","e61")) #+ theme_e61(legend = "bottom")

save_e61("TOT.png",res=2,pad_width = 1,auto_scale = FALSE)


################# Figure 2

shading_periods_plots <- shading_periods[1:15]

fig_2_percentiles_log_inc <- read_csv("fig_2_percentiles_log_inc.csv")

setDT(fig_2_percentiles_log_inc)

fig_2_percentiles_log_inc[,date := as.Date(paste0(year,"-07-01"))]

# Male low
male_low <- fig_2_percentiles_log_inc[gender == "Male",.(year,date,p10_log_inc,p25_log_inc,p50_log_inc,p75_log_inc,p90_log_inc)]

first_year <- min(male_low$year)

# male_low[, `:=`(
#   p10_log_inc = exp(p10_log_inc) / exp(p10_log_inc[year == first_year]),
#   p25_log_inc = exp(p25_log_inc) / exp(p25_log_inc[year == first_year]),
#   p50_log_inc = exp(p50_log_inc) / exp(p50_log_inc[year == first_year]),
#   p75_log_inc = exp(p75_log_inc) / exp(p75_log_inc[year == first_year]),
#   p90_log_inc = exp(p90_log_inc) / exp(p90_log_inc[year == first_year])
# )]

male_low[, `:=`(
  p10_log_inc = p10_log_inc - p10_log_inc[year == first_year],
  p25_log_inc = p25_log_inc - p25_log_inc[year == first_year],
  p50_log_inc = p50_log_inc - p50_log_inc[year == first_year],
  p75_log_inc = p75_log_inc - p75_log_inc[year == first_year],
  p90_log_inc = p90_log_inc - p90_log_inc[year == first_year]
)]

male_low[,year := NULL]

graph_male_low <- melt(male_low,id.vars = "date")

ggplot(graph_male_low,aes(x=date,y=value,colour=variable)) + geom_line() + 
  plab(label = c("p10","p25","p50","p75","p90"),y=c(0.22,0.27,0.32,0.37,0.42),x=c(as.Date("2002-01-01"),as.Date("2002-01-01"),as.Date("2002-01-01"),as.Date("2002-01-01"),as.Date("2002-01-01"))) +
  scale_y_continuous_e61(limits = c(-0.2,0.6,0.1)) + 
  geom_rect(data = shading_periods_plots, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  labs_e61(title = "Men log earnings relative to 2002",y = "", x = "Date",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings, difference from initial log value by percentile"),source = c("ABS","e61", "GRID")) +
  add_baseline()

save_e61("Fig2_Male_low.png",res=2,pad_width = 1,auto_scale = FALSE)

# female low
female_low <- fig_2_percentiles_log_inc[gender == "Female",.(year,date,p10_log_inc,p25_log_inc,p50_log_inc,p75_log_inc,p90_log_inc)]

female_low[, `:=`(
  p10_log_inc = p10_log_inc - p10_log_inc[year == first_year],
  p25_log_inc = p25_log_inc - p25_log_inc[year == first_year],
  p50_log_inc = p50_log_inc - p50_log_inc[year == first_year],
  p75_log_inc = p75_log_inc - p75_log_inc[year == first_year],
  p90_log_inc = p90_log_inc - p90_log_inc[year == first_year]
)]

female_low[,year := NULL]

graph_female_low <- melt(female_low,id.vars = "date")

ggplot(graph_female_low,aes(x=date,y=value,colour=variable)) + geom_line() + 
  plab(label = c("p10","p25","p50","p75","p90"),y=c(0.22,0.27,0.32,0.37,0.42),x=c(as.Date("2002-01-01"),as.Date("2002-01-01"),as.Date("2002-01-01"),as.Date("2002-01-01"),as.Date("2002-01-01"))) +
  scale_y_continuous_e61(limits = c(-0.2,0.6,0.1)) + 
  geom_rect(data = shading_periods_plots, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  labs_e61(title = "Women log earnings relative to 2002",y = "", x = "Date",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings, difference from initial log value by percentile"),source = c("ABS","e61", "GRID"))+
  add_baseline()

save_e61("Fig2_Female_low.png",res=2,pad_width = 1,auto_scale = FALSE)

# Male high
male_high <- fig_2_percentiles_log_inc[gender == "Male",.(year,date,p90_log_inc,p95_log_inc,p99_log_inc,p99_9_log_inc,p99_99_log_inc)]

male_high[, `:=`(
  p90_log_inc = p90_log_inc - p90_log_inc[year == first_year],
  p95_log_inc = p95_log_inc - p95_log_inc[year == first_year],
  p99_log_inc = p99_log_inc - p99_log_inc[year == first_year],
  p99_9_log_inc = p99_9_log_inc - p99_9_log_inc[year == first_year],
  p99_99_log_inc = p99_99_log_inc - p99_99_log_inc[year == first_year]
)]

male_high[,year := NULL]

male_high[,p99_99_log_inc := NULL]

graph_male_high <- melt(male_high,id.vars = "date")

ggplot(graph_male_high,aes(x=date,y=value,colour=variable)) + geom_line() + 
  plab(label = c("p90","p95","p99","p99.9"),y=c(0.22,0.27,0.32,0.37),x=c(as.Date("2002-01-01"),as.Date("2002-01-01"),as.Date("2002-01-01"),as.Date("2002-01-01")),colour = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4])) +
  scale_y_continuous_e61(limits = c(-0.2,0.6,0.1)) + 
  geom_rect(data = shading_periods_plots, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  labs_e61(title = "Men log earnings relative to 2002",y = "", x = "Date",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings, difference from initial log value by percentile","99.99 percentile dropped due to topcoding"),source = c("ABS","e61", "GRID")) + 
  add_baseline() + scale_colour_manual(values = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4]))

save_e61("Fig2_Male_high.png",res=2,pad_width = 1,auto_scale = FALSE)

# Female high
female_high <- fig_2_percentiles_log_inc[gender == "Female",.(year,date,p90_log_inc,p95_log_inc,p99_log_inc,p99_9_log_inc,p99_99_log_inc)]

female_high[, `:=`(
  p90_log_inc = p90_log_inc - p90_log_inc[year == first_year],
  p95_log_inc = p95_log_inc - p95_log_inc[year == first_year],
  p99_log_inc = p99_log_inc - p99_log_inc[year == first_year],
  p99_9_log_inc = p99_9_log_inc - p99_9_log_inc[year == first_year],
  p99_99_log_inc = p99_99_log_inc - p99_99_log_inc[year == first_year]
)]

female_high[,year := NULL]

graph_female_high <- melt(female_high,id.vars = "date")

ggplot(graph_female_high,aes(x=date,y=value,colour=variable)) + geom_line() + 
  plab(label = c("p90","p95","p99","p99.9","p99.99"),y=c(0.22,0.27,0.32,0.37,0.42),x=c(as.Date("2002-01-01"),as.Date("2002-01-01"),as.Date("2002-01-01"),as.Date("2002-01-01"),as.Date("2002-01-01"))) +
  scale_y_continuous_e61(limits = c(-0.2,0.6,0.1)) + 
  geom_rect(data = shading_periods_plots, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  labs_e61(title = "Women log earnings relative to 2002",y = "", x = "Date",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings, difference from initial log value by percentile"),source = c("ABS","e61", "GRID")) +
  add_baseline()


save_e61("Fig2_Female_high.png",res=2,pad_width = 1,auto_scale = FALSE)

##### Figure 3

fig_3_inequality_by_cohort <- read_csv("fig_3_inequality_by_cohort.csv")
setDT(fig_3_inequality_by_cohort)


fig_3_inequality_by_cohort[,cohort := year - age]

cohorts_f3 <- fig_3_inequality_by_cohort[cohort %in% c(1978,1983,1988)]
cohorts_f3[,date := as.Date(paste0(year,"-07-01"))]
ages_f3 <- fig_3_inequality_by_cohort[age %in% c(25,35)]
ages_f3[,date := as.Date(paste0(year,"-07-01"))]

ggplot(cohorts_f3[gender == "Male"],aes(x=date,y=p9010_log_inc,colour=as.factor(cohort))) + geom_line(size = 1.5) +
  geom_line(data = ages_f3[gender == "Male"], aes(x = date, y = p9010_log_inc, colour = as.factor(age)), 
            linetype = "dashed",linewidth=0.5) +
  geom_rect(data = shading_periods_plots, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  labs_e61(title = "Men's earnings 90/10 by cohort",y = "", x = "Date",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings, difference from initial log value by percentile"),source = c("ABS","e61", "GRID")) +
  scale_y_continuous_e61(limits = c(1.4,2.2,by=0.2)) +
  scale_color_manual(values = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) +
  plab(c("1978 birth cohort","1983 birth cohort","1988 birth cohort","25 year olds","35 year olds"),x=c(as.Date("2011-01-01"),as.Date("2011-01-01"),as.Date("2011-01-01"),as.Date("2011-01-01"),as.Date("2011-01-01")),y=c(1.57,1.52,1.47,2.05,1.65))

save_e61("Fig3_Male_cohort.png",res=2,pad_width = 1,auto_scale = FALSE)


ggplot(cohorts_f3[gender == "Female"],aes(x=date,y=p9010_log_inc,colour=as.factor(cohort))) + geom_line(size = 1.5) +
  geom_line(data = ages_f3[gender == "Female"], aes(x = date, y = p9010_log_inc, colour = as.factor(age)), 
            linetype = "dashed",linewidth=0.5) +
  geom_rect(data = shading_periods_plots, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
            fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  labs_e61(title = "Women's earnings 90/10 by cohort",y = "", x = "Date",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings, difference from initial log value by percentile"),source = c("ABS","e61", "GRID")) +
  scale_y_continuous_e61(limits = c(1.4,2.2,by=0.2)) +
  scale_color_manual(values = c(palette_e61(5)[1],palette_e61(5)[2],palette_e61(5)[3],palette_e61(5)[4],palette_e61(5)[5])) +
  plab(c("1978 birth cohort","1983 birth cohort","1988 birth cohort","25 year olds","35 year olds"),x=c(as.Date("2011-01-01"),as.Date("2011-01-01"),as.Date("2011-01-01"),as.Date("2011-01-01"),as.Date("2011-01-01")),y=c(1.67,1.62,1.57,1.77,2.12))

save_e61("Fig3_Female_cohort.png",res=2,pad_width = 1,auto_scale = FALSE)




############### Figure 4


fig_4_1yr_growth_by_gender <- read_csv("fig_4_1yr_growth_by_gender.csv")
setDT(fig_4_1yr_growth_by_gender)
fig_4_1yr_growth_by_gender[,date := as.Date(paste0(year,"-07-01"))]

unique(fig_4_1yr_growth_by_gender$age)


volatility_summary <- fig_4_1yr_growth_by_gender[age == "25-55" & year != 2022]

ggplot(volatility_summary[gender != "All genders"],aes(x=date,y=p9010_res_1yr_log_chg,colour=gender)) + geom_line() + 
  plab(c("Women","Men"),x=c(as.Date("2004-01-01"),as.Date("2004-01-01")),y=c(0.83,0.77)) +
  scale_y_continuous_e61(limits = c(0.5,1,0.1)) + geom_rect(data = shading_periods_plots, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
                                                            fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  labs_e61(title = "90/10 ratio of residualized earnings",y = "", x = "Date",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings growth, residualized"),source = c("ABS","e61", "GRID"))

save_e61("Fig4_90_10.png",res=2,pad_width = 1,auto_scale = FALSE)
  
ggplot(volatility_summary[gender != "All genders"],aes(x=date,y=kelley_res_1yr_log_chg,colour=gender)) + geom_line() + 
  plab(c("Women","Men"),x=c(as.Date("2004-01-01"),as.Date("2004-01-01")),y=c(-0.07,0.13)) +
  scale_y_continuous_e61(limits = c(-0.2,0.2,0.05)) + geom_rect(data = shading_periods_plots, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
                                                            fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  labs_e61(title = "Kelley skewness of residualized earnings",y = "", x = "Date",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings growth, residualized"),source = c("ABS","e61", "GRID")) + add_baseline()

save_e61("Fig4_Kelley.png",res=2,pad_width = 1,auto_scale = FALSE)

ggplot(volatility_summary[gender != "All genders"],aes(x=date,y=crows_res_1yr_log_chg,colour=gender)) + geom_line() + 
  plab(c("Women","Men"),x=c(as.Date("2004-01-01"),as.Date("2004-01-01")),y=c(9.5,12.5)) +
  scale_y_continuous_e61(limits = c(8,14,1)) + geom_rect(data = shading_periods_plots, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
                                                                fill = "grey", alpha = 0.5, inherit.aes = FALSE) +
  labs_e61(title = "Excess Crow-Siddiqui Kurtosis of residualized earnings",y = "", x = "Date",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings growth, residualized"),source = c("ABS","e61", "GRID")) + add_baseline() 

save_e61("Fig4_Crow.png",res=2,pad_width = 1,auto_scale = FALSE)


###################### Figure 5

fig_5_p9010_1yr_growth <- read_csv("fig_5_p9010_1yr_growth.csv")
setDT(fig_5_p9010_1yr_growth)
fig_5_p9010_1yr_growth[,date := as.Date(paste0(year,"-07-01"))]

unique(fig_5_p9010_1yr_growth$year) # looks like 9999 is the average.

fig_5_data <- fig_5_p9010_1yr_growth[year == 9999 & gender != "All genders"]

ggplot(fig_5_data[gender == "Male" & rank_permanent_inc != 100 & age != "25-55"],aes(x=rank_permanent_inc,y=p9010_res_1yr_log_chg_by_skill,colour=age)) + geom_line() +
  geom_point(data = fig_5_data[gender == "Male" & rank_permanent_inc == 100 & age != "25-55"],aes(x = rank_permanent_inc, y = p9010_res_1yr_log_chg_by_skill, colour = age)) +
  plab(c("25-34","35-44","45-54"),x=c(50,50,50),y=c(1.24,1.12,1)) +
  scale_y_continuous_e61(limits = c(0.0,1.8,0.3)) +
  labs_e61(title = "Men Permanent Income growth dispersion (90-10 ratio)",y = "", x = "Rank",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings growth, permanet income"),source = c("ABS","e61", "GRID"))

save_e61("Fig5_male.png",res=2,pad_width = 1,auto_scale = FALSE)
  
ggplot(fig_5_data[gender == "Female" & rank_permanent_inc != 100 & age != "25-55"],aes(x=rank_permanent_inc,y=p9010_res_1yr_log_chg_by_skill,colour=age)) + geom_line() +
  geom_point(data = fig_5_data[gender == "Male" & rank_permanent_inc == 100 & age != "25-55"],aes(x = rank_permanent_inc, y = p9010_res_1yr_log_chg_by_skill, colour = age)) +
  plab(c("25-34","35-44","45-54"),x=c(50,50,50),y=c(1.24,1.12,1)) +
  scale_y_continuous_e61(limits = c(0.0,1.8,0.3)) +
  labs_e61(title = "Women Permanent Income growth dispersion (90-10 ratio)",y = "", x = "Rank",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings growth, permanet income"),source = c("ABS","e61", "GRID"))

save_e61("Fig5_female.png",res=2,pad_width = 1,auto_scale = FALSE)


############## Figure 7

fig_7_skewness_by_perm_income <- read_csv("fig_7_skewness_by_perm_income.csv")
setDT(fig_7_skewness_by_perm_income)

fig_7_data <- fig_7_skewness_by_perm_income[year == 9999 & gender != "All genders"]

ggplot(fig_7_data[gender == "Male" & rank_permanent_inc != 100 & age != "25-55"],aes(x=rank_permanent_inc,y=skew_res_1yr_log_chg_by_skill,colour=age)) + geom_line() +
  geom_point(data = fig_7_data[gender == "Male" & rank_permanent_inc == 100 & age != "25-55"],aes(x = rank_permanent_inc, y = skew_res_1yr_log_chg_by_skill, colour = age)) +
  plab(c("25-34","35-44","45-54"),x=c(20,20,20),y=c(-0.8,-1.2,-1.6)) +
  scale_y_continuous_e61(limits = c(-2.5,1,0.5)) +
  labs_e61(title = "Men Permanent Income skewness",y = "", x = "Rank",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings growth, log growth of residualized earnings"),source = c("ABS","e61", "GRID")) + add_baseline()

save_e61("Fig7_male.png",res=2,pad_width = 1,auto_scale = FALSE)


ggplot(fig_7_data[gender == "Female" & rank_permanent_inc != 100 & age != "25-55"],aes(x=rank_permanent_inc,y=skew_res_1yr_log_chg_by_skill,colour=age)) + geom_line() +
  geom_point(data = fig_7_data[gender == "Female" & rank_permanent_inc == 100 & age != "25-55"],aes(x = rank_permanent_inc, y = skew_res_1yr_log_chg_by_skill, colour = age)) +
  plab(c("25-34","35-44","45-54"),x=c(20,20,20),y=c(-0.8,-1.2,-1.6)) +
  scale_y_continuous_e61(limits = c(-2.5,1,0.5)) +
  labs_e61(title = "Women Permanent Income skewness",y = "", x = "Rank",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings growth, log growth of residualized earnings"),source = c("ABS","e61", "GRID")) + add_baseline()

save_e61("Fig7_female.png",res=2,pad_width = 1,auto_scale = FALSE)


################# Figure 8

fig_7_8_skew_kurt_by_perm_income <- read_csv("fig_7_8_skew_kurt_by_perm_income.csv")
setDT(fig_7_8_skew_kurt_by_perm_income)

fig_8_data <- fig_7_8_skew_kurt_by_perm_income[year == 9999 & gender != "All genders"]

ggplot(fig_8_data[gender == "Male" & rank_permanent_inc != 100 & age != "25-55"],aes(x=rank_permanent_inc,y=kurt_res_1yr_log_chg_by_skill,colour=age)) + geom_line() +
  geom_point(data = fig_8_data[gender == "Male" & rank_permanent_inc == 100 & age != "25-55"],aes(x = rank_permanent_inc, y = kurt_res_1yr_log_chg_by_skill, colour = age)) +
  plab(c("25-34","35-44","45-54"),x=c(70,70,70),y=c(6,11,16)) +
  scale_y_continuous_e61(limits = c(0,40,5)) +
  labs_e61(title = "Men Permanent Income Kurtosis",y = "", x = "Rank",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings growth, log growth of residualized earnings"),source = c("ABS","e61", "GRID")) + add_baseline()

save_e61("Fig8_male.png",res=2,pad_width = 1,auto_scale = FALSE)

ggplot(fig_8_data[gender == "Female" & rank_permanent_inc != 100 & age != "25-55"],aes(x=rank_permanent_inc,y=kurt_res_1yr_log_chg_by_skill,colour=age)) + geom_line() +
  geom_point(data = fig_8_data[gender == "Female" & rank_permanent_inc == 100 & age != "25-55"],aes(x = rank_permanent_inc, y = kurt_res_1yr_log_chg_by_skill, colour = age)) +
  plab(c("25-34","35-44","45-54"),x=c(10,10,10),y=c(16,21,26)) +
  scale_y_continuous_e61(limits = c(0,40,5)) +
  labs_e61(title = "Women Permanent Income Kurtosis",y = "", x = "Rank",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings growth, log growth of residualized earnings"),source = c("ABS","e61", "GRID")) + add_baseline()

save_e61("Fig8_female.png",res=2,pad_width = 1,auto_scale = FALSE)


################# Figure 9

fig_9_rank_rank_mobility <- read_csv("fig_9_rank_rank_mobility.csv")
setDT(fig_9_rank_rank_mobility)

unique(fig_9_rank_rank_mobility$year)

# The Norway graph says we just take a simple average, so lets do that

fig_9_result <- fig_9_rank_rank_mobility[, .(
  avg_one_year_ahead = mean(avg_pct_year_t_1_log_inc, na.rm = TRUE),
  avg_five_year_ahead = mean(avg_pct_year_t_5_log_inc, na.rm = TRUE),
  weighted_avg_one_year_ahead = sum(avg_pct_year_t_1_log_inc * N_pct_year_t_1_log_inc, na.rm = TRUE) / sum(N_pct_year_t_1_log_inc, na.rm = TRUE),
  weighted_avg_five_year_ahead = sum(avg_pct_year_t_5_log_inc * N_pct_year_t_5_log_inc, na.rm = TRUE) / sum(N_pct_year_t_5_log_inc, na.rm = TRUE)
), by = .(gender, pct_year_t_log_earnings, age)]

# fig_9_result_zero <- data.table(gender = c(rep("Female",3),rep("Male",3)),pct_year_t_log_earnings = rep(0,6),age = rep(c("25-34","35-44","45-55"),2),avg_one_year_ahead=rep(0,6),avg_five_year_ahead=rep(0,6),weighted_avg_one_year_ahead=rep(0,6),weighted_avg_five_year_ahead=rep(0,6))
# 
# fig_9_result <- rbind(fig_9_result,fig_9_result_zero)

ggplot(fig_9_result[gender == "Male" & pct_year_t_log_earnings != 100],aes(x=pct_year_t_log_earnings,y=avg_one_year_ahead,colour=age)) + geom_line() +
  geom_point(data = fig_9_result[gender == "Male" & pct_year_t_log_earnings == 100],aes(x = pct_year_t_log_earnings, y = avg_one_year_ahead, colour = age)) +
  plab(c("25-34","35-44","45-54"),x=c(70,70,70),y=c(6,11,16)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_y_continuous_e61(limits = c(-20,100,20)) +
  scale_x_continuous_e61(limits = c(0,100,20)) +
  labs_e61(title = "Men Mobility",y = "", x = "Rank",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings growth, log growth of residualized earnings"),source = c("ABS","e61", "GRID")) + add_baseline()

ggplot(fig_9_result[gender == "Male" & pct_year_t_log_earnings != 100],aes(x=pct_year_t_log_earnings,y=weighted_avg_one_year_ahead,colour=age)) + geom_line() +
  geom_point(data = fig_9_result[gender == "Male" & pct_year_t_log_earnings == 100],aes(x = pct_year_t_log_earnings, y = weighted_avg_one_year_ahead, colour = age)) +
  plab(c("25-34","35-44","45-54"),x=c(70,70,70),y=c(6,11,16)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_y_continuous_e61(limits = c(-20,100,20)) +
  scale_x_continuous_e61(limits = c(0,100,20)) +
  labs_e61(title = "Men Mobility",y = "", x = "Rank",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings growth, log growth of residualized earnings"),source = c("ABS","e61", "GRID")) + add_baseline()




ggplot(fig_9_result[gender == "Male" & pct_year_t_log_earnings != 100],aes(x=pct_year_t_log_earnings,y=avg_five_year_ahead,colour=age)) + geom_line() +
  geom_point(data = fig_9_result[gender == "Male" & pct_year_t_log_earnings == 100],aes(x = pct_year_t_log_earnings, y = avg_five_year_ahead, colour = age)) +
  plab(c("25-34","35-44","45-54"),x=c(70,70,70),y=c(16,31,46)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_y_continuous_e61(limits = c(-20,100,20)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs_e61(title = "Men 5-year Mobility",y = "", x = "Rank",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings growth, average rank of permanent income (2002-2017)"),source = c("ABS","e61", "GRID")) + add_baseline()

save_e61("Fig9_male.png",res=2,pad_width = 1,auto_scale = FALSE)

ggplot(fig_9_result[gender == "Female" & pct_year_t_log_earnings != 100],aes(x=pct_year_t_log_earnings,y=avg_five_year_ahead,colour=age)) + geom_line() +
  geom_point(data = fig_9_result[gender == "Female" & pct_year_t_log_earnings == 100],aes(x = pct_year_t_log_earnings, y = avg_five_year_ahead, colour = age)) +
  plab(c("25-34","35-44","45-54"),x=c(70,70,70),y=c(16,31,46)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_y_continuous_e61(limits = c(-20,100,20)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs_e61(title = "Women 5-year Mobility",y = "", x = "Rank",footnotes = c("Recession defined as period with two quarters of negative GDP per capita growth with GDP per capita below its prior peak","FY labour earnings growth, average rank of permanent income (2002-2017)"),source = c("ABS","e61", "GRID")) + add_baseline()

save_e61("Fig9_female.png",res=2,pad_width = 1,auto_scale = FALSE)

## Graphs for AI newsletter in August 2026.


library(tidyverse)
library(data.table)
library(theme61)
library(readxl)
library(readabs)

AI_dt <- read_excel("AI_scenarios.xlsx", 
                           sheet = "R")

setDT(AI_dt)

graph_dt <- melt(AI_dt,id.vars = c("Year","Shock"))

graph_dt[,value := value*100]

ggplot(graph_dt,aes(x=Year,y=value,colour=variable)) + geom_line() + facet_wrap("Shock")

ggplot() +
  geom_col(
    data = filter(graph_dt[Shock != "Prod"], variable %in% c("Individual", "Corporate")),
    aes(x = Year, y = value, fill = variable),
    position = "dodge"
  ) +
  geom_line(
    data = filter(graph_dt[Shock != "Prod"], variable == "Total"),
    aes(x = Year, y = value, colour = variable, group = 1),
    linewidth = 1
  ) +
  geom_point(
    data = filter(graph_dt[Shock != "Prod"], variable == "Total"),
    aes(x = Year, y = value, colour = variable),
    size = 2
  ) +
  scale_fill_manual(
    values = c(
      "Individual" = palette_e61(3)[1],
      "Corporate" = palette_e61(3)[2]
    )
  ) +
  scale_colour_manual(
    values = c(
      "Total" = palette_e61(3)[3]
    )
  ) +
  facet_wrap(~Shock)

ggplot() +
  geom_col(
    data = filter(graph_dt[Shock == "Prod_wage_2"], variable %in% c("Individual", "Corporate")),
    aes(x = Year, y = value, fill = variable),
    position = "dodge"
  ) +
  geom_line(
    data = filter(graph_dt[Shock == "Prod_wage_2"], variable == "Total"),
    aes(x = Year, y = value, colour = variable, group = 1),
    linewidth = 1
  ) +
  geom_point(
    data = filter(graph_dt[Shock == "Prod_wage_2"], variable == "Total"),
    aes(x = Year, y = value, colour = variable),
    size = 2
  ) +
  scale_fill_manual(
    values = c(
      "Individual" = palette_e61(3)[1],
      "Corporate" = palette_e61(3)[2]
    )
  ) +
  scale_colour_manual(
    values = c(
      "Total" = palette_e61(3)[3]
    )
  ) + labs_e61(title = "Productivity Increase")

prod_plot <- ggplot() +
  geom_col(
    data = filter(graph_dt[Shock == "Prod_wage"], variable %in% c("Individual", "Corporate")),
    aes(x = Year, y = value, fill = variable),
    position = "dodge"
  ) +
  geom_line(
    data = filter(graph_dt[Shock == "Prod_wage"], variable == "Total"),
    aes(x = Year, y = value, colour = variable, group = 1),
    linewidth = 1
  ) +
  geom_point(
    data = filter(graph_dt[Shock == "Prod_wage"], variable == "Total"),
    aes(x = Year, y = value, colour = variable),
    size = 2
  ) +
  scale_fill_manual(
    values = c(
      "Individual" = palette_e61(3)[1],
      "Corporate" = palette_e61(3)[2]
    )
  ) +
  scale_colour_manual(
    values = c(
      "Total" = palette_e61(3)[3]
    )
  ) + labs_e61(title = "Productivity Increase",
               y= "% change from forecast") +
  geom_hline(yintercept = 0) + scale_x_continuous_e61(limits = c(2027,2037,2)) + scale_y_continuous_e61(limits = c(-4,6,2))

wage_plot <- ggplot() +
  geom_col(
    data = filter(graph_dt[Shock == "Wage"], variable %in% c("Individual", "Corporate")),
    aes(x = Year, y = value, fill = variable),
    position = "dodge"
  ) +
  geom_line(
    data = filter(graph_dt[Shock == "Wage"], variable == "Total"),
    aes(x = Year, y = value, colour = variable, group = 1),
    linewidth = 1
  ) +
  geom_point(
    data = filter(graph_dt[Shock == "Wage"], variable == "Total"),
    aes(x = Year, y = value, colour = variable),
    size = 2
  ) +
  scale_fill_manual(
    values = c(
      "Individual" = palette_e61(3)[1],
      "Corporate" = palette_e61(3)[2]
    )
  ) +
  scale_colour_manual(
    values = c(
      "Total" = palette_e61(3)[3]
    )
  ) + labs_e61(title = "Wage decline",
               y= "% change from forecast") +
  geom_hline(yintercept = 0)+ plab(c("Individual tax","Company tax","Total tax"),x=c(2027,2027,2027),y=c(5.5,4.5,3.5)) + scale_x_continuous_e61(limits = c(2027,2037,2)) + scale_y_continuous_e61(limits = c(-4,6,2))

save_e61(filename = "AI_shock",wage_plot,prod_plot,format=c("png","pdf","svg"),res=2,footnotes = c("Scenarios are based on the PBO Build Your Own Budget Tool. Wage shock is equivalent to 1ppt lower wage growth per year. Productivity scenario involves a 0.6ppt increase in productivity growth and 0.5ppt reduction in wage growth relative to baseline forecasts."),sources = c("e61","PBO Build Your Own Budget 2026/27"))

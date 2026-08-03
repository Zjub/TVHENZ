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

ggplot(graph_dt,aes(x=Year,y=value,colour=variable)) + geom_line() + facet_wrap("Shock")

ggplot() +
  geom_col(
    data = filter(graph_dt, variable %in% c("Individual", "Corporate")),
    aes(x = Year, y = value, fill = variable),
    position = "dodge"
  ) +
  geom_line(
    data = filter(graph_dt, variable == "Total"),
    aes(x = Year, y = value, colour = variable, group = 1),
    linewidth = 1
  ) +
  geom_point(
    data = filter(graph_dt, variable == "Total"),
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

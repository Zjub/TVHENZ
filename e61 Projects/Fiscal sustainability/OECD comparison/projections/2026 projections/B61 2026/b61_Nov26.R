## Code for plots for late November 2026 Budget61.

if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  data.table,
  magrittr,
  janitor,
  ggrepel,
  theme61,
  readxl,
  readr,
  devtools
)

rm(list = ls()); gc()

## Overall spending and net debt

base_projection <- read_csv("B61 2026/base_projection.csv")

setDT(base_projection)

unique(base_projection$measure_label)

net_debt <- base_projection[measure_label == "Net debt (% GDP)"]

ggplot(net_debt,aes(x=year,y=value)) + geom_line() +
  labs_e61(title = "Consolidated Net debt",
           y = "% GDP",
           x = "")

save_e61("B61 2026/Benchmark_net_debt.png",res=2)

expenditure <- base_projection[measure_label == "Total expenditure (% GDP)"]

ggplot(expenditure,aes(x=year,y=value)) + geom_line()+
  labs_e61(title = "Consolidated Government Expenditure",
           y = "% GDP",
           x = "")

PC_expenditure <- base_projection[measure_label == "Real primary spending per person ($2030)"]

ggplot(PC_expenditure,aes(x=year,y=value/1000)) + geom_line()+
  labs_e61(title = "Consolidated Real Government Expenditure",
           y = "Per person, ($000)",
           x = "")


## Health function

health_benchmark <- read_csv("B61 2026/health_benchmark.csv")
setDT(health_benchmark)

ggplot(health_benchmark,aes(x=year,y=share_gdp)) + geom_line() +
  labs_e61(title = "Health Expenditure",
           y= "% GDP",
           x = "")

save_e61("B61 2026/Benchmark_Health.png",res=2)

# Subtract out effect of ageing

# health_contribution_year <- read_csv("B61 2026/health_contribution_year.csv")
# setDT(health_contribution_year)

health_contribution_year <- read_csv("B61 2026/health_annual_calculation.csv")
setDT(health_contribution_year)

health_contribution_year[,value_age_adjust := value/age_factor][,value_added_demand_adjust := value/income_factor]

health_age_year <- health_contribution_year[,.(value = sum(value), value_age_adjust = sum(value_age_adjust), value_added_demand_adjust = sum(value_added_demand_adjust)),by=.(year)]

ggplot(melt(health_age_year[,.(year,value,value_age_adjust)], id.vars = "year"), aes(y=value,x=year,colour=variable)) + geom_line() + plab(label = c("Projection","No Ageing")) +
  labs_e61(title = "Health spending would rise without ageing",
           y = "% GDP",
           x =)

save_e61("B61 2026/Health_age.png",res=2)

ggplot(melt(health_age_year[,.(year,value,value_added_demand_adjust)], id.vars = "year"), aes(y=value,x=year,colour=variable)) + geom_line() + plab(label = c("Projection","No Added Demand")) +
  labs_e61(title = "Health spending would fall without added demand",
           y = "% GDP",
           x =)

save_e61("B61 2026/Health_added_demand.png",res=2)

ggplot(melt(health_age_year, id.vars = "year"), aes(y=value,x=year,colour=variable)) + geom_line() + plab(label = c("Projection","Age","No Added Demand")) +
  labs_e61(title = "Health spending: Age and Added Demand",
           y = "% GDP",
           x =)

save_e61("B61 2026/Health_age_v_added_demand.png",res=2)

# Decomposition - Health

health_contributions <- read_csv("B61 2026/health_per_person_contributions.csv")

setDT(health_contributions)

health_decomp_dt <- health_contributions[,.(contributions = sum(contribution)), by=.(factor)][!(contributions == 0)]

health_decomp_dt[
  , factors := base::factor(
    c("Ageing", "Rel Price", "Added Demand",
      "Baumol/Indexation", "Health Productivity", "GDP growth"),
    levels = rev(c("Ageing", "Rel Price", "Added Demand",
               "Baumol/Indexation", "Health Productivity", "GDP growth"))
  )
]

ggplot(health_decomp_dt[!factors %in% "Rel Price"],aes(y=factors,x=contributions,fill=factor)) + geom_col() +
  labs_e61(title = "Drivers of Health Spending",
           subtitle = "Per person spending pressures",
           x = "% per capita GDP")

save_e61("B61 2026/Health_driver.png",res=2)


## Education function

education_benchmark <- read_csv("B61 2026/education_benchmark.csv")
setDT(education_benchmark)

ggplot(education_benchmark,aes(x=year,y=share_gdp)) + geom_line() +
  labs_e61(title = "Education Expenditure",
           y= "% GDP",
           x = "")

save_e61("B61 2026/Benchmark_Education.png",res=2)

# Subtract out effect of ageing

education_contribution_year <- read_csv("B61 2026/education_annual_calculation.csv")
setDT(education_contribution_year)

education_contribution_year[,value_age_adjust := value/age_factor]

education_age_year <- education_contribution_year[,.(value = sum(value), value_age_adjust = sum(value_age_adjust)),by=.(year)]

ggplot(melt(education_age_year, id.vars = "year"), aes(y=value,x=year,colour=variable)) + geom_line() + plab(label = c("Projection","No Ageing")) +
  labs_e61(title = "Education spending would rise without ageing",
           y = "% GDP",
           x =)

save_e61("B61 2026/Education_age.png",res=2)

# Decomposition - Education
# Issue is that we put in an income elasticity AND added service growth, so need to add together

education_contributions <- read_csv("B61 2026/education_per_person_contributions.csv")

setDT(education_contributions)

education_decomp_dt <- education_contributions[,.(contributions = sum(contribution)), by=.(factor)][!(contributions == 0)]

education_decomp_dt <- education_decomp_dt[, .(
  factor = fcase(
    factor %in% c("Income-related demand", "Additional service growth"), 
    "Demand & service growth", # Replace with your preferred new label
    default = factor
  ),
  contributions
)][, .(contributions = sum(contributions)), by = factor]

education_decomp_dt[
  , factors := base::factor(
    c("Ageing", "Added Demand",
      "Baumol/Indexation", "Ed Productivity", "GDP growth"),
    levels = rev(c("Ageing", "Added Demand",
                   "Baumol/Indexation", "Ed Productivity", "GDP growth"))
  )
]

ggplot(education_decomp_dt,aes(y=factors,x=contributions,fill=factor)) + geom_col() +
  labs_e61(title = "Drivers of Education Spending",
           subtitle = "Per person spending pressures",
           x = "% per capita GDP")

save_e61("B61 2026/Education_driver.png",res=2)

# 3. Extract coordinates for the "Added Demand" bar
added_val <- education_decomp_dt[factors == "Added Demand", contributions]
y_pos <- which(levels(education_decomp_dt$factors) == "Added Demand") # Equals 4

# 4. Plot with the dashed box
ggplot(education_decomp_dt, aes(y = factors, x = contributions, fill = factor)) +
  geom_col() +
  # Dashed empty box extending +0.4 from the end of Added Demand
  annotate(
    "rect",
    xmin = added_val,
    xmax = added_val + 0.4,
    ymin = y_pos - 0.45,
    ymax = y_pos + 0.45,
    fill = NA,              # Makes the box empty/transparent
    colour = "grey30",      # Border colour (adjust to match e61 palette if needed)
    linetype = "solid",
    linewidth = 0.6
  ) +
  # Optional: Add a subtle text label inside or next to the box
  #annotate(
  #  "text",
  #  x = added_val + 0.2,
  #  y = y_pos,
  #  label = "Stickiness\n(+0.4)",
  #  size = 3,
  #  colour = "grey30"
  #) +
  labs_e61(
    title = "Drivers of Education Spending",
    subtitle = "Per person spending pressures",
    x = "% per capita GDP",
    footnotes = c("Grey box represents implicit added demand funded if spending is not cut.")
  )

save_e61("B61 2026/Education_driver_sticky.png",res=2)

## Conclusion plot

net_debt_health_demand <- read_csv("B61 2026/net_debt_health_demand.csv")
setDT(net_debt_health_demand)


ggplot(net_debt_health_demand,aes(x=year,y=value,colour=case)) + geom_line() +
  labs_e61(title = "Consolidated Net debt",
           y = "% GDP",
           x = "") + plab(label = c("Status Quo","No Added Demand"),x=c(2035,2050),y=c(55,45))


save_e61("B61 2026/Health_reform_net_debt.png",res=2)


###

rm(list = ls())
gc()

# Load required libraries
library(tidyverse)
library(data.table)
library(vars)       # For VAR models
library(tseries)    # For stationarity tests
library(ggplot2)
library(readabs)

# Load and prepare GDP data
a <- read_abs("5206.0")
setDT(a)

filtered_a <- a %>%
  filter(str_detect(series, regex("price deflator", ignore_case = TRUE)))

unique(filtered_a$series)
unique(filtered_a$table_title)

unique(a[table_no == "5206007_income_from_gdp"]$series)

# Filter for nominal income components
gdp_components <- a[
  date >= as.Date("1980-01-01") &
    series %in% c(
      "Compensation of employees ;",
      "All sectors ;  Gross operating surplus ;",
      "Gross mixed income ;",
      "Taxes less subsidies on production and imports ;",
      "Statistical discrepancy (I) ;"
      #"GROSS DOMESTIC PRODUCT ;"
    ) &
    series_type == "Seasonally Adjusted" &
    table_no == "5206007_income_from_gdp"
]

gdp_check <- a[
  date >= as.Date("1980-01-01") &
    series %in% c(
      #"Compensation of employees ;",
      #"All sectors ;  Gross operating surplus ;",
      #"Gross mixed income ;",
      #"Taxes less subsidies on production and imports ;",
      #"Statistical discrepancy (I) ;"
      "GROSS DOMESTIC PRODUCT ;"
    ) &
    series_type == "Seasonally Adjusted" &
    table_no == "5206007_income_from_gdp"
]

ggplot(gdp_check[date >=as.Date("2014-01-01")],aes(x=date,y=value)) + geom_line()

unique(a$table_title)

# Filter for the deflator series
deflator <- a[
  series == "GROSS DOMESTIC PRODUCT ;" & 
    table_title == "Table 5. Expenditure on Gross Domestic Product (GDP), Implicit price deflators",
  .(date, deflator_value = value, series = "GDP deflator")
]

# Merge gdp_components with the deflator on date
gdp_components <- merge(gdp_components, deflator[, .(date, deflator_value)], by = "date", all.x = TRUE)
gdp_check <- merge(gdp_check, deflator[, .(date, deflator_value)], by = "date", all.x = TRUE)

# Calculate real values
gdp_components[, real_value := value / deflator_value]
gdp_check[, real_value := value / deflator_value]

unique(gdp_components$series)

# Ensure `date` is a proper Date object
gdp_components[, date := as.Date(date)]

# Get values in 2014-Q1 (or January 2014)
reference_values <- gdp_components[date == as.Date("2014-03-01"), .(series, reference_value = real_value)]

# Merge reference values into the main dataset
gdp_components <- merge(gdp_components, reference_values, by = "series", all.x = TRUE)

# Scale values relative to 2014-Q1
gdp_components[, scaled_value := real_value / reference_value]

# Plot each component scaled to 2014
ggplot(gdp_components[date >= as.Date("2014-01-01") & !series %in% c("Statistical discrepancy (I) ;","Taxes less subsidies on production and imports ;")], aes(x = date, y = scaled_value, color = series)) +
  geom_line() +
  labs_e61(
    title = "Components of Income GDP Scaled to 2014 Values",
    subtitle = "Deflated by GDPD",
    x = "Year",
    y = "",
    color = "Component"
  ) +
  theme_e61(legend = "bottom") +
  scale_y_continuous_e61(limits = c(0.9,1.5,0.1)) + 
  scale_x_date(
    date_breaks = "1 year", 
    date_labels = "%Y"      
  )

gdp_components[,reference_value := NULL]

# Calculate total real GDP for each time period
gdp_components[, total_real_gdp := sum(real_value), by = date]

# Get 2014-Q1 values for each component and total GDP
reference_values <- gdp_components[date == as.Date("2014-03-01"), .(
  series, reference_value = real_value
)]

reference_total_gdp <- gdp_components[date == as.Date("2014-03-01"), sum(real_value)]

# Merge 2014 reference values into the main dataset
gdp_components <- merge(gdp_components, reference_values, by = "series", all.x = TRUE)

# Calculate the total change in GDP relative to 2014-Q1
gdp_components[, total_change_gdp := total_real_gdp - reference_total_gdp, by = date]

# Calculate the change in each component’s real value relative to 2014-Q1
gdp_components[, component_change := real_value - reference_value]

# Calculate each component's contribution to the total change in GDP
gdp_components[, contribution := component_change / total_change_gdp]

# Handle cases where total_change_gdp is 0 to avoid division errors
gdp_components[total_change_gdp == 0, contribution := 0]

# Calculate total GDP change as a fraction of the 2014-Q1 value
gdp_components[, total_gdp_relative := total_change_gdp / reference_total_gdp]

# Keep only one row per date for the total GDP line
#total_gdp_line <- gdp_components[, .(date, total_gdp_relative)][!duplicated(date)]

gdp_components[, contribution_to := contribution * total_gdp_relative]


# Plot contributions to GDP growth with the total GDP dashed line
ggplot(
  gdp_components[date >= as.Date("2014-01-01")], # & !series %in% c("Statistical discrepancy (I) ;", "Taxes less subsidies on production and imports ;")
  aes(x = date, y = contribution_to, fill = series)
) +
  geom_area(position = "stack") +
  labs_e61(
    title = "Contribution to GDP Growth Relative to 2014",
    x = "Year",
    y = "Contribution to GDP Growth",
    fill = "Component"
  ) +
  theme_minimal() +
  scale_y_continuous(
    name = "Contribution to GDP Growth",
    sec.axis = sec_axis(~ ., name = "Total GDP Growth (Dashed Line)")
  ) +
  geom_line(aes(x = date, y = total_gdp_relative), 
    color = "black", 
    linetype = "dashed", 
    size = 1
  )


### Also construct a per capita version

population <- a[
  date >= as.Date("1980-01-01") &
    series %in% c(
      "Gross domestic product: Chain volume measures ;",
      "GDP per capita: Chain volume measures ;"
      #"GROSS DOMESTIC PRODUCT ;"
    ) &
    series_type == "Seasonally Adjusted"
][,.(date,series,value)]

pop <- dcast(population, formula = date ~ series)
colnames(pop) <- c("date","Per_cap","GDP")

pop[,population := GDP/Per_cap]

gdp_check <- merge(gdp_check, pop[, .(date, population)], by = "date", all.x = TRUE)
gdp_check[, real_value_pc := real_value / population]

ggplot(gdp_check[date >= as.Date("2014-01-01")],aes(x=date,y=real_value)) + geom_line()
ggplot(gdp_check[date >= as.Date("2014-01-01")],aes(x=date,y=real_value_pc)) + geom_line()

# Merge population data into gdp_components
gdp_components <- merge(gdp_components, pop[, .(date, population)], by = "date", all.x = TRUE)

# Calculate real values per capita
gdp_components[, real_value_pc := real_value / population]

# Calculate total real GDP per capita for each time period
gdp_components[, total_real_gdp_pc := sum(real_value_pc), by = date]

# Get 2014-Q1 values for each component and total GDP per capita
reference_values_pc <- gdp_components[date == as.Date("2014-03-01"), .(
  series, reference_value_pc = real_value_pc
)]
reference_total_gdp_pc <- gdp_components[date == as.Date("2014-03-01"), sum(real_value_pc)]

# Merge 2014 reference values into the main dataset
gdp_components <- merge(gdp_components, reference_values_pc, by = "series", all.x = TRUE)

# Calculate the total change in GDP per capita relative to 2014-Q1
gdp_components[, total_change_gdp_pc := total_real_gdp_pc - reference_total_gdp_pc, by = date]

# Calculate the change in each component’s real value per capita relative to 2014-Q1
gdp_components[, component_change_pc := real_value_pc - reference_value_pc]

# Calculate each component's contribution to the total change in GDP per capita
gdp_components[, contribution_pc := component_change_pc / total_change_gdp_pc]

# Handle cases where total_change_gdp_pc is 0 to avoid division errors
gdp_components[total_change_gdp_pc == 0, contribution_pc := 0]

# Calculate total GDP per capita change as a fraction of the 2014-Q1 value
gdp_components[, total_gdp_relative_pc := total_change_gdp_pc / reference_total_gdp_pc]

# Calculate the contribution to GDP growth per capita
gdp_components[, contribution_to_pc := contribution_pc * total_gdp_relative_pc]

# Plot contributions to GDP per capita growth with the total GDP per capita dashed line
ggplot(
  gdp_components[date >= as.Date("2014-01-01")], 
  aes(x = date, y = contribution_to_pc, fill = series)
) +
  geom_area(position = "stack") +
  labs_e61(
    title = "Contribution to Per Capita GDP Growth Relative to 2014",
    x = "Year",
    y = "",
    fill = "Component"
  ) +
  scale_y_continuous_e61(labels = scales::percent_format(),
                     limits = c(-0.2,0.2,0.05),
    name = ""
  ) +
  geom_line(
    aes(x = date, y = total_gdp_relative_pc), 
    color = "black", 
    linetype = "dotted", 
    size = 1
  ) +
  #theme_e61(legend = "bottom") +
  plab(c("Operating Surplus","Compensation of Employees","Mixed Income","Discrepency","Tax-Subsidies"),x=rep("2014-03-01",times=5),y=c(0.17,0.13,0.08,-0.03,-0.08)) +
  geom_hline(yintercept = 0)

save_e61("GDP income decomp.png",res=2,pad_width = 1)


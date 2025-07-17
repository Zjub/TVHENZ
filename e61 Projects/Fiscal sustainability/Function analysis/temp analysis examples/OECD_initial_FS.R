# Topic: Initial play with SC methods on E/GDP
# Author: Matt Nolan
# Created: 3/7/2025
# Last edit: 3/7/2025
# Last editor: Matt Nolan

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


## Conditions

year_data <- 1999 # Find countries available from a given year.
outliers <- c("Ireland") # Remove countries

### Initial exercise ----

OECD_exp_GDP <- read_excel("table4_gov_exp-gdp.xlsx", 
                                 sheet = "exp_%_gpd", range = "B2:BC88")

setDT(OECD_exp_GDP)
setnames(OECD_exp_GDP, c("...1", "...2"), c("country", "level"))

# Collapse by country
OECD_dt <- OECD_exp_GDP[, lapply(.SD, sum,na.rm=TRUE), by = country, .SDcols = patterns("^\\d{4}$")]
setDT(OECD_dt)

OECD_dt[,.(country,`2022`)][order(`2022`)]


OECD_long_dt <- melt(OECD_dt,id.vars = "country",variable.name = "year",value.name="value")[value > 0]
OECD_long_dt$year <-as.numeric(as.character(OECD_long_dt$year))
OECD_long_dt$group <- ifelse(OECD_long_dt$country == "Australia", "Australia", "Other")

ggplot(OECD_long_dt[year > 1996], aes(x = year, y = value, colour = group, group = country)) +
  geom_line() +
  scale_colour_manual(values = c("Australia" = "blue", "Other" = "grey70")) +
  labs(colour = "")



country_set <- OECD_long_dt[year == year_data & !country %in% outliers]$country
length(country_set)

OECD_forsynth <- OECD_long_dt[country %in% country_set]
OECD_forsynth[, index := value / value[year == year_data], by = country]

synth_result <- OECD_forsynth %>%
  filter(year >= year_data, year <= 2022) %>%
  synthetic_control(outcome = index, #value is value, index is set relative to the starting year - purpose is to remove initial level difference, but still estimate in levels
                    unit = country,
                    time = year,
                    i_unit = "Australia",
                    i_time = 2007,
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = year_data:2007,
                     mean_pre = mean(index, na.rm = TRUE)) %>%
  generate_weights(time_window = year_data:2007) %>%
  generate_control()


plot_trends(synth_result)

save_e61("SC_temp.png",res=2)

plot_differences(synth_result)

donor_weights <- grab_unit_weights(synth_result)
donor_weights %>%
  arrange(desc(weight)) %>%
  print(n = Inf)

ggplot(donor_weights, aes(x = reorder(unit, weight), y = weight)) +
  geom_col(fill = palette_e61(3)[1]) +
  coord_flip() +
  labs(x = "Donor Country", y = "Weight", 
       title = "Weights of Donor Countries in Synthetic Australia")

synth_df <- synth_result %>% grab_synthetic_control()

gap_df <- synth_df %>%
  mutate(gap = real_y - synth_y)

pre_mspe <- gap_df %>%
  filter(time_unit <= 2007) %>%
  summarise(mspe = mean(gap^2)) %>%
  pull(mspe)

post_mspe <- gap_df %>%
  filter(time_unit > 2007) %>%
  summarise(mspe = mean(gap^2)) %>%
  pull(mspe)

mspe_ratio <- post_mspe / pre_mspe
print(mspe_ratio)


#### Compare treatment across countries inc MSPE ----

# Make some placebos to test - this is easier with the Synth package, even if tidy synth is much neater for the intial set up
# Get list of donor countries (excluding Australia)
donor_countries <- OECD_forsynth$country %>% unique()
donor_countries <- setdiff(donor_countries, "Australia")

# Function to run synthetic control for a given treated country
run_synth <- function(treated_country) {
  OECD_forsynth %>%
    filter(year >= year_data, year <= 2022) %>%
    synthetic_control(
      outcome = index,
      unit = country,
      time = year,
      i_unit = treated_country,
      i_time = 2007
    ) %>%
    generate_predictor(time_window = year_data:2007, mean_pre = mean(index, na.rm = TRUE)) %>%
    generate_weights(time_window = year_data:2007) %>%
    generate_control() %>%
    grab_synthetic_control() %>%
    mutate(treated_unit = treated_country)
}

# Run placebo synthetic controls for each donor country, then combine with Australia
placebo_results <- map_dfr(donor_countries, run_synth)

australia_synth <- synth_result %>%
  grab_synthetic_control() %>%
  mutate(treated_unit = "Australia")

all_results <- bind_rows(australia_synth, placebo_results)

all_results <- all_results %>%
  mutate(gap = real_y - synth_y)

setDT(all_results)

ggplot(all_results, aes(x = time_unit, y = gap, group = treated_unit)) +
  geom_line(aes(color = (treated_unit == "Australia")), size = 1) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "grey70")) +
  geom_vline(xintercept = 2007, linetype = "dashed") +
  labs(
    title = "Placebo Test: Gaps Between Actual and Synthetic Controls",
    subtitle = "Australia in red, other countries in grey",
    x = "Year", y = "Gap (Indexed Spending)"
  ) 

mspe_df <- all_results %>%
  group_by(treated_unit) %>%
  summarise(
    pre_mspe = mean(gap[time_unit <= 2007]^2, na.rm = TRUE),
    post_mspe = mean(gap[time_unit > 2007]^2, na.rm = TRUE),
    mspe_ratio = post_mspe / pre_mspe,
    .groups = "drop"
  ) %>%
  arrange(desc(mspe_ratio))

print(mspe_df,n=30)

all_results[gap == min(gap)]
all_results[time_unit != 2008][gap == max(gap)]

### "Leave one out" sensitivity testing to Australia results (Synth offers this automatically you know) ----
# TBD

### Include demographics ----
# First import the five year population totals from the OECD, by country and for years between 1991 and 2022.

OECD_pop <- read_csv("OECD_pop.csv")
setDT(OECD_pop)

unique(OECD_pop$AGE) # Can identify off the last digits on the AGE variable. As we are making more aggregate categories just use names as is.

OECD_pop[,age_group := case_when(AGE %in% c("Y_LE4","Y5T9","Y10T14") ~ 1,
                                 AGE %in% c("Y15T19","Y20T24") ~ 2,
                                 AGE %in% c("Y25T29","Y30T34","Y35T39") ~ 3,
                                 AGE %in% c("Y40T44","Y45T49","Y50T54") ~ 4,
                                 AGE %in% c("Y55T59","Y60T64") ~ 5,
                                 AGE %in% c("Y65T69","Y70T74") ~ 6,
                                 AGE %in% c("Y75T79","Y80T84","Y_GE85") ~ 7,
                                 .default = 999
                                 )]

unique(OECD_pop$age_group)

# Collapse the data down to these seven age categories.

OECD_age_pop <- OECD_pop[,.(number = sum(OBS_VALUE)),by=.(`Reference area`,age_group,TIME_PERIOD)]
OECD_total_pop <- OECD_pop[,.(total = sum(OBS_VALUE)),by=.(`Reference area`,TIME_PERIOD)]

OECD_pop_prop <- OECD_total_pop[OECD_age_pop,on=.(`Reference area`,TIME_PERIOD)][,prop := number/total]
colnames(OECD_pop_prop) <- c("country","year","total_pop","age_group","number_pop","prop_pop")


OECD_pop_prop[year == 2022 & age_group %in% c(6,7)][,.(prop_pop = sum(prop_pop)),by=.(country)][order(prop_pop)]

## Include proportions 65-74 and 75+ into the SC
OECD_age_wide <- OECD_pop_prop[age_group %in% c(6, 7)]

OECD_age_wide <- dcast(
  OECD_age_wide, 
  country + year + total_pop ~ age_group, 
  value.var = "prop_pop"
)

setnames(OECD_age_wide, c("6", "7"), c("prop_65_74", "prop_75_plus"))

OECD_age_wide[is.na(prop_65_74)]
OECD_age_wide[is.na(prop_75_plus)]

# Set up synth

OECD_forsynth2 <- OECD_forsynth[OECD_age_wide,on=.(country,year)]
OECD_forsynth2[year >= year_data &year <= 2022][is.na(value)]

OECD_forsynth2 <- OECD_forsynth2[country %in% country_set] # As we have the Irish population they show back up here, so just need to boot them out

OECD_forsynth2[is.na(prop_65_74)]
OECD_forsynth2[is.na(prop_75_plus)]

dim(OECD_forsynth[year >= year_data &year <= 2022])
dim(OECD_forsynth2[year >= year_data &year <= 2022])

length(unique(OECD_forsynth$country))
length(unique(OECD_forsynth2$country))

OECD_forsynth[year >= year_data & year <= 2022,.N,by=.(country)]
OECD_forsynth2[year >= year_data & year <= 2022,.N,by=.(country)]

OECD_forsynth[year >= year_data,.N,by=.(country)]
OECD_forsynth2[year >= year_data,.N,by=.(country)]

is.na(OECD_forsynth2[,mean(prop_75_plus),by=.(country,year)]$V1)



synth_result2 <- OECD_forsynth2 %>%
  filter(year >= year_data, year <= 2022) %>%
  synthetic_control(
    outcome = index,
    unit = country,
    time = year,
    i_unit = "Australia",
    i_time = 2007,
    generate_placebos = TRUE
  ) %>%
  generate_predictor(
    time_window = year_data:2007,
    mean_pre = mean(index, na.rm = TRUE),
    mean_prop_65_74 = mean(prop_65_74, na.rm = TRUE),
    mean_prop_75_plus = mean(prop_75_plus, na.rm = TRUE)
  ) %>%
  generate_weights(time_window = year_data:2007) %>%
  generate_control()


plot_trends(synth_result2)

save_e61("SC_temp_age.png",res=2)

donor_weights2 <- grab_unit_weights(synth_result2)
donor_weights2 %>%
  arrange(desc(weight)) %>%
  print(n = Inf)

ggplot(donor_weights2, aes(x = reorder(unit, weight), y = weight)) +
  geom_col(fill = palette_e61(3)[1]) +
  coord_flip() +
  labs(x = "Donor Country", y = "Weight", 
       title = "Weights of Donor Countries in Synthetic Australia (age adj)")







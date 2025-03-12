## Slightly edited version of Matthew M's additional charts for the replacement rate note.
# Run this code after MM Replacement Rates analysis_2

library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(tidyverse)
library(data.table)
library(readxl)


# Function to compute weighted quantiles
weighted_quantile <- function(x, w, probs) {
  Hmisc::wtd.quantile(x, weights = w, probs = probs, na.rm = TRUE)
}


Rep_rates_df_subset_filtered <- Rep_rates_df_subset[hours0_taxable_benefit + hours0_net_fam_a_income + hours0_net_fam_b_income > 0]

# Bin 'hours0_net_income' into intervals of 100
Rep_rates_df_subset_filtered <- Rep_rates_df_subset_filtered %>%
  mutate(income_bin = floor(current_net_income / 100) * 100)

# Calculate weighted percentiles per bin
percentile_df <- Rep_rates_df_subset_filtered %>%
  group_by(income_bin) %>%
  summarise(
    p25 = weighted_quantile(hours0_net_income, SIHPSWT, 0.25),
    median = weighted_quantile(hours0_net_income, SIHPSWT, 0.50),
    p75 = weighted_quantile(hours0_net_income, SIHPSWT, 0.75),
    n = n(), 
    .groups = 'drop'
  )

percentile_df <- subset(percentile_df, income_bin < 3000)

# Plot
p22 <- ggplot(percentile_df, aes(x = income_bin)) +
  geom_line(aes(y = p25, color = "25th Percentile"), size = 1, col = e61_maroondark,  linetype = "dashed") +
  geom_line(aes(y = median, color = "Median"), size = 1, color = e61_skydark) +
  geom_line(aes(y = p75, color = "75th Percentile"), size = 1, col = e61_teallight,  linetype = "dashed") +
  labs(x = "Hours0 Net Income (binned)", y = "Income", color = "Quantiles") +
  xlim(0, 3000) + labs_e61(x = "Pre-Job Loss Income", y = "Post $", 
                           title = "Incomes") + scale_y_continuous_e61(limits = c(0, 3000, 1000)) + 
  geom_abline(slope = 1, intercept = 0) + geom_hline(yintercept = 612.18, col = e61_greydark) + xlim(0, 3700) + plot_label(
    c("Pre = Post \nJob Loss \nIncome", "Single \nPoverty \nLine", "Q3", "Median", "Q1"), 
    c(2900, 2, 3000, 3000, 3000), c(2500, 1100, 800, 500, 300), c("black", e61_greydark, e61_teallight, e61_skydark, e61_maroondark)
  )


Rep_rates_df_subset_filtered <- Rep_rates_df_subset_filtered %>%
  mutate(income_bin = floor(current_net_income / 200) * 200)


# Calculate weighted percentiles per bin
percentile_df <- Rep_rates_df_subset_filtered %>%
  group_by(income_bin) %>%
  summarise(
    p25 = weighted_quantile(net_RR * 100, SIHPSWT, 0.25),
    median = weighted_quantile(net_RR * 100, SIHPSWT, 0.50),
    p75 = weighted_quantile(net_RR * 100, SIHPSWT, 0.75),
    n = n(), 
    .groups = 'drop'
  )

percentile_df <- subset(percentile_df, income_bin < 3001)

# Plot
p32 <- ggplot(percentile_df, aes(x = income_bin)) +
  geom_line(aes(y = p25, color = "25th Percentile"), size = 1, col = e61_maroondark,  linetype = "dashed") +
  geom_line(aes(y = median, color = "Median"), size = 1, color = e61_skydark) +
  geom_line(aes(y = p75, color = "75th Percentile"), size = 1, col = e61_teallight,  linetype = "dashed") +
  labs(x = "Hours0 Net Income (binned)", y = "Income", color = "Quantiles") +
  xlim(0, 3000) + labs_e61(x = "Pre-Job Loss Income", y = "%", 
                           title = "Replacement Rates") + xlim(0, 3700) + plot_label(
                             c("Q3", "Median", "Q1"), 
                             c(3050, 3050, 3050), c(20,10, 5), c(e61_teallight, e61_skydark, e61_maroondark) 
                           ) + scale_y_continuous_e61(limits = c(0, 75, 25))

save_e61("Twopanelincomexaxis.pdf", p22, p32, title = "Incomes and Replacement Rates Post Job Loss", 
         subtitle = "Conditional on Recieving any Government Support")



## This appears to be broken 
# 
# # Define bins for net_RR * 100
# percentile_df <- percentile_df %>%
#   mutate(RR_bin = cut(net_RR * 100,
#                       breaks = c(-Inf, 0, 10, 20, 30, 40, 50, 60, Inf),
#                       labels = c("0", "1-\n10", "10-\n20", "20-\n30", "30-\n40", "40-\n50", "50-\n60", "60+"),
#                       include.lowest = TRUE))
# 
# # Aggregate weighted counts by bin
# RR_binned_counts <- percentile_df %>%
#   group_by(RR_bin) %>%
#   summarise(weighted_count = sum(SIHPSWT, na.rm = TRUE), .groups = "drop")
# 
# # Compute total weighted count and proportion
# total_weighted_count <- sum(RR_binned_counts$weighted_count, na.rm = TRUE)
# RR_binned_counts <- RR_binned_counts %>%
#   mutate(proportion = weighted_count / total_weighted_count)
# 
# # Plot the bar chart with proportions
# distribution <- ggplot(RR_binned_counts, aes(x = RR_bin, y = proportion * 100)) +
#   geom_col() +
#   labs_e61(x = "Replacement Rate (%)", 
#            y = "%", 
#            subtitle = "Distribution of Replacement Rates", 
#            sources = c("e61", "ABS")) + scale_y_continuous_e61(limits = c(0, 30, 5))
# 
# save_e61("RR_distribution.pdf", distribution)

# Import data
df <- read_excel("RR_international.xlsx")

# Ensure columns have the correct type
df <- df %>%
  mutate(year = as.integer(year),
         months_unempl = as.integer(months_unempl),
         nrr = as.numeric(nrr))

# Calculate rank of Australia over time (excluding "OECD - Total")
df_ranked <- df %>%
  filter(country_long != "OECD - Total") %>%
  group_by(year, months_unempl) %>%
  mutate(rank = rank(-nrr, ties.method = "first")) %>%  # Rank highest nrr as 1
  ungroup() %>%
  filter(country_long == "Australia")

# Plot 1: Australia’s Rank Over Time
rank <- ggplot(df_ranked, aes(x = months_unempl, y = -rank)) +
  geom_line(color = "gold", size = 1.2) + scale_y_continuous_e61(limits = c(-35, 0)) + add_baseline() + 
  labs_e61(subtitle = "Australia's replacement rate rank among OECD countries, by duration of unemployment", 
           y = "Rank", sources = c("e61", "ABS"),  footnotes = "Replacement Rate is for a single person earning the average wage. Housing
           costs = 30% of the average Wage" )

save_e61("rankplot.pdf", rank)

setDT(df)
unique(df$country_long)

df[months_unempl == 40][order(nrr)]

# Plot 2: nrr for All Countries Over Time
ggplot(df, aes(x = months_unempl, y = nrr, group = country_long, color = country_long)) +
  geom_line(data = df %>% filter(country_long != "Australia" & country_long != "OECD - Total"),
            size = 0.8, alpha = 0.5, color = "#E0E0E0") +  # All other countries in grey
  geom_line(data = df %>% filter(country_long == "Australia"),
            color = "gold", size = 1.2) +  # All other countries in grey
  geom_line(data = df %>% filter(country_long == "Canada"),
            color = e61_maroondark, size = 1.2) + 
  geom_line(data = df %>% filter(country_long == "United States"),
            color = e61_teallight, size = 1.2) + 
  geom_line(data = df %>% filter(country_long == "United Kingdom"),
            color = e61_bluedark, size = 1.2) + 
  geom_line(data = df %>% filter(country_long == "Luxembourg"),
            color = e61_orangedark, size = 1.2) + scale_y_continuous_e61(limits = c(0, 100,20)) + 
  labs_e61(x = "Months in Unemployment", y = "%", sources = c("e61", "OECD"), 
           footnotes = "Replacement Rate is for a single person earning the average wage. Housing
           costs = 30% of the average Wage", subtitle = "Replacement rate by the duration of Unemployment") + 
  plot_label(c("Australia", "Luxembourg",  "Canada", "U.S.","U.K."), 
             c(47, 25, 50, 55,35 ), c(35, 85,  12, 3,35), c("gold", e61_orangedark, e61_maroondark, e61_teallight, e61_bluedark)) + xlim(0, 63)

save_e61("duration.pdf")

## Check alternative RRs from here https://data-explorer.oecd.org/vis?lc=en&fs[0]=Topic%2C1%7CEmployment%23JOB%23%7CBenefits%252C%20earnings%20and%20wages%23JOB_BW%23&pg=0&fc=Topic&bp=true&snb=21&vw=br&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_TAXBEN_NRR%40DF_NRR&df[ag]=OECD.ELS.JAI&df[vs]=1.0&dq=...S_C0..AW100._Z.M60%2BM59%2BM58%2BM57%2BM56%2BM55%2BM54%2BM53%2BM52%2BM51%2BM50%2BM49%2BM47%2BM48%2BM46%2BM45%2BM44%2BM43%2BM42%2BM41%2BM40%2BM38%2BM39%2BM37%2BM30%2BM31%2BM32%2BM33%2BM34%2BM35%2BM29%2BM28%2BM27%2BM26%2BM25%2BM23%2BM22%2BM21%2BM20%2BM19%2BM17%2BM18%2BM16%2BM15%2BM14%2BM13%2BM11%2BM10%2BM9%2BM8%2BM7%2BM5%2BM3%2BM1%2BM2%2BM4%2BM6%2BM12%2BM24%2BM36.YES.YES.A&pd=2023%2C2023&to[TIME_PERIOD]=false

#OECD_RRs <- read_csv("OECD_RRs.csv")
#OECD_RRs <- read_csv("OECD_RRs_mult.csv")
OECD_RRs <- read_csv("OECD_RRs_full.csv")
setDT(OECD_RRs)

year_set = 2023

OECD_RRs[REF_AREA == "MLT" & TIME_PERIOD == 2024]

OECD_RRs <- OECD_RRs[,.(year = TIME_PERIOD,country = `Reference area`,UNEMP_DURATION,nrr = OBS_VALUE)][, Duration := as.numeric(sub("M", "", UNEMP_DURATION))][,UNEMP_DURATION := NULL]

Aus_RRs <- OECD_RRs[country == "Australia"]
US_RRs <- OECD_RRs[country == "United States"]

diff_SR_RRs <- OECD_RRs[Duration ==1 & year %in% c(2003,2023)]
dt_SR_RRs <- dcast(diff_SR_RRs, country ~ year, value.var = "nrr")
dt_SR_RRs[, difference := `2023` - `2003`]

diff_LR_RRs <- OECD_RRs[Duration ==24 & year %in% c(2003,2023)]
dt_LR_RRs <- dcast(diff_LR_RRs, country ~ year, value.var = "nrr")
dt_LR_RRs[, difference := `2023` - `2003`]

OECD_RRs <- OECD_RRs[year == year_set]

ggplot(OECD_RRs,aes(x=Duration,y=nrr,group=country,colour=country)) +
  geom_line(data = OECD_RRs %>% filter(country != "Australia" & country != "OECD - Total"),
            size = 0.8, alpha = 0.5, color = "#E0E0E0") +  # All other countries in grey
  geom_line(data = OECD_RRs %>% filter(country == "Australia"),
            color = "gold", size = 1.2) +  # All other countries in grey
  geom_line(data = OECD_RRs %>% filter(country == "Canada"),
            color = e61_maroondark, size = 1.2) + 
  geom_line(data = OECD_RRs %>% filter(country == "United States"),
            color = e61_teallight, size = 1.2) + 
  geom_line(data = OECD_RRs %>% filter(country == "United Kingdom"),
            color = e61_bluedark, size = 1.2) + 
  geom_line(data = OECD_RRs %>% filter(country == "Luxembourg"),
            color = e61_orangedark, size = 1.2) + scale_y_continuous_e61(limits = c(0, 100,20)) + 
  labs_e61(x = "Months in Unemployment", y = "%", 
           sources = c("e61", "OECD"), 
           footnotes = "Replacement Rate is for a single person earning the average wage. Housing
           costs = 30% of the average Wage", 
           subtitle = paste0("Replacement rates during , ",year_set)) + 
  plot_label(c("Australia", "Luxembourg",  "Canada", "U.S.","U.K."), 
             c(47, 25, 50, 55,35 ), c(65, 85,  12, 3,65), c("gold", e61_orangedark, e61_maroondark, e61_teallight, e61_bluedark)) + xlim(0, 63)

save_e61("duration.pdf") # Use these OECD figures instead

ggplot(Aus_RRs,aes(x=1,y=nrr,group=as.factor(year),colour=as.factor(year))) + geom_col(position="dodge") +
  theme_e61(legend= "bottom")

ggplot(US_RRs,aes(x=Duration,y=nrr,group=year,colour=year)) + geom_line() +
  geom_line(data = US_RRs,
            size = 0.8, alpha = 0.5, color = "#E0E0E0") +
  geom_line(data = US_RRs %>% filter(year == 2001),
            color = e61_maroondark, size = 1.2) +
  geom_line(data = US_RRs %>% filter(year == 2007),
            color = e61_maroonlight, size = 1.2) +
  geom_line(data = US_RRs %>% filter(year == 2016),
            color = e61_bluelight, size = 1.2) +
  geom_line(data = US_RRs %>% filter(year == 2023),
            color = e61_bluedark, size = 1.2) 

dt_SR_RRs <- dt_SR_RRs[order(difference)]
dt_SR_RRs[, country := factor(country, levels = country)]
 
ggplot(dt_SR_RRs[!is.na(difference)], aes(x = country, y = difference)) + 
  geom_col() +
  coord_flip() +
  labs(x = "Country", y = "Difference (2023 - 2003)") 

dt_LR_RRs <- dt_LR_RRs[order(difference)]
dt_LR_RRs[, country := factor(country, levels = country)]

ggplot(dt_LR_RRs[!is.na(difference)], aes(x = country, y = difference)) + 
  geom_col() +
  coord_flip() +
  labs(x = "Country", y = "Difference (2023 - 2003)") 

add_row <- data.table(
  country = "Australia (EIAC)",
  `2003` = NA_real_,  # Ensure NA is stored as a numeric value
  `2023` = 29 * 1.33,
  difference = NA_real_
)

add_row2 <- data.table(
  country = "Australia (ACOSS)",
  `2003` = NA_real_,  # Ensure NA is stored as a numeric value
  `2023` = 29 * 1.47,
  difference = NA_real_
)


dt_LR_RRs <- rbind(dt_LR_RRs,add_row,add_row2)

dt_LR_RRs <- dt_LR_RRs[order(`2023`)]
dt_LR_RRs[, country := factor(country, levels = country)]

ggplot(dt_LR_RRs, aes(x = country, y = `2023`, fill = country %in% c("Australia", "Australia (EIAC)", "Australia (ACOSS)"))) + 
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "gold", "FALSE" = "grey")) +
  labs(x = "Country", y = "",subtitle="2023 Replacement Rate (Two-years unemployed)") 
  


############################################################################################
#############################################################################################

# Convert dataframe to data.table
setDT(Rep_rates_df_subset)
Rep_rates_df_subset_filtered <- Rep_rates_df_subset[hours0_taxable_benefit + hours0_net_fam_a_income + hours0_net_fam_b_income > 0]
# Bin incomes
Rep_rates_df_subset_filtered[, income_bin := floor(current_net_income / 100) * 100]

# Create interaction category
Rep_rates_df_subset_filtered[, interaction_cat := paste0(
  fifelse(partnered == 0, "Single", "Partnered"), 
  ", ", 
  fifelse(Numb_dep_cat == "0", "no dependents", 
          fifelse(Numb_dep_cat == "1-2", "1-2 dependents", 
                  "3+ dependents"))
)]

# Function to calculate weighted quantiles
# weighted_quantile <- function(x, w, probs) {
#   wtd.quantile(x, weights = w, probs = probs, na.rm = TRUE)
# }

# Compute percentiles for each bin
percentile_data <- Rep_rates_df_subset_filtered[, .(
  p25 = weighted_quantile(net_RR, SIHPSWT, 0.25),
  p50 = weighted_quantile(net_RR, SIHPSWT, 0.50),
  p75 = weighted_quantile(net_RR, SIHPSWT, 0.75)
), by = .(partnered, income_bin, interaction_cat)]

# Generate a color palette dynamically based on the number of categories
unique_categories <- unique(percentile_data$interaction_cat)
num_categories <- length(unique_categories)
color_palette <- RColorBrewer::brewer.pal(min(num_categories, 8), "Dark2")  # Adjust color count dynamically

# Plot
ggplot(percentile_data, aes(x = income_bin, group = interaction_cat, color = interaction_cat)) +
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = interaction_cat), alpha = 0.2, color = NA) +
  geom_line(aes(y = p50), size = 1) +
  labs(title = "Net Replacement Rate by Income Bin and Household Type",
       x = "Income Bin",
       y = "Net Replacement Rate",
       color = "Household Type",
       fill = "Household Type") +
  facet_wrap(~partnered, labeller = as_labeller(c("0" = "Single", "1" = "Partnered"))) +
  theme_minimal() +
  scale_color_manual(values = color_palette) +  # Dynamically assigned colors
  scale_fill_manual(values = color_palette) + xlim(0, 3000) # Lighter shades for ribbon

# Load necessary libraries
library(ggplot2)
library(data.table)

# Convert dataframe to data.table
setDT(Rep_rates_df_subset)

# Convert `partnered` to a factor for correct grouping
Rep_rates_df_subset[, partnered := factor(partnered, levels = c(0, 1), labels = c("Single", "Partnered"))]

# Plot
plot2 <- ggplot(Rep_rates_df_subset, aes(x = Numb_dep_cat, y = net_RR * 100, fill = partnered)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.6, outlier.shape = NA) +
  labs_e61(subtitle = "Replacement Rates by Family Status",
           x = "Number of Dependent Children",
           y = "%",
           fill = "Partnered Status",
           color = "Partnered Status", 
           sources = c("e61", "ABS")) + plot_label(c("Single", "Partnered"), c(0.85,1.25), c(55, 22), size = 3) + 
  scale_y_continuous_e61(limits = c(0, 100, 25))


save_e61("Boxplotfamily.pdf", plot2)


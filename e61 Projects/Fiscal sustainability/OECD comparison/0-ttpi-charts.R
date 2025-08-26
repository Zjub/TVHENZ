# -------------------------------------------------------------------------
# title: Recreate TTPI charts
# Author: Jack Buckley
# Date: 06/06/2025
# -------------------------------------------------------------------------

# install and load packages 
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  data.table,
  magrittr,
  janitor,
  ggrepel,
  theme61,
  readxl,
  devtools
)

rm(list = ls()); gc()


# Read in the TTPI data ---------------------------------------------------

#ttpi_tax_paid <- read_excel("./Inputs/TTPI data/chart data 2.1.xlsx") %>% clean_names()
ttpi_tax_paid <- read_excel("chart data 2.1.xlsx") %>% clean_names()
ttpi_tax_paid %<>% rename("age" = x, "tax" = y)

#ttpi_gov_services <- read_excel("./Inputs/TTPI data/chart data 2.3.xlsx") %>% clean_names()
ttpi_gov_services <- read_excel("chart data 2.3.xlsx") %>% clean_names()
ttpi_gov_services %<>% rename("age" = x, "transfer" = y)

#ttpi_income <- read_excel("./Inputs/TTPI data/chart data 3.1.xlsx") %>% clean_names()
ttpi_income <- read_excel("chart data 3.1.xlsx") %>% clean_names()
ttpi_income %<>% rename("age" = x, "incm" = y)

# Create transfers net of gov services
tot_tax <- ttpi_tax_paid %>% 
  group_by(age, group) %>% 
  summarise(tax = sum(tax))

tot_transfers <- ttpi_gov_services %>% 
  group_by(age, group) %>% 
  summarise(transfer = sum(transfer))

tot_incm <- ttpi_income %>% 
  group_by(age, group) %>% 
  summarise(incm = sum(incm))

ttpi_cmbd_tot <- left_join(tot_tax, tot_transfers) %>% left_join(tot_incm) %>% filter(age >= 20)


# Figure 1 - transfers minus taxes ----------------------------------------

ttpi_cmbd_tot %<>% mutate(trans_minus_tax = transfer - tax)

fig1_data <- ttpi_cmbd_tot %>% filter(str_detect(group, "1993|2023"))

(fig1 <- ggplot(fig1_data, aes(x = age, y = trans_minus_tax / 1e3, col = group)) +
  add_baseline() +
  geom_line() + 
  plot_label(
    label = "Points above 0 indicate the individuals on average\nreceive more in government transfers than they\npay in taxes.",
    x = 20, 
    y = 52,
    size = 2.3,
    colour = e61_greylight
  ) + 
  plot_label(
    label = c("2019-23", "1994-98"),
    colour = c(e61_tealdark, e61_skylight),
    x = c(80, 97),
    y = c(45, 35),
    hjust = 1
  ) +
  scale_y_continuous_e61(limits = c(-20, 60, 20)) + 
  scale_x_continuous_e61(limits = c(20, 100, 15), hide_first_last = FALSE) +
  labs_e61(
    title = "Transfers minus taxes by age",
    subtitle = "Average per adult aged 20-100; $2023",
    x = "Age",
    y = "$000s",
    sources = "Varela, Breunig and Smith (2025)",
    footnotes = paste0(
      "Taxes include personal income tax, payroll taxes, consumption taxes, housing taxes, ",
      "superannuation taxes and other taxes such as corporate income tax. ",
      "Transfers include both cash transfers, such as the aged pensionm, and ",
      "in-kind transfers, such as education, aged care and health care. All items are ",
      "calculated on a per adult basis. Transfers made to children, such as ",
      "public education, are assigned as a transfer to their parents."
    )
  ))

save_e61(
  fig1, 
  filename = "./Outputs/ttpi_intergenetational.svg"
  # chart_type = "wide",
  # dim = list(width = 10, height = 15)
)


# Read in the full data ---------------------------------------------------

#ttpi_full_data <- read_excel("./Inputs/TTPI data/main table results.xlsx") %>% clean_names()
ttpi_full_data <- read_excel("TTPI main table results.xlsx") %>% clean_names()

# Fact 1 - Income growth for 18-35 year old Australians between 1993 and 2021
young_income_growth <- ttpi_full_data %>% 
  filter(
    age %between% c(18, 35), 
    grouped_year %in% c("1993/1994-1997/1998", "2018/2019-2022/2023")
  )

young_income_growth %<>%
  group_by(grouped_year) %>% 
  summarise(mean_hs_inc = weighted.mean(haig_simon_income, w = population))

ttpi_full_data$haig_simon_income

# Income of over 60s vs under 30s
young_income_growth <- ttpi_full_data %>% 
  filter(grouped_year %in% c("1993/1994-1997/1998", "2018/2019-2022/2023"))

young_income_growth %<>%
  mutate(
    age_group = case_when(
      age %between% c(18, 30) ~ "18-30",
      age > 60 ~ "60+"
    )
  )

young_income_growth %<>%
  filter(!is.na(age_group)) %>% 
  group_by(grouped_year, age_group) %>% 
  summarise(
    mean_hs_inc = weighted.mean(haig_simon_income, w = population),
    mean_sum_tax = weighted.mean(sum_tax, w = population),
    mean_sum_transfer = weighted.mean(sum_transfers, w = population)
  )

young_income_growth %<>% mutate(total = mean_hs_inc + mean_sum_tax + mean_sum_transfer)

# Share of income from new sources
ttpi_full_data %<>% 
  mutate(
    new_income_share = (real_capital_gain_investment_property + real_capital_gain_owner_occupied + real_superannuation_earnings + real_net_imputed_rent) / haig_simon_income,
    labour_income_share = real_labour_income / haig_simon_income,
    year_broad = case_when(
      grouped_year %in% c("1993/1994-1997/1998", "1998/1999-2002/2003") ~ "1993-2003",
      grouped_year %in% c("2013/2014-2017/2018", "2018/2019-2022/2023") ~ "2013-2023"
    )
  )

income_shares <- ttpi_full_data %<>% 
  filter(age >= 15, !is.na(year_broad)) %>% 
  group_by(year_broad) %>% 
  summarise(
    share_new = weighted.mean(new_income_share, w = population),
    share_labour = weighted.mean(labour_income_share, w = population)
  )

# -------------------------------------------------------------------------
# title: Detailed ABS data
# Author: Jack Buckley
# Date: 17/07/2025
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


# Read in GFS data --------------------------------------------------------

# Expenses data
gfs_expenses <- 
  read_excel("./Inputs/GFS data to e61.xlsx", sheet = "Expenses", skip = 12) %>% clean_names()

gfs_expenses %<>%
  pivot_longer(
    cols = all_of(matches("x\\d{4}")),
    names_to = "fin_year",
    values_to = "gov_expenses_mn"
  )

gfs_expenses %<>% 
  mutate(
    fin_year = paste0(str_extract(fin_year, "(?<=x)\\d{2}"), str_extract(fin_year, "\\d{2,4}$")),
    fin_year = fifelse(nchar(fin_year) == 6, str_extract(fin_year, "\\d{4}$"), fin_year)
  )

# NFA data
gfs_nfa <- 
  read_excel("./Inputs/GFS data to e61.xlsx", sheet = "Transactions in NFA", skip = 12) %>% clean_names()

gfs_nfa %<>%
  pivot_longer(
    cols = all_of(matches("x\\d{4}")),
    names_to = "fin_year",
    values_to = "gov_nfa_mn"
  )

gfs_nfa %<>% 
  mutate(
    fin_year = paste0(str_extract(fin_year, "(?<=x)\\d{2}"), str_extract(fin_year, "\\d{2,4}$")),
    fin_year = fifelse(nchar(fin_year) == 6, str_extract(fin_year, "\\d{4}$"), fin_year)
  )


# Join and add further details --------------------------------------------

# Join and add further details - note the ETFs are completely separate for the two types of transactions
gfs_data <- full_join(gfs_expenses, gfs_nfa)

gfs_data %<>% 
  rename(
    cofog_group_code = cofog, 
    cofog_group_name = cofog_desc,
    etf_subclass_code = etf,
    etf_subclass_name = etf_desc
  ) %>% 
  mutate(etf_subclass_code = as.character(etf_subclass_code))

# Add COFOG division names
cofog_div <- read_csv("./Inputs/Table A1C.1 - Broad classification of the COFOG-A.csv", skip = 1) %>% clean_names()

cofog_div %<>% rename(cofog_div_code = cofog_a, cofog_div_name = descriptor)

gfs_data %<>% mutate(cofog_div_code = str_extract(cofog_group_code, "^.."))

gfs_data %<>% left_join(cofog_div, by = join_by(cofog_div_code))

# Add ETF group and class codes
etf_group <- read_csv("./Inputs/Table A1A.5 - Economic Type Framework.csv", skip = 1) %>% clean_names()

etf_group %<>%
  mutate(
    etf_type_code = fifelse(str_detect(classification_codes, "^ETF \\d$"), str_extract(classification_codes, "\\d"), NA_character_),
    etf_type_name = fifelse(str_detect(classification_codes, "^ETF \\d$"), str_to_sentence(descriptor), NA_character_),
    etf_group_code = fifelse(str_detect(classification_codes, "^ETF \\d{2}$"), str_extract(classification_codes, "\\d{2}"), NA_character_),
    etf_group_name = fifelse(str_detect(classification_codes, "^ETF \\d{2}$"), descriptor, NA_character_),
    etf_class_code = fifelse(str_detect(classification_codes, "^ETF \\d{3}$"), str_extract(classification_codes, "\\d{3}"), NA_character_),
    etf_class_name = fifelse(str_detect(classification_codes, "^ETF \\d{3}$"), descriptor, NA_character_),
    etf_subclass_code = fifelse(str_detect(classification_codes, "^ETF \\d{4}"), str_extract(classification_codes, "\\d{4}"), NA_character_)
  )

etf_group %<>% fill(etf_type_code, etf_type_name, etf_group_code, etf_group_name, etf_class_code, etf_class_name)

etf_group %<>% drop_na(etf_subclass_code) %>% select(-c(descriptor, classification_codes))

gfs_data %<>% left_join(etf_group)


# Add expenditure in real values ------------------------------------------

# CPI data to get real spending
cpi_data <- read_excel("./Inputs/640101.xlsx", skip = 9, sheet = "Data1") %>% clean_names()

cpi_data %<>% mutate(fin_year = as.character(year(series_id)))

cpi_data %<>%
  group_by(fin_year) %>% 
  summarise(cpi_index = mean(a2325846c))

gfs_data %<>% left_join(cpi_data, by = join_by(fin_year))

gfs_data %<>% 
  mutate(
    gov_expenses_mn_real_1112 = gov_expenses_mn * 100 / cpi_index,
    gov_nfa_mn_real_1112 = gov_nfa_mn * 100 / cpi_index
  )


# Add GDP data ------------------------------------------------------------

# Use GDP, current prices original series
gdp_current_prices <- 
  read_excel("./Inputs/5206003_Expenditure_Current_Price.xlsx", sheet = "Data1", skip = 9) %>% 
  clean_names()

gdp_current_prices %<>% mutate(fin_year = as.character(year(series_id)))

gdp_current_prices %<>%
  group_by(fin_year) %>% 
  summarise(gdp_mn = sum(a2302467a))

gfs_data %<>% left_join(gdp_current_prices, by = join_by(fin_year))

gfs_data %<>% 
  mutate(
    gov_expenses_pct_gdp = 100 * gov_expenses_mn / gdp_mn,
    gov_nfa_pct_gdp = 100 * gov_nfa_mn / gdp_mn
  )

gfs_data %<>% select(-gdp_mn)

gfs_data %<>% mutate(fin_year = as.numeric(fin_year))


# Initial checks ----------------------------------------------------------

# Total gov expenses as a share of GDP
test_share <- gfs_data %>% 
  group_by(fin_year) %>% 
  summarise(
    gov_exp_share_gdp = sum(gov_expenses_pct_gdp, na.rm = T),
    gov_tot_share_gdp = sum(gov_expenses_pct_gdp, na.rm = T) + sum(gov_nfa_pct_gdp, na.rm = T),
    tot_expenses_mn = sum(gov_expenses_mn, na.rm = T)
  )


# Arrange and save --------------------------------------------------------

gfs_data %<>% 
  select(
    sector, sector_desc, matches("etf_type"), matches("etf_group"), matches("etf_class"), matches("etf_"), 
    matches("cofog_div"), matches("cofog_group"), fin_year, matches("gov_expenses"), matches("gov_nfa")
  )

write_csv(gfs_data, "./Outputs/abs_gfs_data_clean.csv")

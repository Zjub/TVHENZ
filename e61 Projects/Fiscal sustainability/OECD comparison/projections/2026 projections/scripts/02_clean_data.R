source(file.path("scripts", "00_config.R"))
check_packages(c("data.table", "readxl"))

validation_file <- file.path(table_dir, "data_validation.csv")
if (file.exists(validation_file)) file.remove(validation_file)

na_hist <- extract_abs_national_accounts(read_abs_rds(raw_dir, "5206.0"))
age_hist <- extract_abs_age_shares(read_abs_rds(raw_dir, "3101.0"))
tot_hist <- extract_abs_terms_of_trade(read_abs_rds(raw_dir, "5302.0"))
unemp_hist <- extract_lf_unemployment_xlsx(file.path(raw_dir, "abs_labour_force_may_2026_table1.xlsx"))

historical <- Reduce(
  function(x, y) merge(x, y, by = "year", all = FALSE),
  list(na_hist, age_hist, tot_hist, unemp_hist)
)
historical <- historical[year >= estimation_start]
setorder(historical, year)

historical[, relative_gov_price := gov_consumption_price / gdp_price]
standard_sample <- historical[year <= 2019]
tot_center <- mean(standard_sample$tot_index, na.rm = TRUE)
tot_scale <- sd(standard_sample$tot_index, na.rm = TRUE)
rp_center <- mean(standard_sample$relative_gov_price, na.rm = TRUE)
rp_scale <- sd(standard_sample$relative_gov_price, na.rm = TRUE)

historical[, `:=`(
  tot_z = zscore_with(tot_index, tot_center, tot_scale),
  rp_z = zscore_with(relative_gov_price, rp_center, rp_scale),
  covid = as.integer(year %in% 2020:2022),
  log_population = log(pop_total),
  log_real_gdp_per_capita = log(gdp_real / pop_total),
  population_growth = c(NA_real_, diff(log(pop_total))),
  real_gdp_per_capita_growth = c(NA_real_, diff(log(gdp_real / pop_total))),
  source = "ABS national accounts and labour/demographic data"
)]

scaling <- data.table(
  variable = c("tot_index", "relative_gov_price"),
  center = c(tot_center, rp_center),
  scale = c(tot_scale, rp_scale),
  standardisation_sample = "1980-2019"
)

gfs_path <- file.path(raw_dir, "abs_gfs_annual_2024_25.xlsx")
gfs_operating <- read_gfs_sheet_long(gfs_path, "Table_1")
gfs_purpose <- read_gfs_sheet_long(gfs_path, "Table_4")
gfs_purpose <- gfs_purpose[!grepl("Total Expenses", item, ignore.case = TRUE)]

gdp_fy <- na_hist[, .(year, gdp_nom)]
gfs_purpose <- merge(gfs_purpose, gdp_fy, by = "year", all.x = TRUE)
gfs_purpose[, share_gdp := value / gdp_nom]

gfs_aggregates <- dcast(
  gfs_operating[
    item %in% c(
      "Taxation revenue", "Total GFS revenue", "Total GFS expenses",
      "Interest expenses n.e.c.", "GFS Net operating balance",
      "Total net acquisition of non-financial assets", "GFS Net Lending(+)/Borrowing(-)"
    )
  ],
  year ~ item,
  value.var = "value"
)
setnames(gfs_aggregates, janitor_names(names(gfs_aggregates)))
gfs_aggregates <- merge(gfs_aggregates, gdp_fy, by = "year", all.x = TRUE)
for (col in setdiff(names(gfs_aggregates), c("year", "gdp_nom"))) {
  gfs_aggregates[, paste0(col, "_gdp") := get(col) / gdp_nom]
}

population_projection_raw <- parse_population_projection(
  file.path(raw_dir, "population_statement_2025_age_sex.xlsx")
)
population_projection <- interpolate_projection(
  population_projection_raw,
  seq(min(population_projection_raw$year), projection_end)
)
age_cols <- c("0_14", "15_34", "35_54", "55_64", "65p")
population_projection[, age_sum := rowSums(.SD), .SDcols = age_cols]
population_projection[, (age_cols) := lapply(.SD, function(x) x / age_sum), .SDcols = age_cols]
population_projection[, age_sum := NULL]

fwrite(historical, file.path(processed_dir, "historical_top_down_model_data.csv"))
fwrite(na_hist, file.path(processed_dir, "historical_national_accounts_fiscal.csv"))
fwrite(age_hist, file.path(processed_dir, "historical_age_shares.csv"))
fwrite(gfs_operating, file.path(processed_dir, "historical_gfs_operating_statement.csv"))
fwrite(gfs_purpose, file.path(processed_dir, "historical_gfs_expenses_by_purpose.csv"))
fwrite(gfs_aggregates, file.path(processed_dir, "historical_gfs_aggregates.csv"))
fwrite(population_projection, file.path(processed_dir, "population_projection_age_shares.csv"))
fwrite(population_projection_raw, file.path(processed_dir, "population_projection_published_points.csv"))
fwrite(scaling, file.path(processed_dir, "standardisation_parameters.csv"))

write_validation(
  "Historical population not doubled",
  historical[year == max(year), pop_total] < 35e6,
  paste("Latest population:", round(historical[year == max(year), pop_total] / 1e6, 2), "million"),
  validation_file
)
write_validation(
  "Historical age shares sum to one",
  max(abs(rowSums(historical[, ..age_cols]) - 1), na.rm = TRUE) < 0.001,
  paste("Maximum absolute deviation:", signif(max(abs(rowSums(historical[, ..age_cols]) - 1)), 3)),
  validation_file
)
write_validation(
  "Projection age shares sum to one",
  max(abs(rowSums(population_projection[, ..age_cols]) - 1), na.rm = TRUE) < 0.001,
  paste("Maximum absolute deviation:", signif(max(abs(rowSums(population_projection[, ..age_cols]) - 1)), 3)),
  validation_file
)
write_validation(
  "No partial financial years in national accounts model data",
  max(historical$year) <= 2025,
  paste("Latest complete merged financial year:", max(historical$year)),
  validation_file
)
write_validation(
  "National Accounts interest components reconcile",
  max(abs(
    historical$interest_payable_total_nom -
      historical$interest_payable_other_nom -
      historical$interest_payable_unfunded_super_nom
  ), na.rm = TRUE) <= 4,
  paste(
    "Maximum component gap ($m):",
    round(max(abs(
      historical$interest_payable_total_nom -
        historical$interest_payable_other_nom -
        historical$interest_payable_unfunded_super_nom
    ), na.rm = TRUE), 2)
  ),
  validation_file
)
write_validation(
  "Configured top-down interest treatment applied",
  identical(unique(historical$topdown_interest_treatment), topdown_interest_treatment),
  paste("Treatment:", topdown_interest_treatment_label()),
  validation_file
)
write_validation(
  "GFS purpose categories add to total expenses",
  abs(gfs_purpose[year == 2025, sum(value)] - gfs_operating[year == 2025 & item == "Total GFS expenses", value]) < 2,
  paste("Difference ($m):", round(gfs_purpose[year == 2025, sum(value)] - gfs_operating[year == 2025 & item == "Total GFS expenses", value], 2)),
  validation_file
)

message("Cleaned datasets written to: ", processed_dir)

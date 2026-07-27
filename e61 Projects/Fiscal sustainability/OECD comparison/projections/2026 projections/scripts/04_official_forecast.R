source(file.path("scripts", "00_config.R"))

pbo_path <- file.path(raw_dir, "pbo_national_fiscal_outlook_2025_26.xlsx")
pbo_ratio <- as.data.frame(readxl::read_excel(pbo_path, sheet = "B2", col_names = FALSE))
pbo_dollar <- as.data.frame(readxl::read_excel(pbo_path, sheet = "B1", col_names = FALSE))

metric_map <- data.table(
  metric = c("net_operating_balance", "fiscal_balance", "net_capital_investment", "revenue", "expenses", "net_debt", "gross_debt", "public_debt_interest"),
  ratio_label = c(
    "Net operating balance (%GDP/GSP)", "Fiscal balance (%GDP/GSP)",
    "Net capital investment (%GDP/GSP)", "Revenue (%GDP/GSP)",
    "Expenses (%GDP/GSP)", "Net debt (%GDP/GSP)", "Gross debt (%GDP/GSP)",
    "Public debt interest (%GDP/GSP)"
  ),
  dollar_label = c(
    "Net operating balance ($b)", "Fiscal balance ($b)", "Net capital investment ($b)",
    "Revenue ($b)", "Expenses ($b)", "Net debt ($b)", "Gross debt ($b)",
    "Public debt interest ($b)"
  )
)

ratios <- rbindlist(lapply(seq_len(nrow(metric_map)), function(i) {
  x <- extract_pbo_metric(pbo_ratio, metric_map$ratio_label[i])
  x[, `:=`(metric = metric_map$metric[i], unit = "ratio_gdp", value = value / 100)]
  x
}))
dollars <- rbindlist(lapply(seq_len(nrow(metric_map)), function(i) {
  x <- extract_pbo_metric(pbo_dollar, metric_map$dollar_label[i])
  x[, `:=`(metric = metric_map$metric[i], unit = "billion_dollars")]
  x
}))

pbo_long <- rbind(ratios, dollars, use.names = TRUE)
pbo_long[, status := ifelse(year <= 2025, "Historical/estimate", "Official forecast")]
pbo_wide <- dcast(pbo_long, year + status ~ metric + unit, value.var = "value")

if (all(c("expenses_billion_dollars", "expenses_ratio_gdp") %in% names(pbo_wide))) {
  pbo_wide[, nominal_gdp_billion := expenses_billion_dollars / expenses_ratio_gdp]
}

budget_receipts <- fread(file.path(raw_dir, "budget_2026_27_cash_receipts.csv"), header = FALSE, fill = TRUE)
budget_receipts_gdp <- fread(file.path(raw_dir, "budget_2026_27_receipts_gdp.csv"), header = FALSE, fill = TRUE)
fwrite(budget_receipts, file.path(processed_dir, "budget_2026_27_cash_receipts_raw_table.csv"))
fwrite(budget_receipts_gdp, file.path(processed_dir, "budget_2026_27_receipts_gdp_raw_table.csv"))

fwrite(pbo_long, file.path(processed_dir, "official_pbo_nfo_long.csv"))
fwrite(pbo_wide, file.path(processed_dir, "official_pbo_nfo_wide.csv"))
fwrite(pbo_wide[year >= 2025], file.path(table_dir, "official_forecast_anchor.csv"))

message("Official consolidated forecast anchor written through ", max(pbo_wide$year), ".")

# =====================================================================
# Australia: CPI vs Government Consumption Deflator vs Nominal GDP
# =====================================================================

library(data.table)
library(readabs)
library(ggplot2)
library(lubridate)
library(scales)

# -------------------- CPI --------------------
cpi_raw <- read_abs("6401.0")
setDT(cpi_raw)
cpi <- cpi_raw[
  grepl("^Index Numbers ;  All groups CPI ;  Australia ;", series, ignore.case = TRUE),
  .(date, CPI = value)
][, date := as.Date(date)]

# -------------------- Govt consumption deflator --------------------
na_raw <- read_abs("5206.0")
setDT(na_raw)

govdef <- na_raw[
  grepl("General government", series, ignore.case = TRUE) &
    grepl("Final consumption expenditure: Implicit price deflators", series, ignore.case = TRUE),
  .(date, GovDef = value)
][, date := as.Date(date)]

# -------------------- Nominal GDP --------------------
gdp_nom <- na_raw[
  grepl("^Gross domestic product: Current prices ;", series, ignore.case = TRUE) &
    grepl("5206001_key_aggregates", table_no, ignore.case = TRUE) &
    grepl("Original", series_type, ignore.case = TRUE),
  .(date, GDP_nom = value)
][, date := as.Date(date)]

# -------------------- Combine and trim to last 25 years --------------------
dt <- Reduce(function(x, y) merge(x, y, by = "date", all = FALSE),
             list(cpi, govdef, gdp_nom))[order(date)]
cutoff <- max(dt$date) %m-% years(25)
dt <- dt[date >= cutoff]

# -------------------- Re-index to 100 at window start --------------------
dt[, `:=`(
  CPI_idx = 100 * CPI / CPI[1],
  GovDef_idx = 100 * GovDef / GovDef[1],
  GDP_nom_idx = 100 * GDP_nom / GDP_nom[1]
)]

# -------------------- Summary (quarterly) --------------------
years_range <- as.numeric((max(dt$date) - min(dt$date)) / 365.25)
summary_qtr <- data.table(
  Series = c("Gov’t consumption deflator", "CPI (All groups)", "Nominal GDP"),
  Total_change = round(c(
    dt$GovDef_idx[.N]/dt$GovDef_idx[1] - 1,
    dt$CPI_idx[.N]/dt$CPI_idx[1] - 1,
    dt$GDP_nom_idx[.N]/dt$GDP_nom_idx[1] - 1
  ) * 100, 1),
  CAGR = round(c(
    (dt$GovDef_idx[.N]/dt$GovDef_idx[1])^(1/years_range) - 1,
    (dt$CPI_idx[.N]/dt$CPI_idx[1])^(1/years_range) - 1,
    (dt$GDP_nom_idx[.N]/dt$GDP_nom_idx[1])^(1/years_range) - 1
  ) * 100, 2)
)
print(summary_qtr)

# -------------------- Quarterly Plot --------------------
plot_qtr <- melt(dt, id.vars = "date",
                 measure.vars = c("GovDef_idx", "CPI_idx", "GDP_nom_idx"),
                 variable.name = "Series", value.name = "Index")

p1 <- ggplot(plot_qtr, aes(x = date, y = Index, color = Series)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c("#1B9E77", "#D95F02", "#7570B3"),
    labels = c("Gov’t consumption deflator", "CPI", "Nominal GDP")
  ) +
  scale_y_continuous("Index (base = 100)") +
  scale_x_date(NULL, date_breaks = "2 years", date_labels = "%Y") +
  ggtitle("Australia: Gov’t Consumption Deflator, CPI, and Nominal GDP (Quarterly, last 25 years)") +
  theme_minimal(base_size = 13) +
  theme(legend.title = element_blank(), legend.position = "bottom")

print(p1)

# -------------------- Annual version --------------------
annual <- copy(dt)[, year := year(date)]

# Average within each calendar year
annual_avg <- annual[, .(
  CPI = mean(CPI, na.rm = TRUE),
  GovDef = mean(GovDef, na.rm = TRUE),
  GDP_nom = mean(GDP_nom, na.rm = TRUE)
), by = year][order(year)]

# Re-index
annual_avg[, `:=`(
  CPI_idx = 100 * CPI / CPI[1],
  GovDef_idx = 100 * GovDef / GovDef[1],
  GDP_nom_idx = 100 * GDP_nom / GDP_nom[1]
)]

# Summary (annual)
years_range_ann <- annual_avg$year[.N] - annual_avg$year[1]
summary_ann <- data.table(
  Series = c("Gov’t consumption deflator (annual avg)",
             "CPI (annual avg)", "Nominal GDP (annual avg)"),
  Total_change = round(c(
    annual_avg$GovDef_idx[.N]/annual_avg$GovDef_idx[1] - 1,
    annual_avg$CPI_idx[.N]/annual_avg$CPI_idx[1] - 1,
    annual_avg$GDP_nom_idx[.N]/annual_avg$GDP_nom_idx[1] - 1
  ) * 100, 1),
  CAGR = round(c(
    (annual_avg$GovDef_idx[.N]/annual_avg$GovDef_idx[1])^(1/years_range_ann) - 1,
    (annual_avg$CPI_idx[.N]/annual_avg$CPI_idx[1])^(1/years_range_ann) - 1,
    (annual_avg$GDP_nom_idx[.N]/annual_avg$GDP_nom_idx[1])^(1/years_range_ann) - 1
  ) * 100, 2)
)
print(summary_ann)

# -------------------- Annual Plot --------------------
plot_ann <- melt(annual_avg, id.vars = "year",
                 measure.vars = c("GovDef_idx", "CPI_idx", "GDP_nom_idx"),
                 variable.name = "Series", value.name = "Index")

p2 <- ggplot(plot_ann, aes(x = year, y = Index, color = Series)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_color_manual(
    values = c("#1B9E77", "#D95F02", "#7570B3"),
    labels = c("Gov’t consumption deflator (annual)", "CPI (annual)", "Nominal GDP (annual)")
  ) +
  scale_y_continuous("Index (base = 100)") +
  scale_x_continuous(breaks = pretty(annual_avg$year)) +
  ggtitle("Australia: Gov’t Consumption Deflator, CPI, and Nominal GDP (Annual averages)") +
  theme_minimal(base_size = 13) +
  theme(legend.title = element_blank(), legend.position = "bottom")

print(p2)
# =====================================================================


# ============================================================
# OECD SDMX: NZ rank over time (data.table) + RANK PLOT
#  - RNNDI per capita = B6N(total, PPP-volume USD) / Population
#  - NNI per capita   = B5N(per capita, PPP-volume USD)
#  - GDP per capita   = from MULT_INDICES, keep STRUCTURE_NAME containing "GDP"
#
# Ensures:
#  - SAME COUNTRY SET for all three (intersection)
#  - SAME countries each year (balanced panel years only)
#  - Prints country codes + count
#  - Plots NZ ranks over time
# ============================================================

suppressPackageStartupMessages({
  library(httr2)
  library(readr)
  library(data.table)
  library(stringr)
  library(ggplot2)
})

# ----------------------------
# 1) API calls
# ----------------------------
B6N_URL <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE2_B6_VPVOP,2.0/A.AUS+AUT+BEL+CAN+CHL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+GBR+USA...B6N.......?startPeriod=2000&dimensionAtObservation=AllDimensions"

POP_URL <- "https://sdmx.oecd.org/public/rest/data/OECD.ELS.SAE,DSD_POPULATION@DF_POP_HIST,1.0/.POP.PS._T..?startPeriod=2000&endPeriod=2024&dimensionAtObservation=AllDimensions"

B5N_PC_URL <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE2_B5N_HVPVOB,2.0/A.AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+GBR+USA..........?startPeriod=2000&dimensionAtObservation=AllDimensions"

GDP_PC_URL <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAAG@DF_NAAG_I,1.0/A.AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA.B1GQ_R_POP.USD_PPP_PS.?startPeriod=2000&dimensionAtObservation=AllDimensions"

# ----------------------------
# 2) Robust downloader -> data.table
# ----------------------------
oecd_get_csv_dt <- function(url) {
  if (!str_detect(url, "format=")) {
    sep <- ifelse(str_detect(url, "\\?"), "&", "?")
    url <- paste0(url, sep, "format=csvfilewithlabels")
  }
  message("GET: ", url)
  
  resp <- request(url) |>
    req_headers(
      "Accept" = "text/csv",
      "User-Agent" = "R (httr2); OECD SDMX download"
    ) |>
    req_perform()
  
  if (resp_status(resp) >= 400) {
    stop("HTTP error: ", resp_status(resp), "\n", resp_body_string(resp))
  }
  
  tmp <- tempfile(fileext = ".csv")
  writeBin(resp_body_raw(resp), tmp)
  as.data.table(readr::read_csv(tmp, show_col_types = FALSE, progress = FALSE))
}

std_dt <- function(dt, value_name) {
  out <- dt[, .(
    country = as.character(REF_AREA),
    year    = as.integer(TIME_PERIOD),
    value   = as.numeric(OBS_VALUE)
  )]
  setnames(out, "value", value_name)
  out
}

# ----------------------------
# 3) Download
# ----------------------------
b6n_raw <- oecd_get_csv_dt(B6N_URL)
pop_raw <- oecd_get_csv_dt(POP_URL)
b5n_raw <- oecd_get_csv_dt(B5N_PC_URL)
gdp_raw <- oecd_get_csv_dt(GDP_PC_URL)

# ----------------------------
# 4) Construct series
# ----------------------------

# B6N total
b6n <- copy(b6n_raw)
if ("TRANSACTION" %in% names(b6n)) b6n <- b6n[TRANSACTION == "B6N"]
b6n_std <- std_dt(b6n, "b6n_total")

# Population total (max per country-year)
pop_std <- std_dt(pop_raw, "population")[,
                                         .(population = max(population, na.rm = TRUE)),
                                         by = .(country, year)
]

# RNNDI per capita
setkey(b6n_std, country, year)
setkey(pop_std, country, year)
rnndi_pc <- b6n_std[pop_std, nomatch = 0][,
                                          .(country, year, rnndi_pc = b6n_total / population)
]

# NNI per capita (already)
nni_pc <- std_dt(b5n_raw, "nni_pc")[, .(country, year, nni_pc)]

# GDP per capita: keep only rows where STRUCTURE_NAME contains "GDP"
gdp <- copy(gdp_raw)
if (!("STRUCTURE_NAME" %in% names(gdp))) {
  stop("GDP dataset does not contain STRUCTURE_NAME; inspect names(gdp_raw).")
}
gdp <- gdp[str_detect(STRUCTURE_NAME, "GDP")]

# Collapse to single obs per country-year if needed
gdp_pc <- std_dt(gdp, "gdp_pc")[,
                                .(gdp_pc = max(gdp_pc, na.rm = TRUE)),
                                by = .(country, year)
]

# Drop OECD aggregate if present
gdp_pc <- gdp_pc[country != "OECD"]

# ----------------------------
# 5) SAME COUNTRY SET (intersection)
# ----------------------------
common_countries <- Reduce(intersect, list(
  unique(rnndi_pc$country),
  unique(nni_pc$country),
  unique(gdp_pc$country)
))
common_countries <- sort(common_countries)

rnndi_pc <- rnndi_pc[country %in% common_countries]
nni_pc   <- nni_pc[country %in% common_countries]
gdp_pc   <- gdp_pc[country %in% common_countries]

# ----------------------------
# 6) Merge and keep balanced panel years (same countries each year)
# ----------------------------
setkey(rnndi_pc, country, year)
setkey(nni_pc,   country, year)
setkey(gdp_pc,   country, year)

combo <- rnndi_pc[nni_pc, nomatch = 0]
combo <- combo[gdp_pc, nomatch = 0]

n_common <- length(common_countries)
yrs_full <- combo[, .(n = uniqueN(country)), by = year][n == n_common, year]
combo_full <- combo[year %in% yrs_full]

# ----------------------------
# 7) Ranks (1 = highest)
# ----------------------------
combo_full[, rank_rnndi := frank(-rnndi_pc, ties.method = "dense"), by = year]
combo_full[, rank_nni   := frank(-nni_pc,   ties.method = "dense"), by = year]
combo_full[, rank_gdp   := frank(-gdp_pc,   ties.method = "dense"), by = year]

nz_ranks <- combo_full[country == "NZL",
                       .(year, rank_gdp, rank_nni, rank_rnndi)
][order(year)]

print(nz_ranks)

# ----------------------------
# 8) Rank plot (NZ only)
# ----------------------------
nz_long <- melt(
  nz_ranks,
  id.vars = "year",
  variable.name = "series",
  value.name = "rank"
)

nz_long[, series := fifelse(series == "rank_gdp", "GDP per capita",
                            fifelse(series == "rank_nni", "NNI per capita (B5N)",
                                    "RNNDI per capita (B6N/pop)"))]

p_rank <- ggplot(nz_long, aes(x = year, y = rank, linetype = series)) +
  geom_line(linewidth = 1.1) +
  scale_y_reverse() +
  labs(
    x = NULL,
    y = "NZ rank (1 = highest)",
    title = "New Zealand rank over time (same country set each year)",
    subtitle = "GDP pc vs NNI pc vs RNNDI pc"
  ) +
  theme_bw()

print(p_rank)

# ----------------------------
# 9) Print country codes + count used
# ----------------------------
cat("\nNumber of countries used (intersection across all three series): ", n_common, "\n", sep = "")
cat("Countries used (ISO3 codes):\n")
print(common_countries)

# ----------------------------
# 10) Optional saves
# ----------------------------
fwrite(nz_ranks, "nz_ranks_over_time_same_country_set.csv")
fwrite(combo_full, "all_country_values_and_ranks_same_country_set.csv")
ggsave("nz_rank_over_time_same_country_set.png", p_rank, width = 9, height = 5, dpi = 300)


combo_full[year == 2022][order(rank_rnndi)]

# ----------------------------
# LEVEL COMPARISON PLOTS (NZ only)
# ----------------------------

# Keep NZ only and rescale GDP per capita
nz_levels <- combo_full[country == "NZL",
                        .(
                          year,
                          rnndi_pc_m = rnndi_pc *1000,                  # already PPP-volume USD per capita
                          nni_pc_m = nni_pc / 1000,                    # already PPP-volume USD per capita
                          gdp_pc_m = gdp_pc /1000
                        )
]

# Long format for plotting
nz_levels_long <- melt(
  nz_levels,
  id.vars = "year",
  variable.name = "series",
  value.name = "value"
)

# Clean labels
nz_levels_long[, series :=
                 fifelse(series == "rnndi_pc_m", "RNNDI per capita (B6N / pop)",
                         fifelse(series == "nni_pc_m",   "NNI per capita (B5N)",
                                 "GDP per capita (÷ 1 million)"))
]

# Plot
p_levels <- ggplot(nz_levels_long, aes(x = year, y = value, linetype = series)) +
  geom_line(linewidth = 1.1) +
  labs(
    x = NULL,
    y = "Level (PPP-volume US$)",
    title = "New Zealand: income and production measures per capita",
    subtitle = "XXX"
  ) +
  theme_bw()

print(p_levels)

# Optional save
ggsave("nz_levels_comparison_rnndi_nni_gdp.png",
       p_levels, width = 9, height = 5, dpi = 300)


# ============================================================
# ADD GDHI per capita (Gross Disposable Household Income)
# ============================================================

# 1) API call for GDHI per capita
GDHI_PC_URL <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAAG@DF_NAAG_V,1.0/A..B7GS1M_POP.USD_PPP_PS.?startPeriod=2000&dimensionAtObservation=AllDimensions"

# 2) Download via the same OECD SDMX helper
gdhi_raw <- oecd_get_csv_dt(GDHI_PC_URL)

# 3) Standardise: REF_AREA, TIME_PERIOD, OBS_VALUE
gdhi_pc <- std_dt(gdhi_raw, "gdhi_pc")

# 4) Restrict to common_countries
gdhi_pc <- gdhi_pc[country %in% common_countries]

# 5) Update combo_full build to include gdhi_pc
setkey(rnndi_pc, country, year)
setkey(nni_pc,   country, year)
setkey(gdp_pc,   country, year)
setkey(gdhi_pc,  country, year)

combo <- rnndi_pc[nni_pc, nomatch = 0]
combo <- combo[gdp_pc,   nomatch = 0]
combo <- combo[gdhi_pc,  nomatch = 0]   # join with GDHI

# Now recompute the balanced panel years
n_common <- length(common_countries)
yrs_full <- combo[, .(n = uniqueN(country)), by = year][n == n_common, year]
combo_full <- combo # the year balancing is an issue for this series

# 6) Recompute ranks for all 4 series
combo_full[, rank_rnndi := frank(-rnndi_pc, ties.method = "dense"), by = year]
combo_full[, rank_nni   := frank(-nni_pc,   ties.method = "dense"), by = year]
combo_full[, rank_gdp   := frank(-gdp_pc,   ties.method = "dense"), by = year]
combo_full[, rank_gdhi  := frank(-gdhi_pc,  ties.method = "dense"), by = year]

# 7) Extract NZ ranks including GDHI
nz_ranks <- combo_full[country == "NZL",
                       .(year, rank_gdp, rank_nni, rank_rnndi, rank_gdhi)
][order(year)]

print(nz_ranks)

# 8) Optional: plot NZ ranks over time (now with GDHI)
nz_long <- melt(
  nz_ranks,
  id.vars = "year",
  variable.name = "series",
  value.name = "rank"
)

nz_long[, series := fcase(
  series == "rank_gdp",  "GDP per capita",
  series == "rank_nni",  "NNI per capita (B5N)",
  series == "rank_rnndi","RNNDI per capita (B6N/pop)",
  series == "rank_gdhi", "GDHI per capita (B7GS1M)"
)]

p_rank_all <- ggplot(nz_long, aes(x = year, y = rank, linetype = series, colour = series)) +
  geom_line(linewidth = 1.1) +
  scale_y_reverse() +
  labs(
    x = NULL,
    y = "NZ rank (1 = highest)",
    title = "New Zealand rank over time (same country set)",
    subtitle = "Including Gross Disposable Household Income per capita"
  ) +
  theme_bw()

print(p_rank_all)

# Optional: save
ggsave("nz_rank_with_gdhi.png", p_rank_all, width = 9, height = 5, dpi = 300)


# ------------------------------------------------------------
# Flexible read-across table with ratios
#   Rows: year x measure
#   Cols: selected countries (+ optional ratio cols)
# ------------------------------------------------------------

unique(combo_full$country)

# --- user controls ---
countries   <- c("NZL", "AUS", "USA","JPN","DEU")   # change this freely
ref_country <- "NZL"                   # ratios will be ref / other
measures    <- c("gdp_pc", "nni_pc", "rnndi_pc", "gdhi_pc")

# --- subset ---
dt_sub <- combo_full[country %in% countries]

# --- long ---
dt_long <- melt(
  dt_sub,
  id.vars = c("country", "year"),
  measure.vars = measures,
  variable.name = "measure",
  value.name = "value"
)

# nicer labels (optional)
dt_long[, measure := fcase(
  measure == "gdp_pc",   "GDP per capita",
  measure == "nni_pc",   "NNI per capita",
  measure == "rnndi_pc", "RNNDI per capita",
  measure == "gdhi_pc",  "GDHI per capita",
  default = measure
)]

# --- wide (countries as columns) ---
read_across <- dcast(
  dt_long,
  year + measure ~ country,
  value.var = "value"
)

# order columns: year, measure, then countries
setcolorder(read_across, c("year", "measure", intersect(countries, names(read_across))))

# --- add ratio columns: ref / each other country ---
other_countries <- setdiff(countries, ref_country)

# only compute ratios if the relevant columns exist
for (cc in other_countries) {
  if (ref_country %in% names(read_across) && cc %in% names(read_across)) {
    newcol <- paste0(ref_country, "_over_", cc)
    read_across[, (newcol) := get(ref_country) / get(cc)]
  }
}

# sort for readability
setorder(read_across, measure, year)

# view
print(read_across[measure == "RNNDI per capita"])
print(read_across[measure == "GDP per capita"])
print(read_across[measure == "GDHI per capita"])

(read_across[measure == "RNNDI per capita" & year %in% c(2022)]$NZL/read_across[measure == "RNNDI per capita" & year %in% c(2000)]$NZL)^(1/22)-1
(read_across[measure == "GDP per capita" & year %in% c(2022)]$NZL/read_across[measure == "GDP per capita" & year %in% c(2000)]$NZL)^(1/22)-1
(read_across[measure == "GDHI per capita" & year %in% c(2022)]$NZL/read_across[measure == "GDHI per capita" & year %in% c(2000)]$NZL)^(1/22)-1 # This is in USD and PPP adjusted, but is "nominal" - so the growth is not comparable.


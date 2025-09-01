# Topic: Use TTPI data to start thinking about cohorts formally
# Author: Matt Nolan
# Created: 29/8/2025
# Last edit: 29/8/2025
# Last editor: Matt Nolan

library(dplyr)
library(ggplot2)
library(theme61)
library(readr)
library(readxl)
library(tidyverse)
library(data.table)
library(Hmisc)
library(tidysynth)
library(readabs)

dt <- read_excel("TTPI main table results.xlsx")
setDT(dt)

dt[,net_trans := sum_transfers - sum_tax]

# Take 20 year old in 1993 and trace it through

ggplot(dt[age == year -1973],aes(x=year,y=net_trans)) + geom_line()

ggplot(dt[age == year -1973],aes(x=year,y=Haig_simon_income)) + geom_line()

ggplot(dt[age == year -1973],aes(x=year,y=(Haig_simon_income+net_trans))) + geom_line()

# What about a 25 year old

ggplot(dt[age == year -1968],aes(x=year,y=net_trans)) + geom_line()

ggplot(dt[age == year -1968],aes(x=year,y=Haig_simon_income)) + geom_line()

ggplot(dt[age == year -1968],aes(x=year,y=(Haig_simon_income+net_trans))) + geom_line()

# Ok something wild is happening in COVID - lets cut all this data to be up to 2019.
# 
# birth_years <- seq(1943, 1988, 5)
# year_end <- 2020
# 
# smooth_list <- lapply(birth_years, function(by) {
#   d <- dt[year <= year_end & (year - age) == by,
#           .(year, net_hs = (Haig_simon_income + net_trans) / 1000)][order(year)]
#   print(d)
#   if (nrow(d) >= 5) {
#     fit <- loess(net_hs ~ year, data = d, span = 0.3, degree = 1,
#                  na.action = na.exclude, surface = "direct")
#     print(fit)
#     d[, net_hs_smooth := as.numeric(predict(fit, newdata = data.frame(year = year)))]
#   } else {
#     d[, net_hs_smooth := NA_real_]
#   }
#   d[, cohort := by]
#   d[]
# })
# 
# smooth_dt <- rbindlist(smooth_list, use.names = TRUE, fill = TRUE)
# 
# for (birth_year in birth_years) {
#   cohort <- smooth_dt[cohort == birth_year]
#   cohort_plot <- ggplot(cohort, aes(x = year, y = net_hs)) +
#     geom_line() +
#     geom_line(aes(y = net_hs_smooth), linewidth = 1) +
#     labs_e61(
#       title    = paste0("Cohort born ", birth_year, ": Net Haig–Simons income"),
#       subtitle = "$(000)",
#       x = "Year", y = ""
#     ) +
#     theme_e61(legend = "bottom") +
#     scale_y_continuous_e61(limits = c(-40, 300, 40))
#   print(cohort_plot)
# }
# 
# ggplot(smooth_dt[!is.na(net_hs_smooth)],
#        aes(x = year, y = net_hs_smooth, group = factor(cohort), colour = factor(cohort))) +
#   geom_line(linewidth = 1) +
#   labs_e61(
#     title = "Smoothed Net Haig–Simons Income: Selected Cohorts",
#     y = "Thousands ($000)", colour = "Cohort"
#   ) +
#   theme_e61(legend = "right") 
# 
# smooth_dt[, age := year - cohort]
# 
# ggplot(smooth_dt[!is.na(net_hs_smooth)],
#        aes(x = age, y = net_hs_smooth,
#            group = factor(cohort), colour = factor(cohort))) +
#   geom_line(linewidth = 1) +
#   labs_e61(
#     title = "Smoothed Net Haig–Simons Income by Age",
#     x = "Age",
#     y = "Thousands ($000)",
#     colour = "Cohort"
#   ) +
#   theme_e61(legend = "right") +
#   scale_y_continuous_e61(limits = c(-40, 200, 40))

## Get ChatGPT to smooth the above things

# --------------------------
# 0) Inputs
# --------------------------
birth_start <- 1943
birth_end   <- 1988
year_end    <- 2020

# --------------------------
# 1) Build 5-year cohort bins & average within bin
# --------------------------
base <- copy(dt)[year <= year_end]
# single-year birth year
base[, birth_year := as.integer(year - age)]

# keep only desired birth-year window
base <- base[birth_year >= birth_start & birth_year <= birth_end]

# map birth_year -> 5-year bin lower bound anchored at 1943
base[, cohort5_low := birth_year - ((birth_year - birth_start) %% 5L)]
base[, cohort5_lab := paste0(cohort5_low, "-", cohort5_low + 4L)]
base[, cohort5_mid := cohort5_low + 2L]  # representative birth year (midpoint)

# average net HS income within each 5-year cohort bin for each calendar year
cohort5_dt <- base[, .(
  net_hs = mean((Haig_simon_income + net_trans) / 1000, na.rm = TRUE)
), by = .(cohort5_low, cohort5_lab, cohort5_mid, year)][order(cohort5_low, year)]

# --------------------------
# 2) Smooth each 5-year cohort series over calendar year
# --------------------------
smooth_list <- lapply(unique(cohort5_dt$cohort5_low), function(by_low) {
  d <- cohort5_dt[cohort5_low == by_low, .(year = as.numeric(year),
                                           net_hs = as.numeric(net_hs),
                                           cohort5_low, cohort5_lab, cohort5_mid)][order(year)]
  # guard: drop NA/inf, collapse duplicate years if any
  d <- d[is.finite(year) & is.finite(net_hs)][, .(net_hs = mean(net_hs)), by = .(year, cohort5_low, cohort5_lab, cohort5_mid)][order(year)]
  
  if (nrow(d) >= 5 && length(unique(d$year)) >= 4) {
    fit <- loess(net_hs ~ year, data = d, span = 0.30, degree = 1, na.action = na.exclude)
    d[, net_hs_smooth := as.numeric(predict(fit, newdata = data.frame(year = year)))]
  } else {
    d[, net_hs_smooth := NA_real_]
  }
  d[]
})

smooth_dt5 <- rbindlist(smooth_list, use.names = TRUE, fill = TRUE)

# --------------------------
# 3) Plot by calendar year (cohort bins averaged)
# --------------------------
ggplot(smooth_dt5[!is.na(net_hs_smooth)],
       aes(x = year, y = net_hs_smooth, colour = cohort5_lab, group = cohort5_lab)) +
  geom_line(linewidth = 1) +
  labs_e61(
    title = "Smoothed Net Haig–Simons Income: 5-year Birth Cohorts (averaged)",
    y = "Thousands ($000)", colour = "Birth cohort"
  ) +
  theme_e61(legend = "right")

# --------------------------
# 4) (Optional) Re-express x as AGE using the cohort-bin midpoint
#     -> aligns life-cycle profiles across 5-year cohorts
# --------------------------
smooth_dt5[, age_mid := year - cohort5_mid]

ggplot(smooth_dt5[!is.na(net_hs_smooth)],
       aes(x = age_mid, y = net_hs_smooth, colour = cohort5_lab, group = cohort5_lab)) +
  geom_line(linewidth = 1) +
  labs_e61(
    title = "Smoothed Net Haig–Simons Income by Age (5-year cohorts, midpoint aligned)",
    x = "Age (using midpoint birth year of each 5-year cohort)",
    y = "Thousands ($000)",
    colour = "Birth cohort"
  ) +
  theme_e61(legend = "right")

### Projections

# --- Inputs you can tweak -----------------------------------------------------
age_min_plot   <- 20      # start age for cumulation/plots
age_max_plot   <- 75      # end age for cumulation/plots
youngest_n     <- 4L      # how many youngest 5-yr cohorts to define the template profile
anchor_to_edge <- TRUE    # anchor at each cohort's first observed age

# --- 1) Ensure clean age grid -------------------------------------------------
smooth_dt5[, age_mid := as.integer(year - cohort5_mid)]
sd_ok <- smooth_dt5[!is.na(net_hs_smooth)][order(cohort5_low, age_mid)]

# --- 2) Build a "young cohort" template life-cycle profile --------------------
# pick the youngest N cohort mids
young_mids <- sort(unique(sd_ok$cohort5_mid), decreasing = TRUE)
young_mids <- young_mids[seq_len(min(length(young_mids), youngest_n))]

# template from youngest cohorts (mean across those cohorts at each age)
templ_young <- sd_ok[cohort5_mid %in% young_mids,
                     .(templ = mean(net_hs_smooth, na.rm = TRUE)),
                     by = age_mid]

# fallback template using ALL cohorts (to fill any gaps the "young" template doesn't cover)
templ_all <- sd_ok[, .(templ_all = mean(net_hs_smooth, na.rm = TRUE)), by = age_mid]

# merge and choose the young template when available; otherwise use all-cohort mean
template <- merge(templ_all, templ_young, by = "age_mid", all = TRUE)
template[, templ_use := fifelse(is.finite(templ), templ, templ_all)]
setorder(template, age_mid)

# --- 3) Back-cast early ages for each cohort using scaled template ------------
extend_one <- function(d, template, anchor_to_edge = TRUE) {
  # d: one cohort data with columns (cohort5_low, cohort5_lab, cohort5_mid, age_mid, net_hs_smooth)
  d <- copy(d)[order(age_mid)]
  min_age_obs <- min(d$age_mid)
  
  # choose anchor age: first observed age (default), or nearest where both series exist
  if (anchor_to_edge) {
    anchor_age <- min_age_obs
  } else {
    # pick the age in d that also exists in template and is closest to min_age_obs
    anchor_age <- template[ , age_mid[which.min(abs(age_mid - min_age_obs))] ]
  }
  
  # get anchor values (nearest match if exact age not in template)
  anch_t <- template[ , .SD[which.min(abs(age_mid - anchor_age))], .SDcols = c("age_mid","templ_use")]
  anch_c <- d[      , .SD[which.min(abs(age_mid - anchor_age))], .SDcols = c("age_mid","net_hs_smooth")]
  
  # scaling factor to map template level to cohort level at the anchor
  scale_fac <- as.numeric(anch_c$net_hs_smooth / anch_t$templ_use)
  if (!is.finite(scale_fac)) scale_fac <- NA_real_
  
  # ages to back-cast: below the cohort's first observed age, within template support
  missing_ages <- template[age_mid < min_age_obs, age_mid]
  if (length(missing_ages) == 0L || !is.finite(scale_fac)) {
    d[, source := "observed"]
    return(d[])
  }
  
  imputed <- template[age_mid %in% missing_ages,
                      .(cohort5_low = d$cohort5_low[1],
                        cohort5_lab = d$cohort5_lab[1],
                        cohort5_mid = d$cohort5_mid[1],
                        age_mid,
                        net_hs_smooth = scale_fac * templ_use,
                        source = "backcast")]
  
  obs <- copy(d)[, source := "observed"]
  out <- rbindlist(list(imputed, obs), use.names = TRUE, fill = TRUE)[order(age_mid)]
  out[]
}

ext_list <- lapply(unique(sd_ok$cohort5_low), function(clow) {
  d <- sd_ok[cohort5_low == clow,
             .(cohort5_low, cohort5_lab, cohort5_mid, age_mid, net_hs_smooth)]
  extend_one(d, template, anchor_to_edge = anchor_to_edge)
})

ext_dt <- rbindlist(ext_list, use.names = TRUE, fill = TRUE)

# --- 4) Cumulative income by age (within chosen age band) ---------------------
ext_band <- ext_dt[age_mid >= age_min_plot & age_mid <= age_max_plot]
setorder(ext_band, cohort5_low, age_mid)
ext_band[, cum_net_hs := cumsum(net_hs_smooth), by = cohort5_low]
ext_band[, source := factor(source, levels = c("observed","backcast"),
                            labels = c("Observed","Back-cast"))]

ggplot(ext_band[order(cohort5_low, age_mid)],
       aes(x = age_mid, y = net_hs_smooth)) +
  geom_line(
    aes(group = interaction(cohort5_lab, source, drop = TRUE),
        colour = cohort5_lab, linetype = source),
    linewidth = 0.9, na.rm = TRUE
  ) +
  labs_e61(
    title = "Net Haig–Simons Income by Age (5-yr cohorts, with back-cast)",
    x = "Age", y = "Thousands ($000)",
    colour = "Birth cohort", linetype = "Segment"
  ) +
  theme_e61(legend = "right") +
  scale_linetype_manual(values = c("Observed" = "solid", "Back-cast" = "dashed")) +
  scale_y_continuous_e61()

# (b) CUMULATIVE income by age
ggplot(ext_band, aes(x = age_mid, y = cum_net_hs,
                     colour = cohort5_lab, group = cohort5_lab)) +
  geom_line(linewidth = 1) +
  labs_e61(
    title = paste0("Cumulative Net Haig–Simons Income, ages ", age_min_plot, "–", age_max_plot),
    x = "Age",
    y = "Cumulative thousands ($000)",
    colour = "Birth cohort"
  ) +
  theme_e61(legend = "right") +
  scale_y_continuous_e61()


#########################################

# ---------------- Params ----------------
birth_start  <- 1943L
birth_end    <- 1988L
year_end_inp <- 2019L   # stop before COVID
age_min      <- 20L
age_max      <- 75L
span_loess   <- 0.30

# ---------------- Prep ----------------
DT <- as.data.table(dt)
DT[, net_hs := (Haig_simon_income + net_trans) / 1000]
DT <- DT[year <= year_end_inp & is.finite(net_hs)]
DT[, age := as.integer(age)]
DT[, birth_year := as.integer(year - age)]
DT <- DT[birth_year >= birth_start & birth_year <= birth_end]
DT[, cohort5_low := birth_year - ((birth_year - birth_start) %% 5L)]
DT[, cohort5_lab := paste0(cohort5_low, "-", cohort5_low + 4L)]
DT[, cohort5_mid := cohort5_low + 2L]

# ---------------- 1) Age FE with year FE -> age deltas ----------------
age_fe_dt <- tryCatch({
  stopifnot(requireNamespace("fixest"))
  fe <- fixest::feols(net_hs ~ 1 | age + year, data = DT)
  a  <- fixest::fixef(fe)$age
  data.table(age = as.integer(names(a)), alpha_age = as.numeric(a))
}, error = function(e) {
  # fallback: demean by year, average by age
  yr <- DT[, .(ybar = mean(net_hs, na.rm = TRUE)), by = year]
  tmp <- merge(DT, yr, by = "year", all.x = TRUE)
  tmp[, neutral := net_hs - ybar]
  tmp[, .(alpha_age = mean(neutral, na.rm = TRUE)), by = age][order(age)]
})
setorder(age_fe_dt, age)
age_fe_dt[, d_alpha := alpha_age - shift(alpha_age)]
if (is.na(age_fe_dt$d_alpha[1])) age_fe_dt$d_alpha[1] <- 0
# continuous age-delta function (constant beyond endpoints)
g_at <- function(a) as.numeric(approx(age_fe_dt$age, age_fe_dt$d_alpha,
                                      xout = a, rule = 2)$y)

# ---------------- 2) Smooth each 5-yr cohort in ORIGINAL units ----------------
cohort_year <- DT[, .(net_hs = mean(net_hs, na.rm = TRUE)),
                  by = .(cohort5_low, cohort5_lab, cohort5_mid, year)]
smooth_list <- lapply(unique(cohort_year$cohort5_low), function(cl) {
  d <- cohort_year[cohort5_low == cl][order(year)]
  if (nrow(d) >= 5 && uniqueN(d$year) >= 4) {
    fit <- loess(net_hs ~ year, data = d, span = span_loess, degree = 1,
                 na.action = na.exclude)
    d[, net_obs := as.numeric(predict(fit, newdata = data.frame(year = year)))]
  } else {
    d[, net_obs := net_hs]
  }
  d[, age := as.integer(year - cohort5_mid)]
  d[, .(cohort5_low, cohort5_lab, cohort5_mid, age, net_obs)][age >= age_min & age <= age_max]
})
obs_smooth <- rbindlist(smooth_list, use.names = TRUE, fill = TRUE)
setorder(obs_smooth, cohort5_low, age)

# ---------------- 3) Fill missing ages using Δα (keep original units) --------
# --- FILLER: keep original units; separate sources ---
fill_one <- function(d, a_min = age_min, a_max = age_max) {
  d <- copy(d)[order(age)]
  if (!nrow(d)) return(NULL)
  
  ages <- seq(a_min, a_max, 1L)
  v <- setNames(rep(NA_real_, length(ages)), as.character(ages))
  v[as.character(d$age)] <- d$net_obs  # observed anchor (original units)
  
  A0 <- min(d$age); B0 <- max(d$age)
  
  # back-cast only if needed
  if (A0 > a_min) {
    for (a in seq(from = A0 - 1L, to = a_min, by = -1L)) {
      v[as.character(a)] <- v[as.character(a + 1L)] - g_at(a + 1L)  # use Δα at a+1
    }
  }
  # forward only if needed
  if (B0 < a_max) {
    for (a in seq(from = B0 + 1L, to = a_max, by = 1L)) {
      v[as.character(a)] <- v[as.character(a - 1L)] + g_at(a)       # use Δα at a
    }
  }
  
  out <- data.table(
    cohort5_low = d$cohort5_low[1],
    cohort5_lab = d$cohort5_lab[1],
    age         = ages,
    net_filled  = as.numeric(v)
  )
  out[, source := fifelse(age < A0, "Back-cast",
                          fifelse(age > B0, "Forecast", "Observed"))]
  out[]
}

# rebuild filled_dt with separate sources
filled_dt <- rbindlist(
  lapply(unique(obs_smooth$cohort5_low), function(cl) {
    fill_one(obs_smooth[cohort5_low == cl, .(cohort5_low, cohort5_lab, age, net_obs)])
  }),
  use.names = TRUE, fill = TRUE
)


# ---------------- 4) Simple plots ----------------
# LEVELS
ggplot() +
  geom_line(
    data = filled_dt[source == "Observed"],
    aes(x = age, y = net_filled, colour = cohort5_lab, group = cohort5_lab),
    linewidth = 0.9
  ) +
  geom_line(
    data = filled_dt[source == "Back-cast"],
    aes(x = age, y = net_filled, colour = cohort5_lab, group = cohort5_lab, linetype = source),
    linewidth = 0.9
  ) +
  geom_line(
    data = filled_dt[source == "Forecast"],
    aes(x = age, y = net_filled, colour = cohort5_lab, group = cohort5_lab, linetype = source),
    linewidth = 0.9
  ) +
  scale_linetype_manual(values = c("Back-cast" = "dashed", "Forecast" = "dotted")) +
  labs(title = "Net HS income by age (filled using age Δ with year FE)",
       x = "Age", y = "Thousands ($000)", colour = "5-year cohort", linetype = "") +
  theme_minimal()

# CUMULATIVE (optional)
filled_dt[, cum := cumsum(net_filled), by = cohort5_low]
ggplot() +
  geom_line(
    data = filled_dt[source == "Observed"],
    aes(x = age, y = cum, colour = cohort5_lab, group = cohort5_lab),
    linewidth = 1
  ) +
  geom_line(
    data = filled_dt[source == "Back-cast"],
    aes(x = age, y = cum, colour = cohort5_lab, group = cohort5_lab, linetype = source),
    linewidth = 1
  ) +
  geom_line(
    data = filled_dt[source == "Forecast"],
    aes(x = age, y = cum, colour = cohort5_lab, group = cohort5_lab, linetype = source),
    linewidth = 1
  ) +
  scale_linetype_manual(values = c("Back-cast" = "dashed", "Forecast" = "dotted")) +
  labs(title = "Cumulative net HS income by age (filled; original units)",
       x = "Age", y = "Cumulative thousands ($000)", colour = "5-year cohort", linetype = "") +
  theme_minimal()

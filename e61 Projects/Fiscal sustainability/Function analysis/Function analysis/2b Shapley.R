## ======================================================================
## Shapley decomposition: Δln(government spending per person)
## Factors: GDP per capita, Ageing (five bins), Policy residual
## Data: ABS via readabs (5206.0, 3101.0 Table 59)
## Last update: 18/08/2025   Author: Matt Nolan
## ======================================================================

# -------------------- Packages --------------------
library(data.table)
library(readabs)
library(lubridate)
library(ggplot2)

rm(list = ls()); invisible(gc())

# -------------------- User prefs --------------------
freq            <- "FY"   # "FY" (financial years) or "CY" (calendar years)
use_real        <- TRUE   # TRUE: chain volume; FALSE: current prices
measure         <- "GFCE" # "GFCE" or "GFCE_plus_GFCF"
abs_check_local <- TRUE   # TRUE uses cache (fast after first run)

# -------------------- Helpers --------------------
# FY ends in June in AU: add 6 months and take calendar year
fy_end_year <- function(date) {
  d <- lubridate::as_date(date)
  lubridate::year(d %m+% months(6))
}

pick_pref <- function(dq) {
  sa <- dq[grepl("Seasonally adjusted", series_type, ignore.case = TRUE) |
             grepl("Seasonally adjusted", series, ignore.case = TRUE)]
  if (nrow(sa)) return(sa)
  tr <- dq[grepl("\\bTrend\\b", series_type, ignore.case = TRUE) |
             grepl("\\bTrend\\b", series, ignore.case = TRUE)]
  if (nrow(tr)) return(tr)
  dq[grepl("\\bOriginal\\b", series_type, ignore.case = TRUE) |
       grepl("\\bOriginal\\b", series, ignore.case = TRUE)]
}

annualise <- function(dt, out_name, mode = c("mean","sum"), freq = c("FY","CY")) {
  mode <- match.arg(mode); freq <- match.arg(freq)
  if (!inherits(dt$date, "Date")) dt[, date := as.Date(date)]
  tmp <- if (freq == "FY") {
    if (mode == "mean") dt[, .(val = mean(value, na.rm = TRUE)), by = .(year = fy_end_year(date))]
    else                dt[, .(val = sum(value,  na.rm = TRUE)), by = .(year = fy_end_year(date))]
  } else {
    if (mode == "mean") dt[, .(val = mean(value, na.rm = TRUE)), by = .(year = year(date))]
    else                dt[, .(val = sum(value,  na.rm = TRUE)), by = .(year = year(date))]
  }
  setnames(tmp, "val", out_name)[order(year)]
}

# -------------------- ABS 5206.0: GFCE, GFCF, GDP --------------------
get_gfce_gdp <- function(freq = c("FY","CY"), check_local = TRUE, verbose = TRUE) {
  freq <- match.arg(freq)
  
  na <- as.data.table(read_abs("5206.0", check_local = check_local))
  if (!nrow(na)) stop("read_abs('5206.0') returned 0 rows.")
  if (!inherits(na$date, "Date")) na[, date := as.Date(date)]
  if (!"series_type" %in% names(na)) na[, series_type := ""]  # some caches miss this col
  
  # Keep only columns we need
  keep <- intersect(c("date","series","value","series_type","unit"), names(na))
  na <- na[, ..keep]
  
  # We want "General government ;  Final consumption expenditure ;" (TOTAL),
  # and "General government ;  Gross fixed capital formation ;" (TOTAL),
  # excluding "National" or "State and local".
  is_gen_total <- function(s) grepl("^\\s*General\\s+government\\s*;\\s*[^-]", s, ignore.case = TRUE) &&
    !grepl("National|State and local", s, ignore.case = TRUE)
  is_gfce <- function(s) grepl("Final\\s+consumption\\s+expenditure\\s*;", s, ignore.case = TRUE)
  is_gfcf <- function(s) grepl("Gross\\s+fixed\\s+capital\\s+formation\\s*;", s, ignore.case = TRUE)
  is_bad  <- function(s) grepl("Percentage\\s+changes|Index\\b", s, ignore.case = TRUE)
  
  gg <- na[vapply(series, is_gen_total, logical(1)) &
             !vapply(series, is_bad,     logical(1))]
  
  gg_gfce <- gg[vapply(series, is_gfce, logical(1))]
  gg_gfcf <- gg[vapply(series, is_gfcf, logical(1))]
  
  # Price basis detection: rely on unit/series text
  is_nom  <- function(s,u) grepl("Current\\s*prices?", s, TRUE) | grepl("Current\\s*prices?", u, TRUE)
  is_real <- function(s,u) grepl("Chain\\s*volume|Volume\\s*chain|Chain-?volume|Volume\\s*measures", s, TRUE) |
    grepl("Chain\\s*volume|Volume\\s*chain|Chain-?volume|Volume\\s*measures", u, TRUE)
  
  gfce_nom_q  <- gg_gfce[is_nom (series, unit)]
  gfce_real_q <- gg_gfce[is_real(series, unit)]
  gfcf_nom_q  <- gg_gfcf[is_nom (series, unit)]
  gfcf_real_q <- gg_gfcf[is_real(series, unit)]
  
  # GDP (clear labels exist)
  gdp_nom_q  <- na[grepl("^Gross domestic product: Current prices\\s*;\\s*$", series, TRUE)]
  gdp_real_q <- na[grepl("^Gross domestic product: Chain volume measures\\s*;\\s*$", series, TRUE)]
  
  # prefer SA > Trend > Original (if series_type available)
  gfce_nom_q  <- pick_pref(gfce_nom_q)
  gfce_real_q <- pick_pref(gfce_real_q)
  gfcf_nom_q  <- pick_pref(gfcf_nom_q)
  gfcf_real_q <- pick_pref(gfcf_real_q)
  gdp_nom_q   <- pick_pref(gdp_nom_q)
  gdp_real_q  <- pick_pref(gdp_real_q)
  
  # Allow graceful fallback when only one basis exists
  if (!nrow(gfce_nom_q)  && nrow(gfce_real_q)) gfce_nom_q  <- copy(gfce_real_q)
  if (!nrow(gfce_real_q) && nrow(gfce_nom_q))  gfce_real_q <- copy(gfce_nom_q)
  if (!nrow(gfcf_nom_q)  && nrow(gfcf_real_q)) gfcf_nom_q  <- copy(gfcf_real_q)
  if (!nrow(gfcf_real_q) && nrow(gfcf_nom_q))  gfcf_real_q <- copy(gfcf_nom_q)
  
  # Annualise (quarterly -> FY/CY means)
  gfce_nom_a  <- annualise(gfce_nom_q,  "gfce_nom",  "mean", freq)
  gfce_real_a <- annualise(gfce_real_q, "gfce_real", "mean", freq)
  gfcf_nom_a  <- annualise(gfcf_nom_q,  "gfcf_nom",  "mean", freq)
  gfcf_real_a <- annualise(gfcf_real_q, "gfcf_real", "mean", freq)
  gdp_nom_a   <- annualise(gdp_nom_q,   "gdp_nom",   "mean", freq)
  gdp_real_a  <- annualise(gdp_real_q,  "gdp_real",  "mean", freq)
  
  out <- Reduce(function(x,y) merge(x,y, by = "year", all = TRUE),
                list(gfce_nom_a, gfce_real_a, gfcf_nom_a, gfcf_real_a, gdp_nom_a, gdp_real_a))
  out[order(year)]
}

# -------------------- ABS 3101.0 Table 59: ERP & age shares --------------------
get_erp_total_and_shares <- function(freq = c("FY","CY"),
                                     table_hint = "59",
                                     check_local = TRUE) {
  freq <- match.arg(freq)
  
  erp_raw <- as.data.table(read_abs("3101.0", tables = table_hint, check_local = check_local))
  if (!"date" %in% names(erp_raw)) stop("ABS pull missing 'date' column.")
  if (!inherits(erp_raw$date, "Date")) erp_raw[, date := as.Date(date)]
  
  # Age label lives in 'item' on Table 59; fall back to 'series' if needed
  age_text_col <- if ("item" %in% names(erp_raw)) "item" else "series"
  
  # Prefer Persons if a sex column exists
  if ("sex" %in% names(erp_raw) && any(grepl("^Persons$", erp_raw$sex, TRUE))) {
    erp_raw <- erp_raw[grepl("^Persons$", sex, TRUE)]
  }
  
  # Parse single-year ages (e.g., "0 years", "85 years and over")
  parse_age_bounds <- function(x) {
    x <- tolower(x)
    nums <- regmatches(x, gregexpr("\\d+", x))[[1]]
    if (!length(nums)) return(c(NA_integer_, NA_integer_))
    lo <- as.integer(nums[1])
    hi <- if (grepl("over|and over|\\+", x)) 120L else lo
    c(lo, hi)
  }
  bounds <- do.call(rbind, lapply(erp_raw[[age_text_col]], parse_age_bounds))
  erp_raw[, age_lo := bounds[,1]]
  erp_raw[, age_hi := bounds[,2]]
  ages <- erp_raw[!is.na(age_lo) & !is.na(age_hi)]
  
  # Map ages into five bins
  map_bin <- function(lo, hi) {
    if (hi <= 14) return("0_14")
    if (lo >= 15 && hi <= 34) return("15_34")
    if (lo >= 35 && hi <= 54) return("35_54")
    if (lo >= 55 && hi <= 64) return("55_64")
    if (lo >= 65) return("65p")
    NA_character_
  }
  ages[, group := mapply(map_bin, age_lo, age_hi)]
  ages <- ages[!is.na(group)]
  
  # Sum by date × group; total ERP by date
  ages_grp <- ages[, .(pop_grp = sum(value, na.rm = TRUE)), by = .(date, group)]
  erp_tot  <- ages_grp[, .(pop_total = sum(pop_grp, na.rm = TRUE)), by = .(date)]
  
  # Shares
  erp_m <- merge(ages_grp, erp_tot, by = "date", all.x = TRUE)
  erp_m[, share := pop_grp / pop_total]
  
  if (freq == "FY") {
    shares_a <- erp_m[, .(share = mean(share, na.rm = TRUE)), by = .(year = fy_end_year(date), group)]
    erp_tot_a <- erp_tot[, .(pop_total = mean(pop_total, na.rm = TRUE)), by = .(year = fy_end_year(date))]
  } else {
    shares_a <- erp_m[, .(share = mean(share, na.rm = TRUE)), by = .(year = year(date), group)]
    erp_tot_a <- erp_tot[, .(pop_total = mean(pop_total, na.rm = TRUE)), by = .(year = year(date))]
  }
  
  shares_w <- dcast(shares_a, year ~ group, value.var = "share")
  for (g in c("0_14","15_34","35_54","55_64","65p")) if (!g %in% names(shares_w)) shares_w[, (g) := NA_real_]
  setcolorder(shares_w, c("year","0_14","15_34","35_54","55_64","65p"))
  
  merge(erp_tot_a, shares_w, by = "year", all = TRUE)[order(year)]
}

# -------------------- Build spend per person --------------------
build_spend_pc <- function(freq = c("FY","CY"), use_real = TRUE,
                           measure = c("GFCE","GFCE_plus_GFCF"),
                           check_local = TRUE) {
  freq <- match.arg(freq); measure <- match.arg(measure)
  na  <- get_gfce_gdp(freq = freq, check_local = check_local)
  erp <- get_erp_total_and_shares(freq = freq, check_local = check_local)
  
  dt <- merge(na, erp, by = "year", all = FALSE)
  if (!nrow(dt)) stop("Merged 5206.0 × 3101.0 is empty (no overlapping years). Try switching FY/CY.")
  
  if (!"pop_total" %in% names(dt)) stop("`pop_total` missing after ERP merge.")
  if (!is.numeric(dt$pop_total))   dt[, pop_total := as.numeric(pop_total)]
  
  # pick basis columns
  req_gdp  <- if (use_real) "gdp_real"  else "gdp_nom"
  req_gfce <- if (use_real) "gfce_real" else "gfce_nom"
  req_gfcf <- if (use_real) "gfcf_real" else "gfcf_nom"
  if (!req_gdp %in% names(dt)) stop(sprintf("Missing GDP column `%s`.", req_gdp))
  
  # choose government outlay definition
  if (measure == "GFCE") {
    if (!req_gfce %in% names(dt)) stop(sprintf("Missing GFCE column `%s`.", req_gfce))
    dt[, govt_level := get(req_gfce)]
  } else {
    has_gfce <- req_gfce %in% names(dt)
    has_gfcf <- req_gfcf %in% names(dt)
    if (!has_gfce && !has_gfcf) stop(sprintf("Missing both `%s` and `%s`.", req_gfce, req_gfcf))
    if (!has_gfce) warning(sprintf("`%s` missing; using only `%s`.", req_gfce, req_gfcf))
    if (!has_gfcf) warning(sprintf("`%s` missing; using only `%s`.", req_gfcf, req_gfce))
    dt[, govt_level := (if (has_gfce) get(req_gfce) else 0) + (if (has_gfcf) get(req_gfcf) else 0)]
  }
  
  # per-capita and GDP per capita
  dt[, spend_pc := govt_level / pop_total]
  dt[, gdp_pc   := get(req_gdp) / pop_total]
  
  # quick age-share sanity
  if (all(c("0_14","15_34","35_54","55_64","65p") %in% names(dt))) {
    dt[, age_sum := `0_14` + `15_34` + `35_54` + `55_64` + `65p`]
    bad <- which(abs(dt$age_sum - 1) > 0.03)
    if (length(bad)) warning("Age shares don’t sum to ~1 for years: ", paste(dt$year[bad], collapse=", "))
  } else {
    warning("One or more age-share columns missing after merge.")
  }
  
  dt[order(year)]
}

# -------------------- Age index & Shapley --------------------
pairwise_age_index <- function(s0, s1) {
  mean_s <- 0.5 * (s0 + s1)
  dln    <- log(pmax(s1, 1e-12)) - log(pmax(s0, 1e-12))
  dlnA   <- sum(mean_s * dln)
  list(dlnA = dlnA, contrib_by_group = mean_s * dln)
}

shapley3 <- function(f0, f1) {
  players <- names(f0)
  perms <- list(
    c("GDPpc","Age","Policy"), c("GDPpc","Policy","Age"),
    c("Age","GDPpc","Policy"), c("Age","Policy","GDPpc"),
    c("Policy","GDPpc","Age"), c("Policy","Age","GDPpc")
  )
  v <- function(f) sum(f)
  phi <- setNames(numeric(3), players)
  for (p in perms) {
    prev <- v(f0); cur <- f0
    for (pl in p) {
      nxt <- cur; nxt[pl] <- f1[pl]
      phi[pl] <- phi[pl] + (v(nxt) - prev)
      prev <- v(nxt); cur <- nxt
    }
  }
  phi / length(perms)
}

# -------------------- Master: make_decompositions --------------------
make_decompositions <- function(freq = c("FY","CY"), use_real = TRUE,
                                measure = c("GFCE","GFCE_plus_GFCF"),
                                check_local = TRUE,
                                checkpoint_csv = "decomposition_input.csv") {
  freq <- match.arg(freq); measure <- match.arg(measure)
  dt <- build_spend_pc(freq, use_real, measure, check_local)
  dt <- dt[complete.cases(year, spend_pc, gdp_pc, `0_14`,`15_34`,`35_54`,`55_64`,`65p`)]
  setorder(dt, year)
  
  # ---- Save checkpoint BEFORE period splitting ----
  fwrite(dt, checkpoint_csv)
  assign("decomp_pre_period", dt, envir = .GlobalEnv)
  
  # Decomposition pair function
  decomp_pair <- function(y0, y1) {
    r0 <- dt[year == y0]; r1 <- dt[year == y1]
    if (!nrow(r0) || !nrow(r1)) return(NULL)
    s0 <- unlist(r0[, .(`0_14`,`15_34`,`35_54`,`55_64`,`65p`)], use.names = TRUE)
    s1 <- unlist(r1[, .(`0_14`,`15_34`,`35_54`,`55_64`,`65p`)], use.names = TRUE)
    age <- pairwise_age_index(s0, s1)
    A0 <- 1; A1 <- exp(age$dlnA)
    
    lnGpc0 <- log(r0$spend_pc); lnGpc1 <- log(r1$spend_pc)
    lnGDP0 <- log(r0$gdp_pc);   lnGDP1 <- log(r1$gdp_pc)
    
    lnPol0 <- lnGpc0 - lnGDP0 - log(A0)
    lnPol1 <- lnGpc1 - lnGDP1 - log(A1)
    
    f0 <- c(GDPpc = lnGDP0, Age = log(A0), Policy = lnPol0)
    f1 <- c(GDPpc = lnGDP1, Age = log(A1), Policy = lnPol1)
    
    phi <- shapley3(f0, f1)
    
    data.table(
      y0 = y0, y1 = y1,
      dln_Gpc   = lnGpc1 - lnGpc0,
      dln_GDPpc = phi["GDPpc"],
      dln_Age   = phi["Age"],
      dln_Policy= phi["Policy"],
      age_0_14  = age$contrib_by_group["0_14"],
      age_15_34 = age$contrib_by_group["15_34"],
      age_35_54 = age$contrib_by_group["35_54"],
      age_55_64 = age$contrib_by_group["55_64"],
      age_65p   = age$contrib_by_group["65p"]
    )
  }
  
  # ---- Four automatic periods (quartiles of years) ----
  yrs <- unique(dt$year)
  if (length(yrs) < 4L) stop("Not enough distinct years to form 4 bins.")
  brks <- unique(as.integer(quantile(yrs, probs = c(0,.25,.5,.75,1), type = 1)))
  dt[, bin := findInterval(year, brks, all.inside = TRUE)]
  bin_bounds <- dt[, .(y0 = min(year), y1 = max(year)), by = bin][order(bin)]
  if (nrow(bin_bounds) > 4L) bin_bounds <- bin_bounds[1:4]
  
  four_bin <- rbindlist(lapply(seq_len(nrow(bin_bounds)), function(i) {
    decomp_pair(bin_bounds$y0[i], bin_bounds$y1[i])
  }), use.names = TRUE)
  
  # ---- Panel (adjacent years) ----
  panel <- if (nrow(dt) >= 2) {
    rbindlist(lapply(1:(nrow(dt)-1), function(i) decomp_pair(dt$year[i], dt$year[i+1])), use.names = TRUE)
  } else data.table()
  
  list(base = dt, four_bin = four_bin, panel = panel)
}

# -------------------- Run --------------------
res <- make_decompositions(freq = freq, use_real = use_real,
                           measure = measure, check_local = abs_check_local)

four_bin <- res$four_bin
panel    <- res$panel

# Checks (should sum exactly modulo rounding)
if (nrow(four_bin)) four_bin[, check_sum := dln_GDPpc + dln_Age + dln_Policy]
if (nrow(panel))    panel[,    check_sum := dln_GDPpc + dln_Age + dln_Policy]

# -------------------- Plot (panel) --------------------
if (nrow(panel)) {
  panel_m <- melt(panel,
                  id.vars = c("y0","y1","dln_Gpc"),
                  measure.vars = c("dln_GDPpc","dln_Age","dln_Policy"),
                  variable.name = "component", value.name = "dln_contrib")
  panel_m[, year := y1]
  
  print(
    ggplot(panel_m, aes(x = year, y = dln_contrib, fill = component)) +
      geom_col(position = "stack") +
      geom_line(aes(y = dln_Gpc, group = 1), inherit.aes = FALSE) +
      geom_point(aes(y = dln_Gpc), inherit.aes = FALSE) +
      labs(title = "Shapley: Δln(Government spending per person), year-to-year",
           y = "Log points", x = "Year", fill = "Factor") +
      theme_bw()
  )
}

# -------------------- Inspect --------------------
if (nrow(four_bin)) {
  print(four_bin[, .(Segment = paste0(y0, "→", y1),
                     dln_Gpc, dln_GDPpc, dln_Age, dln_Policy,
                     age_0_14, age_15_34, age_35_54, age_55_64, age_65p)])
}

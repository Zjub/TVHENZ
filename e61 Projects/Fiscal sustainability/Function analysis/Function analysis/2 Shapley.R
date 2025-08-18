## Last update:  18/08/2025
## Author:       Matt Nolan
## Last updater: Matt Nolan
## Purpose: Shapley decomposition of Δln(Govt spending per person) into GDPpc, Ageing, Policy
## Data: ABS via readabs (GFCE, GDP, ERP total + age groups)

# ============================ ALL-IN-ONE SCRIPT ============================

# ---- Packages ----
library(data.table)
library(readabs)
library(lubridate)
library(ggplot2)

rm(list = ls())
gc()

# ---- Preferences ----
freq     <- "FY"     # "FY" = financial years; "CY" = calendar years
use_real <- TRUE     # TRUE = real (chain volume); FALSE = nominal (current prices)

# ---- Utils ----
fy_end_year <- function(date) {
  d <- lubridate::as_date(date)            # ensure Date
  lubridate::year(d %m+% months(6))
}

pick_pref <- function(dq) {
  sa <- dq[grepl("Seasonally adjusted", series, ignore.case = TRUE)]
  if (nrow(sa)) return(sa)
  tr <- dq[grepl("\\bTrend\\b", series, ignore.case = TRUE)]
  if (nrow(tr)) return(tr)
  dq[grepl("\\bOriginal\\b", series, ignore.case = TRUE)]
}

annualise <- function(dt, out_name, mode = c("mean", "sum"),
                      freq = c("FY","CY")) {
  mode <- match.arg(mode)
  freq <- match.arg(freq)
  if (!inherits(dt$date, "Date")) dt[, date := as.Date(date)]
  if (freq == "FY") {
    tmp <- if (mode == "mean") {
      dt[, .(val = mean(value, na.rm = TRUE)), by = .(year = fy_end_year(date))]
    } else {
      dt[, .(val = sum(value,  na.rm = TRUE)), by = .(year = fy_end_year(date))]
    }
  } else {
    tmp <- if (mode == "mean") {
      dt[, .(val = mean(value, na.rm = TRUE)), by = .(year = lubridate::year(lubridate::as_date(date)))]
    } else {
      dt[, .(val = sum(value,  na.rm = TRUE)), by = .(year = lubridate::year(lubridate::as_date(date)))]
    }
  }
  setnames(tmp, "val", out_name)
  tmp[order(year)]
}

# ---- 1) ABS: GFCE (govt spending) & GDP ----
get_gfce_gdp <- function(freq = c("FY","CY"), verbose = TRUE) {
  freq <- match.arg(freq)
  
  na <- as.data.table(read_abs("5206.0", check_local = FALSE))
  if (!nrow(na)) stop("read_abs('5206.0') returned 0 rows.")
  if (!inherits(na$date, "Date")) na[, date := as.Date(date)]
  
  # Build a combined text field across useful metadata columns
  txt_cols <- intersect(
    c("series","table_title","unit","data_type","series_type"),
    names(na)
  )
  if (!length(txt_cols)) txt_cols <- "series"
  na[, .__txt := do.call(paste, c(.SD, list(sep = " | "))), .SDcols = txt_cols]
  
  # Helper: prefer Seasonally adjusted, else Trend, else Original
  prefer_sa <- function(DT) {
    if (!nrow(DT)) return(DT)
    sa <- DT[grepl("Seasonally adjusted", .__txt, ignore.case = TRUE)]
    if (nrow(sa)) return(sa)
    tr <- DT[grepl("\\bTrend\\b", .__txt, ignore.case = TRUE)]
    if (nrow(tr)) return(tr)
    DT[grepl("\\bOriginal\\b", .__txt, ignore.case = TRUE)]
  }
  
  ann <- function(DT, out_name) {
    if (!nrow(DT)) return(data.table(year = integer(), (out_name) := numeric()))
    annualise(DT, out_name = out_name, mode = "mean", freq = freq)
  }
  
  # -------------------- Selectors --------------------
  # We only want the TOTAL "General government ; ..." (exclude National / State & local)
  is_gen_gov_total <- function(s) {
    grepl("^\\s*General\\s+government\\s*;\\s*", s, ignore.case = TRUE) &&
      !grepl("National|State and local", s, ignore.case = TRUE)
  }
  
  # Subject pickers
  is_gfce <- function(s) grepl("Final\\s+consumption\\s+expenditure\\s*;", s, ignore.case = TRUE)
  is_gfcf <- function(s) grepl("Gross\\s+fixed\\s+capital\\s+formation\\s*;", s, ignore.case = TRUE)
  
  # Exclude rates/indices/percentage changes
  is_bad_var <- function(s) grepl("Percentage\\s+changes|Index", s, ignore.case = TRUE)
  
  # Price basis
  is_nom <- function(s) grepl("Current\\s*prices?", s, ignore.case = TRUE) ||
    grepl("\\bCP\\b", s, ignore.case = TRUE)
  is_real <- function(s) grepl("Chain\\s*volume|Volume\\s*chain|Chain-?volume|Volume\\s*measures",
                               s, ignore.case = TRUE) ||
    grepl("\\bCVM\\b|\\bvolume\\b", s, ignore.case = TRUE)
  
  # GDP patterns live clearly in labels
  is_gdp <- function(s) grepl("\\bGross\\s+domestic\\s+product\\b|\\bGDP\\b", s, ignore.case = TRUE)
  
  # -------------------- Filter slices --------------------
  rows_gen_total <- vapply(na$series, is_gen_gov_total, logical(1))
  rows_not_bad   <- !vapply(na$series, is_bad_var, logical(1))
  
  # General government totals only, non-rate/percent rows
  gg <- na[rows_gen_total & rows_not_bad]
  
  # Split by subject
  gg_gfce <- gg[vapply(series, is_gfce, logical(1))]
  gg_gfcf <- gg[vapply(series, is_gfcf, logical(1))]
  
  # Basis splits (try series/table_title/unit text)
  gfce_nom_q  <- gg_gfce[ vapply(.__txt, is_nom,  logical(1)) ]
  gfce_real_q <- gg_gfce[ vapply(.__txt, is_real, logical(1)) ]
  gfcf_nom_q  <- gg_gfcf[ vapply(.__txt, is_nom,  logical(1)) ]
  gfcf_real_q <- gg_gfcf[ vapply(.__txt, is_real, logical(1)) ]
  
  # Prefer SA → Trend → Original
  gfce_nom_q  <- prefer_sa(gfce_nom_q)
  gfce_real_q <- prefer_sa(gfce_real_q)
  gfcf_nom_q  <- prefer_sa(gfcf_nom_q)
  gfcf_real_q <- prefer_sa(gfcf_real_q)
  
  # If a basis is missing (some tables only publish one), fall back gracefully:
  # - If only one basis exists, use it for both (you can later choose which to use).
  if (!nrow(gfce_nom_q)  && nrow(gfce_real_q)) gfce_nom_q  <- copy(gfce_real_q)
  if (!nrow(gfce_real_q) && nrow(gfce_nom_q))  gfce_real_q <- copy(gfce_nom_q)
  if (!nrow(gfcf_nom_q)  && nrow(gfcf_real_q)) gfcf_nom_q  <- copy(gfcf_real_q)
  if (!nrow(gfcf_real_q) && nrow(gfcf_nom_q))  gfcf_real_q <- copy(gfcf_nom_q)
  
  # GDP (clear wording for price basis)
  gdp_nom_q  <- prefer_sa(na[ vapply(na$series, is_gdp,  logical(1)) &
                                vapply(na$.__txt, is_nom,  logical(1)) ])
  gdp_real_q <- prefer_sa(na[ vapply(na$series, is_gdp,  logical(1)) &
                                vapply(na$.__txt, is_real, logical(1)) ])
  
  # Diagnostics if subjects totally missing
  if (!nrow(gg_gfce)) stop("Could not find General government total GFCE rows (check labels).")
  if (!nrow(gg_gfcf)) stop("Could not find General government total GFCF rows (check labels).")
  if (!nrow(gdp_nom_q) && !nrow(gdp_real_q)) stop("Could not find GDP rows (check labels).")
  
  # -------------------- Annualise --------------------
  gfce_nom_a   <- ann(gfce_nom_q,   "gfce_nom")
  gfce_real_a  <- ann(gfce_real_q,  "gfce_real")
  gfcf_nom_a   <- ann(gfcf_nom_q,   "gfcf_nom")
  gfcf_real_a  <- ann(gfcf_real_q,  "gfcf_real")
  gdp_nom_a    <- ann(gdp_nom_q,    "gdp_nom")
  gdp_real_a   <- ann(gdp_real_q,   "gdp_real")
  
  out <- Reduce(function(x, y) merge(x, y, by = "year", all = TRUE),
                list(gfce_nom_a, gfce_real_a, gfcf_nom_a, gfcf_real_a, gdp_nom_a, gdp_real_a))
  setorder(out, year)
  
  if (verbose) {
    yr <- function(d) if (nrow(d)) paste(range(d$year, na.rm = TRUE), collapse = " – ") else "∅"
    msg <- paste(
      paste0("GFCE_nom:", yr(gfce_nom_a)),
      paste0("GFCE_real:", yr(gfce_real_a)),
      paste0("GFCF_nom:", yr(gfcf_nom_a)),
      paste0("GFCF_real:", yr(gfcf_real_a)),
      paste0("GDP_nom:",  yr(gdp_nom_a)),
      paste0("GDP_real:", yr(gdp_real_a)),
      sep = " | "
    )
    message("Year ranges — ", msg)
  }
  
  out
}



# ---- 2) ABS: ERP (population total) & age shares ----
ageband_to_group <- function(lo, hi) {
  if (is.na(lo) || is.na(hi)) return(NA_character_)
  if (hi <= 14) return("0_14")
  if (lo >= 15 && hi <= 34) return("15_34")
  if (lo >= 35 && hi <= 54) return("35_54")
  if (lo >= 55 && hi <= 64) return("55_64")
  if (lo >= 65) return("65p")
  NA_character_
}

parse_age_band_from_series <- function(x) {
  x <- tolower(x)
  m <- regmatches(x, gregexpr("\\d+", x))[[1]]
  if (length(m) == 0L) return(c(NA_integer_, NA_integer_))
  lo <- as.integer(m[1])
  hi <- if (length(m) >= 2) as.integer(m[2]) else if (grepl("over|and over|\\+", x)) 120L else lo
  c(lo, hi)
}

get_erp_total_and_shares <- function(freq = c("FY","CY"),
                                     table_hint = "59") {
  freq <- match.arg(freq)
  
  # Pull Table 59 explicitly (single-year-of-age), bypass local cache routing
  erp_raw <- as.data.table(read_abs("3101.0", tables = table_hint, check_local = FALSE))
  if (!"date" %in% names(erp_raw)) stop("ABS pull is missing a 'date' column.")
  if (!inherits(erp_raw$date, "Date")) erp_raw[, date := as.Date(date)]
  
  # Choose a column to read the age label from: prefer 'item', fall back to 'series'
  age_text_col <- if ("item" %in% names(erp_raw)) "item" else if ("series" %in% names(erp_raw)) "series" else NULL
  if (is.null(age_text_col)) stop("Could not find an age label column ('item' or 'series').")
  
  # If a sex dimension exists, prefer 'Persons' (sum otherwise)
  if ("sex" %in% names(erp_raw)) {
    if (any(grepl("^Persons$", erp_raw$sex, ignore.case = TRUE))) {
      erp_raw <- erp_raw[grepl("^Persons$", sex, ignore.case = TRUE)]
    } # else keep both sexes and sum later
  } else {
    # Some tables encode sex in the text; keep all and sum – Table 59 is usually Persons-only.
    invisible(NULL)
  }
  
  # Parse single-year ages (e.g., "0 years", "1 year", "85 years and over")
  parse_age_bounds <- function(x) {
    x <- tolower(x)
    nums <- regmatches(x, gregexpr("\\d+", x))[[1]]
    if (length(nums) == 0L) return(c(NA_integer_, NA_integer_))
    lo <- as.integer(nums[1])
    hi <- if (grepl("over|and over|\\+", x)) 120L else lo
    c(lo, hi)
  }
  
  # Compute lower/upper for each row
  txt <- erp_raw[[age_text_col]]
  bounds <- do.call(rbind, lapply(txt, parse_age_bounds))
  erp_raw[, age_lo := bounds[,1]]
  erp_raw[, age_hi := bounds[,2]]
  
  # Keep rows that actually parsed as ages
  ages <- erp_raw[!is.na(age_lo) & !is.na(age_hi)]
  
  # Map single-year (or open-ended) to bins
  ageband_to_group <- function(lo, hi) {
    if (hi <= 14) return("0_14")
    if (lo >= 15 && hi <= 34) return("15_34")
    if (lo >= 35 && hi <= 54) return("35_54")
    if (lo >= 55 && hi <= 64) return("55_64")
    if (lo >= 65) return("65p")
    NA_character_
  }
  ages[, group := mapply(ageband_to_group, age_lo, age_hi)]
  ages <- ages[!is.na(group)]
  
  # Sum population by date × group (handles any residual dimensions by summing)
  ages_grp <- ages[, .(pop_grp = sum(value, na.rm = TRUE)), by = .(date, group)]
  
  # Total ERP per date: sum across all groups (safer and consistent with Table 59)
  erp_tot <- ages_grp[, .(pop_total = sum(pop_grp, na.rm = TRUE)), by = .(date)]
  
  # Shares
  erp_m <- merge(ages_grp, erp_tot, by = "date", all.x = TRUE)
  erp_m[, share := pop_grp / pop_total]
  
  # Annualise to FY/CY via means over the period
  if (freq == "FY") {
    shares_a <- erp_m[, .(share = mean(share, na.rm = TRUE)),
                      by = .(year = fy_end_year(date), group)]
    erp_tot_a <- erp_tot[, .(pop_total = mean(pop_total, na.rm = TRUE)),
                         by = .(year = fy_end_year(date))]
  } else {
    shares_a <- erp_m[, .(share = mean(share, na.rm = TRUE)),
                      by = .(year = lubridate::year(lubridate::as_date(date)), group)]
    erp_tot_a <- erp_tot[, .(pop_total = mean(pop_total, na.rm = TRUE)),
                         by = .(year = lubridate::year(lubridate::as_date(date)))]
  }
  
  # Wide shares table with all five bins
  shares_a <- dcast(shares_a, year ~ group, value.var = "share")
  for (g in c("0_14","15_34","35_54","55_64","65p")) if (!g %in% names(shares_a)) shares_a[, (g) := NA_real_]
  setcolorder(shares_a, c("year","0_14","15_34","35_54","55_64","65p"))
  
  out <- merge(erp_tot_a, shares_a, by = "year", all = TRUE)[order(year)]
  if (!nrow(out)) stop("ERP join produced zero rows — check annualisation.")
  out
}

# ---- 3) Build spending per person (real/nominal) ----
build_spend_pc <- function(freq = c("FY","CY"), use_real = TRUE,
                           table_hint = "59",
                           write_csv = TRUE) {
  freq <- match.arg(freq)
  
  message("▶ Pulling GFCE & GDP (", freq, ") …")
  na <- get_gfce_gdp(freq)
  if (!nrow(na)) stop("get_gfce_gdp() returned 0 rows.")
  if (!"year" %in% names(na)) stop("GFCE/GDP table has no 'year' column.")
  
  message("   GFCE/GDP years: ", paste(range(na$year, na.rm=TRUE), collapse = " – "),
          "  (n=", nrow(na), ")")
  
  message("▶ Pulling ERP totals + age shares from 3101.0 (", table_hint, ", ", freq, ") …")
  erp <- get_erp_total_and_shares(freq, table_hint = table_hint)
  if (!nrow(erp)) stop("get_erp_total_and_shares() returned 0 rows.")
  if (!"year" %in% names(erp)) stop("ERP table has no 'year' column.")
  
  message("   ERP years: ", paste(range(erp$year, na.rm=TRUE), collapse = " – "),
          "  (n=", nrow(erp), ")")
  
  # Year intersection diagnostics
  yrs_na  <- sort(unique(na$year))
  yrs_erp <- sort(unique(erp$year))
  yrs_int <- intersect(yrs_na, yrs_erp)
  
  message("▶ Year overlap: ", length(yrs_int), " years")
  if (length(yrs_int)) {
    message("   First/last in overlap: ", min(yrs_int), " – ", max(yrs_int))
  } else {
    message("   No overlap. Check that both pulls used the SAME freq (FY vs CY).")
    message("   GFCE/GDP sample: ", paste(head(yrs_na, 5), collapse=", "), " … ",
            paste(tail(yrs_na, 5), collapse=", "))
    message("   ERP sample:      ", paste(head(yrs_erp,5), collapse=", "), " … ",
            paste(tail(yrs_erp,5), collapse=", "))
  }
  
  # Optional: write inputs
  if (isTRUE(write_csv)) {
    fwrite(na,  "debug_gfce_gdp.csv")
    fwrite(erp, "debug_erp_ageshares.csv")
    message('   Wrote debug_gfce_gdp.csv and debug_erp_ageshares.csv')
  }
  
  # Merge on year
  dt <- merge(na, erp, by = "year", all = FALSE)
  
  message("▶ Merged rows: ", nrow(dt))
  if (!nrow(dt)) {
    message("   Merge produced 0 rows. This is almost always a year alignment issue.")
    message("   Try switching freq (e.g., set freq='CY') or confirm the FY mapping used.")
  }
  
  # Compute streams if we have data
  if (nrow(dt)) {
    if (use_real) {
      if (!all(c("gfce_real","gdp_real") %in% names(dt))) {
        stop("Missing real series columns gfce_real/gdp_real even though use_real=TRUE.")
      }
      dt[, spend_pc := gfce_real / pop_total]
      dt[, gdp_pc   := gdp_real  / pop_total]
    } else {
      if (!all(c("gfce_nom","gdp_nom") %in% names(dt))) {
        stop("Missing nominal series columns gfce_nom/gdp_nom even though use_real=FALSE.")
      }
      dt[, spend_pc := gfce_nom / pop_total]
      dt[, gdp_pc   := gdp_nom  / pop_total]
    }
    
    # Age shares sanity
    if (all(c("0_14","15_34","35_54","55_64","65p") %in% names(dt))) {
      dt[, age_sum := `0_14` + `15_34` + `35_54` + `55_64` + `65p`]
      bad <- which(abs(dt$age_sum - 1) > 0.03)
      if (length(bad)) {
        warning("Age shares don’t sum to ~1 for rows: ",
                paste(dt$year[bad], collapse = ", "), ". Check mapping.")
      }
    } else {
      warning("One or more age-share columns missing after merge.")
    }
  }
  
  # Return everything so you can inspect upstream pieces too
  structure(list(na = na, erp = erp, dt = dt, years_overlap = yrs_int),
            class = "build_spend_pc_debug")
}

dbg <- build_spend_pc(freq = freq, use_real = use_real, table_hint = "59", write_csv = TRUE) # Very slow, just there to check if it is running

# ---- 4) Törnqvist age index & Shapley (3 factors) ----
pairwise_age_index <- function(s0, s1) {
  # s0, s1 named numeric vectors with five shares (0_14,15_34,35_54,55_64,65p)
  mean_s <- 0.5 * (s0 + s1)
  dlnA   <- sum(mean_s * (log(pmax(s1, 1e-12)) - log(pmax(s0, 1e-12))))
  list(dlnA = dlnA,
       contrib_by_group = mean_s * (log(pmax(s1, 1e-12)) - log(pmax(s0, 1e-12))))
}

shapley3 <- function(f0, f1) {
  # f0/f1 are named LOG factors: c(GDPpc=.., Age=.., Policy=..)
  players <- names(f0)
  perms <- list(
    c("GDPpc","Age","Policy"),
    c("GDPpc","Policy","Age"),
    c("Age","GDPpc","Policy"),
    c("Age","Policy","GDPpc"),
    c("Policy","GDPpc","Age"),
    c("Policy","Age","GDPpc")
  )
  v <- function(f) sum(f) # ln G/N = sum of logs
  phi <- setNames(numeric(3), players)
  for (p in perms) {
    prev <- v(f0)
    cur  <- f0
    for (pl in p) {
      nxt <- cur; nxt[pl] <- f1[pl]
      marg <- v(nxt) - prev
      phi[pl] <- phi[pl] + marg
      prev <- v(nxt); cur <- nxt
    }
  }
  phi / length(perms)
}

# ---- 5) Main: Decompositions (four bins + panel) ----
make_decompositions <- function(freq = c("FY","CY"), use_real = TRUE) {
  freq <- match.arg(freq)
  dt <- build_spend_pc(freq, use_real, table_hint = "59")
  dt <- dt[complete.cases(year, spend_pc, gdp_pc, `0_14`,`15_34`,`35_54`,`55_64`,`65p`)]
  setorder(dt, year)
  
  decomp_pair <- function(y0, y1) {
    r0 <- dt[year == y0]; r1 <- dt[year == y1]
    if (!nrow(r0) || !nrow(r1)) return(NULL)
    
    s0 <- unlist(r0[, .(`0_14`,`15_34`,`35_54`,`55_64`,`65p`)], use.names = TRUE)
    s1 <- unlist(r1[, .(`0_14`,`15_34`,`35_54`,`55_64`,`65p`)], use.names = TRUE)
    age <- pairwise_age_index(s0, s1)
    dlnA <- age$dlnA
    A0 <- 1; A1 <- exp(dlnA)
    
    lnGpc0 <- log(r0$spend_pc); lnGpc1 <- log(r1$spend_pc)
    lnGDP0 <- log(r0$gdp_pc);   lnGDP1 <- log(r1$gdp_pc)
    
    # Policy term ensures identity: ln(G/N) = ln(Y/N) + ln(A) + ln(Policy)
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
  
  fwrite(dt, "decomposition_input.csv")
  
  # ---- Four automatic periods (quartiles of years) ----
  yrs <- dt$year
  if (length(unique(yrs)) < 4L) stop("Not enough distinct years to form 4 bins.")
  brks <- unique(as.integer(quantile(yrs, probs = c(0, .25, .5, .75, 1), type = 1)))
  bin_id <- findInterval(yrs, brks, all.inside = TRUE)
  dt[, bin := bin_id]
  bin_bounds <- dt[, .(y0 = min(year), y1 = max(year)), by = bin][order(bin)]
  if (nrow(bin_bounds) > 4L) bin_bounds <- bin_bounds[1:4]
  
  four_bin <- rbindlist(lapply(seq_len(nrow(bin_bounds)), function(i) {
    decomp_pair(bin_bounds$y0[i], bin_bounds$y1[i])
  }), use.names = TRUE)
  
  # ---- Panel (year-to-year) ----
  if (nrow(dt) >= 2) {
    panel <- rbindlist(lapply(1:(nrow(dt)-1), function(i) {
      decomp_pair(dt$year[i], dt$year[i+1])
    }), use.names = TRUE)
  } else {
    panel <- data.table()
  }
  
  list(base = dt, four_bin = four_bin, panel = panel)
}

# ---- 6) Run & quick checks ----
res <- make_decompositions(freq = freq, use_real = use_real)
four_bin <- res$four_bin
panel    <- res$panel

# Add check sums: should equal Δln(G/N)
if (nrow(four_bin)) four_bin[, check_sum := dln_GDPpc + dln_Age + dln_Policy]
if (nrow(panel))    panel[,    check_sum := dln_GDPpc + dln_Age + dln_Policy]

# ---- 7) Plot (panel) ----
if (nrow(panel)) {
  panel_m <- melt(panel,
                  id.vars = c("y0","y1","dln_Gpc"),
                  measure.vars = c("dln_GDPpc","dln_Age","dln_Policy"),
                  variable.name = "component", value.name = "dln_contrib")dx
  panel_m[, year := y1]
  
  ggplot(panel_m, aes(x = year, y = dln_contrib, fill = component)) +
    geom_col(position = "stack") +
    geom_line(aes(y = dln_Gpc, group = 1), inherit.aes = FALSE) +
    geom_point(aes(y = dln_Gpc), inherit.aes = FALSE) +
    labs(title = "Shapley decomposition of Δln(Government spending per person), year-to-year",
         y = "Log points", x = "Year", fill = "Factor") +
    theme_bw()
}

# ---- 8) Inspect four-bin table ----
if (nrow(four_bin)) {
  print(four_bin[, .(Segment = paste0(y0, "→", y1),
                     dln_Gpc, dln_GDPpc, dln_Age, dln_Policy,
                     age_0_14, age_15_34, age_35_54, age_55_64, age_65p)])
}

## ---- 9) Optional: peek at raw series labels to refine filters ----
# na <- as.data.table(read_abs("5206.0", check_local = FALSE))
# head(unique(na$series), 50)
# erp <- as.data.table(read_abs("3101.0", check_local = FALSE))
# head(unique(erp$series), 50)
# if ("item" %in% names(erp)) head(unique(erp$item), 50)
#
#
## Fun ChatGPT command to investigate the series without opening the sheet hahaha
# probe_5206 <- function() {
#   na <- as.data.table(read_abs("5206.0", check_local = FALSE))
#   if (!nrow(na)) stop("read_abs('5206.0') returned 0 rows.")
#   if (!inherits(na$date, "Date")) na[, date := as.Date(date)]
#   
#   cat("\n--- 5206.0: columns ---\n")
#   print(names(na))
#   
#   # Build a combined text field from many columns (robust search)
#   txt_cols <- intersect(
#     c("series","item","table","measure","description","series_description",
#       "series_name","series_title","series_type","data_item","statistic"),
#     names(na)
#   )
#   if (!length(txt_cols)) txt_cols <- "series"
#   na[, .__txt := do.call(paste, c(.SD, list(sep = " | "))), .SDcols = txt_cols]
#   
#   cat("\n--- 5206.0: candidate TABLE names containing 'consumption' or 'expenditure' ---\n")
#   if ("table" %in% names(na)) {
#     print(unique(na[grepl("consumption|expenditure|final", table, ignore.case=TRUE), table]))
#   } else {
#     cat("(no 'table' column present)\n")
#   }
#   
#   # Show a few rows for likely candidates
#   gfce_like <- na[grepl("final\\s+consumption.*government|government.*final\\s+consumption|government\\s+consumption",
#                         .__txt, ignore.case=TRUE)]
#   gdp_like  <- na[grepl("\\bgross\\s+domestic\\s+product\\b|\\bGDP\\b",
#                         .__txt, ignore.case=TRUE)]
#   
#   cat("\n--- Example GFCE-like labels ---\n")
#   print(unique(head(gfce_like$series, 20)))
#   cat("\n--- Example GDP-like labels ---\n")
#   print(unique(head(gdp_like$series, 20)))
#   
#   # Write small CSVs for inspection (won’t be huge)
#   fwrite(head(gfce_like, 2000), "probe_gfce_like_5206.csv")
#   fwrite(head(gdp_like , 2000), "probe_gdp_like_5206.csv")
#   cat("\nWrote: probe_gfce_like_5206.csv and probe_gdp_like_5206.csv\n")
#   
#   invisible(na)
# }
# 
# na5206 <- probe_5206()
#
## ==========================================================================

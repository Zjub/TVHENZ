library(data.table)
library(ggplot2)

# =========================================================
# 1. Australian personal tax schedule
#    2024-25 / 2025-26 resident rates, ignoring Medicare
# =========================================================

tax_au <- function(y) {
  y <- pmax(y, 0)
  ifelse(
    y <= 18200, 0,
    ifelse(
      y <= 45000, 0.16 * (y - 18200),
      ifelse(
        y <= 135000, 4288 + 0.30 * (y - 45000),
        ifelse(
          y <= 190000, 31288 + 0.37 * (y - 135000),
          51638 + 0.45 * (y - 190000)
        )
      )
    )
  )
}

mtr_au <- function(y) {
  y <- pmax(y, 0)
  ifelse(
    y <= 18200, 0,
    ifelse(
      y <= 45000, 0.16,
      ifelse(
        y <= 135000, 0.30,
        ifelse(
          y <= 190000, 0.37,
          0.45
        )
      )
    )
  )
}

# =========================================================
# 2. Generate trust income paths
# =========================================================

simulate_trust_income <- function(
  T = 10,
  base_mean = 80000,
  base_sdlog = 0.25,
  p_lump = 0.20,
  lump_mean = 300000,
  lump_sdlog = 0.90
) {
  base_income <- rlnorm(T, meanlog = log(base_mean) - 0.5 * base_sdlog^2,
                        sdlog = base_sdlog)

  lump_occurs <- rbinom(T, size = 1, prob = p_lump)
  lump_income <- lump_occurs * rlnorm(T, meanlog = log(lump_mean) - 0.5 * lump_sdlog^2,
                                      sdlog = lump_sdlog)

  data.table(
    year = 1:T,
    base_income = base_income,
    lump_income = lump_income,
    trust_income = base_income + lump_income
  )
}

# =========================================================
# 3. Beneficiary income scenarios
# =========================================================

make_other_income <- function(type = "mixed", n_benef = 4, T = 10) {

  if (type == "low") {
    mat <- matrix(0, nrow = T, ncol = n_benef)
  } else if (type == "middle") {
    mat <- matrix(80000, nrow = T, ncol = n_benef)
  } else if (type == "high") {
    mat <- matrix(250000, nrow = T, ncol = n_benef)
  } else if (type == "mixed") {
    # example: one high-income adult, one part-time spouse, two adult children
    v <- c(250000, 30000, 0, 0)
    mat <- matrix(rep(v[1:n_benef], each = T), nrow = T, ncol = n_benef)
  } else {
    stop("Unknown income type")
  }

  colnames(mat) <- paste0("b", 1:n_benef)
  mat
}

# =========================================================
# 4. Australian distribution rules
# =========================================================

# 4a. Equal shares
allocate_equal <- function(trust_income, other_income) {
  n <- length(other_income)
  rep(trust_income / n, n)
}

# 4b. Greedy tax-minimising allocation:
# allocate in small chunks to the beneficiary with the lowest current MTR
allocate_tax_min <- function(trust_income, other_income, step = 1000) {

  n <- length(other_income)
  alloc <- rep(0, n)

  remaining <- trust_income

  while (remaining > 1e-8) {
    chunk <- min(step, remaining)
    current_taxable <- other_income + alloc
    j <- which.min(mtr_au(current_taxable))
    alloc[j] <- alloc[j] + chunk
    remaining <- remaining - chunk
  }

  alloc
}

# =========================================================
# 5. Annual Australian family tax from trust income
# =========================================================

tax_au_year <- function(trust_income, other_income, rule = c("equal", "tax_min")) {
  rule <- match.arg(rule)

  alloc <- switch(
    rule,
    equal = allocate_equal(trust_income, other_income),
    tax_min = allocate_tax_min(trust_income, other_income)
  )

  tax_after <- tax_au(other_income + alloc)
  tax_before <- tax_au(other_income)

  list(
    alloc = alloc,
    trust_tax = sum(tax_after - tax_before)
  )
}

# =========================================================
# 6. NZ-style trustee tax rules
# =========================================================

# Variant A: tax all trust income immediately at trustee rate
tax_trust_flat <- function(trust_income, trustee_rate = 0.39) {
  trustee_rate * trust_income
}

# Variant B: only retain the lumpy component; distribute the rest currently
tax_nz_hybrid_year <- function(base_income, lump_income, other_income,
                               trustee_rate = 0.39,
                               dist_rule = c("equal", "tax_min")) {
  dist_rule <- match.arg(dist_rule)

  # distribute base income this year
  dist_res <- tax_au_year(base_income, other_income, rule = dist_rule)

  # retain lumpy component and tax it at trustee rate
  retained_tax <- trustee_rate * lump_income

  list(
    alloc = dist_res$alloc,
    tax_distributed_base = dist_res$trust_tax,
    tax_retained_lump = retained_tax,
    trust_tax = dist_res$trust_tax + retained_tax
  )
}

# =========================================================
# 7. Lifetime simulation for one family
# =========================================================

simulate_family <- function(
  T = 10,
  n_benef = 4,
  other_income_type = "mixed",
  dist_rule = "tax_min",
  trustee_rate = 0.39,
  base_mean = 80000,
  base_sdlog = 0.25,
  p_lump = 0.20,
  lump_mean = 300000,
  lump_sdlog = 0.90,
  seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  trust_dt <- simulate_trust_income(
    T = T,
    base_mean = base_mean,
    base_sdlog = base_sdlog,
    p_lump = p_lump,
    lump_mean = lump_mean,
    lump_sdlog = lump_sdlog
  )

  other_inc <- make_other_income(
    type = other_income_type,
    n_benef = n_benef,
    T = T
  )

  out <- copy(trust_dt)

  out[, tax_au_equal := 0]
  out[, tax_au_min := 0]
  out[, tax_nz_flat := 0]
  out[, tax_nz_hybrid := 0]

  for (t in 1:T) {
    oi <- other_inc[t, ]

    # Australian equal-share
    res_eq <- tax_au_year(out$trust_income[t], oi, rule = "equal")
    out$tax_au_equal[t] <- res_eq$trust_tax

    # Australian tax-minimising split
    res_min <- tax_au_year(out$trust_income[t], oi, rule = "tax_min")
    out$tax_au_min[t] <- res_min$trust_tax

    # NZ flat trustee tax on all trust income
    out$tax_nz_flat[t] <- tax_trust_flat(out$trust_income[t], trustee_rate)

    # NZ hybrid: distribute regular income, retain lumpy income
    res_hyb <- tax_nz_hybrid_year(
      base_income = out$base_income[t],
      lump_income = out$lump_income[t],
      other_income = oi,
      trustee_rate = trustee_rate,
      dist_rule = dist_rule
    )
    out$tax_nz_hybrid[t] <- res_hyb$trust_tax
  }

  total_income <- sum(out$trust_income)

  summary <- data.table(
    system = c("AU_equal", "AU_tax_min", "NZ_flat_all", "NZ_hybrid"),
    total_tax = c(sum(out$tax_au_equal),
                  sum(out$tax_au_min),
                  sum(out$tax_nz_flat),
                  sum(out$tax_nz_hybrid))
  )

  summary[, etr := total_tax / total_income]
  summary[, total_income := total_income]
  summary[, other_income_type := other_income_type]
  summary[, trustee_rate := trustee_rate]
  summary[, p_lump := p_lump]
  summary[, lump_mean := lump_mean]

  list(
    annual = out,
    summary = summary
  )
}

# =========================================================
# 8. Monte Carlo wrapper
# =========================================================

run_monte_carlo <- function(
  N = 500,
  T = 10,
  n_benef = 4,
  other_income_type = "mixed",
  trustee_rate = 0.39,
  p_lump = 0.20,
  lump_mean = 300000,
  base_mean = 80000,
  dist_rule = "tax_min"
) {
  res_list <- vector("list", N)

  for (i in 1:N) {
    sim <- simulate_family(
      T = T,
      n_benef = n_benef,
      other_income_type = other_income_type,
      dist_rule = dist_rule,
      trustee_rate = trustee_rate,
      p_lump = p_lump,
      lump_mean = lump_mean,
      base_mean = base_mean
    )
    tmp <- copy(sim$summary)
    tmp[, sim_id := i]
    res_list[[i]] <- tmp
  }

  rbindlist(res_list)
}

# =========================================================
# 9. Run scenarios
# =========================================================

scenarios <- CJ(
  other_income_type = c("low", "middle", "high", "mixed"),
  p_lump = c(0.00, 0.10, 0.25, 0.40),
  trustee_rate = c(0.30, 0.39, 0.45)
)

mc <- rbindlist(lapply(1:nrow(scenarios), function(i) {
  s <- scenarios[i]

  tmp <- run_monte_carlo(
    N = 300,
    T = 10,
    n_benef = 4,
    other_income_type = s$other_income_type,
    trustee_rate = s$trustee_rate,
    p_lump = s$p_lump,
    lump_mean = 400000,
    base_mean = 80000
  )

  tmp[, scenario_id := i]
  tmp
}), fill = TRUE)

results <- mc[, .(
  mean_total_tax = mean(total_tax),
  mean_etr = mean(etr),
  p10_etr = quantile(etr, 0.10),
  p90_etr = quantile(etr, 0.90)
), by = .(other_income_type, p_lump, trustee_rate, system)]

# =========================================================
# 10. Comparisons relative to Australian tax-minimising split
# =========================================================

bench <- results[system == "AU_tax_min",
                 .(other_income_type, p_lump, trustee_rate,
                   bench_tax = mean_total_tax,
                   bench_etr = mean_etr)]

comp <- merge(results, bench,
              by = c("other_income_type", "p_lump", "trustee_rate"),
              all.x = TRUE)

comp[, tax_diff_vs_au_min := mean_total_tax - bench_tax]
comp[, etr_diff_vs_au_min := mean_etr - bench_etr]

# =========================================================
# 11. Plots
# =========================================================

ggplot(comp, aes(x = p_lump, y = mean_etr, color = system)) +
  geom_line() +
  geom_point() +
  facet_grid(other_income_type ~ trustee_rate) +
  labs(
    x = "Probability of lumpy income event",
    y = "Mean effective tax rate",
    title = "Effective tax rates under alternative trust tax systems"
  )

ggplot(comp[system != "AU_tax_min"],
       aes(x = p_lump, y = tax_diff_vs_au_min, color = system)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line() +
  geom_point() +
  facet_grid(other_income_type ~ trustee_rate) +
  labs(
    x = "Probability of lumpy income event",
    y = "Mean tax difference relative to AU tax-minimising split",
    title = "Tax advantage/disadvantage relative to Australian annual splitting"
  )
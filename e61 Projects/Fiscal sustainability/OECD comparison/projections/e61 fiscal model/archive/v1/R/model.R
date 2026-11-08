# Independent, deterministic fiscal scenario engine. All monetary levels AUD bn.
# Annual years denote Australian financial years by ending year.
load_inputs <- function(path = 'data') {
  read <- function(f) read.csv(file.path(path, f), check.names = FALSE,
                              stringsAsFactors = FALSE)
  x <- list(official = read('official_pbo_nfo_wide.csv'),
    population = read('population_projection_age_shares.csv'),
    purpose = read('historical_gfs_expenses_by_purpose.csv'),
    operating = read('historical_gfs_operating_statement.csv'),
    programmes = read('programme_parameters.csv'),
    external = read('external_anchors.csv'))
  stopifnot(all(2025:2066 %in% x$population$year), all(2025:2030 %in% x$official$year))
  x
}

default_assumptions <- function() list(productivity = 0.012,
  ngdp_override = FALSE, ngdp_growth = 0.044, relative_price = 0.003,
  tot_change = 0, policy = 'indexed', tax_policy = 'wage_indexed',
  pension_coverage_decline = 0.005, interest_rate = 0.044,
  domestic_inflation = 0.025, tot_income_elasticity = 0.20,
  resource_elasticity = 2, anchor_year = 2030L, end_year = 2066L)

exposures <- function(pop, programmes) {
  ages <- c('0_14', '15_34', '35_54', '55_64', '65_74', '75p')
  weights <- as.matrix(programmes[, paste0('age_', ages)])
  ans <- as.matrix(pop[, ages]) %*% t(weights)
  ans <- ans * pop$pop_total
  colnames(ans) <- programmes$programme
  ans
}

macro_path <- function(inputs, assumptions = default_assumptions()) {
  a <- modifyList(default_assumptions(), assumptions)
  if (a$productivity <= -1 || a$ngdp_growth <= -1 || a$relative_price <= -1 ||
      a$tot_change <= -1) stop('Growth rates must exceed -100%.')
  p <- inputs$population
  p <- p[p$year >= a$anchor_year & p$year <= a$end_year, ]
  # Fixed age employment/hour propensities: a transparent labour-volume proxy.
  # Neither participation projections nor an estimated employment equation.
  labour <- p$pop_total * (0.65*p[['15_34']] + 0.80*p[['35_54']] +
                          0.60*p[['55_64']] + 0.20*p[['65_74']] + 0.03*p[['75p']])
  t <- p$year - a$anchor_year
  productivity_index <- (1 + a$productivity)^t
  real_output <- labour/labour[1] * productivity_index
  tot <- (1 + a$tot_change)^pmin(t/10, 1)
  income_effect <- tot^a$tot_income_elasticity
  gdp0 <- inputs$official$nominal_gdp_billion[inputs$official$year == a$anchor_year]
  if (a$ngdp_override) {
    gdp <- gdp0 * (1+a$ngdp_growth)^t
    prices <- (gdp/gdp0) / (real_output * income_effect)
  } else {
    prices <- (1+a$domestic_inflation)^t
    gdp <- gdp0 * real_output * prices * income_effect
  }
  data.frame(year = p$year, population = p$pop_total, population_index = p$pop_total/p$pop_total[1],
    labour_index = labour/labour[1], productivity_index = productivity_index,
    real_output_index = real_output, domestic_price_index = prices,
    tot_index = tot, income_effect = income_effect, nominal_gdp = gdp,
    nominal_gdp_growth = c(NA, gdp[-1]/gdp[-length(gdp)]-1),
    real_growth = c(NA, real_output[-1]/real_output[-length(real_output)]-1),
    domestic_inflation = c(NA, prices[-1]/prices[-length(prices)]-1),
    # GDP deflator includes the export-price income effect; domestic price proxy does not.
    gdp_deflator_index = prices * income_effect,
    real_income_index = real_output * income_effect,
    government_unit_price_index = prices * productivity_index^0.7 * (1+a$relative_price)^t)
}

calibrate <- function(inputs) {
  # Calibrate once at DEFAULT assumptions. Scenario changes never recalibrate it.
  a <- default_assumptions()
  pp <- inputs$programmes
  pop <- inputs$population[inputs$population$year %in% 2025:2030, ]
  exp <- exposures(pop, pp)
  b <- inputs$purpose[inputs$purpose$year == 2025, ]
  interest <- subset(inputs$operating, year == 2025 & item == 'Interest expenses n.e.c.')$value/1000
  val <- function(label) b$value[b$item == label]/1000
  pension <- inputs$external$value[inputs$external$variable == 'age_pension_outlay']
  base <- c(val('Health'), val('Education'), pension, val('Social Protection')-pension,
            sum(b$value)/1000-interest-val('Health')-val('Education')-val('Social Protection')-val('Defence'),
            val('Defence'))
  # This is an allocation bridge from 2025 GFS to the 2030 PBO aggregate, not an
  # assertion that PBO publishes these programme forecasts.
  real_wage <- (1+a$productivity)^5
  projected <- base * exp[6,]/exp[1,] * (1+a$domestic_inflation)^5 *
    real_wage^pp$wage_pass_through * (1+a$relative_price)^ifelse(pp$kind %in% c('service','mixed'), 5, 0) *
    (1+pp$excess_cost_growth)^5
  off <- inputs$official[inputs$official$year == 2030, ]
  gdp0 <- off$nominal_gdp_billion
  primary <- (off$expenses_ratio_gdp-off$public_debt_interest_ratio_gdp)*gdp0
  scale <- primary/sum(projected)
  pp$base_2025_bn <- base
  pp$anchor_2030_bn <- as.numeric(projected*scale)
  pp$calibration_factor <- scale
  assets <- (off$gross_debt_ratio_gdp-off$net_debt_ratio_gdp)*gdp0
  operating <- subset(inputs$operating, year == 2025)
  interest_share <- operating$value[operating$item == 'Interest income']/
    operating$value[operating$item == 'Total GFS revenue']
  interest_receipts <- gdp0*off$revenue_ratio_gdp*interest_share
  list(programmes = pp, official = off, gdp0 = gdp0,
       gross_debt0 = gdp0*off$gross_debt_ratio_gdp, assets0 = assets,
       interest_receipts0 = interest_receipts, asset_yield = interest_receipts/assets,
       non_interest_revenue0 = gdp0*off$revenue_ratio_gdp-interest_receipts,
       nci0 = gdp0*off$net_capital_investment_ratio_gdp,
       implied_interest_rate = off$public_debt_interest_ratio_gdp*gdp0 /
         (inputs$official$gross_debt_ratio_gdp[inputs$official$year == 2029]*
          inputs$official$nominal_gdp_billion[inputs$official$year == 2029]))
}

project_model <- function(inputs, assumptions = default_assumptions(), calibration = calibrate(inputs)) {
  a <- modifyList(default_assumptions(), assumptions)
  stopifnot(a$anchor_year == 2030, a$end_year <= 2066,
            a$policy %in% c('indexed','standards'), a$tax_policy %in% c('wage_indexed','creep'))
  m <- macro_path(inputs, a)
  p <- inputs$population[match(m$year, inputs$population$year), ]
  pp <- calibration$programmes
  exp <- exposures(p, pp)
  t <- m$year - a$anchor_year
  parts <- vector('list', nrow(pp))
  for (j in seq_len(nrow(pp))) {
    q <- pp[j, ]
    exposure <- exp[,j]/exp[1,j]
    cost <- m$domestic_price_index * m$productivity_index^q$wage_pass_through
    coverage <- rep(1, nrow(m))
    intensity <- (1+q$excess_cost_growth)^t
    if (q$kind %in% c('service','mixed')) cost <- cost*(1+a$relative_price)^t
    if (q$programme == 'Age Pension') {
      # Wage-indexed payment; coverage attrition is an exposed approximation
      # to superannuation/means testing, not a legislated policy simulation.
      if (a$policy == 'indexed') coverage <- (1-a$pension_coverage_decline)^t
      cost <- m$domestic_price_index*m$productivity_index
      intensity <- rep(1, nrow(m))
    }
    if (a$policy == 'standards' && q$kind == 'mixed') {
      cost <- m$domestic_price_index*m$productivity_index*(1+a$relative_price)^t
    }
    value <- q$anchor_2030_bn * exposure * cost * coverage * intensity
    if (q$kind == 'target') {
      share <- approx(c(2030,2035), c(q$anchor_2030_bn/calibration$gdp0,0.025),
                      xout=m$year, rule=2)$y
      value <- m$nominal_gdp*share
    }
    parts[[j]] <- data.frame(year=m$year, programme=q$programme, spending_bn=value,
      exposure_index=exposure, unit_cost_index=cost, coverage_index=coverage,
      intensity_index=intensity, share_gdp=100*value/m$nominal_gdp)
  }
  parts <- do.call(rbind, parts)
  m$primary_expense_bn <- as.numeric(tapply(parts$spending_bn, parts$year, sum))
  # Revenue groups are illustrative allocations of the calibrated non-interest
  # aggregate. They are not claimed to be observed tax-head shares.
  weights <- c(labour=0.42, company=0.20, consumption=0.25, other=0.10, resources=0.03)
  elasticity <- if (a$tax_policy == 'creep') 1.1 else 1
  tax_bases <- cbind(
    labour=m$labour_index*m$domestic_price_index*m$productivity_index^elasticity,
    company=m$nominal_gdp/calibration$gdp0,
    consumption=m$domestic_price_index*m$real_output_index,
    other=m$nominal_gdp/calibration$gdp0,
    resources=m$domestic_price_index*m$real_output_index*m$tot_index^a$resource_elasticity)
  rev <- sweep(tax_bases, 2, weights*calibration$non_interest_revenue0, '*')
  m$non_interest_revenue_bn <- rowSums(rev)
  # Keep financial assets constant per person in domestic purchasing power.
  # This avoids imposing a fixed gross-minus-net wedge as a share of GDP.
  m$financial_assets_bn <- calibration$assets0*m$population_index*m$domestic_price_index
  m$interest_receipts_bn <- c(calibration$interest_receipts0,
                            head(m$financial_assets_bn,-1)*calibration$asset_yield)
  m$revenue_bn <- m$non_interest_revenue_bn+m$interest_receipts_bn
  m$net_capital_investment_bn <- calibration$nci0*m$population_index*
    m$domestic_price_index*m$productivity_index^0.5
  # Asset acquisition is explicit: budget net lending alone does not determine gross debt.
  m$net_financial_asset_acquisition_bn <- c(0,diff(m$financial_assets_bn))
  m$effective_interest_rate <- approx(c(2030,2040),
    c(calibration$implied_interest_rate,a$interest_rate),xout=m$year,rule=2)$y
  m$interest_bn <- m$gross_debt_bn <- m$net_debt_bn <- rep(NA_real_,nrow(m))
  m$interest_bn[1] <- calibration$official$public_debt_interest_ratio_gdp*calibration$gdp0
  m$gross_debt_bn[1] <- calibration$gross_debt0
  m$net_debt_bn[1] <- m$gross_debt_bn[1]-m$financial_assets_bn[1]
  m$stock_flow_adjustment_bn <- rep(0,nrow(m))
  for (i in 2:nrow(m)) {
    m$interest_bn[i] <- m$effective_interest_rate[i]*m$gross_debt_bn[i-1]
    borrowing <- m$primary_expense_bn[i]+m$interest_bn[i]+m$net_capital_investment_bn[i]-m$revenue_bn[i]
    gross_unconstrained <- m$gross_debt_bn[i-1]+borrowing+m$net_financial_asset_acquisition_bn[i]
    # A surplus after debt repayment accumulates in interest-bearing assets.
    if (gross_unconstrained < 0) {
      extra <- -gross_unconstrained
      m$financial_assets_bn[i:nrow(m)] <- m$financial_assets_bn[i:nrow(m)]+extra
      m$net_financial_asset_acquisition_bn[i] <- m$net_financial_asset_acquisition_bn[i]+extra
      if (i < nrow(m)) {
        m$interest_receipts_bn[(i+1):nrow(m)] <- m$interest_receipts_bn[(i+1):nrow(m)]+extra*calibration$asset_yield
        m$revenue_bn[(i+1):nrow(m)] <- m$revenue_bn[(i+1):nrow(m)]+extra*calibration$asset_yield
      }
    }
    m$gross_debt_bn[i] <- max(0,gross_unconstrained)
    m$net_debt_bn[i] <- m$gross_debt_bn[i]-m$financial_assets_bn[i]
  }
  m$total_spending_bn <- m$primary_expense_bn+m$interest_bn+m$net_capital_investment_bn
  m$fiscal_balance_bn <- m$revenue_bn-m$total_spending_bn
  m$primary_balance_bn <- m$non_interest_revenue_bn-m$primary_expense_bn-m$net_capital_investment_bn
  for (name in c('primary_expense','interest','revenue','total_spending','fiscal_balance',
                 'primary_balance','gross_debt','net_debt','net_capital_investment')) {
    m[[paste0(name,'_pct')]] <- 100*m[[paste0(name,'_bn')]]/m$nominal_gdp
  }
  m$real_primary_per_person <- m$primary_expense_bn*1e9/m$domestic_price_index/m$population
  revenue <- data.frame(year=m$year,rev,interest_receipts=m$interest_receipts_bn,check.names=FALSE)
  list(path=m,components=parts,revenue=revenue,assumptions=a,calibration=calibration)
}

scenario_attribution <- function(inputs, assumptions, calibration=calibrate(inputs)) {
  # Exact Shapley allocation of all interactions across the four economic controls.
  # Policy settings common to both cases; nothing here is a causal estimate.
  base <- modifyList(default_assumptions(), assumptions[c('policy','tax_policy','pension_coverage_decline','interest_rate')])
  groups <- list('Productivity'='productivity', 'NGDP override'=c('ngdp_override','ngdp_growth'),
                 'Government service prices'='relative_price','Terms of trade'='tot_change')
  values <- numeric(16)
  for (mask in 0:15) {
    z <- base
    for (j in 1:4) if (bitwAnd(mask,bitwShiftL(1,j-1)) != 0) z[names(assumptions[groups[[j]]])] <- assumptions[groups[[j]]]
    values[mask+1] <- tail(project_model(inputs,z,calibration)$path$net_debt_pct,1)
  }
  contributions <- sapply(1:4,function(j) {
    bit <- bitwShiftL(1,j-1)
    masks <- (0:15)[bitwAnd(0:15,bit)==0]
    sum(sapply(masks,function(mask) {
      k <- sum(as.integer(intToBits(mask))[1:4])
      factorial(k)*factorial(3-k)/factorial(4)*(values[mask+bit+1]-values[mask+1])
    }))
  })
  data.frame(driver=names(groups),net_debt_change_pp=contributions)
}

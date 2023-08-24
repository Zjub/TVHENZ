# Note, additional information compared to base calculator - so add to initial data file for run
LITO_thresh_1 <- 37500
LITO_thresh_2 <- 45000
LITO_gross <- 700
LITO_rate_1 <- 0.005
LITO_rate_2 <- 0.015
LMITO_on <- 1
LMITO_thresh_1 <- 37000
LMITO_thresh_2 <- 48000
LMITO_thresh_3 <- 90000
LMITO_gross <- 255
LMITO_inc_rate <- 0.075
LMITO_max <- 1080
LMITO_dec_rate <- 0.03
BRL_on <- 1
BRL_thresh <- 180000
BRL_rate <- 0.02
BTO_thresh <- 6000
BTO_rate <- 0.15

# Function for calculating income tax
# Includes Low Income Tax Credit, Low and Middle Income Tax Credit, Temporary Budget Repair Levy, and Beneficiary Tax offset.

calc_income_tax <- function(gross_income,work_income,LITO_thresh_1,LITO_thresh_2,LITO_gross,LITO_rate_1,LITO_rate_2) {
  tax_nc <- 0
  tempinc <- gross_income
  for (i in length(tax_brackets):2) {
    if (gross_income > tax_brackets[i]) {
      tax_nc <- tax_nc + (tempinc - tax_brackets[i]) * tax_scale[i]
      tempinc <- tax_brackets[i]
    }
  }

  LITO_credit <- 0
  if (LITO_on == 1){
    LITO_credit <- max(0,LITO_gross - max(0,(gross_income - LITO_thresh_1)*LITO_rate_1) - max(0,(gross_income - LITO_thresh_2)*LITO_rate_2))
  }

  LMITO_credit <- 0
  if (LMITO_on == 1){
    LMITO_credit <- ifelse(gross_income <= LMITO_thresh_1,LMITO_gross,ifelse(gross_income <= LMITO_thresh_2,LMITO_gross + (gross_income-LMITO_thresh_1)*LMITO_inc_rate,ifelse(gross_income <- LMITO_thresh_3,LMITO_max,max(0,LMITO_max - (gross_income - LMITO_thresh_3)*LMITO_dec_rate))))
  }

  BRL <- 0
  if (BRL_on == 1){
    BRL <- max(0,(gross_income - BRL_thresh)*BRL_rate)
  }

  BTO_credit <- 0
  benefit_paid <- calc_benefit_abated(work_income,partner_earnings)[["net_benefit"]]*26
  if (benefit_paid > BTO_thresh){
    BTO_credit <- BTO_rate*(benefit_paid - BTO_thresh)
  }
  tax <- max(0,(tax_nc + BRL - LITO_credit - LMITO_credit - BTO_credit))
  return(tax)
}

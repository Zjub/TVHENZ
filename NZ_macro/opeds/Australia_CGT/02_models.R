# All hypothetical experiments live here. No model output is an empirical forecast.
req <- c('dplyr','tidyr','readr','purrr')
for (p in req) if (!requireNamespace(p,quietly=TRUE)) stop('Missing R package: ',p)
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(readr);library(purrr)})
arg <- grep('^--file=',commandArgs(FALSE),value=TRUE)
root <- if(length(arg)) dirname(normalizePath(sub('^--file=','',arg[1]))) else normalizePath('Australia_CGT')
out <- file.path(root,'processed'); dir.create(out,showWarnings=FALSE)
write <- function(x,name) write_csv(x,file.path(out,paste0(name,'.csv')),na='')
read <- function(name) read_csv(file.path(out,paste0(name,'.csv')),show_col_types=FALSE)
checks <- list()
check <- function(name,ok,detail='') {
  checks[[length(checks)+1L]] <<- tibble(check=name,passed=isTRUE(ok),detail=detail)
  if(!isTRUE(ok)) stop('Validation failed: ',name,' ',detail)
}
# Nominal flows; annual compounding; no fees unless explicitly specified.
# RRA is risk-free nominal allowance, NOT expected risky return or CPI plus a risk premium.
P <- 100000; pi <- .025; r <- .03; allowance <- (1+pi)*(1+r)-1; tau <- .30
params <- tibble(parameter=c('snapshot','principal','inflation','normal_real_return','normal_nominal_allowance','flat_tax_rate','illustrative_excess_real_return','consumption_tax_equivalent_rate','personal_schedule_year'),
 value=c('2026-11-11',P,pi,r,allowance,tau,.04,tau/(1-tau),'2027-28'),
 interpretation=c('Source cut-off; not a prediction',rep('Hypothetical assumption',7),'Frozen future resident scale; excludes Medicare, offsets, HELP and transfers'))
write(params,'parameters')
# Basic tax only. The 2027-28 lowest positive bracket is 14%; other thresholds
# are nominal and frozen across all years of each thought experiment.
tax <- function(y) .14*pmax(0,pmin(y,45000)-18200)+.30*pmax(0,pmin(y,135000)-45000)+.37*pmax(0,pmin(y,190000)-135000)+.45*pmax(0,y-190000)
new_tax <- function(base,gain,exempt=FALSE) {
  ordinary <- tax(base+pmax(gain,0))-tax(base)
  ordinary + (!exempt)*pmax(0,.30*pmax(gain,0)-ordinary)
}
check('2027 basic tax at 45000',abs(tax(45000)-3752)<1e-8)
check('2027 basic tax at 190000',abs(tax(190000)-51102)<1e-8)
write(tibble(threshold=c(0,18200,45000,135000,190000),marginal_rate=c(0,.14,.30,.37,.45),schedule='2027-28 basic resident tax only'),'personal_tax_schedule')

# Datasets: uncertainty, income ranking, and coverage checks.
cgt <- read('ato_cgt_items'); cells<-read('individual_cells'); hist<-read('individual_history'); pct<-read('individual_percentiles')
check('ATO CGT table years',setequal(cgt$year,2012:2023))
check('ATO CGT entities',setequal(unique(cgt$entity),c('Individuals','Companies','Funds')))
check('Individual cells have no subtotal categories',nrow(cells)==1327 && setequal(unique(cells$sex),c('Female','Male')))
asset <- cgt |> filter(section=='Current year capital gains',unit=='$',!grepl('Total current|previously deferred',item))
recon<- asset |> group_by(year,entity) |> summarise(parts=sum(value),.groups='drop') |>
 left_join(cgt |> filter(item=='Total current year capital gains',unit=='$') |> select(year,entity,total=value),by=c('year','entity'))
# Funds have a separately identified release of deferred 2017 super gains.
deferred<-cgt |> filter(grepl('previously deferred',item),unit=='$') |> select(year,entity,deferred=value)
recon<-recon |> left_join(deferred,by=c('year','entity')) |> mutate(gap=total-parts-coalesce(deferred,0))
# Published totals and component sums do NOT reconcile exactly. Preserve and
# display the difference, rather than normalising components to force equality.
check('Published asset reconciliation differences measured',all(is.finite(recon$gap)) && max(abs(recon$gap/recon$total))<.12,paste('Max absolute difference / total:',round(max(abs(recon$gap/recon$total)),4)))
write(recon,'asset_reconciliation')
individual_total<-hist |> filter(year==2023,item=='Net capital gain',unit=='$') |> pull(value)
check('Individual cell gains reconcile to history',abs(sum(cells$net_gain)-individual_total)<100)
check('No double counting ATO percentile state-sex cells',nrow(pct)==1800 && all(table(pct$percentile)==18))
check('Taxpayer percentiles cover 1 to 100',setequal(pct$percentile,1:100))
br<-read('budget_reform');check('Budget combined measure receipts anchor',sum(br$receipts_m)==3630)
check('Budget combined measure payments anchor',abs(sum(br$payments_m)-99)<1e-8)
rent<-read('rental_schedules')
check('Rental tables cover 12 years and eight states',setequal(rent$year,2012:2023) && setequal(rent$state,c('NSW','QLD','VIC','WA','SA','ACT','TAS','NT')))
check('Rental aggregation categories are disjoint',all(rent$position[rent$year==2012]=='All') && setequal(rent$position[rent$year>2012],c('Loss','Neutral/Profit')))
cash<-read('budget_cgt_receipts')
check('CGT cash receipts actual anchor',cash$cgt_m[cash$year==2024]==30400)
check('PBO rounded medium-term combined receipts',abs(sum(read('pbo_measures') |>filter(grepl('gearing',measure)) |>pull(value_bn))-41)<1e-8)
te<-read('teis_discount_income');check('TEIS rounded shares retained',sum(te$benefit_share_pct)==100 && tail(te$benefit_share_pct,1)==83)

# 1. Time value: same pre-tax labour resource, matched tax-exclusive/inclusive rates.
time <- crossing(year=0:40,regime=c('No tax','Comprehensive nominal income tax','Comprehensive real income tax','Labour tax + normal-return allowance','Consumption tax')) |>
 mutate(nominal_return=allowance,
 wealth_nominal=case_when(regime=='No tax'~P*(1+allowance)^year,
 regime=='Comprehensive nominal income tax'~P*(1-tau)*(1+allowance*(1-tau))^year,
 regime=='Comprehensive real income tax'~P*(1-tau)*((1+pi)*(1+r*(1-tau)))^year,
 TRUE~P*(1-tau)*(1+allowance)^year),
 real_consumption=wealth_nominal/(1+pi)^year,
 relative_to_consuming_now=real_consumption/if_else(regime=='No tax',P,P*(1-tau)))
write(time,'normal_return_consumption')
check('RRA and consumption-tax path match at normal return',max(abs(time$real_consumption[time$regime=='Consumption tax']-time$real_consumption[time$regime=='Labour tax + normal-return allowance']))<1e-8)
write(tibble(income_tax_rate=seq(0,.6,.01)) |> mutate(equivalent_gst_rate=income_tax_rate/(1-income_tax_rate)),'tax_rate_mapping')
components<-tibble(component=c('Inflation compensation','Real normal return','Excess return'),value=c(P*pi,P*((1+pi)*(1+r)-1-pi),P*((1+pi)*(1+r+.04)-(1+pi)*(1+r))))
write(components,'return_components')

# Exact two-date PV identity for new investment: normal-allowance tax on
# terminal surplus vs cash-flow tax with refundable deduction at inception.
equiv <- crossing(year=c(1,5,10,20,30),real_return=c(0,.01,.03,.05,.08,.12)) |>
 mutate(terminal=P*((1+pi)*(1+real_return))^year,normal_basis=P*(1+allowance)^year,
 rra_tax_exit=tau*(terminal-normal_basis),rra_tax_pv=rra_tax_exit/(1+allowance)^year,
 cashflow_initial=-tau*P,cashflow_exit=tau*terminal,
 cashflow_tax_pv=cashflow_initial+cashflow_exit/(1+allowance)^year,
 indexed_tax_pv=tau*(terminal-P*(1+pi)^year)/(1+allowance)^year)
write(equiv,'rra_cashflow_equivalence')
check('RRA cash-flow PV identity including negative returns',max(abs(equiv$rra_tax_pv-equiv$cashflow_tax_pv))<1e-8)

allow <- crossing(year=0:40,method=c('Original nominal cost','CPI-adjusted cost','Compounded normal-return basis')) |>
 mutate(basis=P*case_when(method=='Original nominal cost'~1,method=='CPI-adjusted cost'~(1+pi)^year,TRUE~(1+allowance)^year))
write(allow,'allowance_bases')
split<-crossing(cash_yield=seq(0,.08,.002),method=c('50% discount','CPI indexation','Comprehensive RRA')) |>
 mutate(total_return=.08,appreciation=total_return-cash_yield,
 tax=case_when(method=='50% discount'~tau*P*(cash_yield+.5*appreciation),method=='CPI indexation'~tau*P*(cash_yield+pmax(appreciation-pi,0)),TRUE~tau*P*(total_return-allowance)))
write(split,'return_form_neutrality')
sense<-crossing(actual_real_return=seq(0,.12,.002),allowance_real=c(.01,.03,.05)) |>
 mutate(taxable_excess_real=actual_real_return-allowance_real,tax_real=tau*P*taxable_excess_real,allowance_label=paste0(100*allowance_real,'% real allowance'))
write(sense,'allowance_sensitivity')
# Pure-gain asset: unused allowances compound at the allowance rate, not lost.
bank<-crossing(year=0:30,method=c('Unused allowance compounds','Unused allowance adds without interest')) |>
 mutate(normal_terminal=P*(1+allowance)^year,
 basis=if_else(method=='Unused allowance compounds',normal_terminal,P*(1+allowance*year)),tax_on_normal_return=tau*(normal_terminal-basis)/(1+pi)^year)
write(bank,'unused_allowance')

# 2. CGT: fresh eligible asset, no pre-2027 gains, all returns capital gains.
gains<-crossing(nominal_growth=seq(.001,.15,.001),years=c(2,5,10,20,30),base_income=c(0,70000,200000)) |>
 mutate(cost=500000,inflation=pi,sale=cost*(1+nominal_growth)^years,
 nominal_gain=sale-cost,indexed_gain=pmax(0,sale-cost*(1+pi)^years),
 real_gain_at_sale=sale-cost*(1+pi)^years,
 allowance_gain=sale-cost*(1+allowance)^years,
 tax_discount=tax(base_income+.5*nominal_gain)-tax(base_income),
 tax_nominal=tax(base_income+nominal_gain)-tax(base_income),
 tax_index=tax(base_income+indexed_gain)-tax(base_income),
 tax_reform=new_tax(base_income,indexed_gain),
 tax_rra_symmetric=tax(base_income+pmax(allowance_gain,0))-tax(base_income)-tau*pmax(-allowance_gain,0),
 reform_change=tax_reform-tax_discount,
 real_etr_old=if_else(real_gain_at_sale>1,tax_discount/real_gain_at_sale,NA_real_),
 real_etr_reform=if_else(real_gain_at_sale>1,tax_reform/real_gain_at_sale,NA_real_))
write(gains,'cgt_scenarios')
check('CGT reform no tax when positive nominal gain below inflation',all(gains$tax_reform[gains$nominal_growth<pi]==0))
check('Minimum tax is at least 30% eligible indexed gains',all(gains$tax_reform+.0001>=.3*gains$indexed_gain))
check('No effective rate defined at nonpositive real gain',all(is.na(gains$real_etr_old[gains$real_gain_at_sale<=1])))
minimum<-crossing(base_income=seq(0,250000,2500),gain=c(10000,50000,200000),exception=c(FALSE,TRUE)) |>
 mutate(basic_increment=tax(base_income+gain)-tax(base_income),tax=new_tax(base_income,gain,exception),average_gain_rate=tax/gain,case=if_else(exception,'Specified-payment exemption','Subject to minimum'))
write(minimum,'minimum_tax')
lumpy<-crossing(years=1:30,base_income=c(0,70000,150000),annual_real_gain=c(5000,15000)) |>
 mutate(total_real_gain=years*annual_real_gain,
 lump_tax=tax(base_income+total_real_gain)-tax(base_income),
 averaged_tax=years*(tax(base_income+annual_real_gain)-tax(base_income)),
 lump_minimum=new_tax(base_income,total_real_gain),
 tax_bunching_extra=lump_tax-averaged_tax)
write(lumpy,'income_averaging')
# Transition illustration with a supplied valuation; not Commissioner apportionment.
transition<-crossing(pre_years=c(0,5,10,20),post_years=1:20) |>
 mutate(initial=500000,nominal_growth=.06,reset=initial*(1+nominal_growth)^pre_years,
 sale=reset*(1+nominal_growth)^post_years,old_gain=pmax(reset-initial,0),new_gain=pmax(sale-reset*(1+pi)^post_years,0),
 old_taxable=.5*old_gain,base_income=70000,
 transition_tax=tax(base_income+old_taxable+new_gain)-tax(base_income)+pmax(0,.30*new_gain-(tax(base_income+old_taxable+new_gain)-tax(base_income+old_taxable))),
 hypothetical_all_new=new_tax(base_income,pmax(sale-initial*(1+pi)^(pre_years+post_years),0)),
 old_rules_all=tax(base_income+.5*(sale-initial))-tax(base_income))
write(transition,'transition_illustration')

# 3. Deferral, loss treatment, leverage, imputation.
deferral<-crossing(years=1:40,real_return=c(.03,.05,.08),method=c('Annual real-income tax','Real CGT at exit','Half-rate real CGT at exit','RRA at exit')) |>
 mutate(initial=100000,
 aftertax_real=case_when(method=='Annual real-income tax'~initial*(1+real_return*(1-tau))^years,
 method=='Real CGT at exit'~initial*((1+real_return)^years-tau*((1+real_return)^years-1)),
 method=='Half-rate real CGT at exit'~initial*((1+real_return)^years-.5*tau*((1+real_return)^years-1)),
 TRUE~initial*((1+real_return)^years-tau*((1+real_return)^years-(1+r)^years))),
 effective_annual_real_return=(aftertax_real/initial)^(1/years)-1)
write(deferral,'deferral')
# Sell and reinvest or keep embedded gain for ten further years; full real CGT.
switch<-crossing(embedded_gain_share=c(.1,.3,.5,.7),alternative_return=seq(.02,.14,.001)) |>
 mutate(current_return=.05,horizon=10,current_value=100000,basis=current_value*(1-embedded_gain_share),
 keep_net=current_value*(1+current_return)^horizon-tau*(current_value*(1+current_return)^horizon-basis),
 reinvest=current_value-tau*(current_value-basis),
 switch_net=reinvest*(1+alternative_return)^horizon-tau*reinvest*((1+alternative_return)^horizon-1),difference=switch_net-keep_net)
write(switch,'lock_in')
loss<-crossing(delay=0:30,discount_rate=c(.02,.05,.08),loss=10000) |>
 mutate(immediate_refund=tau*loss,deferred_relief_pv=immediate_refund/(1+discount_rate)^delay,relief_loss=immediate_refund-deferred_relief_pv)
write(loss,'loss_delay')
risk<-crossing(treatment=c('No tax','Symmetric tax, fixed scale','Symmetric tax, restored scale','No loss relief, restored upside'),state=c('Success','Failure')) |>
 mutate(gross=if_else(state=='Success',200000,-100000),scale=if_else(treatment %in% c('Symmetric tax, restored scale','No loss relief, restored upside'),1/(1-tau),1),
 payoff=case_when(treatment=='No tax'~gross,treatment=='No loss relief, restored upside'~scale*if_else(gross>0,gross*(1-tau),gross),TRUE~gross*scale*(1-tau)))
write(risk,'risk_sharing')
check('Symmetric scaled tax restores both state payoffs',all(abs(risk$payoff[risk$treatment=='No tax']-risk$payoff[risk$treatment=='Symmetric tax, restored scale'])<1e-8))
risk_lotto<-crossing(prob_success=seq(.1,.9,.02),system=c('No tax','Symmetric tax','No loss relief')) |>
 mutate(initial_wealth=300000,success=200000,failure=-100000,
 up=if_else(system=='No tax',success,success*(1-tau)),down=if_else(system=='Symmetric tax',failure*(1-tau),failure),
 expected_payoff=prob_success*up+(1-prob_success)*down,
 certainty_equivalent=exp(prob_success*log(initial_wealth+up)+(1-prob_success)*log(initial_wealth+down))-initial_wealth)
write(risk_lotto,'risk_certainty_equivalent')
leverage<-crossing(ltv=seq(0,.9,.01),method=c('No tax','Discount + full nominal interest deduction','Indexed gains + full nominal interest deduction','RRA equity + symmetric net return tax')) |>
 mutate(asset=500000,growth=.04,rent_net=.025,interest=.065,debt=asset*ltv,equity=asset-debt,
 pretax=asset*(growth+rent_net)-interest*debt,
 tax=case_when(method=='No tax'~0,method=='Discount + full nominal interest deduction'~tau*(asset*(.5*growth+rent_net)-interest*debt),method=='Indexed gains + full nominal interest deduction'~tau*(asset*(pmax(growth-pi,0)+rent_net)-interest*debt),TRUE~tau*(pretax-allowance*equity)),roe=(pretax-tax)/equity)
write(leverage,'leverage_one_year')
deduction<-crossing(inflation=seq(0,.08,.001),ltv=c(0,.5,.8)) |>
 mutate(asset=500000,real_interest=.03,nominal_interest=(1+inflation)*(1+real_interest)-1,
 debt=ltv*asset,nominal_interest_deduction=debt*nominal_interest,
 real_interest_deduction=debt*(nominal_interest-inflation),extra_nominal_deduction_tax=tau*debt*inflation)
write(deduction,'interest_inflation_symmetry')
# One-period imputation example: common pre-company-tax profit, full payout,
# all credits usable. Retention/foreign ownership/capitalisation are not assumed away
# in interpretation: this exact identity only tests the stated simple case.
imputation<-crossing(company_rate=c(.25,.30),personal_rate=c(.15,.30,.45),profit=10000) |>
 mutate(company_tax=company_rate*profit,cash_dividend=profit-company_tax,credit=company_tax,
 personal_net=personal_rate*profit-credit,total_tax=company_tax+personal_net,
 without_credit=company_tax+personal_rate*cash_dividend)
write(imputation,'imputation')
check('Imputation fully credits one-period company tax',max(abs(imputation$total_tax-imputation$personal_rate*imputation$profit))<1e-8)
existing<-crossing(year=0:30,origin=c('Existing after-tax wealth','New pre-tax earnings'),system=c('Consumption tax introduced','Labour tax + RRA introduced')) |>
 mutate(initial=100000,
 consumption=case_when(origin=='Existing after-tax wealth' & system=='Labour tax + RRA introduced'~initial*(1+r)^year,TRUE~initial*(1-tau)*(1+r)^year))
write(existing,'existing_wealth_transition')
real_loss<-crossing(nominal_return=seq(-.10,.10,.001),system=c('Symmetric real-income benchmark','Indexed gains; nominal loss relief after 10 years','Indexed gains; loss never usable')) |>
 mutate(principal=100000,sale=principal*(1+nominal_return),indexed_basis=principal*(1+pi),real_gain=sale-indexed_basis,
 tax=case_when(system=='Symmetric real-income benchmark'~tau*real_gain,
 system=='Indexed gains; nominal loss relief after 10 years'~tau*pmax(real_gain,0)-tau*pmax(principal-sale,0)/(1+allowance)^10,
 TRUE~tau*pmax(real_gain,0)))
write(real_loss,'real_losses')
labour_gain<-crossing(own_work_hours=seq(0,1000,50),method=c('50% nominal discount','CPI indexation','Normal-return allowance')) |>
 mutate(cost=500000,cash_improvement=50000,own_hour_value=60,market_return=.06,years=2,
 sale=(cost+cash_improvement)*(1+market_return)^years+own_work_hours*own_hour_value,
 basis=cost+cash_improvement,
 gain=case_when(method=='50% nominal discount'~.5*(sale-basis),method=='CPI indexation'~sale-basis*(1+pi)^years,TRUE~sale-basis*(1+allowance)^years),tax=tau*pmax(gain,0))
write(labour_gain,'own_work_gains')

# 4. International evidence: preserve definitions, horizons and identification.
# Values are hand-transcribed from the linked original authors/official coster,
# NOT pooled in a meta estimate. Ranges are specification ranges, not CIs.
literature<-tribble(
 ~study,~country,~definition,~estimate,~low,~high,~reference_rate,~horizon,~kind,~source_id,~locator,~caveat,
 'Minas, Lim & Evans (2018)','Australia','Tax-rate semi-elasticity',-1.75,-1.75,-1.75,.3675,'Annual time series 1988-89 to 2014-15','Empirical','australia_study','Authors Table 1','27 observations; macro confounding; main specification, not CRS alternative 2.15',
 'Agersnap & Zidar (2021), baseline','United States','Net-of-tax elasticity',1.87,1.87,1.87,.22,'Average years 0-10','Empirical','agersnap_zidar','Table 2; baseline policy elasticity','Adjusted for interstate migration; 1.87 is not a tax-rate elasticity',
 'Agersnap & Zidar (2021), alternatives','United States','Tax-rate elasticity',-.42,-.53,-.29,.22,'Average years 0-10','Empirical','agersnap_zidar','Table 2 / discussion section 3','Specification range, not CI; endpoint of working version differs from abstract rounding',
 'Dowd & McClelland (2024), added lags','United States','Tax-rate elasticity',-.59,-.59,-.41,NA_real_,'Beyond current year; lag sensitivity','Empirical','dowd_2024','Authors abstract and specification results','Native evaluation rate; do not force to a 22% tax rate without coefficient',
 'Lavecchia & Tazhitdinova, WP2021 short run','Canada','Net-of-tax elasticity',3.23,2.85,3.61,NA_real_,'Short run exemption cancellation','Empirical','canada','Introduction pp.3-4','Working-paper version; exemption repeal and tax-free crystallisation; range not CI',
 'Lavecchia & Tazhitdinova, WP2021 longer run','Canada','Net-of-tax elasticity',-.64,-1.12,-.16,NA_real_,'Medium and long run','Empirical','canada','Introduction pp.3-4','Wealth effects of losing exemption; not general evidence tax rises stimulate investment',
 'OBR (2025), main rates','United Kingdom','Net-of-tax elasticity',3.6,3.6,3.6,.24,'Costing horizon','Scoring assumption','obr','Paragraph 1.9; Table 1.1','Combined behaviours; 24% illustrative conversion rate, not empirical reference rate',
 'OBR (2025), business relief','United Kingdom','Net-of-tax elasticity',1.4,1.4,1.4,.18,'Costing horizon','Scoring assumption','obr','Paragraph 1.9; Table 1.1','18% illustrative conversion rate; distinct population and relief')
urls<-c(australia_study='https://www.austaxpolicy.com/tax-rate-changes-impact-capital-gains-realisations-evidence-australia/',agersnap_zidar='https://www.nber.org/system/files/working_papers/w27705/w27705.pdf',dowd_2024='https://taxpolicycenter.org/publications/sensitivity-tax-elasticity-capital-gains-lagged-tax-rates-and-migration',canada='https://www.nber.org/system/files/working_papers/w28514/w28514.pdf',obr='https://obr.uk/docs/dlm_uploads/CGT-supplementary-release-Jan-2025.pdf')
literature$url<-unname(urls[literature$source_id]);literature$retrieval_method<-if_else(literature$source_id %in% c('obr','dowd_2024'),'Web-tool verified; direct download denied','Downloaded original/author source')
literature$point_type<-if_else(literature$source_id=='canada','Range midpoint for display only; not a reported point estimate','Reported point / selected specification')
literature<-literature |> mutate(tax_elasticity=case_when(definition=='Tax-rate semi-elasticity'~estimate*reference_rate,definition=='Net-of-tax elasticity'~ -estimate*reference_rate/(1-reference_rate),TRUE~estimate))
write(literature,'literature')
check('Net-of-tax to tax-rate conversion',abs(literature$tax_elasticity[2]+1.87*.22/.78)<1e-12)

# Same local tax-rate elasticity at 22%, two finite-change functional forms.
behaviour<-crossing(tax_rate=seq(.05,.65,.005),elasticity_at_22=c(0,-.2,-.4,-.7,-1),form=c('Constant semi-elasticity','Constant net-of-tax elasticity')) |>
 mutate(reference_rate=.22,beta=elasticity_at_22/.22,net_elasticity=-elasticity_at_22*.78/.22,
 gains_index=100*if_else(form=='Constant semi-elasticity',exp(beta*(tax_rate-.22)),((1-tax_rate)/.78)^net_elasticity),revenue_index=tax_rate*gains_index/.22,
 elasticity_label=paste0('Local elasticity ',elasticity_at_22))
write(behaviour,'behaviour_scenarios')
check('Behaviour no response retains base',all(abs(behaviour$gains_index[behaviour$elasticity_at_22==0]-100)<1e-8))
finite<-crossing(elasticity=seq(-1.5,0,.025),new_rate=c(.25,.30,.40)) |>
 mutate(old_rate=.22,gain_base=100,gains_after=gain_base*exp((elasticity/old_rate)*(new_rate-old_rate)),static_gain=(new_rate-old_rate)*gain_base,behavioural_gain=new_rate*gains_after-old_rate*gain_base,fraction_static=behavioural_gain/static_gain)
write(finite,'finite_revenue_change')
timing<-crossing(year=-2:8,case=c('No timing response','Bring forward 30; unwind over 3 years')) |>
 mutate(baseline_gains=100,shift=if_else(case=='No timing response',0,case_when(year==-1~30,year %in% 0:2~ -10,TRUE~0)),gains=baseline_gains+shift,rate=if_else(year<0,.22,.30),receipts=rate*gains)
write(timing,'retiming')
check('Timing-only gains conserved over complete window',sum(timing$shift)==0)
shifting<-crossing(disappearing_cgt=seq(0,100,2),share_reclassified=c(0,.25,.5,1)) |>
 mutate(cgt_rate=.30,income_rate=.45,cgt_receipt_change=-cgt_rate*disappearing_cgt,income_receipt_change=income_rate*disappearing_cgt*share_reclassified,total_receipt_change=cgt_receipt_change+income_receipt_change)
write(shifting,'income_reclassification')

# Selected numeric anchors for prose, with data/scenario labels and caveats.
latest<-asset |> filter(year==2023,entity=='Individuals'); gross<-cgt |> filter(year==2023,entity=='Individuals',item=='Total current year capital gains',unit=='$') |> pull(value)
findings<-tribble(~measure,~value,~unit,~period,~status,~caveat,
 'Individual reported gross current-year gains',gross/1e9,'AUD billion','2023-24','ATO observation','Associated-form reporting; not all accrued gains; includes trust-origin gains',
 'Direct Australian real-estate share of individual gross gains',100*latest$value[latest$item=='Real estate situated in Australia']/gross,'percent','2023-24','ATO observation','All Australian real estate, not just residential; no trust look-through',
 'Trust-origin share of individual gross gains',100*latest$value[grepl('from a trust',latest$item)]/gross,'percent','2023-24','ATO observation','Underlying property/share composition unknown',
 'Combined CGT and negative-gearing gross receipts',sum(br$receipts_m)/1000,'AUD billion','2025-26 to 2029-30','Budget estimate','Announcement vintage; not isolated CGT; .. rounded to zero for plotting',
 'Combined measure related payments',sum(br$payments_m),'AUD million','2025-26 to 2029-30','Budget estimate','Published totals include rounding',
 'RRA nominal allowance',100*allowance,'percent per year','Scenario','Illustration','2.5% inflation and 3% real normal return; not chosen policy setting',
 'Consumption tax matching 30% income-inclusive rate',100*tau/(1-tau),'percent tax-exclusive','Scenario','Identity','Equivalent new-resource timing example; not Australia GST rate',
 'Tax on normal return, symmetric RRA',0,'AUD','Scenario','Identity','Correct allowance and usable/capitalised relief assumed')
write(findings,'key_findings');write(bind_rows(checks),'validation')
message('Saved models and ',length(checks),' passing source/accounting checks.')

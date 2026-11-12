# Same initial labour resource, asset-neutral pre-tax total return, ten years.
# Run directly: Rscript Australia_CGT/ten_year_example/01_compare.R
# Hypothetical systems, not a simulation of Australian statutory liabilities.
req<-c('dplyr','tidyr','readr','ggplot2','svglite','scales')
for(p in req)if(!requireNamespace(p,quietly=TRUE))stop('Missing package: ',p)
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(readr);library(ggplot2)})
args<-grep('^--file=',commandArgs(FALSE),value=TRUE)
root<-if(length(args))dirname(normalizePath(sub('^--file=','',args[1])))else normalizePath('Australia_CGT/ten_year_example')
for(dir in c('data','figures'))dir.create(file.path(root,dir),recursive=TRUE,showWarnings=FALSE)
save<-function(d,name)write_csv(d,file.path(root,'data',paste0(name,'.csv')),na='')
L<-100000; t<-0.30; inflation<-0.025; normal_real<-0.03
a<-(1+inflation)*(1+normal_real)-1; T<-10; gst<-t/(1-t)
assets<-c('Bank deposit','Domestic company','House: capital appreciation','New business: capital appreciation')
systems<-c('Full-rate income tax','Comprehensive normal-return allowance','CGT inflation allowance only','Broad consumption tax')
tax_events<-list()

# cash_share is the fraction of the nominal total return paid as current income.
# Capital appreciation is the rest; net current income is reinvested at year end.
# The company earns current taxable profits even when it retains the cash.
sim<-function(asset,system,nominal_return=a,years=T,resource=L,cash_share=0,
             company_relief=TRUE,embedded_labour=FALSE,collect=FALSE){
  is_gst<-system=='Broad consumption tax'
  is_rra<-system=='Comprehensive normal-return allowance'
  annual<-asset %in% c('Bank deposit','Domestic company')
  lambda<-if(annual)1 else cash_share
  initial_tax<-if(is_gst || embedded_labour)0 else t*resource
  K<-resource-initial_tax; W<-K; B<-if(embedded_labour)0 else K
  Bnom<-B; Bcpi<-B; credit<-0
  flows<-data.frame(year=0,tax=initial_tax,stage='Initial labour-income tax',wealth=W,basis=B)
  for(j in seq_len(years)){
    current<-lambda*nominal_return*W
    appreciation<-(1-lambda)*nominal_return*W
    if(is_gst){tax_current<-0; unused<-0}
    else if(is_rra){
      if(asset=='Domestic company' && !company_relief){tax_current<-t*current;unused<-0}
      else{allowance<-a*B;tax_current<-t*pmax(current-allowance,0);unused<-pmax(allowance-current,0)}
    }else{tax_current<-t*current;unused<-0}
    reinvest<-current-tax_current
    W<-W+appreciation+reinvest
    if(is_rra && !(asset=='Domestic company' && !company_relief))B<-B+unused+reinvest
    else B<-B+reinvest
    Bnom<-Bnom+reinvest;Bcpi<-Bcpi*(1+inflation)+reinvest
    if(asset=='Domestic company')credit<-credit+tax_current
    flows<-rbind(flows,data.frame(year=j,tax=tax_current,
      stage=if(asset=='Domestic company')'Company tax (net of equity allowance where applicable)'else 'Current capital-income tax',wealth=W,basis=B))
  }
  W_before_exit<-W
  if(is_gst){exit_tax<-t*W;terminal<-W-exit_tax;stage<-'Tax on final consumption'}
  else if(asset=='Domestic company'){
    # Liquidate/distribute all profits at T; return paid-in capital separately.
    # Basic income tax on the grossed-up distribution less company credits.
    profit_cash<-W-K
    if(is_rra && !company_relief){
      accumulated_allowance<-K*((1+a)^years-1)
      exit_tax<-t*(profit_cash+credit-accumulated_allowance)-credit
    }else if(is_rra){
      # Integrated business-level equity allowance. Shareholder base respects
      # the same allowance; already-taxed excess income is fully credited.
      exit_tax<-t*(W-B)
    }else{exit_tax<-t*(profit_cash+credit)-credit}
    terminal<-W-exit_tax;stage<-'Shareholder settlement after usable franking credits'
  }else{
    exit_tax<-if(is_rra)t*(W-B)else if(system=='CGT inflation allowance only')t*pmax(W-Bcpi,0)else t*(W-Bnom)
    terminal<-W-exit_tax;stage<-'Tax on realised capital gain (or symmetric RRA relief)'
  }
  flows<-rbind(flows,data.frame(year=years,tax=exit_tax,stage=stage,wealth=terminal,basis=B))
  C0<-resource*(1-t)
  # Consumption timing return removes the initial labour/consumption tax level.
  nominal_net<-(terminal/C0)^(1/years)-1
  real_net<-(1+nominal_net)/(1+inflation)-1
  real_pre<-(1+nominal_return)/(1+inflation)-1
  result<-data.frame(asset=asset,system=system,years=years,resource=resource,
    nominal_return=nominal_return,pre_real_return=real_pre,normal_nominal_allowance=a,
    inflation=inflation,cash_share=lambda,initial_tax=initial_tax,initial_investment=K,
    exit_tax=exit_tax,wealth_before_exit=W_before_exit,final_consumption=terminal,
    final_consumption_real=terminal/(1+inflation)^years,
    pv_initial_tax=initial_tax,pv_saving_tax=sum(flows$tax/(1+a)^flows$year)-initial_tax,
    pv_total_tax=sum(flows$tax/(1+a)^flows$year),
    pv_year10_tax=sum(flows$tax[flows$year==years])/(1+a)^years,
    pv_tax_at_actual_return=sum(flows$tax/(1+nominal_return)^flows$year),
    nominal_net_return=nominal_net,real_net_return=real_net,
    specified_return_wedge=if(abs(real_pre)<1e-10)NA_real_ else (real_pre-real_net)/real_pre,
    company_relief=company_relief,embedded_labour=embedded_labour)
  if(collect){
    flows$asset<-asset;flows$system<-system;flows$nominal_return<-nominal_return
    flows$pv_tax<-flows$tax/(1+a)^flows$year
    tax_events[[length(tax_events)+1L]]<<-flows
  }
  result
}

base<-bind_rows(lapply(assets,function(asset)bind_rows(lapply(systems,function(system)sim(asset,system,collect=TRUE)))))
# Genuine required-return METR: increase the project's pre-tax return until
# its after-tax real saving return equals the common required normal return.
metr<-function(asset,system,years=T,cash_share=0){
  p<-uniroot(function(g)sim(asset,system,nominal_return=g,years=years,cash_share=cash_share)$real_net_return-normal_real,
    interval=c(inflation-.005,.30),tol=1e-12)$root
  pre_real<-(1+p)/(1+inflation)-1
  data.frame(required_nominal_return=p,required_pre_real_return=pre_real,
    required_after_real_return=normal_real,metr=(pre_real-normal_real)/pre_real)
}
base<-bind_rows(lapply(seq_len(nrow(base)),function(i)cbind(base[i,],metr(base$asset[i],base$system[i]))))
save(base,'baseline');save(bind_rows(tax_events),'tax_payment_timeline')
above<-bind_rows(lapply(assets,function(asset)bind_rows(lapply(systems,function(system)sim(asset,system,nominal_return=.08)))))
save(above,'eight_percent_nominal_return')

# A separate exact project-tax comparison: SAME investment K and terminal V.
# This includes the cash-flow tax's initial investment deduction, rather than
# comparing different investment scales out of the same gross wage resource.
matched_project<-data.frame(nominal_return=c(a,.08),investment=100000,years=T) |>
  mutate(terminal_proceeds=investment*(1+nominal_return)^years,
    normal_basis=investment*(1+a)^years,
    rra_initial_tax=0,rra_exit_tax=t*(terminal_proceeds-normal_basis),
    cashflow_initial_tax=-t*investment,cashflow_exit_tax=t*terminal_proceeds,
    rra_project_pv=rra_exit_tax/(1+a)^years,
    cashflow_project_pv=cashflow_initial_tax+cashflow_exit_tax/(1+a)^years)
save(matched_project,'matched_project_equivalence')

# A strict comprehensive-accrual benchmark taxes all economic appreciation
# annually, regardless of cash payments. Finance/fractional disposal assumed.
accrual<-bind_rows(lapply(assets,function(asset){
  d<-sim('Bank deposit','Full-rate income tax');d$asset<-asset
  cbind(d,metr('Bank deposit','Full-rate income tax'))
}))
save(accrual,'full_comprehensive_accrual_benchmark')
scope<-bind_rows(sim('Domestic company','Comprehensive normal-return allowance'),
                sim('Domestic company','Comprehensive normal-return allowance',company_relief=FALSE)) |>
  mutate(implementation=if_else(company_relief,'Business-level relief, integrated settlement','Shareholder-only allowance at exit; company tax unchanged'))
save(scope,'company_allowance_scope')
embedded<-bind_rows(lapply(systems,function(system)sim('New business: capital appreciation',system,embedded_labour=TRUE)))
save(embedded,'labour_embodied_in_zero_basis_business')

# Optional alternative interpretation: ten equal beginning-of-year earnings,
# each invested until a common date 10. Equal total earnings are $100,000;
# accrued cohorts have horizons 10,...,1. Not added to the primary lump sum.
annual_cohorts<-list()
for(asset in assets)for(system in systems)for(k in 0:9){
  z<-sim(asset,system,years=10-k,resource=L/10)
  z$earning_year<-k
  # sim's PV is at each cohort's earning date; shift it back to date zero.
  z$pv_total_tax<-z$pv_total_tax/(1+a)^k
  z$pv_saving_tax<-z$pv_saving_tax/(1+a)^k
  z$pv_initial_tax<-z$pv_initial_tax/(1+a)^k
  z$final_consumption_real<-z$final_consumption/(1+inflation)^T
  annual_cohorts[[length(annual_cohorts)+1L]]<-z
}
annual<-bind_rows(annual_cohorts)
save(annual,'annual_earnings_cohorts')
annual_summary<-annual |>group_by(asset,system)|>summarise(across(c(resource,initial_tax,initial_investment,final_consumption,final_consumption_real,pv_total_tax,pv_saving_tax,pv_initial_tax),sum),.groups='drop')
save(annual_summary,'annual_earnings_summary')

years_data<-bind_rows(lapply(c(1,2,5,10,20,30),function(n)bind_rows(lapply(assets,function(asset)bind_rows(lapply(systems,function(system){
  z<-sim(asset,system,years=n);cbind(z,metr(asset,system,years=n))
}))))))
save(years_data,'holding_period_sensitivity')
return_data<-bind_rows(lapply(seq(0,.10,.005),function(rr)bind_rows(lapply(assets,function(asset)bind_rows(lapply(systems,function(system){
  sim(asset,system,nominal_return=(1+inflation)*(1+rr)-1)
}))))))
save(return_data,'return_sensitivity')
mix<-bind_rows(lapply(seq(0,1,.1),function(cs)bind_rows(lapply(systems,function(system){
  z<-sim('House: capital appreciation',system,cash_share=cs)
  cbind(z,metr('House: capital appreciation',system,cash_share=cs))
}))))
save(mix,'income_gain_mix')
discounts<-bind_rows(lapply(c(0,.025,a,.08),function(d){
  flows<-bind_rows(tax_events)
  flows |>group_by(asset,system)|>summarise(pv_tax=sum(tax/(1+d)^year),.groups='drop')|>mutate(discount_rate=d)
}))
save(discounts,'discount_rate_sensitivity')

# Independent algebraic checks of tax accounting and required-return solutions.
checks<-list();check<-function(name,ok){checks[[length(checks)+1L]]<<-data.frame(check=name,passed=isTRUE(ok));if(!isTRUE(ok))stop('Check failed: ',name)}
check('All assets have the same pre-tax total return',length(unique(base$nominal_return))==1)
check('At normal return, PV taxes plus PV consumption equals labour resource',max(abs(base$pv_total_tax+base$final_consumption/(1+a)^T-L))<1e-7)
check('Bank/company match under fully usable integrated credits',max(abs(base$final_consumption[base$asset=='Bank deposit']-base$final_consumption[base$asset=='Domestic company']))<1e-7)
neutral<-base |>filter(system %in% c('Comprehensive normal-return allowance','Broad consumption tax'))
check('RRA and consumption tax have zero normal-return METR',max(abs(neutral$metr))<1e-8)
check('RRA and consumption tax have matching PV tax at normal return',max(abs(neutral$pv_total_tax-t*L))<1e-7)
check('GST tax-exclusive rate is correctly matched',abs((1/(1+gst))-(1-t))<1e-12)
check('Matched project RRA and cash-flow tax PVs agree above normal as well',max(abs(matched_project$rra_project_pv-matched_project$cashflow_project_pv))<1e-8)
bank_formula<-L*(1-t)*(1+a*(1-t))^T
check('Bank terminal wealth matches closed form',abs(base$final_consumption[base$asset=='Bank deposit' & base$system=='Full-rate income tax']-bank_formula)<1e-7)
house_formula<-L*(1-t)*((1-t)*(1+a)^T+t)
check('Full-rate realised gain matches closed form',abs(base$final_consumption[base$asset=='House: capital appreciation' & base$system=='Full-rate income tax']-house_formula)<1e-7)
cpi_formula<-L*(1-t)*((1-t)*(1+a)^T+t*(1+inflation)^T)
check('Indexed realised gain matches closed form',abs(base$final_consumption[base$asset=='House: capital appreciation' & base$system=='CGT inflation allowance only']-cpi_formula)<1e-7)
check('Coherent accrual tax gives the same outcome across assets',diff(range(accrual$final_consumption))<1e-8)
check('RRA unused allowance and reinvestment preserve neutrality across payout mixes',max(abs(mix$metr[mix$system=='Comprehensive normal-return allowance']))<1e-8)
check('Deferred zero-basis labour case has no invented CPI or RRA basis',max(abs(embedded$final_consumption-L*(1-t)*(1+a)^T))<1e-7)
check('Every METR root meets the required after-tax return',all(vapply(seq_len(nrow(base)),function(i)abs(sim(base$asset[i],base$system[i],nominal_return=base$required_nominal_return[i])$real_net_return-normal_real)<1e-8,logical(1))))
save(bind_rows(checks),'validation')
save(data.frame(parameter=c('Initial gross labour resource','Years','Personal tax rate','Company tax rate','Inflation','Normal real required return','Normal nominal allowance/return/PV discount','Equivalent tax-exclusive broad GST','Current payout share: bank','Current taxable profit share: company','Current payout share: house/business'),value=c(L,T,t,t,inflation,normal_real,a,gst,1,1,0)),'assumptions')

# Self-contained plots, separate from the main 62-chart catalogue.
theme_set(theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank(),legend.position='bottom',legend.title=element_blank(),axis.title.x=element_text(margin=margin(t=9)),plot.caption=element_text(hjust=0,size=8),plot.title=element_text(face='bold',size=16),plot.margin=margin(12,16,12,14)))
pal<-c('#146b8c','#20836b','#cf671d','#a34b66');names(pal)<-systems
asset_label<-c('Bank deposit'='Bank deposit','Domestic company'='Domestic company\n(fully usable credits)','House: capital appreciation'='House\n(all appreciation)','New business: capital appreciation'='New business\n(all appreciation)')
fmt<-scales::label_dollar(accuracy=1);pct<-scales::label_percent(accuracy=.1)
wrap<-function(s,w=125)paste(strwrap(s,w),collapse='\n')
plots<-list()
emit<-function(id,title,d,p,note,height=8){
 p<-p+labs(title=wrap(paste(id,title),80),subtitle='Hypothetical comparison; flat tax rates and a common pre-tax total return across saving vehicles',caption=wrap(paste('Source: 01_compare.R.',note)))
 mappings<-c(names(p$mapping),unlist(lapply(p$layers,function(z)names(z$mapping))))
 if('colour' %in% mappings)p<-p+scale_colour_manual(values=pal)
 if('fill' %in% mappings)p<-p+scale_fill_manual(values=pal)
 for(ext in c('png','svg'))ggsave(file.path(root,'figures',paste0(id,'.',ext)),p,width=13,height=height,dpi=180,bg='white',device=if(ext=='svg')svglite::svglite else 'png')
 write_csv(d,file.path(root,'figures',paste0(id,'.csv')),na='')
 plots[[length(plots)+1]]<<-data.frame(id=id,title=title,note=note)
}
common<-'Initial labour $100k; tax 30%; inflation 2.5%; normal real return 3%; nominal return/allowance/discount 5.575%. House/business have no current payouts. Company relief and usable credits assumed under RRA.'
emit('T01','Present value of all tax paid',base,ggplot(base,aes(asset,pv_total_tax,fill=system))+geom_col(position='dodge')+scale_x_discrete(labels=asset_label)+scale_y_continuous(labels=fmt)+labs(x='Saving vehicle',y='PV of tax on initial earnings and saving returns (AUD)')+guides(fill=guide_legend(ncol=2)),common)
emit('T02','Tax after date zero, separate from initial labour tax',base,ggplot(base,aes(asset,pv_saving_tax,fill=system))+geom_col(position='dodge')+scale_x_discrete(labels=asset_label)+scale_y_continuous(labels=fmt)+labs(x='Saving vehicle',y='PV of tax paid after date zero (AUD)')+guides(fill=guide_legend(ncol=2)),paste(common,'GST here is a future tax on the entire consumption resource, not just investment returns; its initial tax is zero.'))
emit('T03','Final consumption in starting-year purchasing power',base,ggplot(base,aes(asset,final_consumption_real,fill=system))+geom_col(position='dodge')+scale_x_discrete(labels=asset_label)+scale_y_continuous(labels=fmt)+labs(x='Saving vehicle',y='Consumption at year 10, in date-zero AUD')+guides(fill=guide_legend(ncol=2)),common)
timeline<-bind_rows(tax_events)|>group_by(asset,system,year)|>summarise(tax=sum(tax),.groups='drop')
emit('T04','When is tax actually paid?',timeline,ggplot(timeline,aes(year,tax,colour=system))+geom_line()+geom_point()+facet_wrap(~asset,labeller=as_labeller(asset_label))+scale_y_continuous(labels=fmt)+labs(x='Year (0 = labour income earned; 10 = final consumption)',y='Tax paid in that year (nominal AUD)')+guides(colour=guide_legend(ncol=2)),common,height=10)
emit('T05','Required-return METRs',base,ggplot(base,aes(asset,metr,fill=system))+geom_col(position='dodge')+scale_x_discrete(labels=asset_label)+scale_y_continuous(labels=pct)+labs(x='Saving vehicle',y='METR: (required pre-tax return - 3%) / required pre-tax return')+guides(fill=guide_legend(ncol=2)),paste(common,'Pre-tax return is solved separately so every case delivers a 3% after-tax real saving return.'))
emit('T06','Specified-return wedges are not the same as METRs',base,ggplot(base,aes(metr,specified_return_wedge,colour=system,shape=asset))+geom_point(size=3,alpha=.8)+geom_abline(slope=1,intercept=0,linetype=2)+scale_x_continuous(labels=pct)+scale_y_continuous(labels=pct)+labs(x='Required-return METR',y='Wedge on the specified 3% real pre-tax return')+guides(colour=guide_legend(ncol=2),shape=guide_legend(ncol=2)),paste(common,'Both measures exclude the level effect of initial labour tax by using consumption now as the baseline.'))
emit('T07','Holding period changes realisation-based METRs',years_data,ggplot(years_data,aes(years,metr,colour=system))+geom_line(linewidth=.9)+facet_wrap(~asset,labeller=as_labeller(asset_label))+scale_y_continuous(labels=pct)+labs(x='Years until sale and consumption',y='Required-return METR')+guides(colour=guide_legend(ncol=2)),common,height=10)
emit('T08','Different investment scales confound above-normal PV comparisons',return_data,ggplot(return_data,aes(pre_real_return,pv_total_tax,colour=system))+geom_line(linewidth=.8)+facet_wrap(~asset,labeller=as_labeller(asset_label))+geom_vline(xintercept=normal_real,linetype=2)+scale_x_continuous(labels=pct)+scale_y_continuous(labels=fmt)+labs(x='Common annual real pre-tax return',y='PV of all tax, discounted at the fixed normal rate')+guides(colour=guide_legend(ncol=2)),paste(common,'GST initially invests $100k; income-tax cases invest $70k. This is NOT a matched-project RRA/cash-flow equivalence test. Dashed line: normal return.'),height=10)
emit('T09','Current income versus capital appreciation affects the comparison',mix,ggplot(mix,aes(cash_share,metr,colour=system))+geom_line(linewidth=1)+scale_x_continuous(labels=pct)+scale_y_continuous(labels=pct)+labs(x='Share of pre-tax total return paid currently and reinvested',y='Required-return METR')+guides(colour=guide_legend(ncol=2)),paste(common,'Hypothetical house/asset with payouts from 0% to 100% of total return; original cost and reinvested amounts tracked separately.'))
scope_plot<-scope|>mutate(implementation=factor(implementation,levels=implementation))
emit('T10','An allowance at shareholder level alone can leave a company-tax wedge',scope_plot,ggplot(scope_plot,aes(implementation,pv_total_tax))+geom_col(fill=pal[2])+geom_hline(yintercept=t*L,linetype=2)+scale_x_discrete(labels=function(x)vapply(x,wrap,character(1),w=40))+scale_y_continuous(labels=fmt)+labs(x='Illustrative RRA implementation',y='PV of all tax (AUD)'),paste(common,'Shareholder-only variant banks allowance until payout; ordinary company tax and nominal franking credits remain. Fully usable exit refund assumed; no claim about credit capitalisation.'))
embedded_plot<-embedded|>mutate(system=factor(system,levels=systems))
emit('T11','Labour embodied in a zero-basis asset: a different starting tax treatment',embedded_plot,ggplot(embedded_plot,aes(system,pv_total_tax,fill=system))+geom_col()+scale_x_discrete(labels=function(x)vapply(x,wrap,character(1),w=23))+scale_y_continuous(labels=fmt)+labs(x='Hypothetical tax system',y='PV of total tax on zero-basis business proceeds (AUD)')+guides(fill='none'),paste(common,'No initial cash wage or tax: $100k of value is created by unpaid work, then realised at sale. Zero recorded basis gets no invented CPI/RRA uplift. Not an ordinary-profit deferral or a capital-investment METR.'))
emit('T12','Alternative: earn $10,000 at the start of each of ten years',annual_summary,ggplot(annual_summary,aes(asset,final_consumption_real,fill=system))+geom_col(position='dodge')+scale_x_discrete(labels=asset_label)+scale_y_continuous(labels=fmt)+labs(x='Saving vehicle',y='Consumption at common year 10, in date-zero AUD')+guides(fill=guide_legend(ncol=2)),paste(common,'Ten cohorts: same total $100k nominal labour income, invested for 10,...,1 years. Distinct from the primary date-zero lump sum.'))
save(bind_rows(plots),'chart_catalogue')
cards<-vapply(plots,function(z)paste0('<article><h2>',z$id,' ',z$title,'</h2><a href="figures/',z$id,'.png"><img src="figures/',z$id,'.png" loading="lazy"></a><p>',z$note,'</p><p><a href="figures/',z$id,'.png">PNG</a> | <a href="figures/',z$id,'.svg">SVG</a> | <a href="figures/',z$id,'.csv">CSV</a></p></article>'),character(1))
writeLines(paste0('<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Ten-year tax comparison</title><style>body{max-width:1150px;margin:35px auto;padding:0 20px;font:17px/1.55 system-ui;color:#233744;background:#f5f7f8}article{background:white;padding:24px;margin:25px 0;border:1px solid #d5e1e5;border-radius:8px}img{width:100%}a{color:#146b8c}h1{font-size:34px}</style><h1>Ten-year saving, PV tax and METRs</h1><p><a href="EXAMPLE.pdf">Worked example PDF</a> | <a href="EXAMPLE.md">Markdown</a> | <a href="data/baseline.csv">Main numeric table</a> | <a href="data/tax_payment_timeline.csv">Tax timeline</a></p><p>All values are hypothetical. The four assets share a pre-tax total return; differences arise from tax bases, timing and treatment of the initial labour resource. Full-rate CGT is realised at sale in the main comparison; a strict accrual benchmark is supplied separately.</p>',paste(cards,collapse='\n'),'</html>'),file.path(root,'index.html'),useBytes=TRUE)

# Numeric markdown tables for inclusion in the example without hand transcription.
table_md<-function(d,labels){
 header<-paste('|',paste(labels,collapse=' | '),'|')
 rows<-apply(d,1,function(r)paste('|',paste(r,collapse=' | '),'|'))
 paste(c(header,paste('|',paste(rep('---',ncol(d)),collapse=' | '),'|'),rows),collapse='\n')
}
money<-function(x)paste0('$',format(round(x),big.mark=',',trim=TRUE,scientific=FALSE))
percentage<-function(x)paste0(format(round(100*x,2),nsmall=2,trim=TRUE),'%')
report<-base|>mutate(across(c(initial_tax,pv_saving_tax,pv_total_tax,final_consumption_real),money),across(c(real_net_return,metr,specified_return_wedge),percentage))
writeLines(table_md(report|>select(asset,system,pv_total_tax,final_consumption_real),c('Saving vehicle','Tax system','PV of all tax','Year-10 consumption in initial dollars')),file.path(root,'data','pv_table.md'))
writeLines(table_md(report|>select(asset,system,real_net_return,specified_return_wedge,metr),c('Saving vehicle','Tax system','Net real saving return','Specified-return wedge','Required-return METR')),file.path(root,'data','metr_table.md'))
message('Saved ',nrow(base),' main comparisons, 12 plots and ',length(checks),' passing checks.')

# Standalone publication-size PNG/SVG and the exact plotted CSV for each chart.
req<-c('ggplot2','dplyr','tidyr','readr','purrr','scales','svglite')
for(p in req) if(!requireNamespace(p,quietly=TRUE)) stop('Missing R package: ',p)
suppressPackageStartupMessages({library(ggplot2);library(dplyr);library(tidyr);library(readr);library(purrr);library(scales)})
arg<-grep('^--file=',commandArgs(FALSE),value=TRUE)
root<-if(length(arg)) dirname(normalizePath(sub('^--file=','',arg[1]))) else normalizePath('Australia_CGT')
out<-file.path(root,'processed'); fig<-file.path(root,'figures');dir.create(fig,showWarnings=FALSE)
read<-function(n)read_csv(file.path(out,paste0(n,'.csv')),show_col_types=FALSE)
wrap<-function(x,w=105)paste(strwrap(x,width=w),collapse='\n')
fy<-function(y)paste0(y,'-',sprintf('%02d',(y+1)%%100))
theme_set(theme_minimal(base_size=12,base_family='sans')+theme(panel.grid.minor=element_blank(),plot.title=element_text(face='bold',size=18),plot.subtitle=element_text(size=11,lineheight=1.15),plot.caption=element_text(hjust=0,size=9,lineheight=1.1),legend.position='bottom',legend.title=element_blank(),plot.margin=margin(15,22,13,15),strip.text=element_text(face='bold')))
palette<-c('#146b8c','#cf671d','#20836b','#a34b66','#745898','#9b7825','#58646f','#d5828e','#8c9d42','#1f447c')
scale_colour_discrete<-function(...)scale_colour_manual(values=palette,...)
scale_fill_discrete<-function(...)scale_fill_manual(values=palette,...)
catalogue<-list()
# Titles refer to the visible horizontal axis, including flipped bar charts.
horizontal_axes<-c(
 D01='Income year (July to June)',
 D02='Income year (July to June)',
 D03='Reported gross capital gains (AUD billion)',
 D04='Reported gross capital gains (AUD billion)',
 D05='Reported capital gains or losses (AUD billion)',
 D06='Steps from gross gains to net taxable gains',
 D07='Income year (July to June)',
 D08='Annual taxable income bracket (AUD)',
 D09='Age group (years)',
 D10='Age group (years)',
 D11='Recorded sex',
 D12='Cumulative share of taxable filers, ranked by taxable income (%)',
 D13='Annual taxable income bracket (AUD)',
 D14='Annual net taxable capital gain per filer (AUD)',
 D15='Income year (July to June)',
 D16='Income year (July to June)',
 D17='Income year (July to June)',
 D18='Annual taxable income decile (lowest to highest)',
 D19='Age group (years)',
 B01='Financial year (July to June)',
 B02='Financial year (July to June)',
 B03='Financial year (July to June)',
 B04='Financial year (July to June)',
 B05='Financial year (July to June)',
 B06='Financial year (July to June)',
 M01='Component of the annual nominal investment return',
 M02='Years since earning and investing the initial income',
 M03='Years since earning and investing the initial income',
 M04='Tax-inclusive income tax rate (%)',
 M05='Annual real investment return (%)',
 M06='Years since the initial investment',
 M07='Annual cash yield (%); capital appreciation supplies the remaining return',
 M08='Actual annual real investment return (%)',
 M09='Years until the asset is sold',
 M10='Annual nominal capital appreciation (%)',
 M11='Annual nominal capital appreciation (%)',
 M12='Annual nominal capital appreciation (%)',
 M13='Asset holding period (years)',
 M14='Other annual taxable income, excluding the gain (AUD)',
 M15='Years of gains accumulated before sale',
 M16='Years after 1 July 2027 until sale',
 M17='Assumed hours of unpaid owner work',
 R01='Investment holding period (years)',
 R02='Investment holding period (years)',
 R03='Replacement investment annual real return (%)',
 R04='Years until the capital loss can be used',
 R05='Tax treatment and investment scale',
 R06='Assumed probability of project success (%)',
 R07='Loan as a share of the asset value (%)',
 R08='Annual inflation rate (%)',
 R09='Personal income tax rate (%)',
 R10='Years after the hypothetical tax is introduced',
 R11='One-period nominal asset return (%)',
 E01='Reported behavioural parameter (definition differs by panel)',
 E02='Elasticity of realised gains with respect to the tax rate',
 E03='Tax rate on the hypothetical gains base (%)',
 E04='Tax rate on the hypothetical gains base (%)',
 E05='Local elasticity of realised gains to the tax rate, evaluated at 22%',
 E06='Years relative to reform (0 = first year under the new rate)',
 E07='Units of gains disappearing from the CGT base (initial base = 100)',
 Q01='Observation date (monthly)',
 Q02='Observation date (quarterly)'
)
emit<-function(id,title,d,p,group,status,note,sources,width=12,height=8){
 stopifnot(nrow(d)>0)
 horizontal_axis<-unname(horizontal_axes[id])
 if(length(horizontal_axis)!=1L || is.na(horizontal_axis) || !nzchar(horizontal_axis)) stop('Missing horizontal-axis title: ',id)
 cap<-paste0('Source: ',sources,'.  ',note)
 p<-p+labs(title=wrap(paste(id,title),85),subtitle=wrap(status,125),caption=wrap(cap,145))
 if(inherits(p$coordinates,'CoordFlip')) {
   p<-p+labs(x='Asset category',y=horizontal_axis)
 } else {
   p<-p+labs(x=horizontal_axis)
 }
 p<-p+theme(axis.line.x=element_line(colour='#687780',linewidth=.35),axis.ticks.x=element_line(colour='#687780',linewidth=.35),axis.title.x=element_text(margin=margin(t=10)))
 ggsave(file.path(fig,paste0(id,'.png')),p,width=width,height=height,dpi=180,bg='white')
 ggsave(file.path(fig,paste0(id,'.svg')),p,width=width,height=height,device=svglite::svglite,bg='white')
 write_csv(d,file.path(fig,paste0(id,'.csv')),na='')
 catalogue[[length(catalogue)+1]]<<-tibble(id=id,title=title,group=group,status=status,note=note,sources=sources,horizontal_axis=horizontal_axis,png=paste0(id,'.png'),svg=paste0(id,'.svg'),csv=paste0(id,'.csv'))
 message('Saved ',id)
}
line<-function(d,x,y,colour=NULL){
 if(is.null(colour))ggplot(d,aes(x=.data[[x]],y=.data[[y]]))+geom_line(linewidth=1,colour=palette[1])
 else ggplot(d,aes(x=.data[[x]],y=.data[[y]],colour=.data[[colour]],group=.data[[colour]]))+geom_line(linewidth=1)
}
bar<-function(d,x,y,fill=NULL){
 if(is.null(fill))ggplot(d,aes(x=.data[[x]],y=.data[[y]]))+geom_col(fill=palette[1],width=.75)
 else ggplot(d,aes(x=.data[[x]],y=.data[[y]],fill=.data[[fill]]))+geom_col(position='dodge',width=.75)
}
bn<-label_number(scale=1e-9,suffix='bn',accuracy=.1)
money<-label_dollar(scale=1e-3,suffix='k',accuracy=1)
pc<-label_percent(accuracy=1)
obs<-'Observed Australian tax-return data; income years end in June; nominal AUD unless specified'
sim<-'Hypothetical scenario, not an estimate of reform revenue or household behaviour'
fut<-'Published conditional Budget/PBO estimates; source vintages are kept separate'
cgt<-read('ato_cgt_items'); cells<-read('individual_cells'); hist<-read('individual_history');pct<-read('individual_percentiles')
short_asset<-function(x)recode(x,
 'Shares in companies listed on an Australian securities exchange'='Australian-listed shares',
 'Units in unit trusts listed on an Australian securities exchange'='Australian-listed trust units',
 'Real estate situated in Australia'='Direct Australian real estate',
 'Amount of capital gains from a trust (including a managed fund)'='Distributions from trusts/funds',
 'Other CGT assets and any other CGT events'='Other assets/events',.default=x)
assets<-cgt |>filter(section=='Current year capital gains',unit=='$',!grepl('Total current|previously deferred',item)) |>mutate(asset=short_asset(item))
totals<-cgt |>filter(item=='Total current year capital gains',unit=='$') |>select(year,entity,total=value)
d<-totals
emit('D01','Reported gross realised gains vary substantially across years and entities',d,line(d,'year','total','entity')+scale_y_continuous(labels=bn)+scale_x_continuous(breaks=seq(2012,2023,2),labels=fy),'Composition',obs,'Funds are not individual investors. Trust-origin gains may be included in recipient entities. 2019-20 individual reporting changed.','ATO CGT Table 1')
d<-assets |>filter(entity=='Individuals') |>left_join(totals,by=c('year','entity')) |>mutate(share=value/total)
emit('D02','Housing and trust-origin gains: composition through time',d,line(d,'year','share','asset')+scale_y_continuous(labels=pc)+scale_x_continuous(breaks=seq(2012,2023,2),labels=fy)+guides(colour=guide_legend(ncol=2)),'Composition',obs,'Shares use the published gross total. Components do not always sum to that total; trust gains cannot be assigned to underlying assets.','ATO CGT Table 1',height=9)
d<-assets |>filter(year==2023,entity=='Individuals') |>select(asset,value)
res<-read('asset_reconciliation') |>filter(year==2023,entity=='Individuals')
d<-bind_rows(d,tibble(asset='Unreconciled difference to published total',value=res$gap)) |>mutate(asset=reorder(asset,value))
emit('D03','What individuals reported gains on in 2023-24',d,bar(d,'asset','value')+coord_flip()+scale_y_continuous(labels=bn),'Composition',obs,'Direct Australian real estate includes commercial and other real estate. Trusts remain a separate origin category. Difference to published total is retained.','ATO CGT Table 1')
d<-assets |>filter(year==2023) |>group_by(entity,asset) |>summarise(value=sum(value),.groups='drop')
emit('D04','Asset composition differs between individuals, companies and funds',d,ggplot(d,aes(value,reorder(asset,value),fill=entity))+geom_col()+facet_wrap(~entity,scales='free_x')+scale_x_continuous(labels=bn)+guides(fill='none')+labs(y=NULL),'Composition',obs,'Panels have different dollar scales. Values are reported components, not a reconciled look-through national asset total.','ATO CGT Table 1',width=14,height=9)
d<-cgt |>filter(year==2023,entity=='Individuals',unit=='$',section %in% c('Current year capital gains','Current year capital losses'),!grepl('Total|from a trust|previously deferred',item)) |>mutate(asset=short_asset(item),flow=if_else(section=='Current year capital gains','Gains','Losses'))
emit('D05','Gains and losses coexist within asset classes',d,bar(d,'asset','value','flow')+coord_flip()+scale_y_continuous(labels=bn),'Composition',obs,'Losses are shown positive for comparison. These are gross gains/losses across different taxpayers, not average investor returns.','ATO CGT Table 1')
s<-cgt |>filter(year==2023,entity=='Individuals',unit=='$')
get<-function(n)s$value[s$item==n][1]
g<-get('Total current year capital gains');lo<-get('Total capital losses applied');di<-get('Total CGT discount applied');sb<-get('Total small business concessions applied');ng<-get('Net capital gain')
d<-tibble(step=c('Gross gains','Losses applied','Discount applied','Small-business concessions','Other differences (residual)','Net taxable gains'),value=c(g,-lo,-di,-sb,ng-(g-lo-di-sb),ng)) |>mutate(start=c(0,head(cumsum(value),-1)),end=cumsum(value),start=if_else(step=='Net taxable gains',0,start),end=if_else(step=='Net taxable gains',ng,end),step=factor(step,levels=step))
emit('D06','Gross gains and net taxable gains are different amounts',d,ggplot(d,aes(step,ymin=pmin(start,end),ymax=pmax(start,end),fill=value>=0))+geom_rect(aes(xmin=as.numeric(step)-.35,xmax=as.numeric(step)+.35))+scale_y_continuous(labels=bn)+scale_x_discrete(labels=function(x)vapply(x,wrap,character(1),w=17))+guides(fill='none')+labs(y='Annual amount'),'Composition',obs,'Residual makes the arithmetic reconcile; it is not an identified concession. Schedule-based net gains differ slightly from the all-return Table 1A total.','ATO CGT Table 1')
d<-cgt |>filter(entity=='Individuals',unit=='$',item %in% c('Total current year capital losses','Total capital losses applied','Other net capital losses carried forward to later income years')) |>mutate(item=recode(item,'Other net capital losses carried forward to later income years'='Carried-forward stock','Total current year capital losses'='Current-year losses','Total capital losses applied'='Losses used this year'))
emit('D07','Losses recorded, losses used, and losses waiting for relief',d,line(d,'year','value','item')+scale_y_continuous(labels=bn)+scale_x_continuous(breaks=seq(2012,2023,2),labels=fy),'Composition',obs,'Carried-forward losses are a stock; the other lines are annual flows. Different taxpayers contribute to each. 2019-20 reporting break.','ATO CGT Table 1')
d<-cells |>group_by(tax_bracket) |>summarise(people=sum(people),net_gain=sum(net_gain),gross_gain=sum(gross_gain),estimated_tax=sum(estimated_cgt),.groups='drop') |>mutate(gain_share=net_gain/sum(net_gain),people_share=people/sum(people),tax_bracket=sub('^[a-z]+[.] ','',tax_bracket)) |>select(tax_bracket,gain_share,people_share) |>pivot_longer(-tax_bracket,names_to='measure',values_to='share') |>mutate(measure=recode(measure,gain_share='Net capital gains',people_share='All tax filers'))
emit('D08','Net gains are concentrated among higher annual taxable incomes',d,bar(d,'tax_bracket','share','measure')+scale_y_continuous(labels=pc)+scale_x_discrete(labels=function(x)vapply(x,wrap,character(1),w=17)),'Distribution',obs,'2023-24 income brackets. Ranking includes the capital gain itself and does not measure permanent income, wealth or decade-long income.','ATO Individuals Table 3A')
d<-cells |>group_by(age) |>summarise(people=sum(people),gain_people=sum(net_gain_people),net_gain=sum(net_gain),.groups='drop') |>mutate(participation=gain_people/people,age=sub('^[a-z]+[.] ','',age))
emit('D09','How common is a reported net gain at each age?',d,bar(d,'age','participation')+scale_y_continuous(labels=pc)+scale_x_discrete(labels=function(x)vapply(x,wrap,character(1),w=10)),'Distribution',obs,'2023-24. Denominator is tax filers in each age group, not all residents or asset holders.','ATO Individuals Table 3A')
emit('D10','Age distribution of the dollars of net gains',d,bar(d,'age','net_gain')+scale_y_continuous(labels=bn)+scale_x_discrete(labels=function(x)vapply(x,wrap,character(1),w=10)),'Distribution',obs,'2023-24. Amounts combine gains on different assets and different holding periods.','ATO Individuals Table 3A')
d<-cells |>group_by(sex) |>summarise(people=sum(people),gain_people=sum(net_gain_people),net_gain=sum(net_gain),.groups='drop') |>mutate(average_gain=net_gain/gain_people)
emit('D11','Average net gain among filers reporting a gain, by recorded sex',d,bar(d,'sex','average_gain')+scale_y_continuous(labels=money),'Distribution',obs,'2023-24. Means are not medians; recorded sex follows ATO disclosure categories. Composition and asset ownership are not controlled for.','ATO Individuals Table 3A')
d<-pct |>group_by(percentile) |>summarise(people=sum(people),gains=sum(net_gain),taxable_income=sum(taxable_income),.groups='drop') |>arrange(percentile) |>mutate(cumulative_people=cumsum(people)/sum(people),cumulative_gains=cumsum(gains)/sum(gains),cumulative_income=cumsum(taxable_income)/sum(taxable_income)) |>select(percentile,cumulative_people,cumulative_gains,cumulative_income) |>pivot_longer(c(cumulative_gains,cumulative_income),names_to='measure',values_to='share') |>mutate(measure=recode(measure,cumulative_gains='Net gains',cumulative_income='Taxable income'))
emit('D12','Concentration across taxable-income percentiles',d,line(d,'cumulative_people','share','measure')+geom_abline(slope=1,intercept=0,linetype=2,colour='grey60')+scale_x_continuous(labels=pc)+scale_y_continuous(labels=pc)+labs(x='Cumulative share of taxable filers',y='Cumulative share of dollars'),'Distribution',obs,'2023-24. Taxable filers only; this is income-ranked concentration, not a wealth Lorenz curve. Annual gains affect the ranking.','ATO Individuals Table 16A')
d<-cells |>group_by(tax_bracket) |>summarise(across(c(interest,unfranked_dividends,franked_dividends,franking_credits,net_gain,gross_rent),sum),.groups='drop') |>pivot_longer(-tax_bracket,names_to='income_form',values_to='value') |>mutate(tax_bracket=sub('^[a-z]+[.] ','',tax_bracket))
emit('D13','Capital income extends well beyond taxable capital gains',d,ggplot(d,aes(tax_bracket,value,fill=income_form))+geom_col()+scale_y_continuous(labels=bn)+scale_x_discrete(labels=function(x)vapply(x,wrap,character(1),w=17))+guides(fill=guide_legend(ncol=3)),'Distribution',obs,'2023-24. Gross interest/rent and dividend components versus net taxable gains: this is a reported-income map, not an economically consistent net capital-income total.','ATO Individuals Table 3A',height=9)
d<-read('ato_gain_ranges') |>filter(year==2023,entity=='Individuals') |>group_by(gain_range) |>summarise(count=sum(count),gains=sum(net_gain),.groups='drop') |>mutate(count_share=count/sum(count),gain_share=gains/sum(gains),gain_range=sub('^[a-z][.] ','',gain_range)) |>select(gain_range,count_share,gain_share) |>pivot_longer(-gain_range,names_to='measure',values_to='share') |>mutate(measure=recode(measure,count_share='Share of gain reporters',gain_share='Share of net gains'))
emit('D14','Many small gains, a large share of dollars in large gains',d,bar(d,'gain_range','share','measure')+scale_y_continuous(labels=pc)+scale_x_discrete(labels=function(x)vapply(x,wrap,character(1),w=15)),'Distribution',obs,'2023-24, all taxable statuses. Net gain ranges refer to after-loss/discount amounts, not sale proceeds.','ATO CGT Table 2')
d<-read('ato_gain_ranges') |>group_by(year,entity) |>summarise(net_gain=sum(net_gain),estimated_tax=sum(estimated_tax),.groups='drop') |>mutate(rate=estimated_tax/net_gain)
emit('D15','Estimated tax relative to net taxable gains',d,line(d,'year','rate','entity')+scale_y_continuous(labels=pc)+scale_x_continuous(breaks=seq(2013,2023,2),labels=fy),'Fiscal',obs,'ATO estimated tax allocation, not cash collected; denominator is already net of discounts/losses. This is not an effective rate on gross economic gains.','ATO CGT Table 2')
d<-hist |>filter(item=='Net capital gain',unit=='$',year>=1985) |>mutate(value_real=value) # CPI deflation is constructed below.
cpi<-read('rba_cpi') |>filter(series=='GCPIAG') |>mutate(date=as.Date(date),year=as.integer(format(date,'%Y'))-if_else(as.integer(format(date,'%m'))<=6,1L,0L)) |>group_by(year) |>summarise(cpi=mean(value),quarters=n(),.groups='drop')
base_cpi<-cpi$cpi[cpi$year==2023]
d<-d |>left_join(cpi,by='year') |>filter(quarters==4) |>mutate(value_real=value*base_cpi/cpi)
emit('D16','A longer history of individuals\u2019 net taxable gains',d,line(d,'year','value_real')+scale_y_continuous(labels=bn)+scale_x_continuous(labels=fy),'Fiscal','ATO observations, approximately CPI-deflated into 2023-24 purchasing power','Net taxable gains reflect successive tax rules and reporting changes, including the 1999 discount. This history does not identify the causal effect of reform.','ATO Individuals Table 1A; ABS CPI via RBA G1')
d<-read('rental_schedules') |>group_by(year) |>filter(if(any(position=='All')) position=='All' else position %in% c('Loss','Neutral/Profit')) |>summarise(gross_rent=sum(gross_rent),other_expenses=sum(expenses)-sum(interest),interest=sum(interest),net_rent=sum(net_rent),.groups='drop') |>pivot_longer(-year,names_to='item',values_to='value') |>mutate(item=recode(item,gross_rent='Gross rent',interest='Interest deductions',other_expenses='Other deductions',net_rent='Net rent'))
emit('D17','Rental income, borrowing costs and other deductions',d,line(d,'year','value','item')+scale_y_continuous(labels=bn)+scale_x_continuous(breaks=seq(2012,2023,2),labels=fy)+labs(y='Nominal AUD per year'),'Composition',obs,'Rental-property schedule coverage; not unique landlords. Other deductions include capital works/depreciation. Not a measure of real economic profit.','ATO Individuals Table 26')
d<-read('teis_discount_income')
emit('D18','Who receives the measured benefit of the CGT discount?',d,bar(d,'group','benefit_share_pct')+labs(y='Share of revenue-forgone benefit (%)'),'Distribution','Treasury TEIS distributional estimate for 2022-23; pre-reform benchmark','Income decile includes gains. A tax expenditure is benchmark-dependent revenue forgone, not the revenue obtainable from repeal. Published shares are rounded.','Treasury TEIS chart 2.7')
d<-read('teis_discount_age')
emit('D19','Age profile of the existing discount\u2019s estimated benefit',d,bar(d,'group','benefit_share_pct')+labs(y='Share of revenue-forgone benefit (%)')+scale_x_discrete(labels=function(x)vapply(x,wrap,character(1),w=10)),'Distribution','Treasury TEIS distributional estimate for 2022-23; pre-reform benchmark','Shares and recipient numbers are distinct; this does not measure a lifetime distribution of reform burdens.','Treasury TEIS chart 2.8')

# Budget: actuals, estimates, combined costings, and benchmark tax expenditures.
d<-read('budget_cgt_receipts')
emit('B01','CGT receipts in the 2026-27 Budget',d,ggplot(d,aes(factor(fy(year),levels=fy(year)),cgt_m/1000,fill=status))+geom_col()+labs(y='AUD billion'),'Fiscal',fut,'2024-25 actual; subsequent years are Budget estimates. CGT is embedded in individual/trust, company and superannuation taxes, not an additional tax line.','Budget Paper 1 Table 5.7')
d<-d |>mutate(share=cgt_m/total_tax_m)
emit('B02','How large is CGT relative to all Commonwealth tax receipts?',d,bar(d,'year','share','status')+scale_y_continuous(labels=pc)+scale_x_continuous(breaks=d$year,labels=fy),'Fiscal',fut,'Same cash-receipts denominator and Budget vintage; not CGT divided by personal income tax.','Budget Paper 1 Table 5.7')
d<-read('budget_reform') |>mutate(net_m=receipts_m-payments_m) |>select(year,receipts_m,payments_m,net_m) |>pivot_longer(-year,names_to='measure',values_to='value') |>mutate(measure=recode(measure,receipts_m='Extra receipts',payments_m='Related payments',net_m='Net budget improvement'))
emit('B03','The combined CGT and negative-gearing Budget costing',d,bar(d,'year','value','measure')+scale_x_continuous(breaks=2025:2029,labels=fy)+labs(y='AUD million'),'Fiscal',fut,'Not a stand-alone CGT score. Budget announcement vintage; dash and .. rendered as zero (.. is rounded small magnitude). No new behavioural adjustment is applied.','Budget Paper 2 p.21')
d<-read('pbo_measures') |>filter(grepl('gearing|trusts|Offset',measure))
emit('B04','Reform receipts build beyond the forward estimates',d,line(d,'year','value_bn','measure')+scale_x_continuous(breaks=seq(2026,2036,2),labels=fy)+labs(y='AUD billion per year')+guides(colour=guide_legend(ncol=1)),'Fiscal',fut,'July 2026 PBO preliminary medium-term estimates, rounded to $0.1bn. CGT and negative gearing are combined. These are not updated November legislative costings.','PBO Figure 4-9',height=9)
d<-d |>group_by(measure) |>arrange(year,.by_group=TRUE) |>mutate(cumulative=cumsum(value_bn)) |>ungroup()
emit('B05','Cumulative medium-term amounts depend on the chosen window',d,line(d,'year','cumulative','measure')+scale_x_continuous(breaks=seq(2026,2036,2),labels=fy)+labs(y='Cumulative AUD billion since 2026-27')+guides(colour=guide_legend(ncol=1)),'Fiscal',fut,'Sum of rounded PBO annual estimates. A cumulative amount is not an annual permanent yield.','PBO Figure 4-9',height=9)
d<-read('tax_expenditures') |>filter(code %in% c('E15','E7','E8')) |>mutate(item=paste(code,item))
emit('B06','The benchmark matters: CGT-related tax expenditures',d,line(d,'year','value_m','item')+scale_x_continuous(breaks=seq(2021,2028),labels=fy)+labs(y='Revenue forgone, AUD million')+guides(colour=guide_legend(ncol=1)),'Fiscal','Treasury December 2025 TEIS estimates; pre-Budget policy baseline','Separate main-residence benchmark components and discount estimate. Do not add as repeal revenue or compare directly with the combined reform score.','Treasury TEIS revenue-forgone workbook',height=9)

# Normal return / consumption-tax thought experiments.
d<-read('return_components')
emit('M01','Inflation, the normal return, and excess returns are separate',d,bar(d,'component','value')+scale_y_continuous(labels=label_dollar())+scale_x_discrete(labels=function(x)vapply(x,wrap,character(1),w=22)),'Normal return',sim,'$100k principal; inflation 2.5%, real normal return 3%, excess real return 4pp. Uses exact nominal compounding, not simple addition.','Model parameters and 02_models.R')
d<-read('normal_return_consumption')
emit('M02','How much future consumption can the same new earnings buy?',d,line(d,'year','real_consumption','regime')+scale_y_continuous(labels=money)+labs(y='Consumption in starting-year AUD')+guides(colour=guide_legend(ncol=2)),'Normal return',sim,'$100k pre-tax new labour resource; 30% income-inclusive rate and matching 42.86% consumption-tax rate. Normal real return 3%, inflation 2.5%. RRA and consumption lines coincide.','Model: normal_return_consumption',height=9)
emit('M03','An allowance removes the tax wedge between consuming now and later',d,line(d,'year','relative_to_consuming_now','regime')+labs(y='Future real consumption / own immediate consumption')+guides(colour=guide_legend(ncol=2)),'Normal return',sim,'Normal-return-only case. Consumption measured after tax. Does not assert identical distribution, treatment of existing wealth, or all risky outcomes under actual GST.','Model: normal_return_consumption',height=9)
d<-read('tax_rate_mapping')
emit('M04','Matching income-inclusive and GST-style tax-exclusive rates',d,line(d,'income_tax_rate','equivalent_gst_rate')+scale_x_continuous(labels=pc)+scale_y_continuous(labels=pc)+labs(x='Tax-inclusive income rate',y='Equivalent tax-exclusive consumption rate'),'Normal return','Algebraic mapping, not a proposed GST rate','s = t / (1 - t). Australian GST is 10%; a 30% income tax is not equivalent to a 30% GST.','Model identity')
d<-read('rra_cashflow_equivalence') |>filter(year==10) |>select(real_return,rra_tax_pv,cashflow_tax_pv,indexed_tax_pv) |>pivot_longer(-real_return,names_to='method',values_to='pv_tax') |>mutate(method=recode(method,rra_tax_pv='Normal-return allowance',cashflow_tax_pv='Cash-flow tax, refundable upfront deduction',indexed_tax_pv='CPI allowance only'))
emit('M05','RRA and cash-flow taxation: an exact present-value comparison',d,line(d,'real_return','pv_tax','method')+geom_point()+geom_hline(yintercept=0,linetype=2)+scale_x_continuous(labels=pc)+scale_y_continuous(labels=money)+labs(x='Annual real return',y='Present value of net tax')+guides(colour=guide_legend(ncol=1)),'Normal return',sim,'New $100k investment, 10 years, 30% flat tax. Discount at the 5.575% nominal allowance rate. Full immediate-value loss relief; RRA and cash-flow lines coincide. Risk would require state pricing.','Model: rra_cashflow_equivalence',height=9)
d<-read('allowance_bases')
emit('M06','CPI indexation protects principal; a normal allowance protects more',d,line(d,'year','basis','method')+scale_y_continuous(labels=money),'Normal return',sim,'$100k cost, no interim payouts. CPI 2.5%; nominal normal allowance 5.575%. Basis growth is not a cash subsidy or observed return.','Model: allowance_bases')
d<-read('return_form_neutrality')
emit('M07','Same total return, different split between payouts and gains',d,line(d,'cash_yield','tax','method')+scale_x_continuous(labels=pc)+scale_y_continuous(labels=label_dollar())+labs(x='Annual cash yield; capital appreciation fills the rest',y='One-period tax on $100k asset'),'Normal return',sim,'Total nominal return 8%; 30% flat tax, no leverage or company tax. Gain leg is assumed eligible for discount/indexation. Stylised annual-equivalent comparison; exact holding dates not modelled.','Model: return_form_neutrality')
d<-read('allowance_sensitivity')
emit('M08','Choosing the allowance rate changes what is taxable',d,line(d,'actual_real_return','tax_real','allowance_label')+geom_hline(yintercept=0,linetype=2)+scale_x_continuous(labels=pc)+scale_y_continuous(labels=label_dollar())+labs(x='Actual real return',y='Real tax with symmetric relief'),'Normal return',sim,'One-period real-dollar arithmetic on $100k at 30%. A risk-free allowance is distinct from exempting each investor\u2019s expected risky return. Negative tax assumes fully usable relief.','Model: allowance_sensitivity')
d<-read('unused_allowance')
emit('M09','Unused allowances need a time-value rule',d,line(d,'year','tax_on_normal_return','method')+scale_y_continuous(labels=money)+labs(y='Real tax on an asset earning exactly the normal return'),'Normal return',sim,'All return paid as terminal appreciation. Simple addition of unused annual allowances fails to preserve the compounded normal basis.','Model: unused_allowance')

# CGT design: all gains accrue after July 2027 unless transition is explicit.
g<-read('cgt_scenarios')
d<-g |>filter(years==10,base_income==70000,nominal_growth>=.026) |>select(nominal_growth,real_etr_old,real_etr_reform) |>pivot_longer(-nominal_growth,names_to='method',values_to='rate') |>mutate(method=recode(method,real_etr_old='50% nominal discount',real_etr_reform='Indexed gain + 30% minimum'))
emit('M10','The effective tax rate on real gains depends on the return',d,line(d,'nominal_growth','rate','method')+scale_x_continuous(labels=pc)+scale_y_continuous(labels=pc)+coord_cartesian(ylim=c(0,2))+labs(x='Annual nominal capital appreciation',y='Tax / real gain at sale'),'CGT design',sim,'$500k asset, 10 years, $70k other annual taxable income, CPI 2.5%. Frozen 2027-28 basic tax; Medicare and offsets omitted. Rates above 200% clipped visually; CSV retains them.','Model: cgt_scenarios')
d<-g |>filter(years==10,base_income==70000) |>select(nominal_growth,tax_discount,tax_nominal,tax_index,tax_reform) |>pivot_longer(-nominal_growth,names_to='method',values_to='tax') |>mutate(method=recode(method,tax_discount='50% discount',tax_nominal='Full nominal gain',tax_index='CPI indexation only',tax_reform='Indexation + minimum'))
emit('M11','Tax dollars: inflation adjustment and the minimum rate do different jobs',d,line(d,'nominal_growth','tax','method')+scale_x_continuous(labels=pc)+scale_y_continuous(labels=money)+labs(x='Annual nominal capital appreciation',y='Tax at sale'),'CGT design',sim,'Fresh $500k eligible asset, 10 years, CPI 2.5%, $70k other income. These are basic-tax illustrations, not full tax-return calculations.','Model: cgt_scenarios')
d<-g |>filter(base_income==70000,nominal_growth<=.10)
emit('M12','When does the reform raise or reduce tax in the illustration?',d,ggplot(d,aes(nominal_growth,factor(years),fill=reform_change/1000))+geom_tile()+scale_fill_gradient2(low='#20836b',mid='white',high='#a34b66',midpoint=0,name='Extra tax ($k)')+scale_x_continuous(labels=pc)+labs(x='Annual nominal appreciation',y='Holding years'),'CGT design',sim,'Difference: indexed/minimum tax minus old discount. $500k initial cost, CPI 2.5%, $70k other income. Only new-regime accruals, no grandfathered gains.','Model: cgt_scenarios')
d<-g |>filter(abs(nominal_growth-.06)<1e-8) |>mutate(base_label=paste0('$',format(base_income,big.mark=',',trim=TRUE),' other income'))
emit('M13','Holding period and other income both matter',d,line(d,'years','reform_change','base_label')+geom_hline(yintercept=0,linetype=2)+scale_y_continuous(labels=money)+labs(y='Reform minus old-discount tax at sale'),'CGT design',sim,'Fresh $500k asset growing 6% nominal; CPI 2.5%; other income fixed at sale. No claim about actual distribution of holding periods.','Model: cgt_scenarios')
d<-read('minimum_tax') |>mutate(gain_label=paste0('$',format(gain,big.mark=',',trim=TRUE),' indexed gain'))
emit('M14','The minimum tax is a top-up to basic tax, with specified exemptions',d,line(d,'base_income','average_gain_rate','case')+facet_wrap(~gain_label)+scale_x_continuous(labels=money)+scale_y_continuous(labels=pc)+labs(x='Other taxable income',y='Basic gain tax / indexed gain'),'CGT design',sim,'Frozen 2027-28 resident scale. 30% floor on eligible gain before offsets; no Medicare. Exemption illustrated as a legal status, not a prediction of benefit eligibility.','Model: minimum_tax; Act 49 Division 119',width=14)
d<-read('income_averaging') |>filter(annual_real_gain==15000) |>select(years,base_income,lump_tax,averaged_tax,lump_minimum) |>pivot_longer(c(lump_tax,averaged_tax,lump_minimum),names_to='method',values_to='tax') |>mutate(method=recode(method,lump_tax='Gain all in sale year',averaged_tax='Same gain spread equally',lump_minimum='Lump + 30% minimum'))
emit('M15','Progressive-tax bunching is separate from the value of deferral',d,line(d,'years','tax','method')+facet_wrap(~base_income,labeller=label_both)+scale_y_continuous(labels=money)+labs(y='Undiscounted total basic tax')+guides(colour=guide_legend(ncol=1)),'CGT design',sim,'$15k real annual gain, constant-dollar frozen thresholds; CPI already removed. No discounting here: isolates progressive bunching. Averaging is a counterfactual, not enacted policy.','Model: income_averaging',width=14,height=9)
d<-read('transition_illustration') |>select(pre_years,post_years,transition_tax,old_rules_all,hypothetical_all_new) |>pivot_longer(c(transition_tax,old_rules_all,hypothetical_all_new),names_to='method',values_to='tax') |>mutate(method=recode(method,transition_tax='Valuation split at July 2027',old_rules_all='Old rules for all gains',hypothetical_all_new='New rules for all gains'))
emit('M16','Transition: gains before and after July 2027 need separate treatment',d,line(d,'post_years','tax','method')+facet_wrap(~pre_years,labeller=label_both)+scale_y_continuous(labels=money)+labs(x='Years after July 2027',y='Tax paid at eventual sale')+guides(colour=guide_legend(ncol=1)),'CGT design',sim,'$500k original cost, 6% nominal appreciation, CPI 2.5%, other income $70k. Supplied market valuation; no losses, trusts or alternative Commissioner apportionment. Not a complete implementation calculator.','Model: transition_illustration',width=14,height=10)

# Risk and financing.
d<-read('deferral') |>filter(real_return==.05)
emit('R01','Deferral changes terminal wealth even when gains are fully taxed',d,line(d,'years','aftertax_real','method')+scale_y_continuous(labels=money)+guides(colour=guide_legend(ncol=2)),'Risk and financing',sim,'$100k already-taxed principal; 5% real return; 30% flat tax; 3% real RRA; no inflation, company tax or credit constraints in this real-dollar model. Half-rate comparator is not the nominal Australian discount.','Model: deferral',height=9)
d<-read('deferral')
emit('R02','Deferral expressed as an annual after-tax return',d,line(d,'years','effective_annual_real_return','method')+facet_wrap(~real_return,labeller=label_both)+scale_y_continuous(labels=pc)+labs(y='Effective annual real return')+guides(colour=guide_legend(ncol=2)),'Risk and financing',sim,'Same real-dollar flat-tax assumptions as R01. At exactly the RRA normal return, RRA tax is zero. No empirical investor-return forecast.','Model: deferral',width=14,height=9)
d<-read('lock_in') |>mutate(embedded=paste0(embedded_gain_share*100,'% embedded gain'))
emit('R03','How much better must a replacement investment be to justify sale?',d,line(d,'alternative_return','difference','embedded')+geom_hline(yintercept=0,linetype=2)+scale_x_continuous(labels=pc)+scale_y_continuous(labels=money)+labs(x='Replacement investment annual real return',y='Switch minus keep: net wealth after 10 years'),'Risk and financing',sim,'Current asset earns 5% real; 30% full real CGT. Sale taxes embedded gain, reducing reinvestable funds; both paths liquidated after 10 years. No transaction costs.','Model: lock_in')
d<-read('loss_delay') |>mutate(rate=paste0(discount_rate*100,'% discount rate'))
emit('R04','A future loss deduction is worth less than immediate relief',d,line(d,'delay','deferred_relief_pv','rate')+scale_y_continuous(labels=label_dollar())+labs(x='Years until a loss can be used',y='Present value of $3,000 tax relief'),'Risk and financing',sim,'$10k loss at 30% tax. Certain eventual use is assumed; expiry or never realising an offsetting gain would reduce value further.','Model: loss_delay')
d<-read('risk_sharing')
emit('R05','Taxing gains and sharing losses have different risk effects',d,bar(d,'treatment','payoff','state')+geom_hline(yintercept=0)+scale_y_continuous(labels=money)+scale_x_discrete(labels=function(x)vapply(x,wrap,character(1),w=22)),'Risk and financing',sim,'Success +$200k, failure -$100k, flat 30% tax. Restored scale multiplies investment by 1/(1-t). Requires finance and fully symmetric relief; not an actual CGT loss rule.','Model: risk_sharing')
d<-read('risk_certainty_equivalent')
emit('R06','Risk evaluation depends on the loss rule and the investor',d,line(d,'prob_success','certainty_equivalent','system')+geom_hline(yintercept=0,linetype=2)+scale_x_continuous(labels=pc)+scale_y_continuous(labels=money)+labs(x='Assumed probability of success',y='Certainty-equivalent project payoff'),'Risk and financing',sim,'Illustrative log utility, $300k initial wealth, unscaled +$200k/-$100k project. Not estimated preferences, probabilities or welfare effects.','Model: risk_certainty_equivalent')
d<-read('leverage_one_year')
emit('R07','Leverage interacts with deductions and the definition of taxable return',d,line(d,'ltv','roe','method')+geom_hline(yintercept=0,linetype=2)+scale_x_continuous(labels=pc)+scale_y_continuous(labels=pc)+labs(x='Loan / asset value',y='One-period after-tax nominal return on equity')+guides(colour=guide_legend(ncol=1)),'Risk and financing',sim,'$500k asset, growth 4%, rent net of non-interest costs 2.5%, borrowing 6.5%, flat tax 30%. Immediate relief on negative net taxable income is assumed; eligible longer-held gain treated at annual equivalent.','Model: leverage_one_year',height=10)
d<-read('interest_inflation_symmetry') |>mutate(ltv_label=paste0(ltv*100,'% borrowing'))
emit('R08','Indexing gains raises a parallel question about interest deductions',d,line(d,'inflation','extra_nominal_deduction_tax','ltv_label')+scale_x_continuous(labels=pc)+scale_y_continuous(labels=label_dollar())+labs(x='Annual inflation',y='Tax value of inflation component in interest deduction'),'Risk and financing',sim,'$500k asset; inflation compensation on nominal debt principal; 30% flat tax. A comprehensive real-income system also adjusts lenders\u2019 interest receipts. This is not a costing.','Model: interest_inflation_symmetry')
d<-read('imputation') |>select(company_rate,personal_rate,total_tax,without_credit) |>pivot_longer(c(total_tax,without_credit),names_to='method',values_to='tax') |>mutate(method=recode(method,total_tax='Full usable imputation',without_credit='Company + shareholder tax, no credit'),rate_label=paste0(personal_rate*100,'%'))
emit('R09','Company tax cannot simply be added to personal tax on dividends',d,bar(d,'rate_label','tax','method')+facet_wrap(~company_rate,labeller=label_both)+scale_y_continuous(labels=label_dollar())+labs(x='Personal tax rate',y='Total tax on $10k pre-company profit'),'Risk and financing',sim,'One period, immediate full distribution and fully usable/refundable credits. No claim that credits are fully capitalised in share prices, or all shareholders receive them. Retained gains need a fuller model.','Model: imputation')
d<-read('existing_wealth_transition')
emit('R10','Matching saving incentives does not make transition burdens identical',d,line(d,'year','consumption','system')+facet_wrap(~origin)+scale_y_continuous(labels=money)+labs(y='Real consumption financed by initial resource')+guides(colour=guide_legend(ncol=1)),'Normal return',sim,'3% normal real return. Hypothetical new comprehensive consumption tax at 42.86%, without compensation, versus 30% labour tax + RRA. Existing GST and transition compensation are not modelled.','Model: existing_wealth_transition',width=14,height=9)
d<-read('real_losses')
emit('R11','CPI indexation need not give symmetric relief for real losses',d,line(d,'nominal_return','tax','system')+geom_hline(yintercept=0,linetype=2)+geom_vline(xintercept=c(0,.025),linetype=3)+scale_x_continuous(labels=pc)+scale_y_continuous(labels=label_dollar())+labs(x='One-period nominal asset return',y='Tax less present value of assumed loss relief')+guides(colour=guide_legend(ncol=1)),'Risk and financing',sim,'$100k cost, CPI 2.5%, flat tax 30%. Indexation does not create/increase a nominal capital loss. Assumed future capital-loss use is discounted at 5.575%; no cash refund is promised by CGT.','Model: real_losses; Act 49 subsection 100-40',height=9)
d<-read('own_work_gains')
emit('M17','Some apparent capital gains can embody the owner\u2019s own work',d,line(d,'own_work_hours','tax','method')+scale_y_continuous(labels=money)+labs(x='Assumed hours of owner work',y='Tax on eligible gain at sale'),'Normal return',sim,'Stylised fixer-upper: $500k purchase plus $50k cash improvements at inception, held 2 years, 6% growth, work adds $60/hour to sale value. Unpaid own work has no recorded cost deduction. Abstract CGT case; excludes main-residence/trading rules.','Model: own_work_gains')

# International evidence and behavioural arithmetic.
d<-read('literature')
emit('E01','Published behavioural numbers use different definitions',d,ggplot(d,aes(estimate,reorder(study,estimate),colour=country,shape=kind))+geom_segment(aes(x=low,xend=high,yend=reorder(study,estimate)),linewidth=.8)+geom_point(data=d |>filter(source_id!='canada'),size=3)+facet_wrap(~definition,scales='free',ncol=1)+labs(y=NULL,x='Native reported parameter; signs and definitions differ')+guides(colour=guide_legend(ncol=4)),'Behaviour','Original-author/official-costing parameters; ranges show specifications, not confidence intervals','Canada WP2021 ranges shown without point estimates. OBR uses scoring assumptions. Do not pool these into a single Australian elasticity. Native reference rates and caveats are in the CSV.','Linked original sources in literature.csv',width=14,height=13)
d<-read('literature') |>filter(study %in% c('Minas, Lim & Evans (2018)','Agersnap & Zidar (2021), baseline','OBR (2025), main rates','OBR (2025), business relief')) |>mutate(label=paste0(study,'\nEvaluated at ',round(reference_rate*100,2),'%'))
emit('E02','Converting to a tax-rate elasticity still requires a reference rate',d,ggplot(d,aes(tax_elasticity,reorder(label,tax_elasticity),colour=kind))+geom_point(size=4)+geom_vline(xintercept=0,linetype=2)+labs(y=NULL,x='Elasticity of gains with respect to tax rate'),'Behaviour','Definition conversion, not harmonised identification or an Australian calibration','e_tax = beta*t for semi-log; e_tax = -e_net*t/(1-t). OBR conversion rates are illustrative; these are not equivalent estimands.','literature.csv; 02_models.R',width=14,height=8)
d<-read('behaviour_scenarios')
emit('E03','The same local elasticity gives different responses to large tax changes',d,line(d,'tax_rate','gains_index','elasticity_label')+facet_wrap(~form)+scale_x_continuous(labels=pc)+labs(x='Tax rate on a fixed hypothetical base',y='Realised gains index; 100 at 22%'),'Behaviour',sim,'Constant semi-elasticity and constant net-of-tax forms match locally at 22%, then diverge. Assumed parameters, not a simulation of the Australian base/indexation reforms.','Model: behaviour_scenarios',width=14,height=9)
emit('E04','Revenue curves depend on the behavioural model',d,line(d,'tax_rate','revenue_index','elasticity_label')+facet_wrap(~form)+scale_x_continuous(labels=pc)+labs(x='Tax rate on a fixed hypothetical base',y='Receipts index; 100 at 22%'),'Behaviour',sim,'Gains base starts at index 100. Peaks are functional-form illustrations, not estimates of Australia\u2019s revenue-maximising or welfare-maximising rate.','Model: behaviour_scenarios',width=14,height=9)
d<-read('finite_revenue_change') |>mutate(new_rate_label=paste0('Tax rises 22% to ',new_rate*100,'%'))
emit('E05','How much of a static revenue increase survives a response?',d,line(d,'elasticity','fraction_static','new_rate_label')+geom_hline(yintercept=c(0,1),linetype=2)+scale_y_continuous(labels=pc)+labs(x='Local tax-rate elasticity at 22%',y='Behavioural gain / static gain'),'Behaviour',sim,'Finite increases under constant semi-elasticity. The shortcut 1+elasticity applies only locally. Does not adjust published Budget costings, which already incorporate Treasury judgements.','Model: finite_revenue_change')
d<-read('retiming')
emit('E06','A surge before a tax change can be followed by a slump',d,line(d,'year','gains','case')+geom_vline(xintercept=-.5,linetype=2)+labs(x='Years relative to reform',y='Realised gains; unchanged baseline = 100'),'Behaviour',sim,'Pure timing illustration shifts 30 units into year -1 and removes 10 in each of years 0-2. Total gains conserved over the full window; no permanent response assumed.','Model: retiming')
d<-read('income_reclassification') |>mutate(label=paste0(share_reclassified*100,'% reappears as income'))
emit('E07','Fewer reported capital gains need not mean the same fall in total revenue',d,line(d,'disappearing_cgt','total_receipt_change','label')+geom_hline(yintercept=0,linetype=2)+labs(x='Units of gains disappearing from the CGT base',y='Combined CGT and income-tax receipt change'),'Behaviour',sim,'CGT 30%, other income tax 45%; assumed reclassification shares. This is an accounting sensitivity, not evidence about the amount of shifting in Australia.','Model: income_reclassification')

# Empirical context for choosing an allowance: bond rates, not risky returns.
bonds<-read('rba_bonds') |>mutate(date=as.Date(date)) |>filter(date>=as.Date('2013-01-01'),series %in% c('FCMYGBAG10','FCMYGBAGI')) |>mutate(series=recode(series,FCMYGBAG10='Nominal 10-year government yield',FCMYGBAGI='Indexed 10-year government yield'))
emit('Q01','Observed bond yields provide context for a normal allowance',bonds,line(bonds,'date','value','series')+labs(y='Percent per year'),'Economic context','Observed RBA monthly series, cut at 11 November 2026','Nominal and indexed yields contain term/liquidity premia and are not an automatic policy allowance. Indexed-bond yield is not ex-post realised saving return.','RBA F2.1')
d<-read('rba_cpi') |>filter(series=='GCPIAGYP') |>mutate(date=as.Date(date)) |>filter(date>=as.Date('1985-01-01'))
emit('Q02','Inflation has varied materially across Australia\u2019s CGT history',d,line(d,'date','value')+geom_hline(yintercept=2.5,linetype=2)+labs(y='Year-ended headline inflation (%)'),'Economic context','Observed quarterly headline CPI inflation, ABS series republished by RBA','Dashed 2.5% is the model assumption, not a forecast. Inflation indexation and an interest-rate allowance answer different questions.','ABS / RBA G1')

cat<-bind_rows(catalogue);write_csv(cat,file.path(fig,'chart_catalogue.csv'))
esc<-function(x){x<-gsub('&','&amp;',x,fixed=TRUE);x<-gsub('<','&lt;',x,fixed=TRUE);gsub('"','&quot;',x,fixed=TRUE)}
cards<-vapply(seq_len(nrow(cat)),function(i){r<-cat[i,];paste0('<article data-search="',esc(tolower(paste(r$id,r$title,r$group,r$status))),'" id="',r$id,'"><p class="tag">',esc(r$group),' | ',esc(r$status),'</p><h2>',r$id,' ',esc(r$title),'</h2><a href="',r$png,'"><img loading="lazy" src="',r$png,'" alt="',esc(r$title),'"></a><p>',esc(r$note),'</p><p class="source">Source: ',esc(r$sources),'</p><p><a href="',r$png,'">PNG</a> &middot; <a href="',r$svg,'">SVG</a> &middot; <a href="',r$csv,'">Plotted CSV</a></p></article>')},character(1))
manifest<-read('source_manifest')
source_links<-paste0('<li><a href="',esc(manifest$url),'">',esc(manifest$source_id),'</a>: ',esc(manifest$description),' (<a href="../raw/',manifest$file,'">saved source</a>)</li>',collapse='\n')
html<-paste0('<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Australia CGT research</title><style>body{max-width:1180px;margin:35px auto;padding:0 20px;font:17px/1.55 system-ui;color:#233744;background:#f5f7f8}h1{font-size:36px}h2{font-size:23px}a{color:#146b8c}input{font:inherit;width:95%;padding:12px;border:1px solid #91a7b0;border-radius:5px;position:sticky;top:8px;background:white;z-index:2}article{background:white;padding:24px;margin:25px 0;border-radius:8px;border:1px solid #d6e0e4}img{width:100%;height:auto}.tag,.source{font-size:14px;color:#58646f}.note{background:#e6f0f4;padding:18px}li{margin:9px 0}</style><h1>Australia: capital gains, normal returns and tax reform</h1><p>Research snapshot: 11 November 2026. ',nrow(cat),' charts with exportable images and exact plotted data.</p><p><a href="../RESEARCH_GUIDE.md">Research guide</a> &middot; <a href="../README.md">Reproduction</a> &middot; <a href="../processed/key_findings.csv">Numeric notes</a> &middot; <a href="../processed/literature.csv">Behavioural evidence</a> &middot; <a href="../POLICY_STATUS.md">Policy status</a></p><p class="note">Observed returns, official estimates, and hypothetical experiments are labelled separately. The normal-return comparison is conditional; it does not establish identical distributional or transition effects to GST. Budget costings combine CGT and negative gearing. ATO trust-origin gains lack asset look-through. Read the source and model caveats before quoting a number.</p><input id="q" aria-label="Filter charts" placeholder="Search charts, e.g. normal return, losses, distribution, Budget"><p id="count">',nrow(cat),' charts</p>',paste(cards,collapse='\n'),'<h2>Public source downloads</h2><ul>',source_links,'</ul><p>OBR and Dowd/McClelland direct downloads were denied; their original-source parameters were web-verified and transcribed with locators in literature.csv. Source availability is recorded separately. The slides are user-provided context, not a public data source.</p><script>document.getElementById("q").addEventListener("input",function(){let n=0;const q=this.value.toLowerCase();document.querySelectorAll("article").forEach(c=>{c.hidden=!c.dataset.search.includes(q);if(!c.hidden)n++});document.getElementById("count").textContent=n+" charts"})</script></html>')
writeLines(html,file.path(fig,'index.html'),useBytes=TRUE)
message('Finished ',nrow(cat),' charts and searchable gallery.')

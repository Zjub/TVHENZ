# Reproducible presentation charts; no package installation or network access.
required <- c('ggplot2','dplyr','tidyr','readr','scales','svglite')
stopifnot(all(vapply(required,requireNamespace,logical(1),quietly=TRUE)))
suppressPackageStartupMessages({library(ggplot2);library(dplyr);library(tidyr);library(readr)})
arg <- grep('^--file=',commandArgs(FALSE),value=TRUE)
root <- dirname(normalizePath(sub('^--file=','',arg[1])))
out <- file.path(root,'figures'); dir.create(out,showWarnings=FALSE)
read <- function(n) read_csv(file.path(root,'data',paste0(n,'.csv')),show_col_types=FALSE)
g <- read('gdp_quarterly') |> mutate(date=as.Date(date))
l <- read('labour_quarterly') |> mutate(date=as.Date(date))
colours <- c('Australia'='#C28000','New Zealand'='#007C91')
theme_set(theme_minimal(base_size=18,base_family='Arial') + theme(
 plot.title=element_text(face='bold',size=25,colour='#142D40'),
 plot.subtitle=element_text(size=15,margin=margin(b=15)),
 plot.caption=element_text(size=10,hjust=0,colour='#465563',margin=margin(t=12)),
 axis.title=element_text(size=14),axis.text=element_text(size=13),
 axis.line.x=element_line(colour='#84939E',linewidth=.3),axis.ticks.x=element_line(colour='#84939E'),
 panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
 legend.position='top',legend.title=element_blank(),legend.text=element_text(size=14),
 plot.caption.position='plot',plot.title.position='plot',
 plot.margin=margin(15,25,15,15),plot.background=element_rect(fill='white',colour=NA)))
catalog <- list()
save <- function(id,p,d,title,x,y,note,priority='Appendix') {
 p <- p+labs(title=title,x=x,y=y,caption=paste0(note,'\nSnapshot: 12 November 2026. Calculations and exact plotted data accompany this chart.'))
 ggsave(file.path(out,paste0(id,'.png')),p,width=14,height=7.875,dpi=200,bg='white')
 ggsave(file.path(out,paste0(id,'.svg')),p,width=14,height=7.875,device=svglite::svglite,bg='white')
 ggsave(file.path(out,paste0(id,'.pdf')),p,width=14,height=7.875,device=cairo_pdf,bg='white')
 write_csv(d,file.path(out,paste0(id,'.csv')))
 catalog[[length(catalog)+1L]] <<- tibble(id,title,x_axis=x,y_axis=y,note,priority)
}
line <- function(d,y='value') ggplot(d,aes(x=date,y=.data[[y]],colour=country))+geom_line(linewidth=1.15)+scale_colour_manual(values=colours)+scale_x_date(date_breaks='2 years',date_labels='%Y')
src <- 'Sources: Stats NZ June 2026 GDP CSV; ABS June 2026 Table 1. Real, seasonally adjusted expenditure GDP.'
common <- g |> filter(measure %in% c('Real GDP','Real GDP per person'),date>=as.Date('2019-12-01'))
for (m in c('Real GDP','Real GDP per person')) {
 d<-common |> filter(measure==m)
 id<-if(m=='Real GDP') 'P01' else 'P02'
 title<-if(m=='Real GDP') 'The economies grew; growth per person was much weaker' else 'GDP per person has barely moved since the 2023 election'
 save(id,line(d,'index_2019_12')+geom_hline(yintercept=100,linetype=3,colour='#777777'),d,title,'Quarter','Index: December 2019 = 100',paste(src,'Indices compare growth, not income levels.'),'Main deck')
}
d<-g |> filter(country=='New Zealand',date>=as.Date('2019-12-01'),measure %in% c('Real GDP per person','Real GDP production per person'))
p<-ggplot(d,aes(date,index_2019_12,colour=measure))+geom_line(linewidth=1.2)+scale_colour_manual(values=c('#007C91','#7955A1'),labels=c('Expenditure measure','Production measure'))+geom_hline(yintercept=100,linetype=3)+scale_x_date(date_breaks='2 years',date_labels='%Y')
save('P03',p,d,'New Zealand GDP measures tell different recovery stories','Quarter','Real GDP per person: December 2019 = 100','Source: Stats NZ June 2026 GDP CSV. Production is the NZ headline; expenditure is used for the ABS comparison.')
d<-common |> filter(measure=='Real GDP')
save('P04',line(d,'yoy')+geom_hline(yintercept=0,colour='#777777'),d,'Recent recovery is not the same as a strong whole term','Quarter','Change from the same quarter a year earlier (%)',paste(src,'Year-on-year growth; not annual-average growth.'))
d<-common |> filter(date==max(date)) |> select(country,measure,index_2023_09) |> mutate(change=index_2023_09-100,label=sprintf('%.1f%%',if_else(abs(change)<.05,0,change)))
p<-ggplot(d,aes(country,change,fill=country))+geom_col(width=.55)+geom_text(aes(label=label),vjust=-.5,size=6)+facet_wrap(~measure)+scale_fill_manual(values=colours)+guides(fill='none')+geom_hline(yintercept=0)+scale_y_continuous(expand=expansion(mult=c(.12,.2)))
save('P05',p,d,'Since September 2023, per-person output is roughly flat','Country','Real growth: September 2023 to June 2026 (%)',paste(src,'This aligns dates; it does not identify the causal effect of either government.'),'Main deck')
for (m in c('Unemployment rate','Employment rate','Participation rate')) {
 d<-l |> filter(measure==m,date>=as.Date('2015-03-01'))
 id<-c('Unemployment rate'='P06','Employment rate'='P08','Participation rate'='P09')[[m]]
 title<-c('Unemployment rate'='New Zealand unemployment has risen further', 'Employment rate'='Higher unemployment coexists with a higher employment rate','Participation rate'='New Zealand has a larger share of adults in the labour force')[[m]]
 save(id,line(d),d,title,'Quarter',paste0(m,' (%)'),'Sources: Stats NZ HLFS, September 2026; ABS Labour Force Table 1, September 2026. Seasonally adjusted, age 15+.\nAustralian observations are averages of three monthly rates; national survey scope and composition differ.',if(m=='Unemployment rate') 'Main deck' else 'Appendix')
}
d<-l |> filter(measure=='Unemployment rate',date>=as.Date('2015-03-01')) |> select(country,date,value) |> pivot_wider(names_from=country,values_from=value) |> mutate(gap=`New Zealand`-Australia)
p<-ggplot(d,aes(date,gap))+geom_hline(yintercept=0,colour='#667788')+geom_area(fill='#007C91',alpha=.2)+geom_line(colour='#007C91',linewidth=1.1)+scale_x_date(date_breaks='2 years',date_labels='%Y')
save('P07',p,d,'The unemployment gap has turned against New Zealand','Quarter','NZ minus Australian unemployment (percentage points)','Sources: Stats NZ HLFS; ABS Labour Force. Australia: quarterly average of monthly seasonally adjusted rates.')
d<-l |> filter(country=='New Zealand',date>=as.Date('2015-03-01'),measure %in% c('Unemployment rate','Underutilisation rate'))
p<-ggplot(d,aes(date,value,colour=measure))+geom_line(linewidth=1.15)+scale_colour_manual(values=c('#007C91','#AD4A3F'))+scale_x_date(date_breaks='2 years',date_labels='%Y')
save('P10',p,d,'Unemployment captures only part of unused labour capacity','Quarter','Rate (%)','Source: Stats NZ September 2026 HLFS. Underutilisation includes unemployed, underemployed and potential labour force.\nThe underutilisation denominator is the extended labour force; the unemployment denominator is the labour force.')
e<-read('election_preliminary')
partycols<-c('National'='#0077BD','Labour'='#D52941','Greens'='#279B48','ACT'='#E43E94','NZ First'='#666666','Opportunity'='#00A8A5','Te Pati Maori'='#9B2428')
d<-e |> select(party,vote_pct,seat_pct) |> pivot_longer(-party,names_to='measure',values_to='share') |> mutate(measure=recode(measure,vote_pct='Party vote',seat_pct='Seats'),party=factor(party,levels=rev(e$party)))
p<-ggplot(d,aes(share,party,fill=measure))+geom_col(position=position_dodge(width=.75),width=.65)+scale_fill_manual(values=c('Party vote'='#007C91','Seats'='#C28000'))+scale_x_continuous(expand=expansion(mult=c(0,.08)))
save('P11',p,d,'Party votes and seats differ because of overhangs','Share of valid party votes / parliamentary seats (%)','Party','Source: Electoral Commission preliminary results, 7 November 2026. 131 seats, including 11 above the usual 120.\nPercentages calculated from exact counts; special votes excluded. Official results due 27 November.','Main deck')
d<-read('coalition_arithmetic') |> mutate(group=factor(group,levels=rev(group)))
p<-ggplot(d,aes(seats,group))+geom_col(fill='#007C91',width=.55)+geom_vline(xintercept=66,linetype=2,colour='#A33C34',linewidth=1)+geom_text(aes(label=seats),hjust=1.25,size=6,colour='white')+scale_x_continuous(limits=c(0,87),breaks=seq(0,80,10))
save('P12',p,d,'The incumbent bloc has 67 of 131 preliminary seats','Parliamentary seats (66 for an absolute majority)','Arithmetic grouping','Source: Electoral Commission preliminary results. TPM = Te Pati Maori. Groups are arithmetic examples, not negotiated agreements.\nSpecial votes can change party entitlements, electorate winners, total seats and the majority threshold.','Main deck')
d<-e |> select(party,electorate,list) |> pivot_longer(-party,names_to='type',values_to='seats') |> mutate(party=factor(party,levels=rev(e$party)))
p<-ggplot(d,aes(seats,party,fill=type))+geom_col(width=.65)+scale_fill_manual(values=c('electorate'='#007C91','list'='#C28000'),labels=c('Electorate seats','List seats'))
save('P13',p,d,'National has no list seats in the preliminary allocation','Parliamentary seats','Party','Source: Electoral Commission preliminary results. Losing an electorate does not by itself exclude a candidate from Parliament.\nA list candidate needs room in the party entitlement after electorate MPs have been included.')
d<-read('prefu_fiscal') |> filter(block=='Share GDP',measure %in% c('Core Crown expenses','Core Crown tax revenue')) |> mutate(value=100*value)
# Connect the final observed year to the forecast, preserving source status.
d<-d |> mutate(segment=if_else(status=='Forecast','Forecast','Observed'))
p<-ggplot(d,aes(year,value,colour=measure))+geom_line(linewidth=.9)+geom_point(aes(shape=segment),size=2.5)+scale_colour_manual(values=c('#007C91','#C28000'))+scale_shape_manual(values=c('Forecast'=1,'Observed'=16))+scale_x_continuous(breaks=seq(2017,2031,2))
save('P14',p,d,'Coalition choices sit inside a constrained fiscal outlook','Year ended June','Share of GDP (%)','Source: NZ Treasury PREFU September 2026, Fiscal Time Series. Open markers are forecasts; filled markers actual/unaudited actual.\nCore Crown tax revenue excludes other revenue: the distance between these lines is not the operating balance.')
d<-read('prefu_fiscal') |> filter(block=='Share GDP',measure=='OBEGAL (excluding ACC)') |> mutate(value=100*value,stage=if_else(status=='Forecast','Forecast','Observed'))
p<-ggplot(d,aes(year,value,fill=stage))+geom_col(width=.7)+geom_hline(yintercept=0)+scale_fill_manual(values=c('Forecast'='#C28000','Observed'='#007C91'))+scale_x_continuous(breaks=seq(2017,2031,2))
save('P15',p,d,'The operating-balance improvement is largely a forecast','Year ended June','OBEGAL excluding ACC (% of GDP)','Source: NZ Treasury PREFU September 2026, Fiscal Time Series. Forecasts are conditional Treasury projections, not realised savings.')
d<-g |> filter(measure=='Real GDP per person',date>=as.Date('2000-03-01'))
save('P16',line(d,'index_2000_03'),d,'A longer view changes the growth comparison','Quarter','Real GDP per person: March 2000 = 100',paste(src,'Growth indices do not establish which country has a higher income level.'))
d<-l |> filter(measure=='Unemployment rate',date>=as.Date('1986-03-01'))
save('P17',line(d)+scale_x_date(date_breaks='5 years',date_labels='%Y'),d,'Today\'s unemployment rise in historical perspective','Quarter','Unemployment rate (%)','Sources: Stats NZ HLFS and ABS Labour Force, September 2026 vintages. Seasonally adjusted; Australia quarterly monthly-rate average.\nSurvey redesigns affect long-run comparability; see the release methodology before interpreting small differences.')
h<-read('hlpi') |> filter(level=='All groups',hlpi_name %in% c('All households','Beneficiary','Superannuitant','Income quintile 1 (low)')) |> mutate(date=as.Date(date))
b<-h |> filter(date==as.Date('2019-12-01')) |> select(hlpi_name,base=index)
d<-h |> left_join(b,by='hlpi_name') |> filter(date>=as.Date('2019-12-01')) |> mutate(rebased=100*index/base)
p<-ggplot(d,aes(date,rebased,colour=hlpi_name))+geom_line(linewidth=1.1)+scale_colour_manual(values=c('#007C91','#C28000','#AD4A3F','#7955A1'))+scale_x_date(date_breaks='2 years',date_labels='%Y')
save('P18',p,d,'Slower inflation has not reversed the increase in living costs','Quarter','Household living-cost index: December 2019 = 100','Source: Stats NZ September 2026 Household Living-costs Price Indexes. These household cost baskets include interest payments.\nThey differ from the CPI; the lines measure prices, not disposable income or affordability.','Main deck')
d<-tibble(flips=0:6,right=67-flips,other=64,total=131-flips,majority=floor(total/2)+1) |> pivot_longer(c(right,other,majority),names_to='measure',values_to='seats')
p<-ggplot(d,aes(flips,seats,colour=measure,linetype=measure))+geom_line(linewidth=1.2)+geom_point(size=3)+scale_colour_manual(values=c('right'='#007C91','other'='#C28000','majority'='#444444'),labels=c('majority'='Absolute majority threshold','other'='Other four parties combined','right'='National + ACT + NZ First'))+scale_linetype_manual(values=c('majority'='dashed','other'='solid','right'='solid'),labels=c('majority'='Absolute majority threshold','other'='Other four parties combined','right'='National + ACT + NZ First'))+scale_x_continuous(breaks=0:6)+scale_y_continuous(breaks=seq(61,67))
save('P19',p,d,'Two electorate flips alone would not reverse the majority','National electorates switching to Labour (illustrative)','Parliamentary seats','Source: Electoral Commission preliminary results; mechanical scenario with fixed party-vote entitlements and National-to-Labour flips.\nLabour substitutes electorate MPs for list MPs; National loses overhang seats. This is not a special-vote forecast.')
if(file.exists(file.path(root,'data','special_vote_hurdles.csv'))) source(file.path(root,'06_special_vote_graphs.R'))
cat<-bind_rows(catalog) |> arrange(id);write_csv(cat,file.path(out,'chart_catalogue.csv'))
html<-c('<!doctype html><html><head><meta charset="utf-8"><title>NZ election: charts for an Australian audience</title><style>body{font:17px Arial;max-width:1150px;margin:35px auto;padding:0 20px;color:#142d40}img{width:100%}article{border-top:1px solid #ddd;margin-top:35px}a{color:#007c91}small{line-height:1.6}</style></head><body>',
 '<h1>NZ election: charts for an Australian audience</h1><p>Snapshot: 12 November 2026. Preliminary election results; GDP through June 2026; labour through September 2026.</p><p><a href="../SLIDE_ADVICE.md">Slide advice</a> | <a href="../SLIDE_ADVICE.pdf">Advice PDF</a> | <a href="../SPECIAL_VOTES.pdf">Special-vote analysis (P20-P23)</a> | <a href="../Chart_pack.pptx">PowerPoint chart pack</a> | <a href="../Chart_pack.pdf">Chart pack PDF</a> | <a href="../data/series_selection.csv">Source series</a></p>')
for(i in seq_len(nrow(cat))) {r<-cat[i,];html<-c(html,sprintf('<article><h2>%s: %s</h2><p>%s</p><img src="%s.png" alt="%s"><p><a href="%s.png">PNG</a> | <a href="%s.svg">SVG</a> | <a href="%s.pdf">PDF</a> | <a href="%s.csv">Exact chart data</a></p></article>',r$id,r$title,r$priority,r$id,r$title,r$id,r$id,r$id,r$id))}
writeLines(c(html,'</body></html>'),file.path(out,'index.html'),useBytes=TRUE)
message('Saved ',nrow(cat),' charts in PNG, SVG, PDF and CSV.')

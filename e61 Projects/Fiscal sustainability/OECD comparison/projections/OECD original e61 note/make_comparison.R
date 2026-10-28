# Standalone note chart. Uses the ORIGINAL saved e61 projection, not Shiny output.
args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(dirname(script),mustWork=TRUE))
suppressPackageStartupMessages(library(theme61))
suppressPackageStartupMessages(library(ggplot2))
dir.create('outputs',showWarnings=FALSE)

original<-as.data.frame(readxl::read_excel('data/original_graph_proj_all.xlsx'))
e61<-original[original$variable=='debt',c('year','value')]
stopifnot(!anyDuplicated(e61$year),all(is.finite(e61$value)))
e61$series<-'e61 projections'
e61$provider<-'e61';e61$source_file<-'OECD comparison/graph_proj_all.xlsx'
e61$source_locator<-"Sheet1: variable='debt'; original script uses debt_no_rev (ramp_revenue=FALSE)"
e61$source_url<-''

w<-as.data.frame(suppressMessages(readxl::read_excel('data/oecd_survey_figure_1_14.xlsx',sheet='fig1-14',col_names=FALSE)))
stopifnot(grepl('Gross public debt',w[[10]][30],fixed=TRUE),grepl('fiscal adjustment',w[[13]][31],fixed=TRUE))
years<-suppressWarnings(as.integer(w[[10]]))
keep<-is.finite(years)&is.finite(suppressWarnings(as.numeric(w[[12]])))&is.finite(suppressWarnings(as.numeric(w[[13]])))
oecd<-do.call(rbind,lapply(c('without','with'),function(case){
 data.frame(year=years[keep],value=as.numeric(w[[if(case=='without')12 else 13]][keep]),
   series=if(case=='without')'OECD: without fiscal adjustment' else 'OECD: with fiscal adjustment',
   provider='OECD',source_file='OECD Economic Surveys: Australia 2026 StatLink workbook',
   source_locator=paste('Figure 1.14 panel B; fig1-14 column',if(case=='without')'L' else 'M'),
   source_url='https://stat.link/gdxr47')
}))
d<-rbind(e61,oecd);d<-d[order(d$series,d$year),]
d$measure<-'General government gross debt (% GDP)'
d$notes<-'Original source levels and annual year labels retained; no rebasing, calendar/fiscal conversion, extrapolation or model rerun.'
write.csv(d,'data/comparison_source_series.csv',row.names=FALSE)
chart<-d[d$year>=2030&d$year<=2050,]
labels<-c('OECD: without fiscal adjustment','e61 projections','OECD: with fiscal adjustment')
chart$series<-factor(chart$series,levels=labels)
stopifnot(all(table(chart$series)==21L),!anyDuplicated(chart[c('series','year')]),
 abs(subset(chart,provider=='e61'&year==2050)$value-71.17)<1e-4,
 abs(subset(chart,series==labels[1]&year==2050)$value-113.22773700100977)<1e-7,
 abs(subset(chart,series==labels[3]&year==2050)$value-57.282972469294037)<1e-7)
# Verify the original series is preserved exactly; no rerun or tuning.
original_e61<-original[original$variable=='debt'&original$year>=2030&original$year<=2050,]
plotted_e61<-chart[chart$provider=='e61',]
stopifnot(identical(plotted_e61$value,original_e61$value[match(plotted_e61$year,original_e61$year)]))
write.csv(chart,'outputs/oecd_original_e61_gross_debt_chart_data.csv',row.names=FALSE)
endpoints<-chart[chart$year%in%c(2030,2040,2050),c('year','series','value')]
write.csv(endpoints,'outputs/comparison_endpoints.csv',row.names=FALSE)

palette<-setNames(c(palette_e61(3)[3],palette_e61(3)[2],palette_e61(3)[1]),labels)
p<-theme61::ggplot(chart,aes(year,value,colour=series,linetype=series))+
 geom_line(linewidth=.95)+
 scale_colour_manual(values=palette)+
 scale_linetype_manual(values=setNames(c('longdash','solid','dotted'),labels))+
 scale_x_continuous(breaks=seq(2030,2050,5),limits=c(2030,2050),expand=expansion(mult=c(.015,.015)))+
 scale_y_continuous(breaks=seq(0,120,20),limits=c(0,120),expand=expansion(mult=c(0,.025)))+
 theme61::theme_e61(legend='none',aspect_ratio=.55)+
 guides(colour='none',linetype='none')+
 theme61::plab(label=labels,x=c(2049,2049,2049),y=c(114,75,51),
   colour=unname(palette),hjust=1,auto_position=FALSE)+
 theme61::labs_e61(title='Australian general government gross debt',
   subtitle='e61 projection and OECD fiscal scenarios',
   x='Year',y='% of GDP',colour=NULL,linetype=NULL,
   sources=c('e61-McKinnon Fiscal Report','OECD Economic Surveys: Australia 2026, Figure 1.14'),
   footnotes=c('e61 case holds revenue/GDP flat. Each series retains its original level and assumptions.',
     'Projection years 2030-2050; no rebasing or extension of source series.'))
for(format in c('png','pdf','svg')){
 theme61::save_e61(filename=file.path('outputs',paste0('oecd_original_e61_gross_debt.',format)),plot=p,
   dim=list(width=18.6),format=format,base_size=10,res=2,spell_check=FALSE,preview=FALSE)
}
sources<-c('data/original_graph_proj_all.xlsx','data/original_debt_projection_source.R','data/oecd_survey_figure_1_14.xlsx')
manifest<-data.frame(file=sources,md5=unname(tools::md5sum(sources)),snapshot_date='2026-10-28')
write.csv(manifest,'data/source_manifest.csv',row.names=FALSE)
writeLines(c(paste('R',getRversion()),paste('theme61',packageVersion('theme61')),
 paste('ggplot2',packageVersion('ggplot2')),paste('readxl',packageVersion('readxl'))),'outputs/software_versions.txt')
cat('Standalone theme61 chart exported as PNG, PDF and SVG.\n')
print(endpoints,row.names=FALSE)

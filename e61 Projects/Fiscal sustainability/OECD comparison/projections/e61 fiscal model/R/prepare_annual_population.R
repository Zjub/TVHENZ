# Offline extraction shared by full input preparation and the population-only update.
prepare_annual_population<-function(raw='data/raw/v2',out='data/processed') {
  d<-as.data.frame(readxl::read_excel(file.path(raw,'igr_2026_population_age_sex.xlsx'),
    'PROJECTIONS',skip=4))
  z<-d[d$Sex%in%c('Males','Females'),]
  yrs<-as.integer(substr(names(z)[-(1:2)],1,4))+1L
  pop<-do.call(rbind,lapply(seq_along(yrs),function(i)data.frame(year=yrs[i],
    age=as.integer(z$Age),sex=ifelse(z$Sex=='Males','Male','Female'),
    population=as.numeric(z[[i+2]]))))
  # Only the pre-projection 2025 NDIS/base year comes from Population Statement 2025.
  b<-as.data.frame(readxl::read_excel(file.path(raw,'population_statement_2025.xlsx'),'PROJECTIONS'))
  b<-b[b$Geography=='Australia' & b$Sex%in%c('Male','Female','Males','Females') &
    substr(b[['Financial year']],1,4)=='2024',]
  bridge<-data.frame(year=2025L,age=pmin(100L,as.integer(sub('[^0-9].*$','',b$Age))),
    sex=ifelse(grepl('Female',b$Sex),'Female','Male'),population=as.numeric(b$Value))
  pop<-rbind(bridge,pop);pop<-pop[order(pop$year,pop$sex,pop$age),]
  stopifnot(identical(sort(unique(pop$year)),2025:2066),
    all(table(pop$year)==202),all(is.finite(pop$population)),all(pop$population>0),
    !anyDuplicated(pop[c('year','age','sex')]))
  totals<-aggregate(population~year,pop,sum)
  data.table::fwrite(pop,file.path(out,'population_age_sex.csv'))
  data.table::fwrite(totals,file.path(out,'population_totals.csv'))
  # Published persons and male/female figures are independently rounded to 100 people.
  persons<-d[d$Sex=='Persons',]
  audit<-data.frame(year=yrs,published_persons=colSums(sapply(persons[-(1:2)],as.numeric)),
    model_sex_sum=totals$population[match(yrs,totals$year)])
  audit$rounding_difference<-audit$model_sex_sum-audit$published_persons
  data.table::fwrite(audit,file.path(out,'population_source_reconciliation.csv'))
  invisible(pop)
}

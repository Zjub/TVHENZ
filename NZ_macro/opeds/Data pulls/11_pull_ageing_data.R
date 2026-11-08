# Ageing and NZ fiscal spending: observations, projections and fixed-profile
# counterfactuals. No causal attribution is assumed from category totals.
required <- c("dplyr","tidyr","purrr","readr","readxl","httr2")
missing <- required[!vapply(required,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) stop("Missing packages: ",paste(missing,collapse=", "))
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(purrr);library(readr)})
arg <- grep("^--file=",commandArgs(FALSE),value=TRUE)
root <- if(length(arg)) dirname(normalizePath(sub("^--file=","",arg[1]))) else normalizePath("Data pulls")
raw_dir <- file.path(root,"raw","ageing"); out_dir <- file.path(root,"processed","ageing")
dir.create(raw_dir,recursive=TRUE,showWarnings=FALSE);dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)
refresh <- !"--cached" %in% commandArgs(TRUE) && tolower(Sys.getenv("NZ_DATA_REFRESH","true"))!="false"
manifest <- list();checks <- list()
num <- function(x) suppressWarnings(as.numeric(gsub(",","",trimws(as.character(x)))))
write_data <- function(x,n) write_csv(x,file.path(out_dir,paste0(n,".csv")),na="")
check <- function(n,ok,detail="") {
  if(!isTRUE(ok))stop("Validation failed: ",n," ",detail)
  checks[[length(checks)+1L]] <<- tibble(check=n,passed=TRUE,detail=detail)
}
fetch <- function(id,url,filename,description,old_filename=NULL) {
  path <- file.path(raw_dir,filename)
  if(!refresh && !file.exists(path) && !is.null(old_filename)) {
    old_path <- file.path(root,"raw",old_filename)
    if(file.exists(old_path))file.copy(old_path,path)
  }
  cached <- !refresh && file.exists(path) && file.info(path)$size>20
  if(!cached) {
    message("Pulling ",id);tmp <- tempfile(tmpdir=raw_dir);on.exit(unlink(tmp),add=TRUE)
    response <- httr2::request(url) |> httr2::req_user_agent("NZ ageing and fiscal evidence research") |>
      httr2::req_timeout(90) |> httr2::req_retry(max_tries=3) |> httr2::req_perform(path=tmp)
    ct <- httr2::resp_header(response,"content-type")
    if(!is.null(ct) && grepl("text/html",ct) && !grepl("html$",filename))stop("HTML instead of data: ",id)
    if(file.info(tmp)$size<20 || !file.copy(tmp,path,overwrite=TRUE))stop("Download failed: ",id)
  }
  manifest[[length(manifest)+1L]] <<- tibble(source_id=id,description=description,url=url,
    local_file=filename,md5=unname(tools::md5sum(path)),cached_this_run=cached,snapshot_cutoff="2026-11-02",
    file_saved_utc=format(file.info(path)$mtime,tz="UTC",usetz=TRUE))
  path
}
files <- list(
  ltfs=fetch("ltfs","https://www.treasury.govt.nz/sites/default/files/2025-09/ltfs-2025-charts-data.xlsx","ltfs_2025.xlsx","Treasury LTFS 2025 official figure data; LTFM and OLG identified separately","treasury_ltfs_2025_charts_data.xlsx"),
  model=fetch("ltfm","https://www.treasury.govt.nz/sites/default/files/2025-09/ltfm-htm-sep25.xlsx","ltfm_2025.xlsx","Treasury 2025 Long-Term Fiscal Model, based on BEFU 2025","treasury_ltfm_2025.xlsx"),
  population=fetch("stats_population","https://www.stats.govt.nz/assets/Uploads/National-population-projections/National-population-projections-2024base2078/Download-data/national-population-projections-2024base-2078.xlsx","population_2024base.xlsx","Stats NZ national population projections 2024-base to 2078; published 4 June 2025"),
  history=fetch("fiscal_history","https://www.treasury.govt.nz/sites/default/files/2026-01/fiscaltimeseries1972-2025-year-end25.xlsx","fiscal_history.xlsx","Treasury Fiscal Time Series 1972-2025; first dollar block only; 2006-2025 used","treasury_fiscal_history_1972_2025.xlsx"),
  notes=fetch("model_notes","https://www.treasury.govt.nz/sites/default/files/2026-03/an26-01-v2.pdf","model_notes.pdf","Treasury AN 26/01 revised March 2026; model features and assumption definitions")
)
ex <- function(path,sheet) readxl::read_excel(path,sheet=sheet,col_names=FALSE,col_types="text",
  range=readxl::cell_limits(c(1,1),c(NA,NA)),.name_repair="minimal")
old <- function(n) read_csv(file.path(root,"processed","op_ed_topics",paste0(n,".csv")),show_col_types=FALSE)
election <- function(n) read_csv(file.path(root,"processed","election_2026",paste0(n,".csv")),show_col_types=FALSE)

# Published population quantiles are marginal, not joint simulation draws. Never
# add non-median quantiles or divide marginal population quantiles to get a ratio.
x <- ex(files$population,"Table 1")
blocks <- c(total_population=8,children_0_14=54,adults_15_39=70,adults_40_64=86,older_65_plus=102,
  oldest_85_plus=118,children_percent=134,adults_15_39_percent=150,adults_40_64_percent=166,
  older_percent=182,oldest_percent=198,median_age=214,child_dependency=230,old_dependency=246,total_dependency=262)
stats_population <- imap_dfr(blocks,function(row,measure) {
  rows <- (row+1):(row+15)
  map_dfr(3:13,function(j) tibble(year=as.integer(substr(x[[1]][rows],1,4)),measure=measure,
    variant=if(j<=7)paste0(c(5,25,50,75,95)[j-2],"th percentile") else gsub("\\s+"," ",trimws(x[[j]][7])),
    value=num(x[[j]][rows]),source_row=rows,source_column=j))
}) |> filter(!is.na(year),!is.na(value))
check("Stats population endpoint",max(stats_population$year)==2078)
check("Stats marginal quantiles ordered",stats_population |> filter(grepl("percentile",variant)) |>
  mutate(q=as.integer(sub("th.*","",variant))) |> arrange(measure,year,q) |> group_by(measure,year) |>
  summarise(ok=all(diff(value)>=0),.groups="drop") |> pull(ok) |> all())
write_data(stats_population,"stats_population_projections")

# Direct, age-specific population values stored with the Treasury model. These
# are calibrated to HLFS/forecast population totals; they are not raw ERP counts.
x <- ex(files$model,"Population Treasury");years <- num(unlist(x[3,2:ncol(x)]));valid <- which(!is.na(years))+1
population <- map_dfr(c(15:110,115:210),function(i) tibble(year=years[valid-1],
  sex=if(i<115)"Female" else "Male",age=if(grepl("95",x[[1]][i]))95L else as.integer(x[[1]][i]),
  people=num(unlist(x[i,valid])),source_row=i)) |>
  mutate(age_group=paste0(pmin(90,5*(age%/%5)),if_else(age>=90,"+",paste0("-",5*(age%/%5)+4))),
    status=case_when(year<=2024~"Historical calibrated estimate",year<=2029~"BEFU 2025 forecast",TRUE~"LTFM projection"))
check("Unique Treasury population cells",!anyDuplicated(population[c("year","sex","age")]))
pop_totals <- population |> group_by(year,status) |> summarise(population=sum(people),children=sum(people[age<15]),
  adults_15_64=sum(people[age>=15 & age<65]),older_65_plus=sum(people[age>=65]),oldest_85_plus=sum(people[age>=85]),
  oldest_90_plus=sum(people[age>=90]),.groups="drop") |>
  mutate(older_share=older_65_plus/population,oldest_share=oldest_85_plus/population,
    support_ratio=adults_15_64/older_65_plus,child_dependency=100*children/adults_15_64,
    old_dependency=100*older_65_plus/adults_15_64,total_dependency=100*(children+older_65_plus)/adults_15_64)
published_pop <- tibble(year=years[valid-1],published_population=num(unlist(x[4,valid]))*1e6)
check("Age cells sum to Treasury population",all(abs((left_join(pop_totals,published_pop,by="year") |> mutate(err=population-published_population))$err)<.01))
write_data(population,"treasury_population_age_sex");write_data(pop_totals,"treasury_population_totals")

# Extract selected published LTFS chart tables without reading chart objects.
fig <- function(n,cols,header=5,start=7,period="year") {
  z <- ex(files$ltfs,paste0("Fig ",n," data"))
  map_dfr(cols,function(j) tibble(figure=n,series=trimws(z[[j]][header]),period=z[[2]][start:nrow(z)],
    value=num(z[[j]][start:nrow(z)]),source_sheet=paste0("Fig ",n," data"),source_row=start:nrow(z),source_column=j)) |>
    mutate(year=if(.env$period=="year")num(.data$period) else NA_real_) |>
    filter(!is.na(value),!is.na(.data$period))
}
selected <- bind_rows(fig(15,3:4),fig(16,3:4,6,8),fig(18,3:4),fig(19,3:5),fig(21,3:5),
  fig(22,3:4),fig(23,3:4),fig(26,3:5),fig(27,3:5),fig(28,3:5),fig(36,3:4),fig(39,3:5),fig(12,3:9,6,8))
check("Selected figure years",all(selected$year>=1950 & selected$year<=2150))
write_data(selected,"ltfs_selected_figures")
lfpr <- fig(4,3:5,period="date") |> mutate(date=as.Date(num(period),origin="1899-12-30"))
check("Older LFPR date range",min(lfpr$date)>=as.Date("1986-01-01") && max(lfpr$date)<as.Date("2026-01-01"))
write_data(lfpr,"older_participation")
health_profile <- fig(17,3:7) |> transmute(min_age=as.integer(sub("[^0-9].*$","",period)),
  age_group=paste0(min_age,if_else(min_age==90,"+",paste0("-",min_age+4))),
  service=series,cost_per_person=value*1000,source_row,source_column)
check("Health profile covers all five-year ages to 90+",identical(sort(unique(health_profile$min_age)),seq(0L,90L,5L)))
write_data(health_profile,"health_age_profile")
tax_profile <- fig(14,3:6) |> transmute(min_age=as.integer(sub("[^0-9].*$","",period)),
  age_group=paste0(min_age,if_else(min_age==80,"+",paste0("-",min_age+4))),
  measure=series,dollars_2019=value,source_row,source_column)
write_data(tax_profile,"tax_transfer_age_profile_2019")

# LTFM rows are selected by explicit source row and retained for audit. Dollar
# measures are billions unless explicitly named otherwise. Forecast base is 2025.
x <- ex(files$model,"Long-Term Fiscal Model");cols <- 4:ncol(x);years <- num(unlist(x[6,cols]))
model_rows <- c(7,11,13,16,18,20,21,23,24,25,26,28,29,47,48,49,50,51,78,140,180,181,182,183,
  195,200,201,202,203,228,244,248:253,259:266,276,280,284,294,298,302,306,310,316,320,381)
model <- map_dfr(model_rows,function(i)tibble(year=years,measure=trimws(x[[1]][i]),value=num(unlist(x[i,cols])),source_row=i)) |>
  filter(!is.na(year),!is.na(value)) |> mutate(status=case_when(year<=2024~"Outturn",year<=2029~"BEFU 2025 forecast",TRUE~"LTFM projection"))
write_data(model,"ltfm_selected_rows")
series <- function(i)model |> filter(source_row==i) |> select(year,value)
denom <- reduce(list(series(13)|>rename(gdp_billion=value),series(28)|>rename(cpi=value),
  series(21)|>rename(model_population_million=value)),left_join,by="year")
baseline <- reduce(list(series(48)|>rename(total_expenses=value),series(180)|>rename(nzs=value),
  series(253)|>rename(health=value),series(266)|>rename(education=value),series(228)|>rename(finance=value),
  series(195)|>rename(welfare_total=value),series(47)|>rename(revenue=value)),left_join,by="year") |>
  left_join(denom,by="year") |> left_join(pop_totals,by="year") |>
  mutate(other_welfare=welfare_total-nzs,other_primary=total_expenses-health-education-finance-welfare_total,
    real_gdp_billion=gdp_billion*cpi[year==2025]/cpi,
    pension_real_billion=nzs*cpi[year==2025]/cpi,health_real_billion=health*cpi[year==2025]/cpi,
    pension_percent_gdp=100*nzs/gdp_billion,health_percent_gdp=100*health/gdp_billion,
    health_real_per_resident=health_real_billion*1e9/population,
    pension_real_per_older=pension_real_billion*1e9/older_65_plus,
    real_gdp_per_15_64=real_gdp_billion*1e9/adults_15_64)
check("Model population agrees across worksheets",all(abs(baseline$population/1e6-baseline$model_population_million)<1e-8))
check("LTFM expense components reconcile",all(abs(with(baseline,nzs+health+education+finance+other_welfare+other_primary-total_expenses))<1e-8))
check("LTFM 2065 published anchors",abs(baseline$health_percent_gdp[baseline$year==2065]-9.9)<.06 &&
  abs(baseline$pension_percent_gdp[baseline$year==2065]-7.8)<.06)
write_data(baseline,"ltfm_baseline")

# Fixed age-cost health benchmark: no change in prices, service quality, health,
# proximity to death, morbidity or treatment mix within an age bracket.
age_pop <- population |> group_by(year,age_group) |> summarise(people=sum(people),.groups="drop") |>
  left_join(pop_totals |> select(year,population,status),by="year")
health_need <- age_pop |> inner_join(health_profile |> select(age_group,service,cost_per_person),by="age_group",relationship="many-to-many") |>
  mutate(weighted_cost=people*cost_per_person,age_population_share=people/population)
check("Every population age bracket matched to health profile",n_distinct(health_need$age_group)==19 && nrow(health_need)==nrow(age_pop)*5)
health_weights <- health_need |> group_by(year,status,service) |>
  summarise(fixed_profile_spending=sum(weighted_cost),population=first(population),
    mean_fixed_profile_cost=sum(weighted_cost)/first(population),.groups="drop")
health_total <- health_weights |> group_by(year,status) |> summarise(population=first(population),
  mean_fixed_profile_cost=sum(mean_fixed_profile_cost),fixed_profile_spending=sum(fixed_profile_spending),.groups="drop")
write_data(health_need,"health_fixed_profile_age_cells")
write_data(health_weights,"health_fixed_profile_by_service");write_data(health_total,"health_fixed_profile_totals")
baseyear <- 2029
health_benchmarks <- baseline |> select(year,health_real_billion,health_real_per_resident) |>
  inner_join(health_total,by="year") |> filter(year>=baseyear) |>
  mutate(population_only=100*population/population[year==baseyear],
    population_and_age_mix=100*fixed_profile_spending/fixed_profile_spending[year==baseyear],
    full_ltfm_real_health=100*health_real_billion/health_real_billion[year==baseyear],
    age_mix_only=100*mean_fixed_profile_cost/mean_fixed_profile_cost[year==baseyear],
    full_ltfm_real_per_person=100*health_real_per_resident/health_real_per_resident[year==baseyear])
write_data(health_benchmarks,"health_projection_benchmarks")

# Fiscal history: read only rows 5:58 of the dollar block. The workbook also
# repeats years below under % GDP, which must NEVER be treated as extra dollars.
x <- ex(files$history,"Spending");g <- ex(files$history,"Nominal GDP")
gdp <- tibble(year=as.integer(substr(g[[2]][5:58],1,4)),gdp_millions=num(g[[3]][5:58]))
history <- map_dfr(c(4,7:16),function(j)tibble(year=as.integer(substr(x[[2]][5:58],1,4)),measure=trimws(x[[j]][4]),
  value_millions=num(x[[j]][5:58]),source_row=5:58,source_column=j)) |>
  filter(year>=2006,!is.na(value_millions)) |> left_join(gdp,by="year") |>
  mutate(percent_gdp=100*value_millions/gdp_millions)
check("Unique fiscal history years and measures",!anyDuplicated(history[c("year","measure")]))
check("Fiscal history covers every year 2006-2025",identical(sort(unique(history$year)),2006:2025))
check("Historical expense total anchor",history |> filter(year==2025,measure=="Core Crown Expenses") |> pull(value_millions) |> identical(141675))
write_data(history,"fiscal_history_2006_2025")

# Reused public observations retain their own vintages, dates and hashes.
reuse <- function(id,subdir,name,description,url) {
  path <- file.path(root,"processed",subdir,paste0(name,".csv"))
  if(!file.exists(path))stop("Run the earlier evidence packs first: ",path)
  manifest[[length(manifest)+1L]] <<- tibble(source_id=id,description=description,url=url,
    local_file=paste0("../../processed/",subdir,"/",name,".csv"),md5=unname(tools::md5sum(path)),cached_this_run=TRUE,
    snapshot_cutoff=if(subdir=="election_2026")"2026-11-02" else "2026-10-14",file_saved_utc=format(file.info(path)$mtime,tz="UTC",usetz=TRUE))
  read_csv(path,show_col_types=FALSE)
}
nzs_history <- reuse("nzs_history","op_ed_topics","nzs_expense_caseload_decomposition",
  "Earlier evidence pack: Treasury actual NZS expenses, MSD fiscal-year mean quarterly counts, annual CPI",
  "../../BENEFIT_HISTORY.md")
benefit_history <- reuse("benefit_history","op_ed_topics","benefit_history_groups",
  "Earlier evidence pack: Treasury actual benefit expense history; vintages documented",
  "../../BENEFIT_HISTORY.md")
prices <- reuse("prices","op_ed_topics","benefit_history_prices_population","Earlier evidence pack: annual-average CPI and population, official source vintages documented",
  "../../BENEFIT_HISTORY.md")
historical_context <- history |> filter(measure %in% c("Core Crown Expenses","Health","Education","Finance costs")) |>
  left_join(prices |> select(year,cpi,population),by="year") |>
  mutate(real_2025_millions=value_millions*prices$cpi[prices$year==2025]/cpi,
    real_per_resident=real_2025_millions*1e6/population)
write_data(historical_context,"historical_spending_context")
prefu_benefits <- reuse("prefu_exp","election_2026","prefu_benefits","PREFU September 2026 benefit expenses, actual/unaudited/forecast",
  "https://www.treasury.govt.nz/sites/default/files/2026-09/prefu26-data-expense-tables.xlsx")
prefu_counts <- reuse("prefu_counts","election_2026","prefu_recipients","PREFU September 2026 published recipient counts (thousands)",
  "https://www.treasury.govt.nz/sites/default/files/2026-09/prefu26-data-expense-tables.xlsx")

# LMDI is an exact accounting allocation of a positive product's level change.
# It is NOT causal attribution. Interactions are allocated using a log mean.
lmdi <- function(data,total_col,factor_cols,base_year,denominator_col=NULL) {
  base <- data |> filter(year==base_year);stopifnot(nrow(base)==1)
  map_dfr(seq_len(nrow(data)),function(i) {
    z <- data[i,];a <- z[[total_col]];b <- base[[total_col]]
    weight <- if(abs(a-b)<1e-12)b else (a-b)/log(a/b)
    map_dfr(names(factor_cols),function(label) {
      col <- factor_cols[[label]];change <- weight*log(z[[col]]/base[[col]])
      tibble(year=z$year,component=label,contribution=change,total_change=a-b,base_year=base_year)
    })
  })
}
historic_nzs <- nzs_history |> inner_join(pop_totals |> select(year,treasury_population=population,older_65_plus),by="year") |>
  mutate(population_factor=treasury_population,age_share_factor=older_65_plus/treasury_population,
    coverage_factor=average_recipients/older_65_plus,payment_factor=real_2025_millions*1e6/average_recipients)
hist_decomp <- lmdi(historic_nzs,"real_2025_millions",c("Total population"="population_factor","65+ population share"="age_share_factor",
  "Recipient / population-definition residual"="coverage_factor","Real expense per average recipient"="payment_factor"),2009)
check("Historical pension decomposition reconciles",all(abs((hist_decomp |> group_by(year) |> summarise(e=sum(contribution)-first(total_change)))$e)<1e-7))
write_data(historic_nzs,"historic_nzs_factors");write_data(hist_decomp,"historic_nzs_lmdi")
projected_nzs <- baseline |> filter(year>=2029) |> mutate(age_pressure=older_65_plus/adults_15_64,
  payment_factor=pension_real_per_older,gdp_inverse=1/real_gdp_per_15_64,
  fixed_age_ratio_percent_gdp=pension_percent_gdp*age_pressure[year==2029]/age_pressure)
proj_decomp <- lmdi(projected_nzs,"pension_percent_gdp",c("65+ relative to ages 15-64"="age_pressure",
  "Real expense per person aged 65+"="payment_factor","Real GDP per person aged 15-64"="gdp_inverse"),2029)
check("Projected pension decomposition reconciles",all(abs((proj_decomp |> group_by(year) |> summarise(e=sum(contribution)-first(total_change)))$e)<1e-8))
write_data(projected_nzs,"projected_nzs_factors");write_data(proj_decomp,"projected_nzs_lmdi")
near <- prefu_benefits |> filter(measure=="New Zealand Superannuation") |> select(year,status,value) |>
  inner_join(prefu_counts |> filter(measure=="New Zealand Superannuation") |> select(year,recipients=value),by="year") |>
  mutate(expense_per_published_recipient=value/recipients)
near_decomp <- lmdi(near,"value",c("Published recipient numbers"="recipients","Expense per published recipient"="expense_per_published_recipient"),2026)
check("PREFU pension decomposition reconciles",all(abs((near_decomp |> group_by(year) |> summarise(e=sum(contribution)-first(total_change)))$e)<1e-7))
write_data(near,"prefu_nzs_factors");write_data(near_decomp,"prefu_nzs_lmdi")

# Selected 2019 tax/transfer profiles reweighted by age structure. No claim that
# they cover the entire budget, predict lifetime contributions or current taxes.
tax_age_pop <- population |> mutate(min_age=pmin(80L,5L*(age%/%5L))) |> group_by(year,min_age) |>
  summarise(people=sum(people),.groups="drop") |> left_join(pop_totals |> select(year,population,status),by="year")
tax_benchmark <- tax_age_pop |> inner_join(tax_profile |> select(min_age,measure,dollars_2019),by="min_age",relationship="many-to-many") |>
  group_by(year,status,measure) |> summarise(mean_dollars_2019=sum(people*dollars_2019/population),.groups="drop")
check("Tax profiles matched all ages",nrow(tax_benchmark)==n_distinct(population$year)*4)
write_data(tax_benchmark,"fixed_tax_transfer_profile_benchmark")

# Audit workbook assumptions without recalculating or changing Excel formulas.
x <- ex(files$model,"Assumptions")
assumptions <- tibble(source_row=1:nrow(x),assumption=x[[1]],value=x[[3]]) |> filter(!is.na(assumption),!is.na(value))
write_data(assumptions,"ltfm_assumptions")
findings <- bind_rows(
  pop_totals |> filter(year %in% c(2006,2024,2025,2065)) |> transmute(measure="Age 65+ population share",year,value=100*older_share,unit="percent",basis=status),
  health_total |> filter(year %in% c(2006,2025,2065)) |> transmute(measure="Fixed-profile health cost per resident",year,value=mean_fixed_profile_cost,
    unit="Source-profile NZD (not current prices)",basis="Fixed age-cost-profile benchmark; not a spending forecast"),
  baseline |> filter(year %in% c(2025,2065)) |> select(year,pension_percent_gdp,health_percent_gdp) |>
    pivot_longer(-year,names_to="measure",values_to="value") |> mutate(unit="percent GDP",basis=if_else(year==2025,"BEFU 2025 forecast in LTFS","LTFM conditional projection")),
  hist_decomp |> filter(year==2025) |> transmute(measure=paste("Historical real NZS increase:",component),year,value=contribution,unit="2025 NZD millions",basis="LMDI accounting allocation, 2009-2025"),
  proj_decomp |> filter(year==2065) |> transmute(measure=paste("Projected NZS/GDP change:",component),year,value=contribution,unit="percentage points GDP",basis="LMDI accounting allocation, 2029-2065")
)
health_hist <- historical_context |> filter(measure=="Health")
findings <- bind_rows(findings,tibble(
  measure=c("CPI-deflated actual health expense per resident: growth since 2006",
    "Fixed-profile age-mix health benchmark: growth since 2006",
    "Fixed-profile age-mix health benchmark: growth since 2025"),
  year=c(2025,2025,2065),
  value=c(100*(health_hist$real_per_resident[health_hist$year==2025]/health_hist$real_per_resident[health_hist$year==2006]-1),
    100*(health_total$mean_fixed_profile_cost[health_total$year==2025]/health_total$mean_fixed_profile_cost[health_total$year==2006]-1),
    100*(health_total$mean_fixed_profile_cost[health_total$year==2065]/health_total$mean_fixed_profile_cost[health_total$year==2025]-1)),
  unit="percent change",basis=c("Actual spending; resident-population and annual-CPI denominators",
    "Fixed-profile counterfactual; 2025 Treasury demographic input is forecast",
    "Fixed-profile counterfactual; no change in costs or health within ages")))
check("Numeric research findings finite",all(is.finite(findings$value)))
write_data(findings,"key_findings");write_data(bind_rows(checks),"validation");write_data(bind_rows(manifest),"source_manifest")
message("Ageing evidence saved: ",out_dir)

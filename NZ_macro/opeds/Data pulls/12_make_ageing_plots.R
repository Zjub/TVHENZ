# Historical evidence and conditional projections of ageing-related pressures.
required <- c("dplyr","tidyr","purrr","readr","ggplot2","scales","svglite")
missing <- required[!vapply(required,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing))stop("Missing packages: ",paste(missing,collapse=", "))
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(purrr);library(readr);library(ggplot2)})
arg <- grep("^--file=",commandArgs(FALSE),value=TRUE)
root <- if(length(arg))dirname(normalizePath(sub("^--file=","",arg[1]))) else normalizePath("Data pulls")
data_dir <- file.path(root,"processed","ageing");figure_dir <- file.path(root,"figures","ageing")
dir.create(figure_dir,recursive=TRUE,showWarnings=FALSE)
load_data <- function(n)read_csv(file.path(data_dir,paste0(n,".csv")),show_col_types=FALSE)
sources <- load_data("source_manifest");catalogue <- list()
palette <- c("#087F8C","#C56000","#395A9A","#A53B59","#4D7342","#8063A8","#63584B")
wrap <- function(x,w=36)vapply(x,function(z)paste(strwrap(z,w),collapse="\n"),character(1))
cash <- scales::label_number(prefix="$",big.mark=",",accuracy=1)
save <- function(id,d,p,title,subtitle,note,ids,kind="Observed / derived",section="Historical spending",height=8) {
  src <- sources |> filter(source_id %in% ids);stopifnot(nrow(d)>0,all(ids %in% src$source_id))
  message("Saving ",id)
  caption <- paste(paste(wrap(paste0(kind,": ",note),140),collapse="\n"),
    paste0("Sources: ",paste(wrap(paste(src$description,collapse="; "),140),collapse="\n")),sep="\n")
  p <- p+scale_colour_manual(values=palette,labels=function(x)wrap(x,36))+scale_fill_manual(values=palette,labels=function(x)wrap(x,36))+
    labs(title=paste(wrap(title,85),collapse="\n"),subtitle=paste(wrap(subtitle,108),collapse="\n"),caption=caption)+theme_minimal(base_size=12)+
    theme(plot.title=element_text(size=19,face="bold",hjust=0),plot.subtitle=element_text(size=12),
      plot.caption=element_text(size=9,hjust=0,lineheight=1.15),plot.title.position="plot",plot.caption.position="plot",
      legend.position="bottom",legend.text=element_text(size=10),legend.title=element_text(size=10),
      panel.grid.minor=element_blank(),plot.margin=margin(16,20,16,16))+
    guides(colour=guide_legend(nrow=2,byrow=TRUE),fill=guide_legend(nrow=2,byrow=TRUE))
  ggsave(file.path(figure_dir,paste0(id,".png")),p,width=12,height=height,dpi=180,bg="white")
  ggsave(file.path(figure_dir,paste0(id,".svg")),p,width=12,height=height,bg="white")
  write_csv(d,file.path(figure_dir,paste0(id,"_data.csv")),na="")
  catalogue[[length(catalogue)+1L]] <<- tibble(id,section,title,subtitle,measurement_type=kind,interpretation=note,
    sources=paste(src$url,collapse=" | "),png=paste0(id,".png"),svg=paste0(id,".svg"),data_csv=paste0(id,"_data.csv"))
}
line <- function(d,x,y,g,xlab,ylab)ggplot(d,aes(x=.data[[x]],y=.data[[y]],colour=.data[[g]]))+geom_line(linewidth=.9)+labs(x=xlab,y=ylab,colour=NULL)
population <- load_data("treasury_population_totals");age_pop <- load_data("treasury_population_age_sex")
stats <- load_data("stats_population_projections");figures <- load_data("ltfs_selected_figures")
model <- load_data("ltfm_baseline");health <- load_data("health_fixed_profile_totals")
health_cells <- load_data("health_fixed_profile_age_cells");profile <- load_data("health_age_profile")
hist <- load_data("fiscal_history_2006_2025");context <- load_data("historical_spending_context")
projection_note <- "2025 LTFM vintage: outturn through 2024, BEFU 2025 forecast to 2029, conditional projection from 2030. Not an updated PREFU projection."
population_note <- "Treasury population is calibrated to HLFS/forecast totals, not the raw Stats NZ resident-population series. Ages 15-64 are not a count of workers or taxpayers."
demographic_marker <- geom_vline(xintercept=c(2024.5,2029.5),linetype="dashed",colour="grey60")

# Population size, structure and uncertainty.
d <- stats |> filter(variant=="50th percentile",measure %in% c("children_percent","adults_15_39_percent","adults_40_64_percent","older_percent")) |>
  mutate(measure=recode(measure,children_percent="0-14",adults_15_39_percent="15-39",adults_40_64_percent="40-64",older_percent="65+"))
save("A01_population_age_shares",d,line(d,"year","value","measure","June year","Population share (%)"),
  "An older population changes the mix of public services", "Stats NZ 2024-base median projections through 2078",
  "Median projections at published years; 2024 is the base estimate, later years are projections. Broad age groups partition the population, subject to source rounding.","stats_population","Official demographic projection","Population and age structure")
d <- stats |> filter(measure %in% c("older_percent","oldest_percent"),variant %in% c("5th percentile","50th percentile","95th percentile")) |>
  select(year,measure,variant,value) |> pivot_wider(names_from=variant,values_from=value) |>
  mutate(measure=if_else(measure=="older_percent","Aged 65+","Aged 85+"))
save("A02_ageing_uncertainty",d,ggplot(d,aes(year,`50th percentile`,colour=measure,fill=measure))+
  geom_ribbon(aes(ymin=`5th percentile`,ymax=`95th percentile`),alpha=.15,colour=NA)+geom_line(linewidth=1)+facet_wrap(~measure,scales="free_y")+
  labs(x="June year",y="Population share (%)",colour=NULL,fill=NULL)+guides(colour="none",fill="none"),
  "Ageing is projected with uncertainty", "Median and 5th-95th percentile bands for each published population share",
  "Marginal projection percentiles, not confidence intervals on actual outcomes. Bands are not additive or a joint worst/best-case scenario.","stats_population","Official demographic projection","Population and age structure")
d <- stats |> filter(variant=="50th percentile",measure %in% c("child_dependency","old_dependency","total_dependency")) |>
  mutate(measure=recode(measure,child_dependency="Ages 0-14",old_dependency="Ages 65+",total_dependency="Both age groups"))
save("A03_dependency_ratios",d,line(d,"year","value","measure","June year","People per 100 people aged 15-64"),
  "Fewer children partly offset more older people", "Published Stats NZ median age-dependency ratios",
  "Age ratios do not measure economic dependency: people above 65 work and pay taxes, while many people aged 15-64 are not employed.","stats_population","Official demographic projection","Population and age structure")
d <- population |> select(year,status,older_share,oldest_share) |> pivot_longer(c(older_share,oldest_share),names_to="age",values_to="share") |>
  mutate(share=100*share,age=if_else(age=="older_share","Aged 65+","Aged 85+"))
save("A04_long_age_structure",d,line(d,"year","share","age","June year","Population share (%)")+demographic_marker,
  "Age structure: history, forecast and long-term projection", "Treasury's calibrated population, 2006-2065; dashed dividers after 2024 and 2029",
  paste(projection_note,population_note),"ltfm","Historical / forecast / projection","Population and age structure")
d <- population |> mutate(percent_older=100*oldest_85_plus/older_65_plus,series="85+ among people aged 65+")
save("A05_ageing_within_retirement",d,line(d,"year","percent_older","series","June year","Share of population aged 65+ (%)")+demographic_marker+guides(colour="none"),
  "The older population is itself ageing", "Share of the 65+ population aged 85 or above, Treasury calibrated population",
  paste(projection_note,"An increase within retirement ages matters for care needs even with an unchanged pension eligibility age."),"ltfm","Historical / forecast / projection","Population and age structure")
d <- load_data("older_participation")
save("A06_older_workforce_participation",d,line(d,"date","value","series",NULL,"Labour force participation (%)"),
  "People over 65 are not all outside the labour force", "Quarterly participation by older age group, as published in LTFS 2025",
  "Historical series ends at the LTFS release's final observation, not 2026. Participation includes employment and active unemployment; it is not hours, tax paid or capacity to keep working.","ltfs","Published historical series","Population and age structure")

# Health: fixed age profiles give a conditional measure of ageing pressure.
d <- profile |> mutate(age_group=factor(age_group,levels=unique(age_group[order(min_age)])))
save("A07_health_spending_by_age",d,ggplot(d,aes(age_group,cost_per_person,fill=service))+geom_col()+
  scale_x_discrete(labels=function(x)wrap(x,7))+scale_y_continuous(labels=cash)+labs(x="Age",y="Annual cost per person in source profile",fill=NULL),
  "Age affects both the level and mix of health expenditure", "Treasury LTFS Figure 17 age profile; categories stacked",
  "Published profile is used as a fixed weighting schedule, not current 2026 cost data. Underlying model weights were updated in 2019. It does not control morbidity or proximity to death.","ltfs","Published age-cost profile","Health and service demand")
d <- health |> mutate(index_2006=100*mean_fixed_profile_cost/mean_fixed_profile_cost[year==2006],series="Fixed age-cost benchmark")
save("A08_age_mix_health_pressure",d,line(d,"year","index_2006","series","June year","Fixed-profile cost per resident: 2006 = 100")+demographic_marker+guides(colour="none"),
  "What would ageing alone do at fixed health costs by age?", "Population age mix changes; every age-specific cost and service mix is held fixed",
  paste(population_note,"A demographic-demand benchmark, not actual spending or Treasury's full health model. No changes in health, technology, prices or proximity-to-death costs are modelled."),c("ltfm","ltfs"),"Fixed-profile counterfactual","Health and service demand")
d <- load_data("health_projection_benchmarks") |> select(year,population_only,population_and_age_mix,full_ltfm_real_health) |>
  pivot_longer(-year,names_to="benchmark",values_to="index") |> mutate(benchmark=recode(benchmark,
    population_only="Population growth only",population_and_age_mix="Population + fixed age-cost profile",full_ltfm_real_health="Full LTFM health projection"))
save("A09_health_demography_and_full_projection",d,line(d,"year","index","benchmark","June year","Real health expense / benchmark: 2029 = 100"),
  "Ageing is only one ingredient in projected health spending", "Common 2029 starting point; full model versus two demographic benchmarks",
  "Full model adds wage/productivity growth, residual growth and longevity adjustments. The gap is not a causal estimate of non-ageing costs: demographic methods differ. All three are conditional paths.",c("ltfm","ltfs"),"Projection / fixed-profile counterfactual","Health and service demand")
d <- load_data("health_fixed_profile_by_service") |> group_by(service) |> arrange(year,.by_group=TRUE) |>
  mutate(index_2025=100*mean_fixed_profile_cost/mean_fixed_profile_cost[year==2025]) |> ungroup()
save("A10_age_mix_by_health_service",d,line(d,"year","index_2025","service","June year","Fixed-profile cost per resident: 2025 = 100")+demographic_marker,
  "An older age mix affects services differently", "Each service's fixed age-specific profile reweighted by Treasury population",
  "This isolates changing population composition under an unchanged profile. It does not forecast staffing, bed numbers, service efficiency or healthy ageing.",c("ltfm","ltfs"),"Fixed-profile counterfactual","Health and service demand")
d <- health_cells |> mutate(min_age=as.integer(sub("[^0-9].*$","",age_group)),age=case_when(min_age<65~"Under 65",min_age<85~"65-84",TRUE~"85+")) |>
  group_by(year,age) |> summarise(weighted_cost=sum(weighted_cost),.groups="drop") |> group_by(year) |>
  mutate(share=100*weighted_cost/sum(weighted_cost)) |> ungroup()
save("A11_health_demand_age_mix",d,line(d,"year","share","age","June year","Share of fixed-profile health demand (%)")+demographic_marker,
  "Who accounts for the demographic health-demand benchmark?", "Shares of population-weighted source age costs, not allocated future budgets",
  "All services and costs within an age group are held fixed. Population share differs from expenditure share. No view is assumed about who should receive care.",c("ltfm","ltfs"),"Fixed-profile counterfactual","Health and service demand")

# Historical and near-term spending: avoid labelling every dollar as ageing.
d <- load_data("historic_nzs_lmdi") |> mutate(billions=contribution/1000)
save("A12_historical_pension_growth_decomposition",d,line(d,"year","billions","component","June fiscal year","Contribution to increase since 2009 (2025 NZ$ billions)"),
  "Why did real NZ Super expenditure increase?", "Exact log-mean accounting allocation of 2009-2025 growth",
  "Recipient residual includes timing, coverage and population-definition differences. Age data are HLFS-calibrated; 2025 uses the old BEFU forecast. Per-recipient expense includes rates and mix. Not causal attribution.",c("nzs_history","ltfm"),"Historical accounting decomposition","Historical spending")
benefits <- read_csv(file.path(root,"processed","op_ed_topics","benefit_history_groups.csv"),show_col_types=FALSE)
d <- hist |> filter(measure %in% c("Core Crown Expenses","Health")) |> select(year,measure,value_millions,gdp_millions) |>
  pivot_wider(names_from=measure,values_from=value_millions) |>
  inner_join(benefits |> filter(group=="NZ Superannuation") |> select(year,nzs=value_millions),by="year") |>
  mutate(other=`Core Crown Expenses`-Health-nzs) |> select(year,gdp_millions,Health,nzs,other) |>
  pivot_longer(c(Health,nzs,other),names_to="category",values_to="value_millions") |>
  mutate(percent_gdp=100*value_millions/gdp_millions,category=recode(category,nzs="NZ Superannuation",other="Other core Crown expenses"))
save("A13_historical_spending_categories",d,line(d,"year","percent_gdp","category","June fiscal year","% of GDP"),
  "An ageing population does not explain every spending change", "Actual core Crown expenses, 2006-2025; NZ Super separated from other welfare",
  "Category decomposition, not demographic attribution. Health includes policy, input costs and pandemic effects. History and benefit series have separately documented restatements; residual absorbs those differences.",c("fiscal_history","benefit_history"),"Historical category decomposition","Historical spending")
d <- context |> filter(measure=="Health") |> select(year,real_per_resident) |>
  inner_join(health |> select(year,mean_fixed_profile_cost),by="year") |>
  mutate(actual_index=100*real_per_resident/real_per_resident[year==2006],
    age_mix_index=100*mean_fixed_profile_cost/mean_fixed_profile_cost[year==2006]) |>
  pivot_longer(c(actual_index,age_mix_index),names_to="measure",values_to="index") |>
  mutate(measure=if_else(measure=="actual_index","Actual CPI-deflated health spending","Fixed age-cost demographic benchmark"))
save("A14_historical_health_vs_age_mix",d,line(d,"year","index","measure","June fiscal year","Per resident: 2006 = 100"),
  "How far does a fixed age profile explain historical health growth?", "Actual real health expenditure per resident versus population-composition benchmark",
  "Actual uses resident population; benchmark uses Treasury age shares (2025 is forecast). Gap is not a causal split: it includes prices, services, health, policy and benchmark error. CPI-deflated spending is not output.",c("fiscal_history","prices","ltfm","ltfs"),"Observed / fixed-profile comparison","Historical spending")
d <- load_data("prefu_nzs_lmdi") |> filter(year>=2026) |> mutate(billions=contribution/1000)
save("A15_prefu_pension_growth",d,line(d,"year","billions","component","June fiscal year","Increase since 2025/26 (NZ$ billions)"),
  "The near-term pension forecast combines numbers and average expense", "PREFU 2026: unaudited 2025/26 base, forecasts to 2030/31",
  "Exact nominal accounting allocation using published Treasury counts. Expense per published recipient is not a statutory payment rate; timing and composition matter. Not the older LTFS baseline.",c("prefu_exp","prefu_counts"),"Forecast accounting decomposition","Historical spending")

# Long-run fiscal model: gross spending, economic capacity and alternatives.
d <- model |> select(year,status,gdp_billion,nzs,health,education,finance,other_welfare,other_primary) |>
  pivot_longer(c(nzs,health,education,finance,other_welfare,other_primary),names_to="category",values_to="billions") |>
  mutate(percent_gdp=100*billions/gdp_billion,category=recode(category,nzs="NZ Super",health="Health",education="Education",
    finance="Finance costs",other_welfare="Other welfare",other_primary="Other primary expenses"))
save("A16_long_term_spending_components",d,ggplot(d,aes(year,percent_gdp,fill=category))+geom_area()+
  geom_vline(xintercept=2029.5,linetype="dashed",colour="grey50")+labs(x="June fiscal year",y="Core Crown expenses / GDP (%)",fill=NULL),
  "Long-term spending pressures extend beyond pensions", "Core Crown components reconcile to total expenses in the 2025 LTFM",
  paste(projection_note,"Finance costs reflect accumulated borrowing and interest assumptions; they are not direct expenditure on older people."),"ltfm","Official conditional projection","Long-term spending and scenarios")
d <- model |> filter(year %in% c(2025,2065)) |> select(year,gdp_billion,nzs,health,education,finance,other_welfare,other_primary) |>
  pivot_longer(c(nzs,health,education,finance,other_welfare,other_primary),names_to="category",values_to="billions") |>
  mutate(percent_gdp=100*billions/gdp_billion) |> select(year,category,percent_gdp) |> pivot_wider(names_from=year,values_from=percent_gdp) |>
  mutate(change=`2065`-`2025`,category=recode(category,nzs="NZ Super",health="Health",education="Education",finance="Finance costs",other_welfare="Other welfare",other_primary="Other primary expenses"))
save("A17_projected_spending_increase",d,ggplot(d,aes(reorder(category,change),change,fill=category))+geom_col(show.legend=FALSE)+coord_flip()+
  labs(x=NULL,y="Change in annual spending (percentage points of GDP)"),
  "Which categories account for the projected spending increase?", "Change from the LTFM's 2025 forecast base to its 2065 projection",
  "Accounting by spending category, not a causal allocation to ageing. The debt-service increase also reflects the starting fiscal position, investment and policy assumptions.","ltfm","Projection category decomposition","Long-term spending and scenarios")
d <- load_data("projected_nzs_lmdi")
save("A18_projected_pension_ageing_and_capacity",d,line(d,"year","contribution","component","June fiscal year","Contribution to change since 2029 (percentage points GDP)"),
  "Age pressure and economic capacity pull in different directions", "Exact identity: NZS/GDP = (65+ / 15-64) \u00d7 (real NZS per 65+) / (real GDP per 15-64)",
  "Log-mean allocation; real means CPI-deflated in this identity. GDP per person aged 15-64 is not labour productivity. Per-65+ expense also includes coverage and mix. Baseline GDP held as modelled.","ltfm","Projection accounting decomposition","Long-term spending and scenarios")
d <- load_data("projected_nzs_factors") |> select(year,pension_percent_gdp,fixed_age_ratio_percent_gdp) |>
  pivot_longer(-year,names_to="case",values_to="percent_gdp") |> mutate(case=if_else(case=="pension_percent_gdp","LTFM pension share","Age ratio held at 2029"))
save("A19_pension_fixed_age_ratio",d,line(d,"year","percent_gdp","case","June fiscal year","NZ Super / GDP (%)"),
  "A mechanical pension comparison with a fixed age ratio", "Hold the 65+ / 15-64 ratio fixed; retain modelled spending per older person and GDP per 15-64",
  "This is not a rerun of Treasury's model. A different population would also change GDP, revenue and other spending; no such feedbacks or debt paths are estimated here.","ltfm","Mechanical counterfactual","Long-term spending and scenarios")
d <- load_data("tax_transfer_age_profile_2019") |> mutate(age_group=factor(age_group,levels=unique(age_group[order(min_age)])))
save("A20_tax_transfers_by_age",d,ggplot(d,aes(age_group,dollars_2019,fill=measure))+geom_col()+geom_hline(yintercept=0)+
  scale_y_continuous(labels=cash)+scale_x_discrete(labels=function(x)wrap(x,7))+labs(x="Age",y="Annual average (2018/19 NZ$)",fill=NULL),
  "Tax payments and public benefits vary across the life course", "Treasury age profile for the year ending March 2019; taxes shown below zero",
  "Cross-section, not lifetime accounts. Covers selected direct/indirect taxes, in-kind health/education and income support. Omits other budget items; does not measure each person's net contribution.","ltfs","Published historical age profile","Long-term spending and scenarios",9)
d <- load_data("fixed_tax_transfer_profile_benchmark") |> mutate(type=if_else(grepl("taxes",measure,ignore.case=TRUE),"Selected taxes paid","Selected transfers and services")) |>
  group_by(year,type) |> summarise(mean_dollars=sum(abs(mean_dollars_2019)),.groups="drop") |> group_by(type) |> arrange(year,.by_group=TRUE) |>
  mutate(index=100*mean_dollars/mean_dollars[year==2025]) |> ungroup()
save("A21_fixed_tax_transfer_age_mix",d,line(d,"year","index","type","June year","Fixed-profile amount per resident: 2025 = 100")+demographic_marker,
  "Ageing affects revenue capacity as well as spending demand", "Reweight the same 2019 tax and benefit age profiles using Treasury population",
  "Only age mix changes. No wage growth, policy, tax-rate or behavioural changes; selected profiles do not cover the entire budget. Not an estimate of future deficits or lifetime redistribution.",c("ltfs","ltfm"),"Fixed-profile counterfactual","Long-term spending and scenarios")
d <- figures |> filter(figure==39,year<=2065)
save("A22_health_cost_assumptions",d,line(d,"year","value","series","June fiscal year","Health expenditure / GDP (%)"),
  "Health cost assumptions matter alongside ageing", "Treasury OLG scenarios: baseline and alternative health spending growth",
  "Official conditional OLG scenarios, not forecasts or the LTFM baseline. Reflect different cost-growth assumptions; do not interpret the full change as ageing.","ltfs","Official OLG scenarios","Long-term spending and scenarios")
d <- figures |> filter(figure==26,year<=2065)
save("A23_demographic_alternatives_total",d,line(d,"year","value","series","June fiscal year","Government expenditure / GDP (%)"),
  "Alternative demographics change the timing of fiscal pressure", "Treasury OLG baseline, higher fertility and higher-skilled migration",
  "High-skilled migration changes migrant skill composition at the baseline migrant count, not a higher migrant-volume scenario. Higher fertility initially increases childhood-service demand. Conditional OLG model outcomes.","ltfs","Official OLG scenarios","Long-term spending and scenarios")
d <- figures |> filter(figure==27,year<=2065)
save("A24_demographic_alternatives_health",d,line(d,"year","value","series","June fiscal year","Health expenditure / GDP (%)"),
  "Health needs change under different demographic futures", "Treasury OLG baseline, higher fertility and higher-skilled migration",
  "These are internally modelled scenarios, not independent causal estimates. Migrants also age. OLG coverage and assumptions differ from the LTFM; compare within this chart.","ltfs","Official OLG scenarios","Long-term spending and scenarios")
d <- figures |> filter(figure %in% c(22,23),year<=2065) |> mutate(area=if_else(figure==22,"Health","Pensions"),
  case=if_else(grepl("high productivity",series),"Higher productivity growth","Baseline"))
save("A25_productivity_spending_shares",d,line(d,"year","value","case","June fiscal year","Expenditure / GDP (%)")+facet_wrap(~area),
  "Growth need not remove wage-linked spending pressure", "Treasury OLG baseline and higher total-factor-productivity scenarios",
  "Higher productivity can improve incomes even if spending/GDP stays similar. Wage-linked pensions and labour-intensive healthcare can rise with wages. These scenarios do not say growth is unimportant.","ltfs","Official OLG scenarios","Long-term spending and scenarios")
d <- figures |> filter(figure==36,year>=2025,year<=2065) |> mutate(case=if_else(series=="Net NZS expenditure","Net-of-tax NZS","Including net NZSF contributions / withdrawals"))
save("A26_nzsf_prefunding_role",d,line(d,"year","value","case","June fiscal year","% of GDP"),
  "Existing pre-funding changes the pension financing requirement", "Published LTFS net-of-tax NZS cost with and without net fund contributions/withdrawals",
  "A financing comparison. Fund contributions buy assets; they are not additional pension expense. Gross NZS and net-of-tax NZS are different concepts. Withdrawals do not cancel the underlying pension promise.","ltfs","Official financing projection","Long-term spending and scenarios")
d <- figures |> filter(figure==12,year>=2006,year<=2065,series %in% c("2006 LTFS","2013 LTFS","2021 LTFS","2025 LTFS","Actual"))
save("A27_pension_projection_vintages",d,line(d,"year","value","series","June fiscal year","NZ Super / GDP (%)"),
  "Previous pension projections provide a reality check", "Selected LTFS vintages and the actual series as supplied with LTFS 2025",
  "Each projection has a different baseline and assumptions. Actuals here are the 2025 publication's series, not newly revised 2026 outcomes. This is not a like-for-like forecast-accuracy score.","ltfs","Published projection-vintage comparison","Long-term spending and scenarios")
d <- figures |> filter(figure==21,year>=2025,year<=2065) |> mutate(series=recode(series,Baseline="Baseline borrowing rate",High="Higher borrowing rate",Low="Lower borrowing rate"))
save("A28_interest_and_debt_feedback",d,line(d,"year","value","series","June fiscal year","Net core Crown debt / GDP (%)"),
  "Debt-service feedback amplifies the spending challenge", "Treasury LTFM baseline and alternative interest-rate assumptions",
  "Conditional no-adjustment scenarios, not predictions. Debt reflects many spending/revenue choices and the starting position, not ageing alone. Interest assumptions differ; no specific reform is costed.","ltfs","Official LTFM scenarios","Long-term spending and scenarios")

catalogue <- bind_rows(catalogue);stopifnot(nrow(catalogue)==28,!anyDuplicated(catalogue$id))
write_csv(catalogue,file.path(figure_dir,"chart_catalogue.csv"))
esc <- function(x) {x <- gsub("&","&amp;",x,fixed=TRUE);x <- gsub("<","&lt;",x,fixed=TRUE);x <- gsub(">","&gt;",x,fixed=TRUE);gsub('"',"&quot;",x,fixed=TRUE)}
cards <- map_chr(seq_len(nrow(catalogue)),function(i) {
  z <- catalogue[i,];urls <- strsplit(z$sources," | ",fixed=TRUE)[[1]]
  links <- paste0('<a href="',esc(urls),'">Source ',seq_along(urls),'</a>',collapse=" \u00b7 ")
  paste0('<article data-section="',esc(z$section),'" data-kind="',esc(z$measurement_type),'"><div class="meta">',z$id,' \u00b7 ',esc(z$measurement_type),'</div><h2>',esc(z$title),'</h2><p>',esc(z$subtitle),'</p>',
    '<a href="',z$png,'"><img loading="lazy" src="',z$png,'" alt="',esc(z$title),'"></a><p>',esc(z$interpretation),'</p><p class="links"><a href="',z$png,'">PNG</a> \u00b7 <a href="',z$svg,'">SVG</a> \u00b7 <a href="',z$data_csv,'">Plotted data</a> \u00b7 ',links,'</p></article>')
})
options <- paste0('<option>',esc(unique(catalogue$section)),'</option>',collapse="")
kinds <- paste0('<option>',esc(unique(catalogue$measurement_type)),'</option>',collapse="")
html <- paste0('<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Ageing and NZ fiscal spending</title>',
  '<style>body{margin:0;background:#f4f6f8;color:#183044;font:16px/1.5 system-ui}header,main{max-width:1420px;margin:auto;padding:24px}h1{margin:0}header p{max-width:1100px}h2{font-size:20px}a{color:#075b91}.filters{display:flex;flex-wrap:wrap;gap:12px;position:sticky;top:0;background:#f4f6f8;padding:14px 0;z-index:1}select,input{padding:8px;font:inherit;max-width:100%}.grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(450px,1fr));gap:24px}article{background:white;border:1px solid #d5dfe5;border-radius:8px;padding:20px}img{width:100%;height:auto}.meta,.links{font-size:13px;color:#526579}[hidden]{display:none!important}@media(max-width:650px){.grid{grid-template-columns:1fr}header,main{padding:14px}}</style>',
  '<header><h1>Ageing and New Zealand fiscal spending</h1><p>28 charts: history, population projections, health-demand benchmarks, pension accounting and official long-term fiscal scenarios. Snapshot: 2 November 2026.</p>',
  '<p><a href="../../AGEING_RESEARCH_GUIDE.md">Research guide</a> \u00b7 <a href="../election_2026/index.html">Election / retirement pack</a> \u00b7 <a href="../op_ed_topics/index.html">Earlier evidence pack</a> \u00b7 <a href="chart_catalogue.csv">Catalogue</a> \u00b7 <a href="../../processed/ageing/key_findings.csv">Numeric research notes</a></p>',
  '<p><strong>Reading path:</strong> A04 \u2192 A07 \u2192 A08 \u2192 A14 \u2192 A12 \u2192 A18 \u2192 A17 \u2192 A26. Category spending growth is not automatically ageing. A fixed age-cost profile is a counterfactual benchmark, not a causal estimate.</p>',
  '<p>The 2025 LTFM is based on Budget 2025 forecasts through 2029, with projections from 2030. The newer PREFU comparison is separate (A15). Long-term scenarios are not predictions; LTFM and OLG results have different coverage and assumptions. Published population percentile bands are marginal and must not be added.</p>',
  '<div class="filters"><select id="section"><option value="">All sections</option>',options,'</select><select id="kind"><option value="">All measure types</option>',kinds,'</select><input id="search" aria-label="Search charts" placeholder="Search charts and notes"><span id="count"></span></div></header>',
  '<main class="grid">',paste(cards,collapse="\n"),'</main><script>const cards=[...document.querySelectorAll("article")],section=document.getElementById("section"),kind=document.getElementById("kind"),search=document.getElementById("search");function filter(){let n=0;for(const c of cards){c.hidden=!!((section.value&&section.value!==c.dataset.section)||(kind.value&&kind.value!==c.dataset.kind)||(search.value&&!c.textContent.toLowerCase().includes(search.value.toLowerCase())));if(!c.hidden)n++}document.getElementById("count").textContent=n+" charts"}for(const x of[section,kind,search])x.addEventListener("input",filter);filter();</script></html>')
writeLines(html,file.path(figure_dir,"index.html"),useBytes=TRUE)
message("Saved ",nrow(catalogue)," ageing charts: ",file.path(figure_dir,"index.html"))

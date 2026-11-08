# Standalone companion gallery for six election questions plus retirement design.
required <- c("dplyr", "tidyr", "purrr", "readr", "ggplot2", "scales", "svglite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing packages: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(purrr); library(readr); library(ggplot2)})
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[1]))) else normalizePath("Data pulls")
data_dir <- file.path(root, "processed", "election_2026")
figure_dir <- file.path(root, "figures", "election_2026")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
load_data <- function(n) read_csv(file.path(data_dir, paste0(n, ".csv")), show_col_types = FALSE)
old <- function(n) read_csv(file.path(root, "processed", "op_ed_topics", paste0(n, ".csv")), show_col_types = FALSE)
sources <- load_data("source_manifest")
catalogue <- list()
topics <- c(F = "1. Fiscal promises and the surplus", N = "2. Not taxing inflation and capital gains",
  W = "3. Benefits, jobs and fiscal savings", H = "4. Healthcare access and capacity",
  L = "5. Household costs and recovery", C = "6. Business tax and investment",
  S = "7. Compulsory saving and pre-funded annuities")
palette <- c("#087F8C", "#C56000", "#395A9A", "#A53B59", "#4D7342", "#8063A8", "#63584B")
theme_set(theme_minimal(base_size = 12))
scale_colour_discrete <- function(...) scale_colour_manual(..., values = palette)
scale_fill_discrete <- function(...) scale_fill_manual(..., values = palette)
cash <- scales::label_number(big.mark = ",", prefix = "$", accuracy = 1)
pct <- scales::label_number(suffix = "%", accuracy = .1)
wrap <- function(x, width = 32) vapply(x, function(y) paste(strwrap(y, width), collapse = "\n"), character(1))
save <- function(id, d, p, title, subtitle, note, source_ids, kind = "Observed / derived", height = 8) {
  src <- sources |> filter(source_id %in% source_ids)
  stopifnot(nrow(d) > 0, nrow(src) > 0, all(source_ids %in% sources$source_id))
  message("Saving ", id)
  caption <- paste(paste(strwrap(paste0(kind, ": ", note), 142), collapse = "\n"),
    paste0("Sources: ", paste(strwrap(paste(src$description, collapse = "; "), 140), collapse = "\n")), sep = "\n")
  p <- p + scale_colour_manual(values = palette, labels = function(x) wrap(x, 38)) +
    scale_fill_manual(values = palette, labels = function(x) wrap(x, 38)) +
    labs(title = title, subtitle = paste(strwrap(subtitle, 108), collapse = "\n"), caption = caption) +
    theme(plot.title = element_text(size = 19, face = "bold"), plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 9, hjust = 0, lineheight = 1.15), plot.title.position = "plot",
      plot.caption.position = "plot", legend.position = "bottom", legend.title = element_text(size = 10),
      legend.text = element_text(size = 10), plot.margin = margin(16, 20, 16, 16),
      panel.grid.minor = element_blank(), axis.title = element_text(size = 11)) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE), fill = guide_legend(nrow = 2, byrow = TRUE))
  ggsave(file.path(figure_dir, paste0(id, ".png")), p, width = 12, height = height, dpi = 180, bg = "white")
  ggsave(file.path(figure_dir, paste0(id, ".svg")), p, width = 12, height = height, bg = "white")
  write_csv(d, file.path(figure_dir, paste0(id, "_data.csv")), na = "")
  catalogue[[length(catalogue)+1L]] <<- tibble(id = id, topic = unname(topics[substr(id, 1, 1)]), title = title,
    subtitle = subtitle, measurement_type = kind, interpretation = note, sources = paste(src$url, collapse = " | "),
    png = paste0(id, ".png"), svg = paste0(id, ".svg"), data_csv = paste0(id, "_data.csv"))
}
line <- function(d, x, y, group, xlab, ylab) {
  ggplot(d, aes(x = .data[[x]], y = .data[[y]], colour = .data[[group]])) + geom_line(linewidth = .9) +
    labs(x = xlab, y = ylab, colour = NULL)
}
fiscal <- load_data("prefu_fiscal"); econ <- load_data("prefu_economic")
benefits <- load_data("prefu_benefits"); expenses <- load_data("prefu_expenses")
recipients <- load_data("prefu_recipients")
forecast_line <- geom_vline(xintercept = 2026.5, linetype = "dashed", colour = "grey50")
status_note <- "2026 fiscal results are unaudited; 2027 onward are forecasts (dashed divider). Policy baseline, not election promises."

# 1. Fiscal baseline, forecast revisions and composition.
d <- fiscal |> filter(block == "Share GDP", measure %in% c("Core Crown tax revenue", "Core Crown expenses")) |>
  mutate(percent_gdp = 100 * value)
save("F01_tax_and_expenses", d, line(d, "year", "percent_gdp", "measure", "June fiscal year", "% of GDP") + forecast_line,
  "What does the fiscal plan assume?", "PREFU 2026 tax and expense ratios", status_note, "prefu", "Actuals / unaudited / forecast")
d <- fiscal |> filter(block == "NZD millions", measure %in% c("OBEGAL (excluding ACC)", "OBEGAL (excluding minority interests)", "Core Crown residual cash")) |>
  mutate(billions = value/1000, measure = recode(measure, `OBEGAL (excluding ACC)` = "OBEGALx", `OBEGAL (excluding minority interests)` = "OBEGAL"))
save("F02_balance_and_cash", d, line(d,"year","billions","measure","June fiscal year","NZ$ billions") + forecast_line + geom_hline(yintercept=0, colour="grey50"),
  "A surplus and cash to repay debt are different", "Operating balances and residual cash", status_note, "prefu", "Actuals / unaudited / forecast")
befu <- old("treasury_fiscal")
d <- bind_rows(fiscal |> filter(block == "NZD millions", measure %in% c("Core Crown tax revenue", "Net core Crown debt")) |> mutate(vintage = "PREFU September 2026"),
  befu |> filter(measure %in% c("Core Crown tax revenue", "Net core Crown debt")) |> mutate(vintage = "BEFU May 2026")) |>
  filter(year >= 2025, year <= 2030) |> mutate(billions = value/1000)
save("F03_forecast_revisions", d, line(d,"year","billions","vintage","June fiscal year","NZ$ billions") + facet_wrap(~measure, scales="free_y"),
  "The baseline changed between Budget and the election", "Same measure and year, two Treasury vintages",
  "Revision is not a party's policy saving. Each vintage retains its own actual/forecast labels; the September baseline also changes assumptions.", c("prefu","existing_treasury_fiscal"), "Forecast-vintage comparison")
d <- fiscal |> filter(block == "NZD millions", year %in% c(2026,2031), measure %in% c("Core Crown revenue","Core Crown expenses")) |>
  select(measure,year,value) |> pivot_wider(names_from=year,values_from=value) |>
  mutate(change_billions = (`2031`-`2026`)/1000, contribution_to_core_balance = if_else(measure=="Core Crown expenses", -change_billions, change_billions))
save("F04_core_balance_arithmetic", d, ggplot(d,aes(measure,contribution_to_core_balance,fill=measure)) + geom_col(show.legend=FALSE) +
  geom_hline(yintercept=0) + scale_x_discrete(labels=wrap) + labs(x=NULL,y="Contribution to change (NZ$ billions)"),
  "Revenue growth and expense growth both matter", "2025/26 to 2030/31 change in the core Crown revenue-minus-expenses balance",
  "Nominal accounting decomposition. This is the core Crown balance, not consolidated OBEGALx, and does not identify inflation's causal contribution.", "prefu", "Derived from forecast")
d <- expenses |> filter(year %in% c(2026,2031)) |> select(measure,year,value) |> pivot_wider(names_from=year,values_from=value) |>
  mutate(change_billions=(`2031`-`2026`)/1000) |> arrange(change_billions)
save("F05_spending_growth_components", d, ggplot(d,aes(reorder(measure,change_billions),change_billions,fill=change_billions>0)) + geom_col(show.legend=FALSE) +
  coord_flip() + labs(x=NULL,y="Change in annual spending (NZ$ billions)"),
  "Where does forecast spending growth sit?", "Functional expense changes, 2025/26 to 2030/31",
  "Future operating allowances are unallocated, so existing health/education baselines are not the full future service budgets. Nominal figures include prices and demand.", "prefu_exp", "Derived from forecast", 10)
d <- fiscal |> filter(block=="Share GDP", measure %in% c("Net debt","Net debt (excl. NZS Fund)","Net core Crown debt")) |> mutate(percent_gdp=100*value)
save("F06_debt_definitions",d,line(d,"year","percent_gdp","measure","June fiscal year","% of GDP") + forecast_line,
  "Which debt measure is being promised?", "Three published Treasury definitions on the same chart",
  paste(status_note,"The definitions cover different assets and liabilities; a change in definition does not repay debt."), "prefu", "Actuals / unaudited / forecast")

# 2. Non-taxation of inflation: earnings, interest and capital gains.
d <- load_data("inflation_bracket_scenarios") |> filter(inflation==.02) |> mutate(income=paste0("$",scales::comma(real_income)))
save("N01_constant_real_earnings",d,line(d,"years","extra_real_tax","income","Years with unchanged nominal thresholds","Extra annual tax in starting-year dollars") + scale_y_continuous(labels=cash),
  "Tax can rise without a rise in real earnings", "2% annual inflation; fixed thresholds versus fully indexed thresholds; current rates in both arms",
  "Income-tax arithmetic only: no ACC, tax credits or transfers. This is a household scenario, not aggregate revenue or all Treasury fiscal drag.", "tax_rates", "Scenario")
d <- load_data("inflation_bracket_scenarios") |> filter(years==10) |> mutate(income=paste0("$",scales::comma(real_income)),inflation_percent=100*inflation)
save("N02_indexation_sensitivity",d,line(d,"inflation_percent","extra_real_tax","income","Assumed annual inflation (%)","Extra annual tax after ten years") + scale_y_continuous(labels=cash),
  "The cost of frozen thresholds depends on inflation", "Constant real earnings; the same progressive tax schedule throughout",
  "Annual endpoint difference, not ten-year cumulative revenue; CPI indexation removes inflation progression, not progression from real wage growth.", "tax_rates", "Scenario")
d <- load_data("interest_indexation") |> filter(tax_rate==.33) |> select(inflation,real_before,real_after_nominal_tax,real_after_indexed_tax) |>
  pivot_longer(-inflation,names_to="basis",values_to="real_return") |> mutate(inflation=100*inflation,real_return=100*real_return,
  basis=recode(basis,real_before="Before tax",real_after_nominal_tax="Tax on nominal interest",real_after_indexed_tax="Tax on positive real interest"))
save("N03_interest_real_returns",d,line(d,"inflation","real_return","basis","Inflation (%)","Real annual return (%)") + geom_hline(yintercept=0,linetype="dotted"),
  "An interest payment is not all purchasing-power income", "Assumed 5% nominal interest and 33% income-tax rate",
  "Indexed comparator deducts inflation compensation from taxable interest, with no refund for negative real returns. Rate is assumed, not a current deposit quote.", "tax_rates", "Scenario")
d <- load_data("interest_indexation") |> mutate(inflation=100*inflation,tax_rate=paste0(100*tax_rate,"%"),real_return=100*real_after_nominal_tax)
save("N04_interest_tax_rate_sensitivity",d,line(d,"inflation","real_return","tax_rate","Inflation (%)","Real after-tax return (%)") + geom_hline(yintercept=0),
  "Nominal-interest taxation affects taxpayers differently", "5% assumed nominal interest across illustrative tax rates",
  "Tax-rate comparisons do not describe the income or wealth distribution. Portfolio taxation has additional rules not modelled here.", "tax_rates", "Scenario")
d <- load_data("saving_wealth_scenarios") |> filter(inflation %in% c(.02,.04)) |>
  pivot_longer(c(nominal_tax_real_balance,indexed_tax_real_balance),names_to="basis",values_to="real_balance") |>
  mutate(inflation=paste0(100*inflation,"% inflation"),basis=if_else(grepl("^indexed",basis),"Tax on positive real interest","Tax on nominal interest"))
save("N05_saving_compounding",d,line(d,"years","real_balance","basis","Years","Starting-year purchasing power") + facet_wrap(~inflation) + scale_y_continuous(labels=cash),
  "Small differences in annual tax compound", "$100,000 initial saving; 5% nominal interest; 33% tax; no deposits or withdrawals",
  "A deterministic saving illustration; fixed rates and inflation, no fees or investment risk. It is not a retirement or policy revenue forecast.", "tax_rates", "Scenario")
d <- load_data("cgt_scenarios") |> filter(real_growth==0,years<=30) |> mutate(inflation_label=paste0(100*inflation,"% inflation"))
save("N06_cgt_inflation_component",d,line(d,"years","tax_on_inflation_real","inflation_label","Years held after valuation date","Extra tax versus indexation (real dollars)") + scale_y_continuous(labels=cash,limits=c(0,NA)),
  "How much capital-gains tax comes from inflation?", "$500,000 eligible property; 28% proposed rate; zero real capital growth",
  "Scenario begins at the proposed 1 July 2027 valuation/acquisition date. Zero improvements/costs; family home and other exempt assets excluded. Not a forecast of house prices or receipts.", c("labour_cgt","cgt_design"), "Proposed-policy scenario")
d <- load_data("cgt_scenarios") |> filter(inflation==.02,real_growth>0) |> mutate(real_growth=paste0(100*real_growth,"% real growth"),effective_tax=100*effective_tax_real_gain)
save("N07_cgt_effective_real_rate",d,line(d,"years","effective_tax","real_growth","Years held after valuation date","Tax / real capital gain (%)") + geom_hline(yintercept=28,linetype="dashed"),
  "A nominal tax rate and a tax on real gains differ", "28% of nominal gains; 2% inflation; positive real gains only",
  "Undefined when real gains are zero, so those cases are excluded. Capital-only comparison: ignores rental returns, deductions and financing. Indexation also changes revenue and simplicity.", c("labour_cgt","cgt_design"), "Proposed-policy scenario")
d <- load_data("cpi") |> filter(date>=as.Date("2010-06-01")) |> mutate(purchasing_power=100*first(cpi)/cpi,measure="Purchasing power of $100")
save("N08_purchasing_power",d,line(d,"date","purchasing_power","measure",NULL,"Purchasing power in June 2010 dollars") + guides(colour="none"),
  "Why nominal amounts need a price benchmark", "Observed national CPI; June 2010 purchasing power = 100",
  "CPI is a broad consumption benchmark, not each household's inflation and not an asset-price index. Shows price-level accumulation rather than an annual inflation rate.", "cpi")

# 3. Benefits: composition, cyclical exposure and savings arithmetic.
d <- benefits |> filter(measure %in% c("New Zealand Superannuation","Jobseeker Support and Emergency Benefit","Supported Living Payment","Sole Parent Support")) |>
  mutate(percent_gdp=100*value/gdp_millions)
save("W01_benefits_fiscal_scale",d,line(d,"year","percent_gdp","measure","June fiscal year","% of GDP") + forecast_line,
  "Separate the programmes before discussing welfare savings", "Major benefit expenses, PREFU 2026",
  paste(status_note,"Jobseeker includes health-related recipients and Emergency Benefit; it is not spending solely on unemployed work-ready people."), "prefu_exp", "Actuals / unaudited / forecast")
d <- benefits |> filter(year %in% c(2026,2031)) |> select(measure,year,value) |> pivot_wider(names_from=year,values_from=value) |>
  mutate(change_billions=(`2031`-`2026`)/1000) |> arrange(desc(abs(change_billions))) |> slice_head(n=10)
save("W02_benefit_growth",d,ggplot(d,aes(reorder(measure,change_billions),change_billions)) + geom_col(fill=palette[1]) + coord_flip() + labs(x=NULL,y="Change in annual expense (NZ$ billions)"),
  "Which benefits drive forecast spending growth?", "Ten largest absolute changes between 2025/26 and 2030/31",
  "Nominal changes combine payment rates, numbers and programme changes. This does not identify a policy saving or imply that programmes serve the same purpose.", "prefu_exp", "Derived from forecast",9)
d <- old("jobseeker_history")
# Source schema is checked before selecting the long, official backcast history.
stopifnot(all(c("date","subgroup","recipients") %in% names(d)))
d <- d |> mutate(date=as.Date(date),quarter_date=as.Date(format(date,"%Y-%m-01"))) |>
  filter(quarter_date>=as.Date("2008-09-01"),quarter_date<=as.Date("2026-06-01"))
save("W03_jobseeker_health_workready",d,line(d,"date","recipients","subgroup",NULL,"Recipients") + scale_y_continuous(labels=scales::comma),
  "Two different groups within Jobseeker Support", "Quarter-end counts from 2008; official backcasts before July 2013",
  "Work-ready and health-condition/disability categories do not identify employment status. Some recipients work. Source rounding and classification changes matter.", "existing_jobseeker_history")
labour <- load_data("labour")
d <- labour |> filter(series %in% c("HLFQ.S1F3S","HLFQ.S1H3S"), year>=2000)
save("W04_labour_demand_context",d,line(d,"date","value","measure",NULL,"Percent") + facet_wrap(~measure,scales="free_y") + guides(colour="none"),
  "Benefit receipt needs labour-market context", "HLFS seasonally adjusted rates, 2000 to June 2026",
  "Employment rate uses the working-age population; unemployment uses the labour force. Neither is an administrative Jobseeker count. Descriptive context, not causal attribution.", "labour")
js_base <- benefits |> filter(year==2026,measure=="Jobseeker Support and Emergency Benefit") |> pull(value)
core_base <- fiscal |> filter(year==2026,block=="NZD millions",measure=="Core Crown expenses") |> pull(value)
d <- crossing(expense_reduction=seq(0,.20,.01),replacement_cost_share=c(0,.25,.5,.75)) |>
  mutate(gross_saving=js_base*expense_reduction,net_before_admin=gross_saving*(1-replacement_cost_share),
    net_percent_core=100*net_before_admin/core_base,cost_label=paste0(100*replacement_cost_share,"% offsets"))
save("W05_savings_offsets",d,line(d,"expense_reduction","net_percent_core","cost_label","Hypothetical reduction in total Jobseeker + Emergency expense","Saving as % of core Crown expenses") + scale_x_continuous(labels=scales::label_percent()),
  "Gross benefit reductions are not net fiscal savings", "2025/26 unaudited expense base; offsets are assumptions",
  "Whole-programme arithmetic, not a work-ready-only cut or party costing. Offsets combine replacement assistance/lost taxes; administration and behavioural responses still excluded.", "prefu_exp", "Scenario")
d <- old("benefit_history_groups") |> filter(group %in% c("NZ Superannuation","Main income-replacement basket"))
save("W06_long_spending_context",d,line(d,"year","percent_gdp","group","June fiscal year","% of GDP"),
  "Put the current debate in a longer spending history", "Treasury actual expenses, 2001-2025; separately dated historical pack",
  "Income-replacement basket joins predecessor programmes across reforms. Broad classification is not the work-ready spending base; older vintage differs from restated PREFU values.", "existing_benefit_history_groups")

# 4. Health: observed access and needs, followed by labelled capacity scenarios.
health <- load_data("health_survey")
barriers <- c("Unmet need for GP due to cost","Unmet need for GP due to wait time")
health_note <- "Survey adults are 15+; year labels name July-June survey periods. Barriers can overlap and must not be added. Ribbons/bars are published 95% confidence intervals."
d <- health |> filter(population=="adults",group=="Total",indicator %in% barriers,!is.na(estimate)) |> mutate(end_year=year+1)
save("H01_access_barriers_history",d,line(d,"end_year","estimate","indicator","Survey year ending June","Adults reporting barrier (%)") +
  geom_ribbon(aes(ymin=low,ymax=high,fill=indicator),alpha=.12,colour=NA,show.legend=FALSE),
  "Cost and appointment availability are separate barriers", "NZ Health Survey; compare shared years rather than imputing missing observations",health_note,"health")
d <- health |> filter(population=="adults",year==2024,indicator %in% barriers,group %in% c("15-24","25-34","35-44","45-54","55-64","65-74","75+"),!is.na(estimate))
save("H02_access_by_age",d,ggplot(d,aes(group,estimate,colour=indicator,group=indicator)) + geom_point(position=position_dodge(width=.4),size=2.5) +
  geom_errorbar(aes(ymin=low,ymax=high),position=position_dodge(width=.4),width=.15) + labs(x="Age",y="Adults reporting barrier (%)",colour=NULL),
  "Who faces each access barrier?", "Adults by age, 2024/25",health_note,"health")
d <- health |> filter(population=="adults",year==2024,indicator %in% barriers,grepl("^Quintile",group),!is.na(estimate))
save("H03_access_by_deprivation",d,ggplot(d,aes(group,estimate,colour=indicator,group=indicator)) + geom_point(position=position_dodge(.4),size=2.5) +
  geom_errorbar(aes(ymin=low,ymax=high),position=position_dodge(.4),width=.15) + labs(x="Neighbourhood deprivation quintile (5 = most deprived)",y="Adults reporting barrier (%)",colour=NULL),
  "Does the access problem follow deprivation?", "NZDep neighbourhood groups, 2024/25",
  paste(health_note,"Area deprivation is not personal income; these are unadjusted prevalences."),"health")
d <- health |> filter(population=="adults",year==2024,indicator %in% barriers,group %in% c("Disabled","Non-disabled"),!is.na(estimate))
save("H04_access_by_disability",d,ggplot(d,aes(group,estimate,colour=indicator)) + geom_point(position=position_dodge(.4),size=3) +
  geom_errorbar(aes(ymin=low,ymax=high),position=position_dodge(.4),width=.15) + labs(x=NULL,y="Adults reporting barrier (%)",colour=NULL),
  "Access matters most when need is greater", "Disabled and non-disabled adults, 2024/25",paste(health_note,"Unadjusted comparison; age and need differ."),"health")
d <- health |> filter(population=="adults",year==2024,indicator %in% barriers,group %in% c("M\u0101ori","Pacific","Asian","European/Other"),!is.na(estimate))
save("H05_access_by_ethnicity",d,ggplot(d,aes(group,estimate,colour=indicator)) + geom_point(position=position_dodge(.4),size=2.5) +
  geom_errorbar(aes(ymin=low,ymax=high),position=position_dodge(.4),width=.15) + labs(x=NULL,y="Adults reporting barrier (%)",colour=NULL),
  "Access barriers differ across population groups", "Adults, 2024/25; total-response ethnicity",
  paste(health_note,"Ethnic groups overlap; differences are unadjusted and do not identify causes."),"health")
d <- health |> filter(population=="adults",year==2024,indicator %in% c("GP visit","Last GP visit (any location) free"),
  group %in% c("15-24","25-34","35-44","45-54","55-64","65-74","75+"),!is.na(estimate))
save("H06_use_and_free_visits",d,ggplot(d,aes(group,estimate,colour=indicator,group=indicator)) + geom_point(size=2.5) +
  geom_errorbar(aes(ymin=low,ymax=high),width=.12) + facet_wrap(~indicator) + guides(colour="none") + labs(x="Age",y="Percent"),
  "New subsidies also cover some existing care", "GP use and reported free last visits by age, 2024/25",
  "GP visit measures past-year contact; free-last-visit uses the indicator's eligible visitor population. Denominators differ; neither gives the number of newly created appointments.","health")
d <- expenses |> filter(measure=="Health") |> mutate(percent_gdp=100*value/gdp_millions)
save("H07_health_budget_baseline",d,ggplot(d,aes(year,percent_gdp)) + geom_line(linewidth=1,colour=palette[1]) + forecast_line + labs(x="June fiscal year",y="Health expenses / GDP (%)"),
  "Read future health funding alongside unallocated allowances", "PREFU existing functional baselines",paste(status_note,"Future new spending is not yet allocated to health; this is not a complete final health budget or productivity measure."),"prefu_exp","Actuals / unaudited / forecast")
d <- crossing(additional_visits_millions=seq(.25,4,.25),additional_cost_millions=c(250,500,750)) |>
  mutate(cost_per_extra_visit=additional_cost_millions/additional_visits_millions,cost=paste0("$",additional_cost_millions,"m extra spending"))
save("H08_additional_visit_unit_cost",d,line(d,"additional_visits_millions","cost_per_extra_visit","cost","Additional completed visits per year (millions)","Public cost per additional visit") + scale_y_continuous(labels=cash),
  "Value depends on additional care, not subsidised visits alone", "Sensitivity grid: extra spending divided by genuinely additional completed appointments",
  "Entirely illustrative, not a Medicard costing. No baseline visit substitution, downstream health savings, quality or capacity response has been estimated.","labour_medicard","Scenario")

# 5. Actual household prices and labour incomes; do not call wages disposable income.
hlpi <- load_data("hlpi"); cpi <- load_data("cpi")
selected <- c("allhh","benef","super","disq1","disq5")
d <- hlpi |> filter(nzhec=="A",hlpi %in% selected,year>=2017)
save("L01_household_inflation",d,line(d,"date","change.a","hlpi_name",NULL,"Annual living-cost inflation (%)"),
  "There is no single household inflation experience", "Household living-cost indexes through September 2026",
  "Indexes reflect group expenditure baskets, including interest payments. Income groups can change composition; these are not the same households followed over time.","hlpi")
d <- hlpi |> filter(nzhec=="A",hlpi %in% selected,date>=as.Date("2019-12-01")) |> group_by(hlpi) |>
  arrange(date,.by_group=TRUE) |> mutate(cumulative_cost=100*index/first(index)) |> ungroup()
save("L02_cumulative_household_costs",d,line(d,"date","cumulative_cost","hlpi_name",NULL,"December 2019 = 100"),
  "Lower inflation does not restore the earlier price level", "Cumulative cost of maintaining each group's spending basket",
  "A cost index is not household wellbeing or disposable income; expenditure weights update over time. Base quarter and endpoint are common across groups.","hlpi")
d <- hlpi |> filter(hlpi=="allhh",nzhec %in% c("1","4","7","13"),date>=as.Date("2019-12-01")) |> group_by(nzhec) |>
  arrange(date,.by_group=TRUE) |> mutate(index_2019=100*index/first(index)) |> ungroup()
save("L03_cost_components",d,line(d,"date","index_2019","nzhec_name",NULL,"December 2019 = 100"),
  "Which costs changed the household budget?", "Selected group-level price indexes for all households",
  "Component price growth is not its contribution to total inflation: expenditure weights differ. Housing/utilities and interest are distinct HLPI groups.","hlpi")
wages <- labour |> filter(series %in% c("QEMQ.SAAZ9A","LCIQ.SD53Z9"),date>=as.Date("2019-12-01")) |>
  left_join(cpi |> select(date,cpi),by="date") |> group_by(series) |> arrange(date,.by_group=TRUE) |>
  mutate(nominal_index=100*value/first(value),real_index=100*(value/cpi)/first(value/cpi),
    label=if_else(series=="QEMQ.SAAZ9A","Average hourly earnings (QES)","Fixed-quality wage rates (LCI)")) |> ungroup()
d <- wages |> pivot_longer(c(nominal_index,real_index),names_to="basis",values_to="index") |> mutate(basis=if_else(basis=="real_index","CPI-adjusted","Nominal"))
save("L04_wages_and_prices",d,line(d,"date","index","basis",NULL,"December 2019 = 100") + facet_wrap(~label),
  "Are wage gains restoring purchasing power?", "Observed wages through June 2026; average earnings versus fixed-quality wage rates",
  "QES averages move with workforce composition; LCI controls job quality. Gross hourly wages exclude taxes, transfers, hours changes and non-wage income.",c("labour","cpi"))
d <- labour |> filter(series=="QEMQ.SAAZ9A",date>=as.Date("2019-12-01")) |> select(date,wage=value) |>
  inner_join(hlpi |> filter(nzhec=="A",hlpi %in% selected) |> select(date,hlpi_name,index),by="date") |>
  group_by(hlpi_name) |> arrange(date,.by_group=TRUE) |> mutate(purchasing_power=100*(wage/index)/first(wage/index)) |> ungroup()
save("L05_wage_basket_sensitivity",d,line(d,"date","purchasing_power","hlpi_name",NULL,"December 2019 = 100"),
  "The same wage series buys different household baskets", "National average gross hourly earnings deflated by each group's living costs",
  "Benchmark comparison, NOT measured wage growth of beneficiaries, retirees or each income quintile. Shows basket sensitivity, not household disposable-income recovery.",c("labour","hlpi"),"Context proxy")
d <- labour |> filter(series %in% c("HLFQ.S1A3S","QEMQ.SDAZ9AS"),date>=as.Date("2019-12-01")) |> group_by(series) |>
  arrange(date,.by_group=TRUE) |> mutate(index=100*value/first(value),label=if_else(series=="HLFQ.S1A3S","Employment, HLFS","Ordinary weekly paid hours per FTE, QES")) |> ungroup()
save("L06_jobs_and_hours",d,line(d,"date","index","label",NULL,"December 2019 = 100"),
  "Hourly wages are only part of earned income", "Seasonally adjusted employment and paid-hours context through June 2026",
  "Different survey populations: QES paid hours per FTE are not HLFS total hours. Do not multiply these series into a household income estimate.","labour")
d <- load_data("household_profiles") |> filter(year==max(year),hlpi %in% selected) |> mutate(owned_without_mortgage=own_prop-own_wm_prop,
  owned_with_mortgage=own_wm_prop,other_tenure=100-own_prop) |>
  select(hlpi_name,year,owned_without_mortgage,owned_with_mortgage,other_tenure) |>
  pivot_longer(c(owned_without_mortgage,owned_with_mortgage,other_tenure),names_to="tenure",values_to="percent")
save("L07_household_tenure",d,ggplot(d,aes(hlpi_name,percent,fill=tenure)) + geom_col() + coord_flip() + labs(x=NULL,y="Households (%)",fill=NULL),
  "Housing tenure helps explain different cost pressures", paste("Latest household-profile benchmark year:",max(d$year)),
  "Historical survey benchmark, not September 2026 tenure. Other tenure is the residual, not solely private renters. Ownership categories are source household definitions.","hlpi_facts")

# 6. Business taxes, cash flow and additional investment.
d <- load_data("business_first_year")
save("C01_first_year_tax",d,line(d,"profit_before_investment","tax_due","method","Profit before the $100,000 investment deduction","First-year company tax") + scale_x_continuous(labels=cash) + scale_y_continuous(labels=cash),
  "Tax-rate cuts and investment deductions target different firms", "Illustrative new $100,000 eligible asset; 20% diminishing-value depreciation",
  "28% standard rate versus proposed 20% SME rate and deduction alternatives. No loss refunds, financing or imputation; profit is before asset deductions. Illustrative, not a party costing.",c("nzfirst_sme","investment_boost"),"Scenario")
d <- load_data("business_first_year") |> filter(profit_before_investment<=150000)
save("C02_unused_deductions",d,line(d,"profit_before_investment","unused_loss","method","Profit before the investment deduction","Unused first-year tax loss") + scale_x_continuous(labels=cash) + scale_y_continuous(labels=cash),
  "A deduction need not deliver cash to a loss-making firm", "Same illustrative investment; tax losses are not refunded in this model",
  "Carry-forward value depends on future taxable profits and loss rules. This chart measures unused deductions, not their ultimate economic value.","nzfirst_sme","Scenario")
d <- load_data("business_deductions") |> filter(year<=15)
save("C03_deduction_timing",d,line(d,"year","deduction","method","Years after investment (0 = purchase)","Deduction in that year") + scale_y_continuous(labels=cash),
  "Expensing primarily changes the timing of deductions", "$100,000 asset; full lifetime deductions reconcile to its cost",
  "Assumed depreciation rate. Investment Boost deducts 20% at purchase and depreciates the remaining 80%; ordinary depreciation starts at year one. Profits sufficient to use deductions.","investment_boost","Scenario")
d <- load_data("business_pv") |> mutate(discount_percent=100*discount)
save("C04_present_value_tax_relief",d,line(d,"discount_percent","pv_tax_shield","method","Discount rate (%)","Present value of tax deductions") + scale_y_continuous(labels=cash),
  "Compare tax relief in present-value terms", "Same asset and cash-flow timing, different rates and deductions",
  "Assumes deductions can be used immediately when scheduled. Does not measure induced investment, compliance costs or the fiscal cost of a tax change.","nzfirst_sme","Scenario")
d <- load_data("business_marginal_wedge") |> mutate(required_return_percent=100*real_required_return,wedge_percent=100*effective_marginal_wedge)
save("C05_marginal_investment",d,line(d,"required_return_percent","wedge_percent","method","Required real after-tax return (%)","Stylised effective marginal tax wedge (%)"),
  "Which change lowers the hurdle for a new investment?", "Perpetual unlevered cash-flow model with an immediately usable deduction",
  "Zero inflation, no disposal value, no shareholder tax or financing. Full expensing gives zero wedge under these assumptions; this is not an economy-wide empirical estimate.","nzfirst_sme","Model scenario")
d <- old("world_bank") |> filter(measure=="rd_gdp",iso3 %in% c("NZL","AUS","OED"))
save("C06_rd_context",d,line(d,"year","value","country","Year","R&D expenditure / GDP (%)"),
  "Investment policy needs an outcome benchmark", "R&D intensity: New Zealand, Australia and the OECD aggregate",
  "Descriptive context from the existing snapshot. R&D intensity is not an estimate of tax-credit additionality, and comparator institutions differ.","existing_world_bank","Context proxy")
d <- old("rd_expenditure") |> filter(breakdown=="Sector",!is.na(value_millions))
save("C07_rd_by_sector",d,line(d,"year","value_millions","category","Survey year","R&D expenditure (NZ$ millions)"),
  "Who performs research and development?", "Stats NZ published sector observations; available survey years only",
  "Spending is nominal; business 2025 and all-sector 2024 coverage differ. Sampling errors and footnotes are retained in the data. No interpolation or causal credit effect is estimated.","existing_rd_expenditure")
d <- old("tax_system_spending") |> filter(programme=="R&D tax incentive")
save("C08_rd_incentive_expense",d,ggplot(d,aes(year_end,value_millions,colour=status)) + geom_point(size=3) + geom_line(aes(group=1),colour=palette[1]) + labs(x="June fiscal year",y="R&D tax incentive expense (NZ$ millions)",colour=NULL),
  "Track the fiscal input separately from the R&D outcome", "2026 tax expenditure statement; published actual/estimate/forecast labels",
  "Expense measures support through the tax system, not additional R&D caused by it. Do not divide total R&D by this series and call the ratio a return on the credit.","existing_tax_system_spending","Actuals / estimates / forecast")

# 7. Compulsory saving versus pre-funding a lifelong income.
kiwi <- load_data("kiwisaver"); life <- load_data("life_table_nz")
d <- load_data("kiwisaver_participation") |> mutate(contributing=100*contributed/members_any_time,noncontributing=100*no_contribution/members_any_time) |>
  pivot_longer(c(contributing,noncontributing),names_to="status",values_to="percent")
save("S01_kiwisaver_participation",d,line(d,"year","percent","status","June year","Members at any time in year (%)"),
  "Membership and active saving are different measures", "IRD contributions through Inland Revenue or reported by providers",
  "Includes children, retirees and overseas members. Direct-provider contributions can be incompletely observed. Not the share of employed adults failing to save.","kiwi")
d <- kiwi |> filter(source_sheet=="Payments to providers 2",year==2026) |> mutate(label=if_else(measure=="No contribution","No PAYE contribution",paste0(100*suppressWarnings(as.numeric(measure)),"%")))
save("S02_payroll_contribution_rates",d,ggplot(d,aes(label,value/1e6)) + geom_col(fill=palette[1]) + scale_x_discrete(labels=function(x)wrap(x,16)) + labs(x="Most recent employee payroll contribution rate",y="Members (millions)"),
  "Where compulsory saving would start", "IRD employee contribution-rate classification, June 2026",
  "No PAYE contribution includes members saving outside payroll. These are employee rates; employer contributions face ESCT. 3.5% is the current default, 6% is a proposed future rate.",c("kiwi","national_kiwi"))
d <- kiwi |> filter(source_sheet=="Payments to providers 1",measure %in% c("Employee deductions","Employer contributions","Government contribution"))
save("S03_contribution_sources",d,line(d,"year","value","measure","June year","Payments through IRD (NZ$ millions)"),
  "Who supplies the recorded retirement contributions?", "Gross flows to providers through Inland Revenue",
  "Excludes voluntary payments directly to providers. Government payments can lag the earning year; these are flows, not balances, net national saving or economic incidence.","kiwi")
d <- kiwi |> filter(source_sheet=="Amount withdrawn 1",measure!="Total") |> mutate(billions=value/1e9)
save("S04_early_withdrawals",d,line(d,"year","billions","measure","June year","Withdrawals (NZ$ billions)"),
  "Not all KiwiSaver saving stays invested until retirement", "First-home and hardship withdrawals reported to IRD",
  "IRD notes provider reporting may be incomplete and these are not all withdrawal types. First-home use buys another asset; it is not necessarily a reduction in household wealth.","kiwi")
d <- life |> filter(age>=65) |> mutate(survival_percent=100*survival_from_65)
save("S05_survival_after_65",d,line(d,"age","survival_percent","sex","Age","Survival conditional on reaching 65 (%)"),
  "Retirement lasts an uncertain number of years", "Stats NZ 2022-2024 period mortality; national median life-table estimates",
  "Period tables hold observed mortality rates fixed. They are not a forecast for today's workers; future longevity, health and group differences matter. 105 is an open interval.","life","Derived from observed mortality")
d <- load_data("retirement_longevity") |> filter(discount==.02) |> mutate(age=65+horizon,outlive_percent=100*outlive_probability)
save("S06_drawdown_longevity_risk",d,ggplot(d,aes(age,outlive_percent)) + geom_col(fill=palette[1]) + labs(x="Age when a fixed drawdown plan ends",y="Probability of living beyond that age (%)"),
  "Planning to an average lifespan leaves longevity risk", "Equal weights on male/female survivors at 65; period-life-table arithmetic",
  "A fixed drawdown plan is not lifetime insurance. No mortality improvement assumed. Unisex weights are illustrative, not actual buyer weights or population forecasts.","life","Mortality-based scenario")
d <- load_data("annuity_capital") |> filter(real_payment_growth==0) |> mutate(discount_percent=100*discount)
save("S07_annuity_capital",d,line(d,"discount_percent","loaded_capital","sex","Real liability discount rate (%)","Capital at age 65") + scale_y_continuous(labels=cash),
  "What capital funds a lifelong real income?", "$30,000 a year, CPI-level single-life annuity; assumed 5% expense loading",
  "Actuarial illustration, not an annuity quote or NZ Super liability valuation. No survivor/refund benefit; period mortality and approximate midyear timing including the 105+ tail.","life","Actuarial model scenario")
d <- load_data("retirement_longevity") |> filter(discount==.02) |> select(horizon,self_insured_capital,pooled_capital) |>
  pivot_longer(c(self_insured_capital,pooled_capital),names_to="method",values_to="capital")
save("S08_pooling_vs_long_drawdown",d,line(d,"horizon","capital","method","Fixed retirement drawdown horizon (years)","Capital for $30,000 per year") + scale_y_continuous(labels=cash),
  "Pooling longevity changes the financing arithmetic", "2% real discount; actuarial cost without expense loading in both comparisons",
  "The fixed plan pays everyone for the whole chosen horizon and can leave bequests. The annuity stops at death, has no bequest, and relies on a solvent pool. Not equivalent contracts.","life","Actuarial model scenario")
d <- load_data("retirement_accumulation") |> filter(real_return==.03) |> mutate(contribution=paste0(100*net_contribution,"% invested"))
save("S09_balances_and_contributions",d,line(d,"age","balance","contribution","Age","Balance in constant dollars") + scale_y_continuous(labels=cash),
  "Compulsory deposits build assets, not a specified pension", "$70,000 constant real gross earnings; 3% return net of tax and fees",
  "Contribution fractions are net amounts invested, not employee-plus-employer headline rates. End-year deposits, no breaks/withdrawals. Not an estimate of additional national saving.","kiwi_rules","Scenario")
d <- load_data("retirement_accumulation") |> filter(years==40) |> mutate(return=paste0(100*real_return,"% net real return"),contribution=100*net_contribution)
save("S10_balance_to_income",d,line(d,"contribution","implied_real_annuity","return","Net amount invested / gross earnings (%)","Illustrative lifetime real income / year") + scale_y_continuous(labels=cash),
  "Translate retirement balances into income", "Forty years of contributions; same annuity pricing assumptions across accumulation returns",
  "Annuity uses 2% real liability discount, period mortality, 5% loading. Risky accumulation returns do not increase the liability discount. CPI-level payout, not wage-indexed NZ Super.",c("kiwi_rules","life"),"Scenario")
d <- load_data("retirement_career_gaps")
save("S11_career_gaps",d,ggplot(d,aes(reorder(career,implied_real_annuity),implied_real_annuity)) + geom_col(fill=palette[1]) + coord_flip() + scale_y_continuous(labels=cash) + labs(x=NULL,y="Illustrative lifetime real income / year"),
  "Contribution-based pensions reflect earnings histories", "$70,000 full-time earnings; 8% invested; 3% net real return; same annuity basis",
  "No compensating caregiver credits or government top-ups. Scenarios isolate timing and earnings; they do not estimate actual gender or disability gaps.","life","Scenario")
d <- load_data("retirement_fees") |> filter(years==40) |> mutate(fee_percent=100*fee)
save("S12_fee_sensitivity",d,ggplot(d,aes(fee_percent,implied_real_annuity)) + geom_line(linewidth=1,colour=palette[1]) + scale_y_continuous(labels=cash) + labs(x="Annual asset fee (%)",y="Illustrative lifetime real income / year"),
  "Annual fees compound into differences in retirement income", "8% invested from $70,000 earnings for 40 years; 4% real return after tax, before fees",
  "Assumed fee range, not measured KiwiSaver fees. Fees charged proportionally to assets after gross investment return. Same mortality and annuity expense loading throughout.","life","Scenario")
d <- load_data("retirement_required_contributions") |> filter(target_real_pension==30000) |> mutate(contribution=100*net_contribution_rate,return=paste0(100*real_return,"% net real return"))
save("S13_required_contributions",d,line(d,"start_age","contribution","return","Age contributions begin","Net invested contribution / $70,000 earnings (%)"),
  "Pre-funding a promised income requires an explicit contribution", "Target $30,000 CPI-level lifelong income from 65; fixed annuity pricing basis",
  "Conditional on survival to 65; no pre-retirement mortality pooling. Required contribution is a net deposit, not a payroll tax or a costing of universal NZ Super. Return risk remains.","life","Scenario")
d <- load_data("retirement_distribution") |> select(salary,own_account_annuity,common_pension) |>
  pivot_longer(c(own_account_annuity,common_pension),names_to="rule",values_to="pension")
save("S14_equal_vs_earnings_pension",d,line(d,"salary","pension","rule","Constant annual real earnings","Illustrative lifetime real income / year") + scale_x_continuous(labels=cash) + scale_y_continuous(labels=cash),
  "Individual ownership and a common pension distribute differently", "Same total deposits from an artificial equal-sized earnings grid; 8% invested for 40 years",
  "Common benefit redistributes within this synthetic cohort. Grid is not NZ's earnings distribution. No aggregate saving advantage is assumed; mortality and pricing are identical.","life","Distribution scenario")
d <- load_data("prefunding_transition") |> filter(real_return==.03) |> mutate(contribution=paste0(extra_prefunding_percent_gdp,"% GDP deposited"))
save("S15_prefunding_transition",d,line(d,"year","assets_percent_gdp","contribution","Years after starting extra pre-funding","Accumulated fund / GDP (%)"),
  "Pre-funding requires resources during the transition", "Extra end-year deposits; 3% net real return; 2% real GDP growth; no withdrawals",
  "Existing pensions still need financing. If deposits are debt-funded, gross assets overstate net pre-funding; no borrowing costs, tax changes or pension offsets are included.","prefu","Scenario")
d <- load_data("retirement_pay_incidence") |> mutate(rate=paste0(100*employee_rate,"% each")) |>
  select(rate,remuneration,take_home_before_acc,net_deposit) |> pivot_longer(c(take_home_before_acc,net_deposit),names_to="flow",values_to="dollars")
d <- d |> mutate(flow=recode(flow,take_home_before_acc="Take-home pay before ACC",net_deposit="Net invested contribution"))
save("S16_pay_and_deposits",d,ggplot(d,aes(rate,dollars,fill=flow)) + geom_col(position="dodge") + facet_wrap(~remuneration) + scale_y_continuous(labels=cash) + labs(x="Employee and employer headline rates",y="Annual dollars",fill=NULL),
  "Who funds higher retirement contributions?", "Two remuneration assumptions; 30% illustrative ESCT; current income-tax schedule",
  "Employer contributions need not be a free addition to lifetime pay. Includes no ACC, government top-up or wage bargaining response. Scenarios bracket assumptions, not measured incidence.",c("kiwi_rules","national_kiwi"),"Scenario")
d <- load_data("nzsf_return_indices") |> filter(!grepl("Long-term",series)) |>
  mutate(series=case_when(grepl("^NZ Super",series)~"NZ Super Fund",grepl("Passive",series)~"Passive reference portfolio",TRUE~"Treasury bill benchmark"))
save("S17_nzsf_return_history",d,line(d,"date","return_index","series",NULL,"Cumulative return index at inception = 1"),
  "Collective pre-funding already has an investment history", "NZ Super Fund published cumulative indices through its latest saved month",
  "Provisional, after costs and before NZ tax. Benchmark risk differs; historical excess returns are not guaranteed or a risk-free financing gain. Index excludes government cash contributions.","nzsf","Official provisional return indices")
d <- load_data("annuity_capital") |> filter(sex=="Equal-weight pool") |> mutate(discount_percent=100*discount,growth=paste0(100*real_payment_growth,"% real annual pension growth"))
save("S18_pension_indexation",d,line(d,"discount_percent","loaded_capital","growth","Real liability discount rate (%)","Capital at age 65") + scale_y_continuous(labels=cash),
  "CPI-level and growing real pensions cost different amounts", "$30,000 initial annual pension; same mortality and 5% loading",
  "1% real payment growth is a sensitivity, not the statutory NZ Super indexation formula or a wage forecast. Comparing individual balances with public promises requires matching indexation.","life","Actuarial model scenario")
d <- benefits |> filter(measure=="New Zealand Superannuation") |> inner_join(recipients |> filter(measure=="New Zealand Superannuation") |> select(year,recipient_thousands=value),by="year") |>
  mutate(expense_index=100*value/value[year==2026],recipient_index=100*recipient_thousands/recipient_thousands[year==2026]) |>
  pivot_longer(c(expense_index,recipient_index),names_to="measure_type",values_to="index")
save("S19_public_pension_baseline",d,line(d,"year","index","measure_type","June fiscal year","2025/26 = 100") + forecast_line,
  "The public pension promise grows with numbers and payment rates", "PREFU NZ Super expense and published recipient-count series",
  paste(status_note,"Recipient statistics and expense flows need consistent timing; ratio is not a statutory payment rate. Individual KiwiSaver balances do not automatically reduce this public obligation."),"prefu_exp","Actuals / unaudited / forecast")
d <- load_data("national_saving_sensitivity") |> mutate(displaced_percent=100*displaced_other_saving,
  offset=paste0(100*public_cost_share,"% public dissaving"))
save("S20_accounts_vs_national_saving",d,line(d,"displaced_percent","net_national_saving_billion","offset","Displacement of saving in other assets / extra account deposits (%)","Additional national saving (NZ$ billions)") + geom_hline(yintercept=0,linetype="dashed"),
  "Bigger retirement accounts need not mean equal new saving", "Illustrative $5 billion increase in account deposits, including any public subsidy",
  "Static accounting sensitivity: subtract displaced other saving and additional public dissaving. Not estimated KiwiSaver additionality. Does not model consumption, investment or general-equilibrium responses.","national_kiwi","Scenario")

# Reproducible catalogue and local searchable gallery, with topic reading paths.
catalogue <- bind_rows(catalogue)
stopifnot(!anyDuplicated(catalogue$id),length(unique(catalogue$topic))==7,
  all(file.exists(file.path(figure_dir,catalogue$png))), all(file.exists(file.path(figure_dir,catalogue$svg))))
write_csv(catalogue,file.path(figure_dir,"chart_catalogue.csv"))
esc <- function(x) {
  x <- gsub("&","&amp;",x,fixed=TRUE); x <- gsub("<","&lt;",x,fixed=TRUE)
  x <- gsub(">","&gt;",x,fixed=TRUE); gsub('"',"&quot;",x,fixed=TRUE)
}
cards <- map_chr(seq_len(nrow(catalogue)),function(i) {
  z <- catalogue[i,]; urls <- strsplit(z$sources," | ",fixed=TRUE)[[1]]
  links <- paste0('<a href="',esc(urls),'">Source ',seq_along(urls),'</a>',collapse=" \u00b7 ")
  paste0('<article data-topic="',esc(z$topic),'" data-kind="',esc(z$measurement_type),'">',
    '<div class="meta">',esc(z$id),' \u00b7 ',esc(z$measurement_type),'</div><h2>',esc(z$title),'</h2><p>',esc(z$subtitle),'</p>',
    '<a href="',z$png,'"><img loading="lazy" src="',z$png,'" alt="',esc(z$title),'"></a><p>',esc(z$interpretation),'</p>',
    '<p class="links"><a href="',z$png,'">PNG</a> \u00b7 <a href="',z$svg,'">SVG</a> \u00b7 <a href="',z$data_csv,'">Plotted data</a> \u00b7 ',links,'</p></article>')
})
options <- paste0('<option>',esc(unique(catalogue$topic)),'</option>',collapse="")
kinds <- paste0('<option>',esc(unique(catalogue$measurement_type)),'</option>',collapse="")
html <- paste0('<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>NZ election economic evidence</title><style>body{margin:0;background:#f4f6f8;color:#183044;font:16px/1.5 system-ui}header,main{max-width:1420px;margin:auto;padding:24px}h1{margin:0}header>p{max-width:1050px}h2{font-size:20px}a{color:#075b91}.filters{display:flex;flex-wrap:wrap;gap:12px;position:sticky;top:0;background:#f4f6f8;padding:15px 0;z-index:1}input,select{padding:8px;font:inherit;max-width:100%}.grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(450px,1fr));gap:24px}article{background:white;padding:20px;border:1px solid #d5dfe5;border-radius:8px}img{width:100%;height:auto}.meta,.links{font-size:13px;color:#526579}[hidden]{display:none!important}@media(max-width:650px){.grid{grid-template-columns:1fr}header,main{padding:14px}}@media print{.filters{display:none}article{break-inside:avoid}.grid{display:block}}</style>',
  '<header><h1>New Zealand election: economic evidence</h1><p>',nrow(catalogue),' new charts covering the six election questions and a dedicated retirement comparison. Snapshot: 2 November 2026. Public observations, official forecasts and model scenarios are labelled.</p>',
  '<p><a href="../ageing/index.html">Ageing and fiscal spending</a> | <a href="../../ELECTION_RESEARCH_GUIDE.md">Research guide and reading paths</a> \u00b7 <a href="../op_ed_topics/index.html">Earlier 70-chart evidence pack</a> \u00b7 <a href="chart_catalogue.csv">Chart catalogue</a> \u00b7 <a href="../../processed/election_2026/source_manifest.csv">Download provenance</a></p>',
  '<p><strong>Start with inflation:</strong> N08 \u2192 N01 \u2192 N03 \u2192 N06 \u2192 N07. <strong>Start with retirement:</strong> S01 \u2192 S05 \u2192 S07 \u2192 S10 \u2192 S11 \u2192 S14 \u2192 S15. The research guide explains what each sequence can establish and what would change the interpretation.</p>',
  '<p>Health year 2024 means the 2024/25 survey. Fiscal 2026 results are unaudited; 2027 onward are forecasts. Household price data reach September 2026; labour data reach June. Retirement illustrations price a CPI-level $30,000 income, not statutory wage-linked NZ Super or a commercial annuity quote.</p>',
  '<div class="filters"><select id="topic"><option value="">All topics</option>',options,'</select><select id="kind"><option value="">All measure types</option>',kinds,
  '</select><input id="search" aria-label="Search charts" placeholder="Search charts and caveats"><span id="count"></span></div></header><main class="grid">',paste(cards,collapse="\n"),
  '</main><script>const cards=[...document.querySelectorAll("article")],topic=document.getElementById("topic"),kind=document.getElementById("kind"),search=document.getElementById("search");function filter(){let n=0;for(const c of cards){c.hidden=!!((topic.value&&topic.value!==c.dataset.topic)||(kind.value&&kind.value!==c.dataset.kind)||(search.value&&!c.textContent.toLowerCase().includes(search.value.toLowerCase())));if(!c.hidden)n++}document.getElementById("count").textContent=n+" charts"}for(const x of[topic,kind,search])x.addEventListener("input",filter);filter();</script></html>')
writeLines(html,file.path(figure_dir,"index.html"),useBytes=TRUE)
message("Saved ",nrow(catalogue)," charts: ",file.path(figure_dir,"index.html"))

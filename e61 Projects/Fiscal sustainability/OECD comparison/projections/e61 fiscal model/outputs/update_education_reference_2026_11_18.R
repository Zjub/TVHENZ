setwd('e61 fiscal model')
replace_text<-function(file,old,new){
 s<-readLines(file,encoding='UTF-8',warn=FALSE)
 if(!any(grepl(old,s,fixed=TRUE)))stop('Expected text missing in ',file,': ',old)
 writeLines(enc2utf8(gsub(old,new,s,fixed=TRUE)),file,useBytes=TRUE)
}
replace_text('R/model_v2.R','education_income_elasticity=.2,','education_income_elasticity=.5,')
replace_text('app.R',"'Education: income elasticity of service intensity',min=0,max=1.5,value=.2,step=.05","'Education: income elasticity of service intensity',min=0,max=1.5,value=.5,step=.05")
replace_text('app.R','The 0.2 default is an unverified assumption','The 0.5 default is an unverified assumption')
replace_text('app.R','the default of 0.2 is an unverified assumption','the default of 0.5 is an unverified assumption')
replace_text('app.R',"updateSliderInput(session,'education_income_elasticity',value=.2)","updateSliderInput(session,'education_income_elasticity',value=.5)")
replace_text('app.R',"min=-1.5,max=2.5,value=.5,step=.01) else","min=-1.5,max=2.5,value=if(n=='Education')0 else .5,step=.01) else")
replace_text('app.R','Education retains 0.5% additional growth, an arbitrary discussion assumption.','Education defaults to zero additional growth beyond demographics, income-related demand and explicit input costs. This is an unverified reference assumption; the income elasticity is 0.5.')
replace_text('R/sector_review.R','The default of 0.2 is an unverified assumption','The default of 0.5 is an unverified assumption')
replace_text('R/sector_review.R','The current 0.5%% additional-growth allowance and the historical residual alternative may overlap with income-related intensity: review them alongside a positive elasticity.','Additional growth defaults to zero beyond demographics, income-related intensity and explicit costs. This is an unverified reference assumption, not evidence that remaining pressures are absent. Positive allowances and the historical residual alternative may overlap with income-related intensity: review them jointly.')
p<-'data/service_cost_parameters.csv';d<-read.csv(p,stringsAsFactors=FALSE,check.names=FALSE)
d$additional_service_growth[grepl('^e_',d$id)]<-0
write.csv(d,p,row.names=FALSE,fileEncoding='UTF-8')
replace_text('README.md','| Education | 0.5% a year | Arbitrary modelling choice; not estimated |','| Education | 0% additional growth; income elasticity 0.5 | Unverified reference assumptions |')
replace_text('README.md','Education retains 25% service efficiency and 0.5% additional annual cost growth. Both are arbitrary discussion assumptions.','Education uses an income elasticity of 0.5, 25% service efficiency and zero additional annual cost growth. These are unverified reference assumptions. Zero adds no trend beyond the explicit demand and cost drivers.')
replace_text('README.md','with a default of 0.2.','with a default of 0.5.')
replace_text('README.md','Reference projections use the same 0.2 elasticity. Both reset controls restore 0.2.','Reference projections use the same 0.5 elasticity and zero additional growth. Both reset controls restore these settings.')
replace_text('README.md','The existing 0.5% annual additional-growth assumption remains a separate arbitrary allowance for pressures beyond the explicit factors. Review it alongside a positive income elasticity: it may already represent income-related quality or intensity improvements.','Additional annual growth defaults to zero, an unverified reference choice that adds no residual trend beyond the explicit demographic, income and input-cost factors. The control remains available for additional pressures. A positive allowance may overlap with income-related quality or intensity improvements and should be assessed jointly with the elasticity.')
p<-'data/assumption_register.csv';d<-read.csv(p,stringsAsFactors=FALSE,check.names=FALSE)
k<-d$item=='Education income elasticity of service intensity'
d$value_rule[k]<-'Default 0.5; all education subfunctions multiply by the real-GDP-per-person index raised to this coefficient'
d$limitation[k]<-'Unverified additional service-intensity response beyond wages and efficiency; not the total spending income elasticity. Historical residuals may overlap and require joint review.'
item<-'Education additional spending growth';d<-d[d$item!=item,]
row<-as.data.frame(as.list(setNames(c('Spending',item,'Unverified assumption','Default 0% annually for schools, higher education, VET and other education','e61 reference scenario choice','','No residual trend beyond demographics, income-related intensity and explicit input costs; both resets restore zero','Zero does not establish that future pressures are absent. Optional historical residuals were fitted without the education income factor and are not refitted by the sliders.'),names(d))),stringsAsFactors=FALSE)
write.csv(rbind(d,row),p,row.names=FALSE,fileEncoding='UTF-8')
replace_text('scripts/build_report_methods_box.R','a$education_income_elasticity==.2','a$education_income_elasticity==.5')
replace_text('scripts/build_report_methods_box.R','13 November 2026','18 November 2026')
replace_text('scripts/build_report_methods_box.R','0.8 for health and 0.2 for education','0.8 for health and 0.5 for education')
replace_text('scripts/build_report_methods_box.R','education 0.2 is an unverified assumption','education 0.5 is an unverified assumption')
replace_text('scripts/build_report_methods_box.R','education retains 0.5%% additional annual growth','education also defaults to zero additional annual growth')
replace_text('scripts/build_report_methods_box.R','Education 0.2, efficiency at 25% and additional-growth allowances','Education 0.5, efficiency at 25% and remaining-growth assumptions')
replace_text('estimation/05_health_framework_report.R','Education retains 25% service efficiency and 0.5% additional cost growth as arbitrary discussion assumptions, plus its 2016-2025 historical residual alternative.','Education uses income elasticity 0.5, 25% service efficiency and zero additional cost growth as unverified reference assumptions, plus its 2016-2025 historical residual alternative. That historical residual was fitted without education income demand and may overlap with a positive elasticity.')
cat('Updated Education reference, UI, reset controls, evidence register and documentation sources.\n')

setwd('e61 fiscal model')
replace_text<-function(file,old,new){
 s<-readLines(file,encoding='UTF-8',warn=FALSE)
 stopifnot(any(grepl(old,s,fixed=TRUE)))
 s<-gsub(old,new,s,fixed=TRUE)
 writeLines(enc2utf8(s),file,useBytes=TRUE)
}
replace_text('R/model_v2.R','education_income_elasticity=0,','education_income_elasticity=.2,')
replace_text('app.R',"'Education: income elasticity of service intensity',min=0,max=1.5,value=0,step=.05","'Education: income elasticity of service intensity',min=0,max=1.5,value=.2,step=.05")
replace_text('app.R','Zero is an unestimated discussion default, not evidence of no income response.','The 0.2 default is an unverified assumption, not an estimated Australian parameter.')
replace_text('app.R',"updateSliderInput(session,'education_income_elasticity',value=0)","updateSliderInput(session,'education_income_elasticity',value=.2)")
replace_text('app.R','zero is an arbitrary discussion default, not an Australian estimate.','the default of 0.2 is an unverified assumption, not an Australian estimate.')
replace_text('R/sector_review.R','The zero default is an arbitrary discussion setting, not an estimated Australian elasticity.','The default of 0.2 is an unverified assumption, not an estimated Australian elasticity.')
replace_text('README.md','with an arbitrary discussion default of zero. It is not an estimated Australian parameter.','with a default of 0.2. This is an unverified assumption, not an estimated Australian parameter.')
replace_text('README.md','Reference projections retain the zero elasticity. Reset restores zero.','Reference projections use the same 0.2 elasticity. Both reset controls restore 0.2.')
replace_text('tests/browser_education_income.R',"value==='0'","value==='0.2'")
replace_text('tests/browser_education_income.R','all(base$income_factor==1)','all(base$income_elasticity==.2)')
replace_text('tests/browser_education_income.R','changed$spending_bn/base$spending_bn-changed$income_factor','changed$spending_bn/base$spending_bn-changed$income_factor/base$income_factor')
p<-'data/assumption_register.csv';d<-read.csv(p,stringsAsFactors=FALSE,check.names=FALSE)
item<-'Education income elasticity of service intensity'
d<-d[d$item!=item,]
row<-as.data.frame(as.list(setNames(c('Education',item,'Unverified assumption','Default 0.2; all education subfunctions multiply by the real-GDP-per-person index raised to this coefficient','e61 scenario assumption; not an empirical Australian estimate','','Additional provision per eligible person beyond explicit wage costs and efficiency; same reference and reset default','May overlap with the 0.5% additional-growth allowance or historical residuals; review jointly. Not the total spending income elasticity.'),names(d))),stringsAsFactors=FALSE)
write.csv(rbind(d,row),p,row.names=FALSE,fileEncoding='UTF-8')

# OBR-inspired decomposition; not a replication of OBR/OECD coefficients.
health_pressure_base <- function(x,a) {
 ids<-x$service_cost_parameters$id[grepl('^h_',x$service_cost_parameters$id)]
 if(a$health_pressure_source=='assumed')return(setNames(rep(0,length(ids)),ids))
 d<-x$health_income_residuals[x$health_income_residuals$source==a$health_pressure_source,]
 stopifnot(setequal(d$id,ids),!anyDuplicated(d$id))
 setNames(d$residual_growth,d$id)
}
health_pressure_centre <- function(x,a,cal=NULL) {
 r<-health_pressure_base(x,a);s<-if(is.null(cal))x$health_pressure_weights else subset(cal$subfunctions,programme=='Health')
 round(100*weighted.mean(r[s$id],s$anchor_2030_bn),1)/100
}
health_cost_factors <- function(z,m,a) {
 t<-m$year-a$anchor_year
 inflation<-c(0,m$domestic_price_index[-1]/head(m$domestic_price_index,-1)-1)
 gross<-1+inflation+a$health_cpi_wedge;gross[1]<-1
 if(any(!is.finite(gross)|gross<=0))stop('Health CPI path requires positive gross price factors')
 P<-cumprod(gross)
 real_gdp_pc<-m$real_output_index/m$population_index
 I<-real_gdp_pc^a$health_income_elasticity
 theta<-z$service_productivity_share+a$health_service_productivity_share
 W<-m$nominal_hourly_wage_index/P
 premium<-(1+a$health_wage_premium)^t
 Q<-(1+theta*a$productivity)^t
 L<-z$wage_weight
 if(z$cost_rule=='cpi_proxy'){
   wage<-efficiency<-rep(1,length(t));applied_theta<-0
 } else {
   wage<-(1-L)+L*W*premium
   efficiency<-((1-L)+L*W*premium/Q)/wage;applied_theta<-theta
 }
 data.frame(wage_factor=wage,efficiency_factor=efficiency,real_unit_factor=wage*efficiency,
   intensity_factor=(1+z$additional_service_growth)^t,
   configured_service_productivity_share=theta,service_productivity_share=applied_theta,
   service_productivity_growth=applied_theta*a$productivity,
   additional_service_growth=z$additional_service_growth,wage_weight=L,cost_rule=z$cost_rule,
   income_factor=I,health_price_index=P,real_gdp_per_capita_index=real_gdp_pc,
   health_real_wage_cpi_index=W,health_wage_premium_index=premium,health_productivity_index=if(applied_theta==0)1 else Q,
   income_elasticity=a$health_income_elasticity)
}

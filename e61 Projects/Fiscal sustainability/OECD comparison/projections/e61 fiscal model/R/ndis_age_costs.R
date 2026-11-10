# Add an age-cost shape to the existing participant projection, without changing
# participant counts or the 2025-2030 fiscal allocation bridge.
ndis_cost_summary <- function(counts,pop,x,years) {
  w<-x$ndis_age_costs;ages<-0:100
  stopifnot(!anyDuplicated(w$age_min),all(w$relative_cost_weight>0),
    identical(w$age_min,c(0L,9L,15L,19L,25L,35L,45L,55L,65L)),
    all(w$age_max==c(8,14,18,24,34,44,54,64,100)))
  k<-findInterval(ages,w$age_min)
  costs<-w$annual_cost_proxy[k];weights<-w$relative_cost_weight[k]
  total<-colSums(counts);weighted<-colSums(counts*weights)
  average<-colSums(counts*costs)/total
  z<-data.frame(year=years,participants=total,participants_65plus=colSums(counts[66:101,,drop=FALSE]),
    cost_weighted_participants=weighted,average_age_cost_proxy=average,
    age_cost_mix_index=average/average[match(2030,years)])
  detail<-do.call(rbind,lapply(seq_along(years),function(i)data.frame(year=years[i],age=ages,
    age_group=w$age_group[k],population=pop$population[match(paste(years[i],ages),paste(pop$year,pop$age))],
    participants=counts[,i],relative_cost_weight=weights,annual_cost_proxy=costs,
    cost_weighted_participants=counts[,i]*weights)))
  attr(z,'age_profiles')<-detail;z
}

ndis_age_projection_data <- function(inputs,assumptions=default_assumptions()) {
  x<-economic_inputs(inputs,assumptions);m<-macro_path(x,assumptions,prepared=TRUE)
  d<-attr(x$ndis,'age_profiles');d<-d[d$year%in%m$year,]
  n<-aggregate(cbind(participants,cost_weighted_participants)~year,d,sum)
  d$participant_share<-d$participants/n$participants[match(d$year,n$year)]
  d$cost_proxy_share<-d$cost_weighted_participants/n$cost_weighted_participants[match(d$year,n$year)]
  d
}

# Flags that need to be added
# Eligibility flag will need to be added for SIH integration. So just add with a "1" for now.

ben_eligibility <- 1
rent_max_single_la <- 337.84
rent_min_single_la <- 135.40
rent_max_single_share <- 271.60
rent_min_single_share <- 135.40
rent_max_couple <- 409.6 # Note this is a shard payment. So this will currently double count when both people are eligible for the benefit.
rent_min_couple <- 219.2
rent_max_single_dep1 <- 415.62
rent_min_single_dep1 <- 177.80
rent_max_couple_dep1 <- 500.60 # Note this is a shard payment. So this will currently double count when both people are eligible for the benefit.
rent_min_couple_dep1 <- 262.78
rent_max_single_dep3 <- 446.23
rent_min_single_dep3 <- 177.80
rent_max_couple_dep3 <- 531.21 # Note this is a shard payment. So this will currently double count when both people are eligible for the benefit.
rent_min_couple_dep3 <- 262.78
ra_rate <- 0.75


# Calculator for rent assistance

calc_rent_assistance <- function(Rent,partnered,Numb_dep,living_alone,ben_eligibility){
  rent_assistance = 0
  if (ben_eligibility == 1){
    if (Numb_dep == 0){
      rent_assistance = ifelse(living_alone == 1,max((min(rent_max_single_la,Rent)-rent_min_single_la)*ra_rate,0),ifelse(partnered == 0,max((min(rent_max_single_share,Rent)-rent_min_single_share)*ra_rate,0),max((min(rent_max_couple,Rent)-rent_min_couple)*ra_rate,0)))
    }
    else if (Numb_dep < 3) {
      rent_assistance = ifelse(partnered == 0,max((min(rent_max_single_dep1,Rent)-rent_min_single_dep1)*ra_rate,0),max((min(rent_max_couple_dep1,Rent)-rent_min_couple_dep1)*ra_rate,0))
    }
    else {
      rent_assistance = ifelse(partnered == 0,max((min(rent_max_single_dep3,Rent)-rent_min_single_dep3)*ra_rate,0),max((min(rent_max_couple_dep3,Rent)-rent_min_couple_dep3)*ra_rate,0))
    }
  }
  return(rent_assistance)
}

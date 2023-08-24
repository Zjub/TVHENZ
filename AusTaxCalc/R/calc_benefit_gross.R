# Calculation of the gross benefit payment

calc_benefit_gross <- function(){
  gross_benefit = JSP_pay + PP_pay + calc_rent_assistance(Rent,partnered,Numb_dep,living_alone) + calc_energy_supp(partnered,Have_dep,over_60)
  taxable_benefit = JSP_pay + PP_pay
  supp_benefit = gross_benefit - taxable_benefit
  return(list(gross_benefit = gross_benefit,taxable_benefit = taxable_benefit,supp_benefit = supp_benefit))
}

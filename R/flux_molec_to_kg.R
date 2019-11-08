
# need to add in all NPL30 gases to this
flux_molec_to_kg <- function(flux, gas){
  
  mw_gas <- case_when(
    gas %in% c("ch4", "methane") ~ 16.01,
    gas %in% c("c2h6", "ethane") ~ 30.07,
    gas %in% c("c3h8", "propane") ~ 44.1
  )
  
  flux_kg = flux/avogadro()*mw_gas*0.001
  
  return(flux_kg)
  
}



# create table of constants
constants <- tribble(
  ~name, ~symbol, ~value, ~unit,
  "specific gas constant for dry air", "R", 287.04, "m^2/K*s^2",
  "gravitational constant", "g", 9.81, "m^2/s",
  "air number density at STP", "num_den", 2.49e19, "molec/cm^3",
  "molar mass CH4", "RMM_ch4", 16, "g/mol"
  
)



gravitational_constant <- function() 9.81

specific_gas_constant <- function() 287.04

air_number_density <- function() 2.49e19

molar_mass_ch4 <- function() 16.01

molar_mass_ethane <- function() 30.07

molar_mass_co2 <- function() 44.01

avogadro <- function() 6.03e23

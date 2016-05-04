# Calculate the magnetic field (Gauss) for a given g-factor and microwave 
# frequency (GHz)
gval_to_field <- function(g, v) {
  
  planck_const <- 6.62606957e-34 # SI: J/s 
  bohr_magneton <- 9.27400968e-24 # SI: J/T
  
  H <- (planck_const * v * 10^9) / (bohr_magneton * 10^-4 * g)
  
  return(H)
}

# Calculate the microwave frequency (GHz) for a given g-factor and magnetic
# field (Gauss)
gval_to_freq <- function(g, H) {
  
  planck_const <- 6.62606957e-34 # SI: J/s 
  bohr_magneton <- 9.27400968e-24 # SI: J/T
  
  v <- (bohr_magneton * g * H) / (10^13 * planck_const)
  
  return(v)
}
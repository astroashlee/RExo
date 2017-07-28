# Calculate the equilibrium temperature!
#
# This function calculates the equilibrium temperature
# of a planet at a given distance away from from a star
# of a certain size, applying the definition:
#
# Tp^4 = Tstar^4 * (1 - A) * (Rstar / 2a)^2
#
# inputs:
#   tstar = stellar temp in kelvin
#   rstar = stellar radii in solar radii
#   dplanet = planet semi-major axis in AU
# keywords:
#   alb = bond albedo
#       = 0 if not set
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

calc_teq <- function(tstar,rstar,dplanet,alb=NA){

  rsolar <- 6.957e10   # solar radius in cm
  au <- 1.496e13       # AU in cm

  # convert rstar, dplanet to cm
  rstar_cm <- rstar * rsolar # rsolar --> cm
  dplanet_cm <- dplanet * au # AU --> cm

  # if no albedo is supplied, then just set = 0.0
  if (is.na(alb)) {
    alb <- 0.0
  }

  # calculate teq^4
  teq_planet4th <- (tstar)^4.0 * (1.0 - alb) * (rstar_cm / (2.0 * dplanet_cm))^2.0

  # calculate teq
  teq_planet <- teq_planet4th^0.25

  # return the planet equilibrium temp
  return(teq_planet)

}

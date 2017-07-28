# Calculate the scale height of an atmosphere!
#
#
# This function calculates the scale height (H0)
# of an atmosphere at a given distance away from from a star
# of a certain size, applying the definition:
#
#   H0 = kT / (mu * mH * g)
#
# and del_transit
#   del = 2 Rplanet * H0 / Rstar^2
#
# which relies on the assumptions:
#   1. hydrostatic equilibrium
#   2. isothermal T/P structure
#   3. constant mmw with height
# which are nontrivial! But are okay for hot Jupiter atmospheres.
#
#
# (required) inputs:
#   tplanet =   planetary temp in K
#   rstar   =   stellar radii in solar radii
#   rplanet =   planetary radius (in jupiter radii)
#
# (optional) keywords:
#   mmw     =   mean molecular weight (mu, in amus)
#               if not included, will be set to 2.2
#   logg    =   planetary logg (in cgs units)
#               if not included, will calculate g
#   mplanet =   planetary mass (in jupiter masses)
#               must be included if not providing logg

#
# output:
#
#

calc_scaleh <- function(tplanet,rplanet,rstar,mplanet=NA,logg=NA,mmw=NA){


  massH <- 1.6737236e-24 # mass of hydrogen (grams)
  rsolar <- 6.957e10 # solar radius in cm
  rjup <- 6.9911e9 # rjup in cm
  kb <- 1.38064852e-16  # boltzmann k (cgs)
  grav_g <- 6.674e-8 # gravitational g (cgs)
  mjupiter_g <- 1.89813e30 # mass of jupiter (grams)

  # convert planet radius from jupiter radii to cm
  rplanet_cm <- rplanet * rjup

  # if logg not provided, calculate in cgs
  # if it is provided, convert logg to cgs
  if (is.na(logg)) {

    # convert planet mass to g
    mplanet_g <- mplanet*mjupiter_g

    grav <- grav_g * mplanet_g / (rplanet_cm^2)

  } else {

    grav <- 10.0^logg

  }

  # if no mmw is supplied, then just set = 2.2 (hydrogen-dominated)
  if (is.na(mmw)) {
    mmw <- 2.2
  }

  # convert rstar, dplanet to cm
  rstar_cm <- rstar * rsolar # rsolar --> cm

  # calculate absolute scale height (in cm)
  scaleh_cm <- kb * tplanet / (mmw * massH * grav)

  scaleh_depth <- 2.0 * rplanet_cm * scaleh_cm / (rstar_cm^2.0)

  scaleh <- c(scaleh_cm,scaleh_depth)

  # return the planet equilibrium temp
  return(scaleh)

}

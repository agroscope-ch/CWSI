#' This function calculates the net radiation (Rn) based on incoming solar radiation,
#' air temperature, canopy temperature, and various physical constants.
#' @param Rs Incoming solar radiation (W/m^2)
#' @param Ta Air temperature (°C)
#' @param Tc Canopy temperature (°C)
#' @param alpha Albedo of the surface
#' @param epsilon_a Emissivity of the air
#' @param epsilon_c Emissivity of the canopy
#' @param sigma Stefan-Boltzmann constant
#' @return Net radiation (Rn) in W/m^2
#' @examples
#' Rn_value <- Rn(Rs = 500, Ta = 25, Tc = 30)
#' @noRd

calc_Rn <- function(Rs, Ta, Tc, alpha, epsilon_a, epsilon_c, sigma) {

  Rn <- ((1 - alpha) * Rs) +
         (epsilon_a * sigma * (Ta + 273.15)^4) -
         (epsilon_c * sigma * (Tc + 273.15)^4)

  return(Rn)
}


#' This function calculates the aerodynamic resistance (ra) based on measurement height,
#' displacement height, roughness length, and wind speed.
#' @param h Crop height (m)
#' @param z Measurement height (m)
#' @param u Wind speed (m/s)
#' @param d Displacement height (m)
#' @param z0 Roughness length (m)
#' @return Aerodynamic resistance (ra) in s/m
#' @examples
#' ra_value <- ra(z = 2, d = 0.67, z0 = 0.123, u = 3)
#' @noRd

calc_ra <- function(h, z, u, d, z0) {

  ra <- (4.72 * (log(z - d) / z0)^2) / (1 + 0.54 * u)

  return(ra)
}


#' This function calculates the upper baseline for the temperature difference
#' between canopy and air (Tc - Ta) based on aerodynamic resistance, net radiation,
#' soil heat flux, air density, and specific heat capacity of air.
#' @param ra Aerodynamic resistance (s/m)
#' @param Rn Net radiation (W/m^2)
#' @param G Soil heat flux (W/m^2)
#' @param rho Air density (kg/m^3)
#' @param Cp Specific heat capacity of air (J/kgK)
#' @return Returns the upper baseline for the temperature difference (Tc - Ta) in °C
#' @examples
#' TcTa_U_value <- TcTa_U(ra = 2, Rn = 400)
#' @noRd

calc_TcTa_U <- function(ra, Rn, G, rho, Cp) {

  TcTa_U <- (ra * (Rn - G)) / (rho * Cp)

  return(TcTa_U)
}

#' This function calculates the slope of the saturation vapor pressure-temperature
#' curve (delta) based on air temperature and canopy temperature.
#' @param Ta Air temperature (°C)
#' @param Tc Canopy temperature (°C)
#' @return Slope of the saturation vapor pressure-temperature curve (delta) in Pa/°C
#' @noRd
calc_delta <- function(Ta, Tc) {

  # Calculate saturation vapor pressures at Ta and Tc using Tetens formula
  e_sat_a <- 0.61078 * exp((17.27 * Ta) / (Ta + 237.3)) * 1000  # Pa
  e_sat_c <- 0.61078 * exp((17.27 * Tc) / (Tc + 237.3)) * 1000  # Pa

  delta <- (e_sat_c - e_sat_a) / (Tc - Ta)

  return(delta)
}

#' This function calculates the lower baseline for the temperature difference
#' between canopy and air (Tc - Ta) based on aerodynamic resistance, net radiation,
#' soil heat flux, air density, specific heat capacity of air, slope of the
#' saturation vapor pressure-temperature curve, psychrometric constant, and vapor pressure deficit.
#' @param ra Aerodynamic resistance (s/m)
#' @param Rn Net radiation (W/m^2)
#' @param G Soil heat flux (W/m^2)
#' @param rho Air density (kg/m^3)
#' @param Cp Specific heat capacity of air (J/kgK)
#' @param delta Slope of the saturation vapor pressure-temperature curve (Pa/°C)
#' @param gamma Psychrometric constant (Pa/°C)
#' @return Returns the lower baseline for the temperature difference (Tc - Ta) in °C
#' @examples
#' TcTa_L_value <- TcTa_L(ra = 2, Rn = 400, delta = 250)
#' @noRd

calc_TcTa_L <- function(ra,
                        Rn,
                        G,
                        rho,
                        Cp,
                        VPD,
                        delta,
                        gamma) {

  TcTa_L <- (((ra * (Rn - G)) / (rho * Cp)) *
           (gamma / (delta + gamma))) -
           (VPD / (delta + gamma))

  return(TcTa_L)
}

#' This function calculates the vapor pressure deficit (VPD) based on relative humidity and air temperature.

calc_VPD <- function(rH, Ta) {
  e_sat_a <- ifelse(Ta > 0,
    0.61078 * exp((17.27 * Ta) / (Ta + 237.3)) * 1000,  # Pa
    0.61078 * exp((21.875 * Ta) / (Ta + 265.5)) * 1000)  # Pa
  VPD <- e_sat_a * (1 - rH / 100)
  return(VPD)
}

# ' This function calculates the Crop Water Stress Index (CWSI) based on various parameters.
#' @param Rs Incoming solar radiation (W/m^2)
#' @param Ta Air temperature (°C)
#' @param Tc Canopy temperature (°C)
#' @param h Crop height (m)
#' @param z Measurement height (m)
#' @param u Wind speed (m/s)
#' @param rH Relative humidity (%)
#' @param alpha Albedo of the surface (default = 0.23)
#' @param epsilon_a Emissivity of the air (default = 0.85)
#' @param epsilon_c Emissivity of the canopy (default = 0.98)
#' @param sigma Stefan-Boltzmann constant (default = 5.67e-8 W/m^2K^4)
#' @param d Displacement height (default = 0.63 * h)
#' @param z0 Roughness length (default = 0.13 * h)
#' @param G_fraction Fraction of net radiation used as soil heat flux (default = 0.1)
#' @param rho Air density (default = 1.225 kg/m^3)
#' @param Cp Specific heat capacity of air (default = 1005 J/kgK)
#' @param gamma Psychrometric constant (default = 66 Pa/°C)
#' @return Crop Water Stress Index (CWSI)
#' @examples
#' CWSI_value <- calc_CWSI_V2(Rs = 500, Ta = 25, Tc = 30, h = 1, z = 2, u = 3, rH = 60)
#' @export
calc_CWSI_theoretical <- function(Rs,
                                  Ta,
                                  Tc,
                                  h,
                                  z,
                                  u,
                                  rH,
                                  alpha = 0.23,
                                  epsilon_a = 0.85,
                                  epsilon_c = 0.98,
                                  sigma = 5.67e-8,
                                  d = 0.63 * h,
                                  z0 = 0.13 * h,
                                  G_fraction = 0.1,
                                  rho = 1.225,
                                  Cp = 1005,
                                  gamma = 66) {

  VPD <- calc_VPD(rH = rH, Ta = Ta)

  Rn <- calc_Rn(Rs, Ta, Tc, alpha, epsilon_a, epsilon_c, sigma)

  ra <- calc_ra(h, z, u, d, z0)

  TcTa_U <- calc_TcTa_U(ra, Rn, G = G_fraction * Rn, rho, Cp)

  delta <- calc_delta(Ta, Tc)

  TcTa_L <- calc_TcTa_L(ra, Rn, G = G_fraction * Rn, rho, Cp, VPD, delta, gamma)

  CWSI <- ((Tc - Ta) - TcTa_L) / (TcTa_U - TcTa_L)

  return(CWSI)

}



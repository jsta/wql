#' Convert conductivity to salinity
#' 
#' Electrical conductivity data are converted to salinity using the Practical
#' Salinity Scale and an extension for salinities below 2.
#' 
#' \code{ec2pss} converts electrical conductivity data to salinity using the
#' Practical Salinity Scale 1978 in the range of 2-42 (Fofonoff and Millard
#' 1983). Salinities below 2 are calculated using the extension of the
#' Practical Salinity Scale (Hill et al. 1986).
#' 
#' \code{R2pss} is the same function, except that conductivity ratios rather
#' than conductivities are used as input.
#' 
#' @param ec conductivity, mS/cm
#' @param t temperature, Celsius
#' @param p gauge pressure, decibar
#' @return \code{ec2pss} and \code{R2pss} both return salinity values on the
#' Practical Salinity Scale.
#' @note Input pressures are not absolute pressures but rather gauge pressures.
#' Gauge pressures are measured relative to 1 standard atmosphere, so the gauge
#' pressure at the surface is 0.
#' @references Fofonoff N.P. and Millard Jr R.C. (1983) \emph{Algorithms for
#' Computation of Fundamental Properties of Seawater.} UNESCO Technical Papers
#' in Marine Science 44. UNESCO, Paris, 53 p.
#' 
#' Hill K.D., Dauphinee T.M. and Woods D.J. (1986) The extension of the
#' Practical Salinity Scale 1978 to low salinities. \emph{IEEE Journal of
#' Oceanic Engineering} \bold{11,} 109-112.
#' @keywords manip utilities
#' @examples
#' 
#' # Check values from Fofonoff and Millard (1983):
#' R = c(1, 1.2, 0.65) 
#' t = c(15, 20, 5)
#' p = c(0, 2000, 1500)
#' R2pss(R, t, p)  # 35.000 37.246 27.995
#' # Repeat calculation with equivalent conductivity values by setting 
#' # ec <- R * C(35, 15, 0):
#' ec = c(1, 1.2, 0.65) * 42.9140
#' ec2pss(ec, t, p)  # same results
#' 
#' @export ec2pss
ec2pss <-
function (ec, t, p = 0) {
	# Define conductivity ratio
  R <- ec/42.914
    
  # Estimate temperature correction (valid for -2 < t < 35)
  c <- c(0.6766097, 0.0200564, 0.0001104259, -6.9698e-07, 1.0031e-09)
  rt <- c[1] + c[2] * t + c[3] * t^2 + c[4] * t^3 + c[5] * t^4
    
	# Estimate pressure correction (validity range varies with t and S)
  d <- c(0.03426, 0.0004464, 0.4215, -0.003107)
  e <- c(2.07e-05, -6.37e-10, 3.989e-15)
  Rp <- 1 + p * (e[1] + e[2] * p + e[3] * p^2)/(1 + d[1] * t + 
    d[2] * t^2 + (d[3] + d[4] * t) * R)
       
	# Estimate salinity (valid for 2 < S < 42 and -2 < t < 35).       
  Rt <- R/(Rp * rt)
  a <- c(0.008, -0.1692, 25.3851, 14.0941, -7.0261, 2.7081)
  b <- c(5e-04, -0.0056, -0.0066, -0.0375, 0.0636, -0.0144)
  ft <- (t - 15)/(1 + 0.0162 * (t - 15))
  S <- a[1] + a[2] * Rt^0.5 + a[3] * Rt + a[4] * Rt^1.5 + a[5] * 
    Rt^2 + a[6] * Rt^2.5 + ft * (b[1] + b[2] * Rt^0.5 + b[3] * 
    Rt + b[4] * Rt^1.5 + b[5] * Rt^2 + b[6] * Rt^2.5)
        
	# Estimate salinity correction for S < 2
  x <- 400 * Rt
  y <- 100 * Rt
  ifelse(S >= 2, S, S - a[1]/(1 + 1.5 * x + x^2) - b[1] * ft/(1 + 
    y^0.5 + y + y^1.5))
}

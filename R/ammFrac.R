ammFrac <-
function(pH, t, S, pHscale = c('total', 'free')) {

### Estimates fraction of total ammonium in un-ionized form (Clegg and
### Whitfield 1995). Valid for -2 to 40¡C, 0 to 40 salinity.
### Inputs:
###     pH, pH measurement
###     t, temperature (deg C)
###     S, salinity (PSS)
###     pHscale, scale on which pH is measured
### Output: un-ionized fraction
	
	## Check input.
	pHscale = match.arg(pHscale)
	
	## Empirical constants
	a <- c(0.0500616, -9.412696, -2.029559e-7, -0.0142372, 1.46041e-5,
		3.730005, 7.1047e-5, -0.0229021, -5.521278e-7, 1.95413e-4)
	b <- c(0.04203362, -11.24742, -13.64160, 1.176949, -0.02860785,
		545.4834, -0.1462507, 0.009022648, -1.471361e-4, 10.54250,
		0.004669309, -1.691742e-4, -0.5677934, -2.354039e-5, 0.009698623)
	
	## Calculate dissociation constant, depending on pH scale
	T = t + 273.15
	pKaT = 9.244605 - 2729.33*(1/298.15 - 1/T)	
	if (identical(pHscale, 'free')) {
		pKaStar = pKaT + (a[1] + a[2]/T + a[3]*T^2)*S^.5 + (a[4] + a[5]*T +
			a[6]/T)*S + (a[7] + a[8]/T)*S^2 + (a[9] + a[10]/T)* S^3
	} else {
		pKaStar = pKaT + (b[1] + b[2]/T)*S^.25 + (b[3] + b[4]*T^.5 + b[5]*T
			+ b[6]/T)*S^.5 + (b[7] + b[8]*T^.5 + b[9]*T +b[10]/T)*S^1.5 +
			(b[11] + b[12]*T^.5 + b[13]/T)*S^2 + (b[14] + b[15]/T)*S^2.5
	}

	## Calculate ammonium fraction.
	H = 10^(-pH)
	K = 10^(-pKaStar)
	1/(1+H/K)
}
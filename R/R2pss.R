R2pss <-
function (R, t, p = 0) 
{
	# adj 10/20/09 11:44 AM
	# Conductivity ratios are converted to salinity on the Practical Salinity Scale 1978 in the range of 2-42 (Fofonoff and Millard 1983). Salinities below 2 are calculated using the extension of the Practical Salinity Scale (Hill et al. 1986).
	# Args:
		# R: conductivity ratio, dimensionless
		# t: temperature, deg C
		# p: gauge pressure, decibar above standard atmosphere (p = 0 at surface)
	# Returns: salinity, PSS
	
	# See ec2pss for further explanation.
	ec2pss(42.914 * R, t = t, p = p)
}

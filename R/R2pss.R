R2pss <-
function (R, t, p = 0) 
{
### adj 10/20/09 11:44 AM
### Conductivity ratios are converted to salinity. See ec2pss.
###   R: conductivity ratio, dimensionless
###   t: temperature, deg C
###   p: gauge pressure, decibar above standard atmosphere
### Returns: salinity, PSS
	
	ec2pss(42.914 * R, t = t, p = p)
}

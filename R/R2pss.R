#' R2pss
#'
#' @param t temperature, Celsius
#' @param p gauge pressure, decibar
#' @param R conductivity ratio, dimensionless
#' @author 
#' Alan Jassby, James Cloern
#'
#' @export
R2pss <-
function (R, t, p = 0) 
{
	ec2pss(42.914 * R, t = t, p = p)
}

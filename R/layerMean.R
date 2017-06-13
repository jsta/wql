#' layerMean
#' @importFrom stats na.omit
#' @description Acts on a matrix or data frame with depth in the first
#' column and observations for different variables (or different sites, or
#' different times) in each of the remaining columns. The trapezoidal mean over
#' the given depths is calculated for each of the variables. Replicate depths
#' are averaged, and missing values or data with only one unique depth are
#' handled. Data are not extrapolated to cover missing values at the top or
#' bottom of the layer. The result can differ markedly from the simple mean
#' even for equal spacing of depths, because the top and bottom values are
#' weighted by 0.5 in a trapezoidal mean.
#' @param d data.frame
#' @author 
#' Alan Jassby, James Cloern
#' @export
layerMean <-
function(d) {
    
  # Trapezoidal mean of scalar x versus z
  trapMean <- function(z, x) {
    # Handle NAs
    w <- na.omit(cbind(z, x))
    n <- nrow(w)
    if (identical(n, 0L))
      return(NA)
    z <- w[, 1]
    x <- w[, -1]
    z1 <- diff(z)
    x1 <- 0.5 * (x[-1] + x[-n])
    sum(z1 * x1)/(z[n] - z[1])
  }
  
  # Trapezoidal mean of vector d[, -1] vs d[, 1]
  # Handle single observations
  n <- nrow(d)
  if (is.null(n))
    return(d[-1])
  if (identical(n, 1L))
    return(as.numeric(d[, -1]))
  # Handle duplicates
  d <- aggregate(d[, -1], by = list(z = d[, 1]), mean, na.rm = TRUE)

  apply(d[, -1, drop = FALSE], 2, function(x) trapMean(z = d[, 1], x))
}

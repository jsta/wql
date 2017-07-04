#' Converts matrix to vector time series for various analyses
#' 
#' First aggregates multivariate matrix time series by year. Then converts to a
#' vector time series in which \dQuote{seasons} correspond to these annualized
#' values for the original variables.
#' 
#' The \code{seas} parameter enables focusing the subsequent analysis on
#' seasons of special interest, or to ignore seasons where there are too many
#' missing data. The function can be used in conjunction with \code{seaKen} to
#' conduct a Regional Kendall trend analysis. Sometimes just plotting the
#' resulting function can be useful for exploring a spatial transect over time.
#' @author 
#' Alan Jassby, James Cloern
#' @param x An object of class "mts"
#' @param seas Numeric vector of seasons to aggregate in original time series.
#' @param na.rm Should missing data be ignored when aggregating?
#' @export
#' @return A vector time series
#' @seealso \code{\link{seaKen}}
#' @keywords ts manip
#' @examples
#' 
#' ## Quick plot a spatial transect of chlorophyll a during the 
#' ## spring bloom period (Feb-Apr) for each year.
#' y <- mts2ts(sfbayChla, seas = 2:4)
#' plot(y, type = 'n')
#' abline(v = 1978:2010, col = 'lightgrey')
#' lines(y, type = 'h')
#' 
mts2ts <- 
function(x, seas = 1:frequency(x), na.rm = FALSE) {
  if (!is.mts(x)) 
    stop("x must be of class 'mts'")
  st <- start(x)[1]
  x1 <- window(x, start = st, end = c(end(x)[1], frequency(x)), 
               extend = TRUE)
  x1 <- aggregate(x1, 1, meanSub, sub = seas, na.rm = na.rm)
  x1 <- as.numeric(t(x1))
  ts(x1, start = st, frequency = ncol(x))
} 

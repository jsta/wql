#' Rolling Seasonal Kendall trend test
#' 
#' Calculates the Seasonal Kendall test of significance, including an estimate
#' of the Sen slope, for rolling windows over a time series.
#' 
#' The function \code{seaRoll} applies \code{seaKen} to rolling time windows of
#' width \code{w}. A minimum \code{w} of five years is required. For any
#' window, a season is considered missing if half or more of the possible
#' comparisons between the first and last 20\% of the years is missing. If
#' \code{mval} or more of the seasons are missing, then that windowed trend is
#' considered to be missing.
#' 
#' If \code{plot = TRUE}, a point plot will be drawn with the Sen slope plotted
#' at the leading year of the trend window. The plot symbols indicate,
#' respectively, that the trend is significant or not significant. The plot can
#' be customized by passing any arguments used by \code{\link{plot.default}},
#' as well as graphical parameters described in \code{\link{par}}.
#' 
#' @param x A seasonal time series vector.
#' @param w The window width for \dQuote{rolling} estimates of slope.
#' @param plot Indicates if a plot should be drawn
#' @param pval p-value for significance
#' @param mval Minimum fraction of seasons needed with non-missing slope
#' estimates
#' @param pchs Plot symbols for significant and not significant trend
#' estimates, respectively
#' @param xlab Optional label for x-axis
#' @param ylab Optional label for y-axis
#' @param ...  Other arguments to pass to plotting function
#' @author 
#' Alan Jassby, James Cloern
#' @return \code{seaRoll} returns a matrix with one row per time window
#' containing the Sen slope, the relative Sen slope, and the \emph{p-}value.
#' Rows are labelled with the leading year of the window.
#' @seealso \code{\link{seaKen}}
#' @keywords ts
#' @export
#' @examples
#' 
#' chl27 <- sfbayChla[, 's27']
#' seaRoll(chl27)
#' seaRoll(chl27, plot = TRUE)
#' 
seaRoll <- 
function(x, w = 10, plot = FALSE, pval = .05, mval = .5, 
         pchs = c(19, 21), xlab = NULL, ylab = NULL, ...) {
         
  # validate args
  if (!is.ts(x) || is.matrix(x))
    stop("'x' must be a vector of class 'ts'")
  if (w < 5)
    stop("window must be at least 5 years")
  
  # result for each window
  sr <- function(y, x1=x, w1=w, mval1=mval) {
    # set current window and get slope
    fr <- frequency(x1)
    x2 <- window(x1, start = y, end = c(y + w1 - 1, fr), extend = TRUE)
    sk <- seaKen(x2)
    
    # make sure enough data are present
    miss.ok <- sum(sk$miss >= 0.5) / fr < mval1
    N <- sum(!is.na(x2))
    if (N < 3 * fr || N < 10 || !miss.ok) {
      c(NA, NA, NA)
    } else {
      c(sk$sen.slope, sk$sen.slope.rel, sk$p.value)
    }
  }
  
  # combine windows
  sx <- start(x)[1]
  ex <- end(x)[1]
  ans <- t(sapply(sx:(ex - w + 1), function(i) sr(i)))
  rownames(ans) <- sx:(ex - w + 1)
  colnames(ans) <- c("sen.slope", "sen.slope.rel", "p.value")

  # plot if TRUE
  if (!plot) {
    ans
  } else {
    xlab <- if (is.null(xlab)) "" else xlab
    ylab <- if (is.null(ylab)) "" else ylab
    pch <- ifelse(ans[, "p.value"] < pval, pchs[1], pchs[2])
    plot(ans[, "sen.slope"] ~ rownames(ans), pch = pch, 
         xlab = xlab, ylab = ylab, ...)
  }
}

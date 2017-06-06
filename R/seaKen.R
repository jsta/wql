#' Seasonal and Regional Kendall trend test
#' 
#' Calculates the Seasonal or Regional Kendall test of trend significance,
#' including an estimate of the Sen slope.
#' 
#' The Seasonal Kendall test (Hirsch et al. 1982) is based on the Mann-Kendall
#' tests for the individual seasons (see \code{\link{mannKen}} for additional
#' details). \emph{p}-values provided here are not corrected for serial
#' correlation among seasons.
#' 
#' If \code{plot = TRUE}, then either the Sen slope in units per year
#' (\code{type = "slope"}) or the relative slope in fraction per year
#' (\code{type = "relative"}) is plotted. The relative slope is defined
#' identically to the Sen slope except that each slope is divided by the first
#' of the two values that describe the slope. Plotting the relative slope is
#' useful when the variables in \code{x} are always positive and have different
#' units.
#' 
#' The plot symbols indicate, respectively, that the trend is statistically
#' significant or not. The plot can be customized by passing any arguments used
#' by \code{\link{dotchart}} such as \code{xlab}, as well as graphical
#' parameters described in \code{\link{par}}.
#' 
#' If \code{mval} or more of the seasonal slope estimates are missing, then
#' that trend is considered to be missing. The seasonal slope estimate
#' (\code{\link{mannKen}}), in turn, is missing if half or more of the possible
#' comparisons between the first and last 20\% of the years are missing.
#' 
#' The function can be used in conjunction with \code{mts2ts} to calculate a
#' Regional Kendall test of significance for annualized data, along with a
#' regional estimate of trend (Helsel and Frans 2006). See the examples below.
#' 
#' @param x A numeric vector, matrix or data frame made up of seasonal time
#' series
#' @param plot Should the trends be plotted when x is a matrix or data frame?
#' @param type Type of trend to be plotted, actual or relative to series median
#' @param order Should the plotted trends be ordered by size?
#' @param pval p-value for significance
#' @param mval Minimum fraction of seasons needed with non-missing slope
#' estimates
#' @param pchs Plot symbols for significant and not significant trend
#' estimates, respectively
#' @param ...  Other arguments to pass to plotting function
#' @return A list of the following if \code{x} is a vector: \code{seaKen}
#' returns a list with the following members: \item{sen.slope }{Sen slope}
#' \item{sen.slope.pct}{Sen slope as percent of mean}
#' \item{p.value}{significance of slope} \item{miss}{for each season, the
#' fraction missing of slopes connecting first and last 20\% of the years} or a
#' matrix with corresponding columns if \code{x} is a matrix or data frame.
#' @seealso \code{\link{mannKen}}, \code{\link{mts2ts}},
#' \code{\link{trendHomog}}
#' @references Helsel, D.R. and Frans, L. (2006) Regional Kendall test for
#' trend. \emph{Environmental Science and Technology} \bold{40(13),} 4066-4073.
#' 
#' Hirsch, R.M., Slack, J.R., and Smith, R.A. (1982) Techniques of trend
#' analysis for monthly water quality data. \emph{Water Resources Research}
#' \bold{18,} 107-121.
#' @keywords ts
#' @export
#' @examples
#' 
#' # Seasonal Kendall test:
#' chl <- sfbayChla # monthly chlorophyll at 16 stations in San Francisco Bay
#' seaKen(sfbayChla[, 's27']) # results for a single series at station 27
#' seaKen(sfbayChla) # results for all stations
#' seaKen(sfbayChla, plot=TRUE, type="relative", order=TRUE)
#' 
#' # Regional Kendall test:
#' # Use mts2ts to change 16 series into a single series with 16 "seasons"
#' seaKen(mts2ts(chl))  # too many missing data
#' seaKen(mts2ts(chl, seas = 2:4)) # better when just Feb-Apr, spring bloom period,
#'                                 # but last 4 stations still missing too much.
#' seaKen(mts2ts(chl[, 1:12], 2:4)) # more reliable result
#' 
seaKen <-
function(x, plot = FALSE, type = c("slope", "relative"), order = FALSE,
         pval = .05, mval = .5, pchs = c(19, 21), ...) {

  # validate args
  if (!is.numeric(x) && !is.matrix(x) && !is.data.frame(x))
    stop("'x' must be a vector, matrix, or data.frame")
  if (!is.null(ncol(x)) && is.null(colnames(x)))
    colnames(x) <- paste("series_", 1:ncol(x), sep="")
  type <- match.arg(type)

  # test for single series
  sk <- function(x) {

    # validate args
    if (!is.ts(x))
      stop("'x' must be of class 'ts'")
    if (identical(frequency(x), 1))
      stop("'x' must be a seasonal time series with frequency > 1")

    # extend series to full years
    fr <- frequency(x)
    xmod <- length(x) %% fr
    if (!identical(xmod, 0))
      x <- ts(c(x, rep(NA, fr - xmod)), start = start(x), frequency = fr)

    # apply mannKen to matrix of months
    x1 <- matrix(x, ncol = fr, byrow = TRUE)
    mk1 <- mannKen(x1)

    # calculate sen slope
    slopes <- slopes.rel <- NULL
    for (m in 1:fr) {

      ## select data for current season
      xm <- x[cycle(x) == m]
      tm <- time(x)[cycle(x) == m]

      ## calculate slopes for current season
      outr <- outer(xm, xm, '-')/outer(tm, tm, '-')
      slopes.m <- outr[lower.tri(outr)]
      slopes <- c(slopes, slopes.m)
      outr.rel <- sweep(outr, 2, xm, '/')
      slopes.rel.m <- outr.rel[lower.tri(outr.rel)]
      slopes.rel <- c(slopes.rel, slopes.rel.m)
    }

    sen.slope <- median(slopes, na.rm = TRUE)
    sen.slope.rel <- median(slopes.rel, na.rm = TRUE)

    # calculate sen slope significance
    S <- sum(mk1[, "S"])
    varS <- sum(mk1[, "varS"])
    Z <- (S - sign(S)) / sqrt(varS)
    p.value <- 2 * pnorm(-abs(Z))

    miss <- round(sum(mk1[, "miss"] >= .5) / fr, 3)
    c(sen.slope = sen.slope,
      sen.slope.rel = sen.slope.rel,
      p.value = p.value,
      miss = miss)
  }

  # apply sk for each series
  if (is.null(dim(x))) return(as.list(sk(x)))
  if (ncol(x) == 1) return(as.list(sk(x[, 1])))
  ans <- t(sapply(1:ncol(x), function(i) sk(x[, i])))
  rownames(ans) <- colnames(x)

	# plot if TRUE
	if (!plot) {
	  ans
	} else {
	  v1 <- switch(type,
	               slope = "sen.slope",
	               relative = "sen.slope.rel")
	  if (order) ans <- ans[order(ans[, v1]), ]
	  pch <- ifelse(ans[, "miss"] >= mval, NA,
	                ifelse(ans[, "p.value"] < pval, pchs[1], pchs[2]))
	  dotchart(ans[, v1], pch = pch, ...)
	}
}

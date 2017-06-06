#' Mann-Kendall trend test and the Sen slope
#' 
#' Applies Kendall's tau test for the significance of a monotonic time series
#' trend (Mann 1945). Also calculates the Sen slope as an estimate of this
#' trend.
#' 
#' The Sen slope (alternately, Theil or Theil-Sen slope)---the median slope
#' joining all pairs of observations---is expressed by quantity per unit time.
#' The fraction of missing slopes involving the first and last fifths of the
#' data are provided so that the appropriateness of the slope estimate can be
#' assessed and results flagged. Schertz et al. [1991] discuss this and related
#' decisions about missing data. Other results are used for further analysis by
#' other functions. Serial correlation is ignored, so the interval between
#' points should be long enough to avoid strong serial correlation.
#' 
#' For the relative slope, the slope joining each pair of observations is
#' divided by the first of the pair before the overall median is taken. The
#' relative slope makes sense only as long as the measurement scale is
#' non-negative (not, e.g., temperature on the Celsius scale). Comparing
#' relative slopes is useful when the variables in \code{x} have different
#' units.
#' 
#' If \code{plot = TRUE}, then either the Sen slope (\code{type = "slope"}) or
#' the relative Sen slope (\code{type = "relative"}) are plotted. The plot
#' symbols indicate, respectively, that the trend is significant or not
#' significant. The plot can be customized by passing any arguments used by
#' \code{\link{dotchart}} such as \code{xlab} or \code{xlim}, as well as
#' graphical parameters described in \code{\link{par}}.
#' 
#' @param x A numeric vector, matrix or data frame
#' @param plot Should the trends be plotted when x is a matrix or data frame?
#' @param type Type of trend to be plotted, actual or relative
#' @param order Should the plotted trends be ordered by size?
#' @param pval p-value for significance
#' @param pchs Plot symbols for significant and not significant trend
#' estimates, respectively
#' @param ...  Other arguments to pass to plotting function
#' @return A list of the following if \code{x} is a vector:
#' \item{sen.slope}{Sen slope} \item{sen.slope.rel}{Relative Sen slope}
#' \item{p.value}{Significance of slope} \item{S}{Kendall's S}
#' \item{varS}{Variance of S} \item{miss}{Fraction of missing slopes connecting
#' first and last fifths of \code{x}} or a matrix with corresponding columns if
#' \code{x} is a matrix or data frame.
#' @note Approximate p-values with corrections for ties and continuity are used
#' if \eqn{n > 10} or if there are any ties. Otherwise, exact p-values based on
#' Table B8 of Helsel and Hirsch (2002) are used. In the latter case, \eqn{p =
#' 0.0001} should be interpreted as \eqn{p < 0.0002}.
#' @seealso \code{\link{seaKen}}, \code{\link{seasonTrend}},
#' \code{\link{tsSub}}
#' @references Mann, H.B. (1945) Nonparametric tests against trend.
#' \emph{Econometrica} \bold{13,} 245--259.
#' 
#' Helsel, D.R. and Hirsch, R.M. (2002) \emph{Statistical methods in water
#' resources.} Techniques of Water Resources Investigations, Book 4, chapter
#' A3. U.S. Geological Survey. 522 pages.
#' \url{http://pubs.usgs.gov/twri/twri4a3/}
#' 
#' Schertz, T.L., Alexander, R.B., and Ohe, D.J. (1991) \emph{The computer
#' program EStimate TREND (ESTREND), a system for the detection of trends in
#' water-quality data.} Water-Resources Investigations Report 91-4040, U.S.
#' Geological Survey.
#' @keywords ts
#' @importFrom stats pnorm
#' @export
#' @examples
#' 
#' tsp(Nile)  # an annual time series
#' mannKen(Nile)
#' 
#' y <- sfbayChla
#' y1 <- interpTs(y, gap=1)  # interpolate single-month gaps only
#' y2 <- aggregate(y1, 1, mean, na.rm=FALSE)
#' mannKen(y2)
#' mannKen(y2, plot=TRUE) # missing data means missing trend estimates
#' mannKen(y2, plot=TRUE, xlim = c(0.1, 0.25))
#' mannKen(y2, plot=TRUE, type='relative', order = TRUE, pval = .001,
#'   xlab = "Relative trend")
#' legend("topleft", legend = "p < 0.001", pch = 19, bty="n")
#' 
mannKen <-
function(x, plot = FALSE, type = c("slope", "relative"),
         order = FALSE, pval = .05, pchs = c(19, 21), ...) {

	# validate args
	if (!is.numeric(x) && !is.matrix(x) && !is.data.frame(x))
	  stop("'x' must be a vector, matrix, or data.frame")
	if (!is.null(ncol(x)) && is.null(colnames(x)))
	  colnames(x) <- paste("series_", 1:ncol(x), sep="")
  type <- match.arg(type)

  kendalls_S_2sided_pvalues <-
structure(list(n = c(3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6,
6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8,
8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
9, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10,
10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10), S = c(1,
3, 0, 2, 4, 6, 0, 2, 4, 6, 8, 10, 1, 3, 5, 7, 9, 11, 13, 15,
1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 0, 2, 4, 6, 8, 10, 12,
14, 16, 18, 20, 22, 24, 26, 28, 0, 2, 4, 6, 8, 10, 12, 14, 16,
18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 1, 3, 5, 7, 9, 11, 13,
15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45
), pvalue = c(1, 0.334, 0.625, 0.75, 0.334, 0.084, 0.592, 0.816,
0.484, 0.234, 0.084, 0.0166, 1, 0.72, 0.47, 0.272, 0.136, 0.056,
0.0166, 0.0028, 1, 0.772, 0.562, 0.382, 0.238, 0.136, 0.07, 0.03,
0.0108, 0.0028, 4e-04, 0.548, 0.904, 0.72, 0.548, 0.398, 0.276,
0.178, 0.108, 0.062, 0.0312, 0.0142, 0.0056, 0.0018, 4e-04, 1e-04,
0.54, 0.92, 0.762, 0.612, 0.476, 0.358, 0.26, 0.18, 0.12, 0.076,
0.044, 0.0248, 0.0126, 0.0058, 0.0024, 8e-04, 2e-04, 1e-04, 1e-04,
1, 0.862, 0.728, 0.6, 0.484, 0.38, 0.292, 0.216, 0.156, 0.108,
0.072, 0.046, 0.0286, 0.0166, 0.0092, 0.0046, 0.0022, 0.001,
4e-04, 1e-04, 1e-04, 1e-04, 1e-04)), .Names = c("n", "S", "pvalue"
), row.names = c(NA, 88L), class = "data.frame")

  # function for single vector
	mk <- function(x, ks = kendalls_S_2sided_pvalues) {

	  # extent of NAs in first and last fifths of data
	  len <- length(x)
	  fifth <- ceiling(len/5)
	  xbeg <- x[1:fifth]
	  xend <- x[(len - fifth + 1):len]
	  miss <- (fifth^2 - sum(!is.na(xbeg)) * sum(!is.na(xend)))/fifth^2

	  # get rid of NAs and check data length
	  y <- x[!is.na(x)]
	  t <- time(x)[!is.na(x)]

	  # Sen slope
	  outr <- outer(y, y, "-")/outer(t, t, "-")
	  outr.rel <- sweep(outr, 2, y, '/')
	  sen.slope <- median(outr[lower.tri(outr)])
	  sen.slope.rel <- median(outr.rel[lower.tri(outr.rel)])

	  # Kendall S
	  outr <- sign(outer(y, y, "-")/outer(t, t, "-"))
	  S <- sum(outr[lower.tri(outr)])

	  # variance of S
	  ties <- rle(sort(y))$lengths
	  n <- length(y)
	  t1 <- n * (n - 1) * (2 * n + 5)
	  t2 <- sum(ties * (ties - 1) * (2 * ties + 5))
	  varS <- (t1 - t2)/18

	  # p-value
	  if (n > 10 || any(ties > 1) ) {
	    # using approximate distribution
  	  Z <- (S - sign(S))/sqrt(varS)
      p.value <- 2 * pnorm(-abs(Z))
	  } else {
      if (n < 3) {
        p.value <- NA
      } else {
	      # using exact values
        p.value <- ks[ks$S == abs(S) & ks$n == n, 3]
      }
	  }

	  c(sen.slope = sen.slope,
	    sen.slope.rel = sen.slope.rel,
	    p.value = p.value,
	    S = S,
	    varS = varS,
	    miss = round(miss, 3))
	}

	# apply mk for each vector
	if (is.null(dim(x))) return(as.list(mk(x)))
	if (ncol(x) == 1) return(as.list(mk(x[, 1])))
	ans <- t(sapply(1:ncol(x), function(i) mk(x[, i])))
	rownames(ans) <- colnames(x)

	# plot if TRUE
	if (!plot) {
	  ans
	} else {
	  v1 <- switch(type,
	               slope = "sen.slope",
	               relative = "sen.slope.rel"
	  )
	  if (order) ans <- ans[order(ans[, v1]), ]
	  pch <- ifelse(ans[, "miss"] >= .5, NA,
	           ifelse(ans[, "p.value"] < pval, pchs[1], pchs[2]))
	  dotchart(ans[, v1], pch = pch, ...)
	}
}

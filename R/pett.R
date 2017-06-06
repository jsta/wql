#' Nonparametric Change-Point Detection
#' 
#' Locates a single change-point in an annual series based on the Pettitt test.
#' 
#' Pettitt's (1979) method is a rank-based nonparametric test for abrupt
#' changes in a time series. It uses the Mann-Whitney statistic for testing
#' that two samples (before and after the change-point) come from the same
#' distribution, choosing the change-point that maximizes the statistic. The
#' \emph{p}-value is approximate but accurate to 0.01 for \eqn{p \le} 0.5.
#' Serial correlation is ignored, so the interval between points should be long
#' enough to avoid strong serial correlation. The size of the change is
#' estimated as the median difference between all pairs of observations in
#' which the first one is after the change-point and the second is up to the
#' change-point.
#' 
#' Missing values are allowed at the beginning or end of each variable but
#' interior missing values will produce an NA. Otherwise the change-point might
#' not be meaningful.
#' 
#' If \code{plot = TRUE}, a dot plot of \code{change.times} is shown. If
#' \code{sort = TRUE}, the dots are sorted by \code{change.time}. The plot
#' symbols indicate, respectively, that the trend is significant or not
#' significant. The plot can be customized by passing any arguments used by
#' \code{\link{dotchart}} such as \code{xlab}, as well as graphical parameters
#' described in \code{\link{par}}.
#' 
#' @param x a numeric vector, matrix or data frame with no missing interior
#' values
#' @param plot Should the trends be plotted when x is a matrix?
#' @param order Should the plotted trends be ordered by size?
#' @param pval p-value for significance
#' @param pchs Plot symbols for significant and not significant trend
#' estimates, respectively
#' @param ...  Other arguments to pass to plotting function
#' @return A list of the following if \code{x} is a vector:
#' \item{pettitt.K}{Pettitt's statistic} \item{p.value}{significance
#' probability for statistic} \item{change.point}{last position preceding
#' change to new level} \item{change.time}{if available, time of change.point
#' position} \item{change.size}{median of all differences between points after
#' and up to change.point} or a matrix with corresponding columns if \code{x}
#' is a matrix or data frame.
#' @note The \code{change.point} returned by these functions is the last
#' position before the series actually changes, for consistency with the
#' original Pettitt test. But for reporting purposes, the following position
#' might be more appropriate to call the \dQuote{change-point}.
#' 
#' The Pettitt test produces a supposed change-point, even when the trend is
#' smooth, or when the abrupt change is smaller than the long-term smooth
#' change. Remove any smooth, long-term trend before applying this test.
#' @references Pettitt, A. N. (1979) A non-parametric approach to the
#' change-point problem. \emph{Journal of the Royal Statistical Society. Series
#' C (Applied Statistics)} \bold{28(2),} 126--135.
#' @keywords ts nonparametric
#' @importFrom graphics dotchart
#' @importFrom stats is.ts setNames frequency
#' @export
#' @examples
#' 
#' # data from Pettitt (1979, Table 1):
#' y <- c(-1.05, 0.96, 1.22, 0.58, -0.98, -0.03, -1.54, -0.71, -0.35, 0.66, 0.44,
#'   0.91, -0.02, -1.42, 1.26, -1.02, -0.81, 1.66, 1.05, 0.97, 2.14, 1.22, -0.24,
#'   1.60, 0.72, -0.12, 0.44, 0.03, 0.66, 0.56, 1.37, 1.66, 0.10, 0.80, 1.29, 0.49,
#'   -0.07, 1.18, 3.29, 1.84)
#' pett(y) # K=232, p=0.0146, change-point=17, the same results as Pettitt
#' # identify the year of a change-point in an annual time series:
#' pett(Nile)
#' # apply to a matrix time series:
#' y <- ts.intersect(Nile, LakeHuron)
#' pett(y)
#' pett(y, plot = TRUE, xlab = "Change-point")
#' legend("topleft", legend = "p < 0.05", pch = 19, bty="n")
#' # note how a smooth trend can disguise a change-point:
#' y <- 1:100 + c(rep(0, 75), rep(10, 25)) # smooth trend with change-point at 75
#' pett(y) # gives 50, erroneously
#' pett(residuals(lm(y~I(1:100)))) # removing trend gives 75, correctly
#' 
pett <-
function(x, plot = FALSE, order = FALSE, pval = .05,
         pchs = c(19, 21), ...) {

  # validate args
  if (!is.numeric(x) && !is.matrix(x) && !is.data.frame(x)) {
    stop("'x' must be a vector, matrix, or data.frame")
  }

  # function for single vector
  pet <- function(x) {

    # missing data check
    trimna <- cumsum(!is.na(x)) > 0 & rev(cumsum(rev(!is.na(x)))) > 0
    if (is.ts(x)) {
      tx <- time(x)[trimna]
      x1 <- window(x, start = tx[1], end = tx[length(tx)])
    } else {
      x1 <- x[trimna]
    }
    if (anyNA(x1))
      return(setNames(rep(NA, 4), c("pettitt.k", "p.value",
        "change.point", "change.time")))

    # Pettitt change-point statistic
    n <- length(x1)
    outr <- outer(x1, x1, "-")
    d <- sign(outr)
    u <- sapply(1:(n - 1), function(i) sum(d[1:i, (i + 1):n]))
    pettitt.K <- max(abs(u))

    # approximate probability value for Pettitt statistic
    p.value <- 2 * exp(-6 * pettitt.K ^ 2 / (n ^ 3 + n ^ 2))
    p.value <- signif(p.value, 3)

    # change position
    change.point <- which.max(abs(u))
    if (is.ts(x1) || is.zoo(x1)) {
      change.time <- time(x1)[change.point]
    } else {
      change.time <- change.point
    }

    # change size
    change.size <- median(outr[(change.point+1):n, 1:change.point])

    c(
      pettitt.K = pettitt.K,
      p.value = p.value,
      change.point = change.point,
      change.time = change.time,
      change.size = change.size
    )
  }

  # apply pet for each vector
  if (is.null(dim(x))) return(as.list(pet(x)))
  if (identical(ncol(x), 1)) return(as.list(pet(x[, 1])))
  ans <- t(sapply(1:ncol(x), function(i) pet(x[, i])))
  rownames(ans) <- colnames(x)

  # plot if TRUE
  if (!plot) {
    ans
  } else {
    if (order) ans <- ans[order(ans[, "change.time"]), ]
    pch <- ifelse(ans[, "p.value"] < pval, pchs[1], pchs[2])
    dotchart(ans[, "change.time"], pch = pch, ...)
  }
}

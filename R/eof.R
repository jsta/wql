#' Empirical orthogonal function analysis
#' 
#' Finds and rotates empirical orthogonal functions (EOFs).
#' 
#' EOF analysis is used to study patterns of variability (\dQuote{modes}) in a
#' matrix time series and how these patterns change with time
#' (\dQuote{amplitude time series}). Hannachi et al. (2007) give a detailed
#' discussion of this exploratory approach with emphasis on meteorological
#' data. In oceanography and climatology, the time series represent
#' observations at different spatial locations (columns) over time (rows). But
#' columns can also be seasons of the year (Jassby et al. 1999) or even a
#' combination of seasons and depth layers (Jassby et al. 1990). EOF analysis
#' uses the same techniques as principal component analysis, but the time
#' series are observations of the same variable in the same units. Scaling the
#' data is optional, but it is the default here.
#' 
#' Eigenvectors (unscaled EOFs) and corresponding eigenvalues (amount of
#' explained variance) are found by singular value decomposition of the
#' centered and (optionally) scaled data matrix using \code{\link{prcomp}}. In
#' order to facilitate a physical interpretation of the variability modes, a
#' subset consisting of the \code{n} most important EOFs is rotated (Richman
#' 1986). \code{\link{eofNum}} can be used to help choose \code{n}. Hannachi et
#' al. (2007) recommend orthogonal rotation of EOFs scaled by the square root
#' of the corresponding eigenvalues to avoid possible computation problems and
#' reduce sensitivity to the choice of \code{n}. We follow this recommendation
#' here, using the \code{\link{varimax}} method for the orthogonal rotation.
#' 
#' Note that the signs of the EOFs are arbitrary.
#' 
#' @param x a data frame or matrix, with no missing values
#' @param n number of EOFs to retain for rotation
#' @param scale.  logical indicating whether the (centered) variables should be
#' scaled to have unit variance
#' @return A list with the following members: \item{REOF}{a matrix with rotated
#' EOFs} \item{amplitude}{a matrix with amplitude time series of
#' \acronym{REOF}s} \item{eigen.pct}{all eigenvalues of correlation matrix as
#' percent of total variance} \item{variance}{variance explained by retained
#' EOFs}
#' @seealso \code{\link{eofNum}}, \code{\link{eofPlot}},
#' \code{\link{monthCor}}, \code{\link{ts2df}}
#' @references Hannachi, A., Jolliffe, I.T., and Stephenson, D.B. (2007)
#' Empirical orthogonal functions and related techniques in atmospheric
#' science: A review. \emph{International Journal of Climatology} \bold{27,}
#' 1119--1152.
#' 
#' Jassby, A.D., Powell, T.M., and Goldman, C.R. (1990) Interannual
#' fluctuations in primary production: Direct physical effects and the trophic
#' cascade at Castle Lake, California (USA). \emph{Limnology and Oceanography}
#' \bold{35,} 1021--1038.
#' 
#' Jassby, A.D., Goldman, C.R., Reuter, J.E., and Richards, R.C. (1999) Origins
#' and scale dependence of temporal variability in the transparency of Lake
#' Tahoe, California-Nevada. \emph{Limnology and Oceanography} \bold{44,}
#' 282--294.
#' 
#' Richman, M. (1986) Rotation of principal components. \emph{Journal of
#' Climatology} \bold{6,} 293--335.
#' @keywords ts
#' @examples
#' 
#' # Create an annual matrix time series
#' chla1 <- aggregate(sfbayChla, 1, mean, na.rm = TRUE)
#' chla1 <- chla1[, 1:12]  # remove stations with missing years
#' # eofNum (see examples) suggests n = 1
#' eof(chla1, 1)
#' 
eof <-
function (x, n, scale. = TRUE) {

  # Validate args
  if (!is.matrix(x) && !is.data.frame(x))
    stop("x must be a 'matrix' or 'data.frame'")
  if (identical(colnames(x), NULL))
    colnames(x) <- paste("v", 1:ncol(x), sep="")
  if (anyDuplicated(colnames(x)) > 0)
    stop("x must have distinct column names")
  if (is.mts(x))
    rownames(x) <- time(x)
  if (identical(rownames(x), NULL))
    rownames(x) <- 1:nrow(x)
  if (anyDuplicated(rownames(x)) > 0)
    stop("x must have distinct row names")

  # get EOFs (as scaled eigenvectors)
  pr1 <- prcomp(x, scale.=TRUE)
  eigenval1 <- pr1[["sdev"]][1:n]^2
  eigenvec1 <- pr1[["rotation"]][, 1:n]
  eof1 <- eigenvec1 %*% diag(sqrt(eigenval1), n, n)
  scores1 <- pr1[["x"]][, 1:n]
  amp1 <- scale(scores1)
  attributes(amp1)$`scaled:center` <- attributes(amp1)$`scaled:scale` <- NULL

  # get REOFs by orthogonally rotating EOFs
  if (identical(n, 1)) {
    reof <- as.matrix(eof1)
    amp <- amp1
  } else {
    pr2 <- varimax(eof1)
    reof <- unclass(pr2[["loadings"]])
    rotater <- pr2[["rotmat"]]
    amp <- amp1 %*% rotater
  }

  # percent and cumulative percent of total variance
  eigs <- pr1[["sdev"]]^2
  eigen.pct <- round(100 * eigs/sum(eigs), 1)
  totvar.pct <- round(100 * cumsum(eigs/sum(eigs)), 1)

  # return results
  colnames(reof) <- colnames(amp) <- paste('EOF', 1:n, sep='')
  rownames(reof) <- colnames(x)
  list(REOF=reof, amplitude=amp, eigen.pct=eigen.pct,
  	variance=totvar.pct)
}

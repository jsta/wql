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

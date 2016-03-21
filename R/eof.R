eof <-
function (x, n) {

  # Validate args
  if (!is.matrix(x) && !is.data.frame(x))
    stop("x must be a 'matrix' or 'data.frame'")
  if (identical(colnames(x), NULL) || anyDuplicated(colnames(x)) > 0)
    stop("x must have distinct colnames defined")
  if (is.mts(x)) 
    rownames(x) <- time(x) 
  if (identical(rownames(x), NULL) || anyDuplicated(rownames(x)) > 0)
    stop("x must have distinct rownames defined")
  
  # Calculate eigenvalues and total percent variance for first n
  eigs <- svd(cor(x))$d
  eigen.pct <- round(100 * eigs/ncol(x), 1)
  totvar.pct <- round(sum(eigen.pct[1:n]), 1)
  
  # calculate REOFs
  pr1 <- prcomp(x, scale. = TRUE)
  if (n > 1) {
    pr2 <- promax(pr1$rotation[, 1:n], m=2)
    reof <- unclass(loadings(pr2))
  } else {
    reof <- as.matrix(pr1$rotation[, 1])
  }
  
  # calculate REOF amplitudes (scores)
  amp <- scale(x) %*% reof
  
  # return results
  colnames(reof) <- colnames(amp) <- paste('EOF', 1:n, sep='')
  reof <- cbind(id=ordered(colnames(x), levels=colnames(x)),
  	as.data.frame(reof))
  amp <- cbind(id=ordered(rownames(x), levels=rownames(x)),
  	as.data.frame(amp))
  rownames(reof) <- rownames(amp) <- NULL
  list(REOF=reof, amplitude=amp, eigen.pct=eigen.pct,
  	variance=totvar.pct)
}

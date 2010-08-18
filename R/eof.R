eof <-
  function (x, n) 
{
### aj 10/20/09 4:18 PM
### Find and rotate first n EOFs of a matrix, mts or data.frame. NAs not
###	allowed.
### Args:
###   x: a numeric matrix, mts or data.frame
###   n: number of EOFs to retain and rotate
### Returns a list consisting of:
###   REOF: a data.frame with rotated EOFs
###   amplitude: a data.frame with amplitude time series of REOFs
###   eigenvalue.pc: all eigenvalues of correlation matrix as percent of
###	  total variance
###   variance: variance explained by retained EOFs
  
  ## Validate args
  if (!is(x,'matrix') && !is(x,'mts') && !is(x,'data.frame'))
    stop("x must be a 'matrix' or 'data.frame'")
  if (identical(colnames(x), NULL) || anyDuplicated(colnames(x)) > 0)
    stop("x must have distinct colnames defined")
  if (is(x, 'mts')) 
    rownames(x) <- time(x) 
  if (identical(rownames(x), NULL) || anyDuplicated(rownames(x)) > 0)
    stop("x must have rownames defined")
  
  ## Calculate eigenvalues and total % variance for first n
  eigs <- svd(cor(x))$d
  eigen.pct <- round(100 * eigs/ncol(x), 1)
  totvar.pct <- round(sum(eigen.pct[1:n]), 1)
  
  ## calculate REOFs
  pr1 <- prcomp(x, scale. = TRUE)
  if (n > 1) {
    pr2 <- promax(pr1$rotation[, 1:n], m=2)
    reof <- unclass(loadings(pr2))
  } else {
    reof <- as.matrix(pr1$rotation[, 1])
  }
  
  ## calculate REOF amplitudes (scores)
  amp <- scale(x) %*% reof
  
  ## return results
  colnames(reof) <- colnames(amp) <- paste('EOF', 1:n, sep='')
  reof <- cbind(id=ordered(colnames(x), levels=colnames(x)),
  	as.data.frame(reof))
  amp <- cbind(id=ordered(rownames(x), levels=rownames(x)),
  	as.data.frame(amp))
  rownames(reof) <- rownames(amp) <- NULL
  list(REOF=reof, amplitude=amp, eigen.pct=eigen.pct,
  	variance=totvar.pct)
}

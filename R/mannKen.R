mannKen <-
function(x) {

### aj 10/21/09 4:34 PM
### Calculate sen slope with Mann-Kendall test of significance
### Args:
### x: ts object
### Returns: a list with
### sen.slope: slope
### sen.slope.pct: slope as percent of mean
### p.value: significance of slope
### S: Kendall's S
### varS: variance of S
### miss: extent of NAs in first and last fifth
    
  ## Validate args
  if (!identical(class(x), 'ts'))
    stop("x must be a 'ts'")
  
  ## Define variance of Kendall's S using a function vark() based on
  ## kensen from ESTREND
  vark <- function(y) {
    ties.y <- rle(sort(y))$lengths
    n <- length(y)
    t1 <- n * (n - 1) * (2 * n + 5)
    t2 <- sum(ties.y * (ties.y - 1) * (2 * ties.y +	5))
    v1 <- (t1 - t2)/18
    return(v1)
  }

  ## Extent of NAs in first and last fifths of data
  len <- length(x)
  fifth <- ceiling(len/5)
  xbeg <- x[1:fifth]
  xend <- x[(len - fifth + 1):len]
  miss <- (fifth^2 - sum(!is.na(xbeg)) * sum(!is.na(xend)))/fifth^2

  ## Get rid of NAs and check data length
  y <- x[!is.na(x)]
  t <- time(x)[!is.na(x)]
  n <- length(y)
  
  ## Sen slope
  outr <- outer(y, y, '-')/outer(t, t, '-')	
  sen.slope <- median(outr[lower.tri(outr)])
  sen.slope.pct <- 100 * sen.slope/mean(y)

  ## Kendall's S
  outr <- sign(outer(y, y, '-')/outer(t, t, '-'))	
  S <- sum(outr[lower.tri(outr)])
  
  ## p value
  varS <- vark(y)
  Z <- (S - sign(S))/sqrt(varS)
  p.value <- 2 * pnorm(-abs(Z))

  ## List results
  list(sen.slope = sen.slope, sen.slope.pct = sen.slope.pct, p.value =
    p.value, S = S, varS = varS, miss = round(miss, 3))

}

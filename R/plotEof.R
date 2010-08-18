plotEof <-
function(x, type = c('coef', 'amp'), rev = FALSE, ord = FALSE) {

### aj 2/4/10 9:57 AM
### Plots REOFs or amplitudes from x, the output of eof().
### Args:
###   type: plot REOF coefficients or amplitudes?
###   ord: If TRUE, then coefficients of first eof are displayed in order
###	  of size
###   rev: If TRUE, then coefficients and amplitudes are multiplied by -1
### Returns:
###   a lattice object
  
  require(lattice)
  require(reshape)
  
  ## Validate args
  type <- match.arg(type)
        
  num <- ncol(x$REOF) - 1

  ## Plot
  if (type == 'coef') {
    d1 <- x$REOF
    if (ord) 
      d1[, 'id'] <- reorder(levels(d1[, 'id']), d1[, 'EOF1'])
		if (rev) {
			for (k in 1:num)
				d1[, k + 1] <- -d1[, k + 1]
    }
    m1 <- melt(d1, id='id')
    dotplot(id ~ value|variable, data=m1,
      xlab = 'coefficient',
      panel = function(x, y) {
        panel.abline(v = c(-0.35, -0.2, 0.2, 0.35), col='pink', lwd=.5)
        panel.dotplot(x, y)
      }
    )
  } else {
    d1 <- x$amplitude   
		if (rev) {
			for (k in 1:num)
				d1[, k + 1] <- -d1[, k + 1]
    }
    d1 <- transform(d1, id = as.numeric(as.character(d1$id)))
    m1 <- melt(d1, id='id')
    xyplot(value ~ id|variable, data=m1, 
      layout = c(1, num, 1),
      xlab='', ylab='amplitude',
      panel = function(x, y) {
        panel.abline(h=0, col='pink', lwd=.5)
        panel.xyplot(x, y, type='b', pch=16)
      },
      strip = FALSE,
      strip.left = TRUE
    )
  }
}

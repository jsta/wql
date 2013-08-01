mannKen <- function(x, plot = FALSE, type = c("slope", 
    "pct", "tau"), order = FALSE) {

	# Variables that otherwise have no visible binding
	sen.slope <- sen.slope.pct <- tau <- p.value <- miss <- NULL

    require(ggplot2)
    ## Validate args
    if (!is(x, "ts")) 
        stop("x must be a 'ts'")
    type <- match.arg(type)

    mk <- function(x) {
        ## Define variance of Kendall's S using function vark based on
        ## kensen from ESTREND.
        vark <- function(y) {
            ties.y <- rle(sort(y))$lengths
            n <- length(y)
            t1 <- n * (n - 1) * (2 * n + 5)
            t2 <- sum(ties.y * (ties.y - 1) * (2 * ties.y + 
                5))
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
        outr <- outer(y, y, "-")/outer(t, t, "-")
        sen.slope <- median(outr[lower.tri(outr)])
        sen.slope.pct <- 100 * sen.slope/abs(mean(y))
        ## Kendall's S
        outr <- sign(outer(y, y, "-")/outer(t, t, "-"))
        S <- sum(outr[lower.tri(outr)])
        ## p value
        varS <- vark(y)
        Z <- (S - sign(S))/sqrt(varS)
        p.value <- 2 * pnorm(-abs(Z))
        ## List results
        list(sen.slope = sen.slope, sen.slope.pct = sen.slope.pct, 
            p.value = p.value, S = S, varS = varS, miss = round(miss, 
                3))
    }

    ## ts or mts?
    if (!is(x, "mts")) {
        ans <- mk(x)
    }
    else {
        ans <- matrix(ncol = 7, nrow = dim(x)[2], dimnames =
        	list(colnames(x), c("sen.slope", "sen.slope.pct", "p.value",
        	"S", "varS", "miss", "tau")))
        for (i in 1:dim(x)[2]) {
            xi <- x[, i]
            ans[i, 1:6] <- unlist(mk(xi))
            n <- sum(!is.na(xi))
            ans[i, 7] <- 2 * ans[i, 4]/(n * (n - 1))
        }
    }

    ## Plot
    if (!plot) {
        ans
    }
    else {
        xlab <- switch(type, 
            slope = expression(paste("Trend (units ", year^{-1}, ")")), 
            pct = expression(paste("Trend (% ", year^{-1}, ")")), 
            tau = expression("Tau")
        )
        ans1 = na.omit(data.frame(ans, variable = rownames(ans)))
        if (order) 
            ans1 <- switch(type, 
               slope = within(ans1, variable <- reorder(variable,
               	sen.slope, mean)),
               pct = within(ans1, variable <- reorder(variable,
               	sen.slope.pct, mean)),
               tau = within(ans1, variable <- reorder(variable, tau,
               	mean))
         )
         p1 <- switch(type, 
            slope = ggplot(ans1, aes(x = sen.slope, y = variable)), 
            pct = ggplot(ans1, aes(x = sen.slope.pct, y = variable)), 
            tau = ggplot(ans1, aes(x = tau, y = variable)))
         p1 +  
         geom_point(
            aes(colour = p.value < 0.05, 
            shape = miss < 0.5)
            ) + 
         scale_colour_manual(
            expression(paste(italic(p), "-value < 0.05")), 
            values = c(`FALSE` = "#1B9E77", `TRUE` = "#D95F02")
            ) + 
         scale_shape_manual(
            expression("missing < 50%"), 
            values = c(`FALSE` = 1, `TRUE` = 16)
            ) + 
         labs(list(y = "", x = xlab))
    }
} 

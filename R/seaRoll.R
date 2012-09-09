seaRoll <- function(x, w = 5, rule = 2, plot = FALSE, 
    ylab = NULL, legend = FALSE) {

    ## Validate args
    if (!is(x, "ts")) 
        stop("x must be a 'ts'")
    if (w < 5) 
        stop("A minimum window of 5 years is required")

    fr <- frequency(x)
    sx <- start(x)[1]
    ex <- end(x)[1]

    ans <- NULL
    for (yr in sx:(ex - w + 1)) {
        ## Set current window and get slope
        if (fr > 1) 
            x1 <- window(x, s = yr, end = c(yr + w - 1, fr), 
                extend = TRUE)
        else x1 <- window(x, s = yr, end = yr + w - 1, extend = TRUE)
        sk <- seaKen(x1)

        ## Make sure enough data are present
        rule.ok <- switch(rule, TRUE, sum(sk$miss >= 0.5)/fr < 
            0.5, )
        N <- sum(!is.na(x1))
        if (N < 3 * fr || N < 10 || !rule.ok) 
            ans1 <- c(NA, NA, NA)
        else ans1 <- c(sk$sen.slope, sk$sen.slope.pct, sk$p.value)
        ans <- rbind(ans, ans1)
    }

    ## Plot or list
    colnames(ans) <- c("sen.slope", "sen.slope.pct", "p.value")
    rownames(ans) <- (sx + w - 1):ex
    ans[, 1:2] <- signif(ans[, 1:2], 3)
    ans[, 3] <- round(ans[, 3], 3)
    if (plot) {
        require(ggplot2)
        ans <- transform(as.data.frame(ans), yr = (sx + w - 
            1):ex)
        ans <- na.omit(ans)
        if (is.null(ylab)) 
            ylab <- expression(paste("Trend (units ", year^{-1}, ")"))
        p1 <- ggplot(ans, aes(x = yr, y = sen.slope, shape = p.value < 
            0.05)) + 
            geom_point(colour = "blue") + 
            scale_shape_manual(expression(paste(italic(p), 
            "-value")), values = c(`FALSE` = 1, `TRUE` = 16), 
            breaks = c(FALSE, TRUE), labels = c("> 0.05", 
                "< 0.05")) + 
            labs(x = "", y = ylab)
        if (!legend) 
            p1 <- p1 + theme(legend.position = "none")
        p1
    }
    else {
        ans
    }
} 

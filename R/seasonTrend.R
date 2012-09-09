
seasonTrend <- function(x, first, last, type = c("slope", 
    "slope.pct"), method = c("mk", "lin"), plot = FALSE, 
    xlab = NULL, ylab = NULL, miss = FALSE, legend = FALSE, 
    ...) {
    require(ggplot2)
    ## Validate args
    if (!is(x, "ts")) 
        stop("x must be a 'ts'")
    if (is(x, "mts") && is.null(colnames(x))) 
        stop("Time series matrix must have column names.")
    if (missing(first)) 
        first = start(x)[1]
    if (missing(last)) 
        last = end(x)[1]
    if (first < start(x)[1]) 
        first <- start(x)[1]
    if (last > end(x)[1]) 
        last <- end(x)[1]
    type <- match.arg(type)
    method <- match.arg(method)
    ## To handle single time series
    x0 <- ts.union(x, x)
    nc <- ncol(x0)/2
    ## Trend function for single series
    if (method == "mk") {
        trend <- function(x) unlist(mannKen(x)[c(1:3, 6)])
    } else {
        trend <- function(x) {
            lm1 <- summary(lm(x ~ time(x)))[["coefficients"]]
            slope <- lm1["time(x)", "Estimate"]
            slope.pct <- 100 * slope/mean(x, na.rm = TRUE)
            p.value <- lm1["time(x)", "Pr(>|t|)"]
            len <- length(x)
            fifth <- ceiling(len/5)
            xbeg <- x[1:fifth]
            xend <- x[(len - fifth + 1):len]
            miss <- (fifth^2 - sum(!is.na(xbeg)) * sum(!is.na(xend)))/fifth^2
            c(slope, slope.pct, p.value, miss)
        }
    }
    ## Gather trends for each ts and season
    fr <- frequency(x)
    ans <- as.data.frame(matrix(nrow = fr * nc, ncol = 4))
    colnames(ans) <- c("slope", "slope.pct", "p", "missing")
    x1 <- window(x0, s = first, end = c(last, fr), extend = TRUE)
    for (j in 1:nc) {
        xj <- x1[, j]
        for (i in 1:fr) {
            xij <- ts(xj[cycle(x1) == i], start = first, end = last)
            ans[i + (j - 1) * fr, ] <- trend(xij)
        }
    }
    ## Identify results with ts names
    tsnames <- {
        if (nc == 1) 
            deparse(substitute(x))
        else colnames(x)
    }
    ts.id <- factor(rep(tsnames, each = fr), levels = tsnames, 
        ordered = TRUE)
    ans1 <- data.frame(trend = ans[, type], ans[, 3:4], season = as.factor(rep(1:fr, 
        times = nc)), ts.id)
    ## Plot or tabulate results
    if (!plot) {
        ans1
    } else {
        ans2 <- na.omit(ans1)
        nr <- nrow(ans2)
        if (is.null(ylab)) 
            ylab <- ifelse(type == "slope", 
                expression(paste("Trend (units ", year^{ -1 }, ")")),
                expression(paste("Trend (% ", year^{ -1 }, ")")))
        if (is.null(xlab)) 
            xlab = ifelse(fr == 12, "Month", ifelse(fr == 
                4, "Quarter", "Season"))    
        if (miss) ans2 <- subset(ans2, missing < 0.5)
        p1 <- ggplot(ans2, aes(x = season, y = trend, fill = p < .05)) +
            geom_bar(stat = 'identity') +
            scale_fill_manual(expression(paste(italic(p), "-value < 0.05")), values = c(`FALSE` = "grey65", `TRUE` = "dodgerblue")) +
            labs(list(y = ylab, x = xlab)) + 
            theme(panel.grid.minor =	element_blank())
            if (nc > 1) 
                p1 <- p1 + facet_wrap(~ ts.id, ...)
            if (!legend) 
                p1 <- p1 + theme(legend.position = "none")
            p1
    }
} 
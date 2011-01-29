plotTsTile <- function(x, plot.title = NULL, legend.title = NULL, 
    four = TRUE, loganom = TRUE, square = TRUE, legend = TRUE, 
    trim = TRUE) {
    ### Produces a month x year image plot with data in 10 or 4 groups.
    ### x: monthly time series vector
    ### plot.title: title above plot
    ### legend.title: title placed above legend
    ### four: If TRUE, divide into 4 special groups; otherwise deciles.
    ### loganom: Should x be converted to log-anomaly data?
    ### legend: Should legend be included?
    ### trim: If TRUE, leading and trailing NAs are removed.
    ### Returns ggplot object
    require(ggplot2)
    ## Validate args
    if (!is(x, "ts") || is(x, "mts") || !identical(frequency(x), 
        12)) 
        stop("x must be a vector of class 'ts' with freq = 12")
    ## trim leading and trailing NAS
    if (trim) {
        x <- as.zoo(x)
        x <- na.trim(x)
        x <- as.ts(x)
    }
    # Complete partial years by padding with NAs
    sx <- start(x)[1]
    ex = end(x)[1]
    x1 <- window(x, start = c(sx, 1), end = c(ex, 12), extend = TRUE)
    # Transform to log-anomalies
    if (loganom) {
        if (any(x1 <= 0, na.rm = TRUE)) {
            stop("All values must be positive if loganom=TRUE")
        }
        else {
            x1 <- log10(x1/mean(x1, na.rm = TRUE))
        }
    }
    # Break data.
    if (four) {
        mmin <- min(x1, na.rm = TRUE)
        mlo <- mean(x1[x1 < 0], na.rm = TRUE)
        mhi <- mean(x1[x1 > 0], na.rm = TRUE)
        mmax <- max(x1, na.rm = TRUE)
        the.breaks <- c(mmin, mlo, 0, mhi, mmax)
    }
    else {
        the.breaks <- quantile(x1, probs = seq(0, 1, 0.1), 
            na.rm = TRUE)
    }
    len <- length(the.breaks)
    if (length(unique(the.breaks)) < len) 
        stop("Breaks between groups are\nnot unique: insufficient unique data.")
    x2 <- cut(x1, breaks = the.breaks, include.lowest = TRUE, 
        dig.lab = 2)
    x3 <- data.frame(yr = floor(time(x1)), mon = ordered(month.abb[cycle(x1)], 
        levels = month.abb), value = x2)
    # Plot it.
    mypalette <- colorRampPalette(c("darkblue", "lightblue", 
        "pink", "red"))
    cols <- mypalette(len - 1)
    p1 <- ggplot(x3, aes(yr, mon, fill = value)) + 
      geom_tile(colour = "white", size = 0.25) + 
      scale_x_continuous(name = "", expand = c(0, 0)) + 
      scale_y_discrete(name = "", expand = c(0, 0)) +
    	scale_fill_manual(name = legend.title, values = cols, breaks =
    	   levels(x3$value), labels = levels(x3$value)) + theme_bw() +
    	opts(title = plot.title, panel.grid.minor = theme_blank(),
    	   panel.grid.major = theme_blank())
    if (!legend) 
        p1 <- p1 + opts(legend.position = "none")
    if (square) 
        p1 + coord_equal()
    else p1
} 

tsSub <- function(x1, seas = 1:frequency(x1)) {
    ### Drop seasons from ts
    ### Args-
    ### x1: class 'ts'
    ### seas: numeric vector of seasons to keep
    ### Returns-
    ### A ts with desired seasons
    if (!is(x1, "ts")) 
        stop("x1 must be of class 'ts'")
    stx <- start(x1)
    frx <- frequency(x1)
    if (!is(x1, "mts")) 
        dim(x1) <- c(length(x1), 1)
    x1 <- window(x1, start = stx, end = c(end(x1)[1], frx), 
        ext = TRUE)
    x1 <- x1[cycle(x1) %in% seas, ]
    ts(x1, start = stx[1], freq = length(seas))
} 

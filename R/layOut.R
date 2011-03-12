layOut = function(...) {

    x <- list(...)
    n <- max(sapply(x, function(x) max(x[[2]])))
    p <- max(sapply(x, function(x) max(x[[3]])))
    pushViewport(viewport(layout = grid.layout(n, p)))    

    for (i in seq_len(length(x))) {
        print(x[[i]][[1]], vp = viewport(layout.pos.row = x[[i]][[2]], 
            layout.pos.col = x[[i]][[3]]))
    }
} 

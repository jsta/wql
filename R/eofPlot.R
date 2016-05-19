eofPlot <-
function(x, type = c("coef", "amp"), rev = FALSE, ord = FALSE) {

  # Validate args
  type <- match.arg(type)
  num <- ncol(x$REOF)

  # Plot
  if (type == "coef") {
    d1 <- x$REOF
    if (ord) d1 <- d1[order(d1[, 1]), ]
    if (rev) d1 <- -d1
    m1 <- melt(d1, varnames = c("variable", "eof"))
    ggplot(m1, aes_string(x = "value", y = "variable")) +
      geom_vline(xintercept = 0, colour = "red", size = 0.2) +
      geom_point(colour = "blue") +
      facet_wrap(~ eof, ncol = num) +
      labs(y = "", x = "Coefficient")
  } else {
    d1 <- x$amplitude
    if (rev) d1 <- -d1
    m1 <- melt(d1, varnames = c("obs", "eof"))
    g1 <- ggplot(m1, aes_string(x = "obs", y = "value")) +
      geom_hline(aes(yintercept = 0), colour = "red", size = 0.2) +
      geom_point(colour = "blue") +
      facet_wrap(~ eof, nrow = num) +
      labs(x = "", y = "Amplitude")
    if (is.factor(m1$obs)) return(g1)
    g1 + geom_line(colour = "blue")
  }
}

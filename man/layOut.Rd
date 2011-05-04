\name{layOut}
\Rdversion{1.1}
\alias{layOut}

\title{
Arrange a group of saved plots
}

\description{
Lays out plots on a grid, with flexible choice of the relative dimensions for each plot. Individual plots must be objects of class "ggplot".
}

\usage{
layOut(...)
}

\arguments{
  \item{...}{
A series of 3-element lists, each containing the name of a saved plot of class "ggplot", the sequence of grid numbers of the rows, and the sequence of grid numbers of the columns that will be occupied by that plot.
}
}

\details{
The position of each plot is determined by the beginning row and column numbers. The relative size of each plot is determined by the sequence lengths of row and column numbers. The total grid size of the graph is determined automatically by the grid numbers used for individual plots. Some manual adjustment of the graphics window may be necessary to get proper aspect ratios and prevent text from overlapping.
}

\value{
A graph containing all the given plots.
}

\examples{
chl27 = sfbayChla[, 's27']
g1 <- plotTsTile(chl27, legend.title = 'Chl log-anomaly',
    square=FALSE)
g2 <- seasonTrend(chl27, plot = TRUE, legend = TRUE)
g3 <- plotSeason(chl27, num.era = 3,
    ylab = expression(paste('Chl-', italic(a), ', ', mu*g~L^{-1})))
## quartz("", 10, 6)  # e.g., in mac os x, or:
## grid.newpage()  # to re-use existing plot window
layOut(list(g1, 1:2, 1:6), list(g2, 1:2, 7:10), list(g3, 3:5, 1:8))
}

\keyword{Graphics}

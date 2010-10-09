\name{decompTs}
\Rdversion{1.1}
\alias{decompTs}

\title{
Decompose a time series
}

\description{
The function decomposes a time series into a long-term mean, annual, seasonal and "events" component. The decomposition can be multiplicative or additive.
}

\usage{
decompTs(x, startyr, endyr, event = TRUE, type = c("mult", "add"))
}
\arguments{
  \item{x }{
a monthly time series vector
}
  \item{startyr }{
the desired starting year for the analysis
}
  \item{endyr }{
the ending year
}
  \item{event }{
whether or not an "events" component should be determined
}
  \item{type }{
the kind of decomposition, either multiplicative ("mult") or additive ("add")
}
}

\details{
 The rationale for this simple approach to decomposing a time series, with examples of its application, is given by Cloern and Jassby (2010). It is motivated by the observation that many important events for estuaries (e.g., persistent dry periods, species invasions) start or stop suddenly. Smoothing to extract the annualized term, which can disguise the timing of these events and make analysis of them unnecessarily difficult, is not used.

A multiplicative decomposition will typically be useful for a biological community- or population-related variable (e.g., chlorophyll-a) that experiences exponential changes in time and is approximately lognormal, whereas an additive deomposition is more suitable for a normal variable.  Aside from the long-term mean, each component of a multiplicative decomposition will average 1, whereas each component of an additive decomposition will average 0.   

If \code{event = TRUE}, the seasonal component represents a recurring monthly pattern and the events component a residual series. Otherwise, the seasonal component becomes the residual series. The latter is appropriate when seasonal patterns change systematically over time. You can use \code{\link{plotSeason}} and \code{\link{plotSeasonTrend}} to investigate the way seasonality changes.
}

\value{
A monthly time series matrix with the following individual time series:
\item{original }{original time series}
\item{grandmean }{constant series equal to the long-term mean}
\item{annual }{annual mean series}
\item{seasonal }{repeating seasonal component}
\item{events }{optionally, the residual or "events" series}
}

\references{
Cloern, J.E. and Jassby, A.D. (2010) Patterns and scales of phytoplankton variability in estuarine-coastal ecosystems. \emph{Estuaries and Coasts} \bold{33,} 230--241.
}

\seealso{
\code{\link{plotSeason}}, \code{\link{plotSeasonTrend}}
}

\examples{
# Apply the function to a matrix time series, producing a list of decompositions
ans <- vector('list', ncol(sfbayChla))
names(ans) <- colnames(sfbayChla)
for(i in seq(along = names(ans))) {
  ans[[i]] <- decompTs(sfbayChla[, i])
}

# A quick plot for a time series decomposiiton
plot(ans[[7]], nc = 1, main = paste(names(ans)[7], "Chl-a decomposition"))
}

\keyword{manip}
\keyword{ts}
\name{ammFrac}
\Rdversion{1.1}
\alias{ammFrac}
\title{
Un-ionized ammonia
}
\description{
Calculates fraction of total ammonium in un-ionized form.
}
\usage{
ammFrac(pH, t, S, pHscale = c('total', 'free'))
}
\arguments{
  \item{pH}{
pH value
}
  \item{t}{
temperature, degrees Celsius
}
  \item{S}{
salinity, Practical Salinity Scale
}
  \item{pHscale}{
scale of pH measurement
}
}
\details{
The stoichiometric dissociation constant \eqn{K_a^*}{Ka*} of the ammonium ion is estimated using the equations of Clegg and Whitfield (1995). There are separate equations for use with pH measurements made on the total and free scales. Equations should be valid in the range of 0 to 40 salinity and -2 to 40 C. Accuracy is probably better than 5\% at all temperatures and salinities.	 The dissociation constant is then used to calculate the mole fraction from \eqn{1/(1+ 10^(-pH)/K_a^*)}{1/(1+10^(-pH)/Ka*)}.
If input vectors are not of the same length, shorter ones will be recycled as necessary without warning.
}
\value{
Fraction of total ammonium occurring as un-ionized ammonium.
}
\references{
Clegg, S. and Whitfield, M. (1995) A chemical model of seawater including dissolved ammonia and the stoichiometric dissociation constant of ammonia in estuarine water and seawater from -2 to 40 C. \emph{Geochimica et Cosmochimica Acta} \bold{59(12),} 2403--2421.

Millero, F. (1995) Thermodynamics of the carbon dioxide system in the oceans. \emph{Geochimica et Cosmochimica Acta} \bold{59(4),} 661--677.
}
\note{There are several approaches available to make these estimates, depending on the underlying theory, experimental data and fitting methods. See package \pkg{seacarb} for an algorithm based on the work of Millero (1995), which includes pressure (and thus depth) corrections.
}
\examples{
## Examine different combinations of environmental variables:
ph = c(8, 8, 8, 7.8)
temp = c(10 , 25, 25, 25)
sal = c(0, 0, 35, 35)
round(ammFrac(ph, temp, sal), 4)  # 0.0183 0.0539 0.0556 0.0358
round(ammFrac(ph, temp, sal, 'free'), 4)  # 0.0183 0.0539 0.0440 0.0282
}
\keyword{manip}
\keyword{utilities}
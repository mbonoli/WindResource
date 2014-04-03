\name{plotDistWd}
\alias{plotDistWd}
\title{Plot to compare distribution}
\usage{
plotDistWd(data, ane = NA)
}
\arguments{
  \item{data}{object of class wd.}

  \item{ane}{a vector with the anemometers'name to study.
  By default "ane" takes the first anemometer.}
}
\value{
Histogram of each distribution with its corresponding
density function. QQ plot of each ditribution. A table with
the following columns: -parameter 1: the first parameter
estimated for each distribution. -parameter 2: the second
parameter estimated for each distribution. -aic: Akaike
Index Coefficient. -loglik: maximun likelihood.
}
\description{
Graphical function to determite the best fit of the
variable speed wind
}
\details{
This function allows you to get the adjustments of the next
distributions: Weibull, Gamma and Lognormal. It shows
histograms (with density functions) and qq-plots graphics.
Also, it delivers a table with the different values for
each distribution (see "Value")
}
\examples{
#Execute the function
#plotDistWd(Wd, ane=1)
}
\author{
Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
}


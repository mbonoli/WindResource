\name{fitWd}
\alias{fitWd}
\title{Fit data to selected distribution.}
\usage{
fitWd(data, ane = NA, dist = c("weibull", "gamma", "lognormal"))
}
\arguments{
  \item{data}{object of class wd.}

  \item{ane}{a vector with the anemometers'name to study.
  By default "ane" takes the first anemometer.}

  \item{dist}{character string "name" naming a distribution
  to be used (see details).}
}
\value{
A list with the following components: -aic: Akaike Index
Coefficient.

-loglik: maximun likelihood.

-parameters: a list with the following components: name:
parameter name type: parameter type value: estimated
parameter sd: standard desvest of estimated parameter
}
\description{
Fit of univariate distribution by maximum likelihood.
}
\details{
The "name" of distribution must be one of "Weibull",
"Gamma", "Lognormal".
}
\examples{
# simple example using the wd class
# list_para <- fitWd(Wd, ane=1, dist="weibull")
}
\author{
Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
}


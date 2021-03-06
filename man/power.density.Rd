% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/power.density.R
\name{power.density}
\alias{power.density}
\title{Power density wind calculator}
\usage{
power.density(datawd, ane, dist)
}
\arguments{
\item{datawd}{an object of class \code{windata}.}

\item{ane}{a vector of character strings with anemometers names.}

\item{dist}{a vector of character strings with the distribution to calculate the power density wind. 
The possible distrbutions are: \code{"np"} (no parametric distribution), \code{"weibul"}, \code{"gama"} and \code{"lognormal"}. (see details)}
}
\value{
a data.frame with the results of power density for the differents anemometers.
}
\description{
This function calcultes the power density wind for each anemometer.
}
\details{
It's important to see what is the best fit of distribution before to calcuate the annual energy production. 
It can be seen with the function \code{plotWD(data, ane, type="fit")}
}
\examples{
data("wd10", package = "WindResource")
data("wtgData", package = "WindResource")

## Getting anemometer names
wd10$ane$ane.names
  
## Calculation of the power density of the anemometers "ane10" and "ane18" from "wd" dataset.
power.density(wd10, ane = c("ane10", "ane18"), dist = "np")
}
\seealso{
plotWD
}
\author{
Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
}

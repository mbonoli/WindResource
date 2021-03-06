% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tablewtg.R
\name{tablewtg}
\alias{tablewtg}
\title{Wind Turbine Especifications}
\usage{
tablewtg(datawtg, model)
}
\arguments{
\item{datawtg}{object of class \code{wtgData}.}

\item{model}{one value of character strings with the model wind turbine name. (see details)}
}
\description{
This function returns a table with especificactiones of 'speed wind', 'power' and 'Cp' of wind turbines.
}
\details{
The wind turbines are specificated in \code{wtgData}.
}
\examples{
data("wtgData", package = "WindResource")
 
# Especifications of wind turbine 'E33'.
tablewtg(wtgData, model = "E33")
}
\author{
Mariano Bonoli Escobar, Diego Edwards, Valeria Gogni, Ruben Bufanio
}

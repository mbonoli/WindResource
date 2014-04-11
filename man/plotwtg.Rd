\name{plotwtg}
\alias{plotwtg}
\title{Plots for winddata objects}
\usage{
plotwtg(data, Model)
}
\arguments{
  \item{data}{an object of class winddata}

  \item{ane}{an optional vector specifying a subset of
  anenemometers to plot}

  \item{var}{currently only method = "qr" is supported}

  \item{type}{the type of graphic to plot. Actually
  soported: 'histogram', 'rose', 'correlogram', 'profiles'
  and 'boxplot'. See also 'Details'.}

  \item{by}{an optional string stating if the plot is
  divided in panels by 'month' or 'hour'.}

  \item{since}{an optional string indicating initial date
  to be taken into account to make the plot.  The string
  format is 'YYYY-MM-DD'.}

  \item{to}{an optional string indicating final date to be
  taken into account to make the plot.  The string format
  is 'YYYY-MM-DD'.}
}
\value{
Object of class "windata" (see details)
}
\description{
Shape a dataframe in a object which will be use by the
diferents functions in the package......
}
\details{
The object windata is a list with the parameters that were
mencionated before.
}
\examples{
# simple example using the windspeed data set
data(wd)

# let's examine windspeed to see the variables' names
head(wd)
}
\author{
Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
}


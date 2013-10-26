\name{plotWD}
\alias{plotWD}
\title{Plots for winddata objects}
\usage{
  plotWD(data, ane = NA, var = c("speed"),
    type = c("histogram"), by = c("none", "month", "hour"),
    since = NULL, to = NULL)
}
\arguments{
  \item{data}{an object of class winddata}

  \item{ane}{an optional vector specifying a subset of
  anenemometers to plot}

  \item{var}{currently only method = "qr" is supported}

  \item{type}{the type of graphic to plot. Actually
  soported: 'histogram', 'rose', 'correlogram', 'profiles'
  and 'boxplot'. See also ‘Details’.}

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
  The object windata is a list with the parameters that
  were mencionated before.
}
\examples{
# simple example using the windspeed data set
data(windspeed)

# let's examine windspeed to see the variables' names
head(windspeed)

# create the object wd of class "windata"
wd<-setwd (data=data,
             interval=1,
             date.var="date",
             date.format="DD/MM/YYYY",
             time.var="time",
             time.format="HH:MM",
             ane.names=c("Ane1", "Ane2"),
             ane.height = c(9, 18),
             speed.ave.var=c("ave", "ave2"),
             speed.min.var=c("min", "min2"),
             speed.max.var=c("max", "max2"),
             speed.sd.var=c("sd", "sd2"),
             speed.var.var=c(NA, NA),
             speed.unit="m/s",
             dir.var="dir",
             dir.unit="deg",
             temp.var=NA,
             temp.unit="C",
             pres.var=NA,
             pres.unit="bar")

# let's examine wd
str(wd)
}
\author{
  Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego
  Edwards
}


#' @title Plots for winddata objects
#' 
#' @description
#' Shape a dataframe in a object which will be use by the diferents functions in the package......
#' 
#' @details
#' The object windata is a list with the parameters that were mencionated before.
#'   
#' @param data an object of class winddata 
#' @param Model an optional vector specifying a subset of anenemometers to plot
#' @return Object of class 'windata' (see details)
#' 
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
#' @export
#' @examples
#' # simple example using the windspeed data set
#' data(wd)
#'  
#' # let's examine windspeed to see the variables' names
#' head(wd)

plotwtg <- function(data, Model) {
    dataplot <- data.frame(data[[Model]])
    names(dataplot) <- c("Branch", "Model", "Speed", "Power", "CP")
    ggplot(dataplot, aes(Speed, Power)) + geom_line()
} 

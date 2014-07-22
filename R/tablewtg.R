#' @title Plots for winddata objects
#' 
#' @description
#' Shape a dataframe in a object which will be use by the diferents functions in the package......
#' 
#' @details
#' The object windata is a list with the parameters that were mencionated before.
#'   
#' @param data an object of class winddata 
#' @param ane an optional vector specifying a subset of anenemometers to plot
#' @param var currently only method = 'qr' is supported
#' @param type the type of graphic to plot. Actually soported: 'histogram', 'rose', 
#'        'correlogram', 'profiles' and 'boxplot'. See also 'Details'.
#' @param by an optional string stating if the plot is divided in panels by 'month' or 'hour'. 
#' @param since an optional string indicating initial date to be taken into account to make the plot.
#'        The string format is 'YYYY-MM-DD'.
#' @param to an optional string indicating final date to be taken into account to make the plot.
#'        The string format is 'YYYY-MM-DD'.
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
#' 

tablewtg <- function(data, Model) {
    datatable <- data[[Model]]$data
    return(datatable)
} 

#' @title Power curves
#' 
#' @description
#' Plot the power curve of differentes wind turbines.
#' 
#' @details
#' The wind turbines are specificated in \code{wtgData}.  
#' 
#' @param datawtg object of class \code{wtgData}. 
#' @param model one value of character strings with the model wind turbine name. (see details)
#' 
#' @author Mariano Bonoli Escobar, Diego Edwards, Valeria Gogni, Ruben Bufanio
#' @export
#' @examples
#' data("wtgData", package = "WindResource")  
#'   
#' ## Power curve of wind turbine 'E33'.
#' plotwtg(wtgData, model = "E33")

plotwtg <- function(datawtg, model) {
  dataplot <- data.frame(datawtg[[model]])
  names(dataplot) <- c("Branch", "Model", "Speed", "Power", "CP")
  ggplot(dataplot, aes(Speed, Power)) + geom_line()
} 

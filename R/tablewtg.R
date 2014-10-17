#' @title Wind Turbine Especifications
#' 
#' @description
#' This function returns a table with especificactiones of 'speed wind', 'power' and 'Cp' of wind turbines.
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
#' # Especifications of wind turbine 'E33'.
#' tablewtg(wtgData, model = "E33")


tablewtg <- function(datawtg, model) {
  datatable <- datawtg[[model]]$data
  return(datatable)
} 

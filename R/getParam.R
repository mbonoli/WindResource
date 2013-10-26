#' @title Extracting paramenters
#' 
#' @description
#' Function to extract a parameter value from a list
#' 
#' @param list a list of parameters.
#' @param param the parameter value that will be extracted.
#' @return The parameter value that was extracted.
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards


#Auxiliary function to get the parameters from fitDistWd list
getParam <- function (list, param) {
  if (list$par1$name == param) {
    result <- list$par1$value
  }
  else if (list$par2$name == param) {
    result <- list$par2$value
  }
  else stop(paste("The parameter", param, "doesn't exist", param, "for the distribution", a$dist, sep = ""))
  result
}
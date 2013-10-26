#' @title Extracting paramenters
#' 
#' @description
#' Function to extract a parameter value from a list
#' 
#' @param center a list of parameters.
#' @param radio the parameter value that will be extracted.
#' @param npoints the parameter value that will be extracted.
#' @return The parameter value that was extracted.
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards

circleFun <- function(center = c(0,0),radio = 1, npoints = 100){
  r = radio
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
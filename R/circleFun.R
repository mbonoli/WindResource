#' @title circle coordinates
#' 
#' @description
#' Generate the 
#' 
#' @param center vector with the coordinates of the circle center.
#' @param radio circle radius.
#' @param npoints number of points.
#' @return Data.frame with the circle coordinates.
#' @author Mariano Bonoli Escobar, Diego Edwards, Valeria Gogni, Ruben Bufanio
#' @export
#' @keywords internal
#' 

circleFun <- function(center = c(0, 0), radio = 1, npoints = 100) {
    r = radio
    tt <- seq(0, 2 * pi, length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
} 

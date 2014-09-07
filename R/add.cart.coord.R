#' @title Extracting paramenters
#' 
#' @description
#' Function to extract a parameter value from a list
#' 
#' @param dataplot a list of parameters.
#' @return The parameter value that was extracted.
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
#' #' @export
#' @export
add.cart.coord <- function(dataplot) {
    dataplot$y <- dataplot$value.start * cos(dataplot$ang.start/180 * pi)
    dataplot$x <- dataplot$value.start * sin(dataplot$ang.start/180 * pi)
    dataplot$yend <- dataplot$value.end * cos(dataplot$ang.end/180 * pi)
    dataplot$xend <- dataplot$value.end * sin(dataplot$ang.end/180 * pi)
    dataplot
} 

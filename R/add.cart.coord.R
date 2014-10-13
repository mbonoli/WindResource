#' @title Add cartesian coordinates to a dataset
#' 
#' @description
#' Add cartesian coordinates to a dataset
#' 
#' @param Dataframe with value.start, value.end, ang.start and ang.end fields
#' @return The same data.frame entered as parameters, with the following additional fields: x, y, xend and yend
#' @author Mariano Bonoli Escobar, Diego Edwards, Valeria Gogni, Ruben Bufanio
#' @export
#' @keywords internal
#' 
add.cart.coord <- function(dataplot) {
    dataplot$y <- dataplot$value.start * cos(dataplot$ang.start/180 * pi)
    dataplot$x <- dataplot$value.start * sin(dataplot$ang.start/180 * pi)
    dataplot$yend <- dataplot$value.end * cos(dataplot$ang.end/180 * pi)
    dataplot$xend <- dataplot$value.end * sin(dataplot$ang.end/180 * pi)
    dataplot
} 

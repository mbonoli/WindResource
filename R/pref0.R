#' @title Complete zeros prefix
#' 
#' @description
#' Complete zeros prefix
#' 
#' @param data a number to complete.
#' @param charnum length of target number.
#' @return character number and zeros prefix
#' @author Mariano Bonoli Escobar, Diego Edwards, Valeria Gogni, Ruben Bufanio
#' @export
#' @keywords internal
#' 

# Auxiliary function
pref0 <- function(data, charnum) {
    .str <- paste("000000000", data, sep = "")
    .str_nchar <- nchar(.str)
    substring(.str, .str_nchar - charnum + 1, .str_nchar)
} 

#' @title Complete numbers
#' 
#' @description
#' Function to Complete numbers
#' 
#' @param data a list of paramenters.
#' @param charnum number of digits.
#' @return parameter's value extracted.
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards

# Auxiliary function 
pref0<-function (data, charnum){
  .str <- paste("000000000", data, sep = "")
  .str_nchar <- nchar(.str)
  substring(.str, .str_nchar - charnum + 1, .str_nchar)
}
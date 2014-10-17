#' @title Anemometer Checks
#' 
#' @description
#' Calculating the annual energy production of differents wind turbines to differents anemometers.
#' 
#' @details
#' It's important to see what is the best fit of distribution before to calcuate the annual energy production. 
#' It can be seen with the function \code{plotWD(data, ane, type="fit")}
#' 
#' @seealso
#' plotWD 
#' 
#' @param datawd an object of class \code{windata}.
#' @param ane a vector of character strings with anemometers names.
#'         
#' @return a data.frame with the results of annual energy production for different wind turbines models and anemometers.
#' 
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
#' @export
#' @keywords internal
#' 
check.ane <- function(datawd, ane) {
  err <- 0
  for (i in ane){
    if (!(i %in% datawd$ane$ane.names))
      stop(paste0("'",ane[i],"' is not a valid anemometer name."))
  }
}
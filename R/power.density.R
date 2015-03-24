#' @title Power density wind calculator
#' 
#' @description
#' This function calcultes the power density wind for each anemometer.
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
#' @param dist a vector of character strings with the distribution to calculate the power density wind. 
#'        The possible distrbutions are: \code{"np"} (no parametric distribution), \code{"weibul"}, \code{"gama"} and \code{"lognormal"}. (see details)
#'         
#' @return a data.frame with the results of power density for the differents anemometers.
#' 
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
#' @export
#' @examples
#' data("wd10", package = "WindResource")
#' data("wtgData", package = "WindResource")
#' 
#' ## Getting anemometer names
#' wd10$ane$ane.names
#'   
#' ## Calculation of the power density of the anemometers "ane10" and "ane18" from "wd" dataset.
#' power.density(wd10, ane = c("ane10", "ane18"), dist = "np")

power.density <- function(datawd, ane, dist) {
  check.ane(datawd, ane) 
  pw <- c()
  for (i in ane) {
    if (dist == "np") {
      t <- tableWD(datawd, ane = i, type = "histogram")
      t[3] <- t[3] / sum(t[3])
      mc <- c()
      for (j in 1:(length(t[, 1]))) {
        mc <- c(mc, mean(t[j, 1]: t[j, 2]))
      }
      t[4] <- mc
      pw <- c(pw, round(sum(0.5 * 1.225 * (t[4]) ^ 3 * t[3])))
      
    } else {
      fit <- fitWD(datawd, ane = i)
      param1 <- switch(dist, weibull = fit$K, gamma = fit$alpha, lognormal = fit$meanlog)
      param2 <- switch(dist, weibull = fit$A, gamma = fit$beta, lognormal = fit$sdlog)
      pw <- switch(dist, weibull   = c(pw, round(.5 * 1.225 * (param2 ^ 3) * gamma(1 + (3 / param1)))),
                   lognormal = c(pw, round(.5 * 1.225 * exp(3 * param1 + 0.5 * 3 * param2 ^ 2))),
                   gamma     = c(pw, round(.5 * 1.225 * param1 * (param1 + 1) * (param1 + 2) * param2 ^ 2)))  
    }    
  }
  pw <- data.frame(pw)
  row.names(pw) <- ane
  colnames(pw) <- switch(dist, np = "Density(nonparametric) (kw/m2)", paste("Density", "(", dist, ")", " (kw/m2)", sep = ""))
  return(pw)
} 
    
    
    
    
    
    
    
    
  



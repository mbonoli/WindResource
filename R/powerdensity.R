#' @title Conversion of dataframe in class windata
#' 
#' @description
#' Shape a dataframe in a class 'windata'
#' 
#' @details
#' The object windata is a list with the parameters that were mencionated before.
#'   
#' @param datawd a dataframe to be converted
#' @param ane interval of time betwen two registers. Actually, it's only acepted intervals of 1 minut.
#' @param dist the name of the variable that contains the dates of measurements
#'     
#' @return Object of class 'windata' (see details).
#' 
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
#' @export
#' @examples
#' # simple example using the windspeed data set
#' data(wd)
#'  
#' # let's examine windspeed to see the variables' names
#' head(wd)
#' 

powerdensity <- function(datawd, ane, dist) {
  pw <- c()
  for (i in ane) {
    if (dist == "np") {  
      t <- tableWD(datawd, ane = i, type = "histogram")
      t[3] <- t[3]/sum(t[3])
      mc <- c()
      for (j in 1:(length(t[,1]))) { 
        mc <- c(mc, mean(t[j, 1]: t[j, 2])) 
      }
      t[4] <- mc
      pw <- c(pw, round(sum(0.5 * 1.225 * (t[4])^3 * t[3])))    
    } else {
      fit <- fitWD(datawd, ane = i)
      param1 <- switch(dist, weibull = fit$K, gamma = fit$alpha, lognormal = fit$meanlog)
      param2 <- switch(dist, weibull = fit$A, gamma = fit$beta, lognormal = fit$sdlog)
      
      pw <- switch(dist, weibull   = c(pw, round(.5 * 1.225 * (param2^3)*gamma(1+(3/param1)))),
                         lognormal = c(pw, round(.5 * 1.225 * exp(3 * param1 + 0.5 * 3 * param2^2))),
                         gamma     = c(pw, round(.5 * 1.225 * param1 * (param1 + 1) * (param1 + 2) * param2^2)))  
    }    
  }
  pw <- data.frame(pw)
  row.names(pw) <- ane
  colnames(pw) <- switch(dist, np = "Density (no  parametric) (Watt/m2)", paste("Density ", "(", dist, ")", " (Watt/m2)", sep = ""))
  return(pw)
} 
    
    
    
    
    
    
    
    
  



#' @title Annual energy production calculator
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
#' @param datawtg object of class \code{wtgData}.
#' @param ane a vector of character strings with anemometers names.
#' @param model a vector of character strings with the models names of wind turbines.    
#' @param dist a character string 'name' naming a distribution to calculate the annual energy production. 
#'        The possible distrbutions are: \code{"np"} (no parametric distribution), \code{"weibul"}, \code{"gama"} and \code{"lognormal"}. (see details)
#'         
#' @return a data.frame with the results of annual energy production for different wind turbines models and anemometers.
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
#' # Calculate 
#' energy(datawd = wd10, datawtg = wtgData, ane = c("ane10", "ane18"), model = c("E33", "E48"), dist = "weibull")


energy <- function(datawd, datawtg, ane, model, dist = NA) {
  ener.year <- data.frame()
  if (is.na(dist)) {
    for (i in ane) {
      ener <- c()
      wtdata <- datawd$ane[[i]][[1]][!is.na(datawd$ane[[i]][[1]])]
      for (j in model) { 
        wtg <- data.frame(datawtg[[j]][3])
        wtdata[wtdata > max(wtg[1])] <- max(wtg[1])
        wtgt <- mean(wtg[round(wtdata), "data.Power"])
        ener <- c(ener, wtgt * 8760)
      }
      ener.year <- rbind(ener.year, ener)
    }
  } else {
    for (i in ane) {
      ener <- c()
      fit <- fitWD(datawd, ane = i)
      param1 <- switch(dist, weibull = fit$K, gamma = fit$alpha, lognormal = fit$meanlog)
      param2 <- switch(dist, weibull = fit$A, gamma = fit$beta, lognormal = fit$sdlog)
      for (j in model) {
        tp <- datawtg[[j]]$data
        l <- length(tp[,1])
        for (i in 1:(l-1)) {
          tp <- rbind(tp, data.frame(Speed = (tp[i + 1,1] + tp[i,1]) / 2, Power = (tp[i+1,2] + tp[i,2]) / 2, CP = (tp[i+1,3] + tp[i,3]) / 2)) 
        }
        tp <- tp[order(tp[,1]),]
        prob <- switch(dist, weibull = pweibull(tp[,1], param1, param2), gamma = pgamma(tp[,1], param1, param2), lognormal = plnorm(tp[,1], param1, param2))
        probabi <- c()
        powerdif <- c()
        prod <- c()
        for (i in 1:length(prob) - 1) {
          probabi <- c(probabi, prob[i + 1] - prob[i])        
          powerdif <- c(powerdif, (tp[i + 1, 2] + tp[i, 2]) / 2 )
        }
        prod <- probabi * powerdif
        ener <- c(ener, round(8760 * sum(prod))) 
      }
      ener.year <- rbind(ener.year, ener)       
    }        
  }  
  row.names(ener.year) <- ane
  colnames(ener.year) <- paste(model, "(kwh)", sep = " ")
  return(ener.year)       
}




















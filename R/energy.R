#' @title Conversion of dataframe in class windata
#' 
#' @description
#' Shape a dataframe in a class 'windata'
#' 
#' @details
#' The object windata is a list with the parameters that were mencionated before.
#'   
#' @param datawd a dataframe to be converted
#' @param datawtg interval of time betwen two registers. Actually, it's only acepted intervals of 1 minut.
#' @param ane the name of the variable that contains the dates of measurements
#' @param model the admit formats are:    
#' @param dist the admit formats are:
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
          tp <- rbind(tp, data.frame(Speed = (tp[i+1,1] + tp[i,1]) / 2, Power = (tp[i+1,2] + tp[i,2]) / 2, CP = (tp[i+1,3] + tp[i,3]) / 2)) 
        }
        tp <- tp[order(tp[,1]),]
        prob <- switch(dist, weibull = pweibull(tp[,1], param1, param2), gamma = pgamma(tp[,1], param1, param2), lognormal = plnorm(tp[,1], param1, param2))
        probabi <- c()
        powerdif <- c()
        prod <- c()
        for (i in 1:length(prob)-1) {
          probabi <- c(probabi, prob[i+1] - prob[i])        
          powerdif <- c(powerdif, (tp[i+1,2] + tp[i,2]) / 2 )
        }
        prod <- probabi * powerdif
        ener <- c(ener, round(8760*sum(prod))) 
      }
      ener.year <- rbind(ener.year, ener)       
    }        
  }  
  row.names(ener.year) <- ane
  colnames(ener.year) <- paste(model, "(kwh)", sep = " ")
  return(ener.year)       
}




















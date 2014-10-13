#' @title Extracting paramenters
#' 
#' @description
#' Function to extract a parameter value from a list
#' 
#' @param datawd a list of parameters.
#' @param datawtg the parameter value that will be extracted.
#' @param ane the parameter value that will be extracted.
#' @param model the parameter value that will be extracted.
#' @param dist the parameter value that will be extracted.
#' @return The parameter value that was extracted.
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
#' 


power <- function(datawd, datawtg = NA, ane, model, dist = "np") {
  ener.year <- data.frame()
  if (dist == "np") {
    for (i in ane) {
      wtdata <- datawd$ane[[i]][[1]][!is.na(datawd$ane[[i]][[1]])]
      ener.wind <- c()
      for (j in model) {
        A <- switch(j, E33 = 876, E48 = 1810)
        ener.wind <- c(ener.wind, .5*1.225*A*(mean(wtdata)))
      }
      ener.year <- rbind(ener.year, ener.wind)
    }
  }
  else {
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
          ener <- c(ener, 8760*sum(prod))
        }
        ener.year <- rbind(ener.year, ener)       
      }       
    }
  }
  row.names(ener.year) <- ane
  colnames(ener.year) <- model
  return(ener.year)       
}

      
    
  
    















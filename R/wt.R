#' @title Extracting paramenters
#' 
#' @description
#' Function to extract a parameter value from a list
#' 
#' @param center a list of parameters.
#' @param radio the parameter value that will be extracted.
#' @param npoints the parameter value that will be extracted.
#' @return The parameter value that was extracted.
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
#' 

wt <- function(datawd, datawtg, ane, model) {
  #Obter inter de interval minut
  wtdata <- list()
  wtg <- list()
  wtgt <- c()
  inter <- datawd$interval.minutes
  for (i in ane) {
    wtdata[[i]] <- datawd$ane[[i]][[1]][!is.na(datawd$ane[[i]][[1]])]
    for (j in model) {
      #porc <- length(wtdata[[i]])/(525600/inter) 
      wtg <- data.frame(datawtg[[j]][3])
#       print((wtg))
      wtgt <- c(wtgt, mean(wtg[round(wtdata[[i]]), "data.Power"]))
    }
    wtg <- wtgt * 8760
  }
  return(wtgt)
}

#Cálculo de la energía anual a partir de la distribución elegida
# wt2 <- function (datawd, datawtg, ane, model, dist) {
#   tp <- datawtg[[model]]$data
#   fit <- fitWD(datawd, ane = ane)
#   param1 <- switch(dist, weibull = fit$K, gamma = fit$alpha, lognormal = fit$meanlog)
#   param2 <- switch(dist, weibull = fit$A, gamma = fit$beta, lognormal = fit$sdlog)
#   prob <- switch(dist, weibull = pweibull(tp[,1], param1, param2), gamma = pgamma(tp[,1], param1, param2), lognormal = plnorm(tp[,1], param1, param2))
#   l <- length(tp[,1])
#   for (i in 1:length(l)-1) {
#     tp <- rbind(tp, data.frame(Speed = (tp[i+1,1] + tp[i,1]) / 2, Power = (tp[i+1,2] + tp[i,2]) / 2, CP = (tp[i+1,3] + tp[i,3]) / 2)) 
#   }
#   tp <- tp[order(tp[,1]),]
#   print(tp)
#   probabi <- c()
#   powerdif <- c()
#   prod <- 0
#   for (i in 1:length(prob)-1) {
#     probabi <- c(probabi, prob[i+1] - prob[i])
#     powerdif <- c(powerdif, (tp[i+1,2] + tp[i,2]) / 2 )   
#   }
#   print(probabi)
#   print(powerdif)
#   enerAnual <- 8760*sum(prod)
#   return(enerAnual)
# }
#' @title Fit data to selected distribution.
#' 
#' @description
#' Fit of univariate distribution by maximum likelihood.
#' 
#' @details
#' The "name" of distribution must be one of "Weibull", "Gamma", "Lognormal".
#' 
#' @param data object of class wd.
#' @param ane a vector with the anemometers'name to study. By default "ane" takes the first anemometer.
#' @param dist character string "name" naming a distribution to be used (see details).
#' @return 
#' A list with the following components:
#' -aic: Akaike Index Coefficient.
#' 
#' -loglik: maximun likelihood.
#' 
#' -parameters: a list with the following components:
#'  name: parameter name
#'  type: parameter type
#'  value: estimated parameter
#'  sd: standard desvest of estimated parameter
#' 
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
#' @export
#' @examples
#' # simple example using the wd class
#' # list_para <- fitWd(Wd, ane=1, dist="weibull")


# Function to estimate parameters of differents distributions
fitWd <- function(data, ane = NA, dist = c("weibull", "gamma", "lognormal")) {
  result <- list()
  result$data <- data
  result$dist <- dist
  if (dist == "weibull") {
    fit_w <- fitdist(data, "weibull")
    result$loglik <- fit_w$loglik
    result$aic <- fit_w$aic
    result$par1 <- list(name = "k", type = "shape", value = as.numeric(fit_w$estimate[1]), sd = as.numeric(fit_w$sd[1]))
    result$par2 <- list(name = "A", type = "scale", value = as.numeric(fit_w$estimate[2]), sd = as.numeric(fit_w$sd[2]))
  }
  if (dist == "gamma") {
    fit_w <- fitdist(data, "gamma")
    result$loglik <- fit_w$loglik
    result$aic <- fit_w$aic
    result$par1 <- list(name = "alpha", type = "shape", value = as.numeric(fit_w$estimate[1]), sd = as.numeric(fit_w$sd[1]))
    result$par2 <- list(name = "beta", type = "scale", value = 1/as.numeric(fit_w$estimate[2]), sd = as.numeric(fit_w$sd[2]))
  }
  if (dist == "lnorm") {
    fit_w <- fitdist(data, "lnorm")
    result$loglik <- fit_w$loglik
    result$aic <- fit_w$aic
    result$par1 <- list(name = "meanlog", type = "shape", value = as.numeric(fit_w$estimate[1]), sd = as.numeric(fit_w$sd[1]))
    result$par2 <- list(name = "sdlog", type = "shape", value = as.numeric(fit_w$estimate[2]), sd = as.numeric(fit_w$sd[2]))
  }
  
  result
}

            
                          
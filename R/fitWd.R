#' @title Fit data to selected distribution.
#' 
#' @description
#' Fit of univariate distribution by maximum likelihood.
#' 
#' @details
#' The 'name' of distribution must be one of 'Weibull', 'Gamma', 'Lognormal'.
#' 
#' @param data object of class wd.
#' @param ane a vector with the anemometers'name to study. By default 'ane' takes the first anemometer.
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
#' @importFrom fitdistrplus fitdist
#' @examples
#' # simple example using the wd class
#' # list_para <- fitWd(Wd, ane=1, dist='weibull')


# Function to estimate parameters of differents distributions
fitWD <- function(data, ane) {
    
    # if (is.na(ane)[i]) {ane<-names(data$ane)[i]}
    dat <- data$ane[[ane]]$ave
    dat <- dat[!is.na(dat)]
    dat <- dat[dat != 0]
    
    
    if (length(dat) > 5000) {
        vec <- seq(0, length(dat), floor(length(dat)/5000))
        dat <- dat[vec]
    } else {
        dat <- dat
    }
    
    wd.wei <- fitdist(dat, distr = "weibull")
    wd.gam <- fitdist(dat, distr = "gamma")
    wd.ln <- fitdist(dat, distr = "lnorm")
    
    
    result <- list()
    
    result$data <- dat
    
    result$K <- wd.wei$estimate[[1]]
    result$A <- wd.wei$estimate[[2]]
    result$loglik.wei <- wd.wei$loglik
    result$aic.wei <- wd.wei$aic
    
    result$alpha <- wd.gam$estimate[[1]]
    result$beta <- 1/wd.gam$estimate[[2]]
    result$loglik.ga <- wd.gam$loglik
    result$aic.ga <- wd.gam$aic
    
    result$meanlog <- wd.ln$estimate[[1]]
    result$sdlog <- wd.ln$estimate[[2]]
    result$loglik.ln <- wd.ln$loglik
    result$aic.ln <- wd.ln$aic
    
    result
    
} 

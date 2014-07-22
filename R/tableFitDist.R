#' @title Table to compare differents statisticals fit
#' 
#' @description
#' Function to determinate the  parameters' values and the measures of fit
#' for differents statistical models
#' 
#' @details
#' This function allows you to get the adjustments of the next distributions:
#' Weibull, Gamma and Lognormal. 
#' 
#' @param data object of class windata
#' @param ane a vector with the anemometers'name to study. By default 'ane' takes the first anemometer
#' @return Returns a table with the parameters listed before. 
#' 
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
#' @export
#' @import fitdistrplus
#' @examples           
#' #Execute the function
#' #tableFitDist(Wd, ane=1)

tableFitDist <- function(data, ane = NA) {
    
    if (class(data) != "windata") 
        stop("The data isn't corresponded with the class 'windata'.")
    
    if (is.na(ane)[1]) {
        ane <- names(data$ane)[1]
    }
    
    data <- data$ane[[ane]]$ave
    data <- data[!is.na(data)]
    data <- data[data != 0]
    vec <- seq(0, length(data), floor(length(data)/20000))
    data <- data[vec]
    
    # Estimating parameters for each distribution
    a.wei <- fitWd(data, dist = "weibull")
    a.gamma <- fitWd(data, dist = "gamma")
    a.ln <- fitWd(data, dist = "lnorm")
    
    # Taking parameters from each distribution
    K <- getParam(a.wei, "k")
    A <- getParam(a.wei, "A")
    alpha <- getParam(a.gamma, "alpha")
    beta <- getParam(a.gamma, "beta")
    m <- getParam(a.ln, "meanlog")
    D <- getParam(a.ln, "sdlog")
    
    # Building the table
    t <- data.frame(cbind(Parameter1 = c(paste("k=", round(K, digits = 4)), paste("alpha=", 
        round(alpha, digits = 4)), paste("m=", round(m, digits = 4))), Parameter2 = c(paste("C=", 
        round(A, digits = 4)), paste("beta=", round(beta, digits = 4)), paste("D=", 
        round(D, digits = 4))), loglik = c(round(a.wei$loglik, digits = 4), round(a.gamma$loglik, 
        digits = 4), round(a.ln$loglik, digits = 4)), aic = c(round(a.wei$aic, 
        digits = 4), round(a.gamma$aic, digits = 4), round(a.ln$aic, digits = 4))), 
        row.names = c("Weibull", "Gamma", "Lognormal"))
    
    return(t)
    
} 

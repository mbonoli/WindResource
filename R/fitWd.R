#' @title Fit differents probability distributions
#' 
#' @description
#' Fit by maximum likelihood Weibull, Gamma and Lognormal distributions.
#' 
#' @param datawd object of class \code{windata}.
#' @param ane one character strings with anemometer name to fit.
#' @return 
#' A list with the following components:
#' -aic: Akaike Index Coefficient.
#' 
#' -loglik: maximun likelihood.
#' 
#' -parameters: a list with the following components:
#'  data: data to estimate.
#'  name: parameter name.
#'  type: parameter type.
#'  value: estimated parameter.
#'  sd: standard desvest of estimated parameter.
#' 
#' @author Mariano Bonoli Escobar, Diego Edwards, Valeria Gogni, Ruben Bufanio
#' @export
#' @importFrom fitdistrplus fitdist
#' @examples
#' data("wd", package = "WindResource")
#' 
#' ## Getting anemometer names
#' wd$ane$ane.names
#' 
#' ## Simple example using the wd class
#' fitWD(wd, ane = "ane10")


# Function to estimate parameters of differents distributions
fitWD <- function(datawd, ane) {
  check.ane(datawd, ane) 
  dat <- datawd$ane[[ane]]$ave
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

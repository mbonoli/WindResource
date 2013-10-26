#' @title Plot to compare distribution
#' 
#' @description
#' Graphical function to determite the best fit of the 
#' variable speed wind
#' 
#' @details
#' This function allows you to get the adjustments of the next distributions:
#' Weibull, Gamma and Lognormal.
#' It shows histograms (with density functions) and qq-plots graphics. 
#' Also, it delivers a table with the different values for each distribution (see "Value")
#' 
#' @param data object of class wd.
#' @param ane a vector with the anemometers'name to study. By default "ane" takes the first anemometer.
#' @return Histogram of each distribution with its corresponding density function.
#' QQ plot of each ditribution.
#' A table with the following columns:
#' -parameter 1: the first parameter estimated for each distribution.
#' -parameter 2: the second parameter estimated for each distribution.
#' -aic: Akaike Index Coefficient.
#' -loglik: maximun likelihood.
#' 
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
#' @export
#' @import fitdistrplus
#' @import grid
#' @import ggplot2
#' @examples           
#' #Execute the function
#' #plotDistWd(Wd, ane=1)


# Built histograms and qq-plots to choose the best fit
plotDistWd <- function(data, ane = NA) {
  
  if (class(data) != "windata") stop("Los datos no correponden a la clase 'windata'.")
  if (is.na(ane)[1]) {ane <- names(data$ane)[1]}
  
  data <- data$ane[[ane]]$ave
    
  data <- data[!is.na(data)]
  
  data <- data[data != 0] 
  
  vec <- seq(0, length(data), floor(length(data) / 20000))
  data <- data[vec]
    
  # Estimating parameters for each distribution
  #browser()
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
  
  # Histogram graphics
  data <- c(0, data)
  browser()
  y.wei <- dweibull(data, shape = K, scale = A)
  y.ga <- dgamma(data, shape = alpha, scale = beta)
  y.ln <- dlnorm(data, meanlog = m, sdlog = D)
  
  distr <- c("wei", "ga", "ln")
  
  tittle <- c("Weibull Distribution", "Gamma Distribution", "Lognormal Distribution")
  
  namep1 <- c("K", "alpha", "m")
  para1 <- c(K, alpha, m)
  
  namep2 <- c("A", "beta", "D")
  para2 <- c(A, beta, D)
  
  lo <- c(a.wei$loglik, a.gamma$loglik, a.ln$loglik)
  
  dr <- rbind(data.frame(dist = "wei", speed = data, ajust = y.wei), data.frame(dist = "ga", speed = data, ajust = y.ga), data.frame(dist = "ln", speed = data, ajust = y.ln))
#   dw <- rbind(data.frame(dist = "wei", speed = data, ajust = y.wei))
#   dg <- rbind(data.frame(dist = "ga", speed = data, ajust = y.ga))
#   dl <- rbind(data.frame(dist = "ln", speed = data, ajust = y.ln))
  
  
  
  
#   p1 <- ggplot(dw, aes(x = speed)) + geom_histogram(aes(y = ..density..), binwidth = .5, colour = "black", fill = "blue") + geom_line(aes(y = ajust), size=0.8) 
#   p2 <- ggplot(dg, aes(x = speed)) + geom_histogram(aes(y = ..density..), binwidth = .5, colour = "black", fill = "blue") + geom_line(aes(y = ajust), size=0.8) 
#   p3 <- ggplot(dl, aes(x = speed)) + geom_histogram(aes(y = ..density..), binwidth = .5, colour = "black", fill = "blue") + geom_line(aes(y = ajust), size=0.8) 
#   
#   multiplot(p1, p2, p3, cols=3, names = NULL)
  
#   ggplot(dr, aes(x = speed)) + geom_histogram(aes(y = ..density..), binwidth = .5, colour = "black", fill = "blue") + geom_line(aes(y = ajust), size=0.8) +
#   facet_wrap( ~ dist, ncol = 3, drop = F)
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 3)))  
  vplayout <- function(x, y) viewport(layout.pos.col = x, layout.pos.row = y)
  
  for (i in 1:3) {
    print(ggplot(dr[dr$dist == distr[i], ], aes(x = speed)) + geom_histogram(aes(y = ..density..), binwidth = .5, colour = "black", fill = "blue") +
            ggtitle(tittle[i]) + geom_line(aes(y = ajust), colour = "red", size = 1) + annotate("text", x=max(data)*0.8, y=0.10, label = paste(namep1[i], " = ", round(para1[i], digits = 4))) + 
            annotate("text", x=max(data)*0.8, y=0.09, label = paste(namep2[i], " = ", round(para2[i], digits = 4))) + annotate("text", x=max(data)*0.8, y=0.08, label=paste("Loglik =", round(lo[i], digits = 4))),
            vp = vplayout(rep(1:3)[i], 1))
  
  }
  
  # QQ-plots graphics
  #browser()
  
  
  params.wei <- list(shape = a.wei$par1[[3]], scale = a.wei$par2[[3]])
  params.ga <- list(shape = a.gamma$par1[[3]], scale = a.gamma$par2[[3]])
  params.ln <- list(meanlog = a.ln$par1[[3]], sdlog = a.ln$par2[[3]])
 
  print(ggplot(dr, aes(sample = speed)) + stat_qq(distribution = qweibull, dparams = params.wei), vp = vplayout(1,2))
  print(ggplot(dr, aes(sample = speed)) + stat_qq(distribution = qgamma, dparams = params.ga), vp = vplayout(2,2))
  print(ggplot(dr, aes(sample = speed)) + stat_qq(distribution = qlnorm, dparams = params.ln), vp = vplayout(3,2))

}




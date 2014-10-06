#' @title Conversion of dataframe in class windata
#' 
#' @description
#' Shape a dataframe in a class 'windata'
#' 
#' @details
#' The object windata is a list with the parameters that were mencionated before.
#'   
#' @param wd a dataframe to be converted
#' @param Ns interval of time betwen two registers. Actually, it's only acepted intervals of 1 minut.
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

gen10m <- function(wd, Ns = 60) {
  
  if (class(wd) != "windata") 
    stop("Los datos no correponden a la clase 'windata'.")
  
  if  (wd[["interval.minutes"]] == 10){
    stop("Ya son de 10 minutos los intervalos ")
  } else if (wd[["interval.minutes"]] > 10 ){
    stop("No es posible convertir series con intervalos mayores a 10 minutos")
  }
  
  df <- data.frame()
  for (i in wd$ane$ane.names) {
    df <- rbind(df, data.frame(year = wd$time$year, 
                               month = wd$time$month, day = wd$time$day, hour = wd$time$hour, minute = wd$time$minute, 
                               dt = wd$time$dt, Q = wd$time$Q, ave = wd[["ane"]][[i]]$ave, min = wd[["ane"]][[i]]$min, 
                               max = wd[["ane"]][[i]]$max, sd = wd[["ane"]][[i]]$sd, ane = i, 
                               min10 = floor(wd$time$minute/10) + 1, count = 1, dir = wd[["ane"]][[i]]$dir))
  }
  
  d <- data.table(df)
  d10 <- d[,list(ave10=mean(ave),dt=min(dt), Nk=length(ave),min= min(min), max=max(max), dir=mean(dir), minute=min(minute)),by=c("year" , "month","day" ,"hour" , "min10", "ane")]
  
  # Agrego la media y Nk al dataframe original
  setkeyv(d,c("year" , "month","day" ,"hour" , "min10", "ane"))
  setkeyv(d10,c("year" , "month","day" ,"hour" , "min10", "ane"))
  d <- merge(d, d10[,list(year , month,day,hour,min10,ane, ave10,Nk)],all.x=T)
  
  d$varaux <- (Ns * (d$ave - d$ave10)^2 + (Ns - 1) * (d$sd)^2)/(d$Nk * Ns - 1)
  d10sd <- d[,list(sd=sqrt(sum(varaux))),by=c("year" , "month","day" ,"hour" , "min10","ane")]
  d10 <- merge(d10,d10sd)
  setnames(d10,"ave10","ave")
  
  rose_dir <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", 
                "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  d10$dir[which(d10$dir >= 360)] <- d10$dir[which(d10$dir >= 360)] - 360
  d10$sect_12 <- floor(d10$dir/(360/12)) + 1
  d10$ang_12 = (floor(d10$dir/(360/12)) + 1) * (360/12)
  d10$sect_16 = floor(d10$dir/(360/16)) + 1
  d10$ang_16 <- (floor(d10$dir/(360/16)) + 1) * (360/16)
  d10$rose = factor(rose_dir[floor(d10$dir/(360/16)) + 1], levels = rose_dir)
  
  result <- list()
  d10 <- as.data.frame(d10) 
  result[["name"]] <- wd[["name"]]
  result$time <- d10[d10$ane == wd$ane$ane.names[1], c("dt", "year", "month", "day", "hour", "minute")]
  result[["interval.minutes"]] <- 10
  result[["ane"]][["height"]] <- wd[["ane"]][["height"]]
  result[["ane"]][["nane"]] <- wd[["ane"]][["nane"]]
  result[["ane"]][["ane.names"]] <- wd[["ane"]][["ane.names"]]
# browser()  
  
  for (i in wd$ane$ane.names) {
    result[["ane"]][[i]] <- d10[d10$ane == i, c("ave", "min", "max", "sd","dir","sect_12","ang_12","sect_16","ang_16","rose")]
  }
  
  class(result) <- "windata"
  
  result
} 

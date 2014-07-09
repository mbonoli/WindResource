#' @title Conversion of dataframe in class windata
#' 
#' @description
#' Shape a dataframe in a class 'windata'
#' 
#' @details
#' The object windata is a list with the parameters that were mencionated before.
#'   
#' @param data a dataframe to be converted
#' @param interval interval of time betwen two registers. Actually, it's only acepted intervals of 1 minut.
#' @param date.var the name of the variable that contains the dates of measurements
#' @param date.format the admit formats are:
#'        - "YYYYMMDD","YYYY-MM-DD","YYYY.MM.DD" or "DD/MM/YYYY"       
#' @param time.var the name of the variable that contains the times of measurements
#' @param time.format the admit formats are:
#'        - "HHMM","HHMMSS","HH:MM","HH:MM:SS","HH.MM" or "HH.MM.SS"
#' @param ane.names the names to indicate the differents anemometers in the list.
#' @param ane.height the heights of the anemometers 
#' @param speed.ave.var the name/s of the variable/s that contains the average speeds measures.
#' @param speed.min.var the name/s of the variable/s that contains the min speeds measures.
#' @param speed.max.var the name/s of the variable/s that contains the max speeds measures.
#' @param speed.sd.var the name/s of the variable/s that contains the desvest standar of speeds measures.
#' @param speed.var.var the name/s of the variable/s that contains the variance of speeds measures.
#' @param speed.unit unit wind speed. At the moment, the unit that can be use is "m/s".  
#' @param dir.var the name/s of the variable/s that contains the wind's direction measures
#' @param dir.unit unit wind's directions. At the moment, the unit that can be used is "deg".
#' @param temp.var the name/s of the variable/s that contains the wind's temperature measures
#' @param temp.unit unit wind's temperatures. At the moment, the unit that can be used is "deg".
#' @param pres.var he name/s of the variable/s that contains the wind's atmospheric pressures measures.
#' @param pres.unit unit wind's atmospheric pressures. At the moment, the unit that can be used is "deg".
#' @return Object of class "windata" (see details).
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

gen10m <- function (wd, Ns=60) {
  
  # HECER ESTA CORRECCION
  # wd10$dir$ang_16 <- (wd10$dir$sect_16-1)*22.5
  require(plyr)
  df<-data.frame()
  for (i in 1:(wd$ane$nane)){
    df <- rbind(df, data.frame(
      date=wd$data$date, 
      time=wd$data$time,
      year=wd$time$year, month=wd$time$month, day=wd$time$day, 
      hour=wd$time$hour, minute=wd$time$minute, dt=wd$time$dt, Q=wd$time$Q,
      ave=wd$ane[[i]]$ave, min=wd$ane[[i]]$min, max=wd$ane[[i]]$max, sd=wd$ane[[i]]$sd,
      ane=names(wd$ane)[i],
      min10 = floor(wd$time$minute/10)+1,
      count = 1,
      dir = wd$dir$value)
    )
  }
  
  df.ave <- aggregate(ave ~ ane + year + month + day + hour + min10, data = df, mean)
  df.dt <- aggregate(dt ~ ane + year + month + day + hour + min10, data = df, min)
  df.Nk <- aggregate(ave ~ ane + year + month + day + hour + min10, data = df, length)
  df.min <- aggregate(min ~ ane + year + month + day + hour + min10, data = df, min)
  df.max <- aggregate(max ~ ane + year + month + day + hour + min10, data = df, max)
  df.dir <- aggregate(dir ~ ane + year + month + day + hour + min10, data = df, mean)
  
  colnames(df.Nk)[7] <- "Nk"
  colnames(df.ave)[7] <- "ave10"
  .df <- join_all(list(df, df.ave, df.Nk), type = "left")
  .df$var <- (Ns*(.df$ave - .df$ave10)^2+(Ns-1)*(.df$sd)^2)/(.df$Nk*Ns-1)
  df.sd <- aggregate(var ~ ane + year + month + day + hour + min10, data = .df, sum)
  df.sd$sd <- sqrt(df.sd$var)
  
  df <- join_all(list(df.dt, df.ave, df.min, df.max,df.sd, df.dir, df.Nk), type = "left")
  df$minute <- (df$min10-1)*10
  df<- df[order(df$ane, df$year, df$month, df$day, df$hour, df$minute),]
  rownames(df)<-NULL  
  colnames(df)[which(colnames(df)=="ave10")] <- "ave"
  
  result <- list()
  ane.names <- unique(df$ane)
  result$time <- df[df$ane==ane.names[1],c( "dt", "year", "month", "day", "hour", "minute")]
  result$ane <- split(df[,c("ave", "min", "max", "sd")],df$ane)
  result$dir <- data.frame(value=df$dir,
                           sect_12=dir.sector(df$dir,12),
                           ang_12=dir.ang(df$dir,12),
                           sect_16=dir.sector(df$dir,16),
                           ang_16=dir.ang(df$dir,16),
                           rose=dir.rose(df$dir)
  )
  result$info <- wd$info
  class(result) <- "windata"
  
  result
}
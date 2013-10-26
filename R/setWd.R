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
#' data(windspeed)
#'  
#' # let's examine windspeed to see the variables' names
#' head(data)
#' 
#' # create the object wd of class "windata"
#' Wd <- setWd(data = data, 
#'              interval = 1,
#'              date.var = "date", 
#'              date.format = "DD/MM/YYYY", 
#'              time.var = "time", 
#'              time.format = "HH:MM",
#'              ane.names = c("Ane1", "Ane2"),
#'              ane.height = c(9, 18),
#'              speed.ave.var = c("ave", "ave2"),
#'              speed.min.var = c("min", "min2"),
#'              speed.max.var = c("max", "max2"),
#'              speed.sd.var = c("sd", "sd2"),
#'              speed.var.var = c(NA, NA),
#'              speed.unit = "m/s",
#'              dir.var = "dir",
#'              dir.unit = "deg",
#'              temp.var = NA,
#'              temp.unit = "C",
#'              pres.var = NA,
#'              pres.unit = "bar")
#' 
#' # let's examine wd
#' str(Wd)


setWd <-function (data, 
                  interval = 1,
                  date.var, 
                  date.format = c("YYYYMMDD","YYYY-MM-DD","YYYY.MM.DD","DD/MM/YYYY"), 
                  time.var, 
                  time.format = c("HHMM","HHMMSS","HH:MM","HH:MM:SS","HH.MM","HH.MM.SS"),
                  ane.names,
                  ane.height,
                  speed.ave.var,
                  speed.min.var = NA,
                  speed.max.var = NA,
                  speed.sd.var = NA,
                  speed.var.var = NA,
                  speed.unit = NA,
                  dir.var,
                  dir.unit = "deg",
                  temp.var = NA,
                  temp.unit = "C",
                  pres.var = NA,
                  pres.unit = "bar") {
     
    result <- list()
    
    print("The process has begun")
    cat(paste("Number of registers: ", dim(data)[1], sep = ""))
    
    # Cleaning
    empty.rec <- which(is.na(data[, date.var]) | is.na(data[, time.var]))
    n.empty.rec <- length(empty.rec)
    if (n.empty.rec == 0) {cat ("The registers has information about date and hour")}
    else {cat (paste ("\n",n.empty.rec,"The registers without date/hour were eliminated. See $cleaning$rwd", sep = ""))}
    result$cleaning$rwd <- empty.rec
    data <- data[!is.na(data[, date.var]) & !is.na(data[, time.var]), ]
    
    # $data
    if  (sum(colnames(data) == date.var) == 0) stop (paste("The name of the column date.var = ", date.var," not exist.", sep = ""))
    if  (sum(colnames(data) == time.var) == 0) stop (paste("The name of the column time.var = ",time.var," not exist.",sep = ""))
    result$data <- data.frame(date = data[ ,date.var], time = data[ ,time.var])
      
    # $time
    if (date.format == "YYYYMMDD") {
      result$time <- data.frame(year = as.numeric(substring(result$data$date, 1, 4)),
                                month = as.numeric(substring(result$data$date, 5, 6)),
                                day = as.numeric(substring(result$data$date, 7, 8)))
    }
    else if (date.format == "YYYY-MM-DD" | date.format=="YYYY.MM.DD") {
      result$time <- data.frame(year = as.numeric(substring(result$data$date, 1, 4)),
                                month = as.numeric(substring(result$data$date, 6, 7)),
                                day = as.numeric(substring(result$data$date, 9, 10)))   
    }
    else if (date.format == "DD/MM/YYYY") {
      result$time <- data.frame(year = as.numeric(substring(result$data$date, 7, 10)),
                                month = as.numeric(substring(result$data$date, 4, 5)),
                                day = as.numeric(substring(result$data$date, 1, 2)))      
    }
    else {stop ("The format isn't admit")}
    
    if (time.format == "HHMM" | time.format == "HHMMSS") {
      result$time$hour <-  as.numeric(substring(pref0(result$data$time, 4), 1, 2))
      result$time$minute <- as.numeric(substring(pref0(result$data$time, 4), 3, 4))
      
    }
    else if (time.format == "HH:MM" | time.format == "HH:MM:SS" | time.format == "HH.MM" | time.format == "HH.MM.SS") {
      result$time$hour <-  as.numeric(substring(pref0(result$data$time, 5), 1, 2))
      result$time$minute <- as.numeric(substring(pref0(result$data$time, 5), 4, 5))
    }
    else {stop ("The format isn't admit")}
    result$time$dt <- as.POSIXct(paste(result$time$year,".",pref0(result$time$month,2),".",pref0(result$time$day,2)," ",pref0(result$time$hour,2),":",pref0(result$time$Min,2),":00",sep=""), "%Y.%m.%d %H:%M:%S", tz="")
    result$time$Q <- floor((result$time$month/3) - .01) + 1
    
    # $speed  
    if (is.na(ane.names)[1]) ane.names = "ane1"
    result$nane <- length(ane.names)
    
    result$ane <- list()
    for (i in ane.names) {
      j <- which(ane.names == i, arr.ind = T)
      result$ane[[i]] <- data.frame(ave = data[, speed.ave.var[j]])
      if (!is.na(speed.min.var[j])) {result$ane[[i]]$min <- data[ , speed.min.var[j]]}
      if (!is.na(speed.max.var[j])) {result$ane[[i]]$max <- data[ ,speed.max.var[j]]}
      if (!is.na(speed.var.var[j])) {result$ane[[i]]$sd <- sqrt(data[ ,speed.var.var[j]])}
      if (!is.na(speed.sd.var[j])) {result$ane[[i]]$sd <- data[ ,speed.sd.var[j]]}
    }
    
    # $dir
    dir <- data[ ,dir.var] + (360/12)/2
    dir[which(dir >= 360)] <- dir[which(dir>=360)] - 360
    rose_dir <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
    result$dir <- data.frame (value = data[ ,dir.var],
                              sect_12 = floor(dir/(360/12)) + 1,
                              ang_12 = ((floor(dir/(360/12)) + 1) - 1) * 360/12,
                              sect_16 = floor(dir/(360/16)) + 1,
                              ang_16 = (floor(dir/(360/16)) + 1) * (360/16),
                              rose=factor(rose_dir[floor(dir/(360/16)) + 1],
                              levels = rose_dir)
    )
    
    # $par
    if (!is.na(temp.var)) {
      result$par$temp$value <- data[ ,temp.var]
      result$par$temp$unit <- temp.unit
    }
    if (!is.na(pres.var)) {
      result$par$pres$value <- data[ ,pres.var]
      result$par$pres$unit <- pres.unit
    }
    result$info$interval <- interval
    result$info$unit <- data.frame(speed = speed.unit, dir = dir.unit, temp = temp.unit, pres = pres.unit) 
    result$info$ane$nane <- length(ane.names)
    result$info$ane$ane.names <- ane.names
    result$info$ane$height <- ane.height
    
    class(result) <- "windata"
    
    result
  }
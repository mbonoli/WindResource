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
#'        - 'YYYYMMDD','YYYY-MM-DD','YYYY.MM.DD' or 'DD/MM/YYYY'       
#' @param time.var the name of the variable that contains the times of measurements
#' @param time.format the admit formats are:
#'        - 'HHMM','HHMMSS','HH:MM','HH:MM:SS','HH.MM' or 'HH.MM.SS'
#' @param ane.names the names to indicate the differents anemometers in the list.
#' @param ane.height the heights of the anemometers 
#' @param speed.ave.var the name/s of the variable/s that contains the average speeds measures.
#' @param speed.min.var the name/s of the variable/s that contains the min speeds measures.
#' @param speed.max.var the name/s of the variable/s that contains the max speeds measures.
#' @param speed.sd.var the name/s of the variable/s that contains the desvest standar of speeds measures.
#' @param speed.var.var the name/s of the variable/s that contains the variance of speeds measures.
#' @param speed.unit unit wind speed. At the moment, the unit that can be use is 'm/s'.  
#' @param dir.var the name/s of the variable/s that contains the wind's direction measures
#' @param dir.unit unit wind's directions. At the moment, the unit that can be used is 'deg'.
#' @param temp.var the name/s of the variable/s that contains the wind's temperature measures
#' @param temp.unit unit wind's temperatures. At the moment, the unit that can be used is 'deg'.
#' @param pres.var he name/s of the variable/s that contains the wind's atmospheric pressures measures.
#' @param pres.unit unit wind's atmospheric pressures. At the moment, the unit that can be used is 'deg'.
#' @return Object of class 'windata' (see details).
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

plotwindserie <- function(wd, year, month, vars = c("Ave", "Min", "Max", "Temp", 
    "Pres", "Dir"), axis = c("Ave", "Min", "Max", "Temp", "Pres", "Dir"), shiny = F) {
    
    if (sum(wd$time$year == year & wd$time$month == month) == 0) 
        stop("No existe el año y mes seleccionado")
    require(googleVis)
    data <- data.frame(dt = as.POSIXct(NA), val = NA, type = NA)
    
    colorlines <- "["
    # 'blue', 'lightblue', 'lightblue', 'red']'
    if ("Ave" %in% vars) {
        data <- rbind(data, data.frame(dt = wd$time$dt, val = wd[["ane"]]$Ane1$ave, 
            type = "Ave")[wd$time$year == year & wd$time$month == month, ])
        colorlines <- paste(colorlines, ifelse(nchar(colorlines) == 1, "", ","), 
            " 'blue'", sep = "")
    }
    if ("Min" %in% vars) {
        data <- rbind(data, data.frame(dt = wd$time$dt, val = wd[["ane"]]$Ane1$min, 
            type = "Min")[wd$time$year == year & wd$time$month == month, ])
        colorlines <- paste(colorlines, ifelse(nchar(colorlines) == 1, "", ","), 
            " 'lightgray'", sep = "")
    }
    if ("Max" %in% vars) {
        data <- rbind(data, data.frame(dt = wd$time$dt, val = wd[["ane"]]$Ane1$max, 
            type = "Max")[wd$time$year == year & wd$time$month == month, ])
        colorlines <- paste(colorlines, ifelse(nchar(colorlines) == 1, "", ","), 
            " 'lightgray'", sep = "")
    }
    if ("Temp" %in% vars) {
        data <- rbind(data, data.frame(dt = wd$time$dt, val = wd$par$temp$value, 
            type = "Temp")[wd$time$year == year & wd$time$month == month, ])
        colorlines <- paste(colorlines, ifelse(nchar(colorlines) == 1, "", ","), 
            " 'green'", sep = "")
    }
    if ("Pres" %in% vars) {
        data <- rbind(data, data.frame(dt = wd$time$dt, val = wd$par$pres$value, 
            type = "Pres")[wd$time$year == year & wd$time$month == month, ])
        colorlines <- paste(colorlines, ifelse(nchar(colorlines) == 1, "", ","), 
            " 'lightgreen'", sep = "")
    }
    # if ('Dir' %in% vars) { data<-
    # rbind(data,data.frame(dt=wd$time$dt,val=wd$dir$value,type='Dir')[wd$time$year==year
    # & wd$time$month==month,])
    # colorlines<-paste(colorlines,ifelse(nchar(colorlines)==1,'',','),'
    # 'black'',sep='') }
    colorlines <- paste(colorlines, "]")
    
    # Borro registros sin datos
    data <- data[!is.na(data$dt), ]
    # data<-data[!is.na(data$val),]
    
    # esto es para que las escalas de las 3 velocidades sea la misma. Se modifica
    # el primer registro de la serie
    max <- max(data[, "val"], na.rm = TRUE)
    data[data$type == "Min", ][1, 2] <- max
    data[data$type == "Ave", ][1, 2] <- max
    
    if (length(axis) == 1) {
        scalecol <- paste("[", which(vars == axis[1]) - 1, "]", sep = "")
    } else {
        scalecol <- paste("[", which(vars == axis[1]) - 1, ",", which(vars == axis[2]) - 
            1, "]", sep = "")
    }
    if (shiny == T) {
        gvisAnnotatedTimeLine(data, datevar = "dt", numvar = "val", idvar = "type")
    } else {
        dataplot <- gvisAnnotatedTimeLine(data, datevar = "dt", numvar = "val", 
            idvar = "type")
        plot(dataplot)
    }
} 

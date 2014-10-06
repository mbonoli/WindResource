#' @title Conversion of dataframe in class windata
#' 
#' @description
#' Shape a dataframe in a class 'windata'
#' 
#' @details
#' The object windata is a list with the parameters that were mencionated before.
#'   
#' @param wdata a dataframe to be converted
#' @param var interval of time betwen two registers. Actually, it's only acepted intervals of 1 minut.
#' @param ane the name of the variable that contains the dates of measurements
#' @param year the admit formats are:
#'        - 'YYYYMMDD','YYYY-MM-DD','YYYY.MM.DD' or 'DD/MM/YYYY'       
#' @param month the name of the variable that contains the times of measurements
#' @param axis the admit formats are:
#'        - 'HHMM','HHMMSS','HH:MM','HH:MM:SS','HH.MM' or 'HH.MM.SS'
#' @param shiny the names to indicate the differents anemometers in the list.
#' @return Object of class 'windata' (see details).
#' 
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
#' @export
#' @examples
#' # simple example using the windspeed data set
#' data(data)
#'  
#' # let's examine windspeed to see the variables' names
#' head(data)
#' 

plotwindserie <- function(wdata, year, month, ane, 
                          var = c("Ave", "Min", "Max", "Temp", "Pres", "Dir"), 
                          axis = c("Ave", "Min", "Max", "Temp", "Pres", "Dir"), shiny = F) {
  
  if (sum(wdata$time$year == year & wdata$time$month == month) == 0) 
    stop("No existe el anio y mes seleccionado")

  data <- data.frame(dt = as.POSIXct(NA), val = NA, type = NA)
  
  colorlines <- "["
  # 'blue', 'lightblue', 'lightblue', 'red']'
  if ("ave" %in% var) {
    data <- rbind(data, data.frame(dt = wdata$time$dt, 
                                   val = wdata[["ane"]][[ane]]$ave, 
                                   type = "ave")[wdata$time$year == year & wdata$time$month == month, ])
    colorlines <- paste(colorlines, ifelse(nchar(colorlines) == 1, "", ","), 
                        " 'blue'", sep = "")
  }
  if ("min" %in% var) {
    data <- rbind(data, data.frame(dt = wdata$time$dt, val =  wdata[["ane"]][[ane]]$min, 
                                   type = "min")[wdata$time$year == year & wdata$time$month == month, ])
    colorlines <- paste(colorlines, ifelse(nchar(colorlines) == 1, "", ","), 
                        " 'lightgray'", sep = "")
  }
  if ("max" %in% var) {
    data <- rbind(data, data.frame(dt = wdata$time$dt, val =  wdata[["ane"]][[ane]]$max, 
                                   type = "max")[wdata$time$year == year & wdata$time$month == month, ])
    colorlines <- paste(colorlines, ifelse(nchar(colorlines) == 1, "", ","), 
                        " 'lightgray'", sep = "")
  }
  if ("temp" %in% var) {
    data <- rbind(data, data.frame(dt = wdata$time$dt, val = wdata$par$temp$value, 
                                   type = "temp")[wdata$time$year == year & wdata$time$month == month, ])
    colorlines <- paste(colorlines, ifelse(nchar(colorlines) == 1, "", ","), 
                        " 'green'", sep = "")
  }
  if ("pres" %in% var) {
    data <- rbind(data, data.frame(dt = wdata$time$dt, val = wdata$par$pres$value, 
                                   type = "pres")[wdata$time$year == year & wdata$time$month == month, ])
    colorlines <- paste(colorlines, ifelse(nchar(colorlines) == 1, "", ","), 
                        " 'lightgreen'", sep = "")
  }
  colorlines <- paste(colorlines, "]")
  
  # Borro registros sin datos
  data <- data[!is.na(data$dt), ]
  
  # esto es para que las escalas de las 3 velocidades sea la misma. Se modifica
  # el primer registro de la serie
  max <- max(data[, "val"], na.rm = TRUE)
  data[data$type == "min", ][1, 2] <- max
  data[data$type == "ave", ][1, 2] <- max
  
  if (length(axis) == 1) {
    scalecol <- paste("[", which(var == axis[1]) - 1, "]", sep = "")
  } else {
    scalecol <- paste("[", which(var == axis[1]) - 1, ",", which(var == axis[2]) - 
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

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

plotcalendar<-function(wd, 
                       var=c("ave","min","max"),
                       ane="Ane1",
                       shiny=F) {
  dates <- as.factor(as.Date(wd$time$dt))
  val <- wd$ane[[ane]][var][[1]]
  df <- tapply(val,dates,mean,na.rm=T)
  dataplot <- data.frame(date=as.Date(names(df)), value=as.numeric(df))
  dataplot <- dataplot[complete.cases(dataplot),]
  if (shiny==T){
      gvisCalendar(data=dataplot, 
                   datevar="date",
                   numvar="value",
                   options=list(
                     title="Calendar heat map of MSFT adjsuted close",
                     calendar="{cellSize:10,
                                 yearLabel:{fontSize:20, color:'#444444'},
                                 focusedCellColor:{stroke:'red'}}",
                     width=590, height=320),
                   chartid="Calendar")
  }
  else
  {
    plotdata <- 
      gvisCalendar(data=dataplot, 
                   datevar="date",
                   numvar="value",
                   options=list(
                     title="Calendar heat map of MSFT adjsuted close",
                     calendar="{cellSize:10,
                                 yearLabel:{fontSize:20, color:'#444444'},
                                 focusedCellColor:{stroke:'red'}}",
                     width=590, height=320),
                   chartid="Calendar")
    
    plot(plotdata)
  }
}

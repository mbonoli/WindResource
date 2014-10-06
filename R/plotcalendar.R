#' @title Conversion of dataframe in class windata
#' 
#' @description
#' Shape a dataframe in a class 'windata'
#' 
#' @details
#' The object windata is a list with the parameters that were mencionated before.
#'   
#' @param wd a dataframe to be converted
#' @param var interval of time betwen two registers. Actually, it's only acepted intervals of 1 minut.
#' @param ane the name of the variable that contains the dates of measurements
#' @param shiny the admit formats are:
#'        - 'YYYYMMDD','YYYY-MM-DD','YYYY.MM.DD' or 'DD/MM/YYYY'       
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

plotcalendar <- function(wd, var = c("ave", "min", "max"), ane = "Ane1", shiny = F) {
    dates <- as.factor(as.Date(wd$time$dt))
    val <- wd$ane[[ane]][var][[1]]
    df <- tapply(val, dates, mean, na.rm = T)
    dataplot <- data.frame(date = as.Date(names(df)), value = as.numeric(df))
    dataplot <- dataplot[complete.cases(dataplot), ]
    if (shiny == T) {
        gvisCalendar(data = dataplot, datevar = "date", numvar = "value", options = list(title = "Calendar heat map of MSFT adjsuted close", 
            calendar = "{cellSize:10,\n                                 yearLabel:{fontSize:20, color:'#444444'},\n                                 focusedCellColor:{stroke:'red'}}", 
            width = 590, height = 320), chartid = "Calendar")
    } else {
        plotdata <- gvisCalendar(data = dataplot, datevar = "date", numvar = "value", 
            options = list(title = "Calendar map Anemometer: Anem37aMS", 
                calendar = "{cellSize:10,\n                                 yearLabel:{fontSize:20, color:'#444444'},\n                                 focusedCellColor:{stroke:'red'}}", 
                width = 590, height = 320), chartid = "Calendar")
        
        plot(plotdata)
    }
} 

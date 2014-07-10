#' @title Plots for winddata objects
#' 
#' @description
#' Shape a dataframe in a object which will be use by the diferents functions in the package......
#'
#' @param data a list of parameters.
#' @details
#' The object windata is a list with the parameters that were mencionated before.
#' 
#' @export
runGUI <- function(data, inRS=T){
  if (class(data)=="windata"){
    require(shiny)
    dataGUIwd <- data
    save(dataGUIwd,file=paste(path.package("WindResource"),"/shiny/dataGUIwd.Rd",sep=""))
    runApp(paste(path.package("WindResource"),"/shiny",sep=""))
  }
  else stop ("'data' parameter must be windata class.")
}

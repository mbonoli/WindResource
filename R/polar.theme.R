#' @title Extracting paramenters
#' 
#' @description
#' Function to extract a parameter value from a list
#' 
#' @param plot a list of parameters.
#' @return The parameter value that was extracted.
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
#' 
#' 
polar.theme <- function (plot){
  
  radialaxis<-data.frame(x=0,y=0,
                         xend=10.5*sin((1:16)*360/16/180*pi),
                         yend=10.5*cos((1:16)*360/16/180*pi), 
                         text=c("NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW","N"))      
  result <- plot+
    geom_path(data=circleFun(c(0,0),2,npoints = 100),aes(x,y),color="white")+
    geom_path(data=circleFun(c(0,0),4,npoints = 100),aes(x,y),color="white")+
    geom_path(data=circleFun(c(0,0),6,npoints = 100),aes(x,y),color="white")+
    geom_path(data=circleFun(c(0,0),8,npoints = 100),aes(x,y),color="white")+
    geom_path(data=circleFun(c(0,0),10,npoints = 100),aes(x,y),color="white")+
    geom_segment(data=radialaxis, mapping=aes(
      x=x, y=y, 
      xend=xend, yend=yend),color="white")+
    geom_text(data=radialaxis, mapping=aes(
      x=xend, y=yend, 
      label=text, size=8)) +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.major = element_blank() ,
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank())
  
  result
}

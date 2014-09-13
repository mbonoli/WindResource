
#' @title Extracting paramenters
#' 
#' @description
#' Function to extract a parameter value from a list
#' 
#' @param plot a list of parameters.
#' @return The parameter value that was extracted.
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
#' 
#' @export

polar.theme <- function(plot, maxi, by) {
    
    radialaxis <- data.frame(x = 0, y = 0, xend = maxi * sin((1:16) * 360/16/180 * 
        pi), yend = maxi * cos((1:16) * 360/16/180 * pi), text = c("NNE", "NE", 
        "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", 
        "NNW", "N"))        
    if (by == "none") {
        coor <- data.frame(value.start = c((maxi/5)*1.12, (maxi*2/5)*1.1, (maxi*3/5)*1.07, (maxi*4/5)*1.05, maxi*1.03),
                      value.end = c((maxi/5)*1.12, (maxi*2/5)*1.1, (maxi*3/5)*1.07, (maxi*4/5)*1.05, maxi*1.03),
                      ang.start = c(45/4, 45/4, 45/4, 45/4, 45/4), ang.end = c(45/4, 45/4, 45/4, 45/4, 45/4))
        coorPolar <- add.cart.coord(coor)
        result <- plot + geom_path(data = circleFun(c(0, 0), maxi/5, npoints = 100), 
                  aes(x, y), color = "white") + geom_path(data = circleFun(c(0, 0), maxi * 2/5, npoints = 100), aes(x, y), color = "white") + 
                  geom_path(data = circleFun(c(0, 0), maxi * 3/5, npoints = 100), aes(x, y), color = "white") + geom_path(data = circleFun(c(0, 0), maxi * 4/5, npoints = 100), aes(x, y), color = "white") + 
                  geom_path(data = circleFun(c(0, 0), maxi, npoints = 100), aes(x, y), color = "white") + geom_segment(data = radialaxis, mapping = aes(x = x, y = y, xend = xend, yend = yend), color = "white") + 
                  geom_text(data = radialaxis, mapping = aes(x = xend, y = yend, label = text, size = 8)) + theme(axis.text.y = element_blank(), axis.text.x = element_blank(), panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), axis.ticks = element_blank()) +geom_text(data = radialaxis, mapping = aes(x = xend, y = yend, label = text, size = 8)) +
                  geom_text(x = coorPolar[1,6], y = coorPolar[1,5], label = paste(round(maxi/5, 2), "m/s", sep=" "), size=3.5, srt = 355) + 
                  #geom_text(x = coorPolar[2,6], y = coorPolar[2,5], label = paste(round(maxi*2/5, 2), "m/s", sep=" "), size=3.5, srt = 355) +
                  geom_text(x = coorPolar[3,6], y = coorPolar[3,5], label = paste(round(maxi*3/5, 2), "m/s", sep=" "), size=3.5, srt = 355) +   
                  #geom_text(x = coorPolar[4,6], y = coorPolar[4,5], label = paste(round(maxi*4/5, 2), "m/s", sep=" "), size=3.5, srt = 355) +
                  geom_text(x = coorPolar[5,6], y = coorPolar[5,5], label = paste(round(maxi, 2), "m/s", sep=" "), size=3, srt = 355)

      
        } else { 
          
            result <- plot + geom_path(data = circleFun(c(0, 0), maxi/5, npoints = 100), 
                      aes(x, y), color = "white") + geom_path(data = circleFun(c(0, 0), maxi * 2/5, npoints = 100), aes(x, y), color = "white") + 
                      geom_path(data = circleFun(c(0, 0), maxi * 3/5, npoints = 100), aes(x, y), color = "white") + geom_path(data = circleFun(c(0, 0), maxi * 4/5, npoints = 100), aes(x, y), color = "white") + 
                      geom_path(data = circleFun(c(0, 0), maxi, npoints = 100), aes(x, y), color = "white") + geom_segment(data = radialaxis, mapping = aes(x = x, y = y, xend = xend, yend = yend), color = "white") + 
                      geom_text(data = radialaxis, mapping = aes(x = xend, y = yend, label = text, size = 8)) + theme(axis.text.y = element_blank(), axis.text.x = element_blank(), panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(), axis.ticks = element_blank()) +geom_text(data = radialaxis, mapping = aes(x = xend, y = yend, label = text, size = 8))
          
        }
    
    
    result
    
}

# text(x=((maxi/5)*sin((1:16)*360/16/180*pi)),
# y=((maxi/5)*cos((1:16)*360/16/180*pi)), label='a')

# if (vari == 'min'){ radialaxis<-data.frame(x=0,y=0,
# xend=maxi*sin((1:16)*360/16/180*pi), yend=maxi*cos((1:16)*360/16/180*pi),
# text=c('NNE','NE','ENE','E','ESE','SE','SSE','S','SSW','SW','WSW','W','WNW','NW','NNW','N'))
# result <- plot+ geom_path(data=circleFun(c(0,0),maxi/5, npoints =
# 100),aes(x,y),color='white')+ geom_path(data=circleFun(c(0,0),(maxi/5)*2,
# npoints = 100),aes(x,y),color='white')+
# geom_path(data=circleFun(c(0,0),(maxi/5)*3, npoints =
# 100),aes(x,y),color='white')+ geom_path(data=circleFun(c(0,0), (maxi/5)*4,
# npoints = 100),aes(x,y),color='white')+ geom_path(data=circleFun(c(0,0),maxi
# , npoints = 100),aes(x,y),color='white')+ geom_segment(data=radialaxis,
# mapping=aes( x=x, y=y, xend=xend, yend=yend),color='white')+
# geom_text(data=radialaxis, mapping=aes( x=xend, y=yend, label=text, size=8))
# + theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
# panel.grid.major = element_blank() , panel.grid.minor = element_blank(),
# axis.ticks = element_blank()) result } else if (vari=='mean') {
# radialaxis<-data.frame(x=0,y=0, xend=35.5*sin((1:16)*360/16/180*pi),
# yend=35.5*cos((1:16)*360/16/180*pi),
# text=c('NNE','NE','ENE','E','ESE','SE','SSE','S','SSW','SW','WSW','W','WNW','NW','NNW','N'))
# result <- plot+ geom_path(data=circleFun(c(0,0), 7, npoints =
# 100),aes(x,y),color='white')+ geom_path(data=circleFun(c(0,0), 14, npoints =
# 100),aes(x,y),color='white')+ geom_path(data=circleFun(c(0,0),21, npoints =
# 100),aes(x,y),color='white')+ geom_path(data=circleFun(c(0,0), 28, npoints =
# 100),aes(x,y),color='white')+ geom_path(data=circleFun(c(0,0), 35, npoints =
# 100),aes(x,y),color='white')+ geom_segment(data=radialaxis, mapping=aes( x=x,
# y=y, xend=xend, yend=yend),color='white')+ geom_text(data=radialaxis,
# mapping=aes( x=xend, y=yend, label=text, size=8)) + theme(axis.text.y =
# element_blank(), axis.text.x = element_blank(), panel.grid.major =
# element_blank() , panel.grid.minor = element_blank(), axis.ticks =
# element_blank()) result } else if (vari == 'max'){
# radialaxis<-data.frame(x=0,y=0, xend=35.5*sin((1:16)*360/16/180*pi),
# yend=35.5*cos((1:16)*360/16/180*pi),
# text=c('NNE','NE','ENE','E','ESE','SE','SSE','S','SSW','SW','WSW','W','WNW','NW','NNW','N'))
# result <- plot+ geom_path(data=circleFun(c(0,0), 7, npoints =
# 100),aes(x,y),color='white')+ geom_path(data=circleFun(c(0,0), 14, npoints =
# 100),aes(x,y),color='white')+ geom_path(data=circleFun(c(0,0), 21, npoints =
# 100),aes(x,y),color='white')+ geom_path(data=circleFun(c(0,0), 28, npoints =
# 100),aes(x,y),color='white')+ geom_path(data=circleFun(c(0,0), 35, npoints =
# 100),aes(x,y),color='white')+ geom_segment(data=radialaxis, mapping=aes( x=x,
# y=y, xend=xend, yend=yend),color='white')+ geom_text(data=radialaxis,
# mapping=aes( x=xend, y=yend, label=text, size=8)) + theme(axis.text.y =
# element_blank(), axis.text.x = element_blank(), panel.grid.major =
# element_blank() , panel.grid.minor = element_blank(), axis.ticks =
# element_blank()) result } } 

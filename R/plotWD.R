#' @title Plots for winddata objects
#' 
#' @description
#' Shape a dataframe in a object which will be use by the diferents functions in the package......
#' 
#' @details
#' The object windata is a list with the parameters that were mencionated before.
#'   
#' @param data an object of class winddata 
#' @param ane an optional vector specifying a subset of anenemometers to plot
#' @param var currently only method = "qr" is supported
#' @param type the type of graphic to plot. Actually soported: 'histogram', 'rose', 
#'        'correlogram', 'profiles' and 'boxplot'. See also 'Details'.
#' @param by an optional string stating if the plot is divided in panels by 'month' or 'hour'. 
#' @param since an optional string indicating initial date to be taken into account to make the plot.
#'        The string format is 'YYYY-MM-DD'.
#' @param to an optional string indicating final date to be taken into account to make the plot.
#'        The string format is 'YYYY-MM-DD'.
#' @return Object of class "windata" (see details)
#' 
#' @author Valeria Gogni, Mariano Bonoli, Ruben Bufanio, Diego Edwards
#' @examples
#' # simple example using the windspeed data set
#' data(wd)
#'  
#' # let's examine windspeed to see the variables' names
#' str(wd)
#' 


plotWD <- 
  function(data,
           ane=NA,
           var=c("speed"),
           type=c("histogram"),
           by=c("none","month","hour"),
           since=NULL, 
           to=NULL){
    
#    browser()
#     if (!is.na(since)){
#       date.since <- as.POSIXct(since)
#       date.to <- as.POSIXct(to)
#       valid.cases <- (data$time$dt >= date.since) & (data$time$dt <= date.to) & !is.na(data$time$dt)
#       for (i in c("data","time","dir")){
#         for (j in length(data[[i]])){
#           data[[i]][[j]] <- data[[i]][[j]][valid.cases]
#         }
#       }
#       for (i in length(data[["ane"]])){
#         for (j in length(data[["ane"]][[i]])){
#           data[["ane"]][[i]][[j]] <- data[["ane"]][[i]][[j]][valid.cases]
#         }
# #       }
#       
#       data$data <- data$data[valid.cases]
#       data$dir <- data$dir[valid.cases,]
#       data$time <- data$time[valid.cases,]
#       for (i in ane) {
#         data$ane <- data$ane[[i]]
#       } 
#       
#     }

    month.names<- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    hour.names2 <- c("00:00 - 01:59","02:00 - 03:59","04:00 - 05:59","06:00 - 07:59","08:00 - 09:59","10:00 - 11:59","12:00 - 13:59","14:00 - 15:59","16:00 - 17:59","18:00 - 19:59","20:00 - 21:59","22:00 - 23:59")
    hour.names <- pref0(0:23,2)
    
    if (class(data) != "windata") stop("Los datos no correponden a la clase 'windata'.")
    
    if (is.na(ane)[1]) {ane<-names(data$ane)[1]}
    nane<-length(ane)
    if (type=="histogram"){
      if (by=="none"){
        dp<-data.frame(speed=data$ane[[ane]]$ave)
        ggplot(dp, aes(x=speed)) + 
          geom_histogram(binwidth=.5, colour="black", fill="blue")
      }
      else if (by=="month"){
        ds<-data.frame(speed=data$ane[[ane]]$ave, 
                       month=factor(month.names[data$time$month], levels=month.names))
        ggplot(ds, aes(x=speed)) + 
          geom_histogram(binwidth=.5, colour="black", fill="blue") +
          facet_wrap(  ~ month, ncol=3, drop=F)
      }
      else if (by=="hour"){
        ds<-data.frame(speed=data$ane[[ane]]$ave, 
                       hour=factor(hour.names2[floor(data$time$hour/2)+1], levels=hour.names2))
        ggplot(ds, aes(x=speed)) + 
          geom_histogram(binwidth=.5, colour="black", fill="blue") +
          facet_wrap(  ~ hour, ncol=3, drop=F)
      }
    }
    else if (type=="rose"){
      dfall<-data.frame()
      for (i in ane){
        dfall <- rbind(dfall,
                       data.frame(speed.start=data$ane[[i]]$ave, 
                                  rose=as.factor(data$dir$rose), 
                                  ang.start=data$dir$ang_16, 
                                  ane=i, 
                                  month=factor(month.names[data$time$month], levels=month.names),
                                  hour=factor(hour.names2[floor(data$time$hour/2)+1]))
        )
      }
      dfall<-dfall[complete.cases(dfall),]
      if (by=="none"){
        dataplot <- aggregate(speed.start ~ rose + ane + ang.start , data = dfall, mean)
        dataplot$ang.end <- ifelse(dataplot$ang.start + 22.5 >= 360, dataplot$ang.start - 337.5,dataplot$ang.start + 22.5)
        dataplot$speed.end <- dataplot[apply(dataplot, 1, function(x) ifelse(length(which(as.numeric(dataplot$ang.start)==as.numeric(x["ang.end"]) & dataplot$ane== x["ane"]))==0,NA,which(as.numeric(dataplot$ang.start)==as.numeric(x["ang.end"]) & dataplot$ane== x["ane"]))),"speed.start"]
        dataplot <- add.cart.coord(dataplot)
        polar.theme(ggplot(data=dataplot)) +
          geom_segment(data=dataplot, mapping=aes(
            x=x, y=y, 
            xend=xend, yend=yend, 
            color=ane, group=ane),size=1)
        dataplot
        
      }
      else if (by=="month"){
        dataplot <- aggregate(speed.start ~ rose + ane + ang.start + month , data = dfall, mean)
        dataplot$ang.end <- ifelse(dataplot$ang.start + 22.5 >= 360, dataplot$ang.start - 337.5,dataplot$ang.start + 22.5)
        dataplot$speed.end <- dataplot[apply(dataplot, 1, function(x) ifelse(length(which(as.numeric(dataplot$ang.start)==as.numeric(x["ang.end"]) & dataplot$ane== x["ane"] & dataplot$month==x["month"]))==0,NA,which(as.numeric(dataplot$ang.start)==as.numeric(x["ang.end"]) & dataplot$ane== x["ane"] & dataplot$month==x["month"]))),"speed.start"]
        dataplot <- add.cart.coord(dataplot)
        polar.theme(ggplot(data=dataplot)) +
          geom_segment(data=dataplot, mapping=aes(
            x=x, y=y, 
            xend=xend, yend=yend, 
            color=ane, group=ane),size=1) +
          facet_wrap(  ~ month, ncol=4, drop=F)
      }
      else if (by=="hour"){
        dataplot <- aggregate(speed.start ~ rose + ane + ang.start + hour , data = dfall, mean)
        dataplot$ang.end <- ifelse(dataplot$ang.start + 22.5 >= 360, dataplot$ang.start - 337.5,dataplot$ang.start + 22.5)
        dataplot$speed.end <- dataplot[apply(dataplot, 1, function(x) ifelse(length(which(as.numeric(dataplot$ang.start)==as.numeric(x["ang.end"]) & dataplot$ane== x["ane"] & dataplot$hour==x["hour"]))==0,NA,which(as.numeric(dataplot$ang.start)==as.numeric(x["ang.end"]) & dataplot$ane== x["ane"] & dataplot$hour==x["hour"]))),"speed.start"]
        dataplot <- add.cart.coord(dataplot)
        polar.theme(ggplot(data=dataplot)) +
          geom_segment(data=dataplot, mapping=aes(
            x=x, y=y, 
            xend=xend, yend=yend, 
            color=ane, group=ane),size=1) +
          facet_wrap(  ~ hour, ncol=4, drop=F)
      }
    }
    else if (type=="correlation"){
      df <- data.frame(x=data$ane[[1]]$ave, y=data$ane[[2]]$ave)
      df <- df[complete.cases(df),]
      ggplot(data=df, aes(x=x,y=y))+
        geom_point(size=1)+
        ggtitle("")+
        xlab(paste("Wind Speed ",data$info$ane$height[1],"m [",data$info$unit$speed,"]",sep="")) +
        ylab(paste("Wind Speed ",data$info$ane$height[2],"m [",data$info$unit$speed,"]",sep=""))
    }
    else if (type=="profile"){
#       browser()
      df <- data.frame()
      for (i in ane){
        df <- rbind(df,
                    data.frame(ave=data$ane[[i]]$ave,
                               ane=i, 
                               month=factor(month.names[data$time$month], levels=month.names),
                               hour=factor(hour.names[data$time$hour+1],levels=hour.names)
                    )
        )
      }
      if (by=="month"){
        dataplot <- aggregate(ave ~ ane + month, data = df, mean)
        # Esto es por si faltan meses
        dataplot <- join (expand.grid(ane=ane, month=month.names),dataplot, type="left")
        ggplot(dataplot, aes(x = month, y=ave, color=ane, group =ane)) + 
          geom_line(size=1) +
          labs(title="Profile") + 
          ylab("Speed(m/S)") + 
          scale_y_continuous(limits=c(0, max(dataplot$ave, na.rm=T)*1.2))+
          xlab("Month") 
      }
      else if (by=="hour"){
        dataplot <- aggregate(ave ~ ane + hour, data = df, mean)
        # Esto es por si faltan horarios
        dataplot <- join (expand.grid(ane=ane, hour=hour.names),dataplot, type="left")
        ggplot(dataplot, aes(x = hour, y=ave, color=ane, group =ane)) + 
          geom_line(size=1) +
          labs(title="Profile") + 
          ylab("Speed(m/S)") + 
          scale_y_continuous(limits=c(0, max(dataplot$ave, na.rm=T)*1.2))+
          xlab("Hour") 
      }
      else stop("Profiles plots requires that the By paremeter takes values 'month' or 'hour'.")
    }
    else stop(paste("invalid plot type '",type,"'",sep=""))
  }
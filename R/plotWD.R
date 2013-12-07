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
#' @export
#' @examples
#' # simple example using the windspeed data set
#' data(wd)
#'  
#' # let's examine windspeed to see the variables' names
#' str(wd)
#' 


plotWD <- 
  function(datawd,
           ane=NA,
           var=NA,
           type=c("histogram"),
           by=c("none","month","hour"),
           since=NULL, 
           to=NULL){

    require(plyr)
#    browser()
#     if (!is.na(since)){
#       date.since <- as.POSIXct(since)
#       date.to <- as.POSIXct(to)
#       valid.cases <- (datawd$time$dt >= date.since) & (datawd$time$dt <= date.to) & !is.na(datawd$time$dt)
#       for (i in c("data","time","dir")){
#         for (j in length(datawd[[i]])){
#           datawd[[i]][[j]] <- datawd[[i]][[j]][valid.cases]
#         }
#       }
#       for (i in length(datawd[["ane"]])){
#         for (j in length(datawd[["ane"]][[i]])){
#           datawd[["ane"]][[i]][[j]] <- datawd[["ane"]][[i]][[j]][valid.cases]
#         }
# #       }
#       
#       datawd$data <- datawd$data[valid.cases]
#       datawd$dir <- datawd$dir[valid.cases,]
#       datawd$time <- datawd$time[valid.cases,]
#       for (i in ane) {
#         datawd$ane <- datawd$ane[[i]]
#       } 
#       
#     }
  
    month.names<- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    hour.names2 <- c("00:00 - 01:59","02:00 - 03:59","04:00 - 05:59","06:00 - 07:59","08:00 - 09:59","10:00 - 11:59","12:00 - 13:59","14:00 - 15:59","16:00 - 17:59","18:00 - 19:59","20:00 - 21:59","22:00 - 23:59")
    hour.names <- pref0(0:23,2)

    if (class(datawd) != "windata") stop("Los datos no correponden a la clase 'windata'.")
    
    if (is.na(ane[1])) {ane.names <- names(datawd$ane)}
    else {ane.names <- ane}
   
    if (type=="histogram"){
      if (!(var=="speed" | is.na(var))) stop ("Only 'speed' var is supported for histograms at the moment.")
      if (by=="none"){
        for (iane in ane.names){     
          dp<-data.frame(speed=datawd$ane[[iane]]$ave)
          print(
            ggplot(dp, aes(x=speed)) + 
              geom_histogram(binwidth=1, colour="black", fill="blue")
          )
        }
      }
      else if (by=="month"){
        for (iane in ane.names){
          ds<-data.frame(speed=datawd$ane[[iane]]$ave, 
                         month=factor(month.names[datawd$time$month], levels=month.names))
          print(
            ggplot(ds, aes(x=speed)) + 
            geom_histogram(binwidth=1, colour="black", fill="blue") +
            facet_wrap(  ~ month, ncol=3, drop=F)
          )
        }
      }
      else if (by=="hour"){
        for (iane in ane.names){
          ds<-data.frame(speed=datawd$ane[[iane]]$ave, 
                         hour=factor(hour.names2[floor(datawd$time$hour/2)+1], levels=hour.names2))
          print(
            ggplot(ds, aes(x=speed)) + 
            geom_histogram(binwidth=1, colour="black", fill="blue") +
            facet_wrap(  ~ hour, ncol=3, drop=F)
          )
        }
      }
    }
    else if (type=="rose"){
      dfall<-data.frame()
      for (i in ane.names){
        dfall <- rbind(dfall,
                       data.frame(speed.start=datawd$ane[[i]]$ave, 
                                  rose=as.factor(datawd$dir$rose), 
                                  ang.start=datawd$dir$ang_16, 
                                  ane=i, 
                                  month=factor(month.names[datawd$time$month], levels=month.names),
                                  hour=factor(hour.names2[floor(datawd$time$hour/2)+1]))
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
      df <- data.frame(x=datawd$ane[[1]]$ave, y=datawd$ane[[2]]$ave)
      df <- df[complete.cases(df),]
      ggplot(data=df, aes(x=x,y=y))+
        geom_point(size=1)+
        ggtitle("")+
        xlab(paste("Wind Speed ",datawd$info$ane$height[1],"m [",datawd$info$unit$speed,"]",sep="")) +
        ylab(paste("Wind Speed ",datawd$info$ane$height[2],"m [",datawd$info$unit$speed,"]",sep=""))
    }
    else if (type=="profile"){
#       browser()
      df <- data.frame()
      for (i in ane){
        df <- rbind(df,
                    data.frame(ave=datawd$ane[[i]]$ave,
                               ane=i, 
                               month=factor(month.names[datawd$time$month], levels=month.names),
                               hour=factor(hour.names[datawd$time$hour+1],levels=hour.names)
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
    else if (type=="turbulence"){
      require (sqldf)
      
      df <- data.frame(ave=datawd$ane[[1]]$ave,
                       sd=datawd$ane[[1]]$sd)
      df$I <- df$sd/df$ave*100
      df$bin <- floor(df$ave + .5)
      dataplot<-sqldf("select bin widspeed, count(*) count, avg(I) I from df where bin>=1 group by bin")
      
      ref.point<-data.frame(x=c(15,15,15), 
                            y=c(16, 14, 12), 
                            ref=c("A - High Turbulence characteristics","B - Medium Turbulence characteristics","C - Low Turbulence characteristics"))
      
      c <- ggplot(dataplot, aes(x=widspeed,y= I)) 
      print(c + geom_line(size=1) + 
              coord_cartesian(xlim = c(0, 17), ylim=c(0,35))  +  
              xlab("Vhub [m/s]") +
              ylab("Turbulence Intensity [%]")+
              geom_point(data=ref.point, mapping=aes(x=x, y=y, group=ref, color=ref), size=5)+
              scale_x_continuous(breaks=1:16)+
              scale_y_continuous(breaks=(1:7)*5)+
              theme(axis.title.x = element_text(face="bold", size=12),
                    axis.title.y = element_text(face="bold", size=12))  )
    }
    else if (type=="fit"){
      
      for (i in ane.names){
        
        param <- fitWd(data, ane=i)
        
        #Busco el valor mas alto de las funciones de densidad
        max.y<-max(dweibull(param$A*((param$K-1)/param$K)^(1/param$K), shape=param$K, scale =param$A, log = FALSE),
                   dgamma((param$alpha-1)*param$beta, shape=param$alpha, scale=param$beta),
                   dlnorm(exp(param$meanlog-param$sdlog^2), meanlog=param$meanlog, sdlog=param$sdlog))
        xfit<-seq(0,max(param$data),length=200)
        ylim<-c(0,max.y)
        
        # Weibull Density
        par(mfrow=c(2,3))
        hist(param$data,probability=T, col="gray", main="Distribution Weibull", xlab="Wind speed", ylab="frecuency density",ylim=ylim)
        text(max(param$data)*0.8,max.y*0.8, paste("K=",round(param$K,digits=4)))
        text(max(param$data)*0.8,max.y*0.7, paste("C=",round(param$A,digits=4)))
        text(max(param$data)*0.8,max.y*0.6, paste("Loglik=", round(param$loglik.wei,digits=0)))
        xfit<-seq(0,max(param$data),length=200)
        yfit<-dweibull(xfit, shape=param$K, scale=param$A)  
        lines(xfit, yfit, col="red", lwd=2.8)
        
        # Gamma Density
        hist(param$data,probability=T, col="gray", main="Distribution Gamma", xlab="Wind speed", ylab="frecuency density",ylim=ylim)
        text(max(param$data)*0.8,max.y*0.8, paste("alpha=",round(param$alpha,digits=4))) 
        text(max(param$data)*0.8,max.y*0.7, paste("beta=",round(param$beta,digits=4)))
        text(max(param$data)*0.8,max.y*0.6, paste("Loglik=",round(param$loglik.ga,digits=0)))
        yfit <-dgamma(xfit, shape=param$alpha, scale=param$beta) 
        lines(xfit, yfit, col="red", lwd=2.8)
        
        # LogNormal Density
        hist(param$data,probability=T, col="gray", main="Distribution Lognormal", xlab="Wind speed", ylab="frecuency density",ylim=ylim)
        text(max(param$data)*0.8,max.y*0.8, paste("m=",round(param$meanlog,digits=4))) 
        text(max(param$data)*0.8,max.y*0.7, paste("D=",round(param$sdlog,digits=4)))
        text(max(param$data)*0.8,max.y*0.6, paste("Loglik=",round(param$loglik.ln,digits=0)))
        yfit <-dlnorm(xfit, meanlog=param$meanlog, sdlog=param$sdlog) 
        lines(xfit, yfit, col="red", lwd=2.8)
        
        #Q-Q plots
        c <- seq(0,0.9999,length=length(param$data))
        dist.teo_w <- qweibull(c[1:length(c)],shape=param$K, scale=param$A)
        qqplot(dist.teo_w,param$data,main="QQ-plot Dist. Weibull", xlab="theoretical distribution", ylab="speed")
        abline(0,1)
        dist.teo_g <- qgamma(c[1:length(c)], shape=param$alpha, scale=param$beta)
        qqplot(dist.teo_g, param$data, main="QQ-plot Dist. Gamma", xlab="theoretical distribution", ylab="speed")
        abline(0,1)
        dist.teo_l <- qlnorm(c[1:length(c)],meanlog=param$meanlog, sdlog=param$sdlog)
        qqplot(dist.teo_l, param$data, main="QQ-plot Dist. Lognorm", xlab="theoretical distribution", ylab="speed")
        abline(0,1)       
      }
    }
    else stop(paste("invalid plot type '",type,"'",sep=""))
  }

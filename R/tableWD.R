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
#' head(wd)
#' 


tableWD <- 
  function(datawd,
           ane=NA,
           var=c("speed"),
           type=c("histogram"),
           by=c("none","month","hour"),
           since=NULL, 
           to=NULL,
           binwidth=1){
    
    require (reshape2)
    
    rose_dir<-c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW")
    month.names3<- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    hour.names2 <- c("00:00 - 01:59","02:00 - 03:59","04:00 - 05:59","06:00 - 07:59","08:00 - 09:59","10:00 - 11:59","12:00 - 13:59","14:00 - 15:59","16:00 - 17:59","18:00 - 19:59","20:00 - 21:59","22:00 - 23:59")
    hour.names <- pref0(0:23,2)
    
    if (class(datawd) != "windata") stop("Los datos no correponden a la clase 'windata'.")
    
    # Apply date filter
    if (!is.null(since)){
      date.since <- as.POSIXct(since)
      date.to <- as.POSIXct(to)
      valid.cases <- (datawd$time$dt >= date.since) & (datawd$time$dt <= date.to) & !is.na(datawd$time$dt)
      datawd[["time"]] <- datawd[["time"]][valid.cases,]
      datawd[["dir"]] <- datawd[["dir"]][valid.cases,]
      for (i in names(datawd[["ane"]])){
        datawd[["ane"]][[i]] <- datawd[["ane"]][[i]][valid.cases,]
      }
    }
    
    # Control anemometros
#     browser()
    if (is.na(ane[1])) {ane.names <- names(datawd$ane)}
    else {ane.names <- ane}
    
    if (type=="histogram"){
      if (by=="none"){
        result <- list()
        for (i in ane.names){
          dp<-data.frame(speed=datawd$ane[[i]]$ave)
          histo <- hist(dp$speed,breaks=seq(0,max(as.numeric(dp$speed),na.rm=T)+1,by=binwidth),plot =F)
          result [[i]] <- data.frame(
            Lower=histo$breaks[-length(histo$breaks)],
            Upper=histo$breaks[-1],
            Freq=histo$counts)
        }     
      }
      else if (by=="month"){
        result <- list()
        for (i in ane.names){
          dp<-data.frame(speed=datawd$ane[[i]]$ave,
                         month=month.names3[datawd$time$month])
          histo <- hist(dp$speed,breaks=seq(0,max(as.numeric(dp$speed),na.rm=T)+1,by=binwidth),plot =F)
          breaks <- histo$breaks
          result [[i]] <- data.frame(
            Lower=histo$breaks[-length(histo$breaks)],
            Upper=histo$breaks[-1])
          for (m in month.names3) {
            dpm <- subset(dp,month==m)
            histo <- hist(dpm$speed,breaks=breaks,plot =F)
            result [[i]][m]<- histo$counts
          }
        }
      }
      else if (by=="hour"){
        result <- list()
        for (i in ane.names){
          dp<-data.frame(speed=datawd$ane[[i]]$ave,
                         hour=factor(hour.names2[floor(datawd$time$hour/2)+1], levels=hour.names2))
          histo <- hist(dp$speed,breaks=seq(0,max(as.numeric(dp$speed),na.rm=T)+1,by=binwidth),plot =F)
          breaks <- histo$breaks
          result[[i]] <- data.frame(
            Lower=histo$breaks[-length(histo$breaks)],
            Upper=histo$breaks[-1])
          for (h in hour.names2) {
            dpm <- subset(dp,hour==h)
            histo <- hist(dpm$speed,breaks=breaks,plot =F)
            result[[i]][h]<- histo$counts
          }
        }
      }
    }  
    else if (type=="rose"){
      dfs<-list()
      result<-list()
      for (i in ane.names){
        dfs[[i]]=data.frame(ave=datawd$ane[[i]]$ave,
                            rose=datawd$dir$rose, 
                            month=datawd$time$month,
                            hour=factor(hour.names2[floor(datawd$time$hour/2)+1]))
      }
      for (i in ane.names){
        if (by=="none"){
          result[[i]] <- aggregate(ave ~ rose , data = dfs[[i]], mean)}
        else if (by=="month"){
          result[[i]] <- aggregate(ave ~ rose + month  , data = dfs[[i]], mean)
          result[[i]] <- dcast(result[[i]], rose~month, mean, value.var="ave")
        }
        else if (by=="hour"){
          result[[i]] <- aggregate(ave ~ rose + hour  , data = dfs[[i]], mean)
          result[[i]] <- dcast(result[[i]], rose~hour, mean, value.var="ave")
        }
      }
    }
    else if (type=="correlation"){
      if (nane!=2) stop("There must be 2 anemometers to generate a correlation plot.")
      df <- data.frame(x=datawd$ane[[ane[1]]]$ave, y=datawd$ane[[ane[2]]]$ave)
      df <- df[complete.cases(df),]
      result <- df
    }
    else if (type=="profile"){
      dfs<-list()
      result<-list()
      for (i in ane){
        dfs[[i]]=data.frame(ave=datawd$ane[[i]]$ave,
                            month=factor(month.names3[datawd$time$month], levels=month.names3),
                            hour=factor(hour.names[datawd$time$hour+1],levels=hour.names)
        )
      }
      #       browser()
      for (i in ane){
        if (by=="month"){
          result[[i]] <- aggregate(ave ~ month  , data = dfs[[i]], mean)
          # Esto es por si faltan meses
          result[[i]] <- join (expand.grid(month=month.names3),result[[i]], type="left")
        }
        else if (by=="hour"){
          result[[i]] <- aggregate(ave ~  hour  , data = dfs[[i]], mean)
          # Esto es por si faltan horarios
          result[[i]] <- join (expand.grid(hour=hour.names),result[[i]], type="left")
        }
        else {stop(paste("invalid plot type '",type,"'.",sep=""))}
      }
      
      
    }
    else if (type=='turbulence') {
      require (sqldf)
      
      df <- data.frame(ave=datawd$ane[[1]]$ave,
                       sd=datawd$ane[[1]]$sd)
      df$I <- df$sd/df$ave*100
      df$bin <- floor(df$ave + .5)
      dataplot<-sqldf("select bin widspeed, count(*) count, avg(I) I from df where bin>=1 group by bin")
      
      ref.point<-data.frame(x=c(15,15,15), 
                            y=c(16, 14, 12), 
                            ref=c("A - High Turbulence characteristics","B - Medium Turbulence characteristics","C - Low Turbulence characteristics"))
      
      result<- list(Iref=data.frame(Iref=mean(df[df$ave>=14.5 & df$ave<=15.5,"I"],na.rm=T)),data_turbulence=dataplot)
    }
    else if (type=="fit"){
      
      result<-list()
      for (i in ane.names){
        
        param <- fitWd(data, i)
        
        #Building the table
        result[[i]] <- data.frame(cbind(Parameter1 = c(paste("k=", round(param$K, digits = 4)), paste("alpha=", round(param$alpha, digits = 4)), paste("m=", round(param$meanlog, digits = 4))), 
                                        Parameter2 = c(paste("C=", round(param$A, digits = 4)), paste("beta=", round(param$beta, digits = 4)),paste("D=", round(param$sdlog,digits = 4))), 
                                        loglik = c(floor(param$loglik.wei), floor(param$loglik.ga), floor(param$loglik.ln)),
                                        aic = c(floor(param$aic.wei), floor(param$aic.ga), floor(param$aic.ln))), 
                                  row.names = c("Weibull", "Gamma", "Lognormal"))
       result <- summary(c(1:10)) 
      }
    }
    result

  }
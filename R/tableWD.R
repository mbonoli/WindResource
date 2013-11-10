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
  function(data,
           ane=NA,
           var=c("speed"),
           type=c("histogram"),
           by=c("none","month","hour"),
           since,
           to){
    
    rose_dir<-c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW")
    month.names<- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    hour.names2 <- c("00:00 - 01:59","02:00 - 03:59","04:00 - 05:59","06:00 - 07:59","08:00 - 09:59","10:00 - 11:59","12:00 - 13:59","14:00 - 15:59","16:00 - 17:59","18:00 - 19:59","20:00 - 21:59","22:00 - 23:59")
    hour.names <- pref0(0:23,2)
    
    if (class(data) != "windata") stop("Los datos no correponden a la clase 'windata'.")
    
    # Control anemometros
    ane.names <- names(data$ane)
    nane<-length(ane)
    if (ifelse(nane==1,is.na(ane),F)){
      print(paste("Se utiliza el primer aneometro: ",names(data$ane)[1], sep=""))
      ane<-ane.names[1]
    } else {
      if (nane>=2){
        if (type=="histogram"){
          print(paste("Para los histogramas solo puede tomarse un anemometros. Se toma el primero: ",names(data$ane)[1], sep="") )
          ane<-names(data$ane)[1]
        }
      }
    }
    
    if (type=="histogram"){
      if (by=="none"){
        dp<-data.frame(speed=data$ane[[ane]]$ave)
        histo <- hist(dp$speed,breaks=seq(0,max(as.numeric(dp$speed),na.rm=T)+1,by=1),plot =F)
        result <- list()
        for (i in 1:nane){
          result [[ane[i]]] <- data.frame(
            Lower=histo$breaks[-length(histo$breaks)],
            Upper=histo$breaks[-1],
            Freq=histo$counts)
        }     
      }
      else if (by=="month"){
        dp<-data.frame(speed=data$ane[[ane]]$ave,
                       month=month.name[data$time$month])
        histo <- hist(dp$speed,breaks=seq(0,max(as.numeric(dp$speed),na.rm=T)+.5,by=.5),plot =F)
        breaks <- histo$breaks
        result<-list()
        result [[ane]] <- data.frame(
          LI=histo$breaks[-1],
          LS=histo$breaks[-length(histo$breaks)])
        for (i in month.names) {
          dpm <- subset(dp,month==i)
          histo <- hist(dpm$speed,breaks=breaks,plot =F)
          result [[ane]][i]<- histo$counts
        }
      }
      else if (by=="hour"){
        dp<-data.frame(speed=data$ane[[ane]]$ave,
                       hour=factor(hour.names2[floor(data$time$hour/2)+1], levels=hour.names2))
        histo <- hist(dp$speed,breaks=seq(0,max(as.numeric(dp$speed),na.rm=T)+.5,by=.5),plot =F)
        breaks <- histo$breaks
        result <- list()
        result[[ane]] <- data.frame(
          LI=histo$breaks[-1],
          LS=histo$breaks[-length(histo$breaks)])
        for (i in hour.names2) {
          dpm <- subset(dp,hour==i)
          histo <- hist(dpm$speed,breaks=breaks,plot =F)
          result[[ane]][i]<- histo$counts
        }
      }
    }  
    else if (type=="rose"){
      dfs<-list()
      result<-list()
      for (i in ane){
        dfs[[i]]=data.frame(ave=data$ane[[i]]$ave,
                            rose=data$dir$rose, 
                            month=data$time$month,
                            hour=factor(hour.names2[floor(data$time$hour/2)+1]))
      }
      for (i in ane){
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
      df <- data.frame(x=data$ane[[ane[1]]]$ave, y=data$ane[[ane[2]]]$ave)
      df <- df[complete.cases(df),]
      result <- df
    }
    else if (type=="profile"){
      dfs<-list()
      result<-list()
      for (i in ane){
        dfs[[i]]=data.frame(ave=data$ane[[i]]$ave,
                            month=factor(month.names[data$time$month], levels=month.names),
                            hour=factor(hour.names[data$time$hour+1],levels=hour.names)
        )
      }
      #       browser()
      for (i in ane){
        if (by=="month"){
          result[[i]] <- aggregate(ave ~ month  , data = dfs[[i]], mean)
          # Esto es por si faltan meses
          result[[i]] <- join (expand.grid(month=month.names),result[[i]], type="left")
        }
        else if (by=="hour"){
          result[[i]] <- aggregate(ave ~  hour  , data = dfs[[i]], mean)
          # Esto es por si faltan horarios
          result[[i]] <- join (expand.grid(hour=hour.names),result[[i]], type="left")
        }
        else {stop(paste("invalid plot type '",type,"'.",sep=""))}
      }
      
      
    }
    
    result
  }

set_wd <-
  function (data, 
            interval=1,
            date.var, 
            date.format=c("YYYYMMDD","YYYY-MM-DD","YYYY.MM.DD","DD/MM/YYYY"), 
            time.var, 
            time.format=c("HHMM","HHMMSS","HH:MM","HH:MM:SS","HH.MM","HH.MM.SS"),
            ane.names,
            ane.height,
            speed.ave.var,
            speed.min.var=NA,
            speed.max.var=NA,
            speed.sd.var=NA,
            speed.var.var=NA,
            speed.unit=NA,
            dir.var,
            dir.unit="deg",
            temp.var=NA,
            temp.unit="C",
            pres.var=NA,
            pres.unit="bar"){
    
    # browser()   
    result<-list()
    
    print("\nIniciado proceso")
    cat(paste("\nCantidad de registros: ",dim(data)[1],sep=""))
    # Limpieza
    empty.rec<-which(is.na(data[,date.var]) | is.na(data[,time.var]))
    n.empty.rec<-length(empty.rec)
    if (n.empty.rec==0) {cat ("\nTodos los registros registran informacion de fecha y hora")}
    else {cat (paste ("\n",n.empty.rec," registros sin fecha/hora fueron eliminados. Ver $cleaning$rwd",sep=""))}
    result$cleaning$rwd<-empty.rec
    data<-data[!is.na(data[,date.var]) & !is.na(data[,time.var]),]
    #    browser() 
    # $data
    if  (sum(colnames(data)==date.var)==0) stop (paste("El nombre de columna date.var = ",date.var," no existe.",sep=""))
    if  (sum(colnames(data)==time.var)==0) stop (paste("El nombre de columna time.var = ",time.var," no existe.",sep=""))
    result$data <- data.frame(date=data[,date.var],
                              time=data[,time.var])
    #   browser()  
    # $time
    if (date.format=="YYYYMMDD") {
      result$time <- data.frame(year = as.numeric(substring(result$data$date,1,4)),
                                month = as.numeric(substring(result$data$date,5,6)),
                                day = as.numeric(substring(result$data$date,7,8)))
    }
    else if (date.format=="YYYY-MM-DD" | date.format=="YYYY.MM.DD") {
      result$time <- data.frame(year = as.numeric(substring(result$data$date,1,4)),
                                month = as.numeric(substring(result$data$date,6,7)),
                                day = as.numeric(substring(result$data$date,9,10)))   
    }
    else if (date.format=="DD/MM/YYYY") {
      result$time <- data.frame(year = as.numeric(substring(result$data$date,7,10)),
                                month = as.numeric(substring(result$data$date,4,5)),
                                day = as.numeric(substring(result$data$date,1,2)))      
    }
    else {stop ("El formato de la Fecha no se corresponde con los admitidos")}
    
    if (time.format=="HHMM" | time.format=="HHMMSS") {
      result$time$hour <-  as.numeric(substring(pref0(result$data$time,4),1,2))
      result$time$minute <- as.numeric(substring(pref0(result$data$time,4),3,4))
      
    }
    else if (time.format=="HH:MM" | time.format=="HH:MM:SS" | time.format=="HH.MM" | time.format=="HH.MM.SS") {
      result$time$hour <-  as.numeric(substring(pref0(result$data$time,5),1,2))
      result$time$minute <- as.numeric(substring(pref0(result$data$time,5),4,5))
    }
    else {stop ("El formato de la Hora no se corresponde con los admitidos")}
    result$time$dt <- as.POSIXct(paste(result$time$year,".",pref0(result$time$month,2),".",pref0(result$time$day,2)," ",pref0(result$time$hour,2),":",pref0(result$time$Min,2),":00",sep=""), "%Y.%m.%d %H:%M:%S", tz="")
    result$time$Q <- floor((result$time$month/3)-.01)+1
    
    # $speed  
    if (is.na(ane.names)[1]) ane.names="ane1"
    result$nane<-length(ane.names)
    
    result$ane<-list()
    for (i in ane.names){
      j<-which(ane.names==i,arr.ind = T)
      result$ane[[i]] <- data.frame(ave = data[,speed.ave.var[j]])
      if (!is.na(speed.min.var[j])) {result$ane[[i]]$min<-data[,speed.min.var[j]]}
      if (!is.na(speed.max.var[j])) {result$ane[[i]]$max<-data[,speed.max.var[j]]}
      if (!is.na(speed.var.var[j])) {result$ane[[i]]$sd<-sqrt(data[,speed.var.var[j]])}
      if (!is.na(speed.sd.var[j])) {result$ane[[i]]$sd<-data[,speed.sd.var[j]]}
    }
    
    # $dir
    dir <- data[,dir.var] + (360/12)/2
    dir[which(dir>=360)]<-dir[which(dir>=360)]-360
    rose_dir <- c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW")
    result$dir <- data.frame (value=data[,dir.var],
                              sect_12=floor(dir/(360/12))+1,
                              ang_12=((floor(dir/(360/12))+1)-1)*360/12,
                              sect_16=floor(dir/(360/16))+1,
                              ang_16=(floor(dir/(360/16))+1)*(360/16),
                              rose=factor(rose_dir[floor(dir/(360/16))+1],
                                          levels=rose_dir)
    )
    
    # $par
    if (!is.na(temp.var)) {
      result$par$temp$value<-data[,temp.var]
      result$par$temp$unit<-temp.unit
    }
    if (!is.na(pres.var)) {
      result$par$pres$value<-data[,pres.var]
      result$par$pres$unit<-pres.unit
    }
    result$info$interval <- interval
    result$info$unit <- data.frame(speed=speed.unit,
                                   dir=dir.unit,
                                   temp=temp.unit,
                                   pres=pres.unit) 
    result$info$ane$nane <- length(ane.names)
    result$info$ane$ane.names <- ane.names
    result$info$ane$height <- ane.height
    
    class(result) <- "windata"
    
    result
  }

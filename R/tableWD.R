#' @title Summary Tables for \code{Windata} objects
#' 
#' @description
#' This function returns the values plotted with \code{plotWD} function.
#' 
#' @param datawd an object of class \code{windata}.
#' @param ane a vector of character strings with anemometers names to plot.
#' @param var a vector of character strings the the variables to plot: \code{ave}, \code{min}, \code{max}, \code{freq}.
#' @param type type of plot: \code{histogram}, \code{rose}, \code{profiles} and \code{boxplot}. See also Details.
#' @param by an optional string stating if the plot is divided in panels by \code{month} or \code{hour}. 
#' @param since an optional string with initial date to be taken into account to make the plot. The string format is 'YYYY-MM-DD'.
#' @param to an optional string indicating final date to be taken into account to make the plot. The string format is 'YYYY-MM-DD'.
#' @param binwidth an optional bindwidth interval used in histogram plots.
#' 
#' @author Mariano Bonoli Escobar, Diego Edwards, Valeria Gogni, Ruben Bufanio
#' 
#' @importFrom data.table data.table
#' @importFrom data.table setnames
#' 
#' @export
#' 
#' @examples
#' data("wdMtTom", package = "WindResource")
#' data("wd10", package = "WindResource")
#' 
#' ## Getting anemometer names
#' wdMtTom$ane$ane.names
#' wd10$ane$ane.names
#' 
#' ## Histogram
#' tableWD(data=wdMtTom, ane="Anem24bMS", type="histogram", by="none")
#' tableWD(data=wdMtTom, ane="Anem24bMS", type="histogram", by="none", binwidth=.5)
#' tableWD(data=wdMtTom, ane="Anem24bMS", type="histogram", by="hour")
#' 
#' ## Rose
#' tableWD (data=wdMtTom, ane=c("Anem24bMS", "Anem37aMS"), var="mean", type="rose", by="none")
#' tableWD (data=wdMtTom, ane=c("Anem37aMS"), var="mean", type="rose", by="month")
#' 
#' ## Boxplot
#' tableWD (data=wdMtTom, ane="Anem24bMS", type="boxplot", by="hour")
#' tableWD (data=wdMtTom, ane="Anem24bMS", type="boxplot", by="month")
#' 
#' ## Profile
#' tableWD (data=wdMtTom, ane="Anem24aMS", var="ave", type="profile", by="hour")
#' tableWD (data=wdMtTom,  ane="Anem24aMS",  var="ave", type="profile", by="month")
#' 
#' ## Turbulence Analysis
#' tableWD (data=wd10, ane="ane10", type="turbulence")
#' 
#' ## Fit
#' tableWD (data=wdMtTom, ane="Anem37aMS", type="fit")
#' 
#' @seealso \code{\link{plotWD}}

#' 


tableWD <- function(datawd, ane = NA, var = c("mean"), type = "histogram", 
                    by = "none", since = NULL, to = NULL, binwidth = 1) {
  
  require(WindResource)
  
  rose_dir <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", 
                "SW", "WSW", "W", "WNW", "NW", "NNW")
  month.names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                   "Oct", "Nov", "Dec")
  hour.names2 <- c("00:00 - 01:59", "02:00 - 03:59", "04:00 - 05:59", "06:00 - 07:59", 
                   "08:00 - 09:59", "10:00 - 11:59", "12:00 - 13:59", "14:00 - 15:59", "16:00 - 17:59", 
                   "18:00 - 19:59", "20:00 - 21:59", "22:00 - 23:59")
  hour.names <- pref0(0:23, 2)
  
  if (class(datawd) != "windata") 
    stop("datawd is not windata class objetc")
  
  # Checks
  if (sum(ane %in% datawd$ane$ane.names) != length(ane) & !is.na(ane)[1]) 
    stop("The anemometer names don't match with de dataset")
  
  # Checks Turbulence
  if (type=="turbulence"){
    if (is.na(ane)[1])
      if (datawd[["ane"]][["nane"]]!=1) {
        stop("Debe indicar el nombre del anemometro")
      } else {
        ane <- datawd[["ane"]][["ane.names"]]
      }
    print(ane)
    if (is.null(datawd[["ane"]][[ane]][["sd"]]))
      stop("No se cuenta con informacion de desvios estandar")
    if (datawd[["interval.minutes"]]!=10)
      stop("No se cuenta con informacion diezminutal")
  }
  
  
  # ane
  if (is.na(ane)[1]) {
    if (type != "rose" & type !="roughness") {
      stop("The 'ane' parameter is mandatory.")
    } else {
      ane <- datawd[["ane"]][["ane.names"]]
    }
  }
  
  # Control anemometros browser()
  if (is.na(ane[1])) {
    ane.names <- names(datawd$ane)
  } else {
    ane.names <- ane
  } 
  
  # Apply date filter
  if (!is.null(since)) {
    date.since <- as.POSIXct(since)
    date.to <- as.POSIXct(to)
    valid.cases <- (datawd$time$dt >= date.since) & (datawd$time$dt <= date.to) & 
      !is.na(datawd$time$dt)
    datawd[["time"]] <- datawd[["time"]][valid.cases, ]
    datawd[["dir"]] <- datawd[["dir"]][valid.cases, ]
    for (i in names(datawd[["ane"]])) {
      datawd[["ane"]][[i]] <- datawd[["ane"]][[i]][valid.cases, ]
    }
  }
  
  if (type == "histogram") {
    if (by == "none") {
      dp <- data.frame(mean = datawd$ane[[ane]]$ave)
      histo <- hist(dp$mean, breaks = seq(0, max(as.numeric(dp$mean), 
                                                 na.rm = T) + 1, by = binwidth), plot = F)
      result <- data.frame(Lower = histo$breaks[-length(histo$breaks)], 
                           Upper = histo$breaks[-1], Freq = histo$counts)
    } else if (by == "month") {
      dp <- data.frame(mean = datawd$ane[[ane]]$ave, month = month.names[datawd$time$month])
      histo <- hist(dp$mean, breaks = seq(0, max(as.numeric(dp$mean), 
                                                 na.rm = T) + 1, by = binwidth), plot = F)
      breaks <- histo$breaks
      result <- data.frame(Lower = histo$breaks[-length(histo$breaks)], 
                           Upper = histo$breaks[-1])
      for (m in month.names) {
        dpm <- subset(dp, month == m)
        histo <- hist(dpm$mean, breaks = breaks, plot = F)
        result[m] <- histo$counts
      }
    } else if (by == "hour") {
      dp <- data.frame(mean = datawd$ane[[ane]]$ave, hour = factor(hour.names2[floor(datawd$time$hour/2) + 
                                                                                 1], levels = hour.names2))
      histo <- hist(dp$mean, breaks = seq(0, max(as.numeric(dp$mean), 
                                                 na.rm = T) + 1, by = binwidth), plot = F)
      breaks <- histo$breaks
      result <- data.frame(Lower = histo$breaks[-length(histo$breaks)], 
                           Upper = histo$breaks[-1])
      for (h in hour.names2) {
        dpm <- subset(dp, hour == h)
        histo <- hist(dpm$mean, breaks = breaks, plot = F)
        result[h] <- histo$counts
      }
    }
  }
  else if (type == "rose") {
    dfall <- data.frame()
    j <- switch(var, mean=1, min=2, max=3)
    
    for (i in ane) {
      dfall <- rbind(dfall, data.frame(value.start = datawd$ane[[i]][, j], 
                                       rose = as.factor(datawd$ane[[i]][, "rose"]), 
                                       ang.start = datawd$ane[[i]][, "ang_16"], 
                                       ane = i, month = factor(month.names[datawd$time$month], 
                                                               levels = month.names), 
                                       hour = factor(hour.names2[floor(datawd$time$hour/2) + 1]),
                                       freq =1))          
    }
    if (var!="freq") {
      dataplot <- switch(by,
                         none={aggregate(value.start ~ rose + ane + ang.start, data = dfall, FUN = var)},
                         month={aggregate(value.start ~ rose + ane + ang.start + month, data = dfall, FUN = var)},
                         hour={aggregate(value.start ~ rose + ane + ang.start + hour, data = dfall, FUN = var)})
    } else {
      dataplot <- switch(by,
                         none={aggregate(freq ~ rose + ane + ang.start, data = dfall, FUN = sum)},
                         month={aggregate(freq ~ rose + ane + ang.start + month, data = dfall, FUN = sum)},
                         hour={aggregate(freq ~ rose + ane + ang.start + hour, data = dfall, FUN = sum)})
      names(dataplot)[names(dataplot)=="freq"]<- "value.start"
    }
    result <- dataplot
    
  }
  else if (type == "correlation") {
    if (nane != 2) 
      stop("There must be 2 anemometers to generate a correlation plot.")
    df <- data.frame(x = datawd$ane[[ane[1]]]$ave, y = datawd$ane[[ane[2]]]$ave)
    df <- df[complete.cases(df), ]
    result <- df
  }
  else if (type == "profile") {
    for (i in ane) {
      print(str(datawd$ane[[i]]))
      df <- rbind(df, data.frame(ave = datawd$ane[[i]]$ave, 
                                 ane = i, 
                                 month = factor(month.names[datawd$time$month], levels = month.names),
                                 hour = factor(hour.names[datawd$time$hour + 1], levels = hour.names)))
    }
    if (by == "month") {
      dataplot <- aggregate(ave ~ ane + month, data = df, switch(var, mean=mean,min=min,max=max))     
      # Esto es por si faltan meses
      result <- merge(expand.grid(ane = ane, month = month.names), dataplot, all.x=T)
    } else if (by == "hour") {
      dataplot <- aggregate(ave ~ ane + hour, data = df, switch(var, mean=mean,min=min,max=max))
      # Esto es por si faltan horarios
      result <- merge(expand.grid(ane = ane, hour = hour.names), dataplot, all.x=T)
    } else stop(paste("Profiles tables requires that the By parameter takes values 'month' or 'hour'.", by) )
  }
  else if (type == "boxplot") {
    result <- list()
    df <- data.frame(hour = datawd$time$hour, day = datawd$time$day, month = datawd$time$month, 
                     ave = datawd$ane[[ane]]$ave)  
    a <- tapply(df$ave, df[[by]], summary)
    desv <- tapply(df$ave, df[[by]], sd, na.rm = TRUE)
    for (j in 1:length(table(df[by]))) {
      if (length(a[[j]]) < 7) {
        a[[j]][7] <- 0
      }
    } 
    result <- data.frame(t(sapply(a, c)), round(desv, 4))
    colnames(result) <- c("Min", "1st Qu", "Median", "Mean", "3rd Qu.", 
                          "Max.", "Nas", "DS")
  } 
  # Roughness
  else if (type == "roughness") {
    result <- 
      datawd$ane[[datawd$ane$ane.names[1]]] %>% 
      filter(!is.na(ang_16)) %>% 
      group_by(ang_16) %>% 
      summarize(u1 = mean(ave, na.rm = T)) %>% 
      mutate(h1 = datawd$ane$height[1]) %>% 
      left_join(
        datawd$ane[[datawd$ane$ane.names[2]]] %>% 
          filter(!is.na(ang_16)) %>% 
          group_by(ang_16) %>% 
          summarize(u2 = mean(ave, na.rm = T)) %>% 
          mutate(h2 = datawd$ane$height[2])) %>% 
      mutate(z0 = exp((u1*log(h2) - u2*log(h1))/(u2-u1))) %>% 
      as.data.frame()
  } else if (type == "turbulence") {
    df <- data.frame(ave = datawd[["ane"]][[ane]][["ave"]], sd = datawd[["ane"]][[ane]][["sd"]])
    df$I <- df$sd/df$ave * 100
    df$bin <- floor(df$ave + 0.5)
    df$count <- 1
    DT <- data.table(df[df$bin>=1,])
    dataplot <- DT[,list(count=sum(count),I=mean(I)),by=c("bin" )]
    setnames(dataplot,"bin","windspeed")
    dataplot <- dataplot[order(dataplot$windspeed)]    
    ref.point <- data.frame(x = c(15, 15, 15), y = c(16, 14, 12), ref = c("A - High Turbulence characteristics", 
                                                                          "B - Medium Turbulence characteristics", "C - Low Turbulence characteristics"))
    
    result <- list(Iref = data.frame(Iref = mean(df[df$ave >= 14.5 & df$ave <= 
                                                      15.5, "I"], na.rm = T)), data_turbulence = dataplot)
  } 
  else if (type == "fit") {
    param <- fitWD(datawd, ane = ane)
    # Building the table
    result <- data.frame(cbind(Parameter1 = c(paste("k=", round(param$K, 
                                                                digits = 4)), paste("alpha=", round(param$alpha, digits = 4)), 
                                              paste("m=", round(param$meanlog, digits = 4))), Parameter2 = c(paste("C=", 
                                                                                                                   round(param$A, digits = 4)), paste("beta=", round(param$beta, digits = 4)), 
                                                                                                             paste("D=", round(param$sdlog, digits = 4))), loglik = c(floor(param$loglik.wei), 
                                                                                                                                                                      floor(param$loglik.ga), floor(param$loglik.ln)), aic = c(floor(param$aic.wei), 
                                                                                                                                                                                                                               floor(param$aic.ga), floor(param$aic.ln))), row.names = c("Weibull", 
                                                                                                                                                                                                                                                                                         "Gamma", "Lognormal"))
  } 
  
  else {
    stop(paste("invalid plot type '", type, "'.", sep = ""))
  }
  
  result
} 

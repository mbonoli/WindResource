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
#' @param var currently only method = 'qr' is supported
#' @param type the type of graphic to plot. Actually soported: 'histogram', 'rose', 
#'        'correlogram', 'profiles' and 'boxplot'. See also 'Details'.
#' @param by an optional string stating if the plot is divided in panels by 'month' or 'hour'. 
#' @param since an optional string indicating initial date to be taken into account to make the plot.
#'        The string format is 'YYYY-MM-DD'.
#' @param to an optional string indicating final date to be taken into account to make the plot.
#'        The string format is 'YYYY-MM-DD'.
#' @return Object of class 'windata' (see details)
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


tableWD <- function(datawd, ane = NA, var = c("mean"), type = c("histogram"), by = c("none", 
                                                                                     "month", "hour"), since = NULL, to = NULL, binwidth = 1) {
  
  if (class(datawd) != "windata") 
    stop("Los datos no correponden a la clase 'windata'.")
  
  # Checks
  if (sum(ane %in% datawd$ane$ane.names) != length(ane) & !is.na(ane)[1]) 
    stop("The anemometer names don't match with de dataset")
  
  # Checks Turbulence
  if (type=="turbulence"){
    if (is.na(ane)[1])
      if (wd[["ane"]][["nane"]]!=1) {
        stop("Debe indicar el nombre del anemometro")
      } else {
        ane <- wd[["ane"]][["ane.names"]]
      }
    print(ane)
    if (is.null(wd[["ane"]][[ane]][["sd"]]))
      stop("No se cuenta con información de desvíos estándar")
    if (wd[["interval.minutes"]]!=10)
      stop("No se cuenta con información diezminutal")
  }
  
  # ane
  if (is.na(ane)[1]) {
    if (type!="rose") {
      stop("The 'ane' parameter is mandatory.")
    } else {
      ane.names <- datawd[["ane"]][["ane.names"]]
    }
  }
  else if (length(ane)>1) {
    if (type!="rose") {
      stop("The 'ane' parameter can't contain more than one value.")
    }
  }
  
  
  require(reshape2)
  
  rose_dir <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", 
                "SW", "WSW", "W", "WNW", "NW", "NNW")
  month.names3 <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                    "Oct", "Nov", "Dec")
  hour.names2 <- c("00:00 - 01:59", "02:00 - 03:59", "04:00 - 05:59", "06:00 - 07:59", 
                   "08:00 - 09:59", "10:00 - 11:59", "12:00 - 13:59", "14:00 - 15:59", "16:00 - 17:59", 
                   "18:00 - 19:59", "20:00 - 21:59", "22:00 - 23:59")
  hour.names <- pref0(0:23, 2)
  
  
  
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
  
  # Control anemometros browser()
  if (is.na(ane[1])) {
    ane.names <- names(datawd$ane)
  } else {
    ane.names <- ane
  }
  
  if (type == "histogram") {
    if (by == "none") {
      result <- list()
      for (i in ane.names) {
        dp <- data.frame(mean = datawd$ane[[i]]$ave)
        histo <- hist(dp$mean, breaks = seq(0, max(as.numeric(dp$mean), 
                                                   na.rm = T) + 1, by = binwidth), plot = F)
        result[[i]] <- data.frame(Lower = histo$breaks[-length(histo$breaks)], 
                                  Upper = histo$breaks[-1], Freq = histo$counts)
      }
    } else if (by == "month") {
      result <- list()
      for (i in ane.names) {
        dp <- data.frame(mean = datawd$ane[[i]]$ave, month = month.names3[datawd$time$month])
        histo <- hist(dp$mean, breaks = seq(0, max(as.numeric(dp$mean), 
                                                   na.rm = T) + 1, by = binwidth), plot = F)
        breaks <- histo$breaks
        result[[i]] <- data.frame(Lower = histo$breaks[-length(histo$breaks)], 
                                  Upper = histo$breaks[-1])
        for (m in month.names3) {
          dpm <- subset(dp, month == m)
          histo <- hist(dpm$mean, breaks = breaks, plot = F)
          result[[i]][m] <- histo$counts
        }
      }
    } else if (by == "hour") {
      result <- list()
      for (i in ane.names) {
        dp <- data.frame(mean = datawd$ane[[i]]$ave, hour = factor(hour.names2[floor(datawd$time$hour/2) + 
                                                                                 1], levels = hour.names2))
        histo <- hist(dp$mean, breaks = seq(0, max(as.numeric(dp$mean), 
                                                   na.rm = T) + 1, by = binwidth), plot = F)
        breaks <- histo$breaks
        result[[i]] <- data.frame(Lower = histo$breaks[-length(histo$breaks)], 
                                  Upper = histo$breaks[-1])
        for (h in hour.names2) {
          dpm <- subset(dp, hour == h)
          histo <- hist(dpm$mean, breaks = breaks, plot = F)
          result[[i]][h] <- histo$counts
        }
      }
    }
  } 
  else if (type == "rose") {
    
    dfall <- data.frame()
    
    if (var == "mean") {
      j = 1
    } else if (var == "min") {
      j = 2
    } else if (var == "max") {
      j = 3
    } else {
      j = 4
    }
    
    for (i in ane) {
      dfall <- rbind(dfall, data.frame(speed.start = datawd$ane[[i]][, j], 
                                       rose = as.factor(datawd$ane[[i]][, "rose"]), 
                                       ang.start = datawd$ane[[i]][, "ang_16"], 
                                       ane = i, month = factor(month.names3[datawd$time$month], 
                                                               levels = month.names3), hour = factor(hour.names2[floor(datawd$time$hour/2) + 1])))
      
      
      
    }
    
    dfs <- list()
    result <- list()
    #selcol <- ifelse(var == "mean", "ave", var)
    
    if (var == "frec") {
      
      df <- data.frame()
      t <- list()
      
      if (by == "none") {
        
        for (i in ane) {
          
          result[[i]] <- data.frame(table(rose=dfall$rose))
          #, ane = i, ang.start = c(90, 67.5, 92.5, 0, 45, 22.5, 337.5, 315, 180, 135, 157.5, 202.5, 225, 270, 292.5, 247.5))
          result[[i]]["Freq"] <- result[[i]]["Freq"]/sum(result[[i]]["Freq"])
          #df <- rbind(df, t[[j]])                                                 
          
        }
        
      } else if (by == "month") {
        
        for (i in ane) {
          
          result[[i]] <- data.frame(table(rose = dfall$rose, month = dfall$month), 
                                    ane = i, ang.start = c(90, 67.5, 92.5, 0, 45, 22.5, 337.5, 315, 
                                                           180, 135, 157.5, 202.5, 225, 270, 292.5, 247.5))
          result[[i]]["Freq"] <- t[[i]]["Freq"]/sum(t[[i]]["Freq"])
          df <- rbind(df, t[[i]])
          
        } 
        
      } else if (by == "hour") {
        
        for (i in ane) {
          # browser()
          result[[i]] <- data.frame(table(rose = dfall$rose, hour = dfall$hour), 
                                    ane = i, ang.start = c(90, 67.5, 92.5, 0, 45, 22.5, 337.5, 315, 
                                                           180, 135, 157.5, 202.5, 225, 270, 292.5, 247.5))
          result[[i]]["Freq"] <- t[[i]]["Freq"]/sum(t[[i]]["Freq"])
          df <- rbind(df, t[[i]])
          
        }
      }
      
    } else {
      
      for (i in ane.names) {
        dfs[[i]] = data.frame(ave = datawd$ane[[i]][, j], rose = datawd$ane[[i]]$rose, 
                              month = datawd$time$month, hour = factor(hour.names2[floor(datawd$time$hour/2) + 1]))
        
      }
      
      for (i in ane.names) {
        if (by == "none") {
          result[[i]] <- aggregate(ave ~ rose, data = dfs[[i]], FUN=var)
          
        } else if (by == "month") {
          result[[i]] <- aggregate(ave ~ rose + month, data = dfs[[i]], FUN=var)
          result[[i]] <- dcast(result[[i]], rose ~ month, mean, value.var = "ave")
          
        } else if (by == "hour") {
          result[[i]] <- aggregate(ave ~ rose + hour, data = dfs[[i]], FUN=var)
          result[[i]] <- dcast(result[[i]], rose ~ hour, mean, value.var = "ave")
        }
      }
    }
  }
  else if (type == "correlation") {
    if (nane != 2) 
      stop("There must be 2 anemometers to generate a correlation plot.")
    df <- data.frame(x = datawd$ane[[ane[1]]]$ave, y = datawd$ane[[ane[2]]]$ave)
    df <- df[complete.cases(df), ]
    result <- df
  }
  else if (type == "profile") {
    dfs <- list()
    result <- list()
    for (i in ane) {
      dfs[[i]] = data.frame(ave = datawd$ane[[i]]$ave, month = factor(month.names3[datawd$time$month], 
                                                                      levels = month.names3), hour = factor(hour.names[datawd$time$hour + 
                                                                                                                         1], levels = hour.names))
    }
    # browser()
    for (i in ane) {
      if (by == "month") {
        result[[i]] <- aggregate(ave ~ month, data = dfs[[i]], mean)
        # Esto es por si faltan meses
        result[[i]] <- join(expand.grid(month = month.names3), result[[i]], 
                            type = "left")
      } else if (by == "hour") {
        result[[i]] <- aggregate(ave ~ hour, data = dfs[[i]], mean)
        # Esto es por si faltan horarios
        result[[i]] <- join(expand.grid(hour = hour.names), result[[i]], 
                            type = "left")
      } else {
        stop(paste("invalid plot type '", type, "'.", sep = ""))
      }
    }
    
    
  } 
  else if (type == "turbulence") {
    require(sqldf)
    print(ane)
    df <- data.frame(ave = wd[["ane"]][[ane]][["ave"]], sd = wd[["ane"]][[ane]][["sd"]])
    df$I <- df$sd/df$ave * 100
    df$bin <- floor(df$ave + 0.5)
    dataplot <- sqldf("select bin widspeed, count(*) count, avg(I) I from df where bin>=1 group by bin")
    
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
  else if (type == "boxplot") {
    
    result <- list()
    
    for (i in ane.names) {
      
      df <- data.frame(hour = datawd$time$hour, day = datawd$time$day, month = datawd$time$month, 
                       ave = datawd$ane[[i]]$ave)
      
      a <- tapply(df$ave, df[[by]], summary)
      
      desv <- tapply(df$ave, df[[by]], sd, na.rm = TRUE)
      
      for (j in 1:length(table(df[by]))) {
        
        if (length(a[[j]]) < 7) {
          a[[j]][7] <- 0
        }
      }
      
      
      result[[i]] <- data.frame(t(sapply(a, c)), round(desv, 4))
      colnames(result[[i]]) <- c("Min", "1st Qu", "Median", "Mean", "3rd Qu.", 
                                 "Max.", "Nas", "DS")
      
    }
  } 
  else {
    stop(paste("invalid plot type '", type, "'.", sep = ""))
  }
  
  result
} 

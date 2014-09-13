#' @title Plots for Winddata objects
#' 
#' @description
#' Shape a dataframe in a object which will be use by the diferents functions in the package......
#' 
#' @details
#' The object windata is a list with the parameters that were mencionated before.
#' 
#' @param datawd an object of class winddata.
#' @param ane a vector of character strings containing anemometers names to plot. 
#' @param var a vector of character strings containing the variables to plot.
#' @param type type of plot: 'histogram', 'rose', 
#'        'profiles' and 'boxplot'. See also Details.
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
#' str(wd)
#' 


plotWD <- function(datawd, ane = NA, var = NA, type = c("histogram"), 
                   by = c("none", "month", "hour"), since = NULL, to = NULL) {
  
  if (class(datawd) != "windata") 
    stop("Los datos no correponden a la clase 'windata'.")
  
  # Checks
  if (sum(ane %in% datawd$ane$ane.names) != length(ane) & !is.na(ane)[1]) 
    stop("The anemometer names don't match with de dataset")
  
  # Checks Turbulence
  if (type=="turbulence"){
    if (is.na(ane))
      if (datawd[["ane"]][["nane"]]!=1) {
        stop("Debe indicar el nombre del anemometro")
      } else {
        ane <- datawd[["ane"]][["ane.names"]]
      }
    if (is.null(datawd[["ane"]][[ane]][["sd"]]))
      stop("No se cuenta con informacion de desvios estandar")
    if (datawd[["interval.minutes"]]!=10)
      stop("No se cuenta con informacion diezminutal")
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
    if (type!="rose" & type!="profile") {
      stop("The 'ane' parameter can't contain more than one value.")
    }
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
  
  month.names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                   "Oct", "Nov", "Dec")
  hour.names2 <- c("00:00 - 01:59", "02:00 - 03:59", "04:00 - 05:59", "06:00 - 07:59", 
                   "08:00 - 09:59", "10:00 - 11:59", "12:00 - 13:59", "14:00 - 15:59", "16:00 - 17:59", 
                   "18:00 - 19:59", "20:00 - 21:59", "22:00 - 23:59")
  hour.names <- pref0(0:23, 2)
  
  if (class(datawd) != "windata") 
    stop("Los datos no correponden a la clase 'windata'.")
  
  if (type == "histogram") {
    if (by == "none") {
      dp <- data.frame(mean = datawd$ane[[ane]]$ave)
      print(ggplot(dp, aes(x = mean)) + geom_histogram(colour = "black", fill = "blue",...))
    }
    else if (by == "month") {
      ds <- data.frame(mean = datawd$ane[[ane]]$ave, month = factor(month.names[datawd$time$month], 
                                                                    levels = month.names))
      print(ggplot(ds, aes(x = mean)) + geom_histogram(...,
                                                       colour = "black", fill = "blue") + facet_wrap(~month, ncol = 3, 
                                                                                                     drop = F))
    } else if (by == "hour") {
      ds <- data.frame(mean = datawd$ane[[ane]]$ave, hour = factor(hour.names2[floor(datawd$time$hour/2) + 
                                                                                 1], levels = hour.names2))
      print(ggplot(ds, aes(x = mean)) + geom_histogram(...,
                                                       colour = "black", fill = "blue") + facet_wrap(~hour, ncol = 3, 
                                                                                                     drop = F))
    }
  } 
  else if (type == "rose") {
    
    dfall <- data.frame()
    #print(var)
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
    dataplot$ang.end <- ifelse(dataplot$ang.start + 22.5 >= 360, dataplot$ang.start - 
                                 337.5, dataplot$ang.start + 22.5)
    dataplot$ang.end[dataplot$ang.end == 0] <- 360
    
    switch(by,
           none={dataplot$value.end <- dataplot[apply(dataplot, 1, function(x) ifelse(length(which(as.numeric(dataplot$ang.start) == 
                                                                                                     as.numeric(x["ang.end"]) & dataplot$ane == x["ane"])) == 0, NA, which(as.numeric(dataplot$ang.start) == as.numeric(x["ang.end"]) &                                                                                                                                                               
                                                                                                                                                                             dataplot$ane == x["ane"]))), "value.start"]},
           month={dataplot$value.end <- dataplot[apply(dataplot, 1, function(x) ifelse(length(which(as.numeric(dataplot$ang.start) == 
                                                                                                      as.numeric(x["ang.end"]) & dataplot$ane == x["ane"])) == 0, NA, which(as.numeric(dataplot$ang.start) == as.numeric(x["ang.end"]) &                                                                                                                                                               
                                                                                                                                                                              dataplot$ane == x["ane"] & dataplot$month == x["month"]))), "value.start"]},
           hour={dataplot$value.end <- dataplot[apply(dataplot, 1, function(x) ifelse(length(which(as.numeric(dataplot$ang.start) == 
                                                                                                     as.numeric(x["ang.end"]) & dataplot$ane == x["ane"])) == 0, NA, which(as.numeric(dataplot$ang.start) == as.numeric(x["ang.end"]) &                                                                                                                                                               
                                                                                                                                                                             dataplot$ane == x["ane"] & dataplot$hour == x["hour"]))), "value.start"]})
    dataplot <- add.cart.coord(dataplot)
    maxi <- max(dataplot$value.start)

    plotobj <- polar.theme(ggplot(data = dataplot), maxi = maxi) + geom_segment(data = dataplot, mapping = aes(x = x, y = y, xend = xend, yend = yend, color = ane, group = ane), size = 1)
    switch(by,
           month={plotobj <- plotobj + facet_wrap(~month, ncol = 4, drop = F)},
           hour={plotobj <- plotobj + facet_wrap(~hour, ncol = 4, drop = F)})
    plotobj
  }
  else if (type == "correlation") {
    df <- data.frame(x = datawd$ane[[1]]$ave, y = datawd$ane[[2]]$ave)
    df <- df[complete.cases(df), ]
    ggplot(data = df, aes(x = x, y = y)) + geom_point(size = 1) + ggtitle("") + 
      xlab(paste("Wind Speed ", datawd$info$ane$height[1], "m [", datawd$info$unit$speed, 
                 "]", sep = "")) + ylab(paste("Wind Speed ", datawd$info$ane$height[2], 
                                              "m [", datawd$info$unit$speed, "]", sep = ""))
  } 
  else if (type == "profile") {
    df <- data.frame()
    for (i in ane) {
      df <- rbind(df, data.frame(ave = datawd$ane[[i]]$ave, ane = i, month = factor(month.names[datawd$time$month], 
                                                                                    levels = month.names), hour = factor(hour.names[datawd$time$hour + 
                                                                                                                                      1], levels = hour.names)))
    }
    if (by == "month") {
      dataplot <- aggregate(ave ~ ane + month, data = df, switch(var, ave=mean,min=min,max=max))     
      # Esto es por si faltan meses
      dataplot <- merge(expand.grid(ane = ane, month = month.names), dataplot, all.x=T)
      ggplot(dataplot, aes(x = month, y = ave, color = ane, group = ane)) + 
        geom_line(size = 1) + labs(title = "Profile") + ylab("Speed(m/S)") + 
        scale_y_continuous(limits = c(0, max(dataplot$ave, na.rm = T) * 
                                        1.2)) + xlab("Month")
    } else if (by == "hour") {
      dataplot <- aggregate(ave ~ ane + hour, data = df, switch(var, ave=mean,min=min,max=max))
      # Esto es por si faltan horarios
      dataplot <- merge(expand.grid(ane = ane, hour = hour.names), dataplot, all.x=T)
      ggplot(dataplot, aes(x = hour, y = ave, color = ane, group = ane)) + 
        geom_line(size = 1) + labs(title = "Profile") + ylab("Speed(m/S)") + 
        scale_y_continuous(limits = c(0, max(dataplot$ave, na.rm = T) * 
                                        1.2)) + xlab("Hour")
    } else stop(paste("Profiles plots requires that the By parameter takes values 'month' or 'hour'.", by) )
  }
  else if (type == "boxplot") {
    dfbox <- data.frame(hour = datawd$time$hour, day = datawd$time$day, 
                        month = datawd$time$month, ave = datawd$ane[[ane]]$ave)
    if (by == "hour") {
      print(ggplot(dfbox, aes(factor(hour), ave)) + geom_boxplot())
    } else if (by == "day") {
      print(ggplot(dfbox, aes(factor(day), ave)) + geom_boxplot())
    } else if (by == "month") {
      print(ggplot(dfbox, aes(factor(month), ave)) + geom_boxplot())
    }
  } 
  else if (type == "turbulence") {
    df <- data.frame(ave = wd[["ane"]][[ane]][["ave"]], sd = wd[["ane"]][[ane]][["sd"]])
    df$I <- df$sd/df$ave * 100
    df$bin <- floor(df$ave + 0.5)
    df$count <- 1
    DT <- data.table(df[df$bin>=1,])
    dataplot <- DT[,list(count=sum(count),I=mean(I)),by=c("bin" )]
    setnames(dataplot,"bin","windspeed")
    dataplot <- dataplot[order(windspeed)]   
    ref.point <- data.frame(x = c(15, 15, 15), y = c(16, 14, 12), ref = c("A - High Turbulence characteristics", 
                                                                          "B - Medium Turbulence characteristics", "C - Low Turbulence characteristics"))
    
    c <- ggplot(dataplot, aes(x = windspeed, y = I))
    print(c + geom_line(size = 1) + coord_cartesian(xlim = c(0, 17), ylim = c(0, 
                                                                              35)) + xlab("Vhub [m/s]") + ylab("Turbulence Intensity [%]") + geom_point(data = ref.point, 
                                                                                                                                                        mapping = aes(x = x, y = y, group = ref, color = ref), size = 5) + 
            scale_x_continuous(breaks = 1:16) + scale_y_continuous(breaks = (1:7) * 
                                                                     5) + theme(axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", 
                                                                                                                                                                   size = 12)))
  } 
  else if (type == "fit") {
    param <- fitWD(datawd, ane = ane)
    data1 <- c(0, param$data)
    y.wei <- dweibull(data1, shape = param$K, scale = param$A)
    y.ga <- dgamma(data1, shape = param$alpha, scale = param$beta)
    y.ln <- dlnorm(data1, meanlog = param$meanlog, sdlog = param$sdlog)
    dr <- rbind(data.frame(dist = "wei", mean = data1, ajust = y.wei), 
                data.frame(dist = "ga", mean = data1, ajust = y.ga), data.frame(dist = "ln", 
                                                                                mean = data1, ajust = y.ln))
    dw <- rbind(data.frame(dist = "wei", mean = data1, ajust = y.wei))
    dg <- rbind(data.frame(dist = "ga", mean = data1, ajust = y.ga))
    dl <- rbind(data.frame(dist = "ln", mean = data1, ajust = y.ln))
    distr <- c("wei", "ga", "ln")
    tittle <- c("Weibull Distribution", "Gamma Distribution", "Lognormal Distribution")
    namep1 <- c("K", "alpha", "m")
    para1 <- c(param$K, param$alpha, param$meanlog)
    namep2 <- c("A", "beta", "D")
    para2 <- c(param$A, param$beta, param$sdlog)
    lo <- c(param$loglik.wei, param$loglik.ga, param$loglik.ln)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(2, 3)))
    vplayout <- function(x, y) viewport(layout.pos.col = x, layout.pos.row = y)
    for (i in 1:3) {
      print(ggplot(dr[dr$dist == distr[i], ], aes(x = mean)) + geom_histogram(aes(y = ..density..), 
                                                                              binwidth = 1, colour = "black", fill = "blue") + ggtitle(tittle[i]) + 
              geom_line(aes(y = ajust), colour = "red", size = 1) + annotate("text", 
                                                                             x = max(data1) * 0.8, y = 0.11, label = paste(namep1[i], " = ", 
                                                                                                                           round(para1[i], digits = 4)), size = 3.5) + annotate("text", 
                                                                                                                                                                                x = max(data1) * 0.8, y = 0.09, label = paste(namep2[i], " = ", 
                                                                                                                                                                                                                              round(para2[i], digits = 4)), size = 3.5) + annotate("text", 
                                                                                                                                                                                                                                                                                   x = max(data1) * 0.8, y = 0.07, label = paste("Loglik =", round(lo[i], 
                                                                                                                                                                                                                                                                                                                                                   digits = 4)), size = 3.5), vp = vplayout(rep(1:3)[i], 1))
      
    }
    params.wei <- list(shape = param$K, scale = param$A)
    params.ga <- list(shape = param$alpha, scale = param$beta)
    params.ln <- list(meanlog = param$meanlog, sdlog = param$sdlog)
    print(ggplot(dr, aes(sample = mean)) + stat_qq(distribution = qweibull, 
                                                   dparams = params.wei), vp = vplayout(1, 2))
    print(ggplot(dr, aes(sample = mean)) + stat_qq(distribution = qgamma, 
                                                   dparams = params.ga), vp = vplayout(2, 2))
    print(ggplot(dr, aes(sample = mean)) + stat_qq(distribution = qlnorm, 
                                                   dparams = params.ln), vp = vplayout(3, 2))
  } 

  else stop(paste("invalid plot type '", type, "'", sep = ""))
} 

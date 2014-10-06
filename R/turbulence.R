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

turbulence <- function(wd, ane=NA) {
    # browser()
    require(sqldf)
    require(ggplot2)

    
    if (is.na(ane))
      if (wd[["ane"]][["nane"]]!=1) {
        stop("Debe indicar el nombre del anemometro")
      } else {
        ane <- wd[["ane"]][["ane.names"]]
      }
    if (is.null(wd[["ane"]][[ane]][["sd"]]))
        stop("No se cuenta con información de desvíos estándar")
    if (wd[["interval.minutes"]]!=10)
        stop("No se cuenta con información diezminutal")
    
    df <- data.frame(ave = wd[["ane"]][[ane]][["ave"]], sd = wd[["ane"]][[ane]][["sd"]])
    df$I <- df$sd/df$ave * 100
    df$bin <- floor(df$ave + 0.5)
    dataplot <- sqldf("select bin widspeed, count(*) count, avg(I) I from df where bin>=1 group by bin")
    
    ref.point <- data.frame(x = c(15, 15, 15), y = c(16, 14, 12), ref = c("A - High Turbulence characteristics", 
        "B - Medium Turbulence characteristics", "C - Low Turbulence characteristics"))
    
    c <- ggplot(dataplot, aes(x = widspeed, y = I))
    print(c + geom_line(size = 1) + coord_cartesian(xlim = c(0, 17), ylim = c(0, 
        35)) + xlab("Vhub [m/s]") + ylab("Turbulence Intensity [%]") + geom_point(data = ref.point, 
        mapping = aes(x = x, y = y, group = ref, color = ref), size = 5) + scale_x_continuous(breaks = 1:16) + 
        scale_y_continuous(breaks = (1:7) * 5) + theme(axis.title.x = element_text(face = "bold", 
        size = 12), axis.title.y = element_text(face = "bold", size = 12)))
    
    result <- list(Iref = mean(df[df$ave >= 14.5 & df$ave <= 15.5, "I"], na.rm = T), 
        data_turbulence = dataplot)
    result
} 

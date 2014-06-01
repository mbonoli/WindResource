library("WindResource")
library(shiny)
data(wd)
data(wd10)

# Esta es la forma de guardar los archivos: save(wd10,file="wd10.rda",compress=TRUE)
runGUI(wd10)
runApp("~/GitHub/WindResource/inst/shiny",launch.browser = rstudio::viewer)
runApp("~/GitHub/WindResource/inst/shiny")

str(wd10)
# Histogram
plotWD (data=wd10, var="speed", type="histogram", by="none")
plotWD (data=wd10, var="speed", type="histogram", by="none", binwidth=.2)
plotWD (data=wd10, var="speed", since='2012-11-01', to='2012-12-31', type="histogram", by="none")
plotWD (data=wd10, var="speed", type="histogram", by="none")
tableWD(data=wd10,var="speed", type="histogram", by="none")
plotWD (data=wd10, var="speed", ane="Ane2", type="histogram", by="month")
tableWD (data=wd10,var="speed", ane="Ane2", type="histogram", by="month")
tableWD (data=wd10, var="speed", since='2012-11-01', to='2012-12-31', type="histogram", by="none")
tableWD (data=wd10, var="speed", type="histogram", by="none", binwidth=.2)

# Rose
plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="rose", by="none")
plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="rose", by="hour")
tableWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="rose", by="hour")

plotWD (data=wd10,var="speed", type="correlation", by="hour")

plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="profile", by="hour")

plotWD (data=wd10, type="turbulence")
tableWD (data=wd10, type="turbulence")



library("WindResource")

# wd10$dir$ang_16 <- (wd10$dir$sect_16-1)*22.5
class(wd10)<-"windata"
runGUI(wd10)

# Histogram
plotWD (data=wd10, var="speed", type="histogram", by="none")
plotWD (data=wd10, var="speed", type="histogram", by="month")
plotWD (data=wd10, var="speed", type="histogram", by="hour")

tableWD(data=wd10,var="speed", type="histogram", by="none")
tableWD (data=wd10,var="speed", type="histogram", by="month")
tableWD (data=wd10,var="speed", type="histogram", by="hour")

plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="rose", by="none")
plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="rose", by="month")
plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="rose", by="hour")
plotWD (data=wd10,var="speed", type="rose", by="none")
plotWD (data=wd10,var="speed", type="rose", by="month")
plotWD (data=wd10,var="speed", type="rose", by="hour")

tableWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="rose", by="none")
tableWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="rose", by="month")
tableWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="rose", by="hour")

plotWD (data=wd10,var="speed", type="correlation", by="hour")

plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="profile", by="month")
plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="profile", by="hour")
plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="profile", by="hour",since="2013-02-01",to="2013-02-28")
tableWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="profile", by="month")
tableWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="profile", by="hour")
tableWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="profile", by="hour",since="2013-02-01",to="2013-02-28")


plotWD (data=wd10, type="turbulence")
tableWD (data=wd10, type="turbulence")


plotWD (data=wd10, type="fit")
tableWD (data=wd10, type="fit")


turbulence (wd)
turbulence (wd10)


plotwindserie(wd10,2012,09,
              vars=c("Ave"),
              axis=c("Ave"),
              shiny=F)

plotwindserie(wd10,2012,01,
              vars=c("Ave"),
              axis=c("Ave"))

plotwindserie(datawd,
              as.numeric(input$SELyear),
              as.numeric(input$SELmonth),
              vars=c("Ave"),
              axis=c("Ave","Min","Max","Dir"))

plotcalendar (wd10, var="ave", ane="Ane1", shiny=F)
plotcalendar (wd10, var="ave", ane="Ane2", shiny=F)
plotcalendar (wd10, var="min", ane="Ane1", shiny=F)
  

# 
# prefijos0<-function(numero, caracteres=NA) {
#   long.num<-nchar(numero)
#   if (is.na(caracteres)) {caracteres<-max(nchar(numero))}
#   if (sum(long.num>caracteres)>0) {
#     stop("El numero tiene mas caracteres de lo posible. Ver funcion prefijos0")
#   }
#   ceros<-NULL
#   for (i in 1:caracteres) {ceros<-paste(ceros,"0",sep="")}
#   result<-rep(ceros,length(numero))
#   prefijos<-substr(result,1,caracteres-long.num)
#   result<-paste(prefijos,numero,sep="")
#   
#   result
# }
# wd$time$dt <-
#   as.POSIXct(paste(wd$time$year,
#                    prefijos0(wd$time$month,2),
#                    prefijos0(wd$time$day,2),
#                    " ",
#                    prefijos0(wd$time$hour,2),
#                    prefijos0(wd$time$minute,2),
#                    prefijos0(wd$time$second,2),
#                    sep=""),
#              format="%Y%m%d %H%M")
# 
# 
# wd10$time$dt <-
#   as.POSIXct(paste(wd10$time$year,
#                    prefijos0(wd10$time$month,2),
#                    prefijos0(wd10$time$day,2),
#                    " ",
#                    prefijos0(wd10$time$hour,2),
#                    prefijos0(wd10$time$day),
#                    prefijos0(wd10$time$second,2),
#                    sep=""),
#              format="%Y%m%d %H%M")

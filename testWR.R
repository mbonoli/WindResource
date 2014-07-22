###########################################
#       Instalar paquete desde GitHub     #
###########################################
library(devtools)
install_github("mbonoli/WindResource")
###########################################


###########################################
#             setWd                       #
###########################################
dataOlavarria <- read.csv("~/Investigacion/Vientos/datos/Olavarria.csv", sep=";")
wdOlavarria <- setWd (dataOlavarria, 
                      interval = 1,
                      date.var = "Fecha", 
                      date.format = "YYYYMMDD", 
                      time.var ="Hora", 
                      time.format = "HHMM",
                      ane.names = c("Ane1"),
                      ane.height= NA,
                      speed.ave.var = "Vel",
                      speed.min.var = NA,
                      speed.max.var = NA,
                      speed.sd.var = NA,
                      speed.var.var = NA,
                      speed.unit = NA,
                      dir.var = "Dir",
                      dir.unit = "deg",
                      temp.var = "Temp",
                      temp.unit = "C",
                      pres.var = "Pres",
                      pres.unit = "bar")
str(wdOlavarria)
save(wdOlavarria, file="~/Investigacion/Vientos/WindResource/data/wdOlavarria.rda")

MtTom.0032_1999.12.01_2002.12.31 <- read.csv("~/Investigacion/Vientos/datos/MtTom-0032_1999-12-01_2002-12-31.dat")
MtTom.0032_1999.12.01_2002.12.31$date <- substr(MtTom.0032_1999.12.01_2002.12.31[,1],1,10)
MtTom.0032_1999.12.01_2002.12.31$time <- substr(MtTom.0032_1999.12.01_2002.12.31[,1],12,17)
wdMtTom <- setWd (MtTom.0032_1999.12.01_2002.12.31, 
                  interval = 1,
                  date.var = "date", 
                  date.format = "YYYY-MM-DD", 
                  time.var ="time", 
                  time.format = "HH:MM",
                  ane.names = c("Anem24aMS","Anem24bMS","Anem37aMS","Anem37bMS"),
                  ane.height= c(24,24,37,37),
                  speed.ave.var = c("Anem24aMS","Anem24bMS","Anem37aMS","Anem37bMS"),
                  speed.min.var = NA,
                  speed.max.var = NA,
                  speed.sd.var = c("AnemSD24aMS","AnemSD24bMS","AnemSD37aMS","AnemSD37bMS"),
                  speed.var.var = NA,
                  speed.unit = NA,
                  dir.var = c("Vane24aDEG","Vane24aDEG","Vane37aDEG","Vane37aDEG"),
                  dir.unit = "deg",
                  temp.var = "Etmp3aDEGC",
                  temp.unit = "C",
                  pres.var = NA,
                  pres.unit = NA,
                  NA.values = c(-988, -989, -991, -999))
str(wdMtTom)
save(wdMtTom, file="~/Investigacion/Vientos/WindResource/data/wdMtTom.rda")


data <- read.csv("~/Investigacion/Vientos/BD/INTI/CH1_29072012_f1_V10_V18_Dir.csv")
data$date <- substr(data[,1],1,10)
data$time <- substr(data[,1],12,17)
wd <- setWd (data, 
             date.var = "date", 
             date.format = "DD/MM/YYYY", 
             time.var ="time", 
             time.format = "HH:MM",
             ane.names = c("ane10","ane18"),
             ane.height= c(10,18),
             speed.ave.var = c("ave1","ave2"),
             speed.min.var = c("min1","min2"),
             speed.max.var = c("max1","max2"),
             speed.sd.var = c("sd1","sd2"),
             speed.unit = NA,
             dir.var = c("dir","dir"),
             dir.unit = "deg",
             temp.var = NA,
             temp.unit = NA,
             pres.var = NA,
             pres.unit = NA,
             NA.values = NA)
str(wd)
save(wd, file="~/Investigacion/Vientos/WindResource/data/wd.rda")

data(wd)
w <- gen10m(wd)



library("WindResource")
data(wd)
data(wd10)

data(wdOlavarria)
runGUI(wdOlavarria)

str(wdOlavarria)
# Esta es la forma de guardar los archivos: save(wd10,file="wd10.rda",compress=TRUE)

runGUI(wd10)
runApp("~/GitHub/WindResource/inst/shiny")

str(wd10)
# Histogram
plotWD (data=wdOlavarria, var="mean", type="histogram", by="none")
plotWD (data=wd10, var="mean", type="histogram", by="none", binwidth=.2)
plotWD (data=wd10, var="mean", since='2012-11-01', to='2012-12-31', type="histogram", by="none")
plotWD (data=wd10, var="mean", type="histogram", by="none")
tableWD(data=wd10,var="mean", type="histogram", by="none")
plotWD (data=wdOlavarria, var="mean", ane="Anem24bMS", type="histogram", by="month")
tableWD (data=wd10,var="mean", ane="Ane2", type="histogram", by="month")
tableWD (data=wd10, var="mean", since='2012-11-01', to='2012-12-31', type="histogram", by="none")
tableWD (data=wd10, var="mean", type="histogram", by="none", binwidth=.2)

# Rose
plotWD (data=wdOlavarria,ane=NA,var="mean", type="rose", by="none")
plotWD (data=wdOlavarria,ane=c("Anem24aMS","Anem37bMS"),var="mean", type="rose", by="none")
plotWD (data=wd10,ane=c("Ane1","Ane2"),var="mean", type="rose", by="none")
plotWD (data=wdOlavarria,ane=c("Anem24aMS","Anem37bMS"), var="mean",type="rose", by="month")
plotWD (data=wd10,ane=c("Ane1","Ane2"),var="mean", type="rose", by="hour")
plotWD (data=wd10,ane=c("Ane1","Ane2"),var="min", type="rose", by="none")
plotWD (data=wd10,ane=c("Ane1","Ane2"),var="min", type="rose", by="month")
plotWD (data=wd10,ane=c("Ane1","Ane2"),var="min", type="rose", by="hour")
plotWD (data=wd10,ane=c("Ane1","Ane2"),var="max", type="rose", by="none")
plotWD (data=wd10,ane=c("Ane1","Ane2"),var="max", type="rose", by="month")
plotWD (data=wd10,ane=c("Ane1","Ane2"),var="max", type="rose", by="hour")
plotWD (data=wd10,ane=c("Ane1","Ane2"),var="frec", type="rose", by="none")
plotWD (data=wd10,ane=c("Ane1","Ane2"),var="frec", type="rose", by="month")
plotWD (data=wd10,ane=c("Ane1","Ane2"),var="frec", type="rose", by="hour")

tableWD (data=wd10,ane=c("Ane1","Ane2"),var="mean", type="rose", by="none")
tableWD (data=wd10,ane=c("Ane1","Ane2"),var="mean", type="rose", by="month")
tableWD (data=wd10,ane=c("Ane1","Ane2"),var="mean", type="rose", by="hour")
tableWD (data=wd10,ane=c("Ane1","Ane2"),var="min", type="rose", by="none")
tableWD (data=wd10,ane=c("Ane1","Ane2"),var="min", type="rose", by="month")
tableWD (data=wd10,ane=c("Ane1","Ane2"),var="min", type="rose", by="hour")
tableWD (data=wd10,ane=c("Ane1","Ane2"),var="max", type="rose", by="none")
tableWD (data=wd10,ane=c("Ane1","Ane2"),var="max", type="rose", by="month")
tableWD (data=wd10,ane=c("Ane1","Ane2"),var="max", type="rose", by="hour")
tableWD (data=wd10,ane=c("Ane1","Ane2"),var="frec", type="rose", by="none")
tableWD (data=wd10,ane=c("Ane1","Ane2"),var="frec", type="rose", by="month")
tableWD (data=wd10,ane=c("Ane1","Ane2"),var="frec", type="rose", by="hour")

# boxplot
tableWD (data=wd10,ane="Ane2",var="mean", type="boxplot", by="hour")
tableWD (data=wd10,ane="Ane2",var="mean", type="boxplot", by="month")

plotWD (data=wd10,ane="Ane2",var="mean", type="boxplot", by="hour")
plotWD (data=wd10,ane="Ane2",var="mean", type="boxplot", by="month")

plotWD (data=wd10,var="mean", type="correlation", by="hour")

plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="mean", type="profile", by="hour")

plotWD (data=wd10, type="turbulence")
tableWD (data=wd10, type="turbulence")



library("WindResource")

# wd10$dir$ang_16 <- (wd10$dir$sect_16-1)*22.5
class(wd10)<-"windata"
runGUI(wd10)

# Histogram
plotWD (data=wd10, var="mean", type="histogram", by="none")
plotWD (data=wd10, var="mean", type="histogram", by="month")
plotWD (data=wd10, var="mean", type="histogram", by="hour")

tableWD(data=wd10,var="mean", type="histogram", by="none")
tableWD (data=wd10,var="mean", type="histogram", by="month")
tableWD (data=wd10,var="mean", type="histogram", by="hour")

plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="mean", type="rose", by="none")
plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="mean", type="rose", by="month")
plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="mean", type="rose", by="hour")
plotWD (data=wd10,var="mean", type="rose", by="none")
plotWD (data=wd10,var="mean", type="rose", by="month")
plotWD (data=wd10,var="mean", type="rose", by="hour")

tableWD (data=wd10,ane=c("Ane1", "Ane2"),var="mean", type="rose", by="none")
tableWD (data=wd10,ane=c("Ane1", "Ane2"),var="mean", type="rose", by="month")
tableWD (data=wd10,ane=c("Ane1", "Ane2"),var="mean", type="rose", by="hour")

plotWD (data=wd10,var="mean", type="correlation", by="hour")

plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="mean", type="profile", by="month")
plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="mean", type="profile", by="hour")
plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="mean", type="profile", by="hour",since="2013-02-01",to="2013-02-28")
tableWD (data=wd10,ane=c("Ane1", "Ane2"),var="mean", type="profile", by="month")
tableWD (data=wd10,ane=c("Ane1", "Ane2"),var="mean", type="profile", by="hour")
tableWD (data=wd10,ane=c("Ane1", "Ane2"),var="mean", type="profile", by="hour",since="2013-02-01",to="2013-02-28")


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

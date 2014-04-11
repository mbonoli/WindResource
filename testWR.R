library("WindResource")
library(shiny)
data(wd10)
# Esta es la forma de guardar los archivos: save(wd10,file="wd10.rda",compress=TRUE)
runGUI(wd10)
runApp("~/GitHub/WindResource/inst/shiny",launch.browser = rstudio::viewer)

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

plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="rose", by="none")
plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="rose", by="hour")
tableWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="rose", by="hour")

plotWD (data=wd10,var="speed", type="correlation", by="hour")

plotWD (data=wd10,ane=c("Ane1", "Ane2"),var="speed", type="profile", by="hour")

plotWD (data=wd10, type="turbulence")
tableWD (data=wd10, type="turbulence")



library("WindResource")
data(wd)
data(wd10)
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


plot.wind.serie(wd10,2013,2,vars=c("Ave","Min","Max","Dir"),
                axis=c("Ave","Min","Max","Dir"))



#Aerogeneradores

wtg_model <-list(
  E33=list(
    branch="Enercon",
    model="E33",
    Data=Enercon_E33 <- read.xlsx(file="Enercon_E33.xlsx", sheetName="Hoja1")),
  E48=list(
    branch="Enercon",
    model="E33",
    Data=Enercon_E48 <- read.xlsx(file="Enercon_E48.xlsx", sheetName="Hoja1"))
)


aerogenerador <- function (Branch, Model){
  
  if ( Branch=="Enercon" & Model=="E33"){
    plot(Enercon_E33$Vel.Viento,Enercon_E33$Potencia.P,type="l")
    return(wtg_model$E33)
  }
  else {
    plot(Enercon_E48$Vel.Viento,Enercon_E48$Potencia.P,type="l")
    points(Enercon_E48$Vel.Viento,Enercon_E48$Potencia.P,type="p")
    return(wtg_model$E48)
  }
}

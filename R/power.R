wt <- function(datawd, datawtg, ane, model, inter) {
    
    wtdata <- list()
    wtg <- list()
    wtgt <- c()
    
    for (i in ane) {
        
        wtdata[[i]] <- datawd$ane[[i]][[1]][!is.na(datawd$ane[[i]][[1]])]
        
        
        for (j in model) {
            
            porc <- length(wtdata[[i]])/(525600/inter)
            wtg <- data.frame(datawtg[[j]][3])
            wtgt <- c(wtgt, sum(wtg[round(wtdata[[i]]), "data.Power"])/porc)
            
        }
        
    }
    
    return(wtgt)
    
}






 

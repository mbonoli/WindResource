roughness <- function(datawd) {
  library(dplyr)
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
}
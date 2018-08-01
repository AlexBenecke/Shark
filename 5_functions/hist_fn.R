### A Function to Create Histograms of Explanatory Variables



hist_fn <- function(data = data){
  
  require(FSA)
  require(dplyr)
  require(magrittr)
  
  par(mfrow = c(4,2))
  
  for(i in 1:length(sp_list_hist)){
    
    hist(sp_list_hist[[i]]$cpe,
         freq=FALSE,
         main = names(sp_list_hist[i]))
    
  }
  
  
}








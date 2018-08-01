est_k <- function(dat){
  require(magrittr)
  
  var_list <- c( "STARTLAT", "STARTLON", "AVG.DEPTH", "TEMPBOTM", "TURBBOTM", "OXYBOTM", "SALBOTM", "Dis.to.SHORE")
  
  dat %<>% select(var_list)
  
  k_est <- data.frame(lat = numeric(1),
                      lon = numeric(1),
                      dep = numeric(1),
                      temp = numeric(1),
                      turb = numeric(1),
                      oxy = numeric(1),
                      sal = numeric(1),
                      dis = numeric(1)) 
  
  n_dat <- numeric(ncol(dat))
  
  
  for(i in 1:ncol(dat)){
    n_dat[i] = length(which(is.na(dat[,i]) == FALSE))
    k_est[i] = round(10*(n_dat[i]^(2/9)))
  }
  
  return(k_est)  
}
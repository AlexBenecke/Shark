# Function to extract taxa-specific data.frame
# Input "eff" data and output a single, reduced, df

# Need complete.cases

extract_taxa_fn <- function(data.in = x,
                                tax.id = y) {
  
  red.df <- data.in %>%
    select(YEAR, MONTH, STARTLAT, STARTLON, START_GMT, ENDLAT, ENDLON, 
           STARTDEPTH, ENDDEPTH, AVG.DEPTH, TEMPBOTM, FLUOROBOTM, TURBBOTM, 
           OXYBOTM, SALBOTM, dombot8, dombot4, Dis.to.SHORE)
  
  red.df <- cbind(red.df, cpe = tax.id) 
  
  pos.ind <- which(red.df$cpe >= 0)   ### be sure no negatives
  red.df <- red.df[pos.ind,]
  
  red.df$pres <- numeric(nrow(red.df))
  
  for(i in 1:nrow(red.df)){
    
    if(red.df$cpe[i] > 0){
      
      red.df$pres[i] <- 1
      
    } else if(red.df$cpe[i] == 0){
      
      red.df$pres[i] <- 0
      
    } else{
      
      red.df$pres[i] <- "NA"
      
    }
  }
  
  
  red.df %<>% select(cpe, pres, DATE = START_GMT, YEAR, MONTH, STARTLAT:STARTLON, ENDLAT:ENDLON, 
                     STARTDEPTH, ENDDEPTH, AVG.DEPTH, TEMPBOTM:dombot4, Dis.to.SHORE)
  
  
  return(reduced.df = red.df)
}



###########################################
#red.df <- (data.in[,c(1,6,7,8,9,10,11,12,38,39,40,41,42,57,58,61)])


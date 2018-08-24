### Load Data Function

load_data <- function(data = data, strata = 1){
  require(FSA)
  require(magrittr)
  require(dplyr)
  
  data %<>% mutate(year = factor(year), 
                   pres = factor(pres), 
                   month = ordered(month, 
                                   levels = c("Jul", "Aug", "Sep", "Oct", "Nov") ) 
                   ) 
  
  
  Species_Vec <- levels(data$common)
  Female <- vector("list", length = length(data))
  Male <- vector("list", length = length(data))
  
  
  if(strata == 1){
    
    Female <- list(blacknose =  filterD(data, common == Species_Vec[1], sex == "F"),
                   
                   blacktip = filterD(data, common == Species_Vec[2], sex == "F"),
                   
                   bull = filterD(data, common == Species_Vec[3], sex == "F"),
                   
                   sandbar = filterD(data, common == Species_Vec[4], sex == "F"),
                   
                   sharpnose = filterD(data, common == Species_Vec[5], sex == "F"),
                   
                   spinner = filterD(data, common == Species_Vec[6], sex == "F"),
                   
                   tiger = filterD(data, common == Species_Vec[7], sex == "F"))
    
    
    Male <- list(blacknose =  filterD(data, common == Species_Vec[1], sex == "M"),
                 
                 blacktip = filterD(data, common == Species_Vec[2], sex == "M"),
                 
                 bull = filterD(data, common == Species_Vec[3], sex == "M"),
                 
                 sandbar = filterD(data, common == Species_Vec[4], sex == "M"),
                 
                 sharpnose = filterD(data, common == Species_Vec[5], sex == "M"),
                 
                 spinner = filterD(data, common == Species_Vec[6], sex == "M"),
                 
                 tiger = filterD(data, common == Species_Vec[7], sex == "M"))
    
    output <- list(Female = Female,
                   Male = Male)
    
  } else if(strata == 2){
    
    Female <- list(blacknose =  filterD(data, common == Species_Vec[1], pres == 1, sex == "F"),
                   
                   blacktip = filterD(data, common == Species_Vec[2], pres == 1, sex == "F"),
                   
                   bull = filterD(data, common == Species_Vec[3], pres == 1, sex == "F"),
                   
                   sandbar = filterD(data, common == Species_Vec[4], pres == 1, sex == "F"),
                   
                   sharpnose = filterD(data, common == Species_Vec[5], pres == 1, sex == "F"),
                   
                   spinner = filterD(data, common == Species_Vec[6], pres == 1, sex == "F"),
                   
                   tiger = filterD(data, common == Species_Vec[7], pres == 1, sex == "F"))
    
    
    Male <- list(blacknose =  filterD(data, common == Species_Vec[1], pres == 1, sex == "M"),
                 
                 blacktip = filterD(data, common == Species_Vec[2], pres == 1, sex == "M"),
                 
                 bull = filterD(data, common == Species_Vec[3], pres == 1, sex == "M"),
                 
                 sandbar = filterD(data, common == Species_Vec[4], pres == 1, sex == "M"),
                 
                 sharpnose = filterD(data, common == Species_Vec[5], pres == 1, sex == "M"),
                 
                 spinner = filterD(data, common == Species_Vec[6], pres == 1, sex == "M"),
                 
                 tiger = filterD(data, common == Species_Vec[7], pres == 1, sex == "M"))
    
    output <- list(Female = Female,
                   Male = Male)
    
  } else stop("Strata must equal either 1 or 2 !!!")
  
  return(output)
}




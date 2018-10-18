### Load Data Function

load_data <- function(data = data, strata = 1, fact_vars = TRUE){
  require(FSA)
  require(magrittr)
  require(dplyr)
  
  
    data %<>% mutate(year = factor(year), 
                     month = ordered(month, 
                                     levels = c("Jul", "Aug", "Sep", "Oct", "Nov") ) 
    ) 

  
  
  Species_Vec <- levels(data$common)
  Female <- vector("list", length = length(data))
  Male <- vector("list", length = length(data))
  
  ### Check species_vec W/O others I change sharpnose to 3 (from 5)
  
  if(strata == 1){
    
    Female <- list(blacknose =  filterD(data, common == "Blacknose", sex == "F"),
                   
                   blacktip = filterD(data, common == "Blacktip", sex == "F"),
                   
                   sharpnose = filterD(data, common == "Sharpnose", sex == "F")#,
                   
                  # sandbar = filterD(data, common == "Sandbar", sex == "F"),
                   
                  # spinner = filterD(data, common == "Spinner, sex == "F"),
                   
                  # tiger = filterD(data, common == "Tiger, sex == "F"),
                   
                  # bull = filterD(data, common == "Bull, sex == "F")
                  )
    
    
    Male <- list(blacknose =  filterD(data, common == "Blacknose", sex == "M"),
                 
                 blacktip = filterD(data, common == "Blacktip", sex == "M"),
                 
                 sharpnose = filterD(data, common == "Sharpnose", sex == "M")#,
                 
                # sandbar = filterD(data, common == "Sandbar", sex == "M"),
                 
                # spinner = filterD(data, common == "Spinner, sex == "M"),
                 
                # tiger = filterD(data, common == "Tiger, sex == "M"),
                 
                 #bull = filterD(data, common == "Bull, sex == "M")
                )
    
    output <- list(Female = Female,
                   Male = Male)
    
  } else if(strata == 2){
    
    Female <- list(blacknose =  filterD(data, common == "Blacknose", pres == 1, sex == "F"),
                   
                   blacktip = filterD(data, common == "Blacktip", pres == 1, sex == "F"),
                   
                   sharpnose = filterD(data, common == "Sharpnose", pres == 1, sex == "F")#,
                   
                   # sandbar = filterD(data, common == "Sandbar", pres == 1, sex == "F"),
                   
                   # spinner = filterD(data, common == "Spinner, pres == 1, sex == "F"),
                   
                   # tiger = filterD(data, common == "Tiger, pres == 1, sex == "F"),
                   
                   #bull = filterD(data, common == "Bull, pres == 1, sex == "F")
                   )
    
    
    Male <- list(blacknose =  filterD(data, common == "Blacknose", pres == 1, sex == "M"),
                 
                 blacktip = filterD(data, common == "Blacktip", pres == 1, sex == "M"),
                 
                 sharpnose = filterD(data, common == "Sharpnose", pres == 1, sex == "M")#,
                 
                 # sandbar = filterD(data, common == "Sandbar", pres == 1, sex == "M"),
                 
                 # spinner = filterD(data, common == "Spinner, pres == 1, sex == "M"),
                 
                 # tiger = filterD(data, common == "Tiger, pres == 1, sex == "M"),
                 
                 #bull = filterD(data, common == "Bull, pres == 1, sex == "M")
                 )
    
    output <- list(Female = Female,
                   Male = Male)
    
  } else stop("Strata must equal either 1 or 2 !!!")
  
  return(output)
}




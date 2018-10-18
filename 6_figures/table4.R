### Table 4 Variable Importance

### Load Packages ###
library(FSA)
library(tidyverse)
library(dplyr)
library(magrittr)
library(scales)
library(mgcv)
library(sciplot)
library(knitr)
library(kableExtra)
library(pipeR)
library(MuMIn)

source("5_functions/dropbox_fn.R");rm(load_fn, path_get, save_fn) # For interacting with dropbox
proj = "shark" # Specify project where data is stored 
source("5_functions/load_data.R") 


### Load Models

filenames <- list.files(c("output/output_GAM/strat1/Female/Adult",
                          "output/output_GAM/strat1/Female/Juvenile",
                          "output/output_GAM/strat1/Male/Adult",
                          "output/output_GAM/strat1/Male/Juvenile",
                          "output/output_GAM/strat2/Female/Adult",
                          "output/output_GAM/strat2/Female/Juvenile",
                          "output/output_GAM/strat2/Male/Adult",
                          "output/output_GAM/strat2/Male/Juvenile"),
                        pattern="*.rda", full.names=TRUE)


### AIC Weight Function

aic_weight <- function(results_df){
  
  numerator_wi <- exp((-0.5)*results_df$delta_AIC)
  
  denominator_wi <- sum(numerator_wi)
  
  results_df$aic_wi <- numerator_wi/denominator_wi
  
  results_df$cum_wi <- cumsum(results_df$aic_wi)
  
  return(results_df[results_df$cum_wi<= 0.95,])
  
}

### Function


spec <- list(bn = "Blacknose",
             bt = "Blacktip",
             sn = "Sharpnose")

strat <- list(strat1 = "Presence",
              strat2 = "Abundance")

for( i in 1:length( filenames ) ){ 
  
  if(i ==1){
    
   # i = 1
    
    load( file = filenames[i] ) 
    
    Sex <- str_split( filenames[i], pattern = "/", simplify = TRUE )[1,4]
    
    Maturity <- str_split( filenames[i], pattern = "/", simplify = TRUE )[1,5]
    
    sp <- str_split( filenames[i], pattern = "/", simplify = TRUE )[1,6] %>% 
      str_split(  pattern = "_", simplify = TRUE ) %>% 
      .[1,1]
    
    mod_strat <- str_split( filenames[i], pattern = "/", simplify = TRUE )[1,3]
    
    
    mod_name <- str_split( filenames[i], pattern = "/", simplify = TRUE )[1,6] %>% 
      str_split( pattern = ".rda", simplify = TRUE ) %>%
      .[1,1]
    #mod_name
    
    results_df <- aic_weight( get( mod_name )$results[,c( 1:4 )]) %>%
      subset(delta_AIC < 4)
    #results_df
    
    mod_lst <- get( mod_name )$model_fits[c( as.character( results_df$model ) )]
    
    nrow(results_df)
    length(mod_lst)
    
    #importance(mod_lst)
    
    terms <- str_split(results_df$model, "_", simplify = TRUE) %>%
      as.data.frame()
    
    Dep <- numeric(1)
    nDep <- numeric(1)
    
    Temp <- numeric(1)
    nTemp <- numeric(1)
    
    Turb <- numeric(1)
    nTurb <- numeric(1)
    
    Oxy <- numeric(1)
    nOxy <- numeric(1)
    
    Sal <- numeric(1)
    nSal <- numeric(1)
    
    Dis <- numeric(1)
    nDis <- numeric(1)
    
    Bottype <- numeric(1)
    nBottype <- numeric(1)
    
    for(j in 1:length(mod_lst)){
      
      if(any(terms[j,]== "Dep")){
        Dep = results_df$aic_wi[j] + Dep
        nDep = 1 + nDep
      }
      if(any(terms[j,]== "Temp")){
        Temp = results_df$aic_wi[j] + Temp
        nTemp = 1 + nTemp
      }
      if(any(terms[j,]== "Turb")){
        Turb = results_df$aic_wi[j] + Turb
        nTurb = 1 + nTurb
      }
      if(any(terms[j,]== "Oxy")){
        Oxy = results_df$aic_wi[j] + Oxy
        nOxy = 1 + nOxy
      }
      if(any(terms[j,]== "Sal")){
        Sal = results_df$aic_wi[j] + Sal
        nSal = 1 + nSal
      }
      if(any(terms[j,]== "Dis")){
        Dis = results_df$aic_wi[j] + Dis
        nDis = 1 + nDis
      }
      if(any(terms[j,]== "Bottype")){
        Bottype = results_df$aic_wi[j] + Bottype
        nBottype = 1 + nBottype
      }
      
    }
    
    sum_wi <- sum(results_df$aic_wi)
    
    Dep <- Dep/sum_wi
    
    Temp <- Temp/sum_wi
    
    Turb <- Turb/sum_wi
    
    Oxy <- Oxy/sum_wi
    
    Sal <- Sal/sum_wi
    
    Dis <- Dis/sum_wi
    
    Bottype <- Bottype/sum_wi
    
    
    (Species <- spec[[sp]])
    
    (Strata <- strat[[mod_strat]])
    
    
    (table4_part1 <- cbind(Species, Strata, Sex, Maturity, Dep, Temp, Turb, Oxy, Sal, Dis, Bottype ) %>%
        data.frame()
    )
    
    Maturity <- "n models"
    Dep <- nDep
    Temp <- nTemp
    Turb <- nTurb
    Oxy <- nOxy
    Sal <- nSal
    Dis <- nDis
    Bottype <- nBottype
    
    (table4_n <- cbind(Species, Strata, Sex, Maturity, Dep, Temp, Turb, Oxy, Sal, Dis, Bottype ) %>%
        data.frame()
    )
    (table4 <- rbind(table4_part1, table4_n) %>%
        data.frame())
    
    
    rm(Species, Strata, Sex, Maturity, mod_lst, mod_name, mod_strat, sp, j, list = mod_name, results_df, sum_wi,
       Dep, Temp, Turb, Oxy, Sal, Dis, Bottype, nDep, nTemp, nTurb, nOxy, nSal, nDis, nBottype, table4_part1, table4_n, terms)
    
  } else{
    
    #i = 3
    
    load( file = filenames[i] ) 
    
    Sex <- str_split( filenames[i], pattern = "/", simplify = TRUE )[1,4]
    
    Maturity <- str_split( filenames[i], pattern = "/", simplify = TRUE )[1,5]
    
    sp <- str_split( filenames[i], pattern = "/", simplify = TRUE )[1,6] %>% 
      str_split(  pattern = "_", simplify = TRUE ) %>% 
      .[1,1]
    
    mod_strat <- str_split( filenames[i], pattern = "/", simplify = TRUE )[1,3]
    
    
    mod_name <- str_split( filenames[i], pattern = "/", simplify = TRUE )[1,6] %>% 
      str_split( pattern = ".rda", simplify = TRUE ) %>%
      .[1,1]
    #mod_name
    
    results_df <- aic_weight( get( mod_name )$results[,c( 1:4 )]) %>%
      subset(delta_AIC < 4)
    #results_df
    
    mod_lst <- get( mod_name )$model_fits[c( as.character( results_df$model ) )]
    #mod_lst
    
    #importance(mod_lst)
    
    terms <- str_split(results_df$model, "_", simplify = TRUE) %>%
      as.data.frame()
    
    Dep <- numeric(1)
    nDep <- numeric(1)
    
    Temp <- numeric(1)
    nTemp <- numeric(1)
    
    Turb <- numeric(1)
    nTurb <- numeric(1)
    
    Oxy <- numeric(1)
    nOxy <- numeric(1)
    
    Sal <- numeric(1)
    nSal <- numeric(1)
    
    Dis <- numeric(1)
    nDis <- numeric(1)
    
    Bottype <- numeric(1)
    nBottype <- numeric(1)
    
    for(j in 1:length(mod_lst)){
      
      if(any(terms[j,]== "Dep")){
        Dep = results_df$aic_wi[j] + Dep
        nDep = 1 + nDep
      }
      if(any(terms[j,]== "Temp")){
        Temp = results_df$aic_wi[j] + Temp
        nTemp = 1 + nTemp
      }
      if(any(terms[j,]== "Turb")){
        Turb = results_df$aic_wi[j] + Turb
        nTurb = 1 + nTurb
      }
      if(any(terms[j,]== "Oxy")){
        Oxy = results_df$aic_wi[j] + Oxy
        nOxy = 1 + nOxy
      }
      if(any(terms[j,]== "Sal")){
        Sal = results_df$aic_wi[j] + Sal
        nSal = 1 + nSal
      }
      if(any(terms[j,]== "Dis")){
        Dis = results_df$aic_wi[j] + Dis
        nDis = 1 + nDis
      }
      if(any(terms[j,]== "Bottype")){
        Bottype = results_df$aic_wi[j] + Bottype
        nBottype = 1 + nBottype
      }
      
    }
    
    sum_wi <- sum(results_df$aic_wi)
    
    Dep <- Dep/sum_wi
    
    Temp <- Temp/sum_wi
    
    Turb <- Turb/sum_wi
    
    Oxy <- Oxy/sum_wi
    
    Sal <- Sal/sum_wi
    
    Dis <- Dis/sum_wi
    
    Bottype <- Bottype/sum_wi
    

    (Species <- spec[[sp]])
    
    (Strata <- strat[[mod_strat]])
    
    
    (table4_part1 <- cbind(Species, Strata, Sex, Maturity, Dep, Temp, Turb, Oxy, Sal, Dis, Bottype ) %>%
        data.frame()
    )
    
    Maturity <- "n models"
    Dep <- nDep
    Temp <- nTemp
    Turb <- nTurb
    Oxy <- nOxy
    Sal <- nSal
    Dis <- nDis
    Bottype <- nBottype
    
    (table4_n <- cbind(Species, Strata, Sex, Maturity, Dep, Temp, Turb, Oxy, Sal, Dis, Bottype ) %>%
        data.frame()
    )
    (table4 <- rbind(table4, table4_part1, table4_n) %>%
        data.frame())
    
    
    rm(Species, Strata, Sex, Maturity, mod_lst, mod_name, mod_strat, sp, j, list = mod_name, results_df, sum_wi,
       Dep, Temp, Turb, Oxy, Sal, Dis, Bottype, nDep, nTemp, nTurb, nOxy, nSal, nDis, nBottype, table4_part1, table4_n, terms)
    
  }
    
 
}

names(table4) <- c("Species", "Strata", "Sex", "Maturity", "Depth", "Temperature", "Turbidity", "Oxygen", "Salinity", "Distance to Shore", "Bottom Type")

table4 %>%
  arrange( Species, Strata, Sex )

table4$Depth <- round(table4$Depth, 3)
table4



#10-15-2018#write_csv_fn(table4, "data/clean-data/table-data/Table4.csv", proj, row.names = FALSE)


table4 <- read_csv_fn("data/clean-data/table-data/Table4.csv", proj) %>%
  mutate(Depth = round(Depth, 3), Temperature = round(Temperature,3), Turbidity = round(Turbidity,3), Oxygen = round(Oxygen,3), 
         Salinity = round(Salinity,3), Distance.to.Shore = round(Distance.to.Shore,3), Bottom.Type = round(Bottom.Type,3))

names(table4) <- c("Species", "Strata", "Sex", "Maturity", "Depth", "Temperature", "Turbidity", "Oxygen", "Salinity", "Distance to Shore", "Bottom Type")

str(table4)

table4 %>%
  kable() %>%
  kable_styling(full_width = F, 
                position = "center",
                font_size = 10) %>%
  collapse_rows(columns = c(1:3), valign = "middle")




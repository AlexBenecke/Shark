### Table 3 Model Selection

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
source("5_functions/dropbox_fn.R");rm(load_fn, path_get, save_fn) # For interacting with dropbox
proj = "shark" # Specify project where data is stored 
source("5_functions/load_data.R") 


##### Table 3 #####
  ## Raw ##

spec <- list(bn = "Blacknose",
             bt = "Blacktip",
             sn = "Sharpnose")

strat <- list(strat1 = "Presence",
              strat2 = "Abundance")

aic_weight <- function(results_df){
  
  numerator_wi <- exp((-0.5)*results_df$delta_AIC)
  
  denominator_wi <- sum(numerator_wi)
  
  results_df$aic_wi <- numerator_wi/denominator_wi
  
  results_df$cum_wi <- cumsum(results_df$aic_wi)
  
  return(results_df[results_df$cum_wi<= 0.95,])
  
}

filenames <- list.files(c("output/output_GAM/strat1/Female/Adult",
                          "output/output_GAM/strat1/Female/Juvenile",
                          "output/output_GAM/strat1/Male/Adult",
                          "output/output_GAM/strat1/Male/Juvenile",
                          "output/output_GAM/strat2/Female/Adult",
                          "output/output_GAM/strat2/Female/Juvenile",
                          "output/output_GAM/strat2/Male/Adult",
                          "output/output_GAM/strat2/Male/Juvenile"),
                        pattern="*.rda", full.names=TRUE)

for( i in 1:length( filenames ) ){ 
  
  if(i == 1){
    #i = 23 #bt_MJ
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
    mod_name
    
    results_df <- aic_weight( get( mod_name )$results[,c( 1:4 )])
    results_df
    
    n_mods_aic_wi <- nrow(results_df)
    n_mods_delta_aic <- nrow(results_df[results_df$delta_AIC<4,])
    
    results_df <- results_df[c(1:3),] 
    
    mod_lst <- get( mod_name )$model_fits[c( as.character( results_df$model ) )]
    mod_lst
    
    model <- numeric( length( mod_lst ) )
    dev <- numeric( length( mod_lst ) )
    
    for( j in 1:length( mod_lst ) ){
      
      strsplit( as.character( mod_lst[[j]]$formula ), "~" )[[2]] %>>% (~response)
      
      model[j] <- paste( c( response, paste( c( rownames( summary( mod_lst[[j]] )$s.table ), mod_lst[[j]]$pterms[[3]] ), collapse = " + " ) ), collapse = " ~ " )
      
      dev[j] <- round( summary( mod_lst[[j]] )$dev.expl *100, 2 )
      
    }
    model
    dev
    
    results_df$model <- model
    
    (Species <- spec[[sp]])
    
    (Strata <- strat[[mod_strat]])
    
    
    (table3 <- cbind(Species, Strata, Sex, Maturity, results_df, dev, n_mods_aic_wi, n_mods_delta_aic) %>%
        data.frame() #%>%
        #rename(Model = model, "Dev. Expl. (%)" = dev)
      )
    
    rm(Species, Strata, Sex, Maturity, results_df, dev, model, mod_lst, mod_name, mod_strat, sp, response, j, list = mod_name, n_mods_aic_wi, n_mods_delta_aic)
    
  } else{
   #i = 2 
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
    
    mod_name
    
    results_df <- aic_weight( get( mod_name )$results[,c( 1:4 )])
    results_df
    
    n_mods_aic_wi <- nrow(results_df)
    n_mods_delta_aic <- nrow(results_df[results_df$delta_AIC<4,])
    
    results_df <- results_df[c(1:3),]
    
    mod_lst <- get( mod_name )$model_fits[c( as.character( results_df$model ) )]
    mod_lst
    
    model <- numeric( length( mod_lst ) )
    dev <- numeric( length( mod_lst ) )
    
    for( j in 1:length( mod_lst ) ){
      
      strsplit( as.character( mod_lst[[j]]$formula ), "~" )[[2]] %>>% (~response)
      
      model[j] <- paste( c( response, paste( c( rownames( summary( mod_lst[[j]] )$s.table ), mod_lst[[j]]$pterms[[3]] ), collapse = " + " ) ), collapse = " ~ " )
      
      dev[j] <- round( summary( mod_lst[[j]] )$dev.expl *100, 2 )
      
    }
    model
    dev
    
    results_df$model <- model
    
    (Species <- spec[[sp]])
    
    (Strata <- strat[[mod_strat]])
    
    
    (cbind(Species, Strata, Sex, Maturity, results_df, dev, n_mods_aic_wi, n_mods_delta_aic) %>%
        data.frame() %>%
        rbind(table3,.) -> table3)
    
    rm(Species, Strata, Sex, Maturity, results_df, dev, model, mod_lst, mod_name, mod_strat, sp, response, j, list = mod_name, n_mods_aic_wi, n_mods_delta_aic)
    
  }
  }


table3
names(table3)

table3 <- arrange(table3, Species, Strata, Sex, Maturity)

names(table3) <- c("Species", "Strata", "Sex", "Maturity", "Model Formula", "No. Of Parameters", "AIC", "delta AIC", 
                   "AIC Weight", "Cumulative Weight", "Deviance Explained", "95% Confidence set (n)", "delta AIC < 4 (n)")

table3  
##  ##

library(kableExtra)

table3 %>%
  kable() %>%
  kable_styling(full_width = F, 
                position = "center",
                font_size = 10) %>%
  collapse_rows(columns = c(1:4,12,13), valign = "middle")

#10-15-2018#write.csv(table3, "Table3.csv", row.names = FALSE)

##### #####

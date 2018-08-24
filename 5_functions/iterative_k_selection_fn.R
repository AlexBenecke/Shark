### Building iterative k selection loop

### Load packages
library(mgcv)
library(FSA)
library(dplyr)
library(magrittr)
library(tidyverse)

source("5_functions/dropbox_fn.R") # For interacting with dropbox
proj = "shark" # Specify project where data is stored 

### Load data

sn_pos <- read_csv_fn( "data/clean-data/sharpnose.csv", proj) %>%
  subset(pres == 1) %>%
  mutate(YEAR = factor(YEAR), 
         MONTH = factor(strftime(DATE, format = "%b")))



str(sn_pos)

### Initiate dfs for k selection fn
smooth = rep("T", length = 7)

(N = length(smooth)-1)


(k_0 <- rep(as.numeric(5), length = N))


(edf_vec <- numeric(N))
(k_prime <- numeric(N))
(diff_df <- numeric(N))
(add_k <- rep("TRUE", length = 6))

(old_gcv <- 1000000)
(new_gcv <- numeric(1))

### iterative k selection

system.time(
  while( any(add_k == TRUE) == TRUE  ){
    
    iterative_k <- gam((cpe +1) ~ s(STARTLON, STARTLAT, k = k_0[1]) + 
                         s(AVG.DEPTH, k = k_0[2]) + 
                         s(TEMPBOTM, k = k_0[3]) + 
                         s(TURBBOTM, k = k_0[4]) +
                         s(OXYBOTM, k = k_0[5]) + 
                         s(SALBOTM, k = k_0[6]) + 
                         s(YEAR, bs = "re"),
                       data = sn_pos,
                       family = Gamma(link = log))
    
    new_gcv <- iterative_k$gcv.ubre
    
    k_prime[1] = k_0[1] - 1
    
    edf_vec[1] <- summary(iterative_k)$edf[1]
    
    diff_df[1] <- k_prime[1] - edf_vec[1] -8
    
    ifelse( (diff_df[1] <= 10) == TRUE & (new_gcv <= old_gcv) == TRUE,
            add_k[1] <- TRUE,
            add_k[1] <- FALSE)
    ifelse( (diff_df[1] <= 10) == TRUE & (new_gcv <= old_gcv) == TRUE,
            k_0[1] <- k_0[1] + 5,
            add_k[1] <- FALSE)
    
    
    
    for(i in 2:length(k_0)){
      
      k_prime[i] = k_0[i] - 1
      
      edf_vec[i] <- summary(iterative_k)$edf[i]
      
      diff_df[i] <- k_prime[i] - edf_vec[i] 
      
      ifelse( (diff_df[i] <= 2) == TRUE & (new_gcv <= old_gcv) == TRUE,
              add_k[i] <- TRUE,
              add_k[i] <- FALSE)
      ifelse( (diff_df[i] <= 2) == TRUE & (new_gcv <= old_gcv) == TRUE,
              k_0[i] <- k_0[i] + 1,
              add_k[i] <- FALSE)
      
    }
    
    
    if(new_gcv < old_gcv){
      
      old_gcv = new_gcv
      
    }
    
  }
)

#   user  system elapsed 
#   9.27    0.09    9.70 

### review output

gam.check(iterative_k)

plot(iterative_k)


iterative_k$coefficients





### Now make function scalable to number of smoothing terms in each model

### Chunk from model selection fn

all_Models_Results <- lapply(all_Models_List,
                             function(x) gam(x, data = data,
                                             family = family,
                                             gamma = gamma))

######################################




##############################################
k1 = 5
k2 = 5
k3 = 5
k4 = 5
k5 = 5
k6 = 5


iterative_k <- gam((cpe +1) ~ s(STARTLON, STARTLAT, k = k1) + 
                              s(AVG.DEPTH, k = k2) + 
                              s(TEMPBOTM, k = k3) + 
                              s(TURBBOTM, k = k4) +
                              s(OXYBOTM, k = k5) + 
                              s(SALBOTM, k = k6) + 
                              s(YEAR, bs = "re"),
                            data = sn_pos,
                            family = Gamma(link = log))



summary(iterative_k)$edf










###################


edf_lst <- list(edf1 = summary(iterative_k)$edf[1],
                edf2 = summary(iterative_k)$edf[2],
                edf3 = summary(iterative_k)$edf[3],
                edf4 = summary(iterative_k)$edf[4],
                edf5 = summary(iterative_k)$edf[5],
                edf6 = summary(iterative_k)$edf[6])

diff_df <- data.frame(var1 = k_prime$k1_prime - edf_lst$edf1,
                      var2 = k_prime$k2_prime - edf_lst$edf2,
                      var3 = k_prime$k3_prime - edf_lst$edf3,
                      var4 = k_prime$k4_prime - edf_lst$edf4,
                      var5 = k_prime$k5_prime - edf_lst$edf5,
                      var6 = k_prime$k6_prime - edf_lst$edf6)

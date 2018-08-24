### Modified model_selection_fn.R
gam_select_fn <- function(data = data,
                          response = response,
                          smooth_terms = smooth_terms,
                          smooth_names = smooth_names,
                          rand_eff_smooth = rand_eff_smooth,
                          gamma = NULL,
                          family = Gamma(link = log),
                          in_parallel = FALSE,
                          est_k = FALSE){
  require(mgcv)
  require(magrittr)
  require(plyr)
  require(dplyr)
  
  if(in_parallel == TRUE){
    
    require(parallel)
    require(snow)
    require(pipeR)
    require(doParallel)
    
  }
  
  ### Options for gamma
  
  if(is.null(gamma) == TRUE){
    gamma = 1
  } else{
    gamma = gamma
  }
  
  
  
  ### create matrix of all variable combinations
  N   <- length(smooth_terms)
  vec <- c(TRUE, FALSE)
  
  
  lst <- lapply(numeric(N), function(x) vec)
  
  
  gamMat <- expand.grid(lst)
  
  rm( lst, N, vec)
  
  ### Create all model Combinations
  
  gamMat <- gamMat[-(dim(gamMat)[1]),]
  names(gamMat) <- c(smooth_names)
  
  all_Models_List <- apply( gamMat, 1, function(x) as.formula(
    paste( c( response, 
              paste( c( smooth_terms[x], 
                        rand_eff_smooth),
                     collapse = " + ") ), 
           collapse = " ~ ") ) )
  
  rm( rand_eff_smooth, response, smooth_terms)
  
  
  ### Create a list with names for all vars in each row of regMar ######
  ### This will be used to name edf k_0 ... #####
  col_names <- apply(gamMat, 1, function(x) smooth_names[x] )
  
  ### Create k_i data.frame for each item in col_names list ####
  
  k_i <- vector("list", length = nrow( gamMat ) )
  
  for(i in 1:nrow(gamMat)){
    
    k_i[[i]] <- rbind( add_k = as.integer( gamMat[ i, c( col_names[[i]] )] ),
                       k_0 = rep( 5, length( col_names[[i]] ) ),
                       k_prime = numeric( length( col_names[[i]] ) ), 
                       edf_vec = numeric( length( col_names[[i]] ) ), 
                       diff_df = numeric( length( col_names[[i]] ) ) ) 
    
    colnames(k_i[[i]]) <- col_names[[i]]
    
  };rm(i)
  
  ### Fit All Model Combinations
  
  if(in_parallel == FALSE){
    
    if(est_k == FALSE){
      
      all_Models_Results <- lapply(all_Models_List,
                                   function(x) gam(x, data = data,
                                                   family = family,
                                                   gamma = gamma))
    } else if(est_k == TRUE){
      
      require(pipeR)
      
      all_Models_Results <- vector("list", length = length(all_Models_List))
      
      for(i in 1:length(all_Models_Results)){
        
        old_gcv <- 1000000
        new_gcv <- numeric(1)
        
        while( any( k_i[[i]][ 'add_k', ] == 1)  ){
          
          all_Models_Results[[i]] <- gam(all_Models_List[[i]],
                                         data = data,
                                         family = family,
                                         gamma = gamma) %>>%
            (~tmp)
          
          new_gcv <- tmp$gcv.ubre
          
          tmp %>>%
            summary() %>>%
            (~tmp2)
          
          k_i[[i]]['edf_vec', ] <- tmp2$edf[1:ncol(k_i[[i]])]
          
          
          for(j in col_names[[i]] ){
            
            k_i[[i]]['k_prime', j] = k_i[[i]]['k_0', j] - 1
            
            
            
            if( j == 'Lat_Lon') {
              
              k_i[[i]]['diff_df', j] <- k_i[[i]]['k_prime', j] - k_i[[i]]['edf_vec', j] -8
              
              ifelse( (k_i[[i]]['diff_df', j] <= 10) == TRUE & (new_gcv <= old_gcv) == TRUE,
                      k_i[[i]]['add_k', j] <- 1,
                      k_i[[i]]['add_k', j] <- 0)
              ifelse( ( k_i[[i]]['diff_df', j] <= 10) == TRUE & (new_gcv <= old_gcv) == TRUE,
                      k_i[[i]]['k_0', j] <- k_i[[i]]['k_0', j] + 5,
                      k_i[[i]]['add_k', j] <- 0)
              
            } else{
              
              k_i[[i]]['diff_df', j] <- k_i[[i]]['k_prime', j] - k_i[[i]]['edf_vec', j] 
              
              ifelse( (k_i[[i]]['diff_df', j] <= 2) == TRUE & (new_gcv <= old_gcv) == TRUE,
                      k_i[[i]]['add_k', j] <- 1,
                      k_i[[i]]['add_k', j] <- 0)
              ifelse( (k_i[[i]]['diff_df', j] <= 2) == TRUE & (new_gcv <= old_gcv) == TRUE,
                      k_i[[i]]['k_0', j] <- k_i[[i]]['k_0', j] + 1,
                      k_i[[i]]['add_k', j] <- 0)
            }
            
            
            
            
            
          }
          
          if(new_gcv < old_gcv){
            
            old_gcv <- new_gcv
            
          }
          
        }
        
      }
      
    }
    
  } else if(in_parallel == TRUE){
    
    if(est_k == FALSE){
      
      no_cores <- detectCores()
      
      cl <- makeCluster(no_cores)
      
      environment(all_Models_List) <- .GlobalEnv
      
      clusterExport(cl, list(data = "data", smooth_terms = "smooth_terms", smooth_names = "smooth_names", 
                             response = "response", gamma = "gamma", family = "family", 
                             regMat = "regMat", all_Models_List = "all_Models_List"),
                    envir = environment())
      
      clusterEvalQ(cl, c(require(mgcv), require(dplyr), require(magrittr), require(plyr),
                         require(parallel), require(snow)))
      
      all_Models_Results <- parLapply(cl,
                                      all_Models_List,
                                      function(x) gam(x, data = data,
                                                      family = family,
                                                      gamma = gamma))
      
      stopCluster(cl)
      
    } else if(est_k == TRUE){
      
      stop("est_k doesn't work in Parallel yet!!")
      
      environment(all_Models_List) <- .GlobalEnv
      environment(k_i) <- .GlobalEnv
      environment(col_names) <- .GlobalEnv
      
      number_of_cores <- parallel::detectCores() 
      clusters <- parallel::makeCluster(number_of_cores)
      doParallel::registerDoParallel(clusters)
      
      all_Models_Results <- vector("list", length = length(all_Models_List))
      
      foreach(i = 1:length(all_Models_List),
              .combine = list,
              .export=ls(envir=globalenv()),
              .multicombine=TRUE,
              .packages = c("pipeR", "mgcv")) %dopar% {
                
                old_gcv <- 1000000
                new_gcv <- numeric(1)
              } %*%{
                
                while( any( k_i[[i]][ 'add_k', ] == 1)  ){
                  
                  all_Models_Results[[i]] <- gam(all_Models_List[[i]],
                                                 data = data,
                                                 family = family,
                                                 gamma = gamma) %>>%
                    (~tmp)
                  
                  new_gcv <- tmp$gcv.ubre
                  
                  tmp %>>%
                    summary() %>>%
                    (~tmp2)
                  
                  k_i[[i]]['edf_vec', ] <- tmp2$edf[1:ncol(k_i[[i]])]
                  
                  
                  for(j in col_names[[i]] ){
                    
                    k_i[[i]]['k_prime', j] = k_i[[i]]['k_0', j] - 1
                    
                    for(k in 1:length(j)){
                      
                      if( j[k] == 'Lat_Lon') {
                        
                        k_i[[i]]['diff_df', j[k]] <- k_i[[i]]['k_prime', j[k]] - k_i[[i]]['edf_vec', j[k]] -8
                        
                        ifelse( (k_i[[i]]['diff_df', j[k]] <= 10) == TRUE & (new_gcv <= old_gcv) == TRUE,
                                k_i[[i]]['add_k', j[k]] <- 1,
                                k_i[[i]]['add_k', j[k]] <- 0)
                        ifelse( ( k_i[[i]]['diff_df', j[k]] <= 10) == TRUE & (new_gcv <= old_gcv) == TRUE,
                                k_i[[i]]['k_0', j[k]] <- k_i[[i]]['k_0', j[k]] + 5,
                                k_i[[i]]['add_k', j[k]] <- 0)
                        
                      } else{
                        
                        k_i[[i]]['diff_df', j[k]] <- k_i[[i]]['k_prime', j[k]] - k_i[[i]]['edf_vec', j[k]] 
                        
                        ifelse( (k_i[[i]]['diff_df', j[k]] <= 2) == TRUE & (new_gcv <= old_gcv) == TRUE,
                                k_i[[i]]['add_k', j[k]] <- 1,
                                k_i[[i]]['add_k', j[k]] <- 0)
                        ifelse( (k_i[[i]]['diff_df', j[k]] <= 2) == TRUE & (new_gcv <= old_gcv) == TRUE,
                                k_i[[i]]['k_0', j[k]] <- k_i[[i]]['k_0', j[k]] + 1,
                                k_i[[i]]['add_k', j[k]] <- 0)
                      }
                      
                    }
                    
                    
                    
                  }
                  
                  if(new_gcv < old_gcv){
                    
                    old_gcv <- new_gcv
                    
                  }
                  
                } 
                
              }
      
      stopCluster(clusters) 
    }
  }
  
  ### Clean Names
  
  Name_List <- apply(gamMat, 1, function(x)
    paste(smooth_names[x],collapse = ", ") )
  
  mod_names <- apply(gamMat, 1, function(x)
    paste(smooth_names[x],collapse = "_") )
  
  names(all_Models_Results) <- unlist(mod_names)
  
  ### Summary Stats
  
  NoOfSmooths <- unlist(apply(gamMat, 1, sum))
  
  AIC <- unlist(lapply(all_Models_Results, function(x)
    AIC(x)))
  
  ####
  BIC <- unlist(lapply(all_Models_Results, function(x)
    BIC(x)))
  
  GCV <- unlist(lapply(all_Models_Results, function(x)
    x$gcv.ubre))
  
  
  min_AIC <- min(AIC)
  
  delta_AIC <- numeric(length(AIC))
  for(i in 1:length(AIC)){
    delta_AIC[i] = AIC[i] - min_AIC
  }
  
  min_BIC <- min(BIC)
  
  delta_BIC <- numeric(length(BIC))
  for(i in 1:length(BIC)){
    delta_BIC[i] = BIC[i] - min_BIC
  }
  
  
  ### Create results df
  
  results <- data.frame( model = unlist(Name_List),
                         NoOfSmooths = NoOfSmooths,
                         AIC = AIC,
                         delta_AIC = delta_AIC,
                         BIC = BIC,
                         delta_BIC = delta_BIC,
                         GCV = GCV) %>%
    arrange(AIC)
  
  ### Make list of model results and fitted objects
  
  Output <- list(results = results,
                 model_fits = all_Models_Results)
  
  
  return(Output)
  
}

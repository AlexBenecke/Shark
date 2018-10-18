### Modified model_selection_fn.R
gam_select_fn <- function(data,
                          response,
                          smooth_terms,
                          constant_term,
                          re_term,
                          smooth_names,
                          gamma = 1,
                          family = Gamma(link = log),
                          in_parallel = FALSE,
                          est_k = FALSE){
  ### Load Required Packages
  require(mgcv)
  require(magrittr)
  require(plyr)
  require(dplyr)
  
  
  ### create matrix of all variable combinations
  N   <- length(smooth_terms)
  vec <- c(TRUE, FALSE)
  
  lst <- lapply(numeric(N), function(x) vec)
  
  
  gamMat <- expand.grid(lst)
  
  rm( lst, N, vec)
  
  ### Create all model Combinations
  
  if(is.null(constant_term) == TRUE){
    
    gamMat <- gamMat[-(dim(gamMat)[1]),]
    names(gamMat) <- c(smooth_names)
    
    all_Models_List <- apply( gamMat, 1, function(x) as.formula(
      paste( c( response, paste( c(smooth_terms[x], re_term), collapse = " + ") ), collapse = " ~ ") 
    ) )
    
    
  } else{
    
    var0 <- TRUE
    gamMat <- cbind(var0, gamMat)
    
    names(gamMat) <- c(smooth_names)
    
    terms <- c(constant_term, smooth_terms, re_term)
    
    all_Models_List <- apply( gamMat, 1, function(x) as.formula(
      paste( c( response, paste( terms[x], collapse = " + ") ), collapse = " ~ ") 
    ) )
  }
  
  rm( response, constant_term, smooth_terms, re_term)
  
  mod_names <- apply(gamMat, 1, function(x)
    paste(smooth_names[x],collapse = "_") )
  
  names(all_Models_List) <- unlist(mod_names)
  
  ### Create K_i
  
  col_names <- apply(gamMat, 1, function(x) smooth_names[x] )
  
  k_i <- vector("list", length = nrow( gamMat ) )
  
  for(i in 1:nrow(gamMat)){
    
    k_i[[i]] <- rbind( add_k = as.integer( gamMat[ i, c( col_names[[i]] )] ),
                       k_0 = rep( 5, length( col_names[[i]] ) ),
                       k_prime = numeric( length( col_names[[i]] ) ), 
                       edf_vec = numeric( length( col_names[[i]] ) ), 
                       diff_df = numeric( length( col_names[[i]] ) ) ) 
    
    colnames(k_i[[i]]) <- col_names[[i]]
    
  };rm(i)
  
  ### Fit all Models
  
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
      
      while( any( k_i[[i]][ 'add_k', ] == 1) & new_gcv <= old_gcv  ){
        
        old_gcv <- new_gcv
        
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
          
          if( j == 'LatLon') {
            
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
      }
    }
  }
  
  ### Clean names
  
  names(all_Models_Results) <- mod_names
  
  
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
  
  results <- data.frame( model = unlist(mod_names),
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

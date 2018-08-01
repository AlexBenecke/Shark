gam_select_fn <- function(data = data,
                          smooth_terms = smooth_terms,
                          names = names,
                          response = response,
                          gamma = NULL,
                          family = Gamma(link = log),
                          in_parallel = FALSE){
  require(mgcv)
  require(magrittr)
  require(plyr)
  require(dplyr)
  
  if(in_parallel == TRUE){
    
    require(parallel)
    require(snow)
    
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
  
  
  regMat <- expand.grid(lst)
  
  ### Create all model Combinations
  
  regMat <- regMat[-(dim(regMat)[1]),]
  names(regMat) <- c(names)
  
  all_Models_List <- apply(regMat, 1, function(x) as.formula(
    paste(c(response, paste(smooth_terms[x], 
                            collapse = " + ")), 
          collapse = " ~ ")) )
  
  
  ### Fit All Model Combinations
  
  if(in_parallel == FALSE){
    
      all_Models_Results <- lapply(all_Models_List,
                                   function(x) gam(x, data = data,
                                                   family = family,
                                                   gamma = gamma))
      
  } else if(in_parallel == TRUE){
    
    no_cores <- detectCores()
    
    cl <- makeCluster(no_cores)
    
    environment(all_Models_List) <- .GlobalEnv
    
    clusterExport(cl, list(data = "data", smooth_terms = "smooth_terms", names = "names", 
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
    
  }
  
  ### Clean Names
  
  Name_List <- apply(regMat, 1, function(x)
    paste(names[x],collapse = ", ") )
  
  mod_names <- apply(regMat, 1, function(x)
    paste(names[x],collapse = "_") )
  
  names(all_Models_Results) <- unlist(mod_names)
  
  ### Summary Stats
  
  NoOfSmooths <- unlist(apply(regMat, 1, sum))
  
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
  
  
  ### Create results df
  
  results <- data.frame( model = unlist(Name_List),
                         NoOfSmooths = NoOfSmooths,
                         AIC = AIC,
                         delta_AIC = delta_AIC,
                         BIC = BIC,
                         GCV = GCV) %>%
    arrange(AIC)
  
  ### Make list of model results and fitted objects
  
  Output <- list(results = results,
                 model_fits = all_Models_Results)
  
  
  return(Output)
  
}

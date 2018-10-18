### Create Multiple combinations of variables




mult_comb <- function(response,
                      smooth_terms,
                      constant_term = NULL,
                      smooth_names ){
  
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
      paste( c( response, paste( smooth_terms[x], collapse = " + ") ), collapse = " ~ ") 
    ) )
    
    
  } else{
    
    var0 <- TRUE
    gamMat <- cbind(var0, gamMat)
    
    names(gamMat) <- c(smooth_names)
    
    terms <- c(constant_term, smooth_terms)
    
    all_Models_List <- apply( gamMat, 1, function(x) as.formula(
      paste( c( response, paste( terms[x], collapse = " + ") ), collapse = " ~ ") 
    ) )
    
  }
  
  rm( response, constant_term, smooth_terms)
  
  mod_names <- apply(gamMat, 1, function(x)
    paste(smooth_names[x],collapse = "_") )
  
  names(all_Models_List) <- unlist(mod_names)
  
  
  return(all_Models_List)
  
}










































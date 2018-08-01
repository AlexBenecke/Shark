fit_gam <- function(dat, name = "", gamma = NULL, 
                    k_lat = 10, k_lon = 10, k_dep = 10, 
                    k_oxy = 10, k_sal = 10, k_yr = 10,
                    est_k = TRUE){
  
  source("k_est_fn.R")
  
  
  if(is.null(gamma) == TRUE){
    gamma = 1
  } else{
    gamma = gamma
  }
  
  if(est_k == TRUE){
    k_lon = est_k(dat)$lon
    k_lat = est_k(dat)$lat
    k_dep = est_k(dat)$dep 
    k_oxy = est_k(dat)$oxy 
    k_sal = est_k(dat)$sal 
    k_yr = length((unique(dat$YEAR)))
  } else{
    k_lon = k_lon
    k_lat = k_lat
    k_dep = k_dep 
    k_oxy = k_oxy 
    k_sal = k_sal 
    k_yr = k_yr
  }
    
    gam_fits <- list(
      assign(paste0(name, "_mod0"), 
             gam((cpe + 1) ~ s(STARTLON, STARTLAT, 
                               k = k_lon + k_lat) +
                   s(YEAR, 
                     bs = "re", 
                     k = k_yr),
                 family = Gamma(link=log),
                 data = dat,
                 gamma = gamma)),
      
      
      assign(paste0(name, "_dep"), 
             gam((cpe + 1) ~ s(STARTLON, STARTLAT, 
                               k = k_lon + k_lat) +
                   s(AVG.DEPTH, 
                     k = k_dep) +
                   s(YEAR, 
                     bs = "re", 
                     k = k_yr),
                 family = Gamma(link=log),
                 data = dat,
                 gamma = gamma)),
      
      
      assign(paste0(name, "_oxy"), 
             gam((cpe + 1) ~ s(STARTLON, STARTLAT, 
                               k = k_lon + k_lat) +
                   s(OXYBOTM, 
                     k = k_oxy) +
                   s(YEAR, 
                     bs = "re", 
                     k = k_yr),
                 family = Gamma(link=log),
                 data = dat,
                 gamma = gamma)),
      
      
      assign(paste0(name, "_sal"), 
             gam((cpe + 1) ~ s(STARTLON, STARTLAT, 
                               k = k_lon + k_lat) +
                   s(SALBOTM,
                     k = k_sal) +
                   s(YEAR, 
                     bs = "re", 
                     k = k_yr),
                 family = Gamma(link=log),
                 data = dat,
                 gamma = gamma)),
      
      
      assign(paste0(name, "_dep_oxy"), 
             gam((cpe + 1) ~ s(STARTLON, STARTLAT, 
                               k = k_lon + k_lat) +
                   s(AVG.DEPTH, 
                     k = k_dep) +
                   s(OXYBOTM, 
                     k = k_oxy) +
                   s(YEAR, 
                     bs = "re", 
                     k = k_yr),
                 family = Gamma(link=log),
                 data = dat,
                 gamma = gamma)),
      
      
      assign(paste0(name, "_dep_sal"), 
             gam((cpe + 1) ~ s(STARTLON, STARTLAT,
                               k = k_lon + k_lat) +
                   s(AVG.DEPTH, 
                     k = k_dep) +
                   s(SALBOTM, 
                     k = k_sal) +
                   s(YEAR, 
                     bs = "re", 
                     k = k_yr),
                 family = Gamma(link=log),
                 data = dat,
                 gamma = gamma)),
      
      
      assign(paste0(name, "_oxy_sal"), 
             gam((cpe + 1) ~ s(STARTLON, STARTLAT, 
                               k = k_lon + k_lat) + 
                   s(OXYBOTM, 
                     k = k_oxy) +
                   s(SALBOTM, 
                     k = k_sal) +
                   s(YEAR, 
                     bs = "re", 
                     k = k_yr),
                 family = Gamma(link=log),
                 data = dat,
                 gamma = gamma)),
      
      
      assign(paste0(name, "_base_mod"), 
             gam((cpe + 1) ~ s(STARTLON, STARTLAT, 
                               k = k_lon + k_lat) +
                   s(AVG.DEPTH, 
                     k = k_dep) +
                   s(OXYBOTM, 
                     k = k_oxy) +
                   s(SALBOTM, 
                     k = k_sal) +
                   s(YEAR, 
                     bs = "re", 
                     k = k_yr),
                 family = Gamma(link=log),
                 data = dat,
                 gamma = gamma))
    )    
  
  
  names(gam_fits) <- c(paste0(name, "_mod0"),
                       paste0(name, "_dep"),
                       paste0(name, "_oxy"),
                       paste0(name, "_sal"),
                       paste0(name, "_dep_oxy"),
                       paste0(name, "_dep_sal"),
                       paste0(name, "_oxy_sal"),
                       paste0(name, "_base_mod"))
  
  return(gam_fits)
  
  
}

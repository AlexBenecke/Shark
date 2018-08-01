my_summary <- function(data){
  
  whole <- data.frame(n_Sites = nrow(data),
                      n_Present = nrow(data[data$cpe > 0,]),
                      Average = mean(data$cpe),  
                      Min = range(data$cpe)[1], 
                      Max = range(data$cpe)[2], 
                      SD = sd(data$cpe))
  
  
  n_sites_yr <- data.frame(table(data$YEAR))
  names(n_sites_yr) <- c("Year", "n_Sites_Sampled")
  
  present <- data[data$cpe >0,]
  n_cap_yr <- data.frame(table(present$YEAR))
  names(n_cap_yr) <- c("Year", "n_Sites_Present")
  
  
  avg_yr <- aggregate(cpe~YEAR, data = data, FUN = mean)
  names(avg_yr) <- c("Year", "Average")
  
  min_yr <- aggregate(cpe~YEAR, data = data, FUN = min)
  names(min_yr) <- c("Year", "Min")
  
  max_yr <- aggregate(cpe~YEAR, data = data, FUN = max)
  names(max_yr) <- c("Year", "Max")
  
  st_dev_yr <- aggregate(cpe~YEAR, data = data, FUN = sd)
  names(st_dev_yr) <- c("Year", "St.Dev")
  
  
  annual <- left_join(n_sites_yr, n_cap_yr, by = "Year") %>%
    left_join(avg_yr, by = "Year") %>%
    left_join(min_yr, by = "Year") %>%
    left_join(max_yr, by = "Year") %>%
    left_join(st_dev_yr, by = "Year")
  
  
  ### plots
  
  
  
  output <- list(whole = whole, annual = annual)
  return(output)
}

### Overall Mean Function
overall_mean_fn <- function(dat){
  
  require(sciplot)
  
  for(i in 1:length(dat)){
    assign(paste0(names(dat[i]),"_cpe"),
           dat[[i]] %>%
             select(cpe, YEAR)
    )
  }
  
  combined_cpe <- rbind(blacknose_cpe,
                        spinner_cpe, 
                        silky_cpe,
                        bull_cpe,
                        blacktip_cpe,
                        sandbar_cpe,
                        tiger_cpe,
                        sharpnose_cpe)
  
  bn_center_mean <- tapply(blacknose_cpe$cpe, 
                           blacknose_cpe$YEAR, 
                           mean) - tapply(combined_cpe$cpe, 
                                          combined_cpe$YEAR, 
                                          mean)
  
  bt_center_mean <- tapply(blacktip_cpe$cpe, 
                           blacktip_cpe$YEAR, 
                           mean) - tapply(combined_cpe$cpe, 
                                          combined_cpe$YEAR, 
                                          mean)
  
  bu_center_mean <- tapply(bull_cpe$cpe, 
                           bull_cpe$YEAR, 
                           mean) - tapply(combined_cpe$cpe, 
                                          combined_cpe$YEAR, 
                                          mean)
  
  sb_center_mean <- tapply(sandbar_cpe$cpe, 
                           sandbar_cpe$YEAR, 
                           mean) - tapply(combined_cpe$cpe, 
                                          combined_cpe$YEAR, 
                                          mean)
  
  sn_center_mean <- tapply(sharpnose_cpe$cpe, 
                           sharpnose_cpe$YEAR, 
                           mean) - tapply(combined_cpe$cpe, 
                                          combined_cpe$YEAR, 
                                          mean)
  
  si_center_mean <- tapply(silky_cpe$cpe, 
                           silky_cpe$YEAR, 
                           mean) - tapply(combined_cpe$cpe, 
                                          combined_cpe$YEAR, 
                                          mean)
  
  sp_center_mean <- tapply(spinner_cpe$cpe, 
                           spinner_cpe$YEAR, 
                           mean) - tapply(combined_cpe$cpe, 
                                          combined_cpe$YEAR, 
                                          mean)
  
  ti_center_mean <- tapply(tiger_cpe$cpe, 
                           tiger_cpe$YEAR, 
                           mean) - tapply(combined_cpe$cpe, 
                                          combined_cpe$YEAR, 
                                          mean)
  y_lim <- c(-6, 6)

  
  barplot(bn_center_mean,
          ylim = y_lim,
          las = 2,
          main = "Blacknose")
  
  barplot(bt_center_mean,
          ylim = y_lim,
          las = 2,
          main = "Blacktip")
  
  barplot(bu_center_mean,
          ylim = y_lim,
          las = 2,
          main = "Bull")
  
  barplot(sb_center_mean,
          ylim = y_lim,
          las = 2,
          main = "Sandbar")
  
  barplot(sn_center_mean,
          ylim = y_lim,
          las = 2,
          main = "Sharpnose")
  
  barplot(si_center_mean,
          ylim = y_lim,
          las = 2,
          main = "Silky")
  
  barplot(sp_center_mean,
          ylim = y_lim,
          las = 2,
          main = "Spinner")
  
  barplot(ti_center_mean,
          ylim = y_lim,
          las = 2,
          main = "Tiger")
}



##############################################
dat.list = sp.df
overall_mean_fn <- function(dat.list){
  
  require(dplyr)
  
  for(i in 1:length(dat.list)){
    assign(paste0(names(dat.list[i]),"_cpe"),
           dat.list[[i]] %>%
             select(cpe, YEAR)
    )
    assign(paste0(names(dat.list[i]), "_agg"),
           aggregate(dat.list[[i]]$cpe, by = list(dat.list[[i]]$YEAR), 
                       FUN = function(x) c(mean = mean(dat.list[[i]]$cpe), 
                                           sd = sd(dat.list[[i]]$cpe), 
                                           n = length(dat.list[[i]]$cpe))))
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  require(sciplot)
  
  for(i in 1:length(dat)){
    assign(paste0(names(dat[i]),"_cpe"),
           dat[[i]] %>%
             select(cpe, YEAR)
    )
  }
  
  combined_cpe <- rbind(blacknose_cpe,
                        spinner_cpe, 
                        silky_cpe,
                        bull_cpe,
                        blacktip_cpe,
                        sandbar_cpe,
                        tiger_cpe,
                        sharpnose_cpe)
  
  bn_center_mean <- tapply(blacknose_cpe$cpe, 
                           blacknose_cpe$YEAR, 
                           mean) - tapply(combined_cpe$cpe, 
                                          combined_cpe$YEAR, 
                                          mean)
  
  
  
  bt_center_mean <- tapply(blacktip_cpe$cpe, 
                           blacktip_cpe$YEAR, 
                           mean) - tapply(combined_cpe$cpe, 
                                          combined_cpe$YEAR, 
                                          mean)
  
  bu_center_mean <- tapply(bull_cpe$cpe, 
                           bull_cpe$YEAR, 
                           mean) - tapply(combined_cpe$cpe, 
                                          combined_cpe$YEAR, 
                                          mean)
  
  sb_center_mean <- tapply(sandbar_cpe$cpe, 
                           sandbar_cpe$YEAR, 
                           mean) - tapply(combined_cpe$cpe, 
                                          combined_cpe$YEAR, 
                                          mean)
  
  sn_center_mean <- tapply(sharpnose_cpe$cpe, 
                           sharpnose_cpe$YEAR, 
                           mean) - tapply(combined_cpe$cpe, 
                                          combined_cpe$YEAR, 
                                          mean)
  
  si_center_mean <- tapply(silky_cpe$cpe, 
                           silky_cpe$YEAR, 
                           mean) - tapply(combined_cpe$cpe, 
                                          combined_cpe$YEAR, 
                                          mean)
  
  sp_center_mean <- tapply(spinner_cpe$cpe, 
                           spinner_cpe$YEAR, 
                           mean) - tapply(combined_cpe$cpe, 
                                          combined_cpe$YEAR, 
                                          mean)
  
  ti_center_mean <- tapply(tiger_cpe$cpe, 
                           tiger_cpe$YEAR, 
                           mean) - tapply(combined_cpe$cpe, 
                                          combined_cpe$YEAR, 
                                          mean)
  y_lim <- c(-6, 6)
  
  
  barplot(bn_center_mean,
          ylim = y_lim,
          las = 2,
          main = "Blacknose")
  
  barplot(bt_center_mean,
          ylim = y_lim,
          las = 2,
          main = "Blacktip")
  
  barplot(bu_center_mean,
          ylim = y_lim,
          las = 2,
          main = "Bull")
  
  barplot(sb_center_mean,
          ylim = y_lim,
          las = 2,
          main = "Sandbar")
  
  barplot(sn_center_mean,
          ylim = y_lim,
          las = 2,
          main = "Sharpnose")
  
  barplot(si_center_mean,
          ylim = y_lim,
          las = 2,
          main = "Silky")
  
  barplot(sp_center_mean,
          ylim = y_lim,
          las = 2,
          main = "Spinner")
  
  barplot(ti_center_mean,
          ylim = y_lim,
          las = 2,
          main = "Tiger")
}







































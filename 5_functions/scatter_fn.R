### Scatter Plot Function


scatter_fn <- function(x){
  
  for(i in 1:length(sp.df)){
    
    oldpar <- par(mfrow=c(3,4), mar=c(4,4,2,1), oma=c(0,0,3,1))
    
    plot(cpe~YEAR,
         data = sp.df[[i]])
    mtext(paste0("r = ",
                 cor(sp.df[[i]]$YEAR, sp.df[[i]]$cpe)),
          side=3,line=0)
    
    plot(cpe~MONTH,
         data = sp.df[[i]])
    
    plot(cpe~AVG.DEPTH,
         data = sp.df[[i]])
    mtext(paste0("r = ",
                 cor(sp.df[[i]]$AVG.DEPTH, sp.df[[i]]$cpe, use = "complete.obs")),
          side=3,line=0)
    
    plot(cpe~SALBOTM,
         data = sp.df[[i]])
    mtext(paste0("r = ",
                 cor(sp.df[[i]]$SALBOTM, sp.df[[i]]$cpe, use = "complete.obs")),
          side=3,line=0)
    
    plot(cpe~TEMPBOTM,
         data = sp.df[[i]])
    mtext(paste0("r = ",
                 cor(sp.df[[i]]$TEMPBOTM, sp.df[[i]]$cpe, use = "complete.obs")),
          side=3,line=0)
    
    plot(cpe~FLUOROBOTM,
         data = sp.df[[i]])
    mtext(paste0("r = ",
                 cor(sp.df[[i]]$FLUOROBOTM, sp.df[[i]]$cpe, use = "complete.obs")),
          side=3,line=0)
    
    plot(cpe~TURBBOTM,
         data = sp.df[[i]])
    mtext(paste0("r = ",
                 cor(sp.df[[i]]$TURBBOTM, sp.df[[i]]$cpe, use = "complete.obs")),
          side=3,line=0)
    
    plot(cpe~OXYBOTM,
         data = sp.df[[i]])
    mtext(paste0("r = ",
                 cor(sp.df[[i]]$OXYBOTM, sp.df[[i]]$cpe, use = "complete.obs")),
          side=3,line=0)
    
    plot(cpe~dombot8,
         data = sp.df[[i]],
         xlab="", las=2)
    
    plot(cpe~dombot4,
         data = sp.df[[i]],
         xlab="", las=2)
    
    
    plot(cpe~STARTLAT,
         data = sp.df[[i]])
    mtext(paste0("r = ",
                 cor(sp.df[[i]]$STARTLAT, sp.df[[i]]$cpe)),
          side=3,line=0)
    
    plot(cpe~STARTLON,
         data = sp.df[[i]])
    mtext(paste0("r = ",
                 cor(sp.df[[i]]$STARTLON, sp.df[[i]]$cpe)),
          side=3,line=0)
    
    mtext(names(sp.df[i]), side=3, line=0.65, outer=TRUE, cex=2, 
          font=2)
    
    par(oldpar)
  }
}








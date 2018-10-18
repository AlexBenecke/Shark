plot_interpolation = function(master.df = df.1,
                              taxa.df.1 = df.1,
                              taxa.df.2 = df.2) {
  
  require(matlab)
  require(spatstat)
  require(raster)
  require(rgeos)
  require(sp)
  require(rgdal)
  require(alphahull)
  require(igraph)
  require(maptools)
  require(fields)
  
  if (!exists("poly.")) {
    poly. <- create_bounding_grid_polygons(master.df = df.1) }
  
  interp.raster.stack <- raster::stack()
  
  for (j in 1:2) {
    
    hex.grid.poly <- poly.[[1]]
    bounding.poly <- poly.[[2]]
    extract.mat <- as.data.frame(poly.[[3]])
    in.rast <- poly.[[4]]
    in.rast[1:ncell(in.rast)] <- NA
    
    if (j == 1) {
      ind <- cellFromXY(object = in.rast, xy = as.matrix(cbind(taxa.df.1$startlon, taxa.df.1$startlat)))
      taxa.df.1$ind <- ind
      ind.tib <- as.data.frame(taxa.df.1 %>% group_by(ind) %>% summarize(mean.v = mean(pres))) }
    
    if (j == 2) {
      ind <- cellFromXY(object = in.rast, xy = as.matrix(cbind(taxa.df.2$startlon, taxa.df.2$startlat)))
      taxa.df.2$ind <- ind
      ind.tib <- as.data.frame(taxa.df.2 %>% group_by(ind) %>% summarize(mean.v = mean(CPUE))) }
    
    names(ind.tib)[1] <- "cell"
    
    extract.mat <- left_join(extract.mat, ind.tib, by = "cell")
    extract.mat.2 <- extract.mat %>% group_by(value) %>% summarise(mean.val = mean(mean.v, na.rm = T))
    
    extract.mat <- left_join(extract.mat, extract.mat.2, by = "value")
    
    in.rast[extract.mat$cell] <- extract.mat$mean.val
    # plot(in.rast)
    
    chlor.raster <- in.rast
    chlor.spat.est.agg <- raster::aggregate(chlor.raster,30,na.rm = T)
    xy <- data.frame(xyFromCell(chlor.spat.est.agg, 1:ncell(chlor.spat.est.agg)))
    v <- getValues(chlor.spat.est.agg)
    tps <- suppressWarnings(Tps(xy, v))
    pred.rast <- raster(chlor.raster)
    interp.raster <- interpolate(pred.rast, tps)   
    
    # interp.raster <- t(t(flip(t(t(t(interp.raster))),'y')))
    
    interp.raster <- mask(interp.raster, bounding.poly) 
    plot(interp.raster,
         bty="n")
    
    interp.raster.stack <- raster::addLayer(interp.raster.stack, interp.raster)
  }
  
  interp.raster.stack <- raster::addLayer(interp.raster.stack, interp.raster.stack[[1]]*interp.raster.stack[[2]])
  
  return(interp.raster.stack)
  
}
  
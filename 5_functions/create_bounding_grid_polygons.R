create_bounding_grid_polygons = function(master.df = df.1) {
  
  # startlat','startlon'
  
  require(matlab)
  require(spatstat)
  require(raster)
  require(rgeos)
  require(sp)
  require(rgdal)
  require(alphahull)
  require(igraph)
  require(maptools)
  
  df.un <- master.df[,c('startlat','startlon')]
  df.un <- df.un[!duplicated(df.un),]
  n10 <- ashape(x = df.un[,2], y = df.un[,1], alpha = 0.8)
  n10g = graph.edgelist(cbind(as.character(n10$edges[, "ind1"]), 
                              as.character(n10$edges[, "ind2"])), directed = FALSE)
  cutg = n10g - E(n10g)[1]
  ends = names(which(degree(cutg) == 1))
  path = get.shortest.paths(cutg, ends[1], ends[2])[[1]]
  pathX = as.numeric(V(n10g)[path[[1]]]$name)
  pathX = c(pathX, pathX[1])
  plot(n10$x[pathX, ])
  lines(n10$x[pathX, ])
  
  ## set coordinate reference system with SpatialPolygons(..., proj4string=CRS(...)) e.g. CRS("+proj=longlat +datum=WGS84")
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(n10$x[pathX, ])), ID=1))) 
  PolySPDF <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))
  sp::plot(PolySPDF, add=TRUE, col="gray80")
  writePolyShape(x=PolySPDF, fn="PolyID")
  
  create_HEX_SQR_SHP <- function(square.dim = c(),
                                 hexagon.radius = c(0.1),
                                 in.shape = "PolyID.shp")  {
    
    # Establish shape file
    ShapeStem   <- in.shape
    OverlayFile <- readShapePoly(ShapeStem)
    OverlayFile.name <- unlist(strsplit(chartr(old = "\\",new = "/",ShapeStem),"/"))[length(unlist(strsplit(chartr(old = "\\",new = "/",ShapeStem),"/")))]
    shape.name <- substr(OverlayFile.name,1,c(nchar(OverlayFile.name) - 4))
    field.values <- as.data.frame(OverlayFile) 
    
    shape.dir.path <- substr(strsplit(ShapeStem,shape.name)[[1]][1],1,nchar(strsplit(ShapeStem,shape.name)[[1]][1])-1)
    SHP.dir <- paste(shape.dir.path,"\\",shape.name,"_GRID_SHP",sep = "")
    suppressWarnings(dir.create(SHP.dir))   
    
    field.values <- as.data.frame(OverlayFile) 
    
    # For single polygons in the .SHP file
    if (dim(field.values)[1] == 1)   {                        
      Polygon.name <- shape.name
      Polygon.names  <- Polygon.name }
    
    
    ##########################################################################################################################
    # create_hex_grid
    if (length(hexagon.radius) > 0) {
      for (j in 1:nrow(field.values)) {
        for (z in 1:length(hexagon.radius)) {
          
          data.in <- as.data.frame(OverlayFile)[j,] 
          extract.shape <- SpatialPolygonsDataFrame(SpatialPolygons((list(OverlayFile@polygons[[j]]))),data = data.in)  
          polygon.k <- SpatialPolygonsDataFrame(extract.shape,data = data.in)
          
          # Estabish "crop bound"
          crop.bound <- bbox(polygon.k) 
          hex.list <- list()
          radius <- hexagon.radius[z]
          
          x.pts <- c(seq(crop.bound[1]-1,crop.bound[3]+1,by = 3*radius))
          y.pts <- c(seq(crop.bound[2]-1,crop.bound[4]+1,by = 2*radius))                          
          cent.pt <- expand.grid(x.pts,y.pts)    
          
          x.pts <- c(seq(crop.bound[1]-1.5*radius-1,crop.bound[3]+1.5*radius+1,by = 3*radius))
          y.pts <- c(seq(crop.bound[2]-radius-1,crop.bound[4]+radius+1,by = 2*radius))                            
          cent.pt <- rbind(expand.grid(x.pts,y.pts),cent.pt)  
          
          count <- 1
          center.coords <- c()
          
          for (k in 1:dim(cent.pt)[1]) {
            st.pt.x <- c(cent.pt[k,1]-radius,
                         cent.pt[k,1]-radius/2,
                         cent.pt[k,1]+radius/2,
                         cent.pt[k,1]+radius,
                         cent.pt[k,1]+radius/2,
                         cent.pt[k,1]-radius/2,
                         cent.pt[k,1]-radius)
            
            st.pt.y <- c(cent.pt[k,2],
                         cent.pt[k,2]+radius,
                         cent.pt[k,2]+radius,
                         cent.pt[k,2],
                         cent.pt[k,2]-radius,
                         cent.pt[k,2]-radius,
                         cent.pt[k,2])
            
            hex.list[[count]] <- (cbind(x=st.pt.x,y=st.pt.y))
            center.coords <- rbind(center.coords,cent.pt[k,])
            count <- count + 1  
          }
          
          # Convert polygon list to "hex.list" a SpatialPolygonsDataFrame
          for (k in 1:length(hex.list)) {hex.list[[k]] <- Polygon(hex.list[[k]])}
          for (k in 1:length(hex.list)) {hex.list[[k]] <- Polygons(list(hex.list[[k]]),ID=as.character(k))}  
          hex.list <- SpatialPolygons(hex.list)
          hex.list <- SpatialPolygonsDataFrame(hex.list,data =data.frame(ID = seq(1,length(hex.list))))  
          spatialPDF <- hex.list
          grid.dim <-  radius
          
          return(spatialPDF)
        }
      }
    }
  }
  
hex.grid.poly <- create_HEX_SQR_SHP(square.dim = c(),
                                    hexagon.radius = c(0.1),
                                    in.shape = "PolyID.shp")
bounding.poly <- PolySPDF

in.rast <- raster(extent(bbox(bounding.poly)), res = 0.01)
in.rast[1:ncell(in.rast)] <- 1
extract.df <- extract(in.rast, hex.grid.poly, cellnumbers = T)

extract.mat <- c()
for (j in 1:length(extract.df)) {
  if (!is.null(extract.df[[j]])) {
    extract.df[[j]][,2] = j  
    extract.mat <- rbind(extract.mat, extract.df[[j]])
  }
}

return(list(hex.grid.poly, bounding.poly, extract.mat, in.rast)) }
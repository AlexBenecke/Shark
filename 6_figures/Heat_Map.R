### Heat Map

### Load Packages
library(rgdal)

source("5_functions/dropbox_fn.R") # For interacting with dropbox
proj = "shark" # Specify project where data is stored 
source("5_functions/load_data.R")

#### Load Data #####
eff3 <- read_csv_fn( "data/clean-data/CPUE3.csv", proj)

strat1_Adult <- load_data(data = eff3[eff3$maturity == "A",] %>% droplevels(), strata = 1, fact_vars = FALSE)
strat1_Juvenile <- load_data(data = eff3[eff3$maturity == "J",] %>% droplevels(), strata = 1, fact_vars = FALSE)

strat2_Adult <- load_data(data = eff3[eff3$maturity == "A",] %>% droplevels(), strata = 2, fact_vars = FALSE)
strat2_Juvenile <- load_data(data = eff3[eff3$maturity == "J",] %>% droplevels(), strata = 2, fact_vars = FALSE)

df_1_lst <- list(blacknose_Female_Adult = strat1_Adult$Female$blacknose,
                 blacknose_Male_Adult = strat1_Adult$Male$blacknose,
                 blacknose_Female_Juvenile = strat1_Juvenile$Female$blacknose,
                 blacknose_Male_Juvenile = strat1_Juvenile$Male$blacknose
                 ,
                 blacktip_Female_Adult = strat1_Adult$Female$blacktip,
                 blacktip_Male_Adult = strat1_Adult$Male$blacktip,
                 blacktip_Female_Juvenile = strat1_Juvenile$Female$blacktip,
                 blacktip_Male_Juvenile = strat1_Juvenile$Male$blacktip
                 ,
                 sharpnose_Female_Adult = strat1_Adult$Female$sharpnose,
                 sharpnose_Male_Adult = strat1_Adult$Male$sharpnose,
                 sharpnose_Female_Juvenile = strat1_Juvenile$Female$sharpnose,
                 sharpnose_Male_Juvenile = strat1_Juvenile$Male$sharpnose
)

df_2_lst <- list(blacknose_Female_Adult = strat2_Adult$Female$blacknose,
                 blacknose_Male_Adult = strat2_Adult$Male$blacknose,
                 blacknose_Female_Juvenile = strat2_Juvenile$Female$blacknose,
                 blacknose_Male_Juvenile = strat2_Juvenile$Male$blacknose
                 ,
                 blacktip_Female_Adult = strat2_Adult$Female$blacktip,
                 blacktip_Male_Adult = strat2_Adult$Male$blacktip,
                 blacktip_Female_Juvenile = strat2_Juvenile$Female$blacktip,
                 blacktip_Male_Juvenile = strat2_Juvenile$Male$blacktip
                 ,
                 sharpnose_Female_Adult = strat2_Adult$Female$sharpnose,
                 sharpnose_Male_Adult = strat2_Adult$Male$sharpnose,
                 sharpnose_Female_Juvenile = strat2_Juvenile$Female$sharpnose,
                 sharpnose_Male_Juvenile = strat2_Juvenile$Male$sharpnose
)

rm(eff3, strat1_Adult, strat1_Juvenile, strat2_Adult, strat2_Juvenile)
##### #####

### Load Shapefile
require(maptools)
require(sp)
require(rgdal)
require(lubridate)

gulf.shape <- "shape-file\\Shape\\stateshigh.shp"
gulf.shape <- maptools::readShapePoly(gulf.shape)

### Load Rasters

filenames <- list.files("output/output_GAM/raster",
                        pattern = "*.Rdata",
                        full.names = TRUE)
filenames <- filenames[c(9:12)]


for(i in 1:length(filenames)){
  
  load(filenames[i])
  
}

### Figure Options
layer <- 2

cex_pts <- 1
pch_pts <- "."

line_top <- 0

cex_txt <- 1.25

my_xlim = c(-100,-80)
my_ylim = c(22,33)


### Make Figures


### Sharpnose
tiff(filename = "6_figures/figures/sharpnose_layer2.tiff", width = 3000,  height = 2000, res = 300)

par(mfrow = c(2,2),
    oma = c(0,2,2,1),
    mar = c(3,3,0.5,0.5)#c(2,4,3,1)
    )

## FA
plot(raster_sharpnose_FA[[layer]])

sp::plot(gulf.shape, add= T,
         xlab = "", ylab = "",
         xlim = my_xlim, ylim = my_ylim,
         #xaxt = "n", yaxt = "n",
         col = "gray",
         bty="n")

points(startlat~startlon, 
       data = df_2_lst$sharpnose_Female_Adult,
       xlim = my_xlim, ylim = my_ylim,
       pch = pch_pts,
       col = "black",
       #bg="black",
       cex = cex_pts)

## FJ
plot(raster_sharpnose_FJ[[layer]], bty = "n")

sp::plot(gulf.shape, add= T,
         xlab = "", ylab = "",
         xlim = my_xlim, ylim = my_ylim,
         #xaxt = "n", yaxt = "n",
         col = "gray",
         bty="n")

points(startlat~startlon, 
       data = df_2_lst$sharpnose_Female_Juvenile,
       xlim = my_xlim, ylim = my_ylim,
       pch = pch_pts,
       col = "black",
       #bg="black",
       cex = cex_pts)

## MA
plot(raster_sharpnose_MA[[layer]], bty = "n")

sp::plot(gulf.shape, add= T,
         xlab = "", ylab = "",
         xlim = my_xlim, ylim = my_ylim,
         #xaxt = "n", yaxt = "n",
         col = "gray",
         bty="n")

points(startlat~startlon, 
       data = df_2_lst$sharpnose_Male_Adult,
       xlim = my_xlim, ylim = my_ylim,
       pch = pch_pts,
       col = "black",
       #bg="black",
       cex = cex_pts)

## MJ
plot(raster_sharpnose_MJ[[layer]], bty = "n")

sp::plot(gulf.shape, add= T,
         xlab = "", ylab = "",
         xlim = my_xlim, ylim = my_ylim,
         #xaxt = "n", yaxt = "n",
         col = "gray",
         bty="n")

points(startlat~startlon, 
       data = df_2_lst$sharpnose_Male_Juvenile,
       xlim = my_xlim, ylim = my_ylim,
       pch = pch_pts,
       col = "black",
       #bg="black",
       cex = cex_pts)

mtext("Female",
      side = 2,
      at=3/4,
      outer = TRUE,
      cex = cex_txt)

mtext("Male",
      side = 2,
      at=1/4,
      outer = TRUE,
      cex = cex_txt)

mtext("Adult",
      side = 3,
      at=1/4,
      outer = TRUE,
      cex = cex_txt,
      line = line_top)

mtext("Juvenile",
      side = 3,
      at=3/4,
      outer = TRUE,
      cex = cex_txt,
      line = line_top)

mtext("(c)",
      side = 3, 
      at = 0.025,
      line = line_top,
      cex=cex_txt,
      outer = TRUE)

dev.off()

### Blacknose

tiff(filename = "6_figures/figures/blacknose_layer3.tiff", width = 3000,  height = 2000, res = 300)

par(mfrow = c(2,2),
    oma = c(0,2,2,1),
    mar = c(3,3,0.5,0.5)#c(2,4,3,1)
)

## FA
plot(raster_blacknose_FA[[layer]])

sp::plot(gulf.shape, add= T,
         xlab = "", ylab = "",
         xlim = my_xlim, ylim = my_ylim,
         #xaxt = "n", yaxt = "n",
         col = "gray",
         bty="n")

points(startlat~startlon, 
       data = df_2_lst$blacknose_Female_Adult,
       xlim = my_xlim, ylim = my_ylim,
       pch = pch_pts,
       col = "black",
       #bg="black",
       cex = cex_pts)

## FJ
plot(raster_blacknose_FJ[[layer]], bty = "n")

sp::plot(gulf.shape, add= T,
         xlab = "", ylab = "",
         xlim = my_xlim, ylim = my_ylim,
         #xaxt = "n", yaxt = "n",
         col = "gray",
         bty="n")

points(startlat~startlon, 
       data = df_2_lst$blacknose_Female_Juvenile,
       xlim = my_xlim, ylim = my_ylim,
       pch = pch_pts,
       col = "black",
       #bg="black",
       cex = cex_pts)

## MA
plot(raster_blacknose_MA[[layer]], bty = "n")

sp::plot(gulf.shape, add= T,
         xlab = "", ylab = "",
         xlim = my_xlim, ylim = my_ylim,
         #xaxt = "n", yaxt = "n",
         col = "gray",
         bty="n")

points(startlat~startlon, 
       data = df_2_lst$blacknose_Male_Adult,
       xlim = my_xlim, ylim = my_ylim,
       pch = pch_pts,
       col = "black",
       #bg="black",
       cex = cex_pts)

## MJ
plot(raster_blacknose_MJ[[layer]], bty = "n")

sp::plot(gulf.shape, add= T,
         xlab = "", ylab = "",
         xlim = my_xlim, ylim = my_ylim,
         #xaxt = "n", yaxt = "n",
         col = "gray",
         bty="n")

points(startlat~startlon, 
       data = df_2_lst$blacknose_Male_Juvenile,
       xlim = my_xlim, ylim = my_ylim,
       pch = pch_pts,
       col = "black",
       #bg="black",
       cex = cex_pts)

mtext("Female",
      side = 2,
      at=3/4,
      outer = TRUE,
      cex = cex_txt)

mtext("Male",
      side = 2,
      at=1/4,
      outer = TRUE,
      cex = cex_txt)

mtext("Adult",
      side = 3,
      at=1/4,
      outer = TRUE,
      cex = cex_txt,
      line = line_top)

mtext("Juvenile",
      side = 3,
      at=3/4,
      outer = TRUE,
      cex = cex_txt,
      line = line_top)

mtext("(a)",
      side = 3, 
      at = 0.025,
      line = line_top,
      cex=cex_txt,
      outer = TRUE)

dev.off()


### Blacktip


tiff(filename = "6_figures/figures/blacktip_layer3.tiff", width = 3000,  height = 2000, res = 300)

par(mfrow = c(2,2),
    oma = c(0,2,2,1),
    mar = c(3,3,0.5,0.5)#c(2,4,3,1)
)

## FA
plot(raster_blacktip_FA[[layer]])

sp::plot(gulf.shape, add= T,
         xlab = "", ylab = "",
         xlim = my_xlim, ylim = my_ylim,
         #xaxt = "n", yaxt = "n",
         col = "gray",
         bty="n")

points(startlat~startlon, 
       data = df_2_lst$blacktip_Female_Adult,
       xlim = my_xlim, ylim = my_ylim,
       pch = pch_pts,
       col = "black",
       #bg="black",
       cex = cex_pts)

## FJ
plot(raster_blacktip_FJ[[layer]], bty = "n")

sp::plot(gulf.shape, add= T,
         xlab = "", ylab = "",
         xlim = my_xlim, ylim = my_ylim,
         #xaxt = "n", yaxt = "n",
         col = "gray",
         bty="n")

points(startlat~startlon, 
       data = df_2_lst$blacktip_Female_Juvenile,
       xlim = my_xlim, ylim = my_ylim,
       pch = pch_pts,
       col = "black",
       #bg="black",
       cex = cex_pts)

## MA
plot(raster_blacktip_MA[[layer]], bty = "n")

sp::plot(gulf.shape, add= T,
         xlab = "", ylab = "",
         xlim = my_xlim, ylim = my_ylim,
         #xaxt = "n", yaxt = "n",
         col = "gray",
         bty="n")

points(startlat~startlon, 
       data = df_2_lst$blacktip_Male_Adult,
       xlim = my_xlim, ylim = my_ylim,
       pch = pch_pts,
       col = "black",
       #bg="black",
       cex = cex_pts)

## MJ
plot(raster_blacktip_MJ[[layer]], bty = "n")

sp::plot(gulf.shape, add= T,
         xlab = "", ylab = "",
         xlim = my_xlim, ylim = my_ylim,
         #xaxt = "n", yaxt = "n",
         col = "gray",
         bty="n")

points(startlat~startlon, 
       data = df_2_lst$blacktip_Male_Juvenile,
       xlim = my_xlim, ylim = my_ylim,
       pch = pch_pts,
       col = "black",
       #bg="black",
       cex = cex_pts)

mtext("Female",
      side = 2,
      at=3/4,
      outer = TRUE,
      cex = cex_txt)

mtext("Male",
      side = 2,
      at=1/4,
      outer = TRUE,
      cex = cex_txt)

mtext("Adult",
      side = 3,
      at=1/4,
      outer = TRUE,
      cex = cex_txt,
      line = line_top)

mtext("Juvenile",
      side = 3,
      at=3/4,
      outer = TRUE,
      cex = cex_txt,
      line = line_top)

mtext("(b)",
      side = 3, 
      at = 0.025,
      line = line_top,
      cex=cex_txt,
      outer = TRUE)

dev.off()




















########################################
library(mgcv)
source("5_functions/dropbox_fn.R") # For interacting with dropbox
proj = "shark" # Specify project where data is stored 
source("5_functions/load_data.R")

source("5_functions/create_bounding_grid_polygons.R")
source("5_functions/plot_interpolation.R")
load("5_functions/shark.data.Rdata")

load("output/output_GAM/best/sharpnose/strat1/sn_s1_MA_best.rda")
load("output/output_GAM/best/sharpnose/strat2/sn_s2_MA_best.rda")

eff3 <- read_csv_fn( "data/clean-data/CPUE3.csv", proj)

strat1_Adult <- load_data(data = eff3[eff3$maturity == "A",] %>% droplevels(), strata = 1, fact_vars = FALSE)
strat2_Adult <- load_data(data = eff3[eff3$maturity == "A",] %>% droplevels(), strata = 2, fact_vars = FALSE)



master.df <- eff3 %>%
  subset(., common == "Blacknose" | common ==  "Blacktip" | common ==  "Sharpnose") %>%
  select(startlat,startlon) %>%
  na.omit()

head(master.df)  
str(master.df)



df.1 <- strat1_Adult$Male$sharpnose %>% 
  select(year,startlat:bottype4) %>% 
  mutate(pres = unlist( predict.gam( sn_s1_MA_best, newdata = ., type = "response" ) ) )

str(df.1)

df.2 <- strat2_Adult$Male$sharpnose %>%
  select(year,startlat:bottype4) %>% 
  mutate(CPUE = unlist( predict.gam( sn_s2_MA_best, newdata = ., type = "response" ) ) ) 
  
str(df.2) 


poly. <- create_bounding_grid_polygons(master.df)



tmp_rast <- plot_interpolation(master.df = master.df,
                              taxa.df.1 = df.1,
                              taxa.df.2 = df.2)


plot(tmp_rast)

### Loop for all species




eff3 <- read_csv_fn( "data/clean-data/CPUE3.csv", proj)

strat1_Adult <- load_data(data = eff3[eff3$maturity == "A",] %>% droplevels(), strata = 1, fact_vars = FALSE)
strat2_Adult <- load_data(data = eff3[eff3$maturity == "A",] %>% droplevels(), strata = 2, fact_vars = FALSE)

strat1_Juvenile <- load_data(data = eff3[eff3$maturity == "J",] %>% droplevels(), strata = 1, fact_vars = FALSE)
strat2_Juvenile <- load_data(data = eff3[eff3$maturity == "J",] %>% droplevels(), strata = 2, fact_vars = FALSE)
































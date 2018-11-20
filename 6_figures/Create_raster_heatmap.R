### Creating Heat Maps all Species and Models

### Load Packages
library(mgcv)
library(FSA)
library(stringr)
library(magrittr)
library(dplyr)
source("5_functions/dropbox_fn.R") # For interacting with dropbox
proj = "shark" # Specify project where data is stored 
source("5_functions/load_data.R")

source("5_functions/create_bounding_grid_polygons.R")
source("5_functions/plot_interpolation.R")


### Load Data

eff3 <- read_csv_fn( "data/clean-data/CPUE3.csv", proj)

  ### Create Master.df
master.df <- eff3 %>%
  subset( common == "Blacknose" | common ==  "Blacktip" | common ==  "Sharpnose") %>%
  dplyr::select(startlat,startlon) %>%
  na.omit()

head(master.df)  
str(master.df)

  ### Create df.1 and df.1
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


### List Models

filenames_s1 <- list.files(c("output/output_GAM/best/blacknose/strat1",
                             "output/output_GAM/best/blacktip/strat1",
                             "output/output_GAM/best/sharpnose/strat1"),
                           pattern = "*.rda",
                           full.names = TRUE)

filenames_s1

filenames_s2 <- list.files(c("output/output_GAM/best/blacknose/strat2",
                             "output/output_GAM/best/blacktip/strat2",
                             "output/output_GAM/best/sharpnose/strat2"),
                           pattern = "*.rda",
                           full.names = TRUE)

filenames_s2

### Create poly.

#poly. <- create_bounding_grid_polygons(master.df)

#10-12-18#save(poly., file = "output/output_GAM/poly..rda")

load(file = "output/output_GAM/poly..rda")

### Function to load data and create and save raster containing the heatmaps

    ### currently only using sharpnose models (9-12) due to different variables being in the data frame
for(i in 9:12){#1:length(filenames_s1)){
  
  
  SP <- str_split(filenames_s1[i], "_", simplify = TRUE)[1,2] %>%
    str_split(., "/", simplify = TRUE) %>% 
    .[1,3]
  SM <- str_split(filenames_s1[i], "_", simplify = TRUE)[1,4] 
  rast_ID <- paste0(SP, "_", SM)
  
  rm(SP,SM)
  
  load(filenames_s1[i])
  
  mod_1 <- str_split(filenames_s1[i], "/", simplify = TRUE)[1,6] %>% 
    str_split(., pattern = ".rda", simplify = TRUE) %>% 
    .[1,1]
  
  load(filenames_s2[i])
  
  mod_2 <- str_split(filenames_s2[i], "/", simplify = TRUE)[1,6] %>% 
    str_split(., pattern = ".rda", simplify = TRUE) %>% 
    .[1,1]
  
  df_1 <- df_1_lst[[i]] %>% 
    dplyr::select(year,startlat:bottype4) %>% 
    mutate(pres = unlist( predict.gam( get(mod_1), newdata = ., type = "response" ) ) )
  
  
  df_2 <- df_2_lst[[i]] %>%
    dplyr::select(year,startlat:bottype4) %>% 
    mutate(CPUE = unlist( predict.gam( get(mod_2), newdata = ., type = "response" ) ) )
  
  rm(list = c(mod_1, mod_2))
  
  assign(paste0("raster_", rast_ID),
         plot_interpolation(master.df = master.df,
                            taxa.df.1 = df_1,
                            taxa.df.2 = df_2)) 
  
           #save( get(paste0("raster_", rast_ID)), file = paste0("output/output_GAM/raster/", "raster_", rast_ID, ".Rdata"))
  

  
  #writeRaster( get(paste0("raster_", rast_ID)), file = paste0("output/output_GAM/raster/", "raster_", rast_ID, ".grd"), format = "raster")
  
  rm(df_1, df_2, mod_1, mod_2, rast_ID)
  
  
}


save( raster_blacknose_FA, file = "output/output_GAM/raster/raster_blacknose_FA.Rdata")
save( raster_blacknose_FJ, file = "output/output_GAM/raster/raster_blacknose_FJ.Rdata")
save( raster_blacknose_MA, file = "output/output_GAM/raster/raster_blacknose_MA.Rdata")
save( raster_blacknose_MJ, file = "output/output_GAM/raster/raster_blacknose_MJ.Rdata")

save( raster_blacktip_FA, file = "output/output_GAM/raster/raster_blacktip_FA.Rdata")
save( raster_blacktip_FJ, file = "output/output_GAM/raster/raster_blacktip_FJ.Rdata")
save( raster_blacktip_MA, file = "output/output_GAM/raster/raster_blacktip_MA.Rdata")
save( raster_blacktip_MJ, file = "output/output_GAM/raster/raster_blacktip_MJ.Rdata")

save( raster_sharpnose_FA, file = "output/output_GAM/raster/raster_sharpnose_FA.Rdata")
save( raster_sharpnose_FJ, file = "output/output_GAM/raster/raster_sharpnose_FJ.Rdata")
save( raster_sharpnose_MA, file = "output/output_GAM/raster/raster_sharpnose_MA.Rdata")
save( raster_sharpnose_MJ, file = "output/output_GAM/raster/raster_sharpnose_MJ.Rdata")

# Warning messages:
# 1: package ‘fields’ was built under R version 3.5.1
# 2: package ‘spam’ was built under R version 3.5.1
# 3: package ‘dotCall64’ was built under R version 3.5.1
# 4: package ‘maps’ was built under R version 3.5.1
# 5: In x@data@values[i] <- value :
#   number of items to replace is not a multiple of replacement length
# 6: In x@data@values[i] <- value :
#   number of items to replace is not a multiple of replacement length
# 7: In x@data@values[i] <- value :
#   number of items to replace is not a multiple of replacement length
# 8: In x@data@values[i] <- value :
#   number of items to replace is not a multiple of replacement length
# 9: In x@data@values[i] <- value :
#   number of items to replace is not a multiple of replacement length
# 10: In x@data@values[i] <- value :
#   number of items to replace is not a multiple of replacement length
# 11: In x@data@values[i] <- value :
#   number of items to replace is not a multiple of replacement length
# 12: In x@data@values[i] <- value :
#   number of items to replace is not a multiple of replacement length













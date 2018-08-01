### Fit all gam Models
### 7/30/2018

### Load function
source("model_selection_fn.R")

### Load Data

blacknose <- read.csv("data/acronotus.df.csv") 
blacknose$DATE <- as.POSIXlt(blacknose$DATE)
blacknose$MONTH <- factor(strftime(blacknose$DATE, format = "%b"))
blacknose$YEAR <- factor(blacknose$YEAR)
tmp.bn <- blacknose[blacknose$cpe>0,]

str(blacknose)


blacktip <- read.csv("data/limbatus.df.csv")
blacktip$DATE <- as.POSIXlt(blacktip$DATE)
blacktip$MONTH  <- factor(strftime(blacktip$DATE, format = "%b"))
blacktip$YEAR <- factor(blacktip$YEAR)
tmp.bt <- blacktip[blacktip$cpe>0,]

str(blacktip)

bull <- read.csv("data/leucas.df.csv")
bull$DATE <- as.POSIXlt(bull$DATE)
bull$MONTH  <- factor(strftime(bull$DATE, format = "%b"))
bull$YEAR <- factor(bull$YEAR)
tmp.bu <- bull[bull$cpe>0,]

str(bull)

sandbar <- read.csv("data/plumbeus.df.csv")
sandbar$DATE <- as.POSIXlt(sandbar$DATE)
sandbar$MONTH  <- factor(strftime(sandbar$DATE, format = "%b"))
sandbar$YEAR <- factor(sandbar$YEAR)
tmp.sb <- sandbar[sandbar$cpe>0,]

str(sandbar)

sharpnose <- read.csv("data/terraenovae.df.csv")
sharpnose$DATE <- as.POSIXlt(sharpnose$DATE)
sharpnose$MONTH  <- factor(strftime(sharpnose$DATE, format = "%b"))
sharpnose$YEAR <- factor(sharpnose$YEAR)
tmp.sn <- sharpnose[sharpnose$cpe>0,]

str(sharpnose)

silky <- read.csv("data/falciformis.df.csv")
silky$DATE <- as.POSIXlt(silky$DATE)
silky$MONTH  <- factor(strftime(silky$DATE, format = "%b"))
silky$YEAR <- factor(silky$YEAR)
tmp.si <- silky[silky$cpe>0,]

str(silky)

spinner <- read.csv("data/brevipinna.df.csv")
spinner$DATE <- as.POSIXlt(spinner$DATE)
spinner$MONTH <- factor(strftime(spinner$DATE, format = "%b"))
spinner$YEAR <- factor(spinner$YEAR)
tmp.sp <- spinner[spinner$cpe>0,]

str(spinner)

tiger <- read.csv("data/cuvier.df.csv")
tiger$DATE <- as.POSIXlt(tiger$DATE)
tiger$MONTH  <- factor(strftime(tiger$DATE, format = "%b"))
tiger$YEAR <- factor(tiger$YEAR)
tmp.ti <- tiger[tiger$cpe>0,]

str(tiger)



### Blacknose

bn.time <- system.time(
  bn_gam_select_1 <- gam_select_fn(data = blacknose,
                                   smooth_terms = c("s(STARTLON, STARTLAT, k = k_lon + k_lat)",
                                                    "s(I(AVG.DEPTH^0.25), k = k_dep)",
                                                    "s(TEMPBOTM, k = k_temp)",
                                                    "s(TURBBOTM, k = k_turb)",
                                                    "s(OXYBOTM, k = k_oxy)",
                                                    "s(SALBOTM, k = k_sal)",
                                                    "s(YEAR, bs = 're', k = k_yr)"),
                                   names = c("Lat_Lon","Root_Dep", "Temp", "Turb", 
                                             "Oxy", "Sal", "Yr"),
                                   response = "(cpe + 1)",
                                   gamma = 1.4,
                                   choose_k = FALSE,
                                   dim_k = list(k_lon = 30,
                                                k_lat = 30,
                                                k_dep = 10,
                                                k_temp = 10,
                                                k_turb = 5,
                                                k_oxy = 5,
                                                k_sal = 5,
                                                k_yr = 18),
                                   family = Gamma(link = log),
                                   in_parallel = TRUE)
)


bn.time

#user  system elapsed 
#1.64    0.69   87.78

save(bn_gam_select_1, file = "model-output/bn_gam_select_1.rda")


### Blacktip

bt.time <- system.time(
  bt_gam_select_1 <- gam_select_fn(data = blacktip,
                                   smooth_terms = c("s(STARTLON, STARTLAT, k = k_lon + k_lat)",
                                                    "s(I(AVG.DEPTH^0.25), k = k_dep)",
                                                    "s(TEMPBOTM, k = k_temp)",
                                                    "s(TURBBOTM, k = k_turb)",
                                                    "s(OXYBOTM, k = k_oxy)",
                                                    "s(SALBOTM, k = k_sal)",
                                                    "s(YEAR, bs = 're', k = k_yr)"),
                                   names = c("Lat_Lon","Root_Dep", "Temp", "Turb", 
                                             "Oxy", "Sal", "Yr"),
                                   response = "(cpe + 1)",
                                   gamma = 1.4,
                                   choose_k = FALSE,
                                   dim_k = list(k_lon = 30,
                                                k_lat = 30,
                                                k_dep = 10,
                                                k_temp = 10,
                                                k_turb = 5,
                                                k_oxy = 5,
                                                k_sal = 5,
                                                k_yr = 18),
                                   family = Gamma(link = log),
                                   in_parallel = TRUE)
)

bt.time

save(bt_gam_select_1, file = "model-output/bt_gam_select_1.rda")


### Bull

bu.time <- system.time(
  bu_gam_select_1 <- gam_select_fn(data = bull,
                                   smooth_terms = c("s(STARTLON, STARTLAT, k = k_lon + k_lat)",
                                                    "s(I(AVG.DEPTH^0.25), k = k_dep)",
                                                    "s(TEMPBOTM, k = k_temp)",
                                                    "s(TURBBOTM, k = k_turb)",
                                                    "s(OXYBOTM, k = k_oxy)",
                                                    "s(SALBOTM, k = k_sal)",
                                                    "s(YEAR, bs = 're', k = k_yr)"),
                                   names = c("Lat_Lon","Root_Dep", "Temp", "Turb", 
                                             "Oxy", "Sal", "Yr"),
                                   response = "(cpe + 1)",
                                   gamma = 1.4,
                                   choose_k = FALSE,
                                   dim_k = list(k_lon = 30,
                                                k_lat = 30,
                                                k_dep = 10,
                                                k_temp = 10,
                                                k_turb = 5,
                                                k_oxy = 5,
                                                k_sal = 5,
                                                k_yr = 18),
                                   family = Gamma(link = log),
                                   in_parallel = TRUE)
)

save(bu_gam_select_1, file = "model-output/bu_gam_select_1.rda")


### Sandbar

sb.time <- system.time(
  sb_gam_select_1 <- gam_select_fn(data = sandbar,
                                   smooth_terms = c("s(STARTLON, STARTLAT, k = k_lon + k_lat)",
                                                    "s(I(AVG.DEPTH^0.25), k = k_dep)",
                                                    "s(TEMPBOTM, k = k_temp)",
                                                    "s(TURBBOTM, k = k_turb)",
                                                    "s(OXYBOTM, k = k_oxy)",
                                                    "s(SALBOTM, k = k_sal)",
                                                    "s(YEAR, bs = 're', k = k_yr)"),
                                   names = c("Lat_Lon","Root_Dep", "Temp", "Turb", 
                                             "Oxy", "Sal", "Yr"),
                                   response = "(cpe + 1)",
                                   gamma = 1.4,
                                   choose_k = FALSE,
                                   dim_k = list(k_lon = 30,
                                                k_lat = 30,
                                                k_dep = 10,
                                                k_temp = 10,
                                                k_turb = 5,
                                                k_oxy = 5,
                                                k_sal = 5,
                                                k_yr = 18),
                                   family = Gamma(link = log),
                                   in_parallel = TRUE)
)

save(sb_gam_select_1, file = "model-output/sb_gam_select_1.rda")


### sharpnose

sn.time <- system.time(
  sn_gam_select_1 <- gam_select_fn(data = sharpnose,
                                   smooth_terms = c("s(STARTLON, STARTLAT, k = k_lon + k_lat)",
                                                    "s(I(AVG.DEPTH^0.25), k = k_dep)",
                                                    "s(TEMPBOTM, k = k_temp)",
                                                    "s(TURBBOTM, k = k_turb)",
                                                    "s(OXYBOTM, k = k_oxy)",
                                                    "s(SALBOTM, k = k_sal)",
                                                    "s(YEAR, bs = 're', k = k_yr)"),
                                   names = c("Lat_Lon","Root_Dep", "Temp", "Turb", 
                                             "Oxy", "Sal", "Yr"),
                                   response = "(cpe + 1)",
                                   gamma = 1.4,
                                   choose_k = FALSE,
                                   dim_k = list(k_lon = 30,
                                                k_lat = 30,
                                                k_dep = 10,
                                                k_temp = 10,
                                                k_turb = 5,
                                                k_oxy = 5,
                                                k_sal = 5,
                                                k_yr = 18),
                                   family = Gamma(link = log),
                                   in_parallel = TRUE)
)

save(sn_gam_select_1, file = "model-output/sn_gam_select_1.rda")


### silky

si.time <- system.time(
  si_gam_select_1 <- gam_select_fn(data = silky,
                                   smooth_terms = c("s(STARTLON, STARTLAT, k = k_lon + k_lat)",
                                                    "s(I(AVG.DEPTH^0.25), k = k_dep)",
                                                    "s(TEMPBOTM, k = k_temp)",
                                                    "s(TURBBOTM, k = k_turb)",
                                                    "s(OXYBOTM, k = k_oxy)",
                                                    "s(SALBOTM, k = k_sal)",
                                                    "s(YEAR, bs = 're', k = k_yr)"),
                                   names = c("Lat_Lon","Root_Dep", "Temp", "Turb", 
                                             "Oxy", "Sal", "Yr"),
                                   response = "(cpe + 1)",
                                   gamma = 1.4,
                                   choose_k = FALSE,
                                   dim_k = list(k_lon = 30,
                                                k_lat = 30,
                                                k_dep = 10,
                                                k_temp = 10,
                                                k_turb = 5,
                                                k_oxy = 5,
                                                k_sal = 5,
                                                k_yr = 18),
                                   family = Gamma(link = log),
                                   in_parallel = TRUE)
)

save(si_gam_select_1, file = "model-output/si_gam_select_1.rda")


### spinner

sp.time <- system.time(
  sp_gam_select_1 <- gam_select_fn(data = spinner,
                                   smooth_terms = c("s(STARTLON, STARTLAT, k = k_lon + k_lat)",
                                                    "s(I(AVG.DEPTH^0.25), k = k_dep)",
                                                    "s(TEMPBOTM, k = k_temp)",
                                                    "s(TURBBOTM, k = k_turb)",
                                                    "s(OXYBOTM, k = k_oxy)",
                                                    "s(SALBOTM, k = k_sal)",
                                                    "s(YEAR, bs = 're', k = k_yr)"),
                                   names = c("Lat_Lon","Root_Dep", "Temp", "Turb", 
                                             "Oxy", "Sal", "Yr"),
                                   response = "(cpe + 1)",
                                   gamma = 1.4,
                                   choose_k = FALSE,
                                   dim_k = list(k_lon = 30,
                                                k_lat = 30,
                                                k_dep = 10,
                                                k_temp = 10,
                                                k_turb = 5,
                                                k_oxy = 5,
                                                k_sal = 5,
                                                k_yr = 18),
                                   family = Gamma(link = log),
                                   in_parallel = TRUE)
) 

save(sp_gam_select_1, file = "model-output/sp_gam_select_1.rda")


### tiger

ti.time <- system.time(
  ti_gam_select_1 <- gam_select_fn(data = tiger,
                                   smooth_terms = c("s(STARTLON, STARTLAT, k = k_lon + k_lat)",
                                                    "s(I(AVG.DEPTH^0.25), k = k_dep)",
                                                    "s(TEMPBOTM, k = k_temp)",
                                                    "s(TURBBOTM, k = k_turb)",
                                                    "s(OXYBOTM, k = k_oxy)",
                                                    "s(SALBOTM, k = k_sal)",
                                                    "s(YEAR, bs = 're', k = k_yr)"),
                                   names = c("Lat_Lon","Root_Dep", "Temp", "Turb", 
                                             "Oxy", "Sal", "Yr"),
                                   response = "(cpe + 1)",
                                   gamma = 1.4,
                                   choose_k = FALSE,
                                   dim_k = list(k_lon = 30,
                                                k_lat = 30,
                                                k_dep = 10,
                                                k_temp = 10,
                                                k_turb = 5,
                                                k_oxy = 5,
                                                k_sal = 5,
                                                k_yr = 18),
                                   family = Gamma(link = log),
                                   in_parallel = TRUE)
)

save(ti_gam_select_1, file = "model-output/ti_gam_select_1.rda")

ti.time



### initialize time matrix

time_list <- list(bn.time = bn.time,
                  bt.time = bt.time,
                  bu.time = bu.time,
                  sb.time = sb.time,
                  sn.time = sn.time,
                  si.time = si.time,
                  sp.time = sp.time,
                  ti.time = ti.time)


save(time_list, file = "model-output/time_list.rda")


load(file = "model-output/time_list.rda")

time_list

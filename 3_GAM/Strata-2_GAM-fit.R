### Strata 2 CPUE

### Load function
source("5_functions/model_selection_fn.R")


### Load Data

bn_pos_catch <- read.csv("data/clean-data/blacknose.csv") %>%
  subset(cpe > 0) %>%
  mutate(YEAR = factor(YEAR))

str(bn_pos_catch)


bt_pos_catch <- read.csv("data/clean-data/blacktip.csv") %>%
  subset(cpe > 0) %>%
  mutate(YEAR = factor(YEAR))

str(bt_pos_catch)

bu_pos_catch <- read.csv("data/clean-data/bull.csv") %>%
  subset(cpe > 0) %>%
  mutate(YEAR = factor(YEAR))

str(bu_pos_catch)

sb_pos_catch <- read.csv("data/clean-data/sandbar.csv") %>%
  subset(cpe > 0) %>%
  mutate(YEAR = factor(YEAR))

str(sb_pos_catch)

sn_pos_catch <- read.csv("data/clean-data/sharpnose.csv") %>%
  subset(cpe > 0) %>%
  mutate(YEAR = factor(YEAR))

str(sn_pos_catch)

si_pos_catch <- read.csv("data/clean-data/silky.csv") %>%
  subset(cpe > 0) %>%
  mutate(YEAR = factor(YEAR))

str(si_pos_catch)

sp_pos_catch <- read.csv("data/clean-data/spinner.csv") %>%
  subset(cpe > 0) %>%
  mutate(YEAR = factor(YEAR))

str(sp_pos_catch)

ti_pos_catch <- read.csv("data/clean-data/tiger.csv") %>%
  subset(cpe > 0) %>%
  mutate(YEAR = factor(YEAR))

str(ti_pos_catch)

### Blacknose

bn.time.2 <- system.time(
  bn_gam_strat_2 <- gam_select_fn(data = bn_pos_catch,
                                  smooth_terms = c("s(STARTLON, STARTLAT, k = 60)",
                                                   "s(I(AVG.DEPTH^0.25), k = 10)",
                                                   "s(TEMPBOTM, k = 10)",
                                                   "s(TURBBOTM, k = 5)",
                                                   "s(OXYBOTM, k = 5)",
                                                   "s(SALBOTM, k = 5)",
                                                   "s(Dis.to.SHORE, k = 5)",
                                                   "s(YEAR, bs = 're', k = 18)"),
                                  names = c("Lat_Lon","Root_Dep", "Temp", "Turb", 
                                            "Oxy", "Sal", "Dis", "Yr"),
                                  response = "(cpe + 1)",
                                  gamma = 1.4,
                                  family = Gamma(link = log),
                                  in_parallel = TRUE)
)

save(bn_gam_strat_2, file = "output/output_GAM/bn_gam_strat_2.rda")


### Blacktip

bt.time.2 <- system.time(
  bt_gam_strat_2 <- gam_select_fn(data = bt_pos_catch,
                                  smooth_terms = c("s(STARTLON, STARTLAT, k = 60)",
                                                   "s(I(AVG.DEPTH^0.25), k = 10)",
                                                   "s(TEMPBOTM, k = 10)",
                                                   "s(TURBBOTM, k = 5)",
                                                   "s(OXYBOTM, k = 5)",
                                                   "s(SALBOTM, k = 5)",
                                                   "s(Dis.to.SHORE, k = 5)",
                                                   "s(YEAR, bs = 're', k = 18)"),
                                  names = c("Lat_Lon","Root_Dep", "Temp", "Turb", 
                                            "Oxy", "Sal", "Dis", "Yr"),
                                  response = "(cpe + 1)",
                                  gamma = 1.4,
                                  family = Gamma(link = log),
                                  in_parallel = TRUE)
)



save(bt_gam_strat_2, file = "output/output_GAM/bt_gam_strat_2.rda")


### Bull

bu.time.2 <- system.time(
  bu_gam_strat_2 <- gam_select_fn(data = bu_pos_catch,
                                  smooth_terms = c("s(STARTLON, STARTLAT, k = 60)",
                                                   "s(I(AVG.DEPTH^0.25), k = 10)",
                                                   "s(TEMPBOTM, k = 10)",
                                                   "s(TURBBOTM, k = 5)",
                                                   "s(OXYBOTM, k = 5)",
                                                   "s(SALBOTM, k = 5)",
                                                   "s(Dis.to.SHORE, k = 5)",
                                                   "s(YEAR, bs = 're', k = 18)"),
                                  names = c("Lat_Lon","Root_Dep", "Temp", "Turb", 
                                            "Oxy", "Sal", "Dis", "Yr"),
                                  response = "(cpe + 1)",
                                  gamma = 1.4,
                                  family = Gamma(link = log),
                                  in_parallel = TRUE)
)

save(bu_gam_strat_2, file = "output/output_GAM/bu_gam_strat_2.rda")


### Sandbar

sb.time.2 <- system.time(
  sb_gam_strat_2 <- gam_select_fn(data = sb_pos_catch,
                                  smooth_terms = c("s(STARTLON, STARTLAT, k = 60)",
                                                   "s(I(AVG.DEPTH^0.25), k = 10)",
                                                   "s(TEMPBOTM, k = 10)",
                                                   "s(TURBBOTM, k = 5)",
                                                   "s(OXYBOTM, k = 5)",
                                                   "s(SALBOTM, k = 5)",
                                                   "s(Dis.to.SHORE, k = 5)",
                                                   "s(YEAR, bs = 're', k = 18)"),
                                  names = c("Lat_Lon","Root_Dep", "Temp", "Turb", 
                                            "Oxy", "Sal", "Dis", "Yr"),
                                  response = "(cpe + 1)",
                                  gamma = 1.4,
                                  family = Gamma(link = log),
                                  in_parallel = TRUE)
)

save(sb_gam_strat_2, file = "output/output_GAM/sb_gam_strat_2.rda")


### sharpnose

sn.time.2 <- system.time(
  sn_gam_strat_2 <- gam_select_fn(data = sn_pos_catch,
                                  smooth_terms = c("s(STARTLON, STARTLAT, k = 60)",
                                                   "s(I(AVG.DEPTH^0.25), k = 10)",
                                                   "s(TEMPBOTM, k = 10)",
                                                   "s(TURBBOTM, k = 5)",
                                                   "s(OXYBOTM, k = 5)",
                                                   "s(SALBOTM, k = 5)",
                                                   "s(Dis.to.SHORE, k = 5)",
                                                   "s(YEAR, bs = 're', k = 18)"),
                                  names = c("Lat_Lon","Root_Dep", "Temp", "Turb", 
                                            "Oxy", "Sal", "Dis", "Yr"),
                                  response = "(cpe + 1)",
                                  gamma = 1.4,
                                  family = Gamma(link = log),
                                  in_parallel = TRUE)
)

save(sn_gam_strat_2, file = "output/output_GAM/sn_gam_strat_2.rda")


### silky

si.time.2 <- system.time(
  si_gam_strat_2 <- gam_select_fn(data = si_pos_catch,
                                  smooth_terms = c("s(STARTLON, STARTLAT, k = 60)",
                                                   "s(I(AVG.DEPTH^0.25), k = 10)",
                                                   "s(TEMPBOTM, k = 10)",
                                                   "s(TURBBOTM, k = 5)",
                                                   "s(OXYBOTM, k = 5)",
                                                   "s(SALBOTM, k = 5)",
                                                   "s(Dis.to.SHORE, k = 5)",
                                                   "s(YEAR, bs = 're', k = 18)"),
                                  names = c("Lat_Lon","Root_Dep", "Temp", "Turb", 
                                            "Oxy", "Sal", "Dis", "Yr"),
                                  response = "(cpe + 1)",
                                  gamma = 1.4,
                                  family = Gamma(link = log),
                                  in_parallel = TRUE)
)

save(si_gam_strat_2, file = "output/output_GAM/si_gam_strat_2.rda")


### spinner

sp.time.2 <- system.time(
  sp_gam_strat_2 <- gam_select_fn(data = sp_pos_catch,
                                  smooth_terms = c("s(STARTLON, STARTLAT, k = 60)",
                                                   "s(I(AVG.DEPTH^0.25), k = 10)",
                                                   "s(TEMPBOTM, k = 10)",
                                                   "s(TURBBOTM, k = 5)",
                                                   "s(OXYBOTM, k = 5)",
                                                   "s(SALBOTM, k = 5)",
                                                   "s(Dis.to.SHORE, k = 5)",
                                                   "s(YEAR, bs = 're', k = 18)"),
                                  names = c("Lat_Lon","Root_Dep", "Temp", "Turb", 
                                            "Oxy", "Sal", "Dis", "Yr"),
                                  response = "(cpe + 1)",
                                  gamma = 1.4,
                                  family = Gamma(link = log),
                                  in_parallel = TRUE)
) 

save(sp_gam_strat_2, file = "output/output_GAM/sp_gam_strat_2.rda")
### Failed

### tiger

ti.time.2 <- system.time(
  ti_gam_strat_2 <- gam_select_fn(data = ti_pos_catch,
                                  smooth_terms = c("s(STARTLON, STARTLAT, k = 60)",
                                                   "s(I(AVG.DEPTH^0.25), k = 10)",
                                                   "s(TEMPBOTM, k = 10)",
                                                   "s(TURBBOTM, k = 5)",
                                                   "s(OXYBOTM, k = 5)",
                                                   "s(SALBOTM, k = 5)",
                                                   "s(Dis.to.SHORE, k = 5)",
                                                   "s(YEAR, bs = 're', k = 18)"),
                                  names = c("Lat_Lon","Root_Dep", "Temp", "Turb", 
                                            "Oxy", "Sal", "Dis", "Yr"),
                                  response = "(cpe + 1)",
                                  gamma = 1.4,
                                  family = Gamma(link = log),
                                  in_parallel = TRUE)
)

save(ti_gam_strat_2, file = "output/output_GAM/ti_gam_strat_2.rda")




### initialize time matrix

time_list_2 <- list(bn.time.2 = bn.time.2,
                    bt.time.2 = bt.time.2,
                    bu.time.2 = bu.time.2,
                    sb.time.2 = sb.time.2,
                    sn.time.2 = sn.time.2,
                    si.time.2 = si.time.2,
                    sp.time.2 = sp.time.2,
                    ti.time.2 = ti.time.2)


save(time_list_2, file = "output/output_GAM/time_list_Strat-2.rda")


load(file = "output/output_GAM/time_list_Strat-2.rda")

time_list_2




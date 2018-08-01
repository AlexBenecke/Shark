### Strata 1 Presence Abscence

### Load function
source("5_functions/model_selection_fn.R")


### Load Data

bn_pres <- read.csv("data/clean-data/blacknose.csv") %>%
  mutate(YEAR = factor(YEAR))

str(bn_pres)


bt_pres <- read.csv("data/clean-data/blacktip.csv") %>%
  mutate(YEAR = factor(YEAR))

str(bt_pres)

bu_pres <- read.csv("data/clean-data/bull.csv") %>%
  mutate(YEAR = factor(YEAR))

str(bu_pres)

sb_pres <- read.csv("data/clean-data/sandbar.csv") %>%
  mutate(YEAR = factor(YEAR))

str(sb_pres)

sn_pres <- read.csv("data/clean-data/sharpnose.csv") %>%
  mutate(YEAR = factor(YEAR))

str(sn_pres)

si_pres <- read.csv("data/clean-data/silky.csv") %>%
  mutate(YEAR = factor(YEAR))

str(si_pres)

sp_pres <- read.csv("data/clean-data/spinner.csv") %>%
  mutate(YEAR = factor(YEAR))

str(sp_pres)

ti_pres <- read.csv("data/clean-data/tiger.csv") %>%
  mutate(YEAR = factor(YEAR))

str(ti_pres)

### Blacknose

bn.time.1 <- system.time(
  bn_gam_strat_1 <- gam_select_fn(data = bn_pres,
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
                                  response = "pres",
                                  gamma = 1.4,
                                  family = Gamma(link = log),
                                  in_parallel = FALSE)
)

save(bn_gam_strat_1, file = "output/output_GAM/bn_gam_strat_1.rda")


### Blacktip

bt.time.1 <- system.time(
  bt_gam_strat_1 <- gam_select_fn(data = bt_pres,
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
                                  response = "pres",
                                  gamma = 1.4,
                                  family = Gamma(link = log),
                                  in_parallel = TRUE)
)



save(bt_gam_strat_1, file = "output/output_GAM/bt_gam_strat_1.rda")


### Bull

bu.time.1 <- system.time(
  bu_gam_strat_1 <- gam_select_fn(data = bu_pres,
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
                                  response = "pres",
                                  gamma = 1.4,
                                  family = Gamma(link = log),
                                  in_parallel = TRUE)
)

save(bu_gam_strat_1, file = "output/output_GAM/bu_gam_strat_1.rda")


### Sandbar

sb.time.1 <- system.time(
  sb_gam_strat_1 <- gam_select_fn(data = sb_pres,
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
                                  response = "pres",
                                  gamma = 1.4,
                                  family = Gamma(link = log),
                                  in_parallel = TRUE)
)

save(sb_gam_strat_1, file = "output/output_GAM/sb_gam_strat_1.rda")


### sharpnose

sn.time.1 <- system.time(
  sn_gam_strat_1 <- gam_select_fn(data = sn_pres,
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
                                  response = "pres",
                                  gamma = 1.4,
                                  family = Gamma(link = log),
                                  in_parallel = TRUE)
)

save(sn_gam_strat_1, file = "output/output_GAM/sn_gam_strat_1.rda")


### silky

si.time.1 <- system.time(
  si_gam_strat_1 <- gam_select_fn(data = si_pres,
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
                                  response = "pres",
                                  gamma = 1.4,
                                  family = Gamma(link = log),
                                  in_parallel = TRUE)
)

save(si_gam_strat_1, file = "output/output_GAM/si_gam_strat_1.rda")


### spinner

sp.time.1 <- system.time(
  sp_gam_strat_1 <- gam_select_fn(data = sp_pres,
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
                                  response = "pres",
                                  gamma = 1.4,
                                  family = Gamma(link = log),
                                  in_parallel = TRUE)
) 

save(sp_gam_strat_1, file = "output/output_GAM/sp_gam_strat_1.rda")
### Failed

### tiger

ti.time.1 <- system.time(
  ti_gam_strat_1 <- gam_select_fn(data = ti_pres,
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
                                  response = "pres",
                                  gamma = 1.4,
                                  family = Gamma(link = log),
                                  in_parallel = TRUE)
)

save(ti_gam_strat_1, file = "output/output_GAM/ti_gam_strat_1.rda")




### initialize time matrix

time_list_1 <- list(bn.time.1 = bn.time.1,
                    bt.time.1 = bt.time.1,
                    bu.time.1 = bu.time.1,
                    sb.time.1 = sb.time.1,
                    sn.time.1 = sn.time.1,
                    si.time.1 = si.time.1,
                    sp.time.1 = sp.time.1,
                    ti.time.1 = ti.time.1)


save(time_list_1, file = "output/output_GAM/time_list_Strat-1.rda")


load(file = "output/output_GAM/time_list_Strat-1.rda")

time_list_1

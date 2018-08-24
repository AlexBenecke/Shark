### Comparing AIC values

### Load Packages
library(FSA)
library(tidyverse)
library(dplyr)
library(magrittr)
library(scales)
library(mgcv)
### Load Data
sharpnose <- read.csv("data/terraenovae.df.csv")
sharpnose$DATE <- as.POSIXlt(sharpnose$DATE)
sharpnose$MONTH  <- factor(strftime(sharpnose$DATE, format = "%b"))
sharpnose$YEAR <- factor(sharpnose$YEAR)


### Fit a simple GAM with default k values

AIC_1 <- gam((cpe + 1) ~ s(STARTLON, STARTLAT) +
               s(SALBOTM) +
               s(YEAR,
                 bs = "re"),
             family = Gamma(link=log),
             data = sharpnose)

par(mfrow=c(2,2))
gam.check(AIC_1)

summary(AIC_1)


### Now fit gam with elevated k-value

AIC_2 <- gam((cpe + 1) ~ s(STARTLON, STARTLAT, k = 120) +
               s(SALBOTM,
                 k = 60) +
               s(YEAR,
                 bs = "re",
                 k = 20),
             family = Gamma(link=log),
             data = sharpnose)

gam.check(AIC_2)

summary(AIC_2)



AIC(AIC_1, AIC_2)

AIC_1$aic
AIC_2$aic


### Looks like the higher the basis dimensions (k) the lower the AIC score. 
### This makes sense since the basis dimensions are factored into the total degrees of freedom which affects the AIC score.







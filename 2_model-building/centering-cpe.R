


blacknose <- read.csv("data/acronotus.df.csv") 
blacknose$DATE <- as.POSIXlt(blacknose$DATE)
blacknose$MONTH <- factor(strftime(blacknose$DATE, format = "%b"))
blacknose$YEAR <- factor(blacknose$YEAR)
tmp.bn <- blacknose[blacknose$cpe>0,]

str(blacknose)

sharpnose <- read.csv("data/terraenovae.df.csv")
sharpnose$DATE <- as.POSIXlt(sharpnose$DATE)
sharpnose$MONTH  <- factor(strftime(sharpnose$DATE, format = "%b"))
sharpnose$YEAR <- factor(sharpnose$YEAR)
tmp.sn <- sharpnose[sharpnose$cpe>0,]

str(sharpnose)



### Blacknose centered cpe


blacknose$cpe_center <- (blacknose$cpe - mean(blacknose$cpe))

blacknose$cpe_standard <- (blacknose$cpe - mean(blacknose$cpe))/sd(blacknose$cpe)

summary(blacknose$cpe)

hist(blacknose$cpe)

summary(blacknose$cpe_center)

hist(blacknose$cpe_center)


summary(blacknose$cpe_standard)

hist(blacknose$cpe_standard)



### sharpnose centered cpe


cpe_center <- (sharpnose$cpe[sharpnose$cpe>0] - mean(sharpnose$cpe[sharpnose$cpe>0]))

cpe_standard <- (sharpnose$cpe[sharpnose$cpe>0] - mean(sharpnose$cpe[sharpnose$cpe>0]))/sd(sharpnose$cpe[sharpnose$cpe>0])

summary(sharpnose$cpe)

length(sharpnose$cpe[sharpnose$cpe==0])

hist(sharpnose$cpe)


hist(sharpnose$cpe[sharpnose$cpe>0])





summary(cpe_center)

hist(cpe_center)

hist(log(cpe_center))


summary(cpe_standard)

hist(cpe_standard)




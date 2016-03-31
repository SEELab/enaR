### reading econet models from the econet website
### 1 mar 2016
### mklau

source('read.EcoNet.R')
library(enaR)


## Parse models

enaren.mods <- read.EcoNet(readLines('~/Projects/SEELab/enaRguide/data/oysterreef.net'))
econet.mods <- lapply(mods,read.EcoNet)

par(mfrow=c(2,4),mai=rep(0,4))
gplot(econet.mods[[1]],displaylabels=TRUE)
gplot(econet.mods[[2]],displaylabels=TRUE)
gplot(econet.mods[[3]],displaylabels=TRUE)
gplot(econet.mods[[4]],displaylabels=TRUE)
gplot(enaren.mods,displaylabels=TRUE)




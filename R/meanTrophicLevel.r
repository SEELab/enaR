#' Mean Trophic Levels
#' Stuart Borrett
#' Sept. 2017
#' ==========================

library(enaR)
library(network)

data(enaModels)
m = 12
x = enaModels[[m]]
names(enaModels)[[m]]


mtl <- function(x = 'model',minTL = 2){

    biomass = x%v%'storage'

    etl = enaTroAgg(x)$ETL

    k = which(etl >= minTL)

    mtl = sum(( etl[k] * biomass[k] ) / sum(biomass[k]))

    return(mtl)

}

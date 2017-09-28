#' meanTrophicLevel
#' INPUT = network data object, min trophic level choice
#' OUTPUT = mean trophic level
#'
#' ==============================
#' @param x a network object.  This includes all weighted flows into and out of
#' each node.  The model needs to be at steady-state
#' @param minTL is the minimum trophic level to be included in the calculation fo the mean trophic level.   The default is 2, but users often use 3.25 and 4.
#' @export meanTrophicLevel
#' @author Stuart R. Borrett
#' @examples
#'
#' data(enaModels)
#' meanTrophicLevel(enaModels[[12]], minTL = 3.25)
#' @import network


meanTrophicLevel <- function(x = 'model', minTL = 2){

    biomass = x%v%'storage'

    etl = enaTroAgg(x)$ETL

    k = which(etl >= minTL)

    mtl = sum(( etl[k] * biomass[k] ) / sum(biomass[k]))

    return(mtl)

}

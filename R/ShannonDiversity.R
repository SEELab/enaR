#' Shannon Diversity Metrics
#' These are based on entropy and build Shannon and Weaver 1949
#'
#' Borrett | November 29, 2016
#'
#' INPUT = Vector
#' Output = set of network statistics to charcterize the diversity in the vector
#' ================================================================================

ShannonDiversity <-  function(x){
    p <- x/sum(x)  # relative proportion
    H <- -1 * sum(p * log(p) )  # results in nats (using natural log)  # Shannon Diversity
    Hmax <- log(length(x)) # maximum possible Shannon Diversity
    Hr <- H/Hmax
    Hcentral <- 1-Hr
    effective.n <- exp(H)  # effecive number of elements
    n <- length(x)  # number of elements

    return(c("H"=H, "Hmax" = Hmax, "Hr" = Hr, "Hcentral" = Hcentral,
            "n" = n,
            "effective.n" = effective.n))
}

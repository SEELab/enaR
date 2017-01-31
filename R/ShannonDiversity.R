#' Shannon Diversity Metrics
#' These are based on entropy and build Shannon and Weaver 1949
#'
#' Borrett | November 29, 2016
#'
#' INPUT = Vector
#' Output = set of network statistics to charcterize the diversity in the vector
#' ================================================================================







#' Shannon Diversity Metrics These are based on entropy and build Shannon and
#' Weaver 1949
#' 
#' Borrett | November 29, 2016
#' 
#' INPUT = Vector Output = set of network statistics to charcterize the
#' diversity in the vector
#' ================================================================================
#' Shannon Diversity Metrics These are based on entropy and build Shannon and
#' Weaver 1949
#' 
#' Borrett | November 29, 2016
#' 
#' INPUT = Vector Output = set of network statistics to charcterize the
#' diversity in the vector
#' ================================================================================
#' Shannon information entropy
#' 
#' Calculates a number of metrics based on the Shannon information entropy
#' measure of diversity in a vector, x.
#' 
#' @param x a 1 x n vector.
#' @return \item{H}{Shannon entropy-based metric of diversity.  This captures
#' the effects of both richnes (the length of the vector, n) and the evenennes
#' of the distribution.} \item{Hmax}{The maximum possible value of H given a
#' vector of the length n provided.} \item{Hr}{Relative evenness Hr = H/Hmax}
#' \item{Hcentral}{The centralization or concentration of the values among the
#' n elements} \item{n}{Number of elements in the vector.}
#' \item{effective.n}{effective number of elements in the vector, given the
#' distribution of the relative weights.}
#' @note The formulation for Shannon Diversity uses a natural logarithm.  As
#' the natural logarithm of zero is undefined, the input vector cannot contain
#' zeros.  Analytically, there are two approaches to dealing with this issue if
#' your vector contains zeros.  First, you can apply the analysis to only the
#' non-zero elements.  Second, you can add a tiny amount to all of the elements
#' such that the zero elements are now very small numbers, relative the
#' original vector values.
#' @author Stuart R. Borrett
#' @examples
#' 
#' 
#' 
#' data(oyster)
#' 
#' #' throughflow diversity
#' T <- enaFlow(oyster)$T
#' ShannonDiversity(T)
#' 
#' #' storage (biomass) biodiversity
#' X <- oyster %v% "storage"
#' ShannonDiversity(X)
#' 
#' 
#' 
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

#' keystoneness -- from Libralato et al. 2006
#' INPUT = network object
#' OUTPUT = keystoneness vector
#' Stuart Borrett  |  Feb. 15, 2017
#' ---------------------

#' keystoneness  INPUT = netowrk object OUTPUT = keystoneness vector
#'
#' Calculates the level of keystoneness for each species (node) in the
#' network following Libralato et al. 2006. The assumption is that the
#' input network model represents a food web.
#'
#' @param x a network object.
#' @return \item{ks}{vector of the keystoneness values}
#' @author Stuart R. Borrett
#' @seealso \code{\link{enaMTI}}
#' @references Libralato, S, Christensne, V., Pauly, D. 2006. A method for
#' identifying keystone speices in food web models. Ecol
#' Model. 195:153-171.
#'
#' @examples
#'
#' data(troModel)
#' ks <- keystoneness(troModels[[38]])
#' show(ks)
#'
#' @import network
#' @export keystoneness
keystoneness <- function(x = "model"){
  if (class(x) != 'network'){warning('x is not a network class object')}
                                        #Check for balancing
    if (any(list.network.attributes(x) == 'balanced') == FALSE){x%n%'balanced' <- ssCheck(x)}
    if (x%n%'balanced' == FALSE){warning('Model is not balanced'); stop}

    m <- enaMTI(x, eigen.check=FALSE)$M  # perform Mixed Trophic Impacts analysis
    m <- m - diag(diag(m))
    e <- sqrt(apply(m^2, 1, sum))
    b <- x%v%'storage'
    p <- b/sum(b)
    ks <- log( e * (1-p))
  return(ks)
}

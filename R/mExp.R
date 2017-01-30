#'# mExp --- calculate the exponent of a given matrix
#'# INPUT = a matrix (x) and the exponent (n)
#'# OUTPUT = the resulting exponentiated matrix
#'# 
#'# Alberto Monteiro (https://stat.ethz.ch/pipermail/
#'# r-help/2007-May/131330.html)
#'# ___________________________________________________



#' Calculates the Exponent of a Matrix
#' 
#' Function for calculating the pathway proliferation of flows in a network
#' model through matrix exponentiation.
#' 
#' 
#' @param x A matrix.
#' @param n Desired exponent (i.e. the path length).
#' @return Returns an exponentiated flow matrix.
#' @author Alberto Monteiro
#' (https://stat.ethz.ch/pipermail/r-help/2007-May/131330.html) Matthew K. Lau
#' @seealso \code{\link{findPathLength}}
#' @references This function was originally designed by Alberto Monteiro in the
#' following R help thread:
#' https://stat.ethz.ch/pipermail/r-help/2007-May/131330.html.
#' @export mExp
mExp <- function(x='matrix', n=2){
  if (n == 1) return(x)
  result <- diag(1, ncol(x))
  while (n > 0) {
    if (n %% 2 != 0) {
      result <- result %*% x
      n <- n - 1
    }
    x <- x %*% x
    n <- n / 2
  }

  rownames(result) <- colnames(result)

  return(result)

}

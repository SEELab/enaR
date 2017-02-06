#' Calculates the Ratio of Positive to Negative Elements in a Network
#' 
#' Dependent function for the enaUtility function.
#' 
#' 
#' @param x A matrix of flow values.
#' @return Returns the ratio of positive to negative elements in the flow
#' matrix.
#' @author Stuart R. Borrett
#' @seealso \code{\link{enaUtility}}
#' @references Fath, B.D. and S.R. Borrett. 2006. A MATLAB function for network
#' environ analysis. Environmental Modelling & Software 21:375-405.
bcratio <- function(x='matrix'){
	r=sum(x[x>0])/sum(abs(x[x<0]))  # creates the ratio of positive elements to negative elements.
  return(r)
}

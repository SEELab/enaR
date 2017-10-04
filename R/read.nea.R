#' Read an NEA formatted model into a network object
#' 
#' This function reads in and creates a network object from a NEA formatted
#' data file (Fath and Borrett 2006).
#' 
#' @param file The name and path for the data file.
#' @param sep The separation character used to delimit data values.
#' @param warn LOGICAL: should pack warnings be reported?
#' @return Returns the network object.
#' @author Stuart R. Borrett
#' @seealso \code{\link{write.nea}}
#' @references Fath, B. D., Borrett, S. R. 2006. A Matlab function for
#' Network Environ Analysis.  Environ. Model. Softw. 21, 375-405.
#' @importFrom utils read.table
#' @export read.nea
read.nea <- function(file="file name",sep=',',warn=TRUE){
  dat <- read.table(file,header=FALSE,sep=sep)  # assumes 
  n <- max(dim(dat)) - 2
  Flow <- t(dat[1:n,1:n])   # NEA.m stores flows col to row, so here we transpose
  z <- dat[1:n,(n+1)]  # inputs
  y <- dat[(n+1),1:n]  # outputs
  X <- dat[1:n,(n+2)]  # storage
  if (warn){
    model <- pack(flow=Flow,input=z,respiration=y,storage=X)  # create network data object
  }else{
    suppressWarnings(model <- pack(flow=Flow,input=z,respiration=y,storage=X))   # create network data object
  }
  return(model)
}

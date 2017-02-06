#' R function to read in a matrix formatted as Mdloti (Ursula Sharler)
#' Borrett | Sept. 12, 2012, MKL July 2013
#' Updated - Borrett, May 2016 - to use pack() to create the network data object.
#' ------------------------







#' R function to read in a matrix formatted as Mdloti (Ursula Sharler) Borrett
#' | Sept. 12, 2012, MKL July 2013 Updated - Borrett, May 2016 - to use pack()
#' to create the network data object. ------------------------ R function to
#' read in a matrix formatted as Mdloti (Ursula Sharler) Borrett | Sept. 12,
#' 2012, MKL July 2013 Updated - Borrett, May 2016 - to use pack() to create
#' the network data object. ------------------------ Read ENA Model from an
#' Mdloti Formatted Excel File
#' 
#' This function reads network data from an excel file commonly used by Ursula
#' Sharler.  The file has three header lines (name/source, number of
#' compartments, number of living nodes) and then a n+2 x n+2 matrix of flows.
#' This is the flow matrix with an additional row for imports and biomass each
#' and additional columns for exports and respirations.
#' 
#' 
#' @param file The name and path for the data file.  This function assumes the
#' data are stored on the first sheet of an Microsoft Excel formatted. NOTE:
#' this function depends on the read.xlsx function from the xlsx package, which
#' requires that the entire path be specified from the root directory (i.e. the
#' absolute path).
#' @return Returns the network object.
#' @author Stuart R. Borrett
#' @seealso \code{\link{read.scor}}
#' @references Fath, B. D., Borrett, S. R. 2006. A Matlab function for Network
#' Environ Analysis.  Environ. Model. Softw. 21, 375-405.
#' @export read.enam
read.enam<- function(file="file path and name"){
                                        #I have assumed the file is formatted as an excel speadsheet.
                                        #The data must be on the first sheet in the workbook.
  x <- as.matrix(gdata::read.xls(file,sheet=1,header=FALSE))
  mname <- as.character(x[1,1]); # Get Model ID
  n <- as.numeric(as.character(x[2,2])) # number of nodes
  liv <- as.numeric(as.character(x[3,2])) # number of nodes
  a <- n+6+1 # ending row of flows matrix -- assumes Flows start on row 6 and Imports and Biomasses are at the end
  b <- n+2+2 # ending column of flows matrix -- assumes exports and respirations are at the end
  m <- x[6:a,3:b] # Matrix of Flows
  m <- apply(m,2,as.numeric)
  rownames(m) <- colnames(m) <- as.character(x[6:a,2]) # node names
  m[is.na(m)] <- 0 # replace NAs with zeros
  Flow <- m[1:n,1:n] # flow matrix
  imports <- m[(n+1),1:n]
  biomass <- as.numeric(unlist(m[(n+2),1:n]))
  exports <- as.numeric(unlist(m[1:n,(n+1)]))
  respiration <- as.numeric(unlist(m[1:n,(n+2)]))
  LIV <-  c(rep(TRUE,liv),rep(FALSE,n-liv))
                                       # packing up the attributes into the network object (y)
  y <- pack(flow = Flow, input = imports, respiration = respiration, export = exports, storage = biomass, living = LIV)
  return(y)
}

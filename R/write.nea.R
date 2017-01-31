#'' write.nea.R
#' INPUT = enaR network data object
#' Ouput = CSV formatted file with data arranged as expected input for NEA.m
#'
#' Borrett | July 15, 2013
#' ----------------------------------------







#' ' write.nea.R INPUT = enaR network data object Ouput = CSV formatted file
#' with data arranged as expected input for NEA.m
#' 
#' Borrett | July 15, 2013 ---------------------------------------- '
#' write.nea.R INPUT = enaR network data object Ouput = CSV formatted file with
#' data arranged as expected input for NEA.m
#' 
#' Borrett | July 15, 2013 ---------------------------------------- Write a
#' Network Object to File Using the NEA Data Format
#' 
#' This function writes a network object to a NEA formatted data file (Fath and
#' Borrett 2006).
#' 
#' @param x Network object.
#' @param file.name The file name or path. If a simple file name is given, this
#' function uses the current working directory by default.
#' @param sep The separation character used to delimit data values.
#' @return Writes a network object to a NEA formatted file and returns the
#' output composite matrix.
#' @author Stuart R. Borrett
#' @seealso \code{\link{read.nea}}
#' @references Fath, B. D., Borrett, S. R. 2006. A Matlab function for Network
#' Environ Analysis.  Environ. Model. Softw. 21, 375-405.
#' @export write.nea
write.nea <- function(x, file.name,sep=','){
                                        # Check for network class
  if (class(x) != 'network'){warning('x is not a network class object')}
  U <- unpack(x)  # unpack data
  n <- length(U$z)
  S <- matrix(NA,nrow=(n+1),ncol=(n+2))
  S[1:n,1:n] = t(U$F)
  S[1:n,(n+1)]= U$z
  S[(n+1),1:n] = U$y
  S[1:n,(n+2)]= U$X
  S[(n+1),(n+1):(n+2)] = 0
                                        # write file
  write.table(S,file=file.name,row.names=FALSE,col.names=FALSE,sep=sep) 
                                        # return composite system matrix to workspace 
  return(S)
}

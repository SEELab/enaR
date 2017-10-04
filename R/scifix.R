#' scifix Corrects missing e or E in scientific notation
#' 
#' This is a support function that corrects the scientific notation in SCOR
#' formatted data files.
#' 
#' @param x A numeric or character scalar.
#' @return Returns a numeric scalar in appropriate scientific notation.
#' @author Matthew K. Lau
#' @seealso \code{\link{read.scor}}
scifix <- function(x){
  x <- as.character(x)
                                        #e/E check
  e.check <- grepl('e',x)|grepl('E',x)
                                        #+/- check
  pm.check <- grepl('\\+',x)|grepl('\\-',x)
  if (pm.check&e.check==FALSE){
    if (grepl('\\+',x)){
      x <- sub('\\+','E+',x)
    }else{
      x <- sub('\\-','E-',x)
    }
  }else{}
  return(as.numeric(x))
}

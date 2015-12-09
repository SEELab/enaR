\name{read.nea}
\alias{read.nea}
\title{
  Read NEA Formatted Network Model
}
\description{
  This function reads in and creates a network object from a NEA formatted
  data file (Fath and Borrett 2006). 
}
\usage{
read.nea(file="file name",sep=',',warn=TRUE)
}
\arguments{
  \item{file}{
    The name and path for the data file.
  }
  \item{sep}{
    The separation character used to delimit data values.
  }
  \item{warn}{
    LOGICAL: should pack warnings be reported?
  }
}
\value{
  Returns the network object.
}
\references{
  Fath, B. D., Borrett, S. R. 2006. A Matlab function for Network
  Environ Analysis.  Environ. Model. Softw. 21, 375-405.
}
\author{
  Stuart R. Borrett 
}


\seealso{
  \code{\link{write.nea}}
}

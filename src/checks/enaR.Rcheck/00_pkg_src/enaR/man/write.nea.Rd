\name{write.nea}
\alias{write.nea}
\title{
  Write a network object to file using the NEA data format.
}
\description{
  This function writes a network object to a NEA formatted
  data file (Fath and Borrett 2006). 
}
\usage{
write.nea(x, file.name='neaData.csv',sep=',')
}
\arguments{
  \item{x}{
    Network object.
  }
  \item{file.name}{
    The file name or path. If a simple file name is given, this function
    uses the current working directory by default.
  }
  \item{sep}{
    The separation character used to delimit data values.
  }
}
\value{
  Writes a network boject to a NEA formatted file and returns the output
  composite matrix.
}
\references{
  Fath, B. D., Borrett, S. R. 2006. A Matlab function for Network
  Environ Analysis.  Environ. Model. Softw. 21, 375-405.
}
\author{
  Stuart R. Borrett (borretts@uncw.edu)
}


\seealso{
  \code{\link{read.nea}}
}

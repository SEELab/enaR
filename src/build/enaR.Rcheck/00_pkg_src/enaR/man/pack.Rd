\name{pack}
\alias{pack}
\title{
  Compile network information into a network class.
}
\description{
  This function provides a flexible framework for importing flow network
  information into a network class object for analyses.
}
\usage{
pack(flow,input=NA,respiration=NA,export=NA,storage=NA,living=NA)
}
\arguments{
  \item{flow}{
    The flow matrix.
  }
  \item{input}{
    The inputs into the system.
  }
  \item{export}{
    The exports from the system. 
  }
  \item{respiration}{
    The quantities respired from the system. 
  }
  \item{storage}{
    The quantities stored in compartments within the system. 
  }
  \item{living}{
    A logical vector indicating whether a node is either 'living' (=
    TRUE) or 'dead' (=FALSE).
  }
}

\value{
  Returns a network object for the supplied model.
}

\author{
Matthew K. Lau (mkl48@nau.edu)
Stuart R. Borrett (borretts@uncw.edu)
}

\seealso{
\code{\link{unpack}}
}


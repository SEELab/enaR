\name{write.EcoNet}
\alias{write.EcoPath}
\title{
  Creates an EcoPath model from an enaR network object.
}
\description{
  
}
\usage{
write.EcoPath(x='model',file='file path',mn='ena_model')
}
\arguments{
  \item{x}{
    Network object.
  }
  \item{file}{
    The file name or path. If a simple file name is given, this function
    uses the current working directory by default.
  }
  \item{mn}{
    The model name that EcoPath will use. The DEFAULT is 'ena_model'.
  }
}
\value{
  An EcoPath formatted text file is created from the model, which can be
  input at http://eco.engr.uga.edu.
}
\references{
  
}
\author{
  Matthew K. Lau (enaR.maintainer@gmail.com)
}


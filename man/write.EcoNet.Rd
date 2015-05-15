\name{write.EcoNet}
\alias{write.EcoNet}
\title{
  Creates an EcoNet model from an enaR network object.
}
\description{
  
}
\usage{
write.EcoNet(x='model',file='file path',mn='ena_model')
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
    The model name that EcoNet will use. The DEFAULT is 'ena_model'.
  }
}
\value{
  An EcoNet formatted text file is created from the model, which can be
  input at http://eco.engr.uga.edu.
}
\references{
  About EcoNet (http://eco.engr.uga.edu/DOC/econet1.html)
  
  Kazanci, C. 2009. Handbook of Ecological Modelling and Informatics, by WIT Press.
}
\author{
  Matthew K. Lau (enaR.maintainer@gmail.com)
}


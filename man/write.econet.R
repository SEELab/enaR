\name{write.econet}
\alias{write.econet}
\title{
  Write out a network object as an EcoNet model.
}
\description{
  Write a network object in the format used in the EcoNet modeling software.
}
\usage{
write.econet(x,mn='model name')
}
\arguments{
  \item{x}{
    Network object.
  }
  \item{mn}{
   Model name
  }
}
\value{
  Writes out an EcoNet model file using the model name. 
  Names of nodes are adjusted in order to avoid conflicts with special characters.
}
\references{
  EcoPath can be found at: http://eco.engr.uga.edu/index.html
}
\author{
  Matthew K. Lau (enar.maintainer@gmail.com)
}

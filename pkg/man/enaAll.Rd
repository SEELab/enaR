\name{enaAll}
\alias{enaAll}
\title{
  Conduct all major ENA.
}
\description{
  Conducts all major ENA with default settings and returns the output as a
  named list.
}

\usage{
enaAll(x = "network object")
}

\arguments{
\item{x}{A network object.}
}


\value{
\item{ascendency}{enaAscendency}
\item{control}{enaControl}
\item{environ}{enaEnviron}
\item{flow}{enaFlow}
\item{mti}{enaMTI}
\item{storage}{enaStorage}
\item{structure}{enaStructure}
\item{utility}{enaUtility with eigen.check=FALSE}
}


\author{
  Matthew K. Lau (mkl48@nau.edu)
  Stuart R. Borrett (borretts@uncw.edu)
}


\seealso{
   \code{\link{enaAscendency},\link{enaControl},\link{enaEnviron},\link{enaFlow},\link{enaMTI},\link{enaStorage},\link{enaUtility}}
}

\examples{
data(troModels)
output = enaAll(troModels[[6]])
names(output)
}

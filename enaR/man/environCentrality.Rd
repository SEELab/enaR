\name{environCentrality}
\alias{environCentrality}
\title{
  Calculates the environ centrality of the nodes in an ecological network.
}
\description{
  This function calculates the input, output, and average environ
  centrality of the nodes in the network (Fath and Borret, 2012).  This
  is a type of weighted degree centrality that indicates the relative importance
  of the nodes in the flow activity in the network.
}
\usage{
environCentrality(x='matrix')
}

\arguments{
  \item{x}{A square matrix.  Usually the integral flow marix from
    enaFlow.  The assumption is that the flows are oriented column to row.
  }
}

\value{
\item{ECin}{input oriented environ centrality}
\item{ECout}{output oriented environ centraility}
\item{AEC}{average environ centrality (average of input and output)}
}
\references{
  Fann, S.L. and Borrett, S.R. 2012. Environ centrality reveals the
  tendency of indirect effects to homogenize the functional importance
  of species in ecosystems.  Journal of Theoretical Biology 294: 74-86.
}
\author{
  Matthew K. Lau (mkl48@nau.edu)
  Stuart R. Borrett (borretts@uncw.edu)
}

\seealso{
  \code{\link{enaFlow}}
}
\examples{
data(troModels) 
F<-enaFlow(troModels[[6]])
ec<-environCentrality(F$N)
attributes(ec)
barplot(sort(ec$AEC,decreasing=TRUE),col=4,
   ylab="Average Environ Centrality",
   ylim=c(0,0.4))
}


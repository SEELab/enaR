\name{structure.statistics}
\alias{structure.statistics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Calculates the structural statistics (network statistics) of an ecological network.
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
  This function returns several network statistics that describe a
  network.  
}
\usage{
structure.statistics(A = "matrix")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
%%     ~~Describe \code{A} here~~
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use

%%  \item{comp2 }{Description of 'comp2'}

\item{n}{Number of nodes in A}
\item{L}{Number of direct connections in A}
\item{C}{Connectivity of A}
\item{LD}{Link density}
\item{lam1A}{First dominant eigenvalue of A}
\item{mlam1A}{Multiplicity of the dominant eigenvalue}
\item{lam2A}{Magnitude of the second largest eigenvalue}
\item{rho}{Damping ratio (see Caswell 2001)}
\item{R}{Distance of lam1A from the bulk of the eigen spectrum}
\item{d}{Difference between the dominant eigenvalue and the link density}
\item{no.scc}{Number of strongly connected components}
\item{no.scc.big}{Number of strongly connected components greater than 1}
\item{pscc}{Precent of nodes participating in a strongly connected component}

}
\references{
  Fath, B. D., Borrett, S. R. 2006. A Matlab function for Network
  Environ Analysis.  Environ. Model. Softw. 21, 375-405.

}
\author{
  Matthew K. Lau (mkl48@nau.edu)
  Stuart R. Borrett (borretts@uncw.edu)
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{enaStructure}},\code{\link{scc}}
}
% \examples{
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\name{environCentrality}
\alias{environCentrality}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Calculates the environ centrality of the nodes in an ecological network.
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
  This function calculates the input, output, and average environ
  centrality of the nodes in the network (Fath and Borret, 2012).  This
  is a type of weighted degree centrality that indicates the relative importance
  of the nodes in the flow activity in the network.
}
\usage{
environCentrality(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A square matrix.  Usually the integral flow marix from
    enaFlow.  The assumption is that the flows are oriented column to row.
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{ECin}{input oriented environ centrality}
\item{ECout}{output oriented environ centraility}
\item{AEC}{average environ centrality (average of input and output)}
%% ...
}
\references{
  %% ~put references to the literature/web site here ~
  Fann, S.L. and Borrett, S.R. 2012. Environ centrality reveals the
  tendency of indirect effects to homogenize the functional importance
  of species in ecosystems.  Journal of Theoretical Biology 294: 74-86.
}
\author{
  %%  ~~who you are~~
  Matthew K. Lau (mkl48@nau.edu)
  Stuart R. Borrett (borretts@uncw.edu)
}
\note{
  %%  ~~further notes~~
  Assumes the matrix is oriented from columns to row.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as
  \code{\link{enaFlow}}
}
\examples{
data(oyster)   
F<-enaFlow(oyster)
ec<-environCentrality(F$N)
attributes(ec)
barplot(sort(ec$AEC,decreasing=TRUE),col=4,
   ylab="Average Environ Centrality",
   ylim=c(0,0.4))
  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{unpack}
\alias{unpack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
"Unpacks" the components of a network object into separate objects.
}
\description{
"Unpacks" the components of a network object into separate objects.
This includes inputs, exports, respirations, outputs (exports +
respirations), storage, and internal flows.
   }
\usage{
unpack(x = "network object")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A  network object.  This includes all weighted flows into and out of
    each node. }
}

\value{
  \item{F}{matrix of flows from each node to each node oreinted row to column.}
  \item{z}{Node boundary inputs.}
  \item{r}{Node boundary loss from respiration.}
  \item{e}{Node boundary loss due to exportation}
  \item{y}{Node boundary loss; summation of r and e}
  \item{X}{Node storage or biomass}
  \item{Living}{Logical vector indicating whether each node is living or not}
}

\author{
%%  ~~who you are~~
  Matthew K. Lau (mkl48@nau.edu)
  Stuart R. Borrett (borretts@uncw.edu)}
\note{
  %%  ~~further notes~~
  Flows are oriented from row to column.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as
  \code{\link{pack},\link{read.scor}}
}
\examples{
data(oyster)
unpack(oyster)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

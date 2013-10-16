\name{scc.statistics}
\alias{scc.statistics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Structural statistics of the strongly connected component.
}
\description{
Calculates the structural statistics of the strongly connected component
in an ecological network.
}
\usage{
scc.statistics(A = "adjacency")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{Adjacency matrix}
}

\value{
  Returns a list of the structural network statistics for each large
  (n>1) strongly connected component (SCC) in the network represetned by
  the given djacency matrix.  These include: the number of nodes ("n");
  the number of links ("L"); the connectance ("C"), dominant eigenvlaue
  of the component ("lam1A"); the multiplicity of the dominant
  eigenvalue ("mlam1A"); the second largest eivenvalue ("lam2A"); the
  damping ratio (lam1A/lam2A, "rho"); distance of lam1[A] from the bulk
  of the eigen spectrum ("R"), difference between dominant eigenvalue
  and link density ("d"), number of strongly connected components (SCC; "no.scc"),
  number of SCCs with more than one node
  ("no.scc.big"), fraction of nodes participating in a SCC ("pscc").  
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
  %%  ~~who you are~~
  Matthew K. Lau (mkl48@nau.edu)
  Stuart R. Borrett (borretts@uncw.edu)
}
\note{
  %%  ~~further notes~~
  The adjacency matrix is oriented from columns to rows, though using
  the transpose should not change the results.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as
  \code{\link{enaStructure},\link{structure.statistics},\link{scc}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

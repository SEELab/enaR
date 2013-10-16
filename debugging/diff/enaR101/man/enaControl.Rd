\name{enaControl}
\alias{enaControl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Control analyses of ecological networks.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Analyses for analyzing the control of ecological networks.
}
\usage{
enaControl(x = "network object", zero.na = TRUE, balance.override = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A network object.
}
  \item{zero.na}{
Makes undefined (NA) values zero.
}
  \item{balance.override}{
%%     ~~Describe \code{balance.override} here~~
Turns off balancing and checks of network balance.
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{CN}{Control matrix using flow values.}
\item{CQ}{Control matrix using storage values.}

}
\references{
  Fath, B. D., Borrett, S. R. 2006. A MATLAB function for Network Environ
  Analysis.  Environmental Modelling & Software 21:375-405

}
\author{
  Matthew K. Lau (mkl48@nau.edu)
  Stuart R. Borrett (borretts@uncw.edu)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{enaStorage}}
}
\examples{
data(oyster)
enaControl(oyster)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

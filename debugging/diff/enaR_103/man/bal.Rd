\name{bal}
\alias{bal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Subfunction for balancing by either inputs or outputs.
}
\description{
Dependency for the \code{balance} function.
}
\usage{
bal(x = "extended matrix", method = c("input", "output"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
Extended flow matrix.
}
  \item{method}{
%%     ~~Describe \code{method} here~~
Balance by inputs or outputs.
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns an extended matrix for balancing by inputs or outputs.
}
\references{
%% ~put references to the literature/web site here ~
Fath, B.D. and S.R. Borrett. 2006. A MATLAB function for network
environ analysis. Environmental Modelling & Software 21:375-405.
}
\author{
Matthew K. Lau (mkl48@nau.edu)
Stuart R. Borrett (borretts@uncw.edu)
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{balance}}
}


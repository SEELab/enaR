\name{read.scor}
\alias{read.scor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Read SCOR formatted data files.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Read in network model data files that are in the SCOR format (REFERENCE).
}
\usage{
read.scor(file, type = c("network", "nea", "list", "edge.list"), from.file = TRUE, warn = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
File path or plain text.
}
  \item{type}{
%%     ~~Describe \code{type} here~~
Specifies the format of the object created.
}
  \item{from.file}{
%%     ~~Describe \code{from.file} here~~
States whether the file argument input should be treated as a file path (TRUE)
or plain text (FALSE).
}
  \item{warn}{
%%     ~~Describe \code{warn} here~~
Turn on (TRUE) or off (FALSE) warnings.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The SCOR file must be formatted properly. In particular, the number of
  nodes on the second line must have the first three characters
  dedicated to the total number of nodes and the next three characters
  should contain the number of living nodes. That is, the second line of
  the file should be formatted as 'xxxyyy' where x and y are the
  characters for the total number of nodes and the number of living
  nodes, respectively. Thus, if the total number of nodes is 10 and the
  number of living nodes is 1, then the second line should read, " 10
  1."
}
\value{
Returns a network object.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
% \references{

% }

\author{
%%  ~~who you are~~
  Matthew K. Lau (mkl48@nau.edu)
  Stuart R. Borrett (borretts@uncw.edu)
}


%% ~Make other sections like Warning with \section{Warning }{....} ~



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

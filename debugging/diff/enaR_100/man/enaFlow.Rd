\name{enaFlow}
\alias{enaFlow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Flow analyses of ecological networks.
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
  enaFlow performs the primary throughflow analysis developed for
  input-output systems.  It returns a vector of throughflows, the input and
  output oriented matrices for "direct flow intensities" and "integral
  flow intensities", and a set of flow based network statistics.
}
\usage{
enaFlow(x = "network object", zero.na = TRUE, balance.override = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a network object.  This includes all weighted flows into and out of
    each node.    
}
  \item{zero.na}{
    %%     ~~Describe \code{zero.na} here~~
    
}
\item{balance.override}{
  enaFlow analysis assumes the network model is at steady-state (inputs
  = outputs).  Setting balance.override = TRUE allows the function to be
  run on unbalanced models.

}
}
\details{
%%  ~~ If necessary, more details than the description above ~~

}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{T}{vector of node throughflows - total amount of energy-matter
  flowing into or out of each node}
\item{G}{matrix of the output oriented direct flow intensities}
\item{GP}{matrix of the input oriented direct flow intensities}
\item{N}{matrix of the ouput oriented integral
  (boundary+direct+indirect) flow intensities}
\item{NP}{matrix of the input oriented integral flow intensities}
\item{ns}{vector of flow based network statistics.  These include
  "Boundary" the total input into or output from the system, "TST" the
  total system throughflow, "TSTp" total system throughPUT,"AGG" is the network aggradation
  TST/Boundary which is also called average path length, "BFI" is the
  boundary flow intensity Boundary/TST, "DFI" is the direct flow
  intensity Direct/TST, "IFI" is the indirect flow intensity
  Indirect/TST, "id" is the realized indirect to direct flow intensity,
  "id.I" is the input idealized indirect flow intensity, "id.O"is the
  output idealized indirect flow intensity, "HMG.I" is the input network
  homogenization, "HMG.O" is the output network homogenization,
  "AMP.strong.I" is the strong measure of input network amplifiation,
  "AMP.strong.O" is the strong measure of output network amplification,
  "mode0" is the boundary flow - flow that reaches a compartment from
  across the system boundary, "mode1" is internal first passage flow,
  "mode2" is cycled flow, "mode3" is the dissipative eqivalent to mode2,
  and "mode4" is the dissipative equivalent ot mode0.}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
  Borrett, S. R., Freeze, M. A., 2011. Reconnecting environs to their
  environment. Ecol. Model. 222, 2393-2403.

  Fath, B. D., Borrett, S. R. 2006. A Matlab function for Network
  Environ Analysis.  Environ. Model. Softw. 21, 375-405.

  Fath, B. D., Patten, B. C., 1999. Review of the foundations of network
  environ analysis. Ecosystems 2, 167-179.
  
  Finn, J. T., 1976. Measures of ecosystem structure and function
  derived from analysis of flows. J. Theor. Biol. 56, 363-380.

  Patten, B.C. Higashi, M., Burns, T. P. 1990. Trophic dynamics in
  ecosystem networks: significance of cycles and storage.
  Ecol. Model. 51, 1-28.

  Schramski, J. R., Kazanci, C., Tollner, E. W., 2011. Network environ
  theory, simulation and EcoNet 2.0. Environ. Model. Softw. 26,
  419-428.
}
\author{
  Matthew K. Lau (mkl48@nau.edu)
  Stuart R. Borrett (borretts@uncw.edu)
}
\note{
  %%  ~~further notes~~
  Note that matrices returned are oriented from column to row, which is
  consistant with previous Environ Analysis works.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
  \code{\link{read.scor},\link{read.wand},\link{enaStorage},\link{enaUtility}}
}
\examples{
data(oyster)
F = enaFlow(oyster)  # completes the full analysis
ns = enaFlow(oyster)$ns  # returns just the network statisics

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

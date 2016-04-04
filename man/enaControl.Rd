\name{enaControl}
\alias{enaControl}
\title{
  Control Analyses of Ecological Networks
}
\description{
  Analyses for analyzing the control amongst the nodes in ecological networks.
}
\usage{
enaControl(x, zero.na = TRUE, balance.override = FALSE)
}
\arguments{
  \item{x}{
    A network object.
  }
  \item{zero.na}{
    Makes undefined (NA) values zero.
  }
  \item{balance.override}{
    Turns off balancing and checks of network balance.
  }

}

\value{
\item{CN}{Control matrix using flow values.}
\item{CQ}{Control matrix using storage values.}
\item{CR}{Schramski Control Ratio Matrix}
\item{CD}{Schramski Control Difference Matrix}
\item{sc}{Schramski System Control vector}
\item{scp}{Schramski system control vector as percent of total control}
\item{ns}{vector of network-level summary statistics}
}
\references{
  Fath, B. D., Borrett, S. R. 2006. A MATLAB function for Network Environ
  Analysis.  Environmental Modelling & Software 21:375-405

  Schramski, J.R., Gattie, D.K., Patten, B.C., Borrett S.R., Fath, B.D.,
  Thomas, C.R., and Whipple, S.J. 2006. Indirect effects and distributed
  control in ecosystems: Distributed control in the environ networks of
  a seven compartment model of nitrogen flow in the Neuse River Estuary,
  USA Steady-state analysis. Ecological Modelling 194:189-201

  Schramski, J.R., Gattie, D.K., Patten, B.C., Borrett S.R., Fath, B.D.,
  and Whipple, S.J. 2007. Indirect effects and distributed control in
  ecosystems: Distributed control in the environ networks of a seven
  compartment model of nitrogen flow in the Neuse River Estuary, USA
  Time series analysis. Ecological Modelling 206:18-30

}
\author{
  Matthew K. Lau
  Stuart R. Borrett
  Pawandeep Singh
}

\seealso{
  \code{\link{enaStorage}}
}
\examples{
data(troModels)
enaControl(troModels[[6]])
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

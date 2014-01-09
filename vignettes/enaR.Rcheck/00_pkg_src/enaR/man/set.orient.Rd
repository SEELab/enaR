\name{set.orient}
\alias{set.orient}
\title{
  Globally set the output matrix orientation.
}
\description{
  Changes the orientation of output matrices.
}
\usage{
set.orient(x = c('rc','school'))
}
\arguments{
  \item{x}{
    Orientation setting. If "rc" (DEFAULT), all matrix output will be returned in
    row (=input) to column (=output) orientation, regardless of school. If "school", then
    output matrices from functions from particular ENA schools will be
    oriented as expected in that school (i.e. Patten = column-row or
    Ulanowicz = row-column). Note, that all functions in the enaR package
    expect input matrices to be oriented row-column.
  }
  
}
\details{
  The enaR package as a
  whole, and the broader network analysis community, assumes a row to
  column orientation; thus, the default orientation for the package is
  row to column (DEFAULT = 'rc'). However, functions from the Patten school were
  orignially developed to conduct calculations and produce output in the
  column to row orientation. In order to facilitate the use of these
  functions, we also provide the option for users to return output in
  the orientation of the "school" (i.e. Patten results will be column to
  row oriented) by setting the global orientation to "school" using this fuction.
}

% \references{
% %% ~put references to the literature/web site here ~
% Allesina, S., Bondavalli, C., 2003. Steady state of ecosystem flow
% networks: a comparison between balancing procedures.Ecological Modelling 165(2-3):231-239.
% }

\author{
Matthew K. Lau (mkl48@nau.edu)
Stuart R. Borrett (borretts@uncw.edu)
}

\seealso{
  \code{\link{get.orient}}
}

\examples{
original.orientation = get.orient()
original.orientation
set.orient('school')
get.orient()
set.orient('rc')
get.orient()
set.orient(original.orientation)
}


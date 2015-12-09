\name{enaUncertainty}
\alias{enaUncertainty}
\title{
 Uncertainty Analysis for Ecological Networks
}
\description{
  Applies linear inverse modeling uncertainty analysis from the limSolve
  package (Van den Meershe, 2009; Soetaert, 2009) to flow networks.
  Function generates a specified number of network models with edge
  weights that meet specified constraints.  It returns a list of network
  objects that can be used as the input to other functions in the enaR
  package.
}

\usage{ 
enaUncertainty(x = "network object", p.err = NA, Fu.sd = NA, zu.sd = NA,
yu.sd = NA, Fu.bot = NA, zu.bot = NA, yu.bot = NA, Fu.top = NA, zu.top =
NA, yu.top = NA, iter = 100)
}

\arguments{
  \item{x}{a network object.  This includes all weighted flows into and out of
    each node.}
  \item{p.err}{If present, provides a single value representing the percentage of 
    allowable variation to be assigned to all network edges.}
  \item{Fu.sd}{Matrix of size n by n with values corresponding to each network edge.
    Values specify the variation (plus and minus) to be applied to each edge.  
    Requires zu.sd and yu.sd.}
  \item{zu.sd}{Vector of length n that specifies variation (plus and minus) to be 
    applied to each network input.}
  \item{yu.sd}{Vector of length n that specifies variation (plus and minus) to be 
    applied to each network output.}
  \item{Fu.bot}{Matrix of size n by n with values corresponding to each network edge.
    Values specify lower limit for the acceptable range to be applied to each edge.  
    Requires zu.bot and yu.bot, as well as Fu.top, zu.top, and yu.top to specify 
    the upper limit for the acceptable rage.}
  \item{zu.bot}{Vector of length n that specifies the lower limit to be 
    applied to each network input.}
  \item{yu.bot}{Vector of length n that specifies the lower limit to be 
    applied to each network output.}
  \item{Fu.top}{Matrix of size n by n with values corresponding to each network edge.
    Values specify upper limit for the acceptable range to be applied to each edge.  
    Requires zu.top and yu.top, as well as Fu.bot, zu.bot, and yu.bot to specify 
    the lower limit for the acceptable range.}
  \item{zu.top}{Vector of length n that specifies the upper limit to be 
    applied to each network input.}
  \item{yu.top}{Vector of length n that specifies the upper limit to be 
    applied to each network output.}
  \item{iter}{Number of times to sample the constrained solution space.
    Corresponds to the number of networks generated in the function
    output. Value should be sufficiently large.}
}

\value{
\item{plausible.models}{List of network objects of length iter with 
  edge values that fall within the range of values specified by function inputs.}
}

\references{
  Van den Meersche, K., Soetaert, K. & Van Oevelen, D. (2009) xsample():
  An R function for sampling linear inverse problems. Journal of
  Statistical Software, 30.

  Soetaert, K., Van den Meersche, K. & van Oevelen, D. (2009) limSolve:
  Solving linear inverse models. R package version 1.5.1.
}

\author{
  David E. Hines 
}
\note{
    This function utilizes the xsample() function (Van den Meersche, 2009) from the limSolve package (Soetaert, 2009).
}


\examples{
## # Three different ways to purtub all fluxes in a model by +/- 50%
## # Uniform edge uncertainty (50%)
## data(troModels)
## UA = enaUncertainty(x=troModels[[6]], p.err=50, iter=10000)

## # Data driven uncertainty (standard deviation)
## data(troModels)
## Fu.sd = matrix(c(0, 0, 0, 0, 50,
##                  0, 0, 50, 0, 50,
##                  0, 0, 0, 50, 50,
##                  0, 0, 0, 0, 50,
##                  0, 50, 50, 0, 0), byrow=TRUE, nrow=5, ncol=5)
## zu.sd = c(50, 0, 0, 0, 50)
## yu.sd = c(50, 50, 50, 50, 50)

## UA = enaUncertainty(troModels[[6]], Fu.sd=Fu.sd, zu.sd=zu.sd, yu.sd=yu.sd, iter=10000)

## # Data driven uncertainty (upper and lower limit)
## data(troModels)
## Fu.bot = matrix(c(0, 0, 0, 0, 4440.5,
##                  0, 0, 37.5, 0, 800,
##                  0, 0, 0, 185, 100,
##                  0, 0, 0, 0, 83.5,
##                  0, 2620.5, 1154.5, 0, 0), byrow=TRUE, nrow=5, ncol=5)
## zu.bot = c(5592, 0, 0, 0, 317.5)
## yu.bot = c(1151.5, 1765.0, 907, 101.5, 1984.5)

## Fu.top = matrix(c(0, 0, 0, 0, 13321.5,
##                  0, 0, 112.5, 0, 2500,
##                  0, 0, 0, 555, 300,
##                  0, 0, 0, 0, 250.5,
##                  0, 7807.5, 3463.5, 0, 0), byrow=TRUE, nrow=5, ncol=5)
## zu.top = c(16776, 0, 0, 0, 952.5)
## yu.top = c(3454.5, 5295, 2721, 304.5, 5953.5)

## UA = enaUncertainty(troModels[[6]], Fu.bot=Fu.bot, zu.bot=zu.bot, yu.bot=yu.bot,
##         Fu.top=Fu.top, zu.top=zu.top, yu.top=yu.top, iter=10000)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Uncertainty analysis}
\keyword{Linear inverse modeling}
\keyword{Flow network}

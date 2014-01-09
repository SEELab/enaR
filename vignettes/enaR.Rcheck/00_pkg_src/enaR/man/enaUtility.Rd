\name{enaUtility}
\alias{enaUtility}
\title{
  Utility analysis of ecological networks.
}
\description{
  Performs the flow and storage based utility analysis
  developed for input-output network models of ecosystems.  It returns a
  set of matrices for the direct and integral utilities as well as a set
  of utility based network statistics.
}
\usage{
enaUtility(x,type=c("flow","storage"),eigen.check=TRUE,balance.override=FALSE,tol=10)
}
\arguments{
  \item{x}{a network object.  This includes all weighted flows into and out of
    each node.  For the storage utility analysis this must also include
    the amount of energy--matter stored at each node (biomass).
  }
  \item{type}{
    Determines whether the flow or storage utility analysis is returned.
  }

  \item{eigen.check}{LOGICAL: should the dominant eigenvalue be checked.  Like enaFlow and
    enaStorage analyses, enaUtility analysis considers the utility
    propigated over path lengths ranging for zero to infinity.  For utility
    analysis to work properly, the path sequence must converge.  enaUtility
    checks to see if the utility path sequence is convergent by finding the
    dominant eigenvalue of the direct utility matrix.  If this eigenvalue is
    less than 1, the sequence is convergent and the analysis can be applied;
    if the dominant eigenvalue is greater than one, then the anlysis cannot
    be applied.  By default, the function will not return utility values if
    the eigenvalue is larger than one; however, if eigen.check is set to
    FALSE, then the function will be applied regardless of the mathematic
    validity.
  }
  \item{balance.override}{LOGICAL: should model balancing be ignored.
    enaUtility assumes that the network model is at steady-state.  The
    default setting will not allow the function to be applied to models
    not at steady-state.  However, when balance.override is set to TRUE,
    then the function will work regardless.
  }
  \item{tol}{The integral utility matrix is rounded to the number of
    digits specified in tol.  This approximation eleminates very small
    numbers introduced due to numerical error in the ginv function.  It
    does not eliminate the small numerical error introduced in larger
    values, but does truncate the numbers.
}

}

\value{
  \item{D}{Direct flow utility intensity matrix.  (fij-fji)/Ti for
    i,j=1:n}
  \item{U}{Nondimensional integral flow utility}
  \item{Y}{Dimensional integral flow utility}
  \item{ns}{If type is set to 'flow', this is a list of flow utility network statistics including: the
    dominant eigenvalue of D (lambda\_1D), flow based network
    synergism (synergism.F), and flow based network mutualism (mutualism.F).}
  \item{DS}{Direct storage utility intensity matrix.  (fij-fji)/xi for
    i,j=1:n}
  \item{US}{Nondimensional integral storage utility}
  \item{YS}{Dimensional integral storage utility}
  \item{ns}{If type is set to 'storage', this is a list of storage utility network statistics including: the
    dominant eigenvalue of DS (lambda_1DS), storage based network
    synergism (synergism.S), and storage based network mutualism (mutualism.S).}
}
\references{
  Fath, B.D. and Patten, B.C. 1998. Network synergism: emergence of
  positive relations in ecological systems.
  Ecol. Model. 107:127--143.

  Fath, B.D. and Borrett, S.R. 2006. A Matlab function for Network
  Environ Analysis. Environ. Model. Soft. 21: 375--405.

  Patten, B.C. 1991.  Network ecology: Indirect determination of the
  life-environment relationship in ecosystems.  In: Higashi, M. and
  Burns, T. (eds). Theoretical Studies of Ecosystems: The Network
  Perspective.  Cambridge University Press.  New York.  
}
\author{
  Matthew K. Lau (mkl48@nau.edu)
  Stuart R. Borrett (borretts@uncw.edu)
}
\seealso{
  \code{\link{enaFlow},\link{enaStorage},\link{enaMTI}}
}
\examples{
data(troModels)
U <- enaUtility(troModels[[6]],type="flow",eigen.check=FALSE)
attributes(U)
US <-enaUtility(troModels[[6]],type="storage",eigen.check=FALSE)
}

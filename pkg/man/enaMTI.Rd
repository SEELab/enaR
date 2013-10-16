\name{enaMTI}
\alias{enaMTI}
\title{
  Mixed Trophic Impacts (MTI) measures the impact of one species on another.
}
\description{
  Calculates the Mixed Trophic Impacts of one species on
  another in the given ecosystem model following the algorithm of
  Ulanowicz and Puccia (1990).  This considers both the direct and
  indirect trophic impacts.    
}
\usage{
enaMTI(x,eigen.check=TRUE,zero.na=TRUE, balance.override=FALSE)
}
\arguments{
  \item{x}{a network object.  This includes all weighte dflows into and
    out of each node.  It must also include the "Living" vector that
    identifies the living (TRUE/FALSE) status of each node.  
  }
  \item{zero.na}{
    A logical parameter that specifies if NAs generated in the analysis
    should be reset to zero.  The default is TRUE.
  }
  \item{balance.override}{
    
    Mixed Trophic Impacts analysis builds on flow analysis and thus
    assumes the network model is at steady-state (inputs = outputs).
    Setting balance.override = TRUE allows the function to be run on
    unbalanced models, though this is unadvised.
  }
  \item{eigen.check}{
    
    LOGICAL: should the dominant eigen value be checked?  By default,
    the function will not return utility values if the eigenvalue is
    larger than one; however, if eigen.check is set to
    FALSE, then the function will be applied regardless of the mathematic
    concern.
    
  }

}


\value{
  \item{G}{output-oriented direct flow intensity matrix as in enaFlow,
    except oriented from row to column.}
  \item{FP}{input-oriented direct flow intensity matrix similar to
    enaFlow; however, the calculation exclude respiration losses from the
    throughflow in the denominator to focus on NET production.  Also, if
    the receiver compartment is not living, the flux intensity is set to zero.}
  \item{Q}{direct net trophic impacts (G-t(FP)).}
  \item{M}{Total (direct and indirect) tropic impacts of compartment i on j.}

}
\references{
  %% ~put references to the literature/web site here ~
  Ulanowicz, R.E. and C.J. Puccia.  1990. Mixed trophic impacts in
  ecosystems.  Coenoses 5, 7--16.
}
\author{
  Stuart R. Borrett (borretts@uncw.edu)
  Matthew K. Lau (mkl48@nau.edu)
}
\note{
  This and other Ulanowicz school functions require that export and
  respiration components of output be separately quantified.

  This analysis is similar in concept to the ENA Utility analysis.

  With regard to the eigen.check argument, like enaFlow, enaStorage and enaUtility, this
  analysis considers the trophic impact propigated over path lengths ranging
  for zero to infinity.  For the analysis to work properly, the path
  sequence must converge.  This function checks to see if the path
  sequence is convergent by finding the dominant eigenvalue of the
  direct  matrix.  If this eigenvalue is less than 1, the
  sequence is convergent and the analysis can be applied; if the
  dominant eigenvalue is greater than one, then the anlysis cannot be
  applied.  
}
\seealso{
  \code{\link{enaFlow},\link{enaUtility}}
}
\examples{
data(troModels)
mti <- enaMTI(troModels[[6]])
attributes(mti)
}

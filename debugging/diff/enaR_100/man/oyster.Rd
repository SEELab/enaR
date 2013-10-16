\name{oyster}
\alias{oyster}
\docType{data}
\title{
Intertidal oyster reef ecosystem model.
}
\description{
  Intertidal oyster reef ecosystem model created by Dame and Patten
  (1981).  Data were taken from Patten (1985).  Model flows are in kcal
  m^-2 day^-1; storage data is kcal m^-2.
}
\usage{data(oyster)}
\format{
  The format is:
List of 5
 $ mel:List of 12
  ..$ :List of 3
  .. ..$ inl : int 1
  .. ..$ outl: int 5
  .. ..$ atl :List of 2
  .. .. ..$ na  : logi FALSE
  .. .. ..$ flow: num 0
  ..$ :List of 3
  .. ..$ inl : int 1
  .. ..$ outl: int 6
  .. ..$ atl :List of 2
  .. .. ..$ na  : logi FALSE
  .. .. ..$ flow: num 0
  ..$ :List of 3
  .. ..$ inl : int 2
  .. ..$ outl: int 3
  .. ..$ atl :List of 2
  .. .. ..$ na  : logi FALSE
  .. .. ..$ flow: num 0
  ..$ :List of 3
  .. ..$ inl : int 2
  .. ..$ outl: int 4
  .. ..$ atl :List of 2
  .. .. ..$ na  : logi FALSE
  .. .. ..$ flow: num 0
  ..$ :List of 3
  .. ..$ inl : int 3
  .. ..$ outl: int 4
  .. ..$ atl :List of 2
  .. .. ..$ na  : logi FALSE
  .. .. ..$ flow: num 0.513
  ..$ :List of 3
  .. ..$ inl : int 3
  .. ..$ outl: int 6
  .. ..$ atl :List of 2
  .. .. ..$ na  : logi FALSE
  .. .. ..$ flow: num 15.8
  ..$ :List of 3
  .. ..$ inl : int 4
  .. ..$ outl: int 5
  .. ..$ atl :List of 2
  .. .. ..$ na  : logi FALSE
  .. .. ..$ flow: num 0
  ..$ :List of 3
  .. ..$ inl : int 4
  .. ..$ outl: int 6
  .. ..$ atl :List of 2
  .. .. ..$ na  : logi FALSE
  .. .. ..$ flow: num 0
  ..$ :List of 3
  .. ..$ inl : int 5
  .. ..$ outl: int 6
  .. ..$ atl :List of 2
  .. .. ..$ na  : logi FALSE
  .. .. ..$ flow: num 1.21
  ..$ :List of 3
  .. ..$ inl : int 6
  .. ..$ outl: int 2
  .. ..$ atl :List of 2
  .. .. ..$ na  : logi FALSE
  .. .. ..$ flow: num 1.21
  ..$ :List of 3
  .. ..$ inl : int 6
  .. ..$ outl: int 3
  .. ..$ atl :List of 2
  .. .. ..$ na  : logi FALSE
  .. .. ..$ flow: num 0
  ..$ :List of 3
  .. ..$ inl : int 6
  .. ..$ outl: int 4
  .. ..$ atl :List of 2
  .. .. ..$ na  : logi FALSE
  .. .. ..$ flow: num 0
 $ gal:List of 9
  ..$ n        : num 6
  ..$ mnext    : int 13
  ..$ directed : logi TRUE
  ..$ hyper    : logi FALSE
  ..$ loops    : logi FALSE
  ..$ multiple : logi FALSE
  ..$ bipartite: logi FALSE
  ..$ flow     : num [1:6, 1:6] 0 0 0 0 0.513 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:6] "Filter FEEDERS" "MICROBIOTA" "MEIOFAUNA" "Deposit Feeders" ...
  .. .. ..$ : chr [1:6] "Filter FEEDERS" "MICROBIOTA" "MEIOFAUNA" "Deposit Feeders" ...
  ..$ balanced : logi TRUE
 $ val:List of 6
  ..$ :List of 7
  .. ..$ na          : logi FALSE
  .. ..$ vertex.names: chr "Filter FEEDERS"
  .. ..$ input       : num 41.5
  .. ..$ export      : logi NA
  .. ..$ respiration : num 25.2
  .. ..$ storage     : num 2000
  .. ..$ output      : num 25.2
  ..$ :List of 7
  .. ..$ na          : logi FALSE
  .. ..$ vertex.names: chr "MICROBIOTA"
  .. ..$ input       : num 0
  .. ..$ export      : logi NA
  .. ..$ respiration : num 5.76
  .. ..$ storage     : num 2.41
  .. ..$ output      : num 5.76
  ..$ :List of 7
  .. ..$ na          : logi FALSE
  .. ..$ vertex.names: chr "MEIOFAUNA"
  .. ..$ input       : num 0
  .. ..$ export      : logi NA
  .. ..$ respiration : num 3.58
  .. ..$ storage     : num 24.1
  .. ..$ output      : num 3.58
  ..$ :List of 7
  .. ..$ na          : logi FALSE
  .. ..$ vertex.names: chr "Deposit Feeders"
  .. ..$ input       : num 0
  .. ..$ export      : logi NA
  .. ..$ respiration : num 0.43
  .. ..$ storage     : num 16.3
  .. ..$ output      : num 0.43
  ..$ :List of 7
  .. ..$ na          : logi FALSE
  .. ..$ vertex.names: chr "PREDATORS"
  .. ..$ input       : num 0
  .. ..$ export      : logi NA
  .. ..$ respiration : num 0.359
  .. ..$ storage     : num 69.2
  .. ..$ output      : num 0.359
  ..$ :List of 7
  .. ..$ na          : logi FALSE
  .. ..$ vertex.names: chr "DEPOSITED DETRITUS"
  .. ..$ input       : num 0
  .. ..$ export      : logi NA
  .. ..$ respiration : num 6.18
  .. ..$ storage     : num 1000
  .. ..$ output      : num 6.18
 $ iel:List of 6
  ..$ : int [1:2] 2 1
  ..$ : int [1:2] 4 3
  ..$ : int [1:2] 6 5
  ..$ : int [1:2] 8 7
  ..$ : int 9
  ..$ : int [1:3] 10 11 12
 $ oel:List of 6
  ..$ : int(0) 
  ..$ : int 10
  ..$ : int [1:2] 3 11
  ..$ : int [1:3] 4 5 12
  ..$ : int [1:2] 1 7
  ..$ : int [1:4] 2 6 8 9
 - attr(*, "class")= chr "network"
}
\details{

}
\source{

}
\references{
Dame, R. F., and B. C. Patten. 1981. Analysis of energy flows in an
intertidal oyster reef. Marine Ecology Progress Series 5:115-124.

Patten, B. C. 1985. Energy cycling, length of food chains, and direct
versus indirect effects in ecosystems. Can. Bull. Fish. Aqu. Sci. 213:119-138.
}
\examples{
data(oyster)
## maybe str(oyster) ; plot(oyster) ...
}
\keyword{datasets}

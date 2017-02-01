% Please edit documentation in R/as.extended.R
\name{as.extended}
\alias{as.extended}
\title{as.extended  --- convert a network object to extended format
in Allesina and Bondavalli 2003
INPUT = network model
OUTPUT = the same model in extended format with inputs and
exports/respiration in the same matrix
REFERENCE: Allesina, S., Bondavalli, C., 2003.
Steady state of ecosystem flow networks: a comparison
between balancing procedures. Ecological Modelling 165(2-3):
231-239.
M. Lau July 2011
---------------------------------------------------
as.extended --- convert a network object to extended format in Allesina and
Bondavalli 2003 INPUT = network model OUTPUT = the same model in extended
format with inputs and exports/respiration in the same matrix REFERENCE:
Allesina, S., Bondavalli, C., 2003. Steady state of ecosystem flow networks:
a comparison between balancing procedures. Ecological Modelling 165(2-3):
231-239. M. Lau July 2011
--------------------------------------------------- as.extended --- convert
a network object to extended format in Allesina and Bondavalli 2003 INPUT =
network model OUTPUT = the same model in extended format with inputs and
exports/respiration in the same matrix REFERENCE: Allesina, S., Bondavalli,
C., 2003. Steady state of ecosystem flow networks: a comparison between
balancing procedures. Ecological Modelling 165(2-3): 231-239. M. Lau July
2011 --------------------------------------------------- Create an Extended
Format Matrix}
\usage{
as.extended(x, zero.na = TRUE)
}
\arguments{
\item{x}{A network object.}

\item{zero.na}{Logical: should NA's be replaced with zeros?}
}
\value{
Returns an extended format matrix.
}
\description{
Converts a network object to the extended format of Allesina and Bondavalli
(2003).
}
\details{
Used in the balance function.
}
\examples{



data(troModels)
as.extended(troModels[[6]])



}
\author{
Matthew K. Lau Stuart R. Borrett
}
\references{
Allesina, S., Bondavalli, C., 2003. Steady state of ecosystem
flow networks: a comparison between balancing procedures.Ecological
Modelling 165(2-3):231-239.
}
\seealso{
\code{\link{balance}}
}

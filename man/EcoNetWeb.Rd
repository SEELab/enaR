% Please edit documentation in R/EcoNetWeb.R
\name{EcoNetWeb}
\alias{EcoNetWeb}
\title{Access example EcoNet models from the EcoNet website.}
\usage{
EcoNetWeb(x = "http://eco.engr.uga.edu/Examples/examples.html", model.name = "prompt")
}
\arguments{
\item{x}{The URL for the EcoNet examples.}

\item{model.name}{The model to be accessed. If 'prompt' the user will be
asked for the model they wish to use. Can also be a number for the model or
the name of the model.}
}
\value{
Returns the model formatted as a network object.
}
\description{
This function allows the user to access models that are presented on the
website for EcoNet, the web-based interface for conducting ENA
(http://eco.engr.uga.edu/), by Caner Kazanci at the University of Georgia.
}
\author{
Matthew K. Lau
}
\references{
Kazanci, C., 2007. EcoNet: A new software for ecological
modeling, simulation and network analysis, Ecol. Model., Vol 208/1 pp 3-8.
}
\seealso{
\code{\link{read.EcoNet}}
}

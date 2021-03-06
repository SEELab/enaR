% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enaCycle.R
\name{enaCycle}
\alias{enaCycle}
\title{Full Cycle Analysis of Ecological Networks}
\usage{
enaCycle(x)
}
\arguments{
\item{x}{a network object.  This includes all weighted flows into and out of
each node.}
}
\value{
\item{Table.cycle}{data.frame that presents the details of the
simple cycles in the network. It contains "CYCLE" the cycle number, "NEXUS"
the nexus number corresponding to the cycle, "NODES" the nodes constituting
the cycle} \item{Table.nexus}{data.frame that presents the different nexuses
characterized by their corresponding weak arcs. It contains "NEXUS" the
nexus number, "CYCLES" the number of simple cycles present in that Nexus,
"W.arc.From" the starting node of the corresponding weak arc, "W.arc.To" the
ending node of the corresponding weak arc and "W.arc.Flow" the flow through
that weak arc} \item{CycleDist}{vector of the Cycle Distribution that gives
the flow cycling in loops of different sizes} \item{NormDist}{vector of the
Normalized Distribution i.e. the Cycle Distribution normalized by the Total
System Throughput of the system} \item{ResidualFlows}{matrix of the
straight-through (acyclic) flows in the network}
\item{AggregatedCycles}{matrix of the Aggregated Biogeochemical Cycles in
the network} \item{ns}{vector of the full cycle analysis based network
statistics. These include "NCYCS" the number of simple cycles identified in
the network, "NNEX" the number of the disjoint cycles or number of Nexuses
detected in the network and "CI" the cycling index of the network.}
}
\description{
It performs the full cycle analysis on the network based on the
algorithm described in Ulanowicz (1983) and implemented in NETWRK
4.2b. It returns data.frames with details of the simple cycles and
nexus, vectors of Cycle distributions and Normalized distribution
and matrices of Residual Flows and Aggregated Cycles.
}
\details{
The "NODES" in "Table.cycle" are arranged such that the
weak arc for the nexus is the arc between the first two nodes of
the cycle.
This function uses the backtracking procedure for the
identification of simple cycles, which are cycles that cross a node
only once except the starting node. The backtracking process is a
depth-first search algorithm.

In the data.frame "Table.cycle", if the number of cycles in a nexus
is more than 50, then a blank line is displayed after 50 cycles of
the nexus, followed by the cycles of the next nexus.

The results of the analysis of Feeding Cycles can be obtained as a
byproduct of the enaTroAgg function that analyzes the trophic
dynamics of a network.

At every multiple of 5000 cycles in a nexus, the program prints an
indication for the user to know that it is still running.
}
\examples{

data(troModels)
cyc6 <- enaCycle(troModels[[6]])
attributes(cyc6)

}
\references{
%% ~put references to the literature/web site here ~ Johnson,
D.B. 1975. Finding all the elementary circuits of a directed graph. SIAM J.
Comput. 4:77--84

Ulanowicz, R.E. 1983. Identifying the structure of cycling in ecosystems.
Methematical Biosciences 65:219--237

Ulanowicz, R.E. and Kay, J.J. 1991. A package for the analysis of ecosystem
flow networks. Environmental Software 6:131 -- 142.
}
\seealso{
\code{\link{enaTroAgg}}
}
\author{
Pawandeep Singh
}

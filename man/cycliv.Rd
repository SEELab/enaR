\name{cycliv}
\alias{cycliv}
\title{
  Analysis of Feeding Cycles in a Network
}
\description{
	
  Performs the full cycle analysis on the living subset of the network based on the algorithm described in Ulanowicz (1983) and implemented in NETWRK 4.2b. It returns data.frames with details of the simple cycles and nexus, vectors of Cycle distributions and Normalized distribution and matrices of Residual Flows and Aggregated Cycles.
  
  
}
\usage{
cycliv(x)
}
\arguments{
  \item{x}{
  	a network object.  This includes all weighted flows into and out of each node. It must also include the "Living" vector that identifies the living (TRUE/FALSE) status of each node. Also, non-living nodes must be placed at the end of the node vector. The function netOrder can be used to reorder the network for this.
  }

}


\value{
  \item{Table.cycle}{data.frame that presents the details of the simple cycles in the network. It contains "CYCLE" the cycle number, "NEXUS" the nexus number corresponding to the cycle, "NODES" the nodes constituting the cycle}
  \item{Table.nexus}{data.frame that presents the different nexuses characterized by their corresponding weak arcs. It contains "NEXUS" the nexus number, "CYCLES" the number of simple cycles present in that Nexus, "W.arc.From" the starting node of the corresponding weak arc, "W.arc.To" the ending node of the corresponding weak arc and "W.arc.Flow" the flow through that weak arc}
  \item{CycleDist}{vector of the Cycle Distribution that gives the flow which is cycling in loops of different sizes}
  \item{NormDist}{vector of the Normalized Distribution i.e. the Cycle Distribution normalized by the Total System Throughput for the network}
  \item{ResidualFlows}{matrix of the straight-through (acyclic) flows in the network}
  \item{AggregatedCycles}{matrix of the Aggregated Biogeochemical Cycles in the network}
  \item{ns}{vector of the full cycle analysis based network statistics. These include "NCYCS" the number of simple cycles identified in the network, "NNEX" the number of the disjoint cycles or number of Nexuses detected in the network and "CI" the cycling index of the network.} 
  }
\references{
  %% ~put references to the literature/web site here ~
  Johnson, D.B. 1975. Finding all the elementary circuits of a directed graph. SIAM J. Comput. 4:77--84 

  Ulanowicz, R.E. 1983. Identifying the structure of cycling in ecosystems. Methematical Biosciences 65:219--237

  Ulanowicz, R.E. and Kay, J.J. 1991. A package for the analysis of ecosystem flow networks. Environmental Software 6:131 -- 142.
  
}
\author{
  Pawandeep Singh (pawan.s@live.co.uk)
  }
\note{
	This function uses the same mechanism for analysis as used in the enaCycle function but is restricted to the living nodes only.
	
	Also, similar to the enaCycle function, if the number of cycles in a nexus is more than 50, the "Table.cycle" has a blank line after 50 cycles followed by the cycles for the next nexus.
	
	The analysis requires all the non-living nodes to be placed at the end in the network object.
	}
	
\seealso{
  \code{\link{enaTroAgg}, \link{enaCycle}, \link{netOrder}}
}

\examples{
data(troModels)
liv.cyc <- cycliv(troModels[[6]])
attributes(liv.cyc)
}

#' enaPlot  --- generate a network plot
#' INPUT = network object
#' OUTPUT = network plot
#' M.K. Lau Jan 2016
#' ---------------------------------------------------

library(enaR)
library(ggnet)
library(ggplot2)
library(enaR)
library(igraph)
library(Rgraphviz)
library(intergraph)
## source("http://bioconductor.org/biocLite.R")
## biocLite("Rgraphviz")

## https://www.bioconductor.org/packages/3.3/bioc/vignettes/Rgraphviz/inst/doc/newRgraphvizInterface.pdf

### Rgraphviz
set.seed(123)
V <- letters[1:10]
M <- 1:4
g1 <- randomGraph(V,M,0.2)
plot(g1)

data(oyster)


enaPlot <- function(x){

    ## node names

    ## node scaling

    ## flows 

    ## inputs and outputs

    set.seed(123)
    x <- enaModels[[2]]
    y <- asIgraph(x);y <- as_graphnel(y)
    ew <- as.character(round(unlist(edgeData(y,attr='flow')),2))
    n.ew <- names(unlist(edgeData(y,attr='flow')))
    n.ew <- sub('\\|','~',n.ew);names(ew) <- n.ew
    eAttrs <- list();eAttrs$label <- ew
    
    graph.par(list(edges=list(lty='solid',lwd=0.5,fontsize=10)))
    y <- layoutGraph(y,edgeAttrs=eAttrs)
    renderGraph(y,recipEdges='distinct')

}


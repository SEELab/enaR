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
x <- asIgraph(oyster);x <- as_graphnel(x)
ew <- as.character(round(unlist(edgeData(x,attr='flow')),2))
n.ew <- names(unlist(edgeData(x,attr='flow')))
n.ew <- sub('\\|','~',n.ew);names(ew) <- n.ew
eAttrs <- list();eAttrs$label <- ew

graph.par(list(edges=list(lty='solid',lwd=0.5,fontsize=20)))
x <- layoutGraph(x,edgeAttrs=eAttrs)
renderGraph(x,recipEdges='distinct')

enaPlot <- function(x){
    x %v% 'alp.storage' <- x %v% 'storage' / max(x %v% 'storage')
    x %v% 'col.living' <- ifelse(x %v% 'living' == 'FALSE','red','grey25')
    x %v% 'vertex.names' <- sapply(x %v% 'vertex.names',function(s) 
        paste(sapply(strsplit(s,' ')[[1]],substr,start=1,stop=2),collapse='')
                                   )
    ggnet2(x,edge.size='flow',node.color='col.living',node.size='storage',
           label=TRUE,label.size=6,label.alpha=0.75) +
        guides(color=FALSE)
}


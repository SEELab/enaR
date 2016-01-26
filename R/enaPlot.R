#' enaPlot  --- generate a network plot
#' INPUT = network object
#' OUTPUT = network plot
#' M.K. Lau Jan 2016
#' ---------------------------------------------------

library(ggnet)
library(ggplot2)
library(enaR)

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


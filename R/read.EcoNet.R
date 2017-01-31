### Reading econet models
### 1 Mar 2016
### mklau







#' Read an EcoNet model.
#' 
#' This function allows the user to access models that are formatted for
#' EcoNet, the web-based interface for conducting ENA
#' (http://eco.engr.uga.edu/), by Caner Kazanci at the University of Georgia.
#' 
#' 
#' @param x An object with the EcoNet formatted file, which can be read into R
#' using readLines.
#' @param verbose LOGICAL: should warnings be suppressed?
#' @return Returns the model formatted as a network object.
#' @author Matthew K. Lau
#' @seealso \code{\link{EcoNetWeb}}
#' @references Kazanci, C., 2007. EcoNet: A new software for ecological
#' modeling, simulation and network analysis, Ecol. Model., Vol 208/1 pp 3-8.
read.EcoNet <- function(x,verbose=FALSE){
    if (!(verbose)){options(warn=-1)}
    x <- x[!(grepl('<',x)) & grepl('=',x)]
    x <- x[!(grepl('\\#',x))]
    if (any(!(grepl('c=',x)) & grepl('=',x))){
        stor <- x[grepl('=',x) & !(grepl('c=',x))]
    }else{stor <- NA}
    x <- x[grepl('=',x) & grepl('c=',x)]
    x <- paste(x,collapse=';')
    x <- strsplit(x,' ')[[1]]
    x <- x[x != '']
    x <- paste(x,collapse='')
    x <- strsplit(x,';')[[1]]
    flo <- x[!(grepl('\\*',x))]
    inp <- x[(grepl('\\*->',x))]
    out <- x[(grepl('->\\*',x))]
    flo <- sub('->',';',flo)
    flo <- sub('c=',';',flo)
    flo <- do.call(rbind,strsplit(flo,split=';'))
    flow <- matrix(0,nrow=length(unique(c(flo[,1],flo[,2]))),
                   ncol=length(unique(c(flo[,1],flo[,2]))))
    rownames(flow) <- colnames(flow) <- unique(c(flo[,1],flo[,2]))
    for (i in 1:nrow(flo)){
        flow[rownames(flow) == flo[i,1],colnames(flow) == flo[i,2]] <- as.numeric(flo[i,3])
    }
    inp <- do.call(rbind,strsplit(sub('\\*->','',inp),'c='))
    input <- as.numeric(inp[,2])
    names(input) <- inp[,1]
    out <- do.call(rbind,strsplit(sub('->\\*','',out),'c='))
    output <- as.numeric(out[,2])
    names(output) <- out[,1]
    if (is.na(stor[[1]])){
        storage <- rep(0,nrow(flow))
    }else{
        stor <- paste(stor,collapse='')
        stor <- strsplit(stor,split='')[[1]]
        stor[!(stor %in% c(LETTERS,letters,0:9,'=','.','_',' '))] <- ','
        stor <- paste(stor,collapse='')
        stor <- paste(strsplit(stor,' ')[[1]],collapse='')
        stor <- strsplit(stor,',')[[1]]
        stor <- strsplit(stor,'=')
        stor <- do.call(rbind,stor)
        storage <- as.numeric(stor[,2])
        names(storage) <- stor[,1]
    }
    if (length(input) != nrow(flow)){
        inp <- rep(0,(nrow(flow) - length(input)))
        names(inp) <- rownames(flow)[!(rownames(flow) %in% names(input))]
        input <- c(input,inp)
    }
    if (length(output) != nrow(flow)){
        outp <- rep(0,(nrow(flow) - length(output)))
        names(outp) <- rownames(flow)[!(rownames(flow) %in% names(output))]
        output <- c(output,outp)
    }
    input <- input[match(names(input),rownames(flow))]
    output <- output[match(names(output),rownames(flow))]
    storage <- storage[match(names(storage),rownames(flow))]
    return(pack(flow=flow,input=input,output=output,storage=storage))
}

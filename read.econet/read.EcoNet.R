### Reading econet models
### 1 Mar 2016
### mklau

read.EcoNet <- function(x){
    if (any(grepl('initial stock values',x,ignore.case=TRUE))){
        storage <- x[grep('initial stock values',x,ignore.case=TRUE):length(x)]
    }else{storage <- NA}
    x <- x[grep('#',x,invert=TRUE)]
    main <- x[head(grep('->',x),1):tail(grep('->',x),1)]
    main <- main[main != '']
    main <- strsplit(main,split=' ')
    main <- lapply(main,function(x) x[x != ''])
    main <- do.call(rbind,main)
    main <- main[,-2]
    main[,3] <- sub('c=','',main[,3])
    inputs <- as.numeric(main[main[,1] == '*',3])
    names(inputs) <- main[main[,1] == '*',2]
    outputs <- as.numeric(main[main[,2] == '*',3])
    names(outputs) <- main[main[,2] == '*',1]
    flows <- main[main[,1] != '*'&main[,2] != '*', ]
    M <- matrix(0,nrow=length(unique(c(flows[,1],flows[,2]))),
                ncol=length(unique(c(flows[,1],flows[,2]))))
    rownames(M) <- colnames(M) <- unique(c(flows[,1],flows[,2]))
    for (i in 1:nrow(flows)){
        M[rownames(M) == flows[i,1],colnames(M) == flows[i,2]] <- as.numeric(flows[i,3])
    }
    if (is.na(storage[1])==FALSE){
        storage <- storage[2:(length(storage)-1)]
        storage <- paste(storage,collapse=',',sep='')
        storage <- strsplit(storage,split='')[[1]]
        storage <- storage[!(storage %in% c('',' '))]
        storage <- paste(storage,sep='',collapse='')
        storage <- strsplit(storage,split=',')[[1]]
        storage <- sapply(storage,strsplit,split='=')
        storage <- do.call(rbind,storage)
        storage.vals <- as.numeric(storage[,2])
        names(storage.vals) <- storage[,1]
    }
    if (length(storage.vals) == nrow(M) & all(names(storage.vals) %in% rownames(M))){
        storage.vals <- storage.vals[match(rownames(M),names(storage.vals))]
    }else{
        warning('Storage values do not match flow matrix')
        storage.vals <- NA
    }
    out <- pack(M,input=inputs,output=outputs,storage=storage.vals)
    return(out)
}


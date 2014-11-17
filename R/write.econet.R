###Function to output a model for EcoNet
###http://eco.engr.uga.edu/DOC/econet2.html
###MKLau 17nov2014

###Example
## library(enaR)
## data(enaModels)
## write.econet(enaModels[[3]],names(enaModels)[3])

write.econet <- function(x,mn='model name'){

###
  x <- unpack(x)

###get the model name and set the output file name
  mn <- strsplit(mn,split='')
  mn <- lapply(mn,function(x) x[x%in%letters|x%in%LETTERS|x%in%(1:9)])
  mn <- unlist(lapply(mn,paste,collapse=''))
  file.name <- paste(mn,'.txt',sep='')

###node names
  nn <- rownames(x$F)
  nn <- strsplit(nn,split='')
  nn <- lapply(nn,function(x) x[x%in%letters|x%in%LETTERS|x%in%(1:9)])
  nn <- unlist(lapply(nn,paste,collapse=''))
  rownames(x$F) <- colnames(x$F) <- nn

###Write model name
  write.table(paste('###',mn),file=file.name,col.names=FALSE,row.names=FALSE,quote=FALSE)

###initial conditions
  write.table(paste('###','initial conditions'),file=file.name,col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
  write.table(paste(rownames(x$F),x$X,sep='='),file=file.name,col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)


###write inputs
  write.table(paste('###','inputs'),file=file.name,col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
  write.table(paste('*',' -> ',rownames(x$F),' c=',x$z,sep=''),file=file.name,col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)

###write flows
  write.table(paste('###','flows'),file=file.name,col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
  for (i in 1:nrow(x$F)){
    for (j in 1:nrow(x$F)){
      write.table(paste(rownames(x$F)[i],' -> ',colnames(x$F)[j],' c=',x$F[i,j],sep=''),
                  file=file.name,col.names=FALSE,row.names=FALSE,
                  quote=FALSE,append=TRUE)
    }
  }

###write outputs
  write.table(paste('###','outputs'),file=file.name,col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
  write.table(paste(rownames(x$F),' -> ','*',' c=',(x$r+x$e),sep=''),file=file.name,col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)

}



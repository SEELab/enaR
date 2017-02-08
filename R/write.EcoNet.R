#' ### MKLau 17nov2014

#' Write enaR models to an EcoNet formatted file.
#' 
#' Creates an EcoNet model from an enaR network object that can be used with
#' the online interface for EcoNet.
#' 
#' 
#' @param x Network object.
#' @param file The file name or path. If a simple file name is given, this
#' function uses the current working directory by default.
#' @param mn The model name that EcoNet will use. The DEFAULT is 'ena_model'.
#' @param zero.flows LOGICAL: should zero flow values be written?
#' @return An EcoNet formatted text file is created from the model, which can
#' be input at http://eco.engr.uga.edu.
#' @author Matthew K. Lau
#' @references About EcoNet (http://eco.engr.uga.edu/DOC/econet1.html)
#' 
#' Kazanci, C. 2009. Handbook of Ecological Modelling and Informatics, by WIT
#' Press.
#' @export write.EcoNet
write.EcoNet <- function(x = 'model',file = 'file path',mn = 'ena_model',zero.flows = FALSE){
    x <- unpack(x)
###node names
    nn <- rownames(x$F)
    nn <- strsplit(nn,split='')
    nn <- lapply(nn,function(x) x[x%in%letters|x%in%LETTERS|x%in%(1:9)])
    nn <- unlist(lapply(nn,paste,collapse=''))
    rownames(x$F) <- colnames(x$F) <- nn
###Write model name
    write.table(paste('###',mn),file=file,col.names=FALSE,row.names=FALSE,quote=FALSE)
###initial conditions
    write.table(paste('###','initial conditions'),file=file,col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
    write.table(paste(rownames(x$F),x$X,sep='='),file=file,col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
###write inputs
    write.table(paste('###','inputs'),file=file,col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
    write.table(paste('*',' -> ',rownames(x$F),' c=',x$z,sep=''),file=file,col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
###write flows
    write.table(paste('###','flows'),file=file,col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
    for (i in 1:nrow(x$F)){
        for (j in 1:nrow(x$F)){
            if (x$F[i,j] == 0){}else{
                write.table(paste(rownames(x$F)[i],' -> ',colnames(x$F)[j],' c=',x$F[i,j],sep=''),
                            file=file,col.names=FALSE,row.names=FALSE,
                            quote=FALSE,append=TRUE)
            }
        }
    }
###write outputs
    write.table(paste('###','outputs'),file=file,col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
    write.table(paste(rownames(x$F),' -> ','*',' c=',(x$r+x$e),sep=''),file=file,col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
}

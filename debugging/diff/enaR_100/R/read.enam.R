# R function to read in a matrix formatted as Mdloti (Ursula Sharler)
# Borrett
# Sept. 12, 2012
# ------------------------

read.enam<- function(file="file name and path"){
  # I have assumed the file is formatted as an excel speadsheet.  The
  # data must be on the first sheet in the workbook.
 
  mname=read.xlsx(file,sheetIndex=1,
    rowIndex=1,colIndex=1,header=FALSE); # Get Model ID 
  n=as.numeric(read.xlsx(file,sheetIndex=1,
    rowIndex=c(2),colIndex=2,header=FALSE)); # number of nodes
  liv=as.numeric(read.xlsx(file,sheetIndex=1,
    rowIndex=3,colIndex=2,header=FALSE)); # number of nodes
  
  a = n+6+2 # ending row of flows matrix -- assumes Flows start on row 6 and Imports and Biomasses are at the end
  b = n+3+2 # ending column of flows matrix -- assumes exports and respirations are at the end
  m = read.xlsx(file,sheetIndex=1,header=FALSE,  # Matrix of Flows
    rowIndex=c(6:a), colIndex=c(3:b) )
  m[is.na(m)]=0 # replace NAs with zeros  
  vn=read.xlsx(file,sheetIndex=1,header=FALSE,
    rowIndex=c(6:(n+7)),colIndex=2) # vertex names
  vn=as.character(unlist(vn)) # convert vn to a character strings
  
  
  F = as.matrix(m[1:n,1:n]) # flow matrix

  rownames(F) <- vn[1:n]
  colnames(F) <- vn[1:n]
  imports = as.numeric(unlist(m[(n+1),1:n]))
  biomass = as.numeric(unlist(m[(n+2),1:n]))
  exports = as.numeric(unlist(m[1:n,(n+1)]))
  respiration= as.numeric(unlist(m[1:n,(n+2)]))
  
  y <- network(F,directed=TRUE)
                                        # packing up the attributes into the network object (y)
  set.vertex.attribute(y,'input',imports)
  set.vertex.attribute(y,'export',exports)
  set.vertex.attribute(y,'respiration',respiration)
  set.vertex.attribute(y,'storage',biomass)
  set.vertex.attribute(y,'output',exports+respiration)
  y%v%'vertex.names' <- vn[1:n]
  y%v%'living'=c(rep(TRUE,liv),rep(FALSE,n-liv))
  y%n%'flow' <- F 
  return(y)
}

# read.wand --- WAND formatted file into R 
# INPUT = file path
# OUTPUT = network object
# S. Borrett | May 2012
# ------------------------------------

read.wand <- function(file='file name with path'){
                                        # file is the full excel file name
                                        # asssumes that first sheet is "Main" and second sheet is "Flows".
  d1 <- read.xlsx(file,sheetName="Main",rowIndex=1:8,colIndex=1)
  n <- as.numeric(as.character(d1$Network.Analysis[3]))
  dat.main <- read.xlsx(file,sheetName="Main",rowIndex=9:(n+9),colIndex=1:6)
  dat.main[is.na(dat.main)] <- 0
  dat.main$Compartments.Names <- as.character(dat.main$Compartments.Names)  # convert to strings
                                        # get flows
  F <- read.xlsx(file,sheetName="Flows",rowIndex=1:(n+1),colIndex=1:(n+1))
  flow.mat <- as.matrix(F[1:n,2:(n+1)])
  flow.mat[is.na(flow.mat)] <- 0

  x <- list('flow'=flow.mat,
    'input'=dat.main$Imports,
    'exports'=dat.main$Exports,
    'respiration'=dat.main$Respirations,
    'storage'=dat.main$Standing.Stocks)
  ## --- Create Network Object From Data ---
  y <- network(x[[1]],directed=TRUE)
                                        # packing up the attributes into the network object (y)
  set.vertex.attribute(y,'input',x[[2]])
  set.vertex.attribute(y,'export',x[[3]])
  set.vertex.attribute(y,'respiration',x[[4]])
  set.vertex.attribute(y,'storage',x[[5]])
  set.vertex.attribute(y,'output',x[[3]]+x[[4]])
  y%v%'vertex.names' <- dat.main$Compartments.Names
  rownames(flow.mat) <- dat.main$Compartments.Names
  colnames(flow.mat) <- rownames(flow.mat)
  y%n%'flow' <- flow.mat
  return(y)
}

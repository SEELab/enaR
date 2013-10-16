### R code from vignette source 'enaR.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: z
###################################################
# set plotting parameters
opar <- par(las=1,mar=c(0,0,0,0),xpd=TRUE,bg="white")   

## Taken from https://stat.ethz.ch/pipermail/r-devel/2011-September/062126.html
if (all(ls()!='f.list')){
  require(codetools)
library(enaR)
called.by <- function(tarFunc, tarPack){
  flist <-   sapply(lsf.str(tarPack, all=TRUE), c)
  names(flist) <- NULL
  gotit <- sapply(flist, function(x) tarFunc %in% findGlobals(get(x, tarPack),FALSE)$functions)
  flist[gotit]
}

f.list <- as.character(sapply(lsf.str('package:enaR',all=TRUE),c))
f.array <- array(0,dim=rep(length(f.list),2))
rownames(f.array) <- colnames(f.array) <- f.list
for (i in 1:length(f.list)){
  f.array[match(called.by(f.list[i],'package:enaR'),rownames(f.array)),i] <- 1
}
f.net <- network(t(f.array))
}

plot(f.net,displaylabels=TRUE,label.cex=0.85,arrowhead.cex=0.65,
     edge.lwd=0.75,vertex.col='lightblue',vertex.border='white',edge.col='darkgrey')


###################################################
### code chunk number 2: enaR.Rnw:151-152
###################################################
getOption("SweaveHooks")[["fig"]]()
# set plotting parameters
opar <- par(las=1,mar=c(0,0,0,0),xpd=TRUE,bg="white")   

## Taken from https://stat.ethz.ch/pipermail/r-devel/2011-September/062126.html
if (all(ls()!='f.list')){
  require(codetools)
library(enaR)
called.by <- function(tarFunc, tarPack){
  flist <-   sapply(lsf.str(tarPack, all=TRUE), c)
  names(flist) <- NULL
  gotit <- sapply(flist, function(x) tarFunc %in% findGlobals(get(x, tarPack),FALSE)$functions)
  flist[gotit]
}

f.list <- as.character(sapply(lsf.str('package:enaR',all=TRUE),c))
f.array <- array(0,dim=rep(length(f.list),2))
rownames(f.array) <- colnames(f.array) <- f.list
for (i in 1:length(f.list)){
  f.array[match(called.by(f.list[i],'package:enaR'),rownames(f.array)),i] <- 1
}
f.net <- network(t(f.array))
}

plot(f.net,displaylabels=TRUE,label.cex=0.85,arrowhead.cex=0.65,
     edge.lwd=0.75,vertex.col='lightblue',vertex.border='white',edge.col='darkgrey')


###################################################
### code chunk number 3: enaR.Rnw:182-188
###################################################
  rm(list=ls())
par(mfrow=c(1,1))
options(width=72)
figset <- function() par(mar=c(4,4,1,1)+.1)
options(SweaveHooks = list(fig = figset))
options("prompt" = "> ", "continue" = "+  ")


###################################################
### code chunk number 4: enaR.Rnw:191-192
###################################################
library(enaR)


###################################################
### code chunk number 5: enaR.Rnw:281-297
###################################################
# generate the flow matrix
flow.mat <- array(abs(rnorm(100,4,2))*sample(c(0,1),100,replace=TRUE),
                  dim=c(4,4))
# name the nodes
rownames(flow.mat) <- colnames(flow.mat) <- paste('node',(1:nrow(flow.mat)),sep='')
# generate the inputs
inputs <- runif(nrow(flow.mat),0,4)
# generate the exports
exports <- inputs
# pack
fake.model <- pack(flow=flow.mat,
                   input=inputs,
                   export=exports,
                   living=TRUE)
# model
fake.model


###################################################
### code chunk number 6: enaR.Rnw:303-304
###################################################
attributes(fake.model)


###################################################
### code chunk number 7: enaR.Rnw:311-316
###################################################
fake.model%v%'output'

fake.model%v%'input'

fake.model%v%'living'


###################################################
### code chunk number 8: enaR.Rnw:321-322
###################################################
fake.model%n%'flow'


###################################################
### code chunk number 9: enaR.Rnw:330-331
###################################################
unpack(fake.model)  


###################################################
### code chunk number 10: enaR.Rnw:349-357
###################################################
## --- Check to see if the model is balanced ---#
ssCheck(fake.model)

## --- To BALANCE a model if needed --- #
fake.model <- balance(fake.model,method="AVG2")  

## --- To FORCE BALANCE a model if needed --- #
fake.model <- force.balance(fake.model)


###################################################
### code chunk number 11: enaR.Rnw:377-379
###################################################
scor.model <- readLines('http://people.uncw.edu/borretts/data/oyster.dat')
m <- read.scor(scor.model,from.file=FALSE)


###################################################
### code chunk number 12: enaR.Rnw:386-387
###################################################
unpack(m)


###################################################
### code chunk number 13: enaR.Rnw:392-394
###################################################
data(oyster)
m <- oyster


###################################################
### code chunk number 14: enaR.Rnw:407-408 (eval = FALSE)
###################################################
##   m <- read.wand('./MDmar02_WAND.xls')


###################################################
### code chunk number 15: enaR.Rnw:434-435 (eval = FALSE)
###################################################
##   m <- read.enam('./MDMAR02.xlsx')


###################################################
### code chunk number 16: enaR.Rnw:467-475 (eval = FALSE)
###################################################
## data(oyster)
## # write oyster reef model to a csv file
## write.nea(oyster, file.name="oyster.csv")
## # read in oyster reef model data from NEA.m formatted CSV file
## m <- read.nea("oyster.csv")
## 
## # Again, this model object does NOT contain all 
## # of the information in the "oyster" data object.


###################################################
### code chunk number 17: a
###################################################
data(oyster)  # load data
m <- oyster  
set.seed(2)    # set random seed to control plot
plot(m)       # plot network data object (uses plot.network)


###################################################
### code chunk number 18: b
###################################################
# set colors to use
my.col=c("red","yellow", 
  rgb(204,204,153,maxColorValue=255), 
  "grey22")  
F=m%n%'flow'                   # extract flow information for later use.
f=which(F!=0, arr.ind=T)       # get indices of positive flows
opar <- par(las=1,bg=my.col[4],xpd=TRUE,mai=c(1.02, 0.62, 0.82, 0.42))
set.seed(2)                    # each time the plot is called, the
                               # layout orientation changes.  setting
                               # the seed ensures a consistent
                               # orientation each time the plot
                               # function is called.
plot(m,
     vertex.cex=log(m%v%'storage'), # scale nodes with storage
     label= m%v%'vertex.names',     # add node labels
     boxed.labels=FALSE,
     label.cex=0.65,
     vertex.sides=45,   # to make rounded
     edge.lwd=log10(abs(F[f])),     # scale arrows to flow magnitude
     edge.col=my.col[3],
     vertex.col=my.col[1],
     label.col="white",
     vertex.border = my.col[3],
     vertex.lty = 1,
     xlim=c(-4,1),ylim=c(-2,-2))
rm(opar)             # remove changes to the plotting parameters


###################################################
### code chunk number 19: enaR.Rnw:533-534
###################################################
getOption("SweaveHooks")[["fig"]]()
data(oyster)  # load data
m <- oyster  
set.seed(2)    # set random seed to control plot
plot(m)       # plot network data object (uses plot.network)


###################################################
### code chunk number 20: enaR.Rnw:536-537
###################################################
getOption("SweaveHooks")[["fig"]]()
# set colors to use
my.col=c("red","yellow", 
  rgb(204,204,153,maxColorValue=255), 
  "grey22")  
F=m%n%'flow'                   # extract flow information for later use.
f=which(F!=0, arr.ind=T)       # get indices of positive flows
opar <- par(las=1,bg=my.col[4],xpd=TRUE,mai=c(1.02, 0.62, 0.82, 0.42))
set.seed(2)                    # each time the plot is called, the
                               # layout orientation changes.  setting
                               # the seed ensures a consistent
                               # orientation each time the plot
                               # function is called.
plot(m,
     vertex.cex=log(m%v%'storage'), # scale nodes with storage
     label= m%v%'vertex.names',     # add node labels
     boxed.labels=FALSE,
     label.cex=0.65,
     vertex.sides=45,   # to make rounded
     edge.lwd=log10(abs(F[f])),     # scale arrows to flow magnitude
     edge.col=my.col[3],
     vertex.col=my.col[1],
     label.col="white",
     vertex.border = my.col[3],
     vertex.lty = 1,
     xlim=c(-4,1),ylim=c(-2,-2))
rm(opar)             # remove changes to the plotting parameters


###################################################
### code chunk number 21: enaR.Rnw:599-602
###################################################
St <- enaStructure(m)
attributes(St)
St$ns


###################################################
### code chunk number 22: enaR.Rnw:665-671
###################################################
  F <- enaFlow(m)
attributes(F)
F$ns
G <- F$G # output-oriented direct flow matrix
rm(G)
F$NP     # input-oriented integral flow matrix


###################################################
### code chunk number 23: enaR.Rnw:678-681
###################################################
attach(F)
G
detach(F) 


###################################################
### code chunk number 24: enaR.Rnw:688-689
###################################################
mExp(F$G,2)


###################################################
### code chunk number 25: enaR.Rnw:701-702
###################################################
  enaAscendency(oyster)


###################################################
### code chunk number 26: enaR.Rnw:732-735
###################################################
  S <- enaStorage(m)
attributes(S)
S$ns


###################################################
### code chunk number 27: enaR.Rnw:792-795
###################################################
UF <- enaUtility(m,eigen.check=TRUE,type="flow")  
US <- enaUtility(m,eigen.check=TRUE,type="storage")  
attributes(UF)


###################################################
### code chunk number 28: enaR.Rnw:842-845
###################################################
E <- enaEnviron(m)
attributes(E)
E$output[1] 


###################################################
### code chunk number 29: enaR.Rnw:853-855
###################################################
tet <- TET(m)
show(tet)


###################################################
### code chunk number 30: enaR.Rnw:862-864
###################################################
tes <- TES(m)
show(tes)


###################################################
### code chunk number 31: enaR.Rnw:872-874
###################################################
C <- enaControl(m)               
attributes(C)


###################################################
### code chunk number 32: enaR.Rnw:887-892
###################################################
                                        #conduct mixed trophic impacts
mti <- enaMTI(oyster)
attributes(mti)
                                        #shows the total impact matrix
mti$M


###################################################
### code chunk number 33: enaR.Rnw:901-904
###################################################
  mti <- enaMTI(oyster,eigen.check=FALSE)
attributes(mti)
mti$M  # shows the total impact matrix


###################################################
### code chunk number 34: enaR.Rnw:933-935
###################################################
ns <- get.ns(m)
str(ns)    # examine the structure of ns


###################################################
### code chunk number 35: enaR.Rnw:941-943
###################################################
oyster.ena <- enaAll(oyster)
names(oyster.ena)


###################################################
### code chunk number 36: enaR.Rnw:951-957
###################################################
F <- enaFlow(oyster)

ec <- environCentrality(F$N)
show(ec)

eigenCentrality(F$G)


###################################################
### code chunk number 37: b
###################################################
# set plotting parameters
opar <- par(las=1,mar=c(7,5,1,1),xpd=TRUE,bg="white")   
# find centrality order
o <- order(ec$AEC,decreasing=TRUE)            
bp <- barplot(ec$AEC[o],     # create barplot
              names.arg=NA,
              ylab="Average Environ Centrality",
              col="black",border=NA)
text(bp,-0.008,                # add labels
     labels=names(ec$AEC)[o],
     srt=35,adj=1,cex=1)
rm(opar)  # remove the plotting parameters


###################################################
### code chunk number 38: enaR.Rnw:983-984
###################################################
getOption("SweaveHooks")[["fig"]]()
# set plotting parameters
opar <- par(las=1,mar=c(7,5,1,1),xpd=TRUE,bg="white")   
# find centrality order
o <- order(ec$AEC,decreasing=TRUE)            
bp <- barplot(ec$AEC[o],     # create barplot
              names.arg=NA,
              ylab="Average Environ Centrality",
              col="black",border=NA)
text(bp,-0.008,                # add labels
     labels=names(ec$AEC)[o],
     srt=35,adj=1,cex=1)
rm(opar)  # remove the plotting parameters


###################################################
### code chunk number 39: enaR.Rnw:1005-1019
###################################################
###Check the current orientation
get.orient()
###enaFlow output in row-column
flow.rc <- enaFlow(oyster)$G
###Set the global orientation to school
set.orient('school')
###Check that it worked
get.orient()
###enaFlow output in column-row
flow.cr <- enaFlow(oyster)$G
###Check. Outputs should be transposed from each other.
flow.rc == t(flow.cr)
###Now change back to the default orientation ('rc')
set.orient('rc')


###################################################
### code chunk number 40: enaR.Rnw:1057-1068
###################################################
### Import the model sets
data(bgcModels)
data(troModels)
### Check the first few model names
head(names(bgcModels))
head(names(troModels))
### Isolate a single model
x <- troModels[[1]]
x <- troModels$"Marine Coprophagy (oyster)"
### Check out the model
summary(x)


###################################################
### code chunk number 41: enaR.Rnw:1242-1243
###################################################
data(troModels)


###################################################
### code chunk number 42: enaR.Rnw:1252-1268
###################################################
# balance models as necessary
m.list <- lapply(troModels,balance)

# if balancing fails, you can use force.balance
# to repeatedly apply the balancing procedure
unlist(lapply(m.list,ssCheck))
m.list <- lapply(m.list,force.balance)
##Check that all the models are balanced
all(unlist(lapply(m.list,ssCheck)))

# Example Flow Analysis
F.list <- lapply(m.list, enaFlow)

# the full results of the flow analysis is now stored in the elements
# of the F.list.  To get the results for just the first model...
F.list[[1]]


###################################################
### code chunk number 43: enaR.Rnw:1274-1278
###################################################
# Example of extracting just specific information - Indirect Effects Ratio
IDs <- unlist(lapply(m.list, function(x) enaFlow(x)$ns[8]))
#Look at the first few ID's
head(IDs)


###################################################
### code chunk number 44: enaR.Rnw:1282-1284
###################################################
# Here is a list containing only the output-oriented integral flow matrices
N.list <- lapply(m.list,function(x) enaFlow(x)$N)


###################################################
### code chunk number 45: enaR.Rnw:1291-1298
###################################################
# Collecting and combining all network statistics
ns.list <- lapply(m.list,get.ns) # returns as list
ns <- do.call(rbind,ns.list)  # ns as a data.frame
# Let's take a quick look at some of the output
colnames(ns)    # return network statistic names.  
dim(ns)         # show dimensions of ns matrix
ns[1:5,1:5]     # show selected results


###################################################
### code chunk number 46: b
###################################################
opar <- par(las=1,mar=c(9,7,2,1),xpd=TRUE,mfrow=c(1,2),oma=c(1,1,0,0))
x=dim(ns)[1] # number of models
m.select <- 40:45
bp=barplot(ns$ID.F[m.select],ylab="Indirect-to-Direct Flow Ratio (I/D, Realized)",
        col="darkgreen",border=NA,ylim=c(0,2))
text(bp,-0.05,                # add labels
     labels=rownames(ns)[m.select],
       srt=45,adj=1,cex=0.85)
opar <- par(xpd=FALSE)
abline(h=1,col="orange",lwd=2)
#
plot(ns$FCI,ns$ID.F,pch=20,col="blue",cex=2,
     ylab="Indirect-to-Direct Flow Ratio (I/D, Realized)",
     xlab="Finn Cycling Index (FCI)",
     xlim=c(0,0.8),ylim=c(0,8))
#
rm(opar)  # remove the plotting parameters


###################################################
### code chunk number 47: enaR.Rnw:1329-1330
###################################################
getOption("SweaveHooks")[["fig"]]()
opar <- par(las=1,mar=c(9,7,2,1),xpd=TRUE,mfrow=c(1,2),oma=c(1,1,0,0))
x=dim(ns)[1] # number of models
m.select <- 40:45
bp=barplot(ns$ID.F[m.select],ylab="Indirect-to-Direct Flow Ratio (I/D, Realized)",
        col="darkgreen",border=NA,ylim=c(0,2))
text(bp,-0.05,                # add labels
     labels=rownames(ns)[m.select],
       srt=45,adj=1,cex=0.85)
opar <- par(xpd=FALSE)
abline(h=1,col="orange",lwd=2)
#
plot(ns$FCI,ns$ID.F,pch=20,col="blue",cex=2,
     ylab="Indirect-to-Direct Flow Ratio (I/D, Realized)",
     xlab="Finn Cycling Index (FCI)",
     xlim=c(0,0.8),ylim=c(0,8))
#
rm(opar)  # remove the plotting parameters


###################################################
### code chunk number 48: enaR.Rnw:1356-1359
###################################################
betweenness(oyster)

closeness(oyster)


###################################################
### code chunk number 49: d
###################################################
m <- troModels[[38]]
b <- betweenness(m)         # calculate betweenness centrality
nms <- m%v%'vertex.names'   # get vertex names
show(nms)
nms[b<=(0.1*max(b))] <- NA  # exclude less central nodes

set.seed(3)
opar <- par(xpd=TRUE,mfrow=c(1,1))
# create target plot
gplot.target(m,b,#circ.lab=FALSE,
             edge.col="grey",
             label=nms) # show only labels of most central nodes
             #xlim=c(-1,4))
rm(opar)


###################################################
### code chunk number 50: enaR.Rnw:1387-1388
###################################################
getOption("SweaveHooks")[["fig"]]()
m <- troModels[[38]]
b <- betweenness(m)         # calculate betweenness centrality
nms <- m%v%'vertex.names'   # get vertex names
show(nms)
nms[b<=(0.1*max(b))] <- NA  # exclude less central nodes

set.seed(3)
opar <- par(xpd=TRUE,mfrow=c(1,1))
# create target plot
gplot.target(m,b,#circ.lab=FALSE,
             edge.col="grey",
             label=nms) # show only labels of most central nodes
             #xlim=c(-1,4))
rm(opar)


###################################################
### code chunk number 51: enaR.Rnw:1398-1403
###################################################
centralization(oyster, degree)

centralization(oyster,closeness)

centralization(oyster,betweenness)


###################################################
### code chunk number 52: e
###################################################
library(igraph)
### The adjacency matrix
A <- St$A

### creating an iGraph graph
g <- graph.adjacency(A)
plot(g)  # uses iGraph plot tools


###################################################
### code chunk number 53: enaR.Rnw:1429-1430
###################################################
getOption("SweaveHooks")[["fig"]]()
library(igraph)
### The adjacency matrix
A <- St$A

### creating an iGraph graph
g <- graph.adjacency(A)
plot(g)  # uses iGraph plot tools


###################################################
### code chunk number 54: enaR.Rnw:1435-1455
###################################################
# betweenness centrality (calculated by iGraph and sna)
betweenness(g) 

# shortest path between any two nodes
shortest.paths(g) 

# average path length in the network (graph theory sense)
average.path.length(g,directed=TRUE)   

diameter(g)  # diameter of the graph

vertex.connectivity(g)  # connectivity of a graph (group cohesion)
subcomponent(g,1,'in')  # subcomponent reachable from 1 along inputs
subcomponent(g,2,'in')  # subcomponent reachable from 2 along inputs
subcomponent(g,1,'out') # subcomponent reachable from 1 along outputs
subcomponent(g,2,'out') # subcomponent reachable from 2 along output

edge.connectivity(g)

detach(package:igraph)  # detach igraph package



pkgname <- "enaR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('enaR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("TES")
### * TES

flush(stderr()); flush(stdout())

### Name: TES
### Title: Calculate the total environ storage.
### Aliases: TES

### ** Examples

data(troModels)
TES(troModels[[6]])



cleanEx()
nameEx("TET")
### * TET

flush(stderr()); flush(stdout())

### Name: TET
### Title: Calculates the total environ throughflow for a ecosystem network
###   model.
### Aliases: TET

### ** Examples

data(troModels)
tet <- TET(troModels[[6]])
tet



cleanEx()
nameEx("as.extended")
### * as.extended

flush(stderr()); flush(stdout())

### Name: as.extended
### Title: Create an extended format matrix.
### Aliases: as.extended

### ** Examples

data(troModels)
as.extended(troModels[[6]])



cleanEx()
nameEx("balance")
### * balance

flush(stderr()); flush(stdout())

### Name: balance
### Title: Balance flow network models.
### Aliases: balance

### ** Examples

data(troModels)
balance(troModels[[6]])



cleanEx()
nameEx("enaAll")
### * enaAll

flush(stderr()); flush(stdout())

### Name: enaAll
### Title: Conduct all major ENA.
### Aliases: enaAll

### ** Examples

data(troModels)
output = enaAll(troModels[[6]])
names(output)



cleanEx()
nameEx("enaAscendency")
### * enaAscendency

flush(stderr()); flush(stdout())

### Name: enaAscendency
### Title: Calculates the ascendency of an ecological network.
### Aliases: enaAscendency

### ** Examples

data(troModels)
enaAscendency(troModels[[6]])



cleanEx()
nameEx("enaControl")
### * enaControl

flush(stderr()); flush(stdout())

### Name: enaControl
### Title: Control analyses of ecological networks.
### Aliases: enaControl

### ** Examples

data(troModels)
enaControl(troModels[[6]])



cleanEx()
nameEx("enaEnviron")
### * enaEnviron

flush(stderr()); flush(stdout())

### Name: enaEnviron
### Title: Ecological network environs.
### Aliases: enaEnviron

### ** Examples

data(troModels)
enaEnviron(troModels[[6]])



cleanEx()
nameEx("enaFlow")
### * enaFlow

flush(stderr()); flush(stdout())

### Name: enaFlow
### Title: Flow analyses of ecological networks.
### Aliases: enaFlow

### ** Examples

data(troModels)
F = enaFlow(troModels[[6]])  # completes the full analysis
F$ns  # returns just the network statisics



cleanEx()
nameEx("enaMTI")
### * enaMTI

flush(stderr()); flush(stdout())

### Name: enaMTI
### Title: Mixed Trophic Impacts (MTI) measures the impact of one species
###   on another.
### Aliases: enaMTI

### ** Examples

data(troModels)
mti <- enaMTI(troModels[[6]])
attributes(mti)



cleanEx()
nameEx("enaStorage")
### * enaStorage

flush(stderr()); flush(stdout())

### Name: enaStorage
### Title: Storage analyses of ecological networks.
### Aliases: enaStorage
### Keywords: enaFlow read.scor

### ** Examples

data(oyster)
S<-enaStorage(oyster)
attributes(S)



cleanEx()
nameEx("enaStructure")
### * enaStructure

flush(stderr()); flush(stdout())

### Name: enaStructure
### Title: Structure analyses of ecological network.
### Aliases: enaStructure

### ** Examples

data(troModels)
enaStructure(troModels[[6]])



cleanEx()
nameEx("enaUtility")
### * enaUtility

flush(stderr()); flush(stdout())

### Name: enaUtility
### Title: Utility analysis of ecological networks.
### Aliases: enaUtility

### ** Examples

data(troModels)
U <- enaUtility(troModels[[6]],type="flow",eigen.check=FALSE)
attributes(U)
US <-enaUtility(troModels[[6]],type="storage",eigen.check=FALSE)



cleanEx()
nameEx("environCentrality")
### * environCentrality

flush(stderr()); flush(stdout())

### Name: environCentrality
### Title: Calculates the environ centrality of the nodes in an ecological
###   network.
### Aliases: environCentrality

### ** Examples

data(troModels) 
F<-enaFlow(troModels[[6]])
ec<-environCentrality(F$N)
attributes(ec)
barplot(sort(ec$AEC,decreasing=TRUE),col=4,
   ylab="Average Environ Centrality",
   ylim=c(0,0.4))



cleanEx()
nameEx("findPathLength")
### * findPathLength

flush(stderr()); flush(stdout())

### Name: findPathLength
### Title: Cumulative flow over a range of path lengths.
### Aliases: findPathLength

### ** Examples

data(troModels)
pl10 <- findPathLength(troModels[[6]], plot.sw=TRUE,maxPath=10)
names(pl10)
pl10$thresholds



cleanEx()
nameEx("force.balance")
### * force.balance

flush(stderr()); flush(stdout())

### Name: force.balance
### Title: Repeated, sequential application the balance function.
### Aliases: force.balance

### ** Examples

data(troModels)
ssCheck(troModels[[1]])
fb.model=force.balance(troModels[[2]]) #produces a balanced model



cleanEx()
nameEx("get.ns")
### * get.ns

flush(stderr()); flush(stdout())

### Name: get.ns
### Title: Quick calculation of a range of network statistics.
### Aliases: get.ns

### ** Examples

data(troModels)
get.ns(troModels[[6]])



cleanEx()
nameEx("scc")
### * scc

flush(stderr()); flush(stdout())

### Name: scc
### Title: Finds the strongly connected component (SCC) in a graph.
### Aliases: scc

### ** Examples

data(troModels)
A<-enaStructure(troModels[[6]])$A
scc(A)



cleanEx()
nameEx("set.orient")
### * set.orient

flush(stderr()); flush(stdout())

### Name: set.orient
### Title: Globally set the output matrix orientation.
### Aliases: set.orient

### ** Examples

original.orientation = get.orient()
original.orientation
set.orient('school')
get.orient()
set.orient('rc')
get.orient()
set.orient(original.orientation)



cleanEx()
nameEx("ssCheck")
### * ssCheck

flush(stderr()); flush(stdout())

### Name: ssCheck
### Title: Checks the balance of inputs and outputs from a network.
### Aliases: ssCheck

### ** Examples

data(troModels)
ssCheck(troModels[[2]])
ssCheck(troModels[[6]])



cleanEx()
nameEx("unpack")
### * unpack

flush(stderr()); flush(stdout())

### Name: unpack
### Title: "Unpacks" the components of a network object into separate
###   objects.
### Aliases: unpack

### ** Examples

data(troModels)
unpack(troModels[[6]])



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

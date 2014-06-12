############################################################
#
#  enaUncertainty 2.0     adding data object for uncertainty
#  Initial run file    required function: enaUncertainty_2.0
#                      required packages: enaR 2.6; limSolve
#  Dave Hines                                      6/11/2014
#
############################################################

setwd('~/Desktop/Generalized Uncertainty/enaUncertainty 2.0')

# load enaR
library(enaR)
#set.orient('school')

# load limSolve
library(limSolve)


############################################################
########### Create a hypothetical 5 node model #############
############################################################

flow = matrix(c(0,9,0,4,0,
             3,0,0,0,2,
             4,0,0,0,0,
             16,1,0,0,0,
             0,0,1,13,0), 5, 5, byrow = TRUE)
flow=t(flow) # I had to do this to make the model balance even with 'school' - I think something is up with the orientation in pack

rownames(flow) = c('node 1', 'node 2', 'node 3', 'node 4', 'node 5')
colnames(flow) = c('node 1', 'node 2', 'node 3', 'node 4', 'node 5')

z = c(10,5,0,0,0)
y = c(0,0,3,0,12)
e = y
r = rep(0,5)
X = rep(1,5)

living = c(TRUE, TRUE, FALSE, FALSE, FALSE)

M=pack(flow=flow, input=z, output=y, export=e, respiration=r, storage=X, living=living)

# Check to make sure the model is balanced
ssCheck(M, more=TRUE)

##### Create uncertainty data for flows #####
         # each data point represents a standard deviation for the corresponding value in the model

Fu = matrix(c(0,10.3,0,2.2,0,
             3.1,0,0,0,1.8,
             100.3,0,0,0,0,
             5.1,0.2,0,0,0,
             0,0,1.1,2.9,0), 5, 5, byrow = TRUE)
Fu=t(Fu)

zu = c(3.1,3.2,0,0,0)
yu = c(0,0,4.7,0,4.1)

############################################################
############### Apply uncertainty analysis #################
############################################################

source('enaUncertainty_2.0.R')

PM = enaUncertainty(M, p.err=0.001, iter=100)

############################################################
################ Some basic ENA analyses ###################
############################################################

# Network level indicators
nominal.ns=get.ns(M)

UA.ns=lapply(PM, get.ns)
UA.ns=do.call(rbind, UA.ns)

orig.ns=c(nominal.ns$APL, nominal.ns$FCI, nominal.ns$ID.F, nominal.ns$HMG.I)
box.ns=cbind(UA.ns$APL, UA.ns$FCI, UA.ns$ID.F, UA.ns$HMG.I)

dev.new()
par(las=1)
bp=boxplot(box.ns, names=c('APL', 'FCI', 'ID.F', 'HMG.I'), ylab='Appropriate Units', main='Some Network Statistics')
points(x=seq_along(bp$names), y=orig.ns, col='red', pch=15, cex=1.2)
legend('topright', legend=c('Original Model'), pch=15, col='red', bty='n')

# Flow analysis
F.T=enaFlow(M)$T

UA.F=lapply(PM, function(x) enaFlow(x)$T)
UA.F=do.call(rbind, UA.F)

dev.new()
par(las=1)
bp=boxplot(UA.F, main='Throughflow Centrality', ylab='Model Units')
points(x=seq_along(bp$names), y=F.T, col='red', pch=15, cex=1.2)
legend('topright', legend=c('Original Model'), pch=15, col='red', bty='n')

 
# TET analysis

nominal.tet=TET(M)

UA.tet=lapply(PM, TET)

node1.tet.ri=0
node2.tet.ri=0
node3.tet.ri=0
node4.tet.ri=0
node5.tet.ri=0

node1.tet.ro=0
node2.tet.ro=0
node3.tet.ro=0
node4.tet.ro=0
node5.tet.ro=0

node1.tet.ui=0
node2.tet.ui=0
node3.tet.ui=0
node4.tet.ui=0
node5.tet.ui=0

node1.tet.uo=0
node2.tet.uo=0
node3.tet.uo=0
node4.tet.uo=0
node5.tet.uo=0

# realized input
for(i in 1:length(UA.tet)){
	node1.tet.ri[i]=UA.tet[[i]]$realized.input[1]
	}
for(i in 1:length(UA.tet)){
	node2.tet.ri[i]=UA.tet[[i]]$realized.input[2]
	}
for(i in 1:length(UA.tet)){
	node3.tet.ri[i]=UA.tet[[i]]$realized.input[3]
	}
for(i in 1:length(UA.tet)){
	node4.tet.ri[i]=UA.tet[[i]]$realized.input[4]
	}
for(i in 1:length(UA.tet)){
	node5.tet.ri[i]=UA.tet[[i]]$realized.input[5]
	}

UA.tet.ri=cbind(node1.tet.ri, node2.tet.ri, node3.tet.ri, node4.tet.ri, node5.tet.ri)

# realized output
for(i in 1:length(UA.tet)){
	node1.tet.ro[i]=UA.tet[[i]]$realized.output[1]
	}
for(i in 1:length(UA.tet)){
	node2.tet.ro[i]=UA.tet[[i]]$realized.output[2]
	}
for(i in 1:length(UA.tet)){
	node3.tet.ro[i]=UA.tet[[i]]$realized.output[3]
	}
for(i in 1:length(UA.tet)){
	node4.tet.ro[i]=UA.tet[[i]]$realized.output[4]
	}
for(i in 1:length(UA.tet)){
	node5.tet.ro[i]=UA.tet[[i]]$realized.output[5]
	}

UA.tet.ro=cbind(node1.tet.ro, node2.tet.ro, node3.tet.ro, node4.tet.ro, node5.tet.ro)

# unit input
for(i in 1:length(UA.tet)){
	node1.tet.ui[i]=UA.tet[[i]]$unit.input[1]
	}
for(i in 1:length(UA.tet)){
	node2.tet.ui[i]=UA.tet[[i]]$unit.input[2]
	}
for(i in 1:length(UA.tet)){
	node3.tet.ui[i]=UA.tet[[i]]$unit.input[3]
	}
for(i in 1:length(UA.tet)){
	node4.tet.ui[i]=UA.tet[[i]]$unit.input[4]
	}
for(i in 1:length(UA.tet)){
	node5.tet.ui[i]=UA.tet[[i]]$unit.input[5]
	}

UA.tet.ui=cbind(node1.tet.ui, node2.tet.ui, node3.tet.ui, node4.tet.ui, node5.tet.ui)

# unit output
for(i in 1:length(UA.tet)){
	node1.tet.uo[i]=UA.tet[[i]]$unit.output[1]
	}
for(i in 1:length(UA.tet)){
	node2.tet.uo[i]=UA.tet[[i]]$unit.output[2]
	}
for(i in 1:length(UA.tet)){
	node3.tet.uo[i]=UA.tet[[i]]$unit.output[3]
	}
for(i in 1:length(UA.tet)){
	node4.tet.uo[i]=UA.tet[[i]]$unit.output[4]
	}
for(i in 1:length(UA.tet)){
	node5.tet.uo[i]=UA.tet[[i]]$unit.output[5]
	}

UA.tet.uo=cbind(node1.tet.uo, node2.tet.uo, node3.tet.uo, node4.tet.uo, node5.tet.uo)

dev.new(height=8, width=8)
par(mfrow=c(2,2), las=1)

bp=boxplot(UA.tet.ui, names=c('node1', 'node2', 'node3', 'node4', 'node5'), ylab='Units', main='Unit Input')
points(x=seq_along(bp$names), y=nominal.tet$unit.input, col='red', pch=15, cex=1.2)
legend('topleft', legend=c('Original Model'), pch=15, col='red', bty='n')

bp=boxplot(UA.tet.uo, names=c('node1', 'node2', 'node3', 'node4', 'node5'), ylab='Units', main='Unit Input')
points(x=seq_along(bp$names), y=nominal.tet$unit.output, col='red', pch=15, cex=1.2)

bp=boxplot(UA.tet.ri, names=c('node1', 'node2', 'node3', 'node4', 'node5'), ylab='Model Units', main='Realized Input')
points(x=seq_along(bp$names), y=nominal.tet$realized.input, col='red', pch=15, cex=1.2)

bp=boxplot(UA.tet.ro, names=c('node1', 'node2', 'node3', 'node4', 'node5'), ylab='Model Units', main='Realized Output')
points(x=seq_along(bp$names), y=nominal.tet$realized.output, col='red', pch=15, cex=1.2)

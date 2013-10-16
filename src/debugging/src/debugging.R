###Debugging

###enaR

##enaEnviron
##Re-program from scratch...?

library(enaR)
data(oyster)

                                        #dame and patten 1981 ordering
data(oyster)
dp.order <- c(1,3,4,5,6,2)
o.flow <- oyster%n%'flow'
o.flow <- o.flow[order(dp.order),order(dp.order)]
t(o.flow)
(oyster%v%'input')[order(dp.order)]
(oyster%v%'output')[order(dp.order)]
(oyster%v%'storage')[order(dp.order)]
                                        #Structure
t(enaStructure(oyster)$A)[order(dp.order),order(dp.order)]
                                        #Flow
names(enaFlow(oyster))
enaFlow(oyster)$T[order(dp.order)]
t(enaFlow(oyster)$G[order(dp.order),order(dp.order)])
t(enaFlow(oyster)$GP[order(dp.order),order(dp.order)])
t(enaFlow(oyster)$N[order(dp.order),order(dp.order)])
t(enaFlow(oyster)$NP[order(dp.order),order(dp.order)])
                                        #Storage
names(enaStorage(oyster))
enaStorage(oyster)$X[order(dp.order)]
t(enaStorage(oyster)$C[order(dp.order),order(dp.order)])
t(enaStorage(oyster)$CP[order(dp.order),order(dp.order)])
t(enaStorage(oyster)$S[order(dp.order),order(dp.order)])
t(enaStorage(oyster)$SP[order(dp.order),order(dp.order)])
t(enaStorage(oyster)$P[order(dp.order),order(dp.order)])
t(enaStorage(oyster)$PP[order(dp.order),order(dp.order)])
t(enaStorage(oyster)$Q[order(dp.order),order(dp.order)])
round(t(enaStorage(oyster)$QP[order(dp.order),order(dp.order)]),5)

t(enaStorage(oyster)$C[order(dp.order),order(dp.order)])[sign(t(enaStorage(oyster)$C[order(dp.order),order(dp.order)]))<0]
t(enaStorage(oyster)$CP[order(dp.order),order(dp.order)])[sign(t(enaStorage(oyster)$CP[order(dp.order),order(dp.order)]))<0]
t(enaStorage(oyster)$SP[order(dp.order),order(dp.order)])[sign(t(enaStorage(oyster)$SP[order(dp.order),order(dp.order)]))<0]
t(enaStorage(oyster)$P[order(dp.order),order(dp.order)])[sign(t(enaStorage(oyster)$P[order(dp.order),order(dp.order)]))<0]
t(enaStorage(oyster)$PP[order(dp.order),order(dp.order)])[sign(t(enaStorage(oyster)$PP[order(dp.order),order(dp.order)]))<0]
t(enaStorage(oyster)$Q[order(dp.order),order(dp.order)])[sign(t(enaStorage(oyster)$Q[order(dp.order),order(dp.order)]))<0]
t(enaStorage(oyster)$QP[order(dp.order),order(dp.order)])[sign(t(enaStorage(oyster)$QP[order(dp.order),order(dp.order)]))<0]

                                        #Control
round(t(enaControl(oyster)$CN)[order(dp.order),order(dp.order)],7)
round(t(enaControl(oyster)$CQ)[order(dp.order),order(dp.order)],7)
                                        #Utility
U <- enaUtility(oyster)
names(U)
t(U$D)[order(dp.order),order(dp.order)]
t(U$U)[order(dp.order),order(dp.order)]
t(U$Y)[order(dp.order),order(dp.order)]
                                        #Environ
env <- enaEnviron(oyster)
env.in <- env[[1]][order(dp.order)]
env.out <- env[[2]][order(dp.order)]
t(env.out[[1]])[order(dp.order),order(dp.order)]
t(env.in[[1]])[order(dp.order),order(dp.order)]


                                        #
x <- read.scor('../data/CFRE_HB_summer_ghost.dat')
plot(x,displaylabels=TRUE)
ssCheck(x)
hist(x%n%'flow')
env.out <- enaEnviron(x)
                                        #compare v1.03 to v2.1
v103 <- dget('./results/env_out_v1_03.Rdata')
v21 <- dget('./results/env_out_v2_1.Rdata')
v103[[1]][[1]]-v21[[1]][[1]]
v103[[1]][[1]]-t(v21[[1]][[1]])
                                        #compare v2.0 to 1.01
source('~/projects/debugging/diff/enaR101/R/environ.R')
env101 <- environ
source('~/projects/debugging/diff/enaR200/R/enaEnviron.R')
env200 <- enaEnviron
load('~/projects/enaR/data/oyster.rda')
env101(oyster)[[1]][[2]]
env200(x,type='realized')[[1]][[11]]
                                        #use dave's function that works
                                        #go back to a version that works

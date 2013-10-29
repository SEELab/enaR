###enaR checks
###11Oct2013
rm(list=ls())
args <- commandArgs(trailingOnly = TRUE)
if (length(args)!=1){args[1] <- 1}
err.tolerance <- as.numeric(args[1]) #in number of digits rounded
library(enaR)
data(oyster)
                                        #oyster model output from Fath and Borrett 2006
source('../debugging/data/fath_borrett_output.R')
                                        #
fb.order <- c(1,6,2,3,4,5)
                                        #F is in the CR orientation, so it needs to be transposed
flo <- t(F)
rownames(flo) <- colnames(flo) <- rownames(oyster%n%'flow')[fb.order]
liv <- (oyster%v%'living')[fb.order]
fb.oyster <- pack(flow=flo,input=z,output=y,
                  export=rep(0,length(liv)),respiration=y,storage=x,living=liv)
                                        #For MTI, from Ulanowicz et al. 1990
fig2a <- pack(
              flow=matrix(
                c(0,0,0,
                  40,0,0,
                  20,0,0),
                nrow=3),
              input=c(100,0,0),
              output=c(40,40,20),
              export=c(0,0,0),
              respiration=c(40,40,20),
              storage=c(0,0,0),
              living=c(TRUE,TRUE,TRUE))
                                        #Allesina and Bondavalli 2003
extended.mat <- matrix(c(0,73.161, 25.504, 24.029, 0, 
                         25.640, 0, 25.958, 23.899, 0,
                         0,0, 0, 0, 0,
                         0, 0, 0, 0, 0,
                         104.383, 0, 0, 0, 0),
                       nrow=5,byrow=T)
rownames(extended.mat) <- colnames(extended.mat) <- c('X1','X2','N+1','N+2','N+3')
em.net <- pack(flow=extended.mat[1:2,1:2],input=extended.mat[5,1:2],
               export=extended.mat[1:2,3],respiration=extended.mat[1:2,4],
               living=c(TRUE,TRUE),storage=c(0,0))
Tbal.in <- matrix(c(0,26.506,0,0,104.383,
                     78.048,0,0,0,0,
                     27.208,26.835,0,0,0,
                     25.634,24.706,0,0,0,
                     0,0,0,0,0),
                   nrow=5)
Tbal.out <- matrix(c(0,24.414,0,0,99.390,
                      74.271,0,0,0,0,
                      25.504,25.958,0,0,0,
                      24.029,23.899,0,0,0,
                      0,0,0,0,0),
                    nrow=5)
Tbal.star <- as.extended(em.net)
Tbal.avg <- 0.5 * (Tbal.in + Tbal.out)
Tbal.io <- bal(((0.5*Tbal.in) + (0.5*Tbal.star)),method='output')
Tbal.oi <- bal(((0.5*Tbal.out) + (0.5*Tbal.star)),method='input')
Tbal.avg2 <- 0.5 * (Tbal.io + Tbal.oi)
#############
#####Checks
###
set.orient('rc')
                                        #Check model
checks <- list(F=abs(t(fb.oyster%n%'flow')) - F)
checks[[2]] <- abs(fb.oyster%v%'storage' - x)
checks[[3]] <- abs(fb.oyster%v%'input' - z)
checks[[4]] <- abs(fb.oyster%v%'respiration' - y)
names(checks)[1:4] <- c('F','x','z','y')
                                        #check oyster differences from Fath and Borrett
## checks <- list(checks,abs(t((oyster%n%'flow')[fb.order,fb.order]) - F))
## checks <- list(checks,(oyster%v%'storage')[fb.order] - x)
## checks <- list(checks,(oyster%v%'input')[fb.order] - z)
## checks <- list(checks,(oyster%v%'respiration')[fb.order] - y)
                                        #fb.oyster <- balance(fb.oyster)
                                        #Flow
                                        #output in RC
checks[[5]] <- enaFlow(fb.oyster)$T - T
checks[[6]] <- t(enaFlow(fb.oyster)$G) - G
checks[[7]] <- t(enaFlow(fb.oyster)$GP) - GP
checks[[8]] <- t(enaFlow(fb.oyster)$N) - N
checks[[9]] <- t(enaFlow(fb.oyster)$NP) - NP
names(checks)[5:9] <- c('T','G','GP','N','NP')
                                        #output in school
set.orient('school')
checks[[10]] <- (enaFlow(fb.oyster)$T) - T
checks[[11]] <- (enaFlow(fb.oyster)$G) - G
checks[[12]] <- (enaFlow(fb.oyster)$GP) - GP
checks[[13]] <- (enaFlow(fb.oyster)$N) - N
checks[[14]] <- (enaFlow(fb.oyster)$NP) - NP
set.orient('rc')
names(checks)[10:14] <- c('T','G','GP','N','NP')
                                        #Storage
                                        #enaStorage(fb.oyster)
                                        #Environ
                                        #using RC orientation
set.orient('rc')
                                        #flow
                                        #output
checks[[15]] <- t(enaEnviron(fb.oyster)[[2]][[1]]) - E[,,1]
checks[[16]] <- t(enaEnviron(fb.oyster)[[2]][[2]]) - E[,,2]
                                        #input
checks[[17]] <- t(enaEnviron(fb.oyster)[[1]][[1]]) - EP[,,1]
checks[[18]] <- t(enaEnviron(fb.oyster)[[1]][[2]]) - EP[,,2]
names(checks)[15:18] <- paste('rc',c('E1','E2','EP1','EP2'),sep='_')
                                        #storage
                                        #output
                                        #t(enaEnviron(fb.oyster))[[]][[]]-SE[,,1]
                                        #input
                                        #t(enaEnviron(fb.oyster))[[]][[]]-SEP[,,1]
                                        #
                                        #using school orientation
set.orient('school')
                                        #output
checks[[19]] <- (enaEnviron(fb.oyster)[[2]][[1]]) - E[,,1]
checks[[20]] <- (enaEnviron(fb.oyster)[[2]][[2]]) - E[,,2]
                                        #input
checks[[21]] <- (enaEnviron(fb.oyster)[[1]][[1]]) - EP[,,1]
checks[[22]] <- (enaEnviron(fb.oyster)[[1]][[2]]) - EP[,,2]
set.orient('rc')
names(checks)[19:22] <- paste('school',c('E1','E2','EP1','EP2'),sep='_')
                                        #storage
                                        #output
                                        #t(enaEnviron(fb.oyster))[[]][[]]-SE[,,1]
                                        #input
                                        #t(enaEnviron(fb.oyster))[[]][[]]-SEP[,,1]
                                        #Balancing
                                        #Balance 
checks[[23]] <- as.extended(em.net)-extended.mat
names(checks)[23] <- 'as.extended'
                                        #
checks[[24]] <- round(bal(as.extended(em.net),method='input'),3) - Tbal.in
checks[[25]] <- round(bal(as.extended(em.net),method='output'),3) - Tbal.out
checks[[26]] <- (round(0.5 * (bal(as.extended(em.net),method='input') + bal(as.extended(em.net),method='output')),3) - Tbal.avg)
names(checks)[24:26] <- c('bal input','bal output','bal avg')
                                        #
checks[[27]] <- round(as.extended(balance(em.net,method='AVG')),3) - Tbal.avg
checks[[28]] <- round(as.extended(balance(em.net,method='IO')),3) - Tbal.io
checks[[29]] <- round(as.extended(balance(em.net,method='OI')),3) - Tbal.oi
checks[[30]] <- round(as.extended(balance(em.net,method='AVG2')),3) - Tbal.avg2
names(checks)[27:30] <- c('balance AVG','balance IO','balance OI','balance AVG2')
                                        #
checks[[31]] <- ssCheck(em.net) + FALSE
checks[[32]] <- ((ssCheck(balance(em.net)) == 0) + FALSE)
names(checks)[31:32] <- c('ssCheck unbalanced','ssCheck balanced')
                                        # add storage check
checks[[33]] <- enaStorage(fb.oyster)$X - x 
checks[[34]] <- t(enaStorage(fb.oyster)$C) - C
checks[[35]] <- t(enaStorage(fb.oyster)$P) - P
checks[[36]] <- t(enaStorage(fb.oyster)$S) - S
checks[[37]] <- t(enaStorage(fb.oyster)$Q) - Q
checks[[38]] <- t(enaStorage(fb.oyster)$CP) - CP
checks[[39]] <- t(enaStorage(fb.oyster)$PP) - PP
checks[[40]] <- t(enaStorage(fb.oyster)$SP) - SP
checks[[41]] <- t(enaStorage(fb.oyster)$QP) - QP
checks[[42]] <- enaStorage(fb.oyster)$dt - dt
names(checks)[33:42] <- c('X','C','P','S','Q','CP','PP','SP','QP','dt')
                                        # enaStorage(fb.oyster)$ns
                                        # add structure check
checks[[43]] <- t(enaStructure(fb.oyster)$A) - A
names(checks)[43] <- 'A'
                                        #enaStructure(fb.oyster)$ns -ns
                                        # add utility check
checks[[44]] <- t(enaUtility(fb.oyster,type='flow')$D) - D
checks[[45]] <- t(enaUtility(fb.oyster,type='flow')$U) - U
checks[[46]] <- t(enaUtility(fb.oyster,type='flow')$Y) - Y
checks[[47]] <- t(enaUtility(fb.oyster,type='storage')$DS) - DS
checks[[48]] <- t(enaUtility(fb.oyster,type='storage')$US) - US
checks[[49]] <- t(enaUtility(fb.oyster,type='storage')$YS) - YS
names(checks)[44:49] <- c('D','U','Y','DS','US','YS')
                                        # network stats
## all.ns <- get.ns(fb.oyster)
## ep.ns <-
## c(ep[1:5],NA,NA,NA,NA,NA,NA,NA,NA,ep[12],ep[6],NA,NA,ep[7],NA,NA,NA,NA,ep[15:16],ep[18],ep[19],ep[14],ep[13],ep[8:9],rep(NA,32))
## ep.ns <- round(cbind(as.numeric(all.ns),ep.ns),5)
## rownames(ep.ns) <- names(all.ns)
                                        # add ascendency check
                                        # Mondego Estuary (Zostera sp. meadows)
                                        # values are from Patricio et al. 2006
data(troModels)
checks[[50]] <- enaAscendency(troModels[[39]]) - c(1.525,16574.23,22579,39126,0.423,0.5770809)
names(checks)[50] <- 'ascendency AMI ASC OH CAP ASC.CAP OH.CAP' 
                                        # add MTI check
                                        # values from Ulanowicz et al. 1990
## enaMTI(fig2a)
## G <- 
## FP
## Q
## M
                                        # storage environs (see environs above)
check.out <- unlist(lapply(lapply(checks,round,digits=err.tolerance),function(x) all(x==0)))
if (all(check.out)){
  print('')
  print('')
  print('')
  print('All functions within error tolerance')
}else{
  print('')
  print('')
  print('')
  print('Error exceeds tolerance')
  max.err <- data.frame(max.err=unlist(lapply(checks[check.out==FALSE],max)))
  rownames(max.err) <- names(check.out)[check.out==FALSE]
  print(max.err)
}

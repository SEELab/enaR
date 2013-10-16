###enaR checks
###11Oct2013
args <- commandArgs(trailingOnly = TRUE)
if (length(args)!=1){args[1] <- 2}
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
###Checks
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
                                        #Balance
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
  print(names(check.out)[check.out==FALSE])
}

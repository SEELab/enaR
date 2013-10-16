###enaR checks
###11Oct2013

library(enaR)
data(oyster)

source('~/projects/debugging/src/new_environ.R')
source('~/projects/debugging/data/fath_borrett_output.R')
                                        #
fb.order <- c(1,6,2,3,4,5)
flo <- F
rownames(flo) <- colnames(flo) <- rownames(oyster%n%'flow')[fb.order]
liv <- (oyster%v%'living')[fb.order]
fb.oyster <- pack(flow=flo,input=z,output=y,
                  export=rep(0,length(liv)),respiration=y,storage=x,living=liv)
                                        #Check model
sum(abs(fb.oyster%n%'flow') - F)
sum(abs(fb.oyster%v%'storage' - x))
sum(abs(fb.oyster%v%'input' - z))
sum(abs(fb.oyster%v%'respiration' - y))
sum(abs(t((oyster%n%'flow')[fb.order,fb.order]) - F))
(oyster%v%'storage')[fb.order] - x
(oyster%v%'input')[fb.order] - z
(oyster%v%'respiration')[fb.order] - y
                                        #Balance
                                        #fb.oyster <- balance(fb.oyster)
                                        #Flow
enaFlow(fb.oyster)$T - T
enaFlow(fb.oyster)$G-G
enaFlow(fb.oyster)$GP-GP
enaFlow(fb.oyster)$N-N
enaFlow(fb.oyster)$NP-NP
                                        #Storage
enaStorage(fb.oyster)
                                        #Environ
new_environ(fb.oyster)$E[,,1]-E[,,1]
new_environ(fb.oyster)$E[,,2]-E[,,2]
new_environ(fb.oyster)$EP[,,1]-EP[,,1]
new_environ(fb.oyster)$EP[,,2]-EP[,,2]
new_environ(fb.oyster)$SE[,,1]-SE[,,1]
new_environ(fb.oyster)$SE[,,2]-SE[,,2]
new_environ(fb.oyster)$SEP[,,1]-SEP[,,1]
new_environ(fb.oyster)$SEP[,,2]-SEP[,,2]

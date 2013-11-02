# enaR_netstas.R
#
# This script creates a figure for the enaR MEE paper that summarizes
# the range of a selected number of network statistics.
#
# Borrett, August, 14, 2013
# uses enaR version 2.0
# ---------------------------------------------------------

# preliminaries
rm(list=ls())  # clear working memory
library(enaR)  # load enaR library
setwd("/Users/borretts/Dropbox/RENA/manuscript/mee13/bin")

# load data & create one model list
data(troModels)
data(bgcModels)
model.list=c(troModels,bgcModels)

# balance models, using AVG2 algorithm
m.list <- lapply(model.list, force.balance)

# get network statistics
ns.list <- lapply(m.list,get.ns) # returns resutls as a list
ns <- do.call(rbind, ns.list)   # reformat list as a dataframe

# -- Select Statistics for Analysis --
# n, C, LD, ppr, FCI, APL, IFI, ID.F, HMG.O, AMP.O, AMI, ASC, ASC.CAP, synergism.F, and mutualis.F
 sel.ns <- c(1,3,4,6,18, 17, 21, 26, 28, 34, 38, 58, 59)
names(ns)[sel.ns]
k=length(sel.ns)  # count number of selected network statistics

# -- create figure --
fn <- "../figures/ns_dist.pdf"  # file name
#postscript(fn,width=6,height=7, #7  # open eps file
#postscript(fn,width=6,height=12,  # open eps file
#           onefile=TRUE,
#           horizontal=FALSE,
#           paper="special")
pdf(fn,width=5.5,height=6,family="Times",pointsize=10)  # scaled for 2 columns in MEE
opar <- par(las=1,mar=c(1,0,0,0),
            oma=c(0,9,3,13),mfrow=c(k,1),cex=1)
for(i in 1:k){
  x=ns[,sel.ns[i]]
hist(x,main="",xlab="",axes=FALSE,ylab="",breaks=20,col="black")
mtext(names(ns)[sel.ns[i]],side=2,las=2,line=8,adj=0)
mtext(round(min(x),2),side=2,las=2,line=0.5,adj=1)
mtext(round(max(x),2),side=4,las=2,line=3,adj=1)
mtext(round(median(x),2),side=4,las=2,line=6,adj=1)
mtext(round(mean(x),2),side=4,las=2,line=9,adj=1)
mtext(round(sd(x)/mean(x),2), side=4,las=2,line=12,adj=1)
if(i ==1){
  mtext("Statistic",side=2,las=2,line=8,adj=0,padj=-4)
  mtext("Min",side=2,las=2,line=0.5,adj=1,padj=-4)
  mtext("Max",side=4,las=2,line=3,adj=1,padj=-4)
  mtext("Median",side=4,las=2,line=6,adj=1,padj=-4)
  mtext("Mean",side=4,las=2,line=9,adj=1,padj=-4)
  mtext("CV",side=4,las=2,line=12,adj=1,padj=-4)
  mtext("Distribution",side=3,line=1.25)
}
}
dev.off()  # close eps file
cmd <- paste("open ",fn)
system(cmd) # open eps file (on mac)

rm(opar)


########
# ALL NET STATS
################
 sel.ns <- c(1:62)
names(ns)[sel.ns]
k=length(sel.ns)  # count number of selected network statistics
my.cex=0.85

# -- create figure --
fn <- "../figures/ns_dist_all.pdf"  # file name
pdf(fn,width=6,height=20)
opar <- par(las=1,mar=c(1,1,1,1),oma=c(1,15,3,20),mfrow=c(k,1))
#opar <- par(las=1,oma=c(1,1,1,1),mar=c(1,15,3,20),mfrow=c(k,1))
for(i in 1:k){
  x=ns[,sel.ns[i]]
hist(x,main="",xlab="",axes=FALSE,ylab="",breaks=20,col="black")
mtext(names(ns)[sel.ns[i]],side=2,las=2,line=12,adj=0,cex=my.cex)
mtext(round(min(x),2),side=2,las=2,line=0.5,adj=1,cex=my.cex)
mtext(round(max(x),2),side=4,las=2,line=4,adj=1,cex=my.cex)
mtext(round(median(x),2),side=4,las=2,line=9,adj=1,cex=my.cex)
mtext(round(mean(x),2),side=4,las=2,line=13,adj=1,cex=my.cex)
mtext(round(sd(x)/mean(x),2), side=4,las=2,line=17,adj=1,cex=my.cex)
if(i ==1){
  mtext("Statistic",side=2,las=2,line=12,adj=0,padj=-4,cex=my.cex)
  mtext("Min",side=2,las=2,line=0.5,adj=1,padj=-4,cex=my.cex)
  mtext("Max",side=4,las=2,line=4,adj=1,padj=-4,cex=my.cex)
  mtext("Median",side=4,las=2,line=9,adj=1,padj=-4,cex=my.cex)
  mtext("Mean",side=4,las=2,line=13,adj=1,padj=-4,cex=my.cex)
  mtext("CV",side=4,las=2,line=17,adj=1,padj=-4,cex=my.cex)
  mtext("Distribution",side=3,line=2.6,cex=my.cex)
}
}
rm(opar)
dev.off()  # close eps file
cmd <- paste("open ",fn)
system(cmd) # open eps file (on mac)


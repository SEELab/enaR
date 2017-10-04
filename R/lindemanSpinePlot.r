#' plot.lindeman
#' INPUT = network object
#' OUTPUT = plot of the lindeman spine
#'
#' Dr. Ulrike Schückel, Landesbetrieb für Küstenschutz, Nationalpark und Meeresschutz Schleswig-Holstein, Nationalparkverwaltung, Schlossgarten 1, 25832 Tönning
#'
#' Fall 2016
#' ---------------------------------------------------



#' lindemanSpinePlot
#'
#'
#' U. Schuckel | Fall 2016 ---------------------------------------------------
#' plot.lindeman ---  INPUT = network object OUTPUT = plot of lindeman spine
#'
#' Applies the enaTroAgg function and creates a plot of the Lindeman Spine
#'
#' @param x a network object.  This includes all weighted flows into and out of
#' each node.
#' @return
#'
#' @author Ulrike Schuckel,  Stuart R. Borrett
#' @seealso
#' \code{\link{enaTroAgg}
#' @references
#'
#' Style of the plot according to Baird et al., 2004, 2007
#'
#' @examples
#'
#'
#' @importFrom MASS ginv
#' @import network
#' @export plot.lindeman
#' @import network


plot.lindeman <- function(x = 'model'){
  if (class(x) != 'network'){warning('x is not a network class object')}
  #  Check for balancing
  if (balance.override){}else{
    if (any(list.network.attributes(x) == 'balanced') == FALSE){x%n%'balanced' <- ssCheck(x)}
    if (x%n%'balanced' == FALSE){warning('Model is not balanced'); stop}
  }
    enatroagg <- enaTroAgg(x)
#count number of compartments for LS
ntl = length(enatroagg$GC[enatroagg$GC >0])

#define corners of first 2 rectangles
x1d = 10 ; y1d = 15 ; x2d = 15 ; y2d = 20 # detritus pool
x1 = 10 ; y1 = 25 ; x2 = 15 ; y2 = 30 # compartment I
x1c = 10 ; y1c = 5 ; x2c = 15 ; y2c = 10 # new lindeman I+D

#draw an empty plot
plot(c(0,(ntl+1)*10),c(0,35),type='n',xlab='',ylab='',axes=FALSE)

# Detritus rectangle and its label
rect(x1d,y1d,x2d,y2d,lwd=3)
text(x1d+2.4,y2d-2,labels='D',cex=1.3)
arrows(x1d-5,y1d+2.5,x1d,y1d+2.5,lwd=2,length = 0.2,angle=0)#arrow Detrital input
polygon(c(x1d,x1d-1,x1d-1),c(y1d+2.5,y1d+2.9,y1d+2.1),col='black',lwd=2)
text(x1d-3,y1d+3.5,labels=round(enatroagg$ns[2],2),cex=0.95)#value Detrital Input
arrows(x2d,y2d,x2+5,y2d+5,lwd=2,angle=0,length = 0.2)# arrow for Detritivory from D to II
polygon(c(x2d+5,x2d+4,x2d+4.5),c(y2d+5,y2d+4.5,y2d+4),col='black',lwd=2)
text(x2d+2.5,y1-3,labels=round(enatroagg$ns[1],2),cex=0.95,pos=4)# value Detritivory


#draw all other rectangles
for(comp in 1:ntl) {
rect(x1,y1,x2,y2,lwd=3)#draw rectables
text(x1+2.4,y2-2,labels=as.roman(comp),cex=1.3)#label compartments

#add efficiency to all boxes but last compartment
if(comp <= ntl-1){
if(comp == 2){
ef = (enatroagg$GC[comp+1]*100)/sum(enatroagg$GC[comp]+enatroagg$ns[1])#eff trophic level 2
}else{ef = enatroagg$GC[comp+1]*100/enatroagg$GC[comp]} #efficiency other compartments
if(ef >= 0.01){text(x1+2.4,y2-3.5,labels=paste(round(ef,2),'%',sep=''))
}else{text(x1+2.4,y2-3.5,labels=paste(sprintf('%1.1e',ef),'%',sep=''))}
}


#exogenous input
if(comp == 1){
arrows(x1d,y1d+2.5,x1d-5,y1d+2.5,lwd=2,length = 0.1,angle=140)
arrows(x1,y1+2.5,x1-5,y1+2.5,lwd=2,length = 0.1,angle=140)
}

#Import
if(!is.null(enatroagg$CI)){
if(comp > 1){
ci = enatroagg$CI[comp]
if(ci > 0){
arrows(x1-2,y2+4,x1,y2,lwd=2,length = 0.2,angle=0)
arrows(x1,y2,x1-2,y2+4,lwd=2,length = 0.1,angle=140)
polygon(c(x1,x1-0.75,x1),c(y2,y2+0.5,y2+1),col='black',lwd=2)
if(ci >= 0.01){text(x1-1.5,y2+4.9,labels=round(enatroagg$CI[comp],2),cex=0.95,pos=2)
}else{text(x1-1.5,y2+4.9,labels=sprintf('%1.1e',ci),pos=2,cex=0.95)}
}}}


#Grazing chain
arrows(x1-5,y1+2.5,x1,y1+2.5,lwd=2,length = 0.2,angle=0)#draw arrows flow between rectangles
polygon(c(x1,x1-1,x1-1),c(y1+2.5,y1+2.8,y1+2.2),col='black',lwd=2)
gc = enatroagg$GC[comp]#back to detrital pool
if(gc >= 0.01){text(x1-3,y1+3.5,labels=format(round(gc,2),nsmall=2),cex=0.95)
}else{text(x1-3,y1+3.5,labels=sprintf('%1.1e',gc),cex=0.95)}

#Export
arrows(x2,y2,x2+1.5,y2+1.5,lwd=2,angle=0,length = 0.2)# oblique arrow for exports
polygon(c(x2+1.5,x2+0.5,x2+1),c(y2+1.5,y2+1,y2+0.5),col='white',lwd=2)
ex = enatroagg$CE[comp]#back to detrital pool
if(ex >= 0.01){text(x2+2.6,y2+2.2,labels=format(round(ex,2),nsmall=2),cex=0.95,pos=2)
}else{text(x2+2.6,y2+2.2,labels=sprintf('%1.1e',ex),pos=2,cex=0.95)}

#Detrital Pool
if( comp ==1){arrows(x1+2.5,y1,x1+2.5,y1-5,lwd=2,angle=0)
polygon(c(x1+2.5,x1+2,x1+3),c(y1-5,y1-4,y1-4),col='black',lwd=2)}#arrow from I to D
else{arrows(x1+2.5,y1,x1+2.5,y1-7.5,lwd=2,length = 0.2,angle=0)
polygon(c(x1+2.5,x1+2,x1+3),c(y1-7.5,y1-6.5,y1-6.5),col='black',lwd=2)}#other arrows back to D
dp = enatroagg$RDP[comp]#back to detrital pool
if(dp >= 0.01){text(x1+2.55,y1-3,labels=format(round(dp,2),nsmall=2),pos=4,cex=0.95)
}else{text(x1+2.55,y1-3,labels=sprintf('%1.1e',dp),pos=4,cex=0.95)}

# Respiration
arrows(x1+3.5,y1,x1+3.5,y1-1.2,lwd=2,angle=90,length=0.1)
arrows(x1+3.5,y1,x1+3.5,y1-1.5,lwd=2,angle=90,length=0.05)
re = enatroagg$CR[comp]#respiration
if(re >= 0.01){text(x1+3.8,y1-1,labels=format(round(re,2),nsmall=2),pos=4,cex=0.95)
}else{text(x1+3.8,y1-1,labels=sprintf('%1.1e',re),pos=4,cex=0.95)}

#move coordinates for next rectangle
x1=x1+10 ; x2=x2+10
}

arrows(x1-7.5,y1d+2.5,x2d,y1d+2.5,lwd=2,angle=0)#arrow connecting horizontally to Detrital pool
polygon(c(x2d,x2d+1,x2d+1),c(y1d+2.5,y1d+2.9,y1d+2.1),col='black',lwd=2)
text(x2d+1.5,y1d+3.5,lab=format(round(sum(enatroagg$RDP[2:ntl]),2),nsmall=2),pos=4,cex=0.95)#sum of II-... back to detritus pool


#################################################################################################
# add LS merged I+D
#draw all other rectangles
ntl = length(enatroagg$GC[enatroagg$LS >0])

for(comp in 1:ntl) {
rect(x1c,y1c,x2c,y2c,lwd=3)#draw rectables
if(comp==1){text(x1c+2.4,y2c-2,labels='I + D',cex=1.3)
}else{text(x1c+2.4,y2c-2,labels=as.roman(comp),cex=1.3)}#label compartments
#add efficiency to all boxes but last compartment
if(comp <= ntl-1){
ef = (enatroagg$TE[comp]*100)#efficiency
if(ef >= 0.01){text(x1c+2.4,y2c-3.5,labels=paste(round(ef,2),'%',sep=''))
}else{text(x1c+2.4,y2c-3.5,labels=paste(sprintf('%1.1e',ef),'%',sep=''))}
}

#exogenous input 'haken'
if(comp == 1){
arrows(x1c,y1c+2.5,x1c-5,y1c+2.5,lwd=2,length = 0.1,angle=140)
}

#Import
if(!is.null(enatroagg$CI)){
if(comp > 1){
ci = enatroagg$CI[comp]
if(ci > 0){
arrows(x1c-2,y2c+4,x1c,y2c,lwd=2,length = 0.2,angle=0)
arrows(x1c,y2c,x1c-2,y2c+4,lwd=2,length = 0.1,angle=140)
polygon(c(x1c,x1c-0.75,x1c),c(y2c,y2c+0.5,y2c+1),col='black',lwd=2)
if(ci >= 0.01){text(x1c-1.5,y2c+4.9,labels=round(enatroagg$CI[comp],2),cex=0.95,pos=2)
}else{text(x1c-1.5,y2c+4.9,labels=sprintf('%1.1e',ci),cex=0.95,pos=2)}
}}}



#Grazing chain
arrows(x1c-5,y1c+2.5,x1c,y1c+2.5,lwd=2,length = 0.2,angle=0)#draw arrows flow between rectangles
polygon(c(x1c,x1c-1,x1c-1),c(y1c+2.5,y1c+2.9,y1c+2.1),col='black',lwd=2)
gc = enatroagg$LS[comp]# GC value

if(comp==1){text(x1c-5,y1c+4,labels=format(round(gc,2),nsmall=2),font=2,cex=1.0,pos=2)#total input to I+D, bold
text(x1c-3,y1c+3.5,labels=round(enatroagg$ns[2]+enatroagg$GC[comp],2),cex=0.95)
}else{if(gc >= 0.01){text(x1c-3,y1c+3.5,labels=format(round(gc,2),nsmall=2),cex=0.95)
}else{text(x1c-3,y1c+3.5,labels=sprintf('%1.1e',gc),cex=0.95)}
}
#Export
arrows(x2c,y2c,x2c+1.5,y2c+1.5,lwd=2,angle=0,length = 0.2)# oblique arrow for exports
polygon(c(x2c+1.5,x2c+0.5,x2c+1),c(y2c+1.5,y2c+1,y2c+0.5),col='white',lwd=2)
ex = enatroagg$CE[comp]#back to detrital pool
if(comp==1){text(x2c+2.6,y2c+2.2,labels=round(sum(energetics[c(primprod,nonliving),'output'],na.rm=T)  ###note: necessary to know where the function can find output values belonging to primprod uand non-living
,2),cex=0.95,pos=2)
arrows(x1c+2.5,y1c,x1c+2.5,y1c-2,lwd=2,angle=90,length=0.0)
arrows(x1c+2.5,y1c-2,x1c+0.5,y1c-2,lwd=2,angle=90,length=0.0)
arrows(x1c+0.5,y1c-2,x1c+0.5,y1c,lwd=2,angle=90,length=0.0)
polygon(c(x1c+0.5,x1c,x1c+1),c(y1c,y1c-1,y1c-1),col='black',lwd=2)

}else{if(ex >= 0.01){text(x2c+2.6,y2c+2.2,labels=format(round(ex,2),nsmall=2),cex=0.95,pos=2)
}else{text(x2c+2.6,y2c+2.2,labels=sprintf('%1.1e',ex),pos=2,cex=0.95)}
}
#Detrital Pool
dp = enatroagg$RDP[comp]#back to detrital pool
if( comp ==1){arrows(5,y1c+1.7,x1c,y1c+1.7,lwd=2,angle=0)
polygon(c(x1c,x1c-1,x1c-1),c(y1c+1.7,y1c+2,y1c+1.4),col='black',lwd=2)
text(x1c,y1c-3,labels=format(round(dp,2),nsmall=2),pos=4,cex=0.95)
}else{arrows(x1c+2.5,y1c,x1c+2.5,y1c-5,lwd=2,length = 0.2,angle=0)
polygon(c(x1c+2.5,x1c+2,x1c+3),c(y1c-5,y1c-4,y1c-4),col='black',lwd=2)#other arrows back to D
if(dp >= 0.01){text(x1c+2.55,y1c-3,labels=format(round(dp,2),nsmall=2),pos=4,cex=0.95)
}else{text(x1c+2.55,y1c-3,labels=sprintf('%1.1e',dp),pos=4,cex=0.95)}}

# Respiration
arrows(x1c+3.5,y1c,x1c+3.5,y1c-1.2,lwd=2,angle=90,length=0.1)
arrows(x1c+3.5,y1c,x1c+3.5,y1c-1.5,lwd=2,angle=90,length=0.05)
re = enatroagg$CR[comp]#respiration
if(re >= 0.01){text(x1c+3.8,y1c-1,labels=format(round(re,2),nsmall=2),pos=4,cex=0.95)
}else{text(x1c+3.8,y1c-1,labels=sprintf('%1.1e',re),pos=4,cex=0.95)}

x1c=x1c+10 ; x2c=x2c+10
}
arrows(x1c-7.5,y1c-5,5,y1c-5,lwd=2,angle=0)#arrow connecting horizontally to Detrital pool
arrows(5,y1c-5,5,y1c+1.7,lwd=2,angle=0)#arrow connecting horizontally to Detrital pool
#polygon(c(x2d,x2d+1,x2d+1),c(y1d+1.5,y1d+2.9,y1d+2.1),col='black',lwd=2)
text(5,y1c+0.8,lab=format(round(sum(enatroagg$RDP[2:ntl]),2),nsmall=2),pos=4,cex=0.95)#sum of II-... back to detritus pool


}
##end of function
##########################################################################################################

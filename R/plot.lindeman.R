#' plot.lindeman
#' INPUT = network object
#' OUTPUT = plot of the lindeman spine
#'
#' Dr. Ulrike Schückel, Landesbetrieb für Küstenschutz, Nationalpark
#' und Meeresschutz Schleswig-Holstein, Nationalparkverwaltung,
#' Schlossgarten 1, 25832 Tönning
#'
#' Applies the enaTroAgg function and creates a plot of the Lindeman Spine
#'
#' @param x an ENA network object.
#' @param enatroagg the resutls of the enaTroAgg function applied to the model
#' @param primprod a vector of the nodes that are primary producers
#' @param type switches beteween two types of plots: 1 = I and D are separate, 2 = I and D are combined
#'
#'
#' @author Ulrike Schuckel,  Stuart R. Borrett
#' @seealso
#' \code{\link{enaTroAgg}}
#' @references
#'
#' Style of the plot according to Baird et al., 2004, 2007
#'
#' @examples
#' \dontrun{
#' data(enaModels)
#' model <- enaModels[[8]]
#' plot.lindeman(model)
#' }
#'
#' @export plot.lindeman
#' @import network
#' @importFrom MASS ginv
#' @importFrom graphics rect text arrows polygon
#' @importFrom utils as.roman

plot.lindeman <- function(x = 'model', enatroagg='troagg', primprod, type = 1){
    if (class(x) != 'network'){warning('x is not a network class object')}

    # define primary producers & nonliving (by name)
    u <- unpack(x)
    vn <- x%v%'vertex.names'
    nonliving = which(u$living == FALSE)
    ns <- as.data.frame(enatroagg$ns)

    ## apply Trophic Aggregation
    if (enatroagg == "troagg"){
        enatroagg <- enaTroAgg(x)
    }else{}

    ## primprod?
    if (exists("primprod")){
        warning("Please supply a vector of primary producers as 'primprod'.")
    }else{}

    ## count number of compartments for LS
    ntl = length(enatroagg$GC[enatroagg$GC >0])

    # text scaling  -  to make nicer plots when NTL is large
    if(ntl <= 5){
        lvl.1 = 1.3
        lvl.2 = 1
        lvl.3 = 0.95
        lvl.4 = 1
    } else {
        lvl.1 = 1
        lvl.2 = 0.95
        lvl.3 = 0.85
        lvl.4 = 0.85

    }


  # define corners of first 2 rectangles
    x1  = 10 ; y1  = 15 ; x2  = 15 ; y2  = 20 # compartment I
    x1d = 10 ; y1d =  5 ; x2d = 15 ; y2d = 10 # detritus pool

#    x1  = 10 ; y1  = 25 ; x2  = 15 ; y2  = 30 # compartment I
#    x1d = 10 ; y1d = 15 ; x2d = 15 ; y2d = 20 # detritus pool

    x1c = 10 ; y1c = 5 ;  x2c = 15 ; y2c = 10 # new lindeman I+D

    #draw an empty plot
    opar <- par(oma = c(1,1,1,1), mar = c(0,0,0,0))
    plot(c(0,(ntl+1)*10),c(0,30),type='n',xlab='',ylab='',axes=FALSE)

if(type == 1){

# Detritus rectangle and its label
    rect(x1d,y1d,x2d,y2d,lwd=3, border = "grey75")
    text(x1d+2.4,y2d-2,labels='D',cex=lvl.1)
    arrows(x1d-5,y1d+2.5,x1d,y1d+2.5,lwd=2,length = 0.2,angle=0)#arrow Detrital input
    polygon(c(x1d,x1d-1,x1d-1),c(y1d+2.5,y1d+2.9,y1d+2.1),col='black',lwd=2)
    text(x1d-3,y1d+3.5,labels=round(ns$DetritalInput, 2),cex=lvl.3)#value Detrital Input
    arrows(x2d,y2d,x2+5,y2d+5,lwd=2,angle=0,length = 0.2)# arrow for Detritivory from D to II
    polygon(c(x2d+5,x2d+4,x2d+4.5),c(y2d+5,y2d+4.5,y2d+4),col='black',lwd=2)
    text(x2d+2.5,y1-3,labels=round(ns$Detritivory, 2),cex=lvl.3,pos=4)# value Detritivory


#draw all other rectangles
    for(comp in 1:ntl) {
        rect(x1,y1,x2,y2,lwd=3, border = "grey75")#draw rectables
        text(x1+2.4,y2-2, labels = as.roman(comp), cex=lvl.1)#label compartments

#add efficiency to all boxes but last compartment
        if(comp <= ntl-1){
            if(comp == 2){
                ef = (enatroagg$GC[comp+1]*100)/sum(enatroagg$GC[comp]+ns$Detritivory)#eff trophic level 2
            }else{
                ef = enatroagg$GC[comp+1]*100/enatroagg$GC[comp]
            } #efficiency other compartments
            if(ef >= 0.01){
                text(x1+2.4,y2-3.5,labels=paste(round(ef,2),'%',sep=''), cex = lvl.4)
            }else{
                text(x1+2.4,y2-3.5,labels=paste(sprintf('%1.1e',ef),'%',sep=''), cex = lvl.4)
            }
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
                    if(ci >= 0.01){text(x1-1.5,y2+4.9,labels=round(enatroagg$CI[comp],2),cex = lvl.3,pos=2)
                               }else{text(x1-1.5,y2+4.9,labels=sprintf('%1.1e',ci),pos=2,cex = lvl.3)}
                }}}


#Grazing chain
        arrows(x1-5,y1+2.5,x1,y1+2.5,lwd=2,length = 0.2,angle=0)#draw arrows flow between rectangles
        polygon(c(x1,x1-1,x1-1),c(y1+2.5,y1+2.8,y1+2.2),col='black',lwd=2)
        gc = enatroagg$GC[comp]#back to detrital pool
        if(gc >= 0.01){
            text(x1-3,y1+3.5,labels=format(round(gc,2),nsmall=2),cex=lvl.4)
        }else{
            text(x1-3,y1+3.5,labels=sprintf('%1.1e',gc),cex=lvl.4)
        }

#Export
        arrows(x2,y2,x2+1.5,y2+1.5,lwd=2,angle=0,length = 0.2)# oblique arrow for exports
        polygon(c(x2+1.5,x2+0.5,x2+1),c(y2+1.5,y2+1,y2+0.5),col='white',lwd=2)
        ex = enatroagg$CE[comp]  #back to detrital pool
        if(ex >= 0.01){
            text(x2+2.6,y2+2.2,labels=format(round(ex,2),nsmall=2),cex = lvl.3,pos=2)
        } else{
            text(x2+2.6,y2+2.2,labels=sprintf('%1.1e',ex),pos=2,cex = lvl.3)
        }

#Detrital Pool
        if( comp ==1){
            arrows(x1+2.5,y1,x1+2.5,y1-5,lwd=2,angle=0)
            polygon(c(x1+2.5,x1+2,x1+3),c(y1-5,y1-4,y1-4),col='black',lwd=2)
        } #arrow from I to D
        else{
            arrows(x1+2.5,y1,x1+2.5,y1-7.5,lwd=2,length = 0.2,angle=0)
            polygon(c(x1+2.5,x1+2,x1+3),c(y1-7.5,y1-6.5,y1-6.5),col='black',lwd=2)
        }#other arrows back to D
        dp = enatroagg$RDP[comp]#back to detrital pool
        if(comp == 1){
            if(dp >= 0.01){
                text(x1+2.55,y1-3,labels=format(round(dp,2),nsmall=2), pos=2, cex = lvl.3)
            } else {
                text(x1+2.55,y1-3,labels=sprintf('%1.1e',dp), pos=4, cex = lvl.3)
            }
        } else {
            if(dp >= 0.01){
                text(x1+2.55,y1-3,labels=format(round(dp,2),nsmall=2), pos=4, cex = lvl.3)
            }else{
                text(x1+2.55,y1-3,labels=sprintf('%1.1e',dp), pos=4, cex = lvl.3)
                }
        }

                                        # Respiration
        arrows(x1+3.5,y1,x1+3.5,y1-1.2,lwd=2,angle=90,length=0.1)
        arrows(x1+3.5,y1,x1+3.5,y1-1.5,lwd=2,angle=90,length=0.05)
        re = enatroagg$CR[comp]#respiration
        if(re >= 0.01){text(x1+3.8,y1-1,labels=format(round(re,2),nsmall=2),pos=4,cex = lvl.3)
                   }else{text(x1+3.8,y1-1,labels=sprintf('%1.1e',re),pos=4,cex = lvl.3)}

                                        #move coordinates for next rectangle
        x1=x1+10 ; x2=x2+10
    }

    arrows(x1-7.5,y1d+2.5,x2d,y1d+2.5,lwd=2,angle=0)#arrow connecting horizontally to Detrital pool
    polygon(c(x2d,x2d+1,x2d+1),c(y1d+2.5,y1d+2.9,y1d+2.1),col='black',lwd=2)
    text(x2d+1.5,y1d+3.5,lab=format(round(sum(enatroagg$RDP[2:ntl]),2),nsmall=2),pos=4,cex = lvl.3)#sum of II-... back to detritus pool

} else {

# UNCHANGED (Oct. 4, 2017)

#################################################################################################
# add LS merged I+D
#draw all other rectangles
    ntl = length(enatroagg$GC[enatroagg$LS >0])

    for(comp in 1:ntl) {
        rect(x1c, y1c, x2c, y2c, lwd=3) #draw rectables
        if(comp==1){
            text(x1c+2.4,y2c-2,labels='I + D',cex=1.3)
        }else{
            text(x1c+2.4,y2c-2,labels=as.roman(comp),cex=1.3)
        }#label compartments
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
                    if(ci >= 0.01){text(x1c-1.5,y2c+4.9,labels=round(enatroagg$CI[comp],2),cex = lvl.3,pos=2)
                               }else{text(x1c-1.5,y2c+4.9,labels=sprintf('%1.1e',ci),cex = lvl.3,pos=2)}
                }}}



#Grazing chain
        arrows(x1c-5,y1c+2.5,x1c,y1c+2.5,lwd=2,length = 0.2,angle=0)#draw arrows flow between rectangles
        polygon(c(x1c,x1c-1,x1c-1),c(y1c+2.5,y1c+2.9,y1c+2.1),col='black',lwd=2)
        gc = enatroagg$LS[comp]# GC value

        if(comp==1){
            text(x1c-5,y1c+4,labels=format(round(gc,2),nsmall=2),font=2,cex=1.0,pos=2)#total input to I+D, bold
            text(x1c-3,y1c+3.5,labels=round(ns$DetritalInput + enatroagg$GC[comp],2),cex=0.75)
        }else{
            if(gc >= 0.01){
                text(x1c-3,y1c+3.5,labels=format(round(gc,2),nsmall=2),cex=0.75)
            } else {
                text(x1c-3,y1c+3.5,labels=sprintf('%1.1e',gc),cex=0.75)
            }
        }
#Export
        arrows(x2c,y2c,x2c+1.5,y2c+1.5,lwd=2,angle=0,length = 0.2)# oblique arrow for exports
        polygon(c(x2c+1.5,x2c+0.5,x2c+1),c(y2c+1.5,y2c+1,y2c+0.5),col='white',lwd=2)
        ex = enatroagg$CE[comp]#back to detrital pool

        if(comp==1){
            text(x2c+2.6,y2c+2.2,
                 labels=round(sum(u$output[c(primprod,nonliving)], na.rm = T), 2),
                 cex = lvl.3, pos=2)
            arrows(x1c+2.5,y1c,x1c+2.5,y1c-2,lwd=2,angle=90,length=0.0)
            arrows(x1c+2.5,y1c-2,x1c+0.5,y1c-2,lwd=2,angle=90,length=0.0)
            arrows(x1c+0.5,y1c-2,x1c+0.5,y1c,lwd=2,angle=90,length=0.0)
            polygon(c(x1c+0.5,x1c,x1c+1),c(y1c,y1c-1,y1c-1),col='black',lwd=2)

        }else{
            if(ex >= 0.01){
                text(x2c+2.6,y2c+2.2,labels=format(round(ex,2),nsmall=2),cex = lvl.3,pos=2)
            }else{
                text(x2c+2.6,y2c+2.2,labels=sprintf('%1.1e',ex),pos=2,cex = lvl.3)
            }
        }
#Detrital Pool
        dp = enatroagg$RDP[comp]#back to detrital pool
        if( comp ==1){arrows(5,y1c+1.7,x1c,y1c+1.7,lwd=2,angle=0)
                      polygon(c(x1c,x1c-1,x1c-1),c(y1c+1.7,y1c+2,y1c+1.4),col='black',lwd=2)
                      text(x1c,y1c-3,labels=format(round(dp,2),nsmall=2),pos=4,cex = lvl.3)
                  }else{arrows(x1c+2.5,y1c,x1c+2.5,y1c-5,lwd=2,length = 0.2,angle=0)
                        polygon(c(x1c+2.5,x1c+2,x1c+3),c(y1c-5,y1c-4,y1c-4),col='black',lwd=2)#other arrows back to D
                        if(dp >= 0.01){text(x1c+2.55,y1c-3,labels=format(round(dp,2),nsmall=2),pos=4,cex = lvl.3)
                                   }else{text(x1c+2.55,y1c-3,labels=sprintf('%1.1e',dp),pos=4,cex = lvl.3)}}

# Respiration
        arrows(x1c+3.5,y1c,x1c+3.5,y1c-1.2,lwd=2,angle=90,length=0.1)
        arrows(x1c+3.5,y1c,x1c+3.5,y1c-1.5,lwd=2,angle=90,length=0.05)
        re = enatroagg$CR[comp]#respiration
        if(re >= 0.01){text(x1c+3.8,y1c-1,labels=format(round(re,2),nsmall=2),pos=4,cex = lvl.3)
                   }else{text(x1c+3.8,y1c-1,labels=sprintf('%1.1e',re),pos=4,cex = lvl.3)}

        x1c=x1c+10 ; x2c=x2c+10
    }
    arrows(x1c-7.5,y1c-5,5,y1c-5,lwd=2,angle=0)#arrow connecting horizontally to Detrital pool
    arrows(5,y1c-5,5,y1c+1.7,lwd=2,angle=0)#arrow connecting horizontally to Detrital pool
                                        #polygon(c(x2d,x2d+1,x2d+1),c(y1d+1.5,y1d+2.9,y1d+2.1),col='black',lwd=2)
    text(5,y1c+0.8,lab=format(round(sum(enatroagg$RDP[2:ntl]),2),nsmall=2),pos=4,cex = lvl.3)#sum of II-... back to detritus pool
}
    rm(opar)
}
##end of function
##########################################################################################################

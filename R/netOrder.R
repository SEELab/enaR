### Function to order the nodes in a Network in enaR
### Singh P. | July 2014
### -----------------------------------------

netOrder <- function(x,ordr=0) {
    if (class(x) != "network") {
        stop("x is not a network class object")
    }
                                        # Load Initials
    flow <- x %n% "flow"
    input <- x %v% "input"
    resp <- x %v% "respiration"
    export <- x %v% "export"
    output <- x %v% "output"
    storage <- x %v% "storage"
    living <- x %v% "living"
    N <- length(living)
                                        # Determine Order (ordr)

    if(identical(ordr,0)==TRUE) {
        ordr<-rep(0,N)
        liv1<-which(living==TRUE)
        liv2<-which(living==FALSE)
        ordr<-c(liv1,liv2)
    }



                                        # Rearrange Network Characteristics
    living <- living[ordr]
    flow  <- flow[ordr,ordr]
    export <- export[ordr]
    resp <- resp[ordr]
    storage <- storage[ordr]
    output <- output[ordr]
    input <- input[ordr]


                                        # Modify Network
    x %n% "flow" <- flow
    x %v% "input" <- input
    x %v% "respiration" <- resp
    x %v% "export" <- export
    x %v% "output" <- output
    x %v% "storage" <- storage
    x %v% "living" <- living


                                        # Return the ordered network
    return(x)

}

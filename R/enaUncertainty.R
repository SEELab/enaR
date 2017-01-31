#' Uncertainty Analysis for Ecological Networks
#' 
#' Applies linear inverse modeling uncertainty analysis from the limSolve
#' package (Van den Meershe, 2009; Soetaert, 2009) to flow networks.  Function
#' generates a specified number of network models with edge weights that meet
#' specified constraints.  It returns a list of network objects that can be
#' used as the input to other functions in the enaR package.
#' 
#' 
#' @param x a network object.  This includes all weighted flows into and out of
#' each node.
#' @param p.err If present, provides a single value representing the percentage
#' of allowable variation to be assigned to all network edges.
#' @param Fu.sd Matrix of size n by n with values corresponding to each network
#' edge.  Values specify the variation (plus and minus) to be applied to each
#' edge.  Requires zu.sd and yu.sd.
#' @param zu.sd Vector of length n that specifies variation (plus and minus) to
#' be applied to each network input.
#' @param yu.sd Vector of length n that specifies variation (plus and minus) to
#' be applied to each network output.
#' @param Fu.bot Matrix of size n by n with values corresponding to each
#' network edge.  Values specify lower limit for the acceptable range to be
#' applied to each edge.  Requires zu.bot and yu.bot, as well as Fu.top,
#' zu.top, and yu.top to specify the upper limit for the acceptable rage.
#' @param zu.bot Vector of length n that specifies the lower limit to be
#' applied to each network input.
#' @param yu.bot Vector of length n that specifies the lower limit to be
#' applied to each network output.
#' @param Fu.top Matrix of size n by n with values corresponding to each
#' network edge.  Values specify upper limit for the acceptable range to be
#' applied to each edge.  Requires zu.top and yu.top, as well as Fu.bot,
#' zu.bot, and yu.bot to specify the lower limit for the acceptable range.
#' @param zu.top Vector of length n that specifies the upper limit to be
#' applied to each network input.
#' @param yu.top Vector of length n that specifies the upper limit to be
#' applied to each network output.
#' @param iter Number of times to sample the constrained solution space.
#' Corresponds to the number of networks generated in the function output.
#' Value should be sufficiently large.
#' @return \item{plausible.models}{List of network objects of length iter with
#' edge values that fall within the range of values specified by function
#' inputs.}
#' @note This function utilizes the xsample() function (Van den Meersche, 2009)
#' from the limSolve package (Soetaert, 2009).
#' @author David E. Hines
#' @references Van den Meersche, K., Soetaert, K. & Van Oevelen, D. (2009)
#' xsample(): An R function for sampling linear inverse problems. Journal of
#' Statistical Software, 30.
#' 
#' Soetaert, K., Van den Meersche, K. & van Oevelen, D. (2009) limSolve:
#' Solving linear inverse models. R package version 1.5.1.
#' @keywords Flow Linear Uncertainty analysis inverse modeling network
#' @examples
#' 
#' 
#' 
#' ## # Three different ways to purtub all fluxes in a model by +/- 50% # Uniform
#' ## edge uncertainty (50%) data(troModels) UA = enaUncertainty(x=troModels[[6]],
#' ## p.err=50, iter=10000)
#' 
#' ## # Data driven uncertainty (standard deviation) data(troModels) Fu.sd =
#' ## matrix(c(0, 0, 0, 0, 50, 0, 0, 50, 0, 50, 0, 0, 0, 50, 50, 0, 0, 0, 0, 50, 0,
#' ## 50, 50, 0, 0), byrow=TRUE, nrow=5, ncol=5) zu.sd = c(50, 0, 0, 0, 50) yu.sd =
#' ## c(50, 50, 50, 50, 50)
#' 
#' ## UA = enaUncertainty(troModels[[6]], Fu.sd=Fu.sd, zu.sd=zu.sd, yu.sd=yu.sd,
#' ## iter=10000)
#' 
#' ## # Data driven uncertainty (upper and lower limit) data(troModels) Fu.bot =
#' ## matrix(c(0, 0, 0, 0, 4440.5, 0, 0, 37.5, 0, 800, 0, 0, 0, 185, 100, 0, 0, 0, 0,
#' ## 83.5, 0, 2620.5, 1154.5, 0, 0), byrow=TRUE, nrow=5, ncol=5) zu.bot = c(5592, 0,
#' ## 0, 0, 317.5) yu.bot = c(1151.5, 1765.0, 907, 101.5, 1984.5)
#' 
#' ## Fu.top = matrix(c(0, 0, 0, 0, 13321.5, 0, 0, 112.5, 0, 2500, 0, 0, 0, 555, 300,
#' ## 0, 0, 0, 0, 250.5, 0, 7807.5, 3463.5, 0, 0), byrow=TRUE, nrow=5, ncol=5) zu.top
#' ## = c(16776, 0, 0, 0, 952.5) yu.top = c(3454.5, 5295, 2721, 304.5, 5953.5)
#' 
#' ## UA = enaUncertainty(troModels[[6]], Fu.bot=Fu.bot, zu.bot=zu.bot,
#' ## yu.bot=yu.bot, Fu.top=Fu.top, zu.top=zu.top, yu.top=yu.top, iter=10000)
#' 
#' 
#' 
enaUncertainty <- function(x = 'network object', p.err=NA, Fu.sd=NA, zu.sd=NA, yu.sd=NA,
Fu.bot=NA, zu.bot=NA, yu.bot=NA, Fu.top=NA, zu.top=NA, yu.top=NA, iter=100){

    if (class(x) != 'network'){warning('x is not a network class object')}   # check object class

    ## if (p.err == 0){warning('Zero error given, using 0.0001% error')}
    ## zero error warning

    if (is.na(Fu.sd)[1] == FALSE && is.na(zu.sd)[1] == TRUE){
        warning('Insufficient uncertainty data')}   # check uncertainty data inputs
    if (is.na(Fu.sd)[1] == FALSE && is.na(yu.sd)[1] == TRUE){
        warning('Insufficient uncertainty data')}   # check uncertainty data inputs
    if (is.na(Fu.bot)[1] == FALSE && is.na(zu.bot)[1] == TRUE){
        warning('Insufficient uncertainty data')}   # check uncertainty data inputs
    if (is.na(Fu.bot)[1] == FALSE && is.na(yu.bot)[1] == TRUE){
        warning('Insufficient uncertainty data')}   # check uncertainty data inputs
    if (is.na(Fu.top)[1] == FALSE && is.na(zu.top)[1] == TRUE){
        warning('Insufficient uncertainty data')}   # check uncertainty data inputs
    if (is.na(Fu.top)[1] == FALSE && is.na(yu.top)[1] == TRUE){
        warning('Insufficient uncertainty data')}   # check uncertainty data inputs
    if (is.na(Fu.top)[1] == FALSE && is.na(Fu.bot)[1] == TRUE){
        warning('Lower limit uncertainty data not detected')}   # check uncertainty data inputs
    if (is.na(Fu.bot)[1] == FALSE && is.na(Fu.top)[1] == TRUE){
        warning('Upper limit uncertainty data not detected')}   # check uncertainty data inputs

    U <- unpack(x)  # unpack network object
    fluxes <- which(U$F!=0, arr.ind=TRUE) # identify internal fluxes (from,to)
    inputs <- seq(from=1, to=length(U$z), by=1) # identify inputs
    outputs <- seq(from=1, to=length(U$y), by=1) # identify outputs
    vertex.names <- rep(0, x$gal$n) # get names
    respiration <- rep(0, x$gal$n)
    living <- U$living
    storage <- U$X

    for(i in 1:x$gal$n){
        vertex.names[i] <- x$val[[i]]$vertex.names
    }

                                        # initialize E
    E <- matrix(0, nrow=x$gal$n,
                ncol=(nrow(fluxes)+length(inputs)+length(outputs)))

                                        # add inputs to E
    for(i in 1:length(U$z)){
        E[i,i] <- U$z[i]
    }

                                        # add outputs to E
    for(i in 1:nrow(E)){
        E[i,length(U$z)+outputs[i]] <- -U$y[i]
    }

                                        # add internal node inputs
    for(f in 1:nrow(fluxes)){
        E[fluxes[f,2],(length(U$z)+length(U$y)+f)] <- U$F[fluxes[f,1],fluxes[f,2]]
    }

                                        # add internal node outputs
    for(f in 1:nrow(fluxes)){
        E[fluxes[f,1],(length(U$z)+length(U$y)+f)] <- -U$F[fluxes[f,1],fluxes[f,2]]
    }

    F <- rep(0,x$gal$n) # create F

    G <- rbind(diag(rep(1, ncol(E))), diag(rep(-1, ncol(E)))) # create G

    lower.percent <- 0 # initialize
    upper.percent <- 0 # initialize

                                        # create H
    if(is.na(p.err)[1] == FALSE ){
        if(p.err < 100 && p.err > 0){
            lower.percent <- 1-(p.err/100)
            upper.percent <- 1+(p.err/100)
        }else if(p.err >= 100){
                                        # if p.err is given greater than 100,
                                        # restrict flows to be positive
            lower.percent <- 0.0001
            upper.percent <- 1+(p.err/100)
        }else if(p.err == 0){
            warning('Percent error is 0, setting lower to 0.9999 and upper to 1.0001')
            lower.percent <- 0.9999
            upper.percent <- 1.0001
        }

        H <- c(rep(lower.percent, ncol(E)),rep(-upper.percent, ncol(E)))

    }else if(is.na(Fu.sd)[1] == FALSE){

                                        # order = all z, all y, f by fluxes
        for(h in 1:(length(U$z))){
            lower.percent[h] <- ((U$z[h]-zu.sd[h])/U$z[h])
        }

        for(h in (length(U$z)+1):(length(U$z)+length(U$y))){
            lower.percent[h] <- ((U$y[h-length(U$z)]-yu.sd[h-length(U$z)])/U$y[h-length(U$z)])
        }

        for(h in (length(U$z)+length(U$y)+1):(length(U$z)+length(U$y)+nrow(fluxes))){
            lower.percent[h] <- ((U$F[fluxes[(h-length(U$z)-length(U$y)),1],fluxes[(h-length(U$z)-length(U$y)),2]]-
                                     Fu.sd[fluxes[(h-length(U$z)-length(U$y)),1],fluxes[(h-length(U$z)-length(U$y)),2]])/
                                    U$F[fluxes[(h-length(U$z)-length(U$y)),1],fluxes[(h-length(U$z)-length(U$y)),2]])
        }

        lower.percent[is.na(lower.percent)] <- 0
                                        # restrict values to be positive
        lower.percent[which(lower.percent < 0)] <- 0.0001

        for(h in 1:(length(U$z))){
            upper.percent[h] <- ((U$z[h]+zu.sd[h])/U$z[h])
        }

        for(h in (length(U$z)+1):(length(U$z)+length(U$y))){
            upper.percent[h] <- ((U$y[h-length(U$z)]+yu.sd[h-length(U$z)])/U$y[h-length(U$z)])
        }

        for(h in (length(U$z)+length(U$y)+1):(length(U$z)+length(U$y)+nrow(fluxes))){
            upper.percent[h] <-
                (
                    (U$F[fluxes[(h-length(U$z)-length(U$y)),1],
                         fluxes[(h-length(U$z)-length(U$y)),2]]+
                             Fu.sd[fluxes[(h-length(U$z)-length(U$y)),1],
                                   fluxes[(h-length(U$z)-length(U$y)),2]])/
                        U$F[fluxes[(h-length(U$z)-length(U$y)),1],
                            fluxes[(h-length(U$z)-length(U$y)),2]]
                    )
        }

        upper.percent[is.na(upper.percent)] <- 0
        H <- c(lower.percent,-upper.percent)

    }else if(is.na(Fu.bot)[1] == FALSE && is.na(Fu.top)[1] == FALSE){
        for(h in 1:(length(U$z))){
            lower.percent[h] <- zu.bot[h]/U$z[h]
        }
        for(h in (length(U$z)+1):(length(U$z)+length(U$y))){
            lower.percent[h] <- yu.bot[h-length(U$z)]/U$y[h-length(U$z)]
        }

        for(h in (length(U$z)+length(U$y)+1):(length(U$z)+length(U$y)+nrow(fluxes))){
            lower.percent[h] <- Fu.bot[fluxes[(h-length(U$z)-length(U$y)),1],
                                       fluxes[(h-length(U$z)-length(U$y)),2]]/
                                           U$F[fluxes[(h-length(U$z)-length(U$y)),1],
                                               fluxes[(h-length(U$z)-length(U$y)),2]]
        }

        lower.percent[is.na(lower.percent)] <- 0
        lower.percent[which(lower.percent < 0)] <- 0.0001 # restrict values to be positive

        for(h in 1:(length(U$z))){
            upper.percent[h] <- zu.top[h]/U$z[h]
        }

        for(h in (length(U$z)+1):(length(U$z)+length(U$y))){
            upper.percent[h] <- yu.top[h-length(U$z)]/U$y[h-length(U$z)]
        }

        for(h in (length(U$z)+length(U$y)+1):(length(U$z)+length(U$y)+nrow(fluxes))){
            upper.percent[h] <- Fu.top[fluxes[(h-length(U$z)-length(U$y)),1],
                                       fluxes[(h-length(U$z)-length(U$y)),2]]/
                                           U$F[fluxes[(h-length(U$z)-length(U$y)),1],
                                               fluxes[(h-length(U$z)-length(U$y)),2]]
        }

        upper.percent[is.na(upper.percent)] <- 0

        H <- c(lower.percent,-upper.percent)

    }else{warning('Insufficient data for uncertainty analysis')}

    xs <- xsample(E=E, F=F, G=G, H=H, iter=iter)    # calculate plausible coefficients

    z.ena <- rep(0, x$gal$n)    # initialize
    y.ena <- rep(0, x$gal$n)
    F.ena <- matrix(0, nrow=nrow(U$F), ncol=ncol(U$F))
    plausible.models <- list()

    for(k in 1:nrow(xs$X)){    # construct pluasible models
        for(z in 1:length(U$z)){# inputs (z)
            z.ena[z] <- xs$X[k,z]*U$z[z]
        }
        for(y in (length(U$z)+1):(length(U$z)+length(U$y))){    # outputs (y)
            y.ena[y-length(U$z)] <- xs$X[k,(y)]*U$y[y-length(U$z)]
        }
        for(f in (length(U$z)+length(U$y)+1):ncol(xs$X)){# internal fluxse (F)
            F.ena[fluxes[(f-length(U$z)-length(U$y)),1],
                  fluxes[(f-length(U$z)-length(U$y)),2]] <-
                      xs$X[k,f] * U$F[fluxes[(f-length(U$z)-length(U$y)),1],
                                      fluxes[(f-length(U$z)-length(U$y)),2]]
        }

        rownames(F.ena) <- vertex.names
        colnames(F.ena) <- vertex.names

        plausible.models[[k]] <- pack(flow=F.ena,
                                      input=z.ena,
                                      export=y.ena,
                                      respiration=respiration,
                                      living=living,
                                      output=y.ena,
                                      storage=storage)
    }

    return(plausible.models)
}

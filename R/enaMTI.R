#' enaMTI --- Mixed Trophic Impacts Analysis
#' follows Ulanowicz and Puccia, 1990.
#' INPUT = network object
#' OUTPUT = list of trophic impact statistics
#' Borrett | June 2012, MKL | July 2013
#' ------------------------------------



#' Mixed Trophic Impacts (MTI) Analysis
#' 
#' Calculates the Mixed Trophic Impacts of one species on another in the given
#' ecosystem model following the algorithm of Ulanowicz and Puccia (1990).
#' This considers both the direct and indirect trophic impacts.
#' 
#' 
#' @param x a network object.  This includes all weighte dflows into and out of
#' each node.  It must also include the "Living" vector that identifies the
#' living (TRUE/FALSE) status of each node.
#' @param zero.na A logical parameter that specifies if NAs generated in the
#' analysis should be reset to zero.  The default is TRUE.
#' @param balance.override
#' 
#' Mixed Trophic Impacts analysis builds on flow analysis and thus assumes the
#' network model is at steady-state (inputs = outputs).  Setting
#' balance.override = TRUE allows the function to be run on unbalanced models,
#' though this is unadvised.
#' @param eigen.check
#' 
#' LOGICAL: should the dominant eigen value be checked?  By default, the
#' function will not return utility values if the eigenvalue is larger than
#' one; however, if eigen.check is set to FALSE, then the function will be
#' applied regardless of the mathematic concern.
#' 
#' @return \item{G}{output-oriented direct flow intensity matrix as in enaFlow,
#' except oriented from row to column.} \item{FP}{input-oriented direct flow
#' intensity matrix similar to enaFlow; however, the calculation exclude
#' respiration losses from the throughflow in the denominator to focus on NET
#' production.  Also, if the receiver compartment is not living, the flux
#' intensity is set to zero.} \item{Q}{direct net trophic impacts (G-t(FP)).}
#' \item{M}{Total (direct and indirect) tropic impacts of compartment i on j.}
#' @note This and other Ulanowicz school functions require that export and
#' respiration components of output be separately quantified.
#' 
#' This analysis is similar in concept to the ENA Utility analysis.
#' 
#' With regard to the eigen.check argument, like enaFlow, enaStorage and
#' enaUtility, this analysis considers the trophic impact propigated over path
#' lengths ranging for zero to infinity.  For the analysis to work properly,
#' the path sequence must converge.  This function checks to see if the path
#' sequence is convergent by finding the dominant eigenvalue of the direct
#' matrix.  If this eigenvalue is less than 1, the sequence is convergent and
#' the analysis can be applied; if the dominant eigenvalue is greater than one,
#' then the anlysis cannot be applied.
#' @author Stuart R. Borrett Matthew K. Lau
#' @seealso \code{\link{enaFlow},\link{enaUtility}}
#' @references %% ~put references to the literature/web site here ~ Ulanowicz,
#' R.E. and C.J. Puccia.  1990. Mixed trophic impacts in ecosystems.  Coenoses
#' 5, 7--16.
#' @examples
#' 
#' data(troModels)
#' mti <- enaMTI(troModels[[6]])
#' attributes(mti)
#' 
#' @export enaMTI
enaMTI <- function(x,eigen.check=TRUE,zero.na=TRUE, balance.override=FALSE){
                                        #Check for network class
  if (class(x) != 'network'){warning('x is not a network class object')}
                                        #Data checks
  if (any(is.na(x%v%'respiration'))){
    G <- FP <- Q <- M <- as.matrix(x, attrname = 'flow')
    G[is.na(G)==FALSE] <- FP[is.na(FP)==FALSE] <- Q[is.na(Q)==FALSE] <- M[is.na(M)==FALSE] <- NA
    out <- list('G'=G,'FP'=FP,'Q'=Q,'M'=M)
    warning('Model is missing respiration. Output is NA.')
  }else{
                                        #Check for balancing
    if (balance.override){}else{
      if (any(list.network.attributes(x) == 'balanced') == FALSE){x%n%'balanced' <- ssCheck(x)}
      if (x%n%'balanced' == FALSE){warning('Model is not balanced'); stop}
    }
                                        #Unpack
    Flow <- as.matrix(x, attrname = 'flow')  #flows
    input <- x%v%'input' #inputs
    output <- x%v%'output'
    resp <- x%v%'respiration'
    stor <- x%v%'storage' #storage values
    I <- Flow*0;
    diag(I) <- 1 #create the identity matrix
    T. <- input + apply(Flow,2,sum)
                                        #
    G <- t(t(Flow)/T.)        # input oriented direct flow intensity matrix
    FP <- Flow / (T.-resp)    # modified output oriented direct flow intensity matrix.  Authors exclude respiration to divide only by the NET production of the compartment.

                                        # check and replace NA values with 0 if zero.na
    if (zero.na){
      G[is.na(G)] <- 0
      FP[is.na(FP)] <- 0
    }

    # Make infinity values equal to zero
    G[is.infinite(G)] <- 0
    FP[is.infinite(FP)] <- 0

    # Set FP to zero when receiver compartment (j) is non-living
    FP[,which(x%v%'living'==FALSE)] <- 0
    Q <- G - t(FP)
    dom1Q <- abs(eigen(Q)$values[1])
    if(dom1Q <= 1 ){
      M <- ginv(I-Q)-I              # Total Impacts of i on j.
    } else {
      if(eigen.check==FALSE){
        M <- ginv(I-Q)-I              # Total Impacts of i on j.
      } else { M <- NA}
  }

    if(!any(is.na(M))){
        r <- relationalChange(Q,M)
        IR <- r$Integral.Relations
        r.table <- r$Relations.Table
        names(r.table) <- c("From","To","Net (direct)","Mixed (integral)","changed")
        rownames(r.table) <- c(1:dim(r.table)[1])
    } else {
        IR <- NA
        r.table <- NA
    }

    out <- list('G'=G,'FP'=FP,'Q'=Q,'M'=M,
                "Integral.Relations" = IR,
                "Relational.Table"=r.table)
  }
    return(out)
}

#' enautility --- utility analysis of a flow network
#' INPUT = network object
#' OUTPUT = list of utility statistics
#'
#' M. Lau | July 2011
#' ---------------------------------------------------







#' enautility --- utility analysis of a flow network INPUT = network object
#' OUTPUT = list of utility statistics
#' 
#' M. Lau | July 2011 ---------------------------------------------------
#' enautility --- utility analysis of a flow network INPUT = network object
#' OUTPUT = list of utility statistics
#' 
#' M. Lau | July 2011 ---------------------------------------------------
#' Utility Analysis of Ecological Networks
#' 
#' Performs the flow and storage based utility analysis developed for
#' input-output network models of ecosystems.  It returns a set of matrices for
#' the direct and integral utilities as well as a set of utility based network
#' statistics.
#' 
#' @param x a network object.  This includes all weighted flows into and out of
#' each node.  For the storage utility analysis this must also include the
#' amount of energy--matter stored at each node (biomass).
#' @param type Determines whether the flow or storage utility analysis is
#' returned.
#' @param eigen.check LOGICAL: should the dominant eigenvalue be checked.  Like
#' enaFlow and enaStorage analyses, enaUtility analysis considers the utility
#' propigated over path lengths ranging for zero to infinity.  For utility
#' analysis to work properly, the path sequence must converge.  enaUtility
#' checks to see if the utility path sequence is convergent by finding the
#' dominant eigenvalue of the direct utility matrix.  If this eigenvalue is
#' less than 1, the sequence is convergent and the analysis can be applied; if
#' the dominant eigenvalue is greater than one, then the anlysis cannot be
#' applied.  By default, the function will not return utility values if the
#' eigenvalue is larger than one; however, if eigen.check is set to FALSE, then
#' the function will be applied regardless of the mathematic validity.
#' @param balance.override LOGICAL: should model balancing be ignored.
#' enaUtility assumes that the network model is at steady-state.  The default
#' setting will not allow the function to be applied to models not at
#' steady-state.  However, when balance.override is set to TRUE, then the
#' function will work regardless.
#' @param tol The integral utility matrix is rounded to the number of digits
#' specified in tol.  This approximation eleminates very small numbers
#' introduced due to numerical error in the ginv function.  It does not
#' eliminate the small numerical error introduced in larger values, but does
#' truncate the numbers.
#' @return \item{D}{Direct flow utility intensity matrix.  (fij-fji)/Ti for
#' i,j=1:n} \item{U}{Nondimensional integral flow utility} \item{Y}{Dimensional
#' integral flow utility} \item{ns}{If type is set to 'flow', this is a list of
#' flow utility network statistics including: the dominant eigenvalue of D
#' (lambda\_1D), flow based network synergism (synergism.F), and flow based
#' network mutualism (mutualism.F).} \item{DS}{Direct storage utility intensity
#' matrix.  (fij-fji)/xi for i,j=1:n} \item{US}{Nondimensional integral storage
#' utility} \item{YS}{Dimensional integral storage utility} \item{ns}{If type
#' is set to 'storage', this is a list of storage utility network statistics
#' including: the dominant eigenvalue of DS (lambda_1DS), storage based network
#' synergism (synergism.S), and storage based network mutualism (mutualism.S).}
#' @author Matthew K. Lau Stuart R. Borrett
#' @seealso \code{\link{enaFlow},\link{enaStorage},\link{enaMTI}}
#' @references Fath, B.D. and Patten, B.C. 1998. Network synergism: emergence
#' of positive relations in ecological systems.  Ecol. Model. 107:127--143.
#' 
#' Fath, B.D. and Borrett, S.R. 2006. A Matlab function for Network Environ
#' Analysis. Environ. Model. Soft. 21: 375--405.
#' 
#' Patten, B.C. 1991.  Network ecology: Indirect determination of the
#' life-environment relationship in ecosystems.  In: Higashi, M. and Burns, T.
#' (eds). Theoretical Studies of Ecosystems: The Network Perspective. Cambridge
#' University Press.  New York.
#' @examples
#' 
#' 
#' 
#' data(troModels)
#' U <- enaUtility(troModels[[6]], type = "flow", eigen.check = FALSE)
#' attributes(U)
#' US <- enaUtility(troModels[[6]], type = "storage", eigen.check = FALSE)
#' 
#' 
#' 
#' @export enaUtility
enaUtility <- function(x, type=c('flow','storage'),
                       eigen.check=TRUE,
                       balance.override=FALSE,tol=10){
                                        #Missing Data Check
    if (type == 'storage' && any(is.na(x%v%'storage'))){
        warning('This function requires quantified storage values.')
    }else{
        orient <- get.orient()
                                        #Check for network class
        if (class(x) != 'network'){warning('x is not a network class object')}

                                        #set default for type == 'flow'
        if (length(type) > 1){type <- 'flow'}

                                        #Check for balancing
        if (balance.override){}else{
            if (any(list.network.attributes(x) == 'balanced') == FALSE){x%n%'balanced' <- ssCheck(x)}
            if (x%n%'balanced' == FALSE){warning('Model is not balanced'); stop}
        }
                                        #unpack data from x
        Flow <- t(as.matrix(x, attrname = 'flow')) #flows
        input <- x%v%'input' #inputs
        stor <- x%v%'storage' #storage values
        I <- Flow*0; diag(I) <- 1 #create the identity matrix
        T. <- input + apply(Flow,1,sum)
        FD <- Flow;
        diag(FD) <- -T.
                                        #
        if (type == 'flow'){
                                        #flow utilities
            D <- ginv(diag(T.)) %*% (FD - t(FD))
            rownames(D) <- colnames(D) <- colnames(Flow)

            if (eigen.check & abs(eigen(D)$values[1]) > 1){
                print(paste('Largest eigen value of D > 1:',eigen(D)$values[1]),quote=FALSE)
                out <- NA
            }else{
                U <- ginv(I-D) #non-dimensional integral flow utility
                U <- round(U,tol)
                Y <- diag(T.) %*% U #dimensional integral flow utility
                rownames(U) <- colnames(U) <- colnames(Flow)
                rownames(Y) <- colnames(Y) <- colnames(Flow)

                                        #indices
                synergism.F <- bcratio(Y) #flow benefit cost ratio (calls other function) (Synergism)
                mutualism.F <- bcratio(sign(Y)) # flow ratio of positive to negative signs )

                # get relational data
                R <- relationalChange(D,Y)
                SD <- R$Direct.Signs
                SY <- R$Integral.Signs
                R.table <- R$Relations.Table
                names(R.table) <- c("From","To","Direct","Integral","changed")
                rownames(R.table) <- c(1:dim(R.table)[1])
                                        #re-orient
                if (orient == 'rc'){
                    D <- t(D)
                    U <- t(U)
                    Y <- t(Y)
                    SD <- t(SD)
                    SY <- t(SY)
                }else{}

                ns <- cbind('lam1D' = abs(eigen(D)$values[1]),
                            'relation.change.F' = R$ns[1],
                            'synergism.F' = synergism.F,
                            'mutualism.F' = mutualism.F)

                out <- list('D'=D, 'SD' = SD,
                            'U'=U,
                            'Y'=Y, 'SY' = SY,
                            'Relations.Table' = R.table,
                            'ns'=ns) #pack output

            }

            # ------------------------------------------------------------------------
        }else if (type == 'storage'){
                                        #storage utilities
            x <- stor
            DS <- ginv(diag(x)) %*% (FD - t(FD))
            rownames(DS) <- colnames(DS) <- colnames(Flow)

            if (eigen.check & abs(eigen(DS)$values[1]) > 1){
                print(paste('Largest eigen value of DS > 1:',eigen(DS)$values[1]),quote=FALSE)
                out <- NA
      }else{

          US <- ginv(I - DS)
          US <- round(US,tol)
          YS <- diag(T.) %*% US
          rownames(US) <- colnames(US) <- colnames(Flow)
          rownames(YS) <- colnames(YS) <- colnames(Flow)


                                        #indices
          synergism.S <- bcratio(YS) #storage benefit cost ratio (calls other function) (Synergism)
          mutualism.S <- bcratio(sign(YS)) #storage ratio of positive to negative signs (Y/abs(Y) == sign of Y)

                          # get relational data
          R <- relationalChange(DS,YS)
          SD <- R$Direct.Signs
          SY <- R$Integral.Signs
          R.table <- R$Relations.Table
          names(R.table) <- c("From","To","Direct","Integral","changed")
          rownames(R.table) <- c(1:dim(R.table)[1])

                                        #re-orient
          if (orient == 'rc'){
              DS <- t(DS)
              US <- t(US)
              YS <- t(YS)
              SD <- t(DS)
              SY <- t(SY)

          }else{}

          ns <- cbind('lam1DS'=abs(eigen(DS)$values[1]),
                      'relation.change.S' = R$ns[1],
                      'synergism.S' = synergism.S,
                      'mutualism.S'=mutualism.S)

          out <- list('DS'=DS,'SD'=SD,
                      'US'=US,
                      'YS'=YS,'SY'=SY,
                      'Relations.Table' = R.table,
                      'ns'=ns) #package output
      }
        }
                                        #labeling
        if (length(out)>1){
            for (i in 1:(length(out)-1)){
                if (class(out[[i]])=='matrix'){
                    rownames(out[[i]]) <- colnames(out[[i]]) <- colnames(Flow)
                }
            }
        }
                                        #output
        return(out)
    }
}

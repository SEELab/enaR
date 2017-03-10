#' Trophic Aggregations (TroAgg) Analysis
#'
#' It returns the data quantifying the underlying trophic structure of a given
#' model based on the interaction of the living and non-living nodes. It is
#' based on the Trophic Aggregations suggested by Lindeman (1942) and follows
#' the algorithm by Ulanowicz and Kemp (1979) implemented in NETWRK 4.2b. It
#' removes the Feeding cycles in the network beforehand to provide accurate
#' results.
#'
#'
#' @param x a network object.  This includes all weighted flows into and out of
#' each node. It should include separate respiration and export values for the
#' Canonical Exports and Canonical Respirations results respectively. It must
#' also include the "Living" vector that identifies the living (TRUE/FALSE)
#' status of each node. It must contain the non-living nodes at the end of the
#' node vector, the function \code{\link{netOrder}} can be used for the same.
#' @return \item{Feeding_Cycles}{List that gives the details of the Feeding
#' Cycles in the network. The output being according to the enaCycle function
#' applied to the Living components in the network} \item{A}{matrix that
#' distributes the species in integer Trophic Levels (Lindeman Transformation
#' Matrix). The dimension of A is (NL X NL) where NL is the number of Living
#' nodes.} \item{ETL}{vector of the Effective Trophic Level of each species.}
#' \item{M.flow}{vector of the Migratory flows, if present, in the network.}
#' \item{CI}{vector of Canonical Inputs to the integer trophic levels.
#' Displayed if the Migratory flows are present.} \item{CE}{vector of Canonical
#' exports or the exports from the integer trophic levels} \item{CR}{vector of
#' the Canonical Respirations or the respiration values for integer trophic
#' levels. } \item{GC}{vector of the input flow to a trophic level from the
#' preceeding trophic level. It represents the Grazing Chain for the network.}
#' \item{RDP}{vector of the Returns to Detrital Pool from each trophic level. }
#' \item{LS}{vector of the Lindeman trophic spine. It combines the Detrital
#' pool with the autotrophs and forms a monotonically decreasing sequence of
#' flows from one trophic level to the next, starting with the said
#' combination.} \item{TE}{vector of the trophic efficiencies i.e. the ratio of
#' input to a trophic level to the amount of flow that is passed on the next
#' level from it. } \item{ns}{vector of trophic aggregations based network
#' statistics. These include the average Trohic Level ("ATL"), "Detritivory" the
#' flow from the detrital pool to
#' the second trophic level, "DetritalInput" the exogenous inputs to the
#' detrital pool, "DetritalCirc" the circulation within the detrital pool,
#' "NCYCS" the number of feeding cycles removed, "NNEX" the number of feeding
#' cycle Nexuses removed and "CI" the Cycling Index for the Feeding Cycles.  }
#' @note This and other Ulanowicz school functions require that export and
#' respiration components of output be separately quantified.
#'
#' This analysis involves the ENA Cycle analysis for removal of the Feeding
#' Cycles in the network. These are cycles amongst only the living nodes and
#' cause error in the trophic aggregations.
#'
#' The analysis requires all the non-living nodes to be placed at the end in
#' the network object.
#' @author Pawandeep Singh
#' @seealso \code{\link{enaCycle}, \link{netOrder}}
#' @references %% ~put references to the literature/web site here ~ Lindeman,
#' R.L. 1942. The trophic-dynamic aspect of ecology. Ecology 23:399--418.
#'
#' Ulanowicz, R.E. and Kemp, W.M.  1979. Towards canonical trophic
#' aggregations. The American Naturalist. 114:871--883.
#'
#' Ulanowicz, R.E. 1995. Ecosystem trophic foundations: Lindeman exonerata. pp.
#' 549--560. B.C. Patten and S.E. Jorgensen (eds.) Complex Ecology: The
#' part-whole relation in ecosystems. Prentice Hall, New Jersey.
#'
#' Ulanowicz, R.E. and Kay, J.J. 1991. A package for the analysis of ecosystem
#' flow networks. Environmental Software 6:131 -- 142.
#' @examples
#'
#'
#'
#' data(troModels)
#' tro6 <- enaTroAgg(troModels[[6]])
#' attributes(tro6)
#'
#'
#'
#' @export enaTroAgg
#' @import network
enaTroAgg <- function (x){
  if (class(x) != "network") {
    stop("x is not a network class object")
  }

                                        # Initials

  liv <- x %v% "living"     ##Living vector
  nl = sum(liv)             ##No. of living nodes
  N <- length(liv)
                                        #Living vector check
  liv2<-rep(FALSE,N)
  liv2[1:nl]<-rep(TRUE,nl)
  if(identical(liv,liv2) == FALSE) {
      stop('Non-living nodes must be at the end of the list.')
  }

  flow <- as.matrix(x, attrname = 'flow')
  XCHNGE<-flow
  Feeding_Cycles   <- cycliv(x)
  XCHNGE[1:nl,1:nl] <- Feeding_Cycles$ResidualFlows
  Ti <- x %v% "input"       ##AINPUT
  T <- Ti + apply(XCHNGE, 2, sum)

  exp<-x %v% "export"; exp[is.na(exp)] <- 0
  res<-x %v% "respiration"; res[is.na(res)] <- 0
  # ---------------------------------------------------------

                                        # Determining In-Migration and Obligate Producers
  BINPUT <- x %v% 'input'
  if(identical(XCHNGE,flow[1:nl,1:nl])) {print('Cycle free feeding transfers')}
  NMIG <- NPRM <- 0 ###NMIG - In-migration of Heterotrophs; NPRM - no. of obligate primary producers
  CANON <- rep(0,N)
  ###A compartment is an obligate producer iff it receives sustenance from no other compartment
  for(NP in 1:nl) {
  	if(Ti[NP]<=0){next}
  	for(i in 1:N) {
            ##Otherwise the input represents in-migration
  		if(XCHNGE[i,NP]<=0){next}
  		NMIG=NMIG+1
                ##Record the Migratory Input Temporary in CANON for use below
  		CANON[NP]=Ti[NP]
  		break
  	}
  	NPRM = NPRM+1
  }
  if(NPRM<=0){
  	warning("No Unambiguous primary producers found! All inputs assumed to be primary production!")
  	NMIG <- 0
  	CANON<- rep(0,N)
  	}
  mig.input<-rep(0,nl)
  if(NMIG>0) {
  	###Migratory Inputs. To be treated as Non-Primary Inflows
  	mig.input <- CANON[1:nl]
  }
  #---------------------------------------------------------

                                        #Recreate the matrix of feeding coefficients without the migratory inputs
  TL <- rep(0,N)
  FEED <- flow*0
  for(i in 1:N) {
  	##Store Throughputs in Vector TL
  	TL[i]<-0
  	for(j in 1:N) {TL[i]<-TL[i]+XCHNGE[j,i]}
  	TL[i]<-TL[i]+Ti[i]-CANON[i]
  	if(TL[i]<=0){TL[i]=1}
  	for(j in 1:N) {FEED[j,i]<-XCHNGE[j,i]/TL[i]}
  	BINPUT[i]<-(Ti[i]-CANON[i])/TL[i]
  }
  #---------------------------------------------------------

                                        #Create Lindeman Transformation Matrix
  CANON<- rep(0,N)
  A<-flow*0
  A[1,1:nl]<-BINPUT[1:nl]
  for(k in 2:nl) {
  	KM1=k-1
  	for(l in 1:nl) {
  		if((k<=2)&&(nl<N)) {
  			for(K2 in (nl+1):N) {A[k,l]<-A[k,l]+FEED[K2,l]}
  		}
  		for(j in 1:nl) {
  			A[k,l]<-A[k,l]+(A[KM1,j]*FEED[j,l])
  		}
  	}
  }
  if(nl<N) {for (i in (nl+1):N) {A[N,i]<-1}}
  ## A is the required Lindeman transformation matrix
  rownames(A) <- 1:N

                                        # 2. Effective Trophic Levels
  MF = matrix(1:N, nrow=N, ncol=N, byrow = 'FALSE')
  etl = rep(1,N)
  etl[1:nl] = apply((MF*A)[1:nl,1:nl],2,sum)


  ci <- Ti
  ci <- A %*% Ti
  ci <- as.vector(ci)


                                        # 3. Canonical Exports
  cel = exp
  ce = A %*% exp
  ce1 = as.vector(ce)

                                        # 4. Canonical Respirations
  crl = res
  cr = A%*%res
  cr1=as.vector(cr)

                                        # 5. Grazing Chain
  gc <- rep(0,nl)
  gc[1] <- sum(A[1,]*T)
  AT = A %*% flow %*% t(A)
  gc[2:nl]=apply(AT[1:(nl-1),1:nl,drop=FALSE],1,sum)

                                        # 6. Returns to Detrital Pool
  rtd <- AT[1:nl,N]
  rtd <- as.vector(rtd)

                                        # 7. Detrivory
  dtry <- sum(AT[N,1:nl])

                                        # 8. Input to Detrital Pool
  U <- A %*% Ti
  dinp<-0
  if(nl<N) {  dinp <- sum(U[(nl+1):N]) }

                                        # 9. Circulation within Detrital Pool
  dcir <- AT[N,N]

                                        # 10. Lindeman Spine
  ls = gc

  ls[1] = sum(rtd[2:nl]) + gc[1] + dinp
  ls[2] = gc[2]+dtry
                                        # 11. Trophic Efficiencies
  te=ls
  for(i in 1:nl){
    if(te[i]<=0){break}
    te[i]=te[i+1]/te[i]
  }
  te[is.na(te)] <- 0

                                        # Output Listing
  ATL <- mean(etl) # average trophic level
  Detrivory <- dtry
  DetritalInput <- dinp
  DetritalCirc <- dcir
  ns <- cbind(ATL, Detrivory, DetritalInput, DetritalCirc, Feeding_Cycles$ns)
  if(NMIG>0) {
  	out <- list(Feeding_Cycles=Feeding_Cycles[1:(length(Feeding_Cycles)-1)], A = A[1:nl,1:nl], ETL = etl, M.Flow = mig.input, CI = ci, CE = ce1, CR = cr1, GC = gc, RDP = rtd, LS = ls,TE = te, ns=ns)
  }
  else{
  	out <- list(Feeding_Cycles=Feeding_Cycles[1:(length(Feeding_Cycles)-1)], A = A[1:nl,1:nl], ETL = etl, CE = ce1, CR = cr1, GC = gc, RDP = rtd, LS = ls,TE = te, ns=ns)

  }

  return(out)

  }#End of Function troAgg





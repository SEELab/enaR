#'## NETWRK's Full Cycle Analysis
#'## Singh P. | July 2014
#'## Algorithm Source : Ulanowicz 1991: A package for the Analysis of Ecosystem Flow Networks
#'## -----------------------------------------------








#' ## NETWRK's Full Cycle Analysis ## Singh P. | July 2014 ## Algorithm Source
#' : Ulanowicz 1991: A package for the Analysis of Ecosystem Flow Networks ##
#' ----------------------------------------------- ## NETWRK's Full Cycle
#' Analysis ## Singh P. | July 2014 ## Algorithm Source : Ulanowicz 1991: A
#' package for the Analysis of Ecosystem Flow Networks ##
#' ----------------------------------------------- Full Cycle Analysis of
#' Ecological Networks
#'
#' It performs the full cycle analysis on the network based on the algorithm
#' described in Ulanowicz (1983) and implemented in NETWRK 4.2b. It returns
#' data.frames with details of the simple cycles and nexus, vectors of Cycle
#' distributions and Normalized distribution and matrices of Residual Flows and
#' Aggregated Cycles.
#'
#'
#' @param x a network object.  This includes all weighted flows into and out of
#' each node.
#' @return \item{Table.cycle}{data.frame that presents the details of the
#' simple cycles in the network. It contains "CYCLE" the cycle number, "NEXUS"
#' the nexus number corresponding to the cycle, "NODES" the nodes constituting
#' the cycle} \item{Table.nexus}{data.frame that presents the different nexuses
#' characterized by their corresponding weak arcs. It contains "NEXUS" the
#' nexus number, "CYCLES" the number of simple cycles present in that Nexus,
#' "W.arc.From" the starting node of the corresponding weak arc, "W.arc.To" the
#' ending node of the corresponding weak arc and "W.arc.Flow" the flow through
#' that weak arc} \item{CycleDist}{vector of the Cycle Distribution that gives
#' the flow cycling in loops of different sizes} \item{NormDist}{vector of the
#' Normalized Distribution i.e. the Cycle Distribution normalized by the Total
#' System Throughput of the system} \item{ResidualFlows}{matrix of the
#' straight-through (acyclic) flows in the network}
#' \item{AggregatedCycles}{matrix of the Aggregated Biogeochemical Cycles in
#' the network} \item{ns}{vector of the full cycle analysis based network
#' statistics. These include "NCYCS" the number of simple cycles identified in
#' the network, "NNEX" the number of the disjoint cycles or number of Nexuses
#' detected in the network and "CI" the cycling index of the network.}
#' @note The "NODES" in "Table.cycle" are arranged such that the weak arc for
#' the nexus is the arc between the first two nodes of the cycle.
#'
#' This function uses the backtracking procedure for the identification of
#' simple cycles, which are cycles that cross a node only once except the
#' starting node. The backtracking process is a depth-first search algorithm.
#'
#' In the data.frame "Table.cycle", if the number of cycles in a nexus is more
#' than 50, then a blank line is displayed after 50 cycles of the nexus,
#' followed by the cycles of the next nexus.
#'
#' The results of the analysis of Feeding Cycles can be obtained as a byproduct
#' of the enaTroAgg function that analyzes the trophic dynamics of a network.
#'
#' At every multiple of 5000 cycles in a nexus, the program prints an
#' indication for the user to know that it is still running.
#' @author Pawandeep Singh
#' @seealso \code{\link{enaTroAgg}}
#' @references %% ~put references to the literature/web site here ~ Johnson,
#' D.B. 1975. Finding all the elementary circuits of a directed graph. SIAM J.
#' Comput. 4:77--84
#'
#' Ulanowicz, R.E. 1983. Identifying the structure of cycling in ecosystems.
#' Methematical Biosciences 65:219--237
#'
#' Ulanowicz, R.E. and Kay, J.J. 1991. A package for the analysis of ecosystem
#' flow networks. Environmental Software 6:131 -- 142.
#' @examples
#'
#' data(troModels)
#' cyc6 <- enaCycle(troModels[[6]])
#' attributes(cyc6)
#'
#' @export enaCycle
#' @import network
#'


enaCycle <- function(x){

                                        #Initials

    if(class(x)!='network') {stop("x is not a network class object")}
    web <- as.matrix(x,attrname="flow")
    y <- x %v% "output"
    z <- x %v% "input"
    N <- length(y)

    TPTS <- apply(web,2,sum) + z

    F <- web/TPTS

    TST <- sum(web)+sum(y)+sum(z)
    df<-data.frame(NULL)
    df.cycle<-data.frame(0,0,'cycle', stringsAsFactors=FALSE)
###-----------------------------------------------------------------

                                        #Zero Global Variables
    NFST <- NEXNUM <- NCYC <- 0
    CYCS <- rep(0,N)

###-----------------------------------------------------------------

                                        #Start primary repeat loop
    repeat {
                                        # Zero all local variables
        NNEX  <- 0
        TCYCS <- rep(0,N)
        TMP   <- web*0
                                        #-----------------------------------

                                        #Count cycle arcs and determine exit from return
        NFWD <- NULL
        NTEMP <-NULL
        for (ii in 1:N) {#do 200 ii=1,N
            NFWD <- rep(0,N)
            NFWD[ii] <- 1
            for (k in 1:(N-1)) {
                for (i in 1:N) {
                    if(NFWD[i]>0) {next}
                    for (j in 1:N) {
                        if (NFWD[j] <1) {next}
                        if (web[j,i]<=0) {next}
                        NFWD[i] <- 1
                        break
                    }
                }
            }
            NTEMP[ii] <- 0
            for (i in 1:N) {
                if ((NFWD[i]>0) && (web[i,ii]>0)) {NTEMP[ii] <- NTEMP[ii]+1}
            }
        }#200
                                        # NTEMP will give the no. of Cycles ending in each node
        NSTP <- 0
        MAP <- NULL
        for (i in 1:N) {
            NMAX <- -1
            for (j in 1:N) {
                if (NTEMP[j]<=NMAX) {next}
                NMAX <- NTEMP[j]
                JMAX <- j
            }
            if (NMAX > 0) {NSTP <- NSTP + 1}
            NTEMP[JMAX] <- -2
            MAP[i] <- JMAX
        }
        #print(c('NSTP',NSTP))


        ### Condition for breaking/Exiting the primary repeat loop
        ###----------------------------------------
        if (NSTP<=0) {break} #breaks the primary repeat loop
        ###----------------------------------------

                                        # Start the NSTP2 While loop for Critical Arc determination
        ###----------------------------------------------------------------------------------------------
        NSTP2 <- 0
        repeat {
            slf.loop<-FALSE
            ARCMIN <- 10^25 #arbitrary min arc
            for (ir in 1:NSTP) {
                for (ic in 1:NSTP) {
                    IRTP <- MAP[ir]  # IRTP is the node to be searched at the ir'th position
                    ICTP <- MAP[ic]  # ICTP ----------------------------------ic'th --------
                    if (web[IRTP,ICTP] <= 0) {next}
                    if (web[IRTP,ICTP] >= ARCMIN) {next}
                    ARCMIN <- web[IRTP,ICTP]
                    IMIN <- IRTP
                    IM <- ir
                    JMIN <- ICTP
                    JM <- ic
                }
            }
            #print(ARCMIN)
            #print(min(web[web>0]))
            ### Exit from while(NSTP2<=0) if slf.loop
            if (IMIN == JMIN) {
                slf.loop <- TRUE
                break    #-----------------------BREAK THE WHILE repeat LOOP
            }
                                        #Make sure at least one cycle contains the current smallest arc web[IMIN,JMIN]
            NHALF <- (N/2)+1
            NFWD <- rep(0,N)
            NFWD[JMIN] <- 1
            ### find nodes from JMIN in fwd dirctn
            for (k in 1:NHALF) {
                for (i in 1:N) {
                    if (NFWD[i] >0) {next}
                    for (j in 1:N) {
                        if (NFWD[j] < 1) {next}
                        if (web[j,i] <= 0) {next}
                        NFWD[i] <- 1
                        break
                    }
                }
            }
            ### find nodes from IMIN in bckwd dirctn
            NODE <- rep(0,N)
            NODE[IMIN] <- 1
            for (k in 1:NHALF) {
                for (i in 1:N) {
                    if (NODE[i] > 0) {next}
                    for (j in 1:N) {
                        if (NODE[j] < 1) {next}
                        if (web[i,j] <= 0) {next}
                        NODE[i] <- 1
                        break
                    }
                }
            }
            ### find Common nodes aka members of the NEXUS
            NSTP2 <- 0  ###### ?????
            MAP2 <- rep(0,NSTP)
            for (i in 1:NSTP) {
                if ((NFWD[MAP[i]] <= 0) | (NODE[MAP[i]] <= 0)) {next}
                NSTP2 <- NSTP2 + 1
                MAP2[NSTP2] <- MAP[i]
            }
                                        # reorder mapping for IMIN and JMIN to come 1st and 2nd
            NFWD <- MAP2
            MAP2[1] <- IMIN
            MAP2[2] <- JMIN ##### SHORTER WAY POSSIBLE ####
            if (NSTP2 > 2) {
                INDX <- 2
                for (i in 1:NSTP2) {
                    if ((NFWD[i] == IMIN)||(NFWD[i]==JMIN)) {next}
                    INDX <- INDX+1
                    MAP2[INDX] <- NFWD[i]
                }
            }
            if (NSTP2 > 0) {break}
            web[IMIN,JMIN] = -web[IMIN,JMIN]
        } ###End of While Repeat NSTP2<=0
        ###----------------------------------------------------------------------------------------

                                        #IF the Critical arc is self loop
                                       #---------------------------------
        if(slf.loop == TRUE) {
            slf.loop <- FALSE
        	NCYC <- NCYC+1
            NNEX <- NNEX+1
            CYCS[1]<- CYCS[1]+web[IMIN,JMIN]
            WKARC<-F[IMIN,JMIN]*TPTS[IMIN]
            curr.slf.cyc<-noquote(c(NCYC,'.','(',IMIN,JMIN,')'))
            #print(curr.slf.cyc)
            #this.cycle <- rep(NA,N)
            this.cycle <- c(IMIN,JMIN)
            this.cycle <- paste(this.cycle, collapse='-')
            newcycle<-c(NCYC,(NEXNUM+1),this.cycle)
            df.cycle<-rbind(df.cycle,newcycle)

            web[IMIN,JMIN] <- 0
            NEXNUM <- NEXNUM+1
            curr.nexus <- c(NEXNUM,NNEX,IMIN,JMIN,WKARC)

            df<-rbind(df,curr.nexus) ### ('NEXUS', 'Cycles', weakarc, fromnode, tonode) ####################### df

            NFST <- 1
        }#End of if(slf.loop==TRUE)#

                                        #Begin Search for NEXUS defined by web[IMIN,JMIN] if not a self loop
        ###----------------------------------------------------------------------------------------

        else {
            WHOLE <- 0
                                        # Backtrack Routine Starts --------------
             # Backtrack Routine Starts --------------
            ### Initialize Node and Level
            LEVEL  <- 2
            NODE[1]<- 1
            NODE[2]<- 2
            skip.con.adv <- FALSE
            nex.com <- FALSE

            ### 2 Repeats start. rep1,2.
            repeat { #rep1
                repeat { #rep2
                    skip.con.chk <- FALSE

             ## Adv to nxt levels
                    if(skip.con.adv==FALSE) {
                        LM1         <- LEVEL
                        LEVEL       <- LEVEL+1
                        NODE[LEVEL] <- 1
                    }
### 2 Repeats start. rep3,4
                    repeat { #rep3
                        repeat { #rep4
                            ## Check for conn. b/w nodes at prsnt levels
                            if(skip.con.adv==FALSE){
                                NZ1  <- NODE[LM1]
                                KROW <- MAP2[NZ1]
                                NZ2  <- NODE[LEVEL]
                                KCOL <- MAP2[NZ2]
                                conn.chk <- FALSE
                                ## break rep4 and rep3 if conn exists
                                if(web[KROW,KCOL]>0 && skip.con.chk==FALSE) {
                                    conn.chk <- TRUE
                                    break #brk rep4
                                }
                            }
                            ## try next node in nxt level
                            NODE[LEVEL] <- NODE[LEVEL]+1
                            skip.con.adv <- FALSE
                            skip.con.chk <- FALSE
                            ## break rep4 after all levels are checked(NODE[level]>NSTP2)
                            if(NODE[LEVEL]>NSTP2) {break} #brk rep4
                        }#end of rep4
                        if(conn.chk==TRUE) {
                            conn.chk <- FALSE
                            break #rep3
                        }
                        # Backtrack to prev. level
                        LEVEL <- LEVEL-1
                        LM1   <- LEVEL-1
                        ## if further backtracking is impossible,
                                        #end search under weak arc. nexus complete
                        nex.com<-FALSE
                        if(LEVEL <= 2){
                            nex.com <- TRUE
                            break #break rep3
                        }
                        else {skip.con.chk<-TRUE} #goes to #420 to inc NODE[LEVEL]
                    }#end of rep3
                    if(nex.com==TRUE) {break} #break rep2
                    ##break rep2 if this conn completes cycle
                    if(NODE[LEVEL]==1) {break} #brk rep2
                    skip.con.adv <- FALSE
                    for (k in 1:LM1) {if (NODE[LEVEL] == NODE[k]) {skip.con.adv <- TRUE}}

                }#end of rep2
                if(nex.com==TRUE) {break} #brk rep1
                                        # -----------------BR Ends --------------
                                        # Calculate circuit prob
                WEIGHT <- 1
                for (kk in 1:LM1) {
                    KKP1 <- kk+1
                    KROW <- NODE[kk]
                    KCOL <- NODE[KKP1]
                    KROW <- MAP2[KROW]
                    KCOL <- MAP2[KCOL]
                    WEIGHT <- WEIGHT*F[KROW,KCOL]
                }
                                        # Add this weight
                for (kk in 1:LM1) {
                    KKP1 <- kk+1
                    KROW <- NODE[kk]
                    KCOL <- NODE[KKP1]
                    KROW <- MAP2[KROW]
                    KCOL <- MAP2[KCOL]
                    # Also, add this amount to the cycle distributions
                    TCYCS[LM1] <- TCYCS[LM1]+WEIGHT
                    TMP[KROW,KCOL] <- TMP[KROW,KCOL]+WEIGHT
                }
                                        # Report this cycle
                NNEX <- NNEX+1
                KTRY <- NNEX%%5000
                curr.prog <- noquote(c(NNEX,'Nexus cycles and Counting'))
                if(KTRY==0) {print(curr.prog)}
                NCYC <- NCYC+1
                if(NNEX>50){
                	skip.con.adv <- TRUE
                	next
                }
                L0 <- LM1+1
                for (kk in 1:L0) {
                    NTMP <- NODE[kk]
                    NTEMP[kk] <- MAP2[NTMP]
                }
                #curr.cycle <- noquote(c(NCYC,'.',NTEMP[1:L0]))
                #print(curr.cycle)
                this.cycle <- NTEMP[1:L0]
                #this.cycle[(L0+1):N]<-NA
                this.cycle <- paste(this.cycle, collapse='-')
                newcycle<-c(NCYC,(NEXNUM+1),this.cycle)
                df.cycle<-rbind(df.cycle,newcycle) #################------------------------------------df.cycle
                if(NNEX==50) {df.cycle<-rbind(df.cycle,rep('.',3))}
                skip.con.adv<-TRUE
            }#end of rep1
                                        #-----------------NEXUS COMPLETED---NEXUS REPEAT(rep1) ENDS HERE



                                        # Report this NEXUS
            WKARC=F[IMIN,JMIN]*TPTS[IMIN]
            NEXNUM=NEXNUM+1
            curr.nexus <- c(NEXNUM,NNEX,IMIN,JMIN,WKARC)
            df<-rbind(df,curr.nexus) ### ('NEXUS', 'Cycles', weakarc, fromnode, tonode) ####################### df
            #print(curr.nexus)
            # -------------------------------------
            # Normalize Probability Matrix & Subtract proper amounts from web
            PIVOT <- TMP[IMIN,JMIN]
            if(PIVOT<=0){print('Error in Normalizing Nexus Weights')}
            for(i in 1:N) {
                for(j in 1:N) {
                    if(web[i,j]<=0) {next}
                    web[i,j] <- web[i,j]-((TMP[i,j]/PIVOT)*ARCMIN)
                }
            }

            # Add proper amts to cycle distributions
            for(i in 1:N){CYCS[i]<-CYCS[i]+((TCYCS[i]/PIVOT)*ARCMIN)}               ############################## ERR.CHK CYCS depends on TCYCS, PIVOT, ARCMIN

            # Zero weak arc
            web[IMIN,JMIN] <- 0
            NFST <- 1
            if(WHOLE>1.00001) {print(c('Bad Sum Check = ',WHOLE))}


        }#End of else i.e. if not a slf.loop
    }#End of primary repeat loop

                                              # Report the Overall Results

    ### FIRST, UNCOVER ANY LINKS "HIDDEN" DURING SEARCH.
    web=abs(web)
    if(NFST!=0) {
        #cyc.rem <- noquote((c('A total of ',NCYC,'Cycles removed')))
        #print(cyc.rem)
        #print('Cycle Distributions')
        #print(CYCS)
        cycs<-CYCS
        CYC  <- sum(CYCS)
        CYCS <- (CYCS/TST)
        #print('Normalized Distribution')
        #print(CYCS)
        TEMP <- CYC/TST
        #print(c('cycling index is',TEMP))
        ResidualFlows<-web
        AggregatedCycles<-(as.matrix(x, attrname = 'flow')) - ResidualFlows
        colnames(df)<-c('NEXUS', 'CYCLES','W.arc.From','W.arc.To', 'W.arc.Flow')
        #colnames(df.cycle)<-rep(' ',(N+2))
        colnames(df.cycle)<-c('CYCLE','NEXUS','NODES')
        #df.cycle[is.na(df.cycle)==TRUE]<- ' '
        df.cycle<-df.cycle[-1,]
        rw<-row.names(df.cycle); rw<-as.numeric(rw)
        row.names(df.cycle) <- 2:max(rw)-1
        NCYCS<-NCYC; NNEX<-NEXNUM; CI<-TEMP
        ns <- cbind(NCYCS, NNEX, CI)
        out <- list(Table.cycle=df.cycle,Table.nexus=df,CycleDist = cycs, NormDist=CYCS, ResidualFlows=web, AggregatedCycles=AggregatedCycles, ns=ns)
        return(out)
    }#end of if (NFST!=0)
    else {
        NCYCS<-NCYC;NNEX<-NEXNUM; CI<-0
        ns <- cbind(NCYCS, NNEX, CI)
        out <- list(ResidualFlowks=web,ns=ns)
        return(out)
      }


}#END OF FUNCTION

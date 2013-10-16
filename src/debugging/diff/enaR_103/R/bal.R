# bal --- balances a flow model
# INPUT = network model in extended format
# OUTPUT = balanced model in extended format
# NOTE: this is the work horse for balance.R
# M. Lau | July 2011
# ---------------------------------------------------
bal <-
function(x='extended matrix',method=c('input','output')){

  T <- x  # extended flow matrix

  if (length(method > 1)){method <- method[1]}
  if (method == 'input'){}else if (method == 'output'){T <- t(T)}  # transpose matrix to use output method
                                        #Divide T[1:N,1:(N+3)] by the row sum to obtain F*
  N <- nrow(T)-3
  rs <- apply(T[1:N,1:(N+3)],1,sum) #row sum
  F.star <- T[1:N,1:(N+3)] / rs
                                        # SRB is adding the following line back
  F.star[is.na(F.star)] <- 0 #NA values produced by 0 denominators become 0
                                        #Transpose T[1:N,1:N] and subtract the identity matrix to get R
  R <- F.star[1:N,1:N]  # create identity matrix
  I <- R*0
  diag(I) <- 1
  R <- t(F.star[1:N,1:N]) - I
                                        #Invert R
  R <- ginv(R)

  # there seems to be a LOT of rounding error.  SRB is removing it. (Aug. 1, 2011)
  R[which(abs(R)<=1e-09)] <- 0

  #STEP 5  Revision by SRB (Aug.1, 2011)
  R <- -1*R %*% diag(apply(T[(N+1):(N+3),1:N],2,sum))  # as in bal_input.m

                                        #Multiply rij by the jth input in T and change sign
  #for (i in (1:nrow(R))){
  #  for (j in (1:ncol(R))){
  #    R[i,j] <- -1*R[i,j]*apply(T[(N+1):nrow(T),(1:N)],2,sum)[j]
  #  }
  #}
                                        # (STEP 6) Sum the ith row to build vector U
  U <- apply(R,1,sum)
                                        #Multiply each f*ij by it's corresponding uij
  FU <- F.star * U     
                                        #Coalesce balanced flows with inputs and outputs
  T.bal <- T
  T.bal[1:nrow(FU),] <- FU
                                        #Final transposition if method == output
  if (method == 'input'){}else if (method == 'output'){T.bal <- t(T.bal)}
                                        #  
  return(T.bal)

}

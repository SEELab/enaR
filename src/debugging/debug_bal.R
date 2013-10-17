###Debugging bal on 17Oct2013

bal <- function(T.star='extended, unbalanced matrix',method=c('input','output')){
  
  if (length(method > 1)){method <- method[1]}
  if (method == 'output'){T <- t(T)}else{}  # transpose matrix to use output method

                                        #From Allesina and Bondavalli 2003
                                        #Step 1. Check balancing
                                        #Done in balance()
                                        #Step 2. Get F.star
  F.star <- T.star
  N <- nrow(x) - 3
  for (i in 1:N){
    for (j in 1:(N+3)){
      F.star[i,j] <- T.star[i,j] / apply(T.star,1,sum)[i]
    }
  }
                                        #Step 3. Get R
  
  I.R <- diag(rep(1,N)) #F.star[1:N,1:N] identity matrix 
  R <- t(F.star[1:N,1:N]) - I.R
                                        #Step 4. Invert R
  R <- ginv(R)
                                        #Step 5. Multiply every rij by its corresponding input and change sign
  for (i in 1:nrow(R)){
    for (j in 1:nrow(R)){
      R[i,j] <- -(R[i,j] * (T[N+1,j]+T[N+2,j]+T[N+3,j]))
    }
  }
                                        #Step 6. Build U vector
  U <- apply(R,1,sum)
                                        #Step 7. Multiply F.star by corresponding U
  T.star.bal <- T.star
  for (i in 1:N){
    for (j in 1:(N+3)){
      T.star.bal[i,j] <- F.star[i,j] * U[i]
    }
  }
                                        #Final transposition if method == output
  if (method == 'output'){T.star.bal <- t(T.star.bal)}else{}
                                        #  
  return(T.star.bal)

}

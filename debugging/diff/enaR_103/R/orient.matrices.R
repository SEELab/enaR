# orient.matrices --- globally reorients matrices
# INPUT = matrix orientation (rc or cr)
# OUTPUT = sets the expected orientation of matrices
# 
# M. Lau | Feb 2013
# ---------------------------------------------------

orient.matrices <- local({
  orientation <- 'rc'
  f <- function(x='rc'){
    if (any(x%in%c('rc','cr'))){
      orientation <<- x[1]
      print(paste('Matrix orientation set to ',x,sep=''))
    }else{
      warning('Error: undefined matrix orientation.')
    }
  }
})

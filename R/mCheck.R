# check to see if model conversion was successful

mCheck <- function(x = "old", y = "new"){
  # x and y are two network models
  f1 <- x%n%'flow'
  f2 <- as.matrix(y,attrname='flow')
  er <- sum(abs(f1-f2))
  return(er)
}

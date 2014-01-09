# bcratio --- calculates the ratio of positive to 
# negative elements in a flow model
# INPUT = flow matrix
# OUTPUT = ratio of positive to negetive elements
# 
# S. Borrett | July 2011
# ---------------------------------------------------
bcratio <- function(x='matrix'){
  plus <- sum(sum(abs(x[x>0])))  # sum positive elements
  minus <- abs(sum(sum(abs(x[x<0]))))  # sum negative elements
  r <- plus/minus  # ratio of positive to negative elements
  return(r)
}

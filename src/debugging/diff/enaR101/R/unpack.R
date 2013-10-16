# unpack --- extracts network object into 
# a list
# INPUT = network object
# OUTPUT = list of network model components
# S. Borrett and M. Lau | July 2011
# ------------------------------------

unpack <- function(x='network object'){
  flow <- x%n%'flow'  
  input <- x%v%'input'
  respiration <- x%v%'respiration'
  respiration[is.na(respiration)] <- 0
  export <- x%v%'export'
  export[is.na(export)] <- 0
  output <- respiration + export
  storage <- x%v%'storage'
  living <- x%v%'living'
  return(list("F"=flow,"z"=input,"r"=respiration,"e"=export,"y"=output,"X"=storage,'Living'=living))
}

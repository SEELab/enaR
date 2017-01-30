#' unpack --- extracts network object into
#' a list
#' INPUT = network object
#' OUTPUT = list of network model components
#' S. Borrett and M. Lau | July 2011
#' ------------------------------------



#' "Unpacks" the Network Object into Separate Objects
#' 
#' Separates the components of a network object into separate components within
#' a list. This includes inputs, exports, respirations, outputs (exports +
#' respirations), storage, and internal flows.
#' 
#' 
#' @param x A network object.  This includes all weighted flows into and out of
#' each node.
#' @return \item{F}{matrix of flows from each node to each node oreinted row to
#' column.} \item{z}{Node boundary inputs.} \item{r}{Node boundary loss from
#' respiration.} \item{e}{Node boundary loss due to exportation} \item{y}{Node
#' boundary loss; summation of r and e} \item{X}{Node storage or biomass}
#' \item{living}{Logical vector indicating whether each node is living or not}
#' @note Flows are oriented from row to column.
#' @author Matthew K. Lau Stuart R. Borrett
#' @seealso \code{\link{pack},\link{read.scor}}
#' @examples
#' 
#' data(troModels)
#' unpack(troModels[[6]])
#' 
#' @export unpack
unpack <- function(x='network object'){
  flow <- as.matrix(x, attrname = 'flow')
  input <- x%v%'input'
  respiration <- x%v%'respiration'
  respiration[is.na(respiration)] <- 0
  export <- x%v%'export'
  export[is.na(export)] <- 0
  output <- x%v%'output'   #respiration + export
  storage <- x%v%'storage'
  living <- x%v%'living'
  return(list("F"=flow,"z"=input,"r"=respiration,"e"=export,"y"=output,"X"=storage,'living'=living))
}

#' Returns the global orientation
#' 
#' Returns the current setting for the expected orientation of all
#' matrices, which is either 'rc' (DEFAULT) or 'school' (output
#' orientation as expected for the school of analysis for a given
#' function). This function is intended to provide increase flexibility for users of both the Patten and Ulanowicz schools of ENA.
#' 
#' @author M.K. Lau and S.R. Borrett
#' @export get.orient
get.orient <- function(){
  current.orientation <- get('orientation',envir=environment(set.orient))
  return(current.orientation)
}

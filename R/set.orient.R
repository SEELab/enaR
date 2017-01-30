#' set.orient --- globally reorients matrices
#' INPUT = matrix orientation (rc or cr)
#' OUTPUT = sets the expected orientation of matrices
#' 
#' M. Lau | Feb 2013
#' ---------------------------------------------------





#' set.orient --- globally reorients matrices INPUT = matrix orientation (rc or
#' cr) OUTPUT = sets the expected orientation of matrices
#' 
#' M. Lau | Feb 2013 ---------------------------------------------------
#' Globally Set the Output Matrix Orientation
#' 
#' Changes the orientation of output matrices.
#' 
#' The enaR package as a whole, and the broader network analysis community,
#' assumes a row to column orientation; thus, the default orientation for the
#' package is row to column (DEFAULT = 'rc'). However, functions from the
#' Patten school were orignially developed to conduct calculations and produce
#' output in the column to row orientation. In order to facilitate the use of
#' these functions, we also provide the option for users to return output in
#' the orientation of the "school" (i.e. Patten results will be column to row
#' oriented) by setting the global orientation to "school" using this fuction.
#' 
#' @param x Orientation setting. If "rc" (DEFAULT), all matrix output will be
#' returned in row (=input) to column (=output) orientation, regardless of
#' school. If "school", then output matrices from functions from particular ENA
#' schools will be oriented as expected in that school (i.e. Patten =
#' column-row or Ulanowicz = row-column). Note, that all functions in the enaR
#' package expect input matrices to be oriented row-column.
#' @author Matthew K. Lau Stuart R. Borrett
#' @seealso \code{\link{get.orient}}
#' @examples
#' 
#' 
#' original.orientation = get.orient()
#' original.orientation
#' set.orient("school")
#' get.orient()
#' set.orient("rc")
#' get.orient()
#' set.orient(original.orientation)
#' 
#' 
#' @export set.orient
set.orient <- local({
  orientation <- 'rc'
  warn <- ''
  f <- function(x=c('rc','school')){
    if (any(x%in%c('rc','school','internal'))){
      orientation <<- x[1]
      if (x[1] == 'school'){
        warning('NOTE: output of functions from a particular analytical school will be returned in the standard orientation of that school.')
      }else if (x[1]=='internal'){
        orientation <<- 'school'
      }
    }else{
      warning('Unknown orientation.')
    }
  }
})

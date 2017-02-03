# relationalChange.r
# Which relationships change beteween the direct and integral utility analysis?
#
# Stuart R. Borrett
# February 25, 2016
# ------------------------







#' Relational change compared between two matrices.
#' 
#' Identifies the signs and pairwise relationsips of two matrices and compares
#' the difference between them.
#' 
#' 
#' @param x x is a square matrix of real numbers.  While this function is more
#' general, the initial intention was for this to be the direct utility matrix.
#' @param y y is a square matrix of real numbers.  While this function is more
#' general, the initial intention was for this to be the integral utility
#' matrix or the mixed trophic impacts matrix.
#' @return \item{Direct.Signs}{A sign matrix for matrix x.}
#' \item{Integral.Signs}{A sign matrix for matrix x.} \item{Direct.Relations}{A
#' matrix of the pairwise sign relationships for matrix x.}
#' \item{Integral.Relations}{A matrix of the pairwise signed relationships in
#' matrix y.} \item{Relations.Table}{A table that summarizes the relations.}
#' \item{Changed.Table}{A summary table of only the pariwise relationships that
#' changed between x and y.} \item{ns}{A vector of network statisitcs which
#' currently includes one whole-network statistic - a ratio of the
#' relationships changed between x and y.}
#' @note This function is called by enaUtility and enaMTI to summarize results.
#' @author Stuart R. Borrett
#' @seealso \code{\link{enaUtility}, \link{enaMTI}, \link{signs}}
relationalChange <- function(x="Direct.U",y="Integral.U"){
    vnames <- rownames(x)
    S1 <- signs(x)    # find the signs of the relationships in the direct utility matrix
    S2 <- signs(y)    # find the signs of the relationships in the integral utility matrix
    S1$rs.tab$order <- 1:dim(S1$rs.tab)[1]  # add a column by which we can resort SF
    SF <- merge( S1$rs.tab, S2$rs.tab, by = c("From", "To"),stringsAsFactors=FALSE)  # merges the two relationship results
    names(SF) <- c("From","To","R1","R1.name","order", "R2", "R2.name")
    SF <- SF[,c(-4,-7)]  # remove relationship names (simplify)
    o <- order(SF$order)
    SF <- SF[o,!(names(SF) %in% c("order"))]  # reorder the merged data frame and drop order column

    # which pairs changed?
    d <- rep("-",dim(SF)[1])
    d[which(!(SF$R1 == SF$R2))] <- "*"  # find the differneces
    SF$changed <- d
    CR <- SF[which(SF$changed=="*"),]
    possible.change = length(d)
    no.changed = length(which(SF$changed=="*"))
    r.changed = round(no.changed/possible.change * 100, 2)

    ns <- c("r.change"=r.changed)  # percent of direct relationships that change when all utilities are considered

    return(list("Direct.Signs" = S1$s,
                "Direct.Relations" = S1$relations,
                "Integral.Signs" = S2$s,
                "Integral.Relations" = S2$relations,
                "Relations.Table" = SF,
                "Changed.Table" = CR,
                "ns"=ns))
}




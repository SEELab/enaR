# signs.r
# February 25, 2016
# Stuart R. Borrett

# INPUT/OUPTUT - this function takes a numerical matrix and returns a matrix of its signs and a matrix of the relationships among the row/columns based on these signs
# -------------------

signs <- function(x="matrix"){
    vnames <- rownames(x)   # get row names
    d <- dim(x)             # find matrix dimensions
    s <- matrix(rep(0,prod(d)), ncol = d[2])  # initialize sign matrix
    r <- s                                    # initialize relationship matrix
    r.sparse <- list()                        # initialize r.sparse
    cnt1 <- 1                                 # iniitlize counter

    # dictionary of ecological relationship types
    erd <- matrix(c( "-", "+", "(-,+)",  "predation",
                    "-", "0", "(-,0)", "amensalism",
                    "-", "-", "(-,-)",  "competition",
                    "0", "+", "(0,+)", "anabolism",
                    "0", "0", "(0,0)", "neutralism",
                    "0", "-", "(0,-)", "catabolism",
                    "+", "+", "(+,+)", "mutualism",
                    "+", "0", "(+,0)", "commensalism",
                    "+", "-", "(+,-)", "altruism"),
                  ncol  = 4, byrow = TRUE)
    colnames(erd) <- c("source","sink","pair","name")

    # FIND matrix element sign {+, 0, -}
    positive <- which(x > 0, arr.ind = TRUE)  # which are positive
    negative <- which(x < 0, arr.ind = TRUE)  # which are negative

    # build the sign matrix
    s[positive] <- "+"
    s[negative] <- "-"
    rownames(s) <- colnames(s) <- vnames

    # combine signs to determine pairwise relationships
    for(i in 1:d[1]){
        for(j in 1:d[2]){
            if(j>=i){
                r[i,j] <- paste("(",s[j,i],",",s[i,j],")",sep="")  # pariwise realtionships - matrix
                tmp.relation <- erd[which(erd[,3] == r[i,j]),4]
                r.sparse[[cnt1]] <- c(vnames[i],vnames[j],r[i,j],tmp.relation)  # pariwise realtionships - table
                cnt1 <- cnt1 + 1  # increment counter
            }
        }
    }
    colnames(r) <- vnames; rownames(r) <- vnames
    r.sparse <- do.call(rbind,r.sparse)  # combine results for r.sparse
    colnames(r.sparse) <- c("Source","Sink","Relationship","R.name")  # rename columns

    relationship.counts <- table(r.sparse[,3])

    return(list("sign"=s,
                "relations"=r,
                "rs.tab"=as.data.frame(r.sparse,stringsAsFactors=FALSE),
                'relationship.counts' = relationship.counts)
           )
}

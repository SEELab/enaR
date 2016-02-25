# relationalChange.r
# Which relationships change beteween the direct and integral utility analysis?
#
# Stuart R. Borrett
# February 25, 2016
# ------------------------

relationalChange <- function(x="Direct.U",y="Integral.U"){
    vnames <- rownames(x)
    S1 <- signs(x)    # find the signs of the relationships in the direct utility matrix
    S2 <- signs(y)    # find the signs of the relationships in the integral utility matrix
    SF <- merge(S1$rs.tab, S2$rs.tab, by = c("Source", "Sink"),stringsAsFactors=FALSE)  # merges the two relationship results
    names(SF) <- c("Source","Sink","Direct.Relation","DR.name", "Integral.Relation", "IR.name")

    # which pairs changed?
    d <- rep("-",dim(SF)[1])
    d[which(!(SF$Direct.Relation == SF$Integral.Relation))] <- "*"  # find the differneces
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


\name{enaUncertainty}
\alias{enaUncertainty}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
enaUncertainty(x = "network object", p.err = NA, Fu.sd = NA, zu.sd = NA, yu.sd = NA, Fu.bot = NA, zu.bot = NA, yu.bot = NA, Fu.top = NA, zu.top = NA, yu.top = NA, iter = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{p.err}{
%%     ~~Describe \code{p.err} here~~
}
  \item{Fu.sd}{
%%     ~~Describe \code{Fu.sd} here~~
}
  \item{zu.sd}{
%%     ~~Describe \code{zu.sd} here~~
}
  \item{yu.sd}{
%%     ~~Describe \code{yu.sd} here~~
}
  \item{Fu.bot}{
%%     ~~Describe \code{Fu.bot} here~~
}
  \item{zu.bot}{
%%     ~~Describe \code{zu.bot} here~~
}
  \item{yu.bot}{
%%     ~~Describe \code{yu.bot} here~~
}
  \item{Fu.top}{
%%     ~~Describe \code{Fu.top} here~~
}
  \item{zu.top}{
%%     ~~Describe \code{zu.top} here~~
}
  \item{yu.top}{
%%     ~~Describe \code{yu.top} here~~
}
  \item{iter}{
%%     ~~Describe \code{iter} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x = "network object", p.err = NA, Fu.sd = NA, zu.sd = NA, 
    yu.sd = NA, Fu.bot = NA, zu.bot = NA, yu.bot = NA, Fu.top = NA, 
    zu.top = NA, yu.top = NA, iter = 100) 
{
    if (class(x) != "network") {
        warning("x is not a network class object")
    }
    if (is.na(Fu.sd)[1] == FALSE && is.na(zu.sd)[1] == TRUE) {
        warning("Insufficient uncertainty data")
    }
    if (is.na(Fu.sd)[1] == FALSE && is.na(yu.sd)[1] == TRUE) {
        warning("Insufficient uncertainty data")
    }
    if (is.na(Fu.bot)[1] == FALSE && is.na(zu.bot)[1] == TRUE) {
        warning("Insufficient uncertainty data")
    }
    if (is.na(Fu.bot)[1] == FALSE && is.na(yu.bot)[1] == TRUE) {
        warning("Insufficient uncertainty data")
    }
    if (is.na(Fu.top)[1] == FALSE && is.na(zu.top)[1] == TRUE) {
        warning("Insufficient uncertainty data")
    }
    if (is.na(Fu.top)[1] == FALSE && is.na(yu.top)[1] == TRUE) {
        warning("Insufficient uncertainty data")
    }
    if (is.na(Fu.top)[1] == FALSE && is.na(Fu.bot)[1] == TRUE) {
        warning("Lower limit uncertainty data not detected")
    }
    if (is.na(Fu.bot)[1] == FALSE && is.na(Fu.top)[1] == TRUE) {
        warning("Upper limit uncertainty data not detected")
    }
    U <- unpack(x)
    fluxes <- which(U$F != 0, arr.ind = TRUE)
    inputs <- seq(from = 1, to = length(U$z), by = 1)
    outputs <- seq(from = 1, to = length(U$y), by = 1)
    vertex.names <- rep(0, x$gal$n)
    respiration <- rep(0, x$gal$n)
    living <- U$Living
    storage <- U$X
    for (i in 1:x$gal$n) {
        vertex.names[i] <- x$val[[i]]$vertex.names
    }
    E <- matrix(0, nrow = x$gal$n, ncol = (nrow(fluxes) + length(inputs) + 
        length(outputs)))
    for (i in 1:length(U$z)) {
        E[i, i] <- U$z[i]
    }
    for (i in 1:nrow(E)) {
        E[i, length(U$z) + outputs[i]] <- -U$y[i]
    }
    for (f in 1:nrow(fluxes)) {
        E[fluxes[f, 2], (length(U$z) + length(U$y) + f)] <- U$F[fluxes[f, 
            1], fluxes[f, 2]]
    }
    for (f in 1:nrow(fluxes)) {
        E[fluxes[f, 1], (length(U$z) + length(U$y) + f)] <- -U$F[fluxes[f, 
            1], fluxes[f, 2]]
    }
    F <- rep(0, x$gal$n)
    G <- rbind(diag(rep(1, ncol(E))), diag(rep(-1, ncol(E))))
    lower.percent <- 0
    upper.percent <- 0
    if (is.na(p.err)[1] == FALSE) {
        if (p.err < 100 && p.err > 0) {
            lower.percent <- 1 - (p.err/100)
            upper.percent <- 1 + (p.err/100)
        }
        else if (p.err >= 100) {
            lower.percent <- 1e-04
            upper.percent <- 1 + (p.err/100)
        }
        else if (p.err == 0) {
            warning()
            lower.percent <- 0.9999
            upper.percent <- 1.0001
        }
        H <- c(rep(lower.percent, ncol(E)), rep(-upper.percent, 
            ncol(E)))
    }
    else if (is.na(Fu.sd)[1] == FALSE) {
        for (h in 1:(length(U$z))) {
            lower.percent[h] <- ((U$z[h] - zu.sd[h])/U$z[h])
        }
        for (h in (length(U$z) + 1):(length(U$z) + length(U$y))) {
            lower.percent[h] <- ((U$y[h - length(U$z)] - yu.sd[h - 
                length(U$z)])/U$y[h - length(U$z)])
        }
        for (h in (length(U$z) + length(U$y) + 1):(length(U$z) + 
            length(U$y) + nrow(fluxes))) {
            lower.percent[h] <- ((U$F[fluxes[(h - length(U$z) - 
                length(U$y)), 1], fluxes[(h - length(U$z) - length(U$y)), 
                2]] - Fu.sd[fluxes[(h - length(U$z) - length(U$y)), 
                1], fluxes[(h - length(U$z) - length(U$y)), 2]])/U$F[fluxes[(h - 
                length(U$z) - length(U$y)), 1], fluxes[(h - length(U$z) - 
                length(U$y)), 2]])
        }
        lower.percent[is.na(lower.percent)] <- 0
        lower.percent[which(lower.percent < 0)] <- 1e-04
        for (h in 1:(length(U$z))) {
            upper.percent[h] <- ((U$z[h] + zu.sd[h])/U$z[h])
        }
        for (h in (length(U$z) + 1):(length(U$z) + length(U$y))) {
            upper.percent[h] <- ((U$y[h - length(U$z)] + yu.sd[h - 
                length(U$z)])/U$y[h - length(U$z)])
        }
        for (h in (length(U$z) + length(U$y) + 1):(length(U$z) + 
            length(U$y) + nrow(fluxes))) {
            upper.percent[h] <- ((U$F[fluxes[(h - length(U$z) - 
                length(U$y)), 1], fluxes[(h - length(U$z) - length(U$y)), 
                2]] + Fu.sd[fluxes[(h - length(U$z) - length(U$y)), 
                1], fluxes[(h - length(U$z) - length(U$y)), 2]])/U$F[fluxes[(h - 
                length(U$z) - length(U$y)), 1], fluxes[(h - length(U$z) - 
                length(U$y)), 2]])
        }
        upper.percent[is.na(upper.percent)] <- 0
        H <- c(lower.percent, -upper.percent)
    }
    else if (is.na(Fu.bot)[1] == FALSE && is.na(Fu.top)[1] == 
        FALSE) {
        for (h in 1:(length(U$z))) {
            lower.percent[h] <- zu.bot[h]/U$z[h]
        }
        for (h in (length(U$z) + 1):(length(U$z) + length(U$y))) {
            lower.percent[h] <- yu.bot[h - length(U$z)]/U$y[h - 
                length(U$z)]
        }
        for (h in (length(U$z) + length(U$y) + 1):(length(U$z) + 
            length(U$y) + nrow(fluxes))) {
            lower.percent[h] <- Fu.bot[fluxes[(h - length(U$z) - 
                length(U$y)), 1], fluxes[(h - length(U$z) - length(U$y)), 
                2]]/U$F[fluxes[(h - length(U$z) - length(U$y)), 
                1], fluxes[(h - length(U$z) - length(U$y)), 2]]
        }
        lower.percent[is.na(lower.percent)] <- 0
        lower.percent[which(lower.percent < 0)] <- 1e-04
        for (h in 1:(length(U$z))) {
            upper.percent[h] <- zu.top[h]/U$z[h]
        }
        for (h in (length(U$z) + 1):(length(U$z) + length(U$y))) {
            upper.percent[h] <- yu.top[h - length(U$z)]/U$y[h - 
                length(U$z)]
        }
        for (h in (length(U$z) + length(U$y) + 1):(length(U$z) + 
            length(U$y) + nrow(fluxes))) {
            upper.percent[h] <- Fu.top[fluxes[(h - length(U$z) - 
                length(U$y)), 1], fluxes[(h - length(U$z) - length(U$y)), 
                2]]/U$F[fluxes[(h - length(U$z) - length(U$y)), 
                1], fluxes[(h - length(U$z) - length(U$y)), 2]]
        }
        upper.percent[is.na(upper.percent)] <- 0
        H <- c(lower.percent, -upper.percent)
    }
    else {
        warning("Insufficient data for uncertainty analysis")
    }
    print("ready for xSample")
    xs <- xsample(E = E, F = F, G = G, H = H, iter = iter)
    z.ena <- rep(0, x$gal$n)
    y.ena <- rep(0, x$gal$n)
    F.ena <- matrix(0, nrow = nrow(U$F), ncol = ncol(U$F))
    plausible.models <- list()
    for (k in 1:nrow(xs$X)) {
        for (z in 1:length(U$z)) {
            z.ena[z] <- xs$X[k, z] * U$z[z]
        }
        for (y in (length(U$z) + 1):(length(U$z) + length(U$y))) {
            y.ena[y - length(U$z)] <- xs$X[k, (y)] * U$y[y - 
                length(U$z)]
        }
        for (f in (length(U$z) + length(U$y) + 1):ncol(xs$X)) {
            F.ena[fluxes[(f - length(U$z) - length(U$y)), 1], 
                fluxes[(f - length(U$z) - length(U$y)), 2]] <- xs$X[k, 
                f] * U$F[fluxes[(f - length(U$z) - length(U$y)), 
                1], fluxes[(f - length(U$z) - length(U$y)), 2]]
        }
        rownames(F.ena) <- vertex.names
        colnames(F.ena) <- vertex.names
        plausible.models[[k]] <- pack(flow = F.ena, input = z.ena, 
            export = y.ena, respiration = respiration, living = living, 
            output = y.ena, storage = storage)
    }
    return(plausible.models)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

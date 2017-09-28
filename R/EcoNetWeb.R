#' Access example EcoNet models from the EcoNet website.
#' 
#' This function allows the user to access models that are presented on the
#' website for EcoNet, the web-based interface for conducting ENA
#' (http://eco.engr.uga.edu/), by Caner Kazanci at the University of Georgia.
#' 
#' 
#' @param model.name The model to be accessed. If 'prompt' the user will be
#' asked for the model they wish to use. Can also be a number for the model or
#' the name of the model.
#' @param url The URL for the EcoNet examples.
#' @return Returns the model formatted as a network object.
#' @author Matthew K. Lau
#' @seealso \code{\link{read.EcoNet}}
#' @references Kazanci, C., 2007. EcoNet: A new software for ecological
#' modeling, simulation and network analysis, Ecol. Model., Vol 208/1 pp 3-8.
#' @export EcoNetWeb
EcoNetWeb <- function(model.name = 'prompt', url = 'http://eco.engr.uga.edu/Examples/examples.html'){
    x <- readLines(url)
    x <- x[1:(grep('<!-- Copy paste for new model',x)-1)]
    mod.names <- x[grep('<h2>',x)]
    mod.names <- sub('<h2>','',mod.names);mod.names <- sub('</h2>','',mod.names)
    mod.names <- strsplit(mod.names,split=' ')
    mod.names <- lapply(mod.names,function(x) x[x != ''])
    mod.names <- unlist(lapply(mod.names,paste,collapse=' '))
    mod.loc <- cbind(grep('<hr class=\"duz\">',x),grep('</pre></div></td></tr></table>',x))
    mods <- apply(mod.loc,1,function(loc,x) x[loc[1]:loc[2]],x=x)
    if (model.name == 'prompt'){
        print(mod.names)
        model.name <- as.numeric(readline('Enter model number:'))
        out <- read.EcoNet(mods[[model.name]],parse = TRUE)
    }else if (is.numeric(model.name[1])){
        out <- read.EcoNet(mods[[model.name[1]]],parse = TRUE)
    }else{
        out <- read.EcoNet(mods[[agrep(model.name[1],mod.names)]], parse = TRUE)
    }
    return(out)
}

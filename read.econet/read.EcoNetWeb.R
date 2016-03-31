### Reading models from the econet website
### MKLau 31 Mar 2016

read.EcoNetWeb <- function(x='http://eco.engr.uga.edu/Examples/examples.html',model.name='prompt'){
    x <- readLines(x)
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
        out <- read.EcoNet(mods[[model.name]])
    }else if (is.numeric(model.name[1])){
        out <- read.EcoNet(mods[[model.name[1]]])
    }else{
        out <- read.EcoNet(mods[[agrep(model.name[1],mod.names,ignore.case=T)]])
    }
    return(out)
}

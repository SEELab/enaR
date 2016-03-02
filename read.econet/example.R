### reading econet models from the econet website
### 1 mar 2016
### mklau

x <- readLines('http://eco.engr.uga.edu/Examples/examples.html')
x <- x[1:(grep('<!-- Copy paste for new model',x)-1)]

## Parse models
mod.names <- x[grep('<h2>',x)]
mod.names <- sub('<h2>','',mod.names);mod.names <- sub('</h2>','',mod.names)
mod.names <- strsplit(mod.names,split=' ')
mod.names <- lapply(mod.names,function(x) x[x != ''])
mod.names <- unlist(lapply(mod.names,paste,collapse=' '))
mod.loc <- cbind(grep('<hr class=\"duz\">',x),grep('</pre></div></td></tr></table>',x))

mods <- apply(mod.loc,1,function(loc,x) x[loc[1]:loc[2]],x=x)

econet.mods <- lapply(mods,read.EcoNet)
plot(econet.mods[[1]],displaylabels=TRUE)
plot(econet.mods[[2]],displaylabels=TRUE)
plot(econet.mods[[3]],displaylabels=TRUE)
plot(econet.mods[[4]],displaylabels=TRUE)

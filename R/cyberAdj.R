### Function to create Structure of large random trophic ecosystems
### P. Singh | July 25, 2014
### Algorithm Source: Fath, B.D. 2004. Network analysis applied to large-scale cyber-ecosystems. Eco. Mod. 171(2004) 329-337
###------------------------------------------------------------------------
cyberAdj<-function(M) {

    ## m : primary producers
    ## n : grazers
    ## o : omnivores
    ## p : carnivores
    ## q : detrital feeders
    ## r : detritus compartments
    m<-M[1];n<-M[2];o<-M[3];p<-M[4];q<-M[5];r<-M[6];
    l <- m+n+o+p+q+r #Total system size
                                        #Primary producers
    L1 <- matrix(0,nrow=l,ncol=m)
                                        #Grazers
    L2 <- matrix(0,nrow=l,ncol=n)
    if(length(L2)>0){
    for (i in 1:n) {
        nn=sample(1:m,1)
        for(j in 1:nn) {
            nnn <- sample(1:m,1)  ## Select inflows to grazers
            L2[nnn,i]=1           ## Add 1 in the appropriate posn in adj matrix
        }
    }
    }
                                        #Omnivores
                                        #Feed from 1 to m+n+o+p+q other components
    L3<-matrix(0,nrow=l,ncol=o)
    if(length(L3)>0){
    for(i in 1:o) {
    	oo <- sample(1:(m+n+o+p+q),1)
    	for(j in 1:oo) {
    		ooo<-sample(1:(m+n+o+p+q),1)        ## Select inflows to omnis
    		L3[ooo,i]<-1                      ## Add 1 in the appropriate posn in adj matrix
    	}
    }
       }
                                       #Carnivores
                                       #Feed on grazers,carnis,omnis,detrital_fdrs
    L4<-matrix(0,nrow=l,ncol=p)
    if(length(L4)>0){
    for(i in 1:p) {
    	pp<-sample(1:(n+o+p+q),1)
    	for(j in 1:pp){
    		ppp<-sample(1:(n+o+p+q),1)   ## Select inflows to carnivores
    		L4[(ppp+m),i]<-1             ## Add 1 in the appropriate posn in adj matrix
    	}
    }
    }
    
                                       #Detrital feeders
                                       #Feed only on detritus
    L5<-matrix(0,nrow=l,ncol=q)
    if(length(L5)>0){
    for(i in 1:q){
    	qq<-sample(1:r,1)
    	for(j in 1:qq){
    		qqq<-sample(1:r,1)       ## Select inflows to Detrital Feeders
    		L5[(qqq+m+n+o+p+q),i]<-1 ## Add 1 in the appropriate posn in adj matrix
    	}
    }
    }
                                       #Detrital Compartments
                                       #All other compartments end up as detritus
    L6<-matrix(0,nrow=l,ncol=r)
    if(length(L6)>0){
    for(i in 1:l){
    	mm<-sample(1:r,1)
    	for(j in 1:mm){
    		mmm<-sample(1:mm,1)      ##Select inflows to detritus
    		L6[i,mmm]<-1
    	}
    }  
    }
    A<-cbind(L1,L2,L3,L4,L5,L6)
    A1<-A
    diag(A)<-0
    ssA<-sum(A)
    sizeA<-dim(A)
    eig<-eigen(A)$values
    i<-which(Mod(eig)==max(Mod(eigen(A)$values)))
    Con<-ssA/(l^2);max_eig<-eig[i]
    ns<-Re(cbind(l,Con,max_eig)   )   
    out<-list(A1=A1,A=A,links=ssA,size=sizeA,ns=ns)
    return(out)              

}#end of function

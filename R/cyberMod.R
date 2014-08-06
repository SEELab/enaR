### Formation of Cyber Models
### Singh P. | July 2014
### Algorithm : Fath 2004. Network Analysis applied to large scale cyber-ecosystems. Ecological Modelling 171 (2004) 329-337
### --------------------------------------------------------------
cyberMod<-function(M=NULL, A=NULL, inp=0, non.liv=0){
	if(length(M)!=0){
		A<-cyberAdj(M)$A
		non.liv<-M[6]
	}
	if(length(A)==0){stop('Enter valid M or A value')}
	#given an adjacency matrix A (rc config)
	n<-dim(A)[1]
	
	if(identical(inp,0)){
	z<-rep(0,n)
	Z<-which(apply(A,2,sum)==0)
	
	z[Z]<-100
	}
	else {z<-inp}
	I<-A*0; diag(I)<-1
	A<-A+I
	c<-which(A==0,arr.ind=TRUE)
	
	rand<-runif(n^2)
	q<-which(rand>1)
	while(length(rand[q])>0){
		rand[q]<-rand[q]/10
		q<-which(rand>1)
	}
	rand<-ceiling(rand*100)/100
	G<-matrix(rand,nrow=n,byrow=TRUE)
	
	G[c]<-0
	
	G1<-G*0; diag(G1)<-apply(G,1,sum) #normalize the elements by row sum
	
	G2<-(solve(G1))%*%G
	G<-G2
	Ig<-G*0; diag(Ig)<-diag(G)
	G<-G-Ig
	
	I<-G*0; diag(I)<-1
	N<-solve(I-G)
	
	F1<-G*0; diag(F1)<- z%*%N
	F<-F1%*%G
	#print(F) 
	                                          # Outputs
	y<-apply(F,2,sum)+z-(apply(F,1,sum))


                                          # Create network object
                                          
    Y<-network(F,loops=TRUE,directed=TRUE)
    set.network.attribute(Y,'flow',F)
    set.network.attribute(Y,'balanced',TRUE)
    set.edge.attribute(Y,'flow',F[F>0])
    set.vertex.attribute(Y,'input',z)
    set.vertex.attribute(Y,'output',y)
    set.vertex.attribute(Y,'export',y)
    storage<-rep(1,n)
    set.vertex.attribute(Y,'storage',storage)
    living<-rep(FALSE,n)
    living[1:(n-non.liv)]<-TRUE
    set.vertex.attribute(Y,'living',living)
    return(Y)
    	
}
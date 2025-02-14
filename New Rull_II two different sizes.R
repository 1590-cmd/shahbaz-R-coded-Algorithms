
################################################################################
# R-coded Algorithm Based on Cyclic Shifts (Rule II) to Generate Efficient Classes
# of Circular Balanced Neighbour Designs  for period of two different
# sizes(k1 and k2)
################################################################################

# Algorithm from paper:

# shahbaz et al (2024). 
# Classes of Minimal Circular balance NDs for period of two different 
# sizes(k1 and k2)  CBNDs2 -designs Generated with R 


# Coded by shahbaz et al., 01-08-2024 to 01-02-2025
# Version 2.0  (2024-10-02)
################################################################################



################################################################################
# Selection of i groups of size P1 and one group of size k2-2 from 
# adjusted A. 
################################################################################

grouping<-function(A,k,v,i){
  bs<-c()
  z=0;f=1
  A1=A
  while(f<=i){
    
    for(y in 1:5000){
      comp<-sample(1:length(A1),k[1])
      com<-A1[comp]
      cs<-sum(com)
      if(cs%%v==0){
        bs<-rbind(bs,com)
        A1<-A1[-comp]
        z<-z+1
        f=f+1
      }
      if(z==i) break
    }
    if(z<i) {bs<-c();z=0;f=1;A1=A}  
    
  }
  
  
  bs1<-t(apply(bs,1,sort))
  bs1<-cbind(bs1,rowSums(bs),rowSums(bs)/(v))
  rownames(bs1)<-paste("G",1:i, sep="")
  colnames(bs1)<-c(paste(1:k[1], sep=""),"sum" ,"sum/(v-1)")
  
  bs2<-t(apply(as.matrix(A1),1,sort))
  bs2<-cbind(bs2,rowSums(bs2),rowSums(bs2)/(v))
  rownames(bs2)<-paste("G",i+1, sep="")
  colnames(bs2)<-c(paste(1:(k[2]-2), sep=""),"sum" ,"sum/(v-1)")
  
  
  fs1<-t(apply(bs,1,sort))
  fs1<-delmin(fs1)
  rownames(fs1)<-paste("S",1:i, sep="")
  colnames(fs1)<-rep("",(k[1]-1))
  
  fs2<-t(apply(as.matrix(A1),1,sort))
  rownames(fs2)<-paste("S",i+1, sep="")
  colnames(fs2)<-rep("",(k[2]-2))
  
  
  list(B1=list(fs1,fs2),B2=list(bs1,bs2))
}

################################################################################
# Selection of i groups of size p1 from adjusted A. The set of remaining 
# (Un-selected) elements are saved in the object named as B2. 
################################################################################
grouping1<-function(A,k,v,i){
  bs<-c()
  z=0;f=1
  A1=A
  while(f<=i){
    
    for(y in 1:5000){
      comp<-sample(1:length(A1),k)
      com<-A1[comp]
      cs<-sum(com)
      if(cs%%v==0){
        bs<-rbind(bs,com)
        A1<-A1[-comp]
        z<-z+1
        f=f+1
      }
      if(z==i) break
    }
    if(z<i) {bs<-c();z=0;f=1;A1=A}  
  }
  list(B1=bs,B2=A1)
}

################################################################################
# Selection of i group of size k1 from adjusted A and one group of size k2
# from B2 and one group of size k2-2 from remaining set 
################################################################################
grouping2<-function(A,k,v,i){
  bs1<-c()
  j=i+1
  z=0;f=1
  A1=A
  while(f<=j){
    s<-grouping1(A1,k[1],v,i)
    A2<-s$B2
    z=i;f=f+i
    for(y in 1:1000){
      comp<-sample(1:length(A2),k[2])
      com<-A2[comp]
      cs<-sum(com)
      if(cs%%v==0){
        bs1<-rbind(bs1,com)
        A2<-A2[-comp]
        z<-z+1
        f=f+1
      }
      if(z==j) break
    }
    
    if(z<j) {bs1<-c();z=0;f=1;A1=A} 
  }
  
  
  gs1<-t(apply(s$B1,1,sort))
  gs1<-cbind(gs1,rowSums(gs1),rowSums(gs1)/v)
  rownames(gs1)<-paste("G",1:i, sep="")
  colnames(gs1)<-c(paste(1:k[1], sep=""),"sum" ,"sum/(v-1)")
  
  gs2<-t(apply(as.matrix(bs1),1,sort))
  gs2<-cbind(gs2,rowSums(gs2),rowSums(gs2)/v)
  rownames(gs2)<-paste("G",(nrow(gs1)+1):(nrow(gs1)+1), sep="")
  colnames(gs2)<-c(paste(1:k[2], sep=""),"sum" ,"sum/(v-1)")
  
  
  gs3<-t(apply(as.matrix(A2),1,sort))
  gs3<-cbind(gs3,rowSums(gs3),rowSums(gs3/v))
  rownames(gs3)<-paste("G",(nrow(gs1)+2):(nrow(gs1)+2), sep="")
  colnames(gs3)<-c(paste(1:(k[2]-2), sep=""),"sum" ,"sum/(v-1)")
  
  
  
  fs1<-t(apply(s$B1,1,sort))
  fs1<-delmin(fs1)
  rownames(fs1)<-paste("S",1:i, sep="")
  colnames(fs1)<-rep("",(k[1])-1)
  
  
  fs2<-t(apply(bs1,1,sort))
  fs2<-delmin(fs2)
  rownames(fs2)<-paste("S",(i+1):(i+1), sep="")
  colnames(fs2)<-rep("",(k[2]-1))
  
  fs3<-t(apply(as.matrix(A2),1,sort))
  rownames(fs3)<-paste("S",(i+2):(i+2), sep="")
  colnames(fs3)<-rep("",(k[2]-2))
  
  list(B1=list(fs1,fs2,fs3),B3=list(gs1,gs2,gs3))
}

#######################################################################
# Obtaing set(s) of shifts by deleting smallest value of group
#######################################################################

delmin<-function(z){
  fs<-c()
  n<-nrow(z)
  c<-ncol(z)-1
  for(i in 1:n){
    z1<-z[i,]
    z2<-z1[z1!=min(z1)]
    fs<-rbind(fs,z2)
  }
  return(fs)
}

################################################################################
# Selection of adjusted A and the set(s) of shifts to obtain Circular 
# MCBNDs for periods of two different
# sizes 
################################################################################

# D=1: period of two different sizes with one set of k2
# D=2: period of two different sizes with two set of k2

#   k: Vector of two different period sizes
#   i: Number of sets of shifts for k1



CGN2_2diffsize<-function(v,k,i,D=1,C=1){
  
  if(length(k)>2 | length(k)<2){stop("length(k)=2 ")}
  if(any(k<=2)!=0) stop("k=Block size: Each block size must be greater than 2")
  if(i<=0) stop("i= Must be a positive integer")
  if(k[1]<k[2]) stop("k1>k2")
  
  setClass( "stat_test", representation("list"))
  
  setMethod("show", "stat_test", function(object) {
    row <- paste(rep("=", 51), collapse = "")
    cat(row, "\n")
    
    
    if(D==1){
      cat("Following are required sets of shifts to obtain the 
Classes of Two Sided Circular Balanced Neighbour Designs  for period of 
two different sizes for", "v=" ,object$R[1], ",","k1=",object$R[2],
          "and","k2=",object$R[3],"\n")
      row <- paste(rep("=", 51), collapse = "")
      cat(row, "\n")
      print(object$S[[1]])
      cat("\n")
      print(object$S[[2]])
    }
    
    if(D==2){
      cat("Following are required sets of shifts to obtain the 
Classes of Circular Balanced Neighbour Designs  for period of 
two different sizes for", "v=" ,object$R[1], ",","k1=",object$R[2],
"and","k2=",object$R[3],"\n")
      
      row <- paste(rep("=", 51), collapse = "")
      cat(row, "\n")
      print(object$S[[1]])
      cat("\n")
      print(object$S[[2]])
      cat("\n")
      print(object$S[[3]])}
    
    
  })
  
  if(D==1){
    if(v%%2==0){
      if(C==1)
      {  
        v=i*k[1]+k[2]; m=(v-2)
        A=c(1,2:m)
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
        
      }
      if(C==2)
      {  
        v=i*k[1]+k[2]+1; m=(v-2)
        A<-c(1:((v-2)/2),((v+2)/2),((v+4)/2):m)
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
        
      }
      if(C==3)
      {  
        v=i*k[1]+k[2]+2; m=(v-2)
        A<-c(1:((v-2)/2),((v+4)/2),((v+6)/2):m)
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==4)
      {  
        v=i*k[1]+k[2]-1; m=(v-2)
        A<-c(1:m,(v/2))
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
        
      }
      if(C==5)
      {  
        v=i*k[1]+k[2]-2; m=(v-2)
        A<-c(1:m,(v/2),((v+2)/2))
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==6)
      {  
        v=i*k[1]+k[2]-1; m=(v-2)
        A=c(0,1,2:m)
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
        
      }
      if(C==7)
      {  
        v=i*k[1]+k[2]; m=(v-2)
        A<-c(0,1:((v-2)/2),((v+2)/2),((v+4)/2):m)
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
        
      }
      if(C==8)
      {  
        v=i*k[1]+k[2]+1; m=(v-2)
        A<-c(0,1:((v-2)/2),((v+4)/2),((v+6)/2):m)
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==9)
      {  
        v=i*k[1]+k[2]-2; m=(v-2)
        A<-c(0,1:m,(v/2))
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
        
      }
      if(C==10)
      {  
        v=i*k[1]+k[2]-3; m=(v-2)
        A<-c(0,1:m,(v/2),((v+2)/2))
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
    }
    if(v%%2!=0){
      if(C==1)
      {  
        v=i*k[1]+k[2]; m=(v-2)
        A=c(1,2:m)
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
        
      }
      if(C==2)
      {  
        v=i*k[1]+k[2]+1; m=(v-2)
        A<-c(1:((v-1)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
        
      }
      if(C==3)
      {  
        v=i*k[1]+k[2]+2; m=(v-2)
        A<-c(1:((v-3)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==4)
      {  
        v=i*k[1]+k[2]-1; m=(v-2)
        A<-c(1:m,((v+1)/2))
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
        
      }
      if(C==5)
      {  
        v=i*k[1]+k[2]-2; m=(v-2)
        A<-c(1:m,((v-1)/2),((v+1)/2))
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==6)
      {  
        v=i*k[1]+k[2]-1; m=(v-2)
        A=c(0,1,2:m)
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
        
      }
      if(C==7)
      {  
        v=i*k[1]+k[2]; m=(v-2)
        A<-c(0,1:((v-1)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
        
      }
      if(C==8)
      {  
        v=i*k[1]+k[2]+1; m=(v-2)
        A<-c(0,1:((v-3)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==9)
      {  
        v=i*k[1]+k[2]-2; m=(v-2)
        A<-c(0,1:m,((v+1)/2))
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
        
      }
      if(C==10)
      {  
        v=i*k[1]+k[2]-3; m=(v-2)
        A<-c(0,1:m,((v-1)/2),((v+1)/2))
        A1<-grouping(A,k,v=v-1,i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
    }  
  }    
  if(D==2){
    if(v%%2==0){
      if(C==1)
      {
        v=i*k[1]+2*k[2]; m=(v-2)
        A=c(1,2:m)
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==2)
      {
        v=i*k[1]+2*k[2]+1; m=(v-2)
        A<-c(1:((v-2)/2),((v+2)/2),((v+4)/2):m)
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==3)
      {
        v=i*k[1]+2*k[2]+2; m=(v-2)
        A<-c(1:((v-2)/2),((v+4)/2),((v+6)/2):m)
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==4)
      {
        v=i*k[1]+2*k[2]-1; m=(v-2)
        A<-c(1:m,(v/2))
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==5)
      {
        v=i*k[1]+2*k[2]-2; m=(v-2)
        A<-c(1:m,(v/2),((v+2)/2))
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==6)
      {
        v=i*k[1]+2*k[2]-1; m=(v-2)
        A=c(0,1,2:m)
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==7)
      {
        v=i*k[1]+2*k[2]; m=(v-2)
        A<-c(0,1:((v-2)/2),((v+2)/2),((v+4)/2):m)
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==8)
      {
        v=i*k[1]+2*k[2]+1; m=(v-2)
        A<-c(0,1:((v-2)/2),((v+4)/2),((v+6)/2):m)
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==9)
      {
        v=i*k[1]+2*k[2]-2; m=(v-2)
        A<-c(0,1:m,(v/2))
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==10)
      {
        v=i*k[1]+2*k[2]-3; m=(v-2)
        A<-c(0,1:m,(v/2),((v+2)/2))
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
    }
    if(v%%2!=0){
      if(C==1)
      {
        v=i*k[1]+2*k[2]; m=(v-2)
        A=c(1,2:m)
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==2)
      {
        v=i*k[1]+2*k[2]+1; m=(v-2)
        A<-c(1:((v-1)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==3)
      {
        v=i*k[1]+2*k[2]+2; m=(v-2)
        A<-c(1:((v-3)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==4)
      {
        v=i*k[1]+2*k[2]-1; m=(v-2)
        A<-c(1:m,(v+1)/2)
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==5)
      {
        v=i*k[1]+2*k[2]-2; m=(v-2)
        A<-c(1:m,((v-1)/2),((v+1)/2))
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==6)
      {
        v=i*k[1]+2*k[2]-1; m=(v-2)
        A=c(0,1,2:m)
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==7)
      {
        v=i*k[1]+2*k[2]; m=(v-2)
        A<-c(0,1:((v-1)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==8)
      {
        v=i*k[1]+2*k[2]+1; m=(v-2)
        A<-c(0,1:((v-3)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==9)
      {
        v=i*k[1]+2*k[2]-2; m=(v-2)
        A<-c(0,1:m,(v-1)/2)
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
      if(C==10)
      {
        v=i*k[1]+2*k[2]-3; m=(v-2)
        A<-c(0,1:m,((v-1)/2),((v+1)/2))
        A1<-grouping2(A,k,v=(v-1),i)
        A2<-c(v,k);names(A2)<-c("V","k1","k2")
        x<-list(S=A1$B1,G=A1$B2,R=A2,A=A)
      }
    }   
  }    
  new("stat_test", x) 
} 

##################################################################
# Generation of design using sets of cyclical shifts
###################################################################
# H is an output object from CNDs_2diffsize
# The output is called using the design_CBND to generate design
design<-function(H){
  
  setClass( "CWBND_design", representation("list"))
  setMethod("show", "CWBND_design", function(object) {
    row <- paste(rep("=", 51), collapse = "")
    cat(row, "\n")
    cat("Following is minimal CBND for", "v=" ,object$R[1], "and","k=",object$R[2], "\n")
    row <- paste(rep("=", 51), collapse = "")
    cat(row, "\n")
    for(i in 1:length(ss)){
      W<-ss[[i]]
      nr<-dim(W)[1]
      for(j in 1:nr){
        print(object$Design[[i]][[j]])
        cat("\n\n")
      }}
  })  
  
  v<-H$R[1]
  p<-H$R[2]
  ss<-H$S  
  treat<-(1:v)-1
  fn<-(1:v)
  G<-list()
  
  
  for(j in 1:length(ss)){ 
    W<-ss[[j]]
    nr<-dim(W)[1]
    nc<-dim(W)[2]
    D<-list()
    
    for(i in 1:nr){
      dd<-c()
      d1<-matrix(treat,(nc+1),v,byrow = T)
      ss1<-cumsum(c(0,W[i,]))
      dd2<-d1+ss1
      dd<-rbind(dd,dd2)
      rr<-dd[which(dd>=v)]%%v
      dd[which(dd>=v)]<-rr
      colnames(dd)<-paste("B",fn, sep="")
      rownames(dd)<-rep("",(nc+1))
      fn<-fn+v
      D[[i]]<-dd
    }
    G[[j]]<-D
    
  }
  
  x<-list(Design=G,R=H$R)
  new("CWBND_design", x)
}

###############################################################################
# Examples: Using CBNDs_2diffsize function to obtain the set(s) of shifts
# for construction of Circular BNDs for period of 
# two different sizes (k1 and k2)
###############################################################################

# Example#1
(H<-CGN2_2diffsize(k=c(9,5),v=4,i=1,C=10,D=1))
H$G
(design(H))


# Example#2
(H<-CGN2_2diffsize(k=c(9,7), v=11, i=1,D=2)) 
(design(H))

# Example#3
(H<-CGN2_2diffsize(k=c(6,5),i=2,D=1)) 
(design(H))

# Example#4
(H<-CGN2_2diffsize(k=c(7,5),i=5,D=1))
(design(H))

# Example#5
(H<-CGN2_2diffsize(k=c(4,3),i=3,D=2))
(design(H))

# Example#6
(H<-CGN2_2diffsize(k=c(6,5),i=4,D=2))
(design(H))

# Example#7
(H<-CGN2_2diffsize(k=c(6,5),i=3,D=2))
(design(H))







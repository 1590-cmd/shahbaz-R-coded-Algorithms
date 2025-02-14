
################################################################################
# R-coded Algorithm Based on Cyclic Shifts (Rule II) to Generate Efficient Classes
# of Circular Balanced Neighbour Designs  for period of three 
# sizes(k1, k2 and k3)
################################################################################

# Algorithm from paper:

# shahbaz et al (2024). 
# Classes of Minimal Circular balance RMDs for period of three different 
# sizes(k1, k2 and k3)  CBNDs2 -designs Generated with R 


# Coded by shahbaz et al., 01-08-2024 to 01-02-2025
# Version 2.0  (2024-10-02)
################################################################################




################################################################################
# Selection of i group of size k1 from adjusted A. The set of remaining 
# (Un-selected) elements are saved in object B2. 
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
# Selection of i group of size k1 from adjusted A and selection of required 
# number of groups of size k2 from B2. The set of remaining (Unselected) 
# elements are saved in B3.
################################################################################

grouping2<-function(A,k,v,i,sk2){
  bs1<-c()
  j=i+sk2
  z=0;f=1
  A1=A
  while(f<=j){
    s<-grouping1(A1,k[1],v,i)
    A2<-s$B2
    z=i;f=f+i
    for(y in 1:2000){
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
  
  list(B1=s$B1,B2=bs1,B3=A2)
}

################################################################################
# Selection of i group of size k1 from adjusted A, selection of required number 
# of groups of size k2 from B2, desired group of size k3
# from B3 and one group of size k3-2 from remaining set.
################################################################################
grouping3<-function(A,k,v,i,sk2,sk3){
  bs1<-c()
  j=i+sk2+sk3
  z=0;f=1
  A1=A
  while(f<=j){
    s<-grouping2(A1,k,v,i,sk2)
    A3<-s$B3
    z=i+sk2;f=f+i+sk2
    for(y in 1:1000){
      comp<-sample(1:length(A3),k[3])
      com<-A3[comp]
      cs<-sum(com)
      if(cs%%v==0){
        bs1<-rbind(bs1,com)
        A3<-A3[-comp]
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
  colnames(gs1)<-c(paste(1:k[1], sep=""),"sum" ,"sum/v")
  
  gs2<-t(apply(s$B2,1,sort))
  gs2<-cbind(gs2,rowSums(gs2),rowSums(gs2)/v)
  rownames(gs2)<-paste("G",(i+1):(i+sk2), sep="")
  colnames(gs2)<-c(paste(1:k[2], sep=""),"sum" ,"sum/v")
  
  
  gs3<-t(apply(as.matrix(bs1),1,sort))
  gs3<-cbind(gs3,rowSums(gs3),rowSums(gs3)/v)
  rownames(gs3)<-paste("G",(i+sk2+1):(i+sk2+sk3), sep="")
  colnames(gs3)<-c(paste(1:k[3], sep=""),"sum" ,"sum/v")
  
  gs4<-t(apply(as.matrix(A3),1,sort))
  gs4<-cbind(gs4,rowSums(gs4),rowSums(gs4)/v)
  rownames(gs4)<-paste("G",(i+sk2+sk3+1), sep="")
  colnames(gs4)<-c(paste(1:(k[3]-2), sep=""),"sum" ,"sum/(v-1)")
  
  
  fs1<-t(apply(s$B1,1,sort))
  fs1<-delmin(fs1)
  rownames(fs1)<-paste("S",1:i, sep="")
  colnames(fs1)<-rep("",(k[1])-1)
  
  fs2<-t(apply(s$B2,1,sort))
  fs2<-delmin(fs2)
  rownames(fs2)<-paste("S",(i+1):(i+sk2), sep="")
  colnames(fs2)<-rep("",(k[2])-1)
  
  
  fs3<-t(apply(as.matrix(bs1),1,sort))
  fs3<-delmin(fs3)
  rownames(fs3)<-paste("S",(i+sk2+1):(i+sk2+sk3), sep="")
  colnames(fs3)<-rep("",(k[3]-1))
  
  fs4<-t(apply(as.matrix(A3),1,sort))
  rownames(fs4)<-paste("S",(i+sk2+sk3+1), sep="")
  colnames(fs4)<-rep("",(k[3]-2))
  
  
  list(B1=list(fs1,fs2,fs3,fs4),B4=list(gs1,gs2,gs3,gs4))
}

################################################################################
# Selection of i group of size k1 from adjusted A and desired group of size k2
# from B2 and one group of size k2-2 from remaining set. 
################################################################################
grouping4<-function(A,k,v,i,sk2){
  bs1<-c()
  j=i+sk2
  z=0;f=1
  A1=A
  while(f<=j){
    s<-grouping1(A1,k[1],v,i)
    A2<-s$B2
    z=i;f=f+i
    for(y in 1:2000){
      comp<-sample(1:length(A2),k[2])
      com<-A2[comp]
      cs<-sum(com)
      if(cs%%v==0){
        bs1<-rbind(bs1,com)
        A2<-A2[-comp]
        z<-z+1
        f=f+1
      }
      if(z==j) {break}
    }
    if(z<j) {bs1<-c();z=0;f=1;A1=A}  
  }
  
  
  gs1<-t(apply(s$B1,1,sort))
  gs1<-cbind(gs1,rowSums(gs1),rowSums(gs1)/v)
  rownames(gs1)<-paste("G",1:i, sep="")
  colnames(gs1)<-c(paste(1:k[1], sep=""),"sum" ,"sum/(v-1)")
  
  gs2<-t(apply(as.matrix(bs1),1,sort))
  gs2<-cbind(gs2,rowSums(gs2),rowSums(gs2)/v)
  rownames(gs2)<-paste("G",(nrow(gs1)+1):(nrow(gs1)+sk2), sep="")
  colnames(gs2)<-c(paste(1:k[2], sep=""),"sum" ,"sum/(v-1)")
  
  
  gs3<-t(apply(as.matrix(A2),1,sort))
  gs3<-cbind(gs3,rowSums(gs3),rowSums(gs3)/v)
  rownames(gs3)<-paste("G",(nrow(gs1)+sk2+1), sep="")
  colnames(gs3)<-c(paste(1:(k[3]-2), sep=""),"sum" ,"sum/(v-1)")
  
  
  
  fs1<-t(apply(s$B1,1,sort))
  fs1<-delmin(fs1)
  rownames(fs1)<-paste("S",1:i, sep="")
  colnames(fs1)<-rep("",(k[1])-1)
  
  
  fs2<-t(apply(bs1,1,sort))
  fs2<-delmin(fs2)
  rownames(fs2)<-paste("S",(i+1):(i+sk2), sep="")
  colnames(fs2)<-rep("",(k[2]-1))
  
  fs3<-t(apply(as.matrix(A2),1,sort))
  rownames(fs3)<-paste("S",(i+sk2+1), sep="")
  colnames(fs3)<-rep("",(k[3]-2))
  
  list(B1=list(fs1,fs2,fs3),B3=list(gs1,gs2,gs3))
}


#######################################################################
# Obtain set(s) of shifts by deleting smallest value from group
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
# BNDs for period of three different
# sizes.
################################################################################

# D=1: period of three different sizes with one set of p2
# D=2: period of three different sizes with two set of p2
# D=3: period of three different sizes with one set of p2 and one set of p3
# D=4: period of three different sizes with two set of p2 and one set of p3

#   k: Vector of three different block sizes 
#   i: Number of sets of shifts for k1



CGN2_3diffsize<-function(v,C,k,i,D=1){
  
  if(length(k)>3 | length(k)<3){stop("length(k)=3")}
  if(any(k<=2)!=0) stop("p=period size: Each block size must be greater than 2")
  if(i<=0) stop("i=must be a positive integer")
  if(k[1]==k[2] | k[2]==k[3] |  k[1]==k[3]  ) stop("k1 NOT EQUAL TO k2 NOT EQUAL TO k3")
  
  setClass( "stat_test", representation("list"))
  
  setMethod("show", "stat_test", function(object) {
    row <- paste(rep("=", 52), collapse = "")
    cat(row, "\n")
    
    if(D==1 | D==2){
      cat("Following are required sets of shifts to obtain the 
Classes of Circular Balanced Neighbour Designs for block of 
three different sizes for","v=" ,object$R[1], ",","k1=",object$R[2],",","k2=",object$R[3],"and","k3=",object$R[4],"\n")
      row <- paste(rep("=", 52), collapse = "")
      cat(row, "\n")
      print(object$S[[1]])
      cat("\n")
      print(object$S[[2]])
      cat("\n")
      print(object$S[[3]])
      
    }
    
    if(D==3 | D==4){
      cat("Following are required sets of shifts to obtain the 
Classes of Circular Balanced Neighbour Designs for block of 
three differnt sizes for","v=" ,object$R[1], ",","k1=",object$R[2],",","k2=",object$R[3],"and","k3=",object$R[4],"\n")
      row <- paste(rep("=", 52), collapse = "")
      
      cat(row, "\n")
      print(object$S[[1]])
      cat("\n")
      print(object$S[[2]])
      cat("\n")
      print(object$S[[3]])
      cat("\n")
      print(object$S[[4]])}
  })
  
  if(D==1){
    if(v%%2==0){
      if(C==1)
      {  
        v=i*k[1]+k[2]+k[3]; m=(v-2)
        A=c(1,2:m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==2)
      {  
        v=i*k[1]+k[2]+k[3]+1; m=(v-2)
        A<-c(1:((v-2)/2),((v+2)/2),((v+4)/2):m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==3)
      {  
        v=i*k[1]+k[2]+k[3]+2; m=(v-2)
        A<-c(1:((v-2)/2),((v+4)/2),((v+6)/2):m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==4)
      {  
        v=i*k[1]+k[2]+k[3]-1; m=(v-2)
        A<-c(1:m,(v/2))
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==5)
      {  
        v=i*k[1]+k[2]+k[3]-2; m=(v-2)
        A<-c(1:m,(v/2),((v+2)/2))
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==6)
      {  
        v=i*k[1]+k[2]+k[3]-1; m=(v-2)
        A=c(0,1,2:m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==7)
      {  
        v=i*k[1]+k[2]+k[3]; m=(v-2)
        A<-c(0,1:((v-2)/2),((v+2)/2),((v+4)/2):m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==8)
      {  
        v=i*k[1]+k[2]+k[3]+1; m=(v-2)
        A<-c(0,1:((v-2)/2),((v+4)/2),((v+6)/2):m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==9)
      {  
        v=i*k[1]+k[2]+k[3]-2; m=(v-2)
        A<-c(0,1:m,(v/2))
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==10)
      {  
        v=i*k[1]+k[2]+k[3]-3; m=(v-2)
        A<-c(0,1:m,(v/2),((v+2)/2))
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
    }
    if(v%%2!=0){
      if(C==1)
      {  
        v=i*k[1]+k[2]+k[3]; m=(v-2)
        A=c(1,2:m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==2)
      {  
        v=i*k[1]+k[2]+k[3]+1; m=(v-2)
        A<-c(1:((v-1)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==3)
      {  
        v=i*k[1]+k[2]+k[3]+2; m=(v-2)
        A<-c(1:((v-3)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==4)
      {  
        v=i*k[1]+k[2]+k[3]-1; m=(v-2)
        A<-c(1:m,(v+1/2))
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==5)
      {  
        v=i*k[1]+k[2]+k[3]-2; m=(v-2)
        A<-c(1:m,((v-1)/2),((v+1)/2))
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==6)
      {  
        v=i*k[1]+k[2]+k[3]-1; m=(v-2)
        A=c(0,1,2:m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==7)
      {  
        v=i*k[1]+k[2]+k[3]; m=(v-2)
        A<-c(0,1:((v-1)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==8)
      {  
        v=i*k[1]+k[2]+k[3]+1; m=(v-2)
        A<-c(0,1:((v-3)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==9)
      {  
        v=i*k[1]+k[2]+k[3]-2; m=(v-2)
        A<-c(0,1:m,((v+1)/2))
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==10)
      {  
        v=i*k[1]+k[2]+k[3]-3; m=(v-2)
        A<-c(0,1:m,((v-1)/2),((v+1)/2))
        A1<-grouping4(A,k,v=(v-1),i,sk2=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
    }  
  }    
  
  if(D==2){
    if(v%%2==0){
      if(C==1)
      {  
        v=i*k[1]+k[2]+k[3]; m=(v-2)
        A=c(1,2:m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==2)
      {  
        v=i*k[1]+k[2]+k[3]+1; m=(v-2)
        A<-c(1:((v-2)/2),((v+2)/2),((v+4)/2):m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==3)
      {  
        v=i*k[1]+k[2]+k[3]+2; m=(v-2)
        A<-c(1:((v-2)/2),((v+4)/2),((v+6)/2):m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==4)
      {  
        v=i*k[1]+k[2]+k[3]-1; m=(v-2)
        A<-c(1:m,(v/2))
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==5)
      {  
        v=i*k[1]+k[2]+k[3]-2; m=(v-2)
        A<-c(1:m,(v/2),((v+2)/2))
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==6)
      {  
        v=i*k[1]+k[2]+k[3]-1; m=(v-2)
        A=c(0,1,2:m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==7)
      {  
        v=i*k[1]+k[2]+k[3]; m=(v-2)
        A<-c(0,1:((v-2)/2),((v+2)/2),((v+4)/2):m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==8)
      {  
        v=i*k[1]+k[2]+k[3]+1; m=(v-2)
        A<-c(0,1:((v-2)/2),((v+4)/2),((v+6)/2):m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==9)
      {  
        v=i*k[1]+k[2]+k[3]-2; m=(v-2)
        A<-c(0,1:m,(v/2))
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==10)
      {  
        v=i*k[1]+k[2]+k[3]-3; m=(v-2)
        A<-c(0,1:m,(v/2),((v+2)/2))
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
    }
    if(v%%2!=0){
      if(C==1)
      {  
        v=i*k[1]+k[2]+k[3]; m=(v-2)
        A=c(1,2:m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==2)
      {  
        v=i*k[1]+k[2]+k[3]+1; m=(v-2)
        A<-c(1:((v-1)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==3)
      {  
        v=i*k[1]+k[2]+k[3]+2; m=(v-2)
        A<-c(1:((v-3)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==4)
      {  
        v=i*k[1]+k[2]+k[3]-1; m=(v-2)
        A<-c(1:m,((v+1)/2))
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==5)
      {  
        v=i*k[1]+k[2]+k[3]-2; m=(v-2)
        A<-c(1:m,((v-1)/2),((v+1)/2))
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==6)
      {  
        v=i*k[1]+k[2]+k[3]-1; m=(v-2)
        A=c(0,1,2:m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==7)
      {  
        v=i*k[1]+k[2]+k[3]; m=(v-2)
        A<-c(0,1:((v-1)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==8)
      {  
        v=i*k[1]+k[2]+k[3]+1; m=(v-2)
        A<-c(0,1:((v-3)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==9)
      {  
        v=i*k[1]+k[2]+k[3]-2; m=(v-2)
        A<-c(0,1:m,((v+1)/2))
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
      if(C==10)
      {  
        v=i*k[1]+k[2]+k[3]-3; m=(v-2)
        A<-c(0,1:m,((v-1)/2),((v+1)/2))
        A1<-grouping4(A,k,v=(v-1),i,sk2=2)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B3,R=A2,A=A)
      }
    }   
  }    
  if(D==3){
    if(v%%2==0){
      if(C==1)
      {
        v=i*k[1]+2*k[2]+k[3]; m=(v-2)
        A=c(1,2:m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==2)
      {
        v=i*k[1]+2*k[2]+k[3]+1; m=(v-2)
        A<-c(1:((v-2)/2),((v+2)/2),((v+4)/2):m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==3)
      {
        v=i*k[1]+2*k[2]+k[3]+2; m=(v-2)
        A<-c(1:((v-2)/2),((v+4)/2),((v+6)/2):m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==4)
      {
        v=i*k[1]+2*k[2]+k[3]-1; m=(v-2)
        A<-c(1:m,(v/2))
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==5)
      {
        v=i*k[1]+2*k[2]+k[3]-2; m=(v-2)
        A<-c(1:m,(v/2),((v+2)/2))
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==6)
      {
        v=i*k[1]+2*k[2]+k[3]-1; m=(v-2)
        A=c(0,1,2:m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==7)
      {
        v=i*k[1]+2*k[2]+k[3]; m=(v-2)
        A<-c(0,1:((v-2)/2),((v+2)/2),((v+4)/2):m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==8)
      {
        v=i*k[1]+2*k[2]+k[3]+1; m=(v-2)
        A<-c(0,1:((v-2)/2),((v+4)/2),((v+6)/2):m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==9)
      {
        v=i*k[1]+2*k[2]+k[3]-2; m=(v-2)
        A<-c(0,1:m,(v/2))
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==10)
      {
        v=i*k[1]+2*k[2]+k[3]-3; m=(v-2)
        A<-c(0,1:m,(v/2),((v+2)/2))
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
    }
    if(v%%2!=0){
      if(C==1)
      {
        v=i*k[1]+2*k[2]+k[3]; m=(v-2)
        A=c(1,2:m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==2)
      {
        v=i*k[1]+2*k[2]+k[3]+1; m=(v-2)
        A<-c(1:((v-1)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==3)
      {
        v=i*k[1]+2*k[2]+k[3]+2; m=(v-2)
        A<-c(1:((v-3)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==4)
      {
        v=i*k[1]+2*k[2]+k[3]-1; m=(v-2)
        A<-c(1:m,((v+1)/2))
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==5)
      {
        v=i*k[1]+2*k[2]+k[3]-2; m=(v-2)
        A<-c(1:m,((v-1)/2),((v+1)/2))
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==6)
      {
        v=i*k[1]+2*k[2]+k[3]-1; m=(v-2)
        A=c(0,1,2:m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==7)
      {
        v=i*k[1]+2*k[2]+k[3]; m=(v-2)
        A<-c(0,1:((v-1)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==8)
      {
        v=i*k[1]+2*k[2]+k[3]+1; m=(v-2)
        A<-c(0,1:((v-3)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==9)
      {
        v=i*k[1]+2*k[2]+k[3]-2; m=(v-2)
        A<-c(0,1:m,((v+1)/2))
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==10)
      {
        v=i*k[1]+2*k[2]+k[3]-3; m=(v-2)
        A<-c(0,1:m,((v-1)/2),((v+1)/2))
        A1<-grouping3(A,k,v=(v-1),i,sk2=1,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
    }   
  }  
  
  if(D==4){
    if(v%%2==0){
      if(C==1)
      {
        v=i*k[1]+2*k[2]+k[3]; m=(v-2)
        A=c(1,2:m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==2)
      {
        v=i*k[1]+2*k[2]+k[3]+1; m=(v-2)
        A<-c(1:((v-2)/2),((v+2)/2),((v+4)/2):m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==3)
      {
        v=i*k[1]+2*k[2]+k[3]+2; m=(v-2)
        A<-c(1:((v-2)/2),((v+4)/2),((v+6)/2):m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==4)
      {
        v=i*k[1]+2*k[2]+k[3]-1; m=(v-2)
        A<-c(1:m,(v/2))
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==5)
      {
        v=i*k[1]+2*k[2]+k[3]-2; m=(v-2)
        A<-c(1:m,(v/2),((v+2)/2))
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==6)
      {
        v=i*k[1]+2*k[2]+k[3]-1; m=(v-2)
        A=c(0,1,2:m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==7)
      {
        v=i*k[1]+2*k[2]+k[3]; m=(v-2)
        A<-c(0,1:((v-2)/2),((v+2)/2),((v+4)/2):m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==8)
      {
        v=i*k[1]+2*k[2]+k[3]+1; m=(v-2)
        A<-c(0,1:((v-2)/2),((v+4)/2),((v+6)/2):m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==9)
      {
        v=i*k[1]+2*k[2]+k[3]-2; m=(v-2)
        A<-c(0,1:m,(v/2))
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==10)
      {
        v=i*k[1]+2*k[2]+k[3]-3; m=(v-2)
        A<-c(0,1:m,(v/2),((v+2)/2))
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
    }
    if(v%%2!=0){
      if(C==1)
      {
        v=i*k[1]+2*k[2]+k[3]; m=(v-2)
        A=c(1,2:m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==2)
      {
        v=i*k[1]+2*k[2]+k[3]+1; m=(v-2)
        A<-c(1:((v-1)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==3)
      {
        v=i*k[1]+2*k[2]+k[3]+2; m=(v-2)
        A<-c(1:((v-3)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==4)
      {
        v=i*k[1]+2*k[2]+k[3]-1; m=(v-2)
        A<-c(1:m,((v+1)/2))
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==5)
      {
        v=i*k[1]+2*k[2]+k[3]-2; m=(v-2)
        A<-c(1:m,((v-1)/2),((v+1)/2))
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==6)
      {
        v=i*k[1]+2*k[2]+k[3]-1; m=(v-2)
        A=c(0,1,2:m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==7)
      {
        v=i*k[1]+2*k[2]+k[3]; m=(v-2)
        A<-c(0,1:((v-1)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==8)
      {
        v=i*k[1]+2*k[2]+k[3]+1; m=(v-2)
        A<-c(0,1:((v-3)/2),((v+3)/2),((v+5)/2):m)
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==9)
      {
        v=i*k[1]+2*k[2]+k[3]-2; m=(v-2)
        A<-c(0,1:m,((v+1)/2))
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
      if(C==10)
      {
        v=i*k[1]+2*k[2]+k[3]-3; m=(v-2)
        A<-c(0,1:m,((v-1)/2),((v+1)/2))
        A1<-grouping3(A,k,v=(v-1),i,sk2=2,sk3=1)
        A2<-c(v,k);names(A2)<-c("V","k1","k2","k3")
        x<-list(S=A1$B1,G=A1$B4,R=A2,A=A)
      }
    }   
  }
  new("stat_test", x)  
}

###################################################################
# Generation of design using sets of cyclical shifts
###################################################################
# H is an output object from CGN2_3diffsize
# The output is called using the design_CBND to generate design
design<-function(H){
  
  setClass( "CWBND_design", representation("list"))
  setMethod("show", "CWBND_design", function(object) {
    row <- paste(rep("=", 51), collapse = "")
    cat(row, "\n")
    cat("Following is minimal CGN2 for", "v=" ,object$R[1], "and","k=",object$R[2], "\n")
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
  k<-H$R[2]
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
# Examples: Using CGN2_3diffsize function to obtain the set(s) of shifts
# for construction of Circular BNDs for period  of 
# three different sizes (k1, k2 and k3)
###############################################################################



# Example for case#1
(H<-CGN2_3diffsize(k=c(3,4,5),v=10,i=2,C=8,D=3))
(design(H))

(H<-CGN2_3diffsize(k=c(15,10,8),i=5,D=1))
(design(H))

(H<-CGN2_3diffsize(k=c(8,7,5),i=6,D=1))
(design(H))


# Example for case#2
(H<-CGN2_3diffsize(k=c(8,7,5),i=6,D=2))
(design(H))


(H<-CGN2_3diffsize(k=c(14,9,6),i=8,D=2))
(design(H))


(H<-CGN2_3diffsize(k=c(17,11,7),i=10,D=2))
(design(H))


# Example for case#3
(H<-CGN2_3diffsize(k=c(16,11,8),i=7,D=3))
(design(H))


(H<-CGN2_3diffsize(k=c(8,7,5),i=6,D=3))
(design(H))


# Example for case#4
(H<-CGN2_3diffsize(k=c(8,7,5),i=6,D=4))
(design(H))


(H<-CGN2_3diffsize(k=c(20,13,7),i=14,D=4))
(design(H))


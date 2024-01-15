k1=9
k2=5
V=40
v=V+1
y= 0
NB1=4  #No. of set of shifts for p1=k1 
NB2=1    ##No. of set of shifts for p2=k2
NB=5
T=c(5,8,10,11,25,27,28,38) #First set of shifts
T1=c(9,12,14,15,19,24,26,34) # Second and so on . . . .
T2=c(7,13,18,23,31,32,33,36)
T3=c(1,6,16,17,20,29,30,37)
T4=c(21,22,35)
#T5=c(22,29,47)
#T6=c(9,32,33,37)
#T7=c(35,38,39,40)
#T8=c(18,42,43,44)
#T9=c(23)
#T10=c(11,11,11)
#T11=c(12,12,12)
#T12=c(13,13,13)
#T13=c(14,14,14)
#T14=c(15,15,15)
#T15=c(16,16,16)
#T16=c(17,17,17)
#T17=c(18,18,18)
#T18=c(19,19,19)
#T19=c(20,20,20)
#T20=c(21,21,21)
#T21=c(22,22,22)
#T22=c(23,23,23)
#Increase 

#Initial blocks from set of shifts
IB=c(y,cumsum(T)%%v)
IB1=c(y,cumsum(T1)%%v)
IB2=c(y,cumsum(T2)%%v)
IB3=c(y,cumsum(T3)%%v)
IB4=c(y,cumsum(T4)%%v)
#IB5=c(y,cumsum(T5)%%v)
#IB6=c(y,cumsum(T6)%%v)
#IB7=c(y,cumsum(T7)%%v)
#IB8=c(y,cumsum(T8)%%v)
#IB9=c(y,cumsum(T9)%%v)
#IB10=c(y,cumsum(T10)%%v)
#IB11=c(y,cumsum(T11)%%v)
#IB12=c(y,cumsum(T12)%%v)
#IB13=c(y,cumsum(T13)%%v)
#IB14=c(y,cumsum(T14)%%v)
#IB15=c(y,cumsum(T15)%%v)
#IB16=c(y,cumsum(T16)%%v)
#IB17=c(y,cumsum(T17)%%v)
#IB18=c(y,cumsum(T18)%%v)
#IB19=c(y,cumsum(T19)%%v)
#IB20=c(y,cumsum(T20)%%v) 
#IB21=c(y,cumsum(T21)%%v)
#IB22=c(y,cumsum(T22)%%v)#Increase
p=seq(from=0, to=V-1, by=1)
#print(ww3)
l=NULL;l1=NULL;l2=NULL;l3=NULL;l4=NULL;l5=NULL;l6=NULL;l7=NULL;l8=NULL;l9=NULL;l10=NULL;l11=NULL;l12=NULL;l13=NULL;l14=NULL;l15=NULL;l16=NULL;l17=NULL;l18=NULL;l19=NULL;l20=NULL;l21=NULL;l22=NULL

for(i in 1:k1){
  for(j in 1:V){
    l=c(l,rep((IB[i]+p[j]+V)%% V))
    l1=c(l1,rep((IB1[i]+p[j]+V)%% V))
    l2=c(l2,rep((IB2[i]+p[j]+V)%% V))
    l3=c(l3,rep((IB3[i]+p[j]+V)%% V))
    #l4=c(l4,rep((IB4[i]+p[j]+V)%% V))
    #l5=c(l5,rep((IB5[i]+p[j]+V)%% V))
    
  }}
for(i in 1:k2){
  for(j in 1:V){
    #l=c(l,rep((IB[i]+p[j]+V)%% V))
     #l1=c(l1,rep((IB1[i]+p[j]+V)%% V))
    #l2=c(l2,rep((IB2[i]+p[j]+V)%% V))
     #l3=c(l3,rep((IB3[i]+p[j]+V)%% V))
    l4=c(l4,rep((IB4[i]+p[j]+V)%% V))
    #l5=c(l5,rep((IB5[i]+p[j]+V)%% V))
    #l6=c(l6,rep((IB6[i]+p[j]+V)%% V))
    
  }}

g= matrix(c(l),nrow=k1,ncol=V,byrow =  TRUE )
g1= matrix(c(l1),nrow=k1,ncol=V,byrow =  TRUE)
g2= matrix(c(l2),nrow=k1,ncol=V,byrow =  TRUE)
g3= matrix(c(l3),nrow=k1,ncol=V,byrow =  TRUE)
g4= matrix(c(l4),nrow=k2,ncol=V,byrow =  TRUE)
#g5= matrix(c(l5),nrow=k2,ncol=V,byrow =  TRUE)
#g6= matrix(c(l6),nrow=k2,ncol=V,byrow =  TRUE)
G1=cbind(g,g1,g2,g3)

gg1=cbind(g4)
G2=replace(gg1,which(is.na(gg1)),V)
#G=as.data.frame(GG)
#G[is.na(GGG)]<-7
#G=replace(GG,which(is.na(GG)),V-1)
print (G1)
print (G2)


P1=kronecker(matrix(1,nrow=(v-1)*NB1),diag(nrow = k1))
print(P1)
P2=kronecker(matrix(1,nrow=(v-1)*NB2),diag(nrow = k2))
print(P1)
U1=kronecker(diag(nrow = (v-1)*NB1),matrix(1,nrow=k1))
print(U1)
U2=kronecker(diag(nrow = (v-1)*NB2),matrix(1,nrow=k2))
print(U2)
wrapind1 <- function(i,n1)
  ifelse((r <- i %% n1) == 0, n1, r)
n1 <- nrow(G1)
incmat1 <- matrix(0,ncol=v,nrow=prod(dim(G1)),
                 dimnames=list(NULL,0:(v-1)))
m1 <- 1
for (i in seq(ncol(G1)))
  for (j in seq(nrow(G1))) {
    if(j==1){
      tt <- table(as.character(G1[wrapind1(j-1,n1),i]))
    }
    # if(j>1&j<k){
    #  tt <- table(as.character(G[wrapind(c(j-1,j+1),n),i]))
    #}
    else{
      tt <- table(as.character(G1[wrapind1(j-1,n1),i]))
    }
    
    incmat1[m1,names(tt)] <- tt
    m1 <- m1+1
  }
#for (i in seq(ncol(G)))
# for (j in seq(nrow(G))) {
#  tt <- table(as.character(G[wrapind(c(j-1),n),i]))
# incmat[m,names(tt)] <- tt
#  m <- m+1
#}
wrap1 <- function(i,n1)
  ifelse((R <- i %% n1) == 0, n1, R)
n1 <- nrow(G1)
incma1 <- matrix(0,ncol=v,nrow=prod(dim(G1)),
                dimnames=list(NULL,0:(v-1)))
u1 <- 1
for (i in seq(ncol(G1)))
  for (j in seq(nrow(G1))) {
    ttt<- table(as.character(G1[wrap1((j),n1),i]))
    incma1[u1,names(ttt)] <- ttt
    u1 <- u1+1
  }
wrapind2 <- function(i,n2)
  ifelse((r <- i %% n2) == 0, n2, r)
n2 <- nrow(G2)
incmat2 <- matrix(0,ncol=v,nrow=prod(dim(G2)),
                  dimnames=list(NULL,0:(v-1)))
m2 <- 1
for (i in seq(ncol(G2)))
  for (j in seq(nrow(G2))) {
    if(j==1){
      tt <- table(as.character(G2[wrapind2(j-1,n2),i]))
    }
    # if(j>1&j<k){
    #  tt <- table(as.character(G[wrapind(c(j-1,j+1),n),i]))
    #}
    else{
      tt <- table(as.character(G2[wrapind2(j-1,n2),i]))
    }
    
    incmat2[m2,names(tt)] <- tt
    m2 <- m2+1
  }
#for (i in seq(ncol(G)))
# for (j in seq(nrow(G))) {
#  tt <- table(as.character(G[wrapind(c(j-1),n),i]))
# incmat[m,names(tt)] <- tt
#  m <- m+1
#}
wrap2 <- function(i,n2)
  ifelse((R <- i %% n2) == 0, n2, R)
n2 <- nrow(G2)
incma2 <- matrix(0,ncol=v,nrow=prod(dim(G2)),
                 dimnames=list(NULL,0:(v-1)))
u2 <- 1
for (i in seq(ncol(G2)))
  for (j in seq(nrow(G2))) {
    ttt<- table(as.character(G2[wrap2((j),n2),i]))
    incma2[u2,names(ttt)] <- ttt
    u2 <- u2+1
  }
incma=rbind(incma1,incma2)
incmat=rbind(incmat1,incmat2)
M1=t(incmat1)%*%U1
M2=t(incmat2)%*%U2
N1=t(incma1)%*%U1
print(n1)
N2=t(incma2)%*%U2
print(n2)
CM=N1%*%t(N1)+(N2%*%t(N2))-2*diag(v)
M1M2=M1%*%t(M1)+(M2%*%t(M2))-2*diag(v)
NN=(1/k1*(N1%*%t(N1)))+(1/k2*(N2%*%t(N2)))
L1=t(incma1)%*%incmat1
L2=t(incma2)%*%incmat2
MM=(1/k1*(M1%*%t(M1)))+(1/k2*(M2%*%t(M2)))
nm=(1/k1*(N1%*%t(M1)))+(1/k2*(N2%*%t(M2)))
L=t(incma)%*%incmat

theta=t(incma)%*%incma-NN
er=eigen(theta)
ER=er$values
EER=ER[!ER%in%ER[v]]
EV=(v-1)/sum(1/EER)
Edirect=EV/CM[1,1]


si=t(incmat)%*%incmat-MM
ESI=eigen(si)
EESI=ESI$values
ESR=EESI[!EESI%in%EESI[v]]
ERR=(v-1)/sum(1/ESR)
Ecarryover=ERR/M1M2[1,1]
PI=((L-nm))
L=t(incma)%*%incmat
NO=matrix(c(1),nrow=v,ncol=1)
CN=cbind(NO,L)
CHS=chisq.test(CN)
print(CHS)
SUME= sum(CN)
value=CHS$statistic
VC=sqrt(((value)/SUME)/(min(v-1,v)))
ES=(1-VC)
print(ES)

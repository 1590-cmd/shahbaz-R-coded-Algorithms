k1=13
k2=10
k3=7
V=53
v=V+1
y= 0
NB1=3 #No. of set of shifts for p1=k1 
NB2=1   ##No. of set of shifts for p2=k2
NB3=1

T=c(33,2,3,4,5,6,7,8,9,10,11,12) #First set of shifts
T1=c(14,15,16,17,18,19,37,21,22,23,24,26)# Second and so on . . . .
T2=c(52,50,28,29,39,30,32,1,34,35,36,38)
T3=c(25,40,41,42,43,44,45,46,47)
T4=c(0,27,27,48,31)
#T5=c(0,36,36,67,68,57,56)
#T6=c(1,2,3,20)
#T7=c(10,19)
#T8=c(5,6,9,17)
#T9=c(13,15)
#T10=c(10,28,19)
#T11=c(10,28,19)
#T12=c(10,28,19)
#T13=c(10,28,19)
#T14=c(10,28,19)
#T15=c(10,28,19)
#T16=c(10,28,19)
#T17=c(10,28,19)
#T18=c(10,28,19)
#T19=c(10,28,19)
#T20=c(10,28,19)
#T21=c(10,28,19)
IB=c(y,cumsum(T)%%V)
IB1=c(y,cumsum(T1)%%V)
IB2=c(y,cumsum(T2)%%V)
IB3=c(y,cumsum(T3)%%V)
IB4=c(y,cumsum(T4)%%V)
#IB5=c(y,cumsum(T5)%%V)
#IB6=c(y,cumsum(T6)%%V)
#IB7=c(y,cumsum(T7)%%V)
#IB8=c(y,cumsum(T8)%%V)
#IB9=c(y,cumsum(T9)%%V)
#IB10=c(y,cumsum(T10)%%V)
#IB11=c(y,cumsum(T11)%%V)
#IB12=c(y,cumsum(T12)%%V)
#IB13=c(y,cumsum(T13)%%V)
#IB14=c(y,cumsum(T14)%%V)
#IB15=c(y,cumsum(T15)%%V)
#IB16=c(y,cumsum(T16)%%V)
#IB17=c(y,cumsum(T17)%%V)
#IB18=c(y,cumsum(T18)%%V)
#IB19=c(y,cumsum(T19)%%V)
p=seq(from=0, to=V-1, by=1)
#print(ww3)
l=NULL;l1=NULL;l2=NULL;l3=NULL;l4=NULL;l5=NULL;l6=NULL;l7=NULL;l8=NULL;l9=NULL;l10=NULL;l11=NULL;l12=NULL;l13=NULL;l14=NULL;l15=NULL;l16=NULL;l17=NULL;l18=NULL;l19=NULL;l20=NULL;l21=NULL;l22=NULL

for(i in 1:k1){
  for(j in 1:V){
    l=c(l,rep((IB[i]+p[j]+V)%% V))
      l1=c(l1,rep((IB1[i]+p[j]+V)%% V))
      l2=c(l2,rep((IB2[i]+p[j]+V)%% V))
    #l3=c(l3,rep((IB3[i]+p[j]+V)%% V))
    #l4=c(l4,rep((IB4[i]+p[j]+V)%% V))
    #l5=c(l5,rep((IB5[i]+p[j]+V)%% V))
    #l6=c(l6,rep((IB6[i]+p[j]+V)%% V))
    #l7=c(l7,rep((IB7[i]+p[j]+V)%% V))
    #l8=c(l8,rep((IB8[i]+p[j]+V)%% V))
    #l9=c(l9,rep((IB9[i]+p[j]+V)%% V))
    #l10=c(l10,rep((IB10[i]+p[j]+V)%% V))
    #l11=c(l11,rep((IB11[i]+p[j]+V)%% V))
    #l12=c(l12,rep((IB12[i]+p[j]+V)%% V))
    #l13=c(l13,rep((IB13[i]+p[j]+V)%% V))
    #l14=c(l14,rep((IB14[i]+p[j]+V)%% V))
    #l15=c(l15,rep((IB15[i]+p[j]+V)%% V))
    #l16=c(l16,rep((IB16[i]+p[j]+V)%% V))
    #l17=c(l17,rep((IB17[i]+p[j]+V)%% V))
    #l18=c(l18,rep((IB18[i]+p[j]+V)%% V))
    #l19=c(l19,rep((IB19[i]+p[j]+V)%% V))
    #l20=c(l20,rep((IB20[i]+p[j]+V)%% V))
    
    
    
    
  }}
for(i in 1:k2){
  for(j in 1:V){
    #l=c(l,rep((IB[i]+p[j]+V)%% V))
      #l1=c(l1,rep((IB1[i]+p[j]+V)%% V))
 #l2=c(l2,rep((IB2[i]+p[j]+V)%% V))
   l3=c(l3,rep((IB3[i]+p[j]+V)%% V))
  #l4=c(l4,rep((IB4[i]+p[j]+V)%% V))
    #l5=c(l5,rep((IB5[i]+p[j]+V)%% V))
    #l6=c(l6,rep((IB6[i]+p[j]+V)%% V))
    #l7=c(l7,rep((IB7[i]+p[j]+V)%% V))
    #l8=c(l8,rep((IB8[i]+p[j]+V)%% V))
    #l9=c(l9,rep((IB9[i]+p[j]+V)%% V))
    #l10=c(l10,rep((IB10[i]+p[j]+V)%% V))
    #l11=c(l11,rep((IB11[i]+p[j]+V)%% V))
    #l12=c(l12,rep((IB12[i]+p[j]+V)%% V))
    #l13=c(l13,rep((IB13[i]+p[j]+V)%% V))
    #l14=c(l14,rep((IB14[i]+p[j]+V)%% V))
    #l15=c(l15,rep((IB15[i]+p[j]+V)%% V))
    #l16=c(l16,rep((IB16[i]+p[j]+V)%% V))
    #l17=c(l17,rep((IB17[i]+p[j]+V)%% V))
    #l18=c(l18,rep((IB18[i]+p[j]+V)%% V))
    #l19=c(l19,rep((IB19[i]+p[j]+V)%% V))
    #l20=c(l20,rep((IB20[i]+p[j]+V)%% V))
    
  }}
for(i in 1:k3){
  for(j in 1:V){
    #l=c(l,rep((IB[i]+p[j]+V)%% V))
    #l1=c(l1,rep((IB1[i]+p[j]+V)%% V))
      #l2=c(l2,rep((IB2[i]+p[j]+V)%% V))
    #l3=c(l3,rep((IB3[i]+p[j]+V)%% V))
      l4=c(l4,rep((IB4[i]+p[j]+V)%% V))
   #l5=c(l5,rep((IB5[i]+p[j]+V)%% V))
    #l6=c(l6,rep((IB6[i]+p[j]+V)%% V))
    #l7=c(l7,rep((IB7[i]+p[j]+V)%% V))
    #l8=c(l8,rep((IB8[i]+p[j]+V)%% V))
    #l9=c(l9,rep((IB9[i]+p[j]+V)%% V))
    #l10=c(l10,rep((IB10[i]+p[j]+V)%% V))
    #l11=c(l11,rep((IB11[i]+p[j]+V)%% V))
    #l12=c(l12,rep((IB12[i]+p[j]+V)%% V))
    #l13=c(l13,rep((IB13[i]+p[j]+V)%% V))
    #l14=c(l14,rep((IB14[i]+p[j]+V)%% V))
    #l15=c(l15,rep((IB15[i]+p[j]+V)%% V))
    #l16=c(l16,rep((IB16[i]+p[j]+V)%% V))
    #l17=c(l17,rep((IB17[i]+p[j]+V)%% V))
    #l18=c(l18,rep((IB18[i]+p[j]+V)%% V))
    #l19=c(l19,rep((IB19[i]+p[j]+V)%% V))
    #l20=c(l20,rep((IB20[i]+p[j]+V)%% V))
  }}

g= matrix(c(l),nrow=k1,ncol=V,byrow =  TRUE )
g1= matrix(c(l1),nrow=k1,ncol=V,byrow =  TRUE)
g2= matrix(c(l2),nrow=k1,ncol=V,byrow =  TRUE)
g3= matrix(c(l3),nrow=k2,ncol=V,byrow =  TRUE)
g4= matrix(c(l4),nrow=k3,ncol=V,byrow =  TRUE)
#g5= matrix(c(l5),nrow=k3,ncol=V,byrow =  TRUE)
#g6= matrix(c(l6),nrow=k3,ncol=V,byrow =  TRUE)
#g7= matrix(c(l7),nrow=k3,ncol=V,byrow =  TRUE)
#g8= matrix(c(l8),nrow=k2,ncol=V,byrow =  TRUE)
#g9= matrix(c(l9),nrow=k3,ncol=V,byrow =  TRUE)
#g10= matrix(c(l10),nrow=k3,ncol=V,byrow =  TRUE)
#g11= matrix(c(l11),nrow=k3,ncol=V,byrow =  TRUE)
#g12= matrix(c(l12),nrow=k3,ncol=V,byrow =  TRUE)
#g13= matrix(c(l13),nrow=k3,ncol=V,byrow =  TRUE)
#g14= matrix(c(l14),nrow=k3,ncol=V,byrow =  TRUE)
#g15= matrix(c(l15),nrow=k3,ncol=V,byrow =  TRUE)
#g16= matrix(c(l16),nrow=k3,ncol=V,byrow =  TRUE)
#g17= matrix(c(l17),nrow=k3,ncol=V,byrow =  TRUE)
G1=cbind(g,g1,g2)
G2=cbind(g3)
G3=cbind(g4)
gg1=cbind(g4)
G3=replace(gg1,which(is.na(gg1)),V)
#G=as.data.frame(GG)
#G[is.na(GGG)]<-7
#G=replace(GG,which(is.na(GG)),V)
print (G1)
print (G2)
print(G3)


P1=kronecker(matrix(1,nrow=(v-1)*NB1),diag(nrow = k1))
print(P1)
P2=kronecker(matrix(1,nrow=(v-1)*NB2),diag(nrow = k2))
print(P2)
P3=kronecker(matrix(1,nrow=(v-1)*NB3),diag(nrow = k3))
print(P3)
U1=kronecker(diag(nrow = (v-1)*NB1),matrix(1,nrow=k1))
print(U1)
U2=kronecker(diag(nrow = (v-1)*NB2),matrix(1,nrow=k2))
print(U2)
U3=kronecker(diag(nrow = (v-1)*NB3),matrix(1,nrow=k3))
print(U3)
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
wrapind3 <- function(i,n3)
  ifelse((r <- i %% n3) == 0, n3, r)
n3 <- nrow(G3)
incmat3 <- matrix(0,ncol=v,nrow=prod(dim(G3)),
                  dimnames=list(NULL,0:(v-1)))
m3 <- 1
for (i in seq(ncol(G3)))
  for (j in seq(nrow(G3))) {
    if(j==1){
      tt <- table(as.character(G3[wrapind2(j-1,n3),i]))
    }
    # if(j>1&j<k){
    #  tt <- table(as.character(G[wrapind(c(j-1,j+1),n),i]))
    #}
    else{
      tt <- table(as.character(G3[wrapind2(j-1,n3),i]))
    }
    
    incmat3[m3,names(tt)] <- tt
    m3 <- m3+1
  }
wrap3 <- function(i,n3)
  ifelse((R <- i %% n3) == 0, n3, R)
n3 <- nrow(G3)
incma3 <- matrix(0,ncol=v,nrow=prod(dim(G3)),
                 dimnames=list(NULL,0:(v-1)))
u3 <- 1
for (i in seq(ncol(G3)))
  for (j in seq(nrow(G3))) {
    ttt<- table(as.character(G3[wrap3((j),n3),i]))
    incma3[u3,names(ttt)] <- ttt
    u3 <- u3+1
  }
incma=rbind(incma1,incma2,incma3)
incmat=rbind(incmat1,incmat2,incmat3)
M1=t(incmat1)%*%U1
M2=t(incmat2)%*%U2
M3=t(incmat3)%*%U3
N1=t(incma1)%*%U1
print(n1)
N2=t(incma2)%*%U2
print(n2)
N3=t(incma3)%*%U3
CM=(N1%*%t(N1))+(N2%*%t(N2))+(N3%*%t(N3))-2*diag(v)
M1M2=M1%*%t(M1)+(M2%*%t(M2))+(M3%*%t(M3))-2*diag(v)
NN=(1/k1*(N1%*%t(N1)))+(1/k2*(N2%*%t(N2))+(1/k3*(N3%*%t(N3))))
L1=t(incma1)%*%incmat1
L2=t(incma2)%*%incmat2
L3=t(incma3)%*%incmat3
MM=(1/k1*(M1%*%t(M1)))+(1/k2*(M2%*%t(M2))+(1/k3*(M3%*%t(M3))))
nm=(1/k1*(N1%*%t(M1)))+(1/k2*(N2%*%t(M2))+(1/k3*(N3%*%t(M3))))
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
theta=((NB1*k1)+(NB2*k2)+(NB3*k3))*diag(v)-NN
er=eigen(theta)
ER=er$values
EER=ER[!ER%in%ER[v]]
EV=(v-1)/sum(1/EER)
Edirect=EV/CM[1,1]


si=((NB1*k1)+(NB2*k2)+(NB3*k3))*diag(v)-MM
ESI=eigen(si)
EESI=ESI$values
ESR=EESI[!EESI%in%EESI[v]]
ERR=(v-1)/sum(1/ESR)
Ecarryover=ERR/M1M2[1,1]
k= 9

V= 15
v=V+1
y= 0
     #No. of set of shifts for p1=k1 
NB=2   ##No. of set of shifts for p2=k2

T=c(10,2,3,4,5,6,7,14) #First set of shifts
T1=c(0,1,11,12,13,8,8) # Second and so on . . . .
#T2=c(23,16,17,18,19,20)
#T3=c(0,15,22,13,13)
#T4=c(6,22,21,25)
#T5=c(27,28,29,31)
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
IB=c(y,cumsum(T)%%V)
IB1=c(y,cumsum(T1)%%V)
#IB2=c(y,cumsum(T2)%%v)
#IB3=c(y,cumsum(T3)%%v)
#IB4=c(y,cumsum(T4)%%v)
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

for(i in 1:k){
  for(j in 1:V){
    l=c(l,rep((IB[i]+p[j]+V)%% V))
    l1=c(l1,rep((IB1[i]+p[j]+V)%% V))
    #l2=c(l2,rep((IB2[i]+p[j]+V)%% V))
    #l3=c(l3,rep((IB3[i]+p[j]+V)%% V))
    #l4=c(l4,rep((IB4[i]+p[j]+V)%% V))
    #l5=c(l5,rep((IB5[i]+p[j]+V)%% V))
    
  }}

g= matrix(c(l),nrow=k,ncol=V,byrow =  TRUE )
g1= matrix(c(l1),nrow=k,ncol=V,byrow =  TRUE)
#g2= matrix(c(l2),nrow=k,ncol=V,byrow =  TRUE)
#g3= matrix(c(l3),nrow=k,ncol=V,byrow =  TRUE)
#g4= matrix(c(l4),nrow=k1,ncol=V,byrow =  TRUE)
#g5= matrix(c(l5),nrow=k1,ncol=V,byrow =  TRUE)
#g6= matrix(c(l6),nrow=k2,ncol=V,byrow =  TRUE)
G=cbind(g,g1)
G1 =cbind (g,g1)

gg1g2=cbind(G1)
G1=replace(gg1g2,which(is.na(gg1g2)),V)
#G=as.data.frame(GG)
#G[is.na(GGG)]<-7
#G=replace(GG,which(is.na(GG)),V-1)
print (G)
print (G1)
GG =cbind (G,G1)



P=kronecker(matrix(1,nrow=(v-1)*NB),diag(nrow = k))
print(P)

U=kronecker(diag(nrow = (v-1)*NB),matrix(1,nrow=k))
print(U)

wrapind <- function(i,n)
  ifelse((r <- i %% n) == 0, n, r)
n <- nrow(GG)
incmat <- matrix(0,ncol=v,nrow=prod(dim(GG)),
                  dimnames=list(NULL,0:(v-1)))
m <- 1
for (i in seq(ncol(GG)))
  for (j in seq(nrow(GG))) {
    if(j==1){
      tt <- table(as.character(GG[wrapind(j-1,n),i]))
    }
    # if(j>1&j<k){
    #  tt <- table(as.character(G[wrapind(c(j-1,j+1),n),i]))
    #}
    else{
      tt <- table(as.character(GG[wrapind(j-1,n),i]))
    }
    
    incmat[m,names(tt)] <- tt
    m <- m+1
  }
#for (i in seq(ncol(G)))
# for (j in seq(nrow(G))) {
#  tt <- table(as.character(G[wrapind(c(j-1),n),i]))
# incmat[m,names(tt)] <- tt
#  m <- m+1
#}
wrap <- function(i,n)
  ifelse((R <- i %% n) == 0, n, R)
n <- nrow(GG)
incma <- matrix(0,ncol=v,nrow=prod(dim(GG)),
                 dimnames=list(NULL,0:(v-1)))
u <- 1
for (i in seq(ncol(GG)))
  for (j in seq(nrow(GG))) {
    ttt<- table(as.character(GG[wrap((j),n),i]))
    incma[u,names(ttt)] <- ttt
    u <- u+1
  }


L=t(incma)%*%incmat


L=t(incma)%*%incmat
NO=matrix(c(1),nrow=v,ncol=1)
CN=cbind(NO,L)
CHS=chisq.test(L)
print(CHS)
SUME= sum(CN)
value=CHS$statistic
VC=sqrt(((value)/SUME)/(min(v-1,v)))
ES=(1-VC)
print(ES)

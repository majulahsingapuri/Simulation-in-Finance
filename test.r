AAA<-read.csv("AAA.csv",header=TRUE) # Read data
dt=1/252
AAAprices<-as.matrix(AAA)
AAAprices
AAAprices<-cbind(as.numeric(as.vector(AAAprices[,2])),as.numeric(as.vector(AAAprices[,3])),as.numeric(as.vector(AAAprices[,4])))
n0=nrow(AAAprices)
AAAlogprices<-log(AAAprices)
AAAlogreturns<-AAAlogprices[2:n0,]-AAAlogprices[1:(n0-1),]
v=apply(AAAlogreturns,2,mean)/dt
Sigma=cov(AAAlogreturns)/dt
Sigma

library(MASS)
SimMultiGBMexact<-function(S0,v,Sigma,Deltat,T){ #simulate three assets' paths (once)
  m=T/Deltat # number of periods
  p=length(S0)
  S=matrix(0,nrow=p,ncol=m+1)
  S[,1]=S0
  Z<-mvrnorm(m,v*Deltat,Sigma*Deltat)
  for(j in 2:(m+1)){
    S[,j]=exp(log(S[,j-1])+Z[j-1,])
  }
}
Deltat=1/252
mvrnorm(m,v*Deltat,Sigma*Deltat)

Z<-mvrnorm(m,v/252,Sigma/252)
Z[1, ]

Nsim=10;T=1;dt=1/252
m=T/dt
S0=AAAprices[n0,]
S0
S1=matrix(0,Nsim,m+1)
S2=matrix(0,Nsim,m+1)
S3=matrix(0,Nsim,m+1)
set.seed(4518)
for(i in 1:Nsim){
  S<-SimMultiGBMexact(S0,v,Sigma,dt,T)
  S1[i,]=S[1,]
  S2[i,]=S[2,]
  S3[i,]=S[3,]
}
Sigma

Visualize<-function(S){
  # endindex=ncol(S)
  minS=min(S);maxS=max(S) # the y-limits of the plot
  noS<-nrow(S)
  cl<-rainbow(noS) # vector of rainbow colors
  plot(S[1,],type="l",ylim=c(minS,maxS),col=cl[1])
  if(noS>1){
    for(i in 2:noS){
      lines(S[i,],col=cl[i])
    }
  }
}

HistS1<-matrix(rep(AAAprices[,1],Nsim),ncol=n0,byrow=T)
wholeS1<-cbind(HistS1,S1)
Visualize(wholeS1)
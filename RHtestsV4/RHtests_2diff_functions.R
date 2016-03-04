
#####Function LSmultiple
###RHtestsV4
LSmultiple.RHtestsV4<-function(Y,T,Ips){
  Nx<-length(Y)
  Ns<-length(Ips)-1
  X<-t(t(Y))
  D<-rep(1,Nx)
##Diff
  D<-cbind(D,T-mean(T))
  if(Ns>=1){
    for(i in 1:Ns){
      tmp<-rep(0,Nx)
      tmp[(Ips[i]+1):Ips[i+1]]<-1
      D<-cbind(D,tmp)
    }
  }
  sig<-solve(t(D)%*%D)%*%t(D)%*%X
  fitted<-D%*%sig
  resi<-X-fitted
  SSE<-sum(resi^2)
  oout<-list()
  oout$SSE<-SSE
  oout$fitted<-as.vector(fitted)
  oout$resi<-as.vector(resi)
  oout$sig<-as.vector(sig)
  return(oout)
}


##########################################
###RHtests_dlyPrcp
LSmultiple.RHtests_dlyPrcp<-function(Y,T,Ips){
  Nx<-length(Y)
  Ns<-length(Ips)-1
  X<-t(t(Y))
  D<-rep(1,Nx)
##Diff
  D<-cbind(D,T)
  if(Ns>=1){
    for(i in 1:Ns){
      tmp<-rep(0,Nx)
      tmp[(Ips[i]+1):Ips[i+1]]<-1
      D<-cbind(D,tmp)
    }
  }
  sig<-solve(t(D)%*%D)%*%t(D)%*%X
  fitted<-D%*%sig
  resi<-X-fitted
  SSE<-sum(resi^2)
  oout<-list()
  oout$SSE<-SSE
  oout$fitted<-as.vector(fitted)
  oout$resi<-as.vector(resi)
  oout$sig<-as.vector(sig)
  return(oout)
}


######################################################################################################################################################

##Function LSmultipleRed
###RHtestsV4
LSmultipleRed.RHtestsV4<-function(Y0,Ti,Ips){
  Ns<-length(Ips)-1
  N<-length(Y0)
  otmp<-LSmultiple(Y0,Ti,Ips)
  sig<-otmp$sig
  beta<-otmp$sig[2]
  resi<-otmp$resi
  otmp<-autocorlh(resi,IY0flg)
  cor<-otmp$cor
  corl<-otmp$corl
  corh<-otmp$corh
  resi<-resi+beta*Ti
  W1<-resi/(1-cor)
  W2<-c(W1[1],(resi[2:N]-cor*resi[1:(N-1)])/(1-cor))
  W<-c(1,IY0flg[1:(N-1)])*W2+(!c(1,IY0flg[1:(N-1)]))*W1
  otmp<-LSmatrix(W,Ti,NA)
  beta<-otmp$sig[2]
  St0<-sum((Ti-mean(Ti))^2)
##Diff
  df<-(N-2-Ns-Nt)
  sigmaE2<-otmp$SSE/df
  t.stat<-abs(beta)/sqrt(sigmaE2/St0)
  p.tr<-pt(t.stat,df)
  betaL<-beta-qt(.975,df)*sqrt(sigmaE2/St0)	##RHtests only
  betaU<-beta+qt(.975,df)*sqrt(sigmaE2/St0)	##RHtests only
  itmp<-Y0-beta*Ti
  mu<-rep(0,Ns+1)
  meanhat<-mu
  for(i in 0:Ns){
    I0<- if(i==0) 1 else Ips[i]+1
    I2<-Ips[i+1]
    mu[i+1]<-mean(itmp[I0:I2])
    meanhat[I0:I2]<-mu[i+1]+beta*Ti[I0:I2]
    resi[I0:I2]<-Y0[I0:I2]-meanhat[I0:I2]
  }
  W1<-resi
  W2<-c(resi[1],resi[2:N]-cor*resi[1:(N-1)])
  W3<-c(resi[1],resi[2:N]-corl*resi[1:(N-1)])
  W4<-c(resi[1],resi[2:N]-corh*resi[1:(N-1)])
  W<-c(1,IY0flg[1:(N-1)])*W2+(!c(1,IY0flg[1:(N-1)]))*W1
  WL<-c(1,IY0flg[1:(N-1)])*W3+(!c(1,IY0flg[1:(N-1)]))*W1
  WU<-c(1,IY0flg[1:(N-1)])*W4+(!c(1,IY0flg[1:(N-1)]))*W1
  for(i in 0:Ns){
    I0<- if(i==0) 1 else Ips[i]+1
    I2<-Ips[i+1]
    W[I0:I2]<-W[I0:I2]+mean(itmp[I0:I2])+beta*Ti[I0:I2]
    WL[I0:I2]<-WL[I0:I2]+mean(itmp[I0:I2])+beta*Ti[I0:I2]
    WU[I0:I2]<-WU[I0:I2]+mean(itmp[I0:I2])+beta*Ti[I0:I2]
  }
  oout<-list()
  oout$W<-W
  oout$WL<-WL
  oout$WU<-WU
  oout$sig<-sig
  oout$cor<-cor
  oout$corl<-corl
  oout$corh<-corh
  oout$resi<-resi
  oout$mu<-mu
  oout$meanhat<-meanhat
  oout$trend<-beta
  oout$betaL<-betaL	##RHtests only
  oout$betaU<-betaU	##RHtests only
  oout$p.tr<-p.tr
  return(oout)
}



##########################################
###RHtests_dlyPrcp
LSmultipleRed.RHtests_dlyPrcp<-function(Y0,Ti,Ips){
  Ns<-length(Ips)-1
  N<-length(Y0)
  otmp<-LSmultiple(Y0,Ti,Ips)
  sig<-otmp$sig
  beta<-otmp$sig[2]
  resi<-otmp$resi
  otmp<-autocorlh(resi,IY0flg)
  cor<-otmp$cor
  corl<-otmp$corl
  corh<-otmp$corh
  resi<-resi+beta*Ti
  W1<-resi/(1-cor)
  W2<-c(W1[1],(resi[2:N]-cor*resi[1:(N-1)])/(1-cor))
  W<-c(1,IY0flg[1:(N-1)])*W2+(!c(1,IY0flg[1:(N-1)]))*W1
  otmp<-LSmatrix(W,Ti,NA)
  beta<-otmp$sig[2]
  St0<-sum((Ti-mean(Ti))^2)
##Diff
  df<-N-2
  sigmaE2<-otmp$SSE/df
  t.stat<-abs(beta)/sqrt(sigmaE2/St0)
  p.tr<-pt(t.stat,df)
  itmp<-Y0-beta*Ti
  mu<-rep(0,Ns+1)
  meanhat<-mu
  for(i in 0:Ns){
    I0<- if(i==0) 1 else Ips[i]+1
    I2<-Ips[i+1]
    mu[i+1]<-mean(itmp[I0:I2])
    meanhat[I0:I2]<-mu[i+1]+beta*Ti[I0:I2]
    resi[I0:I2]<-Y0[I0:I2]-meanhat[I0:I2]
  }
  W1<-resi
  W2<-c(resi[1],resi[2:N]-cor*resi[1:(N-1)])
  W3<-c(resi[1],resi[2:N]-corl*resi[1:(N-1)])
  W4<-c(resi[1],resi[2:N]-corh*resi[1:(N-1)])
  W<-c(1,IY0flg[1:(N-1)])*W2+(!c(1,IY0flg[1:(N-1)]))*W1
  WL<-c(1,IY0flg[1:(N-1)])*W3+(!c(1,IY0flg[1:(N-1)]))*W1
  WU<-c(1,IY0flg[1:(N-1)])*W4+(!c(1,IY0flg[1:(N-1)]))*W1
  for(i in 0:Ns){
    I0<- if(i==0) 1 else Ips[i]+1
    I2<-Ips[i+1]
    W[I0:I2]<-W[I0:I2]+mean(itmp[I0:I2])+beta*Ti[I0:I2]
    WL[I0:I2]<-WL[I0:I2]+mean(itmp[I0:I2])+beta*Ti[I0:I2]
    WU[I0:I2]<-WU[I0:I2]+mean(itmp[I0:I2])+beta*Ti[I0:I2]
  }
  oout<-list()
  oout$W<-W
  oout$WL<-WL
  oout$WU<-WU
  oout$sig<-sig
  oout$cor<-cor
  oout$corl<-corl
  oout$corh<-corh
  oout$resi<-resi
  oout$mu<-mu
  oout$meanhat<-meanhat
  oout$trend<-beta
  oout$p.tr<-p.tr
  return(oout)
}


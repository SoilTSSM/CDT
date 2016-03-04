#**** Copyright agreement ****
#       All users of any code in this package agree to the terms and conditions described in the
#file Copyright_RHtests__RClimDex_SoftwarePackages.pdf, which is also included in this package.
#*****************************
# last updated on 2013-06-28
# Changed all as.real() to as.numeric(), since R 2.15 no longer support as.real()

# last updated on 2013-04-03
# Changed Mq upper limit in StartGUI(), QMadjGaussian(), QMadjGaussian.wRef(), from 20 to 100,
# changed button locations on main manu.

# last updated on 2013-04-03
# Changed plots for QMadjust distribution

# last updated on 2013-03-19
# Added function QMadjGaussian.wRef(), changed FindU.wRef(), FindUD.wRef(),
#   StepSize.wRef(), replaced QMadjGaussian() with QMadjGaussian.wRef().
#   Modified Read.wRef(), append ref series to bdata matrix.
# Added a button 'QMadj.wRef' at StartGUI(), corresponding subroutine: QMadj.GaussianDLY.wRef

# last updated on 2012-09-25
# fixed a bug at StepSize() and StepSize.wRef() regarding to fitted value for Feb. 29th

# last updated on 2012-04-26
# at FindU() FindUD() StepSize() FindU.wRef() FindUD.wRef() and
# StepSize.wRef(), add fitted value for Feb. 29th to fitted
# value data files.

# last updated on 2011-12-11
# Change output format for *Ustat.txt in *.wRef(), from "PMF" -> "PMT"
# Change QMadj.GuassianDLY() and ReadDLY.g(), add vector olflg which indicate
#   non-missing data only, but contains Feb 29th data.

# last updated on 2010-06-08
#   In FindU(), FindUD(), StepSize() and 3 corresponding .wRef() functions,
#   added a text indication "Yes ", "YifD", "No  " or "?   " in changepoint list
#   output file, also changed corresponding read-in part.

# last updated on 2010-10-29
# add a sample format for 1Cs when there has no changepoint
# last updated on 2010-05-06
# in QMadjGaussian(), add Nadj option, set empirical prob. segment as length
# Nadj instead of whole segment.
#
# Bug fixed at 2010-02-08
# In FindU.wRef(), FindUD.wRef(), if 0 changepoint found, set meanhatD with
#   fitted value, but not 0. Output *_U.dat or *_UD.dat affected.

# NT=1, 12, and 365 for annual, monthly, and daily series, respectively
# p.lev is the nominal level of confidence; (1 - p.lev) is the nominal level
#     of significance
# choose one of the following 6 p.lev values: 0.75, 0.80, 0.90, 0.95, 0.99,
#     0.9999
# Mq (=0, 1, 2, ..., 20) is the number of points (categories) on which
#     the empirical cumulative distribution function are estimated.
#     If Mq=0, the actual Mq is determined by the length of the shortest segment
#     Mq=1 corresponds to mean adjustments (one adjustment for all data in
#     the same segment)
# In the output: Ns >= 0 is the number of changepoints identified;
#     changepoint positions are Ip(1), Ip(2),...,Ip(Ns)
# If Iadj = 0, the data series is adjusted to the longest segment;
#     otherwise the data series is adjusted to the chosen segment Iadj (if the
#     given integer Iadj > Ns+1, we re-set Iadj=Ns+1, which corresponds to
#     adjusting the series to the last segment). Set Iadj = 10000 if you want
#     to ensure that the series is adjusted to the last segment

######################################################################################################################################################

LSmultiRedCycle<-function(Y0,Ti,Ips,Iseg.adj){
  N<-length(Y0)
  Ns<-length(Ips)-1
  Niter<-0
  tt<-TRUE
  EB1<-EB
  while(tt){
    tt<-FALSE
    Niter<-Niter+1
    EB0<-EB1
    otmp<-LSmultipleRed(Y0,Ti,Ips)
    trend<-otmp$trend
    betaL<-otmp$betaL
    betaU<-otmp$betaU
    resi<-otmp$resi
    cor<-otmp$cor
    corl<-otmp$corl
    corh<-otmp$corh
    p.tr<-otmp$p.tr
    meanhat<-otmp$meanhat
    mu<-otmp$mu
    W<-otmp$W
    WL<-otmp$WL
    WU<-otmp$WU

    if(Nt>1){
      itmp1<-cbind(EB0,Icy)
      itmp2<-cbind(1:N,Imd)
      colnames(itmp2)<-c("idx","Icy")
      itmp<-merge(itmp1,itmp2,by="Icy")
      EBfull<-itmp[order(itmp[,"idx"]),"EB0"]

      for(i in 1:(Ns+1)){
        I0<- if(i==1) 1 else Ips[i-1]+1
        I2<-Ips[i]
#       delta<-if(i==(Ns+1)) 0 else mu[i]-mu[Iseg.adj]
        delta<-mu[i]-mu[Iseg.adj]
        Y0[I0:I2]<-Y0[I0:I2]+EBfull[I0:I2]-delta
      }

      for(i in 1:Nt) EB1[i]<-mean(Y0[Imd==Icy[i]])
      VEB<-sqrt(var(EB1))
      if(is.na(VEB)) tt<-FALSE
      else{
        itmp1<-cbind(EB1,Icy)
        itmp2<-cbind(1:N,Imd)
        colnames(itmp2)<-c("idx","Icy")
        itmp<-merge(itmp1,itmp2,by="Icy")
        EBfull<-itmp[order(itmp[,"idx"]),"EB1"]

        for(i in 1:(Ns+1)){
          I0<- if(i==1) 1 else Ips[i-1]+1
          I2<-Ips[i]
#         delta<-if(i==(Ns+1)) 0 else mu[i]-mu[Iseg.adj]
          delta<-mu[i]-mu[Iseg.adj]
          Y0[I0:I2]<-Y0[I0:I2]-EBfull[I0:I2]+delta
        }

        DEBmx<-max(abs(EB1-EB0))
        if(DEBmx>VEB/1000&Niter<20) tt<-TRUE
      }
    }
  }
  oout<-list()
  oout$trend<-trend
  oout$betaL<-betaL
  oout$betaU<-betaU
  oout$EB<-EB1
  oout$mu<-mu
  oout$cor<-cor
  oout$corl<-corl
  oout$corh<-corh
  oout$W<-W
  oout$WL<-WL
  oout$WU<-WU
  oout$resi<-resi
  oout$Y0<-as.vector(Y0)
  oout$meanhat<-as.vector(meanhat)
  oout$p.tr<-p.tr
  return(oout)
}

######################################################################################################################################################


PTKI0I2<-function(Y0,I0,I2){
# search new breakpoint of Y0[(I0+1):I2] using function PTK()
# output: Ic -- breakpoint, prob and PTx
  Y<-Y0[(I0+1):I2]
  N<-length(Y)
  oout<-list()
  oout$prob<-(-1)
  oout$Ic<-I0
  oout$PTx<-(-9999.9)
  if(N>=(Nmin*2)){
    Pk0<-Pk.PMT(N)
    otmp<-PTK(Y,Pk0)
    oout$Ic<-I0+otmp$KPx
    oout$prob<-pt(otmp$Tx,(N-2))
    oout$PTx<-otmp$PTx
  }
  return(oout)
}

PTK<-function(Y,Pk){
#  search input vector, return PTx.max and corresponding KPx
  PTx<-(-9999.9)
  KPx<-0
  N<-length(Y)
  for(k in Nmin:(N-Nmin)){
    EY1<-mean(Y[1:k])
    EY2<-mean(Y[(k+1):N])
    var<-sum(c((Y[1:k]-EY1)^2,(Y[(k+1):N]-EY2)^2))
    std<-sqrt(var/(N-2))
    Tk<-sqrt(k*(N-k)/N)*abs(EY1-EY2)/std
    PTk<-Tk*Pk[k]
    if(PTk>PTx){
      PTx<-PTk
      KPx<-k
      Tx<-Tk
    }
  }
  oout<-list()
  oout$PTx<-PTx
  oout$KPx<-KPx
  oout$Tx<-Tx
  return(oout)
}

Pk.PMT<-function(N){
# calculate penalty with given series length -- N
# output P, real vector of length N-1
  if(floor(N)!=N) stop("input para is not integer")
  Nle10<- if(N<=10) TRUE else FALSE
  Nlt50<- if(N<50) TRUE else FALSE
  Nle100<- if(N<=100) TRUE else FALSE
  Ngt10lt50<- if(N>10&N<50) TRUE else FALSE

  K<-seq(1,(N-1))
  A<-abs(1-2*K/N)
  B<-log(N)
  C<-log(B)
  D<-log(log(N+150))

  F <- if(Nle100) 1-A^((7*B-2*B*C)/10) else 1-A^(11*B*C/50)
  v <- if(Nle100) (15*C^0.5-11)/100 else (2*C^2+2*C-1)/100
  Po<-(11*C^(9/8)+195)*F^v/200

  K1<-sum(Po[1:floor(N/2)]<1)+1
  L<- if(Ngt10lt50) floor(K1/2)+2 else floor(K1/2)+1

  if(N<=10) delta<-rep(D^0.5*(Po[L+1]-Po[L]),(N-1))
  else if(N<=100) delta<-rep((D^(1/3)*(Po[L+1]-Po[L])+3/(10*N^(4/3))),(N-1))
  else{
    delta<-rep(0,(N-1))
    delta[1:L]<-(Po[L]-Po[1])*A[1:L]^(C^3)/(2*L-4)
    delta[(N-L):(N-1)]<-(Po[N-L]-Po[N-1])*A[(N-L):(N-1)]^(C^3)/(2*L-4)
  }
  P<-Po
  P[1:L]<-P[L]-delta[1:L]*(L-c(1:L))
  P[(N-L):(N-1)]<-P[N-L]-delta[(N-L):(N-1)]*(c((N-L):(N-1))-N+L)
  return(P)
}

PTKIc<-function(Y,Pk,Ic){
# calculate PTk and prob for given data vector and breakpoint Ic
  N<-length(Y)
  oout<-list()
  if(Ic>0){
    EY1<-mean(Y[1:Ic])
    EY2<-mean(Y[(Ic+1):N])
    var<-sum(c((Y[1:Ic]-EY1)^2,(Y[(Ic+1):N]-EY2)^2))
    std<-sqrt(var/(N-2))
    Tk<-sqrt(Ic*(N-Ic)/N)*abs(EY1-EY2)/std
    PTk<-Tk*Pk[Ic]
    oout$PTk<-PTk
    oout$prob<-pt(Tk,(N-2))
  }
  else{
    oout$PTk<-0
    oout$prob<-0
  }
  return(oout)
}

getPTx95<-function(cor,N){
# if(cor<phi[1]|cor>phi[length(phi)]) stop("input series autocorrelation outbound!")
  if(cor<=phi[1])
    PTx95<-PTmax[N,1]
  else if(cor>=phi[length(phi)])
    PTx95<-PTmax[N,length(phi)]
  else{
    for(i in 1:(length(phi)-1))
      if(cor>=phi[i]&cor<phi[i+1]) {
        Kr1<-i
        Kr2<-i+1
        cor1<-phi[i]
        cor2<-phi[i+1]
      }
    tmp1<-PTmax[N,Kr1]
    tmp2<-PTmax[N,Kr2]
    PTx95<-tmp1+(tmp2-tmp1)*(cor-cor1)/(cor2-cor1)
  }
  return(PTx95)
}


######################################################################################################################################################


Read.file<-function(){
  ifname<-tclvalue(tkgetOpenFile())
  if(!nchar(ifname)){
    tkinsert(txt,"end","No file selected in Data Transform!\n")
    return()
  }
  outdirtmp<-strsplit(ifname,"/")[[1]]
  if(length(outdirtmp)<=2){
    curdir<-paste(outdirtmp[1])
    outdir<-paste(outdirtmp[1],"log",sep=":/")
  }
  else{
    curdir<-outdirtmp[1]
    for(i in 2:(length(outdirtmp)-1))
      curdir<-paste(curdir,outdirtmp[i],sep="/")
  }
# if(!file.exists(outdir)) dir.create(outdir)
  ofname<-outdirtmp[length(outdirtmp)]
  if(is.csv(ofname)) csv<-1
  else csv<-0
  if(csv){
    itmp<-try(read.table(ifname,header=F,
            col.names=c("year","month","day","prcp","tmax","tmin"),
            sep=",",na.strings=MissingStr,colClasses=rep("real",6)),            ##'-99.9' replace to MissingStr
#            sep=",",na.strings="-99.9",colClasses=rep("real",6)),
	    silent=T)
    if(inherits(itmp,"try-error")){
      ErrorMSG<<-geterrmessage()
      tkinsert(txt,"end",paste(ErrorMSG,"\n"))
      return()
    }
    else iidata<-itmp
  }
  else{
    itmp<-try(read.table(ifname,header=F,
            col.names=c("year","month","day","prcp","tmax","tmin"),
            na.strings=MissingStr,colClasses=rep("real",6)),            ##'-99.9' replace to MissingStr
#            na.strings="-99.9",colClasses=rep("real",6)),
	    silent=T)
    if(inherits(itmp,"try-error")){
      ErrorMSG<<-geterrmessage()
      tkinsert(txt,"end",paste(ErrorMSG,"\n"))
      return()
    }
    else iidata<-itmp
  }
  if(ncol(iidata)!=6){
    ErrorMSG<-paste(ifname,"has",ncol(iidata),"columns. The number of columns should be 6\n")
    tkinsert(txt,"end",paste(ErrorMSG,"\n"))
    return()
  }
  nlen<-dim(iidata)[1]
  syear<-iidata[1,1]
  eyear<-iidata[nlen,1]
  smonth<-iidata[1,2]
  emonth<-iidata[nlen,2]
  if(eyear<(syear+1)) {
    ErrorMSG<-paste("Time series",ifname,"too short for Transform Data\n")
    return(-1)
  }
  nyrs<-eyear-syear+1
  vars<-c("tmax","tmin","prcp")
  ivars<-length(vars)

  tkinsert(txt,"end",paste("Data from:",syear,"to:",eyear,sep=" "))
  tkinsert(txt,"end","\n")

  for(i in 1:ivars){
    mdata<-NULL
    if(vars[i]=="prcp") mdata1mm<-NULL
    tmpdata<-iidata[iidata[,"year"]==syear,]
    for(k in smonth:12){
      if(sum(is.na(tmpdata[tmpdata[,"month"]==k,vars[i]]))<=3)
        if(vars[i]=="prcp"){
	  itmp<-sum(tmpdata[tmpdata[,"month"]==k,vars[i]],na.rm=T)
	  itmp1mm<-sum(tmpdata[tmpdata[,"month"]==k&
	               tmpdata[,vars[i]]>=1,vars[i]],na.rm=T)
        }
	else
          itmp<-mean(tmpdata[tmpdata[,"month"]==k,vars[i]],na.rm=T)
      else{
        itmp<-NA
	itmp1mm<-NA
      }
      mdata<-rbind(mdata,c(syear,k,0,itmp))
      if(vars[i]=="prcp") mdata1mm<-rbind(mdata1mm,c(syear,k,0,itmp1mm))
    }
    for(j in (syear+1):(eyear-1)){
      year<-j
      tmpdata<-iidata[iidata[,"year"]==year,]
      for(k in 1:12){
        if(sum(is.na(tmpdata[tmpdata[,"month"]==k,vars[i]]))<=3)
	  if(vars[i]=="prcp"){
	    itmp<-sum(tmpdata[tmpdata[,"month"]==k,vars[i]],na.rm=T)
	    itmp1mm<-sum(tmpdata[tmpdata[,"month"]==k&
	                 tmpdata[,vars[i]]>=1,vars[i]],na.rm=T)
          }
	  else
	    itmp<-mean(tmpdata[tmpdata[,"month"]==k,vars[i]],na.rm=T)
        else{
	  itmp<-NA
	  itmp1mm<-NA
        }
        mdata<-rbind(mdata,c(year,k,0,itmp))
        if(vars[i]=="prcp") mdata1mm<-rbind(mdata1mm,c(year,k,0,itmp1mm))
      }
    }
    tmpdata<-iidata[iidata[,"year"]==eyear,]
    for(k in 1:emonth){
      if(sum(is.na(tmpdata[tmpdata[,"month"]==k,vars[i]]))<=3)
        if(vars[i]=="prcp"){
	  itmp<-sum(tmpdata[tmpdata[,"month"]==k,vars[i]],na.rm=T)
	  itmp1mm<-sum(tmpdata[tmpdata[,"month"]==k&
	               tmpdata[,vars[i]]>=1,vars[i]],na.rm=T)
        }
	else
          itmp<-mean(tmpdata[tmpdata[,"month"]==k,vars[i]],na.rm=T)
      else{
        itmp<-NA
	itmp1mm<-NA
      }
      mdata<-rbind(mdata,c(eyear,k,0,itmp))
      if(vars[i]=="prcp") mdata1mm<-rbind(mdata1mm,c(eyear,k,0,itmp1mm))
    }
    if(vars[i]=="prcp") {prcp<-mdata; prcp1mm<-mdata1mm}
    else if(vars[i]=="tmax") tmax<-mdata
    else tmin<-mdata
  }
  logprcp<-prcp
  if(min(prcp[,4],na.rm=T)>0) logprcp[,4]<-log(prcp[,4])
  else logprcp[,4]<-log(prcp[,4]+1)
  logprcp1mm<-prcp1mm
  if(min(prcp1mm[,4],na.rm=T)>0) logprcp1mm[,4]<-log(prcp1mm[,4])
  else logprcp1mm[,4]<-log(prcp1mm[,4]+1)
  otmp<-strsplit(ofname,"\\.")[[1]]
  if(length(otmp)>1){
    ind<-length(otmp)-1
    ofmax2<-paste(otmp[ind],"_tmaxMLY",sep="")
    ofmin2<-paste(otmp[ind],"_tminMLY",sep="")
    ofprcp2<-paste(otmp[ind],"_prcpMLY",sep="")
    ofprcpL2<-paste(otmp[ind],"_LogprcpMLY",sep="")
    ofprcp1mm2<-paste(otmp[ind],"_prcpMLY1mm",sep="")
    ofprcp1mmL2<-paste(otmp[ind],"_LogprcpMLY1mm",sep="")
    ofmaxD2<-paste(otmp[ind],"_tmaxDLY",sep="")
    ofminD2<-paste(otmp[ind],"_tminDLY",sep="")
    ofprcpD2<-paste(otmp[ind],"_prcpDLY",sep="")
    if(ind==1){
      ofmax<-ofmax2
      ofmin<-ofmin2
      ofprcp<-ofprcp2
      ofprcpL<-ofprcpL2
      ofprcp1mm<-ofprcp1mm2
      ofprcp1mmL<-ofprcp1mmL2
      ofmaxD<-ofmaxD2
      ofminD<-ofminD2
      ofprcpD<-ofprcpD2
    }
    else{
      ofmax<-otmp[1]
      ofmin<-otmp[1]
      ofprcp<-otmp[1]
      ofprcpL<-otmp[1]
      ofprcp1mm<-otmp[1]
      ofprcp1mmL<-otmp[1]
      ofmaxD<-otmp[1]
      ofminD<-otmp[1]
      ofprcpD<-otmp[1]
    }
    for(i in 2:length(otmp)){
      if(i==ind){
        ofmax<-paste(ofmax,ofmax2,sep=".")
        ofmin<-paste(ofmin,ofmin2,sep=".")
        ofprcp<-paste(ofprcp,ofprcp2,sep=".")
        ofprcpL<-paste(ofprcpL,ofprcpL2,sep=".")
        ofprcp1mm<-paste(ofprcp1mm,ofprcp1mm2,sep=".")
        ofprcp1mmL<-paste(ofprcp1mmL,ofprcp1mmL2,sep=".")
        ofmaxD<-paste(ofmaxD,ofmaxD2,sep=".")
        ofminD<-paste(ofminD,ofminD2,sep=".")
        ofprcpD<-paste(ofprcpD,ofprcpD2,sep=".")
      }
      else{
        ofmax<-paste(ofmax,otmp[i],sep=".")
        ofmin<-paste(ofmin,otmp[i],sep=".")
        ofprcp<-paste(ofprcp,otmp[i],sep=".")
        ofprcpL<-paste(ofprcpL,otmp[i],sep=".")
        ofprcp1mm<-paste(ofprcp1mm,otmp[i],sep=".")
        ofprcp1mmL<-paste(ofprcp1mmL,otmp[i],sep=".")
        ofmaxD<-paste(ofmaxD,otmp[i],sep=".")
        ofminD<-paste(ofminD,otmp[i],sep=".")
        ofprcpD<-paste(ofprcpD,otmp[i],sep=".")
      }
    }
  }
  else{
    ofmax<-paste(otmp,"_tmaxMLY",sep="")
    ofmin<-paste(otmp,"_tminMLY",sep="")
    ofprcp<-paste(otmp,"_prcpMLY",sep="")
    ofprcpL<-paste(otmp,"_LogprcpMLY",sep="")
    ofprcp1mm<-paste(otmp,"_prcpMLY1mm",sep="")
    ofprcp1mmL<-paste(otmp,"_LogprcpMLY1mm",sep="")
    ofmaxD<-paste(otmp,"_tmaxDLY",sep="")
    ofminD<-paste(otmp,"_tminDLY",sep="")
    ofprcpD<-paste(otmp,"_prcpDLY",sep="")
  }
  ofmax<-paste(curdir,ofmax,sep="/")
  ofmin<-paste(curdir,ofmin,sep="/")
  ofprcp<-paste(curdir,ofprcp,sep="/")
  ofprcpL<-paste(curdir,ofprcpL,sep="/")
  ofprcp1mm<-paste(curdir,ofprcp1mm,sep="/")
  ofprcp1mmL<-paste(curdir,ofprcp1mmL,sep="/")
  ofmaxD<-paste(curdir,ofmaxD,sep="/")
  ofminD<-paste(curdir,ofminD,sep="/")
  ofprcpD<-paste(curdir,ofprcpD,sep="/")
  rownames(tmax)<-NULL
  rownames(tmin)<-NULL
  rownames(prcp)<-NULL
  write.table(tmax,file=ofmax,sep=" ",na=MissingStr,col.names=F,row.names=F)
  write.table(tmin,file=ofmin,sep=" ",na=MissingStr,col.names=F,row.names=F)
  write.table(prcp,file=ofprcp,sep=" ",na=MissingStr,col.names=F,row.names=F)
  write.table(logprcp,file=ofprcpL,sep=" ",na=MissingStr,col.names=F,row.names=F)
  write.table(prcp1mm,file=ofprcp1mm,sep=" ",na=MissingStr,col.names=F,row.names=F)
  write.table(logprcp1mm,file=ofprcp1mmL,sep=" ",na=MissingStr,col.names=F,row.names=F)
  write.table(iidata[,c("year","month","day","prcp")],file=ofprcpD,
                    na=MissingStr,col.names=F,row.names=F)
  write.table(iidata[,c("year","month","day","tmax")],file=ofmaxD,
                    na=MissingStr,col.names=F,row.names=F)
  write.table(iidata[,c("year","month","day","tmin")],file=ofminD,
                    na=MissingStr,col.names=F,row.names=F)
  tkinsert(txt,"end","Data transform finished, monthly series output:\n")
  tkinsert(txt,"end",paste(ofmax,"\n"))
  tkinsert(txt,"end",paste(ofmin,"\n"))
  tkinsert(txt,"end",paste(ofprcp,"\n"))
  tkinsert(txt,"end",paste(ofprcpL,"\n"))
  tkinsert(txt,"end",paste(ofprcp1mm,"\n"))
  tkinsert(txt,"end",paste(ofprcp1mmL,"\n"))
  tkinsert(txt,"end","Daily series output:\n")
  tkinsert(txt,"end",paste(ofmaxD,"\n"))
  tkinsert(txt,"end",paste(ofminD,"\n"))
  tkinsert(txt,"end",paste(ofprcpD,"\n\n"))
  return(0)
}

######################################################################################################################################################


OnFindU<-function(){
  getfile1<-function(){
    if(!exists("ifname")) ifname<-tclvalue(tkgetOpenFile())
    else ifname<-tclvalue(tkgetOpenFile(initialfile=ifname))
    if(!nchar(ifname)){
      tkinsert(txt,"end","No file selected in FindUD!\n\n")
      return()
    }
    otmp<-str40("ifname",ifname)
    tkgrid(tklabel(tt,text=otmp,width=40),column=2,row=2,sticky="e")
    outdirtmp<-strsplit(ifname,"/")[[1]]
    if(length(outdirtmp)<=2){
      curdir<-paste(outdirtmp[1])
      outdir<-paste(outdirtmp[1],"output",sep=":/")
    }
    else{
      curdir<-outdirtmp[1]
      for(i in 2:(length(outdirtmp)-1))
        curdir<-paste(curdir,outdirtmp[i],sep="/")
        outdir<-paste(curdir,"output",sep="/")
    }
    setwd(curdir)
    if(!file.exists(outdir)) dir.create(outdir)
    ofname<-outdirtmp[length(outdirtmp)]
    itmp<-strsplit(ofname,"\\.")[[1]]
    if(length(itmp)<2) ofbody<-itmp[1]
    else{
      ofbody<-itmp[1]
      if(length(itmp)>2) for(i in 2:(length(itmp)-1))
      	ofbody<-paste(ofbody,itmp[i],sep="/")
    }
    ofname<-paste(outdir,ofbody,sep="/")
    assign("curdir",curdir,envir=.GlobalEnv)
    assign("outdir",outdir,envir=.GlobalEnv)
    assign("ifname",ifname,envir=.GlobalEnv)
    assign("ofbody",ofbody,envir=.GlobalEnv)
    assign("ofname",ofname,envir=.GlobalEnv)
  }
  tt<-tktoplevel()
  button.chg1<-tkbutton(tt,text="Change",command=getfile1)
  tkwm.title(tt,"FindU")
  oifname<-str40("ifname",ifname)
  fontLable<-tkfont.create(family="times",size=20,weight="bold")
  tkgrid(tklabel(tt,text="!!Do not",font=fontLable),sticky="e",column=1,row=1)
  tkgrid(tklabel(tt,text="choose daily precipitation data!!",
         font=fontLable),row=1,column=2)
  tkgrid(tklabel(tt,text="Input Data filename:"),column=1,row=2,sticky="w")
  tkgrid(tklabel(tt,text=oifname,width=40),column=2,row=2,sticky="e")
  tkgrid(button.chg1,column=3,row=2)

  OnOk2<-function(){
    oflg<-1
    if(!file.exists(ifname)) {
      oflg<-0
      GuiErrorMSG<-paste("Input Data file ",ifname," does not exist!\n",sep="")
    }
    if(oflg==0) {
      tkinsert(txt,"end",GuiErrorMSG)
      tkfocus(tt)
    }
    else{
      outdirtmp<-strsplit(ifname,"/")[[1]]
      if(length(outdirtmp)<=2){
        curdir<-paste(outdirtmp[1])
        outdir<-paste(outdirtmp[1],"output",sep=":/")
      }
      else{
        curdir<-outdirtmp[1]
        for(i in 2:(length(outdirtmp)-1))
          curdir<-paste(curdir,outdirtmp[i],sep="/")
          outdir<-paste(curdir,"output",sep="/")
      }
      setwd(curdir)
      if(!file.exists(outdir)) dir.create(outdir)
      ofname<-outdirtmp[length(outdirtmp)]
      itmp<-strsplit(ofname,"\\.")[[1]]
      if(length(itmp)<2) ofbody<-itmp[1]
      else{
        ofbody<-itmp[1]
        if(length(itmp)>2) for(i in 2:(length(itmp)-1))
      	  ofbody<-paste(ofbody,itmp[i],sep="/")
      }
      ofname<-paste(outdir,ofbody,sep="/")
      assign("curdir",curdir,envir=.GlobalEnv)
      assign("outdir",outdir,envir=.GlobalEnv)
      assign("ofbody",ofbody,envir=.GlobalEnv)
      assign("ofname",ofname,envir=.GlobalEnv)
      if(!as.numeric(PlevStr)%in%c(0.75,0.8,0.9,0.95,0.99,0.9999)){
        tkinsert(txt,"end","P_lev setting error, reset P_lev...")
        return()
      }
      itmp<-FindU(InSeries=ifname,output=ofname,
            MissingValueCode=MissingStr,p.lev=as.numeric(PlevStr),
	    Iadj=as.numeric(AdjStr),Mq=as.numeric(Mq0Str),Ny4a=as.numeric(Ny4aStr),
	    GUI=TRUE)
      if(itmp<0){
        tkinsert(txt,"end",ErrorMSG)
        return()
      }
      else{
        UIpsName1<-paste(ofname,"_1Cs.txt",sep="")
        UIpsName<-paste(ofname,"_mCs.txt",sep="")
        file.copy(from=UIpsName1,to=UIpsName,overwrite=TRUE)
        assign("iDIpsName",UIpsName,envir=.GlobalEnv)
	oact<-str40("ofbody",ofbody)
	ocurdir<-str40("curdir",curdir)
	ooutdir<-str40("outdir",outdir)
        if(exists("ofref")) rm("ofref",envir=.GlobalEnv)
        b20<-"                    "
        oref<-paste(b20,"                  NA",sep="")
        tkgrid(tklabel(frameMiddle,text=oact,width=40),column=2,row=6,sticky="e")
        tkgrid(tklabel(frameMiddle,text=oref,width=40),column=2,row=7,sticky="e")
        tkgrid(tklabel(frameMiddle,text=ocurdir,width=40),column=2,row=8,sticky="e")
        tkgrid(tklabel(frameMiddle,text=ooutdir,width=40),column=2,row=9,sticky="e")
        tkinsert(txt,"end","FindU finished successfully...\n")
        tkinsert(txt,"end",paste("Modify",UIpsName,"for further calculation...\n\n"))
      }
      tkdestroy(tt)
      tkfocus(main)
      return()
    }

  }
  Ok2.but<-tkbutton(tt,text="   OK   ",command=OnOk2)
  tkgrid(Ok2.but,column=3,row=3)
  tkfocus(tt)
}

######################################################################################################################################################


OnFindUD<-function(){
  getfile1<-function(){
    if(!exists("ifname")) ifname<-tclvalue(tkgetOpenFile())
    else ifname<-tclvalue(tkgetOpenFile(initialfile=ifname))
    if(!nchar(ifname)){
      tkinsert(txt,"end","No file selected in FindUD!\n\n")
      return()
    }
    otmp<-str40("ifname",ifname)
    tkgrid(tklabel(tt,text=otmp,width=40),column=2,row=2,sticky="e")
    outdirtmp<-strsplit(ifname,"/")[[1]]
    if(length(outdirtmp)<=2){
      curdir<-paste(outdirtmp[1])
      outdir<-paste(outdirtmp[1],"output",sep=":/")
    }
    else{
      curdir<-outdirtmp[1]
      for(i in 2:(length(outdirtmp)-1))
        curdir<-paste(curdir,outdirtmp[i],sep="/")
        outdir<-paste(curdir,"output",sep="/")
    }
    setwd(curdir)
    if(!file.exists(outdir)) dir.create(outdir)
    ofname<-outdirtmp[length(outdirtmp)]
    itmp<-strsplit(ofname,"\\.")[[1]]
    if(length(itmp)<2) ofbody<-itmp[1]
    else{
      ofbody<-itmp[1]
      if(length(itmp)>2) for(i in 2:(length(itmp)-1))
      	ofbody<-paste(ofbody,itmp[i],sep="/")
    }
    ofname<-paste(outdir,ofbody,sep="/")
    assign("curdir",curdir,envir=.GlobalEnv)
    assign("outdir",outdir,envir=.GlobalEnv)
    assign("ifname",ifname,envir=.GlobalEnv)
    assign("ofbody",ofbody,envir=.GlobalEnv)
    assign("ofname",ofname,envir=.GlobalEnv)
  }
  getfile2<-function(){
    if(!exists("iDIpsName")) iDIpsName<-tclvalue(tkgetOpenFile())
    else iDIpsName<-tclvalue(tkgetOpenFile(initialfile=iDIpsName))
    otmp<-str40("iDIpsName",iDIpsName)
    tkgrid(tklabel(tt,text=otmp,width=40),column=2,row=3,sticky="e")
    assign("iDIpsName",iDIpsName,env=.GlobalEnv)
  }
  tt<-tktoplevel()
  button.chg1<-tkbutton(tt,text="Change",command=getfile1)
  button.chg2<-tkbutton(tt,text="Change",command=getfile2)
  tkwm.title(tt,"FindUD")
  oifname<-str40("ifname",ifname)
  oIpsName<-str40("iDIpsName",iDIpsName)
  fontLable<-tkfont.create(family="times",size=20,weight="bold")
  tkgrid(tklabel(tt,text="!!Do not",font=fontLable),sticky="e",column=1,row=1)
  tkgrid(tklabel(tt,text="choose daily precipitation data!!",
         font=fontLable),row=1,column=2)
  tkgrid(tklabel(tt,text="Input Data filename:"),column=1,row=2,sticky="w")
  tkgrid(tklabel(tt,text=oifname,width=40),column=2,row=2,sticky="e")
  tkgrid(button.chg1,column=3,row=2)

  tkgrid(tklabel(tt,text="Input changepoints filename:"),column=1,row=3,sticky="w")
  tkgrid(tklabel(tt,text=oIpsName,width=40),column=2,row=3,sticky="e")
  tkgrid(button.chg2,column=3,row=3)

  OnOk2<-function(){
    oflg<-1
    if(!file.exists(ifname)) {
      oflg<-0
      GuiErrorMSG<-paste("Input Data file ",ifname," does not exist!\n",sep="")
    }
    if(!file.exists(iDIpsName)) {
      oflg<-0
      GuiErrorMSG<-paste(GuiErrorMSG,"Input changepoint file ",iDIpsName,
                   " does not exist!\n",sep="")
    }
    if(oflg==0) {
      tkinsert(txt,"end",GuiErrorMSG)
      tkfocus(tt)
    }
    else{
      if(!as.numeric(PlevStr)%in%c(0.75,0.8,0.9,0.95,0.99,0.9999)){
        tkinsert(txt,"end","P_lev setting error, reset P_lev...")
        return()
      }
      outdirtmp<-strsplit(ifname,"/")[[1]]
      if(length(outdirtmp)<=2){
        curdir<-paste(outdirtmp[1])
        outdir<-paste(outdirtmp[1],"output",sep=":/")
      }
      else{
        curdir<-outdirtmp[1]
        for(i in 2:(length(outdirtmp)-1))
          curdir<-paste(curdir,outdirtmp[i],sep="/")
          outdir<-paste(curdir,"output",sep="/")
      }
      setwd(curdir)
      if(!file.exists(outdir)) dir.create(outdir)
      ofname<-outdirtmp[length(outdirtmp)]
      itmp<-strsplit(ofname,"\\.")[[1]]
      if(length(itmp)<2) ofbody<-itmp[1]
      else{
        ofbody<-itmp[1]
        if(length(itmp)>2) for(i in 2:(length(itmp)-1))
      	  ofbody<-paste(ofbody,itmp[i],sep="/")
      }
      ofname<-paste(outdir,ofbody,sep="/")
      assign("curdir",curdir,envir=.GlobalEnv)
      assign("outdir",outdir,envir=.GlobalEnv)
      assign("ofbody",ofbody,envir=.GlobalEnv)
      assign("ofname",ofname,envir=.GlobalEnv)
      itmp<-FindUD(InSeries=ifname,output=ofname,InCs=iDIpsName,
            MissingValueCode=MissingStr,p.lev=as.numeric(PlevStr),
	    Iadj=as.numeric(AdjStr),Mq=as.numeric(Mq0Str),Ny4a=as.numeric(Ny4aStr),
	    GUI=TRUE)
      if(itmp<0){
        tkinsert(txt,"end",ErrorMSG)
        return()
      }
      else{
        UDIpsName1<-paste(ofname,"_pCs.txt",sep="")
        UDIpsName<-paste(ofname,"_mCs.txt",sep="")
        file.copy(from=UDIpsName1,to=UDIpsName,overwrite=TRUE)
        assign("iDIpsName",UDIpsName,envir=.GlobalEnv)
	oact<-str40("ofbody",ofbody)
	ocurdir<-str40("curdir",curdir)
	ooutdir<-str40("outdir",outdir)
        if(exists("ofref")) rm("ofref",envir=.GlobalEnv)
        b20<-"                    "
        oref<-paste(b20,"                  NA",sep="")
        tkgrid(tklabel(frameMiddle,text=oact,width=40),column=2,row=6,sticky="e")
        tkgrid(tklabel(frameMiddle,text=oref,width=40),column=2,row=7,sticky="e")
        tkgrid(tklabel(frameMiddle,text=ocurdir,width=40),column=2,row=8,sticky="e")
        tkgrid(tklabel(frameMiddle,text=ooutdir,width=40),column=2,row=9,sticky="e")
        tkinsert(txt,"end","FindUD finished successfully...\n")
        tkinsert(txt,"end",paste("Modify",UDIpsName,"for further calculation...\n\n"))
      }
      tkdestroy(tt)
      tkfocus(main)
      return()
    }
  }
  Ok2.but<-tkbutton(tt,text="   OK   ",command=OnOk2)
  tkgrid(Ok2.but,column=3,row=4)
  tkfocus(tt)
}

######################################################################################################################################################


OnStepSize<-function(){
  getfile1<-function(){
    if(!exists("ifname")) ifname<-tclvalue(tkgetOpenFile())
    else ifname<-tclvalue(tkgetOpenFile(initialfile=ifname))
    if(!nchar(ifname)){
      tkinsert(txt,"end","No file selected in StepSize!\n\n")
      return()
    }
    otmp<-str40("ifname",ifname)
    tkgrid(tklabel(tt,text=otmp,width=40),column=2,row=2,sticky="e")
    outdirtmp<-strsplit(ifname,"/")[[1]]
    if(length(outdirtmp)<=2){
      curdir<-paste(outdirtmp[1])
      outdir<-paste(outdirtmp[1],"output",sep=":/")
    }
    else{
      curdir<-outdirtmp[1]
      for(i in 2:(length(outdirtmp)-1))
        curdir<-paste(curdir,outdirtmp[i],sep="/")
        outdir<-paste(curdir,"output",sep="/")
    }
    setwd(curdir)
    if(!file.exists(outdir)) dir.create(outdir)
    ofname<-outdirtmp[length(outdirtmp)]
    itmp<-strsplit(ofname,"\\.")[[1]]
    if(length(itmp)<2) ofbody<-itmp[1]
    else{
      ofbody<-itmp[1]
      if(length(itmp)>2) for(i in 2:(length(itmp)-1))
      	ofbody<-paste(ofbody,itmp[i],sep="/")
    }
    ofname<-paste(outdir,ofbody,sep="/")
    assign("curdir",curdir,envir=.GlobalEnv)
    assign("outdir",outdir,envir=.GlobalEnv)
    assign("ifname",ifname,envir=.GlobalEnv)
    assign("ofbody",ofbody,envir=.GlobalEnv)
    assign("ofname",ofname,envir=.GlobalEnv)
  }
  getfile2<-function(){
    if(!exists("iDIpsName")) iDIpsName<-tclvalue(tkgetOpenFile())
    else iDIpsName<-tclvalue(tkgetOpenFile(initialfile=iDIpsName))
    otmp<-str40("iDIpsName",iDIpsName)
    tkgrid(tklabel(tt,text=otmp,width=40),column=2,row=3,sticky="e")
    assign("iDIpsName",iDIpsName,env=.GlobalEnv)
  }
  tt<-tktoplevel()
  button.chg1<-tkbutton(tt,text="Change",command=getfile1)
  button.chg2<-tkbutton(tt,text="Change",command=getfile2)
  tkwm.title(tt,"StepSize")
  oifname<-str40("ifname",ifname)
  oIpsName<-str40("iDIpsName",iDIpsName)
  fontLable<-tkfont.create(family="times",size=20,weight="bold")
  tkgrid(tklabel(tt,text="!!Do not",font=fontLable),sticky="e",column=1,row=1)
  tkgrid(tklabel(tt,text="choose daily precipitation data!!",
         font=fontLable),row=1,column=2)
  tkgrid(tklabel(tt,text="Input Data filename:"),column=1,row=2,sticky="w")
  tkgrid(tklabel(tt,text=oifname,width=40),column=2,row=2,sticky="e")
  tkgrid(button.chg1,column=3,row=2)

  tkgrid(tklabel(tt,text="Input changepoints filename:"),column=1,row=3,sticky="w")
  tkgrid(tklabel(tt,text=oIpsName,width=40),column=2,row=3,sticky="e")
  tkgrid(button.chg2,column=3,row=3)

  OnOk2<-function(){
    oflg<-1
    if(!file.exists(ifname)) {
      oflg<-0
      GuiErrorMSG<-paste("Input Data file ",ifname," does not exist!\n",sep="")
    }
    if(!file.exists(iDIpsName)) {
      oflg<-0
      GuiErrorMSG<-paste(GuiErrorMSG,"Input changepoint file ",iDIpsName,
                   " does not exist!\n",sep="")
    }
    if(oflg==0) {
      tkinsert(txt,"end",GuiErrorMSG)
      tkfocus(tt)
    }
    else{
      if(!as.numeric(PlevStr)%in%c(0.75,0.8,0.9,0.95,0.99,0.9999)){
        tkinsert(txt,"end","P_lev setting error, reset P_lev...")
        return()
      }
      outdirtmp<-strsplit(ifname,"/")[[1]]
      if(length(outdirtmp)<=2){
        curdir<-paste(outdirtmp[1])
        outdir<-paste(outdirtmp[1],"output",sep=":/")
      }
      else{
        curdir<-outdirtmp[1]
        for(i in 2:(length(outdirtmp)-1))
          curdir<-paste(curdir,outdirtmp[i],sep="/")
          outdir<-paste(curdir,"output",sep="/")
      }
      setwd(curdir)
      if(!file.exists(outdir)) dir.create(outdir)
      ofname<-outdirtmp[length(outdirtmp)]
      itmp<-strsplit(ofname,"\\.")[[1]]
      if(length(itmp)<2) ofbody<-itmp[1]
      else{
        ofbody<-itmp[1]
        if(length(itmp)>2) for(i in 2:(length(itmp)-1))
      	  ofbody<-paste(ofbody,itmp[i],sep="/")
      }
      ofname<-paste(outdir,ofbody,sep="/")
      assign("curdir",curdir,envir=.GlobalEnv)
      assign("outdir",outdir,envir=.GlobalEnv)
      assign("ofbody",ofbody,envir=.GlobalEnv)
      assign("ofname",ofname,envir=.GlobalEnv)
      itmp<-StepSize(InSeries=ifname,output=ofname,InCs=iDIpsName,
            MissingValueCode=MissingStr,p.lev=as.numeric(PlevStr),
	    Iadj=as.numeric(AdjStr),Mq=as.numeric(Mq0Str),Ny4a=as.numeric(Ny4aStr),
	    GUI=TRUE)
      if(itmp<0){
        tkinsert(txt,"end",ErrorMSG)
        return()
      }
      else{
        UIpsName1<-paste(ofname,"_fCs.txt",sep="")
        UIpsName<-paste(ofname,"_mCs.txt",sep="")
        file.copy(from=UIpsName1,to=UIpsName,overwrite=TRUE)
        oact<-str40("ofbody",ofbody)
	ocurdir<-str40("curdir",curdir)
        ooutdir<-str40("outdir",outdir)
        if(exists("ofref")) rm("ofref",envir=.GlobalEnv)
        b20<-"                    "
        oref<-paste(b20,"                  NA",sep="")
    tkgrid(tklabel(frameMiddle,text=oact,width=40),column=2,row=6,sticky="e")
    tkgrid(tklabel(frameMiddle,text=oref,width=40),column=2,row=7,sticky="e")
    tkgrid(tklabel(frameMiddle,text=ocurdir,width=40),column=2,row=8,sticky="e")
    tkgrid(tklabel(frameMiddle,text=ooutdir,width=40),column=2,row=9,sticky="e")
        tkinsert(txt,"end","StepSize finished successfully...\n")
        tkinsert(txt,"end",paste("Final output at ",outdir,"/",ofbody,"_*\n\n",sep=""))
      }
      tkdestroy(tt)
      tkfocus(main)
      return()
    }
  }
  Ok2.but<-tkbutton(tt,text="   OK   ",command=OnOk2)
  tkgrid(Ok2.but,column=3,row=4)
  tkfocus(tt)
}

######################################################################################################################################################


OnFindU.wRef<-function(){
  getfile1<-function(){
    if(!exists("ifname")) ifname<-tclvalue(tkgetOpenFile())
    else ifname<-tclvalue(tkgetOpenFile(initialfile=ifname))
    if(!nchar(ifname)){
      tkinsert(txt,"end","No Base file selected in FindU.wRef!\n\n")
      return()
    }
    assign("ifname",ifname,envir=.GlobalEnv)
    otmp<-str40("ifname",ifname)
    tkgrid(tklabel(tt,text=otmp,width=40),column=2,row=2,sticky="e")
    outdirtmp<-strsplit(ifname,"/")[[1]]
    if(length(outdirtmp)<=2){
      curdir<-paste(outdirtmp[1])
      outdir<-paste(outdirtmp[1],"output",sep=":/")
    }
    else{
      curdir<-outdirtmp[1]
      for(i in 2:(length(outdirtmp)-1))
        curdir<-paste(curdir,outdirtmp[i],sep="/")
        outdir<-paste(curdir,"output",sep="/")
    }
    setwd(curdir)
    if(!file.exists(outdir)) dir.create(outdir)
    ofname<-outdirtmp[length(outdirtmp)]
    itmp<-strsplit(ofname,"\\.")[[1]]
    if(length(itmp)<2) ofbody<-itmp[1]
    else{
      ofbody<-itmp[1]
      if(length(itmp)>2) for(i in 2:(length(itmp)-1))
      	ofbody<-paste(ofbody,itmp[i],sep="/")
    }
    assign("curdir",curdir,envir=.GlobalEnv)
    assign("outdir",outdir,envir=.GlobalEnv)
    assign("ofbody",ofbody,envir=.GlobalEnv)
  }
  getfile2<-function(){
    if(!exists("ifrname")) ifrname<-tclvalue(tkgetOpenFile())
    else ifrname<-tclvalue(tkgetOpenFile(initialfile=ifrname))
    if(!nchar(ifrname)){
      tkinsert(txt,"end","No Ref file selected in FindU.wRef!\n\n")
      return()
    }
    assign("ifrname",ifrname,env=.GlobalEnv)
    outdirtmp<-strsplit(ifrname,"/")[[1]]
    ofname<-outdirtmp[length(outdirtmp)]
    itmp<-strsplit(ofname,"\\.")[[1]]
    ofrbody<-itmp[1]
    assign("ofrbody",ofrbody,env=.GlobalEnv)
    otmp<-str40("ifrname",ifrname)
    tkgrid(tklabel(tt,text=otmp,width=40),column=2,row=3,sticky="e")
  }
  tt<-tktoplevel()
  button.chg1<-tkbutton(tt,text="Change",command=getfile1)
  button.chg2<-tkbutton(tt,text="Change",command=getfile2)
  tkwm.title(tt,"FindU.wRef")
  oifname<-str40("ifname",ifname)
  oifrname<-str40("ifrname",ifrname)

  fontLable<-tkfont.create(family="times",size=20,weight="bold")
  tkgrid(tklabel(tt,text="!!Do not",font=fontLable),sticky="e",column=1,row=1)
  tkgrid(tklabel(tt,text="choose daily precipitation data!!",
         font=fontLable),row=1,column=2)
  tkgrid(tklabel(tt,text="Input Base Data filename:"),column=1,row=2,sticky="w")
  tkgrid(tklabel(tt,text=oifname,width=40),column=2,row=2,sticky="e")
  tkgrid(button.chg1,column=3,row=2)

  tkgrid(tklabel(tt,text="Input Ref Data filename:"),column=1,row=3,sticky="w")
  tkgrid(tklabel(tt,text=oifrname,width=40),column=2,row=3,sticky="e")
  tkgrid(button.chg2,column=3,row=3)

  OnOk3<-function(){
    oflg<-1
    if(!file.exists(ifname)) {
      oflg<-0
      GuiErrorMSG<-paste("Input Data file ",ifname," does not exist!\n",sep="")
    }
    if(!file.exists(ifrname)) {
      oflg<-0
      GuiErrorMSG<-paste("Input Ref file ",ifrname," does not exist!\n",sep="")
    }
    if(oflg==0) {
      tkinsert(txt,"end",GuiErrorMSG)
      tkfocus(tt)
    }
    else{
      if(!as.numeric(PlevStr)%in%c(0.75,0.8,0.9,0.95,0.99,0.9999)){
        tkinsert(txt,"end","P_lev setting error, reset P_lev...")
	return()
      }
      ofname<-paste(paste(outdir,ofbody,sep="/"),ofrbody,sep="_")
      itmp<-FindU.wRef(Bseries=ifname,Rseries=ifrname,output=ofname,
            MissingValueCode=MissingStr,p.lev=as.numeric(PlevStr),
    	    Iadj=as.numeric(AdjStr),Mq=as.numeric(Mq0Str),Ny4a=as.numeric(Ny4aStr),
	    GUI=T)
      if(itmp<0){ # Error happens
        tkinsert(txt,"end",ErrorMSG)
        return()
      }
      else{
        assign("curdir",curdir,envir=.GlobalEnv)
        assign("outdir",outdir,envir=.GlobalEnv)
        assign("ifname",ifname,envir=.GlobalEnv)
        assign("ifrname",ifrname,envir=.GlobalEnv)
        assign("ofbody",ofbody,envir=.GlobalEnv)
        assign("ofname",ofname,envir=.GlobalEnv)
        UIpsName1<-paste(ofname,"_1Cs.txt",sep="")
        UIpsName<-paste(ofname,"_mCs.txt",sep="")
        file.copy(from=UIpsName1,to=UIpsName,overwrite=TRUE)
        assign("iIpsName",UIpsName1,envir=.GlobalEnv)
        assign("iDIpsName",UIpsName,envir=.GlobalEnv)
        oact<-str40("ofbody",ofbody)
        oref<-str40("ifrname",ifrname)
        ocurdir<-str40("curdir",curdir)
        ooutdir<-str40("outdir",outdir)
        tkgrid(tklabel(frameMiddle,text=oact,width=40),column=2,row=6,sticky="e")
        tkgrid(tklabel(frameMiddle,text=oref,width=40),column=2,row=7,sticky="e")
        tkgrid(tklabel(frameMiddle,text=ocurdir,width=40),column=2,row=8,sticky="e")
        tkgrid(tklabel(frameMiddle,text=ooutdir,width=40),column=2,row=9,sticky="e")
        tkinsert(txt,"end",paste("data: ",ifname,"\noutput: ",ofname,"_*\n",sep=""))
        if(itmp<0) tkinsert(txt,"end",ErrorMSG)
        else{
          tkinsert(txt,"end","FindU.wRef finished successfully...\n")
          tkinsert(txt,"end",paste("Modify",UIpsName,"for further calculation...\n\n"))
        }
      }
      tkdestroy(tt)
      tkfocus(main)
      return()
    }
  }
  Ok3.but<-tkbutton(tt,text="   OK   ",command=OnOk3)
  tkgrid(Ok3.but,column=3,row=4)
  tkfocus(tt)
}

######################################################################################################################################################


OnFindUD.wRef<-function(){
  getfile1<-function(){
    if(!exists("ifname")) ifname<-tclvalue(tkgetOpenFile())
    else ifname<-tclvalue(tkgetOpenFile(initialfile=ifname))
    if(!nchar(ifname)){
      tkinsert(txt,"end","No Base Data file selected in FindU.wRef!\n\n")
      return()
    }
    assign("ifname",ifname,envir=.GlobalEnv)
    otmp<-str40("ifname",ifname)
    tkgrid(tklabel(tt,text=otmp,width=40),column=2,row=2,sticky="e")
    outdirtmp<-strsplit(ifname,"/")[[1]]
    if(length(outdirtmp)<=2){
      curdir<-paste(outdirtmp[1])
      outdir<-paste(outdirtmp[1],"output",sep=":/")
    }
    else{
      curdir<-outdirtmp[1]
      for(i in 2:(length(outdirtmp)-1))
        curdir<-paste(curdir,outdirtmp[i],sep="/")
        outdir<-paste(curdir,"output",sep="/")
    }
    setwd(curdir)
    if(!file.exists(outdir)) dir.create(outdir)
    ofname<-outdirtmp[length(outdirtmp)]
    itmp<-strsplit(ofname,"\\.")[[1]]
    if(length(itmp)<2) ofbody<-itmp[1]
    else{
      ofbody<-itmp[1]
      if(length(itmp)>2) for(i in 2:(length(itmp)-1))
      	ofbody<-paste(ofbody,itmp[i],sep="/")
    }
    ofname<-paste(outdir,ofbody,sep="/")
    assign("curdir",curdir,envir=.GlobalEnv)
    assign("outdir",outdir,envir=.GlobalEnv)
    assign("ofbody",ofbody,envir=.GlobalEnv)
  }
  getfile2<-function(){
    if(!exists("ifrname")) ifrname<-tclvalue(tkgetOpenFile())
    else ifrname<-tclvalue(tkgetOpenFile(initialfile=ifrname))
    if(!nchar(ifrname)){
      tkinsert(txt,"end","No Ref Data file selected in FindU.wRef!\n\n")
      return()
    }
    assign("ifrname",ifrname,env=.GlobalEnv)
    outdirtmp<-strsplit(ifrname,"/")[[1]]
    ofname<-outdirtmp[length(outdirtmp)]
    itmp<-strsplit(ofname,"\\.")[[1]]
    ofrbody<-itmp[1]
    assign("ofrbody",ofrbody,env=.GlobalEnv)
    otmp<-str40("ifrname",ifrname)
    tkgrid(tklabel(tt,text=otmp,width=40),column=2,row=3,sticky="e")
  }
  getfile3<-function(){
    if(!exists("iDIpsName")) iDIpsName<-tclvalue(tkgetOpenFile())
    else iDIpsName<-tclvalue(tkgetOpenFile(initialfile=iDIpsName))
    assign("iDIpsName",iDIpsName,env=.GlobalEnv)
    otmp<-str40("iDIpsName",iDIpsName)
    tkgrid(tklabel(tt,text=otmp,width=40),column=2,row=4,sticky="e")
  }
  tt<-tktoplevel()
  button.chg1<-tkbutton(tt,text="Change",command=getfile1)
  button.chg2<-tkbutton(tt,text="Change",command=getfile2)
  button.chg3<-tkbutton(tt,text="Change",command=getfile3)
  tkwm.title(tt,"FindUD.wRef")
  oifname<-str40("ifname",ifname)
  oifrname<-str40("ifrname",ifrname)
  oIps<-str40("iDIpsName",iDIpsName)

  fontLable<-tkfont.create(family="times",size=20,weight="bold")
  tkgrid(tklabel(tt,text="!!Do not",font=fontLable),sticky="e",column=1,row=1)
  tkgrid(tklabel(tt,text="choose daily precipitation data!!",
         font=fontLable),row=1,column=2)
  tkgrid(tklabel(tt,text="Input Base Data filename:"),column=1,row=2,sticky="w")
  tkgrid(tklabel(tt,text=oifname,width=40),column=2,row=2,sticky="e")
  tkgrid(button.chg1,column=3,row=2)

  tkgrid(tklabel(tt,text="Input Ref Data filename:"),column=1,row=3,sticky="w")
  tkgrid(tklabel(tt,text=oifrname,width=40),column=2,row=3,sticky="e")
  tkgrid(button.chg2,column=3,row=3)

  tkgrid(tklabel(tt,text="Input changepoints filename:"),column=1,row=4,sticky="w")
  tkgrid(tklabel(tt,text=oIps,width=40),column=2,row=4,sticky="e")
  tkgrid(button.chg3,column=3,row=4)

  OnOk3<-function(){
    oflg<-1
    if(!file.exists(ifname)) {
      oflg<-0
      GuiErrorMSG<-paste("Input Base Data file ",ifname," does not exist!\n",sep="")
    }
    if(!file.exists(ifrname)) {
      oflg<-0
      GuiErrorMSG<-paste("Input Ref Data file ",ifrname," does not exist!\n",sep="")
    }
    if(!file.exists(iDIpsName)) {
      oflg<-0
      GuiErrorMSG<-paste("Input changepoint file ",iDIpsName," does not exist!\n",sep="")
    }
    if(oflg==0) {
      tkinsert(txt,"end",GuiErrorMSG)
      tkfocus(tt)
    }
    else{
      if(!as.numeric(PlevStr)%in%c(0.75,0.8,0.9,0.95,0.99,0.9999)){
        tkinsert(txt,"end","P_lev setting error, reset P_lev...")
	return()
      }
      ofname<-paste(paste(outdir,ofbody,sep="/"),ofrbody,sep="_")
      itmp<-FindUD.wRef(Bseries=ifname,Rseries=ifrname,InCs=iDIpsName,
            output=ofname,MissingValueCode=MissingStr,p.lev=as.numeric(PlevStr),
    	    Iadj=as.numeric(AdjStr),Mq=as.numeric(Mq0Str),Ny4a=as.numeric(Ny4aStr),GUI=T)
      if(itmp<0){ # Error happens
        tkinsert(txt,"end",ErrorMSG)
        return()
      }
      else{
        assign("curdir",curdir,envir=.GlobalEnv)
        assign("outdir",outdir,envir=.GlobalEnv)
        assign("ifname",ifname,envir=.GlobalEnv)
        assign("ifrname",ifrname,envir=.GlobalEnv)
        assign("ofbody",ofbody,envir=.GlobalEnv)
        assign("ofname",ofname,envir=.GlobalEnv)
        UIpsName1<-paste(ofname,"_pCs.txt",sep="")
        UIpsName<-paste(ofname,"_mCs.txt",sep="")
        file.copy(from=UIpsName1,to=UIpsName,overwrite=TRUE)
        assign("iIpsName",UIpsName,envir=.GlobalEnv)
        oact<-str40("ofbody",ofbody)
        oref<-str40("ifrname",ifrname)
        ocurdir<-str40("curdir",curdir)
        ooutdir<-str40("outdir",outdir)
        tkgrid(tklabel(frameMiddle,text=oact,width=40),column=2,row=6,sticky="e")
        tkgrid(tklabel(frameMiddle,text=oref,width=40),column=2,row=7,sticky="e")
        tkgrid(tklabel(frameMiddle,text=ocurdir,width=40),column=2,row=8,sticky="e")
        tkgrid(tklabel(frameMiddle,text=ooutdir,width=40),column=2,row=9,sticky="e")
        tkinsert(txt,"end",paste("data: ",ifname,"\noutput: ",ofname,"_*\n",sep=""))
        if(itmp<0) tkinsert(txt,"end",ErrorMSG)
        else{
          tkinsert(txt,"end","FindUD.wRef finished successfully...\n")
          tkinsert(txt,"end",paste("Modify",UIpsName,"for further calculation...\n\n"))
        }
      }
      tkdestroy(tt)
      tkfocus(main)
      return()
    }
  }
  Ok3.but<-tkbutton(tt,text="   OK   ",command=OnOk3)
  tkgrid(Ok3.but,column=3,row=5)
  tkfocus(tt)
}

######################################################################################################################################################


OnStepSize.wRef<-function(){
  getfile1<-function(){
    if(!exists("ifname")) ifname<-tclvalue(tkgetOpenFile())
    else ifname<-tclvalue(tkgetOpenFile(initialfile=ifname))
    if(!nchar(ifname)){
      tkinsert(txt,"end","No Base Data file selected in StepSize.wRef!\n\n")
      return()
    }
    assign("ifname",ifname,envir=.GlobalEnv)
    otmp<-str40("ifname",ifname)
    tkgrid(tklabel(tt,text=otmp,width=40),column=2,row=2,sticky="e")
    outdirtmp<-strsplit(ifname,"/")[[1]]
    if(length(outdirtmp)<=2){
      curdir<-paste(outdirtmp[1])
      outdir<-paste(outdirtmp[1],"output",sep=":/")
    }
    else{
      curdir<-outdirtmp[1]
      for(i in 2:(length(outdirtmp)-1))
        curdir<-paste(curdir,outdirtmp[i],sep="/")
        outdir<-paste(curdir,"output",sep="/")
    }
    setwd(curdir)
    if(!file.exists(outdir)) dir.create(outdir)
    ofname<-outdirtmp[length(outdirtmp)]
    itmp<-strsplit(ofname,"\\.")[[1]]
    if(length(itmp)<2) ofbody<-itmp[1]
    else{
      ofbody<-itmp[1]
      if(length(itmp)>2) for(i in 2:(length(itmp)-1))
      	ofbody<-paste(ofbody,itmp[i],sep="/")
    }
    ofname<-paste(outdir,ofbody,sep="/")
    assign("curdir",curdir,envir=.GlobalEnv)
    assign("outdir",outdir,envir=.GlobalEnv)
    assign("ofbody",ofbody,envir=.GlobalEnv)
  }
  getfile2<-function(){
    if(!exists("ifrname")) ifrname<-tclvalue(tkgetOpenFile())
    else ifrname<-tclvalue(tkgetOpenFile(initialfile=ifrname))
    if(!nchar(ifrname)){
      tkinsert(txt,"end","No Ref Data file selected in StepSize.wRef!\n\n")
      return()
    }
    assign("ifrname",ifrname,env=.GlobalEnv)
    outdirtmp<-strsplit(ifrname,"/")[[1]]
    ofname<-outdirtmp[length(outdirtmp)]
    itmp<-strsplit(ofname,"\\.")[[1]]
    ofrbody<-itmp[1]
    assign("ofrbody",ofrbody,env=.GlobalEnv)
    otmp<-str40("ifrname",ifrname)
    tkgrid(tklabel(tt,text=otmp,width=40),column=2,row=3,sticky="e")
  }
  getfile3<-function(){
    if(!exists("iDIpsName")) iDIpsName<-tclvalue(tkgetOpenFile())
    else iDIpsName<-tclvalue(tkgetOpenFile(initialfile=iDIpsName))
    assign("iDIpsName",iDIpsName,env=.GlobalEnv)
    otmp<-str40("iDIpsName",iDIpsName)
    tkgrid(tklabel(tt,text=otmp,width=40),column=2,row=4,sticky="e")
  }
  tt<-tktoplevel()
  button.chg1<-tkbutton(tt,text="Change",command=getfile1)
  button.chg2<-tkbutton(tt,text="Change",command=getfile2)
  button.chg3<-tkbutton(tt,text="Change",command=getfile3)
  tkwm.title(tt,"StepSize.wRef")
  oifname<-str40("ifname",ifname)
  oifrname<-str40("ifrname",ifrname)
  oIps<-str40("iDIpsName",iDIpsName)

  fontLable<-tkfont.create(family="times",size=20,weight="bold")
  tkgrid(tklabel(tt,text="!!Do not",font=fontLable),sticky="e",column=1,row=1)
  tkgrid(tklabel(tt,text="choose daily precipitation data!!",
         font=fontLable),row=1,column=2)
  tkgrid(tklabel(tt,text="Input Base Data filename:"),column=1,row=2,sticky="w")
  tkgrid(tklabel(tt,text=oifname,width=40),column=2,row=2,sticky="e")
  tkgrid(button.chg1,column=3,row=2)

  tkgrid(tklabel(tt,text="Input Ref Data filename:"),column=1,row=3,sticky="w")
  tkgrid(tklabel(tt,text=oifrname,width=40),column=2,row=3,sticky="e")
  tkgrid(button.chg2,column=3,row=3)

  tkgrid(tklabel(tt,text="Input changepoints filename:"),column=1,row=4,sticky="w")
  tkgrid(tklabel(tt,text=oIps,width=40),column=2,row=4,sticky="e")
  tkgrid(button.chg3,column=3,row=4)

  OnOk3<-function(){
    oflg<-1
    if(!file.exists(ifname)) {
      oflg<-0
      GuiErrorMSG<-paste("Input Base Data file ",ifname," does not exist!\n",sep="")
    }
    if(!file.exists(ifrname)) {
      oflg<-0
      GuiErrorMSG<-paste("Input Ref Data file ",ifrname," does not exist!\n",sep="")
    }
    if(!file.exists(iDIpsName)) {
      oflg<-0
      GuiErrorMSG<-paste("Input changepoint file ",iDIpsName," does not exist!\n",sep="")
    }
    if(oflg==0) {
      tkinsert(txt,"end",GuiErrorMSG)
      tkfocus(tt)
    }
    else{
      if(!as.numeric(PlevStr)%in%c(0.75,0.8,0.9,0.95,0.99,0.9999)){
        tkinsert(txt,"end","P_lev setting error, reset P_lev...")
	return()
      }
      ofname<-paste(paste(outdir,ofbody,sep="/"),ofrbody,sep="_")
      itmp<-StepSize.wRef(Bseries=ifname,Rseries=ifrname,InCs=iDIpsName,
            output=ofname,MissingValueCode=MissingStr,p.lev=as.numeric(PlevStr),
    	    Iadj=as.numeric(AdjStr),Mq=as.numeric(Mq0Str),Ny4a=as.numeric(Ny4aStr),GUI=T)
      if(itmp<0){ # Error happens
        tkinsert(txt,"end",ErrorMSG)
        return()
      }
      else{
        assign("curdir",curdir,envir=.GlobalEnv)
        assign("outdir",outdir,envir=.GlobalEnv)
        assign("ifname",ifname,envir=.GlobalEnv)
        assign("ifrname",ifrname,envir=.GlobalEnv)
        assign("ofbody",ofbody,envir=.GlobalEnv)
        assign("ofname",ofname,envir=.GlobalEnv)
        oact<-str40("ofbody",ofbody)
        oref<-str40("ifrname",ifrname)
        ocurdir<-str40("curdir",curdir)
        ooutdir<-str40("outdir",outdir)
        UIpsName1<-paste(ofname,"_fCs.txt",sep="")
        UIpsName<-paste(ofname,"_mCs.txt",sep="")
        file.copy(from=UIpsName1,to=UIpsName,overwrite=TRUE)
        tkgrid(tklabel(frameMiddle,text=oact,width=40),column=2,row=6,sticky="e")
        tkgrid(tklabel(frameMiddle,text=oref,width=40),column=2,row=7,sticky="e")
        tkgrid(tklabel(frameMiddle,text=ocurdir,width=40),column=2,row=8,sticky="e")
        tkgrid(tklabel(frameMiddle,text=ooutdir,width=40),column=2,row=9,sticky="e")
        tkinsert(txt,"end",paste("data: ",ifname,"\noutput: ",ofname,"_*\n",sep=""))
        if(itmp<0) tkinsert(txt,"end",ErrorMSG)
        else{
          tkinsert(txt,"end","StepSize.wRef finished successfully...\n")
        }
      }
      tkdestroy(tt)
      tkfocus(main)
      return()
    }
  }
  Ok3.but<-tkbutton(tt,text="   OK   ",command=OnOk3)
  tkgrid(Ok3.but,column=3,row=5)
  tkfocus(tt)
}

######################################################################################################################################################


OnQMadjDLY<-function(){
  getfile1<-function(){
    if(!exists("ifname")) ifname<-tclvalue(tkgetOpenFile())
    else ifname<-tclvalue(tkgetOpenFile(initialfile=ifname))
    if(!nchar(ifname)){
      tkinsert(txt,"end","No file selected in QMadjDLY!\n\n")
      return()
    }
    otmp<-str40("ifname",ifname)
    tkgrid(tklabel(tt,text=otmp,width=40),column=2,row=2,sticky="e")
    outdirtmp<-strsplit(ifname,"/")[[1]]
    if(length(outdirtmp)<=2){
      curdir<-paste(outdirtmp[1])
      outdir<-paste(outdirtmp[1],"output",sep=":/")
    }
    else{
      curdir<-outdirtmp[1]
      for(i in 2:(length(outdirtmp)-1))
        curdir<-paste(curdir,outdirtmp[i],sep="/")
        outdir<-paste(curdir,"output",sep="/")
    }
    setwd(curdir)
    if(!file.exists(outdir)) dir.create(outdir)
    ofname<-outdirtmp[length(outdirtmp)]
    itmp<-strsplit(ofname,"\\.")[[1]]
    if(length(itmp)<2) ofbody<-itmp[1]
    else{
      ofbody<-itmp[1]
      if(length(itmp)>2) for(i in 2:(length(itmp)-1))
      	ofbody<-paste(ofbody,itmp[i],sep="/")
    }
    ofname<-paste(outdir,ofbody,sep="/")
    assign("curdir",curdir,envir=.GlobalEnv)
    assign("outdir",outdir,envir=.GlobalEnv)
    assign("ifname",ifname,envir=.GlobalEnv)
    assign("ofbody",ofbody,envir=.GlobalEnv)
    assign("ofname",ofname,envir=.GlobalEnv)
  }
  getfile2<-function(){
    if(!exists("iDIpsName")) iDIpsName<-tclvalue(tkgetOpenFile())
    else iDIpsName<-tclvalue(tkgetOpenFile(initialfile=iDIpsName))
    otmp<-str40("iDIpsName",iDIpsName)
    tkgrid(tklabel(tt,text=otmp,width=40),column=2,row=3,sticky="e")
    assign("iDIpsName",iDIpsName,env=.GlobalEnv)
  }
  tt<-tktoplevel()
  button.chg1<-tkbutton(tt,text="Change",command=getfile1)
  button.chg2<-tkbutton(tt,text="Change",command=getfile2)
  tkwm.title(tt,"QMadj_GaussianDLY")
  oifname<-str40("ifname",ifname)
  oIpsName<-str40("iDIpsName",iDIpsName)
  fontLable<-tkfont.create(family="times",size=20,weight="bold")
  tkgrid(tklabel(tt,text="!!Do not",font=fontLable),sticky="e",column=1,row=1)
  tkgrid(tklabel(tt,text="choose daily precipitation data!!",
         font=fontLable),row=1,column=2)
  tkgrid(tklabel(tt,text="Input Data filename:"),column=1,row=2,sticky="w")
  tkgrid(tklabel(tt,text=oifname,width=40),column=2,row=2,sticky="e")
  tkgrid(button.chg1,column=3,row=2)

  tkgrid(tklabel(tt,text="Input changepoints filename:"),column=1,row=3,sticky="w")
  tkgrid(tklabel(tt,text=oIpsName,width=40),column=2,row=3,sticky="e")
  tkgrid(button.chg2,column=3,row=3)

  rb1 <- tkradiobutton(tt)
  rb2 <- tkradiobutton(tt)
  rb3 <- tkradiobutton(tt)
  rbValue<-tclVar("1")
  tkconfigure(rb1,variable=rbValue,value="1")
  tkconfigure(rb2,variable=rbValue,value="4")
  tkconfigure(rb3,variable=rbValue,value="12")
  tkgrid(tklabel(tt,text="Choose one of the following    "),column=1,row=4,sticky="w")
  tkgrid(tklabel(tt,text="No seasonality in trend or distribution"),column=1,row=5,sticky="w")
  tkgrid(tklabel(tt,text="Seasonal trends and distributions (DJF,MAM...)"),column=1,row=6,sticky="w")
  tkgrid(tklabel(tt,text="Monthly trends and distributions (Jan,Feb...) "),column=1,row=7,sticky="w")
  tkgrid(rb1,column=2,row=5)
  tkgrid(rb2,column=2,row=6)
  tkgrid(rb3,column=2,row=7)

  OnOk2<-function(){
    oflg<-1
    if(!file.exists(ifname)) {
      oflg<-0
      GuiErrorMSG<-paste("Input Data file ",ifname," does not exist!\n",sep="")
    }
    if(!file.exists(iDIpsName)) {
      oflg<-0
      GuiErrorMSG<-paste(GuiErrorMSG,"Input changepoint file ",iDIpsName,
                   " does not exist!\n",sep="")
    }
    rbVal<-as.character(tclvalue(rbValue))
    if(oflg==0) {
      tkinsert(txt,"end",GuiErrorMSG)
      tkfocus(tt)
    }
    else{
      if(rbVal=="1"){
        itmp<-QMadj.GaussianDLY(InSeries=ifname,output=ofname,InCs=iDIpsName,
              MissingValueCode=MissingStr,Iadj=as.numeric(AdjStr),
	      Mq=as.numeric(Mq0Str),Ny4a=as.numeric(Ny4aStr),GUI=TRUE)
        if(itmp<0){
          tkinsert(txt,"end",ErrorMSG)
          tkdestroy(tt)
          tkfocus(main)
          return()
        }
      }
      else{
        if(rbVal=="4"){
	  Snames<-c("DJF","MAM","JJA","SON")
	  Sstrts<-c(1201,301,601,901)
	  Sends<-c(229,531,831,1130)
	  Nseas<-4
	}
	else if(rbVal=="12"){
	  Snames<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	  Sstrts<-c((1:12)*100+1)
	  Sends<-c(131,229,331,430,531,630,731,831,930,1031,1130,1231)
	  Nseas<-12
	}
	if(is.csv(ifname)) idata<-read.csv(ifname)
	else idata<-read.table(ifname)
	mmdd<-idata[,2]*100+idata[,3]
	for(ith in 1:Nseas){
	  if(Sstrts[ith]>Sends[ith])
	    odata<-idata[mmdd>=Sstrts[ith]|mmdd<=Sends[ith],]
	  else
	    odata<-idata[mmdd>=Sstrts[ith]&mmdd<=Sends[ith],]
	  iftmp<-paste(ofname,"_",Snames[ith],".dat",sep="")
	  oftmp<-paste(ofname,"_",Snames[ith],sep="")
	  write.table(odata,file=iftmp,col.names=F,row.names=F)
          itmp<-QMadj.GaussianDLY(InSeries=iftmp,output=oftmp,InCs=iDIpsName,
                MissingValueCode=MissingStr,Iadj=as.numeric(AdjStr),
	        Mq=as.numeric(Mq0Str),Ny4a=as.numeric(Ny4aStr),GUI=TRUE)
          if(itmp<0){
            tkinsert(txt,"end",ErrorMSG)
            tkdestroy(tt)
            tkfocus(main)
            return()
	  }
        }
	ofileSout<-paste(ofname,"_QMadjDLYstat.txt",sep="")
	ofileAout<-paste(ofname,"_QMadjDLY.dat",sep="")
	if(file.exists(ofileSout)) file.remove(ofileSout)
	odata<-NULL
	for(ith in 1:Nseas){
	  ifileSout<-paste(ofname,"_",Snames[ith],"_QMadjDLYstat.txt",sep="")
	  ifileAout<-paste(ofname,"_",Snames[ith],"_QMadjDLY.dat",sep="")
	  iftmp<-paste(ofname,"_",Snames[ith],".dat",sep="")
	  if(ith>1) cat("\n\n",file=ofileSout,append=T)
	  cat(paste("#  ",ifname,"season:",Snames[ith],"\n"),file=ofileSout,append=T)
	  file.append(ofileSout,ifileSout)
	  file.remove(ifileSout)
	  i1data<-read.table(ifileAout)
	  odata<-rbind(odata,i1data)
	  file.remove(ifileAout)
	  file.remove(iftmp)
	}
	ymd<-odata[,1]*10000+odata[,2]*100+odata[,3]
	o1data<-odata[order(ymd),]
	write.table(o1data,file=ofileAout,col.names=F,row.names=F)
      }

        oact<-str40("ofbody",ofbody)
	ocurdir<-str40("curdir",curdir)
        ooutdir<-str40("outdir",outdir)
        if(exists("ofref")) rm("ofref",envir=.GlobalEnv)
        b20<-"                    "
        oref<-paste(b20,"                  NA",sep="")
    tkgrid(tklabel(frameMiddle,text=oact,width=40),column=2,row=5,sticky="e")
    tkgrid(tklabel(frameMiddle,text=oref,width=40),column=2,row=6,sticky="e")
    tkgrid(tklabel(frameMiddle,text=ocurdir,width=40),column=2,row=7,sticky="e")
    tkgrid(tklabel(frameMiddle,text=ooutdir,width=40),column=2,row=8,sticky="e")
        tkinsert(txt,"end","QMadjGaussianDLY finished successfully...\n")
        tkinsert(txt,"end",paste("Final output at ",outdir,"/",ofbody,"_*\n\n",sep=""))
      tkdestroy(tt)
      tkfocus(main)
      return()
    }
  }
  Ok2.but<-tkbutton(tt,text="   OK   ",command=OnOk2)
  tkgrid(Ok2.but,column=3,row=7)
  tkfocus(tt)
}

######################################################################################################################################################


OnQMadjGaussian.wRef<-function(){
  getfile1<-function(){
    if(!exists("ifname")) ifname<-tclvalue(tkgetOpenFile())
    else ifname<-tclvalue(tkgetOpenFile(initialfile=ifname))
    if(!nchar(ifname)){
      tkinsert(txt,"end","No Base Data file selected in OnQMadjGaussian.wRef!\n\n")
      return()
    }
    otmp<-str40("ifname",ifname)
    tkgrid(tklabel(tt,text=otmp,width=40),column=2,row=1,sticky="e")
    outdirtmp<-strsplit(ifname,"/")[[1]]
    if(length(outdirtmp)<=2){
      curdir<-paste(outdirtmp[1])
      outdir<-paste(outdirtmp[1],"output",sep=":/")
    }
    else{
      curdir<-outdirtmp[1]
      for(i in 2:(length(outdirtmp)-1))
        curdir<-paste(curdir,outdirtmp[i],sep="/")
      outdir<-paste(curdir,"output",sep="/")
    }
    setwd(curdir)
    if(!file.exists(outdir)) dir.create(outdir)
    ofname<-outdirtmp[length(outdirtmp)]
    itmp<-strsplit(ofname,"\\.")[[1]]
    if(length(itmp)<2) ofbody<-itmp[1]
    else{
      ofbody<-itmp[1]
      if(length(itmp)>2) for(i in 2:(length(itmp)-1))
        ofbody<-paste(ofbody,itmp[i],sep="/")
    }
    ofname<-paste(outdir,ofbody,sep="/")
    assign("curdir",curdir,envir=.GlobalEnv)
    assign("outdir",outdir,envir=.GlobalEnv)
    assign("ifname",ifname,envir=.GlobalEnv)
    assign("ofbody",ofbody,envir=.GlobalEnv)
    assign("ofname",ofname,envir=.GlobalEnv)
  }
  getfile2<-function(){
    if(!exists("ifrname")) ifrname<-tclvalue(tkgetOpenFile())
    else ifrname<-tclvalue(tkgetOpenFile(initialfile=ifrname))
    if(!nchar(ifrname)){
      tkinsert(txt,"end","No Ref Data file selected in OnQMadjGaussian.wRef!\n\n")
      return()
    }
    otmp<-str40("ifrname",ifrname)
    tkgrid(tklabel(tt,text=otmp,width=40),column=2,row=2,sticky="e")
    outdirtmp<-strsplit(ifrname,"/")[[1]]
    ofname<-outdirtmp[length(outdirtmp)]
    itmp<-strsplit(ofname,"\\.")[[1]]
    ofrbody<-itmp[1]
    assign("ifrname",ifrname,env=.GlobalEnv)
    assign("ofrbody",ofrbody,env=.GlobalEnv)
  }
  getfile3<-function(){
    if(!exists("iDIpsName")) iDIpsName<-tclvalue(tkgetOpenFile())
    else iDIpsName<-tclvalue(tkgetOpenFile(initialfile=iDIpsName))
    assign("iDIpsName",iDIpsName,env=.GlobalEnv)
    otmp<-str40("iDIpsName",iDIpsName)
    tkgrid(tklabel(tt,text=otmp,width=40),column=2,row=3,sticky="e")
  }
  tt<-tktoplevel()
  button.chg1<-tkbutton(tt,text="Change",command=getfile1)
  button.chg2<-tkbutton(tt,text="Change",command=getfile2)
  button.chg3<-tkbutton(tt,text="Change",command=getfile3)
  tkwm.title(tt,"QMadj_GaussianDLY.wRef")
  oifname<-str40("ifname",ifname)
  oifrname<-str40("ifrname",ifrname)
  oIps<-str40("iDIpsName",iDIpsName)

  fontLable<-tkfont.create(family="times",size=20,weight="bold")
  tkgrid(tklabel(tt,text="Input Base Data filename:"),column=1,row=1,sticky="w")
  tkgrid(tklabel(tt,text=oifname,width=40),column=2,row=1,sticky="e")
  tkgrid(button.chg1,column=3,row=1)

  tkgrid(tklabel(tt,text="Input Ref Data filename:"),column=1,row=2,sticky="w")
  tkgrid(tklabel(tt,text=oifrname,width=40),column=2,row=2,sticky="e")
  tkgrid(button.chg2,column=3,row=2)

  tkgrid(tklabel(tt,text="Input changepoints filename:"),column=1,row=3,sticky="w")
  tkgrid(tklabel(tt,text=oIps,width=40),column=2,row=3,sticky="e")
  tkgrid(button.chg3,column=3,row=3)

  OnOk3<-function(){
    oflg<-1
    if(!file.exists(ifname)) {
      oflg<-0
      GuiErrorMSG<-paste("Input Base Data file ",ifname," does not exist!\n",sep="")
    }
    if(!file.exists(ifrname)) {
      oflg<-0
      GuiErrorMSG<-paste("Input Ref Data file ",ifrname," does not exist!\n",sep="")
    }
    if(!file.exists(iDIpsName)) {
      oflg<-0
      GuiErrorMSG<-paste("Input changepoint file ",iDIpsName," does not exist!\n",sep="")
    }
    if(oflg==0) {
      tkinsert(txt,"end",GuiErrorMSG)
      tkfocus(tt)
    }
    else{
    #       if(!as.numeric(PlevStr)%in%c(0.75,0.8,0.9,0.95,0.99,0.9999)){
    #         tkinsert(txt,"end","P_lev setting error, reset P_lev...")
    #         return()
    #       }
    #ofname<-paste(paste(outdir,ofbody,sep="/"),ofrbody,sep="_")
    itmp<-QMadj.GaussianDLY.wRef(Bseries=ifname,Rseries=ifrname,InCs=iDIpsName,
                                 output=ofname,MissingValue=MissingStr,Iadj=as.numeric(AdjStr),
                                 Mq=as.numeric(Mq0Str),Ny4a=as.numeric(Ny4aStr))
    if(itmp<0){
      tkinsert(txt,"end",ErrorMSG)
      tkdestroy(tt)
      tkfocus(main)
      return()
    }
    else{
      assign("curdir",curdir,envir=.GlobalEnv)
      assign("outdir",outdir,envir=.GlobalEnv)
      assign("ifname",ifname,envir=.GlobalEnv)
      assign("ifrname",ifrname,envir=.GlobalEnv)
      assign("ofbody",ofbody,envir=.GlobalEnv)
      assign("ofname",ofname,envir=.GlobalEnv)

      oact<-str40("ofbody",ofbody)
      oref<-str40("ifrname",ifrname)
      ocurdir<-str40("curdir",curdir)
      ooutdir<-str40("outdir",outdir)
#     UIpsName1<-paste(ofname,"_fCs.txt",sep="")
#     UIpsName<-paste(ofname,"_mCs.txt",sep="")
#     file.copy(from=UIpsName1,to=UIpsName,overwrite=TRUE)
      tkgrid(tklabel(frameMiddle,text=oact,width=40),column=2,row=6,sticky="e")
      tkgrid(tklabel(frameMiddle,text=oref,width=40),column=2,row=7,sticky="e")
      tkgrid(tklabel(frameMiddle,text=ocurdir,width=40),column=2,row=8,sticky="e")
      tkgrid(tklabel(frameMiddle,text=ooutdir,width=40),column=2,row=9,sticky="e")
      tkinsert(txt,"end",paste("data: ",ifname,"\noutput: ",ofname,"_*\n",sep=""))
      if(itmp<0) tkinsert(txt,"end",ErrorMSG)
      else{
        tkinsert(txt,"end","QMadjGaussian.wRef finished successfully...\n")
      }
    }
    tkdestroy(tt)
    tkfocus(main)
    return()
  }
}
Ok3.but<-tkbutton(tt,text="   OK   ",command=OnOk3)
tkgrid(Ok3.but,column=3,row=5)
tkfocus(tt)
}

######################################################################################################################################################


#OnQuit<-function(){
#  if(exists("curdir")) rm("curdir",envir=.GlobalEnv)
#  if(exists("outdir")) rm("outdir",envir=.GlobalEnv)
#  if(exists("ofbody")) rm("ofbody",envir=.GlobalEnv)
#  if(exists("ofref")) rm("ofref",envir=.GlobalEnv)
#  if(exists("ofname")) rm("ofname",envir=.GlobalEnv)
#  if(exists("ifname")) rm("ifname",envir=.GlobalEnv)
#  if(exists("ifrname")) rm("ifrname",envir=.GlobalEnv)
#  if(exists("iIpsName")) rm("iIpsName",envir=.GlobalEnv)
#  if(exists("iDIpsName")) rm("iDIpsName",envir=.GlobalEnv)
#  if(exists("MissingStr")) rm("MissingStr",envir=.GlobalEnv)
#  if(exists("PlevStr")) rm("PlevStr",envir=.GlobalEnv)
#  if(exists("AdjStr")) rm("AdjStr",envir=.GlobalEnv)
#  if(exists("Mq0Str")) rm("Mq0Str",envir=.GlobalEnv)
#  if(exists("Ny4a")) rm("Ny4a",envir=.GlobalEnv)
#  if(exists("xscr")) rm("xscr",envir=.GlobalEnv)
#  if(exists("yscr")) rm("yscr",envir=.GlobalEnv)
#  if(exists("txt")) rm("txt",envir=.GlobalEnv)
#  if(exists("ErrorMSG")) rm("ErrorMSG",envir=.GlobalEnv)
#  if(exists("textMissing")) rm("textMissing",envir=.GlobalEnv)
#  tkdestroy(main)
#  if(exists("main")) rm("main",envir=.GlobalEnv)
#}

#######################################################################################################################################################


#Chg.Para<-function(){
#  tt<-tktoplevel()
#  tkwm.title(tt,"Change Parameters")

#  textMissing<<-tclVar(paste(MissingStr))
#  Entry.Missing<-tkentry(tt,width="10",textvariable=textMissing)
#  tkgrid(tklabel(tt,text="Please enter the Missing Value Code."),sticky="w",
#         column=1,row=1)
#  tkgrid(Entry.Missing,column=2,row=1)

#  textPlev<<-tclVar(paste(PlevStr))
#  Entry.Plev<-tkentry(tt,width="10",textvariable=textPlev)
#  tkgrid(tklabel(tt,text="Please enter the nominal conf. level p.lev value."),
#         sticky="w",column=1,row=2)
#  tkgrid(Entry.Plev,column=2,row=2)

#  textAdj<<-tclVar(paste(AdjStr))
#  Entry.Adj<-tkentry(tt,width="10",textvariable=textAdj)
#  tkgrid(tklabel(tt,text="Please enter integer Iadj (0 to 10000 inclusive)"),
#                 sticky="w",column=1,row=3)
#  tkgrid(Entry.Adj,column=2,row=3)

#  textMq0<<-tclVar(paste(Mq0Str))
#  Entry.Mq0<-tkentry(tt,width="10",textvariable=textMq0)
#  tkgrid(tklabel(tt,text="Please enter integer Mq (# of points for evaluating PDF)"),
#         sticky="w",column=1,row=4)
#  tkgrid(Entry.Mq0,column=2,row=4)

#  textNy4a<<-tclVar(paste(Ny4aStr))
#  Entry.Ny4a<-tkentry(tt,width="10",textvariable=textNy4a)
#  tkgrid(tklabel(tt,text="Please enter integer Ny4a (>=5, or 0 for choosing the whole segment)"),
#                 sticky="w",column=1,row=5)
#  tkgrid(Entry.Ny4a,column=2,row=5)

#  OnOk1<-function(){
#    oflg<-1
#    GuiErrorMSG<-NULL
#    MissingStr<-tclvalue(textMissing)
#    olen<-40
#    assign("MissingStr",MissingStr,envir=.GlobalEnv)
#    if(nchar(MissingStr)>olen) {
#      GuiErrorMSG<-"MissingCode length error!\n"
#      oflg<-0
#    }

#    PlevStr<-tclvalue(textPlev)
#    if(!as.numeric(PlevStr)%in%c(0.75,0.8,0.9,0.95,0.99,0.9999)){
#      GuiErrorMSG<-paste(GuiErrorMSG,"p.lev must be one of these: 0.75,0.80,0.90,0.95,0.99,0.9999. Please re-enter\n")
#      oflg<-0
#    }

#    AdjStr<-tclvalue(textAdj)
#    if(!as.numeric(AdjStr)%in%c(0:10000)){
#      GuiErrorMSG<-paste(GuiErrorMSG,"Integer Iadj must be between 0 and 10000 inclusive, please re-enter\n")
#      oflg<-0
#    }

#    Mq0Str<-tclvalue(textMq0)
#    if(!as.numeric(Mq0Str)%in%c(0:100)){
#      GuiErrorMSG<-paste(GuiErrorMSG,"Mq setting must be an integer between 0 and 100, please re-enter\n")
#      oflg<-0
#    }

#    Ny4aStr<-tclvalue(textNy4a)
#    if(as.numeric(Ny4aStr)!=as.integer(Ny4aStr)){
#       GuiErrorMSG<-paste(GuiErrorMSG,"Ny4a must be an integer, please re-enter\n")
#       oflg<-0
#    }

#    if(oflg==0) {
#      tkinsert(txt,"end",GuiErrorMSG)
#      tkfocus(tt)
#    }
#    else{
#      assign("MissingStr",MissingStr,envir=.GlobalEnv)
#      assign("PlevStr",PlevStr,envir=.GlobalEnv)
#      assign("AdjStr",AdjStr,envir=.GlobalEnv)
#      assign("Mq0Str",Mq0Str,envir=.GlobalEnv)
#      assign("Ny4aStr",Ny4aStr,envir=.GlobalEnv)

#      tkinsert(txt,"end",paste("MissingValueCode set to:",MissingStr,"..\n",sep=" "))
#      tkinsert(txt,"end",paste("The nominal level p.lev = ",PlevStr,"..\n",sep=" "))
#      tkinsert(txt,"end",paste("Current Iadj value is",AdjStr,"..\n",sep=" "))
#      tkinsert(txt,"end",paste("Current Mq value is",Mq0Str,"..\n",sep=" "))
#      tkinsert(txt,"end",paste("Current Ny4a value is",Ny4aStr,"..\n",sep=" "))

#      b20<-"                    "
#      olen<-40
#      otmp<-paste(b20,b20,sep="")
#      substr(otmp,olen+1-nchar(MissingStr),olen)<-MissingStr
#      omiss<-otmp
#      tkgrid(tklabel(frameMiddle,text=omiss,width=40),column=2,row=1,sticky="e")

#      otmp<-paste(b20,b20,sep="")
#      substr(otmp,olen+1-nchar(PlevStr),olen)<-PlevStr
#      oplev<-otmp
#      tkgrid(tklabel(frameMiddle,text=oplev,width=40),column=2,row=2,sticky="e")

#      otmp<-paste(b20,b20,sep="")
#      substr(otmp,olen+1-nchar(AdjStr),olen)<-AdjStr
#      oadj<-otmp
#      tkgrid(tklabel(frameMiddle,text=oadj,width=40),column=2,row=3,sticky="e")

#      otmp<-paste(b20,b20,sep="")
#      substr(otmp,olen+1-nchar(Mq0Str),olen)<-Mq0Str
#      omq0<-otmp
#      tkgrid(tklabel(frameMiddle,text=omq0,width=40),column=2,row=4,sticky="e")

#      otmp<-paste(b20,b20,sep="")
#      substr(otmp,olen+1-nchar(Ny4aStr),olen)<-Ny4aStr
#      ony0<-otmp
#      tkgrid(tklabel(frameMiddle,text=ony0,width=40),column=2,row=5,sticky="e")

#      tkdestroy(tt)
#      tkfocus(main)
#      return()
#    }
#  }

#  tkbind(Entry.Ny4a,"<Return>",OnOk1)

#  Ok1.but<-tkbutton(tt,text="   OK   ",command=OnOk1)
#  tkbind(Entry.Missing,"<Return>",OnOk1)
#  tkgrid(Ok1.but,column=1,sticky="e",row=6)
#  tkfocus(tt)
#}

#######################################################################################################################################################


#StartGUI<-function(){
#  require(tcltk)

#GUI<-function(){
#  if(!exists("MissingStr")) MissingStr<-"-99.9"
#  if(!exists("PlevStr")) PlevStr<-"0.95"
#  if(!exists("AdjStr")) AdjStr<-"10000"
#  if(!exists("Mq0Str")) Mq0Str<-"12"
#  if(!exists("Ny4aStr")) Ny4aStr<-"0"
#  assign("MissingStr",MissingStr,envir=.GlobalEnv)
#  assign("PlevStr",PlevStr,envir=.GlobalEnv)
#  assign("AdjStr",AdjStr,envir=.GlobalEnv)
#  assign("Mq0Str",Mq0Str,envir=.GlobalEnv)
#  assign("Ny4aStr",Ny4aStr,envir=.GlobalEnv)
#  main<-tktoplevel()
#  assign("main",main,envir=.GlobalEnv)
#  tkwm.title(main,"RHtestsV4")
#  fontHeading<-tkfont.create(family="times",size=40,weight="bold",
#                             slant="italic")
#  fontLable<-tkfont.create(family="times",size=15,weight="bold")

#  frameOverall<-tkframe(main)
#  frameUpper<-tkframe(frameOverall,relief="groove",borderwidth=2)
#  assign("frameUpper",frameUpper,env=.GlobalEnv)
#  frameMiddle<-tkframe(frameOverall,relief="groove",borderwidth=2)
#  assign("frameMiddle",frameMiddle,env=.GlobalEnv)
#  tkgrid(tklabel(frameUpper,text="RHtests V4",font=fontHeading),column=2,row=1)

#  ChgPara.but<-tkbutton(frameUpper,text="Change Pars",width=14,command=Chg.Para)
#  Rfile.but<-tkbutton(frameUpper,text="Transform Data",width=14,command=Read.file)
#  FindU.but<-tkbutton(frameUpper,text= "FindU",width=14,command=OnFindU)
#  FindUD.but<-tkbutton(frameUpper,text="FindUD",width=14,command=OnFindUD)
#  StepSize.but<-tkbutton(frameUpper,text="StepSize",width=14,command=OnStepSize)
#  Cancel.but<-tkbutton(frameUpper,text="Quit",width=14,command=OnQuit)
#  FindU.wRef.but<-tkbutton(frameUpper,text= "FindU.wRef",width=14,
#                           command=OnFindU.wRef)
#  FindUD.wRef.but<-tkbutton(frameUpper,text="FindUD.wRef",width=14,
#                            command=OnFindUD.wRef)
#  StepSize.wRef.but<-tkbutton(frameUpper,text="StepSize.wRef",width=14,
#                              command=OnStepSize.wRef)
#  QMadjDLY.but<-tkbutton(frameUpper,text="QMadj",width=14,
#                              command=OnQMadjDLY)
#  QMadjDLY.wRef.but<-tkbutton(frameUpper,text="QMadj.wRef",width=14,
#                         command=OnQMadjGaussian.wRef)
#    olen<-40
#  b20<-"                    "
#  if(nchar(MissingStr)>olen) stop("MissingValueCode length error!")
#  otmp<-paste(b20,b20,sep="")
#  substr(otmp,olen+1-nchar(MissingStr),olen)<-MissingStr
#  omiss<-otmp
#  if(nchar(PlevStr)>olen) stop("P_lev length error!")
#  otmp<-paste(b20,b20,sep="")
#  substr(otmp,olen+1-nchar(PlevStr),olen)<-PlevStr
#  oplev<-otmp
#  otmp<-paste(b20,b20,sep="")
#  substr(otmp,olen+1-nchar(AdjStr),olen)<-AdjStr
#  oadj<-otmp
#  otmp<-paste(b20,b20,sep="")
#  substr(otmp,olen+1-nchar(Mq0Str),olen)<-Mq0Str
#  omq0<-otmp
#  otmp<-paste(b20,b20,sep="")
#  substr(otmp,olen+1-nchar(Ny4aStr),olen)<-Ny4aStr
#  oNy4a<-otmp
#  if(!exists("ofbody")) otmp<-paste(b20,"                  NA",sep="")
#  else{
#    if(nchar(ofbody)>olen) otmp<-paste("...",substr(ofbody,nchar(ofbody)-olen+4,nchar(ofbody)),sep="")
#    else{
#      otmp<-paste(b20,b20,sep="")
#      substr(otmp,olen+1-nchar(ofbody),olen)<-ofbody
#    }
#  }
#  oact<-otmp
#  if(!exists("ofref")) otmp<-paste(b20,"                  NA",sep="")
#  else{
#    if(nchar(ofref)>olen) otmp<-paste("...",substr(ofref,nchar(ofref)-olen+4,nchar(ofref)),sep="")
#    else{
#      otmp<-paste(b20,b20,sep="")
#      substr(otmp,olen+1-nchar(ofref),olen)<-ofref
#    }
#  }
#  oref<-otmp
#  if(!exists("curdir")) otmp<-paste(b20,"                  NA",sep="")
#  else{
#    if(nchar(curdir)>olen) otmp<-paste("...",substr(curdir,nchar(curdir)-olen+4,nchar(curdir)),sep="")
#    else{
#      otmp<-paste(b20,b20,sep="")
#      substr(otmp,olen+1-nchar(curdir),olen)<-curdir
#    }
#  }
#  ocurdir<-otmp
#  if(!exists("outdir")) otmp<-paste(b20,"                  NA",sep="")
#  else{
#    if(nchar(outdir)>olen) otmp<-paste("...",substr(outdir,nchar(outdir)-olen+4,nchar(outdir)),sep="")
#    else{
#      otmp<-paste(b20,b20,sep="")
#      substr(otmp,olen+1-nchar(outdir),olen)<-outdir
#    }
#  }
#  ooutdir<-otmp
## arrange menu buttons and labels
#  tkgrid(tklabel(frameUpper,text="          "),Rfile.but,ChgPara.but,Cancel.but)
#  tkgrid(tklabel(frameUpper,text="PMT and t tests:",font=fontLable),sticky="w")
#  tkgrid(FindU.wRef.but,column=1,row=3)
#  tkgrid(FindUD.wRef.but,column=2,row=3)
#  tkgrid(StepSize.wRef.but,column=3,row=3)
#  tkgrid(tklabel(frameUpper,text="PMF and F tests:",font=fontLable),sticky="w")
#  tkgrid(FindU.but,column=1,row=4)
#  tkgrid(FindUD.but,column=2,row=4)
#  tkgrid(StepSize.but,column=3,row=4)
#  tkgrid(tklabel(frameUpper,text="  To adjust daily",font=fontLable),sticky="w")
#  tkgrid(tklabel(frameUpper,text="Gaussian data:",font=fontLable),sticky="w",column=1,row=5)
#  tkgrid(QMadjDLY.wRef.but,column=2,row=5)
#  tkgrid(QMadjDLY.but,column=3,row=5)
#  tkgrid(tklabel(frameMiddle,text="Current Missing Value Code:"),
#         column=1,row=1,sticky="w")
#  tkgrid(tklabel(frameMiddle,text=omiss,width=40),column=2,row=1,sticky="w")
#  tkgrid(tklabel(frameMiddle,text="Current nominal level of confidence (p.lev):"),
#         column=1,row=2,sticky="w")
#  tkgrid(tklabel(frameMiddle,text=oplev,width=40),column=2,row=2,sticky="e")
#  tkgrid(tklabel(frameMiddle,text="Segment to which to adjust the series (Iadj):"),
#         column=1,row=3,sticky="w")
#  tkgrid(tklabel(frameMiddle,text=oadj,width=40),column=2,row=3,sticky="e")
#  tkgrid(tklabel(frameMiddle,text="Current Mq (# of points for evaluating PDF):"),
#         column=1,row=4,sticky="w")
#  tkgrid(tklabel(frameMiddle,text=omq0,width=40),column=2,row=4,sticky="e")
#  tkgrid(tklabel(frameMiddle,text="Current Ny4a (max # of years of data for estimating PDF):"),
#         column=1,row=5,sticky="w")
#  tkgrid(tklabel(frameMiddle,text=oNy4a,width=40),column=2,row=5,sticky="e")
#  tkgrid(tklabel(frameMiddle,text="Current input Base series filename:"),
#         column=1,row=6,sticky="w")
#  tkgrid(tklabel(frameMiddle,text=oact,width=40),column=2,row=6,sticky="w")
#  tkgrid(tklabel(frameMiddle,text="Current input Reference series filename:"),
#         column=1,row=7,sticky="w")
#  tkgrid(tklabel(frameMiddle,text=oref,width=40),column=2,row=7,sticky="w")
#  tkgrid(tklabel(frameMiddle,text="Current data directory:    "),
#         column=1,row=8,sticky="w")
#  tkgrid(tklabel(frameMiddle,text=ocurdir,width=40),column=2,row=8,sticky="e")
#  tkgrid(tklabel(frameMiddle,text="Current output directory:    "),
#         column=1,row=9,sticky="w")
#  tkgrid(tklabel(frameMiddle,text=ooutdir,width=40),column=2,row=9)

## frameMiddle<-tkframe(frameOverall,relief="groove",borderwidth=2)
## assign("frameMiddle",frameMiddle,env=.GlobalEnv)
#  frameLower<-tkframe(frameOverall,relief="groove",borderwidth=2)
#  assign("xscr",tkscrollbar(frameLower,repeatinterval=5,orient="horizontal",
#                    command=function(...)tkxview(txt,...)),envir=.GlobalEnv)
#  assign("yscr",tkscrollbar(frameLower,repeatinterval=5,
#                    command=function(...)tkyview(txt,...)),envir=.GlobalEnv)
#  assign("txt",tktext(frameLower,bg="white",font="courier",
#              xscrollcommand=function(...)tkset(xscr,...),
#              yscrollcommand=function(...)tkset(yscr,...)
#              ),envir=.GlobalEnv)
#  tkgrid(frameUpper)
#  tkgrid(frameMiddle)
#  tkgrid(txt,yscr)
#  tkgrid(xscr)
#  tkgrid.configure(yscr,sticky="ns")
#  tkgrid.configure(xscr,sticky="ew")
#  tkfocus(txt)
#  tkgrid(frameLower)
#  tkgrid(frameOverall)
#}

#GUI()

#UA1<-'RClimDex and RHtests software packages (all versions included), herein after called "The Library" \n'
#UA2<-'©  Copyright, Environment Canada, 2012, \n'
#UA3<-"The Library was created by the Climate Research Division of Environment Canada and as such all intellectual property rights (including copyright) that may exist in The Library are owned by the Government of Canada.  The Library software code is provided free under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, version 3.0 of the License. It is distributed under the terms of this license 'as-is' and has not been designed or prepared to meet any Licensee's particular requirements. Environment Canada makes no warranty, either express or implied, including but not limited to, warranties of merchantability or fitness for a particular purpose. In no event will Environment Canada be liable for any indirect, special, consequential or other damages attributed to the Licensee's use of The Library. In downloading The Library you understand and agree to these terms and those of the associated LGP License. See the GNU Lesser General Public License for more details.\n"
#UA4<-"You should have received a copy of the GNU Lesser General Public License along with This Library; if not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA."
#UserAgrement <- paste(UA1,UA2,UA3,UA4,sep='\n')
#yesAgree<-function(){
#tkdestroy(topFrame)
#GUI()
#}

#noAgree<-function(){
#tkdestroy(topFrame)
#}

#topFrame <- tktoplevel(pady=2, bg="gray94")
#outFrame <- tkframe(topFrame, padx=2, bg="gray94")
#buttonFrame <- tkframe(topFrame, padx=2, bg="gray94")
#tkwm.title(topFrame, "RHtestsV4 User Agreement")
#tkwm.resizable(topFrame, 0, 0)
#tkgrid(outFrame)
#tkgrid(buttonFrame)
#tkgrid.configure(outFrame, sticky="nsew")
#scrollBar <- tkscrollbar(outFrame, repeatinterval=5, command=function(...)tkyview(textFrame,...))
#textFrame <- tktext(outFrame,bg="gray94",font="courier",yscrollcommand=function(...)tkset(scrollBar,...))
#tkgrid(textFrame,scrollBar)
#tkgrid.configure(scrollBar,sticky="ns")
#tkinsert(textFrame,"end",paste(UserAgrement,sep=""))
#tkconfigure(textFrame, state="disabled")
#yesButton <- tkbutton(buttonFrame, text="I Agree", command=function()yesAgree())
#noButton <- tkbutton(buttonFrame, text="I Do Not Agree", command=function()noAgree())
#tkgrid(yesButton,noButton)
#tkbind(topFrame,"<Destroy>",function()noAgree())
#tkfocus(textFrame)
#}

#StartGUI()

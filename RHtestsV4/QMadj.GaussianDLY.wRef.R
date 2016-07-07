

QMadj.GaussianDLY.wRef<-function(Bseries,Rseries,InCs,output,station,station_ref,fsuffix='DLY',MissingValueCode=-99,Iadj=10000,Mq=20,Ny4a=5){


 #######################################################
Station<-paste('BaseSeries_',station,sep='')
Station_Ref<-paste('RefSeries_',station_ref,sep='')



  if(Ny4a>0&Ny4a<=5) Ny4a<-5

  Read.wRef(Bseries,Rseries,MissingValueCode)
  N<-length(Y0); Nadj<-Nt*Ny4a
  itmp<-readLines(InCs)
  if(length(itmp)<2) {
#   stop("There is no input Ips")
    Ns<-0
    Ips<-N
    Ids<-0
  }
  else{
    Ns<-length(itmp)-1
    Ips<-c(rep(0,Ns),N)
    Ids<-rep(0,Ns)
    for(i in 1:Ns){ # using YYYYMMDD as index, searching for the largest
                    # date less or equal to given YYYYMMDD
      ymdtmp<-as.numeric(substr(itmp[i+1],7,16))
      it<-match(ymdtmp,IY0)
      if(!is.na(it)) Ips[i]<-it
      else Ips[i]<-max(c(1:N)[IY0<=ymdtmp])
      Ids[i]<-as.numeric(substr(itmp[i+1],1,1))
    }
    if(sum(is.na(Ips))>0|!identical(Ips,sort(Ips))){
	InsertMessagesTxt(main.txt.out,paste("StepSize.wRef: Ips read in from ",basename(InCs),"error!"),format=TRUE)
#      ErrorMSG<<-paste("StepSize.wRef: Ips read in from ",basename(InCs),"error!\n",
#                 get("ErrorMSG",env=.GlobalEnv),"\n")
#      if(!GUI) cat(ErrorMSG)
      return(-1)
    }
  }
  if(Ns>0) {
    Nsegs<-Ips-c(0,Ips[1:Ns])
    Iseg.longest<-sort(Nsegs,index=T,decreasing=T)$ix[1]
  }
  else Iseg.longest<-0


##########################################################
## define output file

  ofileSout<-paste(output,paste("_QMadj",fsuffix,"wRefstat.txt",sep=''),sep="")
  file.create(ofileSout)


####################################################
###write "_QMadjDLYwRefstat.txt"

  cat(paste("The base data filename is:", Station,"; N=",N,"\n"),file=ofileSout)
  cat(paste("The ref data filename is:", Station_Ref,"\n"),file=ofileSout,append=T)

# if(Iadj==1|Iseg.longest==0) Iseg.adj<-Ns+1 else Iseg.adj<-Iseg.longest
  if(Iadj>(Ns+1)|Iseg.longest==0) Iseg.adj<-Ns+1
  else if(Iadj==0)Iseg.adj<-Iseg.longest
  else Iseg.adj<-Iadj

# estimate delta from Y0 (Base-Ref)
  otmp<-Rphi(Y0,Ips,Ns)
  cor<-otmp$cor
  muDif<-rep(0,Ns+1)
  for(i in 1:(Ns+1)){
    I0<- if(i==1) 1 else Ips[i-1]+1
    I2<- if(i>Ns) length(Y0) else Ips[i]
    muDif[i]<-mean(Y0[I0:I2])
  }

# transfer Ips(Base-Ref) to Ips(Base)
  Ips0<-Ips
  IY1<-bdata[,1]*10000+bdata[,2]*100+bdata[,3]
  IYM<-bdata[,2]*100+bdata[,3]
  inds<-sort(unique(IYM))
  rtmp<-cbind(1:length(IY1),IY1)
  Ips<-merge(IY0[Ips0],rtmp,by.x=1,by.y="IY1")[,2]
  Ips[Ns+1]<-length(IY1)

  Ti<-TiB
  assign("Ti",Ti,envir=.GlobalEnv)
  N<-length(TiB)
  IY0flg<-IYBflg
  assign("IY0flg",IY0flg,envir=.GlobalEnv)

  dtmp<-rmCycle(bdata)
  adjBase<-dtmp$Base
  EB<-rep(0,length(IY1))
  for(i in 1:length(IY1))
    EB[i]<-dtmp$EB[inds==IYM[i]]

  if(Ns>0)
  for(i in 1:(Ns+1)){
    I0<- if(i==1) 1 else Ips[i-1]+1
    I2<- if(i>Ns) N else Ips[i]
    DeltaD<-muDif[i]-muDif[Iseg.adj]
    adjBase[I0:I2]<-adjBase[I0:I2]+EB[I0:I2]-DeltaD # adjBase is base series adj to last seg
  }
  EPBa<-mean(adjBase)

  Ydata<-cbind(bdata[,1:3],adjBase)
  dtmp<-rmCycle(Ydata)  # adjusted and de-seasonalized Base series
  EB0<-dtmp$EB
  EEBd<-mean(EB0)
  EB<-rep(0,length(IY1))
  for(i in 1:length(IY1))
    EB[i]<-dtmp$EB[inds==IYM[i]]
  Aadj<-dtmp$Base  # de-seasonalize Badj
  Ipd<-c(N)
  dtmp<-LSmultipleRed(Aadj,Ti,Ipd)
# muD<-dtmp$mu[1]+EPBa-mean(bdata[,4])
  muD<-dtmp$mu[1]
  betaD<-dtmp$trend

  otmp<-rmCycle(bdata)
  EB<-rep(0,length(IY1))
  for(i in 1:length(IY1))
    EB[i]<-otmp$EB[inds==IYM[i]]
  Base<-otmp$Base
  EPB<-mean(bdata[,"data"],na.rm=T)
  tt<-TRUE
  Niter<-0
  while(tt){
    Niter<-Niter+1
    EB00<-EB
    otmp<-LSmultipleRed(Base,Ti,Ips)
    meanhat<-otmp$meanhat
    df<-(N-2-Nt-Ns)
    p.cor<-pt(abs(otmp$cor)*sqrt(df/(1-otmp$cor^2)),df)
    otrend<-otmp$trend
    corout<-c(otmp$cor,otmp$corl,otmp$corh,p.cor)
    sig<-otmp$sig
    p.tro<-otmp$p.tr
    if(Ns==0) {
      EB1<-EB00
      tt<-FALSE
      }
    else{
      for(i in 1:(Ns+1)){
        I0<- if(i==1) 1 else Ips[i-1]+1
        I2<- if(i>Ns) length(Base) else Ips[i]
        Delta<- sig[i+1]-sig[Iseg.adj+1]
        Base[I0:I2]<-Base[I0:I2]+EB00[I0:I2]-Delta
      }
# re-estimate seasonal cycle using adjusted series:
      EB1<-rep(0,length(inds))
      for(i in 1:length(inds))
        EB1[i]<-mean(Base[IYM==inds[i]],na.rm=T)
      for(i in 1:length(IY1))
        EB[i]<-EB1[inds==IYM[i]]
      for(i in 1:(Ns+1)){
        I0<- if(i==1) 1 else Ips[i-1]+1
        I2<- if(i>Ns) length(Base) else Ips[i]
        Delta<- sig[i+1]-sig[Iseg.adj+1]
        Base[I0:I2]<-Base[I0:I2]-EB[I0:I2]+Delta
      }
      VEB<-sqrt(var(EB1))
      if(is.na(VEB)) tt<-FALSE
      else { if(max(abs(EB0-EB1))<VEB/1000|Niter>20) tt<-FALSE }
      EB0<-EB1
    }
  }

  adj<-Base+EB
  adjB<-Base+EB
  meanhatD<-rep(0,N)
  if(Ns>0) for(i in 1:(Ns+1)){
    I1<-if(i==1) 1 else Ips[i-1]+1
    I2<-Ips[i]
    Delta<-sig[Iseg.adj+1]-sig[i+1]
    DeltaD<-muDif[Iseg.adj]-muDif[i]
    adj[I1:I2]<-adj[I1:I2]+Delta
    adjB[I1:I2]<-adjB[I1:I2]+DeltaD
    meanhatD[I1:I2]<-EEBd+muD+betaD*Ti[I1:I2]-DeltaD
  }
  else meanhatD<-EPB+muD+betaD*Ti

  if(Ns>0){
    QMout<-QMadjGaussian.wRef(bdata[,4],bdata[,4]-bdata[,5],Ips,Mq,Iseg.adj,Nadj,Nt,Ny4a)
    B<-QMout$PA


####################################################
###write "_QMadjDLYwRefstat.txt"

    cat(paste("Nseg_shortest =",QMout$Nseg.mn,"; Mq = ",QMout$Mq,"; Ny4a = ",Ny4a,"\n"),
        file=ofileSout,append=T)
    cat(paste("\n Adjust to segment", Iseg.adj,": from",
        if(Iseg.adj==1) 1 else Ips[Iseg.adj-1]+1,
        "to",Ips[Iseg.adj],"\n"),file=ofileSout,append=T)
#   cat("#Fcat, DP (CDF and Differnces in category mean)\n",file=ofileSout,
#       append=T)
    oline<-paste('#Fcat: frequency category boundaries\n',
                 '#DP: Difference in the category means\n#',sep='')
    for(i in 1:Ns) oline<-paste(oline,paste('Fcat.Seg',i,sep=''),paste('DP.Seg',i,sep=''))
    oline<-paste(oline,'\n')
    cat(oline,file=ofileSout,append=T)

    write.table(round(QMout$osmean,4),file=ofileSout,append=T,
                row.names=F,col.names=F)
    for(i in 1:(Ns+1)){
      I1<-if(i==1) 1 else Ips[i-1]+1
      I2<-Ips[i]
      if(i!=Iseg.adj)
      cat(paste("Seg. ",i,": mean of QM-adjustments =",round(QMout$AdjM[i],4),
          "\n",sep=""),file=ofileSout,append=T)
    }
  }
  else B<-Base
# else B<-Base-otmp$trend*Ti+EB
# B<-B+otmp$trend*Ti



#####################################################################################
##PLOT

  ofilePdf<-paste(output,paste("_QMadj",fsuffix,"wRef.pdf",sep=''),sep="")

  pdf(file=ofilePdf,onefile=T)
  op <- par(no.readonly = TRUE) # the whole list of settable par's.
  par(mfrow=c(3,1))
  par(mar=c(3,4,3,2)+.1,cex.main=.8,cex.lab=.8,cex.axis=.8,cex=.8)

  uyrs<-unique(floor(ori.bdata[,1]/10))*10
  labels<-NULL
  ats<-NULL
  for(i in 1:length(uyrs)){
    if(!is.na(match(uyrs[i],ori.bdata[,1]))){
      labels<-c(labels,uyrs[i])
      ats<-c(ats,match(uyrs[i],ori.bdata[,1]))
    }
  }

  pdata<-rep(NA,dim(ori.bdata)[1])
    pdata[owflg]<-Base+EB
  plot(1:dim(ori.bdata)[1],pdata,type="l",xlab="",ylab="",
       ylim=c(min(Base+EB),max(Base+EB)),
       xaxt="n",col="black",lwd=0.5,
       main="a. Base series and Mean-adjusted  base series")
  axis(side=1,at=ats,labels=labels)
# pdata[owflg]<-otmp$meanhat+mean(EB1)
# lines(1:dim(ori.bdata)[1],pdata,col="red")
  pdata[owflg]<-meanhatD
  lines(1:dim(ori.bdata)[1],pdata,col="blue")
  pdata[owflg]<-adjB
  lines(1:dim(ori.bdata)[1],pdata,col="red")

  pdata[owflg]<-B
  plot(1:dim(ori.bdata)[1],pdata,type="l",xlab="",ylab="",
       ylim=c(min(B),max(B)),
       xaxt="n",col="black",lwd=0.5,
       main="b. QM-adjusted base series")
  axis(side=1,at=ats,labels=labels)

  # test plot
  if(Ns>0){
    par(mar=c(4,5,3,2)+.1,cex=.8,mfrow=c(2,2),mgp=c(1.2,.5,0))
    col=0
    np<-0
    osp<-QMout$osp
    osmean<-QMout$osmean
    for(i in 1:(Ns+1)){
      Fd<-.5/QMout$Mq
      I1<-if(i==1) 1 else Ips[i-1]+1
      I2<-Ips[i]
      ymax<-max(osp[,2:3],na.rm=T); ymin<-min(osp[,2:3],na.rm=T)
      if(i!=Iseg.adj){
        np<-np+1
        if(col==0) {
          col<-2
          plot(osp[I1:I2,2],osp[I1:I2,3],xlim=c(0,1),ylim=c(ymin,ymax),
               type="l",lwd=1,col=col,xlab="Cumulative Frequency",
               ylab="QM Adjustment")
          title(cex.main=.9,main=paste("distribution of QM adjustments with Mq=",QMout$Mq),line=.5)
          icol<-2*np
          for(j in 1:QMout$Mq){
            lines(c(osmean[(j+1),icol]-Fd,osmean[(j+1),icol]+Fd),
                  c(rep(osmean[(j+1),(icol+1)],2)),col=col,lty=2,lwd=.5)
            if(j>=1&j<QMout$Mq) lines(rep(osmean[(j+1),icol]+Fd,2),
                  c(osmean[(j+1),(icol+1)],osmean[(j+2),(icol+1)]),col=col,lty=2,lwd=.5)
          }
        }
        else{
          col<-col+1
          lines(osp[I1:I2,2],osp[I1:I2,3],lwd=1,col=col)
          icol<-2*np
          for(j in 1:QMout$Mq){
            lines(c(osmean[(j+1),icol]-Fd,osmean[(j+1),icol]+Fd),
                  c(rep(osmean[(j+1),(icol+1)],2)),col=col,lty=2,lwd=.5)
            if(j>=1&j<QMout$Mq) lines(rep(osmean[(j+1),icol]+Fd,2),
                  c(osmean[(j+1),(icol+1)],osmean[(j+2),(icol+1)]),col=col,lty=2,lwd=.5)
          }
        }
        text(.15,ymax-np*(ymax-ymin)/(Ns*3),paste("Seg.",i))
        lines(c(.25,.30),rep(ymax-np*(ymax-ymin)/(Ns*3),2),lwd=2,col=col)
      }
      else np<-np+1
    }
  }
  par(op)
  dev.off()

#####################################################################################

  if(Ns>0){
    odata<-matrix(NA,dim(ori.bdata)[1],8)
    odata[,1]<-ori.bdata[,1]
    odata[,2]<-ori.bdata[,2]
    odata[,3]<-ori.bdata[,3]
    odata[,4]<-ori.bdata[,4]
    odata[owflg,5]<-round(B,4)        # QMadjusted series
    odata[owflg,6]<-round(adjB,4)     # Mean-adjusted series
    odata[owflg,7]<-round(B-ori.bdata[owflg,4],4)
    odata[owflg,8]<-round(adjB-ori.bdata[owflg,4],4)
  }
  else odata<-cbind(ori.itable[,c(1,2,3,4,4,4)],0,0)

####################################################
###write "_QMadjDLYwRef.dat"

  ofileAout<-paste(output,paste("_QMadj",fsuffix,"wRef.dat",sep=''),sep="")
  write.table(odata,file=ofileAout,na=as.character(MissingValueCode),col.names=F,row.names=F)

  InsertMessagesTxt(main.txt.out,paste(paste("QMadj",fsuffix,".wRef",sep=''),"finished successfully for",station))
  return(0)

}


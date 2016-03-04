
QMadj.GaussianDLY<-function(InSeries,InCs,output,station,fsuffix='DLY',MissingValueCode=-99,Iadj=10000,Mq=10,Ny4a=0,GUI=FALSE){

 #######################################################
Station<-paste('BaseSeries_',station,sep='')


#  ErrorMSG<-NA
#  assign("ErrorMSG",ErrorMSG,envir=.GlobalEnv)
  itmp<-ReadDLY.g(InSeries,MissingValueCode)
  if(itmp<0){
    insert.txt(main.txt.out,paste("QMadj.Gaussian: Error in read data from",Station),format=TRUE)
    #cat(paste("QMadj.GaussianDLY: Error in read data from",Station),'\n')
#    ErrorMSG<<-paste("QMadj.GaussianDLY: Error in read data from",Station,"\n",
#               get("ErrorMSG",env=.GlobalEnv),"\n")
#    if(!GUI) cat(ErrorMSG)
    return(-1)
  }
  N<-length(Y0); Nadj<-Ny4a*Nt
  itmp<-readLines(InCs)
  if(length(itmp)>=2){
    Ns<-length(itmp)-1
    Ips<-c(rep(0,Ns),N)
    for(i in 1:Ns){ # using YYYYMMDD as index, searching for the largest
                    # date less or equal to given YYYYMMDD
      ymdtmp<-as.numeric(substr(itmp[i+1],7,16))
      if(ymdtmp==as.integer(ymdtmp/100)*100) ymdtmp<-ymdtmp+15
      # set date as 15 if input break point is 00 for date
      it<-match(ymdtmp,IY0)
      if(!is.na(it)) Ips[i]<-it
      else Ips[i]<-max(c(1:N)[IY0<=ymdtmp])
    }
    if(sum(is.na(Ips))>0|!identical(Ips,sort(Ips))){
#      ErrorMSG<<-paste("QMadj.GaussianDLY: Ips read in from ",basename(InCs),"error:")
      for(i in 1:Ns)
		insert.txt(main.txt.out,paste("QMadj.Gaussian: Ips read in from ",basename(InCs),"error:",Ips[i]),format=TRUE)
		#cat(paste("QMadj.GaussianDLY: Ips read in from ",basename(InCs),"error:",Ips[i]),'\n')
#        ErrorMSG<<-paste(get("ErrorMSG",env=.GlobalEnv),Ips[i])
#      ErrorMSG<<-paste(get("ErrorMSG",env=.GlobalEnv),"\n\n")
#      if(!GUI) cat(ErrorMSG)
      return(-1)
    }
  }
  else{
	  Ns<-0
	  Ips<-N
  }


##########################################################
## define output file

  ofileSout<-paste(output,paste("_QMadj",fsuffix,"stat.txt",sep=''),sep="")
  file.create(ofileSout)

 ####################################################
###write "_QMadjDLYstat.txt"

  cat(paste("Input data filename:", Station,"; N=",N,"\n"),file=ofileSout)

  if(Ns>0) {
    Nsegs<-Ips-c(0,Ips[1:Ns])
    Iseg.longest<-sort(Nsegs,index=T,decreasing=T)$ix[1]
  }
  else Iseg.longest<-0

  if(Iadj>(Ns+1)|Iseg.longest==0) Iseg.adj<-Ns+1
  else if(Iadj==0)Iseg.adj<-Iseg.longest
  else Iseg.adj<-Iadj

  oout<-rmCycle(itable)
  Y1<-oout$Base
  EB<-oout$EB
  assign("EB",EB,envir=.GlobalEnv)
  if(length(EB)!=length(Icy)) {
	insert.txt(main.txt.out,"Annual cycle length (from non-missing data) differ from original dataset",format=TRUE)
	#cat("Annual cycle length (from non-missing data) differ from original dataset",'\n')
#    ErrorMSG<<-paste("Annual cycle length (from non-missing data) differ from original dataset",
#                     "\n",get("ErrorMSG",env=.GlobalEnv),"\n")
#    if(!GUI) cat(ErrorMSG)
    return(-1)
  }

  otmp<-LSmultiRedCycle(Y1,Ti,Ips,Iseg.adj)
  Y1<-otmp$Y0
  cor<-otmp$cor
  corl<-otmp$corl
  corh<-otmp$corh
  pcor<-pt(abs(cor)*sqrt((N-2)/(1-cor^2)),N-2)
  Rf<-otmp$resi
  W<-otmp$W
  L<-otmp$WL
  WU<-otmp$WU
  EB1<-otmp$EB

  itmp1<-cbind(EB1,Icy)
  itmp2<-cbind(1:N,Imd)
  colnames(itmp2)<-c("idx","Icy")
  itmp<-merge(itmp1,itmp2,by="Icy")
  EBfull<-itmp[order(itmp[,"idx"]),"EB1"]
  EEB<-mean(EB1,na.rm=T)

  if(Ns>0){
    Rb<-Y1-otmp$trend*Ti+EBfull
#   QMout<-QMadjDLY(Rb,Ips,Mq,Iseg.adj)
    QMout<-QMadjGaussian(Rb,Ips,Mq,Iseg.adj,Nadj)
    B<-QMout$PA
  }
  else B<-Y1-otmp$trend*Ti+EBfull

  adj<-otmp$Y0+EBfull
  B<-B+otmp$trend*Ti

 ####################################################
###write "_QMadjDLYstat.txt"

  if(Ns>0){
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
      cat(paste("Seg. ",i,": mean of QM-adjustments =",round(mean(QMout$W[I1:I2]),4),
          "\n",sep=""),file=ofileSout,append=T)
    }
  }

  cat(paste("#steps= ",Ns,"; trend=",round(otmp$trend,6),"(",
            round(otmp$betaL,6),",",round(otmp$betaU,6),") (p=",
	    round(otmp$p.tr,4),"); cor=",
	    round(cor,4),"(",round(corl,4),",",round(corh,4),")",
	    round(pcor,4),"\n"),
	    file=ofileSout,append=T)

  if(Ns>0) for(i in 1:Ns){
    I1<-if(i==1) 1 else Ips[i-1]+1
    I2<-Ips[i]
    Delta<-otmp$mu[Ns+1]-otmp$mu[i]
    adj[I1:I2]<-adj[I1:I2]+Delta
    stepsize<-otmp$mu[i+1]-otmp$mu[i]
    cat(paste(Ips[i],IY0[Ips[i]],"stepsize=",round(stepsize,4),"\n"),
        file=ofileSout,append=T)
  }

# oR<-Y1-otmp$meanhat
# oR[2:N]<-oR[2:N]-oR[1:(N-1)]*cor
# Ehat<-mean(otmp$meanhat)

  if(Ns>0){
    odata<-matrix(NA,dim(ori.itable)[1],8)
    odata[,1]<-ori.itable[,1]
    odata[,2]<-ori.itable[,2]
    odata[,3]<-ori.itable[,3]
    odata[olflg,4]<-round(otmp$Y0+EBfull,4)
    odata[olflg,5]<-round(B,4)
    odata[olflg,6]<-round(adj,4)
    odata[olflg,7]<-round(B-otmp$Y0-EBfull,4)
    odata[olflg,8]<-round(adj-otmp$Y0-EBfull,4)
  }
  else odata<-cbind(ori.itable[,c(1,2,3,4,4,4)],0,0)


####################################################
###write "_QMadjDLY.dat"

  ofileAout<-paste(output,paste("_QMadj",fsuffix,".dat",sep=''),sep="")
# ofileAout<-output # suggested by Lucie, user can choose whatever filename
  write.table(odata,file=ofileAout,na=as.character(MissingValueCode),col.names=F,row.names=F)



#####################################################################################
##PLOT


  ofilePdf<-paste(output,paste("_QMadj",fsuffix,".pdf",sep=''),sep="")


  pdf(file=ofilePdf,onefile=T)
  op <- par(no.readonly = TRUE) # the whole list of settable par's
  par(mfrow=c(2,1),cex.main=.8,cex.lab=.8,cex.axis=.8,cex=.8)

  uyrs<-unique(floor(ori.itable[,1]/10))*10
  labels<-NULL
  ats<-NULL
  for(i in 1:length(uyrs)){
    if(!is.na(match(uyrs[i],ori.itable[,1]))){
      labels<-c(labels,uyrs[i])
      ats<-c(ats,match(uyrs[i],ori.itable[,1]))
    }
  }
  par(mar=c(3,4,3,2)+.1)
  pdata<-rep(NA,dim(ori.itable)[1])
  pdata[olflg]<-otmp$Y0
  plot(1:dim(ori.itable)[1],pdata,type="l",xlab="",ylab="",
       ylim=c(min(otmp$Y0,otmp$meanhat),max(otmp$Y0,otmp$meanhat)),
       xaxt="n",col="black",lwd=.3,
       main="Base anomaly series and regression fit")
  axis(side=1,at=ats,labels=labels)
  pdata[olflg]<-otmp$meanhat
  lines(1:dim(ori.itable)[1],pdata,col="red")

  pdata[olflg]<-otmp$Y0+EBfull
  plot(1:dim(ori.itable)[1],pdata,type="l",xlab="",ylab="",
       ylim=c(min(otmp$Y0+EBfull,otmp$meanhat+EBfull),
              max(otmp$Y0+EBfull,otmp$meanhat+EBfull)),
       xaxt="n",col="black",lwd=.3,
       main="Base series and regression fit")
  axis(side=1,at=ats,labels=labels)

  pdata[olflg]<-otmp$meanhat+EEB
  lines(1:dim(ori.itable)[1],pdata,col="red")

  pdata[olflg]<-adj
  plot(1:dim(ori.itable)[1],pdata,type="l",xlab="",ylab="",
       ylim=c(min(c(adj,B)),max(c(adj,B))),
       xaxt="n",col="black",lwd=.3,
       main="Mean-adjusted base series")
  axis(side=1,at=ats,labels=labels)

  pdata[olflg]<-B
  plot(1:dim(ori.itable)[1],pdata,type="l",xlab="",ylab="",
       ylim=c(min(c(adj,B)),max(c(adj,B))),
       xaxt="n",col="black",lwd=.3,
       main="QM-adjusted base series")
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

  insert.txt(main.txt.out,paste(paste("QMadj",fsuffix,sep=''),"finished successfully for",station))
  return(0)
}


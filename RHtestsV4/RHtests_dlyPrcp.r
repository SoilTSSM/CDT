#**** License agreement ****
#       All users of any code in this package agree to the terms and conditions described in the 
#file License_RHtests__RClimDex_SoftwarePackages.pdf, which is also included in this package.
#*****************************
# last updated on 2013-07-19
# changed all as.real() to as.numeric() since R2.15 no longer support as.real()

# last updated on 2013-4-30
# changed Mq upper limit from 20 to 100 in adjDLYp() and StartGUI()

# last updated on 2013-02-22
# added StartGUI() function
# modified FindU(), FindUD() and StepSize(), output pdf file even there has no change points

# last updated on 2012-03-30
# set pthr=0.0 as default for FindU, FindUD and StepSize
# change Ti from 365/yr to 365.25/yr in ReadDLY()
# Changed in Fstat output, removed trend estimate for transfered daily PRCP data,
#   estimate trend from monthly total PRCP for both original series and QMadjusted
#   and IBC adjusted series.

# last updated on 2010-06-08
#   In FindU(), FindUD(), StepSize() and 3 corresponding .wRef() functions,
#   added a text indication "yes", "no " or "?  " in changepoint list
#   output file, also changed corresponding read-in part.

# last modified: 2010-05-06
# in QMadjGaussian(), add Nadj option, set empirical prob. segment as length
# Nadj instead of whole segment.
#
# last modified: 2010-01-14
# changed lambda choices from (0.4,0.2,0.1,0.05,0.02,0.01) to (1,0.5,0.2,0.1)

######################################################################################################################################################        

BCtrans<-function(P,lambda){
  if(sum(P<=0)>0) stop("non-positive value encountered, y<=0!")
  y<-if(lambda==0) log(P) else (P**lambda-1)/lambda
  return(y)
}

IVBCtrans<-function(P,lambda){
  y<-if(lambda==0) exp(P) else (P*lambda+1)**(1/lambda)
  return(y)
}



######################################################################################################################################################        


getMtrendFdly<-function(idata){
  cmon<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Agu","Sep","Oct","Nov","Dec")
  if(dim(idata)[2]!=4) stop("input data does not contain 4 columns in getMtrendFdly())")
  colnames(idata)<-c("year","month","day","data")
  yrs<-unique(idata[,"year"])
  mmean<-rep(NA,12)
  tdata<-NULL
  for(yth in 1:length(yrs)) {
    itmp<-ori.itable[ori.itable[,1]==yrs[yth],]
    for(mon in 1:12){
      mp<-NA
      mtmp<-itmp[itmp[,2]==mon,]
      if(length(mtmp)>0)
        if(dim(mtmp)[1]>20&sum(is.na(mtmp[,4]))<=3) mp<-sum(idata[idata[,1]==yrs[yth]&idata[,2]==mon,4])
      tdata<-rbind(tdata,c(yrs[yth],mon,mp))
    }
  }
  t1data<-tdata
  for(i in 1:12){
    if(sum(is.na(tdata[tdata[,2]==i,3])==F)<5) print(paste("monthly data too few at",cmon[i]))
    mmean[i]<-mean(tdata[tdata[,2]==i,3],na.rm=T)
    t1data[t1data[,2]==i,3]<-t1data[t1data[,2]==i,3]-mmean[i]
  }
  return(summary(lm(t1data[,3]~c(1:dim(tdata)[1])))$coef[2,1])
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
                         sep=",",na.strings="-99.9",colClasses=rep("real",6)),
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
                         na.strings="-99.9",colClasses=rep("real",6)),
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


OnFindU.dlyPrcp<-function(){
  getfile1<-function(){
    if(!exists("ifname")) ifname<-tclvalue(tkgetOpenFile())
    else ifname<-tclvalue(tkgetOpenFile(initialfile=ifname))
    if(!nchar(ifname)){
      tkinsert(txt,"end","No file selected in FindU.dlyPrcp!\n\n")
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
  tkwm.title(tt,"FindU.dlyPrcp")
  oifname<-str40("ifname",ifname)
  fontLable<-tkfont.create(family="times",size=20,weight="bold")
  #tkgrid(tklabel(tt,text="!!Do not",font=fontLable),sticky="e",column=1,row=1)
  #tkgrid(tklabel(tt,text="choose daily precipitation data!!",
  #                font=fontLable),row=1,column=2)
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
      itmp<-FindU.dlyPrcp(InSeries=ifname,output=ofname,
                  MissingValueCode=MissingStr,p.lev=as.numeric(PlevStr),
                  Iadj=as.numeric(AdjStr),Mq=as.numeric(Mq0Str),Ny4a=as.numeric(Ny4aStr),
                  pthr=as.numeric(pthrstr),GUI=TRUE)
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
        tkgrid(tklabel(frameMiddle,text=oact,width=40),column=2,row=7,sticky="e")
        #tkgrid(tklabel(frameMiddle,text=oref,width=40),column=2,row=8,sticky="e")
        tkgrid(tklabel(frameMiddle,text=ocurdir,width=40),column=2,row=8,sticky="e")
        tkgrid(tklabel(frameMiddle,text=ooutdir,width=40),column=2,row=9,sticky="e")
        tkinsert(txt,"end","FindU.dlyPrcp finished successfully...\n")
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

OnFindUD.dlyPrcp<-function(){
  getfile1<-function(){
    if(!exists("ifname")) ifname<-tclvalue(tkgetOpenFile())
    else ifname<-tclvalue(tkgetOpenFile(initialfile=ifname))
    if(!nchar(ifname)){
      tkinsert(txt,"end","No file selected in FindUD.dlyPrcp!\n\n")
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
  tkwm.title(tt,"FindUD.dlyPrcp")
  oifname<-str40("ifname",ifname)
  oIpsName<-str40("iDIpsName",iDIpsName)
  fontLable<-tkfont.create(family="times",size=20,weight="bold")
  #tkgrid(tklabel(tt,text="!!Do not",font=fontLable),sticky="e",column=1,row=1)
  #tkgrid(tklabel(tt,text="choose daily precipitation data!!",
  #               font=fontLable),row=1,column=2)
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
      itmp<-FindUD.dlyPrcp(InSeries=ifname,output=ofname,InCs=iDIpsName,
                   MissingValueCode=MissingStr,p.lev=as.numeric(PlevStr),
                   Iadj=as.numeric(AdjStr),Mq=as.numeric(Mq0Str),Ny4a=as.numeric(Ny4aStr),
                   pthr=as.numeric(pthrstr),GUI=TRUE)
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
        tkgrid(tklabel(frameMiddle,text=oact,width=40),column=2,row=7,sticky="e")
        #tkgrid(tklabel(frameMiddle,text=oref,width=40),column=2,row=7,sticky="e")
        tkgrid(tklabel(frameMiddle,text=ocurdir,width=40),column=2,row=8,sticky="e")
        tkgrid(tklabel(frameMiddle,text=ooutdir,width=40),column=2,row=9,sticky="e")
        tkinsert(txt,"end","FindUD.dlyPrcp finished successfully...\n")
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

OnStepSize.dlyprcp<-function(){
  getfile1<-function(){
    if(!exists("ifname")) ifname<-tclvalue(tkgetOpenFile())
    else ifname<-tclvalue(tkgetOpenFile(initialfile=ifname))
    if(!nchar(ifname)){
      tkinsert(txt,"end","No file selected in StepSize.dlyprcp!\n\n")
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
  tkwm.title(tt,"StepSize.dlyprcp")
  oifname<-str40("ifname",ifname)
  oIpsName<-str40("iDIpsName",iDIpsName)
  fontLable<-tkfont.create(family="times",size=20,weight="bold")
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
      itmp<-StepSize.dlyPrcp(InSeries=ifname,output=ofname,InCs=iDIpsName,
                     MissingValueCode=MissingStr,p.lev=as.numeric(PlevStr),
                     Iadj=as.numeric(AdjStr),Mq=as.numeric(Mq0Str),Ny4a=as.numeric(Ny4aStr),
                     pthr=as.numeric(pthrstr),GUI=TRUE)
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
        tkinsert(txt,"end","StepSize.dlyprcp finished successfully...\n")
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

OnadjDLYp<-function(){
  getfile1<-function(){
    if(!exists("ifname")) ifname<-tclvalue(tkgetOpenFile())
    else ifname<-tclvalue(tkgetOpenFile(initialfile=ifname))
    if(!nchar(ifname)){
      tkinsert(txt,"end","No file selected in adjDLYp!\n\n")
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
  tkwm.title(tt,"QMadjDLYp")
  oifname<-str40("ifname",ifname)
  oIpsName<-str40("iDIpsName",iDIpsName)
  fontLable<-tkfont.create(family="times",size=20,weight="bold")
  #tkgrid(tklabel(tt,text="!!Do not",font=fontLable),sticky="e",column=1,row=1)
  #tkgrid(tklabel(tt,text="choose daily precipitation data!!",
  #               font=fontLable),row=1,column=2)
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
        itmp<-adjDLYp(InSeries=ifname,output=ofname,InCs=iDIpsName,
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
          itmp<-adjDLYp(InSeries=iftmp,output=oftmp,InCs=iDIpsName,
                                  MissingValueCode=MissingStr,Iadj=as.numeric(AdjStr),
                                  Mq=as.numeric(Mq0Str),Ny4a=as.numeric(Ny4aStr),GUI=TRUE)
          if(itmp<0){
            tkinsert(txt,"end",ErrorMSG)
            tkdestroy(tt)
            tkfocus(main)
            return()
          }
        }
        ofileSout<-paste(ofname,"_adjDLYpstat.txt",sep="")
        ofileAout<-paste(ofname,"_adjDLYp.dat",sep="")
        if(file.exists(ofileSout)) file.remove(ofileSout)
        odata<-NULL
        for(ith in 1:Nseas){
          ifileSout<-paste(ofname,"_",Snames[ith],"_adjDLYpstat.txt",sep="")
          ifileAout<-paste(ofname,"_",Snames[ith],"_adjDLYp.dat",sep="")
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
      tkinsert(txt,"end","adjDLYp finished successfully...\n")
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
#  if(exists("pthrstr")) rm("pthrstr",envir=.GlobalEnv)
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
#  
#  textMissing<<-tclVar(paste(MissingStr))
#  Entry.Missing<-tkentry(tt,width="10",textvariable=textMissing)
#  tkgrid(tklabel(tt,text="Please enter the Missing Value Code."),sticky="w",
#         column=1,row=1)
#  tkgrid(Entry.Missing,column=2,row=1)
#  
#  textPlev<<-tclVar(paste(PlevStr))
#  Entry.Plev<-tkentry(tt,width="10",textvariable=textPlev)
#  tkgrid(tklabel(tt,text="Please enter the nominal conf. level p.lev value."),
#         sticky="w",column=1,row=2)
#  tkgrid(Entry.Plev,column=2,row=2)
#  
#  textAdj<<-tclVar(paste(AdjStr))
#  Entry.Adj<-tkentry(tt,width="10",textvariable=textAdj)
#  tkgrid(tklabel(tt,text="Please enter integer Iadj (0 to 10000 inclusive)"),
#         sticky="w",column=1,row=3)
#  tkgrid(Entry.Adj,column=2,row=3)
#  
#  textMq0<<-tclVar(paste(Mq0Str))
#  Entry.Mq0<-tkentry(tt,width="10",textvariable=textMq0)
#  tkgrid(tklabel(tt,text="Please enter integer Mq (# of points for evaluating PDF)"),
#         sticky="w",column=1,row=4)
#  tkgrid(Entry.Mq0,column=2,row=4)
#  
#  textNy4a<<-tclVar(paste(Ny4aStr))
#  Entry.Ny4a<-tkentry(tt,width="10",textvariable=textNy4a)
#  tkgrid(tklabel(tt,text="Please enter integer Ny4a (>=5, or 0 for choosing the whole segment)"),
#         sticky="w",column=1,row=5)
#  tkgrid(Entry.Ny4a,column=2,row=5)
#  
#  textpthr<<-tclVar(paste(pthrstr))
#  Entry.pthr<-tkentry(tt,width="10",textvariable=textpthr)
#  tkgrid(tklabel(tt,text="Please enter the lower precipitation threshold pthr (>=0)"),
#         sticky="w",column=1,row=6) 
#  tkgrid(Entry.pthr,column=2,row=6)
#  
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
#    
#    PlevStr<-tclvalue(textPlev)
#    if(!as.numeric(PlevStr)%in%c(0.75,0.8,0.9,0.95,0.99,0.9999)){
#      GuiErrorMSG<-paste(GuiErrorMSG,"p.lev must be one of these: 0.75,0.80,0.90,0.95,0.99,0.9999. Please re-enter\n")
#      oflg<-0
#    }
#    
#    AdjStr<-tclvalue(textAdj)
#    if(!as.numeric(AdjStr)%in%c(0:10000)){
#      GuiErrorMSG<-paste(GuiErrorMSG,"Integer Iadj must be between 0 and 10000 inclusive, please re-enter\n")
#      oflg<-0
#    }
#    
#    Mq0Str<-tclvalue(textMq0)
#    if(!as.numeric(Mq0Str)%in%c(0:100)){
#      GuiErrorMSG<-paste(GuiErrorMSG,"Mq setting must be an integer between 0 and 100, please re-enter\n")
#      oflg<-0
#    }
#    
#    Ny4aStr<-tclvalue(textNy4a)
#    if(as.numeric(Ny4aStr)!=as.integer(Ny4aStr)){
#      GuiErrorMSG<-paste(GuiErrorMSG,"Ny4a must be an integer, please re-enter\n")
#      oflg<-0
#    }
#    
#    pthrstr<-tclvalue(textpthr)
#    if(as.numeric(pthrstr)!=as.numeric(pthrstr)){
#      GuiErrorMSG<-paste(GuiErrorMSG,"pthr must be an real number, please re-enter\n")
#      oflg<-0
#    }
#    
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
#      assign("pthrstr",pthrstr,envir=.GlobalEnv)
#      
#      tkinsert(txt,"end",paste("MissingValueCode set to:",MissingStr,"..\n",sep=" "))
#      tkinsert(txt,"end",paste("The nominal level p.lev = ",PlevStr,"..\n",sep=" "))
#      tkinsert(txt,"end",paste("Current Iadj value is",AdjStr,"..\n",sep=" "))
#      tkinsert(txt,"end",paste("Current Mq value is",Mq0Str,"..\n",sep=" "))
#      tkinsert(txt,"end",paste("Current Ny4a value is",Ny4aStr,"..\n",sep=" "))
#      tkinsert(txt,"end",paste("Current pthr value is",pthrstr,"..\n",sep=" "))
#      
#      b20<-"                    "
#      olen<-40
#      otmp<-paste(b20,b20,sep="")
#      substr(otmp,olen+1-nchar(MissingStr),olen)<-MissingStr
#      omiss<-otmp
#      tkgrid(tklabel(frameMiddle,text=omiss,width=40),column=2,row=1,sticky="e")
#      
#      otmp<-paste(b20,b20,sep="")
#      substr(otmp,olen+1-nchar(PlevStr),olen)<-PlevStr
#      oplev<-otmp
#      tkgrid(tklabel(frameMiddle,text=oplev,width=40),column=2,row=2,sticky="e")
#      
#      otmp<-paste(b20,b20,sep="")
#      substr(otmp,olen+1-nchar(AdjStr),olen)<-AdjStr
#      oadj<-otmp
#      tkgrid(tklabel(frameMiddle,text=oadj,width=40),column=2,row=3,sticky="e")
#      
#      otmp<-paste(b20,b20,sep="")
#      substr(otmp,olen+1-nchar(Mq0Str),olen)<-Mq0Str
#      omq0<-otmp
#      tkgrid(tklabel(frameMiddle,text=omq0,width=40),column=2,row=4,sticky="e")
#      
#      otmp<-paste(b20,b20,sep="")
#      substr(otmp,olen+1-nchar(Ny4aStr),olen)<-Ny4aStr
#      ony0<-otmp
#      tkgrid(tklabel(frameMiddle,text=ony0,width=40),column=2,row=5,sticky="e")
#      
#      otmp<-paste(b20,b20,sep="")
#      substr(otmp,olen+1-nchar(pthrstr),olen)<-pthrstr
#      onp0<-otmp
#      tkgrid(tklabel(frameMiddle,text=onp0,width=40),column=2,row=6,sticky="e")
#      
#      tkdestroy(tt)
#      tkfocus(main)
#      return()
#    }
#  }
#  
#  tkbind(Entry.Ny4a,"<Return>",OnOk1)
#  
#  Ok1.but<-tkbutton(tt,text="   OK   ",command=OnOk1)
#  tkbind(Entry.Missing,"<Return>",OnOk1)
#  tkgrid(Ok1.but,column=2,sticky="w",row=7)
#  #tkgrid(Ok1.but,column=1,sticky="e",row=7)
#  tkfocus(tt)
#}


#######################################################################################################################################################        

#StartGUI<-function(){
#  require(tcltk)

#GUI<-function(){
#  if(!exists("MissingStr")) MissingStr<-"-99.9"
#  if(!exists("PlevStr")) PlevStr<-"0.95"
#  if(!exists("AdjStr")) AdjStr<-"10000"
#  if(!exists("Mq0Str")) Mq0Str<-"100"
#  if(!exists("Ny4aStr")) Ny4aStr<-"10"
#  if(!exists("pthrstr")) pthrstr<-"0.0"
#  assign("MissingStr",MissingStr,envir=.GlobalEnv)
#  assign("PlevStr",PlevStr,envir=.GlobalEnv)
#  assign("AdjStr",AdjStr,envir=.GlobalEnv)
#  assign("Mq0Str",Mq0Str,envir=.GlobalEnv)
#  assign("Ny4aStr",Ny4aStr,envir=.GlobalEnv)
#  assign("pthrstr",pthrstr,envir=.GlobalEnv)
#  
#  main<-tktoplevel()
#  assign("main",main,envir=.GlobalEnv)
#  tkwm.title(main,"RHtests_dlyPrcp")
#  fontHeading<-tkfont.create(family="times",size=20,weight="bold",
#                             slant="italic")
#  fontLable<-tkfont.create(family="times",size=15,weight="bold")
#  
#  frameOverall<-tkframe(main)
#  frameUpper<-tkframe(frameOverall,relief="groove",borderwidth=2)
#  assign("frameUpper",frameUpper,env=.GlobalEnv)
#  frameMiddle<-tkframe(frameOverall,relief="groove",borderwidth=2)
#  assign("frameMiddle",frameMiddle,env=.GlobalEnv)
#  tkgrid(tklabel(frameUpper,text="RHtests for daily",font=fontHeading),row=1,column=1)
#  tkgrid(tklabel(frameUpper,text="precipitation data",font=fontHeading),row=1,column=2)
#  
#  ChgPara.but<-tkbutton(frameUpper,text="Change Pars",width=14,command=Chg.Para)
#  #Rfile.but<-tkbutton(frameUpper,text="Transform Data",width=14,command=Read.file)
#  FindU.but<-tkbutton(frameUpper,text= "FindU",width=14,command=OnFindU.dlyPrcp)
#  FindUD.but<-tkbutton(frameUpper,text="FindUD",width=14,command=OnFindUD.dlyPrcp)
#  StepSize.but<-tkbutton(frameUpper,text="StepSize",width=14,command=OnStepSize.dlyprcp)
#  Cancel.but<-tkbutton(frameUpper,text="Quit",width=14,command=OnQuit)
##   QMadjGaussian.wRef.but<-tkbutton(frameUpper,text= "QMadjGaussian.wRef",width=19,
##                            command=OnQMadjGaussian.wRef)
#  #FindUD.wRef.but<-tkbutton(frameUpper,text="FindUD.wRef",width=14,
#  #                          command=OnFindUD.wRef)
#  #StepSize.wRef.but<-tkbutton(frameUpper,text="StepSize.wRef",width=14,
#  #                            command=OnStepSize.wRef)
#  
#  adjDLYp.but<-tkbutton(frameUpper,text="QMadjDLYp",width=14,
#                         command=OnadjDLYp)
#  #adjDLYp.wRef.but<-tkbutton(frameUpper,text="adjDLYp.wRef",width=14,
#  #                      command=OnadjDLYp.wRef)
#  
#  olen<-40
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
#  otmp<-paste(b20,b20,sep="")
#  substr(otmp,olen+1-nchar(pthrstr),olen)<-pthrstr
#  opthr<-otmp
#    
#  
#  if(!exists("ofbody")) otmp<-paste(b20,"                  NA",sep="")
#  else{
#    if(nchar(ofbody)>olen) otmp<-paste("...",substr(ofbody,nchar(ofbody)-olen+4,nchar(ofbody)),sep="")
#    else{
#      otmp<-paste(b20,b20,sep="")
#      substr(otmp,olen+1-nchar(ofbody),olen)<-ofbody
#    }
#  }
#  oact<-otmp
##  if(!exists("ofref")) otmp<-paste(b20,"                  NA",sep="")
##  else{
##    if(nchar(ofref)>olen) otmp<-paste("...",substr(ofref,nchar(ofref)-olen+4,nchar(ofref)),sep="")
##    else{
##      otmp<-paste(b20,b20,sep="")
##      substr(otmp,olen+1-nchar(ofref),olen)<-ofref
##    }
##  }
##  oref<-otmp
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
#  # arrange menu buttons and labels
#  tkgrid(tklabel(frameUpper,text="          "),ChgPara.but,tklabel(frameUpper,text="          "),Cancel.but)
#  tkgrid(tklabel(frameUpper,text="PMF and F tests:",font=fontLable),sticky="w")
#  tkgrid(FindU.but,column=1,row=3)
#  tkgrid(FindUD.but,column=2,row=3)
#  tkgrid(StepSize.but,column=3,row=3)
#  #tkgrid(tklabel(frameUpper,text="PMT and t tests:",font=fontLable),sticky="w")
#  #tkgrid(Cancel.but,column=3,row=4)
#  #tkgrid(FindUD.wRef.but,column=2,row=4)
#  #tkgrid(StepSize.wRef.but,column=3,row=4)
#  #tkgrid(tklabel(frameUpper,text="  To adjust daily",font=fontLable),sticky="w")
#  #tkgrid(tklabel(frameUpper,text="Gaussian data:",font=fontLable),sticky="w",column=1,row=4)
#  
#  #tkgrid(adjDLYp.but,column=1,row=4)####jan21
#  #tkgrid(adjDLYp.wRef.but,column=3,row=4)
#  
#  tkgrid(tklabel(frameMiddle,text="Current Missing Value Code:"),
#         column=1,row=1,sticky="w") 
#  tkgrid(tklabel(frameMiddle,text=omiss,width=40),column=2,row=1,sticky="w")
#  
#  tkgrid(tklabel(frameMiddle,text="Current nominal level of confidence (p.lev):"),
#         column=1,row=2,sticky="w") 
#  tkgrid(tklabel(frameMiddle,text=oplev,width=40),column=2,row=2,sticky="e") 
#  
#  tkgrid(tklabel(frameMiddle,text="Segment to which to adjust the series (Iadj):"),
#         column=1,row=3,sticky="w")
#  tkgrid(tklabel(frameMiddle,text=oadj,width=40),column=2,row=3,sticky="e")
#  
#  tkgrid(tklabel(frameMiddle,text="Current Mq (# of points for evaluating PDF):"),
#         column=1,row=4,sticky="w")
#  tkgrid(tklabel(frameMiddle,text=omq0,width=40),column=2,row=4,sticky="e")
#  
#  tkgrid(tklabel(frameMiddle,text="Current Ny4a (max # of years of data for estimating PDF):"),
#         column=1,row=5,sticky="w")
#  tkgrid(tklabel(frameMiddle,text=oNy4a,width=40),column=2,row=5,sticky="e")
#  
#  tkgrid(tklabel(frameMiddle,text="Current pthr (Lower threshold of precipitation):"),
#         column=1,row=6,sticky="w")
#  tkgrid(tklabel(frameMiddle,text=opthr,width=40),column=2,row=6,sticky="e")
#  
#  tkgrid(tklabel(frameMiddle,text="Current input Base series filename:"),
#         column=1,row=7,sticky="w")
#  tkgrid(tklabel(frameMiddle,text=oact,width=40),column=2,row=7,sticky="w")
#  
#  tkgrid(tklabel(frameMiddle,text="Current data directory:    "),
#         column=1,row=8,sticky="w")
#  tkgrid(tklabel(frameMiddle,text=ocurdir,width=40),column=2,row=8,sticky="e")
#  
#  tkgrid(tklabel(frameMiddle,text="Current output directory:    "),
#         column=1,row=9,sticky="w")
#  tkgrid(tklabel(frameMiddle,text=ooutdir,width=40),column=2,row=9)
#  
#  # frameMiddle<-tkframe(frameOverall,relief="groove",borderwidth=2)
#  # assign("frameMiddle",frameMiddle,env=.GlobalEnv)
#  frameLower<-tkframe(frameOverall,relief="groove",borderwidth=2)
#  assign("xscr",tkscrollbar(frameLower,repeatinterval=5,orient="horizontal",
#                            command=function(...)tkxview(txt,...)),envir=.GlobalEnv)
#  assign("yscr",tkscrollbar(frameLower,repeatinterval=5,
#                            command=function(...)tkyview(txt,...)),envir=.GlobalEnv)
#  assign("txt",tktext(frameLower,bg="white",font="courier",
#                      xscrollcommand=function(...)tkset(xscr,...),
#                      yscrollcommand=function(...)tkset(yscr,...) 
#  ),envir=.GlobalEnv)
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
##tkwm.resizable(topFrame, 0, 0)
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

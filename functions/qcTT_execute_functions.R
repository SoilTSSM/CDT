
getElevationData1<-function(){
	single.series<-as.character(gal.params$use.method$Values[1])
	uselv<-as.character(gal.params$use.method$Values[3])
	interp.dem<-as.character(gal.params$use.method$Values[4])
	file.pars<-as.character(gal.params$file.io$Values)

	if(!is.null(EnvQcOutlierData$donnees1)){
		###get elevation data
		msg<-NULL
		elv_dem<-NULL
		if(single.series=="0"){
			lon<-EnvQcOutlierData$donnees1$lon
			lat<-EnvQcOutlierData$donnees1$lat
			elv_stn<-EnvQcOutlierData$donnees1$elv

			if(uselv=="1"){
				if(interp.dem=="0"){
					if(file.pars[3]=="") msg<-'There is no elevation data in format NetCDF'
					else{
						all.open.file<-as.character(unlist(lapply(1:length(file.opfiles),function(j) file.opfiles[[j]][[1]])))
						jncdf<-which(all.open.file==file.pars[3])
						fdem<-file.opfiles[[jncdf]][[2]]
						dem<-fdem$value
						dem[dem<0]<-NA
						dem<-data.frame(expand.grid(x=fdem$x,y=fdem$y),z=c(dem))
						dem<-dem[!is.na(dem$z),]
						xy0<-data.frame(x=lon,y=lat)
						elv_dem<-idw(formula = z ~ 1, locations= ~x+y,data=dem,newdata=xy0,nmax=4,debug.level=0)[,3]
					}
				}else{
					if(is.null(elv_stn)) msg<-'There is no elevation data in your file'
				}
			}
		}
	}else msg<-'No station data found'
	return(list(elv_stn,elv_dem,msg,file.pars[3]))
}

#####################################################

execQctempFun<-function(get.stn){
	status<-0
	msg<-NULL
	regparams<-as.character(gal.params$parameter[[1]]$Values)
	min.stn<-as.numeric(regparams[1])
	max.stn<-as.numeric(regparams[2])
	win<-as.numeric(regparams[3])
	win<-as.integer(win/2)
	thres<-as.numeric(regparams[4])
	R<-as.numeric(regparams[5])
	elv.diff<-as.numeric(regparams[6])

	test.tx<-as.character(gal.params$test.tx)
	single.series<-as.character(gal.params$use.method$Values[1])
	const.chk<-as.character(gal.params$use.method$Values[2])
	uselv<-as.character(gal.params$use.method$Values[3])
	interp.dem<-as.character(gal.params$use.method$Values[4])

	bounds<-gal.params$parameter[[2]]
	spregpar<-c(thres,max.stn)
	spthres<-c(min.stn,R,elv.diff)

	if(single.series=="0"){
		ijstn<-which(as.character(bounds[,1])==get.stn)
		limsup<-as.numeric(bounds[ijstn,2:3])

		if(uselv=='1'){
			if(is.null(EnvQcOutlierData$r_elv)){
				r_elv<-getElevationData1()
				assign('r_elv',r_elv,envir=EnvQcOutlierData)
			}else{
				if(interp.dem=="0" & (is.null(EnvQcOutlierData$r_elv[[2]]) | EnvQcOutlierData$r_elv[[4]]!=as.character(gal.params$file.io$Values[3]))){
					r_elv<-getElevationData1()
					EnvQcOutlierData$r_elv[2:4]<-r_elv[2:4]
				}else if(interp.dem=="1" & is.null(EnvQcOutlierData$r_elv[[1]])){
					elv_stn<-EnvQcOutlierData$donnees1$elv
					if(!is.null(elv_stn)){
						EnvQcOutlierData$r_elv[[1]]<-r_elv[[1]]<-elv_stn
						EnvQcOutlierData$r_elv[[3]]<-r_elv[[3]]<-NULL
					}else{
						EnvQcOutlierData$r_elv[[3]]<-r_elv[[3]]<-'There is no elevation data in your file'
					}
				}else r_elv<-EnvQcOutlierData$r_elv
			}

			if(!is.null(r_elv[[3]])){
				msg<-r_elv[[3]]
				status<-'no'
				elv<-NULL
				return(NULL)
			}else{
				if(interp.dem=="0") elv<-r_elv[[2]]
				else elv<-r_elv[[1]]
			}
		}else elv<-NULL

		coords<-cbind(EnvQcOutlierData$donnees1$lon,EnvQcOutlierData$donnees1$lat)

		if(const.chk=="1"){ #with c.check
			jstn1<-which(as.character(EnvQcOutlierData$donnees1$id)==get.stn)
			jstn2<-which(as.character(EnvQcOutlierData$donnees2$id)==get.stn)

			idstn<-as.character(EnvQcOutlierData$donnees1$id)[jstn1]
			donne<-EnvQcOutlierData$donnees1$data
			dates<-EnvQcOutlierData$donnees1$dates
			if(test.tx=="1"){
				TxData<-EnvQcOutlierData$donnees1$data[,jstn1]
				TxDate<-EnvQcOutlierData$donnees1$dates
				if(length(jstn2)>0){
					TnData<-EnvQcOutlierData$donnees2$data[,jstn2]
					TnDate<-EnvQcOutlierData$donnees2$dates
					int.check<-TRUE
				}else{
					TnData<-NULL
					TnDate<-NULL
					int.check<-FALSE
				}
				tx.test<-TRUE
			}else{
				TnData<-EnvQcOutlierData$donnees1$data[,jstn1]
				TnDate<-EnvQcOutlierData$donnees1$dates
				if(length(jstn2)>0){
					TxData<-EnvQcOutlierData$donnees2$data[,jstn2]
					TxDate<-EnvQcOutlierData$donnees2$dates
					int.check<-TRUE
				}else{
					TxData<-NULL
					TxDate<-NULL
					int.check<-FALSE
				}
				tx.test<-FALSE
			}
			testpars<-list(tx.test=tx.test,int.check=int.check,omit=FALSE,period=gal.params$period)
			xparams<-list(coords=coords,elv=elv,win=win,thres=thres,spthres=spthres,spregpar=spregpar,limsup=limsup)
			Tsdata<-list(data=donne,date=dates,tx.data=TxData,tx.date=TxDate,tn.data=TnData,tn.date=TnDate)
			res.qc.txtn<-txtnQcSpatialCheck(jstn1,idstn,Tsdata,xparams=xparams,testpars=testpars)
		}else{
			jstn<-which(as.character(EnvQcOutlierData$donnees1$id)==get.stn)
			idstn<-as.character(EnvQcOutlierData$donnees1$id)[jstn]
			if(test.tx=="1"){
				ddonne<-donne<-EnvQcOutlierData$donnees1$data
				ddates<-dates<-EnvQcOutlierData$donnees1$dates
				donne1<-dates1<-NULL
				tx.test<-TRUE
			}else{
				donne<-dates<-NULL
				ddonne<-donne1<-EnvQcOutlierData$donnees1$data
				ddates<-dates1<-EnvQcOutlierData$donnees1$dates
				tx.test<-FALSE
			}
			testpars<-list(tx.test=tx.test,int.check=FALSE,omit=TRUE,period=gal.params$period)
			xparams<-list(coords=coords,elv=elv,win=win,thres=thres,spthres=spthres,spregpar=spregpar,limsup=limsup)
			Tsdata<-list(data=ddonne,date=ddates,tx.data=donne[,jstn],tx.date=dates,tn.data=donne1[,jstn],tn.date=dates1)
			res.qc.txtn<-txtnQcSpatialCheck(jstn,idstn,Tsdata,xparams=xparams,testpars=testpars)
		}
	}else{ #a single series
		idstn<-as.character(bounds[1,1])
		limsup<-as.numeric(bounds[1,2:3])
		if(const.chk=="1"){ #with c.check
			if(EnvQcOutlierData$donnees1$nbvar==3){
				TxData<-EnvQcOutlierData$donnees1$var$tx
				TnData<-EnvQcOutlierData$donnees1$var$tn
				TnDate<-TxDate<-EnvQcOutlierData$donnees1$dates
				tx.test<-if(test.tx=="1") TRUE else FALSE
			}else{
				if(test.tx=="1"){
					TxData<-EnvQcOutlierData$donnees1$var$var
					TxDate<-EnvQcOutlierData$donnees1$dates
					TnData<-EnvQcOutlierData$donnees2$var$var
					TnDate<-EnvQcOutlierData$donnees2$dates
					tx.test<-TRUE
				}else{
					TxData<-EnvQcOutlierData$donnees2$var$var
					TxDate<-EnvQcOutlierData$donnees2$dates
					TnData<-EnvQcOutlierData$donnees1$var$var
					TnDate<-EnvQcOutlierData$donnees1$dates
					tx.test<-FALSE
				}
			}
			Tsdata<-list(tx.data=TxData,tx.date=TxDate,tn.data=TnData,tn.date=TnDate)
			xparams<-list(thres=thres,limsup=limsup)
			testpars<-list(tx.test=tx.test,int.check=TRUE,period=gal.params$period)
			res.qc.txtn<-txtnQcSingleSeries(idstn,Tsdata,xparams=xparams,testpars=testpars)
		}else{
			if(test.tx=="1"){
				tx.test<-TRUE
				if(EnvQcOutlierData$donnees1$nbvar==3) TxData<-EnvQcOutlierData$donnees1$var$tx
				else TxData<-EnvQcOutlierData$donnees1$var$var
				TxDate<-EnvQcOutlierData$donnees1$dates
				TnData<-TnDate<-NULL
			}else{
				tx.test<-FALSE
				TxData<-TxDate<-NULL
				if(EnvQcOutlierData$donnees1$nbvar==3) TnData<-EnvQcOutlierData$donnees1$var$tn
				else TnData<-EnvQcOutlierData$donnees1$var$var
				TnDate<-EnvQcOutlierData$donnees1$dates
			}
			Tsdata<-list(tx.data=TxData,tx.date=TxDate,tn.data=TnDate,tn.date=TnDate)
			xparams<-list(thres=thres,limsup=limsup)
			testpars<-list(tx.test=tx.test,int.check=FALSE,period=gal.params$period)
			res.qc.txtn<-txtnQcSingleSeries(idstn,Tsdata,xparams=xparams,testpars=testpars)
		}
	}
	on.exit({
		if(status=='ok') insert.txt(main.txt.out,msg)
		if(status=='no') insert.txt(main.txt.out,msg,format=TRUE)
	})
	return(res.qc.txtn)
}

#####################################################

ExecQcTemp<-function(get.stn){
	ExecTempOneStation<-function(jlstn){
		status<-0
		msg<-NULL
		dates<-EnvQcOutlierData$donnees1$dates
		if(as.character(gal.params$use.method$Values[1])=="0"){
			xpos<-which(as.character(EnvQcOutlierData$donnees1$id)==jlstn)
			xdat<-EnvQcOutlierData$donnees1$data[,xpos]
		}else{
			if(EnvQcOutlierData$donnees1$nbvar==3){
				if(as.character(gal.params$test.tx)=='1') xdat<-EnvQcOutlierData$donnees1$var$tx
				else xdat<-EnvQcOutlierData$donnees1$var$tn
			}else xdat<-EnvQcOutlierData$donnees1$var$var
		}

		outsdir<-file.path(EnvQcOutlierData$baseDir,'Outputs',jlstn,fsep = .Platform$file.sep)
		# ##create by default
		corrdirstn<-file.path(EnvQcOutlierData$baseDir,'CorrectedData',jlstn,fsep = .Platform$file.sep)
		if(!file.exists(corrdirstn)) dir.create(corrdirstn,showWarnings=FALSE,recursive=TRUE)
		file_corrected<-file.path(corrdirstn,paste(jlstn,'.txt',sep=''),fsep = .Platform$file.sep)

		qcout<-try(execQctempFun(jlstn), silent=TRUE)
		if(!inherits(qcout, "try-error")){
			if(!is.null(qcout)){
				if(!file.exists(outsdir)) dir.create(outsdir,showWarnings=FALSE,recursive=TRUE)
				fileoutRdata<-file.path(outsdir,paste(jlstn,'.Rdata',sep=''),fsep = .Platform$file.sep)
				fileoutTXT<-file.path(outsdir,paste(jlstn,'.txt',sep=''),fsep = .Platform$file.sep)
				##
				dirPlot<-file.path(outsdir,paste(jlstn,'OUTLIERS_PLOT',sep='_'),fsep = .Platform$file.sep)
				if(!file.exists(dirPlot)) dir.create(dirPlot,showWarnings=FALSE,recursive=TRUE)
				##
				if(gal.params$AllOrOne=='all') ret.qcout<-qcout
				ret.res<-list(action=gal.params$action,period=gal.params$period,station=jlstn,res=qcout,outputdir=outsdir,AllOrOne=gal.params$AllOrOne)
				save(ret.res,file=fileoutRdata)

				## Default: not replace outliers if less/greater than limsup
				lenNoRepl<-rep(NA,nrow(qcout))
				resqc<-as.numeric(qcout$values)
				limsup<-as.numeric(gal.params$parameter[[2]][as.character(gal.params$parameter[[2]][,1])==jlstn,2:3])
				lenNoRepl[resqc>limsup[1] & resqc<limsup[2]]<-1

				qcout<-data.frame(qcout,not.replace=lenNoRepl,change.values=NA)
				write.table(qcout,fileoutTXT,col.names=TRUE,row.names=FALSE)
				##
				for(jmo in 1:12){
					jmo<-ifelse(jmo<10,paste('0',jmo,sep=''),as.character(jmo))
					flplot<-file.path(dirPlot,paste(format(ISOdate(2014,jmo,1),"%B"),'.jpg',sep=''),fsep = .Platform$file.sep)
					if (Sys.info()["sysname"] == "Windows") jpeg(filename=flplot,width=960,height=620,quality=100,restoreConsole=FALSE)
					else jpeg(filename=flplot,width=960,height=620,quality=100)
					plotOutliers0(jmo,qcout,dates,xdat)
					dev.off()
				}

				##Default: replace by NA (uncomment line below)
				
				xdat[match(qcout$dates[is.na(lenNoRepl)],dates)]<-NA
				msg<-paste("Quality control finished successfully for", jlstn)
				status<-'ok'
			}else{
				if(gal.params$AllOrOne=='one') ret.res<-NULL
				if(gal.params$AllOrOne=='all') ret.qcout<-NULL
				msg<-paste("Quality control failed for", jlstn)
				status<-'no'
			}
		}else{
			if(gal.params$AllOrOne=='one') ret.res<-NULL
			if(gal.params$AllOrOne=='all') ret.qcout<-NULL
			msg<-paste(paste("Quality control failed for", jlstn),'\n',gsub('[\r\n]','',qcout[1]),sep='')
			status<-'no'
		}
		if(gal.params$AllOrOne=='all'){
			tcl("update")
			ret.res<-list(ret.qcout,jlstn,outsdir)
		}

		# ##create by default
		sdon<-data.frame(dates,xdat)
		write.table(sdon,file_corrected,col.names=FALSE,row.names=FALSE)
		on.exit({
			if(status=='ok') insert.txt(main.txt.out,msg)
			if(status=='no') insert.txt(main.txt.out,msg,format=TRUE)
		})
		return(ret.res)
	}

	#############
	ExecTempALLStations<-function(){
		allstn2loop<-as.character(gal.params$parameter[[2]][,1])

		tcl("update","idletasks")
		##Use parLapply
		retAllRun<-lapply(allstn2loop,function(jlstn) ExecTempOneStation(jlstn))

		ret.qcout<-lapply(retAllRun,function(x) x[[1]])
		ret.stnid<-lapply(retAllRun,function(x) x[[2]])
		ret.outdir<-lapply(retAllRun,function(x) x[[3]])

		ret.res<-list(action=gal.params$action,period=gal.params$period,station=ret.stnid,res=ret.qcout,outputdir=ret.outdir,AllOrOne=gal.params$AllOrOne)
		return(ret.res)
	}

	#############
	if(gal.params$AllOrOne=='one') ret.res<-ExecTempOneStation(get.stn)
	if(gal.params$AllOrOne=='all') ret.res<-ExecTempALLStations()
	return(ret.res)
}

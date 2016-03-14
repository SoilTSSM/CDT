
AggregateQcData<-function(){
	outdirs<-as.character(gal.params$file.io)
	datfin<-file.path(outdirs,'AggregateData',fsep = .Platform$file.sep)
	if(!file.exists(datfin)) dir.create(datfin,showWarnings=FALSE,recursive=TRUE)

	outputdir<-file.path(outdirs,'Outputs',fsep = .Platform$file.sep)
	outptID<-list.files(outputdir)
	chkdir<-file.path(outdirs,'CorrectedData',fsep = .Platform$file.sep)
	corrctID<-list.files(chkdir)

	load(file.path(outdirs,'OriginalData','Parameters.RData',fsep = .Platform$file.sep))
	if(paramsGAL$inputPars$action=="qc.rain"){	
		infohead<-cbind(paramsGAL$data$id,paramsGAL$data$lon,paramsGAL$data$lat,paramsGAL$data$elv)
		stnID<-as.character(paramsGAL$data$id)
	}
	if(paramsGAL$inputPars$action=="qc.temp"){
		infohead<-cbind(paramsGAL$data[[1]]$id,paramsGAL$data[[1]]$lon,paramsGAL$data[[1]]$lat,paramsGAL$data[[1]]$elv)
		stnID<-as.character(paramsGAL$data[[1]]$id)
	}
	existStn<-stnID%in%corrctID
	noChck<-!stnID%in%outptID
	stnID<-stnID[existStn]
	noTraiteStn<-infohead[!existStn,]
	noQcChkStn<-infohead[noChck,]
	infohead<-infohead[existStn,]
	if(length(stnID)==0){
		insert.txt(main.txt.out,'No checked stations found or Wrong directory',format=TRUE)
		return(NULL)
	}

	if(nrow(noTraiteStn)>0 | nrow(noQcChkStn)>0){
		if(nrow(noQcChkStn)>0) faileds0<-list('Stations not Checked',noQcChkStn)
		else faileds0<-NULL
		if(nrow(noTraiteStn)>0) faileds1<-list('Stations not aggregated',noTraiteStn)
		else faileds1<-NULL
		faileds<-list(faileds0,faileds1)
		containertab<-displayConsOutputTabs(tknotes,faileds,title='Failed Stations')
		ntab<-length(tab.type)
		tab.type[[ntab+1]]<<-'ctxt'
		tab.data[[ntab+1]]<<-containertab
		tkselect(tknotes,ntab)
	}

	stn.don<-read.table(file.path(chkdir,stnID[1],paste(stnID[1],'.txt',sep=''),fsep = .Platform$file.sep))
	aggData<-stn.don[,1]

	for(jj in stnID){
		filein<-file.path(chkdir,jj,paste(jj,'.txt',sep=''),fsep = .Platform$file.sep)
		stn.don<-read.table(filein)
		aggData<-cbind(aggData,stn.don[,2])
	}

	period<-paramsGAL$inputPars$period
	if(period=='daily') pdate<-'DAILY'
	if(period=='dekadal') pdate<-'DEKADAL'
	if(period=='Monthly') pdate<-'MONTHLY'

	if(ncol(infohead)==3) capition<-c('Stations','LON',paste(pdate,'LAT',sep='/'))
	else capition<-c('Stations','LON','LAT',paste(pdate,'ELV',sep='/'))
	infohead<-cbind(capition,t(infohead))
	aggData<-t(cbind(t(infohead),t(aggData)))
	if(paramsGAL$inputPars$action=="qc.rain") aggData[is.na(aggData)]<-paramsGAL$dataPars[[2]]$miss.val
	if(paramsGAL$inputPars$action=="qc.temp") aggData[is.na(aggData)]<-paramsGAL$dataPars[[1]][[2]]$miss.val

	fileout<-file.path(datfin,paste('Checked',paramsGAL$inputPars$file.io$Values[1],sep='_'),fsep = .Platform$file.sep)
	write.table(aggData,fileout,col.names=F,row.names=F,quote=F)
	return(0)	
}

###################################################

AggregateHomData0<-function(){
	outdirs<-as.character(gal.params$file.io)
	datfin<-file.path(outdirs,'AggregateData',fsep = .Platform$file.sep)
	if(!file.exists(datfin)) dir.create(datfin,showWarnings=FALSE,recursive=TRUE)

	chkdir<-file.path(outdirs,'AdjustedData',fsep = .Platform$file.sep)

	load(file.path(outdirs,'OriginalData','Parameters.RData',fsep = .Platform$file.sep))
	infohead0<-cbind(paramsGAL$data[[1]]$id,paramsGAL$data[[1]]$lon,paramsGAL$data[[1]]$lat,paramsGAL$data[[1]]$elv)
	StnId<-as.character(paramsGAL$data[[1]]$id)
	ggid<-list.files(chkdir)
	if(length(ggid)==0){
		insert.txt(main.txt.out,'No tested stations found or Wrong directory',format=TRUE)
		return(NULL)
	}

	suffix<-c('DLY','DEK','MON')
	prefix<-c('DAILY','DEKADAL','MONTHLY')

	for(xfl in 1:3){
		if(is.null(paramsGAL$data1[[xfl]]))	next	
		dates<-paramsGAL$data1[[xfl]][[1]]$date
		donne1<-round(paramsGAL$data1[[xfl]][[1]]$data,1)
		donne3<-donne2<-donne1
		for(j in 1:length(ggid)){
			xpos<-StnId%in%ggid[j]
			filein2<-file.path(chkdir,ggid[j],fsep = .Platform$file.sep)
			fileChoix<-file.path(filein2,paste(ggid[j],'_CHOICE.txt',sep=''),fsep = .Platform$file.sep)
			filein2<-file.path(filein2,paste(ggid[j],'_',suffix[xfl],'.txt',sep=''),fsep = .Platform$file.sep)
			dat<-read.table(filein2)
			chx<-read.table(fileChoix)
			idx<-chx[,1]
			donne1[,xpos]<-dat[,3]
			donne2[,xpos]<-dat[,4]
			if(idx==1){
				tschx<-dat[,3]
			}else if(idx==2){
				tschx<-dat[,4]
			}else if(idx==3){
				tschx<-dat[,2]
			}else tschx<-dat[,2]
			donne3[,xpos]<-tschx
		}

		if(ncol(infohead0)==3) capition<-c('Stations','LON',paste(prefix[xfl],'LAT',sep='/'))
		if(ncol(infohead0)==4) capition<-c('Stations','LON','LAT',paste(prefix[xfl],'ELV',sep='/'))
		infohead<-t(cbind(capition,t(infohead0)))

		donne1<-t(cbind(infohead,t(cbind(dates,donne1))))
		donne2<-t(cbind(infohead,t(cbind(dates,donne2))))
		donne3<-t(cbind(infohead,t(cbind(dates,donne3))))
		donne1[is.na(donne1)]<- paramsGAL$dataPars[[1]][[2]]$miss.val
		donne2[is.na(donne2)]<- paramsGAL$dataPars[[1]][[2]]$miss.val
		donne3[is.na(donne3)]<- paramsGAL$dataPars[[1]][[2]]$miss.val
		write.table(donne1,file.path(datfin,paste('AdjMean_',suffix[xfl],'_',paramsGAL$inputPars$file.io$Values[1],sep=''),fsep = .Platform$file.sep),col.names=F,row.names=F,quote=F)
		write.table(donne2,file.path(datfin,paste('AdjQM_',suffix[xfl],'_',paramsGAL$inputPars$file.io$Values[1],sep=''),fsep = .Platform$file.sep),col.names=F,row.names=F,quote=F)
		write.table(donne3,file.path(datfin,paste('Combined-Adj_',suffix[xfl],'_',paramsGAL$inputPars$file.io$Values[1],sep=''),fsep = .Platform$file.sep),col.names=F,row.names=F,quote=F)
	}

	noTraiteStn<-StnId[!StnId%in%ggid]
	if(length(noTraiteStn)>0){
		faileds<-list('Not Tested Stations',noTraiteStn)
		containertab<-displayConsOutputTabs(tknotes,faileds,title='Not Tested Stations')
		ntab<-length(tab.type)
		tab.type[[ntab+1]]<<-'ctxt'
		tab.data[[ntab+1]]<<-containertab
		tkselect(tknotes,ntab)
	}
	return(0)
}

###################################################

AggregateHomData<-function(){
	outdirs<-as.character(gal.params$file.io)
	datfin<-file.path(outdirs,'AggregateData',fsep = .Platform$file.sep)
	if(!file.exists(datfin)) dir.create(datfin,showWarnings=FALSE,recursive=TRUE)

	chkdir<-file.path(outdirs,'AdjustedData',fsep = .Platform$file.sep)
	orgdir<-file.path(outdirs,'Data',fsep = .Platform$file.sep)

	xfile<-list.files(orgdir)
	info<-grep('Infos_',xfile)
	pars<-grep('RefSeries_Data',xfile)

	filein<-file.path(orgdir,xfile[info],fsep = .Platform$file.sep)
	gginfo<-read.table(filein,header=T)
	ggid<-as.character(gginfo$IDs)
	ggelv<-gginfo$Elv
	if(sum(!is.na(ggelv))==0){
		infohead<-gginfo[,1:3]
		infohead[,1]<-ggid
		infohead<-t(infohead)
	}else{
		infohead<-gginfo
		infohead[,1]<-ggid
		infohead<-t(infohead)
	}

	fdonne<-xfile[-c(info,pars)]
	for(xfl in fdonne){
		period<-strsplit(xfl,'_')[[1]][1]
		filein1<-file.path(orgdir,xfl,fsep = .Platform$file.sep)
		ggdates<-read.table(filein1,header=F)
		ggdates3<-ggdates2<-ggdates1<-ggdates[,1]
		miss<-NULL
		for(j in 1:length(ggid)){
			filein2<-file.path(chkdir,ggid[j],fsep = .Platform$file.sep)
			fileChoix<-file.path(filein2,paste(ggid[j],'_CHOICE.txt',sep=''),fsep = .Platform$file.sep)
			if(period=='DAILY') filein2<-file.path(filein2,paste(ggid[j],'_DLY.txt',sep=''),fsep = .Platform$file.sep)
			if(period=='DEKADAL') filein2<-file.path(filein2,paste(ggid[j],'_DEK.txt',sep=''),fsep = .Platform$file.sep)
			if(period=='MONTHLY') filein2<-file.path(filein2,paste(ggid[j],'_MON.txt',sep=''),fsep = .Platform$file.sep)
			if(file.exists(filein2)){
				dat<-read.table(filein2)
				chx<-read.table(fileChoix)
				idx<-chx[,1]
				ggdates1<-cbind(ggdates1,dat[,3])
				ggdates2<-cbind(ggdates2,dat[,4])
				if(idx==1){
					tschx<-dat[,3]
				}else if(idx==2){
					tschx<-dat[,4]
				}else if(idx==3){
					tschx<-dat[,2]
				}else tschx<-dat[,2]
				ggdates3<-cbind(ggdates3,tschx)
			}else{
				miss<-c(miss,j)
			}
		}

		if(!is.null(miss)) infohead1<-infohead[,- miss]
		if(nrow(infohead)==3) capition<-c('Stations','LON',paste(period,'LAT',sep='/'))
		if(nrow(infohead)==4) capition<-c('Stations','LON','LAT',paste(period,'ELV',sep='/'))
		infohead2<-cbind(capition,infohead1)
		donne1<-rbind(infohead2,ggdates1)
		donne2<-rbind(infohead2,ggdates2)
		donne3<-rbind(infohead2,ggdates3)
		donne1[is.na(donne1)]<- -99
		donne2[is.na(donne2)]<- -99
		donne3[is.na(donne3)]<- -99
		write.table(donne1,file.path(datfin,paste('AdjMean',xfl,sep='-'),fsep = .Platform$file.sep),col.names=F,row.names=F,quote=F)
		write.table(donne2,file.path(datfin,paste('AdjQM',xfl,sep='-'),fsep = .Platform$file.sep),col.names=F,row.names=F,quote=F)
		write.table(donne3,file.path(datfin,paste('Combined-Adj',xfl,sep='-'),fsep = .Platform$file.sep),col.names=F,row.names=F,quote=F)
	}
	return(0)
}

###################################################################
AggregateDataCDT<-function(gal.params){
	istart.yrs<-as.numeric(as.character(gal.params$StartEnd.date$Values[1]))
	istart.mon<-as.numeric(as.character(gal.params$StartEnd.date$Values[2]))
	istart.day<-as.numeric(as.character(gal.params$StartEnd.date$Values[3]))
	iend.yrs<-as.numeric(as.character(gal.params$StartEnd.date$Values[4]))
	iend.mon<-as.numeric(as.character(gal.params$StartEnd.date$Values[5]))
	iend.day<-as.numeric(as.character(gal.params$StartEnd.date$Values[6]))

	period<-as.character(gal.params$period)
	filefrmt<-as.character(gal.params$file.date.format$Values[1])
	datefrmt<-as.character(gal.params$file.date.format$Values[2])

	file.pars<-as.character(gal.params$file.io$Values)
	all.open.file<-as.character(unlist(lapply(1:length(file.opfiles),function(j) file.opfiles[[j]][[1]])))
	jfile<-which(all.open.file==file.pars[1])
	extf<-file_ext(file.opfiles[[jfile]][[3]])
	delimter<-file.opfiles[[jfile]][[4]]
	f.name<-if(delimter$sepr=="") 'read.table' else 'read.csv'
	readFun<-match.fun(f.name)

	jfile1<-which(all.open.file==file.pars[2])
	gginfo<-file.opfiles[[jfile1]][[2]]
	ncol<-ncol(gginfo)
	ggid<-as.character(gginfo[,1])
	if(ncol==4){
		ggelv<-as.numeric(as.character(gginfo[,4]))
		ggelv<-ifelse(ggelv<0 | ggelv>10000,NA,ggelv)
		if(sum(!is.na(ggelv))==0){
			infohead<-t(gginfo[,1:3])
		}else{
			infohead<-t(gginfo)
		}
	}else{
		infohead<-t(gginfo)
	}
	infohead1<-t(infohead)

	if(period=='daily'){
		istart<-as.Date(paste(istart.yrs,istart.mon,istart.day,sep='-'))
		iend<-as.Date(paste(iend.yrs,iend.mon,iend.day,sep='-'))
		odates<-data.frame(dates=format(seq(istart,iend,'day'),'%Y%m%d'))
	}else if(period=='dekadal'){
		istart<-as.Date(paste(istart.yrs,istart.mon,istart.day,sep='-'))
		iend<-as.Date(paste(iend.yrs,iend.mon,iend.day,sep='-'))
		odates<-seq(istart,iend,'day')
		odates<-paste(format(odates[which(as.numeric(format(odates,'%d'))<=3)],'%Y%m'),
		as.numeric(format(odates[which(as.numeric(format(odates,'%d'))<=3)],'%d')),sep='')
		odates<-data.frame(dates=odates)
	}else if(period=='monthly'){
		istart<-as.Date(paste(istart.yrs,istart.mon,1,sep='-'))
		iend<-as.Date(paste(iend.yrs,iend.mon,1,sep='-'))
		odates<-data.frame(dates=format(seq(istart,iend,'month'),'%Y%m'))
	}

	retval<-odates
	if(filefrmt=="0"){
		retval1<-odates
		retval2<-odates
	}
	miss<-NULL
	for(j in 1:length(ggid)){
		fileopen<-file.path(file.pars[3],paste(ggid[j],'.',extf,sep=''),fsep = .Platform$file.sep)
		if(file.exists(fileopen)){
			is.rdble <- !inherits(try(donne<- readFun(fileopen,header=delimter$header,sep=delimter$sepr,skip=delimter$skip-1,
			na.strings=delimter$miss.val,quote="\"'",strip.white=TRUE,colClasses = "character"), silent=TRUE), "try-error")
			if(!is.rdble){
				insert.txt(main.txt.out,paste("Unable to read file ",fileopen),format=TRUE)
				miss<-c(miss,j)
			}else{
				if(period=='daily'){
					if(filefrmt=="1"){ #1var
						if(datefrmt=="1"){ #1date
							dates<-as.Date(as.character(donne[,1]),format='%Y%m%d')
							var<-as.numeric(donne[,2])
						}else{ #3date
							dates<-as.Date(paste(as.character(donne[,1]),as.character(donne[,2]),as.character(donne[,3]),sep='-'))
							var<-as.numeric(donne[,4])
						}
					}else{#3var
						if(datefrmt=="1"){ #1date
							dates<-as.Date(as.character(donne[,1]),format='%Y%m%d')
							var<-as.numeric(donne[,2])   #rr=2, tx=3, tn=4
							var1<-as.numeric(donne[,3])
							var2<-as.numeric(donne[,4])
						}else{#3date
							dates<-as.Date(paste(as.character(donne[,1]),as.character(donne[,2]),as.character(donne[,3]),sep='-'))
							var<-as.numeric(donne[,4]) #rr=4, tx=5, tn=6
							var1<-as.numeric(donne[,5])
							var2<-as.numeric(donne[,6])
						}
					}
					var<-var[!is.na(dates)]
					dates<-dates[!is.na(dates)]
					var<-var[order(dates)]
					if(filefrmt=="0"){
						var1<-var1[order(dates)]
						var2<-var2[order(dates)]
					}
					dates<-dates[order(dates)]
					dates<-format(dates,'%Y%m%d')
				}else if(period=='dekadal'){
					if(filefrmt=="1"){ #1var
						if(datefrmt=="1"){ #1date
							xan<-substr(as.character(donne[,1]),1,4)
							xmo<-substr(as.character(donne[,1]),5,6)
							xdk<-substr(as.character(donne[,1]),7,7)
							notdek<-which(as.numeric(xdk)>3)
							dates<-as.Date(paste(xan,xmo,xdk,sep='-'))
							dates[notdek]<-NA
							var<-as.numeric(donne[,2])
						}else{ #3date
							dates<-as.Date(paste(as.character(donne[,1]),as.character(donne[,2]),as.character(donne[,3]),sep='-'))
							notdek<-which(as.numeric(as.character(donne[,3]))>3)
							dates[notdek]<-NA
							var<-as.numeric(donne[,4])
						}
					}else{#3var
						if(datefrmt=="1"){ #1date
							xan<-substr(as.character(donne[,1]),1,4)
							xmo<-substr(as.character(donne[,1]),5,6)
							xdk<-substr(as.character(donne[,1]),7,7)
							notdek<-which(as.numeric(xdk)>3)
							dates<-as.Date(paste(xan,xmo,xdk,sep='-'))
							dates[notdek]<-NA
							var<-as.numeric(donne[,2])   #rr=2, tx=3, tn=4
							var1<-as.numeric(donne[,3])
							var2<-as.numeric(donne[,4])
						}else{#3date
							dates<-as.Date(paste(as.character(donne[,1]),as.character(donne[,2]),as.character(donne[,3]),sep='-'))
							notdek<-which(as.numeric(as.character(donne[,3]))>3)
							dates[notdek]<-NA
							var<-as.numeric(donne[,4]) #rr=4, tx=5, tn=6
							var1<-as.numeric(donne[,5])
							var2<-as.numeric(donne[,6])
						}
					}
					var<-var[!is.na(dates)]
					dates<-dates[!is.na(dates)]
					var<-var[order(dates)]
					if(filefrmt=="0"){
						var1<-var1[order(dates)]
						var2<-var2[order(dates)]
					}
					dates<-dates[order(dates)]
					dates<-paste(format(dates,'%Y%m'),as.numeric(format(dates,'%d')),sep='')
				}else if(period=='monthly'){
					if(filefrmt=="1"){ #1var
						if(datefrmt=="1"){ #1date
							xan<-substr(as.character(donne[,1]),1,4)
							xmo<-substr(as.character(donne[,1]),5,6)
							dates<-as.Date(paste(xan,xmo,'1',sep='-'))
							var<-as.numeric(donne[,2])
						}else{ #3date
							dates<-as.Date(paste(as.character(donne[,1]),as.character(donne[,2]),'1',sep='-'))
							if(ncol(donne)==3) var<-as.numeric(donne[,3])
							if(ncol(donne)==4) var<-as.numeric(donne[,4])
						}
					}else{#3var
						if(datefrmt=="1"){ #1date
							xan<-substr(as.character(donne[,1]),1,4)
							xmo<-substr(as.character(donne[,1]),5,6)
							dates<-as.Date(paste(xan,xmo,'1',sep='-'))
							var<-as.numeric(donne[,2])   #rr=2, tx=3, tn=4
							var1<-as.numeric(donne[,3])
							var2<-as.numeric(donne[,4])
						}else{#3date
							dates<-as.Date(paste(as.character(donne[,1]),as.character(donne[,2]),'1',sep='-'))
							if(ncol(donne)==5){
								var<-as.numeric(donne[,3]) #rr=3, tx=4, tn=5
								var1<-as.numeric(donne[,4])
								var2<-as.numeric(donne[,5])
							}
							if(ncol(donne)==6){
								var<-as.numeric(donne[,4]) #rr=4, tx=5, tn=6
								var1<-as.numeric(donne[,5])
								var2<-as.numeric(donne[,6])
							}
						}
					}
					var<-var[!is.na(dates)]
					dates<-dates[!is.na(dates)]
					var<-var[order(dates)]
					if(filefrmt=="0"){
						var1<-var1[order(dates)]
						var2<-var2[order(dates)]
					}
					dates<-dates[order(dates)]
					dates<-format(dates,'%Y%m')
				}
				val<-data.frame(dates=dates,val=var)
				if(filefrmt=="0"){
					val1<-data.frame(dates=dates,val=var1)
					val2<-data.frame(dates=dates,val=var2)
				}
				###############################
				rtmp<-merge(odates,val,by.x='dates',by.y='dates',all=T)
				ideb<-which(as.character(rtmp[,1])==as.character(odates[1,1]))
				ifin<-which(as.character(rtmp[,1])==as.character(odates[nrow(odates),1]))
				retval<-cbind(retval,rtmp[ideb:ifin,2])
				if(filefrmt=="0"){
					rtmp1<-merge(odates,val1,by.x='dates',by.y='dates',all=T)
					retval1<-cbind(retval1,rtmp1[ideb:ifin,2])
					rtmp2<-merge(odates,val2,by.x='dates',by.y='dates',all=T)
					retval2<-cbind(retval2,rtmp2[ideb:ifin,2])
				}

			}
		}else{
			miss<-c(miss,j)
		}
	}
	if(!is.null(miss)) infohead<-infohead[,- miss]
	if(nrow(infohead)==3) capition<-c('Stations','LON',paste(toupper(period),'LAT',sep='/'))
	if(nrow(infohead)==4) capition<-c('Stations','LON','LAT',paste(toupper(period),'ELV',sep='/'))
	infohead<-data.frame(capition,infohead)

	donne<-t(cbind(t(infohead),t(retval)))
	fileout<-file.pars[4]
	if(filefrmt=="0"){
		donne1<-t(cbind(t(infohead),t(retval1)))
		donne2<-t(cbind(t(infohead),t(retval2)))
		fileout<-file.path(dirname(file.pars[4]),paste('PRECIP_',basename(file.pars[4]),sep=''),fsep = .Platform$file.sep)
		fileout1<-file.path(dirname(file.pars[4]),paste('TMAX_',basename(file.pars[4]),sep=''),fsep = .Platform$file.sep)
		fileout2<-file.path(dirname(file.pars[4]),paste('TMIN_',basename(file.pars[4]),sep=''),fsep = .Platform$file.sep)
	}

	donne[is.na(donne)]<- -99
	write.table(donne,fileout,col.names=F,row.names=F,quote=F)

	if(filefrmt=="0"){
		write.table(donne1,fileout1,col.names=F,row.names=F,quote=F)
		write.table(donne2,fileout2,col.names=F,row.names=F,quote=F)
	}
	if(!is.null(miss)){
		faileds<-list('Stations not aggregated',infohead1[miss,])
		containertab<-displayConsOutputTabs(tknotes,faileds,title='Failed Stations')
		ntab<-length(tab.type)
		tab.type[[ntab+1]]<<-'ctxt'
		tab.data[[ntab+1]]<<-containertab
		tkselect(tknotes,ntab)
	}
	return(0)
}

###################################################################

AggregateZeroChkData<-function(){
	outdirs<-as.character(gal.params$file.io)
	datfin<-file.path(outdirs,'AggregateData',fsep = .Platform$file.sep)
	if(!file.exists(datfin)) dir.create(datfin,showWarnings=FALSE,recursive=TRUE)

	outputdir<-file.path(outdirs,'Outputs',fsep = .Platform$file.sep)
	outptID<-list.files(outputdir)
	zchkdir<-file.path(outdirs,'CorrectedData',fsep = .Platform$file.sep)
	corrctID<-list.files(zchkdir)

	load(file.path(outdirs,'OriginalData','Parameters.RData',fsep = .Platform$file.sep))
	infohead<-cbind(paramsGAL$data$id,paramsGAL$data$lon,paramsGAL$data$lat,paramsGAL$data$elv)
	stnID<-as.character(paramsGAL$data$id)

	existStn<-stnID%in%corrctID
	noChck<-!stnID%in%outptID
	stnID<-stnID[existStn]
	noTraiteStn<-infohead[!existStn,]
	noZeroChkStn<-infohead[noChck,]
	infohead<-infohead[existStn,]
	if(length(stnID)==0){
		insert.txt(main.txt.out,'No corrected stations found or Wrong directory',format=TRUE)
		return(NULL)
	}

	if(nrow(noTraiteStn)>0 | nrow(noZeroChkStn)>0){
		if(nrow(noZeroChkStn)>0) faileds0<-list('Stations not Zeros Checked',noZeroChkStn)
		else faileds0<-NULL
		if(nrow(noTraiteStn)>0) faileds1<-list('Stations not aggregated',noTraiteStn)
		else faileds1<-NULL
		faileds<-list(faileds0,faileds1)
		containertab<-displayConsOutputTabs(tknotes,faileds,title='Failed Stations')
		ntab<-length(tab.type)
		tab.type[[ntab+1]]<<-'ctxt'
		tab.data[[ntab+1]]<<-containertab
		tkselect(tknotes,ntab)
	}

	stn.don<-read.table(file.path(zchkdir,stnID[1],paste(stnID[1],'.txt',sep=''),fsep = .Platform$file.sep))
	aggData<-stn.don[,1]

	for(jj in stnID){
		filein<-file.path(zchkdir,jj,paste(jj,'.txt',sep=''),fsep = .Platform$file.sep)
		stn.don<-read.table(filein)
		aggData<-cbind(aggData,stn.don[,2])
	}

	if(is.null(paramsGAL$data$elv)) capition<-c('Stations','LON',paste('DAILY','LAT',sep='/'))
	else capition<-c('Stations','LON','LAT',paste('DAILY','ELV',sep='/'))
	infohead<-cbind(capition,t(infohead))
	aggData<-t(cbind(t(infohead),t(aggData)))
	aggData[is.na(aggData)]<-paramsGAL$dataPars[[2]]$miss.val

	fileout<-file.path(datfin,paste('ZeroChecked',paramsGAL$inputPars$file.io$Values[1],sep='_'),fsep = .Platform$file.sep)
	write.table(aggData,fileout,col.names=F,row.names=F,quote=F)
	return(0)
}




ExeAggTimeSeries<-function(GeneralParameters){

	period<-as.character(GeneralParameters$period)
	period1<-as.character(GeneralParameters$period1)

	datatype<-as.character(GeneralParameters$data.type)
	file.pars<-as.character(GeneralParameters$file.io$Values)

	fun<-as.character(GeneralParameters$compute.var$Values[1])
	minfrac<-as.numeric(GeneralParameters$compute.var$Values[2])

	if(datatype=='cdt'){
		comp.fun<-paste(period,2,period1,sep='')
		comp.fun<-match.fun(comp.fun)
		all.open.file<-as.character(unlist(lapply(1:length(AllOpenFilesData),function(j) AllOpenFilesData[[j]][[1]])))
		jfile<-which(all.open.file==file.pars[1])
		if(length(jfile)==0) return(NULL)
		donne<-AllOpenFilesData[[jfile]][[2]]
		miss.val<-AllOpenFilesData[[jfile]][[4]]$miss.val
		donne<-splitCDTData(donne,period)
		lon<-donne$lon
		lat<-donne$lat
		donne.id<-donne$id
		elv<-donne$elv
		dates<-donne$dates
		donne<-donne$data

		outdonne<-apply(donne,2,comp.fun,dates=dates,fun=fun,frac=minfrac)
		outdates<-as(outdonne[[1]]$Date,'character')
		outdonne<-sapply(outdonne,function(x) x$Values)
		outdonne<-round(outdonne,1)
		outdonne[is.na(outdonne)]<-miss.val
		if(is.null(elv)){
			headers<-t(cbind(donne.id,lon,lat))
			capition<-c('Stations','LON',paste(toupper(period1),'LAT',sep='/'))
		}else{
			headers<-t(cbind(donne.id,lon,lat,elv))
			capition<-c('Stations','LON','LAT',paste(toupper(period1),'ELV',sep='/'))
		}
		entete<-cbind(capition,headers)
		outdonne<-rbind(entete,cbind(outdates,outdonne))
		write.table(outdonne,file.pars[2],col.names=F,row.names=F,quote=F)
	}

	##############################################
	if(datatype=='series'){
		comp.fun<-paste(period,2,period1,sep='')
		comp.fun<-match.fun(comp.fun)
		filefrmt<-as.character(GeneralParameters$file.date.format$Values[1])
		datefrmt<-as.character(GeneralParameters$file.date.format$Values[2])
		all.open.file<-as.character(unlist(lapply(1:length(AllOpenFilesData),function(j) AllOpenFilesData[[j]][[1]])))
		jfile<-which(all.open.file==file.pars[1])
		if(length(jfile)==0) return(NULL)
		donne<-AllOpenFilesData[[jfile]][[2]]
		miss.val<-AllOpenFilesData[[jfile]][[4]]$miss.val
		donne<-splitTsData(donne,period,filefrmt,datefrmt)
		dates<-donne$dates
		if(donne$nbvar==3){
			rr<-comp.fun(donne$var$rr,dates,fun='sum',frac=minfrac)
			tx<-comp.fun(donne$var$tx,dates,fun='mean',frac=minfrac)
			tn<-comp.fun(donne$var$tn,dates,fun='mean',frac=minfrac)
			vars<-round(cbind(rr[,2],tx[,2],tn[,2]),1)
			vars[is.na(vars)]<-miss.val
			vars<-cbind(as.character(rr[,1]),vars)
		}else{
			vars<-comp.fun(donne$var$var,dates,fun=fun,frac=minfrac)
			tmp<-round(vars[,2],1)
			tmp[is.na(tmp)]<-miss.val
			vars<-cbind(as.character(vars[,1]),tmp)
		}
		write.table(vars,file.pars[2],col.names=F,row.names=F,quote=F)
	}

	##############################################
	if(datatype=='netcdf'){
		comp.fun<-paste(period,2,period1,'_nc',sep='')
		comp.fun<-match.fun(comp.fun)
		istart.yrs<-as.numeric(as.character(GeneralParameters$datesSE$Values[1]))
		istart.mon<-as.numeric(as.character(GeneralParameters$datesSE$Values[2]))
		istart.day<-as.numeric(as.character(GeneralParameters$datesSE$Values[3]))
		iend.yrs<-as.numeric(as.character(GeneralParameters$datesSE$Values[4]))
		iend.mon<-as.numeric(as.character(GeneralParameters$datesSE$Values[5]))
		iend.day<-as.numeric(as.character(GeneralParameters$datesSE$Values[6]))

		informat<-as.character(GeneralParameters$IO.file.format$Values[1])
		outformat<-as.character(GeneralParameters$IO.file.format$Values[2])
		varid.out<-as.character(GeneralParameters$netcdf.var$Values[1])
		longname<-as.character(GeneralParameters$netcdf.var$Values[2])

		istart<-paste(istart.yrs,istart.mon,istart.day,sep='-')
		iend<-paste(iend.yrs,iend.mon,iend.day,sep='-')
		dstart<-as.Date(istart,format='%Y-%m-%d')
		dend<-as.Date(iend,format='%Y-%m-%d')

		if(period=='daily'){
			daty<-seq(dstart,dend,'day')
			dates<-format(daty,'%Y%m%d')
		}else if(period=='dekadal'){
			daty<-seq(dstart,dend,'day')
			idays<-as.numeric(format(daty,'%d'))<4
			dates<-paste(format(daty[idays],'%Y%m'),as.numeric(format(daty[idays],'%d')),sep='')
		}else if(period=='monthly'){
			daty<-seq(dstart,dend,'month')
			dates<-format(daty,'%Y%m')
		}

		comp.fun(file.pars,informat,dates,fun,minfrac,varid.out,longname,outformat)
	}
	return(0)
}

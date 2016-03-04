excludeOutStnFun<-function(gal.params){
	file.pars<-as.character(gal.params$file.io$Values)
	stnf<-file.pars[1]
	shpf<-file.pars[2]
	dir2save<-file.pars[3]
	buff<-as.numeric(as.character(gal.params$buffer))/111
	outdir<-file.path(dir2save,paste('ChkCoord',getf.no.ext(stnf),sep='_'),fsep = .Platform$file.sep)
	if(!file.exists(outdir)) dir.create(outdir)

	all.open.file<-as.character(unlist(lapply(1:length(file.opfiles),function(j) file.opfiles[[j]][[1]])))

	jshp<-which(all.open.file==shpf)
	if(type.opfiles[[jshp]]=="shp") shpd<-file.opfiles[[jshp]][[2]]
	else{
		insert.txt(main.txt.out,paste(shpf,'is not an ESRI shapefile'),format=TRUE)
		return(NULL)
	}
	# shpd<-gUnaryUnion(shpd)
	# shpd<-gBuffer(shpd,width=buff)
	# row.names<-sapply(slot(shpd, "polygons"), function(x) slot(x, "ID"))
	# datafr<-data.frame(rep(NA,length(row.names)),row.names=row.names)
	# names(datafr)<-'idsp'
	# shpd<-SpatialPolygonsDataFrame(shpd, data=datafr)

	shpd<-as(shpd,"SpatialPolygons")
	shpd<-gUnaryUnion(shpd)
	shpd<-gSimplify(shpd, tol=0.05, topologyPreserve=TRUE)
	shpd<-gBuffer(shpd,width=buff)


	jfile<-which(all.open.file==stnf)
	if(length(jfile)==0){
		insert.txt(main.txt.out,paste(stnf,'cannot found') ,format=TRUE)
		return(NULL)
	}
	donne<-file.opfiles[[jfile]][[2]]
	missval<-file.opfiles[[jfile]][[4]]$miss.val

	id<-as.character(donne[1,-1])
	lon<-round(as.numeric(donne[2,-1]),6)
	lat<-round(as.numeric(donne[3,-1]),6)

	##missing coordinates
	imiss<-which((is.na(lon) | lon< -180 | lon>360 |is.na(lat) | lat< -90 | lat >90))

	##ducpicates coordinates
	idup0<-which(duplicated(cbind(lon,lat)))
	idup1<-which(duplicated(cbind(lon,lat),fromLast=T))
	idup<-sort(union(idup0,idup1))

	#coordinates outside country boundaries
	infodon<-data.frame(lon=lon,lat=lat,ID=id)
	idout<-1:nrow(infodon)
	infodon<-na.omit(infodon)
	coordinates(infodon)<- ~lon+lat
	# shpd[['vtmp']]<-1
	# ioutc<-which(is.na(over(infodon,shpd)[,'vtmp']))
	# ioutc<-idout[ioutc]

	ioutc<-is.na(over(infodon,geometry(shpd)))
	ioutc<-idout[ioutc]

	####
	exclus<-NULL
	if(length(imiss)>0) exclus<-rbind(exclus,cbind('Missing Coordinates',cbind(id[imiss],lon[imiss],lat[imiss],imiss)))
	if(length(idup)>0)	exclus<-rbind(exclus,cbind('Duplicate Coordinates',cbind(id[idup],lon[idup],lat[idup],idup)))
	if(length(ioutc)>0) exclus<-rbind(exclus,cbind('Coordinates Outside',cbind(id[ioutc],lon[ioutc],lat[ioutc],ioutc)))
	if(!is.null(exclus)){
		exclus<-as.data.frame(exclus)
		exclus<-exclus[!duplicated(exclus[,c(2,5)]),]
		exclus<-exclus[order(exclus[,3,4]),]
	}else{
		exclus<-data.frame(NA,NA,NA,NA,NA)
	}
	names(exclus)<-c('Info','ID.Station','Longitude','Latitude','ID.Col')
	fexcl<-file.path(outdir,paste(getf.no.ext(stnf),'_2CORRECT_STATIONS.txt',sep=''),fsep = .Platform$file.sep)
	write.table(exclus,fexcl,row.names=FALSE,col.names=TRUE)

	###
	if(length(grep('alt|elev|elv',donne[4,1],ignore.case=TRUE))==1){
		HeadInfo<-donne[1:4,]

		if(nchar(as.character(donne[5,1]))==8){
			dates<-as.Date(donne[-c(1:4),1],format='%Y%m%d')
			period<-'daily'
		}else if(nchar(as.character(donne[5,1]))==7){
			xan<-substr(as.character(donne[-c(1:4),1]),1,4)
			xmo<-substr(as.character(donne[-c(1:4),1]),5,6)
			xdk<-substr(as.character(donne[-c(1:4),1]),7,7)
			notdek<-which(as.numeric(xdk)>3)
			dates<-as.Date(paste(xan,xmo,xdk,sep='-'))
			dates[notdek]<-NA
			period<-'dekadal'
		}else if(nchar(as.character(donne[5,1]))==6){
			xan<-substr(as.character(donne[-c(1:4),1]),1,4)
			xmo<-substr(as.character(donne[-c(1:4),1]),5,6)
			dates<-as.Date(paste(xan,xmo,'1',sep='-'))
			period<-'monthly'
		}else{
			insert.txt(main.txt.out,'Date has wrong format.',format=TRUE)
			return(NULL)
		}
		donne<-donne[-c(1:4),-1]
	}else{
		HeadInfo<-donne[1:3,]

		if(nchar(as.character(donne[5,1]))==8){
			dates<-as.Date(donne[-c(1:3),1],format='%Y%m%d')
			period<-'daily'
		}else if(nchar(as.character(donne[5,1]))==7){
			xan<-substr(as.character(donne[-c(1:3),1]),1,4)
			xmo<-substr(as.character(donne[-c(1:3),1]),5,6)
			xdk<-substr(as.character(donne[-c(1:3),1]),7,7)
			notdek<-which(as.numeric(xdk)>3)
			dates<-as.Date(paste(xan,xmo,xdk,sep='-'))
			period<-'dekadal'
		}else if(nchar(as.character(donne[5,1]))==6){
			xan<-substr(as.character(donne[-c(1:3),1]),1,4)
			xmo<-substr(as.character(donne[-c(1:3),1]),5,6)
			dates<-as.Date(paste(xan,xmo,'1',sep='-'))
			period<-'monthly'
		}else{
			insert.txt(main.txt.out,'Date has wrong format.',format=TRUE)
			return(NULL)
		}
		donne<-donne[-c(1:3),-1]
	}

	donne<-apply(donne,2,as.numeric)
	donne<-donne[!is.na(dates),]
	dates<-dates[!is.na(dates)]
	donne<-donne[order(dates),]
	dates<-dates[order(dates)]

	##fill missing dates
	if(period=='daily'){
		odates<-data.frame(format(seq(min(dates),max(dates),'day'),'%Y%m%d'))
		names(odates)<-'dates'
		dates<-format(dates,'%Y%m%d')
	}else if(period=='dekadal'){
		odates<-seq(min(dates),max(dates),'day')
		odates<-paste(format(odates[which(as.numeric(format(odates,'%d'))<=3)],'%Y%m'),
		as.numeric(format(odates[which(as.numeric(format(odates,'%d'))<=3)],'%d')),sep='')
		odates<-data.frame(odates)
		names(odates)<-'dates'
		dates<-paste(format(dates,'%Y%m'),as.numeric(format(dates,'%d')),sep='')
	}else if(period=='monthly'){
		odates<-data.frame(format(seq(min(dates),max(dates),'month'),'%Y%m'))
		names(odates)<-'dates'
		dates<-format(dates,'%Y%m')
	}
	tmp.data<-data.frame(dates=dates,donne)
	tmp.data<-merge(odates,tmp.data,by.x='dates',by.y='dates',all=T)
	donne<-tmp.data[,-1]
	dates<-as.character(tmp.data[,1])

	don<-cbind(dates,donne)
	names(don)<-names(HeadInfo)
	donne1<-rbind(HeadInfo,don)
	donne1[is.na(donne1)]<-missval
	fsave<-file.path(outdir,paste(getf.no.ext(stnf),'_CHECKED_STATIONS.txt',sep=''),fsep = .Platform$file.sep)
	write.table(donne1,fsave,row.names=FALSE,col.names=FALSE)

	res<-list(action='chk.coords',period=period,Stndoute=exclus,data=donne,dates=dates,
	HeadInfo=HeadInfo,missval=missval,outdir=outdir)
	rm(odates,tmp.data,don,donne1)

	return(res)
}

#########################

checkCDTcoords<-function(ret.results,gal.params){
	fexcl<-file.path(ret.results$outdir,paste(getf.no.ext(as.character(gal.params$file.io$Values[1])),
	'_2CORRECT_STATIONS.txt',sep=''),fsep = .Platform$file.sep)
	fsave<-file.path(ret.results$outdir,paste(getf.no.ext(as.character(gal.params$file.io$Values[1])),
	'_CHECKED_STATIONS.txt',sep=''),fsep = .Platform$file.sep)

	#IdStn<-as.character(ret.results$HeadInfo[1,-1])

	exclus<-ret.results$Stndoute
	retenus<-read.table(fexcl,header=T)
######
	idna<-!is.na(as.character(retenus$ID.Station)) & !is.na(as.character(retenus$ID.Col))
	retenus<-retenus[idna,]

	exc.idcl<-as.character(exclus$ID.Col)
	ret.idcl<-as.character(retenus$ID.Col)
	stn.omit<-as.numeric(exc.idcl[is.na(match(exc.idcl,ret.idcl))])

	if(length(ret.idcl)>0){
		ret.col<-as.numeric(ret.idcl)
		ret.results$HeadInfo[2,ret.col+1]<-as.character(retenus$Longitude)
		ret.results$HeadInfo[3,ret.col+1]<-as.character(retenus$Latitude)
	}

	don<-cbind(ret.results$dates,ret.results$data)
	names(don)<-names(ret.results$HeadInfo)
	donne<-rbind(ret.results$HeadInfo,don)
	donne[is.na(donne)]<-ret.results$missval

	if(length(stn.omit)>0){
		donne<-donne[,-(stn.omit+1)]
		ret.results$HeadInfo<-ret.results$HeadInfo[,-(stn.omit+1)]
		ret.results$data<-ret.results$data[,-stn.omit]
	}


######
#	exc.id<-as.character(exclus$ID.Station)
#	ret.id<-as.character(retenus$ID.Station)
#	ret.lon<-as.character(retenus$Longitude)
#	ret.lat<-as.character(retenus$Latitude)
#	idna<-!is.na(ret.id)
#	ret.id<-ret.id[idna]
#	ret.lon<-ret.lon[idna]
#	ret.lat<-ret.lat[idna]
#	stn.omit<-exc.id[is.na(match(exc.id,ret.id))]
#	omit<-which(!is.na(match(IdStn,stn.omit)))

#	ijna<-match(IdStn,ret.id)
#	ijna<-ijna[!is.na(ijna)]
#	ret.col<-which(!is.na(match(IdStn,ret.id)))
#	if(length(ret.col)>0){
#		ret.results$HeadInfo[2,ret.col+1]<-ret.lon[ijna]
#		ret.results$HeadInfo[3,ret.col+1]<-ret.lat[ijna]
#	}

#	don<-cbind(ret.results$dates,ret.results$data)
#	names(don)<-names(ret.results$HeadInfo)
#	donne<-rbind(ret.results$HeadInfo,don)
#	donne[is.na(donne)]<-ret.results$missval

#	if(length(omit)>0){
#		donne<-donne[,-(omit+1)]
#		ret.results$HeadInfo<-ret.results$HeadInfo[,-(omit+1)]
#		ret.results$data<-ret.results$data[,-omit]
#	}
#####

	write.table(donne,fsave,row.names=FALSE,col.names=FALSE)
	rm(don,donne)
	return(ret.results)
}


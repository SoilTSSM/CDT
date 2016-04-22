
#################
splitCDTData<-function(donne,period){
	# ideb<-grep('[[:digit:]]',donne[1:5,1])[1]
	ideb<-nrow(donne)
	if(period=='daily'){
		if(nchar(as.character(donne[ideb,1]))!=8){
			insert.txt(main.txt.out,'Station data: not a daily data',format=TRUE)
			return(NULL)
		}
	}else if(period=='dekadal'){
		if(nchar(as.character(donne[ideb,1]))!=7){
			insert.txt(main.txt.out,'Station data: not a dekadal data',format=TRUE)
			return(NULL)
		}
	}else if(period=='monthly'){
		if(nchar(as.character(donne[ideb,1]))!=6){
			insert.txt(main.txt.out,'Station data: not a monthly data',format=TRUE)
			return(NULL)
		}
	}
	stn.id<-as.character(donne[1,-1])
	stn.lon<-as.numeric(donne[2,-1])
	stn.lat<-as.numeric(donne[3,-1])
	if(length(grep('alt|elev|elv',donne[4,1],ignore.case=TRUE))==1){
		Info<-data.frame(t(donne[1:4,-1]))
		names(Info)<-c('Stations','Lon','Lat','ELV')
		if(period=='daily'){
			dates<-as.Date(donne[-c(1:4),1],format='%Y%m%d')
		}else if(period=='dekadal'){
			xan<-substr(as.character(donne[-c(1:4),1]),1,4)
			xmo<-substr(as.character(donne[-c(1:4),1]),5,6)
			xdk<-substr(as.character(donne[-c(1:4),1]),7,7)
			notdek<-which(as.numeric(xdk)>3)
			dates<-as.Date(paste(xan,xmo,xdk,sep='-'))
			dates[notdek]<-NA
		}else if(period=='monthly'){
			xan<-substr(as.character(donne[-c(1:4),1]),1,4)
			xmo<-substr(as.character(donne[-c(1:4),1]),5,6)
			dates<-as.Date(paste(xan,xmo,'1',sep='-'))
		}
		stn.elv<-as.numeric(donne[4,-1])
		donne<-as.matrix(donne[-c(1:4),-1])
	}else{
		Info<-data.frame(t(donne[1:3,-1]))
		names(Info)<-c('Stations','Lon','Lat')
		if(period=='daily'){
			dates<-as.Date(donne[-c(1:3),1],format='%Y%m%d')
		}else if(period=='dekadal'){
			xan<-substr(as.character(donne[-c(1:3),1]),1,4)
			xmo<-substr(as.character(donne[-c(1:3),1]),5,6)
			xdk<-substr(as.character(donne[-c(1:3),1]),7,7)
			notdek<-which(as.numeric(xdk)>3)
			dates<-as.Date(paste(xan,xmo,xdk,sep='-'))
		}else if(period=='monthly'){
			xan<-substr(as.character(donne[-c(1:3),1]),1,4)
			xmo<-substr(as.character(donne[-c(1:3),1]),5,6)
			dates<-as.Date(paste(xan,xmo,'1',sep='-'))
		}
		stn.elv<-NULL
		donne<-as.matrix(donne[-c(1:3),-1])
	}
	dimnames(donne)<-NULL
	dimdonne<-dim(donne)
	donne<-apply(donne,2,as.numeric)
	dim(donne)<-dimdonne
	donne<-donne[!is.na(dates),,drop=F]
	dates<-dates[!is.na(dates)]
	donne<-donne[order(dates),,drop=F]
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
	donne<-as.matrix(tmp.data[,-1,drop=F])
	dimnames(donne)<-NULL
	dates<-as.character(tmp.data[,1])
	rm(odates,tmp.data)

	###duplicating dates
	#idates<-duplicated(dates) | duplicated(dates, fromLast=TRUE)
	idates<-duplicated(dates)
	dup.dates<-dates[idates]
	donne<-donne[!idates,,drop=F]
	dates<-dates[!idates]

	##missing coordinates
	imiss<-(is.na(stn.lon) | is.na(stn.lat) | stn.lat< -90 | stn.lat >90)
	stn.miss<-Info[imiss,]
	stn.lon<-stn.lon[!imiss]
	stn.lat<-stn.lat[!imiss]
	stn.id<-stn.id[!imiss]
	stn.elv<-stn.elv[!imiss]
	donne<-donne[,!imiss,drop=F]

	##ducpicates coordinates
	idup<-duplicated(cbind(stn.lon,stn.lat))
	idup1<-duplicated(cbind(stn.lon,stn.lat),fromLast=T) #omit doublon
	stn.dup<-rbind(Info[idup1,],Info[idup,])
	stn.dup<-stn.dup[!duplicated(stn.dup[,1]),]
	stn.lon<-stn.lon[!idup]
	stn.lat<-stn.lat[!idup]
	stn.id<-stn.id[!idup]
	stn.elv<-stn.elv[!idup]
	donne<-donne[,!idup,drop=F]
	stnlist<-list(id=stn.id,lon=stn.lon,lat=stn.lat,elv=stn.elv,dates=dates,data=donne,
	stnDuplCoords=stn.dup,stnMissCoords=stn.miss,datesDupl=dup.dates)
	return(stnlist)
}


##########################################

##check numeric value
#CheckNumeric<-function(x) suppressWarnings(!is.na(as.numeric(x) & !is.na(x)))


#	test<-apply(donne,2,function(x) CheckNumeric(x))
#	if(sum(!test)>0){
#		insert.txt(main.txt.out,'There is Non-Numeric value in data',format=TRUE)
#		return(NULL)
#	}


splitTsData<-function(donne,period,filefrmt,datefrmt){
	#get dates
	if(period=='daily'){
		if(datefrmt=="1"){
			if(nchar(as.character(donne[5,1]))!=8){
				insert.txt(main.txt.out,'Station data: not a daily data',format=TRUE)
				return(NULL)
			}
			dates<-as.Date(as.character(donne[,1]),format='%Y%m%d')
		}else{
			dates<-as.Date(paste(as.character(donne[,1]),as.character(donne[,2]),as.character(donne[,3]),sep='-'))
		}
	}else if(period=='dekadal'){
		if(datefrmt=="1"){ #1date
			if(nchar(as.character(donne[5,1]))!=7){
				insert.txt(main.txt.out,'Station data: not a dekadal data',format=TRUE)
				return(NULL)
			}
			xan<-substr(as.character(donne[,1]),1,4)
			xmo<-substr(as.character(donne[,1]),5,6)
			xdk<-substr(as.character(donne[,1]),7,7)
			notdek<-which(as.numeric(xdk)>3)
			dates<-as.Date(paste(xan,xmo,xdk,sep='-'))
			dates[notdek]<-NA
		}else{ #3date
			dates<-as.Date(paste(as.character(donne[,1]),as.character(donne[,2]),as.character(donne[,3]),sep='-'))
			notdek<-which(as.numeric(as.character(donne[,3]))>3)
			dates[notdek]<-NA
		}
	}else if(period=='monthly'){
		if(datefrmt=="1"){ #1date
			if(nchar(as.character(donne[5,1]))!=6){
				insert.txt(main.txt.out,'Station data: not a monthly data',format=TRUE)
				return(NULL)
			}
			xan<-substr(as.character(donne[,1]),1,4)
			xmo<-substr(as.character(donne[,1]),5,6)
			dates<-as.Date(paste(xan,xmo,'1',sep='-'))
		}else{ #3date
			dates<-as.Date(paste(as.character(donne[,1]),as.character(donne[,2]),'1',sep='-'))
		}
	}

	#get vars
	if(filefrmt=="1"){ #1var
		if(datefrmt=="1"){
			var<-as.numeric(donne[,2])
		}else{
			if(period=='monthly'){
				if(ncol(donne)==3) var<-as.numeric(donne[,3])
				if(ncol(donne)==4) var<-as.numeric(donne[,4])
			}else{
				var<-as.numeric(donne[,4])
			}
		}
	}else{#3var
		if(datefrmt=="1"){
			rr<-as.numeric(donne[,2])
			tx<-as.numeric(donne[,3])
			tn<-as.numeric(donne[,4])
		}else{
			if(period=='monthly'){
				if(ncol(donne)==5){
					rr<-as.numeric(donne[,3]) #rr=3, tx=4, tn=5
					tx<-as.numeric(donne[,4])
					tn<-as.numeric(donne[,5])
				}
				if(ncol(donne)==6){
					rr<-as.numeric(donne[,4]) #rr=4, tx=5, tn=6
					tx<-as.numeric(donne[,5])
					tn<-as.numeric(donne[,6])
				}
			}else{
				rr<-as.numeric(donne[,4]) #rr=4, tx=5, tn=6
				tx<-as.numeric(donne[,5])
				tn<-as.numeric(donne[,6])
			}
		}
	}

	##remove NA dates and order
	if(filefrmt=="1"){
		var<-var[!is.na(dates)]
		dates<-dates[!is.na(dates)]
		var<-var[order(dates)]
		dates<-dates[order(dates)]
	}else{
		rr<-rr[!is.na(dates)]
		tx<-tx[!is.na(dates)]
		tn<-tn[!is.na(dates)]
		dates<-dates[!is.na(dates)]
		rr<-rr[order(dates)]
		tx<-tx[order(dates)]
		tn<-tn[order(dates)]
		dates<-dates[order(dates)]
	}

	if(period=='daily') dates<-format(dates,'%Y%m%d')
	if(period=='dekadal') dates<-paste(format(dates,'%Y%m'),as.numeric(format(dates,'%d')),sep='')
	if(period=='monthly') dates<-format(dates,'%Y%m')

 	##duplicated dates
 	idates<-duplicated(dates)
	dup.dates<-dates[idates]
	dates<-dates[!idates]

	if(filefrmt=="1"){
		var<-var[!idates]
		var<-list(var=var)
		nbvar<-1
	}else{
		rr<-rr[!idates]
		tx<-tx[!idates]
		tn<-tn[!idates]
		var<-list(rr=rr,tx=tx,tn=tn)
		nbvar<-3
	}
	if(length(dates)==0){
		insert.txt(main.txt.out,'Wrong date format',format=TRUE)
		return(NULL)
	}
	ret<-list(period=period,nbvar=nbvar,var=var,dates=dates,datesDupl=dup.dates)
	return(ret)
}



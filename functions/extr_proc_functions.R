getExtractDataFun<-function(retExtractParams){
	period<-retExtractParams[1]
	ncdir<-retExtractParams[2]
	ncfformat<-retExtractParams[3]
	shpfl<-retExtractParams[4]
	shpId<-retExtractParams[5]
	yrs1<-as.numeric(retExtractParams[6])
	mon1<-as.numeric(retExtractParams[7])
	day1<-as.numeric(retExtractParams[8])
	yrs2<-as.numeric(retExtractParams[9])
	mon2<-as.numeric(retExtractParams[10])
	day2<-as.numeric(retExtractParams[11])
	outTS<-retExtractParams[12]
	season1<-retExtractParams[13]
	season2<-retExtractParams[14]
	aggfun<-retExtractParams[15]
	missfrac<-as.numeric(retExtractParams[16])
	extType<-retExtractParams[17]
	xminLon<-as.numeric(retExtractParams[18])
	xmaxLon<-as.numeric(retExtractParams[19])
	xminLat<-as.numeric(retExtractParams[20])
	xmaxLat<-as.numeric(retExtractParams[21])
	polyName<-retExtractParams[22]
	spAvrg<-retExtractParams[23]
	out2sav<-retExtractParams[24]
	ChoixOutType<-retExtractParams[25]
	multiptspoly<-retExtractParams[26]

	####
	outTsTable<-cbind(c('Daily','Dekadal','Monthly','3-Months','6-Months','Yearly'),c('daily','dekadal','monthly','season3','season6','yearly'))
	period1<-outTsTable[outTsTable[,1]==outTS,2]

	####
	if(is.na(yrs1) | is.na(mon1) | is.na(day1) | is.na(yrs2) | is.na(mon2) | is.na(day2)){
		insert.txt(main.txt.out,"Invalid date for time series extraction",format=TRUE)
		return(NULL)
	}

	####
	if(period=='Daily data'){
		period0<-'daily'
		dates<-format(seq(as.Date(paste(yrs1,mon1,day1,sep='-')),as.Date(paste(yrs2,mon2,day2,sep='-')),'day'),'%Y%m%d')
		ncfiles<-sprintf(ncfformat,substr(dates,1,4),substr(dates,5,6),substr(dates,7,8))
	}

	if(period=='Dekadal data'){
		period0<-'dekadal'
		dates<-seq(as.Date(paste(yrs1,mon1,day1,sep='-')),as.Date(paste(yrs2,mon2,day2,sep='-')),'day')
		dates<-paste(format(dates[which(as.numeric(format(dates,'%d'))<=3)],'%Y%m'),as.numeric(format(dates[which(as.numeric(format(dates,'%d'))<=3)],'%d')),sep='')
		ncfiles<-sprintf(ncfformat,substr(dates,1,4),substr(dates,5,6),substr(dates,7,7))
	}

	if(period=='Monthly data'){
		period0<-'monthly'
		dates<-format(seq(as.Date(paste(yrs1,mon1,day1,sep='-')),as.Date(paste(yrs2,mon2,day2,sep='-')),'month'),'%Y%m')
		ncfiles<-sprintf(ncfformat,substr(dates,1,4),substr(dates,5,6))
	}

	if(!file.exists(ncdir)){
		insert.txt(main.txt.out,"Directory containing NetCDF files does not exist",format=TRUE)
		return(NULL)
	}

	ncpath<-file.path(ncdir,ncfiles,fsep = .Platform$file.sep)
	existFl<-unlist(lapply(ncpath,file.exists))
	if(length(which(existFl))==0){
		insert.txt(main.txt.out,"Invalid filename format or date outside the range",format=TRUE)
		return(NULL)
	}

	############
	if(extType=='Point'){
		if(is.na(xminLon) | is.na(xminLat)){
			insert.txt(main.txt.out,"Invalid coordinates to extract",format=TRUE)
			return(NULL)
		}
	}

	if(extType=='Rectangle'){
		if(is.na(xminLon) | is.na(xmaxLon) | is.na(xminLat) | is.na(xmaxLat)){
			insert.txt(main.txt.out,"Invalid coordinates for the extraction",format=TRUE)
			return(NULL)
		}
	}

	if(extType=='Polygon'){
		all.open.file<-as.character(unlist(lapply(1:length(file.opfiles),function(j) file.opfiles[[j]][[1]])))
		jfile<-which(all.open.file==shpfl)
		if(type.opfiles[[jfile]]=="shp"){
			shpf<-file.opfiles[[jfile]][[2]]
			regOI<-shpf[as.character(shpf@data[,shpId])==polyName,]
			bbxregOI<-bbox(regOI)
		}else{
			insert.txt(main.txt.out,"Ceci ne devrait pas se produire",format=TRUE)
			return(NULL)
		}
	}

	if(!is.null(multiptspoly)){
		multiptspoly<-gsub("[\r]", "", multiptspoly)
		multiptspoly<-strsplit(multiptspoly,"[\n]")[[1]]
		multiptspoly<-multiptspoly[multiptspoly!=""]

		if(extType=='Multiple Points'){
			multiptspoly<-t(sapply(multiptspoly,function(x) strsplit(x," ")[[1]]))
			if(nrow(multiptspoly)>1) multiptspoly<-apply(multiptspoly,2,as.numeric)
			else multiptspoly<-matrix(as.numeric(multiptspoly),ncol=2)
			headinfo<-cbind(paste('Pts',1:nrow(multiptspoly),sep=''),multiptspoly)
			pts.loc<-as.data.frame(multiptspoly)
			names(pts.loc)<-c('x','y')
			coordinates(pts.loc)<- ~x+y
		}

		if(extType=='Multiple Polygons'){
			all.open.file<-as.character(unlist(lapply(1:length(file.opfiles),function(j) file.opfiles[[j]][[1]])))
			jfile<-which(all.open.file==shpfl)
			if(type.opfiles[[jfile]]=="shp"){
				shpf<-file.opfiles[[jfile]][[2]]
				multiptspoly<-str_trim(multiptspoly)
				regOI<-shpf[as.character(shpf@data[,shpId])%in%multiptspoly,]
				bbxregOI<-bbox(regOI)
				headinfo<-cbind(as.character(regOI@data[,shpId]),round(coordinates(regOI),5))
			}else{
				insert.txt(main.txt.out,"Ceci ne devrait pas se produire",format=TRUE)
				return(NULL)
			}
		}
	}else{
		if(extType=='Multiple Points') insert.txt(main.txt.out,"No selected points",format=TRUE)
		if(extType=='Multiple Polygons') insert.txt(main.txt.out,"No selected polygons",format=TRUE)
		return(NULL)
	}
	#############

	RVAL<-vector(mode='list',length=length(ncpath))

	for(fl in seq_along(ncpath)){
		if(!file.exists(ncpath[fl])){
			insert.txt(main.txt.out,paste('File',basename(ncpath[fl]),'does not exist') ,format=TRUE)
			next
		}

		#######
		nc<-open.ncdf(ncpath[fl])
		xlon<-nc$dim[[1]]$vals
		xlat<-nc$dim[[2]]$vals
		xval<-get.var.ncdf(nc,varid=nc$var[[1]]$name)
		close.ncdf(nc)

#Multiple Points & Polygons still slow
		#######
		if(extType=='Point'){
			iclo<-findInterval(xminLon,xlon)
			ilo<-iclo+(2 * xminLon> xlon[iclo]+xlon[iclo+1])
			icla<-findInterval(xminLat,xlat)
			ila<-icla+(2 * xminLat> xlat[icla]+xlat[icla+1])
			if(length(ilo)==0 | is.na(ilo) | length(ila)==0 | is.na(ila))  rval<-NA
			else rval<-xval[ilo,ila]
			RVAL[[fl]]<-rval
		}else if(extType=='Multiple Points'){
			ilo<-xlon>=min(multiptspoly[,1],na.rm=TRUE) & xlon<=max(multiptspoly[,1],na.rm=TRUE)
			ila<- xlat>=min(multiptspoly[,2],na.rm=TRUE) & xlat<=max(multiptspoly[,2],na.rm=TRUE)
			iloL<-which(ilo)
			ilaL<-which(ila)
			if(iloL[1]>1) ilo[iloL[1]-1]<-TRUE
			if(iloL[length(iloL)]<length(ilo)) ilo[iloL[length(iloL)]+1]<-TRUE
			if(ilaL[1]>1) ila[ilaL[1]-1]<-TRUE
			if(ilaL[length(ilaL)]<length(ila)) ila[ilaL[length(ilaL)]+1]<-TRUE
			rval<-xval[ilo,ila]
			rlon<-xlon[ilo]
			rlat<-xlat[ila]

			sptNC<-expand.grid(x=rlon,y=rlat)
			coordinates(sptNC)<- ~x+y
			sptNC<-SpatialPixels(points =sptNC, tolerance =sqrt(sqrt(.Machine$double.eps)),proj4string = CRS(as.character(NA)))
			RVAL[[fl]]<-rval[unname(over(pts.loc,geometry(sptNC)))]
		}else if(extType=='Multiple Polygons'){
			ilo<-xlon>=bbxregOI[1,1] & xlon<=bbxregOI[1,2]
			ila<- xlat>=bbxregOI[2,1] & xlat<=bbxregOI[2,2]
			rval<-xval[ilo,ila]
			rlon<-xlon[ilo]
			rlat<-xlat[ila]

			sptNC<-data.frame(expand.grid(x=rlon,y=rlat),z=c(rval))
			coordinates(sptNC)<- ~x+y
			idmat<-lapply(seq_along(regOI),function(j) matrix(over(sptNC, geometry(regOI[j,])),nrow=length(rlon),ncol=length(rlat)))
			RVAL[[fl]]<-sapply(seq_along(idmat),function(j) round(mean(rval[!is.na(idmat[[j]])],na.rm=TRUE),1))
		}else{
			if(extType=='Rectangle'){
				ilo<-xlon>=xminLon & xlon<=xmaxLon
				ila<- xlat>=xminLat & xlat<=xmaxLat
				rval<-xval[ilo,ila]
				rlon<-xlon[ilo]
				rlat<-xlat[ila]
			}
			if(extType=='Polygon'){
				ilo<-xlon>=bbxregOI[1,1] & xlon<=bbxregOI[1,2]
				ila<- xlat>=bbxregOI[2,1] & xlat<=bbxregOI[2,2]
				rval<-xval[ilo,ila]
				rlon<-xlon[ilo]
				rlat<-xlat[ila]

				sptNC<-data.frame(expand.grid(x=rlon,y=rlat),z=c(rval))
				coordinates(sptNC)<- ~x+y
				idmat<-matrix(over(sptNC, geometry(regOI)),nrow=length(rlon),ncol=length(rlat))
				rval[is.na(idmat)]<-NA
			}

			if(spAvrg=='1'){
				rval<-round(mean(rval,na.rm=TRUE),1)
				rval<-if(is.nan(rval)) NA else rval
			}
			RVAL[[fl]]<-rval
		}
	}


	##################################

	if(extType=='Point'){
		RVAL[sapply(RVAL,is.null)]<-NA
		xval<-unlist(RVAL)
		if(period0==period1){
			xval[is.na(xval)]<- -99
			xtmp<-cbind(dates,xval)
			writeFiles(xtmp,out2sav)
		}else{
			if(period1=='season3'){
				xtmp<-seasonal_fun(period0,xval,dates,smon=season1,lmon=3,fun=aggfun,frac=missfrac)
				xtmp[,3]<-round(xtmp[,3],1)
				xtmp[is.na(xtmp[,3]),3]<- -99
			}else if(period1=='season6'){
				xtmp<-seasonal_fun(period0,xval,dates,smon=season1,lmon=6,fun=aggfun,frac=missfrac)
				xtmp[,3]<-round(xtmp[,3],1)
				xtmp[is.na(xtmp[,3]),3]<- -99
			}else{
				comp.fun<-paste(period0,2,period1,sep='')
				comp.fun<-match.fun(comp.fun)
				xtmp<-comp.fun(xval,dates,fun=aggfun,frac=missfrac)
				xtmp[,2]<-round(xtmp[,2],1)
				xtmp[is.na(xtmp[,2]),2]<- -99
			}
			writeFiles(xtmp,out2sav)
		}
	}else if(extType=='Multiple Points' | extType=='Multiple Polygons'){
		RVAL[sapply(RVAL,is.null)]<-rep(NA,nrow(headinfo))
		xval<-do.call('rbind',RVAL)
#	assign('RVAL',RVAL,envir=.GlobalEnv)
#	assign('RVAL0',xval,envir=.GlobalEnv)
		capition<-c('Stations','LON','DATE/LAT')

		if(period0==period1){
			xval[is.na(xval)]<- -99
			xtmp<-t(cbind(t(cbind(capition,t(headinfo))),t(cbind(dates,xval))))
			writeFiles(xtmp,out2sav)
		}else{
			if(period1=='season3'){
				xtmp<-seasonal_funMat(period0,xval,dates,smon=season1,lmon=3,fun=aggfun,frac=missfrac)
				xtmp0<-round(xtmp[,-1],1)
				xtmp0[is.na(xtmp0)]<- -99
				xtmp[,-1]<-xtmp0
			}else if(period1=='season6'){
				xtmp<-seasonal_funMat(period0,xval,dates,smon=season1,lmon=6,fun=aggfun,frac=missfrac)
				xtmp0<-round(xtmp[,-1],1)
				xtmp0[is.na(xtmp0)]<- -99
				xtmp[,-1]<-xtmp0
			}else{
				comp.fun<-paste(period0,2,period1,sep='')
				comp.fun<-match.fun(comp.fun)

				#xtmp<-comp.fun(xval,dates,fun=aggfun,frac=missfrac)
				xtmp<-apply(xval,2,comp.fun,dates=dates,fun=aggfun,frac=missfrac)
				xtmp0<-as.character(xtmp[[1]][,1])
				xtmp<-round(sapply(xtmp,function(x)x[,2]),1)
				xtmp[is.na(xtmp)]<- -99
				xtmp<-cbind(xtmp0,xtmp)
			}
			xtmp<-t(cbind(t(cbind(capition,t(headinfo))),t(xtmp)))
			writeFiles(xtmp,out2sav)
		}
	}else{
		if(spAvrg=='1'){
			RVAL[sapply(RVAL,is.null)]<-NA
			xval<-unlist(RVAL)
			if(period0==period1){
				xval[is.na(xval)]<- -99
				xtmp<-cbind(dates,xval)
				writeFiles(xtmp,out2sav)
			}else{
				if(period1=='season3'){
					xtmp<-seasonal_fun(period0,xval,dates,smon=season1,lmon=3,fun=aggfun,frac=missfrac)
					xtmp[,3]<-round(xtmp[,3],1)
					xtmp[is.na(xtmp[,3]),3]<- -99
				}else if(period1=='season6'){
					xtmp<-seasonal_fun(period0,xval,dates,smon=season1,lmon=6,fun=aggfun,frac=missfrac)
					xtmp[,3]<-round(xtmp[,3],1)
					xtmp[is.na(xtmp[,3]),3]<- -99
				}else{
					comp.fun<-paste(period0,2,period1,sep='')
					comp.fun<-match.fun(comp.fun)
					xtmp<-comp.fun(xval,dates,fun=aggfun,frac=missfrac)
					xtmp[,2]<-round(xtmp[,2],1)
					xtmp[is.na(xtmp[,2]),2]<- -99
				}
				writeFiles(xtmp,out2sav)
			}
		}else{
			RVAL[sapply(RVAL,is.null)]<-matrix(NA,ncol=length(rlat),nrow=length(rlon))
			rlon<-matrix(round(rlon,5),nrow=nrow(rval),ncol=ncol(rval))
			rlat<-matrix(round(rlat,5),nrow=nrow(rval),ncol=ncol(rval),byrow=T)
			if(period0==period1){
				if(ChoixOutType=='1'){
					for(j in seq_along(RVAL)){
						fileout<-file.path(out2sav,paste('Output_',dates[j],'.txt',sep=''),fsep = .Platform$file.sep)
						write.table('Longitude',file=fileout,append=TRUE,row.names=FALSE,col.names=FALSE)
						write.table(rlon,file=fileout,append=TRUE,row.names=FALSE,col.names=FALSE)
						write.table('Latitude',file=fileout,append=TRUE,row.names=FALSE,col.names=FALSE)
						write.table(rlat,file=fileout,append=TRUE,row.names=FALSE,col.names=FALSE)
						write.table(paste('DATE:',dates[j]),file=fileout,append=TRUE,row.names=FALSE,col.names=FALSE)
						xtmp<-RVAL[[j]]
						xtmp[is.na(xtmp)]<- -99
						write.table(xtmp,file=fileout,append=TRUE,row.names=FALSE,col.names=FALSE)
					}
				}
				if(ChoixOutType=='2'){
					write.table('Longitude',file=out2sav,append=TRUE,row.names=FALSE,col.names=FALSE)
					write.table(rlon,file=out2sav,append=TRUE,row.names=FALSE,col.names=FALSE)
					write.table('Latitude',file=out2sav,append=TRUE,row.names=FALSE,col.names=FALSE)
					write.table(rlat,file=out2sav,append=TRUE,row.names=FALSE,col.names=FALSE)
					for(j in seq_along(RVAL)){
						write.table(paste('DATE:',dates[j]),file=out2sav,append=TRUE,row.names=FALSE,col.names=FALSE)
						xtmp<-RVAL[[j]]
						xtmp[is.na(xtmp)]<- -99
						write.table(xtmp,file=out2sav,append=TRUE,row.names=FALSE,col.names=FALSE)
					}
				}
				if(ChoixOutType=='3'){
					xlon<-rlon[,1]
					xlat<-as.numeric(rlat[1,])
					xcrds<-expand.grid(xlon,xlat,dates)
					xcrds<-xcrds[,3:1]
					xdata<-unlist(RVAL)
					xdata[is.na(xdata)]<- -99
					xtmp<-cbind(xcrds,xdata)
					writeFiles(xtmp,out2sav)
				}
			}else{
				if(period1=='season3'){
					lstOmat<-seasonal_lstOmat(period0,RVAL,dates,smon=season1,lmon=3,fun=aggfun,minfrac=missfrac)
					Season<-lstOmat[[1]]
					Start_Year<-lstOmat[[2]]
					daty<-paste(Season,Start_Year,sep='-')
					fileout<-file.path(out2sav,paste('Output_',Season,'_',Start_Year,'.txt',sep=''),fsep = .Platform$file.sep)
					capdate<-paste('Season:',Season,'Start_Year:',Start_Year)
					RVAL<-lstOmat[[3]]
				}else if(period1=='season6'){
					lstOmat<-seasonal_lstOmat(period0,RVAL,dates,smon=season1,lmon=6,fun=aggfun,minfrac=missfrac)
					Season<-lstOmat[[1]]
					Start_Year<-lstOmat[[2]]
					daty<-paste(Season,Start_Year,sep='-')
					fileout<-file.path(out2sav,paste('Output_',Season,'_',Start_Year,'.txt',sep=''),fsep = .Platform$file.sep)
					capdate<-paste('Season:',Season,'Start_Year:',Start_Year)
					RVAL<-lstOmat[[3]]
				}else{
					comp.fun<-paste(period0,2,period1,'_lstOmat',sep='')
					comp.fun<-match.fun(comp.fun)
					lstOmat<-comp.fun(RVAL,dates,fun=aggfun,minfrac=missfrac)
					daty<-lstOmat[[1]]
					fileout<-file.path(out2sav,paste('Output_',daty,'.txt',sep=''),fsep = .Platform$file.sep)
					capdate<-paste('DATE:',daty)
					RVAL<-lstOmat[[2]]
				}
				if(ChoixOutType=='1'){
					for(j in seq_along(RVAL)){
						write.table('Longitude',file=fileout[j],append=TRUE,row.names=FALSE,col.names=FALSE)
						write.table(rlon,file=fileout[j],append=TRUE,row.names=FALSE,col.names=FALSE)
						write.table('Latitude',file=fileout[j],append=TRUE,row.names=FALSE,col.names=FALSE)
						write.table(rlat,file=fileout[j],append=TRUE,row.names=FALSE,col.names=FALSE)
						write.table(capdate[j],file=fileout[j],append=TRUE,row.names=FALSE,col.names=FALSE)
						xtmp<-round(RVAL[[j]],1)
						xtmp[is.na(xtmp)]<- -99
						write.table(xtmp,file=fileout[j],append=TRUE,row.names=FALSE,col.names=FALSE)
					}
				}
				if(ChoixOutType=='2'){
					#write.table('Longitude',file=out2sav,append=TRUE,row.names=FALSE,col.names=FALSE)
					writeFiles('Longitude',file=out2sav,append=TRUE)
					#write.table(rlon,file=out2sav,append=TRUE,row.names=FALSE,col.names=FALSE)
					writeFiles(rlon,file=out2sav,append=TRUE)
					#write.table('Latitude',file=out2sav,append=TRUE,row.names=FALSE,col.names=FALSE)
					writeFiles('Latitude',file=out2sav,append=TRUE)
					#write.table(rlat,file=out2sav,append=TRUE,row.names=FALSE,col.names=FALSE)
					writeFiles(rlat,file=out2sav,append=TRUE)
					for(j in seq_along(RVAL)){
						#write.table(capdate[j],file=out2sav,append=TRUE,row.names=FALSE,col.names=FALSE)
						writeFiles(capdate[j],file=out2sav,append=TRUE)
						xtmp<-round(RVAL[[j]],1)
						xtmp[is.na(xtmp)]<- -99
						#write.table(xtmp,file=out2sav,append=TRUE,row.names=FALSE,col.names=FALSE)
						writeFiles(xtmp,file=out2sav,append=TRUE)
					}
				}
				if(ChoixOutType=='3'){
					xlon<-rlon[,1]
					xlat<-as.numeric(rlat[1,])
					xcrds<-expand.grid(xlon,xlat,daty)
					xcrds<-xcrds[,3:1]
					xdata<-round(unlist(RVAL),1)
					xdata[is.na(xdata)]<- -99
					xtmp<-cbind(xcrds,xdata)
					writeFiles(xtmp,out2sav)
				}
			}
		}
	}
	return(0)
}



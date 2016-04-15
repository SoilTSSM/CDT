
ValidationDataFun<-function(retValidParams){
	if(is.null(retValidParams$donne)){
		insert.txt(main.txt.out,'No station data found',format=TRUE)
		return(NULL)
	}

	if(is.null(retValidParams$rfedata)){
		insert.txt(main.txt.out,'Provide a sample of NetCDF data',format=TRUE)
		return(NULL)
	}

	if(retValidParams$ncdir=='' | retValidParams$ncdir=="NA"){
		insert.txt(main.txt.out,'Directory containing the NetCDF data not found',format=TRUE)
		return(NULL)
	}

	if(retValidParams$dir2sav=='' | retValidParams$dir2sav=="NA"){
		insert.txt(main.txt.out,'Provide a directory to save results',format=TRUE)
		return(NULL)
	}

	outValidation<-file.path(retValidParams$dir2sav,paste('Validation',getf.no.ext(retValidParams$filestn),sep='_'),fsep = .Platform$file.sep)
	dir.create(outValidation,showWarnings=FALSE,recursive=TRUE)
	caption<-c('Stations','LON','DATE/LAT')
	headinfo<-cbind(caption,t(cbind(retValidParams$donne$id,retValidParams$donne$lon,retValidParams$donne$lat)))

	if(retValidParams$do_extr==1){
		extr_stn<-ExtractNC2Stn(retValidParams)
		assign('extr_stn',extr_stn,envir=EnvRainValidation)
	}else extr_stn<-EnvRainValidation$extr_stn
	if(is.null(extr_stn)) return(NULL)

	stn.dates<-extr_stn[[3]]
	stn.data<-extr_stn[[2]]
	stn.ncdata<-extr_stn[[1]]

	donnees_stn<-t(cbind(t(headinfo),t(cbind(stn.dates,stn.data))))
	donnees_ncdf<-t(cbind(t(headinfo),t(cbind(stn.dates,round(stn.ncdata,1)))))
	write.table(donnees_stn,file.path(outValidation,'Gauge_VALIDATION_DATA.txt',fsep = .Platform$file.sep),col.names=F,row.names=F,quote=F)
	write.table(donnees_ncdf,file.path(outValidation,'RFE_VALIDATION_DATA.txt',fsep = .Platform$file.sep),col.names=F,row.names=F,quote=F)

	start_mois<-retValidParams$start_mois
	end_mois<-retValidParams$end_mois
	mois<-as.numeric(substr(stn.dates,5,6))

	tmois<-format(ISOdate(2014,1:12,1),"%B")
	smois<-which(tmois==start_mois)
	emois<-which(tmois==end_mois)
	im<-(smois:(smois+((emois-smois)%%12)))%%12
	im[im==0]<-12
	seas<-paste(substr(tmois,1,1)[im],collapse='')
	seasL<-mois%in%im
	stn.data<-stn.data[seasL,,drop=F]
	stn.ncdata<-stn.ncdata[seasL,,drop=F]
	inNA<- !is.na(stn.data) & !is.na(stn.ncdata)
	stn.data[!inNA]<-NA
	stn.ncdata[!inNA]<-NA

	stat<-validationStats(stn.data[inNA],stn.ncdata[inNA],retValidParams$donne$freq)
	write.table(stat,file.path(outValidation,'Validation_Statistics.txt',fsep = .Platform$file.sep),col.names=T,row.names=F)

	jpeg(file.path(outValidation,'Scatter_Gauge-RFE.jpg',fsep = .Platform$file.sep),width=960,height=960,quality=95)
	plot(stn.data[inNA],stn.ncdata[inNA],xlab="Gauge",ylab="RFE")
	abline(a=0, b=1, lwd=2,col='red')
	dev.off()

	jpeg(file.path(outValidation,'CDF_Gauge-RFE.jpg',fsep = .Platform$file.sep),width=960,height=960,quality=95)
	plot(ecdf(stn.data[inNA]),xlab="Rainfall [mm]",main="CDF",col='blue',lwd=2,cex=0.4,ylim=c(0,1))
	plot(ecdf(stn.ncdata[inNA]),add=T, col="red",lwd=2,cex=0.4)
	legend('bottomright',c('Gauge','RFE'),col=c('blue','red'),lwd=3,bg='lightgray')
	dev.off()

	###
	gg_tms<-apply(stn.data,1,mean,na.rm=T)
	gg_tms[is.nan(gg_tms)]<-NA
	rfe_tms<-apply(stn.ncdata,1,mean,na.rm=T)
	rfe_tms[is.nan(rfe_tms)]<-NA
	area_avg<-data.frame(date=stn.dates[seasL],stn=round(gg_tms,1),rfe=round(rfe_tms,1))
	write.table(area_avg,file.path(outValidation,'Spatial_Average_GG-RFE.txt',fsep = .Platform$file.sep),col.names=T,row.names=F,quote=F)

	area_stat<-validationStats(gg_tms,rfe_tms,retValidParams$donne$freq)
	write.table(area_stat,file.path(outValidation,'Spatial_Average_Validation_Statistics.txt',fsep = .Platform$file.sep),col.names=T,row.names=F)

	jpeg(file.path(outValidation,'Spatial_Average_Scatter_Gauge-RFE.jpg',fsep = .Platform$file.sep),width=960,height=960,quality=95)
	plot(gg_tms,rfe_tms,xlab="Gauge",ylab="RFE")
	abline(a=0, b=1, lwd=2,col='red')
	dev.off()

	jpeg(file.path(outValidation,'Spatial_Average_CDF_Gauge-RFE.jpg',fsep = .Platform$file.sep),width=960,height=960,quality=95)
	plot(ecdf(gg_tms),xlab="Rainfall [mm]",main="CDF",col='blue',lwd=2,cex=0.4,ylim=c(0,1))
	plot(ecdf(rfe_tms),add=T, col="red",lwd=2,cex=0.4)
	legend('bottomright',c('Gauge','RFE'),col=c('blue','red'),lwd=3,bg='lightgray')
	dev.off()

	return(list(x=stn.data[inNA],y=stn.ncdata[inNA],stat=stat,xs=gg_tms,ys=rfe_tms,sp.stat=area_stat))
}

###################

validationStats<-function(x,y,ferqData){
	mn0<-mean(x)
	sum1 <-  sum((y-x)^2)
	sum2 <-  sum((x-mn0)^2)
	eff <-  1. - sum1/sum2
	mae <-  mean(abs(y-x))
	mer <-  mean(y-x)
	bis <-  sum(y)/sum(x)
	cor <-  cor(x,y)

	stat <- c(cor, eff,bis, mae, mer)
	name_stat <- c("CORR", "NSE", "BIAS", "MAE", "ME")
	descrip<-c('Correlation','Nash-Sutcliffe Efficiency','Bias','Mean Absolute Error','Mean Error')

	if(ferqData=='daily'){
		rnr <- 1.0
		n1 <- length(which(x>= rnr & y >= rnr))  # Ht
		n2 <- length(which(x < rnr & y >= rnr))  # Fa
		n3 <- length(which(x >= rnr & y < rnr))  # Ms
		n4 <- length(which(x < rnr & y < rnr))   # Cn
		n <- n1+n2+n3+n4

		fbs <- (n1+n2)/(n1+n3)
		csi <- n1/(n-n4)
		pod <- n1/(n1+n3)
		far <- n2/(n1+n2)

		a <- n1*1.0
		b <- n2*1.0
		c <- n3*1.0
		d <- n4*1.0
		C0 = a+d
		N0 = a+b+c+d
		E0= ((a+c)*(a+b)+(b+d)*(c+d))/(a+b+c+d)
		hss= (C0-E0)/(N0-E0)

		stat <- c(cor, eff,bis, mae, mer, pod, far, fbs, csi, hss)
		name_stat <- c("CORR", "NSE", "BIAS", "MAE", "ME", "POD", "FAR","FBS", "CSI", "HSS")
		descrip<-c(descrip,'Probability Of Detection','False Alarm Ratio','Frequency Bias','Critical Success Index','Heidke Skill Score')
	}
	stat<-data.frame(Stat=name_stat,Value=stat,Description=descrip)
	return(stat)
}	


########################

ExtractNC2Stn<-function(retValidParams){
	freqData<-retValidParams$donne$freq
	lon.stn<-retValidParams$donne$lon
	lat.stn<-retValidParams$donne$lat
	date.stn<-retValidParams$donne$date
	data.stn<-retValidParams$donne$data
	stn.loc <- data.frame(lon=lon.stn, lat=lat.stn)
	stn.loc <- SpatialPoints(stn.loc)

	grd.lon<-retValidParams$rfedata[[2]]$x
	grd.lat<-retValidParams$rfedata[[2]]$y
	nlon0<-length(grd.lon)
	nlat0<-length(grd.lat)
	ncdfGrid <- expand.grid(lon=grd.lon, lat=grd.lat)
	coordinates(ncdfGrid) <- ~lon+lat
	ncdfGrid<-SpatialPixels(points =ncdfGrid, tolerance =sqrt(sqrt(.Machine$double.eps)),proj4string = CRS(as.character(NA)))

	#Index of gridded data over stations
	ijGrd <- over(stn.loc, geometry(ncdfGrid))

	ncdir<-retValidParams$ncdir
	ncformat<-retValidParams$ncformat

	if(freqData=='daily') testfile<-file.path(ncdir,sprintf(ncformat,substr(date.stn,1,4),substr(date.stn,5,6),substr(date.stn,7,8)),fsep=.Platform$file.sep)
	if(freqData=='dekadal') testfile<-file.path(ncdir,sprintf(ncformat,substr(date.stn,1,4),substr(date.stn,5,6),substr(date.stn,7,7)),fsep=.Platform$file.sep)
	if(freqData=='monthly') testfile<-file.path(ncdir,sprintf(ncformat,substr(date.stn,1,4),substr(date.stn,5,6)),fsep=.Platform$file.sep)

	existFl<-unlist(lapply(testfile,file.exists))
	if(length(which(existFl))==0){
		insert.txt(main.txt.out,"RFE data not found",format=TRUE)
		return(NULL)
	}
	rfeDataFl<-testfile[existFl]
	date.stn1<-date.stn[existFl]
	data.stn1<-data.stn[existFl,]

	rfe_stn<-matrix(NA,nrow=length(date.stn1),ncol=length(lon.stn))

	for (jfl in seq_along(rfeDataFl)){
		nc <- nc_open(rfeDataFl[jfl])
		rfe.lon <- nc$dim[[retValidParams$rfedata[[2]]$ilon]]$vals
		rfe.lat <- nc$dim[[retValidParams$rfedata[[2]]$ilat]]$vals
		rfe.val <- ncvar_get(nc,varid=retValidParams$rfedata[[2]]$varid)
		nc_close(nc)

		xo<-order(rfe.lon)
		rfe.lon<-rfe.lon[xo]
		yo<-order(rfe.lat)
		rfe.lat<-rfe.lat[yo]
		rfe.val<-rfe.val[xo,yo]

		if(retValidParams$rfedata[[2]]$ilat==1){
			rfe.val<-matrix(c(rfe.val),nrow=length(rfe.lon),ncol=length(rfe.lat),byrow=T)
		}

		rfe_stn[jfl,]<-rfe.val[ijGrd]
	}

	return(list(rfe_stn,data.stn1,date.stn1))
}


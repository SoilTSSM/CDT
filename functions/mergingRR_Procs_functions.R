

ExtractRFE2Stn<-function(ijGrd,gal.params,mrgRaindat){
	insert.txt(main.txt.out,'Extract RFE at guage locations ')
	tcl("update")
	freqData<-gal.params$period
	year1<-as.numeric(as.character(gal.params$dates.coef$Values[1]))
	year2<-as.numeric(as.character(gal.params$dates.coef$Values[2]))
	rfeDir<-as.character(gal.params$file.io$Values[4])
	rfeFileFormat<-as.character(gal.params$prefix$Values[1])
	nstn<-length(mrgRaindat$stnData$lon)

	if(freqData=='daily'){
		bias.dates<-format(seq(as.Date(paste(year1,'0101',sep=''),format='%Y%m%d'), as.Date(paste(year2,'1231',sep=''),format='%Y%m%d'),'day'),'%Y%m%d')
		testfile<-file.path(rfeDir,sprintf(rfeFileFormat,substr(bias.dates,1,4),substr(bias.dates,5,6), substr(bias.dates,7,8)),fsep = .Platform$file.sep)
	}
	if(freqData=='dekadal'){
		bias.dates<-seq(as.Date(paste(year1,'011',sep=''),format='%Y%m%d'), as.Date(paste(year2,'123',sep=''),format='%Y%m%d'),'day')
		bias.dates<-paste(format(bias.dates[which(as.numeric(format(bias.dates,'%d'))<=3)],'%Y%m'), as.numeric(format(bias.dates[which(as.numeric(format(bias.dates,'%d'))<=3)],'%d')),sep='')
		testfile<-file.path(rfeDir,sprintf(rfeFileFormat,substr(bias.dates,1,4),substr(bias.dates,5,6), substr(bias.dates,7,7)),fsep = .Platform$file.sep)
	}
	if(freqData=='monthly'){
		bias.dates<-format(seq(as.Date(paste(year1,'011',sep=''),format='%Y%m%d'),as.Date(paste(year2,'1231',sep=''),format='%Y%m%d'),'month'),'%Y%m')
		testfile<-file.path(rfeDir,sprintf(rfeFileFormat,substr(bias.dates,1,4),substr(bias.dates,5,6)), fsep = .Platform$file.sep)
	}

	rfe_stn<-matrix(NA,nrow=length(bias.dates),ncol=nstn)

	existFl<-unlist(lapply(testfile,file.exists))
	if(length(which(existFl))==0){
		insert.txt(main.txt.out,"RFE data not found",format=TRUE)
		return(NULL)
	}
	rfeDataFl<-testfile[existFl]
	bias.dates1<-bias.dates[existFl]

	for (jfl in seq_along(rfeDataFl)){
		nc <- open.ncdf(rfeDataFl[jfl])
		rfe.lon <- nc$dim[[mrgRaindat$rfeData$rfeILon]]$vals
		rfe.lat <- nc$dim[[mrgRaindat$rfeData$rfeILat]]$vals
		rfe.val <- get.var.ncdf(nc,varid=mrgRaindat$rfeData$rfeVarid)
		close.ncdf(nc)

		xo<-order(rfe.lon)
		rfe.lon<-rfe.lon[xo]
		yo<-order(rfe.lat)
		rfe.lat<-rfe.lat[yo]
		rfe.val<-rfe.val[xo,yo]

		if(mrgRaindat$rfeData$rfeILat==1){
			rfe.val<-matrix(c(rfe.val),nrow=length(rfe.lon),ncol=length(rfe.lat),byrow=T)
		}

		rfe_stn[which(bias.dates==bias.dates1[jfl]),]<-rfe.val[ijGrd]
	}
	insert.txt(main.txt.out,'Done! ')
	return(rfe_stn)
}

########################################################################################################

calcBiasRain<-function(i,ix1,stn.data,rfe_stn){
	stn.val <- as.numeric(stn.data[ix1,i])
	stn.rfe<-as.numeric(rfe_stn[ix1,i])
	ix0 <- !is.na(stn.val) & !is.na(stn.rfe)
	# ix1 <- stn.val>0  & stn.rfe>0
	# ix<- which(ix0 & ix1)
	bs <- NA
	 if(length(ix0)>5){
	 	sum_stn<-sum(stn.val[ix0])
	 	sum_rfe<-sum(stn.rfe[ix0])
	 	if(sum_rfe>0){
	 		bs<- sum_stn/sum_rfe
	 		if(bs>5) bs<-5
	 	} 
		# bs <- sum(stn.val[ix])/sum(stn.rfe[ix])
		# if(bs>5) bs<-5
		# if(is.nan(bs)) bs <- 1    # 0/0
		# if(is.infinite(bs)) bs <- 1.5  # n/0
		# if(bs == 0) bs <- 0.5  # 0/n
	 }
	return(bs)
}


########################################################################################################

ComputeMeanBiasRain<-function(rfe_stn,gal.params,mrgRaindat,paramGrd,origdir){
	freqData<-gal.params$period
	stn.lon<-mrgRaindat$stnData$lon
	stn.lat<-mrgRaindat$stnData$lat
	stn.dates<-mrgRaindat$stnData$dates
	stn.data<-mrgRaindat$stnData$data
	nstn<-length(stn.lon)

	year1<-as.numeric(as.character(gal.params$dates.coef$Values[1]))
	year2<-as.numeric(as.character(gal.params$dates.coef$Values[2]))

	xy.dim<-paramGrd$xy.dim
	newlocation.grid<-paramGrd$newlocation.grid
	nlon0<-paramGrd$nlon0
	nlat0<-paramGrd$nlat0

	min.nbrs<-as.numeric(as.character(gal.params$params.int$Values[1]))
	max.nbrs<-as.numeric(as.character(gal.params$params.int$Values[2]))
	max.dst<-as.numeric(as.character(gal.params$params.int$Values[3]))

	meanBiasPrefix<-as.character(gal.params$prefix$Values[2])
	# dirBias<-file.path(origdir,'Mean_Bias',fsep = .Platform$file.sep)
	# dir.create(dirBias,showWarnings=FALSE)

	if(freqData=='daily'){
		ntimes<-365
		bias.dates<-format(seq(as.Date(paste(year1,'0101',sep=''),format='%Y%m%d'),as.Date(paste(year2,'1231',sep=''),format='%Y%m%d'),'day'),'%Y%m%d')
		endmon<-c(31,28,31,30,31,30,31,31,30,31,30,31)
		bias <- array(data=NA, c(365,nstn))
		vtimes<-cbind(unlist(sapply(endmon,function(j) 1:j)),rep(1:12,endmon))
	}
	if(freqData=='dekadal'){
		ntimes<-36
		bias.dates<-seq(as.Date(paste(year1,'011',sep=''),format='%Y%m%d'),as.Date(paste(year2,'123',sep=''),format='%Y%m%d'),'day')
		bias.dates<-paste(format(bias.dates[which(as.numeric(format(bias.dates,'%d'))<=3)],'%Y%m'),as.numeric(format(bias.dates[which(as.numeric(format(bias.dates,'%d'))<=3)],'%d')),sep='')
		bias <- array(data=NA, c(36,nstn))
		vtimes<-expand.grid(1:3,1:12)
	}
	if(freqData=='monthly'){
		ntimes<-12
		bias.dates<-format(seq(as.Date(paste(year1,'011',sep=''),format='%Y%m%d'),as.Date(paste(year2,'1231',sep=''),format='%Y%m%d'),'month'),'%Y%m')
		bias <- array(data=NA, c(12,nstn))
		vtimes<-c(1:12)
	}	
	ibsdt<-bias.dates%in%stn.dates
	bsdates<-bias.dates[ibsdt]
	rfe_stn<-rfe_stn[ibsdt,,drop=F]
	istdt<-stn.dates%in%bias.dates
	stn.data<-stn.data[istdt,,drop=F]
	if(length(bsdates)>0){
		for (i in 1:nstn){
			for (nt in 1:36){
				if(freqData=='daily'){
					ix1<-which(as.numeric(substr(bsdates,7,8))==vtimes[nt,1] & as.numeric(substr(bsdates,5,6))==vtimes[nt,2])
					ix1<-c(sapply(ix1,function(x) x+(-5:5)))
					ix1<-ix1[ix1>0 & ix1<=length(bsdates)]
				}
				if(freqData=='dekadal'){	
					ix1<-which(as.numeric(substr(bsdates,7,7))==vtimes[nt,1] & as.numeric(substr(bsdates,5,6))==vtimes[nt,2])
				}
				if(freqData=='monthly'){
					ix1<-which(as.numeric(substr(bsdates,5,6))==vtimes[nt])
				}
				bias[nt,i] <- calcBiasRain(i,ix1,stn.data,rfe_stn)
			}
		}
	}

	############
	# Grid Bias
	##block grid
	sDX <- newlocation.grid@grid@cellsize[1]/2
	dBX <- seq(-sDX, sDX, length.out=4)
	sDY <- newlocation.grid@grid@cellsize[2]/2
	dBY <- seq(-sDY, sDY, length.out=4)
	bGrd <- expand.grid(x=dBX, y=dBY)

	#Defines netcdf output
	grd.bs <- var.def.ncdf("grid", "",xy.dim, NA, longname= " Gridded GG/RFE Bias", prec="single")
	
	tcl("update","idletasks")
	for(ij in 1:ntimes){
		bias.stn <- data.frame(bias=bias[ij,],lon=stn.lon,lat=stn.lat)
		ix <- which(!is.na(bias.stn$bias))
		if(length(ix)>10){
			bias.stn <- bias.stn[ix,]
			coordinates(bias.stn) =~lon + lat
			#bs<- idw(bias~1,locations=bias.stn,newdata=newlocation.grid,nmin=min.nbrs,nmax=max.nbrs,maxdist=max.dst, debug.level=0)
			#grd.bias <- bs$var1.pred
			gbias<-krige(bias~1,locations=bias.stn,newdata=newlocation.grid,block=bGrd,nmin=min.nbrs,nmax=max.nbrs,maxdist=max.dst,debug.level=0)
			grd.bias <- gbias$var1.pred
			grd.bias[is.na(grd.bias)]<-1
			#smoothing
			imgbs<-as.image( grd.bias, x= coordinates(gbias), nx=nlon0, ny=nlat0)
			smbias<-image.smooth(imgbs, theta= 0.08)  #decrease theta's value
			grd.bias <- smbias$z
		}else{
			grd.bias <-rep(1,nlon0*nlat0)
			dim(grd.bias) <- c(nlon0, nlat0)
		}
		#dim(grd.bias) <- c(nlon0, nlat0)
		#grd.bias[is.na(grd.bias)] <- 1

		#outfl <- file.path(dirBias,paste(meanBiasPrefix,'_',ij,'.nc',sep=''),fsep = .Platform$file.sep)
		outfl <- file.path(origdir,paste(meanBiasPrefix,'_',ij,'.nc',sep=''),fsep = .Platform$file.sep)
		nc2 <- create.ncdf(outfl,grd.bs)
		put.var.ncdf(nc2,grd.bs,grd.bias)
		close.ncdf(nc2)
		insert.txt(main.txt.out,paste("Computing mean bias finished:",paste(meanBiasPrefix,'_',ij,'.nc',sep='')))
		tcl("update")
	}
	return(0)
}

########################################################################################################

##Adjust downscaled data

AjdMeanBiasRain<-function(freqData,istart,iend,rfeData,paramGrd,gal.params,origdir){

	rfeDir<-as.character(gal.params$file.io$Values[2])
	biasDir<-as.character(gal.params$file.io$Values[3])
	#adjDir<-file.path(origdir,'Adjusted_data',fsep = .Platform$file.sep)
	#dir.create(adjDir,showWarnings=FALSE)

	rfeFileFormat<-as.character(gal.params$prefix$Values[1])
	meanBiasPrefix<-as.character(gal.params$prefix$Values[2])
	adjPrefix<-as.character(gal.params$prefix$Values[3])

	biasFile<-file.path(biasDir,paste(meanBiasPrefix,'_1.nc',sep=''),fsep = .Platform$file.sep)
	if(!file.exists(biasFile)){
		insert.txt(main.txt.out,"Mean bias coefficients not found",format=TRUE)
		return(NULL)
	}

	xy.dim<-paramGrd$xy.dim
	grd.bsadj <- var.def.ncdf("precip", "mm",xy.dim, -99, longname= " Mean Bias Adjusted RFE", prec="single")

	if(freqData=='daily'){
		adj.dates<-format(seq(as.Date(istart,format='%Y%m%d'),as.Date(iend,format='%Y%m%d'),'day'),'%Y%m%d')
		testfile<-file.path(rfeDir,sprintf(rfeFileFormat,substr(adj.dates,1,4),substr(adj.dates,5,6),substr(adj.dates,7,8)),fsep = .Platform$file.sep)
	}
	if(freqData=='dekadal'){
		adj.dates<-seq(as.Date(istart,format='%Y%m%d'),as.Date(iend,format='%Y%m%d'),'day')
		adj.dates<-paste(format(adj.dates[which(as.numeric(format(adj.dates,'%d'))<=3)],'%Y%m'), as.numeric(format(adj.dates[which(as.numeric(format(adj.dates,'%d'))<=3)],'%d')),sep='')
		testfile<-file.path(rfeDir,sprintf(rfeFileFormat,substr(adj.dates,1,4),substr(adj.dates,5,6),substr(adj.dates,7,7)),fsep = .Platform$file.sep)
	}
	if(freqData=='monthly'){
		adj.dates<-format(seq(as.Date(istart,format='%Y%m%d'),as.Date(iend,format='%Y%m%d'),'month'),'%Y%m')
		testfile<-file.path(rfeDir,sprintf(rfeFileFormat,substr(adj.dates,1,4),substr(adj.dates,5,6)),fsep = .Platform$file.sep)
	}

	existFl<-unlist(lapply(testfile,file.exists))
	if(length(which(existFl))==0){
		insert.txt(main.txt.out,"RFE data not found",format=TRUE)
		return(NULL)
	}

	rfeDataFl<-testfile[existFl]
	adj.dates<-adj.dates[existFl]

	tcl("update","idletasks")
	for (jfl in seq_along(rfeDataFl)){
		if(freqData=='daily'){
			ann<-as.numeric(substr(adj.dates[jfl],1,4))
			iday<-as.numeric(strftime(as.Date(adj.dates[jfl],format='%Y%m%d'),format='%j'))
			ijt<-ifelse(ann%%4==0 & iday>59,iday-1,iday)
		}
		if(freqData=='dekadal'){
			mon<-as.numeric(substr(adj.dates[jfl],5,6))
			dek<- as.numeric(substr(adj.dates[jfl],7,7))
			annual.dek<-expand.grid(dek=1:3,mon=1:12)
			ijt<-which(annual.dek$dek==dek & annual.dek$mon==mon)
		}
		if(freqData=='monthly'){
			ijt<-as.numeric(substr(adj.dates[jfl],5,6))
		}

		rfefl<-rfeDataFl[jfl]
		bsfl<-file.path(biasDir,paste(meanBiasPrefix,'_',ijt,'.nc',sep=''),fsep = .Platform$file.sep)
		#outfl<-file.path(adjDir,paste(adjPrefix,'_',adj.dates[jfl],'.nc',sep=''),fsep = .Platform$file.sep)
		outfl<-file.path(origdir,paste(adjPrefix,'_',adj.dates[jfl],'.nc',sep=''),fsep = .Platform$file.sep)

		nc <- open.ncdf(rfefl)
		rfe.lon <- nc$dim[[rfeData$rfeILon]]$vals
		rfe.lat <- nc$dim[[rfeData$rfeILat]]$vals
		rfe <- get.var.ncdf(nc,varid = rfeData$rfeVarid)
		close.ncdf(nc)
		xo<-order(rfe.lon)
		yo<-order(rfe.lat)
		rfe.lon <-rfe.lon[xo]
		rfe.lat <-rfe.lat[yo]
		rfe<-rfe[xo,yo]
		if(rfeData$rfeILat==1){
			rfe<-matrix(c(rfe),nrow=length(rfe.lon),ncol=length(rfe.lat),byrow=T)
		}

		nc <- open.ncdf(bsfl)
		bisa.lon<-nc$dim[[1]]$vals
		bisa.lat<-nc$dim[[2]]$vals
		bias <- get.var.ncdf(nc,varid = nc$var[[1]]$name)
		close.ncdf(nc)

		##
		rfeObj<-list(x=rfe.lon,y=rfe.lat,z=rfe)
		grdnew<-list(x=bisa.lon,y=bisa.lat)
		newObj<-interp.surface.grid(rfeObj,grdnew)
		rfe<-newObj$z

		##
		rfe.adj <- round(rfe * bias,2)
		rfe.adj[is.na(rfe.adj)] <- -99

		#Save adjusted data
		nc2 <- create.ncdf(outfl,grd.bsadj)
		put.var.ncdf(nc2,grd.bsadj,rfe.adj)
		close.ncdf(nc2)

		insert.txt(main.txt.out,paste("RFE data adjusted successfully:",basename(rfefl)))
		tcl("update")
	}
	return(0)
}

########################################################################################################
###Merging

MergingFunction<-function(mrgRaindat,VarioModel,paramsMRG,origdir){
	freqData<-gal.params$period
	istart<-paramsMRG$istart
	iend<-paramsMRG$iend
	ijGrd<-paramsMRG$ijGrd
	nlon0<-paramsMRG$nlon0
	nlat0<-paramsMRG$nlat0
	xy.dim<-paramsMRG$xy.dim
	outMask<-paramsMRG$outMask
	newlocation.merging<-paramsMRG$newlocation.merging

	##block grid
	sDX <- newlocation.merging@grid@cellsize[1]/2
	dBX <- seq(-sDX, sDX, length.out=4)
	sDY <- newlocation.merging@grid@cellsize[2]/2
	dBY <- seq(-sDY, sDY, length.out=4)
	bGrd <- expand.grid(x=dBX, y=dBY)

	grd.out<-var.def.ncdf("precip", "mm",xy.dim,-99,longname="Merged Station-Satellite Rainfall", prec="single")

	stn.ID<-mrgRaindat$stnData$id
	stn.lon<-mrgRaindat$stnData$lon
	stn.lat<-mrgRaindat$stnData$lat
	stn.dates<-mrgRaindat$stnData$dates
	stn.data<-mrgRaindat$stnData$data
	nstn<-length(stn.lon)

	rfeDir<-as.character(gal.params$file.io$Values[4])
	rfeFileFormat<-as.character(gal.params$prefix$Values[1])
	mrgPrefix<-as.character(gal.params$prefix$Values[2])
	mrgSuffix<-as.character(gal.params$prefix$Values[3])

	# outmrgdir<-file.path(origdir,'Merged_RR',fsep = .Platform$file.sep)
	# dir.create(outmrgdir,showWarnings=FALSE)

	if(freqData=='daily'){
		mrg.dates<-format(seq(as.Date(istart,format='%Y%m%d'),as.Date(iend,format='%Y%m%d'),'day'),'%Y%m%d')
		if(gal.params$NewGrd=='1'){
			testfile<-file.path(rfeDir,sprintf(rfeFileFormat,substr(mrg.dates,1,4),substr(mrg.dates,5,6), substr(mrg.dates,7,8)),fsep = .Platform$file.sep)
		}else{
			testfile<-file.path(rfeDir,paste(rfeFileFormat,'_',mrg.dates,'.nc',sep=''),fsep = .Platform$file.sep)
		}
	}
	if(freqData=='dekadal'){
		mrg.dates<-seq(as.Date(istart,format='%Y%m%d'),as.Date(iend,format='%Y%m%d'),'day')
		mrg.dates<-paste(format(mrg.dates[which(as.numeric(format(mrg.dates,'%d'))<=3)],'%Y%m'), as.numeric(format(mrg.dates[which(as.numeric(format(mrg.dates,'%d'))<=3)],'%d')),sep='')
		if(gal.params$NewGrd=='1'){
			testfile<-file.path(rfeDir,sprintf(rfeFileFormat,substr(mrg.dates,1,4),substr(mrg.dates,5,6), substr(mrg.dates,7,7)),fsep = .Platform$file.sep)
		}else{
			testfile<-file.path(rfeDir,paste(rfeFileFormat,'_',mrg.dates,'.nc',sep=''),fsep = .Platform$file.sep)
		}
	}
	if(freqData=='monthly'){
		mrg.dates<-format(seq(as.Date(paste(istart,'1',sep=''),format='%Y%m%d'), as.Date(paste(iend,'1',sep=''),format='%Y%m%d'),'month'),'%Y%m')
		if(gal.params$NewGrd=='1'){
			testfile<-file.path(rfeDir,sprintf(rfeFileFormat,substr(mrg.dates,1,4),substr(mrg.dates,5,6)), fsep = .Platform$file.sep)
		}else{
			testfile<-file.path(rfeDir,paste(rfeFileFormat,'_',mrg.dates,'.nc',sep=''),fsep = .Platform$file.sep)
		}
	}

	existFl<-unlist(lapply(testfile,file.exists))
	if(length(which(existFl))==0){
		insert.txt(main.txt.out,"RFE data not found",format=TRUE)
		return(NULL)
	}
	rfeDataFl<-testfile[existFl]
	mrg.dates1<-mrg.dates[existFl]

	####

	for (jfl in seq_along(rfeDataFl)){
		if(gal.params$NewGrd=='1'){
			nc <- open.ncdf(rfeDataFl[jfl])
			rfe.lon <- nc$dim[[mrgRaindat$rfeData$rfeILon]]$vals
			rfe.lat <- nc$dim[[mrgRaindat$rfeData$rfeILat]]$vals
			rfe.val <- get.var.ncdf(nc,varid=mrgRaindat$rfeData$rfeVarid)
			close.ncdf(nc)

			xo<-order(rfe.lon)
			rfe.lon<-rfe.lon[xo]
			yo<-order(rfe.lat)
			rfe.lat<-rfe.lat[yo]
			rfe.val<-rfe.val[xo,yo]

			if(mrgRaindat$rfeData$rfeILat==1){
				rfe.val<-matrix(c(rfe.val),nrow=length(rfe.lon),ncol=length(rfe.lat),byrow=T)
			}
		}else{
			nc <- open.ncdf(rfeDataFl[jfl])
			rfe.lon <- nc$dim[[1]]$vals
			rfe.lat <- nc$dim[[2]]$vals
			rfe.val <- get.var.ncdf(nc,varid='precip')
			close.ncdf(nc)
		}

		###Convert to vector
		rfe.vec<-c(rfe.val)

		##Extract RFE at newgrid (if create new one)
		if(gal.params$NewGrd=='1' & gal.params$CreateGrd!='1'){
			###IDW
			grd.rfe <- data.frame(expand.grid(lon=rfe.lon,lat=rfe.lat),rfe=rfe.vec)
			grd.rfe<-grd.rfe[!is.na(grd.rfe$rfe),]
			coordinates(grd.rfe)=~lon+lat
			rfe.tmp<-krige(rfe~1, locations=grd.rfe, newdata=newlocation.merging,nmax=4, debug.level=0)
			rfe.vec<-rfe.tmp$var1.pred
			rfe.vec<-ifelse(rfe.vec<0,0,rfe.vec)
		}

		mrg.dates2<-mrg.dates1[jfl]

		#####Merging
		out.mrg<-mergingProcs(stn.lon,stn.lat,stn.data,stn.dates,ijGrd,rfe.val,rfe.vec,mrg.dates2, newlocation.merging,bGrd)
		dim(out.mrg) <- c(nlon0,nlat0)
		out.mrg[is.na(out.mrg)] <- -99

		#Apply mask for area of interest
		if(!is.null(outMask)) out.mrg[is.na(outMask)] <- -99
		outfl<-file.path(origdir,paste(mrgPrefix,'_',mrg.dates1[jfl],'_',mrgSuffix,'.nc',sep=''),fsep = .Platform$file.sep)
		#outfl<-file.path(outmrgdir,paste(mrgPrefix,'_',mrg.dates1[jfl],'_',mrgSuffix,'.nc',sep=''),fsep = .Platform$file.sep)
		nc2 <- create.ncdf(outfl,grd.out)
		put.var.ncdf(nc2,grd.out,out.mrg)
		close.ncdf(nc2)

		#####
		insert.txt(main.txt.out,paste("Rainfall merging finished successfully:", paste(mrgPrefix,'_',mrg.dates1[jfl],'_',mrgSuffix,'.nc',sep='')))
		tcl("update")
	}
	return(0)
}

########################################################################################################

mergingProcs<-function(stn.lon,stn.lat,stn.data,stn.dates,ijGrd,rfe.val,rfe.vec,mrg.dates2,newlocation.merging,bGrd){
	nmin<-as.numeric(as.character(gal.params$params.int$Values[1]))
	nozero<-as.numeric(as.character(gal.params$params.int$Values[2]))
	max.RnR.dist<-as.numeric(as.character(gal.params$params.int$Values[3]))
	maxdist<-as.numeric(as.character(gal.params$params.int$Values[4]))
	min.nbrs<-as.numeric(as.character(gal.params$params.int$Values[5]))
	max.nbrs<-as.numeric(as.character(gal.params$params.int$Values[6]))
	interpMethod<-as.character(gal.params$params.mrg$Values[1])
	RainNoRain<-as.character(gal.params$params.mrg$Values[2])

	#rfe over stn location
	rfe_gg <- rfe.val[ijGrd]
	gg<-as.numeric(stn.data[stn.dates==mrg.dates2,])
	dff <- gg - rfe_gg

	# Remove extremes differences between gauge and satellite
	q1 <- quantile(dff,0.0001,na.rm=T)
	q2 <- quantile(dff,0.9999, na.rm=T)
	dff[dff < q1] <- NA
	dff[dff > q2] <- NA
	ix<-which(!is.na(dff))

	##Take rfe for Initial values
	out.mrg<-rfe.vec

	if(sum(gg,na.rm=TRUE)>0 & length(ix)>=nmin){

		rr.stn <-data.frame(cbind(stn.lon,jitter(stn.lat),gg,rfe_gg,dff))
		#rr.stn <-data.frame(cbind(stn.lon,stn.lat,gg,rfe_gg,dff))
		rr.stn<-rr.stn[ix,]
		names(rr.stn) <- c("lon", "lat", "gg","rfe","dff")
		coordinates(rr.stn) = ~lon+lat

		#ijx1 <- which(rr.stn$gg >0 & rr.stn$rfe >0)
		ijx1 <- which(rr.stn$gg >0)
		if(length(ijx1)>=nozero){
			grd.newloc<-SpatialPointsDataFrame(coords=newlocation.merging,data=data.frame(rfe=rfe.vec))
			rr.glm <- glm(gg~rfe, rr.stn, family=gaussian)
			rr.stn$res <- residuals(rr.glm)
			pred.rr <- predict(rr.glm, newdata=grd.newloc, se.fit=T)

			if(interpMethod=="IDW"){
				grd.rr<- krige(res~1, locations=rr.stn,newdata=newlocation.merging,block=bGrd,nmin=min.nbrs,nmax=max.nbrs,maxdist=maxdist, debug.level=0)
				res.pred<-grd.rr$var1.pred
			}else if(interpMethod=="Kriging"){
				grd.rr<-try(autoKrige(res~1,input_data=rr.stn,new_data=newlocation.merging,model=VarioModel,block=bGrd,nmin=min.nbrs,nmax=max.nbrs,maxdist=maxdist, debug.level=0), silent=TRUE)
				is.okKR <- !inherits(grd.rr, "try-error")
				if(is.okKR){
					res.pred<-grd.rr$krige_output$var1.pred
				}else{
					grd.rr<- krige(res~1, locations=rr.stn,newdata=newlocation.merging,block=bGrd,nmin=min.nbrs,nmax=max.nbrs,maxdist=maxdist, debug.level=0)
					res.pred<-grd.rr$var1.pred
				}
			}
			out.mrg<- as.numeric(res.pred+pred.rr$fit)
			out.mrg<-ifelse(out.mrg<0,0,out.mrg)
		}else{
			grd.rr <- idw(dff~1,locations=rr.stn,newdata=newlocation.merging,block=bGrd,nmin=min.nbrs,nmax=max.nbrs,maxdist=maxdist,debug.level=0)
			out.mrg<- grd.rr$var1.pred + rfe.vec
			out.mrg<-ifelse(out.mrg<0,0,out.mrg)
		}
		# Take RFE for areas where interpolation/merging was not possible
		ix <- which(is.na(out.mrg))
		out.mrg[ix] <- rfe.vec[ix]

		cells<-newlocation.merging@grid

		##smoothing???
		img.mrg<-as.image(out.mrg, x= coordinates(newlocation.merging), nx=cells@cells.dim[1], ny=cells@cells.dim[2])
		smooth.mrg<-image.smooth(img.mrg, theta= 0.09)
		out.mrg <-round(c(smooth.mrg$z),1)

		#Rain-non-Rain Mask
		if(RainNoRain!='None') {
			rr.stn$rnr <- ifelse(rr.stn$gg >=1,1,0)
			if (RainNoRain=='Gauge') {#Gauge only
				rnr.grd<-krige(rnr~1, locations=rr.stn, newdata=newlocation.merging,block=bGrd,nmin=min.nbrs,nmax=max.nbrs,maxdist=max.RnR.dist,debug.level=0)
				rnr.pred<-ifelse(is.na(rnr.grd$var1.pred),1,rnr.grd$var1.pred)
				RnoR<-round(rnr.pred)
			} else if(RainNoRain=='Satellite') {#Satellite only
				RnoR<-ifelse(rfe.vec >=1, 1, 0)
				RnoR[is.na(RnoR)]<-1
			} else if(RainNoRain=='GaugeSatellite') {
				rfe.rnr <- ifelse(rfe.vec >=1, 1, 0)
				rnr.grd<-krige(rnr~1, locations=rr.stn, newdata=newlocation.merging,block=bGrd,nmin=min.nbrs,nmax=max.nbrs,maxdist=max.RnR.dist, debug.level=0)
				RnoR <-rnr.grd$var1.pred
				RnoR<-round(RnoR)
				ix <- which(is.na(RnoR))
				RnoR[ix] <- rfe.rnr[ix]
				RnoR[is.na(RnoR)]<-1

				##smoothing???
				img.RnoR<-as.image(RnoR, x= coordinates(newlocation.merging), nx=cells@cells.dim[1], ny=cells@cells.dim[2])
				smooth.RnoR<-image.smooth(img.RnoR, theta= 0.08)
				RnoR <-round(c(smooth.RnoR$z))
			}
			out.mrg <- out.mrg * RnoR
		}
	}else out.mrg <- out.mrg

	return(out.mrg)
}



###Downscaling JRA
GlmCoefDownscaling<-function(paramsGlmCoef){
	coefdownTempdat<-paramsGlmCoef$coefdownTempdat
	dem.stn<-paramsGlmCoef$dem.stn
	origdir<-paramsGlmCoef$origdir

	year1<-as.numeric(as.character(gal.params$dates.coef$Values[1]))
	year2<-as.numeric(as.character(gal.params$dates.coef$Values[2]))
	stn.dates<-coefdownTempdat$stnData$dates
	stn.data<-coefdownTempdat$stnData$data

	years<- as.numeric(substr(stn.dates,1,4))
	iyrCoef<- years>=year1 & years<=year2
	stn.data<-stn.data[iyrCoef,]
	stn.dates<-stn.dates[iyrCoef]
	months <- as.numeric(substr(stn.dates,5,6))
	coef <-array(NA, c(12,2))
	for (m in 1:12){
		ix <- which(months==m)
		n <- length(ix)
		tt <- as.vector(t(as.matrix(stn.data[ix,])))
		z <- rep(dem.stn,n)
		glm.dat <- data.frame(dem=z, tt=tt)
		glm.dat <- na.omit(glm.dat)
		if(length(glm.dat[,1])==0) next #skip if all data NA
		moy<-unname(apply(glm.dat,2,mean))
		ect<-unname(apply(glm.dat,2,sd))
		if(ect[1]==0 | ect[2]==0) next  #skip if variance null
		glm.dat<-t((t(glm.dat)-moy)/ect)
		glm.dat[glm.dat[,2]< -3,2]<- -3
		glm.dat<-as.data.frame(glm.dat)
		#glm.tt <- glm(tt~dem, data=glm.dat)
		glm.tt <- lm(tt~dem, data=glm.dat)
		coef[m,1] <- glm.tt$coefficients[1]
		coef[m,2] <- glm.tt$coefficients[2]
	}

	##Take the next or previous month non-NA, loop until there are no more
	ina<-which(is.na(coef[,1]))
	if(length(ina)>0){
		tmp<-coef
		while(length(ina)>0){
			coef2<-rbind(tmp,tmp)
			shiftUp<-rbind(coef2[-1,],coef2[1,])
			tmp[ina,]<-shiftUp[ina,]
			tmp[-ina,]<-coef[-ina,]
			ina<-which(is.na(tmp[,1]))
			if(length(ina)>0){
				tmp1<-tmp
				shiftDown<-rbind(coef2[24,],coef2[-24,])
				tmp1[ina,]<-shiftDown[ina,]
				tmp1[-ina,]<-tmp[-ina,]
				tmp<-tmp1
				ina<-which(is.na(tmp[,1]))
			}
		}
		coef<-tmp
	}

	outfile<-file.path(origdir,'STN_DEM_GLM_COEF.txt',fsep = .Platform$file.sep)
	write.table(coef, file = outfile, col.names =FALSE, row.names=FALSE)
	return(0)
}

#################################################
ReanalysisDownscaling<-function(paramsDownscl){
	istart<-paramsDownscl$istart
	iend<-paramsDownscl$iend
	dem.reanal<-paramsDownscl$dem.reanal
	dem<-paramsDownscl$dem
	reanalInfo<-paramsDownscl$reanalInfo
	newlocation.merging<-paramsDownscl$newlocation.merging
	xy.dim<-paramsDownscl$xy.dim
	nlon0<-paramsDownscl$nlon0
	nlat0<-paramsDownscl$nlat0
	origdir<-paramsDownscl$origdir

	freqData<-gal.params$period
	coef<-read.table(as.character(gal.params$file.io$Values[1]))

	dirJRA<-as.character(gal.params$file.io$Values[4])
	rfe.file.format<-as.character(gal.params$IO.file.format$Values[1])
	downPrefix<-as.character(gal.params$IO.file.format$Values[2])

	min.nbrs<-as.numeric(as.character(gal.params$params.int$Values[1]))
	max.nbrs<-as.numeric(as.character(gal.params$params.int$Values[2]))
	max.dst<-as.numeric(as.character(gal.params$params.int$Values[3]))

	#Defines netcdf output
	out.tt <- var.def.ncdf("temp", "DegC",xy.dim, -99, longname= "Dwonscaled temperature from reanalysis data", prec="single")

	##Get all Reanalysis Files
	if(freqData=='daily'){
		down.dates<-format(seq(as.Date(istart,format='%Y%m%d'),as.Date(iend,format='%Y%m%d'),'day'),'%Y%m%d')
		xx1<-substr(down.dates,1,4)
		xx2<-substr(down.dates,5,6)
		xx3<-substr(down.dates,7,8)
		testfile<-file.path(dirJRA,sprintf(rfe.file.format,xx1,xx2,xx3),fsep = .Platform$file.sep)
	}
	if(freqData=='dekadal'){
		down.dates<-seq(as.Date(istart,format='%Y%m%d'),as.Date(iend,format='%Y%m%d'),'day')
		down.dates<-paste(format(down.dates[which(as.numeric(format(down.dates,'%d'))<=3)],'%Y%m'), as.numeric(format(down.dates[which(as.numeric(format(down.dates,'%d'))<=3)],'%d')),sep='')
		xx1<-substr(down.dates,1,4)
		xx2<-substr(down.dates,5,6)
		xx3<-substr(down.dates,7,7)
		testfile<-file.path(dirJRA,sprintf(rfe.file.format,xx1,xx2,xx3),fsep = .Platform$file.sep)
	}
	if(freqData=='monthly'){
		down.dates<-format(seq(as.Date(istart,format='%Y%m%d'),as.Date(iend,format='%Y%m%d'),'month'),'%Y%m')
		xx1<-substr(down.dates,1,4)
		xx2<-substr(down.dates,5,6)
		testfile<-file.path(dirJRA,sprintf(rfe.file.format,xx1,xx2),fsep = .Platform$file.sep)
	}
	existFl<-unlist(lapply(testfile,file.exists))
	if(length(which(existFl))==0){
		insert.txt(main.txt.out,"Reanalysis data not found",format=TRUE)
		return(NULL)
	}	
	ReanalDataFl<-testfile[existFl]
	down.dates<-down.dates[existFl]

	######
	tcl("update","idletasks")
	##Use ParApply
	ret<-lapply(seq_along(ReanalDataFl),function(jfl){
		outfl<-file.path(origdir,paste(downPrefix,'_',down.dates[jfl],'.nc',sep=''),fsep = .Platform$file.sep)
		rfefl <-ReanalDataFl[jfl]
		nc <- open.ncdf(rfefl)
		tt.lon <- nc$dim[[reanalInfo$rfeILon]]$vals
		tt.lat <- nc$dim[[reanalInfo$rfeILat]]$vals
		tt.val <- get.var.ncdf(nc, varid=reanalInfo$rfeVarid)
		close.ncdf(nc)
		xo<-order(tt.lon)
		tt.lon<-tt.lon[xo]
		yo<-order(tt.lat)
		tt.lat<-tt.lat[yo]
		tt.val<-tt.val[xo,yo]
		if(reanalInfo$rfeILat==1){
			tt.val<-matrix(c(tt.val),nrow=length(tt.lon),ncol=length(tt.lat),byrow=T)
		}

		#Downscale dekadl TT using elevation
		tt <- as.vector(tt.val)
		tt.mean <- mean(tt,na.rm=T)
		tt.sd <- sd(tt,na.rm=T)
		tt <- (tt-tt.mean)/tt.sd
		z.mean <- mean(dem.reanal,na.rm=T)
		z.sd <- sd(dem.reanal,na.rm=T)
		z <- (dem.reanal-z.mean)/z.sd
		tt.xyz <-data.frame(expand.grid(lon=reanalInfo$lon,lat=reanalInfo$lat),z, tt)
		coordinates(tt.xyz) = ~lon+lat
		ix <- which(!is.na(tt.xyz$z))
		tt.xyz <- tt.xyz[ix, ]

		mon<-as.numeric(substr(down.dates[jfl],5,6))
		a <- coef[mon,2]
		b <- coef[mon,1]
		tt.est <- a*tt.xyz$z + b
		tt.xyz$res <- tt.xyz$tt - tt.est

		grd.loc1 <-newlocation.merging
		demStdz <-(dem-mean(dem,na.rm=T))/sd(dem,na.rm=T)
		grd.loc1<-SpatialPointsDataFrame(coords=grd.loc1, data=data.frame(dem=demStdz))
		tt.est.grd <- a*grd.loc1$dem + b

		#Interpoate residuals at new grid
		grd.temp <- idw(res~1, locations=tt.xyz, newdata=grd.loc1,nmin=min.nbrs,nmax=max.nbrs,maxdist=max.dst,idp=2.0,debug.level=0)
		p <- grd.temp$var1.pred + tt.est.grd
		p <- p*tt.sd + tt.mean

		#####
		p[is.na(p)]<- -99
		dim(p) <- c(nlon0,nlat0)

		nc2 <- create.ncdf(outfl,out.tt)
		put.var.ncdf(nc2,out.tt,p)
		close.ncdf(nc2)
		insert.txt(main.txt.out,paste("Downscaling  Reanalysis finished:",basename(rfefl)))
		tcl("update")
		return(0)
	})
	return(0)
}

#######################################################################################
# Extract model values at all station locations

ExtractReanal2Stn<-function(ijGrd,nstn,coef.dates){
	insert.txt(main.txt.out,'Extract dwonscaled data at guage locations ')
	tcl("update")
	freqData<-gal.params$period
	downscaledDir<-as.character(gal.params$file.io$Values[3])
	downPrefix<-as.character(gal.params$prefix$Values[1])

	if(freqData=='daily'){
		bias.dates<-format(seq(as.Date(paste(coef.dates[1],'0101',sep=''),format='%Y%m%d'), as.Date(paste(coef.dates[2],'1231',sep=''),format='%Y%m%d'),'day'),'%Y%m%d')
	}
	if(freqData=='dekadal'){
		bias.dates<-seq(as.Date(paste(coef.dates[1],'011',sep=''),format='%Y%m%d'), as.Date(paste(coef.dates[2],'123',sep=''),format='%Y%m%d'),'day')
		bias.dates<-paste(format(bias.dates[which(as.numeric(format(bias.dates,'%d'))<=3)],'%Y%m'), as.numeric(format(bias.dates[which(as.numeric(format(bias.dates,'%d'))<=3)],'%d')),sep='')
	}
	if(freqData=='monthly'){
		bias.dates<-format(seq(as.Date(paste(coef.dates[1],'011',sep=''),format='%Y%m%d'), as.Date(paste(coef.dates[2],'1231',sep=''),format='%Y%m%d'),'month'),'%Y%m')
	}
	testfile<-file.path(downscaledDir,paste(downPrefix,'_',bias.dates,'.nc',sep=''),fsep = .Platform$file.sep)
	model_stn<-data.frame(matrix(NA,nrow=length(bias.dates),ncol=nstn))

	existFl<-unlist(lapply(testfile,file.exists))
	if(length(which(existFl))==0){
		insert.txt(main.txt.out,"Dwonscaled data data not found",format=TRUE)
		return(NULL)
	}
	downDataFl<-testfile[existFl]
	bias.dates1<-bias.dates[existFl]

	for (jfl in seq_along(downDataFl)){
		nc <- open.ncdf(downDataFl[jfl])
		model <- get.var.ncdf(nc,varid = nc$var[[1]]$name)
		close.ncdf(nc)
		model_stn[which(bias.dates==bias.dates1[jfl]),]<-model[ijGrd]
	}
	insert.txt(main.txt.out,'Done! ')
	return(model_stn)
}

# #######################################
calcBiasTemp<-function(i,ix1,stn.data,model_stn){
	stn.val <- as.numeric(stn.data[ix1,i])
	stn.mod<-as.numeric(model_stn[ix1,i])
	ix<- which(!is.na(stn.val) & !is.na(stn.mod))
	bs <- NA
	if(length(ix)>0){
		bs <- sum(stn.val[ix])/sum(stn.mod[ix])
		if(is.nan(bs)) bs <- 1    # 0/0
		if(is.infinite(bs)) bs <- 1.5  # n/0
		if(bs == 0) bs <- 0.5  # 0/n
		if(bs>3) bs<-3
		if(bs <0) bs <-NA
	}
	return(bs)
}

###############
Variogrm_modeling<-function(bias.df,stn.lon,stn.lat){
	bias.df<-bias.df[!is.na(bias.df$bias),]
	coordinates(bias.df)<-~lat + lon
	vgm1 <- variogram(bias~1, bias.df, width=0.10)
	vgm1$gamma[1] <- 0
	vgm1$dist[1] <- ifelse(vgm1$dist[1]==0,1E-6,vgm1$dist[1]) #remove zerodist
	lvgm <- length(vgm1$gamma)
	vgm1$gamma[lvgm] <- vgm1$gamma[lvgm-1] # To remove values computed from small np
	psl0 <- max(vgm1$gamma)
	#nug=0
	rng <- sqrt(max(dist(cbind(stn.lat, stn.lon))))/4
	null.vgm <- vgm(1,"Exp", rng) # initial parameters
	vgm_model <- fit.variogram(vgm1, model=null.vgm)
	#plot(vgm1,model=vgm_model)
	return(vgm_model)
}

##########################################3
###Mean Bias calcul for Bias-kriging method
ComputeMeanBias<-function(paramsBias){
	stnDatas<-paramsBias$stnDatas
	model_stn<-paramsBias$model_stn
	coef.dates<-paramsBias$coef.dates
	xy.dim<-paramsBias$xy.dim
	nlon0<-paramsBias$nlon0
	nlat0<-paramsBias$nlat0
	newlocation.merging<-paramsBias$newlocation.merging
	dirBias<-paramsBias$dirBias

	min.nbrs<-as.numeric(as.character(gal.params$params.int$Values[1]))
	max.nbrs<-as.numeric(as.character(gal.params$params.int$Values[2]))
	max.dst<-as.numeric(as.character(gal.params$params.int$Values[3]))

	freqData<-gal.params$period
	meanBiasPrefix<-as.character(gal.params$prefix$Values[2])

	stn.lon<-stnDatas$lon
	stn.lat<-stnDatas$lat
	stn.dates<-stnDatas$dates
	stn.data<-stnDatas$data
	nstn<-length(stn.lon)

	#####
	if(freqData=='daily'){
		bias.dates<-format(seq(as.Date(paste(coef.dates[1],'0101',sep=''),format='%Y%m%d'), as.Date(paste(coef.dates[2],'1231',sep=''),format='%Y%m%d'),'day'),'%Y%m%d')
		endmon<-c(31,28,31,30,31,30,31,31,30,31,30,31)
		bias <- array(data=NA, c(365,nstn))
		vtimes<-cbind(unlist(sapply(endmon,function(j) 1:j)),rep(1:12,endmon))

		ibsdt<-bias.dates%in%stn.dates
		bsdates<-bias.dates[ibsdt]
		model_stn<-model_stn[ibsdt,,drop=F]
		istdt<-stn.dates%in%bias.dates
		stn.data<-stn.data[istdt,,drop=F]

		# Compute bias
		if(length(bsdates)>0){
			for (i in 1:nstn){
				for (nt in 1:365){
					ix1<-which(as.numeric(substr(bsdates,7,8))==vtimes[nt,1] & as.numeric(substr(bsdates,5,6))==vtimes[nt,2])
					bias[nt,i] <- calcBiasTemp(i,ix1,stn.data,model_stn)
				}
			}
		}
	}
	#####
	if(freqData=='dekadal'){
		bias.dates<-seq(as.Date(paste(coef.dates[1],'011',sep=''),format='%Y%m%d'), as.Date(paste(coef.dates[2],'123',sep=''),format='%Y%m%d'),'day')
		bias.dates<-paste(format(bias.dates[which(as.numeric(format(bias.dates,'%d'))<=3)],'%Y%m'), as.numeric(format(bias.dates[which(as.numeric(format(bias.dates,'%d'))<=3)],'%d')),sep='')
		bias <- array(data=NA, c(36,nstn))
		vtimes<-expand.grid(1:3,1:12)

		ibsdt<-bias.dates%in%stn.dates
		bsdates<-bias.dates[ibsdt]
		model_stn<-model_stn[ibsdt,,drop=F]
		istdt<-stn.dates%in%bias.dates
		stn.data<-stn.data[istdt,,drop=F]

		# Compute bias
		if(length(bsdates)>0){
			for (i in 1:nstn){
				for (nt in 1:36){
					ix1<-which(as.numeric(substr(bsdates,7,7))==vtimes[nt,1] & as.numeric(substr(bsdates,5,6))==vtimes[nt,2])
					bias[nt,i] <- calcBiasTemp(i,ix1,stn.data,model_stn)
				}
			}
		}
	}
	#####
	if(freqData=='monthly'){
		bias.dates<-format(seq(as.Date(paste(coef.dates[1],'011',sep=''),format='%Y%m%d'), as.Date(paste(coef.dates[2],'1231',sep=''),format='%Y%m%d'),'month'),'%Y%m')
		bias <- array(data=NA, c(12,nstn))
		vtimes<-c(1:12)

		ibsdt<-bias.dates%in%stn.dates
		bsdates<-bias.dates[ibsdt]
		model_stn<-model_stn[ibsdt,,drop=F]
		istdt<-stn.dates%in%bias.dates
		stn.data<-stn.data[istdt,,drop=F]

		# Compute bias
		if(length(bsdates)>0){
			for (i in 1:nstn){
				for (nt in 1:12){
					ix1<-which(as.numeric(substr(bsdates,5,6))==vtimes[nt])
					bias[nt,i] <- calcBiasTemp(i,ix1,stn.data,model_stn)
				}
			}
		}
	}

	############
	# Grid Bias
	##block grid
	sDX <- newlocation.merging@grid@cellsize[1]/2
	dBX <- seq(-sDX, sDX, length.out=4)
	sDY <- newlocation.merging@grid@cellsize[2]/2
	dBY <- seq(-sDY, sDY, length.out=4)
	bGrd <- expand.grid(x=dBX, y=dBY)

	#Defines netcdf output
	grd.bs <- var.def.ncdf("grid", "",xy.dim, NA, longname= "Gridded Station/Reanalysis Bias", prec="single")

	################################################
	if(freqData=='daily') ntimes<-365
	if(freqData=='dekadal') ntimes<-36
	if(freqData=='monthly') ntimes<-12

	tcl("update","idletasks")
	for(ij in 1:ntimes){
		bias.stn <- data.frame(bias=bias[ij,],lon=stn.lon,lat=stn.lat)
		ix <- which(!is.na(bias.stn$bias))
		if(length(ix)>8){
			bias.stn <- bias.stn[ix,]
			coordinates(bias.stn) =~lon + lat
			gbias<-krige(bias~1,locations=bias.stn,newdata=newlocation.merging,block=bGrd,nmin=min.nbrs,nmax=max.nbrs,maxdist=max.dst,debug.level=0)
			grd.bias <- gbias$var1.pred
			grd.bias[is.na(grd.bias)]<-1
			#smoothing
			imgbs<-as.image( grd.bias, x= coordinates(gbias), nx=nlon0, ny=nlat0)
			smbias<-image.smooth(imgbs, theta= 0.08)
			grd.bias <- smbias$z
		}else{
			grd.bias <-rep(1,nlon0*nlat0)
			dim(grd.bias) <- c(nlon0, nlat0)
		}

		outfl <- file.path(dirBias,paste(meanBiasPrefix,'_',ij,'.nc',sep=''),fsep = .Platform$file.sep)
		nc2 <- create.ncdf(outfl,grd.bs)
		put.var.ncdf(nc2,grd.bs,grd.bias)
		close.ncdf(nc2)
		insert.txt(main.txt.out,paste("Computing mean bias finished:",paste(meanBiasPrefix,'_',ij,'.nc',sep='')))
		tcl("update")
	}
	return(0)
}

#############################################
#### Regession coefficients for Regression-QM method
ComputeRegCoeff<-function(paramsRegQM){
	stn.data<-paramsRegQM$stn.data
	stn.dates<-paramsRegQM$stn.dates
	coef.dates<-paramsRegQM$coef.dates
	dem.stn<-paramsRegQM$dem.stn
	model_stn<-paramsRegQM$model_stn
	origdir<-paramsRegQM$origdir

	years<- as.numeric(substr(stn.dates,1,4))
	iyrCoef<- years>=coef.dates[1] & years<=coef.dates[2]
	stn.data<-stn.data[iyrCoef,]
	stn.dates<-stn.dates[iyrCoef]
	months <- as.numeric(substr(stn.dates,5,6))

	dem_stn<-matrix(dem.stn,nrow=nrow(model_stn),ncol=length(dem.stn),byrow=TRUE)
	dem_stn <- jitter(dem_stn)
	dem_stn[dem_stn<0] <-1

	# Compute coefficents
	coef <- data.frame(array(data=NA, c(12,5)))
	for (m in 1:12){
		ix <- which(m==months)
		tt <- as.vector(t(as.matrix(stn.data[ix,])))
		x1 <- as.vector(t(as.matrix(model_stn[ix,])))
		x2 <- as.vector(t(as.matrix(dem_stn[ix,])))

		ix <- which(!is.na(tt)  & x1>=0)
		tt <- c(tt[ix])

		coef[m,1] <- mean(tt)
		coef[m,2] <- sd(tt)
		if(!is.na(coef[m,1]) & is.na(coef[m,2])){
			insert.txt(main.txt.out,"Computing regression coefficients are stopped",format=TRUE)
			insert.txt(main.txt.out,paste('Mean and standard deviation of',format(ISOdate(2014,m,1),"%B"), 'are NA'),format=TRUE)
			return(NULL)
		}

		x1 <- c(x1[ix])
		x2 <- c(x2[ix])
		x1 <- (x1-mean(x1))/sd(x1)
		x2 <- (x2-mean(x2))/sd(x2)
		tt <- (tt-mean(tt))/sd(tt)
		nmatch <- length(ix)
		if(nmatch >=7){
			reg.data <- data.frame(model=x1,dem=x2, tt=tt)
			glm.tt <- glm(tt~model+dem, data=reg.data)
			coef[m,3] <- glm.tt$coefficients[1]
			coef[m,4] <- glm.tt$coefficients[2]
			coef[m,5] <- glm.tt$coefficients[3]
		}
	}
	coef <- round(coef, digits=6)
	names(coef)<- c("mean", "sd","const", "model", "dem")
	outfile<-file.path(origdir,'STN_MODEL_DEM_REGRESSION_COEF.txt',fsep = .Platform$file.sep)
	write.table(coef, file = outfile, col.names = TRUE, row.names=FALSE)
	insert.txt(main.txt.out,"Computing regression coefficients finished")
	return(0)
}

##########################################
##Adjust downscaled data

AjdReanalMeanBias<-function(paramsAdjBs){
	istart<-paramsAdjBs$istart
	iend<-paramsAdjBs$iend
	dem<-paramsAdjBs$dem
	xy.dim<-paramsAdjBs$xy.dim
	nlon0<-paramsAdjBs$nlon0
	nlat0<-paramsAdjBs$nlat0
	downscaledDir<-paramsAdjBs$downscaledDir
	biasDirORFile<-paramsAdjBs$biasDirORFile
	adjDir<-paramsAdjBs$adjDir
	downPrefix<-paramsAdjBs$downPrefix
	meanBiasPrefix<-paramsAdjBs$meanBiasPrefix
	adjPrefix<-paramsAdjBs$adjPrefix

	freqData<-gal.params$period
	grd.bsadj <- var.def.ncdf("temp", "DegC",xy.dim, -99, longname= "Mean Bias Adjusted Reanalysis", prec="single")

	##Get all downscaled Files

	if(freqData=='daily'){
		adj.dates<-format(seq(as.Date(istart,format='%Y%m%d'),as.Date(iend,format='%Y%m%d'),'day'),'%Y%m%d')
		testfile<-file.path(downscaledDir,paste(downPrefix,'_',adj.dates,'.nc',sep=''),fsep = .Platform$file.sep)
	}
	if(freqData=='dekadal'){
		adj.dates<-seq(as.Date(istart,format='%Y%m%d'),as.Date(iend,format='%Y%m%d'),'day')
		adj.dates<-paste(format(adj.dates[which(as.numeric(format(adj.dates,'%d'))<=3)],'%Y%m'), as.numeric(format(adj.dates[which(as.numeric(format(adj.dates,'%d'))<=3)],'%d')),sep='')
		testfile<-file.path(downscaledDir,paste(downPrefix,'_',adj.dates,'.nc',sep=''),fsep = .Platform$file.sep)
	}
	if(freqData=='monthly'){
		adj.dates<-format(seq(as.Date(istart,format='%Y%m%d'),as.Date(iend,format='%Y%m%d'),'month'),'%Y%m')
		testfile<-file.path(downscaledDir,paste(downPrefix,'_',adj.dates,'.nc',sep=''),fsep = .Platform$file.sep)
	}

	existFl<-unlist(lapply(testfile,file.exists))
	if(length(which(existFl))==0){
		insert.txt(main.txt.out,"Downscaled data not found",format=TRUE)
		return(NULL)
	}	
	downDataFl<-testfile[existFl]
	adj.dates<-adj.dates[existFl]

	tcl("update","idletasks")
	ret<-lapply(seq_along(downDataFl),function(jfl){
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

		downfl<-downDataFl[jfl]
		bsfl<-file.path(biasDirORFile,paste(meanBiasPrefix,'_',ijt,'.nc',sep=''),fsep = .Platform$file.sep)
		outfl<-file.path(adjDir,paste(adjPrefix,'_',adj.dates[jfl],'.nc',sep=''),fsep = .Platform$file.sep)

		nc <- open.ncdf(downfl)
		temp <- get.var.ncdf(nc,varid = nc$var[[1]]$name)
		close.ncdf(nc)

		nc <- open.ncdf(bsfl)
		bias <- get.var.ncdf(nc,varid = nc$var[[1]]$name)
		close.ncdf(nc)

		temp.adj <- round(temp * bias,2)

		# Replace sea surface temperatures with that of the uncorrected reanalysis
		dem.sea<-matrix(dem,ncol=nlat0,nrow=nlon0)
		ix <- which(dem.sea<=0)
		temp.adj[ix] <- temp[ix]
		temp.adj[is.na(temp.adj)] <- -99

		#Save adjusted Reanalysis
		nc2 <- create.ncdf(outfl,grd.bsadj)
		put.var.ncdf(nc2,grd.bsadj,temp.adj)
		close.ncdf(nc2)

		insert.txt(main.txt.out,paste("Downscaled data adjusted successfully:",paste(downPrefix,'_',adj.dates[jfl],'.nc',sep='')))
		tcl("update")
		return(0)
	})
	return(0)
}

####################################

AjdReanalpmm<-function(paramsAdjBs){
	istart<-paramsAdjBs$istart
	iend<-paramsAdjBs$iend
	stnDatas<-paramsAdjBs$stnDatas
	ijGrd<-paramsAdjBs$ijGrd
	dem<-paramsAdjBs$dem
	xy.dim<-paramsAdjBs$xy.dim
	nlon0<-paramsAdjBs$nlon0
	nlat0<-paramsAdjBs$nlat0
	coefReg<-paramsAdjBs$coefReg
	downscaledDir<-paramsAdjBs$downscaledDir
	adjDir<-paramsAdjBs$adjDir
	downPrefix<-paramsAdjBs$downPrefix
	adjPrefix<-paramsAdjBs$adjPrefix

	freqData<-gal.params$period
	stn.data<-stnDatas$data
	stn.dates<-stnDatas$dates
	years<- as.numeric(substr(stn.dates,1,4))
	months <- as.numeric(substr(stn.dates,5,6))

	grd.bsadj <- var.def.ncdf("temp", "DegC",xy.dim, -99, longname= "Regression-QM Adjusted Reanalysis", prec="single")

	##Get all downscaled Files
	if(freqData=='daily'){
		adj.dates<-format(seq(as.Date(istart,format='%Y%m%d'),as.Date(iend,format='%Y%m%d'),'day'),'%Y%m%d')
		testfile<-file.path(downscaledDir,paste(downPrefix,'_',adj.dates,'.nc',sep=''),fsep = .Platform$file.sep)
	}
	if(freqData=='dekadal'){
		adj.dates<-seq(as.Date(istart,format='%Y%m%d'),as.Date(iend,format='%Y%m%d'),'day')
		adj.dates<-paste(format(adj.dates[which(as.numeric(format(adj.dates,'%d'))<=3)],'%Y%m'),
		as.numeric(format(adj.dates[which(as.numeric(format(adj.dates,'%d'))<=3)],'%d')),sep='')
		testfile<-file.path(downscaledDir,paste(downPrefix,'_',adj.dates,'.nc',sep=''),fsep = .Platform$file.sep)
	}
	if(freqData=='monthly'){
		adj.dates<-format(seq(as.Date(istart,format='%Y%m%d'),as.Date(iend,format='%Y%m%d'),'month'),'%Y%m')
		testfile<-file.path(downscaledDir,paste(downPrefix,'_',adj.dates,'.nc',sep=''),fsep = .Platform$file.sep)
	}

	existFl<-unlist(lapply(testfile,file.exists))
	if(length(which(existFl))==0){
		insert.txt(main.txt.out,"Downscaled data not found",format=TRUE)
		return(NULL)
	}
	downDataFl<-testfile[existFl]
	adj.dates<-adj.dates[existFl]

	tcl("update","idletasks")
	ret<-lapply(seq_along(downDataFl),function(jfl){
		yrs<-as.numeric(substr(adj.dates[jfl],1,4))
		mon<-as.numeric(substr(adj.dates[jfl],5,6))

		if(freqData=='daily'){
			dayss <- as.numeric(substr(stn.dates,7,8))
			day<- as.numeric(substr(adj.dates[jfl],7,8))
			ijt<-which(dayss==day & months==mon & years==yrs)
		}
		if(freqData=='dekadal'){
			dekads <- as.numeric(substr(stn.dates,7,7))
			dek<- as.numeric(substr(adj.dates[jfl],7,7))
			ijt<-which(dekads==dek & months==mon & years==yrs)
		}
		if(freqData=='monthly'){
			ijt<-which(months==mon & years==yrs)
		}

		stn.mn <- coefReg[mon,1]
		stn.sd <- coefReg[mon,2]
		intercept <- coefReg[mon,3]
		slop.model <- coefReg[mon,4]
		slop.dem <- coefReg[mon,5]

		stn.tt <- c(t(stn.data[ijt, ]))
		# Remove extremes station values
		q1 <- quantile(stn.tt,0.005,na.rm=T)
		q2 <- quantile(stn.tt,0.999, na.rm=T)
		stn.tt[(stn.tt < q1) | (stn.tt>q2)] <- NA
		
		ix <- which(!is.na(stn.tt))
		tt <- stn.tt[ix]

		downfl<-downDataFl[jfl]
		outfl<-file.path(adjDir,paste(adjPrefix,'_',adj.dates[jfl],'.nc',sep=''),fsep = .Platform$file.sep)
		
		nc <- open.ncdf(downfl)
		temp <- get.var.ncdf(nc,varid = nc$var[[1]]$name)
		close.ncdf(nc)

		mn <- mean(temp, na.rm=T)
		sd <- sd(temp, na.rm=T)
		x1 <- c((temp-mn)/sd)
		x2 <- (dem-mean(dem))/sd(dem)
		temp.adj <- slop.model * x1  + slop.dem * x2 + intercept
		temp.adj <- temp.adj*stn.sd + stn.mn
		temp.adj <- round(temp.adj, digits=2 )

		temp.adj.stn <- temp.adj[ijGrd]
		Fy <- ecdf(temp.adj.stn)
		prob <- Fy(temp.adj)
		temp.adj2 <- quantile(tt,prob)
		temp.adj2 <- temp.adj2 * (mean(tt)/mean(temp.adj.stn)) #change
		temp.adj2 <- matrix(temp.adj2, nrow=nlon0, ncol=nlat0)

		# Replace sea surface temperatures with that of the uncorrected reanalysis
		dem.sea<-matrix(dem,ncol=nlat0,nrow=nlon0)
		ix <- which(dem.sea<=0)
		temp.adj2[ix] <- temp[ix]
		temp.adj2[is.na(temp.adj2)] <- -99

		#Save adjusted Reanalysis
		nc2 <- create.ncdf(outfl,grd.bsadj)
		put.var.ncdf(nc2,grd.bsadj,temp.adj)
		close.ncdf(nc2)

		insert.txt(main.txt.out,paste("Downscaled data adjusted successfully:",paste(downPrefix,'_',adj.dates[jfl],'.nc',sep='')))
		tcl("update")
		return(0)
	})
	return(0)
}

#######################################################################################

MergeTemp<-function(mrgParam){
	stn.lon<-mrgParam$mrgData$stnData$lon
	stn.lat<-mrgParam$mrgData$stnData$lat
	stn.dates<-mrgParam$mrgData$stnData$dates
	stn.data<-mrgParam$mrgData$stnData$data

	newlocation.merging<-mrgParam$mrgData$newlocation.merging
	outMask<-mrgParam$mrgData$outMask

	nlon0<-mrgParam$mrgInfo$nlon0
	nlat0<-mrgParam$mrgInfo$nlat0
	ijGrd<-mrgParam$mrgInfo$ijGrd
	xy.dim<-mrgParam$mrgInfo$xy.dim
	VarioModel<-mrgParam$mrgInfo$VarioModel

	adjDir<-mrgParam$dirs[1]
	mrgDir<-mrgParam$dirs[2]
	adjPrefix<-mrgParam$prefix[1]
	mrgPrefix<-mrgParam$prefix[2]
	mrgSuffix<-mrgParam$prefix[3]

	freqData<-mrgParam$dates[1]
	istart<-mrgParam$dates[2]
	iend<-mrgParam$dates[3]

	params.mrg<-as.character(gal.params$params.mrg$Values)
	nmin<-as.numeric(params.mrg[1])
	min.nbrs<-as.numeric(params.mrg[2])
	max.nbrs<-as.numeric(params.mrg[3])
	max.dst<- as.numeric(params.mrg[4])
	interpMethod<-params.mrg[5]

	mrgd.tt <- var.def.ncdf('temp', "DegC",xy.dim, -99, longname='Reanalysis merged with station', prec="single")

	if(freqData=='daily'){
		mrg.dates<-format(seq(as.Date(istart,format='%Y%m%d'),as.Date(iend,format='%Y%m%d'),'day'),'%Y%m%d')
		testfile<-file.path(adjDir,paste(adjPrefix,'_',mrg.dates,'.nc',sep=''),fsep = .Platform$file.sep)
	}
	if(freqData=='dekadal'){
		mrg.dates<-seq(as.Date(istart,format='%Y%m%d'),as.Date(iend,format='%Y%m%d'),'day')
		mrg.dates<-paste(format(mrg.dates[which(as.numeric(format(mrg.dates,'%d'))<=3)],'%Y%m'),as.numeric(format(mrg.dates[which(as.numeric(format(mrg.dates,'%d'))<=3)],'%d')),sep='')
		testfile<-file.path(adjDir,paste(adjPrefix,'_',mrg.dates,'.nc',sep=''),fsep = .Platform$file.sep)
	}
	if(freqData=='monthly'){
		mrg.dates<-format(seq(as.Date(istart,format='%Y%m%d'),as.Date(iend,format='%Y%m%d'),'month'),'%Y%m')
		testfile<-file.path(adjDir,paste(adjPrefix,'_',mrg.dates,'.nc',sep=''),fsep = .Platform$file.sep)
	}

	existFl<-unlist(lapply(testfile,file.exists))
	if(length(which(existFl))==0){
		insert.txt(main.txt.out,"Adjusted data not found",format=TRUE)
		return(NULL)
	}
	adjDataFl<-testfile[existFl]
	mrg.dates<-mrg.dates[existFl]

	tcl("update","idletasks")
	ret<-lapply(seq_along(adjDataFl),function(jfl){
		adjfl <- adjDataFl[jfl]
		outfl<-file.path(mrgDir,paste(mrgPrefix,'_',mrg.dates[jfl],'_',mrgSuffix,'.nc',sep=''),fsep = .Platform$file.sep)

		nc <- open.ncdf(adjfl)
		tt.mod <- get.var.ncdf(nc,varid = nc$var[[1]]$name)
		close.ncdf(nc)

		mod.vec <- as.vector(tt.mod)
		newlocation.merging<-SpatialPointsDataFrame(coords=newlocation.merging, data=data.frame(mod=mod.vec), proj4string = CRS(as.character(NA)))

		ic <- which(stn.dates==mrg.dates[jfl])
		mod.stn <- tt.mod[ijGrd]
		tt <- as.vector(t(stn.data[ic,]))
		tt.stn <- data.frame(lon=stn.lon,lat=stn.lat,mod=mod.stn,tt=tt)
		ix <- which(!is.na(tt))

		tt.mrg <- mod.vec  #Default outpit is input adjusted Renalysis

		if(length(ix) >= nmin){  # merging only if num stations >= nmin
			tt.stn <-tt.stn[ix,]
			coordinates(tt.stn) = ~lon+lat

			#remove NA
			tt.stn <- tt.stn[!is.na(tt.stn$mod) & !is.na(tt.stn$tt),]
			tt.stn$diff<-tt.stn$tt-tt.stn$mod
			if(interpMethod=="IDW"){
				grd.res <- krige(diff~1,locations=tt.stn,newdata=newlocation.merging, nmax=max.nbrs,nmin=min.nbrs,maxdist=max.dst,debug.level=0)
				tt.mrg<-grd.res$var1.pred+tt.mrg
			}else if(interpMethod=="Kriging"){
				grd.res <- try(autoKrige(diff~1,input_data=tt.stn,new_data=newlocation.merging, model =VarioModel,nmin=min.nbrs,nmax=max.nbrs,maxdist=max.dst, debug.level=0), silent=TRUE)
				if(!inherits(grd.res, "try-error")){
					tt.mrg<-grd.res$krige_output$var1.pred+tt.mrg
				}else{
					grd.res <- krige(diff~1,locations=tt.stn,newdata=newlocation.merging, nmax=max.nbrs,nmin=min.nbrs,maxdist=max.dst,debug.level=0)
					tt.mrg<-grd.res$var1.pred+tt.mrg
				}
			}
			tt.mrg <- round(tt.mrg,2)
			# Take adjusted Renalysis for areas where interpolation/merging was not possible
			ix <- which(is.na(tt.mrg))
			tt.mrg[ix] <- mod.vec[ix]
		}

		out.tt <- as.numeric(tt.mrg)
		out.tt[is.na(out.tt)] <- -99
		dim(out.tt) <- c(nlon0,nlat0)
		#Apply mask for area of interest
		if(!is.null(outMask)) out.tt[is.na(outMask)]<- -99
		nc2 <- create.ncdf(outfl,mrgd.tt)
		put.var.ncdf(nc2,mrgd.tt,out.tt)
		close.ncdf(nc2)

		insert.txt(main.txt.out,paste("Merging finished successfully:",paste(mrgPrefix,'_',mrg.dates[jfl],'_',mrgSuffix,'.nc',sep='')))
		tcl("update")
		return(0)
	})
	return(0)
}






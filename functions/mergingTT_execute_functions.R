TreatCoefDownTemp<-function(origdir,freqData){

	file.pars<-as.character(gal.params$file.io$Values)
	dir.create(origdir,showWarnings=FALSE)

	all.open.file<-as.character(unlist(lapply(1:length(file.opfiles),function(j) file.opfiles[[j]][[1]])))

	jfile<-which(all.open.file==file.pars[1])
	donne<-file.opfiles[[jfile]][[2]]

	#######get data
	donne<-splitCDTData(donne,freqData)
	if(is.null(donne)) return(NULL)
	stn.lon<-donne$lon
	stn.lat<-donne$lat
	stn.id<-donne$id
	elv<-donne$elv
	dates<-donne$dates
	donne<-donne$data

	#if(nrow(donne$stnDuplCoords)>0)  #diplay table
	#if(nrow(dat$stnMissCoords)>0)

	stnlist<-list(id=stn.id,lon=stn.lon,lat=stn.lat,elv=elv,dates=dates,data=donne)

	###get elevation data
	jncdf<-which(all.open.file==file.pars[2])
	fdem<-file.opfiles[[jncdf]][[2]]
	dem<-fdem$value
	dem[dem<0]<-0
	dem.coord<-data.frame(expand.grid(lon=fdem$x,lat=fdem$y))
	coordinates(dem.coord) = ~lon+lat
	demdf<-data.frame(dem=c(dem))
	demdf <-  SpatialPointsDataFrame(coords=dem.coord,data=demdf, proj4string = CRS(as.character(NA)))
	demlist<-list(lon=fdem$x,lat=fdem$y,demGrd=demdf)

	coefdownTempdat<-list(stnData=stnlist,demData=demlist)
	outfile<-file.path(origdir,'DataUsed2Compute_Coef.RData',fsep = .Platform$file.sep)
	save(coefdownTempdat,file=outfile)

	return(coefdownTempdat)
}

####################################################################

execCoefDownTemp<-function(origdir){
	freqData<-gal.params$period
	year1<-as.numeric(as.character(gal.params$dates.coef$Values[1]))
	year2<-as.numeric(as.character(gal.params$dates.coef$Values[2]))

	coefdownTempdat<-TreatCoefDownTemp(origdir,freqData)
	##DEM data
	dem<-coefdownTempdat$demData$demGrd@data$dem
	grid.loc<-SpatialPixels(points =coefdownTempdat$demData$demGrd)
	##Gauge data
	stn.lon<-coefdownTempdat$stnData$lon
	stn.lat<-coefdownTempdat$stnData$lat
	stn.dates<-coefdownTempdat$stnData$dates
	stn.data<-coefdownTempdat$stnData$data
	nstn<-length(stn.lon)

	#DEM over stations
	stn.loc <- data.frame(lon=stn.lon, lat=stn.lat)
	stn.loc <- SpatialPoints(stn.loc)
	ijGrd <- over(stn.loc, grid.loc)
	dem.stn <- dem[ijGrd]

	#Compute regression parameters between station temperature and elevation
	GlmCoefDownscaling(stn.data,stn.dates,dem.stn,year1,year2,origdir)
	#return(0)
}

#######################################################################################

TreatDownscalingTemp<-function(origdir,freqData){

	file.pars<-as.character(gal.params$file.io$Values)
	dir.create(origdir,showWarnings=FALSE)

	all.open.file<-as.character(unlist(lapply(1:length(file.opfiles),function(j) file.opfiles[[j]][[1]])))

	##############
	##if DEM data from the DEM Used to compute coef
	#coefDir<-file.path(dirname(file.pars[1]),'DataUsed2Compute_Coef.RData',fsep = .Platform$file.sep)
	#load(coefDir)
	#demData<-coefdownTempdat$demData
	#dem<-matrix(demData$demGrd@data[,1],nrow=length(demData$lon),ncol=length(demData$lat))
	#demlist<-list(lon=demData$lon,lat=demData$lat,demMat=dem,demGrd=demData$demGrd)

	###get elevation data
	jncdf<-which(all.open.file==file.pars[2])
	fdem<-file.opfiles[[jncdf]][[2]]
	dem<-fdem$value
	dem[dem<0]<-0
	dem.coord<-data.frame(expand.grid(lon=fdem$x,lat=fdem$y))
	coordinates(dem.coord) = ~lon+lat
	demdf<-data.frame(dem=c(dem))
	demdf <-  SpatialPointsDataFrame(coords=dem.coord,data=demdf, proj4string = CRS(as.character(NA)))
	demlist<-list(lon=fdem$x,lat=fdem$y,demMat=dem,demGrd=demdf)

	##RFE sample file
	jrfe<-which(all.open.file==file.pars[3])
	ncrfe<-file.opfiles[[jrfe]][[2]]
	rfe.coord<-data.frame(expand.grid(lon=ncrfe$x,lat=ncrfe$y))
	coordinates(rfe.coord) = ~lon+lat
	rfelist<-list(lon=ncrfe$x,lat=ncrfe$y,rfeGrd=rfe.coord,rfeVarid=ncrfe$varid,rfeILon=ncrfe$ilon,rfeILat=ncrfe$ilat,irevlat=ncrfe$irevlat)

	downTempdat<-list(demData=demlist,rfeData=rfelist)
	#save(downTempdat,file=fldekdata)
	return(downTempdat)
}


#####################################################

execDownscalingTemp<-function(origdir){
	freqData<-gal.params$period
	downTempdat<-TreatDownscalingTemp(origdir,freqData)
	coef<-read.table(as.character(gal.params$file.io$Values[1]))

	dirJRA<-as.character(gal.params$file.io$Values[4])
	rfe.file.format<-as.character(gal.params$IO.file.format$Values[1])
	downPrefix<-as.character(gal.params$IO.file.format$Values[2])
	create.grd<-as.character(gal.params$CreateGrd)

	datesSE<-as.numeric(as.character(gal.params$dates.down$Values))

	istart<-as.Date(paste(datesSE[1],datesSE[2],datesSE[3],sep='-'))
	iend<-as.Date(paste(datesSE[4],datesSE[5],datesSE[6],sep='-'))
	if(freqData=='dekadal'){
		istart<-paste(format(istart,'%Y%m'),as.numeric(format(istart,'%d')),sep='')
		iend<-paste(format(iend,'%Y%m'),as.numeric(format(iend,'%d')),sep='')
	}else{
		istart<-format(istart,'%Y%m%d')
		iend<-format(iend,'%Y%m%d')
	}


	############################################
	##Create grid for interpolation
	if(create.grd=='1'){
		grd.lon<-downTempdat$demData$lon
		grd.lat<-downTempdat$demData$lat
		nlon0<-length(grd.lon)
		nlat0<-length(grd.lat)
		newlocation.merging <- expand.grid(lon=grd.lon, lat=grd.lat)
	}else if(create.grd=='2'){
		X0<-as.numeric(as.character(gal.params$new.grid$Values[1]))
		X1<-as.numeric(as.character(gal.params$new.grid$Values[2]))
		pX<-as.numeric(as.character(gal.params$new.grid$Values[3]))
		Y0<-as.numeric(as.character(gal.params$new.grid$Values[4]))
		Y1<-as.numeric(as.character(gal.params$new.grid$Values[5]))
		pY<-as.numeric(as.character(gal.params$new.grid$Values[6]))

		grd.lon<-seq(X0,X1,pX)
		nlon0<-length(grd.lon)
		grd.lat<-seq(Y0,Y1,pY)
		nlat0<-length(grd.lat)
		newlocation.merging <- expand.grid(lon=grd.lon, lat=grd.lat)
	}

	coordinates(newlocation.merging)<- ~lon+lat
	grid.loc<-newlocation.merging
	grid.loc<-SpatialPixels(points =grid.loc, tolerance =sqrt(sqrt(.Machine$double.eps)),proj4string = CRS(as.character(NA)))

	##interpolate dem in new grid
	if(create.grd=='2'){
		dem.grd<-krige(formula = dem ~ 1,locations=downTempdat$demData$demGrd,newdata=newlocation.merging,nmax=4,nmin =2,debug.level=0)
		dem<-ifelse(dem.grd@data$var1.pred<0,0,dem.grd@data$var1.pred)
		demMask<-matrix(dem,nrow=nlon0,ncol=nlat0)
		demMask[demMask==0]<-NA
	}else if(create.grd=='1'){
		dem<-c(downTempdat$demData$demMat)
		demMask<-downTempdat$demData$demMat
		demMask[demMask==0]<-NA
	}

	##Reanalysis over new grid
	xlon<-downTempdat$rfeData$lon
	xlat<-downTempdat$rfeData$lat
	rfegrd<-downTempdat$rfeData$rfeGrd
	ijreanal <- over(rfegrd, grid.loc)

	#dem at reanalysis grid
	dem.reanal <- dem[ijreanal]

	#Defines netcdf output dims
	dx <- dim.def.ncdf("Lon", "degreeE", grd.lon)
	dy <- dim.def.ncdf("Lat", "degreeN", grd.lat)
	xy.dim<-list(dx,dy)

	ReanalysisDownscaling(freqData,coef,istart,iend,dem.reanal,dem,downTempdat$rfeData,
	newlocation.merging,xy.dim,nlon0,nlat0,origdir,dirJRA,rfe.file.format,downPrefix)
	#return(0)
}

#####################################################
###Compute Mean Bias Coef
TreatBiasCoefTemp<-function(origdir,freqData){

	file.pars<-as.character(gal.params$file.io$Values)
	dir.create(origdir,showWarnings=FALSE)

	all.open.file<-as.character(unlist(lapply(1:length(file.opfiles),function(j) file.opfiles[[j]][[1]])))

	jfile<-which(all.open.file==file.pars[1])
	donne<-file.opfiles[[jfile]][[2]]

	#######get data
	donne<-splitCDTData(donne,freqData)
	if(is.null(donne)) return(NULL)
	stn.lon<-donne$lon
	stn.lat<-donne$lat
	stn.id<-donne$id
	elv<-donne$elv
	dates<-donne$dates
	donne<-donne$data

	#if(nrow(donne$stnDuplCoords)>0)  #diplay table
	#if(nrow(dat$stnMissCoords)>0)

	stnlist<-list(id=stn.id,lon=stn.lon,lat=stn.lat,elv=elv,dates=dates,data=donne)

	###get elevation data
	jncdf<-which(all.open.file==file.pars[2])
	fdem<-file.opfiles[[jncdf]][[2]]
	dem<-fdem$value
	dem[dem<0]<-0
	dem.coord<-data.frame(expand.grid(lon=fdem$x,lat=fdem$y))
	coordinates(dem.coord) = ~lon+lat
	demdf<-data.frame(dem=c(dem))
	demdf <-  SpatialPointsDataFrame(coords=dem.coord,data=demdf, proj4string = CRS(as.character(NA)))
	demlist<-list(lon=fdem$x,lat=fdem$y,demGrd=demdf)

	coefBiasTempdat<-list(stnData=stnlist,demData=demlist)
	outfile<-file.path(origdir,'DataUsed2Compute_Coef.RData',fsep = .Platform$file.sep)
	save(coefBiasTempdat,file=outfile)
	return(coefBiasTempdat)
}


#################################

execCoefBiasCompute<-function(origdir){
	freqData<-gal.params$period
	coefBiasTempdat<-TreatBiasCoefTemp(origdir,freqData)

	downscaledDir<-as.character(gal.params$file.io$Values[3])
	biasRemoval<- as.character(gal.params$bias.method)

	year1<-as.numeric(as.character(gal.params$dates.coef$Values[1]))
	year2<-as.numeric(as.character(gal.params$dates.coef$Values[2]))
	coef.dates<-c(year1,year2)

	downPrefix<-as.character(gal.params$prefix$Values[1])
	meanBiasPrefix<-as.character(gal.params$prefix$Values[2])
	#########
	downFile<-list.files(downscaledDir,downPrefix)[1]
	if(is.na(downFile)){
		insert.txt(main.txt.out,"Downscaled data not found",format=TRUE)
		return(NULL)
	}
	downfl<-file.path(downscaledDir,downFile,fsep = .Platform$file.sep)
	nc<-open.ncdf(downfl)
	grd.lon<-nc$dim[[1]]$vals
	grd.lat<-nc$dim[[2]]$vals
	close.ncdf(nc)
	nlon0<-length(grd.lon)
	nlat0<-length(grd.lat)
	newlocation.merging <- expand.grid(lon=grd.lon, lat=grd.lat)
	coordinates(newlocation.merging)<- ~lon+lat
	grid.loc<-newlocation.merging
	grid.loc<-SpatialPixels(points =grid.loc, tolerance =sqrt(sqrt(.Machine$double.eps)),proj4string = CRS(as.character(NA)))

	##DEM data
	#dem.grd<-krige(formula = dem ~ 1,locations=coefBiasTempdat$demData$demGrd,newdata=newlocation.merging,nmax=4,nmin =2,debug.level=0)
	#dem<-ifelse(dem.grd@data$var1.pred<0,0,dem.grd@data$var1.pred)
	dem<-coefBiasTempdat$demData$demGrd@data[,1]

	##Gauge data
	stn.lon<-coefBiasTempdat$stnData$lon
	stn.lat<-coefBiasTempdat$stnData$lat
	stn.dates<-coefBiasTempdat$stnData$dates
	stn.data<-coefBiasTempdat$stnData$data
	nstn<-length(stn.lon)

	#Index of new grid over stations
	stn.loc <- data.frame(lon=stn.lon, lat=stn.lat)
	stn.loc <- SpatialPoints(stn.loc)
	ijGrd <- over(stn.loc, grid.loc)
	dem.stn <- dem[ijGrd]

	#Defines netcdf output dims
	dx <- dim.def.ncdf("Lon", "degreeE", grd.lon)
	dy <- dim.def.ncdf("Lat", "degreeN", grd.lat)
	xy.dim<-list(dx,dy)

	#############
	# coefBiasTempdat$demData$dem<-dem
	# #coefBiasTempdat$newgrid<-newlocation.merging
	# dimInfos<-list(nlon0=nlon0,nlat0=nlat0,xy.dim=xy.dim,ijGrd=ijGrd,dem.stn=dem.stn)
	# coefBiasTempdat$dimInfos<-dimInfos

	# outfile<-file.path(origdir,'DataUsed2Compute_Coef.RData',fsep = .Platform$file.sep)
	# save(coefBiasTempdat,file=outfile)
	###########

	# Extract model values at all station locations
	model_stn<-ExtractReanal2Stn(freqData,ijGrd,nstn,coef.dates,downscaledDir,downPrefix)

	#method 1
	if(biasRemoval=='Bias-kriging'){
		dirouts1<-file.path(origdir,'Mean_bias',fsep = .Platform$file.sep)
		if(!file.exists(dirouts1)) dir.create(dirouts1,showWarnings=FALSE)
		ComputeMeanBias(freqData,coefBiasTempdat$stnData,model_stn,coef.dates,xy.dim,
		nlon0,nlat0,newlocation.merging,dirouts1,meanBiasPrefix)
	}

	#method 2
	if(biasRemoval=='Regression-QM'){
		ComputeRegCoeff(stn.data,stn.dates,coef.dates,dem.stn,model_stn,origdir)
	}
	#return(0)
}
#################################################

##Adjust downscaled data
TreatAjdBiasDownTemp<-function(origdir,freqData){

	file.pars<-as.character(gal.params$file.io$Values)
	dir.create(origdir,showWarnings=FALSE)

	all.open.file<-as.character(unlist(lapply(1:length(file.opfiles),function(j) file.opfiles[[j]][[1]])))

	jfile<-which(all.open.file==file.pars[1])
	donne<-file.opfiles[[jfile]][[2]]

	#######get data
	donne<-splitCDTData(donne,freqData)
	if(is.null(donne)) return(NULL)
	stn.lon<-donne$lon
	stn.lat<-donne$lat
	stn.id<-donne$id
	elv<-donne$elv
	dates<-donne$dates
	donne<-donne$data

	#if(nrow(donne$stnDuplCoords)>0)  #diplay table
	#if(nrow(dat$stnMissCoords)>0)

	stnlist<-list(id=stn.id,lon=stn.lon,lat=stn.lat,elv=elv,dates=dates,data=donne)

	###get elevation data
	jncdf<-which(all.open.file==file.pars[2])
	fdem<-file.opfiles[[jncdf]][[2]]
	dem<-fdem$value
	dem[dem<0]<-0
	dem.coord<-data.frame(expand.grid(lon=fdem$x,lat=fdem$y))
	coordinates(dem.coord) = ~lon+lat
	demdf<-data.frame(dem=c(dem))
	demdf <-  SpatialPointsDataFrame(coords=dem.coord,data=demdf, proj4string = CRS(as.character(NA)))
	demlist<-list(lon=fdem$x,lat=fdem$y,demGrd=demdf)

	adjdownTempdat<-list(stnData=stnlist,demData=demlist)
	outfile<-file.path(origdir,'DataUsed2Adjust.RData',fsep = .Platform$file.sep)
	save(adjdownTempdat,file=outfile)

	return(adjdownTempdat)
}


#############


execAjdBiasDownTemp<-function(origdir){
	freqData<-gal.params$period
	adjdownTempdat<-TreatAjdBiasDownTemp(origdir,freqData)

	downscaledDir<-as.character(gal.params$file.io$Values[3])
	biasDirORFile<-as.character(gal.params$file.io$Values[4])
	adjDir<-file.path(origdir,'Adjusted_data',fsep = .Platform$file.sep)
	dir.create(adjDir,showWarnings=FALSE)

	biasRemoval<- as.character(gal.params$bias.method)

	datesSE<-as.numeric(as.character(gal.params$dates.adj$Values))

	istart<-as.Date(paste(datesSE[1],datesSE[2],datesSE[3],sep='-'))
	iend<-as.Date(paste(datesSE[4],datesSE[5],datesSE[6],sep='-'))
	if(freqData=='dekadal'){
		istart<-paste(format(istart,'%Y%m'),as.numeric(format(istart,'%d')),sep='')
		iend<-paste(format(iend,'%Y%m'),as.numeric(format(iend,'%d')),sep='')
	}else{
		istart<-format(istart,'%Y%m%d')
		iend<-format(iend,'%Y%m%d')
	}

	downPrefix<-as.character(gal.params$prefix$Values[1])
	meanBiasPrefix<-as.character(gal.params$prefix$Values[2])
	adjPrefix<-as.character(gal.params$prefix$Values[3])

	#########
	fstdate<-ifelse(freqData=='monthly',substr(istart,1,6),istart)
	downFile<-file.path(downscaledDir,paste(downPrefix,'_',fstdate,'.nc',sep=''),fsep = .Platform$file.sep)
	if(!file.exists(downFile)){
		insert.txt(main.txt.out,"Downscaled data not found",format=TRUE)
		return(NULL)
	}
	if(biasRemoval=='Bias-kriging'){
		biasFile<-file.path(biasDirORFile,paste(meanBiasPrefix,'_1.nc',sep=''),fsep = .Platform$file.sep)
		if(!file.exists(biasFile)){
			insert.txt(main.txt.out,"Mean bias coefficients not found",format=TRUE)
			return(NULL)
		}
	}else{
		if(!file.exists(biasDirORFile)){
			insert.txt(main.txt.out,"Regression coefficients not found",format=TRUE)
			return(NULL)
		}
		is.rdble<-!inherits(try(coefReg<-read.table(biasDirORFile,header=TRUE),silent=TRUE),"try-error")
		if(!is.rdble){
			insert.txt(main.txt.out,paste("Unable to open file",biasDirORFile),format=TRUE)
			return(NULL)
		}
	}

	nc<-open.ncdf(downFile)
	grd.lon<-nc$dim[[1]]$vals
	grd.lat<-nc$dim[[2]]$vals
	close.ncdf(nc)
	nlon0<-length(grd.lon)
	nlat0<-length(grd.lat)
	newlocation.merging <- expand.grid(lon=grd.lon, lat=grd.lat)
	coordinates(newlocation.merging)<- ~lon+lat
	grid.loc<-newlocation.merging
	grid.loc<-SpatialPixels(points =grid.loc, tolerance =sqrt(sqrt(.Machine$double.eps)),proj4string = CRS(as.character(NA)))

	##DEM data
	#dem.grd<-krige(formula = dem ~ 1,locations=adjdownTempdat$demData$demGrd,newdata=newlocation.merging,nmax=4,nmin =2,debug.level=0)
	#dem<-ifelse(dem.grd@data$var1.pred<0,0,dem.grd@data$var1.pred)
	dem<-adjdownTempdat$demData$demGrd@data[,1]


	#Defines netcdf output dims
	dx <- dim.def.ncdf("Lon", "degreeE", grd.lon)
	dy <- dim.def.ncdf("Lat", "degreeN", grd.lat)
	xy.dim<-list(dx,dy)

	#############
	# coefBiasTempdat$demData$dem<-dem
	# #coefBiasTempdat$newgrid<-newlocation.merging
	# dimInfos<-list(nlon0=nlon0,nlat0=nlat0,xy.dim=xy.dim,ijGrd=ijGrd,dem.stn=dem.stn)
	# coefBiasTempdat$dimInfos<-dimInfos

	# outfile<-file.path(origdir,'DataUsed2Compute_Coef.RData',fsep = .Platform$file.sep)
	# save(coefBiasTempdat,file=outfile)
	###########

	#method 1
	if(biasRemoval=='Bias-kriging'){
		AjdReanalMeanBias(freqData,istart,iend,dem,xy.dim,nlon0,nlat0,
		downscaledDir,biasDirORFile,adjDir,downPrefix,meanBiasPrefix,adjPrefix)
	}

	#method 2
	if(biasRemoval=='Regression-QM'){
		##Gauge data
		stn.lon<-adjdownTempdat$stnData$lon
		stn.lat<-adjdownTempdat$stnData$lat
		#Index of new grid over stations
		stn.loc <- data.frame(lon=stn.lon, lat=stn.lat)
		stn.loc <- SpatialPoints(stn.loc)
		ijGrd <- over(stn.loc, grid.loc)
		#dem.stn <- dem[ijGrd]

		AjdReanalpmm(freqData,istart,iend,adjdownTempdat$stnData,ijGrd,dem,xy.dim,nlon0,nlat0,
		coefReg,downscaledDir,adjDir,downPrefix,adjPrefix)

	}
	#return(0)
}
#######################################################################################
TreatmergeTemp<-function(origdir,freqData){

	file.pars<-as.character(gal.params$file.io$Values)
	dir.create(origdir,showWarnings=FALSE)

	all.open.file<-as.character(unlist(lapply(1:length(file.opfiles),function(j) file.opfiles[[j]][[1]])))

	jfile<-which(all.open.file==file.pars[1])
	donne<-file.opfiles[[jfile]][[2]]

	#######get data
	donne<-splitCDTData(donne,freqData)
	if(is.null(donne)) return(NULL)
	stn.lon<-donne$lon
	stn.lat<-donne$lat
	stn.id<-donne$id
	elv<-donne$elv
	dates<-donne$dates
	donne<-donne$data

	#if(nrow(donne$stnDuplCoords)>0)  #diplay table
	#if(nrow(dat$stnMissCoords)>0)

	stnlist<-list(id=stn.id,lon=stn.lon,lat=stn.lat,elv=elv,dates=dates,data=donne)

	###get elevation data
	if(gal.params$blankGrd=="2"){
		jncdf<-which(all.open.file==file.pars[2])
		fdem<-file.opfiles[[jncdf]][[2]]
		dem<-fdem$value
		dem[dem<0]<-0
		dem.coord<-data.frame(expand.grid(lon=fdem$x,lat=fdem$y))
		coordinates(dem.coord) = ~lon+lat
		demdf<-data.frame(dem=c(dem))
		demdf <-  SpatialPointsDataFrame(coords=dem.coord,data=demdf, proj4string = CRS(as.character(NA)))
		demlist<-list(lon=fdem$x,lat=fdem$y,demGrd=demdf)
	}else demlist<-NULL
	###get boundaries shape
	if(gal.params$blankGrd=="3"){
		jshp<-which(all.open.file==file.pars[3])
		shpd<-file.opfiles[[jshp]][[2]]
	}else shpd<-NULL

	mrgTempdat<-list(stnData=stnlist,demData=demlist,shpData=shpd)
	#outfile<-file.path(origdir,'DataUsed2Merge.RData',fsep = .Platform$file.sep)
	#save(mrgTempdat,file=outfile)

	return(mrgTempdat)
}

###########################

execMergeTemp<-function(origdir){
	freqData<-gal.params$period
	mrgTempdat<-TreatmergeTemp(origdir,freqData)

	datesSE<-as.numeric(as.character(gal.params$dates.mrg$Values))
	istart<-as.Date(paste(datesSE[1],datesSE[2],datesSE[3],sep='-'))
	iend<-as.Date(paste(datesSE[4],datesSE[5],datesSE[6],sep='-'))
	if(freqData=='dekadal'){
		istart<-paste(format(istart,'%Y%m'),as.numeric(format(istart,'%d')),sep='')
		iend<-paste(format(iend,'%Y%m'),as.numeric(format(iend,'%d')),sep='')
	}else{
		istart<-format(istart,'%Y%m%d')
		iend<-format(iend,'%Y%m%d')
	}

	adjDir<-as.character(gal.params$file.io$Values[4])
	adjPrefix<-as.character(gal.params$prefix$Values[1])
	mrgPrefix<-as.character(gal.params$prefix$Values[2])
	mrgSuffix<-as.character(gal.params$prefix$Values[3])
	usemask<-as.character(gal.params$blankGrd)

	fstdate<-ifelse(freqData=='monthly',substr(istart,1,6),istart)
	downFile<-file.path(adjDir,paste(adjPrefix,'_',fstdate,'.nc',sep=''),fsep = .Platform$file.sep)
	if(!file.exists(downFile)){
		insert.txt(main.txt.out,"Downscaled or adjusted data not found",format=TRUE)
		return(NULL)
	}

	nc<-open.ncdf(downFile)
	grd.lon<-nc$dim[[1]]$vals
	grd.lat<-nc$dim[[2]]$vals
	close.ncdf(nc)
	nlon0<-length(grd.lon)
	nlat0<-length(grd.lat)
	newlocation.merging <- expand.grid(lon=grd.lon, lat=grd.lat)
	coordinates(newlocation.merging)<- ~lon+lat
	grid.loc<-newlocation.merging
	grid.loc<-SpatialPixels(points =grid.loc, tolerance =sqrt(sqrt(.Machine$double.eps)),proj4string = CRS(as.character(NA)))

	if(usemask=="1") outMask<-NULL
	if(usemask=="2"){
		dem.grd<-krige(formula = dem ~ 1,locations=mrgTempdat$demData$demGrd,newdata=newlocation.merging,nmax=5,nmin =3,debug.level=0)
		dem<-ifelse(dem.grd@data$var1.pred<0,0,dem.grd@data$var1.pred)
		#or
		#dem<-adjdownTempdat$demData$demGrd@data[,1]
		outMask<-matrix(dem,nrow=nlon0,ncol=nlat0)
		outMask[outMask==0]<-NA
	}
	if(usemask=="3"){
		shpd<-mrgTempdat$shpData
		shpd[['vtmp']]<-1
		shpMask<-over(newlocation.merging,shpd)[,'vtmp']
		outMask<-matrix(shpMask,nrow=nlon0,ncol=nlat0)
	}

	###############################
	##Gauge data
	stn.lon<-mrgTempdat$stnData$lon
	stn.lat<-mrgTempdat$stnData$lat
	stn.dates<-mrgTempdat$stnData$dates
	stn.data<-mrgTempdat$stnData$data
	#nstn<-length(stn.lon)

	#Index of new grid over stations
	stn.loc <- data.frame(lon=stn.lon, lat=stn.lat)
	stn.loc <- SpatialPoints(stn.loc)
	ijGrd <- over(stn.loc, grid.loc)

	#Defines netcdf output dims
	dx <- dim.def.ncdf("Lon", "degreeE", grd.lon)
	dy <- dim.def.ncdf("Lat", "degreeN", grd.lat)
	xy.dim<-list(dx,dy)

	VarioModel<-c("Sph", "Exp", "Gau")

	mrgParam<-list(dates=c(freqData,istart,iend),prefix=c(adjPrefix,mrgPrefix,mrgSuffix),dirs=c(adjDir,origdir),
	mrgInfo=list(nlon0=nlon0,nlat0=nlat0,ijGrd=ijGrd,xy.dim=xy.dim,VarioModel=VarioModel),
	mrgData=list(stnData=mrgTempdat$stnData,outMask=outMask,newlocation.merging=newlocation.merging))
	MergeTemp(mrgParam)
	#return(0)
}


#######################################################################################
##eto



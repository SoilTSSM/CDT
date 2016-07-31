TreatCoefDownTemp <- function(origdir){
	freqData <- GeneralParameters$period
	file.pars <- as.character(GeneralParameters$file.io$Values)
	dir.create(origdir, showWarnings = FALSE)

	all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))

	jfile <- which(all.open.file == file.pars[1])
	donne <- AllOpenFilesData[[jfile]][[2]]

	#######get data
	donne <- splitCDTData(donne, freqData)
	if(is.null(donne)) return(NULL)
	stn.lon <- donne$lon
	stn.lat <- donne$lat
	stn.id <- donne$id
	elv <- donne$elv
	dates <- donne$dates
	donne <- donne$data
	#if(nrow(donne$duplicated.coords) > 0)  #diplay table
	#if(nrow(dat$missing.coords) > 0)

	stnlist <- list(id = stn.id, lon = stn.lon, lat = stn.lat, elv = elv, dates = dates, data = donne)

	###get elevation data
	jncdf <- which(all.open.file == file.pars[2])
	fdem <- AllOpenFilesData[[jncdf]][[2]]
	dem <- fdem$value
	dem[dem < 0] <- 0
	dem.coord <- data.frame(expand.grid(lon = fdem$x, lat = fdem$y))
	coordinates(dem.coord) = ~lon+lat
	demdf <- data.frame(dem = c(dem))
	demdf <- SpatialPointsDataFrame(coords = dem.coord, data = demdf, proj4string = CRS(as.character(NA)))
	demlist <- list(lon = fdem$x, lat = fdem$y, demGrd = demdf)

	coefdownTempdat <- list(stnData = stnlist, demData = demlist)
	outfile <- file.path(origdir, 'DataUsed2Compute_Coef.RData', fsep = .Platform$file.sep)
	save(coefdownTempdat, file = outfile)

	return(coefdownTempdat)
}

####################################################################

execCoefDownTemp <- function(origdir){
	coefdownTempdat <- TreatCoefDownTemp(origdir)
	##DEM data
	dem <- coefdownTempdat$demData$demGrd@data$dem
	grid.loc <- SpatialPixels(points = coefdownTempdat$demData$demGrd)
	##Gauge data
	stn.lon <- coefdownTempdat$stnData$lon
	stn.lat <- coefdownTempdat$stnData$lat

	#DEM over stations
	stn.loc <- data.frame(lon = stn.lon, lat = stn.lat)
	stn.loc <- SpatialPoints(stn.loc)
	ijGrd <- over(stn.loc, grid.loc)
	dem.stn <- dem[ijGrd]

	#Compute regression parameters between station temperature and elevation
	paramsGlmCoef <- list(coefdownTempdat = coefdownTempdat, dem.stn = dem.stn, origdir = origdir)
	ret <- GlmCoefDownscaling(paramsGlmCoef)
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

#######################################################################################

TreatDownscalingTemp <- function(origdir){
	file.pars <- as.character(GeneralParameters$file.io$Values)
	dir.create(origdir, showWarnings = FALSE)

	all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))

	###get elevation data
	jncdf <- which(all.open.file == file.pars[2])
	fdem <- AllOpenFilesData[[jncdf]][[2]]
	dem <- fdem$value
	dem[dem < 0] <- 0
	dem.coord <- data.frame(expand.grid(lon = fdem$x, lat = fdem$y))
	coordinates(dem.coord) = ~lon+lat
	demdf <- data.frame(dem = c(dem))
	demdf <- SpatialPointsDataFrame(coords = dem.coord, data = demdf, proj4string = CRS(as.character(NA)))
	demlist <- list(lon = fdem$x, lat = fdem$y, demMat = dem, demGrd = demdf)

	##RFE sample file
	jrfe <- which(all.open.file == file.pars[3])
	ncrfe <- AllOpenFilesData[[jrfe]][[2]]
	rfe.coord <- data.frame(expand.grid(lon = ncrfe$x, lat = ncrfe$y))
	coordinates(rfe.coord) = ~lon+lat
	rfelist <- list(lon = ncrfe$x, lat = ncrfe$y, rfeGrd = rfe.coord, rfeVarid = ncrfe$varid, rfeILon = ncrfe$ilon, rfeILat = ncrfe$ilat, irevlat = ncrfe$irevlat)

	downTempdat <- list(demData = demlist, rfeData = rfelist)
	return(downTempdat)
}


#####################################################

execDownscalingTemp <- function(origdir){
	freqData <- GeneralParameters$period
	downTempdat <- TreatDownscalingTemp(origdir)

	create.grd <- as.character(GeneralParameters$CreateGrd)
	datesSE <- as.numeric(as.character(GeneralParameters$dates.down$Values))

	istart <- as.Date(paste(datesSE[1], datesSE[2], datesSE[3], sep = '-'))
	iend <- as.Date(paste(datesSE[4], datesSE[5], datesSE[6], sep = '-'))
	if(freqData == 'dekadal'){
		istart <- paste(format(istart,'%Y%m'), as.numeric(format(istart,'%d')), sep = '')
		iend <- paste(format(iend,'%Y%m'), as.numeric(format(iend,'%d')), sep = '')
	}else{
		istart <- format(istart,'%Y%m%d')
		iend <- format(iend,'%Y%m%d')
	}

	############################################
	##Create grid for interpolation
	if(create.grd == '1'){
		grd.lon <- downTempdat$demData$lon
		grd.lat <- downTempdat$demData$lat
		nlon0 <- length(grd.lon)
		nlat0 <- length(grd.lat)
		newlocation.merging <- expand.grid(lon = grd.lon, lat = grd.lat)
	}else if(create.grd == '2'){
		X0 <- as.numeric(as.character(GeneralParameters$new.grid$Values[1]))
		X1 <- as.numeric(as.character(GeneralParameters$new.grid$Values[2]))
		pX <- as.numeric(as.character(GeneralParameters$new.grid$Values[3]))
		Y0 <- as.numeric(as.character(GeneralParameters$new.grid$Values[4]))
		Y1 <- as.numeric(as.character(GeneralParameters$new.grid$Values[5]))
		pY <- as.numeric(as.character(GeneralParameters$new.grid$Values[6]))

		grd.lon <- seq(X0, X1, pX)
		nlon0 <- length(grd.lon)
		grd.lat <- seq(Y0, Y1, pY)
		nlat0 <- length(grd.lat)
		newlocation.merging <- expand.grid(lon = grd.lon, lat = grd.lat)
	}

	coordinates(newlocation.merging)<- ~lon+lat
	grid.loc <- newlocation.merging
	grid.loc <- SpatialPixels(points = grid.loc, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))

	##interpolate dem in new grid
	if(create.grd == '2'){
		dem.grd <- krige(formula = dem ~ 1, locations = downTempdat$demData$demGrd, newdata = newlocation.merging, nmax = 4, nmin = 2, debug.level = 0)
		dem <- ifelse(dem.grd@data$var1.pred < 0,0, dem.grd@data$var1.pred)
		demMask <- matrix(dem, nrow = nlon0, ncol = nlat0)
		demMask[demMask == 0] <- NA
	}else if(create.grd == '1'){
		dem <- c(downTempdat$demData$demMat)
		demMask <- downTempdat$demData$demMat
		demMask[demMask == 0] <- NA
	}

	##Reanalysis over new grid
	xlon <- downTempdat$rfeData$lon
	xlat <- downTempdat$rfeData$lat
	rfegrd <- downTempdat$rfeData$rfeGrd
	ijreanal <- over(rfegrd, grid.loc)

	#dem at reanalysis grid
	dem.reanal <- dem[ijreanal]

	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", grd.lon)
	dy <- ncdim_def("Lat", "degreeN", grd.lat)
	xy.dim <- list(dx, dy)

	paramsDownscl <- list(istart = istart, iend = iend, dem.reanal = dem.reanal, dem = dem, reanalInfo = downTempdat$rfeData, newlocation.merging = newlocation.merging, xy.dim = xy.dim, nlon0 = nlon0, nlat0 = nlat0, origdir = origdir)
	ret <- ReanalysisDownscaling(paramsDownscl)
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	}else return(NULL)	
}

#####################################################
###Compute Mean Bias Coef
TreatBiasCoefTemp <- function(origdir){
	freqData <- GeneralParameters$period
	file.pars <- as.character(GeneralParameters$file.io$Values)
	dir.create(origdir, showWarnings = FALSE)

	all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))

	jfile <- which(all.open.file == file.pars[1])
	donne <- AllOpenFilesData[[jfile]][[2]]

	#######get data
	donne <- splitCDTData(donne, freqData)
	if(is.null(donne)) return(NULL)
	stn.lon <- donne$lon
	stn.lat <- donne$lat
	stn.id <- donne$id
	elv <- donne$elv
	dates <- donne$dates
	donne <- donne$data
	#if(nrow(donne$duplicated.coords) > 0)  #diplay table
	#if(nrow(dat$missing.coords) > 0)

	stnlist <- list(id = stn.id, lon = stn.lon, lat = stn.lat, elv = elv, dates = dates, data = donne)

	###get elevation data
	jncdf <- which(all.open.file == file.pars[2])
	fdem <- AllOpenFilesData[[jncdf]][[2]]
	dem <- fdem$value
	dem[dem < 0] <- 0
	dem.coord <- data.frame(expand.grid(lon = fdem$x, lat = fdem$y))
	coordinates(dem.coord) = ~lon+lat
	demdf <- data.frame(dem = c(dem))
	demdf <- SpatialPointsDataFrame(coords = dem.coord, data = demdf, proj4string = CRS(as.character(NA)))
	demlist <- list(lon = fdem$x, lat = fdem$y, demGrd = demdf)

	coefBiasTempdat <- list(stnData = stnlist, demData = demlist)
	return(coefBiasTempdat)
}


#################################

execCoefBiasCompute <- function(origdir){
	coefBiasTempdat <- TreatBiasCoefTemp(origdir)

	downscaledDir <- as.character(GeneralParameters$file.io$Values[3])
	biasRemoval <- as.character(GeneralParameters$bias.method)

	year1 <- as.numeric(as.character(GeneralParameters$dates.coef$Values[1]))
	year2 <- as.numeric(as.character(GeneralParameters$dates.coef$Values[2]))
	coef.dates <- c(year1, year2)

	downPrefix <- as.character(GeneralParameters$prefix$Values[1])
	#########
	downFile <- list.files(downscaledDir, downPrefix)[1]
	if(is.na(downFile)){
		InsertMessagesTxt(main.txt.out, "Downscaled data not found", format = TRUE)
		return(NULL)
	}
	downfl <- file.path(downscaledDir, downFile, fsep = .Platform$file.sep)
	nc <- nc_open(downfl)
	grd.lon <- nc$dim[[1]]$vals
	grd.lat <- nc$dim[[2]]$vals
	nc_close(nc)
	nlon0 <- length(grd.lon)
	nlat0 <- length(grd.lat)
	newlocation.merging <- expand.grid(lon = grd.lon, lat = grd.lat)
	coordinates(newlocation.merging)<- ~lon+lat
	newlocation.merging <- SpatialPixels(points = newlocation.merging, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))

	##DEM data
	dem <- coefBiasTempdat$demData$demGrd@data[,1]

	##Gauge data
	stn.lon <- coefBiasTempdat$stnData$lon
	stn.lat <- coefBiasTempdat$stnData$lat
	stn.dates <- coefBiasTempdat$stnData$dates
	stn.data <- coefBiasTempdat$stnData$data
	nstn <- length(stn.lon)

	#Index of new grid over stations
	stn.loc <- data.frame(lon = stn.lon, lat = stn.lat)
	stn.loc <- SpatialPoints(stn.loc)
	ijGrd <- over(stn.loc, geometry(newlocation.merging))
	dem.stn <- dem[ijGrd]

	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", grd.lon)
	dy <- ncdim_def("Lat", "degreeN", grd.lat)
	xy.dim <- list(dx, dy)

	#############
	# Extract model values at all station locations
	model_stn <- ExtractReanal2Stn(ijGrd, nstn, coef.dates)
	if(is.null(model_stn)) return(NULL)

	#method 1
	if(biasRemoval == 'Bias-kriging'){
		paramsBias <- list(stnDatas = coefBiasTempdat$stnData, model_stn = model_stn, coef.dates = coef.dates, xy.dim = xy.dim, nlon0 = nlon0, nlat0 = nlat0, newlocation.merging = newlocation.merging, dirBias = origdir)
		ret <- ComputeMeanBias(paramsBias)
	}

	#method 2
	if(biasRemoval == 'Regression-QM'){
		paramsRegQM <- list(stn.data = stn.data, stn.dates = stn.dates, coef.dates = coef.dates, dem.stn = dem.stn, model_stn = model_stn, origdir = origdir)
		ret <- ComputeRegCoeff(paramsRegQM)
	}
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	}else return(NULL)	
}
#################################################

##Adjust downscaled data
TreatAjdBiasDownTemp <- function(){
	freqData <- GeneralParameters$period
	file.pars <- as.character(GeneralParameters$file.io$Values)
	all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))

	jfile <- which(all.open.file == file.pars[1])
	donne <- AllOpenFilesData[[jfile]][[2]]

	#######get data
	donne <- splitCDTData(donne, freqData)
	if(is.null(donne)) return(NULL)
	stn.lon <- donne$lon
	stn.lat <- donne$lat
	stn.id <- donne$id
	elv <- donne$elv
	dates <- donne$dates
	donne <- donne$data
	#if(nrow(donne$duplicated.coords) > 0)  #diplay table
	#if(nrow(dat$missing.coords) > 0)

	stnlist <- list(id = stn.id, lon = stn.lon, lat = stn.lat, elv = elv, dates = dates, data = donne)

	###get elevation data
	jncdf <- which(all.open.file == file.pars[2])
	fdem <- AllOpenFilesData[[jncdf]][[2]]
	dem <- fdem$value
	dem[dem < 0] <- 0
	dem.coord <- data.frame(expand.grid(lon = fdem$x, lat = fdem$y))
	coordinates(dem.coord) = ~lon+lat
	demdf <- data.frame(dem = c(dem))
	demdf <- SpatialPointsDataFrame(coords = dem.coord, data = demdf, proj4string = CRS(as.character(NA)))
	demlist <- list(lon = fdem$x, lat = fdem$y, demGrd = demdf)

	adjdownTempdat <- list(stnData = stnlist, demData = demlist)
	return(adjdownTempdat)
}


#############

execAjdBiasDownTemp <- function(origdir){
	freqData <- GeneralParameters$period
	adjdownTempdat <- TreatAjdBiasDownTemp()

	downscaledDir <- as.character(GeneralParameters$file.io$Values[3])
	biasDirORFile <- as.character(GeneralParameters$file.io$Values[4])
	biasRemoval <- as.character(GeneralParameters$bias.method)
	datesSE <- as.numeric(as.character(GeneralParameters$dates.adj$Values))

	istart <- as.Date(paste(datesSE[1], datesSE[2], datesSE[3], sep = '-'))
	iend <- as.Date(paste(datesSE[4], datesSE[5], datesSE[6], sep = '-'))
	if(freqData == 'dekadal'){
		istart <- paste(format(istart,'%Y%m'), as.numeric(format(istart,'%d')), sep = '')
		iend <- paste(format(iend,'%Y%m'), as.numeric(format(iend,'%d')), sep = '')
	}else{
		istart <- format(istart,'%Y%m%d')
		iend <- format(iend,'%Y%m%d')
	}

	downPrefix <- as.character(GeneralParameters$prefix$Values[1])
	meanBiasPrefix <- as.character(GeneralParameters$prefix$Values[2])
	adjPrefix <- as.character(GeneralParameters$prefix$Values[3])

	#########
	fstdate <- ifelse(freqData == 'monthly', substr(istart, 1,6), istart)
	downFile <- file.path(downscaledDir, paste(downPrefix, '_', fstdate,'.nc', sep = ''), fsep = .Platform$file.sep)
	if(!file.exists(downFile)){
		InsertMessagesTxt(main.txt.out, "Downscaled data not found", format = TRUE)
		return(NULL)
	}
	if(biasRemoval == 'Bias-kriging'){
		biasFile <- file.path(biasDirORFile, paste(meanBiasPrefix, '_1.nc', sep = ''), fsep = .Platform$file.sep)
		if(!file.exists(biasFile)){
			InsertMessagesTxt(main.txt.out, "Mean bias coefficients not found", format = TRUE)
			return(NULL)
		}
	}else{
		if(!file.exists(biasDirORFile)){
			InsertMessagesTxt(main.txt.out, "Regression coefficients not found", format = TRUE)
			return(NULL)
		}
		is.rdble<-!inherits(try(coefReg <- read.table(biasDirORFile, header = TRUE), silent = TRUE), "try-error")
		if(!is.rdble){
			InsertMessagesTxt(main.txt.out, paste("Unable to open file", biasDirORFile), format = TRUE)
			return(NULL)
		}
	}

	nc <- nc_open(downFile)
	grd.lon <- nc$dim[[1]]$vals
	grd.lat <- nc$dim[[2]]$vals
	nc_close(nc)
	nlon0 <- length(grd.lon)
	nlat0 <- length(grd.lat)
	grid.loc <- expand.grid(lon = grd.lon, lat = grd.lat)
	coordinates(grid.loc)<- ~lon+lat
	grid.loc <- SpatialPixels(points = grid.loc, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))

	##DEM data
	dem <- adjdownTempdat$demData$demGrd@data[,1]

	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", grd.lon)
	dy <- ncdim_def("Lat", "degreeN", grd.lat)
	xy.dim <- list(dx, dy)

	#############
	#method 1
	if(biasRemoval == 'Bias-kriging'){
		adjDir <- paste(origdir, '_BiasKR', sep = '')
		dir.create(adjDir, showWarnings = FALSE, recursive = TRUE)
		paramsAdjBs <- list(istart = istart, iend = iend, dem = dem, xy.dim = xy.dim, nlon0 = nlon0, nlat0 = nlat0, downscaledDir = downscaledDir, biasDirORFile = biasDirORFile, adjDir = adjDir, downPrefix = downPrefix, meanBiasPrefix = meanBiasPrefix, adjPrefix = adjPrefix)
		ret <- AjdReanalMeanBias(paramsAdjBs)
	}

	#method 2
	if(biasRemoval == 'Regression-QM'){
		##Gauge data
		stn.lon <- adjdownTempdat$stnData$lon
		stn.lat <- adjdownTempdat$stnData$lat
		#Index of new grid over stations
		stn.loc <- data.frame(lon = stn.lon, lat = stn.lat)
		stn.loc <- SpatialPoints(stn.loc)
		ijGrd <- over(stn.loc, geometry(grid.loc))
		adjDir <- paste(origdir, '_RegQM', sep = '')
		dir.create(adjDir, showWarnings = FALSE, recursive = TRUE)
		paramsAdjBs <- list(istart = istart, iend = iend, stnDatas = adjdownTempdat$stnData, ijGrd = ijGrd, dem = dem, xy.dim = xy.dim, nlon0 = nlon0, nlat0 = nlat0, coefReg = coefReg, downscaledDir = downscaledDir, adjDir = adjDir, downPrefix = downPrefix, adjPrefix = adjPrefix)
		ret <- AjdReanalpmm(paramsAdjBs)
	}
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	}else return(NULL)
}
#######################################################################################
TreatmergeTemp <- function(origdir){
	freqData <- GeneralParameters$period
	file.pars <- as.character(GeneralParameters$file.io$Values)
	dir.create(origdir, showWarnings = FALSE)

	all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))

	jfile <- which(all.open.file == file.pars[1])
	donne <- AllOpenFilesData[[jfile]][[2]]

	#######get data
	donne <- splitCDTData(donne, freqData)
	if(is.null(donne)) return(NULL)
	stn.lon <- donne$lon
	stn.lat <- donne$lat
	stn.id <- donne$id
	elv <- donne$elv
	dates <- donne$dates
	donne <- donne$data
	#if(nrow(donne$duplicated.coords) > 0)  #diplay table
	#if(nrow(dat$missing.coords) > 0)

	stnlist <- list(id = stn.id, lon = stn.lon, lat = stn.lat, elv = elv, dates = dates, data = donne)

	###get elevation data
	if(GeneralParameters$blankGrd == "2"){
		jncdf <- which(all.open.file == file.pars[2])
		fdem <- AllOpenFilesData[[jncdf]][[2]]
		dem <- fdem$value
		dem[dem < 0] <- 0
		dem.coord <- data.frame(expand.grid(lon = fdem$x, lat = fdem$y))
		coordinates(dem.coord) = ~lon+lat
		demdf <- data.frame(dem = c(dem))
		demdf <- SpatialPointsDataFrame(coords = dem.coord, data = demdf, proj4string = CRS(as.character(NA)))
		demlist <- list(lon = fdem$x, lat = fdem$y, demGrd = demdf)
	}else demlist <- NULL
	###get boundaries shape
	if(GeneralParameters$blankGrd == "3"){
		jshp <- which(all.open.file == file.pars[3])
		shpd <- AllOpenFilesData[[jshp]][[2]]
	}else shpd <- NULL

	mrgTempdat <- list(stnData = stnlist, demData = demlist, shpData = shpd)
	return(mrgTempdat)
}

###########################

execMergeTemp <- function(origdir){
	freqData <- GeneralParameters$period
	mrgTempdat <- TreatmergeTemp(origdir)

	datesSE <- as.numeric(as.character(GeneralParameters$dates.mrg$Values))
	istart <- as.Date(paste(datesSE[1], datesSE[2], datesSE[3], sep = '-'))
	iend <- as.Date(paste(datesSE[4], datesSE[5], datesSE[6], sep = '-'))
	if(freqData == 'dekadal'){
		istart <- paste(format(istart,'%Y%m'), as.numeric(format(istart,'%d')), sep = '')
		iend <- paste(format(iend,'%Y%m'), as.numeric(format(iend,'%d')), sep = '')
	}else{
		istart <- format(istart,'%Y%m%d')
		iend <- format(iend,'%Y%m%d')
	}

	adjDir <- as.character(GeneralParameters$file.io$Values[4])
	adjPrefix <- as.character(GeneralParameters$prefix$Values[1])
	mrgPrefix <- as.character(GeneralParameters$prefix$Values[2])
	mrgSuffix <- as.character(GeneralParameters$prefix$Values[3])
	usemask <- as.character(GeneralParameters$blankGrd)

	fstdate <- ifelse(freqData == 'monthly', substr(istart, 1,6), istart)
	downFile <- file.path(adjDir, paste(adjPrefix, '_', fstdate,'.nc', sep = ''), fsep = .Platform$file.sep)
	if(!file.exists(downFile)){
		InsertMessagesTxt(main.txt.out, "Downscaled or adjusted data not found", format = TRUE)
		return(NULL)
	}

	nc <- nc_open(downFile)
	grd.lon <- nc$dim[[1]]$vals
	grd.lat <- nc$dim[[2]]$vals
	nc_close(nc)
	nlon0 <- length(grd.lon)
	nlat0 <- length(grd.lat)
	newlocation.merging <- expand.grid(lon = grd.lon, lat = grd.lat)
	coordinates(newlocation.merging)<- ~lon+lat
	grid.loc <- newlocation.merging
	grid.loc <- SpatialPixels(points = grid.loc, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))

	if(usemask == "1") outMask <- NULL
	if(usemask == "2"){
		dem.grd <- krige(formula = dem ~ 1, locations = mrgTempdat$demData$demGrd, newdata = newlocation.merging, nmax = 5, nmin = 3, debug.level = 0)
		dem <- ifelse(dem.grd@data$var1.pred < 0,0, dem.grd@data$var1.pred)
		outMask <- matrix(dem, nrow = nlon0, ncol = nlat0)
		outMask[outMask == 0] <- NA
	}
	if(usemask == "3"){
		shpd <- mrgTempdat$shpData
		shpd[['vtmp']] <- 1
		shpMask <- over(newlocation.merging, shpd)[,'vtmp']
		outMask <- matrix(shpMask, nrow = nlon0, ncol = nlat0)
	}

	###############################
	##Gauge data
	stn.lon <- mrgTempdat$stnData$lon
	stn.lat <- mrgTempdat$stnData$lat
	stn.dates <- mrgTempdat$stnData$dates
	stn.data <- mrgTempdat$stnData$data

	#Index of new grid over stations
	stn.loc <- data.frame(lon = stn.lon, lat = stn.lat)
	stn.loc <- SpatialPoints(stn.loc)
	ijGrd <- over(stn.loc, geometry(grid.loc))

	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", grd.lon)
	dy <- ncdim_def("Lat", "degreeN", grd.lat)
	xy.dim <- list(dx, dy)

	VarioModel <- c("Sph", "Exp", "Gau")
	mrgParam <- list(dates = c(freqData, istart, iend), prefix = c(adjPrefix, mrgPrefix, mrgSuffix), dirs = c(adjDir, origdir),
	mrgInfo = list(nlon0 = nlon0, nlat0 = nlat0, ijGrd = ijGrd, xy.dim = xy.dim, VarioModel = VarioModel),
	mrgData = list(stnData = mrgTempdat$stnData, outMask = outMask, newlocation.merging = newlocation.merging))
	ret <- MergeTemp(mrgParam)
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	}else return(NULL)
}



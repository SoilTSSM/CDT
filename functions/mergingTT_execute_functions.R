TreatCoefDownTemp <- function(origdir){
	freqData <- GeneralParameters$period
	file.pars <- as.character(GeneralParameters$file.io$Values)
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	#######get data
	donne <- getStnOpenData(file.pars[1])
	donne <- getCDTdataAndDisplayMsg(donne, freqData)
	if(is.null(donne)) return(NULL)

	###get elevation data
	demlist <- getDemOpenDataSPDF(file.pars[2])

	coefdownTempdat <- list(stnData = donne, demData = demlist)
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

	#DEM over stations
	stn.loc <- data.frame(lon = coefdownTempdat$stnData$lon, lat = coefdownTempdat$stnData$lat)
	stn.loc <- SpatialPoints(stn.loc)
	ijGrd <- over(stn.loc, grid.loc)
	dem.stn <- dem[ijGrd]

	#Compute regression parameters between station temperature and elevation
	paramsGlmCoef <- list(coefdownTempdat = coefdownTempdat, dem.stn = dem.stn, origdir = origdir)
	ret <- GlmCoefDownscaling(paramsGlmCoef)
	rm(coefdownTempdat, dem, grid.loc, paramsGlmCoef, dem.stn)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

#######################################################################################

TreatDownscalingTemp <- function(origdir){
	file.pars <- as.character(GeneralParameters$file.io$Values)
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	###get elevation data
	demlist <- getDemOpenDataSPDF(file.pars[2])

	##RFE sample file
	rfelist <- getRFESampleData(file.pars[3])

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
		istart <- paste(format(istart, '%Y%m'), as.numeric(format(istart, '%d')), sep = '')
		iend <- paste(format(iend, '%Y%m'), as.numeric(format(iend, '%d')), sep = '')
	}else{
		istart <- format(istart, '%Y%m%d')
		iend <- format(iend, '%Y%m%d')
	}

	############################################
	##Create grid for interpolation
	if(create.grd == '1'){
		grd.lon <- downTempdat$demData$lon
		grd.lat <- downTempdat$demData$lat
	}else if(create.grd == '2'){
		X0 <- as.numeric(as.character(GeneralParameters$new.grid$Values[1]))
		X1 <- as.numeric(as.character(GeneralParameters$new.grid$Values[2]))
		pX <- as.numeric(as.character(GeneralParameters$new.grid$Values[3]))
		Y0 <- as.numeric(as.character(GeneralParameters$new.grid$Values[4]))
		Y1 <- as.numeric(as.character(GeneralParameters$new.grid$Values[5]))
		pY <- as.numeric(as.character(GeneralParameters$new.grid$Values[6]))
		grd.lon <- seq(X0, X1, pX)
		grd.lat <- seq(Y0, Y1, pY)
	}
	nlon0 <- length(grd.lon)
	nlat0 <- length(grd.lat)
	newlocation.merging <- expand.grid(lon = grd.lon, lat = grd.lat)
 	coordinates(newlocation.merging)<- ~lon+lat
	grid.loc <- defSpatialPixels(list(lon = grd.lon, lat = grd.lat))
	
	##interpolate dem in new grid
	if(create.grd == '2'){
		demGrid <- defSpatialPixels(list(lon = downTempdat$demData$lon, lat = downTempdat$demData$lat))
		is.regridDEM <- is.sameSpatialPixelsObj(grid.loc, demGrid, tol = 1e-07)
		if(is.regridDEM){
			dem <- regridDEMFun(downTempdat$demData, list(lon = grd.lon, lat = grd.lat), regrid = 'BLW')
			# dem <- regridDEMFun(downTempdat$demData, list(lon = grd.lon, lat = grd.lat), regrid = 'RASTER', method = "bilinear")
			# dem <- regridDEMFun(downTempdat$demData, list(lon = grd.lon, lat = grd.lat), regrid = 'IDW', nmax = 4, nmin = 2)
		}else{
			dem <- downTempdat$demData$demGrd@data[, 1]
		}
		demMask <- matrix(dem, nrow = nlon0, ncol = nlat0)
		demMask[demMask == 0] <- NA
	}else if(create.grd == '1'){
		dem <- c(downTempdat$demData$demMat)
		demMask <- downTempdat$demData$demMat
		demMask[demMask == 0] <- NA
	}

	##Reanalysis over new grid
	rfegrd <- downTempdat$rfeData$rfeGrd
	ijreanal <- over(rfegrd, grid.loc)

	#dem at reanalysis grid
	dem.reanal <- dem[ijreanal]

	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", grd.lon)
	dy <- ncdim_def("Lat", "degreeN", grd.lat)
	xy.dim <- list(dx, dy)

	paramsDownscl <- list(istart = istart, iend = iend, dem.reanal = dem.reanal, dem = dem,
							reanalInfo = downTempdat$rfeData, newlocation.merging = newlocation.merging, xy.dim = xy.dim,
							nlon0 = nlon0, nlat0 = nlat0, origdir = origdir)
	ret <- ReanalysisDownscaling(paramsDownscl)
	rm(paramsDownscl, downTempdat, newlocation.merging, grid.loc, rfegrd, dem.reanal)
	gc()
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
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	#######get data
	donne <- getStnOpenData(file.pars[1])
	donne <- getCDTdataAndDisplayMsg(donne, freqData)
	if(is.null(donne)) return(NULL)

	###get elevation data
	demlist <- getDemOpenDataSPDF(file.pars[2])

	coefBiasTempdat <- list(stnData = donne, demData = demlist)
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
	downFile <- file.path(downscaledDir, downFile, fsep = .Platform$file.sep)
	nc <- nc_open(downFile)
	grd.lon <- nc$dim[[1]]$vals
	grd.lat <- nc$dim[[2]]$vals
	nc_close(nc)
	nlon0 <- length(grd.lon)
	nlat0 <- length(grd.lat)

	newlocation.merging <- defSpatialPixels(list(lon = grd.lon, lat = grd.lat))

	ijGrd <- grid2pointINDEX(list(lon = coefBiasTempdat$stnData$lon, lat = coefBiasTempdat$stnData$lat),
							list(lon = grd.lon, lat = grd.lat))

	demGrid <- defSpatialPixels(list(lon = coefBiasTempdat$demData$lon, lat = coefBiasTempdat$demData$lat))
	is.regridDEM <- is.sameSpatialPixelsObj(newlocation.merging, demGrid, tol = 1e-07)
	if(is.regridDEM){
		dem <- regridDEMFun(coefBiasTempdat$demData, list(lon = grd.lon, lat = grd.lat), regrid = 'BLW')
		# dem <- regridDEMFun(coefBiasTempdat$demData, list(lon = grd.lon, lat = grd.lat), regrid = 'RASTER', method = "bilinear")
		# dem <- regridDEMFun(coefBiasTempdat$demData, list(lon = grd.lon, lat = grd.lat), regrid = 'IDW', nmax = 4, nmin = 2)
	}else{
		dem <- coefBiasTempdat$demData$demGrd@data[, 1]
	}
	dem[is.na(dem)] <- 0
	dem.stn <- dem[ijGrd]

	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", grd.lon)
	dy <- ncdim_def("Lat", "degreeN", grd.lat)
	xy.dim <- list(dx, dy)

	#############
	# Extract model values at all station locations
	nstn <- length(coefBiasTempdat$stnData$lon)
	model_stn <- ExtractReanal2Stn(ijGrd, nstn, coef.dates)
	if(is.null(model_stn)) return(NULL)

	#method 1
	if(biasRemoval == 'Bias-kriging'){
		paramsBias <- list(coefBiasTempdat = coefBiasTempdat, model_stn = model_stn, coef.dates = coef.dates,
							xy.dim = xy.dim, nlon0 = nlon0, nlat0 = nlat0, newlocation.merging = newlocation.merging,
							dirBias = origdir, dem = dem, dem.stn = dem.stn)
		ret <- ComputeMeanBias(paramsBias)
	}

	#method 2
	if(biasRemoval == 'Regression-QM'){
		paramsRegQM <- list(coefBiasTempdat = coefBiasTempdat, coef.dates = coef.dates, ijGrd = ijGrd,
							model_stn = model_stn, origdir = origdir)
		ret <- ComputeRegCoeff(paramsRegQM)
	}
	rm(coefBiasTempdat, paramsBias, paramsRegQM, model_stn, dem, dem.stn, demGrid)
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

	#######get data
	donne <- getStnOpenData(file.pars[1])
	donne <- getCDTdataAndDisplayMsg(donne, freqData)
	if(is.null(donne)) return(NULL)

	###get elevation data
	demlist <- getDemOpenDataSPDF(file.pars[2])

	adjdownTempdat <- list(stnData = donne, demData = demlist)
	return(adjdownTempdat)
}

#################################################

execAjdBiasDownTemp <- function(origdir){
	freqData <- GeneralParameters$period
	adjdownTempdat <- TreatAjdBiasDownTemp()

	downscaledDir <- as.character(GeneralParameters$file.io$Values[3])
	biasDirORFile <- as.character(GeneralParameters$file.io$Values[4])
	biasRemoval <- as.character(GeneralParameters$bias.method)

	downPrefix <- as.character(GeneralParameters$prefix$Values[1])
	meanBiasPrefix <- as.character(GeneralParameters$prefix$Values[2])
	adjPrefix <- as.character(GeneralParameters$prefix$Values[3])

	datesSE <- as.numeric(as.character(GeneralParameters$dates.adj$Values))

	##Get all downscaled Files
	if(freqData == 'daily'){
		adj.dates <- format(seq(as.Date(paste(datesSE[1], datesSE[2], datesSE[3], sep = '-')),
					as.Date(paste(datesSE[4], datesSE[5], datesSE[6], sep = '-')), 'day'), '%Y%m%d')
		testfile <- file.path(downscaledDir, paste(downPrefix, '_', adj.dates, '.nc', sep = ''), fsep = .Platform$file.sep)
	}
	if(freqData == 'dekadal'){
		adj.dates <- seq(as.Date(paste(datesSE[1], datesSE[2], datesSE[3], sep = '-')),
					as.Date(paste(datesSE[4], datesSE[5], datesSE[6], sep = '-')), 'day')
		adj.dates <- paste(format(adj.dates[which(as.numeric(format(adj.dates, '%d')) <= 3)], '%Y%m'),
					as.numeric(format(adj.dates[which(as.numeric(format(adj.dates, '%d')) <= 3)], '%d')), sep = '')
		testfile <- file.path(downscaledDir, paste(downPrefix, '_', adj.dates, '.nc', sep = ''), fsep = .Platform$file.sep)
	}
	if(freqData == 'monthly'){
		adj.dates <- format(seq(as.Date(paste(datesSE[1], datesSE[2], 1, sep = '-')),
					as.Date(paste(datesSE[4], datesSE[5], 1, sep = '-')), 'month'), '%Y%m')
		testfile <- file.path(downscaledDir, paste(downPrefix, '_', adj.dates, '.nc', sep = ''), fsep = .Platform$file.sep)
	}

	existFl <- unlist(lapply(testfile, file.exists))
	if(!any(existFl)){
		InsertMessagesTxt(main.txt.out, "Downscaled data not found", format = TRUE)
		return(NULL)
	}
	downDataFl <- testfile[existFl]
	adj.dates <- adj.dates[existFl]

	#########
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

	nc <- nc_open(downDataFl[1])
	grd.lon <- nc$dim[[1]]$vals
	grd.lat <- nc$dim[[2]]$vals
	nc_close(nc)
	nlon0 <- length(grd.lon)
	nlat0 <- length(grd.lat)

	grid.loc <- defSpatialPixels(list(lon = grd.lon, lat = grd.lat))

	##DEM data
	demGrid <- defSpatialPixels(list(lon = adjdownTempdat$demData$lon, lat = adjdownTempdat$demData$lat))
	is.regridDEM <- is.sameSpatialPixelsObj(grid.loc, demGrid, tol = 1e-07)
	if(is.regridDEM){
		dem <- regridDEMFun(adjdownTempdat$demData, list(lon = grd.lon, lat = grd.lat), regrid = 'BLW')
		# dem <- regridDEMFun(adjdownTempdat$demData, list(lon = grd.lon, lat = grd.lat), regrid = 'RASTER', method = "bilinear")
		# dem <- regridDEMFun(adjdownTempdat$demData, list(lon = grd.lon, lat = grd.lat), regrid = 'IDW', nmax = 4, nmin = 2)
	}else{
		dem <- adjdownTempdat$demData$demGrd@data[, 1]
	}

	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", grd.lon)
	dy <- ncdim_def("Lat", "degreeN", grd.lat)
	xy.dim <- list(dx, dy)

	#############
	#method 1
	if(biasRemoval == 'Bias-kriging'){
		adjDir <- paste(origdir, '_BiasKR', sep = '')
		dir.create(adjDir, showWarnings = FALSE, recursive = TRUE)
		paramsAdjBs <- list(downDataFl = downDataFl, adj.dates = adj.dates, dem = dem, xy.dim = xy.dim,
							nlon0 = nlon0, nlat0 = nlat0, biasDirORFile = biasDirORFile, adjDir = adjDir,
							downPrefix = downPrefix, meanBiasPrefix = meanBiasPrefix, adjPrefix = adjPrefix)
		ret <- AjdReanalMeanBias(paramsAdjBs)
	}

	#method 2
	if(biasRemoval == 'Regression-QM'){
		#Index of new grid over stations
		ijGrd <- grid2pointINDEX(list(lon = adjdownTempdat$stnData$lon, lat = adjdownTempdat$stnData$lat),
								list(lon = grd.lon, lat = grd.lat))

		adjDir <- paste(origdir, '_RegQM', sep = '')
		dir.create(adjDir, showWarnings = FALSE, recursive = TRUE)
		paramsAdjBs <- list(downDataFl = downDataFl, adj.dates = adj.dates, stnDatas = adjdownTempdat$stnData,
							ijGrd = ijGrd, dem = dem, xy.dim = xy.dim, nlon0 = nlon0, nlat0 = nlat0,
							coefReg = coefReg, adjDir = adjDir, downPrefix = downPrefix, adjPrefix = adjPrefix)
		ret <- AjdReanalpmm(paramsAdjBs)
	}
	rm(adjdownTempdat, paramsAdjBs, grid.loc, demGrid, dem)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	}else return(NULL)
}

#######################################################################################

TreatmergeTemp <- function(origdir){
	freqData <- GeneralParameters$period
	file.pars <- as.character(GeneralParameters$file.io$Values)
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	#######get data
	donne <- getStnOpenData(file.pars[1])
	donne <- getCDTdataAndDisplayMsg(donne, freqData)
	if(is.null(donne)) return(NULL)

	##get elevation data	
	if(GeneralParameters$blankGrd == "2") demlist <- getDemOpenDataSPDF(file.pars[2])
	else demlist <- NULL

	##Shapefile
	if(GeneralParameters$blankGrd == "3") shpd <- getShpOpenData(file.pars[3])[[2]]
	else shpd <- NULL

	mrgTempdat <- list(stnData = donne, demData = demlist, shpData = shpd)
	return(mrgTempdat)
}

###########################

execMergeTemp <- function(origdir){
	freqData <- GeneralParameters$period
	mrgTempdat <- TreatmergeTemp(origdir)

	adjDir <- as.character(GeneralParameters$file.io$Values[4])
	adjPrefix <- as.character(GeneralParameters$prefix$Values[1])
	mrgPrefix <- as.character(GeneralParameters$prefix$Values[2])
	mrgSuffix <- as.character(GeneralParameters$prefix$Values[3])
	usemask <- as.character(GeneralParameters$blankGrd)

	datesSE <- as.numeric(as.character(GeneralParameters$dates.mrg$Values))
	if(freqData == 'daily'){
		mrg.dates <- format(seq(as.Date(paste(datesSE[1], datesSE[2], datesSE[3], sep = '-')),
						as.Date(paste(datesSE[4], datesSE[5], datesSE[6], sep = '-')), 'day'), '%Y%m%d')
		testfile <- file.path(adjDir, paste(adjPrefix, '_', mrg.dates, '.nc', sep = ''), fsep = .Platform$file.sep)
	}
	if(freqData == 'dekadal'){
		mrg.dates <- seq(as.Date(paste(datesSE[1], datesSE[2], datesSE[3], sep = '-')),
						as.Date(paste(datesSE[4], datesSE[5], datesSE[6], sep = '-')), 'day')
		mrg.dates <- paste(format(mrg.dates[which(as.numeric(format(mrg.dates, '%d')) <= 3)], '%Y%m'),
						as.numeric(format(mrg.dates[which(as.numeric(format(mrg.dates, '%d')) <= 3)], '%d')), sep = '')
		testfile <- file.path(adjDir, paste(adjPrefix, '_', mrg.dates, '.nc', sep = ''), fsep = .Platform$file.sep)
	}
	if(freqData == 'monthly'){
		mrg.dates <- format(seq(as.Date(paste(datesSE[1], datesSE[2], 1, sep = '-')),
							as.Date(paste(datesSE[4], datesSE[5], 1, sep = '-')), 'month'), '%Y%m')
		testfile <- file.path(adjDir, paste(adjPrefix, '_', mrg.dates, '.nc', sep = ''), fsep = .Platform$file.sep)
	}
	existFl <- unlist(lapply(testfile, file.exists))
	if(!any(existFl)){
		InsertMessagesTxt(main.txt.out, "Adjusted data not found", format = TRUE)
		return(NULL)
	}
	adjDataFl <- testfile[existFl]
	mrg.dates <- mrg.dates[existFl]

	nc <- nc_open(adjDataFl[1])
	grd.lon <- nc$dim[[1]]$vals
	grd.lat <- nc$dim[[2]]$vals
	nc_close(nc)
	nlon0 <- length(grd.lon)
	nlat0 <- length(grd.lat)
	newlocation.merging <- expand.grid(lon = grd.lon, lat = grd.lat)
	coordinates(newlocation.merging)<- ~lon+lat

	if(usemask == "1") outMask <- NULL
	if(usemask == "2"){
		grid.loc <- defSpatialPixels(list(lon = grd.lon, lat = grd.lat))
		demGrid <- defSpatialPixels(list(lon = mrgTempdat$demData$lon, lat = mrgTempdat$demData$lat))
		is.regridDEM <- is.sameSpatialPixelsObj(grid.loc, demGrid, tol = 1e-07)
		if(is.regridDEM){
			dem <- regridDEMFun(mrgTempdat$demData, list(lon = grd.lon, lat = grd.lat), regrid = 'BLW')
			# dem <- regridDEMFun(mrgTempdat$demData, list(lon = grd.lon, lat = grd.lat), regrid = 'RASTER', method = "bilinear")
			# dem <- regridDEMFun(mrgTempdat$demData, list(lon = grd.lon, lat = grd.lat), regrid = 'IDW', nmax = 4, nmin = 2)
		}else{
			dem <- mrgTempdat$demData$demGrd@data[, 1]
		}
		outMask <- matrix(dem, nrow = nlon0, ncol = nlat0)
		outMask[outMask == 0] <- NA
	}
	if(usemask == "3"){
		shpd <- mrgTempdat$shpData
		shpd[['vtmp']] <- 1
		shpMask <- over(newlocation.merging, shpd)[,'vtmp']
		outMask <- matrix(shpMask, nrow = nlon0, ncol = nlat0)
	}

	#Index of new grid over stations
	ijGrd <- grid2pointINDEX(list(lon = mrgTempdat$stnData$lon, lat = mrgTempdat$stnData$lat),
							list(lon = grd.lon, lat = grd.lat))

	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", grd.lon)
	dy <- ncdim_def("Lat", "degreeN", grd.lat)
	xy.dim <- list(dx, dy)

	VarioModel <- c("Sph", "Exp", "Gau")
	mrgParam <- list(adjDataFl = adjDataFl, mrg.dates = mrg.dates, mrgDir = origdir, prefix = c(mrgPrefix, mrgSuffix),
	mrgInfo = list(nlon0 = nlon0, nlat0 = nlat0, ijGrd = ijGrd, xy.dim = xy.dim, VarioModel = VarioModel),
	mrgData = list(stnData = mrgTempdat$stnData, outMask = outMask, newlocation.merging = newlocation.merging))
	ret <- MergeTemp(mrgParam)
	rm(mrgTempdat, mrgParam, outMask, newlocation.merging)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	}else return(NULL)
}




TreatbiasRain <- function(origdir){
	freqData <- GeneralParameters$period
	file.pars <- as.character(GeneralParameters$file.io$Values)
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	#######get data
	donne <- getStnOpenData(file.pars[1])
	donne <- getCDTdataAndDisplayMsg(donne, freqData)
	if(is.null(donne)) return(NULL)
	
	###get elevation data
	if(GeneralParameters$CreateGrd == "2") demlist <- getDemOpenDataSPDF(file.pars[2])
	else demlist <- NULL
		
	# ##RFE sample file
	rfelist <- getRFESampleData(file.pars[3])

	mrgRaindat <- list(stnData = donne, demData = demlist, rfeData = rfelist)
	
	outfile <- file.path(origdir, 'DataUsed2ComputeBias.RData', fsep = .Platform$file.sep)
	save(mrgRaindat, file = outfile)

	return(mrgRaindat)
}

##############################################################################################################

execBiasRain <- function(origdir){
	freqData <- GeneralParameters$period
	mrgRaindat <- TreatbiasRain(origdir)
	if(is.null(mrgRaindat)) return(NULL)

	create.grd <- as.character(GeneralParameters$CreateGrd)
	##Create grid for interpolation
	if(create.grd == '1'){
		grd.lon <- mrgRaindat$rfeData$lon
		grd.lat <- mrgRaindat$rfeData$lat
	}else if(create.grd == '2'){
		grd.lon <- mrgRaindat$demData$lon
		grd.lat <- mrgRaindat$demData$lat
	}else if(create.grd == '3'){
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

	newlocation.grid <- defSpatialPixels(list(lon = grd.lon, lat = grd.lat))
	ijGrd <- grid2pointINDEX(list(lon = mrgRaindat$stnData$lon, lat = mrgRaindat$stnData$lat),
								list(lon = grd.lon, lat = grd.lat))

	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", grd.lon)
	dy <- ncdim_def("Lat", "degreeN", grd.lat)
	xy.dim <- list(dx, dy)

	paramGrd <- list(nlon0 = nlon0, nlat0 = nlat0, xy.dim = xy.dim, newlocation.grid = newlocation.grid)
	mrgRaindat$stnData$paramGrd <- paramGrd
	outfile <- file.path(origdir, 'DataUsed2ComputeBias.RData', fsep = .Platform$file.sep)
	save(mrgRaindat, file = outfile)

	extractRFEparms <- list(ijGrd = ijGrd, GeneralParameters = GeneralParameters, mrgRaindat = mrgRaindat)
	rfe_stn <- ExtractRFE2Stn(extractRFEparms)
	if(is.null(rfe_stn)) return(NULL)
	comptMBiasparms <- list(rfe_stn = rfe_stn, GeneralParameters = GeneralParameters,
							 mrgRaindat = mrgRaindat, paramGrd = paramGrd, origdir = origdir)
	ret <- ComputeMeanBiasRain(comptMBiasparms)
	rm(comptMBiasparms, mrgRaindat, extractRFEparms, rfe_stn, newlocation.grid, paramGrd)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

##############################################################################################################
execAdjBiasRain <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	rfeData <- getRFESampleData(as.character(GeneralParameters$file.io$Values[1]))

	dataBiasfl <- file.path(as.character(GeneralParameters$file.io$Values[3]), 'DataUsed2ComputeBias.RData', fsep = .Platform$file.sep)
	load(dataBiasfl)
	paramGrd <- mrgRaindat$stnData$paramGrd

	freqData <- GeneralParameters$period
	datesSE <- as.numeric(as.character(GeneralParameters$dates.adj$Values))

	istart <- as.Date(paste(datesSE[1], datesSE[2], datesSE[3], sep = '-'))
	iend <- as.Date(paste(datesSE[4], datesSE[5], datesSE[6], sep = '-'))
	if(freqData == 'dekadal'){
		istart <- paste(format(istart, '%Y%m'), as.numeric(format(istart, '%d')), sep = '')
		iend <- paste(format(iend, '%Y%m'), as.numeric(format(iend, '%d')), sep = '')
	}else{
		istart <- format(istart, '%Y%m%d')
		iend <- format(iend, '%Y%m%d')
	}
	adjMeanBiasparms <- list(freqData = freqData, istart = istart, iend = iend, rfeData = rfeData,
	 						paramGrd = paramGrd, GeneralParameters = GeneralParameters, origdir = origdir)
	ret <- AjdMeanBiasRain(adjMeanBiasparms)
	rm(adjMeanBiasparms, mrgRaindat, paramGrd)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

##############################################################################################################
####Merging

TreatMergeRain <- function(origdir, freqData){
	file.pars <- as.character(GeneralParameters$file.io$Values)
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	#######get data
	donne <- getStnOpenData(file.pars[1])
	donne <- getCDTdataAndDisplayMsg(donne, freqData)
	if(is.null(donne)) return(NULL)

	###get elevation data	
	if(GeneralParameters$NewGrd == '1' & GeneralParameters$CreateGrd == "2") demlist1 <- getDemOpenDataSPDF(file.pars[2])
	else demlist1 <- NULL
	if(GeneralParameters$blankGrd == "2") demlist2 <- getDemOpenDataSPDF(file.pars[7])
	else demlist2 <- NULL

	##RFE sample file
	if(GeneralParameters$NewGrd == '1') rfelist <- getRFESampleData(file.pars[3])
	else rfelist <- NULL
		
	##Shapefile
	if(GeneralParameters$blankGrd == "3") shpd <- getShpOpenData(file.pars[6])[[2]]
	else shpd <- NULL

	mrgRaindat <- list(stnData = donne, demData1 = demlist1, demData2 = demlist2, rfeData = rfelist, shpData = shpd)
	return(mrgRaindat)
}

##############################################################################################################
execMergeRain <- function(origdir){
	freqData <- GeneralParameters$period
	mrgRaindat <- TreatMergeRain(origdir, freqData)

	####Create grid
	if(GeneralParameters$NewGrd == '1'){
		create.grd <- as.character(GeneralParameters$CreateGrd)
		##Create grid for interpolation
		if(create.grd == '1'){
			grd.lon <- mrgRaindat$rfeData$lon
			grd.lat <- mrgRaindat$rfeData$lat
		}else if(create.grd == '2'){
			grd.lon <- mrgRaindat$demData1$lon
			grd.lat <- mrgRaindat$demData1$lat
		}else if(create.grd == '3'){
			X0 <- as.numeric(as.character(GeneralParameters$new.grid$Values[1]))
			X1 <- as.numeric(as.character(GeneralParameters$new.grid$Values[2]))
			pX <- as.numeric(as.character(GeneralParameters$new.grid$Values[3]))
			Y0 <- as.numeric(as.character(GeneralParameters$new.grid$Values[4]))
			Y1 <- as.numeric(as.character(GeneralParameters$new.grid$Values[5]))
			pY <- as.numeric(as.character(GeneralParameters$new.grid$Values[6]))
			grd.lon <- seq(X0, X1, pX)
			grd.lat <- seq(Y0, Y1, pY)
		}
	}else{
		adjDir <- as.character(GeneralParameters$file.io$Values[4])
		adjPrefix <- as.character(GeneralParameters$prefix$Values[1])
		adjFile <- list.files(adjDir, adjPrefix)[1]
		if(is.na(adjFile)){
			InsertMessagesTxt(main.txt.out, "Adjusted RFE data not found", format = TRUE)
			return(NULL)
		}
		adjFile <- file.path(adjDir, adjFile, fsep = .Platform$file.sep)
		nc <- nc_open(adjFile)
		grd.lon <- nc$dim[[1]]$vals
		grd.lat <- nc$dim[[2]]$vals
		nc_close(nc)
	}
	nlon0 <- length(grd.lon)
	nlat0 <- length(grd.lat)

	newlocation.merging <- defSpatialPixels(list(lon = grd.lon, lat = grd.lat))

	########Blank mask
	usemask <- as.character(GeneralParameters$blankGrd)
	if(usemask == "1") outMask <- NULL
	if(usemask == "2"){
		grid.loc <- defSpatialPixels(list(lon = grd.lon, lat = grd.lat))
		demGrid <- defSpatialPixels(list(lon = mrgRaindat$demData2$lon, lat = mrgRaindat$demData2$lat))
		is.regridDEM <- is.sameSpatialPixelsObj(grid.loc, demGrid, tol = 1e-07)
		if(is.regridDEM){
			dem <- regridDEMFun(mrgRaindat$demData2, list(lon = grd.lon, lat = grd.lat), regrid = 'BLW')
			# dem <- regridDEMFun(mrgRaindat$demData2, list(lon = grd.lon, lat = grd.lat), regrid = 'RASTER', method = "bilinear")
			# dem <- regridDEMFun(mrgRaindat$demData2, list(lon = grd.lon, lat = grd.lat), regrid = 'IDW', nmax = 4, nmin = 2)
		}else{
			dem <- mrgRaindat$demData2$demGrd@data[, 1]
		}
		outMask <- matrix(dem, nrow = nlon0, ncol = nlat0)
		outMask[outMask == 0] <- NA
		rm(dem.grd, dem)
	}
	if(usemask == "3"){
		shpd <- mrgRaindat$shpData
		shpd[['vtmp']] <- 1
		shpMask <- over(newlocation.merging, shpd)[, 'vtmp']
		outMask <- matrix(shpMask, nrow = nlon0, ncol = nlat0)
	}

	#Index of new grid over stations
	ijGrd <- grid2pointINDEX(list(lon = mrgRaindat$stnData$lon, lat = mrgRaindat$stnData$lat),
							list(lon = grd.lon, lat = grd.lat))

	#########
	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", grd.lon)
	dy <- ncdim_def("Lat", "degreeN", grd.lat)
	xy.dim <- list(dx, dy)

	VarioModel <- c("Sph", "Exp", "Gau")
	paramsMRG <- list(mrgRaindat = mrgRaindat, VarioModel = VarioModel, origdir = origdir,
						GeneralParameters = GeneralParameters, 
						ijGrd = ijGrd, nlon0 = nlon0, nlat0 = nlat0, xy.dim = xy.dim,
						outMask = outMask, newlocation.merging = newlocation.merging)
	ret <- MergingFunction(paramsMRG)
	rm(paramsMRG, mrgRaindat, outMask, newlocation.merging)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}


##############################################################################################################



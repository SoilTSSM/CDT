
execBiasRain <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	memType <- 2

	create.grd <- GeneralParameters$Create.Grid
	freqData <- GeneralParameters$period

	#######get data
	stnData <- getStnOpenData(GeneralParameters$IO.files$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	if(is.null(stnData)) return(NULL)
	
	###get elevation data
	demData <- getDemOpenDataSPDF(GeneralParameters$IO.files$DEM.file)
	if(is.null(demData)) return(NULL)

	# # ##RFE sample file
	rfeDataInfo <- getRFESampleData(GeneralParameters$IO.files$RFE.file)
	if(is.null(rfeDataInfo)) return(NULL)
	xy.rfe <- list(lon = rfeDataInfo$lon, lat = rfeDataInfo$lat)

	##Create grid for interpolation
	if(create.grd == '1'){
		grd.lon <- rfeDataInfo$lon
		grd.lat <- rfeDataInfo$lat
	}else if(create.grd == '2'){
		grd.lon <- demData$lon
		grd.lat <- demData$lat
	}else if(create.grd == '3'){
		X0 <- GeneralParameters$New.Grid.Def$minlon
		X1 <- GeneralParameters$New.Grid.Def$maxlon
		pX <- GeneralParameters$New.Grid.Def$reslon
		Y0 <- GeneralParameters$New.Grid.Def$minlat
		Y1 <- GeneralParameters$New.Grid.Def$maxlat
		pY <- GeneralParameters$New.Grid.Def$reslat
		grd.lon <- seq(X0, X1, pX)
		grd.lat <- seq(Y0, Y1, pY)
	}
	nlon0 <- length(grd.lon)
	nlat0 <- length(grd.lat)
	xy.grid <- list(lon = grd.lon, lat = grd.lat)

	################

	interp.method <- GeneralParameters$Interpolation.pars$interp.method
	rad.lon <- as.numeric(GeneralParameters$Interpolation.pars$rad.lon)
	rad.lat <- as.numeric(GeneralParameters$Interpolation.pars$rad.lat)
	maxdist <- as.numeric(GeneralParameters$Interpolation.pars$maxdist)

	res.coarse <- if(interp.method == 'NN') sqrt((rad.lon*mean(grd.lon[-1]-grd.lon[-nlon0]))^2 + (rad.lat*mean(grd.lat[-1]-grd.lat[-nlat0]))^2)/2 else maxdist/2
	res.coarse <- if(res.coarse  >= 0.25) res.coarse else 0.25

	################
	year1 <- as.numeric(GeneralParameters$Bias.Date.Range$start.year)
	year2 <- as.numeric(GeneralParameters$Bias.Date.Range$end.year)
	months <- as.numeric(GeneralParameters$Bias.Months)
	rfeDir <- GeneralParameters$IO.files$RFE.dir
	rfefilefrmt <- GeneralParameters$Prefix$RFE.File.Format

	start.date <- as.Date(paste(year1, '0101', sep = ''), format = '%Y%m%d')
	end.date <- as.Date(paste(year2, '1231', sep = ''), format = '%Y%m%d')

	################
	## RFE DATA
	msg <- list(start = 'Read RFE data ...', end = 'Reading RFE data finished')
	errmsg <- "RFE data not found"
	ncfiles <- list(freqData = freqData, start.date = start.date, end.date = end.date,
					months = months, ncDir = rfeDir, ncFileFormat = rfefilefrmt)
	ncinfo <- list(xo = rfeDataInfo$rfeILon, yo = rfeDataInfo$rfeILat, varid = rfeDataInfo$rfeVarid)
	read.ncdf.parms <- list(ncfiles = ncfiles, ncinfo = ncinfo, msg = msg, errmsg = errmsg)

	###############
	comptMBiasparms <- list(GeneralParameters = GeneralParameters, stnData = stnData, rfeData = read.ncdf.parms,
							xy.rfe = xy.rfe, res.coarse = res.coarse, memType = memType, origdir = origdir)
	bias.pars <- ComputeMeanBiasRain(comptMBiasparms)

	###############
	# load('~/Desktop/merging/ETH_ENACTS/RR_DEK/QM_IDW0.6_noDEM/BIAS_PARAMS.RData')
	# bias.pars <- bias.pars$bias

	###############

	interpBiasparams <- list(GeneralParameters = GeneralParameters, bias.pars = bias.pars, stnData = stnData,
							demData = demData, xy.grid = xy.grid, xy.rfe = xy.rfe, res.coarse = res.coarse, origdir = origdir)
	ret <- InterpolateMeanBiasRain(interpBiasparams)

	rm(comptMBiasparms, stnData, demData, rfeDataInfo, rfeData, bias.pars, interpBiasparams)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

##############################################################################################################

execAdjBiasRain <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	memType <- 2

	###get elevation data
	rfeDataInfo <- getRFESampleData(GeneralParameters$IO.files$RFE.file)

	################
	freqData <- GeneralParameters$period
	start.year <- GeneralParameters$Adjust.Date.Range$start.year
	start.mon <- GeneralParameters$Adjust.Date.Range$start.mon
	start.dek <- GeneralParameters$Adjust.Date.Range$start.dek
	end.year <- GeneralParameters$Adjust.Date.Range$end.year
	end.mon <- GeneralParameters$Adjust.Date.Range$end.mon
	end.dek <- GeneralParameters$Adjust.Date.Range$end.dek
	months <- sort(as.numeric(GeneralParameters$Adjust.Months))

	rfeDir <- GeneralParameters$IO.files$RFE.dir
	rfefilefrmt <- GeneralParameters$Prefix$RFE.File.Format

	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	################
	## RFE DATA
	msg <- list(start = 'Read RFE data ...', end = 'Reading RFE data finished')
	errmsg <- "RFE data not found"
	ncfiles <- list(freqData = freqData, start.date = start.date, end.date = end.date,
					months = months, ncDir = rfeDir, ncFileFormat = rfefilefrmt)
	ncinfo <- list(xo = rfeDataInfo$rfeILon, yo = rfeDataInfo$rfeILat, varid = rfeDataInfo$rfeVarid)
	read.ncdf.parms <- list(ncfiles = ncfiles, ncinfo = ncinfo, msg = msg, errmsg = errmsg)

	adjMeanBiasparms <- list(rfeData = read.ncdf.parms, GeneralParameters = GeneralParameters,
							origdir = origdir, memType = memType, readRFE = TRUE, RFEDATA = NULL)
	# readRFE = FALSE, RFEDATA = rfeData # exec fonction ailleurs, multiple sources
	ret <- AjdMeanBiasRain(adjMeanBiasparms)
	rm(adjMeanBiasparms, rfeDataInfo)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

##############################################################################################################
#### Compute LM coef

execLMCoefRain <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	memType <- 2

	freqData <- GeneralParameters$period
	create.grd <- GeneralParameters$Create.Grid

	#######get data
	stnData <- getStnOpenData(GeneralParameters$IO.files$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	if(is.null(stnData)) return(NULL)

	###get elevation data
	demData <- getDemOpenDataSPDF(GeneralParameters$IO.files$DEM.file)
	if(is.null(demData)) return(NULL)

	## RFE DATA
	rfeDataInfo <- getRFESampleData(GeneralParameters$IO.files$RFE.file)

	##Create grid for interpolation
	if(create.grd == '1'){
		grd.lon <- rfeDataInfo$lon
		grd.lat <- rfeDataInfo$lat
	}else if(create.grd == '2'){
		grd.lon <- demData$lon
		grd.lat <- demData$lat
	}else if(create.grd == '3'){
		X0 <- GeneralParameters$New.Grid.Def$minlon
		X1 <- GeneralParameters$New.Grid.Def$maxlon
		pX <- GeneralParameters$New.Grid.Def$reslon
		Y0 <- GeneralParameters$New.Grid.Def$minlat
		Y1 <- GeneralParameters$New.Grid.Def$maxlat
		pY <- GeneralParameters$New.Grid.Def$reslat
		grd.lon <- seq(X0, X1, pX)
		grd.lat <- seq(Y0, Y1, pY)
	}
	nlon0 <- length(grd.lon)
	nlat0 <- length(grd.lat)
	xy.grid <- list(lon = grd.lon, lat = grd.lat)

	################

	interp.method <- GeneralParameters$Interpolation.pars$interp.method
	rad.lon <- as.numeric(GeneralParameters$Interpolation.pars$rad.lon)
	rad.lat <- as.numeric(GeneralParameters$Interpolation.pars$rad.lat)
	maxdist <- as.numeric(GeneralParameters$Interpolation.pars$maxdist)

	res.coarse <- if(interp.method == 'NN') sqrt((rad.lon*mean(grd.lon[-1]-grd.lon[-nlon0]))^2 + (rad.lat*mean(grd.lat[-1]-grd.lat[-nlat0]))^2)/2 else maxdist/2
	res.coarse <- if(res.coarse  >= 0.25) res.coarse else 0.25

	################
	year1 <- as.numeric(GeneralParameters$LM.Date.Range$start.year)
	year2 <- as.numeric(GeneralParameters$LM.Date.Range$end.year)
	months <- as.numeric(GeneralParameters$LM.Months)
	rfeDir <- GeneralParameters$IO.files$RFE.dir
	rfefilefrmt <- GeneralParameters$Prefix$RFE.File.Format

	start.date <- as.Date(paste(year1, '0101', sep = ''), format = '%Y%m%d')
	end.date <- as.Date(paste(year2, '1231', sep = ''), format = '%Y%m%d')

	################
	msg <- list(start = 'Read RFE data ...', end = 'Reading RFE data finished')
	errmsg <- "RFE data not found"
	ncfiles <- list(freqData = freqData, start.date = start.date, end.date = end.date,
					months = months, ncDir = rfeDir, ncFileFormat = rfefilefrmt)
	ncinfo <- list(xo = rfeDataInfo$rfeILon, yo = rfeDataInfo$rfeILat, varid = rfeDataInfo$rfeVarid)
	read.ncdf.parms <- list(ncfiles = ncfiles, ncinfo = ncinfo, msg = msg, errmsg = errmsg)

	################
	comptLMparams <- list(GeneralParameters = GeneralParameters, stnData = stnData, demData = demData, memType = memType,
						rfeData = read.ncdf.parms, xy.grid = xy.grid, res.coarse = res.coarse, origdir = origdir)
	ret <- ComputeLMCoefRain(comptLMparams)

	rm(comptLMparams, stnData, demData)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

##############################################################################################################
####Merging

execMergeRain <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	freqData <- GeneralParameters$period

	#######get data
	stnData <- getStnOpenData(GeneralParameters$IO.files$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	if(is.null(stnData)) return(NULL)

	## RFE/ADJ-RFE DATA
	rfeDataInfo <- getRFESampleData(GeneralParameters$IO.files$RFE.file)

	create.grid <- GeneralParameters$Create.Grid
	grid.from <- GeneralParameters$Grid.From
	blank.grid <- GeneralParameters$Blank.Grid

	grdblnk <- (create.grid & grid.from == '2') | blank.grid == '2'
	auxvar <- GeneralParameters$auxvar$dem | GeneralParameters$auxvar$slope | GeneralParameters$auxvar$aspect

	## DEM data
	demData <- NULL
	if(grdblnk | auxvar){
		demData <- getDemOpenDataSPDF(GeneralParameters$IO.files$DEM.file)
		if(is.null(demData)){
			InsertMessagesTxt(main.txt.out, "No elevation data found", format = TRUE)
			return(NULL)
		}
	}

	##Create grid for interpolation
	if(create.grid){
		if(grid.from == '1'){
			grd.lon <- rfeDataInfo$lon
			grd.lat <- rfeDataInfo$lat
		}else if(grid.from == '2'){
			grd.lon <- demData$lon
			grd.lat <- demData$lat
		}else if(grid.from == '3'){
			X0 <- GeneralParameters$New.Grid.Def$minlon
			X1 <- GeneralParameters$New.Grid.Def$maxlon
			pX <- GeneralParameters$New.Grid.Def$reslon
			Y0 <- GeneralParameters$New.Grid.Def$minlat
			Y1 <- GeneralParameters$New.Grid.Def$maxlat
			pY <- GeneralParameters$New.Grid.Def$reslat
			grd.lon <- seq(X0, X1, pX)
			grd.lat <- seq(Y0, Y1, pY)
		}
	}else{
		if(GeneralParameters$Mrg.Method == "Spatio-Temporal LM"){
			LMCoef.file <- file.path(GeneralParameters$IO.files$LMCoef.dir, paste('LM_Coefficient_', GeneralParameters$Mrg.Months[1], '.nc', sep = ''))
			if(!file.exists(LMCoef.file)){
				InsertMessagesTxt(main.txt.out, "LM coefficients files not found", format = TRUE)
				return(NULL)
			}
			nc <- nc_open(LMCoef.file)
			grd.lon <- nc$dim[[1]]$vals
			grd.lat <- nc$dim[[2]]$vals
			nc_close(nc)
		}else{
			grd.lon <- rfeDataInfo$lon
			grd.lat <- rfeDataInfo$lat
		}
	}
	nlon0 <- length(grd.lon)
	nlat0 <- length(grd.lat)
	xy.grid <- list(lon = grd.lon, lat = grd.lat)
	newGrid <- defSpatialPixels(xy.grid)

	## regrid DEM data
	if(!is.null(demData)){
		demData <- list(x = demData$lon, y = demData$lat, z = demData$demMat)
		if(grdblnk | auxvar){
			demGrid <- defSpatialPixels(list(lon = demData$x, lat = demData$y))
			is.regridDEM <- is.diffSpatialPixelsObj(newGrid, demGrid, tol = 1e-07)
			if(is.regridDEM) demData <- interp.surface.grid(demData, list(x = grd.lon, y = grd.lat))
		}
	}

	## blank
	if(blank.grid == "1") outMask <- NULL
	if(blank.grid == "2"){
		outMask <- demData$z
		outMask[outMask <= 0] <- NA
	}
	if(blank.grid == "3"){
		shpd <- getShpOpenData(GeneralParameters$IO.files$SHP.file)[[2]]
		shpd[['vtmp']] <- 1
		outMask <- over(newGrid, shpd)[, 'vtmp']
		dim(outMask) <- c(nlon0, nlat0)
	}

	paramsMRG <- list(GeneralParameters = GeneralParameters, stnData = stnData, rfeDataInfo = rfeDataInfo,
						demData = demData, xy.grid = xy.grid, outMask = outMask, origdir = origdir)
	ret <- MergingFunctionRain(paramsMRG)
	rm(mrgparams, stnData, outMask, rfeDataInfo, demData)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

##############################################################################################################

##############################################################################################################



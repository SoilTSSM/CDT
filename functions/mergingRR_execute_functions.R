
execBiasRain <- function(origdir){
	freqData <- GeneralParameters$period
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	create.grd <- GeneralParameters$Create.Grid

	year1 <- as.numeric(GeneralParameters$Bias.Date.Range$start.year)
	year2 <- as.numeric(GeneralParameters$Bias.Date.Range$end.year)
	months <- as.numeric(GeneralParameters$Bias.Months)
	rfeDir <- GeneralParameters$IO.files$RFE.dir
	rfefilefrmt <- GeneralParameters$Prefix$RFE.File.Format

	#######get data
	stnData <- getStnOpenData(GeneralParameters$IO.files$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	if(is.null(stnData)) return(NULL)
	
	###get elevation data
	demData <- getDemOpenDataSPDF(GeneralParameters$IO.files$DEM.file)
	if(is.null(demData)) return(NULL)

	# # ##RFE sample file
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

	rfeData <- read.NetCDF.Data(read.ncdf.parms)
	if(is.null(rfeData)) return(NULL)

	xy.rfe <- list(lon = rfeData$lon, lat = rfeData$lat)

	###############
	bias.exist <- GeneralParameters$bias.calc$bias.exist
	bias.file <- str_trim(GeneralParameters$bias.calc$bias.file)
	outfile.bias <- if(bias.file == "") file.path(origdir, 'BiasParams.RData') else bias.file

	if(!bias.exist){
		comptMBiasparms <- list(GeneralParameters = GeneralParameters, stnData = stnData, rfeData = rfeData, res.coarse = res.coarse)
		bias.pars <- ComputeMeanBiasRain(comptMBiasparms)
		save(bias.pars, file = outfile.bias)
	}else load(outfile.bias)

	interpBiasparams <- list(GeneralParameters = GeneralParameters, bias.pars = bias.pars, stnData = stnData,
							demData = demData, xy.grid = xy.grid, xy.rfe = xy.rfe, res.coarse = res.coarse, origdir = origdir)
	ret <- InterpolateMeanBiasRain(interpBiasparams)

	# outfile <- file.path(origdir, 'DataUsed2ComputeBias.RData')
	# save(stnData, demData, rfeData, file = outfile)

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

	################

	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	################

	rfeDataInfo <- getRFESampleData(GeneralParameters$IO.files$RFE.file)

	################
	## RFE DATA
	msg <- list(start = 'Read RFE data ...', end = 'Reading RFE data finished')
	errmsg <- "RFE data not found"
	ncfiles <- list(freqData = freqData, start.date = start.date, end.date = end.date,
					months = months, ncDir = rfeDir, ncFileFormat = rfefilefrmt)
	ncinfo <- list(xo = rfeDataInfo$rfeILon, yo = rfeDataInfo$rfeILat, varid = rfeDataInfo$rfeVarid)
	read.ncdf.parms <- list(ncfiles = ncfiles, ncinfo = ncinfo, msg = msg, errmsg = errmsg)

	rfeData <- read.NetCDF.Data(read.ncdf.parms)
	if(is.null(rfeData)) return(NULL)

	adjMeanBiasparms <- list(rfeData = rfeData, GeneralParameters = GeneralParameters, origdir = origdir)
	ret <- AjdMeanBiasRain(adjMeanBiasparms)
	rm(adjMeanBiasparms, rfeData, rfeDataInfo)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

##############################################################################################################
#### Compute LM coef

execLMCoefRain <- function(origdir){
	freqData <- GeneralParameters$period
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	create.grd <- GeneralParameters$Create.Grid

	year1 <- as.numeric(GeneralParameters$LM.Date.Range$start.year)
	year2 <- as.numeric(GeneralParameters$LM.Date.Range$end.year)
	months <- as.numeric(GeneralParameters$LM.Months)
	rfeDir <- GeneralParameters$IO.files$RFE.dir
	rfefilefrmt <- GeneralParameters$Prefix$RFE.File.Format

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

	start.date <- as.Date(paste(year1, '0101', sep = ''), format = '%Y%m%d')
	end.date <- as.Date(paste(year2, '1231', sep = ''), format = '%Y%m%d')

	################

	msg <- list(start = 'Read RFE data ...', end = 'Reading RFE data finished')
	errmsg <- "RFE data not found"
	ncfiles <- list(freqData = freqData, start.date = start.date, end.date = end.date,
					months = months, ncDir = rfeDir, ncFileFormat = rfefilefrmt)
	ncinfo <- list(xo = rfeDataInfo$rfeILon, yo = rfeDataInfo$rfeILat, varid = rfeDataInfo$rfeVarid)
	read.ncdf.parms <- list(ncfiles = ncfiles, ncinfo = ncinfo, msg = msg, errmsg = errmsg)

	rfeData <- read.NetCDF.Data(read.ncdf.parms)
	if(is.null(rfeData)) return(NULL)

	################
	comptLMparams <- list(GeneralParameters = GeneralParameters, stnData = stnData, demData = demData,
						rfeData = rfeData, xy.grid = xy.grid, res.coarse = res.coarse, origdir = origdir)
	ret <- ComputeLMCoefRain(comptLMparams)
	
	rm(comptLMparams, stnData, demData, rfeData)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

##############################################################################################################
####Merging

execMergeRain <- function(origdir){
	freqData <- GeneralParameters$period
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

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
		if(grdblnk | auxvar){
			demGrid <- defSpatialPixels(list(lon = demData$lon, lat = demData$lat))
			is.regridDEM <- is.diffSpatialPixelsObj(newGrid, demGrid, tol = 1e-07)
			if(is.regridDEM){
				demData <- interp.surface.grid(list(x = demData$lon, y = demData$lat, z = demData$demMat),
												list(x = grd.lon, y = grd.lat))
			}
		}else demData <- list(x = demData$lon, y = demData$lat, z = demData$demMat)
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



execCoefDownTemp <- function(origdir){
	freqData <- GeneralParameters$period
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	#######get data
	stnData <- getStnOpenData(GeneralParameters$IO.files$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	if(is.null(stnData)) return(NULL)

	###get elevation data
	demData <- getDemOpenDataSPDF(GeneralParameters$IO.files$DEM.file)
	if(is.null(demData)) return(NULL)

	#Compute regression parameters between station temperature and elevation
	paramsGlmCoef <- list(GeneralParameters = GeneralParameters, stnData = stnData,
							demData = demData, origdir = origdir)
	ret <- GlmCoefDownscaling(paramsGlmCoef)
	rm(stnData, demData)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

#######################################################################################

execDownscalingTemp <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	freqData <- GeneralParameters$period
	start.year <- GeneralParameters$Down.Date.Range$start.year
	start.mon <- GeneralParameters$Down.Date.Range$start.mon
	start.dek <- GeneralParameters$Down.Date.Range$start.dek
	end.year <- GeneralParameters$Down.Date.Range$end.year
	end.mon <- GeneralParameters$Down.Date.Range$end.mon
	end.dek <- GeneralParameters$Down.Date.Range$end.dek
	months <- GeneralParameters$Down.Months

	reanalDir <- GeneralParameters$IO.files$Reanal.dir
	reanalfilefrmt <- GeneralParameters$Format$Reanal.File.Format

	################
	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	##Reanalysis sample file
	reanalInfo <- getRFESampleData(GeneralParameters$IO.files$Reanal.file)

	################
	downCoef <- try(read.table(GeneralParameters$IO.files$Coef.file), silent = TRUE)
	if(inherits(downCoef, "try-error")){
		InsertMessagesTxt(main.txt.out, 'Error reading downscaling coefficients', format = TRUE)
		return(NULL)
	}
		
	################
	###get elevation data
	demData <- getDemOpenDataSPDF(GeneralParameters$IO.files$DEM.file)
	if(is.null(demData)) return(NULL)
	demGrid <- demData$demMat
	demGrid[demGrid < 0] <- 0

	##Create grid for interpolation
	create.grd <- GeneralParameters$Grid.From
	if(create.grd == '1'){
		grd.lon <- demData$lon
		grd.lat <- demData$lat
	}else if(create.grd == '2'){
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

	##interpolate dem in new grid
	if(create.grd == '2'){
		is.regridDEM <- is.diffSpatialPixelsObj(defSpatialPixels(xy.grid),
						defSpatialPixels(list(lon = demData$lon, lat = demData$lat)), tol = 1e-07)
		if(is.regridDEM){
			demGrid <- list(x = demData$lon, y = demData$lat, z = demGrid)
			demGrid <- interp.surface.grid(demGrid, list(x = xy.grid$lon, y = xy.grid$lat))
			demGrid <- demGrid$z
		}
	}

	################
	msg <- list(start = 'Read Reanalysis data ...', end = 'Reading Reanalysis data finished')
	errmsg <- "Reanalysis data not found"
	ncfiles <- list(freqData = freqData, start.date = start.date, end.date = end.date,
					months = months, ncDir = reanalDir, ncFileFormat = reanalfilefrmt)
	ncinfo <- list(xo = reanalInfo$rfeILon, yo = reanalInfo$rfeILat, varid = reanalInfo$rfeVarid)
	read.ncdf.parms <- list(ncfiles = ncfiles, ncinfo = ncinfo, msg = msg, errmsg = errmsg)

	reanalData <- read.NetCDF.Data(read.ncdf.parms)
	if(is.null(reanalData)) return(NULL)

	paramsDownscl <- list(GeneralParameters = GeneralParameters, demGrid = demGrid, downCoef = downCoef, 
							reanalData = reanalData, xy.grid = xy.grid, origdir = origdir)
	ret <- ReanalysisDownscaling(paramsDownscl)
	rm(paramsDownscl, demData, demGrid, reanalInfo, reanalData)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	}else return(NULL)
}

#######################################################################################

execBiasTemp <- function(origdir){
	freqData <- GeneralParameters$period
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	year1 <- as.numeric(GeneralParameters$Bias.Date.Range$start.year)
	year2 <- as.numeric(GeneralParameters$Bias.Date.Range$end.year)
	months <- as.numeric(GeneralParameters$Bias.Months)

	reanalDir <- GeneralParameters$IO.files$Down.dir
	reanalfilefrmt <- GeneralParameters$Format$Down.File.Format

	#######get data
	stnData <- getStnOpenData(GeneralParameters$IO.files$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	if(is.null(stnData)) return(NULL)
	
	###get elevation data
	demData <- getDemOpenDataSPDF(GeneralParameters$IO.files$DEM.file)
	if(is.null(demData)) return(NULL)

	########
	start.date <- as.Date(paste(year1, '0101', sep = ''), format = '%Y%m%d')
	end.date <- as.Date(paste(year2, '1231', sep = ''), format = '%Y%m%d')

	msg <- list(start = 'Read Downscaled data ...', end = 'Reading Downscaled data finished')
	errmsg <- "Downscaled data not found"
	ncfiles <- list(freqData = freqData, start.date = start.date, end.date = end.date,
					months = months, ncDir = reanalDir, ncFileFormat = reanalfilefrmt)
	ncinfo <- list(xo = 1, yo = 2, varid = "temp")
	read.ncdf.parms <- list(ncfiles = ncfiles, ncinfo = ncinfo, msg = msg, errmsg = errmsg)

	downData <- read.NetCDF.Data(read.ncdf.parms)
	if(is.null(downData)) return(NULL)
	nlon0 <- length(downData$lon)
	nlat0 <- length(downData$lat)
	xy.grid <- list(lon = downData$lon, lat = downData$lat)

	interp.method <- GeneralParameters$Interpolation.pars$interp.method
	rad.lon <- as.numeric(GeneralParameters$Interpolation.pars$rad.lon)
	rad.lat <- as.numeric(GeneralParameters$Interpolation.pars$rad.lat)
	maxdist <- as.numeric(GeneralParameters$Interpolation.pars$maxdist)

	res.coarse <- if(interp.method == 'NN') sqrt((rad.lon*mean(downData$lon[-1]-downData$lon[-nlon0]))^2 + (rad.lat*mean(downData$lat[-1]-downData$lat[-nlat0]))^2)/2 else maxdist/2
	res.coarse <- if(res.coarse  >= 0.25) res.coarse else 0.25

	comptMBiasparms <- list(GeneralParameters = GeneralParameters, stnData = stnData, downData = downData, res.coarse = res.coarse)
	bias.pars <- ComputeMeanBiasTemp(comptMBiasparms)

	interpBiasparams <- list(GeneralParameters = GeneralParameters, bias.pars = bias.pars, stnData = stnData,
							demData = demData, xy.grid = xy.grid, res.coarse = res.coarse, origdir = origdir)
	ret <- InterpolateMeanBiasTemp(interpBiasparams)

	rm(comptMBiasparms, stnData, demData, downData, bias.pars, interpBiasparams)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}


#######################################################################################

execAjdBiasDownTemp <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	freqData <- GeneralParameters$period
	start.year <- GeneralParameters$Adjust.Date.Range$start.year
	start.mon <- GeneralParameters$Adjust.Date.Range$start.mon
	start.dek <- GeneralParameters$Adjust.Date.Range$start.dek
	end.year <- GeneralParameters$Adjust.Date.Range$end.year
	end.mon <- GeneralParameters$Adjust.Date.Range$end.mon
	end.dek <- GeneralParameters$Adjust.Date.Range$end.dek
	months <- sort(as.numeric(GeneralParameters$Adjust.Months))

	downDir <- GeneralParameters$IO.files$Down.dir
	downfilefrmt <- GeneralParameters$Format$Down.File.Format

	################

	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	################
	## Downscaled DATA
	msg <- list(start = 'Read Downscaled data ...', end = 'Reading Downscaled data finished')
	errmsg <- "Downscaled data not found"
	ncfiles <- list(freqData = freqData, start.date = start.date, end.date = end.date,
					months = months, ncDir = downDir, ncFileFormat = downfilefrmt)
	ncinfo <- list(xo = 1, yo = 2, varid = "temp")
	read.ncdf.parms <- list(ncfiles = ncfiles, ncinfo = ncinfo, msg = msg, errmsg = errmsg)

	downData <- read.NetCDF.Data(read.ncdf.parms)
	if(is.null(downData)) return(NULL)

	adjMeanBiasparms <- list(downData = downData, GeneralParameters = GeneralParameters, origdir = origdir)
	ret <- AjdMeanBiasTemp(adjMeanBiasparms)
	rm(adjMeanBiasparms, downData)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

#######################################################################################

#### Compute LM coef

execLMCoefTemp <- function(origdir){
	freqData <- GeneralParameters$period
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	year1 <- as.numeric(GeneralParameters$LM.Date.Range$start.year)
	year2 <- as.numeric(GeneralParameters$LM.Date.Range$end.year)
	months <- as.numeric(GeneralParameters$LM.Months)
	adjDir <- GeneralParameters$IO.files$Down.dir
	adjfilefrmt <- GeneralParameters$Format$Adj.File.Format

	#######get data
	stnData <- getStnOpenData(GeneralParameters$IO.files$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	if(is.null(stnData)) return(NULL)

	###get elevation data
	demData <- getDemOpenDataSPDF(GeneralParameters$IO.files$DEM.file)
	if(is.null(demData)) return(NULL)

	########
	start.date <- as.Date(paste(year1, '0101', sep = ''), format = '%Y%m%d')
	end.date <- as.Date(paste(year2, '1231', sep = ''), format = '%Y%m%d')

	msg <- list(start = 'Read bias corrected reanalysis data ...', end = 'Reading bias corrected reanalysis data finished')
	errmsg <- "Adjusted Reanalysis data not found"
	ncfiles <- list(freqData = freqData, start.date = start.date, end.date = end.date,
					months = months, ncDir = adjDir, ncFileFormat = adjfilefrmt)
	ncinfo <- list(xo = 1, yo = 2, varid = "temp")
	read.ncdf.parms <- list(ncfiles = ncfiles, ncinfo = ncinfo, msg = msg, errmsg = errmsg)

	adjData <- read.NetCDF.Data(read.ncdf.parms)
	if(is.null(adjData)) return(NULL)
	nlon0 <- length(adjData$lon)
	nlat0 <- length(adjData$lat)
	xy.grid <- list(lon = adjData$lon, lat = adjData$lat)

	interp.method <- GeneralParameters$Interpolation.pars$interp.method
	rad.lon <- as.numeric(GeneralParameters$Interpolation.pars$rad.lon)
	rad.lat <- as.numeric(GeneralParameters$Interpolation.pars$rad.lat)
	maxdist <- as.numeric(GeneralParameters$Interpolation.pars$maxdist)

	res.coarse <- if(interp.method == 'NN') sqrt((rad.lon*mean(adjData$lon[-1]-adjData$lon[-nlon0]))^2 + (rad.lat*mean(adjData$lat[-1]-adjData$lat[-nlat0]))^2)/2 else maxdist/2
	res.coarse <- if(res.coarse  >= 0.25) res.coarse else 0.25

	################
	comptLMparams <- list(GeneralParameters = GeneralParameters, stnData = stnData, demData = demData,
						adjData = adjData, xy.grid = xy.grid, res.coarse = res.coarse, origdir = origdir)
	ret <- ComputeLMCoefTemp(comptLMparams)
	
	rm(comptLMparams, stnData, demData, adjData)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

#######################################################################################

execMergeTemp <- function(origdir){
	freqData <- GeneralParameters$period
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

	#######get data
	stnData <- getStnOpenData(GeneralParameters$IO.files$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	if(is.null(stnData)) return(NULL)

	blank.grid <- GeneralParameters$Blank.Grid
	grdblnk <- blank.grid == '2'
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

	adjDir <- GeneralParameters$IO.files$ADJ.dir
	adjfilefrmt <- GeneralParameters$FileFormat$Adj.File.Format
	months <- GeneralParameters$Mrg.Months
	daty <- GeneralParameters$Mrg.Date.Range
	start.date <- as.Date(paste(daty$start.year, daty$start.mon, daty$start.dek, sep = '-'))
	end.date <- as.Date(paste(daty$end.year, daty$end.mon, daty$end.dek, sep = '-'))
	error.msg <- "Reanalysis bias corrected data not found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, adjDir, adjfilefrmt, error.msg)
	if(is.null(ncInfo)) return(NULL)

	##Create grid for interpolation
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
		nc <- nc_open(ncInfo$nc.files[ncInfo$exist][1])
		grd.lon <- nc$dim[[1]]$vals
		grd.lat <- nc$dim[[2]]$vals
		nc_close(nc)
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

	paramsMRG <- list(GeneralParameters = GeneralParameters, stnData = stnData, ncInfo = ncInfo,
						demData = demData, xy.grid = xy.grid, outMask = outMask, origdir = origdir)
	ret <- MergingFunctionTemp(paramsMRG)
	rm(mrgparams, stnData, outMask, ncInfo, demData)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}




execBiasTemp <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	InsertMessagesTxt(main.txt.out, 'Computing bias coefficients ...')

	freqData <- GeneralParameters$period
	#######get data
	stnData <- getStnOpenData(GeneralParameters$IO.files$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	if(is.null(stnData)) return(NULL)

	###get elevation data
	demData <- getDemOpenDataSPDF(GeneralParameters$IO.files$DEM.file)
	if(is.null(demData)) return(NULL)

	########
	year1 <- as.numeric(GeneralParameters$Bias.Date.Range$start.year)
	year2 <- as.numeric(GeneralParameters$Bias.Date.Range$end.year)
	months <- as.numeric(GeneralParameters$Bias.Months)

	reanalDir <- GeneralParameters$IO.files$Down.dir
	reanalfilefrmt <- GeneralParameters$Format$Down.File.Format

	start.date <- as.Date(paste(year1, '0101', sep = ''), format = '%Y%m%d')
	end.date <- as.Date(paste(year2, '1231', sep = ''), format = '%Y%m%d')

	########
	msg <- list(start = 'Read Downscaled data ...', end = 'Reading Downscaled data finished')
	errmsg <- "Downscaled data not found"
	ncfiles <- list(freqData = freqData, start.date = start.date, end.date = end.date,
					months = months, ncDir = reanalDir, ncFileFormat = reanalfilefrmt)
	ncinfo <- list(xo = 1, yo = 2, varid = "temp")
	read.ncdf.parms <- list(ncfiles = ncfiles, ncinfo = ncinfo, msg = msg, errmsg = errmsg)

	########
	ncInfo <- do.call("ncFilesInfo", c(ncfiles, errmsg))
	if(is.null(ncInfo)) return(NULL)
	nc <- nc_open(ncInfo$nc.files[ncInfo$exist][1])
	rlon <- nc$dim[[1]]$vals
	rlat <- nc$dim[[2]]$vals
	nc_close(nc)
	nlon0 <- length(rlon)
	nlat0 <- length(rlat)
	xy.grid <- list(lon = rlon, lat = rlat)

	########
	interp.method <- GeneralParameters$Interpolation.pars$interp.method
	rad.lon <- as.numeric(GeneralParameters$Interpolation.pars$rad.lon)
	rad.lat <- as.numeric(GeneralParameters$Interpolation.pars$rad.lat)
	maxdist <- as.numeric(GeneralParameters$Interpolation.pars$maxdist)

	res.coarse <- if(interp.method == 'NN') sqrt((rad.lon*mean(rlon[-1]-rlon[-nlon0]))^2 + (rad.lat*mean(rlat[-1]-rlat[-nlat0]))^2)/2 else maxdist/2
	res.coarse <- if(res.coarse  >= 0.25) res.coarse else 0.25

	########
	comptMBiasparms <- list(GeneralParameters = GeneralParameters, stnData = stnData, xy.grid = xy.grid,
							downData = read.ncdf.parms, res.coarse = res.coarse, origdir = origdir)
	bias.pars <- ComputeMeanBiasTemp(comptMBiasparms)

	interpBiasparams <- list(GeneralParameters = GeneralParameters, bias.pars = bias.pars, stnData = stnData,
							demData = demData, xy.grid = xy.grid, res.coarse = res.coarse, origdir = origdir)
	ret <- InterpolateMeanBiasTemp(interpBiasparams)

	rm(comptMBiasparms, stnData, demData, bias.pars, interpBiasparams)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}


#######################################################################################

execAjdBiasDownTemp <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	InsertMessagesTxt(main.txt.out, 'Adjustment of downscaled data ...')

	################
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

	adjMeanBiasparms <- list(downData = read.ncdf.parms, GeneralParameters = GeneralParameters, origdir = origdir)
	ret <- AjdMeanBiasTemp(adjMeanBiasparms)
	rm(adjMeanBiasparms)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

#######################################################################################

#### Compute LM coef

execLMCoefTemp <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	InsertMessagesTxt(main.txt.out, 'Computing LM Coefficients ...')

	#######get data
	freqData <- GeneralParameters$period
	stnData <- getStnOpenData(GeneralParameters$IO.files$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	if(is.null(stnData)) return(NULL)

	###get elevation data
	demData <- getDemOpenDataSPDF(GeneralParameters$IO.files$DEM.file)
	if(is.null(demData)) return(NULL)

	########
	year1 <- as.numeric(GeneralParameters$LM.Date.Range$start.year)
	year2 <- as.numeric(GeneralParameters$LM.Date.Range$end.year)
	months <- as.numeric(GeneralParameters$LM.Months)
	adjDir <- GeneralParameters$IO.files$Down.dir
	adjfilefrmt <- GeneralParameters$Format$Adj.File.Format

	start.date <- as.Date(paste(year1, '0101', sep = ''), format = '%Y%m%d')
	end.date <- as.Date(paste(year2, '1231', sep = ''), format = '%Y%m%d')

	################
	msg <- list(start = 'Read bias corrected reanalysis data ...', end = 'Reading bias corrected reanalysis data finished')
	errmsg <- "Adjusted Reanalysis data not found"
	ncfiles <- list(freqData = freqData, start.date = start.date, end.date = end.date,
					months = months, ncDir = adjDir, ncFileFormat = adjfilefrmt)
	ncinfo <- list(xo = 1, yo = 2, varid = "temp")
	read.ncdf.parms <- list(ncfiles = ncfiles, ncinfo = ncinfo, msg = msg, errmsg = errmsg)

	################
	comptLMparams <- list(GeneralParameters = GeneralParameters, stnData = stnData, demData = demData,
							adjData = read.ncdf.parms, origdir = origdir)
	ret <- ComputeLMCoefTemp(comptLMparams)
	
	rm(comptLMparams, stnData, demData)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

#######################################################################################

execMergeTemp <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	InsertMessagesTxt(main.txt.out, 'Merging data ...')

	#######get data
	freqData <- GeneralParameters$period
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
		demData <- list(x = demData$lon, y = demData$lat, z = demData$demMat)
		if(grdblnk | auxvar){
			demGrid <- defSpatialPixels(list(lon = demData$x, lat = demData$y))
			is.regridDEM <- is.diffSpatialPixelsObj(newGrid, demGrid, tol = 1e-07)
			if(is.regridDEM) demData <- interp.surface.grid(demData, list(x = grd.lon, y = grd.lat))
		}
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



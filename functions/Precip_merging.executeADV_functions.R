
execBiasRain <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	InsertMessagesTxt(main.txt.out, 'Computing Gauge-RFE bias ...')

	freqData <- GeneralParameters$period
	GeneralParameters$biasFilenames <- GeneralParameters$output$format

	#######get data
	stnData <- getStnOpenData(GeneralParameters$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	if(is.null(stnData)) return(NULL)

	##################
	## RFE sample file
	rfeDataInfo <- getRFESampleData(GeneralParameters$RFE$sample)
	if(is.null(rfeDataInfo)){
		InsertMessagesTxt(main.txt.out, "No RFE data sample found", format = TRUE)
		return(NULL)
	}

	##################
	## DEM data
	demData <- NULL
	if(GeneralParameters$BIAS$interp.method == "NN" | GeneralParameters$Grid.Creation$grid == "2" |
	   GeneralParameters$auxvar$dem | GeneralParameters$auxvar$slope | GeneralParameters$auxvar$aspect)
	{
		demData <- getDemOpenDataSPDF(GeneralParameters$DEM.file)
		if(is.null(demData)){
			InsertMessagesTxt(main.txt.out, "No elevation data found", format = TRUE)
			return(NULL)
		}
	}

	##################
	##Create grid for interpolation
	if(GeneralParameters$Grid.Creation$grid == '1'){
		grd.lon <- rfeDataInfo$lon
		grd.lat <- rfeDataInfo$lat
	}else if(GeneralParameters$Grid.Creation$grid == '2'){
		grd.lon <- demData$lon
		grd.lat <- demData$lat
	}else if(GeneralParameters$Grid.Creation$grid == '3'){
		X0 <- GeneralParameters$Grid.Creation$minlon
		X1 <- GeneralParameters$Grid.Creation$maxlon
		pX <- GeneralParameters$Grid.Creation$reslon
		Y0 <- GeneralParameters$Grid.Creation$minlat
		Y1 <- GeneralParameters$Grid.Creation$maxlat
		pY <- GeneralParameters$Grid.Creation$reslat
		grd.lon <- seq(X0, X1, pX)
		grd.lat <- seq(Y0, Y1, pY)
	}
	nlon0 <- length(grd.lon)
	nlat0 <- length(grd.lat)
	xy.grid <- list(lon = grd.lon, lat = grd.lat)

	##################
	## regrid DEM data
	if(!is.null(demData)){
		is.regridDEM <- is.diffSpatialPixelsObj(defSpatialPixels(xy.grid), defSpatialPixels(demData[c('lon', 'lat')]), tol = 1e-07)
		if(is.regridDEM)
			demData <- cdt.interp.surface.grid(c(demData[c('lon', 'lat')], list(z = demData$demMat)), xy.grid)
		else demData <- list(x = demData$lon, y = demData$lat, z = demData$demMat)
		demData$z[demData$z < 0] <- 0
	}

	##################
	## Get RFE data info
	errmsg <- "RFE data not found"
	RFE.DIR <- GeneralParameters$RFE$dir
	RFE.Format <- GeneralParameters$RFE$format
	start.date1 <- as.Date(paste0(GeneralParameters$BIAS$start.year, '0101'), format = '%Y%m%d')
	end.date1 <- as.Date(paste0(GeneralParameters$BIAS$end.year, '1231'), format = '%Y%m%d')
	months <- GeneralParameters$BIAS$Months

	ncInfoBias <- ncFilesInfo(freqData, start.date1, end.date1, months, RFE.DIR, RFE.Format, errmsg)
	if(is.null(ncInfoBias)) return(NULL)
	ncInfoBias$ncinfo <- list(xo = rfeDataInfo$rfeILon, yo = rfeDataInfo$rfeILat, varid = rfeDataInfo$rfeVarid)
	ncInfoBias$xy.rfe <- list(lon = rfeDataInfo$lon, lat = rfeDataInfo$lat)

	biasParms <- list(GeneralParameters = GeneralParameters, stnData = stnData,
						ncInfo = ncInfoBias, bias.DIR = origdir, months = months,
						interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
	bias.pars <- Precip_ComputeBias(biasParms)
	rm(biasParms)
	if(is.null(bias.pars)) return(NULL)

	biasParms <- list(bias.pars = bias.pars, GeneralParameters = GeneralParameters, months = months,
					stnData = stnData[c('lon', 'lat')], demData = demData, ncInfo = ncInfoBias, bias.DIR = origdir,
					interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
	ret <- Precip_InterpolateBias(biasParms)

	rm(biasParms, ncInfoBias, stnData, demData, rfeDataInfo, bias.pars)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	}else return(NULL)
}

##############################################################################################################

execAdjBiasRain <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	InsertMessagesTxt(main.txt.out, 'Adjusting Gauge-RFE bias ...')

	##################
	## RFE sample file
	rfeDataInfo <- getRFESampleData(GeneralParameters$RFE$sample)
	if(is.null(rfeDataInfo)){
		InsertMessagesTxt(main.txt.out, "No RFE data sample found", format = TRUE)
		return(NULL)
	}

	##################
	freqData <- GeneralParameters$period
	start.year <- GeneralParameters$Adjust.Date$start.year
	start.mon <- GeneralParameters$Adjust.Date$start.mon
	start.dek <- GeneralParameters$Adjust.Date$start.dek
	end.year <- GeneralParameters$Adjust.Date$end.year
	end.mon <- GeneralParameters$Adjust.Date$end.mon
	end.dek <- GeneralParameters$Adjust.Date$end.dek
	months <- GeneralParameters$Adjust.Date$Months
	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	RFE.DIR <- GeneralParameters$RFE$dir
	RFE.Format <- GeneralParameters$RFE$format

	##################
	errmsg <- "RFE data not found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, RFE.DIR, RFE.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- list(xo = rfeDataInfo$rfeILon, yo = rfeDataInfo$rfeILat, varid = rfeDataInfo$rfeVarid)
	ncInfo$xy.rfe <- list(lon = rfeDataInfo$lon, lat = rfeDataInfo$lat)

	##################
	## READ BIAS FILSES
	BIAS.DIR <- GeneralParameters$BIAS$dir.Bias
	GeneralParameters$biasFilenames <- GeneralParameters$BIAS$format

	biasParms <- list(bias.DIR = BIAS.DIR, GeneralParameters = GeneralParameters, dates = ncInfo$dates, months = months)
	BIAS <- Precip_ReadBiasFiles(biasParms)
	rm(biasParms)
	if(is.null(BIAS)) return(NULL)

	##################
	extractADJ <- FALSE
	biasParms <- list(adj.DIR = origdir, GeneralParameters = GeneralParameters,
					BIAS = BIAS, ncInfo = ncInfo, stnData = list(lon = NULL, lat = NULL))

	ret <- Precip_ApplyBiasCorrection(biasParms, extractADJ)

	rm(biasParms, BIAS, rfeDataInfo, ncInfo); gc()

	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	}else return(NULL)
}

##############################################################################################################
#### Compute LM coef

execLMCoefRain <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	InsertMessagesTxt(main.txt.out, 'Computing LM Coefficients ...')

	freqData <- GeneralParameters$period

	##################
	## Get data
	stnData <- getStnOpenData(GeneralParameters$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	if(is.null(stnData)) return(NULL)

	##################
	## RFE sample file
	rfeDataInfo <- getRFESampleData(GeneralParameters$RFE$sample)
	if(is.null(rfeDataInfo)){
		InsertMessagesTxt(main.txt.out, "No RFE or Adjusted RFE data sample found", format = TRUE)
		return(NULL)
	}

	##################
	## DEM data
	demData <- NULL
	if(GeneralParameters$LMCOEF$interp.method == "NN" | GeneralParameters$Grid.Creation$grid == "2" |
	   GeneralParameters$auxvar$dem | GeneralParameters$auxvar$slope | GeneralParameters$auxvar$aspect)
	{
		demData <- getDemOpenDataSPDF(GeneralParameters$DEM.file)
		if(is.null(demData)){
			InsertMessagesTxt(main.txt.out, "No elevation data found", format = TRUE)
			return(NULL)
		}
	}

	##################
	##Create grid for interpolation
	if(GeneralParameters$Grid.Creation$grid == '1'){
		grd.lon <- rfeDataInfo$lon
		grd.lat <- rfeDataInfo$lat
	}else if(GeneralParameters$Grid.Creation$grid == '2'){
		grd.lon <- demData$lon
		grd.lat <- demData$lat
	}else if(GeneralParameters$Grid.Creation$grid == '3'){
		X0 <- GeneralParameters$Grid.Creation$minlon
		X1 <- GeneralParameters$Grid.Creation$maxlon
		pX <- GeneralParameters$Grid.Creation$reslon
		Y0 <- GeneralParameters$Grid.Creation$minlat
		Y1 <- GeneralParameters$Grid.Creation$maxlat
		pY <- GeneralParameters$Grid.Creation$reslat
		grd.lon <- seq(X0, X1, pX)
		grd.lat <- seq(Y0, Y1, pY)
	}
	nlon0 <- length(grd.lon)
	nlat0 <- length(grd.lat)
	xy.grid <- list(lon = grd.lon, lat = grd.lat)

	##################
	## regrid DEM data
	if(!is.null(demData)){
		is.regridDEM <- is.diffSpatialPixelsObj(defSpatialPixels(xy.grid), defSpatialPixels(demData[c('lon', 'lat')]), tol = 1e-07)
		if(is.regridDEM)
			demData <- cdt.interp.surface.grid(c(demData[c('lon', 'lat')], list(z = demData$demMat)), xy.grid)
		else demData <- list(x = demData$lon, y = demData$lat, z = demData$demMat)
		demData$z[demData$z < 0] <- 0
	}

	##################

	start.date <- as.Date(paste0(GeneralParameters$LMCOEF$start.year, '0101'), format = '%Y%m%d')
	end.date <- as.Date(paste0(GeneralParameters$LMCOEF$end.year, '1231'), format = '%Y%m%d')
	months <- GeneralParameters$LMCOEF$Months

	RFE.DIR <- GeneralParameters$RFE$dir
	RFE.Format <- GeneralParameters$RFE$format
	GeneralParameters$lmCoefFilenames <- GeneralParameters$output$format

	##################
	errmsg <- "RFE or bias corrected RFE data not found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, RFE.DIR, RFE.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- list(xo = rfeDataInfo$rfeILon, yo = rfeDataInfo$rfeILat, varid = rfeDataInfo$rfeVarid)
	ncInfo$xy.rfe <- list(lon = rfeDataInfo$lon, lat = rfeDataInfo$lat)

	lmCoefParms <- list(GeneralParameters = GeneralParameters, stnData = stnData,
						ncInfo = ncInfo, LMCoef.DIR = origdir, months = months,
						interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
	model.coef <- Precip_ComputeLMCoef(lmCoefParms)
	rm(lmCoefParms)
	if(is.null(model.coef)) return(NULL)

	#########
	lmCoefParms <- list(model.coef = model.coef, GeneralParameters = GeneralParameters, months = months,
					stnData = stnData[c('lon', 'lat')], demData = demData, ncInfo = ncInfo, LMCoef.DIR = origdir,
					interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
	ret <- Precip_InterpolateLMCoef(lmCoefParms)

	rm(lmCoefParms, model.coef, ncInfo, stnData, demData, rfeDataInfo)
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
	InsertMessagesTxt(main.txt.out, 'Merging data ...')
	InsertMessagesTxt(main.txt.out, 'Data preparation ...')

	freqData <- GeneralParameters$period

	##################
	## Get data
	stnData <- getStnOpenData(GeneralParameters$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	if(is.null(stnData)) return(NULL)

	##################
	## RFE sample file
	rfeDataInfo <- getRFESampleData(GeneralParameters$RFE$sample)
	if(is.null(rfeDataInfo)){
		InsertMessagesTxt(main.txt.out, "No RFE or Adjusted RFE data sample found", format = TRUE)
		return(NULL)
	}

	##################
	## DEM data
	demData <- NULL
	if(GeneralParameters$blank$blank == "2" | GeneralParameters$Grid.Creation$grid == "2" |
	   GeneralParameters$auxvar$dem | GeneralParameters$auxvar$slope | GeneralParameters$auxvar$aspect)
	{
		demData <- getDemOpenDataSPDF(GeneralParameters$DEM.file)
		if(is.null(demData)){
			InsertMessagesTxt(main.txt.out, "No elevation data found", format = TRUE)
			return(NULL)
		}
	}

	##################
	##Create grid for interpolation
	if(GeneralParameters$Grid.Creation$grid == '1'){
		grd.lon <- rfeDataInfo$lon
		grd.lat <- rfeDataInfo$lat
	}else if(GeneralParameters$Grid.Creation$grid == '2'){
		grd.lon <- demData$lon
		grd.lat <- demData$lat
	}else if(GeneralParameters$Grid.Creation$grid == '3'){
		X0 <- GeneralParameters$Grid.Creation$minlon
		X1 <- GeneralParameters$Grid.Creation$maxlon
		pX <- GeneralParameters$Grid.Creation$reslon
		Y0 <- GeneralParameters$Grid.Creation$minlat
		Y1 <- GeneralParameters$Grid.Creation$maxlat
		pY <- GeneralParameters$Grid.Creation$reslat
		grd.lon <- seq(X0, X1, pX)
		grd.lat <- seq(Y0, Y1, pY)
	}
	nlon0 <- length(grd.lon)
	nlat0 <- length(grd.lat)
	xy.grid <- list(lon = grd.lon, lat = grd.lat)

	##################
	## regrid DEM data
	if(!is.null(demData)){
		is.regridDEM <- is.diffSpatialPixelsObj(defSpatialPixels(xy.grid), defSpatialPixels(demData[c('lon', 'lat')]), tol = 1e-07)
		if(is.regridDEM)
			demData <- cdt.interp.surface.grid(c(demData[c('lon', 'lat')], list(z = demData$demMat)), xy.grid)
		else demData <- list(x = demData$lon, y = demData$lat, z = demData$demMat)
		demData$z[demData$z < 0] <- 0
	}

	##################
	## Get RFE data info
	start.year <- GeneralParameters$Merging.Date$start.year
	start.mon <- GeneralParameters$Merging.Date$start.mon
	start.dek <- GeneralParameters$Merging.Date$start.dek
	end.year <- GeneralParameters$Merging.Date$end.year
	end.mon <- GeneralParameters$Merging.Date$end.mon
	end.dek <- GeneralParameters$Merging.Date$end.dek
	months <- GeneralParameters$Merging.Date$Months
	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	RFE.DIR <- GeneralParameters$RFE$dir
	RFE.Format <- GeneralParameters$RFE$format

	##################
	errmsg <- "RFE or bias corrected RFE data not found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, RFE.DIR, RFE.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- list(xo = rfeDataInfo$rfeILon, yo = rfeDataInfo$rfeILat, varid = rfeDataInfo$rfeVarid)
	ncInfo$xy.rfe <- list(lon = rfeDataInfo$lon, lat = rfeDataInfo$lat)

	##################
	## blanking
	outMask <- switch(GeneralParameters$blank$blank,
					"2" = {
							mask <- demData$z
							mask[mask <= 0] <- NA
							mask[!is.na(mask)] <- 1
							mask
						},
					"3" = {
							shpd <- getShpOpenData(GeneralParameters$blank$SHP.file)[[2]]
							shpd[['vtmp']] <- 1
							mask <- over(defSpatialPixels(xy.grid), shpd)[, 'vtmp']
							dim(mask) <- c(nlon0, nlat0)
							mask
						}, NULL)

	InsertMessagesTxt(main.txt.out, 'Data preparation finished')
	##################
	mrgParms <- list(GeneralParameters = GeneralParameters, months = months, ncInfo = ncInfo,
					stnData = stnData, demData = demData, merge.DIR = origdir,
					interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0), outMask = outMask)

	ret <- Precip_MergingFunctions(mrgParms)

	rm(ncInfo, mrgParms)
	gc()
	if(!is.null(ret)){
		if(ret != 0) return(ret) 
	}else return(NULL)

	##################
	# Scaling up data
	if(GeneralParameters$scale.data$scale){
		scaled.DIR <- paste0(origdir, "_ScaledData")
		dir.create(scaled.DIR, showWarnings = FALSE, recursive = TRUE)
		ncParms <- list(mrg.data = list(tstep = GeneralParameters$period, dir = origdir, sample = "",
					format = GeneralParameters$output$format), Scaling.Date = GeneralParameters$Merging.Date,
					scale.data = GeneralParameters$scale.data, outdir = scaled.DIR)
		ret <- merged_ScalingUpData(ncParms, TRUE)
		if(!is.null(ret)){
			if(ret != 0) return(ret)
		}else return(NULL)
	}
	return(0)
}

##############################################################################################################


execBiasTemp <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	InsertMessagesTxt(main.txt.out, 'Computing bias coefficients ...')

	freqData <- GeneralParameters$period
	GeneralParameters$biasFilenames <- GeneralParameters$output$format

	#######get data
	stnData <- getStnOpenData(GeneralParameters$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	if(is.null(stnData)) return(NULL)

	##################
	## TEMP sample file
	tmpDataInfo <- getRFESampleData(GeneralParameters$TEMP$sample)
	if(is.null(tmpDataInfo)){
		InsertMessagesTxt(main.txt.out, "No downscaled data sample found", format = TRUE)
		return(NULL)
	}

	##################
	## Grid for interpolation
	xy.grid <- tmpDataInfo[c('lon', 'lat')]
	nlon0 <- length(tmpDataInfo$lon)
	nlat0 <- length(tmpDataInfo$lat)

	##################
	## DEM data
	demData <- NULL
	if(GeneralParameters$BIAS$interp.method == "NN" |
	   GeneralParameters$auxvar$dem | GeneralParameters$auxvar$slope |
	   GeneralParameters$auxvar$aspect)
	{
		demData <- getDemOpenDataSPDF(GeneralParameters$DEM.file)
		if(is.null(demData)){
			InsertMessagesTxt(main.txt.out, "No elevation data found", format = TRUE)
			return(NULL)
		}
	}

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
	start.date <- as.Date(paste0(GeneralParameters$BIAS$start.year, '0101'), format = '%Y%m%d')
	end.date <- as.Date(paste0(GeneralParameters$BIAS$end.year, '1231'), format = '%Y%m%d')
	months <- GeneralParameters$BIAS$Months

	TMP.DIR <- GeneralParameters$TEMP$dir
	TMP.Format <- GeneralParameters$TEMP$format

	errmsg <- "Downscaled data not found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, TMP.DIR, TMP.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- list(xo = tmpDataInfo$rfeILon, yo = tmpDataInfo$rfeILat, varid = tmpDataInfo$rfeVarid)

	biasParms <- list(GeneralParameters = GeneralParameters, stnData = stnData,
						ncInfo = ncInfo, bias.DIR = origdir, months = months,
						interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))

	bias.pars <- Temp_ComputeBias(biasParms)
	rm(biasParms)
	if(is.null(bias.pars)) return(NULL)

	#########
	biasParms <- list(bias.pars = bias.pars, GeneralParameters = GeneralParameters, months = months,
					stnData = stnData[c('lon', 'lat')], demData = demData, ncInfo = ncInfo, bias.DIR = origdir,
					interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
	ret <- Temp_InterpolateBias(biasParms)

	rm(biasParms, stnData, demData, bias.pars, ncInfo, tmpDataInfo)
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

	##################
	## TEMP sample file
	tmpDataInfo <- getRFESampleData(GeneralParameters$TEMP$sample)
	if(is.null(tmpDataInfo)){
		InsertMessagesTxt(main.txt.out, "No downscaled data sample found", format = TRUE)
		return(NULL)
	}

	################
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

	TMP.DIR <- GeneralParameters$TEMP$dir
	TMP.Format <- GeneralParameters$TEMP$format

	##################
	errmsg <- "Downscaled data not found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, TMP.DIR, TMP.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- list(xo = tmpDataInfo$rfeILon, yo = tmpDataInfo$rfeILat, varid = tmpDataInfo$rfeVarid)

	##################
	## READ BIAS FILSES
	BIAS.DIR <- GeneralParameters$BIAS$dir.Bias
	GeneralParameters$biasFilenames <- GeneralParameters$BIAS$format

	biasParms <- list(bias.DIR = BIAS.DIR, GeneralParameters = GeneralParameters, dates = ncInfo$dates, months = months)
	BIAS <- Temp_ReadBiasFiles(biasParms)
	rm(biasParms)
	if(is.null(BIAS)) return(NULL)

	##################
	extractADJ <- FALSE
	biasParms <- list(adj.DIR = origdir, GeneralParameters = GeneralParameters,
					BIAS = BIAS, ncInfo = ncInfo, stnData = list(lon = NULL, lat = NULL))

	ret <- Temp_ApplyBiasCorrection(biasParms, extractADJ)
	rm(biasParms, BIAS, tmpDataInfo, ncInfo)
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

	freqData <- GeneralParameters$period

	#######get data
	stnData <- getStnOpenData(GeneralParameters$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	if(is.null(stnData)) return(NULL)

	##################
	## TEMP sample file
	tmpDataInfo <- getRFESampleData(GeneralParameters$TEMP$sample)
	if(is.null(tmpDataInfo)){
		InsertMessagesTxt(main.txt.out, "No downscaled or Adjusted reanalysis data sample found", format = TRUE)
		return(NULL)
	}

	##################
	## Grid for interpolation
	xy.grid <- tmpDataInfo[c('lon', 'lat')]
	nlon0 <- length(tmpDataInfo$lon)
	nlat0 <- length(tmpDataInfo$lat)

	##################
	## DEM data
	demData <- NULL
	if(GeneralParameters$LMCOEF$interp.method == "NN" |
	   GeneralParameters$auxvar$dem | GeneralParameters$auxvar$slope |
	   GeneralParameters$auxvar$aspect)
	{
		demData <- getDemOpenDataSPDF(GeneralParameters$DEM.file)
		if(is.null(demData)){
			InsertMessagesTxt(main.txt.out, "No elevation data found", format = TRUE)
			return(NULL)
		}
	}

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

	TMP.DIR <- GeneralParameters$TEMP$dir
	TMP.Format <- GeneralParameters$TEMP$format
	GeneralParameters$lmCoefFilenames <- GeneralParameters$output$format

	##################
	errmsg <- "No downscaled or Adjusted reanalysis data sample found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, TMP.DIR, TMP.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- list(xo = tmpDataInfo$rfeILon, yo = tmpDataInfo$rfeILat, varid = tmpDataInfo$rfeVarid)

	lmCoefParms <- list(GeneralParameters = GeneralParameters, stnData = stnData,
						ncInfo = ncInfo, LMCoef.DIR = origdir, months = months,
						interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
	model.coef <- Temp_ComputeLMCoef(lmCoefParms)
	rm(lmCoefParms)
	if(is.null(model.coef)) return(NULL)

	##################
	lmCoefParms <- list(model.coef = model.coef, GeneralParameters = GeneralParameters, months = months,
					stnData = stnData[c('lon', 'lat')], demData = demData, ncInfo = ncInfo, LMCoef.DIR = origdir,
					interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))

	ret <- Temp_InterpolateLMCoef(lmCoefParms)

	rm(model.coef, stnData, demData, ncInfo, tmpDataInfo)
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
	InsertMessagesTxt(main.txt.out, 'Data preparation ...')

	freqData <- GeneralParameters$period

	#######get data
	stnData <- getStnOpenData(GeneralParameters$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	if(is.null(stnData)) return(NULL)

	##################
	## TEMP sample file
	tmpDataInfo <- getRFESampleData(GeneralParameters$TEMP$sample)
	if(is.null(tmpDataInfo)){
		InsertMessagesTxt(main.txt.out, "No downscaled or Adjusted reanalysis data sample found", format = TRUE)
		return(NULL)
	}

	##################
	## Grid for interpolation
	xy.grid <- tmpDataInfo[c('lon', 'lat')]
	nlon0 <- length(tmpDataInfo$lon)
	nlat0 <- length(tmpDataInfo$lat)

	##################
	## DEM data
	demData <- NULL
	if(GeneralParameters$blank$blank == "2" |
	   GeneralParameters$auxvar$dem | GeneralParameters$auxvar$slope |
	   GeneralParameters$auxvar$aspect)
	{
		demData <- getDemOpenDataSPDF(GeneralParameters$DEM.file)
		if(is.null(demData)){
			InsertMessagesTxt(main.txt.out, "No elevation data found", format = TRUE)
			return(NULL)
		}
	}

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

	TMP.DIR <- GeneralParameters$TEMP$dir
	TMP.Format <- GeneralParameters$TEMP$format

	##################
	errmsg <- "No downscaled or Adjusted reanalysis data sample found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, TMP.DIR, TMP.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- list(xo = tmpDataInfo$rfeILon, yo = tmpDataInfo$rfeILat, varid = tmpDataInfo$rfeVarid)

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

	mrgParms <- list(GeneralParameters = GeneralParameters, months = months, ncInfo = ncInfo,
					stnData = stnData, demData = demData, merge.DIR = origdir,
					interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0), outMask = outMask)

	ret <- Temp_MergingFunctions(mrgParms)

	rm(mrgParms, stnData, outMask, ncInfo, demData, tmpDataInfo)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}



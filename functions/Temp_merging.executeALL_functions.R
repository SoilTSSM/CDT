Temp_Merging_ALL <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	InsertMessagesTxt(main.txt.out, 'Start merging ...')

	freqData <- GeneralParameters$period
	GeneralParameters$auxvar <- list(dem = FALSE, slope = FALSE, aspect = FALSE, lon = FALSE, lat = FALSE)

	GeneralParameters$biasFilenames <- GeneralParameters$BIAS$format
	GeneralParameters$lmCoefFilenames <- GeneralParameters$LMCOEF$format

	##################
	## Get data
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
	errmsg <- "Downscaled data not found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, TMP.DIR, TMP.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- list(xo = tmpDataInfo$rfeILon, yo = tmpDataInfo$rfeILat, varid = tmpDataInfo$rfeVarid)

	##################
	## DEM data
	demData <- NULL
	if(((!GeneralParameters$BIAS$deja.calc & GeneralParameters$BIAS$interp.method == "NN") |
	(GeneralParameters$LMCOEF$interp.method == "NN" & !GeneralParameters$LMCOEF$deja.calc &
	GeneralParameters$Merging$mrg.method == "Spatio-Temporal LM") | GeneralParameters$blank$blank == "2"))
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
	## Compute BIAS
	if(!GeneralParameters$BIAS$deja.calc)
	{
		start.date1 <- as.Date(paste0(GeneralParameters$BIAS$start.year, '0101'), format = '%Y%m%d')
		end.date1 <- as.Date(paste0(GeneralParameters$BIAS$end.year, '1231'), format = '%Y%m%d')

		ncInfoBias <- ncFilesInfo(freqData, start.date1, end.date1, months, TMP.DIR, TMP.Format, errmsg)
		if(is.null(ncInfoBias)) return(NULL)
		ncInfoBias$ncinfo <- list(xo = tmpDataInfo$rfeILon, yo = tmpDataInfo$rfeILat, varid = tmpDataInfo$rfeVarid)

		# calculate bias factors
		bias.DIR <- file.path(origdir, "BIAS_Data")
		dir.create(bias.DIR, showWarnings = FALSE, recursive = TRUE)
		biasParms <- list(GeneralParameters = GeneralParameters, stnData = stnData,
							ncInfo = ncInfoBias, bias.DIR = bias.DIR, months = months,
							interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
		bias.pars <- Temp_ComputeBias(biasParms)
		rm(biasParms)
		if(is.null(bias.pars)) return(NULL)
		#########
		biasParms <- list(bias.pars = bias.pars, GeneralParameters = GeneralParameters, months = months,
						stnData = stnData[c('lon', 'lat')], demData = demData, ncInfo = ncInfoBias, bias.DIR = bias.DIR,
						interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
		ret <- Temp_InterpolateBias(biasParms)
		rm(biasParms, bias.pars, ncInfoBias)
		gc()
		if(!is.null(ret)){
			if(ret != 0) return(ret) 
		}else return(NULL)
	}else bias.DIR <- GeneralParameters$BIAS$dir.Bias

	##################
	## APPLY BIAS correction
	adj.DIR <- file.path(origdir, "ADJUSTED_Data")
	dir.create(adj.DIR, showWarnings = FALSE, recursive = TRUE)

	if(!GeneralParameters$LMCOEF$deja.calc & GeneralParameters$Merging$mrg.method == "Spatio-Temporal LM")
	{
		start.date1 <- as.Date(paste(GeneralParameters$LMCOEF$start.year, '0101', sep = ''), format = '%Y%m%d')
		end.date1 <- as.Date(paste(GeneralParameters$LMCOEF$end.year, '1231', sep = ''), format = '%Y%m%d')
		start.date1 <- min(start.date1, start.date)
		end.date1 <- max(end.date1, end.date)

		ncInfoAdj <- ncFilesInfo(freqData, start.date1, end.date1, months, TMP.DIR, TMP.Format, errmsg)
		if(is.null(ncInfoAdj)) return(NULL)
		ncInfoAdj$ncinfo <- list(xo = tmpDataInfo$rfeILon, yo = tmpDataInfo$rfeILat, varid = tmpDataInfo$rfeVarid)
		AdjDate <- ncInfoAdj$dates
		extractADJ <- TRUE
	}else{
		ncInfoAdj <- ncInfo
		AdjDate <- ncInfo$dates
		extractADJ <- FALSE
	}

	##################
	## READ BIAS FILSES
	biasParms <- list(bias.DIR = bias.DIR, GeneralParameters = GeneralParameters, dates = AdjDate, months = months)
	BIAS <- Temp_ReadBiasFiles(biasParms)
	rm(biasParms)
	if(is.null(BIAS)) return(NULL)

	##################
	if(freqData%in%c('daily', 'pentad', 'dekadal')) adj.Format <- "temp_adj_%s%s%s.nc"
	if(freqData == 'monthly') adj.Format <- "temp_adj_%s%s.nc"
	GeneralParameters0 <- GeneralParameters
	GeneralParameters0$output$format <- adj.Format

	##################
	biasParms <- list(adj.DIR = adj.DIR, GeneralParameters = GeneralParameters0,
					BIAS = BIAS, ncInfo = ncInfoAdj, stnData = stnData[c('lon', 'lat')])

	data.adj.stn <- Temp_ApplyBiasCorrection(biasParms, extractADJ)

	rm(biasParms, BIAS, ncInfoAdj)
	if(!extractADJ){
		if(!is.null(data.adj.stn)){
			if(data.adj.stn != 0) return(data.adj.stn) 
		}else return(NULL)
	}

	##################
	if(!GeneralParameters$LMCOEF$deja.calc & GeneralParameters$Merging$mrg.method == "Spatio-Temporal LM")
	{
		ncInfoLMCoef <- NULL
		ncInfoLMCoef$data <- data.adj.stn

		LMCoef.DIR <- file.path(origdir, "LMCOEF_Data")
		dir.create(LMCoef.DIR, showWarnings = FALSE, recursive = TRUE)
		lmCoefParms <- list(GeneralParameters = GeneralParameters, stnData = stnData,
							ncInfo = ncInfoLMCoef, LMCoef.DIR = LMCoef.DIR, months = months,
							interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
		model.coef <- Temp_ComputeLMCoef(lmCoefParms)
		rm(lmCoefParms)
		if(is.null(model.coef)) return(NULL)
		#########
		lmCoefParms <- list(model.coef = model.coef, GeneralParameters = GeneralParameters, months = months,
						stnData = stnData[c('lon', 'lat')], demData = demData, ncInfo = ncInfo, LMCoef.DIR = LMCoef.DIR,
						interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
		ret <- Temp_InterpolateLMCoef(lmCoefParms)

		GeneralParameters$LMCOEF$dir.LMCoef <- LMCoef.DIR

		rm(lmCoefParms, model.coef)
		gc()
		if(!is.null(ret)){
			if(ret != 0) return(ret) 
		}else return(NULL)
	}

	##################

	errmsg <- "Adjusted data not found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, adj.DIR, adj.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- list(xo = 1, yo = 2, varid = "temp")

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

	merge.DIR <- file.path(origdir, "Merged_Data")
	dir.create(merge.DIR, showWarnings = FALSE, recursive = TRUE)

	mrgParms <- list(GeneralParameters = GeneralParameters, months = months, ncInfo = ncInfo,
					stnData = stnData, demData = demData, merge.DIR = merge.DIR,
					interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0), outMask = outMask)

	ret <- Temp_MergingFunctions(mrgParms)

	rm(ncInfo, mrgParms)
	gc()
	if(!is.null(ret)){
		if(ret != 0) return(ret) 
	}else return(NULL)


	return(0)
}


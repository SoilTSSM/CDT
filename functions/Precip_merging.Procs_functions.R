Precip_Merging_ALL <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	InsertMessagesTxt(main.txt.out, 'Start merging ...')

	freqData <- GeneralParameters$period
	GeneralParameters$auxvar <- list(dem = FALSE, slope = FALSE, aspect = FALSE, lon = FALSE, lat = FALSE)
	GeneralParameters$sp.trend.aux <- FALSE

	##################
	## Get data
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
	## Grid for interpolation
	GeneralParameters$Grid.Creation$grid <- "1"
	xy.grid <- list(lon = rfeDataInfo$lon, lat = rfeDataInfo$lat)
	nlon0 <- length(rfeDataInfo$lon)
	nlat0 <- length(rfeDataInfo$lat)

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
	errmsg <- "RFE data not found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, RFE.DIR, RFE.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- list(xo = rfeDataInfo$rfeILon, yo = rfeDataInfo$rfeILat, varid = rfeDataInfo$rfeVarid)
	ncInfo$xy.rfe <- list(lon = rfeDataInfo$lon, lat = rfeDataInfo$lat)

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
		demData <- list(x = demData$lon, y = demData$lat, z = demData$demMat)
		if(is.regridDEM) demData <- interp.surface.grid(demData, list(x = xy.grid$lon, y = xy.grid$lat))
		demData$z[demData$z < 0] <- 0
	}

	##################
	## Compute BIAS
	if(!GeneralParameters$BIAS$deja.calc)
	{
		start.date1 <- as.Date(paste(GeneralParameters$BIAS$start.year, '0101', sep = ''), format = '%Y%m%d')
		end.date1 <- as.Date(paste(GeneralParameters$BIAS$end.year, '1231', sep = ''), format = '%Y%m%d')

		ncInfoBias <- ncFilesInfo(freqData, start.date1, end.date1, months, RFE.DIR, RFE.Format, errmsg)
		if(is.null(ncInfoBias)) return(NULL)
		ncInfoBias$ncinfo <- list(xo = rfeDataInfo$rfeILon, yo = rfeDataInfo$rfeILat, varid = rfeDataInfo$rfeVarid)
		ncInfoBias$xy.rfe <- list(lon = rfeDataInfo$lon, lat = rfeDataInfo$lat)

		# calculate bias factors
		bias.DIR <- file.path(origdir, "BIAS_Data")
		dir.create(bias.DIR, showWarnings = FALSE, recursive = TRUE)
		biasParms <- list(GeneralParameters = GeneralParameters, stnData = stnData,
							ncInfo = ncInfoBias, bias.DIR = bias.DIR, months = months,
							interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
		bias.pars <- Precip_ComputeBias(biasParms)
		rm(biasParms)
		if(is.null(bias.pars)) return(NULL)
		#########
		biasParms <- list(bias.pars = bias.pars, GeneralParameters = GeneralParameters, months = months,
						stnData = stnData[c('lon', 'lat')], demData = demData, ncInfo = ncInfoBias, bias.DIR = bias.DIR,
						interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
		ret <- Precip_InterpolateBias(biasParms)
		rm(biasParms, bias.pars, ncInfoBias)
		gc()
		if(!is.null(ret)){
			if(ret != 0) return(ret) 
		}else return(NULL)
	}else bias.DIR <- GeneralParameters$BIAS$dir.Bias

	##################
	## READ BIAS FILSES
	biasParms <- list(bias.DIR = bias.DIR, GeneralParameters = GeneralParameters, dates = ncInfo$dates, months = months)
	BIAS <- Precip_ReadBiasFiles(biasParms)
	rm(biasParms)
	if(is.null(BIAS)) return(NULL)

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

		ncInfoAdj <- ncFilesInfo(freqData, start.date1, end.date1, months, RFE.DIR, RFE.Format, errmsg)
		if(is.null(ncInfoAdj)) return(NULL)
		ncInfoAdj$ncinfo <- list(xo = rfeDataInfo$rfeILon, yo = rfeDataInfo$rfeILat, varid = rfeDataInfo$rfeVarid)
		ncInfoAdj$xy.rfe <- list(lon = rfeDataInfo$lon, lat = rfeDataInfo$lat)
		extractADJ <- TRUE
	}else{
		ncInfoAdj <- ncInfo
		extractADJ <- FALSE
	}

	biasParms <- list(adj.DIR = adj.DIR, GeneralParameters = GeneralParameters,
					BIAS = BIAS, ncInfo = ncInfoAdj, stnData = stnData[c('lon', 'lat')])

	data.adj.stn <- Precip_ApplyBiasCorrection(biasParms, extractADJ)

	rm(biasParms, BIAS, ncInfoAdj)
	if(!extractADJ){
		if(!is.null(data.adj.stn)){
			if(data.adj.stn != 0) return(data.adj.stn) 
		}else return(NULL)
	}

	##################
	if(!GeneralParameters$LMCOEF$deja.calc & GeneralParameters$Merging$mrg.method == "Spatio-Temporal LM")
	{
		# start.date1 <- as.Date(paste(GeneralParameters$LMCOEF$start.year, '0101', sep = ''), format = '%Y%m%d')
		# end.date1 <- as.Date(paste(GeneralParameters$LMCOEF$end.year, '1231', sep = ''), format = '%Y%m%d')

		# # ncInfoLMCoef <- ncFilesInfo(GeneralParameters$period, start.date1, end.date1, months,
		# # 					GeneralParameters$RFE$dir, GeneralParameters$RFE$format, errmsg)
		# ncInfoLMCoef <- ncFilesInfo(GeneralParameters$period, start.date1, end.date1, months,
		# 					adj.DIR, "rr_adj_%s%s%s.nc", errmsg)
		# if(is.null(ncInfoLMCoef)) return(NULL)
		# # ncInfoLMCoef$ncinfo <- list(xo = rfeDataInfo$rfeILon, yo = rfeDataInfo$rfeILat, varid = rfeDataInfo$rfeVarid)
		# ncInfoLMCoef$ncinfo <- list(xo = 1, yo = 2, varid = "precip")
		# ncInfoLMCoef$xy.rfe <- list(lon = rfeDataInfo$lon, lat = rfeDataInfo$lat)

		ncInfoLMCoef <- NULL
		ncInfoLMCoef$data <- data.adj.stn

		LMCoef.DIR <- file.path(origdir, "LMCOEF_Data")
		dir.create(LMCoef.DIR, showWarnings = FALSE, recursive = TRUE)
		lmCoefParms <- list(GeneralParameters = GeneralParameters, stnData = stnData,
							ncInfo = ncInfoLMCoef, LMCoef.DIR = LMCoef.DIR, months = months,
							interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
		model.coef <- Precip_ComputeLMCoef(lmCoefParms)
		rm(lmCoefParms)
		if(is.null(model.coef)) return(NULL)
		#########
		lmCoefParms <- list(model.coef = model.coef, GeneralParameters = GeneralParameters, months = months,
						stnData = stnData[c('lon', 'lat')], demData = demData, ncInfo = ncInfo, LMCoef.DIR = LMCoef.DIR,
						interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
		ret <- Precip_InterpolateLMCoef(lmCoefParms)
		rm(lmCoefParms, model.coef)
		gc()
		if(!is.null(ret)){
			if(ret != 0) return(ret) 
		}else return(NULL)

	}else LMCoef.DIR <- GeneralParameters$LMCOEF$dir.LMCoef

	##################
	if(freqData%in%c('daily', 'pentad', 'dekadal')) adj.Format <- "rr_adj_%s%s%s.nc"
	if(freqData == 'monthly') adj.Format <- "rr_adj_%s%s.nc"

	errmsg <- "Adjusted data not found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, adj.DIR, adj.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- list(xo = 1, yo = 2, varid = "precip")
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

	merge.DIR <- file.path(origdir, "Merged_Data")
	dir.create(merge.DIR, showWarnings = FALSE, recursive = TRUE)

	mrgParms <- list(GeneralParameters = GeneralParameters, months = months, ncInfo = ncInfo,
					stnData = stnData, demData = demData, LMCoef.DIR = LMCoef.DIR, merge.DIR = merge.DIR,
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
		scaled.DIR <- file.path(origdir, "Merged_ScaledData")
		dir.create(scaled.DIR, showWarnings = FALSE, recursive = TRUE)
		ncParms <- list(mrg.data = list(tstep = GeneralParameters$period, dir = merge.DIR, sample = "",
					format = GeneralParameters$output$format), Scaling.Date = GeneralParameters$Merging.Date,
					scale.data = GeneralParameters$scale.data, outdir = scaled.DIR)
		ret <- merged_ScalingUpData(ncParms, TRUE)
		if(!is.null(ret)){
			if(ret != 0) return(ret) 
		}else return(NULL)
	}

	return(0)
}






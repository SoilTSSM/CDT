Temp_execCoefDown <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	InsertMessagesTxt(main.txt.out, 'Computing regression parameters ...')

	#######get data
	stnData <- getStnOpenData(GeneralParameters$IO.files$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, GeneralParameters$period)
	if(is.null(stnData)) return(NULL)

	###get elevation data
	demData <- getDemOpenDataSPDF(GeneralParameters$IO.files$DEM.file)
	if(is.null(demData)) return(NULL)

	#Compute regression parameters between station temperature and elevation
	paramsGlmCoef <- list(GeneralParameters = GeneralParameters, stnData = stnData,
							demData = demData, origdir = origdir)
	ret <- Temp_CoefDownscaling(paramsGlmCoef)
	rm(stnData, demData)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

#######################################################################################

Temp_execDownscaling <- function(origdir){
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	InsertMessagesTxt(main.txt.out, 'Downscaling ...')

	################
	CoefFile <- str_trim(GeneralParameters$DownCoef.file)
	if(!file.exists(CoefFile)){
		InsertMessagesTxt(main.txt.out, paste(CoefFile, "not found"), format = TRUE)
		return(NULL)
	}
	downCoef <- try(read.table(CoefFile), silent = TRUE)
	if(inherits(downCoef, "try-error")){
		InsertMessagesTxt(main.txt.out, 'Error reading downscaling coefficients', format = TRUE)
		return(NULL)
	}

	################
	##Reanalysis sample file
	reanalInfo <- getRFESampleData(GeneralParameters$REANAL$sample)
	if(is.null(reanalInfo)) return(NULL)

	################
	###get elevation data
	demData <- getDemOpenDataSPDF(GeneralParameters$DEM.file)
	if(is.null(demData)) return(NULL)

	##Create grid for interpolation
	create.grd <- GeneralParameters$Grid.Creation$grid
	if(create.grd == '1'){
		grd.lon <- demData$lon
		grd.lat <- demData$lat
	}else if(create.grd == '2'){
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

	## DEM data  at new grid
	demGrid <- demData$demMat
	if(create.grd == '2'){
		is.regridDEM <- is.diffSpatialPixelsObj(defSpatialPixels(xy.grid),
						defSpatialPixels(list(lon = demData$lon, lat = demData$lat)), tol = 1e-07)
		if(is.regridDEM){
			demGrid <- list(x = demData$lon, y = demData$lat, z = demData$demMat)
			demGrid <- interp.surface.grid(demGrid, list(x = xy.grid$lon, y = xy.grid$lat))
			demGrid <- demGrid$z
		}
	}
	demGrid[demGrid < 0] <- 0

	################
	start.year <- GeneralParameters$Down.Date.Range$start.year
	start.mon <- GeneralParameters$Down.Date.Range$start.mon
	start.dek <- GeneralParameters$Down.Date.Range$start.dek
	end.year <- GeneralParameters$Down.Date.Range$end.year
	end.mon <- GeneralParameters$Down.Date.Range$end.mon
	end.dek <- GeneralParameters$Down.Date.Range$end.dek
	months <- GeneralParameters$Down.Date.Range$Months[[1]]

	reanalDir <- GeneralParameters$REANAL$dir
	reanalfilefrmt <- GeneralParameters$REANAL$format

	################
	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	errmsg <- "Reanalysis data not found"
	ncInfo <- ncFilesInfo(GeneralParameters$period, start.date, end.date, months, reanalDir, reanalfilefrmt, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- list(xo = reanalInfo$rfeILon, yo = reanalInfo$rfeILat, varid = reanalInfo$rfeVarid)

	################
	paramsDownscl <- list(GeneralParameters = GeneralParameters, demGrid = demGrid, downCoef = downCoef, 
							reanalData = ncInfo, xy.grid = xy.grid, origdir = origdir)
	ret <- Temp_ReanalysisDownscaling(paramsDownscl)
	rm(paramsDownscl, demData, demGrid, reanalData)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	}else return(NULL)
}

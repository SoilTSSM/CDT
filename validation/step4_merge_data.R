
	## data time step
	freqData <- 'dekadal'

	## date range
	start.year <- 1983
	start.mon <- 1
	start.dek <- 1
	end.year <- 2010
	end.mon <- 12
	end.dek <- 3

	## Output directory
	outDIR <- '/Users/rijaf/Desktop/vETH_ENACTS/Merged'

	## Training data file (CDT data)
	stnFile <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/Data/RR_DEK_83to14_Train.txt'

	## Validation data file (CDT data)
	stnValidFile <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/Data/RR_DEK_83to14_Valid.txt'

	## Station missing values code
	miss.val <- -99

	## elevation data file
	demFile <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/dem/DEM_1_Arc-Minute.nc'

	## Bias corrected data directory
	adjDIR <- '/Users/rijaf/Desktop/vETH_ENACTS/ADJ'
	## corrected data filename format
	adjFFormat <- 'rr_adj_%s%s%s.txt'

	## Spatio-Temporal LM Coef directory
	SPLM.coefDIR <- '/Users/rijaf/Desktop/vETH_ENACTS/LMCoef'

	## RFE directory
	rfeDir <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/TAMSAT_dek'
	## RFE filename format
	rfeFFormat <- 'rfe_%s%s%s.nc'

	#####################################################################################
	maxdist <- 0.6

	months <- 1:12

	GeneralParameters$Interpolation.pars$nmin <- 3
	GeneralParameters$Interpolation.pars$nmax <- 10
	GeneralParameters$Mrg.set$min.stn <- 10
	GeneralParameters$Mrg.set$min.non.zero <- 7

	#########################################################

	GeneralParameters$period <- freqData
	GeneralParameters$LM.Months <- months
	GeneralParameters$Interpolation.pars$maxdist <- maxdist
	GeneralParameters$Interpolation.pars$vgm.model <- list(c("Sph", "Exp", "Gau"))
	GeneralParameters$Mrg.set$use.RnoR <- TRUE
	GeneralParameters$Mrg.set$smooth.RnoR <- TRUE
	GeneralParameters$Mrg.set$wet.day <- 1

	#####################################################################################
	if(doparallel){
		klust <- makeCluster(nb_cores)
		registerDoParallel(klust)
		`%parLoop%` <- `%dopar%`
		closeklust <- TRUE
	}else{
		`%parLoop%` <- `%do%`
		closeklust <- FALSE
	}
	packages <- c('sp', 'gstat', 'automap', 'rgdal')
	toExports <- c('GeneralParameters')

	#####################################################################################

	#### COMBINAISON
	DAS <- expand.grid(dem = c(FALSE, TRUE), slope = c(FALSE, TRUE), aspect = c(FALSE, TRUE))
	aux.var <- apply(DAS, 1, function(j){
		x <- c('D', 'S', 'A')[j]
		if(length(x) > 0) paste(x, collapse = '')
		else 'noD'
	})
	auxdf <- data.frame(n = 1:8, l = aux.var, DAS, stringsAsFactors = FALSE)

	### Bias/adj comb
	BScomb <- expand.grid(aux = aux.var[c(1, 2, 7, 8)], interp = c('NN', 'IDW', 'OK'), Bias = c('QM', 'MBVar', 'MBMon'))
	BScomb <- paste(as.character(BScomb$Bias), as.character(BScomb$interp), as.character(BScomb$aux), sep = '_')

	### Regression kriging no aux var pour spatial trend
	RKcomb <- expand.grid(aux = aux.var[c(1, 2, 7, 8)], interp = c('IDW', 'OK'), adjdir = BScomb)
	## reduire
	xred1 <- as.character(RKcomb$aux) == sapply(strsplit(as.character(RKcomb$adjdir), "_"), '[[', 3)
	xred2 <- as.character(RKcomb$interp) == sapply(strsplit(as.character(RKcomb$adjdir), "_"), '[[', 2)
	xred <- xred1 & xred2
	RKcomb <- RKcomb[xred, ]
	RKadjDir <- file.path(adjDIR, as.character(RKcomb$adjdir))
	RKcomb <- cbind(RKadjDir, "", paste(as.character(RKcomb$adjdir), "RK_RK", paste('RK.noD.spT',
		as.character(RKcomb$interp), as.character(RKcomb$aux), sep = '_'), sep = '-'),
		'RK.noD.spT', as.character(RKcomb$interp), as.character(RKcomb$aux))

	### Regression kriging avec aux var pour spatial trend
	RK1comb <- expand.grid(aux = aux.var[c(2, 7, 8)], interp = c('IDW', 'OK'), adjdir = BScomb)
	## reduire
	xred1 <- as.character(RK1comb$aux) == sapply(strsplit(as.character(RK1comb$adjdir), "_"), '[[', 3)
	xred2 <- as.character(RK1comb$interp) == sapply(strsplit(as.character(RK1comb$adjdir), "_"), '[[', 2)
	xred <- xred1 & xred2
	RK1comb <- RK1comb[xred, ]
	RK1adjDir <- file.path(adjDIR, as.character(RK1comb$adjdir))
	RK1comb <- cbind(RK1adjDir, "", paste(as.character(RK1comb$adjdir), paste("RK", as.character(RK1comb$aux), sep = '_'),
		paste('RK.wD.spT', as.character(RK1comb$interp), as.character(RK1comb$aux), sep = '_'), sep = '-'),
		'RK.wD.spT', as.character(RK1comb$interp), as.character(RK1comb$aux))

	### Spatio-Temp LM coef comb
	SPLMcomb <- expand.grid(aux = aux.var[c(1, 2, 7, 8)], interp = c('NN', 'IDW', 'OK'), adjdir = BScomb)
	## reduire
	xred1 <- as.character(SPLMcomb$aux) == sapply(strsplit(as.character(SPLMcomb$adjdir), "_"), '[[', 3)
	xred2 <- as.character(SPLMcomb$interp) == sapply(strsplit(as.character(SPLMcomb$adjdir), "_"), '[[', 2)
	xred <- xred1 & xred2
	SPLMcomb <- SPLMcomb[xred, ]
	SPLMadjDir <- file.path(adjDIR, as.character(SPLMcomb$adjdir))
	SPLMCoef <- paste(SPLMcomb$adjdir, paste(SPLMcomb$interp, SPLMcomb$aux, sep = '_'), sep = '-')
	SPLMCoefDir <- file.path(SPLM.coefDIR, SPLMCoef)
	SPLMtmp <- paste(SPLMadjDir, SPLMCoefDir, SPLMCoef, 'SP.Temp.LM', sep = ';')

	#####
	SPLMcomb <- expand.grid(aux = aux.var[c(1, 2, 7, 8)], interp = c('IDW', 'OK'), tmp = SPLMtmp)
	SPLMtmp <- strsplit(as.character(SPLMcomb$tmp), ';')
	SPLMtmp <- do.call(rbind, SPLMtmp)
	## reduire
	xred1 <- as.character(SPLMcomb$aux) == sapply(strsplit(SPLMtmp[, 3], "_"), '[[', 4)
	xred2 <- as.character(SPLMcomb$interp) == sapply(strsplit(SPLMtmp[, 3], "_"), '[[', 2)
	xred <- xred1 & xred2
	SPLMcomb <- SPLMcomb[xred, ]
	SPLMtmp <- SPLMtmp[xred, ]

	SPLMcomb <- cbind(SPLMtmp[, 1:2], paste(SPLMtmp[, 3],
		paste(SPLMtmp[, 4], SPLMcomb$interp, SPLMcomb$aux, sep = '_'), sep = '-'),
		SPLMtmp[, 4], as.character(SPLMcomb$interp), as.character(SPLMcomb$aux))

	combMering <- rbind(RKcomb, RK1comb, SPLMcomb)

	#####################################################################################
	## STN data
	stnData <- read.table(stnFile, stringsAsFactors = FALSE, na.strings = str_trim(miss.val))
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)

	## validation coordinates
	coordsValid <- read.table(stnValidFile, stringsAsFactors = FALSE, na.strings = str_trim(miss.val))
	coordsValid <- getCDTdataAndDisplayMsg(coordsValid, freqData)
	coordsValid <- coordsValid[c('lon', 'lat')]

	## DEM data
	nc <- nc_open(demFile)
	demData <- list(lon = nc$dim[[1]]$vals, lat = nc$dim[[2]]$vals, demMat = ncvar_get(nc, nc$var[[1]]$name))
	nc_close(nc)

	##### RFE data
	if(freqData == 'montly') start.dek <- end.dek <- 1
	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '-'))
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '-'))
	errmsg.adj <- "Adjusted data not found"

	###########
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, rfeDir, rfeFFormat, "RFE data not found")
	if(is.null(ncInfo)) return(NULL)
	## RFE info file
	nc <- nc_open(ncInfo$nc.files[ncInfo$exist][1])
	grd.lon <- nc$dim[[1]]$vals
	grd.lat <- nc$dim[[2]]$vals
	varid <- nc$var[[1]]$name
	nc_close(nc)

	#################
	xy.grid <- list(lon = grd.lon, lat = grd.lat)
	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- length(grd.lon)
	nlat0 <- length(grd.lat)

	# #### validation interpolation coordinates
	ijInt <- grid2pointINDEX(list(lon = coordsValid$lon, lat = coordsValid$lat), xy.grid)

	#####################################################################################

	demData$demMat[demData$demMat < 0] <- 0
	demSp <- defSpatialPixels(list(lon = demData$lon, lat = demData$lat))
	is.regridDEM <- is.diffSpatialPixelsObj(grdSp, demSp, tol = 1e-07)

	demGrid <- list(x = demData$lon, y = demData$lat, z = demData$demMat)
	if(is.regridDEM){
		demGrid <- interp.surface.grid(demGrid, list(x = xy.grid$lon, y = xy.grid$lat))
	}

	demres <- grdSp@grid@cellsize
	slpasp <- slope.aspect(demGrid$z, demres[1], demres[2], filter = "sobel")
	demGrid$slp <- slpasp$slope
	demGrid$asp <- slpasp$aspect

	#############
	ijStn <- grid2pointINDEX(list(lon = stnData$lon, lat = stnData$lat), xy.grid)
	ObjStn <- list(x = stnData$lon, y = stnData$lat, z = demGrid$z[ijStn], slp = demGrid$slp[ijStn], asp = demGrid$asp[ijStn])

	res.coarse <- maxdist/2
	res.coarse <- if(res.coarse  >= 0.25) res.coarse else 0.25
	maxdist <- if(maxdist < res.coarse) res.coarse else maxdist

	## create grid to interp
	interp.grid <- createGrid(ObjStn, demGrid, as.dim.elv = FALSE, res.coarse = res.coarse)
	cells <- SpatialPixels(points = interp.grid$newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)))@grid
	bGrd <- createBlock(cells@cellsize, 2, 5)

	## RFE data
	rfeData <- lapply(seq_along(ncInfo$nc.files), function(jj){
		if(ncInfo$exist[jj]){
			nc <- nc_open(ncInfo$nc.files[jj])
			xrfe <- ncvar_get(nc, varid = varid)
			nc_close(nc)
			xrfe[interp.grid$idxy$ix, interp.grid$idxy$iy]
		}else return(NULL)
	})
	if(is.null(rfeData)) return(NULL)
	rfeData[sapply(rfeData, is.null)] <- list(matrix(NA, ncol = length(interp.grid$idxy$ix),
														nrow = length(interp.grid$idxy$iy)))
	interp.grid$newgrid <- interp.grid$newgrid[c(ijStn, ijInt), ]

	#####################################################################################

	InsertMessagesTxt(main.txt.out, 'Merge data ...')

	retfor <- foreach(ll = seq(nrow(combMering)), .packages = packages, .export = toExports) %parLoop% {
		cbmrg <- combMering[ll, ]
		GeneralParameters$Interpolation.pars$interp.method <- if(cbmrg[5] == 'OK') 'Kriging' else cbmrg[5]

		if(cbmrg[4] == "RK.noD.spT"){
			GeneralParameters$Mrg.Method <- "Regression Kriging"
			GeneralParameters$sp.trend.aux <- FALSE
		}
		if(cbmrg[4] == 'RK.wD.spT'){
			GeneralParameters$Mrg.Method <- "Regression Kriging"
			GeneralParameters$sp.trend.aux <- TRUE
		}
		if(cbmrg[4] == 'SP.Temp.LM'){
			GeneralParameters$Mrg.Method <- "Spatio-Temporal LM"
			GeneralParameters$IO.files$LMCoef.dir <- cbmrg[2]
		}

		auxv <- as.logical(auxdf[auxdf$l == cbmrg[6], c("dem", "slope", "aspect")])
		GeneralParameters$auxvar$dem <- auxv[1]
		GeneralParameters$auxvar$slope <- auxv[2]
		GeneralParameters$auxvar$aspect <- auxv[3]

		adjInfo <- ncFilesInfo(freqData, start.date, end.date, months, cbmrg[1], adjFFormat, errmsg.adj)
		if(is.null(adjInfo)) return(NULL)

		origdir <- file.path(outDIR, cbmrg[3])
		dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

		paramsMRG <- list( GeneralParameters = GeneralParameters, stnData = stnData, ijInt = ijInt, adjInfo = adjInfo,
							rfeData = rfeData, interp.grid = interp.grid, bGrd = bGrd, origdir = origdir)

		ret <- MergingFunctionRain.validation(paramsMRG)
	}
	if(closeklust) stopCluster(klust)

	InsertMessagesTxt(main.txt.out, 'Merging finished')
	GeneralParameters <- NULL
	rm(rfeData, interp.grid, stnData, paramsMRG)

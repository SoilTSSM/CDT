
	## data time step
	freqData <- 'dekadal'

	## date range
	start.year <- 1983
	start.mon <- 1
	start.dek <- 1
	end.year <- 2010
	end.mon <- 12
	end.dek <- 3

	## Training data file (CDT data)
	stnFile <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/Data/RR_DEK_83to14_Train.txt'

	## Validation data file (CDT data)
	stnValidFile <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/Data/RR_DEK_83to14_Valid.txt'

	## Station missing values code
	miss.val <- -99

	## Bias corrected data directory
	adjDIR <- '/Users/rijaf/Desktop/vETH_ENACTS/ADJ'
	## corrected data filename format
	adjFFormat <- 'rr_adj_%s%s%s.txt'

	## RFE directory
	rfeDir <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/TAMSAT_dek'
	## RFE filename format
	rfeFFormat <- 'rfe_%s%s%s.nc'

	## Spatio-Temporal LM Coef directory
	SPLM.coefDIR <- '/Users/rijaf/Desktop/vETH_ENACTS/LMCoef'

	## elevation data file
	demFile <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/dem/DEM_1_Arc-Minute.nc'

	## Merged data Output directory
	mrgDIR <- '/Users/rijaf/Desktop/vETH_ENACTS/Merged'

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

	#### COMBINAISON

	MRGcomb <- merging.combination()

	##same aux.var, "noD", "D", "SA", "DSA"
	ix1 <- (MRGcomb$bias.auxvar == MRGcomb$mrg.auxvar) & MRGcomb$bias.auxvar%in%c("noD", "D", "SA", "DSA", "LoLa", "DLoLa", "DSALoLa")
	ix2 <- MRGcomb$mrg.method == "Reg.Kriging"

	## Bias and LMCoef same aux.var and interpolation
	ix3 <- (MRGcomb$mrg.method == "Saptio.Tempo.LM") & (MRGcomb$bias.interp == MRGcomb$sptrend.interp) & (MRGcomb$bias.auxvar == MRGcomb$sptrend.auxvar)

	ix <- ix1 & (ix2 | ix3)
	MRGcomb <- MRGcomb[ix, ]

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

	interp.grid$coords.stn$alon <- interp.grid$coords.stn@coords[, 'lon']
	interp.grid$coords.stn$alat <- interp.grid$coords.stn@coords[, 'lat']
	interp.grid$coords.grd$alon <- interp.grid$coords.grd@coords[, 'lon']
	interp.grid$coords.grd$alat <- interp.grid$coords.grd@coords[, 'lat']
	if(!is.null(interp.grid$coords.rfe)){
		interp.grid$coords.rfe$alon <- interp.grid$coords.rfe@coords[, 'lon']
		interp.grid$coords.rfe$alat <- interp.grid$coords.rfe@coords[, 'lat']
	}
	interp.grid$newgrid$alon <- interp.grid$newgrid@coords[, 'lon']
	interp.grid$newgrid$alat <- interp.grid$newgrid@coords[, 'lat']

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

	auxdf <- generateCombnation()
	bs.mthd <- paste(MRGcomb$bias.method, MRGcomb$bias.interp, MRGcomb$bias.auxvar, sep = '_')
	lm.mthd <- paste(MRGcomb$sptrend.interp, MRGcomb$sptrend.auxvar, sep = '_')
	mrg.mthd <- paste(MRGcomb$mrg.method, MRGcomb$mrg.interp, MRGcomb$mrg.auxvar, sep = '_')
	adj.dir <- file.path(adjDIR, bs.mthd)
	lm.dir <- ifelse(MRGcomb$mrg.method == "Saptio.Tempo.LM", file.path(SPLM.coefDIR, paste(bs.mthd, lm.mthd, sep = '-')), "")
	out.mrg <- paste(bs.mthd, lm.mthd, mrg.mthd, sep = '-')
	MRGcomb <- cbind(adj.dir, lm.dir, out.mrg, lm.mthd, MRGcomb$mrg.method, MRGcomb$mrg.interp, MRGcomb$mrg.auxvar)

	#####################################################################################

	InsertMessagesTxt(main.txt.out, 'Merge data ...')

	retfor <- foreach(ll = seq(nrow(MRGcomb)), .packages = packages, .export = toExports) %parLoop% {
		cbmrg <- MRGcomb[ll, ]
		GeneralParameters$Interpolation.pars$interp.method <- if(cbmrg[6] == 'OK') 'Kriging' else cbmrg[6]

		if(cbmrg[5] == "Reg.Kriging"){
			GeneralParameters$Mrg.Method <- "Regression Kriging"
			if(cbmrg[4] == "RK.sp.trend_noD") GeneralParameters$sp.trend.aux <- FALSE
			else GeneralParameters$sp.trend.aux <- TRUE
		}else{
			GeneralParameters$Mrg.Method <- "Spatio-Temporal LM"
			GeneralParameters$IO.files$LMCoef.dir <- cbmrg[2]
		}

		auxv <- as.logical(auxdf[auxdf$l == cbmrg[7], c("dem", "slope", "aspect", "lon", "lat")])
		GeneralParameters$auxvar$dem <- auxv[1]
		GeneralParameters$auxvar$slope <- auxv[2]
		GeneralParameters$auxvar$aspect <- auxv[3]
		GeneralParameters$auxvar$lon <- auxv[4]
		GeneralParameters$auxvar$lat <- auxv[5]

		adjInfo <- ncFilesInfo(freqData, start.date, end.date, months, cbmrg[1], adjFFormat, errmsg.adj)
		if(is.null(adjInfo)) return(NULL)

		origdir <- file.path(mrgDIR, cbmrg[3])
		dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

		paramsMRG <- list( GeneralParameters = GeneralParameters, stnData = stnData, ijInt = ijInt, adjInfo = adjInfo,
							rfeData = rfeData, interp.grid = interp.grid, bGrd = bGrd, origdir = origdir)

		ret <- MergingFunctionRain.validation(paramsMRG)
	}
	if(closeklust) stopCluster(klust)

	InsertMessagesTxt(main.txt.out, 'Merging finished')
	GeneralParameters <- NULL
	rm(rfeData, interp.grid, stnData, paramsMRG)

	## data time step
	freqData <- 'dekadal'

	## year range
	year1 <- 1983 
	year2 <- 2010

	## Output directory
	outDIR <- '/Users/rijaf/Desktop/vETH_ENACTS/BIAS'

	## Training data file (CDT data)
	stnFile <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/Data/RR_DEK_83to14_Train.txt'

	## Validation data file (CDT data)
	stnValidFile <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/Data/RR_DEK_83to14_Valid.txt'

	## Station missing values code
	miss.val <- -99

	## RFE directory
	rfeDir <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/TAMSAT_dek'
	## RFE filename format
	rfeFFormat <- 'rfe_%s%s%s.nc'

	## elevation data file
	demFile <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/dem/DEM_1_Arc-Minute.nc'

	#####################################################################################
	rad.lon <- 10
	rad.lat <- 10
	rad.elv <- 4
	maxdist <- 0.6

	months <- 1:12

	GeneralParameters$Interpolation.pars$nmin <- 3
	GeneralParameters$Interpolation.pars$nmax <- 10
	
	GeneralParameters$Bias.Factor$min.length <- 15
	GeneralParameters$Bias.Factor$min.stn <- 10

	############
	GeneralParameters$period <- freqData
	GeneralParameters$Bias.Months <- months
	GeneralParameters$Interpolation.pars$rad.lon <- rad.lon
	GeneralParameters$Interpolation.pars$rad.lat <- rad.lat
	GeneralParameters$Interpolation.pars$rad.elv <- rad.elv
	GeneralParameters$Interpolation.pars$maxdist <- maxdist
	GeneralParameters$Interpolation.pars$elev.3rd.dim <- TRUE
	GeneralParameters$Interpolation.pars$latlon.unit <- 'km'
	GeneralParameters$Interpolation.pars$normalize <- TRUE
	GeneralParameters$Interpolation.pars$use.block <- TRUE
	GeneralParameters$Interpolation.pars$vgm.model <- list(c("Sph", "Exp", "Gau"))
	GeneralParameters$Create.Grid <- "1"
	GeneralParameters$Prefix$Mean.Bias.Prefix <- "STN_RFE_MeanBias"
	memType <- 2

	#####################################################################################

	#### COMBINAISON

	BScomb <- Bias.combination()

	##use "noD", "D", "SA", "DSA"
	ix <- BScomb$bias.auxvar%in%c("noD", "D", "SA", "DSA")
	BScomb <- BScomb[ix, ]

	#####################################################################################
	## STN data
	stnData <- read.table(stnFile, stringsAsFactors = FALSE, na.strings = str_trim(miss.val))
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)

	# ## validation coordinates
	coordsValid <- read.table(stnValidFile, stringsAsFactors = FALSE, na.strings = str_trim(miss.val))
	coordsValid <- getCDTdataAndDisplayMsg(coordsValid, freqData)
	coordsValid <- coordsValid[c('lon', 'lat')]

	## DEM data
	nc <- nc_open(demFile)
	demData <- list(lon = nc$dim[[1]]$vals, lat = nc$dim[[2]]$vals, demMat = ncvar_get(nc, nc$var[[1]]$name))
	nc_close(nc)

	##### RFE data
	start.date <- as.Date(paste(year1, '0101', sep = ''), format = '%Y%m%d')
	end.date <- as.Date(paste(year2, '1231', sep = ''), format = '%Y%m%d')
	errmsg <- "RFE data not found"

	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, rfeDir, rfeFFormat, errmsg)
	## RFE info file
	nc <- nc_open(ncInfo$nc.files[ncInfo$exist][1])
	grd.lon <- nc$dim[[1]]$vals
	grd.lat <- nc$dim[[2]]$vals
	varid <- nc$var[[1]]$name
	nc_close(nc)

	msg <- list(start = 'Read RFE data ...', end = 'Reading RFE data finished')
	ncfiles <- list(freqData = freqData, start.date = start.date, end.date = end.date,
					months = months, ncDir = rfeDir, ncFileFormat = rfeFFormat)
	ncinfo <- list(xo = 1, yo = 2, varid = varid)
	read.ncdf.parms <- list(ncfiles = ncfiles, ncinfo = ncinfo, msg = msg, errmsg = errmsg)

	###########################################
	xy.grid <- xy.rfe <- list(lon = grd.lon, lat = grd.lat)
	nlon0 <- length(grd.lon)
	nlat0 <- length(grd.lat)

	#### validation interpolation coordinates
	ijInt <- grid2pointINDEX(list(lon = coordsValid$lon, lat = coordsValid$lat), xy.grid)

	#####################################################################################
	DAS <- expand.grid(dem = c(FALSE, TRUE), slope = c(FALSE, TRUE), aspect = c(FALSE, TRUE))
	aux.var <- apply(DAS, 1, function(j){
		x <- c('D', 'S', 'A')[j]
		if(length(x) > 0) paste(x, collapse = '')
		else 'noD'
	})
	auxdf <- data.frame(n = 1:8, l = aux.var, DAS, stringsAsFactors = FALSE)
	bs.mthd <- paste(BScomb$bias.method, BScomb$bias.interp, BScomb$bias.auxvar, sep = '_')
	BScomb <- apply(cbind(bs.mthd, BScomb), 2, as.character)

	#####################################################################################

	for(ll in seq(nrow(BScomb))){
		cbbs <- BScomb[ll, ]
		interp.method <- if(cbbs[3] == 'OK') 'Kriging' else cbbs[3]
		bias.method <- switch(unname(cbbs[2]),
						'QM' = 'Quantile.Mapping',
						'MBVar' = 'Multiplicative.Bias.Var',
						'MBMon' = 'Multiplicative.Bias.Mon')
		auxv <- as.logical(auxdf[auxdf$l == cbbs[4], c("dem", "slope", "aspect")])

		GeneralParameters$Interpolation.pars$interp.method <- interp.method
		GeneralParameters$Bias.Method <- bias.method
		GeneralParameters$auxvar$dem <- auxv[1]
		GeneralParameters$auxvar$slope <- auxv[2]
		GeneralParameters$auxvar$aspect <- auxv[3]

		res.coarse <- if(interp.method == 'NN') sqrt((rad.lon*mean(grd.lon[-1]-grd.lon[-nlon0]))^2 + (rad.lat*mean(grd.lat[-1]-grd.lat[-nlat0]))^2)/2 else maxdist/2
		res.coarse <- if(res.coarse  >= 0.25) res.coarse else 0.25
		origdir <- file.path(outDIR, cbbs[1])
		dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

		listDir <- do.call(rbind, strsplit(list.files(outDIR), '_'))
		ix <- listDir[, 1] == cbbs[2]
		ix <- if(cbbs[3] == 'NN') (ix & listDir[, 2] == 'NN') else (ix & listDir[, 2]%in%c('IDW', 'OK'))
		bias.file <- file.path(outDIR, apply(listDir[ix, , drop = FALSE], 1, paste, collapse = "_"), 'BIAS_PARAMS.RData')
		exbsfl <- file.exists(bias.file)
		if(any(exbsfl)){
			load(bias.file[exbsfl][1])
			bias.pars <- bias.pars$bias
		}else{
			comptMBiasparms <- list(GeneralParameters = GeneralParameters, stnData = stnData, rfeData = read.ncdf.parms,
									xy.rfe = xy.rfe, res.coarse = res.coarse, memType = memType, origdir = origdir)
			bias.pars <- ComputeMeanBiasRain.validation(comptMBiasparms)
		}

		###########################################

		interpBiasparams <- list(GeneralParameters = GeneralParameters, bias.pars = bias.pars, stnData = stnData, ijInt = ijInt,
								demData = demData, xy.grid = xy.grid, xy.rfe = xy.rfe, res.coarse = res.coarse, origdir = origdir)
		ret <- InterpolateMeanBiasRain.validation(interpBiasparams)
	}

	GeneralParameters <- NULL
	rm(interpBiasparams, demData)

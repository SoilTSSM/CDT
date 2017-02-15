
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
	outDIR <- '/Users/rijaf/Desktop/vETH_ENACTS/ADJ'
	## corrected data filename format
	adjFFormat <- 'rr_adj_%s%s%s.txt'

	## Bias coefs directory
	biasDIR <- '/Users/rijaf/Desktop/vETH_ENACTS/BIAS'

	## RFE directory
	rfeDir <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/TAMSAT_dek'
	## RFE filename format
	rfeFFormat <- 'rfe_%s%s%s.nc'

	## Training data file (CDT data)
	stnFile <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/Data/RR_DEK_83to14_Train.txt'

	## Validation data file (CDT data)
	stnValidFile <- '/Users/rijaf/Desktop/ECHANGE/CDT_WD/new_method_merging/data/ETH/Data/RR_DEK_83to14_Valid.txt'

	## Station missing values code
	miss.val <- -99

	#####################################################################################
	months <- 1:12

	GeneralParameters$Prefix$Adj.File.Format <- adjFFormat
	GeneralParameters$Prefix$Mean.Bias.Prefix <- "STN_RFE_MeanBias"
	GeneralParameters$period <- freqData
	GeneralParameters$Adjust.Months <- months
	GeneralParameters$Adjusted.to.Zero <- TRUE
	memType <- 2

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
	BScomb <- cbind(paste(as.character(BScomb$Bias), as.character(BScomb$interp), as.character(BScomb$aux), sep = '_'),
					apply(BScomb[, 3:1], 2, as.character))

	#####################################################################################
	## STN data
	stnData <- read.table(stnFile, stringsAsFactors = FALSE, na.strings = str_trim(miss.val))
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	stnData <- stnData[c('lon', 'lat')]

	# ## validation coordinates
	coordsValid <- read.table(stnValidFile, stringsAsFactors = FALSE, na.strings = str_trim(miss.val))
	coordsValid <- getCDTdataAndDisplayMsg(coordsValid, freqData)
	coordsValid <- coordsValid[c('lon', 'lat')]

	## RFE DATA
	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')
	msg <- list(start = 'Read RFE data ...', end = 'Reading RFE data finished')
	errmsg <- "RFE data not found"
	ncfiles <- list(freqData = freqData, start.date = start.date, end.date = end.date,
					months = months, ncDir = rfeDir, ncFileFormat = rfeFFormat)
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, rfeDir, rfeFFormat, errmsg)
	if(is.null(ncInfo)) return(NULL)
	nc <- nc_open(ncInfo$nc.files[ncInfo$exist][1])
	varid <- nc$var[[1]]$name
	nc_close(nc)
	ncinfo <- list(xo = 1, yo = 2, varid = varid)
	read.ncdf.parms <- list(ncfiles = ncfiles, ncinfo = ncinfo, msg = msg, errmsg = errmsg)

	#####################################################################################

	if(memType == 2){
		rfeData <- read.NetCDF.Data(read.ncdf.parms)
		if(is.null(rfeData)) return(NULL)

		irfe <- !sapply(rfeData$data, is.null)
		if(!any(irfe)){
			InsertMessagesTxt(main.txt.out, "All RFE data are missing", format = TRUE)
			return(NULL)
		}

		rfeData$dates <- rfeData$dates[irfe]
		rfeData$data <- rfeData$data[irfe]
	}else{
		ncfiles <- read.ncdf.parms$ncfiles
		ncInfo <- ncFilesInfo(ncfiles$freqData, ncfiles$start.date, ncfiles$end.date, ncfiles$months,
							ncfiles$ncDir, ncfiles$ncFileFormat, read.ncdf.parms$errmsg)
		if(is.null(ncInfo)) return(NULL)

		ncInfo$nc.files <- ncInfo$nc.files[ncInfo$exist]
		ncInfo$dates <- ncInfo$dates[ncInfo$exist]

		nc <- nc_open(ncInfo$nc.files[1])
		rlon <- nc$dim[[read.ncdf.parms$ncinfo$xo]]$vals
		rlat <- nc$dim[[read.ncdf.parms$ncinfo$yo]]$vals
		nc_close(nc)
		xo <- order(rlon)
		rlon <- rlon[xo]
		yo <- order(rlat)
		rlat <- rlat[yo]
		rfeData <- list(lon = rlon, lat = rlat, dates = ncInfo$dates, files = ncInfo$nc.files,
						xo = xo, yo = yo, varid = read.ncdf.parms$ncinfo$varid,
						yorder = read.ncdf.parms$ncinfo$yo)
	}

	#### validation interpolation coordinates
	ijInt <- grid2pointINDEX(list(lon = c(stnData$lon, coordsValid$lon), lat = c(stnData$lat, coordsValid$lat)),
								list(lon = rfeData$lon, lat = rfeData$lat))

	#####################################################################################

	for(ll in seq(nrow(BScomb))){
		cbbs <- BScomb[ll, ]
		bias.method <- switch(unname(cbbs[2]),
						'QM' = 'Quantile.Mapping',
						'MBVar' = 'Multiplicative.Bias.Var',
						'MBMon' = 'Multiplicative.Bias.Mon')
		auxv <- as.logical(auxdf[auxdf$l == cbbs[4], c("dem", "slope", "aspect")])

		GeneralParameters$Bias.Method <- bias.method
		GeneralParameters$auxvar$dem <- auxv[1]
		GeneralParameters$auxvar$slope <- auxv[2]
		GeneralParameters$auxvar$aspect <- auxv[3]

		Bias.dir <- file.path(biasDIR, cbbs[1])
		origdir <- file.path(outDIR, cbbs[1])
		GeneralParameters$IO.files$Bias.dir <- Bias.dir
		dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

		adjMeanBiasparms <- list(rfeData = NULL, GeneralParameters = GeneralParameters, ijInt = ijInt,
								origdir = origdir, memType = memType, readRFE = FALSE, RFEDATA = rfeData)
		ret <- AjdMeanBiasRain.validation(adjMeanBiasparms)
	}

	GeneralParameters <- NULL
	rm(adjMeanBiasparms, rfeData)

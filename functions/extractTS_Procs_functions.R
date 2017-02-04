getExtractDataFun <- function(retExtractParams){

	TSClimatologyFun <- function(){
		if(calc.anomaly == "1"){
			xanom <- data.frame(xtmp.clim,  stringsAsFactors = FALSE)
			if(period1 %in% c('daily', 'dekadal', 'monthly')){
				xanom[, 2] <- as.numeric(xanom[, 2])
				comp.fun <- paste('anomaly', period1, sep = '.')
				comp.fun <- match.fun(comp.fun)
				xadaty <- as.character(xanom[, 1])
				xanom <- round(comp.fun(xanom[, 2], xadaty), 1)
				xanom <- data.frame(Date = xadaty, Values = xanom)
				xanom <- xanom[as.numeric(substr(xadaty, 5, 6))%in%id.mois, , drop = FALSE]
				xanom[is.na(xanom[, 2]) | is.nan(xanom[, 2]), 2] <- -9999
			}else{
				if(period1 == 'yearly'){
					xanom[, 2] <- as.numeric(xanom[, 2])
					xanom[, 2] <- round(xanom[, 2]-mean(xanom[, 2], na.rm = TRUE), 1)
					xanom[is.na(xanom[, 2]) | is.nan(xanom[, 2]), 2] <- -9999
				}else{
					xanom[, 3] <- as.numeric(xanom[, 3])
					xanom[, 3] <- round(xanom[, 3]-mean(xanom[, 3], na.rm = TRUE), 1)
					xanom[is.na(xanom[, 3]) | is.nan(xanom[, 3]), 3] <- -9999
				}
			}
			writeFiles(xanom, out2sav.anom)
		}

		if(calc.stanomaly == "1"){
			xstanom <- data.frame(xtmp.clim,  stringsAsFactors = FALSE)
			if(period1 %in% c('daily', 'dekadal', 'monthly')){
				xstanom[, 2] <- as.numeric(xstanom[, 2])
				comp.fun <- paste('standard', period1, sep = '.')
				comp.fun <- match.fun(comp.fun)
				xadaty <- as.character(xstanom[, 1])
				xstanom <- comp.fun(xstanom[, 2], xadaty)
				xstanom <- data.frame(Date = xadaty, Values = round(xstanom, 2))
				xstanom <- xstanom[as.numeric(substr(xadaty, 5, 6))%in%id.mois, , drop = FALSE]
				xstanom[is.na(xstanom[, 2]) | is.nan(xstanom[, 2]), 2] <- -9999
			}else{
				if(period1 == 'yearly'){
					xstanom[, 2] <- as.numeric(xstanom[, 2])
					xstanom[, 2] <- (xstanom[, 2]-mean(xstanom[, 2], na.rm = TRUE))/sd(xstanom[, 2], na.rm = TRUE)
					xstanom[, 2] <- round(xstanom[, 2], 2)
					xstanom[is.na(xstanom[, 2]) | is.nan(xstanom[, 2]), 2] <- -9999
				}else{
					xstanom[, 3] <- as.numeric(xstanom[, 3])
					xstanom[, 3] <- (xstanom[, 3]-mean(xstanom[, 3], na.rm = TRUE))/sd(xstanom[, 3], na.rm = TRUE)
					xstanom[, 3] <- round(xstanom[, 3], 2)
					xstanom[is.na(xstanom[, 3]) | is.nan(xstanom[, 3]), 3] <- -9999
				}
			}
			writeFiles(xstanom, out2sav.stanom)
		}

		if(calc.climato == "1"){
			xclim <- data.frame(xtmp.clim,  stringsAsFactors = FALSE)
			if(period1 %in% c('daily', 'dekadal', 'monthly')){
				xclim [, 2] <- as.numeric(xclim [, 2])
				comp.fun <- paste('climato', period1, sep = '.')
				comp.fun <- match.fun(comp.fun)
				xclim <- comp.fun(xclim[, 2], as.character(xclim[, 1]), 'mean')
				xclim <- cbind(seq_along(xclim), round(xclim, 1))
				if(period1 == 'daily') ijmo <- which(as.numeric(format(seq(as.Date('2014-1-1'), by = 'day', length.out = 365), '%m'))%in%id.mois)
				if(period1 == 'dekadal') ijmo <- which(rep(1:12, each = 3)%in%id.mois)
				if(period1 == 'monthly') ijmo <- id.mois
				xclim <- xclim[ijmo, , drop = FALSE]
			}else{
				if(period1 == 'yearly'){
					xclim [, 2] <- as.numeric(xclim [, 2])
					xclim <- mean(xclim[, 2], na.rm = TRUE)
					xclim <- data.frame(Year =  'Yearly.mean', round(xclim, 1))
				}else{
					xclim [, 3] <- as.numeric(xclim [, 3])
					xclimv <- mean(xclim[, 3], na.rm = TRUE)
					xclim <- data.frame(Season = as.character(xclim[1, 1]), Values = round(xclimv, 1))
				}
			}
			xclim[is.na(xclim[, 2]) | is.nan(xclim[, 2]), 2] <- miss.val
			writeFiles(xclim, out2sav.clim)
		}
	}

	##########################

	CDTClimatologyFun <- function(){
		if(calc.anomaly == "1"){
			xanom <- xtmp.clim
			xdates <- as.character(xanom[, 1])
			xanom <- apply(xanom[, -1], 2, as.numeric)
			if(period1 %in% c('daily', 'dekadal', 'monthly')){
				comp.fun <- paste('anomaly', period1 , sep = '.') 
				comp.fun <- match.fun(comp.fun)

				xanom <- apply(xanom, 2, comp.fun, dates = xdates)
				xanom <- xanom[as.numeric(substr(xdates, 5, 6))%in%id.mois, , drop = FALSE]
			}else{
				xanom <- t(t(xanom)-apply(xanom, 2, mean, na.rm = TRUE))
			}
			xanom <- round(xanom, 1)
			xanom[is.na(xanom) | is.nan(xanom)] <- -9999

			if(is.cpt){
				cptIn <- list(freqOut = period2, xdon = xanom, xdates = xdates,
								xlon = headinfo[, 2], xlat = headinfo[, 3], xid = headinfo[, 1],
								varid = varid, units = units, miss.val = -9999)
				cptOut <- do.call(getDataCPT.Station, cptIn)
				cat(cptOut, file = out2sav.anom)
			}else{
				xtmp[-(1:3), -1] <- xanom
				writeFiles(xtmp, out2sav.anom)
			}
		}

		if(calc.stanomaly == "1"){
			xstanom <- xtmp.clim
			xdates <- as.character(xstanom[, 1])
			xstanom <- apply(xstanom[, -1], 2, as.numeric)
			if(period1 %in% c('daily', 'dekadal', 'monthly')){
				comp.fun <- paste('standard', period1, sep = '.')
				comp.fun <- match.fun(comp.fun)

				xstanom <- apply(xstanom, 2, comp.fun, dates = xdates)
				xstanom <- xstanom[as.numeric(substr(xdates, 5, 6))%in%id.mois, , drop = FALSE]
			}else{
				xstanom <- t((t(xstanom)-apply(xstanom, 2, mean, na.rm = TRUE))/apply(xstanom, 2, sd, na.rm = TRUE))
			}
			xstanom[is.na(xstanom) | is.nan(xstanom)] <- -9999
			xstanom <- round(xstanom, 2)

			if(is.cpt){
				cptIn <- list(freqOut = period2, xdon = xstanom, xdates = xdates,
								xlon = headinfo[, 2], xlat = headinfo[, 3], xid = headinfo[, 1],
								varid = varid, units = units, miss.val = -9999)
				cptOut <- do.call(getDataCPT.Station, cptIn)
				cat(cptOut, file = out2sav.stanom)
			}else{
				xtmp[-(1:3), -1] <- xstanom
				writeFiles(xtmp, out2sav.stanom)
			}
		}

		if(calc.climato == "1"){
			xclim <- xtmp.clim
			xdates <- as.character(xclim[, 1])
			xclim <- apply(xclim[, -1], 2, as.numeric)
			if(period1 %in% c('daily', 'dekadal', 'monthly')){
				comp.fun <- paste('climato', period1, sep = '.')
				comp.fun <- match.fun(comp.fun)

				xclim <- apply(xclim, 2, comp.fun, dates = xdates, fun = 'mean')
				xclim <- round(xclim, 1)
				if(period1 == 'daily') ijmo <- which(as.numeric(format(seq(as.Date('2014-1-1'), by = 'day', length.out = 365), '%m'))%in%id.mois)
				if(period1 == 'dekadal') ijmo <- which(rep(1:12, each = 3)%in%id.mois)
				if(period1 == 'monthly') ijmo <- id.mois
				
				xclim[is.na(xclim) | is.nan(xclim)] <- miss.val
				if(is.cpt){
					cptIn <- list(freqOut = period2, xdon = xclim, xdates = ijmo,
									xlon = headinfo[, 2], xlat = headinfo[, 3], xid = headinfo[, 1],
									varid = varid, units = units, miss.val = miss.val)
					cptOut <- do.call(getDataCPTclim.Station, cptIn)
				}else{
					xclim <- xclim[ijmo, , drop = FALSE]
					xclim <- t(cbind(t(xtmp[1:3, ]), t(cbind(ijmo, xclim))))
				}
			}else{
				xclim <- apply(xclim, 2, mean, na.rm = TRUE)
				xclim <- round(xclim, 1)
				xclim[is.na(xclim) | is.nan(xclim)] <- miss.val
				if(period1 == 'yearly') ijdate <- paste(c(xdates[1], xdates[length(xdates)]), collapse = '_')
				else ijdate <- paste(sapply(strsplit(strsplit(xdates[1], '_')[[1]], '-'), tail, n = 1), collapse = '_')
				if(is.cpt){
					cptIn <- list(freqOut = period2, xdon = matrix(xclim, nrow = 1), xdates = ijdate,
									xlon = headinfo[, 2], xlat = headinfo[, 3], xid = headinfo[, 1],
									varid = varid, units = units, miss.val = miss.val)
					cptOut <- do.call(getDataCPTclim.Station, cptIn)
				}else{
					xtmp[4, -1] <- xclim
					xclim <- xtmp[1:4, ]
					xclim[4, 1] <- ijdate
				}
			}

			if(is.cpt) cat(cptOut, file = out2sav.clim)
			else writeFiles(xclim, out2sav.clim)
		}
	}

	##########################

	ListDataClimatologyFun <- function(){
		if(calc.anomaly == "1"){
			xRVAL <- xtmp.clim
			if(period1 %in% c('daily', 'dekadal', 'monthly')){
				comp.fun <- paste('anomaly.', period1, '_lstOmat', sep = '')
				comp.fun <- match.fun(comp.fun)

				xRVAL <- comp.fun(xRVAL, daty.clim)
				xRVAL <- lapply(xRVAL, round, 1)
				xRVAL <- xRVAL[as.numeric(substr(daty.clim, 5, 6))%in%id.mois]
			}else{
				cmoy <- apply(simplify2array(xRVAL), 1:2, mean, na.rm = TRUE)
				xRVAL <- lapply(xRVAL, function(x) round(x-cmoy, 1))
				rm(cmoy)
			}

			xRVAL <- lapply(xRVAL, function(x){
				x[is.na(x) | is.nan(x)] <- -9999
				x
			})

			fileout.anom <- file.path(dirname(fileout), paste('Anomalies_', basename(fileout), sep = ''))

			if(is.cpt){
				cptIn <- list(freqOut = period2, xdon = xRVAL, xdates = outdonne$out$date,
								xlon = rlon[, 1], xlat = rlat[1, ], 
								varid = varid, units = units, miss.val = -9999)
				cptOut <- do.call(getDataCPT.GRDdata, cptIn)
				cat(cptOut, file = fileout.anom)
			}else{
				writeFilesListData(xRVAL, outdonne$out$date, capdate, fileout.anom, out2sav.anom)
			}
		}

		if(calc.stanomaly == "1"){
			xRVAL <- xtmp.clim
			if(period1 %in% c('daily', 'dekadal', 'monthly')){
				comp.fun <- paste('standard.', period1, '_lstOmat', sep = '')
				comp.fun <- match.fun(comp.fun)

				xRVAL <- comp.fun(xRVAL, daty.clim)
				xRVAL <- lapply(xRVAL, round, 2)
				xRVAL <- xRVAL[as.numeric(substr(daty.clim, 5, 6))%in%id.mois]
			}else{
					cmoy <- apply(simplify2array(xRVAL), 1:2, mean, na.rm = TRUE)
					csd <- apply(simplify2array(xRVAL), 1:2, sd, na.rm = TRUE)
					xRVAL <- lapply(xRVAL, function(x) round((x-cmoy)/csd, 1))
					rm(cmoy, csd)
			}

			xRVAL <- lapply(xRVAL, function(x){
				x[is.na(x) | is.nan(x)] <- -9999
				x
			})

			fileout.stanom <- file.path(dirname(fileout), paste('StandardizedAnomalies_', basename(fileout), sep = ''))

			if(is.cpt){
				cptIn <- list(freqOut = period2, xdon = xRVAL, xdates = outdonne$out$date,
								xlon = rlon[, 1], xlat = rlat[1, ], 
								varid = varid, units = units, miss.val = -9999)
				cptOut <- do.call(getDataCPT.GRDdata, cptIn)
				cat(cptOut, file = fileout.stanom)
			}else{
				writeFilesListData(xRVAL, outdonne$out$date, capdate, fileout.stanom, out2sav.stanom)
			}
		}

		if(calc.climato == "1"){
			xRVAL <- xtmp.clim
			if(period1 %in% c('daily', 'dekadal', 'monthly')){
				comp.fun <- paste('climato.', period1, '_lstOmat', sep = '')
				comp.fun <- match.fun(comp.fun)

				xRVAL <- comp.fun(xRVAL, daty.clim, fun = 'mean')
				xRVAL <- lapply(xRVAL, round, 1)

				if(period1 == 'daily') ijmo <- which(as.numeric(format(seq(as.Date('2014-1-1'), by = 'day', length.out = 365), '%m'))%in%id.mois)
				if(period1 == 'dekadal') ijmo <- which(rep(1:12, each = 3)%in%id.mois)
				if(period1 == 'monthly') ijmo <- id.mois
				xRVAL <- lapply(xRVAL, function(x){
					x[is.na(x) | is.nan(x)] <- miss.val
					x
				})

				if(is.cpt){
					cptIn <- list(freqOut = period2, xdon = xRVAL, xdates = ijmo,
									xlon = rlon[, 1], xlat = rlat[1, ], 
									varid = varid, units = units, miss.val = -9999)
					cptOut <- do.call(getDataCPTclim.GRDdata, cptIn)
					cat(cptOut, file = out2sav.clim)
				}else{
					xRVAL <- xRVAL[ijmo]
					fileout.clim <- file.path(dirname(fileout[1]), paste('Climatologies_Output_', ijmo, '.', file_ext(basename(fileout[1])), sep = ''))
					capdate <- paste('DATE:', ijmo)
					writeFilesListData(xRVAL, ijmo, capdate, fileout.clim, out2sav.clim)
				}
			}else{
				xRVAL <- round(apply(simplify2array(xRVAL), 1:2, mean, na.rm = TRUE), 1)
				xRVAL[is.na(xRVAL) | is.nan(xRVAL)] <- miss.val

				if(period1 == 'yearly') daty <- paste(c(daty.clim[1], daty.clim[length(daty.clim)]), collapse = '_')
				else daty <- paste(sapply(strsplit(strsplit(daty.clim[1], '_')[[1]], '-'), tail, n = 1), collapse = '_')
				
				if(is.cpt){
					cptIn <- list(freqOut = period2, xdon = xRVAL, xdates = daty,
									xlon = rlon[, 1], xlat = rlat[1, ], 
									varid = varid, units = units, miss.val =miss.val)
					cptOut <- do.call(getDataCPTclim1.GRDdata, cptIn)
					cat(cptOut, file = out2sav.clim)
				}else{
					fileout.clim <- file.path(dirname(fileout), paste('Climatologies_', daty, '.', file_ext(fileout), sep = ''))[1]
					capdate <- paste('DATE:', daty)
					writeFilesListData(xRVAL, daty, capdate, fileout.clim, out2sav.clim)
				}
			}
		}
	}

	##########################

	writeFilesListData <- function(RVAL, daty, capdate, fileout1, out2sav1){
		if(ChoixOutType == '1'){
			if(is.list(RVAL)){
				for(j in seq_along(RVAL)){
					writeFiles('Longitude', file = fileout1[j], append = TRUE)
					writeFiles(rlon, file = fileout1[j], append = TRUE)
					writeFiles('Latitude', file = fileout1[j], append = TRUE)
					writeFiles(rlat, file = fileout1[j], append = TRUE)
					writeFiles(capdate[j], file = fileout1[j], append = TRUE)
					xtmp <- round(RVAL[[j]], 1)
					xtmp[is.na(xtmp)] <- miss.val
					writeFiles(xtmp, file = fileout1[j], append = TRUE)
				}
			}else{
				writeFiles('Longitude', file = fileout1, append = TRUE)
				writeFiles(rlon, file = fileout1, append = TRUE)
				writeFiles('Latitude', file = fileout1, append = TRUE)
				writeFiles(rlat, file = fileout1, append = TRUE)
				writeFiles(capdate, file = fileout1, append = TRUE)
				xtmp <- round(RVAL, 1)
				xtmp[is.na(xtmp)] <- miss.val
				writeFiles(xtmp, file = fileout1, append = TRUE)
			}
		}

		if(ChoixOutType == '2'){
			writeFiles('Longitude', file = out2sav1, append = TRUE)
			writeFiles(rlon, file = out2sav1, append = TRUE)
			writeFiles('Latitude', file = out2sav1, append = TRUE)
			writeFiles(rlat, file = out2sav1, append = TRUE)
			if(is.list(RVAL)){
				for(j in seq_along(RVAL)){
					writeFiles(capdate[j], file = out2sav1, append = TRUE)
					xtmp <- round(RVAL[[j]], 1)
					xtmp[is.na(xtmp)] <- miss.val
					writeFiles(xtmp, file = out2sav1, append = TRUE)
				}
			}else{
				writeFiles(capdate, file = out2sav1, append = TRUE)
				xtmp <- round(RVAL, 1)
				xtmp[is.na(xtmp)] <- miss.val
				writeFiles(xtmp, file = out2sav1, append = TRUE)
			}
		}

		if(ChoixOutType == '3'){
			xlon <- rlon[, 1]
			xlat <- as.numeric(rlat[1, ])
			xcrds <- expand.grid(xlon, xlat, daty)
			xcrds <- xcrds[, 3:1]
			if(is.list(RVAL)) xdata <- round(unlist(RVAL), 1)
			else xdata <- round(c(RVAL), 1)
			xdata[is.na(xdata)] <- miss.val
			xtmp <- cbind(xcrds, xdata)
			writeFiles(xtmp, out2sav1)
		}
	}

	######################################################################################################

	period <- retExtractParams$freq
	ncdir <- retExtractParams$ncdat$dir
	ncfformat <- retExtractParams$ncdat$format

	range.date <- as.numeric(str_trim(retExtractParams$dates))
	yrs1 <- range.date[1]
	mon1 <- range.date[2]
	day1 <- range.date[3]
	yrs2 <- range.date[4]
	mon2 <- range.date[5]
	day2 <- range.date[6]
	usemon1 <- retExtractParams$usemon$start
	usemon2 <- retExtractParams$usemon$end

	outTS <- retExtractParams$out.ts
	season1 <- retExtractParams$seasmon$start
	season2 <- retExtractParams$seasmon$end

	aggfun <- retExtractParams$aggre$fun
	missfrac <- as.numeric(retExtractParams$aggre$missfrac)

	extType <- retExtractParams$area.type
	xyrect <- as.numeric(retExtractParams$rect)
	xminLon <- xyrect[1]
	xmaxLon <- xyrect[2]
	xminLat <- xyrect[3]
	xmaxLat <- xyrect[4]

	shpfl <- retExtractParams$shpdat$shpf
	shpId <- retExtractParams$shpdat$attr
	polyName <- retExtractParams$shpdat$id

	spAvrg <- retExtractParams$sp.ave
	out2sav <- retExtractParams$outdir
	ChoixOutType <- retExtractParams$out.type
	multiptspoly <- retExtractParams$polyg

	calc.anomaly <- retExtractParams$climato$anom
	calc.stanomaly <- retExtractParams$climato$stanom
	calc.climato <- retExtractParams$climato$clim
	pts.int <- abs(as.numeric(retExtractParams$pts.int))
	pmLon <- pts.int[1]
	pmLat <- pts.int[2]
	is.cpt <- retExtractParams$is.cpt

	confpath <- file.path(apps.dir, 'configure', 'configure_user.json')
	conffile <- fromJSON(confpath)
	miss.val <- as.numeric(str_trim(conffile$missing.value))

	if(is.cpt & extType%in%c('Rectangle', 'Polygon', 'Multiple Points', 'Multiple Polygons')) spAvrg <- '0'

	####
	outTsTable <- cbind(c('Daily', 'Dekadal', 'Monthly', '3-Months', '6-Months', 'Yearly'),
						c('daily', 'dekadal', 'monthly', 'season3', 'season6', 'yearly'))
	period1 <- outTsTable[outTsTable[, 1] == outTS, 2]
	if(is.cpt) period2 <- ifelse(period1%in%c('season3', 'season6'), 'seasonal', period1)

	####
	if(is.na(yrs1) | is.na(mon1) | is.na(day1) | is.na(yrs2) | is.na(mon2) | is.na(day2)){
		InsertMessagesTxt(main.txt.out, "Invalid date for time series extraction", format = TRUE)
		return(NULL)
	}

	####
	if(period == 'Daily data'){
		period0 <- 'daily'
		daty1 <- try(as.Date(paste(yrs1, mon1, day1, sep = '-')), silent = TRUE)
		daty2 <- try(as.Date(paste(yrs2, mon2, day2, sep = '-')), silent = TRUE)
		if(inherits(daty1, "try-error") | inherits(daty2, "try-error")){
			InsertMessagesTxt(main.txt.out, "Invalid date for time series extraction", format = TRUE)
			return(NULL)
		}
		dates <- format(seq(daty1, daty2, 'day'), '%Y%m%d')
		ncfiles <- sprintf(ncfformat, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 8))
	}

	if(period == 'Dekadal data'){
		period0 <- 'dekadal'
		daty1 <- try(as.Date(paste(yrs1, mon1, day1, sep = '-')), silent = TRUE)
		daty2 <- try(as.Date(paste(yrs2, mon2, day2, sep = '-')), silent = TRUE)
		if(inherits(daty1, "try-error") | inherits(daty2, "try-error")){
			InsertMessagesTxt(main.txt.out, "Invalid date for time series extraction", format = TRUE)
			return(NULL)
		}
		dates <- seq(daty1, daty2, 'day')
		dates <- paste(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%Y%m'),
					as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%d')), sep = '')
		ncfiles <- sprintf(ncfformat, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 7))
	}

	if(period == 'Monthly data'){
		period0 <- 'monthly'
		daty1 <- try(as.Date(paste(yrs1, mon1, day1, sep = '-')), silent = TRUE)
		daty2 <- try(as.Date(paste(yrs2, mon2, day2, sep = '-')), silent = TRUE)
		if(inherits(daty1, "try-error") | inherits(daty2, "try-error")){
			InsertMessagesTxt(main.txt.out, "Invalid date for time series extraction", format = TRUE)
			return(NULL)
		}
		dates <- format(seq(daty1, daty2, 'month'), '%Y%m')
		ncfiles <- sprintf(ncfformat, substr(dates, 1, 4), substr(dates, 5, 6))
	}

	if(!file.exists(ncdir)){
		InsertMessagesTxt(main.txt.out, "Directory containing NetCDF files does not exist", format = TRUE)
		return(NULL)
	}

	###
	dates0 <- dates
	id.mois <- getMonthsInSeason(usemon1, usemon2)
	imois <- as.numeric(substr(dates, 5, 6))%in%id.mois
	dates <- dates[imois]
	ncfiles <- ncfiles[imois]

	ncpath <- file.path(ncdir, ncfiles, fsep = .Platform$file.sep)
	existFl <- unlist(lapply(ncpath, file.exists))
	if(!any(existFl)){
		InsertMessagesTxt(main.txt.out, "Invalid filename format or date outside the range", format = TRUE)
		return(NULL)
	}

	############
	
	id.seas <- getMonthsInSeason(season1, season2)
	if(!all(id.seas%in%id.mois) & period1%in%c('season3', 'season6')){
		InsertMessagesTxt(main.txt.out, "Selected months and season do not overlap", format = TRUE)
		return(NULL)
	}

	if(extType == 'Point'){
		if(is.na(xminLon) | is.na(xminLat)){
			InsertMessagesTxt(main.txt.out, "Invalid coordinates to extract", format = TRUE)
			return(NULL)
		}
	}

	if(extType == 'Rectangle'){
		if(is.na(xminLon) | is.na(xmaxLon) | is.na(xminLat) | is.na(xmaxLat)){
			InsertMessagesTxt(main.txt.out, "Invalid coordinates for the extraction", format = TRUE)
			return(NULL)
		}
	}

	if(extType == 'Polygon'){
		shpf <- getShpOpenData(shpfl)[[2]]
		if(!is.null(shpf)){
			shpf.union <- unionSpatialPolygons(shpf, as.character(shpf@data[, shpId]))
			shpf.df <- aggregate(as(shpf, "data.frame")[, 1], list(as.character(shpf@data[, shpId])), identity)
			shpf.df$x <- seq(shpf.union)
			row.names(shpf.df) <- sapply(slot(shpf.union, "polygons"), function(x) slot(x, "ID"))
			shpf.union <- SpatialPolygonsDataFrame(shpf.union, shpf.df)
			regOI <- shpf.union[as.character(shpf.union@data$Group.1) == polyName, ]
			bbxregOI <- bbox(regOI)
		}else{
			InsertMessagesTxt(main.txt.out, "Ceci ne devrait pas se produire", format = TRUE)
			return(NULL)
		}
	}

	if(extType%in%c('Multiple Points', 'Multiple Polygons')){
		if(!is.na(multiptspoly)){
			multiptspoly <- gsub("[\r]", "", multiptspoly)
			multiptspoly <- str_trim(strsplit(multiptspoly, "[\n]")[[1]])
			multiptspoly <- multiptspoly[multiptspoly != ""]
			if(length(multiptspoly) == 0){
				InsertMessagesTxt(main.txt.out, "No coordinates found", format = TRUE)
				return(NULL)
			}
			if(extType == 'Multiple Points'){
				multiptspoly <- t(sapply(multiptspoly, function(x) strsplit(x, " ")[[1]]))
				multiptspoly <- data.frame(multiptspoly, stringsAsFactors = FALSE)
				rownames(multiptspoly) <- NULL
				names(multiptspoly) <- c('id', 'x', 'y')
				multiptspoly[, 2:3] <- apply(multiptspoly[, 2:3, drop = FALSE], 2, as.numeric)
				headinfo <- multiptspoly
			}

			if(extType == 'Multiple Polygons'){
				shpf <- getShpOpenData(shpfl)[[2]]
				if(!is.null(shpf)){
					shpf.union <- unionSpatialPolygons(shpf, as.character(shpf@data[, shpId]))
					shpf.df <- aggregate(as(shpf, "data.frame")[, 1], list(as.character(shpf@data[, shpId])), identity)
					shpf.df$x <- seq(shpf.union)
					row.names(shpf.df) <- sapply(slot(shpf.union, "polygons"), function(x) slot(x, "ID"))
					shpf.union <- SpatialPolygonsDataFrame(shpf.union, shpf.df)
					regOI <- shpf.union[str_trim(shpf.union@data$Group.1)%in%multiptspoly, ]
					bbxregOI <- bbox(regOI)
					headinfo <- cbind(as.character(regOI@data$Group.1), round(coordinates(regOI), 5))
					headinfo[, 1] <- substr(str_replace_all(headinfo[, 1], "[^[:alnum:]]", ""), 1, 15)
				}
			}
		}else{
			if(extType == 'Multiple Points') InsertMessagesTxt(main.txt.out, "No selected points", format = TRUE)
			if(extType == 'Multiple Polygons') InsertMessagesTxt(main.txt.out, "No selected polygons", format = TRUE)
			return(NULL)
		}
	}

	###########################################################################################

	if(calc.climato == "1") out2sav.clim <- file.path(dirname(out2sav), paste('Climatologies_', basename(out2sav), sep = ''))
	if(calc.anomaly == "1") out2sav.anom <- file.path(dirname(out2sav), paste('Anomalies_', basename(out2sav), sep = ''))
	if(calc.stanomaly == "1") out2sav.stanom <- file.path(dirname(out2sav), paste('StandardizedAnomalies_', basename(out2sav), sep = ''))

	###########################################################################################

	nc <- nc_open(ncpath[existFl][1])
	xlon <- nc$dim[[1]]$vals
	xlat <- nc$dim[[2]]$vals
	varid <- nc$var[[1]]$name
	units <- nc$var[[1]]$units
	xval <- ncvar_get(nc, varid = varid)
	nc_close(nc)

	if(extType == 'Point'){
		nx <- xlon[2]-xlon[1]
		padx <- round(pmLon/nx)
		ny <- xlat[2]-xlat[1]
		pady <- round(pmLat/ny)
		voisin <- expand.grid(x = xminLon + nx*(-padx:padx), y =  xminLat + ny*(-pady:pady))

		iclo <- findInterval(voisin$x, xlon)
		ilo <- iclo+(2 * voisin$x > xlon[iclo]+xlon[iclo+1])
		icla <- findInterval(voisin$y, xlat)
		ila <- icla+(2 * voisin$y > xlat[icla]+xlat[icla+1])
		ilola <- !is.na(ilo) & !is.na(ila)
		if(any(ilola)){
			ilo <- ilo[ilola]
			ila <- ila[ilola]
		}
	}else  if(extType == 'Multiple Points'){
		ilo <- xlon >= (min(multiptspoly$x, na.rm = TRUE)-pmLon) & xlon <= (max(multiptspoly$x, na.rm = TRUE)+pmLon)
		if(!any(ilo)){
			InsertMessagesTxt(main.txt.out, "No data to extract: Object outside data range", format = TRUE)
			return(NULL)
		}

		iloL <- which(ilo)
		if(iloL[1] > 1) ilo[iloL[1]-1] <- TRUE
		if(iloL[length(iloL)] < length(ilo)) ilo[iloL[length(iloL)]+1] <- TRUE
		rlon <- xlon[ilo]

		ila <- xlat >= (min(multiptspoly$y, na.rm = TRUE)-pmLat) & xlat <= (max(multiptspoly$y, na.rm = TRUE)+pmLat)
		if(!any(ila)){
			InsertMessagesTxt(main.txt.out, "No data to extract: Object outside data range", format = TRUE)
			return(NULL)
		}
		ilaL <- which(ila)
		if(ilaL[1] > 1) ila[ilaL[1]-1] <- TRUE
		if(ilaL[length(ilaL)] < length(ila)) ila[ilaL[length(ilaL)]+1] <- TRUE
		rlat <- xlat[ila]

		sptNC <- expand.grid(x = rlon, y = rlat)
		coordinates(sptNC) <- ~x+y
		sptNC <- SpatialPixels(points = sptNC, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))
		nx <- sptNC@grid@cellsize[1]
		padx <- round(pmLon/nx)
		ny <- sptNC@grid@cellsize[2]
		pady <- round(pmLat/ny)

		pts.loc <- multiptspoly[, c('x', 'y'), drop = FALSE]
		pts.w.voisin <- lapply(1:nrow(pts.loc), function(j){
			voisin <- expand.grid(x = pts.loc$x[j] + nx*(-padx:padx), y =  pts.loc$y[j] + ny*(-pady:pady))
			cbind(voisin, j)
		})
		pts.w.voisin <- do.call('rbind', pts.w.voisin)

		if(pmLon > 0 | pmLat > 0){
			coordinates(pts.w.voisin)<- ~x+y
			pts.w.voisin$ijv <- unname(over(pts.w.voisin, geometry(sptNC)))
			pts.w.voisin <- pts.w.voisin[!is.na(pts.w.voisin$ijv), ]
		}else{
			coordinates(pts.loc) <- ~x+y
		}
	}else if(extType == 'Multiple Polygons'){
		ilo <- xlon >= bbxregOI[1, 1] & xlon <= bbxregOI[1, 2]
		ila <- xlat >= bbxregOI[2, 1] & xlat <= bbxregOI[2, 2]
		if(!any(ilo) | !any(ila)){
			InsertMessagesTxt(main.txt.out, "No data to extract: Object outside data range", format = TRUE)
			return(NULL)
		}
		rlon <- xlon[ilo]
		rlat <- xlat[ila]

		rval <- xval[ilo, ila]
		rval[] <- 1:prod(dim(rval))
		sptNC <- expand.grid(x = rlon, y = rlat)
		coordinates(sptNC) <- ~x+y
		sptNC <- SpatialPixels(points = sptNC, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))
		sptNC$z <- c(rval)
		sptNC <- raster(sptNC)
		idmat <- extract(sptNC, regOI, weights = TRUE, normalizeWeights = TRUE, cellnumbers = TRUE)
	}else{
		if(extType == 'Rectangle'){
			ilo <- xlon >= xminLon & xlon <= xmaxLon
			ila <- xlat >= xminLat & xlat <= xmaxLat
			if(!any(ilo) | !any(ila)){
				InsertMessagesTxt(main.txt.out, "No data to extract: Object outside data range", format = TRUE)
				return(NULL)
			}
			rlon <- xlon[ilo]
			rlat <- xlat[ila]
			rval <- xval[ilo, ila]
		}
		if(extType == 'Polygon'){
			ilo <- xlon >= bbxregOI[1, 1] & xlon <= bbxregOI[1, 2]
			ila <- xlat >= bbxregOI[2, 1] & xlat <= bbxregOI[2, 2]
			if(!any(ilo) | !any(ila)){
				InsertMessagesTxt(main.txt.out, "No data to extract: Object outside data range", format = TRUE)
				return(NULL)
			}
			rlon <- xlon[ilo]
			rlat <- xlat[ila]

			rval <- xval[ilo, ila]
			rval[] <- 1:prod(dim(rval))
			w.pix <- id.pix <- rval
			w.pix[] <- id.pix[] <- NA
			sptNC <- expand.grid(x = rlon, y = rlat)
			coordinates(sptNC) <- ~x+y
			sptNC <- SpatialPixels(points = sptNC, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))
			sptNC$z <- c(rval)
			sptNC <- raster(sptNC)
			idmat <- extract(sptNC, regOI, weights = TRUE, normalizeWeights = TRUE, cellnumbers = TRUE)
			id.pix[idmat[[1]][, 2]] <- idmat[[1]][, 2]
			w.pix[idmat[[1]][, 2]] <- idmat[[1]][, 3]
			idmat <- list(id = id.pix, weight = w.pix)
		}
	}

	######################

	if(doparallel & length(ncpath[existFl]) >= 30){
		klust <- makeCluster(nb_cores)
		registerDoParallel(klust)
		`%parLoop%` <- `%dopar%`
		closeklust <- TRUE
	}else{
		`%parLoop%` <- `%do%`
		closeklust <- FALSE
	}
	NCDATA <- foreach(fl = seq_along(ncpath), .packages = 'ncdf4',
					.export = c('ncpath', 'existFl')) %parLoop% {
		if(!existFl[fl]) return(NULL)
		nc <- try(nc_open(ncpath[fl]), silent = TRUE)
		if(inherits(nc, "try-error")) return(NULL)
		xval <- ncvar_get(nc, varid = nc$var[[1]]$name)
		nc_close(nc)
		return(xval)
	}
	if(closeklust) stopCluster(klust)

	######################

	RVAL <- lapply(NCDATA, function(xval){
		if(is.null(xval)) return(NULL)
		rval <- xval[ilo, ila]
		if(extType == 'Point'){
			if(!any(ilola)) rval <- NA
			else{
				if(length(ilo) > 1) rval <- mean(diag(rval), na.rm = TRUE)
			}
		}else if(extType == 'Multiple Points'){
			if(pmLon > 0 | pmLat > 0) rval <- tapply(rval[pts.w.voisin$ijv], pts.w.voisin$j, mean, na.rm = TRUE)
			else rval <- rval[unname(over(pts.loc, geometry(sptNC)))]
		}else if(extType == 'Multiple Polygons'){
			rval <- sapply(idmat, function(x) if(!is.null(x)) round(sum(rval[x[, 2]]*x[, 3], na.rm = TRUE), 1) else NA)
		}else{
			if(extType == 'Rectangle'){
				if(spAvrg == '1'){
					rval <- round(mean(rval, na.rm = TRUE), 1)
					rval <- if(is.nan(rval)) NA else rval
				}
			}
			if(extType == 'Polygon'){
				rval[is.na(idmat$id)] <- NA
				if(spAvrg == '1'){
					rval <- round(sum(rval*idmat$weight, na.rm = TRUE))
					rval <- if(is.nan(rval)) NA else rval
				}
			}
		}
		return(rval)
	})

	######################################################################################################

	if(extType == 'Point'){
		RVAL[sapply(RVAL, is.null)] <- NA
		xval <- unlist(RVAL)
		
		if(period0 == period1){
			xtmp.clim <- cbind(dates0, xval[match(dates0, dates)])
			xtmp <- round(xval, 1)
			xtmp[is.na(xtmp)] <- miss.val
			xtmp <- cbind(dates, xtmp)
		}else{
			if(period1 == 'season3'){
				xtmp <- seasonal_fun(period0, xval, dates, smon = season1, lmon = 3, fun = aggfun, frac = missfrac)
				xtmp.clim <- xtmp
				xtmp[, 3] <- round(xtmp[, 3], 1)
				xtmp[is.na(xtmp[, 3]), 3] <- miss.val
			}else if(period1 == 'season6'){
				xtmp <- seasonal_fun(period0, xval, dates, smon = season1, lmon = 6, fun = aggfun, frac = missfrac)
				xtmp.clim <- xtmp
				xtmp[, 3] <- round(xtmp[, 3], 1)
				xtmp[is.na(xtmp[, 3]), 3] <- miss.val
			}else{
				comp.fun <- paste(period0, 2, period1, sep = '')
				comp.fun <- match.fun(comp.fun)
				xtmp <- comp.fun(xval, dates, fun = aggfun, frac = missfrac)
				daty.clim <- as.character(comp.fun(rep(1, length(dates0)), dates0, fun = aggfun)[, 1])
				xtmp.clim <- cbind(daty.clim, xtmp[match(daty.clim, as.character(xtmp[, 1])), 2])
				xtmp[, 2] <- round(xtmp[, 2], 1)
				xtmp[is.na(xtmp[, 2]), 2] <- miss.val
			}
		}

		writeFiles(xtmp, out2sav)
		###### clim
		TSClimatologyFun()
	}else if(extType%in%c('Multiple Points', 'Multiple Polygons')){
		RVAL[sapply(RVAL, is.null)] <- list(rep(NA, nrow(headinfo)))
		xval <- do.call('rbind', RVAL)
		capition <- c('Stations', 'LON', 'DATE/LAT')

		nona <- apply(xval, 2, function(x) sum(!is.na(x)) > 0)
		if(!any(nona)){
			InsertMessagesTxt(main.txt.out, "All data are missing", format = TRUE)
			return(NULL)
		}
		xval <- xval[, nona, drop = FALSE]
		headinfo <- headinfo[nona, , drop = FALSE]

		if(period0 == period1){
			outdonne <- NULL
			outdonne$out$date <- dates
			outdonne$out$data <- round(xval, 1)
			xtmp <- data.frame(outdonne$out$date, outdonne$out$data)
			xtmp.clim <- cbind(dates0, xval[match(dates0, dates), ])
		}else{
			if(period1 == 'season3'){
				start.mon <- which(format(ISOdate(2014,1:12,1), "%b")%in%season1)
				outdonne <- aggregateSeries(xval, dates, fun = aggfun, na.rm = TRUE, min.frac = missfrac,
							time.step.In = period0, time.step.Out = c(start.mon, 3), type = 'matrix')
				
				xtmp <- data.frame(outdonne$out$date, round(outdonne$out$data, 1))
				xtmp.clim <- xtmp
			}else if(period1 == 'season6'){
				start.mon <- which(format(ISOdate(2014,1:12,1), "%b")%in%season1)
				outdonne <- aggregateSeries(xval, dates, fun = aggfun, na.rm = TRUE, min.frac = missfrac,
							time.step.In = period0, time.step.Out = c(start.mon, 6), type = 'matrix')
				xtmp <- data.frame(outdonne$out$date, round(outdonne$out$data, 1))
				xtmp.clim <- xtmp
			}else{
				outdonne <- aggregateSeries(xval, dates, fun = aggfun, na.rm = TRUE, min.frac = missfrac,
							time.step.In = period0, time.step.Out = period1, type = 'matrix')
				if(period1 != "yearly"){
					xmon <- as.numeric(substr(outdonne$out$date, 5, 6))%in%id.mois
					outdonne$out$date <- outdonne$out$date[xmon]
					outdonne$out$data <- outdonne$out$data[xmon, , drop = FALSE]
				}
				xtmp <- data.frame(outdonne$out$date, round(outdonne$out$data, 1))
				daty.clim <- aggregateSeries(rep(1, length(dates0)), dates0, fun = aggfun, 
							time.step.In = period0, time.step.Out = period1, type = 'vector')
				xtmp.clim <- cbind(daty.clim$out$date, xtmp[match(daty.clim$out$date, outdonne$out$date), -1])
			}
		}

		if(is.cpt){
			outdonne$out$data[is.na(outdonne$out$data)] <- miss.val
			cptIn <- list(freqOut = period2, xdon = outdonne$out$data, xdates = outdonne$out$date,
							xlon = headinfo[, 2], xlat = headinfo[, 3], xid = headinfo[, 1],
							varid = varid, units = units, miss.val = miss.val)
			cptOut <- do.call(getDataCPT.Station, cptIn)
			cat(cptOut, file = out2sav)
			if(calc.climato == "1") xtmp <- t(cbind(t(cbind(capition, t(headinfo))), t(xtmp)))
		}else{
			xtmp[is.na(xtmp)] <- miss.val
			xtmp <- t(cbind(t(cbind(capition, t(headinfo))), t(xtmp)))
			writeFiles(xtmp, out2sav)
		}
		###### clim
		CDTClimatologyFun()
	}else{
		if(spAvrg == '1'){
			
			RVAL[sapply(RVAL, is.null)] <- NA
			xval <- unlist(RVAL)
			
			if(period0 == period1){
				xtmp.clim <- cbind(dates0, xval[match(dates0, dates)])
				xval[is.na(xval)] <- miss.val
				xtmp <- cbind(dates, xval)
			}else{
				if(period1 == 'season3'){
					xtmp <- seasonal_fun(period0, xval, dates, smon = season1, lmon = 3, fun = aggfun, frac = missfrac)
					xtmp.clim <- xtmp
					xtmp[, 3] <- round(xtmp[, 3], 1)
					xtmp[is.na(xtmp[, 3]), 3] <- miss.val
				}else if(period1 == 'season6'){
					xtmp <- seasonal_fun(period0, xval, dates, smon = season1, lmon = 6, fun = aggfun, frac = missfrac)
					xtmp.clim <- xtmp
					xtmp[, 3] <- round(xtmp[, 3], 1)
					xtmp[is.na(xtmp[, 3]), 3] <- miss.val
				}else{
					comp.fun <- paste(period0, 2, period1, sep = '')
					comp.fun <- match.fun(comp.fun)
					xtmp <- comp.fun(xval, dates, fun = aggfun, frac = missfrac)
					daty.clim <- as.character(comp.fun(rep(1, length(dates0)), dates0, fun = aggfun)[, 1])
					xtmp.clim <- cbind(daty.clim, xtmp[match(daty.clim, as.character(xtmp[, 1])), 2])
					xtmp[, 2] <- round(xtmp[, 2], 1)
					xtmp[is.na(xtmp[, 2]), 2] <- miss.val
				}
			}
			
			writeFiles(xtmp, out2sav)
			###### clim
			TSClimatologyFun()
		}else{
			RVAL[sapply(RVAL, is.null)] <- list(matrix(NA, ncol = length(rlat), nrow = length(rlon)))
			rlon <- matrix(round(rlon, 5), nrow = nrow(rval), ncol = ncol(rval))
			rlat <- matrix(round(rlat, 5), nrow = nrow(rval), ncol = ncol(rval), byrow = TRUE)
			if(period0 == period1){
				outdonne <- NULL
				outdonne$out$date <- dates
				outdonne$out$data <- RVAL

				daty.clim <- dates0
				xtmp.clim <- RVAL[match(dates0, dates)]
				xtmp.clim[sapply(xtmp.clim, is.null)] <- list(matrix(NA, nrow = nrow(rval), ncol = ncol(rval)))
			}else{
				if(period1 == 'season3'){
					start.mon <- which(format(ISOdate(2014,1:12,1), "%b")%in%season1)
					outdonne <- aggregateSeries(RVAL, dates, fun = aggfun, na.rm = TRUE, min.frac = missfrac,
								time.step.In = period0, time.step.Out = c(start.mon, 3), type = 'list')

					fileout <- file.path(out2sav, paste('Output_', outdonne$out$date, '.txt', sep = ''))
					capdate <- paste('DATE:', outdonne$out$date)

					daty.clim <- outdonne$out$date
					xtmp.clim <- outdonne$out$data
				}else if(period1 == 'season6'){
					start.mon <- which(format(ISOdate(2014,1:12,1), "%b")%in%season1)
					outdonne <- aggregateSeries(RVAL, dates, fun = aggfun, na.rm = TRUE, min.frac = missfrac,
								time.step.In = period0, time.step.Out = c(start.mon, 6), type = 'list')

					daty.clim <- outdonne$out$date
					xtmp.clim <- outdonne$out$data
				}else{
					outdonne <- aggregateSeries(RVAL, dates, fun = aggfun, na.rm = TRUE, min.frac = missfrac,
											time.step.In = period0, time.step.Out = period1, type = 'list')
					if(period1 != "yearly"){
						xmon <- as.numeric(substr(outdonne$out$date, 5, 6))%in%id.mois
						outdonne$out$date <- outdonne$out$date[xmon]
						outdonne$out$data <- outdonne$out$data[xmon]
					}
					daty.clim <- aggregateSeries(rep(1, length(dates0)), dates0, fun = aggfun, 
								time.step.In = period0, time.step.Out = period1, type = 'vector')
					xtmp.clim <- outdonne$out$data
					xtmp.clim <- xtmp.clim[match(daty.clim$out$date, outdonne$out$date)]
					xtmp.clim[sapply(xtmp.clim, is.null)] <- list(matrix(NA, nrow = nrow(rval), ncol = ncol(rval)))
					daty.clim <- daty.clim$out$date
				}
			}

			if(is.cpt){
				fileout <- out2sav
				cptIn <- list(freqOut = period2, xdon = outdonne$out$data, xdates = outdonne$out$date,
								xlon = rlon[, 1], xlat = rlat[1, ], 
								varid = varid, units = units, miss.val = miss.val)
				cptOut <- do.call(getDataCPT.GRDdata, cptIn)
				cat(cptOut, file = fileout)
			}else{
				fileout <- file.path(out2sav, paste('Output_', outdonne$out$date, '.txt', sep = ''))
				capdate <- paste('DATE:', outdonne$out$date)
				writeFilesListData(outdonne$out$data, outdonne$out$date, capdate, fileout, out2sav)
			}

			###### clim	
			ListDataClimatologyFun()
		}
	}

	tcl('update')
	return(0)
}



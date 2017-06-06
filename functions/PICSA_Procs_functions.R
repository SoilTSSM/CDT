
PICSAProcs <- function(GeneralParameters){

	rainFl <- GeneralParameters$RAIN$dirORfile
	rainSl <- GeneralParameters$RAIN$sample
	rainFf <- GeneralParameters$RAIN$format

	compute.ETP <- GeneralParameters$compute.ETP
	tmaxFl <- GeneralParameters$TMAX$dirORfile
	tmaxFf <- GeneralParameters$TMAX$format
	tmaxSl <- GeneralParameters$TMAX$sample
	tminFl <- GeneralParameters$TMIN$dirORfile
	tminFf <- GeneralParameters$TMIN$format
	tminSl <- GeneralParameters$TMIN$sample

	etpFl <- GeneralParameters$ETP$dirORfile
	etpFf <- GeneralParameters$ETP$format
	etpSl <- GeneralParameters$ETP$sample

	dekmonUse <- GeneralParameters$dekmon$use.dekmon
	dekmonTs <- GeneralParameters$dekmon$time.step
	dekmonFl <- GeneralParameters$dekmon$dirORfile
	dekmonSl <- GeneralParameters$dekmon$sample
	dekmonFf <- GeneralParameters$dekmon$format

	data.type <- GeneralParameters$data.type
	outputDIR <- GeneralParameters$Outdir

	##################

	if(rainFl == "" | rainFl == "NA" | is.na(rainFl) | (data.type == 'netcdf' & rainSl == "")){
		InsertMessagesTxt(main.txt.out, "No daily rainfall data found", format = TRUE)
		return(NULL)
	}

	if(compute.ETP == "temp"){
		if(tmaxFl == "" | tmaxFl == "NA" | is.na(tmaxFl) | (data.type == 'netcdf' & tmaxSl == "")){
			InsertMessagesTxt(main.txt.out, "No daily tmax data found", format = TRUE)
			return(NULL)
		}
		if(tminFl == "" | tminFl == "NA" | is.na(tminFl) | (data.type == 'netcdf' & tminSl == "")){
			InsertMessagesTxt(main.txt.out, "No daily tmin data found", format = TRUE)
			return(NULL)
		}
	}else{
		if(etpFl == "" | etpFl == "NA" | is.na(etpFl) | (data.type == 'netcdf' & etpSl == "")){
			InsertMessagesTxt(main.txt.out, "No daily potential evapotranspiration data found", format = TRUE)
			return(NULL)
		}
	}

	if(dekmonUse & (dekmonFl == "" | dekmonFl == "NA" | is.na(dekmonFl) | (data.type == 'netcdf' & dekmonSl == ""))){
		InsertMessagesTxt(main.txt.out, "No dekadal or monthly rainfall data found for seasonal amounts", format = TRUE)
		return(NULL)
	}

	if(outputDIR == "" | outputDIR == "NA" | is.na(outputDIR)){
		InsertMessagesTxt(main.txt.out, "No directory to save outputs", format = TRUE)
		return(NULL)
	}

	##################

	if(data.type == 'cdt'){
		rainInfo <- getStnOpenDataInfo(rainFl)
		if(!is.null(EnvPICSA$cdtPrecip)){
			if(!isTRUE(all.equal(EnvPICSA$rainInfo, rainInfo))){
				readRaindata <- TRUE
				EnvPICSA$cdtPrecip <- NULL
			}else readRaindata <- FALSE
		}else readRaindata <- TRUE

		if(readRaindata){
			cdtPrecip <- getStnOpenData(rainFl)
			if(is.null(cdtPrecip)) return(NULL)
			cdtPrecip <- getCDTdataAndDisplayMsg(cdtPrecip, "daily")
			if(is.null(cdtPrecip)) return(NULL)
			cdtPrecip <- cdtPrecip[c('id', 'lon', 'lat', 'dates', 'data')]
			EnvPICSA$cdtPrecip <- cdtPrecip
			EnvPICSA$rainInfo <- rainInfo
		}else cdtPrecip <- EnvPICSA$cdtPrecip

		if(compute.ETP == "temp"){
			tmaxInfo <- getStnOpenDataInfo(tmaxFl)
			if(!is.null(EnvPICSA$cdtTmax)){
				if(!isTRUE(all.equal(EnvPICSA$tmaxInfo, tmaxInfo))){
					readTmaxdata <- TRUE
					EnvPICSA$cdtTmax <- NULL
				}else readTmaxdata <- FALSE
			}else readTmaxdata <- TRUE

			if(readTmaxdata){
				cdtTmax <- getStnOpenData(tmaxFl)
				if(is.null(cdtTmax)) return(NULL)
				cdtTmax <- getCDTdataAndDisplayMsg(cdtTmax, "daily")
				if(is.null(cdtTmax)) return(NULL)
				cdtTmax <- cdtTmax[c('id', 'lon', 'lat', 'dates', 'data')]
				EnvPICSA$cdtTmax <- cdtTmax
				EnvPICSA$tmaxInfo <- tmaxInfo
			}else cdtTmax <- EnvPICSA$cdtTmax

			######
			tminInfo <- getStnOpenDataInfo(tminFl)
			if(!is.null(EnvPICSA$cdtTmin)){
				if(!isTRUE(all.equal(EnvPICSA$tminInfo, tminInfo))){
					readTmindata <- TRUE
					EnvPICSA$cdtTmin <- NULL
				}else readTmindata <- FALSE
			}else readTmindata <- TRUE

			if(readTmindata){
				cdtTmin <- getStnOpenData(tminFl)
				if(is.null(cdtTmin)) return(NULL)
				cdtTmin <- getCDTdataAndDisplayMsg(cdtTmin, "daily")
				if(is.null(cdtTmin)) return(NULL)
				cdtTmin <- cdtTmin[c('id', 'lon', 'lat', 'dates', 'data')]
				EnvPICSA$cdtTmin <- cdtTmin
				EnvPICSA$tminInfo <- tminInfo
			}else cdtTmin <- EnvPICSA$cdtTmin

			tmpdates <- intersect(intersect(cdtPrecip$dates, cdtTmax$dates), cdtTmin$dates)
		}else{
			etpInfo <- getStnOpenDataInfo(etpFl)
			if(!is.null(EnvPICSA$cdtETP)){
				if(!isTRUE(all.equal(EnvPICSA$etpInfo, etpInfo))){
					readETPdata <- TRUE
					EnvPICSA$cdtETP <- NULL
				}else readETPdata <- FALSE
			}else readETPdata <- TRUE

			if(readETPdata){
				cdtETP <- getStnOpenData(etpFl)
				if(is.null(cdtETP)) return(NULL)
				cdtETP <- getCDTdataAndDisplayMsg(cdtETP, "daily")
				if(is.null(cdtETP)) return(NULL)
				cdtETP <- cdtETP[c('id', 'lon', 'lat', 'dates', 'data')]
				EnvPICSA$cdtETP <- cdtETP
				EnvPICSA$etpInfo <- etpInfo
			}else cdtETP <- EnvPICSA$cdtETP

			tmpdates <- intersect(cdtPrecip$dates, cdtETP$dates)
		}

		if(length(tmpdates) == 0){
			msg <- if(compute.ETP == "temp") "tmax and tmin" else " and PET"
			InsertMessagesTxt(main.txt.out, paste("Daily rain", msg, "dates did not overlap"), format = TRUE)
			return(NULL)
		}

		if(dekmonUse){
			dekmonInfo <- getStnOpenDataInfo(dekmonFl)
			if(!is.null(EnvPICSA$cdtPrecip1)){
				if(!isTRUE(all.equal(EnvPICSA$dekmonInfo, dekmonInfo))){
					readRaindata1 <- TRUE
					EnvPICSA$cdtPrecip1 <- NULL
				}else readRaindata1 <- FALSE
			}else readRaindata1 <- TRUE

			if(readRaindata1){
				cdtPrecip1 <- getStnOpenData(dekmonFl)
				if(is.null(cdtPrecip1)) return(NULL)
				cdtPrecip1 <- getCDTdataAndDisplayMsg(cdtPrecip1, dekmonTs)
				if(is.null(cdtPrecip1)) return(NULL)
				cdtPrecip1 <- cdtPrecip1[c('id', 'lon', 'lat', 'dates', 'data')]
				EnvPICSA$cdtPrecip1 <- cdtPrecip1
				EnvPICSA$dekmonInfo <- dekmonInfo
			}else cdtPrecip1 <- EnvPICSA$cdtPrecip1
		}

		# cdtPrecip$data <- cdtPrecip$data[cdtPrecip$dates%in%tmpdates, , drop = FALSE]
		# cdtPrecip$dates <- cdtPrecip$dates[cdtPrecip$dates%in%tmpdates]

		# if(compute.ETP == "temp"){
		# 	cdtTmax$data <- cdtTmax$data[cdtTmax$dates%in%tmpdates, , drop = FALSE]
		# 	cdtTmax$dates <- cdtTmax$dates[cdtTmax$dates%in%tmpdates]
		# 	cdtTmin$data <- cdtTmin$data[cdtTmin$dates%in%tmpdates, , drop = FALSE]
		# 	cdtTmin$dates <- cdtTmin$dates[cdtTmin$dates%in%tmpdates]
		# }else{
		# 	cdtETP$data <- cdtETP$data[cdtETP$dates%in%tmpdates, , drop = FALSE]
		# 	cdtETP$dates <- cdtETP$dates[cdtETP$dates%in%tmpdates]
		# }

		if(dekmonUse){
			# if(!any(substr(tmpdates, 1, 6)%in%substr(cdtPrecip1$dates, 1, 6))){
			if(!any(substr(cdtPrecip$dates, 1, 6)%in%substr(cdtPrecip1$dates, 1, 6))){
				InsertMessagesTxt(main.txt.out, "Daily and dekadal or monthly rain dates did not overlap", format = TRUE)
				return(NULL)
			}
			# cdtPrecip1$data <- cdtPrecip1$data[substr(cdtPrecip1$dates, 1, 6)%in%substr(tmpdates, 1, 6), , drop = FALSE]
			# cdtPrecip1$dates <- cdtPrecip1$dates[substr(cdtPrecip1$dates, 1, 6)%in%substr(tmpdates, 1, 6)]
		}
	}

	##################

	if(data.type == 'netcdf'){
		if(GeneralParameters$date.range$all.years){
			startYear <- 1900
			endYear <- 2020
		}else{
			if(is.na(GeneralParameters$date.range$start.year) | is.na(GeneralParameters$date.range$end.year)){
				InsertMessagesTxt(main.txt.out, "Invalid years", format = TRUE)
				return(NULL)
			}
			startYear <- GeneralParameters$date.range$start.year
			endYear <- GeneralParameters$date.range$end.year
		}

		dstart <- as.Date(paste0(startYear, "-1-1"))
		dend <- as.Date(paste0(endYear, "-12-31"))
		dates <- format(seq(dstart, dend, 'day'), '%Y%m%d')
		rainNC <- sprintf(rainFf, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 8))
		if(compute.ETP == "temp"){
			tmaxNC <- sprintf(tmaxFf, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 8))
			tminNC <- sprintf(tminFf, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 8))
		}else etpNC <- sprintf(etpFf, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 8))

		if(dekmonUse){
			if(dekmonTs == 'dekadal'){
				datesDM <- seq(dstart, dend, 'day')
				datesDM <- paste0(format(datesDM[which(as.numeric(format(datesDM, '%d')) <= 3)], '%Y%m'),
								as.numeric(format(datesDM[which(as.numeric(format(datesDM, '%d')) <= 3)], '%d')))
				dekmonNC <- sprintf(dekmonFf, substr(datesDM, 1, 4), substr(datesDM, 5, 6), substr(datesDM, 7, 7))
			}
			if(dekmonTs == 'monthly'){
				datesDM <- format(seq(dstart, dend, 'month'), '%Y%m')
				dekmonNC <- sprintf(dekmonFf, substr(datesDM, 1, 4), substr(datesDM, 5, 6))
			}
		}

		##################
		rainPATH <- file.path(rainFl, rainNC)
		rainExist <- unlist(lapply(rainPATH, file.exists))
		if(!any(rainExist)){
			InsertMessagesTxt(main.txt.out, "Unable to locate daily rainfall files", format = TRUE)
			return(NULL)
		}

		if(compute.ETP == "temp"){
			tmaxPATH <- file.path(tmaxFl, tmaxNC)
			tmaxExist <- unlist(lapply(tmaxPATH, file.exists))
			if(!any(tmaxExist)){
				InsertMessagesTxt(main.txt.out, "Unable to locate daily tmax files", format = TRUE)
				return(NULL)
			}
			tminPATH <- file.path(tminFl, tminNC)
			tminExist <- unlist(lapply(tminPATH, file.exists))
			if(!any(tminExist)){
				InsertMessagesTxt(main.txt.out, "Unable to locate daily tmin files", format = TRUE)
				return(NULL)
			}
		}else{
			etpPATH <- file.path(etpFl, etpNC)
			etpExist <- unlist(lapply(etpPATH, file.exists))
			if(!any(etpExist)){
				InsertMessagesTxt(main.txt.out, "Unable to locate daily PET files", format = TRUE)
				return(NULL)
			}
		}

		if(dekmonUse){
			dekmonPATH <- file.path(dekmonFl, dekmonNC)
			dekmonExist <- unlist(lapply(dekmonPATH, file.exists))
			if(!any(dekmonExist)){
				InsertMessagesTxt(main.txt.out, "Unable to locate dekadal or monthly rainfall files", format = TRUE)
				return(NULL)
			}
		}

		##################
		raindates <- dates[rainExist]
		rainPATH <- rainPATH[rainExist]

		if(compute.ETP == "temp"){
			tmaxdates <- dates[tmaxExist]
			tmaxPATH <- tmaxPATH[tmaxExist]
			tmindates <- dates[tminExist]
			tminPATH <- tminPATH[tminExist]
			tmpdates <- intersect(intersect(raindates, tmaxdates), tmindates)
		}else{
			etpdates <- dates[etpExist]
			etpPATH <- etpPATH[etpExist]
			tmpdates <- intersect(raindates, etpdates)
		}

		if(length(tmpdates) == 0){
			msg <- if(compute.ETP == "temp") "tmax and tmin" else " and PET"
			InsertMessagesTxt(main.txt.out, paste("Daily rain", msg, "dates did not overlap"), format = TRUE)
			return(NULL)
		}

		# rainPATH <- rainPATH[raindates%in%tmpdates]
		# raindates <- raindates[raindates%in%tmpdates]

		# if(compute.ETP == "temp"){
		# 	tmaxPATH <- tmaxPATH[tmaxdates%in%tmpdates]
		# 	tmaxdates <- tmaxdates[tmaxdates%in%tmpdates]
		# 	tminPATH <- tminPATH[tmindates%in%tmpdates]
		# 	tmindates <- tmindates[tmindates%in%tmpdates]
		# }else{
		# 	etpPATH <- etpPATH[etpdates%in%tmpdates]
		# 	etpdates <- etpdates[etpdates%in%tmpdates]
		# }

		if(dekmonUse){
			dekmondates <- datesDM[dekmonExist]
			dekmonPATH <- dekmonPATH[dekmonExist]
			# if(!any(substr(tmpdates, 1, 6)%in%substr(dekmondates, 1, 6))){
			if(!any(substr(raindates, 1, 6)%in%substr(dekmondates, 1, 6))){
				InsertMessagesTxt(main.txt.out, "Daily rain and dekadal or monthly dates did not overlap", format = TRUE)
				return(NULL)
			}
			# dekmonPATH <- dekmonPATH[substr(dekmondates, 1, 6)%in%substr(tmpdates, 1, 6)]
			# dekmondates <- dekmondates[substr(dekmondates, 1, 6)%in%substr(tmpdates, 1, 6)]
		}

		##################
		rainNcInfo <- getRFESampleData(rainSl)
		if(is.null(rainNcInfo)){
			InsertMessagesTxt(main.txt.out, "No daily rainfall sample file found", format = TRUE)
			return(NULL)
		}

		if(compute.ETP == "temp"){
			tmaxNcInfo <- getRFESampleData(tmaxSl)
			if(is.null(tmaxNcInfo)){
				InsertMessagesTxt(main.txt.out, "No daily tmax sample file found", format = TRUE)
				return(NULL)
			}

			tminNcInfo <- getRFESampleData(tminSl)
			if(is.null(tminNcInfo)){
				InsertMessagesTxt(main.txt.out, "No daily tmin sample file found", format = TRUE)
				return(NULL)
			}
		}else{
			etpNcInfo <- getRFESampleData(etpSl)
			if(is.null(etpNcInfo)){
				InsertMessagesTxt(main.txt.out, "No daily PET sample file found", format = TRUE)
				return(NULL)
			}
		}

		if(dekmonUse){
			dekmonNcInfo <- getRFESampleData(dekmonSl)
			if(is.null(dekmonNcInfo)){
				InsertMessagesTxt(main.txt.out, "No dekadal or monthly rainfall sample file found", format = TRUE)
				return(NULL)
			}
		}

		#####################################
		nc <- nc_open(rainPATH[1])
		rainlon <- nc$dim[[rainNcInfo$rfeILon]]$vals
		rainlat <- nc$dim[[rainNcInfo$rfeILat]]$vals
		nc_close(nc)

		rainInfo <- c(rainFl, rainFf)
		if(!is.null(EnvPICSA$cdtPrecip)){
			iexist <- raindates%in%EnvPICSA$cdtPrecip$dates
			if(all(iexist)){
				if(!isTRUE(all.equal(EnvPICSA$rainInfo, rainInfo))){
					readRaindata <- TRUE
					EnvPICSA$cdtPrecip <- NULL
				}else readRaindata <- FALSE
			}else{
				if(any(iexist) & isTRUE(all.equal(EnvPICSA$rainInfo, rainInfo))){
					raindates <- raindates[!iexist]
					rainPATH <- rainPATH[!iexist]
				}else EnvPICSA$cdtPrecip <- NULL
				readRaindata <- TRUE
			}
		}else readRaindata <- TRUE

		if(readRaindata){
			InsertMessagesTxt(main.txt.out, 'Read rainfall data ...')
			if(doparallel & length(rainPATH) >= 180){
				klust <- makeCluster(nb_cores)
				registerDoParallel(klust)
				`%parLoop%` <- `%dopar%`
				closeklust <- TRUE
			}else{
				`%parLoop%` <- `%do%`
				closeklust <- FALSE
			}

			xo <- order(rainlon)
			rainlon <- rainlon[xo]
			yo <- order(rainlat)
			rainlat <- rainlat[yo]

			ncData <- foreach(jj = seq_along(rainPATH), .packages = "ncdf4",
							.export = c("rainPATH", "rainNcInfo")) %parLoop% {
				nc <- nc_open(rainPATH[jj])
				vars <- ncvar_get(nc, varid = rainNcInfo$rfeVarid)
				nc_close(nc)
				vars <- vars[xo, yo]
				if(rainNcInfo$rfeILat < rainNcInfo$rfeILon){
					vars <- matrix(c(vars), nrow = length(rainlon), ncol = length(rainlat), byrow = TRUE)
				}
				vars
			}
			if(closeklust) stopCluster(klust)
			InsertMessagesTxt(main.txt.out, 'Reading rainfall data finished')

			xycrd <- expand.grid(rainlon, rainlat)
			cdtPrecip <- list(
				lon = xycrd[, 1],
				lat = xycrd[, 2],
				dates = raindates,
				data = t(sapply(ncData, c))
			)
			rm(ncData)
			cdtPrecip$dates <- c(EnvPICSA$cdtPrecip$dates, cdtPrecip$dates)
			cdtPrecip$data <- rbind(EnvPICSA$cdtPrecip$data, cdtPrecip$data)
			odaty <- order(cdtPrecip$dates)
			cdtPrecip$dates <- cdtPrecip$dates[odaty]
			cdtPrecip$data <- cdtPrecip$data[odaty, , drop = FALSE]
			EnvPICSA$cdtPrecip <- cdtPrecip
			EnvPICSA$rainInfo <- rainInfo
		}else cdtPrecip <- EnvPICSA$cdtPrecip

		#####################################

		if(compute.ETP == "temp"){
			nc <- nc_open(tmaxPATH[1])
			tmaxlon <- nc$dim[[tmaxNcInfo$rfeILon]]$vals
			tmaxlat <- nc$dim[[tmaxNcInfo$rfeILat]]$vals
			nc_close(nc)

			tmaxInfo <- c(tmaxFl, tmaxFf)
			if(!is.null(EnvPICSA$cdtTmax)){
				iexist <- tmaxdates%in%EnvPICSA$cdtTmax$dates
				if(all(iexist)){
					if(!isTRUE(all.equal(EnvPICSA$tmaxInfo, tmaxInfo))){
						readTmaxdata <- TRUE
						EnvPICSA$cdtTmax <- NULL
					}else readTmaxdata <- FALSE
				}else{
					if(any(iexist) & isTRUE(all.equal(EnvPICSA$tmaxInfo, tmaxInfo))){
						tmaxdates <- tmaxdates[!iexist]
						tmaxPATH <- tmaxPATH[!iexist]
					}else EnvPICSA$cdtTmax <- NULL
					readTmaxdata <- TRUE
				}
			}else readTmaxdata <- TRUE

			if(readTmaxdata){
				InsertMessagesTxt(main.txt.out, 'Read tmax data ...')
				if(doparallel & length(tmaxPATH) >= 180){
					klust <- makeCluster(nb_cores)
					registerDoParallel(klust)
					`%parLoop%` <- `%dopar%`
					closeklust <- TRUE
				}else{
					`%parLoop%` <- `%do%`
					closeklust <- FALSE
				}

				xo <- order(tmaxlon)
				tmaxlon <- tmaxlon[xo]
				yo <- order(tmaxlat)
				tmaxlat <- tmaxlat[yo]

				ncData <- foreach(jj = seq_along(tmaxPATH), .packages = "ncdf4",
								.export = c("tmaxPATH", "tmaxNcInfo")) %parLoop% {
					nc <- nc_open(tmaxPATH[jj])
					vars <- ncvar_get(nc, varid = tmaxNcInfo$rfeVarid)
					nc_close(nc)
					vars <- vars[xo, yo]
					if(tmaxNcInfo$rfeILat < tmaxNcInfo$rfeILon){
						vars <- matrix(c(vars), nrow = length(tmaxlon), ncol = length(tmaxlat), byrow = TRUE)
					}
					vars
				}
				if(closeklust) stopCluster(klust)
				InsertMessagesTxt(main.txt.out, 'Reading tmax data finished')

				xycrd <- expand.grid(tmaxlon, tmaxlat)
				cdtTmax <- list(
					lon = xycrd[, 1],
					lat = xycrd[, 2],
					dates = tmaxdates,
					data = t(sapply(ncData, c))
				)
				rm(ncData)
				cdtTmax$dates <- c(EnvPICSA$cdtTmax$dates, cdtTmax$dates)
				cdtTmax$data <- rbind(EnvPICSA$cdtTmax$data, cdtTmax$data)
				odaty <- order(cdtTmax$dates)
				cdtTmax$dates <- cdtTmax$dates[odaty]
				cdtTmax$data <- cdtTmax$data[odaty, , drop = FALSE]
				EnvPICSA$cdtTmax <- cdtTmax
				EnvPICSA$tmaxInfo <- tmaxInfo
			}else cdtTmax <- EnvPICSA$cdtTmax

			############

			nc <- nc_open(tminPATH[1])
			tminlon <- nc$dim[[tminNcInfo$rfeILon]]$vals
			tminlat <- nc$dim[[tminNcInfo$rfeILat]]$vals
			nc_close(nc)

			tminInfo <- c(tminFl, tminFf)
			if(!is.null(EnvPICSA$cdtTmin)){
				iexist <- tmindates%in%EnvPICSA$cdtTmin$dates
				if(all(iexist)){
					if(!isTRUE(all.equal(EnvPICSA$tminInfo, tminInfo))){
						readTmindata <- TRUE
						EnvPICSA$cdtTmin <- NULL
					}else readTmindata <- FALSE
				}else{
					if(any(iexist) & isTRUE(all.equal(EnvPICSA$tminInfo, tminInfo))){
						tmindates <- tmindates[!iexist]
						tminPATH <- tminPATH[!iexist]
					}else EnvPICSA$cdtTmin <- NULL
					readTmindata <- TRUE
				}
			}else readTmindata <- TRUE

			if(readTmindata){
				InsertMessagesTxt(main.txt.out, 'Read tmin data ...')
				if(doparallel & length(tminPATH) >= 180){
					klust <- makeCluster(nb_cores)
					registerDoParallel(klust)
					`%parLoop%` <- `%dopar%`
					closeklust <- TRUE
				}else{
					`%parLoop%` <- `%do%`
					closeklust <- FALSE
				}

				xo <- order(tminlon)
				tminlon <- tminlon[xo]
				yo <- order(tminlat)
				tminlat <- tminlat[yo]

				ncData <- foreach(jj = seq_along(tminPATH), .packages = "ncdf4",
								.export = c("tminPATH", "tminNcInfo")) %parLoop% {
					nc <- nc_open(tminPATH[jj])
					vars <- ncvar_get(nc, varid = tminNcInfo$rfeVarid)
					nc_close(nc)
					vars <- vars[xo, yo]
					if(tminNcInfo$rfeILat < tminNcInfo$rfeILon){
						vars <- matrix(c(vars), nrow = length(tminlon), ncol = length(tminlat), byrow = TRUE)
					}
					vars
				}
				if(closeklust) stopCluster(klust)
				InsertMessagesTxt(main.txt.out, 'Reading tmin data finished')

				xycrd <- expand.grid(tminlon, tminlat)
				cdtTmin <- list(
					lon = xycrd[, 1],
					lat = xycrd[, 2],
					dates = tmindates,
					data = t(sapply(ncData, c))
				)
				rm(ncData)
				cdtTmin$dates <- c(EnvPICSA$cdtTmin$dates, cdtTmin$dates)
				cdtTmin$data <- rbind(EnvPICSA$cdtTmin$data, cdtTmin$data)
				odaty <- order(cdtTmin$dates)
				cdtTmin$dates <- cdtTmin$dates[odaty]
				cdtTmin$data <- cdtTmin$data[odaty, , drop = FALSE]
				EnvPICSA$cdtTmin <- cdtTmin
				EnvPICSA$tminInfo <- tminInfo
			}else cdtTmin <- EnvPICSA$cdtTmin

		}else{

			nc <- nc_open(etpPATH[1])
			etplon <- nc$dim[[etpNcInfo$rfeILon]]$vals
			etplat <- nc$dim[[etpNcInfo$rfeILat]]$vals
			nc_close(nc)

			etpInfo <- c(etpFl, etpFf)
			if(!is.null(EnvPICSA$cdtETP)){
				iexist <- etpdates%in%EnvPICSA$cdtETP$dates
				if(all(iexist)){
					if(!isTRUE(all.equal(EnvPICSA$etpInfo, etpInfo))){
						readETPdata <- TRUE
						EnvPICSA$cdtETP <- NULL
					}else readETPdata <- FALSE
				}else{
					if(any(iexist) & isTRUE(all.equal(EnvPICSA$etpInfo, etpInfo))){
						etpdates <- etpdates[!iexist]
						etpPATH <- etpPATH[!iexist]
					}else EnvPICSA$cdtETP <- NULL
					readETPdata <- TRUE
				}
			}else readETPdata <- TRUE

			if(readETPdata){
				InsertMessagesTxt(main.txt.out, 'Read PET data ...')
				if(doparallel & length(etpPATH) >= 180){
					klust <- makeCluster(nb_cores)
					registerDoParallel(klust)
					`%parLoop%` <- `%dopar%`
					closeklust <- TRUE
				}else{
					`%parLoop%` <- `%do%`
					closeklust <- FALSE
				}

				xo <- order(etplon)
				etplon <- etplon[xo]
				yo <- order(etplat)
				etplat <- etplat[yo]

				ncData <- foreach(jj = seq_along(etpPATH), .packages = "ncdf4",
								.export = c("etpPATH", "etpNcInfo")) %parLoop% {
					nc <- nc_open(etpPATH[jj])
					vars <- ncvar_get(nc, varid = etpNcInfo$rfeVarid)
					nc_close(nc)
					vars <- vars[xo, yo]
					if(etpNcInfo$rfeILat < etpNcInfo$rfeILon){
						vars <- matrix(c(vars), nrow = length(etplon), ncol = length(etplat), byrow = TRUE)
					}
					vars
				}
				if(closeklust) stopCluster(klust)
				InsertMessagesTxt(main.txt.out, 'Reading PET data finished')

				xycrd <- expand.grid(etplon, etplat)
				cdtETP <- list(
					lon = xycrd[, 1],
					lat = xycrd[, 2],
					dates = etpdates,
					data = t(sapply(ncData, c))
				)
				rm(ncData)
				cdtETP$dates <- c(EnvPICSA$cdtETP$dates, cdtETP$dates)
				cdtETP$data <- rbind(EnvPICSA$cdtETP$data, cdtETP$data)
				odaty <- order(cdtETP$dates)
				cdtETP$dates <- cdtETP$dates[odaty]
				cdtETP$data <- cdtETP$data[odaty, , drop = FALSE]
				EnvPICSA$cdtETP <- cdtETP
				EnvPICSA$etpInfo <- etpInfo
			}else cdtETP <- EnvPICSA$cdtETP
		}

		#####################################

		if(dekmonUse){
			nc <- nc_open(dekmonPATH[1])
			dekmonlon <- nc$dim[[dekmonNcInfo$rfeILon]]$vals
			dekmonlat <- nc$dim[[dekmonNcInfo$rfeILat]]$vals
			nc_close(nc)

			dekmonInfo <- c(dekmonFl, dekmonFf)
			if(!is.null(EnvPICSA$cdtPrecip1)){
				iexist <- dekmondates%in%EnvPICSA$cdtPrecip1$dates
				if(all(iexist)){
					if(!isTRUE(all.equal(EnvPICSA$dekmonInfo, dekmonInfo))){
						readRaindata1 <- TRUE
						EnvPICSA$cdtPrecip1 <- NULL
					}else readRaindata1 <- FALSE
				}else{
					if(any(iexist) & isTRUE(all.equal(EnvPICSA$dekmonInfo, dekmonInfo))){
						dekmondates <- dekmondates[!iexist]
						dekmonPATH <- dekmonPATH[!iexist]
					}else EnvPICSA$cdtPrecip1 <- NULL
					readRaindata1 <- TRUE
				}
			}else readRaindata1 <- TRUE

			if(readRaindata1){
				InsertMessagesTxt(main.txt.out, 'Read rainfall data for seasonal amounts ...')
				if(doparallel & length(dekmonPATH) >= 180){
					klust <- makeCluster(nb_cores)
					registerDoParallel(klust)
					`%parLoop%` <- `%dopar%`
					closeklust <- TRUE
				}else{
					`%parLoop%` <- `%do%`
					closeklust <- FALSE
				}

				xo <- order(dekmonlon)
				dekmonlon <- dekmonlon[xo]
				yo <- order(dekmonlat)
				dekmonlat <- dekmonlat[yo]

				ncData <- foreach(jj = seq_along(dekmonPATH), .packages = "ncdf4",
								.export = c("dekmonPATH", "dekmonNcInfo")) %parLoop% {
					nc <- nc_open(dekmonPATH[jj])
					vars <- ncvar_get(nc, varid = dekmonNcInfo$rfeVarid)
					nc_close(nc)
					vars <- vars[xo, yo]
					if(dekmonNcInfo$rfeILat < dekmonNcInfo$rfeILon){
						vars <- matrix(c(vars), nrow = length(dekmonlon), ncol = length(dekmonlat), byrow = TRUE)
					}
					vars
				}
				if(closeklust) stopCluster(klust)
				InsertMessagesTxt(main.txt.out, 'Reading rainfall data for seasonal amount finished')

				xycrd <- expand.grid(dekmonlon, dekmonlat)
				cdtPrecip1 <- list(
					lon = xycrd[, 1],
					lat = xycrd[, 2],
					dates = dekmondates,
					data = t(sapply(ncData, c))
				)
				rm(ncData)
				cdtPrecip1$dates <- c(EnvPICSA$cdtPrecip1$dates, cdtPrecip1$dates)
				cdtPrecip1$data <- rbind(EnvPICSA$cdtPrecip1$data, cdtPrecip1$data)
				odaty <- order(cdtPrecip1$dates)
				cdtPrecip1$dates <- cdtPrecip1$dates[odaty]
				cdtPrecip1$data <- cdtPrecip1$data[odaty, , drop = FALSE]
				EnvPICSA$cdtPrecip1 <- cdtPrecip1
				EnvPICSA$dekmonInfo <- dekmonInfo
			}else cdtPrecip1 <- EnvPICSA$cdtPrecip1
		}		 
	}

	##################

	if(compute.ETP == "temp"){
		cdtTmax$data <- cdtTmax$data[match(cdtPrecip$dates, cdtTmax$dates), , drop = FALSE]
		cdtTmax$dates <- cdtPrecip$dates
		cdtTmin$data <- cdtTmin$data[match(cdtPrecip$dates, cdtTmin$dates), , drop = FALSE]
		cdtTmin$dates <- cdtPrecip$dates
	}else{
		cdtETP$data <- cdtETP$data[match(cdtPrecip$dates, cdtETP$dates), , drop = FALSE]
		cdtETP$dates <- cdtPrecip$dates
	}

	if(dekmonUse){
		cdtPrecip1$data <- cdtPrecip1$data[substr(cdtPrecip1$dates, 1, 6)%in%substr(cdtPrecip$dates, 1, 6), , drop = FALSE]
		cdtPrecip1$dates <- cdtPrecip1$dates[substr(cdtPrecip1$dates, 1, 6)%in%substr(cdtPrecip$dates, 1, 6)]
	}

	##################

	if(data.type == 'cdt'){
		if(compute.ETP == "temp") idens <- intersect(cdtPrecip$id, intersect(cdtTmax$id, cdtTmin$id))
		if(compute.ETP == "etp") idens <- intersect(cdtPrecip$id, cdtETP$id)
		if(length(idens) == 0){
			msg <- if(compute.ETP == "temp") "tmax and tmin" else " and PET"
			InsertMessagesTxt(main.txt.out, paste("No match between daily rain", msg, "ID"), format = TRUE)
			return(NULL)
		}

		# idens <- cdtPrecip$id
		cdtPrecip$lon <- cdtPrecip$lon[match(idens, cdtPrecip$id)]
		cdtPrecip$lat <- cdtPrecip$lat[match(idens, cdtPrecip$id)]
		cdtPrecip$data <- cdtPrecip$data[, match(idens, cdtPrecip$id), drop = FALSE]
		cdtPrecip$id <- cdtPrecip$id[match(idens, cdtPrecip$id)]

		if(compute.ETP == "temp"){
			cdtTmax$lon <- cdtTmax$lon[match(idens, cdtTmax$id)]
			cdtTmax$lat <- cdtTmax$lat[match(idens, cdtTmax$id)]
			cdtTmax$data <- cdtTmax$data[, match(idens, cdtTmax$id), drop = FALSE]
			cdtTmax$id <- cdtTmax$id[match(idens, cdtTmax$id)]

			cdtTmin$lon <- cdtTmin$lon[match(idens, cdtTmin$id)]
			cdtTmin$lat <- cdtTmin$lat[match(idens, cdtTmin$id)]
			cdtTmin$data <- cdtTmin$data[, match(idens, cdtTmin$id), drop = FALSE]
			cdtTmin$id <- cdtTmin$id[match(idens, cdtTmin$id)]
		}else{
			cdtETP$lon <- cdtETP$lon[match(idens, cdtETP$id)]
			cdtETP$lat <- cdtETP$lat[match(idens, cdtETP$id)]
			cdtETP$data <- cdtETP$data[, match(idens, cdtETP$id), drop = FALSE]
			cdtETP$id <- cdtETP$id[match(idens, cdtETP$id)]
		}

		if(dekmonUse){
			if(length(intersect(idens, cdtPrecip1$id)) == 0){
				InsertMessagesTxt(main.txt.out, "No match between daily and dekadal rain ID", format = TRUE)
				return(NULL)
			}
			cdtPrecip1$lon <- cdtPrecip1$lon[match(idens, cdtPrecip1$id)]
			cdtPrecip1$lat <- cdtPrecip1$lat[match(idens, cdtPrecip1$id)]
			cdtPrecip1$data <- cdtPrecip1$data[, match(idens, cdtPrecip1$id), drop = FALSE]
			cdtPrecip1$id <- cdtPrecip1$id[match(idens, cdtPrecip1$id)]
		}
	}

	if(data.type == 'netcdf'){
		xyPrecip <- data.frame(x = cdtPrecip$lon, y = cdtPrecip$lat)
		coordinates(xyPrecip) <- ~x+y
		xyPrecip <- SpatialPixels(points = xyPrecip, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))

		if(compute.ETP == "temp"){
			xyTmax <- data.frame(x = cdtTmax$lon, y = cdtTmax$lat)
			coordinates(xyTmax) <- ~x+y
			xyTmax <- SpatialPixels(points = xyTmax, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))
			ijmax <- over(xyPrecip, xyTmax)
			cdtTmax$lon <- cdtPrecip$lon
			cdtTmax$lat <- cdtPrecip$lat
			cdtTmax$data <- cdtTmax$data[, ijmax, drop = FALSE]

			xyTmin <- data.frame(x = cdtTmin$lon, y = cdtTmin$lat)
			coordinates(xyTmin) <- ~x+y
			xyTmin <- SpatialPixels(points = xyTmin, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))
			ijmin <- over(xyPrecip, xyTmin)
			cdtTmin$lon <- cdtPrecip$lon
			cdtTmin$lat <- cdtPrecip$lat
			cdtTmin$data <- cdtTmin$data[, ijmin, drop = FALSE]
		}else{
			xyETP <- data.frame(x = cdtETP$lon, y = cdtETP$lat)
			coordinates(xyETP) <- ~x+y
			xyETP <- SpatialPixels(points = xyETP, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))
			ijetp <- over(xyPrecip, xyETP)
			cdtETP$lon <- cdtPrecip$lon
			cdtETP$lat <- cdtPrecip$lat
			cdtETP$data <- cdtETP$data[, ijetp, drop = FALSE]
		}

		if(dekmonUse){
			xyPrecip1 <- data.frame(x = cdtPrecip1$lon, y = cdtPrecip1$lat)
			coordinates(xyPrecip1) <- ~x+y
			xyPrecip1 <- SpatialPixels(points = xyPrecip1, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))
			ijrr1 <- over(xyPrecip, xyPrecip1)
			cdtPrecip1$lon <- cdtPrecip$lon
			cdtPrecip1$lat <- cdtPrecip$lat
			cdtPrecip1$data <- cdtPrecip1$data[, ijrr1, drop = FALSE]
		}
	}

	##################

	if(compute.ETP == "temp"){
		latRa <- unique(cdtTmax$lat)
		Ra.Annual <- ExtraterrestrialRadiation(latRa)
		mmdd.anual <- format(seq(as.Date("2000-1-1"), as.Date("2000-12-31"), 'day'), "%m%d")
		cdt.dates <- substr(cdtTmax$dates, 5, 8)
		ilat <- match(cdtTmax$lat, latRa)
		immdd <- match(cdt.dates, mmdd.anual)
		RA <- Ra.Annual[immdd, ilat, drop = FALSE]

		cdtETP <- NULL
		cdtETP$data <- ETPHargreaves(cdtTmax$data, cdtTmin$data, RA, cdtPrecip$data)
	}

	# ##################
	# ###REMOVE
	# if(compute.ETP == "temp"){
	# 	outDATA <- list(rr = cdtPrecip, tx = cdtTmax, tn = cdtTmin)
	# }else{
	# 	outDATA <- list(rr = cdtPrecip, etp = cdtETP)
	# }
	# if(dekmonUse) outDATA <- c(outDATA, list(rrdk = cdtPrecip1, etp = cdtETP))

	# # outDATA <- c(outDATA, list(onset = onset, cessat = cessat, onset.1970 = onset.1970,
	# # 			onset.julian = onset.julian, cessat.1970 = cessat.1970, cessat.julian = cessat.julian))

	# assign("outDATA", outDATA, envir = .GlobalEnv)


	##################
	InsertMessagesTxt(main.txt.out, 'Calculate onset and cessation of seasonal rainfall ...')

	if(doparallel & ncol(cdtPrecip$data) >= 200){
		klust <- makeCluster(nb_cores)
		registerDoParallel(klust)
		`%parLoop%` <- `%dopar%`
		closeklust <- TRUE
	}else{
		`%parLoop%` <- `%do%`
		closeklust <- FALSE
	}

	toExport <- c("cessationDectection", "onsetDectection", "cdtPrecip", "cdtETP")

	##################

	onset.start.month <- GeneralParameters$onset$early.month
	onset.start.day <- GeneralParameters$onset$early.day
	onset.end.month <- GeneralParameters$onset$late.month
	onset.end.day <- GeneralParameters$onset$late.day

	yearO <- if(onset.end.month <=  onset.start.month) 2015 else 2014
	win2.search <- as.numeric(as.Date(paste(yearO, onset.end.month, onset.end.day, sep = '-'))
						- as.Date(paste(2014, onset.start.month, onset.start.day, sep = '-')))

	onset.pars <- list(rain.total = GeneralParameters$onset$rain.total,
						win0.search = GeneralParameters$onset$first.win.search,
						min.rain.day = GeneralParameters$onset$min.rain.day,
						win1.search = GeneralParameters$onset$dry.spell.win.search,
						dry.spell = GeneralParameters$onset$dry.spell,
						thres.rain.day = GeneralParameters$onset$thres.rain.day, 
						win2.search = win2.search)

	cess.start.month <- GeneralParameters$cessation$early.month
	cess.start.day <- GeneralParameters$cessation$early.day
	cess.end.month <- GeneralParameters$cessation$late.month
	cess.end.day <- GeneralParameters$cessation$late.day
	yearC <- if(cess.end.month <=  cess.start.month) 2015 else 2014
	cess.win.search <- as.numeric(as.Date(paste(yearC, cess.end.month, cess.end.day, sep = '-'))
						- as.Date(paste(2014, cess.start.month, cess.start.day, sep = '-')))

	WB.lag <- 30
	cess.pars <- list(swh.capacity = GeneralParameters$cessation$swh.capacity,
					  min.WB = GeneralParameters$cessation$min.water.balance,
					  length.day = GeneralParameters$cessation$first.win.search,
					  win.search = cess.win.search,
					  WB.lag = WB.lag)

	cess.idx <- getIndexCalcOnsetCess(cdtPrecip$dates, cess.start.month, cess.start.day, WB.lag)
	onset.idx <- getIndexCalcOnsetCess(cdtPrecip$dates, onset.start.month, onset.start.day)

	##################

	onset <- foreach(jj = seq_along(onset.idx), .packages = "matrixStats",
					 .export = toExport) %parLoop% {
		onsetDectection(onset.idx[[jj]], DATA = cdtPrecip$data,
						dates = cdtPrecip$dates, pars = onset.pars, min.frac = 0.99)
	}
	onset <- do.call(rbind, onset)

	cessat <- foreach(jj = seq_along(cess.idx), .packages = "matrixStats",
					 .export = toExport) %parLoop% {
		cessationDectection(cess.idx[[jj]], Precip = cdtPrecip$data, ETP = cdtETP$data,
						dates = cdtPrecip$dates, pars = cess.pars, min.frac = 0.99)
	}
	cessat <- do.call(rbind, cessat)

	if(closeklust) stopCluster(klust)
	InsertMessagesTxt(main.txt.out, 'Calculating onset and cessation of seasonal rainfall finished')

	##################

	onsetXS <- paste(2014, onset.start.month, onset.start.day, sep = '-')
	cessatXS <- as.character(as.Date(paste(2014, cess.start.month, cess.start.day, sep = '-'))-WB.lag)
	yearS <- sapply(onset.idx, function(x) cdtPrecip$dates[x[1]])
	yearE <- sapply(cess.idx, function(x) cdtPrecip$dates[x[1]])

	if(onset.start.month > cess.start.month){
		cessat <- rbind(cessat, NA)
		yearE <- c(yearE, NA)
		onset <- rbind(NA, onset)
		yearS <- c(NA, yearS)
	}

	matCessat <- as.Date(cessat, "%Y%m%d")
	dim(matCessat) <- dim(cessat)
	matCessat <- matCessat - as.Date(yearE, "%Y%m%d")

	matOnset <- as.Date(onset, "%Y%m%d")
	dim(matOnset) <- dim(onset)
	matOnset <- matOnset - as.Date(yearS, "%Y%m%d")

	season.length <- as.Date(cessat, "%Y%m%d")-as.Date(onset, "%Y%m%d")
	dim(season.length) <- dim(onset)

	yrsCesst <- as.numeric(substr(yearE, 1, 4))
	yrsOnset <- as.numeric(substr(yearS, 1, 4))

	##################

	load(file.path(apps.dir, 'data', 'ONI_50-2016.RData'))
	ONI.date <- format(seq(as.Date('1950-1-15'), as.Date('2017-12-15'), "month"), "%Y%m")
	ONI.data <- ONI$ts[, 3]

	##################

	if(data.type == 'cdt'){
		InsertMessagesTxt(main.txt.out, 'Plot stations data ...')

		if(doparallel & ncol(onset) >= 10){
			klust <- makeCluster(nb_cores)
			registerDoParallel(klust)
			`%parLoop%` <- `%dopar%`
			closeklust <- TRUE
		}else{
			`%parLoop%` <- `%do%`
			closeklust <- FALSE
		}

		dekmonUse <- GeneralParameters$dekmon$use.dekmon
		dekmonTs <- GeneralParameters$dekmon$time.step
		thres.rain.day <- GeneralParameters$onset$thres.rain.day
		dryspellfun <- if(thres.rain.day == 0) '==' else '<'
		nbdayfun <- if(thres.rain.day == 0) '>' else '>='
		compute.ETP <- GeneralParameters$compute.ETP

		toExport1 <- c("getIndexSeasonVars", "funAggrMAT", "NumberOfSpell.All", "MaximumLengthOfSpell", "onset", "cessat", "cdtPrecip")
		if(dekmonUse) toExport1 <- c(toExport1, "cdtPrecip1")
		if(compute.ETP == "temp") toExport1 <- c(toExport1, "cdtTmax", "cdtTmin")

		output.cdt <- foreach(j = seq(ncol(onset)), .packages = "matrixStats", .export = toExport1) %parLoop% {
			ijday <- getIndexSeasonVars(onset[, j], cessat[, j], cdtPrecip$dates, "daily")
			## seasonal amounts
			if(dekmonUse){
				ijseas <- getIndexSeasonVars(onset[, j], cessat[, j], cdtPrecip1$dates, dekmonTs)
				RainTotal <- sapply(ijseas, funAggrMAT, DATA = cdtPrecip1$data[, j, drop = FALSE], pars = list(aggr.fun = "sum"))
			}else{
				RainTotal <- sapply(ijday, funAggrMAT, DATA = cdtPrecip$data[, j, drop = FALSE], pars = list(aggr.fun = "sum"))
			}

			max24h <- sapply(ijday, funAggrMAT, DATA = cdtPrecip$data[, j, drop = FALSE], pars = list(aggr.fun = "max"))
			nbdayrain <- sapply(ijday, funAggrMAT, DATA = cdtPrecip$data[, j, drop = FALSE], pars = list(aggr.fun = "count", count.fun = nbdayfun, count.thres = thres.rain.day))

			seasRR <- cdtPrecip$data[unlist(ijday), j]
			seasRR <- seasRR[seasRR > thres.rain.day]
			q95th <- quantile(seasRR, probs = 0.95, type = 8, na.rm = TRUE)

			nbQ95th <- sapply(ijday, funAggrMAT, DATA = cdtPrecip$data[, j, drop = FALSE], pars = list(aggr.fun = "count", count.fun = ">=", count.thres = q95th))

			## dry spell 5, 7, 10, 15
			dryspell <- lapply(ijday, NumberOfSpell.All, DATA = cdtPrecip$data[, j, drop = FALSE], opr.fun = dryspellfun, opr.thres = thres.rain.day, ccday = c(5, 7, 10, 15))
			dryspell <- t(do.call(cbind, dryspell))
			## longest dry spell
			maxdryspell <- sapply(ijday, MaximumLengthOfSpell, DATA = cdtPrecip$data[, j, drop = FALSE], opr.fun = dryspellfun, opr.thres = thres.rain.day)
			ret <- data.frame(RainTotal, dryspell, maxdryspell, max24h, nbdayrain, nbQ95th)
			names(ret) <- c('RainTotal', 'DrySpell5', 'DrySpell7', 'DrySpell10', 'DrySpell15', "DrySpellMax", "MaxDailyRain", "NbRainDay", "ExtremeFrequency95Per")

			##
			if(compute.ETP == "temp"){
				tmax <- sapply(ijday, funAggrMAT, DATA = cdtTmax$data[, j, drop = FALSE], pars = list(aggr.fun = "mean"))
				tmin <- sapply(ijday, funAggrMAT, DATA = cdtTmin$data[, j, drop = FALSE], pars = list(aggr.fun = "mean"))
				ret <- data.frame(ret, tmax =  round(tmax, 1), tmin = round(tmin, 1))
			}
			return(ret)
		}

		###
		outPICSAdir <- file.path(outputDIR, "PICSA.OUT.Stations")
		dir.create(outPICSAdir, showWarnings = FALSE, recursive = TRUE)
		jpg.wcdt <- GeneralParameters$JPEG$width
		jpg.hcdt <- GeneralParameters$JPEG$height
		jpg.ucdt <- GeneralParameters$JPEG$units
		jpg.rcdt <- GeneralParameters$JPEG$res

		allfonct <- unlist(Map(function_name, Filter(is_function, parse(file.path(apps.dir, 'functions', 'PICSA_Plot_functions.R')))))
		toExport1 <- c(toExport1, allfonct, "writeFiles", "table.annuel", "ONI.data", "ONI.date")

		##################

		# for(j in seq(ncol(cdtPrecip$data))){
		ret <- foreach(j = seq(ncol(cdtPrecip$data)), .packages = c("matrixStats", "stringr", "tools", "fitdistrplus"), .export = toExport1) %parLoop% {
			stnDIR <- file.path(outPICSAdir, cdtPrecip$id[j])
			dir.create(stnDIR, showWarnings = FALSE, recursive = TRUE)
			don <- output.cdt[[j]]

			## write data
			don.out <- data.frame(onset = onset[, j], cessation = cessat[, j], seasonLength = as.numeric(season.length[, j]), don)
			writeFiles(don.out, file.path(stnDIR, paste0(cdtPrecip$id[j], '_out', '.csv')), col.names = TRUE)

			## ONI 
			ijoni <- getIndexSeasonVars(onset[, j], cessat[, j], ONI.date, "monthly")
			oni.idx <- sapply(ijoni, function(x) mean(ONI.data[x], na.rm = TRUE))
			oni.idx <- ifelse(oni.idx >= 0.5, 3, ifelse(oni.idx <= -0.5, 1, 2))

			### Rainfall amounts ENSO line
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_rainfall.amount.ENSO_line', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.line.ONI(yrsOnset, don$RainTotal, xlab = 'Year', ylab = 'Rainfall Amount (mm)', colz = oni.idx)
			dev.off()

			### Rainfall amounts ENSO bar
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_rainfall.amount.ENSO_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.bar.ONI(yrsOnset, don$RainTotal, xlab = 'Year', ylab = 'Rainfall Amount (mm)', colz = oni.idx)
			dev.off()

			### Rainfall amounts ENSO prob
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_rainfall.amount.ENSO_prob', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.probaExeecdance.ONI(don$RainTotal, oni.idx, xlab = 'Rainfall Amount (mm)')
			dev.off()

			### daily rainfall
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_daily_rain', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			picsa.plot.daily(cdtPrecip$dates, cdtPrecip$data[, j], thres.rain.day)
			dev.off()

			### Onset
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_onset', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.line.yaxdate(yrsOnset, matOnset[, j], origindate = onsetXS)
			dev.off()

			### Onset1
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_onset1', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.line.yaxdate(yrsOnset, matOnset[, j], origindate = onsetXS, mean = TRUE, tercile = TRUE)
			dev.off()

			### Cessation
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_cessation', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.line.yaxdate(yrsCesst, matCessat[, j], origindate = cessatXS)
			dev.off()

			### Cessation1
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_cessation1', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.line.yaxdate(yrsCesst, matCessat[, j], origindate = cessatXS, mean = TRUE, tercile = TRUE)
			dev.off()

			### Length of season
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_Length.of.season', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.line.yaxvalue(yrsOnset, season.length[, j], xlab = 'Year', ylab = 'Number of Days')
			dev.off()

			### Length of season1
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_Length.of.season', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.line.yaxvalue(yrsOnset, season.length[, j], xlab = 'Year', ylab = 'Number of Days', mean = TRUE, tercile = TRUE)
			dev.off()

			### Rainfall amounts line
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_rainfall.amount_line1', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.line.yaxvalue(yrsOnset, don$RainTotal, xlab = 'Year', ylab = 'Rainfall Amount (mm)')
			dev.off()

			### Rainfall amounts line
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_rainfall.amount_line2', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.line.yaxvalue(yrsOnset, don$RainTotal, xlab = 'Year', ylab = 'Rainfall Amount (mm)', mean = TRUE, tercile = TRUE)
			dev.off()

			### Rainfall amounts bar
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_rainfall.amount_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.bar.yaxvalue(yrsOnset, don$RainTotal, xlab = 'Year', ylab = 'Rainfall Amount (mm)')
			dev.off()

			### Rainfall amounts prob
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_rainfall.amount_prob', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.probaExeecdance.theor(don$RainTotal, xlab = "Rainfall Amount (mm)")
			dev.off()

			## Dry Spell 5 line
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_dry.spell.5days_line', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.line.dryspell(yrsOnset, don$DrySpell5, xlab = 'Year', ylab = 'Number of Dry Spells', sub = "Dry spells - 5 or more consecutive days")
			dev.off()
			## Dry Spell 5 bar
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_dry.spell.5days_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.bar.dryspell(yrsOnset, don$DrySpell5, xlab = 'Year', ylab = 'Number of Dry Spells', sub = "Dry spells - 5 or more consecutive days")
			dev.off()
			## Dry Spell 5 ecdf
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_dry.spell.5days_prob', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.probaExeecdance(don$DrySpell5, xlab = 'Number of Dry Spells', sub = "Dry spells - 5 or more consecutive days")
			dev.off()

			## Dry Spell 7 line
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_dry.spell.7days_line', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.line.dryspell(yrsOnset, don$DrySpell7, xlab = 'Year', ylab = 'Number of Dry Spells', sub = "Dry spells - 7 or more consecutive days")
			dev.off()
			## Dry Spell 7 bar
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_dry.spell.7days_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.bar.dryspell(yrsOnset, don$DrySpell7, xlab = 'Year', ylab = 'Number of Dry Spells', sub = "Dry spells - 7 or more consecutive days")
			dev.off()
			## Dry Spell 7 ecdf
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_dry.spell.7days_prob', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.probaExeecdance(don$DrySpell7, xlab = 'Number of Dry Spells', sub = "Dry spells - 7 or more consecutive days")
			dev.off()

			## Dry Spell 10 line
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_dry.spell.10days_line', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.line.dryspell(yrsOnset, don$DrySpell10, xlab = 'Year', ylab = 'Number of Dry Spells', sub = "Dry spells - 10 or more consecutive days")
			dev.off()
			## Dry Spell 10 bar
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_dry.spell.10days_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.bar.dryspell(yrsOnset, don$DrySpell10, xlab = 'Year', ylab = 'Number of Dry Spells', sub = "Dry spells - 10 or more consecutive days")
			dev.off()
			## Dry Spell 10 ecdf
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_dry.spell.10days_prob', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.probaExeecdance(don$DrySpell10, xlab = 'Number of Dry Spells', sub = "Dry spells - 10 or more consecutive days")
			dev.off()

			## longest dry spell line
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_longest.dry.spell_line', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.line.yaxvalue(yrsOnset, don$DrySpellMax, xlab = 'Year', ylab = 'Longest dry spell (days)', mean = TRUE)
			dev.off()

			## longest dry spell bar
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_longest.dry.spell_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.bar.yaxvalue(yrsOnset, don$DrySpellMax, xlab = 'Year', ylab = 'Longest dry spell (days)')
			dev.off()

			## Rainy days line
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_rainy.days_line', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.line.yaxvalue(yrsOnset, don$NbRainDay, xlab = 'Year', ylab = 'Rainy days (days)', linecol = 'darkgreen', pointcol = "lightblue", mean = TRUE)
			dev.off()

			## Rainy days bar
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_rainy.days_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.bar.yaxvalue(yrsOnset, don$NbRainDay, xlab = 'Year', ylab = 'Rainy days (days)', barcol = "darkgreen")
			dev.off()

			## max 24h line
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_max.1-day.precip_line', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.line.yaxvalue(yrsOnset, don$MaxDailyRain, xlab = 'Year', ylab = 'Seasonal maximum 1-day precipitation (mm)', linecol = 'turquoise4', pointcol = "lightblue", mean = TRUE)
			dev.off()

			## max 24h bar
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_max.1-day.precip_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			plot.bar.yaxvalue(yrsOnset, don$MaxDailyRain, xlab = 'Year', ylab = 'Seasonal maximum 1-day precipitation (mm)', barcol = "turquoise4")
			dev.off()

			## above 95th percentile line
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_nb.precip.gt.95th.perc_line', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			op <- par(mar = c(5.1, 5.1, 3.1, 2.1))
			plot.line.yaxvalue(yrsOnset, don$ExtremeFrequency95Per, xlab = 'Year', ylab = 'Seasonal count of days when\nRR > 95th percentile (days)', linecol = 'darkgreen', pointcol = "lightblue", mean = TRUE)
			par(op)
			dev.off()

			## above 95th percentile line
			jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_nb.precip.gt.95th.perc_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
			op <- par(mar = c(5.1, 5.1, 3.1, 2.1))
			plot.bar.yaxvalue(yrsOnset, don$ExtremeFrequency95Per, xlab = 'Year', ylab = 'Seasonal count of days when\nRR > 95th percentile (days)', barcol = "darkgreen")
			par(op)
			dev.off()

			## tmax & tmin
			if(compute.ETP == "temp"){
				jpeg(file.path(stnDIR, paste0(cdtPrecip$id[j], '_tmax.tmin_line', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				plot.tmax.tmin(yrsOnset, don$tmax, don$tmin)
				dev.off()
			}
		}
		if(closeklust) stopCluster(klust)
		InsertMessagesTxt(main.txt.out, 'Plot done!')
	}

	# ##################
	# ##REMOVE
	# if(compute.ETP == "temp"){
	# 	outDATA <- list(rr = cdtPrecip, tx = cdtTmax, tn = cdtTmin)
	# }else{
	# 	outDATA <- list(rr = cdtPrecip, etp = cdtETP)
	# }
	# if(dekmonUse) outDATA <- c(outDATA, list(rrdk = cdtPrecip1, etp = cdtETP))

	# outDATA <- c(outDATA, list(onset = onset, cessat = cessat, yearS = yearS, yearE = yearE, output.cdt = output.cdt))

	# # outDATA <- c(outDATA, list(onset = onset, cessat = cessat, onset.1970 = onset.1970,
	# # 			onset.julian = onset.julian, cessat.1970 = cessat.1970, cessat.julian = cessat.julian))

	# assign("outDATA", outDATA, envir = .GlobalEnv)

	# ##################


	return(0)

}
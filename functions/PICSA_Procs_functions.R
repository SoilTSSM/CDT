
PICSAProcs <- function(GeneralParameters){
	readPICSAcdtData <- function(cdtData, cdtFile, timestep, outdir, chunksize = 100)
	{
		dir.cdtTmpVar <- file.path(outdir, cdtData)
		dataInfo <- getStnOpenDataInfo(cdtFile)
		if(!is.null(EnvPICSA[[cdtData]])){
			if(!isTRUE(all.equal(EnvPICSA[[cdtData]]$dataInfo, dataInfo))){
				readcdtData <- TRUE
				unlink(dir.cdtTmpVar, recursive = TRUE)
				EnvPICSA[[cdtData]] <- NULL
			}else readcdtData <- FALSE
		}else readcdtData <- TRUE

		if(readcdtData){
			cdtTmpVar <- getStnOpenData(cdtFile)
			if(is.null(cdtTmpVar)) return(NULL)
			cdtTmpVar <- getCDTdataAndDisplayMsg(cdtTmpVar, timestep)
			if(is.null(cdtTmpVar)) return(NULL)

			data.cdtTmpVar <- cdtTmpVar$data
			cdtTmpVar <- cdtTmpVar[c('id', 'lon', 'lat', 'dates')]
			cdtTmpVar$index <- seq_along(cdtTmpVar$dates)

			dir.create(dir.cdtTmpVar, showWarnings = FALSE, recursive = TRUE)
			colInfo <- writecdtDATAchunk(data.cdtTmpVar, dir.cdtTmpVar, chunksize)
			cdtTmpVar$colInfo <- colInfo

			EnvPICSA[[cdtData]] <- cdtTmpVar
			EnvPICSA[[cdtData]]$dataInfo <- dataInfo
			rm(data.cdtTmpVar)
		}else cdtTmpVar <- EnvPICSA[[cdtData]]

		return(cdtTmpVar)
	}

	readPICSANcdfData <- function(cdtData, vardates, ncdfPATH, ncdfInfo,
								ncdfDir, fileFormat, outdir, chunksize = 100)
	{
		nc <- nc_open(ncdfPATH[1])
		nc.lon <- nc$dim[[ncdfInfo$rfeILon]]$vals
		nc.lat <- nc$dim[[ncdfInfo$rfeILat]]$vals
		nc_close(nc)

		xo <- order(nc.lon)
		nc.lon <- nc.lon[xo]
		yo <- order(nc.lat)
		nc.lat <- nc.lat[yo]
		len.lon <- length(nc.lon)
		len.lat <- length(nc.lat)

		col.id <- seq(len.lon*len.lat)
		col.grp <- split(col.id, ceiling(col.id/chunksize))
		col.idx <- rep(seq_along(col.grp), sapply(col.grp, length))

		dir.cdtTmpVar <- file.path(outdir, cdtData)
		if(!file.exists(dir.cdtTmpVar)) dir.create(dir.cdtTmpVar, showWarnings = FALSE, recursive = TRUE)

		dataInfo <- c(ncdfDir, fileFormat)
		bindPicsaData <- FALSE
		if(!is.null(EnvPICSA[[cdtData]])){
			iexist <- vardates%in%EnvPICSA[[cdtData]]$dates
			if(all(iexist)){
				if(!isTRUE(all.equal(EnvPICSA[[cdtData]]$dataInfo, dataInfo))){
					readNcdfData <- TRUE
					unlink(dir.cdtTmpVar, recursive = TRUE)
					EnvPICSA[[cdtData]] <- NULL
				}else readNcdfData <- FALSE
			}else{
				if(isTRUE(all.equal(EnvPICSA[[cdtData]]$dataInfo, dataInfo))){
					bindPicsaData <- TRUE
					if(any(iexist)){
						vardates <- vardates[!iexist]
						ncdfPATH <- ncdfPATH[!iexist]
					}
				}else{
					unlink(dir.cdtTmpVar, recursive = TRUE)
					EnvPICSA[[cdtData]] <- NULL
				}
				readNcdfData <- TRUE
			}
		}else readNcdfData <- TRUE

		if(readNcdfData){
			##################
			# p0 <- Sys.time()

			is.parallel <- doparallel(length(ncdfPATH) >= 180)
			`%parLoop%` <- is.parallel$dofun
			ncDaty <- foreach(jj = seq_along(ncdfPATH), .packages = "ncdf4") %parLoop% {
				nc <- nc_open(ncdfPATH[jj])
				vars <- ncvar_get(nc, varid = ncdfInfo$rfeVarid)
				nc_close(nc)
				vars <- vars[xo, yo]
				if(ncdfInfo$rfeILat < ncdfInfo$rfeILon){
					vars <- matrix(c(vars), nrow = len.lon, ncol = len.lat, byrow = TRUE)
				}
				vars <- round(c(vars), 1)

				for(j in seq_along(col.grp)){
					file.tmp <- file.path(dir.cdtTmpVar, paste0(j, ".gz"))
					con <- gzfile(file.tmp, open = "a", compression = 5)
					cat(c(vars[col.grp[[j]]], '\n'), file = con)
					close(con)
				}
				rm(vars); gc()
				return(vardates[jj])
			}
			if(is.parallel$stop) stopCluster(is.parallel$cluster)

			##################
			# InsertMessagesTxt(main.txt.out, paste('Reading ncdf', difftime(Sys.time(), p0, units = "mins")))

			ncDaty <- do.call(c, ncDaty)

			##################
			# p0 <- Sys.time()

			cdtTmpVar <- NULL
			xycrd <- expand.grid(nc.lon, nc.lat)
			cdtTmpVar$lon <- xycrd[, 1]
			cdtTmpVar$lat <- xycrd[, 2]
			cdtTmpVar$colInfo <- list(id = col.id, index = col.idx)

			is.parallel <- doparallel(length(col.grp) >= 10)
			`%parLoop%` <- is.parallel$dofun
			if(bindPicsaData){
				ret <- foreach(j = seq_along(col.grp)) %parLoop% {
					file.gz <- file.path(dir.cdtTmpVar, paste0(j, ".gz"))
					R.utils::gunzip(file.gz)
					file.tmp <- tools::file_path_sans_ext(file.gz)
					tmp <- data.table::fread(file.tmp, header = FALSE, sep = " ", stringsAsFactors = FALSE, colClasses = "numeric")
					unlink(file.tmp)
					tmp <- as.matrix(tmp)
					dimnames(tmp) <- NULL
					file.rds <- file.path(dir.cdtTmpVar, paste0(j, ".rds"))
					y <- readRDS(file.rds)
					z <- rbind(y, tmp)
					con <- gzfile(file.rds, compression = 5)
					open(con, "wb")
					saveRDS(z, con)
					close(con)
					rm(y, z, tmp); gc()
				}
			}else{
				ret <- foreach(j = seq_along(col.grp)) %parLoop% {
					file.gz <- file.path(dir.cdtTmpVar, paste0(j, ".gz"))
					R.utils::gunzip(file.gz)
					file.tmp <- tools::file_path_sans_ext(file.gz)
					tmp <- data.table::fread(file.tmp, header = FALSE, sep = " ", stringsAsFactors = FALSE, colClasses = "numeric")
					unlink(file.tmp)
					tmp <- as.matrix(tmp)
					dimnames(tmp) <- NULL
					file.rds <- file.path(dir.cdtTmpVar, paste0(j, ".rds"))
					con <- gzfile(file.rds, compression = 5)
					open(con, "wb")
					saveRDS(tmp, con)
					close(con)
					rm(tmp); gc()
				}
			}
			if(is.parallel$stop) stopCluster(is.parallel$cluster)

			##################
			# InsertMessagesTxt(main.txt.out, paste('write ncdata', difftime(Sys.time(), p0, units = "mins")))

			idx <- seq(length(ncDaty))
			if(bindPicsaData){
				cdtTmpVar$dates <- c(EnvPICSA[[cdtData]]$dates, ncDaty)
				cdtTmpVar$index <- c(EnvPICSA[[cdtData]]$index, max(EnvPICSA[[cdtData]]$index)+idx)
			}else{
				cdtTmpVar$dates <- ncDaty
				cdtTmpVar$index <- idx
			}
			odaty <- order(cdtTmpVar$dates)
			cdtTmpVar$dates <- cdtTmpVar$dates[odaty]
			cdtTmpVar$index <- cdtTmpVar$index[odaty]

			cdtTmpVar$dataInfo <- dataInfo
			EnvPICSA[[cdtData]] <- cdtTmpVar

			rm(ncDaty, xycrd, idx, odaty)
			gc()
		}else cdtTmpVar <- EnvPICSA[[cdtData]]

		return(cdtTmpVar)
	}

	extractPICSArdsData <- function(cdtData, index, pid, outdir, chunksize = 100, chunk.par = TRUE)
	{
		dir.cdtData <- file.path(outdir, cdtData)
		data.cdtData <- readcdtDATAchunk(pid, get(cdtData)$colInfo, dir.cdtData, chunksize, chunk.par)
		tmp <- data.cdtData[index, , drop = FALSE]
		rm(data.cdtData); gc()
		return(tmp)
	}

	extractPICSArdsData1 <- function(cdtData, loc, opDATA, outdir, chunksize = 100, chunk.par = TRUE){
		dir.cdtData <- file.path(outdir, cdtData)
		data.cdtData <- readcdtDATAchunk(opDATA[[cdtData]]$pid[loc], get(cdtData)$colInfo, dir.cdtData, chunksize, chunk.par)
		tmp <- data.cdtData[opDATA[[cdtData]]$index, , drop = FALSE]
		rm(data.cdtData); gc()
		return(tmp)
	}

	##################

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
	col.max <- GeneralParameters$COL.MAX
	chunksize <- GeneralParameters$chunksize
	outputDIR <- GeneralParameters$Outdir

	##################

	dirPICSAData <- file.path(outputDIR, "PICSA.OUT.Data")
	outPICSAData <- file.path(dirPICSAData, "Data")
	dir.create(outPICSAData, showWarnings = FALSE, recursive = TRUE)
	outputPICSA <- file.path(dirPICSAData, "Output")
	dir.create(outputPICSA, showWarnings = FALSE, recursive = TRUE)

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

	if(!GeneralParameters$date.range$all.years){
		if(is.na(GeneralParameters$date.range$start.year) | is.na(GeneralParameters$date.range$end.year)){
			InsertMessagesTxt(main.txt.out, "Invalid start and end years", format = TRUE)
			return(NULL)
		}
	}

	##################

	if(data.type == 'cdt'){
		cdtPrecip <- readPICSAcdtData("cdtPrecip", rainFl, "daily", outPICSAData, chunksize)
		if(is.null(cdtPrecip)) return(NULL)

		if(compute.ETP == "temp"){
			cdtTmax <- readPICSAcdtData("cdtTmax", tmaxFl, "daily", outPICSAData, chunksize)
			if(is.null(cdtTmax)) return(NULL)
			cdtTmin <- readPICSAcdtData("cdtTmin", tminFl, "daily", outPICSAData, chunksize)
			if(is.null(cdtTmin)) return(NULL)
			tmpdates <- intersect(intersect(cdtPrecip$dates, cdtTmax$dates), cdtTmin$dates)
		}else{
			cdtETP <- readPICSAcdtData("cdtETP", etpFl, "daily", outPICSAData, chunksize)
			if(is.null(cdtETP)) return(NULL)
			tmpdates <- intersect(cdtPrecip$dates, cdtETP$dates)
		}

		if(length(tmpdates) == 0){
			msg <- if(compute.ETP == "temp") "tmax and tmin" else " and PET"
			InsertMessagesTxt(main.txt.out, paste("Daily rain", msg, "dates did not overlap"), format = TRUE)
			return(NULL)
		}

		if(dekmonUse){
			cdtPrecip1 <- readPICSAcdtData("cdtPrecip1", dekmonFl, dekmonTs, outPICSAData, chunksize)
			if(is.null(cdtPrecip1)) return(NULL)

			if(!any(substr(cdtPrecip$dates, 1, 6)%in%substr(cdtPrecip1$dates, 1, 6))){
				InsertMessagesTxt(main.txt.out, "Daily and dekadal or monthly rain dates did not overlap", format = TRUE)
				return(NULL)
			}
		}
	}

	##################

	if(data.type == 'netcdf'){
		if(GeneralParameters$date.range$all.years){
			startYear <- 1900
			endYear <- 2020
		}else{
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

		if(compute.ETP == "temp"){
			tmaxPATH <- tmaxPATH[tmaxdates%in%raindates]
			tmaxdates <- tmaxdates[tmaxdates%in%raindates]
			tminPATH <- tminPATH[tmindates%in%raindates]
			tmindates <- tmindates[tmindates%in%raindates]
		}else{
			etpPATH <- etpPATH[etpdates%in%raindates]
			etpdates <- etpdates[etpdates%in%raindates]
		}

		##################
		if(dekmonUse){
			dekmondates <- datesDM[dekmonExist]
			dekmonPATH <- dekmonPATH[dekmonExist]
			dekmonInt <- substr(dekmondates, 1, 6)%in%substr(raindates, 1, 6)
			if(!any(dekmonInt)){
				InsertMessagesTxt(main.txt.out, "Daily rain and dekadal or monthly dates did not overlap", format = TRUE)
				return(NULL)
			}
			dekmonPATH <- dekmonPATH[dekmonInt]
			dekmondates <- dekmondates[dekmonInt]
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

		InsertMessagesTxt(main.txt.out, 'Read rainfall data ...')
		cdtPrecip <- readPICSANcdfData("cdtPrecip", raindates, rainPATH, rainNcInfo, rainFl, rainFf, outPICSAData, chunksize)
		InsertMessagesTxt(main.txt.out, 'Reading rainfall data finished')

		#####################################

		if(compute.ETP == "temp"){
			InsertMessagesTxt(main.txt.out, 'Read tmax data ...')
			cdtTmax <- readPICSANcdfData("cdtTmax", tmaxdates, tmaxPATH, tmaxNcInfo, tmaxFl, tmaxFf, outPICSAData, chunksize)
			InsertMessagesTxt(main.txt.out, 'Reading tmax data finished')

			############
			InsertMessagesTxt(main.txt.out, 'Read tmin data ...')
			cdtTmin <- readPICSANcdfData("cdtTmin", tmindates, tminPATH, tminNcInfo, tminFl, tminFf, outPICSAData, chunksize)
			InsertMessagesTxt(main.txt.out, 'Reading tmin data finished')
		}else{
			InsertMessagesTxt(main.txt.out, 'Read PET data ...')
			cdtETP <- readPICSANcdfData("cdtETP", etpdates, etpPATH, etpNcInfo, etpFl, etpFf, outPICSAData, chunksize)
			InsertMessagesTxt(main.txt.out, 'Reading PET data finished')
		}

		#####################################

		if(dekmonUse){
			InsertMessagesTxt(main.txt.out, 'Read rainfall data for seasonal amounts ...')
			cdtPrecip1 <- readPICSANcdfData("cdtPrecip1", dekmondates, dekmonPATH, dekmonNcInfo, dekmonFl, dekmonFf, outPICSAData, chunksize)
			InsertMessagesTxt(main.txt.out, 'Reading rainfall data for seasonal amount finished')
		}
	}

	##################

	opDATA <- NULL
	opDATA$dates <- cdtPrecip$dates

	##################

	if(data.type == 'cdt'){
		if(!GeneralParameters$date.range$all.years){
			taona <- as.numeric(substr(cdtPrecip$dates, 1, 4))
			itaona <- taona >= GeneralParameters$date.range$start.year & taona <= GeneralParameters$date.range$end.year
			opDATA$dates <- cdtPrecip$dates[itaona]
			opDATA$cdtPrecip$index <- cdtPrecip$index[itaona]
		}else opDATA$cdtPrecip$index <- cdtPrecip$index
	}else opDATA$cdtPrecip$index <- cdtPrecip$index

	if(compute.ETP == "temp"){
		opDATA$cdtTmax$index <- cdtTmax$index[match(opDATA$dates, cdtTmax$dates)]
		opDATA$cdtTmin$index <- cdtTmin$index[match(opDATA$dates, cdtTmin$dates)]
	}else opDATA$cdtETP$index <- cdtETP$index[match(opDATA$dates, cdtETP$dates)]

	if(dekmonUse){
		opDATA$cdtPrecip1$index <- cdtPrecip1$index[substr(cdtPrecip1$dates, 1, 6)%in%substr(opDATA$dates, 1, 6)]
		opDATA$cdtPrecip1$dates <- cdtPrecip1$dates[substr(cdtPrecip1$dates, 1, 6)%in%substr(opDATA$dates, 1, 6)]
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

		opDATA$cdtPrecip$pid <- match(idens, cdtPrecip$id)
		opDATA$lon <- cdtPrecip$lon[opDATA$cdtPrecip$pid]
		opDATA$lat <- cdtPrecip$lat[opDATA$cdtPrecip$pid]
		opDATA$id <- cdtPrecip$id[opDATA$cdtPrecip$pid]

		if(compute.ETP == "temp"){
			opDATA$cdtTmax$pid <- match(idens, cdtTmax$id)
			opDATA$cdtTmin$pid <- match(idens, cdtTmin$id)
		}else opDATA$cdtETP$pid <- match(idens, cdtETP$id)

		if(dekmonUse){
			if(length(intersect(idens, cdtPrecip1$id)) == 0){
				InsertMessagesTxt(main.txt.out, "No match between daily and dekadal rain ID", format = TRUE)
				return(NULL)
			}
			opDATA$cdtPrecip1$pid <- match(idens, cdtPrecip1$id)
		}
	}

	if(data.type == 'netcdf'){
		xyPrecip <- data.frame(x = cdtPrecip$lon, y = cdtPrecip$lat)
		coordinates(xyPrecip) <- ~x+y
		xyPrecip <- SpatialPixels(points = xyPrecip, tolerance = sqrt(sqrt(.Machine$double.eps)))
		opDATA$cdtPrecip$pid <- seq_along(cdtPrecip$lon)
		opDATA$lon <- cdtPrecip$lon
		opDATA$lat <- cdtPrecip$lat

		if(compute.ETP == "temp"){
			xyTmax <- data.frame(x = cdtTmax$lon, y = cdtTmax$lat)
			coordinates(xyTmax) <- ~x+y
			xyTmax <- SpatialPixels(points = xyTmax, tolerance = sqrt(sqrt(.Machine$double.eps)))
			opDATA$cdtTmax$pid <- over(xyPrecip, xyTmax)

			xyTmin <- data.frame(x = cdtTmin$lon, y = cdtTmin$lat)
			coordinates(xyTmin) <- ~x+y
			xyTmin <- SpatialPixels(points = xyTmin, tolerance = sqrt(sqrt(.Machine$double.eps)))
			opDATA$cdtTmin$pid <- over(xyPrecip, xyTmin)
			rm(xyTmax, xyTmin)
		}else{
			xyETP <- data.frame(x = cdtETP$lon, y = cdtETP$lat)
			coordinates(xyETP) <- ~x+y
			xyETP <- SpatialPixels(points = xyETP, tolerance = sqrt(sqrt(.Machine$double.eps)))
			opDATA$cdtETP$pid <- over(xyPrecip, xyETP)
			rm(xyETP)
		}

		if(dekmonUse){
			xyPrecip1 <- data.frame(x = cdtPrecip1$lon, y = cdtPrecip1$lat)
			coordinates(xyPrecip1) <- ~x+y
			xyPrecip1 <- SpatialPixels(points = xyPrecip1, tolerance = sqrt(sqrt(.Machine$double.eps)))
			opDATA$cdtPrecip1$pid <- over(xyPrecip, xyPrecip1)
			rm(xyPrecip1)
		}
		rm(xyPrecip)
	}

	##################################################################################################

	COLIDX <- seq_along(opDATA$lon)
	loopCOL <- split(COLIDX, ceiling(COLIDX/col.max))

	##################################################################################################

	if(compute.ETP == "temp"){
		InsertMessagesTxt(main.txt.out, 'Compute PET ...')

		etpdates <- opDATA$dates
		index.cdtPrecip <- opDATA$cdtPrecip$index
		index.cdtTmax <- opDATA$cdtTmax$index
		index.cdtTmin <- opDATA$cdtTmin$index

		dir.cdtETP <- file.path(outPICSAData, "cdtETP")
		tmaxInfo <- c(tmaxFl, tmaxFf)
		tminInfo <- c(tminFl, tminFf)
		change.tmax <- !isTRUE(all.equal(EnvPICSA$cdtTmax$dataInfo, tmaxInfo))
		change.tmin <- !isTRUE(all.equal(EnvPICSA$cdtTmin$dataInfo, tminInfo))
		bindPicsaData <- FALSE
		if(!is.null(EnvPICSA$cdtETP)){
			iexist <- etpdates%in%EnvPICSA$cdtETP$dates
			if(all(iexist)){
				if(change.tmax | change.tmin){
					calcETPdata <- TRUE
					unlink(dir.cdtETP, recursive = TRUE)
					EnvPICSA$cdtETP <- NULL
				}else calcETPdata <- FALSE
			}else{
				if(!change.tmax & !change.tmin){
					bindPicsaData <- TRUE
					if(any(iexist)){
						etpdates <- etpdates[!iexist]
						index.cdtPrecip <- index.cdtPrecip[!iexist]
						index.cdtTmax <- index.cdtTmax[!iexist]
						index.cdtTmin <- index.cdtTmin[!iexist]
					}
				}else{
					unlink(dir.cdtETP, recursive = TRUE)
					EnvPICSA$cdtETP <- NULL
				}
				calcETPdata <- TRUE
			}
		}else calcETPdata <- TRUE

		if(calcETPdata){
			##################
			# p0 <- Sys.time()

			mmdd.anual <- format(seq(as.Date("2000-1-1"), as.Date("2000-12-31"), 'day'), "%m%d")
			immdd <- match(substr(etpdates, 5, 8), mmdd.anual)

			is.parallel <- doparallel(length(loopCOL) >= 6)
			`%parLoop%` <- is.parallel$dofun

			toExports <- c("ExtraterrestrialRadiation", "ETPHargreaves", "doparallel", "readcdtDATAchunk")
			etpData <- foreach(j = seq_along(loopCOL), .export = toExports, .packages = "doParallel") %parLoop% {
				loc <- loopCOL[[j]]
				latRa <- unique(opDATA$lat[loc])
				Ra.Annual <- ExtraterrestrialRadiation(latRa)
				ilat <- match(opDATA$lat[loc], latRa)
				RA <- Ra.Annual[immdd, ilat, drop = FALSE]

				tmax <- extractPICSArdsData("cdtTmax", index.cdtTmax, opDATA$cdtTmax$pid[loc],
											outPICSAData, chunksize, chunk.par = FALSE)
				tmin <- extractPICSArdsData("cdtTmin", index.cdtTmin, opDATA$cdtTmin$pid[loc],
											outPICSAData, chunksize, chunk.par = FALSE)
				precip <- extractPICSArdsData("cdtPrecip", index.cdtPrecip, opDATA$cdtPrecip$pid[loc],
												outPICSAData, chunksize, chunk.par = FALSE)

				etp <- ETPHargreaves(tmax, tmin, RA, precip)
				etp <- round(etp, 1)
				rm(Ra.Annual, RA, tmax, tmin, precip); gc()
				return(etp)
			}
			if(is.parallel$stop) stopCluster(is.parallel$cluster)

			##################
			# InsertMessagesTxt(main.txt.out, paste('compute etp', difftime(Sys.time(), p0, units = "mins")))
			# p0 <- Sys.time()

			etpData <- do.call(cbind, etpData)

			##################
			# InsertMessagesTxt(main.txt.out, paste('do.call etp', difftime(Sys.time(), p0, units = "mins")))
			# p0 <- Sys.time()

			cdtETP <- NULL
			if(data.type == 'cdt') cdtETP$id <- opDATA$id
			cdtETP[c('lon', 'lat')] <- opDATA[c('lon', 'lat')]

			if(!file.exists(dir.cdtETP)) dir.create(dir.cdtETP, showWarnings = FALSE, recursive = TRUE)
			write.cdtETP <- if(bindPicsaData) writebindcdtDATAchunk else writecdtDATAchunk
			colInfo <- write.cdtETP(etpData, dir.cdtETP, chunksize)
			cdtETP$colInfo <- colInfo

			##################
			# InsertMessagesTxt(main.txt.out, paste('write etp', difftime(Sys.time(), p0, units = "mins")))

			idx <- seq(length(etpdates))
			if(bindPicsaData){
				cdtETP$dates <- c(EnvPICSA$cdtETP$dates, etpdates)
				cdtETP$index <- c(EnvPICSA$cdtETP$index, max(EnvPICSA$cdtETP$index)+idx)
			}else{
				cdtETP$dates <- etpdates
				cdtETP$index <- idx
			}
			odaty <- order(cdtETP$dates)
			cdtETP$dates <- cdtETP$dates[odaty]
			cdtETP$index <- cdtETP$index[odaty]

			opDATA$cdtETP$index <- cdtETP$index
			opDATA$cdtETP$pid <- seq_along(opDATA$lon)

			EnvPICSA$cdtETP <- cdtETP
			rm(etpData); gc()
		}else cdtETP <- EnvPICSA$cdtETP
		InsertMessagesTxt(main.txt.out, 'Computing PET done!')
	}

	##################################################################################################

	EnvPICSA$opDATA <- opDATA
	EnvPICSA$PICSA.Coords$lon <- opDATA$lon
	EnvPICSA$PICSA.Coords$lat <- opDATA$lat
	if(data.type == 'cdt') EnvPICSA$PICSA.Coords$id <- opDATA$id

	#################

	EnvPICSA$Pars$data.type <- GeneralParameters$data.type
	EnvPICSA$Pars$compute.ETP <- GeneralParameters$compute.ETP
	EnvPICSA$Pars$dekmonUse <- GeneralParameters$dekmon$use.dekmon
	EnvPICSA$Pars$dekmonTs <- GeneralParameters$dekmon$time.step
	EnvPICSA$Pars$thres.rain.day <- GeneralParameters$onset$thres.rain.day
	EnvPICSA$Pars$col.max <- GeneralParameters$COL.MAX
	EnvPICSA$Pars$chunksize <- GeneralParameters$chunksize

	load(file.path(apps.dir, 'data', 'ONI_50-2016.RData'))
	EnvPICSA$ONI$date <- format(seq(as.Date('1950-1-15'), as.Date('2017-12-15'), "month"), "%Y%m")
	EnvPICSA$ONI$data <- ONI$ts[, 3]

	#################

	if(data.type == 'cdt'){
		cdtHeadIfon <- cbind(c("STN", "LON", "YEARS/LAT"), rbind(opDATA$id, opDATA$lon, opDATA$lat))
	}
	if(data.type == 'netcdf'){
		ncLON <- sort(unique(opDATA$lon))
		ncLAT <- sort(unique(opDATA$lat))
		nLON <- length(ncLON)
		nLAT <- length(ncLAT)
		dx <- ncdim_def("Lon", "degreeE", ncLON)
		dy <- ncdim_def("Lat", "degreeN", ncLAT)
		cdtNcdfDim <- list(dx, dy)
	}

	##################################################################################################

	onset.start.month <- GeneralParameters$onset$early.month
	onset.start.day <- GeneralParameters$onset$early.day
	onset.end.month <- GeneralParameters$onset$late.month
	onset.end.day <- GeneralParameters$onset$late.day
	cess.start.month <- GeneralParameters$cessation$early.month
	cess.start.day <- GeneralParameters$cessation$early.day
	cess.end.month <- GeneralParameters$cessation$late.month
	cess.end.day <- GeneralParameters$cessation$late.day
	thres.rain.day <- GeneralParameters$onset$thres.rain.day
	dryspellfun <- if(thres.rain.day == 0) '==' else '<'
	nbdayfun <- if(thres.rain.day == 0) '>' else '>='

	####################################

	InsertMessagesTxt(main.txt.out, 'Calculate onset of seasonal rainfall ...')

	yearO <- if(onset.end.month <=  onset.start.month) 2015 else 2014
	win2.search <- as.numeric(as.Date(paste(yearO, onset.end.month, onset.end.day, sep = '-'))
						- as.Date(paste(2014, onset.start.month, onset.start.day, sep = '-')))

	onset.pars <- list(rain.total = GeneralParameters$onset$rain.total,
						win0.search = GeneralParameters$onset$first.win.search,
						min.rain.day = GeneralParameters$onset$min.rain.day,
						win1.search = GeneralParameters$onset$dry.spell.win.search,
						dry.spell = GeneralParameters$onset$dry.spell,
						thres.rain.day = thres.rain.day, 
						win2.search = win2.search)

	onset.idx <- getIndexCalcOnsetCess(opDATA$dates, onset.start.month, onset.start.day)

	##################
	is.parallel <- doparallel(length(loopCOL[[1]]) >= 50)
	`%parLoop%` <- is.parallel$dofun

	toExport <- c("onsetDectection")

	ONSET <- foreach(ii = seq_along(loopCOL)) %do% {
		precip <- extractPICSArdsData1("cdtPrecip", loopCOL[[ii]], opDATA, outPICSAData, chunksize, chunk.par = FALSE)
		onset <- foreach(jj = seq_along(onset.idx), .export = toExport) %parLoop% {
			onsetDectection(onset.idx[[jj]], DATA = precip, dates = opDATA$dates, pars = onset.pars, min.frac = 0.99)
		}
		rm(precip); gc()
		do.call(rbind, onset)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	ONSET <- do.call(cbind, ONSET)
	onsetXS <- paste(2014, onset.start.month, onset.start.day, sep = '-')
	yearS <- sapply(onset.idx, function(x) opDATA$dates[x[1]])
	if(onset.start.month > cess.start.month){
		ONSET <- rbind(NA, ONSET)
		yearS <- c(paste0(as.numeric(substr(yearS[1], 1, 4))-1, substr(yearS[1], 5, 8)), yearS)
	}
	yearS <- as.Date(yearS, "%Y%m%d")
	yrsOnset <- as.numeric(format(yearS, "%Y"))
	dimONSET <- dim(ONSET)
	ONSET <- as.Date(ONSET, "%Y%m%d")
	dim(ONSET) <- dimONSET
	ONSET <- as.numeric(ONSET - yearS)
	dim(ONSET) <- dimONSET

	if(data.type == 'cdt'){
		file.cdtONSET1 <- file.path(outputPICSA, "cdtONSET.csv")
		xtmp <- rbind(cdtHeadIfon, cbind(format(yearS, "%Y-%m%d"), ONSET))
		xtmp[is.na(xtmp)] <- -99
		writeFiles(xtmp, file.cdtONSET1)
	}
	if(data.type == 'netcdf'){
		dir.cdtONSET1 <- file.path(outputPICSA, "cdtONSET")
		dir.create(dir.cdtONSET1, showWarnings = FALSE, recursive = TRUE)
		for(j in seq_along(yrsOnset)){
			xtmp <- matrix(ONSET[j, ], nLON, nLAT)
			xtmp[is.na(xtmp)] <- -99
			units <- paste("days since", as.character(yearS[j]))
			nc.grd <- ncvar_def("onset", units, cdtNcdfDim, -99, "Starting dates of the rainy season", "short", shuffle = TRUE, compression = 9)
			ncoutfile <- file.path(dir.cdtONSET1, paste0("onset_", yrsOnset[j], ".nc"))
			nc <- nc_create(ncoutfile, nc.grd)
			ncvar_put(nc, nc.grd, xtmp)
			nc_close(nc)
		}
	}

	dir.cdtONSET <- file.path(outPICSAData, "cdtONSET")
	dir.create(dir.cdtONSET, showWarnings = FALSE, recursive = TRUE)
	colInfo <- writecdtDATAchunk(ONSET, dir.cdtONSET, chunksize)
	saveRDS(ONSET, file.path(dir.cdtONSET, "seas.rds"))

	cdtONSET <- NULL
	# cdtONSET$series.index <- onset.idx
	cdtONSET$years <- yrsOnset
	cdtONSET$days.since <- as.character(yearS)
	cdtONSET$start.labels <- onsetXS
	cdtONSET$colInfo <- colInfo

	EnvPICSA$cdtONSET <- cdtONSET

	rm(ONSET, onset.idx, yearS, colInfo, xtmp); gc()
	InsertMessagesTxt(main.txt.out, 'Calculating onset of seasonal rainfall finished')

	####################################

	InsertMessagesTxt(main.txt.out, 'Calculate cessation of seasonal rainfall ...')

	yearC <- if(cess.end.month <=  cess.start.month) 2015 else 2014
	cess.win.search <- as.numeric(as.Date(paste(yearC, cess.end.month, cess.end.day, sep = '-'))
						- as.Date(paste(2014, cess.start.month, cess.start.day, sep = '-')))

	WB.lag <- 30
	cess.pars <- list(swh.capacity = GeneralParameters$cessation$swh.capacity,
					  min.WB = GeneralParameters$cessation$min.water.balance,
					  length.day = GeneralParameters$cessation$first.win.search,
					  win.search = cess.win.search,
					  WB.lag = WB.lag)

	cess.idx <- getIndexCalcOnsetCess(opDATA$dates, cess.start.month, cess.start.day, WB.lag)

	##################

	is.parallel <- doparallel(length(loopCOL[[1]]) >= 50)
	`%parLoop%` <- is.parallel$dofun

	toExport <- c("cessationDectection")

	CESSAT <- foreach(ii = seq_along(loopCOL)) %do% {
		precip <- extractPICSArdsData1("cdtPrecip", loopCOL[[ii]], opDATA, outPICSAData, chunksize, chunk.par = FALSE)
		etp <- extractPICSArdsData1("cdtETP", loopCOL[[ii]], opDATA, outPICSAData, chunksize, chunk.par = FALSE)
		cessat <- foreach(jj = seq_along(cess.idx), .export = toExport) %parLoop% {
			cessationDectection(cess.idx[[jj]], Precip = precip, ETP = etp, dates = opDATA$dates, pars = cess.pars, min.frac = 0.99)
		}
		rm(precip, etp); gc()
		do.call(rbind, cessat)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	CESSAT <- do.call(cbind, CESSAT)
	cessatXS <- as.character(as.Date(paste(2014, cess.start.month, cess.start.day, sep = '-')))
	yearE <- sapply(cess.idx, function(x) opDATA$dates[x[WB.lag + 1]])
	if(onset.start.month > cess.start.month){
		CESSAT <- rbind(CESSAT, NA)
		yearE <- c(yearE, paste0(as.numeric(substr(yearE[length(yearE)], 1, 4))+1, substr(yearE[length(yearE)], 5, 8)))
	}
	yearE <- as.Date(yearE, "%Y%m%d")
	yrsCesst <- as.numeric(format(yearE, "%Y"))
	dimCESSAT <- dim(CESSAT)
	CESSAT <- as.Date(CESSAT, "%Y%m%d")
	dim(CESSAT) <- dimCESSAT
	CESSAT <- as.numeric(CESSAT - yearE)
	dim(CESSAT) <- dimCESSAT

	if(data.type == 'cdt'){
		file.cdtCESSAT1 <- file.path(outputPICSA, "cdtCESSAT.csv")
		xtmp <- rbind(cdtHeadIfon, cbind(format(yearE, "%Y-%m%d"), CESSAT))
		xtmp[is.na(xtmp)] <- -99
		writeFiles(xtmp, file.cdtCESSAT1)
	}
	if(data.type == 'netcdf'){
		dir.cdtCESSAT1 <- file.path(outputPICSA, "cdtCESSAT")
		dir.create(dir.cdtCESSAT1, showWarnings = FALSE, recursive = TRUE)
		for(j in seq_along(yrsCesst)){
			xtmp <- matrix(CESSAT[j, ], nLON, nLAT)
			xtmp[is.na(xtmp)] <- -99
			units <- paste("days since", as.character(yearE[j]))
			nc.grd <- ncvar_def("cessat", units, cdtNcdfDim, -99, "Ending dates of the rainy season", "short", shuffle = TRUE, compression = 9)
			ncoutfile <- file.path(dir.cdtCESSAT1, paste0("cessation_", yrsCesst[j], ".nc"))
			nc <- nc_create(ncoutfile, nc.grd)
			ncvar_put(nc, nc.grd, xtmp)
			nc_close(nc)
		}
	}

	dir.cdtCESSAT <- file.path(outPICSAData, "cdtCESSAT")
	dir.create(dir.cdtCESSAT, showWarnings = FALSE, recursive = TRUE)
	colInfo <- writecdtDATAchunk(CESSAT, dir.cdtCESSAT, chunksize)
	saveRDS(CESSAT, file.path(dir.cdtCESSAT, "seas.rds"))

	cdtCESSAT <- NULL
	# cdtCESSAT$series.index <- cess.idx
	cdtCESSAT$years <- yrsCesst
	cdtCESSAT$days.since <- as.character(yearE)
	cdtCESSAT$start.labels <- cessatXS
	cdtCESSAT$colInfo <- colInfo

	EnvPICSA$cdtCESSAT <- cdtCESSAT

	rm(CESSAT, cess.idx, yearE, colInfo, xtmp); gc()
	InsertMessagesTxt(main.txt.out, 'Calculating cessation of seasonal rainfall finished')

	###########################################

	saveRDS(EnvPICSA, file = file.path(dirPICSAData, "PICSA.DATA.rds"))

	###########################################

	writeOUTPICSA <- function(cdtData, data.cdtData, nc.grd, prefix, write.rds = TRUE){
		if(data.type == 'cdt'){
			file.cdtData1 <- file.path(outputPICSA, paste0(cdtData, ".csv"))
			xtmp <- rbind(cdtHeadIfon, cbind(cdtONSET$years, data.cdtData))
			xtmp[is.na(xtmp)] <- -99
			writeFiles(xtmp, file.cdtData1)
		}
		if(data.type == 'netcdf'){
			dir.cdtData1 <- file.path(outputPICSA, cdtData)
			dir.create(dir.cdtData1, showWarnings = FALSE, recursive = TRUE)
			for(j in seq_along(cdtONSET$years)){
				xtmp <- matrix(data.cdtData[j, ], nLON, nLAT)
				xtmp[is.na(xtmp)] <- -99
				ncoutfile <- file.path(dir.cdtData1, paste0(prefix, "_", cdtONSET$years[j], ".nc"))
				nc <- nc_create(ncoutfile, nc.grd)
				ncvar_put(nc, nc.grd, xtmp)
				nc_close(nc)
			}
		}
		if(write.rds){
			dir.cdtData <- file.path(outPICSAData, cdtData)
			dir.create(dir.cdtData, showWarnings = FALSE, recursive = TRUE)
			colInfo <- writecdtDATAchunk(data.cdtData, dir.cdtData, chunksize)
			rm(colInfo)
			saveRDS(data.cdtData, file.path(dir.cdtData, "seas.rds"))
		}
		rm(xtmp)
	}

	###########################################

	InsertMessagesTxt(main.txt.out, 'Compute PICSA data ...')

	okparallel <- length(loopCOL) >= 5

	###################

	is.parallel <- doparallel(okparallel)
	`%dofun%` <- is.parallel$dofun
	toExports <- c("doparallel", "readcdtDATAchunk")
	SEASON.LENGTH <- foreach(ii = seq_along(loopCOL), .export = toExports, .packages = "doParallel") %dofun% {
		ONSET <- readcdtDATAchunk(loopCOL[[ii]], cdtONSET$colInfo, file.path(outPICSAData, "cdtONSET"), chunksize, chunk.par = FALSE)
		CESSAT <- readcdtDATAchunk(loopCOL[[ii]], cdtCESSAT$colInfo, file.path(outPICSAData, "cdtCESSAT"), chunksize, chunk.par = FALSE)
		dimONSET <- dim(ONSET)
		ONSET <- as.Date(ONSET, origin = cdtONSET$days.since)
		dim(ONSET) <- dimONSET
		CESSAT <- as.Date(CESSAT, origin = cdtCESSAT$days.since)
		dim(CESSAT) <- dimONSET
		seaslen <- as.numeric(CESSAT - ONSET)
		dim(seaslen) <- dimONSET
		rm(ONSET, CESSAT); gc()
		return(seaslen)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	SEASON.LENGTH <- do.call(cbind, SEASON.LENGTH)
	nc.grd <- NULL
	if(data.type == 'netcdf') nc.grd <- ncvar_def("seas.len", "days", cdtNcdfDim, -99, "Length of the rainy season", "short", shuffle = TRUE, compression = 9)
	writeOUTPICSA("cdtSEASLEN", SEASON.LENGTH, nc.grd, 'season.length')

	rm(SEASON.LENGTH, nc.grd); gc()

	###################

	is.parallel <- doparallel(okparallel)
	`%dofun%` <- is.parallel$dofun
	toExports <- c("getIndexSeasonVarsRow", "doparallel", "readcdtDATAchunk")
	transPose <- if(length(opDATA$lon) > 1) t else as.matrix
	SEASON.IDX <- foreach(ii = seq_along(loopCOL), .export = toExports, .packages = "doParallel") %dofun% {
		ONSET <- readcdtDATAchunk(loopCOL[[ii]], cdtONSET$colInfo, file.path(outPICSAData, "cdtONSET"), chunksize, chunk.par = FALSE)
		CESSAT <- readcdtDATAchunk(loopCOL[[ii]], cdtCESSAT$colInfo, file.path(outPICSAData, "cdtCESSAT"), chunksize, chunk.par = FALSE)
		dimONSET <- dim(ONSET)
		ONSET <- format(as.Date(ONSET, origin = cdtONSET$days.since), "%Y%m%d")
		dim(ONSET) <- dimONSET
		CESSAT <- format(as.Date(CESSAT, origin = cdtCESSAT$days.since), "%Y%m%d")
		dim(CESSAT) <- dimONSET

		debfin.idx <- transPose(sapply(seq(length(cdtONSET$years)), function(j){
							getIndexSeasonVarsRow(ONSET[j, ], CESSAT[j, ], opDATA$dates, "daily")
						}))

		if(dekmonUse){
			debfin.seas <- transPose(sapply(seq(length(cdtONSET$years)), function(j){
								getIndexSeasonVarsRow(ONSET[j, ], CESSAT[j, ], opDATA$cdtPrecip1$dates, dekmonTs)
							}))
		}else debfin.seas <- NULL

		rm(ONSET, CESSAT); gc()
		return(list(days.idx = debfin.idx, seas.idx = debfin.seas))
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	debfin.idx <- do.call(cbind, lapply(SEASON.IDX, function(x) x$days.idx))
	lenDebFin <- sapply(debfin.idx, length)
	dim(lenDebFin) <- dim(debfin.idx)

	if(dekmonUse){
		debfin.seas <- do.call(cbind, lapply(SEASON.IDX, function(x) x$seas.idx))
		lenDebFinS <- sapply(debfin.seas, length)
		dim(lenDebFinS) <- dim(debfin.seas)
	}

	rm(SEASON.IDX)

	###################

	is.parallel <- doparallel(okparallel)
	`%dofun%` <- is.parallel$dofun
	toExports <- c( "doparallel", "readcdtDATAchunk", "cdtPrecip")
	NAFRAC <- foreach(ii = seq_along(loopCOL), .export = toExports, .packages = "doParallel") %dofun% {
		precip <- extractPICSArdsData1("cdtPrecip", loopCOL[[ii]], opDATA, outPICSAData, chunksize, chunk.par = FALSE)
		days.idx <- debfin.idx[, loopCOL[[ii]], drop = FALSE]

		nanb <- lapply(seq(ncol(days.idx)), function(j){
					sapply(days.idx[, j], function(x){
						y <- if(length(x) == 1 & all(is.na(x))) NA else precip[x, j]
						sum(is.na(y))
					})
				})
		nanb <- do.call(cbind, nanb)
		nafrac <- 1 - (nanb/lenDebFin[, loopCOL[[ii]], drop = FALSE])
		rm(precip, days.idx, nanb); gc()
		return(nafrac)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	NAFRAC <- do.call(cbind, NAFRAC)
	NAFRAC <- NAFRAC < 0.98
	rm(lenDebFin)

	if(dekmonUse){
		is.parallel <- doparallel(okparallel)
		`%dofun%` <- is.parallel$dofun
		toExports <- c("doparallel", "readcdtDATAchunk", "cdtPrecip1")
		NAFRACS <- foreach(ii = seq_along(loopCOL), .export = toExports, .packages = "doParallel") %dofun% {
			precip <- extractPICSArdsData1("cdtPrecip1", loopCOL[[ii]], opDATA, outPICSAData, chunksize, chunk.par = FALSE)
			days.idx <- debfin.seas[, loopCOL[[ii]], drop = FALSE]

			nanb <- lapply(seq(ncol(days.idx)), function(j){
						sapply(days.idx[, j], function(x){
							y <- if(length(x) == 1 & all(is.na(x))) NA else precip[x, j]
							sum(is.na(y))
						})
					})
			nanb <- do.call(cbind, nanb)
			nafrac <- 1 - (nanb/lenDebFinS[, loopCOL[[ii]], drop = FALSE])
			rm(precip, days.idx, nanb); gc()
			return(nafrac)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
		NAFRACS <- do.call(cbind, NAFRACS)
		NAFRACS <- NAFRACS < 0.98
		rm(lenDebFinS)
	}

	###################

	is.parallel <- doparallel(okparallel)
	`%dofun%` <- is.parallel$dofun
	toExports <- c("funAggrVEC", "doparallel", "readcdtDATAchunk")
	toExports1 <- if(dekmonUse) c(toExports, "cdtPrecip1") else c(toExports, "cdtPrecip")
	RAINTOTAL <- foreach(ii = seq_along(loopCOL), .export = toExports1, .packages = "doParallel") %dofun% {
		if(dekmonUse){
			days.idx <- debfin.seas[, loopCOL[[ii]], drop = FALSE]
			precip <- extractPICSArdsData1("cdtPrecip1", loopCOL[[ii]], opDATA, outPICSAData, chunksize, chunk.par = FALSE)
			nafrac <- NAFRACS[, loopCOL[[ii]], drop = FALSE]
		}else{
			days.idx <- debfin.idx[, loopCOL[[ii]], drop = FALSE]
			precip <- extractPICSArdsData1("cdtPrecip", loopCOL[[ii]], opDATA, outPICSAData, chunksize, chunk.par = FALSE)
			nafrac <- NAFRAC[, loopCOL[[ii]], drop = FALSE]
		}
		xtmp <- lapply(seq(ncol(days.idx)), function(j) sapply(days.idx[, j], funAggrVEC, DATA = precip[, j], pars = list(aggr.fun = "sum")))
		xtmp <- do.call(cbind, xtmp)
		xtmp[nafrac] <- NA
		rm(precip, nafrac, days.idx); gc()
		return(xtmp)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	RAINTOTAL <- do.call(cbind, RAINTOTAL)
	nc.grd <- NULL
	if(data.type == 'netcdf') nc.grd <- ncvar_def("seas.precip", "mm", cdtNcdfDim, -99, "Seasonal rainfall amounts", "short", shuffle = TRUE, compression = 9)
	writeOUTPICSA("cdtRAINTOTAL", RAINTOTAL, nc.grd, 'rain.total')
	rm(NAFRACS, RAINTOTAL, nc.grd); gc()

	###################

	if(compute.ETP == "temp"){
		is.parallel <- doparallel(okparallel)
		`%dofun%` <- is.parallel$dofun
		toExports <- c("funAggrVEC", "doparallel", "readcdtDATAchunk", "cdtTmax", "cdtTmin")
		TXTN <- foreach(ii = seq_along(loopCOL), .export = toExports, .packages = "doParallel") %dofun% {
			days.idx <- debfin.idx[, loopCOL[[ii]], drop = FALSE]

			tmax <- extractPICSArdsData1("cdtTmax", loopCOL[[ii]], opDATA, outPICSAData, chunksize, chunk.par = FALSE)
			tmax <- lapply(seq(ncol(days.idx)), function(j) sapply(days.idx[, j], funAggrVEC, DATA = tmax[, j], pars = list(aggr.fun = "mean")))
			tmax <- do.call(cbind, tmax)
			tmax[NAFRAC[, loopCOL[[ii]], drop = FALSE]] <- NA
			tmax <- round(tmax, 1)

			tmin <- extractPICSArdsData1("cdtTmin", loopCOL[[ii]], opDATA, outPICSAData, chunksize, chunk.par = FALSE)
			tmin <- lapply(seq(ncol(days.idx)), function(j) sapply(days.idx[, j], funAggrVEC, DATA = tmin[, j], pars = list(aggr.fun = "mean")))
			tmin <- do.call(cbind, tmin)
			tmin[NAFRAC[, loopCOL[[ii]], drop = FALSE]] <- NA
			tmin <- round(tmin, 1)
			list(tmax = tmax, tmin = tmin)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
		tmax <- do.call(cbind, lapply(TXTN, function(x) x$tmax))
		tmin <- do.call(cbind, lapply(TXTN, function(x) x$tmin))
		nc.tmax <- NULL
		if(data.type == 'netcdf') nc.tmax <- ncvar_def("tmax", "degC", cdtNcdfDim, -99, 'Seasonal average maximum temperature', "float", compression = 9)
		writeOUTPICSA("cdtTMAXSEAS", tmax, nc.tmax, 'tmax')
		nc.tmin <- NULL
		if(data.type == 'netcdf') nc.tmin <- ncvar_def("tmin", "degC", cdtNcdfDim, -99, 'Seasonal average minimum temperature', "float", compression = 9)
		writeOUTPICSA("cdtTMINSEAS", tmin, nc.tmin, 'tmin')
		rm(TXTN, tmax, nc.tmax, tmin, nc.tmin); gc()
	}

	###################
	is.parallel <- doparallel(okparallel)
	`%dofun%` <- is.parallel$dofun
	toExports <- c("NumberOfSpell.AllVEC", "doparallel", "readcdtDATAchunk", "cdtPrecip")
	DRYSPELLS <- foreach(ii = seq_along(loopCOL), .export = toExports, .packages = "doParallel") %dofun% {
		days.idx <- debfin.idx[, loopCOL[[ii]], drop = FALSE]
		precip <- extractPICSArdsData1("cdtPrecip", loopCOL[[ii]], opDATA, outPICSAData, chunksize, chunk.par = FALSE)

		xtmp <- lapply(seq(ncol(days.idx)), function(j) 
					lapply(days.idx[, j], NumberOfSpell.AllVEC, DATA = precip[, j], opr.fun = dryspellfun, opr.thres = thres.rain.day))
		xtmp <- do.call(cbind, xtmp)
		xtmp[NAFRAC[, loopCOL[[ii]], drop = FALSE]] <- NA
		rm(precip, days.idx); gc()
		return(xtmp)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	DRYSPELLS <- do.call(cbind, DRYSPELLS)

	dir.cdtDRYSPELLS <- file.path(outPICSAData, "cdtDRYSPELLS")
	dir.create(dir.cdtDRYSPELLS, showWarnings = FALSE, recursive = TRUE)
	colInfo <- writecdtDATAchunk(DRYSPELLS, dir.cdtDRYSPELLS, chunksize)
	rm(colInfo)
	saveRDS(DRYSPELLS, file = file.path(dir.cdtDRYSPELLS, "seas.rds"))

	DRYSPELL5 <- sapply(DRYSPELLS, function(x) sum(x >= 5))
	dim(DRYSPELL5) <- dim(DRYSPELLS)
	nc.grd <- NULL
	if(data.type == 'netcdf') nc.grd <- ncvar_def("dryspell5", "", cdtNcdfDim, -99, 'Number of Dry Spells', "short", shuffle = TRUE, compression = 9)
	writeOUTPICSA("cdtDRYSPELL5", DRYSPELL5, nc.grd, 'dryspell5', write.rds = FALSE)
	rm(DRYSPELL5)

	DRYSPELL7 <- sapply(DRYSPELLS, function(x) sum(x >= 7))
	dim(DRYSPELL7) <- dim(DRYSPELLS)
	nc.grd <- NULL
	if(data.type == 'netcdf') nc.grd <- ncvar_def("dryspell7", "", cdtNcdfDim, -99, 'Number of Dry Spells', "short", shuffle = TRUE, compression = 9)
	writeOUTPICSA("cdtDRYSPELL7", DRYSPELL7, nc.grd, 'dryspell7', write.rds = FALSE)
	rm(DRYSPELL7)

	DRYSPELL10 <- sapply(DRYSPELLS, function(x) sum(x >= 10))
	dim(DRYSPELL10) <- dim(DRYSPELLS)
	nc.grd <- NULL
	if(data.type == 'netcdf') nc.grd <- ncvar_def("dryspell10", "", cdtNcdfDim, -99, 'Number of Dry Spells', "short", shuffle = TRUE, compression = 9)
	writeOUTPICSA("cdtDRYSPELL10", DRYSPELL10, nc.grd, 'dryspell10', write.rds = FALSE)
	rm(DRYSPELL10)

	DRYSPELL15 <- sapply(DRYSPELLS, function(x) sum(x >= 15))
	dim(DRYSPELL15) <- dim(DRYSPELLS)
	nc.grd <- NULL
	if(data.type == 'netcdf') nc.grd <- ncvar_def("dryspell15", "", cdtNcdfDim, -99, 'Number of Dry Spells', "short", shuffle = TRUE, compression = 9)
	writeOUTPICSA("cdtDRYSPELL15", DRYSPELL15, nc.grd, 'dryspell15', write.rds = FALSE)
	rm(DRYSPELL15)

	DRYSPELLmax <- sapply(DRYSPELLS, function(x) max(x))
	dim(DRYSPELLmax) <- dim(DRYSPELLS)
	nc.grd <- NULL
	if(data.type == 'netcdf') nc.grd <- ncvar_def("dryspellmax", "days", cdtNcdfDim, -99, "Longest dry spell", "short", shuffle = TRUE, compression = 9)
	writeOUTPICSA("cdtDRYSPELLmax", DRYSPELLmax, nc.grd, 'dryspellmax', write.rds = FALSE)
	rm(DRYSPELLS, DRYSPELLmax, nc.grd)

	###################
	is.parallel <- doparallel(okparallel)
	`%dofun%` <- is.parallel$dofun
	toExports <- c("funAggrVEC", "doparallel", "readcdtDATAchunk", "cdtPrecip")
	NBRAINDAYS <- foreach(ii = seq_along(loopCOL), .export = toExports, .packages = "doParallel") %dofun% {
		days.idx <- debfin.idx[, loopCOL[[ii]], drop = FALSE]
		precip <- extractPICSArdsData1("cdtPrecip", loopCOL[[ii]], opDATA, outPICSAData, chunksize, chunk.par = FALSE)

		xtmp <- lapply(seq(ncol(days.idx)), function(j) sapply(days.idx[, j], funAggrVEC, DATA = precip[, j],
							pars = list(aggr.fun = "count", count.fun = nbdayfun, count.thres = thres.rain.day)))
		xtmp <- do.call(cbind, xtmp)
		xtmp[NAFRAC[, loopCOL[[ii]], drop = FALSE]] <- NA
		rm(precip, days.idx); gc()
		return(xtmp)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	NBRAINDAYS <- do.call(cbind, NBRAINDAYS)
	nc.grd <- NULL
	if(data.type == 'netcdf') nc.grd <- ncvar_def("nb.rain", "days", cdtNcdfDim, -99, "Seasonal number of rainy days", "short", shuffle = TRUE, compression = 9)
	writeOUTPICSA("cdtNBRAINDAYS", NBRAINDAYS, nc.grd, 'nb.rainy.days')
	rm(NBRAINDAYS, nc.grd); gc()

	###################
	is.parallel <- doparallel(okparallel)
	`%dofun%` <- is.parallel$dofun
	toExports <- c("funAggrVEC", "doparallel", "readcdtDATAchunk", "cdtPrecip")
	RAINMAX24H <- foreach(ii = seq_along(loopCOL), .export = toExports, .packages = "doParallel") %dofun% {
		days.idx <- debfin.idx[, loopCOL[[ii]], drop = FALSE]
		precip <- extractPICSArdsData1("cdtPrecip", loopCOL[[ii]], opDATA, outPICSAData, chunksize, chunk.par = FALSE)

		xtmp <- lapply(seq(ncol(days.idx)), function(j) sapply(days.idx[, j], funAggrVEC, DATA = precip[, j], pars = list(aggr.fun = "max")))
		xtmp <- do.call(cbind, xtmp)
		xtmp[NAFRAC[, loopCOL[[ii]], drop = FALSE]] <- NA
		rm(precip, days.idx); gc()
		return(xtmp)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	RAINMAX24H <- do.call(cbind, RAINMAX24H)
	nc.grd <- NULL
	if(data.type == 'netcdf') nc.grd <- ncvar_def("max24h", "mm", cdtNcdfDim, -99, 'Seasonal maximum of daily rainfall', "short", shuffle = TRUE, compression = 9)
	writeOUTPICSA("cdtRAINMAX24H", RAINMAX24H, nc.grd, 'max24h')
	rm(RAINMAX24H, nc.grd); gc()

	###################

	is.parallel <- doparallel(okparallel)
	`%dofun%` <- is.parallel$dofun
	toExports <- c("funAggrVEC", "quantile8", "doparallel", "readcdtDATAchunk", "cdtPrecip")
	Q95DATA <- foreach(ii = seq_along(loopCOL), .export = toExports, .packages = "doParallel") %dofun% {
		days.idx <- debfin.idx[, loopCOL[[ii]], drop = FALSE]
		precip <- extractPICSArdsData1("cdtPrecip", loopCOL[[ii]], opDATA, outPICSAData, chunksize, chunk.par = FALSE)

		xtmp <- lapply(seq(ncol(days.idx)), function(j) {
			seasRR <- precip[unlist(days.idx[, j]), j]
			q95th <- quantile8(seasRR[seasRR > thres.rain.day], probs = 0.95)
			nbQ95th <- sapply(days.idx[, j], funAggrVEC, DATA = precip[, j], pars = list(aggr.fun = "count", count.fun = ">", count.thres = q95th))
			TotQ95th <- sapply(days.idx[, j], function(x){
								x <- x[!is.na(x)]
								if(length(x) == 0) return(NA)
								VEC <- precip[x, j]
								VEC <- VEC[!is.na(VEC)]
								if(length(VEC) == 0) return(NA)
								sum(VEC[VEC > q95th])
							})
			list(q95 = q95th, nbq95 = nbQ95th, totq95 = TotQ95th)
		})

		Q95 <- sapply(xtmp, function(x) x$q95)
		nbQ95 <- do.call(cbind, lapply(xtmp, function(x) x$nbq95))
		nbQ95[NAFRAC[, loopCOL[[ii]], drop = FALSE]] <- NA
		totQ95 <- do.call(cbind, lapply(xtmp, function(x) x$totq95))
		totQ95[NAFRAC[, loopCOL[[ii]], drop = FALSE]] <- NA

		rm(precip, days.idx, xtmp); gc()
		return(list(q95 = Q95, nbq95 = nbQ95, totq95 = totQ95))
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	Q95th <- do.call(c, lapply(Q95DATA, function(x) x$q95))
	NbQ95th <- do.call(cbind, lapply(Q95DATA, function(x) x$nbq95))
	TotalQ95th <- do.call(cbind, lapply(Q95DATA, function(x) x$totq95))
	nc.nbq95 <- NULL
	if(data.type == 'netcdf') nc.nbq95 <- ncvar_def("nbq95th", "days", cdtNcdfDim, -99, 'Seasonal count of days when RR > 95th percentile', "short", shuffle = TRUE, compression = 9)
	writeOUTPICSA("cdtNBQ95TH", NbQ95th, nc.nbq95, 'nbq95th')
	nc.totq95 <- NULL
	if(data.type == 'netcdf') nc.totq95 <- ncvar_def("totq95th", "mm", cdtNcdfDim, -99, 'Seasonal total of precipitation when RR > 95th percentile', "short", shuffle = TRUE, compression = 9)
	writeOUTPICSA("cdtTOTQ95TH", TotalQ95th, nc.totq95, 'totq95th')
	rm(Q95DATA, NbQ95th, nc.nbq95, TotalQ95th, nc.totq95); gc()
	if(data.type == 'netcdf') rm(Q95th)

	###################

	InsertMessagesTxt(main.txt.out, 'Computing PICSA data done!')

	if(dekmonUse) rm(cdtPrecip1)
	if(compute.ETP == "temp") rm(cdtTmin, cdtTmax)
	rm(cdtPrecip, cdtETP, opDATA, cdtONSET, cdtCESSAT); gc()

	###########################################

	if(data.type == 'cdt' & GeneralParameters$JPEG$plot){
		InsertMessagesTxt(main.txt.out, 'Plot stations data ...')

		COLIDX <- seq_along(EnvPICSA$opDATA$lon)
		loopCOL <- split(COLIDX, ceiling(COLIDX/chunksize))

		is.parallel <- doparallel(length(loopCOL[[1]]) >= 10)
		`%parLoop%` <- is.parallel$dofun

		toExport1 <- c("fit.distributions", "startnorm", "startsnorm", "startlnorm", "startgamma", "startweibull")
		allfonct <- unlist(Map(function_name, Filter(is_function, parse(file.path(apps.dir, 'functions', 'PICSA_Plot_functions.R')))))
		toExport1 <- c(toExport1, allfonct, "quantile8", "writeFiles", "table.annuel", "getIndexSeasonVars",
						"EnvPICSAplot", "doparallel", "readcdtDATAchunk")

		###
		outPICSAdir <- file.path(outputDIR, "PICSA.OUT.Stations")
		dir.create(outPICSAdir, showWarnings = FALSE, recursive = TRUE)
		jpg.wcdt <- GeneralParameters$JPEG$width
		jpg.hcdt <- GeneralParameters$JPEG$height
		jpg.ucdt <- GeneralParameters$JPEG$units
		jpg.rcdt <- GeneralParameters$JPEG$res

		##################

		ret <- foreach(ii = seq_along(loopCOL)) %do% {
			ONSET <- readRDS(file.path(outPICSAData, "cdtONSET", paste0(ii, ".rds")))
			CESSAT <- readRDS(file.path(outPICSAData, "cdtCESSAT", paste0(ii, ".rds")))
			SEASLENGTH <- readRDS(file.path(outPICSAData, "cdtSEASLEN", paste0(ii, ".rds")))
			RAINTOTAL <- readRDS(file.path(outPICSAData, "cdtRAINTOTAL", paste0(ii, ".rds")))
			AllDrySpell <- readRDS(file.path(outPICSAData, "cdtDRYSPELLS", paste0(ii, ".rds")))
			MAX24H <- readRDS(file.path(outPICSAData, "cdtRAINMAX24H", paste0(ii, ".rds")))
			NBDRAIN <- readRDS(file.path(outPICSAData, "cdtNBRAINDAYS", paste0(ii, ".rds")))
			NBQ95TH <- readRDS(file.path(outPICSAData, "cdtNBQ95TH", paste0(ii, ".rds")))
			TOTQ95TH <- readRDS(file.path(outPICSAData, "cdtTOTQ95TH", paste0(ii, ".rds")))

			if(compute.ETP == "temp"){
				TMAX <- readRDS(file.path(outPICSAData, "cdtTMAXSEAS", paste0(ii, ".rds")))
				TMIN <- readRDS(file.path(outPICSAData, "cdtTMINSEAS", paste0(ii, ".rds")))
			}

			IDCOL <- loopCOL[[ii]]
			ret <- foreach(j = seq_along(IDCOL), .export = toExport1, .packages = c("stringr", "tools", "fitdistrplus", "doParallel")) %parLoop% {
				precip.id <- EnvPICSA$opDATA$id[EnvPICSA$cdtONSET$colInfo$id[IDCOL[j]]]
				stnDIR <- file.path(outPICSAdir, precip.id)
				dir.create(stnDIR, showWarnings = FALSE, recursive = TRUE)

				onset <- ONSET[, j]
				if(all(is.na(onset))) return(NULL)
				onset.dates <- format(as.Date(onset, origin = EnvPICSA$cdtONSET$days.since), "%Y%m%d")
				cessat <- CESSAT[, j]
				cessat.dates <- format(as.Date(cessat, origin = EnvPICSA$cdtCESSAT$days.since), "%Y%m%d")
				season.length <- SEASLENGTH[, j]
				RainTotal <- RAINTOTAL[, j]

				DrySpell5 <- sapply(AllDrySpell[, j], function(x) sum(x >= 5))
				DrySpell7 <- sapply(AllDrySpell[, j], function(x) sum(x >= 7))
				DrySpell10 <- sapply(AllDrySpell[, j], function(x) sum(x >= 10))
				DrySpell15 <- sapply(AllDrySpell[, j], function(x) sum(x >= 15))
				DrySpellMax <- sapply(AllDrySpell[, j], function(x) max(x))

				max24h <- MAX24H[, j]
				nbdayrain <- NBDRAIN[, j]
				q95 <- Q95th[EnvPICSA$cdtONSET$colInfo$id[IDCOL[j]]]
				NbQ95th <- NBQ95TH[, j]
				TotalQ95th <- TOTQ95TH[, j]

				yrsOnset <- EnvPICSA$cdtONSET$years
				onsetXS <- EnvPICSA$cdtONSET$start.labels
				yrsCesst <- EnvPICSA$cdtCESSAT$years
				cessatXS <- EnvPICSA$cdtCESSAT$start.labels

				## write data
				don.out <- data.frame(onset = onset.dates, cessation = cessat.dates, seasonLength = season.length, RainTotal = RainTotal,
									 DrySpell5 = DrySpell5, DrySpell7 = DrySpell7, DrySpell10 = DrySpell10, DrySpell15 = DrySpell15, DrySpellMax = DrySpellMax,
									 MaxDailyRain = max24h, NbRainDay = nbdayrain, Q95th = q95, ExtrmFreqQ95th = NbQ95th, RainTotalQ95th = TotalQ95th)
				if(compute.ETP == "temp"){
					tmax <- TMAX[, j]
					tmin <- TMIN[, j]
					don.out <- data.frame(don.out, tmax = tmax, tmin = tmin)
				}
				writeFiles(don.out, file.path(stnDIR, paste0(precip.id, '_out', '.csv')), col.names = TRUE)
				EnvPICSAplot$location <- paste0("Station: ", precip.id)

				## ONI 
				ijoni <- getIndexSeasonVars(onset.dates, cessat.dates, EnvPICSA$ONI$date, "monthly")
				oni.idx <- sapply(ijoni, function(x) mean(EnvPICSA$ONI$data[x], na.rm = TRUE))
				oni.idx <- ifelse(oni.idx >= 0.5, 3, ifelse(oni.idx <= -0.5, 1, 2))

				precip <- readcdtDATAchunk(EnvPICSA$opDATA$cdtPrecip$pid[IDCOL[j]], EnvPICSA$cdtPrecip$colInfo, file.path(outPICSAData, "cdtPrecip"), chunksize, chunk.par = FALSE)
				precip <- precip[EnvPICSA$opDATA$cdtPrecip$index, 1]
				daty <- EnvPICSA$opDATA$dates

				# if(Sys.info()["sysname"] == "Windows") jpeg(plotfile, restoreConsole = FALSE) else jpeg(plotfile)

				### daily rainfall
				jpeg(file.path(stnDIR, paste0(precip.id, '_daily_rain', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.daily(daty, precip, EnvPICSA$Pars$thres.rain.day, axis.font = 2)
				dev.off()

				### Rainfall amounts ENSO line
				jpeg(file.path(stnDIR, paste0(precip.id, '_rainfall.amount.ENSO_line', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.line.ENSO(yrsOnset, RainTotal, oni.idx, xlab = 'Year', ylab = 'Rainfall Amount (mm)', title = "Seasonal rainfall amounts", axis.font = 2, start.zero = TRUE)
				dev.off()

				### Rainfall amounts ENSO bar
				jpeg(file.path(stnDIR, paste0(precip.id, '_rainfall.amount.ENSO_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.bar.ENSO(yrsOnset, RainTotal, oni.idx, xlab = 'Year', ylab = 'Rainfall Amount (mm)', title = "Seasonal rainfall amounts", axis.font = 2, start.zero = TRUE)
				dev.off()

				### Rainfall amounts ENSO prob
				jpeg(file.path(stnDIR, paste0(precip.id, '_rainfall.amount.ENSO_prob', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.proba.ENSO(RainTotal, oni.idx, xlab = 'Rainfall Amount (mm)', title = "Seasonal rainfall amounts", axis.font = 2)
				dev.off()

				### Onset
				jpeg(file.path(stnDIR, paste0(precip.id, '_onset', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.line(yrsOnset, onset, origindate = onsetXS, title = "Starting dates of the rainy season", axis.font = 2, start.zero = TRUE)
				dev.off()

				### Onset1
				jpeg(file.path(stnDIR, paste0(precip.id, '_onset1', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.line(yrsOnset, onset, origindate = onsetXS, title = "Starting dates of the rainy season",
								mean = TRUE, tercile = TRUE, axis.font = 2, start.zero = TRUE)
				dev.off()

				### Cessation
				jpeg(file.path(stnDIR, paste0(precip.id, '_cessation', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.line(yrsCesst, cessat, origindate = cessatXS, title = "Ending dates of the rainy season", axis.font = 2, start.zero = TRUE)
				dev.off()

				### Cessation1
				jpeg(file.path(stnDIR, paste0(precip.id, '_cessation1', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.line(yrsCesst, cessat, origindate = cessatXS, title = "Ending dates of the rainy season",
								mean = TRUE, tercile = TRUE, axis.font = 2, start.zero = TRUE)
				dev.off()

				### Length of season
				jpeg(file.path(stnDIR, paste0(precip.id, '_Length.of.season', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.line(yrsOnset, season.length, xlab = 'Year', ylab = 'Number of Days',
								title = "Length of the rainy season", axis.font = 2, start.zero = TRUE)
				dev.off()

				### Length of season1
				jpeg(file.path(stnDIR, paste0(precip.id, '_Length.of.season', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.line(yrsOnset, season.length, xlab = 'Year', ylab = 'Number of Days',
								title = "Length of the rainy season", mean = TRUE, tercile = TRUE, axis.font = 2, start.zero = TRUE)
				dev.off()

				### Rainfall amounts line
				jpeg(file.path(stnDIR, paste0(precip.id, '_rainfall.amount_line1', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.line(yrsOnset, RainTotal, xlab = 'Year', ylab = 'Rainfall Amount (mm)',
								title = "Seasonal rainfall amounts", axis.font = 2, start.zero = TRUE)
				dev.off()

				### Rainfall amounts line
				jpeg(file.path(stnDIR, paste0(precip.id, '_rainfall.amount_line2', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.line(yrsOnset, RainTotal, xlab = 'Year', ylab = 'Rainfall Amount (mm)',
								title = "Seasonal rainfall amounts", mean = TRUE, tercile = TRUE, axis.font = 2, start.zero = TRUE)
				dev.off()

				### Rainfall amounts bar
				jpeg(file.path(stnDIR, paste0(precip.id, '_rainfall.amount_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.bar(yrsOnset, RainTotal, xlab = 'Year', ylab = 'Rainfall Amount (mm)',
								title = "Seasonal rainfall amounts", axis.font = 2, start.zero = TRUE)
				dev.off()

				### Rainfall amounts prob
				jpeg(file.path(stnDIR, paste0(precip.id, '_rainfall.amount_prob', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.proba(RainTotal, xlab = "Rainfall Amount (mm)", title = "Seasonal rainfall amounts", theoretical = TRUE, axis.font = 2)
				dev.off()

				## Dry Spell 5 line
				jpeg(file.path(stnDIR, paste0(precip.id, '_dry.spell.5days_line', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.line(yrsOnset, DrySpell5, sub = "Dry spells - 5 or more consecutive days", xlab = 'Year',
								ylab = 'Number of Dry Spells', title = "Dry Spells", axis.font = 2, start.zero = TRUE)
				dev.off()
				## Dry Spell 5 bar
				jpeg(file.path(stnDIR, paste0(precip.id, '_dry.spell.5days_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.bar(yrsOnset, DrySpell5, sub = "Dry spells - 5 or more consecutive days", xlab = 'Year', ylab = 'Number of Dry Spells',
								title = "Dry Spells", barcol = "slateblue4", axis.font = 2, start.zero = TRUE)
				dev.off()
				## Dry Spell 5 ecdf
				jpeg(file.path(stnDIR, paste0(precip.id, '_dry.spell.5days_prob', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.proba(DrySpell5, sub = "Dry spells - 5 or more consecutive days", xlab = "Number of Dry Spells", title = "Dry Spells", axis.font = 2)
				dev.off()

				## Dry Spell 7 line
				jpeg(file.path(stnDIR, paste0(precip.id, '_dry.spell.7days_line', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.line(yrsOnset, DrySpell7, sub = "Dry spells - 7 or more consecutive days", xlab = 'Year',
								ylab = 'Number of Dry Spells', title = "Dry Spells", axis.font = 2, start.zero = TRUE)
				dev.off()
				## Dry Spell 7 bar
				jpeg(file.path(stnDIR, paste0(precip.id, '_dry.spell.7days_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.bar(yrsOnset, DrySpell7, sub = "Dry spells - 7 or more consecutive days", xlab = 'Year', ylab = 'Number of Dry Spells',
								title = "Dry Spells", barcol = "slateblue4", axis.font = 2, start.zero = TRUE)
				dev.off()
				## Dry Spell 7 ecdf
				jpeg(file.path(stnDIR, paste0(precip.id, '_dry.spell.7days_prob', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.proba(DrySpell7, sub = "Dry spells - 7 or more consecutive days", xlab = "Number of Dry Spells", title = "Dry Spells", axis.font = 2)
				dev.off()

				## Dry Spell 10 line
				jpeg(file.path(stnDIR, paste0(precip.id, '_dry.spell.10days_line', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.line(yrsOnset, DrySpell10, sub = "Dry spells - 10 or more consecutive days", xlab = 'Year',
								ylab = 'Number of Dry Spells', title = "Dry Spells", axis.font = 2, start.zero = TRUE)
				dev.off()
				## Dry Spell 10 bar
				jpeg(file.path(stnDIR, paste0(precip.id, '_dry.spell.10days_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.bar(yrsOnset, DrySpell10, sub = "Dry spells - 10 or more consecutive days", xlab = 'Year', ylab = 'Number of Dry Spells',
								title = "Dry Spells", barcol = "slateblue4", axis.font = 2, start.zero = TRUE)
				dev.off()
				## Dry Spell 10 ecdf
				jpeg(file.path(stnDIR, paste0(precip.id, '_dry.spell.10days_prob', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.proba(DrySpell10, sub = "Dry spells - 10 or more consecutive days", xlab = "Number of Dry Spells", title = "Dry Spells", axis.font = 2)
				dev.off()

				## longest dry spell line
				jpeg(file.path(stnDIR, paste0(precip.id, '_longest.dry.spell_line', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.line(yrsOnset, DrySpellMax, xlab = 'Year', ylab = 'Number of Days',
								title = "Longest dry spell", mean = TRUE, axis.font = 2, start.zero = TRUE)
				dev.off()

				## longest dry spell bar
				jpeg(file.path(stnDIR, paste0(precip.id, '_longest.dry.spell_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.bar(yrsOnset, DrySpellMax, xlab = 'Year', ylab = 'Number of Days',
								title = "Longest dry spell", axis.font = 2, start.zero = TRUE)
				dev.off()

				## Rainy days line
				jpeg(file.path(stnDIR, paste0(precip.id, '_rainy.days_line', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.line(yrsOnset, nbdayrain, xlab = 'Year', ylab = 'Number of Days', title = "Seasonal number of rainy days",
									col = list(line = 'darkgreen', points = "lightblue"), mean = TRUE, axis.font = 2, start.zero = TRUE)
				dev.off()

				## Rainy days bar
				jpeg(file.path(stnDIR, paste0(precip.id, '_rainy.days_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.bar(yrsOnset, nbdayrain, xlab = 'Year', ylab = 'Number of Days', title = "Seasonal number of rainy days",
								barcol = "darkgreen", axis.font = 2, start.zero = TRUE)
				dev.off()

				## max 24h line
				jpeg(file.path(stnDIR, paste0(precip.id, '_max.1-day.precip_line', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.line(yrsOnset, max24h, xlab = 'Year', ylab = 'Rainfall Depth (mm)', title = 'Seasonal maximum of daily rainfall',
								 col = list(line = 'turquoise4', points = "lightblue"), mean = TRUE, axis.font = 2, start.zero = TRUE)
				dev.off()

				## max 24h bar
				jpeg(file.path(stnDIR, paste0(precip.id, '_max.1-day.precip_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				picsa.plot.bar(yrsOnset, max24h, xlab = 'Year', ylab = 'Rainfall Depth (mm)', title = 'Seasonal maximum of daily rainfall',
								barcol = "turquoise4", axis.font = 2, start.zero = TRUE)
				dev.off()

				## Number of rainy days above 95th percentile line
				jpeg(file.path(stnDIR, paste0(precip.id, '_nb.precip.gt.95th.perc_line', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				op <- par(mar = c(5.1, 5.1, 3.1, 2.1))
				picsa.plot.line(yrsOnset, NbQ95th, xlab = 'Year', ylab = 'Number of Days', title = 'Seasonal count of days when RR > 95th percentile',
									col = list(line = 'darkgreen', points = "lightblue"), mean = TRUE, axis.font = 2, start.zero = TRUE)
				par(op)
				dev.off()

				## Number of rainy days above 95th percentile bar
				jpeg(file.path(stnDIR, paste0(precip.id, '_nb.precip.gt.95th.perc_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				op <- par(mar = c(5.1, 5.1, 3.1, 2.1))
				picsa.plot.bar(yrsOnset, NbQ95th, xlab = 'Year', ylab = 'Number of Days', title = 'Seasonal count of days when RR > 95th percentile',
								barcol = "darkgreen", axis.font = 2, start.zero = TRUE)
				par(op)
				dev.off()

				## Seasonal total of rain above 95th percentile bar
				jpeg(file.path(stnDIR, paste0(precip.id, '_total.precip.gt.95th.perc_bar', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
				op <- par(mar = c(5.1, 5.1, 3.1, 2.1))
				picsa.plot.bar(yrsOnset, TotalQ95th, xlab = 'Year', ylab = 'Rainfall Amount (mm)', title = 'Seasonal total of precipitation when RR > 95th percentile',
								barcol = "darkgreen", axis.font = 2, start.zero = TRUE)
				par(op)
				dev.off()

				## tmax & tmin
				if(compute.ETP == "temp"){
					jpeg(file.path(stnDIR, paste0(precip.id, '_tmax.tmin_line', '.jpg')), width = jpg.wcdt, height = jpg.hcdt, units = jpg.ucdt, res = jpg.rcdt)
					picsa.plot.TxTn(yrsOnset, tmax, tmin, axis.font = 2)
					dev.off()
					rm(tmax, tmin)
				}
				rm(onset, onset.dates, cessat, cessat.dates, season.length, RainTotal,
					DrySpell5, DrySpell7, DrySpell10, DrySpell15, DrySpellMax, max24h,
					nbdayrain, NbQ95th, TotalQ95th, precip, daty); gc()
			}

			if(compute.ETP == "temp") rm(TMAX, TMIN)
			rm(ONSET, CESSAT, SEASLENGTH, RAINTOTAL, AllDrySpell, MAX24H, NBDRAIN, NBQ95TH, TOTQ95TH); gc()
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
		InsertMessagesTxt(main.txt.out, 'Plot done!')
	}

	##################

	return(0)

}
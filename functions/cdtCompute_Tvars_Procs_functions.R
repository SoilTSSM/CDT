computeTvarsProcs <- function(GeneralParameters){
	InsertMessagesTxt(main.txt.out, paste("Compute", GeneralParameters$Tstep, tolower(GeneralParameters$variable), "temperature ......"))

	if(GeneralParameters$data.type == "cdtstation"){
		tmin <- getStnOpenData(GeneralParameters$cdtstation$tmin)
		if(is.null(tmin)) return(NULL)
		tmin <- getCDTdataAndDisplayMsg(tmin, GeneralParameters$Tstep)
		if(is.null(tmin)) return(NULL)

		tmax <- getStnOpenData(GeneralParameters$cdtstation$tmax)
		if(is.null(tmax)) return(NULL)
		tmax <- getCDTdataAndDisplayMsg(tmax, GeneralParameters$Tstep)
		if(is.null(tmax)) return(NULL)

		if(!any(tmin$id%in%tmax$id)){
			InsertMessagesTxt(main.txt.out, "Tmin & Tmax stations do not match", format = TRUE)
			return(NULL)
		}

		if(!any(tmin$dates%in%tmax$dates)){
			InsertMessagesTxt(main.txt.out, "Tmin & Tmax dates do not match", format = TRUE)
			return(NULL)
		}

		##################
		idaty <- match(tmin$dates, tmax$dates)
		idaty <- idaty[!is.na(idaty)]
		daty <- tmax$dates[idaty]
		tmax$data <- tmax$data[idaty, , drop = FALSE]
		tmin$data <- tmin$data[tmin$dates%in%tmax$dates, , drop = FALSE]

		##################
		id <- match(tmin$id, tmax$id)
		id <- id[!is.na(id)]
		stn.id <- tmax$id[id]
		stn.lon <- tmax$lon[id]
		stn.lat <- tmax$lat[id]
		tmax$data <- tmax$data[, id, drop = FALSE]
		tmin$data <- tmin$data[, tmin$id%in%tmax$id, drop = FALSE]

		##################
		if(GeneralParameters$variable == "Mean") outdon <- (tmax$data + tmin$data)/2
		if(GeneralParameters$variable == "Range") outdon <- tmax$data - tmin$data
		outdon <- round(outdon, 1)

		##################
		outdon[is.na(outdon)] <- -99
		xhead <- rbind(stn.id, stn.lon, stn.lat)
		chead <- c('ID.STN', 'LON', paste0(toupper(GeneralParameters$Tstep), '/LAT'))
		infohead <- cbind(chead, xhead)

		outdon <- rbind(infohead, cbind(daty, outdon))
		writeFiles(outdon, GeneralParameters$output)
		rm(tmin, tmax, outdon)
	}

	#######################################################
	if(GeneralParameters$data.type == "cdtnetcdf"){
		tnDataInfo <- getRFESampleData(GeneralParameters$cdtnetcdf$tmin$sample)
		if(is.null(tnDataInfo)){
			InsertMessagesTxt(main.txt.out, "No Tmin data sample found", format = TRUE)
			return(NULL)
		}
		txDataInfo <- getRFESampleData(GeneralParameters$cdtnetcdf$tmax$sample)
		if(is.null(txDataInfo)){
			InsertMessagesTxt(main.txt.out, "No Tmax data sample found", format = TRUE)
			return(NULL)
		}

		##################
		SP1 <- defSpatialPixels(tnDataInfo[c('lon', 'lat')])
		SP2 <- defSpatialPixels(txDataInfo[c('lon', 'lat')])
		if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
			InsertMessagesTxt(main.txt.out, "Tmin & Tmax have different resolution or bbox", format = TRUE)
			return(NULL)
		}
		rm(SP1, SP2)

		##################
		tstep <- GeneralParameters$Tstep
		# start.year <- GeneralParameters$date.range$start.year
		# start.mon <- GeneralParameters$date.range$start.mon
		# start.dek <- GeneralParameters$date.range$start.dek
		# end.year <- GeneralParameters$date.range$end.year
		# end.mon <- GeneralParameters$date.range$end.mon
		# end.dek <- GeneralParameters$date.range$end.dek
		months <- 1:12
		# start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
		# end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')
		start.date <- as.Date("1900-1-1")
		end.date <- as.Date("2050-12-31")

		tmin.DIR <- GeneralParameters$cdtnetcdf$tmin$dir
		tmin.Format <- GeneralParameters$cdtnetcdf$tmin$format
		tmin.errmsg <- "Tmin data not found"
		tminInfo <- ncFilesInfo(tstep, start.date, end.date, months, tmin.DIR, tmin.Format, tmin.errmsg)
		if(is.null(tminInfo)) return(NULL)
		tminInfo$ncinfo <- list(xo = tnDataInfo$rfeILon, yo = tnDataInfo$rfeILat, varid = tnDataInfo$rfeVarid)

		tminInfo$dates <- tminInfo$dates[tminInfo$exist]
		tminInfo$nc.files <- tminInfo$nc.files[tminInfo$exist]
		tminInfo$exist <- tminInfo$exist[tminInfo$exist]

		tmax.DIR <- GeneralParameters$cdtnetcdf$tmax$dir
		tmax.Format <- GeneralParameters$cdtnetcdf$tmax$format
		tmax.errmsg <- "Tmax data not found"
		tmaxInfo <- ncFilesInfo(tstep, start.date, end.date, months, tmax.DIR, tmax.Format, tmax.errmsg)
		if(is.null(tmaxInfo)) return(NULL)
		tmaxInfo$ncinfo <- list(xo = txDataInfo$rfeILon, yo = txDataInfo$rfeILat, varid = txDataInfo$rfeVarid)

		tmaxInfo$dates <- tmaxInfo$dates[tmaxInfo$exist]
		tmaxInfo$nc.files <- tmaxInfo$nc.files[tmaxInfo$exist]
		tmaxInfo$exist <- tmaxInfo$exist[tmaxInfo$exist]

		##################

		if(!any(tminInfo$dates%in%tmaxInfo$dates)){
			InsertMessagesTxt(main.txt.out, "Tmin & Tmax dates do not match", format = TRUE)
			return(NULL)
		}

		##################

		tndaty <- match(tmaxInfo$dates, tminInfo$dates)
		tndaty <- tndaty[!is.na(tndaty)]
		tminInfo$dates <- tminInfo$dates[tndaty]
		tminInfo$nc.files <- tminInfo$nc.files[tndaty]
		tminInfo$exist <- tminInfo$exist[tndaty]

		txdaty <- tmaxInfo$dates%in%tminInfo$dates
		tmaxInfo$dates <- tmaxInfo$dates[txdaty]
		tmaxInfo$nc.files <- tmaxInfo$nc.files[txdaty]
		tmaxInfo$exist <- tmaxInfo$exist[txdaty]

		##################
		nc <- nc_open(tminInfo$nc.files[1])
		nc.lon <- nc$dim[[tminInfo$ncinfo$xo]]$vals
		nc.lat <- nc$dim[[tminInfo$ncinfo$yo]]$vals
		varInfo <- nc$var[[tminInfo$ncinfo$varid]][c('prec', 'units', 'missval')]
		nc_close(nc)

		xo.tn <- order(nc.lon)
		nc.lon <- nc.lon[xo.tn]
		yo.tn <- order(nc.lat)
		nc.lat <- nc.lat[yo.tn]
		len.lon <- length(nc.lon)
		len.lat <- length(nc.lat)

		nc <- nc_open(tmaxInfo$nc.files[1])
		nc.lon1 <- nc$dim[[tmaxInfo$ncinfo$xo]]$vals
		nc.lat1 <- nc$dim[[tmaxInfo$ncinfo$yo]]$vals
		nc_close(nc)
		xo.tx <- order(nc.lon1)
		yo.tx <- order(nc.lat1)

		##################
		dx <- ncdim_def("Lon", "degreeE", nc.lon)
		dy <- ncdim_def("Lat", "degreeN", nc.lat)
		xy.dim <- list(dx, dy)

		if(GeneralParameters$variable == "Mean"){
			ncname <- "tmean"
			longname <- paste(str_to_title(GeneralParameters$Tstep), "mean temperature")
		}
		if(GeneralParameters$variable == "Range"){
			ncname <- "range"
			longname <- paste(str_to_title(GeneralParameters$Tstep), "temperature range")
		}
		grdNC <- ncvar_def(ncname, varInfo$units, xy.dim, varInfo$missval, longname = longname, prec = varInfo$prec, compression = 6)

		##################
		ncdir <- paste0("TEMP_", GeneralParameters$variable, "_", GeneralParameters$Tstep)
		outNCDIR <- file.path(GeneralParameters$output, ncdir)
		dir.create(outNCDIR, showWarnings = FALSE, recursive = TRUE)
		ncprefix <- paste0("temp_", tolower(GeneralParameters$variable))

		##################

		is.parallel <- doparallel(length(tminInfo$nc.files) >= 180)
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(jj = seq_along(tminInfo$nc.files), .packages = "ncdf4") %parLoop% {
			nc <- try(nc_open(tminInfo$nc.files[jj]), silent = TRUE)
			if(inherits(nc, "try-error")) return(NULL)
			tmin <- ncvar_get(nc, varid = tminInfo$ncinfo$varid)
			nc_close(nc)
			tmin <- tmin[xo.tn, yo.tn]
			if(tminInfo$ncinfo$yo < tminInfo$ncinfo$xo)
				tmin <- matrix(c(tmin), nrow = len.lon, ncol = len.lat, byrow = TRUE)

			##################
			nc <- try(nc_open(tmaxInfo$nc.files[jj]), silent = TRUE)
			if(inherits(nc, "try-error")) return(NULL)
			tmax <- ncvar_get(nc, varid = tmaxInfo$ncinfo$varid)
			nc_close(nc)
			tmax <- tmax[xo.tx, yo.tx]
			if(tmaxInfo$ncinfo$yo < tmaxInfo$ncinfo$xo)
				tmax <- matrix(c(tmax), nrow = len.lon, ncol = len.lat, byrow = TRUE)

			##################
			if(GeneralParameters$variable == "Mean") outdon <- (tmax + tmin)/2
			if(GeneralParameters$variable == "Range") outdon <- tmax - tmin
			outdon[is.na(outdon)] <- varInfo$missval

			##################
			outfile <- file.path(outNCDIR, paste0(ncprefix, "_", tminInfo$dates[jj], ".nc"))
			nc <- nc_create(outfile, grdNC)
			ncvar_put(nc, grdNC, outdon)
			nc_close(nc)

			rm(tmin, tmax, outdon); gc()
			return(0)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
		rm(tnDataInfo, txDataInfo, tminInfo, tmaxInfo)
	}

	#######################################################
	if(GeneralParameters$data.type == "cdtdataset"){
		tmin <- try(readRDS(GeneralParameters$cdtdataset$tmin), silent = TRUE)
		if(inherits(tmin, "try-error")){
			InsertMessagesTxt(main.txt.out, paste("Unable to read", GeneralParameters$cdtdataset$tmin), format = TRUE)
			return(NULL)
		}
		if(GeneralParameters$Tstep != tmin$TimeStep){
			InsertMessagesTxt(main.txt.out, paste("Tmin dataset is not a", GeneralParameters$Tstep, "data"), format = TRUE)
			return(NULL)
		}

		tmax <- try(readRDS(GeneralParameters$cdtdataset$tmax), silent = TRUE)
		if(inherits(tmax, "try-error")){
			InsertMessagesTxt(main.txt.out, paste("Unable to read", GeneralParameters$cdtdataset$tmax), format = TRUE)
			return(NULL)
		}
		if(GeneralParameters$Tstep != tmax$TimeStep){
			InsertMessagesTxt(main.txt.out, paste("Tmax dataset is not a", GeneralParameters$Tstep, "data"), format = TRUE)
			return(NULL)
		}

		##################
		SP1 <- defSpatialPixels(list(lon = tmin$coords$mat$x, lat = tmin$coords$mat$y))
		SP2 <- defSpatialPixels(list(lon = tmax$coords$mat$x, lat = tmax$coords$mat$y))
		if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
			InsertMessagesTxt(main.txt.out, "Tmin & Tmax have different resolution or bbox", format = TRUE)
			return(NULL)
		}
		rm(SP1, SP2)

		##################
		if(tmin$chunksize != tmax$chunksize){
			InsertMessagesTxt(main.txt.out, "Tmin & Tmax have different chunk size", format = TRUE)
			return(NULL)
		}

		##################
		if(!any(tmin$dateInfo$date%in%tmax$dateInfo$date)){
			InsertMessagesTxt(main.txt.out, "Tmin & Tmax dates do not match", format = TRUE)
			return(NULL)
		}

		txdaty <- match(tmin$dateInfo$date, tmax$dateInfo$date)
		txdaty <- txdaty[!is.na(txdaty)]
		tmax$dateInfo$date <- tmax$dateInfo$date[txdaty]
		tmax$dateInfo$index <- tmax$dateInfo$index[txdaty]

		tndaty <- tmin$dateInfo$date%in%tmax$dateInfo$date
		tmin$dateInfo$date <- tmin$dateInfo$date[tndaty]
		tmin$dateInfo$index <- tmin$dateInfo$index[tndaty]

		##################
		if(GeneralParameters$variable == "Mean"){
			ncname <- "tmean"
			longname <- paste(str_to_title(GeneralParameters$Tstep), "mean temperature")
		}
		if(GeneralParameters$variable == "Range"){
			ncname <- "range"
			longname <- paste(str_to_title(GeneralParameters$Tstep), "temperature range")
		}
		index.out <- tmin
		index.out$varInfo$name <- ncname
		index.out$varInfo$longname <- longname

		dataset <- paste0("TEMP_", GeneralParameters$variable, "_", GeneralParameters$Tstep)
		outDIR <- file.path(GeneralParameters$output, dataset)
		dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

		datadir <- file.path(outDIR, 'DATA')
		dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
		datafileIdx <- file.path(outDIR, paste0(dataset, '.rds'))

		##################

		chunkfile <- sort(unique(tmin$colInfo$index))
		chunkcalc <- split(chunkfile, ceiling(chunkfile/tmin$chunkfac))

		do.parChunk <- if(tmin$chunkfac > length(chunkcalc)) TRUE else FALSE
		do.parCALC <- if(do.parChunk) FALSE else TRUE

		toExports <- c("readCdtDatasetChunk.sequence", "writeCdtDatasetChunk.sequence", "doparallel")

		is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 5))
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(j = seq_along(chunkcalc), .export = toExports, .packages = "doParallel") %parLoop% {
			tn <- readCdtDatasetChunk.sequence(chunkcalc[[j]], GeneralParameters$cdtdataset$tmin, do.par = do.parChunk)
			tn <- tn[tmin$dateInfo$index, , drop = FALSE]

			tx <- readCdtDatasetChunk.sequence(chunkcalc[[j]], GeneralParameters$cdtdataset$tmax, do.par = do.parChunk)
			tx <- tx[tmax$dateInfo$index, , drop = FALSE]

			if(GeneralParameters$variable == "Mean") outdon <- (tx + tn)/2
			if(GeneralParameters$variable == "Range") outdon <- tx - tn

			writeCdtDatasetChunk.sequence(outdon, chunkcalc[[j]], index.out, datadir, do.par = do.parChunk)

			rm(tn, tx, outdon); gc()
			return(0)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

		##################

		# index.out$dateInfo$date <- tmin$dateInfo$date
		index.out$dateInfo$index <- seq(length(tmin$dateInfo$date))

		con <- gzfile(datafileIdx, compression = 6)
		open(con, "wb")
		saveRDS(index.out, con)
		close(con)

		rm(tmin, tmax, index.out)
	}

	return(0)
}



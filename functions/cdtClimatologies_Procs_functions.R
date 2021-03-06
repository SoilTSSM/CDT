
climatologiesCalcProcs <- function(GeneralParameters){
	if(!dir.exists(GeneralParameters$out.dir)){
		InsertMessagesTxt(main.txt.out, paste(GeneralParameters$out.dir, "did not find"), format = TRUE)
		return(NULL)
	}

	year1 <- GeneralParameters$climato$start
	year2 <- GeneralParameters$climato$end
	minyear <- GeneralParameters$climato$minyear
	freqData <- GeneralParameters$intstep
	xwin <- GeneralParameters$climato$window

	if(any(is.na(c(year1, year2, minyear)))){
		InsertMessagesTxt(main.txt.out, "Invalid base period", format = TRUE)
		return(NULL)
	}

	#####################################################

	if(GeneralParameters$data.type == "cdtstation"){
		don <- getStnOpenData(GeneralParameters$cdtstation$file)
		if(is.null(don)) return(NULL)
		don <- getCDTdataAndDisplayMsg(don, freqData)
		if(is.null(don)) return(NULL)

		daty <- don$dates
		year <- as.numeric(substr(daty, 1, 4))

		if(length(unique(year)) < minyear){
			InsertMessagesTxt(main.txt.out, "No enough data to calculate climatologies", format = TRUE)
			return(NULL)
		}

		### Clim
		iyear <- year >= year1 & year <= year2
		daty <- daty[iyear]
		don$data <- don$data[iyear, , drop = FALSE]
		index <- getClimatologiesIndex(daty, freqData, xwin)

		## Check for each tstep
		div <- if(freqData == "daily") 2*xwin+1 else 1
		Tstep.miss <- (sapply(index$index, length)/div) < minyear

		dat.clim <- lapply(seq_along(index$id), function(jj){
			if(Tstep.miss[jj]){
				tmp <- rep(NA, ncol(don$data))
				 return(list(moy = tmp, sds = tmp))
			}
			xx <- don$data[index$index[[jj]], , drop = FALSE]
			ina <- (colSums(!is.na(xx))/div) <  minyear
			moy <- colMeans(xx, na.rm = TRUE)
			sds <- matrixStats::colSds(xx, na.rm = TRUE)
			moy[ina] <- NA
			sds[ina] <- NA
			rm(xx)
			list(moy = moy, sds = sds)
		})

		dat.moy <- do.call(rbind, lapply(dat.clim, "[[", "moy"))
		dat.sds <- do.call(rbind, lapply(dat.clim, "[[", "sds"))
		dat.moy <- round(dat.moy, 1)
		dat.sds <- round(dat.sds, 1)

		rm(dat.clim)

		#########################################

		outDIR <- file.path(GeneralParameters$out.dir, "CLIMATOLOGY_data")
		dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)
		out.dat.index <- gzfile(file.path(outDIR, "Climatology.rds"), compression = 7)

		datadir <- file.path(outDIR, 'CDTSTATIONS')
		dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
		out.cdt.clim.moy <- file.path(datadir, paste0(freqData, "_Climatology_mean.csv"))
		out.cdt.clim.sds <- file.path(datadir, paste0(freqData, "_Climatology_std.csv"))

		dataOUT1 <- file.path(outDIR, 'CDTMEAN')
		dir.create(dataOUT1, showWarnings = FALSE, recursive = TRUE)
		out.cdt.moy <- gzfile(file.path(dataOUT1, 'CDTMEAN.rds'), compression = 7)

		dataOUT2 <- file.path(outDIR, 'CDTSTD')
		dir.create(dataOUT2, showWarnings = FALSE, recursive = TRUE)
		out.cdt.sds <- gzfile(file.path(dataOUT2, 'CDTSTD.rds'), compression = 7)

		##################

		output <- list(params = GeneralParameters,
					data = list(id = don$id, lon = don$lon, lat = don$lat),
					index = index$id)

		EnvClimatoCalcPlot$output <- output
		EnvClimatoCalcPlot$PathClim <- outDIR

		saveRDS(output, out.dat.index)
		saveRDS(dat.moy, out.cdt.moy)
		saveRDS(dat.sds, out.cdt.sds)

		##################

		xhead <- rbind(don$id, don$lon, don$lat)
		chead <- c('ID.STN', 'LON', 'INDEX/LAT')
		infohead <- cbind(chead, xhead)

		dat.moy[is.na(dat.moy)] <- -99
		dat.moy <- rbind(infohead, cbind(index$id, dat.moy))
		writeFiles(dat.moy, out.cdt.clim.moy)

		dat.sds[is.na(dat.sds)] <- -99
		dat.sds <- rbind(infohead, cbind(index$id, dat.sds))
		writeFiles(dat.sds, out.cdt.clim.sds)

		rm(dat.moy, dat.sds, output, don)
	}

	#####################################################

	if(GeneralParameters$data.type == "cdtdataset"){
		don <- try(readRDS(GeneralParameters$cdtdataset$index), silent = TRUE)
		if(inherits(don, "try-error")){
			InsertMessagesTxt(main.txt.out, paste("Unable to read", GeneralParameters$cdtdataset$index), format = TRUE)
			return(NULL)
		}
		if(freqData != don$TimeStep){
			InsertMessagesTxt(main.txt.out, paste("The dataset is not a", freqData, "data"), format = TRUE)
			return(NULL)
		}

		daty <- don$dateInfo$date
		year <- as.numeric(substr(daty, 1, 4))

		if(length(unique(year)) < minyear){
			InsertMessagesTxt(main.txt.out, "No enough data to calculate climatologies", format = TRUE)
			return(NULL)
		}

		### Clim
		iyear <- year >= year1 & year <= year2
		daty <- daty[iyear]
		index <- getClimatologiesIndex(daty, freqData, xwin)

		## Check for each tstep
		div <- if(freqData == "daily") 2*xwin+1 else 1
		Tstep.miss <- (sapply(index$index, length)/div) < minyear

		#########################################

		outDIR <- file.path(GeneralParameters$out.dir, "CLIMATOLOGY_data")
		dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)
		out.dat.index <- gzfile(file.path(outDIR, "Climatology.rds"), compression = 7)

		ncdfOUT1 <- file.path(outDIR, 'DATA_NetCDF', 'CDTMEAN')
		dir.create(ncdfOUT1, showWarnings = FALSE, recursive = TRUE)
		ncdfOUT2 <- file.path(outDIR, 'DATA_NetCDF', 'CDTSTD')
		dir.create(ncdfOUT2, showWarnings = FALSE, recursive = TRUE)

		datadir1 <- file.path(outDIR, 'CDTMEAN', 'DATA')
		dir.create(datadir1, showWarnings = FALSE, recursive = TRUE)
		file.index1 <- file.path(outDIR, 'CDTMEAN', 'CDTMEAN.rds')

		datadir2 <- file.path(outDIR, 'CDTSTD', 'DATA')
		dir.create(datadir2, showWarnings = FALSE, recursive = TRUE)
		file.index2 <- file.path(outDIR, 'CDTSTD', 'CDTSTD.rds')

		##################

		output <- list(params = GeneralParameters, index = index$id)

		EnvClimatoCalcPlot$output <- output
		EnvClimatoCalcPlot$PathClim <- outDIR

		saveRDS(output, out.dat.index)

		##################

		index.out <- don
		index.out$varInfo$longname <- paste(freqData, "climatology from:", don$varInfo$longname)

		index.out$dateInfo$date <- index$id
		index.out$dateInfo$index <- seq_along(index$id)

		saveRDS(index.out, gzfile(file.index1, compression = 7))
		saveRDS(index.out, gzfile(file.index2, compression = 7))

		#########################################

		chunkfile <- sort(unique(don$colInfo$index))
		chunkcalc <- split(chunkfile, ceiling(chunkfile/don$chunkfac))

		do.parChunk <- if(don$chunkfac > length(chunkcalc)) TRUE else FALSE
		do.parCALC <- if(do.parChunk) FALSE else TRUE

		toExports <- c("readCdtDatasetChunk.sequence", "writeCdtDatasetChunk.sequence", "doparallel")
		packages <- c("doParallel")

		is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 10))
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(jj = seq_along(chunkcalc), .export = toExports, .packages = packages) %parLoop% {
			don.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GeneralParameters$cdtdataset$index, do.par = do.parChunk)
			don.data <- don.data[don$dateInfo$index, , drop = FALSE]
			don.data <- don.data[iyear, , drop = FALSE]

			dat.clim <- lapply(seq_along(index$id), function(j){
				if(Tstep.miss[jj]){
					tmp <- rep(NA, ncol(don.data))
					 return(list(moy = tmp, sds = tmp))
				}

				xx <- don.data[index$index[[j]], , drop = FALSE]
				ina <- (colSums(!is.na(xx))/div) <  minyear
				moy <- colMeans(xx, na.rm = TRUE)
				sds <- matrixStats::colSds(xx, na.rm = TRUE)
				moy[ina] <- NA
				sds[ina] <- NA
				rm(xx)
				list(moy = moy, sds = sds)
			})

			dat.moy <- do.call(rbind, lapply(dat.clim, "[[", "moy"))
			dat.sds <- do.call(rbind, lapply(dat.clim, "[[", "sds"))

			writeCdtDatasetChunk.sequence(dat.moy, chunkcalc[[jj]], index.out, datadir1, do.par = do.parChunk)
			writeCdtDatasetChunk.sequence(dat.sds, chunkcalc[[jj]], index.out, datadir2, do.par = do.parChunk)
			rm(dat.clim, dat.moy, dat.sds, don.data); gc()
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

		##########################################

		x <- index.out$coords$mat$x
		y <- index.out$coords$mat$y
		dx <- ncdim_def("Lon", "degreeE", x)
		dy <- ncdim_def("Lat", "degreeN", y)
		xy.dim <- list(dx, dy)
		nc.grd <- ncvar_def(index.out$varInfo$name, index.out$varInfo$units, xy.dim, -99, index.out$varInfo$longname, "float", compression = 9)

		######################
		ret <- lapply(index$id, function(id){
			dat.moy <- readCdtDatasetChunk.multi.dates.order(file.index1, id, onedate = TRUE)
			dat.moy <- dat.moy$z
			dat.moy[is.na(dat.moy)] <- -99
			filenc <- file.path(ncdfOUT1, paste0("clim_", id, ".nc"))
			nc <- nc_create(filenc, nc.grd)
			ncvar_put(nc, nc.grd, dat.moy)
			nc_close(nc)

			dat.sds <- readCdtDatasetChunk.multi.dates.order(file.index2, id, onedate = TRUE)
			dat.sds <- dat.sds$z
			dat.sds[is.na(dat.sds)] <- -99
			filenc <- file.path(ncdfOUT2, paste0("clim_", id, ".nc"))
			nc <- nc_create(filenc, nc.grd)
			ncvar_put(nc, nc.grd, dat.sds)
			nc_close(nc)

			return(0)
		})

		rm(don, index, output, index.out)
	}

	#####################################################

	if(GeneralParameters$data.type == "cdtnetcdf"){
		sdon <- getRFESampleData(GeneralParameters$cdtnetcdf$sample)
		if(is.null(sdon)){
			InsertMessagesTxt(main.txt.out, "No sample data found", format = TRUE)
			return(NULL)
		}

		months <- 1:12
		start.date <- as.Date("1900-1-1")
		end.date <- as.Date("2050-12-31")

		don.DIR <- GeneralParameters$cdtnetcdf$dir
		don.Format <- GeneralParameters$cdtnetcdf$format
		don.errmsg <- "NetCDF data not found"
		donInfo <- ncFilesInfo(freqData, start.date, end.date, months, don.DIR, don.Format, don.errmsg)
		if(is.null(donInfo)) return(NULL)
		donInfo$ncinfo <- list(xo = sdon$rfeILon, yo = sdon$rfeILat, varid = sdon$rfeVarid)

		donInfo$dates <- donInfo$dates[donInfo$exist]
		donInfo$nc.files <- donInfo$nc.files[donInfo$exist]
		donInfo$exist <- donInfo$exist[donInfo$exist]

		daty <- donInfo$dates
		year <- as.numeric(substr(daty, 1, 4))

		if(length(unique(year)) < minyear){
			InsertMessagesTxt(main.txt.out, "No enough data to calculate climatologies", format = TRUE)
			return(NULL)
		}

		### Clim
		iyear <- year >= year1 & year <= year2
		daty <- daty[iyear]
		index <- getClimatologiesIndex(daty, freqData, xwin)

		## Check for each tstep
		div <- if(freqData == "daily") 2*xwin+1 else 1
		Tstep.miss <- (sapply(index$index, length)/div) < minyear

		#########################################

		outDIR <- file.path(GeneralParameters$out.dir, "CLIMATOLOGY_data")
		dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)
		out.dat.index <- gzfile(file.path(outDIR, "Climatology.rds"), compression = 7)

		# ncdfOUT <- file.path(outDIR, 'DATA_NetCDF')
		# dir.create(ncdfOUT, showWarnings = FALSE, recursive = TRUE)

		# dataOUT <- file.path(outDIR, 'CDTMEAN')
		# datadir <- file.path(dataOUT, 'DATA')
		# dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
		# file.index <- file.path(dataOUT, 'CDTMEAN.rds')

		ncdfOUT1 <- file.path(outDIR, 'DATA_NetCDF', 'CDTMEAN')
		dir.create(ncdfOUT1, showWarnings = FALSE, recursive = TRUE)
		ncdfOUT2 <- file.path(outDIR, 'DATA_NetCDF', 'CDTSTD')
		dir.create(ncdfOUT2, showWarnings = FALSE, recursive = TRUE)

		datadir1 <- file.path(outDIR, 'CDTMEAN', 'DATA')
		dir.create(datadir1, showWarnings = FALSE, recursive = TRUE)
		file.index1 <- file.path(outDIR, 'CDTMEAN', 'CDTMEAN.rds')

		datadir2 <- file.path(outDIR, 'CDTSTD', 'DATA')
		dir.create(datadir2, showWarnings = FALSE, recursive = TRUE)
		file.index2 <- file.path(outDIR, 'CDTSTD', 'CDTSTD.rds')

		##################

		output <- list(params = GeneralParameters, index = index$id)

		EnvClimatoCalcPlot$output <- output
		EnvClimatoCalcPlot$PathClim <- outDIR

		saveRDS(output, out.dat.index)

		#####################################
		nc <- nc_open(donInfo$nc.files[1])
		nc.lon <- nc$dim[[donInfo$ncinfo$xo]]$vals
		nc.lat <- nc$dim[[donInfo$ncinfo$yo]]$vals
		varInfo <- nc$var[[donInfo$ncinfo$varid]][c('name', 'prec', 'units', 'longname')]
		nc_close(nc)

		xo <- order(nc.lon)
		nc.lon <- nc.lon[xo]
		yo <- order(nc.lat)
		nc.lat <- nc.lat[yo]
		len.lon <- length(nc.lon)
		len.lat <- length(nc.lat)

		#########################################

		dx <- ncdim_def("Lon", "degreeE", nc.lon)
		dy <- ncdim_def("Lat", "degreeN", nc.lat)
		xy.dim <- list(dx, dy)
		nc.grd <- ncvar_def(varInfo$name, varInfo$units, xy.dim, -99, varInfo$longname, "float", compression = 9)

		#########################################

		# create chunck
		cdtTmpVar <- NULL
		cdtTmpVar$TimeStep <- freqData
		chunksize <- 100
		cdtTmpVar$chunkfac <- 5

		####
		nxy.chunksize <- round(sqrt(chunksize))
		seqlon <- seq_along(nc.lon)
		seqlat <- seq_along(nc.lat)
		seqcol <- cbind(id = seq(len.lon*len.lat), expand.grid(x = seqlon, y = seqlat))

		split.lon <- split(seqlon, ceiling(seqlon / nxy.chunksize))
		split.lat <- split(seqlat, ceiling(seqlat / nxy.chunksize))
		xgrid <- expand.grid(x = seq_along(split.lon), y = seq_along(split.lat))

		xarrg <- lapply(seq(nrow(xgrid)), function(j){
			crd <- expand.grid(x = nc.lon[split.lon[[xgrid$x[j]]]], y = nc.lat[split.lat[[xgrid$y[j]]]])
			id <- seqcol$id[(seqcol$x %in% split.lon[[xgrid$x[j]]]) & (seqcol$y %in% split.lat[[xgrid$y[j]]])]
			list(coords = crd, id = id, grp = rep(j, length(id)))
		})

		col.idx <- lapply(xarrg, function(x) x$id)
		col.id <- do.call(c, col.idx)
		col.grp <- do.call(c, lapply(xarrg, function(x) x$grp))
		xy.exp <- do.call(rbind, lapply(xarrg, function(x) x$coords))
		col.order <- order(col.id)

		cdtTmpVar$chunksize <- nxy.chunksize*nxy.chunksize
		cdtTmpVar$coords$mat <- list(x = nc.lon, y = nc.lat)
		cdtTmpVar$coords$df <- xy.exp
		attr(cdtTmpVar$coords$df, "out.attrs") <- NULL
		cdtTmpVar$colInfo <- list(id = col.id, index = col.grp, order = col.order)
		cdtTmpVar$varInfo <- varInfo

		# ## daty
		# ncDaty <- do.call(c, lapply(index$index, function(j) donInfo$dates[j]))
		# Adates <- ncDaty
		# Aindex <- seq(length(ncDaty))
		# odaty <- order(Adates)
		# cdtTmpVar$dateInfo <- list(date = Adates[odaty], index = Aindex[odaty])
		cdtTmpVar$dateInfo <- list(date = index$id, index = seq_along(index$id))

		#########################################

		ret <- lapply(seq_along(index$index), function(jj){
			if(Tstep.miss[jj]){
				dat.moy <- rep(NA, len.lon*len.lat)
				dat.sds <- rep(NA, len.lon*len.lat)
			}else{
				id2read <- index$index[[jj]]
				dat.clim <- lapply(seq_along(id2read), function(j){
					nc <- nc_open(donInfo$nc.files[id2read[j]])
					vars <- ncvar_get(nc, varid = donInfo$ncinfo$varid)
					nc_close(nc)
					vars <- vars[xo, yo]
					if(donInfo$ncinfo$yo < donInfo$ncinfo$xo){
						vars <- matrix(c(vars), nrow = len.lon, ncol = len.lat, byrow = TRUE)
					}
					return(c(vars))
				})
				dat.clim <- do.call(rbind, dat.clim)
				ina <- (colSums(!is.na(dat.clim))/div) <  minyear

				dat.moy <- colMeans(dat.clim, na.rm = TRUE)
				dat.moy[ina] <- NA
				dat.sds <- matrixStats::colSds(dat.clim, na.rm = TRUE)
				dat.sds[ina] <- NA
				rm(dat.clim)
			}

			clim <- matrix(dat.moy, len.lon, len.lat)
			clim[is.na(clim)] <- -99
			filenc <- file.path(ncdfOUT1, paste0("clim_", index$id[jj], ".nc"))
			nc <- nc_create(filenc, nc.grd)
			ncvar_put(nc, nc.grd, clim)
			nc_close(nc)
			rm(clim)

			clim <- matrix(dat.sds, len.lon, len.lat)
			clim[is.na(clim)] <- -99
			filenc <- file.path(ncdfOUT2, paste0("clim_", index$id[jj], ".nc"))
			nc <- nc_create(filenc, nc.grd)
			ncvar_put(nc, nc.grd, clim)
			nc_close(nc)
			rm(clim)

			ret0 <- lapply(seq_along(col.idx), function(j){
				file.tmp <- file.path(datadir1, paste0("clim_", j, ".", jj))
				con <- gzfile(file.tmp, open = "wb")
				saveRDS(dat.moy[col.idx[[j]]], con)
				close(con)

				file.tmp <- file.path(datadir2, paste0("clim_", j, ".", jj))
				con <- gzfile(file.tmp, open = "wb")
				saveRDS(dat.sds[col.idx[[j]]], con)
				close(con)

				return(0)
			})
			rm(dat.moy, dat.sds)
			return(0)
		})

		is.parallel <- doparallel(length(col.idx) >= 20)
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(j = seq_along(col.idx)) %parLoop% {
			tmp <- lapply(seq_along(index$index), function(jj){
				file.tmp <- file.path(datadir1, paste0("clim_", j, ".", jj))
				dd <- readRDS(file.tmp)
				unlink(file.tmp)
				return(dd)
			})
			tmp <- do.call(rbind, tmp)

			file.rds <- file.path(datadir1, paste0(j, ".rds"))
			con <- gzfile(file.rds, compression = 6)
			open(con, "wb")
			saveRDS(tmp, con)
			close(con)
			rm(tmp)

			######
			tmp <- lapply(seq_along(index$index), function(jj){
				file.tmp <- file.path(datadir2, paste0("clim_", j, ".", jj))
				dd <- readRDS(file.tmp)
				unlink(file.tmp)
				return(dd)
			})
			tmp <- do.call(rbind, tmp)

			file.rds <- file.path(datadir2, paste0(j, ".rds"))
			con <- gzfile(file.rds, compression = 6)
			open(con, "wb")
			saveRDS(tmp, con)
			close(con)
			rm(tmp); gc()

			return(0)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

		con <- gzfile(file.index1, compression = 6)
		open(con, "wb")
		saveRDS(cdtTmpVar, con)
		close(con)

		con <- gzfile(file.index2, compression = 6)
		open(con, "wb")
		saveRDS(cdtTmpVar, con)
		close(con)

		rm(sdon, cdtTmpVar)
	}

	return(0)
}
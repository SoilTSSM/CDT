
compute_SeasonOnset_Procs <- function(GeneralParameters){
	if(!dir.exists(GeneralParameters$output)){
		InsertMessagesTxt(main.txt.out, paste(GeneralParameters$output, "did not find"), format = TRUE)
		return(NULL)
	}

	if(GeneralParameters$data.type == "cdtstation"){
		prec <- getStnOpenData(GeneralParameters$cdtstation$prec)
		if(is.null(prec)) return(NULL)
		prec <- getCDTdataAndDisplayMsg(prec, "daily")
		if(is.null(prec)) return(NULL)

		if(GeneralParameters$method == 2){
			etp <- getStnOpenData(GeneralParameters$cdtstation$etp)
			if(is.null(etp)) return(NULL)
			etp <- getCDTdataAndDisplayMsg(etp, "daily")
			if(is.null(etp)) return(NULL)

			if(!any(prec$id%in%etp$id)){
				InsertMessagesTxt(main.txt.out, "Precip & PET stations do not match", format = TRUE)
				return(NULL)
			}
			if(!any(prec$dates%in%etp$dates)){
				InsertMessagesTxt(main.txt.out, "Precip & PET dates do not match", format = TRUE)
				return(NULL)
			}

			##################
			inx <- match(prec$dates, etp$dates)
			inx <- inx[!is.na(inx)]
			etp$dates <- etp$dates[inx]
			etp$data <- etp$data[inx, , drop = FALSE]
			prec$data <- prec$data[prec$dates%in%etp$dates, , drop = FALSE]
			daty <- etp$dates

			##################
			jnx <- match(prec$id, etp$id)
			jnx <- jnx[!is.na(jnx)]
			etp$id <- etp$id[jnx]

			stn.id <- etp$id
			stn.lon <- etp$lon[jnx]
			stn.lat <- etp$lat[jnx]
			etp$data <- etp$data[, jnx, drop = FALSE]
			prec$data <- prec$data[, prec$id%in%etp$id, drop = FALSE]
		}else{
			daty <- prec$dates
			stn.id <- prec$id
			stn.lon <- prec$lon
			stn.lat <- prec$lat
			etp <- NULL
		}

		##################

		criteria <- GeneralParameters$onset.criteria

		if(GeneralParameters$onset.reg$region == "Multiple"){
			if(GeneralParameters$onset.reg$subdiv == "Latitude"){
				nbreg <- GeneralParameters$onset.reg$lat$nb
				latdiv <- GeneralParameters$onset.reg$lat$div
				subdiv <- list()
				if(nbreg > 2){
					subdiv[[1]] <- which(stn.lat <= latdiv[[1]])
					for(i in 1:(length(latdiv)-1)) subdiv[[i+1]] <- which(latdiv[[i+1]] >= stn.lat & stn.lat > latdiv[[i]])
					subdiv[[nbreg]] <- which(stn.lat > latdiv[[length(latdiv)]])
				}else{
					div <- stn.lat <= latdiv[[1]]
					subdiv[[1]] <- which(div)
					subdiv[[2]] <- which(!div)
				}
			}

			if(GeneralParameters$onset.reg$subdiv == "Shapefile"){
				shpf <- getShpOpenData(GeneralParameters$onset.reg$shp$file)[[2]]
				if(is.null(shpf)){
					InsertMessagesTxt(main.txt.out, "No polygons found", format = TRUE)
					return(NULL)
				}
				shpattr <- GeneralParameters$onset.reg$shp$attr
				shpf <- shpf[, shpattr]
				shpdiv <- as.character(shpf@data[, shpattr])

				stn.coord <- data.frame(x = stn.lon, y = stn.lat)
				coordinates(stn.coord) <- ~x+y
				subdiv <- lapply(shpdiv, function(x){
					xov <- over(stn.coord, shpf[shpdiv == x, ])
					which(!is.na(xov[, 1]))
				})
			}

			divexist <- sapply(subdiv, length) > 0
			subdiv <- subdiv[divexist]
			criteria <- criteria[divexist]
		}else subdiv <- list(seq_along(stn.lat))

		##################

		res <- lapply(seq_along(subdiv), function(j){
			onset.pars <- criteria[[j]]
			precip <- prec$data[, subdiv[[j]], drop = FALSE]
			evapo <- if(GeneralParameters$method == 2) etp$data[, subdiv[[j]], drop = FALSE] else NULL
			index <- get.Index.DailyYears(daty, onset.pars$earliest$month, onset.pars$earliest$day)
			len.index <- length(index$index)

			ons <- lapply(seq(len.index), function(ii){
				idx <- index$index[[ii]]
				rr <- precip[idx, , drop = FALSE]
				evp <- evapo[idx, , drop = FALSE]
				dates <- daty[idx]
				min.frac <- if(ii == len.index) 0 else GeneralParameters$min.frac
				Season.Onset(dates, rr, evp, GeneralParameters$method, onset.pars, min.frac)
			})
			ons <- do.call(rbind, ons)
			start.date <- as.character(index$range.date[, 1])
			list(onset = ons, start = start.date)
		})

		start.date <- lapply(res, function(x) as.Date(x$start, "%Y%m%d"))
		start.date <- as.Date(matrixStats::rowMins(do.call(cbind, start.date)), origin = "1970-1-1")
		onset <- matrix(NA, length(start.date), length(stn.lat))
		for(j in seq_along(subdiv)) onset[, subdiv[[j]]] <- res[[j]]$onset

		onset.num <- as.Date(onset, "%Y%m%d")
		dim(onset.num) <- dim(onset)

		##################

		outDIR <- file.path(GeneralParameters$output, "ONSET_data")
		dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

		datadir <- file.path(outDIR, 'CDTSTATIONS')
		dir.create(datadir, showWarnings = FALSE, recursive = TRUE)

		dataOUT <- file.path(outDIR, 'CDTDATASET')
		dir.create(dataOUT, showWarnings = FALSE, recursive = TRUE)

		file.onset <- file.path(datadir, "Onset_dates.txt")
		file.onset1 <- file.path(datadir, "Onset_days.txt")
		file.index <- file.path(outDIR, "Onset.rds")
		file.cdt.prec <- file.path(dataOUT, "PRECIP.rds")
		file.cdt.etp <- file.path(dataOUT, "PET.rds")
		file.cdt.onset <- file.path(dataOUT, "ONSET.rds")

		##################

		# stn.data <- list(id = stn.id, lon = stn.lon, lat = stn.lat, date = daty, prec = prec$data, etp = etp$data)
		# output <- list(params = GeneralParameters, data = stn.data, onset = onset.num,
		# 				start.date = start.date, ts.date = daty)

		stn.data <- list(id = stn.id, lon = stn.lon, lat = stn.lat, date = daty)
		output <- list(params = GeneralParameters, data = stn.data, start.date = start.date, ts.date = daty)

		EnvOnsetCalcPlot$output <- output
		EnvOnsetCalcPlot$PathData <- outDIR

		##################
		con <- gzfile(file.index, compression = 7)
		open(con, "wb")
		saveRDS(output, con)
		close(con)

		##################
		con <- gzfile(file.cdt.prec, compression = 7)
		open(con, "wb")
		saveRDS(prec$data, con)
		close(con)

		if(!is.null(etp)){
			con <- gzfile(file.cdt.etp, compression = 7)
			open(con, "wb")
			saveRDS(etp$data, con)
			close(con)
		}

		##################
		con <- gzfile(file.cdt.onset, compression = 7)
		open(con, "wb")
		saveRDS(onset.num, con)
		close(con)

		##################

		onset.num <- onset.num - start.date
		onset[is.na(onset)] <- -99
		onset.num[is.na(onset.num)] <- -99
		start.date <- format(start.date, "%Y%m%d")

		xhead <- rbind(stn.id, stn.lon, stn.lat)
		chead <- c('ID.STN', 'LON', 'START.DATE/LAT')
		infohead <- cbind(chead, xhead)

		onset <- rbind(infohead, cbind(start.date, onset))
		onset.num <- rbind(infohead, cbind(start.date, onset.num))

		writeFiles(onset, file.onset)
		writeFiles(onset.num, file.onset1)

		rm(prec, etp, res, onset, onset.num, stn.data, output)
	}

	#######################################################

	if(GeneralParameters$data.type == "cdtdataset"){
		prec <- try(readRDS(GeneralParameters$cdtdataset$prec), silent = TRUE)
		if(inherits(prec, "try-error")){
			InsertMessagesTxt(main.txt.out, paste("Unable to read", GeneralParameters$cdtdataset$prec), format = TRUE)
			return(NULL)
		}
		if(prec$TimeStep != "daily"){
			InsertMessagesTxt(main.txt.out, paste("Precip dataset is not a daily data"), format = TRUE)
			return(NULL)
		}

		if(GeneralParameters$method == 2){
			etp <- try(readRDS(GeneralParameters$cdtdataset$etp), silent = TRUE)
			if(inherits(etp, "try-error")){
				InsertMessagesTxt(main.txt.out, paste("Unable to read", GeneralParameters$cdtdataset$etp), format = TRUE)
				return(NULL)
			}
			if(etp$TimeStep != "daily"){
				InsertMessagesTxt(main.txt.out, paste("PET dataset is not a daily data"), format = TRUE)
				return(NULL)
			}

			##################
			SP1 <- defSpatialPixels(list(lon = prec$coords$mat$x, lat = prec$coords$mat$y))
			SP2 <- defSpatialPixels(list(lon = etp$coords$mat$x, lat = etp$coords$mat$y))
			if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
				InsertMessagesTxt(main.txt.out, "Precip & PET have different resolution or bbox", format = TRUE)
				return(NULL)
			}
			rm(SP1, SP2)

			##################
			if(prec$chunksize != etp$chunksize){
				InsertMessagesTxt(main.txt.out, "Precip & PET have different chunk size", format = TRUE)
				return(NULL)
			}

			##################
			if(!any(prec$dateInfo$date%in%etp$dateInfo$date)){
				InsertMessagesTxt(main.txt.out, "Precip & PET dates do not match", format = TRUE)
				return(NULL)
			}

			##################
			inx <- match(prec$dateInfo$date, etp$dateInfo$date)
			inx <- inx[!is.na(inx)]
			etp$dateInfo$date <- etp$dateInfo$date[inx]
			etp$dateInfo$index <- etp$dateInfo$index[inx]

			pdaty <- prec$dateInfo$date%in%etp$dateInfo$date
			prec$dateInfo$date <- prec$dateInfo$date[pdaty]
			prec$dateInfo$index <- prec$dateInfo$index[pdaty]
		}

		##################

		if(GeneralParameters$onset.reg$region == "Multiple"){
			if(GeneralParameters$onset.reg$subdiv == "Shapefile"){
				shpf <- getShpOpenData(GeneralParameters$onset.reg$shp$file)[[2]]
				if(is.null(shpf)){
					InsertMessagesTxt(main.txt.out, "No polygons found", format = TRUE)
					return(NULL)
				}
				shpattr <- GeneralParameters$onset.reg$shp$attr
				shpf <- shpf[, shpattr]
				shpdiv <- as.character(shpf@data[, shpattr])
			}
		}

		##################

		outDIR <- file.path(GeneralParameters$output, "ONSET_data")
		dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

		ncdfOUT <- file.path(outDIR, 'DATA_NetCDF')
		dir.create(ncdfOUT, showWarnings = FALSE, recursive = TRUE)

		dataOUT <- file.path(outDIR, 'CDTDATASET')
		dir.create(dataOUT, showWarnings = FALSE, recursive = TRUE)

		datadir <- file.path(dataOUT, 'DATA')
		dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
		datafileIdx <- file.path(dataOUT, 'CDTDATASET.rds')
		file.index <- file.path(outDIR, "Onset.rds")

		index.out <- prec
		index.out$varInfo$name <- "onset"
		index.out$varInfo$units <- "days since 1970-01-01"
		index.out$varInfo$longname <- "Onset date of rainy season"

		##################

		chunkfile <- sort(unique(prec$colInfo$index))
		chunkcalc <- split(chunkfile, ceiling(chunkfile/prec$chunkfac))

		do.parChunk <- if(prec$chunkfac > length(chunkcalc)) TRUE else FALSE
		do.parCALC <- if(do.parChunk) FALSE else TRUE

		toExports <- c("readCdtDatasetChunk.sequence", "writeCdtDatasetChunk.sequence",
						"get.Index.DailyYears", "Season.Onset", "doparallel")
		packages <- c("sp", "stringr", "doParallel")

		is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 5))
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(jj = seq_along(chunkcalc), .export = toExports, .packages = packages) %parLoop% {
			rr.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GeneralParameters$cdtdataset$prec, do.par = do.parChunk)
			rr.data <- rr.data[prec$dateInfo$index, , drop = FALSE]

			if(GeneralParameters$method == 2){
				et.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GeneralParameters$cdtdataset$etp, do.par = do.parChunk)
				et.data <- et.data[etp$dateInfo$index, , drop = FALSE]
			}else et.data <- NULL

			####################################

			lat <- prec$coords$df$y[prec$colInfo$index%in%chunkcalc[[jj]]]
			lon <- prec$coords$df$x[prec$colInfo$index%in%chunkcalc[[jj]]]
			criteria <- GeneralParameters$onset.criteria

			if(GeneralParameters$onset.reg$region == "Multiple"){
				criteria <- GeneralParameters$onset.criteria

				if(GeneralParameters$onset.reg$subdiv == "Latitude"){
					nbreg <- GeneralParameters$onset.reg$lat$nb
					latdiv <- GeneralParameters$onset.reg$lat$div
					subdiv <- list()
					if(nbreg > 2){
						subdiv[[1]] <- which(lat <= latdiv[[1]])
						for(i in 1:(length(latdiv)-1)) subdiv[[i+1]] <- which(latdiv[[i+1]] >= lat & lat > latdiv[[i]])
						subdiv[[nbreg]] <- which(lat > latdiv[[length(latdiv)]])
					}else{
						div <- lat <= latdiv[[1]]
						subdiv[[1]] <- which(div)
						subdiv[[2]] <- which(!div)
					}
				}

				if(GeneralParameters$onset.reg$subdiv == "Shapefile"){
					chunk.coord <- data.frame(x = lon, y = lat)
					coordinates(chunk.coord) <- ~x+y
					subdiv <- lapply(shpdiv, function(x){
						xov <- over(chunk.coord, shpf[shpdiv == x, ])
						which(!is.na(xov[, 1]))
					})
				}

				divexist <- sapply(subdiv, length) > 0
				subdiv <- subdiv[divexist]
				criteria <- criteria[divexist]
			}else subdiv <- list(seq_along(lat))

			##################

			res <- lapply(seq_along(subdiv), function(j){
				daty <- prec$dateInfo$date
				onset.pars <- criteria[[j]]
				precip <- rr.data[, subdiv[[j]], drop = FALSE]
				evapo <- if(GeneralParameters$method == 2) et.data[, subdiv[[j]], drop = FALSE] else NULL
				index <- get.Index.DailyYears(daty, onset.pars$earliest$month, onset.pars$earliest$day)
				len.index <- length(index$index)

				ons <- lapply(seq(len.index), function(ii){
					idx <- index$index[[ii]]
					rr <- precip[idx, , drop = FALSE]
					evp <- evapo[idx, , drop = FALSE]
					dates <- daty[idx]
					min.frac <- if(ii == len.index) 0 else GeneralParameters$min.frac
					Season.Onset(dates, rr, evp, GeneralParameters$method, onset.pars, min.frac)
				})
				ons <- do.call(rbind, ons)
				start.date <- as.character(index$range.date[, 1])
				list(onset = ons, start = start.date)
			})

			start.date <- lapply(res, function(x) as.Date(x$start, "%Y%m%d"))
			start.date <- as.Date(matrixStats::rowMins(do.call(cbind, start.date)), origin = "1970-1-1")
			onset <- matrix(NA, length(start.date), length(lat))
			for(j in seq_along(subdiv)) onset[, subdiv[[j]]] <- res[[j]]$onset

			onset.dim <- dim(onset)
			onset <- as.Date(onset, "%Y%m%d")
			dim(onset) <- onset.dim

			####################################

			writeCdtDatasetChunk.sequence(onset, chunkcalc[[jj]], index.out, datadir, do.par = do.parChunk)

			rm(rr.data, et.data, res, onset); gc()
			return(start.date)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

		####################################

		start.date <- as.Date(matrixStats::rowMins(do.call(cbind, ret)), origin = "1970-1-1")
		output <- list(params = GeneralParameters, start.date = start.date, ts.date = prec$dateInfo$date)
		EnvOnsetCalcPlot$output <- output
		EnvOnsetCalcPlot$PathData <- outDIR

		##################
		con <- gzfile(file.index, compression = 6)
		open(con, "wb")
		saveRDS(output, con)
		close(con)

		##################

		start.date <- format(start.date, "%Y%m%d")
		index.out$dateInfo$date <- start.date
		index.out$dateInfo$index <- seq_along(start.date)

		con <- gzfile(datafileIdx, compression = 6)
		open(con, "wb")
		saveRDS(index.out, con)
		close(con)

		####################################
		chunkdate <- split(start.date, ceiling(seq_along(start.date)/10))

		x <- index.out$coords$mat$x
		y <- index.out$coords$mat$y
		dx <- ncdim_def("Lon", "degreeE", x)
		dy <- ncdim_def("Lat", "degreeN", y)

		ret <- lapply(chunkdate, function(dates){
			dat <- readCdtDatasetChunk.multi.dates.order(datafileIdx, dates)
			for(j in seq_along(dates)){
				time0 <- as.integer(as.Date(dates[j], "%Y%m%d"))
				don <- dat[j, ] - time0
				don <- matrix(don, length(x), length(y))
				don[is.na(don)] <- -99
				time <- ncdim_def("start", "days since 1970-1-1", time0)
				xyt.dim <- list(dx, dy, time)
				grdNC <- ncvar_def("onset", paste("days since", dates[j]), xyt.dim, -99,
									longname = "Onset date of rainy season", prec = "short",
									shuffle = TRUE, compression = 6)

				filenc <- file.path(ncdfOUT, paste0("onset_", dates[j], ".nc"))
				nc <- nc_create(filenc, grdNC)
				ncvar_put(nc, grdNC, don)
				nc_close(nc)
			}
			return(0)
		})
	}

	return(0)
}




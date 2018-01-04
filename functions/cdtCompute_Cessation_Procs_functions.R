
compute_SeasonCessation_Procs <- function(GeneralParameters){
	if(!dir.exists(GeneralParameters$output)){
		InsertMessagesTxt(main.txt.out, paste(GeneralParameters$output, "did not find"), format = TRUE)
		return(NULL)
	}

	if(GeneralParameters$data.type == "cdtstation"){
		wb <- getStnOpenData(GeneralParameters$cdtstation$wb)
		if(is.null(wb)) return(NULL)
		wb <- getCDTdataAndDisplayMsg(wb, "daily")
		if(is.null(wb)) return(NULL)

		daty <- wb$dates
		stn.id <- wb$id
		stn.lon <- wb$lon
		stn.lat <- wb$lat

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
			watb <- wb$data[, subdiv[[j]], drop = FALSE]
			index <- get.Index.DailyYears(daty, onset.pars$earliest$month, onset.pars$earliest$day)
			len.index <- length(index$index)

			cess <- lapply(seq(len.index), function(ii){
				idx <- index$index[[ii]]
				watr <- watb[idx, , drop = FALSE]
				dates <- daty[idx]
				min.frac <- if(ii == len.index) 0 else GeneralParameters$min.frac
				Season.Cessation(dates, watr, onset.pars, min.frac)
			})
			cess <- do.call(rbind, cess)
			start.date <- as.character(index$range.date[, 1])
			list(cessation = cess, start = start.date)
		})

		start.date <- lapply(res, function(x) as.Date(x$start, "%Y%m%d"))
		start.date <- as.Date(matrixStats::rowMins(do.call(cbind, start.date)), origin = "1970-1-1")
		cessation <- matrix(NA, length(start.date), length(stn.lat))
		for(j in seq_along(subdiv)) cessation[, subdiv[[j]]] <- res[[j]]$cessation

		cessation.num <- as.Date(cessation, "%Y%m%d")
		dim(cessation.num) <- dim(cessation)

		##################

		stn.data <- list(id = stn.id, lon = stn.lon, lat = stn.lat, date = daty, wb = wb$data)
		output <- list(params = GeneralParameters, data = stn.data, cessation = cessation.num, start.date = start.date)
		EnvCessationCalcPlot$output <- output

		##################

		outDIR <- file.path(GeneralParameters$output, "CESSATION_data")
		dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

		datadir <- file.path(outDIR, 'CDTSTATIONS')
		dir.create(datadir, showWarnings = FALSE, recursive = TRUE)

		file.cessation <- file.path(datadir, "Cessation_dates.txt")
		file.cessation1 <- file.path(datadir, "Cessation_days.txt")
		file.index <- file.path(outDIR, "Cessation.rds")

		##################
		con <- gzfile(file.index, compression = 7)
		open(con, "wb")
		saveRDS(output, con)
		close(con)

		##################

		cessation.num <- cessation.num - start.date
		cessation[is.na(cessation)] <- -99
		cessation.num[is.na(cessation.num)] <- -99
		start.date <- format(start.date, "%Y%m%d")

		xhead <- rbind(stn.id, stn.lon, stn.lat)
		chead <- c('ID.STN', 'LON', 'START.DATE/LAT')
		infohead <- cbind(chead, xhead)

		cessation <- rbind(infohead, cbind(start.date, cessation))
		cessation.num <- rbind(infohead, cbind(start.date, cessation.num))

		writeFiles(cessation, file.cessation)
		writeFiles(cessation.num, file.cessation1)

		rm(wb, res, cessation, cessation.num, stn.data, output)
	}

	if(GeneralParameters$data.type == "cdtdataset"){
		wb <- try(readRDS(GeneralParameters$cdtdataset$wb), silent = TRUE)
		if(inherits(wb, "try-error")){
			InsertMessagesTxt(main.txt.out, paste("Unable to read", GeneralParameters$cdtdataset$wb), format = TRUE)
			return(NULL)
		}
		if(wb$TimeStep != "daily"){
			InsertMessagesTxt(main.txt.out, paste("Water balance dataset is not a daily data"), format = TRUE)
			return(NULL)
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

		outDIR <- file.path(GeneralParameters$output, "CESSATION_data")
		dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

		ncdfOUT <- file.path(outDIR, 'DATA_NetCDF')
		dir.create(ncdfOUT, showWarnings = FALSE, recursive = TRUE)

		dataOUT <- file.path(outDIR, 'CDTDATASET')
		dir.create(dataOUT, showWarnings = FALSE, recursive = TRUE)

		datadir <- file.path(dataOUT, 'DATA')
		dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
		datafileIdx <- file.path(dataOUT, 'CDTDATASET.rds')
		file.index <- file.path(outDIR, "Cessation.rds")

		index.out <- wb
		index.out$varInfo$name <- "cessation"
		index.out$varInfo$units <- "days since 1970-01-01"
		index.out$varInfo$longname <- "Cessation date of rainy season"

		##################

		chunkfile <- sort(unique(wb$colInfo$index))
		chunkcalc <- split(chunkfile, ceiling(chunkfile/wb$chunkfac))

		do.parChunk <- if(wb$chunkfac > length(chunkcalc)) TRUE else FALSE
		do.parCALC <- if(do.parChunk) FALSE else TRUE

		toExports <- c("readCdtDatasetChunk.sequence", "writeCdtDatasetChunk.sequence",
						"get.Index.DailyYears", "Season.Cessation", "doparallel")
		packages <- c("sp", "stringr", "doParallel")

		is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 5))
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(jj = seq_along(chunkcalc), .export = toExports, .packages = packages) %parLoop% {
			wb.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GeneralParameters$cdtdataset$wb, do.par = do.parChunk)
			wb.data <- wb.data[wb$dateInfo$index, , drop = FALSE]

			####################################

			lat <- wb$coords$df$y[wb$colInfo$index%in%chunkcalc[[jj]]]
			lon <- wb$coords$df$x[wb$colInfo$index%in%chunkcalc[[jj]]]
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
				daty <- wb$dateInfo$date
				onset.pars <- criteria[[j]]
				watb <- wb.data[, subdiv[[j]], drop = FALSE]
				index <- get.Index.DailyYears(daty, onset.pars$earliest$month, onset.pars$earliest$day)
				len.index <- length(index$index)

				cess <- lapply(seq(len.index), function(ii){
					idx <- index$index[[ii]]
					watr <- watb[idx, , drop = FALSE]
					dates <- daty[idx]
					min.frac <- if(ii == len.index) 0 else GeneralParameters$min.frac
					Season.Cessation(dates, watr, onset.pars, min.frac)
				})
				cess <- do.call(rbind, cess)
				start.date <- as.character(index$range.date[, 1])
				list(cessation = cess, start = start.date)
			})

			start.date <- lapply(res, function(x) as.Date(x$start, "%Y%m%d"))
			start.date <- as.Date(matrixStats::rowMins(do.call(cbind, start.date)), origin = "1970-1-1")
			cessation <- matrix(NA, length(start.date), length(lat))
			for(j in seq_along(subdiv)) cessation[, subdiv[[j]]] <- res[[j]]$cessation

			cessation.dim <- dim(cessation)
			cessation <- as.Date(cessation, "%Y%m%d")
			dim(cessation) <- cessation.dim

			####################################

			writeCdtDatasetChunk.sequence(cessation, chunkcalc[[jj]], index.out, datadir, do.par = do.parChunk)

			rm(wb.data, res, cessation); gc()
			return(start.date)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

		####################################

		start.date <- as.Date(matrixStats::rowMins(do.call(cbind, ret)), origin = "1970-1-1")
		output <- list(params = GeneralParameters, start.date = start.date)
		EnvCessationCalcPlot$output <- output

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
				grdNC <- ncvar_def("cessation", paste("days since", dates[j]), xyt.dim, -99,
									longname = "Cessation date of rainy season", prec = "short",
									shuffle = TRUE, compression = 6)

				filenc <- file.path(ncdfOUT, paste0("cessation_", dates[j], ".nc"))
				nc <- nc_create(filenc, grdNC)
				ncvar_put(nc, grdNC, don)
				nc_close(nc)
			}
			return(0)
		})
	}

	return(0)
}




computeWBProcs <- function(GeneralParameters){
	InsertMessagesTxt(main.txt.out, paste("Compute daily water balance ......"))

	if(GeneralParameters$data.type == "cdtstation"){
		prec <- getStnOpenData(GeneralParameters$cdtstation$prec)
		if(is.null(prec)) return(NULL)
		prec <- getCDTdataAndDisplayMsg(prec, GeneralParameters$Tstep)
		if(is.null(prec)) return(NULL)

		etp <- getStnOpenData(GeneralParameters$cdtstation$etp)
		if(is.null(etp)) return(NULL)
		etp <- getCDTdataAndDisplayMsg(etp, GeneralParameters$Tstep)
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
		# Fill missing dates and order (deja fait avec splitCDTData pour les stations)

		##################
		jnx <- match(prec$id, etp$id)
		jnx <- jnx[!is.na(jnx)]
		etp$id <- etp$id[jnx]

		stn.id <- etp$id
		stn.lon <- etp$lon[jnx]
		stn.lat <- etp$lat[jnx]
		etp$data <- etp$data[, jnx, drop = FALSE]
		prec$data <- prec$data[, prec$id%in%etp$id, drop = FALSE]

		##################

		if(GeneralParameters$swhc$multi){
			swhc <- getStnOpenData(GeneralParameters$swhc$file)
			if(is.null(swhc)) return(NULL)

			swhc.id <- as.character(swhc[1, -1])
			# swhc.lon <- as.numeric(swhc[2, -1])
			# swhc.lat <- as.numeric(swhc[3, -1])
			swhc <- as.numeric(swhc[nrow(swhc), -1])

			if(!isTRUE(all.equal(stn.id, swhc.id))){
				InsertMessagesTxt(main.txt.out, "SWHC, Precip & PET stations do not match", format = TRUE)
				return(NULL)
			}

			# if(!any(stn.id%in%swhc.id)){
			# 	InsertMessagesTxt(main.txt.out, "SWHC, Precip & PET stations do not match", format = TRUE)
			# 	return(NULL)
			# }

			# ijs <- match(swhc.id, stn.id)
			# ijs <- ijs[!is.na(ijs)]

			# stn.id <- stn.id[ijs]
			# stn.lon <- stn.lon[ijs]
			# stn.lat <- stn.lat[ijs]
			# etp$data <- etp$data[, ijs, drop = FALSE]
			# prec$data <- prec$data[, ijs, drop = FALSE]
			# swhc <- swhc[swhc.id%in%stn.id]
		}else swhc <- GeneralParameters$swhc$cap.max

		if(GeneralParameters$wb$multi){
			wb1 <- getStnOpenData(GeneralParameters$wb$file)
			if(is.null(wb1)) return(NULL)

			wb1.id <- as.character(wb1[1, -1])
			# wb1.lon <- as.numeric(wb1[2, -1])
			# wb1.lat <- as.numeric(wb1[3, -1])
			wb1 <- as.numeric(wb1[nrow(wb1), -1])

			if(!isTRUE(all.equal(stn.id, wb1.id))){
				InsertMessagesTxt(main.txt.out, "Initial water balance, Precip & PET stations do not match", format = TRUE)
				return(NULL)
			}
		}else wb1 <- GeneralParameters$wb$wb1

		##################

		index <- get.Index.DailyYears(daty, GeneralParameters$hdate$start.month, GeneralParameters$hdate$start.day)
		index <- index$index

		miss1 <- is.na(index[[1]][1])
		startDaty <- which(!is.na(index[[1]]))[1]
		iend <- which(!is.na(index[[length(index)]]))
		lastIx <- index[-length(index)]
		lastIx <- if(length(lastIx) > 0) sum(sapply(lastIx, length)) else 0
		endDaty <- iend[length(iend)]+lastIx

		if(!GeneralParameters$hdate$separate.year){
			index <- if(miss1) list(index[[1]], unlist(index[-1])) else list(unlist(index))
		}
		daty <- do.call(c, lapply(index, function(i) daty[i]))

		### fill missing date
		rg.date <- range(as.Date(daty, '%Y%m%d'), na.rm = TRUE)
		daty <- format(seq(rg.date[1], rg.date[2], 'day'), '%Y%m%d')

		##################

		WB <- lapply(index, function(idx){
			Water.Balance(prec$data[idx, , drop = FALSE], etp$data[idx, , drop = FALSE], swhc, wb1)
		})
		if(miss1) WB[[1]][] <- NA
		WB <- do.call(rbind, WB)
		WB <- WB[startDaty:endDaty, , drop = FALSE]
		WB <- round(WB, 1)
		WB[is.na(WB)] <- -99

		##################

		xhead <- rbind(stn.id, stn.lon, stn.lat)
		chead <- c('ID.STN', 'LON', paste0(toupper(GeneralParameters$Tstep), '/LAT'))
		infohead <- cbind(chead, xhead)

		WB <- rbind(infohead, cbind(daty, WB))
		writeFiles(WB, GeneralParameters$output)
		rm(prec, etp, WB)
	}

	#######################################################

	if(GeneralParameters$data.type == "cdtdataset"){
		prec <- try(readRDS(GeneralParameters$cdtdataset$prec), silent = TRUE)
		if(inherits(prec, "try-error")){
			InsertMessagesTxt(main.txt.out, paste("Unable to read", GeneralParameters$cdtdataset$prec), format = TRUE)
			return(NULL)
		}
		if(GeneralParameters$Tstep != prec$TimeStep){
			InsertMessagesTxt(main.txt.out, paste("Precip dataset is not a", GeneralParameters$Tstep, "data"), format = TRUE)
			return(NULL)
		}

		etp <- try(readRDS(GeneralParameters$cdtdataset$etp), silent = TRUE)
		if(inherits(etp, "try-error")){
			InsertMessagesTxt(main.txt.out, paste("Unable to read", GeneralParameters$cdtdataset$etp), format = TRUE)
			return(NULL)
		}
		if(GeneralParameters$Tstep != etp$TimeStep){
			InsertMessagesTxt(main.txt.out, paste("PET dataset is not a", GeneralParameters$Tstep, "data"), format = TRUE)
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

		daty <- prec$dateInfo$date

		##################

		if(GeneralParameters$swhc$multi){
			swhc <- getDemOpenDataSPDF(GeneralParameters$swhc$file)
			if(is.null(swhc)){
				InsertMessagesTxt(main.txt.out, "No SWHC data found", format = TRUE)
				return(NULL)
			}

			SP1 <- defSpatialPixels(swhc[c('lon', 'lat')])
			SP2 <- defSpatialPixels(list(lon = etp$coords$mat$x, lat = etp$coords$mat$y))
			if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
				InsertMessagesTxt(main.txt.out, "SWHC, Precip & PET have different resolution or bbox", format = TRUE)
				return(NULL)
			}
			rm(SP1, SP2)

			swhc <- swhc$demMat
		}else swhc <- GeneralParameters$swhc$cap.max

		if(GeneralParameters$wb$multi){
			wb1 <- getDemOpenDataSPDF(GeneralParameters$wb$file)
			if(is.null(wb1)){
				InsertMessagesTxt(main.txt.out, "No initial water balance data found", format = TRUE)
				return(NULL)
			}

			SP1 <- defSpatialPixels(wb1[c('lon', 'lat')])
			SP2 <- defSpatialPixels(list(lon = etp$coords$mat$x, lat = etp$coords$mat$y))
			if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
				InsertMessagesTxt(main.txt.out, "Initial water balance, Precip & PET have different resolution or bbox", format = TRUE)
				return(NULL)
			}
			rm(SP1, SP2)

			wb1 <- wb1$demMat
		}else wb1 <- GeneralParameters$wb$wb1

		##################

		index <- get.Index.DailyYears(daty, GeneralParameters$hdate$start.month, GeneralParameters$hdate$start.day)
		index <- index$index

		miss1 <- is.na(index[[1]][1])
		startDaty <- which(!is.na(index[[1]]))[1]
		iend <- which(!is.na(index[[length(index)]]))
		lastIx <- index[-length(index)]
		lastIx <- if(length(lastIx) > 0) sum(sapply(lastIx, length)) else 0
		endDaty <- iend[length(iend)] + lastIx

		if(!GeneralParameters$hdate$separate.year){
			index <- if(miss1) list(index[[1]], unlist(index[-1])) else list(unlist(index))
		}
		daty <- do.call(c, lapply(index, function(i) daty[i]))

		### fill missing date
		rg.date <- range(as.Date(daty, '%Y%m%d'), na.rm = TRUE)
		daty <- format(seq(rg.date[1], rg.date[2], 'day'), '%Y%m%d')

		##################

		index.out <- prec
		index.out$varInfo$name <- "wb"
		index.out$varInfo$units <- "mm"
		index.out$varInfo$longname <- "Daily water balance"

		dataset <- paste0("WaterBalance_", GeneralParameters$Tstep)
		outDIR <- file.path(GeneralParameters$output, dataset)
		dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

		datadir <- file.path(outDIR, 'DATA')
		dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
		datafileIdx <- file.path(outDIR, paste0(dataset, '.rds'))

		##################

		chunkfile <- sort(unique(prec$colInfo$index))
		chunkcalc <- split(chunkfile, ceiling(chunkfile/prec$chunkfac))

		do.parChunk <- if(prec$chunkfac > length(chunkcalc)) TRUE else FALSE
		do.parCALC <- if(do.parChunk) FALSE else TRUE

		toExports <- c("readCdtDatasetChunk.sequence", "writeCdtDatasetChunk.sequence",
						"Water.Balance", "doparallel")

		is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 5))
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(j = seq_along(chunkcalc), .export = toExports, .packages = "doParallel") %parLoop% {
			rr <- readCdtDatasetChunk.sequence(chunkcalc[[j]], GeneralParameters$cdtdataset$prec, do.par = do.parChunk)
			rr <- rr[prec$dateInfo$index, , drop = FALSE]

			et <- readCdtDatasetChunk.sequence(chunkcalc[[j]], GeneralParameters$cdtdataset$etp, do.par = do.parChunk)
			et <- et[etp$dateInfo$index, , drop = FALSE]

			if(GeneralParameters$swhc$multi){
				ix <- prec$colInfo$index%in%chunkcalc[[j]]
				capmx <- swhc[prec$colInfo$id[ix]]
			}else capmx <- swhc

			if(GeneralParameters$wb$multi){
				ix <- prec$colInfo$index%in%chunkcalc[[j]]
				wb0 <- wb1[prec$colInfo$id[ix]]
			}else wb0 <- wb1

			WB <- lapply(index, function(idx){
				Water.Balance(rr[idx, , drop = FALSE], et[idx, , drop = FALSE], capmx, wb0)
			})

			if(miss1) WB[[1]][] <- NA
			WB <- do.call(rbind, WB)
			WB <- WB[startDaty:endDaty, , drop = FALSE]

			writeCdtDatasetChunk.sequence(WB, chunkcalc[[j]], index.out, datadir, do.par = do.parChunk)

			rm(rr, et, WB); gc()
			return(0)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

		##################

		index.out$dateInfo$date <- daty
		index.out$dateInfo$index <- seq(length(daty))

		con <- gzfile(datafileIdx, compression = 6)
		open(con, "wb")
		saveRDS(index.out, con)
		close(con)

		rm(etp, prec, index.out)
	}

	return(0)
}



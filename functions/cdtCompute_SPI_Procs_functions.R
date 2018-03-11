computeSPIProcs <- function(GeneralParameters){
	freqData <- GeneralParameters$intstep
	input.file <- if(GeneralParameters$data.type == 'cdtstation') GeneralParameters$cdtstation else GeneralParameters$cdtdataset
	if(input.file%in%c("", "NA")){
		InsertMessagesTxt(main.txt.out, 'No input data found', format = TRUE)
		return(NULL)
	}

	if(!dir.exists(GeneralParameters$outdir)){
		InsertMessagesTxt(main.txt.out, 'Directory to save results not found', format = TRUE)
		InsertMessagesTxt(main.txt.out, paste('The outputs will be put in', getwd()))
		GeneralParameters$outdir <- getwd()
	}

	#####################
	if(GeneralParameters$data.type == "cdtstation"){
		don <- getStnOpenData(GeneralParameters$cdtstation)
		if(is.null(don)) return(NULL)
		don <- getCDTdataAndDisplayMsg(don, "daily")
		if(is.null(don)) return(NULL)

		daty <- don$dates
	}

	if(GeneralParameters$data.type == "cdtdataset"){
		don <- try(readRDS(GeneralParameters$cdtdataset), silent = TRUE)
		if(inherits(don, "try-error")){
			InsertMessagesTxt(main.txt.out, paste("Unable to read", GeneralParameters$cdtdataset), format = TRUE)
			return(NULL)
		}

		daty <- don$dateInfo$date
	}

	#####################
	outDIR <- file.path(GeneralParameters$outdir, "SPI_data")
	dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)
	out.spi.index <- gzfile(file.path(outDIR, "SPI.rds"), compression = 7)
	dataCDTdir <- file.path(outDIR, 'CDTDATASET')
	dir.create(dataCDTdir, showWarnings = FALSE, recursive = TRUE)

	#####################

	toAggr <- list(input.file, freqData)

	if(freqData != 'monthly'){
		if(is.null(EnvSPICalcPlot$toAggr)){
			aggregatData <- TRUE
		}else{
			aggregatData <- if(!isTRUE(all.equal(EnvSPICalcPlot$toAggr, toAggr))) TRUE else FALSE
		}
	}else aggregatData <- FALSE

	if(aggregatData){
		InsertMessagesTxt(main.txt.out, "Aggregate to monthly data ......")

		index <- split(seq_along(daty), substr(daty, 1, 6))
		outdates <- names(index)
		if(freqData == "daily") nbmon <- sapply(paste0(outdates, "15"), nbDayOfMonth)
		if(freqData == "pentad") nbmon <- rep(6, length(outdates))
		if(freqData == "dekadal") nbmon <- rep(3, length(outdates))

		miss.mon <- sapply(seq_along(index), function(jj){
			length(index[[jj]])/nbmon[jj] < 0.95
		})

		##########
		dataOUT <- file.path(outDIR, "MONTHLY_data")
		dir.create(dataOUT, showWarnings = FALSE, recursive = TRUE)
		file.month <- file.path(dataOUT, "MONTHLY_data.rds")

		##########
		if(GeneralParameters$data.type == "cdtstation"){
			data.mon <- lapply(seq_along(index), function(jj){
				if(miss.mon[jj]) return(rep(NA, ncol(don$data)))
				xx <- don$data[index[[jj]], , drop = FALSE]
				ina <- colSums(!is.na(xx))/nrow(xx) <  0.95
				res <- colSums(xx, na.rm = TRUE)
				res[ina] <- NA
				return(res)
			})
			data.mon <- do.call(rbind, data.mon)
			don <- list(id = don$id, lon = don$lon, lat = don$lat, dates = outdates, data = data.mon)

			saveRDS(don, gzfile(file.month, compression = 7))
		}

		if(GeneralParameters$data.type == "cdtdataset"){
			datadir <- file.path(dataOUT, 'DATA')
			dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
			file.index <- file.path(dataOUT, "MONTHLY_data.rds")

			index.out <- don
			index.out$TimeStep <- "monthly"
			index.out$dateInfo$date <- outdates
			index.out$dateInfo$index <- seq_along(outdates)

			saveRDS(index.out, gzfile(file.month, compression = 7))

			##########
			chunkfile <- sort(unique(don$colInfo$index))
			chunkcalc <- split(chunkfile, ceiling(chunkfile/don$chunkfac))

			do.parChunk <- if(don$chunkfac > length(chunkcalc)) TRUE else FALSE
			do.parCALC <- if(do.parChunk) FALSE else TRUE

			toExports <- c("readCdtDatasetChunk.sequence", "writeCdtDatasetChunk.sequence", "doparallel")
			packages <- c("doParallel")

			is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 10))
			`%parLoop%` <- is.parallel$dofun
			ret <- foreach(chkj = seq_along(chunkcalc), .export = toExports, .packages = packages) %parLoop% {
				don.data <- readCdtDatasetChunk.sequence(chunkcalc[[chkj]], GeneralParameters$cdtdataset, do.par = do.parChunk)
				don.data <- don.data[don$dateInfo$index, , drop = FALSE]

				data.mon <- lapply(seq_along(index), function(jj){
					if(miss.mon[jj]) return(rep(NA, ncol(don.data)))
					xx <- don.data[index[[jj]], , drop = FALSE]
					ina <- colSums(!is.na(xx))/nrow(xx) <  0.95
					res <- colSums(xx, na.rm = TRUE)
					res[ina] <- NA
					return(res)
				})
				data.mon <- do.call(rbind, data.mon)

				writeCdtDatasetChunk.sequence(data.mon, chunkcalc[[chkj]], index.out, datadir, do.par = do.parChunk)
				rm(data.mon, don.data); gc()
			}
			if(is.parallel$stop) stopCluster(is.parallel$cluster)
			don <- index.out
			rm(index.out)
		}
		EnvSPICalcPlot$toAggr <- toAggr
		EnvSPICalcPlot$file.month <- file.month
		InsertMessagesTxt(main.txt.out, "Aggregating monthly data done!")
	}else{
		if(freqData != 'monthly'){
			file.month <- EnvSPICalcPlot$file.month
			don <- readRDS(file.month)
		}else{
			if(GeneralParameters$data.type == "cdtdataset") file.month <- GeneralParameters$cdtdataset
		}
	}

	#####################
	if(GeneralParameters$data.type == "cdtstation"){
		dataSTNdir <- file.path(outDIR, 'CDTSTATIONS')
		dir.create(dataSTNdir, showWarnings = FALSE, recursive = TRUE)
		file.SPI.csv <- file.path(dataSTNdir, paste0("SPI", GeneralParameters$tscale, ".csv"))
		file.SPI.rds <- file.path(dataCDTdir, paste0("SPI", GeneralParameters$tscale, ".rds"))

		data.spi <- SPI_monthly_function(don$data, GeneralParameters$tscale, GeneralParameters$distr)

		saveRDS(data.spi, gzfile(file.SPI.rds, compression = 7))

		xhead <- rbind(don$id, don$lon, don$lat)
		chead <- c('ID.STN', 'LON', 'DATE/LAT')
		infohead <- cbind(chead, xhead)

		xdata <- cbind(don$dates, data.spi)
		if(GeneralParameters$tscale > 1) xdata <- xdata[-(1:(GeneralParameters$tscale-1)), ]
		data.spi <- rbind(infohead, xdata)
		data.spi[is.na(data.spi)] <- -9999
		writeFiles(data.spi, file.SPI.csv)

		index.spi <- list(id = don$id, lon = don$lon, lat = don$lat, date = don$dates)

		# EnvSPICalcPlot$index.spi <- index.spi
	}

	if(GeneralParameters$data.type == "cdtdataset"){
		dataNCdir <- file.path(outDIR, 'DATA_NetCDF', paste0("SPI", GeneralParameters$tscale))
		dir.create(dataNCdir, showWarnings = FALSE, recursive = TRUE)
		dataSPIdir <- file.path(dataCDTdir, paste0("SPI", GeneralParameters$tscale))
		dir.create(dataSPIdir, showWarnings = FALSE, recursive = TRUE)
		file.spi.index <- file.path(dataCDTdir, "SPI.rds")

		index.spi <- don
		index.spi$varInfo$name <- "spi"
		index.spi$varInfo$prec <- "float"
		index.spi$varInfo$units <- ""
		index.spi$varInfo$longname <- "Standardized Precipitation Index"

		saveRDS(index.spi, gzfile(file.spi.index, compression = 7))

		#########################################

		chunkfile <- sort(unique(don$colInfo$index))
		chunkcalc <- split(chunkfile, ceiling(chunkfile/don$chunkfac))

		do.parChunk <- if(don$chunkfac > length(chunkcalc)) TRUE else FALSE
		do.parCALC <- if(do.parChunk) FALSE else TRUE

		toExports <- c("readCdtDatasetChunk.sequence", "writeCdtDatasetChunk.sequence", "doparallel", "SPI_monthly_function")
		packages <- c("doParallel")

		is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 10))
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(chkj = seq_along(chunkcalc), .export = toExports, .packages = packages) %parLoop% {
			don.data <- readCdtDatasetChunk.sequence(chunkcalc[[chkj]], file.month, do.par = do.parChunk)
			don.data <- don.data[don$dateInfo$index, , drop = FALSE]

			data.spi <- SPI_monthly_function(don.data, GeneralParameters$tscale, GeneralParameters$distr)

			writeCdtDatasetChunk.sequence(data.spi, chunkcalc[[chkj]], index.spi, dataSPIdir, do.par = do.parChunk)
			rm(data.spi, don.data); gc()
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

		######################

		x <- index.spi$coords$mat$x
		y <- index.spi$coords$mat$y
		nx <- length(x)
		ny <- length(y)
		dx <- ncdim_def("Lon", "degreeE", x)
		dy <- ncdim_def("Lat", "degreeN", y)
		xy.dim <- list(dx, dy)
		nc.grd <- ncvar_def(index.spi$varInfo$name, index.spi$varInfo$units, xy.dim, -9999, index.spi$varInfo$longname, "float", compression = 9)

		######################

		daty <- if(GeneralParameters$tscale > 1) index.spi$dateInfo$date[-(1:(GeneralParameters$tscale-1))] else index.spi$dateInfo$date

		chunkfile <- sort(unique(index.spi$colInfo$index))
		datyread <- split(daty, ceiling(seq_along(daty)/50))

		do.parChunk <- if(length(chunkfile) > length(datyread)) TRUE else FALSE
		do.parCALC <- if(do.parChunk) FALSE else TRUE

		toExports <- c("readCdtDatasetChunk.sepdir.dates.order", "doparallel")
		packages <- c("doParallel", "ncdf4")

		is.parallel <- doparallel(do.parCALC & (length(datyread) > 30))
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(jj = seq_along(datyread), .export = toExports, .packages = packages) %parLoop% {
			daty0 <- datyread[[jj]]
			dat.spi <- readCdtDatasetChunk.sepdir.dates.order(file.spi.index, dataSPIdir, daty0, do.par = do.parChunk)

			for(j in seq_along(daty0)){
				spi <- dat.spi[j, ]
				dim(spi) <- c(nx, ny)
				spi[is.na(spi)] <- -9999

				filenc <- file.path(dataNCdir, paste0("spi_", daty0[j], ".nc"))
				nc <- nc_create(filenc, nc.grd)
				ncvar_put(nc, nc.grd, spi)
				nc_close(nc)
			}
			rm(daty0, dat.spi, spi); gc()
			return(0)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

		# EnvSPICalcPlot$index.spi <- index.spi
	}

	saveRDS(index.spi, out.spi.index)
	return(0)
}



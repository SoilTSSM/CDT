

cdtDataset_readData <- function(GeneralParameters)
{
	stt <- Sys.time()

	datarepo <- file.path(GeneralParameters$output$dir, GeneralParameters$output$data.name)
	datadir <- file.path(datarepo, 'DATA')
	datafileIdx <- file.path(datarepo, paste0(GeneralParameters$output$data.name, '.rds'))

	#######
	if(GeneralParameters$Update){
		InsertMessagesTxt(main.txt.out, "Update CDT dataset.....")
		if(!dir.exists(datarepo)){
			InsertMessagesTxt(main.txt.out, "Directory containing the data not found", format = TRUE)
			return(NULL)
		}
		if(!dir.exists(datadir)){
			InsertMessagesTxt(main.txt.out, "Directory 'DATA' not found", format = TRUE)
			return(NULL)
		}
		if(!file.exists(datafileIdx)){
			InsertMessagesTxt(main.txt.out, paste("No file",
							paste0(GeneralParameters$output$data.name, '.rds'), "found"), format = TRUE)
			return(NULL)
		}
		cdtTmpVar <- try(readRDS(datafileIdx), silent = TRUE)
		if(inherits(cdtTmpVar, "try-error")){
			InsertMessagesTxt(main.txt.out, paste("Unable to read", datafileIdx), format = TRUE)
			return(NULL)
		}
		chunksize <- cdtTmpVar$chunksize
	}else{
		InsertMessagesTxt(main.txt.out, "Create CDT dataset.....")
		if(!dir.exists(datarepo)) dir.create(datarepo, showWarnings = FALSE, recursive = TRUE)
		if(!dir.exists(datadir)) dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
		cdtTmpVar <- NULL
		cdtTmpVar$TimeStep <- GeneralParameters$Tstep
		chunksize <- GeneralParameters$chunk$chunksize
		cdtTmpVar$chunkfac <- GeneralParameters$chunk$chunkfac
	}

	##################
	ncDataInfo <- getRFESampleData(GeneralParameters$NCDF$sample)
	if(is.null(ncDataInfo)){
		InsertMessagesTxt(main.txt.out, "No netcdf data sample found", format = TRUE)
		return(NULL)
	}

	##################
	tstep <- GeneralParameters$Tstep
	start.year <- GeneralParameters$date.range$start.year
	start.mon <- GeneralParameters$date.range$start.mon
	start.dek <- GeneralParameters$date.range$start.dek
	end.year <- GeneralParameters$date.range$end.year
	end.mon <- GeneralParameters$date.range$end.mon
	end.dek <- GeneralParameters$date.range$end.dek
	months <- GeneralParameters$date.range$Months
	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	NCDF.DIR <- GeneralParameters$NCDF$dir
	NCDF.Format <- GeneralParameters$NCDF$format

	errmsg <- "NCDF data not found"
	ncInfo <- ncFilesInfo(tstep, start.date, end.date, months, NCDF.DIR, NCDF.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- list(xo = ncDataInfo$rfeILon, yo = ncDataInfo$rfeILat, varid = ncDataInfo$rfeVarid)

	ncInfo$dates <- ncInfo$dates[ncInfo$exist]
	ncInfo$nc.files <- ncInfo$nc.files[ncInfo$exist]
	ncInfo$exist <- ncInfo$exist[ncInfo$exist]

	###################
	if(GeneralParameters$Update){
		readDate <- !ncInfo$dates%in%cdtTmpVar$dateInfo$date
		if(!any(readDate)){
			InsertMessagesTxt(main.txt.out, "Data already up to date")
			return(NULL)
		}
		ncInfo$dates <- ncInfo$dates[readDate]
		ncInfo$nc.files <- ncInfo$nc.files[readDate]
		ncInfo$exist <- ncInfo$exist[readDate]
	}

	##################
	nc <- nc_open(ncInfo$nc.files[1])
	nc.lon <- nc$var[[ncInfo$ncinfo$varid]]$dim[[ncInfo$ncinfo$xo]]$vals
	nc.lat <- nc$var[[ncInfo$ncinfo$varid]]$dim[[ncInfo$ncinfo$yo]]$vals
	varInfo <- nc$var[[ncInfo$ncinfo$varid]][c('name', 'prec', 'units', 'longname')]
	nc_close(nc)

	xo <- order(nc.lon)
	nc.lon <- nc.lon[xo]
	yo <- order(nc.lat)
	nc.lat <- nc.lat[yo]
	len.lon <- length(nc.lon)
	len.lat <- length(nc.lat)

	##################

	if(GeneralParameters$Update){
		SP1 <- cdtTmpVar$coords$mat
		SP1 <- defSpatialPixels(list(lon = SP1$x, lat = SP1$y))
		SP2 <- defSpatialPixels(list(lon = nc.lon, lat = nc.lat))
		if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
			InsertMessagesTxt(main.txt.out, "Data have different resolution or bbox", format = TRUE)
			return(NULL)
		}
		rm(SP1, SP2)
	}

	###################

	## sequential chunk
	# col.id <- seq(len.lon*len.lat)
	# col.idx <- split(col.id, ceiling(col.id / chunksize))
	# col.grp <- rep(seq_along(col.idx), sapply(col.idx, length))
	# col.order <- col.id
	# xy.exp <- expand.grid(x = nc.lon, y = nc.lat)

	## square chunk
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

	###################

	if(!GeneralParameters$Update){
		cdtTmpVar$chunksize <- nxy.chunksize*nxy.chunksize
		cdtTmpVar$coords$mat <- list(x = nc.lon, y = nc.lat)
		cdtTmpVar$coords$df <- xy.exp
		attr(cdtTmpVar$coords$df, "out.attrs") <- NULL
		cdtTmpVar$colInfo <- list(id = col.id, index = col.grp, order = col.order)
		cdtTmpVar$varInfo <- varInfo
	}

	#########################################################

	# is.parallel <- doparallel(length(ncInfo$nc.files) >= 180)
	# `%parLoop%` <- is.parallel$dofun
	# ncDaty <- foreach(jj = seq_along(ncInfo$nc.files), .packages = "ncdf4") %parLoop% {
	# 	nc <- try(nc_open(ncInfo$nc.files[jj]), silent = TRUE)
	# 	if(inherits(nc, "try-error")) return(NULL)
	# 	vars <- ncvar_get(nc, varid = ncInfo$ncinfo$varid)
	# 	nc_close(nc)
	# 	vars <- vars[xo, yo]
	# 	if(ncInfo$ncinfo$yo < ncInfo$ncinfo$xo){
	# 		vars <- matrix(c(vars), nrow = len.lon, ncol = len.lat, byrow = TRUE)
	# 	}
	# 	vars <- round(c(vars), 1)

	# 	ret <- lapply(seq_along(col.idx), function(j){
	# 		file.tmp <- file.path(datadir, paste0(j, "_", ncInfo$dates[jj]))
	# 		con <- gzfile(file.tmp, open = "wb")
	# 		saveRDS(vars[col.idx[[j]]], con)
	# 		close(con)
	# 		return(0)
	# 	})

	# 	rm(vars); gc()
	# 	return(ncInfo$dates[jj])
	# }
	# if(is.parallel$stop) stopCluster(is.parallel$cluster)

	# ###################

	# ncDaty <- do.call(c, ncDaty)

	# ###################

	# is.parallel <- doparallel(length(col.idx) >= 20)
	# `%parLoop%` <- is.parallel$dofun
	# ret <- foreach(j = seq_along(col.idx)) %parLoop% {
	# 	tmp <- lapply(ncDaty, function(jj){
	# 		file.tmp <- file.path(datadir, paste0(j, "_", jj))
	# 		dd <- readRDS(file.tmp)
	# 		unlink(file.tmp)
	# 		return(dd)
	# 	})
	# 	tmp <- do.call(rbind, tmp)

	# 	file.rds <- file.path(datadir, paste0(j, ".rds"))
	# 	if(GeneralParameters$Update){
	# 		y <- readRDS(file.rds)
	# 		tmp <- rbind(y, tmp)
	# 	}

	# 	con <- gzfile(file.rds, compression = 6)
	# 	open(con, "wb")
	# 	saveRDS(tmp, con)
	# 	close(con)
	# 	rm(tmp, y); gc()
	# 	return(0)
	# }
	# if(is.parallel$stop) stopCluster(is.parallel$cluster)

	#########################################################

	# dir.size <- system(paste("du -sh", datadir), intern = TRUE)
	# InsertMessagesTxt(main.txt.out, paste("Before:", strsplit(dir.size, "\t")[[1]][1], Sys.time()-stt))

	chunkdate <- split(seq_along(ncInfo$dates), ceiling(seq_along(ncInfo$dates)/30))

	is.parallel <- doparallel(length(chunkdate) >= 10)
	`%parLoop%` <- is.parallel$dofun
	ret <- foreach(jj = seq_along(chunkdate), .packages = "ncdf4") %parLoop% {
		retdaty <- lapply(chunkdate[[jj]], function(j){
			nc <- try(nc_open(ncInfo$nc.files[j]), silent = TRUE)
			if(inherits(nc, "try-error")) return(NULL)
			vars <- ncvar_get(nc, varid = ncInfo$ncinfo$varid)
			nc_close(nc)
			vars <- vars[xo, yo]
			if(ncInfo$ncinfo$yo < ncInfo$ncinfo$xo){
				vars <- matrix(c(vars), nrow = len.lon, ncol = len.lat, byrow = TRUE)
			}
			vars <- round(c(vars), 4)

			file.tmp <- file.path(datadir, paste0(jj, ".gz"))
			con <- gzfile(file.tmp, open = "a", compression = 6)
			cat(c(vars, "\n"), file = con)
			close(con)
			return(ncInfo$dates[j])
		})

		retdaty <- do.call(c, retdaty)
		saveRDS(retdaty, file = file.path(datadir, paste0(jj, "_d.rds")))
		return(0)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	###################
	# dir.size <- system(paste("du -sh", datadir), intern = TRUE)
	# InsertMessagesTxt(main.txt.out, paste("Middle:", strsplit(dir.size, "\t")[[1]][1], Sys.time()-stt))

	ncDaty <- lapply(seq_along(chunkdate), function(jj){
		file.tmp <- file.path(datadir, paste0(jj, "_d.rds"))
		dd <- readRDS(file.tmp)
		unlink(file.tmp)
		return(dd)
	})
	ncDaty <- do.call(c, ncDaty)

	###################

	toExports <- c("col.idx", "datadir")
	is.parallel <- doparallel(length(col.idx) >= 50)
	`%parLoop%` <- is.parallel$dofun

	ret <- lapply(seq_along(chunkdate), function(jj){
		file.gz <- file.path(datadir, paste0(jj, ".gz"))
		R.utils::gunzip(file.gz)
		file.tmp <- tools::file_path_sans_ext(file.gz)
		tmp <- data.table::fread(file.tmp, header = FALSE, sep = " ", stringsAsFactors = FALSE, colClasses = "numeric")
		unlink(file.tmp)
		tmp <- as.matrix(tmp)
		dimnames(tmp) <- NULL

		ret <- foreach(j = seq_along(col.idx), .export = toExports) %parLoop% {
			chk <- tmp[, col.idx[[j]], drop = FALSE]
			file.rds <- file.path(datadir, paste0(j, ".rds"))
			if(file.exists(file.rds)){
				y <- readRDS(file.rds)
				chk <- rbind(y, chk)
			}

			con <- gzfile(file.rds, compression = 7)
			open(con, "wb")
			saveRDS(chk, con)
			close(con)

			return(0)
		}

		stt0 <- Sys.time()-stt
		InsertMessagesTxt(main.txt.out, paste("Date index:", jj, "| Elapsed time:", as.character(stt0), attr(stt0, "units")))

		rm(tmp); gc()
		return(0)
	})

	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	# dir.size <- system(paste("du -sh", datadir), intern = TRUE)
	# InsertMessagesTxt(main.txt.out, paste("After:", strsplit(dir.size, "\t")[[1]][1], Sys.time()-stt))

	#########################################################

	idx <- seq(length(ncDaty))
	if(GeneralParameters$Update){
		Adates <- c(cdtTmpVar$dateInfo$date, ncDaty)
		Aindex <- c(cdtTmpVar$dateInfo$index, max(cdtTmpVar$dateInfo$index)+idx)
	}else{
		Adates <- ncDaty
		Aindex <- idx
	}
	odaty <- order(Adates)
	cdtTmpVar$dateInfo <- list(date = Adates[odaty], index = Aindex[odaty])

	con <- gzfile(datafileIdx, compression = 6)
	open(con, "wb")
	saveRDS(cdtTmpVar, con)
	close(con)

	stt <- Sys.time()-stt
	InsertMessagesTxt(main.txt.out, paste("Elapsed time:", as.character(stt), attr(stt, "units")))

	rm(ncDaty, idx, odaty, Adates, Aindex, cdtTmpVar, ncDataInfo, ncInfo)
	gc()
	return(0)
}

###########################################################

##### read chunk files (sequential)
# chunk files
# fileInfo <- "~/PRECIP/PRECIP.rds"
# cdtData <- readRDS(fileInfo) OR separate cdtdataset info files
# don <- readCdtDatasetChunk.sequence(loc, fileInfo, cdtData, do.par = TRUE)
# don <- readCdtDatasetChunk.sequence(loc, fileInfo, do.par = TRUE)
# return matrix,  row: all dates, col: sum of chunk col number

readCdtDatasetChunk.sequence <- function(chunk, fileInfo, cdtData = NULL, do.par = TRUE)
{
	if(is.null(cdtData)) cdtData <- readRDS(fileInfo)
	datadir <- file.path(dirname(fileInfo), "DATA")

	is.parallel <- doparallel(do.par & (length(chunk) >= 20))
	`%parLoop%` <- is.parallel$dofun
	don <- foreach(j = chunk) %parLoop% {
		file.rds <- file.path(datadir, paste0(j, ".rds"))
		x <- readRDS(file.rds)
		x
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	do.call(cbind, don)
}

####################

writeCdtDatasetChunk.sequence <- function(mat, chunk, cdtData, datadir, do.par = TRUE)
{
	col.grp <- cdtData$colInfo$index[cdtData$colInfo$index%in%chunk]
	col.grp <- split(seq(ncol(mat)), col.grp)

	is.parallel <- doparallel(do.par & (length(chunk) >= 20))
	`%parLoop%` <- is.parallel$dofun
	don <- foreach(j = seq_along(chunk)) %parLoop% {
		tmp <- mat[, col.grp[[j]], drop = FALSE]
		file.rds <- file.path(datadir, paste0(chunk[j], ".rds"))
		con <- gzfile(file.rds, compression = 5)
		open(con, "wb")
		saveRDS(tmp, con)
		close(con)
		rm(tmp); gc()
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	return(0)
}

####################
##### read several dates, fileInfo and datadir are located in a separated directories
# dates: day, pentad, dekad, month 
# dates <- c('2006021', ...., '2006061')
# fileInfo <- "~/PRECIP/PRECIP.rds"
# datadir <- "~/ClimatoAnalysis_monthly_PRECIP_dek/Aggregated_PRECIP_dek/DATA"
# don <- readCdtDatasetChunk.sepdir.dates.order(fileInfo, dates, do.par = TRUE)
# return matrix,  row: date, col: expand x y coords reorder

readCdtDatasetChunk.sepdir.dates.order <- function(fileInfo, datadir, dates, do.par = TRUE, coords = FALSE, onedate = FALSE)
{
	cdtdata <- readRDS(fileInfo)
	chunk <- seq(max(cdtdata$colInfo$index))
	idaty <- cdtdata$dateInfo$index[match(dates, cdtdata$dateInfo$date)]
	# dates <- dates[!is.na(idaty)]
	idaty <- idaty[!is.na(idaty)]
	if(length(idaty) == 0) return(NULL)
	if(onedate) idaty <- idaty[1]

	if(do.par){
		is.parallel <- doparallel(length(chunk) >= 50)
		`%parLoop%` <- is.parallel$dofun
		don <- foreach(j = chunk) %parLoop% {
			file.rds <- file.path(datadir, paste0(j, ".rds"))
			x <- readRDS(file.rds)
			x[idaty, , drop = FALSE]
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
	}else{
		don <- lapply(chunk, function(j){
			file.rds <- file.path(datadir, paste0(j, ".rds"))
			x <- readRDS(file.rds)
			x[idaty, , drop = FALSE]
		})
	}

	don <- do.call(cbind, don)
	don <- don[, cdtdata$colInfo$order, drop = FALSE]
	if(onedate){
		dim(don) <- sapply(cdtdata$coords$mat, length)
		return(c(cdtdata$coords$mat, list(z = don)))
	}
	if(coords) return(c(cdtdata$coords$mat, list(z = don)))
	return(don)
}

####################
##### read several dates
# dates: day, pentad, dekad, month 
# dates <- c('2006021', ...., '2006061')
# fileInfo <- "~/PRECIP/PRECIP.rds"
# don <- readCdtDatasetChunk.multi.dates.order(fileInfo, dates)
# return matrix,  row: date (same dates length), col: expand x y coords
# coords = TRUE; list(x = xcoord, y = ycoord, z = matrix{row: date (same dates length), col: expand x y coords})
# onedate = TRUE; list(x = xcoord, y = ycoord, z = matrix), used by image

readCdtDatasetChunk.multi.dates.order <- function(fileInfo, dates, do.par = TRUE, coords = FALSE, onedate = FALSE)
{
	datadir <- file.path(dirname(fileInfo), "DATA")
	cdtdata <- readRDS(fileInfo)
	chunk <- seq(max(cdtdata$colInfo$index))
	idaty <- cdtdata$dateInfo$index[match(dates, cdtdata$dateInfo$date)]
	# dates <- dates[!is.na(idaty)]
	idaty <- idaty[!is.na(idaty)]
	if(onedate) idaty <- idaty[1]

	if(do.par){
		is.parallel <- doparallel(length(chunk) >= 50)
		`%parLoop%` <- is.parallel$dofun
		don <- foreach(j = chunk) %parLoop% {
			file.rds <- file.path(datadir, paste0(j, ".rds"))
			x <- readRDS(file.rds)
			x[idaty, , drop = FALSE]
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
	}else{
		don <- lapply(chunk, function(j){
			file.rds <- file.path(datadir, paste0(j, ".rds"))
			x <- readRDS(file.rds)
			x[idaty, , drop = FALSE]
		})
	}

	don <- do.call(cbind, don)
	don <- don[, cdtdata$colInfo$order, drop = FALSE]
	if(onedate){
		dim(don) <- sapply(cdtdata$coords$mat, length)
		return(c(cdtdata$coords$mat, list(z = don)))
	}
	if(coords) return(c(cdtdata$coords$mat, list(z = don)))
	return(don)
}

########################
##### read pixels
# loc: index of column from expand x y coords, (from sp::over OR cdt.which | findInterval)
# fileInfo <- "~/PRECIP/PRECIP.rds"
# cdtData <- readRDS(fileInfo) OR separate cdtdataset info files
# don <- readCdtDatasetChunk.locations(loc, fileInfo, cdtData, do.par = TRUE)
# don <- readCdtDatasetChunk.locations(loc, fileInfo, do.par = TRUE)
# return matrix,  row: all dates, col: correspond to loc (same length as loc)


readCdtDatasetChunk.locations <- function(loc, fileInfo, cdtData = NULL, chunkDir = "DATA", do.par = TRUE)
{
	if(is.null(cdtData)) cdtData <- readRDS(fileInfo)
	datadir <- file.path(dirname(fileInfo), chunkDir)

	id <- match(loc, cdtData$colInfo$id)
	col.id <- split(cdtData$colInfo$id[id], cdtData$colInfo$index[id])
	chunk <- as.numeric(names(col.id))
	grp <- cdtData$colInfo$index%in%chunk
	col.grp <- split(cdtData$colInfo$id[grp], cdtData$colInfo$index[grp])
	# idx <- lapply(seq_along(chunk), function(j) which(col.grp[[j]]%in%col.id[[j]]))
	idx <- lapply(seq_along(chunk), function(j) match(col.id[[j]], col.grp[[j]]))
	xcrd <- do.call(c, lapply(seq_along(chunk), function(j) col.grp[[j]][idx[[j]]]))
	coords <- cdtData$coords$df[match(xcrd, cdtData$colInfo$id), , drop = FALSE]
	rownames(coords) <- NULL

	if(do.par){
		is.parallel <- doparallel(length(chunk) >= 50)
		`%parLoop%` <- is.parallel$dofun
		don <- foreach(j = seq_along(chunk)) %parLoop% {
			file.rds <- file.path(datadir, paste0(chunk[j], ".rds"))
			x <- readRDS(file.rds)
			x[, idx[[j]], drop = FALSE]
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
	}else{
		don <- lapply(seq_along(chunk), function(j){
			file.rds <- file.path(datadir, paste0(chunk[j], ".rds"))
			x <- readRDS(file.rds)
			x[, idx[[j]], drop = FALSE]
		})
	}
	don <- do.call(cbind, don)
	list(coords = coords, data = don)
}



######################################################################################################################

# ########################
# ##### read pixels
# # loc: index of column from expand x y coords, (from sp::over OR findInterval)
# # fileInfo <- "~/PRECIP/PRECIP.rds"
# # cdtData <- readRDS(fileInfo) OR separate cdtdataset info files
# # don <- readCdtDatasetChunk.locations(loc, fileInfo, cdtData, do.par = TRUE)
# # don <- readCdtDatasetChunk.locations(loc, fileInfo, do.par = TRUE)
# # return matrix,  row: all dates, col: correspond to loc (same length as loc)


# #########
# ## old
# readCdtDatasetChunk.locations <- function(loc, fileInfo, cdtData = NULL, do.par = TRUE)
# {
# 	if(is.null(cdtData)) cdtData <- readRDS(fileInfo)
# 	datadir <- file.path(dirname(fileInfo), "DATA")

# 	col.id <- match(loc, cdtData$colInfo$id)
# 	col.idx <- cdtData$colInfo$index[col.id]
# 	col.grp <- split(col.id, col.idx)
# 	col.grp <- lapply(col.grp, function(l){
# 		ix <- (l - cdtData$chunksize) %% cdtData$chunksize
# 		ifelse(ix == 0, cdtData$chunksize, ix)
# 	})
# 	# chunk <- unique(col.idx)
# 	chunk <- as.numeric(names(col.grp))

# 	is.parallel <- doparallel(do.par & (length(chunk) >= 20))
# 	`%parLoop%` <- is.parallel$dofun
# 	don <- foreach(j = seq_along(chunk)) %parLoop% {
# 		file.rds <- file.path(datadir, paste0(chunk[j], ".rds"))
# 		# con <- file(file.rds)
# 		# open(con, "rb")
# 		# x <- readRDS(con)
# 		# close(con)
# 		x <- readRDS(file.rds)
# 		x[, col.grp[[j]], drop = FALSE]
# 	}
# 	if(is.parallel$stop) stopCluster(is.parallel$cluster)
# 	do.call(cbind, don)
# }


###########################################################

# ##### read several dates
# # dates: day, pentad, dekad, month 
# # dates <- c('2006021', ...., '2006061')
# # fileInfo <- "~/PRECIP/PRECIP.rds"
# # don <- readCdtDatasetChunk.multi.dates(fileInfo, dates, do.par = TRUE)
# # return matrix,  row: date (same dates length), col: expand x y coords

# readCdtDatasetChunk.multi.dates <- function(fileInfo, dates, do.par = TRUE){
# 	datadir <- file.path(dirname(fileInfo), "DATA")
# 	cdtdata <- readRDS(fileInfo)
# 	chunk <- seq(max(cdtdata$colInfo$index))
# 	idaty <- cdtdata$dateInfo$index[match(dates, cdtdata$dateInfo$date)]
# 	dates <- dates[!is.na(idaty)]
# 	idaty <- idaty[!is.na(idaty)]

# 	is.parallel <- doparallel(do.par & (length(chunk) >= 20))
# 	`%parLoop%` <- is.parallel$dofun
# 	don <- foreach(j = seq_along(chunk)) %parLoop% {
# 		file.rds <- file.path(datadir, paste0(chunk[j], ".rds"))
# 		x <- readRDS(file.rds)
# 		x[idaty, , drop = FALSE]
# 	}
# 	if(is.parallel$stop) stopCluster(is.parallel$cluster)
# 	do.call(cbind, don)
# }

###############

# ##### read several dates, fileInfo and datadir are located in a separated directories
# # dates: day, pentad, dekad, month 
# # dates <- c('2006021', ...., '2006061')
# # fileInfo <- "~/PRECIP/PRECIP.rds"
# # datadir <- "~/ClimatoAnalysis_monthly_PRECIP_dek/Aggregated_PRECIP_dek/DATA"
# # don <- readCdtDatasetChunk.sep_dir.dates(fileInfo, dates, do.par = TRUE)
# # return matrix,  row: date, col: expand x y coords

# readCdtDatasetChunk.sep_dir.dates <- function(fileInfo, datadir, dates, do.par = TRUE){
# 	# datadir <- file.path(dirname(fileInfo), "DATA")
# 	cdtdata <- readRDS(fileInfo)
# 	chunk <- seq(max(cdtdata$colInfo$index))
# 	idaty <- cdtdata$dateInfo$index[match(dates, cdtdata$dateInfo$date)]
# 	dates <- dates[!is.na(idaty)]
# 	idaty <- idaty[!is.na(idaty)]

# 	is.parallel <- doparallel(do.par & (length(chunk) >= 20))
# 	`%parLoop%` <- is.parallel$dofun
# 	don <- foreach(j = seq_along(chunk)) %parLoop% {
# 		file.rds <- file.path(datadir, paste0(chunk[j], ".rds"))
# 		x <- readRDS(file.rds)
# 		x[idaty, , drop = FALSE]
# 	}
# 	if(is.parallel$stop) stopCluster(is.parallel$cluster)
# 	do.call(cbind, don)
# }

###############

##### read one date
# dates: day, pentad, dekad, month 
# dates <- '2006021'
# fileInfo <- "~/PRECIP/PRECIP.rds"
# don <- readCdtDatasetChunk.one.date(fileInfo, dates)
# return list(x = vector, y = vector, z = matrix)

# readCdtDatasetChunk.one.date <- function(fileInfo, dates){
# 	datadir <- file.path(dirname(fileInfo), "DATA")
# 	cdtdata <- readRDS(fileInfo)
# 	chunk <- seq(max(cdtdata$colInfo$index))
# 	idaty <- cdtdata$dateInfo$index[match(dates, cdtdata$dateInfo$date)]
# 	dates <- dates[!is.na(idaty)]
# 	idaty <- idaty[!is.na(idaty)]

# 	don <- foreach(j = seq_along(chunk)) %do% {
# 		file.rds <- file.path(datadir, paste0(chunk[j], ".rds"))
# 		x <- readRDS(file.rds)
# 		x[idaty, , drop = FALSE]
# 	}

# 	don <- do.call(cbind, don)
# 	dim(don) <- sapply(cdtdata$coords$mat, length)
# 	don <- c(cdtdata$coords$mat, list(z = don))
# 	return(don)
# }

###########################################################

# readCdtDatasetChunk for PICSA (a changer)

readCdtDatasetChunk.picsa <- function(col, colInfo, indir, chunksize = 100, chunk.par = TRUE)
{
	col.id <- match(col, colInfo$id)
	col.idx <- colInfo$index[col.id]
	col.grp <- split(col.id, col.idx)
	col.grp <- lapply(col.grp, function(l){
		ix <- (l-chunksize)%%chunksize
		ifelse(ix == 0, chunksize, ix)
	})
	chunk <- unique(col.idx)

	is.parallel <- doparallel(chunk.par & (length(chunk) >= 10))
	`%parLoop%` <- is.parallel$dofun
	don <- foreach(j = seq_along(chunk)) %parLoop% {
		file.rds <- file.path(indir, paste0(chunk[j], ".rds"))
		# con <- file(file.rds)
		# open(con, "rb")
		# x <- readRDS(con)
		# close(con)
		x <- readRDS(file.rds)
		x[, col.grp[[j]], drop = FALSE]
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	do.call(cbind, don)
}

## in picsa

writeCdtDatasetChunk.create <- function(x, outdir, chunksize = 100, chunk.par = TRUE)
{
	col.id <- seq(ncol(x))
	col.grp <- split(col.id, ceiling(col.id/chunksize))
	col.idx <- rep(seq_along(col.grp), sapply(col.grp, length))

	is.parallel <- doparallel(chunk.par & (length(col.grp) >= 10))
	`%parLoop%` <- is.parallel$dofun
	ret <- foreach(j = seq_along(col.grp)) %parLoop% {
		tmp <- x[, col.grp[[j]], drop = FALSE]
		file.rds <- file.path(outdir, paste0(j, ".rds"))
		con <- gzfile(file.rds, compression = 5)
		open(con, "wb")
		saveRDS(tmp, con)
		close(con)
		rm(tmp); gc()
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	return(list(id = col.id, index = col.idx))
}


writeCdtDatasetChunk.rbind <- function(x, outdir, chunksize = 100, chunk.par = TRUE)
{
	col.id <- seq(ncol(x))
	col.grp <- split(col.id, ceiling(col.id/chunksize))
	col.idx <- rep(seq_along(col.grp), sapply(col.grp, length))

	is.parallel <- doparallel(chunk.par & (length(col.grp) >= 10))
	`%parLoop%` <- is.parallel$dofun
	ret <- foreach(j = seq_along(col.grp)) %parLoop% {
		file.rds <- file.path(outdir, paste0(j, ".rds"))
		y <- readRDS(file.rds)
		z <- x[, col.grp[[j]], drop = FALSE]
		tmp <- rbind(y, z)
		con <- gzfile(file.rds, compression = 5)
		open(con, "wb")
		saveRDS(tmp, con)
		close(con)
		rm(y, z, tmp); gc()
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	return(list(id = col.id, index = col.idx))
}


###########################################################

# writeCdtDatasetChunk.locations <- function(mat, loc, cdtData, datadir, do.par = TRUE)
# {
# 	col.id <- match(loc, cdtData$colInfo$id)
# 	col.idx <- cdtData$colInfo$index[col.id]
# 	col.grp <- split(seq_along(col.id), col.idx)
# 	# chunk <- unique(col.idx)
# 	chunk <- as.numeric(names(col.grp))

# 	is.parallel <- doparallel(do.par & (length(chunk) >= 20))
# 	`%parLoop%` <- is.parallel$dofun
# 	don <- foreach(j = seq_along(chunk)) %parLoop% {
# 		tmp <- mat[, col.grp[[j]], drop = FALSE]
# 		file.rds <- file.path(datadir, paste0(chunk[j], ".rds"))
# 		con <- gzfile(file.rds, compression = 5)
# 		open(con, "wb")
# 		saveRDS(tmp, con)
# 		close(con)
# 		rm(tmp); gc()
# 	}
# 	if(is.parallel$stop) stopCluster(is.parallel$cluster)
# 	# return(list(id = col.id, index = col.idx))
# 	return(0)
# }

##################



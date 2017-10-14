
cdtDataset_readData <- function(GeneralParameters)
{
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
		cdtTmpVar$chunksize <- chunksize
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
	nc.lon <- nc$dim[[ncInfo$ncinfo$xo]]$vals
	nc.lat <- nc$dim[[ncInfo$ncinfo$yo]]$vals
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

	col.id <- seq(len.lon*len.lat)
	## mifamadika ny col.grp sy col.idx avy eto
	## miova manaraka an'izay aby ny rehetra efa vita taloha
	## ampifamadihana ireo
	col.idx <- split(col.id, ceiling(col.id / chunksize))
	col.grp <- rep(seq_along(col.idx), sapply(col.idx, length))

	###################

	if(!GeneralParameters$Update){
		cdtTmpVar$coords$mat <- list(x = nc.lon, y = nc.lat)
		cdtTmpVar$coords$df <- expand.grid(x = nc.lon, y = nc.lat)
		attr(cdtTmpVar$coords$df, "out.attrs") <- NULL
		cdtTmpVar$colInfo <- list(id = col.id, index = col.grp)
		cdtTmpVar$varInfo <- varInfo
	}

	###################

	is.parallel <- doparallel(length(ncInfo$nc.files) >= 180)
	`%parLoop%` <- is.parallel$dofun
	ncDaty <- foreach(jj = seq_along(ncInfo$nc.files), .packages = "ncdf4") %parLoop% {
		nc <- nc_open(ncInfo$nc.files[jj])
		vars <- ncvar_get(nc, varid = ncInfo$ncinfo$varid)
		nc_close(nc)
		vars <- vars[xo, yo]
		if(ncInfo$ncinfo$yo < ncInfo$ncinfo$xo){
			vars <- matrix(c(vars), nrow = len.lon, ncol = len.lat, byrow = TRUE)
		}
		vars <- round(c(vars), 1)

		for(j in seq_along(col.idx)){
			file.tmp <- file.path(datadir, paste0(j, ".gz"))
			con <- gzfile(file.tmp, open = "a", compression = 5)
			cat(c(vars[col.idx[[j]]], '\n'), file = con)
			close(con)
		}
		rm(vars); gc()
		return(ncInfo$dates[jj])
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	ncDaty <- do.call(c, ncDaty)

	###################

	is.parallel <- doparallel(length(col.idx) >= 10)
	`%parLoop%` <- is.parallel$dofun
	ret <- foreach(j = seq_along(col.idx)) %parLoop% {
		file.gz <- file.path(datadir, paste0(j, ".gz"))
		R.utils::gunzip(file.gz)
		file.tmp <- tools::file_path_sans_ext(file.gz)
		tmp <- data.table::fread(file.tmp, header = FALSE, sep = " ", stringsAsFactors = FALSE, colClasses = "numeric")
		unlink(file.tmp)
		tmp <- as.matrix(tmp)
		dimnames(tmp) <- NULL
		file.rds <- file.path(datadir, paste0(j, ".rds"))
		if(GeneralParameters$Update){
			y <- readRDS(file.rds)
			tmp <- rbind(y, tmp)
		}

		con <- gzfile(file.rds, compression = 5)
		open(con, "wb")
		saveRDS(tmp, con)
		close(con)
		rm(tmp, y); gc()
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	###################

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

	con <- gzfile(datafileIdx, compression = 5)
	open(con, "wb")
	saveRDS(cdtTmpVar, con)
	close(con)

	rm(ncDaty, idx, odaty, Adates, Aindex, cdtTmpVar, ncDataInfo, ncInfo)
	gc()
	return(0)
}

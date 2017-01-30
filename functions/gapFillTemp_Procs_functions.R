fillDekTempMissVal <- function(GeneralParameters){
	memType <- 2

	freqData <- GeneralParameters$period
	nlen <- GeneralParameters$Fill.Params$dek.windows
	min.len <- GeneralParameters$Fill.Params$min.length

	#######get data
	stnData <- getStnOpenData(GeneralParameters$IO.files$STN.file)
	infoCap <- stnData[1:4, 1]
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	if(is.null(stnData)) return(NULL)
	missval <- getStnOpenDataInfo(GeneralParameters$IO.files$STN.file)[[3]]$miss.val
	missval <- -99

	date.stn <- stnData$dates
	data.stn0 <- data.stn <- stnData$data

	################
	start.year <- GeneralParameters$Fill.Date.Range$start.year
	start.mon <- GeneralParameters$Fill.Date.Range$start.mon
	start.dek <- GeneralParameters$Fill.Date.Range$start.dek
	end.year <- GeneralParameters$Fill.Date.Range$end.year
	end.mon <- GeneralParameters$Fill.Date.Range$end.mon
	end.dek <- GeneralParameters$Fill.Date.Range$end.dek
	months <- GeneralParameters$Fill.Months

	tempDir <- GeneralParameters$IO.files$Temp.dir
	tempfilefrmt <- GeneralParameters$IO.files$Temp.File.Format

	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	msg <- list(start = 'Read temperature data ...', end = 'Reading temperature data finished')
	errmsg <- "Temperature data not found"
	ncfiles <- list(freqData = freqData, start.date = start.date, end.date = end.date,
					months = months, ncDir = tempDir, ncFileFormat = tempfilefrmt)
	ncinfo <- list(xo = 1, yo = 2, varid = "temp")
	read.ncdf.parms <- list(ncfiles = ncfiles, ncinfo = ncinfo, msg = msg, errmsg = errmsg)

	if(memType == 2){
		TempData <- read.NetCDF.Data(read.ncdf.parms)
		if(is.null(TempData)) return(NULL)
		ijGrd <- grid2pointINDEX(list(lon = stnData$lon, lat = stnData$lat), list(lon = TempData$lon, lat = TempData$lat))
		data.tmp <- t(sapply(TempData$data, function(x) if(!is.null(x)) x[ijGrd] else rep(NA, length(ijGrd))))
	}else{
		TempData <- read.NetCDF.Data2Points(read.ncdf.parms, list(lon = stnData$lon, lat = stnData$lat))
		if(is.null(TempData)) return(NULL)
		data.tmp <- t(sapply(TempData$data, function(x) if(!is.null(x)) x else rep(NA, length(stnData$lon))))
	}

	###############
	InsertMessagesTxt(main.txt.out, 'Filling gaps ...')
	tcl("update")

	data.tmp <- data.tmp[TempData$dates%in%date.stn, , drop = FALSE]
	data.stn <- data.stn[date.stn%in%TempData$dates, , drop = FALSE]
	daty <- TempData$dates[TempData$dates%in%date.stn]
	if(nrow(data.tmp) == 0 | nrow(data.stn) == 0){
		InsertMessagesTxt(main.txt.out, "Gap filling failed: date range from station data and netcdf did not match", format = TRUE)
		tcl("update")
		return(NULL)
	}

	tofill <- lapply(seq(ncol(data.stn)), function(j){
		x <- data.stn[, j]
		ina <- is.na(x)
		if(!any(ina) | all(ina)) return(NULL)
		lapply(which(ina), function(i){
			ix <- which(substr(daty, 5, 7) == substr(daty[i], 5, 7))
			ix1 <- ix[ix <= i][which(!is.na(x[ix][ix <= i]))]
			ix2 <- ix[ix > i][which(!is.na(x[ix][ix > i]))]
			ix1 <- rev(rev(ix1)[1:nlen])
			ix2 <- ix2[1:nlen]
			ix <- c(ix1, ix2)
			ix <- ix[!is.na(ix)]
			if(length(ix) >= min.len) list(id = c(i, j), irow = ix) else NULL
		})
	})
	tofill <- do.call('c', tofill)
	tofill <- tofill[!sapply(tofill, is.null)]
	idx <- t(sapply(tofill, '[[', 1))
	if(ncol(idx) == 0){
		InsertMessagesTxt(main.txt.out, "Gap filling failed: data too short to perform regression", format = TRUE)
		tcl("update")
		return(NULL)
	}

	## foreach
	data.stn[idx] <- sapply(tofill, function(Obj){
		y <- data.stn[Obj$irow, Obj$id[2]]
		x <- data.tmp[Obj$irow, Obj$id[2]]
		m.glm <- glm(y~x)
		round(predict(m.glm, newdata = data.frame(x = data.tmp[Obj$id[1], Obj$id[2]])), 1)
	})
	data.stn0[date.stn%in%daty, ] <- data.stn
	data.stn0[is.na(data.stn0)] <- missval
	data.stn0 <- cbind(date.stn, data.stn0)

	headInfo <- do.call('rbind', stnData[c('id', 'lon', 'lat', 'elv')])
	headInfo <- cbind(infoCap[1:nrow(headInfo)], headInfo)
	stnData <- rbind(headInfo, data.stn0)
	writeFiles(stnData, GeneralParameters$IO.files$f2save)

	rm(stnData, headInfo, data.stn0, data.stn, tofill, data.tmp, TempData)
	return(0)
}



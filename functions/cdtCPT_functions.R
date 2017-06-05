
CPT.convertCDTdates <- function(dates){
	dates <- as.character(dates)
	ndate <- nchar(dates[1])
	cpt <- NULL
	# annual
	if(ndate == 4) cpt <- dates
	# month
	if(ndate == 6) cpt <- paste(substr(dates, 1, 4), substr(dates, 5, 6), sep = '-')
	# dekad
	if(ndate == 7){
		year <- substr(dates, 1, 4)
		mon <- substr(dates, 5, 6)
		dek <- as.numeric(substr(dates, 7, 7))
		eom <- sapply(seq_along(year), function(i){
			daty <- as.Date(paste(year[i], mon[i], 28:31, sep = '-'))
			rev((28:31)[which(!is.na(daty))])[1]
		})
		ddek <- dek
		ddek[dek == 1] <- '01/10'
		ddek[dek == 2] <- '11/20'
		ddek[dek == 3] <- paste(21, eom[dek == 3], sep = '/')
		cpt <- paste(year, mon, ddek, sep = '-')
	}
	# day
	if(ndate == 8) cpt <- paste(substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 8), sep = '-')
	# clim annual
	if(ndate == 9 & substr(dates[1], 5, 5) == "-") cpt <- paste(substr(dates, 1, 4), substr(dates, 6, 9), sep = '/')
	# clim month
	if(ndate == 12 & substr(dates[1], 10, 10) == "_") cpt <- substr(dates, 11, 12)
	# clim dek
	if(ndate == 13 & substr(dates[1], 10, 10) == "_"){
		mon <- substr(dates, 11, 12)
		dek <- as.numeric(substr(dates, 13, 13))
		eom <- sapply(seq_along(mon), function(i){
			daty <- as.Date(paste("2014", mon[i], 28:31, sep = '-'))
			rev((28:31)[which(!is.na(daty))])[1]
		})
		ddek <- dek
		ddek[dek == 1] <- '01/10'
		ddek[dek == 2] <- '11/20'
		ddek[dek == 3] <- paste(21, eom[dek == 3], sep = '/')
		cpt <- paste(mon, ddek, sep = '-')
	}
	# clim day
	if(ndate == 14 & substr(dates[1], 10, 10) == "_") cpt <- paste(substr(dates, 11, 12), substr(dates, 13, 14), sep = '-')
	if(ndate == 15){
		# season
		if(substr(dates[1], 8, 8) == "_"){
			year1 <- substr(dates, 1, 4)
			mon1 <- substr(dates, 6, 7)
			year2 <- substr(dates, 9, 12)
			mon2 <- substr(dates, 14, 15)
			if(all(year1 == year2)){
				# cpt <- if(all(mon1 == mon2)) paste0(year1, "-", mon1) else paste0(year1, "-", mon1, "/", mon2)
				if(all(mon1 == mon2)) cpt <- paste0(year1, "-", mon1)
				else{
					cpt <- if(all((as.numeric(mon2)-as.numeric(mon1)+1) %in% c(0, 12))) year1 else paste0(year1, "-", mon1, "/", mon2)
				}
			}else cpt <- paste0(year1, "-", mon1, "/", year2, "-", mon2)
		}
		# clim season
		if(substr(dates[1], 10, 10) == "_"){
			year1 <- substr(dates, 1, 4)
			year2 <- substr(dates, 6, 9)
			mon1 <- substr(dates, 11, 12)
			mon2 <- substr(dates, 14, 15)
			# cpt <- if(all(mon1 == "01") & all(mon2 == "12")) paste0(year1, "/", year2) else paste0(mon1, "/", mon2)
			if(all(mon1 == "01") & all(mon2 == "12")){
				cpt <- paste0(year1, "/", year2)
			}else{
				cpt <- if(all((as.numeric(mon2)-as.numeric(mon1)+1) %in% c(0, 12))) paste0(year1, "/", year2) else paste0(mon1, "/", mon2)
			}
		}
	}
	return(cpt)
}

CPT.getTAG.line <- function(cpt.tags){
	cpt.tags <- paste(paste('cpt', names(cpt.tags), sep = ':'), do.call('c', cpt.tags), sep = '=')
	tags <- paste(paste(cpt.tags, collapse = ', '), '\n', sep = '')
	return(tags)
}

CPT.formatStationData <-function(date, z, width = 13, side = "both"){
	xout <- formatC(c(z))
	xout <- str_pad(xout, width = width, side = side)
	xout <- paste("\t", xout, sep = '')
	dim(xout) <- dim(z)
	xout <- cbind(date, xout)
	xout <- paste(apply(xout, 1, paste, collapse = ""), '\n', sep = '')
	return(xout)
}

CPT.convertStationData <- function(data, date, stninfo, varid, units, missval){
	ncmax <- max(nchar(formatC(c(data))))
	daty <- CPT.convertCDTdates(date)
	id <- as.character(stninfo[, 1])
	lon <- as.character(stninfo[, 2])
	lat <- as.character(stninfo[, 3])
	xmlns <- "xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n"
	cpt.tags <- list(field = varid, nrow = length(date), ncol = length(lon),
					 row = 'T', col = 'station', units = units, missing = missval)
	cpt.tags <- CPT.getTAG.line(cpt.tags)
	cpt.stn <- paste('\t', paste(id, collapse = '\t'), '\n', sep = '')
	cpt.lon <- paste('cpt:X', '\t', paste(lon, collapse = '\t'), '\n', sep = '')
	cpt.lat <- paste('cpt:Y', '\t', paste(lat, collapse = '\t'), '\n', sep = '')
	cpt.data <- CPT.formatStationData(daty, data, width = ncmax)
	cpt.out <- c(xmlns, cpt.tags, cpt.stn, cpt.lon, cpt.lat, cpt.data)
	return(cpt.out)
}

CPT.arrangeDate <- function(date){
	cptT <- paste(c("cpt:T", date), collapse = "\t")
	paste(cptT, '\n', sep = '')
}

CPT.formatGridData <- function(x, y, z, width = 13, side = "both"){
	z <- t(z)
	xout <- formatC(c(z))
	xout <- str_pad(xout, width = width, side = side)
	xout <- paste("\t", xout, sep = '')
	dim(xout) <- dim(z)
	xout <- cbind(round(y, 6), xout)
	xout <- xout[nrow(xout):1, ]
	xout <- paste(apply(xout, 1, paste, collapse = ""), '\n', sep = '')
	xout <- c(paste("\t", paste(round(x, 6), collapse = "\t"), "\n", sep = ""), xout)
	return(xout)
}

CPT.convertGridData <- function(data, date, gridinfo, varid, units, missval){
	ncmax <- max(nchar(formatC(do.call(c, data))))
	daty <- CPT.convertCDTdates(date)
	xmlns <- "xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n"
	nfields <- paste("cpt:nfields=", 1, "\n", sep = '')
	cpt.date <- CPT.arrangeDate(daty)
	lon <- gridinfo$x
	lat <- gridinfo$y
	cpt.data <- lapply(seq_along(daty), function(j){
		cpt.tags <- list(field = varid, T = daty[j], nrow = length(lat), ncol = length(lon),
							row = 'Y', col = 'X', units = units, missing = missval)
		cpt.tags <- CPT.getTAG.line(cpt.tags)
		cpt.data <- CPT.formatGridData(lon, lat, data[[j]], width = ncmax, side = "both")
		c(cpt.tags, cpt.data)
	})
	cpt.data <- do.call(c, cpt.data)
	cpt.out <- c(xmlns, nfields, cpt.date, cpt.data)
	return(cpt.out)
}

#################################################################################
## CPT format

cdtData2CPT <- function(timeStep, cdtFile, csv = FALSE, tsvFile,
						missing.code, field, units){
	if(csv) sep = ',' else sep = ''
	stnData <- read.table(cdtFile, stringsAsFactors = FALSE, na.strings = str_trim(missing.code), sep = sep)
	stnData <- getCDTdataAndDisplayMsg(stnData, timeStep)
	stnData$data[is.na(stnData$data)] <- missing.code
	headinfo <- cbind(stnData$id, stnData$lon, stnData$lat)
	cptIn <- list(data = stnData$data, date = stnData$dates, stninfo = headinfo,
					varid = field, units = units, missval = missing.code)
	cptOut <- do.call(CPT.convertStationData, cptIn)
	cat(cptOut, file = tsvFile)
	return(0)
}

netcdf2CPT <- function(timeStep, start.year, start.mon, start.day = 1,
						end.year, end.mon, end.day = 1,
						ncdfDIR, ncdfFORMAT, tsvFile, missing.code){
	start.date <- as.Date(paste(start.year, start.mon, start.day, sep = '-'))
	end.date <- as.Date(paste(end.year, end.mon, end.day, sep = '-'))
	ncInfo <- ncFilesInfo(timeStep, start.date, end.date, 1:12, ncdfDIR, ncdfFORMAT, "NetCDF data not found")
	if(is.null(ncInfo)) return(NULL)

	nc <- nc_open(ncInfo$nc.files[ncInfo$exist][1])
	xlon <- nc$dim[[1]]$vals
	xlat <- nc$dim[[2]]$vals
	varid <- nc$var[[1]]$name
	units <- nc$var[[1]]$units
	nc_close(nc)

	ncdata <- lapply(seq_along(ncInfo$nc.files), function(j){
		if(ncInfo$exist[j]){
			nc <- nc_open(ncInfo$nc.files[j])
			xdat <- ncvar_get(nc, varid = varid)
			nc_close(nc)
			xdat[is.na(xdat)] <- missing.code
			xdat
		}else return(NULL)
	})
	ncdata[sapply(ncdata, is.null)] <- matrix(missing.code, nrow = length(xlon), ncol = length(xlat))

	cptIn <- list(data = ncdata, date = ncInfo$dates, gridinfo = list(x = xlon, y = xlat),
					varid = varid, units = units, missval = missing.code)
	cptOut <- do.call(getDataCPT.GRDdata, cptIn)
	cat(cptOut, file = tsvFile)
	return(0)
}

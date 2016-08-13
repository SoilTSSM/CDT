
splitCDTData <- function(donne, period){
	# ideb <- grep('[[:digit:]]',donne[1:5,1])[1]
	ideb <- nrow(donne)
	if(period == 'daily'){
		if(nchar(as.character(donne[ideb, 1])) != 8){
			InsertMessagesTxt(main.txt.out, 'Station data: not a daily data', format = TRUE)
			return(NULL)
		}
	}else if(period == 'dekadal'){
		if(nchar(as.character(donne[ideb, 1])) != 7){
			InsertMessagesTxt(main.txt.out, 'Station data: not a dekadal data', format = TRUE)
			return(NULL)
		}
	}else if(period == 'monthly'){
		if(nchar(as.character(donne[ideb, 1])) != 6){
			InsertMessagesTxt(main.txt.out, 'Station data: not a monthly data', format = TRUE)
			return(NULL)
		}
	}
	stn.id <- as.character(as.matrix(donne[1, -1]))
	stn.lon <- as.numeric(as.matrix(donne[2, -1]))
	stn.lat <- as.numeric(as.matrix(donne[3, -1]))
	if(length(grep('alt|elev|elv', as.character(donne[4, 1]), ignore.case = TRUE)) == 1){
		Info <- data.frame(t(donne[1:4, -1]))
		names(Info) <- c('Stations', 'Lon', 'Lat', 'ELV')
		if(period == 'daily'){
			dates.bak <- as.character(donne[-c(1:4), 1])
			dates <- as.Date(dates.bak, format='%Y%m%d')
		}else if(period == 'dekadal'){
			xan <- substr(as.character(donne[-c(1:4), 1]), 1, 4)
			xmo <- substr(as.character(donne[-c(1:4), 1]), 5, 6)
			xdk <- substr(as.character(donne[-c(1:4), 1]), 7, 7)
			notdek <- which(as.numeric(xdk) > 3)
			dates.bak <- paste(xan, xmo, xdk, sep = '-')
			dates <- as.Date(dates.bak)
			dates[notdek] <- NA
		}else if(period == 'monthly'){
			xan <- substr(as.character(donne[-c(1:4), 1]), 1, 4)
			xmo <- substr(as.character(donne[-c(1:4), 1]), 5, 6)
			dates.bak <- paste(xan, xmo, '1', sep = '-')
			dates <- as.Date(dates.bak)
		}
		stn.elv <- as.numeric(donne[4, -1])
		donne <- as.matrix(donne[-c(1:4), -1])
	}else{
		Info <- data.frame(t(donne[1:3, -1]))
		names(Info) <- c('Stations', 'Lon', 'Lat')
		if(period == 'daily'){
			dates.bak <- as.character(donne[-c(1:3), 1])
			dates <- as.Date(dates.bak, format='%Y%m%d')
		}else if(period == 'dekadal'){
			xan <- substr(as.character(donne[-c(1:3), 1]), 1, 4)
			xmo <- substr(as.character(donne[-c(1:3), 1]), 5, 6)
			xdk <- substr(as.character(donne[-c(1:3), 1]), 7, 7)
			notdek <- which(as.numeric(xdk) > 3)
			dates.bak <- paste(xan, xmo, xdk, sep = '-')
			dates <- as.Date(dates.bak)
			dates[notdek] <- NA
		}else if(period == 'monthly'){
			xan <- substr(as.character(donne[-c(1:3), 1]), 1, 4)
			xmo <- substr(as.character(donne[-c(1:3), 1]), 5, 6)
			dates.bak <- paste(xan, xmo, '1', sep = '-')
			dates <- as.Date(dates.bak)
		}
		stn.elv <- NULL
		donne <- as.matrix(donne[-c(1:3), -1])
	}

	dimnames(donne) <- NULL
	dimdonne <- dim(donne)
	donne <- apply(donne, 2, as.numeric)
	dim(donne) <- dimdonne

	iNAdate <- is.na(dates)
	wrong.dates <- dates.bak[iNAdate]
	dwrong.dates <- donne[iNAdate, , drop = FALSE]
	donne <- donne[!iNAdate, , drop = FALSE]
	dates <- dates[!iNAdate]

	###duplicated dates
	idates0 <- duplicated(dates) | duplicated(dates, fromLast = TRUE)
	dup.dates <- dates[idates0]
	ddup.dates <- donne[idates0, , drop = FALSE]
	donne <- donne[!duplicated(dates), , drop = FALSE]
	dates <- dates[!duplicated(dates)]

	##fill missing dates
	if(period == 'daily'){
		alldates <- seq(min(dates), max(dates), 'day')
		ix <- match(alldates,dates)
		dates <- format(alldates, '%Y%m%d')
		miss.dates <- alldates[is.na(ix)]
		miss.dates <- format(miss.dates, '%Y%m%d')
		dup.dates <- format(dup.dates, '%Y%m%d')
	}else if(period == 'dekadal'){
		alldates <- seq(min(dates), max(dates), 'day')
		alldates <- alldates[as.numeric(format(alldates, '%d')) <= 3]
		ix <- match(alldates,dates)
		dates <- paste(format(alldates, '%Y%m'), as.numeric(format(alldates, '%d')), sep = '')
		miss.dates <- alldates[is.na(ix)]
		miss.dates <- paste(format(miss.dates, '%Y%m'), as.numeric(format(miss.dates, '%d')), sep = '')
		dup.dates <- paste(format(dup.dates, '%Y%m'), as.numeric(format(dup.dates, '%d')), sep = '')
	}else if(period == 'monthly'){
		alldates <- seq(min(dates), max(dates), 'month')
		ix <- match(alldates,dates)
		dates <- format(alldates, '%Y%m')
		miss.dates <- alldates[is.na(ix)]
		miss.dates <- format(miss.dates, '%Y%m')
		dup.dates <- format(dup.dates, '%Y%m')
	}
	donne <- donne[ix, , drop = FALSE]

	wrong.dates <- if(any(iNAdate)) list(date = wrong.dates, data = dwrong.dates) else NULL
	duplicated.dates <-  if(any(idates0)) list(date = dup.dates, data = ddup.dates) else NULL
	missing.dates <-  if(any(is.na(ix))) list(date = miss.dates) else NULL

	##missing coordinates
	imiss<-(is.na(stn.lon) | is.na(stn.lat) | stn.lat < -90 | stn.lat > 90)
	stn.lon <- stn.lon[!imiss]
	stn.lat <- stn.lat[!imiss]
	stn.id <- stn.id[!imiss]
	stn.elv <- stn.elv[!imiss]
	donne <- donne[,!imiss, drop = FALSE]

	##duplicates coordinates
	idup <- duplicated(cbind(stn.lon, stn.lat))
	idup1 <- duplicated(cbind(stn.lon, stn.lat), fromLast = T)
	# omit doublon
	stn.lon <- stn.lon[!idup]
	stn.lat <- stn.lat[!idup]
	stn.id <- stn.id[!idup]
	stn.elv <- stn.elv[!idup]
	donne <- donne[,!idup, drop = FALSE]

	stn.miss <- if(any(imiss)) Info[imiss,] else NULL
	stn.dup <- if(any(idup1 | idup)) rbind(Info[idup1, ], Info[idup, ]) else NULL

	stnlist <- list(id = stn.id, lon = stn.lon, lat = stn.lat, elv = stn.elv, dates = dates, data = donne,
					wrong.dates = wrong.dates, duplicated.dates = duplicated.dates, missing.dates = missing.dates,
					duplicated.coords = stn.dup, missing.coords = stn.miss)
	return(stnlist)
}


##########################################


splitTsData <- function(donne, period, filefrmt, datefrmt){
	#get dates
	if(period == 'daily'){
		if(datefrmt == "1"){
			if(nchar(as.character(donne[5, 1])) != 8){
				InsertMessagesTxt(main.txt.out, 'Station data: not a daily data', format = TRUE)
				return(NULL)
			}
			dates.bak <- as.character(donne[, 1])
			dates <- as.Date(dates.bak, format = '%Y%m%d')
		}else{
			dates.bak <- paste(as.character(donne[, 1]), as.character(donne[, 2]), as.character(donne[, 3]), sep = '-')
			dates <- as.Date(dates.bak)
		}
	}else if(period == 'dekadal'){
		if(datefrmt == "1"){ #1date
			if(nchar(as.character(donne[5, 1])) != 7){
				InsertMessagesTxt(main.txt.out, 'Station data: not a dekadal data', format = TRUE)
				return(NULL)
			}
			xan <- substr(as.character(donne[, 1]), 1, 4)
			xmo <- substr(as.character(donne[, 1]), 5, 6)
			xdk <- substr(as.character(donne[, 1]), 7, 7)
			notdek <- which(as.numeric(xdk) > 3)
			dates.bak <- paste(xan, xmo, xdk, sep = '-')
			dates <- as.Date(dates.bak)
		}else{ #3date
			dates.bak <- paste(as.character(donne[, 1]), as.character(donne[, 2]), as.character(donne[, 3]), sep = '-')
			dates <- as.Date(dates.bak)
			notdek <- which(as.numeric(as.character(donne[, 3])) > 3)
		}
		dates[notdek] <- NA
	}else if(period == 'monthly'){
		if(datefrmt == "1"){ #1date
			if(nchar(as.character(donne[5, 1])) != 6){
				InsertMessagesTxt(main.txt.out, 'Station data: not a monthly data', format = TRUE)
				return(NULL)
			}
			xan <- substr(as.character(donne[, 1]), 1, 4)
			xmo <- substr(as.character(donne[, 1]), 5, 6)
			dates.bak <- paste(xan, xmo, '1', sep = '-')
			dates <- as.Date(dates.bak)
		}else{ #3date
			dates.bak <- paste(as.character(donne[, 1]), as.character(donne[, 2]), '1', sep = '-')
			dates <- as.Date(dates.bak)
		}
	}

	#get vars
	if(filefrmt == "1"){ #1var
		if(datefrmt == "1"){
			var <- as.numeric(donne[, 2])
		}else{
			if(period == 'monthly'){
				if(ncol(donne) == 3) var <- as.numeric(donne[, 3])
				if(ncol(donne) == 4) var <- as.numeric(donne[, 4])
			}else{
				var <- as.numeric(donne[, 4])
			}
		}
	}else{#3var
		if(datefrmt == "1"){
			rr <- as.numeric(donne[, 2])
			tx <- as.numeric(donne[, 3])
			tn <- as.numeric(donne[, 4])
		}else{
			if(period == 'monthly'){
				if(ncol(donne) == 5){
					rr <- as.numeric(donne[, 3]) #rr = 3, tx = 4, tn = 5
					tx <- as.numeric(donne[, 4])
					tn <- as.numeric(donne[, 5])
				}
				if(ncol(donne) == 6){
					rr <- as.numeric(donne[, 4]) #rr = 4, tx = 5, tn = 6
					tx <- as.numeric(donne[, 5])
					tn <- as.numeric(donne[, 6])
				}
			}else{
				rr <- as.numeric(donne[, 4]) #rr = 4, tx = 5, tn = 6
				tx <- as.numeric(donne[, 5])
				tn <- as.numeric(donne[, 6])
			}
		}
	}

	iNAdate <- is.na(dates)
	wrong.dates <- dates.bak[iNAdate]

	##remove NA dates and order
	if(filefrmt == "1"){
		dwrong.dates <- var[iNAdate]
		var <- var[!iNAdate]
		dates <- dates[!iNAdate]
	}else{
		dwrong.dates <- cbind(rr[iNAdate], tx[iNAdate], tn[iNAdate])
		rr <- rr[!iNAdate]
		tx <- tx[!iNAdate]
		tn <- tn[!iNAdate]
		dates <- dates[!iNAdate]
	}
	
 	##duplicated dates
 	idates0 <- duplicated(dates) | duplicated(dates, fromLast = TRUE)
	dup.dates <- dates[idates0]

 	idates <- duplicated(dates)
	dates <- dates[!idates]
	if(filefrmt == "1"){
		ddup.dates <- var[idates0]
		var <- var[!idates]
	}else{
		ddup.dates <- cbind(rr[idates0], tx[idates0], tn[idates0])
		rr <- rr[!idates]
		tx <- tx[!idates]
		tn <- tn[!idates]
	}

	if(length(dates) == 0){
		InsertMessagesTxt(main.txt.out, 'Wrong date format', format = TRUE)
		return(NULL)
	}

	if(period == 'daily'){
		alldates <- seq(min(dates), max(dates), 'day')
		ix <- match(alldates,dates)
		dates <- format(alldates, '%Y%m%d')
		miss.dates <- alldates[is.na(ix)]
		miss.dates <- format(miss.dates, '%Y%m%d')
		dup.dates <- format(dup.dates, '%Y%m%d')
	}
	if(period == 'dekadal'){
		alldates <- seq(min(dates), max(dates), 'day')
		alldates <- alldates[as.numeric(format(alldates, '%d')) <= 3]
		ix <- match(alldates,dates)
		dates <- paste(format(alldates, '%Y%m'), as.numeric(format(alldates, '%d')), sep = '')
		miss.dates <- alldates[is.na(ix)]
		miss.dates <- paste(format(miss.dates, '%Y%m'), as.numeric(format(miss.dates, '%d')), sep = '')
		dup.dates <- paste(format(dup.dates, '%Y%m'), as.numeric(format(dup.dates, '%d')), sep = '')
	}
	if(period == 'monthly'){
		alldates <- seq(min(dates), max(dates), 'month')
		ix <- match(alldates,dates)
		dates <- format(alldates, '%Y%m')
		miss.dates <- alldates[is.na(ix)]
		miss.dates <- format(miss.dates, '%Y%m')
		dup.dates <- format(dup.dates, '%Y%m')
	}

	if(filefrmt == "1"){
		var <- list(var = var[ix])
		nbvar <- 1
	}else{
		var <- list(rr = rr[ix], tx = tx[ix], tn = tn[ix])
		nbvar <- 3
	}

	wrong.dates <- if(any(iNAdate)) list(date = wrong.dates, data = dwrong.dates) else NULL
	duplicated.dates <-  if(any(idates0)) list(date = dup.dates, data = ddup.dates) else NULL
	missing.dates <-  if(any(is.na(ix))) list(date = miss.dates) else NULL

	ret <- list(period = period, nbvar = nbvar, var = var, dates = dates,
				wrong.dates = wrong.dates, duplicated.dates = duplicated.dates, missing.dates = missing.dates)
	return(ret)
}



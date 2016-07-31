
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
	DEK.Dates <- NULL
	if(length(grep('alt|elev|elv', as.character(donne[4,1]), ignore.case = TRUE)) == 1){
		Info <- data.frame(t(donne[1:4,-1]))
		names(Info) <- c('Stations', 'Lon', 'Lat', 'ELV')
		if(period == 'daily'){
			dates <- as.Date(donne[-c(1:4), 1], format='%Y%m%d')
		}else if(period == 'dekadal'){
			xan <- substr(as.character(donne[-c(1:4), 1]), 1,4)
			xmo <- substr(as.character(donne[-c(1:4), 1]), 5,6)
			xdk <- substr(as.character(donne[-c(1:4), 1]), 7,7)
			notdek <- which(as.numeric(xdk) > 3)
			dates <- as.Date(paste(xan, xmo, xdk, sep = '-'))
			DEK.Dates <- gsub('-', '', as.character(dates[notdek]))
			DEK.Dates <- paste(substr(DEK.Dates, 1, 6), substr(DEK.Dates, 8, 8), sep = '')
			dates[notdek] <- NA
		}else if(period == 'monthly'){
			xan <- substr(as.character(donne[-c(1:4), 1]), 1,4)
			xmo <- substr(as.character(donne[-c(1:4), 1]), 5,6)
			dates <- as.Date(paste(xan, xmo, '1', sep = '-'))
		}
		stn.elv <- as.numeric(donne[4,-1])
		donne <- as.matrix(donne[-c(1:4),-1])
	}else{
		Info <- data.frame(t(donne[1:3,-1]))
		names(Info) <- c('Stations', 'Lon', 'Lat')
		if(period == 'daily'){
			dates <- as.Date(donne[-c(1:3), 1], format='%Y%m%d')
		}else if(period == 'dekadal'){
			xan <- substr(as.character(donne[-c(1:3), 1]), 1,4)
			xmo <- substr(as.character(donne[-c(1:3), 1]), 5,6)
			xdk <- substr(as.character(donne[-c(1:3), 1]), 7,7)
			notdek <- which(as.numeric(xdk) > 3)
			dates <- as.Date(paste(xan, xmo, xdk, sep = '-'))
			DEK.Dates <- gsub('-', '', as.character(dates[notdek]))
			DEK.Dates <- paste(substr(DEK.Dates, 1, 6), substr(DEK.Dates, 8, 8), sep = '')
			dates[notdek] <- NA
		}else if(period == 'monthly'){
			xan <- substr(as.character(donne[-c(1:3), 1]), 1,4)
			xmo <- substr(as.character(donne[-c(1:3), 1]), 5,6)
			dates <- as.Date(paste(xan, xmo, '1', sep = '-'))
		}
		stn.elv <- NULL
		donne <- as.matrix(donne[-c(1:3),-1])
	}
	dimnames(donne) <- NULL
	dimdonne <- dim(donne)
	donne <- apply(donne, 2, as.numeric)
	dim(donne) <- dimdonne
	donne <- donne[!is.na(dates),,drop = FALSE]
	dates <- dates[!is.na(dates)]
	donne <- donne[order(dates),,drop = FALSE]
	dates <- dates[order(dates)]

	###duplicated dates
	#idates <- duplicated(dates) | duplicated(dates, fromLast = TRUE)
	idates <- duplicated(dates)
	dup.dates <- dates[idates]
	donne <- donne[!idates,,drop = FALSE]
	dates <- dates[!idates]

	##fill missing dates
	if(period == 'daily'){
		alldates <- seq(min(dates), max(dates), 'day')
		ix <- match(alldates,dates)
		dates <- format(alldates, '%Y%m%d')
		dup.dates <- format(dup.dates, '%Y%m%d')
	}else if(period == 'dekadal'){
		alldates <- seq(min(dates), max(dates), 'day')
		alldates <- alldates[as.numeric(format(alldates, '%d')) <= 3]
		ix <- match(alldates,dates)
		dates <- paste(format(alldates, '%Y%m'), as.numeric(format(alldates, '%d')), sep = '')
		dup.dates <- paste(format(dup.dates, '%Y%m'), as.numeric(format(dup.dates, '%d')), sep = '')
	}else if(period == 'monthly'){
		alldates <- seq(min(dates), max(dates), 'month')
		ix <- match(alldates,dates)
		dates <- format(alldates, '%Y%m')
		dup.dates <- format(dup.dates, '%Y%m')
	}
	donne <- donne[ix,, drop = FALSE]

	##missing coordinates
	imiss<-(is.na(stn.lon) | is.na(stn.lat) | stn.lat< -90 | stn.lat > 90)
	stn.miss <- Info[imiss,]
	stn.lon <- stn.lon[!imiss]
	stn.lat <- stn.lat[!imiss]
	stn.id <- stn.id[!imiss]
	stn.elv <- stn.elv[!imiss]
	donne <- donne[,!imiss, drop = FALSE]

	##duplicates coordinates
	idup <- duplicated(cbind(stn.lon, stn.lat))
	idup1 <- duplicated(cbind(stn.lon, stn.lat), fromLast = T) #omit doublon
	stn.dup <- rbind(Info[idup1,],Info[idup,])
	stn.dup <- stn.dup[!duplicated(stn.dup[,1]),]
	stn.lon <- stn.lon[!idup]
	stn.lat <- stn.lat[!idup]
	stn.id <- stn.id[!idup]
	stn.elv <- stn.elv[!idup]
	donne <- donne[,!idup, drop = FALSE]
	stnlist <- list(id = stn.id, lon = stn.lon, lat = stn.lat, elv = stn.elv, dates = dates, data = donne,
					duplicated.coords = stn.dup, missing.coords = stn.miss, duplicated.dates = unique(dup.dates), not.dekad.dates = DEK.Dates)
	return(stnlist)
}


##########################################

##check numeric value
#CheckNumeric <- function(x) suppressWarnings(!is.na(as.numeric(x) & !is.na(x)))


#	test <- apply(donne, 2, function(x) CheckNumeric(x))
#	if(sum(!test) > 0){
#		InsertMessagesTxt(main.txt.out, 'There is Non-Numeric value in data', format = TRUE)
#		return(NULL)
#	}


splitTsData <- function(donne, period, filefrmt, datefrmt){
	#get dates
	DEK.Dates <- NULL
	if(period == 'daily'){
		if(datefrmt == "1"){
			if(nchar(as.character(donne[5, 1])) != 8){
				InsertMessagesTxt(main.txt.out, 'Station data: not a daily data', format = TRUE)
				return(NULL)
			}
			dates <- as.Date(as.character(donne[, 1]), format='%Y%m%d')
		}else{
			dates <- as.Date(paste(as.character(donne[, 1]), as.character(donne[, 2]), as.character(donne[, 3]), sep = '-'))
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
			dates <- as.Date(paste(xan, xmo, xdk, sep = '-'))
		}else{ #3date
			dates <- as.Date(paste(as.character(donne[, 1]), as.character(donne[, 2]), as.character(donne[, 3]), sep = '-'))
			notdek <- which(as.numeric(as.character(donne[, 3]))>3)
		}
		DEK.Dates <- gsub('-', '', as.character(dates[notdek]))
		DEK.Dates <- paste(substr(DEK.Dates, 1, 6), substr(DEK.Dates, 8, 8), sep = '')
		dates[notdek] <- NA
	}else if(period == 'monthly'){
		if(datefrmt == "1"){ #1date
			if(nchar(as.character(donne[5, 1])) != 6){
				InsertMessagesTxt(main.txt.out, 'Station data: not a monthly data', format = TRUE)
				return(NULL)
			}
			xan <- substr(as.character(donne[, 1]), 1, 4)
			xmo <- substr(as.character(donne[, 1]), 5, 6)
			dates <- as.Date(paste(xan, xmo, '1', sep = '-'))
		}else{ #3date
			dates <- as.Date(paste(as.character(donne[, 1]), as.character(donne[, 2]), '1', sep = '-'))
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

	##remove NA dates and order
	if(filefrmt == "1"){
		var <- var[!is.na(dates)]
		dates <- dates[!is.na(dates)]
		var <- var[order(dates)]
	}else{
		rr <- rr[!is.na(dates)]
		tx <- tx[!is.na(dates)]
		tn <- tn[!is.na(dates)]
		dates <- dates[!is.na(dates)]
		rr <- rr[order(dates)]
		tx <- tx[order(dates)]
		tn <- tn[order(dates)]
	}
	
	## order date
	dates <- dates[order(dates)]

 	##duplicated dates
 	idates <- duplicated(dates)
	dup.dates <- dates[idates]
	dates <- dates[!idates]
	if(filefrmt == "1"){
		var <- var[!idates]
	}else{
		rr <- rr[!idates]
		tx <- tx[!idates]
		tn <- tn[!idates]
	}

	if(period == 'daily'){
		alldates <- seq(min(dates), max(dates), 'day')
		ix <- match(alldates,dates)
		dates <- format(alldates, '%Y%m%d')
		dup.dates <- format(dup.dates, '%Y%m%d')
	}
	if(period == 'dekadal'){
		alldates <- seq(min(dates), max(dates), 'day')
		alldates <- alldates[as.numeric(format(alldates, '%d')) <= 3]
		ix <- match(alldates,dates)
		dates <- paste(format(alldates, '%Y%m'), as.numeric(format(alldates, '%d')), sep = '')
		dup.dates <- paste(format(dup.dates, '%Y%m'), as.numeric(format(dup.dates, '%d')), sep = '')
	}
	if(period == 'monthly'){
		alldates <- seq(min(dates), max(dates), 'month')
		ix <- match(alldates,dates)
		dates <- format(alldates, '%Y%m')
		dup.dates <- format(dup.dates, '%Y%m')
	}

	if(filefrmt == "1"){
		var <- list(var = var[ix])
		nbvar <- 1
	}else{
		var <- list(rr = rr[ix], tx = tx[ix], tn = tn[ix])
		nbvar <- 3
	}

	if(length(dates) == 0){
		InsertMessagesTxt(main.txt.out, 'Wrong date format', format = TRUE)
		return(NULL)
	}
	ret <- list(period = period, nbvar = nbvar, var = var, dates = dates, duplicated.dates = dup.dates, not.dekad.dates = DEK.Dates)
	return(ret)
}





Extraterrestrial.Radiation <- function(lat, tstep = "daily"){
	phi <- pi * (lat)/180
	dates <- seq(as.Date("2001-1-1"), as.Date("2001-12-31"), 'day')

	J <- as.numeric(strftime(dates, format = "%j"))
	fJ <- 2*pi*J/365
	dr <- 1 + 0.033 * cos(fJ)
	delta <- 0.409 * sin(fJ-1.39)

	ws <- matrix(NA, nrow = 365, ncol = length(lat))
	sin2 <- cos2 <- ws
	for(j in seq_along(lat)){
		ws[, j] <- acos(-tan(phi[j]) * tan(delta))
		sin2[, j] <- sin(phi[j]) * sin(delta)
		cos2[, j] <- cos(phi[j]) * cos(delta)
	} 
	Ra <- (37.58603 * dr) * (ws * sin2 + cos2 * sin(ws))
	Ra[Ra < 0] <- 0
	if(tstep == "pentad"){
		irow <- as.numeric(format(dates, "%d"))%in%c(3, 8, 13, 18, 23, 28)
		Ra <- Ra[irow, , drop = FALSE]
	}
	if(tstep == "dekadal"){
		irow <- as.numeric(format(dates, "%d"))%in%c(5, 15, 25)
		Ra <- Ra[irow, , drop = FALSE]
	}
	if(tstep == "monthly"){
		irow <- as.numeric(format(dates, "%d")) == 15
		Ra <- Ra[irow, , drop = FALSE]
	}
	# Ra in MJ m-2 d-1
	return(Ra)
}

#################################################################################
## Hargreaves evapotranspiration

Ref.ET.Hargreaves <- function(Tmax, Tmin, Ra, tstep = "daily", Precip = NULL){
	TM <- (Tmax + Tmin)/2
	TR <- Tmax - Tmin
	TR[TR < 0] <- 0
	if(is.null(Precip)){
		# Original Hargreaves equation
		coefs <- if(tstep == "daily") c(0.0028, 19.1869) else c(0.0023, 17.8)
		ETP <- coefs[1] * (0.408 * Ra) * (TM + coefs[2]) * TR^0.5
	}else{
		# Hargreaves modified method
		coefs <- if(tstep == "daily") c(0.0019, 21.0584, 0.0874, 0.6278) else c(0.0013, 17, 0.0123, 0.76)
		PRE <- (TR - coefs[3] * Precip)^coefs[4]
		PRE[is.nan(PRE)] <- 0
		ETP <- coefs[1] * (0.408 * Ra) * (TM + coefs[2]) * PRE
	}
	# mm/day
	return(ETP)
}

#################################################################################

get.Index.DailyYears <- function(dates, start.month, start.day){
	start.daty <- paste0(str_pad(start.month, 2, pad = '0'), str_pad(start.day, 2, pad = '0'))

	dtmp <- range(as.Date(dates, '%Y%m%d'), na.rm = TRUE)
	dmonday <- format(seq(dtmp[1], dtmp[2], "day"), "%m%d")
	nmod <- length(dmonday)

	rleday1 <- rle(dmonday == start.daty)
	cumidx <- cumsum(rleday1$length)
	debidx <- cumidx[rleday1$values]
	if(debidx[1] != 1) debidx <- c(1, debidx)
	if(debidx[length(debidx)] != nmod) debidx <- c(debidx, nmod+1)
	nbsplit <- diff(debidx)

	if(!nbsplit[1]%in%c(365, 366)){
		deb <- paste0(format(dtmp[1]-(366-nbsplit[1]), "%Y"), start.daty)
		deb <- as.Date(deb, '%Y%m%d')
	}else deb <- dtmp[1]

	if(!nbsplit[length(nbsplit)]%in%c(365, 366)){
		fin <- format(dtmp[2]+(366-nbsplit[length(nbsplit)]), "%Y")
		fin <- as.Date(paste0(fin, start.daty), '%Y%m%d') - 1
	}else fin <- dtmp[2]

	cdates <- format(seq(deb, fin, "day"), "%Y%m%d")
	idrow <- seq(length(dates))
	idrow <- idrow[match(cdates, dates)]

	ix <- c(which(substr(cdates, 5, 8) == start.daty), length(cdates)+1)
	idx <- rep(seq(length(ix)-1), ix[-1]-ix[-length(ix)])
	index <- split(idrow, idx)
	dates <- do.call(rbind, lapply(split(cdates, idx), function(x) c(x[1], x[length(x)])))
	return(list(index = index, range.date = dates))
}

# get.Index.DailyYears <- function(dates, start.month, start.day, n = 0){
# 	start.lag <- as.Date(paste(2014, start.month, start.day, sep = '-')) - n
# 	start.month <- as.numeric(format(start.lag, "%m"))
# 	start.day <- as.numeric(format(start.lag, "%d"))

# 	dtmp <- range(as.Date(dates, '%Y%m%d'), na.rm = TRUE)
# 	cdates <- seq(dtmp[1], dtmp[2], "day")
# 	ystart <- seq(as.Date(paste0(format(cdates[1], '%Y'), '-1-1')), cdates[1], "day")
# 	ystart <- ystart[-length(ystart)]
# 	yend <- seq(cdates[length(cdates)], as.Date(paste0(format(cdates[length(cdates)], '%Y'), '-12-31')), "day")
# 	yend <- yend[-1]
# 	if(length(ystart) > 0) cdates <- c(ystart, cdates)
# 	if(length(yend) > 0) cdates <- c(cdates, yend)
# 	cdates <- format(cdates, "%Y%m%d")

# 	idrow <- seq(length(dates))
# 	idrow <- idrow[match(cdates, dates)]

# 	start.daty <- paste0(str_pad(start.month, 2, pad = '0'), str_pad(start.day, 2, pad = '0'))
# 	debstart <- substr(cdates, 5, 8)

# 	rleday1 <- rle(debstart == start.daty)
# 	cumidx <- cumsum(rleday1$length)
# 	debidx <- cumidx[rleday1$values]

# 	firstSplit <- debidx[1] != 1
# 	if(firstSplit) debidx <- c(1, debidx)
# 	if(debidx[length(debidx)] != length(cdates)) debidx <- c(debidx, length(cdates)+1)
# 	idx <- rep(seq(length(debidx)-1), debidx[-1]-debidx[-length(debidx)])
# 	index <- split(idrow, idx)
# 	if(firstSplit) index <- index[-1]
# 	return(index)
# }

#################################################################################

Water.Balance <- function(rain, etp, capacity.max = 100, wb1 = 0){
	# rain, etp: matrix; row: dates, col: stations/points
	# capacity.max: Soil Water Holding Capacity, vector same length as col rain 
	# or one value if capacity.max are equal for all stations 
	# wb1: water balance at day 1, vector same length as col rain 
	# or one value if wb1 are equal for all stations 
	## NA??0
	rain[is.na(rain)] <- 0
	etp[is.na(etp)] <- 0

	ndays <- nrow(rain)
	nbstn <- ncol(rain)

	if(length(wb1) == 1){
		wb1 <- rep(wb1, nbstn)
	}else{
		if(length(wb1) != nbstn){
			cat("The length of wb1 and the number of stations must be equal\n")
			return(NULL)
		}
	}

	minwb <- rep(0, nbstn)
	if(length(capacity.max) == 1){
		maxwb <- rep(capacity.max, nbstn)
	}else{
		if(length(capacity.max) != nbstn){
			cat("The length of capacity.max and the number of stations must be equal\n")
			return(NULL)
		}
		maxwb <- capacity.max
	}

	water.balance <- matrix(NA, nrow = ndays, ncol = nbstn)
	water.balance[1, ] <- wb1

	# simple water balance
	for(iday in 2:ndays){
		water.balance[iday, ] <- water.balance[iday-1, ] + rain[iday, ] - etp[iday, ]
		water.balance[iday, ] <- pmax(minwb, pmin(maxwb, water.balance[iday, ]))
	}

	return(water.balance)
}

#################################################################################

Season.Onset <- function(dates, precip, evapo = NULL, method, onset.pars, min.frac)
{
	total.days <- onset.pars$total.days
	rain.total <- onset.pars$rain.total
	min.rain.day <- onset.pars$min.rain.day
	dryspell.days <- onset.pars$dryspell.days
	dryspell <- onset.pars$dryspell
	thres.rain.day <- onset.pars$thres.rain.day
	etp.frac <- onset.pars$evapo.frac
	initCol <- ncol(precip)

	yearO <- if(onset.pars$latest$month <= onset.pars$earliest$month) 2015 else 2014
	search.days <- as.numeric(as.Date(paste(yearO, onset.pars$latest$month, onset.pars$latest$day, sep = '-'))
					- as.Date(paste(2014, onset.pars$earliest$month, onset.pars$earliest$day, sep = '-'))) + 1

	wsearch <- 1:search.days
	if(length(wsearch) > nrow(precip)) wsearch <- wsearch[seq(nrow(precip))]
	MATdeb <- precip[wsearch, , drop = FALSE]
	dates <- dates[wsearch]
	if(method == 2) ETPdeb <- evapo[wsearch, , drop = FALSE]

	## remove NA
	colID <- colSums(is.na(MATdeb))/nrow(MATdeb) > (1-min.frac)
	if(method == 2){
		etpID <- colSums(is.na(ETPdeb))/nrow(ETPdeb) > (1-min.frac)
		colID <- colID | etpID
	}
	colretenu <- seq(initCol)[!colID]
	MATdeb <- MATdeb[, !colID, drop = FALSE]
	if(method == 2) ETPdeb <- ETPdeb[, !colID, drop = FALSE]
	if(ncol(MATdeb) == 0) return(rep(NA, initCol))

	MATdeb[is.na(MATdeb)] <- 0
	if(method == 2) ETPdeb[is.na(ETPdeb)] <- 0

	MATdeb.truncated <- rbind(matrix(0, nrow = total.days, ncol = ncol(MATdeb)), head(MATdeb, -total.days))
	if(method == 2) ETPdeb.truncated <- rbind(matrix(0, nrow = total.days, ncol = ncol(ETPdeb)), head(ETPdeb, -total.days))

	MATtotal <- (matrixStats::colCumsums(MATdeb) - matrixStats::colCumsums(MATdeb.truncated))
	if(method == 2) ETPtotal <- (matrixStats::colCumsums(ETPdeb) - matrixStats::colCumsums(ETPdeb.truncated))

	istotal <- if(method == 2) MATtotal >= etp.frac * ETPtotal else MATtotal >= rain.total

	## remove no onset
	colID <- colSums(istotal) == 0
	colretenu <- colretenu[!colID]
	istotal <- istotal[, !colID, drop = FALSE]
	MATdeb <- MATdeb[, !colID, drop = FALSE]
	if(ncol(istotal) == 0) return(rep(NA, initCol))

	## remove 1st day onset
	colID <- colSums(istotal) == nrow(istotal)
	colDEB <- colretenu[colID]
	colretenu <- colretenu[!colID]
	istotal <- istotal[, !colID, drop = FALSE]
	MATdeb <- MATdeb[, !colID, drop = FALSE]
	if(ncol(istotal) == 0){
		res <- rep(NA, initCol)
		if(length(colDEB) > 0) res[colDEB] <- 1
		return(dates[res])
	}

	onset <- lapply(seq(ncol(istotal)), function(j){
		y <- istotal[, j]
		ipos <- which(y)

		if(method %in% 1:2){
			istart <- ipos[1]
		}else{
			x <- MATdeb[, j] >= thres.rain.day

			if(method %in% 4:5){
				is.onset <- sapply(ipos, function(i){
					x1 <- !x[i+(1:dryspell.days)]
					x1 <- x1[!is.na(x1)]
					x2 <- rle(x1)
					!any(x2$lengths[x2$values] >= dryspell)
				})

				if(!any(is.onset)) return(NA)
				ipos <- ipos[is.onset]
			}

			if(method == 4) istart <- ipos[1]

			if(method %in% c(3, 5)){
				istart <- NA
				for(i in ipos){
					io <- i-(total.days:1)+1
					io <- io[io > 0]
					if(sum(x[io]) >= min.rain.day){
						istart <- i
						break
					}
				}
			}
		}

		istart
	})
	onset <- do.call(c, onset)

	res <- rep(NA, initCol)
	if(length(colDEB) > 0) res[colDEB] <- 1
	res[colretenu] <- onset
	if(all(is.na(res))) res else dates[res]
}

#################################################################################

Season.Cessation <- function(dates, waterbalance, onset.pars, min.frac)
{
	total.days <- onset.pars$total.days
	min.wb <- onset.pars$min.wb
	if(min.wb == 0) min.wb <- 0.01
	initCol <- ncol(waterbalance)

	yearO <- if(onset.pars$latest$month <= onset.pars$earliest$month) 2015 else 2014
	search.days <- as.numeric(as.Date(paste(yearO, onset.pars$latest$month, onset.pars$latest$day, sep = '-'))
					- as.Date(paste(2014, onset.pars$earliest$month, onset.pars$earliest$day, sep = '-'))) + 1

	wsearch <- 1:search.days
	if(length(wsearch) > nrow(waterbalance)) wsearch <- wsearch[seq(nrow(waterbalance))]
	MATdeb <- waterbalance[wsearch, , drop = FALSE]
	dates <- dates[wsearch]

	## remove NA col
	colID <- colSums(is.na(MATdeb))/nrow(MATdeb) > (1-min.frac)
	colretenu <- seq(initCol)[!colID]
	MATdeb <- MATdeb[, !colID, drop = FALSE]
	if(ncol(MATdeb) == 0) return(rep(NA, initCol))

	#### MATdeb[is.na(MATdeb)] <- 0
	dropWB <- MATdeb < min.wb & !is.na(MATdeb)

	## remove no cessation or replace to the latest date
	colID <- colSums(dropWB) == 0
	colFIN <- colretenu[colID]
	colretenu <- colretenu[!colID]
	dropWB <- dropWB[, !colID, drop = FALSE]
	## MATdeb <- MATdeb[, !colID, drop = FALSE]

	# if(ncol(dropWB) == 0) return(rep(NA, initCol))
	if(ncol(dropWB) == 0){
		res <- rep(NA, initCol)
		if(length(colFIN) > 0) res[colFIN] <- length(wsearch)
		return(dates[res])
	}

	## remove 1st day cessation
	colID <- colSums(dropWB) == nrow(dropWB)
	colDEB <- colretenu[colID]
	colretenu <- colretenu[!colID]
	dropWB <- dropWB[, !colID, drop = FALSE]
	## MATdeb <- MATdeb[, !colID, drop = FALSE]
	if(ncol(dropWB) == 0){
		res <- rep(NA, initCol)
		if(length(colDEB) > 0) res[colDEB] <- 1
		return(dates[res])
	}

	cess <- lapply(seq(ncol(dropWB)), function(j){
		rle.succ <- rle(dropWB[, j])
		isfin <- rle.succ$lengths >= total.days & rle.succ$values
		if(!any(isfin)) return(NA)
		pos <- cumsum(rle.succ$lengths)
		ipos <- which(isfin)-1
		pos <- if(ipos[1] == 0) 1 else pos[ipos[1]]+1
		pos
	})
	cess <- do.call(c, cess)

	res <- rep(NA, initCol)
	if(length(colDEB) > 0) res[colDEB] <- 1

	if(length(colFIN) > 0) res[colFIN] <- length(wsearch)
	res[colretenu] <- cess
	if(all(is.na(res))) res else dates[res]
}








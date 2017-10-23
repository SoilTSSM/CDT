## Hargreaves evapotranspiration

ExtraterrestrialRadiation <- function(lat){
	phi <- pi * (lat)/180
	dates <- seq(as.Date("2000-1-1"), as.Date("2000-12-31"), 'day')

	J <- as.numeric(strftime(dates, format = "%j"))
	fJ <- 2*pi*J/365
	dr <- 1 + 0.033 * cos(fJ)
	delta <- 0.409 * sin(fJ-1.39)

	ws <- matrix(NA, nrow = 366, ncol = length(lat))
	sin2 <- cos2 <- ws
	for(j in seq_along(lat)){
		ws[, j] <- acos(-tan(phi[j]) * tan(delta))
		sin2[, j] <- sin(phi[j]) * sin(delta)
		cos2[, j] <- cos(phi[j]) * cos(delta)
	} 
	Ra <- (37.58603 * dr) * (ws * sin2 + cos2 * sin(ws))
	Ra[Ra < 0] <- 0
	return(Ra)
}

ETPHargreaves <- function(Tmax, Tmin, Ra, Precip = NULL){
	TM <- (Tmax + Tmin)/2
	TD <- Tmax - Tmin
	TD[TD < 0] <- 0
	if(is.null(Precip)){
		# Original Hargreaves equation
		ETP <- 0.0009384 * Ra * (TM + 17.8) * TD^0.5
	}else{
		# Hargreaves modified method
		PRE <- (TD - 0.0123*Precip)^0.76
		PRE[is.nan(PRE)] <- 0
		ETP <- 0.0005304 * Ra * (TM + 17) * PRE
	}
	return(ETP)
}

#################################################################################
## Water Balance

WaterBalance <- function(jj, Precip, ETP, capacity.max = 100){
	etp <- ETP[jj, , drop = FALSE]
	rain <- Precip[jj, , drop = FALSE]

	## NA??
	rain[is.na(rain)] <- 0
	etp[is.na(etp)] <- 0

	ndays <- nrow(rain)
	nbstn <- ncol(rain)
	water.balance <- matrix(NA, nrow = ndays, ncol = nbstn)
	water.balance[1, ] <- 0
	minwb <- rep(0, nbstn)
	maxwb <- rep(capacity.max, nbstn)

	for(iday in 2:ndays){
		# water.balance[iday, ] <- ifelse(is.na(water.balance[iday-1, ]), 0,
		# 								water.balance[iday-1, ] + rain[iday, ] - etp[iday, ])
		water.balance[iday, ] <- water.balance[iday-1, ] + rain[iday, ] - etp[iday, ]
		water.balance[iday, ] <- pmax(minwb, pmin(maxwb, water.balance[iday, ]))
	}

	return(water.balance)
}

#################################################################################
## Onset/Cessation

getIndexCalcOnsetCess <- function(dates, start.month, start.day, n = 0){
	start.lag <- as.Date(paste(2014, start.month, start.day, sep = '-')) - n
	start.month <- as.numeric(format(start.lag, "%m"))
	start.day <- as.numeric(format(start.lag, "%d"))

	dtmp <- range(as.Date(dates, '%Y%m%d'), na.rm = TRUE)
	cdates <- seq(dtmp[1], dtmp[2], "day")
	ystart <- seq(as.Date(paste0(format(cdates[1], '%Y'), '-1-1')), cdates[1], "day")
	ystart <- ystart[-length(ystart)]
	yend <- seq(cdates[length(cdates)], as.Date(paste0(format(cdates[length(cdates)], '%Y'), '-12-31')), "day")
	yend <- yend[-1]
	if(length(ystart) > 0) cdates <- c(ystart, cdates)
	if(length(yend) > 0) cdates <- c(cdates, yend)
	cdates <- format(cdates, "%Y%m%d")

	idrow <- seq(length(dates))
	idrow <- idrow[match(cdates, dates)]

	start.daty <- paste0(str_pad(start.month, 2, pad = '0'), str_pad(start.day, 2, pad = '0'))
	debstart <- substr(cdates, 5, 8)

	rleday1 <- rle(debstart == start.daty)
	cumidx <- cumsum(rleday1$length)
	debidx <- cumidx[rleday1$values]

	firstSplit <- debidx[1] != 1
	if(firstSplit) debidx <- c(1, debidx)
	if(debidx[length(debidx)] != length(cdates)) debidx <- c(debidx, length(cdates)+1)
	idx <- rep(seq(length(debidx)-1), debidx[-1]-debidx[-length(debidx)])
	index <- split(idrow, idx)
	# index <- tapply(idrow, idx, identity)
	if(firstSplit) index <- index[-1]
	return(index)
}

onsetDectection <- function(jj, DATA, dates, pars, min.frac){
	rain.total <- pars$rain.total
	win0.search <- pars$win0.search
	min.rain.day <- pars$min.rain.day
	win1.search <- pars$win1.search
	dry.spell <- pars$dry.spell
	thres.rain.day <- pars$thres.rain.day
	win2.search <- pars$win2.search

	####
	MATdeb <- DATA[jj, , drop = FALSE]
	initCol <- ncol(MATdeb)
	wsearch <- 1:win2.search
	if(length(wsearch) > nrow(MATdeb)) wsearch <- wsearch[seq(nrow(MATdeb))]
	MATdeb <- MATdeb[wsearch, , drop = FALSE]

	## remove NA
	colID <- base::colSums(is.na(MATdeb))/nrow(MATdeb) > (1-min.frac)
	colretenu <- seq(initCol)[!colID]
	MATdeb <- MATdeb[, !colID, drop = FALSE]
	if(ncol(MATdeb) == 0) return(rep(NA, initCol))

	MATdeb[is.na(MATdeb)] <- 0
	MATdeb.truncated <- rbind(matrix(0, nrow = win0.search, ncol = ncol(MATdeb)), head(MATdeb, -win0.search))
	istotal <- (matrixStats::colCumsums(MATdeb) - matrixStats::colCumsums(MATdeb.truncated)) >= rain.total

	## remove (no & all contain) rain.total
	csomTot <- base::colSums(istotal)
	colID0 <- csomTot == 0
	colID1 <- csomTot == nrow(istotal)
	colDEB <- colretenu[colID1]
	colID <- colID0 | colID1
	colretenu <- colretenu[!colID]
	istotal <- istotal[, !colID, drop = FALSE]
	MATdeb <- MATdeb[, !colID, drop = FALSE]
	if(ncol(MATdeb) == 0) return(rep(NA, initCol))

	onset <- mapply(function(x, y){
		ipos <- which(y)
		is.onset <- sapply(ipos, function(i){
			x1 <- x[i+(1:win1.search)]
			x1 <- ifelse(x1 > thres.rain.day, 0, 1)
			x2 <-rle(x1)
			!any(x2$lengths[x2$values == 1] >= dry.spell)
			# !any(rle(x1)$lengths >= dry.spell)
		})
		if(!any(is.onset)) return(NA)
		ipos <- ipos[is.onset]
		istart <- NA
		for(i in ipos){
			io <- i-(win0.search:1)+1
			io <- io[io > 0]
			deb <- io[x[io] >= thres.rain.day]
			if(length(deb) >= min.rain.day){
				istart <- deb[1]
				break
			}
		}
		istart
	}, as.data.frame(MATdeb), as.data.frame(istotal))

	res <- rep(NA, initCol)
	if(length(colDEB) > 0) res[colDEB] <- 1
	res[colretenu] <- onset
	if(all(is.na(res))) res else dates[jj][res]
}

cessationDectection <- function(jj, Precip, ETP, dates, pars, min.frac){
	swh.capacity <- pars$swh.capacity
	min.WB <- pars$min.WB
	length.day <- pars$length.day
	win.search <- pars$win.search
 	WB.lag <- pars$WB.lag
 
	rain <- Precip[jj, , drop = FALSE]
	etp <- ETP[jj, , drop = FALSE]

	initCol <- ncol(rain)
	wsearch <- 1:(win.search+WB.lag)
	if(length(wsearch) > nrow(rain)) wsearch <- wsearch[seq(nrow(rain))]
	etp <- etp[wsearch, , drop = FALSE]
	rain <- rain[wsearch, , drop = FALSE]

	## remove NA
	colID <- base::colSums(is.na(rain) | is.na(etp))/nrow(rain) > (1-min.frac)
	colretenu <- seq(initCol)[!colID]
	rain <- rain[, !colID, drop = FALSE]
	if(ncol(rain) == 0) return(rep(NA, initCol))
	etp <- etp[, !colID, drop = FALSE]

	#### NA??
	rain[is.na(rain)] <- 0
	etp[is.na(etp)] <- 0
	####

	ndays <- nrow(rain)
	nbstn <- ncol(rain)
	water.balance <- matrix(NA, nrow = ndays, ncol = nbstn)
	water.balance[1, ] <- 0
	minwb <- rep(0, nbstn)
	maxwb <- rep(swh.capacity, nbstn)

	for(iday in 2:ndays){
		# water.balance[iday, ] <- ifelse(is.na(water.balance[iday-1, ]), 0,
		# 								water.balance[iday-1, ] + rain[iday, ] - etp[iday, ])
		water.balance[iday, ] <- water.balance[iday-1, ] + rain[iday, ] - etp[iday, ]
		water.balance[iday, ] <- pmax(minwb, pmin(maxwb, water.balance[iday, ]))
	}

	####
	# water.balance <- water.balance[-1, , drop = FALSE]
	oindx <- if(WB.lag == 0) seq(length(jj)) else -(1:WB.lag)
	odates <- dates[jj][oindx]
	water.balance <- water.balance[oindx, , drop = FALSE]
	csWB <- water.balance <= min.WB
	colID <- base::colSums(csWB) == 0
	colretenu <- colretenu[!colID]
	csWB <- csWB[, !colID, drop = FALSE]
	if(ncol(csWB) == 0) return(rep(format(as.Date(odates[1], "%Y%m%d") + win.search, "%Y%m%d"), initCol))
	css <- apply(csWB, 2, function(x){
			rlex <- rle(x)
			fin <- rlex$lengths >= length.day & rlex$values
			if(!any(fin)) return(NA)
			pos <- cumsum(rlex$lengths)
			ipos <- which(fin)-1
			pos <- if(ipos[1] == 0) 1 else pos[ipos[1]]+1
			pos
		})

	res <- rep(NA, initCol)
	res[colretenu] <- css
	if(all(is.na(res))){
		res <- rep(format(as.Date(odates[1], "%Y%m%d") + win.search, "%Y%m%d"), initCol)
	}else{
		res <- odates[res]
		res[is.na(res)] <- format(as.Date(odates[1], "%Y%m%d") + win.search, "%Y%m%d")
	}
	return(res)
}




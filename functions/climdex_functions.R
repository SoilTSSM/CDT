
climdex.completeYear <- function(cdtdata1, cdtdata2 = NULL){
	if(is.null(cdtdata2)){
		dates0 <- cdtdata1$dates[1]
		dates1 <- cdtdata1$dates[length(cdtdata1$dates)]

		ystart <- seq(as.Date(paste(substr(dates0, 1, 4), 1, 1, sep = '-')), as.Date(dates0, "%Y%m%d"), 'day')
		ystart <- ystart[-length(ystart)]
		yend <- seq(as.Date(dates1, "%Y%m%d"), as.Date(paste(substr(dates1, 1, 4), 12, 31, sep = '-')), 'day')
		yend <- yend[-1]

		if(length(ystart) > 0){
			cdtdata1$dates <- c(format(ystart, "%Y%m%d"), cdtdata1$dates)
			cdtdata1$data <- rbind(matrix(NA, ncol = ncol(cdtdata1$data), nrow = length(ystart)), cdtdata1$data)
		}
		if(length(yend) > 0){
			cdtdata1$dates <- c(cdtdata1$dates, format(yend, "%Y%m%d"))
			cdtdata1$data <- rbind(cdtdata1$data, matrix(NA, ncol = ncol(cdtdata1$data), nrow = length(yend)))
		}
		cdtdata <- cdtdata1
	}else{
		dates10 <- cdtdata1$dates[1]
		dates11 <- cdtdata1$dates[length(cdtdata1$dates)]
		dates20 <- cdtdata2$dates[1]
		dates21 <- cdtdata2$dates[length(cdtdata2$dates)]
		dates0 <- if(as.Date(dates10, "%Y%m%d") <= as.Date(dates20, "%Y%m%d")) dates10 else dates20
		dates1 <- if(as.Date(dates11, "%Y%m%d") <= as.Date(dates21, "%Y%m%d")) dates21 else dates11

		ystart1 <- seq(as.Date(paste(substr(dates0, 1, 4), 1, 1, sep = '-')), as.Date(dates10, "%Y%m%d"), 'day')
		ystart1 <- ystart1[-length(ystart1)]
		yend1 <- seq(as.Date(dates11, "%Y%m%d"), as.Date(paste(substr(dates1, 1, 4), 12, 31, sep = '-')), 'day')
		yend1 <- yend1[-1]
		if(length(ystart1) > 0){
			cdtdata1$dates <- c(format(ystart1, "%Y%m%d"), cdtdata1$dates)
			cdtdata1$data <- rbind(matrix(NA, ncol = ncol(cdtdata1$data), nrow = length(ystart1)), cdtdata1$data)
		}
		if(length(yend1) > 0){
			cdtdata1$dates <- c(cdtdata1$dates, format(yend1, "%Y%m%d"))
			cdtdata1$data <- rbind(cdtdata1$data, matrix(NA, ncol = ncol(cdtdata1$data), nrow = length(yend1)))
		}

		ystart2 <- seq(as.Date(paste(substr(dates0, 1, 4), 1, 1, sep = '-')), as.Date(dates20, "%Y%m%d"), 'day')
		ystart2 <- ystart2[-length(ystart2)]
		yend2 <- seq(as.Date(dates21, "%Y%m%d"), as.Date(paste(substr(dates1, 1, 4), 12, 31, sep = '-')), 'day')
		yend2 <- yend2[-1]
		if(length(ystart2) > 0){
			cdtdata2$dates <- c(format(ystart2, "%Y%m%d"), cdtdata2$dates)
			cdtdata2$data <- rbind(matrix(NA, ncol = ncol(cdtdata2$data), nrow = length(ystart2)), cdtdata2$data)
		}
		if(length(yend2) > 0){
			cdtdata2$dates <- c(cdtdata2$dates, format(yend2, "%Y%m%d"))
			cdtdata2$data <- rbind(cdtdata2$data, matrix(NA, ncol = ncol(cdtdata2$data), nrow = length(yend2)))
		}
		cdtdata <- list(cdtdata1, cdtdata2)
	}
	return(cdtdata)
}

climdex.writeNC.Trend <- function(outnc, tDATA, trend.vars, nx, ny){
	nc <- nc_create(outnc, trend.vars)
	ncvar_put(nc, trend.vars[[1]], matrix(tDATA[1, ], ncol = ny, nrow = nx))
	ncvar_put(nc, trend.vars[[2]], matrix(tDATA[2, ], ncol = ny, nrow = nx))
	ncvar_put(nc, trend.vars[[3]], matrix(tDATA[3, ], ncol = ny, nrow = nx))
	ncvar_put(nc, trend.vars[[4]], matrix(tDATA[4, ], ncol = ny, nrow = nx))
	nc_close(nc)
}

climdex.writeNC.Indices <- function(outnc.dir, prefix, DATA, grd.nc, nx, ny){
	for(jj in seq_along(DATA$date)){
		outfile <- file.path(outnc.dir, paste(prefix, "_", DATA$date[jj], ".nc", sep = ""))
		outdata <- matrix(DATA$data[jj, ], ncol = ny, nrow = nx)
		outdata[is.na(outdata)] <- -99
		nc <- nc_create(outfile, grd.nc)
		ncvar_put(nc, grd.nc, outdata)
		nc_close(nc)
	}
}

climdex.Write.Indices <- function(IDX, vars, cdtdataType, indexDIR, nc.pars = NULL)
{
	if(is.null(IDX) | is.null(nc.pars)) return(NULL)

	if(!is.null(IDX[["mIdx"]])){
		idxF.mon <- file.path(indexDIR, vars, "Monthly")
		dir.create(idxF.mon, showWarnings = FALSE, recursive = TRUE)
	}
	idxF.yrs <- file.path(indexDIR, vars, "Yearly")
	dir.create(idxF.yrs, showWarnings = FALSE, recursive = TRUE)
	idxF.tr <- file.path(indexDIR, vars, "Trend")
	dir.create(idxF.tr, showWarnings = FALSE, recursive = TRUE)

	if(cdtdataType == 'netcdf'){
		trend.vars <- nc.pars$trend.vars
		xy.dim <- nc.pars$xy.dim
		nx <- nc.pars$nx
		ny <- nc.pars$ny
		nc.unit <- nc.pars$nc.unit
		nc.prec <- nc.pars$nc.prec
		longname.yrs <- nc.pars$longname.yrs
		nc.var <- tolower(vars)

		if(!is.null(IDX[["mIdx"]])){
			longname.mon <- nc.pars$longname.mon
			grd.idx.m <- ncvar_def(nc.var, nc.unit, xy.dim, -99, longname = longname.mon, prec = nc.prec)
			climdex.writeNC.Indices(idxF.mon, nc.var, IDX[["mIdx"]], grd.idx.m, nx, ny)
		}

		grd.idx.y <- ncvar_def(nc.var, nc.unit, xy.dim, -99, longname = longname.yrs, prec = nc.prec)
		climdex.writeNC.Indices(idxF.yrs, nc.var, IDX[["yIdx"]], grd.idx.y, nx, ny)

		outtrend <- file.path(idxF.tr, paste(vars, "_trend.nc", sep = ""))
		climdex.writeNC.Trend(outtrend, IDX[["tIdx"]], trend.vars, nx, ny)

		IDX$coords <- list(lon = xy.dim[[1]]$vals, lat = xy.dim[[2]]$vals, nlon = nx, nlat = ny)
	}else{
		xhead <- rbind(nc.pars$head$id, nc.pars$head$lon, nc.pars$head$lat)
		if(!is.null(IDX[["mIdx"]])){
			mhead <- cbind(c('ID.STN', 'LON', 'MONTH/LAT'), xhead)
			cdt.mon <- cbind(IDX$mIdx$date, IDX$mIdx$data)
			cdt.mon <- rbind(mhead, cdt.mon)
			cdt.mon[is.na(cdt.mon)] <- -99
			writeFiles(cdt.mon, file.path(idxF.mon, paste(vars, ".csv", sep = "")))
		}
		
		yhead <- cbind(c('ID.STN', 'LON', 'YEAR/LAT'), xhead)
		cdt.yrs <- cbind(IDX$yIdx$date, IDX$yIdx$data)
		cdt.yrs <- rbind(yhead, cdt.yrs)
		cdt.yrs[is.na(cdt.yrs)] <- -99
		writeFiles(cdt.yrs, file.path(idxF.yrs, paste(vars, ".csv", sep = "")))

		thead <- cbind(c('ID.STN', 'LON', 'VARS/LAT'), xhead)
		cdt.tr <- cbind(rownames(IDX$tIdx), IDX$tIdx)
		cdt.tr <- rbind(thead, cdt.tr)
		cdt.tr[is.na(cdt.tr)] <- -99
		writeFiles(cdt.tr, file.path(idxF.tr, paste(vars, ".csv", sep = "")))
		IDX$coords <- nc.pars$head
	}

	IDX$datatype <- cdtdataType
	assign(vars, IDX, envir = environment())
	save(list = vars, file = file.path(indexDIR, vars, paste(vars, ".RData", sep = "")))
	return(0)
}


################

climdex.getTrend.lsfit <- function(y, x){
	ina <- !is.na(y)
	if(length(which(ina)) < 10) return(c(Slope = NA, STD.of.Slope = NA, P.Value = NA, R2 = NA))
	nx <- x[ina]
	ny <- y[ina]
	nyear <- nx[length(nx)]-nx[1]+1
	trFit <- ls.print(lsfit(nx, ny), print.it = FALSE)
	beta <- round(as.numeric(trFit$coef.table[[1]][2, 1])/nyear, 3)
	std <- round(as.numeric(trFit$coef.table[[1]][2, 2]), 3)
	pval <- round(as.numeric(trFit$summary[1, 6]), 3)
	r2 <- round(as.numeric(trFit$summary[1, 2]), 3)
	pval[is.nan(pval)] <- NA
	# signif <- if(!is.na(pval)) if(pval < 0.05) 1 else 0 else NA
	c(Slope = beta, STD.of.Slope = std, P.Value = pval, R2 = r2)
}

quantile8 <- function(x, probs){
	x <- x[!is.na(x)]
	nl <- length(x)
	if(nl == 0) return(rep(NA, length(probs)))
	# xs <- sort.int(x, decreasing = FALSE)
	xs <- .Internal(sort(x, FALSE))
	xq <- nl * probs + 0.3333 * probs + 0.3333
	ix <- trunc(xq)
	xs[ix] + (xq - ix) * (xs[ix + 1] - xs[ix])
}

################

climdex.indexSN <- function(dates){
	Xyear <- as.numeric(substr(dates, 1, 4))
	Xmonth <- as.numeric(substr(dates, 5, 6))
	yRange <- range(Xyear)

	idxS <- lapply(yRange[1]:(yRange[2]-1), function(an){
		iAn0 <- which(Xyear == an)
		iAn1 <- which(Xyear == an+1)
		c(iAn0[Xmonth[iAn0] > 6], iAn1[Xmonth[iAn1] < 7])
	})

	idxN <- lapply(yRange[1]:yRange[2], function(an){
		iAn <- which(Xyear == an)
		c(iAn[Xmonth[iAn] < 7], iAn[Xmonth[iAn] > 6])
	})
	list(idxS = idxS, idxN = idxN)
}

climdex.NACount.MonDay <- function(cdtdata, separe = FALSE){
	nbstn <- ncol(cdtdata$data)
	transPose <- if(nbstn > 1) t else as.matrix
	foo <- function(x, DATA){
		MAT <- DATA[x, , drop = FALSE]
		base::colSums(MAT)
	}

	nadat <- is.na(cdtdata$data)
	midx <- tapply(seq(nrow(nadat)), substr(cdtdata$dates, 1, 6), identity)
	nacount <- transPose(sapply(midx, foo, DATA = nadat))

	yearmon <- rownames(nacount)
	year <- substr(yearmon, 1, 4)
	yyear <- as.numeric(unique(year))

	xdate <- as.Date(paste(yearmon, '15', sep = ''), '%Y%m%d')
	xdate <- c(xdate, addMonths(xdate[length(xdate)], 1))
	enddays <- matrix(rep(as.numeric(diff(xdate)), nbstn), ncol = nbstn)

	if(!separe){
		yidx <- tapply(seq(nrow(nacount)), year, identity)
		ynacount <- transPose(sapply(yidx, foo, DATA = nacount))
	}else{
		yidx <- climdex.indexSN(yearmon)
		nyear <- diff(range(as.numeric(substr(yearmon, 1, 4))))+1
		South <- cdtdata$lat < 0

		if(all(South)){
			ynacount <- transPose(sapply(yidx$idxS, foo, DATA = nacount))
			yyear <- yyear[-length(yyear)]
		}else if(all(!South)){
			ynacount <- transPose(sapply(yidx$idxN, foo, DATA = nacount))
		}else{
			ynacount <- matrix(NA, ncol = nbstn, nrow = nyear)
			ynacount[-nyear, South] <- transPose(sapply(yidx$idxS, foo, DATA = nacount[, South, drop = FALSE]))
			ynacount[, !South] <- transPose(sapply(yidx$idxN, foo, DATA = nacount[, !South, drop = FALSE]))
		}
		ynacount[is.na(ynacount)] <- 365
	}

	list(mnacount = nacount, ynacount = ynacount, enddays = enddays,
		yearmon = yearmon, year = year, yyear = yyear)
}

################

climdex.RX1day <- function(cdtPrecip, nacount, separe = FALSE){
	nbstn <- ncol(cdtPrecip$data)
	transPose <- if(nbstn > 1) t else as.matrix
	foo <- function(x, DATA){
		MAT <- DATA[x, , drop = FALSE]
		matrixStats::colMaxs(MAT, na.rm = TRUE)
	}

	idx1 <- tapply(seq(nrow(cdtPrecip$data)), substr(cdtPrecip$dates, 1, 6), identity)
	mRX1 <- transPose(sapply(idx1, foo, DATA = cdtPrecip$data))

	yearmon <- as.numeric(nacount$yearmon)
	yyear <- nacount$yyear

	mRX1[nacount$mnacount > 3] <- NA
	dimnames(mRX1) <- NULL

	if(!separe){
		idx2 <- tapply(seq(length(nacount$year)), nacount$year, identity)
		yRX1 <- transPose(sapply(idx2, foo, DATA = mRX1))
	}else{
		idx2 <- climdex.indexSN(nacount$yearmon)
		nyear <- diff(range(as.numeric(substr(nacount$yearmon, 1, 4))))+1
		South <- cdtPrecip$lat < 0

		if(all(South)){
			yRX1 <- transPose(sapply(idx2$idxS, foo, DATA = mRX1))
		}else if(all(!South)){
			yRX1 <- transPose(sapply(idx2$idxN, foo, DATA = mRX1))
		}else{
			yRX1 <- matrix(NA, ncol = nbstn, nrow = nyear)
			yRX1[-nyear, South] <- transPose(sapply(idx2$idxS, foo, DATA = mRX1[, South, drop = FALSE]))
			yRX1[, !South] <- transPose(sapply(idx2$idxN, foo, DATA = mRX1[, !South, drop = FALSE]))
		}
	}

	yRX1[nacount$ynacount > 0] <- NA
	dimnames(yRX1) <- NULL

	trRX1 <- apply(yRX1, 2, climdex.getTrend.lsfit, x = yyear)

	list(mIdx = list(date = yearmon, data = mRX1),
		yIdx = list(date = yyear, data = yRX1),
		tIdx = trRX1)
}

climdex.RX5day <- function(cdtPrecip, nacount, winsize = 5, separe = FALSE){
	nl <- nrow(cdtPrecip$data)
	nbstn <- ncol(cdtPrecip$data)
	transPose <- if(nbstn > 1) t else as.matrix
	foo <- function(x, DATA){
		MAT <- DATA[x, , drop = FALSE]
		matrixStats::colMaxs(MAT, na.rm = FALSE)
	}

	CX <- cdtPrecip$data
	CX[is.na(CX)] <- 0
	csCX <- matrixStats::colCumsums(CX)
	rsum <- csCX[winsize:nl, , drop = FALSE] - rbind(0, csCX[1:(nl - winsize), , drop = FALSE])
	rsum <- rbind(matrix(NA, nrow = winsize-1, ncol = ncol(CX)), rsum)

	idx1 <- tapply(seq(nrow(cdtPrecip$data)), substr(cdtPrecip$dates, 1, 6), identity)
	mRX5 <- transPose(sapply(idx1, foo, DATA = rsum))

	yearmon <- as.numeric(nacount$yearmon)
	yyear <- nacount$yyear

	mRX5[nacount$mnacount > 3] <- NA
	dimnames(mRX5) <- NULL

	if(!separe){
		idx2 <- tapply(seq(length(nacount$year)), nacount$year, identity)
		yRX5 <- transPose(sapply(idx2, foo, DATA = mRX5))
	}else{
		idx2 <- climdex.indexSN(nacount$yearmon)
		nyear <- diff(range(as.numeric(substr(nacount$yearmon, 1, 4))))+1
		South <- cdtPrecip$lat < 0

		if(all(South)){
			yRX5 <- transPose(sapply(idx2$idxS, foo, DATA = mRX5))
		}else if(all(!South)){
			yRX5 <- transPose(sapply(idx2$idxN, foo, DATA = mRX5))
		}else{
			yRX5 <- matrix(NA, ncol = nbstn, nrow = nyear)
			yRX5[-nyear, South] <- transPose(sapply(idx2$idxS, foo, DATA = mRX5[, South, drop = FALSE]))
			yRX5[, !South] <- transPose(sapply(idx2$idxN, foo, DATA = mRX5[, !South, drop = FALSE]))
		}
	}

	yRX5[nacount$ynacount > 0] <- NA
	dimnames(yRX5) <- NULL

	trRX5 <- apply(yRX5, 2, climdex.getTrend.lsfit, x = yyear)

	list(mIdx = list(date = yearmon, data = mRX5),
		yIdx = list(date = yyear, data = yRX5),
		tIdx = trRX5)
}

climdex.SDII <- function(cdtPrecip, nacount, separe = FALSE){
	nbstn <- ncol(cdtPrecip$data)
	transPose <- if(nbstn > 1) t else as.matrix
	foo <- function(x, DATA){
		MAT <- DATA[x, , drop = FALSE]
		MAT[MAT < 1] <- NA
		base::colMeans(MAT, na.rm = TRUE)
	}

	if(!separe){
		idx <- tapply(seq(length(cdtPrecip$dates)), substr(cdtPrecip$dates, 1, 4), identity)
		sdii <- transPose(sapply(idx, foo, DATA = cdtPrecip$data))
	}else{
		idx <- climdex.indexSN(cdtPrecip$dates)
		nyear <- diff(range(as.numeric(substr(cdtPrecip$dates, 1, 4))))+1
		South <- cdtPrecip$lat < 0

		if(all(South)){
			sdii <- transPose(sapply(idx$idxS, foo, DATA = cdtPrecip$data))
		}else if(all(!South)){
			sdii <- transPose(sapply(idx$idxN, foo, DATA = cdtPrecip$data))
		}else{
			sdii <- matrix(NA, ncol = nbstn, nrow = nyear)
			sdii[-nyear, South] <- transPose(sapply(idx$idxS, foo, DATA = cdtPrecip$data[, South, drop = FALSE]))
			sdii[, !South] <- transPose(sapply(idx$idxN, foo, DATA = cdtPrecip$data[, !South, drop = FALSE]))
		}
	}

	sdii[nacount$ynacount > 15] <- NA
	dimnames(sdii) <- NULL

	yyear <- nacount$yyear

	trSDII <- apply(sdii, 2, climdex.getTrend.lsfit, x = yyear)

	list(yIdx = list(date = yyear, data = sdii), tIdx = trSDII)
}

climdex.RRsupmm <- function(cdtPrecip, nacount, xup, separe = FALSE){
	nbstn <- ncol(cdtPrecip$data)
	transPose <- if(nbstn > 1) t else as.matrix
	foo <- function(x, DATA, xup){
		MAT <- DATA[x, , drop = FALSE]
		MAT <- MAT >= xup & !is.na(MAT)
		base::colSums(MAT, na.rm = TRUE)
	}

	if(!separe){
		idx <- tapply(seq(length(cdtPrecip$dates)), substr(cdtPrecip$dates, 1, 4), identity)
		Rnn <- transPose(sapply(idx, foo, DATA = cdtPrecip$data, xup = xup))
	}else{
		idx <- climdex.indexSN(cdtPrecip$dates)
		nyear <- diff(range(as.numeric(substr(cdtPrecip$dates, 1, 4))))+1
		South <- cdtPrecip$lat < 0

		if(all(South)){
			Rnn <- transPose(sapply(idx$idxS, foo, DATA = cdtPrecip$data, xup = xup))
		}else if(all(!South)){
			Rnn <- transPose(sapply(idx$idxN, foo, DATA = cdtPrecip$data, xup = xup))
		}else{
			Rnn <- matrix(NA, ncol = nbstn, nrow = nyear)
			Rnn[-nyear, South] <- transPose(sapply(idx$idxS, foo, DATA = cdtPrecip$data[, South, drop = FALSE], xup = xup))
			Rnn[, !South] <- transPose(sapply(idx$idxN, foo, DATA = cdtPrecip$data[, !South, drop = FALSE], xup = xup))
		}
	}

	Rnn[nacount$ynacount > 15] <- NA
	dimnames(Rnn) <- NULL

	yyear <- nacount$yyear

	trRnn <- apply(Rnn, 2, climdex.getTrend.lsfit, x = yyear)

	list(yIdx = list(date = yyear, data = Rnn), tIdx = trRnn)
}

climdex.consecDaySpell <- function(cdtPrecip, nacount, index = 'CDD', separe = FALSE){
	nbstn <- ncol(cdtPrecip$data)
	transPose <- if(nbstn > 1) t else as.matrix
	op <- if(index == 'CDD') `<` else `>=`

	foo <- function(ix, DATA, op){
		MAT <- DATA[ix, , drop = FALSE]
		MAT <- op(MAT, 1) & !is.na(MAT)
		apply(MAT, 2, function(x){
			if(!any(x)) return(NA)
			xx <- rle(x)
			max(xx$lengths[xx$values])
		})
	}

	if(!separe){
		idx <- tapply(seq(length(cdtPrecip$dates)), substr(cdtPrecip$dates, 1, 4), identity)
		xD <- transPose(sapply(idx, foo, DATA = cdtPrecip$data, op = op))
	}else{
		idx <- climdex.indexSN(cdtPrecip$dates)
		nyear <- diff(range(as.numeric(substr(cdtPrecip$dates, 1, 4))))+1
		South <- cdtPrecip$lat < 0

		if(all(South)){
			xD <- transPose(sapply(idx$idxS, foo, DATA = cdtPrecip$data, op = op))
		}else if(all(!South)){
			xD <- transPose(sapply(idx$idxN, foo, DATA = cdtPrecip$data, op = op))
		}else{
			xD <- matrix(NA, ncol = nbstn, nrow = nyear)
			xD[-nyear, South] <- transPose(sapply(idx$idxS, foo, DATA = cdtPrecip$data[, South, drop = FALSE], op = op))
			xD[, !South] <- transPose(sapply(idx$idxN, foo, DATA = cdtPrecip$data[, !South, drop = FALSE], op = op))
		}
	}

	xD[nacount$ynacount > 15] <- NA
	dimnames(xD) <- NULL

	yyear <- nacount$yyear

	trxD <- apply(xD, 2, climdex.getTrend.lsfit, x = yyear)

	list(yIdx = list(date = yyear, data = xD), tIdx = trxD)
}

climdex.RRsupTOT <- function(cdtPrecip, xthres, separe){
	nbstn <- ncol(cdtPrecip$data)
	transPose <- if(nbstn > 1) t else as.matrix
	foo <- function(ix, DATA, xthres){
		MAT <- DATA[ix, , drop = FALSE]
		imat <- t(t(MAT) <= xthres) & !is.na(MAT)
		MAT[imat] <- NA
		base::colSums(MAT, na.rm = TRUE)
	}

	if(!separe){
		idx <- tapply(seq(length(cdtPrecip$dates)), substr(cdtPrecip$dates, 1, 4), identity)
		ret <- transPose(sapply(idx, foo, DATA = cdtPrecip$data, xthres = xthres))
	}else{
		idx <- climdex.indexSN(cdtPrecip$dates)
		nyear <- diff(range(as.numeric(substr(cdtPrecip$dates, 1, 4))))+1
		South <- cdtPrecip$lat < 0

		if(all(South)){
			ret <- transPose(sapply(idx$idxS, foo, DATA = cdtPrecip$data, xthres = xthres))
		}else if(all(!South)){
			ret <- transPose(sapply(idx$idxN, foo, DATA = cdtPrecip$data, xthres = xthres))
		}else{
			ret <- matrix(NA, ncol = nbstn, nrow = nyear)
			ret[-nyear, South] <- transPose(sapply(idx$idxS, foo, DATA = cdtPrecip$data[, South, drop = FALSE], xthres = xthres))
			ret[, !South] <- transPose(sapply(idx$idxN, foo, DATA = cdtPrecip$data[, !South, drop = FALSE], xthres = xthres))
		}
	}
	ret
}

climdex.RRqqTOT <- function(cdtPrecip, nacount, startyear, endyear, separe = FALSE){
	nbstn <- ncol(cdtPrecip$data)
	Xyear <- as.numeric(substr(cdtPrecip$dates, 1, 4))
	Xprcp <- cdtPrecip$data[Xyear%in%(startyear:endyear), , drop = FALSE]
	Xprcp[Xprcp < 1] <- NA
	xquant <- sapply(seq(nbstn), function(j) quantile8(Xprcp[, j], probs = c(0.95, 0.99)))
	xQ95 <- as.numeric(xquant[1, ])
	xQ99 <- as.numeric(xquant[2, ])

	R95pTOT <- climdex.RRsupTOT(cdtPrecip, xQ95, separe = separe)
	R95pTOT[nacount$ynacount > 15] <- NA
	dimnames(R95pTOT) <- NULL
	R99pTOT <- climdex.RRsupTOT(cdtPrecip, xQ99, separe = separe)
	R99pTOT[nacount$ynacount > 15] <- NA
	dimnames(R99pTOT) <- NULL

	yyear <- nacount$yyear

	trR95pTOT <- apply(R95pTOT, 2, climdex.getTrend.lsfit, x = yyear)
	trR99pTOT <- apply(R99pTOT, 2, climdex.getTrend.lsfit, x = yyear)

	list(R95pTOT = R95pTOT, R99pTOT = R99pTOT, date = yyear,
		tR95pTOT = trR95pTOT, tR99pTOT = trR99pTOT)
}

climdex.RRthresTOT <- function(cdtPrecip, nacount, thres, separe = FALSE){
	nbstn <- ncol(cdtPrecip$data)
	xthres <- rep(thres-1e-5, nbstn)
	PRCPTOT <- climdex.RRsupTOT(cdtPrecip, xthres, separe = separe)

	PRCPTOT[nacount$ynacount > 15] <- NA
	dimnames(PRCPTOT) <- NULL

	yyear <- nacount$yyear

	trPRCPTOT <- apply(PRCPTOT, 2, climdex.getTrend.lsfit, x = yyear)

	list(yIdx = list(date = yyear, data = PRCPTOT), tIdx = trPRCPTOT)
}

################

climdex.TempDailyExtreme <- function(cdtdata, nacount, separe = FALSE){
	nbstn <- ncol(cdtdata$data)
	foo <- function(x, DATA){
		MAT <- DATA[x, , drop = FALSE]
		TTn <- matrixStats::colMins(MAT, na.rm = TRUE)
		TTx <- matrixStats::colMaxs(MAT, na.rm = TRUE)
		list(TTn, TTx)
	}

	idx1 <- tapply(seq(nrow(cdtdata$data)), substr(cdtdata$dates, 1, 6), identity)
	mTTxn <- t(sapply(idx1, foo, DATA = cdtdata$data))

	mTTn <- do.call(rbind, mTTxn[, 1])
	mTTx <- do.call(rbind, mTTxn[, 2])

	mTTn[nacount$mnacount > 3] <- NA
	dimnames(mTTn) <- NULL
	mTTx[nacount$mnacount > 3] <- NA
	dimnames(mTTx) <- NULL

	if(!separe){
		idx2 <- tapply(seq(nrow(cdtdata$data)), substr(cdtdata$dates, 1, 4), identity)
		yTTxn <- t(sapply(idx2,  foo, DATA = cdtdata$data))
	}else{
		idx2 <- climdex.indexSN(cdtdata$dates)
		nyear <- diff(range(as.numeric(substr(cdtdata$dates, 1, 4))))+1
		South <- cdtdata$lat < 0

		if(all(South)){
			yTTxn <- t(sapply(idx2$idxS, foo, DATA = cdtdata$data))
		}else if(all(!South)){
			yTTxn <- t(sapply(idx2$idxN, foo, DATA = cdtdata$data))
		}else{
			yTTxnS <- t(sapply(idx2$idxS, foo, DATA = cdtdata$data[, South, drop = FALSE]))
			yTTxnN <- t(sapply(idx2$idxN, foo, DATA = cdtdata$data[, !South, drop = FALSE]))

			yTTn <- matrix(NA, ncol = nbstn, nrow = nyear)
			yTTn[-nyear, South] <- do.call(rbind, yTTxnS[, 1])
			yTTn[, !South] <- do.call(rbind, yTTxnN[, 1])
			yTTn <- lapply(seq(nyear), function(j) yTTn[j, ])
			yTTx <- matrix(NA, ncol = nbstn, nrow = nyear)
			yTTx[-nyear, South] <- do.call(rbind, yTTxnS[, 2])
			yTTx[, !South] <- do.call(rbind, yTTxnN[, 2])
			yTTx <- lapply(seq(nyear), function(j) yTTx[j, ])
			yTTxn <- cbind(yTTn, yTTx)
		}
	}

	yTTn <- do.call(rbind, yTTxn[, 1])
	yTTx <- do.call(rbind, yTTxn[, 2])

	yTTn[nacount$ynacount > 15] <- NA
	dimnames(yTTn) <- NULL
	yTTx[nacount$ynacount > 15] <- NA
	dimnames(yTTx) <- NULL

	yearmon <- as.numeric(nacount$yearmon)
	yyear <- nacount$yyear

	trTTn <- apply(yTTn, 2, climdex.getTrend.lsfit, x = yyear)
	trTTx <- apply(yTTx, 2, climdex.getTrend.lsfit, x = yyear)

	list(mTTxn = list(date = yearmon, data = list(low = mTTn, up = mTTx)),
		yTTxn = list(date = yyear, data = list(low = yTTn, up = yTTx)),
		tTTn = trTTn, tTTx = trTTx)
}

climdex.MONTHQ1090 <- function(cdtdata, startyear, endyear, winsize, combine, maxpercNA = 25, separe = FALSE){
	MONTHQ1090 <- NULL
	Xyear <- as.numeric(substr(cdtdata$dates, 1, 4))
	feb29 <- which(substr(cdtdata$dates, 5, 8) == '0229')

	ixnorm <- index.data.rollNormal(cdtdata$dates, startyear, endyear, winsize)
	ixdays <- index.data.rollClimato(cdtdata$dates, startyear, endyear, winsize)
	nbstn <- ncol(cdtdata$data)

	####################
	## remove station/grid with na more than minpercNA for the reference period
	pNArm <- (endyear-startyear+1) * winsize * maxpercNA/100

	naClim <- is.na(cdtdata$data[ixnorm[!ixnorm%in%feb29], , drop = FALSE])
	NAoneday <- t(sapply(1:365, function(j) base::colSums(naClim[ixdays[[j]], , drop = FALSE]) > pNArm))
	NAoneday <- colAnys(NAoneday)

	iomit <- which(NAoneday)
	if(length(iomit) == nbstn){
		MONTHQ1090$status <- FALSE
		return(NULL)
	}

	MONTHQ1090$omitted <- FALSE
	if(length(iomit) > 0){
		MONTHQ1090$stnOut <- NAoneday
		MONTHQ1090$omitted <- TRUE
		cdtdata$id <- cdtdata$id[!NAoneday]
		cdtdata$lon <- cdtdata$lon[!NAoneday]
		cdtdata$lat <- cdtdata$lat[!NAoneday]
		cdtdata$data <- cdtdata$data[, !NAoneday, drop = FALSE]
		nbstn <- ncol(cdtdata$data)
	}

	XdatClim <- cdtdata$data[ixnorm[!ixnorm%in%feb29], , drop = FALSE]

	xquant <- sapply(1:365, function(j){
		 sapply(seq(nbstn), function(i) quantile8(XdatClim[ixdays[[j]], i], probs = c(0.1, 0.9)))
	})

	monQ10 <- t(xquant[seq(1, 2*nbstn, 2), , drop = FALSE]) - 1e-05
	monQ90 <- t(xquant[seq(2, 2*nbstn, 2), , drop = FALSE]) + 1e-05

	MONTHQ1090$status <- TRUE
	MONTHQ1090$monQ10 <- monQ10
	MONTHQ1090$monQ90 <- monQ90

	MONTHQ1090$cdtdata <- cdtdata
	MONTHQ1090$XdatClim <- XdatClim
	MONTHQ1090$nbstn <- nbstn

	MONTHQ1090$Xyear <- Xyear
	MONTHQ1090$feb29 <- feb29

	MONTHQ1090$ixnorm <- ixnorm
	MONTHQ1090$ixdays <- ixdays

	MONTHQ1090$startyear <- startyear
	MONTHQ1090$endyear <- endyear
	MONTHQ1090$winsize <- winsize
	MONTHQ1090$maxpercNA <- maxpercNA
	MONTHQ1090$combine <- combine
	MONTHQ1090$separe <- separe
	MONTHQ1090$nacount <- climdex.NACount.MonDay(cdtdata, separe = separe)

	return(MONTHQ1090)
}

climdex.TempQ1090 <- function(MONTHQ1090, bootsrap = TRUE){
	if(is.null(MONTHQ1090$status)){
		InsertMessagesTxt(main.txt.out, "No daily temperature data found, can not calculate Exceedance rate", format = TRUE)
		return(NULL)
	}
	if(!MONTHQ1090$status){
		InsertMessagesTxt(main.txt.out, paste("More than", MONTHQ1090$maxpercNA,
						"% data missing, Exceedance rate will not be calculated!!"), format = TRUE)
		return(NULL)
	}

	Xyear <- MONTHQ1090$Xyear
	feb29 <- MONTHQ1090$feb29

	ixnorm <- MONTHQ1090$ixnorm
	ixdays <- MONTHQ1090$ixdays
	nbstn <- MONTHQ1090$nbstn

	startyear <- MONTHQ1090$startyear
	endyear <- MONTHQ1090$endyear
	winsize <- MONTHQ1090$winsize

	cdtdata <- MONTHQ1090$cdtdata

	####################
	ihorsClim <- (Xyear < startyear) | (Xyear > endyear)
	yhorsClim <- Xyear[ihorsClim]
	XdathorsClim <- cdtdata$data[ihorsClim, , drop = FALSE]

	XdatClim <- MONTHQ1090$XdatClim
	Xsdata <- lapply(ixdays, function(j) XdatClim[j, , drop = FALSE])

	nbyear1 <- endyear - startyear
	xmois <- table.annuel()[, 2]

	transPose <- if(nbstn > 1) t else as.matrix

	####################################################

	nb_cores <- detectCores()-1
	doparallel <- if(nb_cores < 3) FALSE else TRUE

	if(doparallel){
		klust <- makeCluster(nb_cores)
		registerDoParallel(klust)
		`%parLoop%` <- `%dopar%`
		closeklust <- TRUE
	}else{
		`%parLoop%` <- `%do%`
		closeklust <- FALSE
	}

	toExports <- c('nbyear1', 'winsize', 'is.leapyear', 'quantile8')

	##### boot strap #####
	idxN0 <- rep(seq(nbyear1+1), each = winsize)
	idxN <- rep(seq(nbyear1), each = winsize)
	lidxN0 <- length(idxN0)
	idxnorm <- seq(lidxN0)
	seqBootstrap <- if(bootsrap) seq(nbyear1) else 1

	AllYearBoot <- lapply(seq(nbyear1+1), function(ian){
		if(bootsrap){
			yearBootinXsdata <- matrix(0, nrow = lidxN0, ncol = nbyear1)
			for (k in seqBootstrap){
				xtmp <- cbind(idxnorm, idxN0)
				xtmp <- xtmp[xtmp[, 2] != ian, ]
				xtmp[, 2] <- idxN
				yearBootinXsdata[, k] <- c(xtmp[xtmp[, 2] == k, 1], xtmp[, 1])
			}
		}else{
			yearBootinXsdata <- matrix(idxnorm, ncol = 1)
		}
		yearBootinXsdata
	})

	fooBootStrap <- function(ii, year, Xdat){
		xquant <- sapply(Xsdata, function(x){
			  sapply(seq(nbstn), function(i) quantile8(x[ii, i], probs = c(0.1, 0.9)))
		})
		monQ10a <- t(xquant[seq(1, 2*nbstn, 2), , drop = FALSE]) - 1e-05
		monQ90a <- t(xquant[seq(2, 2*nbstn, 2), , drop = FALSE]) + 1e-05

		retQ10 <- Xdat - monQ10a
		retQ10 <- retQ10 < 0 & !is.na(retQ10)
		monQ10a <- transPose(sapply(1:12, function(i){
			qtmp <- base::colSums(retQ10[xmois == i, , drop = FALSE])
			if(is.leapyear(year) & i == 2){
				thisYear <- which(Xyear == year)
				xx <- cdtdata$data[thisYear[thisYear%in%feb29], ]
				ix <- xx < monQ10a[59, ] & !is.na(xx) & !is.na(monQ10a[58, ])
				qtmp[ix] <- qtmp[ix]+1
			}
			qtmp
		}))

		retQ90 <- Xdat - monQ90a
		retQ90 <- retQ90 > 0 & !is.na(retQ90)
		monQ90a <- transPose(sapply(1:12, function(i){
			qtmp <- base::colSums(retQ90[xmois == i, , drop = FALSE])
			if(is.leapyear(year) & i == 2){
				thisYear <- which(Xyear == year)
				xx <- cdtdata$data[thisYear[thisYear%in%feb29], ]
				ix <- xx > monQ90a[59, ] & !is.na(xx) & !is.na(monQ90a[58, ])
				qtmp[ix] <- qtmp[ix]+1
			}
			qtmp
		}))

		rbind(monQ10a, monQ90a)
	}

	iinorm <- which(Xyear%in%(startyear:endyear))
	Yrnorm2 <- Xyear[iinorm[!iinorm%in%feb29]]
	Xdatnorm2 <- cdtdata$data[iinorm[!iinorm%in%feb29], , drop = FALSE]

	## Add parallel
	MONQ1090 <- foreach(year = startyear:endyear, .export = toExports) %parLoop% {
		ian <- year - (startyear - 1)
		Xdat <- Xdatnorm2[Yrnorm2 == year, , drop = FALSE]
		Reduce('+', lapply(seqBootstrap, function(k) fooBootStrap(AllYearBoot[[ian]][, k], year, Xdat)))
	}

	if(closeklust) stopCluster(klust)

	###########
	monQ10 <- do.call(rbind, lapply(MONQ1090, function(x) x[1:12, , drop = FALSE]))
	monQ90 <- do.call(rbind, lapply(MONQ1090, function(x) x[13:24, , drop = FALSE]))

	if(bootsrap){
		monQ10 <- monQ10/nbyear1
		monQ90 <- monQ90/nbyear1
	}

	####################################################

	seqYear <- unique(yhorsClim)
	MONQ1090 <- lapply(seqYear, function(year){
		if(is.leapyear(year)){
			monQ10 <- MONTHQ1090$monQ10[c(1:59, 59, 60:365), , drop = FALSE]
			monQ90 <- MONTHQ1090$monQ90[c(1:59, 59, 60:365), , drop = FALSE]
			xmois1 <- c(xmois[1:59], 2, xmois[60:365])
		}else{
			monQ10 <- MONTHQ1090$monQ10
			monQ90 <- MONTHQ1090$monQ90
			xmois1 <- xmois
		}

		retQ10 <- XdathorsClim[yhorsClim == year, , drop = FALSE] - monQ10
		retQ10 <- retQ10 < 0 & !is.na(retQ10)
		monQ10 <- transPose(sapply(1:12, function(i){
			base::colSums(retQ10[xmois1 == i, , drop = FALSE])
		}))

		retQ90 <- XdathorsClim[yhorsClim == year, , drop = FALSE] - monQ90
		retQ90 <- retQ90 > 0 & !is.na(retQ90)
		monQ90 <- transPose(sapply(1:12, function(i){
			base::colSums(retQ90[xmois1 == i, , drop = FALSE])
		}))

		rbind(monQ10, monQ90)
	})

	monQ10x <- do.call(rbind, lapply(MONQ1090, function(x) x[1:12, , drop = FALSE]))
	monQ90x <- do.call(rbind, lapply(MONQ1090, function(x) x[13:24, , drop = FALSE]))

	####################################################

	seqYear1 <- rep(seqYear, each = 12)
	MONQ10 <- rbind(monQ10x[seqYear1 < startyear, , drop = FALSE], monQ10, monQ10x[seqYear1 > endyear, , drop = FALSE])
	MONQ90 <- rbind(monQ90x[seqYear1 < startyear, , drop = FALSE], monQ90, monQ90x[seqYear1 > endyear, , drop = FALSE])

	########
	mnacount <- MONTHQ1090$nacount$mnacount
	enddays <- MONTHQ1090$nacount$enddays
	year <- MONTHQ1090$nacount$year
	ynacount <- MONTHQ1090$nacount$ynacount
	yearmon <- as.numeric(MONTHQ1090$nacount$yearmon)
	yyear <- MONTHQ1090$nacount$yyear

	MONQ10[mnacount > 10] <- NA
	MONQ10 <- MONQ10 * enddays/(enddays - mnacount)
	MONQ90[mnacount > 10] <- NA
	MONQ90 <- MONQ90 * enddays/(enddays - mnacount)

	########
	foo <- function(x, DATA){
		MAT <- DATA[x, , drop = FALSE]
		base::colSums(MAT, na.rm = TRUE)
	}

	if(!MONTHQ1090$separe){
		idx <- tapply(seq(nrow(MONQ10)), year, identity)
		YEARQ10 <- transPose(sapply(idx, foo, DATA = MONQ10))
		YEARQ90 <- transPose(sapply(idx, foo, DATA = MONQ90))
	}else{
		idx <- climdex.indexSN(yearmon)
		nyear <- diff(range(as.numeric(substr(yearmon, 1, 4))))+1
		South <- cdtdata$lat < 0

		if(all(South)){
			YEARQ10 <- transPose(sapply(idx$idxS, foo, DATA = MONQ10))
			YEARQ90 <- transPose(sapply(idx$idxS, foo, DATA = MONQ90))
		}else if(all(!South)){
			YEARQ10 <- transPose(sapply(idx$idxN, foo, DATA = MONQ10))
			YEARQ90 <- transPose(sapply(idx$idxN, foo, DATA = MONQ90))
		}else{
			YEARQ10 <- matrix(NA, ncol = nbstn, nrow = nyear)
			YEARQ10[-nyear, South] <- transPose(sapply(idx$idxS, foo, DATA = MONQ10[, South, drop = FALSE]))
			YEARQ10[, !South] <- transPose(sapply(idx$idxN, foo, DATA = MONQ10[, !South, drop = FALSE]))

			YEARQ90 <- matrix(NA, ncol = nbstn, nrow = nyear)
			YEARQ90[-nyear, South] <- transPose(sapply(idx$idxS, foo, DATA = MONQ90[, South, drop = FALSE]))
			YEARQ90[, !South] <- transPose(sapply(idx$idxN, foo, DATA = MONQ90[, !South, drop = FALSE]))
		}
	}

	YEARQ10[ynacount > 15] <- NA
	YEARQ90[ynacount > 15] <- NA

	MONQ10 <- MONQ10 * 100/enddays
	MONQ90 <- MONQ90 * 100/enddays
	myear <- as.numeric(rownames(MONQ10))
	dimnames(MONQ10) <- NULL
	dimnames(MONQ90) <- NULL

	YEARQ10 <- YEARQ10 * 100/365
	YEARQ90 <- YEARQ90 * 100/365
	# yyear <- as.numeric(rownames(YEARQ10))
	dimnames(YEARQ10) <- NULL
	dimnames(YEARQ90) <- NULL

	trQ10 <- apply(YEARQ10, 2, climdex.getTrend.lsfit, x = yyear)
	trQ90 <- apply(YEARQ90, 2, climdex.getTrend.lsfit, x = yyear)

	if(MONTHQ1090$omitted){
		xx1 <- xx <- matrix(NA, nrow = nrow(MONQ10), ncol = length(MONTHQ1090$stnOut))
		xx[, !MONTHQ1090$stnOut] <- MONQ10
		xx1[, !MONTHQ1090$stnOut] <- MONQ90
		MONQ10 <- xx
		MONQ90 <- xx1

		xx1 <- xx <- matrix(NA, nrow = nrow(YEARQ10), ncol = length(MONTHQ1090$stnOut))
		xx[, !MONTHQ1090$stnOut] <- YEARQ10
		xx1[, !MONTHQ1090$stnOut] <- YEARQ90
		YEARQ10 <- xx
		YEARQ90 <- xx1

		xx1 <- xx <- matrix(NA, nrow = 4, ncol = length(MONTHQ1090$stnOut))
		xx[, !MONTHQ1090$stnOut] <- trQ10
		xx1[, !MONTHQ1090$stnOut] <- trQ90
		dimnames(xx1)[[1]] <- dimnames(xx)[[1]] <- dimnames(trQ10)[[1]]
		trQ10 <- xx
		trQ90 <- xx1
	}

	list(Tt10p = MONQ10, Tt90p = MONQ90, mon.date = myear,
		Ttg10p = YEARQ10, Ttg90p = YEARQ90, yrs.date = yyear,
		tr10p = trQ10, tr90p = trQ90)
}

climdex.WSDI.CSDI <- function(MONTHQ1090, Indice = 'WSDI'){
	if(is.null(MONTHQ1090$status)){
		InsertMessagesTxt(main.txt.out, "No temperature data found, can not calculate WSDI and CSDI", format = TRUE)
		return(NULL)
	}
	if(!MONTHQ1090$status){
		InsertMessagesTxt(main.txt.out, paste("More than", MONTHQ1090$maxpercNA,
							"% data missing, WSDI and CSDI will not be calculated!!"), format = TRUE)
		return(NULL)
	}

	Xyear <- MONTHQ1090$Xyear
	nbstn <- MONTHQ1090$nbstn
	cdtdata <- MONTHQ1090$cdtdata

	if(MONTHQ1090$combine){
		if(MONTHQ1090$omitted){
			NAoneday <- MONTHQ1090$stnOut
			stnOut <- c(rep('X', length(MONTHQ1090$nbTmax)), rep('N', length(MONTHQ1090$nbTmin)))[!NAoneday]
			ncTx <- which(stnOut == "X")
			ncTn <- which(stnOut == "N")
		}else{
			ncTx <- MONTHQ1090$nbTmax
			ncTn <- MONTHQ1090$nbTmin
		}

		if(length(ncTx) == 0){
			cWSDI <- FALSE
			InsertMessagesTxt(main.txt.out, paste("More than", MONTHQ1090$maxpercNA,
								"% data missing, WSDI will not be calculated!!"), format = TRUE)
		}else cWSDI <- TRUE

		if(length(ncTn) == 0){
			cCSDI <- FALSE
			InsertMessagesTxt(main.txt.out, paste("More than", MONTHQ1090$maxpercNA,
								"% data missing, CSDI will not be calculated!!"), format = TRUE)
		}else cCSDI <- TRUE
	}else{
		if(Indice == 'WSDI'){
			 ncTx <- 1:nbstn
			 cWSDI <- TRUE
			 cCSDI <- FALSE
		}
		if(Indice == 'CSDI'){
			ncTn <- 1:nbstn
			 cWSDI <- FALSE
			 cCSDI <- TRUE
		}
	}

	# yyear <- unique(Xyear)
	yyear <- MONTHQ1090$nacount$yyear

	wcSDI <- lapply(yyear, function(year){
		iirow <- if(is.leapyear(year)) c(1:59, 59, 60:365) else 1:365

		if(cWSDI){
			monQ90 <- MONTHQ1090$monQ90[iirow, ncTx, drop = FALSE]
			vTmax <- cdtdata$data[Xyear == year, ncTx, drop = FALSE]
			dTmax <- vTmax - monQ90
			WSDI <- apply(!is.na(dTmax) & dTmax > 0, 2, function(x){
				xx <- rle(x)
				sum(xx$lengths[xx$lengths >= 6 & xx$values])
			})
		}else WSDI <- NULL

		if(cCSDI){
			monQ10 <- MONTHQ1090$monQ10[iirow, ncTn, drop = FALSE]
			vTmin <- cdtdata$data[Xyear == year, ncTn, drop = FALSE]
			dTmin <- monQ10 - vTmin
			CSDI <- apply(!is.na(dTmin) & dTmin > 0, 2, function(x){
				xx <- rle(x)
				sum(xx$lengths[xx$lengths >= 6 & xx$values])
			})
		}else CSDI <- NULL

		list(WSDI = WSDI, CSDI = CSDI)
	})

	if(cWSDI){
		WSDI <- do.call(rbind, lapply(wcSDI, '[[', 1))
		WSDI[MONTHQ1090$nacount$ynacount[, ncTx, drop = FALSE] > 15] <- NA
		trWSDI <- apply(WSDI, 2, climdex.getTrend.lsfit, x = yyear)
		if(MONTHQ1090$omitted){
			ix <- if(MONTHQ1090$combine) MONTHQ1090$nbTmax else seq(length( MONTHQ1090$stnOut))
			xx <- matrix(NA, nrow = nrow(WSDI), ncol = length(ix))
			xx[, !MONTHQ1090$stnOut[ix]] <- WSDI
			WSDI <- xx
			xx <- matrix(NA, nrow = 4, ncol = length(ix))
			xx[, !MONTHQ1090$stnOut[ix]] <- trWSDI
			dimnames(xx)[[1]] <- dimnames(trWSDI)[[1]]
			trWSDI <- xx
		}
	}else{
		WSDI <- NULL
		trWSDI <- NULL
	}

	if(cCSDI){
		CSDI <- do.call(rbind, lapply(wcSDI, '[[', 2))
		CSDI[MONTHQ1090$nacount$ynacount[, ncTn, drop = FALSE] > 15] <- NA
		trCSDI <- apply(CSDI, 2, climdex.getTrend.lsfit, x = yyear)
		if(MONTHQ1090$omitted){
			ix <- if(MONTHQ1090$combine) MONTHQ1090$nbTmin else seq(length( MONTHQ1090$stnOut))
			xx <- matrix(NA, nrow = nrow(CSDI), ncol = length(ix))
			xx[, !MONTHQ1090$stnOut[ix]] <- CSDI
			CSDI <- xx
			xx <- matrix(NA, nrow = 4, ncol = length(ix))
			xx[, !MONTHQ1090$stnOut[ix]] <- trCSDI
			dimnames(xx)[[1]] <- dimnames(trCSDI)[[1]]
			trCSDI <- xx
		}
	}else{
		CSDI <- NULL
		trCSDI <- NULL
	}

	list(WSDI = WSDI, CSDI = CSDI, trWSDI = trWSDI, trCSDI = trCSDI, yrs.date = yyear)
}

################

climdex.GrowingSeasonL <- function(Xtmp, index, threshold, consec.day){
	GSL.position <- function(x){
		if(all(is.na(x$values))) return(NA)
		pos <- which(x$values == 1 & x$lengths >= consec.day)
		if(length(pos) == 0) return(0)
		if(pos[1] == 1) return(1)
		cumsum(x$lengths)[pos[1]-1] + 1
	}
	transPose <- if(ncol(Xtmp) > 1) t else as.matrix

	gsl <- transPose(sapply(index, function(ix){
		## start
		Xstart <- Xtmp[ix[[1]], , drop = FALSE]
		NAstart <- base::colSums(is.na(Xstart))
		Lstart <- nrow(Xstart)
		NC <- ncol(Xstart)

		Zstart <- Xstart
		Zstart[Xstart <= threshold] <- 0
		Zstart[Xstart > threshold] <- 1
		Zstart <- lapply(lapply(seq_len(NC), function(i) Zstart[, i]), rle)
		Ideb <- sapply(Zstart, GSL.position)

		## end
		Xend <- Xtmp[ix[[2]], , drop = FALSE]
		NAend <- base::colSums(is.na(Xend))
		Lend <- nrow(Xend)

		Zend <- Xend
		Zend[Xend < threshold] <- 1
		Zend[Xend >= threshold] <- 0
		Zend <- lapply(lapply(seq_len(NC), function(i) Zend[, i]), rle)
		Ifin <- sapply(Zend, GSL.position)

		out <- Ifin + Lstart - Ideb  ## + 1
		out[Ifin == 0 & !is.na(Ifin)] <- Lend + Lstart - Ideb[Ifin == 0 & !is.na(Ifin)]   ## + 1
		out[Ideb == 0] <- 0
		out[NAstart + NAend > 15] <- NA
		out
	}))
	return(gsl)
}

climdex.GSL <- function(cdtdata, threshold = 5, consec.day = 6){
	Xmois <- as.numeric(substr(cdtdata$dates, 5, 6))
	Xanne <- as.numeric(substr(cdtdata$dates, 1, 4))
	yRange <- range(Xanne)

	## south
	idS <- lapply(yRange[1]:(yRange[2]-1), function(an){
		iAn0 <- which(Xanne == an)
		iAn1 <- which(Xanne == an+1)
		list(iAn0[Xmois[iAn0] > 6], iAn1[Xmois[iAn1] < 7])
	})

	## north
	idN <- lapply(yRange[1]:yRange[2], function(an){
		iAn <- which(Xanne == an)
		list(iAn[Xmois[iAn] < 7], iAn[Xmois[iAn] > 6])
	})

	nbstn <- length(cdtdata$lat)
	South <- cdtdata$lat < 0
	nyear <- diff(yRange)+1
	res <- matrix(NA, ncol = nbstn, nrow = nyear)

	if(all(South)){
		res[-nyear, ] <- climdex.GrowingSeasonL(cdtdata$data, idS,
								threshold = threshold, consec.day = consec.day)
	}else if(all(!South)){
		res <- climdex.GrowingSeasonL(cdtdata$data, idN,
					threshold = threshold, consec.day = consec.day)
	}else{
		res[-nyear, South] <- climdex.GrowingSeasonL(cdtdata$data[, South, drop = FALSE], idS,
											threshold = threshold, consec.day = consec.day)
		res[, !South] <- climdex.GrowingSeasonL(cdtdata$data[, !South, drop = FALSE], idN,
											threshold = threshold, consec.day = consec.day)
	}

	trGSL <- apply(res, 2, climdex.getTrend.lsfit, x = yRange[1]:yRange[2])

	list(yIdx = list(date = yRange[1]:yRange[2], data = res), tIdx = trGSL)
}

climdex.DTR <- function(cdtdata, nacount, separe = FALSE){
	nbstn <- ncol(cdtdata$data)
	transPose <- if(nbstn > 1) t else as.matrix
	foo <- function(x, DATA){
		MAT <- DATA[x, , drop = FALSE]
		base::colMeans(MAT, na.rm = TRUE)
	}

	idx1 <- tapply(seq(nrow(cdtdata$data)), substr(cdtdata$dates, 1, 6), identity)
	mDTR <- transPose(sapply(idx1, foo, DATA = cdtdata$data))

	yearmon <- as.numeric(nacount$yearmon)
	yyear <- nacount$yyear

	mDTR[nacount$mnacount > 3] <- NA
	dimnames(mDTR) <- NULL

	if(!separe){
		idx2 <- tapply(seq(length(nacount$year)), nacount$year, identity)
		yDTR <- transPose(sapply(idx2, foo, DATA = mDTR))
	}else{
		idx2 <- climdex.indexSN(nacount$yearmon)
		nyear <- diff(range(as.numeric(substr(nacount$yearmon, 1, 4))))+1
		South <- cdtdata$lat < 0

		if(all(South)){
			yDTR <- transPose(sapply(idx2$idxS, foo, DATA = mDTR))
		}else if(all(!South)){
			yDTR <- transPose(sapply(idx2$idxN, foo, DATA = mDTR))
		}else{
			yDTR <- matrix(NA, ncol = nbstn, nrow = nyear)
			yDTR[-nyear, South] <- transPose(sapply(idx2$idxS, foo, DATA = mDTR[, South, drop = FALSE]))
			yDTR[, !South] <- transPose(sapply(idx2$idxN, foo, DATA = mDTR[, !South, drop = FALSE]))
		}
	}

	yDTR[nacount$ynacount > 15] <- NA
	dimnames(yDTR) <- NULL

	trDTR <- apply(yDTR, 2, climdex.getTrend.lsfit, x = yyear)

	list(mIdx = list(date = yearmon, data = mDTR),
		yIdx = list(date = yyear, data = yDTR),
		tIdx = trDTR)
}



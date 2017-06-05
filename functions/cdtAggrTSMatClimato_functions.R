

#################################################################################
## Climdex,  Extraction
## Index for sliding window, climatology

index.data.rollNormal <- function(dates, startyear, endyear, winsize){
	ixnorm <- as.numeric(substr(dates, 1, 4)) %in% startyear:endyear
	winh <- winsize/2
	if(winsize%%2 == 0){
		add1 <- winh-1
		add2 <- winh
	}else{
		add1 <- floor(winh)
		add2 <- floor(winh)
	}

	iinorm <- which(ixnorm)
	iadd1 <- if(add1 > 0) iinorm[1]-(add1:1) else iinorm[1]
	iadd2 <- if(add2 > 0) iinorm[length(iinorm)]+(1:add2) else iinorm[length(iinorm)]

	iadd1[iadd1 < 1] <- NA
	iadd2[iadd2 > length(ixnorm)] <- NA
	c(iadd1, iinorm, iadd2)
}

table.annuel <- function(){
	uneAnne <- as.character(seq(as.Date('2014-1-1'), by = 'day', length.out = 365))
	vtimes <- do.call(rbind, strsplit(uneAnne, '-'))[, 3:2]
	vtimes <- cbind(apply(vtimes, 2, as.numeric), 1:365)
	vtimes
}

index.data.rollClimato <- function(dates, startyear, endyear, winsize){
	ixnorm <- as.numeric(substr(dates, 1, 4)) %in% startyear:endyear
	winh <- winsize/2
	if(winsize%%2 == 0){
		add1 <- winh-1
		add2 <- winh
	}else{
		add1 <- floor(winh)
		add2 <- floor(winh)
	}

	iinorm <- which(ixnorm)
	iadd1 <- if(add1 > 0) iinorm[1]-(add1:1) else iinorm[1]
	iadd2 <- if(add2 > 0) iinorm[length(iinorm)]+(1:add2) else iinorm[length(iinorm)]

	iadd1[iadd1 < 1] <- NA
	iadd2[iadd2 > length(ixnorm)] <- NA
	ixnorm <- c(iadd1, iinorm, iadd2)

	dates0 <- dates[ixnorm]
	dates0 <- dates0[substr(dates0, 5, 8) != '0229']
	xdday <- as.numeric(substr(dates0, 7, 8))
	xdmon <- as.numeric(substr(dates0, 5, 6))

	vtimes <- table.annuel()
	xdaty <- paste(xdday, xdmon, sep = '_')
	xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
	times.stn <- vtimes[match(xdaty, xvtm), 3]

	ixdays <- lapply(1:365, function(nt){
		ix1 <- which(times.stn == nt)
		ix1 <- sapply(ix1, function(x) x+(-add1:add2))
		if(!is.matrix(ix1)) ix1 <- matrix(ix1, nrow = 1)
		ix1 <- c(ix1[, apply(ix1, 2, function(x) !any(x < 1 | x > length(xdday)))])
		ix1
	})
	ixdays
}

####################################
## aggregation Time series
## matrix aggregation

getIndexSeason <- function(dates, inTimestep, yearSeas, startMonth, seasonLength){
	if(inTimestep == 'monthly'){
		dtmp <- range(as.Date(paste0(dates, '01'), '%Y%m%d'), na.rm = TRUE)
		pastemps <- 'month'
	}else{
		dtmp <- range(as.Date(dates, '%Y%m%d'), na.rm = TRUE)
		pastemps <- 'day'
	}
	cdates <- seq(dtmp[1], dtmp[2], pastemps)
	ystart <- seq(as.Date(paste0(format(cdates[1], '%Y'), '-1-1')), cdates[1], pastemps)
	ystart <- ystart[-length(ystart)]
	yend <- seq(cdates[length(cdates)], as.Date(paste0(format(cdates[length(cdates)], '%Y'), '-12-31')), pastemps)
	yend <- yend[-1]
	if(length(ystart) > 0) cdates <- c(ystart, cdates)
	if(length(yend) > 0) cdates <- c(cdates, yend)
	if(inTimestep == 'daily') cdates <- format(cdates, "%Y%m%d")
	if(inTimestep == 'dekadal'){
		dek <- as.numeric(format(cdates, '%d'))
		cdates <- paste0(format(cdates, "%Y%m")[dek <= 3], dek[dek <= 3])
	}
	if(inTimestep == 'monthly') cdates <- format(cdates, "%Y%m")

	##################

	idrow <- seq(length(dates))
	idrow <- idrow[match(cdates, dates)]

	##################

	monthSeas <- (startMonth:(startMonth+(seasonLength-1)))%%12
	monthSeas[monthSeas == 0] <- 12

	##################

	itmp <- as.numeric(substr(cdates, 1, 4))%in%yearSeas & as.numeric(substr(cdates, 5, 6))%in%monthSeas
	xrow <- idrow[itmp]
	xdaty <- cdates[itmp]

	####################

	if(seasonLength == 1){
		indx <- tapply(xrow, substr(xdaty, 1, 6), identity)
		odaty <- names(indx)
		odaty <- paste0(substr(odaty, 1, 4), '-', str_pad(startMonth, width = 2, pad = "0"), '_',
						substr(odaty, 1, 4), '-', str_pad(startMonth, width = 2, pad = "0"))
		len.data <- sapply(indx, length)
	}else{
		xmois <- as.numeric(substr(xdaty, 5, 6))

		rleMois <- rle(xmois)
		xrank <- cumsum(rleMois$lengths)
		istart <- seq(which(rleMois$values %in% monthSeas[1])[1], length(rleMois$values), seasonLength)
		iend <- istart + seasonLength - 1

		istart <- xrank[istart]-rleMois$lengths[istart]+1

		debSeas <- as.Date(paste0(substr(xdaty[istart], 1, 6), '01'), '%Y%m%d')
		finSeas <- sapply(lapply(debSeas, addMonths, n = seasonLength-1), format, '%Y-%m')

		## end
		nend <- 0
		if(iend[length(iend)] > length(rleMois$lengths)){
			iex <- iend[length(iend)] - length(rleMois$lengths)
			iex <- (rleMois$values[length(rleMois$lengths)] + (1:iex))%%12
			iex[iex == 0] <- 12

			if(inTimestep == 'daily'){
				nend <- mapply(function(an, mon) rev((28:31)[!is.na(as.Date(paste(an, mon, 28:31, sep = '-')))])[1],
										rep(as.numeric(substr(finSeas[length(finSeas)], 1, 4)), length(iex)), iex)
				nend <- sum(nend)
			}
			if(inTimestep == 'dekadal') nend <- sum(length(iex))*3
			if(inTimestep == 'monthly') nend <- sum(length(iex))

			iend[length(iend)] <- length(rleMois$lengths)
		}
		iend <- xrank[iend]

		## index
		indx <- lapply(seq_along(istart), function(j) xrow[istart[j]:iend[j]])

		odaty <- paste0(format(debSeas, '%Y-%m'), '_', finSeas)
		len.data <- sapply(indx, length)
		len.data[length(len.data)] <- len.data[length(len.data)] + nend
	}

	list(index = indx, dates = odaty, full.len = len.data)
}


getIndexSeasonVars <- function(startSeas, endSeas, dates, inTimestep){
	idx <- vector(mode = "list", length = length(startSeas))
	ina <- is.na(startSeas) | is.na(endSeas)
	idx[ina] <- NA
	startSeas <- startSeas[!ina]
	endSeas <- endSeas[!ina]

	if(inTimestep == "daily"){
		start.seas <- as.numeric(as.character(startSeas))
		end.seas <- as.numeric(as.character(endSeas))
	}
	if(inTimestep == "monthly"){
		start.seas <- as.numeric(substr(startSeas, 1, 6))
		end.seas <- as.numeric(substr(endSeas, 1, 6))
	}
	if(inTimestep == "dekadal"){
		start.seas1 <- as.numeric(substr(startSeas, 1, 6))
		end.seas1 <- as.numeric(substr(endSeas, 1, 6))
		start.seas2 <- as.numeric(substr(startSeas, 7, 8))
		end.seas2 <- as.numeric(substr(endSeas, 7, 8))
		start.seas2 <- ifelse(start.seas2 <= 10, 1, ifelse(start.seas2 > 20, 3, 2))
		end.seas2 <- ifelse(end.seas2 <= 10, 1, ifelse(end.seas2 > 20, 3, 2))
		start.seas <- as.numeric(paste0(start.seas1, start.seas2))
		end.seas <- as.numeric(paste0(end.seas1, end.seas2))
	}

	##################

	if(inTimestep == 'monthly'){
		dtmp <- range(as.Date(paste0(dates, '01'), '%Y%m%d'), na.rm = TRUE)
		pastemps <- 'month'
	}else{
		dtmp <- range(as.Date(dates, '%Y%m%d'), na.rm = TRUE)
		pastemps <- 'day'
	}
	cdates <- seq(dtmp[1], dtmp[2], pastemps)
	ystart <- seq(as.Date(paste0(format(cdates[1], '%Y'), '-1-1')), cdates[1], pastemps)
	ystart <- ystart[-length(ystart)]
	yend <- seq(cdates[length(cdates)], as.Date(paste0(format(cdates[length(cdates)], '%Y'), '-12-31')), pastemps)
	yend <- yend[-1]
	if(length(ystart) > 0) cdates <- c(ystart, cdates)
	if(length(yend) > 0) cdates <- c(cdates, yend)
	if(inTimestep == 'daily') cdates <- format(cdates, "%Y%m%d")
	if(inTimestep == 'dekadal'){
		dek <- as.numeric(format(cdates, '%d'))
		cdates <- paste0(format(cdates, "%Y%m")[dek <= 3], dek[dek <= 3])
	}
	if(inTimestep == 'monthly') cdates <- format(cdates, "%Y%m")

	idrow <- seq(length(dates))
	idrow <- idrow[match(cdates, dates)]

	##################

	cdates <- as.numeric(as.character(cdates))
	istart <- which(cdates%in%start.seas)
	iend <- which(cdates%in%end.seas)
	# idrow <- lapply(seq_along(istart), function(j) idrow[istart[j]:iend[j]])
	idrow <- lapply(seq_along(istart), function(j) {
		if(!is.na(iend[j])) idrow[istart[j]:iend[j]] else NA
	})
	idx[!ina] <- idrow
	return(idx)
}

###############################################

### NA count
funMissMAT <- function(x, DATA){
	MAT <- is.na(DATA[x, , drop = FALSE])
	base::colSums(MAT)
}

### Aggregation
funAggrMAT <- function(x, DATA, pars){
	x <- x[!is.na(x)]
	if(length(x) == 0) return(rep(NA, ncol(DATA)))
	MAT <- DATA[x, , drop = FALSE]
	if(pars$aggr.fun == "max") res <- matrixStats::colMaxs(MAT, na.rm = TRUE)
	if(pars$aggr.fun == "min") res <- matrixStats::colMins(MAT, na.rm = TRUE)
	if(pars$aggr.fun == "mean") res <- base::colMeans(MAT, na.rm = TRUE)
	if(pars$aggr.fun == "sum") res <- base::colSums(MAT, na.rm = TRUE)
	if(pars$aggr.fun == "count"){
		count.fun <- match.fun(pars$count.fun)
		MAT <- count.fun(MAT, pars$count.thres) & !is.na(MAT)
		res <- base::colSums(MAT, na.rm = TRUE)
	}
	return(res)
}

###############################################
### DRY & WET SPELL
# Number of dry spells
NumberOfSpell <- function(ix, DATA, opr.fun = "<", opr.thres = 1, ccday = 5){
	opr.fun <- match.fun(opr.fun)
	if(length(ix) == 1) if(is.na(ix)) return(NA)
	MAT <- DATA[ix, , drop = FALSE]
	MAT <- opr.fun(MAT, opr.thres) & !is.na(MAT)
	apply(MAT, 2, function(x){
		if(!any(x)) return(0)
		xx <- rle(x)
		length(xx$lengths[xx$values & xx$lengths >= ccday])
	})
}


NumberOfSpell.All <- function(ix, DATA, opr.fun = "<", opr.thres = 1, ccday = 1:20){
	opr.fun <- match.fun(opr.fun)
	if(length(ix) == 1) if(is.na(ix)) return(NA)
	MAT <- DATA[ix, , drop = FALSE]
	MAT <- opr.fun(MAT, opr.thres) & !is.na(MAT)
	apply(MAT, 2, function(x){
		if(!any(x)) return(rep(0, nrow = length(ccday)))
		xx <- rle(x)
		res <- rep(0, nrow = length(ccday))
		for(i in seq_along(ccday)) res[i] <- length(xx$lengths[xx$values & xx$lengths >= ccday[i]])
		res
	})
}

NumberOfSpell.All1 <- function(ix, DATA, opr.fun = "<", opr.thres = 1){
	opr.fun <- match.fun(opr.fun)
	if(length(ix) == 1) if(is.na(ix)) return(NA)
	MAT <- DATA[ix, , drop = FALSE]
	MAT <- opr.fun(MAT, opr.thres) & !is.na(MAT)
	lapply(seq(ncol(MAT)), function(i){
		x <- MAT[, i]
		if(!any(x)) return(0)
		xx <- rle(x)
		xx$lengths[xx$values]
	})
}


# Maximum length of dry spell, maximum number of consecutive days with RR < 1mm
# Maximum length of wet spell, maximum number of consecutive days with RR ≥ 1mm
MaximumLengthOfSpell <- function(ix, DATA, opr.fun = "<", opr.thres = 1){
	opr.fun <- match.fun(opr.fun)
	if(length(ix) == 1) if(is.na(ix)) return(NA)
	MAT <- DATA[ix, , drop = FALSE]
	MAT <- opr.fun(MAT, opr.thres) & !is.na(MAT)
	apply(MAT, 2, function(x){
		if(!any(x)) return(0)
		xx <- rle(x)
		max(xx$lengths[xx$values])
	})
}


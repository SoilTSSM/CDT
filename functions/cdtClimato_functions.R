

########################################### aggregateSeries wrapper ###########################################
# time.step.Out = 'dekadal', 'monthly', 'yearly'
#for seasonal time.step.Out = c(start.month, season.length), c(7, 3) for JAS
#for seasonal time.step.Out = season.length
#data structure: type = 'vector', 'matrix', 'array', 'list'

aggregateSeries <- function(xvar, dates, fun = 'sum', ..., min.frac = 1.0,
							time.step.In = 'daily', time.step.Out = 'monthly', type = 'vector')
{
	dates.bak <- dates <- as.character(dates)

	if(time.step.In == 'daily'){
		dates <- as.Date(dates, format='%Y%m%d')
		iNAdate <- is.na(dates)
		wrong.dates <- dates.bak[iNAdate]
		dates <- dates[!iNAdate]
		iDUPdate <- duplicated(dates) | duplicated(dates,fromLast=TRUE)
		dup.dates <- dates[iDUPdate]
		dup.dates <- format(dup.dates, '%Y%m%d')
		iDUPdate1 <- duplicated(dates)
		dates <- dates[!iDUPdate1]
		alldates <- seq(min(dates), max(dates), 'day')
		iMISSdate <- match(alldates,dates)
		dates <- format(alldates, '%Y%m%d')
		miss.dates <- alldates[is.na(iMISSdate)]
		miss.dates <- format(miss.dates, '%Y%m%d')
	}
	if(time.step.In == 'dekadal'){
		xan <- substr(dates, 1,4)
		xmo <- substr(dates, 5,6)
		xdk <- substr(dates, 7,7)
		notdek <- which(as.numeric(xdk) > 3)
		dates <- as.Date(paste(xan, xmo, xdk, sep = '-'))
		dates[notdek] <- NA
		iNAdate <- is.na(dates)
		wrong.dates <- dates.bak[iNAdate]
		dates <- dates[!iNAdate]
		iDUPdate <- duplicated(dates) | duplicated(dates,fromLast=TRUE)
		dup.dates <- dates[iDUPdate]
		dup.dates <- paste(format(dup.dates, '%Y%m'), as.numeric(format(dup.dates, '%d')), sep = '')
		iDUPdate1 <- duplicated(dates)
		dates <- dates[!iDUPdate1]
		alldates <- seq(min(dates), max(dates), 'day')
		alldates <- alldates[as.numeric(format(alldates, '%d')) <= 3]
		iMISSdate <- match(alldates,dates)
		dates <- paste(format(alldates, '%Y%m'), as.numeric(format(alldates, '%d')), sep = '')
		miss.dates <- alldates[is.na(iMISSdate)]
		miss.dates <- paste(format(miss.dates, '%Y%m'), as.numeric(format(miss.dates, '%d')), sep = '')
	}
	if(time.step.In == 'monthly'){
		xan <- substr(dates, 1,4)
		xmo <- substr(dates, 5,6)
		dates <- as.Date(paste(xan, xmo, '1', sep = '-'))
		iNAdate <- is.na(dates)
		wrong.dates <- dates.bak[iNAdate]
		dates <- dates[!iNAdate]
		iDUPdate <- duplicated(dates) | duplicated(dates,fromLast=TRUE)
		dup.dates <- dates[iDUPdate]
		dup.dates <- format(dup.dates, '%Y%m')
		iDUPdate1 <- duplicated(dates)
		dates <- dates[!iDUPdate1]
		alldates <- seq(min(dates), max(dates), 'month')
		iMISSdate <- match(alldates,dates)
		dates <- format(alldates, '%Y%m')
		miss.dates <- alldates[is.na(iMISSdate)]
		miss.dates <- format(miss.dates, '%Y%m')
	}

	if(length(time.step.Out) == 1){
		if(time.step.Out %in% c('dekadal', 'monthly', 'yearly')){
			index <- index_DayDekMon(time.step.In, time.step.Out, dates)
			funAggr <- DayDekMon2DekMonYear
		}else if(is.numeric(time.step.Out)){
			index <- index_Season1(time.step.In, time.step.Out, dates)
			funAggr <- DayDekMon2Season1
		}else{
			stop("Unknown output format")
		}
	}else if(is.numeric(time.step.Out) & length(time.step.Out) == 2){
		index <- index_Season(time.step.In, time.step.Out, dates)
		funAggr <- DayDekMon2Season
	}else{
		stop("Unknown output format")
	}

	if(type == 'vector'){
		if(!is.vector(xvar , mode = 'numeric')) stop("Input data must be a numeric vector")
		xvar0 <- xvar[iNAdate]
		xvar1 <- xvar[iDUPdate]
		xvar <- xvar[!iNAdate]
		xvar <- xvar[!iDUPdate1]
		xvar <- xvar[iMISSdate]
		xout <- funAggr(xvar, index = index, fun = fun, ..., min.frac = min.frac)
	}else if(type == 'matrix'){
		if(!is.matrix(xvar)) stop("Input data must be a matrix")
		xvar0 <- xvar[iNAdate, , drop = FALSE]
		xvar1 <- xvar[iDUPdate, , drop = FALSE]
		xvar <- xvar[!iNAdate, , drop = FALSE]
		xvar <- xvar[!iDUPdate1, , drop = FALSE]
		xvar <- xvar[iMISSdate, , drop = FALSE]
		xout <- apply(xvar, 2, funAggr, index = index, fun = fun, ..., min.frac = min.frac)
	}else if(type == 'array'){
		if(!is.array(xvar)) stop("Input data must be an 3d array")
		xvar0 <- xvar[, , iNAdate, drop = FALSE]
		xvar1 <- xvar[, , iDUPdate, drop = FALSE]
		xvar <- xvar[, , !iNAdate, drop = FALSE]
		xvar <- xvar[, , !iDUPdate1, drop = FALSE]
		xvar <- xvar[, , iMISSdate, drop = FALSE]
		xout <- aperm(apply(xvar, 1:2, funAggr, index = index, fun = fun, ..., min.frac = min.frac), c(2, 3, 1))
	}else if(type == 'list'){
		if(!is.vector(xvar , mode = 'list')) stop("Input data must be a list of matrix")
		xvar0 <- xvar[iNAdate]
		xvar1 <- xvar[iDUPdate]
		ix <- sapply(xvar,is.null)
		xvar[ix] <- list(matrix(NA, nrow = dim(xvar[!ix][[1]])[1], ncol = dim(xvar[!ix][[1]])[2]))
		xvar <- simplify2array(xvar)
		xvar <- xvar[, , !iNAdate, drop = FALSE]
		xvar <- xvar[, , !iDUPdate1, drop = FALSE]
		xvar <- xvar[, , iMISSdate, drop = FALSE]
		xout <- aperm(apply(xvar, 1:2, funAggr, index = index, fun = fun, ..., min.frac = min.frac), c(2, 3, 1))
		xout <- lapply(seq(dim(xout)[3]), function(x) xout[, , x])
	}else{
		stop("Unknown input data type")
	}
	
	out <- list(date = index$date, data = xout)
	wrong.dates <- if(any(iNAdate)) list(date = wrong.dates, data = xvar0) else NULL
	duplicated.dates <-  if(any(iDUPdate)) list(date = dup.dates, data = xvar1) else NULL
	missing.dates <-  if(any(is.na(iMISSdate))) list(date = miss.dates) else NULL
	ret <- list(out = out, wrong.dates = wrong.dates, duplicated.dates = duplicated.dates, missing.dates = missing.dates)
	return(ret)
}

########################################### Climatologies wrapper ###########################################
#time.step = 'daily', 'dekadal', 'monthly'
#data structure: type =  'vector', 'matrix', 'array', 'list'

Climatologies <- function(xvar, dates, fun = 'mean', ..., time.step = 'daily', type ='vector',
							clim.range = NULL, clim.nbyear.min = NA)
{
	if(time.step == 'daily'){
		dates <- paste('Day', 1:365, sep = '-')
	}else if(time.step == 'dekadal'){
		dates <- paste('Dekad', 1:36, sep = '-')
	}else if(time.step == 'monthly'){
		dates <- format(ISOdate(2014,1:12,1), "%b")
	}else{
		stop('Unknown time step input')
	}
	if(!type %in% c('vector', 'matrix', 'array', 'list')) stop('Unknown data type input')
	funClim <- paste('Climatologies.', time.step, '_', type, sep = '')
	funClim <- match.fun(funClim)
	xout <- funClim(xvar, dates, fun = fun, ..., clim.range = clim.range, clim.nbyear.min = clim.nbyear.min)
	return(list(date = dates, data = xout))
}

Anomalies <- function(xvar, dates, time.step = 'daily', type ='vector',
						clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL)
{
	if(!time.step %in% c('daily', 'dekadal', 'monthly')) stop('Unknown time step input')
	if(!type %in% c('vector', 'matrix', 'array', 'list')) stop('Unknown data type input')
	funClim <- paste('Anomalies.', time.step, '_', type, sep = '')
	funClim <- match.fun(funClim)
	xout <- funClim(xvar, dates, clim.range = clim.range, clim.nbyear.min = clim.nbyear.min, clim.mean = clim.mean)
	return(list(date = dates, data = xout))
}

StandardizedAnomalies <- function(xvar, dates, time.step = 'daily', type ='vector',
									clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL, clim.sd = NULL)
{
	if(!time.step %in% c('daily', 'dekadal', 'monthly')) stop('Unknown time step input')
	if(!type %in% c('vector', 'matrix', 'array', 'list')) stop('Unknown data type input')
	funClim <- paste('StandardizedAnomalies.', time.step, '_', type, sep = '')
	funClim <- match.fun(funClim)
	xout <- funClim(xvar, dates, clim.range = clim.range, clim.nbyear.min = clim.nbyear.min, clim.mean = clim.mean, clim.sd = clim.sd)
	return(list(date = dates, data = xout))	
}



################################### AGGREGATE TS FUNCTIONS ######################################################

## fun: sum, mean, sd, var, median, min, max, user defined function
DayDekMon2DekMonYear <- function(x, index, fun, ..., min.frac = 1.0){
	fun <- match.fun(fun)
	nPeriod <- tapply(x, index$index, function(j) length(which(!is.na(j))))/index$nbOfPeriod
	xval <- tapply(x, index$index, fun, ...)
	xval[nPeriod < min.frac] <- NA
	unname(xval)
}

DayDekMon2Season <- function(x, index, fun, ..., min.frac = 1.0){
	fun <- match.fun(fun)
	nPeriod <- tapply(x[index$id.season], index$index, function(j) length(which(!is.na(j))))/index$nbOfPeriod
	xval <- tapply(x[index$id.season], index$index, fun, ...)
	xval[nPeriod < min.frac] <- NA
	unname(xval)
}

DayDekMon2Season1 <- function(x, index, fun, ..., min.frac = 1.0){
	fun <- match.fun(fun)
	nPeriod <- sapply(index$index, function(i) length(which(!is.na(x[i]))))/index$nbOfPeriod
	xval <- sapply(index$index, function(i) fun(x[i], ...))
	xval[nPeriod < min.frac] <- NA
	xval
}

#################################

nbDayOfDekad <- function(dates){
	year <- substr(dates, 1, 4)
	mon <- substr(dates, 5, 6)
	day <- as.numeric(substr(dates, 7, 8))
	ifelse(day <= 10, 10, ifelse(day > 10 & day <= 20, 10, rev((28:31)[!is.na(as.Date(paste(year, mon, 28:31, sep='-')))])[1] - 20))
}

nbDayOfMonth <- function(dates){
	year <- substr(dates, 1, 4)
	mon <- substr(dates, 5, 6)
	rev((28:31)[!is.na(as.Date(paste(year, mon, 28:31, sep = '-')))])[1]
}

nbDayOfYear <- function(dates){
	year <- as.numeric(substr(dates, 1, 4))
	if(year%%4 > 0) 365 else 366
}

cycleMonth1 <- function(start, n){
	mois <- format(ISOdate(2014,1:12,1), "%b")
	ix <- which(mois == start)
	im<-(ix+(n-1))%%12
	if(im == 0) im <- 12
	return(mois[im])
}

add.months <- function(daty, n = 1){
	date0 <- seq(daty, by = paste(n, "months"), length = 2)[2]
	date1 <- seq(as.Date(paste(format(daty,'%Y-%m'),'01', sep = '-')), by = paste(n+1, "months"), length = 2)[2]-1
	daty <- if(date0 > date1) date1 else date0
	return(daty)
}

add.dekads <- function(daty, n = 1){
	idek <- as.numeric(substr(format(daty,'%Y%m%d'), 8,8))+n
	dek <- idek%%3
	if(dek == 0) dek <- 3
	daty <- format(add.months(daty, floor((idek-1)/3)),'%Y-%m')
	daty <- as.Date(paste(daty, dek, sep = '-'))
	return(daty)
}


#################################

index_DayDekMon <- function(freqIn, freqOut = 'monthly', dates){
	if(freqOut == 'dekadal'){
		yymm <- substr(dates, 1, 6)
		jour <- as.numeric(substr(dates, 7, 8))
		jour[jour <= 10] <- 1
		jour[jour > 10 & jour <= 20] <- 2
		jour[jour > 20] <- 3
		indx <- paste(yymm, jour, sep = '')
	}
	if(freqOut == 'monthly'){
		indx <- substr(dates, 1, 6)
	}
	if(freqOut == 'yearly'){
		indx <- substr(dates, 1, 4)
	}
	nbd <- tapply(rep(1, length(dates)), indx, sum)
	if(freqIn == 'daily'){
		if(freqOut == 'dekadal'){
			nbd[1] <- nbDayOfDekad(dates[1])
			nbd[length(nbd)] <- nbDayOfDekad(dates[length(dates)])
		}
		if(freqOut == 'monthly'){
			nbd[1] <- nbDayOfMonth(dates[1])
			nbd[length(nbd)] <- nbDayOfMonth(dates[length(dates)])
		}
		if(freqOut == 'yearly'){
			nbd[1] <- nbDayOfYear(dates[1])
			nbd[length(nbd)] <- nbDayOfYear(dates[length(dates)])
		}
	}
	if(freqIn == 'dekadal'){
		if(freqOut == 'monthly') nbd[c(1, length(nbd))] <- 3
		if(freqOut == 'yearly')	nbd[c(1, length(nbd))] <- 36
	}
	if(freqIn == 'monthly') nbd[c(1, length(nbd))] <- 12
	list(index = indx, nbOfPeriod = unname(nbd), date = as.numeric(names(nbd)))
}

index_Season <- function(freqIn, freqOut = c(1, 3), dates){
	startMonth <- freqOut[1]
	seasonLength <- freqOut[2]
	annee <- substr(dates,1,4)
	mois <- substr(dates, 5, 6)
	month12 <- format(ISOdate(2014,1:12,1), "%b")
	if(seasonLength < 12){
		whichMon <- (startMonth:(startMonth+(seasonLength-1)))%%12
		whichMon[whichMon == 0] <- 12
		season <- paste(substr(month12, 1, 1)[whichMon], collapse='')
		whichMon <- ifelse(whichMon < 10, paste(0, whichMon, sep = ''), as.character(whichMon))
		id.season <- mois%in%whichMon
		alt.season <- rle(id.season)
		nbd <- alt.season$lengths[alt.season$values]
		nbd0 <- nbd
		if(freqIn == 'daily') nbd[c(1, length(nbd))] <- sum(sapply(whichMon, function(x) rev((28:31)[!is.na(as.Date(paste(2014, x, 28:31, sep = '-')))])[1]))
		if(freqIn == 'dekadal') nbd[c(1, length(nbd))] <- 3*seasonLength
		if(freqIn == 'monthly') nbd[c(1, length(nbd))] <- seasonLength
		year <- unique(annee[id.season])
		if(length(nbd) > length(year)){
			year <- c(as.numeric(year[1])-1, year)
		}else{
			sYear <- match(rle(mois)$values,whichMon)
			if(sYear[!is.na(sYear)][1] != 1){
				year <- c(as.numeric(year[1])-1, year[-length(year)])
			}
		}
		xdates <- paste(season, year, sep = '-')
		indx <- rep(xdates, times = nbd0)
	}else{
		startMonth[startMonth < 10] <- paste(0, startMonth, sep = '')
		id.mois <- rle(mois)
		idx <- seq(which(id.mois$values%in%startMonth)[1],length(id.mois$values),seasonLength)
		istart <- cumsum(id.mois$lengths)
		istart <- istart-istart[1]+1
		istart <- istart[idx]
		iend <- c(istart[-1]-1, length(dates))
		nbd <- iend-istart+1
		nbd0 <- nbd
		nbd[length(nbd)] <- nbd[length(nbd)-1]
		id.season <- rep(FALSE, length(dates))
		id.season[istart[1]:length(id.season)] <- TRUE
		xndx <- paste(paste(month12[startMonth], seasonLength, sep=''), annee[istart], sep = '-')
		indx <- rep(xndx, times = nbd0)
		## add 4 pour sauter feb28 ou june30...
		if(freqIn == 'daily') sdaty <- paste(month12[as.numeric(mois[istart+4])], annee[istart+4], sep = '')
		else sdaty <- paste(month12[as.numeric(mois[istart])], annee[istart], sep = '')
		edaty <- paste(month12[as.numeric(mois[iend[-length(iend)]])], annee[iend[-length(iend)]], sep = '')
		edaty <- c(edaty, format(add.months(as.Date(paste('01', edaty[length(edaty)], sep = ''), '%d%b%Y'),seasonLength), '%b%Y'))
		xdates <- paste(sdaty, edaty, sep = '-')
	}
	list(index = indx, nbOfPeriod = nbd, date = xdates, id.season =  id.season)
}

index_Season1 <- function(freqIn, freqOut = 3, dates){
	seasonLength <- freqOut
	month12 <- format(ISOdate(2014,1:12,1), "%b")
	dmon <- substr(dates, 1, 6)
	roll.mon <- rle(dmon)
	ij <- 1:length(roll.mon$values)
	ij <- ij[ij   <=  (length(ij)-seasonLength+1)]
	ij <- lapply(ij, function(i) i:(i + seasonLength - 1))
	nbd <- sapply(ij, function(i) sum(roll.mon$lengths[i]))

	if(freqIn == 'daily'){
		ix1 <- ij[[1]]
		nb1 <- roll.mon$lengths[ix1]
		x1 <- substr(roll.mon$values[ix1[1]], 5, 6)
		nb1[1] <- rev((28:31)[!is.na(as.Date(paste(2014, x1, 28:31, sep = '-')))])[1]
		nbd[1] <- sum(nb1)

		ix2 <- ij[[length(ij)]]
		nb2 <- roll.mon$lengths[ix2]
		x2 <- substr(roll.mon$values[ix2[seasonLength]], 5, 6)
		nb2[seasonLength] <- rev((28:31)[!is.na(as.Date(paste(2014, x2, 28:31, sep = '-')))])[1]
		nbd[length(nbd)] <- sum(nb2)
	}
	if(freqIn == 'dekadal') nbd[c(1, length(nbd))] <- 3*seasonLength
	if(freqIn == 'monthly') nbd[c(1, length(nbd))] <- seasonLength
	indx <- lapply(ij, function(j) which(dmon %in% roll.mon$values[j]))
	xdates <- sapply(ij, function(j){
			paste(paste(substr(month12[as.numeric(substr(roll.mon$values[j], 5, 6))], 1, 1), collapse = ''),
			substr(roll.mon$values[j], 1, 4)[1], sep = '-')
		})
	list(index = indx, nbOfPeriod = nbd, date = xdates)
}

######################################################  CLIMATO MONTHLY  ########################################################

### Climatologies

## Clim vector
Climatologies.monthly_vector <- function(xvar, dates, fun = 'mean', ..., clim.range = NULL, clim.nbyear.min = NA){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]

	xvar <- matrix(as.numeric(c(rep(NA, slen), xvar, rep(NA, elen))), nrow = 12)

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- unique(as.numeric(substr(dates, 1,4)))
		vtmp.clim <- xvar[, an >= clim.range[1] & an <= clim.range[2], drop = FALSE]
	}

	fun <- match.fun(fun)
	xvar <- apply(vtmp.clim, 1, fun, ...)
	if(!is.na(clim.nbyear.min)){
		ValToNA <- apply(vtmp.clim, 1, function(j) length(which(!is.na(j)))) < clim.nbyear.min
		xvar[ValToNA] <- NA
	}
	rm(dates, vtmp.clim)
	return(xvar)
}

## Clim matrix
Climatologies.monthly_matrix <- function(xvar, dates, fun = 'mean', ..., clim.range = NULL, clim.nbyear.min = NA){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]

	vmat1 <- matrix(NA, ncol = ncol(xvar), nrow = slen)
	vmat2 <- matrix(NA, ncol = ncol(xvar), nrow = elen)
	xvar <- rbind(vmat1, xvar, vmat2)

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(substr(dates, 1,4))
		vtmp.clim <- xvar[an >= clim.range[1] & an <= clim.range[2], , drop = FALSE]
	}
	
	fun <- match.fun(fun)
	vtmp.clim <- array(vtmp.clim, c(12, nrow(vtmp.clim)/12, ncol(vtmp.clim)))
	xvar <- apply(vtmp.clim, c(1,3), fun, ...)
	if(!is.na(clim.nbyear.min)){
		ValToNA <- apply(vtmp.clim, c(1,3), function(j) length(which(!is.na(j)))) < clim.nbyear.min
		xvar[ValToNA] <- NA
	}
	rm(vmat1, vmat2, vtmp.clim, dates)
	return(xvar)
}

## Clim array
Climatologies.monthly_array <- function(xvar, dates, fun = 'mean', ..., clim.range = NULL, clim.nbyear.min = NA){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]

	vmat1 <- array(NA, dim = c(dim(xvar[, , 1]), slen))
	vmat2 <- array(NA, dim = c(dim(xvar[, , 1]), elen))
	xvar <- array(c(vmat1, xvar, vmat2), dim = c(dim(xvar)[1:2], (dim(vmat1)+dim(xvar)+dim(vmat2))[3]))

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(substr(dates, 1,4))
		vtmp.clim <- xvar[, , an >= clim.range[1] & an <= clim.range[2], drop = FALSE]
	}

	fun <- match.fun(fun)
	xvar <- sapply(1:12, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 12), drop = FALSE], 1:2, fun, ...), simplify = "array")
	if(!is.na(clim.nbyear.min)){
		ValToNA <- sapply(1:12, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 12), drop = FALSE], 1:2, function(j) length(which(!is.na(j)))), simplify = "array")  < clim.nbyear.min
		xvar[ValToNA] <- NA
	}
	rm(vtmp.clim, dates, vmat1, vmat2)
	gc()
	return(xvar)
}

## Clim list
Climatologies.monthly_list <- function(xvar, dates, fun = 'mean', ..., clim.range = NULL, clim.nbyear.min = NA){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]

	vmat <- xvar[[1]]
	vmat[] <- NA
	vmat1 <- vector(mode = 'list', length = slen)
	vmat2 <- vector(mode = 'list', length = elen)
	xvar <- c(lapply(vmat1, function(x) vmat), xvar, lapply(vmat2, function(x) vmat))

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(substr(dates, 1,4))
		vtmp.clim <- xvar[an >= clim.range[1] & an <= clim.range[2]]
	}

	fun <- match.fun(fun)
	xvar <- lapply(1:12, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 12)]), 1:2, fun, ...))

	if(!is.na(clim.nbyear.min)){
		ValToNA <-  lapply(1:12, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 12)]), 1:2, function(j) length(which(!is.na(j)))) < clim.nbyear.min)  
		xvar <- lapply(1:length(xvar), function(j){
			x <- xvar[[j]]
			toNA <- ValToNA[[j]]
			x[toNA] <- NA
			x
		})
	}
	rm(vtmp.clim,  dates, vmat, vmat1, vmat2)
	gc()
	return(xvar)
}

###############################################################

### Standardized Anomalies

## stAnom vector
StandardizedAnomalies.monthly_vector <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL, clim.sd = NULL){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]

	xvar <- matrix(as.numeric(c(rep(NA, slen), xvar, rep(NA, elen))), nrow = 12)
	
	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- unique(as.numeric(substr(dates, 1,4)))
		vtmp.clim <- xvar[, an >= clim.range[1] & an <= clim.range[2], drop = FALSE]
	}

	if(!is.na(clim.nbyear.min)){
		ValToNA <- apply(vtmp.clim, 1, function(j) length(which(!is.na(j)))) < clim.nbyear.min
	}
		
	if(is.null(clim.mean)){
		cmoy <- apply(vtmp.clim, 1, mean, na.rm = TRUE)
		if(!is.na(clim.nbyear.min)) cmoy[ValToNA] <- NA
	}else{
		if(!is.vector(clim.mean , mode = 'numeric')) stop("Climatological values must be a numeric vector")
		if(length(clim.mean) != 12) stop("Climatological values must have 12 months length")
		cmoy <- clim.mean
	}
	if(is.null(clim.sd)){
		csd <- apply(vtmp.clim, 1, sd, na.rm = TRUE)
		if(!is.na(clim.nbyear.min)) csd[ValToNA] <- NA
	}else{
		if(!is.vector(clim.sd , mode = 'numeric')) stop("Climatological standard deviation must be a numeric vector")
		if(length(clim.sd) != 12) stop("Climatological standard deviation must have 12 months length")
		csd <- clim.sd
	}

	xvar <- c((xvar-cmoy)/csd)
	
	if(elen > 0) xvar <- head(xvar, -elen)
	if(slen > 0) xvar <- tail(xvar, -slen)
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(cmoy, csd, dates, vtmp.clim)
	return(xvar)
}

## stAnom matrix
StandardizedAnomalies.monthly_matrix <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL, clim.sd = NULL){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]

	vmat1 <- matrix(NA, ncol = ncol(xvar), nrow = slen)
	vmat2 <- matrix(NA, ncol = ncol(xvar), nrow = elen)
	xvar <- rbind(vmat1, xvar, vmat2)

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(substr(dates, 1,4))
		vtmp.clim <- xvar[an >= clim.range[1] & an <= clim.range[2], , drop = FALSE]
	}
	vtmp.clim <- array(vtmp.clim, c(12, nrow(vtmp.clim)/12, ncol(vtmp.clim)))

	if(!is.na(clim.nbyear.min)){
		ValToNA <- apply(vtmp.clim, c(1,3), function(j) length(which(!is.na(j)))) < clim.nbyear.min
	}
	
	if(is.null(clim.mean)){
		cmoy <- apply(vtmp.clim, c(1,3), mean, na.rm = T)
		if(!is.na(clim.nbyear.min)) cmoy[ValToNA] <- NA
	}else{
		if(!is.matrix(clim.mean)) stop("Climatological values must be a matrix")
		if(nrow(clim.mean) != 12) stop("Climatological values must have 12 months rows")
		cmoy <- clim.mean
	}
	if(is.null(clim.sd)){
		csd <- apply(vtmp.clim, c(1,3), sd, na.rm = T)
		if(!is.na(clim.nbyear.min)) csd[ValToNA] <- NA
	}else{
		if(!is.matrix(clim.sd)) stop("Climatological standard deviation must be a matrix")
		if(nrow(clim.sd) != 12) stop("Climatological standard deviation must have 12 months rows")
		csd <- clim.sd
	}

	cmoy1 <- matrix(rep(t(cmoy), nrow(xvar)/12), ncol = ncol(xvar), byrow = TRUE)
	csd1 <- matrix(rep(t(csd), nrow(xvar)/12), ncol = ncol(xvar), byrow = TRUE)
	xvar <- (xvar-cmoy1)/csd1

	if(elen > 0) xvar <- xvar[head(1:nrow(xvar), -elen), , drop = FALSE]
	if(slen > 0) xvar <- xvar[head(1:nrow(xvar), -slen), , drop = FALSE]
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(vmat1, vmat2, vtmp.clim, cmoy, cmoy1, csd, csd1, dates)
	return(xvar)
}

## stAnom array
StandardizedAnomalies.monthly_array <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL, clim.sd = NULL){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]

	vmat1 <- array(NA, dim = c(dim(xvar[, , 1]), slen))
	vmat2 <- array(NA, dim = c(dim(xvar[, , 1]), elen))
	xvar <- array(c(vmat1, xvar, vmat2), dim = c(dim(xvar)[1:2], (dim(vmat1)+dim(xvar)+dim(vmat2))[3]))

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(substr(dates, 1,4))
		vtmp.clim <- xvar[, , an >= clim.range[1] & an <= clim.range[2], drop = FALSE]
	}

	if(!is.na(clim.nbyear.min)){
		ValToNA <- sapply(1:12, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 12), drop = FALSE], 1:2, function(j) length(which(!is.na(j)))), simplify = "array")  < clim.nbyear.min
	}

	if(is.null(clim.mean)){
		cmoy <- sapply(1:12, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 12), drop = FALSE], 1:2, mean, na.rm = TRUE), simplify = "array")
		if(!is.na(clim.nbyear.min)) cmoy[ValToNA] <- NA
	}else{
		if(!is.array(clim.mean)) stop("Climatological values must be an 3d array")
		if(dim(clim.mean)[3] != 12) stop("Climatological values must have 12 months length")
		cmoy <- clim.mean
	}
	if(is.null(clim.sd)){
		csd <- sapply(1:12, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 12), drop = FALSE], 1:2, sd, na.rm = TRUE), simplify = "array")
		if(!is.na(clim.nbyear.min)) csd[ValToNA] <- NA
	}else{
		if(!is.array(clim.sd)) stop("Climatological standard deviation must be an 3d array")
		if(dim(clim.sd)[3] != 12) stop("Climatological standard deviation must have 12 months length")
		csd <- clim.sd
	}

	for(j in 1:12){
		ijL <- seq(j, dim(xvar)[3], 12)
		xvar[, , ijL] <- (xvar[, , ijL, drop = FALSE]-replicate(length(ijL), cmoy[, , j], simplify = "array"))/replicate(length(ijL), csd[, , j], simplify = "array")
	}

	if(elen > 0) xvar <- xvar[, , head(1:dim(xvar)[3], -elen), drop = FALSE]
	if(slen > 0) xvar <- xvar[, , tail(1:dim(xvar)[3], -slen), drop = FALSE]
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(vtmp.clim, cmoy, csd, dates, vmat1, vmat2)
	gc()
	return(xvar)
}

## stAnom list
StandardizedAnomalies.monthly_list <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL, clim.sd = NULL){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]

	vmat <- xvar[[1]]
	vmat[] <- NA
	vmat1 <- vector(mode = 'list', length = slen)
	vmat2 <- vector(mode = 'list', length = elen)
	xvar <- c(lapply(vmat1, function(x) vmat), xvar, lapply(vmat2, function(x) vmat))

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(substr(dates, 1,4))
		vtmp.clim <- xvar[an >= clim.range[1] & an <= clim.range[2]]
	}

	if(!is.na(clim.nbyear.min)){
		ValToNA <-  lapply(1:12, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 12)]), 1:2, function(j) length(which(!is.na(j)))) < clim.nbyear.min)  
	}

	if(is.null(clim.mean)){
		cmoy <- lapply(1:12, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 12)]), 1:2, mean, na.rm = TRUE))
		cmoy <- lapply(1:length(cmoy), function(j){
			x <- cmoy[[j]]
			toNA <- ValToNA[[j]]
			x[toNA] <- NA
			x
		})
	}else{
		if(!is.vector(clim.mean , mode = 'list')) stop("Climatological values must be a list of matrix")
		if(length(clim.mean) != 12) stop("Climatological values must have 12 months length")
		cmoy <- clim.mean
	}
	if(is.null(clim.sd)){
		csd <- lapply(1:12, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 12)]), 1:2, sd, na.rm = TRUE))
		csd <- lapply(1:length(csd), function(j){
			x <- csd[[j]]
			toNA <- ValToNA[[j]]
			x[toNA] <- NA
			x
		})
	}else{
		if(!is.vector(clim.sd , mode = 'list')) stop("Climatological standard deviation must be a list of matrix")
		if(length(clim.sd) != 12) stop("Climatological standard deviation must have 12 months length")
		csd <- clim.sd
	}

	for(j in 1:12){
		ijL <- seq(j, length(xvar), 12)
		xvar[ijL] <- lapply(xvar[ijL], function(x) (x-cmoy[[j]])/csd[[j]])
	}

	if(elen > 0) xvar <- xvar[head(1:length(xvar), -elen)]
	if(slen > 0) xvar <- xvar[tail(1:length(xvar), -slen)]
	xvar <- lapply(xvar, function(x){
		x[is.nan(x)] <- 0
		x[is.infinite(x)] <- 0
		x
	})
	rm(vtmp.clim, cmoy, csd, dates, vmat, vmat1, vmat2)
	gc()
	return(xvar)
}

###############################################################

### Anomalies

## Anom vector
Anomalies.monthly_vector <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]

	xvar <- matrix(as.numeric(c(rep(NA, slen), xvar, rep(NA, elen))), nrow = 12)
	
	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- unique(as.numeric(substr(dates, 1,4)))
		vtmp.clim <- xvar[, an >= clim.range[1] & an <= clim.range[2], drop = FALSE]
	}

	if(!is.na(clim.nbyear.min)){
		ValToNA <- apply(vtmp.clim, 1, function(j) length(which(!is.na(j)))) < clim.nbyear.min
	}
		
	if(is.null(clim.mean)){
		cmoy <- apply(vtmp.clim, 1, mean, na.rm = TRUE)
		if(!is.na(clim.nbyear.min)) cmoy[ValToNA] <- NA
	}else{
		if(!is.vector(clim.mean , mode = 'numeric')) stop("Climatological values must be a numeric vector")
		if(length(clim.mean) != 12) stop("Climatological values must have 12 months length")
		cmoy <- clim.mean
	}

	xvar <- c(xvar-cmoy)
	
	if(elen > 0) xvar <- head(xvar, -elen)
	if(slen > 0) xvar <- tail(xvar, -slen)
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(cmoy, dates, vtmp.clim)
	return(xvar)
}

## Anom matrix
Anomalies.monthly_matrix <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]

	vmat1 <- matrix(NA, ncol = ncol(xvar), nrow = slen)
	vmat2 <- matrix(NA, ncol = ncol(xvar), nrow = elen)
	xvar <- rbind(vmat1, xvar, vmat2)

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(substr(dates, 1,4))
		vtmp.clim <- xvar[an >= clim.range[1] & an <= clim.range[2], , drop = FALSE]
	}
	vtmp.clim <- array(vtmp.clim, c(12, nrow(vtmp.clim)/12, ncol(vtmp.clim)))

	if(!is.na(clim.nbyear.min)){
		ValToNA <- apply(vtmp.clim, c(1,3), function(j) length(which(!is.na(j)))) < clim.nbyear.min
	}
	
	if(is.null(clim.mean)){
		cmoy <- apply(vtmp.clim, c(1,3), mean, na.rm = T)
		if(!is.na(clim.nbyear.min)) cmoy[ValToNA] <- NA
	}else{
		if(!is.matrix(clim.mean)) stop("Climatological values must be a matrix")
		if(nrow(clim.mean) != 12) stop("Climatological values must have 12 months rows")
		cmoy <- clim.mean
	}

	cmoy1 <- matrix(rep(t(cmoy), nrow(xvar)/12), ncol = ncol(xvar), byrow = TRUE)
	xvar <- xvar-cmoy1

	if(elen > 0) xvar <- xvar[head(1:nrow(xvar), -elen), , drop = FALSE]
	if(slen > 0) xvar <- xvar[head(1:nrow(xvar), -slen), , drop = FALSE]
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(vmat1, vmat2, vtmp.clim, cmoy, cmoy1, dates)
	return(xvar)
}

## Anom array
Anomalies.monthly_array <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]

	vmat1 <- array(NA, dim = c(dim(xvar[, , 1]), slen))
	vmat2 <- array(NA, dim = c(dim(xvar[, , 1]), elen))
	xvar <- array(c(vmat1, xvar, vmat2), dim = c(dim(xvar)[1:2], (dim(vmat1)+dim(xvar)+dim(vmat2))[3]))

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(substr(dates, 1,4))
		vtmp.clim <- xvar[, , an >= clim.range[1] & an <= clim.range[2], drop = FALSE]
	}

	if(!is.na(clim.nbyear.min)){
		ValToNA <- sapply(1:12, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 12), drop = FALSE], 1:2, function(j) length(which(!is.na(j)))), simplify = "array")  < clim.nbyear.min
	}

	if(is.null(clim.mean)){
		cmoy <- sapply(1:12, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 12), drop = FALSE], 1:2, mean, na.rm = TRUE), simplify = "array")
		if(!is.na(clim.nbyear.min)) cmoy[ValToNA] <- NA
	}else{
		if(!is.array(clim.mean)) stop("Climatological values must be an 3d array")
		if(dim(clim.mean)[3] != 12) stop("Climatological values must have 12 months length")
		cmoy <- clim.mean
	}

	for(j in 1:12){
		ijL <- seq(j, dim(xvar)[3], 12)
		xvar[, , ijL] <- xvar[, , ijL, drop = FALSE]-replicate(length(ijL), cmoy[, , j], simplify = "array")
	}

	if(elen > 0) xvar <- xvar[, , head(1:dim(xvar)[3], -elen), drop = FALSE]
	if(slen > 0) xvar <- xvar[, , tail(1:dim(xvar)[3], -slen), drop = FALSE]
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(vtmp.clim, cmoy, dates, vmat1, vmat2)
	gc()
	return(xvar)
}

## Anom list
Anomalies.monthly_list <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]

	vmat <- xvar[[1]]
	vmat[] <- NA
	vmat1 <- vector(mode = 'list', length = slen)
	vmat2 <- vector(mode = 'list', length = elen)
	xvar <- c(lapply(vmat1, function(x) vmat), xvar, lapply(vmat2, function(x) vmat))

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(substr(dates, 1,4))
		vtmp.clim <- xvar[an >= clim.range[1] & an <= clim.range[2]]
	}

	if(!is.na(clim.nbyear.min)){
		ValToNA <-  lapply(1:12, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 12)]), 1:2, function(j) length(which(!is.na(j)))) < clim.nbyear.min)  
	}

	if(is.null(clim.mean)){
		cmoy <- lapply(1:12, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 12)]), 1:2, mean, na.rm = TRUE))
		cmoy <- lapply(1:length(cmoy), function(j){
			x <- cmoy[[j]]
			toNA <- ValToNA[[j]]
			x[toNA] <- NA
			x
		})
	}else{
		if(!is.vector(clim.mean , mode = 'list')) stop("Climatological values must be a list of matrix")
		if(length(clim.mean) != 12) stop("Climatological values must have 12 months length")
		cmoy <- clim.mean
	}

	for(j in 1:12){
		ijL <- seq(j, length(xvar), 12)
		xvar[ijL] <- lapply(xvar[ijL], function(x) x-cmoy[[j]])
	}

	if(elen > 0) xvar <- xvar[head(1:length(xvar), -elen)]
	if(slen > 0) xvar <- xvar[tail(1:length(xvar), -slen)]
	xvar <- lapply(xvar, function(x){
		x[is.nan(x)] <- 0
		x[is.infinite(x)] <- 0
		x
	})
	rm(vtmp.clim, cmoy, dates, vmat, vmat1, vmat2)
	gc()
	return(xvar)
}

######################################################  CLIMATO DEKADAL  ########################################################

### Climatologies

## Clim vector
Climatologies.dekadal_vector <- function(xvar, dates, fun = 'mean', ..., clim.range = NULL, clim.nbyear.min = NA){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	dek <- as.numeric(substr(dates, 7,7))
	oneyrdek <- rev(expand.grid(1:3,1:12))
	slen <- which(oneyrdek[,1] == mois[1] & oneyrdek[,2] == dek[1])-1
	elen <- 36-which(oneyrdek[,1] == mois[length(mois)] & oneyrdek[,2] == dek[length(mois)])

	xvar <- matrix(as.numeric(c(rep(NA, slen), xvar, rep(NA, elen))), nrow = 36)

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- unique(as.numeric(substr(dates, 1,4)))
		vtmp.clim <- xvar[, an >= clim.range[1] & an <= clim.range[2], drop = FALSE]
	}

	fun <- match.fun(fun)
	xvar <- apply(vtmp.clim, 1, fun, ...)
	if(!is.na(clim.nbyear.min)){
		ValToNA <- apply(vtmp.clim, 1, function(j) length(which(!is.na(j)))) < clim.nbyear.min
		xvar[ValToNA] <- NA
	}
	rm(dates, vtmp.clim)
	return(xvar)
}

## Clim matrix
Climatologies.dekadal_matrix <- function(xvar, dates, fun = 'mean', ..., clim.range = NULL, clim.nbyear.min = NA){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	dek <- as.numeric(substr(dates, 7,7))
	oneyrdek <- rev(expand.grid(1:3,1:12))
	slen <- which(oneyrdek[,1] == mois[1] & oneyrdek[,2] == dek[1])-1
	elen <- 36-which(oneyrdek[,1] == mois[length(mois)] & oneyrdek[,2] == dek[length(mois)])

	vmat1 <- matrix(NA, ncol = ncol(xvar), nrow = slen)
	vmat2 <- matrix(NA, ncol = ncol(xvar), nrow = elen)
	xvar <- rbind(vmat1, xvar, vmat2)

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(substr(dates, 1,4))
		vtmp.clim <- xvar[an >= clim.range[1] & an <= clim.range[2], , drop = FALSE]
	}
	
	fun <- match.fun(fun)
	vtmp.clim <- array(vtmp.clim, c(36, nrow(vtmp.clim)/36, ncol(vtmp.clim)))
	xvar <- apply(vtmp.clim, c(1,3), fun, ...)
	if(!is.na(clim.nbyear.min)){
		ValToNA <- apply(vtmp.clim, c(1,3), function(j) length(which(!is.na(j)))) < clim.nbyear.min
		xvar[ValToNA] <- NA
	}
	rm(vmat1, vmat2, vtmp.clim, dates)
	return(xvar)
}

## Clim array
Climatologies.dekadal_array <- function(xvar, dates, fun = 'mean', ..., clim.range = NULL, clim.nbyear.min = NA){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	dek <- as.numeric(substr(dates, 7,7))
	oneyrdek <- rev(expand.grid(1:3,1:12))
	slen <- which(oneyrdek[,1] == mois[1] & oneyrdek[,2] == dek[1])-1
	elen <- 36-which(oneyrdek[,1] == mois[length(mois)] & oneyrdek[,2] == dek[length(mois)])

	vmat1 <- array(NA, dim = c(dim(xvar[, , 1]), slen))
	vmat2 <- array(NA, dim = c(dim(xvar[, , 1]), elen))
	xvar <- array(c(vmat1, xvar, vmat2), dim = c(dim(xvar)[1:2], (dim(vmat1)+dim(xvar)+dim(vmat2))[3]))

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(substr(dates, 1,4))
		vtmp.clim <- xvar[, , an >= clim.range[1] & an <= clim.range[2], drop = FALSE]
	}

	fun <- match.fun(fun)
	xvar <- sapply(1:36, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 36), drop = FALSE], 1:2, fun, ...), simplify = "array")
	if(!is.na(clim.nbyear.min)){
		ValToNA <- sapply(1:36, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 36), drop = FALSE], 1:2, function(j) length(which(!is.na(j)))), simplify = "array")  < clim.nbyear.min
		xvar[ValToNA] <- NA
	}
	rm(vtmp.clim, dates, vmat1, vmat2)
	gc()
	return(xvar)
}

## Clim list
Climatologies.dekadal_list <- function(xvar, dates, fun = 'mean', ..., clim.range = NULL, clim.nbyear.min = NA){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	dek <- as.numeric(substr(dates, 7,7))
	oneyrdek <- rev(expand.grid(1:3,1:12))
	slen <- which(oneyrdek[,1] == mois[1] & oneyrdek[,2] == dek[1])-1
	elen <- 36-which(oneyrdek[,1] == mois[length(mois)] & oneyrdek[,2] == dek[length(mois)])

	vmat <- xvar[[1]]
	vmat[] <- NA
	vmat1 <- vector(mode = 'list', length = slen)
	vmat2 <- vector(mode = 'list', length = elen)
	xvar <- c(lapply(vmat1, function(x) vmat), xvar, lapply(vmat2, function(x) vmat))

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(substr(dates, 1,4))
		vtmp.clim <- xvar[an >= clim.range[1] & an <= clim.range[2]]
	}

	fun <- match.fun(fun)
	xvar <- lapply(1:36, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 36)]), 1:2, fun, ...))

	if(!is.na(clim.nbyear.min)){
		ValToNA <-  lapply(1:36, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 36)]), 1:2, function(j) length(which(!is.na(j)))) < clim.nbyear.min)  
		xvar <- lapply(1:length(xvar), function(j){
			x <- xvar[[j]]
			toNA <- ValToNA[[j]]
			x[toNA] <- NA
			x
		})
	}
	rm(vtmp.clim,  dates, vmat, vmat1, vmat2)
	gc()
	return(xvar)
}

###############################################################

### Standardized Anomalies

## stAnom vector
StandardizedAnomalies.dekadal_vector <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL, clim.sd = NULL){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	dek <- as.numeric(substr(dates, 7,7))
	oneyrdek <- rev(expand.grid(1:3,1:12))
	slen <- which(oneyrdek[,1] == mois[1] & oneyrdek[,2] == dek[1])-1
	elen <- 36-which(oneyrdek[,1] == mois[length(mois)] & oneyrdek[,2] == dek[length(mois)])

	xvar <- matrix(as.numeric(c(rep(NA, slen), xvar, rep(NA, elen))), nrow = 36)
	
	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- unique(as.numeric(substr(dates, 1,4)))
		vtmp.clim <- xvar[, an >= clim.range[1] & an <= clim.range[2], drop = FALSE]
	}

	if(!is.na(clim.nbyear.min)){
		ValToNA <- apply(vtmp.clim, 1, function(j) length(which(!is.na(j)))) < clim.nbyear.min
	}
		
	if(is.null(clim.mean)){
		cmoy <- apply(vtmp.clim, 1, mean, na.rm = TRUE)
		if(!is.na(clim.nbyear.min)) cmoy[ValToNA] <- NA
	}else{
		if(!is.vector(clim.mean , mode = 'numeric')) stop("Climatological values must be a numeric vector")
		if(length(clim.mean) != 36) stop("Climatological values must have 36 dekads length")
		cmoy <- clim.mean
	}
	if(is.null(clim.sd)){
		csd <- apply(vtmp.clim, 1, sd, na.rm = TRUE)
		if(!is.na(clim.nbyear.min)) csd[ValToNA] <- NA
	}else{
		if(!is.vector(clim.sd , mode = 'numeric')) stop("Climatological standard deviation must be a numeric vector")
		if(length(clim.sd) != 36) stop("Climatological standard deviation must have 36 dekads length")
		csd <- clim.sd
	}

	xvar <- c((xvar-cmoy)/csd)
	
	if(elen > 0) xvar <- head(xvar, -elen)
	if(slen > 0) xvar <- tail(xvar, -slen)
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(cmoy, csd, dates, vtmp.clim)
	return(xvar)
}

## stAnom matrix
StandardizedAnomalies.dekadal_matrix <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL, clim.sd = NULL){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	dek <- as.numeric(substr(dates, 7,7))
	oneyrdek <- rev(expand.grid(1:3,1:12))
	slen <- which(oneyrdek[,1] == mois[1] & oneyrdek[,2] == dek[1])-1
	elen <- 36-which(oneyrdek[,1] == mois[length(mois)] & oneyrdek[,2] == dek[length(mois)])

	vmat1 <- matrix(NA, ncol = ncol(xvar), nrow = slen)
	vmat2 <- matrix(NA, ncol = ncol(xvar), nrow = elen)
	xvar <- rbind(vmat1, xvar, vmat2)

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(substr(dates, 1,4))
		vtmp.clim <- xvar[an >= clim.range[1] & an <= clim.range[2], , drop = FALSE]
	}
	vtmp.clim <- array(vtmp.clim, c(36, nrow(vtmp.clim)/36, ncol(vtmp.clim)))

	if(!is.na(clim.nbyear.min)){
		ValToNA <- apply(vtmp.clim, c(1,3), function(j) length(which(!is.na(j)))) < clim.nbyear.min
	}
	
	if(is.null(clim.mean)){
		cmoy <- apply(vtmp.clim, c(1,3), mean, na.rm = T)
		if(!is.na(clim.nbyear.min)) cmoy[ValToNA] <- NA
	}else{
		if(!is.matrix(clim.mean)) stop("Climatological values must be a matrix")
		if(nrow(clim.mean) != 36) stop("Climatological values must have 36 dekads rows")
		cmoy <- clim.mean
	}
	if(is.null(clim.sd)){
		csd <- apply(vtmp.clim, c(1,3), sd, na.rm = T)
		if(!is.na(clim.nbyear.min)) csd[ValToNA] <- NA
	}else{
		if(!is.matrix(clim.sd)) stop("Climatological standard deviation must be a matrix")
		if(nrow(clim.sd) != 36) stop("Climatological standard deviation must have 36 dekads rows")
		csd <- clim.sd
	}

	cmoy1 <- matrix(rep(t(cmoy), nrow(xvar)/36), ncol = ncol(xvar), byrow = TRUE)
	csd1 <- matrix(rep(t(csd), nrow(xvar)/36), ncol = ncol(xvar), byrow = TRUE)
	xvar <- (xvar-cmoy1)/csd1

	if(elen > 0) xvar <- xvar[head(1:nrow(xvar), -elen), , drop = FALSE]
	if(slen > 0) xvar <- xvar[head(1:nrow(xvar), -slen), , drop = FALSE]
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(vmat1, vmat2, vtmp.clim, cmoy, cmoy1, csd, csd1, dates)
	return(xvar)
}

## stAnom array
StandardizedAnomalies.dekadal_array <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL, clim.sd = NULL){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	dek <- as.numeric(substr(dates, 7,7))
	oneyrdek <- rev(expand.grid(1:3,1:12))
	slen <- which(oneyrdek[,1] == mois[1] & oneyrdek[,2] == dek[1])-1
	elen <- 36-which(oneyrdek[,1] == mois[length(mois)] & oneyrdek[,2] == dek[length(mois)])

	vmat1 <- array(NA, dim = c(dim(xvar[, , 1]), slen))
	vmat2 <- array(NA, dim = c(dim(xvar[, , 1]), elen))
	xvar <- array(c(vmat1, xvar, vmat2), dim = c(dim(xvar)[1:2], (dim(vmat1)+dim(xvar)+dim(vmat2))[3]))

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(substr(dates, 1,4))
		vtmp.clim <- xvar[, , an >= clim.range[1] & an <= clim.range[2], drop = FALSE]
	}

	if(!is.na(clim.nbyear.min)){
		ValToNA <- sapply(1:36, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 36), drop = FALSE], 1:2, function(j) length(which(!is.na(j)))), simplify = "array")  < clim.nbyear.min
	}

	if(is.null(clim.mean)){
		cmoy <- sapply(1:36, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 36), drop = FALSE], 1:2, mean, na.rm = TRUE), simplify = "array")
		if(!is.na(clim.nbyear.min)) cmoy[ValToNA] <- NA
	}else{
		if(!is.array(clim.mean)) stop("Climatological values must be an 3d array")
		if(dim(clim.mean)[3] != 36) stop("Climatological values must have 36 dekads length")
		cmoy <- clim.mean
	}
	if(is.null(clim.sd)){
		csd <- sapply(1:36, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 36), drop = FALSE], 1:2, sd, na.rm = TRUE), simplify = "array")
		if(!is.na(clim.nbyear.min)) csd[ValToNA] <- NA
	}else{
		if(!is.array(clim.sd)) stop("Climatological standard deviation must be an 3d array")
		if(dim(clim.sd)[3] != 36) stop("Climatological standard deviation must have 36 dekads length")
		csd <- clim.sd
	}

	for(j in 1:36){
		ijL <- seq(j, dim(xvar)[3], 36)
		xvar[, , ijL] <- (xvar[, , ijL, drop = FALSE]-replicate(length(ijL), cmoy[, , j], simplify = "array"))/replicate(length(ijL), csd[, , j], simplify = "array")
	}

	if(elen > 0) xvar <- xvar[, , head(1:dim(xvar)[3], -elen), drop = FALSE]
	if(slen > 0) xvar <- xvar[, , tail(1:dim(xvar)[3], -slen), drop = FALSE]
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(vtmp.clim, cmoy, csd, dates, vmat1, vmat2)
	gc()
	return(xvar)
}

## stAnom list
StandardizedAnomalies.dekadal_list <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL, clim.sd = NULL){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	dek <- as.numeric(substr(dates, 7,7))
	oneyrdek <- rev(expand.grid(1:3,1:12))
	slen <- which(oneyrdek[,1] == mois[1] & oneyrdek[,2] == dek[1])-1
	elen <- 36-which(oneyrdek[,1] == mois[length(mois)] & oneyrdek[,2] == dek[length(mois)])

	vmat <- xvar[[1]]
	vmat[] <- NA
	vmat1 <- vector(mode = 'list', length = slen)
	vmat2 <- vector(mode = 'list', length = elen)
	xvar <- c(lapply(vmat1, function(x) vmat), xvar, lapply(vmat2, function(x) vmat))

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(substr(dates, 1,4))
		vtmp.clim <- xvar[an >= clim.range[1] & an <= clim.range[2]]
	}

	if(!is.na(clim.nbyear.min)){
		ValToNA <-  lapply(1:36, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 36)]), 1:2, function(j) length(which(!is.na(j)))) < clim.nbyear.min)  
	}

	if(is.null(clim.mean)){
		cmoy <- lapply(1:36, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 36)]), 1:2, mean, na.rm = TRUE))
		cmoy <- lapply(1:length(cmoy), function(j){
			x <- cmoy[[j]]
			toNA <- ValToNA[[j]]
			x[toNA] <- NA
			x
		})
	}else{
		if(!is.vector(clim.mean , mode = 'list')) stop("Climatological values must be a list of matrix")
		if(length(clim.mean) != 36) stop("Climatological values must have 36 dekads length")
		cmoy <- clim.mean
	}
	if(is.null(clim.sd)){
		csd <- lapply(1:36, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 36)]), 1:2, sd, na.rm = TRUE))
		csd <- lapply(1:length(csd), function(j){
			x <- csd[[j]]
			toNA <- ValToNA[[j]]
			x[toNA] <- NA
			x
		})
	}else{
		if(!is.vector(clim.sd , mode = 'list')) stop("Climatological standard deviation must be a list of matrix")
		if(length(clim.sd) != 36) stop("Climatological standard deviation must have 36 dekads length")
		csd <- clim.sd
	}

	for(j in 1:36){
		ijL <- seq(j, length(xvar), 36)
		xvar[ijL] <- lapply(xvar[ijL], function(x) (x-cmoy[[j]])/csd[[j]])
	}

	if(elen > 0) xvar <- xvar[head(1:length(xvar), -elen)]
	if(slen > 0) xvar <- xvar[tail(1:length(xvar), -slen)]
	xvar <- lapply(xvar, function(x){
		x[is.nan(x)] <- 0
		x[is.infinite(x)] <- 0
		x
	})
	rm(vtmp.clim, cmoy, csd, dates, vmat, vmat1, vmat2)
	gc()
	return(xvar)
}

###############################################################

### Anomalies

## Anom vector
Anomalies.dekadal_vector <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	dek <- as.numeric(substr(dates, 7,7))
	oneyrdek <- rev(expand.grid(1:3,1:12))
	slen <- which(oneyrdek[,1] == mois[1] & oneyrdek[,2] == dek[1])-1
	elen <- 36-which(oneyrdek[,1] == mois[length(mois)] & oneyrdek[,2] == dek[length(mois)])

	xvar <- matrix(as.numeric(c(rep(NA, slen), xvar, rep(NA, elen))), nrow = 36)
	
	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- unique(as.numeric(substr(dates, 1,4)))
		vtmp.clim <- xvar[, an >= clim.range[1] & an <= clim.range[2], drop = FALSE]
	}

	if(!is.na(clim.nbyear.min)){
		ValToNA <- apply(vtmp.clim, 1, function(j) length(which(!is.na(j)))) < clim.nbyear.min
	}
		
	if(is.null(clim.mean)){
		cmoy <- apply(vtmp.clim, 1, mean, na.rm = TRUE)
		if(!is.na(clim.nbyear.min)) cmoy[ValToNA] <- NA
	}else{
		if(!is.vector(clim.mean , mode = 'numeric')) stop("Climatological values must be a numeric vector")
		if(length(clim.mean) != 36) stop("Climatological values must have 36 dekads length")
		cmoy <- clim.mean
	}

	xvar <- c(xvar-cmoy)
	
	if(elen > 0) xvar <- head(xvar, -elen)
	if(slen > 0) xvar <- tail(xvar, -slen)
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(cmoy, dates, vtmp.clim)
	return(xvar)
}

## Anom matrix
Anomalies.dekadal_matrix <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	dek <- as.numeric(substr(dates, 7,7))
	oneyrdek <- rev(expand.grid(1:3,1:12))
	slen <- which(oneyrdek[,1] == mois[1] & oneyrdek[,2] == dek[1])-1
	elen <- 36-which(oneyrdek[,1] == mois[length(mois)] & oneyrdek[,2] == dek[length(mois)])

	vmat1 <- matrix(NA, ncol = ncol(xvar), nrow = slen)
	vmat2 <- matrix(NA, ncol = ncol(xvar), nrow = elen)
	xvar <- rbind(vmat1, xvar, vmat2)

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(substr(dates, 1,4))
		vtmp.clim <- xvar[an >= clim.range[1] & an <= clim.range[2], , drop = FALSE]
	}
	vtmp.clim <- array(vtmp.clim, c(36, nrow(vtmp.clim)/36, ncol(vtmp.clim)))

	if(!is.na(clim.nbyear.min)){
		ValToNA <- apply(vtmp.clim, c(1,3), function(j) length(which(!is.na(j)))) < clim.nbyear.min
	}
	
	if(is.null(clim.mean)){
		cmoy <- apply(vtmp.clim, c(1,3), mean, na.rm = T)
		if(!is.na(clim.nbyear.min)) cmoy[ValToNA] <- NA
	}else{
		if(!is.matrix(clim.mean)) stop("Climatological values must be a matrix")
		if(nrow(clim.mean) != 36) stop("Climatological values must have 36 dekads rows")
		cmoy <- clim.mean
	}

	cmoy1 <- matrix(rep(t(cmoy), nrow(xvar)/36), ncol = ncol(xvar), byrow = TRUE)
	xvar <- xvar-cmoy1

	if(elen > 0) xvar <- xvar[head(1:nrow(xvar), -elen), , drop = FALSE]
	if(slen > 0) xvar <- xvar[head(1:nrow(xvar), -slen), , drop = FALSE]
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(vmat1, vmat2, vtmp.clim, cmoy, cmoy1, dates)
	return(xvar)
}

## Anom array
Anomalies.dekadal_array <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	dek <- as.numeric(substr(dates, 7,7))
	oneyrdek <- rev(expand.grid(1:3,1:12))
	slen <- which(oneyrdek[,1] == mois[1] & oneyrdek[,2] == dek[1])-1
	elen <- 36-which(oneyrdek[,1] == mois[length(mois)] & oneyrdek[,2] == dek[length(mois)])

	vmat1 <- array(NA, dim = c(dim(xvar[, , 1]), slen))
	vmat2 <- array(NA, dim = c(dim(xvar[, , 1]), elen))
	xvar <- array(c(vmat1, xvar, vmat2), dim = c(dim(xvar)[1:2], (dim(vmat1)+dim(xvar)+dim(vmat2))[3]))

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(substr(dates, 1,4))
		vtmp.clim <- xvar[, , an >= clim.range[1] & an <= clim.range[2], drop = FALSE]
	}

	if(!is.na(clim.nbyear.min)){
		ValToNA <- sapply(1:36, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 36), drop = FALSE], 1:2, function(j) length(which(!is.na(j)))), simplify = "array")  < clim.nbyear.min
	}

	if(is.null(clim.mean)){
		cmoy <- sapply(1:36, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 36), drop = FALSE], 1:2, mean, na.rm = TRUE), simplify = "array")
		if(!is.na(clim.nbyear.min)) cmoy[ValToNA] <- NA
	}else{
		if(!is.array(clim.mean)) stop("Climatological values must be an 3d array")
		if(dim(clim.mean)[3] != 36) stop("Climatological values must have 36 dekads length")
		cmoy <- clim.mean
	}

	for(j in 1:36){
		ijL <- seq(j, dim(xvar)[3], 36)
		xvar[, , ijL] <- xvar[, , ijL, drop = FALSE]-replicate(length(ijL), cmoy[, , j], simplify = "array")
	}

	if(elen > 0) xvar <- xvar[, , head(1:dim(xvar)[3], -elen), drop = FALSE]
	if(slen > 0) xvar <- xvar[, , tail(1:dim(xvar)[3], -slen), drop = FALSE]
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(vtmp.clim, cmoy, dates, vmat1, vmat2)
	gc()
	return(xvar)
}

## Anom list
Anomalies.dekadal_list <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL){
	dates <- as.character(dates)
	mois <- as.numeric(substr(dates, 5,6))
	dek <- as.numeric(substr(dates, 7,7))
	oneyrdek <- rev(expand.grid(1:3,1:12))
	slen <- which(oneyrdek[,1] == mois[1] & oneyrdek[,2] == dek[1])-1
	elen <- 36-which(oneyrdek[,1] == mois[length(mois)] & oneyrdek[,2] == dek[length(mois)])

	vmat <- xvar[[1]]
	vmat[] <- NA
	vmat1 <- vector(mode = 'list', length = slen)
	vmat2 <- vector(mode = 'list', length = elen)
	xvar <- c(lapply(vmat1, function(x) vmat), xvar, lapply(vmat2, function(x) vmat))

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(substr(dates, 1,4))
		vtmp.clim <- xvar[an >= clim.range[1] & an <= clim.range[2]]
	}

	if(!is.na(clim.nbyear.min)){
		ValToNA <-  lapply(1:36, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 36)]), 1:2, function(j) length(which(!is.na(j)))) < clim.nbyear.min)  
	}

	if(is.null(clim.mean)){
		cmoy <- lapply(1:36, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 36)]), 1:2, mean, na.rm = TRUE))
		cmoy <- lapply(1:length(cmoy), function(j){
			x <- cmoy[[j]]
			toNA <- ValToNA[[j]]
			x[toNA] <- NA
			x
		})
	}else{
		if(!is.vector(clim.mean , mode = 'list')) stop("Climatological values must be a list of matrix")
		if(length(clim.mean) != 36) stop("Climatological values must have 36 dekads length")
		cmoy <- clim.mean
	}

	for(j in 1:36){
		ijL <- seq(j, length(xvar), 36)
		xvar[ijL] <- lapply(xvar[ijL], function(x) x-cmoy[[j]])
	}

	if(elen > 0) xvar <- xvar[head(1:length(xvar), -elen)]
	if(slen > 0) xvar <- xvar[tail(1:length(xvar), -slen)]
	xvar <- lapply(xvar, function(x){
		x[is.nan(x)] <- 0
		x[is.infinite(x)] <- 0
		x
	})
	rm(vtmp.clim, cmoy, dates, vmat, vmat1, vmat2)
	gc()
	return(xvar)
}

######################################################  CLIMATO DAILY  ##########################################################
### Climatologies

## Clim vector
Climatologies.daily_vector <- function(xvar, dates, fun = 'mean', clim.range = NULL, clim.nbyear.min = NA, ...){
	dates <- as.Date(dates, format='%Y%m%d')
	leap.year <- format(dates,'%m%d') == '0229'

	xvar <- xvar[!leap.year]
	dates <- dates[!leap.year]

	dstart <- seq(as.Date(paste(format(dates[1],'%Y'), 1,1, sep = '-')), dates[1],'day')
	dstart <- dstart[-length(dstart)]
	dend <- seq(dates[length(dates)], as.Date(paste(format(dates[length(dates)],'%Y'), 12,31, sep = '-')),'day')[-1]
	slen <- length(dstart[!format(dstart,'%m%d') == '0229'])
	elen <- length(dend[!format(dend,'%m%d') == '0229'])	

	xvar <- matrix(as.numeric(c(rep(NA, slen), xvar, rep(NA, elen))), nrow = 365)

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- unique(as.numeric(format(dates,'%Y')))
		vtmp.clim <- xvar[, an >= clim.range[1] & an <= clim.range[2], drop = FALSE]
	}

	fun <- match.fun(fun)
	xvar <- apply(vtmp.clim, 1, fun, ...)
	if(!is.na(clim.nbyear.min)){
		ValToNA <- apply(vtmp.clim, 1, function(j) length(which(!is.na(j)))) < clim.nbyear.min
		xvar[ValToNA] <- NA
	}
	rm(dates, dstart, dend, vtmp.clim)
	return(xvar)
}

## Clim matrix
Climatologies.daily_matrix <- function(xvar, dates, fun = 'mean', clim.range = NULL, clim.nbyear.min = NA, ...){
	dates <- as.Date(dates, format='%Y%m%d')
	leap.year <- format(dates,'%m%d') == '0229'

	xvar <- xvar[!leap.year, ]
	dates <- dates[!leap.year]
	
	dstart <- seq(as.Date(paste(format(dates[1],'%Y'), 1,1, sep = '-')), dates[1],'day')
	dstart <- dstart[-length(dstart)]
	dend <- seq(dates[length(dates)], as.Date(paste(format(dates[length(dates)],'%Y'), 12,31, sep = '-')),'day')[-1]
	slen <- length(dstart[!format(dstart,'%m%d') == '0229'])
	elen <- length(dend[!format(dend,'%m%d') == '0229'])

	vmat1 <- matrix(NA, ncol = ncol(xvar), nrow = slen)
	vmat2 <- matrix(NA, ncol = ncol(xvar), nrow = elen)
	xvar <- rbind(vmat1, xvar, vmat2)

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(format(dates,'%Y'))
		vtmp.clim <- xvar[an >= clim.range[1] & an <= clim.range[2], , drop = FALSE]
	}
	
	fun <- match.fun(fun)
	vtmp.clim <- array(vtmp.clim, c(365, nrow(vtmp.clim)/365, ncol(vtmp.clim)))
	xvar <- apply(vtmp.clim, c(1,3), fun, ...)
	if(!is.na(clim.nbyear.min)){
		ValToNA <- apply(vtmp.clim, c(1,3), function(j) length(which(!is.na(j)))) < clim.nbyear.min
		xvar[ValToNA] <- NA
	}
	rm(vmat1, vmat2, vtmp.clim, dates, dstart, dend)
	return(xvar)
}

## Clim array
Climatologies.daily_array <- function(xvar, dates, fun = 'mean', clim.range = NULL, clim.nbyear.min = NA, ...){
	dates <- as.Date(dates, format='%Y%m%d')
	leap.year <- format(dates,'%m%d') == '0229'
	
	xvar <- xvar[, , !leap.year]
	dates <- dates[!leap.year]
	
	dstart <- seq(as.Date(paste(format(dates[1],'%Y'), 1,1, sep = '-')), dates[1],'day')
	dstart <- dstart[-length(dstart)]
	dend <- seq(dates[length(dates)], as.Date(paste(format(dates[length(dates)],'%Y'), 12,31, sep = '-')),'day')[-1]
	slen <- length(dstart[!format(dstart,'%m%d') == '0229'])
	elen <- length(dend[!format(dend,'%m%d') == '0229'])

	vmat1 <- array(NA, dim = c(dim(xvar[, , 1]), slen))
	vmat2 <- array(NA, dim = c(dim(xvar[, , 1]), elen))
	xvar <- array(c(vmat1, xvar, vmat2), dim = c(dim(xvar)[1:2], (dim(vmat1)+dim(xvar)+dim(vmat2))[3]))

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(format(dates,'%Y'))
		vtmp.clim <- xvar[, , an >= clim.range[1] & an <= clim.range[2], drop = FALSE]
	}

	fun <- match.fun(fun)
	xvar <- sapply(1:365, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 365), drop = FALSE], 1:2, fun, ...), simplify = "array")
	if(!is.na(clim.nbyear.min)){
		ValToNA <- sapply(1:365, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 365), drop = FALSE], 1:2, function(j) length(which(!is.na(j)))), simplify = "array")  < clim.nbyear.min
		xvar[ValToNA] <- NA
	}
	rm(vtmp.clim, dates, dstart, dend, vmat1, vmat2)
	gc()
	return(xvar)
}

## Clim list
Climatologies.daily_list <- function(xvar, dates, fun = 'mean', clim.range = NULL, clim.nbyear.min = NA, ...){
	dates <- as.Date(dates, format='%Y%m%d')
	leap.year <- format(dates,'%m%d') == '0229'
	
	xvar <- xvar[!leap.year]
	dates <- dates[!leap.year]
	
	dstart <- seq(as.Date(paste(format(dates[1],'%Y'), 1,1, sep = '-')), dates[1],'day')
	dstart <- dstart[-length(dstart)]
	dend <- seq(dates[length(dates)], as.Date(paste(format(dates[length(dates)],'%Y'), 12,31, sep = '-')),'day')[-1]
	slen <- length(dstart[!format(dstart,'%m%d') == '0229'])
	elen <- length(dend[!format(dend,'%m%d') == '0229'])

	vmat <- xvar[[1]]
	vmat[] <- NA
	vmat1 <- vector(mode = 'list', length = slen)
	vmat2 <- vector(mode = 'list', length = elen)
	xvar <- c(lapply(vmat1, function(x) vmat), xvar, lapply(vmat2, function(x) vmat))

	if(is.null(clim.range)){
		vtmp.clim <- xvar
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(format(dates,'%Y'))
		vtmp.clim <- xvar[an >= clim.range[1] & an <= clim.range[2]]
	}

	fun <- match.fun(fun)
	xvar <- lapply(1:365, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 365)]), 1:2, fun, ...))

	if(!is.na(clim.nbyear.min)){
		ValToNA <-  lapply(1:365, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 365)]), 1:2, function(j) length(which(!is.na(j)))) < clim.nbyear.min)  
		xvar <- lapply(1:length(xvar), function(j){
			x <- xvar[[j]]
			toNA <- ValToNA[[j]]
			x[toNA] <- NA
			x
		})
	}
	rm(vtmp.clim,  dates, dstart, dend, vmat, vmat1, vmat2)
	gc()
	return(xvar)
}

###############################################################

### Standardized Anomalies

## stAnom vector
StandardizedAnomalies.daily_vector <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL, clim.sd = NULL){
	dates <- as.Date(dates, format='%Y%m%d')
	leap.year <- format(dates,'%m%d') == '0229'

	vtmp <- xvar[!leap.year]
	dates <- dates[!leap.year]
	
	dstart <- seq(as.Date(paste(format(dates[1],'%Y'), 1,1, sep = '-')), dates[1],'day')
	dstart <- dstart[-length(dstart)]
	dend <- seq(dates[length(dates)], as.Date(paste(format(dates[length(dates)],'%Y'), 12,31, sep = '-')),'day')[-1]
	slen <- length(dstart[!format(dstart,'%m%d') == '0229'])
	elen <- length(dend[!format(dend,'%m%d') == '0229'])

	vtmp <- matrix(as.numeric(c(rep(NA, slen), vtmp, rep(NA, elen))), nrow = 365)
	
	if(is.null(clim.range)){
		vtmp.clim <- vtmp
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- unique(as.numeric(format(dates,'%Y')))
		vtmp.clim <- vtmp[, an >= clim.range[1] & an <= clim.range[2], drop = FALSE]
	}

	if(!is.na(clim.nbyear.min)){
		ValToNA <- apply(vtmp.clim, 1, function(j) length(which(!is.na(j)))) < clim.nbyear.min
	}
		
	if(is.null(clim.mean)){
		cmoy <- apply(vtmp.clim, 1, mean, na.rm = TRUE)
		if(!is.na(clim.nbyear.min)) cmoy[ValToNA] <- NA
	}else{
		if(!is.vector(clim.mean , mode = 'numeric')) stop("Climatological values must be a numeric vector")
		if(length(clim.mean) != 365) stop("Climatological values must have 365 days length")
		cmoy <- clim.mean
	}
	if(is.null(clim.sd)){
		csd <- apply(vtmp.clim, 1, sd, na.rm = TRUE)
		if(!is.na(clim.nbyear.min)) csd[ValToNA] <- NA
	}else{
		if(!is.vector(clim.sd , mode = 'numeric')) stop("Climatological standard deviation must be a numeric vector")
		if(length(clim.sd) != 365) stop("Climatological standard deviation must have 365 days length")
		csd <- clim.sd
	}

	vtmp <- c((vtmp-cmoy)/csd)
	
	if(elen > 0) vtmp <- head(vtmp, -elen)
	if(slen > 0) vtmp <- tail(vtmp, -slen)

	xvar[!leap.year] <- vtmp
	xvar[leap.year] <- (xvar[leap.year]-cmoy[59])/csd[59]
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(vtmp, cmoy, csd, dates, dstart, dend, vtmp.clim)
	return(xvar)
}

## stAnom matrix
StandardizedAnomalies.daily_matrix <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL, clim.sd = NULL){
	dates <- as.Date(dates, format='%Y%m%d')
	leap.year <- format(dates,'%m%d') == '0229'

	vtmp <- xvar[!leap.year, ]
	dates <- dates[!leap.year]
	
	dstart <- seq(as.Date(paste(format(dates[1],'%Y'), 1,1, sep = '-')), dates[1],'day')
	dstart <- dstart[-length(dstart)]
	dend <- seq(dates[length(dates)], as.Date(paste(format(dates[length(dates)],'%Y'), 12,31, sep = '-')),'day')[-1]
	slen <- length(dstart[!format(dstart,'%m%d') == '0229'])
	elen <- length(dend[!format(dend,'%m%d') == '0229'])

	vmat1 <- matrix(NA, ncol = ncol(vtmp), nrow = slen)
	vmat2 <- matrix(NA, ncol = ncol(vtmp), nrow = elen)
	vtmp <- rbind(vmat1, vtmp, vmat2)

	if(is.null(clim.range)){
		vtmp.clim <- vtmp
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(format(dates,'%Y'))
		vtmp.clim <- vtmp[an >= clim.range[1] & an <= clim.range[2], , drop = FALSE]
	}
	vtmp.clim <- array(vtmp.clim, c(365, nrow(vtmp.clim)/365, ncol(vtmp.clim)))

	if(!is.na(clim.nbyear.min)){
		ValToNA <- apply(vtmp.clim, c(1,3), function(j) length(which(!is.na(j)))) < clim.nbyear.min
	}
	
	if(is.null(clim.mean)){
		cmoy <- apply(vtmp.clim, c(1,3), mean, na.rm = T)
		if(!is.na(clim.nbyear.min)) cmoy[ValToNA] <- NA
	}else{
		if(!is.matrix(clim.mean)) stop("Climatological values must be a matrix")
		if(nrow(clim.mean) != 365) stop("Climatological values must have 365 days rows")
		cmoy <- clim.mean
	}
	if(is.null(clim.sd)){
		csd <- apply(vtmp.clim, c(1,3), sd, na.rm = T)
		if(!is.na(clim.nbyear.min)) csd[ValToNA] <- NA
	}else{
		if(!is.matrix(clim.sd)) stop("Climatological standard deviation must be a matrix")
		if(nrow(clim.sd) != 365) stop("Climatological standard deviation must have 365 days rows")
		csd <- clim.sd
	}

	cmoy1 <- matrix(rep(t(cmoy), nrow(vtmp)/365), ncol = ncol(vtmp), byrow = TRUE)
	csd1 <- matrix(rep(t(csd), nrow(vtmp)/365), ncol = ncol(vtmp), byrow = TRUE)
	vtmp <- (vtmp-cmoy1)/csd1

	if(elen > 0) vtmp <- vtmp[head(1:nrow(vtmp), -elen), , drop = FALSE]
	if(slen > 0) vtmp <- vtmp[head(1:nrow(vtmp), -slen), , drop = FALSE]

	xvar[!leap.year, ] <- vtmp
	xvar[leap.year, ] <- (xvar[leap.year, , drop = FALSE]-cmoy[59, , drop = FALSE])/csd[59, , drop = FALSE]
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(vtmp, vmat1, vmat2, vtmp.clim, cmoy, cmoy1, csd, csd1, dates, dstart, dend)
	return(xvar)
}

## stAnom array
StandardizedAnomalies.daily_array <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL, clim.sd = NULL){
	dates <- as.Date(dates, format='%Y%m%d')
	leap.year <- format(dates,'%m%d') == '0229'
	
	vtmp <- xvar[, , !leap.year]
	dates <- dates[!leap.year]
	
	dstart <- seq(as.Date(paste(format(dates[1],'%Y'), 1,1, sep = '-')), dates[1],'day')
	dstart <- dstart[-length(dstart)]
	dend <- seq(dates[length(dates)], as.Date(paste(format(dates[length(dates)],'%Y'), 12,31, sep = '-')),'day')[-1]
	slen <- length(dstart[!format(dstart,'%m%d') == '0229'])
	elen <- length(dend[!format(dend,'%m%d') == '0229'])

	vmat1 <- array(NA, dim = c(dim(vtmp[, , 1]), slen))
	vmat2 <- array(NA, dim = c(dim(vtmp[, , 1]), elen))
	vtmp <- array(c(vmat1, vtmp, vmat2), dim = c(dim(vtmp)[1:2], (dim(vmat1)+dim(vtmp)+dim(vmat2))[3]))

	if(is.null(clim.range)){
		vtmp.clim <- vtmp
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(format(dates,'%Y'))
		vtmp.clim <- vtmp[, , an >= clim.range[1] & an <= clim.range[2], drop = FALSE]
	}

	if(!is.na(clim.nbyear.min)){
		ValToNA <- sapply(1:365, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 365), drop = FALSE], 1:2, function(j) length(which(!is.na(j)))), simplify = "array")  < clim.nbyear.min
	}

	if(is.null(clim.mean)){
		cmoy <- sapply(1:365, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 365), drop = FALSE], 1:2, mean, na.rm = TRUE), simplify = "array")
		if(!is.na(clim.nbyear.min)) cmoy[ValToNA] <- NA
	}else{
		if(!is.array(clim.mean)) stop("Climatological values must be an 3d array")
		if(dim(clim.mean)[3] != 365) stop("Climatological values must have 365 days length")
		cmoy <- clim.mean
	}
	if(is.null(clim.sd)){
		csd <- sapply(1:365, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 365), drop = FALSE], 1:2, sd, na.rm = TRUE), simplify = "array")
		if(!is.na(clim.nbyear.min)) csd[ValToNA] <- NA
	}else{
		if(!is.array(clim.sd)) stop("Climatological standard deviation must be an 3d array")
		if(dim(clim.sd)[3] != 365) stop("Climatological standard deviation must have 365 days length")
		csd <- clim.sd
	}

	for(j in 1:365){
		ijL <- seq(j, dim(vtmp)[3], 365)
		vtmp[, , ijL] <- (vtmp[, , ijL, drop = FALSE]-replicate(length(ijL), cmoy[, , j], simplify = "array"))/replicate(length(ijL), csd[, , j], simplify = "array")
	}

	if(elen > 0) vtmp <- vtmp[, , head(1:dim(vtmp)[3], -elen), drop = FALSE]
	if(slen > 0) vtmp <- vtmp[, , tail(1:dim(vtmp)[3], -slen), drop = FALSE]

	xvar[, , !leap.year]  <- vtmp
	xvar[, , leap.year] <- (xvar[, , leap.year, drop = FALSE]-replicate(length(which(leap.year)), cmoy[, , 59], simplify = "array"))/replicate(length(which(leap.year)), csd[, , 59], simplify = "array")
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(vtmp, vtmp.clim, cmoy, csd, dates, dstart, dend, vmat1, vmat2)
	gc()
	return(xvar)
}

## stAnom list
StandardizedAnomalies.daily_list <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL, clim.sd = NULL){
	dates <- as.Date(dates, format='%Y%m%d')
	leap.year <- format(dates,'%m%d') == '0229'
	
	vtmp <- xvar[!leap.year]
	dates <- dates[!leap.year]
	
	dstart <- seq(as.Date(paste(format(dates[1],'%Y'), 1,1, sep = '-')), dates[1],'day')
	dstart <- dstart[-length(dstart)]
	dend <- seq(dates[length(dates)], as.Date(paste(format(dates[length(dates)],'%Y'), 12,31, sep = '-')),'day')[-1]
	slen <- length(dstart[!format(dstart,'%m%d') == '0229'])
	elen <- length(dend[!format(dend,'%m%d') == '0229'])

	vmat <- vtmp[[1]]
	vmat[] <- NA
	vmat1 <- vector(mode = 'list', length = slen)
	vmat2 <- vector(mode = 'list', length = elen)
	vtmp <- c(lapply(vmat1, function(x) vmat), vtmp, lapply(vmat2, function(x) vmat))

	if(is.null(clim.range)){
		vtmp.clim <- vtmp
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(format(dates,'%Y'))
		vtmp.clim <- vtmp[an >= clim.range[1] & an <= clim.range[2]]
	}

	if(!is.na(clim.nbyear.min)){
		ValToNA <-  lapply(1:365, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 365)]), 1:2, function(j) length(which(!is.na(j)))) < clim.nbyear.min)  
	}

	if(is.null(clim.mean)){
		cmoy <- lapply(1:365, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 365)]), 1:2, mean, na.rm = TRUE))
		cmoy <- lapply(1:length(cmoy), function(j){
			x <- cmoy[[j]]
			toNA <- ValToNA[[j]]
			x[toNA] <- NA
			x
		})
	}else{
		if(!is.vector(clim.mean , mode = 'list')) stop("Climatological values must be a list of matrix")
		if(length(clim.mean) != 365) stop("Climatological values must have 365 days length")
		cmoy <- clim.mean
	}
	if(is.null(clim.sd)){
		csd <- lapply(1:365, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 365)]), 1:2, sd, na.rm = TRUE))
		csd <- lapply(1:length(csd), function(j){
			x <- csd[[j]]
			toNA <- ValToNA[[j]]
			x[toNA] <- NA
			x
		})
	}else{
		if(!is.vector(clim.sd , mode = 'list')) stop("Climatological standard deviation must be a list of matrix")
		if(length(clim.sd) != 365) stop("Climatological standard deviation must have 365 days length")
		csd <- clim.sd
	}

	for(j in 1:365){
		ijL <- seq(j, length(vtmp), 365)
		vtmp[ijL] <- lapply(vtmp[ijL], function(x) (x-cmoy[[j]])/csd[[j]])
	}

	if(elen > 0) vtmp <- vtmp[head(1:length(vtmp), -elen)]
	if(slen > 0) vtmp <- vtmp[tail(1:length(vtmp), -slen)]

	xvar[!leap.year]  <- vtmp
	xvar[leap.year] <- lapply(xvar[leap.year], function(x) (x-cmoy[[59]])/csd[[59]])
	xvar <- lapply(xvar, function(x){
		x[is.nan(x)] <- 0
		x[is.infinite(x)] <- 0
		x
	})
	rm(vtmp, vtmp.clim, cmoy, csd, dates, dstart, dend, vmat, vmat1, vmat2)
	gc()
	return(xvar)
}

###############################################################

### Anomalies

## Anom vector
Anomalies.daily_vector <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL){
	dates <- as.Date(dates, format='%Y%m%d')
	leap.year <- format(dates,'%m%d') == '0229'

	vtmp <- xvar[!leap.year]
	dates <- dates[!leap.year]
	
	dstart <- seq(as.Date(paste(format(dates[1],'%Y'), 1,1, sep = '-')), dates[1],'day')
	dstart <- dstart[-length(dstart)]
	dend <- seq(dates[length(dates)], as.Date(paste(format(dates[length(dates)],'%Y'), 12,31, sep = '-')),'day')[-1]
	slen <- length(dstart[!format(dstart,'%m%d') == '0229'])
	elen <- length(dend[!format(dend,'%m%d') == '0229'])

	vtmp <- matrix(as.numeric(c(rep(NA, slen), vtmp, rep(NA, elen))), nrow = 365)
	
	if(is.null(clim.range)){
		vtmp.clim <- vtmp
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- unique(as.numeric(format(dates,'%Y')))
		vtmp.clim <- vtmp[, an >= clim.range[1] & an <= clim.range[2], drop = FALSE]
	}

	if(!is.na(clim.nbyear.min)){
		ValToNA <- apply(vtmp.clim, 1, function(j) length(which(!is.na(j)))) < clim.nbyear.min
	}
		
	if(is.null(clim.mean)){
		cmoy <- apply(vtmp.clim, 1, mean, na.rm = TRUE)
		if(!is.na(clim.nbyear.min)) cmoy[ValToNA] <- NA
	}else{
		if(!is.vector(clim.mean , mode = 'numeric')) stop("Climatological values must be a numeric vector")
		if(length(clim.mean) != 365) stop("Climatological values must have 365 days length")
		cmoy <- clim.mean
	}

	vtmp <- c(vtmp-cmoy)
	
	if(elen > 0) vtmp <- head(vtmp, -elen)
	if(slen > 0) vtmp <- tail(vtmp, -slen)

	xvar[!leap.year] <- vtmp
	xvar[leap.year] <- xvar[leap.year]-cmoy[59]
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(vtmp, cmoy, dates, dstart, dend, vtmp.clim)
	return(xvar)
}

## Anom matrix
Anomalies.daily_matrix <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL){
	dates <- as.Date(dates, format='%Y%m%d')
	leap.year <- format(dates,'%m%d') == '0229'

	vtmp <- xvar[!leap.year, ]
	dates <- dates[!leap.year]
	
	dstart <- seq(as.Date(paste(format(dates[1],'%Y'), 1,1, sep = '-')), dates[1],'day')
	dstart <- dstart[-length(dstart)]
	dend <- seq(dates[length(dates)], as.Date(paste(format(dates[length(dates)],'%Y'), 12,31, sep = '-')),'day')[-1]
	slen <- length(dstart[!format(dstart,'%m%d') == '0229'])
	elen <- length(dend[!format(dend,'%m%d') == '0229'])

	vmat1 <- matrix(NA, ncol = ncol(vtmp), nrow = slen)
	vmat2 <- matrix(NA, ncol = ncol(vtmp), nrow = elen)
	vtmp <- rbind(vmat1, vtmp, vmat2)

	if(is.null(clim.range)){
		vtmp.clim <- vtmp
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(format(dates,'%Y'))
		vtmp.clim <- vtmp[an >= clim.range[1] & an <= clim.range[2], , drop = FALSE]
	}
	vtmp.clim <- array(vtmp.clim, c(365, nrow(vtmp.clim)/365, ncol(vtmp.clim)))

	if(!is.na(clim.nbyear.min)){
		ValToNA <- apply(vtmp.clim, c(1,3), function(j) length(which(!is.na(j)))) < clim.nbyear.min
	}
	
	if(is.null(clim.mean)){
		cmoy <- apply(vtmp.clim, c(1,3), mean, na.rm = T)
		if(!is.na(clim.nbyear.min)) cmoy[ValToNA] <- NA
	}else{
		if(!is.matrix(clim.mean)) stop("Climatological values must be a matrix")
		if(nrow(clim.mean) != 365) stop("Climatological values must have 365 days rows")
		cmoy <- clim.mean
	}

	cmoy1 <- matrix(rep(t(cmoy), nrow(vtmp)/365), ncol = ncol(vtmp), byrow = TRUE)
	vtmp <- vtmp-cmoy1

	if(elen > 0) vtmp <- vtmp[head(1:nrow(vtmp), -elen), , drop = FALSE]
	if(slen > 0) vtmp <- vtmp[head(1:nrow(vtmp), -slen), , drop = FALSE]

	xvar[!leap.year, ] <- vtmp
	xvar[leap.year, ] <- xvar[leap.year, , drop = FALSE]-cmoy[59, , drop = FALSE]
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(vtmp, vmat1, vmat2, vtmp.clim, cmoy, cmoy1, dates, dstart, dend)
	return(xvar)
}

## Anom array
Anomalies.daily_array <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL){
	dates <- as.Date(dates, format='%Y%m%d')
	leap.year <- format(dates,'%m%d') == '0229'
	
	vtmp <- xvar[, , !leap.year]
	dates <- dates[!leap.year]
	
	dstart <- seq(as.Date(paste(format(dates[1],'%Y'), 1,1, sep = '-')), dates[1],'day')
	dstart <- dstart[-length(dstart)]
	dend <- seq(dates[length(dates)], as.Date(paste(format(dates[length(dates)],'%Y'), 12,31, sep = '-')),'day')[-1]
	slen <- length(dstart[!format(dstart,'%m%d') == '0229'])
	elen <- length(dend[!format(dend,'%m%d') == '0229'])

	vmat1 <- array(NA, dim = c(dim(vtmp[, , 1]), slen))
	vmat2 <- array(NA, dim = c(dim(vtmp[, , 1]), elen))
	vtmp <- array(c(vmat1, vtmp, vmat2), dim = c(dim(vtmp)[1:2], (dim(vmat1)+dim(vtmp)+dim(vmat2))[3]))

	if(is.null(clim.range)){
		vtmp.clim <- vtmp
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(format(dates,'%Y'))
		vtmp.clim <- vtmp[, , an >= clim.range[1] & an <= clim.range[2], drop = FALSE]
	}

	if(!is.na(clim.nbyear.min)){
		ValToNA <- sapply(1:365, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 365), drop = FALSE], 1:2, function(j) length(which(!is.na(j)))), simplify = "array")  < clim.nbyear.min
	}

	if(is.null(clim.mean)){
		cmoy <- sapply(1:365, function(j) apply(vtmp.clim[, , seq(j, dim(vtmp.clim)[3], 365), drop = FALSE], 1:2, mean, na.rm = TRUE), simplify = "array")
		if(!is.na(clim.nbyear.min)) cmoy[ValToNA] <- NA
	}else{
		if(!is.array(clim.mean)) stop("Climatological values must be an 3d array")
		if(dim(clim.mean)[3] != 365) stop("Climatological values must have 365 days length")
		cmoy <- clim.mean
	}

	for(j in 1:365){
		ijL <- seq(j, dim(vtmp)[3], 365)
		vtmp[, , ijL] <- vtmp[, , ijL, drop = FALSE]-replicate(length(ijL), cmoy[, , j], simplify = "array")
	}

	if(elen > 0) vtmp <- vtmp[, , head(1:dim(vtmp)[3], -elen), drop = FALSE]
	if(slen > 0) vtmp <- vtmp[, , tail(1:dim(vtmp)[3], -slen), drop = FALSE]

	xvar[, , !leap.year]  <- vtmp
	xvar[, , leap.year] <- xvar[, , leap.year, drop = FALSE]-replicate(length(which(leap.year)), cmoy[, , 59], simplify = "array")
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(vtmp, vtmp.clim, cmoy, dates, dstart, dend, vmat1, vmat2)
	gc()
	return(xvar)
}

## Anom list
Anomalies.daily_list <- function(xvar, dates, clim.range = NULL, clim.nbyear.min = NA, clim.mean = NULL){
	dates <- as.Date(dates, format='%Y%m%d')
	leap.year <- format(dates,'%m%d') == '0229'
	
	vtmp <- xvar[!leap.year]
	dates <- dates[!leap.year]
	
	dstart <- seq(as.Date(paste(format(dates[1],'%Y'), 1,1, sep = '-')), dates[1],'day')
	dstart <- dstart[-length(dstart)]
	dend <- seq(dates[length(dates)], as.Date(paste(format(dates[length(dates)],'%Y'), 12,31, sep = '-')),'day')[-1]
	slen <- length(dstart[!format(dstart,'%m%d') == '0229'])
	elen <- length(dend[!format(dend,'%m%d') == '0229'])

	vmat <- vtmp[[1]]
	vmat[] <- NA
	vmat1 <- vector(mode = 'list', length = slen)
	vmat2 <- vector(mode = 'list', length = elen)
	vtmp <- c(lapply(vmat1, function(x) vmat), vtmp, lapply(vmat2, function(x) vmat))

	if(is.null(clim.range)){
		vtmp.clim <- vtmp
	}else{
		if(length(clim.range) != 2) stop("Period range must be a vector of length 2")
		an <- as.numeric(format(dates,'%Y'))
		vtmp.clim <- vtmp[an >= clim.range[1] & an <= clim.range[2]]
	}

	if(!is.na(clim.nbyear.min)){
		ValToNA <-  lapply(1:365, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 365)]), 1:2, function(j) length(which(!is.na(j)))) < clim.nbyear.min)  
	}

	if(is.null(clim.mean)){
		cmoy <- lapply(1:365, function(j) apply(simplify2array(vtmp.clim[seq(j, length(vtmp.clim), 365)]), 1:2, mean, na.rm = TRUE))
		cmoy <- lapply(1:length(cmoy), function(j){
			x <- cmoy[[j]]
			toNA <- ValToNA[[j]]
			x[toNA] <- NA
			x
		})
	}else{
		if(!is.vector(clim.mean , mode = 'list')) stop("Climatological values must be a list of matrix")
		if(length(clim.mean) != 365) stop("Climatological values must have 365 days length")
		cmoy <- clim.mean
	}

	for(j in 1:365){
		ijL <- seq(j, length(vtmp), 365)
		vtmp[ijL] <- lapply(vtmp[ijL], function(x) x-cmoy[[j]])
	}

	if(elen > 0) vtmp <- vtmp[head(1:length(vtmp), -elen)]
	if(slen > 0) vtmp <- vtmp[tail(1:length(vtmp), -slen)]

	xvar[!leap.year]  <- vtmp
	xvar[leap.year] <- lapply(xvar[leap.year], function(x) x-cmoy[[59]])
	xvar <- lapply(xvar, function(x){
		x[is.nan(x)] <- 0
		x[is.infinite(x)] <- 0
		x
	})
	rm(vtmp, vtmp.clim, cmoy, dates, dstart, dend, vmat, vmat1, vmat2)
	gc()
	return(xvar)
}

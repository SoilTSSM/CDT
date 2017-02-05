standard.daily <- function(xvar, dates){
	dates <- as.Date(dates, format='%Y%m%d')
	leap.year <- format(dates,'%m%d') == '0229'
	
	vtmp <- xvar
	vtmp <- vtmp[!leap.year]
	vdates <- dates[!leap.year]
	
	dstart <- seq(as.Date(paste(format(vdates[1],'%Y'), 1,1, sep = '-')), vdates[1],'day')
	dstart <- dstart[-length(dstart)]
	dend <- seq(vdates[length(vdates)], as.Date(paste(format(vdates[length(vdates)],'%Y'), 12,31, sep = '-')),'day')[-1]
	slen <- length(dstart[!format(dstart,'%m%d') == '0229'])
	elen <- length(dend[!format(dend,'%m%d') == '0229'])
	
	mat <- matrix(as.numeric(c(rep(NA, slen), vtmp, rep(NA, elen))), nrow = 365)
	cmoy <- apply(mat, 1, mean, na.rm = T)
	csd <- apply(mat, 1, sd, na.rm = T)

	mat1 <- c((mat-cmoy)/csd)
	if(elen > 0) mat1 <- head(mat1,-elen)
	if(slen > 0) mat1 <- tail(mat1,-slen)
	xvar[!leap.year] <- mat1
	xvar[leap.year] <- (xvar[leap.year]-cmoy[59])/csd[59]
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(vtmp, cmoy, csd, vdates, dates, dstart, dend, mat, mat1)
	return(xvar)
}

standard.daily_lstOmat <- function(xvar, dates){
	dates <- as.Date(dates, format='%Y%m%d')
	leap.year <- format(dates,'%m%d') == '0229'
	
	vtmp <- xvar
	vtmp <- vtmp[!leap.year]
	vdates <- dates[!leap.year]
	
	dstart <- seq(as.Date(paste(format(vdates[1],'%Y'), 1,1, sep = '-')), vdates[1],'day')
	dstart <- dstart[-length(dstart)]
	dend <- seq(vdates[length(vdates)], as.Date(paste(format(vdates[length(vdates)],'%Y'), 12,31, sep = '-')),'day')[-1]
	slen <- length(dstart[!format(dstart,'%m%d') == '0229'])
	elen <- length(dend[!format(dend,'%m%d') == '0229'])

	vmat <- vtmp[[1]]
	vmat[] <- NA
	vmat1 <- vector(mode = 'list', length = slen)
	vmat2 <- vector(mode = 'list', length = elen)
	vtmp <- c(lapply(vmat1, function(x) vmat), vtmp, lapply(vmat2, function(x) vmat))
	cmoy <- lapply(1:365, function(j) apply(simplify2array(vtmp[seq(j, length(vtmp), 365)]), 1:2, mean, na.rm = TRUE))
	csd  <- lapply(1:365, function(j) apply(simplify2array(vtmp[seq(j, length(vtmp), 365)]), 1:2, sd, na.rm = TRUE))

	for(j in 1:365) vtmp[seq(j, length(vtmp), 365)] <- lapply(vtmp[seq(j, length(vtmp), 365)], function(x) (x-cmoy[[j]])/csd[[j]])
	if(elen > 0) vtmp <- vtmp[head(1:length(vtmp), -elen)]
	if(slen > 0) vtmp <- vtmp[tail(1:length(vtmp), -slen)]
	xvar[!leap.year]  <- vtmp
	xvar[leap.year] <- lapply(xvar[leap.year], function(x) (x-cmoy[[59]])/csd[[59]])
	xvar <- lapply(xvar, function(x){
		x[is.nan(x)] <- 0
		x[is.infinite(x)] <- 0
		x
	})
	rm(vtmp, cmoy, csd, vdates, dates, dstart, dend, vmat, vmat1, vmat2)
	return(xvar)
}

#####

standard.monthly <- function(xvar, dates){
	dates <- as.character(dates)
	an <- as.numeric(substr(dates, 1,4))
	mois <- as.numeric(substr(dates, 5,6))

	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]
	
	mat <- matrix(as.numeric(c(rep(NA, slen), xvar, rep(NA, elen))), nrow = 12)
	cmoy <- apply(mat, 1, mean, na.rm = T)
	csd <- apply(mat, 1, sd, na.rm = T)
	
	xvar <- c((mat-cmoy)/csd)
	if(elen > 0) xvar <- head(xvar,-elen)
	if(slen > 0) xvar <- tail(xvar,-slen)
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(mat, cmoy, csd, dates, an, mois)
	return(xvar)
}

standard.monthly_lstOmat <- function(xvar, dates){
	dates <- as.character(dates)
	an <- as.numeric(substr(dates, 1,4))
	mois <- as.numeric(substr(dates, 5,6))

	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]

	vmat <- xvar[[1]]
	vmat[] <- NA
	vmat1 <- vector(mode = 'list', length = slen)
	vmat2 <- vector(mode = 'list', length = elen)
	xvar <- c(lapply(vmat1, function(x) vmat), xvar, lapply(vmat2, function(x) vmat))

	cmoy <- lapply(1:12, function(j) apply(simplify2array(xvar[seq(j, length(xvar), 12)]), 1:2, mean, na.rm = TRUE))
	csd  <- lapply(1:12, function(j) apply(simplify2array(xvar[seq(j, length(xvar), 12)]), 1:2, sd, na.rm = TRUE))
	for(j in 1:12) xvar[seq(j, length(xvar), 12)] <- lapply(xvar[seq(j, length(xvar), 12)], function(x) (x-cmoy[[j]])/csd[[j]])
	if(elen > 0) xvar <- xvar[head(1:length(xvar), -elen)]
	if(slen > 0) xvar <- xvar[tail(1:length(xvar), -slen)]
	xvar <- lapply(xvar, function(x){
		x[is.nan(x)] <- 0
		x[is.infinite(x)] <- 0
		x
	})
	rm(vmat, vmat1, vmat2, cmoy, csd, dates, an, mois)
	return(xvar)
}

#######

standard.dekadal <- function(xvar, dates){
	dates <- as.character(dates)
	an <- as.numeric(substr(dates, 1,4))
	mois <- as.numeric(substr(dates, 5,6))
	dek <- as.numeric(substr(dates, 7,7))

	oneyrdek <- rev(expand.grid(1:3,1:12))
	slen <- which(oneyrdek[,1] == mois[1] & oneyrdek[,2] == dek[1])-1
	elen <- 36-which(oneyrdek[,1] == mois[length(mois)] & oneyrdek[,2] == dek[length(mois)])
	
	mat <- matrix(as.numeric(c(rep(NA, slen), xvar, rep(NA, elen))), nrow = 36)
	cmoy <- apply(mat, 1, mean, na.rm = T)
	csd <- apply(mat, 1, sd, na.rm = T)

	xvar <- c((mat-cmoy)/csd)
	if(elen > 0) xvar <- head(xvar,-elen)
	if(slen > 0) xvar <- tail(xvar,-slen)
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(mat, cmoy, csd, dates, an, mois, dek)
	return(xvar)
}

standard.dekadal_lstOmat <- function(xvar, dates){
	dates <- as.character(dates)
	an <- as.numeric(substr(dates, 1,4))
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

	cmoy <- lapply(1:36, function(j) apply(simplify2array(xvar[seq(j, length(xvar), 36)]), 1:2, mean, na.rm = TRUE))
	csd  <- lapply(1:36, function(j) apply(simplify2array(xvar[seq(j, length(xvar), 36)]), 1:2, sd, na.rm = TRUE))
	for(j in 1:36) xvar[seq(j, length(xvar), 36)] <- lapply(xvar[seq(j, length(xvar), 36)], function(x) (x-cmoy[[j]])/csd[[j]])
	if(elen > 0) xvar <- xvar[head(1:length(xvar), -elen)]
	if(slen > 0) xvar <- xvar[tail(1:length(xvar), -slen)]
	xvar <- lapply(xvar, function(x){
		x[is.nan(x)] <- 0
		x[is.infinite(x)] <- 0
		x
	})
	rm(vmat, vmat1, vmat2, cmoy, csd, dates, an, mois, dek)
	return(xvar)
}

#########################################################

anomaly.daily <- function(xvar, dates){
	dates <- as.Date(dates, format='%Y%m%d')
	leap.year <- format(dates,'%m%d') == '0229'
	
	vtmp <- xvar
	vtmp <- vtmp[!leap.year]
	vdates <- dates[!leap.year]

	dstart <- seq(as.Date(paste(format(vdates[1],'%Y'), 1,1, sep = '-')), vdates[1],'day')
	dstart <- dstart[-length(dstart)]
	dend <- seq(vdates[length(vdates)], as.Date(paste(format(vdates[length(vdates)],'%Y'), 12,31, sep = '-')),'day')[-1]
	slen <- length(dstart[!format(dstart,'%m%d') == '0229'])
	elen <- length(dend[!format(dend,'%m%d') == '0229'])

	mat <- matrix(as.numeric(c(rep(NA, slen), vtmp, rep(NA, elen))), nrow = 365)
	cmoy <- apply(mat, 1, mean, na.rm = T)

	mat1 <- c(mat-cmoy)
	if(elen > 0) mat1 <- head(mat1,-elen)
	if(slen > 0) mat1 <- tail(mat1,-slen)
	xvar[!leap.year] <- mat1
	xvar[leap.year] <- xvar[leap.year]-cmoy[59]
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(vtmp, cmoy, vdates, dates, dstart, dend, mat, mat1)
	return(xvar)
}

anomaly.daily_lstOmat <- function(xvar, dates){
	dates <- as.Date(dates, format='%Y%m%d')
	leap.year <- format(dates,'%m%d') == '0229'

	vtmp <- xvar
	vtmp <- vtmp[!leap.year]
	vdates <- dates[!leap.year]
	
	dstart <- seq(as.Date(paste(format(vdates[1],'%Y'), 1,1, sep = '-')), vdates[1],'day')
	dstart <- dstart[-length(dstart)]
	dend <- seq(vdates[length(vdates)], as.Date(paste(format(vdates[length(vdates)],'%Y'), 12,31, sep = '-')),'day')[-1]
	slen <- length(dstart[!format(dstart,'%m%d') == '0229'])
	elen <- length(dend[!format(dend,'%m%d') == '0229'])

	vmat <- vtmp[[1]]
	vmat[] <- NA
	vmat1 <- vector(mode = 'list', length = slen)
	vmat2 <- vector(mode = 'list', length = elen)
	vtmp <- c(lapply(vmat1, function(x) vmat), vtmp, lapply(vmat2, function(x) vmat))
	cmoy <- lapply(1:365, function(j) apply(simplify2array(vtmp[seq(j, length(vtmp), 365)]), 1:2, mean, na.rm = TRUE))

	for(j in 1:365) vtmp[seq(j, length(vtmp), 365)] <- lapply(vtmp[seq(j, length(vtmp), 365)], function(x) x-cmoy[[j]])
	if(elen > 0) vtmp <- vtmp[head(1:length(vtmp), -elen)]
	if(slen > 0) vtmp <- vtmp[tail(1:length(vtmp), -slen)]
	xvar[!leap.year]  <- vtmp
	xvar[leap.year] <- lapply(xvar[leap.year], function(x) x-cmoy[[59]])
	xvar <- lapply(xvar, function(x){
		x[is.nan(x)] <- 0
		x[is.infinite(x)] <- 0
		x
	})
	rm(vtmp, cmoy, vdates, dates, dstart, dend, vmat, vmat1, vmat2)
	return(xvar)
}

#######

anomaly.monthly <- function(xvar, dates){
	dates <- as.character(dates)
	an <- as.numeric(substr(dates, 1,4))
	mois <- as.numeric(substr(dates, 5,6))

	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]
	
	mat <- matrix(as.numeric(c(rep(NA, slen), xvar, rep(NA, elen))), nrow = 12)
	cmoy <- apply(mat, 1, mean, na.rm = T)
	
	xvar <- c(mat-cmoy)
	if(elen > 0) xvar <- head(xvar,-elen)
	if(slen > 0) xvar <- tail(xvar,-slen)
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(mat, cmoy, dates, an, mois)
	return(xvar)
}

anomaly.monthly_lstOmat <- function(xvar, dates){
	dates <- as.character(dates)
	an <- as.numeric(substr(dates, 1,4))
	mois <- as.numeric(substr(dates, 5,6))

	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]

	vmat <- xvar[[1]]
	vmat[] <- NA
	vmat1 <- vector(mode = 'list', length = slen)
	vmat2 <- vector(mode = 'list', length = elen)
	xvar <- c(lapply(vmat1, function(x) vmat), xvar, lapply(vmat2, function(x) vmat))

	cmoy <- lapply(1:12, function(j) apply(simplify2array(xvar[seq(j, length(xvar), 12)]), 1:2, mean, na.rm = TRUE))
	
	for(j in 1:12) xvar[seq(j, length(xvar), 12)] <- lapply(xvar[seq(j, length(xvar), 12)], function(x) x-cmoy[[j]])
	if(elen > 0) xvar <- xvar[head(1:length(xvar), -elen)]
	if(slen > 0) xvar <- xvar[tail(1:length(xvar), -slen)]
	xvar <- lapply(xvar, function(x){
		x[is.nan(x)] <- 0
		x[is.infinite(x)] <- 0
		x
	})
	rm(vmat, vmat1, vmat2, cmoy, dates, an, mois)
	return(xvar)
}

#######

anomaly.dekadal <- function(xvar, dates){
	dates <- as.character(dates)
	an <- as.numeric(substr(dates, 1,4))
	mois <- as.numeric(substr(dates, 5,6))
	dek <- as.numeric(substr(dates, 7,7))

	oneyrdek <- rev(expand.grid(1:3,1:12))
	slen <- which(oneyrdek[,1] == mois[1] & oneyrdek[,2] == dek[1])-1
	elen <- 36-which(oneyrdek[,1] == mois[length(mois)] & oneyrdek[,2] == dek[length(mois)])
	
	mat <- matrix(as.numeric(c(rep(NA, slen), xvar, rep(NA, elen))), nrow = 36)
	cmoy <- apply(mat, 1, mean, na.rm = T)
	
	xvar <- c(mat-cmoy)
	if(elen > 0) xvar <- head(xvar,-elen)
	if(slen > 0) xvar <- tail(xvar,-slen)
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	rm(mat, cmoy, dates, an, mois, dek)
	return(xvar)
}

anomaly.dekadal_lstOmat <- function(xvar, dates){
	dates <- as.character(dates)
	an <- as.numeric(substr(dates, 1,4))
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

	cmoy <- lapply(1:36, function(j) apply(simplify2array(xvar[seq(j, length(xvar), 36)]), 1:2, mean, na.rm = TRUE))
	
	for(j in 1:36) xvar[seq(j, length(xvar), 36)] <- lapply(xvar[seq(j, length(xvar), 36)], function(x) x-cmoy[[j]])
	if(elen > 0) xvar <- xvar[head(1:length(xvar), -elen)]
	if(slen > 0) xvar <- xvar[tail(1:length(xvar), -slen)]
	xvar <- lapply(xvar, function(x){
		x[is.nan(x)] <- 0
		x[is.infinite(x)] <- 0
		x
	})
	rm(vmat, vmat1, vmat2, cmoy, dates, an, mois, dek)
	return(xvar)
}


####################################################
ratio.daily <- function(xvar, dates){
	dates <- as.Date(dates, format='%Y%m%d')
	leap.year <- format(dates,'%m%d') == '0229'

	vtmp <- xvar
	vtmp <- vtmp[!leap.year]
	vdates <- dates[!leap.year]

	dstart <- seq(as.Date(paste(format(vdates[1],'%Y'), 1,1, sep = '-')), vdates[1],'day')
	dstart <- dstart[-length(dstart)]
	dend <- seq(vdates[length(vdates)], as.Date(paste(format(vdates[length(vdates)],'%Y'), 12,31, sep = '-')),'day')[-1]
	slen <- length(dstart[!format(dstart,'%m%d') == '0229'])
	elen <- length(dend[!format(dend,'%m%d') == '0229'])
	
	mat <- matrix(as.numeric(c(rep(NA, slen), vtmp, rep(NA, elen))), nrow = 365)
	cmoy <- apply(mat, 1, mean, na.rm = T)
	
	mat1 <- c(mat/cmoy)
	if(elen > 0) mat1 <- head(mat1,-elen)
	if(slen > 0) mat1 <- tail(mat1,-slen)
	xvar[!leap.year] <- mat1
	xvar[leap.year] <- xvar[leap.year]/cmoy[59]
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	return(xvar)
}

ratio.monthly <- function(xvar, dates){
	dates <- as.character(dates)
	an <- as.numeric(substr(dates, 1,4))
	mois <- as.numeric(substr(dates, 5,6))

	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]
	
	mat <- matrix(as.numeric(c(rep(NA, slen), xvar, rep(NA, elen))), nrow = 12)
	cmoy <- apply(mat, 1, mean, na.rm = T)
	
	xvar <- c(mat/cmoy)
	if(elen > 0) xvar <- head(xvar,-elen)
	if(slen > 0) xvar <- tail(xvar,-slen)
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	return(xvar)
}

ratio.dekadal <- function(xvar, dates){
	dates <- as.character(dates)
	an <- as.numeric(substr(dates, 1,4))
	mois <- as.numeric(substr(dates, 5,6))
	dek <- as.numeric(substr(dates, 7,7))

	oneyrdek <- rev(expand.grid(1:3,1:12))
	slen <- which(oneyrdek[,1] == mois[1] & oneyrdek[,2] == dek[1])-1
	elen <- 36-which(oneyrdek[,1] == mois[length(mois)] & oneyrdek[,2] == dek[length(mois)])
	
	mat <- matrix(as.numeric(c(rep(NA, slen), xvar, rep(NA, elen))), nrow = 36)
	cmoy <- apply(mat, 1, mean, na.rm = T)
	
	xvar <- c(mat/cmoy)
	if(elen > 0) xvar <- head(xvar,-elen)
	if(slen > 0) xvar <- tail(xvar,-slen)
	xvar[is.nan(xvar)] <- 0
	xvar[is.infinite(xvar)] <- 0
	return(xvar)
}

########################
climato.monthly <- function(xvar, dates, fun = 'mean'){
	fun <- match.fun(fun)

	dates <- as.character(dates)
	an <- as.numeric(substr(dates, 1,4))
	mois <- as.numeric(substr(dates, 5,6))

	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]

	mat <- matrix(as.numeric(c(rep(NA, slen), xvar, rep(NA, elen))), nrow = 12)
	xvar <- as.numeric(apply(mat, 1, fun, na.rm = T))
	return(xvar)
}

climato.monthly_lstOmat <- function(xvar, dates, fun = 'mean'){
	fun <- match.fun(fun)

	dates <- as.character(dates)
	an <- as.numeric(substr(dates, 1,4))
	mois <- as.numeric(substr(dates, 5,6))

	slen <- mois[1]-1
	elen <- 12-mois[length(mois)]

	vmat <- xvar[[1]]
	vmat[] <- NA
	vmat1 <- vector(mode = 'list', length = slen)
	vmat2 <- vector(mode = 'list', length = elen)
	xvar <- c(lapply(vmat1, function(x) vmat), xvar, lapply(vmat2, function(x) vmat))

	xvar <- lapply(1:12, function(j) apply(simplify2array(xvar[seq(j, length(xvar), 12)]), 1:2, fun, na.rm = TRUE))
	rm(vmat, vmat1, vmat2, dates, an, mois)
	return(xvar)
}

#########

climato.dekadal <- function(xvar, dates, fun = 'mean'){
	fun <- match.fun(fun)

	dates <- as.character(dates)
	an <- as.numeric(substr(dates, 1,4))
	mois <- as.numeric(substr(dates, 5,6))
	dek <- as.numeric(substr(dates, 7,7))

	oneyrdek <- rev(expand.grid(1:3,1:12))
	slen <- which(oneyrdek[,1] == mois[1] & oneyrdek[,2] == dek[1])-1
	elen <- 36-which(oneyrdek[,1] == mois[length(mois)] & oneyrdek[,2] == dek[length(mois)])
	
	mat <- matrix(as.numeric(c(rep(NA, slen), xvar, rep(NA, elen))), nrow = 36)
	xvar <- as.numeric(apply(mat, 1, fun, na.rm = T))
	return(xvar)
}

climato.dekadal_lstOmat <- function(xvar, dates, fun = 'mean'){
	fun <- match.fun(fun)

	dates <- as.character(dates)
	an <- as.numeric(substr(dates, 1,4))
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

	xvar <- lapply(1:36, function(j) apply(simplify2array(xvar[seq(j, length(xvar), 36)]), 1:2, fun, na.rm = TRUE))
	rm(vmat, vmat1, vmat2, dates, an, mois, dek)
	return(xvar)
}

########

climato.daily <- function(xvar, dates, fun = 'mean'){
	fun <- match.fun(fun)

	dates <- as.character(dates)
	dates <- as.Date(dates, format='%Y%m%d')
	leap.year <- format(dates,'%m%d') == '0229'

	xvar <- xvar[!leap.year]
	dates <- dates[!leap.year]

	dstart <- seq(as.Date(paste(format(dates[1],'%Y'), 1,1, sep = '-')), dates[1],'day')
	dstart <- dstart[-length(dstart)]
	dend <- seq(dates[length(dates)], as.Date(paste(format(dates[length(dates)],'%Y'), 12,31, sep = '-')),'day')[-1]
	slen <- length(dstart[!format(dstart,'%m%d') == '0229'])
	elen <- length(dend[!format(dend,'%m%d') == '0229'])
	
	mat <- matrix(as.numeric(c(rep(NA, slen), xvar, rep(NA, elen))), nrow = 365)
	xvar <- apply(mat, 1, fun, na.rm = T)
	return(xvar)
}

climato.daily_lstOmat <- function(xvar, dates, fun = 'mean'){
	fun <- match.fun(fun)

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

	xvar <- lapply(1:365, function(j) apply(simplify2array(xvar[seq(j, length(xvar), 365)]), 1:2, fun, na.rm = TRUE))
	rm(dates, dstart, dend, vmat, vamt1, vmat2)
	return(xvar)
}

########################################################
daily2dekadal <- function(var, dates, fun, frac = 1){
	dates <- as.character(dates)
	fun <- match.fun(fun)
	an <- as.character(substr(dates, 1,4))
	mois <- as.character(substr(dates, 5,6))
	jour <- as.numeric(substr(dates, 7,8))
	nbs <- ifelse(jour[1] <= 10,10, ifelse(jour[1] > 10 & jour[1] <= 20,10, rev((28:31)[which(!is.na(as.Date(paste(an[1], mois[1], 28:31, sep = '-'))))])[1]-20))
	nbe <- ifelse(jour[length(an)] <= 10,10, ifelse(jour[length(an)] > 10 & jour[length(an)] <= 20,10, rev((28:31)[which(!is.na(as.Date(paste(an[length(an)], mois[length(an)], 28:31, sep = '-'))))])[1]-20))
	jour[jour <= 10] <- 1
	jour[jour > 10 & jour <= 20] <- 2
	jour[jour > 20] <- 3
	nbd <- rle(jour)$lengths
	nbd[1] <- nbs
	nbd[length(nbd)] <- nbe
	w <- ifelse(is.na(var), 0,1)
	nna <- aggregate(w, by = list(as.factor(paste(an, mois, jour, sep = ''))), sum, na.rm = T)[,2]
	rval <- as.data.frame(aggregate(var, by = list(as.factor(paste(an, mois, jour, sep = ''))), fun, na.rm = T))
	names(rval) <- c('Date', 'Values')
	rval[which((nna/nbd) < frac), 2] <- NA
	return(rval)
}
##
daily2monthly <- function(var, dates, fun, frac = 1){
	dates <- as.character(dates)
	fun <- match.fun(fun)
	an <- as.character(substr(dates, 1,4))
	mois <- as.character(substr(dates, 5,6))
	nbs <- rev((28:31)[which(!is.na(as.Date(paste(an[1], mois[1], 28:31, sep = '-'))))])[1]
	nbe <- rev((28:31)[which(!is.na(as.Date(paste(an[length(an)], mois[length(an)], 28:31, sep = '-'))))])[1]
	nbd <- rle(mois)$lengths
	nbd[1] <- nbs
	nbd[length(nbd)] <- nbe
	w <- ifelse(is.na(var), 0,1)
	nna <- aggregate(w, by = list(as.factor(paste(an, mois, sep = ''))), sum, na.rm = T)[,2]
	rval <- as.data.frame(aggregate(var, by = list(as.factor(paste(an, mois, sep = ''))), fun, na.rm = T))
	names(rval) <- c('Date', 'Values')
	rval[which((nna/nbd) < frac), 2] <- NA
	return(rval)
}
##
daily2yearly <- function(var, dates, fun, frac = 1){
	dates <- as.character(dates)
	fun <- match.fun(fun)
	an <- as.character(substr(dates, 1,4))
	nbs <- if(as.numeric(an[1])%%4 == 0) 366 else 365
	nbe <- if(as.numeric(an[length(an)])%%4 == 0) 366 else 365
	nbd <- rle(an)$lengths
	nbd[1] <- nbs
	nbd[length(nbd)] <- nbe
	w <- ifelse(is.na(var), 0,1)
	nna <- aggregate(w, by = list(as.factor(an)), sum, na.rm = T)[,2]
	rval <- as.data.frame(aggregate(var, by = list(as.factor(an)), fun, na.rm = T))
	names(rval) <- c('Date', 'Values')
	rval[which((nna/nbd) < frac), 2] <- NA
	return(rval)
}
##
dekadal2monthly <- function(var, dates, fun, frac = 1){
	dates <- as.character(dates)
	fun <- match.fun(fun)
	an <- as.character(substr(dates, 1,4))
	mois <- as.character(substr(dates, 5,6))
	nbd <- rle(mois)$lengths
	nbd[1] <- 3
	nbd[length(nbd)] <- 3
	w <- ifelse(is.na(var), 0,1)
	nna <- aggregate(w, by = list(as.factor(paste(an, mois, sep = ''))), sum, na.rm = T)[,2]
	rval <- as.data.frame(aggregate(var, by = list(as.factor(paste(an, mois, sep = ''))), fun, na.rm = FALSE))
	names(rval) <- c('Date', 'Values')
	rval[which((nna/nbd) < frac), 2] <- NA
	return(rval)
}
##
dekadal2yearly <- function(var, dates, fun, frac = 1){
	dates <- as.character(dates)
	fun <- match.fun(fun)
	an <- as.character(substr(dates, 1,4))
	nbd <- rle(an)$lengths
	nbd[1] <- 36
	nbd[length(nbd)] <- 36
	w <- ifelse(is.na(var), 0,1)
	nna <- aggregate(w, by = list(as.factor(an)), sum, na.rm = T)[,2]
	rval <- as.data.frame(aggregate(var, by = list(as.factor(an)), fun, na.rm = T))
	names(rval) <- c('Date', 'Values')
	rval[which((nna/nbd) < frac), 2] <- NA
	return(rval)
}
##
monthly2yearly <- function(var, dates, fun, frac = 1){
	dates <- as.character(dates)
	fun <- match.fun(fun)
	an <- as.character(substr(dates, 1, 4))
	nbd <- rle(an)$lengths
	nbd[1] <- 12
	nbd[length(nbd)] <- 12
	w <- ifelse(is.na(var), 0, 1)
	nna <- aggregate(w, by = list(as.factor(an)), sum, na.rm = T)[, 2]
	rval <- as.data.frame(aggregate(var, by = list(as.factor(an)), fun, na.rm = FALSE))
	names(rval) <- c('Date', 'Values')
	rval[which((nna/nbd) < frac), 2] <- NA
	return(rval)
}

###

seasonal_fun <- function(freq, var, dates, smon, lmon, fun, frac = 1){
	dates <- as.character(dates)
	fun <- match.fun(fun)
	an <- as.character(substr(dates, 1, 4))
	mois <- as.character(substr(dates, 5, 6))

	# im <- getMonthsInSeason1(smon, lmon)
	tmois <- format(ISOdate(2014, 1:12, 1), "%b")
	ix <- which(tmois == smon)
	im <- (ix:(ix+(lmon-1)))%%12
	im[im == 0] <- 12

	seas <- paste(substr(tmois, 1, 1)[im], collapse='')
	im <- ifelse(im < 10, paste(0, im, sep = ''), im)
	seasL <- mois%in%im
	rrL <- rle(seasL)
	nbd <- rrL$lengths[rrL$values]
	xsL <- str_pad(rep(1:length(nbd), times = nbd), width = nchar(length(nbd)), pad = '0')
	dtseas <- paste(seas, xsL, sep = '')

	var <- var[seasL]
	an <- sort(as.numeric(levels(as.factor(an[seasL]))))
	xtmp <- aggregate(var, by = list(as.factor(dtseas)), fun, na.rm = FALSE)[, 2]
	if(length(an) != length(xtmp)) an <- c(an[1]-1, an)
	rval <- data.frame(Season = seas, Start_Year = an, Values = xtmp)

	if(freq == 'daily'){
		eom <- sum(sapply(im, function(x) rev((28:31)[which(!is.na(as.Date(paste(2014, x, 28:31, sep = '-'))))])[1]))
		nbd[1] <- eom
		nbd[length(nbd)] <- eom
	}
	if(freq == 'dekadal'){
		nbd[1] <- 3*lmon
		nbd[length(nbd)] <- 3*lmon
	}
	if(freq == 'monthly'){
		nbd[1] <- lmon
		nbd[length(nbd)] <- lmon
	}

	w <- ifelse(is.na(var), 0,1)
	nna <- aggregate(w, by = list(as.factor(dtseas)), sum, na.rm = T)[, 2]
	rval[which((nna/nbd) < frac), 3] <- NA
	return(rval)
}

########################################################

seasonal_funMat <- function(freq, var, dates, smon, lmon, fun, frac = 1){
	dates <- as.character(dates)
	fun <- match.fun(fun)
	an <- as.character(substr(dates, 1,4))
	mois <- as.character(substr(dates, 5,6))

	# im <- getMonthsInSeason1(smon, lmon)
	tmois <- format(ISOdate(2014, 1:12, 1), "%b")
	ix <- which(tmois == smon)
	im <- (ix:(ix+(lmon-1)))%%12
	im[im == 0] <- 12

	seas <- paste(substr(tmois, 1,1)[im], collapse='')
	im <- ifelse(im < 10, paste(0, im, sep = ''), im)
	seasL <- mois%in%im
	rrL <- rle(seasL)
	nbd <- rrL$lengths[rrL$values]
	xsL <- str_pad(rep(1:length(nbd), times = nbd), width = nchar(length(nbd)), pad = '0')
	dtseas <- paste(seas, xsL, sep = '')

	an <- sort(as.numeric(levels(as.factor(an[seasL]))))
	var <- var[seasL,]
	xtmp <- aggregate(var, by = list(as.factor(dtseas)), fun, na.rm = FALSE)[,-1]

	if(length(an) != nrow(xtmp)) an <- c(an[1]-1, an)
	rval <- data.frame(paste(seas, an, sep = '-'), xtmp)

	if(freq == 'daily'){
		eom <- sum(sapply(im, function(x) rev((28:31)[which(!is.na(as.Date(paste(2014, x, 28:31, sep = '-'))))])[1]))
		nbd[1] <- eom
		nbd[length(nbd)] <- eom
	}
	if(freq == 'dekadal'){
		nbd[1] <- 3*lmon
		nbd[length(nbd)] <- 3*lmon
	}
	if(freq == 'monthly'){
		nbd[1] <- lmon
		nbd[length(nbd)] <- lmon
	}

	w <- ifelse(is.na(var), 0, 1)
	nna <- aggregate(w, by = list(as.factor(dtseas)), sum, na.rm = T)[, -1]
	rval0 <- rval[, -1]
	rval0[(nna/nbd) < frac] <- NA
	rval[, -1] <- rval0
	return(rval)
}

########################################################

daily2dekadal_lstOmat <- function(ListOfMatrix, dates, fun, minfrac){
	dates <- as.character(dates)
	fun <- match.fun(fun)
	nx <- nrow(ListOfMatrix[[1]])
	ny <- ncol(ListOfMatrix[[1]])

	an <- as.character(substr(dates, 1, 4))
	mois <- as.character(substr(dates, 5, 6))
	day <- as.character(substr(dates, 7, 8))
	jour <- as.numeric(day)

	jour[jour <= 10] <- 1
	jour[jour > 10 & jour <= 20] <- 2
	jour[jour > 20] <- 3

	nna <- as.data.frame(aggregate(rep(1, length(an)), by = list(as.factor(paste(an, mois, jour, sep = ''))),'sum', na.rm = FALSE))
	nbd <- nna[, 2]

	nbs <- ifelse(jour[1] <= 10, 10, ifelse(jour[1] > 10 & jour[1] <= 20, 10, rev((28:31)[which(!is.na(as.Date(paste(an[1], mois[1], 28:31, sep = '-'))))])[1]-20))
	nbe <- ifelse(jour[length(an)] <= 10, 10, ifelse(jour[length(an)] > 10 & jour[length(an)] <= 20, 10, rev((28:31)[which(!is.na(as.Date(paste(an[length(an)], mois[length(an)], 28:31, sep = '-'))))])[1]-20))
	nbd[1] <- nbs
	nbd[length(nbd)] <- nbe

	ret_lstOmat <- vector(mode = 'list', length = length(nna[, 1]))
	for(j in seq_along(nna[, 1])){
		if(nna[j, 2]/nbd[j] < minfrac){
			don <- matrix(NA, nrow = nx, ncol = ny)
		}else{
			xdates <- as.character(nna[j, 1])

			idx <- which(paste(an, mois, jour, sep = '')%in%xdates)

			vartmp <- array(unlist(ListOfMatrix[idx]), c(nx, ny, length(idx)))
			w <- vartmp
			w[!is.na(w)] <- 1
			w[is.na(w)] <- 0
			Nonmiss <- apply(w, c(1, 2), sum)
			don <- apply(vartmp, c(1, 2), fun, na.rm = T)
			don[Nonmiss/nbd[j] < minfrac] <- NA
		}
		ret_lstOmat[[j]] <- don
	}
	return(list(as.character(nna[, 1]), ret_lstOmat))
}

####################

daily2monthly_lstOmat <- function(ListOfMatrix, dates, fun, minfrac){
	dates <- as.character(dates)
	fun <- match.fun(fun)
	nx <- nrow(ListOfMatrix[[1]])
	ny <- ncol(ListOfMatrix[[1]])

	an <- as.character(substr(dates, 1, 4))
	mois <- as.character(substr(dates, 5, 6))

	nna <- as.data.frame(aggregate(rep(1, length(an)), by = list(as.factor(paste(an, mois, sep = ''))),'sum', na.rm = FALSE))
	nbd <- nna[, 2]

	nbs <- rev((28:31)[which(!is.na(as.Date(paste(an[1], mois[1], 28:31, sep = '-'))))])[1]
	nbe <- rev((28:31)[which(!is.na(as.Date(paste(an[length(an)], mois[length(an)], 28:31, sep = '-'))))])[1]
	nbd[1] <- nbs
	nbd[length(nbd)] <- nbe

	ret_lstOmat <- vector(mode = 'list', length = length(nna[,1]))
	for(j in seq_along(nna[,1])){
		if(nna[j, 2]/nbd[j] < minfrac){
			don <- matrix(NA, nrow = nx, ncol = ny)
		}else{
			xdates <- as.character(nna[j, 1])

			idx <- which(paste(an, mois, sep = '')%in%xdates)

			vartmp <- array(unlist(ListOfMatrix[idx]), c(nx, ny, length(idx)))
			w <- vartmp
			w[!is.na(w)] <- 1
			w[is.na(w)] <- 0
			Nonmiss <- apply(w, c(1,2), sum)
			don <- apply(vartmp, c(1,2), fun, na.rm = T)
			don[Nonmiss/nbd[j] < minfrac] <- NA
		}
		ret_lstOmat[[j]] <- don
	}
	return(list(as.character(nna[,1]), ret_lstOmat))
}


####################
daily2yearly_lstOmat <- function(ListOfMatrix, dates, fun, minfrac){
	dates <- as.character(dates)
	fun <- match.fun(fun)
	nx <- nrow(ListOfMatrix[[1]])
	ny <- ncol(ListOfMatrix[[1]])

	an <- as.character(substr(dates, 1,4))

	nna <- as.data.frame(aggregate(rep(1, length(an)), by = list(as.factor(an)),'sum', na.rm = FALSE))
	nbd <- nna[,2]

	nbs <- if(as.numeric(an[1])%%4 == 0) 366 else 365
	nbe <- if(as.numeric(an[length(an)])%%4 == 0) 366 else 365
	nbd[1] <- nbs
	nbd[length(nbd)] <- nbe

	ret_lstOmat <- vector(mode = 'list', length = length(nna[,1]))
	for(j in seq_along(nna[,1])){
		if(nna[j, 2]/nbd[j] < minfrac){
			don <- matrix(NA, nrow = nx, ncol = ny)
		}else{
			xdates <- as.character(nna[j, 1])

			idx <- which(an%in%xdates)

			vartmp <- array(unlist(ListOfMatrix[idx]), c(nx, ny, length(idx)))
			w <- vartmp
			w[!is.na(w)] <- 1
			w[is.na(w)] <- 0
			Nonmiss <- apply(w, c(1,2), sum)
			don <- apply(vartmp, c(1,2), fun, na.rm = T)
			don[Nonmiss/nbd[j] < minfrac] <- NA
		}
		ret_lstOmat[[j]] <- don
	}
	return(list(as.character(nna[,1]), ret_lstOmat))
}


####################


dekadal2monthly_lstOmat <- function(ListOfMatrix, dates, fun, minfrac){
	dates <- as.character(dates)
	fun <- match.fun(fun)
	nx <- nrow(ListOfMatrix[[1]])
	ny <- ncol(ListOfMatrix[[1]])

	an <- as.character(substr(dates, 1,4))
	mois <- as.character(substr(dates, 5,6))

	nna <- as.data.frame(aggregate(rep(1, length(an)), by = list(as.factor(paste(an, mois, sep = ''))),'sum', na.rm = FALSE))
	nbd <- 3

	ret_lstOmat <- vector(mode = 'list', length = length(nna[,1]))
	for(j in seq_along(nna[,1])){
		if(nna[j, 2]/nbd < minfrac){
			don <- matrix(NA, nrow = nx, ncol = ny)
		}else{
			xdates <- as.character(nna[j, 1])

			idx <- which(paste(an, mois, sep = '')%in%xdates)

			vartmp <- array(unlist(ListOfMatrix[idx]), c(nx, ny, length(idx)))
			w <- vartmp
			w[!is.na(w)] <- 1
			w[is.na(w)] <- 0
			Nonmiss <- apply(w, c(1,2), sum)
			don <- apply(vartmp, c(1,2), fun, na.rm = T)
			don[Nonmiss/nbd < minfrac] <- NA
		}
		ret_lstOmat[[j]] <- don
	}
	return(list(as.character(nna[,1]), ret_lstOmat))
}

####################nbd ----> nbd[j] rep(12, lengthnna[,1]) ,3,36,12...

dekadal2yearly_lstOmat <- function(ListOfMatrix, dates, fun, minfrac){
	dates <- as.character(dates)
	fun <- match.fun(fun)
	nx <- nrow(ListOfMatrix[[1]])
	ny <- ncol(ListOfMatrix[[1]])

	an <- as.character(substr(dates, 1,4))

	nna <- as.data.frame(aggregate(rep(1, length(an)), by = list(as.factor(an)),'sum', na.rm = FALSE))
	nbd <- 36

	ret_lstOmat <- vector(mode = 'list', length = length(nna[,1]))
	for(j in seq_along(nna[,1])){
		if(nna[j, 2]/nbd < minfrac){
			don <- matrix(NA, nrow = nx, ncol = ny)
		}else{
			xdates <- as.character(nna[j, 1])

			idx <- which(an%in%xdates)

			vartmp <- array(unlist(ListOfMatrix[idx]), c(nx, ny, length(idx)))
			w <- vartmp
			w[!is.na(w)] <- 1
			w[is.na(w)] <- 0
			Nonmiss <- apply(w, c(1,2), sum)
			don <- apply(vartmp, c(1,2), fun, na.rm = T)
			don[Nonmiss/nbd < minfrac] <- NA
		}
		ret_lstOmat[[j]] <- don
	}
	return(list(as.character(nna[,1]), ret_lstOmat))
}


###############

monthly2yearly_lstOmat <- function(ListOfMatrix, dates, fun, minfrac){
	dates <- as.character(dates)
	fun <- match.fun(fun)
	nx <- nrow(ListOfMatrix[[1]])
	ny <- ncol(ListOfMatrix[[1]])

	an <- as.character(substr(dates, 1,4))

	nna <- as.data.frame(aggregate(rep(1, length(an)), by = list(as.factor(an)),'sum', na.rm = FALSE))
	nbd <- 12

	ret_lstOmat <- vector(mode = 'list', length = length(nna[,1]))
	for(j in seq_along(nna[,1])){
		if(nna[j, 2]/nbd < minfrac){
			don <- matrix(NA, nrow = nx, ncol = ny)
		}else{
			xdates <- as.character(nna[j, 1])

			idx <- which(an%in%xdates)

			vartmp <- array(unlist(ListOfMatrix[idx]), c(nx, ny, length(idx)))
			w <- vartmp
			w[!is.na(w)] <- 1
			w[is.na(w)] <- 0
			Nonmiss <- apply(w, c(1,2), sum)
			don <- apply(vartmp, c(1,2), fun, na.rm = T)
			don[Nonmiss/nbd < minfrac] <- NA
		}
		ret_lstOmat[[j]] <- don
	}
	return(list(as.character(nna[,1]), ret_lstOmat))
}


####################seasonal
seasonal_lstOmat <- function(freq, ListOfMatrix, dates, smon, lmon, fun, minfrac = 1){
	dates <- as.character(dates)
	fun <- match.fun(fun)
	nx <- nrow(ListOfMatrix[[1]])
	ny <- ncol(ListOfMatrix[[1]])

	an <- as.character(substr(dates, 1, 4))
	mois <- as.character(substr(dates, 5, 6))

	# im <- getMonthsInSeason1(smon, lmon)
	tmois <- format(ISOdate(2014, 1:12, 1), "%b")
	ix <- which(tmois == smon)
	im <- (ix:(ix+(lmon-1)))%%12
	im[im == 0] <- 12

	seas <- paste(substr(tmois, 1, 1)[im], collapse = '')
	im <- ifelse(im < 10, paste(0, im, sep = ''), im)
	seasL <- mois%in%im
	rrL <- rle(seasL)
	nbd <- rrL$lengths[rrL$values]
	xsL <- rep(1:length(nbd), times = nbd)
	dtseas <- paste(seas, xsL, sep = '')
	ListOfMatrix <- ListOfMatrix[seasL]

	an <- sort(as.numeric(levels(as.factor(an[seasL]))))
	xsL <- sort(as.numeric(levels(as.factor(xsL))))
	if(length(an) != length(xsL)) an <- c(an[1]-1, an)
	nna <- data.frame(paste(seas, xsL, sep = ''), nbd, an)

	if(freq == 'daily'){
		eom <- sum(sapply(im, function(x) rev((28:31)[which(!is.na(as.Date(paste(2014, x, 28:31, sep = '-'))))])[1]))
		nbd[1] <- eom
		nbd[length(nbd)] <- eom
	}
	if(freq == 'dekadal'){
		nbd[1] <- 3*lmon
		nbd[length(nbd)] <- 3*lmon
	}
	if(freq == 'monthly'){
		nbd[1] <- lmon
		nbd[length(nbd)] <- lmon
	}

	ret_lstOmat <- vector(mode = 'list', length = length(nna[, 1]))
	for(j in seq_along(nna[, 1])){
		if(nna[j, 2]/nbd[j] < minfrac){
			don <- matrix(NA, nrow = nx, ncol = ny)
		}else{
			xdates <- as.character(nna[j, 1])
			idx <- which(dtseas%in%xdates)

			vartmp <- array(unlist(ListOfMatrix[idx]), c(nx, ny, length(idx)))
			w <- vartmp
			w[!is.na(w)] <- 1
			w[is.na(w)] <- 0
			Nonmiss <- apply(w, c(1, 2), sum)
			don <- apply(vartmp, c(1, 2), fun, na.rm = T)
			don[Nonmiss/nbd[j] < minfrac] <- NA
		}
		ret_lstOmat[[j]] <- don
	}
	return(list(seas, as.character(nna[, 3]), ret_lstOmat))
}


#######################################################
daily2dekadal_nc <- function(file.pars, informat, dates, fun, minfrac, varid.out, longname, outformat){
	dates <- as.character(dates)
	fun <- match.fun(fun)

	an <- as.character(substr(dates, 1,4))
	mois <- as.character(substr(dates, 5,6))
	day <- as.character(substr(dates, 7,8))
	jour <- as.numeric(day)

	nbs <- ifelse(jour[1] <= 10,10, ifelse(jour[1] > 10 & jour[1] <= 20,10, rev((28:31)[which(!is.na(as.Date(paste(an[1], mois[1], 28:31, sep = '-'))))])[1]-20))
	nbe <- ifelse(jour[length(an)] <= 10,10, ifelse(jour[length(an)] > 10 & jour[length(an)] <= 20,10, rev((28:31)[which(!is.na(as.Date(paste(an[length(an)], mois[length(an)], 28:31, sep = '-'))))])[1]-20))

	jour[jour <= 10] <- 1
	jour[jour > 10 & jour <= 20] <- 2
	jour[jour > 20] <- 3

	nna <- as.data.frame(aggregate(rep(1, length(an)), by = list(as.factor(paste(an, mois, jour, sep = ''))), 'sum', na.rm = FALSE))
	nna[1,2] <- nbs
	nna[length(nna[,1]), 2] <- nbe

	filein0 <- file.path(file.pars[1], sprintf(informat, an, mois, day), fsep = .Platform$file.sep)
	filein0 <- filein0[file.exists(filein0)][1]

	nc <- nc_open(filein0)
	missval <- nc$var[[1]]$missval
	units <- nc$var[[1]]$units
	dims <- nc$var[[1]]$dim
	nc_close(nc)
	nx <- dims[[1]]$len
	ny <- dims[[2]]$len

	grd <- ncvar_def(name = varid.out, units = units, dim = dims, missval = missval, longname = longname, prec = "short")

	for(j in seq_along(nna[,1])){
		#don <- matrix(missval, nrow = nx, ncol = ny)
		xdates <- as.character(nna[j, 1])
		idx <- which(paste(an, mois, jour, sep = '')%in%xdates)
		if(length(idx)/nna[j, 2] < minfrac){
			#don <- don
			next
		}else{
			filein <- sprintf(informat, an[idx], mois[idx], day[idx])
			ncfiles <- file.path(file.pars[1], filein, fsep = .Platform$file.sep)
			isFexist <- file.exists(ncfiles)
			if(sum(isFexist)/nna[j, 2] < minfrac){
				#don <- don
				next
			}else{
				ncfiles <- ncfiles[isFexist]
				vartmp <- lapply(seq_along(ncfiles), function(ik){
					nc <- nc_open(ncfiles[ik])
					vars <- ncvar_get(nc, varid = nc$var[[1]]$name)
					nc_close(nc)
					vars
				})

				vartmp <- array(unlist(vartmp), c(nx, ny, length(ncfiles)))
				w <- vartmp
				w[!is.na(w)] <- 1
				w[is.na(w)] <- 0
				Nonmiss <- apply(w, c(1,2), sum)
				don <- apply(vartmp, c(1,2), fun, na.rm = T)
				don[Nonmiss/nna[j, 2] < minfrac] <- missval
			}
		}
		fileout <- sprintf(outformat, substr(xdates, 1,4), substr(xdates, 5,6), substr(xdates, 7,7))
		ncfiles2 <- file.path(file.pars[2], fileout, fsep = .Platform$file.sep)
		nc2 <- nc_create(ncfiles2, grd)
		ncvar_put(nc2, grd, don)
		nc_close(nc2)
	}
}

##
daily2monthly_nc <- function(file.pars, informat, dates, fun, minfrac, varid.out, longname, outformat){
	dates <- as.character(dates)
	fun <- match.fun(fun)

	an <- as.character(substr(dates, 1, 4))
	mois <- as.character(substr(dates, 5, 6))
	day <- as.character(substr(dates, 7, 8))

	nbs <- rev((28:31)[which(!is.na(as.Date(paste(an[1], mois[1], 28:31, sep = '-'))))])[1]
	nbe <- rev((28:31)[which(!is.na(as.Date(paste(an[length(an)], mois[length(an)], 28:31, sep = '-'))))])[1]

	nna <- as.data.frame(aggregate(rep(1, length(an)), by = list(as.factor(paste(an, mois, sep = ''))),'sum', na.rm = FALSE))
	nna[1, 2] <- nbs
	nna[length(nna[, 1]), 2] <- nbe

	filein0 <- file.path(file.pars[1], sprintf(informat, an, mois, day), fsep = .Platform$file.sep)
	filein0 <- filein0[file.exists(filein0)][1]

	nc <- nc_open(filein0)
	missval <- nc$var[[1]]$missval
	units <- nc$var[[1]]$units
	dims <- nc$var[[1]]$dim
	nc_close(nc)
	nx <- dims[[1]]$len
	ny <- dims[[2]]$len
	grd <- ncvar_def(name = varid.out, units = units, dim = dims, missval = missval, longname = longname, prec = "short")

	for(j in seq_along(nna[,1])){
		#don <- matrix(missval, nrow = nx, ncol = ny)
		xdates <- as.character(nna[j, 1])
		idx <- which(paste(an, mois, sep = '')%in%xdates)
		if(length(idx)/nna[j, 2] < minfrac){
			#don <- don
			next
		}else{
			filein <- sprintf(informat, an[idx], mois[idx], day[idx])
			ncfiles <- file.path(file.pars[1], filein, fsep = .Platform$file.sep)
			isFexist <- file.exists(ncfiles)
			if(sum(isFexist)/nna[j, 2] < minfrac){
				#don <- don
				next
			}else{
				ncfiles <- ncfiles[isFexist]
				vartmp <- lapply(seq_along(ncfiles), function(ik){
					nc <- nc_open(ncfiles[ik])
					vars <- ncvar_get(nc, varid = nc$var[[1]]$name)
					nc_close(nc)
					vars
				})

				vartmp <- array(unlist(vartmp), c(nx, ny, length(ncfiles)))
				w <- vartmp
				w[!is.na(w)] <- 1
				w[is.na(w)] <- 0
				Nonmiss <- apply(w, c(1,2), sum)
				don <- apply(vartmp, c(1,2), fun, na.rm = T)
				don[Nonmiss/nna[j, 2] < minfrac] <- missval
			}
		}
		fileout <- sprintf(outformat, substr(xdates, 1,4), substr(xdates, 5,6))
		ncfiles2 <- file.path(file.pars[2], fileout, fsep = .Platform$file.sep)
		nc2 <- nc_create(ncfiles2, grd)
		ncvar_put(nc2, grd, don)
		nc_close(nc2)
	}
}


##
daily2yearly_nc <- function(file.pars, informat, dates, fun, minfrac, varid.out, longname, outformat){
	dates <- as.character(dates)
	fun <- match.fun(fun)

	an <- as.character(substr(dates, 1,4))
	mois <- as.character(substr(dates, 5,6))
	day <- as.character(substr(dates, 7,8))

	nbs <- if(as.numeric(an[1])%%4 == 0) 366 else 365
	nbe <- if(as.numeric(an[length(an)])%%4 == 0) 366 else 365

	nna <- as.data.frame(aggregate(rep(1, length(an)), by = list(as.factor(an)),'sum', na.rm = FALSE))
	nna[1,2] <- nbs
	nna[length(nna[,1]), 2] <- nbe

	filein0 <- file.path(file.pars[1], sprintf(informat, an, mois, day), fsep = .Platform$file.sep)
	filein0 <- filein0[file.exists(filein0)][1]

	nc <- nc_open(filein0)
	missval <- nc$var[[1]]$missval
	units <- nc$var[[1]]$units
	dims <- nc$var[[1]]$dim
	nc_close(nc)
	nx <- dims[[1]]$len
	ny <- dims[[2]]$len
	grd <- ncvar_def(name = varid.out, units = units, dim = dims, missval = missval, longname = longname, prec = "short")

	for(j in seq_along(nna[,1])){
		#don <- matrix(missval, nrow = nx, ncol = ny)
		xdates <- as.character(nna[j, 1])
		idx <- which(an%in%xdates)
		if(length(idx)/nna[j, 2] < minfrac){
			#don <- don
			next
		}else{
			filein <- sprintf(informat, an[idx], mois[idx], day[idx])
			ncfiles <- file.path(file.pars[1], filein, fsep = .Platform$file.sep)
			isFexist <- file.exists(ncfiles)
			if(sum(isFexist)/nna[j, 2] < minfrac){
				#don <- don
				next
			}else{
				ncfiles <- ncfiles[isFexist]
				vartmp <- lapply(seq_along(ncfiles), function(ik){
					nc <- nc_open(ncfiles[ik])
					vars <- ncvar_get(nc, varid = nc$var[[1]]$name)
					nc_close(nc)
					vars
				})

				vartmp <- array(unlist(vartmp), c(nx, ny, length(ncfiles)))
				w <- vartmp
				w[!is.na(w)] <- 1
				w[is.na(w)] <- 0
				Nonmiss <- apply(w, c(1,2), sum)
				don <- apply(vartmp, c(1,2), fun, na.rm = T)
				don[Nonmiss/nna[j, 2] < minfrac] <- missval
			}
		}
		fileout <- sprintf(outformat, xdates)
		ncfiles2 <- file.path(file.pars[2], fileout, fsep = .Platform$file.sep)
		nc2 <- nc_create(ncfiles2, grd)
		ncvar_put(nc2, grd, don)
		nc_close(nc2)
	}
}

###
dekadal2monthly_nc <- function(file.pars, informat, dates, fun, minfrac, varid.out, longname, outformat){
	dates <- as.character(dates)
	fun <- match.fun(fun)
	an <- as.character(substr(dates, 1,4))
	mois <- as.character(substr(dates, 5,6))
	dek <- as.numeric(substr(dates, 7,7))

	nna <- as.data.frame(aggregate(rep(1, length(an)), by = list(as.factor(paste(an, mois, sep = ''))),'sum', na.rm = FALSE))
	filein0 <- file.path(file.pars[1], sprintf(informat, an, mois, dek), fsep = .Platform$file.sep)
	filein0 <- filein0[file.exists(filein0)][1]

	nc <- nc_open(filein0)
	missval <- nc$var[[1]]$missval
	units <- nc$var[[1]]$units
	dims <- nc$var[[1]]$dim
	nc_close(nc)
	nx <- dims[[1]]$len
	ny <- dims[[2]]$len
	grd <- ncvar_def(name = varid.out, units = units, dim = dims, missval = missval, longname = longname, prec = "short")

	for(j in seq_along(nna[,1])){
		#don <- matrix(missval, nrow = nx, ncol = ny)
		xdates <- as.character(nna[j, 1])
		idx <- which(paste(an, mois, sep = '')%in%xdates)
		if(length(idx)/3 < minfrac){
			#don <- don
			next
		}else{
			filein <- sprintf(informat, an[idx], mois[idx], dek[idx])
			ncfiles <- file.path(file.pars[1], filein, fsep = .Platform$file.sep)
			isFexist <- file.exists(ncfiles)
			if(sum(isFexist)/3 < minfrac){
				#don <- don
				next
			}else{
				ncfiles <- ncfiles[isFexist]
				vartmp <- lapply(seq_along(ncfiles), function(ik){
					nc <- nc_open(ncfiles[ik])
					vars <- ncvar_get(nc, varid = nc$var[[1]]$name)
					nc_close(nc)
					vars
				})

				vartmp <- array(unlist(vartmp), c(nx, ny, length(ncfiles)))
				w <- vartmp
				w[!is.na(w)] <- 1
				w[is.na(w)] <- 0
				Nonmiss <- apply(w, c(1,2), sum)
				don <- apply(vartmp, c(1,2), fun, na.rm = T)
				don[Nonmiss/3 < minfrac] <- missval
			}
		}
		fileout <- sprintf(outformat, substr(xdates, 1,4), substr(xdates, 5,6))
		ncfiles2 <- file.path(file.pars[2], fileout, fsep = .Platform$file.sep)
		nc2 <- nc_create(ncfiles2, grd)
		ncvar_put(nc2, grd, don)
		nc_close(nc2)
	}
}

##
dekadal2yearly_nc <- function(file.pars, informat, dates, fun, minfrac, varid.out, longname, outformat){
	dates <- as.character(dates)
	fun <- match.fun(fun)
	an <- as.character(substr(dates, 1,4))
	mois <- as.character(substr(dates, 5,6))
	dek <- as.numeric(substr(dates, 7,7))

	nna <- as.data.frame(aggregate(rep(1, length(an)), by = list(as.factor(an)),'sum', na.rm = FALSE))
	filein0 <- file.path(file.pars[1], sprintf(informat, an, mois, dek), fsep = .Platform$file.sep)
	filein0 <- filein0[file.exists(filein0)][1]

	nc <- nc_open(filein0)
	missval <- nc$var[[1]]$missval
	units <- nc$var[[1]]$units
	dims <- nc$var[[1]]$dim
	nc_close(nc)
	nx <- dims[[1]]$len
	ny <- dims[[2]]$len
	grd <- ncvar_def(name = varid.out, units = units, dim = dims, missval = missval, longname = longname, prec = "short")

	for(j in seq_along(nna[,1])){
		#don <- matrix(missval, nrow = nx, ncol = ny)
		xdates <- as.character(nna[j, 1])
		idx <- which(an%in%xdates)
		if(length(idx)/36 < minfrac){
			#don <- don
			next
		}else{
			filein <- sprintf(informat, an[idx], mois[idx], dek[idx])
			ncfiles <- file.path(file.pars[1], filein, fsep = .Platform$file.sep)
			isFexist <- file.exists(ncfiles)
			if(sum(isFexist)/36 < minfrac){
				#don <- don
				next
			}else{
				ncfiles <- ncfiles[isFexist]
				vartmp <- lapply(seq_along(ncfiles), function(ik){
					nc <- nc_open(ncfiles[ik])
					vars <- ncvar_get(nc, varid = nc$var[[1]]$name)
					nc_close(nc)
					vars
				})

				vartmp <- array(unlist(vartmp), c(nx, ny, length(ncfiles)))
				w <- vartmp
				w[!is.na(w)] <- 1
				w[is.na(w)] <- 0
				Nonmiss <- apply(w, c(1,2), sum)
				don <- apply(vartmp, c(1,2), fun, na.rm = T)
				don[Nonmiss/36 < minfrac] <- missval
			}
		}
		fileout <- sprintf(outformat, xdates)
		ncfiles2 <- file.path(file.pars[2], fileout, fsep = .Platform$file.sep)
		nc2 <- nc_create(ncfiles2, grd)
		ncvar_put(nc2, grd, don)
		nc_close(nc2)
	}
}

##
monthly2yearly_nc <- function(file.pars, informat, dates, fun, minfrac, varid.out, longname, outformat){
	dates <- as.character(dates)
	fun <- match.fun(fun)
	an <- as.character(substr(dates, 1,4))
	mois <- as.character(substr(dates, 5,6))

	nna <- as.data.frame(aggregate(rep(1, length(an)), by = list(as.factor(an)),'sum', na.rm = FALSE))
	filein0 <- file.path(file.pars[1], sprintf(informat, an, mois), fsep = .Platform$file.sep)
	filein0 <- filein0[file.exists(filein0)][1]

	nc <- nc_open(filein0)
	missval <- nc$var[[1]]$missval
	units <- nc$var[[1]]$units
	dims <- nc$var[[1]]$dim
	nc_close(nc)
	nx <- dims[[1]]$len
	ny <- dims[[2]]$len
	grd <- ncvar_def(name = varid.out, units = units, dim = dims, missval = missval, longname = longname, prec = "short")

	for(j in seq_along(nna[,1])){
		#don <- matrix(missval, nrow = nx, ncol = ny)
		xdates <- as.character(nna[j, 1])
		idx <- which(an%in%xdates)
		if(length(idx)/12 < minfrac){
			#don <- don
			next
		}else{
			filein <- sprintf(informat, an[idx], mois[idx])
			ncfiles <- file.path(file.pars[1], filein, fsep = .Platform$file.sep)
			isFexist <- file.exists(ncfiles)
			if(sum(isFexist)/12 < minfrac){
				#don <- don
				next
			}else{
				ncfiles <- ncfiles[isFexist]
				vartmp <- lapply(seq_along(ncfiles), function(ik){
					nc <- nc_open(ncfiles[ik])
					vars <- ncvar_get(nc, varid = nc$var[[1]]$name)
					nc_close(nc)
					vars
				})

				vartmp <- array(unlist(vartmp), c(nx, ny, length(ncfiles)))
				w <- vartmp
				w[!is.na(w)] <- 1
				w[is.na(w)] <- 0
				Nonmiss <- apply(w, c(1,2), sum)
				don <- apply(vartmp, c(1,2), fun, na.rm = T)
				don[Nonmiss/12 < minfrac] <- missval
			}
		}
		fileout <- sprintf(outformat, xdates)
		ncfiles2 <- file.path(file.pars[2], fileout, fsep = .Platform$file.sep)
		nc2 <- nc_create(ncfiles2, grd)
		ncvar_put(nc2, grd, don)
		nc_close(nc2)
	}
}



################
NbdayInDekad <- function(var, dates, thres, lessthan = TRUE, frac = 1){
	dates <- as.character(dates)
	an <- as.character(substr(dates, 1,4))
	mois <- as.character(substr(dates, 5,6))
	jour <- as.numeric(substr(dates, 7,8))
	nbs <- ifelse(jour[1] <= 10,10, ifelse(jour[1] > 10 & jour[1] <= 20,10, rev((28:31)[which(!is.na(as.Date(paste(an[1], mois[1], 28:31, sep = '-'))))])[1]-20))
	nbe <- ifelse(jour[length(an)] <= 10,10, ifelse(jour[length(an)] > 10 & jour[length(an)] <= 20,10, rev((28:31)[which(!is.na(as.Date(paste(an[length(an)], mois[length(an)], 28:31, sep = '-'))))])[1]-20))
	jour[jour <= 10] <- 1
	jour[jour > 10 & jour <= 20] <- 2
	jour[jour > 20] <- 3
	nbd <- rle(jour)$lengths
	nbd[1] <- nbs
	nbd[length(nbd)] <- nbe
	w <- ifelse(is.na(var), 0,1)
	nna <- aggregate(w, by = list(as.factor(paste(an, mois, jour, sep = ''))), sum, na.rm = T)[,2]
	if(lessthan) var <- ifelse(var <= thres, 1,0)
	else var <- ifelse(var >= thres, 1,0)
	rval <- as.data.frame(aggregate(var, by = list(as.factor(paste(an, mois, jour, sep = ''))), sum, na.rm = T))
	names(rval) <- c('Date', 'Values')
	rval[which((nna/nbd) < frac), 2] <- NA
	return(rval)
}

NbdayInMonth <- function(var, dates, thres, lessthan = TRUE, frac = 1){
	dates <- as.character(dates)
	an <- as.character(substr(dates, 1,4))
	mois <- as.character(substr(dates, 5,6))
	nbs <- rev((28:31)[which(!is.na(as.Date(paste(an[1], mois[1], 28:31, sep = '-'))))])[1]
	nbe <- rev((28:31)[which(!is.na(as.Date(paste(an[length(an)], mois[length(an)], 28:31, sep = '-'))))])[1]
	nbd <- rle(mois)$lengths
	nbd[1] <- nbs
	nbd[length(nbd)] <- nbe
	w <- ifelse(is.na(var), 0,1)
	nna <- aggregate(w, by = list(as.factor(paste(an, mois, sep = ''))), sum, na.rm = T)[,2]
	if(lessthan) var <- ifelse(var <= thres, 1,0)
	else var <- ifelse(var >= thres, 1,0)
	rval <- as.data.frame(aggregate(var, by = list(as.factor(paste(an, mois, sep = ''))), sum, na.rm = T))
	names(rval) <- c('Date', 'Values')
	rval[which((nna/nbd) < frac), 2] <- NA
	return(rval)
}

NbdayInYear <- function(var, dates, thres, lessthan = TRUE, frac = 1){
	dates <- as.character(dates)
	an <- as.character(substr(dates, 1,4))
	nbs <- if(as.numeric(an[1])%%4 == 0) 366 else 365
	nbe <- if(as.numeric(an[length(an)])%%4 == 0) 366 else 365
	nbd <- rle(an)$lengths
	nbd[1] <- nbs
	nbd[length(nbd)] <- nbe
	w <- ifelse(is.na(var), 0,1)
	nna <- aggregate(w, by = list(as.factor(an)), sum, na.rm = T)[,2]
	if(lessthan) var <- ifelse(var <= thres, 1,0)
	else var <- ifelse(var >= thres, 1,0)
	rval <- as.data.frame(aggregate(var, by = list(as.factor(an)), sum, na.rm = T))
	names(rval) <- c('Date', 'Values')
	rval[which((nna/nbd) < frac), 2] <- NA
	return(rval)
}


mergeQcValues <- function(x, y){
	y[!is.na(x)] <- x[!is.na(x)]
	return(y)
}

###convert km to degree
km2deg <- function(dkm, lat){
	return(dkm*sqrt((1/110.54)^2+(1/(111.32*cos(lat*pi/180)))^2)/sqrt(2))
}
#######quarter repartition
repartition.check1 <- function(x0, y0, x, y){
	ang <- ifelse(sign(x-x0) >= 0, atan((y-y0)/(x-x0)), atan((y-y0)/(x-x0))+pi)
	id1 <- which(ang >= 0 & ang < pi/2) #I
	id2 <- which(ang >= pi/2 & ang < pi) #II
	id3 <- which(ang >= pi & ang < 3*pi/2) #III
	id4 <- which(ang >= -pi/2 & ang < 0) #IV
	quadrant <- length(id1) > 0 & length(id2) > 0 & length(id3) > 0 & length(id4) > 0
	return(quadrant)
}

###
repartCheck <- function(x0, y0, x, y){
	ang <- ifelse(sign(x-x0) >= 0, atan((y-y0)/(x-x0)), atan((y-y0)/(x-x0))+pi)
	ang <- ifelse(ang < 0, ang+2*pi, ang)

	#4 part
	id1 <- which(ang >= 0 & ang < pi/2) #I
	id2 <- which(ang >= pi/2 & ang < pi) #II
	id3 <- which(ang >= pi & ang < 3*pi/2) #III
	id4 <- which(ang >= 3*pi/2 & ang <= 2*pi) #IV
	#quadrant <- length(id1) > 0 & length(id2) > 0 & length(id3) > 0 & length(id4) > 0

	comb <- t(combn(1:4,3))
	strx <- paste("length(id", comb[,1], ")>0 & length(id", comb[,2], ")>0 & length(id", comb[,3], ")>0", sep = '')
	is3by4 <- as.vector(sapply(strx, function(x) eval(parse(text = x))))
	quadrant <- sum(is3by4) > 0

	#ramener 1er points to 0
	ang<-(ang-ang[1])%%(2*pi)

	##Above
	id1a <- which(ang >= 0 & ang < pi/3) #I
	id2a <- which(ang >= pi/3 & ang < 2*pi/3) #II
	id3a <- which(ang >= 2*pi/3 & ang <= pi) #III
	above <- length(id1a) > 0 & length(id2a) > 0 & length(id3a) > 0

	##Below
	ang <- ifelse(ang == 0,2*pi, ang)
	id1b <- which(ang >= pi & ang < 4*pi/3) #I
	id2b <- which(ang >= 4*pi/3 & ang < 5*pi/3) #II
	id3b <- which(ang >= 5*pi/3 & ang <= 2*pi) #III
	below <- length(id1b) > 0 & length(id2b) > 0 & length(id3b) > 0
	return(length(x) >= 4 & (quadrant | above | below))
}

#######################
##Arrange Tx, Tn
commonTxTn <- function(Tsdata){
	#take the same dates
	tx.dt <- Tsdata$tx.date%in%Tsdata$tn.date
	dates <- Tsdata$tx.date[tx.dt]
	tx.com <- Tsdata$tx.data[tx.dt]
	tn.dt <- Tsdata$tn.date%in%Tsdata$tx.date
	tn.com <- Tsdata$tn.data[tn.dt]
	return(list(tx = tx.com, tn = tn.com, date = dates))
}

#######################################
##Upper and lower bounds check
RainTempLimBounds <- function(x, xup, xlow){
	upp <- which(x > xup)
	lop <- which(x < xlow)
	y <- if(GeneralParameters$action == "qc.rain") x[x != 0] else x
	Q <- quantile(y, prob = c(0.0027,0.25,0.75,0.9973), names = F, na.rm = T)
	slop <- abs(x[lop]-Q[1])/(3*(Q[3]-Q[2]))
	supp<-(x[upp]-Q[4])/(3*(Q[3]-Q[2]))
	return(list(cbind(lop, slop), cbind(upp, supp)))
}

#######
limBoundsQc <- function(x, dates, xup, xlow){
	idLimBd <- RainTempLimBounds(x, xup, xlow)
	outQcLim <- matrix(NA, nrow = 0, ncol = 4)
	if(nrow(idLimBd[[1]])>0){
		datLimLow <- cbind(dates[idLimBd[[1]][,1]], x[idLimBd[[1]][,1]], 1, round(idLimBd[[1]][,2], 4))
		outQcLim <- rbind(outQcLim, datLimLow)
	}
	if(nrow(idLimBd[[2]])>0){
		datLimUp <- cbind(dates[idLimBd[[2]][,1]], x[idLimBd[[2]][,1]], 2, round(idLimBd[[2]][,2], 4))
		outQcLim <- rbind(outQcLim, datLimUp)
	}
	if(nrow(outQcLim) > 0) outQcLim <- data.frame(as.character(outQcLim[,1]), as.numeric(as.character(outQcLim[,2])),
	as.numeric(as.character(outQcLim[,3])), as.numeric(as.character(outQcLim[,4])))
	else outQcLim <- data.frame(NA, NA, NA, NA)
	names(outQcLim) <- c('dates', 'values', 'extreme.outlier', 'extreme.outlier.stat')
	idLm <- c(idLimBd[[1]][,1], idLimBd[[2]][,1])
	x[idLm] <- NA
	return(list(outQcLim, x))
}

##################################################################################################
quantilef <- function(x, prob){
	x <- x[!is.na(x) & x > 0]
	ret <- NULL
	#au moins 4 stations non zero
	if(length(x) > 3) ret <- quantile(x, prob = prob, names = F)
	return(ret)
}

###
quantilef1 <- function(x, prob, period){
	ret <- NA
	if(period == 'daily') minMonNonNA <- 30
	if(period == 'dekadal') minMonNonNA <- 10
	if(period == 'monthly') minMonNonNA <- 5
	if(length(x) >= minMonNonNA) ret <- quantile(x, prob = prob, names = F)
	return(ret)
}

########

indepStationParams <- function(jst, donne, months, period){
	#function(vstn, months, period) #apply(donne, 2, indepStationParams,...) comment vstn <- donne[,jst]
	vstn <- donne[,jst]
	iusx <- which(!is.na(vstn) & vstn > 0.5)
	mox <- months[iusx]
	varx <- vstn[iusx]
	ispmax <- data.frame(1:12, NA)
	if(length(varx) > 0){
		Qnt <- tapply(varx, list(as.factor(mox)), quantilef1, prob = 0.05, period = period)
		ispmax <- data.frame(as.numeric(names(Qnt)), Qnt)
	}
	names(ispmax) <- c('mo', 'ispmax')

	iusq <- which(!is.na(vstn) & vstn > 1)
	moq <- months[iusq]
	varq <- vstn[iusq]
	isdmin <- isdq1 <- data.frame(1:12, NA)
	if(length(varq) > 0){
		Qnt1 <- tapply(varq, list(as.factor(moq)), quantilef1, prob = c(0.1,0.25), period = period)
		monN <- as.numeric(names(Qnt1))
		isdmin <- data.frame(monN, sapply(Qnt1, function(x) x[1]))
		isdq1 <- data.frame(monN, sapply(Qnt1, function(x) x[2]))
	}
	names(isdmin) <- c('mo', 'isdmin')
	names(isdq1) <- c('mo', 'isdq1')

	tmp <- data.frame(mo = 1:12)
	tmp <- merge(tmp, ispmax, by = 'mo', all = T)
	tmp <- merge(tmp, isdmin, by = 'mo', all = T)
	tmp <- merge(tmp, isdq1, by = 'mo', all = T)
	#names(tmp) <- c('mo', 'ispmax', 'isdmin', 'isdq1')
	return(tmp)
}

######
depStationParams <- function(jst, idst, donne, months){
	donne <- as.matrix(donne)
	Qlu <- apply(donne[,idst[[jst]]], 1, quantilef, prob = c(0.05,0.75))
	inul <- unlist(lapply(Qlu, function(x) !is.null(x)))
	isdobs <- ispobs <- data.frame(1:12, NA)
	if(any(inul)){
		mo <- months[inul]
		Qlu <- Qlu[inul]
		Qlow <- sapply(Qlu, function(x) x[1])
		Qup <- sapply(Qlu, function(x) x[2])
		res <- tapply(Qlow, list(as.factor(mo)), median)
		isdobs <- cbind(expand.grid(dimnames(res)), c(res))
		res <- tapply(Qup, list(as.factor(mo)), median)
		ispobs <- cbind(expand.grid(dimnames(res)), c(res))
	}
	names(isdobs) <- c('mo', 'isdobs')
	names(ispobs) <- c('mo', 'ispobs')
	tmp <- data.frame(mo = 1:12)
	tmp <- merge(tmp, ispobs, by = 'mo', all = T)
	tmp <- merge(tmp, isdobs, by = 'mo', all = T)
	#names(tmp) <- c('mo', 'ispobs', 'isdobs')
	return(tmp)
}

######
removeSomeNA <- function(inRes, idmety, dists){
	inRes <- lapply(inRes, as.matrix)
	#faire la moyenne ponderee (distance) des 5 stations les plus proches, puis moyenne avec la station cible
	idstmety <- lapply(idmety, function(j){
		xdst <- dists[idmety, j]
		odst <- order(xdst)
		Xres <- inRes[odst[1:6]]
		wdist <- xdst[odst[2:6]]
		Xres0 <- Xres[-1]
		noNA <- lapply(Xres0, function(x) !is.na(x))
		denom <- Reduce('+',lapply(seq_along(noNA), function(l) noNA[[l]]*wdist[l]))
		Xw0 <- lapply(seq_along(Xres0), function(l){
			x <- Xres0[[l]]*wdist[l]
			x[is.na(x)] <- 0
			Xres0[[l]] <- x
		})
		Xw0 <- Reduce('+',Xw0)
		Xwtd <- Xw0/denom
		Xwtd[Xwtd == 0] <- NA
		Xloc <- Xres[[1]]
		Xwtd[is.na(Xwtd)] <- Xloc[is.na(Xwtd)]
		Xloc[is.na(Xloc)] <- Xwtd[is.na(Xloc)]
		(Xwtd+Xloc)/2
	})
####
	#change NA si 0 | NaN | Inf
	idstmety <- lapply(seq_along(idstmety), function(j){
		Xres <- idstmety[[j]]
		Xres[Xres == 0] <- NA
		Xres[is.nan(Xres)] <- NA
		Xres[is.infinite(Xres)] <- NA
		Xres
	})
#####
	#remplacer NA avec les valeur des stations voisines si possible (distance max < 60km)
	nlmety <- length(idstmety)
	toutNa <- lapply(idstmety, is.na)
	idttNa <- which(sapply(toutNa, sum) > 0)
	if(length(idttNa) > 0){
		idstmety1 <- lapply(idttNa, function(j){
			aReplaceNa <- toutNa[[j]]
			nyVal <- idstmety[[j]]
			xdst <- dists[idmety, idmety[j]]
			odst <- order(xdst)
			k <- 2
			while(sum(aReplaceNa) > 0 & xdst[odst][k] < 60 & k < nlmety){
				toReplace <- idstmety[[odst[k]]]
				nyVal[aReplaceNa] <- toReplace[aReplaceNa]
				aReplaceNa <- is.na(nyVal)
				k <- k+1
			}
			nyVal
		})
		idstmety[idttNa] <- idstmety1
	}
	return(idstmety)
}

########
###IDW lon, lat, month
getRainSpatInitCond <- function(xlon, xlat, xval, newgrid){
	datz <- data.frame(x = rep(xlon, 12), y = rep(xlat, 12), m = rep(1:12, each = nrow(xval)), z = c(xval))
	datz <- datz[!is.na(datz$z),]
	coordinates(datz)<- ~x+y+m
	res <- krige(z~1, locations = datz, newdata = newgrid, nmax = 3, debug.level = 0)
	res <- res$var1.pred
	res <- ifelse(res < 0.5,0.5, round(res, 1))
	matrix(res, ncol = 12)
}

####################

getRainInitParams <- function(donne, period){
	lon <- donne$lon
	lat <- donne$lat
	idStn <- donne$id
	dates <- donne$dates
	donne <- donne$data
	months <- as.numeric(substr(dates, 5,6))

	##distance matrix in km
	coord <- matrix(c(lon, lat), ncol = 2)
	dists <- rdist.earth(coord, miles = FALSE)
	##stations voisine dans un rayon de 50 km
	idst <- apply(dists, 2, function(x) which(x < 50 & x > 0.0001))

	####
	ijall <- 1:length(idStn)
	neiL <- sapply(idst, length)
	##minimum 4 stations voisines
	ijs4 <- which(neiL > 3)
	##3/4 des quadrants(pi/2) ou 3/3 des secteurs(pi/3) a 180deg
	ijsp <- sapply(ijs4, function(jst){
		ij <- idst[[jst]]
		repartCheck(lon[jst], lat[jst], lon[ij], lat[ij])
	})
	ijok <- ijs4[ijsp]

	################################

	if(nb_cores>1){
		clust <- makeCluster(nb_cores, type = "PSOCK")  #"FORK" linux/mac #"PSOCK" windows
		clusterExport(clust, varlist = c("quantilef", "quantilef1", "indepStationParams", "depStationParams",
		"idst", "donne", "months", "period"), envir = environment())
		indepRes <- parLapply(clust, ijall, function(jst) indepStationParams(jst, donne, months, period))
		depRes <- parLapply(clust, ijok, function(jst) depStationParams(jst, idst, donne, months))
		stopCluster(clust)
	}else{
		indepRes <- lapply(ijall, function(jst) indepStationParams(jst, donne, months, period))
		depRes <- lapply(ijok, function(jst) depStationParams(jst, idst, donne, months))
	}

	##############################
	##prend tout ce qui sont complet i.e matrix of 4 column
	indepID <- which(sapply(indepRes, function(x) sum(!is.na(x[,-1]))>0))
	indepRes <- indepRes[indepID]

	indepRes <- removeSomeNA(indepRes, indepID, dists)

	###############
	depID <- which(sapply(depRes, function(x) sum(!is.na(x[,-1]))>0))
	depRes <- depRes[depID]
	depID <- ijok[depID]

	depRes <- removeSomeNA(depRes, depID, dists)

	########################################
	deplon <- lon[depID]
	deplat <- lat[depID]
	ispobs <- t(sapply(depRes, function(x) x[,2]))
	isdobs <- t(sapply(depRes, function(x) x[,3]))

	indeplon <- lon[indepID]
	indeplat <- lat[indepID]
	ispmax <- t(sapply(indepRes, function(x) x[,2]))
	isdmin <- t(sapply(indepRes, function(x) x[,3]))
	isdq1 <- t(sapply(indepRes, function(x) x[,4]))

	###
	newgrid <- data.frame(x = rep(lon, 12), y = rep(lat, 12), m = rep(1:12, each = length(lon)))
	coordinates(newgrid)<- ~x+y+m

	ispmax <- getRainSpatInitCond(indeplon, indeplat, ispmax, newgrid)
	isdmin <- getRainSpatInitCond(indeplon, indeplat, isdmin, newgrid)
	isdq1 <- getRainSpatInitCond(indeplon, indeplat, isdq1, newgrid)
	ispobs <- getRainSpatInitCond(deplon, deplat, ispobs, newgrid)
	isdobs <- getRainSpatInitCond(deplon, deplat, isdobs, newgrid)

	nom <- c('Station ID', format(ISOdate(2014,1:12,1), "%b"))
	ispmax <- data.frame(idStn, ispmax)
	names(ispmax) <- nom
	isdmin <- data.frame(idStn, isdmin)
	names(isdmin) <- nom
	isdq1 <- data.frame(idStn, isdq1)
	names(isdq1) <- nom
	ispobs <- data.frame(idStn, ispobs)
	names(ispobs) <- nom
	isdobs <- data.frame(idStn, isdobs)
	names(isdobs) <- nom
	ftldev <- data.frame(Month = format(ISOdate(2014,1:12,1), "%B"), ftldev = rep(3.0,12))
	spatparam <- list(ispmax = ispmax, ispobs = ispobs, isdmin = isdmin, isdobs = isdobs, isdq1 = isdq1, ftldev = ftldev)

	return(spatparam)
}

###############

getRainInitParams0 <- function(donne, freqdata){
	lon <- donne$lon
	lat <- donne$lat
	idStn <- donne$id
	# donne <- donne$data

	# #####
	# limUp <- apply(donne, 2, function(x){
	# 	x <- x[!is.na(x) & x > 0]
	# 	Q <- quantile(x, prob = c(0.25,0.75,0.9973), names = F)
	# 	round(Q[3]+3*(Q[2]-Q[1]))
	# })
	# limUp <- as.vector(limUp)
	# limUp <- ifelse(is.na(limUp), max(limUp, na.rm = T), limUp)
	# limControl <- data.frame(idStn, 0, limUp, lon, lat)

	if(freqdata == "daily") limUp <- 300
	if(freqdata == "dekadal") limUp <- 1000
	if(freqdata == "monthly") limUp <- 3000
	limControl <- data.frame(idStn, 0, limUp, lon, lat)


	names(limControl) <- c('Station ID', 'Lower Bounds', 'Upper Bounds', 'Lon', 'Lat')
	return(limControl)
}



###############
getTempInitParams <- function(donne){
	lon <- donne$lon
	lat <- donne$lat
	idStn <- donne$id
	# donne <- donne$data

	# limUL <- apply(donne, 2, function(x){
	# 	x <- x[!is.na(x)]
	# 	Q <- quantile(x, prob = c(0.0027,0.25,0.75,0.9973), names = F)
	# 	c(round(Q[1]-3*(Q[3]-Q[2])), round(Q[4]+3*(Q[3]-Q[2])))
	# })
	# limUL <- t(limUL)
	# limLo <- ifelse(is.na(limUL[,1]), min(limUL[,1], na.rm = T), limUL[,1])
	# limLo <- as.vector(limLo)
	# limUp <- ifelse(is.na(limUL[,2]), max(limUL[,2], na.rm = T), limUL[,2])
	# limUp <- as.vector(limUp)
	limLo<- -40
	limUp <- 60
	limControl <- data.frame(idStn, limLo, limUp, lon, lat)
	names(limControl) <- c('Station ID', 'Lower Bounds', 'Upper Bounds', 'Lon', 'Lat')
	return(limControl)
}



########
#enableJIT(3)
#merge.qc.val <- cmpfun(merge.qc.val)
#repartCheck <- cmpfun(repartCheck)
#common.txtn <- cmpfun(common.txtn)
#limit.bounds <- cmpfun(limit.bounds)
#limit.bounds.qc <- cmpfun(limit.bounds.qc)
#quantilef <- cmpfun(quantilef)
#quantilef1 <- cmpfun(quantilef1)
#indepStationParams <- cmpfun(indepStationParams)
#depStationParams <- cmpfun(depStationParams)
#removeSomeNA <- cmpfun(removeSomeNA)
#getRainSpatInitCond <- cmpfun(getRainSpatInitCond)
#getRainInitParams <- cmpfun(getRainInitParams)
#getTempInitParams <- cmpfun(getTempInitParams)


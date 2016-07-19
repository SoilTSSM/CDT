
######
#Gamma upper limit
rainGammaTest <- function(x, alpha, limsup){
	###Wilson & Hilferty transformation
	n <- length(x)
	ret <- NULL
	if(n > 5){
		# Y<-(x/n)^(1/3)
		# Y1 <- Y[x < limsup]
		# f <- qt(1-(1-alpha)/2, df = length(Y1)-1)
		# q50 <- quantile(Y1, prob = 0.5)
		# iqr <- IQR(Y1)
		# if(iqr > 0){
		# 	stats<-(Y-q50)/(f*iqr)
		# 	test <- which(stats > 1)
		# 	ret <- cbind(test, stats[test])
		# }

		# muy <- sd(Y1)  #muy <- 1-2/(9*n)
		# sdy <- mean(Y1)  #sdy <- sqrt(2/(9*n))
		# if(sdy > 0){
		# 	stats<-(Y-muy)/(f*sdy)
		# 	test <- which(stats > 1)
		# 	ret <- cbind(test, stats[test])
		# }

		Y <- x^(1/3)
		Y1 <- Y[x < limsup]
		muy <- mean(Y1)
		sdy <- sd(Y1)
		if(sdy > 0){
			me <- qt(1-(1-alpha), df = length(Y1)-1)*sdy*sqrt(1+(1/length(Y1)))
			stats <- x/(muy+me)^3
			test <- which(stats > 1)	
			ret <- cbind(test, stats[test])
		}
	}
	return(ret)
}


#########
ConfIntGammaQc <- function(x, dates, thres, limsup, period){
	alpha <- thres/100
	if(period == 'daily') nonzeroL <- 30 #at least 30 days of non zero values
	if(period == 'dekadal') nonzeroL <- 9
	if(period == 'monthly') nonzeroL <- 5

	dates <- as.character(dates)
	idTs<-!is.na(x) & x > 0
	oId <- c(1:length(x))[idTs]
	dates <- dates[idTs]
	x <- x[idTs]
	id.out <- NULL
	if(length(x) > nonzeroL){
		dt <- as.numeric(substr(dates, 5,6))
		upLim <- tapply(x, list(as.factor(dt)), rainGammaTest, alpha = alpha, limsup = limsup)
		isnull <- sapply(upLim, function(x) is.null(x))
		upLim <- upLim[!isnull]
		nMo <- sapply(upLim, function(x) nrow(x))
		xMo <- as.numeric(names(nMo))
		xMo <- xMo[nMo > 0]
		nMo <- unname(nMo[nMo > 0])
		mo <- rep(xMo, nMo)
		ret <- do.call(rbind, upLim)
		id <- as.numeric(sapply(seq_along(mo), function(j) oId[dt == mo[j]][ret[j, 1]]))
		id.out <- cbind(id, ret[,2])
	}
	return(id.out)
}


##
UpperOutlierQc <- function(x, dates, thres, limsup, period){
	idCI <- ConfIntGammaQc(x, dates, thres, limsup, period)
	outQcCI <- data.frame(NA, NA, NA, NA)
	if(!is.null(idCI))  if(nrow(idCI) > 0) outQcCI <- data.frame(dates[idCI[,1]], x[idCI[,1]], 2, round(idCI[,2], 4))
	names(outQcCI) <- c('dates', 'values', 'upper.outlier', 'upper.outlier.stat')
	return(outQcCI)
}

###
rainQcSingleSeriesCalc <- function(x, idstn, dates, thres, limsup, period){
	#negative values
	neg <- which(x < 0)
	outQcLim <- data.frame(NA, NA, NA)
	if(length(neg) > 0){
		outQcLim <- data.frame(dates[neg], x[neg], 1)
		x[neg] <- NA
	}
	names(outQcLim) <- c('dates', 'values', 'negative.values')

	outQcCI <- UpperOutlierQc(x, dates, thres, limsup, period)
	outQc0 <- merge(outQcLim, outQcCI, by.x = 'dates', by.y = 'dates', all = T)
	outQc0 <- outQc0[!is.na(outQc0$dates),]

	if(nrow(outQc0) > 0){
		outQc0 <- data.frame(idstn, outQc0[,1], mergeQcValues(outQc0[,2], outQc0[,4]),
		outQc0[,3], outQc0[,5], outQc0[,6])
		ids <- order(as.character(outQc0[,2]))
		outQc0 <- outQc0[ids,]
	}else{
		outQc0 <- data.frame(NA, NA, NA, NA, NA, NA)
	}
	names(outQc0) <- c('stn', 'dates', 'values', 'negative.values', 'upper.outlier', 'upper.outlier.stat')
	return(outQc0)
}

######################
#Pure spatial check

rainSpatialCheck <- function(z0, z, spx, spthres, spatpar){
	ispmax <- spatpar[1]
	ispobs <- spatpar[2]
	isdmin <- spatpar[3]
	isdobs <- spatpar[4]
	isdq1 <- spatpar[5]
	ftldev <- spatpar[6]

	x0 <- spx[[1]][1]
	y0 <- spx[[1]][2]
	elv0 <- spx[[1]][3]
	lon <- spx[[2]][,1]
	lat <- spx[[2]][,2]

	id <- which(!is.na(z))
	flag <- c(-1, NA)
	if(length(id) >= spthres[1]){
		lon <- lon[id]
		lat <- lat[id]
		zNei <- z[id]
		if(!is.na(elv0)){
			elv <- spx[[2]][,3]
			elv <- elv[id]
			idELV <- which(abs(elv0-elv) < spthres[3])
			if(length(idELV) >= spthres[1]){
				zNei <- zNei[idELV]
				lon <- lon[idELV]
				lat <- lat[idELV]
			}
		}
		if(repartCheck(x0, y0, lon, lat)){
			mxNei <- max(zNei)
			mnNei <- min(zNei)
			zq25 <- quantile(zNei, 1/4, names = F)
			#zq50 <- quantile(zNei, 1/2, names = F)
			zq75 <- quantile(zNei, 3/4, names = F)
			iqr <- IQR(zNei)

			xz <- c(z0, zNei)
			iqr1 <- IQR(xz)
			stat0<-(z0-median(xz))/(ftldev*iqr1)
			ispr1 <- ispmax-mxNei
			ispr2 <- z0-ispobs
			isdr1 <- mnNei-isdmin
			isdr2 <- isdobs-2*z0
			isdr3 <- zq25-isdq1
			#atldv1<-(z0-zq50)/(2*ftldev*iqr)
			atldv1<-(z0-zq75)/(2*ftldev*iqr)
			atldv2 <- z0-ispobs
			#btldv1<-(z0-zq50)/(ftldev*iqr)
			btldv1<-(z0-zq25)/(ftldev*iqr)
			btldv2 <- isdr2

			statx1 <- abs(stat0)
			statx2 <- atldv1
			statx3 <- abs(btldv1)
			#a revoir
			# statx1 <- ifelse(statx1 > 2,2, statx1)
			# statx2 <- ifelse(statx2 > 2,2, statx2)
			# statx3 <- ifelse(statx3 > 2,2, statx3)
			flag <- c(0, max(statx1, statx2, statx3))
			if(iqr > 0){
				if(btldv1< -1 & btldv2 > 0) flag <- c(4, statx3)
				if(atldv1 > 1 & atldv2 > 0) flag <- c(3, statx2)
			}
			if(iqr1 > 0){
				if(isdr1 > 0 & isdr2 > 0 & isdr3 > 0 & stat0< -1) flag <- c(2, statx1)
				if(ispr1 > 0 & ispr2 > 0 & stat0 > 1) flag <- c(1, statx1)
			}
		}
	}
	return(flag)
}

#########
rainSpatialQc <- function(loopT, pos, var, dates, idstn, coords, elv, spthres, spparam){
	ispmax <- spparam[,1]
	ispobs <- spparam[,2]
	isdmin <- spparam[,3]
	isdobs <- spparam[,4]
	isdq1 <- spparam[,5]
	ftldev <- spparam[,6]
	lon <- coords[,1]
	lat <- coords[,2]

	xvar <- var[,pos]
	dtmo <- as.numeric(substr(dates, 5,6))
	out.qc1 <- data.frame(NA, NA, NA, NA, NA)
	if(sum(!is.na(xvar)) > 0 & var(xvar, na.rm = TRUE) > 0){
		loops <- NULL
		coordStn <- matrix(c(lon[pos], lat[pos]), ncol = 2)
		coordNei <- matrix(c(lon, lat), ncol = 2)
		dist <- as.numeric(rdist.earth(coordStn, coordNei, miles = FALSE))
		idR <- which(dist < spthres[2])
		idR <- idR[idR != pos]
		if(length(idR) >= spthres[1]){
			for(j in 1:12){
				mois <- dtmo == j
				IDm <- which(mois)
				#Inf
				ixinf <- IDm[which(xvar[mois] < isdobs[j])]
				tmpinf <- var[ixinf, idR]
				if(is.null(dim(tmpinf))) q05s <- as.vector(quantile(tmpinf, prob = 0.05, na.rm = T, names = F))
				else q05s <- as.vector(apply(tmpinf, 1, quantile, prob = 0.05, na.rm = T, names = F))
				loopmoninf <- ixinf[which(xvar[ixinf] < q05s)]
				##sup
				ixsup <- IDm[which(xvar[mois] > quantile(xvar[mois], prob = 0.99, na.rm = TRUE, names = F) & xvar[mois] > ispobs[j])]
				tmpsup <- var[ixsup, idR]
				if(is.null(dim(tmpsup))) q95s <- as.vector(quantile(tmpsup, prob = 0.95, na.rm = T, names = F))
				else q95s <- as.vector(apply(tmpsup, 1, quantile, prob = 0.95, na.rm = T, names = F))
				loopmonsup <- ixsup[which(xvar[ixsup] > q95s)]
				loops <- c(loops, c(loopmoninf, loopmonsup))
			}
		}

		loops <- c(loops, loopT)
		if(length(loops) > 0){
			loops <- loops[!duplicated(loops)]
			elv <- if(is.null(elv)) rep(NA, length(lon)) else elv
			spx <- list(c(lon[pos], lat[pos], elv[pos]), cbind(lon[idR], lat[idR], elv[idR]))

			ret <- lapply(loops, function(j){
				z <- as.vector(var[j,])
				res <- c(-1, NA)
				varz <- var(z[idR], na.rm = T)
				if(!is.na(varz) & varz > 0){
					jm <- dtmo[j]
					spatpar <- c(ispmax[jm], ispobs[jm], isdmin[jm], isdobs[jm], isdq1[jm], ftldev[jm])
					res <- rainSpatialCheck(z[pos], z[idR], spx, spthres, spatpar)
				}
				return(res)
			})

			ret <- do.call(rbind, ret)
			ret <- cbind(loops, ret)

			ii<-(ret[,1]%in%loopT) | (ret[,2] > 0)
			if(sum(ii) > 0){
				ret <- ret[ii,]
				if(is.null(dim(ret))) out.qc1 <- data.frame(idstn, dates[ret[1]], var[ret[1], pos], ret[2], ret[3])
				else out.qc1 <- data.frame(idstn, dates[ret[,1]], var[ret[,1], pos], ret[,2], ret[,3])
			}
		}
	}
	names(out.qc1) <- c('stn', 'dates', 'values', 'spatial.check', 'spatial.check.stat')
	return(out.qc1)
}

#########
rainQcSingleSeries <- function(dats, idstn, thres, limsup, period){
	x <- dats[[1]]
	dates <- dats[[2]]
	outQc <- rainQcSingleSeriesCalc(x, idstn, dates, thres, limsup, period)
	return(outQc)
}

######
rainQcSpatialCheck <- function(pos, idstn, dats, coords, elv, thres, spthres, spparam, limsup, period){
	var <- dats[[1]]
	dates <- dats[[2]]
	out.qc0 <- rainQcSingleSeriesCalc(var[,pos], idstn, dates, thres, limsup, period)
	outTs <- as.character(out.qc0$dates)
	outTs <- outTs[!is.na(outTs)]
	loopT <- NULL
	if(length(outTs) > 0) loopT <- which(dates%in%outTs)
	out.qc1 <- rainSpatialQc(loopT, pos, var, dates, idstn, coords, elv, spthres, spparam)
	out.qc <- merge(out.qc0, out.qc1, by.x = 'dates', by.y = 'dates', all = T)
	out.qc <- out.qc[!is.na(out.qc$dates),]

	if(nrow(out.qc) > 0){
		stat1 <- out.qc$upper.outlier.stat
		stat1 <- ifelse(is.na(stat1), 0, stat1)
		stat2 <- out.qc$spatial.check.stat
		stat2 <- ifelse(is.na(stat2), 0, stat2)
		stat<-(3*stat1+stat2)/4
		stat <- 3*(stat-min(stat))/(max(stat)-min(stat))
		stat <- ifelse(is.nan(stat), 3.0, stat)

		out.qc <- data.frame(idstn, out.qc$dates, mergeQcValues(out.qc$values.x, out.qc$values.y), out.qc$negative.values, out.qc$upper.outlier, out.qc$spatial.check)
		out.qc <- data.frame(out.qc, round(stat, 3))
		ids <- order(as.character(out.qc[,2]))
		out.qc <- out.qc[ids,]
	}else{
		out.qc <- data.frame(NA, NA, NA, NA, NA, NA, NA)
	}
	names(out.qc) <- c('stn', 'dates', 'values', 'negative.values', 'upper.outlier', 'spatial.check', 'statistic')
	return(out.qc)
}


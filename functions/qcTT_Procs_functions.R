###internal consistency check for tx & tn

intConsistCheck <- function(Tsdata, testpars){
	ret <- commonTxTn(Tsdata)
	rtest <- ret$tx < ret$tn
	rtest[is.na(rtest)] <- FALSE
	idtest <- which(rtest)

	if(testpars$tx.test){
		var <- Tsdata$tx.data
		dates <- Tsdata$tx.date
	}else{
		var <- Tsdata$tn.data
		dates <- Tsdata$tn.date
	}

	if(length(idtest) > 0){
		idt <- match(as.character(ret$date[idtest]), dates)
		dat <- data.frame(dates[idt], var[idt], 1) ##flag 1 (a changer)
		var[idt] <- NA
	}else dat <- data.frame(NA, NA, NA)
	return(list(out.qc = dat, var = var))
}

############################################################
#confidence limit
tempSigmaTest <- function(x, x0, alpha, limsup, period){
	if(period == 'daily') MinNonNA <- 30*5
	if(period == 'dekadal') MinNonNA <- 3*5
	if(period == 'monthly') MinNonNA <- 5

	x[x <= limsup[1] | x >= limsup[2]] <- NA
	x <- x[!is.na(x)]
	ret <- NULL
	if(length(x) < MinNonNA | is.na(var(x))) return(ret)
	f <- qnorm(1-((1-alpha)/2))
	q50 <- quantile(x, prob = 0.5)
	iqr <- IQR(x)
	if(iqr > 0){
		stats<-(x0-q50)/(f*iqr)
		testup <- which(stats > 1)
		testlow <- which(stats< -1)
		ret <- list(cbind(testlow, abs(stats[testlow])), cbind(testup, stats[testup]))
	}
	return(ret)
}

############################################################

confTxTnQc <- function(x, x0, dates, xparams, period){
	alpha <- xparams$thres/100
	limsup <- xparams$limsup

	idTs<-!is.na(x0)
	oId <- c(1:length(x0))[idTs]
	x0 <- x0[idTs]
	x <- x[idTs]
	dt <- as.numeric(substr(dates[idTs], 5,6))

	outTest <- lapply(1:12, function(j){
		ret <- tempSigmaTest(x[dt == j], x0[dt == j], alpha, limsup, period)
		ret1 <- ret2 <- NULL
		if(!is.null(ret)){
			low <- ret[[1]]
			if(nrow(low) > 0) ret1 <- cbind(oId[dt == j][low[,1]], low[,2])
			upp <- ret[[2]]
			if(nrow(upp) > 0) ret2 <- cbind(oId[dt == j][upp[,1]], upp[,2])
		}
		list(low = ret1, upp = ret2)
	})

	low <- do.call('rbind', lapply(outTest, function(x) x$low))
	upp <- do.call('rbind', lapply(outTest, function(x) x$upp))
	return(list(low = low, upp = upp))
}

############################################################

outlierTxTnQc <- function(x, Tsdata, xparams, testpars){
	if(testpars$tx.test){
		dates <- Tsdata$tx.date
		x0 <- Tsdata$tx.data
		if(is.null(x)) x <- x0
	}else{
		dates <- Tsdata$tn.date
		x0 <- Tsdata$tn.data
		if(is.null(x)) x <- x0
	}

	idCI <- confTxTnQc(x, x0, dates, xparams, testpars$period)
	outLow <- outUpp <- NULL
	if(!is.null(idCI$low)) outLow <- cbind(dates[idCI$low[,1]], x0[idCI$low[,1]], 1, round(idCI$low[,2], 4)) #flag 1 low
	if(!is.null(idCI$upp)) outUpp <- cbind(dates[idCI$upp[,1]], x0[idCI$upp[,1]], 2, round(idCI$upp[,2], 4)) #flag 2 upp
	outQcCI <- rbind(outLow, outUpp)

	if(!is.null(outQcCI)){
		outQcCI <- data.frame(outQcCI)
		outQcCI[,-1] <- apply(outQcCI[,-1], 2, as.numeric)
	}else outQcCI <- data.frame(NA, NA, NA, NA)
	names(outQcCI) <- c('dates', 'values', 'outlier.check', 'outlier.check.stat')
	return(outQcCI)
}

############################################################

txtnQcSingleSeriesCalc <- function(idstn, Tsdata, xparams, testpars){
	outQc0 <- data.frame(NA, NA, NA)
	if(testpars$int.check){
		resInt <- intConsistCheck(Tsdata, testpars)
		outQc0 <- resInt$out.qc
		x1 <- resInt$var
	}else x1 <- NULL
	names(outQc0) <- c('dates', 'values', 'tx_LT_tn.check')

	outQcCI <- outlierTxTnQc(x1, Tsdata, xparams, testpars)
	outQc1 <- merge(outQc0, outQcCI, by.x = 'dates', by.y = 'dates', all = T)
	outQc1 <- outQc1[!is.na(outQc1$dates),]
	if(nrow(outQc1) > 0){
		outQc1 <- data.frame(idstn, outQc1[,1], mergeQcValues(outQc1[,2], outQc1[,4]), outQc1[,3], outQc1[,5], outQc1[,6])
		ids <- order(as.character(outQc1[,2]))
		outQc1 <- outQc1[ids,]
	}else outQc1 <- data.frame(NA, NA, NA, NA, NA, NA)
	names(outQc1) <- c('stn', 'dates', 'values', 'tx_LT_tn.check', 'outlier.check', 'outlier.check.stat')
	return(outQc1)
}

############################################################

txtnChooseNeignbors <- function(xpos, XX, xparams){
	idNA <- as.logical(apply(XX, 2, function(x) (length(x[!is.na(x)])/(2*xparams$win+1)) > 0.8))
	##Distance
	coordStn <- matrix(c(xparams$coords[xpos, 1], xparams$coords[xpos, 2]), ncol = 2)
	coordNei <- matrix(c(xparams$coords[,1], xparams$coords[,2]), ncol = 2)
	dist <- as.numeric(rdist.earth(coordStn, coordNei, miles = FALSE))
	idR <- dist < xparams$spthres[2]
	id <- which(idNA & idR)
	id <- id[id != xpos]
	Y <- NULL
	if(length(id) >= xparams$spthres[1]){
		Y <- XX[,id]
		if(!is.null(xparams$elv)){
			idELV <- which(abs(xparams$elv[xpos]-xparams$elv[id])<xparams$spthres[3])
			if(length(idELV) >= xparams$spthres[1]) Y <- Y[,idELV]
		}
	}
	return(Y)
}

############################################################

txtnLinMod <- function(y, x, upos){
	md <- lm(x~y)
	reg <- summary(md)
	s2 <- mean(reg$residuals^2)
	xi <- predict.lm(md)
	id <- as.numeric(names(xi)) == upos
	if(any(id)) xi <- xi[id]
	else xi <- median(xi)
	return(c(xi, s2))
}

############################################################

txtnSpatReg <- function(xpos, upos, XX, xparams){
	Y <- txtnChooseNeignbors(xpos, XX, xparams)

	flag <- c(-1, NA, NA)
	if(!is.null(Y)){
		alpha <- xparams$spregpar[1]/100
		max.stn <- xparams$spregpar[2]
		f <- qnorm(1-((1-alpha)/2))
		rY <- apply(Y, 2, txtnLinMod, XX[,xpos], upos)
		rY <- rY[,round(rY[2,],5) != 0]
		if(!is.null(dim(rY))){
			if(ncol(rY) > max.stn){
				rY <- rY[,order(rY[2,])]
				rY <- rY[,1:max.stn]
			}

			if(ncol(rY) >= xparams$spthres[1]){
				xe <- sum(rY[1,]/rY[2,])/sum(1/rY[2,])
				se <- sqrt(ncol(rY)/sum(1/rY[2,]))
				stats<-(XX[upos, xpos]-xe)/(f*se)
				flag <- c(0, round(xe, 1), abs(stats)) #flag 0 value ok
				if(stats< -1) flag <- c(1, round(xe, 1), abs(stats)) #flag 1 low value
				if(stats > 1) flag <- c(2, round(xe, 1), abs(stats)) #flag 2 large value
			}
		}
	}
	return(flag)
}

############################################################

txtnSpatialQc <- function(loopT, xpos, idstn, Tsdata, xparams){
	ndt <- nrow(Tsdata$data)
	xvar <- Tsdata$data[,xpos]
	dt <- as.numeric(substr(Tsdata$date, 5,6))
	out.qc1 <- data.frame(NA, NA, NA, NA, NA, NA)
	if(sum(!is.na(xvar)) > 0 & var(xvar, na.rm = TRUE) > 0){
		loops <- NULL
		coordStn <- matrix(c(xparams$coords[xpos, 1], xparams$coords[xpos, 2]), ncol = 2)
		coordNei <- matrix(c(xparams$coords[,1], xparams$coords[,2]), ncol = 2)
		dist <- as.numeric(rdist.earth(coordStn, coordNei, miles = FALSE))
		idst <- which(dist < xparams$spthres[2])
		idst <- idst[idst != xpos]
		if(length(idst) >= xparams$spthres[1]){
			for(j in 1:12){
				mois <- dt == j
				IDm <- which(mois)
				ixinf <- IDm[which(xvar[mois] < quantile(xvar[mois], prob = 0.02, na.rm = T, names = F))]
				ixsup <- IDm[which(xvar[mois] > quantile(xvar[mois], prob = 0.98, na.rm = T, names = F))]
				loops <- c(loops, c(ixinf, ixsup))
			}
		}

		loops <- c(loops, loopT)
		if(length(loops) > 0){
			loops <- loops[!duplicated(loops)]

			resLoop <- lapply(loops, function(i){
				win <- xparams$win
				if(i<(win+1)){
					XX <- Tsdata$data[1:(2*win+1),]
					upos <- i
				}else if((i >= (win+1)) & (i <= (ndt-win))){
					XX <- Tsdata$data[(i-win):(i+win),]
					upos <- win+1
				}else{
					XX <- Tsdata$data[(ndt-2*win):ndt,]
					upos <- i-ndt+2*win+1
				}
				if(is.na(XX[upos, xpos])) return(NULL)
				#au moins 80% des donnees sont disponible pour la station cible pour le fenetre win --> 2*win+1
				if(sum(!is.na(XX[,xpos]))/(2*win+1) > 0.8) res <- txtnSpatReg(xpos, upos, XX, xparams)
				else res <- c(-1, NA, NA) #flag -1 no spatial check performed
				return(c(i, res))
			})

			resLoop <- do.call('rbind', resLoop)
			ii<-(resLoop[,1]%in%loopT) | (resLoop[,2] > 0)
			if(any(ii)){
				resLoop <- resLoop[ii,]
				if(is.null(dim(resLoop))) out.qc1 <- data.frame(idstn, Tsdata$date[resLoop[1]], xvar[resLoop[1]], resLoop[2], resLoop[3], resLoop[4])
				else out.qc1 <- data.frame(idstn, Tsdata$date[resLoop[,1]], xvar[resLoop[,1]], resLoop[,2], resLoop[,3], resLoop[,4])
			}
		}
	}
	names(out.qc1) <- c('stn', 'dates', 'values', 'spatial.reg.check', 'estimated.values', 'spatial.reg.stat')
	return(out.qc1)
}


############################################################

txtnQcSingleSeries <- function(idstn, Tsdata, xparams, testpars){
	outQc <- txtnQcSingleSeriesCalc(idstn, Tsdata, xparams, testpars)
	names(outQc) <- c('stn', 'dates', 'values', 'consistency.check', 'outlier.check', 'statistic')
	if(!testpars$int.check) outQc <- outQc[,-4]
	return(outQc)
}

############################################################

txtnQcSpatialCheck <- function(xpos, idstn, Tsdata, xparams, testpars){
	outQc0 <- txtnQcSingleSeriesCalc(idstn, Tsdata, xparams, testpars)
	outTs <- as.character(outQc0$dates)
	outTs <- outTs[!is.na(outTs)]
	loopT <- NULL
	if(length(outTs) > 0) loopT <- which(Tsdata$date%in%outTs)
	outQc1 <- txtnSpatialQc(loopT, xpos, idstn, Tsdata, xparams)
	outQc <- merge(outQc0, outQc1, by.x = 'dates', by.y = 'dates', all = T)
	outQc <- outQc[!is.na(outQc$dates),]
	if(nrow(outQc) > 0){

		stat1 <- outQc$outlier.check.stat
		stat1 <- ifelse(is.na(stat1), 0, stat1)
		stat2 <- outQc$spatial.reg.stat
		stat2 <- ifelse(is.na(stat2), 0, stat2)
		stat<-(2*stat1+stat2)/3
		stat <- 3*(stat-min(stat))/(max(stat)-min(stat))
		stat <- ifelse(is.nan(stat), 3.0, stat)

		outQc <- data.frame(idstn, outQc$dates, mergeQcValues(outQc$values.x, outQc$values.y), outQc$tx_LT_tn.check, outQc$outlier.check, outQc$spatial.reg.check, round(stat, 3), outQc$estimated.values)
		ids <- order(as.character(outQc[,2]))
		outQc <- outQc[ids,]
	}else{
		outQc <- data.frame(NA, NA, NA, NA, NA, NA, NA, NA)
	}
	names(outQc) <- c('stn', 'dates', 'values', 'consistency.check', 'outlier.check',	'spatial.reg.check', 'statistic', 'estimated.values')
	if(!testpars$int.check & testpars$omit) outQc <- outQc[,-4]
	return(outQc)
}

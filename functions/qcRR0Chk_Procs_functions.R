ZeroCheckStn <- function(pos, rr.dat, dates, coords, Zparams){
	max.dst <- Zparams[1]
	max.nbrs <- Zparams[2]
	min.nbrs <- Zparams[3]
	min.days <- Zparams[4]
	pct.trsh <- Zparams[5]
	lon <- coords$lon
	lat <- coords$lat

	coordStn <- matrix(c(lon[pos], lat[pos]), ncol = 2)
	coordNei <- matrix(c(lon, lat), ncol = 2)
	dist <- as.numeric(rdist.earth(coordStn, coordNei, miles = FALSE))
	ii <- dist <= max.dst
	ii[pos] <- FALSE
	out.data <- data.frame(matrix(NA, ncol = 7, nrow = 1))
	if(length(which(ii)) >= min.nbrs){
		rr.nei <- rr.dat[,ii]
		dst.nei <- dist[ii]
		rr.stn <- rr.dat[,pos]
		yymm <- substr(dates, 1,6)

		outzero <- lapply(levels(as.factor(yymm)), function(im){
			jmon <- yymm == im
			x <- rr.stn[jmon]
			x <- x[!is.na(x)]
			rr.nbrs <- rr.nei[jmon,]
			if(length(x) >= min.days){
				pct.zero.stn <- round(100*sum(x == 0, na.rm = T)/sum(x >= 0, na.rm = T))
				num.days2 <- as.numeric(apply(rr.nbrs, 2, function(x) sum(x >= 0, na.rm = T)))
				ix <- which(num.days2 >= min.days)
				n.nbrs <- length(ix)
				if(n.nbrs >= min.nbrs){
					dst.nbrs <- dst.nei[ix]
					rr.nbrs <- rr.nbrs[,ix]
					if(n.nbrs > max.nbrs) n.nbrs <- max.nbrs
					idst <- sort(dst.nbrs, index.return = T)
					dst.nbrs <- idst$x[1:n.nbrs]
					rr.nbrs <- rr.nbrs[,idst$ix][,1:n.nbrs]
					pct.zero.nbrs <- as.numeric(apply(rr.nbrs, 2, function(x) round(100*sum(x == 0, na.rm = T)/sum(x >= 0, na.rm = T))))

					ix <- which(pct.zero.nbrs == max(pct.zero.nbrs))
					if(length(ix) == n.nbrs) ix <- 1   #All stations have zero values #???
					pct.zero.nbrs <- pct.zero.nbrs[-ix] # Remove largest value #???
					dst.nbrs <- dst.nbrs[-ix] #???

					avg.pct <- round(mean(pct.zero.nbrs))
					#rng.nbrs <- max(pct.zero.nbrs) - min(pct.zero.nbrs)
					ix <- which(dst.nbrs == min(dst.nbrs))
					nrst.stn <- dst.nbrs[ix[1]]
					nrst.val <- pct.zero.nbrs[ix[1]]
					r.pct <- pct.zero.stn/(avg.pct+1)
					if((r.pct >= pct.trsh) & (pct.zero.stn > 60)) c(im, pct.zero.stn, nrst.val, round(nrst.stn, 1), avg.pct, max(pct.zero.nbrs))
				}
			}
		})
		outzero <- do.call('rbind', outzero)
		if(!is.null(outzero)){
			out.data <- cbind(coords$id[pos], outzero)
			out.data <- data.frame(out.data, stringsAsFactors = FALSE)
			out.data[,-(1:2)] <- apply(out.data[,-(1:2)], 2, as.numeric)
		}
	}
	names(out.data) <- c("ID", "YYYYMM", "val.stn", "nrst.val", "nrst.stn_km", "avg.nbrs", "max.nbrs")
	return(out.data)
}

###################################################################

QcOutZeroChkFormat <- function(){
	outputdir <- ReturnExecResults$outputdir
	if(ReturnExecResults$AllOrOne == 'one'){
		IJstation <- ReturnExecResults$station
	}
	if(ReturnExecResults$AllOrOne == 'all'){
		stns <- sapply(ReturnExecResults$res, function(x) x$station)
		ijstn <- which(stns == tclvalue(stn.choix.val))
		IJstation <- ReturnExecResults$res[[ijstn]]$station
	}

	fileout <- file.path(outputdir, IJstation, paste(IJstation,'.txt', sep = ''), fsep = .Platform$file.sep)
	if(!file.exists(fileout)) return(NULL)
	zcdat <- read.table(fileout, header = TRUE, colClasses = 'character')
	retdata <- list(zcdat, fileout)
	return(retdata)
}

###################################################################

replaceZeroChkbyNA <- function(IJstation, retRes){
	outputdir <- retRes$outputdir
	datadir <- retRes$datadir

	filein <- file.path(outputdir, IJstation, paste(IJstation,'.txt', sep = ''), fsep = .Platform$file.sep)
	if(!file.exists(filein)){
		InsertMessagesTxt(main.txt.out, paste(IJstation, 'not checked'), format = TRUE)
		return(NULL)
	}
	outstn <- read.table(filein, header = TRUE, colClasses = 'character')
	zcdaty <- as.character(outstn$YYYYMM)
	if(sum(!is.na(zcdaty)) == 0){
		InsertMessagesTxt(main.txt.out, paste(IJstation, 'OK! No data replaced'))
		return(1)
	}
	zcdaty <- zcdaty[!is.na(zcdaty)]
	zcdaty <- str_trim(zcdaty)
	notmonth <- which(nchar(zcdaty) != 6)
	if(length(notmonth) > 0){
		InsertMessagesTxt(main.txt.out, paste(paste(zcdaty[notmonth], collapse=';'),'wrong date format'), format = TRUE)
		return(NULL)
	}
	filein1 <- file.path(datadir, IJstation, paste(IJstation,'.txt', sep = ''), fsep = .Platform$file.sep)
	if(!file.exists(filein1)){
		InsertMessagesTxt(main.txt.out, paste(filein1, 'not found'), format = TRUE)
		return(NULL)
	}
	datstn <- read.table(filein1)
	irepl <- substr(as.character(datstn[,1]), 1,6)%in%zcdaty
	datstn[irepl & (!is.na(datstn[,2]) & datstn[,2] == 0), 2] <- NA
	write.table(datstn, filein1, col.names = FALSE, row.names = FALSE)
	return(0)
}

###################################################################

QcOutZeroChk_Neighbors <- function(IJstation,date_month){
	idstn <- EnvQcZeroChkData$donnees$id
	dates <- EnvQcZeroChkData$donnees$dates
	lon <- EnvQcZeroChkData$donnees$lon
	lat <- EnvQcZeroChkData$donnees$lat
	rr.dat <- EnvQcZeroChkData$donnees$data

	##
	pos <- which(idstn == IJstation)
	imon <- substr(dates, 1,6) == date_month

	coordStn <- matrix(c(lon[pos], lat[pos]), ncol = 2)
	coordNei <- matrix(c(lon, lat), ncol = 2)
	dist <- as.numeric(rdist.earth(coordStn, coordNei, miles = FALSE))
	ii <- dist <= as.numeric(GeneralParameters$param.zero$Values[4])
	ii[pos] <- FALSE
	rr.nei <- rr.dat[imon, ii]
	dst.nei <- dist[ii]
	id.nei <- idstn[ii]
	rr.stn <- rr.dat[imon, pos]

	num.days2 <- as.numeric(apply(rr.nei, 2, function(x) sum(x >= 0, na.rm = T)))
	ix <- which(num.days2 >= as.numeric(GeneralParameters$param.zero$Values[3]))
	dst.nei <- dst.nei[ix]
	rr.nei <- rr.nei[,ix]
	id.nei <- id.nei[ix]
	nmax.nei <- if(length(ix) > as.numeric(GeneralParameters$param.zero$Values[2])) as.numeric(GeneralParameters$param.zero$Values[2]) else length(ix)
	oo <- order(dst.nei)
	dst.nei <- dst.nei[oo]
	rr.nei <- rr.nei[,oo]
	id.nei <- id.nei[oo]
	dst.nei <- dst.nei[1:nmax.nei]
	rr.nei <- rr.nei[,1:nmax.nei]
	id.nei <- id.nei[1:nmax.nei]

	xdon <- cbind(c('Stations', 'Distance', dates[imon]), t(cbind(cbind(c(tclvalue(stn.choix.val), id.nei), c(0, round(dst.nei, 1))), t(cbind(rr.stn, rr.nei)))))
	xdon <- data.frame(xdon)
	names(xdon) <- NULL
	return(xdon)
}


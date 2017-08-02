
AggregateTS_Execute <- function(GeneralParameters){
	InsertMessagesTxt(main.txt.out, 'Aggregate time series ...')

	period <- GeneralParameters$in.tstep
	period1 <- GeneralParameters$out.tstep

	min.frac <- GeneralParameters$aggr.series$min.frac
	aggr.fun <- GeneralParameters$aggr.series$aggr.fun
	opr.fun <- GeneralParameters$aggr.series$opr.fun
	opr.thres <- GeneralParameters$aggr.series$opr.thres

	datatype <- GeneralParameters$data.type

	if(datatype == 'cdt'){
		donne <- getStnOpenData(GeneralParameters$stn.data)
		if(is.null(donne)) return(NULL)
		donneInfo <- getStnOpenDataInfo(GeneralParameters$stn.data)
		if(is.null(donneInfo)) return(NULL)
		donne <- getCDTdataAndDisplayMsg(donne, period)
		if(is.null(donne)) return(NULL)
		miss.val <- donneInfo[[3]]$miss.val
		dates <- donne$dates
	}
	if(datatype == 'netcdf'){
		istart.yrs <- GeneralParameters$Date.Range$start.year
		istart.mon <- GeneralParameters$Date.Range$start.mon
		istart.day <- GeneralParameters$Date.Range$start.day
		iend.yrs <- GeneralParameters$Date.Range$end.year
		iend.mon <- GeneralParameters$Date.Range$end.mon
		iend.day <- GeneralParameters$Date.Range$end.day

		dstart <- as.Date(paste(istart.yrs, istart.mon, istart.day, sep = '-'), format = '%Y-%m-%d')
		dend <- as.Date(paste(iend.yrs, iend.mon, iend.day, sep = '-'), format = '%Y-%m-%d')
		if(is.na(dstart)){
			InsertMessagesTxt(main.txt.out, "Wrong start date", format = TRUE)
			return(NULL)
		}
		if(is.na(dend)){
			InsertMessagesTxt(main.txt.out, "Wrong end date", format = TRUE)
			return(NULL)
		}

		if(period == "daily"){
			dates <- format(seq(dstart, dend, 'day'), '%Y%m%d')
			ncfiles <- sprintf(GeneralParameters$ncdf.data$format, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 8))
		}
		if(period == "pentad"){
			dates <- seq(dstart, dend, 'day')
			dates <- paste0(format(dates[which(as.numeric(format(dates, '%d')) <= 6)], '%Y%m'),
							as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 6)], '%d')))
			ncfiles <- sprintf(GeneralParameters$ncdf.data$format, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 7))
		}
		if(period == "dekadal"){
			dates <- seq(dstart, dend, 'day')
			dates <- paste0(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%Y%m'),
							as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%d')))
			ncfiles <- sprintf(GeneralParameters$ncdf.data$format, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 7))
		}
		if(period == "monthly"){
			dates <- format(seq(dstart, dend, 'month'), '%Y%m')
			ncfiles <- sprintf(GeneralParameters$ncdf.data$format, substr(dates, 1, 4), substr(dates, 5, 6))
		}

		ncPATH <- file.path(GeneralParameters$ncdf.data$dir, ncfiles)
		ncEXIST <- unlist(lapply(ncPATH, file.exists))
		if(!any(ncEXIST)){
			InsertMessagesTxt(main.txt.out, "Unable to locate netcdf files", format = TRUE)
			return(NULL)
		}

		ncsample <- getRFESampleData(GeneralParameters$ncdf.data$sample)
		if(is.null(ncsample)){
			InsertMessagesTxt(main.txt.out, "Netcdf data sample not found", format = TRUE)
			return(NULL)
		}
		ncINFO <- list(xo = ncsample$rfeILon, yo = ncsample$rfeILat, varid = ncsample$rfeVarid)
	}

	#########################
	## index dates


	if(period1 == "pentad"){
		yymm <- substr(dates, 1, 6)
		jour <- as.numeric(substr(dates, 7, 8))
		jour <- cut(jour, c(1, 5, 10, 15, 20, 25, 31), labels = FALSE, include.lowest = TRUE)
		index <- split(seq_along(dates), paste0(yymm, jour))
		nbd0 <- nbDayOfPentad(dates[1])
		nbd1 <- nbDayOfPentad(dates[length(dates)])
		odaty <- names(index)
	}

	if(period1 == "dekadal"){
		if(period == "daily"){
			yymm <- substr(dates, 1, 6)
			jour <- as.numeric(substr(dates, 7, 8))
			jour <- cut(jour, c(1, 10, 20, 31), labels = FALSE, include.lowest = TRUE)
			index <- split(seq_along(dates), paste0(yymm, jour))
			nbd0 <- nbDayOfDekad(dates[1])
			nbd1 <- nbDayOfDekad(dates[length(dates)])
		}
		if(period == "pentad"){
			yymm <- substr(dates, 1, 6)
			pen <- as.numeric(substr(dates, 7, 8))
			pen <- cut(pen, c(1, 2, 4, 6), labels = FALSE, include.lowest = TRUE)
			index <- split(seq_along(dates), paste0(yymm, pen))
			nbd0 <- 2
			nbd1 <- 2
		}
		odaty <- names(index)
	}

	if(period1 == "monthly"){
		index <- split(seq_along(dates), substr(dates, 1, 6))
		if(period == "daily"){
			nbd0 <- nbDayOfMonth(dates[1])
			nbd1 <- nbDayOfMonth(dates[length(dates)])
		}
		if(period == "pentad"){
			nbd0 <- 6
			nbd1 <- 6
		}
		if(period == "dekadal"){
			nbd0 <- 3
			nbd1 <- 3
		}
		odaty <- names(index)
	}

	if(period1 == "annual"){
		index <- split(seq_along(dates), substr(dates, 1, 4))
		if(period == "daily"){
			nbd0 <- nbDayOfYear(dates[1])
			nbd1 <- nbDayOfYear(dates[length(dates)])
		}
		if(period == "pentad"){
			nbd0 <- 72
			nbd1 <- 72
		}
		if(period == "dekadal"){
			nbd0 <- 36
			nbd1 <- 36
		}
		if(period == "monthly"){
			nbd0 <- 12
			nbd1 <- 12
		}
		odaty <- names(index)
	}

	if(period1 == "seasonal"){
		startMonth <- GeneralParameters$Seasonal$start.mon
		seasonLength <- GeneralParameters$Seasonal$length.mon
		monthtoAna <- (startMonth:(startMonth+(seasonLength-1)))%%12
		monthtoAna[monthtoAna == 0] <- 12

		if(seasonLength == 1){
			index <- split(seq_along(dates), substr(dates, 1, 6))
			if(period == "daily"){
				nbd0 <- nbDayOfMonth(dates[1])
				nbd1 <- nbDayOfMonth(dates[length(dates)])
			}
			if(period == "pentad"){
				nbd0 <- 6
				nbd1 <- 6
			}
			if(period == "dekadal"){
				nbd0 <- 3
				nbd1 <- 3
			}
			odaty <- names(index)
		}else{
			if(period == 'monthly'){
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
			if(period == 'daily') cdates <- format(cdates, "%Y%m%d")
			if(period == 'pentad'){
				dek <- as.numeric(format(cdates, '%d'))
				cdates <- paste0(format(cdates, "%Y%m")[dek <= 6], dek[dek <= 6])
			}
			if(period == 'dekadal'){
				dek <- as.numeric(format(cdates, '%d'))
				cdates <- paste0(format(cdates, "%Y%m")[dek <= 3], dek[dek <= 3])
			}
			if(period == 'monthly') cdates <- format(cdates, "%Y%m")

			idrow <- seq(length(dates))
			idrow <- idrow[match(cdates, dates)]

			itmp <- as.numeric(substr(cdates, 5, 6))%in%monthtoAna
			xrow <- idrow[itmp]
			xdaty <- cdates[itmp]

			##################

			xmois <- as.numeric(substr(xdaty, 5, 6))

			rleMois <- rle(xmois)
			xrank <- cumsum(rleMois$lengths)
			istart <- seq(which(rleMois$values %in% monthtoAna[1])[1], length(rleMois$values), seasonLength)
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

				if(period == 'daily'){
					nend <- mapply(function(an, mon) rev((28:31)[!is.na(as.Date(paste(an, mon, 28:31, sep = '-')))])[1],
											rep(as.numeric(substr(finSeas[length(finSeas)], 1, 4)), length(iex)), iex)
					nend <- sum(nend)
				}
				if(period == 'pentad') nend <- sum(length(iex)) * 6
				if(period == 'dekadal') nend <- sum(length(iex)) * 3
				if(period == 'monthly') nend <- sum(length(iex))

				iend[length(iend)] <- length(rleMois$lengths)
			}
			iend <- xrank[iend]
			index <- lapply(seq_along(istart), function(j) xrow[istart[j]:iend[j]])
			nbd0 <- length(index[[1]])
			nbd1 <- length(index[[length(index)]]) + nend
			index <- lapply(index, function(x) x[!is.na(x)])
			odaty <- paste0(format(debSeas, '%Y-%m'), '_', finSeas)
		}
	}

	if(period1 == "roll.seas"){
		seasonLength <- GeneralParameters$Seasonal$length.mon
		month12 <- str_pad(1:12, width = 2, pad = "0")

		dmon <- substr(dates, 1, 6)
		roll.mon <- rle(dmon)
		ij <- 1:length(roll.mon$values)
		ij <- ij[ij <= (length(ij)-seasonLength+1)]
		ij <- lapply(ij, function(i) i:(i + seasonLength - 1))

		if(period == 'daily'){
			ix1 <- ij[[1]]
			nb1 <- roll.mon$lengths[ix1]
			x1 <- substr(roll.mon$values[ix1[1]], 5, 6)
			nb1[1] <- rev((28:31)[!is.na(as.Date(paste(2014, x1, 28:31, sep = '-')))])[1]
			nbd0 <- sum(nb1)

			ix2 <- ij[[length(ij)]]
			nb2 <- roll.mon$lengths[ix2]
			x2 <- substr(roll.mon$values[ix2[seasonLength]], 5, 6)
			nb2[seasonLength] <- rev((28:31)[!is.na(as.Date(paste(2014, x2, 28:31, sep = '-')))])[1]
			nbd1 <- sum(nb2)
		}
		if(period == 'pentad') nbd0 <- nbd1 <- 6*seasonLength
		if(period == 'dekadal') nbd0 <- nbd1 <- 3*seasonLength
		if(period == 'monthly') nbd0 <- nbd1 <- seasonLength

		index <- lapply(ij, function(j) which(dmon %in% roll.mon$values[j]))
		odaty <- sapply(ij, function(j){
			mon <- month12[as.numeric(substr(roll.mon$values[j], 5, 6))]
			an <- substr(roll.mon$values[j], 1, 4)
			paste(paste(an[1], mon[1], sep = '-'),
			paste(an[length(j)], mon[length(j)], sep = '-'), sep = '_')
		})
	}

	#########################

	nbd.in <- nbd <- sapply(index, length)
	nbd[1] <- nbd0
	nbd[length(nbd)] <- nbd1
	# ifull <- nbd.in == nbd
	ifull <- (nbd.in/nbd) >= min.frac

	# odaty1 <- odaty[!ifull]
	# index1 <- index[!ifull]

	odaty <- odaty[ifull]
	index <- index[ifull]
	nbd.in <- nbd.in[ifull]

	#########################

	if(datatype == 'cdt'){
		cdtdata <- lapply(index, function(ix){
			ncdon <- donne$data[ix, , drop = FALSE]
			miss <- (colSums(is.na(ncdon))/nrow(ncdon)) > min.frac

			if(aggr.fun == 'max') out <- matrixStats::colMaxs(ncdon, na.rm = TRUE)
			if(aggr.fun == 'min') out <- matrixStats::colMins(ncdon, na.rm = TRUE)
			if(aggr.fun == 'sum') out <- colSums(ncdon, na.rm = TRUE)
			if(aggr.fun == 'mean') out <- colMeans(ncdon, na.rm = TRUE)
			if(aggr.fun == 'count'){
				count.fun <- match.fun(opr.fun)
				ncdon <- count.fun(ncdon, opr.thres) & !is.na(ncdon)
				out <- base::colSums(ncdon, na.rm = TRUE)
			}

			out[miss] <- miss.val
			out[is.na(out) | is.nan(out) | is.infinite(out)] <- miss.val
			out
		})
		cdtdata <- do.call(rbind, cdtdata)

		if(is.null(donne$elv)){
			headers <- t(cbind(donne$id, donne$lon, donne$lat))
			capition <- c('Stations', 'LON', paste(toupper(period1),'LAT', sep = '/'))
		}else{
			headers <- t(cbind(donne$id, donne$lon, donne$lat, donne$elv))
			capition <- c('Stations', 'LON', 'LAT', paste(toupper(period1),'ELV', sep = '/'))
		}
		headers[is.na(headers)] <- miss.val
		entete <- cbind(capition, headers)
		cdtdata <- rbind(entete, cbind(odaty, cdtdata))
		writeFiles(cdtdata, GeneralParameters$output)
		rm(cdtdata)
	}

	if(datatype == 'netcdf'){
		outputDIR <- file.path(GeneralParameters$output, "Aggregated_Data")
		dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)

		nc <- nc_open(ncPATH[which(ncEXIST)[1]])
		xlon0 <- nc$dim[[ncINFO$xo]]$vals
		xlat0 <- nc$dim[[ncINFO$yo]]$vals
		varid0 <- ncINFO$varid
		units0 <- nc$var[[varid0]]$units
		prec0 <- nc$var[[varid0]]$prec
		missval0 <- nc$var[[varid0]]$missval
		longname0 <- nc$var[[varid0]]$longname
		nc_close(nc)
		xo0 <- order(xlon0)
		xlon0 <- xlon0[xo0]
		yo0 <- order(xlat0)
		xlat0 <- xlat0[yo0]
		xnlon0 <- length(xlon0)
		xnlat0 <- length(xlat0)

		########
		outnc <- paste0(strsplit(GeneralParameters$ncdf.data$format, "%")[[1]][1], odaty, '.nc')
		out.ncfiles <- file.path(outputDIR, outnc)

		#######
		dx <- ncdim_def("Lon", "degreeE", xlon0)
		dy <- ncdim_def("Lat", "degreeN", xlat0)
		grd.nc.out <- ncvar_def(varid0, units0, list(dx, dy), missval0, longname = longname0, prec = prec0)

		#######
		is.parallel <- doparallel(length(index) >= 20)
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(jj = seq_along(index), .packages = "ncdf4") %parLoop% {
			ix <- index[[jj]]
			nc.files <- ncPATH[ix]
			nc.exist <- ncEXIST[ix]
			nc.files <- nc.files[nc.exist]
			len.nc.files <- length(nc.files)

			if((len.nc.files == 0) | (len.nc.files/nbd.in[jj] < min.frac)){
				out <- matrix(missval0, nrow = xnlon0, ncol = xnlat0)
				nc2 <- nc_create(out.ncfiles[jj], grd.nc.out)
				ncvar_put(nc2, grd.nc.out, out)
				nc_close(nc2)
				return(NULL)
			} 

			ncdon <- lapply(seq_along(nc.files), function(j){
				nc <- nc_open(nc.files[j])
				don <- ncvar_get(nc, varid = varid0)
				nc_close(nc)
				if(ncINFO$yo == 1){
					don <- matrix(c(don), nrow = xnlon0, ncol = xnlat0, byrow = TRUE)
				}
				c(don[xo0, yo0])
			})
			ncdon <- do.call(rbind, ncdon)
			miss <- (colSums(is.na(ncdon))/nrow(ncdon)) > min.frac

			if(aggr.fun == 'max') out <- matrixStats::colMaxs(ncdon, na.rm = TRUE)
			if(aggr.fun == 'min') out <- matrixStats::colMins(ncdon, na.rm = TRUE)
			if(aggr.fun == 'sum') out <- colSums(ncdon, na.rm = TRUE)
			if(aggr.fun == 'mean') out <- colMeans(ncdon, na.rm = TRUE)
			if(aggr.fun == 'count'){
				count.fun <- match.fun(opr.fun)
				ncdon <- count.fun(ncdon, opr.thres) & !is.na(ncdon)
				out <- base::colSums(ncdon, na.rm = TRUE)
			}

			out[miss] <- missval0
			out[is.na(out) | is.nan(out) | is.infinite(out)] <- missval0
			out <- matrix(out, nrow = xnlon0, ncol = xnlat0)

			nc2 <- nc_create(out.ncfiles[jj], grd.nc.out)
			ncvar_put(nc2, grd.nc.out, out)
			nc_close(nc2)
			rm(out, ncdon); gc()
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
	}

	InsertMessagesTxt(main.txt.out, 'Aggregating time series finished')
	tcl("update")
	return(0)
}


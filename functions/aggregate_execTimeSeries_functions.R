ExeAggTimeSeries <- function(GeneralParameters){
	start.mon <- as.numeric(GeneralParameters$seasonal$Values[1])
	length.mon <- as.numeric(GeneralParameters$seasonal$Values[2])

	period <- as.character(GeneralParameters$period)
	period1 <- switch(as.character(GeneralParameters$period1),
						'seasonal' =  c(start.mon, length.mon),
						'seasonal1' = length.mon,
						as.character(GeneralParameters$period1))

	datatype <- as.character(GeneralParameters$data.type)
	file.pars <- as.character(GeneralParameters$file.io$Values)

	fun <- as.character(GeneralParameters$compute.var$Values[1])
	minfrac <- as.numeric(GeneralParameters$compute.var$Values[2])

	if(datatype == 'cdt'){
		donne <- getStnOpenData(file.pars[1])
		donneInfo <- getStnOpenDataInfo(file.pars[1])
		donne <- getCDTdataAndDisplayMsg(donne, period)
		if(is.null(donne)) return(NULL)
		miss.val <- donneInfo[[3]]$miss.val

		outdonne <- aggregateSeries(donne$data, donne$dates, fun = fun, na.rm = TRUE, min.frac = minfrac,
									time.step.In = period, time.step.Out = period1, type = 'matrix')
		outdates <- outdonne$out$date
		outdonne <- round(outdonne$out$data, 1)
		outdonne[is.na(outdonne)] <- miss.val

		if(is.null(donne$elv)){
			headers <- t(cbind(donne$id, donne$lon, donne$lat))
			capition <- c('Stations', 'LON', paste(toupper(as.character(GeneralParameters$period1)),'LAT', sep = '/'))
		}else{
			headers <- t(cbind(donne$id, donne$lon, donne$lat, donne$elv))
			capition <- c('Stations', 'LON', 'LAT', paste(toupper(as.character(GeneralParameters$period1)),'ELV', sep = '/'))
		}
		entete <- cbind(capition, headers)
		outdonne <- rbind(entete, cbind(outdates, outdonne))
		writeFiles(outdonne, file.pars[2])
	}

	##############################################
	if(datatype == 'series'){
		filefrmt <- as.character(GeneralParameters$file.date.format$Values[1])
		datefrmt <- as.character(GeneralParameters$file.date.format$Values[2])

		donne <- getStnOpenData(file.pars[1])
		donneInfo <- getStnOpenDataInfo(file.pars[1])
		donne <- getCDTTSdataAndDisplayMsg(donne, period, filefrmt, datefrmt)
		if(is.null(donne)) return(NULL)
		miss.val <- donneInfo[[3]]$miss.val

		if(donne$nbvar == 3){
			rr <- aggregateSeries(donne$var$rr, donne$dates, fun = 'sum', na.rm = TRUE, min.frac = minfrac,
									time.step.In = period, time.step.Out = period1, type = 'vector')
			txtn <- aggregateSeries(cbind(donne$var$tx, donne$var$tn), donne$dates, fun = 'mean', na.rm = TRUE, min.frac = minfrac,
									time.step.In = period, time.step.Out = period1, type = 'matrix')
			vars <- round(cbind(rr$out$data, txtn$out$data), 1)
			vars[is.na(vars)] <- miss.val
			vars <- cbind(rr$out$date, vars)
		}else{
			vars <- aggregateSeries(donne$var$var, donne$dates, fun = fun, na.rm = TRUE, min.frac = minfrac,
									time.step.In = period, time.step.Out = period1, type = 'vector')
			tmp <- round(vars$out$data, 1)
			tmp[is.na(tmp)] <- miss.val
			vars <- cbind(vars$out$date, tmp)
		}
		writeFiles(vars, file.pars[2])
	}

	##############################################
	if(datatype == 'netcdf'){
		istart.yrs <- as.numeric(as.character(GeneralParameters$datesSE$Values[1]))
		istart.mon <- as.numeric(as.character(GeneralParameters$datesSE$Values[2]))
		istart.day <- as.numeric(as.character(GeneralParameters$datesSE$Values[3]))
		iend.yrs <- as.numeric(as.character(GeneralParameters$datesSE$Values[4]))
		iend.mon <- as.numeric(as.character(GeneralParameters$datesSE$Values[5]))
		iend.day <- as.numeric(as.character(GeneralParameters$datesSE$Values[6]))

		informat <- as.character(GeneralParameters$IO.file.format$Values[1])
		outformat <- as.character(GeneralParameters$IO.file.format$Values[2])

		dstart <- as.Date(paste(istart.yrs, istart.mon, istart.day, sep = '-'), format = '%Y-%m-%d')
		dend <- as.Date(paste(iend.yrs, iend.mon, iend.day, sep = '-'), format = '%Y-%m-%d')

		if(period == 'daily'){
			daty <- seq(dstart, dend, 'day')
			dates <- format(daty, '%Y%m%d')
			filein0 <- file.path(file.pars[1], sprintf(informat, as.character(substr(dates, 1, 4)),
			 						as.character(substr(dates, 5,6)), as.character(substr(dates, 7, 8))))
		}
		if(period == 'dekadal'){
			daty <- seq(dstart, dend, 'day')
			idays <- as.numeric(format(daty, '%d')) < 4
			dates <- paste(format(daty[idays], '%Y%m'), as.numeric(format(daty[idays], '%d')), sep = '')
			filein0 <- file.path(file.pars[1], sprintf(informat, as.character(substr(dates, 1, 4)),
		 							as.character(substr(dates, 5, 6)), as.numeric(substr(dates, 7, 7))))
		}
		if(period == 'monthly'){
			daty <- seq(dstart, dend, 'month')
			dates <- format(daty, '%Y%m')
			filein0 <- file.path(file.pars[1], sprintf(informat, as.character(substr(dates, 1, 4)),
														 as.character(substr(dates, 5, 6))))
		}

		existFl <- unlist(lapply(filein0, file.exists))
		if(!any(existFl)){
			InsertMessagesTxt(main.txt.out, "Unable to locate NetCDF files", format = TRUE)
			return(NULL)
		}
		filein0 <- filein0[existFl]
		dates <- dates[existFl]

		nc <- nc_open(filein0[1])
		# missval <- nc$var[[1]]$missval
		units <- nc$var[[1]]$units
		dims <- nc$var[[1]]$dim
		prec <- nc$var[[1]]$prec
		longname <- nc$var[[1]]$longname
		varname <- nc$var[[1]]$name
		nc_close(nc)
		
		nx <- dims[[1]]$len
		ny <- dims[[2]]$len

		prec <- switch(prec, 
						'byte' = 'short', 
						'single' = 'float', 
						prec)
		missval <- -9999

		grd <- ncvar_def(name = varname, units = units, dim = dims, missval = missval, longname = longname, prec = prec)

		## do parallel
		vartmp <- lapply(filein0, function(ncfile){
						nc <- nc_open(ncfile)
						vars <- ncvar_get(nc, varid = nc$var[[1]]$name)
						nc_close(nc)
						vars
					})
		vartmp <- aggregateSeries(vartmp, dates, fun = fun, na.rm = TRUE, min.frac = minfrac,
								time.step.In = period, time.step.Out = period1, type = 'list')

		for(j in seq_along(vartmp$out$date)){
			fileout <- paste(outformat, '_', vartmp$out$date[j], '.nc', sep = '')
			don <- vartmp$out$data[[j]]
			don[is.na(don)] <- missval
			ncfiles2 <- file.path(file.pars[2], fileout, fsep = .Platform$file.sep)
			nc2 <- nc_create(ncfiles2, grd)
			ncvar_put(nc2, grd, don)
			nc_close(nc2)
		}
		rm(vartmp, dates, filein0, don, grd, daty)
		gc()
	}
	return(0)
}

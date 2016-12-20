ExeAggTimeSeries <- function(GeneralParameters){
	InsertMessagesTxt(main.txt.out, 'Aggregate time series ...')
	tcl("update")

	start.mon <- GeneralParameters$Seasonal$start.mon
	length.mon <- GeneralParameters$Seasonal$length.mon

	period <- GeneralParameters$period
	period1 <- switch(GeneralParameters$output.time,
						'seasonal' =  c(start.mon, length.mon),
						'seasonal1' = length.mon,
						GeneralParameters$output.time)

	datatype <- GeneralParameters$data.type
	input.data <- GeneralParameters$IO.files$In.dir.file
	output2save <- GeneralParameters$IO.files$Out.dir.file
	fun <- GeneralParameters$compute.var$Function
	minfrac <- GeneralParameters$compute.var$miss.frac

	if(datatype == 'cdt'){
		donne <- getStnOpenData(input.data)
		if(is.null(donne)) return(NULL)
		donneInfo <- getStnOpenDataInfo(input.data)
		if(is.null(donneInfo)) return(NULL)
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
			capition <- c('Stations', 'LON', paste(toupper(period1),'LAT', sep = '/'))
		}else{
			headers <- t(cbind(donne$id, donne$lon, donne$lat, donne$elv))
			capition <- c('Stations', 'LON', 'LAT', paste(toupper(period1),'ELV', sep = '/'))
		}
		entete <- cbind(capition, headers)
		outdonne <- rbind(entete, cbind(outdates, outdonne))
		writeFiles(outdonne, output2save)
	}

	##############################################
	if(datatype == 'series'){
		filefrmt <- GeneralParameters$One.series$file.format
		datefrmt <- GeneralParameters$One.series$date.format

		donne <- getStnOpenData(input.data)
		if(is.null(donne)) return(NULL)
		donneInfo <- getStnOpenDataInfo(input.data)
		if(is.null(donneInfo)) return(NULL)
		donne <- getCDTTSdataAndDisplayMsg(donne, period, filefrmt, datefrmt)
		if(is.null(donne)) return(NULL)
		miss.val <- donneInfo[[3]]$miss.val

		if(donne$nbvar == 3){
			rr <- aggregateSeries(donne$var$rr, donne$dates, fun = 'sum', na.rm = TRUE, min.frac = minfrac,
									time.step.In = period, time.step.Out = period1, type = 'vector')
			txtn <- aggregateSeries(cbind(donne$var$tx, donne$var$tn), donne$dates, fun = 'mean', na.rm = TRUE,
									min.frac = minfrac, time.step.In = period, time.step.Out = period1, type = 'matrix')
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
		writeFiles(vars, output2save)
	}

	##############################################
	if(datatype == 'netcdf'){
		istart.yrs <- GeneralParameters$Date.Range$start.year
		istart.mon <- GeneralParameters$Date.Range$start.mon
		istart.day <- GeneralParameters$Date.Range$start.day
		iend.yrs <- GeneralParameters$Date.Range$end.year
		iend.mon <- GeneralParameters$Date.Range$end.mon
		iend.day <- GeneralParameters$Date.Range$end.day

		informat <- GeneralParameters$ncdf.file.format$input
		outformat <- GeneralParameters$ncdf.file.format$output

		dstart <- as.Date(paste(istart.yrs, istart.mon, istart.day, sep = '-'), format = '%Y-%m-%d')
		dend <- as.Date(paste(iend.yrs, iend.mon, iend.day, sep = '-'), format = '%Y-%m-%d')
		if(is.na(dstart)){
			InsertMessagesTxt(main.txt.out, "Wrong start date", format = TRUE)
			tcl("update")
			return(NULL)
		}
		if(is.na(dend)){
			InsertMessagesTxt(main.txt.out, "Wrong end date", format = TRUE)
			tcl("update")
			return(NULL)
		}

		######
		if(period == 'daily'){
			daty <- seq(dstart, dend, 'day')
			dates <- format(daty, '%Y%m%d')
			filein0 <- file.path(input.data, sprintf(informat, as.character(substr(dates, 1, 4)),
			 						as.character(substr(dates, 5,6)), as.character(substr(dates, 7, 8))))
		}
		if(period == 'dekadal'){
			daty <- seq(dstart, dend, 'day')
			idays <- as.numeric(format(daty, '%d')) < 4
			dates <- paste(format(daty[idays], '%Y%m'), as.numeric(format(daty[idays], '%d')), sep = '')
			filein0 <- file.path(input.data, sprintf(informat, as.character(substr(dates, 1, 4)),
		 							as.character(substr(dates, 5, 6)), as.numeric(substr(dates, 7, 7))))
		}
		if(period == 'monthly'){
			daty <- seq(dstart, dend, 'month')
			dates <- format(daty, '%Y%m')
			filein0 <- file.path(input.data, sprintf(informat, as.character(substr(dates, 1, 4)),
														 as.character(substr(dates, 5, 6))))
		}

		######
		existFl <- unlist(lapply(filein0, file.exists))
		if(!any(existFl)){
			InsertMessagesTxt(main.txt.out, "Unable to locate NetCDF files", format = TRUE)
			tcl("update")
			return(NULL)
		}
		filein0 <- filein0[existFl]
		dates <- dates[existFl]

		######
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
		grd.out <- ncvar_def(name = varname, units = units, dim = dims, missval = missval, longname = longname, prec = prec)

		######
		## do parallel
		vartmp <- lapply(filein0, function(ncfile){
						nc <- nc_open(ncfile)
						vars <- ncvar_get(nc, varid = nc$var[[1]]$name)
						nc_close(nc)
						vars
					})
		vartmp <- aggregateSeries(vartmp, dates, fun = fun, na.rm = TRUE, min.frac = minfrac,
								time.step.In = period, time.step.Out = period1, type = 'list')
		######
		if(period1 == 'dekadal'){
			ncfiles2 <- file.path(output2save, sprintf(outformat, substr(vartmp$out$date, 1, 4),
								substr(vartmp$out$date, 5, 6), substr(vartmp$out$date, 7, 7)))
		}else  if(period1 == 'monthly'){
			ncfiles2 <- file.path(output2save, sprintf(outformat, substr(vartmp$out$date, 1, 4),
								substr(vartmp$out$date, 5, 6)))
		}else{
			ncfiles2 <- file.path(output2save, sprintf(outformat, vartmp$out$date))
		}

		######
		for(j in seq_along(vartmp$out$date)){
			outfl <- ncfiles2[j]
			don <- vartmp$out$data[[j]]
			don[is.na(don)] <- missval
			nc2 <- nc_create(outfl, grd.out)
			ncvar_put(nc2, grd.out, don)
			nc_close(nc2)
		}
		rm(vartmp, dates, filein0, don, grd.out, daty)
		gc()
	}
	InsertMessagesTxt(main.txt.out, 'Aggregating time series finished')
	tcl("update")
	return(0)
}

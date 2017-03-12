
computeHomogData <- function(GeneralParameters){
	freqdata <- GeneralParameters$period
	varcat <- GeneralParameters$stn.type$vars
	comp.fun <- GeneralParameters$aggr.var$fonction
	miss.frac <- GeneralParameters$aggr.var$miss.frac

	#####
	fileparams <- file.path(EnvHomogzData$baseDir, 'OriginalData', 'Parameters.RData')
	load(fileparams)

	if(!GeneralParameters$stn.type$single.series){
		if(freqdata == 'daily'){
			###monthly data
			mdonne <- apply(EnvHomogzData$donnees1$data, 2, daily2monthly, dates = EnvHomogzData$donnees1$dates, fun = comp.fun, frac = miss.frac)
			dmdonne <- round(sapply(mdonne, function(x) x$Values), 1)
			dmdaty <- as(mdonne[[1]]$Date, 'character')
			paramsGAL$data2 <- list(list(dates = dmdaty, data = dmdonne))
			##dekadal data
			ddonne <- apply(EnvHomogzData$donnees1$data, 2, daily2dekadal, dates = EnvHomogzData$donnees1$dates, fun = comp.fun, frac = miss.frac)
			dkdonne <- round(sapply(ddonne, function(x) x$Values), 1)
			dkdaty <- as(ddonne[[1]]$Date, 'character')
			paramsGAL$data1 <- list(list(dates = dkdaty, data = dkdonne))

			mon_data <- list(list(date = dmdaty, data = dmdonne))
			dek_data <- list(list(date = dkdaty, data = dkdonne))
			dly_data <- list(list(date = EnvHomogzData$donnees1$dates, data = EnvHomogzData$donnees1$data))
		}else if(freqdata == 'dekadal'){
			###monthly data
			mdonne <- apply(EnvHomogzData$donnees1$data, 2, dekadal2monthly, dates = EnvHomogzData$donnees1$dates, fun = comp.fun, frac = miss.frac)
			dmdonne <- round(sapply(mdonne, function(x) x$Values), 1)
			dmdaty <- as(mdonne[[1]]$Date, 'character')
			paramsGAL$data1 <- list(list(dates = dmdaty, data = dmdonne))

			mon_data <- list(list(date = dmdaty, data = dmdonne))
			dek_data <- list(list(date = EnvHomogzData$donnees1$dates, data = EnvHomogzData$donnees1$data))
			dly_data <- NULL
		}else if(freqdata == 'monthly'){
			mon_data <- list(list(date = EnvHomogzData$donnees1$dates, data = EnvHomogzData$donnees1$data))
			dek_data <- NULL
			dly_data <- NULL
		}else return(NULL)
	}else{
		if(EnvHomogzData$donnees1$nbvar == 3){
			cdates <- EnvHomogzData$donnees1$dates
			if(varcat == "1") cdat <- EnvHomogzData$donnees1$var$rr
			if(varcat == "2") cdat <- EnvHomogzData$donnees1$var$tx
			if(varcat == "3") cdat <- EnvHomogzData$donnees1$var$tn
			if(GeneralParameters$use.refSeries){
				rdates <- EnvHomogzData$donnees2$dates
				if(varcat == "1") rdat <- EnvHomogzData$donnees2$var$rr
				if(varcat == "2") rdat <- EnvHomogzData$donnees2$var$tx
				if(varcat == "3") rdat <- EnvHomogzData$donnees2$var$tn
			}
		}else{
			cdates <- EnvHomogzData$donnees1$dates
			cdat <- EnvHomogzData$donnees1$var$var
			if(GeneralParameters$use.refSeries){
				rdates <- EnvHomogzData$donnees2$dates
				rdat <- EnvHomogzData$donnees2$var$var
			}
		}

		if(freqdata == 'daily'){
			###monthly data
			mcand <- daily2monthly(cdat, dates = cdates, fun = comp.fun, frac = miss.frac)
			dmdonne <- round(mcand$Values, 1)
			dmdaty <- as.character(mcand$Date)
			paramsGAL$data2 <- list(list(dates = dmdaty, data = dmdonne))
			##dekadal data
			dcand <- daily2dekadal(cdat, dates = cdates, fun = comp.fun, frac = miss.frac)
			dkdonne <- round(dcand$Values, 1)
			dkdaty <- as.character(dcand$Date)
			paramsGAL$data1 <- list(list(dates = dkdaty, data = dkdonne))

			mon_data <- list(list(date = dmdaty, data = dmdonne))
			dek_data <- list(list(date = dkdaty, data = dkdonne))
			dly_data <- list(list(date = cdates, data = cdat))
			if(GeneralParameters$use.refSeries){
				###monthly data
				mrefs <- daily2monthly(rdat, dates = rdates, fun = comp.fun, frac = miss.frac)
				rdmdonne <- round(mrefs$Values, 1)
				rdmdaty <- as.character(mrefs$Date)
				paramsGAL$data2[[2]] <- list(dates = rdmdaty, data = rdmdonne)
				EnvHomogzData$mon_data[[2]] <- list(date = rdmdaty, data = rdmdonne)
				##dekadal data
				drefs <- daily2dekadal(rdat, dates = rdates, fun = comp.fun, frac = miss.frac)
				rdkdonne <- round(drefs$Values, 1)
				rdkdaty <- as.character(drefs$Date)
				paramsGAL$data1[[2]] <- list(dates = rdkdaty, data = rdkdonne)
				EnvHomogzData$dek_data[[2]] <- list(date = rdkdaty, data = rdkdonne)
				##daily data
				EnvHomogzData$dly_data[[2]] <- list(date = rdates, data = rdat)
			}
		}else if(freqdata == 'dekadal'){
			###monthly data
			mcand <- dekadal2monthly(cdat, dates = cdates, fun = comp.fun, frac = miss.frac)
			dmdonne <- round(mcand$Values, 1)
			dmdaty <- as.character(mcand$Date)
			paramsGAL$data1 <- list(list(dates = dmdaty, data = dmdonne))

			mon_data <- list(list(date = dmdaty, data = dmdonne))
			dek_data <- list(list(date = cdates, data = cdat))
			dly_data <- NULL
			if(GeneralParameters$use.refSeries){
				###monthly data
				mrefs <- dekadal2monthly(rdat, dates = rdates, fun = comp.fun, frac = miss.frac)
				rdmdonne <- round(mrefs$Values, 1)
				rdmdaty <- as.character(mrefs$Date)
				paramsGAL$data1[[2]] <- list(dates = rdmdaty, data = rdmdonne)
				EnvHomogzData$mon_data[[2]] <- list(date = rdmdaty, data = rdmdonne)
				##dekadal data
				EnvHomogzData$dek_data[[2]] <- list(date = rdates, data = rdat)
			}
		}else if(freqdata == 'monthly'){
			mon_data <- list(list(date = cdates, data = cdat))
			dek_data <- NULL
			dly_data <- NULL
			if(GeneralParameters$use.refSeries){
				###monthly data
				EnvHomogzData$mon_data[[2]] <- list(date = rdates, data = rdat)
			}
		}else return(NULL)
	}

	assign('mon_data', mon_data, envir = EnvHomogzData)
	assign('dek_data', dek_data, envir = EnvHomogzData)
	assign('dly_data', dly_data, envir = EnvHomogzData)

	save(paramsGAL, file = fileparams)
}

##################################################################################

getElevationData2 <- function(){
	if(!is.null(EnvHomogzData$donnees1)){
		###get elevation data
		msg <- NULL
		elv_dem <- NULL
		if(!GeneralParameters$stn.type$single.series){
			elv_stn <- EnvHomogzData$donnees1$elv

			if(GeneralParameters$refSeries.choix$use.elv){
				if(GeneralParameters$refSeries.choix$interp.dem == "0"){
					if(GeneralParameters$IO.files$DEM.file == "") msg <- 'There is no elevation data in format NetCDF'
					else{
						demData <- getDemOpenDataSPDF(GeneralParameters$IO.files$DEM.file)
						ijGrd <- grid2pointINDEX(list(lon = EnvHomogzData$donnees1$lon, lat = EnvHomogzData$donnees1$lat),
												list(lon = demData$lon, lat = demData$lat))
						elv_dem <- demData$demGrd@data[ijGrd, 1]
					}
				}else{
					if(is.null(elv_stn)) msg <- 'There is no elevation data in your file'
				}
			}
		}
	}else msg <- 'No station data found'
	return(list(elv_stn, elv_dem, msg, GeneralParameters$IO.files$DEM.file))
}

##################################################################################

ExecHomData <- function(get.stn){
	freqdata <- GeneralParameters$period
	ypos <- GeneralParameters$stn.user.choice

	if(!GeneralParameters$stn.type$single.series){
		xpos <- which(as.character(EnvHomogzData$donnees1$id) == get.stn)

		if(GeneralParameters$use.refSeries){
			if(!GeneralParameters$refSeries.by.user){
				RefSer <- getrefSeries(xpos)
				dyref <- dkref <- moref <- NULL

				#monthly
				moref <- RefSer$retmon$refs
				momsg <- RefSer$retmon$msg
				if(!is.null(momsg)) InsertMessagesTxt(main.txt.out, paste(get.stn, '::', momsg$msg, ' == >',
																	momsg$msg1, 'for monthly data'), format = TRUE)
				if(freqdata != 'monthly'){
					#dekadal
					dkref <- RefSer$retdek$refs
					dkmsg <- RefSer$retdek$msg
					if(!is.null(dkmsg)) InsertMessagesTxt(main.txt.out, paste(get.stn, '::', dkmsg$msg, ' == >',
																		dkmsg$msg1, 'for dekadal data'), format = TRUE)
					if(freqdata == 'daily'){
						#daily
						dyref <- RefSer$retdly$refs
						dymsg <- RefSer$retdly$msg
						if(!is.null(dymsg)) InsertMessagesTxt(main.txt.out, paste(get.stn, '::', dymsg$msg, ' == >',
																			dymsg$msg1, 'for daily data'), format = TRUE)
					}
				}
			}else{
				if(length(ypos) > 1){
					RefSer <- getrefSeriesUser(xpos)
					dyref <- dkref <- moref <- NULL

					moref <- RefSer$retmon$refs
					if(freqdata != 'monthly'){
						dkref <- RefSer$retdek$refs
						if(freqdata == 'daily') dyref <- RefSer$retdly$refs
					}
				}else if(length(ypos) == 1){
					RefSer <- getrefSeries1S(xpos, ypos)
					dyref <- dkref <- moref <- NULL

					moref <- RefSer$retmon
					if(freqdata != 'monthly'){
						dkref <- RefSer$retdek
						if(freqdata == 'daily') dyref <- RefSer$retdly
					}
				}else{
					RefSer <- gettestSeries(xpos)
					dyref <- dkref <- moref <- NULL

					moref <- RefSer$retmon
					if(freqdata != 'monthly'){
						dkref <- RefSer$retdek
						if(freqdata == 'daily') dyref <- RefSer$retdly
					}
					InsertMessagesTxt(main.txt.out, paste(get.stn, '::', 'No chosen station', ' == >',
										'The breakpoint detection is carried without reference series'), format = TRUE)
				}
			}
		}else{
				RefSer <- gettestSeries(xpos)
				dyref <- dkref <- moref <- NULL

				moref <- RefSer$retmon
				if(freqdata != 'monthly'){
					dkref <- RefSer$retdek
					if(freqdata == 'daily') dyref <- RefSer$retdly
				}
		}
	}else{
		if(GeneralParameters$use.refSeries){
				RefSer <- getrefSeries1S(xpos = NULL, ypos = NULL)
				dyref <- dkref <- moref <- NULL

				moref <- RefSer$retmon
				if(freqdata != 'monthly'){
					dkref <- RefSer$retdek
					if(freqdata == 'daily') dyref <- RefSer$retdly
				}
		}else{
				RefSer <- gettestSeries(xpos = NULL)
				dyref <- dkref <- moref <- NULL

				moref <- RefSer$retmon
				if(freqdata != 'monthly'){
					dkref <- RefSer$retdek
					if(freqdata == 'daily') dyref <- RefSer$retdly
				}
		}
	}

	brkpts <- getBreakpoints(dyref = dyref, dkref = dkref, moref = moref)

	return(list(breakpts = brkpts, dyref = dyref, dkref = dkref, moref = moref))
}





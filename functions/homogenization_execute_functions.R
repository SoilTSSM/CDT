
getElevationData2 <- function(){
	single.series <- as.character(GeneralParameters$use.method$Values[2])
	uselv <- as.character(GeneralParameters$ref.series.choix$Values[3])
	interp.dem <- as.character(GeneralParameters$ref.series.choix$Values[4])
	file.pars <- as.character(GeneralParameters$file.io$Values)

	if(!is.null(EnvHomogzData$donnees1)){
		###get elevation data
		msg <- NULL
		elv_dem <- NULL
		if(single.series == "0"){
			lon <- EnvHomogzData$donnees1$lon
			lat <- EnvHomogzData$donnees1$lat
			elv_stn <- EnvHomogzData$donnees1$elv

			if(uselv == "1"){
				if(interp.dem == "0"){
					if(file.pars[3] == "") msg <- 'There is no elevation data in format NetCDF'
					else{
						all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))
						jncdf <- which(all.open.file == file.pars[3])
						fdem <- AllOpenFilesData[[jncdf]][[2]]
						dem <- fdem$value
						dem[dem < 0] <- NA
						dem <- data.frame(expand.grid(x = fdem$x, y = fdem$y), z = c(dem))
						dem <- dem[!is.na(dem$z),]
						xy0 <- data.frame(x = lon, y = lat)
						elv_dem <- idw(formula = z ~ 1, locations= ~x+y, data = dem, newdata = xy0, nmax = 4, debug.level = 0)[,3]
					}
				}else{
					if(is.null(elv_stn)) msg <- 'There is no elevation data in your file'
				}
			}
		}
	}else msg <- 'No station data found'
	return(list(elv_stn,elv_dem,msg, file.pars[3]))
}

##################################################################################

computeHomogData <- function(GeneralParameters){
	freqdata <- GeneralParameters$period
	umthdpas <- as.character(GeneralParameters$use.method$Values)
	single.series <- umthdpas[2]
	useref <- umthdpas[3]
	varcat <- as.character(GeneralParameters$file.date.format$Values[3])
	comppars <- as.character(GeneralParameters$compute.var$Values)
	comp.fun <- comppars[1]
	miss.frac <- as.numeric(comppars[2])

	#####
	fileparams <- file.path(EnvHomogzData$baseDir, 'OriginalData', 'Parameters.RData', fsep = .Platform$file.sep)
	load(fileparams)

	if(single.series == "0"){
		if(freqdata == 'daily'){
			###monthly data
			mdonne <- apply(EnvHomogzData$donnees1$data, 2, daily2monthly, dates = EnvHomogzData$donnees1$dates, fun = comp.fun, frac = miss.frac)
			dmdonne <- round(sapply(mdonne, function(x) x$Values), 1)
			dmdaty <- as(mdonne[[1]]$Date, 'character')
			paramsGAL$data2 <- list(list(dates = dmdaty, data = dmdonne))
			assign('mon_data', list(list(date = dmdaty, data = dmdonne)), envir = EnvHomogzData)
			##dekadal data
			ddonne <- apply(EnvHomogzData$donnees1$data, 2, daily2dekadal, dates = EnvHomogzData$donnees1$dates, fun = comp.fun, frac = miss.frac)
			dkdonne <- round(sapply(ddonne, function(x) x$Values), 1)
			dkdaty <- as(ddonne[[1]]$Date, 'character')
			paramsGAL$data1 <- list(list(dates = dkdaty, data = dkdonne))
			assign('dek_data', list(list(date = dkdaty, data = dkdonne)), envir = EnvHomogzData)
			##daily data
			assign('dly_data', list(list(date = EnvHomogzData$donnees1$dates, data = EnvHomogzData$donnees1$data)), envir = EnvHomogzData)
		}else if(freqdata == 'dekadal'){
			###monthly data
			mdonne <- apply(EnvHomogzData$donnees1$data, 2, dekadal2monthly, dates = EnvHomogzData$donnees1$dates, fun = comp.fun)
			dmdonne <- round(sapply(mdonne, function(x) x$Values), 1)
			dmdaty <- as(mdonne[[1]]$Date, 'character')
			paramsGAL$data1 <- list(list(dates = dmdaty, data = dmdonne))
			assign('mon_data', list(list(date = dmdaty, data = dmdonne)), envir = EnvHomogzData)
			##dekadal data
			assign('dek_data', list(list(date = EnvHomogzData$donnees1$dates, data = EnvHomogzData$donnees1$data)), envir = EnvHomogzData)
			assign('dly_data', NULL, envir = EnvHomogzData)
		}else if(freqdata == 'monthly'){
			assign('mon_data', list(list(date = EnvHomogzData$donnees1$dates, data = EnvHomogzData$donnees1$data)), envir = EnvHomogzData)
			assign('dek_data', NULL, envir = EnvHomogzData)
			assign('dly_data', NULL, envir = EnvHomogzData)
		}else{
			return(NULL)
		}
	}else{
		if(EnvHomogzData$donnees1$nbvar == 3){
			cdates <- EnvHomogzData$donnees1$dates
			if(varcat == "1") cdat <- EnvHomogzData$donnees1$var$rr
			if(varcat == "2") cdat <- EnvHomogzData$donnees1$var$tx
			if(varcat == "3") cdat <- EnvHomogzData$donnees1$var$tn
			if(useref == '1'){
				rdates <- EnvHomogzData$donnees2$dates
				if(varcat == "1") rdat <- EnvHomogzData$donnees2$var$rr
				if(varcat == "2") rdat <- EnvHomogzData$donnees2$var$tx
				if(varcat == "3") rdat <- EnvHomogzData$donnees2$var$tn
			}
		}else{
			cdates <- EnvHomogzData$donnees1$dates
			cdat <- EnvHomogzData$donnees1$var$var
			if(useref == '1'){
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
			assign('mon_data', list(list(date = dmdaty, data = dmdonne)), envir = EnvHomogzData)
			##dekadal data
			dcand <- daily2dekadal(cdat, dates = cdates, fun = comp.fun, frac = miss.frac)
			dkdonne <- round(dcand$Values, 1)
			dkdaty <- as.character(dcand$Date)
			paramsGAL$data1 <- list(list(dates = dkdaty, data = dkdonne))
			assign('dek_data', list(list(date = dkdaty, data = dkdonne)), envir = EnvHomogzData)
			##daily data
			assign('dly_data', list(list(date = cdates, data = cdat)), envir = EnvHomogzData)
			if(useref == '1'){
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
			assign('mon_data', list(list(date = dmdaty, data = dmdonne)), envir = EnvHomogzData)
			##dekadal data
			assign('dek_data', list(list(date = cdates, data = cdat)), envir = EnvHomogzData)
			if(useref == '1'){
				###monthly data
				mrefs <- dekadal2monthly(rdat, dates = rdates, fun = comp.fun, frac = miss.frac)
				rdmdonne <- round(mrefs$Values, 1)
				rdmdaty <- as.character(mrefs$Date)
				paramsGAL$data1[[2]] <- list(dates = rdmdaty, data = rdmdonne)
				EnvHomogzData$mon_data[[2]] <- list(date = rdmdaty, data = rdmdonne)
				##dekadal data
				EnvHomogzData$dek_data[[2]] <- list(date = rdates, data = rdat)
			}
			assign('dly_data', NULL, envir = EnvHomogzData)
		}else if(freqdata == 'monthly'){
			###monthly data
			assign('mon_data', list(list(date = cdates, data = cdat)), envir = EnvHomogzData)
			if(useref == '1'){
				###monthly data
				EnvHomogzData$mon_data[[2]] <- list(date = rdates, data = rdat)
			}
			assign('dek_data', NULL, envir = EnvHomogzData)
			assign('dly_data', NULL, envir = EnvHomogzData)
		}else{
			return(NULL)
		}
	}
	save(paramsGAL, file = fileparams)
	#return(0)
}


##################################################################################

ExecHomData <- function(get.stn){
	freqdata <- GeneralParameters$period
	umthdpas <- as.character(GeneralParameters$use.method$Values)
	single.series <- umthdpas[2]
	useref <- umthdpas[3]

	stnref.choix <- as.character(GeneralParameters$ref.series.user)
	ypos <- as.numeric(GeneralParameters$stn.user.choice)


	if(single.series == "0"){
		xpos <- which(as.character(EnvHomogzData$donnees1$id) == get.stn)

		if(useref == '1'){
			if(stnref.choix == '0'){
				RefSer <- getrefSeries(xpos)
				dyref <- dkref <- moref <- NULL

				#monthly
				moref <- RefSer$retmon$refs
				momsg <- RefSer$retmon$msg
				if(!is.null(momsg)) InsertMessagesTxt(main.txt.out, paste(get.stn,'::',momsg$msg,' == >',momsg$msg1, 'for monthly data'), format = TRUE)
				if(freqdata != 'monthly'){
					#dekadal
					dkref <- RefSer$retdek$refs
					dkmsg <- RefSer$retdek$msg
					if(!is.null(dkmsg)) InsertMessagesTxt(main.txt.out, paste(get.stn,'::',dkmsg$msg,' == >',dkmsg$msg1, 'for dekadal data'), format = TRUE)
					if(freqdata == 'daily'){
						#daily
						dyref <- RefSer$retdly$refs
						dymsg <- RefSer$retdly$msg
						if(!is.null(dymsg)) InsertMessagesTxt(main.txt.out, paste(get.stn,'::',dymsg$msg,' == >',dymsg$msg1, 'for daily data'), format = TRUE)
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
					InsertMessagesTxt(main.txt.out, paste(get.stn,'::','No chosen station',' == >','The breakpoint detection is carried without reference series'), format = TRUE)
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
		if(useref == '1'){
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





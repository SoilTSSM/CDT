
getElevationData1 <- function(){
	if(!is.null(EnvQcOutlierData$donnees1)){
		###get elevation data
		msg <- NULL
		elv_dem <- NULL
		if(!GeneralParameters$stn.type$single.series){
			elv_stn <- EnvQcOutlierData$donnees1$elv

			if(GeneralParameters$dem$use.elv){
				if(GeneralParameters$dem$interp.dem == "0"){
					if(GeneralParameters$IO.files$DEM.file == "") msg <- 'There is no elevation data in format NetCDF'
					else{
						demData <- getDemOpenDataSPDF(GeneralParameters$IO.files$DEM.file)
						ijGrd <- grid2pointINDEX(list(lon = EnvQcOutlierData$donnees1$lon, lat = EnvQcOutlierData$donnees1$lat),
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

#####################################################

execQctempFun <- function(get.stn){
	status <- 0
	msg <- NULL

	min.stn <- GeneralParameters$select.pars$min.stn
	max.stn <- GeneralParameters$select.pars$max.stn
	win <- as.integer(GeneralParameters$select.pars$win.len/2)
	thres <- GeneralParameters$select.pars$conf.lev
	R <- GeneralParameters$select.pars$max.dist
	elv.diff <- GeneralParameters$select.pars$elv.diff

	spregpar <- c(thres, max.stn)
	spthres <- c(min.stn, R, elv.diff)
	bounds <- GeneralParameters$stnInfo

	if(!GeneralParameters$stn.type$single.series){
		ijstn <- which(as.character(bounds$Station.ID) == get.stn)
		limsup <- as.numeric(bounds[ijstn, 2:3])

		if(GeneralParameters$dem$use.elv){
			if(is.null(EnvQcOutlierData$r_elv)){
				r_elv <- getElevationData1()
				assign('r_elv', r_elv, envir = EnvQcOutlierData)
			}else{
				if(GeneralParameters$dem$interp.dem == "0" & (is.null(EnvQcOutlierData$r_elv[[2]]) |
					EnvQcOutlierData$r_elv[[4]] != GeneralParameters$IO.files$DEM.file)){
					r_elv <- getElevationData1()
					EnvQcOutlierData$r_elv[2:4] <- r_elv[2:4]
				}else if(GeneralParameters$dem$interp.dem == "1" & is.null(EnvQcOutlierData$r_elv[[1]])){
					elv_stn <- EnvQcOutlierData$donnees1$elv
					if(!is.null(elv_stn)){
						EnvQcOutlierData$r_elv[[1]] <- r_elv[[1]] <- elv_stn
						EnvQcOutlierData$r_elv[[3]] <- r_elv[[3]] <- NULL
					}else{
						EnvQcOutlierData$r_elv[[3]] <- r_elv[[3]] <- 'There is no elevation data in your file'
					}
				}else r_elv <- EnvQcOutlierData$r_elv
			}

			if(!is.null(r_elv[[3]])){
				msg <- r_elv[[3]]
				status <- 'no'
				elv <- NULL
				return(NULL)
			}else{
				if(GeneralParameters$dem$interp.dem == "0") elv <- r_elv[[2]]
				else elv <- r_elv[[1]]
			}
		}else elv <- NULL

		coords <- cbind(EnvQcOutlierData$donnees1$lon, EnvQcOutlierData$donnees1$lat)

		donne <- EnvQcOutlierData$donnees1$data
		dates <- EnvQcOutlierData$donnees1$dates

		if(GeneralParameters$consist.check){ #with c.check
			jstn <- which(as.character(EnvQcOutlierData$donnees1$id) == get.stn)
			jstn2 <- which(as.character(EnvQcOutlierData$donnees2$id) == get.stn)
			idstn <- as.character(EnvQcOutlierData$donnees1$id)[jstn]

			donne2 <- EnvQcOutlierData$donnees2$data
			dates2 <- EnvQcOutlierData$donnees2$dates

			if(GeneralParameters$is.TX){
				TxData <- donne[, jstn]
				TxDate <- dates
				if(length(jstn2) > 0){
					TnData <- donne2[, jstn2]
					TnDate <- dates2
					int.check <- TRUE
				}else{
					TnData <- NULL
					TnDate <- NULL
					int.check <- FALSE
				}
			}else{
				if(length(jstn2) > 0){
					TxData <- donne2[, jstn2]
					TxDate <- dates2
					int.check <- TRUE
				}else{
					TxData <- NULL
					TxDate <- NULL
					int.check <- FALSE
				}
				TnData <- donne[, jstn]
				TnDate <- dates
			}
			omit <- FALSE
		}else{
			jstn <- which(as.character(EnvQcOutlierData$donnees1$id) == get.stn)
			idstn <- as.character(EnvQcOutlierData$donnees1$id)[jstn]

			if(GeneralParameters$is.TX){
				TxData <- donne[, jstn]
				TxDate <- dates
				TnData <- NULL
				TnDate <- NULL
			}else{
				TxData <- NULL
				TxDate <- NULL
				TnData <- donne[, jstn]
				TnDate <- dates
			}
			int.check <- FALSE
			omit <- TRUE
		}

		Tsdata <- list(data = donne, date = dates, tx.data = TxData, tx.date = TxDate, tn.data = TnData, tn.date = TnDate)
		testpars <- list(tx.test = GeneralParameters$is.TX, int.check = int.check, omit = omit, period = GeneralParameters$period)
		xparams <- list(coords = coords, elv = elv, win = win, thres = thres, spthres = spthres, spregpar = spregpar, limsup = limsup)
		res.qc.txtn <- txtnQcSpatialCheck(jstn, idstn, Tsdata, xparams = xparams, testpars = testpars)
	}else{ #a single series
		idstn <- as.character(bounds$Station.ID[1])
		limsup <- as.numeric(bounds[1, 2:3])
		if(GeneralParameters$consist.check){ #with c.check
			if(EnvQcOutlierData$donnees1$nbvar == 3){
				TxData <- EnvQcOutlierData$donnees1$var$tx
				TxDate <- EnvQcOutlierData$donnees1$dates
				TnData <- EnvQcOutlierData$donnees1$var$tn
				TnDate <- EnvQcOutlierData$donnees1$dates
			}else{
				if(GeneralParameters$is.TX){
					TxData <- EnvQcOutlierData$donnees1$var$var
					TxDate <- EnvQcOutlierData$donnees1$dates
					TnData <- EnvQcOutlierData$donnees2$var$var
					TnDate <- EnvQcOutlierData$donnees2$dates
				}else{
					TxData <- EnvQcOutlierData$donnees2$var$var
					TxDate <- EnvQcOutlierData$donnees2$dates
					TnData <- EnvQcOutlierData$donnees1$var$var
					TnDate <- EnvQcOutlierData$donnees1$dates
				}
			}
		}else{
			if(GeneralParameters$is.TX){
				TxData <- if(EnvQcOutlierData$donnees1$nbvar == 3) EnvQcOutlierData$donnees1$var$tx else EnvQcOutlierData$donnees1$var$var
				TxDate <- EnvQcOutlierData$donnees1$dates
				TnData <- NULL
				TnDate <- NULL
			}else{
				TxData <- NULL
				TxDate <- NULL
				TnData <- if(EnvQcOutlierData$donnees1$nbvar == 3) EnvQcOutlierData$donnees1$var$tn else EnvQcOutlierData$donnees1$var$var
				TnDate <- EnvQcOutlierData$donnees1$dates
			}
		}

		Tsdata <- list(tx.data = TxData, tx.date = TxDate, tn.data = TnData, tn.date = TnDate)
		xparams <- list(thres = thres, limsup = limsup)
		testpars <- list(tx.test = GeneralParameters$is.TX, int.check = GeneralParameters$consist.check, period = GeneralParameters$period)
		res.qc.txtn <- txtnQcSingleSeries(idstn, Tsdata, xparams = xparams, testpars = testpars)
	}
	on.exit({
		if(status == 'ok') InsertMessagesTxt(main.txt.out, msg)
		if(status == 'no') InsertMessagesTxt(main.txt.out, msg, format = TRUE)
	})
	return(res.qc.txtn)
}

#####################################################

ExecQcTemp <- function(get.stn){
	ExecTempOneStation <- function(jlstn){
		status <- 0
		msg <- NULL
		dates <- EnvQcOutlierData$donnees1$dates
		if(!GeneralParameters$stn.type$single.series){
			xpos <- which(as.character(EnvQcOutlierData$donnees1$id) == jlstn)
			xdat <- EnvQcOutlierData$donnees1$data[,xpos]
		}else{
			if(EnvQcOutlierData$donnees1$nbvar == 3){
				if(GeneralParameters$is.TX) xdat <- EnvQcOutlierData$donnees1$var$tx
				else xdat <- EnvQcOutlierData$donnees1$var$tn
			}else xdat <- EnvQcOutlierData$donnees1$var$var
		}

		outsdir <- file.path(EnvQcOutlierData$baseDir, 'Outputs', jlstn)
		# ##create by default
		corrdirstn <- file.path(EnvQcOutlierData$baseDir, 'CorrectedData', jlstn)
		if(!file.exists(corrdirstn)) dir.create(corrdirstn, showWarnings = FALSE, recursive = TRUE)
		file_corrected <- file.path(corrdirstn, paste(jlstn, '.txt', sep = ''))

		qcout <- try(execQctempFun(jlstn), silent = TRUE)
		if(!inherits(qcout, "try-error")){
			if(!is.null(qcout)){
				if(!file.exists(outsdir)) dir.create(outsdir, showWarnings = FALSE, recursive = TRUE)
				fileoutRdata <- file.path(outsdir, paste(jlstn, '.Rdata', sep = ''))
				fileoutTXT <- file.path(outsdir, paste(jlstn, '.txt', sep = ''))
				if(GeneralParameters$AllOrOne == 'all') ret.qcout <- qcout
				ret.res <- list(action = GeneralParameters$action, period = GeneralParameters$period,
								station = jlstn, res = qcout, outputdir = outsdir,
								AllOrOne = GeneralParameters$AllOrOne)
				save(ret.res, file = fileoutRdata)

				## Default: not replace outliers if less/greater than limsup
				lenNoRepl <- rep(NA, nrow(qcout))
				resqc <- as.numeric(qcout$values)
				limsup <- as.numeric(GeneralParameters$stnInfo[as.character(GeneralParameters$stnInfo$Station.ID) == jlstn, 2:3])
				lenNoRepl[resqc > limsup[1] & resqc < limsup[2]] <- 1

				qcout <- data.frame(qcout, not.replace = lenNoRepl, change.values = NA)
				write.table(qcout, fileoutTXT, col.names = TRUE, row.names = FALSE)

				##Default: replace by NA (uncomment line below)
				
				xdat[match(qcout$dates[is.na(lenNoRepl)], dates)] <- NA
				msg <- paste("Quality control finished successfully for", jlstn)
				status <- 'ok'
			}else{
				if(GeneralParameters$AllOrOne == 'one') ret.res <- NULL
				if(GeneralParameters$AllOrOne == 'all') ret.qcout <- NULL
				msg <- paste("Quality control failed for", jlstn)
				status <- 'no'
			}
		}else{
			if(GeneralParameters$AllOrOne == 'one') ret.res <- NULL
			if(GeneralParameters$AllOrOne == 'all') ret.qcout <- NULL
			msg <- paste(paste("Quality control failed for", jlstn), '\n', gsub('[\r\n]', '', qcout[1]), sep = '')
			status <- 'no'
		}
		if(GeneralParameters$AllOrOne == 'all'){
			tcl("update")
			ret.res <- list(ret.qcout, jlstn, outsdir)
		}

		# ##create by default
		sdon <- data.frame(dates, xdat)
		write.table(sdon, file_corrected, col.names = FALSE, row.names = FALSE)
		on.exit({
			if(status == 'ok') InsertMessagesTxt(main.txt.out, msg)
			if(status == 'no') InsertMessagesTxt(main.txt.out, msg, format = TRUE)
		})
		return(ret.res)
	}

	#############
	ExecTempALLStations <- function(){
		allstn2loop <- as.character(GeneralParameters$stnInfo$Station.ID)

		# if(doparallel & length(allstn2loop) >= 20){
		# 	klust <- makeCluster(nb_cores)
		# 	registerDoParallel(klust)
		# 	`%parLoop%` <- `%dopar%`
		# 	closeklust <- TRUE
		# }else{
			`%parLoop%` <- `%do%`
			closeklust <- FALSE
		# }

		toExports <- c('allstn2loop', 'EnvQcOutlierData', 'GeneralParameters', 'ExecTempOneStation', 'execQctempFun',
					'txtnQcSpatialCheck', 'txtnQcSingleSeriesCalc', 'mergeQcValues', 'intConsistCheck', 'commonTxTn', 'txtnLinMod', 
					'outlierTxTnQc', 'confTxTnQc', 'tempSigmaTest', 'txtnSpatialQc', 'txtnSpatReg', 'txtnChooseNeignbors',
					'AllOpenFilesData', 'getElevationData', 'getDemOpenDataSPDF', 'grid2pointINDEX',
					'InsertMessagesTxt', 'main.txt.out')
		packages <- c('sp', 'gstat', 'fields', 'tcltk')
		retAllRun <- foreach(jlstn = allstn2loop, .export = toExports, .packages = packages) %parLoop% {
			ExecTempOneStation(jlstn)
		}
		# if(closeklust) stopCluster(klust)
		ret.qcout <- lapply(retAllRun, '[[', 1)
		ret.stnid <- lapply(retAllRun, '[[', 2)
		ret.outdir <- lapply(retAllRun, '[[', 3)
		ret.res <- list(action = GeneralParameters$action, period = GeneralParameters$period,
						station = ret.stnid, res = ret.qcout, outputdir = ret.outdir,
						AllOrOne = GeneralParameters$AllOrOne)
		return(ret.res)
	}

	#############
	if(GeneralParameters$AllOrOne == 'one') ret.res <- ExecTempOneStation(get.stn)
	if(GeneralParameters$AllOrOne == 'all') ret.res <- ExecTempALLStations()
	return(ret.res)
}

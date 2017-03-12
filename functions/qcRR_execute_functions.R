
getElevationData <- function(){
	if(!is.null(EnvQcOutlierData$donnees)){
		###get elevation data
		msg <- NULL
		elv_dem <- NULL
		if(!GeneralParameters$stn.type$single.series){
			elv_stn <- EnvQcOutlierData$donnees$elv

			if(GeneralParameters$dem$use.elv){
				if(GeneralParameters$dem$interp.dem == "0"){
					if(GeneralParameters$IO.files$DEM.file == "") msg <- 'There is no elevation data in format NetCDF'
					else{
						demData <- getDemOpenDataSPDF(GeneralParameters$IO.files$DEM.file)
						ijGrd <- grid2pointINDEX(list(lon = EnvQcOutlierData$donnees$lon, lat = EnvQcOutlierData$donnees$lat),
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

execQcrainFun <- function(get.stn){
	status <- 0
	msg <- NULL

	thres <- GeneralParameters$select.pars$conf.lev
	min.stn <- GeneralParameters$select.pars$min.stn
	R <- GeneralParameters$select.pars$max.dist
	elv.diff <- GeneralParameters$select.pars$elv.diff
	spthres <- c(min.stn, R, elv.diff)

	bounds <- GeneralParameters$stnInfo
	interp.dem <- GeneralParameters$dem$interp.dem

	IsPmax <- GeneralParameters$sp.check$ispmax
	IsPobs <- GeneralParameters$sp.check$ispobs
	IsDmin <- GeneralParameters$sp.check$isdmin
	IsDobs <- GeneralParameters$sp.check$isdobs
	IsDq1 <- GeneralParameters$sp.check$isdq1
	FtlDev <- GeneralParameters$sp.check$ftldev

	if(!GeneralParameters$stn.type$single.series){
		stn.id <- as.character(bounds$Station.ID)
		jstn <- which(stn.id == get.stn)

		limsup <- as.numeric(bounds$Upper.Bounds[jstn])
		ispmax <- rep(IsPmax, 12)
		ispobs <- rep(IsPobs, 12)
		isdmin <- rep(IsDmin, 12)
		isdobs <- rep(IsDobs, 12)
		isdq1 <- rep(IsDq1, 12)
		ftldev <- rep(FtlDev, 12)

		spparam <- cbind(ispmax, ispobs, isdmin, isdobs, isdq1, ftldev)

		jstn1 <- which(as.character(EnvQcOutlierData$donnees$id) == get.stn)
		idstn <- as.character(EnvQcOutlierData$donnees$id)[jstn1]

		if(GeneralParameters$dem$use.elv){
			if(is.null(EnvQcOutlierData$r_elv)){
				r_elv <- getElevationData()
				assign('r_elv', r_elv, envir = EnvQcOutlierData)
			}else{
				if(interp.dem == "0" & (is.null(EnvQcOutlierData$r_elv[[2]]) | EnvQcOutlierData$r_elv[[4]] != GeneralParameters$IO.files$DEM.file)){
					r_elv <- getElevationData()
					EnvQcOutlierData$r_elv[2:4] <- r_elv[2:4]
				}else if(interp.dem == "1" & is.null(EnvQcOutlierData$r_elv[[1]])){
					elv_stn <- EnvQcOutlierData$donnees$elv
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
				if(interp.dem == "0") elv <- r_elv[[2]]
				else elv <- r_elv[[1]]
			}
		}else elv <- NULL

		coords <- cbind(EnvQcOutlierData$donnees$lon, EnvQcOutlierData$donnees$lat)
		dats <- list(EnvQcOutlierData$donnees$data, EnvQcOutlierData$donnees$dates)
		sortie.qc <- rainQcSpatialCheck(jstn1, idstn, dats, coords = coords, elv = elv,
										thres = thres, spthres = spthres, spparam = spparam,
										limsup = limsup, period = GeneralParameters$period)
	}else{
		idstn <- as.character(bounds$Station.ID[1])
		limsup <- bounds$Upper.Bounds[1]
		xdon <- if(EnvQcOutlierData$donnees$nbvar == 3) EnvQcOutlierData$donnees$var$rr else EnvQcOutlierData$donnees$var$var
		dats <- list(xdon, EnvQcOutlierData$donnees$dates)
		sortie.qc <- rainQcSingleSeries(dats, idstn, thres, limsup, period = GeneralParameters$period)
	}
	on.exit({
		if(status == 'ok') InsertMessagesTxt(main.txt.out, msg)
		if(status == 'no') InsertMessagesTxt(main.txt.out, msg, format = TRUE)
	})
	return(sortie.qc)
}

#####################################################

ExecQcRain <- function(get.stn){
	ExecRainOneStation <- function(jlstn){
		status <- 0
		msg <- NULL
		dates <- EnvQcOutlierData$donnees$dates
		if(!GeneralParameters$stn.type$single.series){
			xpos <- which(as.character(EnvQcOutlierData$donnees$id) == jlstn)
			xdat <- EnvQcOutlierData$donnees$data[, xpos]
		}else{
			if(EnvQcOutlierData$donnees$nbvar == 3) xdat <- EnvQcOutlierData$donnees$var$rr
			else xdat <- EnvQcOutlierData$donnees$var$var
		}

		outsdir <- file.path(EnvQcOutlierData$baseDir, 'Outputs', jlstn)
		# ##create by default
		corrdirstn <- file.path(EnvQcOutlierData$baseDir, 'CorrectedData', jlstn)
		if(!file.exists(corrdirstn)) dir.create(corrdirstn, showWarnings = FALSE, recursive = TRUE)
		file_corrected <- file.path(corrdirstn, paste(jlstn, '.txt', sep = ''))

		qcout <- try(execQcrainFun(jlstn), silent = TRUE)
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

				## Default: not replace outliers if less than limsup
				lenNoRepl <- rep(NA, nrow(qcout))
				resqc <- as.numeric(qcout$values)
				limsup <- as.numeric(GeneralParameters$stnInfo$Upper.Bounds[as.character(GeneralParameters$stnInfo$Station.ID) == jlstn])
				lenNoRepl[resqc < limsup] <- 1

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
	ExecRainALLStations <- function(){
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

		toExports <- c('allstn2loop', 'EnvQcOutlierData', 'GeneralParameters', 'ExecRainOneStation', 'execQcrainFun',
					'rainQcSpatialCheck', 'mergeQcValues', 'rainQcSingleSeriesCalc', 'UpperOutlierQc',
					'ConfIntGammaQc', 'rainGammaTest', 'rainSpatialQc', 'rainSpatialCheck', 'repartCheck',
					'AllOpenFilesData', 'getElevationData', 'getDemOpenDataSPDF', 'grid2pointINDEX',  
					'InsertMessagesTxt', 'main.txt.out')
		packages <- c('sp', 'gstat', 'fields', 'tcltk')
		retAllRun <- foreach(jlstn = allstn2loop, .export = toExports, .packages = packages) %parLoop% {
			ExecRainOneStation(jlstn)
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
	if(GeneralParameters$AllOrOne == 'one') ret.res <- ExecRainOneStation(get.stn)
	if(GeneralParameters$AllOrOne == 'all') ret.res <- ExecRainALLStations()
	return(ret.res)
}

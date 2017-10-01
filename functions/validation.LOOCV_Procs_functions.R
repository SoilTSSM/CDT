
LOOCValidationDataProcs <- function(GeneralParameters){

	if(tclvalue(EnvLOOCValidationplot$loocv) == "1"){
		outValidation <- dirname(EnvLOOCValidationplot$file.loocv)
	}else{
		outValidation <- file.path(GeneralParameters$outdir, paste0('LOOCValidation_',
			getf.no.ext(GeneralParameters$STN.file)))
	}

	xvargal <- c("date.range", "aggr.series", "dicho.fcst", "stat.data", "add.to.plot")
	EnvLOOCValidation$GeneralParameters[xvargal] <- GeneralParameters[xvargal]
	GeneralParameters <- EnvLOOCValidation$GeneralParameters
	clim.var <- GeneralParameters$clim.var
	timestep <- GeneralParameters$Tstep
	startYear <- GeneralParameters$date.range$start.year
	endYear <- GeneralParameters$date.range$end.year
	startMonth <- GeneralParameters$date.range$start.month
	endMonth <- GeneralParameters$date.range$end.month

	###################

	dstart <- as.Date(paste0(startYear, "-1-1"))
	dend <- as.Date(paste0(endYear, "-12-31"))

	if(timestep == "daily"){
		dates <- format(seq(dstart, dend, 'day'), '%Y%m%d')
	}
	if(timestep == "pentad"){
		dates <- seq(dstart, dend, 'day')
		dates <- paste0(format(dates[which(as.numeric(format(dates, '%d')) <= 6)], '%Y%m'),
						as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 6)], '%d')))
	}
	if(timestep == "dekadal"){
		dates <- seq(dstart, dend, 'day')
		dates <- paste0(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%Y%m'),
						as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%d')))
	}
	if(timestep == "monthly"){
		dates <- format(seq(dstart, dend, 'month'), '%Y%m')
	}

	seasonLength <- (endMonth-startMonth+1)%%12
	seasonLength <- ifelse(seasonLength == 0, 12, seasonLength)
	monthtoValid <- (startMonth:(startMonth+(seasonLength-1)))%%12
	monthtoValid[monthtoValid == 0] <- 12

	imonValid <- as.numeric(substr(dates, 5, 6))%in%monthtoValid
	dates <- dates[imonValid]

	idaty <- dates%in%EnvLOOCValidation$cdtData$dates
	dates <- dates[idaty]
	dates0 <- dates

	EnvLOOCValidation$opDATA$id <- EnvLOOCValidation$cdtData$info$id
	EnvLOOCValidation$opDATA$lon <- EnvLOOCValidation$cdtData$info$lon
	EnvLOOCValidation$opDATA$lat <- EnvLOOCValidation$cdtData$info$lat

	###################
	AggrSeries <- list(aggr.fun = GeneralParameters$aggr.series$aggr.fun,
						count.fun = GeneralParameters$aggr.series$opr.fun,
						count.thres = GeneralParameters$aggr.series$opr.thres,
						aggr.data = GeneralParameters$aggr.series$aggr.data)

	if(is.null(EnvLOOCValidation$Statistics) |
	   !isTRUE(all.equal(EnvLOOCValidation$opDATA$dates, dates0)) |
	   !isTRUE(all.equal(EnvLOOCValidation$opDATA$AggrSeries, AggrSeries)))
	{
		idx <- match(dates0, EnvLOOCValidation$cdtData$dates)
		EnvLOOCValidation$opDATA$dates <- dates0
		EnvLOOCValidation$opDATA$stn <- EnvLOOCValidation$cdtData$obs[idx, , drop = FALSE]
		EnvLOOCValidation$opDATA$ncdf <- EnvLOOCValidation$cdtData$fcst[idx, , drop = FALSE]

		inNA <- is.na(EnvLOOCValidation$opDATA$stn) | is.na(EnvLOOCValidation$opDATA$ncdf)
		EnvLOOCValidation$opDATA$stn[inNA] <- NA
		EnvLOOCValidation$opDATA$ncdf[inNA] <- NA

		###################
		xtm <- EnvLOOCValidation$opDATA$dates
		if(timestep == "daily") temps <- as.Date(xtm, format = "%Y%m%d")
		if(timestep == "pentad") temps <- as.Date(paste0(substr(xtm, 1, 6), c(3, 8, 13, 18, 23, 28)[as.numeric(substr(xtm, 7, 7))]), format = "%Y%m%d")
		if(timestep == "dekadal") temps <- as.Date(paste0(substr(xtm, 1, 6), c(5, 15, 25)[as.numeric(substr(xtm, 7, 7))]), format = "%Y%m%d")
		if(timestep == "monthly") temps <- as.Date(paste0(xtm, 15), format = "%Y%m%d")

		###################

		if(GeneralParameters$aggr.series$aggr.data){
			InsertMessagesTxt(main.txt.out, 'Aggregate data ...')
			idrow <- seq(length(EnvLOOCValidation$opDATA$dates))
			xmois <- as.numeric(substr(EnvLOOCValidation$opDATA$dates, 5, 6))

			rleMois <- rle(xmois)
			xrank <- cumsum(rleMois$lengths)
			istart <- seq(which(rleMois$values %in% monthtoValid[1])[1], length(rleMois$values), seasonLength)
			iend <- istart + seasonLength - 1
			istart <- xrank[istart]-rleMois$lengths[istart]+1
			debSeas <- as.Date(paste0(substr(EnvLOOCValidation$opDATA$dates[istart], 1, 6), '01'), '%Y%m%d')
			finSeas <- sapply(lapply(debSeas, addMonths, n = seasonLength-1), format, '%Y-%m')

			## end
			nend <- 0
			if(iend[length(iend)] > length(rleMois$lengths)){
				iex <- iend[length(iend)] - length(rleMois$lengths)
				iex <- (rleMois$values[length(rleMois$lengths)] + (1:iex))%%12
				iex[iex == 0] <- 12
				if(timestep == 'daily'){
					nend <- mapply(function(an, mon) rev((28:31)[!is.na(as.Date(paste(an, mon, 28:31, sep = '-')))])[1],
											rep(as.numeric(substr(finSeas[length(finSeas)], 1, 4)), length(iex)), iex)
					nend <- sum(nend)
				}
				if(timestep == 'pentad') nend <- sum(length(iex))*6
				if(timestep == 'dekadal') nend <- sum(length(iex))*3
				if(timestep == 'monthly') nend <- sum(length(iex))

				iend[length(iend)] <- length(rleMois$lengths)
			}
			iend <- xrank[iend]

			## index
			indx <- lapply(seq_along(istart), function(j) idrow[istart[j]:iend[j]])
			len.data <- sapply(indx, length)
			len.data[length(len.data)] <- len.data[length(len.data)] + nend

			AggrcdtData <- lapply(indx, funAggrMAT, DATA = EnvLOOCValidation$opDATA$stn, pars = AggrSeries)
			AggrcdtData <- do.call(rbind, AggrcdtData)
			AggrncdfData <- lapply(indx, funAggrMAT, DATA = EnvLOOCValidation$opDATA$ncdf, pars = AggrSeries)
			AggrncdfData <- do.call(rbind, AggrncdfData)
			missData <- lapply(indx, funMissMAT, DATA = EnvLOOCValidation$opDATA$stn)
			missData <- do.call(rbind, missData)
			inNA <- missData == len.data
			AggrcdtData[inNA] <- NA
			AggrncdfData[inNA] <- NA

			temps <- do.call(c, lapply(indx, function(x) mean(temps[x])))
			InsertMessagesTxt(main.txt.out, 'Aggregating data finished')
		}else{
			AggrcdtData <- EnvLOOCValidation$opDATA$stn
			AggrncdfData <- EnvLOOCValidation$opDATA$ncdf
		}

		EnvLOOCValidation$opDATA$stnStatData <- AggrcdtData
		EnvLOOCValidation$opDATA$ncStatData <- AggrncdfData
		EnvLOOCValidation$opDATA$temps <- temps
		EnvLOOCValidation$opDATA$AggrSeries <- AggrSeries
	}

	InsertMessagesTxt(main.txt.out, 'Calculate statistics ...')

	AggrcdtData <- EnvLOOCValidation$opDATA$stnStatData
	AggrncdfData <- EnvLOOCValidation$opDATA$ncStatData
	dichotomous <- GeneralParameters$dicho.fcst

	EnvLOOCValidation$Statistics$STN <- ValidationStatisticsFun(AggrcdtData, AggrncdfData, dichotomous)
	EnvLOOCValidation$Statistics$ALL <- ValidationStatisticsFun(matrix(AggrcdtData, ncol = 1),
												matrix(AggrncdfData, ncol = 1), dichotomous)
	EnvLOOCValidation$Statistics$AVG <- ValidationStatisticsFun(matrix(rowMeans(AggrcdtData, na.rm = TRUE), ncol = 1),
												matrix(rowMeans(AggrncdfData, na.rm = TRUE), ncol = 1), dichotomous)

	InsertMessagesTxt(main.txt.out, 'Statistics calculation done!')

	fileValidOut <- file.path(outValidation, "VALIDATION_DATA_OUT.rds")
	saveRDS(EnvLOOCValidation, file = fileValidOut)

	###################
	# Plot to files

	###################
	# write to table
	dirSTATS <- file.path(outValidation, "STATS_DATA")
	dir.create(dirSTATS, showWarnings = FALSE, recursive = TRUE)

	stat.ALL <- EnvLOOCValidation$Statistics$ALL
	stat.ALL <- data.frame(Name = rownames(stat.ALL$statistics), Statistics = stat.ALL$statistics, Description = stat.ALL$description)
	file.stat.all <- file.path(dirSTATS, "All_Data_Statistics.csv")
	writeFiles(stat.ALL, file.stat.all)

	stat.AVG <- EnvLOOCValidation$Statistics$AVG
	stat.AVG <- data.frame(Name = rownames(stat.AVG$statistics), Statistics = stat.AVG$statistics, Description = stat.AVG$description)
	file.stat.avg <- file.path(dirSTATS, "Spatial_Average_Statistics.csv")
	writeFiles(stat.AVG, file.stat.avg)

	headinfo <- cbind(c("Station", "LON", "STATS/LAT"), do.call(rbind, EnvLOOCValidation$opDATA[c('id', 'lon', 'lat')]))
	stat.STN <- EnvLOOCValidation$Statistics$STN$statistics
	stat.STN <- cbind(rownames(stat.STN), stat.STN)
	stat.STN <- rbind(headinfo, stat.STN)
	file.stat.stn <- file.path(dirSTATS, "Stations_Statistics.csv")
	writeFiles(stat.STN, file.stat.stn)

	return(0)
}





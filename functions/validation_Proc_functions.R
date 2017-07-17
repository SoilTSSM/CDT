
HandOutValidationDataProcs <- function(GeneralParameters){
	clim.var <- GeneralParameters$clim.var
	timestep <- GeneralParameters$stn.file$tstep
	startYear <- GeneralParameters$date.range$start.year
	endYear <- GeneralParameters$date.range$end.year
	startMonth <- GeneralParameters$date.range$start.month
	endMonth <- GeneralParameters$date.range$end.month

	outValidation <- file.path(GeneralParameters$outdir, paste0('Validation_', getf.no.ext(GeneralParameters$stn.file$file)))
	dir.create(outValidation, showWarnings = FALSE, recursive = TRUE)
	EnvHOValidation$GeneralParameters <- GeneralParameters

	###################

	stnInfo <- getStnOpenDataInfo(GeneralParameters$stn.file$file)
	if(!is.null(EnvHOValidation$cdtData)){
		if(!isTRUE(all.equal(EnvHOValidation$cdtData$stnInfo, stnInfo))){
			readcdtData <- TRUE
			EnvHOValidation$cdtData <- NULL
		}else readcdtData <- FALSE
	}else readcdtData <- TRUE

	if(readcdtData){
		cdtTmpVar <- getStnOpenData(GeneralParameters$stn.file$file)
		if(is.null(cdtTmpVar)) return(NULL)
		cdtTmpVar <- getCDTdataAndDisplayMsg(cdtTmpVar, timestep)
		if(is.null(cdtTmpVar)) return(NULL)
		cdtTmpVar <- cdtTmpVar[c('id', 'lon', 'lat', 'dates', 'data')]
		cdtTmpVar$index <- seq_along(cdtTmpVar$dates)
		# cdtTmpVar$colInfo <- seq_along(cdtTmpVar$id)
		EnvHOValidation$cdtData <- cdtTmpVar
		EnvHOValidation$cdtData$stnInfo <- stnInfo
		rm(data.cdtTmpVar)
	}

	###################

	if(GeneralParameters$type.select != "all"){
		if(GeneralParameters$type.select == "rect"){
			ilon <- EnvHOValidation$cdtData$lon >= GeneralParameters$Geom$minlon &
					EnvHOValidation$cdtData$lon <= GeneralParameters$Geom$maxlon
			ilat <- EnvHOValidation$cdtData$lat >= GeneralParameters$Geom$minlat &
					EnvHOValidation$cdtData$lat <= GeneralParameters$Geom$maxlat
			ixy <- ilon & ilat
		}

		if(GeneralParameters$type.select == "poly"){
			shp.dat <- getShpOpenData(GeneralParameters$shp.file$shp)[[2]]
			shp <- shp.dat[as.character(shp.dat@data[, GeneralParameters$shp.file$attr]) == GeneralParameters$Geom$namePoly, ]
			pts.dat <- data.frame(x = EnvHOValidation$cdtData$lon, y = EnvHOValidation$cdtData$lat)
			coordinates(pts.dat)<- ~x+y
			ixy <- unname(!is.na(over(pts.dat, geometry(shp))))
		}
		if(!any(ixy)){
			InsertMessagesTxt(main.txt.out, 'The selection did not contain any stations', format = TRUE)
			return(NULL)
		}
	}else ixy <- rep(TRUE, length(EnvHOValidation$cdtData$lon))

	###################

	dstart <- as.Date(paste0(startYear, "-1-1"))
	dend <- as.Date(paste0(endYear, "-12-31"))

	if(timestep == "daily"){
		dates <- format(seq(dstart, dend, 'day'), '%Y%m%d')
		ncfiles <- sprintf(GeneralParameters$ncdf.file$format, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 8))
	}

	if(timestep == "dekadal"){
		dates <- seq(dstart, dend, 'day')
		dates <- paste0(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%Y%m'),
						as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%d')))
		ncfiles <- sprintf(GeneralParameters$ncdf.file$format, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 7))
	}
	if(timestep == "monthly"){
		dates <- format(seq(dstart, dend, 'month'), '%Y%m')
		ncfiles <- sprintf(GeneralParameters$ncdf.file$format, substr(dates, 1, 4), substr(dates, 5, 6))
	}

	seasonLength <- (endMonth-startMonth+1)%%12
	seasonLength <- ifelse(seasonLength == 0, 12, seasonLength)
	monthtoValid <- (startMonth:(startMonth+(seasonLength-1)))%%12
	monthtoValid[monthtoValid == 0] <- 12
	imonValid <- as.numeric(substr(dates, 5, 6))%in%monthtoValid
	dates <- dates[imonValid]
	ncfiles <- ncfiles[imonValid]

	ncPATH <- file.path(GeneralParameters$ncdf.file$dir, ncfiles)
	ncflExist <- unlist(lapply(ncPATH, file.exists))
	if(!any(ncflExist)){
		InsertMessagesTxt(main.txt.out, "Unable to locate netcdf files", format = TRUE)
		return(NULL)
	}

	dates <- dates[ncflExist]
	ncPATH <- ncPATH[ncflExist]
	dates0 <- dates

	if(length(intersect(EnvHOValidation$cdtData$dates, dates)) == 0){
		InsertMessagesTxt(main.txt.out, "Station and netcdf dates did not overlap", format = TRUE)
		return(NULL)
	}

	###################

	ncInfo <- getRFESampleData(GeneralParameters$ncdf.file$sample)
	if(is.null(ncInfo)){
		InsertMessagesTxt(main.txt.out, "No netcdf sample file found", format = TRUE)
		return(NULL)
	}

	nc <- nc_open(ncPATH[1])
	lon <- nc$dim[[ncInfo$rfeILon]]$vals
	lat <- nc$dim[[ncInfo$rfeILat]]$vals
	nc_close(nc)
	xo <- order(lon)
	lon <- lon[xo]
	yo <- order(lat)
	lat <- lat[yo]
	nclon <- length(lon)
	nclat <- length(lat)

	stnCoords <- list(lon = EnvHOValidation$cdtData$lon, lat = EnvHOValidation$cdtData$lat)
	ijx <- grid2pointINDEX(stnCoords, list(lon = lon, lat = lat))

	###################

	ncdataInfo <- c(GeneralParameters$ncdf.file$dir, GeneralParameters$ncdf.file$format)
	bindncdfData <- FALSE
	if(!is.null(EnvHOValidation$ncdfData)){
		iexist <- dates%in%EnvHOValidation$ncdfData$dates
		if(all(iexist)){
			if(!isTRUE(all.equal(EnvHOValidation$ncdfData$ncdataInfo, ncdataInfo))){
				readNcdfData <- TRUE
				EnvHOValidation$ncdfData <- NULL
			}else readNcdfData <- FALSE
		}else{
			if(isTRUE(all.equal(EnvHOValidation$ncdfData$ncdataInfo, ncdataInfo))){
				bindncdfData <- TRUE
				if(any(iexist)){
					dates <- dates[!iexist]
					ncPATH <- ncPATH[!iexist]
				}
			}else EnvHOValidation$ncdfData <- NULL
			readNcdfData <- TRUE
		}
	}else readNcdfData <- TRUE

	if(readNcdfData){
		InsertMessagesTxt(main.txt.out, 'Read and extract netcdf data ...')
		is.parallel <- doparallel(length(ncPATH) >= 180)
		`%parLoop%` <- is.parallel$dofun

		ncData <- foreach(jj = seq_along(ncPATH), .packages = "ncdf4") %parLoop% {
			nc <- nc_open(ncPATH[jj])
			vars <- ncvar_get(nc, varid = ncInfo$rfeVarid)
			nc_close(nc)
			vars <- vars[xo, yo]
			if(ncInfo$rfeILat < ncInfo$rfeILon){
				vars <- matrix(c(vars), nrow = nclon, ncol = nclat, byrow = TRUE)
			}
			vars[ijx]
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
		InsertMessagesTxt(main.txt.out, 'Reading netcdf data finished')

		ncData <- do.call(rbind, ncData)

		cdtTmpVar <- NULL
		idx <- seq(length(dates))
		if(bindncdfData){
			cdtTmpVar$data <- rbind(EnvHOValidation$ncdfData$data, ncData)
			cdtTmpVar$dates <- c(EnvHOValidation$ncdfData$dates, dates)
			cdtTmpVar$index <- c(EnvHOValidation$ncdfData$index, max(EnvHOValidation$ncdfData$index)+idx)
		}else{
			cdtTmpVar$data <- ncData
			cdtTmpVar$dates <- dates
			cdtTmpVar$index <- idx
		}
		odaty <- order(cdtTmpVar$dates)
		cdtTmpVar$dates <- cdtTmpVar$dates[odaty]
		cdtTmpVar$index <- cdtTmpVar$index[odaty]

		cdtTmpVar$ncdataInfo <- ncdataInfo
		EnvHOValidation$ncdfData <- cdtTmpVar
		rm(cdtTmpVar, ncData, odaty, idx)
	}

	###################
	AggrSeries <- list(aggr.fun = GeneralParameters$aggr.series$aggr.fun,
						count.fun = GeneralParameters$aggr.series$opr.fun,
						count.thres = GeneralParameters$aggr.series$opr.thres,
						aggr.data = GeneralParameters$aggr.series$aggr.data)

	if(readcdtData | readNcdfData | is.null(EnvHOValidation$Statistics) |
		!isTRUE(all.equal(EnvHOValidation$opDATA$dates, dates0)) |
		!isTRUE(all.equal(EnvHOValidation$opDATA$ixy, ixy)) |
		!isTRUE(all.equal(EnvHOValidation$opDATA$AggrSeries, AggrSeries)))
	{
		idx.stn <- EnvHOValidation$cdtData$index[match(dates0, EnvHOValidation$cdtData$dates)]
		idx.ncdf <- EnvHOValidation$ncdfData$index[match(dates0, EnvHOValidation$ncdfData$dates)]
		EnvHOValidation$opDATA$dates <- dates0
		EnvHOValidation$opDATA$ixy <- ixy
		EnvHOValidation$opDATA$id <- EnvHOValidation$cdtData$id[ixy]
		EnvHOValidation$opDATA$lon <- EnvHOValidation$cdtData$lon[ixy]
		EnvHOValidation$opDATA$lat <- EnvHOValidation$cdtData$lat[ixy]
		EnvHOValidation$opDATA$stn <- EnvHOValidation$cdtData$data[idx.stn, ixy, drop = FALSE]
		EnvHOValidation$opDATA$ncdf <- EnvHOValidation$ncdfData$data[idx.ncdf, ixy, drop = FALSE]

		inNA <- is.na(EnvHOValidation$opDATA$stn) | is.na(EnvHOValidation$opDATA$ncdf)
		EnvHOValidation$opDATA$stn[inNA] <- NA
		EnvHOValidation$opDATA$ncdf[inNA] <- NA

		###################

		if(GeneralParameters$aggr.series$aggr.data){
			InsertMessagesTxt(main.txt.out, 'Aggregate data ...')
			idrow <- seq(length(EnvHOValidation$opDATA$dates))
			xmois <- as.numeric(substr(EnvHOValidation$opDATA$dates, 5, 6))

			rleMois <- rle(xmois)
			xrank <- cumsum(rleMois$lengths)
			istart <- seq(which(rleMois$values %in% monthtoValid[1])[1], length(rleMois$values), seasonLength)
			iend <- istart + seasonLength - 1
			istart <- xrank[istart]-rleMois$lengths[istart]+1
			debSeas <- as.Date(paste0(substr(EnvHOValidation$opDATA$dates[istart], 1, 6), '01'), '%Y%m%d')
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
				if(timestep == 'dekadal') nend <- sum(length(iex))*3
				if(timestep == 'monthly') nend <- sum(length(iex))

				iend[length(iend)] <- length(rleMois$lengths)
			}
			iend <- xrank[iend]

			## index
			indx <- lapply(seq_along(istart), function(j) idrow[istart[j]:iend[j]])
			len.data <- sapply(indx, length)
			len.data[length(len.data)] <- len.data[length(len.data)] + nend

			AggrcdtData <- lapply(indx, funAggrMAT, DATA = EnvHOValidation$opDATA$stn, pars = AggrSeries)
			AggrcdtData <- do.call(rbind, AggrcdtData)
			AggrncdfData <- lapply(indx, funAggrMAT, DATA = EnvHOValidation$opDATA$ncdf, pars = AggrSeries)
			AggrncdfData <- do.call(rbind, AggrncdfData)
			missData <- lapply(indx, funMissMAT, DATA = EnvHOValidation$opDATA$stn)
			missData <- do.call(rbind, missData)
			inNA <- missData == len.data
			AggrcdtData[inNA] <- NA
			AggrncdfData[inNA] <- NA
			InsertMessagesTxt(main.txt.out, 'Aggregating data finished')
		}else{
			AggrcdtData <- EnvHOValidation$opDATA$stn
			AggrncdfData <- EnvHOValidation$opDATA$ncdf
		}

		EnvHOValidation$opDATA$stnStatData <- AggrcdtData
		EnvHOValidation$opDATA$ncStatData <- AggrncdfData
		EnvHOValidation$opDATA$AggrSeries <- AggrSeries

		InsertMessagesTxt(main.txt.out, 'Calculate statistics ...')
		dichotomous <- clim.var == 'RR' & timestep == 'daily' & !GeneralParameters$aggr.series$aggr.data
		EnvHOValidation$Statistics$STN <- ValidationStatisticsFun(AggrcdtData, AggrncdfData, dichotomous)
		EnvHOValidation$Statistics$ALL <- ValidationStatisticsFun(matrix(AggrcdtData, ncol = 1),
															matrix(AggrncdfData, ncol = 1), dichotomous)
		EnvHOValidation$Statistics$AVG <- ValidationStatisticsFun(matrix(rowMeans(AggrcdtData, na.rm = TRUE), ncol = 1),
										matrix(rowMeans(AggrncdfData, na.rm = TRUE), ncol = 1), dichotomous)
		InsertMessagesTxt(main.txt.out, 'Statistics calculation done!')

		fileValidOut <- file.path(outValidation, "VALIDATION_DATA_OUT.rds")
		save(EnvHOValidation, file = fileValidOut)

		###################
		# Plot to files

		###################
		# write to table
		dirSTATS <- file.path(outValidation, "STATS_DATA")
		dir.create(dirSTATS, showWarnings = FALSE, recursive = TRUE)

		stat.ALL <- EnvHOValidation$Statistics$ALL
		stat.ALL <- data.frame(Name = rownames(stat.ALL$statistics), Statistics = stat.ALL$statistics, Description = stat.ALL$description)
		file.stat.all <- file.path(dirSTATS, "All_Data_Statistics.csv")
		writeFiles(stat.ALL, file.stat.all)

		stat.AVG <- EnvHOValidation$Statistics$AVG
		stat.AVG <- data.frame(Name = rownames(stat.AVG$statistics), Statistics = stat.AVG$statistics, Description = stat.AVG$description)
		file.stat.avg <- file.path(dirSTATS, "Spatial_Average_Statistics.csv")
		writeFiles(stat.AVG, file.stat.avg)

		headinfo <- cbind(c("Station", "LON", "STATS/LAT"), do.call(rbind, EnvHOValidation$opDATA[c('id', 'lon', 'lat')]))
		stat.STN <- EnvHOValidation$Statistics$STN$statistics
		stat.STN <- cbind(rownames(stat.STN), stat.STN)
		stat.STN <- rbind(headinfo, stat.STN)
		file.stat.stn <- file.path(dirSTATS, "Stations_Statistics.csv")
		writeFiles(stat.STN, file.stat.stn)
	}

	return(0)
}





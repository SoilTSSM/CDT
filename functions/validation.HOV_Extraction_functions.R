
HOV_DataExtraction <- function(GeneralParameters){
	if(GeneralParameters$outdir%in%c("", "NA")){
		InsertMessagesTxt(main.txt.out, "Directory to save results doesn't exist", format = TRUE)
		return(NULL)
	}

	outValidation <- file.path(GeneralParameters$outdir, paste0('HOValidation_',
								getf.no.ext(GeneralParameters$STN.file)))
	dir.create(outValidation, showWarnings = FALSE, recursive = TRUE)

	freqData <- GeneralParameters$Tstep

	##################
	## Get data

	stnInfo <- getStnOpenDataInfo(GeneralParameters$STN.file)
	if(!is.null(EnvHOValidation$stnData)){
		if(!isTRUE(all.equal(EnvHOValidation$stnData$stnInfo, stnInfo))){
			readstnData <- TRUE
			EnvHOValidation$stnData <- NULL
		}else readstnData <- FALSE
	}else readstnData <- TRUE

	if(readstnData){
		cdtTmpVar <- getStnOpenData(GeneralParameters$STN.file)
		cdtTmpVar <- getCDTdataAndDisplayMsg(cdtTmpVar, freqData)
		if(is.null(cdtTmpVar)) return(NULL)
		cdtTmpVar <- cdtTmpVar[c('id', 'lon', 'lat', 'dates', 'data')]
		cdtTmpVar$index <- seq_along(cdtTmpVar$dates)
		EnvHOValidation$stnData <- cdtTmpVar
		EnvHOValidation$stnData$stnInfo <- stnInfo
		rm(cdtTmpVar)
	}

	###################
	## define geometry
	if(GeneralParameters$type.select != "all"){
		if(GeneralParameters$type.select == "rect"){
			ilon <- EnvHOValidation$stnData$lon >= GeneralParameters$Geom$minlon &
					EnvHOValidation$stnData$lon <= GeneralParameters$Geom$maxlon
			ilat <- EnvHOValidation$stnData$lat >= GeneralParameters$Geom$minlat &
					EnvHOValidation$stnData$lat <= GeneralParameters$Geom$maxlat
			ixy <- ilon & ilat
		}

		if(GeneralParameters$type.select == "poly"){
			shp.dat <- getShpOpenData(GeneralParameters$shp.file$shp)[[2]]
			shp <- shp.dat[as.character(shp.dat@data[, GeneralParameters$shp.file$attr]) == GeneralParameters$Geom$namePoly, ]
			pts.dat <- data.frame(x = EnvHOValidation$stnData$lon, y = EnvHOValidation$stnData$lat)
			coordinates(pts.dat)<- ~x+y
			ixy <- unname(!is.na(over(pts.dat, geometry(shp))))
		}
		if(!any(ixy)){
			InsertMessagesTxt(main.txt.out, 'The selection did not contain any stations', format = TRUE)
			return(NULL)
		}
	}else ixy <- rep(TRUE, length(EnvHOValidation$stnData$lon))

	##################
	## RFE sample file
	ncDataInfo <- getRFESampleData(GeneralParameters$ncdf.file$sample)
	if(is.null(ncDataInfo)){
		InsertMessagesTxt(main.txt.out, "No netcdf data sample found", format = TRUE)
		return(NULL)
	}

	##################
	## Get NCDF data info
	start.year <- GeneralParameters$Extract.Date$start.year
	start.mon <- GeneralParameters$Extract.Date$start.mon
	start.dek <- GeneralParameters$Extract.Date$start.dek
	end.year <- GeneralParameters$Extract.Date$end.year
	end.mon <- GeneralParameters$Extract.Date$end.mon
	end.dek <- GeneralParameters$Extract.Date$end.dek
	months <- GeneralParameters$Extract.Date$Months
	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	NCDF.DIR <- GeneralParameters$ncdf.file$dir
	NCDF.Format <- GeneralParameters$ncdf.file$format

	errmsg <- "NCDF data not found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, NCDF.DIR, NCDF.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- list(xo = ncDataInfo$rfeILon, yo = ncDataInfo$rfeILat, varid = ncDataInfo$rfeVarid)

	##################

	if(length(intersect(EnvHOValidation$stnData$dates, ncInfo$dates[ncInfo$exist])) == 0){
		InsertMessagesTxt(main.txt.out, "Station and netcdf dates did not overlap", format = TRUE)
		return(NULL)
	}

	##################

	dates <- ncInfo$date[ncInfo$exist]
	ncPATH <- ncInfo$nc.files[ncInfo$exist]

	##################

	nc <- nc_open(ncPATH[1])
	lon <- nc$dim[[ncInfo$ncinfo$xo]]$vals
	lat <- nc$dim[[ncInfo$ncinfo$yo]]$vals
	nc_close(nc)
	xo <- order(lon)
	lon <- lon[xo]
	yo <- order(lat)
	lat <- lat[yo]
	nclon <- length(lon)
	nclat <- length(lat)

	stnCoords <- list(lon = EnvHOValidation$stnData$lon, lat = EnvHOValidation$stnData$lat)
	ijx <- grid2pointINDEX(stnCoords, list(lon = lon, lat = lat))

	##################

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
			vars <- ncvar_get(nc, varid = ncInfo$ncinfo$varid)
			nc_close(nc)
			vars <- vars[xo, yo]
			if(ncInfo$ncinfo$yo < ncInfo$ncinfo$xo){
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

	idx.stn <- match(EnvHOValidation$ncdfData$dates, EnvHOValidation$stnData$dates)
	idx.stn <- EnvHOValidation$stnData$index[na.omit(idx.stn)]
	idx.ncdf <- match(EnvHOValidation$stnData$dates, EnvHOValidation$ncdfData$dates)
	idx.ncdf <- EnvHOValidation$ncdfData$index[na.omit(idx.ncdf)]

	EnvHOValidation$cdtData$info$id <- EnvHOValidation$stnData$id[ixy]
	EnvHOValidation$cdtData$info$lon <- EnvHOValidation$stnData$lon[ixy]
	EnvHOValidation$cdtData$info$lat <- EnvHOValidation$stnData$lat[ixy]
	EnvHOValidation$cdtData$dates <- EnvHOValidation$stnData$dates[idx.stn]
	EnvHOValidation$cdtData$obs <- EnvHOValidation$stnData$data[idx.stn, ixy, drop = FALSE]
	EnvHOValidation$cdtData$fcst <- EnvHOValidation$ncdfData$data[idx.ncdf, ixy, drop = FALSE]
	EnvHOValidation$GeneralParameters <- GeneralParameters

	xhead <- cbind(c("STN", "LON", "DATE/LAT"), rbind(EnvHOValidation$cdtData$info$id,
					EnvHOValidation$cdtData$info$lon, EnvHOValidation$cdtData$info$lat))
	obs2file <- rbind(xhead, cbind(EnvHOValidation$cdtData$dates, EnvHOValidation$cdtData$obs))
	fcst2file <- rbind(xhead, cbind(EnvHOValidation$cdtData$dates, EnvHOValidation$cdtData$fcst))
	obs2file[is.na(obs2file)] <- -99
	fcst2file[is.na(fcst2file)] <- -99

	dirCDTdata <- file.path(outValidation, "OBS_GRD_DATA")
	dir.create(dirCDTdata, showWarnings = FALSE, recursive = TRUE)
	writeFiles(obs2file, file.path(dirCDTdata, "Observations.csv"))
	writeFiles(fcst2file, file.path(dirCDTdata, "Gridded_at_Obs_Loc.csv"))

	fileValidOut <- file.path(outValidation, "VALIDATION_DATA_OUT.rds")
	saveRDS(EnvHOValidation, file = fileValidOut)

	InsertMessagesTxt(main.txt.out, 'Data extraction finished')
	return(0)
}


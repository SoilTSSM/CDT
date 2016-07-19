
TreatbiasRain <- function(origdir){
	freqData <- GeneralParameters$period
	file.pars <- as.character(GeneralParameters$file.io$Values)
	dir.create(origdir, showWarnings = FALSE)

	all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))
	jfile <- which(all.open.file == file.pars[1])
	donne <- AllOpenFilesData[[jfile]][[2]]

	#######get data
	donne <- splitCDTData(donne, freqData)
	if(is.null(donne)) return(NULL)
	stn.lon <- donne$lon
	stn.lat <- donne$lat
	stn.id <- donne$id
	elv <- donne$elv
	dates <- donne$dates
	donne <- donne$data

	#if(nrow(donne$stnDuplCoords) > 0)  #diplay table
	#if(nrow(dat$stnMissCoords) > 0)

	stnlist <- list(id = stn.id, lon = stn.lon, lat = stn.lat, elv = elv, dates = dates, data = donne)

	###get elevation data
	if(GeneralParameters$CreateGrd == "2"){
		jncdf <- which(all.open.file == file.pars[2])
		fdem <- AllOpenFilesData[[jncdf]][[2]]
		dem <- fdem$value
		dem[dem < 0] <- 0
		dem.coord <- data.frame(expand.grid(lon = fdem$x, lat = fdem$y))
		coordinates(dem.coord) = ~lon+lat
		demdf <- data.frame(dem = c(dem))
		demdf <- SpatialPointsDataFrame(coords = dem.coord, data = demdf, proj4string = CRS(as.character(NA)))
		demlist <- list(lon = fdem$x, lat = fdem$y, demGrd = demdf)
	}else demlist <- NULL

	##RFE sample file
	jrfe <- which(all.open.file == file.pars[3])
	ncrfe <- AllOpenFilesData[[jrfe]][[2]]
	rfe.coord <- data.frame(expand.grid(lon = ncrfe$x, lat = ncrfe$y))
	coordinates(rfe.coord) = ~lon+lat
	rfelist <- list(lon = ncrfe$x, lat = ncrfe$y, rfeGrd = rfe.coord, rfeVarid = ncrfe$varid, rfeILon = ncrfe$ilon, rfeILat = ncrfe$ilat, irevlat = ncrfe$irevlat)

	mrgRaindat <- list(stnData = stnlist, demData = demlist, rfeData = rfelist)
	outfile <- file.path(origdir, 'DataUsed2ComputeBias.RData', fsep = .Platform$file.sep)
	save(mrgRaindat, file = outfile)

	return(mrgRaindat)
}

##############################################################################################################

execBiasRain <- function(origdir){

	freqData <- GeneralParameters$period
	mrgRaindat <- TreatbiasRain(origdir)
	if(is.null(mrgRaindat)) return(NULL)

	create.grd <- as.character(GeneralParameters$CreateGrd)
	##Create grid for interpolation
	if(create.grd == '1'){
		grd.lon <- mrgRaindat$rfeData$lon
		grd.lat <- mrgRaindat$rfeData$lat
		nlon0 <- length(grd.lon)
		nlat0 <- length(grd.lat)
	}else if(create.grd == '2'){
		grd.lon <- mrgRaindat$demData$lon
		grd.lat <- mrgRaindat$demData$lat
		nlon0 <- length(grd.lon)
		nlat0 <- length(grd.lat)
	}else if(create.grd == '3'){
		X0 <- as.numeric(as.character(GeneralParameters$new.grid$Values[1]))
		X1 <- as.numeric(as.character(GeneralParameters$new.grid$Values[2]))
		pX <- as.numeric(as.character(GeneralParameters$new.grid$Values[3]))
		Y0 <- as.numeric(as.character(GeneralParameters$new.grid$Values[4]))
		Y1 <- as.numeric(as.character(GeneralParameters$new.grid$Values[5]))
		pY <- as.numeric(as.character(GeneralParameters$new.grid$Values[6]))

		grd.lon <- seq(X0, X1, pX)
		nlon0 <- length(grd.lon)
		grd.lat <- seq(Y0, Y1, pY)
		nlat0 <- length(grd.lat)
	}

	newlocation.grid <- expand.grid(lon = grd.lon, lat = grd.lat)
	coordinates(newlocation.grid)<- ~lon+lat
	newlocation.grid <- SpatialPixels(points = newlocation.grid, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))

	##Gauge data
	stn.lon <- mrgRaindat$stnData$lon
	stn.lat <- mrgRaindat$stnData$lat

	#Index of new grid over stations
	stn.loc <- data.frame(lon = stn.lon, lat = stn.lat)
	stn.loc <- SpatialPoints(stn.loc)
	ijGrd <- over(stn.loc, geometry(newlocation.grid))

	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", grd.lon)
	dy <- ncdim_def("Lat", "degreeN", grd.lat)
	xy.dim <- list(dx, dy)

	paramGrd <- list(nlon0 = nlon0, nlat0 = nlat0, xy.dim = xy.dim, newlocation.grid = newlocation.grid)
	mrgRaindat$stnData$paramGrd <- paramGrd
	outfile <- file.path(origdir, 'DataUsed2ComputeBias.RData', fsep = .Platform$file.sep)
	save(mrgRaindat, file = outfile)

	rfe_stn <- ExtractRFE2Stn(ijGrd, GeneralParameters, mrgRaindat)
	if(is.null(rfe_stn)) return(NULL)
	ret <- ComputeMeanBiasRain(rfe_stn,GeneralParameters, mrgRaindat, paramGrd, origdir)
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

##############################################################################################################

TreatAdjBiasRain <- function(origdir, freqData){

	file.pars <- as.character(GeneralParameters$file.io$Values)
	dir.create(origdir, showWarnings = FALSE)
	all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))

	jfile <- which(all.open.file == file.pars[1])
	donne <- AllOpenFilesData[[jfile]][[2]]

	#######get data
	donne <- splitCDTData(donne, freqData)
	if(is.null(donne)) return(NULL)
	stn.lon <- donne$lon
	stn.lat <- donne$lat
	stn.id <- donne$id
	elv <- donne$elv
	dates <- donne$dates
	donne <- donne$data

	#if(nrow(donne$stnDuplCoords) > 0)  #diplay table
	#if(nrow(dat$stnMissCoords) > 0)

	stnlist <- list(id = stn.id, lon = stn.lon, lat = stn.lat, elv = elv, dates = dates, data = donne)

	##RFE sample file
	jrfe <- which(all.open.file == file.pars[2])
	ncrfe <- AllOpenFilesData[[jrfe]][[2]]
	rfe.coord <- data.frame(expand.grid(lon = ncrfe$x, lat = ncrfe$y))
	coordinates(rfe.coord) = ~lon+lat
	rfelist <- list(lon = ncrfe$x, lat = ncrfe$y, rfeGrd = rfe.coord, rfeVarid = ncrfe$varid, rfeILon = ncrfe$ilon, rfeILat = ncrfe$ilat, irevlat = ncrfe$irevlat)

	mrgRaindat <- list(stnData = stnlist, demData = demlist, rfeData = rfelist)
	#outfile <- file.path(origdir, 'DataUsed2Merge.RData', fsep = .Platform$file.sep)
	#save(mrgRaindat, file = outfile)

	return(mrgRaindat)
}

##############################################################################################################
execAdjBiasRain <- function(origdir){

	dir.create(origdir, showWarnings = FALSE)
	all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))

	jrfe <- which(all.open.file == as.character(GeneralParameters$file.io$Values[1]))
	ncrfe <- AllOpenFilesData[[jrfe]][[2]]
	rfeData <- list(rfeVarid = ncrfe$varid, rfeILon = ncrfe$ilon, rfeILat = ncrfe$ilat, irevlat = ncrfe$irevlat)

	dataBiasfl <- file.path(as.character(GeneralParameters$file.io$Values[3]),'DataUsed2ComputeBias.RData', fsep = .Platform$file.sep)

	load(dataBiasfl)
	paramGrd <- mrgRaindat$stnData$paramGrd

	freqData <- GeneralParameters$period
	datesSE <- as.numeric(as.character(GeneralParameters$dates.adj$Values))

	istart <- as.Date(paste(datesSE[1], datesSE[2], datesSE[3], sep = '-'))
	iend <- as.Date(paste(datesSE[4], datesSE[5], datesSE[6], sep = '-'))
	if(freqData == 'dekadal'){
		istart <- paste(format(istart,'%Y%m'), as.numeric(format(istart,'%d')), sep = '')
		iend <- paste(format(iend,'%Y%m'), as.numeric(format(iend,'%d')), sep = '')
	}else{
		istart <- format(istart,'%Y%m%d')
		iend <- format(iend,'%Y%m%d')
	}

	ret <- AjdMeanBiasRain(freqData, istart, iend, rfeData, paramGrd, GeneralParameters, origdir)
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

##############################################################################################################
####Merging

TreatMergeRain <- function(origdir, freqData){

	file.pars <- as.character(GeneralParameters$file.io$Values)
	dir.create(origdir, showWarnings = FALSE)

	all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))

	jfile <- which(all.open.file == file.pars[1])
	donne <- AllOpenFilesData[[jfile]][[2]]

	#######get data
	donne <- splitCDTData(donne, freqData)
	if(is.null(donne)) return(NULL)
	stn.lon <- donne$lon
	stn.lat <- donne$lat
	stn.id <- donne$id
	elv <- donne$elv
	dates <- donne$dates
	donne <- donne$data

	#if(nrow(donne$stnDuplCoords) > 0)  #diplay table
	#if(nrow(dat$stnMissCoords) > 0)

	stnlist <- list(id = stn.id, lon = stn.lon, lat = stn.lat, elv = elv, dates = dates, data = donne)

	###get elevation data
	if(GeneralParameters$NewGrd == '1' & GeneralParameters$CreateGrd == "2"){
		jncdf <- which(all.open.file == file.pars[2])
		fdem <- AllOpenFilesData[[jncdf]][[2]]
		dem <- fdem$value
		dem[dem < 0] <- 0
		dem.coord <- data.frame(expand.grid(lon = fdem$x, lat = fdem$y))
		coordinates(dem.coord) = ~lon+lat
		demdf <- data.frame(dem = c(dem))
		demdf <- SpatialPointsDataFrame(coords = dem.coord, data = demdf, proj4string = CRS(as.character(NA)))
		demlist1 <- list(lon = fdem$x, lat = fdem$y, demGrd = demdf)
	}else demlist1 <- NULL
	if(GeneralParameters$blankGrd == "2"){
		jncdf <- which(all.open.file == file.pars[7])
		fdem <- AllOpenFilesData[[jncdf]][[2]]
		dem <- fdem$value
		dem[dem < 0] <- 0
		dem.coord <- data.frame(expand.grid(lon = fdem$x, lat = fdem$y))
		coordinates(dem.coord) = ~lon+lat
		demdf <- data.frame(dem = c(dem))
		demdf <- SpatialPointsDataFrame(coords = dem.coord, data = demdf, proj4string = CRS(as.character(NA)))
		demlist2 <- list(lon = fdem$x, lat = fdem$y, demGrd = demdf)
	}else demlist2 <- NULL

	##RFE sample file
	if(GeneralParameters$NewGrd == '1'){
		jrfe <- which(all.open.file == file.pars[3])
		ncrfe <- AllOpenFilesData[[jrfe]][[2]]
		rfe.coord <- data.frame(expand.grid(lon = ncrfe$x, lat = ncrfe$y))
		coordinates(rfe.coord) = ~lon+lat
		rfelist <- list(lon = ncrfe$x, lat = ncrfe$y, rfeGrd = rfe.coord, rfeVarid = ncrfe$varid, rfeILon = ncrfe$ilon, rfeILat = ncrfe$ilat, irevlat = ncrfe$irevlat)
	}else rfelist <- NULL

	##Shapefile
	if(GeneralParameters$blankGrd == "3"){
		jshp <- which(all.open.file == file.pars[6])
		shpd <- AllOpenFilesData[[jshp]][[2]]
	}else shpd <- NULL

	mrgRaindat <- list(stnData = stnlist, demData1 = demlist1, demData2 = demlist2, rfeData = rfelist, shpData = shpd)
	#outfile <- file.path(origdir, 'DataUsed2ComputeBias.RData', fsep = .Platform$file.sep)
	#save(mrgRaindat, file = outfile)

	return(mrgRaindat)
}

##############################################################################################################
execMergeRain <- function(origdir){
	freqData <- GeneralParameters$period
	mrgRaindat <- TreatMergeRain(origdir, freqData)

	datesSE <- as.numeric(as.character(GeneralParameters$dates.mrg$Values))
	istart <- as.Date(paste(datesSE[1], datesSE[2], datesSE[3], sep = '-'))
	iend <- as.Date(paste(datesSE[4], datesSE[5], datesSE[6], sep = '-'))
	if(freqData == 'dekadal'){
		istart <- paste(format(istart,'%Y%m'), as.numeric(format(istart,'%d')), sep = '')
		iend <- paste(format(iend,'%Y%m'), as.numeric(format(iend,'%d')), sep = '')
	}else{
		istart <- format(istart,'%Y%m%d')
		iend <- format(iend,'%Y%m%d')
	}

	####Create grid
	if(GeneralParameters$NewGrd == '1'){
		create.grd <- as.character(GeneralParameters$CreateGrd)
		##Create grid for interpolation
		if(create.grd == '1'){
			grd.lon <- mrgRaindat$rfeData$lon
			grd.lat <- mrgRaindat$rfeData$lat
			nlon0 <- length(grd.lon)
			nlat0 <- length(grd.lat)
		}else if(create.grd == '2'){
			grd.lon <- mrgRaindat$demData1$lon
			grd.lat <- mrgRaindat$demData1$lat
			nlon0 <- length(grd.lon)
			nlat0 <- length(grd.lat)
		}else if(create.grd == '3'){
			X0 <- as.numeric(as.character(GeneralParameters$new.grid$Values[1]))
			X1 <- as.numeric(as.character(GeneralParameters$new.grid$Values[2]))
			pX <- as.numeric(as.character(GeneralParameters$new.grid$Values[3]))
			Y0 <- as.numeric(as.character(GeneralParameters$new.grid$Values[4]))
			Y1 <- as.numeric(as.character(GeneralParameters$new.grid$Values[5]))
			pY <- as.numeric(as.character(GeneralParameters$new.grid$Values[6]))

			grd.lon <- seq(X0, X1, pX)
			nlon0 <- length(grd.lon)
			grd.lat <- seq(Y0, Y1, pY)
			nlat0 <- length(grd.lat)
		}
	}else{
		adjDir <- as.character(GeneralParameters$file.io$Values[4])
		adjPrefix <- as.character(GeneralParameters$prefix$Values[1])
		#mrgPrefix <- as.character(GeneralParameters$prefix$Values[2])
		fstdate <- ifelse(freqData == 'monthly', substr(istart, 1,6), istart)
		adjFile <- file.path(adjDir, paste(adjPrefix, '_', fstdate,'.nc', sep = ''), fsep = .Platform$file.sep)
		if(!file.exists(adjFile)){
			InsertMessagesTxt(main.txt.out, "Adjusted RFE data not found", format = TRUE)
			return(NULL)
		}
		nc <- nc_open(adjFile)
		grd.lon <- nc$dim[[1]]$vals
		grd.lat <- nc$dim[[2]]$vals
		nc_close(nc)
		nlon0 <- length(grd.lon)
		nlat0 <- length(grd.lat)
	}

	newlocation.merging <- expand.grid(lon = grd.lon, lat = grd.lat)
	coordinates(newlocation.merging)<- ~lon+lat
	newlocation.merging <- SpatialPixels(points = newlocation.merging, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))

	########Blank mask
	usemask <- as.character(GeneralParameters$blankGrd)
	if(usemask == "1") outMask <- NULL
	if(usemask == "2"){
		dem.grd <- krige(formula = dem ~ 1, locations = mrgRaindat$demData2$demGrd, newdata = newlocation.merging, nmax = 4, nmin = 2, debug.level = 0)
		dem <- ifelse(dem.grd@data$var1.pred < 0,0, dem.grd@data$var1.pred)
		outMask <- matrix(dem, nrow = nlon0, ncol = nlat0)
		outMask[outMask == 0] <- NA
	}
	if(usemask == "3"){
		shpd <- mrgRaindat$shpData
		shpd[['vtmp']] <- 1
		shpMask <- over(newlocation.merging, shpd)[,'vtmp']
		outMask <- matrix(shpMask, nrow = nlon0, ncol = nlat0)
	}

	#########
	##Gauge data
	stn.lon <- mrgRaindat$stnData$lon
	stn.lat <- mrgRaindat$stnData$lat

	#Index of new grid over stations
	stn.loc <- data.frame(lon = stn.lon, lat = stn.lat)
	stn.loc <- SpatialPoints(stn.loc)
	ijGrd <- unname(over(stn.loc, geometry(newlocation.merging)))

	#########
	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", grd.lon)
	dy <- ncdim_def("Lat", "degreeN", grd.lat)
	xy.dim <- list(dx, dy)

	VarioModel <- c("Sph", "Exp", "Gau")

	paramsMRG <- list(istart = istart, iend = iend, ijGrd = ijGrd, nlon0 = nlon0, nlat0 = nlat0, xy.dim = xy.dim, outMask = outMask, newlocation.merging = newlocation.merging)

	ret <- MergingFunction(mrgRaindat, VarioModel, paramsMRG, origdir)
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}


##############################################################################################################



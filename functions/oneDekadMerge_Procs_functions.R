
mergeOneDekadRain <- function(){
	dir2save <- GeneralParameters$output$dir
	dyear <- GeneralParameters$Merging.Date$year
	dmon <- GeneralParameters$Merging.Date$month
	ddek <- as.numeric(GeneralParameters$Merging.Date$dekad)
	if(ddek > 3 | ddek < 1){
		InsertMessagesTxt(main.txt.out, 'Dekad must be 1, 2 or 3', format = TRUE)
		tcl("update")
		return(NULL)
	}

	daty <- paste0(format(as.Date(paste(dyear, dmon, ddek, sep = '-')), '%Y%m'), ddek)
	dmon <- substr(daty, 5, 6)
	yeardekad <- expand.grid(1:3, 1:12)
	dir2save <- file.path(dir2save, paste('DEKAD', daty, sep = '_'))
	dir.create(dir2save, showWarnings = FALSE, recursive = TRUE)

	################
	## get rfe data
	if(GeneralParameters$RFE$downloaded){
		rfeData <- getNcdfOpenData(GeneralParameters$RFE$file)[[2]]
		ncInfo <- list(dates = daty, nc.files = rfeData$file, exist = TRUE)
		ncInfo$ncinfo <- list(xo = rfeData$ilon, yo = rfeData$ilat, varid = rfeData$varid)
		ncInfo$xy.rfe <- list(lon = rfeData$x, lat = rfeData$y)

		rfeData <- rfeData[1:3]
		names(rfeData) <- c('lon', 'lat', 'z')
	}else{
		InsertMessagesTxt(main.txt.out, "Download RFE data .....")

		if(!testConnection()){
			InsertMessagesTxt(main.txt.out, 'No Internet connection', format = TRUE)
			tcl("update")
			return(NULL)
		}

		dataRFE <- GeneralParameters$RFE$source
		minlon <- GeneralParameters$RFE.bbox$minlon
		maxlon <- GeneralParameters$RFE.bbox$maxlon
		minlat <- GeneralParameters$RFE.bbox$minlat
		maxlat <- GeneralParameters$RFE.bbox$maxlat
	
		downrfeDir <- file.path(dir2save, 'downloaded_RFE')
		dir.create(downrfeDir, showWarnings = FALSE, recursive = TRUE)

		## change to IRI DL
		if(dataRFE == 'TAMSATv2'){
			url <- 'https://www.tamsat.org.uk/public_data'
			file0 <- paste0('rfe', dyear, '_', dmon, '-dk', ddek, '.nc')
			url <- paste(url, dyear, dmon, file0, sep = '/')
		}
		if(dataRFE == 'TAMSATv3'){
			url <- 'https://www.tamsat.org.uk/public_data/TAMSAT3'
			file0 <- paste0('rfe', dyear, '_', dmon, '-dk', ddek, '.v3.nc')
			url <- paste(url, dyear, dmon, file0, sep = '/')
		}
		if(dataRFE == 'CHIRP'){
			url <- 'ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRP/dekads/africa'
			file0 <- paste0('chirp.', dyear, '.', dmon, '.', ddek, '.tif')
			url <- paste(url, file0, sep = '/')
			## bil smaller,  dekad 1:36, chirp201735.tar.gz
			# url <- "ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRP/bils/dekads/africa"
		}
		destfile0 <- file.path(downrfeDir, paste0('Africa_', file0))
		testURL <- try(suppressWarnings(url(url, open = 'rb')), silent = TRUE)
		if(inherits(testURL, "try-error")){
			InsertMessagesTxt(main.txt.out, paste('Cannot open URL or file does not exist:', file0), format = TRUE)
			tcl("update")
			close(testURL)
			return(NULL)
		}else close(testURL)
		ret <- try(download.file(url, destfile0, mode = "wb", quiet = TRUE), silent = TRUE)
		if(ret != 0){
			InsertMessagesTxt(main.txt.out, paste('Download failed:', file0), format = TRUE)
			tcl("update")
			return(NULL)
		}else{
			InsertMessagesTxt(main.txt.out, paste('Download:', file0, 'done!'))
			tcl("update")
		}

		if(dataRFE%in%c('TAMSATv2', 'TAMSATv3')){
			if(dataRFE == 'TAMSATv2'){
				dim.xo <- 2
				dim.yo <- 1
			}
			if(dataRFE == 'TAMSATv3'){
				dim.xo <- 1
				dim.yo <- 2
			}
			nc <- nc_open(destfile0)
			xm <- nc$dim[[dim.xo]]$vals
			ym <- nc$dim[[dim.yo]]$vals
			rfe <- ncvar_get(nc, varid = nc$var[[1]]$name)
			nc_close(nc)
			xo <- order(xm)
			yo <- order(ym)
			xm <- xm[xo]
			ym <- ym[yo]
			rfe <- rfe[xo, yo]
			idx <- which(xm >= minlon & xm <= maxlon)
			idy <- which(ym >= minlat & ym <= maxlat)
			rfe.lon <- xm[idx]
			rfe.lat <- ym[idy]
			rfe <- rfe[idx, idy]
			longname <- "TAMSAT 10-days rainfall estimate"
		}
		if(dataRFE == 'CHIRP'){
			tif <- readGDAL(destfile0, silent = TRUE)
			rfe <- tif@data[, 1]
			xy <- coordinates(tif)
			ix <- (xy[, 1] >= minlon & xy[, 1] <= maxlon) & (xy[, 2] >= minlat & xy[, 2] <= maxlat)
			rfe <- rfe[ix]
			xy <- xy[ix, ]
			rfe <- reshapeXYZ2Matrix(cbind(xy, rfe))
			rfe.lon <- rfe$x
			rfe.lat <- rfe$y
			rfe <- rfe$z
			longname <- "CHIRP 10-days rainfall estimate"
			file0 <- paste0(getf.no.ext(file0), '.nc')
		}
		rfeData <- list(lon = rfe.lon, lat = rfe.lat, z = rfe)

		rfe[rfe < 0 | is.na(rfe)] <- -99
		dx <- ncdim_def("Lon", "degreeE", rfe.lon)
		dy <- ncdim_def("Lat", "degreeN", rfe.lat)
		rfeout <- ncvar_def('precip', "mm", list(dx, dy), -99, longname = longname,
							prec = "short", shuffle = TRUE, compression = 9)
		outfl <- file.path(downrfeDir, file0)
		nc2 <- nc_create(outfl, rfeout)
		ncvar_put(nc2, rfeout, rfe)
		nc_close(nc2)

		#########
		ncInfo <- list(dates = daty, nc.files = outfl, exist = TRUE)
		ncInfo$ncinfo <- list(xo = 1, yo = 2, varid = 'precip')
		ncInfo$xy.rfe <- list(lon = rfe.lon, lat = rfe.lat)
	}

	################
	## correct bias
	if(GeneralParameters$BIAS$Adjust){
		bias.method <- GeneralParameters$BIAS$method
		biasDir <- GeneralParameters$BIAS$Dir

		if(bias.method == "Quantile.Mapping"){
			parsstnf <- paste0('Bernoulli-Gamma_Pars.STN', '_', as.numeric(dmon), '.nc')
			pars.stnFile <- file.path(biasDir, parsstnf)
			if(!file.exists(pars.stnFile)){
				InsertMessagesTxt(main.txt.out, paste(parsstnf, "doesn't exist"), format = TRUE)
				tcl("update")
				return(NULL)
			}
			parsrfef <- paste0('Bernoulli-Gamma_Pars.RFE', '_', as.numeric(dmon), '.nc')
			pars.rfeFile <- file.path(biasDir, parsrfef)
			if(!file.exists(pars.rfeFile)){
				InsertMessagesTxt(main.txt.out, paste(parsrfef, "doesn't exist"), format = TRUE)
				tcl("update")
				return(NULL)
			}

			nc <- nc_open(pars.stnFile)
			lon.bias <- nc$dim[[1]]$vals
			lat.bias <- nc$dim[[2]]$vals
			prob.stn <- ncvar_get(nc, varid = "prob")
			scale.stn <- ncvar_get(nc, varid = "scale")
			shape.stn <- ncvar_get(nc, varid = "shape")
			nc_close(nc)
			pars.stn <- list(prob = prob.stn, scale = scale.stn, shape = shape.stn)

			nc <- nc_open(pars.rfeFile)
			prob.rfe <- ncvar_get(nc, varid = "prob")
			scale.rfe <- ncvar_get(nc, varid = "scale")
			shape.rfe <- ncvar_get(nc, varid = "shape")
			nc_close(nc)
			pars.rfe <- list(prob = prob.rfe, scale = scale.rfe, shape = shape.rfe)
		}else{
			idek <- if(bias.method == "Multiplicative.Bias.Mon") as.numeric(dmon) else which(yeardekad[, 2] == as.numeric(dmon) & yeardekad[, 1] == ddek)
			biasf <- paste0("STN_GRID_MeanBias_", idek, '.nc')
			biasFile <- file.path(biasDir, biasf)
			if(!file.exists(biasFile)){
				InsertMessagesTxt(main.txt.out, paste(biasf, "doesn't exist"), format = TRUE)
				tcl("update")
				return(NULL)
			}
			nc <- nc_open(biasFile)
			lon.bias <- nc$dim[[1]]$vals
			lat.bias <- nc$dim[[2]]$vals
			data.bias <- ncvar_get(nc, varid = "bias")
			nc_close(nc)
		}

		biasCoords <- list(lon = lon.bias, lat = lat.bias)
		biasSp <- defSpatialPixels(biasCoords)
		rfeSp <- defSpatialPixels(rfeData)
		is.regridRFE <- is.diffSpatialPixelsObj(biasSp, rfeSp, tol = 1e-07)
		if(is.regridRFE){
			rfeData <- cdt.interp.surface.grid(rfeData, biasCoords)
		}

		if(bias.method == "Quantile.Mapping"){
			xadj <- quantile.mapping.BGamma(rfeData$z, pars.stn, pars.rfe, TRUE)
		}else xadj <- rfeData$z * data.bias
		xadj[xadj < 0] <- 0

		xadj[is.na(xadj)] <- -99
		dx <- ncdim_def("Lon", "degreeE", lon.bias)
		dy <- ncdim_def("Lat", "degreeN", lat.bias)

		grd.bsadj <- ncvar_def("precip", "mm", list(dx, dy), -99, longname = " Mean Bias Adjusted RFE",
								prec = "short", shuffle = TRUE, compression = 9)
		bias.outfl <- file.path(dir2save, paste0('rr_adj', '_', daty,'.nc'))
		nc2 <- nc_create(bias.outfl, grd.bsadj)
		ncvar_put(nc2, grd.bsadj, xadj)
		nc_close(nc2)

		#########
		ncInfo <- list(dates = daty, nc.files = bias.outfl, exist = TRUE)
		ncInfo$ncinfo <- list(xo = 1, yo = 2, varid = 'precip')
		ncInfo$xy.rfe <- list(lon = lon.bias, lat = lat.bias)
	}

	################
	## merging
	if(!GeneralParameters$STN$No.Stn.Data){
		stnData <- getStnOpenData(GeneralParameters$STN$file)
		stnData <- getCDTdataAndDisplayMsg(stnData, 'dekadal')
		if(is.null(stnData)) return(NULL)

		################

		xy.grid <- list(lon = ncInfo$xy.rfe$lon, lat = ncInfo$xy.rfe$lat)
		nlon0 <- length(ncInfo$xy.rfe$lon)
		nlat0 <- length(ncInfo$xy.rfe$lat)

		##################
		## DEM data
		demData <- NULL
		if(GeneralParameters$blank$blank == "2"){
			demData <- getDemOpenDataSPDF(GeneralParameters$blank$DEM.file)
			if(is.null(demData)){
				InsertMessagesTxt(main.txt.out, "No elevation data found", format = TRUE)
				return(NULL)
			}
		}

		##################
		## regrid DEM data
		if(!is.null(demData)){
			is.regridDEM <- is.diffSpatialPixelsObj(defSpatialPixels(xy.grid), defSpatialPixels(demData[c('lon', 'lat')]), tol = 1e-07)
			if(is.regridDEM)
				demData <- cdt.interp.surface.grid(c(demData[c('lon', 'lat')], list(z = demData$demMat)), xy.grid)
			else demData <- list(x = demData$lon, y = demData$lat, z = demData$demMat)
			demData$z[demData$z < 0] <- 0
		}

		##################
		## blanking
		outMask <- switch(GeneralParameters$blank$blank,
						"2" = {
								mask <- demData$z
								mask[mask <= 0] <- NA
								mask[!is.na(mask)] <- 1
								mask
							},
						"3" = {
								shpd <- getShpOpenData(GeneralParameters$blank$SHP.file)[[2]]
								shpd[['vtmp']] <- 1
								mask <- over(defSpatialPixels(xy.grid), shpd)[, 'vtmp']
								dim(mask) <- c(nlon0, nlat0)
								mask
							}, NULL)

		################

		mrgParms <- list(GeneralParameters = GeneralParameters, months = as.numeric(dmon), ncInfo = ncInfo,
						stnData = stnData, demData = demData, merge.DIR = dir2save,
						interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0), outMask = outMask)

		ret <- Precip_MergingFunctions(mrgParms)

		if(!is.null(ret)){
			if(ret != 0) return(ret) 
		}else return(NULL)
	}

	return(0)
}


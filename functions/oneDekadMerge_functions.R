
mergeOneDekadRain <- function(){

	noStn <- GeneralParameters$No.Stn.Data
	downRFE <- GeneralParameters$Downloaded.RFE
	useAdj <- GeneralParameters$Adjust.Bias
	usemask <- GeneralParameters$Blank.Grid

	VarioModel <- GeneralParameters$model
	min.Stn <- as.numeric(GeneralParameters$Merging.pars$min.Stn)
	min.Stn.non0 <- as.numeric(GeneralParameters$Merging.pars$min.Stn.non0)
	max.RnR.dist <- as.numeric(GeneralParameters$Merging.pars$max.RnR.dist)
	maxdist <- as.numeric(GeneralParameters$Interpolation.pars$maxdist)
	nmin <- as.numeric(GeneralParameters$Interpolation.pars$nmin)
	nmax <- as.numeric(GeneralParameters$Interpolation.pars$nmax)
	interpMethod <- GeneralParameters$Interpolation.pars$Interp.Method
	RainNoRain <- GeneralParameters$Merging.pars$Rain.no.Rain

	savedir <- GeneralParameters$IO.files$dir2save
	rfeDir <- GeneralParameters$IO.files$RFE.dir
	biasDir <- GeneralParameters$IO.files$BIAS.dir
	rfeFileFormat <- GeneralParameters$Prefix$RFE.File.Format
	meanBiasPrefix <- GeneralParameters$Prefix$Mean.Bias.Prefix

	###################################################
	## check date
	daty <- try(as.Date(paste(as.numeric(GeneralParameters$Merging.Date$year),
				as.numeric(GeneralParameters$Merging.Date$month),
				as.numeric(GeneralParameters$Merging.Date$dekad), sep = '-')), silent = TRUE)
	if(inherits(daty, "try-error") | !inherits(daty, "Date")){
		InsertMessagesTxt(main.txt.out, "Wrong date", format = TRUE)
		return(NULL)
	}
	daty <- paste(format(daty, '%Y%m'), as.numeric(format(daty, '%d')), sep = '')
	yrs <- substr(daty, 1, 4)
	mon <- substr(daty, 5, 6)
	dek <- substr(daty, 7, 7)
	if(as.numeric(dek) > 3){
		InsertMessagesTxt(main.txt.out, 'Dekad must be between 1 and 3', format = TRUE)
		return(NULL)
	}
	yeardekad <- expand.grid(1:3, 1:12)

	###################################################
	## RFE data
	if(downRFE == '1'){
		rfeFile <- file.path(rfeDir, sprintf(rfeFileFormat, yrs, mon, dek))
		if(!file.exists(rfeFile)){
			InsertMessagesTxt(main.txt.out, "RFE data not found", format = TRUE)
			return(NULL)
		}

		nc <- nc_open(rfeFile)
		rfe.lon <- nc$dim[[1]]$vals
		rfe.lat <- nc$dim[[2]]$vals
		rfe <- ncvar_get(nc, varid = nc$var[[1]]$name)
		xo <- order(rfe.lon)
		yo <- order(rfe.lat)
		rfe.lon <- rfe.lon[xo]
		rfe.lat <- rfe.lat[yo]
		rfe <- rfe[xo, yo]
		nc_close(nc)
	}else{
		if(!testConnection()){
			InsertMessagesTxt(main.txt.out, 'No internet connection', format = TRUE)
			return(NULL)
		}
		dataRFE <- GeneralParameters$RFE.data
		minlon <- as.numeric(GeneralParameters$RFE.bbox$minlon)
		maxlon <- as.numeric(GeneralParameters$RFE.bbox$maxlon)
		minlat <- as.numeric(GeneralParameters$RFE.bbox$minlat)
		maxlat <- as.numeric(GeneralParameters$RFE.bbox$maxlat)

		downrfeDir <- file.path(savedir, 'downloaded_RFE')
		dir.create(downrfeDir, showWarnings = FALSE, recursive = TRUE)
		
		if(dataRFE == 'TAMSAT'){
			url <- 'http://tamsat.org.uk/public_data'
			file0 <- paste('rfe', yrs, '_', mon,'-dk', dek,'.nc', sep = '')
			url <- paste(url, yrs, mon, file0, sep = '/')
		}
		if(dataRFE == 'CHIRP'){
			url <- 'ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRP/dekads/africa'
			file0 <- paste('chirp.', yrs, '.', mon,'.', dek,'.tif', sep = '')
			url <- paste(url, file0, sep = '/')
		}

		destfile0 <- file.path(downrfeDir, paste('Africa_', file0, sep = ''))
		# url.exists(url) #RCurl
		# testURL <- try(suppressWarnings(readLines(url, n = 1)), silent = TRUE)
		testURL <- try(suppressWarnings(url(url, open='rb')), silent = TRUE)
		if(inherits(testURL, "try-error")){
			InsertMessagesTxt(main.txt.out, paste('Cannot open URL or file does not exist:',file0),
			format = TRUE)
			close(testURL)
			return(NULL)
		}else close(testURL)
		ret <- try(download.file(url, destfile0, mode = "wb", quiet = TRUE), silent = TRUE)
		if(ret  != 0){
			InsertMessagesTxt(main.txt.out, paste('Download failed:',file0), format = TRUE)
			return(NULL)
		}else{
			InsertMessagesTxt(main.txt.out, paste('Download:',file0, 'done!'))
		}
		if(dataRFE == 'TAMSAT'){
			nc <- nc_open(destfile0)
			xm <- nc$dim[[2]]$vals
			ym <- nc$dim[[1]]$vals
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
			rfe[is.na(rfe)] <- -99
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
			rfe[rfe < 0 | is.na(rfe)] <- -99
			longname <- "CHIRP 10-days rainfall estimate"
			file0 <- paste(getf.no.ext(file0), '.nc', sep = '')
		}
		dx <- ncdim_def("Lon", "degreeE", rfe.lon)
		dy <- ncdim_def("Lat", "degreeN", rfe.lat)
		rfeout <- ncvar_def('precip', "mm", list(dx, dy), -99, longname = longname, prec = "short")
		outfl <- file.path(downrfeDir, file0)
		nc2 <- nc_create(outfl, rfeout)
		ncvar_put(nc2, rfeout, rfe)
		nc_close(nc2)
	}

	###################################################
	## Bias data
	if(useAdj == '1'){
		idek <- which(yeardekad[, 2] == as.numeric(mon) & yeardekad[, 1] == as.numeric(dek))
		biasFile <- file.path(biasDir, paste(meanBiasPrefix, '_', idek, '.nc', sep = ''))
		if(!file.exists(biasFile)){
			InsertMessagesTxt(main.txt.out, "Mean bias coefficients not found", format = TRUE)
			return(NULL)
		}

		nc <- nc_open(biasFile)
		bias.lon <- nc$dim[[1]]$vals
		bias.lat <- nc$dim[[2]]$vals
		bias <- ncvar_get(nc, varid = nc$var[[1]]$name)
		nc_close(nc)
	}

	###################################################
	## define grid,  from RFE or Bias
	if(useAdj == '1'){
		grd.lon <- bias.lon 
		grd.lat <- bias.lat
	}else{
		grd.lon <- rfe.lon
		grd.lat <- rfe.lat
	}
	nlon0 <- length(grd.lon)
	nlat0 <- length(grd.lat)
	newlocation.merging <- defSpatialPixels(list(lon = grd.lon, lat = grd.lat))

	###################################################
	dx <- ncdim_def("Lon", "degreeE", grd.lon)
	dy <- ncdim_def("Lat", "degreeN", grd.lat)
	xy.dim <- list(dx, dy)

	###################################################
	## BIAS ADJUSTMENT
	if(useAdj == '1'){
		rfeObj <- list(x = rfe.lon, y = rfe.lat, z = rfe)
		grdnew <- list(x = grd.lon, y = grd.lat)
		newObj <- interp.surface.grid(rfeObj, grdnew)
		rfe <- round(newObj$z * bias, 2)

		rfe.adj <- rfe
		rfe.adj[is.na(rfe.adj)] <- -99
		grd.bsadj <- ncvar_def("precip", "mm", xy.dim, -99, longname= " Mean Bias Adjusted RFE", prec = "short")
		bias.outfl <- file.path(savedir, paste('rr_adj', '_', daty,'.nc', sep = ''), fsep = .Platform$file.sep)
		nc2 <- nc_create(bias.outfl, grd.bsadj)
		ncvar_put(nc2, grd.bsadj, rfe.adj)
		nc_close(nc2)
	}

	###################################################
	## merging with station data
	if(noStn == '0'){
		## Blank mask
		### get elevation data
		if(usemask == "2") demData <- getDemOpenDataSPDF(GeneralParameters$IO.files$DEM.file)
		else demData <- NULL

		## Shapefile
		if(usemask == "3") shpd <- getShpOpenData(GeneralParameters$IO.files$SHP.file)[[2]]
		else shpd <- NULL

		if(usemask == "1") outMask <- NULL
		if(usemask == "2"){
			grid.loc <- defSpatialPixels(list(lon = grd.lon, lat = grd.lat))
			demGrid <- defSpatialPixels(list(lon = demData$lon, lat = demData$lat))
			is.regridDEM <- is.diffSpatialPixelsObj(grid.loc, demGrid, tol = 1e-07)
			if(is.regridDEM){
				dem <- regridDEMFun(demData, list(lon = grd.lon, lat = grd.lat), regrid = 'BLW')
			}else{
				dem <- demData$demGrd@data[, 1]
			}
			outMask <- matrix(dem, nrow = nlon0, ncol = nlat0)
			outMask[outMask == 0] <- NA
			rm(dem.grd, dem)
		}
		if(usemask == "3"){
			shpd[['vtmp']] <- 1
			shpMask <- over(newlocation.merging, shpd)[, 'vtmp']
			outMask <- matrix(shpMask, nrow = nlon0, ncol = nlat0)
		}

		###################################################
		## block grid
		if(as.logical(GeneralParameters$use.block)){
			sDX <- newlocation.merging@grid@cellsize[1]/2
			dBX <- seq(-sDX, sDX, length.out = 4)
			sDY <- newlocation.merging@grid@cellsize[2]/2
			dBY <- seq(-sDY, sDY, length.out = 4)
			bGrd <- expand.grid(x = dBX, y = dBY)
		}

		###################################################
		### get data
		donne <- getStnOpenData(GeneralParameters$IO.files$STN.file)
		donne <- getCDTdataAndDisplayMsg(donne, 'dekadal')
		if(is.null(donne)) return(NULL)
		ix <- which(donne$dates%in%daty)
		if(length(ix) != 1){
			InsertMessagesTxt(main.txt.out, "The input date does not match the date in the station data file", format = TRUE)
			return(NULL)
		}
		donne$data <- as.numeric(donne$data[ix,])
		donne$dates <- donne$dates[ix]

		stn.lon <- donne$lon
		stn.lat <- donne$lat
		stn.data <- donne$data

		## RFE over stn location
		ijGrd <- grid2pointINDEX(list(lon = stn.lon, lat = stn.lat), list(lon = grd.lon, lat = grd.lat))
		rfe_gg <- rfe[ijGrd]
		dff <- stn.data - rfe_gg

		# Remove extremes differences between gauge and satellite
		q1 <- quantile(dff, 0.0001, na.rm = T)
		q2 <- quantile(dff, 0.9999, na.rm = T)
		dff[dff < q1] <- NA
		dff[dff > q2] <- NA
		ix <- which(!is.na(dff))

		##Initial rfe
		rfe.vec <- c(rfe)
		out.mrg <- rfe.vec  

		if(sum(stn.data, na.rm = TRUE) > 0 & length(ix) >= min.Stn){
			rr.stn <- data.frame(lon = stn.lon, lat = jitter(stn.lat), gg = stn.data, rfe = rfe_gg, dff = dff)
			rr.stn <- rr.stn[ix,]
			coordinates(rr.stn) <- ~lon+lat

			ijx1 <- which(rr.stn$gg > 0)
			if(length(ijx1) >= min.Stn.non0){
				grd.newloc <- SpatialPointsDataFrame(coords = newlocation.merging, data = data.frame(rfe = rfe.vec))
				rr.glm <- glm(gg~rfe, rr.stn, family = gaussian)
				rr.stn$res <- residuals(rr.glm)
				pred.rr <- predict(rr.glm, newdata = grd.newloc, se.fit = T)

				if(interpMethod == "IDW"){
					grd.rr <- krige(res~1, locations = rr.stn, newdata = newlocation.merging, block = bGrd,
										nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
					res.pred <- grd.rr$var1.pred
				}else if(interpMethod == "Kriging"){
					grd.rr <- try(autoKrige(res~1, input_data = rr.stn, new_data = newlocation.merging, block = bGrd,
									model = VarioModel, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0), silent = TRUE)
					if(!inherits(grd.rr, "try-error")){
						res.pred <- grd.rr$krige_output$var1.pred
					}else{
						grd.rr <- krige(res~1, locations = rr.stn, newdata = newlocation.merging, block = bGrd,
											nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
						res.pred <- grd.rr$var1.pred
					}
				}
				out.mrg <- as.numeric(res.pred+pred.rr$fit)
				out.mrg <- ifelse(out.mrg < 0,0, out.mrg)
			}else{
				grd.rr <- idw(dff~1, locations = rr.stn, newdata = newlocation.merging, block = bGrd,
								nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				out.mrg <- grd.rr$var1.pred + rfe.vec
				out.mrg <- ifelse(out.mrg < 0,0, out.mrg)
			}
			# Take RFE for areas where interpolation/merging was not possible
			ix <- which(is.na(out.mrg))
			out.mrg[ix] <- rfe.vec[ix]

			cells <- as(newlocation.merging, 'SpatialPixels')@grid
			##smoothing???
			# img.mrg <- as.image(out.mrg, x = coordinates(newlocation.merging), nx = cells@cells.dim[1], ny = cells@cells.dim[2])
			# smooth.mrg <- image.smooth(img.mrg, theta = 0.075)
			# out.mrg <- round(c(smooth.mrg$z), 1)

			#Rain-non-Rain Mask
			if(RainNoRain != 'None') {
				rr.stn$rnr <- ifelse(rr.stn$gg >= 1,1,0)
				if (RainNoRain == 'Gauge') {#Gauge only
					rnr.grd <- krige(rnr~1, locations = rr.stn, newdata = newlocation.merging, block = bGrd,
									nmin = nmin, nmax = nmax, maxdist = max.RnR.dist, debug.level = 0)
					rnr.pred <- ifelse(is.na(rnr.grd$var1.pred), 1, rnr.grd$var1.pred)
					RnoR <- round(rnr.pred)
					RnoR[is.na(RnoR)] <- 1
				} else if(RainNoRain == 'Satellite') {#Satellite only
					RnoR <- ifelse(rfe.vec >= 1, 1, 0)
					RnoR[is.na(RnoR)] <- 1
				} else if(RainNoRain == 'GaugeSatellite') {
					rfe.rnr <- ifelse(rfe.vec >= 1, 1, 0)
					rnr.grd <- krige(rnr~1, locations = rr.stn, newdata = newlocation.merging, block = bGrd,
									nmin = nmin, nmax = nmax, maxdist = max.RnR.dist, debug.level = 0)
					RnoR <- rnr.grd$var1.pred
					RnoR <- round(RnoR)
					ix <- which(is.na(RnoR))
					RnoR[ix] <- rfe.rnr[ix]
					RnoR[is.na(RnoR)] <- 1
					##smoothing???
					img.RnoR <- as.image(RnoR, x = coordinates(newlocation.merging), nx = cells@cells.dim[1], ny = cells@cells.dim[2])
					smooth.RnoR <- image.smooth(img.RnoR, theta = 0.075)
					RnoR <- round(c(smooth.RnoR$z))
				}
				out.mrg <- out.mrg * RnoR
			}
		}

		dim(out.mrg) <- c(nlon0, nlat0)
		out.mrg[is.na(out.mrg)] <- -99

		#Apply mask for area of interest
		if(!is.null(outMask)) out.mrg[is.na(outMask)] <- -99

		grd.out <- ncvar_def("precip", "mm", xy.dim,-99, longname=" Merged Station-Satellite Rainfall", prec = "short")
		mrg.outfl <- file.path(savedir, paste('rr_mrg', '_', daty, '_MON.nc', sep = ''), fsep = .Platform$file.sep)
		nc2 <- nc_create(mrg.outfl, grd.out)
		ncvar_put(nc2, grd.out, out.mrg)
		nc_close(nc2)
	}

	return(0)
}


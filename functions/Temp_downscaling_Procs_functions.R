

Temp_CoefDownscaling <- function(paramsGlmCoef){
	InsertMessagesTxt(main.txt.out, 'Compute downscaling coefficients ...')

	GeneralParameters <- paramsGlmCoef$GeneralParameters

	stnData <- paramsGlmCoef$stnData
	lon.stn <- stnData$lon
	lat.stn <- stnData$lat
	date.stn <- stnData$dates
	data.stn <- stnData$data

	demData <- paramsGlmCoef$demData
	lon.dem <- demData$lon
	lat.dem <- demData$lat
	grd.dem <- demData$demMat
	grd.dem[grd.dem < 0] <- 0

	ijdem <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = lon.dem, lat = lat.dem))
	dem.stn <- grd.dem[ijdem]

	year1 <- GeneralParameters$Down.Date.Range$start.year
	year2 <- GeneralParameters$Down.Date.Range$end.year
	years <- as.numeric(substr(date.stn, 1, 4))
	iyrCoef <- years >= year1 & years <= year2

	data.stn <- data.stn[iyrCoef, , drop = FALSE]
	date.stn <- date.stn[iyrCoef]
	months <- as.numeric(substr(date.stn, 5, 6))

	coef <- matrix(NA, 12, 2)
	for(m in 1:12){
		ix <- which(months == m)
		glm.dat <- cbind(rep(dem.stn, length(ix)), c(t(data.stn[ix, , drop = FALSE])))
		glm.dat <- glm.dat[!is.na(glm.dat[, 1]) & !is.na(glm.dat[, 2]), , drop = FALSE]
		if(nrow(glm.dat) == 0) next #skip if all data NA
		## standardize
		moy <- base::colMeans(glm.dat)
		ect <- matrixStats::colSds(glm.dat)
		if(ect[1] == 0 | ect[2] == 0) next  #skip if variance null
		glm.dat <- t((t(glm.dat)-moy)/ect)
		glm.dat[glm.dat[, 2] < -3, 2] <- -3
		glm.tt <- glm(glm.dat[, 2] ~ glm.dat[, 1])

		coef[m, 1] <- glm.tt$coefficients[1]
		coef[m, 2] <- glm.tt$coefficients[2]
	}

	##Take the next or previous month non-NA, loop until there are no more NA
	ina <- which(is.na(coef[, 1]))
	if(length(ina) == 12){
		InsertMessagesTxt(main.txt.out, "All coefficients are NA", format = TRUE)
		return(NULL)
	}
	if(length(ina) > 0){
		tmp <- coef
		while(length(ina) > 0){
			coef2 <- rbind(tmp, tmp)
			shiftUp <- rbind(coef2[-1, ], coef2[1, ])
			tmp[ina, ] <- shiftUp[ina, ]
			tmp[-ina, ] <- coef[-ina, ]
			ina <- which(is.na(tmp[, 1]))
			if(length(ina) > 0){
				tmp1 <- tmp
				shiftDown <- rbind(coef2[24, ], coef2[-24, ])
				tmp1[ina, ] <- shiftDown[ina, ]
				tmp1[-ina, ] <- tmp[-ina, ]
				tmp <- tmp1
				ina <- which(is.na(tmp[, 1]))
			}
		}
		coef <- tmp
	}

	outfile <- file.path(paramsGlmCoef$origdir, 'STN_DEM_GLM_COEF.txt')
	write.table(coef, file = outfile, col.names = FALSE, row.names = FALSE)

	InsertMessagesTxt(main.txt.out, 'Computing downscaling coefficients finished')
	rm(stnData, demData, grd.dem, data.stn)
	gc()
	return(0)
}

#################################################################################################

Temp_ReanalysisDownscaling <- function(paramsDownscl){
	GeneralParameters <- paramsDownscl$GeneralParameters
	interp.method <- GeneralParameters$Interpolation$interp.method
	nmin <- GeneralParameters$Interpolation$nmin
	nmax <- GeneralParameters$Interpolation$nmax
	maxdist <- GeneralParameters$Interpolation$maxdist
	vgm.model <- str_trim(GeneralParameters$Interpolation$vgm.model[[1]])
	use.block <- GeneralParameters$Interpolation$use.block

	freqData <- GeneralParameters$period
	Down.File.Format <- GeneralParameters$output$format
	origdir <- paramsDownscl$origdir

	###############

	nc <- nc_open(paramsDownscl$reanalData$nc.files[which(paramsDownscl$reanalData$exist)[1]])
	lon.reanl <- nc$dim[[paramsDownscl$reanalData$ncinfo$xo]]$vals
	lat.reanl <- nc$dim[[paramsDownscl$reanalData$ncinfo$yo]]$vals
	nc_close(nc)
	xo <- order(lon.reanl)
	lon.reanl <- lon.reanl[xo]
	yo <- order(lat.reanl)
	lat.reanl <- lat.reanl[yo]
	nlon.r <- length(lon.reanl)
	nlat.r <- length(lat.reanl)

	###############
	xy.grid <- paramsDownscl$xy.grid
	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- length(xy.grid$lon)
	nlat0 <- length(xy.grid$lat)

	###############
	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	out.tt <- ncvar_def("temp", "DegC", list(dx, dy), -99, longname = "Dwonscaled temperature from reanalysis data", prec = "float", compression = 9)

	###############
	## DEM at reanalysis grid
	coords.reanl <- expand.grid(lon = lon.reanl, lat = lat.reanl)
	ijreanl <- grid2pointINDEX(list(lon = coords.reanl$lon, lat = coords.reanl$lat), xy.grid)

	##############
	## create grid
	demGrid <- paramsDownscl$demGrid
	# demres <- grdSp@grid@cellsize
	# slpasp <- slope.aspect(demGrid, demres[1], demres[2], filter = "sobel")
	# slpasp <- raster.slope.aspect(demGrid)
	slope <- matrix(0, nlon0, nlat0)
	aspect <- matrix(0, nlon0, nlat0)

	ObjGrd <- list(x = xy.grid$lon, y = xy.grid$lat, z = demGrid, slp = slope, asp = aspect)
	ObjStn <- list(x = coords.reanl$lon, y = coords.reanl$lat, z = demGrid[ijreanl], slp = slope[ijreanl], asp = aspect[ijreanl])
	interp.grid <- createGrid(ObjStn, ObjGrd, as.dim.elv = FALSE, coarse.grid = FALSE)

	bGrd <- NULL
	if(use.block){
		cells <- SpatialPixels(points = interp.grid$newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)))@grid
		bGrd <- createBlock(cells@cellsize, 2, 5)
	}

	###############
	## Coeff standardized dem and stn
	downCoef <- paramsDownscl$downCoef

	###############
	## standardize dem
	dem.mean <- mean(demGrid, na.rm = TRUE)
	dem.sd <- sd(demGrid, na.rm = TRUE)
	demStand <- (demGrid - dem.mean)/dem.sd
	dem.reanl <- demStand[ijreanl]
	dim(dem.reanl) <- c(nlon.r, nlat.r)

	###############
	InsertMessagesTxt(main.txt.out, "Downscale  Reanalysis ...")

	is.parallel <- doparallel(length(which(paramsDownscl$reanalData$exist)) >= 30)
	`%parLoop%` <- is.parallel$dofun

	packages <- c('fields', 'gstat', 'automap', 'ncdf4')
	ret <- foreach(jj = seq_along(paramsDownscl$reanalData$nc.files), .packages = packages) %parLoop% {
		if(paramsDownscl$reanalData$exist[jj]){
			nc <- nc_open(paramsDownscl$reanalData$nc.files[jj])
			tt.reanl <- ncvar_get(nc, varid = paramsDownscl$reanalData$ncinfo$varid)
			nc_close(nc)
			if(paramsDownscl$reanalData$ncinfo$yo == 1){
				tt.reanl <- matrix(c(tt.reanl), nrow = nlon.r, ncol = nlat.r, byrow = TRUE)
			}
			tt.reanl <- tt.reanl[xo, yo]
		}else return(NULL)
		date.reanl <- paramsDownscl$reanalData$dates[[jj]]

		############
		## standardize reanal
		tt.mean <- mean(tt.reanl, na.rm = TRUE)
		tt.sd <- sd(tt.reanl, na.rm = TRUE)
		tt.std <- (tt.reanl-tt.mean)/tt.sd

		mon <- as.numeric(substr(date.reanl, 5, 6))
		resid <- tt.std - (downCoef[mon, 2] * dem.reanl + downCoef[mon, 1])

		############
		if(interp.method == 'FBL'){
			resid[is.na(resid)] <- 0
			residObj <- list(x = lon.reanl, y = lat.reanl, z = resid)
			residInterp <- interp.surface.grid(residObj, list(x = xy.grid$lon, y = xy.grid$lat))
			downTT <- residInterp$z
		}else{
			locations.reanl <- interp.grid$coords.stn
			locations.reanl$res <- c(resid)
			locations.reanl <- locations.reanl[!is.na(locations.reanl$res), ]
			if(length(locations.reanl$res) < 2) return(NULL)

			if(interp.method == 'Kriging'){
				vgm <- try(autofitVariogram(res~1, input_data = locations.reanl, model = vgm.model, cressie = TRUE), silent = TRUE)
				vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
			}else vgm <- NULL

			grd.temp <- krige(res~1, locations = locations.reanl, newdata = interp.grid$newgrid, model = vgm,
								block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
			downTT <- matrix(grd.temp$var1.pred, ncol = nlat0, nrow = nlon0)
		}

 		############
		downTT[is.na(downTT)] <- 0
		downTT <- (downCoef[mon, 2] * demStand + downCoef[mon, 1]) + downTT
		downTT <- downTT * tt.sd + tt.mean
		downTT[is.na(downTT)] <- -99

		############
		year <- substr(date.reanl, 1, 4)
		month <- substr(date.reanl, 5, 6)
		if(freqData == 'daily') outncfrmt <- sprintf(Down.File.Format, year, month, substr(date.reanl, 7, 8))
		else if(freqData == 'dekadal') outncfrmt <- sprintf(Down.File.Format, year, month, substr(date.reanl, 7, 7))
		else outncfrmt <- sprintf(Down.File.Format, year, month)
		outfl <- file.path(origdir, outncfrmt)

		nc2 <- nc_create(outfl, out.tt)
		ncvar_put(nc2, out.tt, downTT)
		nc_close(nc2)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	InsertMessagesTxt(main.txt.out, 'Downscaling  Reanalysis finished')
	rm(demGrid, demStand, dem.reanl, interp.grid, ObjGrd, ObjStn)
	gc()
	return(0)
}

#################################################################################################

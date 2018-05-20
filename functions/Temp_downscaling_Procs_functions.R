

Coeff.downscaling.LapseRate <- function(stnData, useClimato = TRUE, aspect.slope = FALSE,
										polynomial = FALSE, order = 1, standardize = FALSE)
{
	if(aspect.slope) polynomial <- FALSE
	# Constant lapse rate
	months <- as.numeric(substr(stnData$dates, 5, 6))

	lm.res <- lapply(1:12, function(m){
		ix <- which(months == m)
		data.lm <- stnData$data[ix, , drop = FALSE]
		if(useClimato){
			## climatologies
			if(polynomial){
				lon.lm <- stnData$lon
				lat.lm <- stnData$lat
			}
			if(aspect.slope){
				asp.lm <- stnData$asp
				slp.lm <- stnData$slp
			}
			dem.lm <- stnData$dem
			data.lm <- colMeans(data.lm, na.rm = TRUE)
			data.lm[is.nan(data.lm)] <- NA
		}else{
			## whole data
			len.dat <- length(ix)
			if(polynomial){
				lon.lm <- rep(stnData$lon, len.dat)
				lat.lm <- rep(stnData$lat, len.dat)
			}
			if(aspect.slope){
				asp.lm <- rep(stnData$asp, len.dat)
				slp.lm <- rep(stnData$slp, len.dat)
			}
			dem.lm <- rep(stnData$dem, len.dat)
			data.lm <- c(t(data.lm))
		}
		data.lm <- data.frame(z = dem.lm, v = data.lm)
		if(aspect.slope) data.lm <- data.frame(s = slp.lm, a = asp.lm, data.lm)
		if(polynomial) data.lm <- data.frame(x = lon.lm, y = lat.lm, data.lm)

		ina <- !is.na(data.lm$z) & !is.na(data.lm$v)
		## skip if all data NA
		if(!any(ina)) return(NULL)

		data.lm <- data.lm[ina, , drop = FALSE]
		## skip if variance null or too small
		sds <- sd(data.lm$v)
		if(!is.na(sds) & sds < 1e-9) return(NULL)

		#####
		moy <- NULL
		sds <- NULL
		if(standardize){
			data.lm0 <- as.matrix(data.lm)
			moy <- colMeans(data.lm0, na.rm = TRUE)
			sds <- matrixStats::colSds(data.lm0, na.rm = TRUE)
			names(sds) <- names(moy)
			data.lm <- sweep(sweep(data.lm, 2, moy, FUN = "-"), 2, sds, FUN = "/")
			data.lm$v[data.lm$v < -3] <- -3
			rm(data.lm0)
		}

		#####
		if(polynomial)
			formule <- if(order == 1) v ~ x + y + z else v ~ x + y + I(x*x) + I(y*y) + I(x*y) + z
		else
			formule <- if(aspect.slope) v ~ s + a + z else v ~ z

		mod.lm <- glm(as.formula(formule), data = data.lm)
		remove.data <- c("residuals", "fitted.values", "effects", "linear.predictors", "weights", "prior.weights", "y", "model", "data")
		mod.lm[remove.data] <- NULL
		mod.lm[["qr"]][["qr"]] <- NULL
		mod.lm$std.pars <- list(mean = moy, sd = sds)
		return(mod.lm)
	})

	inull <- sapply(lm.res, is.null)
	if(all(inull)) return(NULL)

	if(polynomial){
		nomc <- if(order == 1) c("Intercept", "lon", "lat", "elv")
				else c("Intercept", "lon", "lat", "lon2", "lat2", "lonlat", "elv")
		fitting <- "polynomial"
	}else{
		nomc <- if(aspect.slope) c("Intercept", "slope", "aspect", "elv")
				else c("Intercept", "elv")
		fitting <- if(aspect.slope) "aspect.slope" else "elevation"
	}
	coef.lm <- matrix(NA, 12, length(nomc))
	dimnames(coef.lm)[[2]] <- nomc
	coef.lm[!inull, ] <- do.call(rbind, lapply(lm.res, coefficients))

	coef.idx <- 1:12
	coef.idx[inull] <- NA
	if(any(inull)){
		tmp <- coef.idx
		while(any(inull)){
			coef2 <- tmp
			shiftUp <- c(coef2[-1], coef2[1])
			tmp[inull] <- shiftUp[inull]
			tmp[!inull] <- coef.idx[!inull]
			inull <- is.na(tmp)
			if(any(inull)){
				tmp1 <- tmp
				shiftDown <- c(coef2[12], coef2[-12])
				tmp1[inull] <- shiftDown[inull]
				tmp1[!inull] <- tmp[!inull]
				tmp <- tmp1
				inull <- is.na(tmp)
			}
		}
		coef.idx <- tmp
	}

	return(list(model = lm.res[coef.idx], coeff = coef.lm[coef.idx, ],
				fitting = fitting, standardize = standardize))
}

##################################
old_Coeff.downscaling.LapseRate <- function(stnData){
	months <- as.numeric(substr(stnData$dates, 5, 6))

	coef <- matrix(NA, 12, 2)
	for(m in 1:12){
		ix <- which(months == m)
		glm.dat <- cbind(rep(stnData$dem, length(ix)), c(t(stnData$data[ix, , drop = FALSE])))
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

	ina <- which(is.na(coef[, 1]))
	if(length(ina) == 12) return(NULL)

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
	return(coef)
}

#################################################################################################

Temp_CoefDownscaling <- function(paramsGlmCoef){
	InsertMessagesTxt(main.txt.out, 'Compute downscaling coefficients ...')

	GeneralParameters <- paramsGlmCoef$GeneralParameters

	stnData <- paramsGlmCoef$stnData
	demData <- paramsGlmCoef$demData

	ijdem <- grid2pointINDEX(stnData, demData)
	stnData$dem <- demData$demMat[ijdem]
	stnData$dem[stnData$dem < 0] <- 0

	year1 <- GeneralParameters$Down.Date.Range$start.year
	year2 <- GeneralParameters$Down.Date.Range$end.year
	years <- as.numeric(substr(stnData$dates, 1, 4))
	iyrCoef <- years >= year1 & years <= year2

	stnData$data <- stnData$data[iyrCoef, , drop = FALSE]
	stnData$dates <- stnData$dates[iyrCoef]

	coef <- Coeff.downscaling.LapseRate(stnData, useClimato = FALSE, standardize = TRUE)

	outfile <- file.path(paramsGlmCoef$origdir, 'STN_DEM_GLM_COEF.txt')
	write.table(coef$coeff, file = outfile, col.names = TRUE, row.names = FALSE, quote = FALSE)

	outfile.rds <- file.path(paramsGlmCoef$origdir, 'STN_DEM_GLM_COEF.rds')
	saveRDS(coef, file = outfile.rds)

	InsertMessagesTxt(main.txt.out, 'Computing downscaling coefficients finished')
	rm(stnData, demData)
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
	ncInfos <- paramsDownscl$reanalData
	ncinfo <- ncInfos$ncinfo

	###############

	nc <- nc_open(ncInfos$nc.files[which(ncInfos$exist)[1]])
	lon.reanl <- nc$var[[ncinfo$varid]]$dim[[ncinfo$xo]]$vals
	lat.reanl <- nc$var[[ncinfo$varid]]$dim[[ncinfo$yo]]$vals
	nc_close(nc)

	xo <- order(lon.reanl)
	lon.reanl <- lon.reanl[xo]
	yo <- order(lat.reanl)
	lat.reanl <- lat.reanl[yo]

	nlon.r <- length(lon.reanl)
	nlat.r <- length(lat.reanl)
	xrnl <- lon.reanl[2]-lon.reanl[1]
	yrnl <- lat.reanl[2]-lat.reanl[1]

	res.max <- max(xrnl, yrnl)
	maxdist <- if(maxdist < res.max) sqrt(2) * res.max else maxdist

	###############

	demGrid <- paramsDownscl$demGrid

	xy.grid <- paramsDownscl$xy.grid
	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- length(xy.grid$lon)
	nlat0 <- length(xy.grid$lat)

	###############
	## coeff
	downCoef <- paramsDownscl$downCoef

	## elevation
	if(downCoef$fitting == "elevation") data.grid <- data.frame(z = c(demGrid))
	## polynomial
	# if(downCoef$fitting == "polynomial"){
	# 	data.grid <- data.frame(x = rep(xy.grid$lon, nlat0),
	# 							y = rep(xy.grid$lat, each = nlon0),
	# 							z = c(demGrid))
	# }
	## slope & aspect
	# if(downCoef$fitting == "aspect.slope"){
	# 	demData <- list(x = xy.grid$lon, y = xy.grid$lat, z = demGrid)
	# 	slpasp <- raster.slope.aspect(demData)
	# 	data.grid <- data.frame(s = c(slpasp$slope), a = c(slpasp$aspect), z = c(demGrid))
	# }
	## mix
	## data.grid <- data.frame(x = rep(xy.grid$lon, ny.grid), y = rep(xy.grid$lat, each = nx.grid),
	## 							s = c(slpasp$slope), a = c(slpasp$aspect), z = c(demGrid))

	if(downCoef$standardize){
		data.grid0 <- as.matrix(data.grid)
		moy <- colMeans(data.grid0, na.rm = TRUE)
		sds <- matrixStats::colSds(data.grid0, na.rm = TRUE)
		data.grid <- sweep(sweep(data.grid, 2, moy, FUN = "-"), 2, sds, FUN = "/")
		rm(data.grid0, moy, sds)
	}

	###############
	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	out.tt <- ncvar_def("temp", "DegC", list(dx, dy), -99, longname = "Dwonscaled temperature from reanalysis data", prec = "float", compression = 9)

	###############
	## DEM at reanalysis grid

	dem.reanal <- list(lon = lon.reanl, lat = lat.reanl)
	dem.reanal$lon <- c(lon.reanl[1]-xrnl, lon.reanl, lon.reanl[nlon.r]+xrnl)
	dem.reanal$lat <- c(lat.reanl[1]-yrnl, lat.reanl, lat.reanl[nlat.r]+yrnl)
	dem.reanal <- cdt.aggregate.grid(c(xy.grid, list(z = demGrid)), grid.list = dem.reanal, FUN = mean, na.rm = TRUE)
	# slpasp.reanal <- raster.slope.aspect(dem.reanal)
	dem.reanal <- dem.reanal$z[-c(1, nlon.r+2), -c(1, nlat.r+2)]

	## elevation
	if(downCoef$fitting == "elevation") data.reanal <- data.frame(z = c(dem.reanal))
	## polynomial
	# if(downCoef$fitting == "polynomial"){
	# 	data.reanal <- data.frame(x = rep(lon.reanl, nlat.r),
	# 							  y = rep(lat.reanl, each = nlon.r),
	# 							  z = c(dem.reanal))
	# }
	## slope & aspect
	# if(downCoef$fitting == "aspect.slope"){
	# 	slpasp.reanal$slope <- slpasp.reanal$slope[-c(1, nlon.r+2), -c(1, nlat.r+2)]
	# 	slpasp.reanal$aspect <- slpasp.reanal$aspect[-c(1, nlon.r+2), -c(1, nlat.r+2)]
	# 	data.reanal <- data.frame(s = c(slpasp.reanal$slope), a = c(slpasp.reanal$aspect), z = c(dem.reanal))
	# }
	## mix
	## data.reanal <- data.frame(x = rep(lon.reanl, nlat.r), y = rep(lat.reanl, each = nlon.r),
	## 				s = c(slpasp.reanal$slope), a = c(slpasp.reanal$aspect), z = c(dem.reanal))

	if(downCoef$standardize){
		data.reanal0 <- as.matrix(data.reanal)
		moy <- colMeans(data.reanal0, na.rm = TRUE)
		sds <- matrixStats::colSds(data.reanal0, na.rm = TRUE)
		data.reanal <- sweep(sweep(data.reanal, 2, moy, FUN = "-"), 2, sds, FUN = "/")
		rm(data.reanal0, moy, sds)
	}

	locations.reanl <- expand.grid(lon = lon.reanl, lat = lat.reanl)
	coordinates(locations.reanl) <- ~lon+lat

	##############
	## create grid

	interp.grid <- list(x = xy.grid$lon, y = xy.grid$lat,
					z = matrix(1, nlon0, nlat0),
					slp = matrix(0, nlon0, nlat0),
					asp = matrix(0, nlon0, nlat0))
	interp.grid <- createGrid.merging(interp.grid, coarse.grid = FALSE)

	cells <- SpatialPixels(points = interp.grid$newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)))@grid
	bGrd <- if(use.block) createBlock(cells@cellsize, 1, 5) else NULL

	###############
	InsertMessagesTxt(main.txt.out, "Downscale  Reanalysis ...")

	is.parallel <- doparallel(length(which(ncInfos$exist)) >= 30)
	`%parLoop%` <- is.parallel$dofun

	toExports <- "cdt.interp.surface.grid"
	packages <- c('gstat', 'automap', 'ncdf4')
	ret <- foreach(jj = seq_along(ncInfos$nc.files), .packages = packages, .export = toExports) %parLoop% {
		if(ncInfos$exist[jj]){
			nc <- nc_open(ncInfos$nc.files[jj])
			tt.reanl <- ncvar_get(nc, varid = ncinfo$varid)
			nc_close(nc)
			tt.reanl <- if(ncinfo$xo < ncinfo$yo) tt.reanl[xo, yo] else t(tt.reanl)[xo, yo]
		}else return(NULL)

		date.reanl <- ncInfos$dates[[jj]]
		mon <- as.numeric(substr(date.reanl, 5, 6))

		if(downCoef$standardize){
			# tt.mean <- mean(tt.reanl, na.rm = TRUE)
			# tt.sd <- sd(tt.reanl, na.rm = TRUE)
			tt.mean <- downCoef$model[[mon]]$std.pars$mean['v']
			tt.sd <- downCoef$model[[mon]]$std.pars$sd['v']
			tt.reanl <- (tt.reanl-tt.mean)/tt.sd
		}

		resid <- tt.reanl - predict(downCoef$model[[mon]], newdata = data.reanal)

		############
		if(interp.method == 'FBL'){
			resid[is.na(resid)] <- 0
			residObj <- list(lon = lon.reanl, lat = lat.reanl, z = resid)
			residInterp <- cdt.interp.surface.grid(residObj, xy.grid)
			residInterp <- residInterp$z
		}else{
			locations.reanl$res <- c(resid)
			locations.reanl <- locations.reanl[!is.na(locations.reanl$res), ]
			if(length(locations.reanl$res) < 2) return(NULL)

			if(interp.method == 'Kriging'){
				if(length(locations.reanl$res) > 7){
					vgm <- try(autofitVariogram(res~1, input_data = locations.reanl, model = vgm.model, cressie = TRUE), silent = TRUE)
					vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
				}else vgm <- NULL
			}else vgm <- NULL

			grd.temp <- krige(res~1, locations = locations.reanl, newdata = interp.grid$newgrid, model = vgm,
								block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
			residInterp <- matrix(grd.temp$var1.pred, ncol = nlat0, nrow = nlon0)
		}

 		############
		residInterp[is.na(residInterp)] <- 0
		downTT <- predict(downCoef$model[[mon]], newdata = data.grid) + residInterp
		if(downCoef$standardize) downTT <- downTT * tt.sd + tt.mean
		downTT[is.na(downTT)] <- -99

		############
		year <- substr(date.reanl, 1, 4)
		month <- substr(date.reanl, 5, 6)
		if(freqData == 'daily') outncfrmt <- sprintf(Down.File.Format, year, month, substr(date.reanl, 7, 8))
		else if(freqData%in%c('pentad', 'dekadal')) outncfrmt <- sprintf(Down.File.Format, year, month, substr(date.reanl, 7, 7))
		else outncfrmt <- sprintf(Down.File.Format, year, month)
		outfl <- file.path(origdir, outncfrmt)

		nc <- nc_create(outfl, out.tt)
		ncvar_put(nc, out.tt, downTT)
		nc_close(nc)
		rm(residInterp, downTT, resid, tt.reanl)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	InsertMessagesTxt(main.txt.out, 'Downscaling  Reanalysis finished')
	rm(demGrid, demStand, dem.reanl, interp.grid, ObjGrd, ObjStn)
	gc()
	return(0)
}

#################################################################################################


###Downscaling JRA
GlmCoefDownscaling <- function(paramsGlmCoef){
	InsertMessagesTxt(main.txt.out, 'Compute downscaling coefficients ...')
	tcl("update")

	GeneralParameters <- paramsGlmCoef$GeneralParameters
	origdir <- paramsGlmCoef$origdir

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

	year1 <- GeneralParameters$Down.Date.Range$start.year
	year2 <- GeneralParameters$Down.Date.Range$end.year

	ijdem <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = lon.dem, lat = lat.dem))

	dem.stn <- grd.dem[ijdem]
	years <- as.numeric(substr(date.stn, 1, 4))
	iyrCoef <- years >= year1 & years <= year2
	data.stn <- data.stn[iyrCoef, , drop = FALSE]
	date.stn <- date.stn[iyrCoef]
	months <- as.numeric(substr(date.stn, 5, 6))
	coef <- array(NA, c(12, 2))
	for (m in 1:12){
		ix <- which(months == m)
		tt <- as.vector(t(data.stn[ix, , drop = FALSE]))
		z <- rep(dem.stn, length(ix))
		glm.dat <- data.frame(dem = z, tt = tt)
		glm.dat <- na.omit(glm.dat)
		if(length(glm.dat[, 1]) == 0) next #skip if all data NA
		moy <- unname(apply(glm.dat, 2, mean))
		ect <- unname(apply(glm.dat, 2, sd))
		if(ect[1] == 0 | ect[2] == 0) next  #skip if variance null
		glm.dat <- t((t(glm.dat)-moy)/ect)
		glm.dat[glm.dat[, 2] < -3, 2] <- -3
		glm.dat <- as.data.frame(glm.dat)
		glm.tt <- lm(tt~dem, data = glm.dat)
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

	outfile <- file.path(origdir, 'STN_DEM_GLM_COEF.txt')
	write.table(coef, file = outfile, col.names = FALSE, row.names = FALSE)

	InsertMessagesTxt(main.txt.out, 'Computing downscaling coefficients finished')
	tcl("update")
	rm(stnData, demData, grd.dem, data.stn)
	gc()
	return(0)
}

#################################################################################################

ReanalysisDownscaling <- function(paramsDownscl){
	InsertMessagesTxt(main.txt.out, "Downscale  Reanalysis ...")
	tcl("update")

	if(doparallel & length(paramsDownscl$reanalData$data) >= 20){
		klust <- makeCluster(nb_cores)
		registerDoParallel(klust)
		`%parLoop%` <- `%dopar%`
		closeklust <- TRUE
	}else{
		`%parLoop%` <- `%do%`
		closeklust <- FALSE
	}

	###############
	GeneralParameters <- paramsDownscl$GeneralParameters
	interp.method <- GeneralParameters$Interpolation.pars$interp.method
	nmin <- GeneralParameters$Interpolation.pars$nmin
	nmax <- GeneralParameters$Interpolation.pars$nmax
	maxdist <- GeneralParameters$Interpolation.pars$maxdist
	vgm.model <- GeneralParameters$Interpolation.pars$vgm.model
	use.block <- GeneralParameters$Interpolation.pars$use.block

	freqData <- GeneralParameters$period
	Down.File.Format <- GeneralParameters$Format$Down.File.Format
	origdir <- paramsDownscl$origdir

	###############
	xy.grid <- paramsDownscl$xy.grid
	newGrid <- defSpatialPixels(xy.grid)
	nlon0 <- length(xy.grid$lon)
	nlat0 <- length(xy.grid$lat)

	bGrd <- NULL
	if(use.block) bGrd <- createBlock(newGrid@grid@cellsize, 2, 5)

	###############
	downCoef <- paramsDownscl$downCoef

	###############
	reanalData <- paramsDownscl$reanalData
	lon.reanl <- reanalData$lon
	lat.reanl <- reanalData$lat
	date.reanl <- reanalData$dates
	data.reanl <- reanalData$data

	###############
	demGrid <- paramsDownscl$demGrid
	coords.reanl <- expand.grid(lon = lon.reanl, lat = lat.reanl)
	ijreanl <- grid2pointINDEX(list(lon = coords.reanl$lon, lat = coords.reanl$lat),
								list(lon = xy.grid$lon, lat = xy.grid$lat))
	dem.reanl <- demGrid[ijreanl]
	dem.reanl.mean <- mean(dem.reanl, na.rm = TRUE)
	dem.reanl.sd <- sd(dem.reanl, na.rm = TRUE)
	dem.reanl <- (dem.reanl-dem.reanl.mean)/dem.reanl.sd
	dim(dem.reanl) <- c(length(lon.reanl), length(lat.reanl))
	###
	dem.mean <- mean(demGrid, na.rm = TRUE)
	dem.sd <- sd(demGrid, na.rm = TRUE)
	dem <- (demGrid - dem.mean)/dem.sd

	###############
	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	out.tt <- ncvar_def("temp", "DegC", list(dx, dy), -99, longname = "Dwonscaled temperature from reanalysis data", prec = "float")

	packages <- c('sp', 'gstat', 'automap', 'ncdf4')

	ret <- foreach(jj= seq_along(data.reanl), .packages = packages) %parLoop% {
		tt.reanl <- data.reanl[[jj]]
		if(is.null(tt.reanl)) return(NULL)
		mon <- as.numeric(substr(date.reanl[jj], 5, 6))
		tt.mean <- mean(tt.reanl, na.rm = TRUE)
		tt.sd <- sd(tt.reanl, na.rm = TRUE)
		tt.std <- (tt.reanl-tt.mean)/tt.sd
		tt.res <- tt.std - (downCoef[mon, 2] * dem.reanl + downCoef[mon, 1])

		tt.Obj <- data.frame(expand.grid(lon = lon.reanl, lat = lat.reanl), res = c(tt.res))
		tt.Obj <- tt.Obj[!is.na(tt.Obj$res), ]
		coordinates(tt.Obj) <- ~lon+lat

		############
		if(interp.method == 'Kriging'){
			vgm <- try(autofitVariogram(res~1, input_data = tt.Obj, model = vgm.model, cressie = TRUE), silent = TRUE)
			if(!inherits(vgm, "try-error")) vgm <- vgm$var_model
			else vgm <- NULL
		}else vgm <- NULL

 		grd.temp <- krige(res~1, locations = tt.Obj, newdata = newGrid, model = vgm, block = bGrd,
 							nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
 		downTT <- matrix(grd.temp$var1.pred, ncol = nlat0, nrow = nlon0)
		downTT <- (downCoef[mon, 2] * dem + downCoef[mon, 1]) + downTT
		downTT <- downTT * tt.sd + tt.mean
		downTT[is.na(downTT)] <- -99

		############
		if(freqData == 'daily'){
			outfl <- file.path(origdir, sprintf(Down.File.Format, substr(date.reanl[jj], 1, 4),
								substr(date.reanl[jj], 5, 6), substr(date.reanl[jj], 7, 8)))
		}else  if(freqData == 'dekadal'){
			outfl <- file.path(origdir, sprintf(Down.File.Format, substr(date.reanl[jj], 1, 4),
								substr(date.reanl[jj], 5, 6), substr(date.reanl[jj], 7, 7)))
		}else  if(freqData == 'monthly'){
			outfl <- file.path(origdir, sprintf(Down.File.Format, substr(date.reanl[jj], 1, 4),
								substr(date.reanl[jj], 5, 6)))
		}

		nc2 <- nc_create(outfl, out.tt)
		ncvar_put(nc2, out.tt, downTT)
		nc_close(nc2)
	}
	if(closeklust) stopCluster(klust)

	InsertMessagesTxt(main.txt.out, 'Downscaling  Reanalysis finished')
	tcl("update")
	rm(reanalData, demGrid, dem)
	gc()
	return(0)
}

#################################################################################################

ComputeMeanBiasTemp <- function(comptMBiasparms){
	InsertMessagesTxt(main.txt.out, 'Compute bias factors ...')
	tcl("update")

	GeneralParameters <- comptMBiasparms$GeneralParameters
	freqData <- GeneralParameters$period
	months <- as.numeric(GeneralParameters$Bias.Months)
	bias.method <- GeneralParameters$Bias.Method
	min.len <- as.numeric(GeneralParameters$Bias.Factor$min.length)
	
	# res.coarse <- as.numeric(GeneralParameters$Interpolation.pars$res.coarse)
	res.coarse <- comptMBiasparms$res.coarse

	stnData <- comptMBiasparms$stnData
	id.stn <- stnData$id
	lon.stn <- stnData$lon
	lat.stn <- stnData$lat
	date.stn <- stnData$dates
	data.stn <- stnData$data

	downData <- comptMBiasparms$downData
	lon.down <- downData$lon
	lat.down <- downData$lat
	date.down <- downData$dates
	data.down <- downData$data
	###
	date.bias <- downData$dates

	ijGrd <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = lon.down, lat = lat.down))

	ibsdt <- date.bias%in%date.stn
	istdt <- date.stn%in%date.bias
	if(!any(ibsdt)) return(NULL)

	if(bias.method == 'Multiplicative.Bias.Mon'){
		data.down.stn <- t(sapply(data.down, function(x){
			if(!is.null(x)) x[ijGrd]
			else rep(NA, length(ijGrd))
		}))

		date.bias <- date.bias[ibsdt]
		data.down.stn <- data.down.stn[ibsdt, , drop = FALSE]
		# date.stn <- date.stn[istdt]
		data.stn <- data.stn[istdt, , drop = FALSE]

		month.stn <- as(substr(date.bias, 5, 6), 'numeric')
		dataf <- data.frame(id.stn = rep(id.stn, each = nrow(data.stn)),
					times = rep(month.stn, ncol(data.stn)), stn = c(data.stn), tt = c(data.down.stn))

		bias <- by(dataf, dataf$id.stn, bias.TT.times.fun, min.len)
		bias <- lapply(bias, function(x) sapply(x,'[[', 1))
		bias <- t(do.call('rbind', bias))

		rm(data.down.stn, dataf)
	}

	if(bias.method == 'Multiplicative.Bias.Var'){
		data.down.stn <- t(sapply(data.down, function(x){
			if(!is.null(x)) x[ijGrd]
			else rep(NA, length(ijGrd))
		}))

		date.bias <- date.bias[ibsdt]
		data.down.stn <- data.down.stn[ibsdt, , drop = FALSE]
		# date.stn <- date.stn[istdt]
		data.stn <- data.stn[istdt, , drop = FALSE]

		if(freqData == 'daily'){
			endmon <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
			vtimes <- cbind(unlist(sapply(endmon, function(j) 1:j)), rep(1:12, endmon), 1:365)
			xdaty <- paste(as.numeric(substr(date.bias, 7, 8)), as.numeric(substr(date.bias, 5, 6)), sep = '_')
			xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
			times.stn <- vtimes[match(xdaty, xvtm), 3]
			times.stn[is.na(times.stn)] <- 59
			## Add  +/- 5 days
			ix5days <- lapply(unique(times.stn), function(nt){
				ix1 <- which(times.stn == nt)
				ix1 <- c(sapply(ix1, function(x) x+(-5:5)))
				cbind(nt, ix1[ix1 > 0 & ix1 <= length(date.bias)])
			})
			ix5days <- do.call('rbind', ix5days)
			times.stn <- ix5days[, 1]
			data.down.stn <- data.down.stn[ix5days[, 2], ]
			data.stn <- data.stn[ix5days[, 2], ]
		}
		if(freqData == 'dekadal'){
			vtimes <- cbind(expand.grid(1:3, 1:12), 1:36)
			xdaty <- paste(as.numeric(substr(date.bias, 7, 7)), as.numeric(substr(date.bias, 5, 6)), sep = '_')
			xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
			times.stn <- vtimes[match(xdaty, xvtm), 3]
		}
		if(freqData == 'monthly'){
			times.stn <- as.numeric(substr(date.bias, 5, 6))
		}
		dataf <- data.frame(id.stn = rep(id.stn, each = nrow(data.stn)),
					times = rep(times.stn, ncol(data.stn)), stn = c(data.stn), tt = c(data.down.stn))

		bias <- by(dataf, dataf$id.stn, bias.TT.times.fun, min.len)
		bias <- lapply(bias, function(x) sapply(x,'[[', 1))
		bias <- t(do.call('rbind', bias))

		rm(data.down.stn, dataf)
	}

	if(bias.method == 'Quantile.Mapping'){
		if(doparallel & length(months) >= 3){
			klust <- makeCluster(nb_cores)
			registerDoParallel(klust)
			`%parLoop%` <- `%dopar%`
			closeklust <- TRUE
		}else{
			`%parLoop%` <- `%do%`
			closeklust <- FALSE
		}
		packages <- c('fitdistrplus')

		data.down[sapply(data.down, is.null)] <- list(matrix(NA, ncol = length(lat.down), nrow = length(lon.down)))
		idcoarse <- indexCoarseGrid(lon.down, lat.down, res.coarse)
		date.stn <- date.stn[istdt]
		data.stn <- data.stn[istdt, , drop = FALSE]
		month.stn <- as(substr(date.stn, 5, 6), 'numeric')

		toExports <- c('data.down', 'id.stn', 'data.stn', 'fit.norm.temp', 'min.len')

		pars.Obs.Stn <- vector(mode = 'list', length = 12)
		pars.Obs.Stn[months] <- foreach (m = months, .packages = packages, .export = toExports) %:% 
		  foreach (id = id.stn, .packages = packages, .export = toExports) %parLoop% {
			xdata <- data.stn[month.stn == m, id.stn == id]
		    fit.norm.temp(xdata, min.len)
		}

		month.down <- as(substr(date.bias, 5, 6), 'numeric')
		data.down <- simplify2array(data.down)
		data.down <- data.down[idcoarse$ix, idcoarse$iy, ]
		nbgrd <- prod(dim(data.down)[1:2])
		nt <- dim(data.down)[3]
		data.down <- aperm(data.down, c(3, 1, 2))
		dim(data.down) <- c(nt, nbgrd)

		id.grdDown <- 1:nbgrd
		pars.Crs.Down <- vector(mode = 'list', length = 12)
		pars.Crs.Down[months] <- foreach (m = months, .packages = packages, .export = toExports) %:% 
		  foreach (id = id.grdDown, .packages = packages, .export = toExports) %parLoop% {
			xdata <- data.down[month.down == m, id.grdDown == id]
		    fit.norm.temp(xdata, min.len)
		}
		if(closeklust) stopCluster(klust)

		pars.Stn <- extract.Gau.parameters(months, pars.Obs.Stn)
		pars.Down <- extract.Gau.parameters(months, pars.Crs.Down)
		bias <- list(fit.stn = pars.Obs.Stn, fit.down = pars.Crs.Down, pars.stn = pars.Stn, pars.down = pars.Down)
		rm(pars.Obs.Stn, pars.Crs.Down, pars.Stn, pars.Down)
	}

	InsertMessagesTxt(main.txt.out, 'Computing bias factors finished')
	tcl("update")
	rm(stnData, downData, data.stn, data.down)
	gc()
	return(bias)
}

#################################################################################################

InterpolateMeanBiasTemp <- function(interpBiasparams){
	InsertMessagesTxt(main.txt.out, 'Interpolate bias factors ...')
	tcl("update")

	GeneralParameters <- interpBiasparams$GeneralParameters
	freqData <- GeneralParameters$period

	interp.method <- GeneralParameters$Interpolation.pars$interp.method
	as.dim.elv <- GeneralParameters$Interpolation.pars$elev.3rd.dim
	latlong <- GeneralParameters$Interpolation.pars$latlon.unit
	normalize <- GeneralParameters$Interpolation.pars$normalize
	rad.lon <- as.numeric(GeneralParameters$Interpolation.pars$rad.lon)
	rad.lat <- as.numeric(GeneralParameters$Interpolation.pars$rad.lat)
	rad.elv <- as.numeric(GeneralParameters$Interpolation.pars$rad.elv)
	nmin <- as.numeric(GeneralParameters$Interpolation.pars$nmin)
	nmax <- as.numeric(GeneralParameters$Interpolation.pars$nmax)
	maxdist <- as.numeric(GeneralParameters$Interpolation.pars$maxdist)
	use.block <- GeneralParameters$Interpolation.pars$use.block
	vgm.model <- as.character(GeneralParameters$Interpolation.pars$vgm.model[[1]])
	min.stn <- as.numeric(GeneralParameters$Bias.Factor$min.stn)

	# res.coarse <- as.numeric(GeneralParameters$Interpolation.pars$res.coarse)
	res.coarse <- interpBiasparams$res.coarse
	# xy.rfe <- interpBiasparams$xy.rfe

	months <- as.numeric(GeneralParameters$Bias.Months)
	bias.method <- GeneralParameters$Bias.Method
	bias.pars <- interpBiasparams$bias.pars
	meanBiasPrefix <- GeneralParameters$Format$Mean.Bias.Prefix

	origdir <- interpBiasparams$origdir
	
	#############
	stnData <- interpBiasparams$stnData
	lon.stn <- stnData$lon
	lat.stn <- stnData$lat

	#############
	demData <- interpBiasparams$demData
	lon.dem <- demData$lon
	lat.dem <- demData$lat
	grd.dem <- demData$demMat
	grd.dem[grd.dem < 0] <- 0

	xy.grid <- interpBiasparams$xy.grid
	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- length(xy.grid$lon)
	nlat0 <- length(xy.grid$lat)

	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	xy.dim <- list(dx, dy)

	## regrid dem si necessaire
	demSp <- defSpatialPixels(list(lon = lon.dem, lat = lat.dem))
	is.regridDEM <- is.diffSpatialPixelsObj(grdSp, demSp, tol = 1e-07)
	
	demGrid <- list(x = lon.dem, y = lat.dem, z = grd.dem)
	if(is.regridDEM){
		demGrid <- interp.surface.grid(demGrid, list(x = xy.grid$lon, y = xy.grid$lat))
	}

	## dem at stn loc
	ijGrd <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), xy.grid)
	ObjStn <- list(x = lon.stn, y = lat.stn, z = demGrid$z[ijGrd])

	## create grid to interp
	bGrd <- NULL
	if(interp.method == 'NN'){
		interp.grid <- createGrid(ObjStn, demGrid, as.dim.elv = as.dim.elv, latlong = latlong,
							normalize = normalize, coarse.grid = TRUE, res.coarse = res.coarse)
		xy.maxdist <- sqrt(sum((c(rad.lon, rad.lat)*(apply(coordinates(interp.grid$newgrid)[, c('lon', 'lat')], 2,
						function(x) diff(range(x)))/(c(nlon0, nlat0)-1)))^2))
		elv.grd <- range(demGrid$z, na.rm = TRUE)
		nelv <- length(seq(elv.grd[1], elv.grd[2], 100))
		nelv <- if(nelv > 1) nelv else 2
		z.maxdist <- rad.elv*(diff(range(coordinates(interp.grid$newgrid)[, 'elv']))/(nelv-1))
		xy.maxdist <- if(xy.maxdist < res.coarse) res.coarse else xy.maxdist
	}else{
		interp.grid <- createGrid(ObjStn, demGrid, as.dim.elv = FALSE,
								coarse.grid = TRUE, res.coarse = res.coarse)
		maxdist <- if(maxdist < res.coarse) res.coarse else maxdist
		cells <- SpatialPixels(points = interp.grid$newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)))@grid
		bGrd <- createBlock(cells@cellsize, 2, 5)
	}

	params.grid <- list(xy.grid = xy.grid, interp.grid = interp.grid)
	# mrgRaindat$params.grid <- params.grid

	## interpolation
	if(bias.method != 'Quantile.Mapping'){
		itimes <- as.numeric(rownames(bias.pars))
		if(bias.method == 'Multiplicative.Bias.Mon'){
			ntimes <- 12
		}
		if(bias.method == 'Multiplicative.Bias.Var'){
			ntimes <- switch(freqData, 'daily' = 365, 'dekadal' = 36, 'monthly' = 12)				
		}

		if(doparallel & length(itimes) >= 3){
			klust <- makeCluster(nb_cores)
			registerDoParallel(klust)
			`%parLoop%` <- `%dopar%`
			closeklust <- TRUE
		}else{
			`%parLoop%` <- `%do%`
			closeklust <- FALSE
		}
		packages <- c('sp', 'gstat', 'automap')
		toExports <- c('bias.pars', 'itimes', 'interp.grid', 'interp.method',
						'min.stn', 'vgm.model', 'nmin', 'nmax', 'maxdist', 'bGrd')
		BIAS <- vector(mode = 'list', length = ntimes)
		BIAS[itimes] <- foreach(m = itimes, .packages = packages, .export = toExports) %parLoop% {
			locations.stn <- interp.grid$coords.stn
			locations.stn$pars <- bias.pars[itimes == m, ]
			locations.stn <- locations.stn[!is.na(locations.stn$pars), ]
			if(length(locations.stn$pars) < min.stn) return(matrix(1, ncol = nlat0, nrow = nlon0))
			if(!any(locations.stn$pars != 1)) return(matrix(1, ncol = nlat0, nrow = nlon0))

			if(interp.method == 'Kriging'){
				vgm <- try(autofitVariogram(pars~1, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
				if(!inherits(vgm, "try-error")) vgm <- vgm$var_model
				else vgm <- NULL
			}else vgm <- NULL

			xstn <- as.data.frame(locations.stn)
			xadd <- as.data.frame(interp.grid$coords.grd)
			xadd$pars <- 1
			iadd <- rep(TRUE, nrow(xadd))

			for(k in 1:nrow(xstn)){
				if(interp.method == 'NN'){
					xy.dst <- sqrt((xstn$lon[k]-xadd$lon)^2+(xstn$lat[k]-xadd$lat)^2)*sqrt(2)
					z.dst <- abs(xstn$elv[k]-xadd$elv)
					z.iadd <- (z.dst < z.maxdist) & (xy.dst == min(xy.dst))
					iadd <- iadd & (xy.dst >= xy.maxdist) & !z.iadd
				}else{
					dst <- sqrt((xstn$lon[k]-xadd$lon)^2+(xstn$lat[k]-xadd$lat)^2)*sqrt(2)
					iadd <- iadd & (dst >= maxdist)
				}
			}
			xadd <- xadd[iadd, ]
			locations.stn <- rbind(xstn, xadd)

			if(interp.method == 'NN'){
				coordinates(locations.stn) <- ~lon+lat+elv
				pars.grd <- krige(pars~1, locations = locations.stn, newdata = interp.grid$newgrid, nmax = 1, debug.level = 0)	
			}else{
				coordinates(locations.stn) <- ~lon+lat
				pars.grd <- krige(pars~1, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
									block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				ina <- is.na(pars.grd$var1.pred)
				if(any(ina)){
					pars.grd.na <- krige(var1.pred~1, locations = pars.grd[!ina, ], newdata = interp.grid$newgrid[ina, ], model = vgm,
										block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
					pars.grd$var1.pred[ina] <- pars.grd.na$var1.pred
				}
			}
			grdbias <- pars.grd$var1.pred
			grdbias[grdbias > 3] <- 3
			grdbias[grdbias < 0] <- 1
			grdbias[is.na(grdbias)] <- 1
			matrix(grdbias, ncol = nlat0, nrow = nlon0)
		}
		if(closeklust) stopCluster(klust)
			
		bias.factor <- list(bias = bias.pars, interp.bias = BIAS)
		# mrgRaindat$bias.factor <- bias.factor

		#Defines netcdf output
		grd.bs <- ncvar_def("bias", "", xy.dim, NA, longname= "Multiplicative Mean Bias Factor", prec = "float")
		for(jfl in itimes){
			outnc <- file.path(origdir, paste(meanBiasPrefix, '_', jfl, '.nc', sep = ''))
			nc2 <- nc_create(outnc, grd.bs)
			ncvar_put(nc2, grd.bs, bias.factor$interp.bias[[jfl]])
			nc_close(nc2)
		}
		rm(BIAS, bias.factor)
	}else{
		if(doparallel & length(months) >= 3){
			klust <- makeCluster(nb_cores)
			registerDoParallel(klust)
			`%parLoop%` <- `%dopar%`
			closeklust <- TRUE
		}else{
			`%parLoop%` <- `%do%`
			closeklust <- FALSE
		}
		packages <- c('sp', 'gstat', 'automap')
		toExports <- c('bias.pars', 'months', 'interp.grid', 'interp.method', 'vgm.model',
						'min.stn', 'nmin', 'nmax', 'maxdist', 'bGrd')

		PARS.stn <- vector(mode = 'list', length = 12)
		PARS.stn[months] <- foreach(m = months, .packages = packages, .export = toExports) %parLoop% {
			pars.mon <- lapply(1:2, function(j){
				locations.stn <- interp.grid$coords.stn
				locations.stn$pars <- bias.pars$pars.stn[[m]][[j]]
				locations.stn <- locations.stn[!is.na(locations.stn$pars), ]
				if(length(locations.stn$pars) < min.stn) return(NULL)

				extrm <- quantile(locations.stn$pars, probs = c(0.0001, 0.9999))
				locations.stn <- locations.stn[locations.stn$pars > extrm[1] & locations.stn$pars < extrm[2], ]

				if(interp.method == 'Kriging'){
					vgm <- try(autofitVariogram(pars~1, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
					if(!inherits(vgm, "try-error")) vgm <- vgm$var_model
					else vgm <- NULL
				}else vgm <- NULL

				xstn <- as.data.frame(locations.stn)
				xadd <- as.data.frame(interp.grid$coords.grd)
				xadd$pars <- bias.pars$pars.down[[m]][[j]]
				xadd <- xadd[!is.na(xadd$pars), ]
				extrm1 <- quantile(xadd$pars, probs = c(0.0001, 0.9999))
				xadd <- xadd[xadd$pars > extrm1[1] & xadd$pars < extrm1[2], ]
				iadd <- rep(TRUE, nrow(xadd))

				for(k in 1:nrow(xstn)){
					if(interp.method == 'NN'){
						xy.dst <- sqrt((xstn$lon[k]-xadd$lon)^2+(xstn$lat[k]-xadd$lat)^2)*sqrt(2)
						z.dst <- abs(xstn$elv[k]-xadd$elv)
						z.iadd <- (z.dst < z.maxdist) & (xy.dst == min(xy.dst))
						iadd <- iadd & (xy.dst >= xy.maxdist) & !z.iadd
					}else{
						dst <- sqrt((xstn$lon[k]-xadd$lon)^2+(xstn$lat[k]-xadd$lat)^2)*sqrt(2)
						iadd <- iadd & (dst >= maxdist)
					}
				}
				xadd <- xadd[iadd, ]
				locations.stn <- rbind(xstn, xadd)

				if(interp.method == 'NN'){
					coordinates(locations.stn) <- ~lon+lat+elv
					pars.grd <- krige(pars~1, locations = locations.stn, newdata = interp.grid$newgrid, nmax = 1, debug.level = 0)
				}else{
					coordinates(locations.stn) <- ~lon+lat
					pars.grd <- krige(pars~1, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
										block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
					if(j == 2) pars.grd$var1.pred[pars.grd$var1.pred < 0] <- NA
					ina <- is.na(pars.grd$var1.pred)
					if(any(ina)){
						pars.grd.na <- krige(var1.pred~1, locations = pars.grd[!ina, ], newdata = interp.grid$newgrid[ina, ], model = vgm,
											block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
						pars.grd$var1.pred[ina] <- pars.grd.na$var1.pred
					}
				}
				matrix(pars.grd$var1.pred, ncol = nlat0, nrow = nlon0)
			})
			names(pars.mon) <- c('mean', 'sd')
			pars.mon
		}

		PARS.down <- vector(mode = 'list', length = 12)
		PARS.down[months] <- foreach(m = months, .packages = packages, .export = toExports) %parLoop% {
			pars.mon <- lapply(1:2, function(j){
				locations.down <- interp.grid$coords.grd
				locations.down$pars <- bias.pars$pars.down[[m]][[j]]
				locations.down <- locations.down[!is.na(locations.down$pars), ]
				if(length(locations.down$pars) < min.stn) return(NULL)
				extrm <- quantile(locations.down$pars, probs = c(0.0001, 0.9999))
				locations.down <- locations.down[locations.down$pars > extrm[1] & locations.down$pars < extrm[2], ]

				if(interp.method == 'Kriging'){
					vgm <- try(autofitVariogram(pars~1, input_data = locations.down, model = vgm.model, cressie = TRUE), silent = TRUE)
					if(!inherits(vgm, "try-error")) vgm <- vgm$var_model
					else vgm <- NULL
				}else vgm <- NULL

				if(interp.method == 'NN'){
					pars.grd <- krige(pars~1, locations = locations.down, newdata = interp.grid$newgrid, nmax = 1, debug.level = 0)
				}else{
					pars.grd <- krige(pars~1, locations = locations.down, newdata = interp.grid$newgrid, model = vgm,
									block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
					if(j == 2) pars.grd$var1.pred[pars.grd$var1.pred < 0] <- NA
					ina <- is.na(pars.grd$var1.pred)
					if(any(ina)){
						pars.grd.na <- krige(var1.pred~1, locations = pars.grd[!ina, ], newdata = interp.grid$newgrid[ina, ], model = vgm,
											block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
						pars.grd$var1.pred[ina] <- pars.grd.na$var1.pred
					}
				}
				matrix(pars.grd$var1.pred, ncol = nlat0, nrow = nlon0)
			})
			names(pars.mon) <- c('mean', 'sd')
			pars.mon
		}
		if(closeklust) stopCluster(klust)


		for(m in months){
			for(j in 1:2){
				par.stn <- PARS.stn[[m]][[j]]
				par.down <- PARS.down[[m]][[j]]
				if(!is.null(par.stn) & !is.null(par.down)){
					ix <- is.na(par.stn)
					par.stn[ix] <- par.down[ix]
					PARS.stn[[m]][[j]] <- par.stn
				}
				if(is.null(par.down)){
					ret <- matrix(NA, ncol = nlat0, nrow = nlon0)
					if(j == 1) ret[] <- 0
					if(j == 2) ret[] <- 1
					PARS.stn[[m]][[j]] <- ret
					PARS.down[[m]][[j]] <- ret
				}
			}
		}

		params.distr <- list(fit.stn = bias.pars$fit.stn, fit.down = bias.pars$fit.down,
									pars.stn = bias.pars$pars.stn, pars.down = bias.pars$pars.down,
									interp.pars.stn = PARS.stn, interp.pars.down = PARS.down)
		# mrgRaindat$params.distr <- params.distr

		#Defines netcdf output
		grd.mean <- ncvar_def("mean", "degC", xy.dim, NA, longname= "Means normal distribution", prec = "float")
		grd.sd <- ncvar_def("sd", "degC", xy.dim, NA, longname= "Standard deviations normal distribution", prec = "float")

		for(jfl in months){
			if(is.null(params.distr$interp.pars.stn[[jfl]]$mean) |
				is.null(params.distr$interp.pars.stn[[jfl]]$sd)){
				mean.nc <- params.distr$interp.pars.down[[jfl]]$mean
				sd.nc <- params.distr$interp.pars.down[[jfl]]$sd
			}else{
				mean.nc <- params.distr$interp.pars.stn[[jfl]]$mean
				sd.nc <- params.distr$interp.pars.stn[[jfl]]$sd
			}
			outnc1 <- file.path(origdir, paste('Gaussian_Pars.STN', '_', jfl, '.nc', sep = ''))
			nc1 <- nc_create(outnc1, list(grd.mean, grd.sd))
			ncvar_put(nc1, grd.mean, mean.nc)
			ncvar_put(nc1, grd.sd, sd.nc)
			nc_close(nc1)
		}
			
		for(jfl in months){
			outnc2 <- file.path(origdir, paste('Gaussian_Pars.REANAL', '_', jfl, '.nc', sep = ''))
			nc2 <- nc_create(outnc2, list(grd.mean, grd.sd))
			ncvar_put(nc2, grd.mean, params.distr$interp.pars.down[[jfl]]$mean)
			ncvar_put(nc2, grd.sd, params.distr$interp.pars.down[[jfl]]$sd)
			nc_close(nc2)
		}
		rm(PARS.stn, PARS.down, params.distr)
	}

	# outfile <- file.path(origdir, 'DataUsed2ComputeBias.RData')
	# save(mrgRaindat, file = outfile)
	rm(stnData, demData, demGrid, ObjStn, interp.grid, params.grid)
	gc()
	InsertMessagesTxt(main.txt.out, 'Interpolating bias factors finished')
	tcl("update")
	return(0)
}

#################################################################################################

##correct downscaled reanalysis bias
AjdMeanBiasTemp <- function(adjMeanBiasparms){
	InsertMessagesTxt(main.txt.out, 'Correct Reanalysis Bias ...')
	tcl("update")

	GeneralParameters <- adjMeanBiasparms$GeneralParameters
	origdir <- adjMeanBiasparms$origdir

	freqData <- GeneralParameters$period
	bias.method <- GeneralParameters$Bias.Method
	biasDir <- GeneralParameters$IO.files$Bias.dir
	meanBiasPrefix <- GeneralParameters$Format$Mean.Bias.Prefix
	adjRreanalFF <- GeneralParameters$Format$Adj.File.Format
	months <- sort(as.numeric(GeneralParameters$Adjust.Months))

	downData <- adjMeanBiasparms$downData

	if(bias.method == "Multiplicative.Bias.Mon"){
		biasFile <- file.path(biasDir, paste(meanBiasPrefix, '_', months, '.nc', sep = ''))
		exist.bias <- unlist(lapply(biasFile, file.exists))
		if(any(!exist.bias)){
			miss.bias <- months[!exist.bias]
			for(j in seq_along(miss.bias)){
				msg <- paste(meanBiasPrefix, '_', miss.bias[j], '.nc', sep = '')
				InsertMessagesTxt(main.txt.out, paste(msg, "doesn't exist"), format = TRUE)
				tcl("update")
			}
			return(NULL)
		}
		nc <- nc_open(biasFile[which(exist.bias)[1]])
		lon <- nc$dim[[1]]$vals
		lat <- nc$dim[[2]]$vals
		nc_close(nc)
		BIAS <- vector(mode = 'list', length = 12)
		BIAS[months] <- lapply(seq_along(months), function(m){
			nc <- nc_open(biasFile[m])
			xvar <- ncvar_get(nc, varid = "bias")
			nc_close(nc)
			xvar
		})
		toExports <- c('lon', 'lat', 'BIAS')
	}

	if(bias.method == "Multiplicative.Bias.Var"){
		if(freqData == 'daily'){
			endmon <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
			vtimes <- cbind(unlist(sapply(endmon, function(j) 1:j)), rep(1:12, endmon), 1:365)
			vtimes <- vtimes[vtimes[, 2]%in%months, , drop = FALSE]

			daty <- downData$dates
			daty <- daty[as.numeric(substr(daty, 5, 6))%in%vtimes[, 2]]
			xdaty <- paste(as.numeric(substr(daty, 7, 8)), as.numeric(substr(daty, 5, 6)), sep = '_')
			xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
			times.stn <- vtimes[match(xdaty, xvtm), 3]
			times.stn[is.na(times.stn)] <- 59
			times.stn <- sort(unique(times.stn))
			BIAS <- vector(mode = 'list', length = 365) 
		}
		if(freqData == 'dekadal'){
			vtimes <- cbind(expand.grid(1:3, 1:12), 1:36)
			vtimes <- vtimes[vtimes[, 2]%in%months, , drop = FALSE]

			daty <- downData$dates
			daty <- daty[as.numeric(substr(daty, 5, 6))%in%vtimes[, 2]]
			xdaty <- paste(as.numeric(substr(daty, 7, 7)), as.numeric(substr(daty, 5, 6)), sep = '_')
			xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
			times.stn <- vtimes[match(xdaty, xvtm), 3]
			times.stn <- sort(unique(times.stn))
			BIAS <- vector(mode = 'list', length = 36)
		}
		if(freqData == 'monthly'){
			times.stn <- months
			BIAS <- vector(mode = 'list', length = 12)
		}
			
		biasFile <- file.path(biasDir, paste(meanBiasPrefix, '_', times.stn, '.nc', sep = ''))
		exist.bias <- unlist(lapply(biasFile, file.exists))

		if(any(!exist.bias)){
			miss.bias <- times.stn[!exist.bias]
			for(j in seq_along(miss.bias)){
				msg <- paste(meanBiasPrefix, '_', miss.bias[j], '.nc', sep = '')
				InsertMessagesTxt(main.txt.out, paste(msg, "doesn't exist"), format = TRUE)
				tcl("update")
			}
			return(NULL)
		}
		nc <- nc_open(biasFile[which(exist.bias)[1]])
		lon <- nc$dim[[1]]$vals
		lat <- nc$dim[[2]]$vals
		nc_close(nc)
		BIAS[times.stn] <- lapply(seq_along(times.stn), function(m){
			nc <- nc_open(biasFile[m])
			xvar <- ncvar_get(nc, varid = nc$var[[1]]$name) #varid = "bias"
			nc_close(nc)
			xvar
		})
		toExports <- c('lon', 'lat', 'BIAS')
	}

	if(bias.method == "Quantile.Mapping"){
		pars.stnFile <- file.path(biasDir, paste('Gaussian_Pars.STN', '_', months, '.nc', sep = ''))
		exist.pars.stn <- unlist(lapply(pars.stnFile, file.exists))
		if(any(!exist.pars.stn)){
			miss.pars.stn <- months[!exist.pars.stn]
			for(j in seq_along(miss.pars.stn)){
				msg <- paste('Gaussian_Pars.STN', '_', miss.pars.stn[j], '.nc', sep = '')
				InsertMessagesTxt(main.txt.out, paste(msg, "doesn't exist"), format = TRUE)
				tcl("update")
			}
			return(NULL)
		}

		pars.downFile <- file.path(biasDir, paste('Gaussian_Pars.REANAL', '_', months, '.nc', sep = ''))
		exist.pars.down <- unlist(lapply(pars.downFile, file.exists))
		if(any(!exist.pars.down)){
			miss.pars.down <- months[!exist.pars.down]
			for(j in seq_along(miss.pars.down)){
				msg <- paste('Gaussian_Pars.REANAL', '_', miss.pars.down[j], '.nc', sep = '')
				InsertMessagesTxt(main.txt.out, paste(msg, "doesn't exist"), format = TRUE)
				tcl("update")
			}
			return(NULL)
		}
		nc <- nc_open(pars.stnFile[which(exist.pars.stn)[1]])
		lon <- nc$dim[[1]]$vals
		lat <- nc$dim[[2]]$vals
		nc_close(nc)
		PARS.stn <- vector(mode = 'list', length = 12)
		PARS.down <- vector(mode = 'list', length = 12)
		PARS.stn[months] <- lapply(seq_along(months), function(m){
			nc <- nc_open(pars.stnFile[m])
			mean1 <- ncvar_get(nc, varid = "mean")
			sd1 <- ncvar_get(nc, varid = "sd")
			nc_close(nc)
			list(mean = mean1, sd = sd1)
		})

		PARS.down[months] <- lapply(seq_along(months), function(m){
			nc <- nc_open(pars.downFile[m])
			mean1 <- ncvar_get(nc, varid = "mean")
			sd1 <- ncvar_get(nc, varid = "sd")
			nc_close(nc)
			list(mean = mean1, sd = sd1)
		})
		toExports <- c('quantile.mapping.Gau', 'lon', 'lat', 'PARS.stn', 'PARS.down')
	}

	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", lon)
	dy <- ncdim_def("Lat", "degreeN", lat)
	grd.bsadj <- ncvar_def("temp", "DegC", list(dx, dy), -99, longname= "Bias Corrected Reanalysis", prec = "float")

	irnl <- !sapply(downData$data, is.null)
	if(!any(irnl)){
		InsertMessagesTxt(main.txt.out, "Downscaled reanalysis data not found", format = TRUE)
		return(NULL)
	}

	downData$dates <- downData$dates[irnl]
	downData$data <- downData$data[irnl]

	####

	if(doparallel & length(downData$dates) >= 30){
		klust <- makeCluster(nb_cores)
		registerDoParallel(klust)
		`%parLoop%` <- `%dopar%`
		closeklust <- TRUE
	}else{
		`%parLoop%` <- `%do%`
		closeklust <- FALSE
	}

	packages <- c('ncdf4')
	toExports <- c(toExports, 'downData', 'bias.method', 'origdir',
					 'grd.bsadj', 'freqData', 'adjRreanalFF')

	ret <- foreach(jfl = seq_along(downData$dates), .packages = packages, .export = toExports) %parLoop% {
		xtmp <- downData$data[[jfl]]
		dtmp <- downData$dates[jfl]
		if(bias.method == "Multiplicative.Bias.Var"){
			if(freqData == 'daily'){
				ann <- as.numeric(substr(dtmp, 1, 4))
				iday <- as.numeric(strftime(as.Date(dtmp, format = '%Y%m%d'), format = '%j'))
				ijt <- ifelse(ann%%4 == 0 & iday > 59, iday-1, iday)
			}
			if(freqData == 'dekadal'){
				mon <- as.numeric(substr(dtmp, 5, 6))
				dek <- as.numeric(substr(dtmp, 7, 7))
				annual.dek <- expand.grid(dek = 1:3, mon = 1:12)
				ijt <- which(annual.dek$dek == dek & annual.dek$mon == mon)
			}
			if(freqData == 'monthly'){
				ijt <- as.numeric(substr(dtmp, 5, 6))
			}
		}else{
			ijt <- as.numeric(substr(dtmp, 5, 6))
		}

		if(bias.method == "Quantile.Mapping"){
			xadj <- quantile.mapping.Gau(xtmp, PARS.stn[[ijt]], PARS.down[[ijt]])
			xadj[is.nan(xadj)] <- xtmp[is.nan(xadj)]
			xadj[is.infinite(xadj)] <- xtmp[is.infinite(xadj)]
		}else xadj <- round(xtmp * BIAS[[ijt]], 2)

		xadj[is.na(xadj)] <- -99

		############
		if(freqData == 'daily'){
			outfl <- file.path(origdir, sprintf(adjRreanalFF, substr(dtmp, 1, 4), substr(dtmp, 5, 6), substr(dtmp, 7, 8)))
		}else  if(freqData == 'dekadal'){
			outfl <- file.path(origdir, sprintf(adjRreanalFF, substr(dtmp, 1, 4), substr(dtmp, 5, 6), substr(dtmp, 7, 7)))
		}else  if(freqData == 'monthly'){
			outfl <- file.path(origdir, sprintf(adjRreanalFF, substr(dtmp, 1, 4), substr(dtmp, 5, 6)))
		}
		#Save adjusted data
		nc2 <- nc_create(outfl, grd.bsadj)
		ncvar_put(nc2, grd.bsadj, xadj)
		nc_close(nc2)
		return(0)
	}
	if(closeklust) stopCluster(klust)
	rm(downData)
	gc()
	InsertMessagesTxt(main.txt.out, 'Bias Correction finished')
	tcl("update")
	return(0)
}

#################################################################################################

ComputeLMCoefTemp <- function(comptLMparams){
	GeneralParameters <- comptLMparams$GeneralParameters
	min.len <- as.numeric(GeneralParameters$LM.Params$min.length)
	min.stn <- as.numeric(GeneralParameters$LM.Params$min.stn)
	months <- sort(as.numeric(GeneralParameters$LM.Months))
	interp.method <- GeneralParameters$Interpolation.pars$interp.method
	nmin <- as.numeric(GeneralParameters$Interpolation.pars$nmin)
	nmax <- as.numeric(GeneralParameters$Interpolation.pars$nmax)
	maxdist <- as.numeric(GeneralParameters$Interpolation.pars$maxdist)
	vgm.model <- as.character(GeneralParameters$Interpolation.pars$vgm.model[[1]])
	use.block <- GeneralParameters$Interpolation.pars$use.block

	rad.lon <- as.numeric(GeneralParameters$Interpolation.pars$rad.lon)
	rad.lat <- as.numeric(GeneralParameters$Interpolation.pars$rad.lat)
	rad.elv <- as.numeric(GeneralParameters$Interpolation.pars$rad.elv)
	as.dim.elv <- GeneralParameters$Interpolation.pars$elev.3rd.dim
	latlong <- GeneralParameters$Interpolation.pars$latlon.unit
	normalize <- GeneralParameters$Interpolation.pars$normalize

	year1 <- as.numeric(GeneralParameters$LM.Date.Range$start.year)
	year2 <- as.numeric(GeneralParameters$LM.Date.Range$end.year)

	origdir <- comptLMparams$origdir

	# res.coarse <- as.numeric(GeneralParameters$Interpolation.pars$res.coarse)
	res.coarse <- comptLMparams$res.coarse

	#############
	stnData <- comptLMparams$stnData
	id.stn <- stnData$id
	lon.stn <- stnData$lon
	lat.stn <- stnData$lat
	date.stn <- stnData$dates
	data.stn <- stnData$data

	#############
	demData <- comptLMparams$demData
	lon.dem <- demData$lon
	lat.dem <- demData$lat
	grd.dem <- demData$demMat
	grd.dem[grd.dem < 0] <- 0

	#############
	adjData <- comptLMparams$adjData
	lon.adj <- adjData$lon
	lat.adj <- adjData$lat
	# date.adj <- adjData$dates
	data.adj <- adjData$data

	xy.grid <- comptLMparams$xy.grid
	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- length(xy.grid$lon)
	nlat0 <- length(xy.grid$lat)

	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	xy.dim <- list(dx, dy)

	InsertMessagesTxt(main.txt.out, 'Compute LM Coefficients ...')
	tcl("update")

	## temp at stn loc
	ijtmp <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = lon.adj, lat = lat.adj))
	data.adj.stn  <- t(sapply(data.adj, function(x){
		if(!is.null(x)) x[ijtmp]
		else rep(NA, length(ijtmp))
	}))

	## regrid dem si necessaire
	demSp <- defSpatialPixels(list(lon = lon.dem, lat = lat.dem))
	is.regridDEM <- is.diffSpatialPixelsObj(grdSp, demSp, tol = 1e-07)
	
	demGrid <- list(x = lon.dem, y = lat.dem, z = grd.dem)
	if(is.regridDEM){
		demGrid <- interp.surface.grid(demGrid, list(x = xy.grid$lon, y = xy.grid$lat))
	}

	## dem at stn loc
	ijdem <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), xy.grid)
	ObjStn <- list(x = lon.stn, y = lat.stn, z = demGrid$z[ijdem])

	## create grid to interp
	bGrd <- NULL
	if(interp.method == 'NN'){
		interp.grid <- createGrid(ObjStn, demGrid, as.dim.elv = as.dim.elv, latlong = latlong,
							normalize = normalize, coarse.grid = TRUE, res.coarse = res.coarse)
		xy.maxdist <- sqrt(sum((c(rad.lon, rad.lat)*(apply(coordinates(interp.grid$newgrid)[, c('lon', 'lat')], 2,
						function(x) diff(range(x)))/(c(nlon0, nlat0)-1)))^2))
		elv.grd <- range(demGrid$z, na.rm = TRUE)
		nelv <- length(seq(elv.grd[1], elv.grd[2], 100))
		nelv <- if(nelv > 1) nelv else 2
		z.maxdist <- rad.elv*(diff(range(coordinates(interp.grid$newgrid)[, 'elv']))/(nelv-1))
		xy.maxdist <- if(xy.maxdist < res.coarse) res.coarse else xy.maxdist
	}else{
		interp.grid <- createGrid(ObjStn, demGrid, as.dim.elv = FALSE, res.coarse = res.coarse)
		maxdist <- if(maxdist < res.coarse) res.coarse else maxdist
		cells <- SpatialPixels(points = interp.grid$newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)))@grid
		bGrd <- createBlock(cells@cellsize, 2, 5)
	}

	month.stn <- as(substr(date.stn, 5, 6), 'numeric')
	year.stn <- as(substr(date.stn, 1, 4), 'numeric')
	iyear0 <- (year.stn >= year1 & year.stn <= year2) & (month.stn%in%months)

	data.stn.reg <- data.stn[iyear0, ]
	mon.stn.reg <- month.stn[iyear0]
	dataf <- data.frame(id.stn = rep(id.stn, each = nrow(data.stn.reg)), month = rep(mon.stn.reg, ncol(data.stn.reg)),
						stn = c(data.stn.reg), tt = c(data.adj.stn))

	##############
	model <- by(dataf, dataf$id.stn, fitLM.month.TT, min.len)
	model.coef <- lapply(model, function(x){
		sapply(x, function(m){
			if(is.null(m)) c(NA, NA)
			else coefficients(m)
		})
	})
	model.coef <- list(slope = sapply(model.coef, function(x) x[2, ]), intercept = sapply(model.coef, function(x) x[1, ]))

	if(doparallel & length(months) >= 3){
		klust <- makeCluster(nb_cores)
		registerDoParallel(klust)
		`%parLoop%` <- `%dopar%`
		closeklust <- TRUE
	}else{
		`%parLoop%` <- `%do%`
		closeklust <- FALSE
	}
	packages <- c('sp', 'gstat', 'automap')
	toExports <- c('model.coef', 'months', 'interp.grid', 'interp.method', 'min.stn',
				'vgm.model', 'nmin', 'nmax', 'maxdist', 'bGrd', 'nlat0', 'nlon0')
	MODEL.COEF <- vector(mode = 'list', length = 12)
	MODEL.COEF[months] <- foreach(m = months, .packages = packages, .export = toExports) %parLoop% {
		pars.mon <- lapply(1:2, function(jc){
			locations.stn <- interp.grid$coords.stn
			xcoef <- model.coef[[jc]]
			locations.stn$pars <- xcoef[as.numeric(rownames(xcoef)) == m, ]
			locations.stn <- locations.stn[!is.na(locations.stn$pars), ]
			if(length(locations.stn$pars) < min.stn) return(NULL)

			extrm <- quantile(locations.stn$pars, probs = c(0.0001, 0.9999))
			locations.stn <- locations.stn[locations.stn$pars > extrm[1] & locations.stn$pars < extrm[2], ]

			if(interp.method == 'Kriging'){
				vgm <- try(autofitVariogram(pars~1, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
				if(!inherits(vgm, "try-error")) vgm <- vgm$var_model
				else vgm <- NULL
			}else vgm <- NULL
			
			xstn <- as.data.frame(locations.stn)
			xadd <- as.data.frame(interp.grid$coords.grd)
			xadd$pars <- if(jc == 1) 1 else 0
			iadd <- rep(TRUE, nrow(xadd))

			for(k in 1:nrow(xstn)){
				if(interp.method == 'NN'){
					xy.dst <- sqrt((xstn$lon[k]-xadd$lon)^2+(xstn$lat[k]-xadd$lat)^2)*sqrt(2)
					z.dst <- abs(xstn$elv[k]-xadd$elv)
					z.iadd <- (z.dst < z.maxdist) & (xy.dst == min(xy.dst))
					iadd <- iadd & (xy.dst >= xy.maxdist) & !z.iadd
				}else{
					dst <- sqrt((xstn$lon[k]-xadd$lon)^2+(xstn$lat[k]-xadd$lat)^2)*sqrt(2)
					iadd <- iadd & (dst >= maxdist)
				}
			}
			xadd <- xadd[iadd, ]
			locations.stn <- rbind(xstn, xadd)
			if(interp.method == 'NN'){
				coordinates(locations.stn) <- ~lon+lat+elv
				pars.grd <- krige(pars~1, locations = locations.stn, newdata = interp.grid$newgrid, nmax = 1, debug.level = 0)
			}else{
				coordinates(locations.stn) <- ~lon+lat
				pars.grd <- krige(pars~1, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
									block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				ina <- is.na(pars.grd$var1.pred)
				if(any(ina)){
					pars.grd.na <- krige(var1.pred~1, locations = pars.grd[!ina, ], newdata = interp.grid$newgrid[ina, ], model = vgm,
											block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
					pars.grd$var1.pred[ina] <- pars.grd.na$var1.pred
				}
			}
			matrix(pars.grd$var1.pred, ncol = nlat0, nrow = nlon0)
		})
		names(pars.mon) <- c('slope', 'intercept')
		pars.mon
	}
	if(closeklust) stopCluster(klust)
	grd.slope <- ncvar_def("slope", "", xy.dim, NA, longname= "Linear model Coef: Slope", prec = "float")
	grd.intercept <- ncvar_def("intercept", "", xy.dim, NA, longname= "Linear model Coef: Intercept", prec = "float")

	for(jfl in months){
		if(is.null(MODEL.COEF[[jfl]]$slope) | is.null(MODEL.COEF[[jfl]]$intercept)){
			MODEL.COEF[[jfl]]$slope <- matrix(1, ncol = nlat0, nrow = nlon0)
			MODEL.COEF[[jfl]]$intercept <- matrix(0, ncol = nlat0, nrow = nlon0)
		}
		outnc1 <- file.path(origdir, paste('LM_Coefficient', '_', jfl, '.nc', sep = ''))
		nc1 <- nc_create(outnc1, list(grd.slope, grd.intercept))
		ncvar_put(nc1, grd.slope, MODEL.COEF[[jfl]]$slope)
		ncvar_put(nc1, grd.intercept, MODEL.COEF[[jfl]]$intercept)
		nc_close(nc1)
	}

	InsertMessagesTxt(main.txt.out, 'Computing LM Coefficients finished')
	tcl("update")
	rm(adjData, stnData, demData, data.adj.stn, grd.dem,
		data.stn, data.adj, demGrid, interp.grid, data.stn.reg,
		model, model.coef, MODEL.COEF)
	return(0)
}

#################################################################################################

MergingFunctionTemp <- function(paramsMRG){
	InsertMessagesTxt(main.txt.out, 'Merging data ...')
	tcl("update")

	GeneralParameters <- paramsMRG$GeneralParameters
	freqData <- GeneralParameters$period
	ncInfo <- paramsMRG$ncInfo

	#############
	if(doparallel & length(which(ncInfo$exist)) >= 20){
		klust <- makeCluster(nb_cores)
		registerDoParallel(klust)
		`%parLoop%` <- `%dopar%`
		closeklust <- TRUE
	}else{
		`%parLoop%` <- `%do%`
		closeklust <- FALSE
	}

	#############
	freqData <- GeneralParameters$period
	months <- GeneralParameters$Mrg.Months

	xy.grid <- paramsMRG$xy.grid
	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- length(xy.grid$lon)
	nlat0 <- length(xy.grid$lat)

	#############
	## Def ncdf
	dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	grd.nc.out <- ncvar_def('temp', "DegC", list(dx, dy), -99, longname = 'Reanalysis merged with station', prec = "float")

	#############
	stnData <- paramsMRG$stnData
	id.stn <- stnData$id
	lon.stn <- stnData$lon
	lat.stn <- stnData$lat
	date.stn <- stnData$dates
	data.stn <- stnData$data

	#############
	interp.method <- GeneralParameters$Interpolation.pars$interp.method
	nmin <- GeneralParameters$Interpolation.pars$nmin
	nmax <- GeneralParameters$Interpolation.pars$nmax
	maxdist <- GeneralParameters$Interpolation.pars$maxdist
	vgm.model <- GeneralParameters$Interpolation.pars$vgm.model[[1]]
	# use.block <- GeneralParameters$Interpolation.pars$use.block
	res.coarse <- maxdist/2
	res.coarse <- if(res.coarse  >= 0.25) res.coarse else 0.25
	maxdist <- if(maxdist < res.coarse) res.coarse else maxdist

	#############
	Mrg.Method <- GeneralParameters$Mrg.Method
	min.stn <- GeneralParameters$Mrg.set$min.stn

	outMask <- paramsMRG$outMask
	origdir <- paramsMRG$origdir
	Mrg.file.format <- GeneralParameters$FileFormat$Mrg.file.format

	#############
	demData <- paramsMRG$demData
	if(!is.null(demData)){
		grd.dem <- demData$demMat
		grd.dem[grd.dem < 0] <- 0
		demSp <- defSpatialPixels(list(lon = demData$lon, lat = demData$lat))
		is.regridDEM <- is.diffSpatialPixelsObj(grdSp, demSp, tol = 1e-07)
		
		demGrid <- list(x = demData$lon, y = demData$lat, z = grd.dem)
		if(is.regridDEM){
			demGrid <- interp.surface.grid(demGrid, list(x = xy.grid$lon, y = xy.grid$lat))
		}
		## dem at stn loc
		ijdem <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), xy.grid)
		dem.stn <- demGrid$z[ijdem]
	}else{
		demGrid <- list(x = xy.grid$lon, y = xy.grid$lat, z = matrix(1, nrow = nlon0, ncol = nlat0))
		dem.stn <- rep(1, length(lon.stn))
	}
	ObjStn <- list(x = lon.stn, y = lat.stn, z = dem.stn)

	## create grid to interp
	interp.grid <- createGrid(ObjStn, demGrid, as.dim.elv = FALSE, res.coarse = res.coarse)
	cells <- SpatialPixels(points = interp.grid$newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)))@grid
	bGrd <- createBlock(cells@cellsize, 2, 5)

	#############
	if(Mrg.Method == "Spatio-Temporal LM"){
		coefFiles <- file.path(GeneralParameters$IO.files$LMCoef.dir, paste('LM_Coefficient_', months, '.nc', sep = ''))
		existLMCfl <- file.exists(coefFiles)
		if(any(!existLMCfl)){
			for(i in which(!existLMCfl)) InsertMessagesTxt(main.txt.out, paste(coefFiles[i], "doesn't exist"), format = TRUE)
			return(NULL)
		}
		MODEL.COEF <- vector(mode = 'list', length = 12)
		for(fl in seq(coefFiles)){
			nc <- nc_open(coefFiles[fl])
			coef1 <- ncvar_get(nc, varid = 'slope')
			coef2 <- ncvar_get(nc, varid = 'intercept')
			nc_close(nc)
			MODEL.COEF[[months[fl]]] <- list(slope = coef1, intercept = coef2)
		}
	}

	#############
	nc <- nc_open(ncInfo$nc.files[ncInfo$exist][1])
	xlon <- nc$dim[[1]]$vals
	xlat <- nc$dim[[2]]$vals
	nc_close(nc)

	## gridded data at stn loc
	ijGrd <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = xlon, lat = xlat))

	#############
	packages <- c('ncdf4', 'gstat', 'automap')
	toExports <- c('ncInfo')
	ncdata <- foreach(jj = seq_along(ncInfo$nc.files), .packages = packages, .export = toExports) %parLoop% {
		if(ncInfo$exist[jj]){
			nc <- nc_open(ncInfo$nc.files[jj])
			xtmp <- ncvar_get(nc, varid = nc$var[[1]]$name)
			nc_close(nc)
		}else return(NULL)

		############
		locations.stn <- interp.grid$coords.stn
		locations.stn$stn <- data.stn[date.stn == ncInfo$dates[jj], ]
		locations.stn$tmp <- xtmp[ijGrd]
		xadd <- as.data.frame(interp.grid$coords.grd)
		xadd$tmp <- c(xtmp[interp.grid$idxy$ix, interp.grid$idxy$iy])
		xadd$stn <- xadd$tmp
		xadd$res <- 0
		interp.grid$newgrid$tmp <- c(xtmp)

		############
		noNA <- !is.na(locations.stn$stn)
		min.stn.nonNA <- length(which(noNA))
		locations.stn <- locations.stn[noNA, ]

		if(min.stn.nonNA >= min.stn){
			do.merging <- TRUE
			if(Mrg.Method == "Spatio-Temporal LM"){
				mo <- as(substr(ncInfo$dates[jj], 5, 6), 'numeric')
				sp.trend <- xtmp * MODEL.COEF[[mo]]$slope + MODEL.COEF[[mo]]$intercept
				locations.stn$res <- locations.stn$stn - sp.trend[ijGrd][noNA]
			}else{
				glm.stn <- glm(stn~tmp, data = locations.stn, family = gaussian)
				sp.trend <- predict(glm.stn, newdata = interp.grid$newgrid)
				sp.trend <- matrix(sp.trend, ncol = nlat0, nrow = nlon0)
				locations.stn$res <- residuals(glm.stn)
			}
			
			############
			locations.stn <- locations.stn[!is.na(locations.stn$res), ]
			extrm <- quantile(locations.stn$res, probs = c(0.0001, 0.9999))
			locations.stn <- locations.stn[locations.stn$res > extrm[1] & locations.stn$res < extrm[2], ]
		}else do.merging <- FALSE

		############

		if(do.merging){
			if(interp.method == 'Kriging'){
				vgm <- try(autofitVariogram(res~1, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
				if(!inherits(vgm, "try-error")) vgm <- vgm$var_model
				else vgm <- NULL
			}else vgm <- NULL

			###########
			xstn <- as.data.frame(locations.stn)
			iadd <- rep(TRUE, nrow(xadd))
			for(k in 1:nrow(xstn)){
				dst <- sqrt((xstn$lon[k]-xadd$lon)^2+(xstn$lat[k]-xadd$lat)^2)*sqrt(2)
				iadd <- iadd & (dst >= maxdist)
			}

			xadd <- xadd[iadd, ]
			locations.stn <- rbind(xstn, xadd)
			coordinates(locations.stn) <- ~lon+lat

			###########
			res.grd <- krige(res~1, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
								block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
			ina <- is.na(res.grd$var1.pred)
			if(any(ina)){
				res.grd.na <- krige(var1.pred~1, locations = res.grd[!ina, ], newdata = interp.grid$newgrid[ina, ], model = vgm,
										block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				res.grd$var1.pred[ina] <- res.grd.na$var1.pred
			}
			resid <- matrix(res.grd$var1.pred, ncol = nlat0, nrow = nlon0)
			resid[is.na(resid)] <- 0

			out.mrg <- sp.trend + resid
		}else out.mrg <- xtmp

		#Apply mask for area of interest
		if(!is.null(outMask)) out.mrg[is.na(outMask)] <- NA
		out.mrg[is.na(out.mrg)] <- -99

		############
		if(freqData == 'daily'){
			outfl <- file.path(origdir, sprintf(Mrg.file.format, substr(ncInfo$dates[jj], 1, 4),
								substr(ncInfo$dates[jj], 5, 6), substr(ncInfo$dates[jj], 7, 8)))
		}else  if(freqData == 'dekadal'){
			outfl <- file.path(origdir, sprintf(Mrg.file.format, substr(ncInfo$dates[jj], 1, 4),
								substr(ncInfo$dates[jj], 5, 6), substr(ncInfo$dates[jj], 7, 7)))
		}else  if(freqData == 'monthly'){
			outfl <- file.path(origdir, sprintf(Mrg.file.format, substr(ncInfo$dates[jj], 1, 4),
								substr(ncInfo$dates[jj], 5, 6)))
		}

		nc2 <- nc_create(outfl, grd.nc.out)
		ncvar_put(nc2, grd.nc.out, out.mrg)
		nc_close(nc2)
		return(0)
	}
	if(closeklust) stopCluster(klust)

	InsertMessagesTxt(main.txt.out, 'Merging finished')
	tcl("update")

	return(0)
}

#################################################################################################



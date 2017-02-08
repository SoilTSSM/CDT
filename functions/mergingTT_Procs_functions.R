
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
		## standardize
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
	
	if(paramsDownscl$memType == 2){
		reanalData <- read.NetCDF.Data(paramsDownscl$reanalData$read.ncdf.parms)
		if(is.null(reanalData)) return(NULL)
		reanalDataExist <- length(reanalData$data)
	}else reanalDataExist <- length(which(paramsDownscl$reanalData$exist))

	if(doparallel & reanalDataExist >= 20){
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
	vgm.model <- str_trim(GeneralParameters$Interpolation.pars$vgm.model[[1]])
	use.block <- GeneralParameters$Interpolation.pars$use.block

	freqData <- GeneralParameters$period
	Down.File.Format <- GeneralParameters$Format$Down.File.Format
	origdir <- paramsDownscl$origdir

	#############
	# auxvar <- c('dem', 'slp', 'asp')
	# is.auxvar <- c(GeneralParameters$auxvar$dem, GeneralParameters$auxvar$slope, GeneralParameters$auxvar$aspect)
	# if(any(is.auxvar)){
	# 	formule <- formula(paste('res', '~', paste(auxvar[is.auxvar], collapse = '+'), sep = ''))
	# }else formule <- formula(paste('res', '~', 1, sep = ''))

	###############
	if(paramsDownscl$memType == 2){
		lon.reanl <- reanalData$lon
		lat.reanl <- reanalData$lat
		dates.reanl <- reanalData$dates
		data.reanl <- reanalData$data
	}else{
		nc <- nc_open(paramsDownscl$reanalData$nc.files[which(paramsDownscl$reanalData$exist)[1]])
		lon.reanl <- nc$dim[[paramsDownscl$reanalData$ncinfo$xo]]$vals
		lat.reanl <- nc$dim[[paramsDownscl$reanalData$ncinfo$yo]]$vals
		nc_close(nc)
		xo <- order(lon.reanl)
		lon.reanl <- lon.reanl[xo]
		yo <- order(lat.reanl)
		lat.reanl <- lat.reanl[yo]
	}

	###############
	InsertMessagesTxt(main.txt.out, "Downscale  Reanalysis ...")
	tcl("update")

	###############
	xy.grid <- paramsDownscl$xy.grid
	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- length(xy.grid$lon)
	nlat0 <- length(xy.grid$lat)

	###############
	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	out.tt <- ncvar_def("temp", "DegC", list(dx, dy), -99, longname = "Dwonscaled temperature from reanalysis data", prec = "float")

	###############

	## DEM at reanalysis grid
	coords.reanl <- expand.grid(lon = lon.reanl, lat = lat.reanl)
	ijreanl <- grid2pointINDEX(list(lon = coords.reanl$lon, lat = coords.reanl$lat), xy.grid)

	demGrid <- paramsDownscl$demGrid
	demres <- grdSp@grid@cellsize
	slpasp <- slope.aspect(demGrid, demres[1], demres[2], filter = "sobel")

	ObjGrd <- list(x = xy.grid$lon, y = xy.grid$lat, z = demGrid, slp = slpasp$slope, asp = slpasp$aspect)
	ObjStn <- list(x = coords.reanl$lon, y = coords.reanl$lat, z = demGrid[ijreanl], slp = slpasp$slope[ijreanl], asp = slpasp$aspect[ijreanl])
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
	dim(dem.reanl) <- c(length(lon.reanl), length(lat.reanl))

	###############
	packages <- c('sp', 'gstat', 'automap', 'ncdf4')

	jloop <- if(paramsDownscl$memType == 2) seq_along(data.reanl) else seq_along(paramsDownscl$reanalData$nc.files)
	ret <- foreach(jj = jloop, .packages = packages) %parLoop% {
		if(paramsDownscl$memType == 2){
			tt.reanl <- data.reanl[[jj]]
			if(is.null(tt.reanl)) return(NULL)
			date.reanl <- dates.reanl[jj]
		}else{
			if(paramsDownscl$reanalData$exist[jj]){
				nc <- nc_open(paramsDownscl$reanalData$nc.files[jj])
				tt.reanl <- ncvar_get(nc, varid = paramsDownscl$reanalData$ncinfo$varid)
				nc_close(nc)
				tt.reanl <- tt.reanl[xo, yo]
				if(paramsDownscl$reanalData$ncinfo$yo == 1){
					tt.reanl <- matrix(c(tt.reanl), nrow = length(lon.reanl), ncol = length(lat.reanl), byrow = TRUE)
				}
			}else return(NULL)
			date.reanl <- paramsDownscl$reanalData$dates[[jj]]
		}

		############
		## standardize reanal
		tt.mean <- mean(tt.reanl, na.rm = TRUE)
		tt.sd <- sd(tt.reanl, na.rm = TRUE)
		tt.std <- (tt.reanl-tt.mean)/tt.sd

		mon <- as.numeric(substr(date.reanl, 5, 6))
	
		locations.reanl <- interp.grid$coords.stn
		locations.reanl$res <- c(tt.std - (downCoef[mon, 2] * dem.reanl + downCoef[mon, 1]))
		locations.reanl <- locations.reanl[!is.na(locations.reanl$res), ]

		############
		if(interp.method == 'Kriging'){
			vgm <- try(autofitVariogram(res~1, input_data = locations.reanl, model = vgm.model, cressie = TRUE), silent = TRUE)
			vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
		}else vgm <- NULL

		grd.temp <- krige(res~1, locations = locations.reanl, newdata = interp.grid$newgrid, model = vgm,
							block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
 
		############
		# if(any(is.auxvar)) locations.reanl <- locations.reanl[Reduce("&", as.data.frame(!is.na(locations.reanl@data[, auxvar[is.auxvar]]))), ]

		# if(interp.method == 'Kriging'){
		# 	vgm <- try(autofitVariogram(formule, input_data = locations.reanl, model = vgm.model, cressie = TRUE), silent = TRUE)
		# 	vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
		# }else vgm <- NULL

		# block <- if(any(is.auxvar)) NULL else bGrd
		# grd.temp <- krige(formule, locations = locations.reanl, newdata = interp.grid$newgrid, model = vgm,
		# 				block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)

 		############
		downTT <- matrix(grd.temp$var1.pred, ncol = nlat0, nrow = nlon0)
		downTT[is.na(downTT)] <- 0
		downTT <- (downCoef[mon, 2] * demStand + downCoef[mon, 1]) + downTT
		downTT <- downTT * tt.sd + tt.mean
		downTT[is.na(downTT)] <- -99

		############
		if(freqData == 'daily'){
			outfl <- file.path(origdir, sprintf(Down.File.Format, substr(date.reanl, 1, 4),
								substr(date.reanl, 5, 6), substr(date.reanl, 7, 8)))
		}else  if(freqData == 'dekadal'){
			outfl <- file.path(origdir, sprintf(Down.File.Format, substr(date.reanl, 1, 4),
								substr(date.reanl, 5, 6), substr(date.reanl, 7, 7)))
		}else  if(freqData == 'monthly'){
			outfl <- file.path(origdir, sprintf(Down.File.Format, substr(date.reanl, 1, 4),
								substr(date.reanl, 5, 6)))
		}

		nc2 <- nc_create(outfl, out.tt)
		ncvar_put(nc2, out.tt, downTT)
		nc_close(nc2)
	}
	if(closeklust) stopCluster(klust)

	InsertMessagesTxt(main.txt.out, 'Downscaling  Reanalysis finished')
	tcl("update")
	rm(demGrid, demStand, dem.reanl, interp.grid, ObjGrd, ObjStn)
	if(paramsDownscl$memType == 2) rm(reanalData, data.reanl)
	gc()
	return(0)
}

#################################################################################################

ComputeMeanBiasTemp <- function(comptMBiasparms){
	GeneralParameters <- comptMBiasparms$GeneralParameters
	freqData <- GeneralParameters$period
	months <- as.numeric(GeneralParameters$Bias.Months)
	bias.method <- GeneralParameters$Bias.Method
	min.len <- as.numeric(GeneralParameters$Bias.Factor$min.length)
	
	# res.coarse <- as.numeric(GeneralParameters$Interpolation.pars$res.coarse)
	res.coarse <- comptMBiasparms$res.coarse

	###############
	stnData <- comptMBiasparms$stnData
	id.stn <- stnData$id
	lon.stn <- stnData$lon
	lat.stn <- stnData$lat
	date.stn <- stnData$dates
	data.stn <- stnData$data

	###############
	if(comptMBiasparms$memType == 2){
		downData <- read.NetCDF.Data(comptMBiasparms$downData)
		if(is.null(downData)) return(NULL)
		ijGrd <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = downData$lon, lat = downData$lat))
		data.down.stn <- t(sapply(downData$data, function(x){
			if(!is.null(x)) x[ijGrd]
			else rep(NA, length(ijGrd))
		}))
		if(bias.method == 'Quantile.Mapping'){
			data.down <- downData$data
			data.down[sapply(data.down, is.null)] <- list(matrix(NA, ncol = length(downData$lat), nrow = length(downData$lon)))
			idcoarse <- indexCoarseGrid(downData$lon, downData$lat, res.coarse)
			data.down <- t(sapply(data.down, function(x) x[idcoarse$ix, idcoarse$iy, drop = FALSE]))
			ptsData1 <- expand.grid(lon = downData$lon[idcoarse$ix], lat = downData$lat[idcoarse$iy])
		}
	}else{
		nstn <- length(lon.stn)
		ptsData <- list(lon = lon.stn, lat = lat.stn)
		if(bias.method == 'Quantile.Mapping'){
			idcoarse <- indexCoarseGrid(comptMBiasparms$xy.grid$lon, comptMBiasparms$xy.grid$lat, res.coarse)
			ptsData1 <- expand.grid(lon = comptMBiasparms$xy.grid$lon[idcoarse$ix], lat = comptMBiasparms$xy.grid$lat[idcoarse$iy])
			nbgrd <- nrow(ptsData1)
			ptsData <- list(lon = c(ptsData$lon, ptsData1$lon), lat = c(ptsData$lat, ptsData1$lat))
		}
		downData <- read.NetCDF.Data2Points(comptMBiasparms$downData, ptsData)
		if(is.null(downData)) return(NULL)
		data.down0 <- t(sapply(downData$data, function(x) if(!is.null(x)) x else rep(NA, length(ptsData$lon))))
		data.down.stn <- data.down0[, 1:nstn]
		if(bias.method == 'Quantile.Mapping') data.down <- data.down0[, nstn+(1:nbgrd)]
	}

	###############
	date.bias <- downData$dates
	ibsdt <- date.bias%in%date.stn
	istdt <- date.stn%in%date.bias
	if(!any(ibsdt)) return(NULL)

	###############
	InsertMessagesTxt(main.txt.out, 'Compute bias factors ...')
	tcl("update")

	if(bias.method == 'Multiplicative.Bias.Mon'){
		date.bias <- date.bias[ibsdt]
		data.down.stn <- data.down.stn[ibsdt, , drop = FALSE]
		data.stn <- data.stn[istdt, , drop = FALSE]

		month.stn <- as(substr(date.bias, 5, 6), 'numeric')
		dataf <- data.frame(id.stn = rep(id.stn, each = nrow(data.stn)),
					times = rep(month.stn, ncol(data.stn)), stn = c(data.stn), tt = c(data.down.stn))

		bias <- by(dataf, dataf$id.stn, bias.TT.times.fun, min.len)
		bias <- lapply(bias, function(x) sapply(x,'[[', 1))
		bias <- t(do.call('rbind', bias))

		bias.pars <- list(bias.stn = bias, lon.stn = lon.stn, lat.stn = lat.stn, id.stn = id.stn,
							data.stn = data.stn, date.down = data.down.stn, date = date.bias)

		rm(data.down.stn, dataf)
	}

	if(bias.method == 'Multiplicative.Bias.Var'){
		date.bias <- date.bias[ibsdt]
		data.down.stn <- data.down.stn[ibsdt, , drop = FALSE]
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

		bias.pars <- list(bias.stn = bias, lon.stn = lon.stn, lat.stn = lat.stn, id.stn = id.stn,
							data.stn = data.stn, date.down = data.down.stn, date = date.bias)
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

		date.stn <- date.stn[istdt]
		data.stn <- data.stn[istdt, , drop = FALSE]
		month.stn <- as(substr(date.stn, 5, 6), 'numeric')
		month.down <- as(substr(date.bias, 5, 6), 'numeric')

		packages <- c('fitdistrplus')
		toExports <- c('data.down', 'data.down.stn', 'data.stn', 'fit.norm.temp', 'min.len')

		parsDistr <- vector(mode = 'list', length = 12)
		parsDistr[months] <- foreach (m = months, .packages = packages, .export = toExports) %parLoop% {
			xstn <- data.stn[month.stn == m, , drop = FALSE]
			xdownstn <- data.down.stn[month.down == m, , drop = FALSE]
			xdown <- data.down[month.down == m, , drop = FALSE]
			xstn <- lapply(seq(ncol(xstn)), function(j) fit.norm.temp(xstn[, j], min.len))
			xdownstn <- lapply(seq(ncol(xdownstn)), function(j) fit.norm.temp(xdownstn[, j], min.len))
			xdown <- lapply(seq(ncol(xdown)), function(j) fit.norm.temp(xdown[, j], min.len))
			list(stn = xstn, downstn = xdownstn, down = xdown)
		}
		if(closeklust) stopCluster(klust)

		pars.Obs.Stn <- lapply(parsDistr, '[[', 1)
		pars.Obs.down <- lapply(parsDistr, '[[', 2)
		pars.Crs.down <- lapply(parsDistr, '[[', 3)

		SW.stn <- outputSWNTest(pars.Obs.Stn, months)
		SW.downstn <- outputSWNTest(pars.Obs.down, months)
		SW.down <- outputSWNTest(pars.Crs.down, months)

		pars.Stn <- extractNormDistrParams(pars.Obs.Stn, months)
		pars.Downstn <- extractNormDistrParams(pars.Obs.down, months)
		pars.Down <- extractNormDistrParams(pars.Crs.down, months)

		parsSW <- vector(mode = 'list', length = 12)
		parsSW[months] <- lapply(months, function(j){
			istn <- SW.stn[[j]] == 'yes' & SW.downstn[[j]] == 'yes'
			idown <- SW.down[[j]] == 'yes'
			list(down = idown, stn = istn)
		})

		parsSW.Down <- lapply(parsSW, '[[', 1)
		parsSW.Stn <- lapply(parsSW, '[[', 2)
		bias <- list(pars.sw.stn = parsSW.Stn, pars.sw.down = parsSW.Down, pars.stn = pars.Stn,
					pars.downstn = pars.Downstn, pars.down = pars.Down)

		##########
		bias.pars <- list(fit.stn = pars.Obs.Stn, fit.downstn = pars.Obs.down, fit.down = pars.Crs.down,
						lon.stn = lon.stn, lat.stn = lat.stn, id.stn = id.stn,
						lon.down = comptMBiasparms$xy.grid$lon[idcoarse$ix],
						lat.down = comptMBiasparms$xy.grid$lat[idcoarse$iy],
						data.stn = data.stn, data.downstn = data.down.stn,
						data.down = data.down, date = date.bias)
		rm(parsDistr, pars.Obs.Stn, pars.Obs.down, pars.Crs.down, pars.Stn, pars.Down,
			pars.Downstn, parsSW, parsSW.Down, parsSW.Stn)
	}

	save(bias.pars, file = file.path(comptMBiasparms$origdir, "BIAS_PARAMS.RData"))

	InsertMessagesTxt(main.txt.out, 'Computing bias factors finished')
	tcl("update")
	rm(stnData, downData, data.stn, bias.pars)
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

	months <- as.numeric(GeneralParameters$Bias.Months)
	bias.method <- GeneralParameters$Bias.Method
	bias.pars <- interpBiasparams$bias.pars
	meanBiasPrefix <- GeneralParameters$Format$Mean.Bias.Prefix

	origdir <- interpBiasparams$origdir

	#############
	auxvar <- c('dem', 'slp', 'asp')
	is.auxvar <- c(GeneralParameters$auxvar$dem, GeneralParameters$auxvar$slope, GeneralParameters$auxvar$aspect)
	if(any(is.auxvar)){
		formule <- formula(paste('pars', '~', paste(auxvar[is.auxvar], collapse = '+'), sep = ''))
	}else formule <- formula(paste('pars', '~', 1, sep = ''))

	#############
	stnData <- interpBiasparams$stnData
	lon.stn <- stnData$lon
	lat.stn <- stnData$lat

	#############
	xy.grid <- interpBiasparams$xy.grid
	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- length(xy.grid$lon)
	nlat0 <- length(xy.grid$lat)

	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	xy.dim <- list(dx, dy)

	#############
	demData <- interpBiasparams$demData
	lon.dem <- demData$lon
	lat.dem <- demData$lat
	grd.dem <- demData$demMat
	grd.dem[grd.dem < 0] <- 0
	demSp <- defSpatialPixels(list(lon = lon.dem, lat = lat.dem))
	is.regridDEM <- is.diffSpatialPixelsObj(grdSp, demSp, tol = 1e-07)

	demGrid <- list(x = lon.dem, y = lat.dem, z = grd.dem)
	if(is.regridDEM){
		demGrid <- interp.surface.grid(demGrid, list(x = xy.grid$lon, y = xy.grid$lat))
	}

	demres <- grdSp@grid@cellsize
	slpasp <- slope.aspect(demGrid$z, demres[1], demres[2], filter = "sobel")
	demGrid$slp <- slpasp$slope
	demGrid$asp <- slpasp$aspect

	ijGrd <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), xy.grid)
	ObjStn <- list(x = lon.stn, y = lat.stn, z = demGrid$z[ijGrd], slp = demGrid$slp[ijGrd], asp = demGrid$asp[ijGrd])

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
		maxdist <- sqrt(xy.maxdist^2 + z.maxdist^2)
	}else{
		interp.grid <- createGrid(ObjStn, demGrid, as.dim.elv = FALSE,
								coarse.grid = TRUE, res.coarse = res.coarse)
		maxdist <- if(maxdist < res.coarse) res.coarse else maxdist
		cells <- SpatialPixels(points = interp.grid$newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)))@grid
		bGrd <- createBlock(cells@cellsize, 2, 5)
	}

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
		toExports <- c('bias.pars', 'itimes', 'interp.grid', 'interp.method', 'formule', 'auxvar',
						'is.auxvar', 'min.stn', 'vgm.model', 'nmin', 'nmax', 'maxdist', 'bGrd')
		BIAS <- vector(mode = 'list', length = ntimes)
		BIAS[itimes] <- foreach(m = itimes, .packages = packages, .export = toExports) %parLoop% {
			locations.stn <- interp.grid$coords.stn
			locations.stn$pars <- bias.pars[itimes == m, ]
			locations.stn <- locations.stn[!is.na(locations.stn$pars), ]
			
			extrm <- quantile(locations.stn$pars, probs = c(0.001, 0.999))
			locations.stn <- locations.stn[locations.stn$pars > extrm[1] & locations.stn$pars < extrm[2], ]

			if(length(locations.stn$pars) < min.stn) return(matrix(1, ncol = nlat0, nrow = nlon0))
			if(!any(locations.stn$pars != 1)) return(matrix(1, ncol = nlat0, nrow = nlon0))

			if(any(is.auxvar)) locations.stn <- locations.stn[Reduce("&", as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))), ]
			if(interp.method == 'Kriging'){
				vgm <- try(autofitVariogram(formule, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
				vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
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
				pars.grd <- krige(pars~1, locations = locations.stn, newdata = interp.grid$newgrid,
									nmax = 1, maxdist = maxdist, debug.level = 0)	
			}else{
				coordinates(locations.stn) <- ~lon+lat
				if(any(is.auxvar)){
					locations.stn <- locations.stn[Reduce("&", as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))), ]
					block <- NULL
				}else block <- bGrd

				pars.grd <- krige(formule, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
									block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)

				extrm <- c(min(locations.stn$pars, na.rm = TRUE), max(locations.stn$pars, na.rm = TRUE))
				ixtrm <- is.na(pars.grd$var1.pred) | (pars.grd$var1.pred <= extrm[1] | pars.grd$var1.pred >= extrm[2])
				pars.grd$var1.pred[ixtrm] <- NA

				ina <- is.na(pars.grd$var1.pred)
				if(any(ina)){
					pars.grd.na <- krige(var1.pred~1, locations = pars.grd[!ina, ], newdata = interp.grid$newgrid[ina, ], model = vgm,
										block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
					pars.grd$var1.pred[ina] <- pars.grd.na$var1.pred
				}
			}
			grdbias <- matrix(pars.grd$var1.pred, ncol = nlat0, nrow = nlon0)
			grdbias[grdbias > 1.5] <- 1
			grdbias[grdbias < 0.6] <- 1
			grdbias[is.na(grdbias)] <- 1
			grdbias
		}
		if(closeklust) stopCluster(klust)

		#Defines netcdf output
		grd.bs <- ncvar_def("bias", "", xy.dim, NA, longname= "Multiplicative Mean Bias Factor", prec = "float")
		for(jfl in itimes){
			outnc <- file.path(origdir, paste(meanBiasPrefix, '_', jfl, '.nc', sep = ''))
			nc2 <- nc_create(outnc, grd.bs)
			ncvar_put(nc2, grd.bs, BIAS[[jfl]])
			nc_close(nc2)
		}
		rm(BIAS)
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
		toExports <- c('bias.pars', 'months', 'interp.grid', 'interp.method', 'vgm.model', 'formule',
						'auxvar', 'is.auxvar', 'min.stn', 'nmin', 'nmax', 'maxdist', 'bGrd')

		PARS.stn <- vector(mode = 'list', length = 12)
		PARS.stn[months] <- foreach(m = months, .packages = packages, .export = toExports) %parLoop% {
			pars.mon <- lapply(1:2, function(j){
				locations.stn <- interp.grid$coords.stn
				locations.stn$pars <- bias.pars$pars.stn[[m]][, j]
				locations.stn$pars[!bias.pars$pars.sw.stn[[m]]] <- NA
				locations.stn <- locations.stn[!is.na(locations.stn$pars), ]
				if(length(locations.stn$pars) < min.stn) return(NULL)

				if(interp.method == 'Kriging'){
					vgm <- try(autofitVariogram(formule, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
					vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
				}else vgm <- NULL

				xstn <- as.data.frame(locations.stn)
				xadd <- as.data.frame(interp.grid$coords.grd)
				xadd$pars <- bias.pars$pars.down[[m]][, j]
				xadd$pars[!bias.pars$pars.sw.down[[m]]] <- NA

				xaddstn <- as.data.frame(interp.grid$coords.stn)
				xaddstn$pars <- bias.pars$pars.downstn[[m]][, j]
				xaddstn$pars[!bias.pars$pars.sw.stn[[m]]] <- NA

				xadd <- rbind(xadd, xaddstn)
				xadd <- xadd[!is.na(xadd$pars), ]
				if(length(xadd$pars) < min.stn) return(NULL)

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

				extrm <- quantile(locations.stn$pars, probs = c(0.001, 0.99))
				locations.stn <- locations.stn[locations.stn$pars > extrm[1] & locations.stn$pars < extrm[2], ]

				if(interp.method == 'NN'){
					locations.stn <- locations.stn[!duplicated(locations.stn[, c('lon', 'lat', 'elv')]), ]
					coordinates(locations.stn) <- ~lon+lat+elv
					pars.grd <- krige(pars~1, locations = locations.stn, newdata = interp.grid$newgrid,
										nmax = 1, maxdist = maxdist, debug.level = 0)
				}else{
					locations.stn <- locations.stn[!duplicated(locations.stn[, c('lon', 'lat')]), ]
					coordinates(locations.stn) <- ~lon+lat
					if(any(is.auxvar)){
						locations.stn <- locations.stn[Reduce("&", as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))), ]
						block <- NULL
					}else  block <- bGrd

					pars.grd <- krige(formule, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
										block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				}
				ret <- matrix(pars.grd$var1.pred, ncol = nlat0, nrow = nlon0)
				if(j == 2) ret[ret < 0] <- NA
				ret
			})
			names(pars.mon) <- c('mean', 'sd')
			pars.mon
		}

		PARS.down <- vector(mode = 'list', length = 12)
		PARS.down[months] <- foreach(m = months, .packages = packages, .export = toExports) %parLoop% {
			pars.mon <- lapply(1:2, function(j){
				locations.down <- interp.grid$coords.grd
				locations.down$pars <- bias.pars$pars.down[[m]][, j]
				locations.down$pars[!bias.pars$pars.sw.down[[m]]] <- NA

				locations.downstn <- interp.grid$coords.stn
				locations.downstn$pars <- bias.pars$pars.downstn[[m]][, j]
				locations.downstn$pars[!bias.pars$pars.sw.stn[[m]]] <- NA

				locations.down <- rbind(locations.down, locations.downstn)
				locations.down <- locations.down[!is.na(locations.down$pars), ]
				if(length(locations.down$pars) < min.stn) return(NULL)

				extrm <- quantile(locations.down$pars, probs = c(0.001, 0.99))
				locations.down <- locations.down[locations.down$pars > extrm[1] & locations.down$pars < extrm[2], ]
				locations.down <- remove.duplicates(locations.down)

				if(any(is.auxvar)) locations.down <- locations.down[Reduce("&", as.data.frame(!is.na(locations.down[, auxvar[is.auxvar]]))), ]
				if(interp.method == 'Kriging'){
					vgm <- try(autofitVariogram(formule, input_data = locations.down, model = vgm.model, cressie = TRUE), silent = TRUE)
					vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
				}else vgm <- NULL

				if(interp.method == 'NN'){
					pars.grd <- krige(pars~1, locations = locations.down, newdata = interp.grid$newgrid,
										nmax = 1, maxdist = maxdist, debug.level = 0)
				}else{
					block <- if(any(is.auxvar)) NULL else bGrd
					pars.grd <- krige(formule, locations = locations.down, newdata = interp.grid$newgrid, model = vgm,
											block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				}
				ret <- matrix(pars.grd$var1.pred, ncol = nlat0, nrow = nlon0)
				if(j == 2) ret[ret < 0] <- NA
				ret
			})
			names(pars.mon) <- c('mean', 'sd')
			pars.mon
		}
		if(closeklust) stopCluster(klust)

		################

		default <- apply(do.call('rbind', bias.pars$pars.down), 2, median, na.rm = TRUE)
		PARS <- vector(mode = 'list', length = 12)
		PARS[months] <- lapply(months, function(m){
			par.stn <- PARS.stn[[m]]
			par.down <- PARS.down[[m]]
			xmean <- xsd <- matrix(NA, ncol = nlat0, nrow = nlon0)
			if(!is.null(par.stn) & !is.null(par.down)){
				if(!is.null(par.stn$mean) & !is.null(par.stn$sd) &
					!is.null(par.down$mean) & !is.null(par.down$sd)){
					ina <- is.na(par.stn$mean) | is.na(par.stn$sd) |
							is.na(par.down$mean) | is.na(par.down$sd)
					mmean <- median(par.down$mean, na.rm = TRUE)
					msd <-  median(par.down$sd, na.rm = TRUE)
					par.stn$mean[ina] <- mmean
					par.stn$sd[ina] <- msd
					par.down$mean[ina] <- mmean
					par.down$sd[ina] <- msd
					ret.stn <- list(mean = par.stn$mean, sd = par.stn$sd)
					ret.down <- list(mean = par.down$mean, sd = par.down$sd)
				}else{
					xmean[] <- default['mean']
					xsd[] <- default['sd']
					ret.stn <- ret.down <- list(mean = xmean, sd = xsd)
				}
			}else{
				xmean[] <- default['mean']
				xsd[] <- default['sd']
				ret.stn <- ret.down <- list(mean = xmean, sd = xsd)
			}
			list(stn = ret.stn, down = ret.down)
		})

		################
		#Defines netcdf output
		grd.mean <- ncvar_def("mean", "degC", xy.dim, NA, longname= "Means normal distribution", prec = "float")
		grd.sd <- ncvar_def("sd", "degC", xy.dim, NA, longname= "Standard deviations normal distribution", prec = "float")

		for(jfl in months){
			outnc1 <- file.path(origdir, paste('Gaussian_Pars.STN', '_', jfl, '.nc', sep = ''))
			nc1 <- nc_create(outnc1, list(grd.mean, grd.sd))
			ncvar_put(nc1, grd.mean, PARS[[jfl]]$stn$mean)
			ncvar_put(nc1, grd.sd, PARS[[jfl]]$stn$sd)
			nc_close(nc1)
		}
			
		for(jfl in months){
			outnc2 <- file.path(origdir, paste('Gaussian_Pars.REANAL', '_', jfl, '.nc', sep = ''))
			nc2 <- nc_create(outnc2, list(grd.mean, grd.sd))
			ncvar_put(nc2, grd.mean, PARS[[jfl]]$down$mean)
			ncvar_put(nc2, grd.sd, PARS[[jfl]]$down$sd)
			nc_close(nc2)
		}
		rm(PARS.stn, PARS.down, PARS)
	}

	rm(stnData, demData, demGrid, ObjStn, interp.grid)
	gc()
	InsertMessagesTxt(main.txt.out, 'Interpolating bias factors finished')
	tcl("update")
	return(0)
}

#################################################################################################

##correct downscaled reanalysis bias
AjdMeanBiasTemp <- function(adjMeanBiasparms){

	GeneralParameters <- adjMeanBiasparms$GeneralParameters
	origdir <- adjMeanBiasparms$origdir

	freqData <- GeneralParameters$period
	bias.method <- GeneralParameters$Bias.Method
	biasDir <- GeneralParameters$IO.files$Bias.dir
	meanBiasPrefix <- GeneralParameters$Format$Mean.Bias.Prefix
	adjRreanalFF <- GeneralParameters$Format$Adj.File.Format
	months <- sort(as.numeric(GeneralParameters$Adjust.Months))

	memType <- adjMeanBiasparms$memType

	###############
	if(memType == 2){
		downData <- read.NetCDF.Data(adjMeanBiasparms$downData)
		if(is.null(downData)) return(NULL)

		irnl <- !sapply(downData$data, is.null)
		if(!any(irnl)){
			InsertMessagesTxt(main.txt.out, "Downscaled reanalysis data not found", format = TRUE)
			return(NULL)
		}

		downData$dates <- downData$dates[irnl]
		downData$data <- downData$data[irnl]
	}else{
		ncfiles <- adjMeanBiasparms$downData$ncfiles
		ncInfo <- ncFilesInfo(ncfiles$freqData, ncfiles$start.date, ncfiles$end.date, ncfiles$months,
							ncfiles$ncDir, ncfiles$ncFileFormat, adjMeanBiasparms$downData$errmsg)
		if(is.null(ncInfo)) return(NULL)

		ncInfo$nc.files <- ncInfo$nc.files[ncInfo$exist]
		ncInfo$dates <- ncInfo$dates[ncInfo$exist]

		nc <- nc_open(ncInfo$nc.files[1])
		rlon <- nc$dim[[adjMeanBiasparms$downData$ncinfo$xo]]$vals
		rlat <- nc$dim[[adjMeanBiasparms$downData$ncinfo$yo]]$vals
		nc_close(nc)
		xo <- order(rlon)
		rlon <- rlon[xo]
		yo <- order(rlat)
		rlat <- rlat[yo]

		downData <- list(lon = rlon, lat = rlat, dates = ncInfo$dates, files = ncInfo$nc.files,
						xo = xo, yo = yo, varid = adjMeanBiasparms$downData$ncinfo$varid,
						yorder = adjMeanBiasparms$downData$ncinfo$yo)
	}

	###############
	InsertMessagesTxt(main.txt.out, 'Correct Reanalysis Bias ...')
	tcl("update")

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
		if(memType == 2){
			xtmp <- downData$data[[jfl]]
			dtmp <- downData$dates[jfl]
		}else{
			nc <- nc_open(downData$files[jfl])
			xtmp <- ncvar_get(nc, varid = downData$varid)
			nc_close(nc)
			xtmp <- xtmp[downData$xo, downData$yo]
			if(downData$yorder == 1){
				xtmp <- matrix(c(xtmp), nrow = length(downData$lon), ncol = length(downData$lat), byrow = TRUE)
			}
			dtmp <- downData$dates[jfl]
		}

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

	#############
	auxvar <- c('dem', 'slp', 'asp')
	is.auxvar <- c(GeneralParameters$auxvar$dem, GeneralParameters$auxvar$slope, GeneralParameters$auxvar$aspect)
	if(any(is.auxvar)){
		formule <- formula(paste('pars', '~', paste(auxvar[is.auxvar], collapse = '+'), sep = ''))
	}else formule <- formula(paste('pars', '~', 1, sep = ''))

	#############
	stnData <- comptLMparams$stnData
	id.stn <- stnData$id
	lon.stn <- stnData$lon
	lat.stn <- stnData$lat
	date.stn <- stnData$dates
	data.stn <- stnData$data

	#############
	if(comptLMparams$memType == 2){
		# read then extract
		adjData <- read.NetCDF.Data(comptLMparams$adjData)
		if(is.null(adjData)) return(NULL)
		ijtmp <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = adjData$lon, lat = adjData$lat))
		data.adj.stn  <- t(sapply(adjData$data, function(x){
			if(!is.null(x)) x[ijtmp]
			else rep(NA, length(ijtmp))
		}))
	}else{
		# read and extract
		adjData <- read.NetCDF.Data2Points(comptLMparams$adjData, list(lon = lon.stn, lat = lat.stn))
		if(is.null(adjData)) return(NULL)
		data.adj.stn <- t(sapply(adjData$data, function(x) if(!is.null(x)) x else rep(NA, length(lon.stn))))
	}
	date.adj <- adjData$dates

	#############
	xy.grid <- list(lon = adjData$lon, lat = adjData$lat)

	# res.coarse <- as.numeric(GeneralParameters$Interpolation.pars$res.coarse)
	res.coarse <- if(interp.method == 'NN') sqrt((rad.lon*mean(xy.grid$lon[-1]-xy.grid$lon[-nlon0]))^2 + (rad.lat*mean(xy.grid$lat[-1]-xy.grid$lat[-nlat0]))^2)/2 else maxdist/2
	res.coarse <- if(res.coarse  >= 0.25) res.coarse else 0.25

	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- length(xy.grid$lon)
	nlat0 <- length(xy.grid$lat)

	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	xy.dim <- list(dx, dy)

	#############
	demData <- comptLMparams$demData
	lon.dem <- demData$lon
	lat.dem <- demData$lat
	grd.dem <- demData$demMat
	grd.dem[grd.dem < 0] <- 0
	demSp <- defSpatialPixels(list(lon = lon.dem, lat = lat.dem))
	is.regridDEM <- is.diffSpatialPixelsObj(grdSp, demSp, tol = 1e-07)
	
	demGrid <- list(x = lon.dem, y = lat.dem, z = grd.dem)
	if(is.regridDEM){
		demGrid <- interp.surface.grid(demGrid, list(x = xy.grid$lon, y = xy.grid$lat))
	}

	demres <- grdSp@grid@cellsize
	slpasp <- slope.aspect(demGrid$z, demres[1], demres[2], filter = "sobel")
	demGrid$slp <- slpasp$slope
	demGrid$asp <- slpasp$aspect

	ijdem <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), xy.grid)
	ObjStn <- list(x = lon.stn, y = lat.stn, z = demGrid$z[ijdem], slp = demGrid$slp[ijdem], asp = demGrid$asp[ijdem])

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

	#############
	InsertMessagesTxt(main.txt.out, 'Compute LM Coefficients ...')
	tcl("update")

	dtadj <- date.adj%in%date.stn
	dtstn <- date.stn%in%date.adj
	data.stn <- data.stn[dtstn, , drop = FALSE]
	date.stn <- date.stn[dtstn]
	data.adj.stn <- data.adj.stn[dtadj, , drop = FALSE]
	# date.adj <- date.adj[dtadj]

	month.stn <- as(substr(date.stn, 5, 6), 'numeric')
	year.stn <- as(substr(date.stn, 1, 4), 'numeric')
	iyear0 <- (year.stn >= year1 & year.stn <= year2) & (month.stn%in%months)

	data.stn.reg <- data.stn[iyear0, ]
	mon.stn.reg <- month.stn[iyear0]
	dataf <- data.frame(id.stn = rep(id.stn, each = nrow(data.stn.reg)), month = rep(mon.stn.reg, ncol(data.stn.reg)),
						stn = c(data.stn.reg), tt = c(data.adj.stn))

	##############
	model <- by(dataf, dataf$id.stn, fitLM.month.TT, min.len)

	xmod <- lapply(model, function(x){
		sapply(x, function(m){
			if(is.null(m)) c(NA, NA, NA, NA, NA)
			else{
				smod <- summary(m)
				xcoef <- coefficients(m)
				if(is.null(smod$fstatistic)) c(xcoef, NA, NA, NA)
				else c(xcoef, pf(smod$fstatistic[1], smod$fstatistic[2], smod$fstatistic[3], lower.tail = FALSE), smod$r.squared, smod$adj.r.squared)
			}
		})
	})

	model.coef <- list(slope = sapply(xmod, function(x) x[2, ]),
					   intercept = sapply(xmod, function(x) x[1, ]),
					   pvalue = sapply(xmod, function(x) x[3, ]),
					   rsquared = sapply(xmod, function(x) x[4, ]),
					   adj.rsquared = sapply(xmod, function(x) x[5, ]))
	nommodcoef <- names(model.coef)

	islp <- !is.na(model.coef$slope) & model.coef$slope > 0
	model.coef$slope[!islp] <- NA
	extrm <- t(apply(model.coef$slope, 1, quantile, prob = c(0.001, 0.999), na.rm = TRUE))
	islp <- !is.na(model.coef$slope) & model.coef$slope > extrm[, 1] & model.coef$slope < extrm[, 2]
	intrcp <- !is.na(model.coef$intercept)
	ipval <- !is.na(model.coef$pvalue) & !is.nan(model.coef$pvalue) & model.coef$pvalue < 0.05
	irsq <- !is.na(model.coef$adj.rsquared) & model.coef$adj.rsquared > 0.2

	model.coef <- lapply(model.coef, function(x){
		x[!(islp & intrcp & ipval & irsq)] <- NA
		x
	})
	names(model.coef) <- nommodcoef

	##########
	model.params <- list(model = model, coef = model.coef)
	save(model.params, file = file.path(origdir, "LM_MODEL_PARS.RData"))
	##########

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
	toExports <- c('model.coef', 'months', 'interp.grid', 'interp.method', 'min.stn','formule',
				'auxvar', 'is.auxvar', 'vgm.model', 'nmin', 'nmax', 'maxdist', 'bGrd', 'nlat0', 'nlon0')
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

			if(any(is.auxvar)) locations.stn <- locations.stn[Reduce("&", as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))), ]
			if(interp.method == 'Kriging'){
				vgm <- try(autofitVariogram(formule, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
				vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
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
				if(any(is.auxvar)){
					locations.stn <- locations.stn[Reduce("&", as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))), ]
					block <- NULL
				}else block <- bGrd
				pars.grd <- krige(formule, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
									block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)

				extrm <- c(min(locations.stn$pars, na.rm = TRUE), max(locations.stn$pars, na.rm = TRUE))
				ixtrm <- is.na(pars.grd$var1.pred) | (pars.grd$var1.pred <= extrm[1] | pars.grd$var1.pred >= extrm[2])
				pars.grd$var1.pred[ixtrm] <- NA

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
		data.stn, demGrid, interp.grid, data.stn.reg,
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
	auxvar <- c('dem', 'slp', 'asp')
	is.auxvar <- c(GeneralParameters$auxvar$dem, GeneralParameters$auxvar$slope, GeneralParameters$auxvar$aspect)
	if(any(is.auxvar)){
		formule <- formula(paste('res', '~', paste(auxvar[is.auxvar], collapse = '+'), sep = ''))
	}else formule <- formula(paste('res', '~', 1, sep = ''))


	#############
	demData <- paramsMRG$demData
	if(!is.null(demData)){
		demData$z[demData$z < 0] <- 0
		demres <- grdSp@grid@cellsize
		slpasp <- slope.aspect(demData$z, demres[1], demres[2], filter = "sobel")
		demData$slp <- slpasp$slope
		demData$asp <- slpasp$aspect
		ijdem <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), xy.grid)
		dem.stn.val <- demData$z[ijdem]
		dem.stn.slp <- demData$slp[ijdem]
		dem.stn.asp <- demData$asp[ijdem]
	}else{
		dem.grd.val <- matrix(1, nrow = nlon0, ncol = nlat0)
		dem.grd.slp <- matrix(0, nrow = nlon0, ncol = nlat0)
		dem.grd.asp <- matrix(0, nrow = nlon0, ncol = nlat0)
		demData <- list(x = xy.grid$lon, y = xy.grid$lat, z = dem.grd.val, slp = dem.grd.slp, asp = dem.grd.asp)
		dem.stn.val <- rep(1, length(lon.stn))
		dem.stn.slp <- rep(0, length(lon.stn))
		dem.stn.asp <- rep(0, length(lon.stn))
	}
	ObjStn <- list(x = lon.stn, y = lat.stn, z = dem.stn.val, slp = dem.stn.slp, asp = dem.stn.asp)

	## create grid to interp
	interp.grid <- createGrid(ObjStn, demData, as.dim.elv = FALSE, res.coarse = res.coarse)
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
			if(any(is.auxvar)) locations.stn <- locations.stn[Reduce("&", as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))), ]
			if(interp.method == 'Kriging'){
				vgm <- try(autofitVariogram(formule, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
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
			if(any(is.auxvar)){
				locations.stn <- locations.stn[Reduce("&", as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))), ]
				block <- NULL
			}else block <- bGrd
			res.grd <- krige(formule, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
								block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)

			extrm <- c(min(locations.stn$res, na.rm = TRUE), max(locations.stn$res, na.rm = TRUE))
			ixtrm <- is.na(res.grd$var1.pred) | (res.grd$var1.pred <= extrm[1] | res.grd$var1.pred >= extrm[2])
			res.grd$var1.pred[ixtrm] <- NA

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



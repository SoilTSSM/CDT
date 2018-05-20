
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
	nstn <- length(lon.stn)

	###############
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

	###############
	date.bias <- downData$dates
	ibsdt <- date.bias%in%date.stn
	istdt <- date.stn%in%date.bias
	if(!any(ibsdt)) return(NULL)

	###############
	InsertMessagesTxt(main.txt.out, 'Compute bias factors ...')

	if(bias.method != 'Quantile.Mapping'){
		date.bias <- date.bias[ibsdt]
		data.down.stn <- data.down.stn[ibsdt, , drop = FALSE]
		data.stn <- data.stn[istdt, , drop = FALSE]

		if(bias.method == 'Multiplicative.Bias.Mon'){
			times.stn <- as.numeric(substr(date.bias, 5, 6))
		}
		if(bias.method == 'Multiplicative.Bias.Var'){
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
		}

		index <- split(seq(length(times.stn)), times.stn)
		bias <- lapply(index, function(x){
			stn <- data.stn[x, , drop = FALSE]
			tt <- data.down.stn[x, , drop = FALSE]
			na.data <- is.na(stn) | is.na(tt)
			stn[na.data] <- NA
			tt[na.data] <- NA
			len <- colSums(!na.data)
			stn <- colSums(stn, na.rm = TRUE)
			tt <- colSums(tt, na.rm = TRUE)
			bs <- stn/tt
			bs[len < min.len] <- 1
			bs[is.na(bs)] <- 1
			bs[is.infinite(bs)] <- 1.5
			bs[is.nan(bs)] <- 1
			bs[bs < 0] <- 1
			bs[bs == 0] <- 0.6
			bs[bs > 1.5] <- 1.5
			bs
		})
		bias <- do.call(rbind, bias)

		bias.pars <- list(bias = bias, lon.stn = lon.stn, lat.stn = lat.stn, id.stn = id.stn,
							data.stn = data.stn, data.down = data.down.stn, date = date.bias)

		rm(data.down.stn)
	}else{
		is.parallel <- doparallel(length(months) >= 3)
		`%parLoop%` <- is.parallel$dofun

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
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

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
		bias.pars <- list(bias = bias, fit.stn = pars.Obs.Stn, fit.downstn = pars.Obs.down, fit.down = pars.Crs.down,
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
	rm(stnData, downData, data.stn, bias.pars)
	gc()
	return(bias)
}

#################################################################################################

InterpolateMeanBiasTemp <- function(interpBiasparams){
	InsertMessagesTxt(main.txt.out, 'Interpolate bias factors ...')

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
	auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
	is.auxvar <- c(GeneralParameters$auxvar$dem, GeneralParameters$auxvar$slope,
					GeneralParameters$auxvar$aspect, GeneralParameters$auxvar$lon,
					GeneralParameters$auxvar$lat)
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

	#############
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
	if(is.regridDEM)
		demGrid <- cdt.interp.surface.grid(list(lon = lon.dem, lat = lat.dem, z = grd.dem), xy.grid)
	else demGrid <- list(x = lon.dem, y = lat.dem, z = grd.dem)

	# demres <- grdSp@grid@cellsize
	# slpasp <- slope.aspect(demGrid$z, demres[1], demres[2], filter = "sobel")
	slpasp <- raster.slope.aspect(demGrid)
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
	interp.grid$coords.stn$alon <- interp.grid$coords.stn@coords[, 'lon']
	interp.grid$coords.stn$alat <- interp.grid$coords.stn@coords[, 'lat']
	interp.grid$coords.grd$alon <- interp.grid$coords.grd@coords[, 'lon']
	interp.grid$coords.grd$alat <- interp.grid$coords.grd@coords[, 'lat']
	if(!is.null(interp.grid$coords.rfe)){
		interp.grid$coords.rfe$alon <- interp.grid$coords.rfe@coords[, 'lon']
		interp.grid$coords.rfe$alat <- interp.grid$coords.rfe@coords[, 'lat']
	}
	interp.grid$newgrid$alon <- interp.grid$newgrid@coords[, 'lon']
	interp.grid$newgrid$alat <- interp.grid$newgrid@coords[, 'lat']

	########################################
	## interpolation
	if(bias.method != 'Quantile.Mapping'){
		itimes <- as.numeric(rownames(bias.pars))
		is.parallel <- doparallel(length(itimes) >= 3)
		`%parLoop%` <- is.parallel$dofun

		packages <- c('sp', 'gstat', 'automap', 'ncdf4')
		toExports <- c('bias.pars', 'itimes', 'interp.grid', 'interp.method', 'formule', 'auxvar',
						'is.auxvar', 'min.stn', 'vgm.model', 'nmin', 'nmax', 'maxdist',
						'bGrd', 'origdir', 'meanBiasPrefix')
		grd.bs <- ncvar_def("bias", "", xy.dim, NA, longname= "Multiplicative Mean Bias Factor", prec = "float", compression = 9)

		ret <- foreach(m = itimes, .packages = packages, .export = toExports) %parLoop% {
			locations.stn <- interp.grid$coords.stn
			locations.stn$pars <- bias.pars[itimes == m, ]
			locations.stn <- locations.stn[!is.na(locations.stn$pars), ]

			extrm <- quantile(locations.stn$pars, probs = c(0.001, 0.999))
			locations.stn <- locations.stn[locations.stn$pars > extrm[1] & locations.stn$pars < extrm[2], ]

			if(any(is.auxvar) & interp.method != 'NN'){
				locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
				locations.stn <- locations.stn[Reduce("&", locations.df), ]
			}

			if(length(locations.stn$pars) >= min.stn & any(locations.stn$pars != 1)){
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
						locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
						locations.stn <- locations.stn[Reduce("&", locations.df), ]
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
			}else grdbias <- matrix(1, ncol = nlat0, nrow = nlon0)

			#######
			outnc <- file.path(origdir, paste(meanBiasPrefix, '_', m, '.nc', sep = ''))
			nc2 <- nc_create(outnc, grd.bs)
			ncvar_put(nc2, grd.bs, grdbias)
			nc_close(nc2)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
	}else{
		is.parallel <- doparallel(length(months) >= 3)
		`%parLoop%` <- is.parallel$dofun

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

				if(any(is.auxvar) & interp.method != 'NN'){
					locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
					locations.stn <- locations.stn[Reduce("&", locations.df), ]
				}
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
						locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
						locations.stn <- locations.stn[Reduce("&", locations.df), ]
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

				if(any(is.auxvar) & interp.method != 'NN'){
					locations.df <- as.data.frame(!is.na(locations.down@data[, auxvar[is.auxvar]]))
					locations.down <- locations.down[Reduce("&", locations.df), ]
				}
				if(length(locations.down$pars) < min.stn) return(NULL)

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
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

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
		grd.mean <- ncvar_def("mean", "degC", xy.dim, NA, longname= "Means normal distribution", prec = "float", compression = 9)
		grd.sd <- ncvar_def("sd", "degC", xy.dim, NA, longname= "Standard deviations normal distribution", prec = "float", compression = 9)

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
	return(0)
}

#################################################################################################

##correct downscaled reanalysis bias
AjdMeanBiasTemp <- function(adjMeanBiasparms){
	InsertMessagesTxt(main.txt.out, 'Reanalysis Bias correction...')

	GeneralParameters <- adjMeanBiasparms$GeneralParameters
	origdir <- adjMeanBiasparms$origdir

	freqData <- GeneralParameters$period
	bias.method <- GeneralParameters$Bias.Method
	biasDir <- GeneralParameters$IO.files$Bias.dir
	meanBiasPrefix <- GeneralParameters$Format$Mean.Bias.Prefix
	adjRreanalFF <- GeneralParameters$Format$Adj.File.Format
	months <- sort(as.numeric(GeneralParameters$Adjust.Months))


	###############
	ncinfo <- adjMeanBiasparms$downData
	ncfiles <- ncinfo$ncfiles
	ncInfo <- ncFilesInfo(ncfiles$freqData, ncfiles$start.date, ncfiles$end.date, ncfiles$months,
						ncfiles$ncDir, ncfiles$ncFileFormat, ncinfo$errmsg)
	if(is.null(ncInfo)) return(NULL)

	ncInfo$nc.files <- ncInfo$nc.files[ncInfo$exist]
	ncInfo$dates <- ncInfo$dates[ncInfo$exist]

	nc <- nc_open(ncInfo$nc.files[1])
	# rlon <- nc$dim[[adjMeanBiasparms$downData$ncinfo$xo]]$vals
	# rlat <- nc$dim[[adjMeanBiasparms$downData$ncinfo$yo]]$vals

	rlon <- nc$var[[ncinfo$varid]]$dim[[ncinfo$xo]]$vals
	rlat <- nc$var[[ncinfo$varid]]$dim[[ncinfo$yo]]$vals
	nc_close(nc)
	xo <- order(rlon)
	rlon <- rlon[xo]
	yo <- order(rlat)
	rlat <- rlat[yo]

	downData <- list(lon = rlon, lat = rlat, dates = ncInfo$dates, files = ncInfo$nc.files,
					xo = xo, yo = yo, varid = ncinfo$varid, yorder = ncinfo$yo)

	###############
	InsertMessagesTxt(main.txt.out, 'Read bias data ...')

	if(bias.method == "Multiplicative.Bias.Mon"){
		biasFile <- file.path(biasDir, paste(meanBiasPrefix, '_', months, '.nc', sep = ''))
		exist.bias <- unlist(lapply(biasFile, file.exists))
		if(any(!exist.bias)){
			miss.bias <- months[!exist.bias]
			for(j in seq_along(miss.bias)){
				msg <- paste(meanBiasPrefix, '_', miss.bias[j], '.nc', sep = '')
				InsertMessagesTxt(main.txt.out, paste(msg, "doesn't exist"), format = TRUE)
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
			}
			return(NULL)
		}
		nc <- nc_open(biasFile[which(exist.bias)[1]])
		lon <- nc$dim[[1]]$vals
		lat <- nc$dim[[2]]$vals
		nc_close(nc)
		BIAS[times.stn] <- lapply(seq_along(times.stn), function(m){
			nc <- nc_open(biasFile[m])
			xvar <- ncvar_get(nc, varid = "bias")
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

	###############

	InsertMessagesTxt(main.txt.out, 'Reading bias data finished')
	InsertMessagesTxt(main.txt.out, 'Correct Reanalysis Bias ...')

	###############
	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", lon)
	dy <- ncdim_def("Lat", "degreeN", lat)
	grd.bsadj <- ncvar_def("temp", "DegC", list(dx, dy), -99, longname= "Bias Corrected Reanalysis", prec = "float", compression = 9)

	###############

	is.parallel <- doparallel(length(downData$dates) >= 30)
	`%parLoop%` <- is.parallel$dofun

	packages <- c('ncdf4')
	toExports <- c(toExports, 'downData', 'bias.method', 'origdir', 'quantile.mapping.Gau',
					 'grd.bsadj', 'freqData', 'adjRreanalFF', 'is.leapyear')

	ret <- foreach(jfl = seq_along(downData$dates), .packages = packages, .export = toExports) %parLoop% {
		nc <- nc_open(downData$files[jfl])
		xtmp <- ncvar_get(nc, varid = downData$varid)
		nc_close(nc)
		xtmp <- if(downData$yorder == 1) t(xtmp)[downData$xo, downData$yo] else xtmp[downData$xo, downData$yo]
		dtmp <- downData$dates[jfl]

		if(bias.method == "Multiplicative.Bias.Var"){
			if(freqData == 'daily'){
				ann <- as.numeric(substr(dtmp, 1, 4))
				iday <- as.numeric(strftime(as.Date(dtmp, format = '%Y%m%d'), format = '%j'))
				ijt <- ifelse(is.leapyear(ann) & iday > 59, iday-1, iday)
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
		year <- substr(dtmp, 1, 4)
		month <- substr(dtmp, 5, 6)

		if(freqData == 'daily') adjfrmt <- sprintf(adjRreanalFF, year, month, substr(dtmp, 7, 8))
		else if(freqData == 'dekadal') adjfrmt <- sprintf(adjRreanalFF, year, month, substr(dtmp, 7, 7))
		else adjfrmt <- sprintf(adjRreanalFF, year, month)

		outfl <- file.path(origdir, adjfrmt)
		nc2 <- nc_create(outfl, grd.bsadj)
		ncvar_put(nc2, grd.bsadj, xadj)
		nc_close(nc2)
		return(0)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	rm(downData)
	gc()
	InsertMessagesTxt(main.txt.out, 'Bias Correction finished')
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
	auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
	is.auxvar <- c(GeneralParameters$auxvar$dem, GeneralParameters$auxvar$slope,
					GeneralParameters$auxvar$aspect, GeneralParameters$auxvar$lon,
					GeneralParameters$auxvar$lat)
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
	# read and extract
	adjData <- read.NetCDF.Data2Points(comptLMparams$adjData, list(lon = lon.stn, lat = lat.stn))
	if(is.null(adjData)) return(NULL)
	data.adj.stn <- t(sapply(adjData$data, function(x) if(!is.null(x)) x else rep(NA, length(lon.stn))))
	date.adj <- adjData$dates

	#############
	InsertMessagesTxt(main.txt.out, 'Compute LM Coefficients ...')

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
	nstn <- ncol(data.stn.reg)

	#############
	index <- split(seq(length(mon.stn.reg)), mon.stn.reg)
	model.coef <- lapply(index, function(x){
		Y <- data.stn.reg[x, , drop = FALSE]
		X <- data.adj.stn[x, , drop = FALSE]
		ina <- is.na(X) | is.na(Y)
		X[ina] <- NA
		Y[ina] <- NA
		nbY <- base::colSums(!is.na(Y))
		ix <- nbY >= min.len
		if(!any(ix)) return(NULL)
		nbY <- nbY[ix]
		Y <- Y[, ix, drop = FALSE]
		X <- X[, ix, drop = FALSE]
		ncolY <- ncol(Y)
		nrowY <- nrow(Y)

		mX <- base::colMeans(X, na.rm = TRUE)
		mY <- base::colMeans(Y, na.rm = TRUE)
		vX <- matrixStats::colVars(X, na.rm = TRUE)
		vY <- matrixStats::colVars(Y, na.rm = TRUE)

		X1 <- X - matrix(mX, nrowY, ncolY, byrow = TRUE)
		Y1 <- Y - matrix(mY, nrowY, ncolY, byrow = TRUE)
		COV <- base::colSums(X1 * Y1, na.rm = TRUE) / (nbY - 1)
		alpha <- COV / vX
		beta <- mY - alpha * mX

		hatY <- matrix(alpha, nrowY, ncolY, byrow = TRUE) * X + matrix(beta, nrowY, ncolY, byrow = TRUE)
		SSE <- base::colSums((hatY - Y)^2, na.rm = TRUE)
		MSE <- SSE/(nbY-2)
		sigma <- sqrt(MSE)
		std.alpha <- sigma / (sqrt(nbY-1)*sqrt(vX))
		# std.beta <- sigma * sqrt((1/nbY) + (mX^2/((nbY-1)*vX)))
		SXX <- (nbY-1)*vX
		tvalue.alpha <- alpha / sqrt(MSE/SXX)
		# tvalue.beta <- beta / sqrt(MSE * ((1/nbY) + (mX^2/SXX)))
		pvalue.alpha <- 2 * pt(-abs(tvalue.alpha), nbY-2)
		# pvalue.beta <- 2 * pt(-abs(tvalue.beta), nbY-2)
		R2 <- COV^2 / (vX * vY)

		out <- matrix(NA, 4, length(ix))
		out[, ix] <- rbind(alpha, beta, pvalue.alpha, R2)
		return(out)
	})

	model.coef <- list(slope = do.call(rbind, lapply(model.coef, function(x) if(is.null(x)) rep(NA, nstn) else x[1, ])),
					   intercept = do.call(rbind, lapply(model.coef, function(x) if(is.null(x)) rep(NA, nstn) else x[2, ])),
					   pvalue = do.call(rbind, lapply(model.coef, function(x) if(is.null(x)) rep(NA, nstn) else x[3, ])),
					   rsquared = do.call(rbind, lapply(model.coef, function(x)if(is.null(x)) rep(NA, nstn) else x[4, ])))
	nommodcoef <- names(model.coef)
	coef0 <- model.coef

	islp <- !is.na(model.coef$slope) & model.coef$slope > 0
	model.coef$slope[!islp] <- NA
	extrm <- t(apply(model.coef$slope, 1, quantile, prob = c(0.001, 0.999), na.rm = TRUE))
	islp <- !is.na(model.coef$slope) & model.coef$slope > extrm[, 1] & model.coef$slope < extrm[, 2]
	intrcp <- !is.na(model.coef$intercept)
	ipval <- !is.na(model.coef$pvalue) & !is.nan(model.coef$pvalue) & model.coef$pvalue < 0.05
	irsq <- !is.na(model.coef$rsquared) & model.coef$rsquared > 0.2

	model.coef <- lapply(model.coef, function(x){
		x[!(islp & intrcp & ipval & irsq)] <- NA
		x
	})
	names(model.coef) <- nommodcoef

	##########
	model.params <- list(coef0 = coef0, coef = model.coef, id.stn = id.stn,
						lon.stn = lon.stn, lat.stn = lat.stn, date.stn = date.stn[iyear0],
						data.stn = data.stn.reg, data.tt = data.adj.stn)
	save(model.params, file = file.path(origdir, "LM_MODEL_PARS.RData"))

	##########

	InsertMessagesTxt(main.txt.out, 'Computing LM Coefficients finished')
	InsertMessagesTxt(main.txt.out, 'Interpolate LM Coefficients ...')

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
		demGrid <- demGrid[c('x', 'y', 'z')]
	}

	# demres <- grdSp@grid@cellsize
	# slpasp <- slope.aspect(demGrid$z, demres[1], demres[2], filter = "sobel")
	slpasp <- raster.slope.aspect(demGrid)
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
		maxdist <- sqrt(xy.maxdist^2 + z.maxdist^2)
	}else{
		interp.grid <- createGrid(ObjStn, demGrid, as.dim.elv = FALSE, res.coarse = res.coarse)
		maxdist <- if(maxdist < res.coarse) res.coarse else maxdist
		cells <- SpatialPixels(points = interp.grid$newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)))@grid
		bGrd <- createBlock(cells@cellsize, 2, 5)
	}
	interp.grid$coords.stn$alon <- interp.grid$coords.stn@coords[, 'lon']
	interp.grid$coords.stn$alat <- interp.grid$coords.stn@coords[, 'lat']
	interp.grid$coords.grd$alon <- interp.grid$coords.grd@coords[, 'lon']
	interp.grid$coords.grd$alat <- interp.grid$coords.grd@coords[, 'lat']
	if(!is.null(interp.grid$coords.rfe)){
		interp.grid$coords.rfe$alon <- interp.grid$coords.rfe@coords[, 'lon']
		interp.grid$coords.rfe$alat <- interp.grid$coords.rfe@coords[, 'lat']
	}
	interp.grid$newgrid$alon <- interp.grid$newgrid@coords[, 'lon']
	interp.grid$newgrid$alat <- interp.grid$newgrid@coords[, 'lat']

	#############
	is.parallel <- doparallel(length(months) >= 3)
	`%parLoop%` <- is.parallel$dofun

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

			if(any(is.auxvar) & interp.method != 'NN'){
				locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
				locations.stn <- locations.stn[Reduce("&", locations.df), ]
			}
			if(length(locations.stn$pars) < min.stn) return(NULL)

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
				pars.grd <- krige(pars~1, locations = locations.stn, newdata = interp.grid$newgrid,
									nmax = 1, maxdist = maxdist, debug.level = 0)
			}else{
				coordinates(locations.stn) <- ~lon+lat
				if(any(is.auxvar)){
					locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
					locations.stn <- locations.stn[Reduce("&", locations.df), ]
					block <- NULL
				}else block <- bGrd
				pars.grd <- krige(formule, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
									block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)

				extrm1 <- min(locations.stn$pars, na.rm = TRUE)
				pars.grd$var1.pred[pars.grd$var1.pred <= extrm1] <- extrm1
				extrm2  <- max(locations.stn$pars, na.rm = TRUE)
				pars.grd$var1.pred[pars.grd$var1.pred >= extrm2] <- extrm2
			}
			matrix(pars.grd$var1.pred, ncol = nlat0, nrow = nlon0)
		})
		names(pars.mon) <- c('slope', 'intercept')
		pars.mon
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	###########
	grd.slope <- ncvar_def("slope", "", xy.dim, NA, longname= "Linear model Coef: Slope", prec = "float")
	grd.intercept <- ncvar_def("intercept", "", xy.dim, NA, longname= "Linear model Coef: Intercept", prec = "float")

	for(jfl in months){
		if(is.null(MODEL.COEF[[jfl]]$slope) | is.null(MODEL.COEF[[jfl]]$intercept)){
			MODEL.COEF[[jfl]]$slope <- matrix(1, ncol = nlat0, nrow = nlon0)
			MODEL.COEF[[jfl]]$intercept <- matrix(0, ncol = nlat0, nrow = nlon0)
		}
		ina <- is.na(MODEL.COEF[[jfl]]$slope) | is.na(MODEL.COEF[[jfl]]$intercept)
		MODEL.COEF[[jfl]]$slope[ina] <- 1
		MODEL.COEF[[jfl]]$intercept[ina] <- 0

		outnc1 <- file.path(origdir, paste('LM_Coefficient', '_', jfl, '.nc', sep = ''))
		nc1 <- nc_create(outnc1, list(grd.slope, grd.intercept))
		ncvar_put(nc1, grd.slope, MODEL.COEF[[jfl]]$slope)
		ncvar_put(nc1, grd.intercept, MODEL.COEF[[jfl]]$intercept)
		nc_close(nc1)
	}

	InsertMessagesTxt(main.txt.out, 'Interpolating LM Coefficients finished')

	rm(adjData, stnData, demData, data.adj.stn, grd.dem,
		data.stn, demGrid, interp.grid, data.stn.reg,
		model.coef, MODEL.COEF, coef0, model.params)
	gc()
	return(0)
}

#################################################################################################

MergingFunctionTemp <- function(paramsMRG){
	GeneralParameters <- paramsMRG$GeneralParameters
	freqData <- GeneralParameters$period
	ncInfo <- paramsMRG$ncInfo

	#############
	is.parallel <- doparallel(length(which(ncInfo$exist)) >= 20)
	`%parLoop%` <- is.parallel$dofun

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
	auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
	is.auxvar <- c(GeneralParameters$auxvar$dem, GeneralParameters$auxvar$slope,
					GeneralParameters$auxvar$aspect, GeneralParameters$auxvar$lon,
					GeneralParameters$auxvar$lat)
	if(any(is.auxvar)){
		formule <- formula(paste('res', '~', paste(auxvar[is.auxvar], collapse = '+'), sep = ''))
		if(Mrg.Method == "Regression Kriging"){
			sp.trend.aux <- GeneralParameters$sp.trend.aux
			if(sp.trend.aux) formuleRK <- formula(paste('stn', '~', 'tmp', '+', paste(auxvar[is.auxvar], collapse = '+'), sep = ''))
			else formuleRK <- formula(paste('stn', '~', 'tmp', sep = ''))
		}
	}else{
		formule <- formula(paste('res', '~', 1, sep = ''))
		if(Mrg.Method == "Regression Kriging") formuleRK <- formula(paste('stn', '~', 'tmp', sep = ''))
	}

	#############
	demData <- paramsMRG$demData
	if(!is.null(demData)){
		demData$z[demData$z < 0] <- 0
		# demres <- grdSp@grid@cellsize
		# slpasp <- slope.aspect(demData$z, demres[1], demres[2], filter = "sobel")
		slpasp <- raster.slope.aspect(demData)
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

	interp.grid$coords.stn$alon <- interp.grid$coords.stn@coords[, 'lon']
	interp.grid$coords.stn$alat <- interp.grid$coords.stn@coords[, 'lat']
	interp.grid$coords.grd$alon <- interp.grid$coords.grd@coords[, 'lon']
	interp.grid$coords.grd$alat <- interp.grid$coords.grd@coords[, 'lat']
	if(!is.null(interp.grid$coords.rfe)){
		interp.grid$coords.rfe$alon <- interp.grid$coords.rfe@coords[, 'lon']
		interp.grid$coords.rfe$alat <- interp.grid$coords.rfe@coords[, 'lat']
	}
	interp.grid$newgrid$alon <- interp.grid$newgrid@coords[, 'lon']
	interp.grid$newgrid$alat <- interp.grid$newgrid@coords[, 'lat']

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
	ret <- foreach(jj = seq_along(ncInfo$nc.files), .packages = packages, .export = toExports) %parLoop% {
		if(ncInfo$exist[jj]){
			nc <- nc_open(ncInfo$nc.files[jj])
			xtmp <- ncvar_get(nc, varid = nc$var[[1]]$name)
			nc_close(nc)
		}else return(NULL)

		############
		locations.stn <- interp.grid$coords.stn
		donne.stn <- data.stn[date.stn == ncInfo$dates[jj], , drop = FALSE]
		if(nrow(donne.stn) == 0) return(NULL)
		locations.stn$stn <- c(donne.stn[1, ])

		locations.stn$tmp <- xtmp[ijGrd]
		xadd <- as.data.frame(interp.grid$coords.grd)
		xadd$tmp <- c(xtmp[interp.grid$idxy$ix, interp.grid$idxy$iy])
		xadd$stn <- xadd$tmp
		xadd$res <- 0
		interp.grid$newgrid$tmp <- c(xtmp)
		coords.grd <- data.frame(coordinates(interp.grid$newgrid))

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
			}else if(Mrg.Method == "Regression Kriging"){
				glm.stn <- glm(formuleRK, data = locations.stn, family = gaussian)
				sp.trend <- predict(glm.stn, newdata = interp.grid$newgrid)
				sp.trend <- matrix(sp.trend, ncol = nlat0, nrow = nlon0)
				sp.trend[is.na(sp.trend)] <- xtmp[is.na(sp.trend)]
				if(length(glm.stn$na.action) > 0) locations.stn$res[-glm.stn$na.action] <- glm.stn$residuals
				else locations.stn$res <- glm.stn$residuals
			}else{
				sp.trend <- xtmp
				locations.stn$res <- locations.stn$stn - locations.stn$tmp
			}
			
			locations.stn <- locations.stn[!is.na(locations.stn$res), ]
			############
			if(any(is.auxvar)){
				locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
				locations.stn <- locations.stn[Reduce("&", locations.df), ]
			}
			if(length(locations.stn) < min.stn) do.merging <- FALSE
		}else do.merging <- FALSE

		############

		if(do.merging){
			if(interp.method == 'Kriging'){
				vgm <- try(autofitVariogram(formule, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
				if(!inherits(vgm, "try-error")) vgm <- vgm$var_model
				else vgm <- NULL
			}else vgm <- NULL

			###########
			xstn <- as.data.frame(locations.stn)
			iadd.in <- iadd.out <- rep(TRUE, nrow(xadd))
			# igrd <- rep(TRUE, nrow(coords.grd))

			for(k in 1:nrow(xstn)){
				dst <- sqrt((xstn$lon[k]-xadd$lon)^2+(xstn$lat[k]-xadd$lat)^2)*sqrt(2)
				iadd <- iadd & (dst >= maxdist)
			}
			# for(k in 1:nrow(xstn)){
			# 	dst <- sqrt((xstn$lon[k]-xadd$lon)^2+(xstn$lat[k]-xadd$lat)^2)*sqrt(2)
			# 	iadd.in <- iadd.in & (dst >= maxdist)
			# 	iadd.out <- iadd.out & (dst >= 3*maxdist)
			# 	dst.grd <- sqrt((xstn$lon[k]-coords.grd$lon)^2+(xstn$lat[k]-coords.grd$lat)^2)*sqrt(2)
			# 	igrd <- igrd & (dst.grd >= 2*maxdist)
			# }

			xadd <- xadd[iadd, ]
			locations.stn <- rbind(xstn, xadd)
			coordinates(locations.stn) <- ~lon+lat
			newdata <- interp.grid$newgrid
			# xadd <- xadd[iadd.in & !iadd.out, ]
			# locations.stn <- rbind(xstn, xadd)
			# coordinates(locations.stn) <- ~lon+lat
			# newdata <- interp.grid$newgrid[!igrd, ]

			###########
			if(any(is.auxvar)){
				locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
				locations.stn <- locations.stn[Reduce("&", locations.df), ]
				block <- NULL
			}else block <- bGrd
			res.grd <- krige(formule, locations = locations.stn, newdata = newdata, model = vgm,
								block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)

			extrm <- c(min(locations.stn$res, na.rm = TRUE), max(locations.stn$res, na.rm = TRUE))
			ixtrm <- is.na(res.grd$var1.pred) | (res.grd$var1.pred < extrm[1] | res.grd$var1.pred > extrm[2])
			res.grd$var1.pred[ixtrm] <- NA

			ina <- is.na(res.grd$var1.pred)
			if(any(ina)){
				res.grd.na <- krige(var1.pred~1, locations = res.grd[!ina, ], newdata = newdata[ina, ], model = vgm,
										block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				res.grd$var1.pred[ina] <- res.grd.na$var1.pred
			}

			###########
			resid <- matrix(res.grd$var1.pred, ncol = nlat0, nrow = nlon0)
			resid[is.na(resid)] <- 0
			# resid <- rep(0, length(igrd))
			# resid[!igrd] <- res.grd$var1.pred
			# resid <- matrix(resid, ncol = nlat0, nrow = nlon0)
			# resid[is.na(resid)] <- 0
			# sp.trend[igrd] <- xtmp[igrd]

			out.mrg <- sp.trend + resid
		}else out.mrg <- xtmp

		#Apply mask for area of interest
		if(!is.null(outMask)) out.mrg[is.na(outMask)] <- NA
		out.mrg[is.na(out.mrg)] <- -99

		############
		year <- substr(ncInfo$dates[jj], 1, 4)
		month <- substr(ncInfo$dates[jj], 5, 6)
		if(freqData == 'daily') mrgfrmt <- sprintf(Mrg.file.format, year, month, substr(ncInfo$dates[jj], 7, 8))
		else if(freqData == 'dekadal') mrgfrmt <- sprintf(Mrg.file.format, year, month, substr(ncInfo$dates[jj], 7, 7))
		else  mrgfrmt <- sprintf(Mrg.file.format, year, month)

		outfl <- file.path(origdir, mrgfrmt)
		nc2 <- nc_create(outfl, grd.nc.out)
		ncvar_put(nc2, grd.nc.out, out.mrg)
		nc_close(nc2)

		rm(out.mrg, sp.trend, resid, res.grd, xadd, locations.stn, newdata, coords.grd, xtmp)
		gc()
		return(0)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	InsertMessagesTxt(main.txt.out, 'Merging finished')

	return(0)
}

#################################################################################################



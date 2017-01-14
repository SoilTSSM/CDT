
ComputeMeanBiasRain <- function(comptMBiasparms){
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

	rfeData <- comptMBiasparms$rfeData
	lon.rfe <- rfeData$lon
	lat.rfe <- rfeData$lat
	date.rfe <- rfeData$dates
	data.rfe <- rfeData$data
	###
	date.bias <- rfeData$dates

	ijGrd <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = lon.rfe, lat = lat.rfe))

	ibsdt <- date.bias%in%date.stn
	istdt <- date.stn%in%date.bias
	if(!any(ibsdt)) return(NULL)

	if(bias.method == 'Multiplicative.Bias.Mon'){
		data.rfe.stn <- t(sapply(data.rfe, function(x){
			if(!is.null(x)) x[ijGrd]
			else rep(NA, length(ijGrd))
		}))

		date.bias <- date.bias[ibsdt]
		data.rfe.stn <- data.rfe.stn[ibsdt, , drop = FALSE]
		# date.stn <- date.stn[istdt]
		data.stn <- data.stn[istdt, , drop = FALSE]

		month.stn <- as(substr(date.bias, 5, 6), 'numeric')
		dataf <- data.frame(id.stn = rep(id.stn, each = nrow(data.stn)),
					times = rep(month.stn, ncol(data.stn)), stn = c(data.stn), rfe = c(data.rfe.stn))

		bias <- by(dataf, dataf$id.stn, bias.RR.times.fun, min.len)
		bias <- lapply(bias, function(x) sapply(x,'[[', 1))
		bias <- t(do.call('rbind', bias))

		rm(data.rfe.stn, dataf)
	}

	if(bias.method == 'Multiplicative.Bias.Var'){
		data.rfe.stn <- t(sapply(data.rfe, function(x){
			if(!is.null(x)) x[ijGrd]
			else rep(NA, length(ijGrd))
		}))

		date.bias <- date.bias[ibsdt]
		data.rfe.stn <- data.rfe.stn[ibsdt, , drop = FALSE]
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
			data.rfe.stn <- data.rfe.stn[ix5days[, 2], ]
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
					times = rep(times.stn, ncol(data.stn)), stn = c(data.stn), rfe = c(data.rfe.stn))

		bias <- by(dataf, dataf$id.stn, bias.RR.times.fun, min.len)
		bias <- lapply(bias, function(x) sapply(x,'[[', 1))
		bias <- t(do.call('rbind', bias))

		rm(data.rfe.stn, dataf)
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
		packages <- c('qmap', 'fitdistrplus')

		data.rfe[sapply(data.rfe, is.null)] <- list(matrix(NA, ncol = length(lat.rfe), nrow = length(lon.rfe)))
		idcoarse <- indexCoarseGrid(lon.rfe, lat.rfe, res.coarse)
		date.stn <- date.stn[istdt]
		data.stn <- data.stn[istdt, , drop = FALSE]
		month.stn <- as(substr(date.stn, 5, 6), 'numeric')

		toExports <- c('data.rfe', 'id.stn', 'data.stn', 'fit.berngamma', 'min.len')

		pars.Obs.Stn <- vector(mode = 'list', length = 12)
		pars.Obs.Stn[months] <- foreach (m = months, .packages = packages, .export = toExports) %:% 
		  foreach (id = id.stn, .packages = packages, .export = toExports) %parLoop% {
			xdata <- data.stn[month.stn == m, id.stn == id]
		    fit.berngamma(xdata, min.len)
		}

		month.rfe <- as(substr(date.bias, 5, 6), 'numeric')
		data.rfe <- simplify2array(data.rfe)
		data.rfe <- data.rfe[idcoarse$ix, idcoarse$iy, ]
		nbgrd <- prod(dim(data.rfe)[1:2])
		nt <- dim(data.rfe)[3]
		data.rfe <- aperm(data.rfe, c(3, 1, 2))
		dim(data.rfe) <- c(nt, nbgrd)
		id.grdRfe <- 1:nbgrd
		pars.Crs.Rfe <- vector(mode = 'list', length = 12)
		pars.Crs.Rfe[months] <- foreach (m = months, .packages = packages, .export = toExports) %:% 
		  foreach (id = id.grdRfe, .packages = packages, .export = toExports) %parLoop% {
			xdata <- data.rfe[month.rfe == m, id.grdRfe == id]
		    fit.berngamma(xdata, min.len)
		}
		if(closeklust) stopCluster(klust)

		pars.Stn <- extract.BG.parameters(months, pars.Obs.Stn)
		pars.Rfe <- extract.BG.parameters(months, pars.Crs.Rfe)
		bias <- list(fit.stn = pars.Obs.Stn, fit.rfe = pars.Crs.Rfe, pars.stn = pars.Stn, pars.rfe = pars.Rfe)
		rm(pars.Obs.Stn, pars.Crs.Rfe, pars.Stn, pars.Rfe)
	}

	InsertMessagesTxt(main.txt.out, 'Computing bias factors finished')
	tcl("update")
	rm(stnData, rfeData, data.stn, data.rfe)
	gc()
	return(bias)
}


########################################################################################################

InterpolateMeanBiasRain <- function(interpBiasparams){
	InsertMessagesTxt(main.txt.out, 'Interpolate bias factors ...')
	tcl("update")

	GeneralParameters <- interpBiasparams$GeneralParameters
	freqData <- GeneralParameters$period
	create.grd <- GeneralParameters$Create.Grid

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
	xy.rfe <- interpBiasparams$xy.rfe

	months <- as.numeric(GeneralParameters$Bias.Months)
	bias.method <- GeneralParameters$Bias.Method
	bias.pars <- interpBiasparams$bias.pars
	meanBiasPrefix <- GeneralParameters$Prefix$Mean.Bias.Prefix

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

	# regrid RFE
	rfeGrid <- NULL
	rfeSp <- defSpatialPixels(xy.rfe)
	is.regridRFE <- is.diffSpatialPixelsObj(grdSp, rfeSp, tol = 1e-07)
	if(create.grd != '1' & is.regridRFE){
		rfeGrid <- interp.surface.grid(demGrid, list(x = xy.rfe$lon, y = xy.rfe$lat))
		irfe <- over(rfeSp, grdSp)
		rfeGrid$slp <- matrix(demGrid$slp[irfe], length(xy.rfe$lon), length(xy.rfe$lat))
		rfeGrid$asp <- matrix(demGrid$asp[irfe], length(xy.rfe$lon), length(xy.rfe$lat))
	}

	## create grid to interp
	bGrd <- NULL
	if(interp.method == 'NN'){
		interp.grid <- createGrid(ObjStn, demGrid, rfeGrid, as.dim.elv = as.dim.elv, latlong = latlong,
							normalize = normalize, coarse.grid = TRUE, res.coarse = res.coarse)
		xy.maxdist <- sqrt(sum((c(rad.lon, rad.lat)*(apply(coordinates(interp.grid$newgrid)[, c('lon', 'lat')], 2,
						function(x) diff(range(x)))/(c(nlon0, nlat0)-1)))^2))
		elv.grd <- range(demGrid$z, na.rm = TRUE)
		nelv <- length(seq(elv.grd[1], elv.grd[2], 100))
		nelv <- if(nelv > 1) nelv else 2
		z.maxdist <- rad.elv*(diff(range(coordinates(interp.grid$newgrid)[, 'elv']))/(nelv-1))
		xy.maxdist <- if(xy.maxdist < res.coarse) res.coarse else xy.maxdist
	}else{
		interp.grid <- createGrid(ObjStn, demGrid, rfeGrid, as.dim.elv = FALSE,
								coarse.grid = TRUE, res.coarse = res.coarse)
		maxdist <- if(maxdist < res.coarse) res.coarse else maxdist
		cells <- SpatialPixels(points = interp.grid$newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)))@grid
		bGrd <- createBlock(cells@cellsize, 2, 5)
	}

	# params.grid <- list(xy.grid = xy.grid, interp.grid = interp.grid)
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
		toExports <- c('bias.pars', 'itimes', 'interp.grid', 'interp.method', 'formule',
						'auxvar', 'is.auxvar', 'min.stn', 'vgm.model', 'nmin', 'nmax', 'maxdist', 'bGrd')
		BIAS <- vector(mode = 'list', length = ntimes)
		BIAS[itimes] <- foreach(m = itimes, .packages = packages, .export = toExports) %parLoop% {
			locations.stn <- interp.grid$coords.stn
			locations.stn$pars <- bias.pars[itimes == m, ]
			locations.stn <- locations.stn[!is.na(locations.stn$pars), ]

			extrm <- quantile(locations.stn$pars, probs = c(0.0001, 0.9999))
			locations.stn <- locations.stn[locations.stn$pars > extrm[1] & locations.stn$pars < extrm[2], ]

			if(length(locations.stn$pars) < min.stn) return(matrix(1, ncol = nlat0, nrow = nlon0))
			if(!any(locations.stn$pars != 1)) return(matrix(1, ncol = nlat0, nrow = nlon0))

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
				pars.grd <- krige(pars~1, locations = locations.stn, newdata = interp.grid$newgrid, nmax = 1, debug.level = 0)	
			}else{
				coordinates(locations.stn) <- ~lon+lat
				if(any(is.auxvar)){
					locations.stn <- locations.stn[Reduce("&", as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))), ]
					pars.grd <- krige(formule, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
										nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				}else{
					pars.grd <- krige(formule, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
										block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				}
				# pars.grd$var1.pred[pars.grd$var1.pred < 0] <- NA
				# pars.grd$var1.pred[pars.grd$var1.pred > 3] <- NA
				# extrm <- quantile(pars.grd$var1.pred, probs = c(0.001, 0.999), na.rm = TRUE)
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
			grdbias <- pars.grd$var1.pred
			grdbias[grdbias > 3] <- 3
			grdbias[grdbias < 0] <- 0.1
			grdbias[is.na(grdbias)] <- 1
			matrix(grdbias, ncol = nlat0, nrow = nlon0)
		}
		if(closeklust) stopCluster(klust)
			
		# bias.factor <- list(bias = bias.pars, interp.bias = BIAS)
		# mrgRaindat$bias.factor <- bias.factor

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
						'auxvar', 'is.auxvar', 'min.stn', 'nmin', 'nmax', 'maxdist', 'bGrd', 'create.grd', 'is.regridRFE')

		PARS.stn <- vector(mode = 'list', length = 12)
		PARS.stn[months] <- foreach(m = months, .packages = packages, .export = toExports) %parLoop% {
			pars.mon <- lapply(1:3, function(j){
				locations.stn <- interp.grid$coords.stn
				locations.stn$pars <- bias.pars$pars.stn[[m]][[j]]
				locations.stn <- locations.stn[!is.na(locations.stn$pars), ]
				if(length(locations.stn$pars) < min.stn) return(NULL)
				
				extrm <- quantile(locations.stn$pars, probs = c(0.0001, 0.9999))
				locations.stn <- locations.stn[locations.stn$pars > extrm[1] & locations.stn$pars < extrm[2], ]

				if(interp.method == 'Kriging'){
					vgm <- try(autofitVariogram(formule, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
					vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
				}else vgm <- NULL

				xstn <- as.data.frame(locations.stn)

				if(create.grd != '1' & is.regridRFE) xadd <- as.data.frame(interp.grid$coords.rfe)
				else  xadd <- as.data.frame(interp.grid$coords.grd)
				xadd$pars <- bias.pars$pars.rfe[[m]][[j]]
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
					if(any(is.auxvar)){
						locations.stn <- locations.stn[Reduce("&", as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))), ]
						pars.grd <- krige(formule, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
											nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
					}else{
						pars.grd <- krige(formule, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
											block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
					}
					pars.grd$var1.pred[pars.grd$var1.pred < 0] <- NA
					if(j == 1) pars.grd$var1.pred[pars.grd$var1.pred > 1] <- NA

					# extrm <- quantile(pars.grd$var1.pred, probs = c(0.001, 0.999), na.rm = TRUE)
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
				ret <- matrix(pars.grd$var1.pred, ncol = nlat0, nrow = nlon0)
				if(j == 1){
					ret[ret < 0] <- 0
					ret[ret > 1] <- 1
				}else if(j == 2){
					ret[ret < 0] <- 0.01
				}else ret[ret < 0] <- 0.1
				ret
			})
			names(pars.mon) <- c('prob', 'scale', 'shape')
			pars.mon
		}

		PARS.rfe <- vector(mode = 'list', length = 12)
		PARS.rfe[months] <- foreach(m = months, .packages = packages, .export = toExports) %parLoop% {
			pars.mon <- lapply(1:3, function(j){
				locations.rfe <- interp.grid$coords.grd
				locations.rfe$pars <- bias.pars$pars.rfe[[m]][[j]]
				locations.rfe <- locations.rfe[!is.na(locations.rfe$pars), ]
				if(length(locations.rfe$pars) < min.stn) return(NULL)
				extrm <- quantile(locations.rfe$pars, probs = c(0.0001, 0.9999))
				locations.rfe <- locations.rfe[locations.rfe$pars > extrm[1] & locations.rfe$pars < extrm[2], ]

				if(interp.method == 'Kriging'){
					vgm <- try(autofitVariogram(formule, input_data = locations.rfe, model = vgm.model, cressie = TRUE), silent = TRUE)
					vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
				}else vgm <- NULL

				if(interp.method == 'NN'){
					pars.grd <- krige(pars~1, locations = locations.rfe, newdata = interp.grid$newgrid, nmax = 1, debug.level = 0)
				}else{
					if(any(is.auxvar)){
						locations.rfe <- locations.rfe[Reduce("&", as.data.frame(!is.na(locations.rfe@data[, auxvar[is.auxvar]]))), ]
						pars.grd <- krige(formule, locations = locations.rfe, newdata = interp.grid$newgrid, model = vgm,
										nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
					}else{
						pars.grd <- krige(formule, locations = locations.rfe, newdata = interp.grid$newgrid, model = vgm,
										block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
					}
					pars.grd$var1.pred[pars.grd$var1.pred < 0] <- NA
					if(j == 1) pars.grd$var1.pred[pars.grd$var1.pred > 1] <- NA

					# extrm <- quantile(pars.grd$var1.pred, probs = c(0.001, 0.999), na.rm = TRUE)
					extrm <- c(min(locations.rfe$pars, na.rm = TRUE), max(locations.rfe$pars, na.rm = TRUE))
					ixtrm <- is.na(pars.grd$var1.pred) | (pars.grd$var1.pred <= extrm[1] | pars.grd$var1.pred >= extrm[2])
					pars.grd$var1.pred[ixtrm] <- NA

					ina <- is.na(pars.grd$var1.pred)
					if(any(ina)){
						pars.grd.na <- krige(var1.pred~1, locations = pars.grd[!ina, ], newdata = interp.grid$newgrid[ina, ], nmax = 1, debug.level = 0)
						pars.grd$var1.pred[ina] <- pars.grd.na$var1.pred
					}
				}
				ret <- matrix(pars.grd$var1.pred, ncol = nlat0, nrow = nlon0)
				if(j == 1){
					ret[ret < 0] <- 0
					ret[ret > 1] <- 1
				}else if(j == 2){
					ret[ret < 0] <- 0.01
				}else ret[ret < 0] <- 0.1
				ret
			})
			names(pars.mon) <- c('prob', 'scale', 'shape')
			pars.mon
		}
		if(closeklust) stopCluster(klust)

		for(m in months){
			for(j in 1:3){
				par.stn <- PARS.stn[[m]][[j]]
				par.rfe <- PARS.rfe[[m]][[j]]
				if(!is.null(par.stn) & !is.null(par.rfe)){
					ix <- is.na(par.stn)
					par.stn[ix] <- par.rfe[ix]
					PARS.stn[[m]][[j]] <- par.stn
				}
				if(is.null(par.rfe)){
					ret <- matrix(NA, ncol = nlat0, nrow = nlon0)
					if(j == 1) ret[] <- 0
					if(j == 2) ret[] <- 1
					if(j == 3) ret[] <- 1
					PARS.stn[[m]][[j]] <- ret
					PARS.rfe[[m]][[j]] <- ret
				}
			}
		}

		# params.distr <- list(fit.stn = bias.pars$fit.stn, fit.rfe = bias.pars$fit.rfe,
		# 							pars.stn = bias.pars$pars.stn, pars.rfe = bias.pars$pars.rfe,
		# 							interp.pars.stn = PARS.stn, interp.pars.rfe = PARS.rfe)
		# mrgRaindat$params.distr <- params.distr

		#Defines netcdf output
		grd.prob <- ncvar_def("prob", "", xy.dim, NA, longname= "Probability of non-zero event Bernoulli-Gamma distribution", prec = "float")
		grd.scale <- ncvar_def("scale", "", xy.dim, NA, longname= "Scale parameters of the gamma distribution", prec = "float")
		grd.shape <- ncvar_def("shape", "", xy.dim, NA, longname= "Shape parameters of the gamma distribution", prec = "float")

		for(jfl in months){
			if(is.null(PARS.stn[[jfl]]$prob) | is.null(PARS.stn[[jfl]]$scale) | is.null(PARS.stn[[jfl]]$shape)){
				prob.nc <- PARS.rfe[[jfl]]$prob
				scale.nc <- PARS.rfe[[jfl]]$scale
				shape.nc <- PARS.rfe[[jfl]]$shape
			}else{
				prob.nc <- PARS.stn[[jfl]]$prob
				scale.nc <- PARS.stn[[jfl]]$scale
				shape.nc <- PARS.stn[[jfl]]$shape
			}
			outnc1 <- file.path(origdir, paste('Bernoulli-Gamma_Pars.STN', '_', jfl, '.nc', sep = ''))
			nc1 <- nc_create(outnc1, list(grd.prob, grd.scale, grd.shape))
			ncvar_put(nc1, grd.prob, prob.nc)
			ncvar_put(nc1, grd.scale, scale.nc)
			ncvar_put(nc1, grd.shape, shape.nc)
			nc_close(nc1)
		}

		for(jfl in months){
			outnc2 <- file.path(origdir, paste('Bernoulli-Gamma_Pars.RFE', '_', jfl, '.nc', sep = ''))
			nc2 <- nc_create(outnc2, list(grd.prob, grd.scale, grd.shape))
			ncvar_put(nc2, grd.prob, PARS.rfe[[jfl]]$prob)
			ncvar_put(nc2, grd.scale, PARS.rfe[[jfl]]$scale)
			ncvar_put(nc2, grd.shape, PARS.rfe[[jfl]]$shape)
			nc_close(nc2)
		}
		rm(PARS.stn, PARS.rfe)
	}

	# outfile <- file.path(origdir, 'DataUsed2ComputeBias.RData')
	# save(mrgRaindat, file = outfile)
	rm(stnData, demData, demGrid, ObjStn, interp.grid)
	gc()
	InsertMessagesTxt(main.txt.out, 'Interpolating bias factors finished')
	tcl("update")
	return(0)
}

########################################################################################################

##correct RFE bias
AjdMeanBiasRain <- function(adjMeanBiasparms){
	InsertMessagesTxt(main.txt.out, 'Correct RFE Bias ...')
	tcl("update")

	GeneralParameters <- adjMeanBiasparms$GeneralParameters
	origdir <- adjMeanBiasparms$origdir

	freqData <- GeneralParameters$period
	bias.method <- GeneralParameters$Bias.Method
	biasDir <- GeneralParameters$IO.files$Bias.dir
	meanBiasPrefix <- GeneralParameters$Prefix$Mean.Bias.Prefix
	adjRfeFF <- GeneralParameters$Prefix$Adj.File.Format
	months <- sort(as.numeric(GeneralParameters$Adjust.Months))
	adjZero <- GeneralParameters$Adjusted.to.Zero

	rfeData <- adjMeanBiasparms$rfeData

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

			daty <- rfeData$dates
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

			daty <- rfeData$dates
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
		pars.stnFile <- file.path(biasDir, paste('Bernoulli-Gamma_Pars.STN', '_', months, '.nc', sep = ''))
		exist.pars.stn <- unlist(lapply(pars.stnFile, file.exists))
		if(any(!exist.pars.stn)){
			miss.pars.stn <- months[!exist.pars.stn]
			for(j in seq_along(miss.pars.stn)){
				msg <- paste('Bernoulli-Gamma_Pars.STN', '_', miss.pars.stn[j], '.nc', sep = '')
				InsertMessagesTxt(main.txt.out, paste(msg, "doesn't exist"), format = TRUE)
				tcl("update")
			}
			return(NULL)
		}

		pars.rfeFile <- file.path(biasDir, paste('Bernoulli-Gamma_Pars.RFE', '_', months, '.nc', sep = ''))
		exist.pars.rfe <- unlist(lapply(pars.rfeFile, file.exists))
		if(any(!exist.pars.rfe)){
			miss.pars.rfe <- months[!exist.pars.rfe]
			for(j in seq_along(miss.pars.rfe)){
				msg <- paste('Bernoulli-Gamma_Pars.RFE', '_', miss.pars.rfe[j], '.nc', sep = '')
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
		PARS.rfe <- vector(mode = 'list', length = 12)
		PARS.stn[months] <- lapply(seq_along(months), function(m){
			nc <- nc_open(pars.stnFile[m])
			prob <- ncvar_get(nc, varid = "prob")
			scale <- ncvar_get(nc, varid = "scale")
			shape <- ncvar_get(nc, varid = "shape")
			nc_close(nc)
			list(prob = prob, scale = scale, shape = shape)
		})

		PARS.rfe[months] <- lapply(seq_along(months), function(m){
			nc <- nc_open(pars.rfeFile[m])
			prob <- ncvar_get(nc, varid = "prob")
			scale <- ncvar_get(nc, varid = "scale")
			shape <- ncvar_get(nc, varid = "shape")
			nc_close(nc)
			list(prob = prob, scale = scale, shape = shape)
		})
		toExports <- c('quantile.mapping.BGamma', 'lon', 'lat', 'PARS.stn', 'PARS.rfe', 'adjZero')
	}

	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", lon)
	dy <- ncdim_def("Lat", "degreeN", lat)
	xy.dim <- list(dx, dy)
	grd.bsadj <- ncvar_def("precip", "mm", xy.dim, -99, longname= "Bias Corrected RFE", prec = "short")

	irfe <- !sapply(rfeData$data, is.null)
	if(!any(irfe)){
		InsertMessagesTxt(main.txt.out, "RFE data not found", format = TRUE)
		return(NULL)
	}

	rfeData$dates <- rfeData$dates[irfe]
	rfeData$data <- rfeData$data[irfe]

	####
	biasSp <- defSpatialPixels(list(lon = lon, lat = lat))
	rfeSp <- defSpatialPixels(list(lon = rfeData$lon, lat = rfeData$lat))
	is.regridRFE <- is.diffSpatialPixelsObj(biasSp, rfeSp, tol = 1e-07)

	if(doparallel & length(rfeData$dates) >= 30){
		klust <- makeCluster(nb_cores)
		registerDoParallel(klust)
		`%parLoop%` <- `%dopar%`
		closeklust <- TRUE
	}else{
		`%parLoop%` <- `%do%`
		closeklust <- FALSE
	}

	packages <- c('ncdf4', 'fields')
	toExports <- c(toExports, 'rfeData', 'is.regridRFE', 'bias.method', 'origdir',
					 'grd.bsadj', 'freqData', 'adjRfeFF')

	ret <- foreach(jfl = seq_along(rfeData$dates), .packages = packages, .export = toExports) %parLoop% {
		xrfe <- rfeData$data[[jfl]]
		drfe <- rfeData$dates[jfl]
		if(is.regridRFE){
			rfeGrid <- interp.surface.grid(list(x = rfeData$lon, y = rfeData$lat, z = xrfe), list(x = lon, y = lat))
			xrfe <- rfeGrid$z
		}
		if(bias.method == "Multiplicative.Bias.Var"){
			if(freqData == 'daily'){
				ann <- as.numeric(substr(drfe, 1, 4))
				iday <- as.numeric(strftime(as.Date(drfe, format = '%Y%m%d'), format = '%j'))
				ijt <- ifelse(ann%%4 == 0 & iday > 59, iday-1, iday)
			}
			if(freqData == 'dekadal'){
				mon <- as.numeric(substr(drfe, 5, 6))
				dek <- as.numeric(substr(drfe, 7, 7))
				annual.dek <- expand.grid(dek = 1:3, mon = 1:12)
				ijt <- which(annual.dek$dek == dek & annual.dek$mon == mon)
			}
			if(freqData == 'monthly'){
				ijt <- as.numeric(substr(drfe, 5, 6))
			}
		}else{
			ijt <- as.numeric(substr(drfe, 5, 6))
		}

		if(bias.method == "Quantile.Mapping"){
			xadj <- quantile.mapping.BGamma(xrfe, PARS.stn[[ijt]], PARS.rfe[[ijt]], adjZero)
		}else xadj <- xrfe * BIAS[[ijt]]
		xadj[xadj < 0] <- 0
		xadj[is.na(xadj)] <- -99

		############
		if(freqData == 'daily'){
			outfl <- file.path(origdir, sprintf(adjRfeFF, substr(drfe, 1, 4), substr(drfe, 5, 6), substr(drfe, 7, 8)))
		}else  if(freqData == 'dekadal'){
			outfl <- file.path(origdir, sprintf(adjRfeFF, substr(drfe, 1, 4), substr(drfe, 5, 6), substr(drfe, 7, 7)))
		}else  if(freqData == 'monthly'){
			outfl <- file.path(origdir, sprintf(adjRfeFF, substr(drfe, 1, 4), substr(drfe, 5, 6)))
		}
		#Save adjusted data
		nc2 <- nc_create(outfl, grd.bsadj)
		ncvar_put(nc2, grd.bsadj, xadj)
		nc_close(nc2)
		return(0)
	}
	if(closeklust) stopCluster(klust)
	rm(rfeData)
	gc()
	InsertMessagesTxt(main.txt.out, 'Bias Correction finished')
	tcl("update")
	return(0)
}

########################################################################################################

ComputeLMCoefRain <- function(comptLMparams){
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
	rfeData <- comptLMparams$rfeData
	lon.rfe <- rfeData$lon
	lat.rfe <- rfeData$lat
	date.rfe <- rfeData$dates
	data.rfe <- rfeData$data

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

	## rfe at stn loc
	ijrfe <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = lon.rfe, lat = lat.rfe))
	data.rfe.stn  <- t(sapply(data.rfe, function(x){
		if(!is.null(x)) x[ijrfe]
		else rep(NA, length(ijrfe))
	}))

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

	dtrfe <- date.rfe%in%date.stn
	dtstn <- date.stn%in%date.rfe
	data.stn <- data.stn[dtstn, ]
	date.stn <- date.stn[dtstn]
	data.rfe.stn <- data.rfe.stn[dtrfe, ]
	# date.rfe <- date.rfe[dtrfe]

	month.stn <- as(substr(date.stn, 5, 6), 'numeric')
	year.stn <- as(substr(date.stn, 1, 4), 'numeric')
	iyear0 <- (year.stn >= year1 & year.stn <= year2) & (month.stn%in%months)

	data.stn.reg <- data.stn[iyear0, ]
	mon.stn.reg <- month.stn[iyear0]
	dataf <- data.frame(id.stn = rep(id.stn, each = nrow(data.stn.reg)), month = rep(mon.stn.reg, ncol(data.stn.reg)),
						stn = c(data.stn.reg), rfe = c(data.rfe.stn))

	##############
	model <- by(dataf, dataf$id.stn, fitLM.month.RR, min.len)
	model.coef <- lapply(model, function(x){
		sapply(x, function(m){
			if(is.null(m)) c(NA, NA)
			else{
				xcoef <- coefficients(m)
				if(is.na(xcoef[2]) | xcoef[2] <= 0) xcoef <- c(NA, NA)
				xcoef
			}
		})
	})
	model.coef <- list(slope = sapply(model.coef, function(x) x[2, ]), intercept = sapply(model.coef, function(x) x[1, ]))

	#model.params <- list(model = model, coef = model.coef)
	#save(model.params, file = file.path(origdir, "MODEL_OUTPUT.RData"))

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
					pars.grd <- krige(formule, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
										nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				}else{
					pars.grd <- krige(formule, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
										block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				}
				
				# extrm <- quantile(pars.grd$var1.pred, probs = c(0.001, 0.999), na.rm = TRUE)
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
		xneg <- MODEL.COEF[[jfl]]$slope <= 0
		MODEL.COEF[[jfl]]$slope[xneg] <- 1
		MODEL.COEF[[jfl]]$intercept[xneg] <- 0
		outnc1 <- file.path(origdir, paste('LM_Coefficient', '_', jfl, '.nc', sep = ''))
		nc1 <- nc_create(outnc1, list(grd.slope, grd.intercept))
		ncvar_put(nc1, grd.slope, MODEL.COEF[[jfl]]$slope)
		ncvar_put(nc1, grd.intercept, MODEL.COEF[[jfl]]$intercept)
		nc_close(nc1)
	}

	InsertMessagesTxt(main.txt.out, 'Computing LM Coefficients finished')
	tcl("update")
	rm(rfeData, stnData, demData, data.rfe.stn, grd.dem,
		data.stn, data.rfe, demGrid, interp.grid, data.stn.reg,
		model, model.coef, MODEL.COEF)
	return(0)
}

########################################################################################################
###Merging

MergingFunctionRain <- function(paramsMRG){
	InsertMessagesTxt(main.txt.out, 'Merging data ...')
	tcl("update")

	GeneralParameters <- paramsMRG$GeneralParameters
	freqData <- GeneralParameters$period

	rfeDir <- GeneralParameters$IO.files$RFE.dir
	rfefilefrmt <- GeneralParameters$FileFormat$RFE.File.Format
	months <- GeneralParameters$Mrg.Months
	daty <- GeneralParameters$Mrg.Date.Range
	start.date <- as.Date(paste(daty$start.year, daty$start.mon, daty$start.dek, sep = '-'))
	end.date <- as.Date(paste(daty$end.year, daty$end.mon, daty$end.dek, sep = '-'))
	error.msg <- "RFE data not found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, rfeDir, rfefilefrmt, error.msg)
	if(is.null(ncInfo)) return(NULL)

	#############
	if(doparallel & length(which(ncInfo$exist)) >= 10){
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

	xy.grid <- paramsMRG$xy.grid
	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- length(xy.grid$lon)
	nlat0 <- length(xy.grid$lat)

	#############
	## Def ncdf
	dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	grd.nc.out <- ncvar_def("precip", "mm", list(dx, dy), -99, longname = "Merged Station-Satellite Rainfall", prec = "short")

	#############
	stnData <- paramsMRG$stnData
	id.stn <- stnData$id
	lon.stn <- stnData$lon
	lat.stn <- stnData$lat
	date.stn <- stnData$dates
	data.stn <- stnData$data

	#############
	rfeDataInfo <- paramsMRG$rfeDataInfo
	lon.rfe <- rfeDataInfo$lon
	lat.rfe <- rfeDataInfo$lat
	rfeSp <- defSpatialPixels(list(lon = lon.rfe, lat = lat.rfe))
	is.regridRFE <- is.diffSpatialPixelsObj(grdSp, rfeSp, tol = 1e-07)

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
	min.non.zero <- GeneralParameters$Mrg.set$min.non.zero

 	use.RnoR <- GeneralParameters$Mrg.set$use.RnoR
 	smooth.RnoR <- GeneralParameters$Mrg.set$smooth.RnoR
	wet.day <- GeneralParameters$Mrg.set$wet.day
	# maxdist.RnoR <- GeneralParameters$Mrg.set$maxdist.RnoR
	# maxdist.RnoR <- if(maxdist.RnoR < res.coarse) res.coarse else maxdist.RnoR

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
			if(closeklust) stopCluster(klust)
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
	nc <- nc_open(ncInfo$nc.files[which(ncInfo$exist)[1]])
	xlon <- nc$dim[[rfeDataInfo$rfeILon]]$vals
	xlat <- nc$dim[[rfeDataInfo$rfeILat]]$vals
	nc_close(nc)
	xo <- order(xlon)
	xlon <- xlon[xo]
	yo <- order(xlat)
	xlat <- xlat[yo]

	## gridded data at stn loc
	ijGrd <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = xlon, lat = xlat))

	#############
	packages <- c('ncdf4', 'gstat', 'automap')
	toExports <- c('ncInfo', 'smooth.matrix')
	ncdata <- foreach(jj = seq_along(ncInfo$nc.files), .packages = packages, .export = toExports) %parLoop% {
		if(ncInfo$exist[jj]){
			nc <- nc_open(ncInfo$nc.files[jj])
			xrfe <- ncvar_get(nc, varid = rfeDataInfo$rfeVarid)
			nc_close(nc)
			xrfe <- xrfe[xo, yo]
			if(rfeDataInfo$rfeILat == 1){
				xrfe <- matrix(c(xrfe), nrow = length(xlon), ncol = length(xlat), byrow = TRUE)
			}
		}else return(NULL)

		############
		if(is.regridRFE){
			rfeGrid <- interp.surface.grid(list(x = xlon, y = xlat, z = xrfe), list(x = xy.grid$lon, y = xy.grid$lat))
			xrfe <- rfeGrid$z
		}

		############
		locations.stn <- interp.grid$coords.stn
		locations.stn$stn <- data.stn[date.stn == ncInfo$dates[jj], ]
		locations.stn$rfe <- xrfe[ijGrd]
		xadd <- as.data.frame(interp.grid$coords.grd)
		xadd$rfe <- c(xrfe[interp.grid$idxy$ix, interp.grid$idxy$iy])
		xadd$stn <- xadd$rfe
		xadd$res <- 0
		interp.grid$newgrid$rfe <- c(xrfe)

		############
		noNA <- !is.na(locations.stn$stn)
		min.stn.nonNA <- length(which(noNA))
		nb.stn.nonZero <- length(which(noNA & locations.stn$stn > 0))
		locations.stn <- locations.stn[noNA, ]

		if(min.stn.nonNA >= min.stn){
			do.merging <- TRUE
			if(nb.stn.nonZero >= min.non.zero){
				if(Mrg.Method == "Spatio-Temporal LM"){
					mo <- as(substr(ncInfo$dates[jj], 5, 6), 'numeric')
					sp.trend <- xrfe * MODEL.COEF[[mo]]$slope + MODEL.COEF[[mo]]$intercept
					locations.stn$res <- locations.stn$stn - sp.trend[ijGrd][noNA]
				}else{
					simplediff <- if(var(locations.stn$stn) < 1e-07 | var(locations.stn$rfe) < 1e-07) TRUE else FALSE
					glm.stn <- glm(stn~rfe, data = locations.stn, family = gaussian)
					if(is.na(glm.stn$coefficients[2]) | glm.stn$coefficients[2] < 0) simplediff <- TRUE
					if(!simplediff){
						sp.trend <- predict(glm.stn, newdata = interp.grid$newgrid)
						sp.trend <- matrix(sp.trend, ncol = nlat0, nrow = nlon0)
						locations.stn$res <- residuals(glm.stn)
					}else{
						sp.trend <- xrfe
						locations.stn$res <- locations.stn$stn - locations.stn$rfe
					}
				}
			}else{
				sp.trend <- xrfe
				locations.stn$res <- locations.stn$stn - locations.stn$rfe
			}
			
			############
			locations.stn <- locations.stn[!is.na(locations.stn$res), ]
			# extrm <- quantile(locations.stn$res, probs = c(0.0001, 0.9999))
			# locations.stn <- locations.stn[locations.stn$res > extrm[1] & locations.stn$res < extrm[2], ]
		}else do.merging <- FALSE

		############

		if(do.merging){
			if(use.RnoR){
				rnr.rfe <- xrfe
				rnr.rfe[] <- 0
				rnr.rfe[xrfe >= wet.day] <- 1
				locations.stn$rnr.stn <- ifelse(locations.stn$stn >= wet.day,1,0)
				xadd$rnr.stn <- c(rnr.rfe[interp.grid$idxy$ix, interp.grid$idxy$iy])

				###########
				### binomial logistic regression
				glm.binom <- glm(rnr.stn ~ rfe, data = locations.stn, family = binomial(link = "logit"))

				############
				xadd$rnr.res <- 0
				locations.stn$rnr.res <- residuals(glm.binom)
				rnr <- predict(glm.binom, newdata = interp.grid$newgrid, type = 'link')
				rnr <- matrix(rnr, ncol = nlat0, nrow = nlon0)
			
				coords.grd <- data.frame(coordinates(interp.grid$newgrid))
				irnr <- rep(TRUE, nrow(coords.grd))
			}

			############
			if(interp.method == 'Kriging'){
				vgm <- try(autofitVariogram(formule, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
				vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
			}else vgm <- NULL

			###########
			xstn <- as.data.frame(locations.stn)
			iadd <- rep(TRUE, nrow(xadd))
			for(k in 1:nrow(xstn)){
				dst <- sqrt((xstn$lon[k]-xadd$lon)^2+(xstn$lat[k]-xadd$lat)^2)*sqrt(2)
				iadd <- iadd & (dst >= maxdist)
				if(use.RnoR){
					dst.grd <- sqrt((xstn$lon[k]-coords.grd$lon)^2+(xstn$lat[k]-coords.grd$lat)^2)*sqrt(2)
					irnr <- irnr & (dst.grd >= maxdist)
				}
			}

			xadd <- xadd[iadd, ]
			locations.stn <- rbind(xstn, xadd)
			coordinates(locations.stn) <- ~lon+lat

			###########
			if(any(is.auxvar)){
				locations.stn <- locations.stn[Reduce("&", as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))), ]
				res.grd <- krige(formule, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
									nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
			}else{
				res.grd <- krige(formule, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
									block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
			}

			# extrm <- quantile(res.grd$var1.pred, probs = c(0.001, 0.999), na.rm = TRUE)
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

			############
			if(use.RnoR){
				rnr.res.grd <- krige(rnr.res~1, locations = locations.stn, newdata = interp.grid$newgrid,
									maxdist = maxdist, block = bGrd,  debug.level = 0)
				ina <- is.na(rnr.res.grd$var1.pred)
				if(any(ina)){
					rnr.res.grd.na <- krige(var1.pred~1, locations = rnr.res.grd[!ina, ], newdata = interp.grid$newgrid[ina, ],
											block = bGrd, maxdist = maxdist, debug.level = 0)
					rnr.res.grd$var1.pred[ina] <- rnr.res.grd.na$var1.pred
				}

				rnr.res.grd <- matrix(rnr.res.grd$var1.pred, ncol = nlat0, nrow = nlon0)
				rnr.res.grd[is.na(rnr.res.grd)] <- 0

				rnr <- rnr + rnr.res.grd
				rnr[rnr > 100] <- 100
				rnr <- exp(rnr)/(1+exp(rnr))

				### decision boundary 0.6
				rnr[rnr >= 0.6] <- 1
				rnr[rnr < 0.6] <- 0

				imsk <- matrix(irnr, nrow = nlon0, ncol = nlat0)
				rnr[imsk] <- rnr.rfe[imsk]
				if(smooth.RnoR){
					npix <- if(sum(rnr.rfe, na.rm = TRUE) == 0) 5 else 3
					rnr <- smooth.matrix(rnr, npix)
				}
			}else rnr <- matrix(1, ncol = nlat0, nrow = nlon0)

			############
			out.mrg <- sp.trend + resid
			out.mrg[out.mrg < 0] <- 0
			out.mrg <- out.mrg * rnr
		}else out.mrg <- xrfe

		#Apply mask for area of interest
		if(!is.null(outMask)) out.mrg[is.na(outMask)] <- NA
		out.mrg[is.na(out.mrg)] <- -99

		############
		if(freqData == 'daily'){
			outfl <- file.path(origdir, sprintf(Mrg.file.format, substr(ncInfo$dates[jj], 1, 4),
								substr(ncInfo$dates[jj], 5, 6), substr(ncInfo$dates[jj], 7, 8)))
		}else if(freqData == 'dekadal'){
			outfl <- file.path(origdir, sprintf(Mrg.file.format, substr(ncInfo$dates[jj], 1, 4),
								substr(ncInfo$dates[jj], 5, 6), substr(ncInfo$dates[jj], 7, 7)))
		}else if(freqData == 'monthly'){
			outfl <- file.path(origdir, sprintf(Mrg.file.format, substr(ncInfo$dates[jj], 1, 4),
								substr(ncInfo$dates[jj], 5, 6)))
		}

		nc2 <- nc_create(outfl, grd.nc.out)
		ncvar_put(nc2, grd.nc.out, round(out.mrg))
		nc_close(nc2)
		return(0)
	}

	############
	InsertMessagesTxt(main.txt.out, 'Scaling up daily data ...')
	tcl("update")
	dekdaty <- ncInfo$dates[ncInfo$exist]
	if(freqData == "daily" & GeneralParameters$Scale.daily & length(dekdaty) > 7){
		dekDataInfo <- getRFESampleData(GeneralParameters$IO.files$DEK.file)
		dekDir <- GeneralParameters$IO.files$DEK.dir
		dekfilefrmt <- GeneralParameters$FileFormat$DEK.File.Format
		months <- GeneralParameters$Mrg.Months

		an <- substr(dekdaty, 1, 4)
		mois <- substr(dekdaty, 5, 6)
		jour <- as.numeric(substr(dekdaty, 7, 8))
		outfl <- file.path(origdir, sprintf(Mrg.file.format, an, mois, jour))

		nbs <- ifelse(jour[1] <= 10, 10, ifelse(jour[1] > 10 & jour[1] <= 20, 10,
					rev((28:31)[which(!is.na(as.Date(paste(an[1], mois[1], 28:31, sep = '-'))))])[1]-20))
		nbe <- ifelse(jour[length(an)] <= 10, 10, ifelse(jour[length(an)] > 10 & jour[length(an)] <= 20, 10,
					rev((28:31)[which(!is.na(as.Date(paste(an[length(an)], mois[length(an)], 28:31, sep = '-'))))])[1]-20))
		jour[jour <= 10] <- 1
		jour[jour > 10 & jour <= 20] <- 2
		jour[jour > 20] <- 3
		nbd <- rle(jour)$lengths
		nbd[c(1, length(nbd))] <- c(nbs, nbe)
		jj <- paste(an, mois, jour, sep = '')
		dekDate <- cbind(aggregate(rep(1, length(an)), by = list(jj), sum), nbd, seq_along(nbd))
		names(dekDate) <- c('date', 'izy', 'feno', 'id')

		daily.data <- data.frame(id = seq_along(jj), file = outfl)
		dekdaty1 <- as.character(dekDate$date)
		for(j in seq_along(dekdaty1)) daily.data[jj == dekdaty1[j], 1] <- dekDate$id[j]

		an1 <- substr(dekdaty1, 1, 4)
		mois1 <- substr(dekdaty1, 5, 6)
		jour1 <- substr(dekdaty1, 7, 7)
		start.date1 <- as.Date(paste(an1[1], mois1[1], jour1[1], sep = '-'))
		end.date1 <- as.Date(paste(an1[length(an1)], mois1[length(an1)], jour1[length(an1)], sep = '-'))

		error.msg <- "Merged dekadal data not found"
		dekInfo <- ncFilesInfo('dekadal', start.date1, end.date1, months, dekDir, dekfilefrmt, error.msg)
		if(is.null(dekInfo)){
			if(closeklust) stopCluster(klust)
			return(NULL)
		}
		dekinfo <- data.frame(date = dekInfo$dates, exist = dekInfo$exist, file = dekInfo$nc.files)
		dekinfo <- merge(dekDate, dekinfo, by = "date", all.x = TRUE)
		dekinfo <- dekinfo[dekinfo$izy == dekinfo$feno & dekinfo$exist, , drop = FALSE]
		daily.data <- daily.data[daily.data$id%in%dekinfo$id, , drop = FALSE]
		oper.dek <- list()
		for(j in seq_along(dekinfo$id)) oper.dek[[j]] <- list(dek = as.character(dekinfo$file[dekinfo$id == dekinfo$id[j]]),
															day = as.character(daily.data$file[daily.data$id == dekinfo$id[j]]))
		############
		nc <- nc_open(oper.dek[[1]]$dek)
		xlon <- nc$dim[[dekDataInfo$rfeILon]]$vals
		xlat <- nc$dim[[dekDataInfo$rfeILat]]$vals
		nc_close(nc)
		xo <- order(xlon)
		xlon <- xlon[xo]
		yo <- order(xlat)
		xlat <- xlat[yo]
		rfeDek <- defSpatialPixels(list(lon = xlon, lat = xlat))

		nc <- nc_open(oper.dek[[1]]$day[1])
		dlon <- nc$dim[[1]]$vals
		dlat <- nc$dim[[2]]$vals
		nc_close(nc)
		rfeDay <- defSpatialPixels(list(lon = dlon, lat = dlat))

		is.regridDEK <- is.diffSpatialPixelsObj(rfeDay, rfeDek, tol = 1e-07)

		packages <- c('ncdf4', 'fields')
		ncdata <- foreach(jj = seq_along(oper.dek), .packages = packages) %parLoop% {
			nc <- nc_open(oper.dek[[jj]]$dek)
			xdek <- ncvar_get(nc, varid = dekDataInfo$rfeVarid)
			nc_close(nc)
			xdek <- xdek[xo, yo]
			if(dekDataInfo$rfeILat == 1){
				xdek <- matrix(c(xdek), nrow = length(xlon), ncol = length(xlat), byrow = TRUE)
			}

			if(is.regridDEK){
				dekGrid <- interp.surface.grid(list(x = xlon, y = xlat, z = xdek), list(x = dlon, y = dlat))
				xdek <- dekGrid$z
			}

			xday <- lapply(seq_along(oper.dek[[jj]]$day), function(j){
				nc <- nc_open(oper.dek[[jj]]$day[j])
				xdd <- ncvar_get(nc, varid = nc$var[[1]]$name)
				nc_close(nc)
				xdd
			})

			xday <- simplify2array(xday)
			ddek <- apply(xday, 1:2, sum)
			scl <- xdek/ddek
			scl[is.na(scl) | is.nan(scl) | is.infinite(scl)] <- 1
			xday <- sweep(xday, 1:2, scl, `*`)

			for(j in seq_along(oper.dek[[jj]]$day)){
				nc2 <- nc_create(oper.dek[[jj]]$day[j], grd.nc.out)
				ncvar_put(nc2, grd.nc.out, round(xday[, , j]))
				nc_close(nc2)
			}
		}
	}

	if(closeklust) stopCluster(klust)

	InsertMessagesTxt(main.txt.out, 'Merging finished')
	tcl("update")

	return(0)
}




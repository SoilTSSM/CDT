
########################################################################################################
## Pour validation
########################################################################################################

ComputeMeanBiasRain.validation <- function(comptMBiasparms){
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
		rfeData <- read.NetCDF.Data(comptMBiasparms$rfeData)
		if(is.null(rfeData)) return(NULL)
		ijrfe <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = rfeData$lon, lat = rfeData$lat))
		data.rfe.stn <- t(sapply(rfeData$data, function(x){
			if(!is.null(x)) x[ijrfe]
			else rep(NA, length(ijrfe))
		}))
		if(bias.method == 'Quantile.Mapping'){
			data.rfe <- rfeData$data
			data.rfe[sapply(data.rfe, is.null)] <- list(matrix(NA, ncol = length(rfeData$lat), nrow = length(rfeData$lon)))
			idcoarse <- indexCoarseGrid(rfeData$lon, rfeData$lat, res.coarse)
			data.rfe <- t(sapply(data.rfe, function(x) x[idcoarse$ix, idcoarse$iy, drop = FALSE]))
			ptsData1 <- expand.grid(lon = rfeData$lon[idcoarse$ix], lat = rfeData$lat[idcoarse$iy])
		}
	}else{
		nstn <- length(lon.stn)
		ptsData <- list(lon = lon.stn, lat = lat.stn)
		if(bias.method == 'Quantile.Mapping'){
			idcoarse <- indexCoarseGrid(comptMBiasparms$xy.rfe$lon, comptMBiasparms$xy.rfe$lat, res.coarse)
			ptsData1 <- expand.grid(lon = comptMBiasparms$xy.rfe$lon[idcoarse$ix], lat = comptMBiasparms$xy.rfe$lat[idcoarse$iy])
			nbgrd <- nrow(ptsData1)
			ptsData <- list(lon = c(ptsData$lon, ptsData1$lon), lat = c(ptsData$lat, ptsData1$lat))
		}
		rfeData <- read.NetCDF.Data2Points(comptMBiasparms$rfeData, ptsData)
		if(is.null(rfeData)) return(NULL)
		data.rfe0 <- t(sapply(rfeData$data, function(x) if(!is.null(x)) x else rep(NA, length(ptsData$lon))))
		data.rfe.stn <- data.rfe0[, 1:nstn]
		if(bias.method == 'Quantile.Mapping') data.rfe <- data.rfe0[, nstn+(1:nbgrd)]
	}

	###############
	date.bias <- rfeData$dates
	ibsdt <- date.bias%in%date.stn
	istdt <- date.stn%in%date.bias
	if(!any(ibsdt)){
		InsertMessagesTxt(main.txt.out, "Date out of range", format = TRUE)
		return(NULL)
	}

	###############
	InsertMessagesTxt(main.txt.out, 'Compute bias factors ...')

	if(bias.method == 'Multiplicative.Bias.Mon'){
		date.bias <- date.bias[ibsdt]
		data.rfe.stn <- data.rfe.stn[ibsdt, , drop = FALSE]
		data.stn <- data.stn[istdt, , drop = FALSE]

		month.stn <- as(substr(date.bias, 5, 6), 'numeric')
		dataf <- data.frame(id.stn = rep(id.stn, each = nrow(data.stn)),
					times = rep(month.stn, ncol(data.stn)), stn = c(data.stn), rfe = c(data.rfe.stn))

		bias <- by(dataf, dataf$id.stn, bias.RR.times.fun, min.len)
		bias <- lapply(bias, function(x) sapply(x,'[[', 1))
		bias <- t(do.call('rbind', bias))

		##########
		# bias.pars <- list(bias = bias, lon.stn = lon.stn, lat.stn = lat.stn, id.stn = id.stn,
		# 					data.stn = data.stn, date.rfe = data.rfe.stn, date = date.bias)
		rm(data.rfe.stn, dataf)
	}

	if(bias.method == 'Multiplicative.Bias.Var'){
		date.bias <- date.bias[ibsdt]
		data.rfe.stn <- data.rfe.stn[ibsdt, , drop = FALSE]
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

		##########
		# bias.pars <- list(bias = bias, lon.stn = lon.stn, lat.stn = lat.stn, id.stn = id.stn,
		# 					data.stn = data.stn, date.rfe = data.rfe.stn, date = date.bias)
		rm(data.rfe.stn, dataf)
	}

	if(bias.method == 'Quantile.Mapping'){
		is.parallel <- doparallel(length(months) >= 3)
		`%parLoop%` <- is.parallel$dofun

		mplus.dekad.date <- function(daty){
			dek1 <- as.character(daty)
			dek2 <- sapply(lapply(as.Date(dek1, format = '%Y%m%d'), addDekads, n = 1),
					function(x) paste(format(x, '%Y%m'), as.numeric(format(x, '%d')), sep = ''))
			dek0 <- sapply(lapply(as.Date(dek1, format = '%Y%m%d'), addDekads, n = -1),
					function(x) paste(format(x, '%Y%m'), as.numeric(format(x, '%d')), sep = ''))
			unique(sort(c(dek0, dek1, dek2)))
		}
		mplus.month.date <- function(daty){
			mon1 <- as.character(daty)
			mon2 <- sapply(lapply(as.Date(paste(mon1, '01', sep = ''), format = '%Y%m%d'), addMonths, n = 1), format, '%Y%m')
			mon0 <- sapply(lapply(as.Date(paste(mon1, '01', sep = ''), format = '%Y%m%d'), addMonths, n = -1), format, '%Y%m')
			unique(sort(c(mon0, mon1, mon2)))
		}

		date.stn <- date.stn[istdt]
		data.stn <- data.stn[istdt, , drop = FALSE]
		month.stn <- as(substr(date.stn, 5, 6), 'numeric')
		month.rfe <- as(substr(date.bias, 5, 6), 'numeric')

		packages <- c('qmap', 'fitdistrplus', 'ADGofTest')
		toExports <- c('data.rfe', 'data.rfe.stn', 'data.stn', 'fit.mixture.distr', 'min.len', 'addDekads', 'addMonths')

		parsDistr <- vector(mode = 'list', length = 12)
		parsDistr[months] <- foreach (m = months, .packages = packages, .export = toExports) %parLoop% {
			if(freqData == 'daily'){
				xdt.stn <- month.stn == m
				xdt.rfe <- month.rfe == m
			}
			if(freqData == 'dekadal'){
				xdt.stn <- date.stn%in%mplus.dekad.date(date.stn[month.stn == m])
				xdt.rfe <- date.bias%in%mplus.dekad.date(date.bias[month.rfe == m])
			}
			if(freqData == 'monthly'){
				xdt.stn <- date.stn%in%mplus.month.date(date.stn[month.stn == m])
				xdt.rfe <- date.bias%in%mplus.month.date(date.bias[month.rfe == m])
			}
			xstn <- data.stn[xdt.stn, , drop = FALSE]
			xrfestn <- data.rfe.stn[xdt.rfe, , drop = FALSE]
			xrfe <- data.rfe[xdt.rfe, , drop = FALSE]
			xstn <- lapply(seq(ncol(xstn)), function(j) fit.mixture.distr(xstn[, j], min.len, distr.fun = "berngamma"))
			xrfestn <- lapply(seq(ncol(xrfestn)), function(j) fit.mixture.distr(xrfestn[, j], min.len, distr.fun = "berngamma"))
			xrfe <- lapply(seq(ncol(xrfe)), function(j) fit.mixture.distr(xrfe[, j], min.len, distr.fun = "berngamma"))

			# xstn <- data.stn[month.stn == m, , drop = FALSE]
			# xrfestn <- data.rfe.stn[month.rfe == m, , drop = FALSE]
			# xrfe <- data.rfe[month.rfe == m, , drop = FALSE]
			# xstn <- lapply(seq(ncol(xstn)), function(j) fit.mixture.distr(xstn[, j], min.len, distr.fun = "berngamma"))
			# xrfestn <- lapply(seq(ncol(xrfestn)), function(j) fit.mixture.distr(xrfestn[, j], min.len, distr.fun = "berngamma"))
			# xrfe <- lapply(seq(ncol(xrfe)), function(j) fit.mixture.distr(xrfe[, j], min.len, distr.fun = "berngamma"))
			# # xstn <- lapply(seq(ncol(xstn)), function(j) fit.mixture.distr(xstn[, j], min.len))
			# # xrfestn <- lapply(seq(ncol(xrfestn)), function(j) fit.mixture.distr(xrfestn[, j], min.len))
			# # xrfe <- lapply(seq(ncol(xrfe)), function(j) fit.mixture.distr(xrfe[, j], min.len))
			list(stn = xstn, rfestn = xrfestn, rfe = xrfe)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

		pars.Obs.Stn <- lapply(parsDistr, '[[', 1)
		pars.Obs.rfe <- lapply(parsDistr, '[[', 2)
		pars.Crs.Rfe <- lapply(parsDistr, '[[', 3)

		AD.stn <- outputADTest(pars.Obs.Stn, months, distr = "berngamma")
		AD.rfestn <- outputADTest(pars.Obs.rfe, months, distr = "berngamma")
		AD.rfe <- outputADTest(pars.Crs.Rfe, months, distr = "berngamma")

		pars.Stn <- extractDistrParams(pars.Obs.Stn, months, distr = "berngamma")
		pars.Rfestn <- extractDistrParams(pars.Obs.rfe, months, distr = "berngamma")
		pars.Rfe <- extractDistrParams(pars.Crs.Rfe, months, distr = "berngamma")

		parsAD <- vector(mode = 'list', length = 12)
		parsAD[months] <- lapply(months, function(j){
			istn <- AD.stn[[j]] == 'yes' & AD.rfestn[[j]] == 'yes'
			irfe <- AD.rfe[[j]] == 'yes'
			list(rfe = irfe, stn = istn)
		})

		parsAD.Rfe <- lapply(parsAD, '[[', 1)
		parsAD.Stn <- lapply(parsAD, '[[', 2)
		bias <- list(pars.ad.stn = parsAD.Stn, pars.ad.rfe = parsAD.Rfe, pars.stn = pars.Stn,
					pars.rfestn = pars.Rfestn, pars.rfe = pars.Rfe)

		##########
		# bias.pars <- list(bias = bias, fit.stn = pars.Obs.Stn, fit.rfestn = pars.Obs.rfe, fit.rfe = pars.Crs.Rfe,
		# 				lon.stn = lon.stn, lat.stn = lat.stn, id.stn = id.stn,
		# 				lon.rfe = comptMBiasparms$xy.rfe$lon[idcoarse$ix],
		# 				lat.rfe = comptMBiasparms$xy.rfe$lat[idcoarse$iy],
		# 				data.stn = data.stn, data.rfestn = data.rfe.stn,
		# 				data.rfe = data.rfe, date = date.bias)
		rm(parsDistr, pars.Obs.Stn, pars.Obs.rfe, pars.Crs.Rfe, pars.Stn, pars.Rfe,
			pars.Rfestn, parsAD, parsAD.Rfe, parsAD.Stn)
	}

	bias.pars <- list(bias = bias)
	save(bias.pars, file = file.path(comptMBiasparms$origdir, "BIAS_PARAMS.RData"))

	InsertMessagesTxt(main.txt.out, 'Computing bias factors finished')
	rm(stnData, rfeData, data.stn, data.rfe, bias.pars)
	gc()
	return(bias)
}

########################################################################################################

InterpolateMeanBiasRain.validation <- function(interpBiasparams){
	InsertMessagesTxt(main.txt.out, 'Interpolate bias factors ...')

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

	#Defines netcdf output dims
	# dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	# dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	# xy.dim <- list(dx, dy)

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
		maxdist <- sqrt(xy.maxdist^2 + z.maxdist^2)
	}else{
		interp.grid <- createGrid(ObjStn, demGrid, rfeGrid, as.dim.elv = FALSE,
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

	####################
	ijInt <- interpBiasparams$ijInt
	ijStn0 <- ijStn <- seq(length(ijGrd))

	##################################################################################
	## interpolation
	if(bias.method != 'Quantile.Mapping'){
		itimes <- as.numeric(rownames(bias.pars))
		if(bias.method == 'Multiplicative.Bias.Mon'){
			ntimes <- 12
		}
		if(bias.method == 'Multiplicative.Bias.Var'){
			ntimes <- switch(freqData, 'daily' = 365, 'dekadal' = 36, 'monthly' = 12)
		}

		# if(doparallel & length(itimes) >= 3){
		# 	klust <- makeCluster(nb_cores)
		# 	registerDoParallel(klust)
		# 	`%parLoop%` <- `%dopar%`
		# 	closeklust <- TRUE
		# }else{
		# 	`%parLoop%` <- `%do%`
		# 	closeklust <- FALSE
		# }
		# packages <- c('sp', 'gstat', 'automap')
		# toExports <- c('bias.pars', 'itimes', 'interp.grid', 'interp.method', 'formule',
		# 				'auxvar', 'is.auxvar', 'min.stn', 'vgm.model', 'nmin', 'nmax', 'maxdist', 'bGrd')
		BIAS <- vector(mode = 'list', length = ntimes)
		# BIAS[itimes] <- foreach(m = itimes, .packages = packages, .export = toExports) %parLoop% {
		BIAS[itimes] <- lapply(itimes, function(m){
			locations.stn <- interp.grid$coords.stn
			locations.stn$pars <- bias.pars[itimes == m, ]
			ipna <- !is.na(locations.stn$pars)
			locations.stn <- locations.stn[ipna, ]

			ijStn <- ijStn[ipna]

			extrm <- quantile(locations.stn$pars, probs = c(0.001, 0.999))
			ixtrm <- locations.stn$pars > extrm[1] & locations.stn$pars < extrm[2]
			locations.stn <- locations.stn[ixtrm, ]

			ijStn <- ijStn[ixtrm]
			ijx <- ijStn0[!ijStn0%in%ijStn]
			ijI <- c(ijInt, ijGrd[!ijStn0%in%ijStn])

			if(length(locations.stn$pars) < min.stn) return(rep(1, length(ijGrd)+length(ijInt)))
			if(!any(locations.stn$pars != 1)) return(rep(1, length(ijGrd)+length(ijInt)))
			# if(length(locations.stn$pars) < min.stn) return(matrix(1, ncol = nlat0, nrow = nlon0))
			# if(!any(locations.stn$pars != 1)) return(matrix(1, ncol = nlat0, nrow = nlon0))

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
				pars.grd <- krige(pars~1, locations = locations.stn, newdata = interp.grid$newgrid[ijI, ],
									nmax = 1, maxdist = maxdist, debug.level = 0)	
			}else{
				coordinates(locations.stn) <- ~lon+lat
				if(any(is.auxvar)){
					locations.stn <- locations.stn[Reduce("&", as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))), ]
					block <- NULL
				}else block <- bGrd

				pars.grd <- krige(formule, locations = locations.stn, newdata = interp.grid$newgrid[ijI, ], model = vgm,
									block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)

				extrm <- c(min(locations.stn$pars, na.rm = TRUE), max(locations.stn$pars, na.rm = TRUE))
				ixtrm <- is.na(pars.grd$var1.pred) | (pars.grd$var1.pred < extrm[1] | pars.grd$var1.pred > extrm[2])
				pars.grd$var1.pred[ixtrm] <- NA

				# ina <- is.na(pars.grd$var1.pred)
				# if(any(ina)){
				# 	pars.grd.na <- krige(var1.pred~1, locations = pars.grd[!ina, ], newdata = interp.grid$newgrid[ijI, ][ina, ], model = vgm,
				# 						block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				# 	pars.grd$var1.pred[ina] <- pars.grd.na$var1.pred
				# }
			}
			grdbias <- pars.grd$var1.pred
			# grdbias <- matrix(pars.grd$var1.pred, ncol = nlat0, nrow = nlon0)

			vInterp <- grdbias[1:length(ijInt)]
			stnPars <- bias.pars[itimes == m, ]
			if(length(ijx) > 0) stnPars[ijx] <- grdbias[(length(ijInt)+1):length(ijI)]
			grdbias <- c(stnPars, vInterp)

			grdbias[grdbias > 3] <- 3
			grdbias[grdbias < 0] <- 0.1
			grdbias[is.na(grdbias)] <- 1
			round(grdbias, 3)
		})
		# }
		# if(closeklust) stopCluster(klust)

		for(jfl in itimes){
			outfl <- file.path(origdir, paste(meanBiasPrefix, '_', jfl, '.txt', sep = ''))
			write.table(BIAS[[jfl]], outfl, col.names = FALSE, row.names = FALSE, quote = FALSE)
		}

		#Defines netcdf output
		# grd.bs <- ncvar_def("bias", "", xy.dim, NA, longname= "Multiplicative Mean Bias Factor", prec = "float")
		# for(jfl in itimes){
		# 	outnc <- file.path(origdir, paste(meanBiasPrefix, '_', jfl, '.nc', sep = ''))
		# 	nc2 <- nc_create(outnc, grd.bs)
		# 	ncvar_put(nc2, grd.bs, BIAS[[jfl]])
		# 	nc_close(nc2)
		# }
		rm(BIAS)
	}else{
		# if(doparallel & length(months) >= 3){
		# 	klust <- makeCluster(nb_cores)
		# 	registerDoParallel(klust)
		# 	`%parLoop%` <- `%dopar%`
		# 	closeklust <- TRUE
		# }else{
		# 	`%parLoop%` <- `%do%`
		# 	closeklust <- FALSE
		# }
		# packages <- c('sp', 'gstat', 'automap')
		# toExports <- c('bias.pars', 'months', 'interp.grid', 'interp.method', 'vgm.model', 'formule',
		# 				'auxvar', 'is.auxvar', 'min.stn', 'nmin', 'nmax', 'maxdist', 'bGrd', 'create.grd', 'is.regridRFE')

		PARS.stn <- vector(mode = 'list', length = 12)
		# PARS.stn[months] <- foreach(m = months, .packages = packages, .export = toExports) %parLoop% {
		PARS.stn[months] <- lapply(months, function(m){
			pars.mon <- lapply(1:3, function(j){
				locations.stn <- interp.grid$coords.stn
				locations.stn$pars <- bias.pars$pars.stn[[m]][, j]
				if(j > 1) locations.stn$pars[!bias.pars$pars.ad.stn[[m]]] <- NA
				ipna <- !is.na(locations.stn$pars)
				locations.stn <- locations.stn[ipna, ]

				ijStn <- ijStn[ipna]

				extrm <- quantile(locations.stn$pars, probs = c(0.001, 0.99))
				ixtrm <- locations.stn$pars > extrm[1] & locations.stn$pars < extrm[2]
				locations.stn <- locations.stn[ixtrm, ]
			
				ijStn <- ijStn[ixtrm]
				ijx <- ijStn0[!ijStn0%in%ijStn]
				ijI <- c(ijInt, ijGrd[!ijStn0%in%ijStn])

				if(length(locations.stn$pars) < min.stn) return(NULL)

				if(interp.method == 'Kriging'){
					vgm <- try(autofitVariogram(formule, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
					vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
				}else vgm <- NULL

				xstn <- as.data.frame(locations.stn)
				xadd <- if(create.grd != '1' & is.regridRFE) as.data.frame(interp.grid$coords.rfe) else as.data.frame(interp.grid$coords.grd)
				xadd$pars <- bias.pars$pars.rfe[[m]][, j]
				if(j > 1) xadd$pars[!bias.pars$pars.ad.rfe[[m]]] <- NA

				xaddstn <- as.data.frame(interp.grid$coords.stn)
				xaddstn$pars <- bias.pars$pars.rfestn[[m]][, j]
				if(j > 1) xaddstn$pars[!bias.pars$pars.ad.stn[[m]]] <- NA

				xadd <- rbind(xadd, xaddstn)
				xadd <- xadd[!is.na(xadd$pars), ]
				axtrm <- quantile(xadd$pars, probs = c(0.001, 0.99))
				xadd <- xadd[xadd$pars > axtrm[1] & xadd$pars < axtrm[2], , drop = FALSE]
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

				if(interp.method == 'NN'){
					locations.stn <- locations.stn[!duplicated(locations.stn[, c('lon', 'lat', 'elv')]), ]
					coordinates(locations.stn) <- ~lon+lat+elv
					pars.grd <- krige(pars~1, locations = locations.stn, newdata = interp.grid$newgrid[ijI, ],
										nmax = 1, maxdist = maxdist, debug.level = 0)
				}else{
					locations.stn <- locations.stn[!duplicated(locations.stn[, c('lon', 'lat')]), ]
					coordinates(locations.stn) <- ~lon+lat
					if(any(is.auxvar)){
						locations.stn <- locations.stn[Reduce("&", as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))), ]
						block <- NULL
					}else block <- bGrd
					pars.grd <- krige(formule, locations = locations.stn, newdata = interp.grid$newgrid[ijI, ], model = vgm,
										block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				}
				ret <- pars.grd$var1.pred
				# ret <- matrix(pars.grd$var1.pred, ncol = nlat0, nrow = nlon0)

				vInterp <- ret[1:length(ijInt)]
				stnPars <- bias.pars$pars.stn[[m]][, j]
				if(length(ijx) > 0) stnPars[ijx] <- ret[(length(ijInt)+1):length(ijI)]
				ret <- c(stnPars, vInterp)

				if(j == 1){
					ret[ret < 0] <- 0.01
					ret[ret > 1] <- 0.99
				}else ret[ret < 0] <- NA
				ret
			})
			names(pars.mon) <- c('prob', 'scale', 'shape')
			pars.mon
		})
		# }

		PARS.rfe <- vector(mode = 'list', length = 12)
		# PARS.rfe[months] <- foreach(m = months, .packages = packages, .export = toExports) %parLoop% {
		PARS.rfe[months] <- lapply(months, function(m){
			pars.mon <- lapply(1:3, function(j){
				locations.rfe <- interp.grid$coords.grd
				locations.rfe$pars <- bias.pars$pars.rfe[[m]][, j]
				if(j > 1) locations.rfe$pars[!bias.pars$pars.ad.rfe[[m]]] <- NA

				locations.rfestn <- interp.grid$coords.stn
				locations.rfestn$pars <- bias.pars$pars.rfestn[[m]][, j]
				if(j > 1) locations.rfestn$pars[!bias.pars$pars.ad.stn[[m]]] <- NA

				locations.rfe <- rbind(locations.rfe, locations.rfestn)
				locations.rfe <- locations.rfe[!is.na(locations.rfe$pars), ]
				if(length(locations.rfe$pars) < min.stn) return(NULL)

				extrm <- quantile(locations.rfe$pars, probs = c(0.001, 0.99))
				locations.rfe <- locations.rfe[locations.rfe$pars > extrm[1] & locations.rfe$pars < extrm[2], ]
				locations.rfe <- remove.duplicates(locations.rfe)

				if(any(is.auxvar) & interp.method != 'NN') locations.rfe <- locations.rfe[Reduce("&", as.data.frame(!is.na(locations.rfe@data[, auxvar[is.auxvar]]))), ]
				if(interp.method == 'Kriging'){
					vgm <- try(autofitVariogram(formule, input_data = locations.rfe, model = vgm.model, cressie = TRUE), silent = TRUE)
					vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
				}else vgm <- NULL

				if(interp.method == 'NN'){
					pars.grd <- krige(pars~1, locations = locations.rfe, newdata = interp.grid$newgrid[c(ijGrd, ijInt), ],
										nmax = 1, maxdist = maxdist, debug.level = 0)
				}else{
					block <- if(any(is.auxvar)) NULL else bGrd
					pars.grd <- krige(formule, locations = locations.rfe, newdata = interp.grid$newgrid[c(ijGrd, ijInt), ], model = vgm,
									block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				}
				ret <- pars.grd$var1.pred
				# ret <- matrix(pars.grd$var1.pred, ncol = nlat0, nrow = nlon0)
				if(j == 1){
					ret[ret < 0] <- 0.01
					ret[ret > 1] <- 0.99
				}else ret[ret < 0] <- NA
				ret
			})
			names(pars.mon) <- c('prob', 'scale', 'shape')
			pars.mon
		})
		# }
		# if(closeklust) stopCluster(klust)

		################

		default <- apply(do.call('rbind', bias.pars$pars.rfe), 2, median, na.rm = TRUE)
		PARS <- vector(mode = 'list', length = 12)
		PARS[months] <- lapply(months, function(m){
			par.stn <- PARS.stn[[m]]
			par.rfe <- PARS.rfe[[m]]
			prob <- scale <- shape <- rep(NA, length(ijGrd)+length(ijInt))
			# prob <- scale <- shape <- matrix(NA, ncol = nlat0, nrow = nlon0)
			if(!is.null(par.stn) & !is.null(par.rfe)){
				if(!is.null(par.stn$prob) & !is.null(par.stn$scale) & !is.null(par.stn$shape) &
					!is.null(par.rfe$prob) & !is.null(par.rfe$scale) & !is.null(par.rfe$shape)){
					ina <- is.na(par.stn$prob) | is.na(par.stn$scale) | is.na(par.stn$shape) |
							is.na(par.rfe$prob) | is.na(par.rfe$scale) | is.na(par.rfe$shape)
					mprob <- median(par.rfe$prob, na.rm = TRUE)
					mscale <- median(par.rfe$scale, na.rm = TRUE)
					mshape <- median(par.rfe$shape, na.rm = TRUE)
					par.stn$prob[ina] <- mprob
					par.stn$scale[ina] <- mscale
					par.stn$shape[ina] <- mshape
					par.rfe$prob[ina] <- mprob
					par.rfe$scale[ina] <- mscale
					par.rfe$shape[ina] <- mshape
					ret.stn <- list(prob = par.stn$prob, scale = par.stn$scale, shape = par.stn$shape)
					ret.rfe <- list(prob = par.rfe$prob, scale = par.rfe$scale, shape = par.rfe$shape)
				}else{
					prob[] <- default['prob']
					scale[] <- default['scale']
					shape[] <- default['shape']
					ret.stn <- ret.rfe <- list(prob = prob, scale = scale, shape = shape)
				}
				################
			}else{
				prob[] <- default['prob']
				scale[] <- default['scale']
				shape[] <- default['shape']
				ret.stn <- ret.rfe <- list(prob = prob, scale = scale, shape = shape)
			}
			list(stn = ret.stn, rfe = ret.rfe)
		})

		for(jfl in months){
			outfl1 <- file.path(origdir, paste('Bernoulli-Gamma_Pars.STN', '_', jfl, '.txt', sep = ''))
			out.dat1 <- data.frame(prob = PARS[[jfl]]$stn$prob, scale = PARS[[jfl]]$stn$scale, shape = PARS[[jfl]]$stn$shape)
			write.table(round(out.dat1, 3), outfl1, col.names = TRUE, row.names = FALSE, quote = FALSE)
		}

		for(jfl in months){
			outfl2 <- file.path(origdir, paste('Bernoulli-Gamma_Pars.RFE', '_', jfl, '.txt', sep = ''))
			out.dat2 <- data.frame(prob = PARS[[jfl]]$rfe$prob, scale = PARS[[jfl]]$rfe$scale, shape = PARS[[jfl]]$rfe$shape)
			write.table(round(out.dat2, 3), outfl2, col.names = TRUE, row.names = FALSE, quote = FALSE)
		}

		# #Defines netcdf output
		# grd.prob <- ncvar_def("prob", "", xy.dim, NA, longname = "Probability of non-zero event Bernoulli-Gamma distribution", prec = "float")
		# grd.scale <- ncvar_def("scale", "", xy.dim, NA, longname = "Scale parameters of the gamma distribution", prec = "float")
		# grd.shape <- ncvar_def("shape", "", xy.dim, NA, longname = "Shape parameters of the gamma distribution", prec = "float")

		# for(jfl in months){
		# 	outnc1 <- file.path(origdir, paste('Bernoulli-Gamma_Pars.STN', '_', jfl, '.nc', sep = ''))
		# 	nc1 <- nc_create(outnc1, list(grd.prob, grd.scale, grd.shape))
		# 	ncvar_put(nc1, grd.prob, PARS[[jfl]]$stn$prob)
		# 	ncvar_put(nc1, grd.scale, PARS[[jfl]]$stn$scale)
		# 	ncvar_put(nc1, grd.shape, PARS[[jfl]]$stn$shape)
		# 	nc_close(nc1)
		# }

		# for(jfl in months){
		# 	outnc2 <- file.path(origdir, paste('Bernoulli-Gamma_Pars.RFE', '_', jfl, '.nc', sep = ''))
		# 	nc2 <- nc_create(outnc2, list(grd.prob, grd.scale, grd.shape))
		# 	ncvar_put(nc2, grd.prob, PARS[[jfl]]$rfe$prob)
		# 	ncvar_put(nc2, grd.scale, PARS[[jfl]]$rfe$scale)
		# 	ncvar_put(nc2, grd.shape, PARS[[jfl]]$rfe$shape)
		# 	nc_close(nc2)
		# }
		rm(PARS.stn, PARS.rfe, PARS)
	}

	rm(stnData, demData, demGrid, ObjStn, interp.grid)
	gc()
	InsertMessagesTxt(main.txt.out, 'Interpolating bias factors finished')
	return(0)
}

########################################################################################################

AjdMeanBiasRain.validation <- function(adjMeanBiasparms){
	GeneralParameters <- adjMeanBiasparms$GeneralParameters
	origdir <- adjMeanBiasparms$origdir

	freqData <- GeneralParameters$period
	bias.method <- GeneralParameters$Bias.Method
	biasDir <- GeneralParameters$IO.files$Bias.dir
	meanBiasPrefix <- GeneralParameters$Prefix$Mean.Bias.Prefix
	adjRfeFF <- GeneralParameters$Prefix$Adj.File.Format
	months <- sort(as.numeric(GeneralParameters$Adjust.Months))
	adjZero <- GeneralParameters$Adjusted.to.Zero

	ijInt <- adjMeanBiasparms$ijInt
	memType <- adjMeanBiasparms$memType

	###############
	if(adjMeanBiasparms$readRFE){
		if(memType == 2){
			rfeData <- read.NetCDF.Data(adjMeanBiasparms$rfeData)
			if(is.null(rfeData)) return(NULL)

			irfe <- !sapply(rfeData$data, is.null)
			if(!any(irfe)){
				InsertMessagesTxt(main.txt.out, "All RFE data are missing", format = TRUE)
				return(NULL)
			}

			rfeData$dates <- rfeData$dates[irfe]
			rfeData$data <- rfeData$data[irfe]
		}else{
			ncfiles <- adjMeanBiasparms$rfeData$ncfiles
			ncInfo <- ncFilesInfo(ncfiles$freqData, ncfiles$start.date, ncfiles$end.date, ncfiles$months,
								ncfiles$ncDir, ncfiles$ncFileFormat, adjMeanBiasparms$rfeData$errmsg)
			if(is.null(ncInfo)) return(NULL)

			ncInfo$nc.files <- ncInfo$nc.files[ncInfo$exist]
			ncInfo$dates <- ncInfo$dates[ncInfo$exist]

			nc <- nc_open(ncInfo$nc.files[1])
			rlon <- nc$dim[[adjMeanBiasparms$rfeData$ncinfo$xo]]$vals
			rlat <- nc$dim[[adjMeanBiasparms$rfeData$ncinfo$yo]]$vals
			nc_close(nc)
			xo <- order(rlon)
			rlon <- rlon[xo]
			yo <- order(rlat)
			rlat <- rlat[yo]
			rfeData <- list(lon = rlon, lat = rlat, dates = ncInfo$dates, files = ncInfo$nc.files,
							xo = xo, yo = yo, varid = adjMeanBiasparms$rfeData$ncinfo$varid,
							yorder = adjMeanBiasparms$rfeData$ncinfo$yo)
		}
	}else rfeData <- adjMeanBiasparms$RFEDATA
	# rfeSp <- defSpatialPixels(list(lon = rfeData$lon, lat = rfeData$lat))

	###############
	InsertMessagesTxt(main.txt.out, 'Correct RFE Bias ...')

	if(bias.method == "Multiplicative.Bias.Mon"){
		biasFile <- file.path(biasDir, paste(meanBiasPrefix, '_', months, '.txt', sep = ''))
		exist.bias <- unlist(lapply(biasFile, file.exists))
		if(any(!exist.bias)){
			miss.bias <- months[!exist.bias]
			for(j in seq_along(miss.bias)){
				msg <- paste(meanBiasPrefix, '_', miss.bias[j], '.txt', sep = '')
				InsertMessagesTxt(main.txt.out, paste(msg, "doesn't exist"), format = TRUE)
			}
			return(NULL)
		}
		# nc <- nc_open(biasFile[which(exist.bias)[1]])
		# lon <- nc$dim[[1]]$vals
		# lat <- nc$dim[[2]]$vals
		# nc_close(nc)
		BIAS <- vector(mode = 'list', length = 12)
		BIAS[months] <- lapply(seq_along(months), function(m){
			xvar <- read.table(biasFile[m])[, 1]
			# nc <- nc_open(biasFile[m])
			# xvar <- ncvar_get(nc, varid = "bias")
			# nc_close(nc)
			xvar
		})
		# toExports <- c('lon', 'lat', 'BIAS')
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
			
		biasFile <- file.path(biasDir, paste(meanBiasPrefix, '_', times.stn, '.txt', sep = ''))
		exist.bias <- unlist(lapply(biasFile, file.exists))

		if(any(!exist.bias)){
			miss.bias <- times.stn[!exist.bias]
			for(j in seq_along(miss.bias)){
				msg <- paste(meanBiasPrefix, '_', miss.bias[j], '.txt', sep = '')
				InsertMessagesTxt(main.txt.out, paste(msg, "doesn't exist"), format = TRUE)
			}
			return(NULL)
		}
		# nc <- nc_open(biasFile[which(exist.bias)[1]])
		# lon <- nc$dim[[1]]$vals
		# lat <- nc$dim[[2]]$vals
		# nc_close(nc)
		BIAS[times.stn] <- lapply(seq_along(times.stn), function(m){
			xvar <- read.table(biasFile[m])[, 1]
			# nc <- nc_open(biasFile[m])
			# xvar <- ncvar_get(nc, varid = nc$var[[1]]$name) #varid = "bias"
			# nc_close(nc)
			xvar
		})
		# toExports <- c('lon', 'lat', 'BIAS')
	}

	if(bias.method == "Quantile.Mapping"){
		pars.stnFile <- file.path(biasDir, paste('Bernoulli-Gamma_Pars.STN', '_', months, '.txt', sep = ''))
		exist.pars.stn <- unlist(lapply(pars.stnFile, file.exists))
		if(any(!exist.pars.stn)){
			miss.pars.stn <- months[!exist.pars.stn]
			for(j in seq_along(miss.pars.stn)){
				msg <- paste('Bernoulli-Gamma_Pars.STN', '_', miss.pars.stn[j], '.txt', sep = '')
				InsertMessagesTxt(main.txt.out, paste(msg, "doesn't exist"), format = TRUE)
			}
			return(NULL)
		}

		pars.rfeFile <- file.path(biasDir, paste('Bernoulli-Gamma_Pars.RFE', '_', months, '.txt', sep = ''))
		exist.pars.rfe <- unlist(lapply(pars.rfeFile, file.exists))
		if(any(!exist.pars.rfe)){
			miss.pars.rfe <- months[!exist.pars.rfe]
			for(j in seq_along(miss.pars.rfe)){
				msg <- paste('Bernoulli-Gamma_Pars.RFE', '_', miss.pars.rfe[j], '.txt', sep = '')
				InsertMessagesTxt(main.txt.out, paste(msg, "doesn't exist"), format = TRUE)
			}
			return(NULL)
		}

		# nc <- nc_open(pars.stnFile[which(exist.pars.stn)[1]])
		# lon <- nc$dim[[1]]$vals
		# lat <- nc$dim[[2]]$vals
		# nc_close(nc)

		PARS.stn <- vector(mode = 'list', length = 12)
		PARS.rfe <- vector(mode = 'list', length = 12)
		PARS.stn[months] <- lapply(seq_along(months), function(m){
			pars <- read.table(pars.stnFile[m], header = TRUE)
			prob <- pars$prob
			scale <- pars$scale
			shape <- pars$shape
			# nc <- nc_open(pars.stnFile[m])
			# prob <- ncvar_get(nc, varid = "prob")
			# scale <- ncvar_get(nc, varid = "scale")
			# shape <- ncvar_get(nc, varid = "shape")
			# nc_close(nc)
			list(prob = prob, scale = scale, shape = shape)
		})

		PARS.rfe[months] <- lapply(seq_along(months), function(m){
			pars <- read.table(pars.rfeFile[m], header = TRUE)
			prob <- pars$prob
			scale <- pars$scale
			shape <- pars$shape
			# nc <- nc_open(pars.rfeFile[m])
			# prob <- ncvar_get(nc, varid = "prob")
			# scale <- ncvar_get(nc, varid = "scale")
			# shape <- ncvar_get(nc, varid = "shape")
			# nc_close(nc)
			list(prob = prob, scale = scale, shape = shape)
		})
		# toExports <- c('quantile.mapping.BGamma', 'lon', 'lat', 'PARS.stn', 'PARS.rfe', 'adjZero')
	}

	########
	## RFE regrid?
	# biasSp <- defSpatialPixels(list(lon = lon, lat = lat))
	# is.regridRFE <- is.diffSpatialPixelsObj(biasSp, rfeSp, tol = 1e-07)
	
	########
	#Defines netcdf output dims
	# dx <- ncdim_def("Lon", "degreeE", lon)
	# dy <- ncdim_def("Lat", "degreeN", lat)
	# xy.dim <- list(dx, dy)
	# grd.bsadj <- ncvar_def("precip", "mm", xy.dim, -99, longname= "Bias Corrected RFE", prec = "short")

	########

	# if(doparallel & length(rfeData$dates) >= 30){
	# 	klust <- makeCluster(nb_cores)
	# 	registerDoParallel(klust)
	# 	`%parLoop%` <- `%dopar%`
	# 	closeklust <- TRUE
	# }else{
	# 	`%parLoop%` <- `%do%`
	# 	closeklust <- FALSE
	# }

	# packages <- c('ncdf4', 'fields')
	# toExports <- c(toExports, 'rfeData', 'is.regridRFE', 'bias.method', 'origdir',
	# 				 'grd.bsadj', 'freqData', 'adjRfeFF', 'memType')

	# ret <- foreach(jfl = seq_along(rfeData$dates), .packages = packages, .export = toExports) %parLoop% {
	ret <- lapply(seq_along(rfeData$dates), function(jfl){
		if(memType == 2){
			xrfe <- rfeData$data[[jfl]]
			drfe <- rfeData$dates[jfl]
		}else{
			nc <- nc_open(rfeData$files[jfl])
			xrfe <- ncvar_get(nc, varid = rfeData$varid)
			nc_close(nc)
			if(rfeData$yorder == 1){
				xrfe <- matrix(c(xrfe), nrow = length(rfeData$lon), ncol = length(rfeData$lat), byrow = TRUE)
			}
			xrfe <- xrfe[rfeData$xo, rfeData$yo]
			drfe <- rfeData$dates[jfl]
		}

		xrfe <- xrfe[ijInt]
		# if(is.regridRFE){
		# 	rfeGrid <- interp.surface.grid(list(x = rfeData$lon, y = rfeData$lat, z = xrfe), list(x = lon, y = lat))
		# 	xrfe <- rfeGrid$z
		# }

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
		# xadj[is.na(xadj)] <- -99

		############
		if(freqData == 'daily'){
			outfl <- file.path(origdir, sprintf(adjRfeFF, substr(drfe, 1, 4), substr(drfe, 5, 6), substr(drfe, 7, 8)))
		}else  if(freqData == 'dekadal'){
			outfl <- file.path(origdir, sprintf(adjRfeFF, substr(drfe, 1, 4), substr(drfe, 5, 6), substr(drfe, 7, 7)))
		}else  if(freqData == 'monthly'){
			outfl <- file.path(origdir, sprintf(adjRfeFF, substr(drfe, 1, 4), substr(drfe, 5, 6)))
		}
		#Save adjusted data
		write.table(round(xadj, 1), outfl, col.names = FALSE, row.names = FALSE)
		# nc2 <- nc_create(outfl, grd.bsadj)
		# ncvar_put(nc2, grd.bsadj, xadj)
		# nc_close(nc2)
		return(0)
	})
	# }
	# if(closeklust) stopCluster(klust)
	rm(rfeData)
	gc()
	InsertMessagesTxt(main.txt.out, 'Bias Correction finished')
	return(0)
}

########################################################################################################

ComputeLMCoefRain.validation <- function(comptLMparams){
	GeneralParameters <- comptLMparams$GeneralParameters
	min.len <- as.numeric(GeneralParameters$LM.Params$min.length)
	min.stn <- as.numeric(GeneralParameters$LM.Params$min.stn)
	interp.method <- GeneralParameters$Interpolation.pars$interp.method
	nmin <- as.numeric(GeneralParameters$Interpolation.pars$nmin)
	nmax <- as.numeric(GeneralParameters$Interpolation.pars$nmax)
	vgm.model <- as.character(GeneralParameters$Interpolation.pars$vgm.model[[1]])

	# use.block <- GeneralParameters$Interpolation.pars$use.block

	# maxdist <- as.numeric(GeneralParameters$Interpolation.pars$maxdist)
	# rad.lon <- as.numeric(GeneralParameters$Interpolation.pars$rad.lon)
	# rad.lat <- as.numeric(GeneralParameters$Interpolation.pars$rad.lat)
	# rad.elv <- as.numeric(GeneralParameters$Interpolation.pars$rad.elv)
	# as.dim.elv <- GeneralParameters$Interpolation.pars$elev.3rd.dim
	# latlong <- GeneralParameters$Interpolation.pars$latlon.unit
	# normalize <- GeneralParameters$Interpolation.pars$normalize

	months <- sort(as.numeric(GeneralParameters$LM.Months))
	year1 <- as.numeric(GeneralParameters$LM.Date.Range$start.year)
	year2 <- as.numeric(GeneralParameters$LM.Date.Range$end.year)

	origdir <- comptLMparams$origdir

	# res.coarse <- as.numeric(GeneralParameters$Interpolation.pars$res.coarse)
	# res.coarse <- comptLMparams$res.coarse

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
	# lon.stn <- stnData$lon
	# lat.stn <- stnData$lat
	date.stn <- stnData$dates
	data.stn <- stnData$data

	ijInt <- comptLMparams$ijInt

	#############
	adjInfo <- comptLMparams$adjInfo
	adjData <- lapply(seq_along(adjInfo$nc.files), function(j){
		if(adjInfo$exist[j]) read.table(adjInfo$nc.files[j])[, 1]
		else NULL
	})

	data.rfe.stn <- t(sapply(adjData, function(x){
		if(!is.null(x)) x
		else rep(NA, length(stnData$lon)+length(ijInt))
	}))

	date.rfe <- comptLMparams$adjInfo$dates
	data.rfe.stn <- data.rfe.stn[, seq(length(stnData$lon)), drop = FALSE]

	#############
	# if(comptLMparams$memType == 2){
	# 	# read then extract
	# 	rfeData <- read.NetCDF.Data(comptLMparams$rfeData)
	# 	if(is.null(rfeData)) return(NULL)
	# 	ijrfe <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = rfeData$lon, lat = rfeData$lat))
	# 	data.rfe.stn  <- t(sapply(rfeData$data, function(x){
	# 		if(!is.null(x)) x[ijrfe]
	# 		else rep(NA, length(ijrfe))
	# 	}))
	# }else{
	# 	# read and extract
	# 	rfeData <- read.NetCDF.Data2Points(comptLMparams$rfeData, list(lon = lon.stn, lat = lat.stn))
	# 	if(is.null(rfeData)) return(NULL)
	# 	data.rfe.stn <- t(sapply(rfeData$data, function(x) if(!is.null(x)) x else rep(NA, length(lon.stn))))
	# }
	# date.rfe <- rfeData$dates

	#############
	# xy.grid <- comptLMparams$xy.grid
	# grdSp <- defSpatialPixels(xy.grid)
	# nlon0 <- length(xy.grid$lon)
	# nlat0 <- length(xy.grid$lat)

	# #Defines netcdf output dims
	# dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	# dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	# xy.dim <- list(dx, dy)

	#############
	# demData <- comptLMparams$demData
	# lon.dem <- demData$lon
	# lat.dem <- demData$lat
	# grd.dem <- demData$demMat
	# grd.dem[grd.dem < 0] <- 0
	# demSp <- defSpatialPixels(list(lon = lon.dem, lat = lat.dem))
	# is.regridDEM <- is.diffSpatialPixelsObj(grdSp, demSp, tol = 1e-07)

	# demGrid <- list(x = lon.dem, y = lat.dem, z = grd.dem)
	# if(is.regridDEM){
	# 	demGrid <- interp.surface.grid(demGrid, list(x = xy.grid$lon, y = xy.grid$lat))
	# }

	# demres <- grdSp@grid@cellsize
	# slpasp <- slope.aspect(demGrid$z, demres[1], demres[2], filter = "sobel")
	# demGrid$slp <- slpasp$slope
	# demGrid$asp <- slpasp$aspect

	# #############
	# ijdem <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), xy.grid)
	# ObjStn <- list(x = lon.stn, y = lat.stn, z = demGrid$z[ijdem], slp = demGrid$slp[ijdem], asp = demGrid$asp[ijdem])

	# ## create grid to interp
	# bGrd <- NULL
	# if(interp.method == 'NN'){
	# 	interp.grid <- createGrid(ObjStn, demGrid, as.dim.elv = as.dim.elv, latlong = latlong,
	# 						normalize = normalize, coarse.grid = TRUE, res.coarse = res.coarse)
	# 	xy.maxdist <- sqrt(sum((c(rad.lon, rad.lat)*(apply(coordinates(interp.grid$newgrid)[, c('lon', 'lat')], 2,
	# 					function(x) diff(range(x)))/(c(nlon0, nlat0)-1)))^2))
	# 	elv.grd <- range(demGrid$z, na.rm = TRUE)
	# 	nelv <- length(seq(elv.grd[1], elv.grd[2], 100))
	# 	nelv <- if(nelv > 1) nelv else 2
	# 	z.maxdist <- rad.elv*(diff(range(coordinates(interp.grid$newgrid)[, 'elv']))/(nelv-1))
	# 	xy.maxdist <- if(xy.maxdist < res.coarse) res.coarse else xy.maxdist
	# 	maxdist <- sqrt(xy.maxdist^2 + z.maxdist^2)
	# }else{
	# 	interp.grid <- createGrid(ObjStn, demGrid, as.dim.elv = FALSE, res.coarse = res.coarse)
	# 	maxdist <- if(maxdist < res.coarse) res.coarse else maxdist
	# 	cells <- SpatialPixels(points = interp.grid$newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)))@grid
	# 	bGrd <- createBlock(cells@cellsize, 2, 5)
	# }

	gridObj <- comptLMparams$gridObj
	if(interp.method == 'NN'){
		interp.grid <- gridObj$NN$interp.grid
		maxdist <- gridObj$NN$maxdist
		xy.maxdist <- gridObj$NN$xy.maxdist
		z.maxdist <- gridObj$NN$z.maxdist
	}else{
		interp.grid <- gridObj$IDWOK$interp.grid
		maxdist <- gridObj$IDWOK$maxdist
		bGrd <- gridObj$IDWOK$bGrd
	}

	#############
	# InsertMessagesTxt(main.txt.out, 'Compute LM Coefficients ...')

	dtrfe <- date.rfe%in%date.stn
	dtstn <- date.stn%in%date.rfe
	data.stn <- data.stn[dtstn, , drop = FALSE]
	date.stn <- date.stn[dtstn]
	data.rfe.stn <- data.rfe.stn[dtrfe, , drop = FALSE]
	# date.rfe <- date.rfe[dtrfe]

	month.stn <- as(substr(date.stn, 5, 6), 'numeric')
	year.stn <- as(substr(date.stn, 1, 4), 'numeric')
	iyear0 <- (year.stn >= year1 & year.stn <= year2) & (month.stn%in%months)
	data.stn.reg <- data.stn[iyear0, ]
	mon.stn.reg <- month.stn[iyear0]

	##############
	# dataf <- data.frame(id.stn = rep(id.stn, each = nrow(data.stn.reg)), month = rep(mon.stn.reg, ncol(data.stn.reg)),
	# 					stn = c(data.stn.reg), rfe = c(data.rfe.stn))
	# model <- by(dataf, dataf$id.stn, fitLM.month.RR, min.len)

	# xmod <- lapply(model, function(x){
	# 	sapply(x, function(m){
	# 		if(is.null(m)) c(NA, NA, NA, NA, NA)
	# 		else{
	# 			smod <- summary(m)
	# 			xcoef <- coefficients(m)
	# 			if(is.null(smod$fstatistic)) c(xcoef, NA, NA, NA)
	# 			else c(xcoef, pf(smod$fstatistic[1], smod$fstatistic[2], smod$fstatistic[3], lower.tail = FALSE), smod$r.squared, smod$adj.r.squared)
	# 		}
	# 	})
	# })

	# model.coef <- list(slope = sapply(xmod, function(x) x[2, ]),
	# 				   intercept = sapply(xmod, function(x) x[1, ]),
	# 				   pvalue = sapply(xmod, function(x) x[3, ]),
	# 				   rsquared = sapply(xmod, function(x) x[4, ]),
	# 				   adj.rsquared = sapply(xmod, function(x) x[5, ]))
	# nommodcoef <- names(model.coef)

	##############

	data.stn.reg <- data.frame(data.stn.reg)
	names(data.stn.reg) <- id.stn
	data.rfe.stn <- data.frame(data.rfe.stn)
	names(data.rfe.stn) <- id.stn

	model <- lapply(months, function(m){
		ix <- which(mon.stn.reg == m)
		mapply(function(rfe, stn){
			ina <- !is.na(stn) & !is.na(rfe)
			rfe <- rfe[ina]
			stn <- stn[ina]
			if(length(stn) >= min.len) lm(stn~rfe)
		}, data.rfe.stn[ix, , drop = FALSE], data.stn.reg[ix, , drop = FALSE],
			SIMPLIFY = FALSE, USE.NAMES = TRUE)
	})

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

	model.coef <- list(slope = t(sapply(xmod, function(x) x[2, ])),
					   intercept = t(sapply(xmod, function(x) x[1, ])),
					   pvalue = t(sapply(xmod, function(x) x[3, ])),
					   rsquared = t(sapply(xmod, function(x) x[4, ])),
					   adj.rsquared = t(sapply(xmod, function(x) x[5, ])))
	nommodcoef <- names(model.coef)



	##############
	# coef0 <- model.coef

	islp <- !is.na(model.coef$slope) & model.coef$slope > 0
	model.coef$slope[!islp] <- NA
	extrm <- t(apply(model.coef$slope, 1, quantile, prob = c(0.001, 0.99), na.rm = TRUE))
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
	# model.params <- list(model = model, coef0 = coef0, coef = model.coef, id.stn = id.stn,
	# 					lon.stn = lon.stn, lat.stn = lat.stn, date.stn = date.stn[iyear0],
	# 					data.stn = data.stn.reg, data.rfe = data.rfe.stn)
	# save(model.params, file = file.path(origdir, "LM_MODEL_PARS.RData"))

	# InsertMessagesTxt(main.txt.out, 'Computing LM Coefficients finished')

	##########
	# InsertMessagesTxt(main.txt.out, 'Interpolate LM Coefficients ...')

	# if(doparallel & length(months) >= 3){
	# 	klust <- makeCluster(nb_cores)
	# 	registerDoParallel(klust)
	# 	`%parLoop%` <- `%dopar%`
	# 	closeklust <- TRUE
	# }else{
	# 	`%parLoop%` <- `%do%`
	# 	closeklust <- FALSE
	# }
	# packages <- c('sp', 'gstat', 'automap')
	# toExports <- c('model.coef', 'months', 'interp.grid', 'interp.method', 'min.stn','formule',
	# 				'auxvar', 'is.auxvar', 'vgm.model', 'nmin', 'nmax', 'maxdist', 'bGrd', 'nlat0', 'nlon0')
	MODEL.COEF <- vector(mode = 'list', length = 12)
	# MODEL.COEF[months] <- foreach(m = months, .packages = packages, .export = toExports) %parLoop% {
	MODEL.COEF[months] <- lapply(months, function(m){
		pars.mon <- lapply(1:2, function(jc){
			locations.stn <- interp.grid$coords.stn
			xcoef <- model.coef[[jc]]
			# locations.stn$pars <- xcoef[as.numeric(rownames(xcoef)) == m, ]
			locations.stn$pars <- xcoef[months == m, ]
			locations.stn <- locations.stn[!is.na(locations.stn$pars), ]
			if(length(locations.stn$pars) < min.stn) return(NULL)

			if(any(is.auxvar) & interp.method != 'NN') locations.stn <- locations.stn[Reduce("&", as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))), ]
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
					locations.stn <- locations.stn[Reduce("&", as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))), ]
					block <- NULL 
				}else block <- bGrd
				pars.grd <- krige(formule, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
									block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)

				extrm <- c(min(locations.stn$pars, na.rm = TRUE), max(locations.stn$pars, na.rm = TRUE))
				ixtrm <- is.na(pars.grd$var1.pred) | (pars.grd$var1.pred <= extrm[1] | pars.grd$var1.pred >= extrm[2])
				pars.grd$var1.pred[ixtrm] <- NA
				# ina <- is.na(pars.grd$var1.pred)
				# if(any(ina)){
				# 	pars.grd.na <- krige(var1.pred~1, locations = pars.grd[!ina, ], newdata = interp.grid$newgrid[ina, ], model = vgm,
				# 						block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				# 	pars.grd$var1.pred[ina] <- pars.grd.na$var1.pred
				# }
			}
			# matrix(pars.grd$var1.pred, ncol = nlat0, nrow = nlon0)
			round(pars.grd$var1.pred, 5)
		})
		names(pars.mon) <- c('slope', 'intercept')
		pars.mon
	})
	# }
	# if(closeklust) stopCluster(klust)

	# InsertMessagesTxt(main.txt.out, 'Interpolating LM Coefficients finished')

	###########
	# grd.slope <- ncvar_def("slope", "", xy.dim, NA, longname= "Linear model Coef: Slope", prec = "float")
	# grd.intercept <- ncvar_def("intercept", "", xy.dim, NA, longname= "Linear model Coef: Intercept", prec = "float")

	for(jfl in months){
		if(is.null(MODEL.COEF[[jfl]]$slope) | is.null(MODEL.COEF[[jfl]]$intercept)){
			# MODEL.COEF[[jfl]]$slope <- matrix(1, ncol = nlat0, nrow = nlon0)
			# MODEL.COEF[[jfl]]$intercept <- matrix(0, ncol = nlat0, nrow = nlon0)
			MODEL.COEF[[jfl]]$slope <- rep(1, length(stnData$lon)+length(ijInt))
			MODEL.COEF[[jfl]]$intercept <- rep(0, length(stnData$lon)+length(ijInt))
		}
		ina <- is.na(MODEL.COEF[[jfl]]$slope) | is.na(MODEL.COEF[[jfl]]$intercept)
		MODEL.COEF[[jfl]]$slope[ina] <- 1
		MODEL.COEF[[jfl]]$intercept[ina] <- 0

		xneg <- MODEL.COEF[[jfl]]$slope <= 0
		MODEL.COEF[[jfl]]$slope[xneg] <- 1
		MODEL.COEF[[jfl]]$intercept[xneg] <- 0

		out.coef <- cbind(MODEL.COEF[[jfl]]$slope, MODEL.COEF[[jfl]]$intercept)
		outfl <- file.path(origdir, paste('LM_Coefficient', '_', jfl, '.txt', sep = ''))
		write.table(out.coef, outfl, col.names = FALSE, row.names = FALSE, quote = FALSE)

		# outnc1 <- file.path(origdir, paste('LM_Coefficient', '_', jfl, '.nc', sep = ''))
		# nc1 <- nc_create(outnc1, list(grd.slope, grd.intercept))
		# ncvar_put(nc1, grd.slope, MODEL.COEF[[jfl]]$slope)
		# ncvar_put(nc1, grd.intercept, MODEL.COEF[[jfl]]$intercept)
		# nc_close(nc1)
	}

	rm(adjData, stnData, demData, data.rfe.stn, grd.dem,
		data.stn, demGrid, interp.grid, data.stn.reg,
		model, model.coef, MODEL.COEF, coef0, xmod, model.params)
	return(0)
}

########################################################################################################


MergingFunctionRain.validation <- function(paramsMRG){
	# InsertMessagesTxt(main.txt.out, 'Merging data ...')

	GeneralParameters <- paramsMRG$GeneralParameters
	# freqData <- GeneralParameters$period

	# rfeDir <- GeneralParameters$IO.files$RFE.dir
	# rfefilefrmt <- GeneralParameters$FileFormat$RFE.File.Format
	# months <- GeneralParameters$Mrg.Months
	# daty <- GeneralParameters$Mrg.Date.Range
	# start.date <- as.Date(paste(daty$start.year, daty$start.mon, daty$start.dek, sep = '-'))
	# end.date <- as.Date(paste(daty$end.year, daty$end.mon, daty$end.dek, sep = '-'))
	# error.msg <- "RFE data not found"
	# ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, rfeDir, rfefilefrmt, error.msg)
	# if(is.null(ncInfo)) return(NULL)
	adjInfo <- paramsMRG$adjInfo

	#############
	# if(doparallel & length(which(ncInfo$exist)) >= 10){
	# 	klust <- makeCluster(nb_cores)
	# 	registerDoParallel(klust)
	# 	`%parLoop%` <- `%dopar%`
	# 	closeklust <- TRUE
	# }else{
	# 	`%parLoop%` <- `%do%`
	# 	closeklust <- FALSE
	# }

	#############
	freqData <- GeneralParameters$period

	# xy.grid <- paramsMRG$xy.grid
	# grdSp <- defSpatialPixels(xy.grid)
	# nlon0 <- length(xy.grid$lon)
	# nlat0 <- length(xy.grid$lat)

	#############
	## Def ncdf
	# dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	# dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	# grd.nc.out <- ncvar_def("precip", "mm", list(dx, dy), -99, longname = "Merged Station-Satellite Rainfall", prec = "short")

	#############
	stnData <- paramsMRG$stnData
	# id.stn <- stnData$id
	# lon.stn <- stnData$lon
	# lat.stn <- stnData$lat
	date.stn <- stnData$dates
	data.stn <- stnData$data

	#############
	# rfeDataInfo <- paramsMRG$rfeDataInfo
	# lon.rfe <- rfeDataInfo$lon
	# lat.rfe <- rfeDataInfo$lat
	# rfeSp <- defSpatialPixels(list(lon = lon.rfe, lat = lat.rfe))
	# is.regridRFE <- is.diffSpatialPixelsObj(grdSp, rfeSp, tol = 1e-07)

	#############
	interp.method <- GeneralParameters$Interpolation.pars$interp.method
	nmin <- GeneralParameters$Interpolation.pars$nmin
	nmax <- GeneralParameters$Interpolation.pars$nmax
	maxdist <- GeneralParameters$Interpolation.pars$maxdist
	vgm.model <- GeneralParameters$Interpolation.pars$vgm.model[[1]]
	# use.block <- GeneralParameters$Interpolation.pars$use.block
	# res.coarse <- GeneralParameters$Interpolation.pars$res.coarse
	# res.coarse <- maxdist/2
	# res.coarse <- if(res.coarse  >= 0.25) res.coarse else 0.25
	# maxdist <- if(maxdist < res.coarse) res.coarse else maxdist

	#############
	Mrg.Method <- GeneralParameters$Mrg.Method
	min.stn <- GeneralParameters$Mrg.set$min.stn
	min.non.zero <- GeneralParameters$Mrg.set$min.non.zero

 	use.RnoR <- GeneralParameters$Mrg.set$use.RnoR
 	smooth.RnoR <- GeneralParameters$Mrg.set$smooth.RnoR
	wet.day <- GeneralParameters$Mrg.set$wet.day
	# maxdist.RnoR <- GeneralParameters$Mrg.set$maxdist.RnoR
	# maxdist.RnoR <- if(maxdist.RnoR < res.coarse) res.coarse else maxdist.RnoR

	# outMask <- paramsMRG$outMask
	origdir <- paramsMRG$origdir
	# Mrg.file.format <- GeneralParameters$FileFormat$Mrg.file.format

	#############
	auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
	is.auxvar <- c(GeneralParameters$auxvar$dem, GeneralParameters$auxvar$slope,
					GeneralParameters$auxvar$aspect, GeneralParameters$auxvar$lon,
					GeneralParameters$auxvar$lat)
	if(any(is.auxvar)){
		formule <- formula(paste('res', '~', paste(auxvar[is.auxvar], collapse = '+'), sep = ''))
		if(Mrg.Method == "Regression Kriging"){
			sp.trend.aux <- GeneralParameters$sp.trend.aux
			if(sp.trend.aux) formuleRK <- formula(paste('stn', '~', 'rfe', '+', paste(auxvar[is.auxvar], collapse = '+'), sep = ''))
			else formuleRK <- formula(paste('stn', '~', 'rfe', sep = ''))
		}
	}else{
		formule <- formula(paste('res', '~', 1, sep = ''))
		if(Mrg.Method == "Regression Kriging") formuleRK <- formula(paste('stn', '~', 'rfe', sep = ''))
	}

	#############

	# demData <- paramsMRG$demData
	# if(!is.null(demData)){
	# 	demData$z[demData$z < 0] <- 0
	# 	demres <- grdSp@grid@cellsize
	# 	slpasp <- slope.aspect(demData$z, demres[1], demres[2], filter = "sobel")
	# 	demData$slp <- slpasp$slope
	# 	demData$asp <- slpasp$aspect
	# 	ijdem <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), xy.grid)
	# 	dem.stn.val <- demData$z[ijdem]
	# 	dem.stn.slp <- demData$slp[ijdem]
	# 	dem.stn.asp <- demData$asp[ijdem]
	# }else{
	# 	dem.grd.val <- matrix(1, nrow = nlon0, ncol = nlat0)
	# 	dem.grd.slp <- matrix(0, nrow = nlon0, ncol = nlat0)
	# 	dem.grd.asp <- matrix(0, nrow = nlon0, ncol = nlat0)
	# 	demData <- list(x = xy.grid$lon, y = xy.grid$lat, z = dem.grd.val, slp = dem.grd.slp, asp = dem.grd.asp)
	# 	dem.stn.val <- rep(1, length(lon.stn))
	# 	dem.stn.slp <- rep(0, length(lon.stn))
	# 	dem.stn.asp <- rep(0, length(lon.stn))
	# }
	# ObjStn <- list(x = lon.stn, y = lat.stn, z = dem.stn.val, slp = dem.stn.slp, asp = dem.stn.asp)

	## create grid to interp
	# interp.grid <- createGrid(ObjStn, demData, as.dim.elv = FALSE, res.coarse = res.coarse)
	# cells <- SpatialPixels(points = interp.grid$newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)))@grid
	# bGrd <- createBlock(cells@cellsize, 2, 5)

	interp.grid <- paramsMRG$interp.grid
	bGrd <- paramsMRG$bGrd
	ijInt <- paramsMRG$ijInt

	#############
	if(Mrg.Method == "Spatio-Temporal LM"){
		coefFiles <- file.path(GeneralParameters$IO.files$LMCoef.dir, paste('LM_Coefficient_', months, '.txt', sep = ''))
		existLMCfl <- file.exists(coefFiles)
		if(any(!existLMCfl)){
			# for(i in which(!existLMCfl)) InsertMessagesTxt(main.txt.out, paste(coefFiles[i], "doesn't exist"), format = TRUE)
			# if(closeklust) stopCluster(klust)
			unlink(origdir, recursive = TRUE)
			return(NULL)
		}
		MODEL.COEF <- vector(mode = 'list', length = 12)
		for(fl in seq(coefFiles)){
			coeff <- read.table(coefFiles[fl])
			coef1 <- coeff[, 1]
			coef2 <- coeff[, 2]
			# nc <- nc_open(coefFiles[fl])
			# coef1 <- ncvar_get(nc, varid = 'slope')
			# coef2 <- ncvar_get(nc, varid = 'intercept')
			# nc_close(nc)
			MODEL.COEF[[months[fl]]] <- list(slope = coef1, intercept = coef2)
		}
	}

	#############
	# nc <- nc_open(ncInfo$nc.files[which(ncInfo$exist)[1]])
	# xlon <- nc$dim[[rfeDataInfo$rfeILon]]$vals
	# xlat <- nc$dim[[rfeDataInfo$rfeILat]]$vals
	# nc_close(nc)
	# xo <- order(xlon)
	# xlon <- xlon[xo]
	# yo <- order(xlat)
	# xlat <- xlat[yo]

	## gridded data at stn loc
	# ijGrd <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = xlon, lat = xlat))

	#############
	# packages <- c('ncdf4', 'gstat', 'automap')
	# toExports <- c('ncInfo', 'smooth.matrix')

	rfeData <- paramsMRG$rfeData

	adjData <- lapply(seq_along(adjInfo$nc.files), function(j){
		if(adjInfo$exist[j]) read.table(adjInfo$nc.files[j])[, 1]
		else NULL
	})

	adjData <- t(sapply(adjData, function(x){
		if(!is.null(x)) x
		else rep(NA, length(stnData$lon)+length(ijInt))
	}))


	# ncdata <- foreach(jj = seq_along(ncInfo$nc.files), .packages = packages, .export = toExports) %parLoop% {
	mrg.data <- lapply(seq_along(adjInfo$nc.files), function(jj){
		xrfe <- adjData[jj, ]
		if(all(is.na(xrfe))) return(rep(NA, length(xrfe)))
		xrfe.stn <- xrfe[1:length(stnData$lon)]
		xrfe.newgrd <- xrfe
		xrfe.grd <- c(rfeData[[jj]])

		# if(ncInfo$exist[jj]){
		# 	nc <- nc_open(ncInfo$nc.files[jj])
		# 	xrfe <- ncvar_get(nc, varid = rfeDataInfo$rfeVarid)
		# 	nc_close(nc)
		# 	if(rfeDataInfo$rfeILat == 1){
		# 		xrfe <- matrix(c(xrfe), nrow = length(xlon), ncol = length(xlat), byrow = TRUE)
		# 	}
		# 	xrfe <- xrfe[xo, yo]
		# }else return(NULL)

		# ############
		# if(is.regridRFE){
		# 	rfeGrid <- interp.surface.grid(list(x = xlon, y = xlat, z = xrfe), list(x = xy.grid$lon, y = xy.grid$lat))
		# 	xrfe <- rfeGrid$z
		# }

		############
		locations.stn <- interp.grid$coords.stn
		locations.stn$stn <- data.stn[which(date.stn == adjInfo$dates[jj]), ]
		locations.stn$rfe <- xrfe.stn
		locations.stn$res <- NA
		xadd <- as.data.frame(interp.grid$coords.grd)
		xadd$rfe <- xrfe.grd
		# xadd$rfe <- c(xrfe[interp.grid$idxy$ix, interp.grid$idxy$iy])
		xadd$stn <- xadd$rfe
		xadd$res <- 0
		interp.grid$newgrid$rfe <- xrfe.newgrd

		############################################

		noNA <- !is.na(locations.stn$stn)
		min.stn.nonNA <- length(which(noNA))
		nb.stn.nonZero <- length(which(noNA & locations.stn$stn > 0))
		locations.stn <- locations.stn[noNA, ]

		if(min.stn.nonNA >= min.stn){
			do.merging <- TRUE
			if(nb.stn.nonZero >= min.non.zero){
				if(Mrg.Method == "Spatio-Temporal LM"){
					mo <- as(substr(adjInfo$dates[jj], 5, 6), 'numeric')
					sp.trend <- xrfe * MODEL.COEF[[mo]]$slope + MODEL.COEF[[mo]]$intercept
					locations.stn$res <- locations.stn$stn - sp.trend[1:length(stnData$lon)][noNA]
				}else{
					simplediff <- if(var(locations.stn$stn) < 1e-07 | var(locations.stn$rfe, na.rm = TRUE) < 1e-07) TRUE else FALSE
					glm.stn <- glm(formuleRK, data = locations.stn, family = gaussian)
					if(is.na(glm.stn$coefficients[2]) | glm.stn$coefficients[2] < 0) simplediff <- TRUE
					if(!simplediff){
						sp.trend <- predict(glm.stn, newdata = interp.grid$newgrid)
						# sp.trend <- matrix(sp.trend, ncol = nlat0, nrow = nlon0)
						sp.trend[is.na(sp.trend)] <- xrfe.newgrd[is.na(sp.trend)]
						# locations.stn$res <- residuals(glm.stn)
						if(length(glm.stn$na.action) > 0) locations.stn$res[-glm.stn$na.action] <- glm.stn$residuals
						else locations.stn$res <- glm.stn$residuals
					}else{
						sp.trend <- xrfe.newgrd
						locations.stn$res <- locations.stn$stn - locations.stn$rfe
					}
				}
			}else{
				sp.trend <- xrfe.newgrd
				locations.stn$res <- locations.stn$stn - locations.stn$rfe
			}

			locations.stn <- locations.stn[!is.na(locations.stn$res), ]
			# extrm <- quantile(locations.stn$res, probs = c(0.0001, 0.9999))
			# locations.stn <- locations.stn[locations.stn$res > extrm[1] & locations.stn$res < extrm[2], ]
		}else do.merging <- FALSE

		############

		if(do.merging){
			if(use.RnoR){
				rnr.rfe <- ifelse(xrfe.newgrd >= wet.day, 1, 0)
				locations.stn$rnr.stn <- ifelse(locations.stn$stn >= wet.day, 1, 0)
				xadd$rnr.stn <- ifelse(xadd$rfe >= wet.day, 1, 0)

				###########
				### binomial logistic regression
				glm.binom <- glm(rnr.stn ~ rfe, data = locations.stn, family = binomial(link = "logit"))

				############
				xadd$rnr.res <- 0
				locations.stn$rnr.res <- residuals(glm.binom)
				rnr <- predict(glm.binom, newdata = interp.grid$newgrid, type = 'link')
				# rnr <- matrix(rnr, ncol = nlat0, nrow = nlon0)

				coords.grd <- data.frame(coordinates(interp.grid$newgrid))
				irnr <- rep(TRUE, nrow(coords.grd))
			}

			############
			if(any(is.auxvar)) locations.stn <- locations.stn[Reduce("&", as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))), ]
			if(interp.method == 'Kriging'){
				vgm <- try(autofitVariogram(formule, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
				vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
			}else vgm <- NULL

			###########
			coords.grd <- data.frame(coordinates(interp.grid$newgrid))
			irnr <- rep(TRUE, nrow(coords.grd))

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
				block <- NULL
			}else block <- bGrd

			res.grd <- krige(formule, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
								block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)

			extrm <- c(min(locations.stn$res, na.rm = TRUE), max(locations.stn$res, na.rm = TRUE))
			ixtrm <- is.na(res.grd$var1.pred) | (res.grd$var1.pred < extrm[1] | res.grd$var1.pred > extrm[2])
			res.grd$var1.pred[ixtrm] <- 0

			# ina <- is.na(res.grd$var1.pred)
			# if(any(ina)){
			# 	res.grd.na <- krige(var1.pred~1, locations = res.grd[!ina, ], newdata = interp.grid$newgrid[ina, ], model = vgm,
			# 							block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
			# 	res.grd$var1.pred[ina] <- res.grd.na$var1.pred
			# }
			
			resid <- res.grd$var1.pred
			# resid <- matrix(res.grd$var1.pred, ncol = nlat0, nrow = nlon0)
			resid[is.na(resid)] <- 0

			############
			if(use.RnoR){
				rnr.res.grd <- krige(rnr.res~1, locations = locations.stn, newdata = interp.grid$newgrid,
									maxdist = maxdist, block = bGrd,  debug.level = 0)
				# ina <- is.na(rnr.res.grd$var1.pred)
				# if(any(ina)){
				# 	rnr.res.grd.na <- krige(var1.pred~1, locations = rnr.res.grd[!ina, ], newdata = interp.grid$newgrid[ina, ],
				# 							block = bGrd, maxdist = maxdist, debug.level = 0)
				# 	rnr.res.grd$var1.pred[ina] <- rnr.res.grd.na$var1.pred
				# }

				rnr.res.grd <- rnr.res.grd$var1.pred
				# rnr.res.grd <- matrix(rnr.res.grd$var1.pred, ncol = nlat0, nrow = nlon0)
				rnr.res.grd[is.na(rnr.res.grd)] <- 0

				rnr <- rnr + rnr.res.grd
				rnr[rnr > 100] <- 100
				rnr <- exp(rnr)/(1+exp(rnr))

				### decision boundary 0.6
				rnr[rnr >= 0.6] <- 1
				rnr[rnr < 0.6] <- 0

				imsk <- irnr
				# imsk <- matrix(irnr, nrow = nlon0, ncol = nlat0)
				rnr[imsk] <- rnr.rfe[imsk]
				# if(smooth.RnoR){
				# 	npix <- if(sum(rnr.rfe, na.rm = TRUE) == 0) 5 else 3
				# 	rnr <- smooth.matrix(rnr, npix)
				# }
			}else{
				rnr <- rep(1, length(ijInt))
				# rnr <- matrix(1, ncol = nlat0, nrow = nlon0)
			}

			############
			out.mrg <- sp.trend + resid
			out.mrg[out.mrg < 0] <- 0
			out.mrg <- out.mrg * rnr
		}else out.mrg <- xrfe.newgrd

		# #Apply mask for area of interest
		# if(!is.null(outMask)) out.mrg[is.na(outMask)] <- NA
		# out.mrg[is.na(out.mrg)] <- -99

		# ############
		# if(freqData == 'daily'){
		# 	outfl <- file.path(origdir, sprintf(Mrg.file.format, substr(adjInfo$dates[jj], 1, 4),
		# 						substr(adjInfo$dates[jj], 5, 6), substr(adjInfo$dates[jj], 7, 8)))
		# }else if(freqData == 'dekadal'){
		# 	outfl <- file.path(origdir, sprintf(Mrg.file.format, substr(adjInfo$dates[jj], 1, 4),
		# 						substr(adjInfo$dates[jj], 5, 6), substr(adjInfo$dates[jj], 7, 7)))
		# }else if(freqData == 'monthly'){
		# 	outfl <- file.path(origdir, sprintf(Mrg.file.format, substr(adjInfo$dates[jj], 1, 4),
		# 						substr(adjInfo$dates[jj], 5, 6)))
		# }

		# write.table(round(out.mrg, 1), outfl, row.names = FALSE, col.names = FALSE, quote = FALSE)
		# nc2 <- nc_create(outfl, grd.nc.out)
		# ncvar_put(nc2, grd.nc.out, round(out.mrg))
		# nc_close(nc2)
		return(round(out.mrg, 1))
	})
	# }

	mrg.data <- do.call(rbind, mrg.data)
	mrg.data.train <- data.frame(adjInfo$dates, mrg.data[, 1:length(stnData$lon), drop = FALSE])
	mrg.data.valid <- data.frame(adjInfo$dates, mrg.data[, length(stnData$lon)+(1:length(ijInt)), drop = FALSE])

	outfl.valid <- file.path(origdir, 'Merged_Validation_Data.txt')
	write.table(mrg.data.valid, outfl.valid, row.names = FALSE, col.names = FALSE, quote = FALSE)

	outfl.train <- file.path(origdir, 'Merged_Training_Data.txt')
	write.table(mrg.data.train, outfl.train, row.names = FALSE, col.names = FALSE, quote = FALSE)

	############

	# dekdaty <- adjInfo$dates[adjInfo$exist]
	# if(freqData == "daily" & GeneralParameters$Scale.daily & length(dekdaty) > 7){
	# 	InsertMessagesTxt(main.txt.out, 'Scaling up daily data ...')

	# 	dekDataInfo <- getRFESampleData(GeneralParameters$IO.files$DEK.file)
	# 	dekDir <- GeneralParameters$IO.files$DEK.dir
	# 	dekfilefrmt <- GeneralParameters$FileFormat$DEK.File.Format
	# 	months <- GeneralParameters$Mrg.Months

	# 	an <- substr(dekdaty, 1, 4)
	# 	mois <- substr(dekdaty, 5, 6)
	# 	jour <- as.numeric(substr(dekdaty, 7, 8))
	# 	outfl <- file.path(origdir, sprintf(Mrg.file.format, an, mois, jour))

	# 	nbs <- ifelse(jour[1] <= 10, 10, ifelse(jour[1] > 10 & jour[1] <= 20, 10,
	# 				rev((28:31)[which(!is.na(as.Date(paste(an[1], mois[1], 28:31, sep = '-'))))])[1]-20))
	# 	nbe <- ifelse(jour[length(an)] <= 10, 10, ifelse(jour[length(an)] > 10 & jour[length(an)] <= 20, 10,
	# 				rev((28:31)[which(!is.na(as.Date(paste(an[length(an)], mois[length(an)], 28:31, sep = '-'))))])[1]-20))
	# 	jour[jour <= 10] <- 1
	# 	jour[jour > 10 & jour <= 20] <- 2
	# 	jour[jour > 20] <- 3
	# 	nbd <- rle(jour)$lengths
	# 	nbd[c(1, length(nbd))] <- c(nbs, nbe)
	# 	jj <- paste(an, mois, jour, sep = '')
	# 	dekDate <- cbind(aggregate(rep(1, length(an)), by = list(jj), sum), nbd, seq_along(nbd))
	# 	names(dekDate) <- c('date', 'izy', 'feno', 'id')

	# 	daily.data <- data.frame(id = seq_along(jj), file = outfl)
	# 	dekdaty1 <- as.character(dekDate$date)
	# 	for(j in seq_along(dekdaty1)) daily.data[jj == dekdaty1[j], 1] <- dekDate$id[j]

	# 	an1 <- substr(dekdaty1, 1, 4)
	# 	mois1 <- substr(dekdaty1, 5, 6)
	# 	jour1 <- substr(dekdaty1, 7, 7)
	# 	start.date1 <- as.Date(paste(an1[1], mois1[1], jour1[1], sep = '-'))
	# 	end.date1 <- as.Date(paste(an1[length(an1)], mois1[length(an1)], jour1[length(an1)], sep = '-'))

	# 	error.msg <- "Merged dekadal data not found"
	# 	dekInfo <- ncFilesInfo('dekadal', start.date1, end.date1, months, dekDir, dekfilefrmt, error.msg)
	# 	if(is.null(dekInfo)){
	# 		if(closeklust) stopCluster(klust)
	# 		return(NULL)
	# 	}
	# 	dekinfo <- data.frame(date = dekInfo$dates, exist = dekInfo$exist, file = dekInfo$nc.files)
	# 	dekinfo <- merge(dekDate, dekinfo, by = "date", all.x = TRUE)
	# 	dekinfo <- dekinfo[dekinfo$izy == dekinfo$feno & dekinfo$exist, , drop = FALSE]
	# 	daily.data <- daily.data[daily.data$id%in%dekinfo$id, , drop = FALSE]
	# 	oper.dek <- list()
	# 	for(j in seq_along(dekinfo$id)) oper.dek[[j]] <- list(dek = as.character(dekinfo$file[dekinfo$id == dekinfo$id[j]]),
	# 														day = as.character(daily.data$file[daily.data$id == dekinfo$id[j]]))
	# 	############
	# 	nc <- nc_open(oper.dek[[1]]$dek)
	# 	xlon <- nc$dim[[dekDataInfo$rfeILon]]$vals
	# 	xlat <- nc$dim[[dekDataInfo$rfeILat]]$vals
	# 	nc_close(nc)
	# 	xo <- order(xlon)
	# 	xlon <- xlon[xo]
	# 	yo <- order(xlat)
	# 	xlat <- xlat[yo]
	# 	rfeDek <- defSpatialPixels(list(lon = xlon, lat = xlat))

	# 	nc <- nc_open(oper.dek[[1]]$day[1])
	# 	dlon <- nc$dim[[1]]$vals
	# 	dlat <- nc$dim[[2]]$vals
	# 	nc_close(nc)
	# 	rfeDay <- defSpatialPixels(list(lon = dlon, lat = dlat))

	# 	is.regridDEK <- is.diffSpatialPixelsObj(rfeDay, rfeDek, tol = 1e-07)

	# 	packages <- c('ncdf4', 'fields')
	# 	ncdata <- foreach(jj = seq_along(oper.dek), .packages = packages) %parLoop% {
	# 		nc <- nc_open(oper.dek[[jj]]$dek)
	# 		xdek <- ncvar_get(nc, varid = dekDataInfo$rfeVarid)
	# 		nc_close(nc)
	# 		if(dekDataInfo$rfeILat == 1){
	# 			xdek <- matrix(c(xdek), nrow = length(xlon), ncol = length(xlat), byrow = TRUE)
	# 		}
	# 		xdek <- xdek[xo, yo]

	# 		if(is.regridDEK){
	# 			dekGrid <- interp.surface.grid(list(x = xlon, y = xlat, z = xdek), list(x = dlon, y = dlat))
	# 			xdek <- dekGrid$z
	# 		}

	# 		xday <- lapply(seq_along(oper.dek[[jj]]$day), function(j){
	# 			nc <- nc_open(oper.dek[[jj]]$day[j])
	# 			xdd <- ncvar_get(nc, varid = nc$var[[1]]$name)
	# 			nc_close(nc)
	# 			xdd
	# 		})

	# 		xday <- simplify2array(xday)
	# 		ddek <- apply(xday, 1:2, sum)
	# 		scl <- xdek/ddek
	# 		scl[is.na(scl) | is.nan(scl) | is.infinite(scl)] <- 1
	# 		xday <- sweep(xday, 1:2, scl, `*`)

	# 		for(j in seq_along(oper.dek[[jj]]$day)){
	# 			nc2 <- nc_create(oper.dek[[jj]]$day[j], grd.nc.out)
	# 			ncvar_put(nc2, grd.nc.out, round(xday[, , j]))
	# 			nc_close(nc2)
	# 		}
	# 	}
	# }

	# if(closeklust) stopCluster(klust)

	# InsertMessagesTxt(main.txt.out, 'Merging finished')
	rm(rfeData, adjData, mrg.data, mrg.data.train, mrg.data.valid,interp.grid, stnData, data.stn)
	gc()
	return(0)
}

########################################################################################################

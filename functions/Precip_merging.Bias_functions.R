Precip_ComputeBias <- function(biasParms){
	InsertMessagesTxt(main.txt.out, 'Compute bias factors ...')

	GeneralParameters <- biasParms$GeneralParameters
	freqData <- GeneralParameters$period
	date.stn <- biasParms$stnData$dates
	data.stn <- biasParms$stnData$data
	nstn <- length(biasParms$stnData$lon)

	############### 

	if(GeneralParameters$BIAS$interp.method == 'NN'){
		rad.lon <- GeneralParameters$BIAS$rad.lon
		rad.lat <- GeneralParameters$BIAS$rad.lat
		grd.lon <- biasParms$interp.grid$grid$lon
		grd.lat <- biasParms$interp.grid$grid$lat
		nlon0 <- biasParms$interp.grid$nlon
		nlat0 <- biasParms$interp.grid$nlat
		res.coarse <- sqrt((rad.lon*mean(grd.lon[-1]-grd.lon[-nlon0]))^2 + (rad.lat*mean(grd.lat[-1]-grd.lat[-nlat0]))^2)/2
	}else res.coarse <- GeneralParameters$BIAS$maxdist/2
	res.coarse <- if(res.coarse >= 0.25) res.coarse else 0.25

	ptsData <- biasParms$stnData[c('lon', 'lat')]
	if(GeneralParameters$BIAS$bias.method == 'Quantile.Mapping'){
		idcoarse <- indexCoarseGrid(biasParms$ncInfo$xy.rfe$lon, biasParms$ncInfo$xy.rfe$lat, res.coarse)
		ptsData1 <- expand.grid(lon = biasParms$ncInfo$xy.rfe$lon[idcoarse$ix], lat = biasParms$ncInfo$xy.rfe$lat[idcoarse$iy])
		nbgrd <- nrow(ptsData1)
		ptsData <- list(lon = c(ptsData$lon, ptsData1$lon), lat = c(ptsData$lat, ptsData1$lat))
	}
	msg <- list(start = 'Read  and extract RFE data ...', end = 'Reading RFE data finished')
	rfeData <- readNetCDFData2Points(biasParms$ncInfo, ptsData, msg)

	data.rfe.stn <- rfeData$data[, 1:nstn]
	if(GeneralParameters$BIAS$bias.method == 'Quantile.Mapping') data.rfe <- rfeData$data[, nstn+(1:nbgrd)]
	date.bias <- rfeData$dates

	###############

	ibsdt <- match(date.stn, date.bias)
	ibsdt <- ibsdt[!is.na(ibsdt)]
	istdt <- match(date.bias, date.stn)
	istdt <- istdt[!is.na(istdt)]

	if(length(ibsdt) == 0){
		InsertMessagesTxt(main.txt.out, "Date out of range", format = TRUE)
		return(NULL)
	}

	###############
	if(GeneralParameters$BIAS$bias.method != 'Quantile.Mapping'){
		date.bias <- date.bias[ibsdt]
		data.rfe.stn <- data.rfe.stn[ibsdt, , drop = FALSE]
		data.stn <- data.stn[istdt, , drop = FALSE]

		if(GeneralParameters$BIAS$bias.method == 'Multiplicative.Bias.Mon'){
			times.stn <- as(substr(date.bias, 5, 6), 'numeric')
		}
		if(GeneralParameters$BIAS$bias.method == 'Multiplicative.Bias.Var'){
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
				data.rfe.stn <- data.rfe.stn[ix5days[, 2], , drop = FALSE]
				data.stn <- data.stn[ix5days[, 2], , drop = FALSE]
			}
			if(freqData == 'pentad'){
				vtimes <- cbind(expand.grid(1:6, 1:12), 1:72)
				xdaty <- paste(as.numeric(substr(date.bias, 7, 7)), as.numeric(substr(date.bias, 5, 6)), sep = '_')
				xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
				times.stn <- vtimes[match(xdaty, xvtm), 3]
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
			rfe <- data.rfe.stn[x, , drop = FALSE]
			na.data <- is.na(stn) | is.na(rfe)
			stn[na.data] <- NA
			rfe[na.data] <- NA
			len <- colSums(!na.data)
			stn <- colSums(stn, na.rm = TRUE)
			rfe <- colSums(rfe, na.rm = TRUE)
			bs <- stn/rfe
			bs[len < GeneralParameters$BIAS$min.length] <- 1
			bs[is.na(bs)] <- 1
			bs[is.infinite(bs)] <- 1
			bs[is.nan(bs)] <- 1
			bs[bs > 3] <- 3
			bs
		})

		bias <- do.call(rbind, bias)

		##########
		bias.pars <- list(bias = bias, lon.stn = biasParms$stnData$lon, lat.stn = biasParms$stnData$lat,
							id.stn = biasParms$stnData$id, data.stn = data.stn, data.rfe = data.rfe.stn,
							date = date.bias, grid = biasParms$interp.grid)
		rm(data.rfe.stn)
	}else{
		mplus.pentad.date <- function(daty){
			pen1 <- as.character(daty)
			pen2 <- addDekadsVec(as.Date(pen1, format = '%Y%m%d'), n = 1)
			pen2 <- paste0(format(pen2, '%Y%m'), as.numeric(format(pen2, '%d')))
			pen0 <- addDekadsVec(as.Date(pen1, format = '%Y%m%d'), n = -1)
			pen0 <- paste0(format(pen0, '%Y%m'), as.numeric(format(pen0, '%d')))
			unique(sort(c(pen0, pen1, pen2)))
		}
		mplus.dekad.date <- function(daty){
			dek1 <- as.character(daty)
			dek2 <- addDekadsVec(as.Date(dek1, format = '%Y%m%d'), n = 1)
			dek2 <- paste0(format(dek2, '%Y%m'), as.numeric(format(dek2, '%d')))
			dek0 <- addDekadsVec(as.Date(dek1, format = '%Y%m%d'), n = -1)
			dek0 <- paste0(format(dek0, '%Y%m'), as.numeric(format(dek0, '%d')))
			unique(sort(c(dek0, dek1, dek2)))
		}
		mplus.month.date <- function(daty){
			mon1 <- as.character(daty)
			mon2 <- addMonthsVec(as.Date(paste0(mon1, '01'), format = '%Y%m%d'), addMonths, n = 1)
			mon2 <- format(mon2, '%Y%m')
			mon0 <- addMonthsVec(as.Date(paste0(mon1, '01'), format = '%Y%m%d'), addMonths, n = -1)
			mon0 <- format(mon0, '%Y%m')
			unique(sort(c(mon0, mon1, mon2)))
		}

		data.stn <- data.stn[istdt, , drop = FALSE]
		date.bias <- date.bias[ibsdt]
		data.rfe <- data.rfe[ibsdt, , drop = FALSE]
		data.rfe.stn <- data.rfe.stn[ibsdt, , drop = FALSE]

		months <- biasParms$months
		xmonth <- as(substr(date.bias, 5, 6), 'numeric')

		packages <- c('qmap', 'fitdistrplus', 'ADGofTest')
		toExports <- c('fit.mixture.distr', 'addPentads', 'addDekads', 'addMonths', 
						'addPentadsVec', 'addDekadsVec', 'addMonthsVec')
		parsDistr <- vector(mode = 'list', length = 12)

		is.parallel <- doparallel(length(months) >= 3)
		`%parLoop%` <- is.parallel$dofun

		parsDistr[months] <- foreach (m = months, .packages = packages, .export = toExports) %parLoop% {
			if(freqData == 'daily') xdt <- xmonth == m
			if(freqData == 'pentad') xdt <- date.bias%in%mplus.pentad.date(date.bias[xmonth == m])
			if(freqData == 'dekadal') xdt <- date.bias%in%mplus.dekad.date(date.bias[xmonth == m])
			if(freqData == 'monthly') xdt <- date.bias%in%mplus.month.date(date.bias[xmonth == m])

			xstn <- data.stn[xdt, , drop = FALSE]
			xrfestn <- data.rfe.stn[xdt, , drop = FALSE]
			xrfe <- data.rfe[xdt, , drop = FALSE]
			xstn <- lapply(seq(ncol(xstn)), function(j) fit.mixture.distr(xstn[, j], GeneralParameters$BIAS$min.length, distr.fun = "berngamma"))
			xrfestn <- lapply(seq(ncol(xrfestn)), function(j) fit.mixture.distr(xrfestn[, j], GeneralParameters$BIAS$min.length, distr.fun = "berngamma"))
			xrfe <- lapply(seq(ncol(xrfe)), function(j) fit.mixture.distr(xrfe[, j], GeneralParameters$BIAS$min.length, distr.fun = "berngamma"))

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
		if(GeneralParameters$BIAS$AD.test){
			parsAD[months] <- lapply(months, function(j){
				istn <- AD.stn[[j]] == 'yes' & AD.rfestn[[j]] == 'yes'
				irfe <- AD.rfe[[j]] == 'yes'
				list(rfe = irfe, stn = istn)
			})
		}else{
			parsAD[months] <- lapply(months, function(j){
				istn <- rep(TRUE, length(AD.stn[[j]]))
				irfe <- rep(TRUE, length(AD.rfe[[j]]))
				list(rfe = irfe, stn = istn)
			})
		}

		parsAD.Rfe <- lapply(parsAD, '[[', 1)
		parsAD.Stn <- lapply(parsAD, '[[', 2)

		bias <- list(pars.ad.stn = parsAD.Stn, pars.ad.rfe = parsAD.Rfe, pars.stn = pars.Stn,
					pars.rfestn = pars.Rfestn, pars.rfe = pars.Rfe)

		##########
		bias.pars <- list(bias = bias, fit.stn = pars.Obs.Stn, fit.rfestn = pars.Obs.rfe, fit.rfe = pars.Crs.Rfe,
						lon.stn = biasParms$stnData$lon, lat.stn = biasParms$stnData$lat, id.stn = biasParms$stnData$id,
						lon.rfe = biasParms$ncInfo$xy.rfe$lon[idcoarse$ix], lat.rfe = biasParms$ncInfo$xy.rfe$lat[idcoarse$iy],
						data.stn = data.stn, data.rfestn = data.rfe.stn, data.rfe = data.rfe, date = date.bias, grid = biasParms$interp.grid)
		rm(parsDistr, pars.Obs.Stn, pars.Obs.rfe, pars.Crs.Rfe, pars.Stn, pars.Rfe, pars.Rfestn, parsAD, parsAD.Rfe, parsAD.Stn)
	}

	saveRDS(bias.pars, file = file.path(biasParms$bias.DIR, "BIAS_PARAMS.rds"))

	InsertMessagesTxt(main.txt.out, 'Computing bias factors finished')
	rm(rfeData, data.stn, data.rfe.stn, data.rfe, bias.pars)
	gc()
	return(bias)
}

########################################################################################################

Precip_InterpolateBias <- function(biasParms){
	InsertMessagesTxt(main.txt.out, 'Interpolate bias factors ...')

	GeneralParameters <- biasParms$GeneralParameters

	#############
	auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
	is.auxvar <- unlist(GeneralParameters$auxvar)
	if(any(is.auxvar)){
		formule <- formula(paste0('pars', '~', paste(auxvar[is.auxvar], collapse = '+')))
	}else formule <- formula(paste0('pars', '~', 1))

	#############
	xy.grid <- biasParms$interp.grid$grid
	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- biasParms$interp.grid$nlon
	nlat0 <- biasParms$interp.grid$nlat

	#############
	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	xy.dim <- list(dx, dy)

	#############
	demGrid <- biasParms$demData
	if(!is.null(demGrid)){
		# demres <- grdSp@grid@cellsize
		# slpasp <- slope.aspect(demGrid$z, demres[1], demres[2], filter = "sobel")
		slpasp <- raster.slope.aspect(demGrid)
		demGrid$slp <- slpasp$slope
		demGrid$asp <- slpasp$aspect
	}else{
		demGrid <- list(x = xy.grid$lon, y = xy.grid$lat, z = matrix(1, nlon0, nlat0))
		demGrid$slp <- matrix(0, nlon0, nlat0)
		demGrid$asp <- matrix(0, nlon0, nlat0)
	}

	ijGrd <- grid2pointINDEX(biasParms$stnData[c('lon', 'lat')], xy.grid)
	ObjStn <- list(x = biasParms$stnData$lon, y = biasParms$stnData$lat,
					z = demGrid$z[ijGrd], slp = demGrid$slp[ijGrd], asp = demGrid$asp[ijGrd])

	#############
	# regrid RFE
	create.grd <- GeneralParameters$Grid.Creation$grid
	rfeGrid <- NULL
	rfeSp <- defSpatialPixels(biasParms$ncInfo$xy.rfe)
	is.regridRFE <- is.diffSpatialPixelsObj(grdSp, rfeSp, tol = 1e-07)
	if(create.grd != '1' & is.regridRFE){
		xy.rfe <- biasParms$ncInfo$xy.rfe
		demGrid0 <- demGrid[c('x', 'y', 'z')]
		names(demGrid0) <- c('lon', 'lat', 'z')
		rfeGrid <- cdt.interp.surface.grid(demGrid0, xy.rfe)
		irfe <- over(rfeSp, grdSp)
		rfeGrid$slp <- matrix(demGrid$slp[irfe], length(xy.rfe$lon), length(xy.rfe$lat))
		rfeGrid$asp <- matrix(demGrid$asp[irfe], length(xy.rfe$lon), length(xy.rfe$lat))
	}

	#############
	## interpolation grid
	interp.method <- GeneralParameters$BIAS$interp.method
	nmin <- GeneralParameters$BIAS$nmin
	nmax <- GeneralParameters$BIAS$nmax
	maxdist <- GeneralParameters$BIAS$maxdist
	vgm.model <- GeneralParameters$BIAS$vgm.model
	rad.lon <- GeneralParameters$BIAS$rad.lon
	rad.lat <- GeneralParameters$BIAS$rad.lat
	rad.elv <- GeneralParameters$BIAS$rad.elv

	min.stn <- GeneralParameters$BIAS$min.stn

	if(interp.method == 'NN'){
		grd.lon <- xy.grid$lon
		grd.lat <- xy.grid$lat
		res.coarse <- sqrt((rad.lon*mean(grd.lon[-1]-grd.lon[-nlon0]))^2 + (rad.lat*mean(grd.lat[-1]-grd.lat[-nlat0]))^2)/2
	}else res.coarse <- maxdist/2
	res.coarse <- if(res.coarse >= 0.25) res.coarse else 0.25

	#############
	## create grid to interp
	bGrd <- NULL
	if(interp.method == 'NN'){
		interp.grid <- createGrid(ObjStn, demGrid, rfeGrid,
						as.dim.elv = GeneralParameters$BIAS$elev.3rd.dim,
						latlong = GeneralParameters$BIAS$latlon.unit,
						normalize = GeneralParameters$BIAS$normalize,
						coarse.grid = TRUE, res.coarse = res.coarse)
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
		bGrd <- if(GeneralParameters$BIAS$use.block) createBlock(cells@cellsize, 2, 5) else NULL
	}

	#############
	if(is.auxvar['lon']){
		interp.grid$coords.stn$alon <- interp.grid$coords.stn@coords[, 'lon']
		interp.grid$coords.grd$alon <- interp.grid$coords.grd@coords[, 'lon']
		if(!is.null(interp.grid$coords.rfe)) interp.grid$coords.rfe$alon <- interp.grid$coords.rfe@coords[, 'lon']
		interp.grid$newgrid$alon <- interp.grid$newgrid@coords[, 'lon']
	}

	if(is.auxvar['lat']){
		interp.grid$coords.stn$alat <- interp.grid$coords.stn@coords[, 'lat']
		interp.grid$coords.grd$alat <- interp.grid$coords.grd@coords[, 'lat']
		if(!is.null(interp.grid$coords.rfe)) interp.grid$coords.rfe$alat <- interp.grid$coords.rfe@coords[, 'lat']
		interp.grid$newgrid$alat <- interp.grid$newgrid@coords[, 'lat']
	}

	#######################################
	## interpolation

	months <- biasParms$months
	bias.pars <- biasParms$bias.pars

	if(GeneralParameters$BIAS$bias.method != 'Quantile.Mapping'){
		itimes <- as.numeric(rownames(bias.pars))
		grd.bs <- ncvar_def("bias", "", xy.dim, NA, longname = "Multiplicative Mean Bias Factor", prec = "float", compression = 9)

		packages <- c('sp', 'gstat', 'automap', 'ncdf4')
		is.parallel <- doparallel(length(itimes) >= 3)
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(m = itimes, .packages = packages) %parLoop% {
			locations.stn <- interp.grid$coords.stn
			locations.stn$pars <- bias.pars[itimes == m, ]
			locations.stn <- locations.stn[!is.na(locations.stn$pars), ]

			# extrm <- quantile(locations.stn$pars, probs = c(0.001, 0.999))
			# locations.stn <- locations.stn[locations.stn$pars > extrm[1] & locations.stn$pars < extrm[2], ]

			if(any(is.auxvar) & interp.method != 'NN'){
				locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
				locations.stn <- locations.stn[Reduce("&", locations.df), ]
			}

			if(length(locations.stn$pars) >= 1 & any(locations.stn$pars != 1)){
				if(interp.method == 'Kriging'){
					if(length(locations.stn$pars) > 7){
						vgm <- try(autofitVariogram(formule, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
						vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
					}else vgm <- NULL
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

					xtrm <- range(locations.stn$pars, na.rm = TRUE)
					pars.grd$var1.pred[!is.na(pars.grd$var1.pred) & pars.grd$var1.pred < xtrm[1]] <- xtrm[1]
					pars.grd$var1.pred[!is.na(pars.grd$var1.pred) & pars.grd$var1.pred > xtrm[2]] <- xtrm[2]

					ina <- is.na(pars.grd$var1.pred)
					if(all(ina)){
						pars.grd$var1.pred <- 1
					}else{
						if(any(ina)){
							pars.grd.na <- krige(var1.pred~1, locations = pars.grd[!ina, ], newdata = interp.grid$newgrid[ina, ], model = vgm,
												block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
							pars.grd$var1.pred[ina] <- pars.grd.na$var1.pred
						}
					}
				}

				grdbias <- matrix(pars.grd$var1.pred, ncol = nlat0, nrow = nlon0)
				grdbias[grdbias > 3] <- 3
				grdbias[grdbias < 0] <- 0.1
				grdbias[is.na(grdbias)] <- 1
			}else grdbias <- matrix(1, ncol = nlat0, nrow = nlon0)

			######
			outnc <- file.path(biasParms$bias.DIR, sprintf(GeneralParameters$biasFilenames, m))
			nc2 <- nc_create(outnc, grd.bs)
			ncvar_put(nc2, grd.bs, grdbias)
			nc_close(nc2)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
	}else{
		is.parallel <- doparallel(length(months) >= 3)
		`%parLoop%` <- is.parallel$dofun

		packages <- c('sp', 'gstat', 'automap')
		PARS.stn <- vector(mode = 'list', length = 12)
		PARS.stn[months] <- foreach(m = months, .packages = packages) %parLoop% {
			pars.mon <- lapply(1:3, function(j){
				locations.stn <- interp.grid$coords.stn
				locations.stn$pars <- bias.pars$pars.stn[[m]][, j]
				if(j > 1) locations.stn$pars[!bias.pars$pars.ad.stn[[m]]] <- NA
				locations.stn <- locations.stn[!is.na(locations.stn$pars), ]

				# extrm <- quantile(locations.stn$pars, probs = c(0.001, 0.99))
				# locations.stn <- locations.stn[locations.stn$pars > extrm[1] & locations.stn$pars < extrm[2], ]

				if(any(is.auxvar) & interp.method != 'NN'){
					locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
					locations.stn <- locations.stn[Reduce("&", locations.df), ]
				}
				if(length(locations.stn$pars) < min.stn) return(NULL)

				if(interp.method == 'Kriging'){
					if(length(locations.stn$pars) > 7){
						vgm <- try(autofitVariogram(formule, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
						vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
					}else vgm <- NULL
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

				# axtrm <- quantile(xadd$pars, probs = c(0.001, 0.99))
				# xadd <- xadd[xadd$pars > axtrm[1] & xadd$pars < axtrm[2], , drop = FALSE]

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
					pars.grd <- krige(pars~1, locations = locations.stn, newdata = interp.grid$newgrid,
										nmax = 1, maxdist = maxdist, debug.level = 0)
				}else{
					locations.stn <- locations.stn[!duplicated(locations.stn[, c('lon', 'lat')]), ]
					coordinates(locations.stn) <- ~lon+lat
					if(any(is.auxvar)){
						locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
						locations.stn <- locations.stn[Reduce("&", locations.df), ]
						block <- NULL
					}else block <- bGrd
					pars.grd <- krige(formule, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
										block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				}
				ret <- matrix(pars.grd$var1.pred, ncol = nlat0, nrow = nlon0)
				if(j == 1){
					ret[ret < 0] <- 0.01
					ret[ret > 1] <- 0.99
				}else ret[ret < 0] <- NA
				ret
			})
			names(pars.mon) <- c('prob', 'scale', 'shape')
			pars.mon
		}

		PARS.rfe <- vector(mode = 'list', length = 12)
		PARS.rfe[months] <- foreach(m = months, .packages = packages) %parLoop% {
			pars.mon <- lapply(1:3, function(j){
				locations.rfe <- if(create.grd != '1' & is.regridRFE) interp.grid$coords.rfe else interp.grid$coords.grd
				locations.rfe$pars <- bias.pars$pars.rfe[[m]][, j]
				if(j > 1) locations.rfe$pars[!bias.pars$pars.ad.rfe[[m]]] <- NA

				locations.rfestn <- interp.grid$coords.stn
				locations.rfestn$pars <- bias.pars$pars.rfestn[[m]][, j]
				if(j > 1) locations.rfestn$pars[!bias.pars$pars.ad.stn[[m]]] <- NA

				locations.rfe <- rbind(locations.rfe, locations.rfestn)
				locations.rfe <- locations.rfe[!is.na(locations.rfe$pars), ]
				if(length(locations.rfe$pars) < min.stn) return(NULL)

				# extrm <- quantile(locations.rfe$pars, probs = c(0.001, 0.999))
				# locations.rfe <- locations.rfe[locations.rfe$pars > extrm[1] & locations.rfe$pars < extrm[2], ]

				locations.rfe <- remove.duplicates(locations.rfe)

				if(any(is.auxvar) & interp.method != 'NN'){
					locations.df <- as.data.frame(!is.na(locations.rfe@data[, auxvar[is.auxvar]]))
					locations.rfe <- locations.rfe[Reduce("&", locations.df), ]
				}
				if(length(locations.rfe$pars) < min.stn) return(NULL)

				if(interp.method == 'Kriging'){
					if(length(locations.rfe$pars) > 7){
						vgm <- try(autofitVariogram(formule, input_data = locations.rfe, model = vgm.model, cressie = TRUE), silent = TRUE)
						vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
					}else vgm <- NULL
				}else vgm <- NULL

				if(interp.method == 'NN'){
					pars.grd <- krige(pars~1, locations = locations.rfe, newdata = interp.grid$newgrid,
										nmax = 1, maxdist = maxdist, debug.level = 0)
				}else{
					block <- if(any(is.auxvar)) NULL else bGrd
					pars.grd <- krige(formule, locations = locations.rfe, newdata = interp.grid$newgrid, model = vgm,
									block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				}
				ret <- matrix(pars.grd$var1.pred, ncol = nlat0, nrow = nlon0)
				if(j == 1){
					ret[ret < 0] <- 0.01
					ret[ret > 1] <- 0.99
				}else ret[ret < 0] <- NA
				ret
			})
			names(pars.mon) <- c('prob', 'scale', 'shape')
			pars.mon
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

		################

		default <- matrixStats::colMedians(do.call(rbind, bias.pars$pars.rfe), na.rm = TRUE)
		names(default) <- c('prob', 'scale', 'shape')
		PARS <- vector(mode = 'list', length = 12)
		PARS[months] <- lapply(months, function(m){
			par.stn <- PARS.stn[[m]]
			par.rfe <- PARS.rfe[[m]]
			prob <- scale <- shape <- matrix(NA, ncol = nlat0, nrow = nlon0)
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

		#Defines netcdf output
		grd.prob <- ncvar_def("prob", "", xy.dim, NA, longname = "Probability of non-zero event Bernoulli-Gamma distribution", prec = "float", compression = 9)
		grd.scale <- ncvar_def("scale", "", xy.dim, NA, longname = "Scale parameters of the gamma distribution", prec = "float", compression = 9)
		grd.shape <- ncvar_def("shape", "", xy.dim, NA, longname = "Shape parameters of the gamma distribution", prec = "float", compression = 9)

		for(jfl in months){
			outnc1 <- file.path(biasParms$bias.DIR, paste0('Bernoulli-Gamma_Pars.STN_', jfl, '.nc'))
			nc1 <- nc_create(outnc1, list(grd.prob, grd.scale, grd.shape))
			ncvar_put(nc1, grd.prob, PARS[[jfl]]$stn$prob)
			ncvar_put(nc1, grd.scale, PARS[[jfl]]$stn$scale)
			ncvar_put(nc1, grd.shape, PARS[[jfl]]$stn$shape)
			nc_close(nc1)
		}

		for(jfl in months){
			outnc2 <- file.path(biasParms$bias.DIR, paste0('Bernoulli-Gamma_Pars.RFE_', jfl, '.nc'))
			nc2 <- nc_create(outnc2, list(grd.prob, grd.scale, grd.shape))
			ncvar_put(nc2, grd.prob, PARS[[jfl]]$rfe$prob)
			ncvar_put(nc2, grd.scale, PARS[[jfl]]$rfe$scale)
			ncvar_put(nc2, grd.shape, PARS[[jfl]]$rfe$shape)
			nc_close(nc2)
		}
		rm(PARS.stn, PARS.rfe, PARS)
	}

	rm(demGrid, rfeGrid, ObjStn, interp.grid, bias.pars)
	gc()
	InsertMessagesTxt(main.txt.out, 'Interpolating bias factors finished')
	return(0)
}

########################################################################################################

Precip_ReadBiasFiles <- function(biasParms){
	InsertMessagesTxt(main.txt.out, 'Read bias data ...')
	GeneralParameters <- biasParms$GeneralParameters
	months <- biasParms$months

	if(GeneralParameters$BIAS$bias.method == "Multiplicative.Bias.Mon"){
		biasFilename <- sprintf(GeneralParameters$biasFilenames, months)
		biasFile <- file.path(biasParms$bias.DIR, biasFilename)
		exist.bias <- unlist(lapply(biasFile, file.exists))
		if(any(!exist.bias)){
			miss.bias <- months[!exist.bias]
			for(j in seq_along(miss.bias)){
				msg <- biasFilename[miss.bias[j]]
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
			# xvar <- ncvar_get(nc, varid = "bias")
			xvar <- ncvar_get(nc, varid = nc$var[[1]]$name)
			nc_close(nc)
			xvar
		})
		bias <- list(lon = lon, lat = lat, bias = BIAS)
	}

	if(GeneralParameters$BIAS$bias.method == "Multiplicative.Bias.Var"){
		freqData <- GeneralParameters$period
		if(freqData == 'daily'){
			endmon <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
			vtimes <- cbind(unlist(sapply(endmon, function(j) 1:j)), rep(1:12, endmon), 1:365)
			vtimes <- vtimes[vtimes[, 2]%in%months, , drop = FALSE]

			daty <- biasParms$dates
			daty <- daty[as.numeric(substr(daty, 5, 6))%in%vtimes[, 2]]
			xdaty <- paste(as.numeric(substr(daty, 7, 8)), as.numeric(substr(daty, 5, 6)), sep = '_')
			xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
			times.stn <- vtimes[match(xdaty, xvtm), 3]
			times.stn[is.na(times.stn)] <- 59
			times.stn <- sort(unique(times.stn))
			BIAS <- vector(mode = 'list', length = 365) 
		}
		if(freqData == 'pentad'){
			vtimes <- cbind(expand.grid(1:6, 1:12), 1:72)
			vtimes <- vtimes[vtimes[, 2]%in%months, , drop = FALSE]

			daty <- biasParms$dates
			daty <- daty[as.numeric(substr(daty, 5, 6))%in%vtimes[, 2]]
			xdaty <- paste(as.numeric(substr(daty, 7, 7)), as.numeric(substr(daty, 5, 6)), sep = '_')
			xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
			times.stn <- vtimes[match(xdaty, xvtm), 3]
			times.stn <- sort(unique(times.stn))
			BIAS <- vector(mode = 'list', length = 72)
		}
		if(freqData == 'dekadal'){
			vtimes <- cbind(expand.grid(1:3, 1:12), 1:36)
			vtimes <- vtimes[vtimes[, 2]%in%months, , drop = FALSE]

			daty <- biasParms$dates
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

		biasFilename <- sprintf(GeneralParameters$biasFilenames, times.stn)
		biasFile <- file.path(biasParms$bias.DIR, biasFilename)
		exist.bias <- unlist(lapply(biasFile, file.exists))

		if(any(!exist.bias)){
			miss.bias <- times.stn[!exist.bias]
			for(j in seq_along(miss.bias)){
				msg <- biasFilename[miss.bias[j]]
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
			# xvar <- ncvar_get(nc, varid = "bias")
			xvar <- ncvar_get(nc, varid = nc$var[[1]]$name)
			nc_close(nc)
			xvar
		})
		bias <- list(lon = lon, lat = lat, bias = BIAS)
	}

	if(GeneralParameters$BIAS$bias.method == "Quantile.Mapping"){
		pars.stnFile <- file.path(biasParms$bias.DIR, paste0('Bernoulli-Gamma_Pars.STN_', months, '.nc'))
		exist.pars.stn <- unlist(lapply(pars.stnFile, file.exists))
		if(any(!exist.pars.stn)){
			miss.pars.stn <- months[!exist.pars.stn]
			for(j in seq_along(miss.pars.stn)){
				msg <- paste('Bernoulli-Gamma_Pars.STN_', miss.pars.stn[j], '.nc', sep = '')
				InsertMessagesTxt(main.txt.out, paste(msg, "doesn't exist"), format = TRUE)
			}
			return(NULL)
		}

		pars.rfeFile <- file.path(biasParms$bias.DIR, paste0('Bernoulli-Gamma_Pars.RFE_', months, '.nc'))
		exist.pars.rfe <- unlist(lapply(pars.rfeFile, file.exists))
		if(any(!exist.pars.rfe)){
			miss.pars.rfe <- months[!exist.pars.rfe]
			for(j in seq_along(miss.pars.rfe)){
				msg <- paste0('Bernoulli-Gamma_Pars.RFE_', miss.pars.rfe[j], '.nc')
				InsertMessagesTxt(main.txt.out, paste(msg, "doesn't exist"), format = TRUE)
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
		bias <- list(lon = lon, lat = lat, bias.stn = PARS.stn, bias.rfe = PARS.rfe)
	}
	InsertMessagesTxt(main.txt.out, 'Reading bias data finished')
	return(bias)
}

########################################################################################################

Precip_ApplyBiasCorrection <- function(biasParms, extractADJ = FALSE){
	InsertMessagesTxt(main.txt.out, 'Apply bias correction ...')
	GeneralParameters <- biasParms$GeneralParameters
	bias.method <- GeneralParameters$BIAS$bias.method
	freqData <- GeneralParameters$period

	################
	dx <- ncdim_def("Lon", "degreeE", biasParms$BIAS$lon)
	dy <- ncdim_def("Lat", "degreeN", biasParms$BIAS$lat)
	xy.dim <- list(dx, dy)
	grd.bsadj <- ncvar_def("precip", "mm", xy.dim, -99, longname= "Bias Corrected RFE", prec = "short", shuffle = TRUE, compression = 9)

	################ 
	## RFE regrid?
	biasGrd <- biasParms$BIAS[c('lon', 'lat')]
	biasSp <- defSpatialPixels(biasGrd)
	rfeSp <- defSpatialPixels(biasParms$ncInfo$xy.rfe[c('lon', 'lat')])
	is.regridRFE <- is.diffSpatialPixelsObj(biasSp, rfeSp, tol = 1e-07)

	if(extractADJ) ijSTN <- grid2pointINDEX( biasParms$stnData[c('lon', 'lat')], biasGrd)

	################
	nc.dates <- biasParms$ncInfo$dates[biasParms$ncInfo$exist]
	nc.files <- biasParms$ncInfo$nc.files[biasParms$ncInfo$exist]
	ncinfo <- biasParms$ncInfo$ncinfo

	##################
	nc <- nc_open(nc.files[1])
	xlon <- nc$var[[ncinfo$varid]]$dim[[ncinfo$xo]]$vals
	xlat <- nc$var[[ncinfo$varid]]$dim[[ncinfo$yo]]$vals
	nc_close(nc)

	xo <- order(xlon)
	xlon <- xlon[xo]
	yo <- order(xlat)
	xlat <- xlat[yo]
	nlon <- length(xlon)
	nlat <- length(xlat)

	toExports <- "cdt.interp.surface.grid"
	if(bias.method == "Quantile.Mapping") toExports <- c(toExports, 'quantile.mapping.BGamma')
	if(bias.method == "Multiplicative.Bias.Var" & freqData == 'daily') toExports <- c(toExports, 'is.leapyear')
	packages <- 'ncdf4'

	is.parallel <- doparallel(length(nc.files) >= 30)
	`%parLoop%` <- is.parallel$dofun
	adj.stn <- foreach(jfl = seq_along(nc.files), .packages = packages, .export = toExports) %parLoop% {
		nc <- nc_open(nc.files[jfl])
		xrfe <- ncvar_get(nc, varid = ncinfo$varid)
		nc_close(nc)
		xrfe <- if(ncinfo$xo < ncinfo$yo) xrfe[xo, yo] else t(xrfe)[xo, yo]
		rfe.date <- nc.dates[jfl]

		############
		if(is.regridRFE){
			rfeGrid <- cdt.interp.surface.grid(list(lon = xlon, lat = xlat, z = xrfe), biasGrd)
			xrfe <- rfeGrid$z
		}

		############
		if(bias.method == "Multiplicative.Bias.Var"){
			if(freqData == 'daily'){
				ann <- as.numeric(substr(rfe.date, 1, 4))
				iday <- as.numeric(strftime(as.Date(rfe.date, format = '%Y%m%d'), format = '%j'))
				ijt <- ifelse(is.leapyear(ann) & iday > 59, iday-1, iday)
			}
			if(freqData == 'pentad'){
				mon <- as.numeric(substr(rfe.date, 5, 6))
				pen <- as.numeric(substr(rfe.date, 7, 7))
				annual.pen <- expand.grid(pen = 1:6, mon = 1:12)
				ijt <- which(annual.pen$pen == pen & annual.pen$mon == mon)
			}
			if(freqData == 'dekadal'){
				mon <- as.numeric(substr(rfe.date, 5, 6))
				dek <- as.numeric(substr(rfe.date, 7, 7))
				annual.dek <- expand.grid(dek = 1:3, mon = 1:12)
				ijt <- which(annual.dek$dek == dek & annual.dek$mon == mon)
			}
			if(freqData == 'monthly'){
				ijt <- as.numeric(substr(rfe.date, 5, 6))
			}
		}else{
			ijt <- as.numeric(substr(rfe.date, 5, 6))
		}

		############
		if(bias.method == "Quantile.Mapping"){
			xadj <- quantile.mapping.BGamma(xrfe, biasParms$BIAS$bias.stn[[ijt]],
									biasParms$BIAS$bias.rfe[[ijt]], rfe.zero = TRUE)
		}else xadj <- xrfe * biasParms$BIAS$bias[[ijt]]
		xadj[xadj < 0] <- 0
		adjSTN <- if(extractADJ) xadj[ijSTN] else NULL
		xadj[is.na(xadj)] <- -99

		############
		year <- substr(rfe.date, 1, 4)
		month <- substr(rfe.date, 5, 6)
		if(freqData == 'daily'){
			adjfrmt <- sprintf("rr_adj_%s%s%s.nc", year, month, substr(rfe.date, 7, 8))
		}else if(freqData%in%c('pentad', 'dekadal')){
			adjfrmt <- sprintf("rr_adj_%s%s%s.nc", year, month, substr(rfe.date, 7, 7))
		}else adjfrmt <- sprintf("rr_adj_%s%s.nc", year, month)
		outfl <- file.path(biasParms$adj.DIR, adjfrmt)

		nc2 <- nc_create(outfl, grd.bsadj)
		ncvar_put(nc2, grd.bsadj, xadj)
		nc_close(nc2)

		return(adjSTN)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	if(extractADJ){
		adjSTN <- list(dates = nc.dates, data = do.call(rbind, adj.stn))
	}else adjSTN <- 0

	rm(biasGrd, biasSp, rfeSp, adj.stn)
	gc()
	InsertMessagesTxt(main.txt.out, 'Bias Correction done')
	return(adjSTN)
}








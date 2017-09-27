Precip_MergingFunctions <- function(mrgParms){
	InsertMessagesTxt(main.txt.out, 'Merge data ...')

	GeneralParameters <- mrgParms$GeneralParameters
	freqData <- GeneralParameters$period

	#############
	xy.grid <- mrgParms$interp.grid$grid
	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- mrgParms$interp.grid$nlon
	nlat0 <- mrgParms$interp.grid$nlat

	#############
	## Def ncdf
	dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	grd.nc.out <- ncvar_def("precip", "mm", list(dx, dy), -99, longname = "Merged Station-Satellite Rainfall", prec = "short")

	#############
	mrg.method <- GeneralParameters$Merging$mrg.method
	interp.method <- GeneralParameters$Merging$interp.method
	nmin <- GeneralParameters$Merging$nmin
	nmax <- GeneralParameters$Merging$nmax
	maxdist <- GeneralParameters$Merging$maxdist
	vgm.model <- GeneralParameters$Merging$vgm.model
	min.stn <- GeneralParameters$Merging$min.stn
	min.non.zero <- GeneralParameters$Merging$min.non.zero

	res.coarse <- maxdist/2
	res.coarse <- if(res.coarse  >= 0.25) res.coarse else 0.25
	maxdist <- if(maxdist < res.coarse) res.coarse else maxdist

	use.RnoR <- GeneralParameters$RnoR$use.RnoR
	maxdist.RnoR <- GeneralParameters$RnoR$maxdist.RnoR
	smooth.RnoR <- GeneralParameters$RnoR$smooth.RnoR
	wet.day <- GeneralParameters$RnoR$wet.day

	#############
	auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
	is.auxvar <- unlist(GeneralParameters$auxvar)
	if(any(is.auxvar)){
		formule <- formula(paste0('res', '~', paste(auxvar[is.auxvar], collapse = '+')))
		if(mrg.method == "Regression Kriging"){
			sp.trend.aux <- GeneralParameters$Merging$sp.trend.aux
			if(sp.trend.aux) formuleRK <- formula(paste0('stn', '~', 'rfe', '+', paste(auxvar[is.auxvar], collapse = '+')))
			else formuleRK <- formula(paste0('stn', '~', 'rfe'))
		}
	}else{
		formule <- formula(paste0('res', '~', 1))
		if(mrg.method == "Regression Kriging") formuleRK <- formula(paste0('stn', '~', 'rfe'))
	}

	#############
	lon.stn <- mrgParms$stnData$lon
	lat.stn <- mrgParms$stnData$lat
	date.stn <- mrgParms$stnData$dates
	data.stn <- mrgParms$stnData$data
	nstn <- length(lon.stn)
	demData <- mrgParms$demData

	#############
	if(!is.null(demData)){
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
		dem.grd.val <- matrix(1, nlon0, nlat0)
		dem.grd.slp <- matrix(0, nlon0, nlat0)
		dem.grd.asp <- matrix(0, nlon0, nlat0)
		demData <- list(x = xy.grid$lon, y = xy.grid$lat, z = dem.grd.val, slp = dem.grd.slp, asp = dem.grd.asp)
		dem.stn.val <- rep(1, nstn)
		dem.stn.slp <- rep(0, nstn)
		dem.stn.asp <- rep(0, nstn)
	}
	ObjStn <- list(x = lon.stn, y = lat.stn, z = dem.stn.val, slp = dem.stn.slp, asp = dem.stn.asp)
	interp.grid <- createGrid(ObjStn, demData, as.dim.elv = FALSE, res.coarse = res.coarse)
	cells <- SpatialPixels(points = interp.grid$newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)))@grid
	bGrd <- if(GeneralParameters$Merging$use.block) createBlock(cells@cellsize, 2, 5) else NULL

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

	#############

	months <- mrgParms$months
	if(mrg.method == "Spatio-Temporal LM"){
		coefFiles <- file.path(GeneralParameters$LMCOEF$dir.LMCoef, sprintf(GeneralParameters$LMCOEF$format, months))
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
	ncInfo <- mrgParms$ncInfo
	rfeSp <- defSpatialPixels(ncInfo$xy.rfe)
	is.regridRFE <- is.diffSpatialPixelsObj(grdSp, rfeSp, tol = 1e-07)

	#############
	nc <- nc_open(ncInfo$nc.files[which(ncInfo$exist)[1]])
	xlon <- nc$dim[[ncInfo$ncinfo$xo]]$vals
	xlat <- nc$dim[[ncInfo$ncinfo$yo]]$vals
	nc_close(nc)
	xo <- order(xlon)
	xlon <- xlon[xo]
	yo <- order(xlat)
	xlat <- xlat[yo]
	xnlon <- length(xlon)
	xnlat <- length(xlat)

	## gridded data at stn loc
	ijGrd <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = xlon, lat = xlat))

	#############
	packages <- c('ncdf4', 'gstat', 'automap', 'fields', 'rgeos', 'maptools')
	toExports <- 'smooth.matrix'

	is.parallel <- doparallel(length(which(ncInfo$exist)) >= 10)
	`%parLoop%` <- is.parallel$dofun
	ret <- foreach(jj = seq_along(ncInfo$nc.files), .packages = packages, .export = toExports) %parLoop% {
		if(ncInfo$exist[jj]){
			nc <- nc_open(ncInfo$nc.files[jj])
			xrfe <- ncvar_get(nc, varid = ncInfo$ncinfo$varid)
			nc_close(nc)
			if(ncInfo$ncinfo$yo == 1){
				xrfe <- matrix(c(xrfe), nrow = xnlon, ncol = xnlat, byrow = TRUE)
			}
			xrfe <- xrfe[xo, yo]
		}else return(NULL)

		if(all(is.na(xrfe))) return(NULL)

		############
		if(is.regridRFE){
			rfeGrid <- interp.surface.grid(list(x = xlon, y = xlat, z = xrfe), list(x = xy.grid$lon, y = xy.grid$lat))
			xrfe <- rfeGrid$z
		}

		############
		locations.stn <- interp.grid$coords.stn
		donne.stn <- data.stn[date.stn == ncInfo$dates[jj], , drop = FALSE]
		if(nrow(donne.stn) == 0) return(NULL)
		locations.stn$stn <- c(donne.stn[1, ])
		locations.stn$rfe <- xrfe[ijGrd]

		xadd <- interp.grid$coords.grd
		xadd$rfe <- xadd$stn <- c(xrfe[interp.grid$idxy$ix, interp.grid$idxy$iy])
		xadd$res <- 0

		interp.grid$newgrid$rfe <- c(xrfe)
		newdata <- interp.grid$newgrid

		############
		noNA <- !is.na(locations.stn$stn)
		min.stn.nonNA <- length(which(noNA))
		nb.stn.nonZero <- length(which(noNA & locations.stn$stn > 0))
		locations.stn <- locations.stn[noNA, ]

		if(min.stn.nonNA >= min.stn){
			do.merging <- TRUE
			if(nb.stn.nonZero >= min.non.zero){
				if(mrg.method == "Spatio-Temporal LM"){
					mo <- as(substr(ncInfo$dates[jj], 5, 6), 'numeric')
					sp.trend <- xrfe * MODEL.COEF[[mo]]$slope + MODEL.COEF[[mo]]$intercept
					locations.stn$res <- locations.stn$stn - sp.trend[ijGrd][noNA]
				}else if(mrg.method == "Regression Kriging"){
					if(all(is.na(locations.stn$rfe))) return(NULL)
					simplediff <- if(var(locations.stn$stn) < 1e-07 | var(locations.stn$rfe, na.rm = TRUE) < 1e-07) TRUE else FALSE
					glm.stn <- glm(formuleRK, data = locations.stn, family = gaussian)
					if(is.na(glm.stn$coefficients[2]) | glm.stn$coefficients[2] < 0) simplediff <- TRUE
					if(!simplediff){
						sp.trend <- predict(glm.stn, newdata = interp.grid$newgrid)
						sp.trend <- matrix(sp.trend, ncol = nlat0, nrow = nlon0)
						ina.trend <- is.na(sp.trend)
						sp.trend[ina.trend] <- xrfe[ina.trend]
						locations.stn$res <- NA
						if(length(glm.stn$na.action) > 0) locations.stn$res[-glm.stn$na.action] <- glm.stn$residuals
						else locations.stn$res <- glm.stn$residuals
					}else{
						sp.trend <- xrfe
						locations.stn$res <- locations.stn$stn - locations.stn$rfe
					}
				}else{
					sp.trend <- xrfe
					locations.stn$res <- locations.stn$stn - locations.stn$rfe
				}
			}else{
				sp.trend <- xrfe
				locations.stn$res <- locations.stn$stn - locations.stn$rfe
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
			if(use.RnoR){
				rnr.rfe <- xrfe
				rnr.rfe[] <- 0
				rnr.rfe[xrfe >= wet.day] <- 1

				locations.stn$rnr.stn <- ifelse(locations.stn$stn >= wet.day, 1, 0)
				xadd$rnr.stn <- c(rnr.rfe[interp.grid$idxy$ix, interp.grid$idxy$iy])

				###########
				### binomial logistic regression
				glm.binom <- glm(rnr.stn ~ rfe, data = locations.stn, family = binomial(link = "logit"))

				############
				xadd$rnr.res <- 0
				locations.stn$rnr.res <- residuals(glm.binom)
				rnr <- predict(glm.binom, newdata = interp.grid$newgrid, type = 'link')
			}

			############
			if(interp.method == 'Kriging'){
				if(length(locations.stn$res) > 7){
					vgm <- try(autofitVariogram(formule, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
					vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
				}else vgm <- NULL
			}else vgm <- NULL

			############
			# create buffer for stations
			buffer.ina <- gBuffer(locations.stn, width = maxdist) ## ina apres interp
			buffer.grid <- gBuffer(locations.stn, width = maxdist*1.5) ## grid to interp
			buffer.xaddin <- gBuffer(locations.stn, width = maxdist/sqrt(2)) ## xadd in
			buffer.xaddout <- gBuffer(locations.stn, width = maxdist*2.5) ## xadd out

			if(use.RnoR){
				buffer.rnor <- gBuffer(locations.stn, width = maxdist.RnoR/sqrt(2)) ## outside rnor rfe
				buffer.grid.rnor <- gBuffer(locations.stn, width = maxdist.RnoR*1.5) ## grid rnor to interp

				irnr <- !as.logical(over(newdata, buffer.rnor))
				irnr[is.na(irnr)] <- TRUE

				igrid.rnr <- as.logical(over(newdata, buffer.grid.rnor))
				igrid.rnr[is.na(igrid.rnr)] <- FALSE
				newdata0.rnr <- newdata[igrid.rnr, ]
			}

			xadd.in <- !as.logical(over(xadd, buffer.xaddin))
			xadd.in[is.na(xadd.in)] <- TRUE
			xadd.out <- as.logical(over(xadd, buffer.xaddout))
			xadd.out[is.na(xadd.out)] <- FALSE
			iadd <- xadd.in & xadd.out

			xadd <- xadd[iadd, ]
			row.names(locations.stn) <- 1:length(locations.stn)
			row.names(xadd) <- length(locations.stn)+(1:length(xadd))
			locations.stn <- spRbind(locations.stn, xadd)

			igrid <- as.logical(over(newdata, buffer.grid))
			igrid[is.na(igrid)] <- FALSE
			newdata0 <- newdata[igrid, ]

			# irfe.out <- !as.logical(over(newdata, buffer.xaddout))
			# irfe.out[is.na(irfe.out)] <- TRUE

			###########
			if(any(is.auxvar)){
				locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
				locations.stn <- locations.stn[Reduce("&", locations.df), ]
				block <- NULL
			}else block <- bGrd

			res.grd <- krige(formule, locations = locations.stn, newdata = newdata0, model = vgm,
							block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)

			extrm1 <- min(locations.stn$res, na.rm = TRUE)
			res.grd$var1.pred[!is.na(res.grd$var1.pred) & res.grd$var1.pred < extrm1] <- extrm1
			extrm2  <- max(locations.stn$res, na.rm = TRUE)
			res.grd$var1.pred[!is.na(res.grd$var1.pred) & res.grd$var1.pred > extrm2] <- extrm2

			inside <- as.logical(over(res.grd, buffer.ina))
			inside[is.na(inside)] <- FALSE
			ina <- is.na(res.grd$var1.pred) & inside
			if(any(ina)){
				tmp.res.grd <- res.grd[!ina, ]
				tmp.res.grd <- tmp.res.grd[!is.na(tmp.res.grd$var1.pred), ]
				res.grd.na <- krige(var1.pred~1, locations = tmp.res.grd, newdata = newdata0[ina, ], model = vgm,
										block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				res.grd$var1.pred[ina] <- res.grd.na$var1.pred
				rm(tmp.res.grd, res.grd.na)
			}

			resid <- rep(0, length(newdata))
			resid[igrid] <- res.grd$var1.pred
			resid[is.na(resid)] <- 0
			resid <- matrix(resid, ncol = nlat0, nrow = nlon0)

			# sp.trend[irfe.out] <- xrfe[irfe.out]
			out.mrg <- sp.trend + resid
			out.mrg[out.mrg < 0] <- 0

			############
			if(use.RnoR){
				rnr.res.grd <- krige(rnr.res~1, locations = locations.stn, newdata = newdata0.rnr,
										maxdist = maxdist.RnoR, block = bGrd,  debug.level = 0)

				ina <- is.na(rnr.res.grd$var1.pred)
				if(any(ina)){
					rnr.res.grd.na <- krige(var1.pred~1, locations = rnr.res.grd[!ina, ], newdata = newdata0.rnr[ina, ],
												block = bGrd, maxdist = maxdist.RnoR, debug.level = 0)
					rnr.res.grd$var1.pred[ina] <- rnr.res.grd.na$var1.pred
				}

				rnr0 <- rep(0, length(newdata))
				rnr0[igrid.rnr] <- rnr.res.grd$var1.pred
				rnr0[is.na(rnr0)] <- 0

				rnr <- rnr + rnr0
				rnr[rnr > 100] <- 100
				rnr <- exp(rnr)/(1+exp(rnr))

				### decision boundary 0.6
				rnr[rnr >= 0.6] <- 1
				rnr[rnr < 0.6] <- 0

				rnr <- matrix(rnr, nrow = nlon0, ncol = nlat0)
				imsk <- matrix(irnr, nrow = nlon0, ncol = nlat0)

				rnr[imsk] <- rnr.rfe[imsk]
				rnr[is.na(rnr)] <- 1

				if(smooth.RnoR){
					npix <- if(sum(rnr.rfe, na.rm = TRUE) == 0) 5 else 3
					rnr <- smooth.matrix(rnr, npix)
				}
				rm(imsk, rnr.res.grd, rnr.res, rnr.rfe)
			}else rnr <- matrix(1, ncol = nlat0, nrow = nlon0)

			############
			out.mrg <- out.mrg * rnr
		}else out.mrg <- xrfe

		#Apply mask for area of interest
		if(!is.null(mrgParms$outMask)) out.mrg[is.na(mrgParms$outMask)] <- NA
		out.mrg[is.na(out.mrg)] <- -99

		############
		year <- substr(ncInfo$dates[jj], 1, 4)
		month <- substr(ncInfo$dates[jj], 5, 6)
		if(freqData == 'daily'){
			mrgfrmt <- sprintf(GeneralParameters$output$format, year, month, substr(ncInfo$dates[jj], 7, 8))
		}else if(freqData%in%c('pentad', 'dekadal')){
			mrgfrmt <- sprintf(GeneralParameters$output$format, year, month, substr(ncInfo$dates[jj], 7, 7))
		}else  mrgfrmt <- sprintf(GeneralParameters$output$format, year, month)

		outfl <- file.path(mrgParms$merge.DIR, mrgfrmt)
		nc2 <- nc_create(outfl, grd.nc.out)
		ncvar_put(nc2, grd.nc.out, round(out.mrg))
		nc_close(nc2)

		rm(out.mrg, sp.trend, rnr, resid, xadd, locations.stn, newdata, res.grd, coords.grd, xrfe)
		rm(imsk, rnr0, newdata0, newdata0.rnr)
		gc()
		return(0)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	InsertMessagesTxt(main.txt.out, 'Merging finished')
	return(0)
}


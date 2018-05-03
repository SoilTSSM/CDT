Precip_MergingFunctions <- function(mrgParms){
	InsertMessagesTxt(main.txt.out, 'Merge data ...')

	GeneralParameters <- mrgParms$GeneralParameters
	freqData <- GeneralParameters$period

	log.file <- file.path(mrgParms$merge.DIR, "log_file.txt")

	#############
	xy.grid <- mrgParms$interp.grid$grid
	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- mrgParms$interp.grid$nlon
	nlat0 <- mrgParms$interp.grid$nlat

	#############
	## Def ncdf
	dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	grd.nc.out <- ncvar_def("precip", "mm", list(dx, dy), -99,
							longname = "Merged Station-Satellite Rainfall",
							prec = "short", shuffle = TRUE, compression = 9)

	#############
	mrg.method <- GeneralParameters$Merging$mrg.method
	interp.method <- GeneralParameters$Merging$interp.method
	nmin <- GeneralParameters$Merging$nmin
	nmax <- GeneralParameters$Merging$nmax
	maxdist <- GeneralParameters$Merging$maxdist
	vgm.model <- GeneralParameters$Merging$vgm.model
	min.stn <- GeneralParameters$Merging$min.stn
	min.non.zero <- GeneralParameters$Merging$min.non.zero

	use.RnoR <- GeneralParameters$RnoR$use.RnoR
	maxdist.RnoR <- GeneralParameters$RnoR$maxdist.RnoR
	smooth.RnoR <- GeneralParameters$RnoR$smooth.RnoR
	wet.day <- GeneralParameters$RnoR$wet.day

	#############
	lon.stn <- mrgParms$stnData$lon
	lat.stn <- mrgParms$stnData$lat
	date.stn <- mrgParms$stnData$dates
	data.stn <- mrgParms$stnData$data
	nstn <- length(lon.stn)
	demData <- mrgParms$demData

	#############

	res.lon <- diff(range(xy.grid$lon))/(nlon0-1)
	res.lat <- diff(range(xy.grid$lat))/(nlat0-1)
	res.latlon <- max(res.lon, res.lat)

	if(maxdist < res.latlon) maxdist <- res.latlon*sqrt(2)
	res.coarse <- maxdist/sqrt(2)
	if(res.coarse < res.latlon) res.coarse <- maxdist

	if(!is.null(demData)){
		slpasp <- raster.slope.aspect(demData)
		demData$slp <- slpasp$slope
		demData$asp <- slpasp$aspect
	}else{
		demData <- list(x = xy.grid$lon, y = xy.grid$lat,
						z = matrix(1, nlon0, nlat0),
						slp = matrix(0, nlon0, nlat0),
						asp = matrix(0, nlon0, nlat0))
	}

	interp.grid <- createGrid.merging(demData, res.coarse = res.coarse)
	cells <- SpatialPixels(points = interp.grid$newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)))@grid
	bGrd <- if(GeneralParameters$Merging$use.block) createBlock(cells@cellsize, 1, 5) else NULL

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
	if(is.auxvar['lon']){
		interp.grid$coords.coarse$alon <- interp.grid$coords.coarse@coords[, 'lon']
		if(!is.null(interp.grid$coords.var)) interp.grid$coords.var$alon <- interp.grid$coords.var@coords[, 'lon']
		interp.grid$newgrid$alon <- interp.grid$newgrid@coords[, 'lon']
	}

	if(is.auxvar['lat']){
		interp.grid$coords.coarse$alat <- interp.grid$coords.coarse@coords[, 'lat']
		if(!is.null(interp.grid$coords.var)) interp.grid$coords.var$alat <- interp.grid$coords.var@coords[, 'lat']
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
	xlon <- nc$var[[ncInfo$ncinfo$varid]]$dim[[ncInfo$ncinfo$xo]]$vals
	xlat <- nc$var[[ncInfo$ncinfo$varid]]$dim[[ncInfo$ncinfo$yo]]$vals
	nc_close(nc)
	xo <- order(xlon)
	xlon <- xlon[xo]
	yo <- order(xlat)
	xlat <- xlat[yo]

	ijGrd <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = xlon, lat = xlat))

	#############
	packages <- c('ncdf4', 'gstat', 'automap', 'fields', 'rgeos', 'maptools')
	toExports <- c('writeNC.merging', 'smooth.matrix')

	is.parallel <- doparallel(length(which(ncInfo$exist)) >= 20)
	`%parLoop%` <- is.parallel$dofun
	ret <- foreach(jj = seq_along(ncInfo$nc.files), .packages = packages, .export = toExports) %parLoop% {
		if(ncInfo$exist[jj]){
			nc <- nc_open(ncInfo$nc.files[jj])
			xrfe <- ncvar_get(nc, varid = ncInfo$ncinfo$varid)
			nc_close(nc)
			xrfe <- if(ncInfo$ncinfo$xo < ncInfo$ncinfo$yo) xrfe[xo, yo] else t(xrfe)[xo, yo]
		}else {
			cat(paste(ncInfo$dates[jj], ":", "no RFE data", "|", "no file generated", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		if(all(is.na(xrfe))){
			cat(paste(ncInfo$dates[jj], ":", "all RFE data are missing", "|", "no file generated", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		############
		if(is.regridRFE){
			rfeGrid <- interp.surface.grid(list(x = xlon, y = xlat, z = xrfe), list(x = xy.grid$lon, y = xy.grid$lat))
			xrfe <- rfeGrid$z
		}

		############

		donne.stn <- data.stn[date.stn == ncInfo$dates[jj], , drop = FALSE]
		if(nrow(donne.stn) == 0){
			writeNC.merging(xrfe, ncInfo$dates[jj], freqData, grd.nc.out,
					mrgParms$merge.DIR, GeneralParameters$output$format)
			cat(paste(ncInfo$dates[jj], ":", "no station data", "|", "RFE data", "\n"), file = log.file, append = TRUE)
			return(NULL)
		}
		donne.stn <- data.frame(lon = lon.stn, lat = lat.stn, stn = c(donne.stn))
		stng <- createGrid.StnData(donne.stn, ijGrd, interp.grid$newgrid, min.stn, weighted = FALSE)
		if(is.null(stng)){
			writeNC.merging(xrfe, ncInfo$dates[jj], freqData, grd.nc.out,
					mrgParms$merge.DIR, GeneralParameters$output$format)
			cat(paste(ncInfo$dates[jj], ":", "not enough station data", "|", "RFE data", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		############
		locations.stn <- interp.grid$newgrid
		locations.stn$stn <- stng
		locations.stn$rfe <- c(xrfe)

		############
		xadd <- interp.grid$coords.coarse
		xadd$rfe <- xadd$stn <- c(xrfe[interp.grid$id.coarse$ix, interp.grid$id.coarse$iy])
		xadd$res <- 0

		############
		interp.grid$newgrid$rfe <- c(xrfe)
		newdata <- interp.grid$newgrid

		############
		noNA <- !is.na(locations.stn$stn)
		min.stn.nonNA <- length(which(noNA))
		nb.stn.nonZero <- length(which(noNA & locations.stn$stn > 0))
		locations.stn <- locations.stn[noNA, ]

		if(all(is.na(locations.stn$rfe))){
			writeNC.merging(xrfe, ncInfo$dates[jj], freqData, grd.nc.out,
					mrgParms$merge.DIR, GeneralParameters$output$format)
			cat(paste(ncInfo$dates[jj], ":", "all RFE data @ station location are missing", "|", "RFE data", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		############
		# spatial trend
		sp.trend <- xrfe
		locations.stn$res <- locations.stn$stn - locations.stn$rfe

		if(nb.stn.nonZero >= min.non.zero){
			if(mrg.method == "Spatio-Temporal LM"){
				mo <- as(substr(ncInfo$dates[jj], 5, 6), 'numeric')
				sp.trend <- xrfe * MODEL.COEF[[mo]]$slope + MODEL.COEF[[mo]]$intercept
				sp.trend[sp.trend < 0] <- 0
				locations.stn$res <- locations.stn$stn - sp.trend[noNA]
			}
			if(mrg.method == "Regression Kriging"){
				simplediff <- if(var(locations.stn$stn) < 1e-07 | var(locations.stn$rfe, na.rm = TRUE) < 1e-07) TRUE else FALSE
				if(simplediff){
					cat(paste(ncInfo$dates[jj], ":", "Zero variance", "|", "Simple Bias Adjustment", "\n"),
						file = log.file, append = TRUE)
				}

				glm.stn <- glm(formuleRK, data = locations.stn, family = gaussian)
				if(is.na(glm.stn$coefficients[2]) | glm.stn$coefficients[2] < 0){
					simplediff <- TRUE
					cat(paste(ncInfo$dates[jj], ":", "Invalid GLM coeffs", "|", "Simple Bias Adjustment", "\n"),
						file = log.file, append = TRUE)
				}
				if(!simplediff){
					sp.trend <- predict(glm.stn, newdata = interp.grid$newgrid)
					sp.trend <- matrix(sp.trend, ncol = nlat0, nrow = nlon0)
					ina.trend <- is.na(sp.trend)
					sp.trend[ina.trend] <- xrfe[ina.trend]
					sp.trend[sp.trend < 0] <- 0
					locations.stn$res <- NA
					if(length(glm.stn$na.action) > 0) locations.stn$res[-glm.stn$na.action] <- glm.stn$residuals
					else locations.stn$res <- glm.stn$residuals
				}
			}
		}else{
			cat(paste(ncInfo$dates[jj], ":", paste("Too much zero >", min.non.zero), "|", "Simple Bias Adjustment", "\n"),
				file = log.file, append = TRUE)
		}

		############
		locations.stn <- locations.stn[!is.na(locations.stn$res), ]

		if(length(locations.stn) < min.stn){
			writeNC.merging(xrfe, ncInfo$dates[jj], freqData, grd.nc.out,
					mrgParms$merge.DIR, GeneralParameters$output$format)
			cat(paste(ncInfo$dates[jj], ":", "not enough station data", "|", "RFE data", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		############
		if(any(is.auxvar)){
			locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
			locations.stn <- locations.stn[Reduce("&", locations.df), ]

			if(length(locations.stn) < min.stn){
				writeNC.merging(xrfe, ncInfo$dates[jj], freqData, grd.nc.out,
						mrgParms$merge.DIR, GeneralParameters$output$format)
				cat(paste(ncInfo$dates[jj], ":", "not enough station data combined with auxiliary var", "|", "RFE data", "\n"),
					file = log.file, append = TRUE)
				return(NULL)
			}
		}

		############
		if(interp.method == 'Kriging'){
			if(length(locations.stn$res) > 7){
				vgm <- try(autofitVariogram(formule, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
				vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
			}else{
				cat(paste(ncInfo$dates[jj], ":", "Unable to compute variogram", "|", "Interpolation using IDW", "\n"),
					file = log.file, append = TRUE)
				vgm <- NULL
			}
		}else vgm <- NULL

		############
		# create buffer for stations
		buffer.ina <- gBuffer(locations.stn, width = maxdist) ## ina apres interp
		buffer.grid <- gBuffer(locations.stn, width = maxdist*1.5) ## grid to interp
		buffer.xaddin <- gBuffer(locations.stn, width = maxdist/sqrt(2)) ## xadd in
		buffer.xaddout <- gBuffer(locations.stn, width = maxdist*2.5) ## xadd out

		############
		## inner interpolation grid
		igrid <- as.logical(over(newdata, buffer.grid))
		igrid[is.na(igrid)] <- FALSE
		newdata0 <- newdata[igrid, ]

		############
		# get coarse grid to add to location.stn
		xadd.in <- !as.logical(over(xadd, buffer.xaddin))
		xadd.in[is.na(xadd.in)] <- TRUE
		xadd.out <- as.logical(over(xadd, buffer.xaddout))
		xadd.out[is.na(xadd.out)] <- FALSE
		iadd <- xadd.in & xadd.out
		xadd <- xadd[iadd, ]
		## plot ici

		############
		## add coarse grid
		row.names(locations.stn) <- 1:length(locations.stn)
		row.names(xadd) <- length(locations.stn)+(1:length(xadd))
		locations.stn <- spRbind(locations.stn, xadd)

		###########
		if(any(is.auxvar)){
			locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
			locations.stn <- locations.stn[Reduce("&", locations.df), ]
			block <- NULL
		}else block <- bGrd

		###########
		# interpolate residual
		res.grd <- krige(formule, locations = locations.stn, newdata = newdata0, model = vgm,
						block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)

		###########
		# remove extreme residuals outside station range
		xtrm <- range(locations.stn$res, na.rm = TRUE)
		extrm1 <- xtrm[1]-diff(xtrm)*0.05
		extrm2 <- xtrm[2]+diff(xtrm)*0.05
		res.grd$var1.pred[!is.na(res.grd$var1.pred) & res.grd$var1.pred < extrm1] <- extrm1
		res.grd$var1.pred[!is.na(res.grd$var1.pred) & res.grd$var1.pred > extrm2] <- extrm2

		# fill missing inside interpolation buffer.ina
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

		###########
		resid <- rep(0, length(newdata))
		resid[igrid] <- res.grd$var1.pred
		resid[is.na(resid)] <- 0
		resid <- matrix(resid, ncol = nlat0, nrow = nlon0)

		###########
		out.mrg <- sp.trend + resid
		out.mrg[out.mrg < 0] <- 0

		###########

		if(mrg.method == "Regression Kriging"){
			bsmoo <- as.logical(over(newdata, buffer.ina))
			bsmoo[is.na(bsmoo)] <- FALSE
			mout.in <- !as.logical(over(newdata, buffer.xaddin))
			mout.in[is.na(mout.in)] <- TRUE
			imout <- mout.in & igrid

			out.tmp <- xrfe
			out.tmp[bsmoo] <- out.mrg[bsmoo]
			out.tmp <- smooth.matrix(out.tmp, 1)
			out.mrg[!igrid] <- xrfe[!igrid]
			out.mrg[imout] <- out.tmp[imout]
			rm(bsmoo, mout.in, imout, out.tmp)
		}

		###########

		rm(resid, sp.trend, res.grd, inside, ina, newdata0,
			igrid, locations.stn, xadd, xadd.in, xadd.out,
			buffer.ina, buffer.grid, buffer.xaddin, buffer.xaddout)

		###########
		# Rain-no-Rain
		rnr <- matrix(1, ncol = nlat0, nrow = nlon0)
		if(use.RnoR){
			rnr.stn <- ifelse(stng < wet.day+0.001, 0, 1)
			rnr.rfe <- ifelse(xrfe < wet.day+0.001, 0, 1)

			locations.stn <- interp.grid$newgrid
			locations.stn$rnr.stn <- rnr.stn
			locations.stn$rnr.rfe <- c(rnr.rfe)
			locations.stn <- locations.stn[, c('rnr.stn', 'rnr.rfe')]
			locations.stn <- locations.stn[!is.na(locations.stn$rnr.stn) & !is.na(locations.stn$rnr.rfe), ]

			if(length(locations.stn) < min.stn){
				writeNC.merging(out.mrg, ncInfo$dates[jj], freqData, grd.nc.out,
						mrgParms$merge.DIR, GeneralParameters$output$format)
				cat(paste(ncInfo$dates[jj], ":", "No rain-no-rain mask performed", "|", "Merged data", "\n"),
					file = log.file, append = TRUE)
				return(NULL)
			}

			###########
			xadd <- interp.grid$coords.coarse
			xadd$rnr.rfe <- xadd$rnr.stn <- c(rnr.rfe[interp.grid$id.coarse$ix, interp.grid$id.coarse$iy])
			xadd <- xadd[, c('rnr.stn', 'rnr.rfe')]
			xadd$rnr.res <- 0

			###########
			# binomial logistic regression
			newdata.glm <- interp.grid$newgrid
			newdata.glm$rnr.rfe <- c(rnr.rfe)
			glm.binom <- glm(rnr.stn ~ rnr.rfe, data = locations.stn, family = binomial(link = "logit"))
			locations.stn$rnr.res <- residuals(glm.binom)
			rnr <- predict(glm.binom, newdata = newdata.glm, type = 'link')
			rm(newdata.glm)

			###########
			buffer.rnor.in <- gBuffer(locations.stn, width = maxdist.RnoR/sqrt(2)) ## in buff
			buffer.rnor.out <- gBuffer(locations.stn, width = maxdist.RnoR*2.5) ## out buff
			buffer.rnor.grid <- gBuffer(locations.stn, width = maxdist.RnoR*1.5) ## grid rnor to interp

			############
			# get coarse grid to add to location.stn
			xadd.in <- !as.logical(over(xadd, buffer.rnor.in))
			xadd.in[is.na(xadd.in)] <- TRUE
			xadd.out <- as.logical(over(xadd, buffer.rnor.out))
			xadd.out[is.na(xadd.out)] <- FALSE
			iadd <- xadd.in & xadd.out
			xadd <- xadd[iadd, ]

			###########
			row.names(locations.stn) <- 1:length(locations.stn)
			row.names(xadd) <- length(locations.stn)+(1:length(xadd))
			locations.stn <- spRbind(locations.stn, xadd)

			###########

			buff.rnr <- as.logical(over(newdata, buffer.rnor.grid))

			## rnr interpolation grid
			igrid.rnr <- buff.rnr
			igrid.rnr[is.na(igrid.rnr)] <- FALSE
			newdata0.rnr <- newdata[igrid.rnr, ]

			## rnr.rfe outside interp grid
			irnr <- !buff.rnr
			irnr[is.na(irnr)] <- TRUE

			###########
			rnr.res.grd <- krige(rnr.res~1, locations = locations.stn, newdata = newdata0.rnr, maxdist = maxdist.RnoR, debug.level = 0)
			ina <- is.na(rnr.res.grd$var1.pred)
			if(any(ina)){
				rnr.res.grd.na <- krige(var1.pred~1, locations = rnr.res.grd[!ina, ],
										newdata = newdata0.rnr[ina, ], maxdist = maxdist.RnoR, debug.level = 0)
				rnr.res.grd$var1.pred[ina] <- rnr.res.grd.na$var1.pred
			}

			rnr0 <- rep(0, length(newdata))
			rnr0[igrid.rnr] <- rnr.res.grd$var1.pred
			rnr0[is.na(rnr0)] <- 0

			rnr <- rnr + rnr0
			rnr <- exp(rnr)/(1+exp(rnr))
			### decision boundary 0.5
			rnr[rnr >= 0.5] <- 1
			rnr[rnr < 0.5] <- 0

			rnr[!is.na(rnr.stn)] <- rnr.stn[!is.na(rnr.stn)]
			rnr <- matrix(rnr, nrow = nlon0, ncol = nlat0)
			imsk <- matrix(irnr, nrow = nlon0, ncol = nlat0)
			rnr[imsk] <- rnr.rfe[imsk]
			rnr[is.na(rnr)] <- 1

			if(smooth.RnoR){
				# buffer to smooth between stn convex hull and interp grid
				rnr.in <- !as.logical(over(newdata, buffer.rnor.in))
				rnr.in[is.na(rnr.in)] <- TRUE
				rnr.out <- as.logical(over(newdata, buffer.rnor.grid))
				rnr.out[is.na(rnr.out)] <- FALSE
				irnr <- rnr.in & rnr.out
				rnr0 <- smooth.matrix(rnr, 1)
				rnr[irnr] <- rnr0[irnr]
				rnr[imsk] <- rnr.rfe[imsk]
				rm(rnr.in, rnr.out)
			}

			rm(rnr0, imsk, irnr, rnr.res.grd, newdata0.rnr, igrid.rnr, buff.rnr,
				iadd, xadd, xadd.out, xadd.in, locations.stn, rnr.stn, rnr.stn,
				buffer.rnor.in, buffer.rnor.out, buffer.rnor.grid)
		}

		###########

		out.mrg <- out.mrg * rnr

		#Apply mask for area of interest
		if(!is.null(mrgParms$outMask)) out.mrg[is.na(mrgParms$outMask)] <- NA

		writeNC.merging(out.mrg, ncInfo$dates[jj], freqData, grd.nc.out,
				mrgParms$merge.DIR, GeneralParameters$output$format)

		rm(out.mrg, rnr, newdata)
		gc()
		return(0)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	InsertMessagesTxt(main.txt.out, 'Merging finished')
	return(0)
}


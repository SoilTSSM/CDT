LOOCV_MergingDataExec <- function(GeneralParameters){

	if(GeneralParameters$outdir%in%c("", "NA")){
		InsertMessagesTxt(main.txt.out, "Directory to save results doesn't exist", format = TRUE)
		return(NULL)
	}

	outValidation <- file.path(GeneralParameters$outdir, paste0('LOOCValidation_', getf.no.ext(GeneralParameters$STN.file)))
	dir.create(outValidation, showWarnings = FALSE, recursive = TRUE)

	freqData <- GeneralParameters$Tstep

	##################
	## Get data
	stnData <- getStnOpenData(GeneralParameters$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData)
	if(is.null(stnData)) return(NULL)

	##################
	## RFE sample file
	ncDataInfo <- getRFESampleData(GeneralParameters$NCDF$sample)
	if(is.null(ncDataInfo)){
		InsertMessagesTxt(main.txt.out, "No netcdf data sample found", format = TRUE)
		return(NULL)
	}

	##################
	## DEM data
	demData <- NULL
	if(GeneralParameters$auxvar$dem |
	   GeneralParameters$auxvar$slope |
	   GeneralParameters$auxvar$aspect)
	{
		demData <- getDemOpenDataSPDF(GeneralParameters$DEM.file)
		if(is.null(demData)){
			InsertMessagesTxt(main.txt.out, "No elevation data found", format = TRUE)
			return(NULL)
		}
	}

	##################
	nlon0 <- length(ncDataInfo$lon)
	nlat0 <- length(ncDataInfo$lat)
	xy.grid <- list(lon = ncDataInfo$lon, lat = ncDataInfo$lat)

	##################
	## regrid DEM data
	if(!is.null(demData)){
		is.regridDEM <- is.diffSpatialPixelsObj(defSpatialPixels(xy.grid), defSpatialPixels(demData[c('lon', 'lat')]), tol = 1e-07)
		demData <- list(x = demData$lon, y = demData$lat, z = demData$demMat)
		if(is.regridDEM) demData <- interp.surface.grid(demData, list(x = xy.grid$lon, y = xy.grid$lat))
		demData$z[demData$z < 0] <- 0
	}

	##################
	## Get NCDF data info
	start.year <- GeneralParameters$Merging.Date$start.year
	start.mon <- GeneralParameters$Merging.Date$start.mon
	start.dek <- GeneralParameters$Merging.Date$start.dek
	end.year <- GeneralParameters$Merging.Date$end.year
	end.mon <- GeneralParameters$Merging.Date$end.mon
	end.dek <- GeneralParameters$Merging.Date$end.dek
	months <- GeneralParameters$Merging.Date$Months
	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	NCDF.DIR <- GeneralParameters$NCDF$dir
	NCDF.Format <- GeneralParameters$NCDF$format

	errmsg <- "NCDF data not found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, NCDF.DIR, NCDF.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- list(xo = ncDataInfo$rfeILon, yo = ncDataInfo$rfeILat, varid = ncDataInfo$rfeVarid)
	# ncInfo$xy.nc <- list(lon = ncDataInfo$lon, lat = ncDataInfo$lat)

	mrgParms <- list(GeneralParameters = GeneralParameters, months = months, ncInfo = ncInfo,
					stnData = stnData, demData = demData, merge.DIR = outValidation,
					interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))

	ret <- LOOCV_MergingDataProcs(mrgParms)

	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)

}

############################################################

LOOCV_MergingDataProcs <- function(mrgParms){
	InsertMessagesTxt(main.txt.out, 'Leave-one-out cross-validation ...')

	GeneralParameters <- mrgParms$GeneralParameters
	freqData <- GeneralParameters$Tstep

	#############
	xy.grid <- mrgParms$interp.grid$grid
	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- mrgParms$interp.grid$nlon
	nlat0 <- mrgParms$interp.grid$nlat

	#############
	mrg.method <- GeneralParameters$Merging$mrg.method
	interp.method <- GeneralParameters$Merging$interp.method
	nmin <- GeneralParameters$Merging$nmin
	nmax <- GeneralParameters$Merging$nmax
	maxdist <- GeneralParameters$Merging$maxdist
	vgm.model <- GeneralParameters$Merging$vgm.model
	min.stn <- GeneralParameters$Merging$min.stn

	if(GeneralParameters$clim.var == "RR"){
		min.non.zero <- GeneralParameters$Merging$min.non.zero
		use.RnoR <- GeneralParameters$RnoR$use.RnoR
		maxdist.RnoR <- GeneralParameters$RnoR$maxdist.RnoR
		wet.day <- GeneralParameters$RnoR$wet.day
	}

	res.coarse <- maxdist/2
	sarea <- prod(apply(sapply(xy.grid, range), 2, diff))
	min.thres <- 0.9*(1-exp(-3*(sarea/567.5)^2))+0.1
	res.coarse <- if(res.coarse >= min.thres) res.coarse else min.thres
	maxdist <- if(maxdist < res.coarse) res.coarse else maxdist

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

	is.parallel <- doparallel(length(which(ncInfo$exist)) >= 10)
	`%parLoop%` <- is.parallel$dofun

	if(GeneralParameters$clim.var == "RR"){
		results.cv <- foreach(jj = seq_along(ncInfo$nc.files), .packages = packages) %parLoop% {
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
			locations.stn <- interp.grid$coords.stn
			donne.stn <- data.stn[date.stn == ncInfo$dates[jj], , drop = FALSE]
			if(nrow(donne.stn) == 0) return(NULL)
			locations.stn$stn <- c(donne.stn[1, ])
			locations.stn$rfe <- xrfe[ijGrd]
			locations.stn$ID <- seq_along(locations.stn)

			xadd <- interp.grid$coords.grd
			xadd$rfe <- xadd$stn <- c(xrfe[interp.grid$idxy$ix, interp.grid$idxy$iy])
			xadd$ID <- 0
			xadd$res <- 0

			if(use.RnoR){
				rnr.rfe <- xrfe
				rnr.rfe[] <- 0
				rnr.rfe[xrfe >= wet.day] <- 1
				xadd$rnr.stn <- c(rnr.rfe[interp.grid$idxy$ix, interp.grid$idxy$iy])
				xadd$rnr.res <- 0
			}

			interp.grid$newgrid$rfe <- c(xrfe)

			############
			noNA <- !is.na(locations.stn$stn)
			min.stn.nonNA <- length(which(noNA))
			locations.stn <- locations.stn[noNA, ]

			if(all(is.na(locations.stn$rfe))) return(NULL)

			############
			if(mrg.method == "Spatio-Temporal LM"){
				mo <- as(substr(ncInfo$dates[jj], 5, 6), 'numeric')
				LM.trend <- xrfe * MODEL.COEF[[mo]]$slope + MODEL.COEF[[mo]]$intercept
				LM.trend <- LM.trend[ijGrd][noNA]
			}

			############
			OUT.CV <- rep(NA, length(locations.stn))

			for(ii in seq_along(locations.stn)){
				locations.stn0 <- locations.stn[-ii, ]
				newdata0 <- locations.stn[ii, names(interp.grid$newgrid)]
				nb.stn.nonZero <- length(which(locations.stn0$stn > 0))

				############
				if(min.stn.nonNA >= min.stn){
					do.merging <- TRUE

					sp.trend <- locations.stn$rfe[ii]
					locations.stn0$res <- locations.stn0$stn - locations.stn0$rfe

					if(nb.stn.nonZero >= min.non.zero){
						if(mrg.method == "Spatio-Temporal LM"){
							locations.stn0$res <- locations.stn0$stn - LM.trend[-ii]
							sp.trend <- LM.trend[ii]
						}
						if(mrg.method == "Regression Kriging"){
							simplediff <- if(var(locations.stn0$stn) < 1e-07 | var(locations.stn0$rfe, na.rm = TRUE) < 1e-07) TRUE else FALSE
							glm.stn <- glm(formuleRK, data = locations.stn0, family = gaussian)
							if(is.na(glm.stn$coefficients[2]) | glm.stn$coefficients[2] < 0) simplediff <- TRUE
							if(!simplediff){
								sp.trend <- predict(glm.stn, newdata = newdata0)
								sp.trend <- if(is.na(sp.trend)) locations.stn$rfe[ii] else sp.trend
								locations.stn0$res <- NA
								if(length(glm.stn$na.action) > 0) locations.stn0$res[-glm.stn$na.action] <- glm.stn$residuals
								else locations.stn0$res <- glm.stn$residuals
							}
						}
					}

					locations.stn0 <- locations.stn0[!is.na(locations.stn0$res), ]
					############
					if(any(is.auxvar)){
						locations.df <- as.data.frame(!is.na(locations.stn0@data[, auxvar[is.auxvar]]))
						locations.stn0 <- locations.stn0[Reduce("&", locations.df), ]
					}
					if(length(locations.stn0) < min.stn) do.merging <- FALSE
				}else do.merging <- FALSE

				############

				if(do.merging){
					if(use.RnoR){
						locations.stn0$rnr.stn <- ifelse(locations.stn0$stn >= wet.day, 1, 0)
						glm.binom <- glm(rnr.stn ~ rfe, data = locations.stn0, family = binomial(link = "logit"))
						locations.stn0$rnr.res <- residuals(glm.binom)
						rnr <- predict(glm.binom, newdata = newdata0, type = 'link')
					}

					############
					if(interp.method == 'Kriging'){
						if(length(locations.stn0$res) > 7){
							vgm <- try(autofitVariogram(formule, input_data = locations.stn0, model = vgm.model, cressie = TRUE), silent = TRUE)
							vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
						}else vgm <- NULL
					}else vgm <- NULL

					############
					# create buffer for stations
					buffer.xaddin <- gBuffer(locations.stn0, width = maxdist/sqrt(2)) ## xadd in
					# buffer.xaddout <- gBuffer(locations.stn0, width = maxdist*2.5) ## xadd out

					if(use.RnoR){
						buffer.rnor <- gBuffer(locations.stn0, width = maxdist.RnoR/sqrt(2)) ## outside rnor rfe
						irnr <- !as.logical(over(newdata0, buffer.rnor))
						irnr[is.na(irnr)] <- TRUE
					}

					xadd.in <- !as.logical(over(xadd, buffer.xaddin))
					xadd.in[is.na(xadd.in)] <- TRUE
					# xadd.out <- as.logical(over(xadd, buffer.xaddout))
					# xadd.out[is.na(xadd.out)] <- FALSE
					# iadd <- xadd.in & xadd.out
					iadd <- xadd.in
					xadd0 <- xadd[iadd, ]

					row.names(locations.stn0) <- 1:length(locations.stn0)
					row.names(xadd0) <- length(locations.stn0)+(1:length(xadd0))
					locations.stn0 <- spRbind(locations.stn0, xadd0)

					###########
					if(any(is.auxvar)){
						locations.df <- as.data.frame(!is.na(locations.stn0@data[, auxvar[is.auxvar]]))
						locations.stn0 <- locations.stn0[Reduce("&", locations.df), ]
						block <- NULL
					}else block <- bGrd

					res.grd <- krige(formule, locations = locations.stn0, newdata = newdata0, model = vgm,
									block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)

					extrm1 <- min(locations.stn0$res, na.rm = TRUE)
					res.grd$var1.pred[!is.na(res.grd$var1.pred) & res.grd$var1.pred < extrm1] <- extrm1
					extrm2  <- max(locations.stn0$res, na.rm = TRUE)
					res.grd$var1.pred[!is.na(res.grd$var1.pred) & res.grd$var1.pred > extrm2] <- extrm2

					resid <- res.grd$var1.pred
					resid[is.na(resid)] <- 0
					out.mrg <- sp.trend + resid
					out.mrg[out.mrg < 0] <- 0

					############
					if(use.RnoR){
						rnr.res.grd <- krige(rnr.res~1, locations = locations.stn0, newdata = newdata0,
												maxdist = maxdist.RnoR, block = bGrd,  debug.level = 0)
						rnr0 <- rnr.res.grd$var1.pred
						rnr0[is.na(rnr0)] <- 0

						rnr <- rnr + rnr0
						rnr[rnr > 100] <- 100
						rnr <- exp(rnr)/(1+exp(rnr))

						### decision boundary 0.6
						rnr[rnr >= 0.6] <- 1
						rnr[rnr < 0.6] <- 0

						if(irnr) rnr <- rnr.rfe[ijGrd][noNA][ii]
						rnr[is.na(rnr)] <- 1
					}else rnr <- 1

					############
					OUT.CV[ii] <- out.mrg * rnr
				}else OUT.CV[ii] <- locations.stn$rfe[ii]
			}
			out.cv <- cbind(locations.stn$ID, round(cbind(locations.stn$stn, OUT.CV), 1))
			return(out.cv)
		}
	}

	if(GeneralParameters$clim.var == "TT"){
		results.cv <- foreach(jj = seq_along(ncInfo$nc.files), .packages = packages) %parLoop% {
			if(ncInfo$exist[jj]){
				nc <- nc_open(ncInfo$nc.files[jj])
				xtmp <- ncvar_get(nc, varid = nc$var[[1]]$name)
				nc_close(nc)
			}else return(NULL)

			if(all(is.na(xtmp))) return(NULL)

			############
			locations.stn <- interp.grid$coords.stn
			donne.stn <- data.stn[date.stn == ncInfo$dates[jj], , drop = FALSE]
			if(nrow(donne.stn) == 0) return(NULL)
			locations.stn$stn <- c(donne.stn[1, ])
			locations.stn$tmp <- xtmp[ijGrd]
			locations.stn$ID <- seq_along(locations.stn)

			xadd <- interp.grid$coords.grd
			xadd$tmp <- xadd$stn <- c(xtmp[interp.grid$idxy$ix, interp.grid$idxy$iy])
			xadd$ID <- 0
			xadd$res <- 0

			interp.grid$newgrid$tmp <- c(xtmp)

			############
			noNA <- !is.na(locations.stn$stn)
			locations.stn <- locations.stn[noNA, ]

			############
			if(mrg.method == "Spatio-Temporal LM"){
				mo <- as(substr(ncInfo$dates[jj], 5, 6), 'numeric')
				LM.trend <- xtmp * MODEL.COEF[[mo]]$slope + MODEL.COEF[[mo]]$intercept
				LM.trend <- LM.trend[ijGrd][noNA]
			}

			############

			OUT.CV <- rep(NA, length(locations.stn))

			for(ii in seq_along(locations.stn)){
				locations.stn0 <- locations.stn[-ii, ]
				newdata0 <- locations.stn[ii, names(interp.grid$newgrid)]

				############
				noNA0 <- !is.na(locations.stn0$tmp)
				min.stn.nonNA <- length(which(noNA0))
				locations.stn0 <- locations.stn0[noNA0, ]

				############
				if(min.stn.nonNA >= min.stn){
					do.merging <- TRUE
					if(mrg.method == "Spatio-Temporal LM"){
						locations.stn0$res <- locations.stn0$stn - LM.trend[noNA0][-ii]
						sp.trend <- LM.trend[noNA0][ii]
					}else if(mrg.method == "Regression Kriging"){
						glm.stn <- glm(formuleRK, data = locations.stn0, family = gaussian)
						sp.trend <- predict(glm.stn, newdata = newdata0)
						sp.trend <- if(is.na(sp.trend)) locations.stn$tmp[ii] else sp.trend
						locations.stn0$res <- glm.stn$residuals
					}else{
						sp.trend <- locations.stn$tmp[ii]
						locations.stn0$res <- locations.stn0$stn - locations.stn0$tmp
					}
					
					locations.stn0 <- locations.stn0[!is.na(locations.stn0$res), ]
					############
					if(any(is.auxvar)){
						locations.df <- as.data.frame(!is.na(locations.stn0@data[, auxvar[is.auxvar]]))
						locations.stn0 <- locations.stn0[Reduce("&", locations.df), ]
					}
					if(length(locations.stn0) < min.stn) do.merging <- FALSE
				}else do.merging <- FALSE

				############

				if(do.merging){
					if(interp.method == 'Kriging'){
						if(length(locations.stn0$res) > 7){
							vgm <- try(autofitVariogram(formule, input_data = locations.stn0, model = vgm.model, cressie = TRUE), silent = TRUE)
							vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
						}else vgm <- NULL
					}else vgm <- NULL

					# # ############
					# # create buffer for stations
					buffer.xaddin <- gBuffer(locations.stn0, width = maxdist/sqrt(2)) ## xadd in
					# buffer.xaddout <- gBuffer(locations.stn0, width = maxdist*2.5) ## xadd out

					xadd.in <- !as.logical(over(xadd, buffer.xaddin))
					xadd.in[is.na(xadd.in)] <- TRUE
					# xadd.out <- as.logical(over(xadd, buffer.xaddout))
					# xadd.out[is.na(xadd.out)] <- FALSE
					# iadd <- xadd.in & xadd.out
					iadd <- xadd.in
					xadd0 <- xadd[iadd, ]

					row.names(locations.stn0) <- 1:length(locations.stn0)
					row.names(xadd0) <- length(locations.stn0)+(1:length(xadd0))
					locations.stn0 <- spRbind(locations.stn0, xadd0)

					###########
					if(any(is.auxvar)){
						locations.df <- as.data.frame(!is.na(locations.stn0@data[, auxvar[is.auxvar]]))
						locations.stn0 <- locations.stn0[Reduce("&", locations.df), ]
						block <- NULL
					}else block <- bGrd

					res.grd <- krige(formule, locations = locations.stn0, newdata = newdata0, model = vgm,
									block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)

					extrm1 <- min(locations.stn0$res, na.rm = TRUE)
					res.grd$var1.pred[!is.na(res.grd$var1.pred) & res.grd$var1.pred < extrm1] <- extrm1
					extrm2  <- max(locations.stn0$res, na.rm = TRUE)
					res.grd$var1.pred[!is.na(res.grd$var1.pred) & res.grd$var1.pred > extrm2] <- extrm2
					resid <- res.grd$var1.pred
					resid[is.na(resid)] <- 0
					OUT.CV[ii] <- sp.trend + resid
				}else OUT.CV[ii] <- locations.stn$tmp[ii]
			}

			out.cv <- cbind(locations.stn$ID, round(cbind(locations.stn$stn, OUT.CV), 1))
			return(out.cv)
		}
	}

	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	#############

	isnull <- sapply(results.cv, is.null)
	daty <- ncInfo$dates[!isnull]
	results.cv <- results.cv[!isnull]

	INITSTN <- rep(NA, nstn)
	ret <- lapply(results.cv, function(x){
		FCST <- OBS <- INITSTN
		OBS[x[, 1]] <- x[, 2]
		FCST[x[, 1]] <- x[, 3]
		list(obs = OBS, fcst = FCST)
	})

	OBS <- do.call(rbind, lapply(ret, function(x) x$obs))
	FCST <- do.call(rbind, lapply(ret, function(x) x$fcst))

	xhead <- cbind(c("STN", "LON", "DATE/LAT"), rbind(mrgParms$stnData$id,
					mrgParms$stnData$lon, mrgParms$stnData$lat))

	obs2file <- rbind(xhead, cbind(mrgParms$stnData$dates, OBS))
	fcst2file <- rbind(xhead, cbind(mrgParms$stnData$dates, FCST))
	obs2file[is.na(obs2file)] <- -99
	fcst2file[is.na(fcst2file)] <- -99

	dirCDTdata <- file.path(mrgParms$merge.DIR, "OBS_FCST_DATA")
	dir.create(dirCDTdata, showWarnings = FALSE, recursive = TRUE)
	writeFiles(obs2file, file.path(dirCDTdata, "Observations.csv"))
	writeFiles(fcst2file, file.path(dirCDTdata, "Merged.csv"))

	EnvLOOCValidation$cdtData$info <- mrgParms$stnData[c('id', 'lon', 'lat')]
	EnvLOOCValidation$cdtData$dates <- daty
	EnvLOOCValidation$cdtData$obs <- OBS
	EnvLOOCValidation$cdtData$fcst <- FCST
	EnvLOOCValidation$GeneralParameters <- GeneralParameters

	fileValidOut <- file.path(mrgParms$merge.DIR, "VALIDATION_DATA_OUT.rds")
	saveRDS(EnvLOOCValidation, file = fileValidOut)

	InsertMessagesTxt(main.txt.out, 'Leave-one-out cross-validation finished')
	return(0)
}


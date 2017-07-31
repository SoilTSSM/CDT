
Temp_MergingFunctions <- function(mrgParms){
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
	grd.nc.out <- ncvar_def('temp', "DegC", list(dx, dy), -99, longname = 'Reanalysis merged with station', prec = "float")

	#############
	mrg.method <- GeneralParameters$Merging$mrg.method
	interp.method <- GeneralParameters$Merging$interp.method
	nmin <- GeneralParameters$Merging$nmin
	nmax <- GeneralParameters$Merging$nmax
	maxdist <- GeneralParameters$Merging$maxdist
	vgm.model <- GeneralParameters$Merging$vgm.model
	min.stn <- GeneralParameters$Merging$min.stn

	res.coarse <- maxdist/2
	res.coarse <- if(res.coarse  >= 0.25) res.coarse else 0.25
	maxdist <- if(maxdist < res.coarse) res.coarse else maxdist

	#############
	auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
	is.auxvar <- unlist(GeneralParameters$auxvar)
	if(any(is.auxvar)){
		formule <- formula(paste0('res', '~', paste(auxvar[is.auxvar], collapse = '+')))
		if(mrg.method == "Regression Kriging"){
			sp.trend.aux <- GeneralParameters$sp.trend.aux
			if(sp.trend.aux) formuleRK <- formula(paste0('stn', '~', 'tmp', '+', paste(auxvar[is.auxvar], collapse = '+')))
			else formuleRK <- formula(paste0('stn', '~', 'tmp'))
		}
	}else{
		formule <- formula(paste0('res', '~', 1))
		if(mrg.method == "Regression Kriging") formuleRK <- formula(paste0('stn', '~', 'tmp'))
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
		demres <- grdSp@grid@cellsize
		slpasp <- slope.aspect(demData$z, demres[1], demres[2], filter = "sobel")
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
		coefFiles <- file.path(mrgParms$LMCoef.DIR, paste0('LM_Coefficient_', months, '.nc'))
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

	## gridded data at stn loc
	ijGrd <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = xlon, lat = xlat))

	#############
	packages <- c('ncdf4', 'gstat', 'automap')

	is.parallel <- doparallel(length(which(ncInfo$exist)) >= 10)
	`%parLoop%` <- is.parallel$dofun
	ret <- foreach(jj = seq_along(ncInfo$nc.files), .packages = packages) %parLoop% {
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
			if(mrg.method == "Spatio-Temporal LM"){
				mo <- as(substr(ncInfo$dates[jj], 5, 6), 'numeric')
				sp.trend <- xtmp * MODEL.COEF[[mo]]$slope + MODEL.COEF[[mo]]$intercept
				locations.stn$res <- locations.stn$stn - sp.trend[ijGrd][noNA]
			}else if(mrg.method == "Regression Kriging"){
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
				vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
			}else vgm <- NULL

			############
			xstn <- as.data.frame(locations.stn)
			iadd <- rep(TRUE, nrow(xadd))
			for(k in 1:nrow(xstn)){
				dst <- sqrt((xstn$lon[k]-xadd$lon)^2+(xstn$lat[k]-xadd$lat)^2)*sqrt(2)
				iadd <- iadd & (dst >= maxdist)
			}

			xadd <- xadd[iadd, ]
			locations.stn <- rbind(xstn, xadd)
			coordinates(locations.stn) <- ~lon+lat
			newdata <- interp.grid$newgrid

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

			out.mrg <- sp.trend + resid
		}else out.mrg <- xtmp

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

		rm(out.mrg, sp.trend, resid, xadd, locations.stn, newdata, res.grd, coords.grd, xtmp)
		gc()
		return(0)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	InsertMessagesTxt(main.txt.out, 'Merging finished')
	return(0)
}

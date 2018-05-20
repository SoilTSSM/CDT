
Temp_MergingFunctions <- function(mrgParms){
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
	grd.nc.out <- ncvar_def('temp', "DegC", list(dx, dy), -99,
							longname = 'Reanalysis merged with station',
							prec = "float", compression = 9)

	#############
	mrg.method <- GeneralParameters$Merging$mrg.method
	interp.method <- GeneralParameters$Merging$interp.method
	nmin <- GeneralParameters$Merging$nmin
	nmax <- GeneralParameters$Merging$nmax
	maxdist <- GeneralParameters$Merging$maxdist
	vgm.model <- GeneralParameters$Merging$vgm.model
	min.stn <- GeneralParameters$Merging$min.stn

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
			if(sp.trend.aux) formuleRK <- formula(paste0('stn', '~', 'tmp', '+', paste(auxvar[is.auxvar], collapse = '+')))
			else formuleRK <- formula(paste0('stn', '~', 'tmp'))
		}
	}else{
		formule <- formula(paste0('res', '~', 1))
		if(mrg.method == "Regression Kriging") formuleRK <- formula(paste0('stn', '~', 'tmp'))
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

	nc <- nc_open(ncInfo$nc.files[which(ncInfo$exist)[1]])
	xlon <- nc$var[[ncInfo$ncinfo$varid]]$dim[[ncInfo$ncinfo$xo]]$vals
	xlat <- nc$var[[ncInfo$ncinfo$varid]]$dim[[ncInfo$ncinfo$yo]]$vals
	nc_close(nc)
	xo <- order(xlon)
	xlon <- xlon[xo]
	yo <- order(xlat)
	xlat <- xlat[yo]

	## gridded data at stn loc
	ijGrd <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = xlon, lat = xlat))

	#############
	packages <- c('ncdf4', 'gstat', 'automap', 'rgeos', 'maptools')
	toExports <- c('writeNC.merging', 'createGrid.StnData', 'smooth.matrix')

	is.parallel <- doparallel(length(which(ncInfo$exist)) >= 10)
	`%parLoop%` <- is.parallel$dofun
	ret <- foreach(jj = seq_along(ncInfo$nc.files), .packages = packages, .export = toExports) %parLoop% {
		if(ncInfo$exist[jj]){
			nc <- nc_open(ncInfo$nc.files[jj])
			xtmp <- ncvar_get(nc, varid = ncInfo$ncinfo$varid)
			nc_close(nc)
			xtmp <- if(ncInfo$ncinfo$xo < ncInfo$ncinfo$yo) xtmp[xo, yo] else t(xtmp)[xo, yo]
		}else{
			cat(paste(ncInfo$dates[jj], ":", "no temperature data", "|", "no file generated", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		if(all(is.na(xtmp))){
			cat(paste(ncInfo$dates[jj], ":", "all data are missing", "|", "no file generated", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		############

		donne.stn <- data.stn[date.stn == ncInfo$dates[jj], , drop = FALSE]
		if(nrow(donne.stn) == 0){
			writeNC.merging(xtmp, ncInfo$dates[jj], freqData, grd.nc.out,
					mrgParms$merge.DIR, GeneralParameters$output$format)
			cat(paste(ncInfo$dates[jj], ":", "no station data", "|", "Input temperature data", "\n"), file = log.file, append = TRUE)
			return(NULL)
		}
		donne.stn <- data.frame(lon = lon.stn, lat = lat.stn, stn = c(donne.stn))
		stng <- createGrid.StnData(donne.stn, ijGrd, interp.grid$newgrid, min.stn, weighted = FALSE)
		if(is.null(stng)){
			writeNC.merging(xtmp, ncInfo$dates[jj], freqData, grd.nc.out,
					mrgParms$merge.DIR, GeneralParameters$output$format)
			cat(paste(ncInfo$dates[jj], ":", "not enough station data", "|", "Input temperature data", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		############
		locations.stn <- interp.grid$newgrid
		locations.stn$stn <- stng
		locations.stn$tmp <- c(xtmp)

		############
		xadd <- interp.grid$coords.coarse
		xadd$tmp <- xadd$stn <- c(xtmp[interp.grid$id.coarse$ix, interp.grid$id.coarse$iy])
		xadd$res <- 0

		############
		interp.grid$newgrid$tmp <- c(xtmp)
		newdata <- interp.grid$newgrid

		############
		noNA <- !is.na(locations.stn$stn)
		locations.stn <- locations.stn[noNA, ]

		if(all(is.na(locations.stn$tmp))){
			writeNC.merging(xtmp, ncInfo$dates[jj], freqData, grd.nc.out,
					mrgParms$merge.DIR, GeneralParameters$output$format)
			cat(paste(ncInfo$dates[jj], ":", "all temperature data @ station location are missing", "|", "Input temperature data", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		############
		# spatial trend
		sp.trend <- xtmp
		locations.stn$res <- locations.stn$stn - locations.stn$tmp

		if(mrg.method == "Spatio-Temporal LM"){
			mo <- as(substr(ncInfo$dates[jj], 5, 6), 'numeric')
			sp.trend <- xtmp * MODEL.COEF[[mo]]$slope + MODEL.COEF[[mo]]$intercept
			locations.stn$res <- locations.stn$stn - sp.trend[noNA]
		}
		if(mrg.method == "Regression Kriging"){
			simplediff <- if(var(locations.stn$stn) < 1e-07 | var(locations.stn$tmp, na.rm = TRUE) < 1e-07) TRUE else FALSE
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
				sp.trend[ina.trend] <- xtmp[ina.trend]
				locations.stn$res <- NA
				if(length(glm.stn$na.action) > 0) locations.stn$res[-glm.stn$na.action] <- glm.stn$residuals
				else locations.stn$res <- glm.stn$residuals
			}
		}

		############
		locations.stn <- locations.stn[!is.na(locations.stn$res), ]

		if(length(locations.stn) < min.stn){
			writeNC.merging(xtmp, ncInfo$dates[jj], freqData, grd.nc.out,
					mrgParms$merge.DIR, GeneralParameters$output$format)
			cat(paste(ncInfo$dates[jj], ":", "not enough station data", "|", "Input temperature data", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		############
		if(any(is.auxvar)){
			locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
			locations.stn <- locations.stn[Reduce("&", locations.df), ]

			if(length(locations.stn) < min.stn){
				writeNC.merging(xtmp, ncInfo$dates[jj], freqData, grd.nc.out,
						mrgParms$merge.DIR, GeneralParameters$output$format)
				cat(paste(ncInfo$dates[jj], ":", "not enough station data combined with auxiliary var", "|",
						"Input temperature data", "\n"), file = log.file, append = TRUE)
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
		out.mrg <- sp.trend + resid

		###########

		if(mrg.method == "Regression Kriging"){
			bsmoo <- as.logical(over(newdata, buffer.ina))
			bsmoo[is.na(bsmoo)] <- FALSE
			mout.in <- !as.logical(over(newdata, buffer.xaddin))
			mout.in[is.na(mout.in)] <- TRUE
			imout <- mout.in & igrid

			out.tmp <- xtmp
			out.tmp[bsmoo] <- out.mrg[bsmoo]
			out.tmp <- smooth.matrix(out.tmp, 1)
			out.mrg[!igrid] <- xtmp[!igrid]
			out.mrg[imout] <- out.tmp[imout]
			rm(bsmoo, mout.in, imout, out.tmp)
		}

		rm(resid, sp.trend, res.grd, inside, ina, newdata0,
			igrid, locations.stn, xadd, xadd.in, xadd.out,
			buffer.ina, buffer.grid, buffer.xaddin, buffer.xaddout)

		###########
		#Apply mask for area of interest
		if(!is.null(mrgParms$outMask)) out.mrg[is.na(mrgParms$outMask)] <- NA

		writeNC.merging(out.mrg, ncInfo$dates[jj], freqData, grd.nc.out,
				mrgParms$merge.DIR, GeneralParameters$output$format)

		rm(out.mrg, xtmp, newdata)
		gc()
		return(0)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	InsertMessagesTxt(main.txt.out, 'Merging finished')
	return(0)
}


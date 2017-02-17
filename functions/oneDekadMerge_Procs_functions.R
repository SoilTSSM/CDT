
mergeOneDekadRain <- function(){
	dir2save <- GeneralParameters$IO.files$dir2save
	dyear <- GeneralParameters$Merging.Date$year
	dmon <- GeneralParameters$Merging.Date$month
	ddek <- as.numeric(GeneralParameters$Merging.Date$dekad)
	if(ddek>3 | ddek < 1){
		InsertMessagesTxt(main.txt.out, 'Dekad must be 1, 2 or 3', format = TRUE)
		tcl("update")
		return(NULL)
	}

	daty <- paste(format(as.Date(paste(dyear, dmon, ddek, sep = '-')), '%Y%m'), ddek, sep = '')
	dmon <- substr(daty, 5, 6)
	yeardekad <- expand.grid(1:3, 1:12)
	dir2save <- file.path(dir2save, paste('DEKAD', daty, sep = '_'))
	dir.create(dir2save, showWarnings = FALSE, recursive = TRUE)

	## get rfe data
	if(GeneralParameters$Downloaded.RFE){
		rfeData <- getNcdfOpenData(GeneralParameters$IO.files$RFE.file)[[2]][1:3]
		names(rfeData) <- c('x', 'y', 'z')
	}else{
		if(!testConnection()){
			InsertMessagesTxt(main.txt.out, 'No internet connection', format = TRUE)
			tcl("update")
			return(NULL)
		}

		dataRFE <- GeneralParameters$RFE.data
		minlon <- GeneralParameters$RFE.bbox$minlon
		maxlon <- GeneralParameters$RFE.bbox$maxlon
		minlat <- GeneralParameters$RFE.bbox$minlat
		maxlat <- GeneralParameters$RFE.bbox$maxlat
	
		downrfeDir <- file.path(dir2save, 'downloaded_RFE')
		dir.create(downrfeDir, showWarnings = FALSE, recursive = TRUE)

		if(dataRFE == 'TAMSAT'){
			url <- 'http://tamsat.org.uk/public_data'
			file0 <- paste('rfe', dyear, '_', dmon,'-dk', ddek,'.nc', sep = '')
			url <- paste(url, dyear, dmon, file0, sep = '/')
		}
		if(dataRFE == 'CHIRP'){
			url <- 'ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRP/dekads/africa'
			file0 <- paste('chirp.', dyear, '.', dmon,'.', ddek,'.tif', sep = '')
			url <- paste(url, file0, sep = '/')
		}
		destfile0 <- file.path(downrfeDir, paste('Africa_', file0, sep = ''))
		testURL <- try(suppressWarnings(url(url, open = 'rb')), silent = TRUE)
		if(inherits(testURL, "try-error")){
			InsertMessagesTxt(main.txt.out, paste('Cannot open URL or file does not exist:', file0), format = TRUE)
			tcl("update")
			close(testURL)
			return(NULL)
		}else close(testURL)
		ret <- try(download.file(url, destfile0, mode = "wb", quiet = TRUE), silent = TRUE)
		if(ret != 0){
			InsertMessagesTxt(main.txt.out, paste('Download failed:',file0), format = TRUE)
			tcl("update")
			return(NULL)
		}else{
			InsertMessagesTxt(main.txt.out, paste('Download:',file0, 'done!'))
			tcl("update")
		}

		if(dataRFE == 'TAMSAT'){
			nc <- nc_open(destfile0)
			xm <- nc$dim[[2]]$vals
			ym <- nc$dim[[1]]$vals
			rfe <- ncvar_get(nc, varid = nc$var[[1]]$name)
			nc_close(nc)
			xo <- order(xm)
			yo <- order(ym)
			xm <- xm[xo]
			ym <- ym[yo]
			rfe <- rfe[xo, yo]
			idx <- which(xm >= minlon & xm <= maxlon)
			idy <- which(ym >= minlat & ym <= maxlat)
			rfe.lon <- xm[idx]
			rfe.lat <- ym[idy]
			rfe <- rfe[idx, idy]
			longname <- "TAMSAT 10-days rainfall estimate"
		}
		if(dataRFE == 'CHIRP'){
			tif <- readGDAL(destfile0, silent = TRUE)
			rfe <- tif@data[, 1]
			xy <- coordinates(tif)
			ix <- (xy[, 1] >= minlon & xy[, 1] <= maxlon) & (xy[, 2] >= minlat & xy[, 2] <= maxlat)
			rfe <- rfe[ix]
			xy <- xy[ix, ]
			rfe <- reshapeXYZ2Matrix(cbind(xy, rfe))
			rfe.lon <- rfe$x
			rfe.lat <- rfe$y
			rfe <- rfe$z
			longname <- "CHIRP 10-days rainfall estimate"
			file0 <- paste(getf.no.ext(file0), '.nc', sep = '')
		}
		rfeData <- list(x = rfe.lon, y = rfe.lat, z = rfe)
		rfe[rfe < 0 | is.na(rfe)] <- -99
		dx <- ncdim_def("Lon", "degreeE", rfe.lon)
		dy <- ncdim_def("Lat", "degreeN", rfe.lat)
		rfeout <- ncvar_def('precip', "mm", list(dx, dy), -99, longname = longname, prec = "short")
		outfl <- file.path(downrfeDir, file0)
		nc2 <- nc_create(outfl, rfeout)
		ncvar_put(nc2, rfeout, rfe)
		nc_close(nc2)
	}

	## correct bias
	if(GeneralParameters$Adjust.Bias){
		bias.method <- GeneralParameters$Bias.Method
		biasDir <- GeneralParameters$IO.files$BIAS.dir
		meanBiasPrefix <- GeneralParameters$Prefix$Mean.Bias.Prefix

		if(bias.method == "Quantile.Mapping"){
			parsstnf <- paste('Bernoulli-Gamma_Pars.STN', '_', as.numeric(dmon), '.nc', sep = '')
			pars.stnFile <- file.path(biasDir, parsstnf)
			if(!file.exists(pars.stnFile)){
				InsertMessagesTxt(main.txt.out, paste(parsstnf, "doesn't exist"), format = TRUE)
				tcl("update")
				return(NULL)
			}
			parsrfef <- paste('Bernoulli-Gamma_Pars.RFE', '_', as.numeric(dmon), '.nc', sep = '')
			pars.rfeFile <- file.path(biasDir, parsrfef)
			if(!file.exists(pars.rfeFile)){
				InsertMessagesTxt(main.txt.out, paste(parsrfef, "doesn't exist"), format = TRUE)
				tcl("update")
				return(NULL)
			}

			nc <- nc_open(pars.stnFile)
			lon.bias <- nc$dim[[1]]$vals
			lat.bias <- nc$dim[[2]]$vals
			prob.stn <- ncvar_get(nc, varid = "prob")
			scale.stn <- ncvar_get(nc, varid = "scale")
			shape.stn <- ncvar_get(nc, varid = "shape")
			nc_close(nc)
			pars.stn <- list(prob = prob.stn, scale = scale.stn, shape = shape.stn)

			nc <- nc_open(pars.rfeFile)
			prob.rfe <- ncvar_get(nc, varid = "prob")
			scale.rfe <- ncvar_get(nc, varid = "scale")
			shape.rfe <- ncvar_get(nc, varid = "shape")
			nc_close(nc)
			pars.rfe <- list(prob = prob.rfe, scale = scale.rfe, shape = shape.rfe)
		}else{
			if(bias.method == "Multiplicative.Bias.Mon") idek <- as.numeric(dmon)
			else idek <- which(yeardekad[, 2] == as.numeric(dmon) & yeardekad[, 1] == ddek)
			biasf <- paste(meanBiasPrefix, '_', idek, '.nc', sep = '')	
			biasFile <- file.path(biasDir, biasf)
			if(!file.exists(biasFile)){
				InsertMessagesTxt(main.txt.out, paste(biasf, "doesn't exist"), format = TRUE)
				tcl("update")
				return(NULL)
			}
			nc <- nc_open(biasFile)
			lon.bias <- nc$dim[[1]]$vals
			lat.bias <- nc$dim[[2]]$vals
			data.bias <- ncvar_get(nc, varid = "bias")
			nc_close(nc)
		}

		biasSp <- defSpatialPixels(list(lon = lon.bias, lat = lat.bias))
		rfeSp <- defSpatialPixels(list(lon = rfeData$x, lat = rfeData$y))
		is.regridRFE <- is.diffSpatialPixelsObj(biasSp, rfeSp, tol = 1e-07)
		if(is.regridRFE){
			rfeData <- interp.surface.grid(rfeData, list(x = lon.bias, y = lat.bias))
		}

		if(bias.method == "Quantile.Mapping"){
			xadj <- quantile.mapping.BGamma(rfeData$z, pars.stn, pars.rfe, TRUE)
			xadj[!is.na(xadj) & xadj > 1000] <- rfeData$z[!is.na(xadj) & xadj > 1000]
		}else xadj <- rfeData$z * data.bias
		xadj[xadj < 0] <- 0
		rfeData <- list(x = lon.bias, y = lat.bias, z = xadj)

		xadj[is.na(xadj)] <- -99
		dx <- ncdim_def("Lon", "degreeE", lon.bias)
		dy <- ncdim_def("Lat", "degreeN", lat.bias)

		grd.bsadj <- ncvar_def("precip", "mm", list(dx, dy), -99, longname = " Mean Bias Adjusted RFE", prec = "short")
		bias.outfl <- file.path(dir2save, paste('rr_adj', '_', daty,'.nc', sep = ''))
		nc2 <- nc_create(bias.outfl, grd.bsadj)
		ncvar_put(nc2, grd.bsadj, xadj)
		nc_close(nc2)
	}

	################
	## merging
	no.stnData <- GeneralParameters$No.Stn.Data
	if(!no.stnData){
		interp.method <- GeneralParameters$Interpolation.pars$interp.method
		nmin <- GeneralParameters$Interpolation.pars$nmin
		nmax <- GeneralParameters$Interpolation.pars$nmax
		maxdist <- GeneralParameters$Interpolation.pars$maxdist
		vgm.model <- GeneralParameters$vgm.model
		# use.block <- GeneralParameters$use.block
		res.coarse <- maxdist/2
		res.coarse <- if(res.coarse  >= 0.25) res.coarse else 0.25
		maxdist <- if(maxdist < res.coarse) res.coarse else maxdist

		Mrg.Method <- GeneralParameters$Mrg.Method
		min.stn <- GeneralParameters$Merging.pars$min.stn
		min.non.zero <- GeneralParameters$Merging.pars$min.non.zero
	 	use.RnoR <- GeneralParameters$Merging.pars$use.RnoR
	 	smooth.RnoR <- GeneralParameters$Merging.pars$smooth.RnoR
		wet.day <- GeneralParameters$Merging.pars$wet.day

		if(Mrg.Method == "Spatio-Temporal LM"){
			coeffl <- paste('LM_Coefficient_', as.numeric(dmon), '.nc', sep = '')
			coefFiles <- file.path(GeneralParameters$IO.files$LMCoef.dir, coeffl)
			if(!file.exists(coefFiles)){
				InsertMessagesTxt(main.txt.out, paste(coeffl, "doesn't exist"), format = TRUE)
				tcl("update")
				return(NULL)
			}

			nc <- nc_open(coefFiles)
			lon.coef <- nc$dim[[1]]$vals
			lat.coef <- nc$dim[[2]]$vals
			coef1 <- ncvar_get(nc, varid = 'slope')
			coef2 <- ncvar_get(nc, varid = 'intercept')
			nc_close(nc)
			
			coefSp <- defSpatialPixels(list(lon = lon.coef, lat = lat.coef))
			rfeSp <- defSpatialPixels(list(lon = rfeData$x, lat = rfeData$y))
			is.regridRFE <- is.diffSpatialPixelsObj(coefSp, rfeSp, tol = 1e-07)
			if(is.regridRFE){
				rfeData <- interp.surface.grid(rfeData, list(x = lon.coef, y = lon.coef))
			}
		}

		#############
		nlon0 <- length(rfeData$x)
		nlat0 <- length(rfeData$y)
		newGrid <- defSpatialPixels(list(lon = rfeData$x, lat = rfeData$y))

		#####
		stnData <- getStnOpenData(GeneralParameters$IO.files$STN.file)
		stnData <- getCDTdataAndDisplayMsg(stnData, 'dekadal')
		if(is.null(stnData)) return(NULL)
		lon.stn <- stnData$lon
		lat.stn <- stnData$lat
		date.stn <- stnData$dates
		data.stn <- stnData$data

		#####
		blank.grid <- GeneralParameters$Blank.Grid
		if(blank.grid == "1") outMask <- NULL
		if(blank.grid == "2"){
			demData <- getDemOpenDataSPDF(GeneralParameters$IO.files$DEM.file)
			if(is.null(demData)) return(NULL)
			demGrid <- defSpatialPixels(list(lon = demData$lon, lat = demData$lat))
			is.regridDEM <- is.diffSpatialPixelsObj(newGrid, demGrid, tol = 1e-07)
			if(is.regridDEM){
				outMask <- interp.surface.grid(list(x = demData$lon, y = demData$lat, z = demData$demMat), rfeData[1:2])
				outMask <- outMask$z
			}else outMask <- demData$demMat
			outMask[outMask <= 0] <- NA
		}
		if(blank.grid == "3"){
			shpd <- getShpOpenData(GeneralParameters$IO.files$SHP.file)[[2]]
			shpd[['vtmp']] <- 1
			outMask <- over(newGrid, shpd)[, 'vtmp']
			dim(outMask) <- c(nlon0, nlat0)
		}
		
		#######
		dem.grd.val <- matrix(1, nrow = nlon0, ncol = nlat0)
		dem.grd.slp <- matrix(0, nrow = nlon0, ncol = nlat0)
		dem.grd.asp <- matrix(0, nrow = nlon0, ncol = nlat0)
		dem.stn.val <- rep(1, length(lon.stn))
		dem.stn.slp <- rep(0, length(lon.stn))
		dem.stn.asp <- rep(0, length(lon.stn))
		demGrid <- list(x = rfeData$x, y = rfeData$y, z = dem.grd.val, slp = dem.grd.slp, asp = dem.grd.asp)
		ObjStn <- list(x = lon.stn, y = lat.stn,  z = dem.stn.val, slp = dem.stn.slp, asp = dem.stn.asp)

		interp.grid <- createGrid(ObjStn, demGrid, as.dim.elv = FALSE, res.coarse = res.coarse)
		cells <- SpatialPixels(points = interp.grid$newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)))@grid
		bGrd <- createBlock(cells@cellsize, 2, 5)

		#############
		ijGrd <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = rfeData$x, lat = rfeData$y))

		############
		locations.stn <- interp.grid$coords.stn
		locations.stn$stn <- data.stn[date.stn == daty, ]
		locations.stn$rfe <- rfeData$z[ijGrd]
		xadd <- as.data.frame(interp.grid$coords.grd)
		xadd$rfe <- c(rfeData$z[interp.grid$idxy$ix, interp.grid$idxy$iy])
		xadd$stn <- xadd$rfe
		xadd$res <- 0
		interp.grid$newgrid$rfe <- c(rfeData$z)

		############
		noNA <- !is.na(locations.stn$stn) & !is.na(locations.stn$rfe)
		min.stn.nonNA <- length(which(noNA))
		nb.stn.nonZero <- length(which(noNA & locations.stn$stn > 0))
		locations.stn <- locations.stn[noNA, ]

		if(min.stn.nonNA >= min.stn){
			do.merging <- TRUE
			if(nb.stn.nonZero >= min.non.zero){
				if(Mrg.Method == "Spatio-Temporal LM"){
					sp.trend <- rfeData$z * coef1 + coef2
					locations.stn$res <- locations.stn$stn - sp.trend[ijGrd][noNA]
				}else{
					simplediff <- if(var(locations.stn$stn) < 1e-07 | var(locations.stn$rfe, na.rm = TRUE) < 1e-07) TRUE else FALSE
					glm.stn <- glm(stn~rfe, data = locations.stn, family = gaussian)
					if(is.na(glm.stn$coefficients[2]) | glm.stn$coefficients[2] < 0) simplediff <- TRUE
					if(!simplediff){
						sp.trend <- predict(glm.stn, newdata = interp.grid$newgrid)
						sp.trend <- matrix(sp.trend, ncol = nlat0, nrow = nlon0)
						sp.trend[is.na(sp.trend)] <- rfeData$z[is.na(sp.trend)]
						# locations.stn$res <- residuals(glm.stn)
						locations.stn$res <- NA
						if(length(glm.stn$na.action) > 0) locations.stn$res[-glm.stn$na.action] <- glm.stn$residuals
						else locations.stn$res <- glm.stn$residuals
					}else{
						sp.trend <- rfeData$z
						locations.stn$res <- locations.stn$stn - locations.stn$rfe
					}
				}
			}else{
				sp.trend <- rfeData$z
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
				rnr.rfe <- rfeData$z
				rnr.rfe[] <- 0
				rnr.rfe[rfeData$z >= wet.day] <- 1
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
				vgm <- try(autofitVariogram(res~1, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
				if(!inherits(vgm, "try-error")) vgm <- vgm$var_model
				else vgm <- NULL
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
			res.grd <- krige(res~1, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
								block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
			extrm <- c(min(locations.stn$res, na.rm = TRUE), max(locations.stn$res, na.rm = TRUE))
			ixtrm <- is.na(res.grd$var1.pred) | (res.grd$var1.pred < extrm[1] | res.grd$var1.pred > extrm[2])
			res.grd$var1.pred[ixtrm] <- NA

			ina <- is.na(res.grd$var1.pred)
			if(any(ina)){
				res.grd.na <- krige(var1.pred~1, locations = res.grd[!ina, ], newdata = interp.grid$newgrid[ina, ], model = vgm,
										block = bGrd, nmin = nmax, nmax = nmax, maxdist = maxdist, debug.level = 0)
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
					if(sum(rnr.rfe, na.rm = TRUE) == 0) npix <- 5
					else npix <- 3
					rnr <- smooth.matrix(rnr, npix)
				}
			}else rnr <- matrix(1, ncol = nlat0, nrow = nlon0)

			############
			out.mrg <- sp.trend + resid
			out.mrg[out.mrg < 0] <- 0
			out.mrg <- out.mrg * rnr
		}else out.mrg <- rfeData$z

		#Apply mask for area of interest
		if(!is.null(outMask)) out.mrg[is.na(outMask)] <- NA
		out.mrg[is.na(out.mrg)] <- -99

		#############
		## Def ncdf
		dx <- ncdim_def("Lon", "degreeE", rfeData$x)
		dy <- ncdim_def("Lat", "degreeN", rfeData$y)
		grd.nc.out <- ncvar_def("precip", "mm", list(dx, dy), -99, longname = "Merged Station-Satellite Rainfall", prec = "short")

		mrg.outfl <- file.path(dir2save, paste('rr_mrg', '_', daty, '_MON.nc', sep = ''))
		nc2 <- nc_create(mrg.outfl, grd.nc.out)
		ncvar_put(nc2, grd.nc.out, out.mrg)
		nc_close(nc2)
	}
	return(0)
}


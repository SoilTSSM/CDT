update1DekProc_Mali <- function(origdir){
	existRFE <- as.character(GeneralParameters$rfeDownSep)
	existSTN <- as.character(GeneralParameters$stnDataDispo)
	year <- str_trim((as.character(GeneralParameters$dates.mrg$Values[1])))
	mon <- str_trim(as.character(GeneralParameters$dates.mrg$Values[2]))
	mon <- ifelse(as.numeric(mon) < 10, paste('0', mon, sep = ''), mon)
	dek <- str_trim(as.character(GeneralParameters$dates.mrg$Values[3]))
	rfeAfrica <- as.character(GeneralParameters$file.io$Values[2])
	file.stn <- as.character(GeneralParameters$file.io$Values[1])

	nmin <- 1
	nmax <- 5
	maxdist <- 1.0
	maxdistRnR <- 0.3
	minStnNoNA <- 10
	minStnNoZero <- 7

	VarioModel <- c("Sph", "Exp", "Gau")

	deb <- try(as.Date(paste(year, mon, dek, sep = '-')), silent = TRUE)
	if(inherits(deb, "try-error")| is.na(deb)){
		InsertMessagesTxt(main.txt.out, 'Verifier la date', format = TRUE)
		return(NULL)
	}
	deb <- strsplit(as.character(deb),'-')
	year1 <- deb[[1]][1]
	mon1 <- deb[[1]][2]
	dek1 <- as.numeric(deb[[1]][3])
	if(dek1 > 3){
		InsertMessagesTxt(main.txt.out, 'La decade doit etre 1, 2 ou 3', format = TRUE)
		return(NULL)
	}

	minlon<- -12.375
	maxlon <- 4.3875
	minlat <- 10.0125
	maxlat <- 25.2

	if(existRFE == '0'){
		if(!testConnection()){
			InsertMessagesTxt(main.txt.out, 'No internet connection', format = TRUE)
			return(NULL)
		}
		url <- 'http://tamsat.org.uk/public_data'
		outdir0 <- file.path(origdir, 'Dekad_TAMSAT_Africa', fsep = .Platform$file.sep)
		if(!file.exists(outdir0)) dir.create(outdir0, showWarnings = FALSE, recursive = TRUE)
		file0 <- paste('rfe', year, '_', mon,'-dk', dek,'.nc', sep = '')
		link <- paste(url, year, mon, file0, sep = '/')
		destfile0 <- file.path(outdir0, file0, fsep = .Platform$file.sep)
		test <- try(suppressWarnings(readLines(link, n = 1)), silent = TRUE)
		if(inherits(test, "try-error")){
			InsertMessagesTxt(main.txt.out, paste(file0,": n'est pas encore disponible ou la connexion internet est perdue"), format = TRUE)
			return(NULL)
		}

		InsertMessagesTxt(main.txt.out, "Téléchargement.................")
		tcl("update")
		ret <- try(download.file(link, destfile0, mode = "wb", quiet = TRUE), silent = TRUE)
		if(ret != 0){
			InsertMessagesTxt(main.txt.out, paste('Échec du téléchargement pour:',file0), format = TRUE)
			return(NULL)
		}else{
			nc <- nc_open(destfile0)
			xm <- nc$dim[[2]]$vals
			ym <- nc$dim[[1]]$vals
			xdat <- ncvar_get(nc, varid = nc$var[[1]]$name)
			nc_close(nc)
			InsertMessagesTxt(main.txt.out, paste('Téléchargement pour:',file0, 'terminé'))
		}

	}else{
		if(!file.exists(rfeAfrica)){
			InsertMessagesTxt(main.txt.out, "Les donnees de TAMSAT pour Africa n'existe pas", format = TRUE)
			return(NULL)
		}
		nc <- nc_open(rfeAfrica)
		xm <- nc$dim[[2]]$vals
		ym <- nc$dim[[1]]$vals
		xdat <- ncvar_get(nc, varid = nc$var[[1]]$name)
		nc_close(nc)
		InsertMessagesTxt(main.txt.out, paste('Extraction  terminée pour:',basename(rfeAfrica)))
	}
	tcl("update")
	
	xo <- order(xm)
	xm <- xm[xo]
	yo <- order(ym)
	ym <- ym[yo]
	xdat <- xdat[xo, yo]
	idx <- which(xm >= minlon & xm <= maxlon)
	idy <- which(ym >= minlat & ym <= maxlat)
	xm <- xm[idx]
	ym <- ym[idy]
	xdat <- xdat[idx, idy]
	xdat[is.na(xdat)] <- -99
	outdir1 <- file.path(origdir, 'Dekad_TAMSAT_MALI', fsep = .Platform$file.sep)
	if(!file.exists(outdir1)) dir.create(outdir1, showWarnings = FALSE, recursive = TRUE)
	nc_out_file <- file.path(outdir1, paste('rfe', year, '_', mon,'-dk', dek,'.nc', sep = ''), fsep = .Platform$file.sep)

	nlon0 <- length(xm)
	nlat0 <- length(ym)
	dx <- ncdim_def("Lon", "degreeE", xm)
	dy <- ncdim_def("Lat", "degreeN", ym)
	nc_out_var <- ncvar_def('precip', "mm", list(dx, dy), -99, longname = "TAMSAT Rain Fall Estimate (RFE)", prec = "short")
	nc <- nc_create(nc_out_file, nc_out_var)
	ncvar_put(nc, nc_out_var, xdat)
	nc_close(nc)

	newlocation.merging <- expand.grid(lon = xm, lat = ym)
	coordinates(newlocation.merging)<- ~lon+lat
	grid.loc <- newlocation.merging
	grid.loc <- SpatialPixels(points = grid.loc, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))

	shpd <- readShapeSpatial(file.path(apps.dir, 'country', 'Mali', 'MLI_adm0.shp', fsep = .Platform$file.sep))
	shpd[['vtmp']] <- 1
	shpMask <- over(newlocation.merging, shpd)[,'vtmp']
	outMask <- matrix(shpMask, nrow = nlon0, ncol = nlat0)

	outdir2 <- file.path(origdir, 'MALI_MERGED_DATA', fsep = .Platform$file.sep)
	if(!file.exists(outdir2)) dir.create(outdir2, showWarnings = FALSE, recursive = TRUE)
	rr_mrg_mon <- file.path(outdir2, paste('rr_mrg_', year, mon, dek, '_MON.nc', sep = ''), fsep = .Platform$file.sep)

	if(existSTN == '1'){
		all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))
		jfile <- which(all.open.file == file.stn[1])
		donne <- AllOpenFilesData[[jfile]][[2]]
		stn.lon <- as.numeric(donne[2,-1])
		stn.lat <- as.numeric(donne[3,-1])
		dates <- as.character(donne[4,1])
		if(paste(year, mon, dek, sep = '') != dates){
			InsertMessagesTxt(main.txt.out, "La date entrée ne correspond pas a la date des données de stations", format = TRUE)
			return(NULL)
		}
		stn.data <- as.numeric(donne[4,-1])
		stn.data[stn.data < 0] <- NA
		stn.loc <- data.frame(lon = stn.lon, lat = stn.lat)
		stn.loc <- SpatialPoints(stn.loc)
		ijGrd <- unname(over(stn.loc, geometry(grid.loc)))
	
		##Mean bias adjustment
		annual.dek <- expand.grid(dek = 1:3, mon = 1:12)
		ijt <- which(annual.dek$dek == as.numeric(dek) & annual.dek$mon == as.numeric(mon))
		biasfile <- file.path(apps.dir, 'country', 'Mali', 'Climatological_MeanBias_RainDek', paste('RFE_GG_MeanBias_', ijt,'.nc', sep = ''), fsep = .Platform$file.sep)
		nc <- nc_open(biasfile)
		bias <- ncvar_get(nc, varid = nc$var[[1]]$name)
		nc_close(nc)

		xdat[xdat == -99] <- NA
		xdat.adj <- round(xdat * bias, 1)
		xdat <- xdat.adj
		xdat.adj[is.na(xdat.adj)] <- -99
		grd.adj <- ncvar_def("precip", "mm", list(dx, dy),-99, longname=" Mean-Bias Adjusted satellite Rainfall", prec = "short")
		nc_adj_file <- file.path(outdir1, paste('rr_adj_', year, mon, dek,'.nc', sep = ''), fsep = .Platform$file.sep)
		nc <- nc_create(nc_adj_file, grd.adj)
		ncvar_put(nc, grd.adj, xdat.adj)
		nc_close(nc)
		rm(xdat.adj)

		grd.out <- ncvar_def("precip", "mm", list(dx, dy),-99, longname=" Merged Station-Satellite Rainfall", prec = "short")
		cells <- as(newlocation.merging, 'SpatialPixels')@grid

		##block grid
		sDX <- cells@cellsize[1]/2
		dBX <- seq(-sDX, sDX, length.out = 4)
		sDY <- cells@cellsize[2]/2
		dBY <- seq(-sDY, sDY, length.out = 4)
		bGrd <- expand.grid(x = dBX, y = dBY)

		#rfe over stn location
		rfe_gg <- xdat[ijGrd]
		dff <- stn.data - rfe_gg

		# stnData <- data.frame(lon = stn.lon, lat = stn.lat, gg = stn.data)
		# stnData <- stnData[!is.na(stnData$gg),]
		# coordinates(stnData) = ~lon+lat
		# grdStnData <- krige(gg~1, locations = stnData, newdata = newlocation.merging, block = bGrd, nmin = 1, nmax = 5, maxdist = 0.1, debug.level = 0)
		# out.stn <- grdStnData$var1.pred
		# out.stn <- ifelse(out.stn < 0,0, out.stn)
		# dim(out.stn) <- c(nlon0, nlat0)

		# Remove extremes differences between gauge and satellite
		# q1 <- quantile(dff, 0.0001, na.rm = T)
		# q2 <- quantile(dff, 0.9999, na.rm = T)
		# dff[dff < q1] <- NA
		# dff[dff > q2] <- NA
		ix <- which(!is.na(dff))
		rfe.vec <- c(xdat)
		out.mrg <- rfe.vec  ##Initial rfe

		if(sum(stn.data, na.rm = TRUE) > 0 & length(ix) >= minStnNoNA){
			rr.stn <- data.frame(cbind(stn.lon, stn.lat, stn.data, rfe_gg, dff))
			rr.stn <- rr.stn[ix,]
			names(rr.stn) <- c("lon", "lat", "gg", "rfe", "dff")
			coordinates(rr.stn) = ~lon+lat
			ijx1 <- which(rr.stn$gg > 0)
			if(length(ijx1) >= minStnNoZero){
				grd.newloc <- SpatialPointsDataFrame(coords = newlocation.merging, data = data.frame(rfe = rfe.vec))

				## RK GLM
				rr.glm <- glm(gg~rfe, rr.stn, family = gaussian)
				rr.stn$res <- residuals(rr.glm)
				vgm.glm <- try(autofitVariogram(res~1, input_data = rr.stn, model = VarioModel), silent = TRUE)
				if(!inherits(vgm.glm, "try-error")) vgm.glm <- vgm.glm$var_model
				else vgm.glm <- NULL
				pred.rr.glm <- predict(rr.glm, newdata = grd.newloc, se.fit = T)
				grd.res <- krige(res~1, locations = rr.stn, newdata = newlocation.merging, model = vgm.glm, block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				out.mrg <- as.numeric(grd.res$var1.pred+pred.rr.glm$fit)
				out.mrg <- ifelse(out.mrg < 0,0, out.mrg)
				
				## RK GLS
				# rr.ols <- lm(gg~rfe, rr.stn)
				# rr.stn$res <- residuals(rr.ols)
				# vgm.ols <- try(autofitVariogram(res~1, input_data = rr.stn, model = VarioModel), silent = TRUE)
				# if(!inherits(vgm.ols, "try-error")){
				# 	vgm.ols <- vgm.ols$var_model
				# 	vgm.olsFun <- as.character(vgm.ols$model[2])
				# 	if(vgm.olsFun == "Sph") corFun <- match.fun('corSpher')
				# 	if(vgm.olsFun == "Exp") corFun <- match.fun('corExp')
				# 	if(vgm.olsFun == "Gau") corFun <- match.fun('corGaus')
				# 	rr.gls1 <- gls(gg~rfe, data = rr.stn, correlation = corFun(c(vgm.ols$range[2], vgm.ols$psill[1]/vgm.ols$psill[2]), form=~lon+lat, nugget = TRUE, fixed = TRUE))
				# 	rr.stn$res <- residuals(rr.gls1)
				# 	vgm.gls <- try(autofitVariogram(res~1, input_data = rr.stn, model = vgm.olsFun), silent = TRUE)
				# 	if(!inherits(vgm.gls, "try-error")){
				# 		vgm.gls <- vgm.gls$var_model
				# 		rr.gls2 <- gls(gg~rfe, data = rr.stn, correlation = corFun(c(vgm.gls$range[2], vgm.gls$psill[1]/vgm.gls$psill[2]), form=~lon+lat, nugget = TRUE, fixed = TRUE))
				# 		rr.stn$res <- residuals(rr.gls2)
				# 		pred.rr <- as.numeric(predict(rr.gls2, newdata = grd.newloc, na.action = na.pass))
				# 	}else{
				# 		vgm.gls <- vgm.ols
				# 		pred.rr <- as.numeric(predict(rr.gls1, newdata = grd.newloc, na.action = na.pass))
				# 	}
				# }else{
				# 	vgm.gls <- NULL
				# 	pred.rr.ols <- predict(rr.ols, newdata = grd.newloc, se.fit = T)
				# 	pred.rr <- as.numeric(pred.rr.ols$fit)
				# }
				# grd.res <- krige(res~1, locations = rr.stn, newdata = newlocation.merging, model = vgm.gls, block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				# out.mrg <- as.numeric(grd.res$var1.pred+pred.rr)
				# out.mrg <- ifelse(out.mrg < 0,0, out.mrg)
			}else{
				grd.rr <- krige(dff~1, locations = rr.stn, newdata = newlocation.merging, block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				out.mrg <- grd.rr$var1.pred + rfe.vec
				out.mrg <- ifelse(out.mrg < 0,0, out.mrg)
			}

			# Take RFE for areas where interpolation/merging was not possible
			ix <- which(is.na(out.mrg))
			out.mrg[ix] <- rfe.vec[ix]

			#Rain-non-Rain Mask
			rr.stn$rnr <- ifelse(rr.stn$gg >= 1,1,0)
			rfe.rnr <- ifelse(rfe.vec >= 1, 1, 0)
			rnr.grd <- krige(rnr~1, locations = rr.stn, newdata = newlocation.merging, block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdistRnR, debug.level = 0)
			RnoR <- rnr.grd$var1.pred
			RnoR <- round(RnoR)
			ix <- which(is.na(RnoR))
			RnoR[ix] <- rfe.rnr[ix]
			RnoR[is.na(RnoR)] <- 1
			##smoothing???
			# img.RnoR <- as.image(RnoR, x = coordinates(newlocation.merging), nx = cells@cells.dim[1], ny = cells@cells.dim[2])
			# smooth.RnoR <- image.smooth(img.RnoR, theta = 0.075)
			# RnoR <- round(c(smooth.RnoR$z))
			out.mrg <- out.mrg * RnoR
		}
		dim(out.mrg) <- c(nlon0, nlat0)

		# out.mrg[!is.na(out.stn)] <- out.stn[!is.na(out.stn)]
		# img.mrg <- as.image(out.mrg, x = coordinates(newlocation.merging), nx = cells@cells.dim[1], ny = cells@cells.dim[2])
		# smooth.mrg <- image.smooth(img.mrg, theta = 0.03)
		# out.mrg <- round(smooth.mrg$z, 1)
		out.mrg[is.na(out.mrg)] <- -99

		#Apply mask for area of interest
		out.mrg[is.na(outMask)] <- -99
		nc2 <- nc_create(rr_mrg_mon, grd.out)
		ncvar_put(nc2, grd.out, out.mrg)
		nc_close(nc2)
	}else{
		xdat[is.na(outMask)] <- -99
		nc <- nc_create(rr_mrg_mon, nc_out_var)
		ncvar_put(nc, nc_out_var, xdat)
		nc_close(nc)
	}

	return(0)
}

ValidationDataFun <- function(retValidParams){
	if(is.null(retValidParams$stn$donne)){
		InsertMessagesTxt(main.txt.out, 'No station data found', format = TRUE)
		return(NULL)
	}

	if(is.null(retValidParams$rfe$rfedata)){
		InsertMessagesTxt(main.txt.out, 'Provide a sample of NetCDF data', format = TRUE)
		return(NULL)
	}

	if(retValidParams$rfe$ncdir == '' | retValidParams$rfe$ncdir == "NA"){
		InsertMessagesTxt(main.txt.out, 'Directory containing the NetCDF data not found', format = TRUE)
		return(NULL)
	}

	if(retValidParams$dir2sav == '' | retValidParams$dir2sav == "NA"){
		InsertMessagesTxt(main.txt.out, 'Provide a directory to save results', format = TRUE)
		return(NULL)
	}

	if((retValidParams$select_type == "Rectangle") & any(is.na(as.numeric(retValidParams$rect)))){
		InsertMessagesTxt(main.txt.out, 'Invalid bbox selection', format = TRUE)
		return(NULL)
	}

	if((retValidParams$select_type == "Polygons") & is.null(retValidParams$shp$shpf)){
		InsertMessagesTxt(main.txt.out, 'Invalid ESRI shapefiles', format = TRUE)
		return(NULL)
	}

	outValidation <- file.path(retValidParams$dir2sav, paste('Validation', getf.no.ext(retValidParams$stn$filestn), sep = '_'))
	dir.create(outValidation, showWarnings = FALSE, recursive = TRUE)
	# caption <- c('Stations', 'LON', 'DATE/LAT')
	# headinfo <- cbind(caption, t(cbind(retValidParams$stn$donne$id, retValidParams$stn$donne$lon, retValidParams$stn$donne$lat)))

	if(retValidParams$do_extr == 1){
		extr_stn <- ExtractNC2Stn(retValidParams)
	}else{
		extr_stn <- EnvRainValidation$extr_stn
	}
	if(is.null(extr_stn)) return(NULL)

	stn.dates <- extr_stn$date
	stn.data <- extr_stn$stn
	stn.ncdata <- extr_stn$rfe

	####
	# donnees_stn <- t(cbind(t(headinfo), t(cbind(stn.dates, stn.data))))
	# file.stn <- file.path(outValidation, 'STN_VALIDATION_DATA.txt', fsep = .Platform$file.sep)
	# write.table(donnees_stn, file.stn, col.names = FALSE, row.names = FALSE, quote = FALSE)

	# donnees_ncdf <- t(cbind(t(headinfo), t(cbind(stn.dates, round(stn.ncdata, 1)))))
	# file.ncdf <- file.path(outValidation, 'GRDatSTN_VALIDATION_DATA.txt', fsep = .Platform$file.sep)
	# write.table(donnees_ncdf, file.ncdf, col.names = FALSE, row.names = FALSE, quote = FALSE)

	####
	select_type <- retValidParams$select_type
	if(select_type  != "All Stations"){
		lon.stn <- retValidParams$stn$donne$lon
		lat.stn <- retValidParams$stn$donne$lat
		if(select_type == "Rectangle"){
			rect <- as.numeric(retValidParams$rect)
			ilon <- lon.stn >= rect[1] & lon.stn <= rect[2]
			ilat <- lat.stn >= rect[3] & lat.stn <= rect[4]
			ixy <- ilon & ilat
		}
		if(select_type == "Polygons"){
			shp.dat <- retValidParams$shp$shpf[[2]]
			shp.attr <- retValidParams$shp$attr
			shp.id <- retValidParams$shp$id
			shp <- shp.dat[as.character(shp.dat@data[, shp.attr]) == shp.id, ]
			pts.dat <- data.frame(x = lon.stn, y = lat.stn)
			coordinates(pts.dat)<- ~x+y
			ixy <- unname(!is.na(over(pts.dat, geometry(shp))))
		}
		if(!any(ixy)){
			InsertMessagesTxt(main.txt.out, 'The selection did not contain any stations', format = TRUE)
			return(NULL)
		}
		stn.data <- stn.data[, ixy, drop = FALSE]
		stn.ncdata <- stn.ncdata[, ixy, drop = FALSE]
	}

	inNA<- !is.na(stn.data) & !is.na(stn.ncdata)
	stn.data[!inNA] <- NA
	stn.ncdata[!inNA] <- NA

	####
	scatt.dat <- data.frame(stn = stn.data[inNA], grd = stn.ncdata[inNA])
	file.scatt.dat <- file.path(outValidation, 'STN_GRD_VALIDATION_DATA.txt', fsep = .Platform$file.sep)
	write.table(scatt.dat, file.scatt.dat, col.names = TRUE, row.names = FALSE, quote = FALSE)

	####
	stat <- validationStats(stn.data[inNA], stn.ncdata[inNA], retValidParams)
	file.stat <- file.path(outValidation, 'Validation_Statistics.txt', fsep = .Platform$file.sep)
	write.table(stat, file.stat, col.names = TRUE, row.names = FALSE)

	# grphlim <- c(0, max(stn.data[inNA], stn.ncdata[inNA]))
	# jpeg(file.path(outValidation, 'Scatter_Gauge-RFE.jpg', fsep = .Platform$file.sep), width = 960, height = 960, quality = 95)
	# plot(stn.data[inNA], stn.ncdata[inNA], xlab = "Gauge", ylab = "RFE", xlim = grphlim, ylim = grphlim)
	# abline(a = 0, b = 1, lwd = 2, col = 'red')
	# dev.off()

	# jpeg(file.path(outValidation, 'CDF_Gauge-RFE.jpg', fsep = .Platform$file.sep), width = 960, height = 960, quality = 95)
	# plot(ecdf(stn.data[inNA]), xlab = "Rainfall [mm]", main = "CDF", col = 'blue', lwd = 2, cex = 0.4, ylim = c(0,1))
	# plot(ecdf(stn.ncdata[inNA]), add = T, col = "red", lwd = 2, cex = 0.4)
	# legend('bottomright', c('Gauge', 'RFE'), col = c('blue', 'red'), lwd = 3, bg = 'lightgray')
	# dev.off()

	###
	gg_tms <- apply(stn.data, 1, mean, na.rm = TRUE)
	gg_tms[is.nan(gg_tms)] <- NA
	rfe_tms <- apply(stn.ncdata, 1, mean, na.rm = TRUE)
	rfe_tms[is.nan(rfe_tms)] <- NA

	area_avg <- data.frame(date = stn.dates, stn = round(gg_tms, 1), grd = round(rfe_tms, 1))
	file.area.avg <- file.path(outValidation, 'Spatial_Average_STN_GRDatSTN.txt', fsep = .Platform$file.sep)
	write.table(area_avg, file.area.avg, col.names = TRUE, row.names = FALSE, quote = FALSE)

	area_stat <- validationStats(gg_tms, rfe_tms, retValidParams)
	file.area.stat <- file.path(outValidation, 'Spatial_Average_Validation_Statistics.txt', fsep = .Platform$file.sep)
	write.table(area_stat, file.area.stat, col.names = TRUE, row.names = FALSE)

	# grphlim0 <- c(0, max(gg_tms, rfe_tms))
	# jpeg(file.path(outValidation, 'Spatial_Average_Scatter_Gauge-RFE.jpg', fsep = .Platform$file.sep), width = 960, height = 960, quality = 95)
	# plot(gg_tms, rfe_tms, xlab = "Gauge", ylab = "RFE", xlim = grphlim0, ylim = grphlim0)
	# abline(a = 0, b = 1, lwd = 2, col = 'red')
	# dev.off()

	# jpeg(file.path(outValidation, 'Spatial_Average_CDF_Gauge-RFE.jpg', fsep = .Platform$file.sep), width = 960, height = 960, quality = 95)
	# plot(ecdf(gg_tms), xlab = "Rainfall [mm]", main = "CDF", col = 'blue', lwd = 2, cex = 0.4, ylim = c(0,1))
	# plot(ecdf(rfe_tms), add = T, col = "red", lwd = 2, cex = 0.4)
	# legend('bottomright', c('Gauge', 'RFE'), col = c('blue', 'red'), lwd = 3, bg = 'lightgray')
	# dev.off()

	return(list(x = stn.data[inNA], y = stn.ncdata[inNA], stat = stat,
				xs = gg_tms, ys = rfe_tms, sp.stat = area_stat, clim.var = retValidParams$clim.var))
}

###################

validationStats <- function(x, y, retValidParams){
	freqData <- retValidParams$stn$donne$freq
	clim.var <- retValidParams$clim.var
	mn0 <- mean(x)
	sum1 <- sum((y-x)^2)
	sum2 <- sum((x-mn0)^2)
	eff <- 1. - sum1/sum2
	mae <- mean(abs(y-x))
	mer <- mean(y-x)
	bis <- sum(y)/sum(x)
	cor <- cor(x, y)

	stat <- c(cor, eff, bis, mae, mer)
	name_stat <- c("CORR", "NSE", "BIAS", "MAE", "ME")
	descrip <- c('Correlation', 'Nash-Sutcliffe Efficiency', 'Bias', 'Mean Absolute Error', 'Mean Error')

	if(freqData == 'daily' & clim.var == 'RR'){
		rnr <- 1.0
		n1 <- length(which(x >= rnr & y >= rnr))  # Ht
		n2 <- length(which(x < rnr & y >= rnr))  # Fa
		n3 <- length(which(x >= rnr & y < rnr))  # Ms
		n4 <- length(which(x < rnr & y < rnr))   # Cn
		n <- n1+n2+n3+n4

		fbs <- (n1+n2)/(n1+n3)
		csi <- n1/(n-n4)
		pod <- n1/(n1+n3)
		far <- n2/(n1+n2)

		a <- n1*1.0
		b <- n2*1.0
		c <- n3*1.0
		d <- n4*1.0
		C0 <- a+d
		N0 <- a+b+c+d
		E0 <- ((a+c)*(a+b)+(b+d)*(c+d))/(a+b+c+d)
		hss <- (C0-E0)/(N0-E0)

		stat <- c(stat, pod, far, fbs, csi, hss)
		name_stat <- c(name_stat, "POD", "FAR", "FBS", "CSI", "HSS")
		descrip <- c(descrip, 'Probability Of Detection', 'False Alarm Ratio', 'Frequency Bias', 'Critical Success Index', 'Heidke Skill Score')
	}
	stat <- data.frame(Stat = name_stat, Value = round(stat, 3), Description = descrip)
	return(stat)
}

########################

ExtractNC2Stn <- function(retValidParams){
	freqData <- retValidParams$stn$donne$freq
	lon.stn <- retValidParams$stn$donne$lon
	lat.stn <- retValidParams$stn$donne$lat
	date.stn <- retValidParams$stn$donne$date
	data.stn <- retValidParams$stn$donne$data

	#######
	start_year <- as.numeric(retValidParams$dates$start_year)
	end_year <- as.numeric(retValidParams$dates$end_year)
	start_mois <- retValidParams$dates$start_mois
	end_mois <- retValidParams$dates$end_mois
	mois <- as.numeric(substr(date.stn, 5, 6))
	year.stn <- as.numeric(substr(date.stn, 1, 4))
	iyear <- year.stn >= start_year & year.stn <= end_year

	im <- getMonthsInSeason(start_mois, end_mois, full = TRUE)
	imois <- mois%in%im
	iseas <- imois & iyear

	if(!is.null(EnvRainValidation$valid.data)){
		date0 <- EnvRainValidation$valid.data$date
		exist.date <- date.stn %in% date0
		iseas1 <- iseas & !exist.date
		if(!any(iseas1)) return(EnvRainValidation$extr_stn)
	}else iseas1 <- iseas
	date.stn <- date.stn[iseas1]
	data.stn <- data.stn[iseas1, , drop = FALSE]

	#####
	grd.lon <- retValidParams$rfe$rfedata[[2]]$x
	grd.lat <- retValidParams$rfe$rfedata[[2]]$y
	ncdir <- retValidParams$rfe$ncdir
	ncformat <- retValidParams$rfe$ncformat

	#Index of gridded data over stations
	ijGrd <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = grd.lon, lat = grd.lat))

	if(freqData == 'daily') testfile <- file.path(ncdir, sprintf(ncformat, substr(date.stn, 1,4), substr(date.stn, 5,6), substr(date.stn, 7,8)))
	if(freqData == 'dekadal') testfile <- file.path(ncdir, sprintf(ncformat, substr(date.stn, 1,4), substr(date.stn, 5,6), substr(date.stn, 7,7)))
	if(freqData == 'monthly') testfile <- file.path(ncdir, sprintf(ncformat, substr(date.stn, 1,4), substr(date.stn, 5,6)))

	existFl <- unlist(lapply(testfile, file.exists))
	if(!any(existFl)){
		InsertMessagesTxt(main.txt.out, "RFE data not found", format = TRUE)
		return(NULL)
	}
	rfeDataFl <- testfile[existFl]
	date.stn1 <- date.stn[existFl]
	data.stn1 <- data.stn[existFl, , drop = FALSE]

	if(doparallel & length(rfeDataFl) >= 90){
		klust <- makeCluster(nb_cores)
		registerDoParallel(klust)
		`%parLoop%` <- `%dopar%`
		closeklust <- TRUE
	}else{
		`%parLoop%` <- `%do%`
		closeklust <- FALSE
	}

	packages <- c('ncdf4')
	toExports <- c('rfeDataFl', 'date.stn1', 'retValidParams', 'ijGrd')
	ret <- foreach(jfl = seq_along(rfeDataFl), .combine = 'rbind',
					.packages = packages, .export = toExports) %parLoop% {
		nc <- nc_open(rfeDataFl[jfl])
		rfe.lon <- nc$dim[[retValidParams$rfe$rfedata[[2]]$ilon]]$vals
		rfe.lat <- nc$dim[[retValidParams$rfe$rfedata[[2]]$ilat]]$vals
		rfe.val <- ncvar_get(nc, varid = retValidParams$rfe$rfedata[[2]]$varid)
		nc_close(nc)

		xo <- order(rfe.lon)
		rfe.lon <- rfe.lon[xo]
		yo <- order(rfe.lat)
		rfe.lat <- rfe.lat[yo]
		rfe.val <- rfe.val[xo, yo]

		if(retValidParams$rfe$rfedata[[2]]$ilat == 1){
			rfe.val <- matrix(c(rfe.val), nrow = length(rfe.lon), ncol = length(rfe.lat), byrow = TRUE)
		}
		c(as.numeric(date.stn1[jfl]), rfe.val[ijGrd])
	}
	if(closeklust) stopCluster(klust)

	rfe_stn <- matrix(NA, nrow = length(date.stn1), ncol = length(lon.stn))
	rfe_stn[match(as.character(ret[, 1]), date.stn1), ] <- ret[, -1]

	if(is.null(EnvRainValidation$valid.data)){
		ret <- list(rfe = rfe_stn, stn = data.stn1, date = date.stn1)
		assign('valid.data', ret, envir = EnvRainValidation)
	}else{
		date1 <- c(EnvRainValidation$valid.data$date, date.stn1)
		stn1 <- rbind(EnvRainValidation$valid.data$stn, data.stn1)
		rfe1 <- rbind(EnvRainValidation$valid.data$rfe, rfe_stn)

		iorder <- order(date1)
		date1 <- date1[iorder]
		stn1 <- stn1[iorder, ]
		rfe1 <- rfe1[iorder, ]
		assign('valid.data', list(rfe = rfe1, stn = stn1, date = date1), envir = EnvRainValidation)

		year.v <- as.numeric(substr(date1, 1, 4))
		mois.v <- as.numeric(substr(date1, 5, 6))
		iyear <- year.v >= start_year & year.v <= end_year
		imois <- mois.v%in%im
		iseas <- imois & iyear
		ret <- list(rfe = rfe1[iseas, ], stn = stn1[iseas, ], date = date1[iseas])
	}
	assign('extr_stn', ret, envir = EnvRainValidation)
	assign('YEAR', unique(as.numeric(substr(EnvRainValidation$valid.data$date, 1, 4))), envir = EnvRainValidation)
	assign('MONTH', unique(as.numeric(substr(EnvRainValidation$valid.data$date, 5, 6))), envir = EnvRainValidation)
	return(ret)
}

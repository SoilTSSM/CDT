
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

	extr_stn <- if(retValidParams$do_extr == 1) ExtractNC2Stn(retValidParams) else EnvRainValidation$extr_stn
	if(is.null(extr_stn)) return(NULL)

	stn.dates <- extr_stn$date
	stn.data <- extr_stn$stn
	stn.ncdata <- extr_stn$rfe

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
		stn.id <- retValidParams$stn$donne$id[ixy]
	}

	inNA <- !is.na(stn.data) & !is.na(stn.ncdata)
	stn.data[!inNA] <- NA
	stn.ncdata[!inNA] <- NA
	stn <- stn.data[inNA]
	grd <- stn.ncdata[inNA]

	####
	scatt.dat <- data.frame(stn.id = rep(stn.id, each = nrow(stn.data)),
							date = rep(stn.dates, ncol(stn.data)),
							stn = c(stn.data), grd = c(stn.ncdata))
	scatt.dat <- scatt.dat[!is.na(scatt.dat$stn), ]
	file.scatt.dat <- file.path(outValidation, 'STN_GRD_VALIDATION_DATA.txt')
	write.table(scatt.dat, file.scatt.dat, col.names = TRUE, row.names = FALSE, quote = FALSE)

	stat <- validationStats(stn, grd, retValidParams)
	file.stat <- file.path(outValidation, 'STN_GRD_Validation_Statistics.txt')
	write.table(stat, file.stat, col.names = TRUE, row.names = FALSE)

	###
	if(retValidParams$clim.var == 'RR'){
		xlab <- "Rainfall [mm]"
		ylab <- 'RFE'
		lgtxt <- c('Gauge', 'RFE')
	}
	if(retValidParams$clim.var == 'TT'){
		xlab <- "Temperature"
		ylab <- 'Estimate'
		lgtxt <- c('Gauge', 'Estimate')
	}

	scatter.file <- file.path(outValidation, 'STN_GRD_SCATTER_PLOT.jpg')
	if(Sys.info()["sysname"] == "Windows") jpeg(scatter.file, restoreConsole = FALSE) else jpeg(scatter.file)
	grphlim <- c(0, max(c(stn, grd), na.rm = TRUE))
	plot(stn, grd, xlab = "Gauge", ylab = ylab, xlim = grphlim, ylim = grphlim, type = 'n')
	grid()
	abline(a = 0, b = 1, lwd = 2, col = 'red')
	points(stn, grd)
	dev.off()

	cdf.file <- file.path(outValidation, 'STN_GRD_ECDF_PLOT.jpg')
	if(Sys.info()["sysname"] == "Windows") jpeg(cdf.file, restoreConsole = FALSE) else jpeg(cdf.file)
	plot(ecdf(stn), xlab = xlab, ylab = "CDF", main = 'Empirical CDF', col = 'blue', lwd = 2, cex = 0.4, ylim = c(0, 1))
	grid()
	plot(ecdf(grd), add = TRUE, col = "red", lwd = 2, cex = 0.4)
	legend('bottomright', lgtxt, col = c('blue', 'red'), lwd = 3, bg = 'lightgoldenrodyellow')
	dev.off()

	###
	gg_tms <- apply(stn.data, 1, mean, na.rm = TRUE)
	gg_tms[is.nan(gg_tms)] <- NA
	rfe_tms <- apply(stn.ncdata, 1, mean, na.rm = TRUE)
	rfe_tms[is.nan(rfe_tms)] <- NA

	###
	area_avg <- data.frame(date = stn.dates, stn = round(gg_tms, 1), grd = round(rfe_tms, 1))
	file.area.avg <- file.path(outValidation, 'Spatial_Average_STN_GRD_VALIDATION_DATA.txt')
	write.table(area_avg, file.area.avg, col.names = TRUE, row.names = FALSE, quote = FALSE)

	area_stat <- validationStats(gg_tms, rfe_tms, retValidParams)
	file.area.stat <- file.path(outValidation, 'Spatial_Average_Validation_Statistics.txt')
	write.table(area_stat, file.area.stat, col.names = TRUE, row.names = FALSE)

	###
	s.scatter.file <- file.path(outValidation, 'Spatial_Average_STN_GRD_SCATTER_PLOT.jpg')
	if(Sys.info()["sysname"] == "Windows") jpeg(s.scatter.file, restoreConsole = FALSE) else jpeg(s.scatter.file)
	grphlim <- c(0, max(c(gg_tms, rfe_tms), na.rm = TRUE))
	plot(gg_tms, rfe_tms, xlab = "Gauge", ylab = ylab, xlim = grphlim, ylim = grphlim, type = 'n')
	grid()
	abline(a = 0, b = 1, lwd = 2, col = 'red')
	points(gg_tms, rfe_tms)
	dev.off()

	s.cdf.file <- file.path(outValidation, 'Spatial_Average_STN_GRD_ECDF_PLOT.jpg')
	if(Sys.info()["sysname"] == "Windows") jpeg(s.cdf.file, restoreConsole = FALSE) else jpeg(s.cdf.file)
	plot(ecdf(gg_tms), xlab = xlab, ylab = "CDF", main = 'Empirical CDF', col = 'blue', lwd = 2, cex = 0.4, ylim = c(0, 1))
	grid()
	plot(ecdf(rfe_tms), add = TRUE, col = "red", lwd = 2, cex = 0.4)
	legend('bottomright', lgtxt, col = c('blue', 'red'), lwd = 3, bg = 'lightgoldenrodyellow')
	dev.off()

	return(list(x = stn.data[inNA], y = stn.ncdata[inNA], stat = stat,
				xs = gg_tms, ys = rfe_tms, sp.stat = area_stat,
				clim.var = retValidParams$clim.var))
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
		descrip <- c(descrip, 'Probability Of Detection', 'False Alarm Ratio', 'Frequency Bias',
								'Critical Success Index', 'Heidke Skill Score')
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
		if(!is.null(EnvRainValidation$valid.data)) return(EnvRainValidation$extr_stn)
		else{
			InsertMessagesTxt(main.txt.out, "RFE data not found", format = TRUE)
			return(NULL)
		}
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

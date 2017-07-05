
climdexCalc.TT <- function(GeneralParameters){
	noTmax <- FALSE
	noTmin <- FALSE
	if(GeneralParameters$IO.files$TX.dir.file == "" | GeneralParameters$IO.files$TX.dir.file == "NA"){
		InsertMessagesTxt(main.txt.out, 'No daily maximum temperature data found', format = TRUE)
		noTmax <- TRUE
	}

	if(GeneralParameters$IO.files$TN.dir.file == "" | GeneralParameters$IO.files$TN.dir.file == "NA"){
		if(GeneralParameters$data.type == 'series' & GeneralParameters$One.series$file.format == "3") noTmin <- FALSE
		else{
			InsertMessagesTxt(main.txt.out, 'No daily minimum temperature data found', format = TRUE)
			noTmin <- TRUE
		}
	}

	if(!file.exists(GeneralParameters$IO.files$Out.dir.file)){
		InsertMessagesTxt(main.txt.out, 'Directory to save computed indices not found', format = TRUE)
		InsertMessagesTxt(main.txt.out, paste('The indices will be put in', getwd()))
		GeneralParameters$IO.files$Out.dir.file <- getwd()
	}

	#############
	indexDIR <- file.path(GeneralParameters$IO.files$Out.dir.file, "ClimdexTT")
	if(file.exists(indexDIR)){
		msg <- paste0("The directory\n", indexDIR, "\nalready exists. Do you want to overwrite it?")
		retval <- tkmessageBox(message = msg, icon = "question", type = "yesno", default = "yes")
		retval <- substr(tolower(tclvalue(retval)), 1, 1)
		if(retval == 'n'){
			InsertMessagesTxt(main.txt.out, 'Change the directory to save indices', format = TRUE)
			return(NULL)
		}
		tcl("update")
	}else dir.create(indexDIR, showWarnings = FALSE, recursive = TRUE)

	#############
	if(GeneralParameters$data.type == 'cdt'){
		if(!noTmax){
			cdtTmax <- getStnOpenData(GeneralParameters$IO.files$TX.dir.file)
			if(is.null(cdtTmax)) noTmax <- TRUE
			if(!noTmax){
				cdtTmax <- getCDTdataAndDisplayMsg(cdtTmax, "daily")
				if(is.null(cdtTmax)) noTmax <- TRUE
				if(!noTmax){
					cdtInfo <- getStnOpenDataInfo(GeneralParameters$IO.files$TX.dir.file)
					miss.val1 <- cdtInfo[[3]]$miss.val
					cdtTmax <- cdtTmax[c('id', 'lon', 'lat', 'dates', 'data')]
				}
			}
		}

		######
		if(!noTmin){
			cdtTmin <- getStnOpenData(GeneralParameters$IO.files$TN.dir.file)
			if(is.null(cdtTmin)) noTmin <- TRUE
			if(!noTmin){
				cdtTmin <- getCDTdataAndDisplayMsg(cdtTmin, "daily")
				if(is.null(cdtTmin)) noTmin <- TRUE
				if(!noTmin){
					cdtInfo <- getStnOpenDataInfo(GeneralParameters$IO.files$TN.dir.file)
					miss.val2 <- cdtInfo[[3]]$miss.val
					cdtTmin <- cdtTmin[c('id', 'lon', 'lat', 'dates', 'data')]
				}
			}
		}
	}

	if(GeneralParameters$data.type == 'series'){
		if(!noTmax){
			cdtTmax <- getStnOpenData(GeneralParameters$IO.files$TX.dir.file)
			if(is.null(cdtTmax)) noTmax <- TRUE
			if(!noTmax){
				cdtTmax <- getCDTTSdataAndDisplayMsg(cdtTmax, "daily",
													GeneralParameters$One.series$file.format,
													GeneralParameters$One.series$date.format)
				if(is.null(cdtTmax)) noTmax <- TRUE
				if(!noTmax){
					cdtInfo <- getStnOpenDataInfo(GeneralParameters$IO.files$TX.dir.file)
					miss.val1 <- cdtInfo[[3]]$miss.val
					nbvar <- cdtTmax$nbvar
					if(nbvar == 3){
						tt.mat <- as.matrix(cbind(cdtTmax$var$tx, cdtTmax$var$tn))
						stxtn <- TRUE
					}else{
						tt.mat <- matrix(cdtTmax$var$var, ncol = 1)
						stxtn <- FALSE
					}

					cdtTmax <- list(
						id = GeneralParameters$One.series$id,
						lon = GeneralParameters$One.series$lon,
						lat = GeneralParameters$One.series$lat,
						dates = cdtTmax$dates,
						data = tt.mat
					)
				}
			}
		}

		######
		if(nbvar == 3 & stxtn){
			cdtTmin <- cdtTmax
			cdtTmax$data <- cdtTmax$data[, 1, drop = FALSE]
			cdtTmin$data <- cdtTmin$data[, 2, drop = FALSE]
			miss.val2 <- miss.val1
		}else{
			if(!noTmin){
				cdtTmin <- getStnOpenData(GeneralParameters$IO.files$TN.dir.file)
				if(is.null(cdtTmin)) noTmin <- TRUE
				if(!noTmin){
					cdtTmin <- getCDTTSdataAndDisplayMsg(cdtTmin, "daily",
														GeneralParameters$One.series$file.format,
														GeneralParameters$One.series$date.format)
					if(is.null(cdtTmin)) noTmin <- TRUE
					if(!noTmin){
						cdtInfo <- getStnOpenDataInfo(GeneralParameters$IO.files$TN.dir.file)
						miss.val2 <- cdtInfo[[3]]$miss.val
						cdtTmin <- list(
							id = GeneralParameters$One.series$id,
							lon = GeneralParameters$One.series$lon,
							lat = GeneralParameters$One.series$lat,
							dates = cdtTmin$dates,
							data = matrix(cdtTmin$var$var, ncol = 1)
						)
					}
				}
			}			
		}
	}

	if(GeneralParameters$data.type == 'netcdf'){
		dstart <- try(as.Date(paste(GeneralParameters$ncdf.file$start.year,
									GeneralParameters$ncdf.file$start.mon,
									GeneralParameters$ncdf.file$start.day, sep = '-')),
						silent = TRUE)
		dend <- try(as.Date(paste(GeneralParameters$ncdf.file$end.year,
								GeneralParameters$ncdf.file$end.mon,
								GeneralParameters$ncdf.file$end.day, sep = '-')),
					silent = TRUE)
		if(inherits(dstart, "try-error")){
			InsertMessagesTxt(main.txt.out, "Wrong start date for netcdf data", format = TRUE)
			return(NULL)
		}
		if(inherits(dend, "try-error")){
			InsertMessagesTxt(main.txt.out, "Wrong end date for netcdf data", format = TRUE)
			return(NULL)
		}

		dates <- format(seq(dstart, dend, 'day'), '%Y%m%d')

		######
		if(!noTmax){
			filein0 <- file.path(GeneralParameters$IO.files$TX.dir.file,
								sprintf(GeneralParameters$ncdf.file$TX.format,
								as.character(substr(dates, 1, 4)),
								as.character(substr(dates, 5, 6)),
								as.character(substr(dates, 7, 8))))

			existFl <- unlist(lapply(filein0, file.exists))
			if(!any(existFl)){
				InsertMessagesTxt(main.txt.out, "Unable to locate daily maximum temperature files", format = TRUE)
				noTmax <- TRUE
			}

			if(!noTmax){
				ncInfo <- getRFESampleData(GeneralParameters$ncdf.file$TX.ncsample)
				if(is.null(ncInfo)){
					InsertMessagesTxt(main.txt.out, "No daily maximum temperature sample file found", format = TRUE)
					noTmax <- TRUE
				}
				if(!noTmax){
					nc <- nc_open(filein0[which(existFl)[1]])
					lon <- nc$dim[[ncInfo$rfeILon]]$vals
					lat <- nc$dim[[ncInfo$rfeILat]]$vals
					nc_close(nc)

					InsertMessagesTxt(main.txt.out, 'Read daily maximum temperature data ...')
					
					is.parallel <- doparallel(length(which(existFl)) >= 180)
					`%parLoop%` <- is.parallel$dofun

					xo <- order(lon)
					lon <- lon[xo]
					yo <- order(lat)
					lat <- lat[yo]

					ncData <- foreach(jj = seq_along(filein0), .packages = "ncdf4",
									.export = c("existFl", "filein0", "ncInfo")) %parLoop% {
						if(existFl[jj]){
							nc <- nc_open(filein0[jj])
							vars <- ncvar_get(nc, varid = ncInfo$rfeVarid)
							nc_close(nc)
							vars <- vars[xo, yo]
							if(ncInfo$rfeILat < ncInfo$rfeILon){
								vars <- matrix(c(vars), nrow = length(lon), ncol = length(lat), byrow = TRUE)
							}
						}else vars <- NULL
						vars
					}
					if(is.parallel$stop) stopCluster(is.parallel$cluster)

					InsertMessagesTxt(main.txt.out, 'Reading daily maximum temperature data finished')

					ncData[sapply(ncData, is.null)] <- list(matrix(NA, nrow = length(lon), ncol = length(lat)))
					xycrd <- expand.grid(lon, lat)

					####
					nxX <- length(lon)
					nyX <- length(lat)
					dx <- ncdim_def("Lon", "degreeE", lon)
					dy <- ncdim_def("Lat", "degreeN", lat)
					xy.dimX <- list(dx, dy)
					grd.slpX <- ncvar_def("slope", "", xy.dimX, NA, longname = "Slope estimate", prec = "float")
					grd.std.slpX <- ncvar_def("std.slope", "", xy.dimX, NA, longname = "Slope error", prec = "float")
					grd.pvalueX <- ncvar_def("pvalue", "", xy.dimX, NA, longname = "P-value", prec = "float")
					grd.r2X <- ncvar_def("r2", "", xy.dimX, NA, longname = "Coefficient of determination R2", prec = "float")
					trend.varsX <- list(grd.slpX, grd.std.slpX, grd.pvalueX, grd.r2X)

					####
					cdtTmax <- list(
						id = paste("p", seq(nrow(xycrd)), sep = ""),
						lon = xycrd[, 1],
						lat = xycrd[, 2],
						dates = dates,
						data = t(sapply(ncData, c))
					)
					rm(ncData)
				}
			}
		}

		######
		if(!noTmin){
			filein0 <- file.path(GeneralParameters$IO.files$TN.dir.file,
								sprintf(GeneralParameters$ncdf.file$TN.format,
								as.character(substr(dates, 1, 4)),
								as.character(substr(dates, 5, 6)),
								as.character(substr(dates, 7, 8))))

			existFl <- unlist(lapply(filein0, file.exists))
			if(!any(existFl)){
				InsertMessagesTxt(main.txt.out, "Unable to locate daily minimum temperature files", format = TRUE)
				noTmin <- TRUE
			}

			if(!noTmin){
				ncInfo <- getRFESampleData(GeneralParameters$ncdf.file$TN.ncsample)
				if(is.null(ncInfo)){
					InsertMessagesTxt(main.txt.out, "No daily minimum temperature sample file found", format = TRUE)
					noTmin <- TRUE
				}
				if(!noTmin){
					nc <- nc_open(filein0[which(existFl)[1]])
					lon <- nc$dim[[ncInfo$rfeILon]]$vals
					lat <- nc$dim[[ncInfo$rfeILat]]$vals
					nc_close(nc)

					InsertMessagesTxt(main.txt.out, 'Read daily minimum temperature data ...')
					
					is.parallel <- doparallel(length(which(existFl)) >= 180)
					`%parLoop%` <- is.parallel$dofun

					xo <- order(lon)
					lon <- lon[xo]
					yo <- order(lat)
					lat <- lat[yo]

					ncData <- foreach(jj = seq_along(filein0), .packages = "ncdf4",
									.export = c("existFl", "filein0", "ncInfo")) %parLoop% {
						if(existFl[jj]){
							nc <- nc_open(filein0[jj])
							vars <- ncvar_get(nc, varid = ncInfo$rfeVarid)
							nc_close(nc)
							vars <- vars[xo, yo]
							if(ncInfo$rfeILat < ncInfo$rfeILon){
								vars <- matrix(c(vars), nrow = length(lon), ncol = length(lat), byrow = TRUE)
							}
						}else vars <- NULL
						vars
					}
					if(is.parallel$stop) stopCluster(is.parallel$cluster)

					InsertMessagesTxt(main.txt.out, 'Reading daily minimum temperature data finished')

					ncData[sapply(ncData, is.null)] <- list(matrix(NA, nrow = length(lon), ncol = length(lat)))
					xycrd <- expand.grid(lon, lat)

					####
					nxN <- length(lon)
					nyN <- length(lat)
					dx <- ncdim_def("Lon", "degreeE", lon)
					dy <- ncdim_def("Lat", "degreeN", lat)
					xy.dimN <- list(dx, dy)
					grd.slpN <- ncvar_def("slope", "", xy.dimN, NA, longname = "Slope estimate", prec = "float")
					grd.std.slpN <- ncvar_def("std.slope", "", xy.dimN, NA, longname = "Slope error", prec = "float")
					grd.pvalueN <- ncvar_def("pvalue", "", xy.dimN, NA, longname = "P-value", prec = "float")
					grd.r2N <- ncvar_def("r2", "", xy.dimN, NA, longname = "Coefficient of determination R2", prec = "float")
					trend.varsN <- list(grd.slpN, grd.std.slpN, grd.pvalueN, grd.r2N)

					####
					cdtTmin <- list(
						id = paste("p", seq(nrow(xycrd)), sep = ""),
						lon = xycrd[, 1],
						lat = xycrd[, 2],
						dates = dates,
						data = t(sapply(ncData, c))
					)
					rm(ncData)
				}
			}
		}
	}

	#############
	is.TXx <- GeneralParameters$Indices$TXx
	is.TXn <- GeneralParameters$Indices$TXn
	is.TX10p <- GeneralParameters$Indices$TX10p
	is.TX90p <- GeneralParameters$Indices$TX90p
	is.WSDI <- GeneralParameters$Indices$WSDI
	is.SU <- GeneralParameters$Indices$SU
	is.ID <- GeneralParameters$Indices$ID
	is.TNx <- GeneralParameters$Indices$TNx
	is.TNn <- GeneralParameters$Indices$TNn
	is.TN10p <- GeneralParameters$Indices$TN10p
	is.TN90p <- GeneralParameters$Indices$TN90p
	is.CSDI <- GeneralParameters$Indices$CSDI
	is.TR <- GeneralParameters$Indices$TR
	is.FD <- GeneralParameters$Indices$FD
	is.DTR <- GeneralParameters$Indices$DTR
	is.GSL <- GeneralParameters$Indices$GSL

	indxTX <- c("TXx", "TXn", "TX10p", "TX90p", "WSDI", "SU", "ID")
	indxTN <- c("TNx", "TNn", "TN10p", "TN90p", "CSDI", "TR", "FD")
	indxTXN <- c("DTR", "GSL")

	jTmaxmin <- !noTmax & !noTmin
	jTmax <- !noTmax & noTmin
	jTmin <- noTmax & !noTmin

	if(jTmaxmin){
		indxlst <- c(indxTX, indxTN, indxTXN)
		if(!any(unlist(GeneralParameters$Indices[indxlst]))){
			InsertMessagesTxt(main.txt.out, 'No indices selected.', format = TRUE)
			return(0)
		}
		cdtdata <- climdex.completeYear(cdtTmax, cdtTmin)
		cdtTmax <- cdtdata[[1]]
		cdtTmin <- cdtdata[[2]]
		combine <- TRUE
		nbTmax <- 1:ncol(cdtTmax$data)
		nbTmin <- ncol(cdtTmax$data)+(1:ncol(cdtTmin$data))
		cdtdata <- cdtTmin
		cdtdata$id <- c(cdtTmax$id, cdtTmin$id)
		cdtdata$lon <- c(cdtTmax$lon, cdtTmin$lon)
		cdtdata$lat <- c(cdtTmax$lat, cdtTmin$lat)
		cdtdata$data <- cbind(cdtTmax$data, cdtTmin$data)
	}else if(jTmax){
		if(!any(unlist(GeneralParameters$Indices[indxTX]))){
			InsertMessagesTxt(main.txt.out, 'No indices selected.', format = TRUE)
			return(0)
		}
		InsertMessagesTxt(main.txt.out, 'No daily minimum temperature data found.', format = TRUE)
		InsertMessagesTxt(main.txt.out, paste(paste(indxTN, collapse = ", "), ", DTR, GSL", "will not be calculated"))
		cdtTmax <- climdex.completeYear(cdtTmax)
		combine <- FALSE
		cdtdata <- cdtTmax
	}else if(jTmin){
		if(!any(unlist(GeneralParameters$Indices[indxTN]))){
			InsertMessagesTxt(main.txt.out, 'No indices selected.', format = TRUE)
			return(0)
		}
		InsertMessagesTxt(main.txt.out, 'No daily maximum temperature data found.', format = TRUE)
		InsertMessagesTxt(main.txt.out, paste(paste(indxTX, collapse = ", "), ", DTR, GSL", "will not be calculated"))
		cdtTmin <- climdex.completeYear(cdtTmin)
		combine <- FALSE
		cdtdata <- cdtTmin
	}else{
		InsertMessagesTxt(main.txt.out, 'No daily temperature data found', format = TRUE)
		return(NULL)
	}

	#############

	separe <- FALSE
	nacount <- climdex.NACount.MonDay(cdtdata, separe = separe)

	#############

	TXn <- TXx <- TNn <- TNx <- NULL

	if(is.TXx | is.TXn | is.TNx | is.TNn){
		TXNxn <- climdex.TempDailyExtreme(cdtdata, nacount, separe = separe)
		# TXn: Monthly minimum value of daily maximum temperature
		if(is.TXn){
			if(jTmaxmin){
				TXn <- list(mIdx = list(date = TXNxn$mTTxn$date, data = TXNxn$mTTxn$data$low[, nbTmax, drop = FALSE]),
							yIdx = list(date = TXNxn$yTTxn$date, data = TXNxn$yTTxn$data$low[, nbTmax, drop = FALSE]),
							tIdx = TXNxn$tTTn[, nbTmax, drop = FALSE])
			}

			if(jTmax){
				TXn <- list(mIdx = list(date = TXNxn$mTTxn$date, data = TXNxn$mTTxn$data$low),
							yIdx = list(date = TXNxn$yTTxn$date, data = TXNxn$yTTxn$data$low),
							tIdx = TXNxn$tTTn)
			}
		}

		# TXx: Monthly maximum value of daily maximum temperature
		if(is.TXx){
			if(jTmaxmin){
				TXx <- list(mIdx = list(date = TXNxn$mTTxn$date, data = TXNxn$mTTxn$data$up[, nbTmax, drop = FALSE]),
							yIdx = list(date = TXNxn$yTTxn$date, data = TXNxn$yTTxn$data$up[, nbTmax, drop = FALSE]),
							tIdx = TXNxn$tTTx[, nbTmax, drop = FALSE])
			}

			if(jTmax){
				TXx <- list(mIdx = list(date = TXNxn$mTTxn$date, data = TXNxn$mTTxn$data$up),
							yIdx = list(date = TXNxn$yTTxn$date, data = TXNxn$yTTxn$data$up),
							tIdx = TXNxn$tTTx)
			}
		}

		# TNn: Monthly minimum value of daily minimum temperature
		if(is.TNn){
			if(jTmaxmin){
				TNn <- list(mIdx = list(date = TXNxn$mTTxn$date, data = TXNxn$mTTxn$data$low[, nbTmin, drop = FALSE]),
							yIdx = list(date = TXNxn$yTTxn$date, data = TXNxn$yTTxn$data$low[, nbTmin, drop = FALSE]),
							tIdx = TXNxn$tTTn[, nbTmin, drop = FALSE])
			}

			if(jTmin){
				TNn <- list(mIdx = list(date = TXNxn$mTTxn$date, data = TXNxn$mTTxn$data$low),
							yIdx = list(date = TXNxn$yTTxn$date, data = TXNxn$yTTxn$data$low),
							tIdx = TXNxn$tTTn)
			}
		}

		# TNx: Monthly maximum value of daily minimum temperature
		if(is.TNx){
			if(jTmaxmin){
				TNx <- list(mIdx = list(date = TXNxn$mTTxn$date, data = TXNxn$mTTxn$data$up[, nbTmin, drop = FALSE]),
							yIdx = list(date = TXNxn$yTTxn$date, data = TXNxn$yTTxn$data$up[, nbTmin, drop = FALSE]),
							tIdx = TXNxn$tTTx[, nbTmin, drop = FALSE])
			}

			if(jTmin){
				TNx <- list(mIdx = list(date = TXNxn$mTTxn$date, data = TXNxn$mTTxn$data$up),
							yIdx = list(date = TXNxn$yTTxn$date, data = TXNxn$yTTxn$data$up),
							tIdx = TXNxn$tTTx)
			}
		}

		#############
		ncparsTXn <- ncparsTXx <- ncparsTNn <- ncparsTNx <- NULL

		if(GeneralParameters$data.type == 'netcdf'){
			if(is.TXn) ncparsTXn <- list(trend.vars = trend.varsX, xy.dim = xy.dimX, nx = nxX, ny = nyX,
							nc.unit = "degC", nc.prec = "float",
							longname.yrs = "Annual minimum value of daily maximum temperature",
							longname.mon = "Monthly minimum value of daily maximum temperature")

			if(is.TXx) ncparsTXx <- list(trend.vars = trend.varsX, xy.dim = xy.dimX, nx = nxX, ny = nyX,
							nc.unit = "degC", nc.prec = "float",
							longname.yrs = "Annual maximum value of daily maximum temperature",
							longname.mon = "Monthly maximum value of daily maximum temperature")

			if(is.TNn) ncparsTNn <- list(trend.vars = trend.varsN, xy.dim = xy.dimN, nx = nxN, ny = nyN,
							nc.unit = "degC", nc.prec = "float",
							longname.yrs = "Annual minimum value of daily minimum temperature",
							longname.mon = "Monthly minimum value of daily minimum temperature")

			if(is.TNx) ncparsTNx <- list(trend.vars = trend.varsN, xy.dim = xy.dimN, nx = nxN, ny = nyN,
							nc.unit = "degC", nc.prec = "float",
							longname.yrs = "Annual maximum value of daily minimum temperature",
							longname.mon = "Monthly maximum value of daily minimum temperature")
		}else{
			if(is.TXn) ncparsTXn <- list(head = cdtTmax[c('id', 'lon', 'lat')])
			if(is.TXx) ncparsTXx <- list(head = cdtTmax[c('id', 'lon', 'lat')])
			if(is.TNn) ncparsTNn <- list(head = cdtTmin[c('id', 'lon', 'lat')])
			if(is.TNx) ncparsTNx <- list(head = cdtTmin[c('id', 'lon', 'lat')])
		}

		climdex.Write.Indices(TXn, "TXn", GeneralParameters$data.type, indexDIR, nc.pars = ncparsTXn)
		climdex.Write.Indices(TXx, "TXx", GeneralParameters$data.type, indexDIR, nc.pars = ncparsTXx)
		climdex.Write.Indices(TNn, "TNn", GeneralParameters$data.type, indexDIR, nc.pars = ncparsTNn)
		climdex.Write.Indices(TNx, "TNx", GeneralParameters$data.type, indexDIR, nc.pars = ncparsTNx)
	}

	#############
	MONTHQ1090 <- NULL

	maxpercNA <- 25
	winsize <- 5

	if(is.TN10p | is.TN90p | is.TX10p | is.TX90p | is.WSDI | is.CSDI){
		startyear <- GeneralParameters$baseYear$start.year
		endyear <- GeneralParameters$baseYear$end.year
		if(is.na(startyear) | is.na(endyear)){
			InsertMessagesTxt(main.txt.out, 'Base period is missing, all years will be used to compute climatology', format = TRUE)
			baseYear <- as.numeric(substr(c(cdtdata$dates[1], cdtdata$dates[length(cdtdata$dates)]), 1, 4))
			startyear <- baseYear[1]
			endyear <- baseYear[2]
		}

		MONTHQ1090 <- climdex.MONTHQ1090(cdtdata, startyear, endyear, winsize, combine, maxpercNA, separe = separe)
	}

	#############

	TX90p <- TX10p <- TN90p <- TN10p <- NULL

	if(!is.null(MONTHQ1090)){
		bootsrap <- if(GeneralParameters$data.type == 'netcdf') FALSE else TRUE

		if(is.TX10p | is.TX90p | is.TN10p | is.TN90p){
			TXTN1090p <- climdex.TempQ1090(MONTHQ1090, bootsrap)
			if(!is.null(TXTN1090p)){
				# TX10p, Percentage of days when TX < 10th percentile
				if(is.TX10p){
					if(jTmaxmin){
						TX10p <- list(mIdx = list(date = TXTN1090p$mon.date, data = TXTN1090p$Tt10p[, nbTmax, drop = FALSE]),
									yIdx = list(date = TXTN1090p$yrs.date, data = TXTN1090p$Ttg10p[, nbTmax, drop = FALSE]),
									tIdx = TXTN1090p$tr10p[, nbTmax, drop = FALSE])
					}

					if(jTmax){
						TX10p <- list(mIdx = list(date = TXTN1090p$mon.date, data = TXTN1090p$Tt10p),
									yIdx = list(date = TXTN1090p$yrs.date, data = TXTN1090p$Ttg10p),
									tIdx = TXTN1090p$tr10p)
					}
				}

				# TX90p, Percentage of days when TX > 90th percentile
				if(is.TX90p){
					if(jTmaxmin){
						TX90p <- list(mIdx = list(date = TXTN1090p$mon.date, data = TXTN1090p$Tt90p[, nbTmax, drop = FALSE]),
									yIdx = list(date = TXTN1090p$yrs.date, data = TXTN1090p$Ttg90p[, nbTmax, drop = FALSE]),
									tIdx = TXTN1090p$tr90p[, nbTmax, drop = FALSE])
					}

					if(jTmax){
						TX90p <- list(mIdx = list(date = TXTN1090p$mon.date, data = TXTN1090p$Tt90p),
									yIdx = list(date = TXTN1090p$yrs.date, data = TXTN1090p$Ttg90p),
									tIdx = TXTN1090p$tr90p)
					}
				}

				# TN10p, Percentage of days when TN < 10th percentile
				if(is.TN10p){
					if(jTmaxmin){
						TN10p <- list(mIdx = list(date = TXTN1090p$mon.date, data = TXTN1090p$Tt10p[, nbTmin, drop = FALSE]),
									yIdx = list(date = TXTN1090p$yrs.date, data = TXTN1090p$Ttg10p[, nbTmin, drop = FALSE]),
									tIdx = TXTN1090p$tr10p[, nbTmin, drop = FALSE])
					}

					if(jTmin){
						TN10p <- list(mIdx = list(date = TXTN1090p$mon.date, data = TXTN1090p$Tt10p),
									yIdx = list(date = TXTN1090p$yrs.date, data = TXTN1090p$Ttg10p),
									tIdx = TXTN1090p$tr10p)
					}
				}

				# TN90p, Percentage of days when TN > 90th percentile
				if(is.TN90p){
					if(jTmaxmin){
						TN90p <- list(mIdx = list(date = TXTN1090p$mon.date, data = TXTN1090p$Tt90p[, nbTmin, drop = FALSE]),
									yIdx = list(date = TXTN1090p$yrs.date, data = TXTN1090p$Ttg90p[, nbTmin, drop = FALSE]),
									tIdx = TXTN1090p$tr90p[, nbTmin, drop = FALSE])
					}

					if(jTmin){
						TN90p <- list(mIdx = list(date = TXTN1090p$mon.date, data = TXTN1090p$Tt90p),
									yIdx = list(date = TXTN1090p$yrs.date, data = TXTN1090p$Ttg90p),
									tIdx = TXTN1090p$tr90p)
					}
				}
			}

			#############
			ncparsTX10p <- ncparsTX90p <- ncparsTN10p <- ncparsTN90p <- NULL
			if(GeneralParameters$data.type == 'netcdf'){
				if(is.TX10p) ncparsTX10p <- list(trend.vars = trend.varsX, xy.dim = xy.dimX, nx = nxX, ny = nyX,
								nc.unit = "%", nc.prec = "float",
								longname.yrs = "Percentage of days when TX < 10th percentile",
								longname.mon = "Percentage of days when TX < 10th percentile")

				if(is.TX90p) ncparsTX90p <- list(trend.vars = trend.varsX, xy.dim = xy.dimX, nx = nxX, ny = nyX,
								nc.unit = "%", nc.prec = "float",
								longname.yrs = "Percentage of days when TX > 90th percentile",
								longname.mon = "Percentage of days when TX > 90th percentile")

				if(is.TN10p) ncparsTN10p <- list(trend.vars = trend.varsN, xy.dim = xy.dimN, nx = nxN, ny = nyN,
								nc.unit = "%", nc.prec = "float",
								longname.yrs = "Percentage of days when TN < 10th percentile",
								longname.mon = "Percentage of days when TN < 10th percentile")

				if(is.TN90p) ncparsTN90p <- list(trend.vars = trend.varsN, xy.dim = xy.dimN, nx = nxN, ny = nyN,
								nc.unit = "%", nc.prec = "float",
								longname.yrs = "Percentage of days when TN > 90th percentile",
								longname.mon = "Percentage of days when TN > 90th percentile")
			}else{
				if(is.TX10p) ncparsTX10p <- list(head = cdtTmax[c('id', 'lon', 'lat')])
				if(is.TX90p) ncparsTX90p <- list(head = cdtTmax[c('id', 'lon', 'lat')])
				if(is.TN10p) ncparsTN10p <- list(head = cdtTmin[c('id', 'lon', 'lat')])
				if(is.TN90p) ncparsTN90p <- list(head = cdtTmin[c('id', 'lon', 'lat')])
			}

			climdex.Write.Indices(TX10p, "TX10p", GeneralParameters$data.type, indexDIR, nc.pars = ncparsTX10p)
			climdex.Write.Indices(TX90p, "TX90p", GeneralParameters$data.type, indexDIR, nc.pars = ncparsTX90p)
			climdex.Write.Indices(TN10p, "TN10p", GeneralParameters$data.type, indexDIR, nc.pars = ncparsTN10p)
			climdex.Write.Indices(TN90p, "TN90p", GeneralParameters$data.type, indexDIR, nc.pars = ncparsTN90p)
		}
	}else{
		if(jTmaxmin) InsertMessagesTxt(main.txt.out, "Unable to calculate TN10p, TN90p, TX10p, TX90p", format = TRUE)
		if(jTmin) InsertMessagesTxt(main.txt.out, "Unable to calculate TN10p, TN90p", format = TRUE)
		if(jTmax) InsertMessagesTxt(main.txt.out, "Unable to calculate TX10p, TX90p", format = TRUE)
	}

	#############
	# WSDI, Warm spell duration index
	# CSDI, Cold spell duration index
	WSDI <- CSDI <- NULL

	if(!is.null(MONTHQ1090)){
		if(jTmaxmin){
			if(is.WSDI | is.CSDI){
				MONTHQ1090$nbTmax <- nbTmax
				MONTHQ1090$nbTmin <- nbTmin
				sdi <- climdex.WSDI.CSDI(MONTHQ1090)
				if(is.WSDI){
					WSDI <- if(!is.null(sdi$WSDI)) list(yIdx = list(date = sdi$yrs.date, data = sdi$WSDI), tIdx = sdi$trWSDI) else NULL
				}
				if(is.CSDI){
					CSDI <- if(!is.null(sdi$CSDI)) list(yIdx = list(date = sdi$yrs.date, data = sdi$CSDI), tIdx = sdi$trCSDI) else NULL
				}
			}
		}

		if(jTmax & is.WSDI){
				sdi <- climdex.WSDI.CSDI(MONTHQ1090, Indice = 'WSDI')
				WSDI <- if(!is.null(sdi$WSDI)) list(yIdx = list(date = sdi$yrs.date, data = sdi$WSDI), tIdx = sdi$trWSDI) else NULL
		}

		if(jTmin & is.CSDI){
			sdi <- climdex.WSDI.CSDI(MONTHQ1090, Indice = 'CSDI')
			CSDI <- if(!is.null(sdi$CSDI)) list(yIdx = list(date = sdi$yrs.date, data = sdi$CSDI), tIdx = sdi$trCSDI) else NULL
		}

		#############
		ncparsWSDI <- ncparsCSDI <- NULL
		if(GeneralParameters$data.type == 'netcdf'){
			if(is.WSDI) ncparsWSDI <- list(trend.vars = trend.varsX, xy.dim = xy.dimX, nx = nxX, ny = nyX,
								nc.unit = "day", nc.prec = "short",
								longname.yrs = "Warm spell duration index")

			if(is.CSDI) ncparsCSDI <- list(trend.vars = trend.varsN, xy.dim = xy.dimN, nx = nxN, ny = nyN,
								nc.unit = "day", nc.prec = "short",
								longname.yrs = "Cold spell duration index")
		}else{
			if(is.WSDI) ncparsWSDI <- list(head = cdtTmax[c('id', 'lon', 'lat')])
			if(is.CSDI) ncparsCSDI <- list(head = cdtTmin[c('id', 'lon', 'lat')])
		}

		climdex.Write.Indices(WSDI, "WSDI", GeneralParameters$data.type, indexDIR, nc.pars = ncparsWSDI)
		climdex.Write.Indices(CSDI, "CSDI", GeneralParameters$data.type, indexDIR, nc.pars = ncparsCSDI)
	}

	#############
	GSL <- NULL
	DTR <- NULL
	if(jTmaxmin){
		if(is.GSL | is.DTR){
			if(GeneralParameters$data.type == 'cdt'){
				okGSLDTR <- FALSE
				iens <- intersect(cdtTmax$id, cdtTmin$id)
				if(length(iens) > 0){
					imax <- match(iens, cdtTmax$id)
					imin <- match(iens, cdtTmin$id)

					okGSLDTR <- TRUE
					if(is.GSL){
						cdtTmean <- cdtTmax
						cdtTmean$id <- cdtTmax$id[imax]
						cdtTmean$lon <- cdtTmax$lon[imax]
						cdtTmean$lat <- cdtTmax$lat[imax]
						cdtTmean$data <- (cdtTmax$data[, imax, drop = FALSE]+cdtTmin$data[, imin, drop = FALSE])/2
					}
					if(is.DTR){
						cdtDtr <- cdtTmax
						cdtDtr$id <- cdtTmax$id[imax]
						cdtDtr$lon <- cdtTmax$lon[imax]
						cdtDtr$lat <- cdtTmax$lat[imax]
						cdtDtr$data <- cdtTmax$data[, imax, drop = FALSE]-cdtTmin$data[, imin, drop = FALSE]
					}
				}
			}
			if(GeneralParameters$data.type == 'series'){
				okGSLDTR <- TRUE
				if(is.GSL){
					cdtTmean <- cdtTmax
					cdtTmean$data <- (cdtTmax$data+cdtTmin$data)/2
				}
				if(is.DTR){
					cdtDtr <- cdtTmax
					cdtDtr$data <- cdtTmax$data-cdtTmin$data
				}
			}

			if(GeneralParameters$data.type == 'netcdf'){
				xyTmax <- data.frame(x = cdtTmax$lon, y = cdtTmax$lat)
				coordinates(xyTmax) <- ~x+y
				xyTmax <- SpatialPixels(points = xyTmax, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))

				xyTmin <- data.frame(x = cdtTmin$lon, y = cdtTmin$lat)
				coordinates(xyTmin) <- ~x+y
				xyTmin <- SpatialPixels(points = xyTmin, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))

				csztx <- xyTmax@grid@cellsize
				csztn <- xyTmin@grid@cellsize
				res <- pmin(csztx, csztn)

				rxmax <- range(cdtTmax$lon)
				rxmin <- range(cdtTmin$lon)
				rymax <- range(cdtTmax$lat)
				rymin <- range(cdtTmin$lat)

				x <- seq(max(c(rxmax[1], rxmin[1])), min(c(rxmax[2], rxmin[2])), res[1])
				y <- seq(max(c(rymax[1], rymin[1])), min(c(rymax[2], rymin[2])), res[2])

				nx1 <- length(x)
				ny1 <- length(y)
				dx <- ncdim_def("Lon", "degreeE", x)
				dy <- ncdim_def("Lat", "degreeN", y)
				xy.dim1 <- list(dx, dy)
				grd.slp <- ncvar_def("slope", "", xy.dim1, NA, longname = "Slope estimate", prec = "float")
				grd.std.slp <- ncvar_def("std.slope", "", xy.dim1, NA, longname = "Slope error", prec = "float")
				grd.pvalue <- ncvar_def("pvalue", "", xy.dim1, NA, longname = "P-value", prec = "float")
				grd.r2 <- ncvar_def("r2", "", xy.dim1, NA, longname = "Coefficient of determination R2", prec = "float")
				trend.vars <- list(grd.slp, grd.std.slp, grd.pvalue, grd.r2)

				xyGrd <- expand.grid(x = x, y = y)
				coordinates(xyGrd) <- ~x+y
				xyGrd <- SpatialPixels(points = xyGrd, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))

				ijmax <- over(xyGrd, xyTmax)
				ijmin <- over(xyGrd, xyTmin)

				okGSLDTR <- TRUE
				if(is.GSL){
					cdtTmean <- cdtTmax
					cdtTmean$id <- paste("p", seq(length(xyGrd)), sep = "")
					cdtTmean$lon <- as.numeric(xyGrd$x)
					cdtTmean$lat <- as.numeric(xyGrd$y)
					cdtTmean$data <- (cdtTmax$data[, ijmax, drop = FALSE]+cdtTmin$data[, ijmin, drop = FALSE])/2
				}
				if(is.DTR){
					cdtDtr <- cdtTmax
					cdtDtr$id <- paste("p", seq(length(xyGrd)), sep = "")
					cdtDtr$lon <- as.numeric(xyGrd$x)
					cdtDtr$lat <- as.numeric(xyGrd$y)
					cdtDtr$data <- cdtTmax$data[, ijmax, drop = FALSE]-cdtTmin$data[, ijmin, drop = FALSE]
				}
			}
		}

		#############

		# Daily temperature range
		if(is.DTR & okGSLDTR){
			nacount <- climdex.NACount.MonDay(cdtDtr, separe = separe)
			DTR <- climdex.DTR(cdtDtr, nacount, separe)
		}

		# Growing season length
		if(is.GSL & okGSLDTR){
			## threshold 5degC 
			threshold <- 5 
			consec.day <- 6
			GSL <- climdex.GSL(cdtTmean, threshold, consec.day)
		}

		#############
		ncparsGSL <- ncparsDTR <- NULL
		if(GeneralParameters$data.type == 'netcdf'){
			if(is.GSL) ncparsGSL <- list(trend.vars = trend.vars, xy.dim = xy.dim1, nx = nx1, ny = ny1,
								nc.unit = "day", nc.prec = "short",
								longname.yrs = "Growing season length")

			if(is.DTR) ncparsDTR <- list(trend.vars = trend.vars, xy.dim = xy.dim1, nx = nx1, ny = ny1,
								nc.unit = "degC", nc.prec = "float",
								longname.yrs = "Annual mean value of daily temperature range",
								longname.mon = "Monthly mean value of daily temperature range")
		}else{
			if(is.GSL) ncparsGSL <- list(head = cdtTmean[c('id', 'lon', 'lat')])
			if(is.DTR) ncparsDTR <- list(head = cdtDtr[c('id', 'lon', 'lat')])
		}

		climdex.Write.Indices(GSL, "GSL", GeneralParameters$data.type, indexDIR, nc.pars = ncparsGSL)
		climdex.Write.Indices(DTR, "DTR", GeneralParameters$data.type, indexDIR, nc.pars = ncparsDTR)
	}

	return(0)
}



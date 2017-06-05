
climatoAnalysisProcs <- function(GeneralParameters){
	if(GeneralParameters$IO.files$In.dir.file == "" | GeneralParameters$IO.files$In.dir.file == "NA"){
		InsertMessagesTxt(main.txt.out, 'No input data found', format = TRUE)
		return(NULL)
	}

	if(!file.exists(GeneralParameters$IO.files$Out.dir.file)){
		InsertMessagesTxt(main.txt.out, 'Directory to save results not found', format = TRUE)
		InsertMessagesTxt(main.txt.out, paste('The outputs will be put in', getwd()))
		GeneralParameters$IO.files$Out.dir.file <- getwd()
	}

	#############
	outputDIR <- file.path(GeneralParameters$IO.files$Out.dir.file, paste0("ClimatoAnalysis_", GeneralParameters$analysis.method$mth.fun))
	# if(file.exists(outputDIR)){
	# 	msg <- paste0("The directory\n", outputDIR, "\nalready exists. Do you want to overwrite it?")
	# 	retval <- tkmessageBox(message = msg, icon = "question", type = "yesno", default = "yes")
	# 	retval <- substr(tolower(tclvalue(retval)), 1, 1)
	# 	if(retval == 'n'){
	# 		InsertMessagesTxt(main.txt.out, 'Change the directory to place outputs', format = TRUE)
	# 		return(NULL)
	# 	}
	# 	tcl("update")
	# }else dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)
	dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)

	#############

	if(GeneralParameters$data.type == 'cdt'){
		cdtdata <- getStnOpenData(GeneralParameters$IO.files$In.dir.file)
		if(is.null(cdtdata)) return(NULL)
		cdtdata <- getCDTdataAndDisplayMsg(cdtdata, GeneralParameters$in.series)
		if(is.null(cdtdata)) return(NULL)
		cdtdataInfo <- getStnOpenDataInfo(GeneralParameters$IO.files$In.dir.file)
		miss.val <- cdtdataInfo[[3]]$miss.val
		cdtdata <- cdtdata[c('id', 'lon', 'lat', 'dates', 'data')]
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

		if(GeneralParameters$in.series == 'daily'){
			dates <- format(seq(dstart, dend, 'day'), '%Y%m%d')
			ncfiles <- sprintf(GeneralParameters$ncdf.file$format, substr(dates, 1, 4),
								substr(dates, 5, 6), substr(dates, 7, 8))
		}

		if(GeneralParameters$in.series == 'dekadal'){
			dates <- seq(dstart, dend, 'day')
			dates <- paste(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%Y%m'),
						as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%d')), sep = '')
			ncfiles <- sprintf(GeneralParameters$ncdf.file$format, substr(dates, 1, 4),
								substr(dates, 5, 6), as.numeric(substr(dates, 7, 7)))
		}

		if(GeneralParameters$in.series == 'monthly'){
			dates <- format(seq(dstart, dend, 'month'), '%Y%m')
			ncfiles <- sprintf(GeneralParameters$ncdf.file$format, substr(dates, 1, 4), substr(dates, 5, 6))
		}

		filein0 <- file.path(GeneralParameters$IO.files$In.dir.file, ncfiles)

		#############
		if(!GeneralParameters$time.series$all.years){
			if(GeneralParameters$time.series$nseq.years) yeartoAna <- GeneralParameters$time.series$custom.years[[1]]
			else yeartoAna <- GeneralParameters$time.series$start.year:GeneralParameters$time.series$end.year
		}else yeartoAna <- as.numeric(substr(dates, 1, 4))

		if(GeneralParameters$out.series$out.series == "monthly"){
			if(GeneralParameters$time.series$nseq.months) monthtoAna <- GeneralParameters$time.series$custom.months[[1]]
			else monthtoAna <- GeneralParameters$time.series$start.month:GeneralParameters$time.series$end.month
		}else if(GeneralParameters$out.series$out.series == "seasonal"){
			startMonth <- GeneralParameters$out.series$start.seas
			seasonLength <- GeneralParameters$out.series$len.seas
			monthtoAna <- (startMonth:(startMonth+(seasonLength-1)))%%12
			monthtoAna[monthtoAna == 0] <- 12
			monthtoAna <- sort(unique(monthtoAna))
		}else monthtoAna <- 1:12

		#############

		idaty <- as.numeric(substr(dates, 1, 4))%in%yeartoAna & as.numeric(substr(dates, 5, 6))%in%monthtoAna
		dates <- dates[idaty]
		filein0 <- filein0[idaty]

		existFl <- unlist(lapply(filein0, file.exists))
		if(!any(existFl)){
			InsertMessagesTxt(main.txt.out, "Unable to locate NetCDF files", format = TRUE)
			return(NULL)
		}

		dates <- dates[existFl]
		filein0 <- filein0[existFl]

		ncInfo <- getRFESampleData(GeneralParameters$ncdf.file$sample)
		if(is.null(ncInfo)){
			InsertMessagesTxt(main.txt.out, "No daily rainfall sample file found", format = TRUE)
			return(NULL)
		}

		nc <- nc_open(filein0[1])
		lon <- nc$dim[[ncInfo$rfeILon]]$vals
		lat <- nc$dim[[ncInfo$rfeILat]]$vals
		nc_close(nc)

		#############

		infoFiles <- c(GeneralParameters$IO.files$In.dir.file, GeneralParameters$ncdf.file$format)
		if(!is.null(EnvClimatoAnalysis$cdtdata)){
			iexist <- dates%in%EnvClimatoAnalysis$cdtdata$dates
			if(all(iexist)){
				if(!isTRUE(all.equal(EnvClimatoAnalysis$infoFiles, infoFiles))){
					readNCDFdata <- TRUE
					EnvClimatoAnalysis$cdtdata <- NULL
				}else readNCDFdata <- FALSE
			}else{
				if(any(iexist) & isTRUE(all.equal(EnvClimatoAnalysis$infoFiles, infoFiles))){
					dates <- dates[!iexist]
					filein0 <- filein0[!iexist]
				}else EnvClimatoAnalysis$cdtdata <- NULL
				readNCDFdata <- TRUE
			}
		}else readNCDFdata <- TRUE

		if(readNCDFdata){
			InsertMessagesTxt(main.txt.out, 'Read netcdf data ...')
			if(doparallel & length(filein0) >= 180){
				klust <- makeCluster(nb_cores)
				registerDoParallel(klust)
				`%parLoop%` <- `%dopar%`
				closeklust <- TRUE
			}else{
				`%parLoop%` <- `%do%`
				closeklust <- FALSE
			}

			xo <- order(lon)
			lon <- lon[xo]
			yo <- order(lat)
			lat <- lat[yo]

			ncData <- foreach(jj = seq_along(filein0), .packages = "ncdf4",
							.export = c("filein0", "ncInfo")) %parLoop% {
				nc <- nc_open(filein0[jj])
				vars <- ncvar_get(nc, varid = ncInfo$rfeVarid)
				nc_close(nc)
				vars <- vars[xo, yo]
				if(ncInfo$rfeILat < ncInfo$rfeILon){
					vars <- matrix(c(vars), nrow = length(lon), ncol = length(lat), byrow = TRUE)
				}
				vars
			}
			if(closeklust) stopCluster(klust)
			InsertMessagesTxt(main.txt.out, 'Reading netcdf data finished')

			xycrd <- expand.grid(lon, lat)
			cdtdata <- list(
				id = paste("p", seq(nrow(xycrd)), sep = ""),
				lon = xycrd[, 1],
				lat = xycrd[, 2],
				dates = dates,
				data = t(sapply(ncData, c))
			)
			rm(ncData)
			cdtdata$dates <- c(EnvClimatoAnalysis$cdtdata$dates, cdtdata$dates)
			cdtdata$data <- rbind(EnvClimatoAnalysis$cdtdata$data, cdtdata$data)
			odaty <- order(cdtdata$dates)
			cdtdata$dates <- cdtdata$dates[odaty]
			cdtdata$data <- cdtdata$data[odaty, , drop = FALSE]
			EnvClimatoAnalysis$cdtdata <- cdtdata
			EnvClimatoAnalysis$infoFiles <- infoFiles
		}else cdtdata <- EnvClimatoAnalysis$cdtdata
	}

	####################################################

	if(GeneralParameters$in.series == 'monthly'){
		dtmp <- range(as.Date(paste0(cdtdata$dates, '01'), '%Y%m%d'), na.rm = TRUE)
		pastemps <- 'month'
	}else{
		dtmp <- range(as.Date(cdtdata$dates, '%Y%m%d'), na.rm = TRUE)
		pastemps <- 'day'
	}
	cdates <- seq(dtmp[1], dtmp[2], pastemps)
	ystart <- seq(as.Date(paste0(format(cdates[1], '%Y'), '-1-1')), cdates[1], pastemps)
	ystart <- ystart[-length(ystart)]
	yend <- seq(cdates[length(cdates)], as.Date(paste0(format(cdates[length(cdates)], '%Y'), '-12-31')), pastemps)
	yend <- yend[-1]
	if(length(ystart) > 0) cdates <- c(ystart, cdates)
	if(length(yend) > 0) cdates <- c(cdates, yend)
	if(GeneralParameters$in.series == 'daily') cdates <- format(cdates, "%Y%m%d")
	if(GeneralParameters$in.series == 'dekadal'){
		dek <- as.numeric(format(cdates, '%d'))
		cdates <- paste0(format(cdates, "%Y%m")[dek <= 3], dek[dek <= 3])
	}
	if(GeneralParameters$in.series == 'monthly') cdates <- format(cdates, "%Y%m")

	##################

	idrow <- seq(length(cdtdata$dates))
	idrow <- idrow[match(cdates, cdtdata$dates)]

	##################

	if(!GeneralParameters$time.series$all.years){
		if(GeneralParameters$time.series$nseq.years) yeartoAna <- GeneralParameters$time.series$custom.years[[1]]
		else yeartoAna <- GeneralParameters$time.series$start.year:GeneralParameters$time.series$end.year
	}else yeartoAna <- as.numeric(substr(cdtdata$dates, 1, 4))

	if(GeneralParameters$out.series$out.series == "monthly"){
		seasonLength <- 1
		if(GeneralParameters$time.series$nseq.months) monthtoAna <- GeneralParameters$time.series$custom.months[[1]]
		else monthtoAna <- GeneralParameters$time.series$start.month:GeneralParameters$time.series$end.month
	}else if(GeneralParameters$out.series$out.series == "seasonal"){
		startMonth <- GeneralParameters$out.series$start.seas
		seasonLength <- GeneralParameters$out.series$len.seas
		monthtoAna <- (startMonth:(startMonth+(seasonLength-1)))%%12
		monthtoAna[monthtoAna == 0] <- 12
	}else{
		startMonth <- GeneralParameters$time.series$start.month
		endMonth <- GeneralParameters$time.series$end.month
		seasonLength <- 12
		# monthtoAna <- (startMonth:(startMonth+(seasonLength-1)))%%12
		monthtoAna <- ((startMonth:(12+endMonth))[1:12])%%12
		monthtoAna[monthtoAna == 0] <- 12
	}

	##################

	itmp <- as.numeric(substr(cdates, 1, 4))%in%yeartoAna & as.numeric(substr(cdates, 5, 6))%in%monthtoAna
	xrow <- idrow[itmp]
	xdaty <- cdates[itmp]

	##################

	if(GeneralParameters$out.series$out.series == "monthly"){
		indx <- tapply(xrow, substr(xdaty, 1, 6), identity)
		odaty <- names(indx)
		odaty <- paste0(substr(odaty, 1, 4), '-', substr(odaty, 5, 6), '_',
						substr(odaty, 1, 4), '-', substr(odaty, 5, 6))
		len.data <- sapply(indx, length)
	}else if(GeneralParameters$out.series$out.series == "seasonal" & seasonLength == 1){
		indx <- tapply(xrow, substr(xdaty, 1, 6), identity)
		odaty <- names(indx)
		odaty <- paste0(substr(odaty, 1, 4), '-', str_pad(startMonth, width = 2, pad = "0"), '_',
						substr(odaty, 1, 4), '-', str_pad(startMonth, width = 2, pad = "0"))
		len.data <- sapply(indx, length)
	}else{
		xmois <- as.numeric(substr(xdaty, 5, 6))

		rleMois <- rle(xmois)
		xrank <- cumsum(rleMois$lengths)
		istart <- seq(which(rleMois$values %in% monthtoAna[1])[1], length(rleMois$values), seasonLength)
		iend <- istart + seasonLength - 1

		istart <- xrank[istart]-rleMois$lengths[istart]+1

		debSeas <- as.Date(paste0(substr(xdaty[istart], 1, 6), '01'), '%Y%m%d')
		finSeas <- sapply(lapply(debSeas, addMonths, n = seasonLength-1), format, '%Y-%m')

		## end
		nend <- 0
		if(iend[length(iend)] > length(rleMois$lengths)){
			iex <- iend[length(iend)] - length(rleMois$lengths)
			iex <- (rleMois$values[length(rleMois$lengths)] + (1:iex))%%12
			iex[iex == 0] <- 12

			if(GeneralParameters$in.series == 'daily'){
				nend <- mapply(function(an, mon) rev((28:31)[!is.na(as.Date(paste(an, mon, 28:31, sep = '-')))])[1],
										rep(as.numeric(substr(finSeas[length(finSeas)], 1, 4)), length(iex)), iex)
				nend <- sum(nend)
			}
			if(GeneralParameters$in.series == 'dekadal') nend <- sum(length(iex))*3
			if(GeneralParameters$in.series == 'monthly') nend <- sum(length(iex))

			iend[length(iend)] <- length(rleMois$lengths)
		}
		iend <- xrank[iend]

		## index
		indx <- lapply(seq_along(istart), function(j) xrow[istart[j]:iend[j]])

		odaty <- paste0(format(debSeas, '%Y-%m'), '_', finSeas)
		len.data <- sapply(indx, length)
		len.data[length(len.data)] <- len.data[length(len.data)] + nend
	}

	################################################

	nbstn <- ncol(cdtdata$data)
	transPose <- if(nbstn > 1) t else as.matrix
	len.MAT <- matrix(len.data, nrow = length(len.data), ncol = nbstn)


	## moved to cdtAggrTSMatClimato_functions.R
	### NA count
	# funMissMAT <- function(x, DATA){
	# 	MAT <- is.na(DATA[x, , drop = FALSE])
	# 	colSums(MAT)
	# }

	### Aggregation
	# funAggrMAT <- function(x, DATA){
	# 	x <- x[!is.na(x)]
	# 	if(length(x) == 0) return(rep(NA, ncol(DATA)))
	# 	MAT <- DATA[x, , drop = FALSE]
	# 	funAggr <- GeneralParameters$aggr.series$aggr.fun
	# 	if(funAggr == "mean") res <- colMeans(MAT, na.rm = TRUE)
	# 	if(funAggr == "sum") res <- colSums(MAT, na.rm = TRUE)
	# 	if(funAggr == "count"){
	# 		opr.fun <- match.fun(GeneralParameters$aggr.series$opr.fun)
	# 		opr.thres <- GeneralParameters$aggr.series$opr.thres
	# 		MAT <- opr.fun(MAT, opr.thres) & !is.na(MAT)
	# 		res <- colSums(MAT, na.rm = TRUE)
	# 	}
	# 	return(res)
	# }

	# funAggrMAT <- function(x, DATA, pars){
	# 	x <- x[!is.na(x)]
	# 	if(length(x) == 0) return(rep(NA, ncol(DATA)))
	# 	MAT <- DATA[x, , drop = FALSE]
	# 	if(pars$aggr.fun == "mean") res <- colMeans(MAT, na.rm = TRUE)
	# 	if(pars$aggr.fun == "sum") res <- colSums(MAT, na.rm = TRUE)
	# 	if(pars$aggr.fun == "count"){
	# 		count.fun <- match.fun(pars$count.fun)
	# 		MAT <- count.fun(MAT, pars$count.thres) & !is.na(MAT)
	# 		res <- colSums(MAT, na.rm = TRUE)
	# 	}
	# 	return(res)
	# }

	#############

	toAggr <- list(indx, GeneralParameters$aggr.series)
	if(is.null(EnvClimatoAnalysis$toAggr)){
		aggregatData <- TRUE
	}else{
		aggregatData <- if(!isTRUE(all.equal(EnvClimatoAnalysis$toAggr, toAggr))) TRUE else FALSE
	}

	if(aggregatData){
		AggrSeries <- list(aggr.fun = GeneralParameters$aggr.series$aggr.fun,
							count.fun = GeneralParameters$aggr.series$opr.fun,
							count.thres = GeneralParameters$aggr.series$opr.thres)

		missData <- transPose(sapply(indx, funMissMAT, DATA = cdtdata$data))
		AggrData <- transPose(sapply(indx, funAggrMAT, DATA = cdtdata$data, pars = AggrSeries))
		AggrData[(1-(missData/len.MAT)) < GeneralParameters$aggr.series$min.frac] <- NA
		EnvClimatoAnalysis$AggrData <- AggrData
		EnvClimatoAnalysis$toAggr <- toAggr
	}else AggrData <- EnvClimatoAnalysis$AggrData

	#############

	### Analysis
	Trend.lsfit <- function(y, x, minyear){
		ina <- !is.na(y)
		if(length(which(ina)) < minyear) return(c(Slope = NA, STD.of.Slope = NA, P.Value = NA, R2 = NA, Intercept = NA))
		nx <- x[ina]
		ny <- y[ina]
		nyear <- nx[length(nx)]-nx[1]+1
		trFit <- ls.print(lsfit(nx, ny), print.it = FALSE)
		beta <- round(as.numeric(trFit$coef.table[[1]][2, 1]), 3)
		intr <- round(as.numeric(trFit$coef.table[[1]][1, 1]), 3)
		std <- round(as.numeric(trFit$coef.table[[1]][2, 2]), 3)
		pval <- round(as.numeric(trFit$summary[1, 6]), 3)
		r2 <- round(as.numeric(trFit$summary[1, 2]), 3)
		pval[is.nan(pval)] <- NA
		# signif <- if(!is.na(pval)) if(pval < 0.05) 1 else 0 else NA
		c(Slope = beta, STD.of.Slope = std, P.Value = pval, R2 = r2, Intercept = intr)
	}

	funAnalysisMAT <- function(x, DATA){
		x <- x[!is.na(x)]

		funAnalysis <- GeneralParameters$analysis.method$mth.fun
		if(length(x) == 0){
			if(funAnalysis == "trend") return(matrix(NA, nrow = 5, ncol = ncol(DATA)))
			else return(rep(NA, ncol(DATA)))
		}
		MAT <- DATA[x, , drop = FALSE]
		if(funAnalysis == "average") res <- base::colMeans(MAT, na.rm = TRUE)
		if(funAnalysis == "median") res <- matrixStats::colMedians(MAT, na.rm = TRUE)
		if(funAnalysis == "std") res <- matrixStats::colSds(MAT, na.rm = TRUE)
		if(funAnalysis == "cv") res <- 100*matrixStats::colSds(MAT, na.rm = TRUE)/base::colMeans(MAT, na.rm = TRUE)
		if(funAnalysis == "trend") res <- apply(MAT, 2, Trend.lsfit, x = yyear, minyear = GeneralParameters$Trend.min.year)
		if(funAnalysis == "percentile"){
			probs <- GeneralParameters$analysis.method$mth.perc/100
			res <- matrixStats::colQuantiles(MAT, probs = probs, na.rm = TRUE)
		}
		if(funAnalysis == "frequency"){
			low.thres <- GeneralParameters$analysis.method$low.thres
			up.thres <- GeneralParameters$analysis.method$up.thres
			nyear <- base::colSums(!is.na(MAT))
			MAT <- MAT >= low.thres & MAT <= up.thres & !is.na(MAT)
			res <- 10*base::colSums(MAT, na.rm = TRUE)/nyear
		}
		res[is.nan(res) | is.infinite(res)] <- NA
		return(res)
	}

	#############

	if(GeneralParameters$out.series$out.series == "monthly"){
		# ixm <- substr(dimnames(AggrData)[[1]], 5, 6)
		ixm <- substr(odaty, 6, 7)
		ixm <- tapply(seq(length(odaty)), ixm, identity)
		# yyear <- lapply(ixm, function(x) as.numeric(substr(odaty[x], 1, 4)))
		yyear <- as.numeric(substr(odaty[ixm[[1]]], 1, 4))
		AggrNA <- lapply(ixm, function(x){
			MAT <- AggrData[x, , drop = FALSE]
			matrix(1-(base::colSums(is.na(MAT))/nrow(MAT)), nrow = 1)
		})
		odaty <- lapply(ixm, function(j) odaty[j])
	}else{
		ixm <- list(seq(nrow(AggrData)))
		yyear <- as.numeric(substr(odaty, 1, 4))
		AggrNA <- list(matrix(1-(base::colSums(is.na(AggrData))/nrow(AggrData)), nrow = 1))
		odaty <- list(odaty)
	}

	funAnalysis <- GeneralParameters$analysis.method$mth.fun
	AnalysData <- if(funAnalysis == "trend") lapply(ixm, funAnalysisMAT, DATA = AggrData) else transPose(sapply(ixm, funAnalysisMAT, DATA = AggrData))

	################################################
	outTSDIR <- file.path(outputDIR, "TimeSeries")
	outAnaDIR <- file.path(outputDIR, "Analysis")
	dir.create(outTSDIR, showWarnings = FALSE, recursive = TRUE)
	dir.create(outAnaDIR, showWarnings = FALSE, recursive = TRUE)

	outAna <- NULL

	if(GeneralParameters$data.type == 'netcdf'){
		lon <- sort(unique(cdtdata$lon))
		lat <- sort(unique(cdtdata$lat))
		nx <- length(lon)
		ny <- length(lat)
		dx <- ncdim_def("Lon", "degreeE", lon)
		dy <- ncdim_def("Lat", "degreeN", lat)
		xy.dim <- list(dx, dy)
		grdNA <- ncvar_def("nonNA", "", xy.dim, NA, longname = "Fraction of the available data", prec = "float")
		grdTS <- ncvar_def("ts", "", xy.dim, NA, longname = "Time series", prec = "float")
		outAna$coords <- list(lon = lon, lat = lat, nlon = nx, nlat = ny)

		if(funAnalysis == "trend"){
			grd.slp <- ncvar_def("slope", "", xy.dim, NA, longname = "Slope estimate", prec = "float")
			grd.std.slp <- ncvar_def("std.slope", "", xy.dim, NA, longname = "Slope error", prec = "float")
			grd.pvalue <- ncvar_def("pvalue", "", xy.dim, NA, longname = "P-value", prec = "float")
			grd.r2 <- ncvar_def("r2", "", xy.dim, NA, longname = "Coefficient of determination R2", prec = "float")
			grd.interc <- ncvar_def("intercept", "", xy.dim, NA, longname = "Intercept", prec = "float")
			out.vars <- list(grd.slp, grd.std.slp, grd.pvalue, grd.r2, grd.interc, grdNA)
		}else{
			if(funAnalysis == "average"){
				nc.var <- "avrg"
				longname.mon <- "Average"
			}
			if(funAnalysis == "median"){
				nc.var <- "med"
				longname.mon <- "Median"
			}
			if(funAnalysis == "std"){
				nc.var <- "std"
				longname.mon <- "Standard deviation"
			}
			if(funAnalysis == "cv"){
				nc.var <- "cv"
				longname.mon <- "Coefficient of variation"
			}
			if(funAnalysis == "percentile"){
				nc.var <- "perc"
				longname.mon <- paste0(GeneralParameters$analysis.method$mth.perc, "th Percentile")
			}
			if(funAnalysis == "frequency"){
				nc.var <- "ferq"
				longname.mon <- "Frequency, number of event every 10 years"
			}
			grdOut <- ncvar_def(nc.var, "", xy.dim, NA, longname = longname.mon, prec = "float")
			out.vars <- list(grdOut, grdNA)
		}

		## Analysis
		for(jj in seq_along(ixm)){
			yrsAna <- paste0(range(yeartoAna), collapse = "-")
			if(seasonLength <= 12){
				year1 <- substr(odaty[[jj]][1], 1, 4) 
				mon1 <- substr(odaty[[jj]][1], 6, 7)
				year2 <- substr(odaty[[jj]][1], 9, 12)
				mon2 <- substr(odaty[[jj]][1], 14, 15)
				if(year1 == year2){
					if(mon1 != mon2){
						dateAna <- if(mon1 == "01" & mon2 == "12") yrsAna else paste0(yrsAna, "_", paste0(mon1, "-", mon2))
					}else dateAna <- paste0(yrsAna, "_", mon1)
				}else dateAna <- paste0(yrsAna, "_", paste0(mon1, "-", mon2))
			}else dateAna <- paste0(yrsAna, "_", seasonLength)

			outfile <- file.path(outAnaDIR, paste0(funAnalysis, "_", dateAna, ".nc"))
			nc <- nc_create(outfile, out.vars)
			if(funAnalysis == "trend"){
				ncvar_put(nc, out.vars[[1]], matrix(as.numeric(AnalysData[[jj]][1, ]), ncol = ny, nrow = nx))
				ncvar_put(nc, out.vars[[2]], matrix(as.numeric(AnalysData[[jj]][2, ]), ncol = ny, nrow = nx))
				ncvar_put(nc, out.vars[[3]], matrix(as.numeric(AnalysData[[jj]][3, ]), ncol = ny, nrow = nx))
				ncvar_put(nc, out.vars[[4]], matrix(as.numeric(AnalysData[[jj]][4, ]), ncol = ny, nrow = nx))
				ncvar_put(nc, out.vars[[5]], matrix(as.numeric(AnalysData[[jj]][5, ]), ncol = ny, nrow = nx))
				ncvar_put(nc, out.vars[[6]], matrix(as.numeric(AggrNA[[jj]]), ncol = ny, nrow = nx))
			}else{
				ncvar_put(nc, out.vars[[1]], matrix(as.numeric(AnalysData[jj, ]), ncol = ny, nrow = nx))
				ncvar_put(nc, out.vars[[2]], matrix(as.numeric(AggrNA[[jj]]), ncol = ny, nrow = nx))
			}
			nc_close(nc)
		}

		## Time series
		for(jj in seq_along(ixm)){
			aggrdatTS <- AggrData[ixm[[jj]], , drop = FALSE]
			tsdaty <- odaty[[jj]]
			for(ii in seq_along(tsdaty)){
				year1 <- substr(tsdaty[ii], 1, 4) 
				mon1 <- substr(tsdaty[ii], 6, 7)
				year2 <- substr(tsdaty[ii], 9, 12)
				mon2 <- substr(tsdaty[ii], 14, 15)
				if(year1 == year2){
					if(mon1 == mon2) dateTS <- paste0(year1, mon1)
					else{
						dateTS <- if(mon1 == "01" & mon2 == "12") year1 else tsdaty[ii]
					}
				}else dateTS <- tsdaty[ii]

				outfile <- file.path(outTSDIR, paste0("outTS", "_", dateTS, ".nc"))
				nc <- nc_create(outfile, grdTS)
				ncvar_put(nc, grdTS, matrix(as.numeric(aggrdatTS[ii, ]), ncol = ny, nrow = nx))
				nc_close(nc)
			}
		}
	}else{
		xhead <- rbind(cdtdata$id, cdtdata$lon, cdtdata$lat)
		headInfo <- cbind(c('ID.STN', 'LON', 'DATE/LAT'), xhead)
		infohead <- cbind(c('ID.STN', 'LON', 'VARS/LAT'), xhead)
		outAna$coords <- list(id = cdtdata$id, lon = cdtdata$lon, lat = cdtdata$lat)

		## Analysis
		for(jj in seq_along(ixm)){
			yrsAna <- paste0(range(yeartoAna), collapse = "-")
			if(seasonLength <= 12){
				year1 <- substr(odaty[[jj]][1], 1, 4) 
				mon1 <- substr(odaty[[jj]][1], 6, 7)
				year2 <- substr(odaty[[jj]][1], 9, 12)
				mon2 <- substr(odaty[[jj]][1], 14, 15)
				if(year1 == year2){
					if(mon1 != mon2){
						dateAna <- if(mon1 == "01" & mon2 == "12") yrsAna else paste0(yrsAna, "_", paste0(mon1, "-", mon2))
					}else dateAna <- paste0(yrsAna, "_", mon1)
				}else dateAna <- paste0(yrsAna, "_", paste0(mon1, "-", mon2))
			}else dateAna <- paste0(yrsAna, "_", seasonLength)

			outfile <- file.path(outAnaDIR, paste0(funAnalysis, "_", dateAna, ".csv"))
			nonmiss <- cbind("Non-missing-data", round(AggrNA[[jj]], 3))

			if(funAnalysis == "trend") outdata <- cbind(rownames(AnalysData[[jj]]), AnalysData[[jj]])
			else outdata <- cbind(paste0(funAnalysis, "_", dateAna), round(AnalysData[jj, , drop = FALSE], 3))

			outdata <- rbind(infohead, outdata, nonmiss)
			writeFiles(outdata, outfile)
		}

		## Time series
		for(jj in seq_along(ixm)){
			yrsAna <- paste0(range(yeartoAna), collapse = "-")
			year1 <- substr(odaty[[jj]], 1, 4) 
			mon1 <- substr(odaty[[jj]], 6, 7)
			year2 <- substr(odaty[[jj]], 9, 12)
			mon2 <- substr(odaty[[jj]], 14, 15)
			if(all(year1 == year2)){
				if(all(mon1 == mon2)){
					dateTS <- paste0(year1, mon1)
					outDFile <- paste0(yrsAna, "_", mon1[1])
				}else{
					if(all(mon1 == "01") & all(mon2 == "12")){
						dateTS <- year1
						outDFile <- yrsAna
					}else{
						dateTS <- odaty[[jj]]
						outDFile <- paste0(yrsAna, "_", paste0(mon1[1], "-", mon2[1]))
					}
				}
			}else{
				dateTS <- odaty[[jj]]
				outDFile <- paste0(yrsAna, "_", paste0(mon1[1], "-", mon2[1]))
			}
			outDFile <- if(seasonLength <= 12) outDFile else paste0(yrsAna, "_", seasonLength)

			outfile <- file.path(outTSDIR, paste0("outTS", "_", outDFile, ".csv"))
			outdata <- cbind(dateTS, round(AggrData[ixm[[jj]], , drop = FALSE], 3))
			outdata <- rbind(headInfo, outdata)
			writeFiles(outdata, outfile)
		}
	}

	outAna$analysis <- funAnalysis
	outAna$datatype <- GeneralParameters$data.type
	outAna$out.series <- GeneralParameters$out.series$out.series
	outAna$donTS <- AggrData
	outAna$donAna <- AnalysData
	outAna$naAna <- AggrNA
	outAna$dateTS <- odaty
	outAna$indxTS <- ixm
	save(outAna, file = file.path(outputDIR, "Out.Analysis.RData"))
	return(0)
}



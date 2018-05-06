
spatialAnalysisProcs <- function(GeneralParameters){
	if(GeneralParameters$in.file%in%c("", "NA")){
		InsertMessagesTxt(main.txt.out, 'No input data found', format = TRUE)
		return(NULL)
	}

	if(!dir.exists(GeneralParameters$out.dir)){
		InsertMessagesTxt(main.txt.out, 'Directory to save results not found', format = TRUE)
		InsertMessagesTxt(main.txt.out, paste('The outputs will be put in', getwd()))
		GeneralParameters$out.dir <- getwd()
	}

	#############
	outputDIR <- file.path(GeneralParameters$out.dir, paste0("SpatialAnalysis_",
							GeneralParameters$time.series$out.series, "_",
							getf.no.ext(basename(GeneralParameters$in.file))))
	dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)

	#############

	if(GeneralParameters$data.type == 'cdtstation'){
		cdtdata <- getStnOpenData(GeneralParameters$in.file)
		if(is.null(cdtdata)) return(NULL)
		cdtdata <- getCDTdataAndDisplayMsg(cdtdata, GeneralParameters$in.tstep)
		if(is.null(cdtdata)) return(NULL)
		cdtdataInfo <- getStnOpenDataInfo(GeneralParameters$in.file)
		miss.val <- cdtdataInfo[[3]]$miss.val
		cdtdata <- cdtdata[c('id', 'lon', 'lat', 'dates', 'data')]

		daty <- cdtdata$dates
		idrow <- seq(length(daty))
	}

	if(GeneralParameters$data.type == 'cdtdataset'){
		cdtdata <- try(readRDS(GeneralParameters$in.file), silent = TRUE)
		if(inherits(cdtdata, "try-error")){
			InsertMessagesTxt(main.txt.out, paste("Unable to read", GeneralParameters$in.file), format = TRUE)
			return(NULL)
		}
		if(GeneralParameters$in.tstep != cdtdata$TimeStep){
			InsertMessagesTxt(main.txt.out, paste("The dataset is not a", GeneralParameters$in.tstep, "data"), format = TRUE)
			return(NULL)
		}

		daty <- cdtdata$dateInfo$date
		idrow <- cdtdata$dateInfo$index
	}

	####################################################

	if(GeneralParameters$in.tstep == 'monthly'){
		dtmp <- range(as.Date(paste0(daty, '01'), '%Y%m%d'), na.rm = TRUE)
		pastemps <- 'month'
	}else{
		dtmp <- range(as.Date(daty, '%Y%m%d'), na.rm = TRUE)
		pastemps <- 'day'
	}

	cdates <- seq(dtmp[1], dtmp[2], pastemps)
	ystart <- seq(as.Date(paste0(format(cdates[1], '%Y'), '-1-1')), cdates[1], pastemps)
	ystart <- ystart[-length(ystart)]
	yend <- seq(cdates[length(cdates)], as.Date(paste0(format(cdates[length(cdates)], '%Y'), '-12-31')), pastemps)
	yend <- yend[-1]
	if(length(ystart) > 0) cdates <- c(ystart, cdates)
	if(length(yend) > 0) cdates <- c(cdates, yend)
	if(GeneralParameters$in.tstep == 'daily') cdates <- format(cdates, "%Y%m%d")
	if(GeneralParameters$in.tstep == 'pentad'){
		pen <- as.numeric(format(cdates, '%d'))
		cdates <- paste0(format(cdates, "%Y%m")[pen <= 6], pen[pen <= 6])
	}
	if(GeneralParameters$in.tstep == 'dekadal'){
		dek <- as.numeric(format(cdates, '%d'))
		cdates <- paste0(format(cdates, "%Y%m")[dek <= 3], dek[dek <= 3])
	}
	if(GeneralParameters$in.tstep == 'monthly') cdates <- format(cdates, "%Y%m")

	idrow <- idrow[match(cdates, daty)]

	##################

	if(!GeneralParameters$time.series$all.years){
		if(GeneralParameters$time.series$nseq.years) yeartoAna <- GeneralParameters$time.series$custom.years[[1]]
		else yeartoAna <- GeneralParameters$time.series$start.year:GeneralParameters$time.series$end.year
	}else yeartoAna <- as.numeric(substr(daty, 1, 4))

	startMonth <- GeneralParameters$time.series$start.month
	endMonth <- GeneralParameters$time.series$end.month
	seasonLength <- (endMonth-startMonth+1)%%12
	seasonLength[seasonLength == 0] <- 12
	monthtoAna <- (startMonth:(startMonth+(seasonLength-1)))%%12
	monthtoAna[monthtoAna == 0] <- 12

	if(GeneralParameters$time.series$out.series == "monthly"){
		if(GeneralParameters$time.series$nseq.months)
			monthtoAna <- GeneralParameters$time.series$custom.months[[1]]
		seasonLength <- 1
	}

	##################

	itmp <- as.numeric(substr(cdates, 1, 4))%in%yeartoAna & as.numeric(substr(cdates, 5, 6))%in%monthtoAna
	xrow <- idrow[itmp]
	xdaty <- cdates[itmp]

	##################

	if(GeneralParameters$time.series$out.series == "monthly"){
		indx <- tapply(xrow, substr(xdaty, 1, 6), identity)
		odaty <- names(indx)
		odaty <- paste0(substr(odaty, 1, 4), '-', substr(odaty, 5, 6), '_',
						substr(odaty, 1, 4), '-', substr(odaty, 5, 6))
		len.data <- sapply(indx, length)
	}else if(GeneralParameters$time.series$out.series == "seasonal" & seasonLength == 1){
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

			if(GeneralParameters$in.tstep == 'daily'){
				nend <- mapply(function(an, mon) rev((28:31)[!is.na(as.Date(paste(an, mon, 28:31, sep = '-')))])[1],
										rep(as.numeric(substr(finSeas[length(finSeas)], 1, 4)), length(iex)), iex)
				nend <- sum(nend)
			}
			if(GeneralParameters$in.tstep == 'dekadal') nend <- sum(length(iex))*3
			if(GeneralParameters$in.tstep == 'monthly') nend <- sum(length(iex))

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

	toAggr <- list(indx, GeneralParameters$aggr.series)
	if(is.null(EnvSpatialAnalysis$toAggr)){
		aggregatData <- TRUE
	}else{
		aggregatData <- if(!isTRUE(all.equal(EnvSpatialAnalysis$toAggr, toAggr))) TRUE else FALSE
	}

	if(aggregatData){
		InsertMessagesTxt(main.txt.out, "Aggregate data......")
		AggrSeries <- list(aggr.fun = GeneralParameters$aggr.series$aggr.fun,
							count.fun = GeneralParameters$aggr.series$opr.fun,
							count.thres = GeneralParameters$aggr.series$opr.thres)

		####
		if(GeneralParameters$data.type == 'cdtstation'){
			nbstn <- ncol(cdtdata$data)
			transPose <- if(nbstn > 1) t else as.matrix
			len.MAT <- matrix(len.data, nrow = length(len.data), ncol = nbstn)

			missData <- transPose(sapply(indx, funMissMAT, DATA = cdtdata$data))
			AggrData <- transPose(sapply(indx, funAggrMAT, DATA = cdtdata$data, pars = AggrSeries))
			AggrData[(1-(missData/len.MAT)) < GeneralParameters$aggr.series$min.frac] <- NA
			rm(missData)
		}

		if(GeneralParameters$data.type == 'cdtdataset'){
			outDatasetDIR <- file.path(outputDIR, paste0("Aggregated_",
									getf.no.ext(basename(GeneralParameters$in.file))))
			outChunkDIR <- file.path(outDatasetDIR, "DATA")
			dir.create(outChunkDIR, showWarnings = FALSE, recursive = TRUE)

			chunkfile <- sort(unique(cdtdata$colInfo$index))
			chunkcalc <- split(chunkfile, ceiling(chunkfile/cdtdata$chunkfac))

			do.parChunk <- if(cdtdata$chunkfac > length(chunkcalc)) TRUE else FALSE
			do.parCALC <- if(do.parChunk) FALSE else TRUE
			toExports <- c("readCdtDatasetChunk.sequence", "writeCdtDatasetChunk.sequence",
							"funMissMAT", "funAggrMAT", "doparallel")

			is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 5))
			`%parLoop%` <- is.parallel$dofun
			ret <- foreach(ll = seq_along(chunkcalc), .export = toExports, .packages = "doParallel") %parLoop% {
				don <- readCdtDatasetChunk.sequence(chunkcalc[[ll]], GeneralParameters$in.file, cdtdata, do.par = do.parChunk)
				missData <- t(sapply(indx, funMissMAT, DATA = don))
				len.MAT <- matrix(len.data, nrow = length(len.data), ncol = ncol(missData))
				AggrData <- t(sapply(indx, funAggrMAT, DATA = don, pars = AggrSeries))
				AggrData[(1-(missData/len.MAT)) < GeneralParameters$aggr.series$min.frac] <- NA
				
				writeCdtDatasetChunk.sequence(AggrData, chunkcalc[[ll]], cdtdata, outChunkDIR, do.par = do.parChunk)
				rm(missData, AggrData, len.MAT)
				return(0)
			}
			if(is.parallel$stop) stopCluster(is.parallel$cluster)

			#### write index data
			AggrData <- cdtdata

			AggrData$TimeStep <- GeneralParameters$time.series$out.series
			AggrData$dateInfo$date <- odaty
			AggrData$dateInfo$index <- seq_along(odaty)

			datafileIdx <- file.path(outDatasetDIR, basename(GeneralParameters$in.file))
			con <- gzfile(datafileIdx, compression = 5)
			open(con, "wb")
			saveRDS(AggrData, con)
			close(con)
			AggrData$file <- datafileIdx
		}

		EnvSpatialAnalysis$AggrData <- AggrData
		EnvSpatialAnalysis$toAggr <- toAggr
		InsertMessagesTxt(main.txt.out, "Aggregation done!")
	}else AggrData <- EnvSpatialAnalysis$AggrData

	#############

	funAnalysisMAT <- function(x, DATA, fonct,
							  trend = list(year = NA, min.year = 10, unit = 1),
							  percentile = 90,
							  freq.thres = list(low = NA, up = NA))
	{
		x <- x[!is.na(x)]

		if(length(x) == 0){
			if(fonct == "trend") return(matrix(NA, nrow = 5, ncol = ncol(DATA)))
			else return(rep(NA, ncol(DATA)))
		}
		MAT <- DATA[x, , drop = FALSE]
		if(fonct == "mean") res <- base::colMeans(MAT, na.rm = TRUE)
		if(fonct == "median") res <- matrixStats::colMedians(MAT, na.rm = TRUE)
		if(fonct == "std") res <- matrixStats::colSds(MAT, na.rm = TRUE)
		if(fonct == "cv") res <- 100*matrixStats::colSds(MAT, na.rm = TRUE)/base::colMeans(MAT, na.rm = TRUE)
		if(fonct == "trend"){
			res <- regression.Vector(trend$year, MAT, trend$min.year)
			if(trend$unit == 2) res[1, ] <- as.numeric(res[1, ])*(diff(range(trend$year, na.rm = TRUE))+1)
			if(trend$unit == 3) res[1, ] <- 100*as.numeric(res[1, ])*(diff(range(trend$year, na.rm = TRUE))+1)/colMeans(MAT, na.rm = TRUE)
			res <- round(res[c(1, 2, 4, 9), , drop = FALSE], 3)
		}
		if(fonct == "percentile"){
			probs <- percentile/100
			res <- matrixStats::colQuantiles(MAT, probs = probs, na.rm = TRUE, type = 8)
		}
		if(fonct == "frequency"){
			# nyear <- base::colSums(!is.na(MAT))
			MAT <- MAT >= freq.thres$low & MAT <= freq.thres$up & !is.na(MAT)
			res <- base::colSums(MAT, na.rm = TRUE)
			## par decenie
			# res <- 10 * base::colSums(MAT, na.rm = TRUE)/nyear
		}
		res[is.nan(res) | is.infinite(res)] <- NA
		return(res)
	}

	################################################

	if(GeneralParameters$time.series$out.series == "monthly"){
		ixm <- substr(odaty, 6, 7)
		ixm <- tapply(seq(length(odaty)), ixm, identity)
		yyear <- as.numeric(substr(odaty[ixm[[1]]], 1, 4))
		odaty <- lapply(ixm, function(j) odaty[j])
	}else{
		ixm <- if(GeneralParameters$data.type == 'cdtstation') nrow(AggrData) else length(AggrData$dateInfo$index)
		ixm <- list(seq(ixm))
		yyear <- as.numeric(substr(odaty, 1, 4))
		odaty <- list(odaty)
	}

	####
	funAnalysis <- GeneralParameters$analysis.method$mth.fun

	####
	if(GeneralParameters$data.type == 'cdtstation'){
		nbstn <- ncol(cdtdata$data)
		transPose <- if(nbstn > 1) t else as.matrix

		if(funAnalysis != "anomaly"){
			if(GeneralParameters$time.series$out.series == "monthly"){
				AggrNA <- lapply(ixm, function(x){
					MAT <- AggrData[x, , drop = FALSE]
					matrix(1-(base::colSums(is.na(MAT))/nrow(MAT)), nrow = 1)
				})
			}else{
				AggrNA <- list(matrix(1-(base::colSums(is.na(AggrData))/nrow(AggrData)), nrow = 1))
			}
		}

		if(funAnalysis == "anomaly"){
			iyr <- yyear >= GeneralParameters$analysis.method$startYr.anom & yyear <= GeneralParameters$analysis.method$endYr.anom
			ixm1 <- lapply(ixm, function(ix) ix[iyr])
			climatoMean <- lapply(ixm1, funAnalysisMAT, DATA = AggrData, fonct = 'mean')
			AnalysData <- lapply(seq_along(ixm), function(jj){
				don <- AggrData[ixm[[jj]], , drop = FALSE]
				clim <- climatoMean[[jj]]
				anom <- sweep(don, 2, clim, FUN = "-")
				if(GeneralParameters$analysis.method$perc.anom) anom <- 100*sweep(anom, 2, clim+0.001, FUN = "/")
				anom
			})
			AnalysData <- do.call(rbind, AnalysData)
			AnalysData <- AnalysData[order(unlist(ixm)), , drop = FALSE]
		}else if(funAnalysis == "trend"){
			AnalysData <- lapply(ixm, funAnalysisMAT, DATA = AggrData, fonct = 'trend',
									trend = list(year = yyear,
									min.year = GeneralParameters$analysis.method$trend.min.year,
									unit = GeneralParameters$analysis.method$trend.unit))
		}else{
			AnalysData <- transPose(sapply(ixm, funAnalysisMAT, DATA = AggrData, fonct = funAnalysis,
									percentile = GeneralParameters$analysis.method$mth.perc,
									freq.thres = list(low = GeneralParameters$analysis.method$low.thres,
													  up = GeneralParameters$analysis.method$up.thres)))
		}
	}

	######
	if(GeneralParameters$data.type == 'cdtdataset'){
		if(funAnalysis == "anomaly"){
			outDatasetDIR <- file.path(outputDIR, paste0("Aggregated_",
									getf.no.ext(basename(GeneralParameters$in.file))))
			outChunkAnom <- file.path(outDatasetDIR, "TMP_ANOMALY")
			dir.create(outChunkAnom, showWarnings = FALSE, recursive = TRUE)
		}

		chunkfile <- sort(unique(AggrData$colInfo$index))
		chunkcalc <- split(chunkfile, ceiling(chunkfile/cdtdata$chunkfac))

		do.parChunk <- if(cdtdata$chunkfac > length(chunkcalc)) TRUE else FALSE
		do.parCALC <- if(do.parChunk) FALSE else TRUE
		toExports <- c("readCdtDatasetChunk.sequence", "writeCdtDatasetChunk.sequence",
						"regression.Vector", "doparallel")

		is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 5))
		`%parLoop%` <- is.parallel$dofun
		AnalysData <- foreach(ll = seq_along(chunkcalc), .export = toExports, .packages = "doParallel") %parLoop% {
			don <- readCdtDatasetChunk.sequence(chunkcalc[[ll]], AggrData$file, AggrData, do.par = do.parChunk)

			if(funAnalysis != "anomaly"){
				if(GeneralParameters$time.series$out.series == "monthly"){
					AggrNA <- lapply(ixm, function(x){
						MAT <- don[x, , drop = FALSE]
						matrix(1-(base::colSums(is.na(MAT))/nrow(MAT)), nrow = 1)
					})
				}else{
					AggrNA <- list(matrix(1-(base::colSums(is.na(don))/nrow(don)), nrow = 1))
				}
			}

			if(funAnalysis == "anomaly"){
				iyr <- yyear >= GeneralParameters$analysis.method$startYr.anom & yyear <= GeneralParameters$analysis.method$endYr.anom
				ixm1 <- lapply(ixm, function(ix) ix[iyr])
				climatoMean <- lapply(ixm1, funAnalysisMAT, DATA = don, fonct = 'mean')
				AnalysData <- lapply(seq_along(ixm), function(jj){
					don <- don[ixm[[jj]], , drop = FALSE]
					clim <- climatoMean[[jj]]
					anom <- sweep(don, 2, clim, FUN = "-")
					if(GeneralParameters$analysis.method$perc.anom) anom <- 100*sweep(anom, 2, clim+0.001, FUN = "/")
					anom
				})
				AnalysData <- do.call(rbind, AnalysData)
				AnalysData <- AnalysData[order(unlist(ixm)), , drop = FALSE]
				writeCdtDatasetChunk.sequence(AnalysData, chunkcalc[[ll]], AggrData, outChunkAnom, do.par = do.parChunk)
				return(NULL)
			}else if(funAnalysis == "trend"){
				AnalysData <- lapply(ixm, funAnalysisMAT, DATA = don, fonct = 'trend',
										trend = list(year = yyear,
										min.year = GeneralParameters$analysis.method$trend.min.year,
										unit = GeneralParameters$analysis.method$trend.unit))
				return(list(Data = AnalysData, NonMiss = AggrNA))
			}else{
				AnalysData <- lapply(ixm, funAnalysisMAT, DATA = don, fonct = funAnalysis,
										percentile = GeneralParameters$analysis.method$mth.perc,
										freq.thres = list(low = GeneralParameters$analysis.method$low.thres,
														  up = GeneralParameters$analysis.method$up.thres))
				AnalysData <- do.call(rbind, AnalysData)
				return(list(Data = AnalysData, NonMiss = AggrNA))
			}
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

		if(funAnalysis != "anomaly"){
			AggrNA <- lapply(AnalysData, function(x) x$NonMiss)
			AggrNA <- do.call(mapply, c(cbind, AggrNA, SIMPLIFY = FALSE))
			AnalysData <- lapply(AnalysData, function(x) x$Data)
			if(funAnalysis == "trend") AnalysData <- do.call(mapply, c(cbind, AnalysData, SIMPLIFY = FALSE))
			else AnalysData <- do.call(cbind, AnalysData)
		}
	}

	################################################
	ANALYSIS <- c('Mean', 'Median', 'Standard_deviation', 'Trend',
				'Coefficient_of_variation', 'Percentiles', 'Frequency', 'Anomaly')
	analysis.dir <- switch(GeneralParameters$analysis.method$mth.fun, 
									'mean' = ANALYSIS[1],
									'median' = ANALYSIS[2], 
									'std' = ANALYSIS[3],
									'trend' = ANALYSIS[4], 
									'cv' = ANALYSIS[5],
									'percentile' = ANALYSIS[6],
									'frequency' = ANALYSIS[7], 
									'anomaly' = ANALYSIS[8])

	outAnaDIR <- file.path(outputDIR, analysis.dir)
	dir.create(outAnaDIR, showWarnings = FALSE, recursive = TRUE)

	outTSDIR <- file.path(outputDIR, "Aggregated_TimeSeries")
	dir.create(outTSDIR, showWarnings = FALSE, recursive = TRUE)

	################################################

	outAna.dates <- rep(NA, length(ixm))
	outTS.dates <- vector(mode = "list", length = length(ixm))

	################################################

	if(GeneralParameters$data.type == 'cdtdataset'){
		lon <- AggrData$coords$mat$x
		lat <- AggrData$coords$mat$y
		iorder <- AggrData$colInfo$order
		nx <- length(lon)
		ny <- length(lat)
		dx <- ncdim_def("Lon", "degreeE", lon)
		dy <- ncdim_def("Lat", "degreeN", lat)
		xy.dim <- list(dx, dy)

		if(funAnalysis != "anomaly"){
			grdNA <- ncvar_def("nonNA", "", xy.dim, NA, longname = "Fraction of the available data", prec = "float", compression = 6)
			if(funAnalysis == "trend"){
				if(GeneralParameters$analysis.method$trend.unit == 1) trend.longname <- "change or trend/year"
				if(GeneralParameters$analysis.method$trend.unit == 2) trend.longname <- "change or trend over the period"
				if(GeneralParameters$analysis.method$trend.unit == 3) trend.longname <- "change or trend/average (in %)"
				grd.slp <- ncvar_def("trend", "", xy.dim, NA, longname = trend.longname, prec = "float", compression = 6)
				grd.std.slp <- ncvar_def("std.slope", "", xy.dim, NA, longname = "Slope error", prec = "float", compression = 6)
				grd.pvalue <- ncvar_def("pvalue", "", xy.dim, NA, longname = "P-value", prec = "float", compression = 6)
				grd.r2 <- ncvar_def("r2", "", xy.dim, NA, longname = "Coefficient of determination R2", prec = "float", compression = 6)
				out.vars <- list(grd.slp, grd.std.slp, grd.pvalue, grd.r2, grdNA)
			}else{
				if(funAnalysis == "mean"){
					nc.var <- "mean"
					longname.mon <- "Mean"
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
				grdOut <- ncvar_def(nc.var, "", xy.dim, NA, longname = longname.mon, prec = "float", compression = 6)
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

				outAna.dates[jj] <- dateAna
				outfile <- file.path(outAnaDIR, paste0(funAnalysis, "_", dateAna, ".nc"))
				nc <- nc_create(outfile, out.vars)
				if(funAnalysis == "trend"){
					mat.slp <- as.numeric(AnalysData[[jj]][1, ])
					mat.slp <- matrix(mat.slp[iorder], ncol = ny, nrow = nx)
					mat.std.slp <- as.numeric(AnalysData[[jj]][2, ])
					mat.std.slp <- matrix(mat.std.slp[iorder], ncol = ny, nrow = nx)
					mat.pval <- as.numeric(AnalysData[[jj]][3, ])
					mat.pval <- matrix(mat.pval[iorder], ncol = ny, nrow = nx)
					mat.r2 <- as.numeric(AnalysData[[jj]][4, ])
					mat.r2 <- matrix(mat.r2[iorder], ncol = ny, nrow = nx)
					mat.na <- as.numeric(AggrNA[[jj]])
					mat.na <- matrix(mat.na[iorder], ncol = ny, nrow = nx)
					
					ncvar_put(nc, out.vars[[1]], mat.slp)
					ncvar_put(nc, out.vars[[2]], mat.std.slp)
					ncvar_put(nc, out.vars[[3]], mat.pval)
					ncvar_put(nc, out.vars[[4]], mat.r2)
					ncvar_put(nc, out.vars[[5]], mat.na)
				}else{
					mat.out <- as.numeric(AnalysData[jj, ])
					mat.out <- matrix(mat.out[iorder], ncol = ny, nrow = nx)
					mat.na <- as.numeric(AggrNA[[jj]])
					mat.na <- matrix(mat.na[iorder], ncol = ny, nrow = nx)
					ncvar_put(nc, out.vars[[1]], mat.out)
					ncvar_put(nc, out.vars[[2]], mat.na)
				}
				nc_close(nc)
			}
		}

		if(funAnalysis == "anomaly"){
			longname <- paste("Anomaly", if(GeneralParameters$analysis.method$perc.anom) "percentage of mean" else NULL)
			grdAnom <- ncvar_def("anom", "", xy.dim, NA, longname = longname, prec = "float", compression = 6)
			for(jj in seq_along(ixm)){
				tsdaty <- odaty[[jj]]
				AnalysData <- readCdtDatasetChunk.sepdir.dates.order(AggrData$file, outChunkAnom, tsdaty)
				outTSdaty <- vector(mode = "character", length = length(tsdaty))

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

					outTSdaty[ii] <- dateTS
					outfile <- file.path(outAnaDIR, paste0(funAnalysis, "_", dateTS, ".nc"))
					nc <- nc_create(outfile, grdAnom)
					ncvar_put(nc, grdAnom, matrix(as.numeric(AnalysData[ii, ]), ncol = ny, nrow = nx))
					nc_close(nc)
				}

				#####
				yrsAna <- paste0(range(yeartoAna), collapse = "-")
				if(seasonLength <= 12){
					year1 <- substr(tsdaty[1], 1, 4) 
					mon1 <- substr(tsdaty[1], 6, 7)
					year2 <- substr(tsdaty[1], 9, 12)
					mon2 <- substr(tsdaty[1], 14, 15)
					if(year1 == year2){
						if(mon1 != mon2){
							dateAna <- if(mon1 == "01" & mon2 == "12") yrsAna else paste0(yrsAna, "_", paste0(mon1, "-", mon2))
						}else dateAna <- paste0(yrsAna, "_", mon1)
					}else dateAna <- paste0(yrsAna, "_", paste0(mon1, "-", mon2))
				}else dateAna <- paste0(yrsAna, "_", seasonLength)

				outAna.dates[jj] <- dateAna
			}
			unlink(outChunkAnom, recursive = TRUE)
		}

		## Time series
		if(aggregatData){
			grdTS <- ncvar_def("ts", "", xy.dim, NA, longname = "Time series", prec = "float", compression = 6)
			for(jj in seq_along(ixm)){
				tsdaty <- odaty[[jj]]
				aggrdatTS <- readCdtDatasetChunk.multi.dates.order(AggrData$file, tsdaty)
				outTSdaty <- vector(mode = "character", length = length(tsdaty))

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

					outTSdaty[ii] <- dateTS
					outfile <- file.path(outTSDIR, paste0("outTS", "_", dateTS, ".nc"))
					nc <- nc_create(outfile, grdTS)
					ncvar_put(nc, grdTS, matrix(as.numeric(aggrdatTS[ii, ]), ncol = ny, nrow = nx))
					nc_close(nc)
				}
				outTS.dates[[jj]] <- list(outAna.dates[jj], outTSdaty)
			}
			EnvSpatialAnalysis$outTS.dates <- outTS.dates
		}else outTS.dates <- EnvSpatialAnalysis$outTS.dates
	}else{
		xhead <- rbind(cdtdata$id, cdtdata$lon, cdtdata$lat)
		infohead <- cbind(c('ID.STN', 'LON', 'VARS/LAT'), xhead)

		## Analysis
		if(funAnalysis != "anomaly"){
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

				outAna.dates[jj] <- dateAna
				outfile <- file.path(outAnaDIR, paste0(funAnalysis, "_", dateAna, ".csv"))
				nonmiss <- cbind("Non-missing-data", round(AggrNA[[jj]], 3))

				if(funAnalysis == "trend"){
					if(GeneralParameters$analysis.method$trend.unit == 1) trend.longname <- "change or trend/year"
					if(GeneralParameters$analysis.method$trend.unit == 2) trend.longname <- "change or trend over the period"
					if(GeneralParameters$analysis.method$trend.unit == 3) trend.longname <- "change or trend/average (in %)"
					outdata <- cbind(c(trend.longname, rownames(AnalysData[[jj]])[-1]), AnalysData[[jj]])
				}else{
					outdata <- cbind(paste0(funAnalysis, "_", dateAna), round(AnalysData[jj, , drop = FALSE], 3))
				}

				outdata <- rbind(infohead, outdata, nonmiss)
				writeFiles(outdata, outfile)
			}
		}

		if(funAnalysis == "anomaly"){
			headInfo <- cbind(c('ID.STN', 'LON', 'DATE/LAT'), xhead)
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

				outfile <- file.path(outAnaDIR, paste0(funAnalysis, "_", outDFile, ".csv"))
				outdata <- cbind(dateTS, round(AnalysData[ixm[[jj]], , drop = FALSE], 2))
				outdata <- rbind(headInfo, outdata)
				writeFiles(outdata, outfile)

				####
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

				outAna.dates[jj] <- dateAna
			}
		}

		## Time series
		if(aggregatData){
			headInfo <- cbind(c('ID.STN', 'LON', 'DATE/LAT'), xhead)
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

				outTS.dates[[jj]] <- list(outDFile, dateTS)
				outfile <- file.path(outTSDIR, paste0("outTS", "_", outDFile, ".csv"))
				outdata <- cbind(dateTS, round(AggrData[ixm[[jj]], , drop = FALSE], 3))
				outdata <- rbind(headInfo, outdata)
				writeFiles(outdata, outfile)
			}
			EnvSpatialAnalysis$outTS.dates <- outTS.dates
		}else outTS.dates <- EnvSpatialAnalysis$outTS.dates
	}
	rm(cdtdata, AggrData, AggrNA, AnalysData); gc()
	out <- NULL
	out$params <- GeneralParameters
	out$monthtoAna <- monthtoAna
	out$stats <- outAna.dates
	out$timeseries <- outTS.dates
	saveRDS(out, file = file.path(outAnaDIR, "params.rds"))

	dirAnalysis <- list.dirs(outputDIR, full.names = FALSE, recursive = FALSE)
	dirAnalysis <- ANALYSIS[ANALYSIS%in%dirAnalysis]
	outStat <- list(Stats = dirAnalysis, last = analysis.dir)
	saveRDS(outStat, file = file.path(outputDIR, "SpatialAnalysis.rds"))
	EnvSpatialAnalysisplot$DirStat <- outStat
	EnvSpatialAnalysisplot$PathStat <- outputDIR
	return(0)
}

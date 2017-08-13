ExtractDataProcs <- function(GeneralParameters){
	start.year <- GeneralParameters$date.range$start.year
	start.mon <- GeneralParameters$date.range$start.mon
	start.day <- GeneralParameters$date.range$start.day
	end.year <- GeneralParameters$date.range$end.year
	end.mon <- GeneralParameters$date.range$end.mon
	end.day <- GeneralParameters$date.range$end.day
	startMonth <- GeneralParameters$date.range$start.month
	endMonth <- GeneralParameters$date.range$end.month

	xminLon <- GeneralParameters$Geom$minlon
	xmaxLon <- GeneralParameters$Geom$maxlon
	xminLat <- GeneralParameters$Geom$minlat
	xmaxLat <- GeneralParameters$Geom$maxlat
	xpadLon <- GeneralParameters$Geom$padlon
	xpadLat <- GeneralParameters$Geom$padlat

	####
	outputDIR <- GeneralParameters$out.data$outdir
	if(outputDIR == "" | outputDIR == "NA" | is.na(outputDIR)){
		if(GeneralParameters$out.data$format == 'ncdf') msg <- "No directory to save the extracted data"
		else msg <- "No File to save the extracted data"
		InsertMessagesTxt(main.txt.out, msg, format = TRUE)
		return(NULL)
	}

	if(GeneralParameters$in.series %in% c("daily", "dekadal")){
		if(is.na(start.year) | is.na(start.mon) | is.na(start.day) |
			is.na(end.year) | is.na(end.mon) | is.na(end.day)){
			InsertMessagesTxt(main.txt.out, "Invalid date for time series extraction", format = TRUE)
			return(NULL)
		}
	}else{
		if(is.na(start.year) | is.na(start.mon) | is.na(end.year) | is.na(end.mon)){
			InsertMessagesTxt(main.txt.out, "Invalid date for time series extraction", format = TRUE)
			return(NULL)
		}
	}

	if(GeneralParameters$in.series == "dekadal"){
		if(GeneralParameters$date.range$start.day > 3 | GeneralParameters$date.range$end.day >3){
			InsertMessagesTxt(main.txt.out, "Invalid dekad date", format = TRUE)
			return(NULL)
		}
	}

	####
	if(GeneralParameters$type.series != 'rawts'){
		if(!GeneralParameters$climato$all.years){
			if(is.na(GeneralParameters$climato$start.year) | is.na(GeneralParameters$climato$end.year)){
				InsertMessagesTxt(main.txt.out, "Invalid start and end of years for climatology calculation", format = TRUE)
				return(NULL)
			}
		}
		if(is.na(GeneralParameters$climato$min.year)){
			InsertMessagesTxt(main.txt.out, "Invalid minimum number of year for climatology calculation", format = TRUE)
			return(NULL)
		}
		if(GeneralParameters$in.series == "daily" & is.na(GeneralParameters$climato$winsize)){
			InsertMessagesTxt(main.txt.out, "Invalid window size for climatology calculation", format = TRUE)
			return(NULL)
		}
	}

	####
	if(GeneralParameters$type.extract == 'point'){
		if(is.na(xminLon) | is.na(xminLat)){
			InsertMessagesTxt(main.txt.out, "Invalid coordinates to extract", format = TRUE)
			return(NULL)
		}
	}

	if(GeneralParameters$type.extract == 'rect'){
		if(is.na(xminLon) | is.na(xmaxLon) | is.na(xminLat) | is.na(xmaxLat)){
			InsertMessagesTxt(main.txt.out, "Invalid coordinates for the extraction", format = TRUE)
			return(NULL)
		}
	}

	if(GeneralParameters$type.extract %in% c('mpoint', 'mpoly')){
		if(is.null(GeneralParameters$Geom$multiObj)){
			if(GeneralParameters$type.extract == 'mpoint') InsertMessagesTxt(main.txt.out, "No selected points", format = TRUE)
			if(GeneralParameters$type.extract == 'mpoly') InsertMessagesTxt(main.txt.out, "No selected polygons", format = TRUE)
			return(NULL)
		}

		multiptspoly <- gsub("[\r]", "", GeneralParameters$Geom$multiObj)
		multiptspoly <- str_trim(strsplit(multiptspoly, "[\n]")[[1]])
		multiptspoly <- multiptspoly[multiptspoly != ""]
		if(length(multiptspoly) == 0){
			InsertMessagesTxt(main.txt.out, "No coordinates  or polygons found", format = TRUE)
			return(NULL)
		}
	}

	if(GeneralParameters$type.extract %in% c('poly', 'mpoly')){
		shpf <- getShpOpenData(GeneralParameters$shp.file$shp)[[2]]
		if(!is.null(shpf)){
			shpf.union <- unionSpatialPolygons(shpf, as.character(shpf@data[, GeneralParameters$shp.file$attr]))
			shpf.df <- aggregate(as(shpf, "data.frame")[, 1], list(as.character(shpf@data[, GeneralParameters$shp.file$attr])), identity)
			shpf.df$x <- seq(shpf.union)
			row.names(shpf.df) <- sapply(slot(shpf.union, "polygons"), function(x) slot(x, "ID"))
			shpf.union <- SpatialPolygonsDataFrame(shpf.union, shpf.df)
		}else{
			InsertMessagesTxt(main.txt.out, "No polygons found", format = TRUE)
			return(NULL)
		}
	}

	####
	if(GeneralParameters$in.series %in% c('daily', 'dekadal')){
		daty1 <- try(as.Date(paste(start.year, start.mon, start.day, sep = '-')), silent = TRUE)
		daty2 <- try(as.Date(paste(end.year, end.mon, end.day, sep = '-')), silent = TRUE)
	}

	if(GeneralParameters$in.series == 'monthly'){
		daty1 <- try(as.Date(paste(start.year, start.mon, '01', sep = '-')), silent = TRUE)
		daty2 <- try(as.Date(paste(end.year, end.mon, '01', sep = '-')), silent = TRUE)
	}

	if(inherits(daty1, "try-error") | inherits(daty2, "try-error")){
		InsertMessagesTxt(main.txt.out, "Invalid date for time series extraction", format = TRUE)
		return(NULL)
	}

	####
	if(GeneralParameters$in.series == 'daily'){
		dates <- format(seq(daty1, daty2, 'day'), '%Y%m%d')
		ncfiles <- sprintf(GeneralParameters$ncdf.file$format, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 8))
	}

	if(GeneralParameters$in.series == 'dekadal'){
		dates <- seq(daty1, daty2, 'day')
		dates <- paste(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%Y%m'),
					as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%d')), sep = '')
		ncfiles <- sprintf(GeneralParameters$ncdf.file$format, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 7))
	}

	if(GeneralParameters$in.series == 'monthly'){
		dates <- format(seq(daty1, daty2, 'month'), '%Y%m')
		ncfiles <- sprintf(GeneralParameters$ncdf.file$format, substr(dates, 1, 4), substr(dates, 5, 6))
	}

	#####################################
	seasonLength <- (endMonth-startMonth+1)%%12
	seasonLength[seasonLength == 0] <- 12
	monthtoExtr <- (startMonth:(startMonth+(seasonLength-1)))%%12
	monthtoExtr[monthtoExtr == 0] <- 12

	imois <-  as.numeric(substr(dates, 5, 6))%in%monthtoExtr
	dates <- dates[imois]
	ncfiles <- ncfiles[imois]

	ncpath <- file.path(GeneralParameters$ncdf.file$dir, ncfiles)
	existFl <- unlist(lapply(ncpath, file.exists))
	if(!any(existFl)){
		InsertMessagesTxt(main.txt.out, "Unable to locate netcdf files", format = TRUE)
		return(NULL)
	}

	dates <- dates[existFl]
	ncpath <- ncpath[existFl]

	####
	ncInfo <- getRFESampleData(GeneralParameters$ncdf.file$sample)
	if(is.null(ncInfo)){
		InsertMessagesTxt(main.txt.out, "No netcdf data sample file found", format = TRUE)
		return(NULL)
	}

	#####################################
	nc <- nc_open(ncpath[1])
	lon <- nc$dim[[ncInfo$rfeILon]]$vals
	lat <- nc$dim[[ncInfo$rfeILat]]$vals
	nc.name <- ncInfo$rfeVarid
	nc.longname <- nc$var[[nc.name]]$longname
	nc.units <- nc$var[[nc.name]]$units
	# nc.missval <- nc$var[[nc.name]]$missval
	nc.prec <- nc$var[[nc.name]]$prec
	## xval <- ncvar_get(nc, varid = nc.name)
	nc_close(nc)

	xo <- order(lon)
	lon <- lon[xo]
	yo <- order(lat)
	lat <- lat[yo]

	spxycrd <- expand.grid(x = lon, y = lat)
	coordinates(spxycrd) <- ~x+y
	spxycrd <- SpatialPixels(points = spxycrd, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))

	#####################################

	parsextr <- list(xminLon, xminLat, xmaxLon, xmaxLat, xpadLon, xpadLat,
					GeneralParameters$Geom$namePoly, GeneralParameters$Geom$multiObj,
					GeneralParameters$shp.file$shp, GeneralParameters$shp.file$attr,
					GeneralParameters$type.extract)

	if(is.null(EnvExtractData$parsextr)){
		extractGeom <- TRUE
	}else{
		extractGeom <- if(!isTRUE(all.equal(EnvExtractData$parsextr, parsextr))) TRUE else FALSE
	}

	if(extractGeom){
		InsertMessagesTxt(main.txt.out, 'Define extraction geometry ...')

		if(GeneralParameters$type.extract == 'point'){
			if(is.na(xpadLon)){
				InsertMessagesTxt(main.txt.out, "Pad lon is missing, no padding will be applied belong the longitude", format = TRUE)
				xpadLon <- 0
			}
			if(is.na(xpadLat)){
				InsertMessagesTxt(main.txt.out, "Pad lat is missing, no padding will be applied belong the latitude", format = TRUE)
				xpadLat <- 0
			}

			headinfo <- cbind('Point', xminLon, xminLat)
			nxy <- spxycrd@grid@cellsize
			padx <- round(xpadLon/nxy[1])
			pady <- round(xpadLat/nxy[2])
			voisin <- expand.grid(x = xminLon + nxy[1]*(-padx:padx), y =  xminLat + nxy[2]*(-pady:pady))
			coordinates(voisin) <- ~x+y
			if(length(voisin) > 1) voisin <- SpatialPixels(points = voisin, tolerance = sqrt(sqrt(.Machine$double.eps)))
			ij2xtr <- over(voisin, spxycrd)
			if(!any(!is.na(ij2xtr))){
				InsertMessagesTxt(main.txt.out, "No data to extract: Object outside data range", format = TRUE)
				return(NULL)
			}
		}

		if(GeneralParameters$type.extract == 'mpoint'){
			if(is.na(xpadLon)){
				InsertMessagesTxt(main.txt.out, "Pad lon is missing, no padding will be applied belong the longitude", format = TRUE)
				xpadLon <- 0
			}
			if(is.na(xpadLat)){
				InsertMessagesTxt(main.txt.out, "Pad lat is missing, no padding will be applied belong the latitude", format = TRUE)
				xpadLat <- 0
			}

			multiptspoly <- t(sapply(multiptspoly, function(x) strsplit(x, " ")[[1]]))
			multiptspoly <- data.frame(multiptspoly, stringsAsFactors = FALSE)
			rownames(multiptspoly) <- NULL
			names(multiptspoly) <- c('id', 'x', 'y')
			multiptspoly[, 2:3] <- apply(multiptspoly[, 2:3, drop = FALSE], 2, as.numeric)
			headinfo <- multiptspoly

			nxy <- spxycrd@grid@cellsize
			padx <- round(xpadLon/nxy[1])
			pady <- round(xpadLat/nxy[2])
			voisin <- lapply(seq(nrow(multiptspoly)), function(j){
							xy <- expand.grid(x = multiptspoly[j, 'x'] + nxy[1]*(-padx:padx),
											y =  multiptspoly[j, 'y'] + nxy[2]*(-pady:pady))
							coordinates(xy) <- ~x+y
							if(length(xy) > 1) xy <- SpatialPixels(points = xy, tolerance = sqrt(sqrt(.Machine$double.eps)))
							return(xy)
						})
			ij2xtr <- lapply(voisin, over, y = spxycrd)
			if(all(sapply(ij2xtr, function(x) !any(!is.na(x))))){
				InsertMessagesTxt(main.txt.out, "No data to extract: Object outside data range", format = TRUE)
				return(NULL)
			}
		}

		if(GeneralParameters$type.extract == 'rect'){
			rectPoly <- Polygon(cbind(c(xminLon, xmaxLon, xmaxLon, xminLon, xminLon),
									c(xminLat, xminLat, xmaxLat, xmaxLat, xminLat)))
			rectPoly <- Polygons(list(rectPoly), "p1")
			rectPoly <- SpatialPolygons(list(rectPoly), 1:1)
			headinfo <- NULL

			polyRas <- spxycrd
			polyRas$z <- seq_along(spxycrd)
			polyRas <- raster(polyRas)
			ij2xtr <- extract(polyRas, rectPoly, weights = TRUE, normalizeWeights = TRUE, cellnumbers = TRUE)
			if(is.null(ij2xtr[[1]])){
				InsertMessagesTxt(main.txt.out, "No data to extract: Object outside data range", format = TRUE)
				return(NULL)
			}
		}

		if(GeneralParameters$type.extract == 'poly'){
			shpf.regOI <- shpf.union[str_trim(shpf.union@data$Group.1) == GeneralParameters$Geom$namePoly, ]
			headinfo <- NULL

			polyRas <- spxycrd
			polyRas$z <- seq_along(spxycrd)
			polyRas <- raster(polyRas)
			ij2xtr <- extract(polyRas, shpf.regOI, weights = TRUE, normalizeWeights = TRUE, cellnumbers = TRUE)
			if(is.null(ij2xtr[[1]])){
				InsertMessagesTxt(main.txt.out, "No data to extract: Object outside data range", format = TRUE)
				return(NULL)
			}
		}

		if(GeneralParameters$type.extract == 'mpoly'){
			shpf.regOI <- shpf.union[str_trim(shpf.union@data$Group.1)%in%multiptspoly, ]
			headinfo <- cbind(as.character(shpf.regOI@data$Group.1), round(coordinates(shpf.regOI), 5))
			headinfo[, 1] <- substr(str_replace_all(headinfo[, 1], "[^[:alnum:]]", ""), 1, 15)

			polyRas <- spxycrd
			polyRas$z <- seq_along(spxycrd)
			polyRas <- raster(polyRas)
			ij2xtr <- extract(polyRas, shpf.regOI, weights = TRUE, normalizeWeights = TRUE, cellnumbers = TRUE)
			if(all(sapply(ij2xtr, is.null))){
				InsertMessagesTxt(main.txt.out, "No data to extract: Object outside data range", format = TRUE)
				return(NULL)
			}
		}

		EnvExtractData$parsextr <- parsextr
		EnvExtractData$ij2xtr <- ij2xtr
		EnvExtractData$headinfo <- headinfo
		InsertMessagesTxt(main.txt.out, 'Extraction geometry definition done!')
	}else{
		ij2xtr <- EnvExtractData$ij2xtr
		headinfo <- EnvExtractData$headinfo
	}

	#####################################

	if(GeneralParameters$type.extract == 'point'){
		ij2xtr <- ij2xtr[!is.na(ij2xtr)]
	}

	if(GeneralParameters$type.extract == 'mpoint'){
		ij2xtr <- lapply(ij2xtr, function(x) x[!is.na(x)])
		nonZero <- sapply(ij2xtr, length) > 0
		ij2xtr <- ij2xtr[nonZero]
	}

	if(GeneralParameters$type.extract == 'rect'){
		ij2xtr <- lapply(ij2xtr, function(x) x[order(x[, "value"]), , drop = FALSE])
		# RAGNE (take all pixels)
		# RANGEEDGES (only take full pixels)
		# ij2xtr <- lapply(ij2xtr, function(x){
		# 				mx <- x[, "weight"]
		# 				x <- x[mx == max(mx), , drop = FALSE]
		# 				x[, "weight"] <- 1/nrow(x)
		# 				return(x)
		# 			})
		headinfo <- lapply(ij2xtr, function(x) reshapeXYZ2Matrix(cbind(spxycrd@coords[x[, 'value'], , drop = FALSE], seq(nrow(x)))))
		headinfo <- headinfo[[1]]
		if(GeneralParameters$out.data$sp.avrg){
			headinfo <- cbind('Rectangle', mean(headinfo$x), mean(headinfo$y))
		}
	}

	if(GeneralParameters$type.extract == 'poly'){
		ij2xtr <- lapply(ij2xtr, function(x) x[order(x[, "value"]), , drop = FALSE])
		headinfo <- lapply(ij2xtr, function(x) reshapeXYZ2Matrix(cbind(spxycrd@coords[x[, 'value'], , drop = FALSE], seq(nrow(x)))))
		headinfo <- headinfo[[1]]
		if(GeneralParameters$out.data$sp.avrg){
			namepoly <- substr(str_replace_all(GeneralParameters$Geom$namePoly, "[^[:alnum:]]", ""), 1, 15)
			headinfo <- cbind(namepoly, coordinates(shpf.regOI))
		}
	}

	if(GeneralParameters$type.extract == 'mpoly'){
		nonNull <- !sapply(ij2xtr, is.null)
		ij2xtr <- ij2xtr[nonNull]
		ij2xtr <- lapply(ij2xtr, function(x) x[order(x[, "value"]), , drop = FALSE])
	}

	#####################################

	infoFiles <- c(GeneralParameters$ncdf.file$dir, GeneralParameters$ncdf.file$format)
	bindData <- FALSE
	if(!is.null(EnvExtractData$cdtdata)){
		if(isTRUE(all.equal(EnvExtractData$ij2xtr1, ij2xtr))){
			iexist <- dates%in%EnvExtractData$cdtdata$dates
			if(all(iexist)){
				if(!isTRUE(all.equal(EnvExtractData$infoFiles, infoFiles))){
					readNCDFdata <- TRUE
					EnvExtractData$cdtdata <- NULL
				}else readNCDFdata <- FALSE
			}else{
				if(isTRUE(all.equal(EnvExtractData$infoFiles, infoFiles))){
					bindData <- TRUE
					if(any(iexist)){
						dates <- dates[!iexist]
						ncpath <- ncpath[!iexist]
					}
				}else EnvExtractData$cdtdata <- NULL
				readNCDFdata <- TRUE
			}
		}else{
			readNCDFdata <- TRUE
			EnvExtractData$cdtdata <- NULL
		}
	}else readNCDFdata <- TRUE

	if(readNCDFdata){
		InsertMessagesTxt(main.txt.out, 'Read and extract netcdf data ...')

		is.parallel <- doparallel(length(ncpath) >= 180)
		`%parLoop%` <- is.parallel$dofun
		ncData <- foreach(jj = seq_along(ncpath), .packages = "ncdf4") %parLoop% {
			nc <- nc_open(ncpath[jj])
			vars <- ncvar_get(nc, varid = ncInfo$rfeVarid)
			nc_close(nc)
			vars <- vars[xo, yo]
			if(ncInfo$rfeILat < ncInfo$rfeILon){
				vars <- matrix(c(vars), nrow = length(xo), ncol = length(yo), byrow = TRUE)
			}
			vars <- round(c(vars), 1)

			if(GeneralParameters$type.extract == 'point'){
				DATAext <- vars[ij2xtr]
				if(length(ij2xtr) > 1) DATAext <- mean(DATAext, na.rm = TRUE)
			}

			if(GeneralParameters$type.extract == 'mpoint'){
				DATAext <- sapply(ij2xtr, function(ij){
					MAT <- vars[ij]
					if(length(ij) > 1) MAT <- mean(MAT, na.rm = TRUE)
					MAT
				})
				if(!all(nonZero)){
					DATAtmp <- rep(NA, length(nonZero))
					DATAtmp[nonZero] <- DATAext
					DATAext <- DATAtmp
					rm(DATAtmp)
				}
			}

			if(GeneralParameters$type.extract == 'mpoly'){
				DATAext <- sapply(ij2xtr, function(ij){
					MAT <- vars[ij[, "value"]]
					nijt <- nrow(ij)
					if(nijt > 1){
						MAT <- MAT * ij[, "weight"]
						MAT <- sum(MAT, na.rm = TRUE)
					}
					MAT
				})

				if(!all(nonNull)){
					DATAtmp <- rep(NA, ncol = length(nonNull))
					DATAtmp[nonNull] <- DATAext
					DATAext <- DATAtmp
					rm(DATAtmp)
				}
			}

			if(GeneralParameters$type.extract%in%c('rect', 'poly')){
				DATAext <- vars[ij2xtr[[1]][, "value"]]
			}

			rm(vars); gc()
			return(DATAext)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
		InsertMessagesTxt(main.txt.out, 'Reading and extracting netcdf data finished')

		ncData <- do.call(rbind, ncData)
		cdtdata <- list(dates = dates,
						data = ncData)
		rm(ncData); gc()

		if(bindData){
			cdtdata$dates <- c(EnvExtractData$cdtdata$dates, cdtdata$dates)
			cdtdata$data <- rbind(EnvExtractData$cdtdata$data, cdtdata$data)
			odaty <- order(cdtdata$dates)
			cdtdata$dates <- cdtdata$dates[odaty]
			cdtdata$data <- cdtdata$data[odaty, , drop = FALSE]
		}
		EnvExtractData$cdtdata <- cdtdata
		EnvExtractData$infoFiles <- infoFiles
		EnvExtractData$ij2xtr1 <- ij2xtr
	}else cdtdata <- EnvExtractData$cdtdata

	#####################################

	if(GeneralParameters$type.extract%in%c('rect', 'poly')){
		if(GeneralParameters$out.data$sp.avrg){
			nDAT <- nrow(cdtdata$data)
			nijt <- nrow(ij2xtr[[1]])
			cdtdata$data <- cdtdata$data * rep(ij2xtr[[1]][, "weight"], rep(nDAT, nijt))
			cdtdata$data <- matrix(base::rowSums(cdtdata$data, na.rm = TRUE), ncol = 1)
		}
	}

	#####################################

	# start.seas <- GeneralParameters$out.series$start.seas
	# end.seas <- GeneralParameters$out.series$end.seas
	# len.seas <- (end.seas-start.seas+1)%%12
	# len.seas[len.seas == 0] <- 12
	# monthtoSeas <- (start.seas:(start.seas+(len.seas-1)))%%12
	# monthtoSeas[monthtoSeas == 0] <- 12

	#####################################

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

	itmp <- as.numeric(substr(cdates, 5, 6))%in%monthtoExtr
	xrow <- idrow[itmp]
	xdaty <- cdates[itmp]

	##################

	if(GeneralParameters$in.series == GeneralParameters$out.series$out.series){
		AggrData <- cdtdata$data[xrow, , drop = FALSE]
		odaty <- xdaty
		indx <- xrow
	}else{
		InsertMessagesTxt(main.txt.out, 'Aggregate data ...')
		if(GeneralParameters$out.series$out.series == "dekadal"){
			jour <- as.numeric(substr(xdaty, 7, 8))
			jour[jour <= 10] <- 1
			jour[jour > 10 & jour <= 20] <- 2
			jour[jour > 20] <- 3
			indx <- tapply(xrow, paste0(substr(xdaty, 1, 6), jour), identity)
			odaty <- names(indx)
			len.data <- sapply(indx, length)
		}else if(GeneralParameters$out.series$out.series == "monthly"){
			indx <- tapply(xrow, substr(xdaty, 1, 6), identity)
			odaty <- names(indx)
			len.data <- sapply(indx, length)
		}else{
			xmois <- as.numeric(substr(xdaty, 5, 6))

			rleMois <- rle(xmois)
			xrank <- cumsum(rleMois$lengths)
			istart <- seq(which(rleMois$values %in% monthtoExtr[1])[1], length(rleMois$values), seasonLength)
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

		##################

		nbstn <- ncol(cdtdata$data)
		transPose <- if(nbstn > 1) t else as.matrix
		len.MAT <- matrix(len.data, nrow = length(len.data), ncol = nbstn)

		##################

		toAggr <- list(indx, GeneralParameters$aggr.series, headinfo)

		if(is.null(EnvExtractData$toAggr)){
			aggregatData <- TRUE
		}else{
			aggregatData <- if(!isTRUE(all.equal(EnvExtractData$toAggr, toAggr))) TRUE else FALSE
		}

		if(aggregatData){
			AggrSeries <- list(aggr.fun = GeneralParameters$aggr.series$aggr.fun,
								count.fun = GeneralParameters$aggr.series$opr.fun,
								count.thres = GeneralParameters$aggr.series$opr.thres)

			missData <- transPose(sapply(indx, funMissMAT, DATA = cdtdata$data))
			AggrData <- transPose(sapply(indx, funAggrMAT, DATA = cdtdata$data, pars = AggrSeries))
			AggrData[(1-(missData/len.MAT)) < GeneralParameters$aggr.series$min.frac] <- NA
			EnvExtractData$AggrData <- AggrData
			EnvExtractData$toAggr <- toAggr
		}else AggrData <- EnvExtractData$AggrData
		InsertMessagesTxt(main.txt.out, 'Aggregating data finished')
	}

	#####################################
	## Climatologies

	if(GeneralParameters$type.series != "rawts"){
		InsertMessagesTxt(main.txt.out, 'Calculate climatologies ...')

		## Climatologies
		calClim <- list(indx, GeneralParameters$aggr.series, headinfo, GeneralParameters$climato)
		if(is.null(EnvExtractData$calClim)){
			computeClimato <- TRUE
		}else{
			# computeClimato <- if(!isTRUE(all.equal(EnvExtractData$calClim, calClim))) TRUE else FALSE
			if(isTRUE(all.equal(EnvExtractData$calClim, calClim))){
				computeClimato <- if(is.null(EnvExtractData$sdOUT)) TRUE else FALSE
			}else computeClimato <- TRUE
		}

		if(computeClimato){
			climYear1 <- GeneralParameters$climato$start.year
			climYear2 <- GeneralParameters$climato$end.year

			if(GeneralParameters$out.series$out.series == "daily"){
				winsize <- GeneralParameters$climato$winsize
				if(winsize < 1) winsize <- 1
				years <- as.numeric(substr(cdates, 1, 4))
				feb29 <- which(substr(cdates, 5, 8) == '0229')
				if(GeneralParameters$climato$all.years){
					ryear <- range(years)
					climYear1 <- ryear[1]
					climYear2 <- ryear[2]
				}
				ixnorm <- index.data.rollNormal(cdates, climYear1, climYear2, winsize)
				ixdays <- index.data.rollClimato(cdates, climYear1, climYear2, winsize)
				iClim <- idrow[ixnorm[!ixnorm%in%feb29]]

				idxclim <- lapply(1:365, function(j) iClim[ixdays[[j]]])
				vtimes <- table.annuel()
				itmp <- vtimes[, 2]%in%monthtoExtr
				vtimes <- vtimes[itmp, , drop = FALSE]
				idxclim <- idxclim[itmp]
				climOUT <- lapply(idxclim, function(x){
					MAT <- cdtdata$data[x, , drop = FALSE]
					naYear <- base::colSums(!is.na(MAT))
					clim <- base::colMeans(MAT, na.rm = TRUE)
					clim[naYear < GeneralParameters$climato$min.year * winsize] <- NA
					clim
				})
				if(GeneralParameters$type.series == 'stanom'){
					sdOUT <- lapply(idxclim, function(x){
						MAT <- cdtdata$data[x, , drop = FALSE]
						naYear <- base::colSums(!is.na(MAT))
						vars <- matrixStats::colVars(MAT, na.rm = TRUE)
						vars[naYear < GeneralParameters$climato$min.year * winsize] <- NA
						sqrt(vars)
					})
				}
				climdates <- paste0(paste0(climYear1, "-", climYear2), "_",
							 paste0(str_pad(vtimes[, 2], 2, pad =  "0"),
									str_pad(vtimes[, 1], 2, pad =  "0")))
			}else{
				years <- as.numeric(substr(odaty, 1, 4))
				iyear <-if(GeneralParameters$climato$all.years) rep(TRUE, length(years)) else (years >= climYear1 & years <= climYear2)
				years <- years[iyear]
				climMAT <- AggrData[iyear, , drop = FALSE]
				cdaty <- odaty[iyear]

				if(GeneralParameters$out.series$out.series %in% c("dekadal", "monthly")){
					if(GeneralParameters$out.series$out.series == "dekadal"){
						idxc <- substr(cdaty, 5, 7)
					}else idxc <- substr(cdaty, 5, 6)

					idxclim <- tapply(seq(length(cdaty)), idxc, identity)
					climOUT <- lapply(idxclim, function(x){
						MAT <- climMAT[x, , drop = FALSE]
						naYear <- base::colSums(!is.na(MAT))
						clim <- base::colMeans(MAT, na.rm = TRUE)
						clim[naYear < GeneralParameters$climato$min.year] <- NA
						clim
					})
					if(GeneralParameters$type.series == 'stanom'){
						sdOUT <- lapply(idxclim, function(x){
							MAT <- climMAT[x, , drop = FALSE]
							naYear <- base::colSums(!is.na(MAT))
							vars <- matrixStats::colVars(MAT, na.rm = TRUE)
							vars[naYear < GeneralParameters$climato$min.year] <- NA
							sqrt(vars)
						})
					}
					climdates <- sapply(names(climOUT), function(x) paste(paste0(range(years), collapse = "-"), x, sep = "_"))
				}else{
					naYear <- base::colSums(!is.na(climMAT))
					climOUT <- base::colMeans(climMAT, na.rm = TRUE)
					climOUT[naYear < GeneralParameters$climato$min.year] <- NA
					if(GeneralParameters$type.series == 'stanom'){
						sdOUT <- matrixStats::colVars(climMAT, na.rm = TRUE)
						sdOUT[naYear < GeneralParameters$climato$min.year] <- NA
						sdOUT <- sqrt(sdOUT)
					}
					climdates <- paste(paste0(range(years), collapse = "-"),
										paste0(substr(cdaty[1], 6, 7), "-",
										substr(cdaty[1], 14, 15)), sep = "_")
				}
				rm(climMAT)
			}

			EnvExtractData$climdates <- climdates
			EnvExtractData$climOUT <- climOUT
			if(GeneralParameters$type.series == 'stanom') EnvExtractData$sdOUT <- sdOUT
			EnvExtractData$calClim <- calClim
		}else{
			climdates <- EnvExtractData$climdates
			climOUT <- EnvExtractData$climOUT
			if(GeneralParameters$type.series == 'stanom') sdOUT <- EnvExtractData$sdOUT
		}

		##################
		## Anomalies
		if(GeneralParameters$type.series == 'anom'){
			if(GeneralParameters$out.series$out.series == "daily"){
				idtclim <- substr(climdates, 11, 14)
				idtseries <- substr(odaty, 5, 8)

				idxc <- match(idtseries, idtclim)
				idxc[idtseries == '0229'] <- which(idtclim == "0228")
				outMAT <- do.call(rbind, climOUT)
				outMAT <- outMAT[idxc, , drop = FALSE]
			}else{
				if(GeneralParameters$out.series$out.series %in% c("dekadal", "monthly")){
					if(GeneralParameters$out.series$out.series == "dekadal"){
						idtclim <- substr(climdates, 11, 13)
						idtseries <- substr(odaty, 5, 7)
					}else{
						idtclim <- substr(climdates, 11, 12)
						idtseries <- substr(odaty, 5, 6)
					}

					idxc <- match(idtseries, idtclim)
					outMAT <- do.call(rbind, climOUT)
					outMAT <- outMAT[idxc, , drop = FALSE]
				}else{
					outMAT <- matrix(climOUT, nrow = nrow(AggrData), ncol = ncol(AggrData), byrow = TRUE)
				}
			}

			outMAT <- AggrData - outMAT
		}

		##################
		## Standardized Anomalies
		if(GeneralParameters$type.series == 'stanom'){
			if(GeneralParameters$out.series$out.series == "daily"){
				idtclim <- substr(climdates, 11, 14)
				idtseries <- substr(odaty, 5, 8)

				idxc <- match(idtseries, idtclim)
				idxc[idtseries == '0229'] <- which(idtclim == "0228")
				outMAT <- do.call(rbind, climOUT)
				outMAT <- outMAT[idxc, , drop = FALSE]
				outMAT1 <- do.call(rbind, sdOUT)
				outMAT1 <- outMAT1[idxc, , drop = FALSE]
			}else{
				if(GeneralParameters$out.series$out.series %in% c("dekadal", "monthly")){
					if(GeneralParameters$out.series$out.series == "dekadal"){
						idtclim <- substr(climdates, 11, 13)
						idtseries <- substr(odaty, 5, 7)
					}else{
						idtclim <- substr(climdates, 11, 12)
						idtseries <- substr(odaty, 5, 6)
					}

					idxc <- match(idtseries, idtclim)
					outMAT <- do.call(rbind, climOUT)
					outMAT <- outMAT[idxc, , drop = FALSE]
					outMAT1 <- do.call(rbind, sdOUT)
					outMAT1 <- outMAT1[idxc, , drop = FALSE]
				}else{
					outMAT <- matrix(climOUT, nrow = nrow(AggrData), ncol = ncol(AggrData), byrow = TRUE)
					outMAT1 <- matrix(sdOUT, nrow = nrow(AggrData), ncol = ncol(AggrData), byrow = TRUE)
				}
			}

			outMAT <- (AggrData - outMAT)/outMAT1
			outMAT[is.nan(outMAT) | is.infinite(outMAT)] <- 0
			rm(outMAT1)
		}
		InsertMessagesTxt(main.txt.out, 'Calculating climatologies done!')
	}

	#####################################
	## Write data
	InsertMessagesTxt(main.txt.out, 'Writing data ...')

	if(GeneralParameters$out.data$format == "ncdf"){
		if(GeneralParameters$type.series == 'rawts'){
			if(GeneralParameters$in.series == GeneralParameters$out.series$out.series){
				if(GeneralParameters$in.series == "daily" & GeneralParameters$aggr.series$aggr.fun == "count"){
					name <- 'number'
					units <- 'count'
					longname <- paste0("Number of (", paste(nc.name, GeneralParameters$aggr.series$opr.fun, GeneralParameters$aggr.series$opr.thres), ") from ", nc.longname)
					prec <- "short"
				}else{
					name <- nc.name
					units <- nc.units
					longname <- nc.longname
					prec <- nc.prec
				}
			}else{
				if(GeneralParameters$out.series$out.series == "dekadal") outFormat <- "Dekadal"
				if(GeneralParameters$out.series$out.series == "monthly") outFormat <- "Monthly"
				if(GeneralParameters$out.series$out.series == "seasonal3") outFormat <- paste("Seasonal", paste0(substr(odaty, 6, 7), "-", substr(odaty, 14, 15)))
				if(GeneralParameters$out.series$out.series == "seasonal6") outFormat <- paste("Seasonal", paste0(substr(odaty, 6, 7), "-", substr(odaty, 14, 15)))
				if(GeneralParameters$out.series$out.series == "annual") outFormat <- paste("Annual", paste0(substr(odaty, 6, 7), "-", substr(odaty, 14, 15)))
				name <- nc.name
				units <- nc.units
				longname <- paste(outFormat, "from", nc.longname)
				prec <- nc.prec
			}
		}

		if(GeneralParameters$type.series %in% c('climato', 'anom', 'stanom')){
			if(GeneralParameters$out.series$out.series == "daily") outFormat <- "Daily"
			if(GeneralParameters$out.series$out.series == "dekadal") outFormat <- "Dekadal"
			if(GeneralParameters$out.series$out.series == "monthly") outFormat <- "Monthly"
			if(GeneralParameters$out.series$out.series == "seasonal3") outFormat <- paste("Seasonal", paste0(substr(odaty, 6, 7), "-", substr(odaty, 14, 15)))
			if(GeneralParameters$out.series$out.series == "seasonal6") outFormat <- paste("Seasonal", paste0(substr(odaty, 6, 7), "-", substr(odaty, 14, 15)))
			if(GeneralParameters$out.series$out.series == "annual") outFormat <- paste("Annual", paste0(substr(odaty, 6, 7), "-", substr(odaty, 14, 15)))
			name1 <- "climato"
			if(GeneralParameters$in.series == "daily" & GeneralParameters$aggr.series$aggr.fun == "count"){
				units1 <- "count"
				prec1 <- "short"
			}else{
				units1 <- nc.units
				prec1 <- nc.prec
			}
			longname1 <- paste(outFormat, "climatology using", substr(climdates[1], 1, 9), "base period")
		}

		if(GeneralParameters$type.series == 'anom'){
			if(GeneralParameters$out.series$out.series == "daily") outFormat <- "Daily"
			if(GeneralParameters$out.series$out.series == "dekadal") outFormat <- "Dekadal"
			if(GeneralParameters$out.series$out.series == "monthly") outFormat <- "Monthly"
			if(GeneralParameters$out.series$out.series == "seasonal3") outFormat <- paste("Seasonal", paste0(substr(odaty, 6, 7), "-", substr(odaty, 14, 15)))
			if(GeneralParameters$out.series$out.series == "seasonal6") outFormat <- paste("Seasonal", paste0(substr(odaty, 6, 7), "-", substr(odaty, 14, 15)))
			if(GeneralParameters$out.series$out.series == "annual") outFormat <- paste("Annual", paste0(substr(odaty, 6, 7), "-", substr(odaty, 14, 15)))
			name <- "anom"
			if(GeneralParameters$in.series == "daily" & GeneralParameters$aggr.series$aggr.fun == "count"){
				units <- "count"
				prec <- "short"
			}else{
				units <- nc.units
				prec <- nc.prec
			}
			longname <- paste(outFormat, "anomalies, based on", substr(climdates[1], 1, 9), "climatology")
		}

		if(GeneralParameters$type.series == 'stanom'){
			if(GeneralParameters$out.series$out.series == "daily") outFormat <- "Daily"
			if(GeneralParameters$out.series$out.series == "dekadal") outFormat <- "Dekadal"
			if(GeneralParameters$out.series$out.series == "monthly") outFormat <- "Monthly"
			if(GeneralParameters$out.series$out.series == "seasonal3") outFormat <- paste("Seasonal", paste0(substr(odaty, 6, 7), "-", substr(odaty, 14, 15)))
			if(GeneralParameters$out.series$out.series == "seasonal6") outFormat <- paste("Seasonal", paste0(substr(odaty, 6, 7), "-", substr(odaty, 14, 15)))
			if(GeneralParameters$out.series$out.series == "annual") outFormat <- paste("Annual", paste0(substr(odaty, 6, 7), "-", substr(odaty, 14, 15)))
			name <- "stanom"
			units <- ""
			prec <- "float"
			longname <- paste(outFormat, "standardized anomalies, based on", substr(climdates[1], 1, 9), "climatology")
		}

		#########
		prefix <- strsplit(GeneralParameters$ncdf.file$format, "%")[[1]][1]
		missval <- -9999
		missval1 <- -9999

		dx <- ncdim_def("Lon", "degreeE", headinfo$x)
		dy <- ncdim_def("Lat", "degreeN", headinfo$y)
		xy.dim <- list(dx, dy)
		if(GeneralParameters$type.series %in% c('rawts', 'anom', 'stanom')) grd.out <- ncvar_def(name, units, xy.dim, missval, longname, prec)
		if(GeneralParameters$type.series %in% c('climato', 'anom', 'stanom')) grd.Clim <- ncvar_def(name1, units1, xy.dim, missval1, longname1, prec1)

		#########
		if(GeneralParameters$type.series == 'rawts'){
			outputDIR <- file.path(GeneralParameters$out.data$outdir, "Extracted_Data")
			dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)
			for(j in seq_along(odaty)){
				xtmp <- AggrData[j, ]
				xtmp <- xtmp[headinfo$z]
				dim(xtmp) <- dim(headinfo$z)
				xtmp[is.na(xtmp)] <- missval

				ncoutfile <- file.path(outputDIR, paste0(prefix, odaty[j], ".nc"))
				nc <- nc_create(ncoutfile, grd.out)
				ncvar_put(nc, grd.out, xtmp)
				nc_close(nc)
			}
		}else{
			if(GeneralParameters$type.series %in% c('climato', 'anom', 'stanom')){
				outputDIR <- file.path(GeneralParameters$out.data$outdir, "Climatology")
				dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)
				for(j in seq_along(climdates)){
					xtmp <- if(is.list(climOUT)) climOUT[[j]] else climOUT
					xtmp <- xtmp[headinfo$z]
					dim(xtmp) <- dim(headinfo$z)
					xtmp[is.na(xtmp)] <- missval

					ncoutfile <- file.path(outputDIR, paste0(prefix, climdates[j], ".nc"))
					nc <- nc_create(ncoutfile, grd.Clim)
					ncvar_put(nc, grd.Clim, xtmp)
					nc_close(nc)
				}
			}

			if(GeneralParameters$type.series == 'stanom'){
				outputDIR <- file.path(GeneralParameters$out.data$outdir, "Climatology_SD")
				dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)
				for(j in seq_along(climdates)){
					xtmp <- if(is.list(sdOUT)) sdOUT[[j]] else sdOUT
					xtmp <- xtmp[headinfo$z]
					dim(xtmp) <- dim(headinfo$z)
					xtmp[is.na(xtmp)] <- missval

					ncoutfile <- file.path(outputDIR, paste0(prefix, climdates[j], ".nc"))
					nc <- nc_create(ncoutfile, grd.Clim)
					ncvar_put(nc, grd.Clim, xtmp)
					nc_close(nc)
				}
			}

			if(GeneralParameters$type.series %in% c('anom', 'stanom')){
				odir <- if(GeneralParameters$type.series == 'anom') "Anomaly" else "StandardizedAnomaly"
				outputDIR <- file.path(GeneralParameters$out.data$outdir, odir)
				dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)
				for(j in seq_along(odaty)){
					xtmp <- outMAT[j, ]
					xtmp <- xtmp[headinfo$z]
					dim(xtmp) <- dim(headinfo$z)
					xtmp[is.na(xtmp)] <- missval

					ncoutfile <- file.path(outputDIR, paste0(prefix, odaty[j], ".nc"))
					nc <- nc_create(ncoutfile, grd.out)
					ncvar_put(nc, grd.out, xtmp)
					nc_close(nc)
				}
			}
		}
	}

	#####################################

	if(GeneralParameters$out.data$format == "tyxz"){
		outputFile <- GeneralParameters$out.data$outdir
		tyxz.crds <- expand.grid(round(headinfo$x, 6), round(headinfo$y, 6))
		tyxz.crds <- tyxz.crds[, 2:1]

		if(GeneralParameters$type.series == 'rawts'){
			outfile <- file.path(dirname(outputFile), paste0("TS_", basename(outputFile)))

			ret <- lapply(seq_along(odaty), function(j){
				xtmp <- AggrData[j, ]
				xtmp <- round(xtmp[headinfo$z], 3)
				xtmp[is.na(xtmp)] <- -9999
				xtmp <- data.frame(odaty[j], tyxz.crds, xtmp)
				xtmp
			})
			ret <- do.call(rbind, ret)
			writeFiles(ret, outfile)
		}else{
			if(GeneralParameters$type.series %in% c('climato', 'anom', 'stanom')){
				outfile <- file.path(dirname(outputFile), paste0("CLM-MEAN_", basename(outputFile)))

				ret <- lapply(seq_along(climdates), function(j){
					xtmp <- if(is.list(climOUT)) climOUT[[j]] else climOUT
					xtmp <- round(xtmp[headinfo$z], 3)
					xtmp[is.na(xtmp)] <- -9999
					xtmp <- data.frame(climdates[j], tyxz.crds, xtmp)
					xtmp
				})
				ret <- do.call(rbind, ret)
				writeFiles(ret, outfile)
			}

			if(GeneralParameters$type.series == 'stanom'){
				outfile <- file.path(dirname(outputFile), paste0("CLM-SD_", basename(outputFile)))

				ret <- lapply(seq_along(climdates), function(j){
					xtmp <- if(is.list(sdOUT)) sdOUT[[j]] else sdOUT
					xtmp <- round(xtmp[headinfo$z], 3)
					xtmp[is.na(xtmp)] <- -9999
					xtmp <- data.frame(climdates[j], tyxz.crds, xtmp)
					xtmp
				})
				ret <- do.call(rbind, ret)
				writeFiles(ret, outfile)
			}

			if(GeneralParameters$type.series %in% c('anom', 'stanom')){
				odir <- if(GeneralParameters$type.series == 'anom') "Anomaly" else "STDAnomaly"
				outfile <- file.path(dirname(outputFile), paste0(odir, "_", basename(outputFile)))

				ret <- lapply(seq_along(odaty), function(j){
					xtmp <- outMAT[j, ]
					xtmp <- round(xtmp[headinfo$z], 3)
					xtmp[is.na(xtmp)] <- -9999
					xtmp <- data.frame(odaty[j], tyxz.crds, xtmp)
					xtmp
				})
				ret <- do.call(rbind, ret)
				writeFiles(ret, outfile)
			}
		}
	}

	#####################################

	if(GeneralParameters$out.data$format == "cdt"){
		outputFile <- GeneralParameters$out.data$outdir
		caption <- c('ID', 'LON', 'DATE/LAT')
		xhead <- t(headinfo)

		if(GeneralParameters$type.series == 'rawts'){
			outfile <- file.path(dirname(outputFile), paste0("TS_", basename(outputFile)))
			xtmp <- round(AggrData, 3)
			xtmp[is.na(xtmp)] <- -9999
			xtmp <- rbind(xhead, xtmp)
			xtmp <- cbind(c(caption, odaty), xtmp)
			writeFiles(xtmp, outfile)
		}else{
			if(GeneralParameters$type.series %in% c('climato', 'anom', 'stanom')){
				outfile <- file.path(dirname(outputFile), paste0("CLM-MEAN_", basename(outputFile)))
				xtmp <- if(is.list(climOUT)) do.call(rbind, climOUT) else climOUT
				xtmp <- round(xtmp, 3)
				xtmp[is.na(xtmp)] <- -9999
				xtmp <- rbind(xhead, xtmp)
				xtmp <- cbind(c(caption, climdates), xtmp)
				writeFiles(xtmp, outfile)
			}

			if(GeneralParameters$type.series == 'stanom'){
				outfile <- file.path(dirname(outputFile), paste0("CLM-SD_", basename(outputFile)))
				xtmp <- if(is.list(sdOUT)) do.call(rbind, sdOUT) else sdOUT
				xtmp <- round(xtmp, 3)
				xtmp[is.na(xtmp)] <- -9999
				xtmp <- rbind(xhead, xtmp)
				xtmp <- cbind(c(caption, climdates), xtmp)
				writeFiles(xtmp, outfile)
			}

			if(GeneralParameters$type.series %in% c('anom', 'stanom')){
				odir <- if(GeneralParameters$type.series == 'anom') "Anomaly" else "STDAnomaly"
				outfile <- file.path(dirname(outputFile), paste0(odir, "_", basename(outputFile)))
				xtmp <- round(outMAT, 3)
				xtmp[is.na(xtmp)] <- -9999
				xtmp <- rbind(xhead, xtmp)
				xtmp <- cbind(c(caption, odaty), xtmp)
				writeFiles(xtmp, outfile)
			}
		}
	}

	#####################################

	if(GeneralParameters$out.data$format == "cpt"){
		outputFile <- GeneralParameters$out.data$outdir
		name <- nc.name
		units <- if(GeneralParameters$in.series == "daily" & GeneralParameters$aggr.series$aggr.fun == "count") 'count' else nc.units
		cptgrid <- GeneralParameters$type.extract %in% c('rect', 'poly') & !GeneralParameters$out.data$sp.avrg

		if(GeneralParameters$type.series == 'rawts'){
			outfile <- file.path(dirname(outputFile), paste0("TS_", basename(outputFile)))
			if(cptgrid){
				xtmp <- lapply(seq_along(odaty), function(j){
					x <- round(AggrData[j, ], 3)
					x <- x[headinfo$z]
					dim(x) <- dim(headinfo$z)
					x[is.na(x)] <- -9999
					x
				})
				gridinfo <- headinfo[c('x', 'y')]
				cptIn <- list(data = xtmp, date = odaty,  gridinfo = gridinfo,
							varid = name, units = units, missval = -9999)
				cptOut <- do.call(CPT.convertGridData, cptIn)
			}else{
				xtmp <- round(AggrData, 3)
				xtmp[is.na(xtmp)] <- -9999
				cptIn <- list(data = xtmp, date = odaty,  stninfo = headinfo,
								varid = name, units = units, missval = -9999)
				cptOut <- do.call(CPT.convertStationData, cptIn)
			}
			cat(cptOut, file = outfile)
		}else{
			if(GeneralParameters$type.series %in% c('climato', 'anom', 'stanom')){
				outfile <- file.path(dirname(outputFile), paste0("CLM-MEAN_", basename(outputFile)))
				if(cptgrid){
					xtmp0 <- if(is.list(climOUT)) climOUT else list(climOUT)
					xtmp <- lapply(seq_along(climdates), function(j){
						x <- round(xtmp0[[j]], 3)
						x <- x[headinfo$z]
						dim(x) <- dim(headinfo$z)
						x[is.na(x)] <- -9999
						x
					})
					gridinfo <- headinfo[c('x', 'y')]
					cptIn <- list(data = xtmp, date = climdates,  gridinfo = gridinfo,
								varid = name, units = units, missval = -9999)
					cptOut <- do.call(CPT.convertGridData, cptIn)
				}else{
					xtmp <- if(is.list(climOUT)) do.call(rbind, climOUT) else matrix(climOUT, nrow = 1)
					xtmp <- round(xtmp, 3)
					xtmp[is.na(xtmp)] <- -9999
					cptIn <- list(data = xtmp, date = climdates,  stninfo = headinfo,
									varid = name, units = units, missval = -9999)
					cptOut <- do.call(CPT.convertStationData, cptIn)
				}
				cat(cptOut, file = outfile)
			}

			if(GeneralParameters$type.series == 'stanom'){
				outfile <- file.path(dirname(outputFile), paste0("CLM-SD_", basename(outputFile)))
				if(cptgrid){
					xtmp0 <- if(is.list(sdOUT)) sdOUT else list(sdOUT)
					xtmp <- lapply(seq_along(climdates), function(j){
						x <- round(xtmp0[[j]], 3)
						x <- x[headinfo$z]
						dim(x) <- dim(headinfo$z)
						x[is.na(x)] <- -9999
						x
					})
					gridinfo <- headinfo[c('x', 'y')]
					cptIn <- list(data = xtmp, date = climdates,  gridinfo = gridinfo,
								varid = name, units = units, missval = -9999)
					cptOut <- do.call(CPT.convertGridData, cptIn)
				}else{
					xtmp <- if(is.list(sdOUT)) do.call(rbind, sdOUT) else matrix(sdOUT, nrow = 1)
					xtmp <- round(xtmp, 3)
					xtmp[is.na(xtmp)] <- -9999
					cptIn <- list(data = xtmp, date = climdates,  stninfo = headinfo,
									varid = name, units = units, missval = -9999)
					cptOut <- do.call(CPT.convertStationData, cptIn)
				}
				cat(cptOut, file = outfile)
			}

			if(GeneralParameters$type.series %in% c('anom', 'stanom')){
				odir <- if(GeneralParameters$type.series == 'anom') "Anomaly" else "STDAnomaly"
				outfile <- file.path(dirname(outputFile), paste0(odir, "_", basename(outputFile)))
				if(cptgrid){
					xtmp <- lapply(seq_along(odaty), function(j){
						x <- round(outMAT[j, ], 3)
						x <- x[headinfo$z]
						dim(x) <- dim(headinfo$z)
						x[is.na(x)] <- -9999
						x
					})
					gridinfo <- headinfo[c('x', 'y')]
					cptIn <- list(data = xtmp, date = odaty,  gridinfo = gridinfo,
								varid = name, units = units, missval = -9999)
					cptOut <- do.call(CPT.convertGridData, cptIn)
				}else{
					xtmp <- round(outMAT, 3)
					xtmp[is.na(xtmp)] <- -9999
					cptIn <- list(data = xtmp, date = odaty,  stninfo = headinfo,
									varid = name, units = units, missval = -9999)
					cptOut <- do.call(CPT.convertStationData, cptIn)
				}
				cat(cptOut, file = outfile)
			}
		}
	}

	#####################################

	return(0)
}



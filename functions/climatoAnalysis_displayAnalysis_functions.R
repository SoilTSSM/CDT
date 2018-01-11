
climatoAnalysis.plotStatMaps <- function(){
	don <- EnvClimatoAnalysisplot$don
	climMapOp <- EnvClimatoAnalysisplot$climMapOp

	## titre
	if(!climMapOp$title$user){
		params <- EnvClimatoAnalysisplot$statpars$params
		titre1 <- str_to_title(params$time.series$out.series)
		titre2 <- tclvalue(EnvClimatoAnalysisplot$climStat)
		titre3 <- switch(params$analysis.method$mth.fun,
						"percentile" = paste0("(", params$analysis.method$mth.perc, "th", ")"),
						"frequency" = paste0("(", params$analysis.method$low.thres, " < X < ",
												params$analysis.method$up.thres, ")"),
						"trend" = {
							if(params$analysis.method$trend.unit == 1) "per year"
							if(params$analysis.method$trend.unit == 2) "over"
							if(params$analysis.method$trend.unit == 3) "/ average (in %)"
						},
						NULL)
		titre4 <- tclvalue(EnvClimatoAnalysisplot$climDate)
		titre <- paste(titre1, titre2, titre3, titre4)
	}else titre <- climMapOp$title$title

	## colorscale title
	if(climMapOp$colkeyLab$user){
		legend.texta <- climMapOp$colkeyLab$label
	}else legend.texta <- NULL

	## breaks
	if(!climMapOp$userLvl$custom){
		breaks <- pretty(don$z, n = 10, min.n = 5)
		breaks <- if(length(breaks) > 0) breaks else c(0, 1) 
	}else breaks <- climMapOp$userLvl$levels

	## colors
	if(climMapOp$userCol$custom){
		kolFonction <- colorRampPalette(climMapOp$userCol$color)
		kolor <- kolFonction(length(breaks)-1)
	}else{
		kolFonction <- match.fun(climMapOp$presetCol$color)
		kolor <- kolFonction(length(breaks)-1)
		if(climMapOp$presetCol$reverse) kolor <- rev(kolor)
	}

	### shape files
	shpf <- EnvClimatoAnalysisplot$shp
	ocrds <- if(tclvalue(shpf$add.shp) == "1" & !is.null(shpf$ocrds)) shpf$ocrds else matrix(NA, 1, 2)

	#################

	if(all(is.na(ocrds[, 1])) | all(is.na(ocrds[, 2]))){
		xlim <- range(don$x, na.rm = TRUE)
		ylim <- range(don$y, na.rm = TRUE)
	}else{
		xlim <- range(range(don$x, na.rm = TRUE), range(ocrds[, 1], na.rm = TRUE))
		ylim <- range(range(don$y, na.rm = TRUE), range(ocrds[, 2], na.rm = TRUE))
	}

	if(diff(xlim) > diff(ylim)){
		horizontal <- TRUE
		legend.mar <- 3.5
		legend.width <- 0.7
		mar <- c(7, 4, 2.5, 2.5)
		legend.args <- if(!is.null(legend.texta)) list(text = legend.texta, cex = 0.8, side = 1, line = 2) else NULL
	}else{
		horizontal <- FALSE
		legend.mar <- 6.2
		mar <- c(4, 4, 2.5, 6)
		legend.width <- 0.9
		line <- if(max(nchar(as.character(breaks))) > 4) 3 else 2
		legend.args <- if(!is.null(legend.texta)) list(text = legend.texta, cex = 0.8, side = 4, line = line) else NULL
	}

	legendLabel <- breaks

	#################

	opar <- par(mar = mar)
	plot(1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
	axlabsFun <- if(Sys.info()["sysname"] == "Windows") LatLonAxisLabels else LatLonAxisLabels1
	axlabs <- axlabsFun(axTicks(1), axTicks(2))
	axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tcl = -0.2, cex.axis = 0.8)
	axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tcl = -0.2, las = 1, cex.axis = 0.8)
	title(main = titre, cex.main = 1, font.main= 2)

	# if(length(xna) > 0) points(xna, yna, pch = '*')

	if(climMapOp$userLvl$equidist){
		image(don, breaks = breaks, col = kolor, xaxt = 'n', yaxt = 'n', add = TRUE)
		breaks1 <- seq(0, 1, length.out = length(breaks))
		image.plot(zlim = c(0, 1), breaks = breaks1, col = kolor, horizontal = horizontal,
					legend.only = TRUE, legend.mar = legend.mar, legend.width = legend.width,
					legend.args = legend.args, axis.args = list(at = breaks1, labels = legendLabel,
					cex.axis = 0.7, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)))
	}else{
		image.plot(don, breaks = breaks, col = kolor, horizontal = horizontal,
					xaxt = 'n', yaxt = 'n', add = TRUE, legend.mar = legend.mar,
					legend.width = legend.width, legend.args = legend.args,
					axis.args = list(at = breaks, labels = legendLabel, cex.axis = 0.7,
					font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)))
	}

	if(tclvalue(EnvClimatoAnalysisplot$climStat) == "Trend"){
		if(EnvClimatoAnalysisplot$statpars$params$data.type == "cdtstation"){
			ipvl <- !is.na(don$p.value) & don$p.value < 0.05
			if(any(ipvl)){
				# points(don$x0[ipvl], don$y0[ipvl], col = adjustcolor('gray40', alpha.f = 0.8))
				points(don$x0[ipvl], don$y0[ipvl], pch = 19, cex = 0.5)
			}
		}else{
			ipvl <- c(don$pval)
			ipvl <- !is.na(ipvl) & ipvl < 0.05
			if(any(ipvl)){
				grd <- don$x[2]-don$x[1]
				dd <- expand.grid(x = don$x, y = don$y)
				coordinates(dd) <- ~x+y
				dd <- dd[ipvl, ]
				buffer <- gBuffer(dd, width = grd*1.02)

				# # plot(buffer, col = adjustcolor('gray40', alpha.f = 0.8), density = 20, angle = 45, border = NA, lty = 1, fillOddEven = TRUE, add = TRUE)
				# # plot(buffer, col = adjustcolor('gray40', alpha.f = 0.8), density = 20, angle = -45, border = NA, lty = 1, fillOddEven = TRUE, add = TRUE)
				# plot(buffer, col = 'gray30', density = 20, angle = 45, border = NA, lty = 1, fillOddEven = TRUE, add = TRUE)
				# plot(buffer, col = 'gray30', density = 20, angle = -45, border = NA, lty = 1, fillOddEven = TRUE, add = TRUE)

				dd <- disaggregate(buffer)
				centr <- coordinates(dd)
				bbx <- lapply(seq_along(dd), function(i) bbox(dd[i]))
				esp <- if(grd > 0.25) 0.25 else grd*5
				esp <- if(esp > 0.25) 0.25 else esp
				dd <- lapply(seq_along(bbx), function(i){	
					xpt <- c(rev(seq(centr[i, 1], bbx[[i]][1, 1], -esp)[-1]), seq(centr[i, 1], bbx[[i]][1, 2], esp))
					ypt <- c(rev(seq(centr[i, 2], bbx[[i]][2, 1], -esp)[-1]), seq(centr[i, 2], bbx[[i]][2, 2], esp))
					xy <- expand.grid(x = xpt, y = ypt)
					coordinates(xy) <- ~x+y
					ij <- as.logical(over(xy, dd[i]))
					ij[is.na(ij)] <- FALSE
					coordinates(xy[ij, ])
				})
				dd <- do.call(rbind, dd)
				# points(dd[, 1], dd[, 2], pch = 15, cex = 0.3, col = adjustcolor('gray20', alpha.f = 0.9))
				points(dd[, 1], dd[, 2], pch = 15, cex = 0.3)
			}
		}
	}

	abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)

	lines(ocrds[, 1], ocrds[, 2], lwd = EnvClimatoAnalysisplot$SHPOp$lwd, col = EnvClimatoAnalysisplot$SHPOp$col)
	
	## scale bar
	if(climMapOp$scalebar$add){
		if(climMapOp$scalebar$pos == 'bottomleft') posx <- 0.05
		if(climMapOp$scalebar$pos == 'bottomcenter') posx <- 0.425
		if(climMapOp$scalebar$pos == 'bottomright') posx <- 0.75
		posy <- 0.08

		scalebarX <- grconvertX(posx, "npc")
		scalebarY <- grconvertY(posy, "npc")

		map.scale(x = scalebarX, y = scalebarY, relwidth = 0.15, metric = TRUE, ratio = FALSE, cex = 0.7, font = 2)
	}

	plt <- par("plt")
	usr <- par("usr")
	par(opar)
	return(list(par = c(plt, usr)))
}

#######################################

climatoAnalysis.plotTSMaps <- function(){
	TSMapOp <- EnvClimatoAnalysisplot$TSMapOp

	if(tclvalue(EnvClimatoAnalysisplot$TSData) == "Data")
		don <- EnvClimatoAnalysisplot$tsdata
	if(tclvalue(EnvClimatoAnalysisplot$TSData) == "Anomaly")
		don <- EnvClimatoAnalysisplot$anomData

	if(!TSMapOp$title$user){
		if(tclvalue(EnvClimatoAnalysisplot$TSData) == "Data"){
			params <- EnvClimatoAnalysisplot$statpars$params
			titre1 <- str_to_title(params$time.series$out.series)
			titre2 <- switch(params$aggr.series$aggr.fun, "sum" = "total", "mean" = "average", "count" = "number")
			titre3 <- if(params$aggr.series$aggr.fun == "count")
							paste("(", params$aggr.series$opr.fun, params$aggr.series$opr.thres, ")") else NULL
			titre4 <- tclvalue(EnvClimatoAnalysisplot$TSDate)
			titre <- paste(titre1, titre2, titre3, titre4)
		}

		if(tclvalue(EnvClimatoAnalysisplot$TSData) == "Anomaly"){
			params <- don$params
			titre1 <- str_to_title(params$time.series$out.series)
			titre2 <- "anomaly"
			titre3 <- if(params$analysis.method$perc.anom) "% of mean" else NULL
			titre4 <- tclvalue(EnvClimatoAnalysisplot$TSDate)
			titre <- paste(titre1, titre2, titre3, titre4)
		}
	}else titre <- TSMapOp$title$title

	## colorscale title
	if(TSMapOp$colkeyLab$user){
		legend.texta <- TSMapOp$colkeyLab$label
	}else legend.texta <- NULL

	## breaks
	if(!TSMapOp$userLvl$custom){
		breaks <- pretty(don$z, n = 10, min.n = 5)
		breaks <- if(length(breaks) > 0) breaks else c(0, 1) 
	}else breaks <- TSMapOp$userLvl$levels

	## colors
	if(TSMapOp$userCol$custom){
		kolFonction <- colorRampPalette(TSMapOp$userCol$color)
		kolor <- kolFonction(length(breaks)-1)
	}else{
		kolFonction <- match.fun(TSMapOp$presetCol$color)
		kolor <- kolFonction(length(breaks)-1)
		if(TSMapOp$presetCol$reverse) kolor <- rev(kolor)
	}

	### shape files
	shpf <- EnvClimatoAnalysisplot$shp
	ocrds <- if(tclvalue(shpf$add.shp) == "1" & !is.null(shpf$ocrds)) shpf$ocrds else matrix(NA, 1, 2)

	#################

	if(all(is.na(ocrds[, 1])) | all(is.na(ocrds[, 2]))){
		xlim <- range(don$x, na.rm = TRUE)
		ylim <- range(don$y, na.rm = TRUE)
	}else{
		xlim <- range(range(don$x, na.rm = TRUE), range(ocrds[, 1], na.rm = TRUE))
		ylim <- range(range(don$y, na.rm = TRUE), range(ocrds[, 2], na.rm = TRUE))
	}

	if(diff(xlim) > diff(ylim)){
		horizontal <- TRUE
		legend.mar <- 3.5
		legend.width <- 0.7
		mar <- c(7, 4, 2.5, 2.5)
		legend.args <- if(!is.null(legend.texta)) list(text = legend.texta, cex = 0.8, side = 1, line = 2) else NULL
	}else{
		horizontal <- FALSE
		legend.mar <- 6.2
		mar <- c(4, 4, 2.5, 6)
		legend.width <- 0.9
		line <- if(max(nchar(as.character(breaks))) > 4) 3 else 2
		legend.args <- if(!is.null(legend.texta)) list(text = legend.texta, cex = 0.8, side = 4, line = line) else NULL
	}

	legendLabel <- breaks

	#################

	opar <- par(mar = mar)
	plot(1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
	axlabsFun <- if(Sys.info()["sysname"] == "Windows") LatLonAxisLabels else LatLonAxisLabels1
	axlabs <- axlabsFun(axTicks(1), axTicks(2))
	axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tcl = -0.2, cex.axis = 0.8)
	axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tcl = -0.2, las = 1, cex.axis = 0.8)
	title(main = titre, cex.main = 1, font.main= 2)

	# if(length(xna) > 0) points(xna, yna, pch = '*')

	if(TSMapOp$userLvl$equidist){
		image(don, breaks = breaks, col = kolor, xaxt = 'n', yaxt = 'n', add = TRUE)
		breaks1 <- seq(0, 1, length.out = length(breaks))
		image.plot(zlim = c(0, 1), breaks = breaks1, col = kolor, horizontal = horizontal,
					legend.only = TRUE, legend.mar = legend.mar, legend.width = legend.width,
					legend.args = legend.args, axis.args = list(at = breaks1, labels = legendLabel,
					cex.axis = 0.7, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)))
	}else{
		image.plot(don, breaks = breaks, col = kolor, horizontal = horizontal,
					xaxt = 'n', yaxt = 'n', add = TRUE, legend.mar = legend.mar,
					legend.width = legend.width, legend.args = legend.args,
					axis.args = list(at = breaks, labels = legendLabel, cex.axis = 0.7,
					font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)))
	}

	abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)

	lines(ocrds[, 1], ocrds[, 2], lwd = EnvClimatoAnalysisplot$SHPOp$lwd, col = EnvClimatoAnalysisplot$SHPOp$col)

	## scale bar
	if(TSMapOp$scalebar$add){
		if(TSMapOp$scalebar$pos == 'bottomleft') posx <- 0.05
		if(TSMapOp$scalebar$pos == 'bottomcenter') posx <- 0.425
		if(TSMapOp$scalebar$pos == 'bottomright') posx <- 0.75
		posy <- 0.08

		scalebarX <- grconvertX(posx, "npc")
		scalebarY <- grconvertY(posy, "npc")
		map.scale(x = scalebarX, y = scalebarY, relwidth = 0.15, metric = TRUE, ratio = FALSE, cex = 0.7, font = 2)
	}

	plt <- par("plt")
	usr <- par("usr")
	par(opar)
	return(list(par = c(plt, usr)))
}

#######################################

climatoAnalysis.plotTSGraph <- function(){
	TSGraphOp <- EnvClimatoAnalysisplot$TSGraphOp

	if(EnvClimatoAnalysisplot$statpars$params$data.type == "cdtstation"){
		ixy <- which(EnvClimatoAnalysisplot$tsdata$id == str_trim(tclvalue(EnvClimatoAnalysisplot$graph$stnIDTSp)))
		if(length(ixy) == 0){
			InsertMessagesTxt(main.txt.out, "Station not found", format = TRUE)
			return(NULL)
		}
		don <- EnvClimatoAnalysisplot$tsdata$data[, ixy]
		dates <- EnvClimatoAnalysisplot$tsdata$date
		daty <- as.numeric(substr(dates, 1, 4))
		EnvClimatoAnalysisplot$location <- paste0("Station: ", EnvClimatoAnalysisplot$tsdata$id[ixy])
	}else{
		cdtdataset <- EnvClimatoAnalysisplot$cdtdataset
		xlon <- cdtdataset$coords$mat$x
		xlat <- cdtdataset$coords$mat$y
		# xlon <- sort(unique(cdtdataset$coords$df$x))
		# xlat <- sort(unique(cdtdataset$coords$df$y))
		ilon <- as.numeric(str_trim(tclvalue(EnvClimatoAnalysisplot$graph$lonLOC)))
		ilat <- as.numeric(str_trim(tclvalue(EnvClimatoAnalysisplot$graph$latLOC)))

		iclo <- findInterval(ilon, xlon)
		ilo <- iclo + (2 * ilon > xlon[iclo] + xlon[iclo+1])
		icla <- findInterval(ilat, xlat)
		ila <- icla + (2 * ilat > xlat[icla] + xlat[icla+1])

		if(is.na(ilo) | is.na(ila)){
			InsertMessagesTxt(main.txt.out, "Coordinates outside of data range", format = TRUE)
			return(NULL)
		}
		ixy <- ilo + length(xlon) * (ila-1)

		don <- readCdtDatasetChunk.locations(ixy, cdtdataset$fileInfo, cdtdataset, do.par = FALSE)
		don <- as.numeric(don$data[, 1])
		dates <- cdtdataset$dateInfo$date

		######
		year1 <- substr(dates, 1, 4) 
		mon1 <- substr(dates, 6, 7)
		year2 <- substr(dates, 9, 12)
		mon2 <- substr(dates, 14, 15)
		if(year1 == year2){
			if(mon1 == mon2) dateTS <- paste0(year1, mon1)
			else{
				dateTS <- if(mon1 == "01" & mon2 == "12") year1 else dates
			}
		}else dateTS <- dates
		ipos <- which(EnvClimatoAnalysisplot$statpars$stats == tclvalue(EnvClimatoAnalysisplot$climDate))
		idaty <- dateTS%in%EnvClimatoAnalysisplot$statpars$timeseries[[ipos]][[2]]
		dates <- dateTS[idaty]
		don <- don[idaty]

		daty <- as.numeric(substr(dates, 1, 4))
		EnvClimatoAnalysisplot$location <- paste0("Longitude: ", round(ilon, 5), ", Latitude: ", round(ilat, 5))
	}

	#########
	GRAPHTYPE <- str_trim(tclvalue(EnvClimatoAnalysisplot$graph$typeTSp))

	if(GRAPHTYPE == "Line"){
		optsgph <- TSGraphOp$line
		xlim <- range(daty, na.rm = TRUE)
		if(optsgph$xlim$is.min) xlim[1] <- optsgph$xlim$min
		if(optsgph$xlim$is.max) xlim[2] <- optsgph$xlim$max
		ylim <- range(pretty(don))
		if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
		if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

		xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else ''
		ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else ''

		if(optsgph$title$is.title){
			titre <- optsgph$title$title
			titre.pos <- optsgph$title$position
		}else{
			titre <- ""
			titre.pos <- "top"
		}

		legends <- NULL
		if(optsgph$legend$is$mean){
			legends$add$mean <- optsgph$legend$add$mean
			legends$col$mean <- optsgph$legend$col$mean
			legends$text$mean <- optsgph$legend$text$mean
			legends$lwd$mean <- optsgph$legend$lwd$mean
		}else{
			if(tclvalue(EnvClimatoAnalysisplot$graph$averageTSp) == "1") legends$add$mean <- TRUE
		}
		if(optsgph$legend$is$linear){
			legends$add$linear <- optsgph$legend$add$linear
			legends$col$linear <- optsgph$legend$col$linear
			legends$text$linear <- optsgph$legend$text$linear
			legends$lwd$linear <- optsgph$legend$lwd$linear
		}else{
			if(tclvalue(EnvClimatoAnalysisplot$graph$trendTSp) == "1") legends$add$linear <- TRUE
		}
		if(optsgph$legend$is$tercile){
			legends$add$tercile <- optsgph$legend$add$tercile
			legends$col$tercile1 <- optsgph$legend$col$tercile1
			legends$text$tercile1 <- optsgph$legend$text$tercile1
			legends$col$tercile2 <- optsgph$legend$col$tercile2
			legends$text$tercile2 <- optsgph$legend$text$tercile2
			legends$lwd$tercile <- optsgph$legend$lwd$tercile
		}else{
			if(tclvalue(EnvClimatoAnalysisplot$graph$tercileTSp) == "1") legends$add$tercile <- TRUE
		}

		climatoAnalysis.plot.line(daty, don, xlim = xlim, ylim = ylim,
									xlab = xlab, ylab = ylab, ylab.sub = NULL,
									title = titre, title.position = titre.pos, axis.font = 1,
									plotl = optsgph$plot, legends = legends,
									location = EnvClimatoAnalysisplot$location)
	}

	if(GRAPHTYPE == "Barplot"){
		optsgph <- TSGraphOp$bar
		xlim <- range(daty, na.rm = TRUE)
		if(optsgph$xlim$is.min) xlim[1] <- optsgph$xlim$min
		if(optsgph$xlim$is.max) xlim[2] <- optsgph$xlim$max
		ylim <- range(pretty(don))
		if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
		if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

		xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else ''
		ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else ''

		if(optsgph$title$is.title){
			titre <- optsgph$title$title
			titre.pos <- optsgph$title$position
		}else{
			titre <- ""
			titre.pos <- "top"
		}

		climatoAnalysis.plot.bar(daty, don, xlim = xlim, ylim = ylim,
								xlab = xlab, ylab = ylab, ylab.sub = NULL,
								title = titre, title.position = titre.pos, axis.font = 1,
								barcol = optsgph$colors$col,
								location = EnvClimatoAnalysisplot$location)
	}

	if(GRAPHTYPE == "Probability"){
		optsgph <- TSGraphOp$proba
		xlim <- range(don, na.rm = TRUE)
		if(optsgph$xlim$is.min) xlim[1] <- optsgph$xlim$min
		if(optsgph$xlim$is.max) xlim[2] <- optsgph$xlim$max
		ylim <- c(0, 100)
		if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
		if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

		xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else ''
		ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else "Probability of Exceeding"

		if(optsgph$title$is.title){
			titre <- optsgph$title$title
			titre.pos <- optsgph$title$position
		}else{
			titre <- ""
			titre.pos <- "top"
		}

		climatoAnalysis.plot.proba(don, xlim = xlim, ylim = ylim,
									xlab = xlab, xlab.sub = NULL, ylab = ylab,
									title = titre, title.position = titre.pos, axis.font = 1,
									proba = list(theoretical = optsgph$proba$theoretical),
									plotp = optsgph$proba, plotl = optsgph$plot,
									location = EnvClimatoAnalysisplot$location)
	}

	####
	if(GRAPHTYPE%in%c("ENSO-Line", "ENSO-Barplot", "ENSO-Proba")){
		if(nchar(dates[1]) == 4){
			start.mon <- paste0(dates, "0115")
			end.mon <- paste0(dates, "1215")
		}
		if(nchar(dates[1]) == 6){
			start.mon <- paste0(dates, "15")
			end.mon <- paste0(dates, "15")
		}
		if(nchar(dates[1]) == 15){
			dates <- lapply(strsplit(dates, '_'), function(x) format(as.Date(paste0(x, "-15")), "%Y%m%d"))
			start.mon <- sapply(dates, '[[', 1)
			end.mon <- sapply(dates, '[[', 2)
		}

		ijoni <- getIndexSeasonVars(start.mon, end.mon, EnvClimatoAnalysisplot$ONI$date, "monthly")
		oni <- sapply(ijoni, function(x) mean(EnvClimatoAnalysisplot$ONI$data[x], na.rm = TRUE))
		oni <- ifelse(oni >= 0.5, 3, ifelse(oni <= -0.5, 1, 2))
	}

	if(GRAPHTYPE == "ENSO-Line"){
		optsgph <- TSGraphOp$line.enso
		xlim <- range(daty, na.rm = TRUE)
		if(optsgph$xlim$is.min) xlim[1] <- optsgph$xlim$min
		if(optsgph$xlim$is.max) xlim[2] <- optsgph$xlim$max
		ylim <- range(pretty(don))
		if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
		if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

		xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else ''
		ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else ''

		if(optsgph$title$is.title){
			titre <- optsgph$title$title
			titre.pos <- optsgph$title$position
		}else{
			titre <- ""
			titre.pos <- "top"
		}

		legends <- NULL
		if(optsgph$legend$is$mean){
			legends$add$mean <- optsgph$legend$add$mean
			legends$col$mean <- optsgph$legend$col$mean
			legends$text$mean <- optsgph$legend$text$mean
			legends$lwd$mean <- optsgph$legend$lwd$mean
		}else{
			if(tclvalue(EnvClimatoAnalysisplot$graph$averageTSp) == "1") legends$add$mean <- TRUE
		}
		if(optsgph$legend$is$linear){
			legends$add$linear <- optsgph$legend$add$linear
			legends$col$linear <- optsgph$legend$col$linear
			legends$text$linear <- optsgph$legend$text$linear
			legends$lwd$linear <- optsgph$legend$lwd$linear
		}else{
			if(tclvalue(EnvClimatoAnalysisplot$graph$trendTSp) == "1") legends$add$linear <- TRUE
		}
		if(optsgph$legend$is$tercile){
			legends$add$tercile <- optsgph$legend$add$tercile
			legends$col$tercile1 <- optsgph$legend$col$tercile1
			legends$text$tercile1 <- optsgph$legend$text$tercile1
			legends$col$tercile2 <- optsgph$legend$col$tercile2
			legends$text$tercile2 <- optsgph$legend$text$tercile2
			legends$lwd$tercile <- optsgph$legend$lwd$tercile
		}else{
			if(tclvalue(EnvClimatoAnalysisplot$graph$tercileTSp) == "1") legends$add$tercile <- TRUE
		}

		climatoAnalysis.plot.line.ENSO(daty, don, oni, xlim = xlim, ylim = ylim,
										xlab = xlab, ylab = ylab, ylab.sub = NULL,
										title = titre, title.position = titre.pos, axis.font = 1,
										plotl = optsgph$plot, legends = legends,
										location = EnvClimatoAnalysisplot$location)
	}

	if(GRAPHTYPE == "ENSO-Barplot"){
		optsgph <- TSGraphOp$bar.enso
		xlim <- range(daty, na.rm = TRUE)
		if(optsgph$xlim$is.min) xlim[1] <- optsgph$xlim$min
		if(optsgph$xlim$is.max) xlim[2] <- optsgph$xlim$max
		ylim <- range(pretty(don))
		if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
		if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

		xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else ''
		ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else ''

		if(optsgph$title$is.title){
			titre <- optsgph$title$title
			titre.pos <- optsgph$title$position
		}else{
			titre <- ""
			titre.pos <- "top"
		}

		climatoAnalysis.plot.bar.ENSO(daty, don, oni, xlim = xlim, ylim = ylim,
									xlab = xlab, ylab = ylab, ylab.sub = NULL,
									title = titre, title.position = titre.pos, axis.font = 1,
									barcol = optsgph$colors$col, location = EnvClimatoAnalysisplot$location)
	}

	if(GRAPHTYPE == "ENSO-Proba"){
		optsgph <- TSGraphOp$proba.enso
		xlim <- range(don, na.rm = TRUE)
		if(optsgph$xlim$is.min) xlim[1] <- optsgph$xlim$min
		if(optsgph$xlim$is.max) xlim[2] <- optsgph$xlim$max
		ylim <- c(0, 100)
		if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
		if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

		xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else ''
		ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else "Probability of Exceeding"

		if(optsgph$title$is.title){
			titre <- optsgph$title$title
			titre.pos <- optsgph$title$position
		}else{
			titre <- ""
			titre.pos <- "top"
		}

		climatoAnalysis.plot.proba.ENSO(don, oni, xlim = xlim, ylim = ylim,
										xlab = xlab, xlab.sub = NULL, ylab = ylab,
 										title = titre, title.position = titre.pos, axis.font = 1,
 										plotl = optsgph$plot, location = EnvClimatoAnalysisplot$location)
	}

	if(GRAPHTYPE == "Anomaly"){
		optsgph <- TSGraphOp$anomaly
		xlim <- range(daty, na.rm = TRUE)
		if(optsgph$xlim$is.min) xlim[1] <- optsgph$xlim$min
		if(optsgph$xlim$is.max) xlim[2] <- optsgph$xlim$max
		ylim <- c(-100, 100)
		if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
		if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max
		if(!optsgph$ylim$is.min & !optsgph$ylim$is.max) ylim <- NULL

		percent <- optsgph$anom$perc.anom
		xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else ''
		ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else {if(percent) "Anomaly (% of Mean)" else ""}

		if(optsgph$title$is.title){
			titre <- optsgph$title$title
			titre.pos <- optsgph$title$position
		}else{
			titre <- ""
			titre.pos <- "top"
		}

		loko <- c(optsgph$colors$negative, optsgph$colors$positive)

		period <- range(daty, na.rm = TRUE)
		if(optsgph$anom$basePeriod){
			startYr <- optsgph$anom$startYr.anom
			endYr <- optsgph$anom$endYr.anom
			period <- c(startYr, endYr)
		}

		climatoAnalysis.plot.bar.Anomaly(daty, don, period = period, percent = percent,
										xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ylab.sub = NULL,
										title = titre, title.position = titre.pos, axis.font = 1,
										barcol = loko, location = EnvClimatoAnalysisplot$location)
	}
}

##############################################################################

climatoAnalysis.DisplayStatMaps <- function(parent){
	varplot <- c("parPlotSize1", "parPlotSize2", "parPlotSize3", "parPlotSize4",
				 "usrCoords1", "usrCoords2", "usrCoords3", "usrCoords4")
	parPltCrd <- setNames(lapply(varplot, function(x) assign(x, tclVar(), env = parent.frame())), varplot)

	plotIt <- function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		op <- par(bg = "white")
		pltusr <- climatoAnalysis.plotStatMaps()
		par(op)
		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvClimatoAnalysisplot$notebookTab.climMap, 'Clim-Analysis-Maps', AllOpenTabType, AllOpenTabData)
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	#########
	tkbind(img, "<Motion>", function(W, x, y){
		if(length(ls(EnvClimatoAnalysisplot)) == 0) return(NULL)
		if(is.null(EnvClimatoAnalysisplot$statpars)) return(NULL)

		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		xydisp <- LatLonLabels(xyMouse$x, xyMouse$y)
		frxcoord <- ifelse(xyMouse$inout, '', xydisp$xdisp)
		frycoord <- ifelse(xyMouse$inout, '', xydisp$ydisp)

		if(EnvClimatoAnalysisplot$statpars$params$data.type == "cdtstation"){
			fdispIdStn <- function(x){
				y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvClimatoAnalysisplot$don$x0)^2 + (xyMouse$y-EnvClimatoAnalysisplot$don$y0)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			frzcoord <- ifelse(xyMouse$inout | rayondisp, '', EnvClimatoAnalysisplot$don$id[inear])
		}else{
			frzcoord <- ""
		}

		tclvalue(xpcoord) <- frxcoord
		tclvalue(ypcoord) <- frycoord
		tclvalue(zpcoord) <- frzcoord
	})

	tkbind(img, "<Button-1>", function(W, x, y){
		if(length(ls(EnvClimatoAnalysisplot)) == 0) return(NULL)
		if(is.null(EnvClimatoAnalysisplot$statpars)) return(NULL)

		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		if(EnvClimatoAnalysisplot$statpars$params$data.type == "cdtstation"){
			fdispIdStn <- function(x){
				 y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvClimatoAnalysisplot$don$x0)^2 + (xyMouse$y-EnvClimatoAnalysisplot$don$y0)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			if(!(xyMouse$inout | rayondisp)){
				tclvalue(EnvClimatoAnalysisplot$graph$stnIDTSp) <- EnvClimatoAnalysisplot$don$id[inear]
				plotTS <- TRUE
			}else plotTS <- FALSE
		}else{
			if(!xyMouse$inout){
				tclvalue(EnvClimatoAnalysisplot$graph$lonLOC) <- round(xyMouse$x, 6)
				tclvalue(EnvClimatoAnalysisplot$graph$latLOC) <- round(xyMouse$y, 6)
				plotTS <- TRUE
			}else plotTS <- FALSE
		}

		if(plotTS){
			imgContainer <- climatoAnalysis.DisplayTSPlot(tknotes)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvClimatoAnalysisplot$notebookTab.tsplot, AllOpenTabType, AllOpenTabData)
			EnvClimatoAnalysisplot$notebookTab.tsplot <- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}

#######################################

climatoAnalysis.DisplayTSMaps <- function(parent){
	varplot <- c("parPlotSize1", "parPlotSize2", "parPlotSize3", "parPlotSize4",
				 "usrCoords1", "usrCoords2", "usrCoords3", "usrCoords4")
	parPltCrd <- setNames(lapply(varplot, function(x) assign(x, tclVar(), env = parent.frame())), varplot)

	plotIt <- function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		op <- par(bg = "white")
		pltusr <- climatoAnalysis.plotTSMaps()
		par(op)
		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvClimatoAnalysisplot$notebookTab.TSMap, 'Aggregated-Data', AllOpenTabType, AllOpenTabData)
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	#########
	tkbind(img, "<Motion>", function(W, x, y){
		if(length(ls(EnvClimatoAnalysisplot)) == 0) return(NULL)
		if(is.null(EnvClimatoAnalysisplot$statpars)) return(NULL)

		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		xydisp <- LatLonLabels(xyMouse$x, xyMouse$y)
		frxcoord <- ifelse(xyMouse$inout, '', xydisp$xdisp)
		frycoord <- ifelse(xyMouse$inout, '', xydisp$ydisp)

		if(EnvClimatoAnalysisplot$statpars$params$data.type == "cdtstation"){
			fdispIdStn <- function(x){
				y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvClimatoAnalysisplot$tsdata$x0)^2 + (xyMouse$y-EnvClimatoAnalysisplot$tsdata$y0)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			frzcoord <- ifelse(xyMouse$inout | rayondisp, '', EnvClimatoAnalysisplot$tsdata$id[inear])
		}else{
			frzcoord <- ""
		}

		tclvalue(xpcoord) <- frxcoord
		tclvalue(ypcoord) <- frycoord
		tclvalue(zpcoord) <- frzcoord
	})

	tkbind(img, "<Button-1>", function(W, x, y){
		if(length(ls(EnvClimatoAnalysisplot)) == 0) return(NULL)
		if(is.null(EnvClimatoAnalysisplot$statpars)) return(NULL)

		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		if(EnvClimatoAnalysisplot$statpars$params$data.type == "cdtstation"){
			fdispIdStn <- function(x){
				 y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvClimatoAnalysisplot$tsdata$x0)^2 + (xyMouse$y-EnvClimatoAnalysisplot$tsdata$y0)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			if(!(xyMouse$inout | rayondisp)){
				tclvalue(EnvClimatoAnalysisplot$graph$stnIDTSp) <- EnvClimatoAnalysisplot$tsdata$id[inear]
				plotTS <- TRUE
			}else plotTS <- FALSE
		}else{
			if(!xyMouse$inout){
				tclvalue(EnvClimatoAnalysisplot$graph$lonLOC) <- round(xyMouse$x, 6)
				tclvalue(EnvClimatoAnalysisplot$graph$latLOC) <- round(xyMouse$y, 6)
				plotTS <- TRUE
			}else plotTS <- FALSE
		}

		if(plotTS){
			imgContainer <- climatoAnalysis.DisplayTSPlot(tknotes)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvClimatoAnalysisplot$notebookTab.tsplot, AllOpenTabType, AllOpenTabData)
			EnvClimatoAnalysisplot$notebookTab.tsplot <- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}

################################

climatoAnalysis.DisplayTSPlot <- function(parent){
	plotIt <- function(){
		climatoAnalysis.plotTSGraph()
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvClimatoAnalysisplot$notebookTab.tsplot, 'Time-Series-Plot', AllOpenTabType, AllOpenTabData)
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))
	hscrFrame <- as.integer(tclvalue(tkwinfo("height", panel.right)))
	wscrFrame <- as.integer(tclvalue(tkwinfo("width", panel.right)))

	scrollwin <- bwScrolledWindow(onglet[[2]])
	tkgrid(scrollwin)
	tkgrid.rowconfigure(scrollwin, 0, weight = 1)
	tkgrid.columnconfigure(scrollwin, 0, weight = 1)
	containerFrame <- bwScrollableFrame(scrollwin, width = wscrFrame, height = hscrFrame)

	img <- tkrplot(containerFrame, fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	return(list(onglet, img))
}




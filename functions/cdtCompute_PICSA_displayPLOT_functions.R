
PICSA.plot.TSMaps <- function(){
	TSMapOp <- EnvPICSAplot$TSMapOp
	don <- EnvPICSAplot$tsdata

	if(!TSMapOp$title$user){
		titre <- switch(str_trim(tclvalue(EnvPICSAplot$varPICSA)),
						"Onset" = "Starting dates of the rainy season",
						"Cessation" = "Ending dates of the rainy season",
						"Season Length" = "Length of the rainy season",
						"Seasonal Rainfall Amounts" = "Seasonal rainfall amounts",
						"Longest Dry Spell" = "Longest dry spell",
						"Number of rain day" = "Seasonal number of rainy days",
						"Maximum daily rain" = 'Seasonal maximum of daily rainfall',
						"Total rain when RR>95thPerc" = 'Seasonal total of precipitation when RR > 95th percentile',
						"Nb of day when RR>95thPerc" = 'Seasonal count of days when RR > 95th percentile',
						"Dry Spells" = {
							drydef <- str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.dryspell)))
							paste0("Dry spells - ", drydef, " or more consecutive days")
							})
	}else titre <- TSMapOp$title$title

	## colorscale title
	if(TSMapOp$colkeyLab$user){
		legend.texta <- TSMapOp$colkeyLab$label
	}else{
		legend.texta <- switch(str_trim(tclvalue(EnvPICSAplot$varPICSA)),
							"Onset" = NULL,
							"Cessation" = NULL,
							"Season Length" = 'Number of Days',
							"Seasonal Rainfall Amounts" = 'Rainfall Amount (mm)',
							"Longest Dry Spell" = 'Number of Days',
							"Number of rain day" = 'Number of Days',
							"Maximum daily rain" = 'Rainfall Depth (mm)',
							"Total rain when RR>95thPerc" = 'Rainfall Amount (mm)',
							"Nb of day when RR>95thPerc" = 'Number of Days',
							"Dry Spells" = 'Number of Dry Spells')
	}

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
	shpf <- EnvPICSAplot$shp
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

	if(str_trim(tclvalue(EnvPICSAplot$varPICSA))%in%c("Onset", "Cessation")){
		start.date <- format(EnvPICSAplot$output$start.date, '%Y%m%d')
		start.dateYear <- format(EnvPICSAplot$output$start.date, '%Y')
		odaty <- start.date[start.dateYear == str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.year)))]
		odaty <- as.character(as.Date(odaty, '%Y%m%d'))
		legendLabel <- format(as.Date(breaks, origin = odaty), '%d-%b')
	}else legendLabel <- breaks

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

	lines(ocrds[, 1], ocrds[, 2], lwd = EnvPICSAplot$SHPOp$lwd, col = EnvPICSAplot$SHPOp$col)

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

PICSA.plot.ClimMaps <- function(){
	don <- EnvPICSAplot$climdata
	climMapOp <- EnvPICSAplot$climMapOp
	StatOp <- str_trim(tclvalue(EnvPICSAplot$analysis.method))

	## titre
	if(!climMapOp$title$user){
		titre <- switch(str_trim(tclvalue(EnvPICSAplot$varPICSA)),
						"Onset" = "Starting dates of the rainy season",
						"Cessation" = "Ending dates of the rainy season",
						"Season Length" = "Length of the rainy season",
						"Seasonal Rainfall Amounts" = "Seasonal rainfall amounts",
						"Longest Dry Spell" = "Longest dry spell",
						"Number of rain day" = "Seasonal number of rainy days",
						"Maximum daily rain" = 'Seasonal maximum of daily rainfall',
						"Total rain when RR>95thPerc" = 'Seasonal total of precipitation when RR > 95th percentile',
						"Nb of day when RR>95thPerc" = 'Seasonal count of days when RR > 95th percentile',
						"Dry Spells" = {
							drydef <- str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.dryspell)))
							paste0("Dry spells - ", drydef, " or more consecutive days")
							})
	}else titre <- climMapOp$title$title

	## colorscale title
	if(climMapOp$colkeyLab$user){
		legend.texta <- climMapOp$colkeyLab$label
		if(str_trim(legend.texta) == "") legend.texta <- NULL
	}else{
		start.dateYear <- as.numeric(format(EnvPICSAplot$output$start.date, '%Y'))
		utrnd <- (diff(range(start.dateYear, na.rm = TRUE))+1)
		uu <- TRUE
		if(str_trim(tclvalue(EnvPICSAplot$trend)) == "Change (trend) / year") utrnd <- "/ year"
		if(str_trim(tclvalue(EnvPICSAplot$trend)) == "Change (trend) over the period") utrnd <- paste("over", utrnd, "years")
		if(str_trim(tclvalue(EnvPICSAplot$trend)) == "Change (trend) / average (in %)"){
			 utrnd <- "change / average (in %)"
			 uu <- FALSE
		}
		dryUn <- "Number of Dry Spells"

		legUnit <- switch(str_trim(tclvalue(EnvPICSAplot$varPICSA)),
					"Onset" = list(NULL, NULL, "days", NULL, "count", if(uu) paste("days", utrnd) else utrnd),
					"Cessation" = list(NULL, NULL, "days", NULL, "count", if(uu) paste("days", utrnd) else utrnd),
					"Season Length" = list("days", "days", "days", "days", "count", if(uu) paste("days", utrnd) else utrnd),
					"Seasonal Rainfall Amounts" = list("mm", "mm", "mm", "mm", "count", if(uu) paste("mm", utrnd) else utrnd),
					"Longest Dry Spell" = list("days", "days", "days", "days", "count", if(uu) paste("days", utrnd) else utrnd),
					"Number of rain day" = list("days", "days", "days", "days", "count", if(uu) paste("days", utrnd) else utrnd),
					"Maximum daily rain" = list("mm", "mm", "mm", "mm", "count", if(uu) paste("mm", utrnd) else utrnd),
					"Total rain when RR>95thPerc" = list("mm", "mm", "mm", "mm", "count", if(uu) paste("mm", utrnd) else utrnd),
					"Nb of day when RR>95thPerc" = list("days", "days", "days", "days", "count", if(uu) paste("days", utrnd) else utrnd),
					"Dry Spells" = list(dryUn, dryUn, dryUn, dryUn, "count", if(uu) paste(dryUn, utrnd) else utrnd))

		StatVal <- c("Average", "Median", "Standard deviation", "Percentiles", "Frequency", "Trend")
		units <- legUnit[[which(StatVal == StatOp)]]
		units <- if(!is.null(units)) paste0("(Units: ", units, ")") else ""
		legend.texta <- paste(StatOp, units)
	}

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
	shpf <- EnvPICSAplot$shp
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

	if(tclvalue(EnvPICSAplot$varPICSA)%in%c("Onset", "Cessation") &
				StatOp%in%c("Average", "Median", "Percentiles"))
	{
		odaty <- format(EnvPICSAplot$output$start.date[1], '%Y-%m-%d')
		legendLabel <- format(as.Date(breaks, origin = odaty), '%d-%b')
	}else legendLabel <- breaks

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

	abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)

	lines(ocrds[, 1], ocrds[, 2], lwd = EnvPICSAplot$SHPOp$lwd, col = EnvPICSAplot$SHPOp$col)
	
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

PICSA.plot.TSGraph <- function(){
	TSGraphOp <- EnvPICSAplot$TSGraphOp
	dryspl <- as.numeric(str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.dryspell))))
	varPICSA <- str_trim(tclvalue(EnvPICSAplot$varPICSA))

	if(EnvPICSAplot$output$data.type == "cdtstation"){
		ixy <- which(EnvPICSAplot$output$data$id == str_trim(tclvalue(EnvPICSAplot$graph$stnIDTSp)))
		if(length(ixy) == 0){
			InsertMessagesTxt(main.txt.out, "Station not found", format = TRUE)
			return(NULL)
		}

		if(tclvalue(EnvPICSAplot$graph$varTSp) == "From Maps"){
			don <- EnvPICSAplot$tsdata$data[, ixy]
			if(varPICSA == "Dry Spells"){
				nval <- sapply(don, function(x) (length(x) == 1) & is.na(x[1]))
				don <- sapply(don, function(x) sum(!is.na(x) & x >= dryspl))
				don[nval] <- NA
			}else don <- as.numeric(don)
			# if(str_trim(tclvalue(EnvPICSAplot$varPICSA)) == "Longest Dry Spell"){
			# 	don <- sapply(don, max, na.rm = TRUE)
			# 	don[is.infinite(don)] <- NA
			# }
			dates <- EnvPICSAplot$tsdata$date
			daty <- as.numeric(substr(dates, 1, 4))
		}else{
			don <- EnvPICSAplot$daily.precip[, ixy]
			dates <- EnvPICSAplot$output$data$date
		}

		EnvPICSAplot$location <- paste0("Station: ", EnvPICSAplot$output$data$id[ixy])
	}else{
		tsdata.dir <- switch(varPICSA,
							"Onset" = "Onset_days",
							"Cessation" = "Cessation_days",
							"Season Length" = "Season_length",
							"Seasonal Rainfall Amounts" = "Seasonal_rain_amount",
							"Number of rain day" = "Number_rainy_day",
							"Maximum daily rain" = "Maximum_rain_daily",
							"Total rain when RR>95thPerc" = "Total_rain_above_Perc95th",
							"Nb of day when RR>95thPerc" = "Number_day_above_Perc95th",
							"Longest Dry Spell" = "Dry_Spells",
							"Dry Spells" = "Dry_Spells")

		cdtdataset <- EnvPICSAplot$cdtdataset
		xlon <- cdtdataset$coords$mat$x
		xlat <- cdtdataset$coords$mat$y
		ilon <- as.numeric(str_trim(tclvalue(EnvPICSAplot$graph$lonLOC)))
		ilat <- as.numeric(str_trim(tclvalue(EnvPICSAplot$graph$latLOC)))

		iclo <- findInterval(ilon, xlon)
		ilo <- iclo + (2 * ilon > xlon[iclo] + xlon[iclo+1])
		icla <- findInterval(ilat, xlat)
		ila <- icla + (2 * ilat > xlat[icla] + xlat[icla+1])

		if(is.na(ilo) | is.na(ila)){
			InsertMessagesTxt(main.txt.out, "Coordinates outside of data range", format = TRUE)
			return(NULL)
		}
		ixy <- ilo + length(xlon) * (ila-1)

		if(tclvalue(EnvPICSAplot$graph$varTSp) == "From Maps"){
			don <- readCdtDatasetChunk.locations(ixy, cdtdataset$fileInfo, cdtdataset, chunkDir = tsdata.dir, do.par = FALSE)

			if(varPICSA == "Dry Spells"){
				nval <- sapply(don, function(x) (length(x) == 1) & is.na(x[1]))
				don <- sapply(don, function(x) sum(!is.na(x) & x >= dryspl))
				don[nval] <- NA
			}
			if(varPICSA == "Longest Dry Spell"){
				don <- sapply(don, max, na.rm = TRUE)
				don[is.infinite(don)] <- NA
			}

			don <- as.numeric(don$data[, 1])
			dates <- cdtdataset$dateInfo$date
			daty <- as.numeric(substr(dates, 1, 4))
		}else{
			don <- readCdtDatasetChunk.locations(ixy, EnvPICSAplot$output$daily.precip, EnvPICSAplot$daily.precip, do.par = FALSE)
			don <- as.numeric(don$data[EnvPICSAplot$daily.precip$dateInfo$index, 1])
			dates <- EnvPICSAplot$daily.precip$dateInfo$date
		}

		EnvPICSAplot$location <- paste0("Longitude: ", round(ilon, 5), ", Latitude: ", round(ilat, 5))
	}

	if(tclvalue(EnvPICSAplot$graph$varTSp) == "Daily Rainfall"){
		picsa.plot.daily(dates, don, EnvPICSAplot$output$params$dryday)
		return(NULL)
	}

	#########
	GRAPHTYPE <- str_trim(tclvalue(EnvPICSAplot$graph$typeTSp))
	origindate <- if(varPICSA%in%c("Onset", "Cessation")) as.character(EnvPICSAplot$output$start.date[1]) else NULL

	if(varPICSA == "Onset"){
		xlab0 <- ''
		ylab0 <- ''
		sub <- NULL
		theoretical <- FALSE
		title <- "Starting dates of the rainy season"
	}
	if(varPICSA == "Cessation"){
		xlab0 <- ''
		ylab0 <- ''
		sub <- NULL
		theoretical <- FALSE
		title <- "Ending dates of the rainy season"
	}
	if(varPICSA == "Season Length"){
		xlab0 <- 'Year'
		ylab0 <- 'Number of Days'
		sub <- NULL
		theoretical <- TRUE
		title <- "Length of the rainy season"
	}
	if(varPICSA == "Seasonal Rainfall Amounts"){
		xlab0 <- 'Year'
		ylab0 <- 'Rainfall Amount (mm)'
		sub <- NULL
		theoretical <- TRUE
		title <- "Seasonal rainfall amounts"
	}
	if(varPICSA == "Dry Spells"){
		xlab0 <- 'Year'
		ylab0 <- 'Number of Dry Spells'
		sub <- paste("Dry spells -", dryspl, "or more consecutive days")
		theoretical <- FALSE
		title <- "Dry Spells"
	}
	if(varPICSA == "Longest Dry Spell"){
		xlab0 <- 'Year'
		ylab0 <- 'Number of Days'
		sub <- NULL
		theoretical <- FALSE
		title <- "Longest dry spell"
	}
	if(varPICSA == "Number of rain day"){
		xlab0 <- 'Year'
		ylab0 <- 'Number of Days'
		sub <- NULL
		theoretical <- TRUE
		title <- "Seasonal number of rainy days"
	}
	if(varPICSA == "Maximum daily rain"){
		xlab0 <- 'Year'
		ylab0 <- 'Rainfall Depth (mm)'
		sub <- NULL
		theoretical <- FALSE
		title <- 'Seasonal maximum of daily rainfall'
	}
	if(varPICSA == "Total rain when RR>95thPerc"){
		xlab0 <- 'Year'
		ylab0 <- 'Rainfall Amount (mm)'
		sub <- NULL
		theoretical <- FALSE
		title <- 'Seasonal total of precipitation when RR > 95th percentile'
	}
	if(varPICSA == "Nb of day when RR>95thPerc"){
		xlab0 <- 'Year'
		ylab0 <- 'Number of Days'
		sub <- NULL
		theoretical <- FALSE
		title <- 'Seasonal count of days when RR > 95th percentile'
	}

	if(GRAPHTYPE == "Line"){
		optsgph <- TSGraphOp$line
		xlim <- range(daty, na.rm = TRUE)
		if(optsgph$xlim$is.min) xlim[1] <- as.numeric(optsgph$xlim$min)
		if(optsgph$xlim$is.max) xlim[2] <- as.numeric(optsgph$xlim$max)
		idt <- daty >= xlim[1] & daty <= xlim[2]
		daty <- daty[idt]
		don <- don[idt]
		ylim <- range(pretty(don))
		if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
		if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

		xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else xlab0
		# ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else ylab0
		if(optsgph$axislabs$is.ylab){
			ylab <- optsgph$axislabs$ylab
			sub <- NULL
		}else ylab <- ylab0

		if(optsgph$title$is.title){
			titre <- optsgph$title$title
			titre.pos <- optsgph$title$position
		}else{
			titre <- title
			titre.pos <- "top"
		}

		legends <- NULL
		if(optsgph$legend$is$mean){
			legends$add$mean <- optsgph$legend$add$mean
			legends$col$mean <- optsgph$legend$col$mean
			legends$text$mean <- optsgph$legend$text$mean
			legends$lwd$mean <- optsgph$legend$lwd$mean
		}else{
			if(tclvalue(EnvPICSAplot$graph$averageTSp) == "1") legends$add$mean <- TRUE
		}
		if(optsgph$legend$is$linear){
			legends$add$linear <- optsgph$legend$add$linear
			legends$col$linear <- optsgph$legend$col$linear
			legends$text$linear <- optsgph$legend$text$linear
			legends$lwd$linear <- optsgph$legend$lwd$linear
		}else{
			if(tclvalue(EnvPICSAplot$graph$trendTSp) == "1") legends$add$linear <- TRUE
		}
		if(optsgph$legend$is$tercile){
			legends$add$tercile <- optsgph$legend$add$tercile
			legends$col$tercile1 <- optsgph$legend$col$tercile1
			legends$text$tercile1 <- optsgph$legend$text$tercile1
			legends$col$tercile2 <- optsgph$legend$col$tercile2
			legends$text$tercile2 <- optsgph$legend$text$tercile2
			legends$lwd$tercile <- optsgph$legend$lwd$tercile
		}else{
			if(tclvalue(EnvPICSAplot$graph$tercileTSp) == "1") legends$add$tercile <- TRUE
		}

		climatoAnalysis.plot.line(daty, don, xlim = xlim, ylim = ylim, origindate = origindate,
									xlab = xlab, ylab = ylab, ylab.sub = sub,
									title = titre, title.position = titre.pos, axis.font = 1,
									plotl = optsgph$plot, legends = legends,
									location = EnvPICSAplot$location)
	}

	if(GRAPHTYPE == "Barplot"){
		optsgph <- TSGraphOp$bar
		xlim <- range(daty, na.rm = TRUE)
		if(optsgph$xlim$is.min) xlim[1] <- as.numeric(optsgph$xlim$min)
		if(optsgph$xlim$is.max) xlim[2] <- as.numeric(optsgph$xlim$max)
		idt <- daty >= xlim[1] & daty <= xlim[2]
		daty <- daty[idt]
		don <- don[idt]
		ylim <- range(pretty(don))
		if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
		if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

		xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else xlab0
		# ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else ylab0
		if(optsgph$axislabs$is.ylab){
			ylab <- optsgph$axislabs$ylab
			sub <- NULL
		}else ylab <- ylab0

		if(optsgph$title$is.title){
			titre <- optsgph$title$title
			titre.pos <- optsgph$title$position
		}else{
			titre <- title
			titre.pos <- "top"
		}

		climatoAnalysis.plot.bar(daty, don, xlim = xlim, ylim = ylim, origindate = origindate,
								xlab = xlab, ylab = ylab, ylab.sub = sub,
								title = titre, title.position = titre.pos, axis.font = 1,
								barcol = optsgph$colors$col,
								location = EnvPICSAplot$location)
	}

	if(GRAPHTYPE == "Probability"){
		optsgph <- TSGraphOp$proba
		xlim <- range(don, na.rm = TRUE)
		if(optsgph$xlim$is.min) xlim[1] <- as.numeric(optsgph$xlim$min)
		if(optsgph$xlim$is.max) xlim[2] <- as.numeric(optsgph$xlim$max)
		ylim <- c(0, 100)
		if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
		if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

		xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else ""
		ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else "Probability of Exceeding"

		if(optsgph$title$is.title){
			titre <- optsgph$title$title
			titre.pos <- optsgph$title$position
		}else{
			titre <- title
			titre.pos <- "top"
		}

		if(theoretical) theoretical <- optsgph$proba$theoretical

		climatoAnalysis.plot.proba(don, xlim = xlim, ylim = ylim, origindate = origindate,
									xlab = xlab, xlab.sub = NULL, ylab = ylab,
									title = titre, title.position = titre.pos, axis.font = 1,
									proba = list(theoretical = theoretical),
									plotp = optsgph$proba, plotl = optsgph$plot,
									location = EnvPICSAplot$location)
	}

	####
	if(GRAPHTYPE%in%c("ENSO-Line", "ENSO-Barplot", "ENSO-Proba")){
		if(EnvPICSAplot$output$data.type == "cdtstation"){
			onset <- readRDS(file.path(EnvPICSAplot$PathPicsa, "CDTDATASET", "Onset_days.rds"))
			onset <- as.numeric(onset[, ixy])
			cessat <- readRDS(file.path(EnvPICSAplot$PathPicsa, "CDTDATASET", "Cessation_days.rds"))
			cessat <- as.numeric(cessat[, ixy])
		}else{
			onset <- readCdtDatasetChunk.locations(ixy, cdtdataset$fileInfo, cdtdataset, chunkDir = "Onset_days", do.par = FALSE)
			onset <- as.numeric(onset$data[EnvPICSAplot$daily.precip$dateInfo$index, 1])
			cessat <- readCdtDatasetChunk.locations(ixy, cdtdataset$fileInfo, cdtdataset, chunkDir = "Cessation_days", do.par = FALSE)
			cessat <- as.numeric(cessat$data[EnvPICSAplot$daily.precip$dateInfo$index, 1])
		}

		onset <- format(onset + EnvPICSAplot$output$start.date, "%Y%m%d")
		cessat <- format(cessat + EnvPICSAplot$output$start.date, "%Y%m%d")

		ijoni <- getIndexSeasonVars(onset, cessat, EnvPICSAplot$ONI$date, "monthly")
		oni <- sapply(ijoni, function(x) mean(EnvPICSAplot$ONI$data[x], na.rm = TRUE))
		oni[is.nan(oni)] <- NA
		oni <- ifelse(oni >= 0.5, 3, ifelse(oni <= -0.5, 1, 2))
	}

	if(GRAPHTYPE == "ENSO-Line"){
		optsgph <- TSGraphOp$line.enso
		xlim <- range(daty, na.rm = TRUE)
		if(optsgph$xlim$is.min) xlim[1] <- as.numeric(optsgph$xlim$min)
		if(optsgph$xlim$is.max) xlim[2] <- as.numeric(optsgph$xlim$max)
		idt <- daty >= xlim[1] & daty <= xlim[2]
		daty <- daty[idt]
		don <- don[idt]
		oni <- oni[idt]
		ylim <- range(pretty(don))
		if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
		if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

		xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else xlab0
		# ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else ylab0
		if(optsgph$axislabs$is.ylab){
			ylab <- optsgph$axislabs$ylab
			sub <- NULL
		}else ylab <- ylab0

		if(optsgph$title$is.title){
			titre <- optsgph$title$title
			titre.pos <- optsgph$title$position
		}else{
			titre <- title
			titre.pos <- "top"
		}

		legends <- NULL
		if(optsgph$legend$is$mean){
			legends$add$mean <- optsgph$legend$add$mean
			legends$col$mean <- optsgph$legend$col$mean
			legends$text$mean <- optsgph$legend$text$mean
			legends$lwd$mean <- optsgph$legend$lwd$mean
		}else{
			if(tclvalue(EnvPICSAplot$graph$averageTSp) == "1") legends$add$mean <- TRUE
		}
		if(optsgph$legend$is$linear){
			legends$add$linear <- optsgph$legend$add$linear
			legends$col$linear <- optsgph$legend$col$linear
			legends$text$linear <- optsgph$legend$text$linear
			legends$lwd$linear <- optsgph$legend$lwd$linear
		}else{
			if(tclvalue(EnvPICSAplot$graph$trendTSp) == "1") legends$add$linear <- TRUE
		}
		if(optsgph$legend$is$tercile){
			legends$add$tercile <- optsgph$legend$add$tercile
			legends$col$tercile1 <- optsgph$legend$col$tercile1
			legends$text$tercile1 <- optsgph$legend$text$tercile1
			legends$col$tercile2 <- optsgph$legend$col$tercile2
			legends$text$tercile2 <- optsgph$legend$text$tercile2
			legends$lwd$tercile <- optsgph$legend$lwd$tercile
		}else{
			if(tclvalue(EnvPICSAplot$graph$tercileTSp) == "1") legends$add$tercile <- TRUE
		}

		climatoAnalysis.plot.line.ENSO(daty, don, oni, xlim = xlim, ylim = ylim, origindate = origindate,
										xlab = xlab, ylab = ylab, ylab.sub = sub,
										title = titre, title.position = titre.pos, axis.font = 1,
										plotl = optsgph$plot, legends = legends,
										location = EnvPICSAplot$location)
	}

	if(GRAPHTYPE == "ENSO-Barplot"){
		optsgph <- TSGraphOp$bar.enso
		xlim <- range(daty, na.rm = TRUE)
		if(optsgph$xlim$is.min) xlim[1] <- as.numeric(optsgph$xlim$min)
		if(optsgph$xlim$is.max) xlim[2] <- as.numeric(optsgph$xlim$max)
		idt <- daty >= xlim[1] & daty <= xlim[2]
		daty <- daty[idt]
		don <- don[idt]
		oni <- oni[idt]
		ylim <- range(pretty(don))
		if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
		if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

		xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else xlab0
		# ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else ylab0
		if(optsgph$axislabs$is.ylab){
			ylab <- optsgph$axislabs$ylab
			sub <- NULL
		}else ylab <- ylab0

		if(optsgph$title$is.title){
			titre <- optsgph$title$title
			titre.pos <- optsgph$title$position
		}else{
			titre <- title
			titre.pos <- "top"
		}

		climatoAnalysis.plot.bar.ENSO(daty, don, oni, xlim = xlim, ylim = ylim, origindate = origindate,
									xlab = xlab, ylab = ylab, ylab.sub = sub,
									title = titre, title.position = titre.pos, axis.font = 1,
									barcol = optsgph$colors$col, location = EnvPICSAplot$location)
	}

	if(GRAPHTYPE == "ENSO-Proba"){
		optsgph <- TSGraphOp$proba.enso
		xlim <- range(don, na.rm = TRUE)
		if(optsgph$xlim$is.min) xlim[1] <- as.numeric(optsgph$xlim$min)
		if(optsgph$xlim$is.max) xlim[2] <- as.numeric(optsgph$xlim$max)
		ylim <- c(0, 100)
		if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
		if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

		xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else ""
		ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else "Probability of Exceeding"

		if(optsgph$title$is.title){
			titre <- optsgph$title$title
			titre.pos <- optsgph$title$position
		}else{
			titre <- title
			titre.pos <- "top"
		}

		climatoAnalysis.plot.proba.ENSO(don, oni, xlim = xlim, ylim = ylim, origindate = origindate,
										xlab = xlab, xlab.sub = NULL, ylab = ylab,
 										title = titre, title.position = titre.pos, axis.font = 1,
 										plotl = optsgph$plot, location = EnvPICSAplot$location)
	}

	if(GRAPHTYPE == "Anomaly"){
		optsgph <- TSGraphOp$anomaly
		xlim <- range(daty, na.rm = TRUE)
		if(optsgph$xlim$is.min) xlim[1] <- as.numeric(optsgph$xlim$min)
		if(optsgph$xlim$is.max) xlim[2] <- as.numeric(optsgph$xlim$max)
		idt <- daty >= xlim[1] & daty <= xlim[2]
		daty <- daty[idt]
		don <- don[idt]
		ylim <- range(pretty(don))
		if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
		if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max
		if(!optsgph$ylim$is.min & !optsgph$ylim$is.max) ylim <- NULL

		percent <- optsgph$anom$perc.anom
		xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else xlab0
		# ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else {if(percent) "Anomaly (% of Mean)" else ylab0}
		if(optsgph$axislabs$is.ylab){
			ylab <- optsgph$axislabs$ylab
			sub <- NULL
		}else ylab <- if(percent) "Anomaly (% of Mean)" else ylab0

		if(optsgph$title$is.title){
			titre <- optsgph$title$title
			titre.pos <- optsgph$title$position
		}else{
			titre <- title
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
										xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ylab.sub = sub,
										title = titre, title.position = titre.pos, axis.font = 1,
										barcol = loko, location = EnvPICSAplot$location)
	}
}


##############################################################################

PICSA.Display.TSMaps <- function(parent){
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
		pltusr <- PICSA.plot.TSMaps()
		par(op)
		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvPICSAplot$notebookTab.TSMap, 'Aggregated-Data', AllOpenTabType, AllOpenTabData)
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	#########
	tkbind(img, "<Motion>", function(W, x, y){
		if(length(ls(EnvPICSAplot)) == 0) return(NULL)
		if(is.null(EnvPICSAplot$output)) return(NULL)

		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		xydisp <- LatLonLabels(xyMouse$x, xyMouse$y)
		frxcoord <- ifelse(xyMouse$inout, '', xydisp$xdisp)
		frycoord <- ifelse(xyMouse$inout, '', xydisp$ydisp)

		if(EnvPICSAplot$output$data.type == "cdtstation"){
			fdispIdStn <- function(x){
				y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvPICSAplot$output$data$lon)^2 + (xyMouse$y-EnvPICSAplot$output$data$lat)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			frzcoord <- ifelse(xyMouse$inout | rayondisp, '', EnvPICSAplot$output$data$id[inear])
		}else{
			frzcoord <- ""
		}

		tclvalue(xpcoord) <- frxcoord
		tclvalue(ypcoord) <- frycoord
		tclvalue(zpcoord) <- frzcoord
	})

	tkbind(img, "<Button-1>", function(W, x, y){
		if(length(ls(EnvPICSAplot)) == 0) return(NULL)
		if(is.null(EnvPICSAplot$output)) return(NULL)

		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		if(EnvPICSAplot$output$data.type == "cdtstation"){
			fdispIdStn <- function(x){
				 y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvPICSAplot$output$data$lon)^2 + (xyMouse$y-EnvPICSAplot$output$data$lat)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			if(!(xyMouse$inout | rayondisp)){
				tclvalue(EnvPICSAplot$graph$stnIDTSp) <- EnvPICSAplot$output$data$id[inear]
				plotTS <- TRUE
			}else plotTS <- FALSE
		}else{
			if(!xyMouse$inout){
				tclvalue(EnvPICSAplot$graph$lonLOC) <- round(xyMouse$x, 6)
				tclvalue(EnvPICSAplot$graph$latLOC) <- round(xyMouse$y, 6)
				plotTS <- TRUE
			}else plotTS <- FALSE
		}

		if(plotTS){
			imgContainer <- PICSA.Display.TSPlot(tknotes)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvPICSAplot$notebookTab.tsplot, AllOpenTabType, AllOpenTabData)
			EnvPICSAplot$notebookTab.tsplot <- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}

#######################################

PICSA.Display.ClimMap <- function(parent){
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
		pltusr <- PICSA.plot.ClimMaps()
		par(op)
		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvPICSAplot$notebookTab.climMap, 'Clim-Analysis-Maps', AllOpenTabType, AllOpenTabData)
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	#########
	tkbind(img, "<Motion>", function(W, x, y){
		if(length(ls(EnvPICSAplot)) == 0) return(NULL)
		if(is.null(EnvPICSAplot$output)) return(NULL)

		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		xydisp <- LatLonLabels(xyMouse$x, xyMouse$y)
		frxcoord <- ifelse(xyMouse$inout, '', xydisp$xdisp)
		frycoord <- ifelse(xyMouse$inout, '', xydisp$ydisp)

		if(EnvPICSAplot$output$data.type == "cdtstation"){
			fdispIdStn <- function(x){
				y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvPICSAplot$output$data$lon)^2 + (xyMouse$y-EnvPICSAplot$output$data$lat)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			frzcoord <- ifelse(xyMouse$inout | rayondisp, '', EnvPICSAplot$output$data$id[inear])
		}else{
			frzcoord <- ""
		}

		tclvalue(xpcoord) <- frxcoord
		tclvalue(ypcoord) <- frycoord
		tclvalue(zpcoord) <- frzcoord
	})

	tkbind(img, "<Button-1>", function(W, x, y){
		if(length(ls(EnvPICSAplot)) == 0) return(NULL)
		if(is.null(EnvPICSAplot$output)) return(NULL)

		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		if(EnvPICSAplot$output$data.type == "cdtstation"){
			fdispIdStn <- function(x){
				 y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvPICSAplot$output$data$lon)^2 + (xyMouse$y-EnvPICSAplot$output$data$lat)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			if(!(xyMouse$inout | rayondisp)){
				tclvalue(EnvPICSAplot$graph$stnIDTSp) <- EnvPICSAplot$output$data$id[inear]
				plotTS <- TRUE
			}else plotTS <- FALSE
		}else{
			if(!xyMouse$inout){
				tclvalue(EnvPICSAplot$graph$lonLOC) <- round(xyMouse$x, 6)
				tclvalue(EnvPICSAplot$graph$latLOC) <- round(xyMouse$y, 6)
				plotTS <- TRUE
			}else plotTS <- FALSE
		}

		if(plotTS){
			imgContainer <- PICSA.Display.TSPlot(tknotes)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvPICSAplot$notebookTab.tsplot, AllOpenTabType, AllOpenTabData)
			EnvPICSAplot$notebookTab.tsplot <- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}

#######################################

PICSA.Display.TSPlot <- function(parent){
	plotIt <- function(){
		PICSA.plot.TSGraph()
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvPICSAplot$notebookTab.tsplot, 'Time-Series-Plot', AllOpenTabType, AllOpenTabData)
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




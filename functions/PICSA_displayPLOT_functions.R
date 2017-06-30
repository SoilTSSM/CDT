PICSA.plotTSMaps <- function(ocrds){
	iyear <- as.numeric(str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.year))))

	#################

	if(tclvalue(EnvPICSAplot$varPICSA) == "Onset"){
		don <- EnvPICSA$output$Onset.nb[EnvPICSA$index$onsetYear == iyear, ]
		texta <- NULL
		title <- "Starting dates of the rainy season"
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Cessation"){
		don <- EnvPICSA$output$Cessation.nb[EnvPICSA$index$cessatYear == iyear, ]
		texta <- NULL
		title <- "Ending dates of the rainy season"
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Season Length"){
		don <- EnvPICSA$output$SeasonLength[EnvPICSA$index$onsetYear == iyear, ]
		texta <- 'Number of Days'
		title <- "Length of the rainy season"
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Seasonal Rainfall Amounts"){
		don <- EnvPICSA$picsa$RainTotal[EnvPICSA$index$onsetYear == iyear, ]
		texta <- 'Rainfall Amount (mm)'
		title <- "Seasonal rainfall amounts"
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Dry Spells"){
		don <- EnvPICSA$picsa$AllDrySpell[EnvPICSA$index$onsetYear == iyear, ]
		drydef <- as.numeric(str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.dryspell))))
		don <- sapply(don, function(x) sum(x >= drydef))
		texta <- paste0('Number of Dry Spells', '\n', "Dry spells - ", drydef, " or more consecutive days")
		title <- "Dry Spells"
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Longest Dry Spell"){
		don <- EnvPICSA$picsa$AllDrySpell[EnvPICSA$index$onsetYear == iyear, ]
		don <- sapply(don, max)
		texta <- 'Number of Days'
		title <- "Longest dry spell"
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Number of rain day"){
		don <- EnvPICSA$picsa$nbdayrain[EnvPICSA$index$onsetYear == iyear, ]
		texta <- 'Number of Days'
		title <- "Seasonal number of rainy days"
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Maximum daily rain"){
		don <- EnvPICSA$picsa$max24h[EnvPICSA$index$onsetYear == iyear, ]
		texta <- 'Rainfall Depth (mm)'
		title <- 'Seasonal maximum of daily rainfall'
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Total rain when RR>95thPerc"){
		don <- EnvPICSA$picsa$TotalQ95th[EnvPICSA$index$onsetYear == iyear, ]
		texta <- 'Rainfall Amount (mm)'
		title <- 'Seasonal total of precipitation when RR > 95th percentile'
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Nb of day when RR>95thPerc"){
		don <- EnvPICSA$picsa$NbQ95th[EnvPICSA$index$onsetYear == iyear, ]
		texta <- 'Number of Days'
		title <- 'Seasonal count of days when RR > 95th percentile'
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Maximum temperature"){
		don <- EnvPICSA$picsa$tmax[EnvPICSA$index$onsetYear == iyear, ]
		texta <- 'Temperature (°C)'
		title <- 'Seasonal average maximum temperature'
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Minimum temperature"){
		don <- EnvPICSA$picsa$tmin[EnvPICSA$index$onsetYear == iyear, ]
		texta <- 'Temperature (°C)'
		title <- 'Seasonal average minimum temperature'
	}

	#################

	if(EnvPICSA$data.type == "cdt"){
		xna <- EnvPICSA$cdtPrecip$lon[is.na(don)]
		yna <- EnvPICSA$cdtPrecip$lat[is.na(don)]
		don <- as.image(don, x = cbind(EnvPICSA$cdtPrecip$lon, EnvPICSA$cdtPrecip$lat), nx = 60, ny = 60)
	}else{
		xna <- NULL
		lon <- sort(unique(EnvPICSA$cdtPrecip$lon))
		lat <- sort(unique(EnvPICSA$cdtPrecip$lat))
		don <- list(x = lon, y = lat, z = matrix(don, nrow = length(lon), ncol = length(lat)))
	}

	#################

	breaks <- pretty(range(don$z, na.rm = TRUE))
	breaks <- if(length(breaks) > 0) breaks else c(0, 1) 
	if(length(breaks) < 7) breaks <- round(seq(breaks[1], breaks[length(breaks)], length.out = 7), 4)

	kolFonction <- match.fun("tim.colors")
	kolor <- kolFonction(length(breaks)-1)

	if(diff(EnvPICSAplot$xlim.maps) > diff(EnvPICSAplot$ylim.maps)){
		horizontal <- TRUE
		legend.mar <- 3.5
		legend.width <- 0.7
		mar <- c(7, 4, 2.5, 2.5)
		legend.args <- if(!is.null(texta)) list(text = texta, cex = 0.8, side = 1, line = 2) else NULL
	}else{
		horizontal <- FALSE
		legend.mar <- 6.2
		mar <- c(4, 4, 2.5, 6)
		legend.width <- 0.9
		legend.args <- if(!is.null(texta)) list(text = texta, cex = 0.8, side = 4, line = 3) else NULL
	}

	#################

	if(tclvalue(EnvPICSAplot$varPICSA) == "Onset"){
		legendLabel <- format(as.Date(breaks, origin = EnvPICSA$index$onsetOrigDate), '%d %b')
	}else if(tclvalue(EnvPICSAplot$varPICSA) == "Cessation"){
		legendLabel <- format(as.Date(breaks, origin = EnvPICSA$index$cessatOrigDate), '%d %b')
	}else legendLabel <- breaks

	#################

	opar <- par(mar = mar)
	plot(1, xlim = EnvPICSAplot$xlim.maps, ylim = EnvPICSAplot$ylim.maps, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
	axlabsFun <- if(Sys.info()["sysname"] == "Windows") LatLonAxisLabels else LatLonAxisLabels1
	axlabs <- axlabsFun(axTicks(1), axTicks(2))
	axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tcl = -0.2, cex.axis = 0.8)
	axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tcl = -0.2, las = 1, cex.axis = 0.8)
	title(main = title, cex.main = 1, font.main= 2)

	if(length(xna) > 0) points(xna, yna, pch = '*')
	image.plot(don, breaks = breaks, col = kolor, horizontal = horizontal, xaxt = 'n', yaxt = 'n', add = TRUE,
				legend.mar = legend.mar, legend.width = legend.width, legend.args = legend.args,
				axis.args = list(at = breaks, labels = legendLabel, cex.axis = 0.7, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)))

	abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)
	lines(ocrds[, 1], ocrds[, 2])

	plt <- par("plt")
	usr <- par("usr")
	par(opar)
	return(list(par = c(plt, usr)))
}

##############################

PICSA.plotClimMaps <- function(ocrds){
	StatOp <- tclvalue(EnvPICSAplot$analysis.method)
	if(StatOp == "Average") aggrFonction <- function(Y, X = NULL, O = NULL) base::colMeans(Y, na.rm = TRUE)
	if(StatOp == "Median") aggrFonction <- function(Y, X = NULL, O = NULL) matrixStats::colMedians(Y, na.rm = TRUE)
	if(StatOp == "Standard deviation") aggrFonction <- function(Y, X = NULL, O = NULL) matrixStats::colSds(Y, na.rm = TRUE)
	if(StatOp == "Trend") 
		aggrFonction <- function(Y, X = NULL, O = NULL)
		{
			ncolY <- ncol(Y)
			nrowY <- nrow(Y)
			X <- if(is.matrix(X)) X else matrix(X, nrow = nrowY, ncol = ncolY)
			ina <- is.na(X) | is.na(Y)
			X[ina] <- NA
			Y[ina] <- NA
			nbY <- base::colSums(!is.na(Y))
			nbY[nbY < 3] <- NA

			mX <- base::colMeans(X, na.rm = TRUE)
			mY <- base::colMeans(Y, na.rm = TRUE)
			vX <- matrixStats::colVars(X, na.rm = TRUE)
			# vY <- matrixStats::colVars(Y, na.rm = TRUE)

			X1 <- X - matrix(mX, nrowY, ncolY, byrow = TRUE)
			Y1 <- Y - matrix(mY, nrowY, ncolY, byrow = TRUE)
			COV <- base::colSums(X1 * Y1, na.rm = TRUE) / (nbY - 1)
			alpha <- COV / vX
			return(alpha)
		}
	if(StatOp == "Percentiles")
		aggrFonction <- function(Y, X = NULL, O = NULL){
			Q <- as.numeric(tclvalue(EnvPICSAplot$mth.perc))/100
			apply(Y, 2, quantile8, probs = Q)
			matrixStats::colQuantiles(Y, probs = Q, na.rm = TRUE)
		}
	if(StatOp == "Frequency")
		aggrFonction <- function(Y, X = NULL, O = NULL)
		{
			xlow <- tclvalue(EnvPICSAplot$low.thres)
			xup <- tclvalue(EnvPICSAplot$up.thres)

			if(tclvalue(EnvPICSAplot$varPICSA)%in%c("Onset", "Cessation")){
				dlo <- as.Date(paste(2014, xlow, sep = '-'))
				dup <- as.Date(paste(2014, xup, sep = '-'))
				if(dlo > dup) dup <- as.Date(paste(2015, xup, sep = '-'))
				xlow <- as.numeric(dlo-as.Date(O))
				xup <- as.numeric(dup-as.Date(O))
			}else{
				xlow <- as.numeric(xlow)
				xup <- as.numeric(xup)
			}
			base::colSums(Y >= xlow & Y <= xup, na.rm = TRUE)
		}

	################# 

	if(tclvalue(EnvPICSAplot$varPICSA) == "Onset"){
		don <- EnvPICSA$output$Onset.nb
		dimdon <- dim(don)
		don <- as.numeric(don)
		dim(don) <- dimdon
		title <- "Starting dates of the rainy season"
		unit.avg <- NULL
		unit.med <- NULL
		unit.sd <- "days"
		unit.trend <- "days/year"
		unit.perc <- NULL
		unit.freq <- "count"
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Cessation"){
		don <- EnvPICSA$output$Cessation.nb
		dimdon <- dim(don)
		don <- as.numeric(don)
		dim(don) <- dimdon
		title <- "Ending dates of the rainy season"
		unit.avg <- NULL
		unit.med <- NULL
		unit.sd <- "days"
		unit.trend <- "days/year"
		unit.perc <- NULL
		unit.freq <- "count"
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Season Length"){
		don <- EnvPICSA$output$SeasonLength
		title <- "Length of the rainy season"
		unit.avg <- "days"
		unit.med <- "days"
		unit.sd <- "days"
		unit.trend <- "days/year"
		unit.perc <- "days"
		unit.freq <- "count"
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Seasonal Rainfall Amounts"){
		don <- EnvPICSA$picsa$RainTotal
		title <- "Seasonal rainfall amounts"
		unit.avg <- "mm"
		unit.med <- "mm"
		unit.sd <- "mm"
		unit.trend <- "mm/year"
		unit.perc <- "mm"
		unit.freq <- "count"
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Dry Spells"){
		don <- EnvPICSA$picsa$AllDrySpell
		dimdon <- dim(don)
		drydef <- as.numeric(str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.dryspell))))
		if(!is.null(EnvPICSAplot$DrySpellVal)){
			if(EnvPICSAplot$DrySpellDef != drydef){
				extDS <- TRUE
				EnvPICSAplot$DrySpellVal <- NULL
			}else extDS <- FALSE
		}else extDS <- TRUE
		if(extDS){
			don <- sapply(don, function(x) sum(x >= drydef))
			EnvPICSAplot$DrySpellVal <- don
			EnvPICSAplot$DrySpellDef <- drydef
		}else don <- EnvPICSAplot$DrySpellVal
		dim(don) <- dimdon
		title <- "Dry Spells"
		unit.avg <- "Number of Dry Spells"
		unit.med <- "Number of Dry Spells"
		unit.sd <- "Number of Dry Spells"
		unit.trend <- "Number of Dry Spells/year"
		unit.perc <- "Number of Dry Spells"
		unit.freq <- "count"
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Longest Dry Spell"){
		don <- EnvPICSA$picsa$AllDrySpell
		dimdon <- dim(don)
		don <- sapply(don, max)
		dim(don) <- dimdon
		title <- "Longest dry spell"
		unit.avg <- "days"
		unit.med <- "days"
		unit.sd <- "days"
		unit.trend <- "days/year"
		unit.perc <- "days"
		unit.freq <- "count"
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Number of rain day"){
		don <- EnvPICSA$picsa$nbdayrain
		title <- "Seasonal number of rainy days"
		unit.avg <- "days"
		unit.med <- "days"
		unit.sd <- "days"
		unit.trend <- "days/year"
		unit.perc <- "days"
		unit.freq <- "count"
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Maximum daily rain"){
		don <- EnvPICSA$picsa$max24h
		title <- 'Seasonal maximum of daily rainfall'
		unit.avg <- "mm"
		unit.med <- "mm"
		unit.sd <- "mm"
		unit.trend <- "mm/year"
		unit.perc <- "mm"
		unit.freq <- "count"
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Total rain when RR>95thPerc"){
		don <- EnvPICSA$picsa$TotalQ95th
		title <- 'Seasonal total of precipitation when RR > 95th percentile'
		unit.avg <- "mm"
		unit.med <- "mm"
		unit.sd <- "mm"
		unit.trend <- "mm/year"
		unit.perc <- "mm"
		unit.freq <- "count"
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Nb of day when RR>95thPerc"){
		don <- EnvPICSA$picsa$NbQ95th
		title <- 'Seasonal count of days when RR > 95th percentile'
		unit.avg <- "days"
		unit.med <- "days"
		unit.sd <- "days"
		unit.trend <- "days/year"
		unit.perc <- "days"
		unit.freq <- "count"
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Maximum temperature"){
		don <- EnvPICSA$picsa$tmax
		title <- 'Seasonal average maximum temperature'
		unit.avg <- "°C"
		unit.med <- "°C"
		unit.sd <- "°C"
		unit.trend <- "°C/year"
		unit.perc <- "°C"
		unit.freq <- "count"
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Minimum temperature"){
		don <- EnvPICSA$picsa$tmin
		title <- 'Seasonal average minimum temperature'
		unit.avg <- "°C"
		unit.med <- "°C"
		unit.sd <- "°C"
		unit.trend <- "°C/year"
		unit.perc <- "°C"
		unit.freq <- "count"
	}

	if(tclvalue(EnvPICSAplot$varPICSA) == "Onset") don <- aggrFonction(don, EnvPICSA$index$onsetYear, EnvPICSA$index$onsetOrigDate)
	else if(tclvalue(EnvPICSAplot$varPICSA) == "Cessation") don <- aggrFonction(don, EnvPICSA$index$cessatYear, EnvPICSA$index$cessatOrigDate)
	else don <- aggrFonction(don, EnvPICSA$index$onsetYear)
	don[is.nan(don) | is.infinite(don)] <- NA

	#################

	if(EnvPICSA$data.type == "cdt"){
		xna <- EnvPICSA$cdtPrecip$lon[is.na(don)]
		yna <- EnvPICSA$cdtPrecip$lat[is.na(don)]
		don <- as.image(don, x = cbind(EnvPICSA$cdtPrecip$lon, EnvPICSA$cdtPrecip$lat), nx = 60, ny = 60)
	}else{
		xna <- NULL
		lon <- sort(unique(EnvPICSA$cdtPrecip$lon))
		lat <- sort(unique(EnvPICSA$cdtPrecip$lat))
		don <- list(x = lon, y = lat, z = matrix(don, nrow = length(lon), ncol = length(lat)))
	}

	#################
	if(StatOp == "Average") units <- unit.avg
	if(StatOp == "Median") units <- unit.med
	if(StatOp == "Standard deviation") units <- unit.sd
	if(StatOp == "Trend") units <- unit.trend
	if(StatOp == "Percentiles") units <- unit.perc
	if(StatOp == "Frequency") units <- unit.freq
	units <- if(!is.null(units)) paste0("(Units: ", units, ")") else ""
	texta <- paste(StatOp, units)

	#################

	breaks <- pretty(range(don$z, na.rm = TRUE))
	breaks <- if(length(breaks) > 0) breaks else c(0, 1) 
	if(length(breaks) < 7) breaks <- round(seq(breaks[1], breaks[length(breaks)], length.out = 7), 4)

	kolFonction <- match.fun("tim.colors")
	kolor <- kolFonction(length(breaks)-1)

	if(diff(EnvPICSAplot$xlim.maps) > diff(EnvPICSAplot$ylim.maps)){
		horizontal <- TRUE
		legend.mar <- 3.5
		legend.width <- 0.7
		mar <- c(7, 4, 2.5, 2.5)
		legend.args <- list(text = texta, cex = 0.8, side = 1, line = 2)
	}else{
		horizontal <- FALSE
		legend.mar <- 6.2
		mar <- c(4, 4, 2.5, 6)
		legend.width <- 0.9
		legend.args <- list(text = texta, cex = 0.8, side = 4, line = 3)
	}

	#################

	if(tclvalue(EnvPICSAplot$varPICSA) == "Onset" & StatOp%in%c("Average", "Median", "Percentiles")){
		legendLabel <- format(as.Date(breaks, origin = EnvPICSA$index$onsetOrigDate), '%d %b')
	}else if(tclvalue(EnvPICSAplot$varPICSA) == "Cessation" & StatOp%in%c("Average", "Median", "Percentiles")){
		legendLabel <- format(as.Date(breaks, origin = EnvPICSA$index$cessatOrigDate), '%d %b')
	}else legendLabel <- breaks

	#################

	opar <- par(mar = mar)
	plot(1, xlim = EnvPICSAplot$xlim.maps, ylim = EnvPICSAplot$ylim.maps, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
	axlabsFun <- if(Sys.info()["sysname"] == "Windows") LatLonAxisLabels else LatLonAxisLabels1
	axlabs <- axlabsFun(axTicks(1), axTicks(2))
	axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tcl = -0.2, cex.axis = 0.8)
	axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tcl = -0.2, las = 1, cex.axis = 0.8)
	title(main = title, cex.main = 1, font.main= 2)

	if(length(xna) > 0) points(xna, yna, pch = '*')
	image.plot(don, breaks = breaks, col = kolor, horizontal = horizontal, xaxt = 'n', yaxt = 'n', add = TRUE,
				legend.mar = legend.mar, legend.width = legend.width, legend.args = legend.args,
				axis.args = list(at = breaks, labels = legendLabel, cex.axis = 0.7, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)))

	abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)
	lines(ocrds[, 1], ocrds[, 2])

	plt <- par("plt")
	usr <- par("usr")
	par(opar)
	return(list(par = c(plt, usr)))
}

##############################

PICSA.plotTSGraph <- function(){
	if(EnvPICSA$data.type == "cdt"){
		ixy <- which(EnvPICSA$cdtPrecip$id == str_trim(tclvalue(EnvPICSAplot$stnIDTSp)))

		if(length(ixy) == 0){
			InsertMessagesTxt(main.txt.out, "Station not found", format = TRUE)
			return(NULL)
		}
		EnvPICSAplot$location <- paste0("Station: ", EnvPICSA$cdtPrecip$id[ixy])
	}else{
		xlon <- sort(unique(EnvPICSA$cdtPrecip$lon))
		xlat <- sort(unique(EnvPICSA$cdtPrecip$lat))
		ilon <- as.numeric(str_trim(tclvalue(EnvPICSAplot$lonLOC)))
		ilat <- as.numeric(str_trim(tclvalue(EnvPICSAplot$latLOC)))
		iclo <- findInterval(ilon, xlon)
		ilo <- iclo + (2 * ilon > xlon[iclo] + xlon[iclo+1])
		icla <- findInterval(ilat, xlat)
		ila <- icla + (2 * ilat > xlat[icla] + xlat[icla+1])

		if(is.na(ilo) | is.na(ila)){
			InsertMessagesTxt(main.txt.out, "Coordinates outside of data range", format = TRUE)
			return(NULL)
		}
		ixy <- matrix(seq(length(EnvPICSA$cdtPrecip$lon)), length(xlon), length(xlat))[ilo, ila]
		EnvPICSAplot$location <- paste0("Longitude: ", round(ilon, 5), ", Latitude: ", round(ilat, 5))
	}

	if(str_trim(tclvalue(EnvPICSAplot$varTSp)) == "From Maps"){
		if(tclvalue(EnvPICSAplot$varPICSA) %in% c("Maximum temperature", "Minimum temperature")){
			tmax <- EnvPICSA$picsa$tmax[, ixy]
			tmin <- EnvPICSA$picsa$tmin[, ixy]
			picsa.plot.TxTn(EnvPICSA$index$onsetYear, tmax, tmin, axis.font = 1)
		}else{
			if(tclvalue(EnvPICSAplot$varPICSA) == "Onset"){
				don <- EnvPICSA$output$Onset.nb[, ixy]
				xlab <- ''
				ylab <- ''
				sub <- NULL
				theoretical <- FALSE
				title <- "Starting dates of the rainy season"
			}
			if(tclvalue(EnvPICSAplot$varPICSA) == "Cessation"){
				don <- EnvPICSA$output$Cessation.nb[, ixy]
				xlab <- ''
				ylab <- ''
				sub <- NULL
				theoretical <- FALSE
				title <- "Ending dates of the rainy season"
			}
			if(tclvalue(EnvPICSAplot$varPICSA) == "Season Length"){
				don <- EnvPICSA$output$SeasonLength[, ixy]
				xlab <- 'Year'
				ylab <- 'Number of Days'
				sub <- NULL
				theoretical <- TRUE
				title <- "Length of the rainy season"
			}
			if(tclvalue(EnvPICSAplot$varPICSA) == "Seasonal Rainfall Amounts"){
				don <- EnvPICSA$picsa$RainTotal[, ixy]
				xlab <- 'Year'
				ylab <- 'Rainfall Amount (mm)'
				sub <- NULL
				theoretical <- TRUE
				title <- "Seasonal rainfall amounts"
			}
			if(tclvalue(EnvPICSAplot$varPICSA) == "Dry Spells"){
				don <- EnvPICSA$picsa$AllDrySpell[, ixy]
				drydef <- as.numeric(str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.dryspell))))
				don <- sapply(don, function(x) sum(x >= drydef))
				xlab <- 'Year'
				ylab <- 'Number of Dry Spells'
				sub <- paste("Dry spells -", drydef, "or more consecutive days")
				theoretical <- FALSE
				title <- "Dry Spells"
			}
			if(tclvalue(EnvPICSAplot$varPICSA) == "Longest Dry Spell"){
				don <- EnvPICSA$picsa$AllDrySpell[, ixy]
				don <- sapply(don, max)
				xlab <- 'Year'
				ylab <- 'Number of Days'
				sub <- NULL
				theoretical <- FALSE
				title <- "Longest dry spell"
			}
			if(tclvalue(EnvPICSAplot$varPICSA) == "Number of rain day"){
				don <- EnvPICSA$picsa$nbdayrain[, ixy]
				xlab <- 'Year'
				ylab <- 'Number of Days'
				sub <- NULL
				theoretical <- TRUE
				title <- "Seasonal number of rainy days"
			}
			if(tclvalue(EnvPICSAplot$varPICSA) == "Maximum daily rain"){
				don <- EnvPICSA$picsa$max24h[, ixy]
				xlab <- 'Year'
				ylab <- 'Rainfall Depth (mm)'
				sub <- NULL
				theoretical <- FALSE
				title <- 'Seasonal maximum of daily rainfall'
			}
			if(tclvalue(EnvPICSAplot$varPICSA) == "Total rain when RR>95thPerc"){
				don <- EnvPICSA$picsa$TotalQ95th[, ixy]
				xlab <- 'Year'
				ylab <- 'Rainfall Amount (mm)'
				sub <- NULL
				theoretical <- FALSE
				title <- 'Seasonal total of precipitation when RR > 95th percentile'
			}
			if(tclvalue(EnvPICSAplot$varPICSA) == "Nb of day when RR>95thPerc"){
				don <- EnvPICSA$picsa$NbQ95th[, ixy]
				xlab <- 'Year'
				ylab <- 'Number of Days'
				sub <- NULL
				theoretical <- FALSE
				title <- 'Seasonal count of days when RR > 95th percentile'
			}

			####
			if(tclvalue(EnvPICSAplot$varPICSA) == "Onset"){
				xaxe <- EnvPICSA$index$onsetYear
				origindate <- EnvPICSA$index$onsetOrigDate
			}else if(tclvalue(EnvPICSAplot$varPICSA) == "Cessation"){
				xaxe <- EnvPICSA$index$cessatYear
				origindate <- EnvPICSA$index$cessatOrigDate
			}else{
				xaxe <- EnvPICSA$index$onsetYear
				origindate <- NULL
			}

			####
			if(str_trim(tclvalue(EnvPICSAplot$typeTSp)) == "Line"){
				plt.avg <- if(tclvalue(EnvPICSAplot$averageTSp) == "1") TRUE else FALSE
				plt.terc <- if(tclvalue(EnvPICSAplot$tercileTSp) == "1") TRUE else FALSE
				plt.trd <- if(tclvalue(EnvPICSAplot$trendTSp) == "1") TRUE else FALSE
				picsa.plot.line(xaxe, don, origindate = origindate, 
								sub = sub, xlab = xlab, ylab = ylab, title = title,
								mean = plt.avg, tercile = plt.terc, linear = plt.trd,
								col = list(line = "red", points = "blue"), axis.font = 1, start.zero = FALSE,
								col.add = list(mean = "black", tercile1 = "green", tercile2 = "blue", linear = "purple3"))
			}

			if(str_trim(tclvalue(EnvPICSAplot$typeTSp)) == "Barplot"){
				picsa.plot.bar(xaxe, don, origindate = origindate,
								sub = sub, xlab = xlab, ylab = ylab, title = title,
								barcol = "darkblue", axis.font = 1, start.zero = FALSE)
			}

			if(str_trim(tclvalue(EnvPICSAplot$typeTSp)) == "Probability"){
				picsa.plot.proba(don, origindate = origindate, sub = sub, xlab = ylab, title = title,
								 axis.font = 1, theoretical = theoretical,  gof.c = "ad",
								distr = c("norm", "snorm", "lnorm", "gamma", "weibull"),
								col = list(line = "blue", points = "lightblue", prob = "black"))
			}

			####
			if(str_trim(tclvalue(EnvPICSAplot$typeTSp))%in%c("ENSO-Line", "ENSO-Barplot", "ENSO-Proba")){
				ijoni <- getIndexSeasonVars(as.numeric(EnvPICSA$output$Onset.date[, ixy]),
							as.numeric(EnvPICSA$output$Cessation.date[, ixy]),
							EnvPICSA$ONI$date, "monthly")
				oni <- sapply(ijoni, function(x) mean(EnvPICSA$ONI$data[x], na.rm = TRUE))
				oni <- ifelse(oni >= 0.5, 3, ifelse(oni <= -0.5, 1, 2))
			}

			####
			if(str_trim(tclvalue(EnvPICSAplot$typeTSp)) == "ENSO-Line"){
				plt.avg <- if(tclvalue(EnvPICSAplot$averageTSp) == "1") TRUE else FALSE
				plt.terc <- if(tclvalue(EnvPICSAplot$tercileTSp) == "1") TRUE else FALSE
				plt.trd <- if(tclvalue(EnvPICSAplot$trendTSp) == "1") TRUE else FALSE
				picsa.plot.line.ENSO(xaxe, don, oni, origindate = origindate, 
								sub = sub, xlab = xlab, ylab = ylab, title = title,
								mean = plt.avg, tercile = plt.terc, linear = plt.trd,
								axis.font = 1, start.zero = FALSE,
								col = list(line = "black", points = c("blue", "gray", "red")),
								col.add = list(mean = "darkblue", tercile1 = "chartreuse4", tercile2 = "darkgoldenrod4", linear = "purple3"))
			}

			if(str_trim(tclvalue(EnvPICSAplot$typeTSp)) == "ENSO-Barplot"){
				picsa.plot.bar.ENSO(xaxe, don, oni, origindate = origindate,
								sub = sub, xlab = xlab, ylab = ylab, title = title,
								barcol = c("blue", "gray", "red"), axis.font = 1, start.zero = FALSE)
			}

			if(str_trim(tclvalue(EnvPICSAplot$typeTSp)) == "ENSO-Proba"){
				picsa.plot.proba.ENSO(don, oni, origindate = origindate, sub = sub, xlab = ylab,
									title = title, axis.font = 1,
									col.all = list(line = "black", points = "lightgray"),
									col.nino = list(line = "red", points = "lightpink"),
									col.nina = list(line = "blue", points = "lightblue"),
									col.neutre = list(line = "gray", points = "lightgray"))
			}
		}
	}else{
		don <- EnvPICSA$cdtPrecip$data[, ixy]
		picsa.plot.daily(EnvPICSA$cdtPrecip$dates, don, EnvPICSA$thres.rain.day)
	}
}

######################################################################################################

PICSA.DisplayMaps <- function(parent, ocrds){
	varplot <- c("parPlotSize1", "parPlotSize2", "parPlotSize3", "parPlotSize4",
				 "usrCoords1", "usrCoords2", "usrCoords3", "usrCoords4")
	parPltCrd <- setNames(lapply(varplot, function(x) assign(x, tclVar(), env = parent.frame())), varplot)

	plotIt <- function(){
		op <- par(bg = "white")
		pltusr <- PICSA.plotTSMaps(ocrds)
		par(op)
		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvPICSAplot$notebookTab.maps, 'Time-Series-Maps', AllOpenTabType, AllOpenTabData)
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	#########
	tkbind(img, "<Motion>", function(W, x, y){
		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		xydisp <- LatLonLabels(xyMouse$x, xyMouse$y)
		frxcoord <- ifelse(xyMouse$inout, '', xydisp$xdisp)
		frycoord <- ifelse(xyMouse$inout, '', xydisp$ydisp)

		if(EnvPICSA$data.type == "cdt"){
			fdispIdStn <- function(x){
				y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvPICSA$cdtPrecip$lon)^2 + (xyMouse$y-EnvPICSA$cdtPrecip$lat)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			frzcoord <- ifelse(xyMouse$inout | rayondisp, '', EnvPICSA$cdtPrecip$id[inear])
		}else{
			frzcoord <- ""
		}

		tclvalue(xpcoord) <- frxcoord
		tclvalue(ypcoord) <- frycoord
		tclvalue(zpcoord) <- frzcoord
	})

	tkbind(img, "<Button-1>", function(W, x, y){
		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		if(EnvPICSA$data.type == "cdt"){
			fdispIdStn <- function(x){
				 y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvPICSA$cdtPrecip$lon)^2 + (xyMouse$y-EnvPICSA$cdtPrecip$lat)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			if(!(xyMouse$inout | rayondisp)){
				tclvalue(EnvPICSAplot$stnIDTSp) <- EnvPICSA$cdtPrecip$id[inear]
				plotTS <- TRUE
			}else plotTS <- FALSE
		}else{
			if(!xyMouse$inout){
				tclvalue(EnvPICSAplot$lonLOC) <- round(xyMouse$x, 6)
				tclvalue(EnvPICSAplot$latLOC) <- round(xyMouse$y, 6)
				plotTS <- TRUE
			}else plotTS <- FALSE
		}

		if(plotTS){
			imgContainer <- PICSA.DisplayTSPlot(tknotes)
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

################################

PICSA.DisplayClimMaps <- function(parent, ocrds){
	varplot <- c("parPlotSize1", "parPlotSize2", "parPlotSize3", "parPlotSize4",
				 "usrCoords1", "usrCoords2", "usrCoords3", "usrCoords4")
	parPltCrd <- setNames(lapply(varplot, function(x) assign(x, tclVar(), env = parent.frame())), varplot)

	plotIt <- function(){
		op <- par(bg = "white")
		pltusr <- PICSA.plotClimMaps(ocrds)
		par(op)
		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvPICSAplot$notebookTab.clmaps, 'Clim-Analysis-Maps', AllOpenTabType, AllOpenTabData)
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	#########
	tkbind(img, "<Motion>", function(W, x, y){
		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		xydisp <- LatLonLabels(xyMouse$x, xyMouse$y)
		frxcoord <- ifelse(xyMouse$inout, '', xydisp$xdisp)
		frycoord <- ifelse(xyMouse$inout, '', xydisp$ydisp)

		if(EnvPICSA$data.type == "cdt"){
			fdispIdStn <- function(x){
				y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvPICSA$cdtPrecip$lon)^2 + (xyMouse$y-EnvPICSA$cdtPrecip$lat)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			frzcoord <- ifelse(xyMouse$inout | rayondisp, '', EnvPICSA$cdtPrecip$id[inear])
		}else{
			frzcoord <- ""
		}

		tclvalue(xpcoord) <- frxcoord
		tclvalue(ypcoord) <- frycoord
		tclvalue(zpcoord) <- frzcoord
	})

	tkbind(img, "<Button-1>", function(W, x, y){
		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		if(EnvPICSA$data.type == "cdt"){
			fdispIdStn <- function(x){
				 y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvPICSA$cdtPrecip$lon)^2 + (xyMouse$y-EnvPICSA$cdtPrecip$lat)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			if(!(xyMouse$inout | rayondisp)){
				tclvalue(EnvPICSAplot$stnIDTSp) <- EnvPICSA$cdtPrecip$id[inear]
				plotTS <- TRUE
			}else plotTS <- FALSE
		}else{
			if(!xyMouse$inout){
				tclvalue(EnvPICSAplot$lonLOC) <- round(xyMouse$x, 6)
				tclvalue(EnvPICSAplot$latLOC) <- round(xyMouse$y, 6)
				plotTS <- TRUE
			}else plotTS <- FALSE
		}

		if(plotTS){
			imgContainer <- PICSA.DisplayTSPlot(tknotes)
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

################################

PICSA.DisplayTSPlot <- function(parent){
	plotIt <- function(){
		PICSA.plotTSGraph()
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







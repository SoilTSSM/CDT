PICSA.plotTSMaps <- function(ocrds){
	iyear <- as.numeric(str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.year))))

	#################

	if(tclvalue(EnvPICSAplot$varPICSA) == "Onset"){
		don <- EnvPICSA$output$Onset.nb[EnvPICSA$index$onsetYear == iyear, ]
		texta <- NULL
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Cessation"){
		don <- EnvPICSA$output$Cessation.nb[EnvPICSA$index$cessatYear == iyear, ]
		texta <- NULL
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Season Length"){
		don <- EnvPICSA$output$SeasonLength[EnvPICSA$index$onsetYear == iyear, ]
		texta <- 'Season length (days)'
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Seasonal Rainfall Amounts"){
		don <- EnvPICSA$picsa$RainTotal[EnvPICSA$index$onsetYear == iyear, ]
		texta <- 'Seasonal Rainfall Amount (mm)'
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Dry Spells"){
		don <- EnvPICSA$picsa$AllDrySpell[EnvPICSA$index$onsetYear == iyear, ]
		drydef <- as.numeric(str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.dryspell))))
		don <- sapply(don, function(x) sum(x >= drydef))
		texta <- paste0('Number of Dry Spells', '\n', "Dry spells - ", drydef, " or more consecutive days")
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Longest Dry Spell"){
		don <- EnvPICSA$picsa$AllDrySpell[EnvPICSA$index$onsetYear == iyear, ]
		don <- sapply(don, max)
		texta <- 'Longest dry spell (days)'
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Number of rain day"){
		don <- EnvPICSA$picsa$nbdayrain[EnvPICSA$index$onsetYear == iyear, ]
		texta <- 'Number of rainy days (days)'
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Maximum daily rain"){
		don <- EnvPICSA$picsa$max24h[EnvPICSA$index$onsetYear == iyear, ]
		texta <- 'Seasonal maximum daily rainfall depth (mm)'
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Total rain when RR>95thPerc"){
		don <- EnvPICSA$picsa$TotalQ95th[EnvPICSA$index$onsetYear == iyear, ]
		texta <- 'Seasonal total of precipitation\nwhen RR > 95th percentile (days)'
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Nb of day when RR>95thPerc"){
		don <- EnvPICSA$picsa$NbQ95th[EnvPICSA$index$onsetYear == iyear, ]
		texta <- 'Seasonal count of days when\nRR > 95th percentile (days)'
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Maximum temperature"){
		don <- EnvPICSA$picsa$tmax[EnvPICSA$index$onsetYear == iyear, ]
		texta <- 'Seasonal average maximum temperature (°C)'
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Minimum temperature"){
		don <- EnvPICSA$picsa$tmin[EnvPICSA$index$onsetYear == iyear, ]
		texta <- 'Seasonal average minimum temperature (°C)'
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

	kolFonction <- match.fun("tim.colors")

	breaks <- pretty(range(don$z, na.rm = TRUE))
	breaks <- if(length(breaks > 0)) breaks else c(0, 1) 
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

	if(length(xna) > 0) points(xna, yna, pch = '*')
	# image.plot(don, breaks = breaks, col = kolor, lab.breaks = legendLabel, horizontal = horizontal,
	# 			xaxt = 'n', yaxt = 'n', add = TRUE, legend.mar = legend.mar, legend.width = legend.width,
	# 			legend.args = legend.args, axis.args = list(cex.axis = 0.5, font = 2, col.axis = 4))
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
	if(tclvalue(EnvPICSAplot$analysis.method) == "Average") aggrFonction <- function(Y, X = NULL, O = NULL) base::colMeans(Y, na.rm = TRUE)
	if(tclvalue(EnvPICSAplot$analysis.method) == "Median") aggrFonction <- function(Y, X = NULL, O = NULL) matrixStats::colMedians(Y, na.rm = TRUE)
	if(tclvalue(EnvPICSAplot$analysis.method) == "Standard deviation") aggrFonction <- function(Y, X = NULL, O = NULL) matrixStats::colSds(Y, na.rm = TRUE)
	if(tclvalue(EnvPICSAplot$analysis.method) == "Trend") 
		aggrFonction <- function(Y, X = NULL, O = NULL){
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
	if(tclvalue(EnvPICSAplot$analysis.method) == "Percentiles")
		aggrFonction <- function(Y, X = NULL, O = NULL){
			Q <- as.numeric(tclvalue(EnvPICSAplot$mth.perc))/100
			apply(Y, 2, quantile8, probs = Q)
			matrixStats::colQuantiles(Y, probs = Q, na.rm = TRUE)
		}
	if(tclvalue(EnvPICSAplot$analysis.method) == "Frequency")
		aggrFonction <- function(Y, X = NULL, O = NULL){
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
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Cessation"){
		don <- EnvPICSA$output$Cessation.nb
		dimdon <- dim(don)
		don <- as.numeric(don)
		dim(don) <- dimdon
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Season Length"){
		don <- EnvPICSA$output$SeasonLength
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Seasonal Rainfall Amounts"){
		don <- EnvPICSA$picsa$RainTotal
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
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Longest Dry Spell"){
		don <- EnvPICSA$picsa$AllDrySpell
		dimdon <- dim(don)
		don <- sapply(don, max)
		dim(don) <- dimdon
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Number of rain day"){
		don <- EnvPICSA$picsa$nbdayrain
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Maximum daily rain"){
		don <- EnvPICSA$picsa$max24h
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Total rain when RR>95thPerc"){
		don <- EnvPICSA$picsa$TotalQ95th
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Nb of day when RR>95thPerc"){
		don <- EnvPICSA$picsa$NbQ95th
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Maximum temperature"){
		don <- EnvPICSA$picsa$tmax
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Minimum temperature"){
		don <- EnvPICSA$picsa$tmin
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

	kolFonction <- match.fun("tim.colors")

	breaks <- pretty(range(don$z, na.rm = TRUE))
	kolor <- kolFonction(length(breaks)-1)

	if(diff(EnvPICSAplot$xlim.maps) > diff(EnvPICSAplot$ylim.maps)){
		horizontal <- TRUE
		legend.mar <- 3.5
		legend.width <- 0.7
		mar <- c(7, 4, 2.5, 2.5)
		# legend.args <- list(text = "unknown units", col = "black", font = 2, cex = 0.8, side = 1, line = 2)
		legend.args = NULL
	}else{
		horizontal <- FALSE
		legend.mar <- 6.2
		mar <- c(4, 4, 2.5, 6)
		legend.width <- 0.9
		# legend.args <- list(text = "unknown units", col = "black", font = 2, cex = 0.8, side = 4, line = 3)
		legend.args = NULL
	}

	#################

	if(tclvalue(EnvPICSAplot$varPICSA) == "Onset" & tclvalue(EnvPICSAplot$analysis.method)%in%c("Average", "Median", "Percentiles")){
		legendLabel <- format(as.Date(breaks, origin = EnvPICSA$index$onsetOrigDate), '%d %b')
	}else if(tclvalue(EnvPICSAplot$varPICSA) == "Cessation" & tclvalue(EnvPICSAplot$analysis.method)%in%c("Average", "Median", "Percentiles")){
		legendLabel <- format(as.Date(breaks, origin = EnvPICSA$index$cessatOrigDate), '%d %b')
	}else legendLabel <- breaks

	#################

	opar <- par(mar = mar)
	plot(1, xlim = EnvPICSAplot$xlim.maps, ylim = EnvPICSAplot$ylim.maps, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
	axlabsFun <- if(Sys.info()["sysname"] == "Windows") LatLonAxisLabels else LatLonAxisLabels1
	axlabs <- axlabsFun(axTicks(1), axTicks(2))
	axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tcl = -0.2, cex.axis = 0.8)
	axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tcl = -0.2, las = 1, cex.axis = 0.8)

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
			}
			if(tclvalue(EnvPICSAplot$varPICSA) == "Cessation"){
				don <- EnvPICSA$output$Cessation.nb[, ixy]
				xlab <- ''
				ylab <- ''
				sub <- NULL
				theoretical <- FALSE
			}
			if(tclvalue(EnvPICSAplot$varPICSA) == "Season Length"){
				don <- EnvPICSA$output$SeasonLength[, ixy]
				xlab <- 'Year'
				ylab <- 'Number of Days'
				sub <- NULL
				theoretical <- TRUE
			}
			if(tclvalue(EnvPICSAplot$varPICSA) == "Seasonal Rainfall Amounts"){
				don <- EnvPICSA$picsa$RainTotal[, ixy]
				xlab <- 'Year'
				ylab <- 'Rainfall Amount (mm)'
				sub <- NULL
				theoretical <- TRUE
			}
			if(tclvalue(EnvPICSAplot$varPICSA) == "Dry Spells"){
				don <- EnvPICSA$picsa$AllDrySpell[, ixy]
				drydef <- as.numeric(str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.dryspell))))
				don <- sapply(don, function(x) sum(x >= drydef))
				xlab <- 'Year'
				ylab <- 'Number of Dry Spells'
				sub <- paste("Dry spells -", drydef, "or more consecutive days")
				theoretical <- FALSE
			}
			if(tclvalue(EnvPICSAplot$varPICSA) == "Longest Dry Spell"){
				don <- EnvPICSA$picsa$AllDrySpell[, ixy]
				don <- sapply(don, max)
				xlab <- 'Year'
				ylab <- 'Longest dry spell (days)'
				sub <- NULL
				theoretical <- FALSE
			}
			if(tclvalue(EnvPICSAplot$varPICSA) == "Number of rain day"){
				don <- EnvPICSA$picsa$nbdayrain[, ixy]
				xlab <- 'Year'
				ylab <- 'Number of rainy days (days)'
				sub <- NULL
				theoretical <- TRUE
			}
			if(tclvalue(EnvPICSAplot$varPICSA) == "Maximum daily rain"){
				don <- EnvPICSA$picsa$max24h[, ixy]
				xlab <- 'Year'
				ylab <- 'Seasonal maximum daily rainfall depth (mm)'
				sub <- NULL
				theoretical <- FALSE
			}
			if(tclvalue(EnvPICSAplot$varPICSA) == "Total rain when RR>95thPerc"){
				don <- EnvPICSA$picsa$TotalQ95th[, ixy]
				xlab <- 'Year'
				ylab <- 'Seasonal total of precipitation\nwhen RR > 95th percentile (days)'
				sub <- NULL
				theoretical <- FALSE
			}
			if(tclvalue(EnvPICSAplot$varPICSA) == "Nb of day when RR>95thPerc"){
				don <- EnvPICSA$picsa$NbQ95th[, ixy]
				xlab <- 'Year'
				ylab <- 'Seasonal count of days when\nRR > 95th percentile (days)'
				sub <- NULL
				theoretical <- FALSE
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
								sub = sub, xlab = xlab, ylab = ylab,
								mean = plt.avg, tercile = plt.terc, linear = plt.trd,
								col = list(line = "red", points = "blue"), axis.font = 1, start.zero = FALSE,
								col.add = list(mean = "black", tercile1 = "green", tercile2 = "blue", linear = "purple3"))
			}

			if(str_trim(tclvalue(EnvPICSAplot$typeTSp)) == "Barplot"){
				picsa.plot.bar(xaxe, don, origindate = origindate,
								sub = sub, xlab = xlab, ylab = ylab,
								barcol = "darkblue", axis.font = 1, start.zero = FALSE)
			}

			if(str_trim(tclvalue(EnvPICSAplot$typeTSp)) == "Probability"){
				picsa.plot.proba(don, origindate = origindate, sub = sub, xlab = ylab, axis.font = 1,
								theoretical = theoretical,  gof.c = "ad",
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
								sub = sub, xlab = xlab, ylab = ylab,
								mean = plt.avg, tercile = plt.terc, linear = plt.trd,
								axis.font = 1, start.zero = FALSE,
								col = list(line = "black", points = c("blue", "gray", "red")),
								col.add = list(mean = "darkblue", tercile1 = "chartreuse4", tercile2 = "darkgoldenrod4", linear = "purple3"))
			}

			if(str_trim(tclvalue(EnvPICSAplot$typeTSp)) == "ENSO-Barplot"){
				picsa.plot.bar.ENSO(xaxe, don, oni, origindate = origindate,
								sub = sub, xlab = xlab, ylab = ylab,
								barcol = c("blue", "gray", "red"), axis.font = 1, start.zero = FALSE)
			}

			if(str_trim(tclvalue(EnvPICSAplot$typeTSp)) == "ENSO-Proba"){
				picsa.plot.proba.ENSO(don, oni, origindate = origindate, sub = sub, xlab = ylab, axis.font = 1,
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







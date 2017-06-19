PICSA.plotTSMaps <- function(ocrds){
	iyear <- as.numeric(str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.year))))

	if(tclvalue(EnvPICSAplot$varPICSA) == "Onset"){
		don <- EnvPICSA$output$Onset.nb[EnvPICSA$index$onsetYear == iyear, ]
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Cessation"){
		don <- EnvPICSA$output$Cessation.nb[EnvPICSA$index$cessatYear == iyear, ]
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Season Length"){
		don <- EnvPICSA$output$SeasonLength[EnvPICSA$index$onsetYear == iyear, ]
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Seasonal Rainfall Amounts"){
		don <- EnvPICSA$picsa$RainTotal[EnvPICSA$index$onsetYear == iyear, ]
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Dry Spells"){
		don <- EnvPICSA$picsa$AllDrySpell[EnvPICSA$index$onsetYear == iyear, ]
		don <- sapply(don, function(x) sum(x >= as.numeric(str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.dryspell))))))
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Longest Dry Spell"){
		don <- EnvPICSA$picsa$AllDrySpell[EnvPICSA$index$onsetYear == iyear, ]
		don <- sapply(don, max)
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Number of rain day"){
		don <- EnvPICSA$picsa$nbdayrain[EnvPICSA$index$onsetYear == iyear, ]
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Maximum daily rain"){
		don <- EnvPICSA$picsa$max24h[EnvPICSA$index$onsetYear == iyear, ]
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Total rain when RR>95thPerc"){
		don <- EnvPICSA$picsa$TotalQ95th[EnvPICSA$index$onsetYear == iyear, ]
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Nb of day when RR>95thPerc"){
		don <- EnvPICSA$picsa$NbQ95th[EnvPICSA$index$onsetYear == iyear, ]
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Maximum temperature"){
		don <- EnvPICSA$picsa$tmax[EnvPICSA$index$onsetYear == iyear, ]
	}
	if(tclvalue(EnvPICSAplot$varPICSA) == "Minimum temperature"){
		don <- EnvPICSA$picsa$tmin[EnvPICSA$index$onsetYear == iyear, ]
	}

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
		legendPlace <- TRUE
		legend.mar <- 3.5
		legend.width <- 0.8
		mar <- c(7, 4, 2.5, 2.5)
		# legend.args <- list(text = "unknown units", col = "black", font = 2, cex = 0.8, side = 1, line = 2)
		legend.args = NULL
	}else{
		legendPlace <- FALSE
		legend.mar <- 5
		mar <- c(4, 4, 2.5, 6)
		legend.width <- 1
		# legend.args <- list(text = "unknown units", col = "black", font = 2, cex = 0.8, side = 4, line = 2)
		legend.args = NULL
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
	axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tck = -0.01, cex.axis = 0.8)
	axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tck = -0.01, las = 1, cex.axis = 0.8)

	if(length(xna) > 0)points(xna, yna, pch = '*')
	image.plot(don, breaks = breaks, col = kolor, lab.breaks = legendLabel, horizontal = legendPlace,
				xaxt = 'n', yaxt = 'n', add = TRUE, legend.mar = legend.mar, legend.width = legend.width,
				legend.args = legend.args)
	abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)
	lines(ocrds[, 1], ocrds[, 2])

	plt <- par("plt")
	usr <- par("usr")
	par(opar)
	return(list(par = c(plt, usr)))
}

##############################

######################################################################################################

PICSA.DisplayMaps <- function(parent, ocrds){

	########PLOT
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
	onglet <- imageNotebookTab_open(parent, EnvPICSAplot$notebookTab.maps, 'TS MAPS', AllOpenTabType, AllOpenTabData)
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
			}	
		}else{
			if(!xyMouse$inout){
				tclvalue(EnvPICSAplot$lonLOC) <- round(xyMouse$x, 6)
				tclvalue(EnvPICSAplot$latLOC) <- round(xyMouse$y, 6)
			}
		}

		##plot time series here


	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}

################################

PICSA.DisplayClimMaps <- function(parent, ocrds){

}


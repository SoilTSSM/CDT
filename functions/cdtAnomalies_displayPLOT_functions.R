
anomaliesCalc.plotAnomMaps <- function(){
	don <- EnvAnomalyCalcPlot$anomdata$map
	anomMapOp <- EnvAnomalyCalcPlot$anomMapOp

	## titre
	if(!anomMapOp$title$user){
		# titre1 <- switch(EnvAnomalyCalcPlot$output$params$intstep,
		# 				"daily" = "Daily",
		# 				"pentad" = "Pentad",
		# 				"dekadal" = "Dekadal",
		# 				"monthly" = "Monthly")
		titre1 <- switch(EnvAnomalyCalcPlot$output$params$anomaly,
						"Difference" = "Anomaly:",
						"Percentage" = "Anomaly (% of Mean):",
						"Standardized" = "Standardized Anomaly:")
		titre <- paste(titre1, str_trim(tclvalue(EnvAnomalyCalcPlot$anomDate)))
	}else titre <- anomMapOp$title$title

	#################
	## colorscale title
	if(anomMapOp$colkeyLab$user){
		legend.texta <- anomMapOp$colkeyLab$label
	}else legend.texta <- NULL

	#################
	## breaks
	brks <- image.plot_Legend_pars(don$z, anomMapOp$userLvl, anomMapOp$userCol, anomMapOp$presetCol)
	breaks <- brks$breaks
	zlim <- brks$legend.breaks$zlim
	breaks2 <- brks$legend.breaks$breaks
	kolor <- brks$colors
	breaks1 <- brks$legend.axis$at
	lab.breaks <- brks$legend.axis$labels

	## legend label
	legendLabel <- lab.breaks

	#################
	### shape files
	shpf <- EnvAnomalyCalcPlot$shp
	ocrds <- if(tclvalue(shpf$add.shp) == "1" & !is.null(shpf$ocrds)) shpf$ocrds else matrix(NA, 1, 2)

	#################

	if(all(is.na(ocrds[, 1])) | all(is.na(ocrds[, 2]))){
		xlim <- range(don$x, na.rm = TRUE)
		ylim <- range(don$y, na.rm = TRUE)
	}else{
		xlim <- range(range(don$x, na.rm = TRUE), range(ocrds[, 1], na.rm = TRUE))
		ylim <- range(range(don$y, na.rm = TRUE), range(ocrds[, 2], na.rm = TRUE))
	}

	#################

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

	#################

	opar <- par(mar = mar)
	plot(1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
	axlabsFun <- if(Sys.info()["sysname"] == "Windows") LatLonAxisLabels else LatLonAxisLabels1
	axlabs <- axlabsFun(axTicks(1), axTicks(2))
	axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tcl = -0.2, cex.axis = 0.8)
	axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tcl = -0.2, las = 1, cex.axis = 0.8)
	title(main = titre, cex.main = 1, font.main = 2)

	image(don, breaks = breaks, col = kolor, xaxt = 'n', yaxt = 'n', add = TRUE)
	image.plot(zlim = zlim, breaks = breaks2, col = kolor, horizontal = horizontal,
				legend.only = TRUE, legend.mar = legend.mar, legend.width = legend.width,
				legend.args = legend.args, axis.args = list(at = breaks1, labels = legendLabel,
				cex.axis = 0.7, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)), legend.shrink = 0.8)

	abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)
	lines(ocrds[, 1], ocrds[, 2], lwd = EnvAnomalyCalcPlot$SHPOp$lwd, col = EnvAnomalyCalcPlot$SHPOp$col)

	## scale bar
	if(anomMapOp$scalebar$add){
		if(anomMapOp$scalebar$pos == 'bottomleft') posx <- 0.05
		if(anomMapOp$scalebar$pos == 'bottomcenter') posx <- 0.425
		if(anomMapOp$scalebar$pos == 'bottomright') posx <- 0.75
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

anomaliesCalc.plotAnomGraph <- function(){
	TSGraphOp <- EnvAnomalyCalcPlot$TSGraphOp

	if(EnvAnomalyCalcPlot$output$params$data.type == "cdtstation"){
		ixy <- which(EnvAnomalyCalcPlot$output$data$id == str_trim(tclvalue(EnvAnomalyCalcPlot$graph$stnIDTSp)))
		if(length(ixy) == 0){
			InsertMessagesTxt(main.txt.out, "Station not found", format = TRUE)
			return(NULL)
		}
		don <- EnvAnomalyCalcPlot$anomdata$data[, ixy]
		daty <- EnvAnomalyCalcPlot$output$data$dates
		EnvAnomalyCalcPlot$location <- paste0("Station: ", EnvAnomalyCalcPlot$output$data$id[ixy])
		titre3 <- paste0("(", EnvAnomalyCalcPlot$output$data$id[ixy], ")")
	}else{
		cdtdataset <- EnvAnomalyCalcPlot$cdtdataset
		xlon <- cdtdataset$coords$mat$x
		xlat <- cdtdataset$coords$mat$y
		ilon <- as.numeric(str_trim(tclvalue(EnvAnomalyCalcPlot$graph$lonLOC)))
		ilat <- as.numeric(str_trim(tclvalue(EnvAnomalyCalcPlot$graph$latLOC)))

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
		daty <- cdtdataset$dateInfo$date
		EnvAnomalyCalcPlot$location <- paste0("Longitude: ", round(ilon, 5), ", Latitude: ", round(ilat, 5))
		titre3 <- ""
	}

	#########

	if(EnvAnomalyCalcPlot$output$params$intstep == "daily"){
		titre1 <- "Daily"
		daty <- as.Date(daty, "%Y%m%d")
	}
	if(EnvAnomalyCalcPlot$output$params$intstep == "pentad"){
		titre1 <- "Pentad"
		seqtime <- as.Date(daty, "%Y%m%d")
		daty <- as.Date(paste0(format(seqtime, "%Y-%m-"), c(1, 6, 11, 16, 21, 26)[as.numeric(format(seqtime, "%d"))]))
	}
	if(EnvAnomalyCalcPlot$output$params$intstep == "dekadal"){
		titre1 <- "Dekadal"
		seqtime <- as.Date(daty, "%Y%m%d")
		daty <- as.Date(paste0(format(seqtime, "%Y-%m-"), c(1, 11, 21)[as.numeric(format(seqtime, "%d"))]))
	}
	if(EnvAnomalyCalcPlot$output$params$intstep == "monthly"){
		titre1 <- "Monthly"
		daty <- as.Date(paste0(daty, "01"), "%Y%m%d")
	}

	titre2 <- switch(EnvAnomalyCalcPlot$output$params$anomaly,
					"Difference" = "Anomaly",
					"Percentage" = "Anomaly (% of Mean)",
					"Standardized" = "Standardized Anomaly")

	titre <- paste(titre1, titre2, titre3)

	#########

	GRAPHTYPE <- str_trim(tclvalue(EnvAnomalyCalcPlot$graph$typeTSp))
	if(GRAPHTYPE == "Line") optsgph <- TSGraphOp$line
	if(GRAPHTYPE == "Bar") optsgph <- TSGraphOp$anomaly

	xlim <- range(daty, na.rm = TRUE)
	if(optsgph$xlim$is.min){
		xx <- strsplit(optsgph$xlim$min, "-")[[1]]
		x3 <- as.numeric(xx[3])
		if(EnvAnomalyCalcPlot$output$params$intstep == "pentad"){
			if(is.na(x3) | x3 < 1 | x3 > 6){
				InsertMessagesTxt(main.txt.out, "xlim: pentad must be  between 1 and 6", format = TRUE)
				return(NULL)
			}
			x3 <- c(1, 6, 11, 16, 21, 26)[x3]
		}
		if(EnvAnomalyCalcPlot$output$params$intstep == "dekadal"){
			if(is.na(x3) | x3 < 1 | x3 > 3){
				InsertMessagesTxt(main.txt.out, "xlim: dekad must be 1, 2 or 3", format = TRUE)
				return(NULL)
			}
			x3 <- c(1, 11, 21)[x3]
		}
		if(EnvAnomalyCalcPlot$output$params$intstep == "monthly") x3 <- 1
		x1 <- as.numeric(xx[1])
		x2 <- str_pad(as.numeric(xx[2]), 2, pad = "0")
		x3 <- str_pad(x3, 2, pad = "0")
		xx <- as.Date(paste0(x1, x2, x3), "%Y%m%d")
		if(is.na(xx)){
			InsertMessagesTxt(main.txt.out, "xlim: invalid date", format = TRUE)
			return(NULL)
		}
		xlim[1] <- xx
	}
	if(optsgph$xlim$is.max){
		xx <- strsplit(optsgph$xlim$max, "-")[[1]]
		x3 <- as.numeric(xx[3])
		if(EnvAnomalyCalcPlot$output$params$intstep == "pentad"){
			if(is.na(x3) | x3 < 1 | x3 > 6){
				InsertMessagesTxt(main.txt.out, "xlim: pentad must be  between 1 and 6", format = TRUE)
				return(NULL)
			}
			x3 <- c(1, 6, 11, 16, 21, 26)[x3]
		}
		if(EnvAnomalyCalcPlot$output$params$intstep == "dekadal"){
			if(is.na(x3) | x3 < 1 | x3 > 3){
				InsertMessagesTxt(main.txt.out, "xlim: dekad must be 1, 2 or 3", format = TRUE)
				return(NULL)
			}
			x3 <- c(1, 11, 21)[x3]
		}
		if(EnvAnomalyCalcPlot$output$params$intstep == "monthly") x3 <- 1
		x1 <- as.numeric(xx[1])
		x2 <- str_pad(as.numeric(xx[2]), 2, pad = "0")
		x3 <- str_pad(x3, 2, pad = "0")
		xx <- as.Date(paste0(x1, x2, x3), "%Y%m%d")
		if(is.na(xx)){
			InsertMessagesTxt(main.txt.out, "xlim: invalid date", format = TRUE)
			return(NULL)
		}
		xlim[2] <- xx
	}
	idt <- daty >= xlim[1] & daty <= xlim[2]
	daty <- daty[idt]
	don <- don[idt]
	ylim <- range(pretty(don))
	if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
	if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

	xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else ''
	ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else ''

	if(optsgph$title$is.title){
		titre <- optsgph$title$title
		titre.pos <- optsgph$title$position
	}else{
		titre <- titre
		titre.pos <- "top"
	}

	#########

	if(GRAPHTYPE == "Line"){
		graphs.plot.line(daty, don, xlim = xlim, ylim = ylim,
						xlab = xlab, ylab = ylab, ylab.sub = NULL,
						title = titre, title.position = titre.pos, axis.font = 1,
						plotl = optsgph$plot, legends = NULL,
						location = EnvAnomalyCalcPlot$location)
	}

	if(GRAPHTYPE == "Bar"){
		loko <- c(optsgph$colors$negative, optsgph$colors$positive)

		graphs.plot.bar.Anomaly(daty, don, period = NULL, percent = FALSE,
								xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ylab.sub = NULL,
								title = titre, title.position = titre.pos, axis.font = 1,
								barcol = loko, location = EnvAnomalyCalcPlot$location)
	}
}

##############################################################################

anomaliesCalc.Display.Maps <- function(parent){
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
		pltusr <- anomaliesCalc.plotAnomMaps()
		par(op)
		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvAnomalyCalcPlot$notebookTab.AnomMap, 'Anomaly-Map', AllOpenTabType, AllOpenTabData)
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	#########
	tkbind(img, "<Motion>", function(W, x, y){
		if(length(ls(EnvAnomalyCalcPlot)) == 0) return(NULL)
		if(is.null(EnvAnomalyCalcPlot$output)) return(NULL)

		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		xydisp <- LatLonLabels(xyMouse$x, xyMouse$y)
		frxcoord <- ifelse(xyMouse$inout, '', xydisp$xdisp)
		frycoord <- ifelse(xyMouse$inout, '', xydisp$ydisp)

		if(EnvAnomalyCalcPlot$output$params$data.type == "cdtstation"){
			fdispIdStn <- function(x){
				y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvAnomalyCalcPlot$output$data$lon)^2 + (xyMouse$y-EnvAnomalyCalcPlot$output$data$lat)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			frzcoord <- ifelse(xyMouse$inout | rayondisp, '', EnvAnomalyCalcPlot$output$data$id[inear])
		}else{
			frzcoord <- ""
		}

		tclvalue(xpcoord) <- frxcoord
		tclvalue(ypcoord) <- frycoord
		tclvalue(zpcoord) <- frzcoord
	})

	tkbind(img, "<Button-1>", function(W, x, y){
		if(length(ls(EnvAnomalyCalcPlot)) == 0) return(NULL)
		if(is.null(EnvAnomalyCalcPlot$output)) return(NULL)

		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		if(EnvAnomalyCalcPlot$output$params$data.type == "cdtstation"){
			fdispIdStn <- function(x){
				 y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvAnomalyCalcPlot$output$data$lon)^2 + (xyMouse$y-EnvAnomalyCalcPlot$output$data$lat)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			if(!(xyMouse$inout | rayondisp)){
				tclvalue(EnvAnomalyCalcPlot$graph$stnIDTSp) <- EnvAnomalyCalcPlot$output$data$id[inear]
				plotTS <- TRUE
			}else plotTS <- FALSE
		}else{
			if(!xyMouse$inout){
				tclvalue(EnvAnomalyCalcPlot$graph$lonLOC) <- round(xyMouse$x, 6)
				tclvalue(EnvAnomalyCalcPlot$graph$latLOC) <- round(xyMouse$y, 6)
				plotTS <- TRUE
			}else plotTS <- FALSE
		}

		if(plotTS){
			imgContainer <- anomaliesCalc.Display.Graph(tknotes)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvAnomalyCalcPlot$notebookTab.AnomGraph, AllOpenTabType, AllOpenTabData)
			EnvAnomalyCalcPlot$notebookTab.AnomGraph <- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}

#######################################

anomaliesCalc.Display.Graph <- function(parent){
	plotIt <- function(){
		anomaliesCalc.plotAnomGraph()
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvAnomalyCalcPlot$notebookTab.AnomGraph, 'Anomaly-Graph', AllOpenTabType, AllOpenTabData)
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

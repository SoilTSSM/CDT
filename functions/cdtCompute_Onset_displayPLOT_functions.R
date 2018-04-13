
OnsetCalc.plotOnsetMaps <- function(){
	don <- EnvOnsetCalcPlot$varData$map
	dataMapOp <- EnvOnsetCalcPlot$dataMapOp

	## titre
	if(!dataMapOp$title$user){
		titre <- paste("Starting dates of the rainy season:", str_trim(tclvalue(EnvOnsetCalcPlot$donDate)))
	}else titre <- dataMapOp$title$title

	## colorscale title
	if(dataMapOp$colkeyLab$user){
		legend.texta <- dataMapOp$colkeyLab$label
	}else legend.texta <- NULL

	## breaks
	if(!dataMapOp$userLvl$custom){
		breaks <- pretty(don$z, n = 10, min.n = 5)
		breaks <- if(length(breaks) > 0) breaks else c(0, 1) 
	}else breaks <- dataMapOp$userLvl$levels

	## colors
	if(dataMapOp$userCol$custom){
		kolFonction <- colorRampPalette(dataMapOp$userCol$color)
		kolor <- kolFonction(length(breaks)-1)
	}else{
		kolFonction <- match.fun(dataMapOp$presetCol$color)
		kolor <- kolFonction(length(breaks)-1)
		if(dataMapOp$presetCol$reverse) kolor <- rev(kolor)
	}

	### shape files
	shpf <- EnvOnsetCalcPlot$shp
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
	#################

	donDates <- format(EnvOnsetCalcPlot$output$start.date, "%Y")
	idt <- which(donDates == str_trim(tclvalue(EnvOnsetCalcPlot$donDate)))
	legendLabel <- format(breaks + EnvOnsetCalcPlot$output$start.date[idt], '%d-%b')
	# legendLabel <- breaks

	#################

	opar <- par(mar = mar)
	plot(1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
	axlabsFun <- if(Sys.info()["sysname"] == "Windows") LatLonAxisLabels else LatLonAxisLabels1
	axlabs <- axlabsFun(axTicks(1), axTicks(2))
	axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tcl = -0.2, cex.axis = 0.8)
	axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tcl = -0.2, las = 1, cex.axis = 0.8)
	title(main = titre, cex.main = 1, font.main= 2)

	if(dataMapOp$userLvl$equidist){
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

	lines(ocrds[, 1], ocrds[, 2], lwd = EnvOnsetCalcPlot$SHPOp$lwd, col = EnvOnsetCalcPlot$SHPOp$col)

	## scale bar
	if(dataMapOp$scalebar$add){
		if(dataMapOp$scalebar$pos == 'bottomleft') posx <- 0.05
		if(dataMapOp$scalebar$pos == 'bottomcenter') posx <- 0.425
		if(dataMapOp$scalebar$pos == 'bottomright') posx <- 0.75
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

OnsetCalc.plotOnsetGraph <- function(){
	TSGraphOp <- EnvOnsetCalcPlot$TSGraphOp

	if(EnvOnsetCalcPlot$output$params$data.type == "cdtstation"){
		ixy <- which(EnvOnsetCalcPlot$output$data$id == str_trim(tclvalue(EnvOnsetCalcPlot$graph$stnIDTSp)))
		if(length(ixy) == 0){
			InsertMessagesTxt(main.txt.out, "Station not found", format = TRUE)
			return(NULL)
		}
		don <- EnvOnsetCalcPlot$varData$data[, ixy]
		EnvOnsetCalcPlot$location <- paste0("Station: ", EnvOnsetCalcPlot$output$data$id[ixy])
		titre <- paste0("(", EnvOnsetCalcPlot$output$data$id[ixy], ")")
	}else{
		cdtdataset <- EnvOnsetCalcPlot$cdtdataset
		xlon <- cdtdataset$coords$mat$x
		xlat <- cdtdataset$coords$mat$y
		ilon <- as.numeric(str_trim(tclvalue(EnvOnsetCalcPlot$graph$lonLOC)))
		ilat <- as.numeric(str_trim(tclvalue(EnvOnsetCalcPlot$graph$latLOC)))

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
		don <- as.Date(don$data[, 1], origin = "1970-1-1")
		EnvOnsetCalcPlot$location <- paste0("Longitude: ", round(ilon, 5), ", Latitude: ", round(ilat, 5))
		titre <- ""
	}

	don <- as.numeric(don - EnvOnsetCalcPlot$output$start.date)
	daty <- as.numeric(format(EnvOnsetCalcPlot$output$start.date, "%Y"))
	origindate <- as.character(EnvOnsetCalcPlot$output$start.date[1])

	titre <- paste("Starting dates of the rainy season", titre)

	#########

	GRAPHTYPE <- str_trim(tclvalue(EnvOnsetCalcPlot$graph$typeTSp))

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

		xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else ''
		ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else ''

		if(optsgph$title$is.title){
			titre <- optsgph$title$title
			titre.pos <- optsgph$title$position
		}else{
			titre <- titre
			titre.pos <- "top"
		}

		climatoAnalysis.plot.line(daty, don, xlim = xlim, ylim = ylim, origindate = origindate,
									xlab = xlab, ylab = ylab, ylab.sub = NULL,
									title = titre, title.position = titre.pos, axis.font = 1,
									plotl = optsgph$plot, legends = NULL,
									location = EnvOnsetCalcPlot$location)
	}

	if(GRAPHTYPE == "Barplot"){
		optsgph <- TSGraphOp$bar
		xlim <- range(daty, na.rm = TRUE)
		if(optsgph$xlim$is.min) xlim[1] <- as.Date(optsgph$xlim$min)
		if(optsgph$xlim$is.max) xlim[2] <- as.Date(optsgph$xlim$max)
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

		climatoAnalysis.plot.bar(daty, don, xlim = xlim, ylim = ylim, origindate = origindate,
								xlab = xlab, ylab = ylab, ylab.sub = NULL,
								title = titre, title.position = titre.pos, axis.font = 1,
								barcol = optsgph$colors$col,
								location = EnvOnsetCalcPlot$location)
	}
}

##############################################################################

OnsetCalc.Display.Maps <- function(parent){
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
		pltusr <- OnsetCalc.plotOnsetMaps()
		par(op)
		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvOnsetCalcPlot$notebookTab.dataMap, 'Onset-Map', AllOpenTabType, AllOpenTabData)
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	#########
	tkbind(img, "<Motion>", function(W, x, y){
		if(length(ls(EnvOnsetCalcPlot)) == 0) return(NULL)
		if(is.null(EnvOnsetCalcPlot$output)) return(NULL)

		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		xydisp <- LatLonLabels(xyMouse$x, xyMouse$y)
		frxcoord <- ifelse(xyMouse$inout, '', xydisp$xdisp)
		frycoord <- ifelse(xyMouse$inout, '', xydisp$ydisp)

		if(EnvOnsetCalcPlot$output$params$data.type == "cdtstation"){
			fdispIdStn <- function(x){
				y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvOnsetCalcPlot$output$data$lon)^2 + (xyMouse$y-EnvOnsetCalcPlot$output$data$lat)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			frzcoord <- ifelse(xyMouse$inout | rayondisp, '', EnvOnsetCalcPlot$output$data$id[inear])
		}else{
			frzcoord <- ""
		}

		tclvalue(xpcoord) <- frxcoord
		tclvalue(ypcoord) <- frycoord
		tclvalue(zpcoord) <- frzcoord
	})

	tkbind(img, "<Button-1>", function(W, x, y){
		if(length(ls(EnvOnsetCalcPlot)) == 0) return(NULL)
		if(is.null(EnvOnsetCalcPlot$output)) return(NULL)

		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		if(EnvOnsetCalcPlot$output$params$data.type == "cdtstation"){
			fdispIdStn <- function(x){
				 y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvOnsetCalcPlot$output$data$lon)^2 + (xyMouse$y-EnvOnsetCalcPlot$output$data$lat)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			if(!(xyMouse$inout | rayondisp)){
				tclvalue(EnvOnsetCalcPlot$graph$stnIDTSp) <- EnvOnsetCalcPlot$output$data$id[inear]
				plotTS <- TRUE
			}else plotTS <- FALSE
		}else{
			if(!xyMouse$inout){
				tclvalue(EnvOnsetCalcPlot$graph$lonLOC) <- round(xyMouse$x, 6)
				tclvalue(EnvOnsetCalcPlot$graph$latLOC) <- round(xyMouse$y, 6)
				plotTS <- TRUE
			}else plotTS <- FALSE
		}

		if(plotTS){
			imgContainer <- OnsetCalc.Display.Graph(tknotes)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvOnsetCalcPlot$notebookTab.dataGraph, AllOpenTabType, AllOpenTabData)
			EnvOnsetCalcPlot$notebookTab.dataGraph <- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}

#######################################

OnsetCalc.Display.Graph <- function(parent){
	plotIt <- function(){
		OnsetCalc.plotOnsetGraph()
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvOnsetCalcPlot$notebookTab.dataGraph, 'Onset-Graph', AllOpenTabType, AllOpenTabData)
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


SPICalc.Plot.Map <- function(){
	don <- EnvSPICalcPlot$varData$map
	dataMapOp <- EnvSPICalcPlot$dataMapOp

	## titre
	if(!dataMapOp$title$user){
		titre <- paste(str_trim(tclvalue(EnvSPICalcPlot$spi.tscale)), ":", EnvSPICalcPlot$varData$spi$this.daty)
	}else titre <- dataMapOp$title$title

	#################
	## colorscale title
	if(dataMapOp$colkeyLab$user){
		legend.texta <- dataMapOp$colkeyLab$label
	}else legend.texta <- NULL

	#################
	## breaks
	brks <- image.plot_Legend_pars(don$z, dataMapOp$userLvl, dataMapOp$userCol, dataMapOp$presetCol)
	don$z <- don$z+1e-15
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
	shpf <- EnvSPICalcPlot$shp
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
	lines(ocrds[, 1], ocrds[, 2], lwd = EnvSPICalcPlot$SHPOp$lwd, col = EnvSPICalcPlot$SHPOp$col)

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

SPICalc.Plot.Graph <- function(){
	if(EnvSPICalcPlot$output$params$data.type == "cdtstation"){
		ixy <- which(EnvSPICalcPlot$output$data$id == str_trim(tclvalue(EnvSPICalcPlot$graph$stnIDTSp)))
		if(length(ixy) == 0){
			InsertMessagesTxt(main.txt.out, "Station not found", format = TRUE)
			return(NULL)
		}
		don <- as.numeric(EnvSPICalcPlot$cdtdataset$spi[, ixy])
		EnvSPICalcPlot$location <- paste0("Station: ", EnvSPICalcPlot$output$data$id[ixy])
		titre <- paste0("(", EnvSPICalcPlot$output$data$id[ixy], ")")
	}else{
		cdtdataset <- EnvSPICalcPlot$cdtdataset
		xlon <- cdtdataset$coords$mat$x
		xlat <- cdtdataset$coords$mat$y
		ilon <- as.numeric(str_trim(tclvalue(EnvSPICalcPlot$graph$lonLOC)))
		ilat <- as.numeric(str_trim(tclvalue(EnvSPICalcPlot$graph$latLOC)))

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
		EnvSPICalcPlot$location <- paste0("Longitude: ", round(ilon, 5), ", Latitude: ", round(ilat, 5))
		titre <- ""
	}

	daty <- EnvSPICalcPlot$varData$ts$dates
	if(EnvSPICalcPlot$varData$ts$step == "Dekad"){
		seqtime <- as.Date(daty, "%Y%m%d")
		daty <- as.Date(paste0(format(seqtime, "%Y-%m-"), c(1, 11, 21)[as.numeric(format(seqtime, "%d"))]))
	}
	if(EnvSPICalcPlot$varData$ts$step == "Month")
		daty <- as.Date(paste0(daty, "01"), "%Y%m%d")

	#########

	titre <- paste(str_trim(tclvalue(EnvSPICalcPlot$spi.tscale)), titre)

	#########
	TSGraphOp <- EnvSPICalcPlot$TSGraphOp

	GRAPHTYPE <- str_trim(tclvalue(EnvSPICalcPlot$graph$typeTSp))
	# if(GRAPHTYPE == "Bar-Line") optsgph <- TSGraphOp$bar.line
	# if(GRAPHTYPE == "Polygon") optsgph <- TSGraphOp$polygon
	optsgph <- TSGraphOp$bar.line

	xlim <- range(daty, na.rm = TRUE)
	if(optsgph$xlim$is.min){
		xx <- strsplit(optsgph$xlim$min, "-")[[1]]

		if(EnvSPICalcPlot$varData$ts$step == "Dekad"){
			x3 <- as.numeric(xx[3])
			if(is.na(x3) | x3 < 1 | x3 > 3){
				InsertMessagesTxt(main.txt.out, "xlim: dekad must be 1, 2 or 3", format = TRUE)
				return(NULL)
			}
			x3 <- c(1, 11, 21)[x3]
		}
		if(EnvSPICalcPlot$varData$ts$step == "Month") x3 <- 1
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
		if(EnvSPICalcPlot$varData$ts$step == "Dekad"){
			x3 <- as.numeric(xx[3])
			if(is.na(x3) | x3 < 1 | x3 > 3){
				InsertMessagesTxt(main.txt.out, "xlim: dekad must be 1, 2 or 3", format = TRUE)
				return(NULL)
			}
			x3 <- c(1, 11, 21)[x3]
		}
		if(EnvSPICalcPlot$varData$ts$step == "Month") x3 <- 1
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

	yticks <- if(optsgph$userYTcks$custom) optsgph$userYTcks$ticks else NULL
	loko <- c(optsgph$colors$negative, optsgph$colors$positive)

	#########

	if(GRAPHTYPE == "Bar-Line"){
		graphs.plot.bar.line(daty, don, y0 = optsgph$colors$y0, yticks = yticks,
						xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ylab.sub = NULL,
						title = titre, title.position = titre.pos, axis.font = 1,
						barcol = loko, plot.line = optsgph$line, location = EnvSPICalcPlot$location)
	}

	if(GRAPHTYPE == "Polygon"){
		graphs.plot.polygon(daty, don, y0 = optsgph$colors$y0, yticks = yticks,
						xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ylab.sub = NULL,
						title = titre, title.position = titre.pos, axis.font = 1,
						fillcol = loko, plot.line = optsgph$line, location = EnvSPICalcPlot$location)
	}
}

#######################################

SPICalc.Plot.VizTS <- function(){
	if(EnvSPICalcPlot$output$params$data.type == "cdtstation"){
		ixy <- which(EnvSPICalcPlot$output$data$id == str_trim(tclvalue(EnvSPICalcPlot$graph$stnIDTSp)))
		if(length(ixy) == 0){
			InsertMessagesTxt(main.txt.out, "Station not found", format = TRUE)
			return(NULL)
		}
		don <- as.numeric(EnvSPICalcPlot$spiViz$cdtdataset$data[, ixy])
		daty <- EnvSPICalcPlot$spiViz$cdtdataset$dates
		EnvSPICalcPlot$location <- paste0("Station: ", EnvSPICalcPlot$output$data$id[ixy])
		titre <- paste0("(", EnvSPICalcPlot$output$data$id[ixy], ")")
	}else{
		cdtdataset <- EnvSPICalcPlot$spiViz$cdtdataset
		xlon <- cdtdataset$coords$mat$x
		xlat <- cdtdataset$coords$mat$y
		ilon <- as.numeric(str_trim(tclvalue(EnvSPICalcPlot$graph$lonLOC)))
		ilat <- as.numeric(str_trim(tclvalue(EnvSPICalcPlot$graph$latLOC)))

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
		EnvSPICalcPlot$location <- paste0("Longitude: ", round(ilon, 5), ", Latitude: ", round(ilat, 5))
		titre <- ""
	}

	#################

	spiVizOpt <- EnvSPICalcPlot$spiVizOp
	tscales <- 1:as.numeric(str_trim(tclvalue(EnvSPICalcPlot$spiViz$max.tscale)))

	#################

	calculSPI <- list(EnvSPICalcPlot$spiViz$tstep, EnvSPICalcPlot$location, tscales)
	if(is.null(EnvSPICalcPlot$spiViz$calculSPI)){
		doCalcul <- TRUE
	}else{
		doCalcul <- if(!isTRUE(all.equal(EnvSPICalcPlot$spiViz$calculSPI, calculSPI))) TRUE else FALSE
	}

	if(doCalcul){
		if(EnvSPICalcPlot$spiViz$tstep == "dekadal"){
			don <- tapply(don, substr(daty, 1, 6), sum, na.rm = TRUE)
			daty <- names(don)
			don <- as.numeric(don)
		}
		don <- matrix(don, ncol = 1)
		daty <- as.Date(paste0(daty, "01"), "%Y%m%d")

		spi.mat <- lapply(tscales, function(tsc){
			SPEI_function(don, tscale = tsc, distribution = EnvSPICalcPlot$output$params$distr)
		})
		spi.mat <- do.call(cbind, spi.mat)
		EnvSPICalcPlot$spiViz$calculSPI <- calculSPI
		EnvSPICalcPlot$spiViz$spi.mat <- spi.mat
		EnvSPICalcPlot$spiViz$daty <- daty
	}else{
		spi.mat <- EnvSPICalcPlot$spiViz$spi.mat
		daty <- EnvSPICalcPlot$spiViz$daty
	}

	#################
	## titre
	if(!spiVizOpt$title$user){
		titre <- paste("Time-scales visualization", titre)
	}else titre <- spiVizOpt$title$title

	#################

	xlab <- if(spiVizOpt$axislabs$is.xlab) spiVizOpt$axislabs$xlab else ''
	ylab <- if(spiVizOpt$axislabs$is.ylab) spiVizOpt$axislabs$ylab else ''

	#################
	## colorscale title
	if(spiVizOpt$colkeyLab$user){
		legend.texta <- spiVizOpt$colkeyLab$label
	}else legend.texta <- NULL

	#################
	## breaks
	brks <- image.plot_Legend_pars(spi.mat, spiVizOpt$userLvl, spiVizOpt$userCol, spiVizOpt$presetCol)
	spi.mat <- spi.mat+1e-15
	breaks <- brks$breaks
	zlim <- brks$legend.breaks$zlim
	breaks2 <- brks$legend.breaks$breaks
	kolor <- brks$colors
	breaks1 <- brks$legend.axis$at
	lab.breaks <- brks$legend.axis$labels

	## legend label
	legendLabel <- lab.breaks

	#################

	xlim <- range(daty, na.rm = TRUE)
	ylim <- range(tscales)

	#################

	horizontal <- FALSE
	legend.mar <- 5.2
	mar <- c(4, 4, 2.5, 5.5)
	legend.width <- 0.9
	line <- if(max(nchar(as.character(breaks))) > 4) 3 else 2
	legend.args <- if(!is.null(legend.texta)) list(text = legend.texta, cex = 0.8, side = 4, line = line) else NULL

	#################

	opar <- par(mar = mar)
	plot(1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n', xaxs = "i")

	xTck <- axTicks.Date(daty, 1)
	yTck <- axTicks(2)

	if(as.numeric(diff(xlim)) > 1095){
		xminor <- seq(as.Date(paste0(format(xlim[1], "%Y"), "-01-01")),
					as.Date(paste0(as.numeric(format(xlim[2], "%Y"))+1, "-01-01")), "year")
		xminor <- xminor[!xminor%in%xTck]
	}else xminor <- NULL

	if(as.numeric(diff(ylim)) > 5){
		yminor <- seq(floor(ylim[1]), floor(ylim[2]), 1)
		yminor <- yminor[!yminor%in%yTck]
	}else yminor <- NULL

	axis.Date(1, at = xTck, cex.axis = 0.8)
	if(length(xminor) > 0) axis.Date(1, at = xminor, labels = NA, tcl = par("tcl")*0.5)
	axis(2, at = yTck, las = 1, cex.axis = 0.8)
	if(length(yminor) > 0) axis(2, at = yminor, labels = NA, tcl = par("tcl")*0.5)

	mtext(xlab, side = 1, line = 2.1)
	mtext(ylab, side = 2, line = 2.1)
	mtext(EnvSPICalcPlot$location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6)
	title(main = titre, cex.main = 1, font.main = 2)

	image(daty, tscales, spi.mat, breaks = breaks, col = kolor, xaxt = 'n', yaxt = 'n', add = TRUE)
	image.plot(zlim = zlim, breaks = breaks2, col = kolor, horizontal = horizontal,
				legend.only = TRUE, legend.mar = legend.mar, legend.width = legend.width,
				legend.args = legend.args, axis.args = list(at = breaks1, labels = legendLabel,
				cex.axis = 0.7, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)), legend.shrink = 0.8)

	abline(h = yTck, v = xTck, col = "lightgray", lty = 3)
	box()
	par(opar)
}

##############################################################################

SPICalc.Display.Maps <- function(parent){
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
		pltusr <- SPICalc.Plot.Map()
		par(op)
		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvSPICalcPlot$notebookTab.dataMap, 'SPI - Map', AllOpenTabType, AllOpenTabData)
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	#########
	tkbind(img, "<Motion>", function(W, x, y){
		if(length(ls(EnvSPICalcPlot)) == 0) return(NULL)
		if(is.null(EnvSPICalcPlot$output)) return(NULL)

		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		xydisp <- LatLonLabels(xyMouse$x, xyMouse$y)
		frxcoord <- ifelse(xyMouse$inout, '', xydisp$xdisp)
		frycoord <- ifelse(xyMouse$inout, '', xydisp$ydisp)

		if(EnvSPICalcPlot$output$params$data.type == "cdtstation"){
			fdispIdStn <- function(x){
				y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvSPICalcPlot$output$data$lon)^2 + (xyMouse$y-EnvSPICalcPlot$output$data$lat)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			frzcoord <- ifelse(xyMouse$inout | rayondisp, '', EnvSPICalcPlot$output$data$id[inear])
		}else{
			frzcoord <- ""
		}

		tclvalue(xpcoord) <- frxcoord
		tclvalue(ypcoord) <- frycoord
		tclvalue(zpcoord) <- frzcoord
	})

	tkbind(img, "<Button-1>", function(W, x, y){
		if(length(ls(EnvSPICalcPlot)) == 0) return(NULL)
		if(is.null(EnvSPICalcPlot$output)) return(NULL)

		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		if(EnvSPICalcPlot$output$params$data.type == "cdtstation"){
			fdispIdStn <- function(x){
				 y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvSPICalcPlot$output$data$lon)^2 + (xyMouse$y-EnvSPICalcPlot$output$data$lat)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			if(!(xyMouse$inout | rayondisp)){
				tclvalue(EnvSPICalcPlot$graph$stnIDTSp) <- EnvSPICalcPlot$output$data$id[inear]
				plotTS <- TRUE
			}else plotTS <- FALSE
		}else{
			if(!xyMouse$inout){
				tclvalue(EnvSPICalcPlot$graph$lonLOC) <- round(xyMouse$x, 6)
				tclvalue(EnvSPICalcPlot$graph$latLOC) <- round(xyMouse$y, 6)
				plotTS <- TRUE
			}else plotTS <- FALSE
		}

		if(plotTS){
			imgContainer <- SPICalc.Display.Graph(tknotes)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvSPICalcPlot$notebookTab.dataGraph, AllOpenTabType, AllOpenTabData)
			EnvSPICalcPlot$notebookTab.dataGraph <- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}

#######################################

SPICalc.Display.Graph <- function(parent){
	plotIt <- function(){
		SPICalc.Plot.Graph()
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvSPICalcPlot$notebookTab.dataGraph, 'SPI - Time Series', AllOpenTabType, AllOpenTabData)
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

#######################################

SPICalc.Display.VizTS <- function(parent){
	plotIt <- function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		op <- par(bg = "white")
		SPICalc.Plot.VizTS()
		par(op)
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvSPICalcPlot$notebookTab.spiViz, 'SPI - Time Scales', AllOpenTabType, AllOpenTabData)
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



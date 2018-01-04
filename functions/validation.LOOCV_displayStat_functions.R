
LOOCValidation.plotStatMaps <- function(){
	mapstat <- tclvalue(EnvLOOCValidationplot$statistics)
	istat <- which(EnvLOOCValidation$Statistics$STN$description == mapstat)
	don <- EnvLOOCValidation$Statistics$STN$statistics[istat, ]

	xna <- EnvLOOCValidation$opDATA$lon[is.na(don)]
	yna <- EnvLOOCValidation$opDATA$lat[is.na(don)]
	nx <- as.integer(diff(range(EnvLOOCValidation$opDATA$lon))/(0.0375*2.5))
	ny <- as.integer(diff(range(EnvLOOCValidation$opDATA$lat))/(0.0375*2.5))
	don <- as.image(don, x = cbind(EnvLOOCValidation$opDATA$lon, EnvLOOCValidation$opDATA$lat), nx = nx, ny = ny)

	ocrds <- EnvLOOCValidationplot$shp

	#################

	breaks <- pretty(don$z, n = 10, n.min = 7)
	breaks <- if(length(breaks) > 0) breaks else c(0, 1) 

	kolFonction <- match.fun("tim.colors")
	kolor <- kolFonction(length(breaks)-1)

	if(diff(EnvLOOCValidationplot$xlim.maps) > diff(EnvLOOCValidationplot$ylim.maps)){
		horizontal <- TRUE
		legend.mar <- 3.5
		legend.width <- 0.7
		mar <- c(7, 4, 2.5, 2.5)
		# legend.args <- list(text = texta, cex = 0.8, side = 1, line = 2)
		legend.args <- NULL
	}else{
		horizontal <- FALSE
		legend.mar <- 6.2
		mar <- c(4, 4, 2.5, 6)
		legend.width <- 0.9
		# legend.args <- list(text = texta, cex = 0.8, side = 4, line = 3)
		legend.args <- NULL
	}

	#################

	legendLabel <- breaks

	#################
	xlim <- EnvLOOCValidationplot$xlim.maps
	ylim <- EnvLOOCValidationplot$ylim.maps

	#################

	opar <- par(mar = mar)
	plot(1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
	axlabsFun <- if(Sys.info()["sysname"] == "Windows") LatLonAxisLabels else LatLonAxisLabels1
	axlabs <- axlabsFun(axTicks(1), axTicks(2))
	axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tcl = -0.2, cex.axis = 0.8)
	axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tcl = -0.2, las = 1, cex.axis = 0.8)
	title(main = mapstat, cex.main = 1, font.main= 2)

	if(tclvalue(EnvLOOCValidationplot$add.dem) == "1" & !is.null(EnvLOOCValidationplot$dem)){
		image(EnvLOOCValidationplot$dem$elv, col = gray.colors(256), add = TRUE)
		# image(EnvHOValidationplot$dem$hill, col = gray.colors(256), add = TRUE)
	}

	if(length(xna) > 0) points(xna, yna, pch = '*')
	image.plot(don, breaks = breaks, col = kolor, horizontal = horizontal, xaxt = 'n', yaxt = 'n', add = TRUE,
				legend.mar = legend.mar, legend.width = legend.width, legend.args = legend.args,
				axis.args = list(at = breaks, labels = legendLabel, cex.axis = 0.7, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)))

	abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)

	if(tclvalue(EnvLOOCValidationplot$add.shp) == "1") lines(ocrds[, 1], ocrds[, 2], lwd = 1.5)

	plt <- par("plt")
	usr <- par("usr")
	par(opar)
	return(list(par = c(plt, usr)))
}


###############################

LOOCValidation.plotGraph <- function(){
	if(EnvLOOCValidation$GeneralParameters$stat.data == 'all'){
		x <- c(EnvLOOCValidation$opDATA$stnStatData)
		y <- c(EnvLOOCValidation$opDATA$ncStatData)
		title <- "All Data"
	}
	if(EnvLOOCValidation$GeneralParameters$stat.data == 'avg'){
		x <- rowMeans(EnvLOOCValidation$opDATA$stnStatData, na.rm = TRUE)
		y <- rowMeans(EnvLOOCValidation$opDATA$ncStatData, na.rm = TRUE)
		title <- "Spatial Average"
	}
	if(EnvLOOCValidation$GeneralParameters$stat.data == 'stn'){
		istn <- which(EnvLOOCValidation$opDATA$id == tclvalue(EnvLOOCValidationplot$stnIDGraph))
		x <- EnvLOOCValidation$opDATA$stnStatData[, istn]
		y <- EnvLOOCValidation$opDATA$ncStatData[, istn]
		title <- tclvalue(EnvLOOCValidationplot$stnIDGraph)
	}

	##############
	AggrSeries <- EnvLOOCValidation$opDATA$AggrSeries
	if(AggrSeries$aggr.fun == "count"){
		units <- paste0("(Number of day ", AggrSeries$count.fun, " ", AggrSeries$count.thres, ")")
	}else{
		units <- if(EnvLOOCValidation$GeneralParameters$clim.var == "RR") "(mm)" else "(°C)"
	}

	##############
	plotType <- tclvalue(EnvLOOCValidationplot$type.graph)

	## choix xlim&ylim default
	xmin <- min(c(x, y), na.rm = TRUE)
	xmin <- ifelse(is.infinite(xmin), 0, xmin)
	xmax <- max(c(x, y), na.rm = TRUE)
	xmax <- ifelse(is.infinite(xmax), 0, xmax)

	if(plotType == "Scatter"){
		xlim <- c(xmin, xmax)
		ylim <- c(xmin, xmax)

		xlab <- paste('Station', units)
		ylab <- paste('Estimate', units)

		legendlab <- NA
	}
	if(plotType == "CDF"){
		xlim <- c(xmin, xmax)
		ylim <- c(0, 1)

		xlab <- if(EnvLOOCValidation$GeneralParameters$clim.var == "RR") "Rainfall" else "Temperature"
		xlab <- paste(xlab, units)
		ylab <- "Cumulative density"

		legendlab <- c('Station', 'Estimate')
	}
	if(plotType == "Lines"){
		xlim <- NA
		ylim <- c(xmin, xmax)

		xlab <- ""
		ylab <- if(EnvLOOCValidation$GeneralParameters$clim.var == "RR") "Rainfall" else "Temperature"
		ylab <- paste(ylab, units)

		legendlab <- c('Station', 'Estimate')
	}

	##############

	if(plotType == "Scatter"){
		plot(1, xlim = xlim, ylim = ylim, type = 'n', xlab = xlab, ylab = ylab, main = title)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = axTicks(1), col = "lightgray", lty = "dotted")
		points(x, y, pch = 19, col = 'grey10', cex = 0.7)
		abline(a = 0, b = 1, lwd = 2, col = 'red')
	}

	if(plotType == "CDF"){
		plot(1, xlim = xlim, ylim = ylim, type = 'n', xlab = xlab, ylab = ylab, main = title)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = axTicks(1), col = "lightgray", lty = "dotted")

		if(any(!is.na(x)) & any(!is.na(y))){
			xax <- seq(min(c(x, y), na.rm = TRUE), max(c(x, y), na.rm = TRUE), length.out = 1000)
			fx <- ecdf(x)
			fy <- ecdf(y)
			lines(xax, fx(xax), lwd = 2, col = 'blue', type = 'l')
			lines(xax, fy(xax), lwd = 2, col = 'red', type = 'l')
		}
		legend('bottomright', legendlab, col = c('blue', 'red'), lwd = 3, bg = 'lightgoldenrodyellow')
	}

	if(plotType == "Lines"){
		layout(matrix(1:2, ncol = 1), widths = 1, heights = c(0.9, 0.1), respect = FALSE)
		op <- par(mar = c(3, 4, 2, 2))
		plot(EnvLOOCValidation$opDATA$temps, x, ylim = ylim, type = 'n', xlab = xlab, ylab = ylab, main = title)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = axTicks(1), col = "lightgray", lty = "dotted")

		lines(EnvLOOCValidation$opDATA$temps, x, lwd = 2, col = 'blue', type = 'l')
		lines(EnvLOOCValidation$opDATA$temps, y, lwd = 2, col = 'red', type = 'l')
		par(op)

		op <- par(mar = c(0, 4, 0, 2))
		plot.new()
		legend('top', 'groups', legend = legendlab, col = c('blue', 'red'), lwd = 3, lty = 1, horiz = TRUE)
		par(op)
	}
}

###############################

LOOCValidation.DisplayStatMaps <- function(parent){
	varplot <- c("parPlotSize1", "parPlotSize2", "parPlotSize3", "parPlotSize4",
				 "usrCoords1", "usrCoords2", "usrCoords3", "usrCoords4")
	parPltCrd <- setNames(lapply(varplot, function(x) assign(x, tclVar(), env = parent.frame())), varplot)

	plotIt <- function(){
		op <- par(bg = "white")
		pltusr <- LOOCValidation.plotStatMaps()
		par(op)
		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvLOOCValidationplot$notebookTab.maps, 'Statistics-Maps', AllOpenTabType, AllOpenTabData)
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

		fdispIdStn <- function(x){
			y <- if(x <= 2) 0.0006944444 * x else 0.002777778
			return(y)
		}

		sdist <- (xyMouse$x-EnvLOOCValidation$opDATA$lon)^2 + (xyMouse$y-EnvLOOCValidation$opDATA$lat)^2
		inear <- which.min(sdist)
		rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
		frzcoord <- ifelse(xyMouse$inout | rayondisp, '', EnvLOOCValidation$opDATA$id[inear])

		tclvalue(xpcoord) <- frxcoord
		tclvalue(ypcoord) <- frycoord
		tclvalue(zpcoord) <- frzcoord
	})

	tkbind(img, "<Button-1>", function(W, x, y){
		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		fdispIdStn <- function(x){
			 y <- if(x <= 2) 0.0006944444 * x else 0.002777778
			return(y)
		}

		sdist <- (xyMouse$x-EnvLOOCValidation$opDATA$lon)^2 + (xyMouse$y-EnvLOOCValidation$opDATA$lat)^2
		inear <- which.min(sdist)
		rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
		if(!(xyMouse$inout | rayondisp)){
			tclvalue(EnvLOOCValidationplot$stnIDGraph) <- EnvLOOCValidation$opDATA$id[inear]
			plotGRAPH <- TRUE
		}else plotGRAPH <- FALSE

		if(plotGRAPH){
			imgContainer <- LOOCValidation.DisplayGraph(tknotes)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvLOOCValidationplot$notebookTab.graph, AllOpenTabType, AllOpenTabData)
			EnvLOOCValidationplot$notebookTab.graph <- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}

################################

LOOCValidation.DisplayGraph <- function(parent){
	plotIt <- function(){
		LOOCValidation.plotGraph()
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvLOOCValidationplot$notebookTab.graph, 'Validation-Plot', AllOpenTabType, AllOpenTabData)
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


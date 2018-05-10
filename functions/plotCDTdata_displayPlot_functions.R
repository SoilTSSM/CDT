
plotCDTdata.Maps <- function(){
	don <- EnvCDTdataPlot$stndata$map
	dataMapOp <- EnvCDTdataPlot$dataMapOp

	## titre
	if(!dataMapOp$title$user){
		titre <- paste("Observation:", don$t)
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
	shpf <- EnvCDTdataPlot$shp
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

	abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)
	lines(ocrds[, 1], ocrds[, 2], lwd = EnvCDTdataPlot$SHPOp$lwd, col = EnvCDTdataPlot$SHPOp$col)

	if(don$p == "Points"){
		kolor.p <- kolor[findInterval(don$z, breaks, rightmost.closed = TRUE)]
		points(don$x, don$y, col = kolor.p, cex = dataMapOp$pointSize, pch = 20)
	}
	if(don$p == "Pixels"){
		image(don, breaks = breaks, col = kolor, xaxt = 'n', yaxt = 'n', add = TRUE)
	}

	image.plot(zlim = zlim, breaks = breaks2, col = kolor, horizontal = horizontal,
				legend.only = TRUE, legend.mar = legend.mar, legend.width = legend.width,
				legend.args = legend.args, axis.args = list(at = breaks1, labels = legendLabel,
				cex.axis = 0.7, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)), legend.shrink = 0.8)

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

	par(opar)
}

#######################################

plotCDTdata.Graph <- function(){
	TSGraphOp <- EnvCDTdataPlot$TSGraphOp
	daty <- EnvCDTdataPlot$tsdates
	don <- EnvCDTdataPlot$stndata$series$ts

	titre <- paste("Station:", EnvCDTdataPlot$stndata$series$id)
	location <- paste0("Station: ", EnvCDTdataPlot$stndata$series$id)

	#######

	GRAPHTYPE <- str_trim(tclvalue(EnvCDTdataPlot$graph$typeTSp))
	if(GRAPHTYPE == "Line") optsgph <- TSGraphOp$line
	if(GRAPHTYPE == "Barplot") optsgph <- TSGraphOp$bar

	xlim <- range(daty, na.rm = TRUE)
	if(EnvCDTdataPlot$tstep != "others"){
		if(optsgph$xlim$is.min){
			xx <- strsplit(optsgph$xlim$min, "-")[[1]]
			x3 <- as.numeric(xx[3])
			if(EnvCDTdataPlot$tstep == "pentad"){
				if(is.na(x3) | x3 < 1 | x3 > 6){
					InsertMessagesTxt(main.txt.out, "xlim: pentad must be  between 1 and 6", format = TRUE)
					return(NULL)
				}
				x3 <- c(1, 6, 11, 16, 21, 26)[x3]
			}
			if(EnvCDTdataPlot$tstep == "dekadal"){
				if(is.na(x3) | x3 < 1 | x3 > 3){
					InsertMessagesTxt(main.txt.out, "xlim: dekad must be 1, 2 or 3", format = TRUE)
					return(NULL)
				}
				x3 <- c(1, 11, 21)[x3]
			}
			if(EnvCDTdataPlot$tstep == "monthly") x3 <- 1
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
			if(EnvCDTdataPlot$tstep == "pentad"){
				if(is.na(x3) | x3 < 1 | x3 > 6){
					InsertMessagesTxt(main.txt.out, "xlim: pentad must be  between 1 and 6", format = TRUE)
					return(NULL)
				}
				x3 <- c(1, 6, 11, 16, 21, 26)[x3]
			}
			if(EnvCDTdataPlot$tstep == "dekadal"){
				if(is.na(x3) | x3 < 1 | x3 > 3){
					InsertMessagesTxt(main.txt.out, "xlim: dekad must be 1, 2 or 3", format = TRUE)
					return(NULL)
				}
				x3 <- c(1, 11, 21)[x3]
			}
			if(EnvCDTdataPlot$tstep == "monthly") x3 <- 1
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
	}else{
		if(optsgph$xlim$is.min) xlim[1] <- as.numeric(optsgph$xlim$min)
		if(optsgph$xlim$is.max) xlim[2] <- as.numeric(optsgph$xlim$max)
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

	#######

	if(GRAPHTYPE == "Line"){
		graphs.plot.line(daty, don, xlim = xlim, ylim = ylim,
						xlab = xlab, ylab = ylab, ylab.sub = NULL,
						title = titre, title.position = titre.pos, axis.font = 1,
						plotl = optsgph$plot, legends = NULL,
						location = location)
	}

	if(GRAPHTYPE == "Barplot"){
		graphs.plot.bar(daty, don, xlim = xlim, ylim = ylim, origindate = NULL,
						xlab = xlab, ylab = ylab, ylab.sub = NULL,
						title = titre, title.position = titre.pos, axis.font = 1,
						barcol = optsgph$colors$col,
						location = location)
	}
}

####################################################################################

CDTdataStation.Display.Maps <- function(parent){
	plotIt <- function(){
		plotCDTdata.Maps()
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvCDTdataPlot$notebookTab.dataMap,
				paste('Map -', EnvCDTdataPlot$stndata$map$t), AllOpenTabType, AllOpenTabData)
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

CDTdataStation.Display.Graph <- function(parent){
	plotIt <- function(){
		plotCDTdata.Graph()
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvCDTdataPlot$notebookTab.dataGraph,
				paste('Station -', EnvCDTdataPlot$stndata$series$id), AllOpenTabType, AllOpenTabData)
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

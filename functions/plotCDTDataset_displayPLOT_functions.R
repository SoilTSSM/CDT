
CDTdataset.Plot.Map <- function(){
	ocrds <- EnvCDTdatasetPlot$shp$ocrds
	xlim <- range(ocrds[, 1], na.rm = TRUE)
	ylim <- range(ocrds[, 2], na.rm = TRUE)

	opar <- par(mar = c(4, 4, 2.5, 2.5))
	plot(1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
	axlabsFun <- if(Sys.info()["sysname"] == "Windows") LatLonAxisLabels else LatLonAxisLabels1
	axlabs <- axlabsFun(axTicks(1), axTicks(2))
	axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tcl = -0.2, cex.axis = 0.8)
	axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tcl = -0.2, las = 1, cex.axis = 0.8)

	abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)
	lines(ocrds[, 1], ocrds[, 2], lwd = 1, col = "black")

	plt <- par("plt")
	usr <- par("usr")
	par(opar)
	return(list(par = c(plt, usr)))
}

#######################################

CDTdataset.Plot.Graph <- function(){
	cdtdataset <- EnvCDTdatasetPlot$cdtdataset
	xlon <- cdtdataset$coords$mat$x
	xlat <- cdtdataset$coords$mat$y
	nx <- xlon[2]-xlon[1]
	ny <- xlat[2]-xlat[1]
	padx <- round(as.numeric(str_trim(tclvalue(EnvCDTdatasetPlot$graph$lonPAD)))/nx)
	pady <- round(as.numeric(str_trim(tclvalue(EnvCDTdatasetPlot$graph$latPAD)))/ny)

	ilon0 <- as.numeric(str_trim(tclvalue(EnvCDTdatasetPlot$graph$lonLOC)))
	ilat0 <- as.numeric(str_trim(tclvalue(EnvCDTdatasetPlot$graph$latLOC)))
	voisin <- expand.grid(x = ilon0 + nx*(-padx:padx), y =  ilat0 + ny*(-pady:pady))
	ilon <- voisin$x
	ilat <- voisin$y

	iclo <- findInterval(ilon, xlon)
	ilo <- iclo + (2 * ilon > xlon[iclo] + xlon[iclo+1])
	icla <- findInterval(ilat, xlat)
	ila <- icla + (2 * ilat > xlat[icla] + xlat[icla+1])

	ina <- !is.na(ilo) & !is.na(ila)
	if(all(!ina)){
		InsertMessagesTxt(main.txt.out, "Coordinates outside of data range", format = TRUE)
		return(NULL)
	}
	ixy <- ilo[ina] + length(xlon) * (ila[ina] - 1)

	don <- readCdtDatasetChunk.locations(ixy, cdtdataset$fileInfo, cdtdataset, do.par = FALSE)
	don <- rowMeans(don$data, na.rm = TRUE)

	daty <- cdtdataset$dateInfo$date[cdtdataset$dateInfo$index]
	nchar.daty <- nchar(daty[1])
	known.data.tstep <- switch(cdtdataset$TimeStep,
							"daily" = nchar.daty == 8,
							"pentad" = nchar.daty == 7,
							"dekadal" = nchar.daty == 7,
							"monthly" = nchar.daty == 6,
							FALSE)
	if(known.data.tstep){
		if(cdtdataset$TimeStep == "daily") daty <- as.Date(daty, "%Y%m%d")
		if(cdtdataset$TimeStep == "pentad"){
			pen <- c(1, 6, 11, 16, 21, 26)[as.numeric(substr(daty, 7, 7))]
			daty <- as.Date(paste0(substr(daty, 1, 6), pen), "%Y%m%d")
		}
		if(cdtdataset$TimeStep == "dekadal"){
			dek <- c(1, 11, 21)[as.numeric(substr(daty, 7, 7))]
			daty <- as.Date(paste0(substr(daty, 1, 6), dek), "%Y%m%d")
		}
		if(cdtdataset$TimeStep == "monthly") daty <- as.Date(paste0(daty, 1), "%Y%m%d")
	}else daty <- seq_along(daty)

	timestep <- if(known.data.tstep) cdtdataset$TimeStep else "others"
	titre <- cdtdataset$varInfo$longname
	location <- paste0("Longitude: ", round(ilon0, 5), ", Latitude: ", round(ilat0, 5))

	#######
	TSGraphOp <- EnvCDTdatasetPlot$TSGraphOp

	GRAPHTYPE <- str_trim(tclvalue(EnvCDTdatasetPlot$graph$typeTSp))
	if(GRAPHTYPE == "Line") optsgph <- TSGraphOp$line
	if(GRAPHTYPE == "Barplot") optsgph <- TSGraphOp$bar

	xlim <- range(daty, na.rm = TRUE)
	if(timestep != "others"){
		if(optsgph$xlim$is.min){
			xx <- strsplit(optsgph$xlim$min, "-")[[1]]
			x3 <- as.numeric(xx[3])
			if(timestep == "pentad"){
				if(is.na(x3) | x3 < 1 | x3 > 6){
					InsertMessagesTxt(main.txt.out, "xlim: pentad must be  between 1 and 6", format = TRUE)
					return(NULL)
				}
				x3 <- c(1, 6, 11, 16, 21, 26)[x3]
			}
			if(timestep == "dekadal"){
				if(is.na(x3) | x3 < 1 | x3 > 3){
					InsertMessagesTxt(main.txt.out, "xlim: dekad must be 1, 2 or 3", format = TRUE)
					return(NULL)
				}
				x3 <- c(1, 11, 21)[x3]
			}
			if(timestep == "monthly") x3 <- 1
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
			if(timestep == "pentad"){
				if(is.na(x3) | x3 < 1 | x3 > 6){
					InsertMessagesTxt(main.txt.out, "xlim: pentad must be  between 1 and 6", format = TRUE)
					return(NULL)
				}
				x3 <- c(1, 6, 11, 16, 21, 26)[x3]
			}
			if(timestep == "dekadal"){
				if(is.na(x3) | x3 < 1 | x3 > 3){
					InsertMessagesTxt(main.txt.out, "xlim: dekad must be 1, 2 or 3", format = TRUE)
					return(NULL)
				}
				x3 <- c(1, 11, 21)[x3]
			}
			if(timestep == "monthly") x3 <- 1
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

##############################################################################

CDTdataset.Display.Map <- function(parent){
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
		pltusr <- CDTdataset.Plot.Map()
		par(op)
		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvCDTdatasetPlot$notebookTab.MapSelect, 'Administrative-Map', AllOpenTabType, AllOpenTabData)
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	#########
	tkbind(img, "<Motion>", function(W, x, y){
		if(length(ls(EnvCDTdatasetPlot)) == 0) return(NULL)
		if(is.null(EnvCDTdatasetPlot$shp$data)) return(NULL)

		displayCursorPosition3Var(W, x, y, parPltCrd, getAdminLabel,
								shp = EnvCDTdatasetPlot$shp$data[[2]],
								idField = EnvCDTdatasetPlot$shp$cb.attrshp)
	})

	tkbind(img, "<Button-1>", function(W, x, y){
		if(length(ls(EnvCDTdatasetPlot)) == 0) return(NULL)
		if(is.null(EnvCDTdatasetPlot$shp$data)) return(NULL)

		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		####
		if(!xyMouse$inout){
			tclvalue(EnvCDTdatasetPlot$graph$lonLOC) <- round(xyMouse$x, 6)
			tclvalue(EnvCDTdatasetPlot$graph$latLOC) <- round(xyMouse$y, 6)
			plotTS <- TRUE
		}else plotTS <- FALSE

		if(plotTS){
			if(!is.null(EnvCDTdatasetPlot$cdtdataset)){
				imgContainer <- CDTdataset.Display.Graph(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvCDTdatasetPlot$notebookTab.dataGraph, AllOpenTabType, AllOpenTabData)
				EnvCDTdatasetPlot$notebookTab.dataGraph <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		}
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}

#######################################

CDTdataset.Display.Graph <- function(parent){
	plotIt <- function(){
		CDTdataset.Plot.Graph()
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvCDTdatasetPlot$notebookTab.dataGraph,
								"CDT Dataset - TS", AllOpenTabType, AllOpenTabData)
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



SummaryData.Initial.Map <- function(){
	don <- EnvSummaryDataplot$output$map
	breaks <- pretty(don$z, n = 10, min.n = 5)
	breaks <- if(length(breaks) > 0) breaks else c(0, 1) 
	kolor <- tim.colors(length(breaks)-1)

	### shape files
	shpf <- EnvSummaryDataplot$shp
	ocrds <- if(tclvalue(shpf$add.shp) == "1" & !is.null(shpf$ocrds)) shpf$ocrds else matrix(NA, 1, 2)

	#################

	if(all(is.na(ocrds[, 1])) | all(is.na(ocrds[, 2]))){
		xlim <- range(don$x, na.rm = TRUE)
		ylim <- range(don$y, na.rm = TRUE)
	}else{
		xlim <- range(range(don$x, na.rm = TRUE), range(ocrds[, 1], na.rm = TRUE))
		ylim <- range(range(don$y, na.rm = TRUE), range(ocrds[, 2], na.rm = TRUE))
	}

	opar <- par(mar = c(4, 4, 2.5, 2.5))
	plot(1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
	axlabsFun <- if(Sys.info()["sysname"] == "Windows") LatLonAxisLabels else LatLonAxisLabels1
	axlabs <- axlabsFun(axTicks(1), axTicks(2))
	axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tcl = -0.2, cex.axis = 0.8)
	axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tcl = -0.2, las = 1, cex.axis = 0.8)

	image(don, breaks = breaks, col = kolor, xaxt = 'n', yaxt = 'n', add = TRUE)

	abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)

	lines(ocrds[, 1], ocrds[, 2], lwd = EnvSummaryDataplot$SHPOp$lwd, col = EnvSummaryDataplot$SHPOp$col)

	plt <- par("plt")
	usr <- par("usr")
	par(opar)
	return(list(par = c(plt, usr)))
}

SummaryData.Plot.Graph <- function(){
	if(EnvSummaryDataplot$output$params$data.type == "cdtstation"){
		ixy <- which(EnvSummaryDataplot$output$data$id == str_trim(tclvalue(EnvSummaryDataplot$graph$stnIDTSp)))
		if(length(ixy) == 0){
			InsertMessagesTxt(main.txt.out, "Station not found", format = TRUE)
			return(NULL)
		}
		stn <- EnvSummaryDataplot$output$data$id[ixy]
		pts <- c(EnvSummaryDataplot$output$data$lon[ixy], EnvSummaryDataplot$output$data$lat[ixy])
		don <- EnvSummaryDataplot$output$data$data[, ixy]
	}else{
		cdtdataset <- EnvSummaryDataplot$output$data
		xlon <- cdtdataset$coords$mat$x
		xlat <- cdtdataset$coords$mat$y
		ilon <- as.numeric(str_trim(tclvalue(EnvSummaryDataplot$graph$lonLOC)))
		ilat <- as.numeric(str_trim(tclvalue(EnvSummaryDataplot$graph$latLOC)))

		iclo <- findInterval(ilon, xlon)
		ilo <- iclo + (2 * ilon > xlon[iclo] + xlon[iclo+1])
		icla <- findInterval(ilat, xlat)
		ila <- icla + (2 * ilat > xlat[icla] + xlat[icla+1])

		if(is.na(ilo) | is.na(ila)){
			InsertMessagesTxt(main.txt.out, "Coordinates outside of data range", format = TRUE)
			return(NULL)
		}
		ixy <- ilo + length(xlon) * (ila-1)

		don <- readCdtDatasetChunk.locations(ixy, EnvSummaryDataplot$output$index.file, cdtdataset, do.par = FALSE)
		pts <- don$coords
		stn <- "Pixel"
		don <- as.numeric(don$data[cdtdataset$dateInfo$index, 1])
	}

	index <- EnvSummaryDataplot$output$index
	mois <- format(ISOdate(2014, 1:12, 1), "%b")

	if(str_trim(tclvalue(EnvSummaryDataplot$plotType)) == "Boxplot"){
		mdon <- lapply(seq_along(index), function(j){
			data.frame(mois[as.numeric(names(index[j]))], don[index[[j]]])
		})
		ylim <- range(pretty(don))
		mdon <- do.call(rbind, mdon)
		names(mdon) <- c("group", "value")
		don <- data.frame(group = "ALL", value = don)
		don <- rbind(mdon, don)

		xlab <- ''
		ylab <- ''
		title <- ''
		plot(don$value, xlim = c(1, 13), ylim = ylim, type = 'n', xaxt = 'n', las = 2, xlab = xlab, ylab = ylab, main = title)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted", lwd = 0.8)

		boxplot(value ~ group, data = don, add = TRUE, notch = FALSE,
				col = "lightblue", medcol = "red", whiskcol = "blue",
				staplecol = "blue", boxcol = "blue", outcol = 'blue', outbg = "lightblue",
				outcex = 0.7, outpch = 21, yaxt = 'n', range = round(ylim[2]*0.25))
	}else{
		if(str_trim(tclvalue(EnvSummaryDataplot$plotMois)) != "ALL")
			don <- don[index[[which(mois == str_trim(tclvalue(EnvSummaryDataplot$plotMois)))]]]
		xlab <- ''
		ylab <- 'Density'
		title <- ''
		hst <- hist(don, freq = FALSE, plot = FALSE)
		dst <- density(don, bw = 1.0, na.rm = TRUE)
		xhst <- range(hst$breaks)
		yhst <- range(hst$density)
		xdst <- range(dst$x)
		ydst <- range(dst$y)
		xlim <- c(min(xhst[1], xdst[1]), max(xhst[2], xdst[2]))
		ylim <- c(min(yhst[1], ydst[1]), max(yhst[2], ydst[2]))
		plot(1, xlim = xlim, ylim = ylim, type = 'n', xaxt = 'n', yaxt = 'n', xlab = xlab, ylab = ylab, main = title)
		abline(v = axTicks(1), col = "lightgray", lty = "dotted", lwd = 0.8)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted", lwd = 0.8)
		xTck <- axTicks(1)
		xminor <- hst$breaks
		xminor <- xminor[!xminor%in%xTck]
		axis(1, at = axTicks(1))
		axis(1, at = xminor, labels = NA, tcl = par("tcl")*0.5)
		axis(2, at = axTicks(2), las = 2)
		hist(don, freq = FALSE, add = TRUE, xlab = '', ylab = '', main = '', axes = FALSE, col = "lightblue", border = "blue")
		lines(dst, col = "red", lwd = 1.5)
	}
}

##############################################################################

SummaryData.Display.Map <- function(parent){
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
		pltusr <- SummaryData.Initial.Map()
		par(op)
		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvSummaryDataplot$notebookTab.Map, 'Summary-Map', AllOpenTabType, AllOpenTabData)
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	#########
	tkbind(img, "<Motion>", function(W, x, y){
		if(length(ls(EnvSummaryDataplot)) == 0) return(NULL)
		if(is.null(EnvSummaryDataplot$output)) return(NULL)

		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		xydisp <- LatLonLabels(xyMouse$x, xyMouse$y)
		frxcoord <- ifelse(xyMouse$inout, '', xydisp$xdisp)
		frycoord <- ifelse(xyMouse$inout, '', xydisp$ydisp)

		if(EnvSummaryDataplot$output$params$data.type == "cdtstation"){
			fdispIdStn <- function(x){
				y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvSummaryDataplot$output$data$lon)^2 + (xyMouse$y-EnvSummaryDataplot$output$data$lat)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			frzcoord <- ifelse(xyMouse$inout | rayondisp, '', EnvSummaryDataplot$output$data$id[inear])
		}else{
			frzcoord <- ""
		}

		tclvalue(xpcoord) <- frxcoord
		tclvalue(ypcoord) <- frycoord
		tclvalue(zpcoord) <- frzcoord
	})

	tkbind(img, "<Button-1>", function(W, x, y){
		if(length(ls(EnvSummaryDataplot)) == 0) return(NULL)
		if(is.null(EnvSummaryDataplot$output)) return(NULL)

		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		if(EnvSummaryDataplot$output$params$data.type == "cdtstation"){
			fdispIdStn <- function(x){
				 y <- if(x <= 2) 0.0006944444 * x else 0.002777778
				return(y)
			}

			sdist <- (xyMouse$x-EnvSummaryDataplot$output$data$lon)^2 + (xyMouse$y-EnvSummaryDataplot$output$data$lat)^2
			inear <- which.min(sdist)
			rayondisp <- sdist[inear] > fdispIdStn(as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1)))
			if(!(xyMouse$inout | rayondisp)){
				tclvalue(EnvSummaryDataplot$graph$stnIDTSp) <- EnvSummaryDataplot$output$data$id[inear]
				plotTS <- TRUE
			}else plotTS <- FALSE
		}else{
			if(!xyMouse$inout){
				tclvalue(EnvSummaryDataplot$graph$lonLOC) <- round(xyMouse$x, 6)
				tclvalue(EnvSummaryDataplot$graph$latLOC) <- round(xyMouse$y, 6)
				plotTS <- TRUE
			}else plotTS <- FALSE
		}

		if(plotTS){
			imgContainer <- SummaryData.Display.Graph(tknotes)
			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvSummaryDataplot$notebookTab.Graph, AllOpenTabType, AllOpenTabData)
			EnvSummaryDataplot$notebookTab.Graph <- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}

###################

SummaryData.Display.Graph <- function(parent){
	plotIt <- function(){
		SummaryData.Plot.Graph()
	}

	#########
	onglet <- imageNotebookTab_open(parent, EnvSummaryDataplot$notebookTab.Graph, 'Summary-Plot', AllOpenTabType, AllOpenTabData)
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

##############################################################################

SummaryData.Get.Table <- function(){
	if(EnvSummaryDataplot$output$params$data.type == "cdtstation"){
		ixy <- which(EnvSummaryDataplot$output$data$id == str_trim(tclvalue(EnvSummaryDataplot$graph$stnIDTSp)))
		if(length(ixy) == 0){
			InsertMessagesTxt(main.txt.out, "Station not found", format = TRUE)
			return(NULL)
		}
		stn <- EnvSummaryDataplot$output$data$id[ixy]
		pts <- c(EnvSummaryDataplot$output$data$lon[ixy], EnvSummaryDataplot$output$data$lat[ixy])
		don <- EnvSummaryDataplot$output$data$data[, ixy]
	}else{
		cdtdataset <- EnvSummaryDataplot$output$data
		xlon <- cdtdataset$coords$mat$x
		xlat <- cdtdataset$coords$mat$y
		ilon <- as.numeric(str_trim(tclvalue(EnvSummaryDataplot$graph$lonLOC)))
		ilat <- as.numeric(str_trim(tclvalue(EnvSummaryDataplot$graph$latLOC)))

		iclo <- findInterval(ilon, xlon)
		ilo <- iclo + (2 * ilon > xlon[iclo] + xlon[iclo+1])
		icla <- findInterval(ilat, xlat)
		ila <- icla + (2 * ilat > xlat[icla] + xlat[icla+1])

		if(is.na(ilo) | is.na(ila)){
			InsertMessagesTxt(main.txt.out, "Coordinates outside of data range", format = TRUE)
			return(NULL)
		}
		ixy <- ilo + length(xlon) * (ila-1)

		don <- readCdtDatasetChunk.locations(ixy, EnvSummaryDataplot$output$index.file, cdtdataset, do.par = FALSE)
		pts <- don$coords
		stn <- "Pixel"
		don <- as.numeric(don$data[cdtdataset$dateInfo$index, 1])
	}

	index <- EnvSummaryDataplot$output$index

	summ <- lapply(index, function(ix){
		sm <- as.numeric(summary(don[ix]))
		sm[is.nan(sm) | is.infinite(sm)] <- NA
		sm <- if(length(sm) == 7) sm else if(length(sm) == 6) c(sm, NA) else c(rep(NA, 5), sm)
		sm
	})
	mdon <- do.call(cbind, summ)
	adon <- as.numeric(summary(don))
	adon <- if(length(adon) == 7) adon else if(length(adon) == 6) c(adon, NA) else c(rep(NA, 5), adon)
	mdon <- cbind(mdon, adon)

	std <- sapply(index, function(ix) sd(don[ix], na.rm = TRUE))
	std <- c(std, sd(don, na.rm = TRUE))
	std <- round(std, 4)

	mdon <- rbind(mdon[1:6, ], std, mdon[7, ])
	mdon <- rbind(mdon, c(stn, "Longitude", pts[1], "Latitude", pts[2], rep(NA, 8)))
	stats <- c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Maximum", "Standard Deviation", "Missing", "Station")
	mdon <- data.frame(stats, mdon)
	names(mdon) <- c("Statistics", format(ISOdate(2014, 1:12, 1), "%b"), "ALL")
	return(mdon)
}

################

SummaryData.Display.Table <- function(parent, titleTab, notebookTab, tabType, tabData){
	ntab <- length(tabType)

	summary.df <- SummaryData.Get.Table()
	summary.df[is.na(summary.df)] <- ""

	if(is.null(notebookTab)){
		tableDisp <- Display_data.frame_Table(parent, summary.df, titleTab)
		tabType[[ntab+1]] <- 'arrSummary'
		tabData[[ntab+1]] <- tableDisp
		notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
		tkselect(parent, ntab)
		# popupAddRemoveRow0(parent, tabData, ntab+1)
	}else{
		if(ntab > 0){
			AllNoteTab <- sapply(1:ntab, function(j){
				if(!is.null(attributes(tabData[[j]][[1]][[1]]))) tabData[[j]][[1]][[1]]$ID
				else tabData[[j]][[1]][[1]]
			})
			idTabs <- which(AllNoteTab == notebookTab)
			if(length(idTabs) > 0){
				.Tcl(paste('destroy', tclvalue(tkwinfo("children", tabData[[idTabs]][[1]][[2]]))))

				dtab <- tclArrayVar(summary.df)
				tabData[[idTabs]][[2]] <- displayTable(tabData[[idTabs]][[1]][[2]], tclArray = dtab, colwidth = 10)
				tcl(parent, 'tab', tabData[[idTabs]][[1]][[1]], '-text', titleTab)
				notebookTab <- tabData[[idTabs]][[1]][[1]]$ID
				tkselect(parent, idTabs-1)
				# popupAddRemoveRow0(parent, tabData, idTabs)
			}else{
				tableDisp <- Display_data.frame_Table(parent, summary.df, titleTab)
				tabType[[ntab+1]] <- 'arrSummary'
				tabData[[ntab+1]] <- tableDisp
				notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
				tkselect(parent, ntab)
				# popupAddRemoveRow0(parent, tabData, ntab+1)
			}
		}else{
			tableDisp <- Display_data.frame_Table(parent, summary.df, titleTab)
			tabType[[ntab+1]] <- 'arrSummary'
			tabData[[ntab+1]] <- tableDisp
			notebookTab <- tabData[[ntab+1]][[1]][[1]]$ID
			tkselect(parent, ntab)
			# popupAddRemoveRow0(parent, tabData, ntab+1)
		}
	}
	retTab <- list(notebookTab = notebookTab, AllOpenTabType = tabType, AllOpenTabData = tabData)
	return(retTab)
}

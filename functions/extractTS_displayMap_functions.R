plotMap4Extraction <- function(ocrds, ZoomXYval, selectedPolygon){

	xmin <- ZoomXYval[1]
	xmax <- ZoomXYval[2]
	ymin <- ZoomXYval[3]
	ymax <- ZoomXYval[4]

	if(is.na(xmin) | is.null(xmin) | is.infinite(xmin)){
		InsertMessagesTxt(main.txt.out, 'Longitude min not valid', format = TRUE)
		return(NULL)
	}
	if(is.na(xmax) | is.null(xmax) | is.infinite(xmax)){
		InsertMessagesTxt(main.txt.out, 'Longitude max not valid', format = TRUE)
		return(NULL)
	}
	if(is.na(ymin) | is.null(ymin) | is.infinite(ymin)){
		InsertMessagesTxt(main.txt.out, 'Latitude min not valid', format = TRUE)
		return(NULL)
	}
	if(is.na(ymax) | is.null(ymax) | is.infinite(ymax)){
		InsertMessagesTxt(main.txt.out, 'Latitude max not valid', format = TRUE)
		return(NULL)
	}

	#######
	opar <- par(mar = c(4, 4, 2, 2))
	plot(1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
	lines(ocrds)
	if(!is.null(selectedPolygon)) lines(selectedPolygon, col = 'red', lwd = 2)

	abline(h = axTicks(2), v = axTicks(1) , col = "lightgray", lty = 3)
	if(Sys.info()["sysname"] == "Windows") axlabs <- LatLonAxisLabels(axTicks(1), axTicks(2))
	else axlabs <- LatLonAxisLabels1(axTicks(1), axTicks(2))
	axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tck = -0.01, cex.axis = 0.8)
	axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tck = -0.01, las = 1, cex.axis = 0.8)
	plt <- par("plt")
	usr <- par("usr")
	par(opar)

	return(list(par = c(plt, usr)))
}


#################################################################

displayMap4Extraction <- function(parent, shpf, ZoomXYval, notebookTab){
	ocrds <- getBoundaries(shpf)
	selectedPolygon <- NULL

	varplot <- c("parPlotSize1", "parPlotSize2", "parPlotSize3", "parPlotSize4",
				 "usrCoords1", "usrCoords2", "usrCoords3", "usrCoords4")
	parPltCrd <- setNames(lapply(varplot, function(x) assign(x, tclVar(), env = parent.frame())), varplot)

	plotIt <- function(){
		op <- par(bg = 'white')
		pltusr <- plotMap4Extraction(ocrds, ZoomXYval, selectedPolygon)
		par(op)

		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
	}

	###################################################################

	onglet <- imageNotebookTab_open(parent, notebookTab, tabTitle = 'Extraction Map', AllOpenTabType, AllOpenTabData)
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))

	canvas <- tkcanvas(onglet[[2]])
	tkgrid(canvas)

	img <- tkrplot1(canvas, fun = plotIt, hscale = hscale, vscale = vscale)
	img_w <- as.double(tcl('image', 'width', img$image))
	img_h <- as.double(tcl('image', 'height', img$image))
	tkconfigure(canvas, width = img_w, height = img_h)
	tkcreate(canvas, "image", 0, 0, anchor = 'nw', image = img$image)

	tcl('raise', canvas)
	tcl('update')

	if(is.null(lcmd.frame_extrdata)) return(NULL)

	tkbind(canvas, "<Enter>", function(){
		if(tclvalue(EnvZoomPars$pressButP) == "1") tkconfigure(canvas, cursor = 'sizing')
		else if(tclvalue(EnvZoomPars$pressButM) == "1") tkconfigure(canvas, cursor = 'sizing')
		else if(tclvalue(EnvZoomPars$pressButRect) == "1") tkconfigure(canvas, cursor = 'sizing')
		else if(tclvalue(EnvZoomPars$pressButDrag) == "1") tkconfigure(canvas, cursor = 'hand1')
		else if(tclvalue(EnvExtractData$pressGetCoords) == "1") tkconfigure(canvas, cursor = 'draped_box')
		else tkconfigure(canvas, cursor = 'crosshair')
	})

	tkbind(canvas, "<Leave>", function() tkconfigure(canvas, cursor = ''))

	#####
	##draw rectangle initial value
	lastX <- 0
	lastY <- 0

	##zoom factor
	factZoom <- 0.2

	##zoom rectangle
	rectZoomInit <- ZoomXYval

	##Pan Image
	panZoomInit <- c(0, 0, 0, 0, 0, 0)
	factPan <- 0.2

	##########
	## first click on map
	tkbind(canvas, "<Button-1>", function(W, x, y){
		ret <- getXYCoords(W, x, y, parPltCrd)
		tkdelete(W, 'rect')

		##get coordinates or polygon id
		if(tclvalue(EnvExtractData$pressGetCoords) == "1" & !ret$oin){
			if(tclvalue(EnvExtractData$type.extract) == "Point"){
				tclvalue(EnvExtractData$minlonRect) <- round(ret$xc, 4)
				tclvalue(EnvExtractData$maxlonRect) <- ''
				tclvalue(EnvExtractData$minlatRect) <- round(ret$yc, 4)
				tclvalue(EnvExtractData$maxlatRect) <- ''
				selectedPolygon <<- NULL
				stateADD <- 'disabled'
				colorADD <- 'lightblue'
			}

			##
			if(tclvalue(EnvExtractData$type.extract) == "Rectangle"){
				pPressRect(W, x, y, width = 1, outline = "red")
				tclvalue(EnvExtractData$minlonRect) <- round(ret$xc, 4)
				tclvalue(EnvExtractData$minlatRect) <- round(ret$yc, 4)
				selectedPolygon <<- NULL
				stateADD <- 'disabled'
				colorADD <- 'lightblue'
			}

			##
			if(tclvalue(EnvExtractData$type.extract) == "Polygon"){
				xypts <- data.frame(x = ret$xc, y = ret$yc)
				coordinates(xypts) <- ~x+y
				admin_name <- over(xypts, shpf)
				admin_name <- c(t(admin_name[1, ]))

				ids <- as.numeric(tclvalue(tcl(EnvExtractData$cb.shpAttr, 'current')))+1
				admin_name <- admin_name[ids]
				if(!is.na(admin_name)){
					tclvalue(EnvExtractData$namePoly) <- as.character(admin_name)
					selectedPolygon <<- getBoundaries(shpf[shpf@data[, ids] == tclvalue(EnvExtractData$namePoly), ])
				}else{
					selectedPolygon <<- NULL
				}
				stateADD <- 'disabled'
				colorADD <- 'lightblue'
			}

			##
			if(tclvalue(EnvExtractData$type.extract) == 'Multiple Points'){
				tclvalue(EnvExtractData$minlonRect) <- round(ret$xc, 4)
				tclvalue(EnvExtractData$maxlonRect) <- ''
				tclvalue(EnvExtractData$minlatRect) <- round(ret$yc, 4)
				tclvalue(EnvExtractData$maxlatRect) <- ''
				selectedPolygon <<- NULL
				stateADD <- 'normal'
				colorADD <- 'red'
			}

			##
			if(tclvalue(EnvExtractData$type.extract) == 'Multiple Polygons'){
				xypts <- data.frame(x = ret$xc, y = ret$yc)
				coordinates(xypts) <- ~x+y
				admin_name <- over(xypts, shpf)
				admin_name <- c(t(admin_name[1, ]))

				ids <- as.numeric(tclvalue(tcl(EnvExtractData$cb.shpAttr, 'current')))+1
				admin_name <- admin_name[ids]
				if(!is.na(admin_name)){
					tclvalue(EnvExtractData$namePoly) <- as.character(admin_name)
					selectedPolygon <<- getBoundaries(shpf[shpf@data[, ids] == tclvalue(EnvExtractData$namePoly), ])
				}else{
					selectedPolygon <<- NULL
				}
				stateADD <- 'normal'
				colorADD <- 'red'
			}

			tkconfigure(EnvExtractData$bt.ADDObj, relief = 'raised', bg = colorADD, state = stateADD)

			refreshPlot1(W, img, hscale = as.numeric(tclvalue(tkget(spinH))), vscale = as.numeric(tclvalue(tkget(spinV))))
		}

		#Zoom plus
		if(tclvalue(EnvZoomPars$pressButP) == "1" & !ret$oin){
			rgX <- as.numeric(tclvalue(parPltCrd$usrCoords2))-as.numeric(tclvalue(parPltCrd$usrCoords1))
			rgY <- as.numeric(tclvalue(parPltCrd$usrCoords4))-as.numeric(tclvalue(parPltCrd$usrCoords3))
			shiftX <- rgX*(1-factZoom)/2
			shiftY <- rgY*(1-factZoom)/2
			xmin1 <- ret$xc-shiftX
			xmax1 <- ret$xc+shiftX
			ymin1 <- ret$yc-shiftY
			ymax1 <- ret$yc+shiftY

			ZoomXYval <<- c(xmin1, xmax1, ymin1, ymax1)

			tclvalue(EnvZoomPars$xx1) <- round(xmin1, 4)
			tclvalue(EnvZoomPars$xx2) <- round(xmax1, 4)
			tclvalue(EnvZoomPars$yy1) <- round(ymin1, 4)
			tclvalue(EnvZoomPars$yy2) <- round(ymax1, 4)

			refreshPlot1(W, img, hscale = as.numeric(tclvalue(tkget(spinH))), vscale = as.numeric(tclvalue(tkget(spinV))))
		}

		#Zoom Moins
		if(tclvalue(EnvZoomPars$pressButM) == "1"  & !ret$oin){
			rgX <- as.numeric(tclvalue(parPltCrd$usrCoords2))-as.numeric(tclvalue(parPltCrd$usrCoords1))
			rgY <- as.numeric(tclvalue(parPltCrd$usrCoords4))-as.numeric(tclvalue(parPltCrd$usrCoords3))
			shiftX <- rgX*(1+factZoom)/2
			shiftY <- rgY*(1+factZoom)/2
			xmin1 <- ret$xc-shiftX
			xmax1 <- ret$xc+shiftX
			ymin1 <- ret$yc-shiftY
			ymax1 <- ret$yc+shiftY

			if(xmin1< -180 | xmax1 > 180 | ymin1< -90 | ymax1 > 90){
				tclvalue(EnvZoomPars$pressButP) <- 0
				tclvalue(EnvZoomPars$pressButM) <- 0
				tclvalue(EnvZoomPars$pressButRect) <- 0
				tclvalue(EnvZoomPars$pressButDrag) <- 0
				
				tkconfigure(EnvZoomPars$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
				tkconfigure(EnvZoomPars$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
				tkconfigure(EnvZoomPars$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
				tkconfigure(EnvZoomPars$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

				tkconfigure(W, cursor = 'crosshair')
			}else{
				ZoomXYval <<- c(xmin1, xmax1, ymin1, ymax1)

				tclvalue(EnvZoomPars$xx1) <- round(xmin1, 4)
				tclvalue(EnvZoomPars$xx2) <- round(xmax1, 4)
				tclvalue(EnvZoomPars$yy1) <- round(ymin1, 4)
				tclvalue(EnvZoomPars$yy2) <- round(ymax1, 4)

				refreshPlot1(W, img, hscale = as.numeric(tclvalue(tkget(spinH))), vscale = as.numeric(tclvalue(tkget(spinV))))
			}
		}

		##Zoom rectangle
		if(tclvalue(EnvZoomPars$pressButRect) == "1"  & !ret$oin){
			pPressRect(W, x, y, width = 1, outline = "red")
			rectZoomInit[1] <<- ret$xc
			rectZoomInit[3] <<- ret$yc
		}

		##Pan image
		if(tclvalue(EnvZoomPars$pressButDrag) == "1"  & !ret$oin){
			panZoomInit[1] <<- ret$xc
			panZoomInit[2] <<- ret$yc

			panZoomInit[3] <<- as.numeric(tclvalue(EnvZoomPars$xx1))
			panZoomInit[4] <<- as.numeric(tclvalue(EnvZoomPars$xx2))
			panZoomInit[5] <<- as.numeric(tclvalue(EnvZoomPars$yy1))
			panZoomInit[6] <<- as.numeric(tclvalue(EnvZoomPars$yy2))

			tkconfigure(canvas, cursor = 'hand2')
		}

	})

	##########
	## cursor movement
	tkbind(canvas, "<Motion>", function(W, x, y){
		displayCursorPosition3Var(W, x, y, parPltCrd, xpcoord, ypcoord, zpcoord,
								getAdminLabel, shp = shpf, idField = EnvExtractData$cb.shpAttr)
	})

	#########
	## cursor movement with button-1 pressed
	tkbind(canvas, "<B1-Motion>", function(W, x, y){
		ret <- getXYCoords(W, x, y, parPltCrd)

		##get coordinates rect
		if(tclvalue(EnvExtractData$pressGetCoords) == "1"  & tclvalue(EnvExtractData$type.extract) == "Rectangle"){
			pMoveRect(W, x, y)
			tclvalue(EnvExtractData$maxlonRect) <- round(ret$xc, 4)
			tclvalue(EnvExtractData$maxlatRect) <- round(ret$yc, 4)
		}

		##Zoom rectangle
		if(tclvalue(EnvZoomPars$pressButRect) == "1"){
			pMoveRect(W, x, y)
		}

		##Pan image
		if(tclvalue(EnvZoomPars$pressButDrag) == "1"){
			transX <- ret$xc-panZoomInit[1]
			transY <- ret$yc-panZoomInit[2]

			tclvalue(EnvZoomPars$xx1) <- round(panZoomInit[3]+factPan*transX, 4)
			tclvalue(EnvZoomPars$xx2) <- round(panZoomInit[4]+factPan*transX, 4)
			tclvalue(EnvZoomPars$yy1) <- round(panZoomInit[5]+factPan*transY, 4)
			tclvalue(EnvZoomPars$yy2) <- round(panZoomInit[6]+factPan*transY, 4)

			ZoomXYval <<- as.numeric(c(tclvalue(EnvZoomPars$xx1), tclvalue(EnvZoomPars$xx2),
										tclvalue(EnvZoomPars$yy1), tclvalue(EnvZoomPars$yy2)))
			refreshPlot1(W, img, hscale = as.numeric(tclvalue(tkget(spinH))), vscale = as.numeric(tclvalue(tkget(spinV))))
		}
	})

	#########
	## release button1
	tkbind(canvas, "<ButtonRelease>", function(W, x, y){
		ret <- getXYCoords(W, x, y, parPltCrd)

		##get coordinates rect
		if(tclvalue(EnvExtractData$pressGetCoords) == "1"){
			if(tclvalue(EnvExtractData$type.extract) == "Rectangle"){
				xpr <- c(as.numeric(tclvalue(EnvExtractData$minlonRect)), round(ret$xc, 4),
						as.numeric(tclvalue(EnvExtractData$minlatRect)), round(ret$yc, 4))
				if(xpr[1] > xpr[2]) xpr <- xpr[c(2, 1, 3, 4)]
				if(xpr[3] > xpr[4]) xpr <- xpr[c(1, 2, 4, 3)]

				tclvalue(EnvExtractData$minlonRect) <- xpr[1]
				tclvalue(EnvExtractData$maxlonRect) <- xpr[2]
				tclvalue(EnvExtractData$minlatRect) <- xpr[3]
				tclvalue(EnvExtractData$maxlatRect) <- xpr[4]
			}

			tclvalue(EnvExtractData$pressGetCoords) <- 0
			tkconfigure(EnvExtractData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(W, cursor = 'crosshair')
		}

		##Zoom rectangle
		if(tclvalue(EnvZoomPars$pressButRect) == "1"){
			rectZoomInit[2] <<- ret$xc
			rectZoomInit[4] <<- ret$yc
			if(rectZoomInit[1] > rectZoomInit[2]) rectZoomInit <- rectZoomInit[c(2, 1, 3, 4)]
			if(rectZoomInit[3] > rectZoomInit[4]) rectZoomInit <- rectZoomInit[c(1, 2, 4, 3)]
			ZoomXYval <<- rectZoomInit

			tclvalue(EnvZoomPars$xx1) <- round(rectZoomInit[1], 4)
			tclvalue(EnvZoomPars$xx2) <- round(rectZoomInit[2], 4)
			tclvalue(EnvZoomPars$yy1) <- round(rectZoomInit[3], 4)
			tclvalue(EnvZoomPars$yy2) <- round(rectZoomInit[4], 4)

			refreshPlot1(W, img, hscale = as.numeric(tclvalue(tkget(spinH))), vscale = as.numeric(tclvalue(tkget(spinV))))
			tkdelete(W, 'rect')
		}

		##Pan image
		if(tclvalue(EnvZoomPars$pressButDrag) == "1"){
			tkconfigure(canvas, cursor = 'hand1')
		}
	})

	###############################################
	## deactivate zoom (right button)
	tkbind(canvas, "<Button-3>", function(W){
		tclvalue(EnvZoomPars$pressButP) <- 0
		tclvalue(EnvZoomPars$pressButM) <- 0
		tclvalue(EnvZoomPars$pressButRect) <- 0
		tclvalue(EnvZoomPars$pressButDrag) <- 0

		tkconfigure(EnvZoomPars$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(EnvZoomPars$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

		tkconfigure(EnvExtractData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
		stateADD <- if(tclvalue(EnvExtractData$type.extract)%in%c('Multiple Points', 'Multiple Polygons')) "normal" else "disabled"
		tkconfigure(EnvExtractData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)

		tkconfigure(canvas, cursor = 'crosshair')

		tkdelete(W, 'rect')

		selectedPolygon <<- NULL
		refreshPlot1(W, img, hscale = as.numeric(tclvalue(tkget(spinH))), vscale = as.numeric(tclvalue(tkget(spinV))))
	})

	###
	return(list(onglet, list(canvas, img)))
}



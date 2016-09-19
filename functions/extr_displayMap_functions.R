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
	opar <- par(mar = c(4,4,2,2))
	plot(1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), xlab="", ylab="", type = "n", xaxt = 'n', yaxt = 'n')
	lines(ocrds)
	if(!is.null(selectedPolygon)) lines(selectedPolygon, col = 'red')

	abline(h = axTicks(2), v = axTicks(1) , col = "lightgray", lty = 3)
	if(Sys.info()["sysname"] == "Windows") axlabs <- LatLonAxisLabels(axTicks(1), axTicks(2))
	else axlabs <- LatLonAxisLabels1(axTicks(1), axTicks(2))
	axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tck=-0.01, cex.axis = 0.8)
	axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tck=-0.01, las = 1, cex.axis = 0.8)
	plt <- par("plt")
	usr <- par("usr")
	par(opar)

	return(list(plt = plt, usr = usr))
}


#################################################################

displayMap4Extraction <- function(parent, shpf, ZoomXYval, notebookTab){

	ocrds <- getBoundaries(shpf)
	selectedPolygon <- NULL
	pltusr <- NULL
	parPlotSize1 <- tclVar()
	parPlotSize2 <- tclVar()
	parPlotSize3 <- tclVar()
	parPlotSize4 <- tclVar()
	usrCoords1 <- tclVar()
	usrCoords2 <- tclVar()
	usrCoords3 <- tclVar()
	usrCoords4 <- tclVar()
	plotIt <- function(){
		op <- par(bg = 'white')
		pltusr <<- plotMap4Extraction(ocrds, ZoomXYval, selectedPolygon)
		tclvalue(parPlotSize1) <<- pltusr$plt[1]
		tclvalue(parPlotSize2) <<- pltusr$plt[2]
		tclvalue(parPlotSize3) <<- pltusr$plt[3]
		tclvalue(parPlotSize4) <<- pltusr$plt[4]
		tclvalue(usrCoords1) <<- pltusr$usr[1]
		tclvalue(usrCoords2) <<- pltusr$usr[2]
		tclvalue(usrCoords3) <<- pltusr$usr[3]
		tclvalue(usrCoords4) <<- pltusr$usr[4]
		par(op)
	}

	parPltCrd <- list(parPlotSize1 = parPlotSize1, parPlotSize2 = parPlotSize2,
	parPlotSize3 = parPlotSize3, parPlotSize4 = parPlotSize4,
	usrCoords1 = usrCoords1, usrCoords2 = usrCoords2, usrCoords3 = usrCoords3, usrCoords4 = usrCoords4)

	###################################################################

	onglet <- imageNotebookTab_open(parent, notebookTab, tabTitle=' Map ',AllOpenTabType, AllOpenTabData)
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

	tkbind(canvas,"<Enter>", function(){
		if(tclvalue(pressButP) == "1") tkconfigure(canvas, cursor = 'sizing')
		else if(tclvalue(pressButM) == "1") tkconfigure(canvas, cursor = 'sizing')
		else if(tclvalue(pressButRect) == "1") tkconfigure(canvas, cursor = 'sizing')
		else if(tclvalue(pressButDrag) == "1") tkconfigure(canvas, cursor = 'hand1')
		else if(tclvalue(pressGetCoords) == "1") tkconfigure(canvas, cursor='draped_box')
		else tkconfigure(canvas, cursor = 'crosshair')
	})

	tkbind(canvas,"<Leave>", function() tkconfigure(canvas, cursor=''))

	#####
	##draw rectangle initial value
	lastX <- 0
	lastY <- 0

	##zoom factor
	factZoom <- 0.2

	##zoom rectangle
	rectZoomInit <- ZoomXYval

	##Pan Image
	panZoomInit <- c(0,0,0,0,0,0)
	factPan <- 0.2

	##########

	tkbind(canvas, "<Button-1>", function(W, x, y){
		ret <- getXYCoords(W, x, y, parPltCrd)
		tkdelete(W, 'rect')

		##get coordinates or polygon id
		if(tclvalue(pressGetCoords) == "1" & !ret$oin){
			if(tclvalue(area_type) == "Point"){
				tclvalue(minlonRect) <<- round(ret$xc, 4)
				tclvalue(maxlonRect) <<- ''
				tclvalue(minlatRect) <<- round(ret$yc, 4)
				tclvalue(maxlatRect) <<- ''
				selectedPolygon <<- NULL
			}

			##
			if(tclvalue(area_type) == "Rectangle"){
				pPressRect(W, x, y, width = 1, outline = "red")
				#cat(paste('deb', ret$xc, ret$yc),'\n')
				tclvalue(minlonRect) <<- round(ret$xc, 4)
				tclvalue(minlatRect) <<- round(ret$yc, 4)
				selectedPolygon <<- NULL
			}

			##
			if(tclvalue(area_type) == "Polygon"){
				xypts <- data.frame(x = ret$xc, y = ret$yc)
				coordinates(xypts)=~x+y
				admin_name <- over(xypts, shpf)
				admin_name <- c(t(admin_name[1,]))

				ids <- as.numeric(tclvalue(tcl(adminVar.tab1, 'current')))+1
				admin_name <- admin_name[ids]
				if(!is.na(admin_name)){
					tclvalue(namePoly) <<- as.character(admin_name)
					selectedPolygon <<- getBoundaries(shpf[shpf@data[,ids] == tclvalue(namePoly),])
				}else{
					selectedPolygon <<- NULL
				}
			}

			##
			if(tclvalue(area_type) == 'Multiple Points'){
				tclvalue(minlonRect) <<- round(ret$xc, 4)
				tclvalue(maxlonRect) <<- ''
				tclvalue(minlatRect) <<- round(ret$yc, 4)
				tclvalue(maxlatRect) <<- ''
				selectedPolygon <<- NULL
			}

			##
			if(tclvalue(area_type) == 'Multiple Polygons'){
				xypts <- data.frame(x = ret$xc, y = ret$yc)
				coordinates(xypts)=~x+y
				admin_name <- over(xypts, shpf)
				admin_name <- c(t(admin_name[1,]))

				ids <- as.numeric(tclvalue(tcl(adminVar.tab1, 'current')))+1
				admin_name <- admin_name[ids]
				if(!is.na(admin_name)){
					tclvalue(namePoly) <<- as.character(admin_name)
					selectedPolygon <<- getBoundaries(shpf[shpf@data[,ids] == tclvalue(namePoly),])
				}else{
					selectedPolygon <<- NULL
				}
			}


			refreshPlot1(W, img, hscale = as.numeric(tclvalue(tkget(spinH))), vscale = as.numeric(tclvalue(tkget(spinV))))
		}

		#Zoom plus
		if(tclvalue(pressButP) == "1" & !ret$oin){
			rgX <- as.numeric(tclvalue(usrCoords2))-as.numeric(tclvalue(usrCoords1))
			rgY <- as.numeric(tclvalue(usrCoords4))-as.numeric(tclvalue(usrCoords3))
			shiftX <- rgX*(1-factZoom)/2
			shiftY <- rgY*(1-factZoom)/2
			xmin1 <- ret$xc-shiftX
			xmax1 <- ret$xc+shiftX
			ymin1 <- ret$yc-shiftY
			ymax1 <- ret$yc+shiftY
			ZoomXYval <<- c(xmin1, xmax1, ymin1, ymax1)
			tclvalue(xx1) <<- round(xmin1, 4)
			tclvalue(xx2) <<- round(xmax1, 4)
			tclvalue(yy1) <<- round(ymin1, 4)
			tclvalue(yy2) <<- round(ymax1, 4)
			refreshPlot1(W, img, hscale = as.numeric(tclvalue(tkget(spinH))), vscale = as.numeric(tclvalue(tkget(spinV))))
		}

		#Zoom Moins
		if(tclvalue(pressButM) == "1"  & !ret$oin){
			rgX <- as.numeric(tclvalue(usrCoords2))-as.numeric(tclvalue(usrCoords1))
			rgY <- as.numeric(tclvalue(usrCoords4))-as.numeric(tclvalue(usrCoords3))
			shiftX <- rgX*(1+factZoom)/2
			shiftY <- rgY*(1+factZoom)/2
			xmin1 <- ret$xc-shiftX
			xmax1 <- ret$xc+shiftX
			ymin1 <- ret$yc-shiftY
			ymax1 <- ret$yc+shiftY

			if(xmin1< -180 | xmax1 > 180 | ymin1< -90 | ymax1 > 90){
				tclvalue(pressButP) <<- 0
				tclvalue(pressButM) <<- 0
				tclvalue(pressButRect) <<- 0
				tclvalue(pressButDrag) <<- 0
				tkconfigure(btZoomP.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
				tkconfigure(btZoomM.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
				tkconfigure(btZoomRect.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
				tkconfigure(btPanImg.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
				tkconfigure(W, cursor = 'crosshair')
			}else{
				ZoomXYval <<- c(xmin1, xmax1, ymin1, ymax1)
				tclvalue(xx1) <<- round(xmin1, 4)
				tclvalue(xx2) <<- round(xmax1, 4)
				tclvalue(yy1) <<- round(ymin1, 4)
				tclvalue(yy2) <<- round(ymax1, 4)
				refreshPlot1(W, img, hscale = as.numeric(tclvalue(tkget(spinH))), vscale = as.numeric(tclvalue(tkget(spinV))))
			}
		}

		##Zoom rectangle
		if(tclvalue(pressButRect) == "1"  & !ret$oin){
			pPressRect(W, x, y, width = 1, outline = "red")
			rectZoomInit[1] <<- ret$xc
			rectZoomInit[3] <<- ret$yc
		}

		##Pan image
		if(tclvalue(pressButDrag) == "1"  & !ret$oin){
			panZoomInit[1] <<- ret$xc
			panZoomInit[2] <<- ret$yc
			panZoomInit[3] <<- as.numeric(tclvalue(xx1))
			panZoomInit[4] <<- as.numeric(tclvalue(xx2))
			panZoomInit[5] <<- as.numeric(tclvalue(yy1))
			panZoomInit[6] <<- as.numeric(tclvalue(yy2))
			tkconfigure(canvas, cursor = 'hand2')
		}

	})

	##########
	tkbind(canvas, "<Motion>", function(W, x, y){
		displayCursorPosition3Var(W, x, y, parPltCrd, xpcoord, ypcoord, zpcoord, getAdminLabel, shp = shpf, idField = adminVar.tab1)
	})


	#########
	tkbind(canvas, "<B1-Motion>", function(W, x, y){
		ret <- getXYCoords(W, x, y, parPltCrd)

		##get coordinates rect
		if(tclvalue(pressGetCoords) == "1"  & tclvalue(area_type) == "Rectangle"){
			pMoveRect(W, x, y)
			tclvalue(maxlonRect) <<- round(ret$xc, 4)
			tclvalue(maxlatRect) <<- round(ret$yc, 4)
		}

		##Zoom rectangle
		if(tclvalue(pressButRect) == "1"){
			pMoveRect(W, x, y)
		}

		##Pan image
		if(tclvalue(pressButDrag) == "1"){
			transX <- ret$xc-panZoomInit[1]
			transY <- ret$yc-panZoomInit[2]
			tclvalue(xx1) <<- round(panZoomInit[3]+factPan*transX, 4)
			tclvalue(xx2) <<- round(panZoomInit[4]+factPan*transX, 4)
			tclvalue(yy1) <<- round(panZoomInit[5]+factPan*transY, 4)
			tclvalue(yy2) <<- round(panZoomInit[6]+factPan*transY, 4)
			ZoomXYval <<- as.numeric(c(tclvalue(xx1), tclvalue(xx2), tclvalue(yy1), tclvalue(yy2)))
			refreshPlot1(W, img, hscale = as.numeric(tclvalue(tkget(spinH))), vscale = as.numeric(tclvalue(tkget(spinV))))
		}
	})

	#########
	tkbind(canvas, "<ButtonRelease>", function(W, x, y){
		ret <- getXYCoords(W, x, y, parPltCrd)

		##get coordinates rect
		if(tclvalue(pressGetCoords) == "1"){
			if(tclvalue(area_type) == "Rectangle"){
				xpr <- c(as.numeric(tclvalue(minlonRect)), round(ret$xc, 4), as.numeric(tclvalue(minlatRect)), round(ret$yc, 4))
				if(xpr[1] > xpr[2]) xpr <- xpr[c(2,1,3,4)]
				if(xpr[3] > xpr[4]) xpr <- xpr[c(1,2,4,3)]
				tclvalue(minlonRect) <<- xpr[1]
				tclvalue(maxlonRect) <<- xpr[2]
				tclvalue(minlatRect) <<- xpr[3]
				tclvalue(maxlatRect) <<- xpr[4]
			}
			tclvalue(pressGetCoords) <<- 0
			tkconfigure(getArea.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(W, cursor = 'crosshair')
		}

		##Zoom rectangle
		if(tclvalue(pressButRect) == "1"){
			rectZoomInit[2] <<- ret$xc
			rectZoomInit[4] <<- ret$yc
			if(rectZoomInit[1] > rectZoomInit[2]) rectZoomInit <- rectZoomInit[c(2,1,3,4)]
			if(rectZoomInit[3] > rectZoomInit[4]) rectZoomInit <- rectZoomInit[c(1,2,4,3)]
			ZoomXYval <<- rectZoomInit
			tclvalue(xx1) <<- round(rectZoomInit[1], 4)
			tclvalue(xx2) <<- round(rectZoomInit[2], 4)
			tclvalue(yy1) <<- round(rectZoomInit[3], 4)
			tclvalue(yy2) <<- round(rectZoomInit[4], 4)
			refreshPlot1(W, img, hscale = as.numeric(tclvalue(tkget(spinH))), vscale = as.numeric(tclvalue(tkget(spinV))))
			tkdelete(W, 'rect')
		}

		##Pan image
		if(tclvalue(pressButDrag) == "1"){
			tkconfigure(canvas, cursor = 'hand1')
		}
	})

	###############################################

	tkbind(canvas, "<Button-3>", function(W){
		tclvalue(pressButP) <<- 0
		tclvalue(pressButM) <<- 0
		tclvalue(pressButRect) <<- 0
		tclvalue(pressButDrag) <<- 0
		tkconfigure(btZoomP.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomM.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btZoomRect.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(btPanImg.tab3, relief = 'raised', bg = 'lightblue', state = 'normal')

		tkconfigure(canvas, cursor = 'crosshair')

		tkdelete(W, 'rect')

		selectedPolygon <<- NULL
		refreshPlot1(W, img, hscale = as.numeric(tclvalue(tkget(spinH))), vscale = as.numeric(tclvalue(tkget(spinV))))
	})

	###
	return(list(onglet, list(canvas, img)))
}



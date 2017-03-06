
plotNetCDFdata <- function(donne, atLev, listCol, ocrds, units){
	##color and tick
	ticks <- as.numeric(atLev)
	nticks <- length(ticks)
	labticks <- ticks  #paste(ticks, expression(paste(degree, "C", sep = '')))
	loko <- colorRampPalette(listCol)(nticks-1)
	units <- str_trim(units)

	ret <- PlotImage(donne, at = ticks, col = loko, shpd = ocrds, units = units, grid = TRUE,
				colorKey = list(labels = labticks))
	print(ret)
}


####################################################################################

displayNetCDFdata <- function(parent, notebookTab, donne, atLev, listCol, shpf,
							units, blank, title.tab){
	if(is.null(donne)){
		InsertMessagesTxt(main.txt.out, 'No NetCDF data found', format = TRUE)
		return(NULL)
	}
	atLev <- as.numeric(atLev)
	atLev <- atLev[!is.na(atLev)]
	if(length(atLev) < 2){
		InsertMessagesTxt(main.txt.out, 'Levels must be at least 2', format = TRUE)
		return(NULL)
	}
	
	ocrds <- getBoundaries(shpf)
	if(blank == '1'){
		plotgrd <- expand.grid(x = donne$x, y = donne$y)
		coordinates(plotgrd) <- ~x+y
		plotgrd <- SpatialPixels(points = plotgrd, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))
		shpf[['vtmp']] <- 1
		shpMask <- over(plotgrd, shpf)[,'vtmp']
		outMask <- matrix(shpMask, nrow = length(donne$x), ncol = length(donne$y))
		donne$value[is.na(outMask)] <- NA
	}

	plotIt <- function(){
		plotNetCDFdata(donne, atLev, listCol, ocrds, units)
	}
	
	###################################################################	

	onglet <- imageNotebookTab_open(parent, notebookTab, tabTitle = paste('Map -', substr(title.tab, 1, 16)), AllOpenTabType, AllOpenTabData)

	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))
	
	hscrFrame <- as.integer(tclvalue(tkwinfo("height", panel.right)))
	wscrFrame <- as.integer(tclvalue(tkwinfo("width", panel.right)))
	
	scrollwin <- bwScrolledWindow(onglet[[2]])
	tkgrid(scrollwin)
	tkgrid.rowconfigure(scrollwin, 0, weight = 1)
	tkgrid.columnconfigure(scrollwin, 0, weight = 1)
	containerFrame <- bwScrollableFrame(scrollwin, width = wscrFrame, height = hscrFrame)
	
	img <- tkrplot1(containerFrame, fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	
	return(list(onglet, img))
}

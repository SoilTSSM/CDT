plotGGvsSatellite <- function(outValiddata, dataType){
	if(tclvalue(dataType) == 'All Data'){
		x <- outValiddata$x
		y <- outValiddata$y
	}else{
		x <- outValiddata$xs
		y <- outValiddata$ys
	}
	grphlim <- c(0, max(x, y))
	if(outValiddata$clim.var == 'RR') ylab <- 'RFE'
	if(outValiddata$clim.var == 'TT') ylab <- 'Estimate'
	plot(x, y, xlab = "Gauge", ylab = ylab, xlim = grphlim, ylim = grphlim, type = 'n')
	grid()
	abline(a = 0, b = 1, lwd = 2, col = 'red')
	points(x, y)
}

###############################
displayGGvsSatFun <- function(parent, notebookTab, outValiddata, dataType){

	plotIt <- function(){
		plotGGvsSatellite(outValiddata, dataType)
	}

	onglet <- imageNotebookTab_open(parent, notebookTab, tabTitle = 'Scatter Plot', AllOpenTabType, AllOpenTabData)

	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))
	hscale <- min(hscale, vscale)
	vscale <- min(hscale, vscale)

	hscrFrame <- as.integer(tclvalue(tkwinfo("height", panel.right)))
	wscrFrame <- as.integer(tclvalue(tkwinfo("width", panel.right)))

	scrollwin <- bwScrolledWindow(onglet[[2]])
	tkgrid(scrollwin)
	tkgrid.rowconfigure(scrollwin, 0, weight = 1)
	tkgrid.columnconfigure(scrollwin, 0, weight = 1)
	containerFrame <- bwScrollableFrame(scrollwin, width = wscrFrame, height = hscrFrame)

	img <- tkrplot(containerFrame, fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)

	return(list(onglet, img))
}

######################################################
cdfGGvsSatellite <- function(outValiddata, dataType){
	if(tclvalue(dataType) == 'All Data'){
		x <- outValiddata$x
		y <- outValiddata$y
	}else{
		x <- outValiddata$xs
		y <- outValiddata$ys
	}
	if(outValiddata$clim.var == 'RR'){
		xlab <- "Rainfall [mm]"
		lgtxt <- c('Gauge', 'RFE')
	}
	if(outValiddata$clim.var == 'TT'){
		xlab <- "Temperature"
		lgtxt <- c('Gauge', 'Estimate')
	}
	plot(ecdf(x), xlab = xlab, ylab = "CDF", main = 'Empirical CDF', col = 'blue', lwd = 2, cex = 0.4, ylim = c(0, 1))
	grid()
	plot(ecdf(y), add = TRUE, col = "red", lwd = 2, cex = 0.4)
	legend('bottomright', lgtxt, col = c('blue', 'red'), lwd = 3, bg = 'lightgray')
}

###############################
displayCDFGGvsSatFun <- function(parent, notebookTab, outValiddata, dataType){

	plotIt <- function(){
		cdfGGvsSatellite(outValiddata, dataType)
	}

	onglet <- imageNotebookTab_open(parent, notebookTab, tabTitle = 'Empirical CDF - Plot', AllOpenTabType, AllOpenTabData)

	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))
	hscale <- min(hscale, vscale)
	vscale <- min(hscale, vscale)

	hscrFrame <- as.integer(tclvalue(tkwinfo("height", panel.right)))
	wscrFrame <- as.integer(tclvalue(tkwinfo("width", panel.right)))

	scrollwin <- bwScrolledWindow(onglet[[2]])
	tkgrid(scrollwin)
	tkgrid.rowconfigure(scrollwin, 0, weight = 1)
	tkgrid.columnconfigure(scrollwin, 0, weight = 1)
	containerFrame <- bwScrollableFrame(scrollwin, width = wscrFrame, height = hscrFrame)

	img <- tkrplot(containerFrame, fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)

	return(list(onglet, img))
}



plotVariogramFun <- function(odata, vgmChx, VgmMod, vgmModList, useELV){
	if(vgmChx == '0'){
		if(useELV == '1') evgm <- variogram(z~elv, odata)
		else evgm <- variogram(z~1, odata)
		#fvgm <- fit.variogram(evgm, model = vgm(psill = max(evgm$gamma, na.rm = T), model = VgmMod, range = max(evgm$dist, na.rm = T), nugget = min(evgm$gamma, na.rm = T)))
		fvgm <- fit.variogram(evgm, model = vgm(psill = var(odata$z, na.rm = T), model = VgmMod, range = quantile(evgm$dist, probs = 0.8, na.rm = T), nugget = min(evgm$gamma, na.rm = T)))
		p1 <- plot(evgm, fvgm, plot.numbers = TRUE, main = "Experimental variogram and fitted variogram model", xlab = 'Distance')
	}

	if(vgmChx == '1'){
		if(useELV == '1') autovgm <- autofitVariogram(z~elv, model = vgmModList, input_data = odata)
		else autovgm <- autofitVariogram(z~1, model = vgmModList, input_data = odata)
		p1 <- plot(autovgm)
	}
	print(p1)
}


###############################
displayVariogramFun <- function(parent, notebookTab, donne, vgmChx, VgmMod, vgmModList, useELV){
	if(is.null(donne)){
		InsertMessagesTxt(main.txt.out, 'No station data found', format = TRUE)
		return(NULL)
	}

	if(useELV == '1' & is.null(donne$elv)){
		InsertMessagesTxt(main.txt.out, 'No DEM data found', format = TRUE)
		return(NULL)
	}

	odata <- data.frame(lon = donne$lon, lat = donne$lat, z = donne$z)
	if(useELV == '1'){
		odata$elv <- donne$elv
		odata$elv[is.na(odata$elv)] <- 1
	}
	odata <- odata[!is.na(odata$z), ]

	if(nrow(odata) < 5){
		InsertMessagesTxt(main.txt.out, 'Number of observations is too small to interpolate', format = TRUE)
		return(NULL)
	}

	if(vgmChx == '1' & length(vgmModList) < 1){
		InsertMessagesTxt(main.txt.out, 'No variogram model found', format = TRUE)
		return(NULL)
	}

	coordinates(odata)<- ~lon+lat
	proj4string(odata) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")
	#odata <- SpatialPointsDataFrame(coords = odata[,c('lon', 'lat')],data = data.frame(z = odata[,'z']), proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))


	plotIt <- function(){
		plotVariogramFun(odata, vgmChx, VgmMod, vgmModList, useELV)
	}

	###################################################################

	onglet <- imageNotebookTab_open(parent, notebookTab, tabTitle = paste('Variogram -',donne$date), AllOpenTabType, AllOpenTabData)

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

	return(list(onglet, img))
}

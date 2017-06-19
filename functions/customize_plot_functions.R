
##get boundaries from shapefile

getBoundaries <- function(shpf){
	ocrds <- matrix(NA, nrow = 1, ncol = 2)
	if(!is.null(shpf)){
		retPolygon <- lapply(slot(shpf, "polygons"), function(i) slot(i, "Polygons"))
		polys <- lapply(retPolygon, function(x){
			ret <- NULL
			for(i in seq_along(x)){
				poly <- rbind(slot(x[[i]], "coords"), cbind(NA, NA))
				ret <- rbind(ret, poly)
			}
			ret
		})
		ocrds <- do.call('rbind', polys)
	}
	return(ocrds)
}

######################################
##format lat-lon axis label
LatLonAxisLabels <- function(axis.x, axis.y){
	axis.x <- ifelse(axis.x > 180, -360 + axis.x, axis.x)
	sym1 <- ifelse(axis.x < 0 & axis.x > -180, 'W', ifelse(axis.x > 0 & axis.x < 180, 'E', ''))
	sym1 <- paste("paste(", paste(abs(axis.x), "*degree"), ",", sym1, ")", collapse = ',')
	lon_lab <- eval(parse(text = paste("expression(", sym1, ")", sep = "")))
	sym2 <- ifelse(axis.y < 0, 'S', ifelse(axis.y > 0, 'N', ''))
	sym2 <- paste("paste(", paste(abs(axis.y), "*degree"), ",", sym2, ")", collapse = ',')
	lat_lab <- eval(parse(text = paste("expression(", sym2, ")", sep = "")))
	return(list(xaxl = lon_lab, yaxl = lat_lab))
}

#######################
LatLonAxisLabels1 <- function(axis.x, axis.y){
	axis.x <- ifelse(axis.x > 180, -360 + axis.x, axis.x)
	sym1 <- ifelse(axis.x < 0 & axis.x > -180, 'W', ifelse(axis.x > 0 & axis.x < 180, 'E', ''))
	lon_lab <- paste(abs(axis.x), "°", sym1, sep = '')
	sym2 <- ifelse(axis.y < 0, 'S', ifelse(axis.y > 0, 'N', ''))
	lat_lab <- paste(abs(axis.y), "°", sym2, sep = '')
	return(list(xaxl = lon_lab, yaxl = lat_lab))
}

#######################
LatLonAxisLabels2 <- function(axis.x, axis.y){
	frac <- function(x) abs(x - trunc(x))
	axis.x <- ifelse(axis.x > 180, -360 + axis.x, axis.x)
	degLo <- abs(trunc(axis.x))
	degLa <- abs(trunc(axis.y))
	xm <- frac(axis.x)*60
	ym <- frac(axis.y)*60
	minLo <- trunc(xm)
	minLa <- trunc(ym)
	secLo <- round(frac(axis.x)*60, 2)
	secLa <- round(frac(axis.y)*60, 2)
	sym1 <- ifelse(axis.x < 0 & axis.x > -180, 'W', ifelse(axis.x > 0 & axis.x < 180, 'E', ''))
	sym1 <- paste("paste(", paste(degLo, "*degree", ",", minLo, "*minute", ",", secLo, "*second"), ",", sym1, ")", collapse = ',')
	lon_lab <- eval(parse(text = paste("expression(", sym1, ")", sep = "")))
	sym2 <- ifelse(axis.y < 0, 'S', ifelse(axis.y > 0, 'N', ''))
	sym2 <- paste("paste(", paste(degLa, "*degree", ",", minLa, "*minute", ",", secLa, "*second"), ",", sym2, ")", collapse = ',')
	lat_lab <- eval(parse(text = paste("expression(", sym2, ")", sep = "")))
	return(list(xaxl = lon_lab, yaxl = lat_lab))
}

######################################
###Display lat-lon in status bar

LatLonLabels <- function(xlon, xlat){
	frac <- function(x) abs(x - trunc(x))
	xlon <- ifelse(xlon > 180, -360 + xlon, xlon)
	degLo <- abs(trunc(xlon))
	degLa <- abs(trunc(xlat))
	xm <- frac(xlon)*60
	ym <- frac(xlat)*60
	minLo <- trunc(xm)
	minLa <- trunc(ym)
	secLo <- round(frac(xm)*60, 2)
	secLa <- round(frac(ym)*60, 2)
	sLon <- ifelse(xlon > 0 & xlon < 180, 'E', ifelse(xlon > -180 & xlon < 0, 'W', ''))
	sLat <- ifelse(xlat > 0, 'N', ifelse(xlat < 0, 'S', ''))
	degsym <- if(Sys.info()["sysname"] == "Windows") "\xb0 " else  "° "
	lon_lab <- paste(sprintf("%03d", degLo), degsym, sprintf("%02d", minLo), "' ",
				paste(sprintf("%02d", trunc(secLo)), substr(sprintf("%.2f", frac(secLo)), 3, 4), sep = '.'), '" ', sLon, sep = '')
	lat_lab <- paste(sprintf("%03d", degLa), degsym, sprintf("%02d", minLa), "' ",
				paste(sprintf("%02d", trunc(secLa)), substr(sprintf("%.2f", frac(secLa)), 3, 4), sep = '.'), '" ', sLat, sep = '')
	return(list(xdisp = lon_lab, ydisp = lat_lab))
}

######################################
###Layout for lattice plot
manageLayout <- function(nbPlot, transpose = FALSE){
	n <- 5 #nmaxPlot = n*n+n
	nLayout <- cbind(c(rep(1:n, seq(1, n*2,2)), rep(n+1, n)), rep(1:n, seq(2, n*2,2)))
	dimLayout <- nLayout[nbPlot,]
	if(transpose) dimLayout <- rev(dimLayout)

	matdim <- dimLayout[1]*dimLayout[2]
	mat <- rep(NA, matdim)
	mat[((matdim-nbPlot)+1):matdim] <- 1:nbPlot
	matLayout <- matrix(mat, ncol = dimLayout[1], nrow = dimLayout[2], byrow = T)
	line1 <- matLayout[1,]
	line1 <- line1[!is.na(line1)]
	ltmp <- rep(NA, dimLayout[1])
	ltmp[line1] <- line1
	matLayout[1,] <- ltmp
	matLayout <- matLayout[dimLayout[2]:1,]
	orderLayout <- c(t(matLayout))
	orderLayout <- orderLayout[!is.na(orderLayout)]

	return(list(dim = dimLayout, order = orderLayout))
}

################
##Axis ticks
parAxisPlotFun <- function(x, factor = 0.04){
	#factor = 0.04 par defaut pour R
	x <- x[!is.na(x)]
	if(length(x) > 1) 	xlim <- range(x) + c(-1, 1) * factor * diff(range(x))
	else if(length(x) == 1 & x != 0) xlim <- x + c(-1, 1) * factor * abs(x)
	else  xlim <- factor * c(-1,1)
	xtick <- pretty(x)
	bInf <- min(xtick[xtick >= min(xlim)])
	bSup <- max(xtick[xtick <= max(xlim)])
	intervl <- length(which(xtick >= bInf & xtick <= bSup))-1
	ticks <- axTicks(side = 1, usr = xlim, axp = c(bInf, bSup, intervl))
	return(list(usr = xlim, axp = ticks))
}

getParUsrPlot <- function(x, factor = 0.04){
	x <- x[!is.na(x)]
	xlim <- range(x) + factor * diff(range(x)) * c(-1,1)
	return(xlim)
}

######################################
## Plot one gridded data

PlotImage <- function(donne, at = NULL, col = rainbow(20), shpd = NULL, units = NULL,
					latlon.axis = TRUE, grid = FALSE, interpolate = FALSE, 
					colorKey = list(place = NULL, eqDist = TRUE, labels = NULL,
					raster = FALSE, interp = FALSE, height = 1, width = 1.1))
{
	if(is.null(at)){
		zlim <- range(donne$z, na.rm = TRUE)
		ticks <- pretty(zlim)
	}else ticks <- at
	nticks <- length(ticks)
	col <- colorRampPalette(col)(nticks-1)

	if(!is.null(units)){
		 units <- str_trim(units)
		 colorkeyTitle <- if(units != "") paste('(', units, ')', sep = '') else ''
	}else colorkeyTitle <- ''

	PlotObj <- levelplot(donne$z, row.values = donne$x, column.values = donne$y,
				at = ticks, interpolate = interpolate, region = TRUE,
				panel = function(...){
					panel.levelplot(...)
					if(!is.null(shpd)) panel.lines(shpd, col = "black", lwd = 0.5)
					if(grid) panel.abline(h = grid.y, v = grid.x, col = "lightgray", lty = 3)
				}, colorkey = FALSE)

	##X-Y Axis
	toutLon <- c(shpd[, 1], donne$x)
	toutLat <- c(shpd[, 2], donne$y)
	parLon <- parAxisPlotFun(toutLon)
	parLat <- parAxisPlotFun(toutLat)
	axis.x <- grid.x <- parLon$axp
	axis.y <- grid.y <- parLat$axp
	xlim <- parLon$usr
	ylim <- parLat$usr

	##Axis lab
	axlabs <- if(latlon.axis) LatLonAxisLabels(axis.x, axis.y) else list(xaxl = axis.x, yaxl = axis.y)
	Xaxis = list(relation = "same", draw = TRUE, alternating = 1, at = axis.x, labels = axlabs$xaxl, tck = c(1, 0))
	Yaxis = list(relation = "same", draw = TRUE, alternating = 1, at = axis.y, labels = axlabs$yaxl, tck = c(1, 0))

	###Colorkey 
	colorKey0 = list(place = NULL, eqDist = TRUE, labels = NULL,
					raster = FALSE, interp = FALSE, height = 0.7, width = 1.1)
	colorKey <- c(colorKey, colorKey0[!(names(colorKey0)%in%intersect(names(colorKey0), names(colorKey)))])

	if(is.null(colorKey$place)){
		colorkeyPlace <- if(diff(xlim) >= diff(ylim)) 'bottom' else 'right'
	}else colorkeyPlace <- colorKey$place
	atcolorkey <- if(colorKey$eqDist) 1:nticks else ticks
	labticks <- if(!is.null(colorKey$labels)) colorKey$labels else ticks

	colorkeyRaster <- colorKey$raster
	colorkeyInterp <- colorKey$interp
	colorkeyHeight <- colorKey$height
	colorkeyWidth <- colorKey$width

	### Colorkey option
	if(colorkeyPlace == 'bottom'){
		layout.pad <- c(1, 1, 1, 2) #left, right, top, bottom
		posTitle <- 'right'
		xyposTitle <- c(1, 0.2)
		justTitle <- c("center", "center")
		rotTitle <- 0 
	}
	if(colorkeyPlace == 'right'){
		layout.pad <- c(1, 2, 1, 1)
		posTitle <- 'top'
		xyposTitle <- c(1, 1.5)
		justTitle <- c("right", "center")
		rotTitle <- 0
	}

	##par.settings
	parSettings <- list(background = list(alpha = 1, col = 'white'),
						layout.widths = list(left.padding = layout.pad[1], right.padding = layout.pad[2]),
						layout.heights = list(top.padding = layout.pad[3], bottom.padding = layout.pad[4]))

	##Colorkey
	colorkey <- list(space = colorkeyPlace, col = col, width = colorkeyWidth, height = colorkeyHeight,
					raster = colorkeyRaster, interpolate = colorkeyInterp, at = atcolorkey,
					labels = list(labels = labticks, at = atcolorkey, cex = 0.8, col = 'black', rot = 0),
					axis.line = list(alpha = 0.5, lty = 1, lwd = 1, col = 'black'))
	colorkeyFrame <- draw.colorkey(key = colorkey, draw = FALSE, vp = NULL)

	##add legend title
	grobObj <- textGrob(colorkeyTitle, x = xyposTitle[1], y = xyposTitle[2], just = justTitle, rot = rotTitle,
						gp = gpar(fontsize = 12, fontface = 'plain', col = "black", cex = 0.8))
	lezandyGrob <- packGrob(frame = colorkeyFrame, grob = grobObj, side = posTitle, dynamic = TRUE)

	##legend function
	if(colorkeyPlace == 'bottom'){
		lezandy <- list(bottom = list(fun = lezandyGrob))
	}else if(colorkeyPlace == 'right'){
		lezandy <- list(right = list(fun = lezandyGrob))
	}

	retplot <- update(PlotObj, aspect = 'fill', as.table = TRUE, par.settings = parSettings, xlab = '', ylab = '',
				xlim = xlim, ylim = ylim, col.regions = col, scales = list(x = Xaxis, y = Yaxis), legend = lezandy)

	return(retplot)
}






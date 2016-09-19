
##get boundaries from shapefile

getBoundaries <- function(shpf){
	if(!is.null(shpf)){
		retPolygon <- lapply(slot(shpf, "polygons"), function(i) slot(i, "Polygons"))
		polys <- lapply(retPolygon, function(x){
			ret <- NULL
			for(i in seq_along(x)){
				poly <- rbind(slot(x[[i]], "coords"), cbind(NA, NA) )
				ret <- rbind(ret, poly)
			}
			ret
		})

		ocrds <- do.call('rbind', polys)
	}else{
		ocrds <- matrix(NA, nrow = 1, ncol = 2)
	}
	return(ocrds)
}

######################################
##format lat-lon axis label
LatLonAxisLabels <- function(axis.x, axis.y){
	axis.x <- ifelse(axis.x > 180, -360 +axis.x, axis.x)
	sym1 <- ifelse(axis.x < 0 & axis.x> -180, 'W', ifelse(axis.x > 0 & axis.x < 180, 'E',''))
	sym1 <- paste("paste(", paste(abs(axis.x), "*degree") ,",", sym1,")", collapse=',')
	lon_lab <- eval(parse(text = paste("expression(", sym1,")", sep = "")))
	sym2 <- ifelse(axis.y < 0, 'S', ifelse(axis.y > 0, 'N',''))
	sym2 <- paste("paste(", paste(abs(axis.y), "*degree") ,",", sym2,")", collapse=',')
	lat_lab <- eval(parse(text = paste("expression(", sym2,")", sep = "")))
	return(list(xaxl = lon_lab, yaxl = lat_lab))
}

#######################
LatLonAxisLabels1 <- function(axis.x, axis.y){
	axis.x <- ifelse(axis.x > 180, -360 +axis.x, axis.x)
	sym1 <- ifelse(axis.x < 0 & axis.x> -180, 'W', ifelse(axis.x > 0 & axis.x < 180, 'E',''))
	lon_lab <- paste(abs(axis.x), "°", sym1, sep = '')
	sym2 <- ifelse(axis.y < 0, 'S', ifelse(axis.y > 0, 'N',''))
	lat_lab <- paste(abs(axis.y), "°", sym2, sep = '')
	return(list(xaxl = lon_lab, yaxl = lat_lab))
}

#######################
LatLonAxisLabels2 <- function(axis.x, axis.y){
	frac <- function(x) abs(x - trunc(x))
	axis.x <- ifelse(axis.x > 180, -360 +axis.x, axis.x)
	degLo <- abs(trunc(axis.x))
	degLa <- abs(trunc(axis.y))
	xm <- frac(axis.x)*60
	ym <- frac(axis.y)*60
	minLo <- trunc(xm)
	minLa <- trunc(ym)
	secLo <- round(frac(axis.x)*60,2)
	secLa <- round(frac(axis.y)*60,2)
	sym1 <- ifelse(axis.x < 0 & axis.x> -180, 'W', ifelse(axis.x > 0 & axis.x < 180, 'E',''))
	sym1 <- paste("paste(", paste(degLo,"*degree",",", minLo,"*minute",",", secLo,"*second") ,",", sym1,")", collapse=',')
	lon_lab <- eval(parse(text = paste("expression(", sym1,")", sep = "")))
	sym2 <- ifelse(axis.y < 0, 'S', ifelse(axis.y > 0, 'N',''))
	sym2 <- paste("paste(", paste(degLa,"*degree",",", minLa,"*minute",",", secLa,"*second") ,",", sym2,")", collapse=',')
	lat_lab <- eval(parse(text = paste("expression(", sym2,")", sep = "")))
	return(list(xaxl = lon_lab, yaxl = lat_lab))
}

######################################
###Display lat-lon in status bar

LatLonLabels <- function(xlon, xlat){
	frac <- function(x) abs(x - trunc(x))
	xlon <- ifelse(xlon > 180, -360 +xlon, xlon)
	degLo <- abs(trunc(xlon))
	degLa <- abs(trunc(xlat))
	xm <- frac(xlon)*60
	ym <- frac(xlat)*60
	minLo <- trunc(xm)
	minLa <- trunc(ym)
	secLo <- round(frac(xm)*60,2)
	secLa <- round(frac(ym)*60,2)
	sLon <- ifelse(xlon > 0 & xlon < 180, 'E', ifelse(xlon> -180 & xlon < 0, 'W',''))
	sLat <- ifelse(xlat > 0, 'N', ifelse(xlat < 0, 'S',''))
	if(Sys.info()["sysname"] == "Windows") degsym <- "\xb0 "
	else degsym<-"° "
	lon_lab <- paste(sprintf("%03d", degLo), degsym, sprintf("%02d", minLo), "' ",
	paste(sprintf("%02d", trunc(secLo)), substr(sprintf("%.2f", frac(secLo)), 3,4), sep = '.'),'" ',sLon, sep = '')
	lat_lab <- paste(sprintf("%03d", degLa), degsym, sprintf("%02d", minLa), "' ",
	paste(sprintf("%02d", trunc(secLa)), substr(sprintf("%.2f", frac(secLa)), 3,4), sep = '.'),'" ',sLat, sep = '')
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
	if(length(x) > 1) 	xlim <- range(x) + factor * diff(range(x)) * c(-1,1)
	else if(length(x) == 1 & x != 0) xlim <- x + factor * x * c(-1,1)
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


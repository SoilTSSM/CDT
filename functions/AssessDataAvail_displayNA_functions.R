
assess.plotNASummary <- function(dates, xseries, jstn, period){
	years <- as.numeric(substr(dates, 1, 4))
	yNonNA <- tapply(xseries, list(years), FUN = function(x) length(which(complete.cases(x))))
	taxis <- as.numeric(names(yNonNA))
	at_tick <- seq_len(length(taxis) + 1)
	at_lab <- seq_along(taxis) - 0.5
	yval <- as.vector(yNonNA)
	at_val <- at_lab[yval > 0]
	yval <- yval[yval > 0]
	if(period == 'daily') ylim <- c(0, 400)
	if(period == 'dekadal') ylim <- c(0, 40)
	if(period == 'monthly') ylim <- c(0, 14)

	opar <- par(mar = c(3.5, 4, 3, 2))
	barplot(yNonNA, space = 0, ylab = 'Number of Non-Missing data/Year', ylim = ylim, col = 'cyan', xaxt = 'n', yaxt = 'n', main = jstn)
	axis(side = 2)
	axis(side = 1, at = at_tick - 1, labels = FALSE)
	axis(side = 1, at = at_lab , tick = FALSE, labels = taxis)
	abline(h = axTicks(2), col = "lightgray", lty = "dotted")
	box()
	plt <- par("plt")
	usr <- par("usr")
	par(opar)
	return(list(par = c(plt, usr), years = taxis))
}

######################################################################################################

assess.plotAllNASummary <- function(donne, dates, period){
	if(period == 'daily'){
		xdates <- as.Date(dates, format = '%Y%m%d')
	}else if(period == 'dekadal'){
		xdates <- as.Date(paste(substr(dates, 1, 6), ifelse(substr(dates, 7, 7) == "1", "05",
					ifelse(substr(dates, 7, 7) == "2", "15", "25")), sep = ''), format = '%Y%m%d')
	}else if(period == 'monthly'){
		xdates <- as.Date(paste(dates, 16, sep = ''), format = '%Y%m%d')
	}

	stnTworking <- function(x){
		nonNA <- which(complete.cases(x))
		c(min(nonNA), max(nonNA))
	}

	nbstn <- ncol(donne)
	nlstn <- nrow(donne)
	vNonNA <- nbstn - base::rowSums(is.na(donne))

	tstn <- t(apply(donne, 2, stnTworking))
	mat <- matrix(0, nrow = nlstn, ncol = nbstn)
	for(j in 1:nbstn) mat[tstn[j, 1]:tstn[j, 2], j] <- 1
	sworking <- base::rowSums(mat)

	layout(matrix(1:2, ncol = 1), widths = 1, heights = c(0.1, 1), respect = FALSE)
	op <- par(mar = c(0, 0, 0, 0))
	plot.new()
	legend("center", "groups", legend = c('Active Stations', 'Reported Data'), fill = c('cyan1', 'pink'), horiz = TRUE)
	par(op)
	op <- par(mar = c(3.5, 4, 0, 2))
	plot(xdates, sworking, xlab = '', ylab = 'Number of stations', ylim = c(0, max(sworking, na.rm = TRUE)+1), type = 'n')
	polygon(c(rev(xdates), xdates), c(rev(vNonNA), rep(0, nlstn)), col = 'pink', border = NA)
	polygon(c(rev(xdates), xdates), c(rev(sworking), vNonNA), col = 'cyan1', border = NA)
	abline(v = pretty(extendrange(xdates)), h = pretty(extendrange(c(0, sworking))), col = 'lightgrey', lty = "dotted")
	box()
	plt <- par("plt")
	usr <- par("usr")
	par(op)
	return(list(par = c(plt, usr)))
}

######################################################################################################

assess.plotAnnualAverage <- function(donne){
	nadon <- base::rowSums(!is.na(donne$data))

	year <- as.numeric(substr(donne$dates, 1, 4))
	meanyear <- tapply(nadon, year, mean)
	minyear <- tapply(nadon, year, min)
	maxyear <- tapply(nadon, year, max)
	xyear <- as.numeric(names(meanyear))
	ylim <- c(0, max(pretty(maxyear)))
	xlim <- range(xyear)

	plot(xyear, meanyear, type = 'n', axes = FALSE, xlim = xlim, ylim = ylim,
		xlab = '', ylab = "Number of stations", main = "Average number of stations reporting each year")

	minTck <- axTicks(2)
	minTck <- minTck[-length(minTck)] + diff(minTck)/2
	minTck <-c(min(axTicks(2))-diff(minTck)[1]/2, minTck, max(axTicks(2))+diff(minTck)[1]/2)
	abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(h = minTck, col = "lightgray", lty = "dotted")

	ixx <- which(xyear%%5 == 0)
	xxax <- if(length(ixx) < 5) axTicks(1) else xyear[ixx]

	lines(xyear, meanyear, type = "h", lwd = 10, lend = "butt", col = 4)
	points(xyear, meanyear, pch = 20, col = 2, cex = 0.5)
	arrows(xyear, minyear, xyear, maxyear, angle = 90, code = 3, length = 0.03, col = 2)

	axis(1, labels = xxax, at = xxax)
	axis(2, at = axTicks(2), las = 1)
	box(bty = 'l')
	box(bty = '7', col = 'gray')
}

######################################################################################################

assess.funCorDist <- function(donne, latlon){
	lon <- latlon[, 1]
	lat <- latlon[, 2]
	#######

	test_equal <- function(x){
		x <- x[!is.na(x)]
		all(x == x[1])
	}
	###
	summaryDistCor <- function(x, y){
		nbreaks <- function(n) round(exp(0.4607)*n^0.6003)
		o <- order(x)
		x <- x[o]
		y <- y[o]
		#
		nbrk <- nbreaks(length(x))
		ibrk <- seq(0, length(x), nbrk)
		if(length(ibrk[length(ibrk)]:length(x)) < nbrk/4){
			breaks <- c(0, x[ibrk[-c(1, length(ibrk))]], max(x))
		}else{
			breaks <- c(0, x[ibrk[-1]], max(x))
		}

		xquant <- tapply(y, list(cut(x, breaks = breaks)), quantile, prob = c(0.05, 0.5, 0.95))
		intrv <- names(xquant)
		intrv <- gsub('\\(|\\]|\\[', '', intrv)
		Intrv <- matrix(as.numeric(do.call('rbind', strsplit(intrv, ','))), ncol = 2)
		mids <- (Intrv[, 2]+Intrv[, 1])/2
		xquant <- do.call('rbind', xquant)
		return(unname(cbind(mids, xquant)))
	}
	###
	fDistCor <- function(don, dist){
		corm <- suppressWarnings(cor(don, use = 'pairwise.complete.obs'))
		corm[lower.tri(corm, diag = TRUE)] <- NA
		dist[lower.tri(dist, diag = TRUE)] <- NA
		idn <- which(!is.na(corm))
		xcor <- corm[idn]
		xdst <- dist[idn]
		return(cbind(xdst, xcor))
	}
	###################

	id0 <- apply(donne, 2, test_equal)
	##
	infoIDen <- NULL
	if(sum(id0) > 0){
		val <- sapply(which(id0), function(j){
			x <- donne[, j]
			x <- x[!is.na(x)]
			x[1]
		})

		infoIDen <- cbind(lon[id0], lat[id0], val)
		lon <- lon[!id0]
		lat <- lat[!id0]
		donne <- donne[, !id0]
	}
	##
	coord <- matrix(c(lon, lat), ncol = 2)
	dists <- rdist.earth(coord, miles = FALSE)
	###
	xdist <- fDistCor(donne, dists)
	x <- xdist[, 1]
	y <- xdist[, 2]
	###

	xysumm <- summaryDistCor(x, y)
	###
	loess <- loess.smooth(x, y)
	###
	op <- par(mar = c(4, 4, 2, 2))
	plot(x, y, xlab = "Distance (km)", ylab = "Correlation", type = 'n', xlim = c(0, max(x, na.rm = TRUE)),
		ylim = c(min(xysumm[, 2], na.rm = TRUE), ifelse(max(xysumm[, 4], na.rm = TRUE) < 0.9, max(xysumm[, 4], na.rm = TRUE)+0.1, 1)))
	polygon(c(rev(xysumm[, 1]), xysumm[, 1]), c(rev(xysumm[, 4]), xysumm[, 2]), col = "darkslategray1", border = NA)
	lines(xysumm[, 1], xysumm[, 3], col = 'blue', lwd = 2)
	lines(loess$x, loess$y, lwd = 2, col = 'red')
	abline(v = axTicks(1), h = axTicks(2), col = 'lightgrey', lty = "dotted")
	legend(x = 'topright', legend = c("5/95th Percentile", "Median", 'Loess smooth'),
			col = c('darkslategray1', 'blue', 'red'), lty = c(0, 1, 1), lwd = c(0, 2, 2),
			pch = c(22, NA, NA), pt.bg = c('darkslategray1', NA, NA), pt.cex = 2, bg = 'gray97')
	par(op)
}

######################################################################################################

DisplayStnNASum <- function(parent, jstn, donne, period, notebookTab){
	dates <- donne$dates
	IdStn <- donne$id
	donne <- donne$data
	ijx <- which(IdStn == jstn)
	xseries <- donne[, ijx]

	########PLOT
	yearAxis <- NULL
	varplot <- c("parPlotSize1", "parPlotSize2", "parPlotSize3", "parPlotSize4",
				 "usrCoords1", "usrCoords2", "usrCoords3", "usrCoords4")
	parPltCrd <- setNames(lapply(varplot, function(x) assign(x, tclVar(), env = parent.frame())), varplot)

	plotIt <- function(){
		op <- par(bg = "white")
		pltusr <- assess.plotNASummary(dates, xseries, jstn, period)
		par(op)
		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
		yearAxis <<- pltusr$years
	}

	#########
	onglet <- imageNotebookTab_open(parent, notebookTab, paste(jstn, 'Miss.Summary', sep = '-'), AllOpenTabType, AllOpenTabData)
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	tkbind(img, "<Motion>", function(W, x, y){
		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		ipos <- ceiling(xyMouse$x)
		yearAxisRange <- ipos < 1 | ipos > length(yearAxis) | xyMouse$inout

		frxcoord <- ifelse(yearAxisRange, '', yearAxis[ipos])
		frycoord <- ifelse(xyMouse$inout, '', round(xyMouse$y))

		tclvalue(xpcoord) <- frxcoord
		tclvalue(ypcoord) <- frycoord
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}

######################################################################################################

DisplayAllStnNASum <- function(parent, donne, period, notebookTab){
	dates <- donne$dates
	donne <- donne$data
	nna <- base::colSums(is.na(donne))
	# exclude station without data (all missing values)
	donne <- donne[, nna != nrow(donne)]

	########PLOT
	varplot <- c("parPlotSize1", "parPlotSize2", "parPlotSize3", "parPlotSize4",
				 "usrCoords1", "usrCoords2", "usrCoords3", "usrCoords4")
	parPltCrd <- setNames(lapply(varplot, function(x) assign(x, tclVar(), env = parent.frame())), varplot)

	plotIt <- function(){
		op <- par(bg = "white")
		pltusr <- assess.plotAllNASummary(donne, dates, period)
		par(op)
		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
	}

	#########
	onglet <- imageNotebookTab_open(parent, notebookTab, 'Missing data Summary', AllOpenTabType, AllOpenTabData)
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	#########
	tkbind(img, "<Motion>", function(W, x, y){
		xyMouse <- mouseMouvment(W, x, y, parPltCrd, ydiv = c(0, 1/(0.1+1)))

		if(period == 'daily') labdates <- format(as.Date(xyMouse$x, origin = '1970-1-1'), '%d-%b-%Y')
		if(period == 'dekadal'){
			dtdk <- as.Date(xyMouse$x, origin = '1970-1-1')
			dek <- ifelse(as.numeric(format(dtdk, '%d')) <= 10, 1, ifelse(as.numeric(format(dtdk, '%d')) > 20, 3, 2))
			moyr <- format(dtdk, '%b-%Y')
			labdates <- paste(dek, moyr, sep = '-')
		}
		if(period == 'monthly') labdates <- format(as.Date(xyMouse$x, origin = '1970-1-1'),'%b-%Y')

		frxcoord <- ifelse(xyMouse$inout, '', labdates)
		frycoord <- ifelse(xyMouse$inout, '', round(xyMouse$y))

		tclvalue(xpcoord) <- frxcoord
		tclvalue(ypcoord) <- frycoord
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}

######################################################################################################

DisplayDistCorr <- function(parent, donne, period, notebookTab){
	latlon <- cbind(donne$lon, donne$lat)
	donne <- donne$data

	nna <- base::colSums(is.na(donne))
	latlon <- latlon[nna != nrow(donne), ]
	donne <- donne[, nna != nrow(donne)]

	########PLOT
	plotIt <- function(){
		op <- par(bg = "white")
		assess.funCorDist(donne, latlon)
		par(op)
	}

	#########
	onglet <- imageNotebookTab_open(parent, notebookTab, 'Distance_Corrorelation', AllOpenTabType, AllOpenTabData)
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))
	return(list(onglet, img))
}


######################################################################################################

DisplayAnnualAvg <- function(parent, donne, notebookTab){
	plotIt <- function(){
		op <- par(bg = "white")
		assess.plotAnnualAverage(donne)
		par(op)
	}

	#########
	onglet <- imageNotebookTab_open(parent, notebookTab, 'Annual_Average', AllOpenTabType, AllOpenTabData)
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))
	return(list(onglet, img))
}



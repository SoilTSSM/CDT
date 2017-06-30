
picsa.plot.daily <- function(dates, prec, thres.rain = 1, axis.font = 1){
	vtimes <- table.annuel()
	vmmdd <- paste0(str_pad(vtimes[, 2], 2, pad = '0'), str_pad(vtimes[, 1], 2, pad = '0'))
	years <- as.numeric(substr(dates, 1, 4))
	mmdd <- substr(dates, 5, 8)
	mmdd[mmdd == '0229'] <- '0228'
	yday <- match(mmdd, vmmdd)
	dfplot <- data.frame(yy = years, day = yday)
	rnor <- prec > thres.rain

	layout(matrix(1:2, ncol = 1), widths = 1, heights = c(0.9, 0.1), respect = FALSE)
	op <- par(mar = c(3.1, 3.1, 2.1, 2.1))
	plot(dfplot$yy, dfplot$day, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', ylim = c(0, 370))
	abline(h = axTicks(2), col = "lightgray", lty = "dotted")
	abline(v = axTicks(1), col = "lightgray", lty = "dotted")
	points(dfplot$yy[!rnor], dfplot$day[!rnor], pch = 15, col = 7, cex = 0.4)
	points(dfplot$yy[rnor], dfplot$day[rnor], pch = 8, col = 4, cex = 0.3)
	axis(1, at = axTicks(1), font = axis.font)
	mtext('Year', side = 1, line = 2)
	axis(2, at = axTicks(2), font = axis.font)
	mtext('Day of Year', side = 2, line = 2)
	mtext(EnvPICSAplot$location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6) 
	legend(x = 'topright', legend = c("Rain", "Dry", 'NA'), bty = "n", fill = c(4, 7, NA), horiz = TRUE, cex = 0.8, inset = -0.01)
	par(op)

	op <- par(mar = c(1, 3.1, 0, 2.1))
	plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
	bbx <- par("usr")
	rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
	text(1, 1, "Rain Present", cex = 0.9, font = 2)
	par(op)
}

picsa.plot.bar <- function(x, y, origindate = NULL, sub = NULL, xlab = '', ylab = '',
						title = '', barcol = "darkblue", axis.font = 1, start.zero = FALSE)
{
	ylim <- if(start.zero) c(0, max(pretty(y))) else range(y, na.rm = TRUE)

	layout(matrix(1:2, ncol = 1), widths = 1, heights = c(0.9, 0.1), respect = FALSE)
	op <- par(mar = c(3.1, 5.1, 2.1, 2.1))
	plot(x, y, type = 'n', xlab = '', ylab = '', axes = FALSE, ylim = ylim)

	minTck <- axTicks(2)
	minTck <- minTck[-length(minTck)] + diff(minTck)/2
	minTck <-c(min(axTicks(2))-diff(minTck)[1]/2, minTck, max(axTicks(2))+diff(minTck)[1]/2)
	abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(h = minTck, col = "lightgray", lty = "dotted")

	lines(x, y, type = "h", lwd = 10, lend = "butt", col = barcol)

	axis(1, at = axTicks(1), font = axis.font)
	mtext(xlab, side = 1, line = 2)

	if(!is.null(origindate)){
		yaxlab <- format(as.Date(axTicks(2), origin = origindate), '%d %b')
		axis(2, at = axTicks(2), labels = yaxlab, las = 2, font = axis.font)
	}else axis(2, at = axTicks(2), las = 1, font = axis.font)
	line <- if(max(nchar(as.character(axTicks(2)))) > 2) 3 else 2
	if(!is.null(sub)){
		mtext(ylab, side = 2, line = line+1)
		mtext(sub, side = 2, line = line, font = 3, cex = 0.8)
	}else mtext(ylab, side = 2, line = line)

	box(bty = 'l')
	box(bty = '7', col = 'gray')
	mtext(EnvPICSAplot$location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6) 
	par(op)

	op <- par(mar = c(1, 5.1, 0, 2.1))
	plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
	bbx <- par("usr")
	rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
	text(1, 1, title, cex = 0.9, font = 2)
	par(op)
}

picsa.plot.line <- function(x, y, origindate = NULL, sub = NULL, xlab = '', ylab = '',
							title = '', mean = FALSE, tercile = FALSE, linear = FALSE,
							axis.font = 1, start.zero = FALSE,
							col = list(line = "red", points = "blue"),
							col.add = list(mean = "black", tercile1 = "green", tercile2 = "blue", linear = "purple3"))
{
	ylim <- if(start.zero) c(0, max(pretty(y))) else range(y, na.rm = TRUE)

	layout(matrix(1:3, ncol = 1), widths = 1, heights = c(0.8, 0.1, 0.1), respect = FALSE)
	op <- par(mar = c(3.1, 6.5, 2.1, 2.1))

	plot(x, y, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', ylim = ylim)
	abline(h = axTicks(2), col = "lightgray", lty = "dotted")
	abline(v = axTicks(1), col = "lightgray", lty = "dotted")

	if(!("line" %in% names(col))) col$line <- "red"
	if(!("points" %in% names(col))) col$points <- "blue"
	lines(x, y, type = 'o', col = col$line, lwd = 2, pch = 21, bg = col$points, cex = 1.4)

	collegend <- NULL
	txtlegend <- NULL
	if(mean){
		if(!("mean" %in% names(col.add))) col.add$mean <- "black"
		abline(h = mean(y, na.rm = TRUE), col = col.add$mean, lwd = 2)
		collegend <- c(collegend, col.add$mean)
		txtlegend <- c(txtlegend, "Average")
	}
	if(linear){
		if(!("linear" %in% names(col.add))) col.add$linear <- "purple3"
		abline(lm(y~x), col = col.add$linear, lwd = 2)
		collegend <- c(collegend, col.add$linear)
		txtlegend <- c(txtlegend, "Trend line")
	}
	if(tercile){
		if(!("tercile1" %in% names(col.add))) col.add$tercile1 <- "green"
		if(!("tercile2" %in% names(col.add))) col.add$tercile2 <- "blue"
		terc <- quantile8(y, probs = c(0.33333, 0.66667))
		abline(h = terc[1], col = col.add$tercile1, lwd = 2)
		abline(h = terc[2], col =  col.add$tercile2, lwd = 2)
		collegend <- c(collegend,  col.add$tercile1,  col.add$tercile2)
		txtlegend <- c(txtlegend, "Tercile 0.33333", "Tercile 0.66666")
	}

	axis(1, at = axTicks(1), font = axis.font, cex.axis = 1.5)
	mtext(xlab, side = 1, line = 2)

	if(!is.null(origindate)){
		yaxlab <- format(as.Date(axTicks(2), origin = origindate), '%d %b')
		axis(2, at = axTicks(2), labels = yaxlab, las = 2, font = axis.font, cex.axis = 1.5)
	}else axis(2, at = axTicks(2), font = axis.font, las = 1, cex.axis = 1.5)

	line <- if(max(nchar(as.character(axTicks(2)))) > 2) 4 else 3
	if(!is.null(sub)){
		mtext(ylab, side = 2, line = line+1)
		mtext(sub, side = 2, line = line, font = 3, cex = 0.8)
	}else mtext(ylab, side = 2, line = line)
	mtext(EnvPICSAplot$location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6) 
	par(op)

	op <- par(mar = c(0, 6.5, 0, 2.1))
	if(mean | tercile | linear){
		plot.new()
		legend("center", "groups", legend = txtlegend, col = collegend, lwd = 3, horiz = TRUE, cex = 1.2)
	}
	par(op)

	op <- par(mar = c(1, 6.5, 0, 2.1))
	plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
	bbx <- par("usr")
	rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
	text(1, 1, title, cex = 1.5, font = 2)
	par(op)
}

picsa.plot.TxTn <- function(x, tmax, tmin, axis.font = 1){
	ylim <- range(c(pretty(tmin), pretty(tmax)))

	layout(matrix(1:2, ncol = 1), widths = 1, heights = c(0.9, 0.1), respect = FALSE)
	op <- par(mar = c(3, 4, 2, 2))
	plot(x, tmin, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = 'Temperature (°C)', ylim = ylim)

	abline(h = axTicks(2), col = "lightgray", lty = "dotted")
	abline(v = axTicks(1), col = "lightgray", lty = "dotted")

	mtext('Year', side = 1, line = 2)
	axis(1, at = axTicks(1), font = axis.font)
	axis(2, at = axTicks(2), las = 1, font = axis.font)

	lines(x, tmin, col = 'blue', lwd = 2)
	lines(x, tmax, col = 'red', lwd = 2)

	abline(lm(tmax~x), lwd = 2)
	abline(lm(tmin~x), lwd = 2)
	mtext(EnvPICSAplot$location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6) 
	par(op)

	op <- par(mar = c(0, 4, 0, 2))
	plot.new()
	legend("top", "groups", legend = c('Tmax', 'Tmin', 'Trend line'), col = c('red', 'blue', 'black'), lwd = 3, lty = 1, horiz = TRUE)
	par(op)
}

picsa.plot.proba <- function(dat, origindate = NULL, sub = NULL, xlab = '', title = '', axis.font = 1,
							theoretical = FALSE, distr = c("norm", "snorm", "lnorm", "gamma", "weibull"), gof.c = "ad",
							col = list(line = "blue", points = "lightblue", prob = "black"))
{
	dat <- as.numeric(dat)
	dat <- dat[!is.na(dat)]

	xlim <- if(length(dat) > 1) range(pretty(dat)) else c(0, 1)
	ylim <- c(0, 100)

	layout(matrix(1:2, ncol = 1), widths = 1, heights = c(0.9, 0.1), respect = FALSE)
	op <- par(mar = c(4.1, 5.1, 2.1, 2.1))
	plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlim = xlim, ylim = ylim, xlab = '', ylab = '')

	xminTck <- axTicks(1)
	xminTck <- xminTck[-length(xminTck)] + diff(xminTck)/2
	xminTck <-c(min(axTicks(2))-diff(xminTck)[1]/2, xminTck, max(axTicks(2))+diff(xminTck)[1]/2)
	yminTck <- axTicks(2)
	yminTck <- yminTck[-length(yminTck)] + diff(yminTck)/2
	yminTck <-c(min(axTicks(2))-diff(yminTck)[1]/2, yminTck, max(axTicks(2))+diff(yminTck)[1]/2)
	abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(h = yminTck, col = "lightgray", lty = "dotted")
	abline(v = axTicks(1), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(v = xminTck, col = "lightgray", lty = "dotted")

	if(!is.null(origindate)){
		xaxlab <- format(as.Date(axTicks(1), origin = origindate), '%d %b')
		axis(1, at = axTicks(1), labels = xaxlab, font = axis.font)
	}else{
		axis(1, at = axTicks(1), font = axis.font)
	}	
	mtext(xlab, side = 1, line = 2)
	if(!is.null(sub)) mtext(sub, side = 1, line = 3, font = 3, cex = 0.8)

	yaxlab <- paste0(axTicks(2), "%")
	axis(2, at = axTicks(2), labels = yaxlab, las = 2, font = axis.font)
	mtext("Probability of Exceeding", side = 2, line = 3)

	if(length(dat) > 1){
		fn <- ecdf(dat)
		x <- sort(dat)
		y <- 100*(1-fn(x))

		if(!("line" %in% names(col))) col$line <- "blue"
		if(!("points" %in% names(col))) col$points <- "lightblue"
		lines(x, y, type = 'o', col = col$line, lwd = 2, pch = 21, bg = col$points, cex = 0.8)

		####
		if(theoretical){
			fit.distrs <- fit.distributions(x, distr)
			if(!is.null(fit.distrs)){
				gof <- gofstat(fit.distrs)
				imin <- which.min(gof[[gof.c]])
				plotTheo <- TRUE
			}else plotTheo <- FALSE

			if(plotTheo){
				selected.distr <- fit.distrs[[imin]]$distname
				selected.pars <- as.list(fit.distrs[[imin]]$estimate)
				pdists <- function(x){
					foo <- match.fun(paste0("p", selected.distr))
					do.call(foo, c(list(q = x), selected.pars))
				}
				if(!("prob" %in% names(col))) col$prob <- "red"
				curve(100 * (1 - pdists(x)), from = xlim[1], to = xlim[2], lwd = 2, add = TRUE, col = col$prob)
				legend("topright", 
					c(paste0("distr: ", selected.distr), sapply(seq_along(selected.pars),
							function(j) paste0(names(selected.pars)[j], ": ", round(selected.pars[[j]], 5)))),
					box.lwd = 0, box.col = "gray97", bg = "gray98", cex = 0.6)
			}
		}
	}
	mtext(EnvPICSAplot$location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6) 
	par(op)

	op <- par(mar = c(1, 5.1, 0, 2.1))
	plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
	bbx <- par("usr")
	rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
	text(1, 1, title, cex = 0.9, font = 2)
	par(op)
}

picsa.plot.bar.ENSO <- function(x, y, oni, origindate = NULL, sub = NULL, xlab = '', ylab = '', title = '',
							barcol = c("blue", "gray", "red"), axis.font = 1, start.zero = FALSE)
{
	ylim <- if(start.zero) c(0, max(pretty(y))) else range(y, na.rm = TRUE)

	layout(matrix(1:3, ncol = 1), widths = 1, heights = c(0.8, 0.1, 0.1), respect = FALSE)
	op <- par(mar = c(3.1, 6.5, 2.1, 2.1))
	plot(x, y, type = 'n', xlab = '', ylab = '', axes = FALSE, ylim = ylim)

	minTck <- axTicks(2)
	minTck <- minTck[-length(minTck)] + diff(minTck)/2
	minTck <-c(min(axTicks(2))-diff(minTck)[1]/2, minTck, max(axTicks(2))+diff(minTck)[1]/2)
	abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(h = minTck, col = "lightgray", lty = "dotted")

	lines(x, y, type = "h", lwd = 10, lend = "butt", col = barcol[oni])

	axis(1, at = axTicks(1), font = axis.font, cex.axis = 1.5)
	mtext(xlab, side = 1, line = 2)

	if(!is.null(origindate)){
		yaxlab <- format(as.Date(axTicks(2), origin = origindate), '%d %b')
		axis(2, at = axTicks(2), labels = yaxlab, las = 2, font = axis.font, cex.axis = 1.5)
	}else axis(2, at = axTicks(2), las = 1, font = axis.font, cex.axis = 1.5)
	line <- if(max(nchar(as.character(axTicks(2)))) > 2) 4 else 3
	if(!is.null(sub)){
		mtext(ylab, side = 2, line = line+1)
		mtext(sub, side = 2, line = line, font = 3, cex = 0.8)
	}else mtext(ylab, side = 2, line = line)

	box(bty = 'l')
	box(bty = '7', col = 'gray')
	mtext(EnvPICSAplot$location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6) 
	par(op)

	op <- par(mar = c(0, 6.5, 0, 2.1))
	plot.new()
	legend("center", "groups", legend = c('La Niña', 'Neutral', 'El Niño'), fill = barcol, horiz = TRUE, cex = 1.2)
	par(op)

	op <- par(mar = c(1, 6.5, 0, 2.1))
	plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
	bbx <- par("usr")
	rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
	text(1, 1, title, cex = 1.5, font = 2)
	par(op)
}

picsa.plot.line.ENSO <- function(x, y, oni, origindate = NULL, sub = NULL, xlab = '', ylab = '',
							title = title, mean = FALSE, tercile = FALSE, linear = FALSE,
							axis.font = 1, start.zero = FALSE,
							col = list(line = "black", points = c("blue", "gray", "red")),
							col.add = list(mean = "darkblue", tercile1 = "chartreuse4", tercile2 = "darkgoldenrod4", linear = "purple3"))
{
	ylim <- if(start.zero) c(0, max(pretty(y))) else range(y, na.rm = TRUE)

	layout(matrix(1:3, ncol = 1), widths = 1, heights = c(0.8, 0.15, 0.1), respect = FALSE)
	op <- par(mar = c(3.1, 6.5, 2.1, 2.1))

	plot(x, y, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', ylim = ylim)
	abline(h = axTicks(2), col = "lightgray", lty = "dotted")
	abline(v = axTicks(1), col = "lightgray", lty = "dotted")

	if(!("line" %in% names(col))) col$line <- "black"
	if(!("points" %in% names(col))) col$points <- c("blue", "gray", "red")
	lines(x, y, col = col$line, lwd = 2)
	points(x, y, pch = 21, col = 'gray30', bg = col$points[oni], cex = 2)

	collegend <- NULL
	txtlegend <- NULL
	if(mean){
		if(!("mean" %in% names(col.add))) col.add$mean <- "darkblue"
		abline(h = mean(y, na.rm = TRUE), col = col.add$mean, lwd = 2)
		collegend <- c(collegend, col.add$mean)
		txtlegend <- c(txtlegend, "Average")
	}
	if(linear){
		if(!("linear" %in% names(col.add))) col.add$linear <- "purple3"
		abline(lm(y~x), col = col.add$linear, lwd = 2)
		collegend <- c(collegend, col.add$linear)
		txtlegend <- c(txtlegend, "Trend line")
	}
	if(tercile){
		if(!("tercile1" %in% names(col.add))) col.add$tercile1 <- "chartreuse4"
		if(!("tercile2" %in% names(col.add))) col.add$tercile2 <- "darkgoldenrod4"
		terc <- quantile8(y, probs = c(0.33333, 0.66667))
		abline(h = terc[1], col = col.add$tercile1, lwd = 2)
		abline(h = terc[2], col =  col.add$tercile2, lwd = 2)
		collegend <- c(collegend,  col.add$tercile1,  col.add$tercile2)
		txtlegend <- c(txtlegend, "Tercile 0.33333", "Tercile 0.66666")
	}

	axis(1, at = axTicks(1), font = axis.font, cex.axis = 1.5)
	mtext(xlab, side = 1, line = 2)

	if(!is.null(origindate)){
		yaxlab <- format(as.Date(axTicks(2), origin = origindate), '%d %b')
		axis(2, at = axTicks(2), labels = yaxlab, las = 2, font = axis.font, cex.axis = 1.5)
	}else axis(2, at = axTicks(2), font = axis.font, las = 1, cex.axis = 1.5)

	line <- if(max(nchar(as.character(axTicks(2)))) > 2) 4 else 3
	if(!is.null(sub)){
		mtext(ylab, side = 2, line = line+1)
		mtext(sub, side = 2, line = line, font = 3, cex = 0.8)
	}else mtext(ylab, side = 2, line = line)
	mtext(EnvPICSAplot$location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6) 
	par(op)

	nino <- c('La Niña', 'Neutral', 'El Niño')
	txtlegend <- if(mean | tercile | linear) c(nino, txtlegend) else nino
	collegend <- if(mean | tercile | linear) c(rep(col$line, 3), collegend) else rep(col$line, 3)

	op <- par(mar = c(0, 6.5, 0, 2.1))
	plot.new()
	legend("center", "groups", legend = txtlegend, col = collegend, pch = c(rep(21, 3), rep(NA, 4)),
			pt.bg = c(col$points, rep(NA, 4)), pt.cex = c(rep(2, 3), rep(NA, 4)),
			pt.lwd = c(rep(1, 3), rep(NA, 4)), lwd = 3, ncol = 3, cex = 1.2)
	par(op)

	op <- par(mar = c(1, 6.5, 0, 2.1))
	plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
	bbx <- par("usr")
	rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
	text(1, 1, title, cex = 1.5, font = 2)
	par(op)
}

picsa.plot.proba.ENSO <- function(dat, oni, origindate = NULL, sub = NULL, xlab = '',
									title = '', axis.font = 1,
									col.all = list(line = "black", points = "lightgray"),
									col.nino = list(line = "red", points = "lightpink"),
									col.nina = list(line = "blue", points = "lightblue"),
									col.neutre = list(line = "gray", points = "lightgray"))
{
	dat <- as.numeric(dat)
	dat <- dat[!is.na(dat)]

	xlim <- if(length(dat) > 1) range(pretty(dat)) else c(0, 1)
	ylim <- c(0, 100)

	layout(matrix(1:3, ncol = 1), widths = 1, heights = c(0.8, 0.1, 0.1), respect = FALSE)
	op <- par(mar = c(4.5, 6.0, 2.1, 2.1))
	plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlim = xlim, ylim = ylim, xlab = '', ylab = '')

	xminTck <- axTicks(1)
	xminTck <- xminTck[-length(xminTck)] + diff(xminTck)/2
	xminTck <-c(min(axTicks(2))-diff(xminTck)[1]/2, xminTck, max(axTicks(2))+diff(xminTck)[1]/2)
	yminTck <- axTicks(2)
	yminTck <- yminTck[-length(yminTck)] + diff(yminTck)/2
	yminTck <-c(min(axTicks(2))-diff(yminTck)[1]/2, yminTck, max(axTicks(2))+diff(yminTck)[1]/2)
	abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(h = yminTck, col = "lightgray", lty = "dotted")
	abline(v = axTicks(1), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(v = xminTck, col = "lightgray", lty = "dotted")

	if(!is.null(origindate)){
		xaxlab <- format(as.Date(axTicks(1), origin = origindate), '%d %b')
		axis(1, at = axTicks(1), labels = xaxlab, font = axis.font, cex.axis = 1.5)
		line <- 2
	}else{
		axis(1, at = axTicks(1), font = axis.font, cex.axis = 1.5)
		line <- 3
	}	
	mtext(xlab, side = 1, line = line)
	if(!is.null(sub)) mtext(sub, side = 1, line = line+1, font = 3, cex = 0.8)

	yaxlab <- paste0(axTicks(2), "%")
	axis(2, at = axTicks(2), labels = yaxlab, las = 2, font = axis.font, cex.axis = 1.5)
	mtext("Probability of Exceeding", side = 2, line = 4)

	if(length(dat) > 1){
		fn0 <- ecdf(dat)
		x0 <- sort(dat)
		y0 <- 100*(1-fn0(x0))

		x1 <- sort(dat[oni == 1])
		fn1 <- ecdf(x1)
		y1 <- 100*(1-fn1(x1))

		x2 <- sort(dat[oni == 2])
		fn2 <- ecdf(x2)
		y2 <- 100*(1-fn2(x2))

		x3 <- sort(dat[oni == 3])
		fn3 <- ecdf(x3)
		y3 <- 100*(1-fn3(x3))

		if(!("line" %in% names(col.all))) col.all$line <- "black"
		if(!("points" %in% names(col.all))) col.all$points <- "lightgray"
		if(!("line" %in% names(col.nina))) col.nina$line <- "blue"
		if(!("points" %in% names(col.nina))) col.nina$points <- "lightblue"
		if(!("line" %in% names(col.neutre))) col.neutre$line <- "gray"
		if(!("points" %in% names(col.neutre))) col.neutre$points <- "lightgray"
		if(!("line" %in% names(col.nino))) col.nino$line <- "red"
		if(!("points" %in% names(col.nino))) col.nino$points <- "lightpink"

		lines(x0, y0, type = 'o', col = col.all$line, lwd = 2, pch = 21, bg = col.all$points, cex = 1.4)
		lines(x1, y1, type = 'o', col = col.nina$line, lwd = 2, pch = 21, bg = col.nina$points, cex = 1.4)
		lines(x2, y2, type = 'o', col = col.neutre$line, lwd = 2, pch = 21, bg = col.neutre$points, cex = 1.4)
		lines(x3, y3, type = 'o', col = col.nino$line, lwd = 2, pch = 21, bg = col.nino$points, cex = 1.4)
	}
	mtext(EnvPICSAplot$location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6) 
	par(op)

	op <- par(mar = c(0, 6, 0, 2.1))
	plot.new()
	legend("center", "groups", legend = c('All years', 'La Niña', 'Neutral', 'El Niño'),
			col = c(col.all$line, col.nina$line, col.neutre$line, col.nino$line),
			lwd = 2, lty = 1, pch = 21, horiz = TRUE, cex = 1.4,
			pt.bg = c(col.all$points, col.nina$points, col.neutre$points, col.nino$points))
	par(op)

	op <- par(mar = c(1, 6, 0, 2.1))
	plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
	bbx <- par("usr")
	rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
	text(1, 1, title, cex = 1.5, font = 2)
	par(op)
}

###############




picsa.plot.daily <- function(dates, prec, thres.rain = 1){
	vtimes <- table.annuel()
	vmmdd <- paste0(str_pad(vtimes[, 2], 2, pad = '0'), str_pad(vtimes[, 1], 2, pad = '0'))
	years <- as.numeric(substr(dates, 1, 4))
	mmdd <- substr(dates, 5, 8)
	mmdd[mmdd == '0229'] <- '0228'
	yday <- match(mmdd, vmmdd)

	dfplot <- data.frame(yy = years, day = yday)
	rnor <- prec > thres.rain
	plot(dfplot$yy, dfplot$day, type = 'n', xaxt = 'n', yaxt = 'n', xlab = 'Year', ylab = 'Day of Year', ylim = c(0, 370))
	abline(h = axTicks(2), col = "lightgray", lty = "dotted")
	abline(v = axTicks(1), col = "lightgray", lty = "dotted")
	points(dfplot$yy[!rnor], dfplot$day[!rnor], pch = 15, col = 7, cex = 0.4)
	points(dfplot$yy[rnor], dfplot$day[rnor], pch = 8, col = 4, cex = 0.3)
	axis(1, at = axTicks(1), font = 2)
	axis(2, at = axTicks(2), font = 2)
	legend(x = 'topright', legend = c("Rain", "Dry", 'NA'), bty = "n", fill = c(4, 7, NA), horiz = TRUE, cex = 0.8, inset = -0.01)
}

plot.line.yaxdate <- function(x, y, origindate = NULL, xlab = '', ylab = '',
							linecol = 'red', pointcol = "blue",
							mean = FALSE, tercile = FALSE, linear = FALSE){
	plot(x, y, type = 'n', xaxt = 'n', yaxt = 'n', xlab = xlab, ylab = ylab)
	abline(h = axTicks(2), col = "lightgray", lty = "dotted")
	abline(v = axTicks(1), col = "lightgray", lty = "dotted")
	if(!is.null(origindate)){
		yaxlab <- format(as.Date(axTicks(2), origin = origindate), '%d %b')
		axis(2, at = axTicks(2), labels = yaxlab, las = 2, font = 2)
	}
	collegend <- NULL
	txtlegend <- NULL
	if(mean){
		abline(h = mean(y, na.rm = TRUE), col = "black", lwd = 2)
		collegend <- "black"
		txtlegend <- "Average"
	}
	if(tercile){
		terc <- quantile(y, probs = c(0.33333, 0.66667), na.rm = TRUE)
		abline(h = terc[1], col = "green", lwd = 2)
		abline(h = terc[2], col = "blue", lwd = 2)
		collegend <- c(collegend, "green", "blue")
		txtlegend <- c(txtlegend, "Tercile 0.33333", "Tercile 0.66666")
	}
	if(linear){
		abline(lm(y~x), col = "purple3", lwd = 2)
		collegend <- c(collegend, "purple3")
		txtlegend <- c(txtlegend, "Linear")
	}
	lines(x, y, type = 'o', col = linecol, lwd = 2, pch = 21, bg = pointcol, cex = 0.8)
	axis(1, at = axTicks(1), font = 2)
	if(mean | tercile | linear) legend("topright", txtlegend, col = collegend, bty = "n", lwd = 2)
}


plot.line.yaxvalue <- function(x, y,  xlab = '', ylab = '',
							linecol = 'red', pointcol = "blue",
							mean = FALSE, tercile = FALSE, linear = FALSE){
	ylim <- c(0, max(pretty(y)))
	plot(x, y, type = 'n', xaxt = 'n', yaxt = 'n', xlab = xlab, ylab = ylab, ylim = ylim)
	abline(h = axTicks(2), col = "lightgray", lty = "dotted")
	abline(v = axTicks(1), col = "lightgray", lty = "dotted")
	collegend <- NULL
	txtlegend <- NULL
	if(mean){
		abline(h = mean(y, na.rm = TRUE), col = "black", lwd = 2)
		collegend <- "black"
		txtlegend <- "Average"
	}
	if(tercile){
		terc <- quantile(y, probs = c(0.33333, 0.66667), na.rm = TRUE)
		abline(h = terc[1], col = "green", lwd = 2)
		abline(h = terc[2], col = "blue", lwd = 2)
		collegend <- c(collegend, "green", "blue")
		txtlegend <- c(txtlegend, "Tercile 0.33333", "Tercile 0.66666")
	}
	if(linear){
		abline(lm(y~x), col = "purple3", lwd = 2)
		collegend <- c(collegend, "purple3")
		txtlegend <- c(txtlegend, "Linear")
	}
	lines(x, y, type = 'o', col = linecol, lwd = 2, pch = 21, bg = pointcol, cex = 0.8)
	axis(1, at = axTicks(1), font = 2)
	axis(2, at = axTicks(2), font = 2)

	if(mean | tercile | linear) legend("topright", txtlegend, col = collegend, bty = "n", lwd = 2)
}


plot.bar.yaxvalue <- function(x, y,  xlab = '', ylab = '', barcol = "darkblue"){
	ylim <- c(0, max(pretty(y)))
	plot(x, y, type = 'n', xlab = xlab, ylab = ylab, axes = FALSE, ylim = ylim)

	minTck <- axTicks(2)
	minTck <- minTck[-length(minTck)] + diff(minTck)/2
	minTck <-c(min(axTicks(2))-diff(minTck)[1]/2, minTck, max(axTicks(2))+diff(minTck)[1]/2)
	abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(h = minTck, col = "lightgray", lty = "dotted")

	lines(x, y, type = "h", lwd = 10, lend = "butt", col = barcol)
	axis(1, at = axTicks(1), font = 2)
	axis(2, at = axTicks(2), las = 1, font = 2)
	box(bty = 'l')
	box(bty = '7', col = 'gray')
}


plot.probaExeecdance <- function(dat, xlab = '', sub = NULL, linecol = 'blue', pointcol = "lightblue"){
	fn <- ecdf(dat)
	x <- sort(dat)
	y <- 100*(1-fn(x))

	xlim <- range(pretty(dat))
	ylim <- c(0, 100)
	plot(x, y, type = 'n', xaxt = 'n', yaxt = 'n', xlim = xlim, ylim = ylim, xlab = '', ylab = "Probability of Exceeding")

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

	lines(x, y, type = 'o', col = linecol, lwd = 2, pch = 21, bg = pointcol, cex = 0.8)
	axis(1, at = axTicks(1), font = 2)
	axis(2, at = axTicks(2), labels = paste0(axTicks(2), "%"), font = 2)
	mtext(xlab, side = 1, line = 2)
	if(!is.null(sub)) mtext(sub, side = 1, line = 3, font = 3, cex = 0.8)
}


plot.probaExeecdance.theor <- function(dat, xlab = '', linecol = 'blue', pointcol = "lightblue", probcol = "black"){
	fn <- ecdf(dat)
	x <- sort(dat)
	y <- 100*(1-fn(x))

	moy0 <- mean(x)
	sd0 <- sd(x)
	fit.norm <- fitdist(x, "norm", method = 'mle', start = list(mean = moy0, sd = sd0))

	xlim <- range(pretty(dat))
	ylim <- c(0, 100)
	plot(x, y, type = 'n', xaxt = 'n', yaxt = 'n', xlim = xlim, ylim = ylim, ylab = "Probability of Exceeding", xlab = xlab)

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

	lines(x, y, type = 'o', col = linecol, lwd = 2, pch = 21, bg = pointcol, cex = 0.8)
	curve(100*(1-pnorm(x, mean = fit.norm$estimate[1], sd = fit.norm$estimate[2])),
			from = xlim[1], to = xlim[2], lwd = 2, add = TRUE, col = probcol)

	axis(1, at = axTicks(1), font = 2)
	axis(2, at = axTicks(2), labels = paste0(axTicks(2), "%"), font = 2)
}

plot.line.dryspell <- function(x, y,  xlab = '', ylab = '',
							linecol = 'red', pointcol = "blue", sub = NULL,
							mean = FALSE, tercile = FALSE, linear = FALSE){
	ylim <- c(0, max(pretty(y)))
	plot(x, y, type = 'n', xaxt = 'n', yaxt = 'n', xlab = xlab, ylab = '', ylim = ylim)
	abline(h = axTicks(2), col = "lightgray", lty = "dotted")
	abline(v = axTicks(1), col = "lightgray", lty = "dotted")
	collegend <- NULL
	txtlegend <- NULL
	if(mean){
		abline(h = mean(y, na.rm = TRUE), col = "black", lwd = 2)
		collegend <- "black"
		txtlegend <- "Average"
	}
	if(tercile){
		terc <- quantile(y, probs = c(0.33333, 0.66667), na.rm = TRUE)
		abline(h = terc[1], col = "green", lwd = 2)
		abline(h = terc[2], col = "blue", lwd = 2)
		collegend <- c(collegend, "green", "blue")
		txtlegend <- c(txtlegend, "Tercile 0.33333", "Tercile 0.66666")
	}
	if(linear){
		abline(lm(y~x), col = "purple3", lwd = 2)
		collegend <- c(collegend, "purple3")
		txtlegend <- c(txtlegend, "Linear")
	}
	lines(x, y, type = 'o', col = linecol, lwd = 2, pch = 21, bg = pointcol, cex = 0.8)
	axis(1, at = axTicks(1), font = 2)
	axis(2, at = axTicks(2), font = 2)

	mtext(ylab, side = 2, line = 3)
	if(!is.null(sub)) mtext(sub, side = 2, line = 2, font = 3, cex = 0.8)
	if(mean | tercile | linear) legend("topright", txtlegend, col = collegend, bty = "n", lwd = 2)
}


plot.bar.dryspell <- function(x, y,  xlab = '', ylab = '', sub = NULL, barcol = "slateblue4"){
	ylim <- c(0, max(pretty(y)))
	plot(x, y, type = 'n', xlab = xlab, ylab = '', axes = FALSE, ylim = ylim)

	minTck <- axTicks(2)
	minTck <- minTck[-length(minTck)] + diff(minTck)/2
	minTck <-c(min(axTicks(2))-diff(minTck)[1]/2, minTck, max(axTicks(2))+diff(minTck)[1]/2)
	abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(h = minTck, col = "lightgray", lty = "dotted")

	lines(x, y, type = "h", lwd = 10, lend = "butt", col = barcol)
	axis(1, at = axTicks(1), font = 2)
	axis(2, at = axTicks(2), las = 1, font = 2)
	box(bty = 'l')
	box(bty = '7', col = 'gray')
	mtext(ylab, side = 2, line = 3)
	if(!is.null(sub)) mtext(sub, side = 2, line = 2, font = 3, cex = 0.8)
}


plot.tmax.tmin <- function(x, tmax, tmin){
	ylim <- range(c(pretty(tmin), pretty(tmax)))
	layout(matrix(1:2, ncol = 1), widths = 1, heights = c(1, 0.1), respect = FALSE)
	op <- par(mar = c(3, 4, 2, 2))
	plot(x, tmin, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = 'Temperature (°C)', ylim = ylim)
	mtext('Year', side = 1, line = 2)
	axis(1, at = axTicks(1), font = 2)
	axis(2, at = axTicks(2), las = 1, font = 2)
	abline(h = axTicks(2), col = "lightgray", lty = "dotted")
	abline(v = axTicks(1), col = "lightgray", lty = "dotted")
	lines(x, tmin, col = 4, lwd = 2)
	lines(x, tmax, col = 2, lwd = 2)
	abline(lm(tmax~x), lwd = 2)
	abline(lm(tmin~x), lwd = 2)
	par(op)
	op <- par(mar = c(0, 0, 0, 0))
	plot.new()
	legend("center", "groups", legend = c('Tmax', 'Tmin'), col = c('red', 'blue'), lwd = 2, lty = 1, horiz = TRUE)
	par(op)
}


plot.bar.ONI <- function(x, y,  xlab = '', ylab = '', colz = NA, col = c("blue", "gray", "red")){
	layout(matrix(1:2, ncol = 1), widths = 1, heights = c(0.9, 0.1), respect = FALSE)
	op <- par(mar = c(3, 4, 2, 2))
	ylim <- c(0, max(pretty(y)))
	plot(x, y, type = 'n', xlab = '', ylab = ylab, axes = FALSE, ylim = ylim)

	minTck <- axTicks(2)
	minTck <- minTck[-length(minTck)] + diff(minTck)/2
	minTck <-c(min(axTicks(2))-diff(minTck)[1]/2, minTck, max(axTicks(2))+diff(minTck)[1]/2)
	abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(h = minTck, col = "lightgray", lty = "dotted")

	lines(x, y, type = "h", lwd = 10, lend = "butt", col = col[colz])
	axis(1, at = axTicks(1), font = 2)
	axis(2, at = axTicks(2), las = 1, font = 2)
	mtext(xlab, side = 1, line = 2)
	box(bty = 'l')
	box(bty = '7', col = 'gray')
	par(op)

	op <- par(mar = c(0, 0, 0, 0))
	plot.new()
	legend("top", "groups", legend = c('La Niña', 'Neutral', 'El Niño'), fill = col, horiz = TRUE)
	par(op)
}


plot.line.ONI <- function(x, y,  xlab = '', ylab = '', colz = NA, col = c("blue", "gray", "red")){
	layout(matrix(1:2, ncol = 1), widths = 1, heights = c(0.9, 0.1), respect = FALSE)
	op <- par(mar = c(3, 4, 2, 2))
	ylim <- c(0, max(pretty(y)))
	plot(x, y, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = ylab, ylim = ylim)
	abline(v = axTicks(1), col = "lightgray", lty = "dotted")
	minTck <- axTicks(2)
	minTck <- minTck[-length(minTck)] + diff(minTck)/2
	minTck <-c(min(axTicks(2))-diff(minTck)[1]/2, minTck, max(axTicks(2))+diff(minTck)[1]/2)
	abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(h = minTck, col = "lightgray", lty = "dotted")

	lines(x, y, lwd = 2)
	points(x, y, pch = 21, col = 'gray30', bg = col[colz], cex = 1.4)

	axis(1, at = axTicks(1), font = 2)
	mtext(xlab, side = 1, line = 2)
	axis(2, at = axTicks(2), font = 2)
	par(op)

	op <- par(mar = c(0, 0, 0, 0))
	plot.new()
	legend("top", "groups", legend = c('La Niña', 'Neutral', 'El Niño'), fill = col, horiz = TRUE)
	par(op)
}


plot.probaExeecdance.ONI <- function(dat, oni, xlab = ''){
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

	xlim <- range(pretty(dat))
	ylim <- c(0, 100)

	layout(matrix(1:2, ncol = 1), widths = 1, heights = c(0.9, 0.1), respect = FALSE)
	op <- par(mar = c(3, 4, 2, 2))
	plot(x0, y0, type = 'n', xaxt = 'n', yaxt = 'n', xlim = xlim, ylim = ylim, xlab = '', ylab = "Probability of Exceeding")

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

	lines(x0, y0, type = 'o', col = "black", lwd = 2, pch = 21, bg = "lightgray", cex = 0.8)
	lines(x1, y1, type = 'o', col = "blue", lwd = 2, pch = 21, bg = "lightblue", cex = 0.8)
	lines(x2, y2, type = 'o', col = "gray", lwd = 2, pch = 21, bg = "lightgray", cex = 0.8)
	lines(x3, y3, type = 'o', col = "red", lwd = 2, pch = 21, bg = "lightpink", cex = 0.8)

	axis(1, at = axTicks(1), font = 2)
	axis(2, at = axTicks(2), labels = paste0(axTicks(2), "%"), font = 2)
	mtext(xlab, side = 1, line = 2)
	par(op)

	op <- par(mar = c(0, 0, 0, 0))
	plot.new()
	legend("top", "groups", legend = c('All years', 'La Niña', 'Neutral', 'El Niño'), col = c("black", "blue", "gray", "red"),
			lwd = 2, lty = 1, pch = 21, pt.bg = c("lightgray", "lightblue", "lightgray", "lightpink"), horiz = TRUE)
	par(op)
}


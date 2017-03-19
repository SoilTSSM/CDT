
plotAdjustedSeries <- function(adjDon, iselect){

	kolors <- c('black', 'red', 'blue')
	linetype <- c(1, 2, 3)
	textlenged <- c("Base series", "Adjusted by mean", "Adjusted by QM")
	xcol <- if(length(iselect) > 0) kolors[iselect] else "white"
	xlty <- if(length(iselect) > 0) linetype[iselect] else 0
	xtxt <- if(length(iselect) > 0) textlenged[iselect] else ""

	modates <- adjDon$DatesAdjmon
	mox1 <- adjDon$DataMon
	mox2 <- adjDon$DataAdjMon
	mox3 <- adjDon$DataAdjMon1

	modates <- as.Date(paste(modates, 16, sep = ''), format = '%Y%m%d')
	xlim <- c(min(modates), max(modates))
	vgrid <- seq(as.Date(paste(format(modates[1], '%Y'), 1, 1, sep = '-')),
				as.Date(paste(format(modates[length(mox1)], '%Y'), 12, 31, sep = '-'))+1, 'year')
	layout(matrix(1:2, ncol = 1), widths = 1, heights = c(0.125, 1), respect = FALSE)

	if(ReturnExecResults$period != 'monthly'){
		dkdates <- adjDon$DatesAdjDek
		dkx1 <- adjDon$DataDek
		dkx2 <- adjDon$DataAdjDek
		dkx3 <- adjDon$DataAdjDek1
		dkdates <- as.Date(paste(substr(dkdates, 1, 6), ifelse(substr(dkdates, 7, 7) == "1", "05",
					ifelse(substr(dkdates, 7, 7) == "2", "15", "25")), sep = ''), format = '%Y%m%d')
		xlim <- c(min(c(dkdates, modates)), max(c(dkdates, modates)))
		vgrid <- seq(as.Date(paste(format(dkdates[1], '%Y'), 1, 1, sep = '-')),
					as.Date(paste(format(dkdates[length(dkx1)], '%Y'), 12, 31, sep = '-'))+1, 'year')
		layout(matrix(1:3, ncol = 1), widths = 1, heights = c(0.125, 1, 1), respect = FALSE)

		if(ReturnExecResults$period == 'daily'){
			dydates <- adjDon$DatesAdjDly
			dyx1 <- adjDon$DataDly
			dyx2 <- adjDon$DataAdjDly
			dyx3 <- adjDon$DataAdjDly1
			dydates <- as.Date(dydates, format = '%Y%m%d')
			xlim <- c(min(c(dydates, dkdates, modates)), max(c(dydates, dkdates, modates)))
			vgrid <- seq(as.Date(paste(format(dydates[1], '%Y'), 1, 1, sep = '-')),
						as.Date(paste(format(dydates[length(dyx1)], '%Y'), 12, 31, sep = '-'))+1, 'year')
			layout(matrix(1:4, ncol = 1), widths = 1, heights = c(0.125, 1, 1, 1), respect = FALSE)
		}
	}

	op <- par(mar = c(0, 0, 0, 0))
	plot.new()
	legend("center", "groups", xtxt, lty = xlty, col = xcol, horiz = TRUE)
	par(op)

	if(ReturnExecResults$period == 'daily'){
		op <- par(mar = c(0, 4, 0, 2))
		plot(modates, mox1, type = 'n', xaxt = 'n', ylab = 'Monthly', xlim = xlim)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = vgrid, col = "lightgray", lty = "dotted")
		if(length(iselect) > 0) for(j in iselect) lines(modates, get(paste('mox', j, sep = '')), lty = linetype[j], col = kolors[j])
		par(op)

		op <- par(mar = c(0, 4, 0, 2))
		plot(dkdates, dkx1, type = 'n', xaxt = 'n', ylab = 'Dekadal', xlim = xlim)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = vgrid, col = "lightgray", lty = "dotted")
		if(length(iselect) > 0) for(j in iselect) lines(dkdates, get(paste('dkx', j, sep = '')), lty = linetype[j], col = kolors[j])
		par(op)

		op <- par(mar = c(3, 4, 0, 2))
		plot(dydates, dyx1, type = 'n', ylab = 'Daily', xlim = xlim)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = vgrid, col = "lightgray", lty = "dotted")
		if(length(iselect) > 0) for(j in iselect) lines(dydates, get(paste('dyx', j, sep = '')), lty = linetype[j], col = kolors[j])
		par(op)
	}

	if(ReturnExecResults$period == 'dekadal'){
		op <- par(mar = c(0, 4, 0, 2))
		plot(modates, mox1, type = 'n', xaxt = 'n', ylab = 'Monthly', xlim = xlim)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = vgrid, col = "lightgray", lty = "dotted")
		if(length(iselect) > 0) for(j in iselect) lines(modates, get(paste('mox', j, sep = '')), lty = linetype[j], col = kolors[j])
		par(op)

		op <- par(mar = c(3, 4, 0, 2))
		plot(dkdates, dkx1, type = 'n', ylab = 'Dekadal', xlim = xlim)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = vgrid, col = "lightgray", lty = "dotted")
		if(length(iselect) > 0) for(j in iselect) lines(dkdates, get(paste('dkx', j, sep = '')), lty = linetype[j], col = kolors[j])
		par(op)
	}

	if(ReturnExecResults$period == 'monthly'){
		op <- par(mar = c(3, 4, 0, 2))
		plot(modates, mox1, type = 'n', ylab = 'Monthly', xlim = xlim)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = vgrid, col = "lightgray", lty = "dotted")
		if(length(iselect) > 0) for(j in iselect) lines(modates, get(paste('mox', j, sep = '')), lty = linetype[j], col = kolors[j])
		par(op)
	}
}

####################################################################################

plotadjusted <- function(parent, iselect, noteHomAdjPlot){

	if(ReturnExecResults$action == 'homog'){
		plotIt <- function(){
			op1 <- par(bg = 'white')
			plotAdjustedSeries(adjDon, iselect)
			par(op1)
		}
	}else{
		InsertMessagesTxt(main.txt.out, 'There is no adjusted data yet', format = TRUE)
		return(NULL)
	}

	###################################################################
	titre <- paste(ReturnExecResults$station, 'Adjusted Series', sep = '-')
	onglet <- imageNotebookTab_open(parent, noteHomAdjPlot, titre, AllOpenTabType, AllOpenTabData)

	##########
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




plotOutliers0 <- function(jmo, outqcf, xdates, xdat){
	opar <- par(mar = c(3, 4, 3, 1))
	outdates <- as.character(outqcf$dates)
	if(GeneralParameters$action == 'qc.rain') outlq <- as.character(outqcf$upper.outlier)
	if(GeneralParameters$action == 'qc.temp') outlq <- as.character(outqcf$outlier.check)
	outdates1 <- outdates[!is.na(outlq)]
	moqc <- substr(outdates1, 5, 6)
	moval <- substr(xdates, 5, 6)
	idqc <- which(moqc == jmo)

	idm <- which(moval == jmo)
	datym <- xdates[idm]
	valm <- xdat[idm]

	if(GeneralParameters$action == 'qc.temp') txtnLab <- if(GeneralParameters$is.TX) 'maximum' else 'minimum'

	if(GeneralParameters$period == 'daily'){
		datym1st <- which(substr(datym, 7, 8) == '01')
		datymid <- which(substr(datym, 7, 8) == '15')
		if(GeneralParameters$action == 'qc.rain'){
			ylab <- 'Daily rainfall [mm]'
		}
		if(GeneralParameters$action == 'qc.temp'){
			if(Sys.info()["sysname"] == "Windows") ylab <- expression(paste("Daily", txtnLab, "temperature[ " * degree, 'C]'))
			else ylab <- paste('Daily', txtnLab, 'temperature [°C]')
		}
		xlabels <- format(as.Date(datym[datymid], format = '%Y%m%d'), '%Y')
	}
	if(GeneralParameters$period == 'dekadal'){
		datym1st <- which(substr(datym, 7, 7) == '1')
		datymid <- which(substr(datym, 7, 7) == '2')
		if(GeneralParameters$action == 'qc.rain'){
			ylab <- 'Dekadal rainfall [mm]'
		}
		if(GeneralParameters$action == 'qc.temp'){
			if(Sys.info()["sysname"] == "Windows") ylab <- expression(paste("Dekadal", txtnLab, "temperature[ " * degree, 'C]'))
			else ylab <- paste('Dekadal', txtnLab, 'temperature [°C]')
		}
		xlabels <- format(as.Date(datym[datymid], format = '%Y%m%d'), '%Y')
	}
	if(GeneralParameters$period == 'monthly'){
		datym1st <- 1:length(datym)
		datymid <- 1:length(datym)
		if(GeneralParameters$action == 'qc.rain'){
			ylab <- 'Monthly rainfall [mm]'
		}
		if(GeneralParameters$action == 'qc.temp'){
			if(Sys.info()["sysname"] == "Windows") ylab <- expression(paste("Monthly", txtnLab, "temperature[ " * degree, 'C]'))
			else ylab <- paste('Monthly', txtnLab, 'temperature [°C]')
		}
		xlabels <- format(as.Date(paste(datym, '15', sep = ''), format = '%Y%m%d'), '%Y')
	}

	if(sum(!is.na(valm)) == 0) plot(valm, type = 'n', xaxt = 'n', lwd = 2, ylab = ylab, xlab = '', ylim = c(0, 10))
	else plot(valm, type = 'n', xaxt = 'n', lwd = 2, ylab = ylab, xlab = '')
	abline(h = axTicks(2), col = "lightgray", lty = "dotted")
	abline(v = datym1st, col = "lightgray", lty = "dotted")
	lines(valm, type = 'h')

	if(length(idqc) > 0){
		datyq <- outdates1[idqc]
		imatch <- match(datyq, datym)
		points(imatch, valm[imatch], col = 2)
		lines(imatch, valm[imatch], col = 2, lwd = 2.5, type = 'h')
	}

	axis(side = 1, at = datymid, labels = xlabels, tck = 0)
	title(main = format(ISOdate(2014, jmo, 1), "%B"), cex.main = 1.5, font.main = 1.5)
	par(opar)
}


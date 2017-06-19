
plotCPTFun <- function(ReturnExecResults, stat.fun, replotBreak){

	dydates <- EnvHomogzData$dly_data[[1]]$date
	dkdates <- EnvHomogzData$dek_data[[1]]$date
	modates <- EnvHomogzData$mon_data[[1]]$date

	## Base series or series composite
	##base series
	#if(!GeneralParameters$stn.type$single.series){
	#	get.stn <- ReturnExecResults$station
	#	xpos <- which(as.character(EnvHomogzData$donnees1$id) == get.stn)
	#	dyx <- EnvHomogzData$dly_data[[1]]$data[, xpos]
	#	dkx <- EnvHomogzData$dek_data[[1]]$data[, xpos]
	#	mox <- EnvHomogzData$mon_data[[1]]$data[, xpos]
	#}else{
	#	dyx <- EnvHomogzData$dly_data[[1]]$data
	#	dkx <- EnvHomogzData$dek_data[[1]]$data
	#	mox <- EnvHomogzData$mon_data[[1]]$data
	#}
	##series composite used for the test
	dyx <- ReturnExecResults$refSerie$dyref
	dkx <- ReturnExecResults$refSerie$dkref
	mox <- ReturnExecResults$refSerie$moref

	##cpt obj
	if(replotBreak){
		dyobj <- dkobj <- moobj <- NULL
		monfileout <- file.path(ReturnExecResults$outputdir, paste(ReturnExecResults$station, '_MON.txt', sep = ''))
		modat <- read.table(monfileout, header = TRUE, colClasses = 'character')
		moobj <- na.omit(as.numeric(as.character(modat$Breakpoints.Index)))
		if(ReturnExecResults$period != 'monthly'){
			dekfileout <- file.path(ReturnExecResults$outputdir, paste(ReturnExecResults$station, '_DEK.txt', sep = ''))
			dkdat <- read.table(dekfileout, header = TRUE, colClasses = 'character')
			dkobj <- na.omit(as.numeric(as.character(dkdat$Breakpoints.Index)))
			if(ReturnExecResults$period == 'daily'){
				dlyfileout <- file.path(ReturnExecResults$outputdir, paste(ReturnExecResults$station, '_DLY.txt', sep = ''))
				dydat <- read.table(dlyfileout, header = TRUE, colClasses = 'character')
				dyobj <- na.omit(as.numeric(as.character(dydat$Breakpoints.Index)))
			}
		}
	}else{
		dyobj <- ReturnExecResults$res$dyobj[[2]]
		dkobj <- ReturnExecResults$res$dkobj[[2]]
		moobj <- ReturnExecResults$res$moobj[[2]]
	}

	if(ReturnExecResults$period == 'daily') testIfList <- is.list(dyobj) & is.list(dkobj) & is.list(moobj)
	if(ReturnExecResults$period == 'dekadal') testIfList <- is.list(dkobj) & is.list(moobj)
	if(ReturnExecResults$period == 'monthly') testIfList <- is.list(moobj)

	##cpt index
	if(testIfList){
		dydx <- dyobj$cpt$index
		dkdx <- dkobj$cpt$index
		modx <- moobj$cpt$index
	}else{
		dydx <- dyobj
		dkdx <- dkobj
		modx <- moobj
	}

	##get segment
	if(stat.fun == "CUSUMtr"){
		if(testIfList){
			if(!is.null(dyobj)) dyseg <- getTrend.cptSeg(dyobj, dyx)
			if(!is.null(dkobj)) dkseg <- getTrend.cptSeg(dkobj, dkx)
			moseg <- getTrend.cptSeg(moobj, mox)
		}else{
			if(!is.null(dyobj)) dyseg <- getTrend.cptSeg1(dyobj, dyx)
			if(!is.null(dkobj)) dkseg <- getTrend.cptSeg1(dkobj, dkx)
			moseg <- getTrend.cptSeg1(moobj, mox)
		}
	}else{
		if(testIfList){
			if(!is.null(dyobj)) dyseg <- getMean.cptSeg(dyobj, dyx)
			if(!is.null(dkobj)) dkseg <- getMean.cptSeg(dkobj, dkx)
			moseg <- getMean.cptSeg(moobj, mox)
		}else{
			if(!is.null(dyobj)) dyseg <- getMean.cptSeg1(dyobj, dyx)
			if(!is.null(dkobj)) dkseg <- getMean.cptSeg1(dkobj, dkx)
			moseg <- getMean.cptSeg1(moobj, mox)
		}
	}

	if(!is.null(dyobj)){
		dycptm <- dyseg[dydx]
		dyseg[is.na(dyx)] <- NA
	} 
	if(!is.null(dkobj)){
		dkcptm <- dkseg[dkdx]
		dkseg[is.na(dkx)] <- NA
	} 
	mocptm <- moseg[modx]
	moseg[is.na(mox)] <- NA

	#######
	if(ReturnExecResults$period == 'daily'){
		##format dates
		dydates <- as.Date(dydates, format = '%Y%m%d')
		dycptd <- dydates[dydx]
		dkdates <- as.Date(paste(substr(dkdates, 1, 6), ifelse(substr(dkdates, 7, 7) == "1", "05",
					ifelse(substr(dkdates, 7, 7) == "2", "15", "25")), sep = ''), format = '%Y%m%d')
		dkcptd <- dkdates[dkdx]
		modates <- as.Date(paste(modates, 16, sep = ''), format = '%Y%m%d')
		mocptd <- modates[modx]

		xlim <- c(min(c(dydates, dkdates, modates)), max(c(dydates, dkdates, modates)))
		vgrid <- seq(as.Date(paste(format(dydates[1], '%Y'), 1, 1, sep = '-')),
					as.Date(paste(format(dydates[length(dyx)], '%Y'), 12, 31, sep = '-'))+1, 'year')

		layout(matrix(1:3, ncol = 1), widths = 1, heights = c(1, 1, 1), respect = FALSE)
		op <- par(mar = c(0, 4, 2, 2))
		plot(modates, mox, type = 'n', xaxt = 'n', ylab = 'Monthly', xlim = xlim)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = vgrid, col = "lightgray", lty = "dotted")
		lines(modates, mox)
		lines(modates, moseg, col = 2, lwd = 2)
		points(mocptd, mocptm, col = 4, cex = 1.5)
		segments(mocptd, rep(-100, length(mocptd)), mocptd, mocptm, col = 4, lwd = 2, lty = '1373')
		pltmo <- par("plt")
		usrmo <- par("usr")
		par(op)

		op <- par(mar = c(0, 4, 0, 2))
		plot(dkdates, dkx, type = 'n', xaxt = 'n', ylab = 'Dekadal', xlim = xlim)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = vgrid, col = "lightgray", lty = "dotted")
		lines(dkdates, dkx)
		lines(dkdates, dkseg, col = 2, lwd = 2)
		points(dkcptd, dkcptm, col = 4, cex = 1.5)
		segments(dkcptd, rep(-100, length(dkcptd)), dkcptd, dkcptm, col = 4, lwd = 2, lty = '1373')
		pltdk <- par("plt")
		usrdk <- par("usr")
		par(op)

		op <- par(mar = c(3, 4, 0, 2))
		plot(dydates, dyx, type = 'n', ylab = 'Daily', xlim = xlim)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = vgrid, col = "lightgray", lty = "dotted")
		lines(dydates, dyx)
		lines(dydates, dyseg, col = 2, lwd = 2)
		points(dycptd, dycptm, col = 4, cex = 1.5)
		segments(dycptd, rep(-100, length(dycptd)), dycptd, dycptm, col = 4, lwd = 2, lty = '1373')
		pltdy <- par("plt")
		usrdy <- par("usr")
		par(op)
		return(list(parmo = c(pltmo, usrmo), pardk = c(pltdk, usrdk), pardy = c(pltdy, usrdy)))
	}
	if(ReturnExecResults$period == 'dekadal'){
		##format dates
		dkdates <- as.Date(paste(substr(dkdates, 1, 6), ifelse(substr(dkdates, 7, 7) == "1", "05",
					ifelse(substr(dkdates, 7, 7) == "2", "15", "25")), sep = ''), format = '%Y%m%d')
		dkcptd <- dkdates[dkdx]
		modates <- as.Date(paste(modates, 16, sep = ''), format = '%Y%m%d')
		mocptd <- modates[modx]

		xlim <- c(min(c(dkdates, modates)), max(c(dkdates, modates)))
		vgrid <- seq(as.Date(paste(format(dkdates[1], '%Y'), 1, 1, sep = '-')),
					as.Date(paste(format(dkdates[length(dkx)], '%Y'), 12, 31, sep = '-'))+1, 'year')

		layout(matrix(1:2, ncol = 1), widths = 1, heights = c(1,1), respect = FALSE)
		op <- par(mar = c(0, 4, 2, 2))
		plot(modates, mox, type = 'n', xaxt = 'n', ylab = 'Monthly', xlim = xlim)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = vgrid, col = "lightgray", lty = "dotted")
		lines(modates, mox)
		lines(modates, moseg, col = 2, lwd = 2)
		points(mocptd, mocptm, col = 4, cex = 1.5)
		segments(mocptd, rep(-100, length(mocptd)), mocptd, mocptm, col = 4, lwd = 2, lty = '1373')
		pltmo <- par("plt")
		usrmo <- par("usr")
		par(op)

		op <- par(mar = c(3, 4, 0, 2))
		plot(dkdates, dkx, type = 'n', ylab = 'Dekadal', xlim = xlim)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = vgrid, col = "lightgray", lty = "dotted")
		lines(dkdates, dkx)
		lines(dkdates, dkseg, col = 2, lwd = 2)
		points(dkcptd, dkcptm, col = 4, cex = 1.5)
		segments(dkcptd, rep(-100, length(dkcptd)), dkcptd, dkcptm, col = 4, lwd = 2, lty = '1373')
		pltdk <- par("plt")
		usrdk <- par("usr")
		par(op)
		return(list(parmo = c(pltmo, usrmo), pardk = c(pltdk, usrdk), pardy = rep(0, 8)))
	}
	if(ReturnExecResults$period == 'monthly'){
		##format dates
		modates <- as.Date(paste(modates, 16, sep = ''), format = '%Y%m%d')
		mocptd <- modates[modx]

		xlim <- c(min(modates), max(modates))
		vgrid <- seq(as.Date(paste(format(modates[1], '%Y'), 1, 1, sep = '-')),
					as.Date(paste(format(modates[length(mox)], '%Y'), 12, 31, sep = '-'))+1, 'year')

		op <- par(mar = c(3, 4, 2, 2))
		plot(modates, mox, type = 'n', ylab = 'Monthly', xlim = xlim)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = vgrid, col = "lightgray", lty = "dotted")
		lines(modates, mox)
		lines(modates, moseg, col = 2, lwd = 2)
		points(mocptd, mocptm, col = 4, cex = 1.5)
		segments(mocptd, rep(-100, length(mocptd)), mocptd, mocptm, col = 4, lwd = 2, lty = '1373')
		pltmo <- par("plt")
		usrmo <- par("usr")
		par(op)
		return(list(parmo = c(pltmo, usrmo), pardk = rep(0, 8), pardy = rep(0, 8)))
	}
}


########################################################################################################################################################################

###plot breakpoints
plotHomogBreakPts <- function(parent, notebookTab, replotBreak){
	if(ReturnExecResults$action == 'homog' & GeneralParameters$action == 'homog'){
		varplot <- c("parPlotSize1", "parPlotSize2", "parPlotSize3", "parPlotSize4",
					 "usrCoords1", "usrCoords2", "usrCoords3", "usrCoords4")
		parPltCrdMon <- setNames(lapply(varplot, function(x) assign(x, tclVar(), env = parent.frame())), varplot)
		parPltCrdDek <- setNames(lapply(varplot, function(x) assign(x, tclVar(), env = parent.frame())), varplot)
		parPltCrdDly <- setNames(lapply(varplot, function(x) assign(x, tclVar(), env = parent.frame())), varplot)
		
		plotIt <- function(){
			op1 <- par(bg = 'white')
			pltusr <- plotCPTFun(ReturnExecResults, GeneralParameters$hom.stats, replotBreak)
			par(op1)

			for(j in seq_along(varplot)) tclvalue(parPltCrdMon[[varplot[j]]]) <- pltusr$parmo[j]
			for(j in seq_along(varplot)) tclvalue(parPltCrdDek[[varplot[j]]]) <- pltusr$pardk[j]
			for(j in seq_along(varplot)) tclvalue(parPltCrdDly[[varplot[j]]]) <- pltusr$pardly[j]
		}
	}else{
		InsertMessagesTxt(main.txt.out, 'Reinitialize the operation. Parameters or Outputs are not a homogenization results', format = TRUE)
		return(NULL)
	}

	###################################################################

	tabTitre <- if(replotBreak) paste(ReturnExecResults$station, 'Changed-Breakpoints', sep = '-')
				else paste(ReturnExecResults$station, 'Breakpoints', sep = '-')

	#########
	onglet <- imageNotebookTab_open(parent, notebookTab, tabTitre, AllOpenTabType, AllOpenTabData)
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	#########
	tkbind(img, "<Motion>", function(W, x, y){
		if(ReturnExecResults$period == 'daily'){
			xyMon <- mouseMouvment(W, x, y, parPltCrdMon, ydiv = c(2/3, 1))
			xyDek <- mouseMouvment(W, x, y, parPltCrdDek, ydiv = c(1/3, 2/3))
			xyDly <- mouseMouvment(W, x, y, parPltCrdDly, ydiv = c(0, 1/3))

			if(xyDly$xym$y >= 0 & xyDly$xym$y < 1/3){
				frxcoord <- ifelse(xyDly$inout, '', format(as.Date(xyDly$x, origin = '1970-1-1'), '%d-%b-%Y'))
				frycoord <- ifelse(xyDly$inout, '', round(xyDly$y, 1))
			}else if(xyDek$xym$y >= 1/3 & xyDek$xym$y < 2/3){
				dtdk <- as.Date(xyDek$x, origin = '1970-1-1')
				dek <- ifelse(as.numeric(format(dtdk, '%d')) <= 10, 1,
						ifelse(as.numeric(format(dtdk, '%d')) > 20, 3, 2))
				moyr <- format(dtdk, '%b-%Y')
				frxcoord <- ifelse(xyDek$inout, '', paste(dek, moyr, sep = '-'))
				frycoord <- ifelse(xyDek$inout, '', round(xyDek$y, 1))
			}else if(xyMon$xym$y >= 2/3 & xyMon$xym$y < 1){
				frxcoord <- ifelse(xyMon$inout, '', format(as.Date(xyMon$x, origin = '1970-1-1'), '%b-%Y'))
				frycoord <- ifelse(xyMon$inout, '', round(xyMon$y, 1))
			}else{
				frxcoord <- ''
				frycoord <- ''
			}
		}else if(ReturnExecResults$period == 'dekadal'){
			xyMon <- mouseMouvment(W, x, y, parPltCrdMon, ydiv = c(1/2, 1))
			xyDek <- mouseMouvment(W, x, y, parPltCrdDek, ydiv = c(0, 1/2))

			if(xyDek$xym$y >= 0 & xyDek$xym$y < 1/2){
				dtdk <- as.Date(xyDek$x, origin = '1970-1-1')
				dek <- ifelse(as.numeric(format(dtdk, '%d')) <= 10, 1,
						ifelse(as.numeric(format(dtdk, '%d')) > 20, 3, 2))
				moyr <- format(dtdk, '%b-%Y')
				frxcoord <- ifelse(xyDek$inout, '', paste(dek, moyr, sep = '-'))
				frycoord <- ifelse(xyDek$inout, '', round(xyDek$y, 1))
			}else if(xyMon$xym$y >= 1/2 & xyMon$xym$y < 1){
				frxcoord <- ifelse(xyMon$inout, '', format(as.Date(xyMon$x, origin = '1970-1-1'), '%b-%Y'))
				frycoord <- ifelse(xyMon$inout, '', round(xyMon$y, 1))
			}else{
				frxcoord <- ''
				frycoord <- ''
			}
		}else if(ReturnExecResults$period == 'monthly'){
			xyMon <- mouseMouvment(W, x, y, parPltCrdMon, ydiv = c(0, 1))

			frxcoord <- ifelse(xyMon$inout, '', format(as.Date(xyMon$x, origin = '1970-1-1'), '%b-%Y'))
			frycoord <- ifelse(xyMon$inout, '', round(xyMon$y, 1))
		}else{
			frxcoord <- ''
			frycoord <- ''
		}

		tclvalue(xpcoord) <- frxcoord
		tclvalue(ypcoord) <- frycoord
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}


########################################################################################################################################################################


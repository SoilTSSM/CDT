
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
		return(list(pltmo = pltmo, usrmo = usrmo, pltdk = pltdk, usrdk = usrdk, pltdy = pltdy, usrdy = usrdy))
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
		return(list(pltmo = pltmo, usrmo = usrmo, pltdk = pltdk, usrdk = usrdk))
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
		return(list(pltmo = pltmo, usrmo = usrmo))
	}
}


########################################################################################################################################################################

###plot breakpoints
plotHomogBreakPts <- function(parent, notebookTab, replotBreak){
	pltusr <- NULL

	if(ReturnExecResults$action == 'homog' & GeneralParameters$action == 'homog'){
		stat.fun <- GeneralParameters$hom.stats
		plotIt <- function(){
			op1 <- par(bg = 'white')
			pltusr <<- plotCPTFun(ReturnExecResults, stat.fun, replotBreak)
			par(op1)
		}
	}else{
		InsertMessagesTxt(main.txt.out, 'Reinitialize the operation. Parameters or Outputs are not a homogenization results', format = TRUE)
		return(NULL)
	}

	###################################################################

	tabTitre <- if(replotBreak) paste(ReturnExecResults$station, 'Changed-Breakpoints', sep = '-')
				else paste(ReturnExecResults$station, 'Breakpoints', sep = '-')
	onglet <- imageNotebookTab_open(parent, notebookTab, tabTitre, AllOpenTabType, AllOpenTabData)

	#########
	hscale <- as.numeric(tclvalue(tkget(spinH)))
	vscale <- as.numeric(tclvalue(tkget(spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)

	display.cursor.type <- function(x, y){
		xmouse <- as.numeric(x)
		ymouse <- as.numeric(y)

		imgw <- as.numeric(tclvalue(tkwinfo("reqwidth", img)))
		imgh <- as.numeric(tclvalue(tkwinfo("reqheight", img)))

		imgmw <- as.numeric(tclvalue(tkwinfo("width", img)))
		imgmh <- as.numeric(tclvalue(tkwinfo("height", img)))

		posimgx <- round((imgmw-imgw)/2)
		posimgy <- round((imgmh-imgh)/2)
		orgx <- ifelse(posimgx < 0, 0, posimgx)
		orgy <- ifelse(posimgy < 0, 0, posimgy)

		xpos <- (xmouse-orgx)/imgw
		ypos <- 1-(ymouse-orgy)/imgh

		if(ReturnExecResults$period == 'daily'){

			xpltmo1 <- pltusr$pltmo[1]
			xpltmo2 <- pltusr$pltmo[2]
			ypltmo1 <- pltusr$pltmo[3]
			ypltmo2 <- pltusr$pltmo[4]
			minXmo <- pltusr$usrmo[1]
			rangeXmo <- pltusr$usrmo[2]-pltusr$usrmo[1]
			minYmo <- pltusr$usrmo[3]
			rangeYmo <- pltusr$usrmo[4]-pltusr$usrmo[3]
			xcoordmo <- minXmo+(xpos-xpltmo1)*rangeXmo/(xpltmo2-xpltmo1)
			#change scale:layout (2/3 to 1)
			yposmo <- (ypos-2/3)/(1-2/3)
			ycoordmo <- minYmo+(yposmo-ypltmo1)*rangeYmo/(ypltmo2-ypltmo1)

			xpltdk1 <- pltusr$pltdk[1]
			xpltdk2 <- pltusr$pltdk[2]
			ypltdk1 <- pltusr$pltdk[3]
			ypltdk2 <- pltusr$pltdk[4]
			minXdk <- pltusr$usrdk[1]
			rangeXdk <- pltusr$usrdk[2]-pltusr$usrdk[1]
			minYdk <- pltusr$usrdk[3]
			rangeYdk <- pltusr$usrdk[4]-pltusr$usrdk[3]
			xcoorddk <- minXdk+(xpos-xpltdk1)*rangeXdk/(xpltdk2-xpltdk1)
			yposdk <- (ypos-1/3)/(2/3-1/3)
			ycoorddk <- minYdk+(yposdk-ypltdk1)*rangeYdk/(ypltdk2-ypltdk1)

			xpltdy1 <- pltusr$pltdy[1]
			xpltdy2 <- pltusr$pltdy[2]
			ypltdy1 <- pltusr$pltdy[3]
			ypltdy2 <- pltusr$pltdy[4]
			minXdy <- pltusr$usrdy[1]
			rangeXdy <- pltusr$usrdy[2]-pltusr$usrdy[1]
			minYdy <- pltusr$usrdy[3]
			rangeYdy <- pltusr$usrdy[4]-pltusr$usrdy[3]
			xcoorddy <- minXdy+(xpos-xpltdy1)*rangeXdy/(xpltdy2-xpltdy1)
			yposdy <- (ypos-0)/(1/3-0)
			ycoorddy <- minYdy+(yposdy-ypltdy1)*rangeYdy/(ypltdy2-ypltdy1)

			if(ypos >= 0 & ypos < 1/3){
				frxcoord <- format(as.Date(xcoorddy, origin = '1970-1-1'), '%d-%b-%Y')
				frycoord <- round(ycoorddy, 1)
			}else if(ypos >= 1/3 & ypos < 2/3){
				dtdk <- as.Date(xcoorddk, origin = '1970-1-1')
				dek <- ifelse(as.numeric(format(dtdk, '%d')) <= 10, 1,
						ifelse(as.numeric(format(dtdk, '%d')) > 20, 3, 2))
				moyr <- format(dtdk, '%b-%Y')
				frxcoord <- paste(dek, moyr, sep = '-')
				frycoord <- round(ycoorddk, 1)
			}else if(ypos >= 2/3 & ypos < 1){
				frxcoord <- format(as.Date(xcoordmo, origin = '1970-1-1'), '%b-%Y')
				frycoord <- round(ycoordmo, 1)
			}
			if(xcoordmo < pltusr$usrmo[1] | xcoordmo > pltusr$usrmo[2] | ycoorddy < pltusr$usrdy[3] | ycoordmo > pltusr$usrmo[4]){
				frxcoord <- ''
				frycoord <- ''
			}
		}else if(ReturnExecResults$period == 'dekadal'){
			xpltmo1 <- pltusr$pltmo[1]
			xpltmo2 <- pltusr$pltmo[2]
			ypltmo1 <- pltusr$pltmo[3]
			ypltmo2 <- pltusr$pltmo[4]
			minXmo <- pltusr$usrmo[1]
			rangeXmo <- pltusr$usrmo[2]-pltusr$usrmo[1]
			minYmo <- pltusr$usrmo[3]
			rangeYmo <- pltusr$usrmo[4]-pltusr$usrmo[3]
			xcoordmo <- minXmo+(xpos-xpltmo1)*rangeXmo/(xpltmo2-xpltmo1)
			yposmo <- (ypos-1/2)/(1-1/2)
			ycoordmo <- minYmo+(yposmo-ypltmo1)*rangeYmo/(ypltmo2-ypltmo1)

			xpltdk1 <- pltusr$pltdk[1]
			xpltdk2 <- pltusr$pltdk[2]
			ypltdk1 <- pltusr$pltdk[3]
			ypltdk2 <- pltusr$pltdk[4]
			minXdk <- pltusr$usrdk[1]
			rangeXdk <- pltusr$usrdk[2]-pltusr$usrdk[1]
			minYdk <- pltusr$usrdk[3]
			rangeYdk <- pltusr$usrdk[4]-pltusr$usrdk[3]
			xcoorddk <- minXdk+(xpos-xpltdk1)*rangeXdk/(xpltdk2-xpltdk1)
			#scale
			yposdk <- (ypos-0)/(1/2-0)
			ycoorddk <- minYdk+(yposdk-ypltdk1)*rangeYdk/(ypltdk2-ypltdk1)

			if(ypos >= 0 & ypos < 1/2){
				dtdk <- as.Date(xcoorddk, origin = '1970-1-1')
				dek <- ifelse(as.numeric(format(dtdk, '%d')) <= 10, 1,
						ifelse(as.numeric(format(dtdk, '%d')) > 20, 3, 2))
				moyr <- format(dtdk, '%b-%Y')
				frxcoord <- paste(dek, moyr, sep = '-')
				frycoord <- round(ycoorddk, 1)
			}else if(ypos >= 1/2 & ypos < 1){
				frxcoord <- format(as.Date(xcoordmo, origin = '1970-1-1'), '%b-%Y')
				frycoord <- round(ycoordmo, 1)
			}
			if(xcoordmo < pltusr$usrmo[1] | xcoordmo > pltusr$usrmo[2] | ycoorddk < pltusr$usrdk[3] | ycoordmo > pltusr$usrmo[4]){
				frxcoord <- ''
				frycoord <- ''
			}
		}else if(ReturnExecResults$period == 'monthly'){
			xpltmo1 <- pltusr$pltmo[1]
			xpltmo2 <- pltusr$pltmo[2]
			ypltmo1 <- pltusr$pltmo[3]
			ypltmo2 <- pltusr$pltmo[4]
			minXmo <- pltusr$usrmo[1]
			rangeXmo <- pltusr$usrmo[2]-pltusr$usrmo[1]
			minYmo <- pltusr$usrmo[3]
			rangeYmo <- pltusr$usrmo[4]-pltusr$usrmo[3]
			xcoordmo <- minXmo+(xpos-xpltmo1)*rangeXmo/(xpltmo2-xpltmo1)
			#scale
			yposmo <- (ypos-0)/(1-0)
			ycoordmo <- minYmo+(yposmo-ypltmo1)*rangeYmo/(ypltmo2-ypltmo1)
			frxcoord <- format(as.Date(xcoordmo, origin = '1970-1-1'), '%b-%Y')
			frycoord <- round(ycoordmo, 1)
			if(xcoordmo < pltusr$usrmo[1] | xcoordmo > pltusr$usrmo[2] | ycoordmo < pltusr$usrmo[3] | ycoordmo > pltusr$usrmo[4]){
				frxcoord <- ''
				frycoord <- ''
			}
		}

		tclvalue(xpcoord) <- frxcoord
		tclvalue(ypcoord) <- frycoord
	}

	tkbind(img, "<Motion>", function(x, y){
		display.cursor.type(x, y)
	})
	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}


########################################################################################################################################################################


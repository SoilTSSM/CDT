###test internet connection
testConnection <- function(url = "https://cloud.r-project.org") {
    if(!as.logical(capabilities(what = "http/ftp"))) return(FALSE)
    test <- try(suppressWarnings(readLines(url, n = 1)), silent = TRUE)
    ifelse(inherits(test, "try-error"), FALSE, TRUE)
}

####################################################################
## Setting Initialization Parameters
initialize.parameters <- function(action, freqData, previous = FALSE, gal.params = GeneralParameters){
	if(!is.null(gal.params)){
		if(!is.null(gal.params$action)){
			if(gal.params$action == action) initpars <- gal.params
			else{
				if(previous & gal.params$action == action) initpars <- init.params(action, as.character(gal.params$period))
				else initpars <- init.params(action, freqData)
			}
		}else initpars <- init.params(action, freqData)
	}else initpars <- init.params(action, freqData)
	return(initpars)
}

####################################################################
## Refresh all environment, destroy left panel widgets
refreshCDT.lcmd.env <- function(lcmdf = lcmd.frame, choixStn = lchoixStnFr){
	tkdestroy(lcmdf)
	tkdestroy(choixStn)
	choixStn$env$stn.choix <- c('')
	tclvalue(choixStn$env$stn.choix.val) <- choixStn$env$stn.choix[1]

	all.cdt.env <- list(EnvQcOutlierData, EnvQcZeroChkData, EnvInterpolation, EnvExtractData,
						EnvHomogzData, EnvZoomPars,
						EnvHOValidation, EnvHOValidationplot,
						EnvClimatoAnalysis, EnvClimatoAnalysisplot,
						EnvPICSA, EnvPICSAplot,
						EnvLOOCValidation, EnvLOOCValidationplot,
						EnvClimatoCalcPlot, EnvAnomalyCalcPlot,
						EnvOnsetCalcPlot, EnvCessationCalcPlot, EnvSeasLengthCalcPlot,
						EnvDailyRainAnalysisplot, EnvSummaryDataplot)
	ret <- lapply(cdt.lcmd.container, assign, NULL, envir = .GlobalEnv)
	ret <- lapply(all.cdt.env, function(x) rm(list = ls(envir = x), envir = x))
}

####################################################################
#get the file name without extension
getf.no.ext <- function(flname){
	ig <- grep('\\.', flname)
	if(length(ig) == 0){
		fret <- flname
	}else{
		fsplit <- unlist(strsplit(flname, '\\.'))
		fret <- sub(paste0('.', fsplit[length(fsplit)]), '', flname)
	}
	return(fret)
}

####################################################################
#BWidget info-bulle(ballon, tooltip) help
infobulle <- function(tclobj, text){
	tcl("interp", "alias", "", "help", "", "DynamicHelp::register") 
	tcl('help', tclobj, 'balloon', text)
}

##Binding event in toolbar and display on status bar
status.bar.display <- function(tclobj, tclvar, text){
	tkbind(tclobj, "<Enter>", function() tclvalue(tclvar) <- text)
	tkbind(tclobj, "<Leave>", function() tclvalue(tclvar) <- "")
}


###########################################
helpWidget <- function(tclobj, statusbar_tclvar, text_ballon, text_statusbar){
	tkbind(tclobj, "<Enter>", function() tclvalue(statusbar_tclvar) <- text_statusbar)
	tkbind(tclobj, "<Leave>", function() tclvalue(statusbar_tclvar) <- "")
	tcl("DynamicHelp::register", tclobj, 'balloon', text_ballon)
}


####################################################################
##Create button on toolbar
tkbutton.toolbar <- function(frame, img.dir, img.file, txtvar.status, txt.tooltip, txt.status){
	picture <- tkimage.create('photo', '-file', file.path(img.dir, img.file, fsep = .Platform$file.sep))
	button <- tkbutton(frame, image = picture, relief = 'flat')
	infobulle(button, txt.tooltip)
	status.bar.display(button, txtvar.status, txt.status)
	return(button)
}

####################################################################
##spinbox
ttkspinbox <- function(parent, ...) tkwidget(parent, "ttk::spinbox", ...)

### deactivate spinbox
spinbox.state <- function(horiz = spinH, vert = spinV, state = 'disabled'){
	tkconfigure(horiz, state = state)
	tkconfigure(vert, state = state)
}

####################################################################
### Insert text 
InsertMessagesTxt <- function(wdgt, texta, format = FALSE, fgcolor = 'red', bgcolor = 'yellow'){
	font1 <- tkfont.create(family = "times", weight = "bold", slant = "roman", size = 11)
	font2 <- tkfont.create(family = "times", weight = "normal", slant = "italic", size = 11)
	tktag.configure(wdgt, "formated1", foreground = fgcolor, background = bgcolor, font = font1)
	tktag.configure(wdgt, "formated2", foreground = fgcolor, background = bgcolor, font = font2)
	txtformated <- if(fgcolor == 'red' & bgcolor == 'yellow') "formated1" else "formated2"
	# tktag.configure(wdgt, "sel", foreground = fgcolor, background = 'blue', font = font2)
	# chn <- tclvalue(tkget(wdgt, "0.0", "end"))
	# vectxt <- unlist(strsplit(chn, "\n"))
	# lnt <- length(vectxt)
	if(format) tkinsert(wdgt, "end", paste(texta, "\n"), txtformated)
	else tkinsert(wdgt, "end", paste(texta, "\n"))
	tcl(wdgt, 'yview', 'moveto', '1.0')
	tcl("update")
}

###############################################################################
#BWidget NoteBook
isaTabBwNb <- local({
	k <- 0
	function() {
		k <<- k + 1
		return(k)
	}
})

###
bwNoteBook <- function(parent, side = 'top', ...){
	tn <- tkwidget(parent, "NoteBook", side = side, ...)
	return(tn)
}

##arg ... pass to insert
bwAddTab <- function(parent, text = "Tab", ...){
	IDtab <- paste0('_BwNb', isaTabBwNb())
	tab <- tkinsert(parent, 'end', IDtab, text = text, ...)
	win <- .Tk.newwin(tclvalue(tab))
	win$IDtab <- IDtab
	return(win)
}

##
bwRaiseTab <- function(parent, tab) tcl(parent, 'raise', tab$IDtab)

###############################################################################

##BWidget PanedWindow
bwPanedWindow <- function(parent, ...){
	panewin <- tkwidget(parent, 'PanedWindow', ...)
	return(panewin)
}

bwAddPanedWindow <- function(parent, ...){
	panWidget <- tkadd(parent, ...)
	win <- .Tk.newwin(panWidget)
	return(win)
}

##Usage
# panw <- bwPanedWindow(parent_frame, side = 'right')
# tkgrid(panw)
# panfr <- bwAddPanedWindow(panw, minsize = 100)
# fr1 <- tkframe(panfr)
# tkgrid(fr1)
###############################################################################
##BWidget ScrolledWindow ScrollableFrame
bwScrolledWindow <- function(parent, ...){
	scrwin <- tkwidget(parent, "ScrolledWindow", ...)
	return(scrwin)
}

bwScrollableFrame <- function(parent, ...){
	scrfrm <- tkwidget(parent, "ScrollableFrame", ...)
	tcl(parent, "setwidget", scrfrm)
	##new 
	#return(scrfrm)
	#old
	 subfram <- tclvalue(tcl(scrfrm, "getframe"))
	 win <- .Tk.newwin(subfram)
	 return(win)
}

##Usage
# scrwin <- bwScrolledWindow(parent_frame, relief = 'sunken', borderwidth = 2)
# tkgrid(scrwin)
# subfram <- bwScrollableFrame(scrwin, width = 400, height = 400)
# lid <- tklabel(subfram, text = "Name")
# tkgrid(lid)

###############################################################################
##ScrollableCanvas

ScrollCanvas <- function(parent, ...){
	canvas <- tkcanvas(parent, ...)
	tcl(parent, "setwidget", canvas)
	return(canvas)
}

setScrollCanvas <- function(parent, width, height){
	bbox <- tkbbox(parent, "all")
	tkconfigure(parent, width = width, height = height, scrollregion = bbox)
}

setScrollCanvas1 <- function(parent){
	bbox <- tkbbox(parent, "all")
	tkconfigure(parent, scrollregion = bbox)
}

##Usage
# scrwin <- bwScrolledWindow(parent_frame, relief = 'sunken', borderwidth = 2)
# tkgrid(scrwin)
# canvas <- ScrollCanvasscrwin, width = 600, height = 400)
# tkgrid(canvas)
# tkcreate(canvas, 'line', 0,0,600,400)
# tkcreate(canvas, 'line', 0,400,600,0)
# setScrollCanvas(canvas)
# always add setScrollCanvas(canvas) when one object or a group of object are added

###############################################################################
#Resize Tcl image type photo

resizeTclImage <- function(file, factor = 2, zoom = TRUE){
	imgtmp <- tkimage.create('photo', file = file)
	imgrsz <- tkimage.create("photo")
	if(zoom) tcl(imgrsz, "copy", imgtmp, zoom = factor)
	else tcl(imgrsz, "copy", imgtmp, subsample = factor)  
	tcl('image', 'delete', imgtmp)
	return(imgrsz)
}

###############################################################################
## Test leap year

is.leapyear <- function(year){
	leap <- year%%c(4, 100, 400)
	((leap[1] == 0) & (leap[2] != 0)) | (leap[3] == 0)
}

is.leapyears <- function(years){
	leap <- sapply(years, function(x) x%%c(4, 100, 400))
	((leap[1, ] == 0) & (leap[2, ] != 0)) | (leap[3, ] == 0)
}

# is.leapyear <- function(year){return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))}


###############################################################################
#Cycle month, start month, n: number of month (DJF, start = 'December', n = 3)
# full = TRUE (July),  FALSE (Jul)
# return end month

cycleMonth <- function(start, n, full = FALSE){
	frmt <- if(full) "%B" else "%b"
	mois <- format(ISOdate(2014, 1:12, 1), frmt)
	ix <- which(mois == start)
	im <- (ix+(n-1))%%12
	if(im == 0) im <- 12
	return(mois[im])
}

#cycleMonth1 -->  file: cdtClimato_functions.R
###############################################################################
## Add or substract months

addMonths <- function(daty, n = 1){
	date0 <- seq(daty, by = paste(n, "months"), length = 2)[2]
	date1 <- seq(as.Date(paste(format(daty,'%Y-%m'),'01', sep = '-')), by = paste(n+1, "months"), length = 2)[2]-1
	daty <- if(date0 > date1) date1 else date0
	return(daty)
}
# addMonthsVec <- Vectorize(addMonths, "daty", SIMPLIFY = FALSE)
addMonthsVec <- function(daty, n = 1) do.call(c, lapply(daty, addMonths, n))

#add.months -->  file: cdtClimato_functions.R
###############################################################################
## Add or substract dekads

addDekads <- function(daty, n = 1){
	idek <- as.numeric(substr(format(daty,'%Y%m%d'), 8, 8))+n
	dek <- idek%%3
	if(dek == 0) dek <- 3
	daty <- format(addMonths(daty, floor((idek-1)/3)), '%Y-%m')
	daty <- as.Date(paste(daty, dek, sep = '-'))
	return(daty)
}
# addDekadsVec <- Vectorize(addDekads, "daty", SIMPLIFY = FALSE)
addDekadsVec <- function(daty, n = 1) do.call(c, lapply(daty, addDekads, n))

#add.dekads -->  file: cdtClimato_functions.R
###############################################################################
## Add or substract pentad

addPentads <- function(daty, n = 1){
	ipen <- as.numeric(substr(format(daty,'%Y%m%d'), 8, 8))+n
	pen <- ipen%%6
	if(pen == 0) pen <- 6
	daty <- format(addMonths(daty, floor((ipen-1)/6)), '%Y-%m')
	daty <- as.Date(paste(daty, pen, sep = '-'))
	return(daty)
}
# addPentadsVec <- Vectorize(addPentads, "daty", SIMPLIFY = FALSE)
addPentadsVec <- function(daty, n = 1) do.call(c, lapply(daty, addPentads, n))

###############################################################################
## get month in numeric format for a season
## getMonthsInSeason('November','February') #donne 11 12  1  2

getMonthsInSeason <- function(start.mois, end.mois, full = FALSE){
	if(full)  frmt <- "%B"
	else frmt <- "%b"
	tmois <- format(ISOdate(2014, 1:12, 1), frmt)
	smois <- which(tmois == start.mois)
	emois <- which(tmois == end.mois)
	im <- (smois:(smois+((emois-smois)%%12)))%%12
	im[im == 0] <- 12
	return(im)
}

getMonthsInSeason1 <- function(start.mois, len.mois, full = FALSE){
	if(full) frmt <- "%B"
	else frmt <- "%b"
	tmois <- format(ISOdate(2014, 1:12, 1), frmt)
	ix <- which(tmois == start.mois)
	im <- (ix:(ix+(len.mois-1)))%%12
	im[im == 0] <- 12
	return(im)
}

###############################################################################
#File or directory to save result
#filedirVar tclVar

fileORdir2Save <- function(filedirVar, initialdir = getwd(), isFile = TRUE){
	if(isFile){
		filetypes <- "{{Text Files} {.txt .TXT}} {{CSV Files} {.csv .CSV}} {{All files} *}"
		if(Sys.info()["sysname"] == "Windows"){
			file2save <- tkgetSaveFile(initialdir = initialdir, initialfile = "",
										filetypes = filetypes, defaultextension = TRUE)
		}else{
			file2save <- tkgetSaveFile(initialdir = initialdir, initialfile = "", filetypes = filetypes)
		}
		tclvalue(filedirVar) <- if(!is.na(file2save)) file2save else ""
	}else{
		dir2save <- tk_choose.dir(default = initialdir, caption = "")
		if(is.na(dir2save)) tclvalue(filedirVar) <- ""
		else{
			dir.create(dir2save, showWarnings = FALSE, recursive = TRUE)
			tclvalue(filedirVar) <- dir2save
		}
	}
}


###############################################################################
##get all open files

openFile_ttkcomboList <- function(){
	nopfs <- length(AllOpenFilesType)
	if(nopfs != 0){
		listOpenFiles <- c('', lapply(1:nopfs, function(j) AllOpenFilesData[[j]][[1]]))
	}else{
		listOpenFiles <- list()
		listOpenFiles[[1]] <- c('')
	}
	return(listOpenFiles)
}

###############################################################################
##get index of selected file from AllOpenFilesData

getIndex.AllOpenFiles <- function(nomfile){
	if(inherits(nomfile, "tclVar")){
		fileio <- tclvalue(nomfile)
	}else if(is.character(nomfile)){
		fileio <- nomfile
	}else return(NULL)
	if(fileio != ""){
		all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))
		jfile <- which(all.open.file == fileio)
		return(jfile)
	}else return(NULL)
}

##################################################################################
#Reshape data.frame XYZ to matrix list(x, y, z = matrix)

reshapeXYZ2Matrix <- function(df){
	#require(reshape2)
	df <- as.data.frame(df, stringsAsFactors = FALSE)
	names(df) <- c('x', 'y', 'z')
	x <- sort(unique(df$x))
	y <- sort(unique(df$y))
	z <- acast(df, x~y, value.var = "z")
	dimnames(z) <- NULL
	return(list(x = x, y = y, z = z))
}

##################################################################################
###get index of points at grid
##pts_Coords = list(lon, lat)
##grd_Coords  = list(lon, lat)

grid2pointINDEX <- function(pts_Coords, grd_Coords){
	newgrid <- expand.grid(lon = grd_Coords$lon, lat = grd_Coords$lat)
	coordinates(newgrid) <- ~lon+lat
	newgrid <- SpatialPixels(points = newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))
	pts.loc <- data.frame(lon = pts_Coords$lon, lat = pts_Coords$lat)
	pts.loc <- SpatialPoints(pts.loc)
	ijGrd <- unname(over(pts.loc, geometry(newgrid)))
	return(ijGrd)
}

##################################################################################
###define spatialPixels
##grd_Coords  = list(lon, lat)

defSpatialPixels <- function(grd_Coords){
	newgrid <- expand.grid(lon = grd_Coords$lon, lat = grd_Coords$lat)
	coordinates(newgrid) <- ~lon+lat
	newgrid <- SpatialPixels(points = newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))
	return(newgrid)
}

##################################################################################
### grid resolution
##grd_Coords  = list(lon, lat)

gridResolution <- function(grd_Coords){
	newgrid <- expand.grid(lon = grd_Coords$lon, lat = grd_Coords$lat)
	coordinates(newgrid) <- ~lon+lat
	newgrid <- SpatialPixels(points = newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)), proj4string = CRS(as.character(NA)))
	return(newgrid@grid)
}

##################################################################################
## Compare if 2 SpatialPixelsObjare have the same resolution

is.diffSpatialPixelsObj <- function(SP1, SP2, tol = 1e-07){
	SP1CelldX <- SP1@grid@cellsize[1]
	SP1CelldY <- SP1@grid@cellsize[2]
	SP1CellSX <- SP1@grid@cells.dim[1]
	SP1CellSY <- SP1@grid@cells.dim[2]
	SP2CelldX <- SP2@grid@cellsize[1]
	SP2CelldY <- SP2@grid@cellsize[2]
	SP2CellSX <- SP2@grid@cells.dim[1]
	SP2CellSY <- SP2@grid@cells.dim[2]
	unname(abs(SP1CelldX-SP2CelldX) > tol | (SP1CellSX != SP2CellSX) | abs(SP1CelldY-SP2CelldY) > tol | SP1CellSY != SP2CellSY)
}

##################################################################################
### get Coarse grid from matrix data with dim lon/lat

indexCoarseGrid <- function(lon, lat, res = 0.25){
	res <- if(length(res) > 1) res[1:2] else c(res, res)
	nlon <- length(lon)
	nlat <- length(lat)
	ilon <- diff(range(lon))/(nlon-1)
	ilat <- diff(range(lat))/(nlat-1)
	ix <- round(res[1]/ilon)-1
	iy <- round(res[2]/ilat)-1
	if(ix < 1) ix <- 1
	if(iy < 1) iy <- 1
	ix <- seq(1, nlon, ix)
	iy <- seq(1, nlat, iy)
	nx <- length(ix)
	ny <- length(iy)
	if(ix[nx] < nlon)
		ix <- if((lon[nlon]-lon[ix[nx]]) < res[1]/2) c(ix[-nx], nlon) else c(ix, nlon)
	if(iy[ny] < nlat)
		iy <- if((lat[nlat]-lat[iy[ny]]) < res[2]/2) c(iy[-ny], nlat) else c(iy, nlat)
	return(list(ix = ix, iy = iy))
}

##################################################################################
### create grid for interpolation 

createGrid <- function(ObjStn, ObjGrd, ObjRfe = NULL, as.dim.elv = TRUE, latlong = 'km', normalize = FALSE, coarse.grid = TRUE, res.coarse = 0.25){
	lon2UTM <- function(lon) (floor((lon + 180)/6) %% 60) + 1
	lonO <- ObjStn$x
	latO <- ObjStn$y
	lonS <- ObjGrd$x
	latS <- ObjGrd$y
	gridO <- data.frame(lon = lonO, lat = latO, elv = ObjStn$z, dem = ObjStn$z, slp = ObjStn$slp, asp = ObjStn$asp)
	gridS <- data.frame(expand.grid(lon = lonS, lat = latS), elv = c(ObjGrd$z), dem = c(ObjGrd$z), slp = c(ObjGrd$slp), asp = c(ObjGrd$asp))

	ixOa <- is.na(gridO$elv)
	if(any(ixOa)){
		gridOa <- gridO[ixOa, c('lon', 'lat'), drop = FALSE]
		gridOa <- krige(formula = elv~1, locations = ~lon+lat, data = na.omit(gridS),
						newdata = gridOa, nmin = 3, nmax = 10, debug.level = 0)
		gridO$elv[ixOa] <- round(gridOa$var1.pred)
	}

	ixSa <- is.na(gridS$elv)
	if(any(ixSa)){
		gridSa <- gridS[ixSa, c('lon', 'lat'), drop = FALSE]
		gridSa <- krige(formula = elv~1, locations = ~lon+lat, data = na.omit(gridS),
						newdata = gridSa, nmin = 3, nmax = 10, debug.level = 0)
		gridS$elv[ixSa] <- round(gridSa$var1.pred)
		ObjGrd$z[ixSa] <- round(gridSa$var1.pred)
	}

	coordinates(gridO) <- ~lon+lat
	coordinates(gridS) <- ~lon+lat

	gridS1 <- NULL
	gridS2 <- NULL
	idcoarse <- NULL
	idcoarse1 <- NULL
	if(coarse.grid){
		idcoarse <- indexCoarseGrid(ObjGrd$x, ObjGrd$y, res.coarse)
		lonS1 <- ObjGrd$x[idcoarse$ix]
		latS1 <- ObjGrd$y[idcoarse$iy]
		demS1 <- c(ObjGrd$z[idcoarse$ix, idcoarse$iy])
		slpS1 <- c(ObjGrd$slp[idcoarse$ix, idcoarse$iy])
		aspS1 <- c(ObjGrd$asp[idcoarse$ix, idcoarse$iy])
		gridS1 <- data.frame(expand.grid(lon = lonS1, lat = latS1), elv = demS1, dem = demS1, slp = slpS1, asp = aspS1)
		coordinates(gridS1) <- ~lon+lat
		if(!is.null(ObjRfe)){
			idcoarse1 <- indexCoarseGrid(ObjRfe$x, ObjRfe$y, res.coarse)
			lonS2 <- ObjRfe$x[idcoarse1$ix]
			latS2 <- ObjRfe$y[idcoarse1$iy]
			demS2 <- c(ObjRfe$z[idcoarse1$ix, idcoarse1$iy])
			slpS2 <- c(ObjRfe$slp[idcoarse1$ix, idcoarse1$iy])
			aspS2 <- c(ObjRfe$asp[idcoarse1$ix, idcoarse1$iy])
			gridS2 <- data.frame(expand.grid(lon = lonS2, lat = latS2), elv = demS2, dem = demS2, slp = slpS2, asp = aspS2)
			coordinates(gridS2) <- ~lon+lat
		}
	}

	if(as.dim.elv){
		proj4string(gridO) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
		proj4string(gridS) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

		zone <- lon2UTM(sum(range(lonS))/2)
		orient <- if((sum(range(latS))/2) >= 0) 'north' else 'south'
		proj <- paste("+proj=utm +zone=", zone, " +", orient, " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep = "")

		gridO.utm <- spTransform(gridO, CRS(proj))
		gridS.utm <- spTransform(gridS, CRS(proj))

		gridO <- as(gridO.utm, 'data.frame')
		gridS <- as(gridS.utm, 'data.frame')

		origin <- rbind(apply(gridS[, c('lon', 'lat')],2,min), apply(gridO[, c('lon', 'lat')],2,min))
		origin <- apply(origin, 2, min)
		gridO[, c('lon', 'lat')] <- t(t(gridO[, c('lon', 'lat')])-origin)
		gridS[, c('lon', 'lat')] <- t(t(gridS[, c('lon', 'lat')])-origin)
		if(tolower(latlong) == 'km'){
			gridO[, c('lon', 'lat')] <- gridO[, c('lon', 'lat')]/1000
			gridS[, c('lon', 'lat')] <- gridS[, c('lon', 'lat')]/1000
		}
		if(normalize){
			mgrd <- apply(rbind(gridO[, c('lon', 'lat', 'elv')], gridS[, c('lon', 'lat', 'elv')]), 2, mean)
			sgrd <- apply(rbind(gridO[, c('lon', 'lat', 'elv')], gridS[, c('lon', 'lat', 'elv')]), 2, sd)
			gridO[, c('lon', 'lat', 'elv')] <- as.data.frame(t((t(gridO[, c('lon', 'lat', 'elv')])-mgrd)/sgrd))
			gridS[, c('lon', 'lat', 'elv')] <- as.data.frame(t((t(gridS[, c('lon', 'lat', 'elv')])-mgrd)/sgrd))
		}
		coordinates(gridO) <- ~lon+lat+elv
		coordinates(gridS) <- ~lon+lat+elv

		if(coarse.grid){
			proj4string(gridS1) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
			gridS1.utm <- spTransform(gridS1, CRS(proj))
			gridS1 <- as(gridS1.utm, 'data.frame')
			gridS1[, c('lon', 'lat')] <- t(t(gridS1[, c('lon', 'lat')])-origin)
			if(tolower(latlong) == 'km') gridS1[, c('lon', 'lat')] <- gridS1[, c('lon', 'lat')]/1000
			if(normalize){
				# mgrd1 <- apply(gridS1[, c('lon', 'lat', 'elv')], 2, mean)
				# sgrd1 <- apply(gridS1[, c('lon', 'lat', 'elv')], 2, sd)
				gridS1[, c('lon', 'lat', 'elv')] <- as.data.frame(t((t(gridS1[, c('lon', 'lat', 'elv')])-mgrd)/sgrd))
			}
			coordinates(gridS1) <- ~lon+lat+elv
			
			if(!is.null(ObjRfe)){
				proj4string(gridS2) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
				gridS2.utm <- spTransform(gridS2, CRS(proj))
				gridS2 <- as(gridS2.utm, 'data.frame')
				gridS2[, c('lon', 'lat')] <- t(t(gridS2[, c('lon', 'lat')])-origin)
				if(tolower(latlong) == 'km') gridS2[, c('lon', 'lat')] <- gridS2[, c('lon', 'lat')]/1000
				if(normalize){
					# mgrd2 <- apply(gridS2[, c('lon', 'lat', 'elv')], 2, mean)
					# sgrd2 <- apply(gridS2[, c('lon', 'lat', 'elv')], 2, sd)
					gridS2[, c('lon', 'lat', 'elv')] <- as.data.frame(t((t(gridS2[, c('lon', 'lat', 'elv')])-mgrd)/sgrd))
				}
				coordinates(gridS2) <- ~lon+lat+elv
			}
		}
	}
	max.slope <- 1.570796 #change 90 if degree
	max.aspect <- 360
	min.dem <- min(gridS$dem, na.rm = TRUE)
	max.dem <- max(gridS$dem, na.rm = TRUE)

	gridO$dem <- (gridO$dem-min.dem)/(max.dem-min.dem)
	gridO$slp <- gridO$slp/max.slope
	gridO$asp <- gridO$asp/max.aspect

	gridS$dem <- (gridS$dem-min.dem)/(max.dem-min.dem)
	gridS$slp <- gridS$slp/max.slope
	gridS$asp <- gridS$asp/max.aspect

	if(!is.null(gridS1)){
		gridS1$dem <- (gridS1$dem-min.dem)/(max.dem-min.dem)
		gridS1$slp <- gridS1$slp/max.slope
		gridS1$asp <- gridS1$asp/max.aspect
	}
	if(!is.null(gridS2)){
		gridS2$dem <- (gridS2$dem-min.dem)/(max.dem-min.dem)
		gridS2$slp <- gridS2$slp/max.slope
		gridS2$asp <- gridS2$asp/max.aspect
	}

	return(list(coords.stn = gridO, coords.grd = gridS1, coords.rfe = gridS2,
				newgrid = gridS, idxy = idcoarse, idxy.rfe = idcoarse1))
}

##################################################################################
### create grid for interpolation 

createGrid.merging <- function(grdData, ObjVar = NULL, coarse.grid = TRUE, res.coarse = 0.25){
	gridS <- data.frame(expand.grid(lon = grdData$x,
									lat = grdData$y),
						dem = c(grdData$z),
						slp = c(grdData$slp),
						asp = c(grdData$asp))
	coordinates(gridS) <- ~lon+lat

	gridS1 <- NULL
	gridS2 <- NULL
	idcoarse <- NULL
	idcoarse1 <- NULL

	if(coarse.grid){
		idcoarse <- indexCoarseGrid(grdData$x, grdData$y, res.coarse)
		gridS1 <- data.frame(expand.grid(lon = grdData$x[idcoarse$ix],
										lat = grdData$y[idcoarse$iy]),
							dem = c(grdData$z[idcoarse$ix, idcoarse$iy]),
							slp = c(grdData$slp[idcoarse$ix, idcoarse$iy]),
							asp = c(grdData$asp[idcoarse$ix, idcoarse$iy]))
		coordinates(gridS1) <- ~lon+lat
		if(!is.null(ObjVar)){
			idcoarse1 <- indexCoarseGrid(ObjVar$x, ObjVar$y, res.coarse)
			gridS2 <- data.frame(expand.grid(lon = ObjVar$x[idcoarse1$ix],
											lat = ObjVar$y[idcoarse1$iy]),
								dem = c(ObjVar$z[idcoarse1$ix, idcoarse1$iy]),
								slp = c(ObjVar$slp[idcoarse1$ix, idcoarse1$iy]),
								asp = c(ObjVar$asp[idcoarse1$ix, idcoarse1$iy]))
			coordinates(gridS2) <- ~lon+lat
		}
	}

	max.slope <- max(gridS$slp, na.rm = TRUE)
	max.aspect <- max(gridS$asp, na.rm = TRUE)
	min.dem <- min(gridS$dem, na.rm = TRUE)
	max.dem <- max(gridS$dem, na.rm = TRUE)

	gridS$dem <- (gridS$dem-min.dem)/(max.dem-min.dem)
	gridS$slp <- gridS$slp/max.slope
	gridS$asp <- gridS$asp/max.aspect

	if(!is.null(gridS1)){
		gridS1$dem <- (gridS1$dem-min.dem)/(max.dem-min.dem)
		gridS1$slp <- gridS1$slp/max.slope
		gridS1$asp <- gridS1$asp/max.aspect
	}
	if(!is.null(gridS2)){
		gridS2$dem <- (gridS2$dem-min.dem)/(max.dem-min.dem)
		gridS2$slp <- gridS2$slp/max.slope
		gridS2$asp <- gridS2$asp/max.aspect
	}

	return(list(newgrid = gridS, coords.coarse = gridS1, coords.var = gridS2,
				id.coarse = idcoarse, id.var = idcoarse1))
}

##################################################################################
### create grid for stations data 
createGrid.StnData <- function(donne.stn, ijGrd, newgrid, min.stn, weighted = FALSE){
	ij <- !is.na(donne.stn$stn)
	donne.stn <- donne.stn[ij, , drop = FALSE]
	if(nrow(donne.stn) < min.stn) return(NULL)
	ijGrd <- ijGrd[ij]
	idx <- split(seq_along(ijGrd), ijGrd)

	if(any(sapply(idx, length) > 1)){
		w <- rep(1, length(ijGrd))
		if(weighted){
			idup <- duplicated(ijGrd) | duplicated(ijGrd, fromLast = TRUE)
			stn.grd <- newgrid@coords[ijGrd, ]
			dist <- 1/((stn.grd[idup, 1]-donne.stn$lon[idup])^2 + (stn.grd[idup, 2]-donne.stn$lat[idup])^2)
			dist[is.infinite(dist)] <- 2*max(dist[!is.infinite(dist)])
			w[idup] <- dist
		}
		val <- sapply(idx, function(j) sum(w[j]*donne.stn$stn[j])/sum(w[j]))
	}else val <- donne.stn$stn[unlist(idx)]

	ij <- as.numeric(names(idx))
	stng <- rep(NA, length(newgrid))
	stng[ij] <- val
	return(stng)
}

#################################################################################
## nx and ny for as.image
# x: diff(range( lon or lat ))

nx_ny_as.image <- function(x) round(x / (0.0167323 * x^0.9602))

##################################################################################
## same as fields::as.image
cdt.as.image <- function(pts.val, pts.xy, grid = NULL, nx = 64, ny = 64, weighted = FALSE){
	if(is.null(grid)){
		xlim <- range(pts.xy[, 1], na.rm = TRUE)
		ylim <- range(pts.xy[, 2], na.rm = TRUE)
		xlim <- xlim + diff(xlim)*c(-1, 1)*0.01
		ylim <- ylim + diff(ylim)*c(-1, 1)*0.01
		grid <- list(lon = seq(xlim[1], xlim[2], length.out = nx),
					 lat = seq(ylim[1], ylim[2], length.out = ny))
	}
	xy <- do.call(expand.grid, grid)
	ijGrd <- grid2pointINDEX(list(lon = pts.xy[, 1], lat = pts.xy[, 2]), grid)
	out <- list(x = grid$lon, y = grid$lat, z = matrix(NA, length(grid$lon), length(grid$lat)))

	ij <- !is.na(pts.val)
	pts.val <- pts.val[ij]
	if(length(pts.val) == 0) return(out)
	pts.xy <- pts.xy[ij, , drop = FALSE]
	ijGrd <- ijGrd[ij]
	idx <- split(seq_along(ijGrd), ijGrd)

	if(any(sapply(idx, length) > 1)){
		w <- rep(1, length(ijGrd))
		if(weighted){
			idup <- duplicated(ijGrd) | duplicated(ijGrd, fromLast = TRUE)
			stn.grd <- xy[ijGrd, ]
			dist <- 1/((stn.grd[idup, 1]-pts.xy[idup, 1])^2+(stn.grd[idup, 2]-pts.xy[idup, 2])^2)
			dist[is.infinite(dist)] <- 2*max(dist[!is.infinite(dist)])
			w[idup] <- dist
		}
		val <- sapply(idx, function(j) sum(w[j]*pts.val[j])/sum(w[j]))
	}else val <- pts.val[unlist(idx)]
	ij <- as.numeric(names(idx))
	out$z[ij] <- val
	return(out)
}

##################################################################################

### gstat block size
createBlock <- function(cellsize, fac = 0.5, len = 4){
	sDX <- cellsize[1]*fac
	dBX <- seq(-sDX, sDX, length.out = len)
	sDY <- cellsize[2]*fac
	dBY <- seq(-sDY, sDY, length.out = len)
	bGrd <- expand.grid(x = dBX, y = dBY)
	return(bGrd)
}

##################################################################################
## write ncdf,  merging
writeNC.merging <- function(mat, daty, tstep, grid.nc, dir2save, file.format, missval = -99)
{
	year <- substr(daty, 1, 4)
	month <- substr(daty, 5, 6)
	if(tstep == 'daily'){
		mrgfrmt <- sprintf(file.format, year, month, substr(daty, 7, 8))
	}else if(tstep%in%c('pentad', 'dekadal')){
		mrgfrmt <- sprintf(file.format, year, month, substr(daty, 7, 7))
	}else  mrgfrmt <- sprintf(file.format, year, month)

	mat[is.na(mat)] <- missval

	outfl <- file.path(dir2save, mrgfrmt)
	nc <- nc_create(outfl, grid.nc)
	ncvar_put(nc, grid.nc, mat)
	nc_close(nc)
}

#################################################################################
######################          STATIONS DATA          ##########################
#################################################################################
## get stn data  in the list (all open files)
## return CDT data format

getStnOpenData <- function(file.stnfl){
	jfile <- getIndex.AllOpenFiles(file.stnfl)
	donne <- NULL
	if(length(jfile) > 0){
		if(AllOpenFilesType[[jfile]] == "ascii") donne <- AllOpenFilesData[[jfile]][[2]]
	}
	return(donne)
}

getStnOpenDataInfo <- function(file.stnfl){
	jfile <- getIndex.AllOpenFiles(file.stnfl)
	info <- NULL
	if(length(jfile) > 0){
		if(AllOpenFilesType[[jfile]] == "ascii") info <- AllOpenFilesData[[jfile]][c(1, 3:4)]
	}
	return(info)
}

#################################################################################

getCDTdataAndDisplayMsg <- function(donne, period){
	if(is.null(donne)) return(NULL)
	donne <- splitCDTData(donne, period)
	if(is.null(donne)) return(NULL)
	if(!is.null(donne$duplicated.coords)){
		tmp <- as.matrix(donne$duplicated.coords)
		tmp0 <- paste(dimnames(tmp)[[2]], collapse = '\t')
		for(i in 1:nrow(tmp)) tmp0 <- paste(tmp0, paste(tmp[i, ], collapse = '\t'), sep = '\n')
		InsertMessagesTxt(main.txt.out, 'Duplicated coordinates', format = TRUE)
		InsertMessagesTxt(main.txt.out, tmp0)
	}
	if(!is.null(donne$missing.coords)){
		tmp <- as.matrix(donne$missing.coords)
		tmp0 <- paste(dimnames(tmp)[[2]], collapse = '\t')
		for(i in 1:nrow(tmp)) tmp0 <- paste(tmp0, paste(tmp[i, ], collapse = '\t'), sep = '\n')
		InsertMessagesTxt(main.txt.out, 'Missing coordinates', format = TRUE)
		InsertMessagesTxt(main.txt.out, tmp0)
	}
	if(!is.null(donne$duplicated.dates)){
		InsertMessagesTxt(main.txt.out, 'Duplicated dates', format = TRUE)
		InsertMessagesTxt(main.txt.out, paste(donne$duplicated.dates$date, collapse = ' '))
	}
	if(!is.null(donne$wrong.dates)){
		InsertMessagesTxt(main.txt.out, 'Wrong dates format', format = TRUE)
		InsertMessagesTxt(main.txt.out, paste(donne$wrong.dates$date, collapse = ' '))
	}
	if(!is.null(donne$missing.dates)){
		InsertMessagesTxt(main.txt.out, 'Missing dates', format = TRUE)
		InsertMessagesTxt(main.txt.out, paste(donne$missing.dates$date, collapse = ' '))
	}
	return(donne)
}

#################################################################################

getCDTTSdataAndDisplayMsg <- function(donne, period, filefrmt, datefrmt){
	if(is.null(donne)) return(NULL)
	donne <- splitTsData(donne, period, filefrmt, datefrmt)
	if(is.null(donne)) return(NULL)
	if(!is.null(donne$duplicated.dates)){
		InsertMessagesTxt(main.txt.out, 'Duplicated dates', format = TRUE)
		InsertMessagesTxt(main.txt.out, paste(donne$duplicated.dates$date, collapse = ' '))
	}
	if(!is.null(donne$wrong.dates)){
		InsertMessagesTxt(main.txt.out, 'Wrong dates format', format = TRUE)
		InsertMessagesTxt(main.txt.out, paste(donne$wrong.dates$date, collapse = ' '))
	}
	if(!is.null(donne$missing.dates)){
		InsertMessagesTxt(main.txt.out, 'Missing dates', format = TRUE)
		InsertMessagesTxt(main.txt.out, paste(donne$missing.dates$date, collapse = ' '))
	}
	return(donne)
}

#################################################################################
## get CDT data  in the list (all open files)
## return split data

getCDTdata <- function(file.stnfl, file.period){
	donne <- getStnOpenData(file.stnfl)
	if(is.null(donne)) return(NULL)
	if(tclvalue(file.period) == 'Daily data') freqData <- 'daily'
	if(tclvalue(file.period) == 'Dekadal data') freqData <- 'dekadal'
	if(tclvalue(file.period) == 'Monthly data') freqData <- 'monthly'
	donne <- splitCDTData(donne, freqData)
	if(is.null(donne)) return(NULL)
	lon <- donne$lon
	lat <- donne$lat
	id <- donne$id
	elv <- donne$elv
	dates <- donne$dates
	donne <- donne$data
	return(list(freq = freqData, lon = lon, lat = lat, id = id, elv = elv, date = dates, data = donne))
}	

#################################################################################
## get CDT data for one date
## donne: output  getCDTdata

getCDTdata1Date <- function(donne, yrs, mon, day){
	if(is.null(donne)) return(NULL)
	freqData <- donne$freq
	lon <- donne$lon
	lat <- donne$lat
	id <- donne$id
	elv <- donne$elv
	dates <- donne$date
	donne <- donne$data

	daty <- try(as.Date(paste(as.numeric(yrs), as.numeric(mon), as.numeric(day), sep = '-')), silent = TRUE)
	if(inherits(daty, "try-error")){
		InsertMessagesTxt(main.txt.out, "Date format invalid", format = TRUE)
		return(NULL)
	}
	if(freqData == 'daily') daty <- format(daty, '%Y%m%d')
	if(freqData == 'dekadal') daty <- paste0(format(daty, '%Y%m'), as.numeric(format(daty, '%d')))
	if(freqData == 'monthly') daty <- format(daty, '%Y%m')
	idate <- match(daty, dates)
	if(is.na(idate)) return(NULL)
	zval <- as.numeric(donne[idate,])
	return(list(date = daty, lon = lon, lat = lat, id = id, z = zval, elv = elv))
}


#################################################################################
########################          DEM DATA          #############################
#################################################################################
## get DEM data  in the list (all open files)
## return $lon $lat $dem

getDemOpenData <- function(file.grddem, convertNeg2NA = TRUE){
	jfile <- getIndex.AllOpenFiles(file.grddem)
	dem <- NULL
	if(length(jfile) > 0){
		if(AllOpenFilesType[[jfile]] == "netcdf"){
			fdem <- AllOpenFilesData[[jfile]][[2]]
			demv <- fdem$value
			if(convertNeg2NA) demv[demv < 0] <- NA
			dem <- list(lon = fdem$x, lat = fdem$y, dem = demv)
		}
	}
	return(dem)
}

#################################################################################
## get DEM data  in the list (all open files)
## return $lon $lat $demGrd $demMat

getDemOpenDataSPDF <- function(file.grddem){
	jncdf <- getIndex.AllOpenFiles(file.grddem)
	demlist <- NULL
	if(length(jncdf) > 0){
		if(AllOpenFilesType[[jncdf]] == "netcdf"){
			fdem <- AllOpenFilesData[[jncdf]][[2]]
			dem <- fdem$value
			demMat <- dem
			dem[dem < 0] <- 0
			dem.coord <- data.frame(expand.grid(lon = fdem$x, lat = fdem$y))
			coordinates(dem.coord) <- ~lon+lat
			demdf <- data.frame(dem = c(dem))
			demdf <- SpatialPointsDataFrame(coords = dem.coord, data = demdf, proj4string = CRS(as.character(NA)))
			demlist <- list(lon = fdem$x, lat = fdem$y, demGrd = demdf, demMat = demMat)
		}
	}
	return(demlist)
}

#################################################################################
##get DEM at stations locations (interpolation leftCmd)

getDEMatStationsLoc <- function(donne, dem){
	crdStn <- data.frame(lon = donne$lon, lat = donne$lat)
	coordinates(crdStn) <- c('lon', 'lat')
	demgrd <- expand.grid(lon = dem$x, lat = dem$y)
	coordinates(demgrd) <- c('lon', 'lat')
	demgrd <- SpatialPixels(points = demgrd, tolerance = sqrt(sqrt(.Machine$double.eps)))
	ijGrd <- unname(over(crdStn, geometry(demgrd)))
	sdem <- rep(NA, length(crdStn))
	inNA <- which(!is.na(ijGrd))
	sdem[inNA] <- dem$value[ijGrd[inNA]]
	return(sdem)
}

#################################################################################
##get DEM at a new grid 

getDEMatNewGrid <- function(newgrid, dem){
	newgrid <- as.data.frame(newgrid)
	crdPts <- data.frame(lon = newgrid$lon, lat = newgrid$lat)
	coordinates(crdPts) <- c('lon', 'lat')
	demgrd <- expand.grid(lon = dem$x, lat = dem$y)
	coordinates(demgrd) <- c('lon', 'lat')
	demgrd <- SpatialPixels(points = demgrd, tolerance = sqrt(sqrt(.Machine$double.eps)))
	ijGrd <- unname(over(crdPts, geometry(demgrd)))
	sdem <- rep(NA, length(crdPts))
	inNA <- which(!is.na(ijGrd))
	sdem[inNA] <- dem$value[ijGrd[inNA]]
	return(sdem)
}

#################################################################################
##regrid DEM 

regridDEMFun <- function(demObj, newgrd, regrid = c('BLW', 'IDW', 'RASTER'), ...){
	if(regrid[1] == 'BLW'){
		zdem <- demObj$demMat
		zdem[zdem < 0] <- 0
		demObj <- list(x = demObj$lon, y = demObj$lat, z = zdem)
		demNew <- list(x = newgrd$lon, y = newgrd$lat)
		newObj <- interp.surface.grid(demObj, demNew)
		dem <- newObj$z
		dem[dem < 0] <- 0
		dem <- c(dem)
	}
	if(regrid[1] == 'IDW'){
		newgrid <- defSpatialPixels(newgrd)
		dem.grd <- krige(formula = dem ~ 1, locations = demObj$demGrd, newdata = newgrid, ..., debug.level = 0)
		dem <- ifelse(dem.grd@data$var1.pred < 0, 0, dem.grd@data$var1.pred)
	}
	if(regrid[1] == 'RASTER'){
		require(raster)
		zdem <- demObj$demMat
		zdem[zdem < 0] <- 0
		rasterdem <- raster(list(x = demObj$lon, y = demObj$lat, z = zdem))
		newgrid <- raster(list(x = newgrd$lon, y = newgrd$lat, z = matrix(0, nrow = length(newgrd$lon), ncol = length(newgrd$lat))))
		dem <- resample(rasterdem, newgrid, ...)
		dem <- mask(dem, newgrid)
		dem<-c(t(apply(as.matrix(dem), 2, rev)))
		dem[dem < 0] <- 0
	}	
	return(dem)
}


#################################################################################
########################          SHP DATA          #############################
#################################################################################
## get shp file  in the list (all open files)
##return [[1]] name [[2]] shp [[3]] path

getShpOpenData <- function(shp){
	jfile <- getIndex.AllOpenFiles(shp)
	shpf <- NULL
	if(length(jfile) > 0){
		if(AllOpenFilesType[[jfile]] == "shp") shpf <- AllOpenFilesData[[jfile]]
	}
	return(shpf)
}


#################################################################################
########################          NetCDF DATA          ##########################
#################################################################################
## get NetCDF data  in the list (all open files)
## return $lon $lat $val

getNcdfOpenData <- function(file.netcdf){
	jfile <- getIndex.AllOpenFiles(file.netcdf)
	nc <- NULL
	if(length(jfile) > 0){
		if(AllOpenFilesType[[jfile]] == "netcdf") nc <- AllOpenFilesData[[jfile]]
	}
	return(nc)
}

#################################################################################
## get RFE sample data  in the list (all open files)
## return $lon $lat $val $rfeGrd $rfeVarid $rfeILon $rfeILat $irevlat

getRFESampleData <- function(file.netcdf){
	jfile <- getIndex.AllOpenFiles(file.netcdf)
	rfelist <- NULL
	if(length(jfile) > 0){
		if(AllOpenFilesType[[jfile]] == "netcdf"){
			ncrfe <- AllOpenFilesData[[jfile]][[2]]
			rfe.coord <- data.frame(expand.grid(lon = ncrfe$x, lat = ncrfe$y))
			coordinates(rfe.coord) <- ~lon+lat
			rfelist <- list(lon = ncrfe$x, lat = ncrfe$y, rfeGrd = rfe.coord, rfeVarid = ncrfe$varid,
							 rfeILon = ncrfe$ilon, rfeILat = ncrfe$ilat, irevlat = ncrfe$irevlat)
		}
	}
	return(rfelist)
}

#################################################################################
## get NetCDF data plot merging outputs menu

getNcdfData2Plot <- function(dataNCDF, freqData, yrs, mon, day, ncOrder = c(1, 2)){
	ilon <- ncOrder[1]
	ilat <- ncOrder[2]
	if(freqData == 'Monthly data') daty <- try(format(as.Date(paste(as.numeric(yrs), as.numeric(mon), 15, sep = '-')), '%Y%m%d'), silent = TRUE)
	else daty <- try(format(as.Date(paste(as.numeric(yrs), as.numeric(mon), as.numeric(day), sep = '-')), '%Y%m%d'), silent = TRUE)
	if(inherits(daty, "try-error") | is.na(daty)){
		InsertMessagesTxt(main.txt.out, paste("Date format invalid", tclvalue(dataNCDF[[2]])), format = TRUE)
		return(NULL)
	}

	if(freqData == 'Daily data') filelpath <- file.path(tclvalue(dataNCDF[[1]]), sprintf(tclvalue(dataNCDF[[2]]), substr(daty, 1, 4), substr(daty, 5, 6), substr(daty, 7, 8)))
	if(freqData == 'Dekadal data') filelpath <- file.path(tclvalue(dataNCDF[[1]]), sprintf(tclvalue(dataNCDF[[2]]), substr(daty, 1, 4), substr(daty, 5, 6), substr(daty, 8, 8)))
	if(freqData == 'Monthly data') filelpath <- file.path(tclvalue(dataNCDF[[1]]), sprintf(tclvalue(dataNCDF[[2]]), substr(daty, 1, 4), substr(daty, 5, 6)))
	if(!file.exists(filelpath)){
		InsertMessagesTxt(main.txt.out, paste(filelpath, "doesn't exist"), format = TRUE)
		return(NULL)
	}

	nc <- nc_open(filelpath)
	lon <- nc$dim[[ilon]]$vals
	lat <- nc$dim[[ilat]]$vals
	val <- ncvar_get(nc, varid = nc$var[[1]]$name)
	nc_close(nc)
	return(list(x = lon, y = lat, value = val))
}

#################################################################################
## get RFE data plot qc spatial check result

getSatelliteData <- function(dir_ncdf, ff_ncdf, spchkQcDateVal){
	if(!is.null(ReturnExecResults)){
		spchkoutdates <- isSpatialCheckOk()
		if(nrow(spchkoutdates) != 0){
			dataNCDF <- list(dir_ncdf, ff_ncdf)
			freqData <- ifelse(ReturnExecResults$period == 'daily', 'Daily data',
						ifelse(ReturnExecResults$period == 'dekadal', 'Dekadal data', 'Monthly data'))
			spdaty <- tclvalue(spchkQcDateVal)
			year <- as.numeric(substr(spdaty, 1, 4))
			mon <- as.numeric(substr(spdaty, 5, 6))
			day <- as.numeric(substr(spdaty, 7, 8))
			rfedat <- getNcdfData2Plot(dataNCDF, freqData, year, mon, day)
			return(rfedat)
		}else{
			InsertMessagesTxt(main.txt.out, 'No spatial check performed', format = TRUE)
			return(NULL)
		}	
	}else{
		InsertMessagesTxt(main.txt.out, 'There is no qc-results outputs', format = TRUE)
		return(NULL)
	}
}

#################################################################################
## list of available ncdf files

ncFilesInfo <- function(freqData, start.date, end.date, months, ncDir, ncFileFormat, error.msg){
	if(freqData == 'daily'){
		dates <- format(seq(start.date, end.date, 'day'), '%Y%m%d')
		ncDataFiles <- file.path(ncDir, sprintf(ncFileFormat, substr(dates, 1, 4),
										substr(dates, 5, 6), substr(dates, 7, 8)))
	}
	if(freqData == 'pentad'){
		dates <- seq(start.date,  end.date, 'day')
		dates <- paste0(format(dates[which(as.numeric(format(dates, '%d')) <= 6)], '%Y%m'),
					as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 6)], '%d')))
		ncDataFiles <- file.path(ncDir, sprintf(ncFileFormat, substr(dates, 1, 4),
										substr(dates, 5, 6), substr(dates, 7, 7)))
	}
	if(freqData == 'dekadal'){
		dates <- seq(start.date,  end.date, 'day')
		dates <- paste0(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%Y%m'),
					as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%d')))
		ncDataFiles <- file.path(ncDir, sprintf(ncFileFormat, substr(dates, 1, 4),
										substr(dates, 5, 6), substr(dates, 7, 7)))
	}
	if(freqData == 'monthly'){
		dates <- format(seq(start.date, end.date, 'month'), '%Y%m')
		ncDataFiles <- file.path(ncDir, sprintf(ncFileFormat, substr(dates, 1, 4),
												substr(dates, 5, 6)))
	}
	months.dates <- as(substr(dates, 5, 6), 'numeric')
	imo <- months.dates%in%months
	dates <- dates[imo]
	ncDataFiles <- ncDataFiles[imo]

	existFl <- unlist(lapply(ncDataFiles, file.exists))
	if(!any(existFl)){
		InsertMessagesTxt(main.txt.out, error.msg, format = TRUE)
		return(NULL)
	}
	return(list(dates = dates, nc.files = ncDataFiles, exist = existFl))
}

#################################################################################
## read ncdf files
## input arguments 
## read.ncdf.parms <- list(
## ncfiles = list(freqData , start.date, end.date, ncDir, ncFileFormat),
## ncinfo = list(xo, yo, varid), msg = list(start, end), errmsg = errmsg)

read.NetCDF.Data <- function(read.ncdf.parms){
	InsertMessagesTxt(main.txt.out, read.ncdf.parms$msg$start)

	ncInfo <- ncFilesInfo(read.ncdf.parms$ncfiles$freqData, read.ncdf.parms$ncfiles$start.date,
							read.ncdf.parms$ncfiles$end.date, read.ncdf.parms$ncfiles$months,
							read.ncdf.parms$ncfiles$ncDir, read.ncdf.parms$ncfiles$ncFileFormat,
							read.ncdf.parms$errmsg)
	if(is.null(ncInfo)) return(NULL)

	is.parallel <- doparallel(length(which(ncInfo$exist)) >= 180)
	`%parLoop%` <- is.parallel$dofun

	nc <- nc_open(ncInfo$nc.files[which(ncInfo$exist)[1]])
	lon <- nc$var[[read.ncdf.parms$ncinfo$varid]]$dim[[read.ncdf.parms$ncinfo$xo]]$vals
	lat <- nc$var[[read.ncdf.parms$ncinfo$varid]]$dim[[read.ncdf.parms$ncinfo$yo]]$vals
	nc_close(nc)

	xo <- order(lon)
	lon <- lon[xo]
	yo <- order(lat)
	lat <- lat[yo]

	packages <- c('ncdf4')
	toExports <- c('ncInfo', 'read.ncdf.parms')
	ncdata <- foreach(jj = seq_along(ncInfo$nc.files), .packages = packages, .export = toExports) %parLoop% {
		if(ncInfo$exist[jj]){
			nc <- try(nc_open(ncInfo$nc.files[jj]), silent = TRUE)
			if(inherits(nc, "try-error")) return(NULL)
			xvar <- ncvar_get(nc, varid = read.ncdf.parms$ncinfo$varid)
			nc_close(nc)
			if(nlon != nrow(xvar) | nlat != ncol(xvar)) return(NULL)
			xvar <- xvar[xo, yo]
			if(read.ncdf.parms$ncinfo$yo == 1){
				xvar <- matrix(c(xvar), nrow = length(lon), ncol = length(lat), byrow = TRUE)
			}
		}else xvar <- NULL
		xvar
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	ret <- list(lon = lon, lat = lat, dates = ncInfo$dates, data = ncdata)
	InsertMessagesTxt(main.txt.out, read.ncdf.parms$msg$end)
	return(ret)
}


#################################################################################
## read ncdf files
## input arguments 
## extact at points list.lonlat.pts <- list(lon = lon.pts, lat = lat.pts)

read.NetCDF.Data2Points <- function(read.ncdf.parms, list.lonlat.pts){
	InsertMessagesTxt(main.txt.out, read.ncdf.parms$msg$start)

	ncInfo <- ncFilesInfo(read.ncdf.parms$ncfiles$freqData, read.ncdf.parms$ncfiles$start.date,
							read.ncdf.parms$ncfiles$end.date, read.ncdf.parms$ncfiles$months,
							read.ncdf.parms$ncfiles$ncDir, read.ncdf.parms$ncfiles$ncFileFormat,
							read.ncdf.parms$errmsg)
	if(is.null(ncInfo)) return(NULL)

	is.parallel <- doparallel(length(which(ncInfo$exist)) >= 180)
	`%parLoop%` <- is.parallel$dofun

	nc <- nc_open(ncInfo$nc.files[which(ncInfo$exist)[1]])
	lon <- nc$var[[read.ncdf.parms$ncinfo$varid]]$dim[[read.ncdf.parms$ncinfo$xo]]$vals
	lat <- nc$var[[read.ncdf.parms$ncinfo$varid]]$dim[[read.ncdf.parms$ncinfo$yo]]$vals
	nc_close(nc)

	xo <- order(lon)
	lon <- lon[xo]
	yo <- order(lat)
	lat <- lat[yo]
	ijx <- grid2pointINDEX(list.lonlat.pts, list(lon = lon, lat = lat))

	ncdata <- foreach(jj = seq_along(ncInfo$nc.files), .packages = 'ncdf4') %parLoop% {
		if(ncInfo$exist[jj]){
			nc <- try(nc_open(ncInfo$nc.files[jj]), silent = TRUE)
			if(inherits(nc, "try-error")) return(NULL)
			xvar <- ncvar_get(nc, varid = read.ncdf.parms$ncinfo$varid)
			nc_close(nc)
			if(nlon != nrow(xvar) | nlat != ncol(xvar)) return(NULL)
			xvar <- xvar[xo, yo]
			if(read.ncdf.parms$ncinfo$yo == 1){
				xvar <- matrix(c(xvar), nrow = length(lon), ncol = length(lat), byrow = TRUE)
			}
			xvar <- xvar[ijx]
		}else xvar <- NULL
		xvar
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	ret <- list(dates = ncInfo$dates, data = ncdata, lon = lon, lat = lat,
				lon.pts = list.lonlat.pts$lon, lat.pts = list.lonlat.pts$lat)
	InsertMessagesTxt(main.txt.out, read.ncdf.parms$msg$end)
	return(ret)
}

readNetCDFData2Points <- function(ncInfo, list.lonlat.pts, msg){
	InsertMessagesTxt(main.txt.out, msg$start)
	nc <- nc_open(ncInfo$nc.files[which(ncInfo$exist)[1]])
	lon <- nc$var[[ncInfo$ncinfo$varid]]$dim[[ncInfo$ncinfo$xo]]$vals
	lat <- nc$var[[ncInfo$ncinfo$varid]]$dim[[ncInfo$ncinfo$yo]]$vals
	nc_close(nc)

	xo <- order(lon)
	lon <- lon[xo]
	yo <- order(lat)
	lat <- lat[yo]
	nlon <- length(lon)
	nlat <- length(lat)
	ijx <- grid2pointINDEX(list.lonlat.pts, list(lon = lon, lat = lat))

	is.parallel <- doparallel(length(which(ncInfo$exist)) >= 180)
	`%parLoop%` <- is.parallel$dofun

	ncdata <- foreach(jj = seq_along(ncInfo$nc.files), .packages = 'ncdf4') %parLoop% {
		if(ncInfo$exist[jj]){
			nc <- try(nc_open(ncInfo$nc.files[jj]), silent = TRUE)
			if(inherits(nc, "try-error")) return(NULL)
			xvar <- ncvar_get(nc, varid = ncInfo$ncinfo$varid)
			nc_close(nc)
			if(nlon != nrow(xvar) | nlat != ncol(xvar)) return(NULL)
			xvar <- xvar[xo, yo]
			if(ncInfo$ncinfo$yo == 1){
				xvar <- matrix(c(xvar), nrow = nlon, ncol = nlat, byrow = TRUE)
			}
			xvar <- xvar[ijx]
		}else xvar <- NULL
		xvar
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	inull <- sapply(ncdata, is.null)
	ncdata <- do.call(rbind, ncdata)
	dates <- ncInfo$dates[!inull]
	ret <- list(dates = dates, data = ncdata)
	InsertMessagesTxt(main.txt.out, msg$end)
	return(ret)
}

#################################################################################

## Test if the elements of two vectors are equals
isEquals <- function(x, y){
	ix <- (x == y) | (is.na(x) & is.na(y))
	ix[is.na(ix)] <- FALSE
	ix
}

## Test if two vectors are equals
isEqual <- function(x, y) !any(!isEquals(x, y))



#################################################################################

# smooth.matrix <- function(mat, ns){
# 	matrix.shift <- function(n, shift){
# 		mshift <- matrix(NA, ncol = n, nrow = 2*shift+1)
# 		mshift[shift+1, ] <- seq(n)
# 		for(j in 1:shift){
# 			is <- shift-(j-1)
# 			mshift[j, ] <- c(tail(seq(n), -is), rep(NA, is))
# 			mshift[2*(shift+1)-j, ] <- c(rep(NA, is), head(seq(n), -is))
# 		}
# 		mshift
# 	}
# 	icol <- matrix.shift(ncol(mat), ns)
# 	matCol <- lapply(1:nrow(icol), function(j) mat[, icol[j, ]])
# 	irow <- matrix.shift(nrow(mat), ns)
# 	matRow <- lapply(1:nrow(irow), function(j) mat[irow[j, ], ])
# 	matEns <- c(matCol, matRow)
# 	matEns <- simplify2array(matEns)
# 	res <- apply(matEns, 1:2, mean, na.rm = TRUE)
# 	res[is.nan(res)] <- NA
# 	return(res)
# }

smooth.matrix <- function(mat, ns){
	M <- matrix(NA, nrow(mat)+2*ns, ncol(mat)+2*ns)
	sqC <- (ns+1):(ncol(M)-ns)
	sqR <- (ns+1):(nrow(M)-ns)
	M[sqR, sqC] <- mat
	sqN <- -ns:ns
	for(j in sqC)
		for(i in sqR)
			mat[i-ns, j-ns] <- mean(M[i+sqN, j+sqN], na.rm = TRUE)
	mat[is.nan(mat)] <- NA
	return(mat)
}

#################################################################################


Conv2Type1 <- function(mat1, mat2, inverse = 'col'){
    demimat <- nrow(mat2) %/% 2
    nout <- nrow(mat2) + 2
    Out <- matrix(NA, nout, nout)
    inmat <- matrix(0, nout, nout)
    for(i in 1:dim(mat1)[1]){
        for(j in 1:dim(mat1)[2]){
            inmat[demimat + i, demimat + j] <- mat1[i, j]
        }
    }
    for(i in seq(nout)) {
        for(j in seq(nout)) {
            ii <- (i-demimat):(i+demimat)
            jj <- (j-demimat):(j+demimat)
            ii[ii < 1] <- 1
            ii[ii > nout] <- nout
            jj[jj < 1] <- 1
            jj[jj > nout] <- nout
            Out[i, j] <- sum(mat2 * inmat[ii, jj])
        }
    }
    if(inverse == 'col') Out <- Out[, nout:1]
    if(inverse == 'row') Out <- Out[nout:1, ]
    return(Out)
}

Conv2Type2 <- function(mat, Kernel) { 
 	## equivalent raster::focal
 	halfkersize <- nrow(Kernel)%/%2
	for(j in 1:halfkersize) mat <- cbind(mat[, 1], mat, mat[, ncol(mat)])
	for(j in 1:halfkersize) mat <- rbind(mat[1, ], mat, mat[nrow(mat), ])
 	Out <- mat
 	Out[] <- NA
 	ii0 <- (1+halfkersize):(nrow(mat)-halfkersize)
 	jj0 <- (1+halfkersize):(ncol(mat)-halfkersize)
    for(i in ii0){
        for(j in jj0){
            ii <- (i-halfkersize):(i+halfkersize)
            jj <- (j-halfkersize):(j+halfkersize)
            Out[i, j] <- sum(Kernel * mat[ii, jj])
        }
    }
    Out <- Out[ii0, jj0]
    return(Out)
}


Sobel.Filter <- function(BaseVector, KernelVector, k = 3, direction = "X"){
	if(!(is.vector(BaseVector) & is.numeric(BaseVector))){
		stop("BaseVector must be a numeric vector.\n")
	}
	if(!(is.vector(KernelVector) & is.numeric(KernelVector))){
		stop("KernelVector must be a numeric vector.\n")
	}
	if(k == 1 | k%%2 != 1) stop("k must be an odd integer and greater than 1.\n")
	if(direction == "X"){
		sobel <- (BaseVector %*% t(KernelVector))/8
	}else if(direction == "Y"){
		sobel <- (KernelVector %*% t(BaseVector))/8
	}else{
		stop('direction must be "X" or "Y"\n')
	}
	BaseMatrix <- BaseVector %*% t(BaseVector)
	if(k > 3){
		k <- k%/%2-1
		inv <- if(direction == "X") 'col' else 'row'
		for(i in 1:k) sobel <- Conv2Type1(BaseMatrix, sobel, inverse = inv)/16
	}
	return(sobel)
}

slope.aspect1 <- function(mat, xres, yres, Gx = NULL, Gy = NULL, smoothing = 1){
	mat <- data.matrix(mat)
	if(is.null(Gx) | is.null(Gy)){
		Gx <- Sobel.Filter(c(1, 2, 1), c(-1, 0, 1), k = 3, direction = "X")
		Gy <- Sobel.Filter(c(1, 2, 1), c(1, 0, -1), k = 3, direction = "Y")
	}
	x.dir <- Conv2Type2(mat, Gx)/xres
	y.dir <- Conv2Type2(mat, Gy)/yres
	##Add Tangential curvature
	##Add Profile curvature
	# slope <- atan(sqrt(x.dir^2 + y.dir^2)/smoothing)
	# aspect <- 180 - atan2(y.dir, x.dir) + 90 * (x.dir/abs(x.dir))

	slope <- (180/pi) * atan(sqrt(x.dir^2 + y.dir^2)/smoothing)
	aspect <- 180 - (180/pi) * atan(y.dir/x.dir) + 90 * (x.dir/abs(x.dir))
	aspect[slope == 0] <- 0
	return(list(slope = slope, aspect = aspect))
}


slope.aspect <- function(mat, xres, yres, filter = "sobel", smoothing = 1){
	mat <- data.matrix(mat)
	ras <- raster(mat)
	# BaseVector <- c(1, 1, 1)
	# KernelVector <- c(-1, 0, 1)
	if(filter == "evans"){
		#Prewitt
		Gx <- c(1, 1, 1) %*% t(c(-1, 0, 1))/6
		Gy <- c(1, 0, -1) %*% t(c(1, 1, 1))/6
		Gx2 <- c(1, 1, 1) %*% t(c(1, -2, 1))/3
		Gy2 <- c(1, -2, 1) %*% t(c(1, 1, 1))/3
		Gxy <- c(-1, 0, 1) %*% t(c(1, 0, -1))/4
	}

	if(filter == "shary"){
		#Prewitt
		Gx <- c(1, 1, 1) %*% t(c(-1, 0, 1))/6
		Gy <- c(1, 0, -1) %*% t(c(1, 1, 1))/6
		Gx2 <- c(1, 3, 1) %*% t(c(1, -2, 1))/5
		Gy2 <- c(1, -2, 1) %*% t(c(1, 3, 1))/5
		Gxy <- c(-1, 0, 1) %*% t(c(1, 0, -1))/4
	}

	if(filter == "zev.tho"){
		Gx <- c(0, 1, 0) %*% t(c(-1, 0, 1))/2
		Gy <- c(1, 0, -1) %*% t(c(0, 1, 0))/2
		Gx2 <- c(0, 1, 0) %*% t(c(1, -2, 1))/2
		Gy2 <- c(1, -2, 1) %*% t(c(0, 1, 0))/2
		Gxy <- c(-1, 0, 1) %*% t(c(1, 0, -1))/4
	}

	if(filter == "moore"){
		Gx <- c(0, 1, 0) %*% t(c(-1, 0, 1))/2
		Gy <- c(1, 0, -1) %*% t(c(0, 1, 0))/2
		Gx2 <- c(0, 1, 0) %*% t(c(1, -2, 1))/1
		Gy2 <- c(1, -2, 1) %*% t(c(0, 1, 0))/1
		Gxy <- c(-1, 0, 1) %*% t(c(1, 0, -1))/4
	}

	if(filter == "sobel"){
		#Sobel
		Gx <- c(1, 2, 1) %*% t(c(-1, 0, 1))/8
		Gy <- c(1, 0, -1) %*% t(c(1, 2, 1))/8
		Gx2 <- c(1, 2, 1) %*% t(c(1, -2, 1))/4
		Gy2 <- c(1, -2, 1) %*% t(c(1, 2, 1))/4
		Gxy <- c(-1, 0, 1) %*% t(c(1, 0, -1))/4
	}

	x.dir <- focal(ras, w = Gx, pad = TRUE, padValue = 0)/xres
	y.dir <- focal(ras, w = Gy, pad = TRUE, padValue = 0)/yres
	x2.dir <- focal(ras, w = Gx2, pad = TRUE, padValue = 0)/xres^2
	y2.dir <- focal(ras, w = Gy2, pad = TRUE, padValue = 0)/yres^2
	xy.dir <- focal(ras, w = Gxy, pad = TRUE, padValue = 0)/(xres*yres)

	slope <- atan(sqrt(x.dir^2 + y.dir^2)/smoothing)
	aspect <- 180 - atan2(y.dir, x.dir) + 90 * (x.dir/abs(x.dir))
	plan.curvature <- -1*(y.dir^2*x2.dir - 2*x.dir*y.dir*xy.dir + x.dir^2*y2.dir)/((x.dir^2 + y.dir^2)*sqrt(1 + x.dir^2 + y.dir^2))
	prof.curvature <- -1*(x.dir^2*x2.dir + 2*x.dir*y.dir*xy.dir + y.dir^2*y2.dir)/((x.dir^2 + y.dir^2)*sqrt(1 + x.dir^2 + y.dir^2)^3)

	ret <- list(slope = as.matrix(slope), aspect = as.matrix(aspect),
				tan.curvature = as.matrix(plan.curvature), prof.curvature = as.matrix(prof.curvature))
	return(ret)
}

raster.slope.aspect <- function(dem){
	dem <- raster(dem)
	slope <- raster::terrain(dem, opt = "slope", unit = 'degrees', neighbors = 8) 
	aspect <- raster::terrain(dem, opt = "aspect", unit = 'degrees', neighbors = 8) 
	slope <- t(as.matrix(slope))
	slope <- slope[, rev(seq(ncol(slope)))]
	aspect <- t(as.matrix(aspect))
	aspect <- aspect[, rev(seq(ncol(aspect)))]
	list(slope = slope, aspect = aspect)
}

#################################################################################
### Merging Method combination
generateCombnation <- function(){
	DAS <- expand.grid(dem = c(FALSE, TRUE), slope = c(FALSE, TRUE), aspect = c(FALSE, TRUE),
						lon = c(FALSE, TRUE), lat = c(FALSE, TRUE))
	aux.var <- apply(DAS, 1, function(j){
		x <- c('D', 'S', 'A', 'Lo', 'La')[j]
		if(length(x) > 0) paste(x, collapse = '')
		else 'noD'
	})
	auxdf <- data.frame(n = 1:32, l = aux.var, DAS, stringsAsFactors = FALSE)
	return(auxdf)
}

merging.combination <- function(){
	DAS <- expand.grid(dem = c(FALSE, TRUE), slope = c(FALSE, TRUE), aspect = c(FALSE, TRUE),
						lon = c(FALSE, TRUE), lat = c(FALSE, TRUE))
	aux.var <- apply(DAS, 1, function(j){
		x <- c('D', 'S', 'A', 'Lo', 'La')[j]
		if(length(x) > 0) paste(x, collapse = '')
		else 'noD'
	})
	auxdf <- data.frame(n = 1:32, l = aux.var, DAS, stringsAsFactors = FALSE)

	### Bias/adj comb
	BScomb <- expand.grid(aux = aux.var, interp = c('NN', 'IDW', 'OK'), Bias = c('QM', 'MBVar', 'MBMon'))
	BScomb <- paste(as.character(BScomb$Bias), as.character(BScomb$interp), as.character(BScomb$aux), sep = '_')

	RKcomb <- expand.grid(aux = aux.var, interp = c('IDW', 'OK'), adjdir = BScomb)
	### Regression kriging no aux var pour spatial trend
	RKcomb1 <- paste(as.character(RKcomb$adjdir), paste("RK.sp.trend", "noD", sep = '_'),
				paste('Reg.Kriging', as.character(RKcomb$interp), as.character(RKcomb$aux), sep = '_'), sep = '-')
	### Regression kriging avec aux var pour spatial trend
	RKcomb2 <- paste(as.character(RKcomb$adjdir), paste("RK.sp.trend", as.character(RKcomb$aux), sep = '_'),
				paste('Reg.Kriging', as.character(RKcomb$interp), as.character(RKcomb$aux), sep = '_'), sep = '-')

	### Spatio-Temp LM coef comb
	SPLMcomb <- expand.grid(aux = aux.var, interp = c('NN', 'IDW', 'OK'), adjdir = BScomb)
	SPLMCoef <- paste(SPLMcomb$adjdir, paste(SPLMcomb$interp, SPLMcomb$aux, sep = '_'), sep = '-')

	#####
	SPLMcomb <- expand.grid(aux = aux.var, interp = c('IDW', 'OK'), tmp = SPLMCoef)
	SPLMcomb <- paste(SPLMcomb$tmp, paste('Saptio.Tempo.LM', SPLMcomb$interp, SPLMcomb$aux, sep = '_'), sep = '-')

	combMering <- c(RKcomb1, RKcomb2, SPLMcomb)
	combMering <- combMering[!duplicated(combMering)]
	mthd <- do.call(rbind, lapply(strsplit(combMering, "-"), function(x) do.call(c, strsplit(x, "_"))))
	mthd <- data.frame(mthd, stringsAsFactors = FALSE)
	names(mthd) <- c('bias.method', 'bias.interp', 'bias.auxvar', 'sptrend.interp', 'sptrend.auxvar', 'mrg.method', 'mrg.interp', 'mrg.auxvar')
	return(mthd)
}

LMCoef.combination <- function(){
	DAS <- expand.grid(dem = c(FALSE, TRUE), slope = c(FALSE, TRUE), aspect = c(FALSE, TRUE),
						lon = c(FALSE, TRUE), lat = c(FALSE, TRUE))
	aux.var <- apply(DAS, 1, function(j){
		x <- c('D', 'S', 'A', 'Lo', 'La')[j]
		if(length(x) > 0) paste(x, collapse = '')
		else 'noD'
	})
	auxdf <- data.frame(n = 1:32, l = aux.var, DAS, stringsAsFactors = FALSE)

	### Bias/adj comb
	BScomb <- expand.grid(aux = aux.var, interp = c('NN', 'IDW', 'OK'), Bias = c('QM', 'MBVar', 'MBMon'))
	BScomb <- paste(as.character(BScomb$Bias), as.character(BScomb$interp), as.character(BScomb$aux), sep = '_')

	### LM coef comb
	LMcomb <- expand.grid(aux = aux.var, interp = c('NN', 'IDW', 'OK'), adjdir = BScomb)
	LMcomb <- paste(as.character(LMcomb$adjdir), paste(as.character(LMcomb$interp), as.character(LMcomb$aux), sep = '_'), sep = '-')

	mthd <- do.call(rbind, lapply(strsplit(LMcomb, "-"), function(x) do.call(c, strsplit(x, "_"))))
	mthd <- data.frame(mthd, stringsAsFactors = FALSE)
	names(mthd) <- c('bias.method', 'bias.interp', 'bias.auxvar', 'LMcoef.interp', 'LMcoef.auxvar')
	return(mthd)
}

Bias.combination <- function(){
	DAS <- expand.grid(dem = c(FALSE, TRUE), slope = c(FALSE, TRUE), aspect = c(FALSE, TRUE),
						lon = c(FALSE, TRUE), lat = c(FALSE, TRUE))
	aux.var <- apply(DAS, 1, function(j){
		x <- c('D', 'S', 'A', 'Lo', 'La')[j]
		if(length(x) > 0) paste(x, collapse = '')
		else 'noD'
	})
	auxdf <- data.frame(n = 1:32, l = aux.var, DAS, stringsAsFactors = FALSE)

	### Bias/adj comb
	BScomb <- expand.grid(bias.auxvar = aux.var, bias.interp = c('NN', 'IDW', 'OK'), bias.method = c('QM', 'MBVar', 'MBMon'))
	BScomb <- data.frame(apply(BScomb[, 3:1], 2, as.character), stringsAsFactors = FALSE)
	return(BScomb)
}

#################################################################################
##  lists all the functions in file

is_assign <- function (expr) is.call(expr) && as.character(expr[[1]]) %in% c('=', '<-', 'assign')
is_function <- function (expr){
    if(!is_assign(expr)) return(FALSE)
    value <- expr[[3]]
    is.call(value) && as.character(value[[1]]) == 'function'
}
function_name <- function (expr) as.character(expr[[2]])

# unlist(Map(function_name, Filter(is_function, parse(filename))))

#################################################################################
## Vectorization, regression 

regression.Vector <- function(X, Y, min.len){
	# regression by column with a vector
	# Y~X
	# X vector
	# Y matrix, nrow(Y) == length(X) 
	Y[is.na(X) | is.na(Y)] <- NA
	nbY <- base::colSums(!is.na(Y))
	ix <- nbY >= min.len
	RES <- matrix(NA, nrow = 9, ncol = ncol(Y))
	dimnames(RES)[[1]] <- c("slope", "std.slope", "t-value.slope", "p-value.slope", "intercept",
							"std.intercept", "t-value.intercept", "p-value.intercept", "R2")
	if(!any(ix)) return(RES)
	Y <- Y[, ix, drop = FALSE]
	nbY <- nbY[ix]

	mX <- mean(X, na.rm = TRUE)
	mY <- base::colMeans(Y, na.rm = TRUE)
	vX <- var(X, na.rm = TRUE)
	vY <- matrixStats::colVars(Y, na.rm = TRUE)

	X1 <- X - mX
	Y1 <- sweep(Y, 2, mY, FUN = "-")
	COV <- base::colSums(X1 * Y1, na.rm = TRUE) / (nbY - 1)
	alpha <- COV / vX
	beta <- mY - alpha * mX

	hatY <- t(sapply(X, `*`, e2 = alpha) + beta)
	SSE <- base::colSums((hatY - Y)^2, na.rm = TRUE)
	MSE <- SSE/(nbY-2)
	sigma <- sqrt(MSE)
	std.alpha <- sigma / (sqrt(nbY-1)*sqrt(vX))
	std.beta <- sigma * sqrt((1/nbY) + (mX^2/((nbY-1)*vX)))
	SXX <- (nbY-1)*vX
	tvalue.alpha <- alpha / sqrt(MSE/SXX)
	tvalue.beta <- beta / sqrt(MSE * ((1/nbY) + (mX^2/SXX)))
	pvalue.alpha <- 2 * pt(-abs(tvalue.alpha), nbY-2)
	pvalue.beta <- 2 * pt(-abs(tvalue.beta), nbY-2)
	R2 <- COV^2 / (vX * vY)
	RES[, ix] <- rbind(alpha, std.alpha, tvalue.alpha, pvalue.alpha,
						beta, std.beta, tvalue.beta, pvalue.beta, R2)
	return(RES)
}

regression.Matrix <- function(X, Y, min.len){
	# regression column by column between 2 matrices
	# Y~X
	# X, Y matrix same dim 
	ina <- is.na(X) | is.na(Y)
	X[ina] <- NA
	Y[ina] <- NA
	nbY <- base::colSums(!is.na(Y))
	ix <- nbY >= min.len
	RES <- matrix(NA, nrow = 9, ncol = ncol(Y))
	dimnames(RES)[[1]] <- c("slope", "std.slope", "t-value.slope", "p-value.slope", "intercept",
							"std.intercept", "t-value.intercept", "p-value.intercept", "R2")
	if(!any(ix)) return(RES)
	Y <- Y[, ix, drop = FALSE]
	X <- X[, ix, drop = FALSE]
	nbY <- nbY[ix]

	mX <- base::colMeans(X, na.rm = TRUE)
	mY <- base::colMeans(Y, na.rm = TRUE)
	vX <- matrixStats::colVars(X, na.rm = TRUE)
	vY <- matrixStats::colVars(Y, na.rm = TRUE)

	X1 <- sweep(X, 2, mX, FUN = "-")
	Y1 <- sweep(Y, 2, mY, FUN = "-")
	COV <- base::colSums(X1 * Y1, na.rm = TRUE) / (nbY - 1)
	alpha <- COV / vX
	beta <- mY - alpha * mX

	hatY <- sweep(sweep(X, 2, alpha, FUN = "*"), 2, beta, FUN = "+")
	SSE <- base::colSums((hatY - Y)^2, na.rm = TRUE)
	MSE <- SSE/(nbY-2)
	sigma <- sqrt(MSE)
	std.alpha <- sigma / (sqrt(nbY-1)*sqrt(vX))
	std.beta <- sigma * sqrt((1/nbY) + (mX^2/((nbY-1)*vX)))
	SXX <- (nbY-1)*vX
	tvalue.alpha <- alpha / sqrt(MSE/SXX)
	tvalue.beta <- beta / sqrt(MSE * ((1/nbY) + (mX^2/SXX)))
	pvalue.alpha <- 2 * pt(-abs(tvalue.alpha), nbY-2)
	pvalue.beta <- 2 * pt(-abs(tvalue.beta), nbY-2)
	R2 <- COV^2 / (vX * vY)

	RES[, ix] <- rbind(alpha, std.alpha, tvalue.alpha, pvalue.alpha,
						beta, std.beta, tvalue.beta, pvalue.beta, R2)
	return(RES)
}

#################################################################################
## get index
# x0: x coord of points (vector)
# y0: y coord of points (vector)
# x: x coords of the matrix (vector, sorted increasingly)
# y: y coords of the matrix (vector, sorted increasingly)
# if expanded grid, transform first
# xy <- expand.grid(x, y)
# x <- sort(unique(xy[, 1]))
# y <- sort(unique(xy[, 2]))

cdt.which <- function(x0, y0, x, y, arr.ind = FALSE){
	iclo <- findInterval(x0, x)
	ilo <- iclo + (2 * x0 > x[iclo] + x[iclo+1])
	icla <- findInterval(y0, y)
	ila <- icla + (2 * y0 > y[icla] + y[icla+1])

	if(length(ilo) == 0) ilo <- rep(NA, length(x0))
	ilo[ilo == 0 & !is.na(ilo)] <- NA
	if(length(ila) == 0) ila <- rep(NA, length(x0))
	ila[ila == 0 & !is.na(ila)] <- NA
	ina <- is.na(ilo) | is.na(ila)
	ilo[ina] <- NA
	ila[ina] <- NA

	if(arr.ind) cbind(row = ilo, col = ila) else ilo + length(x) * (ila-1)
}







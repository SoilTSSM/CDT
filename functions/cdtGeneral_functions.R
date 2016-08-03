###test internet connection
#http://stackoverflow.com/questions/5076593/how-to-determine-if-you-have-an-internet-connection-in-r
testConnection <- function(url = "http://www.google.com") {
    # test the http capabilities of the current R build
    http <- as.logical(capabilities(what = "http/ftp"))
    if (!http) return(FALSE)

    # test connection by trying to read first line at url
    test <- try(suppressWarnings(readLines(url, n = 1)), silent = TRUE)  # silent errors

    # return FALSE if test is class 'try-error'
    ifelse(inherits(test, "try-error"), FALSE, TRUE)
}

####################################################################
#get the file name without extension
getf.no.ext <- function(flname){
	ig <- grep('\\.',flname)
	if(length(ig) == 0){
		fret <- flname
	}else{
		fsplit <- unlist(strsplit(flname,'\\.'))
		fret <- sub(paste('.',fsplit[length(fsplit)], sep = ''),'',flname)
	}
	return(fret)
}
#ou
#getf.no.ext <- function(flname){
#	require(tools)
#	extf <- file_ext(flname)
#	if(extf == "") fret <- flname
#	else fret <- sub(paste('.',extf, sep = ''),'',flname)
#	return(fret)
#}


####################################################################
##voir http://docs.activestate.com/activetcl/8.4/bwidget/DynamicHelp.html
#BWidget info-bulle(ballon, tooltip) help
infobulle <- function(tclobj, text){
	tcl("interp", "alias", "", "help", "", "DynamicHelp::register") 
	tcl('help', tclobj, 'balloon', text)
}

##Binding event in toolbar and display on status bar
status.bar.display <- function(tclobj, tclvar, text){
	tkbind(tclobj,"<Enter>", function() tclvalue(tclvar) <- text)
	tkbind(tclobj,"<Leave>", function() tclvalue(tclvar)<-"")
}


###########################################
helpWidget <- function(tclobj, statusbar_tclvar, text_ballon, text_statusbar){
	tkbind(tclobj,"<Enter>", function() tclvalue(statusbar_tclvar)<-text_statusbar)
	tkbind(tclobj,"<Leave>", function() tclvalue(statusbar_tclvar)<-"")
	tcl("DynamicHelp::register", tclobj, 'balloon', text_ballon)
}


####################################################################
##Create button on toolbar
tkbutton.toolbar <- function(frame, img.dir, img.file, txtvar.status, txt.tooltip, txt.status){
	picture <- tkimage.create('photo','-file', file.path(img.dir, img.file, fsep = .Platform$file.sep))
	button <- tkbutton(frame, image = picture, relief = 'flat')
	infobulle(button, txt.tooltip)
	status.bar.display(button, txtvar.status, txt.status)
	return(button)
}

##Create button with help
tkbutton.h <- function(frame, text, txtvar.status, txt.tooltip, txt.status){
	button <- tkbutton(frame, text = text)
	infobulle(button, txt.tooltip)
	status.bar.display(button, txtvar.status, txt.status)
	return(button)
}

##Create entry with help
tkentry.h <- function(frame, txtvar.status, txt.tooltip, txt.status){
	entry <- tkentry(frame)
	infobulle(entry, txt.tooltip)
	status.bar.display(entry, txtvar.status, txt.status)
	return(entry)
}

##Create label with help
tklabel.h <- function(frame, text, txtvar.status, txt.tooltip, txt.status){
	labl <- tklabel(frame, text = text)
	infobulle(labl, txt.tooltip)
	status.bar.display(labl, txtvar.status, txt.status)
	return(labl)
}

####################################################################
##spinbox
ttkspinbox <- function(parent, ...) tkwidget(parent, "ttk::spinbox", ...)

####################################################################
### Insert text 
InsertMessagesTxt <- function(wdgt, texta, format = FALSE, fgcolor = 'red', bgcolor = 'yellow'){
	#tktag.add(wdgt, "formated", "end -1 lines linestart", "end -1 lines lineend")
	font1 <- tkfont.create(family = "times", weight = "bold", slant = "roman", size = 11)
	font2 <- tkfont.create(family = "times", weight = "normal", slant = "italic", size = 11)
	tktag.configure(wdgt, "formated1", foreground = fgcolor, background = bgcolor, font = font1)
	tktag.configure(wdgt, "formated2", foreground = fgcolor, background = bgcolor, font = font2)
	txtformated <- if(fgcolor == 'red' & bgcolor == 'yellow') "formated1" else "formated2"
	chn <- tclvalue(tkget(wdgt, "0.0", "end"))
	vectxt <- unlist(strsplit(chn,"\n"))
	lnt <- length(vectxt)
#	if(lnt == 50){
#		tkdelete(wdgt, "0.0", "2.0") 
#		if(format) tkinsert(wdgt, "end", paste(texta,"\n"), txtformated)
#		else tkinsert(wdgt, "end", paste(texta,"\n"))
#	}else{
		if(format) tkinsert(wdgt, "end", paste(texta,"\n"), txtformated)
		else tkinsert(wdgt, "end", paste(texta,"\n"))
#	}
	tcl(wdgt, 'yview', 'moveto', '1.0')
}

###############################################################################
###To remplace the tooltip and statusbar widget
## generic
# tkwidget.x <- function (parent, type, txtVar, txtSatus, txtTooltip,...){
# 	win <- .Tk.subwin(parent)
# 	tcl(type, win, ...)
# 	infobulle(win, txtTooltip)
# 	status.bar.display(win, txtVar, txtSatus)
# 	return(win)
# }

##############Redefine widget
# tkbutton.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "button", txtVar, txtSatus, txtTooltip,...)
# tkcanvas.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "canvas", txtVar, txtSatus, txtTooltip,...)
# tkcheckbutton.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "checkbutton", txtVar, txtSatus, txtTooltip,...)
# tkentry.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "entry", txtVar, txtSatus, txtTooltip,...)
# tkframe.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "frame", txtVar, txtSatus, txtTooltip,...)
# tklabel.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "label", txtVar, txtSatus, txtTooltip,...)
# tklistbox.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "listbox", txtVar, txtSatus, txtTooltip,...)
# tkmenu.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "menu", txtVar, txtSatus, txtTooltip,...)
# tkmenubutton.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "menubutton", txtVar, txtSatus, txtTooltip,...)
# tkmessage.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "message", txtVar, txtSatus, txtTooltip,...)
# tkradiobutton.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "radiobutton", txtVar, txtSatus, txtTooltip,...)
# tkscale.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "scale", txtVar, txtSatus, txtTooltip,...)
# tkscrollbar.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "scrollbar", txtVar, txtSatus, txtTooltip,...)
# tktext.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "text", txtVar, txtSatus, txtTooltip,...)
# ttkbutton.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "ttk::button", txtVar, txtSatus, txtTooltip,...)
# ttkcheckbutton.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "ttk::checkbutton", txtVar, txtSatus, txtTooltip,...)
# ttkcombobox.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "ttk::combobox", txtVar, txtSatus, txtTooltip,...)
# ttkentry.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "ttk::entry", txtVar, txtSatus, txtTooltip,...)
# ttkframe.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "ttk::frame", txtVar, txtSatus, txtTooltip,...)
# ttkimage.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "ttk::image", txtVar, txtSatus, txtTooltip,...)
# ttklabel.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "ttk::label", txtVar, txtSatus, txtTooltip,...)
# ttklabelframe.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "ttk::labelframe", txtVar, txtSatus, txtTooltip,...)
# ttkmenubutton.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "ttk::menubutton", txtVar, txtSatus, txtTooltip,...)
# ttknotebook.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "ttk::notebook", txtVar, txtSatus, txtTooltip,...)
# ttkpanedwindow.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "ttk::panedwindow", txtVar, txtSatus, txtTooltip,...)
# ttkprogressbar.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "ttk::progressbar", txtVar, txtSatus, txtTooltip,...)
# ttkradiobutton.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "ttk::radiobutton", txtVar, txtSatus, txtTooltip,...)
# ttkscrollbar.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "ttk::scrollbar", txtVar, txtSatus, txtTooltip,...)
# ttkseparator.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "ttk::separator", txtVar, txtSatus, txtTooltip,...)
# ttksizegrip.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "ttk::sizegrip", txtVar, txtSatus, txtTooltip,...)
# ttktreeview.x <- function(parent, txtVar, txtSatus, txtTooltip,...) tkwidget.x(parent, "ttk::treeview", txtVar, txtSatus, txtTooltip,...)

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
bwNoteBook <- function(parent, side = 'top',...){
	tn <- tkwidget(parent, "NoteBook", side = side,...)
	return(tn)
}

##arg ... pass to insert
bwAddTab <- function(parent, text = "Tab",...){
	IDtab <- paste('_BwNb', isaTabBwNb(), sep = '')
	tab <- tkinsert(parent, 'end', IDtab, text = text,...)
	#win <- .Tk.newwin(tclvalue(tcl(parent, "getframe", IDtab))) #mitovy
	win <- .Tk.newwin(tclvalue(tab))
	win$IDtab <- IDtab
	return(win)
}

##
bwRaiseTab <- function(parent, tab) tcl(parent, 'raise', tab$IDtab)

###############################################################################

##BWidget PanedWindow
bwPanedWindow <- function(parent,...){
	panewin <- tkwidget(parent, 'PanedWindow',...)
	return(panewin)
}

bwAddPanedWindow <- function(parent,...){
	panWidget <- tkadd(parent,...)
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
bwScrolledWindow <- function(parent,...){
	scrwin <- tkwidget(parent, "ScrolledWindow",...)
	return(scrwin)
}

bwScrollableFrame <- function(parent,...){
	scrfrm <- tkwidget(parent, "ScrollableFrame",...)
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

ScrollCanvas <- function(parent,...){
	canvas <- tkcanvas(parent,...)
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
#Cycle month, start month, n: number of month (DJF, start = 'December', n = 3)
# return end month

cycleMonth <- function(start, n){
	mois <- format(ISOdate(2014,1:12,1), "%B")
	ix <- which(mois == start)
	im<-(ix+(n-1))%%12
	if(im == 0) im <- 12
	return(mois[im])
}

###############################################################################
## Add or substract months

addMonths <- function(daty, n = 1){
	date0 <- seq(daty, by = paste(n, "months"), length = 2)[2]
	date1 <- seq(as.Date(paste(format(daty,'%Y-%m'),'01', sep = '-')), by = paste(n+1, "months"), length = 2)[2]-1
	daty <- if(date0 > date1) date1 else date0
	return(daty)
}

###############################################################################
## Add or substract dekads

addDekads <- function(daty, n = 1){
	idek <- as.numeric(substr(format(daty,'%Y%m%d'), 8,8))+n
	dek <- idek%%3
	if(dek == 0) dek <- 3
	daty <- format(addMonths(daty, floor((idek-1)/3)),'%Y-%m')
	daty <- as.Date(paste(daty, dek, sep = '-'))
	return(daty)
}

###############################################################################
#File or directory to save result
#filedirVar tclVar

fileORdir2Save <- function(filedirVar, isFile = TRUE){
	if(isFile){
		filetypes  <-  "{{Text Files} {.txt .TXT}} {{CSV Files} {.csv .CSV}} {{All files} *}"
		if(Sys.info()["sysname"] == "Windows") file2save <- tkgetSaveFile(initialdir = getwd(), initialfile = "", filetypes = filetypes, defaultextension = TRUE)
		else file2save <- tkgetSaveFile(initialdir = getwd(), initialfile = "", filetypes = filetypes)
		if(is.na(file2save)) tclvalue(filedirVar) <- ""
		else tclvalue(filedirVar) <- file2save
	}else{
		dir2save <- tk_choose.dir(getwd(), "")
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
		listOpenFiles <- c('',lapply(1:nopfs, function(j) AllOpenFilesData[[j]][[1]]))
	}else{
		listOpenFiles <- list()
		listOpenFiles[[1]] <- c('')
	}
	return(listOpenFiles)
}


#################################################################################
## get shp file  in the list (all open files)
##return [[1]] name [[2]] shp [[3]] path

getShpOpenData <- function(shp){
	if(tclvalue(shp) != ""){
		all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))
		jfile <- which(all.open.file == tclvalue(shp))
		if(AllOpenFilesType[[jfile]] == "shp"){
			shpf <- AllOpenFilesData[[jfile]]
		}else shpf <- NULL
	}else shpf <- NULL
	return(shpf)
}


#################################################################################
## get stn data  in the list (all open files)
## return CDT data format

getStnOpenData <- function(file.stnfl){
	if(tclvalue(file.stnfl) != ""){
		all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))
		jfile <- which(all.open.file == tclvalue(file.stnfl))
		if(AllOpenFilesType[[jfile]] == "ascii"){
			donne <- AllOpenFilesData[[jfile]][[2]]
		}else donne <- NULL
	}else donne <- NULL
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
	if(freqData == 'daily') daty <- format(daty,'%Y%m%d')
	if(freqData == 'dekadal') daty <- paste(format(daty,'%Y%m'), as.numeric(format(daty,'%d')), sep = '')
	if(freqData == 'monthly') daty <- format(daty,'%Y%m')
	idate <- match(daty, dates)
	if(is.na(idate)) return(NULL)
	zval <- as.numeric(donne[idate,])
	return(list(date = daty, lon = lon, lat = lat, id = id, z = zval, elv = elv))
}	

#################################################################################
## get DEM data  in the list (all open files)
## return $lon $lat $dem

getDemOpenData <- function(file.grddem){
	if(tclvalue(file.grddem) != ""){
		all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))
		jfile <- which(all.open.file == tclvalue(file.grddem))
		if(AllOpenFilesType[[jfile]] == "netcdf"){
			fdem <- AllOpenFilesData[[jfile]][[2]]
			demv <- fdem$value
			demv[demv < 0] <- NA
			dem <- list(lon = fdem$x, lat = fdem$y, dem = demv)
		}else dem <- NULL
	}else dem <- NULL
	return(dem)
}


#################################################################################
## get NetCDF data  in the list (all open files)
## return $lon $lat $val

getNcdfOpenData <- function(file.netcdf){
	if(tclvalue(file.netcdf) != ""){
		all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))
		jfile <- which(all.open.file == tclvalue(file.netcdf))
		if(AllOpenFilesType[[jfile]] == "netcdf"){
			nc <- AllOpenFilesData[[jfile]]
		}else nc <- NULL
	}else nc <- NULL
	return(nc)
}

#################################################################################
## get NetCDF data plot merging outputs menu

getNcdfData2Plot <- function(dataNCDF, freqData, yrs, mon, day, ncOrder = c(1,2)){
	ilon <- ncOrder[1]
	ilat <- ncOrder[2]
	if(freqData == 'Monthly data') daty <- try(format(as.Date(paste(as.numeric(yrs), as.numeric(mon), 15, sep = '-')),'%Y%m%d'), silent = TRUE)
	else daty <- try(format(as.Date(paste(as.numeric(yrs), as.numeric(mon), as.numeric(day), sep = '-')),'%Y%m%d'), silent = TRUE)
	if(inherits(daty, "try-error") | is.na(daty)){
		InsertMessagesTxt(main.txt.out, paste("Date format invalid", tclvalue(dataNCDF[[2]])), format = TRUE)
		return(NULL)
	}

	if(freqData == 'Daily data') filelpath <- file.path(tclvalue(dataNCDF[[1]]), sprintf(tclvalue(dataNCDF[[2]]), substr(daty, 1,4), substr(daty, 5,6), substr(daty, 7,8)))
	if(freqData == 'Dekadal data') filelpath <- file.path(tclvalue(dataNCDF[[1]]), sprintf(tclvalue(dataNCDF[[2]]), substr(daty, 1,4), substr(daty, 5,6), substr(daty, 8,8)))
	if(freqData == 'Monthly data') filelpath <- file.path(tclvalue(dataNCDF[[1]]), sprintf(tclvalue(dataNCDF[[2]]), substr(daty, 1,4), substr(daty, 5,6)))
	if(!file.exists(filelpath)){
		InsertMessagesTxt(main.txt.out, paste(filelpath, "doesn't exist"), format = TRUE)
		return(NULL)
	}

	nc <- nc_open(filelpath)
	lon <- nc$dim[[ilon]]$vals
	lat <- nc$dim[[ilat]]$vals
	val <- ncvar_get(nc, varid = nc$var[[1]]$name)
	nc_close(nc)
	# xo <- order(lon)
	# lon <- rfe.lon[xo]
	# yo <- order(lat)
	# lat <- lat[yo]
	# val <- val[xo, yo]
	# if(ilat == 1){
	# 	val <- matrix(c(val), nrow = length(lon), ncol = length(lat), byrow = T)
	# }
	return(list(x = lon, y = lat, value = val))
}

#################################################################################
## get RFE data plot qc spatial check result

getSatelliteData <- function(dir_ncdf, ff_ncdf, spchkQcDateVal){
	if(!is.null(ReturnExecResults)){
		spchkoutdates <- isSpatialCheckOk()
		if(nrow(spchkoutdates) != 0){
			dataNCDF <- list(dir_ncdf, ff_ncdf)
			freqData <- ifelse(ReturnExecResults$period == 'daily', 'Daily data', ifelse(ReturnExecResults$period == 'dekadal', 'Dekadal data', 'Monthly data'))
			spdaty <- tclvalue(spchkQcDateVal)
			year <- as.numeric(substr(spdaty, 1,4))
			mon <- as.numeric(substr(spdaty, 5,6))
			day <- as.numeric(substr(spdaty, 7,8))
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
##get DEM at stations locations (interpolation leftCmd)
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

##################################################################################
#Reshape data.frame XYZ to matrix list(x, y, z = matrix)

## 8.5 times faster
reshapeXYZ2Matrix <- function(df){
	#require(reshape2)
	df <- as.data.frame(df)
	names(df) <- c('x', 'y', 'z')
	x <- sort(unique(df$x))
	y <- sort(unique(df$y))
	z <- acast(df, x~y, value.var = "z")
	dimnames(z) <- NULL
	return(list(x = x, y = y, z = z))
}


# ## 4 times faster
# reshapeXYZ2Matrix1 <- function(df){
# 	df <- as.data.frame(df)
# 	names(df) <- c('x', 'y', 'z')
# 	x <- sort(unique(df$x))
# 	y <- sort(unique(df$y))
# 	dxy <- expand.grid(x = x, y = y)
# 	ix <- match(paste(df$x, df$y, sep = '_'), paste(dxy$x, dxy$y, sep = '_'))
# 	z <- matrix(NA, nrow = length(x), ncol = length(y))
# 	z[ix] <- df$z
# 	rm(dxy, ix)
# 	return(list(x = x, y = y, z = z))
# }


# ## 1.2 times faster
# reshapeXYZ2Matrix2 <- function(df){
# 	df <- as.data.frame(df)
# 	names(df) <- c('x', 'y', 'z')
# 	x <- sort(unique(df$x))
# 	y <- sort(unique(df$y))
# 	dxy <- data.frame(expand.grid(x = x, y = y), z = 1:(length(x)*length(y)))
# 	dat <- merge(df, dxy, by = c('y', 'x'), all = T, sort = FALSE)
# 	dat <- dat[order(dat[,4]),]
# 	z <- matrix(dat[,3], nrow = length(x), ncol = length(y))
# 	rm(dxy, dat)
# 	return(list(x = x, y = y, z = z))
# }

# ## slow
# reshapeXYZ2Matrix3 <- function(df){
# 	df <- as.data.frame(df)
# 	names(df) <- c('x', 'y', 'z')
# 	tmp <- attributes(xtabs(z~x+y, data = df, sparse = TRUE))
# 	x <- as.numeric(tmp$Dimnames[[1]])
# 	y <- as.numeric(tmp$Dimnames[[2]])
# 	xz <- matrix(NA, nrow = tmp$Dim[1], ncol = tmp$Dim[2])
# 	end <- tmp$p[-1]
# 	start <- c(1,(end+1)[-length(end)])
# 	z <- sapply(seq_along(start), function(j){
# 		ic<-(tmp$i+1)[start[j]:end[j]]
# 		xz[ic, j] <- tmp$x[start[j]:end[j]]
# 		xz[,j]
# 	})
# 	rm(tmp, xz, end, start)
# 	return(list(x = x, y = y, z = z))
# }

##################################################################################
###List exchange (dialog qc, hom, merging)

# listExchange <- function(list1, list2, list3){
# 	#retList <- list2[sapply(1:length(list2), function(x) !identical(list2[[x]], list1[[x]]))]  	#index list
# 	retList <- list2[sapply(names(list2), function(x) !identical(list2[[x]], list1[[x]]))]        #named list
# 	if(length(retList) > 0){
# 		#id <- which(!list2%in%list1) #index list
# 		id <- names(retList)    #named list
# 		for(i in id){
# 			diffElement <- list2[[i]] != list1[[i]]
# 			list3[[i]][diffElement] <- list2[[i]][diffElement]
# 		}
# 	}
# 	return(list3)
# }

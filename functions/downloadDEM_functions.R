getDEMFun <- function(parent.win){
	if (Sys.info()["sysname"] == "Windows"){
		largeur <- 35
		largeur1 <- 15
	}else{
		largeur <- 28
		largeur1 <- 15
	}

	downdem <- fromJSON(file.path(apps.dir, 'init_params', 'Download_DEM.json'))
	if(str_trim(downdem$dir2save) == "") downdem$dir2save <- getwd()

	#####
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frGrd0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frGrd1 <- tkframe(tt)

	#####
	frA1 <- tkframe(frGrd0, relief = 'sunken', bd = 2)

	minLon <- tclVar(downdem$bbox$minlon)
	maxLon <- tclVar(downdem$bbox$maxlon)
	minLat <- tclVar(downdem$bbox$minlat)
	maxLat <- tclVar(downdem$bbox$maxlat)

	fr_grd <- ttklabelframe(frA1, text = "Area of interest", relief = "groove", borderwidth = 2)

	grd_llon <- tklabel(fr_grd, text = "Longitude", anchor = 'e', justify = 'right', width = largeur1)
	grd_llat <- tklabel(fr_grd, text = "Latitude", anchor = 'e', justify = 'right')
	grd_lb1 <- tklabel(fr_grd, text = "Minimum")
	grd_lb2 <- tklabel(fr_grd, text = "Maximum")
	grd_vlon1 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = minLon)
	grd_vlon2 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = maxLon)
	grd_vlat1 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = minLat)
	grd_vlat2 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = maxLat)

	tkgrid(grd_lb1, row = 0, column = 1, sticky = "ew")
	tkgrid(grd_lb2, row = 0, column = 2, sticky = "ew")
	tkgrid(grd_llon, row = 1, column = 0, sticky = "ew")
	tkgrid(grd_vlon1, row = 1, column = 1, sticky = "ew")
	tkgrid(grd_vlon2, row = 1, column = 2, sticky = "ew")
	tkgrid(grd_llat, row = 2, column = 0, sticky = "ew")
	tkgrid(grd_vlat1, row = 2, column = 1, sticky = "ew")
	tkgrid(grd_vlat2, row = 2, column = 2, sticky = "ew")

	tkgrid(fr_grd, row = 0, column = 0, sticky = "ew", padx = 5, pady = 5)

	infobulle(grd_vlon1, 'Minimum longitude in decimal degree')
	status.bar.display(grd_vlon1, TextOutputVar, 'Minimum longitude in decimal degree')
	infobulle(grd_vlon2, 'Maximum longitude in decimal degree')
	status.bar.display(grd_vlon2, TextOutputVar, 'Maximum longitude in decimal degree')
	infobulle(grd_vlat1, 'Minimum latitude in decimal degree')
	status.bar.display(grd_vlat1, TextOutputVar, 'Minimum latitude in decimal degree')
	infobulle(grd_vlat2, 'Maximum latitude in decimal degree')
	status.bar.display(grd_vlat2, TextOutputVar, 'Maximum latitude in decimal degree')

	#########
	frA2 <- tkframe(frGrd0, relief = 'sunken', bd = 2)

	dir2save <- tclVar(downdem$dir2save)

	lab2 <- tklabel(frA2, text = 'Directory to save downloaded files', anchor = 'w', justify = 'left')
	enfile.save <- tkentry(frA2, textvariable = dir2save, width = largeur)
	btfile.save <- tkbutton(frA2, text = "...")

	###
	tkconfigure(btfile.save, command = function() fileORdir2Save(dir2save, isFile = FALSE))

	###
	tkgrid(lab2, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(enfile.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(btfile.save, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(enfile.save, 'Enter the full path to directory to save downloaded files')
	status.bar.display(enfile.save, TextOutputVar, 'Enter the full path to directory to save downloaded files')
	infobulle(btfile.save, 'or browse here')

	######
	tkgrid(frA1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(frA2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)

	######
	btOK <- ttkbutton(frGrd1, text = 'Download')

	tkconfigure(btOK, command = function(){
		outdir <- str_trim(tclvalue(dir2save))
		minlon <- as.numeric(tclvalue(minLon))
		maxlon <- as.numeric(tclvalue(maxLon))
		minlat <- as.numeric(tclvalue(minLat))
		maxlat <- as.numeric(tclvalue(maxLat))

		if(outdir == "" | outdir == "NA"){
			tkmessageBox(message = "Browse or enter directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
			if(testConnection()){
				InsertMessagesTxt(main.txt.out, "Downloading.................")
				#return(ExecDownload_DEM(minlon, maxlon, minlat, maxlat, outdir))
				ExecDownload_DEM(minlon, maxlon, minlat, maxlat, outdir)
			}else{
				InsertMessagesTxt(main.txt.out, 'No internet connection', format = TRUE)
				return(NULL)
			}
		}
	})

	tkgrid(btOK, row = 0, column = 0, sticky = 'e', padx = 5, pady = 5)

	#####
	tkgrid(frGrd0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frGrd1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Digital Elevation Model')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {
		tkgrab.release(tt)
		tkfocus(parent.win)
		return(NULL)
	})
	tkwait.window(tt)
	return(0)
}

###################

ExecDownload_DEM <- function(minlon, maxlon, minlat, maxlat, outdir){
	tkconfigure(main.win, cursor = 'watch')
	tcl('update')
	getDEM(minlon, maxlon, minlat, maxlat, outdir)
	tkconfigure(main.win, cursor = '')
}

#######################

aggregateDEM <- function(destfile, xm, ym, outdir, varid, longname, res1 = FALSE){
	nc <- nc_open(destfile)
	xd <- nc$dim[[1]]$vals
	yd <- nc$dim[[2]]$vals
	xz <- ncvar_get(nc, varid = varid)
	nc_close(nc)
	demobj <- list(lon = xd, lat = yd, z = xz)
	xnew <- xm[xm >= min(xd) & xm <= max(xd)]
	ynew <- ym[ym >= min(yd) & ym <= max(yd)]
	if(res1){
		xnew <- xnew[-1]
		ynew <- ynew[-length(ynew)]
	}
	grdnew <- list(lon = xnew, lat = ynew)
	newobj <- cdt.interp.surface.grid(demobj, grdnew)
	newobj$z[is.na(newobj$z)] <- NaN
	dx <- ncdim_def("Lon", "degreeE", newobj$x)
	dy <- ncdim_def("Lat", "degreeN", newobj$y)
	demnc <- ncvar_def('dem', "m", list(dx, dy), NaN, longname = longname, prec = "short")
	outfl <- file.path(outdir, 'DEM_for_Merging.nc')
	nc2 <- nc_create(outfl, demnc)
	ncvar_put(nc2, demnc, newobj$z)
	nc_close(nc2)
}

#######################

getDEM <- function(minlon, maxlon, minlat, maxlat, outdir){
	##DEM NOAA NGDC ETOPO2v2: ETOPO2v2c Global Gridded 2-minute elevation and bathymetric data.
	url <- 'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NGDC/.ETOPO2v2/.z'
	area <- paste('X', minlon-0.0375, maxlon+0.0375, 'RANGEEDGES', 'Y', minlat-0.0375, maxlat+0.0375, 'RANGEEDGES', sep = '/')
	destfile <- file.path(outdir, 'DEM_2_Arc-Minute.nc')
	link <- paste(url, area, 'data.nc', sep = '/')
	ret <- try(download.file(link, destfile, method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE), silent = TRUE)

	##NOAA NGDC ETOPO1: ETOPO1 Grid Registered 1 Arc-Minute Global Relief Model
	url1 <- 'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NGDC/.ETOPO1/.z_bedrock'
	area1 <- paste('lon', minlon, maxlon, 'RANGEEDGES', 'lat', minlat, maxlat, 'RANGEEDGES', sep = '/')
	destfile1 <- file.path(outdir, 'DEM_1_Arc-Minute.nc')
	link1 <- paste(url1, area1, 'data.nc', sep = '/')
	ret1 <- try(download.file(link1, destfile1, method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE), silent = TRUE)

	### Africa
	xm <- seq(-19.0125, 51.975, 0.0375)
	ym <- seq(-35.9625, 38.025, 0.0375)

	aggregateDEM2Merge <- function(ret, destfile, res1, varid, longname, msg){
		if(ret == 0){
			down <- try(aggregateDEM(destfile, xm, ym, outdir, varid, longname, res1), silent = TRUE)
			if(!inherits(down, "try-error")) InsertMessagesTxt(main.txt.out, msg)
			else{
				InsertMessagesTxt(main.txt.out, 'Unable to aggregate DEM for mering', format = TRUE)
				InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', down[1]), format = TRUE)
			}
		}else InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', ret[1]), format = TRUE)
	}

	aggregateFailedMsg <- function(ret, destfile, msg){
		unlink(destfile)
		InsertMessagesTxt(main.txt.out, msg, format = TRUE)
		InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', ret[1]), format = TRUE)
	}

	if(!inherits(ret, "try-error")) aggregateDEM2Merge(ret, destfile, TRUE, 'z', "Elevation and bathymetric data", 'Download finished for DEM 2-min')
	else{
		if(!inherits(ret1, "try-error")) aggregateDEM2Merge(ret1, destfile1, FALSE, 'z_bedrock', "Global Relief Model", 'Download finished for DEM 1-min')
		else aggregateFailedMsg(ret1, destfile1, 'Download failed for DEM 1-min')
		aggregateFailedMsg(ret, destfile, 'Download failed for DEM 2-min')
	}
}


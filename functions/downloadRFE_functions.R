DownloadRFE <- function(parent.win){
	if(Sys.info()["sysname"] == "Windows"){
		largeur <- 27
		largeur1 <- 24
	}else{
		largeur <- 21
		largeur1 <- 24
	}

	downrfe <- fromJSON(file.path(apps.dir, 'init_params', 'Download_RFE.json'))
	if(str_trim(downrfe$dir2save) == "") downrfe$dir2save <- getwd()
	
	#####################
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frA <- tkframe(tt)
	frB <- tkframe(tt)

	#####################
	frA1 <- tkframe(frA, relief = 'sunken', borderwidth = 2)

	fileSource <- tclVar(str_trim(downrfe$rfe.data))
	rfeChoix <- c('10-DAYS TAMSATv3', '10-DAYS TAMSATv2', '10-DAYS CHIRPSv2.0', '10-DAYS CHIRPv1.0',
				  '------------------',
				  'DAILY TAMSATv3', 'DAILY TAMSATv2', 'DAILY CHIRPSv2.0')

	cb.period <- ttkcombobox(frA1, values = rfeChoix, textvariable = fileSource, width = largeur1)

	######
	tkgrid(cb.period, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.period, 'Choose the data source')
	status.bar.display(cb.period, TextOutputVar, 'Choose the data source')

	######
	tkbind(cb.period, "<<ComboboxSelected>>", function(){
		if(tclvalue(fileSource)%in%c('10-DAYS TAMSATv3', '10-DAYS TAMSATv2', '10-DAYS CHIRPSv2.0', '10-DAYS CHIRPv1.0')){
			tclvalue(daytext) <- 'Dek'
			tclvalue(iend.day) <- '3'
		}else{
			tclvalue(daytext) <- 'Day'
			tclvalue(iend.day) <- '31'
		}
	})

	######
	frA2 <- tkframe(frA, relief = 'sunken', borderwidth = 2)

	istart.yrs <- tclVar(downrfe$date$year1)
	istart.mon <- tclVar(downrfe$date$mon1)
	istart.day <- tclVar(downrfe$date$day1)
	iend.yrs <- tclVar(downrfe$date$year2)
	iend.mon <- tclVar(downrfe$date$mon2)
	iend.day <- tclVar(downrfe$date$day2)
	daytext <- tclVar(downrfe$date$day.label)

	deb.txt <- tklabel(frA2, text = 'Start date', anchor = 'e', justify = 'right')
	fin.txt <- tklabel(frA2, text = 'End date', anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(frA2, text = 'Year')
	mon.txt <- tklabel(frA2, text = 'Mon')
	day.txt <- tklabel(frA2, text = tclvalue(daytext), textvariable = daytext)

	yrs1.v <- tkentry(frA2, width = 4, textvariable = istart.yrs, justify = "right")
	mon1.v <- tkentry(frA2, width = 4, textvariable = istart.mon, justify = "right")
	day1.v <- tkentry(frA2, width = 4, textvariable = istart.day, justify = "right")
	yrs2.v <- tkentry(frA2, width = 4, textvariable = iend.yrs, justify = "right")
	mon2.v <- tkentry(frA2, width = 4, textvariable = iend.mon, justify = "right")
	day2.v <- tkentry(frA2, width = 4, textvariable = iend.day, justify = "right")

	tkgrid(deb.txt, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(fin.txt, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs.txt, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon.txt, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day.txt, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs1.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon1.v, row = 1, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day1.v, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs2.v, row = 2, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon2.v, row = 2, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day2.v, row = 2, column = 3, sticky = 'ew', padx = 1, pady = 1)

	infobulle(frA2, 'Start and end date to download RFE')
	status.bar.display(frA2, TextOutputVar, 'Start and end date to download RFE')

	######
	frA3 <- tkframe(frA)

	bt.prm.OK <- ttkbutton(frA3, text = "Download")

	tkconfigure(bt.prm.OK, command = function(){
		istart.yrs0 <- as.numeric(tclvalue(istart.yrs))
		istart.mon0 <- as.numeric(tclvalue(istart.mon))
		istart.day0 <- as.numeric(tclvalue(istart.day))
		iend.yrs0 <- as.numeric(tclvalue(iend.yrs))
		iend.mon0 <- as.numeric(tclvalue(iend.mon))
		iend.day0 <- as.numeric(tclvalue(iend.day))
		istart <- paste(istart.yrs0, istart.mon0, istart.day0, sep = '-')
		iend <- paste(iend.yrs0, iend.mon0, iend.day0, sep = '-')
		minlon <- as.numeric(tclvalue(minLon))
		maxlon <- as.numeric(tclvalue(maxLon))
		minlat <- as.numeric(tclvalue(minLat))
		maxlat <- as.numeric(tclvalue(maxLat))
		outdir <- str_trim(tclvalue(file.save1))
		datasrc <- str_trim(tclvalue(fileSource))

		if(outdir == "" | outdir == "NA"){
			tkmessageBox(message = "Browse or enter directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
			if(testConnection()){
				InsertMessagesTxt(main.txt.out, "Downloading.................")
				return(ExecDownload_SatData(datasrc, istart, iend, minlon, maxlon, minlat, maxlat, outdir))
			}else{
				InsertMessagesTxt(main.txt.out, 'No Internet connection', format = TRUE)
				return(NULL)
			}
		}
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	##################
	tkgrid(frA1, row = 0, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(frA2, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frA3)

	##################

	frB1 <- tkframe(frB, relief = 'sunken', borderwidth = 2)

	minLon <- tclVar(downrfe$bbox$minlon)
	maxLon <- tclVar(downrfe$bbox$maxlon)
	minLat <- tclVar(downrfe$bbox$minlat)
	maxLat <- tclVar(downrfe$bbox$maxlat)

	fr_grd <- ttklabelframe(frB1, text = "Area of interest", relief = "groove", borderwidth = 2)

	grd_llon <- tklabel(fr_grd, text = "Longitude", anchor = 'e', justify = 'right')
	grd_llat <- tklabel(fr_grd, text = "Latitude", anchor = 'e', justify = 'right')
	grd_lb1 <- tklabel(fr_grd, text = "Minimum")
	grd_lb2 <- tklabel(fr_grd, text = "Maximum")
	grd_vlon1 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = minLon)
	grd_vlon2 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = maxLon)
	grd_vlat1 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = minLat)
	grd_vlat2 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = maxLat)

	######
	tkgrid(grd_lb1, row = 0, column = 1, sticky = "ew")
	tkgrid(grd_lb2, row = 0, column = 2, sticky = "ew")
	tkgrid(grd_llon, row = 1, column = 0, sticky = "ew")
	tkgrid(grd_vlon1, row = 1, column = 1, sticky = "ew")
	tkgrid(grd_vlon2, row = 1, column = 2, sticky = "ew")
	tkgrid(grd_llat, row = 2, column = 0, sticky = "ew")
	tkgrid(grd_vlat1, row = 2, column = 1, sticky = "ew")
	tkgrid(grd_vlat2, row = 2, column = 2, sticky = "ew")

	tkgrid(fr_grd, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(grd_vlon1, 'Minimum longitude in degree decimal')
	status.bar.display(grd_vlon1, TextOutputVar, 'Minimum longitude in degree decimal')
	infobulle(grd_vlon2, 'Maximum longitude in degree decimal')
	status.bar.display(grd_vlon2, TextOutputVar, 'Maximum longitude in degree decimal')
	infobulle(grd_vlat1, 'Minimum latitude in degree decimal')
	status.bar.display(grd_vlat1, TextOutputVar, 'Minimum latitude in degree decimal')
	infobulle(grd_vlat2, 'Maximum latitude in degree decimal')
	status.bar.display(grd_vlat2, TextOutputVar, 'Maximum latitude in degree decimal')

	########

	frB2 <- tkframe(frB, relief = 'sunken', borderwidth = 2)

	file.save1 <- tclVar(downrfe$dir2save)

	txt.file.save <- tklabel(frB2, text = 'Directory to save downloaded files', anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frB2, textvariable = file.save1, width = largeur)
	bt.file.save <- tkbutton(frB2, text = "...")

	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(downrfe$dir2save, "")
			if(is.na(file2save1)) tclvalue(file.save1) <- downrfe$dir2save
			else{
				dir.create(file2save1, showWarnings = FALSE, recursive = TRUE)
				tclvalue(file.save1) <- file2save1
			}
	})

	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(en.file.save, 'Enter the full path to directory to save downloaded files')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save downloaded files')
	infobulle(bt.file.save, 'or browse here')

	##################
	tkgrid(frB1, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frB2, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	##########
	tkgrid(frA, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frB, row = 0, column = 1, sticky = 'nswe', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Download Satellite Rainfall Estimate data')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {
		tkgrab.release(tt)
		tkfocus(parent.win)
		return(NULL)
	})
	tkwait.window(tt)
}


###################
ExecDownload_SatData <- function(datasrc, istart, iend, minlon, maxlon, minlat, maxlat, outdir){

	tkconfigure(main.win, cursor = 'watch');tcl('update')
	ret <- downloadRFE_fun(datasrc, istart, iend, minlon, maxlon, minlat, maxlat, outdir)
	tkconfigure(main.win, cursor = '')
	if(!is.null(ret)){
		if(ret == 0) InsertMessagesTxt(main.txt.out, "Download Done!")
		else if(ret == -1) InsertMessagesTxt(main.txt.out, "Some files could not be downloaded", format = TRUE)
		else InsertMessagesTxt(main.txt.out, "Download Failed!", format = TRUE)
	}else InsertMessagesTxt(main.txt.out, "Download Failed!", format = TRUE)
}

##########
downloadRFE_fun <- function(datasrc, istart, iend, minlon, maxlon, minlat, maxlat, outdir){
	outRet <- 0
	if(datasrc%in%c('10-DAYS TAMSATv3', '10-DAYS TAMSATv2')){
		if(datasrc == '10-DAYS TAMSATv2') url <- 'https://www.tamsat.org.uk/public_data'
		if(datasrc == '10-DAYS TAMSATv3') url <- 'https://www.tamsat.org.uk/public_data/TAMSAT3'
		outdir0 <- file.path(outdir, 'Dekad_TAMSAT_Africa')
		if(!file.exists(outdir0)) dir.create(outdir0, showWarnings = FALSE, recursive = TRUE)
		outdir1 <- file.path(outdir, 'Dekad_TAMSAT_Extracted')
		if(!file.exists(outdir1)) dir.create(outdir1, showWarnings = FALSE, recursive = TRUE)

		deb <- try(as.Date(istart), silent = TRUE)
		if(inherits(deb, "try-error")| is.na(deb)){
			InsertMessagesTxt(main.txt.out, 'Check the start date', format = TRUE)
			return(NULL)
		}
		deb <- strsplit(as.character(deb), '-')
		year1 <- deb[[1]][1]
		mon1 <- deb[[1]][2]
		dek1 <- as.numeric(deb[[1]][3])
		if(dek1 > 3){
			InsertMessagesTxt(main.txt.out, 'Dekad must be between 1 and 3', format = TRUE)
			return(NULL)
		}
		fin <- try(as.Date(iend), silent = TRUE)
		if(inherits(fin, "try-error")| is.na(fin)){
			InsertMessagesTxt(main.txt.out, 'Check the end date', format = TRUE)
			return(NULL)
		}
		fin <- strsplit(as.character(fin), '-')
		year2 <- fin[[1]][1]
		mon2 <- fin[[1]][2]
		dek2 <- as.numeric(fin[[1]][3])
		if(dek2 > 3){
			InsertMessagesTxt(main.txt.out, 'Dekad must be between 1 and 3', format = TRUE)
			return(NULL)
		}

		dates <- seq(as.Date(paste(year1, mon1, dek1, sep = '/')), as.Date(paste(year2, mon2, dek2, sep = '/')), 'day')
		dates <- format(dates[as.numeric(format(dates, '%d')) <= 3], '%Y-%m-%d')

		tcl("update", "idletasks")
		for(fl in dates){
			daty <- strsplit(as.character(fl), '-')
			year <- daty[[1]][1]
			mon <- daty[[1]][2]
			dek <- as.numeric(daty[[1]][3])
			if(datasrc == '10-DAYS TAMSATv2') file0 <- paste('rfe', year, '_', mon, '-dk', dek, '.nc', sep = '')
			if(datasrc == '10-DAYS TAMSATv3') file0 <- paste('rfe', year, '_', mon, '-dk', dek, '.v3.nc', sep = '')
			link <- paste(url, year, mon, file0, sep = '/')
			destfile0 <- file.path(outdir0, file0)
			test <- try(suppressWarnings(readLines(link, n = 1)), silent = TRUE)
			if(inherits(test, "try-error")){
				InsertMessagesTxt(main.txt.out, paste('Cannot open the connection or file does not exist:', file0), format = TRUE)
				outRet <- -1
				next
			}else{
				ret <- try(download.file(link, destfile0, mode = "wb", quiet = TRUE), silent = TRUE)
				if(ret != 0){
					InsertMessagesTxt(main.txt.out, paste('Download failed :', file0), format = TRUE)
					outRet <- -1
					next
				}else{
					InsertMessagesTxt(main.txt.out, paste('Download :', file0, 'done!'))
					if(datasrc == '10-DAYS TAMSATv2'){
						dim.xo <- 2
						dim.yo <- 1
					}
					if(datasrc == '10-DAYS TAMSATv3'){
						dim.xo <- 1
						dim.yo <- 2
					}
					nc <- nc_open(destfile0)
					xm <- nc$dim[[dim.xo]]$vals
					ym <- nc$dim[[dim.yo]]$vals
					xdat <- ncvar_get(nc, varid = nc$var[[1]]$name)
					nc_close(nc)
					xo <- order(xm)
					xm <- xm[xo]
					yo <- order(ym)
					ym <- ym[yo]
					xdat <- xdat[xo, yo]
					idx <- which(xm >= minlon & xm <= maxlon)
					idy <- which(ym >= minlat & ym <= maxlat)
					xm <- xm[idx]
					ym <- ym[idy]
					xdat <- xdat[idx, idy]
					xdat[is.na(xdat)] <- -99
					dx <- ncdim_def("Lon", "degreeE", xm)
					dy <- ncdim_def("Lat", "degreeN", ym)
					rfeout <- ncvar_def('rfe', "mm", list(dx, dy), -99, longname = "TAMSAT 10-days rainfall estimate", prec = "short")
					outfl <- file.path(outdir1, file0)
					nc2 <- nc_create(outfl, rfeout)
					ncvar_put(nc2, rfeout, xdat)
					nc_close(nc2)
					InsertMessagesTxt(main.txt.out, paste('Extraction :', file0, 'over',
										paste('bbox', minlon, minlat, maxlon, maxlat, sep = ':'), 'done!'))
				}
			}
			tcl("update")
		}
	}
	#####################################################

	if(datasrc%in%c('DAILY TAMSATv3', 'DAILY TAMSATv2')){
		if(datasrc == 'DAILY TAMSATv2') url <- 'https://www.tamsat.org.uk/public_data'
		if(datasrc == 'DAILY TAMSATv3') url <- 'https://www.tamsat.org.uk/public_data/TAMSAT3'
		outdir0 <- file.path(outdir, 'Daily_TAMSAT_Africa')
		if(!file.exists(outdir0)) dir.create(outdir0, showWarnings = FALSE, recursive = TRUE)
		outdir1 <- file.path(outdir, 'Daily_TAMSAT_Extracted')
		if(!file.exists(outdir1)) dir.create(outdir1, showWarnings = FALSE, recursive = TRUE)

		deb <- try(as.Date(istart), silent = TRUE)
		if(inherits(deb, "try-error")| is.na(deb)){
			InsertMessagesTxt(main.txt.out, 'Check the start date', format = TRUE)
			return(NULL)
		}
		deb <- strsplit(as.character(deb), '-')
		year1 <- deb[[1]][1]
		mon1 <- deb[[1]][2]
		day1 <- deb[[1]][3]

		fin <- try(as.Date(iend), silent = TRUE)
		if(inherits(fin, "try-error")| is.na(fin)){
			InsertMessagesTxt(main.txt.out, 'Check the end date', format = TRUE)
			return(NULL)
		}
		fin <- strsplit(as.character(fin), '-')
		year2 <- fin[[1]][1]
		mon2 <- fin[[1]][2]
		day2 <- fin[[1]][3]

		dates <- as.character(seq(as.Date(paste(year1, mon1, day1, sep = '-')),
								as.Date(paste(year2, mon2, day2, sep = '-')), 'day'))

		tcl("update", "idletasks")
		for(fl in dates){
			daty <- strsplit(as.character(fl), '-')
			year <- daty[[1]][1]
			mon <- daty[[1]][2]
			day <- daty[[1]][3]
			if(datasrc == 'DAILY TAMSATv2') file0 <- paste('rfe', year, '_', mon, '_', day, '.nc', sep = '')
			if(datasrc == 'DAILY TAMSATv3') file0 <- paste('rfe', year, '_', mon, '_', day, '.v3.nc', sep = '')
			link <- paste(url, year, mon, file0, sep = '/')
			destfile0 <- file.path(outdir0, file0)
			test <- try(suppressWarnings(readLines(link, n = 1)), silent = TRUE)
			if(inherits(test, "try-error")){
				InsertMessagesTxt(main.txt.out, paste('Cannot open the connection or file does not exist:', file0), format = TRUE)
				outRet <- -1
				next
			}else{
				ret <- try(download.file(link, destfile0, mode = "wb", quiet = TRUE), silent = TRUE)
				if(ret != 0){
					InsertMessagesTxt(main.txt.out, paste('Download failed :', file0), format = TRUE)
					outRet <- -1
					next
				}else{
					InsertMessagesTxt(main.txt.out, paste('Download :', file0, 'done!'))
					if(datasrc == 'DAILY TAMSATv2'){
						dim.xo <- 2
						dim.yo <- 1
					}
					if(datasrc == 'DAILY TAMSATv3'){
						dim.xo <- 1
						dim.yo <- 2
					}
					nc <- nc_open(destfile0)
					xm <- nc$dim[[dim.xo]]$vals
					ym <- nc$dim[[dim.yo]]$vals
					xdat <- ncvar_get(nc, varid = nc$var[[1]]$name)
					nc_close(nc)
					xo <- order(xm)
					xm <- xm[xo]
					yo <- order(ym)
					ym <- ym[yo]
					xdat <- xdat[xo, yo]
					idx <- which(xm >= minlon & xm <= maxlon)
					idy <- which(ym >= minlat & ym <= maxlat)
					xm <- xm[idx]
					ym <- ym[idy]
					xdat <- xdat[idx, idy]
					xdat[is.na(xdat)] <- -99
					dx <- ncdim_def("Lon", "degreeE", xm)
					dy <- ncdim_def("Lat", "degreeN", ym)
					rfeout <- ncvar_def('rfe', "mm", list(dx, dy), -99, longname = "TAMSAT daily rainfall estimate", prec = "short")
					outfl <- file.path(outdir1, file0)
					nc2 <- nc_create(outfl, rfeout)
					ncvar_put(nc2, rfeout, xdat)
					nc_close(nc2)
					InsertMessagesTxt(main.txt.out, paste('Extraction :', file0, 'over',
										paste('bbox', minlon, minlat, maxlon, maxlat, sep = ':'), 'done!'))
				}
			}
			tcl("update")
		}
	}

	#####################################################

	if(datasrc%in%c('10-DAYS CHIRPSv2.0', '10-DAYS CHIRPv1.0')){
		if(datasrc == '10-DAYS CHIRPv1.0'){
			url <- 'http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRP/.v1p0/.dekad/.prcp'
			outdir0 <- file.path(outdir, 'Dekad_CHIRP_Extracted')
		}
		if(datasrc == '10-DAYS CHIRPSv2.0'){
			url <- 'http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.dekad/.prcp'
			outdir0 <- file.path(outdir, 'Dekad_CHIRPS_Extracted')
		}
		if(!file.exists(outdir0)) dir.create(outdir0, showWarnings = FALSE, recursive = TRUE)
		mois <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

		area <- paste('X', minlon, maxlon, 'RANGEEDGES', 'Y', minlat, maxlat, 'RANGEEDGES', sep = '/')

		deb <- try(as.Date(istart), silent = TRUE)
		if(inherits(deb, "try-error")| is.na(deb)){
			InsertMessagesTxt(main.txt.out, 'Check the start date', format = TRUE)
			return(NULL)
		}
		dek1 <- as.numeric(strsplit(as.character(deb), '-')[[1]][3])
		if(dek1 > 3){
			InsertMessagesTxt(main.txt.out, 'Dekad must be between 1 and 3', format = TRUE)
			return(NULL)
		}
		fin <- try(as.Date(iend), silent = TRUE)
		if(inherits(fin, "try-error")| is.na(fin)){
			InsertMessagesTxt(main.txt.out, 'Check the end date', format = TRUE)
			return(NULL)
		}
		dek2 <- as.numeric(strsplit(as.character(fin), '-')[[1]][3])
		if(dek2 > 3){
			InsertMessagesTxt(main.txt.out, 'Dekad must be between 1 and 3', format = TRUE)
			return(NULL)
		}

		dates <- seq(deb, fin, 'day')
		dates <- do.call('rbind', strsplit(as.character(dates[which(as.numeric(format(dates, '%d')) <= 3)]), '-'))
		dates <- dates[, c(1:3, 2:3), drop = FALSE]
		dates[, 4] <- mois[as.numeric(dates[, 2])]
		dates[, 3] <- ifelse(dates[, 3] == '01', '1-10', ifelse(dates[, 3] == '02', '11-20', '03'))
		if(length(which(dates[, 3] == '03')) > 0){
			endmois <- apply(matrix(dates[dates[, 3] == '03', 1:2], ncol = 2), 1,
						function(x) rev((28:31)[which(!is.na(as.Date(paste(x[1], x[2], 28:31, sep = '-'))))])[1])
			dates[dates[, 3] == '03', 3] <- paste(21, endmois, sep = '-')
		}

		tcl("update", "idletasks")
		for(j in 1:nrow(dates)){
			time <- paste('T/%28', dates[j, 3], '%20', dates[j, 4], '%20', dates[j, 1], '%29/VALUES', sep = '')
			link <- paste(url, area, time, 'data.nc', sep = '/')
			if(datasrc == '10-DAYS CHIRPv1.0') fileout <- paste('chirpV1.0_', dates[j, 1], dates[j, 2], as.numeric(dates[j, 5]), '.nc', sep = '')
			if(datasrc == '10-DAYS CHIRPSv2.0') fileout <- paste('chirpsV2.0_', dates[j, 1], dates[j, 2], as.numeric(dates[j, 5]), '.nc', sep = '')
			destfile <- file.path(outdir0, fileout)
			ret <- try(download.file(link, destfile, mode = "wb", quiet = TRUE), silent = TRUE)
			if(ret != 0){
				InsertMessagesTxt(main.txt.out, paste('Download failed :', fileout), format = TRUE)
				outRet <- -1
				next
			}else{
				InsertMessagesTxt(main.txt.out, paste('Extraction :', fileout, 'over',
									paste('bbox', minlon, minlat, maxlon, maxlat, sep = ':'), 'done!'))
			}
			tcl("update")
		}
	}

	#####################################################

	if(datasrc == 'DAILY CHIRPSv2.0'){
		url <- 'http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.daily-improved/.global/.0p05/.prcp'
		mois <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
		outdir0 <- file.path(outdir, 'Daily_CHIRPS_Extracted')
		if(!file.exists(outdir0)) dir.create(outdir0, showWarnings = FALSE, recursive = TRUE)

		area <- paste('X', minlon, maxlon, 'RANGEEDGES', 'Y', minlat, maxlat, 'RANGEEDGES', sep = '/')

		deb <- try(as.Date(istart), silent = TRUE)
		if(inherits(deb, "try-error")| is.na(deb)){
			InsertMessagesTxt(main.txt.out, 'Check the start date', format = TRUE)
			return(NULL)
		}
		fin <- try(as.Date(iend), silent = TRUE)
		if(inherits(fin, "try-error")| is.na(fin)){
			InsertMessagesTxt(main.txt.out, 'Check the end date', format = TRUE)
			return(NULL)
		}

		dates <- seq(deb, fin, 'day')
		dates <- do.call('rbind', strsplit(as.character(dates), '-'))
		dates <- dates[, c(1:3, 2), drop = FALSE]
		dates[, 4] <- mois[as.numeric(dates[, 2])]

		tcl("update", "idletasks")
		for(j in 1:nrow(dates)){
			time <- paste('T/%28', as.numeric(dates[j, 3]), '%20', dates[j, 4], '%20', dates[j, 1], '%29/VALUES', sep = '')
			link <- paste(url, area, time, 'data.nc', sep = '/')
			fileout <- paste('chirpsV2.0_', dates[j, 1], dates[j, 2], dates[j, 3], '.nc', sep = '')
			destfile <- file.path(outdir0, fileout)
			ret <- try(download.file(link, destfile, mode = "wb", quiet = TRUE), silent = TRUE)
			if(ret != 0){
				InsertMessagesTxt(main.txt.out, paste('Download failed :', fileout), format = TRUE)
				outRet <- -1
				next
			}else{
				InsertMessagesTxt(main.txt.out, paste('Extraction :',fileout, 'over',
									paste('bbox', minlon, minlat, maxlon, maxlat, sep = ':'), 'done!'))
			}
			tcl("update")
		}
	}

	return(outRet)
}


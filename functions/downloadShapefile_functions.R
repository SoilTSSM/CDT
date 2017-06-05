getCountryShapefile <- function(parent.win){
	#tkentry width, file path
	if(Sys.info()["sysname"] == "Windows"){
		largeur <- 34
		largeur1 <- 35
	}else{
		largeur <- 31
		largeur1 <- 35
	}

	downshp <- fromJSON(file.path(apps.dir, 'init_params', 'Download_Shapefile.json'))
	if(str_trim(downshp$dir2save) == "") downshp$dir2save <- getwd()

	#####
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frGrd0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frGrd1 <- tkframe(tt)

	#####
	frA1 <- tkframe(frGrd0, relief = 'sunken', bd = 2)

	flcntr <- file.path(apps.dir, 'data', 'Afica_Country.txt')
	cntr <- read.table(flcntr, header = TRUE, colClasses = 'character', stringsAsFactors = FALSE)
	cbvalues <- cntr$NAME_ENGLISH
	country <- tclVar(downshp$shp$country)
	level_sub <- tclVar(downshp$shp$level)

	lab1 <- tklabel(frA1, text = 'Select Country', anchor = 'w', justify = 'left')
	cb.country <- ttkcombobox(frA1, values = cbvalues, textvariable = country, width = largeur1)
	separator1 <- ttkseparator(frA1)
	lab2 <- tklabel(frA1, text = 'Select level subdivisions', anchor = 'w', justify = 'left')
	cb.level_sub <- ttkcombobox(frA1, values = 0:4, textvariable = level_sub, width = largeur1)

	###
	tkgrid(lab1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.country, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(separator1, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(lab2, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.level_sub, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.country, 'Select a country')
	status.bar.display(cb.country, TextOutputVar, 'Select a country')
	infobulle(cb.level_sub,'Select level subdivisions such as\ncountry, provinces, districts,... (0 is country level)')
	status.bar.display(cb.level_sub, TextOutputVar, 'Select level subdivisions such as\ncountry, provinces, districts,... (0 is country level)')

	###
	tkbind(cb.country, "<<ComboboxSelected>>", function(){
		cntr_id <- match(tclvalue(country), cbvalues)
		maxlev <- as.numeric(cntr[cntr_id, "max_lev"])
		level_val <- 0:maxlev
		tclvalue(level_sub) <- '0'
		tkconfigure(cb.level_sub, values = level_val)
	})

	#####
	frA2 <- tkframe(frGrd0, relief = 'sunken', bd = 2)

	dir2save <- tclVar(downshp$dir2save)

	txt.file.save <- tklabel(frA2, text = 'Directory to save downloaded files', anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frA2, textvariable = dir2save, width = largeur)
	bt.file.save <- tkbutton(frA2, text = "...")

	###
	tkconfigure(bt.file.save, command = function() fileORdir2Save(dir2save, isFile = FALSE))

	###
	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(en.file.save, 'Enter the full path to directory to save downloaded files')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save downloaded files')
	infobulle(bt.file.save, 'or browse here')

	###
	tkgrid(frA1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(frA2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)

	###
	btOK <- ttkbutton(frGrd1, text = 'Download')

	tkconfigure(btOK, command = function(){
		cntr_choix <- tclvalue(country)
		level <- tclvalue(level_sub)
		outdir <- str_trim(tclvalue(dir2save))

		cntr_id <- match(cntr_choix, cbvalues)
		cntr_iso3 <- cntr[cntr_id, "ISO3"]
		if(outdir == "" | outdir == "NA"){
			tkmessageBox(message = "Browse or enter directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
			if(testConnection()){
				InsertMessagesTxt(main.txt.out, "Downloading.................")
				return(ExecDownload_GADM(cntr_iso3, level, outdir))
			}else{
				InsertMessagesTxt(main.txt.out, 'No Internet connection', format = TRUE)
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
	tkwm.title(tt, 'www.gadm.org')
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
ExecDownload_GADM <- function(countryISO3, level, dirshp){
	tkconfigure(main.win, cursor = 'watch')
	tcl('update')
	ret <- getGADM(countryISO3, level, dirshp)
	if(is.null(ret)){
		InsertMessagesTxt(main.txt.out, 'Download failed', format = TRUE)
	}else{
		if(ret == 0) InsertMessagesTxt(main.txt.out, 'File downloaded successfully')
		else InsertMessagesTxt(main.txt.out, 'Download failed', format = TRUE)
	}
	tkconfigure(main.win, cursor = '')
}

#####################################

getGADM <- function(countryISO3, level, dirshp){
	baseURL <- 'http://biogeo.ucdavis.edu/data/gadm2.8/rds/'
	urlfl <- paste(baseURL, countryISO3, '_adm', level,'.rds', sep = '')
	destfl <- paste(tempfile(), '.rds', sep = '')
	ret <- try(download.file(urlfl, destfl, method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE), silent = TRUE)
	if(ret != 0) return(NULL)

	shp <- readRDS(destfl)
	level <- as.character(level)
	if(level == '0') varname <- c("OBJECTID", "ID_0", "ISO", "NAME_ISO")
	if(level == '1') varname <- c("OBJECTID", "ID_0", "ID_1", "ISO", "NAME_0", "NAME_1", "ENGTYPE_1")
	if(level == '2') varname <- c("OBJECTID", "ID_0", "ID_1", "ID_2", "ISO", "NAME_0", "NAME_1", "NAME_2", "ENGTYPE_2")
	if(level == '3') varname <- c("OBJECTID", "ID_0", "ID_1", "ID_2", "ID_3", "ISO", "NAME_0", "NAME_1", "NAME_2", "NAME_3", "ENGTYPE_3")
	if(level == '4') varname <- c("OBJECTID", "ID_0", "ID_1", "ID_2", "ID_3", "ID_4", "ISO", "NAME_0", "NAME_1", "NAME_2", "NAME_3", "NAME_4", "ENGTYPE_4")
	shp <- shp[, varname]

	writePolyShape(shp, file.path(dirshp, paste(countryISO3, '_adm', level, sep = ''))) ##maptools
	#writeOGR(shp, dsn = dirshp, layer = paste(countryISO3, '_adm', level, sep = ''), driver = "ESRI Shapefile") ##rgdal
	unlink(destfl)
	return(0)
}




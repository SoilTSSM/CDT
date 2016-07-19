getCountryShapefile <- function(parent.win){
	#tkentry width, file path
	if(Sys.info()["sysname"] == "Windows") largeur <- 33
	else largeur <- 31

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frGrd0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frGrd1 <- tkframe(tt)

	frA1 <- tkframe(frGrd0, relief = 'sunken', bd = 2)
	frA2 <- tkframe(frGrd0, relief = 'sunken', bd = 2)
	tkgrid(frA1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(frA2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)

	lab1 <- tklabel(frA1, text = 'Select Country', anchor = 'w', justify = 'left')
	###
	flcntr <- file.path(apps.dir, 'country', 'Afica_Country.txt', fsep = .Platform$file.sep)
	cntr <- read.table(flcntr, header = TRUE, colClasses = 'character', stringsAsFactors = FALSE)

	cbvalues <- cntr$NAME_ENGLISH
	country <- tclVar('Madagascar')
	cb.country <- ttkcombobox(frA1, values = cbvalues, textvariable = country, width = 34)  #
	infobulle(cb.country, 'Choose country')
	status.bar.display(cb.country, TextOutputVar, 'Choose country')

	###
	separator1 <- ttkseparator(frA1)
	lab2 <- tklabel(frA1, text = 'Select level subdivisions', anchor = 'w', justify = 'left')

	level_sub <- tclVar(0)
	cb.level_sub <- ttkcombobox(frA1, values = 0:4, textvariable = level_sub,width = 34)  #
	infobulle(cb.level_sub,'Choose level subdivisions such as\ncountry, provinces, districts,...\n0 is country level')
	status.bar.display(cb.level_sub,TextOutputVar, 'Choose level subdivisions such as country, provinces, districts,...(0 is country level)')

	###
	tkgrid(lab1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.country, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(separator1, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(lab2, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.level_sub,row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###
	tkbind(cb.country,"<<ComboboxSelected>>", function(){
		cntr_id <- match(tclvalue(country), cbvalues)
		maxlev <- as.numeric(cntr[cntr_id,"max_lev"])
		level_val <- 0:maxlev
		tclvalue(level_sub)<-'0'
		tkconfigure(cb.level_sub,values = level_val)
	})

	###
	lab3 <- tklabel(frA2, text = 'Directory to save downloaded files', anchor = 'w', justify = 'left')

	dir2save <- tclVar('')
	enfile.save <- tkentry(frA2, textvariable = dir2save, width = largeur)  #
	btfile.save <- tkbutton.h(frA2, text = "...", TextOutputVar, 'or browse here','')
	tkconfigure(btfile.save, command = function() fileORdir2Save(dir2save, isFile = FALSE))

	infobulle(frA2, 'Enter the full path to directory to save downloaded files')
	status.bar.display(frA2, TextOutputVar, 'Enter the full path to directory to save downloaded files')

	tkgrid(lab3, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(enfile.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(btfile.save, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###
	btOK <- ttkbutton(frGrd1, text = 'Download')
	tkgrid(btOK, row = 0, column = 0, sticky = 'e', padx = 5, pady = 5)
	tkconfigure(btOK, command = function(){
		cntr_choix <- tclvalue(country)
		level <- tclvalue(level_sub)
		outdir <- tclvalue(dir2save)

		cntr_id <- match(cntr_choix,cbvalues)
		cntr_iso3 <- cntr[cntr_id,"ISO3"]
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
				InsertMessagesTxt(main.txt.out, 'No internet connection', format = TRUE)
				return(NULL)
			}
		}
	})

	tkgrid(frGrd0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frGrd1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+',tt.x,'+',tt.y, sep = ''))
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

	tkconfigure(main.win, cursor = 'watch');tcl('update')
	getGADM(countryISO3, level, dirshp)
	tkconfigure(main.win, cursor='')
	InsertMessagesTxt(main.txt.out, "Download Done!")
}

#####################################


getGADM <- function(countryISO3, level, dirshp){
	baseURL <- 'http://biogeo.ucdavis.edu/data/gadm2.7/rds/'
	urlfl <- paste(baseURL, countryISO3, '_adm', level,'.rds', sep = '')
	destfl <- paste(tempfile(), '.rds', sep = '')
	ret <- try(download.file(urlfl, destfl, method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE), silent = TRUE)

	if(ret != 0){
		InsertMessagesTxt(main.txt.out, 'Download failed', format = TRUE)
		return(NULL)
	}else{
		InsertMessagesTxt(main.txt.out, 'File downloaded sucessfully')
	}
	shp <- readRDS(destfl)
	level <- as.character(level)
	if(level == '0') varname <- c("OBJECTID", "ID_0", "ISO", "NAME_0")
	if(level == '1') varname <- c("OBJECTID", "ID_0", "ID_1", "ISO", "NAME_0", "NAME_1", "ENGTYPE_1")
	if(level == '2') varname <- c("OBJECTID", "ID_0", "ID_1", "ID_2", "ISO", "NAME_0", "NAME_1", "NAME_2", "ENGTYPE_2")
	if(level == '3') varname <- c("OBJECTID", "ID_0", "ID_1", "ID_2", "ID_3", "ISO", "NAME_0", "NAME_1", "NAME_2", "NAME_3", "ENGTYPE_3")
	if(level == '4') varname <- c("OBJECTID", "ID_0", "ID_1", "ID_2", "ID_3", "ID_4", "ISO", "NAME_0", "NAME_1", "NAME_2", "NAME_3", "NAME_4", "ENGTYPE_4")
	shp <- shp[,varname]

	writePolyShape(shp, file.path(dirshp, paste(countryISO3, '_adm', level, sep = ''), fsep = .Platform$file.sep)) ##maptools
	#writeOGR(shp, dsn = dirshp, layer = paste(countryISO3, '_adm', level, sep = ''), driver = "ESRI Shapefile") ##rgdal
	unlink(destfl)
}




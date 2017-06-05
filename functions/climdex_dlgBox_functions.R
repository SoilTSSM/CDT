

oneStnDataClimdex <- function(top.win, GeneralParameters){
	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt1)

	###################

	fr.fileformat1 <- ttklabelframe(frMRG0, text = "File Format", labelanchor = "nw", relief = "groove", borderwidth = 2)

	rbffrmt <- tclVar(GeneralParameters$One.series$file.format)
 
	ffrmt1 <- tkradiobutton(fr.fileformat1, text = "One variable", anchor = 'w', justify = 'left')
	ffrmt2 <- tkradiobutton(fr.fileformat1, text = "Rain Tmax Tmin", anchor = 'w', justify = 'left')

	tkconfigure(ffrmt1, variable = rbffrmt, value = "1")
	tkconfigure(ffrmt2, variable = rbffrmt, value = "3")

	tkgrid(ffrmt1, row = 0, column = 0, sticky = "we")
	tkgrid(ffrmt2, row = 1, column = 0, sticky = "we")

	infobulle(ffrmt1, 'In case of single series: The file contains 1 variable')
	status.bar.display(ffrmt1, TextOutputVar, 'In case of single series: The file contains 1 variable')
	infobulle(ffrmt2, 'In case of single series: The file contains Rain, Tmax and Tmin in this order')
	status.bar.display(ffrmt2, TextOutputVar, 'In case of single series:The file contains Rain, Tmax and Tmin in this order')

	###################

	fr.fileformat2 <- ttklabelframe(frMRG0, text = "Dates Format", labelanchor = "nw", relief = "groove", borderwidth = 2)

	rbdtfrmt <- tclVar(GeneralParameters$One.series$date.format)

	dtfrmt1 <- tkradiobutton(fr.fileformat2, text = "YYYYMMDD", anchor = 'w', justify = 'left')
	dtfrmt2 <- tkradiobutton(fr.fileformat2, text = "YYYY MM DD", anchor = 'w', justify = 'left')

	tkconfigure(dtfrmt1, variable = rbdtfrmt, value = "1")
	tkconfigure(dtfrmt2, variable = rbdtfrmt, value = "3")

	tkgrid(dtfrmt1, row = 0, column = 0, sticky = "we")
	tkgrid(dtfrmt2, row = 1, column = 0, sticky = "we")

	infobulle(dtfrmt1, 'In case of single series: dates are merged')
	status.bar.display(dtfrmt1, TextOutputVar, 'In case of single series: dates are merged')
	infobulle(dtfrmt2, 'In case of single series: dates are separated by space, tabulation or CSV format')
	status.bar.display(dtfrmt2, TextOutputVar, 'In case of single series: dates are separated by space, tabulation or CSV format')

	###################

	fr.station <- ttklabelframe(frMRG0, text = "Station", labelanchor = "nw", relief = "groove", borderwidth = 2)

	stn.id <- tclVar(GeneralParameters$One.series$id)
	stn.lon <- tclVar(GeneralParameters$One.series$lon)
	stn.lat <- tclVar(GeneralParameters$One.series$lat)

	txt.stn.id <- tklabel(fr.station, text = 'Name/ID')
	en.stn.id <- tkentry(fr.station, textvariable = stn.id, width = 20)

	txt.stn.lon <- tklabel(fr.station, text = 'Longitude')
	en.stn.lon <- tkentry(fr.station, textvariable = stn.lon, width = 6)

	txt.stn.lat <- tklabel(fr.station, text = 'Latitude')
	en.stn.lat <- tkentry(fr.station, textvariable = stn.lat, width = 6)

	tkgrid(txt.stn.id, row = 0, column = 0, columnspan = 1)
	tkgrid(en.stn.id, row = 0, column = 1, sticky = 'we', columnspan = 3)

	tkgrid(txt.stn.lon, row = 1, column = 0, columnspan = 1)
	tkgrid(en.stn.lon, row = 1, column = 1, sticky = 'we', columnspan = 1)
	tkgrid(txt.stn.lat, row = 1, column = 2, columnspan = 1)
	tkgrid(en.stn.lat, row = 1, column = 3, sticky = 'we', columnspan = 1)

	#####
	tkgrid(fr.fileformat1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.fileformat2, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.station, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(dtfrmt1, 'Name or code of the station')
	status.bar.display(dtfrmt1, TextOutputVar, 'Name or code of the station')

	################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		GeneralParameters$One.series$file.format <<- tclvalue(rbffrmt)
		GeneralParameters$One.series$date.format <<- tclvalue(rbdtfrmt)
		GeneralParameters$One.series$id <<- str_trim(tclvalue(stn.id))
		GeneralParameters$One.series$lon <<- as.numeric(str_trim(tclvalue(stn.lon)))
		GeneralParameters$One.series$lat <<- as.numeric(str_trim(tclvalue(stn.lat)))

		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 5, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################
	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'One Station - Settings')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(top.win)})
	tkwait.window(tt1)
	return(GeneralParameters)
}

#######################################################################################################

netcdfDataClimdexRR <- function(tt, GeneralParameters, ncDIR){
	listOpenFiles <- openFile_ttkcomboList()

	largeur1 <- if(Sys.info()["sysname"] == "Windows")  27 else 25
	###################

	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt1)

	###################

	frDate <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	istart.yrs <- tclVar(GeneralParameters$ncdf.file$start.year)
	istart.mon <- tclVar(GeneralParameters$ncdf.file$start.mon)
	istart.day <- tclVar(GeneralParameters$ncdf.file$start.day)
	iend.yrs <- tclVar(GeneralParameters$ncdf.file$end.year)
	iend.mon <- tclVar(GeneralParameters$ncdf.file$end.mon)
	iend.day <- tclVar(GeneralParameters$ncdf.file$end.day)

	deb.txt <- tklabel(frDate, text = 'Start date', anchor = 'e', justify = 'right')
	fin.txt <- tklabel(frDate, text = 'End date', anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(frDate, text = 'Year')
	mon.txt <- tklabel(frDate, text = 'Month')
	day.txt <- tklabel(frDate, text = 'Day')

	yrs1.v <- tkentry(frDate, width = 4, textvariable = istart.yrs, justify = "right")
	mon1.v <- tkentry(frDate, width = 4, textvariable = istart.mon, justify = "right")
	day1.v <- tkentry(frDate, width = 4, textvariable = istart.day, justify = "right")
	yrs2.v <- tkentry(frDate, width = 4, textvariable = iend.yrs, justify = "right")
	mon2.v <- tkentry(frDate, width = 4, textvariable = iend.mon, justify = "right")
	day2.v <- tkentry(frDate, width = 4, textvariable = iend.day, justify = "right")

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

	infobulle(frDate, 'Start and end date of daily rainfall data')
	status.bar.display(frDate, TextOutputVar, 'Start and end date of daily rainfall data')

	###################

	frFF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	inrfeff <- tclVar(GeneralParameters$ncdf.file$format)
	rfesample <- tclVar(GeneralParameters$ncdf.file$sample)

	txt.ncsample <- tklabel(frFF, text = "Daily rainfall sample file", anchor = 'w', justify = 'left')
	cb.ncsample <- ttkcombobox(frFF, values = unlist(listOpenFiles), textvariable = rfesample, width = largeur1)
	bt.ncsample <- tkbutton(frFF, text = "...")
	txt.inrfeff <- tklabel(frFF, text = 'Input daily rainfall filenames format', anchor = 'w', justify = 'left')
	en.inrfeff <- tkentry(frFF, textvariable = inrfeff, width = largeur1)

	###################

	tkconfigure(bt.ncsample, command = function(){
		initialdir <- if(file.exists(ncDIR)) ncDIR else getwd()
		nc.opfiles <- getOpenNetcdf(main.win, all.opfiles, initialdir = initialdir)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(rfesample) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.ncsample, values = unlist(listOpenFiles), textvariable = rfesample)
		}else return(NULL)
	})

	###################

	tkgrid(txt.ncsample, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.ncsample, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.ncsample, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.inrfeff, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.inrfeff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	status.bar.display(cb.ncsample, TextOutputVar, 'File containing a sample of daily rainfall data in netcdf')
	infobulle(bt.ncsample, 'Browse file if not listed')
	infobulle(en.inrfeff, 'Enter the filename format of daily rainfall data,\nexample: rr_mrg_19830125_CLM.nc')
	status.bar.display(en.inrfeff, TextOutputVar, 'Enter the filename format of daily rainfall data,\nexample: rr_mrg_19830125_CLM.nc')

	###################

	tkgrid(frDate, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(frFF, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)

	################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		GeneralParameters$ncdf.file$start.year <<- as.numeric(str_trim(tclvalue(istart.yrs)))
		GeneralParameters$ncdf.file$start.mon <<- as.numeric(str_trim(tclvalue(istart.mon)))
		GeneralParameters$ncdf.file$start.day <<- as.numeric(str_trim(tclvalue(istart.day)))
		GeneralParameters$ncdf.file$end.year <<- as.numeric(str_trim(tclvalue(iend.yrs)))
		GeneralParameters$ncdf.file$end.mon <<- as.numeric(str_trim(tclvalue(iend.mon)))
		GeneralParameters$ncdf.file$end.day <<- as.numeric(str_trim(tclvalue(iend.day)))
		GeneralParameters$ncdf.file$format <<- str_trim(tclvalue(inrfeff))
		GeneralParameters$ncdf.file$sample <<- str_trim(tclvalue(rfesample))

		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 5, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################
	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'NetCDF Data - Settings')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(GeneralParameters)
}

#######################################################################################################

netcdfDataClimdexTT <- function(tt, GeneralParameters, ncDIR1, ncDIR2){
	listOpenFiles <- openFile_ttkcomboList()

	largeur1 <- if(Sys.info()["sysname"] == "Windows")  27 else 25
	###################

	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt1)

	###################

	frDate <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	istart.yrs <- tclVar(GeneralParameters$ncdf.file$start.year)
	istart.mon <- tclVar(GeneralParameters$ncdf.file$start.mon)
	istart.day <- tclVar(GeneralParameters$ncdf.file$start.day)
	iend.yrs <- tclVar(GeneralParameters$ncdf.file$end.year)
	iend.mon <- tclVar(GeneralParameters$ncdf.file$end.mon)
	iend.day <- tclVar(GeneralParameters$ncdf.file$end.day)

	deb.txt <- tklabel(frDate, text = 'Start date', anchor = 'e', justify = 'right')
	fin.txt <- tklabel(frDate, text = 'End date', anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(frDate, text = 'Year')
	mon.txt <- tklabel(frDate, text = 'Month')
	day.txt <- tklabel(frDate, text = 'Day')

	yrs1.v <- tkentry(frDate, width = 4, textvariable = istart.yrs, justify = "right")
	mon1.v <- tkentry(frDate, width = 4, textvariable = istart.mon, justify = "right")
	day1.v <- tkentry(frDate, width = 4, textvariable = istart.day, justify = "right")
	yrs2.v <- tkentry(frDate, width = 4, textvariable = iend.yrs, justify = "right")
	mon2.v <- tkentry(frDate, width = 4, textvariable = iend.mon, justify = "right")
	day2.v <- tkentry(frDate, width = 4, textvariable = iend.day, justify = "right")

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

	infobulle(frDate, 'Start and end date of daily temperature data')
	status.bar.display(frDate, TextOutputVar, 'Start and end date of daily temperature data')

	###################

	frFF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	inrfeff1 <- tclVar(GeneralParameters$ncdf.file$TX.format)
	inrfeff2 <- tclVar(GeneralParameters$ncdf.file$TN.format)
	TXnc <- tclVar(GeneralParameters$ncdf.file$TX.ncsample)
	TNnc <- tclVar(GeneralParameters$ncdf.file$TN.ncsample)

	txt.ncsample1 <- tklabel(frFF, text = "Daily maximum temperature sample file", anchor = 'w', justify = 'left')
	cb.ncsample1 <- ttkcombobox(frFF, values = unlist(listOpenFiles), textvariable = TXnc, width = largeur1)
	bt.ncsample1 <- tkbutton(frFF, text = "...")
	txt.inrfeff1 <- tklabel(frFF, text = 'Input Tmax filenames format', anchor = 'w', justify = 'left')
	en.inrfeff1 <- tkentry(frFF, textvariable = inrfeff1, width = largeur1)

	txt.ncsample2 <- tklabel(frFF, text = "Daily minimum temperature sample file", anchor = 'w', justify = 'left')
	cb.ncsample2 <- ttkcombobox(frFF, values = unlist(listOpenFiles), textvariable = TNnc, width = largeur1)
	bt.ncsample2 <- tkbutton(frFF, text = "...")
	txt.inrfeff2 <- tklabel(frFF, text = 'Input Tmin filenames format', anchor = 'w', justify = 'left')
	en.inrfeff2 <- tkentry(frFF, textvariable = inrfeff2, width = largeur1)

	###################

	tkconfigure(bt.ncsample1, command = function(){
		initialdir <- if(file.exists(ncDIR1)) ncDIR1 else getwd()
		nc.opfiles <- getOpenNetcdf(main.win, all.opfiles, initialdir = initialdir)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(TXnc) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.ncsample1, values = unlist(listOpenFiles), textvariable = TXnc)
			tkconfigure(cb.ncsample2, values = unlist(listOpenFiles), textvariable = TNnc)
		}else return(NULL)
	})

	tkconfigure(bt.ncsample2, command = function(){
		initialdir <- if(file.exists(ncDIR2)) ncDIR2 else getwd()
		nc.opfiles <- getOpenNetcdf(main.win, all.opfiles, initialdir = initialdir)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(TNnc) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.ncsample1, values = unlist(listOpenFiles), textvariable = TXnc)
			tkconfigure(cb.ncsample2, values = unlist(listOpenFiles), textvariable = TNnc)
		}else return(NULL)
	})

	###################

	tkgrid(txt.ncsample1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.ncsample1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.ncsample1, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.inrfeff1, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.inrfeff1, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.ncsample2, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.ncsample2, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.ncsample2, row = 5, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.inrfeff2, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.inrfeff2, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	status.bar.display(cb.ncsample1, TextOutputVar, 'File containing a sample of daily maximum temperature data in netcdf')
	infobulle(bt.ncsample1, 'Browse file if not listed')
	infobulle(en.inrfeff1, 'Enter the filename format of daily maximum temperature data,\nexample: tmax_mrg_19810101_CLM.nc')
	status.bar.display(en.inrfeff1, TextOutputVar, 'Enter the filename format of daily maximum temperature data,\nexample: tmax_mrg_19810101_CLM.nc')

	status.bar.display(cb.ncsample2, TextOutputVar, 'File containing a sample of daily minimum temperature data in netcdf')
	infobulle(bt.ncsample2, 'Browse file if not listed')
	infobulle(en.inrfeff2, 'Enter the filename format of daily minimum temperature data,\nexample: tmin_mrg_19810101_CLM.nc')
	status.bar.display(en.inrfeff2, TextOutputVar, 'Enter the filename format of daily minimum temperature data,\nexample: tmin_mrg_19810101_CLM.nc')

	###################

	tkgrid(frDate, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(frFF, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)

	################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		GeneralParameters$ncdf.file$start.year <<- as.numeric(str_trim(tclvalue(istart.yrs)))
		GeneralParameters$ncdf.file$start.mon <<- as.numeric(str_trim(tclvalue(istart.mon)))
		GeneralParameters$ncdf.file$start.day <<- as.numeric(str_trim(tclvalue(istart.day)))
		GeneralParameters$ncdf.file$end.year <<- as.numeric(str_trim(tclvalue(iend.yrs)))
		GeneralParameters$ncdf.file$end.mon <<- as.numeric(str_trim(tclvalue(iend.mon)))
		GeneralParameters$ncdf.file$end.day <<- as.numeric(str_trim(tclvalue(iend.day)))
		GeneralParameters$ncdf.file$TX.format <<- str_trim(tclvalue(inrfeff1))
		GeneralParameters$ncdf.file$TN.format <<- str_trim(tclvalue(inrfeff2))
		GeneralParameters$ncdf.file$TX.ncsample <<- str_trim(tclvalue(TXnc))
		GeneralParameters$ncdf.file$TN.ncsample <<- str_trim(tclvalue(TNnc))

		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 5, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################
	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'NetCDF Data - Settings')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(GeneralParameters)
}



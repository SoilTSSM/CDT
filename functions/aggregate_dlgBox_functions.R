
AggregateInputStationData <- function(parent.win, GeneralParameters){

	listOpenFiles <- openFile_ttkcomboList()

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	fr.A <- tkframe(tt, relief = "groove", borderwidth = 2)
	fr.B <- tkframe(tt, relief = "groove", borderwidth = 2)
	tkgrid(fr.A, fr.B)
	tkgrid.configure(fr.A, row = 0, column = 0, sticky = 'news', padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid.configure(fr.B, row = 0, column = 1, sticky = 'news', padx = 5, pady = 5, ipadx = 1, ipady = 1)

	pr.relief.set <- c('sunken', 'sunken', 'sunken', 'sunken', 'sunken')
	for(i in 0:4) assign(paste('fr.A', i, sep = ''), tkframe(fr.A, relief = pr.relief.set[i+1], borderwidth = 2))
	for(i in 0:4) tkgrid(get(paste('fr.A', i, sep = '')))
	for(i in 0:4) tkgrid.configure(get(paste('fr.A', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	################################
	fr.A01 <- tkframe(fr.A0)
	fr.A02 <- tkframe(fr.A0)
	tkgrid(fr.A01, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.A02, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	file.period <- tclVar()
	tclvalue(file.period) <- ifelse(as.character(GeneralParameters$period) == 'daily',
	'Daily data', ifelse(as.character(GeneralParameters$period) == 'dekadal', 'Dekadal data', 'Monthly data'))

	cb.period <- ttkcombobox(fr.A02, values = c('Daily data', 'Dekadal data', 'Monthly data'), textvariable = file.period, width = 25)
	infobulle(cb.period, 'Choose the frequency of data')
	status.bar.display(cb.period, TextOutputVar, 'Choose the frequency of data')
	tkgrid(cb.period)


	###########################
	file.choix1 <- tclVar()
	tclvalue(file.choix1) <- as.character(GeneralParameters$file.io$Values[1])
	file.choix2 <- tclVar()
	tclvalue(file.choix2) <- as.character(GeneralParameters$file.io$Values[2])

	fr.A11 <- tkframe(fr.A1)
	fr.A12 <- tkframe(fr.A1)
	tkgrid(fr.A11, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.A12, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	frA11.txt1 <- tklabel(fr.A11, text = 'Station Data Files Sample')
	tkgrid(frA11.txt1)

	cb.file.stn.sample <- ttkcombobox(fr.A12, values = unlist(listOpenFiles), textvariable = file.choix1, width = 25)
	infobulle(cb.file.stn.sample, 'Choose the file in the list')
	status.bar.display(cb.file.stn.sample, TextOutputVar, 'File containing a sample of data')

	bt.file.stn.sample <- tkbutton.h(fr.A12, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
	tkgrid(cb.file.stn.sample, bt.file.stn.sample)
	tkgrid.configure(cb.file.stn.sample, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.file.stn.sample, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.file.stn.sample, command = function(){
		dat.opfiles <- getOpenFiles(parent.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.choix1) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.file.stn.sample, values = unlist(listOpenFiles), textvariable = file.choix1)
			tkconfigure(cb.file.stn.info, values = unlist(listOpenFiles), textvariable = file.choix2)
		}else{
			return(NULL)
		}
	})

	################################
	fr.A21 <- tkframe(fr.A2)
	fr.A22 <- tkframe(fr.A2)
	tkgrid(fr.A21, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.A22, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	frA21.txt1 <- tklabel(fr.A21, text = 'Stations Information File')
	tkgrid(frA21.txt1)

	cb.file.stn.info <- ttkcombobox(fr.A22, values = unlist(listOpenFiles), textvariable = file.choix2, width = 25)
	infobulle(cb.file.stn.info, 'Choose the file in the list')
	status.bar.display(cb.file.stn.info, TextOutputVar, 'File containing the ids and coordinates of the stations')

	bt.file.stn.info <- tkbutton.h(fr.A22, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
	tkgrid(cb.file.stn.info, bt.file.stn.info)
	tkgrid.configure(cb.file.stn.info, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.file.stn.info, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.file.stn.info, command = function(){
		dat.opfiles <- getOpenFiles(parent.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.choix2) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.file.stn.sample, values = unlist(listOpenFiles), textvariable = file.choix1)
			tkconfigure(cb.file.stn.info, values = unlist(listOpenFiles), textvariable = file.choix2)
		}else{
			return(NULL)
		}
	})
	#############################
	fr.A31 <- tkframe(fr.A3)
	fr.A32 <- tkframe(fr.A3)
	tkgrid(fr.A31, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.A32, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	frA31.txt1 <- tklabel(fr.A31, text = 'Directory containing Stations files')
	tkgrid(frA31.txt1)

	dir.stn <- tclVar(as.character(GeneralParameters$file.io$Values[3]))
	en.dir.stn <- tkentry(fr.A32, textvariable = dir.stn, width = 28) #try dynamic width
	infobulle(en.dir.stn, 'Enter the full path to directory\ncontaining the station files')
	status.bar.display(en.dir.stn, TextOutputVar, 'Enter the full path to directory containing the station files')
	bt.dir.stn <- tkbutton.h(fr.A32, text = "...", TextOutputVar, 'or browse here','')
	tkgrid(en.dir.stn, bt.dir.stn)
	tkgrid.configure(en.dir.stn, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.dir.stn, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.dir.stn, command = function(){
		dir4stn <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[3]), "")
		if(is.na(dir4stn)) tclvalue(dir.stn)<-""
		else tclvalue(dir.stn) <- dir4stn
	})

	################################
	fr.A41 <- tkframe(fr.A4)
	fr.A42 <- tkframe(fr.A4)
	tkgrid(fr.A41, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.A42, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	frA41.txt1 <- tklabel(fr.A41, text = 'File to save aggregated data')
	tkgrid(frA41.txt1)

	file.save1 <- tclVar(as.character(GeneralParameters$file.io$Values[4]))
	en.file.save <- tkentry(fr.A42, textvariable = file.save1, width = 28) #try dynamic width
	infobulle(en.file.save, 'Enter the file to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the file to save aggregated data')
	bt.file.save <- tkbutton.h(fr.A42, text = "...", TextOutputVar, 'or browse here', 'or browse here')
	tkgrid(en.file.save, bt.file.save)
	tkgrid.configure(en.file.save, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.file.save, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.file.save, command = function(){
		filetypes  <-  "{{Text Files} {.txt .TXT}} {{CSV Files} {.csv .CSV}} {{All files} *}"
		if (Sys.info()["sysname"] == "Windows") file2save1 <- tkgetSaveFile(initialfile = "", filetypes = filetypes, defaultextension = TRUE)
		else file2save1 <- tkgetSaveFile(initialfile = "", filetypes = filetypes)
		if(is.na(file2save1)) tclvalue(file.save1) <- as.character(GeneralParameters$file.io$Values[4])
		else tclvalue(file.save1) <- file2save1
	})

	###############################################
	pr.relief.set1 <- c('sunken', 'sunken', 'sunken', 'sunken', 'flat')
	for(i in 0:4) assign(paste('fr.B', i, sep = ''), tkframe(fr.B, relief = pr.relief.set1[i+1], borderwidth = 2))
	for(i in 0:4) tkgrid(get(paste('fr.B', i, sep = '')))
	for(i in 0:4) tkgrid.configure(get(paste('fr.B', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	fr.B01 <- ttklabelframe(fr.B0, text = "File Format", labelanchor = "nw", relief = "flat", borderwidth = 2)
	tkgrid(fr.B01)
	ffrmt1 <- tkradiobutton(fr.B01, text = "One variable", anchor = 'w', justify = 'left')
	infobulle(ffrmt1, 'In case of single serie:\nThe file contains 1 variable')
	status.bar.display(ffrmt1, TextOutputVar, 'In case of single serie: The file contains 1 variable')
	ffrmt2 <- tkradiobutton(fr.B01, text = "Rain Tmax Tmin", anchor = 'w', justify = 'left')
	infobulle(ffrmt2, 'In case of single serie:\nThe file contains Rain, Tmax\nand Tmin in this order')
	status.bar.display(ffrmt2, TextOutputVar, 'In case of single serie:The file contains Rain, Tmax and Tmin in this order')
	tkgrid(ffrmt1, row = 0, column = 0, sticky = "we")
	tkgrid(ffrmt2, row = 1, column = 0, sticky = "we")

	rbffrmt <- tclVar(as.character(GeneralParameters$file.date.format$Values[1]))
	tkconfigure(ffrmt1, variable = rbffrmt, value = "1")
	tkconfigure(ffrmt2, variable = rbffrmt, value = "0")

	fr.B11 <- ttklabelframe(fr.B1, text = "Dates Format", labelanchor = "nw", relief = "flat", borderwidth = 2)
	tkgrid(fr.B11)
	dtfrmt1 <- tkradiobutton(fr.B11, text = "YYYYMMDD", anchor = 'w', justify = 'left')
	infobulle(dtfrmt1, 'In case of single serie:\n dates are merged')
	status.bar.display(dtfrmt1, TextOutputVar, 'In case of single serie: dates are merged')
	dtfrmt2 <- tkradiobutton(fr.B11, text = "YYYY MM DD", anchor = 'w', justify = 'left')
	infobulle(dtfrmt2, 'In case of single serie:\ndates are separated by space\nor tabulation')
	status.bar.display(dtfrmt2, TextOutputVar, 'In case of single serie:dates are separated by space or tabulation')
	tkgrid(dtfrmt1, row = 0, column = 0, sticky = "we")
	tkgrid(dtfrmt2, row = 1, column = 0, sticky = "we")

	rbdtfrmt <- tclVar(as.character(GeneralParameters$file.date.format$Values[2]))
	tkconfigure(dtfrmt1, variable = rbdtfrmt, value = "1")
	tkconfigure(dtfrmt2, variable = rbdtfrmt, value = "0")

	#####################

	infobulle(fr.B2, 'Start and end date for the aggregation')
	status.bar.display(fr.B2, TextOutputVar, 'Start and end date for the aggregation')

	deb.txt <- tklabel(fr.B2, text = 'Start date', anchor = 'e', justify = 'right')
	fin.txt <- tklabel(fr.B2, text = 'End date', anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(fr.B2, text = 'Year')
	mon.txt <- tklabel(fr.B2, text = 'Month')
	lperiod <- tclVar("Day")
	day.txt <- tklabel(fr.B2, text = tclvalue(lperiod), textvariable = lperiod)

	istart.yrs <- tclVar(as.character(GeneralParameters$StartEnd.date$Values[1]))
	istart.mon <- tclVar(as.character(GeneralParameters$StartEnd.date$Values[2]))
	istart.day <- tclVar(as.character(GeneralParameters$StartEnd.date$Values[3]))
	iend.yrs <- tclVar(as.character(GeneralParameters$StartEnd.date$Values[4]))
	iend.mon <- tclVar(as.character(GeneralParameters$StartEnd.date$Values[5]))
	iend.day <- tclVar(as.character(GeneralParameters$StartEnd.date$Values[6]))

	yrs1.v <- tkentry(fr.B2, width = 4, textvariable = istart.yrs, justify = "right")
	mon1.v <- tkentry(fr.B2, width = 4, textvariable = istart.mon, justify = "right")
	day1.v <- tkentry(fr.B2, width = 4, textvariable = istart.day, justify = "right")
	yrs2.v <- tkentry(fr.B2, width = 4, textvariable = iend.yrs, justify = "right")
	mon2.v <- tkentry(fr.B2, width = 4, textvariable = iend.mon, justify = "right")
	day2.v <- tkentry(fr.B2, width = 4, textvariable = iend.day, justify = "right")

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

	tkbind(cb.period,"<<ComboboxSelected>>", function(){
		if(tclvalue(file.period) == 'Daily data'){
			tclvalue(lperiod) <- 'Day'
			tkconfigure(day1.v, state = 'normal')
			tkconfigure(day2.v, state = 'normal')
			tclvalue(iend.day) <- as.character(GeneralParameters$StartEnd.date$Values[6])
		}else if(tclvalue(file.period) == 'Dekadal data'){
			tclvalue(lperiod) <- 'Dekad'
			tkconfigure(day1.v, state = 'normal')
			tkconfigure(day2.v, state = 'normal')
			xend <- as.numeric(as.character(GeneralParameters$StartEnd.date$Values[6]))
			tclvalue(iend.day) <- ifelse(xend > 3, '3', as.character(xend))
		}else if(tclvalue(file.period) == 'Monthly data'){
			tclvalue(lperiod)<-''
			tkconfigure(day1.v, state = 'disabled')
			tkconfigure(day2.v, state = 'disabled')
			tclvalue(iend.day) <- as.character(GeneralParameters$StartEnd.date$Values[6])
		}else{
			tclvalue(lperiod) <- 'Day'
			tkconfigure(day1.v, state = 'normal')
			tkconfigure(day2.v, state = 'normal')
			tclvalue(iend.day) <- as.character(GeneralParameters$StartEnd.date$Values[6])
		}
	})
	#################################
	infobulle(fr.B3, 'Minimum % of non-missing of the stations series to be accepted to the aggregated data')
	status.bar.display(fr.B3, TextOutputVar, 'Minimum % of non-missing of the stations series to be accepted to the aggregated data')

	minperc.lab <- tklabel(fr.B3, text = 'Min percentage (%)',anchor = 'e', justify = 'right')
	minperc <- tclVar(as.character(GeneralParameters$min.perc))
	minperc.ent <- tkentry(fr.B3, width = 4, textvariable = minperc, justify = "right")
	tkgrid(minperc.lab, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(minperc.ent, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)

	#################################
	bt.opt.OK <- tkbutton(fr.B4, text = "OK")
	tkgrid(bt.opt.OK, row = 0, column = 0, sticky = 'w', padx = 15, pady = 5, ipadx = 1, ipady = 1)
	tkconfigure(bt.opt.OK, command = function(){
		if(tclvalue(file.choix1) == ""){
			tkmessageBox(message = "You have to provide a station sample file", icon = "warning", type = "ok")
		}else if(tclvalue(file.choix2) == ""){
			tkmessageBox(message = "You have to provide the stations information", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(dir.stn) == "" | tclvalue(dir.stn) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory containing the stations files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save1) == "" |tclvalue(file.save1) == "NA"){
			tkmessageBox(message = "You have to provide the file name to save aggregated data", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$file.io$Values <<- c(tclvalue(file.choix1), tclvalue(file.choix2), tclvalue(dir.stn), tclvalue(file.save1))
			GeneralParameters$file.date.format$Values <<- c(tclvalue(rbffrmt), tclvalue(rbdtfrmt))
			GeneralParameters$period <<- ifelse(tclvalue(file.period) == 'Daily data', 'daily', ifelse(tclvalue(file.period) == 'Dekadal data', 'dekadal', 'monthly'))
			GeneralParameters$StartEnd.date$Values <<- c(tclvalue(istart.yrs), tclvalue(istart.mon), tclvalue(istart.day), tclvalue(iend.yrs), tclvalue(iend.mon), tclvalue(iend.day))
			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
		}
	})

	bt.opt.CA <- tkbutton(fr.B4, text = "Cancel")
	tkgrid(bt.opt.CA, row = 0, column = 2, sticky = 'e', padx = 15, pady = 5, ipadx = 1, ipady = 1)
	tkconfigure(bt.opt.CA, command = function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+',tt.x,'+',tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Aggregate Stations- Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

################################################################

AggregateOutputStationData <- function(parent.win, GeneralParameters){
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frA <- tkframe(tt, relief = "sunken", borderwidth = 2)
	frB <- tkframe(tt)
	tkgrid(frA)
	tkgrid(frB)
	frA1 <- tkframe(frA)
	frA2 <- tkframe(frA)
	tkgrid(frA1, row = 0, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(frA2, row = 1, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 1)

	frA1.txt <- tklabel(frA1, text = 'Directory contaning the folders Outputs&OriginalData')
	tkgrid(frA1.txt)

	file.save1 <- tclVar(as.character(GeneralParameters$file.io))
	en.file.save <- tkentry(frA2, textvariable = file.save1, width = 45) #try dynamic width
	infobulle(en.file.save, 'Enter the full path of the directory containing\nthe folders Outputs, OriginalData or CorrectedData')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path of the directory containing the folders Outputs, OriginalData or CorrectedData')
	bt.file.save <- tkbutton.h(frA2, text = "...", TextOutputVar, 'or browse here', 'or browse here')
	tkgrid(en.file.save, bt.file.save)
	tkgrid.configure(en.file.save, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.file.save, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(as.character(GeneralParameters$file.io), "")
		if(!file.exists(file2save1)){
			tkmessageBox(message = paste(file2save1, 'does not exist.\n It will be created.',sep = ' '), icon = "warning", type = "ok")
			dir.create(file2save1, recursive = TRUE)
			tclvalue(file.save1) <- file2save1
		}else tclvalue(file.save1) <- file2save1
	})

	bt.OK <- tkbutton(frB, text=" OK ")
	tkgrid(bt.OK, ipadx = 10)
	tkconfigure(bt.OK, command = function(){
		GeneralParameters$file.io <<- tclvalue(file.save1)
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+',tt.x,'+',tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Aggregate data')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

###################################################


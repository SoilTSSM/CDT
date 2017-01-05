

AggregateInputStationData <- function(parent.win, GeneralParameters){
	# largeur <- if (Sys.info()["sysname"] == "Windows") 40 else 40
	if (Sys.info()["sysname"] == "Windows"){
		largeur <- 51
		largeur1 <- 23
		largeur2 <- 22
	}else{
		largeur <- 40
		largeur1 <- 18
		largeur2 <- 21
	}

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)


	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	############################################

	frDate <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(GeneralParameters$period, 
									'daily' = cb.periodVAL[1], 
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3])
	lperiod <- tclVar("Day")
	istart.yrs <- tclVar(GeneralParameters$Date.Range$start.year)
	istart.mon <- tclVar(GeneralParameters$Date.Range$start.mon)
	istart.day <- tclVar(GeneralParameters$Date.Range$start.day)
	iend.yrs <- tclVar(GeneralParameters$Date.Range$end.year)
	iend.mon <- tclVar(GeneralParameters$Date.Range$end.mon)
	iend.day <- tclVar(GeneralParameters$Date.Range$end.day)

	minperc <- tclVar(GeneralParameters$min.perc)

	#########
	cb.period <- ttkcombobox(frDate, values = cb.periodVAL, textvariable = file.period, width = largeur1)
	frtxtDate <- ttklabelframe(frDate, text = "Date Range", relief = 'groove')
	minperc.lab <- tklabel(frDate, text = 'Min percentage (%)',anchor = 'e', justify = 'right')
	minperc.ent <- tkentry(frDate, width = 4, textvariable = minperc, justify = "right")

	#########
	deb.txt <- tklabel(frtxtDate, text = 'Start date', anchor = 'e', justify = 'right')
	fin.txt <- tklabel(frtxtDate, text = 'End date', anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(frtxtDate, text = 'Year')
	mon.txt <- tklabel(frtxtDate, text = 'Month')
	day.txt <- tklabel(frtxtDate, text = tclvalue(lperiod), textvariable = lperiod)
	yrs1.v <- tkentry(frtxtDate, width = 4, textvariable = istart.yrs, justify = "right")
	mon1.v <- tkentry(frtxtDate, width = 4, textvariable = istart.mon, justify = "right")
	day1.v <- tkentry(frtxtDate, width = 4, textvariable = istart.day, justify = "right")
	yrs2.v <- tkentry(frtxtDate, width = 4, textvariable = iend.yrs, justify = "right")
	mon2.v <- tkentry(frtxtDate, width = 4, textvariable = iend.mon, justify = "right")
	day2.v <- tkentry(frtxtDate, width = 4, textvariable = iend.day, justify = "right")

	#########
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

	#########
	tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(minperc.lab, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(minperc.ent, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frtxtDate, row = 0, column = 2, sticky = 'we', rowspan = 2, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#########
	infobulle(cb.period, 'Choose the frequency of data')
	status.bar.display(cb.period, TextOutputVar, 'Choose the frequency of data')
	infobulle(frtxtDate, 'Start and end date to take account')
	status.bar.display(frtxtDate, TextOutputVar, 'Start and end date to take account')
	ret0 <- lapply(list(minperc.lab, minperc.ent), function(x) infobulle(x, 'Minimum % of non-missing of the stations series\nto be accepted to the aggregated data'))
	ret0 <- lapply(list(minperc.lab, minperc.ent), function(x) status.bar.display(x, TextOutputVar, 'Minimum % of non-missing of the stations series\nto be accepted to the aggregated data'))

	#########
	tkbind(cb.period, "<<ComboboxSelected>>", function(){
		if(tclvalue(file.period) == 'Daily data'){
			tclvalue(lperiod) <- 'Day'
			tkconfigure(day1.v, state = 'normal')
			tkconfigure(day2.v, state = 'normal')
			# tclvalue(iend.day) <- GeneralParameters$Date.Range$end.day
		}else if(tclvalue(file.period) == 'Dekadal data'){
			tclvalue(lperiod) <- 'Dekad'
			tkconfigure(day1.v, state = 'normal')
			tkconfigure(day2.v, state = 'normal')
			# xend <- as.numeric(GeneralParameters$Date.Range$end.day)
			# tclvalue(iend.day) <- ifelse(xend > 3, '3', as.character(xend))
		}else if(tclvalue(file.period) == 'Monthly data'){
			tclvalue(lperiod) <- 'Day'
			tkconfigure(day1.v, state = 'disabled')
			tkconfigure(day2.v, state = 'disabled')
			# tclvalue(iend.day) <- GeneralParameters$Date.Range$end.day
		}else{
			tclvalue(lperiod) <- 'Day'
			tkconfigure(day1.v, state = 'normal')
			tkconfigure(day2.v, state = 'normal')
			# tclvalue(iend.day) <- GeneralParameters$Date.Range$end.day
		}
	})

	############################################
	frSave <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

	file.save1 <- tclVar(GeneralParameters$IO.files$File2Save)

	txt.file.save <- tklabel(frSave, text = 'File to save aggregated data', anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frSave, textvariable = file.save1, width = largeur)
	bt.file.save <- tkbutton(frSave, text = "...")

	#####

	tkconfigure(bt.file.save, command = function(){
		filetypes  <-  "{{Text Files} {.txt .TXT}} {{CSV Files} {.csv .CSV}} {{All files} *}"
		if (Sys.info()["sysname"] == "Windows") file2save1 <- tkgetSaveFile(initialfile = "", filetypes = filetypes, defaultextension = TRUE)
		else file2save1 <- tkgetSaveFile(initialfile = "", filetypes = filetypes)
		tclvalue(file.save1) <- if(is.na(file2save1)) GeneralParameters$IO.files$File2Save else file2save1
	})

	#####

	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.file.save, 'Enter the full path to the file to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to the file to save result')
	infobulle(bt.file.save, 'or browse here')
	status.bar.display(bt.file.save, TextOutputVar, 'or browse here')

	############################################
	frData <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

	data.type <- tclVar(GeneralParameters$data.type)

	txt.dataType <- tklabel(frData, text = 'Format of input files', anchor = 'w', justify = 'left')
	cb.dataType <- ttkcombobox(frData, values = c("Multiple Files", "Single File"), textvariable = data.type, width = largeur1)
	bt.dataType <- tkbutton(frData, text = "Input File Setting", width = largeur2)

	if(tclvalue(data.type) == "Multiple Files") dataType.Fun <- "multipleFileCDTFormat"
	if(tclvalue(data.type) == "Single File") dataType.Fun <- "singleFileCDTFormat"

	settingdone <- NULL
	tkconfigure(bt.dataType, command = function(){
		dataType.Fun <- match.fun(dataType.Fun)
		GeneralParameters <<- dataType.Fun(tt, GeneralParameters, tclvalue(file.period))
		settingdone <<- 1
	})

	tkgrid(txt.dataType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.dataType, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.dataType, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(frData, 'Format of input files to be converted to CDT format')
	status.bar.display(frData, TextOutputVar, 'Format of input files to be converted to CDT format')


	tkbind(cb.dataType,"<<ComboboxSelected>>", function(){
		if(tclvalue(data.type) == "Multiple Files") dataType.Fun <- "multipleFileCDTFormat"
		if(tclvalue(data.type) == "Single File") dataType.Fun <- "singleFileCDTFormat"

		tkconfigure(bt.dataType, command = function(){
			dataType.Fun <- match.fun(dataType.Fun)
			GeneralParameters <<- dataType.Fun(tt, GeneralParameters, tclvalue(file.period))
			settingdone <<- 1
		})
	})

	############################################
	tkgrid(frDate, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 1, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frData, row = 2, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	#######

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(file.save1)) == "" | str_trim(tclvalue(file.save1)) == "NA"){
			tkmessageBox(message = "Provide the file to save the formated data", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$period <<- switch(str_trim(tclvalue(file.period)), 
			 									'Daily data' = 'daily',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')
			GeneralParameters$data.type <<- str_trim(tclvalue(data.type))
			GeneralParameters$IO.files$File2Save <<- str_trim(tclvalue(file.save1))
			GeneralParameters$min.perc <<- as.numeric(str_trim(tclvalue(minperc)))

			GeneralParameters$Date.Range$start.year <<- as.numeric(str_trim(tclvalue(istart.yrs)))
			GeneralParameters$Date.Range$start.mon <<- as.numeric(str_trim(tclvalue(istart.mon)))
			GeneralParameters$Date.Range$start.day <<- as.numeric(str_trim(tclvalue(istart.day)))
			GeneralParameters$Date.Range$end.year <<- as.numeric(str_trim(tclvalue(iend.yrs)))
			GeneralParameters$Date.Range$end.mon <<- as.numeric(str_trim(tclvalue(iend.mon)))
			GeneralParameters$Date.Range$end.day <<- as.numeric(str_trim(tclvalue(iend.day)))

			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
		}
	})

	#######

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################
	
	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Format CDT input data')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

################################################################

multipleFileCDTFormat <- function(top.win, GeneralParameters, speriod){
	listOpenFiles <- openFile_ttkcomboList()
	largeur <- if (Sys.info()["sysname"] == "Windows") 25 else 25

	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt1)

	###################

	frFileFormat <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

	####
	fr.fileformat1 <- ttklabelframe(frFileFormat, text = "File Format", labelanchor = "nw", relief = "groove", borderwidth = 2)

	rbffrmt <- tclVar(GeneralParameters$Multiple.File$file.format)

	ffrmt1 <- tkradiobutton(fr.fileformat1, text = "One variable", anchor = 'w', justify = 'left')
	ffrmt2 <- tkradiobutton(fr.fileformat1, text = "Rain Tmax Tmin", anchor = 'w', justify = 'left')

	tkconfigure(ffrmt1, variable = rbffrmt, value = "1")
	tkconfigure(ffrmt2, variable = rbffrmt, value = "3")

	tkgrid(ffrmt1, row = 0, column = 0, sticky = "we")
	tkgrid(ffrmt2, row = 1, column = 0, sticky = "we")

	infobulle(ffrmt1, 'In case of single serie: The file contains 1 variable')
	status.bar.display(ffrmt1, TextOutputVar, 'In case of single serie: The file contains 1 variable')
	infobulle(ffrmt2, 'In case of single serie: The file contains\nRain, Tmax and Tmin in this order')
	status.bar.display(ffrmt2, TextOutputVar, 'In case of single serie:The file contains Rain, Tmax and Tmin in this order')

	#####
	fr.fileformat2 <- ttklabelframe(frFileFormat, text = "Dates Format", labelanchor = "nw", relief = "groove", borderwidth = 2)

	rbdtfrmt <- tclVar(GeneralParameters$Multiple.File$date.format)

	if(speriod == 'Daily data') {txtdtfrmt1 <- "YYYYMMDD"; txtdtfrmt2 <- "YYYY MM DD"}
	if(speriod == 'Dekadal data') {txtdtfrmt1 <- "YYYYMMD"; txtdtfrmt2 <- "YYYY MM D"}
	if(speriod == 'Monthly data') {txtdtfrmt1 <- "YYYYMM"; txtdtfrmt2 <- "YYYY MM"}

	dtfrmt1 <- tkradiobutton(fr.fileformat2, text = txtdtfrmt1, anchor = 'w', justify = 'left')
	dtfrmt2 <- tkradiobutton(fr.fileformat2, text = txtdtfrmt2, anchor = 'w', justify = 'left', width = 12)
	dtfrmt3 <- tklabel(fr.fileformat2, text = '')

	tkconfigure(dtfrmt1, variable = rbdtfrmt, value = "1")
	tkconfigure(dtfrmt2, variable = rbdtfrmt, value = "3")

	tkgrid(dtfrmt1, row = 0, column = 0, sticky = "we")
	tkgrid(dtfrmt2, row = 1, column = 0, sticky = "we")
	tkgrid(dtfrmt3, row = 3, column = 0, sticky = "we")

	infobulle(dtfrmt1, 'In case of single serie:\n dates are grouped')
	status.bar.display(dtfrmt1, TextOutputVar, 'In case of single serie: dates are grouped')
	infobulle(dtfrmt2, 'In case of single serie:\ndates are separated by space,\ntabulation or CSV format')
	status.bar.display(dtfrmt2, TextOutputVar, 'In case of single serie: dates are separated by space, tabulation or CSV format')

	###################
	tkgrid(fr.fileformat1, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.fileformat2, row = 0, column = 1, sticky = 'nswe', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###################

	frInput <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

	dir.stn <- tclVar(GeneralParameters$IO.files$STN.dir)
	file.sample <- tclVar(GeneralParameters$IO.files$STN.sample.file)
	file.coords <- tclVar(GeneralParameters$IO.files$STN.coords.file)
	include.elv <- tclVar(GeneralParameters$Multiple.File$include.elev)

	txt.dir.stn <- tklabel(frInput, text = 'Directory of Station data files', anchor = 'w', justify = 'left')
	en.dir.stn <- tkentry(frInput, textvariable = dir.stn)
	bt.dir.stn <- tkbutton(frInput, text = "...")

	txt.file.stn.sample <- tklabel(frInput, text = 'Station Data Files Sample', anchor = 'w', justify = 'left')
	cb.file.stn.sample <- ttkcombobox(frInput, values = unlist(listOpenFiles), textvariable = file.sample, width = largeur)
	bt.file.stn.sample <- tkbutton(frInput, text = "...")

	txt.file.stn.info <- tklabel(frInput, text = 'Station Coordinates File', anchor = 'w', justify = 'left')
	cb.file.stn.info <- ttkcombobox(frInput, values = unlist(listOpenFiles), textvariable = file.coords, width = largeur)
	bt.file.stn.info <- tkbutton(frInput, text = "...")

	chk.include.elv <- tkcheckbutton(frInput, variable = include.elv, text = 'Include elevation data', anchor = 'w', justify = 'left')

	###########
	tkconfigure(bt.dir.stn, command = function(){
		dir4stn <- tk_choose.dir(GeneralParameters$IO.files$STN.dir, "")
		tclvalue(dir.stn) <- if(!is.na(dir4stn)) dir4stn else ""
	})

	tkconfigure(bt.file.stn.sample, command = function(){
		dat.opfiles <- getOpenFiles(tt1, all.opfiles, initialdir = tclvalue(dir.stn))
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.sample) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.file.stn.sample, values = unlist(listOpenFiles), textvariable = file.sample)
			tkconfigure(cb.file.stn.info, values = unlist(listOpenFiles), textvariable = file.coords)
		}else return(NULL)
	})

	tkconfigure(bt.file.stn.info, command = function(){
		dat.opfiles <- getOpenFiles(tt1, all.opfiles, filetype = 'csv')
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.coords) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.file.stn.sample, values = unlist(listOpenFiles), textvariable = file.sample)
			tkconfigure(cb.file.stn.info, values = unlist(listOpenFiles), textvariable = file.coords)
		}else return(NULL)
	})

	###########

	tkgrid(txt.dir.stn, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.dir.stn, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.stn, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.file.stn.sample, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.file.stn.sample, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.stn.sample, row = 3, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.file.stn.info, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.file.stn.info, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.stn.info, row = 5, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(chk.include.elv, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###########
	infobulle(en.dir.stn, 'Enter the full path to directory\ncontaining the station files')
	status.bar.display(en.dir.stn, TextOutputVar, 'Enter the full path to directory containing the station files')
	infobulle(en.dir.stn, 'or browse here')
	status.bar.display(en.dir.stn, TextOutputVar, 'or browse here')

	infobulle(cb.file.stn.sample, 'Choose the file in the list')
	status.bar.display(cb.file.stn.sample, TextOutputVar, 'File containing a sample of data')
	infobulle(txt.file.stn.sample, 'Browse file if not listed')
	status.bar.display(txt.file.stn.sample, TextOutputVar, 'Browse file if not listed')

	infobulle(cb.file.stn.info, 'Choose the file in the list')
	status.bar.display(cb.file.stn.info, TextOutputVar, 'File containing the ids and coordinates of the stations')
	infobulle(bt.file.stn.info, 'Browse file if not listed')
	status.bar.display(bt.file.stn.info, TextOutputVar, 'Browse file if not listed')

	infobulle(chk.include.elv, 'Include elevation data into the output CDT format')
	status.bar.display(chk.include.elv, TextOutputVar, 'Include elevation data into the output CDT format')

	###################
	tkgrid(frInput, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frFileFormat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	################################

	bt.prm.OK <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(dir.stn)) == "" | str_trim(tclvalue(dir.stn)) == "NA"){
			tkmessageBox(message = "Choose or enter the  directory containing the STN files", icon = "warning", type = "ok")
			tkwait.window(tt1)
		}else if(str_trim(tclvalue(file.sample)) == ""){
			tkmessageBox(message = "You have to provide a sample file", icon = "warning", type = "ok")
			tkwait.window(tt1)
		}else if(str_trim(tclvalue(file.coords)) == ""){
			tkmessageBox(message = "Select the file containing the coordinates", icon = "warning", type = "ok")
			tkwait.window(tt1)
		}else{
			GeneralParameters$IO.files$STN.dir <<- str_trim(tclvalue(dir.stn))
			GeneralParameters$IO.files$STN.sample.file <<- str_trim(tclvalue(file.sample))
			GeneralParameters$IO.files$STN.coords.file <<- str_trim(tclvalue(file.coords))
			GeneralParameters$Multiple.File$file.format <<- tclvalue(rbffrmt)
			GeneralParameters$Multiple.File$date.format <<- tclvalue(rbdtfrmt)
			GeneralParameters$Multiple.File$include.elev <<- switch(tclvalue(include.elv), '0' = FALSE, '1' = TRUE)

			tkgrab.release(tt1)
			tkdestroy(tt1)
			tkfocus(top.win)
		}
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
	tkwm.title(tt1, 'Multiple Files - Settings')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(top.win)})
	tkwait.window(tt1)
	return(GeneralParameters)
}

################################################################

singleFileCDTFormat <- function(top.win, GeneralParameters, speriod){
	listOpenFiles <- openFile_ttkcomboList()
	largeur <- if (Sys.info()["sysname"] == "Windows") 30 else 30

	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt1)

	###################

	frInput <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

	file.stnfl <- tclVar(GeneralParameters$IO.files$STN.single.file)
	file.coords <- tclVar(GeneralParameters$IO.files$STN.coords.file)
	include.elv <- tclVar(GeneralParameters$Single.File$include.elev)
	coords.infile <- tclVar(GeneralParameters$Single.File$coords.included)

	statecrds <- if(tclvalue(coords.infile) == '1') 'disabled' else 'normal'
	txt.stnfl <- tklabel(frInput, text = 'Station data file', anchor = 'w', justify = 'left')
	cb.stnfl <- ttkcombobox(frInput, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur)
	bt.stnfl <- tkbutton(frInput, text = "...")

	chk.coords.infile <- tkcheckbutton(frInput, variable = coords.infile, text = 'Coordinates included in the data', anchor = 'w', justify = 'left')
	txt.coords <- tklabel(frInput, text = 'Station coordinates file', anchor = 'w', justify = 'left')
	cb.coords <- ttkcombobox(frInput, values = unlist(listOpenFiles), textvariable = file.coords, state = statecrds, width = largeur)
	bt.coords <- tkbutton(frInput, text = "...", state = statecrds)

	chk.include.elv <- tkcheckbutton(frInput, variable = include.elv, text = 'Include elevation data', anchor = 'w', justify = 'left')

	######
	tkconfigure(bt.stnfl, command = function(){
		dat.opfiles <- getOpenFiles(tt1, all.opfiles, filetype = 'csv')
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.stnfl) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.coords, values = unlist(listOpenFiles), textvariable = file.coords)
		}else return(NULL)
	})

	tkconfigure(bt.coords, command = function(){
		dat.opfiles <- getOpenFiles(tt1, all.opfiles, filetype = 'csv')
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.coords) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.coords, values = unlist(listOpenFiles), textvariable = file.coords)
		}else return(NULL)
	})

	######

	tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(chk.coords.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	tkgrid(txt.coords, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.coords, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.coords, row = 4, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(chk.include.elv, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	###################
	infobulle(cb.stnfl, 'Choose the file in the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the station data to be formated')
	infobulle(bt.stnfl, 'Browse file if not listed')
	status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')

	infobulle(cb.coords, 'Choose the file in the list')
	status.bar.display(cb.coords, TextOutputVar, 'Choose the file containing the station coordinates')
	infobulle(bt.coords, 'Browse file if not listed')
	status.bar.display(bt.coords, TextOutputVar, 'Browse file if not listed')

	infobulle(chk.include.elv, 'Include elevation data into the output CDT format')
	status.bar.display(chk.include.elv, TextOutputVar, 'Include elevation data into the output CDT format')

	###################
	tkbind(chk.coords.infile, "<Button-1>", function(){
		if(tclvalue(coords.infile) == '1'){
			statecrds <- 'normal'
			statecrds1 <- 'disabled'
			if(tclvalue(include.elv) == '0') stateElv <- 'disabled'
		}else{
			statecrds <- 'disabled'
			statecrds1 <- 'normal'
			stateElv <- if(tclvalue(include.elv) == '0') 'disabled' else 'normal'
		}
		tkconfigure(cb.coords, state = statecrds)
		tkconfigure(bt.coords, state = statecrds)
		tkconfigure(en.col.lon, state = statecrds1)
		tkconfigure(en.col.lat, state = statecrds1)
		tkconfigure(en.col.elv, state = stateElv)
	})

	###################

	frColIdx <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

	stn.id <- tclVar(GeneralParameters$Single.File$col.stn.id)
	stn.lon <- tclVar(GeneralParameters$Single.File$col.stn.lon)
	stn.lat <- tclVar(GeneralParameters$Single.File$col.stn.lat)
	stn.elv <- tclVar(GeneralParameters$Single.File$col.stn.elv)
	stn.year <- tclVar(GeneralParameters$Single.File$col.year)
	stn.mon <- tclVar(GeneralParameters$Single.File$col.month)
	stn.day <- tclVar(GeneralParameters$Single.File$col.day.dek)
	stn.data <- tclVar(GeneralParameters$Single.File$col.start.data)

	nb.column <- tclVar()
	if(speriod == 'Daily data'){
		cb.nbcolVAL <- c('1 column', '31 columns')
		tclvalue(nb.column) <- switch(str_trim(GeneralParameters$Single.File$nb.column), 
										'1' = cb.nbcolVAL[1], 
										'31' = cb.nbcolVAL[2],
												cb.nbcolVAL[2])
		statemon <- 'normal'
		statedaydek <- if(tclvalue(nb.column) == '1 column') 'normal' else 'disabled'
	}
	if(speriod == 'Dekadal data'){
		cb.nbcolVAL <- c('1 column', '3 columns', '36 columns')
		tclvalue(nb.column) <- switch(str_trim(GeneralParameters$Single.File$nb.column), 
										'1' = cb.nbcolVAL[1], 
										'3' = cb.nbcolVAL[2], 
										'36' = cb.nbcolVAL[3],
												cb.nbcolVAL[3])
		statemon <- if(tclvalue(nb.column) == '36 columns') 'disabled' else 'normal'
		statedaydek <- if(tclvalue(nb.column) == '1 column') 'normal' else 'disabled'
	}
	if(speriod == 'Monthly data'){
		cb.nbcolVAL <- c('1 column', '12 columns')
		tclvalue(nb.column) <- switch(str_trim(GeneralParameters$Single.File$nb.column), 
										'1' = cb.nbcolVAL[1], 
										'12' = cb.nbcolVAL[2],
												cb.nbcolVAL[2])
		statemon <- if(tclvalue(nb.column) == '12 columns') 'disabled' else 'normal'
		statedaydek <- 'disabled'
	}

	stateCrds <- if(GeneralParameters$Single.File$coords.included) 'normal' else 'disabled'

	if(GeneralParameters$Single.File$include.elev){
		stateElv <- if(GeneralParameters$Single.File$coords.included) 'normal' else 'disabled'
	}else stateElv <- 'disabled'

	txt.col.idx <- tklabel(frColIdx, text = 'Column Index', anchor = 'w', justify = 'left')

	txt.col.id <- tklabel(frColIdx, text = 'COL.ID', anchor = 'e', justify = 'right')
	txt.col.data <- tklabel(frColIdx, text = 'COL.DATA', anchor = 'e', justify = 'right')
	txt.col.lon <- tklabel(frColIdx, text = 'COL.LON', anchor = 'e', justify = 'right')
	txt.col.lat <- tklabel(frColIdx, text = 'COL.LAT', anchor = 'e', justify = 'right')
	txt.col.elv <- tklabel(frColIdx, text = 'COL.ELEV', anchor = 'e', justify = 'right')
	txt.col.year <- tklabel(frColIdx, text = 'COL.YEAR', anchor = 'e', justify = 'right')
	txt.col.mon <- tklabel(frColIdx, text = 'COL.MONTH', anchor = 'e', justify = 'right')
	txt.col.day <- tklabel(frColIdx, text = 'COL.DAY/DEK', anchor = 'e', justify = 'right')

	en.col.id <- tkentry(frColIdx, textvariable = stn.id, width = 2)
	en.col.data <- tkentry(frColIdx, textvariable = stn.data, width = 2)
	en.col.lon <- tkentry(frColIdx, textvariable = stn.lon, width = 2, state = stateCrds)
	en.col.lat <- tkentry(frColIdx, textvariable = stn.lat, width = 2, state = stateCrds)
	en.col.elv <- tkentry(frColIdx, textvariable = stn.elv, width = 2, state = stateElv)
	en.col.year <- tkentry(frColIdx, textvariable = stn.year, width = 2)
	en.col.mon <- tkentry(frColIdx, textvariable = stn.mon, width = 2, state = statemon)
	en.col.day <- tkentry(frColIdx, textvariable = stn.day, width = 2, state = statedaydek)

	txt.nb.col <- tklabel(frColIdx, text = "Data column number", anchor = 'e', justify = 'right')
	cb.nb.col <- ttkcombobox(frColIdx, values = cb.nbcolVAL, textvariable = nb.column, width = 10)


	###################
	tkgrid(txt.col.idx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)

	tkgrid(txt.col.id, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.col.id, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.col.data, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.col.data, row = 3, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.col.lon, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.col.lon, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.col.lat, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.col.lat, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.col.elv, row = 3, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.col.elv, row = 3, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.col.year, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.col.year, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.col.mon, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.col.mon, row = 2, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.col.day, row = 3, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.col.day, row = 3, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.nb.col, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(cb.nb.col, row = 4, column = 3, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 5, ipadx = 1, ipady = 1)

	###
	infobulle(en.col.id, 'Column index containing the station ID')
	status.bar.display(en.col.id, TextOutputVar, 'Column index containing the station ID')
	infobulle(en.col.data, 'Column index from which observed data start')
	status.bar.display(en.col.data, TextOutputVar, 'Column index from which observed data start')

	infobulle(en.col.lon, 'Column index containing the longitude')
	status.bar.display(en.col.lon, TextOutputVar, 'Column index containing the longitude')
	infobulle(en.col.lat, 'Column index containing the latitude')
	status.bar.display(en.col.lat, TextOutputVar, 'Column index containing the latitude')
	infobulle(en.col.elv, 'Column index containing the elevation data')
	status.bar.display(en.col.elv, TextOutputVar, 'Column index containing the elevation data')

	infobulle(en.col.year, 'Column index containing the years')
	status.bar.display(en.col.year, TextOutputVar, 'Column index containing the years')
	infobulle(en.col.mon, 'Column index containing the months')
	status.bar.display(en.col.mon, TextOutputVar, 'Column index containing the months')
	infobulle(en.col.day, 'Column index containing the days or dekad')
	status.bar.display(en.col.day, TextOutputVar, 'Column index containing the days or dekad')

	infobulle(cb.nb.col, 'Number of column containing the data')
	status.bar.display(cb.nb.col, TextOutputVar, 'Number of column containing the data')

	###################

	tkbind(chk.include.elv, "<Button-1>", function(){
		if(tclvalue(include.elv) == '0'){
			stateElv <- if(tclvalue(coords.infile) == '1') 'normal' else 'disabled'
		}else{
			if(tclvalue(coords.infile) == '0') stateElv <- 'disabled'
		}
		tkconfigure(en.col.elv, state = stateElv)
	})

	tkbind(cb.nb.col, "<<ComboboxSelected>>", function(){
		if(speriod == 'Daily data'){
			statemon <- 'normal'
			statedaydek <- if(tclvalue(nb.column) == '1 column') 'normal' else 'disabled'
		}
		if(speriod == 'Dekadal data'){
			statemon <- if(tclvalue(nb.column) == '36 columns') 'disabled' else 'normal'
			statedaydek <- if(tclvalue(nb.column) == '1 column') 'normal' else 'disabled'
		}
		if(speriod == 'Monthly data'){
			statemon <- if(tclvalue(nb.column) == '12 columns') 'disabled' else 'normal'
			statedaydek <- 'disabled'
		}
		tkconfigure(en.col.mon, state = statemon)
		tkconfigure(en.col.day, state = statedaydek)
	})

	###################
	tkgrid(frInput, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frColIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	################################

	bt.prm.OK <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(file.stnfl)) == ""){
			tkmessageBox(message = "Select the file containing the observed data", icon = "warning", type = "ok")
		}else if(tclvalue(coords.infile) == '0' & str_trim(tclvalue(file.coords)) == ""){
			tkmessageBox(message = "Select the file containing the coordinates", icon = "warning", type = "ok")
			tkwait.window(tt1)
		}else{
			GeneralParameters$IO.files$STN.single.file <<- str_trim(tclvalue(file.stnfl))
			GeneralParameters$IO.files$STN.coords.file <<- str_trim(tclvalue(file.coords))
			GeneralParameters$Single.File$include.elev <<- switch(tclvalue(include.elv), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Single.File$coords.included <<- switch(tclvalue(coords.infile), '0' = FALSE, '1' = TRUE)

			GeneralParameters$Single.File$col.stn.id <<- as.numeric(str_trim(tclvalue(stn.id)))
			GeneralParameters$Single.File$col.stn.lon <<- as.numeric(str_trim(tclvalue(stn.lon)))
			GeneralParameters$Single.File$col.stn.lat <<- as.numeric(str_trim(tclvalue(stn.lat)))
			GeneralParameters$Single.File$col.stn.elv <<- as.numeric(str_trim(tclvalue(stn.elv)))
			GeneralParameters$Single.File$col.year <<- as.numeric(str_trim(tclvalue(stn.year)))
			GeneralParameters$Single.File$col.month <<- as.numeric(str_trim(tclvalue(stn.mon)))
			GeneralParameters$Single.File$col.day.dek <<- as.numeric(str_trim(tclvalue(stn.day)))
			GeneralParameters$Single.File$col.start.data <<- as.numeric(str_trim(tclvalue(stn.data)))
			if(speriod == 'Daily data'){
				GeneralParameters$Single.File$nb.column <<- switch(str_trim(tclvalue(nb.column)),
																'1 column' = 1, '31 columns' = 31)
			}
			if(speriod == 'Dekadal data'){
				GeneralParameters$Single.File$nb.column <<- switch(str_trim(tclvalue(nb.column)),
																'1 column' = 1, '3 columns' = 3, '36 columns' = 36)
			}
			if(speriod == 'Monthly data'){
				GeneralParameters$Single.File$nb.column <<- switch(str_trim(tclvalue(nb.column)),
																'1 column' = 1, '12 columns' = 12)
			}

			tkgrab.release(tt1)
			tkdestroy(tt1)
			tkfocus(top.win)
		}
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
	tkwm.title(tt1, 'Single File - Settings')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(top.win)})
	tkwait.window(tt1)
	return(GeneralParameters)
}

################################################################

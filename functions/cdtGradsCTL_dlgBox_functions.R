
grads_create.ctl_getParams <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur0 <- 36
		largeur1 <- 43
	}else{
		largeur0 <- 35
		largeur1 <- 24
	}

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################

	frtimestep <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Monthly data', 'Annual data')
	# cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data', 'Annual data')
	tclvalue(file.period) <- switch(GeneralParameters$tstep,
									'daily' = cb.periodVAL[1],
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3],
									'annual' = cb.periodVAL[4])

	cb.period <- ttkcombobox(frtimestep, values = cb.periodVAL, textvariable = file.period, width = largeur1)

	tkgrid(cb.period, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)

	infobulle(cb.period, 'Select the time step of the data')
	status.bar.display(cb.period, TextOutputVar, 'Select the time step of the data')

	###########
	tkbind(cb.period, "<<ComboboxSelected>>", function(){
		tclvalue(day.txtVar) <- ifelse(str_trim(tclvalue(file.period)) == 'Dekadal data', 'Dek', 'Day')
		stateday <- if(str_trim(tclvalue(file.period))%in%c('Monthly data', 'Annual data')) 'disabled' else 'normal'
		statemon <- if(str_trim(tclvalue(file.period)) == 'Annual data') 'disabled' else 'normal'
		tkconfigure(en.day1, state = stateday)
		tkconfigure(en.day2, state = stateday)
		tkconfigure(en.mon1, state = statemon)
		tkconfigure(en.mon2, state = statemon)

		tclvalue(istart.day) <- if(as.numeric(str_trim(tclvalue(istart.day))) > 3) 1 else tclvalue(istart.day)
		tclvalue(iend.day) <- if(as.numeric(str_trim(tclvalue(iend.day))) > 3) 3 else tclvalue(iend.day)
	})

	############################################

	frameInData <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	input.DataF <- tclVar(GeneralParameters$nc$dir)

	txt.indata <- tklabel(frameInData, text = 'Directory containing the NetCDF data', anchor = 'w', justify = 'left')
	set.indata <- tkbutton(frameInData, text = "Settings")
	cb.en.indata <- tkentry(frameInData, textvariable = input.DataF, width = largeur0)
	bt.indata <- tkbutton(frameInData, text = "...")

	settingNCF <- GeneralParameters$settingNCF
	tkconfigure(set.indata, command = function(){
		GeneralParameters[["nc"]] <<- grads.getInfoNetcdfData(tt, GeneralParameters[["nc"]],
															str_trim(tclvalue(input.DataF)))
		settingNCF <<- 1
	})

	tkconfigure(bt.indata, command = function(){
		dirnc <- tk_choose.dir(getwd(), "")
		tclvalue(input.DataF) <- if(dirnc%in%c("", "NA") | is.na(dirnc)) "" else dirnc
	})

	############ 
	tkgrid(txt.indata, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.indata, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.en.indata, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.indata, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	############ 
	infobulle(cb.en.indata, 'Enter the full path to directory containing the netcdf files')
	status.bar.display(cb.en.indata, TextOutputVar, 'Enter the full path to directory containing the netcdf files')
	infobulle(bt.indata, 'or browse here')
	status.bar.display(bt.indata, TextOutputVar, 'or browse here')

	############################################

	frDate <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	istart.yrs <- tclVar(GeneralParameters$date$year1)
	istart.mon <- tclVar(GeneralParameters$date$mon1)
	istart.day <- tclVar(GeneralParameters$date$day1)
	iend.yrs <- tclVar(GeneralParameters$date$year2)
	iend.mon <- tclVar(GeneralParameters$date$mon2)
	iend.day <- tclVar(GeneralParameters$date$day2)

	txtdek <- ifelse(GeneralParameters$tstep == 'dekadal', 'Dek', 'Day')
	day.txtVar <- tclVar(txtdek)
	stateday <- if(GeneralParameters$tstep%in%c("monthly", "annual")) 'disabled' else 'normal'
	statemon <- if(GeneralParameters$tstep == "annual") 'disabled' else 'normal'

	txt.Date <- tklabel(frDate, text = "Date Range")
	txt.deb <- tklabel(frDate, text = 'Start date', anchor = 'e', justify = 'right')
	txt.fin <- tklabel(frDate, text = 'End date', anchor = 'e', justify = 'right')
	txt.yrs <- tklabel(frDate, text = 'Year')
	txt.mon <- tklabel(frDate, text = 'Month')
	txt.day <- tklabel(frDate, text = tclvalue(day.txtVar), textvariable = day.txtVar)
	en.yrs1 <- tkentry(frDate, width = 5, textvariable = istart.yrs, justify = "right")
	en.mon1 <- tkentry(frDate, width = 5, textvariable = istart.mon, justify = "right", state = statemon)
	en.day1 <- tkentry(frDate, width = 5, textvariable = istart.day, justify = "right", state = stateday)
	en.yrs2 <- tkentry(frDate, width = 5, textvariable = iend.yrs, justify = "right")
	en.mon2 <- tkentry(frDate, width = 5, textvariable = iend.mon, justify = "right", state = statemon)
	en.day2 <- tkentry(frDate, width = 5, textvariable = iend.day, justify = "right", state = stateday)

	tkgrid(txt.Date, row = 0, column = 0, sticky = 'ew', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.deb, row = 2, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.fin, row = 3, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.yrs, row = 1, column = 1, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.mon, row = 1, column = 2, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.day, row = 1, column = 3, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.yrs1, row = 2, column = 1, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.mon1, row = 2, column = 2, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.day1, row = 2, column = 3, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.yrs2, row = 3, column = 1, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.mon2, row = 3, column = 2, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.day2, row = 3, column = 3, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################

	frSave <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.save <- tclVar(GeneralParameters$out.ctl)

	txt.file.save <- tklabel(frSave, text = 'File to save the descriptor file', anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frSave, textvariable = file.save, width = largeur0)
	bt.file.save <- tkbutton(frSave, text = "...")

	#########
	tkconfigure(bt.file.save, command = function(){
		filetypes <- "{{Control File} {.ctl .CTL}} {{Text Files} {.txt .TXT}} {{All files} *}"
		if(Sys.info()["sysname"] == "Windows"){
			file2save <- tkgetSaveFile(initialdir = getwd(), initialfile = "",
										filetypes = filetypes, defaultextension = TRUE)
		}else{
			file2save <- tkgetSaveFile(initialdir = getwd(), initialfile = "", filetypes = filetypes)
		}
		tclvalue(file.save) <- if(!is.na(file2save)) file2save else ""
	})

	#########
	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	#########

	infobulle(en.file.save, 'Enter the full path of the file to save the GrADS Data Descriptor File')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path of the file to save the GrADS Data Descriptor File')
	infobulle(bt.file.save, 'or browse here')
	status.bar.display(bt.file.save, TextOutputVar, 'or browse here')

	############################################
	tkgrid(frtimestep, row = 0, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frDate, row = 2, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	############################################
	
	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(input.DataF))%in%c("", "NA")){
			tkmessageBox(message = "No input data found", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save)%in%c("", "NA")){
			tkmessageBox(message = "Provide the file to save the GrADS Data Descriptor File", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(is.null(settingNCF)){
				tkmessageBox(message = "You have to set the netcdf files parameters", icon = "warning", type = "ok")
				tkwait.window(tt)
		}else{
			GeneralParameters$tstep <<- switch(str_trim(tclvalue(file.period)), 
												'Daily data' = 'daily',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly',
												'Annual data' = 'annual')

			GeneralParameters$nc$dir <<- str_trim(tclvalue(input.DataF))
			GeneralParameters$date$year1 <<- as.numeric(str_trim(tclvalue(istart.yrs)))
			GeneralParameters$date$mon1 <<- as.numeric(str_trim(tclvalue(istart.mon)))
			GeneralParameters$date$day1 <<- as.numeric(str_trim(tclvalue(istart.day)))
			GeneralParameters$date$year2 <<- as.numeric(str_trim(tclvalue(iend.yrs)))
			GeneralParameters$date$mon2 <<- as.numeric(str_trim(tclvalue(iend.mon)))
			GeneralParameters$date$day2 <<- as.numeric(str_trim(tclvalue(iend.day)))
			GeneralParameters$out.ctl <<- str_trim(tclvalue(file.save))

			GeneralParameters$settingNCF <<- settingNCF

			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
		}
	})

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

	############################3
	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5 - tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5 - tt.h*0.5)
	tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt)
	tkwm.title(tt, 'GrADS Data Descriptor File')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}


###################################################################################################

grads.getInfoNetcdfData <- function(tt, Parameters, ncDIR){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		largeur1 <- 43
		largeur2 <- 28
	}else{
		largeur1 <- 34
		largeur2 <- 31
	}
	###################

	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt1)

	###################

	frFF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	inrfeff <- tclVar(Parameters$format)
	rfesample <- tclVar(Parameters$sample)

	txt.ncsample <- tklabel(frFF, text = "Netcdf data sample file", anchor = 'w', justify = 'left')
	cb.ncsample <- ttkcombobox(frFF, values = unlist(listOpenFiles), textvariable = rfesample, width = largeur1)
	bt.ncsample <- tkbutton(frFF, text = "...")
	txt.inrfeff <- tklabel(frFF, text = 'Netcdf data filenames format', anchor = 'w', justify = 'left')
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

	tkgrid(txt.ncsample, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.ncsample, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.ncsample, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.inrfeff, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.inrfeff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	status.bar.display(cb.ncsample, TextOutputVar, 'File containing a sample of the data in netcdf')
	infobulle(bt.ncsample, 'Browse file if not listed')
	infobulle(en.inrfeff, 'Enter the filename format of netcdf data (see below)')
	status.bar.display(en.inrfeff, TextOutputVar, 'Enter the filename format of netcdf data,  (see below)')

	###################

	frHH <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	yscr.Help <- tkscrollbar(frHH, repeatinterval = 4, command = function(...) tkyview(txta.Help, ...))
	txta.Help <- tktext(frHH, bg = "white", font = "courier", cursor = "", wrap = "word",
							height = 7, width = largeur2,
							yscrollcommand = function(...) tkset(yscr.Help, ...))

	tkgrid(txta.Help, yscr.Help)
	tkgrid.configure(yscr.Help, sticky = "ns")
	tkgrid.configure(txta.Help, sticky = 'nswe')

	TXTA1 <- "Replace the year, month, dekad or day by\n"
	TXTA2 <- "Year: %Y; Month: %M; Dekad: %T; Day: %D\n"
	TXTA3 <- "Example:\n"
	TXTA4a <- "rr_mrg_19810731_ALL.nc"
	TXTA4b <- "rr_mrg_%Y%M%D_ALL.nc\n\n"
	TXTA5a <- "rfe_1983-09-dk1.nc"
	TXTA5b <- "rfe_%Y-%M-dk%T.nc"
	TXTAby <- " => "

	font1 <- tkfont.create(family = "times", size = 12, weight = "bold")
	font2 <- tkfont.create(family = "times", size = 12, weight = "bold", underline = TRUE)
	font3 <- tkfont.create(family = "courier", size = 11)
	font4 <- tkfont.create(family = "courier", size = 11, weight = "bold")

	tktag.configure(txta.Help, "font1.tag", font = font1, foreground = 'red')
	tktag.configure(txta.Help, "font2.tag", font = font2)
	tktag.configure(txta.Help, "font3.tag", font = font3)
	tktag.configure(txta.Help, "font4.tag", font = font4, foreground = 'red')

	tkinsert(txta.Help, "end", TXTA1, "font3.tag")
	tkinsert(txta.Help, "end", TXTA2, "font1.tag")
	tkinsert(txta.Help, "end", TXTA3, "font2.tag")
	tkinsert(txta.Help, "end", TXTA4a, "font3.tag")
	tkinsert(txta.Help, "end", TXTAby, "font4.tag")
	tkinsert(txta.Help, "end", TXTA4b, "font3.tag")
	tkinsert(txta.Help, "end", TXTA5a, "font3.tag")
	tkinsert(txta.Help, "end", TXTAby, "font4.tag")
	tkinsert(txta.Help, "end", TXTA5b, "font3.tag")

	tkconfigure(txta.Help, state = "disabled")

	###################

	tkgrid(frFF, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(frHH, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)

	################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(rfesample)) == ""){
			tkmessageBox(message = "You have to provide a sample file", icon = "warning", type = "ok")
			tkwait.window(tt1)
		}else{
			Parameters$format <<- str_trim(tclvalue(inrfeff))
			Parameters$sample <<- str_trim(tclvalue(rfesample))

			tkgrab.release(tt1)
			tkdestroy(tt1)
			tkfocus(tt)
		}
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################
	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5 - tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5 - tt.h*0.5)
	tkwm.geometry(tt1, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'NetCDF Data - Settings')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(Parameters)
}



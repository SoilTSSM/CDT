CPT.convert_getParams <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur0 <- 46
		largeur1 <- 43
		largeur2 <- 9
	}else{
		largeur0 <- 35
		largeur1 <- 34
		largeur2 <- 9
	}

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################

	frdatatype <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	DataType <- tclVar()
	CbdatatypeVAL <- c('CDT stations data format', 'NetCDF gridded data')
	tclvalue(DataType) <- switch(GeneralParameters$data.type,
								'cdtstation' = CbdatatypeVAL[1],
								'cdtnetcdf' = CbdatatypeVAL[2])

	txt.datatyp <- tklabel(frdatatype, text = 'Format of input data', anchor = 'w', justify = 'left')
	cb.datatyp <- ttkcombobox(frdatatype, values = CbdatatypeVAL, textvariable = DataType, width = largeur1)

	tkgrid(txt.datatyp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.datatyp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.datatyp, 'Select the format of the input data')
	status.bar.display(cb.datatyp, TextOutputVar, 'Select the format of the input data')

	###############

	tkbind(cb.datatyp, "<<ComboboxSelected>>", function(){
		tkdestroy(cb.en.indata)
		tclvalue(input.DataF) <- ''

		###
		stateSetNC <- if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data') "normal" else "disabled"
		tkconfigure(set.indata, state = stateSetNC)

		###
		if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
			tclvalue(txt.INDat.var) <- 'File containing stations data'

			cb.en.indata <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.DataF, width = largeur1)

			######
			tkconfigure(bt.indata, command = function(){
				dat.opfiles <- getOpenFiles(tt, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(input.DataF) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.en.indata, values = unlist(listOpenFiles), textvariable = input.DataF)
				}else return(NULL)
			})

			######
			infobulle(cb.en.indata, 'Select the file containing the stations data in CDT format')
			status.bar.display(cb.en.indata, TextOutputVar, 'Select the file containing the stations data in CDT format')
			infobulle(bt.indata, 'Browse file if not listed')
			status.bar.display(bt.indata, TextOutputVar, 'Browse file if not listed')
		}

		###
		if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data'){
			tclvalue(txt.INDat.var) <- 'Directory of the NetCDF files'

			cb.en.indata <- tkentry(frameInData, textvariable = input.DataF, width = largeur0)

			######
			tkconfigure(set.indata, command = function(){
				GeneralParameters[["cdtnetcdf"]] <<- CPT.getInfoNetcdfData(tt, GeneralParameters[["cdtnetcdf"]],
																			str_trim(tclvalue(input.DataF)))
				settingNCF <<- 1
			})

			tkconfigure(bt.indata, command = function(){
				dirnc <- tk_choose.dir(getwd(), "")
				tclvalue(input.DataF) <- if(dirnc%in%c("", "NA")) "" else dirnc
			})

			######
			infobulle(cb.en.indata, 'Enter the full path to directory containing the netcdf files')
			status.bar.display(cb.en.indata, TextOutputVar, 'Enter the full path to directory containing the netcdf files')
			infobulle(bt.indata, 'or browse here')
			status.bar.display(bt.indata, TextOutputVar, 'or browse here')
		}

		#######
		tkgrid(cb.en.indata, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		tkfocus(tt)
	})

	############################################

	frameInData <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	if(GeneralParameters$data.type == 'cdtstation'){
		input.DataF <- tclVar(GeneralParameters$cdtstation)
		txt.INDat <- 'File containing stations data'
		stateSetNC <- "disabled"
	}else{
		input.DataF <- tclVar(GeneralParameters$cdtnetcdf$dir)
		txt.INDat <- 'Directory of the NetCDF files'
		stateSetNC <- "normal"
	}
	txt.INDat.var <- tclVar(txt.INDat)

	##############
	txt.indata <- tklabel(frameInData, text = tclvalue(txt.INDat.var), textvariable = txt.INDat.var, anchor = 'w', justify = 'left')
	set.indata <- tkbutton(frameInData, text = "Settings", state = stateSetNC)

	if(GeneralParameters$data.type == 'cdtstation'){
		cb.en.indata <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.DataF, width = largeur1)
	}else{
		cb.en.indata <- tkentry(frameInData, textvariable = input.DataF, width = largeur0)
	}
	bt.indata <- tkbutton(frameInData, text = "...")

	############
	settingNCF <- GeneralParameters$settingNCF
	tkconfigure(set.indata, command = function(){
		GeneralParameters[["cdtnetcdf"]] <<- CPT.getInfoNetcdfData(tt, GeneralParameters[["cdtnetcdf"]],
																	str_trim(tclvalue(input.DataF)))
		settingNCF <<- 1
	})

	tkconfigure(bt.indata, command = function(){
		if(GeneralParameters$data.type == 'cdtstation'){
			dat.opfiles <- getOpenFiles(tt, all.opfiles)
			if(!is.null(dat.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'ascii'
				AllOpenFilesData[[nopf+1]] <<- dat.opfiles

				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
				tclvalue(input.DataF) <- AllOpenFilesData[[nopf+1]][[1]]
				tkconfigure(cb.en.indata, values = unlist(listOpenFiles), textvariable = input.DataF)
			}else return(NULL)
		}else{
			dirnc <- tk_choose.dir(getwd(), "")
			tclvalue(input.DataF) <- if(dirnc%in%c("", "NA") | is.na(dirnc)) "" else dirnc
		}
	})

	############ 
	tkgrid(txt.indata, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.indata, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.en.indata, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.indata, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	#############
	if(GeneralParameters$data.type == 'cdtstation'){
		infobulle(cb.en.indata, 'Select the file containing the stations data in CDT format')
		status.bar.display(cb.en.indata, TextOutputVar, 'Select the file containing the stations data in CDT format')
		infobulle(bt.indata, 'Browse file if not listed')
		status.bar.display(bt.indata, TextOutputVar, 'Browse file if not listed')
	}else{
		infobulle(cb.en.indata, 'Enter the full path to directory containing the netcdf files')
		status.bar.display(cb.en.indata, TextOutputVar, 'Enter the full path to directory containing the netcdf files')
		infobulle(bt.indata, 'or browse here')
		status.bar.display(bt.indata, TextOutputVar, 'or browse here')
	}

	############################################

	frCPTInfo <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	cptinfo.Name <- tclVar(GeneralParameters$cptinfo$name)
	cptinfo.Unit <- tclVar(GeneralParameters$cptinfo$units)
	cptinfo.Miss <- tclVar(GeneralParameters$cptinfo$missval)

	txt.cpt.name <- tklabel(frCPTInfo, text = 'Field Name', anchor = 'e', justify = 'right')
	en.cpt.name <- tkentry(frCPTInfo, textvariable = cptinfo.Name, width = largeur2)
	txt.cpt.unit <- tklabel(frCPTInfo, text = 'Field Units', anchor = 'e', justify = 'right')
	en.cpt.unit <- tkentry(frCPTInfo, textvariable = cptinfo.Unit, width = largeur2)
	txt.cpt.miss <- tklabel(frCPTInfo, text = 'Missing Value', anchor = 'e', justify = 'right')
	en.cpt.miss <- tkentry(frCPTInfo, textvariable = cptinfo.Miss, width = largeur2)

	#########
	tkgrid(txt.cpt.name, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.cpt.name, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.cpt.unit, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.cpt.unit, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.cpt.miss, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.cpt.miss, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################

	frSave <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.save <- tclVar(GeneralParameters$output)

	txt.file.save <- tklabel(frSave, text = 'File to save the output', anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frSave, textvariable = file.save, width = largeur0)
	bt.file.save <- tkbutton(frSave, text = "...")

	#########
	tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save, isFile = TRUE))

	#########
	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	#########

	infobulle(en.file.save, 'Enter the full path of the file to save the result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path of the file to save the result')
	infobulle(bt.file.save, 'or browse here')
	status.bar.display(bt.file.save, TextOutputVar, 'or browse here')

	############################################
	tkgrid(frdatatype, row = 0, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frCPTInfo, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
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
			tkmessageBox(message = "Provide the file to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data' & is.null(settingNCF)){
				tkmessageBox(message = "You have to set the netcdf files parameters", icon = "warning", type = "ok")
				tkwait.window(tt)
		}else{
			GeneralParameters$data.type <<- switch(str_trim(tclvalue(DataType)),
												'CDT stations data format' = 'cdtstation',
												'NetCDF gridded data' = 'cdtnetcdf')

			if(str_trim(tclvalue(DataType)) == 'CDT stations data format')
				GeneralParameters$cdtstation <<- str_trim(tclvalue(input.DataF))

			if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data')
				GeneralParameters$cdtnetcdf$dir <<- str_trim(tclvalue(input.DataF))

			GeneralParameters$cptinfo$name <<- str_trim(tclvalue(cptinfo.Name))
			GeneralParameters$cptinfo$units <<- str_trim(tclvalue(cptinfo.Unit))
			GeneralParameters$cptinfo$missval <<- str_trim(tclvalue(cptinfo.Miss))

			GeneralParameters$output <<- str_trim(tclvalue(file.save))

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
	tkwm.title(tt, 'Convert to CPT Format')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

###################################################################################################

CPT.getInfoNetcdfData <- function(tt, Parameters, ncDIR){
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

	TXTA1 <- "Replace the year, month, dekad, pentad or day by\n"
	TXTA2 <- "Year: %Y; Month: %M; Dekad: %T; Pentad: %P; Day: %D\n"
	TXTA3 <- "Example:\n"
	TXTA4a <- "outTS_1981-07_1982-06.nc"
	TXTA4b <- "outTS_%Y-%M_%Y-%M.nc\n\n"
	TXTA5a <- "onset_19830901.nc"
	TXTA5b <- "onset_%Y%M%D.nc"
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


computeWB_getParams <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		# largeur0 <- 12
		largeur1 <- 45
		largeur2 <- 55
		largeur3 <- 52
	}else{
		# largeur0 <- 10
		largeur1 <- 30
		largeur2 <- 41
		largeur3 <- 40
	}

	MOIS <- format(ISOdate(2014, 1:12, 1), "%B")

	############################################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################

	frdatatype <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	DataType <- tclVar()
	CbdatatypeVAL <- c('CDT stations data format', 'CDT dataset format (gridded)')
	tclvalue(DataType) <- switch(GeneralParameters$data.type,
								'cdtstation' = CbdatatypeVAL[1],
								'cdtdataset' = CbdatatypeVAL[2])

	txt.datatyp <- tklabel(frdatatype, text = 'Format of input data', anchor = 'w', justify = 'left')
	cb.datatyp <- ttkcombobox(frdatatype, values = CbdatatypeVAL, textvariable = DataType, width = largeur1)

	tkgrid(txt.datatyp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.datatyp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.datatyp, 'Select the format of the input data')
	status.bar.display(cb.datatyp, TextOutputVar, 'Select the format of the input data')

	###############

	tkbind(cb.datatyp, "<<ComboboxSelected>>", function(){
		tkdestroy(cb.en.etp)
		tclvalue(input.Etp) <- ''

		tkdestroy(cb.en.prec)
		tclvalue(input.Prec) <- ''

		###
		if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
			tclvalue(txt.INEtp.var) <- 'File containing stations daily PET data'
			tclvalue(txt.INPrec.var) <- 'File containing stations daily Precip data'

			cb.en.etp <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Etp, width = largeur3)
			cb.en.prec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur3)

			######
			tkconfigure(bt.etp, command = function(){
				dat.opfiles <- getOpenFiles(tt, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(input.Etp) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.en.etp, values = unlist(listOpenFiles), textvariable = input.Etp)
					tkconfigure(cb.en.prec, values = unlist(listOpenFiles), textvariable = input.Prec)
				}else return(NULL)
			})

			tkconfigure(bt.prec, command = function(){
				dat.opfiles <- getOpenFiles(tt, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(input.Prec) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.en.etp, values = unlist(listOpenFiles), textvariable = input.Etp)
					tkconfigure(cb.en.prec, values = unlist(listOpenFiles), textvariable = input.Prec)
				}else return(NULL)
			})

			######
			infobulle(cb.en.etp, 'Select the file containing the daily pontetial evapotranspiration')
			status.bar.display(cb.en.etp, TextOutputVar, 'Select the file containing the daily pontetial evapotranspiration')
			infobulle(cb.en.prec, 'Select the file containing the daily precipitation')
			status.bar.display(cb.en.prec, TextOutputVar, 'Select the file containing the daily precipitation')

			infobulle(bt.etp, 'Browse file if not listed')
			status.bar.display(bt.etp, TextOutputVar, 'Browse file if not listed')
			infobulle(bt.prec, 'Browse file if not listed')
			status.bar.display(bt.prec, TextOutputVar, 'Browse file if not listed')
		}

		###
		if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)'){
			tclvalue(txt.INEtp.var) <- 'Index file (*.rds) for daily PET data'
			tclvalue(txt.INPrec.var) <- 'Index file (*.rds) for daily Precip data'

			cb.en.etp <- tkentry(frameInData, textvariable = input.Etp, width = largeur2)
			cb.en.prec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2)

			######
			tkconfigure(bt.etp, command = function(){
				filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
				tclvalue(input.Etp) <- if(path.rds%in%c("", "NA")) "" else path.rds
			})

			tkconfigure(bt.prec, command = function(){
				filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
				tclvalue(input.Prec) <- if(path.rds%in%c("", "NA")) "" else path.rds
			})

			######
			infobulle(cb.en.etp, 'Enter the full path to the file <daily pontetial evapotranspiration dataset name>.rds')
			status.bar.display(cb.en.etp, TextOutputVar, 'Enter the full path to the file <daily pontetial evapotranspiration dataset name>.rds')
			infobulle(cb.en.prec, 'Enter the full path to the file <daily precipitation dataset name>.rds')
			status.bar.display(cb.en.prec, TextOutputVar, 'Enter the full path to the file <daily precipitation dataset name>.rds')

			infobulle(bt.etp, 'or browse here')
			status.bar.display(bt.etp, TextOutputVar, 'or browse here')
			infobulle(bt.prec, 'or browse here')
			status.bar.display(bt.prec, TextOutputVar, 'or browse here')
		}

		#######
		if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
			txtSaveHelp <- 'Enter the full path of the file to save the result'
			tclvalue(fileORdir) <- 'File to save the result'
			isFile <- TRUE
		}else{
			tclvalue(fileORdir) <- 'Directory to save the result'
			txtSaveHelp <- 'Enter the full path to the directory to save the result'
			isFile <- FALSE
		}

		tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save, isFile = isFile))

		infobulle(en.file.save, txtSaveHelp)
		status.bar.display(en.file.save, TextOutputVar, txtSaveHelp)
		infobulle(bt.file.save, 'or browse here')
		status.bar.display(bt.file.save, TextOutputVar, 'or browse here')

		#######
		tkgrid(cb.en.prec, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(cb.en.etp, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkfocus(tt)
	})

	############################################

	frameInData <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	if(GeneralParameters$data.type == 'cdtstation'){
		input.Etp <- tclVar(GeneralParameters$cdtstation$etp)
		input.Prec <- tclVar(GeneralParameters$cdtstation$prec)
		txt.INEtp <- 'File containing stations daily PET data'
		txt.INPrec <- 'File containing stations daily Precip data'
	}else{
		input.Etp <- tclVar(GeneralParameters$cdtdataset$etp)
		input.Prec <- tclVar(GeneralParameters$cdtdataset$prec)
		txt.INEtp <- 'Index file (*.rds) for daily PET data'
		txt.INPrec <- 'Index file (*.rds) for daily Precip data'
	}
	txt.INEtp.var <- tclVar(txt.INEtp)
	txt.INPrec.var <- tclVar(txt.INPrec)


	##############
	txt.prec <- tklabel(frameInData, text = tclvalue(txt.INPrec.var), textvariable = txt.INPrec.var, anchor = 'w', justify = 'left')

	if(GeneralParameters$data.type == 'cdtstation'){
		cb.en.prec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur3)
	}else{
		cb.en.prec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2)
	}
	bt.prec <- tkbutton(frameInData, text = "...")

	############

	tkconfigure(bt.prec, command = function(){
		if(GeneralParameters$data.type == 'cdtstation'){
			dat.opfiles <- getOpenFiles(tt, all.opfiles)
			if(!is.null(dat.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'ascii'
				AllOpenFilesData[[nopf+1]] <<- dat.opfiles

				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
				tclvalue(input.Prec) <- AllOpenFilesData[[nopf+1]][[1]]
				tkconfigure(cb.en.etp, values = unlist(listOpenFiles), textvariable = input.Etp)
				tkconfigure(cb.en.prec, values = unlist(listOpenFiles), textvariable = input.Prec)
			}else return(NULL)
		}else if(GeneralParameters$data.type == 'cdtdataset'){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			tclvalue(input.Prec) <- if(path.rds%in%c("", "NA")) "" else path.rds
		}
	})

	##############
	txt.etp <- tklabel(frameInData, text = tclvalue(txt.INEtp.var), textvariable = txt.INEtp.var, anchor = 'w', justify = 'left')

	if(GeneralParameters$data.type == 'cdtstation'){
		cb.en.etp <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Etp, width = largeur3)
	}else{
		cb.en.etp <- tkentry(frameInData, textvariable = input.Etp, width = largeur2)
	}
	bt.etp <- tkbutton(frameInData, text = "...")

	############

	tkconfigure(bt.etp, command = function(){
		if(GeneralParameters$data.type == 'cdtstation'){
			dat.opfiles <- getOpenFiles(tt, all.opfiles)
			if(!is.null(dat.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'ascii'
				AllOpenFilesData[[nopf+1]] <<- dat.opfiles

				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
				tclvalue(input.Etp) <- AllOpenFilesData[[nopf+1]][[1]]
				tkconfigure(cb.en.etp, values = unlist(listOpenFiles), textvariable = input.Etp)
				tkconfigure(cb.en.prec, values = unlist(listOpenFiles), textvariable = input.Prec)
			}else return(NULL)
		}else{
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			tclvalue(input.Etp) <- if(path.rds%in%c("", "NA")) "" else path.rds
		}
	})

	############ 

	tkgrid(txt.prec, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.en.prec, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.prec, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	tkgrid(txt.etp, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.en.etp, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.etp, row = 3, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	#############
	if(GeneralParameters$data.type == 'cdtstation'){
		infobulle(cb.en.etp, 'Select the file containing the daily pontetial evapotranspiration')
		status.bar.display(cb.en.etp, TextOutputVar, 'Select the file containing the daily pontetial evapotranspiration')
		infobulle(cb.en.prec, 'Select the file containing the daily precipitation')
		status.bar.display(cb.en.prec, TextOutputVar, 'Select the file containing the daily precipitation')

		infobulle(bt.etp, 'Browse file if not listed')
		status.bar.display(bt.etp, TextOutputVar, 'Browse file if not listed')
		infobulle(bt.prec, 'Browse file if not listed')
		status.bar.display(bt.prec, TextOutputVar, 'Browse file if not listed')
	}else{
		infobulle(cb.en.etp, 'Enter the full path to the file <daily pontetial evapotranspiration dataset name>.rds')
		status.bar.display(cb.en.etp, TextOutputVar, 'Enter the full path to the file <daily pontetial evapotranspiration dataset name>.rds')
		infobulle(cb.en.prec, 'Enter the full path to the file <daily precipitation dataset name>.rds')
		status.bar.display(cb.en.prec, TextOutputVar, 'Enter the full path to the file <daily precipitation dataset name>.rds')

		infobulle(bt.etp, 'or browse here')
		status.bar.display(bt.etp, TextOutputVar, 'or browse here')
		infobulle(bt.prec, 'or browse here')
		status.bar.display(bt.prec, TextOutputVar, 'or browse here')
	}

	############################################

	frSave <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.save <- tclVar(GeneralParameters$output)

	if(GeneralParameters$data.type == 'cdtstation'){
		txtSaveDir <- 'File to save the output'
		isFile <- TRUE
	}else{
		txtSaveDir <- 'Directory to save the output'
		isFile <- FALSE
	}
	fileORdir <- tclVar(txtSaveDir)

	txt.file.save <- tklabel(frSave, text = tclvalue(fileORdir), textvariable = fileORdir, anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frSave, textvariable = file.save, width = largeur2)
	bt.file.save <- tkbutton(frSave, text = "...")

	#########
	tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save, isFile = isFile))

	#########
	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	#########
	if(GeneralParameters$data.type == 'cdtstation'){
		txtSaveHelp <- 'Enter the full path of the file to save the result'
	}else{
		txtSaveHelp <- 'Directory to save the result'
	}

	infobulle(en.file.save, txtSaveHelp)
	status.bar.display(en.file.save, TextOutputVar, txtSaveHelp)
	infobulle(bt.file.save, 'or browse here')
	status.bar.display(bt.file.save, TextOutputVar, 'or browse here')

	############################################

	frWBalance <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	imon <- as.numeric(str_trim(GeneralParameters$hdate$start.month))
	start.month <- tclVar(MOIS[imon])
	start.day <- tclVar(GeneralParameters$hdate$start.day)
	separate.year <- tclVar(GeneralParameters$hdate$separate.year)
	start.wb <- tclVar(GeneralParameters$wb$wb1)
	capacity.max <- tclVar(GeneralParameters$swhc$cap.max)

	use.multi.wb <- tclVar(GeneralParameters$wb$multi)
	use.multi.swhc <- tclVar(GeneralParameters$swhc$multi)

	stateMWB <- if(GeneralParameters$wb$multi) "normal" else "disabled"
	stateMSWHC <- if(GeneralParameters$swhc$multi) "normal" else "disabled"

	chk.sep.year <- tkcheckbutton(frWBalance, variable = separate.year, text = 'Compute each year separately', anchor = 'w', justify = 'left')

	txt.1stdate0 <- tklabel(frWBalance, text = "Start Water Balance from", anchor = 'e', justify = 'right')
	txt.1stdate1 <- tklabel(frWBalance, text = "Month", anchor = 'e', justify = 'right')
	cb.1stdate1 <- ttkcombobox(frWBalance, values = MOIS, textvariable = start.month, width = 9)
	txt.1stdate2 <- tklabel(frWBalance, text = "Day", anchor = 'e', justify = 'right')
	cb.1stdate2 <- ttkcombobox(frWBalance, values = 1:31, textvariable = start.day, width = 2)

	txt.wb.1stday <- tklabel(frWBalance, text = "First Day Water Balance", anchor = 'w', justify = 'left')
	en.wb.1stday <- tkentry(frWBalance, textvariable = start.wb, width = 4)

	chk.wb.1stday <- tkcheckbutton(frWBalance, variable = use.multi.wb, text = "Multiple WB", anchor = 'w', justify = 'left')
	bt.wb.1stday <- tkbutton(frWBalance, text = "Set", state = stateMWB)

	txt.wb.swhc <- tklabel(frWBalance, text = "Soil Water Holding Capacity", anchor = 'w', justify = 'left')
	en.wb.swhc <- tkentry(frWBalance, textvariable = capacity.max, width = 4)

	chk.wb.swhc <- tkcheckbutton(frWBalance, variable = use.multi.swhc, text = "Multiple SWHC", anchor = 'w', justify = 'left')
	bt.wb.swhc <- tkbutton(frWBalance, text = "Set", state = stateMSWHC)

	###############
	tkconfigure(bt.wb.1stday, command = function(){
		GeneralParameters$wb[["file"]] <<- computeWB_get.WB.SWHC(tt, GeneralParameters$wb[["file"]],
																str_trim(tclvalue(DataType)), "WB")
	})

	tkconfigure(bt.wb.swhc, command = function(){
		GeneralParameters$swhc[["file"]] <<- computeWB_get.WB.SWHC(tt, GeneralParameters$swhc[["file"]],
																	str_trim(tclvalue(DataType)), "SWHC")
	})

	###############

	tkgrid(chk.sep.year, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.1stdate0, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.1stdate1, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.1stdate1, row = 1, column = 6, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.1stdate2, row = 1, column = 7, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.1stdate2, row = 1, column = 8, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.wb.1stday, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.wb.1stday, row = 2, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.wb.1stday, row = 2, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.wb.1stday, row = 2, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.wb.swhc, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.wb.swhc, row = 3, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.wb.swhc, row = 3, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.wb.swhc, row = 3, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###############

	tkbind(chk.wb.1stday, "<Button-1>", function(){
		stateMWB <- if(tclvalue(use.multi.wb) == '0') 'normal' else 'disabled'
		tkconfigure(bt.wb.1stday, state = stateMWB)
	})

	tkbind(chk.wb.swhc, "<Button-1>", function(){
		stateMSWHC <- if(tclvalue(use.multi.swhc) == '0') 'normal' else 'disabled'
		tkconfigure(bt.wb.swhc, state = stateMSWHC)
	})

	############################################
	# tkgrid(frtimestep, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frdatatype, row = 1, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frameInData, row = 2, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 3, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frWBalance, row = 4, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	############################################
	
	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(input.Etp))%in%c("", "NA")){
			tkmessageBox(message = "No input for daily potential evapotranspiration", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(input.Prec))%in%c("", "NA")){
			tkmessageBox(message = "No input for daily precipitation", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save)%in%c("", "NA")){
			tkmessageBox(message = "Choose a directory or enter the file to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$data.type <<- switch(str_trim(tclvalue(DataType)),
												'CDT stations data format' = 'cdtstation',
												'CDT dataset format (gridded)' = 'cdtdataset')

			if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
				GeneralParameters$cdtstation$etp <<- str_trim(tclvalue(input.Etp))
				GeneralParameters$cdtstation$prec <<- str_trim(tclvalue(input.Prec))
			}

			if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)'){
				GeneralParameters$cdtdataset$etp <<- str_trim(tclvalue(input.Etp))
				GeneralParameters$cdtdataset$prec <<- str_trim(tclvalue(input.Prec))
			}

			GeneralParameters$output <<- str_trim(tclvalue(file.save))

			GeneralParameters$hdate$start.month <<- which(MOIS%in%str_trim(tclvalue(start.month)))
			GeneralParameters$hdate$start.day <<- as.numeric(str_trim(tclvalue(start.day)))
			GeneralParameters$hdate$separate.year <<- switch(tclvalue(separate.year), '0' = FALSE, '1' = TRUE)

			GeneralParameters$wb$multi <<- switch(tclvalue(use.multi.wb), '0' = FALSE, '1' = TRUE)
			GeneralParameters$swhc$multi <<- switch(tclvalue(use.multi.swhc), '0' = FALSE, '1' = TRUE)

			if(tclvalue(use.multi.wb) == 0)
				GeneralParameters$wb$wb1 <<- as.numeric(str_trim(tclvalue(start.wb)))
			if(tclvalue(use.multi.swhc) == 0)
				GeneralParameters$swhc$cap.max <<- as.numeric(str_trim(tclvalue(capacity.max)))

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
	tkwm.title(tt, 'Water Balance')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

############################################################

computeWB_get.WB.SWHC <- function(parent.win, Parameters, dataType, donne)
{
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		largeur <- 45
		largeur1 <- 29
	}else{
		largeur <- 34
		largeur1 <- 30
	}

	###################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	###################

	frFF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	input.file <- tclVar(Parameters)

	if(dataType == 'CDT stations data format'){
		if(donne == "WB"){
			LABL <- "CDT stations file of the Water Balance on day 1"
			TXTA <- "CDT stations data format file  containing the water balance on the first day of calculation, the data must have the same stations as the Precipitation and PET data"
		}else{
			LABL <- "CDT stations file containing the SWHC data"
			TXTA <- "CDT station data format file  containing the soil water holding capacity data, the data must have the same stations as the Precipitation and PET data"
		}
	}else{
		if(donne == "WB"){
			LABL <- "NetCDF file of the Water Balance on day 1"
			TXTA <- "A NetCDF file containing the water balance on the first day of calculation, the grid must have the same resolution and extent as the Precipitation and PET"
		}else{
			LABL <- "NetCDF file containing the SWHC data"
			TXTA <- "A NetCDF file containing the soil water holding capacity data, the grid must have the same resolution and extent as the Precipitation and PET"
		}
	}

	txt.WB <- tklabel(frFF, text = LABL, anchor = 'w', justify = 'left')
	cb.WB <- ttkcombobox(frFF, values = unlist(listOpenFiles), textvariable = input.file, width = largeur)
	bt.WB <- tkbutton(frFF, text = "...")
	txta.WB <- tktext(frFF, bg = "white", font = "courier", cursor = "", wrap = "word", height = 7, width = largeur1)

	tkconfigure(bt.WB, command = function(){
		if(dataType == 'CDT stations data format'){
			dat.opfiles <- getOpenFiles(tt, all.opfiles)
			if(!is.null(dat.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'ascii'
				AllOpenFilesData[[nopf+1]] <<- dat.opfiles

				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
				tclvalue(input.file) <- AllOpenFilesData[[nopf+1]][[1]]
				tkconfigure(cb.WB, values = unlist(listOpenFiles), textvariable = input.file)
			}else return(NULL)
		}else{
			nc.opfiles <- getOpenNetcdf(tt, all.opfiles, initialdir = getwd())
			if(!is.null(nc.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'netcdf'
				AllOpenFilesData[[nopf+1]] <<- nc.opfiles

				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
				tclvalue(input.file) <- AllOpenFilesData[[nopf+1]][[1]]
				tkconfigure(cb.WB, values = unlist(listOpenFiles), textvariable = input.file)
			}else return(NULL)
		}
	})

	tkgrid(txt.WB, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.WB, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.WB, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txta.WB, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkinsert(txta.WB, "1.0", TXTA)
	tkconfigure(txta.WB, state = "disabled")

	###################

	tkgrid(frFF, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)

	################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(input.file))%in%c("", "NA")){
			tkmessageBox(message = "No input data found", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			Parameters <<- str_trim(tclvalue(input.file))

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

	################################
	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################
	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5 - tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5 - tt.h*0.5)
	tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt)
	if(donne == "WB"){
		titre <- 'Water Balance - Initialization'
	}else{
		titre <- 'Soil Water Holding Capacity'
	}
	tkwm.title(tt, titre)
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(Parameters)
}

############################################################





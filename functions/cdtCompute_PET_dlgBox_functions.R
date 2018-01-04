computePET_getParams <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur0 <- 12
		largeur1 <- 14
		largeur2 <- 45
		largeur3 <- 43
	}else{
		largeur0 <- 10
		largeur1 <- 16
		largeur2 <- 40
		largeur3 <- 39
	}

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################

	frtimestep <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	timeSteps <- tclVar()
	cb.periodVAL <- c('Daily data', 'Pentad data', 'Dekadal data', 'Monthly data')
	tclvalue(timeSteps) <- switch(GeneralParameters$Tstep,
									'daily' = cb.periodVAL[1],
									'pentad' = cb.periodVAL[2],
									'dekadal' = cb.periodVAL[3],
									'monthly' = cb.periodVAL[4])
	ref.evap <- tclVar()
	cb.evapVAL <- c('Hargreaves', 'Modified-Hargreaves')
	tclvalue(ref.evap) <- switch(GeneralParameters$method,
								'HAR' = cb.evapVAL[1],
								'MHAR' = cb.evapVAL[2])

	txt.period <- tklabel(frtimestep, text = 'Time step', anchor = 'e', justify = 'right')
	cb.period <- ttkcombobox(frtimestep, values = cb.periodVAL, textvariable = timeSteps, width = largeur0)
	txt.evap <- tklabel(frtimestep, text = 'Method', anchor = 'e', justify = 'right')
	cb.evap <- ttkcombobox(frtimestep, values = cb.evapVAL, textvariable = ref.evap, width = largeur1)

	tkgrid(txt.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(cb.period, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(txt.evap, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(cb.evap, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)

	infobulle(cb.period, 'Select the time step of the input data')
	status.bar.display(cb.period, TextOutputVar, 'Select the time step of the input data')
	infobulle(cb.evap, 'Select the method to be used to calculate the evapotranspiration')
	status.bar.display(cb.evap, TextOutputVar, 'Select the method to be used to calculate the evapotranspiration')

	############

	tkbind(cb.evap, "<<ComboboxSelected>>", function(){
		statePrecip <- if(str_trim(tclvalue(ref.evap)) == 'Hargreaves') "disabled" else "normal"
		tkconfigure(cb.en.prec, state = statePrecip)
		tkconfigure(bt.prec, state = statePrecip)
		stateSetNC1 <- if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data' &
						str_trim(tclvalue(ref.evap)) == 'Modified-Hargreaves') "normal" else "disabled"
		tkconfigure(set.prec, state = stateSetNC1)
	})

	############################################

	frdatatype <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	DataType <- tclVar()
	CbdatatypeVAL <- c('CDT stations data format', 'CDT dataset format (gridded)', 'NetCDF gridded data')
	tclvalue(DataType) <- switch(GeneralParameters$data.type,
								'cdtstation' = CbdatatypeVAL[1],
								'cdtdataset' = CbdatatypeVAL[2],
								'cdtnetcdf' = CbdatatypeVAL[3])

	txt.datatyp <- tklabel(frdatatype, text = 'Format of input data', anchor = 'w', justify = 'left')
	cb.datatyp <- ttkcombobox(frdatatype, values = CbdatatypeVAL, textvariable = DataType, width = largeur3)

	tkgrid(txt.datatyp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.datatyp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.datatyp, 'Select the format of the input data')
	status.bar.display(cb.datatyp, TextOutputVar, 'Select the format of the input data')

	###############

	tkbind(cb.datatyp, "<<ComboboxSelected>>", function(){
		tkdestroy(cb.en.tmin)
		tclvalue(input.Tmin) <- ''

		tkdestroy(cb.en.tmax)
		tclvalue(input.Tmax) <- ''

		tkdestroy(cb.en.prec)
		tclvalue(input.Prec) <- ''

		###
		stateSetNC <- if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data') "normal" else "disabled"
		tkconfigure(set.tmin, state = stateSetNC)
		tkconfigure(set.tmax, state = stateSetNC)
		stateSetNC1 <- if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data' &
						str_trim(tclvalue(ref.evap)) == 'Modified-Hargreaves') "normal" else "disabled"
		tkconfigure(set.prec, state = stateSetNC1)

		statePrecip <- if(str_trim(tclvalue(ref.evap)) == 'Modified-Hargreaves') "normal" else "disabled"

		###
		if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
			tclvalue(txt.INTmin.var) <- 'File containing stations Tmin data'
			tclvalue(txt.INTmax.var) <- 'File containing stations Tmax data'
			tclvalue(txt.INPrec.var) <- 'File containing stations Precip data'

			cb.en.tmin <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Tmin, width = largeur3)
			cb.en.tmax <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Tmax, width = largeur3)
			cb.en.prec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur3, state = statePrecip)

			######
			tkconfigure(bt.tmin, command = function(){
				dat.opfiles <- getOpenFiles(tt, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(input.Tmin) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.en.tmin, values = unlist(listOpenFiles), textvariable = input.Tmin)
					tkconfigure(cb.en.tmax, values = unlist(listOpenFiles), textvariable = input.Tmax)
					tkconfigure(cb.en.prec, values = unlist(listOpenFiles), textvariable = input.Prec)
				}else return(NULL)
			})

			tkconfigure(bt.tmax, command = function(){
				dat.opfiles <- getOpenFiles(tt, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(input.Tmax) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.en.tmin, values = unlist(listOpenFiles), textvariable = input.Tmin)
					tkconfigure(cb.en.tmax, values = unlist(listOpenFiles), textvariable = input.Tmax)
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
					tkconfigure(cb.en.tmin, values = unlist(listOpenFiles), textvariable = input.Tmin)
					tkconfigure(cb.en.tmax, values = unlist(listOpenFiles), textvariable = input.Tmax)
					tkconfigure(cb.en.prec, values = unlist(listOpenFiles), textvariable = input.Prec)
				}else return(NULL)
			})

			######
			infobulle(cb.en.tmin, 'Select the file containing the minimum temperature')
			status.bar.display(cb.en.tmin, TextOutputVar, 'Select the file containing the minimum temperature')
			infobulle(cb.en.tmax, 'Select the file containing the maximum temperature')
			status.bar.display(cb.en.tmax, TextOutputVar, 'Select the file containing the maximum temperature')
			infobulle(cb.en.prec, 'Select the file containing the precipitation')
			status.bar.display(cb.en.prec, TextOutputVar, 'Select the file containing the precipitation')

			infobulle(bt.tmin, 'Browse file if not listed')
			status.bar.display(bt.tmin, TextOutputVar, 'Browse file if not listed')
			infobulle(bt.tmax, 'Browse file if not listed')
			status.bar.display(bt.tmax, TextOutputVar, 'Browse file if not listed')
			infobulle(bt.prec, 'Browse file if not listed')
			status.bar.display(bt.prec, TextOutputVar, 'Browse file if not listed')
		}

		###
		if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)'){
			tclvalue(txt.INTmin.var) <- 'Index file (*.rds) for Tmin data'
			tclvalue(txt.INTmax.var) <- 'Index file (*.rds) for Tmax data'
			tclvalue(txt.INPrec.var) <- 'Index file (*.rds) for Precip data'

			cb.en.tmin <- tkentry(frameInData, textvariable = input.Tmin, width = largeur2)
			cb.en.tmax <- tkentry(frameInData, textvariable = input.Tmax, width = largeur2)
			cb.en.prec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2, state = statePrecip)

			######
			tkconfigure(bt.tmin, command = function(){
				filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
				tclvalue(input.Tmin) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
			})

			tkconfigure(bt.tmax, command = function(){
				filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
				tclvalue(input.Tmax) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
			})

			tkconfigure(bt.prec, command = function(){
				filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
				tclvalue(input.Prec) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
			})

			######
			infobulle(cb.en.tmin, 'Enter the full path to the file <minimum temperature dataset name>.rds')
			status.bar.display(cb.en.tmin, TextOutputVar, 'Enter the full path to the file <minimum temperature dataset name>.rds')
			infobulle(cb.en.tmax, 'Enter the full path to the file <maximum temperature dataset name>.rds')
			status.bar.display(cb.en.tmax, TextOutputVar, 'Enter the full path to the file <maximum temperature dataset name>.rds')
			infobulle(cb.en.prec, 'Enter the full path to the file <precipitation dataset name>.rds')
			status.bar.display(cb.en.prec, TextOutputVar, 'Enter the full path to the file <precipitation dataset name>.rds')

			infobulle(bt.tmin, 'or browse here')
			status.bar.display(bt.tmin, TextOutputVar, 'or browse here')
			infobulle(bt.tmax, 'or browse here')
			status.bar.display(bt.tmax, TextOutputVar, 'or browse here')
			infobulle(bt.prec, 'or browse here')
			status.bar.display(bt.prec, TextOutputVar, 'or browse here')
		}

		###
		if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data'){
			tclvalue(txt.INTmin.var) <- 'Directory of the Tmin NetCDF files'
			tclvalue(txt.INTmax.var) <- 'Directory of the Tmax NetCDF files'
			tclvalue(txt.INPrec.var) <- 'Directory of the Precip NetCDF files'

			cb.en.tmin <- tkentry(frameInData, textvariable = input.Tmin, width = largeur2)
			cb.en.tmax <- tkentry(frameInData, textvariable = input.Tmax, width = largeur2)
			cb.en.prec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2, state = statePrecip)

			######
			tkconfigure(set.tmin, command = function(){
				GeneralParameters$cdtnetcdf[["tmin"]] <<- getInfoNetcdfData(tt, GeneralParameters$cdtnetcdf[["tmin"]],
															str_trim(tclvalue(input.Tmin)), tclvalue(timeSteps))
				settingTmin <<- 1
			})

			tkconfigure(set.tmax, command = function(){
				GeneralParameters$cdtnetcdf[["tmax"]] <<- getInfoNetcdfData(tt, GeneralParameters$cdtnetcdf[["tmax"]],
															str_trim(tclvalue(input.Tmax)), tclvalue(timeSteps))
				settingTmax <<- 1
			})

			tkconfigure(set.prec, command = function(){
				GeneralParameters$cdtnetcdf[["prec"]] <<- getInfoNetcdfData(tt, GeneralParameters$cdtnetcdf[["prec"]],
															str_trim(tclvalue(input.Prec)), tclvalue(timeSteps))
				settingPrec <<- 1
			})

			tkconfigure(bt.tmin, command = function(){
				dirnc <- tk_choose.dir(getwd(), "")
				tclvalue(input.Tmin) <- if(dirnc%in%c("", "NA") | is.na(dirnc)) "" else dirnc
			})

			tkconfigure(bt.tmax, command = function(){
				dirnc <- tk_choose.dir(getwd(), "")
				tclvalue(input.Tmax) <- if(dirnc%in%c("", "NA") | is.na(dirnc)) "" else dirnc
			})

			tkconfigure(bt.prec, command = function(){
				dirnc <- tk_choose.dir(getwd(), "")
				tclvalue(input.Prec) <- if(dirnc%in%c("", "NA") | is.na(dirnc)) "" else dirnc
			})

			######
			infobulle(cb.en.tmin, 'Enter the full path to directory containing the minimum temperature files')
			status.bar.display(cb.en.tmin, TextOutputVar, 'Enter the full path to directory containing the minimum temperature files')
			infobulle(cb.en.tmax, 'Enter the full path to directory containing the maximum temperature files')
			status.bar.display(cb.en.tmax, TextOutputVar, 'Enter the full path to directory containing the maximum temperature files')
			infobulle(cb.en.prec, 'Enter the full path to directory containing the precipitation files')
			status.bar.display(cb.en.prec, TextOutputVar, 'Enter the full path to directory containing the precipitation files')

			infobulle(bt.tmin, 'or browse here')
			status.bar.display(bt.tmin, TextOutputVar, 'or browse here')
			infobulle(bt.tmax, 'or browse here')
			status.bar.display(bt.tmax, TextOutputVar, 'or browse here')
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
		tkbind(cb.evap, "<<ComboboxSelected>>", function(){
			statePrecip <- if(str_trim(tclvalue(ref.evap)) == 'Hargreaves') "disabled" else "normal"
			tkconfigure(cb.en.prec, state = statePrecip)
			tkconfigure(bt.prec, state = statePrecip)
			stateSetNC1 <- if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data' &
							str_trim(tclvalue(ref.evap)) == 'Modified-Hargreaves') "normal" else "disabled"
			tkconfigure(set.prec, state = stateSetNC1)
		})

		#######
		tkgrid(cb.en.tmin, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(cb.en.tmax, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(cb.en.prec, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkfocus(tt)
	})

	############################################

	frameInData <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	if(GeneralParameters$data.type == 'cdtstation'){
		input.Tmin <- tclVar(GeneralParameters$cdtstation$tmin)
		input.Tmax <- tclVar(GeneralParameters$cdtstation$tmax)
		input.Prec <- tclVar(GeneralParameters$cdtstation$prec)
		txt.INTmin <- 'File containing stations Tmin data'
		txt.INTmax <- 'File containing stations Tmax data'
		txt.INPrec <- 'File containing stations Precip data'
		stateSetNC <- "disabled"
		stateSetNC1 <- "disabled"
	}else if(GeneralParameters$data.type == 'cdtdataset'){
		input.Tmin <- tclVar(GeneralParameters$cdtdataset$tmin)
		input.Tmax <- tclVar(GeneralParameters$cdtdataset$tmax)
		input.Prec <- tclVar(GeneralParameters$cdtdataset$prec)
		txt.INTmin <- 'Index file (*.rds) for Tmin data'
		txt.INTmax <- 'Index file (*.rds) for Tmax data'
		txt.INPrec <- 'Index file (*.rds) for Precip data'
		stateSetNC <- "disabled"
		stateSetNC1 <- "disabled"
	}else{
		input.Tmin <- tclVar(GeneralParameters$cdtnetcdf$tmin$dir)
		input.Tmax <- tclVar(GeneralParameters$cdtnetcdf$tmax$dir)
		input.Prec <- tclVar(GeneralParameters$cdtnetcdf$prec$dir)
		txt.INTmin <- 'Directory of the Tmin NetCDF files'
		txt.INTmax <- 'Directory of the Tmax NetCDF files'
		txt.INPrec <- 'Directory of the Precip NetCDF files'
		stateSetNC <- "normal"
		stateSetNC1 <- if(GeneralParameters$method == "MHAR") "normal" else "disabled"
	}
	txt.INTmin.var <- tclVar(txt.INTmin)
	txt.INTmax.var <- tclVar(txt.INTmax)
	txt.INPrec.var <- tclVar(txt.INPrec)

	statePrecip <- if(GeneralParameters$method == "MHAR") "normal" else "disabled"

	##############
	txt.tmin <- tklabel(frameInData, text = tclvalue(txt.INTmin.var), textvariable = txt.INTmin.var, anchor = 'w', justify = 'left')
	set.tmin <- tkbutton(frameInData, text = "Settings", state = stateSetNC)

	if(GeneralParameters$data.type == 'cdtstation'){
		cb.en.tmin <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Tmin, width = largeur3)
	}else{
		cb.en.tmin <- tkentry(frameInData, textvariable = input.Tmin, width = largeur2)
	}
	bt.tmin <- tkbutton(frameInData, text = "...")

	############
	settingTmin <- GeneralParameters$settingTmin
	tkconfigure(set.tmin, command = function(){
		GeneralParameters$cdtnetcdf[["tmin"]] <<- getInfoNetcdfData(tt, GeneralParameters$cdtnetcdf[["tmin"]],
													str_trim(tclvalue(input.Tmin)), tclvalue(timeSteps))
		settingTmin <<- 1
	})

	tkconfigure(bt.tmin, command = function(){
		if(GeneralParameters$data.type == 'cdtstation'){
			dat.opfiles <- getOpenFiles(tt, all.opfiles)
			if(!is.null(dat.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'ascii'
				AllOpenFilesData[[nopf+1]] <<- dat.opfiles

				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
				tclvalue(input.Tmin) <- AllOpenFilesData[[nopf+1]][[1]]
				tkconfigure(cb.en.tmin, values = unlist(listOpenFiles), textvariable = input.Tmin)
				tkconfigure(cb.en.tmax, values = unlist(listOpenFiles), textvariable = input.Tmax)
				tkconfigure(cb.en.prec, values = unlist(listOpenFiles), textvariable = input.Prec)
			}else return(NULL)
		}else if(GeneralParameters$data.type == 'cdtdataset'){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			tclvalue(input.Tmin) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
		}else{
			dirnc <- tk_choose.dir(getwd(), "")
			tclvalue(input.Tmin) <- if(dirnc%in%c("", "NA") | is.na(dirnc)) "" else dirnc
		}
	})

	##############
	txt.tmax <- tklabel(frameInData, text = tclvalue(txt.INTmax.var), textvariable = txt.INTmax.var, anchor = 'w', justify = 'left')
	set.tmax <- tkbutton(frameInData, text = "Settings", state = stateSetNC)

	if(GeneralParameters$data.type == 'cdtstation'){
		cb.en.tmax <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Tmax, width = largeur3)
	}else{
		cb.en.tmax <- tkentry(frameInData, textvariable = input.Tmax, width = largeur2)
	}
	bt.tmax <- tkbutton(frameInData, text = "...")

	############
	settingTmax <- GeneralParameters$settingTmax
	tkconfigure(set.tmax, command = function(){
		GeneralParameters$cdtnetcdf[["tmax"]] <<- getInfoNetcdfData(tt, GeneralParameters$cdtnetcdf[["tmax"]],
													str_trim(tclvalue(input.Tmax)), tclvalue(timeSteps))
		settingTmax <<- 1
	})

	tkconfigure(bt.tmax, command = function(){
		if(GeneralParameters$data.type == 'cdtstation'){
			dat.opfiles <- getOpenFiles(tt, all.opfiles)
			if(!is.null(dat.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'ascii'
				AllOpenFilesData[[nopf+1]] <<- dat.opfiles

				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
				tclvalue(input.Tmax) <- AllOpenFilesData[[nopf+1]][[1]]
				tkconfigure(cb.en.tmin, values = unlist(listOpenFiles), textvariable = input.Tmin)
				tkconfigure(cb.en.tmax, values = unlist(listOpenFiles), textvariable = input.Tmax)
				tkconfigure(cb.en.prec, values = unlist(listOpenFiles), textvariable = input.Prec)
			}else return(NULL)
		}else if(GeneralParameters$data.type == 'cdtdataset'){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			tclvalue(input.Tmax) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
		}else{
			dirnc <- tk_choose.dir(getwd(), "")
			tclvalue(input.Tmax) <- if(dirnc%in%c("", "NA") | is.na(dirnc)) "" else dirnc
		}
	})

	##############
	txt.prec <- tklabel(frameInData, text = tclvalue(txt.INPrec.var), textvariable = txt.INPrec.var, anchor = 'w', justify = 'left')
	set.prec <- tkbutton(frameInData, text = "Settings", state = stateSetNC1)

	if(GeneralParameters$data.type == 'cdtstation'){
		cb.en.prec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur3, state = statePrecip)
	}else{
		cb.en.prec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2, state = statePrecip)
	}
	bt.prec <- tkbutton(frameInData, text = "...", state = statePrecip)

	############
	settingPrec <- GeneralParameters$settingPrec
	tkconfigure(set.prec, command = function(){
		GeneralParameters$cdtnetcdf[["prec"]] <<- getInfoNetcdfData(tt, GeneralParameters$cdtnetcdf[["prec"]],
													str_trim(tclvalue(input.Prec)), tclvalue(timeSteps))
		settingPrec <<- 1
	})

	tkconfigure(bt.prec, command = function(){
		if(GeneralParameters$data.type == 'cdtstation'){
			dat.opfiles <- getOpenFiles(tt, all.opfiles)
			if(!is.null(dat.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'ascii'
				AllOpenFilesData[[nopf+1]] <<- dat.opfiles

				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
				tclvalue(input.Prec) <- AllOpenFilesData[[nopf+1]][[1]]
				tkconfigure(cb.en.tmin, values = unlist(listOpenFiles), textvariable = input.Tmin)
				tkconfigure(cb.en.tmax, values = unlist(listOpenFiles), textvariable = input.Tmax)
				tkconfigure(cb.en.prec, values = unlist(listOpenFiles), textvariable = input.Prec)
			}else return(NULL)
		}else if(GeneralParameters$data.type == 'cdtdataset'){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			tclvalue(input.Prec) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
		}else{
			dirnc <- tk_choose.dir(getwd(), "")
			tclvalue(input.Prec) <- if(dirnc%in%c("", "NA") | is.na(dirnc)) "" else dirnc
		}
	})

	############ 
	tkgrid(txt.tmin, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.tmin, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.en.tmin, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.tmin, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	tkgrid(txt.tmax, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.tmax, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.en.tmax, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.tmax, row = 3, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	tkgrid(txt.prec, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.prec, row = 4, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.en.prec, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.prec, row = 5, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	#############
	if(GeneralParameters$data.type == 'cdtstation'){
		infobulle(cb.en.tmin, 'Select the file containing the minimum temperature')
		status.bar.display(cb.en.tmin, TextOutputVar, 'Select the file containing the minimum temperature')
		infobulle(cb.en.tmax, 'Select the file containing the maximum temperature')
		status.bar.display(cb.en.tmax, TextOutputVar, 'Select the file containing the maximum temperature')
		infobulle(cb.en.prec, 'Select the file containing the precipitation')
		status.bar.display(cb.en.prec, TextOutputVar, 'Select the file containing the precipitation')

		infobulle(bt.tmin, 'Browse file if not listed')
		status.bar.display(bt.tmin, TextOutputVar, 'Browse file if not listed')
		infobulle(bt.tmax, 'Browse file if not listed')
		status.bar.display(bt.tmax, TextOutputVar, 'Browse file if not listed')
		infobulle(bt.prec, 'Browse file if not listed')
		status.bar.display(bt.prec, TextOutputVar, 'Browse file if not listed')
	}else if(GeneralParameters$data.type == 'cdtdataset'){
		infobulle(cb.en.tmin, 'Enter the full path to the file <minimum temperature dataset name>.rds')
		status.bar.display(cb.en.tmin, TextOutputVar, 'Enter the full path to the file <minimum temperature dataset name>.rds')
		infobulle(cb.en.tmax, 'Enter the full path to the file <maximum temperature dataset name>.rds')
		status.bar.display(cb.en.tmax, TextOutputVar, 'Enter the full path to the file <maximum temperature dataset name>.rds')
		infobulle(cb.en.prec, 'Enter the full path to the file <precipitation dataset name>.rds')
		status.bar.display(cb.en.prec, TextOutputVar, 'Enter the full path to the file <precipitation dataset name>.rds')

		infobulle(bt.tmin, 'or browse here')
		status.bar.display(bt.tmin, TextOutputVar, 'or browse here')
		infobulle(bt.tmax, 'or browse here')
		status.bar.display(bt.tmax, TextOutputVar, 'or browse here')
		infobulle(bt.prec, 'or browse here')
		status.bar.display(bt.prec, TextOutputVar, 'or browse here')
	}else{
		infobulle(cb.en.tmin, 'Enter the full path to directory containing the minimum temperature files')
		status.bar.display(cb.en.tmin, TextOutputVar, 'Enter the full path to directory containing the minimum temperature files')
		infobulle(cb.en.tmax, 'Enter the full path to directory containing the maximum temperature files')
		status.bar.display(cb.en.tmax, TextOutputVar, 'Enter the full path to directory containing the maximum temperature files')
		infobulle(cb.en.prec, 'Enter the full path to directory containing the precipitation files')
		status.bar.display(cb.en.prec, TextOutputVar, 'Enter the full path to directory containing the precipitation files')

		infobulle(bt.tmin, 'or browse here')
		status.bar.display(bt.tmin, TextOutputVar, 'or browse here')
		infobulle(bt.tmax, 'or browse here')
		status.bar.display(bt.tmax, TextOutputVar, 'or browse here')
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
	tkgrid(frtimestep, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frdatatype, row = 1, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frameInData, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	############################################
	
	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(input.Tmin))%in%c("", "NA")){
			tkmessageBox(message = "No input for minimum temperature", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(input.Tmax))%in%c("", "NA")){
			tkmessageBox(message = "No input for maximum temperature", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(input.Prec))%in%c("", "NA") &
				str_trim(tclvalue(ref.evap)) == 'Modified-Hargreaves')
		{
			tkmessageBox(message = "No input for precipitation", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save)%in%c("", "NA")){
			tkmessageBox(message = "Choose a directory or enter the file to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data' &
				is.null(settingTmin))
		{
				tkmessageBox(message = "You have to set the Tmin files parameters", icon = "warning", type = "ok")
				tkwait.window(tt)
		}else if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data' &
				is.null(settingTmax))
		{
				tkmessageBox(message = "You have to set the Tmax files parameters", icon = "warning", type = "ok")
				tkwait.window(tt)
		}else if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data' &
				str_trim(tclvalue(ref.evap)) == 'Modified-Hargreaves' &
				is.null(settingPrec))
		{
				tkmessageBox(message = "You have to set the Precip files parameters", icon = "warning", type = "ok")
				tkwait.window(tt)
		}else{
			GeneralParameters$Tstep <<- switch(str_trim(tclvalue(timeSteps)), 
												'Daily data' = 'daily',
												'Pentad data' = 'pentad',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')
			GeneralParameters$method <<- switch(str_trim(tclvalue(ref.evap)),
												'Hargreaves' = 'HAR',
												'Modified-Hargreaves' = 'MHAR')

			GeneralParameters$data.type <<- switch(str_trim(tclvalue(DataType)),
												'CDT stations data format' = 'cdtstation',
												'CDT dataset format (gridded)' = 'cdtdataset',
												'NetCDF gridded data' = 'cdtnetcdf')

			if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
				GeneralParameters$cdtstation$tmin <<- str_trim(tclvalue(input.Tmin))
				GeneralParameters$cdtstation$tmax <<- str_trim(tclvalue(input.Tmax))
				if(str_trim(tclvalue(ref.evap)) == 'Modified-Hargreaves')
					GeneralParameters$cdtstation$prec <<- str_trim(tclvalue(input.Prec))
			}

			if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)'){
				GeneralParameters$cdtdataset$tmin <<- str_trim(tclvalue(input.Tmin))
				GeneralParameters$cdtdataset$tmax <<- str_trim(tclvalue(input.Tmax))
				if(str_trim(tclvalue(ref.evap)) == 'Modified-Hargreaves')
					GeneralParameters$cdtdataset$prec <<- str_trim(tclvalue(input.Prec))
			}

			if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data'){
				GeneralParameters$cdtnetcdf$tmin$dir <<- str_trim(tclvalue(input.Tmin))
				GeneralParameters$cdtnetcdf$tmax$dir <<- str_trim(tclvalue(input.Tmax))
				if(str_trim(tclvalue(ref.evap)) == 'Modified-Hargreaves')
					GeneralParameters$cdtnetcdf$prec$dir <<- str_trim(tclvalue(input.Prec))
			}

			GeneralParameters$output <<- str_trim(tclvalue(file.save))

			GeneralParameters$settingTmin <<- settingTmin
			GeneralParameters$settingTmax <<- settingTmax
			GeneralParameters$settingPrec <<- settingPrec

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
	tkwm.title(tt, 'Potential Evapotranspiration')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}



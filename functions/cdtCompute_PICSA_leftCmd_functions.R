
PICSACalcPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(46)
		largeur0 <- as.integer(w.scale(29)/sfont0)
		largeur1 <- as.integer(w.scale(22)/sfont0)
		largeur2 <- as.integer(w.scale(27)/sfont0)

		largeur3 <- 26
		largeur4 <- 15
		largeur5 <- 14
		largeur6 <- 22
	}else{
		wscrlwin <- w.scale(30)
		hscrlwin <- h.scale(48)
		largeur0 <- as.integer(w.scale(23.5)/sfont0)
		largeur1 <- as.integer(w.scale(20)/sfont0)
		largeur2 <- as.integer(w.scale(22.5)/sfont0)

		largeur3 <- 21
		largeur4 <- 11
		largeur5 <- 10
		largeur6 <- 14
	}


	# GeneralParameters <- fromJSON(file.path(apps.dir, 'init_params', 'ClimatoAnalysis.json'))
	GeneralParameters <- list(onset = "", cessation = "", output = "",
							seastot = list(useTotal = FALSE, Tstep = "dekadal",
										data.type = "cdtstation",
										cdtstation = list(prec = ""),
										cdtdataset = list(prec = "")),
							dryday = 0.85, min.frac = 0.95, 
							plot2file = FALSE)

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Output")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Maps")
	cmd.tab4 <- bwAddTab(tknote.cmd, text = "Graphs")
	cmd.tab5 <- bwAddTab(tknote.cmd, text = "Boundaries")

	bwRaiseTab(tknote.cmd, cmd.tab1)
	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab3, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab4, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab5, 0, weight = 1)

	#######################################################################################################

	#Tab1
	frTab1 <- tkframe(cmd.tab1)
	tkgrid(frTab1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab1, 0, weight = 1)

	scrw1 <- bwScrolledWindow(frTab1)
	tkgrid(scrw1)
	tkgrid.columnconfigure(scrw1, 0, weight = 1)
	subfr1 <- bwScrollableFrame(scrw1, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr1, 0, weight = 1)

		############################################

		frameInData <- ttklabelframe(subfr1, text = "Onset & Cessation", relief = 'groove')

		input.Onset <- tclVar(GeneralParameters$onset)
		input.Cessation <- tclVar(GeneralParameters$cessation)

		txt.Ons <- tklabel(frameInData, text = 'Path to the onset data <Onset.rds>', anchor = 'w', justify = 'left')
		en.Ons <- tkentry(frameInData, textvariable = input.Onset, width = largeur0)
		bt.Ons <- tkbutton(frameInData, text = "...")

		txt.Ces <- tklabel(frameInData, text = 'Path to the cessation data <Cessation.rds>', anchor = 'w', justify = 'left')
		en.Ces <- tkentry(frameInData, textvariable = input.Cessation, width = largeur0)
		bt.Ces <- tkbutton(frameInData, text = "...")

		tkconfigure(bt.Ons, command = function(){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			tclvalue(input.Onset) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
		})

		tkconfigure(bt.Ces, command = function(){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			tclvalue(input.Cessation) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
		})

		tkgrid(txt.Ons, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.Ons, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.Ons, row = 1, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.Ces, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.Ces, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.Ces, row = 3, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		############
		infobulle(en.Ons, 'Enter the full path to the file <Onset.rds>')
		status.bar.display(en.Ons, TextOutputVar, 'Enter the full path to the file <Onset.rds>')
		infobulle(en.Ces, 'Enter the full path to the file <Cessation.rds>')
		status.bar.display(en.Ces, TextOutputVar, 'Enter the full path to the file <Cessation.rds>')

		infobulle(bt.Ons, 'or browse here')
		status.bar.display(bt.Ons, TextOutputVar, 'or browse here')
		infobulle(bt.Ces, 'or browse here')
		status.bar.display(bt.Ces, TextOutputVar, 'or browse here')

		############################################

		frameSeasTot <- ttklabelframe(subfr1, text = "Seasonal rainfall amounts", relief = 'groove')

		useTotal <- tclVar(GeneralParameters$seastot$useTotal)

		timeSteps <- tclVar()
		cb.periodVAL <- c('Pentad data', 'Dekadal data', 'Monthly data')
		tclvalue(timeSteps) <- switch(GeneralParameters$seastot$Tstep,
									'pentad' = cb.periodVAL[1],
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3])

		DataType <- tclVar()
		CbdatatypeVAL <- c('CDT stations data format', 'CDT dataset format (gridded)')
		tclvalue(DataType) <- switch(GeneralParameters$seastot$data.type,
									'cdtstation' = CbdatatypeVAL[1],
									'cdtdataset' = CbdatatypeVAL[2])

		if(GeneralParameters$seastot$data.type == 'cdtstation'){
			input.Prec <- tclVar(GeneralParameters$seastot$cdtstation$prec)
			txt.INPrec <- 'File containing stations Precip data'
		}else{
			input.Prec <- tclVar(GeneralParameters$seastot$cdtdataset$prec)
			txt.INPrec <- 'Index file (*.rds) of Precip data'
		}
		txt.INPrec.var <- tclVar(txt.INPrec)

		stateSEAS <- if(GeneralParameters$seastot$useTotal) 'normal' else 'disabled'

		chk.seastot <- tkcheckbutton(frameSeasTot, variable = useTotal, text = "Use aggregated data for seasonal total", anchor = 'w', justify = 'left')
		txt.period <- tklabel(frameSeasTot, text = 'Time step', anchor = 'e', justify = 'right')
		cb.period <- ttkcombobox(frameSeasTot, values = cb.periodVAL, textvariable = timeSteps, width = largeur1, state = stateSEAS)
		txt.datatype <- tklabel(frameSeasTot, text = 'Format', anchor = 'e', justify = 'right')
		cb.datatype <- ttkcombobox(frameSeasTot, values = CbdatatypeVAL, textvariable = DataType, width = largeur1, state = stateSEAS)

		txt.INPrec <- tklabel(frameSeasTot, text = tclvalue(txt.INPrec.var), textvariable = txt.INPrec.var, anchor = 'w', justify = 'left')
		if(GeneralParameters$seastot$data.type == 'cdtstation'){
			cb.en.INPrec <- ttkcombobox(frameSeasTot, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur2, state = stateSEAS)
		}else{
			cb.en.INPrec <- tkentry(frameSeasTot, textvariable = input.Prec, width = largeur0, state = stateSEAS)
		}
		bt.INPrec <- tkbutton(frameSeasTot, text = "...", state = stateSEAS)

		############

		tkconfigure(bt.INPrec, command = function(){
			if(GeneralParameters$seastot$data.type == 'cdtstation'){
				dat.opfiles <- getOpenFiles(main.win, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(input.Prec) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.en.INPrec, values = unlist(listOpenFiles), textvariable = input.Prec)
					tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = file.plotShp)
				}else return(NULL)
			}else{
				filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
				tclvalue(input.Prec) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
			}
		})

		############

		tkgrid(chk.seastot, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(txt.period, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.period, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(txt.datatype, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.datatype, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(txt.INPrec, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.en.INPrec, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.INPrec, row = 4, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		############
		infobulle(cb.period, 'Select the time step of the input data')
		status.bar.display(cb.period, TextOutputVar, 'Select the time step of the input data')
		infobulle(cb.datatype, 'Select the format of the input data')
		status.bar.display(cb.datatype, TextOutputVar, 'Select the format of the input data')

		if(GeneralParameters$seastot$data.type == 'cdtstation'){
			infobulle(cb.en.INPrec, 'Select the file containing the precipitation data')
			status.bar.display(cb.en.INPrec, TextOutputVar, 'Select the file containing the precipitation data')

			infobulle(bt.INPrec, 'Browse file if not listed')
			status.bar.display(bt.INPrec, TextOutputVar, 'Browse file if not listed')
		}else{
			infobulle(cb.en.INPrec, 'Enter the full path to the file <precipitation dataset name>.rds')
			status.bar.display(cb.en.INPrec, TextOutputVar, 'Enter the full path to the file <precipitation dataset name>.rds')

			infobulle(bt.INPrec, 'or browse here')
			status.bar.display(bt.INPrec, TextOutputVar, 'or browse here')
		}

		############

		tkbind(chk.seastot, "<Button-1>", function(){
			stateSEAS <- if(tclvalue(useTotal) == '1') 'disabled' else 'normal'
			tkconfigure(cb.period, state = stateSEAS)
			tkconfigure(cb.datatype, state = stateSEAS)
			tkconfigure(cb.en.INPrec, state = stateSEAS)
			tkconfigure(bt.INPrec, state = stateSEAS)
		})

		############

		tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
			tkdestroy(cb.en.INPrec)
			tclvalue(input.Prec) <- ''

			stateSEAS <- if(tclvalue(useTotal) == '1') 'normal' else 'disabled'

			###
			if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
				tclvalue(txt.INPrec.var) <- 'File containing stations Precip data'

				cb.en.INPrec <- ttkcombobox(frameSeasTot, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur2, state = stateSEAS)

				######
				tkconfigure(bt.INPrec, command = function(){
					dat.opfiles <- getOpenFiles(tt, all.opfiles)
					if(!is.null(dat.opfiles)){
						nopf <- length(AllOpenFilesType)
						AllOpenFilesType[[nopf+1]] <<- 'ascii'
						AllOpenFilesData[[nopf+1]] <<- dat.opfiles

						listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
						tclvalue(input.Prec) <- AllOpenFilesData[[nopf+1]][[1]]
						tkconfigure(cb.en.INPrec, values = unlist(listOpenFiles), textvariable = input.Prec)
						tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = file.plotShp)
					}else return(NULL)
				})

				######
				infobulle(cb.en.INPrec, 'Select the file containing the precipitation data')
				status.bar.display(cb.en.INPrec, TextOutputVar, 'Select the file containing the precipitation data')

				infobulle(bt.INPrec, 'Browse file if not listed')
				status.bar.display(bt.INPrec, TextOutputVar, 'Browse file if not listed')
			}

			###
			if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)'){
				tclvalue(txt.INPrec.var) <- 'Index file (*.rds) of Precip data'

				cb.en.INPrec <- tkentry(frameSeasTot, textvariable = input.Prec, width = largeur0, state = stateSEAS)

				######
				tkconfigure(bt.INPrec, command = function(){
					filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
					path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
					tclvalue(input.Prec) <- if(path.rds%in%c("", "NA")) "" else path.rds
				})

				######
				infobulle(cb.en.INPrec, 'Enter the full path to the file <precipitation dataset name>.rds')
				status.bar.display(cb.en.INPrec, TextOutputVar, 'Enter the full path to the file <precipitation dataset name>.rds')

				infobulle(bt.INPrec, 'or browse here')
				status.bar.display(bt.INPrec, TextOutputVar, 'or browse here')
			}

			#######
			tkgrid(cb.en.INPrec, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)

			#######
			tkbind(chk.seastot, "<Button-1>", function(){
				stateSEAS <- if(tclvalue(useTotal) == '1') 'disabled' else 'normal'
				tkconfigure(cb.period, state = stateSEAS)
				tkconfigure(cb.datatype, state = stateSEAS)
				tkconfigure(cb.en.INPrec, state = stateSEAS)
				tkconfigure(bt.INPrec, state = stateSEAS)
			})
		})

		############################################

		tkgrid(frameInData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameSeasTot, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab2
	frTab2 <- tkframe(cmd.tab2)
	tkgrid(frTab2, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab2, 0, weight = 1)

	scrw2 <- bwScrolledWindow(frTab2)
	tkgrid(scrw2)
	tkgrid.columnconfigure(scrw2, 0, weight = 1)
	subfr2 <- bwScrollableFrame(scrw2, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr2, 0, weight = 1)

		##############################################

		frameDryDay <- ttklabelframe(subfr2, text = "Dry Day definition", relief = 'groove')

		drywet.day <- tclVar(GeneralParameters$dryday)

		txt.DryDay1 <- tklabel(frameDryDay, text = 'Rainfall amount below', anchor = 'w', justify = 'left')
		en.DryDay <- tkentry(frameDryDay, textvariable = drywet.day, width = 5)
		txt.DryDay2 <- tklabel(frameDryDay, text = 'mm/day', anchor = 'w', justify = 'left')

		tkgrid(txt.DryDay1, en.DryDay, txt.DryDay2)

		##############################################

		frameMinFrac <- tkframe(subfr2, relief = 'groove', borderwidth = 2)

		min.frac <- tclVar(GeneralParameters$min.frac)

		txt.MinFrac <- tklabel(frameMinFrac, text = 'Minimum fraction of non-missing values', anchor = 'w', justify = 'left')
		en.MinFrac <- tkentry(frameMinFrac, textvariable = min.frac, width = 5)
		tkgrid(txt.MinFrac, en.MinFrac)

		##############################################

		frameDirSav <- tkframe(subfr2, relief = 'groove', borderwidth = 2)

		dir.save <- tclVar(GeneralParameters$output)

		txt.dir.save <- tklabel(frameDirSav, text = "Directory to save results",  anchor = 'w', justify = 'left')
		en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur0)
		bt.dir.save <- tkbutton(frameDirSav, text = "...")

		######
		tkconfigure(bt.dir.save, command = function() fileORdir2Save(dir.save, isFile = FALSE))

		######
		tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.dir.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		infobulle(en.dir.save, 'Enter the full path to directory to save outputs')
		status.bar.display(en.dir.save, TextOutputVar, 'Enter the full path to directory to save outputs')
		infobulle(bt.dir.save, 'or browse here')
		status.bar.display(bt.dir.save, TextOutputVar, 'or browse here')

		############################################

		framePlot2file <- tkframe(subfr2, relief = 'groove', borderwidth = 2)

		plot2file <- tclVar(GeneralParameters$plot2file)
		chk.plot2file <- tkcheckbutton(framePlot2file, variable = plot2file, text = "Plot PICSA graphs to files", anchor = 'w', justify = 'left')

		tkgrid(chk.plot2file,  sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(chk.plot2file, 'Check this box to plot directly the graphs to files')
		status.bar.display(chk.plot2file, TextOutputVar, 'Check this box to plot directly the graphs to files')

		############################################

		frameCalc <- tkframe(subfr2)

		if(!is.null(EnvPICSAplot$DirExist)){
			stateCaclBut <- if(tclvalue(EnvPICSAplot$DirExist) == "1") "normal" else "disabled"
		}else stateCaclBut <- "normal"

		bt.CalcPICSA <- tkbutton(frameCalc, text = 'Calculate PICSA Data', state = stateCaclBut, bg = 'lightgreen')

		tkconfigure(bt.CalcPICSA, command = function(){
			GeneralParameters$onset <- str_trim(tclvalue(input.Onset))
			GeneralParameters$cessation <- str_trim(tclvalue(input.Cessation))
			GeneralParameters$output <- str_trim(tclvalue(dir.save))

			GeneralParameters$min.frac <- as.numeric(str_trim(tclvalue(min.frac)))
			GeneralParameters$dryday <- as.numeric(str_trim(tclvalue(drywet.day)))

			GeneralParameters$seastot$useTotal <- switch(tclvalue(useTotal), '0' = FALSE, '1' = TRUE)
			if(tclvalue(useTotal) == "1"){
				GeneralParameters$seastot$Tstep <- switch(str_trim(tclvalue(timeSteps)), 
														'Pentad data' = 'pentad',
														'Dekadal data' =  'dekadal',
														'Monthly data' = 'monthly')

				GeneralParameters$seastot$data.type <- switch(str_trim(tclvalue(DataType)),
													'CDT stations data format' = 'cdtstation',
													'CDT dataset format (gridded)' = 'cdtdataset')

				if(str_trim(tclvalue(DataType)) == 'CDT stations data format')
					GeneralParameters$seastot$cdtstation$prec <- str_trim(tclvalue(input.Prec))

				if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)')
					GeneralParameters$seastot$cdtdataset$prec <- str_trim(tclvalue(input.Prec))
			}

			GeneralParameters$plot2file <- switch(tclvalue(plot2file), '0' = FALSE, '1' = TRUE)

			# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

			tkconfigure(main.win, cursor = 'watch')
			InsertMessagesTxt(main.txt.out, "Calculate PICSA data ......")

			ret <- tryCatch(
				compute_PICSA_Procs(GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = tkconfigure(main.win, cursor = '')
			)

			msg0 <- "PICSA data calculation finished successfully"
			msg1 <- "PICSA data calculation failed"

			if(!is.null(ret)){
				if(ret == 0){
					InsertMessagesTxt(main.txt.out, msg0)

					###################

					load.PICSA.Data()

				}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
			}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
		})

		####################

		tkgrid(bt.CalcPICSA, row = 0, column = 0, sticky = 'we', pady = 1)

		############################################

		tkgrid(frameDryDay, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameMinFrac, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(framePlot2file, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameCalc, row = 4, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)


	#######################################################################################################

	#Tab3
	frTab3 <- tkframe(cmd.tab3)
	tkgrid(frTab3, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab3, 0, weight = 1)

	scrw3 <- bwScrolledWindow(frTab3)
	tkgrid(scrw3)
	tkgrid.columnconfigure(scrw3, 0, weight = 1)
	subfr3 <- bwScrollableFrame(scrw3, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr3, 0, weight = 1)

		##############################################

		framePICSADat <- ttklabelframe(subfr3, text = "PICSA data", relief = 'groove')

		EnvPICSAplot$DirExist <- tclVar(0)
		file.PICSAIndex <- tclVar()

		statePICSADat <- if(tclvalue(EnvPICSAplot$DirExist) == "1") "normal" else "disabled"

		chk.PICSAIdx <- tkcheckbutton(framePICSADat, variable = EnvPICSAplot$DirExist, text = "PICSA data are already calculated", anchor = 'w', justify = 'left')
		en.PICSAIdx <- tkentry(framePICSADat, textvariable = file.PICSAIndex, width = largeur0, state = statePICSADat)
		bt.PICSAIdx <- tkbutton(framePICSADat, text = "...", state = statePICSADat)

		tkconfigure(bt.PICSAIdx, command = function(){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.Stat <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			if(path.Stat%in%c("", "NA") | is.na(path.Stat)) return(NULL)
			tclvalue(file.PICSAIndex) <- path.Stat

			if(file.exists(str_trim(tclvalue(file.PICSAIndex)))){
				OutPicsa <- try(readRDS(str_trim(tclvalue(file.PICSAIndex))), silent = TRUE)
				if(inherits(OutPicsa, "try-error")){
					InsertMessagesTxt(main.txt.out, 'Unable to load PICSA data', format = TRUE)
					InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', OutPicsa[1]), format = TRUE)
					return(NULL)
				}

				EnvPICSAplot$output <- OutPicsa
				EnvPICSAplot$PathPicsa <- dirname(str_trim(tclvalue(file.PICSAIndex)))

				###################

				load.PICSA.Data()
			}
		})

		tkgrid(chk.PICSAIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.PICSAIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.PICSAIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		infobulle(chk.PICSAIdx, "Check this box if the PICSA data are already calculated")
		status.bar.display(chk.PICSAIdx, TextOutputVar, "Check this box if the PICSA data are already calculated")
		infobulle(en.PICSAIdx, "Enter the full path to the file <PICSA.rds>")
		status.bar.display(en.PICSAIdx, TextOutputVar, "Enter the full path to the file <PICSA.rds>")
		infobulle(bt.PICSAIdx, 'or browse here')
		status.bar.display(bt.PICSAIdx, TextOutputVar, 'or browse here')

		###############

		tkbind(chk.PICSAIdx, "<Button-1>", function(){
			statePICSADat <- if(tclvalue(EnvPICSAplot$DirExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.PICSAIdx, state = statePICSADat)
			tkconfigure(bt.PICSAIdx, state = statePICSADat)
			stateCaclBut <- if(tclvalue(EnvPICSAplot$DirExist) == '1') 'normal' else 'disabled'
			tkconfigure(bt.CalcPICSA, state = stateCaclBut)
		})

		##############################################

		framePICSATSMap <- ttklabelframe(subfr3, text = "Maps", relief = 'groove')

		EnvPICSAplot$varPICSA <- tclVar("Onset")
		varPICSA.val <- c("Onset", "Cessation", "Season Length", "Seasonal Rainfall Amounts",
						"Dry Spells", "Longest Dry Spell", 
						"Number of rain day", "Maximum daily rain",
						"Total rain when RR>95thPerc", "Nb of day when RR>95thPerc")

		stateDrySpl <- 'disabled'

		cb.TsMap.picsavar <- ttkcombobox(framePICSATSMap, values = varPICSA.val, textvariable = EnvPICSAplot$varPICSA, width = largeur3)
		txt.TsMap.dryspell <- tklabel(framePICSATSMap, text = "DrySpell",  anchor = 'w', justify = 'left')
		EnvPICSAplot$spin.TsMap.dryspell <- ttkspinbox(framePICSATSMap, from = 1, to = 50, increment = 1, justify = 'center', width = 2, state = stateDrySpl)
		tkset(EnvPICSAplot$spin.TsMap.dryspell, 5)

		bt.TsMap.prev <- ttkbutton(framePICSATSMap, text = "<<", width = 4)
		bt.TsMap.next <- ttkbutton(framePICSATSMap, text = ">>", width = 4)
		EnvPICSAplot$spin.TsMap.year <- ttkspinbox(framePICSATSMap, from = 1800, to = 2200, increment = 1, justify = 'center', width = 4)
		tkset(EnvPICSAplot$spin.TsMap.year, 2015)
		bt.TsMap.plot <- ttkbutton(framePICSATSMap, text = "PLOT")
		bt.TsMap.Opt <- ttkbutton(framePICSATSMap, text = "Options")

		#################

		EnvPICSAplot$TSMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
												userCol = list(custom = FALSE, color = NULL),
												userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
												title = list(user = FALSE, title = ''),
												colkeyLab = list(user = FALSE, label = ''),
												scalebar = list(add = FALSE, pos = 'bottomleft'))

		tkconfigure(bt.TsMap.Opt, command = function(){
			if(!is.null(EnvPICSAplot$tsdata)){
				atlevel <- pretty(EnvPICSAplot$tsdata$z, n = 10, min.n = 7)
				if(is.null(EnvPICSAplot$TSMapOp$userLvl$levels)){
					EnvPICSAplot$TSMapOp$userLvl$levels <- atlevel
				}else{
					if(!EnvPICSAplot$TSMapOp$userLvl$custom)
						EnvPICSAplot$TSMapOp$userLvl$levels <- atlevel
				}
			}
			EnvPICSAplot$TSMapOp <- climatoAnalysis.MapOptions(main.win, EnvPICSAplot$TSMapOp)
		})

		EnvPICSAplot$notebookTab.TSMap <- NULL
		tkconfigure(bt.TsMap.plot, command = function(){
			if(!is.null(EnvPICSAplot$tsdata)){
				ret <- read.PicsaTSData()
				if(is.null(ret)) return(NULL)

				imgContainer <- PICSA.Display.TSMaps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvPICSAplot$notebookTab.TSMap, AllOpenTabType, AllOpenTabData)
				EnvPICSAplot$notebookTab.TSMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.TsMap.prev, command = function(){
			if(!is.null(EnvPICSAplot$tsdata)){
				range.TsMap.year <- range(as.numeric(format(EnvPICSAplot$output$start.date, '%Y')))
				iyear <- as.numeric(str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.year))))
				iyear <- iyear-1
				if(iyear < range.TsMap.year[1]) iyear <- range.TsMap.year[2]
				tkset(EnvPICSAplot$spin.TsMap.year, iyear)

				ret <- read.PicsaTSData()
				if(is.null(ret)) return(NULL)

				imgContainer <- PICSA.Display.TSMaps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvPICSAplot$notebookTab.TSMap, AllOpenTabType, AllOpenTabData)
				EnvPICSAplot$notebookTab.TSMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.TsMap.next, command = function(){
			if(!is.null(EnvPICSAplot$tsdata)){
				range.TsMap.year <- range(as.numeric(format(EnvPICSAplot$output$start.date, '%Y')))
				iyear <- as.numeric(str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.year))))
				iyear <- iyear+1
				if(iyear > range.TsMap.year[2]) iyear <- range.TsMap.year[1]
				tkset(EnvPICSAplot$spin.TsMap.year, iyear)

				ret <- read.PicsaTSData()
				if(is.null(ret)) return(NULL)

				imgContainer <- PICSA.Display.TSMaps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvPICSAplot$notebookTab.TSMap, AllOpenTabType, AllOpenTabData)
				EnvPICSAplot$notebookTab.TSMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		#################

		tkgrid(cb.TsMap.picsavar, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.TsMap.dryspell, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(EnvPICSAplot$spin.TsMap.dryspell, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(bt.TsMap.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(EnvPICSAplot$spin.TsMap.year, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.TsMap.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.TsMap.plot, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(bt.TsMap.Opt, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.TsMap.picsavar, "Select the variable to plot")
		status.bar.display(cb.TsMap.picsavar, TextOutputVar, "Select the variable to plot")
		infobulle(EnvPICSAplot$spin.TsMap.dryspell, "Dry spell definition (continuous dry days)")
		status.bar.display(EnvPICSAplot$spin.TsMap.dryspell, TextOutputVar, "Dry spell definition (continuous dry days)")
		infobulle(bt.TsMap.prev, "Plot the previous year")
		status.bar.display(bt.TsMap.prev, TextOutputVar, "Plot the previous year")
		infobulle(bt.TsMap.next, "Plot the next year")
		status.bar.display(bt.TsMap.next, TextOutputVar, "Plot the next year")
		infobulle(EnvPICSAplot$spin.TsMap.year, "Select the year to plot")
		status.bar.display(EnvPICSAplot$spin.TsMap.year, TextOutputVar, "Select the year to plot")

		#################

		tkbind(cb.TsMap.picsavar, "<<ComboboxSelected>>", function(){
			stateDrySpl <- if(tclvalue(EnvPICSAplot$varPICSA) == "Dry Spells") "normal" else "disabled"
			tkconfigure(EnvPICSAplot$spin.TsMap.dryspell, state = stateDrySpl)

			ret <- read.PicsaTSData()
			if(is.null(ret)) return(NULL)
		})

		##############################################

		framePICSACLMMap <- ttklabelframe(subfr3, text = "Climatological Analysis", relief = 'groove')

		ANALYSIS <- c('Average', 'Median', 'Standard deviation', 'Trend', 'Percentiles', 'Frequency')
		EnvPICSAplot$analysis.method <- tclVar('Average')
		EnvPICSAplot$mth.perc <- tclVar(95)
		EnvPICSAplot$low.thres <- tclVar("09-15")
		EnvPICSAplot$up.thres <- tclVar("11-30")
		EnvPICSAplot$trend <- tclVar("Change (trend) / year")
		TRENDOPT <- c("Change (trend) / year", "Change (trend) over the period", "Change (trend) / average (in %)")
		# TRENDOPT <- c("Trend / year", "Trend over the period", "Trend / average (in %)")

		statePrc <- if(tclvalue(EnvPICSAplot$analysis.method) == 'Percentiles') 'normal' else 'disabled'
		stateFrq <- if(tclvalue(EnvPICSAplot$analysis.method) == 'Frequency') 'normal' else 'disabled'
		stateTrend <- if(tclvalue(EnvPICSAplot$analysis.method) == 'Trend') 'normal' else 'disabled'

		cb.anMthd <- ttkcombobox(framePICSACLMMap, values = ANALYSIS, textvariable = EnvPICSAplot$analysis.method, width = largeur3)
		txt.Percent <- tklabel(framePICSACLMMap, text = "Percentile",  anchor = 'w', justify = 'left')
		en.Percent <- tkentry(framePICSACLMMap, textvariable = EnvPICSAplot$mth.perc, width = 3, state = statePrc)

		txt.Freq1 <- tklabel(framePICSACLMMap, text = "Between",  anchor = 'w', justify = 'left')
		en.Freq1 <- tkentry(framePICSACLMMap, textvariable = EnvPICSAplot$low.thres, width = 5, state = stateFrq)
		txt.Freq2 <- tklabel(framePICSACLMMap, text = "And",  anchor = 'w', justify = 'left')
		en.Freq2 <- tkentry(framePICSACLMMap, textvariable = EnvPICSAplot$up.thres, width = 5, state = stateFrq)
		bt.ClimMap.plot <- ttkbutton(framePICSACLMMap, text = "PLOT")
		cb.Trend <- ttkcombobox(framePICSACLMMap, values =TRENDOPT, textvariable = EnvPICSAplot$trend, width = largeur3, state = stateTrend)
		bt.ClimMap.Opt <- ttkbutton(framePICSACLMMap, text = "Options")

		#################
		EnvPICSAplot$climMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
												userCol = list(custom = FALSE, color = NULL),
												userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
												title = list(user = FALSE, title = ''),
												colkeyLab = list(user = FALSE, label = ''),
												scalebar = list(add = FALSE, pos = 'bottomleft'))

		tkconfigure(bt.ClimMap.Opt, command = function(){
			if(!is.null(EnvPICSAplot$climdata)){
				atlevel <- pretty(EnvPICSAplot$climdata$z, n = 10, min.n = 7)
				if(is.null(EnvPICSAplot$climMapOp$userLvl$levels)){
					EnvPICSAplot$climMapOp$userLvl$levels <- atlevel
				}else{
					if(!EnvPICSAplot$climMapOp$userLvl$custom)
						EnvPICSAplot$climMapOp$userLvl$levels <- atlevel
				}
			}
			EnvPICSAplot$climMapOp <- climatoAnalysis.MapOptions(main.win, EnvPICSAplot$climMapOp)
		})

		EnvPICSAplot$notebookTab.climMap <- NULL
		tkconfigure(bt.ClimMap.plot, command = function(){
			ret <- calculate.ClimStat()
			if(is.null(ret)) return(NULL)

			if(!is.null(EnvPICSAplot$climdata)){
				imgContainer <- PICSA.Display.ClimMap(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvPICSAplot$notebookTab.climMap, AllOpenTabType, AllOpenTabData)
				EnvPICSAplot$notebookTab.climMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		#################

		tkgrid(cb.anMthd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.Percent, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.Percent, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(txt.Freq1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.Freq1, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.Freq2, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.Freq2, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.ClimMap.plot, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.Trend, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.ClimMap.Opt, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.anMthd, "Select the analysis method")
		status.bar.display(cb.anMthd, TextOutputVar, "Select the analysis method")
		infobulle(en.Percent, "Enter the nth percentile to be calculated")
		status.bar.display(en.Percent, TextOutputVar, "Enter the nth percentile to be calculated")
		infobulle(en.Freq1, "Enter the lower bound of the interval to count the number of occurrences.\nIn the case of Onset and Cessation, the limit should be of the form Month-Day,\nnumber otherwise")
		status.bar.display(en.Freq1, TextOutputVar, "Enter the lower bound of the interval to count the number of occurrences.\nIn the case of Onset and Cessation, the limit should be of the form Month-Day,\nnumber otherwise")
		infobulle(en.Freq2, "Enter the upper bound of the interval to count the number of occurrences.\nIn the case of Onset and Cessation, the limit should be of the form Month-Day,\nnumber otherwise")
		status.bar.display(en.Freq2, TextOutputVar, "Enter the upper bound of the interval to count the number of occurrences.\nIn the case of Onset and Cessation, the limit should be of the form Month-Day,\nnumber otherwise")

		#################
		tkbind(cb.anMthd, "<<ComboboxSelected>>", function(){
			statePrc <- if(tclvalue(EnvPICSAplot$analysis.method) == 'Percentiles') 'normal' else 'disabled'
			stateFrq <- if(tclvalue(EnvPICSAplot$analysis.method) == 'Frequency') 'normal' else 'disabled'
			stateTrend <- if(tclvalue(EnvPICSAplot$analysis.method) == 'Trend') 'normal' else 'disabled'
			tkconfigure(en.Percent, state = statePrc)
			tkconfigure(en.Freq1, state = stateFrq)
			tkconfigure(en.Freq2, state = stateFrq)
			tkconfigure(cb.Trend, state = stateTrend)
		})

		##############################################

		tkgrid(framePICSADat, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(framePICSATSMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(framePICSACLMMap, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab4
	frTab4 <- tkframe(cmd.tab4)
	tkgrid(frTab4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab4, 0, weight = 1)

	scrw4 <- bwScrolledWindow(frTab4)
	tkgrid(scrw4)
	tkgrid.columnconfigure(scrw4, 0, weight = 1)
	subfr4 <- bwScrollableFrame(scrw4, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr4, 0, weight = 1)

		##############################################

		framePICSATSGRAPH <- ttklabelframe(subfr4, text = "Time Series Graph", relief = 'groove')

		varTSPLOT <- c("From Maps", "Daily Rainfall")
		EnvPICSAplot$graph$varTSp <- tclVar("From Maps")

		typeTSPLOT <- c("Line", "Barplot", "Probability", "ENSO-Line", "ENSO-Barplot", "ENSO-Proba", "Anomaly")
		EnvPICSAplot$graph$typeTSp <- tclVar("Line")
		EnvPICSAplot$graph$averageTSp <- tclVar(FALSE)
		EnvPICSAplot$graph$tercileTSp <- tclVar(FALSE)
		EnvPICSAplot$graph$trendTSp <- tclVar(FALSE)

		stateTsp <- if(tclvalue(EnvPICSAplot$graph$varTSp) == "From Maps") "normal" else "disabled"
		stateType <- if(tclvalue(EnvPICSAplot$graph$typeTSp)%in%c("Line", "ENSO-Line") && tclvalue(EnvPICSAplot$graph$varTSp) == "From Maps") "normal" else "disabled"

		frTS0 <- tkframe(framePICSATSGRAPH)
		cb.varTSp <- ttkcombobox(frTS0, values = varTSPLOT, textvariable = EnvPICSAplot$graph$varTSp, width = largeur4)
		cb.typeTSp <- ttkcombobox(frTS0, values = typeTSPLOT, textvariable = EnvPICSAplot$graph$typeTSp, width = largeur5, state = stateTsp)
		bt.TsGraph.plot <- ttkbutton(frTS0, text = "PLOT", width = 8)
		tkgrid(cb.varTSp, cb.typeTSp, bt.TsGraph.plot)

		frTS1 <- tkframe(framePICSATSGRAPH)
		chk.meanTSp <- tkcheckbutton(frTS1, variable = EnvPICSAplot$graph$averageTSp, text =  "Add Mean", anchor = 'w', justify = 'left', state = stateType)
		chk.tercTSp <- tkcheckbutton(frTS1, variable = EnvPICSAplot$graph$tercileTSp, text =  "Add Terciles", anchor = 'w', justify = 'left', state = stateType)
		chk.trendTSp <- tkcheckbutton(frTS1, variable = EnvPICSAplot$graph$trendTSp, text =  "Add Trend", anchor = 'w', justify = 'left', state = stateType)
		tkgrid(chk.meanTSp, chk.tercTSp, chk.trendTSp)

		bt.TsGraph.Opt <- ttkbutton(framePICSATSGRAPH, text = "Options", width = 8, state = stateTsp)

		#################
		EnvPICSAplot$TSGraphOp <- list(
					anomaly = list(
							anom = list(perc.anom = FALSE, basePeriod = FALSE, startYr.anom = 1981, endYr.anom = 2010),
							xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
							ylim = list(is.min = FALSE, min = -100, is.max = FALSE, max = 100),
							axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
							title = list(is.title = FALSE, title = '', position = 'top'),
							colors = list(negative = "blue", positive = "red")
							),
					bar = list(
						xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
						ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
						axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
						title = list(is.title = FALSE, title = '', position = 'top'),
						colors = list(col = "darkblue")
						),
					line = list(
						xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
						ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
						axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
						title = list(is.title = FALSE, title = '', position = 'top'),
						plot = list(type = 'both',
							col = list(line = "red", points = "blue"),
							lwd = 2, cex = 1.4),
						legend = list(
							is = list(mean = FALSE, tercile = FALSE, linear = FALSE),
							add = list(mean = FALSE, tercile = FALSE, linear = FALSE),
							col = list(mean = "black", tercile1 = "green", tercile2 = "blue", linear = "purple3"),
							text = list(mean = "Average", tercile1 = "Tercile 0.33333", tercile2 = "Tercile 0.66666", linear = "Trend line"),
							lwd = list(mean = 2, tercile = 2, linear = 2))
						),
					proba = list(
						xlim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
						ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
						axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
						title = list(is.title = FALSE, title = '', position = 'top'),
						plot = list(type = 'both',
							col = list(line = "red", points = "blue"),
							lwd = 2, cex = 0.8),
						proba = list(theoretical = TRUE, col = 'black', lwd = 2)
						),
					line.enso = list(
						xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
						ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
						axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
						title = list(is.title = FALSE, title = '', position = 'top'),
						plot = list(lwd = 2, cex = 2, col = list(line = "black",
									points = c("blue", "gray", "red"))),
						legend = list(
							is = list(mean = FALSE, tercile = FALSE, linear = FALSE),
							add = list(mean = FALSE, tercile = FALSE, linear = FALSE),
							col = list(mean = "darkblue", tercile1 = "chartreuse4", tercile2 = "darkgoldenrod4", linear = "purple3"),
							text = list(mean = "Average", tercile1 = "Tercile 0.33333", tercile2 = "Tercile 0.66666", linear = "Trend line"),
							lwd = list(mean = 2, tercile = 2, linear = 2))
						),
					bar.enso = list(
						xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
						ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
						axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
						title = list(is.title = FALSE, title = '', position = 'top'),
						colors = list(col = c("blue", "gray", "red"))
						),
					proba.enso = list(
						xlim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
						ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
						axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
						title = list(is.title = FALSE, title = '', position = 'top'),
						plot = list(type = 'both', lwd = 2, cex = 1.4,
							all = list(line = "black", points = "lightgray"),
							nina = list(line = "blue", points = "lightblue"),
							neutre = list(line = "gray", points = "lightgray"),
							nino = list(line = "red", points = "lightpink"))
						)
					)

		tkconfigure(bt.TsGraph.Opt, command = function(){
			suffix.fun <- switch(tclvalue(EnvPICSAplot$graph$typeTSp),
									"Anomaly" = "Anomaly",
									"Barplot" = "Bar",
									"Line" = "Line",
									"Probability" = "Proba",
									"ENSO-Line" = "LineENSO",
									"ENSO-Barplot" = "BarENSO",
									"ENSO-Proba" = "ProbaENSO")
			plot.fun <- match.fun(paste0("climatoAnalysis.GraphOptions.", suffix.fun))
			EnvPICSAplot$TSGraphOp <- plot.fun(main.win, EnvPICSAplot$TSGraphOp)
		})

		EnvPICSAplot$notebookTab.tsplot <- NULL
		tkconfigure(bt.TsGraph.plot, command = function(){
			if(!is.null(EnvPICSAplot$output)){
				imgContainer <- PICSA.Display.TSPlot(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvPICSAplot$notebookTab.tsplot, AllOpenTabType, AllOpenTabData)
				EnvPICSAplot$notebookTab.tsplot <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkgrid(frTS0, row = 0, column = 0, sticky = 'we', pady = 1, columnspan = 3)
		tkgrid(frTS1, row = 1, column = 0, sticky = 'we', pady = 1, columnspan = 3)
		tkgrid(bt.TsGraph.Opt, row = 2, column = 2, sticky = 'e', pady = 1, columnspan = 1)

		#################

		tkbind(cb.varTSp, "<<ComboboxSelected>>", function(){
			stateTsp <- if(tclvalue(EnvPICSAplot$graph$varTSp) == "From Maps") "normal" else "disabled"
			tkconfigure(cb.typeTSp, state = stateTsp)
			tkconfigure(bt.TsGraph.Opt, state = stateTsp)

			stateType <- if(tclvalue(EnvPICSAplot$graph$typeTSp)%in%c("Line", "ENSO-Line") && tclvalue(EnvPICSAplot$graph$varTSp) == "From Maps") "normal" else "disabled"
			tkconfigure(chk.meanTSp, state = stateType)
			tkconfigure(chk.tercTSp, state = stateType)
			tkconfigure(chk.trendTSp, state = stateType)
		})

		tkbind(cb.typeTSp, "<<ComboboxSelected>>", function(){
			stateType <- if(tclvalue(EnvPICSAplot$graph$typeTSp)%in%c("Line", "ENSO-Line") && tclvalue(EnvPICSAplot$graph$varTSp) == "From Maps") "normal" else "disabled"
			tkconfigure(chk.meanTSp, state = stateType)
			tkconfigure(chk.tercTSp, state = stateType)
			tkconfigure(chk.trendTSp, state = stateType)
		})

		tkbind(chk.meanTSp, "<Button-1>", function(){
			EnvPICSAplot$TSGraphOp$line$legend$add$mean <- 
						if(tclvalue(EnvPICSAplot$graph$averageTSp) == '0') TRUE else FALSE
			EnvPICSAplot$TSGraphOp$line.enso$legend$add$mean <- 
						if(tclvalue(EnvPICSAplot$graph$averageTSp) == '0') TRUE else FALSE
		})

		tkbind(chk.tercTSp, "<Button-1>", function(){
			EnvPICSAplot$TSGraphOp$line$legend$add$tercile <- 
						if(tclvalue(EnvPICSAplot$graph$tercileTSp) == '0') TRUE else FALSE
			EnvPICSAplot$TSGraphOp$line.enso$legend$add$tercile <- 
						if(tclvalue(EnvPICSAplot$graph$tercileTSp) == '0') TRUE else FALSE
		})

		tkbind(chk.trendTSp, "<Button-1>", function(){
			EnvPICSAplot$TSGraphOp$line$legend$add$linear <- 
						if(tclvalue(EnvPICSAplot$graph$trendTSp) == '0') TRUE else FALSE
			EnvPICSAplot$TSGraphOp$line.enso$legend$add$linear <- 
						if(tclvalue(EnvPICSAplot$graph$trendTSp) == '0') TRUE else FALSE
		})

		##############################################

		frameSTNCrds <- ttklabelframe(subfr4, text = "Station/Coordinates", relief = 'groove')

		frTS2 <- tkframe(frameSTNCrds)
		EnvPICSAplot$graph$lonLOC <- tclVar()
		EnvPICSAplot$graph$latLOC <- tclVar()
		EnvPICSAplot$graph$stnIDTSp <- tclVar()

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

		##############################################

		tkgrid(framePICSATSGRAPH, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameSTNCrds, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab5
	frTab5 <- tkframe(cmd.tab5)
	tkgrid(frTab5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab5, 0, weight = 1)

	scrw5 <- bwScrolledWindow(frTab5)
	tkgrid(scrw5)
	tkgrid.columnconfigure(scrw5, 0, weight = 1)
	subfr5 <- bwScrollableFrame(scrw5, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr5, 0, weight = 1)

		##############################################

		frameSHP <- ttklabelframe(subfr5, text = "Boundaries", relief = 'groove')

		EnvPICSAplot$shp$add.shp <- tclVar(FALSE)
		file.plotShp <- tclVar()
		stateSHP <- "disabled"

		chk.addshp <- tkcheckbutton(frameSHP, variable = EnvPICSAplot$shp$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
		bt.addshpOpt <- ttkbutton(frameSHP, text = "Options", state = stateSHP)
		cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur2, state = stateSHP)
		bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

		########
		tkconfigure(bt.addshp, command = function(){
			shp.opfiles <- getOpenShp(main.win, all.opfiles)
			if(!is.null(shp.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'shp'
				AllOpenFilesData[[nopf+1]] <<- shp.opfiles
				tclvalue(file.plotShp) <- AllOpenFilesData[[nopf+1]][[1]]
				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]

				tkconfigure(cb.en.INPrec, values = unlist(listOpenFiles), textvariable = input.Prec)
				tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = file.plotShp)

				shpofile <- getShpOpenData(file.plotShp)
				if(is.null(shpofile)) EnvPICSAplot$shp$ocrds <- NULL
				EnvPICSAplot$shp$ocrds <- getBoundaries(shpofile[[2]])
			}else return(NULL)
		})

		########
		EnvPICSAplot$SHPOp <- list(col = "black", lwd = 1.5)

		tkconfigure(bt.addshpOpt, command = function(){
			EnvPICSAplot$SHPOp <- climatoAnalysis.GraphOptions.LineSHP(main.win, EnvPICSAplot$SHPOp)
		})

		########
		tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
		tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		#################
		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile)) EnvPICSAplot$shp$ocrds <- NULL
			EnvPICSAplot$shp$ocrds <- getBoundaries(shpofile[[2]])
		})

		tkbind(chk.addshp, "<Button-1>", function(){
			stateSHP <- if(tclvalue(EnvPICSAplot$shp$add.shp) == "1") "disabled" else "normal"
			tkconfigure(cb.addshp, state = stateSHP)
			tkconfigure(bt.addshp, state = stateSHP)
			tkconfigure(bt.addshpOpt, state = stateSHP)
		})

		##############################################

		tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', pady = 1)

	#######################################################################################################

	load.PICSA.Data <- function(){
		range.TsMap.year <- range(as.numeric(format(EnvPICSAplot$output$start.date, '%Y')))
		tkconfigure(EnvPICSAplot$spin.TsMap.year, from = range.TsMap.year[1], to = range.TsMap.year[2])
		tkset(EnvPICSAplot$spin.TsMap.year, range.TsMap.year[2])

		###################
		ret <- read.PicsaTSData()
		if(is.null(ret)) return(NULL)

		###################
		plotCHOIX <- c("anomaly", "bar", "line", "line.enso", "bar.enso")
		for(pp in plotCHOIX){
			EnvPICSAplot$TSGraphOp[[pp]]$xlim$min <- range.TsMap.year[1]
			EnvPICSAplot$TSGraphOp[[pp]]$xlim$max <- range.TsMap.year[2]
		}

		###################
		# widgets.Station.Pixel
		tkdestroy(frTS2)
		frTS2 <<- tkframe(frameSTNCrds)

		if(EnvPICSAplot$output$data.type == "cdtstation"){
			stnIDTSPLOT <- EnvPICSAplot$output$data$id
			txt.stnSel <- tklabel(frTS2, text = "Select a station to plot", anchor = 'w', justify = 'left')
			txt.stnID <- tklabel(frTS2, text = "Station", anchor = 'e', justify = 'right')
			cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = EnvPICSAplot$graph$stnIDTSp, width = largeur6)
			tclvalue(EnvPICSAplot$graph$stnIDTSp) <- stnIDTSPLOT[1]

			tkgrid(txt.stnSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.stnID, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}else{
			txt.crdSel <- tklabel(frTS2, text = "Enter longitude and latitude to plot", anchor = 'w', justify = 'left')
			txt.lonLoc <- tklabel(frTS2, text = "Longitude", anchor = 'e', justify = 'right')
			en.lonLoc <- tkentry(frTS2, textvariable = EnvPICSAplot$graph$lonLOC, width = 8)
			txt.latLoc <- tklabel(frTS2, text = "Latitude", anchor = 'e', justify = 'right')
			en.latLoc <- tkentry(frTS2, textvariable = EnvPICSAplot$graph$latLOC, width = 8)
			stnIDTSPLOT <- ""
			tclvalue(EnvPICSAplot$graph$stnIDTSp) <- ""

			tkgrid(txt.crdSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.lonLoc, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.lonLoc, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.latLoc, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.latLoc, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}
		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

		###################
		# load daily precip
		if(EnvPICSAplot$output$data.type == "cdtstation"){
			file.daily.rr <- file.path(EnvPICSAplot$PathPicsa, "CDTDATASET", "Daily_precip.rds") 
		}else file.daily.rr <- EnvPICSAplot$output$daily.precip

		if(!file.exists(file.daily.rr)){
			InsertMessagesTxt(main.txt.out, paste(file.daily.rr, 'not found'), format = TRUE)
			return(NULL)
		}

		EnvPICSAplot$daily.precip <- readRDS(file.daily.rr)
	}


	#######################################################################################################

	read.PicsaTSData <- function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		tsdata.dir <- switch(str_trim(tclvalue(EnvPICSAplot$varPICSA)),
							"Onset" = "Onset_days",
							"Cessation" = "Cessation_days",
							"Season Length" = "Season_length",
							"Seasonal Rainfall Amounts" = "Seasonal_rain_amount",
							"Number of rain day" = "Number_rainy_day",
							"Maximum daily rain" = "Maximum_rain_daily",
							"Total rain when RR>95thPerc" = "Total_rain_above_Perc95th",
							"Nb of day when RR>95thPerc" = "Number_day_above_Perc95th",
							"Longest Dry Spell" = "Longest_dry_spell",
							"Dry Spells" = "Dry_Spells")

		start.date <- format(EnvPICSAplot$output$start.date, '%Y%m%d')
		start.dateYear <- format(EnvPICSAplot$output$start.date, '%Y')
		idaty <- start.date[start.dateYear == str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.year)))]
		dryspl <- as.numeric(str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.dryspell))))

		if(EnvPICSAplot$output$data.type == "cdtstation"){
			tsdata.path <- file.path(EnvPICSAplot$PathPicsa, "CDTDATASET")
			filetsdata <- file.path(tsdata.path, paste0(tsdata.dir, ".rds"))

			if(!file.exists(filetsdata)){
				InsertMessagesTxt(main.txt.out, paste(filetsdata, 'not found'), format = TRUE)
				return(NULL)
			}

			readTsData <- TRUE
			if(!is.null(EnvPICSAplot$tsdata))
				if(!is.null(EnvPICSAplot$filetsdata))
					if(EnvPICSAplot$filetsdata == filetsdata) readTsData <- FALSE

			if(readTsData){
				EnvPICSAplot$tsdata <- list(date = start.date,
											data = readRDS(filetsdata))
				EnvPICSAplot$filetsdata <- filetsdata
			}

			########
			rasterTsData <- TRUE
			if(!readTsData)
				if(!is.null(EnvPICSAplot$rasterTsData))
					if(EnvPICSAplot$filetsdata == filetsdata)
						if(EnvPICSAplot$rasterTsData == idaty) rasterTsData <- FALSE

			if(tsdata.dir == "Dry_Spells")
				if(!is.null(EnvPICSAplot$oldDryspell))
					if(EnvPICSAplot$oldDryspell != dryspl & !rasterTsData) rasterTsData <- TRUE

			if(rasterTsData){
				idt <- which(EnvPICSAplot$tsdata$date == idaty)
				nx <- nx_ny_as.image(diff(range(EnvPICSAplot$output$data$lon)))
				ny <- nx_ny_as.image(diff(range(EnvPICSAplot$output$data$lat)))

				if(tsdata.dir == "Dry_Spells"){
					tmp <- EnvPICSAplot$tsdata$data[idt, ]
					nval <- sapply(tmp, function(x) (length(x) == 1) & is.na(x[1]))
					tmp <- sapply(tmp, function(x) sum(!is.na(x) & x >= dryspl))
					tmp[nval] <- NA
					rm(nval)
					EnvPICSAplot$oldDryspell <- dryspl
				}else tmp <- as.numeric(EnvPICSAplot$tsdata$data[idt, ])

				tmp <- cdt.as.image(tmp, nx = nx, ny = ny, pts.xy = cbind(EnvPICSAplot$output$data$lon, EnvPICSAplot$output$data$lat))
				EnvPICSAplot$tsdata$x <- tmp$x
				EnvPICSAplot$tsdata$y <- tmp$y
				EnvPICSAplot$tsdata$z <- tmp$z
				EnvPICSAplot$rasterTsData <- idaty
				rm(tmp)
			}
		}else{
			if(tsdata.dir == "Dry_Spells"){
				tsdata.path <- file.path(EnvPICSAplot$PathPicsa, "CDTDATASET", tsdata.dir)
				tsdata.index <- file.path(EnvPICSAplot$PathPicsa, "CDTDATASET", "CDTDATASET.rds")
				filetsdata <- tsdata.path
			}else{
				tsdata.path <- file.path(EnvPICSAplot$PathPicsa, "DATA_NetCDF")
				filetsdata <- file.path(tsdata.path, tsdata.dir, paste0("data_", idaty, ".nc"))
			}

			if(!file.exists(filetsdata)){
				InsertMessagesTxt(main.txt.out, paste(filetsdata, 'not found'), format = TRUE)
				return(NULL)
			}

			readTsData <- TRUE
			if(!is.null(EnvPICSAplot$tsdata))
				if(!is.null(EnvPICSAplot$filetsdata))
					if(EnvPICSAplot$filetsdata == filetsdata) readTsData <- FALSE

			if(tsdata.dir == "Dry_Spells")
				if(!is.null(EnvPICSAplot$oldDryspell))
					if(EnvPICSAplot$oldDryspell != dryspl & !rasterTsData) rasterTsData <- TRUE

			if(readTsData){
				if(tsdata.dir == "Dry_Spells"){
					EnvPICSAplot$tsdata <- readCdtDatasetChunk.sepdir.dates.order(tsdata.index, tsdata.path, idaty, onedate = TRUE)
					zdim <- dim(EnvPICSAplot$tsdata$z)
					nval <- sapply(EnvPICSAplot$tsdata$z, function(x) (length(x) == 1) & is.na(x[1]))
					zval <- sapply(EnvPICSAplot$tsdata$z, function(x) sum(!is.na(x) & x >= dryspl))
					zval[nval] <- NA
					dim(zval) <- zdim
					EnvPICSAplot$tsdata$z <- zval
					rm(nval, zval)
					EnvPICSAplot$oldDryspell <- dryspl
				}else{
					nc <- nc_open(filetsdata)
					EnvPICSAplot$tsdata$x <- nc$dim[[1]]$vals
					EnvPICSAplot$tsdata$y <- nc$dim[[2]]$vals
					EnvPICSAplot$tsdata$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
					nc_close(nc)
				}
				EnvPICSAplot$filetsdata <- filetsdata
			}

			if(is.null(EnvPICSAplot$cdtdataset)){
				tsdata.index <- file.path(EnvPICSAplot$PathPicsa, "CDTDATASET", "CDTDATASET.rds")
				EnvPICSAplot$cdtdataset <- readRDS(tsdata.index)
				EnvPICSAplot$cdtdataset$fileInfo <- tsdata.index
			}
		}

		if(is.null(EnvPICSAplot$ONI)){
			load(file.path(apps.dir, 'data', 'ONI_50-2017.RData'))
			EnvPICSAplot$ONI$date <- format(seq(as.Date('1950-1-15'), as.Date('2017-12-15'), "month"), "%Y%m")
			EnvPICSAplot$ONI$data <- ONI$ts[, 3]
		}

		return(0)
	}

	#######################################################################################################

	calculate.ClimStat <- function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		varPICSA <- str_trim(tclvalue(EnvPICSAplot$varPICSA))
		tsdata.dir <- switch(varPICSA,
							"Onset" = "Onset_days",
							"Cessation" = "Cessation_days",
							"Season Length" = "Season_length",
							"Seasonal Rainfall Amounts" = "Seasonal_rain_amount",
							"Number of rain day" = "Number_rainy_day",
							"Maximum daily rain" = "Maximum_rain_daily",
							"Total rain when RR>95thPerc" = "Total_rain_above_Perc95th",
							"Nb of day when RR>95thPerc" = "Number_day_above_Perc95th",
							"Longest Dry Spell" = "Dry_Spells",
							"Dry Spells" = "Dry_Spells")

		start.dateYear <- as.numeric(format(EnvPICSAplot$output$start.date, '%Y'))
		dryspl <- as.numeric(str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.dryspell))))

		if(EnvPICSAplot$output$data.type == "cdtstation"){
			tsdata.path <- file.path(EnvPICSAplot$PathPicsa, "CDTDATASET")
			filetsdata <- file.path(tsdata.path, paste0(tsdata.dir, ".rds"))

			if(!file.exists(filetsdata)){
				InsertMessagesTxt(main.txt.out, paste(filetsdata, 'not found'), format = TRUE)
				return(NULL)
			}

			StatCalc <- str_trim(tclvalue(EnvPICSAplot$analysis.method))

			calcClim <- TRUE
			if(!is.null(EnvPICSAplot$climdata))
				if(!is.null(EnvPICSAplot$filetsdata1))
					if(EnvPICSAplot$filetsdata1 == filetsdata)
						if(EnvPICSAplot$StatCalc == StatCalc) calcClim <- FALSE

			trendUnit <- str_trim(tclvalue(EnvPICSAplot$trend))
			if(StatCalc == "Trend")
				if(!is.null(EnvPICSAplot$trendUnit))
					if(EnvPICSAplot$trendUnit != trendUnit & !calcClim) calcClim <- TRUE

			if(varPICSA == "Dry Spells")
				if(!is.null(EnvPICSAplot$oldDryspell1))
					if(EnvPICSAplot$oldDryspell1 != dryspl & !calcClim) calcClim <- TRUE

			if(varPICSA == "Longest Dry Spell")
				if(!is.null(EnvPICSAplot$oldDryspell2))
					if(EnvPICSAplot$oldDryspell2 != dryspl & !calcClim) calcClim <- TRUE

			if(calcClim){
				don <- readRDS(filetsdata)
				if(varPICSA == "Dry Spells"){
					ndim <- dim(don)
					nval <- sapply(don, function(x) (length(x) == 1) & is.na(x[1]))
					don <- sapply(don, function(x) sum(!is.na(x) & x >= dryspl))
					don[nval] <- NA
					dim(don) <- ndim
					rm(nval)
					EnvPICSAplot$oldDryspell1 <- dryspl
				}
				if(varPICSA == "Longest Dry Spell"){
					ndim <- dim(don)
					don <- sapply(don, max, na.rm = TRUE)
					don[is.infinite(don)] <- NA
					dim(don) <- ndim
					EnvPICSAplot$oldDryspell2 <- dryspl
				}

				## na min.frac
				don <- statisticFunction(don)
				nx <- nx_ny_as.image(diff(range(EnvPICSAplot$output$data$lon)))
				ny <- nx_ny_as.image(diff(range(EnvPICSAplot$output$data$lat)))
				don <- cdt.as.image(don, nx = nx, ny = ny, pts.xy = cbind(EnvPICSAplot$output$data$lon, EnvPICSAplot$output$data$lat))
				EnvPICSAplot$climdata$x <- don$x
				EnvPICSAplot$climdata$y <- don$y
				EnvPICSAplot$climdata$z <- don$z
				
				rm(don)
				EnvPICSAplot$filetsdata1 <- filetsdata
				EnvPICSAplot$StatCalc <- StatCalc
				if(StatCalc == "Trend") EnvPICSAplot$trendUnit <- trendUnit
			}
		}else{
			tsdata.path <- file.path(EnvPICSAplot$PathPicsa, "CDTDATASET", tsdata.dir)
			tsdata.index <- file.path(EnvPICSAplot$PathPicsa, "CDTDATASET", "CDTDATASET.rds")
			filetsdata <- tsdata.path

			if(!file.exists(filetsdata)){
				InsertMessagesTxt(main.txt.out, paste(filetsdata, 'not found'), format = TRUE)
				return(NULL)
			}

			StatCalc <- str_trim(tclvalue(EnvPICSAplot$analysis.method))

			calcClim <- TRUE
			if(!is.null(EnvPICSAplot$climdata))
				if(!is.null(EnvPICSAplot$filetsdata1))
					if(EnvPICSAplot$filetsdata1 == filetsdata)
						if(EnvPICSAplot$StatCalc == StatCalc) calcClim <- FALSE

			trendUnit <- str_trim(tclvalue(EnvPICSAplot$trend))
			if(StatCalc == "Trend")
				if(!is.null(EnvPICSAplot$trendUnit))
					if(EnvPICSAplot$trendUnit != trendUnit & !calcClim) calcClim <- TRUE

			if(varPICSA == "Dry Spells")
				if(!is.null(EnvPICSAplot$oldDryspell1))
					if(EnvPICSAplot$oldDryspell1 != dryspl & !calcClim) calcClim <- TRUE

			if(varPICSA == "Longest Dry Spell")
				if(!is.null(EnvPICSAplot$oldDryspell2))
					if(EnvPICSAplot$oldDryspell2 != dryspl & !calcClim) calcClim <- TRUE

			if(calcClim){
				index <- readRDS(tsdata.index)

				chunkfile <- sort(unique(index$colInfo$index))
				chunkcalc <- split(chunkfile, ceiling(chunkfile/index$chunkfac))

				toExports <- c("EnvPICSAplot", "quantile8", "statisticFunction", 
							 	"TrendFunction", "doparallel")
				packages <- c("stringr", "tcltk")

				is.parallel <- doparallel(length(chunkcalc) > 10)
				`%parLoop%` <- is.parallel$dofun
				don <- foreach(jj = seq_along(chunkcalc), .export = toExports, .packages = packages) %parLoop% {
					don <- lapply(chunkcalc[[jj]], function(j){
						file.rds <- file.path(tsdata.path, paste0(j, ".rds"))
						readRDS(file.rds)
					})
					don <- do.call(cbind, don)

					if(varPICSA == "Dry Spells"){
						ndim <- dim(don)
						nval <- sapply(don, function(x) (length(x) == 1) & is.na(x[1]))
						don <- sapply(don, function(x) sum(!is.na(x) & x >= dryspl))
						don[nval] <- NA
						dim(don) <- ndim
						rm(nval)
					}
					if(varPICSA == "Longest Dry Spell"){
						ndim <- dim(don)
						don <- sapply(don, max, na.rm = TRUE)
						don[is.infinite(don)] <- NA
						dim(don) <- ndim
					}

					statisticFunction(don)
				}
				if(is.parallel$stop) stopCluster(is.parallel$cluster)

				don <- do.call(c, don)
				don <- don[index$colInfo$order]
				dim(don) <- sapply(index$coords$mat, length)

				EnvPICSAplot$climdata$x <- index$coords$mat$x
				EnvPICSAplot$climdata$y <- index$coords$mat$y
				EnvPICSAplot$climdata$z <- don
				
				rm(don, index)
				if(varPICSA == "Dry Spells") EnvPICSAplot$oldDryspell1 <- dryspl
				if(varPICSA == "Longest Dry Spell") EnvPICSAplot$oldDryspell2 <- dryspl
				if(StatCalc == "Trend") EnvPICSAplot$trendUnit <- trendUnit
				EnvPICSAplot$filetsdata1 <- filetsdata
				EnvPICSAplot$StatCalc <- StatCalc
			}
		}

		return(0)
	}

	#######################################################################################################

	TrendFunction <- function(Y, X){
		ncolY <- ncol(Y)
		nrowY <- nrow(Y)
		X <- if(is.matrix(X)) X else matrix(X, nrow = nrowY, ncol = ncolY)
		ina <- is.na(X) | is.na(Y)
		X[ina] <- NA
		Y[ina] <- NA
		nbY <- colSums(!is.na(Y))
		nbY[nbY < 3] <- NA

		mX <- colMeans(X, na.rm = TRUE)
		mY <- colMeans(Y, na.rm = TRUE)
		vX <- matrixStats::colVars(X, na.rm = TRUE)
		# vY <- matrixStats::colVars(Y, na.rm = TRUE)

		X1 <- X - matrix(mX, nrowY, ncolY, byrow = TRUE)
		Y1 <- Y - matrix(mY, nrowY, ncolY, byrow = TRUE)
		COV <- colSums(X1 * Y1, na.rm = TRUE) / (nbY - 1)
		alpha <- COV / vX
		return(alpha)
	}

	###################################

	statisticFunction <- function(don){
		start.dateYear <- as.numeric(format(EnvPICSAplot$output$start.date, '%Y'))

		if(str_trim(tclvalue(EnvPICSAplot$analysis.method)) == 'Average'){
			don <- colMeans(don, na.rm = TRUE)
		}

		if(str_trim(tclvalue(EnvPICSAplot$analysis.method)) == 'Median'){
			don <- matrixStats::colMedians(don, na.rm = TRUE)
		}

		if(str_trim(tclvalue(EnvPICSAplot$analysis.method)) == 'Standard deviation'){
			don <- matrixStats::colSds(don, na.rm = TRUE)
		}

		if(str_trim(tclvalue(EnvPICSAplot$analysis.method)) == 'Trend'){
			tmp <- TrendFunction(don, start.dateYear)
			if(str_trim(tclvalue(EnvPICSAplot$trend)) == "Change (trend) / year") don <- tmp
			if(str_trim(tclvalue(EnvPICSAplot$trend)) == "Change (trend) over the period"){
				don <- tmp * (diff(range(start.dateYear, na.rm = TRUE))+1)
			}
			if(str_trim(tclvalue(EnvPICSAplot$trend)) == "Change (trend) / average (in %)"){
				don <- 100 * tmp * (diff(range(start.dateYear, na.rm = TRUE))+1) / colMeans(don, na.rm = TRUE)
			}
			rm(tmp)
		}

		if(str_trim(tclvalue(EnvPICSAplot$analysis.method)) == 'Percentiles'){
			Q <- as.numeric(tclvalue(EnvPICSAplot$mth.perc))/100
			don <- apply(don, 2, quantile8, probs = Q)
		}

		if(str_trim(tclvalue(EnvPICSAplot$analysis.method)) == 'Frequency'){
			xlow <- tclvalue(EnvPICSAplot$low.thres)
			xup <- tclvalue(EnvPICSAplot$up.thres)
			if(str_trim(tclvalue(EnvPICSAplot$varPICSA))%in%c("Onset", "Cessation")){
				dlo <- as.POSIXlt(as.Date(paste(start.dateYear, xlow, sep = '-')))
				dup <- as.POSIXlt(as.Date(paste(start.dateYear, xup, sep = '-')))
				ix <- dlo > dup
				dup$year[ix] <- dup$year[ix] + 1
				xlow <- as.numeric(as.Date(dlo) - EnvPICSAplot$output$start.date)
				xup <- as.numeric(as.Date(dup) - EnvPICSAplot$output$start.date)
			}else{
				xlow <- as.numeric(xlow)
				xup <- as.numeric(xup)
			}
			don <- colSums(don >= xlow & don <= xup, na.rm = TRUE)
		}

		return(don)
	}

	#######################################################################################################
	tcl('update')
	tkgrid(cmd.frame, sticky = '', pady = 1)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)
	######
	return(cmd.frame)
}


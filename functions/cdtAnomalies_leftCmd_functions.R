
anomaliesCalcPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(46)
		largeur0 <- as.integer(w.scale(17)/sfont0)
		largeur1 <- as.integer(w.scale(27)/sfont0)
		largeur2 <- as.integer(w.scale(29)/sfont0)
		largeur3 <- 20
		largeur4 <- 21
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(48)
		largeur0 <- as.integer(w.scale(18)/sfont0)
		largeur1 <- as.integer(w.scale(22)/sfont0)
		largeur2 <- as.integer(w.scale(23)/sfont0)
		largeur3 <- 15
		largeur4 <- 14
	}

	# GeneralParameters <- fromJSON(file.path(apps.dir, 'init_params', 'ClimatoAnalysis.json'))
	# MOIS <- format(ISOdate(2014, 1:12, 1), "%B")
	GeneralParameters <- list(intstep = "dekadal", data.type = "cdtstation", 
							cdtstation = list(file = ""),
							cdtdataset = list(index = ""),
							cdtnetcdf = list(dir = "", sample = "", format = "rfe_%s%s%s.nc"),
							Dates = list(start.year = 1981, start.mon = 1, start.dek = 1,
										end.year = 2017, end.mon = 12, end.dek = 3),
							climato = list(clim.exist = FALSE, clim.file = "",
									start = 1981, end = 2010, minyear = 20, window = 0),
							anomaly = "Difference", outdir = list(update = FALSE, dir = ""))

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Anomalies")
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

		#######################

		frameTimeS <- ttklabelframe(subfr1, text = "Time step of input data", relief = 'groove')

		timeSteps <- tclVar()
		CbperiodVAL <- c('Daily data', 'Pentad data', 'Dekadal data', 'Monthly data')
		tclvalue(timeSteps) <- switch(GeneralParameters$intstep, 
										'daily' = CbperiodVAL[1],
										'pentad' = CbperiodVAL[2],
										'dekadal' = CbperiodVAL[3],
										'monthly' = CbperiodVAL[4])

		cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = timeSteps, width = largeur1)

		tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.fperiod, 'Select the time step of the data')
		status.bar.display(cb.fperiod, TextOutputVar, 'Select the time step of the data')

		############

		tkbind(cb.fperiod, "<<ComboboxSelected>>", function(){
			if(tclvalue(updateAnom) == '0'){
				statedayW <- if(str_trim(tclvalue(timeSteps)) == 'Daily data' &
								tclvalue(climDataExist) == '0') "normal" else "disabled"
			}else statedayW <- "disabled"

			tkconfigure(en.daywin, state = statedayW)
			tclvalue(day.txtVar) <- switch(tclvalue(timeSteps), 'Dekadal data' = 'Dek', 'Pentad data' = 'Pen', 'Day')
			stateday <- if(tclvalue(timeSteps) == 'Monthly data') 'disabled' else 'normal'
			tkconfigure(en.day1, state = stateday)
			tkconfigure(en.day2, state = stateday)
		})

		#######################

		frameInData <- ttklabelframe(subfr1, text = "Input Data", relief = 'groove')

		DataType <- tclVar()
		CbdatatypeVAL <- c('CDT stations data format', 'CDT dataset format (gridded)')
		# CbdatatypeVAL <- c('CDT stations data format', 'CDT dataset format (gridded)', 'NetCDF gridded data')
		tclvalue(DataType) <- switch(GeneralParameters$data.type,
									'cdtstation' = CbdatatypeVAL[1],
									'cdtdataset' = CbdatatypeVAL[2],
									'cdtnetcdf' = CbdatatypeVAL[3])

		if(GeneralParameters$data.type == 'cdtstation'){
			input.file <- tclVar(GeneralParameters$cdtstation$file)
			txt.INData <- 'File containing stations data'
			stateSetNC <- "disabled"
		}else if(GeneralParameters$data.type == 'cdtdataset'){
			input.file <- tclVar(GeneralParameters$cdtdataset$index)
			txt.INData <- 'Index file (*.rds) of the dataset'
			stateSetNC <- "disabled"
		}else{
			input.file <- tclVar(GeneralParameters$cdtnetcdf$dir)
			txt.INData <- 'Directory of the NetCDF files'
			stateSetNC <- "normal"
		}
		txt.INData.var <- tclVar(txt.INData)

		txt.datatype <- tklabel(frameInData, text = "Format", anchor = 'w', justify = 'left')
		cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)

		txt.infile <- tklabel(frameInData, text = tclvalue(txt.INData.var), textvariable = txt.INData.var, anchor = 'w', justify = 'left')
		# set.infile <- tkbutton(frameInData, text = "Settings", width = 5, state = stateSetNC)

		if(GeneralParameters$data.type == 'cdtstation'){
			cb.en.infile <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)
		}else{
			cb.en.infile <- tkentry(frameInData, textvariable = input.file, width = largeur2)
		}
		bt.infile <- tkbutton(frameInData, text = "...")

		############
		settingINData <- GeneralParameters$settingINData
		# tkconfigure(set.infile, command = function(){
		# 	GeneralParameters$cdtnetcdf <<- getInfoNetcdfData(main.win, GeneralParameters$cdtnetcdf,
		# 														str_trim(tclvalue(input.file)),
		# 														tclvalue(timeSteps))
		# 	settingINData <<- 1
		# })

		tkconfigure(bt.infile, command = function(){
			if(GeneralParameters$data.type == 'cdtstation'){
				dat.opfiles <- getOpenFiles(main.win, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(input.file) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.en.infile, values = unlist(listOpenFiles), textvariable = input.file)
				}else return(NULL)
			}else if(GeneralParameters$data.type == 'cdtdataset'){
				filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
				tclvalue(input.file) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
			}else{
				dirnc <- tk_choose.dir(getwd(), "")
				tclvalue(input.file) <- if(dirnc%in%c("", "NA") | is.na(dirnc)) "" else dirnc
			}
		})

		############

		tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.infile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		# tkgrid(set.infile, row = 1, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.en.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.infile, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		############
		infobulle(cb.datatype, 'Select the format of the input data')
		status.bar.display(cb.datatype, TextOutputVar, 'Select the format of the input data')

		if(GeneralParameters$data.type == 'cdtstation'){
			infobulle(cb.en.infile, 'Select the file containing the input data')
			status.bar.display(cb.en.infile, TextOutputVar, 'Select the file containing the input data')
			infobulle(bt.infile, 'Browse file if not listed')
			status.bar.display(bt.infile, TextOutputVar, 'Browse file if not listed')
		}else if(GeneralParameters$data.type == 'cdtdataset'){
			infobulle(cb.en.infile, 'Enter the full path to the file <dataset name>.rds')
			status.bar.display(cb.en.infile, TextOutputVar, 'Enter the full path to the file <dataset name>.rds')
			infobulle(bt.infile, 'or browse here')
			status.bar.display(bt.infile, TextOutputVar, 'or browse here')
		}else{
			infobulle(cb.en.infile, 'Enter the full path to directory containing the netcdf files')
			status.bar.display(cb.en.infile, TextOutputVar, 'Enter the full path to directory containing the netcdf files')
			infobulle(bt.infile, 'or browse here')
			status.bar.display(bt.infile, TextOutputVar, 'or browse here')
		}

		############

		tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
			tkdestroy(cb.en.infile)
			tclvalue(input.file) <- ''

			###
			stateSetNC <- if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data') "normal" else "disabled"
			# tkconfigure(set.infile, state = stateSetNC)

			###
			if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
				tclvalue(txt.INData.var) <- 'File containing stations data'

				cb.en.infile <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)

				tkconfigure(bt.infile, command = function(){
					dat.opfiles <- getOpenFiles(main.win, all.opfiles)
					if(!is.null(dat.opfiles)){
						nopf <- length(AllOpenFilesType)
						AllOpenFilesType[[nopf+1]] <<- 'ascii'
						AllOpenFilesData[[nopf+1]] <<- dat.opfiles

						listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
						tclvalue(input.file) <- AllOpenFilesData[[nopf+1]][[1]]
						tkconfigure(cb.en.infile, values = unlist(listOpenFiles), textvariable = input.file)
					}else return(NULL)
				})

				infobulle(cb.en.infile, 'Select the file containing the input data')
				status.bar.display(cb.en.infile, TextOutputVar, 'Select the file containing the input data')
				infobulle(bt.infile, 'Browse file if not listed')
				status.bar.display(bt.infile, TextOutputVar, 'Browse file if not listed')
			}

			###
			if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)'){
				tclvalue(txt.INData.var) <- 'Index file (*.rds) of the dataset'

				cb.en.infile <- tkentry(frameInData, textvariable = input.file, width = largeur2)

				tkconfigure(bt.infile, command = function(){
					filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
					path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
					tclvalue(input.file) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
				})

				infobulle(cb.en.infile, 'Enter the full path to the file <dataset name>.rds')
				status.bar.display(cb.en.infile, TextOutputVar, 'Enter the full path to the file <dataset name>.rds')
				infobulle(bt.infile, 'or browse here')
				status.bar.display(bt.infile, TextOutputVar, 'or browse here')
			}

			###
			if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data'){
				tclvalue(txt.INData.var) <- 'Directory of the NetCDF files'

				cb.en.infile <- tkentry(frameInData, textvariable = input.file, width = largeur2)

				# tkconfigure(set.infile, command = function(){
				# 	GeneralParameters$cdtnetcdf <<- getInfoNetcdfData(main.win, GeneralParameters$cdtnetcdf,
				# 													str_trim(tclvalue(input.file)),
				# 													tclvalue(timeSteps))
				# 	settingINData <<- 1
				# })

				tkconfigure(bt.infile, command = function(){
					dirnc <- tk_choose.dir(getwd(), "")
					tclvalue(input.file) <- if(dirnc%in%c("", "NA") | is.na(dirnc)) "" else dirnc
				})

				infobulle(cb.en.infile, 'Enter the full path to directory containing the netcdf files')
				status.bar.display(cb.en.infile, TextOutputVar, 'Enter the full path to directory containing the netcdf files')
				infobulle(bt.infile, 'or browse here')
				status.bar.display(bt.infile, TextOutputVar, 'or browse here')
			}

			tkgrid(cb.en.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		})

		#############################

		frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		updateAnom <- tclVar(GeneralParameters$outdir$update)
		outAnom <- tclVar(GeneralParameters$outdir$dir)

		if(GeneralParameters$outdir$update){
			txt.upAnom <- 'Path to the file Anomaly.rds'
		}else{
			txt.upAnom <- "Directory to save the outputs"
		}
		txt.upAnom.var <- tclVar(txt.upAnom)

		chk.outAnom <- tkcheckbutton(frameDirSav, variable = updateAnom, text = "Update existing anomalies data", anchor = 'w', justify = 'left')
		txt.outAnom <- tklabel(frameDirSav, text = tclvalue(txt.upAnom.var), textvariable = txt.upAnom.var, anchor = 'w', justify = 'left')
		en.outAnom <- tkentry(frameDirSav, textvariable = outAnom, width = largeur2)
		bt.outAnom <- tkbutton(frameDirSav, text = "...")

		######

		tkconfigure(bt.outAnom, command = function(){
			if(GeneralParameters$outdir$update){
				filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
				path.anomIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
				tclvalue(outAnom) <- if(path.anomIdx%in%c("", "NA") | is.na(path.anomIdx)) "" else path.anomIdx
			}else{
				dirAnom <- tk_choose.dir(getwd(), "")
				tclvalue(outAnom) <- if(dirAnom%in%c("", "NA") | is.na(dirAnom)) "" else dirAnom
			}
		})

		######
		tkgrid(chk.outAnom, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(txt.outAnom, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.outAnom, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.outAnom, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		infobulle(chk.outAnom, 'Check this box to update an existing anomalies data')
		status.bar.display(chk.outAnom, TextOutputVar, 'Check this box to update an existing anomalies data')
		if(GeneralParameters$outdir$update){
			infobulle(en.outAnom, 'Enter the full path to the file Anomaly.rds')
			status.bar.display(en.outAnom, TextOutputVar, 'Enter the full path to the file Anomaly.rds')
		}else{
			infobulle(en.outAnom, 'Enter the full path to directory to save outputs')
			status.bar.display(en.outAnom, TextOutputVar, 'Enter the full path to directory to save outputs')
		}
		infobulle(bt.outAnom, 'or browse here')
		status.bar.display(bt.outAnom, TextOutputVar, 'or browse here')

		#######

		tkbind(chk.outAnom, "<Button-1>", function(){
			if(tclvalue(updateAnom) == '0'){
				tclvalue(txt.upAnom.var) <- 'Path to the file Anomaly.rds'

				tkconfigure(bt.outAnom, command = function(){
					filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
					path.anomIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
					tclvalue(outAnom) <- if(path.anomIdx%in%c("", "NA") | is.na(path.anomIdx)) "" else path.anomIdx
				})

				infobulle(en.outAnom, 'Enter the full path to the file Anomaly.rds')
				status.bar.display(en.outAnom, TextOutputVar, 'Enter the full path to the file Anomaly.rds')
			}else{
				tclvalue(txt.upAnom.var) <- "Directory to save the outputs"

				tkconfigure(bt.outAnom, command = function(){
					dirAnom <- tk_choose.dir(getwd(), "")
					tclvalue(outAnom) <- if(dirAnom%in%c("", "NA") | is.na(dirAnom)) "" else dirAnom
				})

				infobulle(en.outAnom, 'Enter the full path to directory to save outputs')
				status.bar.display(en.outAnom, TextOutputVar, 'Enter the full path to directory to save outputs')
			}

			if(tclvalue(updateAnom) == '1'){
				stateClim.Ex <- 'normal'
				stateClim <- if(tclvalue(climDataExist) == '1') 'normal' else 'disabled'
				stateBaseP <- if(tclvalue(climDataExist) == '1') 'disabled' else 'normal'
				statedayW <- if(str_trim(tclvalue(timeSteps)) == 'Daily data' & 
								tclvalue(climDataExist) == '0') "normal" else "disabled"
				stateAnomC <- 'normal'
			}else{
				stateClim.Ex <- 'disabled'
				stateClim <- 'disabled'
				stateBaseP <- 'disabled'
				statedayW <- 'disabled'
				stateAnomC <- 'disabled'
			}

			tkconfigure(chk.climIdx, state = stateClim.Ex)
			tkconfigure(en.climIdx, state = stateClim)
			tkconfigure(bt.climIdx, state = stateClim)

			tkconfigure(en.sYear, state = stateBaseP)
			tkconfigure(en.eYear, state = stateBaseP)
			tkconfigure(en.minYear, state = stateBaseP)

			tkconfigure(en.daywin, state = statedayW)

			tkconfigure(cb.anomaly, state = stateAnomC)
		})

		############################################

		tkgrid(frameTimeS, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

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

		frametDate <- ttklabelframe(subfr2, text = "Anomalies Date Range", relief = 'groove')

		istart.yrs <- tclVar(GeneralParameters$Dates$start.year)
		istart.mon <- tclVar(GeneralParameters$Dates$start.mon)
		istart.day <- tclVar(GeneralParameters$Dates$start.dek)
		iend.yrs <- tclVar(GeneralParameters$Dates$end.year)
		iend.mon <- tclVar(GeneralParameters$Dates$end.mon)
		iend.day <- tclVar(GeneralParameters$Dates$end.dek)

		txtdek <- switch(GeneralParameters$intstep, 'dekadal' = 'Dek', 'pentad' = 'Pen', 'Day')
		day.txtVar <- tclVar(txtdek)
		statedate <- if(GeneralParameters$intstep == 'monthly') 'disabled' else 'normal'

		txt.deb <- tklabel(frametDate, text = 'Start date', anchor = 'e', justify = 'right')
		txt.fin <- tklabel(frametDate, text = 'End date', anchor = 'e', justify = 'right')
		txt.yrs <- tklabel(frametDate, text = 'Year')
		txt.mon <- tklabel(frametDate, text = 'Month')
		txt.day <- tklabel(frametDate, text = tclvalue(day.txtVar), textvariable = day.txtVar)
		en.yrs1 <- tkentry(frametDate, width = 4, textvariable = istart.yrs, justify = "right")
		en.mon1 <- tkentry(frametDate, width = 4, textvariable = istart.mon, justify = "right")
		en.day1 <- tkentry(frametDate, width = 4, textvariable = istart.day, justify = "right", state = statedate)
		en.yrs2 <- tkentry(frametDate, width = 4, textvariable = iend.yrs, justify = "right")
		en.mon2 <- tkentry(frametDate, width = 4, textvariable = iend.mon, justify = "right")
		en.day2 <- tkentry(frametDate, width = 4, textvariable = iend.day, justify = "right", state = statedate)

		tkgrid(txt.deb, row = 1, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.fin, row = 2, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.yrs, row = 0, column = 1, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.mon, row = 0, column = 2, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.day, row = 0, column = 3, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.yrs1, row = 1, column = 1, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.mon1, row = 1, column = 2, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.day1, row = 1, column = 3, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.yrs2, row = 2, column = 1, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.mon2, row = 2, column = 2, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.day2, row = 2, column = 3, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(frametDate, 'Start and end date to calculate the anomalies')
		status.bar.display(frametDate, TextOutputVar, 'Start and end date to calculate the anomalies')

		#############################

		frameBaseP <- ttklabelframe(subfr2, text = "Climatology", relief = 'groove')

		climDataExist <- tclVar(GeneralParameters$climato$clim.exist)
		file.ClimIndex <- tclVar(GeneralParameters$climato$clim.file)
		startYear <- tclVar(GeneralParameters$climato$start)
		endYear <- tclVar(GeneralParameters$climato$end)
		minYear <- tclVar(GeneralParameters$climato$minyear)
		dayWin <- tclVar(GeneralParameters$climato$window)

		if(!GeneralParameters$outdir$update){
			stateClim.Ex <- 'normal'
			stateClim <- if(GeneralParameters$climato$clim.exist) 'normal' else 'disabled'
			stateBaseP <- if(GeneralParameters$climato$clim.exist) 'disabled' else 'normal'
			statedayW <- if(GeneralParameters$intstep == "daily" & 
							!GeneralParameters$climato$clim.exist) "normal" else "disabled"
		}else{
			stateClim.Ex <- 'disabled'
			stateClim <- 'disabled'
			stateBaseP <- 'disabled'
			statedayW <- 'disabled'
		}

		chk.climIdx <- tkcheckbutton(frameBaseP, variable = climDataExist, text = "Climatologies data already computed", anchor = 'w', justify = 'left', state = stateClim.Ex)
		en.climIdx <- tkentry(frameBaseP, textvariable = file.ClimIndex, width = largeur2, state = stateClim)
		bt.climIdx <- tkbutton(frameBaseP, text = "...", state = stateClim)

		# txt.BaseP <- tklabel(frameBaseP, text = "Base Period",  anchor = 'w', justify = 'left')
		txt.sYear <- tklabel(frameBaseP, text = "Base Period: Start",  anchor = 'e', justify = 'right')
		txt.eYear <- tklabel(frameBaseP, text = "End",  anchor = 'e', justify = 'right')
		en.sYear <- tkentry(frameBaseP, textvariable = startYear, width = 5, state = stateBaseP)
		en.eYear <- tkentry(frameBaseP, textvariable = endYear, width = 5, state = stateBaseP)

		txt.minYear <- tklabel(frameBaseP, text = "Minimum number of years",  anchor = 'e', justify = 'right')
		en.minYear <- tkentry(frameBaseP, textvariable = minYear, width = 3, state = stateBaseP)

		txt.daywin1 <- tklabel(frameBaseP, text = "Centered time window",  anchor = 'e', justify = 'right')
		en.daywin <- tkentry(frameBaseP, textvariable = dayWin, width = 3, state = statedayW)
		txt.daywin2 <- tklabel(frameBaseP, text = "days",  anchor = 'w', justify = 'left')

		######

		tkconfigure(bt.climIdx, command = function(){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.climIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			tclvalue(file.ClimIndex) <- if(path.climIdx%in%c("", "NA") | is.na(path.climIdx)) "" else path.climIdx
		})

		######

		tkgrid(chk.climIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.climIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.climIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		# tkgrid(txt.BaseP, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.sYear, row = 2, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.sYear, row = 2, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.eYear, row = 2, column = 2, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.eYear, row = 2, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(txt.minYear, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.minYear, row = 3, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(txt.daywin1, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.daywin, row = 4, column = 2, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.daywin2, row = 4, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)


		infobulle(en.climIdx, 'Enter the full path to the file Climatology.rds')
		status.bar.display(en.climIdx, TextOutputVar, 'Enter the full path to the file Climatology.rds')
		infobulle(bt.climIdx, 'or browse here')
		status.bar.display(bt.climIdx, TextOutputVar, 'or browse here')

		infobulle(en.sYear, 'Enter the start year of the base period')
		status.bar.display(en.sYear, TextOutputVar, 'Enter the start year of the base period')
		infobulle(en.eYear, 'Enter the end year of the base period')
		status.bar.display(en.eYear, TextOutputVar, 'Enter the end year of the base period')

		infobulle(en.minYear, 'Enter the minimum number of years with non missing values to calculate the climatology')
		status.bar.display(en.minYear, TextOutputVar, 'Enter the minimum number of years with non missing values to calculate the climatology')

		infobulle(en.daywin, 'The daily climatology is calculated using a centered (2 x window + 1) time window')
		status.bar.display(en.daywin, TextOutputVar, 'The daily climatology is calculated using a centered (2 x window + 1) time window')

		#######

		tkbind(chk.climIdx, "<Button-1>", function(){
			if(tclvalue(updateAnom) == '0'){
				stateClim <- if(tclvalue(climDataExist) == '1') 'disabled' else 'normal'
				stateBaseP <- if(tclvalue(climDataExist) == '1') 'normal' else 'disabled'
				statedayW <- if(str_trim(tclvalue(timeSteps)) == 'Daily data' &
								tclvalue(climDataExist) == '1') 'normal' else 'disabled'
			}else{
				stateClim <- 'disabled'
				stateBaseP <- 'disabled'
				statedayW <- 'disabled'
			}

			tkconfigure(en.climIdx, state = stateClim)
			tkconfigure(bt.climIdx, state = stateClim)

			tkconfigure(en.sYear, state = stateBaseP)
			tkconfigure(en.eYear, state = stateBaseP)
			tkconfigure(en.minYear, state = stateBaseP)

			tkconfigure(en.daywin, state = statedayW)
		})

		#############################

		frameAnom <- tkframe(subfr2, relief = 'groove', borderwidth = 2)

		AnomType <- c("Difference", "Percentage", "Standardized")
		anomaly <- tclVar(GeneralParameters$anomaly)

		stateAnomC <- if(!GeneralParameters$outdir$update) 'normal' else 'disabled'

		txt.anomaly <- tklabel(frameAnom, text = "Anomaly",  anchor = 'e', justify = 'right')
		cb.anomaly <- ttkcombobox(frameAnom, values = AnomType, textvariable = anomaly, width = largeur3, state = stateAnomC)

		tkgrid(txt.anomaly, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.anomaly, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.anomaly, 'Select the method to calculate the anomalies')
		status.bar.display(cb.anomaly, TextOutputVar, 'Select the method to calculate the anomalies')

		#############################

		if(!is.null(EnvAnomalyCalcPlot$DirExist)){
			stateCaclBut <- if(tclvalue(EnvAnomalyCalcPlot$DirExist) == "1") "normal" else "disabled"
		}else stateCaclBut <- "normal"

		calculateBut <- ttkbutton(subfr2, text = "Calculate", state = stateCaclBut)

		#################

		tkconfigure(calculateBut, command = function(){
			GeneralParameters$intstep <- switch(str_trim(tclvalue(timeSteps)), 
			 									'Daily data' = 'daily',
			 									'Pentad data' = 'pentad',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')

			GeneralParameters$data.type <- switch(str_trim(tclvalue(DataType)),
												'CDT stations data format' = 'cdtstation',
												'CDT dataset format (gridded)' = 'cdtdataset',
												'NetCDF gridded data' = 'cdtnetcdf')

			if(str_trim(tclvalue(DataType)) == 'CDT stations data format')
				GeneralParameters$cdtstation$file <- str_trim(tclvalue(input.file))
			if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)')
				GeneralParameters$cdtdataset$index <- str_trim(tclvalue(input.file))
			if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data')
				GeneralParameters$cdtnetcdf$dir <- str_trim(tclvalue(input.file))

			GeneralParameters$Dates$start.year <- as.numeric(str_trim(tclvalue(istart.yrs)))
			GeneralParameters$Dates$start.mon <- as.numeric(str_trim(tclvalue(istart.mon)))
			GeneralParameters$Dates$start.dek <- as.numeric(str_trim(tclvalue(istart.day)))
			GeneralParameters$Dates$end.year <- as.numeric(str_trim(tclvalue(iend.yrs)))
			GeneralParameters$Dates$end.mon <- as.numeric(str_trim(tclvalue(iend.mon)))
			GeneralParameters$Dates$end.dek <- as.numeric(str_trim(tclvalue(iend.day)))

			GeneralParameters$outdir$update <- switch(tclvalue(updateAnom), '0' = FALSE, '1' = TRUE)
			GeneralParameters$outdir$dir <- str_trim(tclvalue(outAnom))

			GeneralParameters$climato$clim.exist <- switch(tclvalue(climDataExist), '0' = FALSE, '1' = TRUE)
			GeneralParameters$climato$clim.file <- str_trim(tclvalue(file.ClimIndex))
			GeneralParameters$climato$start <- as.numeric(str_trim(tclvalue(startYear)))
			GeneralParameters$climato$end <- as.numeric(str_trim(tclvalue(endYear)))
			GeneralParameters$climato$end <- as.numeric(str_trim(tclvalue(endYear)))
			GeneralParameters$climato$minyear <- as.numeric(str_trim(tclvalue(minYear)))
			GeneralParameters$climato$window <- as.numeric(str_trim(tclvalue(dayWin)))

			GeneralParameters$anomaly <- str_trim(tclvalue(anomaly))

			if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data' & is.null(settingINData)){
				InsertMessagesTxt(main.txt.out, "You have to set the netcdf files parameters", format = TRUE)
				return(NULL)
			}

			# assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)

			tkconfigure(main.win, cursor = 'watch')
			InsertMessagesTxt(main.txt.out, "Calculate anomaly ......")

			ret <- tryCatch(
				anomaliesCalcProcs(GeneralParameters),
				# warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = tkconfigure(main.win, cursor = '')
			)

			msg0 <- "Anomaly calculation finished successfully"
			msg1 <- "Anomaly calculation failed"

			if(!is.null(ret)){
				if(ret == 0){
					InsertMessagesTxt(main.txt.out, msg0)

					###################

					set.anomaly.dates()
					widgets.Station.Pixel()
					res <- try(EnvAnomalyCalcPlot$read.Anomaly.Map(), silent = TRUE)
					if(inherits(res, "try-error") | is.null(res)) return(NULL)
				}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
			}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
		})

		############################################

		tkgrid(frametDate, row = 0, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameBaseP, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameAnom, row = 2, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(calculateBut, row = 3, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

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

		frameAnomalyDat <- ttklabelframe(subfr3, text = "Anomalies data", relief = 'groove')

		EnvAnomalyCalcPlot$DirExist <- tclVar(0)
		file.AnomIndex <- tclVar()

		stateAnomDat <- if(tclvalue(EnvAnomalyCalcPlot$DirExist) == "1") "normal" else "disabled"

		chk.anomIdx <- tkcheckbutton(frameAnomalyDat, variable = EnvAnomalyCalcPlot$DirExist, text = "Anomalies data already computed", anchor = 'w', justify = 'left')
		en.anomIdx <- tkentry(frameAnomalyDat, textvariable = file.AnomIndex, width = largeur2, state = stateAnomDat)
		bt.anomIdx <- tkbutton(frameAnomalyDat, text = "...", state = stateAnomDat)

		tkconfigure(bt.anomIdx, command = function(){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.Anom <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			if(path.Anom%in%c("", "NA") | is.na(path.Anom)) return(NULL)
			tclvalue(file.AnomIndex) <- path.Anom

			if(file.exists(str_trim(tclvalue(file.AnomIndex)))){
				OutAnomdata <- try(readRDS(str_trim(tclvalue(file.AnomIndex))), silent = TRUE)
				if(inherits(OutAnomdata, "try-error")){
					InsertMessagesTxt(main.txt.out, 'Unable to load anomalies data', format = TRUE)
					InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', OutAnomdata[1]), format = TRUE)
					tkconfigure(cb.anom.Date, values = "")
					tclvalue(EnvAnomalyCalcPlot$anomDate) <- ""
					return(NULL)
				}

				EnvAnomalyCalcPlot$output <- OutAnomdata
				EnvAnomalyCalcPlot$PathAnom <- dirname(str_trim(tclvalue(file.AnomIndex)))

				###################
				set.anomaly.dates()
				widgets.Station.Pixel()
				ret <- try(EnvAnomalyCalcPlot$read.Anomaly.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		tkgrid(chk.anomIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.anomIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.anomIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(chk.anomIdx, "<Button-1>", function(){
			stateAnomDat <- if(tclvalue(EnvAnomalyCalcPlot$DirExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.anomIdx, state = stateAnomDat)
			tkconfigure(bt.anomIdx, state = stateAnomDat)
			stateCaclBut <- if(tclvalue(EnvAnomalyCalcPlot$DirExist) == '1') 'normal' else 'disabled'
			tkconfigure(calculateBut, state = stateCaclBut)
		})
		##############################################

		frameAnomalyMap <- ttklabelframe(subfr3, text = "Anomaly Map", relief = 'groove')

		EnvAnomalyCalcPlot$anomDate <- tclVar()

		cb.anom.Date <- ttkcombobox(frameAnomalyMap, values = "", textvariable = EnvAnomalyCalcPlot$anomDate, width = largeur4)
		bt.anom.Date.prev <- ttkbutton(frameAnomalyMap, text = "<<", width = 3)
		bt.anom.Date.next <- ttkbutton(frameAnomalyMap, text = ">>", width = 3)
		bt.anom.maps <- ttkbutton(frameAnomalyMap, text = "PLOT", width = 7)
		bt.anom.MapOpt <- ttkbutton(frameAnomalyMap, text = "Options", width = 7)

		###############

		EnvAnomalyCalcPlot$anomMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
												userCol = list(custom = FALSE, color = NULL),
												userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
												title = list(user = FALSE, title = ''),
												colkeyLab = list(user = FALSE, label = ''),
												scalebar = list(add = FALSE, pos = 'bottomleft'))

		tkconfigure(bt.anom.MapOpt, command = function(){
			if(!is.null(EnvAnomalyCalcPlot$anomdata$map)){
				atlevel <- pretty(EnvAnomalyCalcPlot$anomdata$map$z, n = 10, min.n = 7)
				if(is.null(EnvAnomalyCalcPlot$anomMapOp$userLvl$levels)){
					EnvAnomalyCalcPlot$anomMapOp$userLvl$levels <- atlevel
				}else{
					if(!EnvAnomalyCalcPlot$anomMapOp$userLvl$custom)
						EnvAnomalyCalcPlot$anomMapOp$userLvl$levels <- atlevel
				}
			}
			EnvAnomalyCalcPlot$anomMapOp <- MapGraph.MapOptions(main.win, EnvAnomalyCalcPlot$anomMapOp)
		})

		#########
		EnvAnomalyCalcPlot$notebookTab.AnomMap <- NULL

		tkconfigure(bt.anom.maps, command = function(){
			if(str_trim(tclvalue(EnvAnomalyCalcPlot$anomDate)) != "" &
				!is.null(EnvAnomalyCalcPlot$anomdata))
			{
				imgContainer <- anomaliesCalc.Display.Maps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvAnomalyCalcPlot$notebookTab.AnomMap, AllOpenTabType, AllOpenTabData)
				EnvAnomalyCalcPlot$notebookTab.AnomMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.anom.Date.prev, command = function(){
			if(str_trim(tclvalue(EnvAnomalyCalcPlot$anomDate)) != ""){
				if(EnvAnomalyCalcPlot$output$params$data.type == "cdtstation")
					anomDates <- EnvAnomalyCalcPlot$output$data$dates
				else anomDates <- EnvAnomalyCalcPlot$output$dates

				idaty <- which(anomDates == str_trim(tclvalue(EnvAnomalyCalcPlot$anomDate)))
				idaty <- idaty-1
				if(idaty < 1) idaty <- length(anomDates)
				tclvalue(EnvAnomalyCalcPlot$anomDate) <- anomDates[idaty]

				ret <- try(EnvAnomalyCalcPlot$read.Anomaly.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				imgContainer <- anomaliesCalc.Display.Maps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvAnomalyCalcPlot$notebookTab.AnomMap, AllOpenTabType, AllOpenTabData)
				EnvAnomalyCalcPlot$notebookTab.AnomMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.anom.Date.next, command = function(){
			if(str_trim(tclvalue(EnvAnomalyCalcPlot$anomDate)) != ""){
				if(EnvAnomalyCalcPlot$output$params$data.type == "cdtstation")
					anomDates <- EnvAnomalyCalcPlot$output$data$dates
				else anomDates <- EnvAnomalyCalcPlot$output$dates

				idaty <- which(anomDates == str_trim(tclvalue(EnvAnomalyCalcPlot$anomDate)))
				idaty <- idaty+1
				if(idaty > length(anomDates)) idaty <- 1
				tclvalue(EnvAnomalyCalcPlot$anomDate) <- anomDates[idaty]

				ret <- try(EnvAnomalyCalcPlot$read.Anomaly.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				imgContainer <- anomaliesCalc.Display.Maps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvAnomalyCalcPlot$notebookTab.AnomMap, AllOpenTabType, AllOpenTabData)
				EnvAnomalyCalcPlot$notebookTab.AnomMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		###############

		tkgrid(bt.anom.Date.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.anom.Date, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.anom.Date.next, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.anom.maps, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.anom.MapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		###############

		tkbind(cb.anom.Date, "<<ComboboxSelected>>", function(){
			if(!is.null(EnvAnomalyCalcPlot$anomdata)){
				ret <- try(EnvAnomalyCalcPlot$read.Anomaly.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		##############################################

		tkgrid(frameAnomalyDat, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameAnomalyMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

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

		frameAnomalyTS <- ttklabelframe(subfr4, text = "Anomaly Graph", relief = 'groove')

		typeTSPLOT <- c("Bar", "Line")
		EnvAnomalyCalcPlot$graph$typeTSp <- tclVar("Bar")

		cb.typeTSp <- ttkcombobox(frameAnomalyTS, values = typeTSPLOT, textvariable = EnvAnomalyCalcPlot$graph$typeTSp, width = largeur4)
		bt.TsGraph.plot <- ttkbutton(frameAnomalyTS, text = "PLOT", width = 7)
		bt.TSGraphOpt <- ttkbutton(frameAnomalyTS, text = "Options", width = 8)

		#################

		EnvAnomalyCalcPlot$TSGraphOp <- list(
					anomaly = list(
							anom = NULL,
							xlim = list(is.min = FALSE, min = "1981-1-1", is.max = FALSE, max = "2017-12-31"),
							ylim = list(is.min = FALSE, min = -100, is.max = FALSE, max = 100),
							axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
							title = list(is.title = FALSE, title = '', position = 'top'),
							colors = list(negative = "blue", positive = "red")
						),
					line = list(
						xlim = list(is.min = FALSE, min = "1981-1-1", is.max = FALSE, max = "2017-12-31"),
						ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
						axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
						title = list(is.title = FALSE, title = '', position = 'top'),
						plot = list(type = 'both',
							col = list(line = "red", points = "blue"),
							lwd = 2, cex = 1.4),
						legend = NULL)
					)

		tkconfigure(bt.TSGraphOpt, command = function(){
			suffix.fun <- switch(str_trim(tclvalue(EnvAnomalyCalcPlot$graph$typeTSp)),
									"Bar" = "Anomaly",
									"Line" = "Line")
			plot.fun <- match.fun(paste0("MapGraph.GraphOptions.", suffix.fun))
			EnvAnomalyCalcPlot$TSGraphOp <- plot.fun(main.win, EnvAnomalyCalcPlot$TSGraphOp)
		})

		#########
		EnvAnomalyCalcPlot$notebookTab.AnomGraph <- NULL

		tkconfigure(bt.TsGraph.plot, command = function(){
			if(!is.null(EnvAnomalyCalcPlot$anomdata)){
				imgContainer <- anomaliesCalc.Display.Graph(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvAnomalyCalcPlot$notebookTab.AnomGraph, AllOpenTabType, AllOpenTabData)
				EnvAnomalyCalcPlot$notebookTab.AnomGraph <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		#################

		tkgrid(cb.typeTSp, row = 0, column = 0, sticky = 'we', pady = 1, columnspan = 1)
		tkgrid(bt.TSGraphOpt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
		tkgrid(bt.TsGraph.plot, row = 0, column = 2, sticky = 'we', pady = 1, columnspan = 1)

		##############################################

		frameSTNCrds <- ttklabelframe(subfr4, text = "Station/Coordinates", relief = 'groove')

		frTS2 <- tkframe(frameSTNCrds)
		EnvAnomalyCalcPlot$graph$lonLOC <- tclVar()
		EnvAnomalyCalcPlot$graph$latLOC <- tclVar()
		EnvAnomalyCalcPlot$graph$stnIDTSp <- tclVar()

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

		##############################################

		tkgrid(frameAnomalyTS, row = 0, column = 0, sticky = 'we', pady = 1)
		tkgrid(frameSTNCrds, row = 1, column = 0, sticky = '', pady = 3)

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

		EnvAnomalyCalcPlot$shp$add.shp <- tclVar(FALSE)
		file.plotShp <- tclVar()
		stateSHP <- "disabled"

		chk.addshp <- tkcheckbutton(frameSHP, variable = EnvAnomalyCalcPlot$shp$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
		bt.addshpOpt <- ttkbutton(frameSHP, text = "Options", state = stateSHP)
		cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur1, state = stateSHP)
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

				tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = file.plotShp)

				shpofile <- getShpOpenData(file.plotShp)
				if(is.null(shpofile)) EnvAnomalyCalcPlot$shp$ocrds <- NULL
				EnvAnomalyCalcPlot$shp$ocrds <- getBoundaries(shpofile[[2]])
			}else return(NULL)
		})

		########
		EnvAnomalyCalcPlot$SHPOp <- list(col = "black", lwd = 1.5)

		tkconfigure(bt.addshpOpt, command = function(){
			EnvAnomalyCalcPlot$SHPOp <- MapGraph.GraphOptions.LineSHP(main.win, EnvAnomalyCalcPlot$SHPOp)
		})

		########
		tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
		tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		#################
		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile)) EnvAnomalyCalcPlot$shp$ocrds <- NULL
			EnvAnomalyCalcPlot$shp$ocrds <- getBoundaries(shpofile[[2]])
		})

		tkbind(chk.addshp, "<Button-1>", function(){
			stateSHP <- if(tclvalue(EnvAnomalyCalcPlot$shp$add.shp) == "1") "disabled" else "normal"
			tkconfigure(cb.addshp, state = stateSHP)
			tkconfigure(bt.addshp, state = stateSHP)
			tkconfigure(bt.addshpOpt, state = stateSHP)
		})

		##############################################

		tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', pady = 1)

	#######################################################################################################

	widgets.Station.Pixel <- function(){
		tkdestroy(frTS2)
		frTS2 <<- tkframe(frameSTNCrds)

		if(EnvAnomalyCalcPlot$output$params$data.type == "cdtstation"){
			stnIDTSPLOT <- EnvAnomalyCalcPlot$output$data$id
			txt.stnSel <- tklabel(frTS2, text = "Select station to plot")
			bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = 6)
			bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = 6)
			cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = EnvAnomalyCalcPlot$graph$stnIDTSp, width = largeur4)
			tclvalue(EnvAnomalyCalcPlot$graph$stnIDTSp) <- stnIDTSPLOT[1]

			tkconfigure(bt.stnID.prev, command = function(){
				if(!is.null(EnvAnomalyCalcPlot$anomdata)){
					istn <- which(stnIDTSPLOT == str_trim(tclvalue(EnvAnomalyCalcPlot$graph$stnIDTSp)))
					istn <- istn-1
					if(istn < 1) istn <- length(stnIDTSPLOT)
					tclvalue(EnvAnomalyCalcPlot$graph$stnIDTSp) <- stnIDTSPLOT[istn]

					imgContainer <- anomaliesCalc.Display.Graph(tknotes)
					retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvAnomalyCalcPlot$notebookTab.AnomGraph, AllOpenTabType, AllOpenTabData)
					EnvAnomalyCalcPlot$notebookTab.AnomGraph <- retNBTab$notebookTab
					AllOpenTabType <<- retNBTab$AllOpenTabType
					AllOpenTabData <<- retNBTab$AllOpenTabData
				}
			})

			tkconfigure(bt.stnID.next, command = function(){
				if(!is.null(EnvAnomalyCalcPlot$anomdata)){
					istn <- which(stnIDTSPLOT == str_trim(tclvalue(EnvAnomalyCalcPlot$graph$stnIDTSp)))
					istn <- istn+1
					if(istn > length(stnIDTSPLOT)) istn <- 1
					tclvalue(EnvAnomalyCalcPlot$graph$stnIDTSp) <- stnIDTSPLOT[istn]

					imgContainer <- anomaliesCalc.Display.Graph(tknotes)
					retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvAnomalyCalcPlot$notebookTab.AnomGraph, AllOpenTabType, AllOpenTabData)
					EnvAnomalyCalcPlot$notebookTab.AnomGraph <- retNBTab$notebookTab
					AllOpenTabType <<- retNBTab$AllOpenTabType
					AllOpenTabData <<- retNBTab$AllOpenTabData
				}
			})

			tkgrid(txt.stnSel, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}else{
			txt.crdSel <- tklabel(frTS2, text = "Enter longitude and latitude to plot", anchor = 'w', justify = 'left')
			txt.lonLoc <- tklabel(frTS2, text = "Longitude", anchor = 'e', justify = 'right')
			en.lonLoc <- tkentry(frTS2, textvariable = EnvAnomalyCalcPlot$graph$lonLOC, width = 8)
			txt.latLoc <- tklabel(frTS2, text = "Latitude", anchor = 'e', justify = 'right')
			en.latLoc <- tkentry(frTS2, textvariable = EnvAnomalyCalcPlot$graph$latLOC, width = 8)
			stnIDTSPLOT <- ""
			tclvalue(EnvAnomalyCalcPlot$graph$stnIDTSp) <- ""

			tkgrid(txt.crdSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.lonLoc, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.lonLoc, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.latLoc, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.latLoc, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)
		return(0)
	}

	set.anomaly.dates <- function(){
		if(EnvAnomalyCalcPlot$output$params$data.type == "cdtstation")
			anomDates <- EnvAnomalyCalcPlot$output$data$dates
		else anomDates <- EnvAnomalyCalcPlot$output$dates
		tkconfigure(cb.anom.Date, values = anomDates)
		tclvalue(EnvAnomalyCalcPlot$anomDate) <- anomDates[1]
		return(0)
	}

	#######################################################################################################


	EnvAnomalyCalcPlot$read.Anomaly.Map <- function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		if(EnvAnomalyCalcPlot$output$params$data.type == "cdtstation"){
			fileAnomdata <- file.path(EnvAnomalyCalcPlot$PathAnom, "CDTANOM/CDTANOM.rds")
			if(!file.exists(fileAnomdata)){
				InsertMessagesTxt(main.txt.out, paste(fileAnomdata, 'not found'), format = TRUE)
				return(NULL)
			}

			readAnomData <- TRUE
			if(!is.null(EnvAnomalyCalcPlot$anomdata))
				if(!is.null(EnvAnomalyCalcPlot$fileAnomdata))
					if(EnvAnomalyCalcPlot$fileAnomdata == fileAnomdata) readAnomData <- FALSE

			if(readAnomData){
				EnvAnomalyCalcPlot$anomdata$data <- readRDS(fileAnomdata)
				EnvAnomalyCalcPlot$fileAnomdata <- fileAnomdata
			}

			########
			rasterAnomData <- TRUE
			if(!rasterAnomData)
				if(!is.null(EnvAnomalyCalcPlot$anomdata$rasterDate))
					if(EnvAnomalyCalcPlot$fileAnomdata == fileAnomdata)
						if(EnvAnomalyCalcPlot$anomdata$rasterDate == str_trim(tclvalue(EnvAnomalyCalcPlot$anomDate))) rasterAnomData <- FALSE

			if(rasterAnomData){
				idt <- which(EnvAnomalyCalcPlot$output$data$dates == as.numeric(str_trim(tclvalue(EnvAnomalyCalcPlot$anomDate))))
				nx <- nx_ny_as.image(diff(range(EnvAnomalyCalcPlot$output$data$lon)))
				ny <- nx_ny_as.image(diff(range(EnvAnomalyCalcPlot$output$data$lat)))
				tmp <- cdt.as.image(as.numeric(EnvAnomalyCalcPlot$anomdata$data[idt, ]), nx = nx, ny = ny,
								pts.xy = cbind(EnvAnomalyCalcPlot$output$data$lon, EnvAnomalyCalcPlot$output$data$lat))
				EnvAnomalyCalcPlot$anomdata$map$x <- tmp$x
				EnvAnomalyCalcPlot$anomdata$map$y <- tmp$y
				EnvAnomalyCalcPlot$anomdata$map$z <- tmp$z
				EnvAnomalyCalcPlot$anomdata$rasterDate <- str_trim(tclvalue(EnvAnomalyCalcPlot$anomDate))
				rm(tmp)
			}
		}else{
			fileAnomdata <- file.path(EnvAnomalyCalcPlot$PathAnom, "DATA_NetCDF/CDTANOM",
							paste0("anomaly_", str_trim(tclvalue(EnvAnomalyCalcPlot$anomDate)), ".nc"))
			if(!file.exists(fileAnomdata)){
				InsertMessagesTxt(main.txt.out, paste(fileAnomdata, 'not found'), format = TRUE)
				return(NULL)
			}

			readAnomData <- TRUE
			if(!is.null(EnvAnomalyCalcPlot$anomdata))
				if(!is.null(EnvAnomalyCalcPlot$fileAnomdata))
					if(EnvAnomalyCalcPlot$fileAnomdata == fileAnomdata) readAnomData <- FALSE

			if(readAnomData){
				nc <- nc_open(fileAnomdata)
				EnvAnomalyCalcPlot$anomdata$map$x <- nc$dim[[1]]$vals
				EnvAnomalyCalcPlot$anomdata$map$y <- nc$dim[[2]]$vals
				EnvAnomalyCalcPlot$anomdata$map$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
				nc_close(nc)
				EnvAnomalyCalcPlot$fileAnomdata <- fileAnomdata
			}

			###################

			fileAnomIdx <- file.path(EnvAnomalyCalcPlot$PathAnom, "CDTANOM/CDTANOM.rds")

			readAnomIdx<- TRUE
			if(!is.null(EnvAnomalyCalcPlot$cdtdataset))
				if(!is.null(EnvAnomalyCalcPlot$fileAnomIdx))
					if(EnvAnomalyCalcPlot$fileAnomIdx == fileAnomIdx) readAnomIdx <- FALSE
			if(readAnomIdx){
				EnvAnomalyCalcPlot$cdtdataset <- readRDS(fileAnomIdx)
				EnvAnomalyCalcPlot$cdtdataset$fileInfo <- fileAnomIdx
				EnvAnomalyCalcPlot$fileAnomIdx <- fileAnomIdx
			}
		}

		return(0)
	}

	#######################################################################################################

	tcl('update')
	tkgrid(cmd.frame, sticky = '', pady = 1)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)

	######
	return(cmd.frame)
}

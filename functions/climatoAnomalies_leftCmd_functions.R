
anomaliesCalcPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(45)
		largeur0 <- as.integer(w.scale(22)/sfont0)
		largeur1 <- as.integer(w.scale(29)/sfont0)
		largeur2 <- as.integer(w.scale(31)/sfont0)
		largeur3 <- 20
		# largeur4 <- largeur1-5
		# largeur5 <- 30
		# largeur6 <- 22
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(47)
		largeur0 <- as.integer(w.scale(18)/sfont0)
		largeur1 <- as.integer(w.scale(22)/sfont0)
		largeur2 <- as.integer(w.scale(23)/sfont0)
		largeur3 <- 15
		# largeur4 <- largeur1
		# largeur5 <- 22
		# largeur6 <- 14
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
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Plot")

	bwRaiseTab(tknote.cmd, cmd.tab1)
	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab3, 0, weight = 1)


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

			assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)

			tkconfigure(main.win, cursor = 'watch')
			InsertMessagesTxt(main.txt.out, "Calculate anomaly ......")

			ret <- tryCatch(
				anomaliesCalcProcs(GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = tkconfigure(main.win, cursor = '')
			)

			msg0 <- "Anomaly calculation finished successfully"
			msg1 <- "Anomaly calculation failed"

			if(!is.null(ret)){
				if(ret == 0){
					InsertMessagesTxt(main.txt.out, msg0)

					###################

					# load.PICSA.Data()

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
		file.Stat <- tclVar()

		statedirStat <- if(tclvalue(EnvAnomalyCalcPlot$DirExist) == "1") "normal" else "disabled"

		chk.dirStat <- tkcheckbutton(frameAnomalyDat, variable = EnvAnomalyCalcPlot$DirExist, text = "Anomalies data already computed", anchor = 'w', justify = 'left')
		en.dirStat <- tkentry(frameAnomalyDat, textvariable = file.Stat, width = largeur2, state = statedirStat)
		bt.dirStat <- tkbutton(frameAnomalyDat, text = "...", state = statedirStat)

		tkconfigure(bt.dirStat, command = function(){

		})


		tkgrid(chk.dirStat, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.dirStat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.dirStat, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(chk.dirStat, "<Button-1>", function(){
			statedirStat <- if(tclvalue(EnvAnomalyCalcPlot$DirExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.dirStat, state = statedirStat)
			tkconfigure(bt.dirStat, state = statedirStat)
			stateCaclBut <- if(tclvalue(EnvAnomalyCalcPlot$DirExist) == '1') 'normal' else 'disabled'
			tkconfigure(calculateBut, state = stateCaclBut)
		})

		##############################################

		frameClimatoMap <- ttklabelframe(subfr3, text = "Anomalies Map", relief = 'groove')



		##############################################

		frameClimatoTS <- ttklabelframe(subfr3, text = "Anomalies Graph", relief = 'groove')



		##############################################

		tkgrid(frameAnomalyDat, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameClimatoMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameClimatoTS, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	tcl('update')
	tkgrid(cmd.frame, sticky = '', pady = 1)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)

	######
	return(cmd.frame)
}



climatologiesCalcPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(45)
		largeur0 <- as.integer(w.scale(22)/sfont0)
		largeur1 <- as.integer(w.scale(29)/sfont0)
		largeur2 <- as.integer(w.scale(31)/sfont0)
		# largeur3 <- largeur2-10
		# largeur4 <- largeur1-5
		# largeur5 <- 30
		# largeur6 <- 22
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(47)
		largeur0 <- as.integer(w.scale(18)/sfont0)
		largeur1 <- as.integer(w.scale(22)/sfont0)
		largeur2 <- as.integer(w.scale(23)/sfont0)
		# largeur3 <- largeur2+2
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
							climato = list(start = 1981, end = 2010, minyear = 20, window = 0),
							out.dir = "")

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Climatologies")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Plot")

	bwRaiseTab(tknote.cmd, cmd.tab1)
	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)

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
			statedayW <- if(str_trim(tclvalue(timeSteps)) == 'Daily data') "normal" else "disabled"
			tkconfigure(en.daywin, state = statedayW)
		})

		#######################

		frameInData <- ttklabelframe(subfr1, text = "Input Data", relief = 'groove')

		DataType <- tclVar()
		CbdatatypeVAL <- c('CDT stations data format', 'CDT dataset format (gridded)', 'NetCDF gridded data')
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
		set.infile <- tkbutton(frameInData, text = "Settings", width = 5, state = stateSetNC)

		if(GeneralParameters$data.type == 'cdtstation'){
			cb.en.infile <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)
		}else{
			cb.en.infile <- tkentry(frameInData, textvariable = input.file, width = largeur2)
		}
		bt.infile <- tkbutton(frameInData, text = "...")

		############
		settingINData <- GeneralParameters$settingINData
		tkconfigure(set.infile, command = function(){
			GeneralParameters$cdtnetcdf <<- getInfoNetcdfData(main.win, GeneralParameters$cdtnetcdf,
																str_trim(tclvalue(input.file)),
																tclvalue(timeSteps))
			settingINData <<- 1
		})

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
		tkgrid(set.infile, row = 1, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
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
			tkconfigure(set.infile, state = stateSetNC)

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

				tkconfigure(set.infile, command = function(){
					GeneralParameters$cdtnetcdf <<- getInfoNetcdfData(main.win, GeneralParameters$cdtnetcdf,
																	str_trim(tclvalue(input.file)),
																	tclvalue(timeSteps))
					settingINData <<- 1
				})

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

		#######################

		frameBaseP <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		startYear <- tclVar(GeneralParameters$climato$start)
		endYear <- tclVar(GeneralParameters$climato$end)
		minYear <- tclVar(GeneralParameters$climato$minyear)
		dayWin <- tclVar(GeneralParameters$climato$window)

		statedayW <- if(GeneralParameters$intstep == "daily") "normal" else "disabled"

		txt.BaseP <- tklabel(frameBaseP, text = "Base Period",  anchor = 'w', justify = 'left')
		txt.sYear <- tklabel(frameBaseP, text = "Start",  anchor = 'e', justify = 'right')
		txt.eYear <- tklabel(frameBaseP, text = "End",  anchor = 'e', justify = 'right')
		en.sYear <- tkentry(frameBaseP, textvariable = startYear, width = 5)
		en.eYear <- tkentry(frameBaseP, textvariable = endYear, width = 5)

		txt.minYear <- tklabel(frameBaseP, text = "Minimum number of years",  anchor = 'e', justify = 'right')
		en.minYear <- tkentry(frameBaseP, textvariable = minYear, width = 3)

		txt.daywin1 <- tklabel(frameBaseP, text = "Centered time window",  anchor = 'e', justify = 'right')
		en.daywin <- tkentry(frameBaseP, textvariable = dayWin, width = 3, state = statedayW)
		txt.daywin2 <- tklabel(frameBaseP, text = "days",  anchor = 'w', justify = 'left')

		######
		tkgrid(txt.BaseP, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.sYear, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.sYear, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.eYear, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.eYear, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(txt.minYear, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.minYear, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(txt.daywin1, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.daywin, row = 2, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.daywin2, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(en.sYear, 'Enter the start year of the base period')
		status.bar.display(en.sYear, TextOutputVar, 'Enter the start year of the base period')
		infobulle(en.eYear, 'Enter the end year of the base period')
		status.bar.display(en.eYear, TextOutputVar, 'Enter the end year of the base period')

		infobulle(en.minYear, 'Enter the minimum number of years with non missing values to calculate the climatology')
		status.bar.display(en.minYear, TextOutputVar, 'Enter the minimum number of years with non missing values to calculate the climatology')

		infobulle(en.daywin, 'The daily climatology is calculated using a centered (2 x window + 1) time window')
		status.bar.display(en.daywin, TextOutputVar, 'The daily climatology is calculated using a centered (2 x window + 1) time window')

		#############################

		frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		dir.save <- tclVar(GeneralParameters$out.dir)

		txt.dir.save <- tklabel(frameDirSav, text = "Directory to save results",  anchor = 'w', justify = 'left')
		en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur2)
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

		#############################

		if(!is.null(EnvClimatoCalcPlot$DirExist)){
			stateCaclBut <- if(tclvalue(EnvClimatoCalcPlot$DirExist) == "1") "normal" else "disabled"
		}else stateCaclBut <- "normal"

		calculateBut <- ttkbutton(subfr1, text = "Calculate", state = stateCaclBut)

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

			GeneralParameters$out.dir <- str_trim(tclvalue(dir.save))
			GeneralParameters$climato$start <- as.numeric(str_trim(tclvalue(startYear)))
			GeneralParameters$climato$end <- as.numeric(str_trim(tclvalue(endYear)))
			GeneralParameters$climato$minyear <- as.numeric(str_trim(tclvalue(minYear)))
			GeneralParameters$climato$window <- if(GeneralParameters$intstep == 'daily') as.numeric(str_trim(tclvalue(dayWin))) else 0

			if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data' & is.null(settingINData)){
				InsertMessagesTxt(main.txt.out, "You have to set the netcdf files parameters", format = TRUE)
				return(NULL)
			}

			# assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)

			tkconfigure(main.win, cursor = 'watch')
			InsertMessagesTxt(main.txt.out, "Calculate climatology ......")

			ret <- tryCatch(
				climatologiesCalcProcs(GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = tkconfigure(main.win, cursor = '')
			)

			msg0 <- "Climatology calculation finished successfully"
			msg1 <- "Climatology calculation failed"

			if(!is.null(ret)){
				if(ret == 0){
					InsertMessagesTxt(main.txt.out, msg0)

					###################

					# load.PICSA.Data()

				}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
			}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
		})

		############################################

		tkgrid(frameTimeS, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameBaseP, row = 2, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 3, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(calculateBut, row = 4, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab2
	frTab2 <- tkframe(cmd.tab2)
	tkgrid(frTab2, padx = 5, pady = 5, ipadx = 2, ipady = 2)
	tkgrid.columnconfigure(frTab2, 0, weight = 1)

	scrw2 <- bwScrolledWindow(frTab2)
	tkgrid(scrw2)
	tkgrid.columnconfigure(scrw2, 0, weight = 1)
	subfr2 <- bwScrollableFrame(scrw2, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr2, 0, weight = 1)

		##############################################

		frameClimatoDat <- ttklabelframe(subfr2, text = "Climatologies data", relief = 'groove')

		EnvClimatoCalcPlot$DirExist <- tclVar(0)
		file.ClimIndex <- tclVar()

		stateClimatoDat <- if(tclvalue(EnvClimatoCalcPlot$DirExist) == "1") "normal" else "disabled"

		chk.climIdx <- tkcheckbutton(frameClimatoDat, variable = EnvClimatoCalcPlot$DirExist, text = "Climatologies data already computed", anchor = 'w', justify = 'left')
		en.climIdx <- tkentry(frameClimatoDat, textvariable = file.ClimIndex, width = largeur2, state = stateClimatoDat)
		bt.climIdx <- tkbutton(frameClimatoDat, text = "...", state = stateClimatoDat)

		tkconfigure(bt.climIdx, command = function(){

		})


		tkgrid(chk.climIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.climIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.climIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(chk.climIdx, "<Button-1>", function(){
			stateClimatoDat <- if(tclvalue(EnvClimatoCalcPlot$DirExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.climIdx, state = stateClimatoDat)
			tkconfigure(bt.climIdx, state = stateClimatoDat)
			stateCaclBut <- if(tclvalue(EnvClimatoCalcPlot$DirExist) == '1') 'normal' else 'disabled'
			tkconfigure(calculateBut, state = stateCaclBut)
		})

		##############################################

		frameClimatoMap <- ttklabelframe(subfr2, text = "Climatologies Map", relief = 'groove')



		##############################################

		frameClimatoTS <- ttklabelframe(subfr2, text = "Climatologies Graph", relief = 'groove')



		##############################################

		tkgrid(frameClimatoDat, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameClimatoMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameClimatoTS, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################
	tcl('update')
	tkgrid(cmd.frame, sticky = '', pady = 1)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)
	######
	return(cmd.frame)
}



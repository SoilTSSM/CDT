
climatologiesCalcPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(46)
		largeur0 <- as.integer(w.scale(22)/sfont0)
		largeur1 <- as.integer(w.scale(27)/sfont0)
		largeur2 <- as.integer(w.scale(29)/sfont0)
		largeur3 <- 30
		largeur4 <- 21
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(47)
		largeur0 <- as.integer(w.scale(18)/sfont0)
		largeur1 <- as.integer(w.scale(22)/sfont0)
		largeur2 <- as.integer(w.scale(23)/sfont0)
		largeur3 <- 23
		largeur4 <- 14
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
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Maps")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Graphs")
	cmd.tab4 <- bwAddTab(tknote.cmd, text = "Boundaries")

	bwRaiseTab(tknote.cmd, cmd.tab1)
	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab3, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab4, 0, weight = 1)

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
		set.infile <- tkbutton(frameInData, text = "Settings", width = 8, state = stateSetNC)

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
		tkgrid(txt.BaseP, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(txt.sYear, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.sYear, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(txt.eYear, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.eYear, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)

		tkgrid(txt.minYear, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.minYear, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(txt.daywin1, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.daywin, row = 2, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(txt.daywin2, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)

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

					set.climato.index()
					widgets.Station.Pixel()
					res <- try(EnvClimatoCalcPlot$read.Climatology.Map(), silent = TRUE)
					if(inherits(res, "try-error") | is.null(res)) return(NULL)
				}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
			}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
		})

		############################################

		tkgrid(frameTimeS, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameBaseP, row = 2, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 3, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(calculateBut, row = 4, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)

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
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.Clim <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			if(path.Clim%in%c("", "NA") | is.na(path.Clim)) return(NULL)
			tclvalue(file.ClimIndex) <- path.Clim

			if(file.exists(str_trim(tclvalue(file.ClimIndex)))){
				OutClimdata <- try(readRDS(str_trim(tclvalue(file.ClimIndex))), silent = TRUE)
				if(inherits(OutClimdata, "try-error")){
					InsertMessagesTxt(main.txt.out, 'Unable to load climatology data', format = TRUE)
					InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', OutClimdata[1]), format = TRUE)
					tkconfigure(cb.clim.Date, values = "")
					tclvalue(EnvClimatoCalcPlot$climDate) <- ""
					return(NULL)
				}

				EnvClimatoCalcPlot$output <- OutClimdata
				EnvClimatoCalcPlot$PathClim <- dirname(str_trim(tclvalue(file.ClimIndex)))

				###################
				set.climato.index()
				widgets.Station.Pixel()
				ret <- try(EnvClimatoCalcPlot$read.Climatology.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
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

		frameClimatoMap <- ttklabelframe(subfr2, text = "Climatology Map", relief = 'groove')

		EnvClimatoCalcPlot$climVar <- tclVar("Mean")
		EnvClimatoCalcPlot$climDate <- tclVar()

		cb.clim.Var <- ttkcombobox(frameClimatoMap, values = c("Mean", "Standard deviation"), textvariable = EnvClimatoCalcPlot$climVar, width = largeur3)
		bt.clim.maps <- ttkbutton(frameClimatoMap, text = "PLOT", width = 8)
		cb.clim.Date <- ttkcombobox(frameClimatoMap, values = "", textvariable = EnvClimatoCalcPlot$climDate, width = 3)
		bt.clim.Date.prev <- ttkbutton(frameClimatoMap, text = "<<", width = 3)
		bt.clim.Date.next <- ttkbutton(frameClimatoMap, text = ">>", width = 3)
		bt.clim.MapOpt <- ttkbutton(frameClimatoMap, text = "Options", width = 8)

		###############
		EnvClimatoCalcPlot$climMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
												userCol = list(custom = FALSE, color = NULL),
												userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
												title = list(user = FALSE, title = ''),
												colkeyLab = list(user = FALSE, label = ''),
												scalebar = list(add = FALSE, pos = 'bottomleft'))

		tkconfigure(bt.clim.MapOpt, command = function(){
			if(!is.null(EnvClimatoCalcPlot$climdata$map)){
				atlevel <- pretty(EnvClimatoCalcPlot$climdata$map$z, n = 10, min.n = 7)
				if(is.null(EnvClimatoCalcPlot$climMapOp$userLvl$levels)){
					EnvClimatoCalcPlot$climMapOp$userLvl$levels <- atlevel
				}else{
					if(!EnvClimatoCalcPlot$climMapOp$userLvl$custom)
						EnvClimatoCalcPlot$climMapOp$userLvl$levels <- atlevel
				}
			}
			EnvClimatoCalcPlot$climMapOp <- MapGraph.MapOptions(main.win, EnvClimatoCalcPlot$climMapOp)
		})

		#########
		EnvClimatoCalcPlot$notebookTab.ClimMap <- NULL

		tkconfigure(bt.clim.maps, command = function(){
			if(str_trim(tclvalue(EnvClimatoCalcPlot$climVar)) != "" &
				str_trim(tclvalue(EnvClimatoCalcPlot$climDate)) != "" &
				!is.null(EnvClimatoCalcPlot$climdata))
			{
				imgContainer <- climatologiesCalc.Display.Maps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvClimatoCalcPlot$notebookTab.ClimMap, AllOpenTabType, AllOpenTabData)
				EnvClimatoCalcPlot$notebookTab.ClimMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.clim.Date.prev, command = function(){
			if(str_trim(tclvalue(EnvClimatoCalcPlot$climDate)) != ""){
				idaty <- which(EnvClimatoCalcPlot$output$index == as.numeric(str_trim(tclvalue(EnvClimatoCalcPlot$climDate))))
				idaty <- idaty-1
				if(idaty < 1) idaty <- length(EnvClimatoCalcPlot$output$index)
				tclvalue(EnvClimatoCalcPlot$climDate) <- EnvClimatoCalcPlot$output$index[idaty]

				ret <- try(EnvClimatoCalcPlot$read.Climatology.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				imgContainer <- climatologiesCalc.Display.Maps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvClimatoCalcPlot$notebookTab.ClimMap, AllOpenTabType, AllOpenTabData)
				EnvClimatoCalcPlot$notebookTab.ClimMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.clim.Date.next, command = function(){
			if(str_trim(tclvalue(EnvClimatoCalcPlot$climDate)) != ""){
				idaty <- which(EnvClimatoCalcPlot$output$index == as.numeric(str_trim(tclvalue(EnvClimatoCalcPlot$climDate))))
				idaty <- idaty+1
				if(idaty > length(EnvClimatoCalcPlot$output$index)) idaty <- 1
				tclvalue(EnvClimatoCalcPlot$climDate) <- EnvClimatoCalcPlot$output$index[idaty]

				ret <- try(EnvClimatoCalcPlot$read.Climatology.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				imgContainer <- climatologiesCalc.Display.Maps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvClimatoCalcPlot$notebookTab.ClimMap, AllOpenTabType, AllOpenTabData)
				EnvClimatoCalcPlot$notebookTab.ClimMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		###############

		tkgrid(cb.clim.Var, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.clim.maps, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.clim.Date.prev, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.clim.Date, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.clim.Date.next, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.clim.MapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		###############

		tkbind(cb.clim.Var, "<<ComboboxSelected>>", function(){
			if(!is.null(EnvClimatoCalcPlot$output)){
				ret <- try(EnvClimatoCalcPlot$read.Climatology.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		tkbind(cb.clim.Date, "<<ComboboxSelected>>", function(){
			if(!is.null(EnvClimatoCalcPlot$climdata)){
				ret <- try(EnvClimatoCalcPlot$read.Climatology.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		##############################################

		tkgrid(frameClimatoDat, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameClimatoMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

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

		frameClimatoTS <- ttklabelframe(subfr3, text = "Climatology Graph", relief = 'groove')

		typeTSPLOT <- c("Line", "Barplot")
		EnvClimatoCalcPlot$graph$typeTSp <- tclVar("Line")

		cb.typeTSp <- ttkcombobox(frameClimatoTS, values = typeTSPLOT, textvariable = EnvClimatoCalcPlot$graph$typeTSp, width = largeur4)
		bt.TsGraph.plot <- ttkbutton(frameClimatoTS, text = "PLOT", width = 7)
		bt.TSGraphOpt <- ttkbutton(frameClimatoTS, text = "Options", width = 8)

		#################

		EnvClimatoCalcPlot$TSGraphOp <- list(
							bar = list(
									xlim = list(is.min = FALSE, min = "1-1", is.max = FALSE, max = "12-3"),
									ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
									axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
									title = list(is.title = FALSE, title = '', position = 'top'),
									colors = list(col = "darkblue")
								),
							line = list(
								xlim = list(is.min = FALSE, min = "1-1", is.max = FALSE, max = "12-3"),
								ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
								axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
								title = list(is.title = FALSE, title = '', position = 'top'),
								plot = list(type = 'both',
									col = list(line = "red", points = "blue"),
									lwd = 2, cex = 1.4),
								legend = NULL)
							)

		tkconfigure(bt.TSGraphOpt, command = function(){
			suffix.fun <- switch(str_trim(tclvalue(EnvClimatoCalcPlot$graph$typeTSp)),
									"Barplot" = "Bar",
									"Line" = "Line")
			plot.fun <- match.fun(paste0("MapGraph.GraphOptions.", suffix.fun))
			EnvClimatoCalcPlot$TSGraphOp <- plot.fun(main.win, EnvClimatoCalcPlot$TSGraphOp)
		})

		#########
		EnvClimatoCalcPlot$notebookTab.ClimGraph <- NULL

		tkconfigure(bt.TsGraph.plot, command = function(){
			if(!is.null(EnvClimatoCalcPlot$climdata)){
				imgContainer <- climatologiesCalc.Display.Graph(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvClimatoCalcPlot$notebookTab.ClimGraph, AllOpenTabType, AllOpenTabData)
				EnvClimatoCalcPlot$notebookTab.ClimGraph <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		#################

		tkgrid(cb.typeTSp, row = 0, column = 0, sticky = 'we', pady = 1, columnspan = 1)
		tkgrid(bt.TSGraphOpt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
		tkgrid(bt.TsGraph.plot, row = 0, column = 2, sticky = 'we', pady = 1, columnspan = 1)

		##############################################

		frameSTNCrds <- ttklabelframe(subfr3, text = "Station/Coordinates", relief = 'groove')

		frTS2 <- tkframe(frameSTNCrds)
		EnvClimatoCalcPlot$graph$lonLOC <- tclVar()
		EnvClimatoCalcPlot$graph$latLOC <- tclVar()
		EnvClimatoCalcPlot$graph$stnIDTSp <- tclVar()

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

		##############################################

		tkgrid(frameClimatoTS, row = 0, column = 0, sticky = 'we', pady = 1)
		tkgrid(frameSTNCrds, row = 1, column = 0, sticky = '', pady = 3)

	#######################################################################################################

	#Tab4
	frTab4 <- tkframe(cmd.tab4)
	tkgrid(frTab4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab4, 0, weight = 1)

	scrw4 <- bwScrolledWindow(frTab4)
	tkgrid(scrw4)
	tkgrid.columnconfigure(scrw4, 0, weight = 1)
	subfr4 <- bwScrollableFrame(scrw4, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr3, 0, weight = 1)

		##############################################

		frameSHP <- ttklabelframe(subfr4, text = "Boundaries", relief = 'groove')

		EnvClimatoCalcPlot$shp$add.shp <- tclVar(FALSE)
		file.plotShp <- tclVar()
		stateSHP <- "disabled"

		chk.addshp <- tkcheckbutton(frameSHP, variable = EnvClimatoCalcPlot$shp$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
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
				if(is.null(shpofile)) EnvClimatoCalcPlot$shp$ocrds <- NULL
				EnvClimatoCalcPlot$shp$ocrds <- getBoundaries(shpofile[[2]])
			}else return(NULL)
		})

		########
		EnvClimatoCalcPlot$SHPOp <- list(col = "black", lwd = 1.5)

		tkconfigure(bt.addshpOpt, command = function(){
			EnvClimatoCalcPlot$SHPOp <- MapGraph.GraphOptions.LineSHP(main.win, EnvClimatoCalcPlot$SHPOp)
		})

		########
		tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
		tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		#################
		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile)) EnvClimatoCalcPlot$shp$ocrds <- NULL
			EnvClimatoCalcPlot$shp$ocrds <- getBoundaries(shpofile[[2]])
		})

		tkbind(chk.addshp, "<Button-1>", function(){
			stateSHP <- if(tclvalue(EnvClimatoCalcPlot$shp$add.shp) == "1") "disabled" else "normal"
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

		if(EnvClimatoCalcPlot$output$params$data.type == "cdtstation"){
			stnIDTSPLOT <- EnvClimatoCalcPlot$output$data$id
			txt.stnSel <- tklabel(frTS2, text = "Select station to plot")
			bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = 6)
			bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = 6)
			cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = EnvClimatoCalcPlot$graph$stnIDTSp, width = largeur4)
			tclvalue(EnvClimatoCalcPlot$graph$stnIDTSp) <- stnIDTSPLOT[1]

			tkconfigure(bt.stnID.prev, command = function(){
				if(!is.null(EnvClimatoCalcPlot$climdata)){
					istn <- which(stnIDTSPLOT == str_trim(tclvalue(EnvClimatoCalcPlot$graph$stnIDTSp)))
					istn <- istn-1
					if(istn < 1) istn <- length(stnIDTSPLOT)
					tclvalue(EnvClimatoCalcPlot$graph$stnIDTSp) <- stnIDTSPLOT[istn]

					imgContainer <- climatologiesCalc.Display.Graph(tknotes)
					retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvClimatoCalcPlot$notebookTab.ClimGraph, AllOpenTabType, AllOpenTabData)
					EnvClimatoCalcPlot$notebookTab.ClimGraph <- retNBTab$notebookTab
					AllOpenTabType <<- retNBTab$AllOpenTabType
					AllOpenTabData <<- retNBTab$AllOpenTabData
				}
			})

			tkconfigure(bt.stnID.next, command = function(){
				if(!is.null(EnvClimatoCalcPlot$climdata)){
					istn <- which(stnIDTSPLOT == str_trim(tclvalue(EnvClimatoCalcPlot$graph$stnIDTSp)))
					istn <- istn+1
					if(istn > length(stnIDTSPLOT)) istn <- 1
					tclvalue(EnvClimatoCalcPlot$graph$stnIDTSp) <- stnIDTSPLOT[istn]

					imgContainer <- climatologiesCalc.Display.Graph(tknotes)
					retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvClimatoCalcPlot$notebookTab.ClimGraph, AllOpenTabType, AllOpenTabData)
					EnvClimatoCalcPlot$notebookTab.ClimGraph <- retNBTab$notebookTab
					AllOpenTabType <<- retNBTab$AllOpenTabType
					AllOpenTabData <<- retNBTab$AllOpenTabData
				}
			})

			tkgrid(txt.stnSel, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}else{
			txt.crdSel <- tklabel(frTS2, text = "Enter longitude and latitude to plot", anchor = 'w', justify = 'left')
			txt.lonLoc <- tklabel(frTS2, text = "Longitude", anchor = 'e', justify = 'right')
			en.lonLoc <- tkentry(frTS2, textvariable = EnvClimatoCalcPlot$graph$lonLOC, width = 8)
			txt.latLoc <- tklabel(frTS2, text = "Latitude", anchor = 'e', justify = 'right')
			en.latLoc <- tkentry(frTS2, textvariable = EnvClimatoCalcPlot$graph$latLOC, width = 8)
			stnIDTSPLOT <- ""
			tclvalue(EnvClimatoCalcPlot$graph$stnIDTSp) <- ""

			tkgrid(txt.crdSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.lonLoc, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.lonLoc, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.latLoc, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.latLoc, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)
		return(0)
	}

	set.climato.index <- function(){
		tkconfigure(cb.clim.Date, values = EnvClimatoCalcPlot$output$index)
		tclvalue(EnvClimatoCalcPlot$climDate) <- EnvClimatoCalcPlot$output$index[1]
		return(0)
	}

	#######################################################################################################

	EnvClimatoCalcPlot$read.Climatology.Map <- function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		cilmdata.Var <- if(str_trim(tclvalue(EnvClimatoCalcPlot$climVar)) == "Mean") "CDTMEAN" else "CDTSTD"

		if(EnvClimatoCalcPlot$output$params$data.type == "cdtstation"){
			fileClimdata <- file.path(EnvClimatoCalcPlot$PathClim, cilmdata.Var, paste0(cilmdata.Var, ".rds"))
			if(!file.exists(fileClimdata)){
				InsertMessagesTxt(main.txt.out, paste(fileClimdata, 'not found'), format = TRUE)
				return(NULL)
			}

			readClimData <- TRUE
			if(!is.null(EnvClimatoCalcPlot$climdata))
				if(!is.null(EnvClimatoCalcPlot$fileClimdata))
					if(EnvClimatoCalcPlot$fileClimdata == fileClimdata) readClimData <- FALSE

			if(readClimData){
				EnvClimatoCalcPlot$climdata$data <- readRDS(fileClimdata)
				EnvClimatoCalcPlot$fileClimdata <- fileClimdata
			}

			########
			rasterClimData <- TRUE
			if(!rasterClimData)
				if(!is.null(EnvClimatoCalcPlot$climdata$rasterIdx))
					if(EnvClimatoCalcPlot$fileClimdata == fileClimdata)
						if(EnvClimatoCalcPlot$climdata$rasterIdx == str_trim(tclvalue(EnvClimatoCalcPlot$climDate))) rasterClimData <- FALSE

			if(rasterClimData){
				idt <- which(EnvClimatoCalcPlot$output$index == as.numeric(str_trim(tclvalue(EnvClimatoCalcPlot$climDate))))
				nx <- nx_ny_as.image(diff(range(EnvClimatoCalcPlot$output$data$lon)))
				ny <- nx_ny_as.image(diff(range(EnvClimatoCalcPlot$output$data$lat)))
				tmp <- cdt.as.image(as.numeric(EnvClimatoCalcPlot$climdata$data[idt, ]), nx = nx, ny = ny,
								pts.xy = cbind(EnvClimatoCalcPlot$output$data$lon, EnvClimatoCalcPlot$output$data$lat))
				EnvClimatoCalcPlot$climdata$map$x <- tmp$x
				EnvClimatoCalcPlot$climdata$map$y <- tmp$y
				EnvClimatoCalcPlot$climdata$map$z <- tmp$z
				EnvClimatoCalcPlot$climdata$rasterIdx <- str_trim(tclvalue(EnvClimatoCalcPlot$climDate))
				EnvClimatoCalcPlot$climdata$Var <- cilmdata.Var
				rm(tmp)
			}
		}else{
			fileClimdata <- file.path(EnvClimatoCalcPlot$PathClim, "DATA_NetCDF", cilmdata.Var,
							paste0("clim_", as.numeric(str_trim(tclvalue(EnvClimatoCalcPlot$climDate))), ".nc"))
			if(!file.exists(fileClimdata)){
				InsertMessagesTxt(main.txt.out, paste(fileClimdata, 'not found'), format = TRUE)
				return(NULL)
			}

			readClimData <- TRUE
			if(!is.null(EnvClimatoCalcPlot$climdata))
				if(!is.null(EnvClimatoCalcPlot$fileClimdata))
					if(EnvClimatoCalcPlot$fileClimdata == fileClimdata) readClimData <- FALSE

			if(readClimData){
				nc <- nc_open(fileClimdata)
				EnvClimatoCalcPlot$climdata$map$x <- nc$dim[[1]]$vals
				EnvClimatoCalcPlot$climdata$map$y <- nc$dim[[2]]$vals
				EnvClimatoCalcPlot$climdata$map$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
				nc_close(nc)
				EnvClimatoCalcPlot$fileClimdata <- fileClimdata
				EnvClimatoCalcPlot$climdata$Var <- cilmdata.Var
			}

			###################

			fileClimIdx <- file.path(EnvClimatoCalcPlot$PathClim, cilmdata.Var, paste0(cilmdata.Var, ".rds"))

			readClimIdx <- TRUE
			if(!is.null(EnvClimatoCalcPlot$cdtdataset))
				if(!is.null(EnvClimatoCalcPlot$fileClimIdx))
					if(EnvClimatoCalcPlot$fileClimIdx == fileClimIdx) rreadClimIdx <- FALSE
			if(readClimIdx){
				EnvClimatoCalcPlot$cdtdataset <- readRDS(fileClimIdx)
				EnvClimatoCalcPlot$cdtdataset$fileInfo <- fileClimIdx
				EnvClimatoCalcPlot$fileClimIdx <- fileClimIdx
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

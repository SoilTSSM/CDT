
SPICalcPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(45)
		largeur0 <- as.integer(w.scale(22)/sfont0)
		largeur1 <- as.integer(w.scale(27)/sfont0)
		largeur2 <- as.integer(w.scale(29)/sfont0)
		largeur3 <- 20
		largeur4 <- 26
		largeur5 <- 20
		# largeur6 <- 22
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(47)
		largeur0 <- as.integer(w.scale(18)/sfont0)
		largeur1 <- as.integer(w.scale(22)/sfont0)
		largeur2 <- as.integer(w.scale(23)/sfont0)
		largeur3 <- 15
		largeur4 <- 20
		largeur5 <- 14
		# largeur6 <- 14
	}

	# GeneralParameters <- fromJSON(file.path(apps.dir, 'init_params', 'ClimatoAnalysis.json'))
	GeneralParameters <- list(intstep = "dekadal", data.type = "cdtstation", 
							cdtstation = "", cdtdataset = "",
							tscale = 3, distr = 'Gamma', outdir = "")
	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
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

		# tkbind(cb.fperiod, "<<ComboboxSelected>>", function(){
		# 	if(tclvalue(updateSPI) == '0'){
		# 		statedayW <- if(str_trim(tclvalue(timeSteps)) == 'Daily data' &
		# 						tclvalue(climDataExist) == '0') "normal" else "disabled"
		# 	}else statedayW <- "disabled"

		# 	tkconfigure(en.daywin, state = statedayW)
		# 	tclvalue(day.txtVar) <- switch(tclvalue(timeSteps), 'Dekadal data' = 'Dek', 'Pentad data' = 'Pen', 'Day')
		# 	stateday <- if(tclvalue(timeSteps) == 'Monthly data') 'disabled' else 'normal'
		# 	tkconfigure(en.day1, state = stateday)
		# 	tkconfigure(en.day2, state = stateday)
		# })

		#######################

		frameInData <- ttklabelframe(subfr1, text = "Input Data", relief = 'groove')

		DataType <- tclVar()
		CbdatatypeVAL <- c('CDT stations data format', 'CDT dataset format (gridded)')
		tclvalue(DataType) <- switch(GeneralParameters$data.type,
									'cdtstation' = CbdatatypeVAL[1],
									'cdtdataset' = CbdatatypeVAL[2])

		if(GeneralParameters$data.type == 'cdtstation'){
			input.file <- tclVar(GeneralParameters$cdtstation)
			txt.INData <- 'File containing stations Precip data'
			stateSetNC <- "disabled"
		}else{
			input.file <- tclVar(GeneralParameters$cdtdataset)
			txt.INData <- 'Index file (*.rds) for Precip dataset'
			stateSetNC <- "disabled"
		}

		txt.INData.var <- tclVar(txt.INData)

		txt.datatype <- tklabel(frameInData, text = "Format", anchor = 'w', justify = 'left')
		cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)

		txt.infile <- tklabel(frameInData, text = tclvalue(txt.INData.var), textvariable = txt.INData.var, anchor = 'w', justify = 'left')

		if(GeneralParameters$data.type == 'cdtstation'){
			cb.en.infile <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)
		}else{
			cb.en.infile <- tkentry(frameInData, textvariable = input.file, width = largeur2)
		}
		bt.infile <- tkbutton(frameInData, text = "...")

		############

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
			}else{
				filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
				tclvalue(input.file) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
			}
		})

		############

		tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.infile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
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
		}else{
			infobulle(cb.en.infile, 'Enter the full path to the file <dataset name>.rds')
			status.bar.display(cb.en.infile, TextOutputVar, 'Enter the full path to the file <dataset name>.rds')
			infobulle(bt.infile, 'or browse here')
			status.bar.display(bt.infile, TextOutputVar, 'or browse here')
		}

		############

		tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
			tkdestroy(cb.en.infile)
			tclvalue(input.file) <- ''

			###
			if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
				tclvalue(txt.INData.var) <- 'File containing stations Precip data'

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
				tclvalue(txt.INData.var) <- 'Index file (*.rds) for Precip dataset'

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

			tkgrid(cb.en.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		})

		#############################

		frameParams <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		frameTscale <- tkframe(frameParams)
		txt.Tscale1 <- tklabel(frameTscale, text = "SPI timescale", anchor = 'e', justify = 'right')
		spin.Tscale <- ttkspinbox(frameTscale, from = 1, to = 60, increment = 1, justify = 'center', width = 2)
		tkset(spin.Tscale, GeneralParameters$tscale)
		txt.Tscale2 <- tklabel(frameTscale, text = "-month", anchor = 'w', justify = 'left')

		tkgrid(txt.Tscale1, spin.Tscale, txt.Tscale2)

		########
		frameDistrb <- tkframe(frameParams)

		DistrbVAL <- c("Gamma", "Pearson Type III", "log-Logistic", "Z-Score")
		DistrbFun <- tclVar(GeneralParameters$distr)

		txt.Distrb <- tklabel(frameDistrb, text = "Distribution function", anchor = 'e', justify = 'right')
		cb.Distrb <- ttkcombobox(frameDistrb, values = DistrbVAL, textvariable = DistrbFun, width = largeur3)

		tkgrid(txt.Distrb, cb.Distrb)

		########
		tkgrid(frameTscale, row = 0, column = 0, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameDistrb, row = 1, column = 0, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		#############################

		frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		outSPIdir <- tclVar(GeneralParameters$outdir)

		txt.outSPI <- tklabel(frameDirSav, text = "Directory to save the outputs", anchor = 'w', justify = 'left')
		en.outSPI <- tkentry(frameDirSav, textvariable = outSPIdir, width = largeur2)
		bt.outSPI <- tkbutton(frameDirSav, text = "...")

		######

		tkconfigure(bt.outSPI, command = function(){
			dirSPI <- tk_choose.dir(getwd(), "")
			tclvalue(outSPIdir) <- if(dirSPI%in%c("", "NA") | is.na(dirSPI)) "" else dirSPI
		})

		######
		tkgrid(txt.outSPI, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.outSPI, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.outSPI, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		infobulle(en.outSPI, 'Enter the full path to directory to save outputs')
		status.bar.display(en.outSPI, TextOutputVar, 'Enter the full path to directory to save outputs')
		infobulle(bt.outSPI, 'or browse here')
		status.bar.display(bt.outSPI, TextOutputVar, 'or browse here')

		#############################

		if(!is.null(EnvSPICalcPlot$DirExist)){
			stateCaclBut <- if(tclvalue(EnvSPICalcPlot$DirExist) == "1") "normal" else "disabled"
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
												'CDT dataset format (gridded)' = 'cdtdataset')

			if(str_trim(tclvalue(DataType)) == 'CDT stations data format')
				GeneralParameters$cdtstation <- str_trim(tclvalue(input.file))
			if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)')
				GeneralParameters$cdtdataset <- str_trim(tclvalue(input.file))

			GeneralParameters$outdir <- str_trim(tclvalue(outSPIdir))
			GeneralParameters$tscale <- as.numeric(str_trim(tclvalue(tkget(spin.Tscale))))
			GeneralParameters$distr <- str_trim(tclvalue(DistrbFun))

			assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

			tkconfigure(main.win, cursor = 'watch')
			InsertMessagesTxt(main.txt.out, "Calculate SPI ......")

			ret <- tryCatch(
				computeSPIProcs(GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = tkconfigure(main.win, cursor = '')
			)

			msg0 <- "SPI calculation finished successfully"
			msg1 <- "SPI calculation failed"

			if(!is.null(ret)){
				if(ret == 0){
					InsertMessagesTxt(main.txt.out, msg0)

					###################

					# load.PICSA.Data()
					###################

					widgets.Station.Pixel()

				}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
			}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
		})

		############################################

		tkgrid(frameTimeS, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameParams, row = 2, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(calculateBut, row = 4, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

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

		frameSPIDat <- ttklabelframe(subfr2, text = "SPI data", relief = 'groove')

		EnvSPICalcPlot$DirExist <- tclVar(0)
		file.Stat <- tclVar()

		statedirStat <- if(tclvalue(EnvSPICalcPlot$DirExist) == "1") "normal" else "disabled"

		chk.dirStat <- tkcheckbutton(frameSPIDat, variable = EnvSPICalcPlot$DirExist, text = "SPI data already computed", anchor = 'w', justify = 'left')
		en.dirStat <- tkentry(frameSPIDat, textvariable = file.Stat, width = largeur2, state = statedirStat)
		bt.dirStat <- tkbutton(frameSPIDat, text = "...", state = statedirStat)

		tkconfigure(bt.dirStat, command = function(){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.Stat <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			if(path.Stat%in%c("", "NA") | is.na(path.Stat)) return(NULL)
			tclvalue(file.Stat) <- path.Stat


			if(file.exists(str_trim(tclvalue(file.Stat)))){
				OutSPIdata <- try(readRDS(str_trim(tclvalue(file.Stat))), silent = TRUE)
				if(inherits(OutSPIdata, "try-error")){
					InsertMessagesTxt(main.txt.out, 'Unable to load SPI data', format = TRUE)
					InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', OutSPIdata[1]), format = TRUE)
					tkconfigure(cb.spi.maps, values = "")
					tclvalue(EnvSPICalcPlot$spi.tscale) <- ""
					tkconfigure(cb.spi.Date, values = "")
					tclvalue(EnvSPICalcPlot$spi.date) <- ""
					return(NULL)
				}

				EnvSPICalcPlot$output <- OutSPIdata
				EnvSPICalcPlot$PathSPI <- dirname(str_trim(tclvalue(file.Stat)))

				###################

				widgets.Station.Pixel()
			}

		})


		tkgrid(chk.dirStat, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.dirStat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.dirStat, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(chk.dirStat, "<Button-1>", function(){
			statedirStat <- if(tclvalue(EnvSPICalcPlot$DirExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.dirStat, state = statedirStat)
			tkconfigure(bt.dirStat, state = statedirStat)
			stateCaclBut <- if(tclvalue(EnvSPICalcPlot$DirExist) == '1') 'normal' else 'disabled'
			tkconfigure(calculateBut, state = stateCaclBut)
		})

		##############################################

		frameSPIMap <- ttklabelframe(subfr2, text = "SPI Map", relief = 'groove')

		EnvSPICalcPlot$spi.tscale <- tclVar()
		EnvSPICalcPlot$spi.date <- tclVar()

		cb.spi.maps <- ttkcombobox(frameSPIMap, values = "", textvariable = EnvSPICalcPlot$spi.tscale, width = largeur4)
		bt.spi.maps <- ttkbutton(frameSPIMap, text = "PLOT", width = 7)
		cb.spi.Date <- ttkcombobox(frameSPIMap, values = "", textvariable = EnvSPICalcPlot$spi.date, width = largeur5)
		bt.spi.Date.prev <- ttkbutton(frameSPIMap, text = "<<", width = 3)
		bt.spi.Date.next <- ttkbutton(frameSPIMap, text = ">>", width = 3)
		bt.spi.MapOpt <- ttkbutton(frameSPIMap, text = "Options", width = 7)


		tkgrid(cb.spi.maps, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.spi.maps, row = 0, column = 4, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.spi.Date.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.spi.Date, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.spi.Date.next, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.spi.MapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		##############################################

		frameSPITS <- ttklabelframe(subfr2, text = "SPI Graph", relief = 'groove')

		typeTSPLOT <- c("Line", "Barplot")
		EnvSPICalcPlot$graph$typeTSp <- tclVar("Line")

		cb.typeTSp <- ttkcombobox(frameSPITS, values = typeTSPLOT, textvariable = EnvSPICalcPlot$graph$typeTSp, width = largeur5)
		bt.TsGraph.plot <- ttkbutton(frameSPITS, text = "PLOT", width = 7)
		bt.TSGraphOpt <- ttkbutton(frameSPITS, text = "Options", width = 8)

		frTS2 <- tkframe(frameSPITS)
		EnvSPICalcPlot$graph$lonLOC <- tclVar()
		EnvSPICalcPlot$graph$latLOC <- tclVar()
		EnvSPICalcPlot$graph$stnIDTSp <- tclVar()



		tkgrid(cb.typeTSp, row = 0, column = 0, sticky = 'we', pady = 1, columnspan = 1)
		tkgrid(bt.TSGraphOpt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
		tkgrid(bt.TsGraph.plot, row = 0, column = 2, sticky = 'we', pady = 1, columnspan = 1)
		tkgrid(frTS2, row = 1, column = 0, sticky = 'e', pady = 1, columnspan = 3)


		##############################################

		frameSHP <- ttklabelframe(subfr2, text = "Boundaries", relief = 'groove')

		EnvSPICalcPlot$shp$add.shp <- tclVar(0)
		file.plotShp <- tclVar()
		stateSHP <- "disabled"

		chk.addshp <- tkcheckbutton(frameSHP, variable = EnvSPICalcPlot$shp$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
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
				if(is.null(shpofile)) EnvSPICalcPlot$shp$ocrds <- NULL
				EnvSPICalcPlot$shp$ocrds <- getBoundaries(shpofile[[2]])
			}else return(NULL)
		})

		########
		EnvSPICalcPlot$SHPOp <- list(col = "black", lwd = 1.5)

		tkconfigure(bt.addshpOpt, command = function(){
			EnvSPICalcPlot$SHPOp <- climatoAnalysis.GraphOptions.LineSHP(main.win, EnvSPICalcPlot$SHPOp)
		})

		########
		tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
		tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		#################
		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile)) EnvSPICalcPlot$shp$ocrds <- NULL
			EnvSPICalcPlot$shp$ocrds <- getBoundaries(shpofile[[2]])
		})

		tkbind(chk.addshp, "<Button-1>", function(){
			stateSHP <- if(tclvalue(EnvSPICalcPlot$shp$add.shp) == "1") "disabled" else "normal"
			tkconfigure(cb.addshp, state = stateSHP)
			tkconfigure(bt.addshp, state = stateSHP)
			tkconfigure(bt.addshpOpt, state = stateSHP)
		})


		##############################################

		tkgrid(frameSPIDat, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameSPIMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameSPITS, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameSHP, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################################################################################################

	widgets.Station.Pixel <- function(){
		tkdestroy(frTS2)
		frTS2 <<- tkframe(frameSPITS)

		if(EnvSPICalcPlot$output$params$data.type == "cdtstation"){
			stnIDTSPLOT <- EnvSPICalcPlot$output$index$id
			txt.stnID <- tklabel(frTS2, text = "Station", anchor = 'e', justify = 'right')
			cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = EnvSPICalcPlot$graph$stnIDTSp, width = largeur5)
			tclvalue(EnvSPICalcPlot$graph$stnIDTSp) <- stnIDTSPLOT[1]
			tkgrid(txt.stnID, cb.stnID)
		}else{
			txt.lonLoc <- tklabel(frTS2, text = "Longitude", anchor = 'e', justify = 'right')
			en.lonLoc <- tkentry(frTS2, textvariable = EnvSPICalcPlot$graph$lonLOC, width = 8)
			txt.latLoc <- tklabel(frTS2, text = "Latitude", anchor = 'e', justify = 'right')
			en.latLoc <- tkentry(frTS2, textvariable = EnvSPICalcPlot$graph$latLOC, width = 8)
			tkgrid(txt.lonLoc, en.lonLoc, txt.latLoc, en.latLoc)
			stnIDTSPLOT <- ""
			tclvalue(EnvSPICalcPlot$graph$stnIDTSp) <- ""
		}

		tkgrid(frTS2, row = 1, column = 0, sticky = 'e', pady = 1, columnspan = 3)
	}

	#######################################################################################################

	tcl('update')
	tkgrid(cmd.frame, sticky = '', pady = 1)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)

	######
	return(cmd.frame)
}

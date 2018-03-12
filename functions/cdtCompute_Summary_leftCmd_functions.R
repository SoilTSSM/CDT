
summariesDataPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(45)
		largeur0 <- as.integer(w.scale(22)/sfont0)
		largeur1 <- as.integer(w.scale(29)/sfont0)
		largeur2 <- as.integer(w.scale(31)/sfont0)
		largeur3 <- 20
		largeur4 <- 30
		largeur5 <- 22
		largeur6 <- 22
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(47)
		largeur0 <- as.integer(w.scale(18)/sfont0)
		largeur1 <- as.integer(w.scale(22)/sfont0)
		largeur2 <- as.integer(w.scale(23)/sfont0)
		largeur3 <- 15
		largeur4 <- 22
		largeur5 <- 14
		largeur6 <- 14
	}

	# GeneralParameters <- fromJSON(file.path(apps.dir, 'init_params', 'ClimatoAnalysis.json'))
	# MOIS <- format(ISOdate(2014, 1:12, 1), "%B")
	GeneralParameters <- list(intstep = "dekadal", data.type = "cdtstation", 
							cdtstation = list(file = ""),
							cdtdataset = list(index = ""),
							outdir = "")

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Table & Graph")

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

		#######################

		frameInData <- ttklabelframe(subfr1, text = "Input Data", relief = 'groove')

		DataType <- tclVar()
		CbdatatypeVAL <- c('CDT stations data format', 'CDT dataset format (gridded)')
		tclvalue(DataType) <- switch(GeneralParameters$data.type,
									'cdtstation' = CbdatatypeVAL[1],
									'cdtdataset' = CbdatatypeVAL[2])

		if(GeneralParameters$data.type == 'cdtstation'){
			input.file <- tclVar(GeneralParameters$cdtstation$file)
			txt.INData <- 'File containing stations data'
			stateSetNC <- "disabled"
		}else{
			input.file <- tclVar(GeneralParameters$cdtdataset$index)
			txt.INData <- 'Index file (*.rds) of the dataset'
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

			tkgrid(cb.en.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		})


		#############################

		frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		outAnom <- tclVar(GeneralParameters$outdir)

		txt.outAnom <- tklabel(frameDirSav, text = "Directory to save the outputs", anchor = 'w', justify = 'left')
		en.outAnom <- tkentry(frameDirSav, textvariable = outAnom, width = largeur2)
		bt.outAnom <- tkbutton(frameDirSav, text = "...")

		######

		tkconfigure(bt.outAnom, command = function(){
			dirAnom <- tk_choose.dir(getwd(), "")
			tclvalue(outAnom) <- if(dirAnom%in%c("", "NA") | is.na(dirAnom)) "" else dirAnom
		})

		######
		tkgrid(txt.outAnom, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.outAnom, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.outAnom, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		infobulle(en.outAnom, 'Enter the full path to directory to save outputs')
		status.bar.display(en.outAnom, TextOutputVar, 'Enter the full path to directory to save outputs')
		infobulle(bt.outAnom, 'or browse here')
		status.bar.display(bt.outAnom, TextOutputVar, 'or browse here')


		############################################

		if(!is.null(EnvSummaryDataplot$DirExist)){
			stateSumBut <- if(tclvalue(EnvSummaryDataplot$DirExist) == "1") "normal" else "disabled"
		}else stateSumBut <- "normal"

		summaryBut <- ttkbutton(subfr1, text = "Summarize", state = stateSumBut)

		#################

		tkconfigure(summaryBut, command = function(){
			GeneralParameters$intstep <- switch(str_trim(tclvalue(timeSteps)), 
			 									'Daily data' = 'daily',
			 									'Pentad data' = 'pentad',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')

			GeneralParameters$data.type <- switch(str_trim(tclvalue(DataType)),
												'CDT stations data format' = 'cdtstation',
												'CDT dataset format (gridded)' = 'cdtdataset')

			if(str_trim(tclvalue(DataType)) == 'CDT stations data format')
				GeneralParameters$cdtstation$file <- str_trim(tclvalue(input.file))
			if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)')
				GeneralParameters$cdtdataset$index <- str_trim(tclvalue(input.file))

			GeneralParameters$outdir <- str_trim(tclvalue(outAnom))

			# assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)

			tkconfigure(main.win, cursor = 'watch')
			InsertMessagesTxt(main.txt.out, "Summarizing data ......")

			ret <- tryCatch(
				summarizeDataProcs(GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = tkconfigure(main.win, cursor = '')
			)

			msg0 <- "Summarizing finished successfully"
			msg1 <- "Summarizing failed"

			if(!is.null(ret)){
				if(ret == 0){
					InsertMessagesTxt(main.txt.out, msg0)

					###################

					widgets.Station.Pixel()
				}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
			}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
		})



		############################################

		tkgrid(frameTimeS, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(summaryBut, row = 3, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

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

		frameSumData <- ttklabelframe(subfr2, text = "Summaries data", relief = 'groove')

		EnvSummaryDataplot$DirExist <- tclVar(0)
		file.Stat <- tclVar()

		statedirStat <- if(tclvalue(EnvSummaryDataplot$DirExist) == "1") "normal" else "disabled"

		chk.dirStat <- tkcheckbutton(frameSumData, variable = EnvSummaryDataplot$DirExist, text = "Data already summarized", anchor = 'w', justify = 'left')
		en.dirStat <- tkentry(frameSumData, textvariable = file.Stat, width = largeur2, state = statedirStat)
		bt.dirStat <- tkbutton(frameSumData, text = "...", state = statedirStat)
		bt.plotMap <- ttkbutton(frameSumData, text = "Plot map to select a pixel or station")

		frameSTNPX <- tkframe(frameSumData)
		EnvSummaryDataplot$graph$lonLOC <- tclVar()
		EnvSummaryDataplot$graph$latLOC <- tclVar()
		EnvSummaryDataplot$graph$stnIDTSp <- tclVar()

		tkconfigure(bt.dirStat, command = function(){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.Stat <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			if(path.Stat%in%c("", "NA") | is.na(path.Stat)) return(NULL)
			tclvalue(file.Stat) <- path.Stat

			if(file.exists(str_trim(tclvalue(file.Stat)))){
				OutSummary <- try(readRDS(str_trim(tclvalue(file.Stat))), silent = TRUE)
				if(inherits(OutSummary, "try-error")){
					InsertMessagesTxt(main.txt.out, 'Unable to load Summary data', format = TRUE)
					InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', OutSummary[1]), format = TRUE)
					return(NULL)
				}

				EnvSummaryDataplot$output <- OutSummary
				EnvSummaryDataplot$PathSum <- dirname(str_trim(tclvalue(file.Stat)))

				###################

				widgets.Station.Pixel()
			}
		})

		EnvSummaryDataplot$notebookTab.Map <- NULL
		tkconfigure(bt.plotMap, command = function(){
			if(!is.null(EnvSummaryDataplot$output)){
				imgContainer <- SummaryData.Display.Map(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvSummaryDataplot$notebookTab.Map, AllOpenTabType, AllOpenTabData)
				EnvSummaryDataplot$notebookTab.Map <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		###############

		tkgrid(chk.dirStat, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.dirStat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.dirStat, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.plotMap, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameSTNPX, row = 3, column = 0, sticky = 'e', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(chk.dirStat, "<Button-1>", function(){
			statedirStat <- if(tclvalue(EnvSummaryDataplot$DirExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.dirStat, state = statedirStat)
			tkconfigure(bt.dirStat, state = statedirStat)
			stateSumBut <- if(tclvalue(EnvSummaryDataplot$DirExist) == '1') 'normal' else 'disabled'
			tkconfigure(summaryBut, state = stateSumBut)
		})

		##############################################

		bt.SumTable <- ttkbutton(subfr2, text = "Display Summary Table")

		EnvSummaryDataplot$notebookTab.Table <- NULL

		tkconfigure(bt.SumTable, command = function(){
			if(!is.null(EnvSummaryDataplot$output)){
				
				retNBTab <- SummaryData.Display.Table(tknotes, "Summary Table", EnvSummaryDataplot$notebookTab.Table, AllOpenTabType, AllOpenTabData)
				EnvSummaryDataplot$notebookTab.Table <<- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
				ReturnExecResults <<- 0
			}
		})

		##############################################

		frameSumGraph <- ttklabelframe(subfr2, text = "Summary Graph", relief = 'groove')

		mois <- c(format(ISOdate(2014, 1:12, 1), "%b"), "ALL")
		EnvSummaryDataplot$plotType <- tclVar("Boxplot")
		EnvSummaryDataplot$plotMois <- tclVar(mois[1])

		cb.SumGraph.Type <- ttkcombobox(frameSumGraph, values = c("Boxplot", "Histogram"), textvariable = EnvSummaryDataplot$plotType, width = largeur4)
		bt.SumGraph.Plot <- ttkbutton(frameSumGraph, text = "PLOT")
		cb.SumGraph.Mois <- ttkcombobox(frameSumGraph, values = mois, textvariable = EnvSummaryDataplot$plotMois, width = largeur5, state = "disabled")
		bt.SumGraph.Opt <- ttkbutton(frameSumGraph, text = "Options")

		EnvSummaryDataplot$notebookTab.Graph <- NULL
		tkconfigure(bt.SumGraph.Plot, command = function(){
			if(!is.null(EnvSummaryDataplot$output)){
				imgContainer <- SummaryData.Display.Graph(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvSummaryDataplot$notebookTab.Graph, AllOpenTabType, AllOpenTabData)
				EnvSummaryDataplot$notebookTab.Graph <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		###################
		tkgrid(cb.SumGraph.Type, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.SumGraph.Plot, row = 0, column = 4, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.SumGraph.Mois, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.SumGraph.Opt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkbind(cb.SumGraph.Type, "<<ComboboxSelected>>", function(){
			stateMois <- if(str_trim(tclvalue(EnvSummaryDataplot$plotType)) == "Boxplot") "disabled" else "normal"
			tkconfigure(cb.SumGraph.Mois, state = stateMois)
		})

		##############################################

		frameSHP <- ttklabelframe(subfr2, text = "Boundaries", relief = 'groove')

		EnvSummaryDataplot$shp$add.shp <- tclVar(0)
		file.plotShp <- tclVar()
		stateSHP <- "disabled"

		chk.addshp <- tkcheckbutton(frameSHP, variable = EnvSummaryDataplot$shp$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
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

				# tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
				tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = file.plotShp)

				shpofile <- getShpOpenData(file.plotShp)
				if(is.null(shpofile)) EnvSummaryDataplot$shp$ocrds <- NULL
				EnvSummaryDataplot$shp$ocrds <- getBoundaries(shpofile[[2]])
			}else return(NULL)
		})

		########
		EnvSummaryDataplot$SHPOp <- list(col = "black", lwd = 1.5)

		tkconfigure(bt.addshpOpt, command = function(){
			EnvSummaryDataplot$SHPOp <- climatoAnalysis.GraphOptions.LineSHP(main.win, EnvSummaryDataplot$SHPOp)
		})

		########
		tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
		tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		#################
		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile)) EnvSummaryDataplot$shp$ocrds <- NULL
			EnvSummaryDataplot$shp$ocrds <- getBoundaries(shpofile[[2]])
		})

		tkbind(chk.addshp, "<Button-1>", function(){
			stateSHP <- if(tclvalue(EnvSummaryDataplot$shp$add.shp) == "1") "disabled" else "normal"
			tkconfigure(cb.addshp, state = stateSHP)
			tkconfigure(bt.addshp, state = stateSHP)
			tkconfigure(bt.addshpOpt, state = stateSHP)
		})

		##############################################

		tkgrid(frameSumData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.SumTable, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameSumGraph, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameSHP, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################################################################################################

	widgets.Station.Pixel <- function(){
		tkdestroy(frameSTNPX)
		frameSTNPX <<- tkframe(frameSumData)

		if(EnvSummaryDataplot$output$params$data.type == "cdtstation"){
			stnIDTSPLOT <- EnvSummaryDataplot$output$data$id
			txt.stnID <- tklabel(frameSTNPX, text = "Station", anchor = 'e', justify = 'right')
			cb.stnID <- ttkcombobox(frameSTNPX, values = stnIDTSPLOT, textvariable = EnvSummaryDataplot$graph$stnIDTSp, width = largeur6)
			tclvalue(EnvSummaryDataplot$graph$stnIDTSp) <- stnIDTSPLOT[1]
			tkgrid(txt.stnID, cb.stnID)
		}else{
			txt.lonLoc <- tklabel(frameSTNPX, text = "Longitude", anchor = 'e', justify = 'right')
			en.lonLoc <- tkentry(frameSTNPX, textvariable = EnvSummaryDataplot$graph$lonLOC, width = 8)
			txt.latLoc <- tklabel(frameSTNPX, text = "Latitude", anchor = 'e', justify = 'right')
			en.latLoc <- tkentry(frameSTNPX, textvariable = EnvSummaryDataplot$graph$latLOC, width = 8)
			tkgrid(txt.lonLoc, en.lonLoc, txt.latLoc, en.latLoc)
			stnIDTSPLOT <- ""
			tclvalue(EnvSummaryDataplot$graph$stnIDTSp) <- ""
		}
		tkgrid(frameSTNPX, row = 3, column = 0, sticky = 'e', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	}

	#######################################################################################################

	tcl('update')
	tkgrid(cmd.frame, sticky = '', pady = 1)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)

	######
	return(cmd.frame)
}


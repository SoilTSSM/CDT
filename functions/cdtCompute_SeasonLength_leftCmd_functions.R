

SeasonLengthCalcPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(45)
		largeur0 <- as.integer(w.scale(29)/sfont0)
		largeur1 <- as.integer(w.scale(27)/sfont0)
		largeur2 <- 21
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(50)
		largeur0 <- as.integer(w.scale(23)/sfont0)
		largeur1 <- as.integer(w.scale(21)/sfont0)
		largeur2 <- 14
	}

	# GeneralParameters <- fromJSON(file.path(apps.dir, 'init_params', 'ClimatoAnalysis.json'))
	GeneralParameters <- list(onset = "", cessation = "", output = "")

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Season Length")
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

		frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

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

		frameCalc <- tkframe(subfr1)

		if(!is.null(EnvSeasLengthCalcPlot$DirExist)){
			stateCaclBut <- if(tclvalue(EnvSeasLengthCalcPlot$DirExist) == "1") "normal" else "disabled"
		}else stateCaclBut <- "normal"

		bt.CalcOnset <- tkbutton(frameCalc, text = 'Calculate Season Length', state = stateCaclBut, bg = 'lightgreen')

		tkconfigure(bt.CalcOnset, command = function(){
			GeneralParameters$onset <- str_trim(tclvalue(input.Onset))
			GeneralParameters$cessation <- str_trim(tclvalue(input.Cessation))
			GeneralParameters$output <- str_trim(tclvalue(dir.save))

			# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

			tkconfigure(main.win, cursor = 'watch')
			InsertMessagesTxt(main.txt.out, "Calculate Length of the season ......")

			ret <- tryCatch(
				compute_SeasonLength_Procs(GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = tkconfigure(main.win, cursor = '')
			)

			msg0 <- "Season Length calculation finished successfully"
			msg1 <- "Season Length calculation failed"

			if(!is.null(ret)){
				if(ret == 0){
					InsertMessagesTxt(main.txt.out, msg0)

					###################
					set.Data.Dates()
					widgets.Station.Pixel()
					res <- EnvSeasLengthCalcPlot$read.Data.Map()
					if(inherits(res, "try-error") | is.null(res)) return(NULL)
				}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
			}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
		})

		####################

		tkgrid(bt.CalcOnset, row = 0, column = 0, sticky = 'we', pady = 1)

		############################################

		tkgrid(frameInData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameCalc, row = 2, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

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

		frameDataExist <- ttklabelframe(subfr2, text = "Season Length data", relief = 'groove')

		EnvSeasLengthCalcPlot$DirExist <- tclVar(0)
		file.dataIndex <- tclVar()

		stateExistData <- if(tclvalue(EnvSeasLengthCalcPlot$DirExist) == "1") "normal" else "disabled"

		chk.dataIdx <- tkcheckbutton(frameDataExist, variable = EnvSeasLengthCalcPlot$DirExist, text = "Season length data already computed", anchor = 'w', justify = 'left')
		en.dataIdx <- tkentry(frameDataExist, textvariable = file.dataIndex, width = largeur0, state = stateExistData)
		bt.dataIdx <- tkbutton(frameDataExist, text = "...", state = stateExistData)

		tkconfigure(bt.dataIdx, command = function(){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.dataIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			if(path.dataIdx%in%c("", "NA") | is.na(path.dataIdx)) return(NULL)
			tclvalue(file.dataIndex) <- path.dataIdx

			if(file.exists(str_trim(tclvalue(file.dataIndex)))){
				OutIndexdata <- try(readRDS(str_trim(tclvalue(file.dataIndex))), silent = TRUE)
				if(inherits(OutIndexdata, "try-error")){
					InsertMessagesTxt(main.txt.out, 'Unable to load season length data', format = TRUE)
					InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', OutIndexdata[1]), format = TRUE)
					tkconfigure(cb.data.Index, values = "")
					tclvalue(EnvSeasLengthCalcPlot$donDate) <- ""
					return(NULL)
				}

				EnvSeasLengthCalcPlot$output <- OutIndexdata
				EnvSeasLengthCalcPlot$PathData <- dirname(str_trim(tclvalue(file.dataIndex)))

				###################
				set.Data.Dates()
				widgets.Station.Pixel()
				ret <- EnvSeasLengthCalcPlot$read.Data.Map()
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		tkgrid(chk.dataIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.dataIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.dataIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(chk.dataIdx, "<Button-1>", function(){
			stateExistData <- if(tclvalue(EnvSeasLengthCalcPlot$DirExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.dataIdx, state = stateExistData)
			tkconfigure(bt.dataIdx, state = stateExistData)
			stateCaclBut <- if(tclvalue(EnvSeasLengthCalcPlot$DirExist) == '1') 'normal' else 'disabled'
			tkconfigure(bt.CalcOnset, state = stateCaclBut)
		})
		##############################################

		frameDataMap <- ttklabelframe(subfr2, text = "Season Length Map", relief = 'groove')

		EnvSeasLengthCalcPlot$donDate <- tclVar()

		cb.data.Index <- ttkcombobox(frameDataMap, values = "", textvariable = EnvSeasLengthCalcPlot$donDate, width = largeur2)
		bt.data.Index.prev <- ttkbutton(frameDataMap, text = "<<", width = 3)
		bt.data.Index.next <- ttkbutton(frameDataMap, text = ">>", width = 3)
		bt.data.maps <- ttkbutton(frameDataMap, text = "PLOT", width = 7)
		bt.data.MapOpt <- ttkbutton(frameDataMap, text = "Options", width = 7)

		###############

		EnvSeasLengthCalcPlot$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
												userCol = list(custom = FALSE, color = NULL),
												userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
												title = list(user = FALSE, title = ''),
												colkeyLab = list(user = FALSE, label = ''),
												scalebar = list(add = FALSE, pos = 'bottomleft'))

		tkconfigure(bt.data.MapOpt, command = function(){
			if(!is.null(EnvSeasLengthCalcPlot$varData$map)){
				atlevel <- pretty(EnvSeasLengthCalcPlot$varData$map$z, n = 10, min.n = 7)
				if(is.null(EnvSeasLengthCalcPlot$dataMapOp$userLvl$levels)){
					EnvSeasLengthCalcPlot$dataMapOp$userLvl$levels <- atlevel
				}else{
					if(!EnvSeasLengthCalcPlot$dataMapOp$userLvl$custom)
						EnvSeasLengthCalcPlot$dataMapOp$userLvl$levels <- atlevel
				}
			}
			EnvSeasLengthCalcPlot$dataMapOp <- climatoAnalysis.MapOptions(main.win, EnvSeasLengthCalcPlot$dataMapOp)
		})

		#########
		EnvSeasLengthCalcPlot$notebookTab.dataMap <- NULL

		tkconfigure(bt.data.maps, command = function(){
			if(str_trim(tclvalue(EnvSeasLengthCalcPlot$donDate)) != "" &
				!is.null(EnvSeasLengthCalcPlot$varData))
			{
				imgContainer <- SeasonLengthCalc.Display.Maps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvSeasLengthCalcPlot$notebookTab.dataMap, AllOpenTabType, AllOpenTabData)
				EnvSeasLengthCalcPlot$notebookTab.dataMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.data.Index.prev, command = function(){
			if(str_trim(tclvalue(EnvSeasLengthCalcPlot$donDate)) != ""){
				donDates <- format(EnvSeasLengthCalcPlot$output$start.date, "%Y")
				idaty <- which(donDates == str_trim(tclvalue(EnvSeasLengthCalcPlot$donDate)))
				idaty <- idaty-1
				if(idaty < 1) idaty <- length(donDates)
				tclvalue(EnvSeasLengthCalcPlot$donDate) <- donDates[idaty]

				ret <- try(EnvSeasLengthCalcPlot$read.Data.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				imgContainer <- SeasonLengthCalc.Display.Maps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvSeasLengthCalcPlot$notebookTab.dataMap, AllOpenTabType, AllOpenTabData)
				EnvSeasLengthCalcPlot$notebookTab.dataMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.data.Index.next, command = function(){
			if(str_trim(tclvalue(EnvSeasLengthCalcPlot$donDate)) != ""){
				donDates <- format(EnvSeasLengthCalcPlot$output$start.date, "%Y")
				idaty <- which(donDates == str_trim(tclvalue(EnvSeasLengthCalcPlot$donDate)))
				idaty <- idaty+1
				if(idaty > length(donDates)) idaty <- 1
				tclvalue(EnvSeasLengthCalcPlot$donDate) <- donDates[idaty]

				ret <- try(EnvSeasLengthCalcPlot$read.Data.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				imgContainer <- SeasonLengthCalc.Display.Maps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvSeasLengthCalcPlot$notebookTab.dataMap, AllOpenTabType, AllOpenTabData)
				EnvSeasLengthCalcPlot$notebookTab.dataMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		###############

		tkgrid(bt.data.Index.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.data.Index, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.data.Index.next, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.data.maps, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.data.MapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		###############

		tkbind(cb.data.Index, "<<ComboboxSelected>>", function(){
			if(!is.null(EnvSeasLengthCalcPlot$varData)){
				ret <- try(EnvSeasLengthCalcPlot$read.Data.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		##############################################

		tkgrid(frameDataExist, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameDataMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

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

		frameDataTS <- ttklabelframe(subfr3, text = "Season Length Graph", relief = 'groove')

		typeTSPLOT <- c("Line", "Barplot")
		EnvSeasLengthCalcPlot$graph$typeTSp <- tclVar("Line")

		cb.typeTSp <- ttkcombobox(frameDataTS, values = typeTSPLOT, textvariable = EnvSeasLengthCalcPlot$graph$typeTSp, width = largeur2)
		bt.TsGraph.plot <- ttkbutton(frameDataTS, text = "PLOT", width = 7)
		bt.TSGraphOpt <- ttkbutton(frameDataTS, text = "Options", width = 8)

		#################

		EnvSeasLengthCalcPlot$TSGraphOp <- list(
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
						legend = NULL)
					)

		tkconfigure(bt.TSGraphOpt, command = function(){
			suffix.fun <- switch(str_trim(tclvalue(EnvSeasLengthCalcPlot$graph$typeTSp)),
									"Barplot" = "Bar",
									"Line" = "Line")
			plot.fun <- match.fun(paste0("climatoAnalysis.GraphOptions.", suffix.fun))
			EnvSeasLengthCalcPlot$TSGraphOp <- plot.fun(main.win, EnvSeasLengthCalcPlot$TSGraphOp)
		})

		#########
		EnvSeasLengthCalcPlot$notebookTab.dataGraph <- NULL

		tkconfigure(bt.TsGraph.plot, command = function(){
			if(!is.null(EnvSeasLengthCalcPlot$varData)){
				imgContainer <- SeasonLengthCalc.Display.Graph(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvSeasLengthCalcPlot$notebookTab.dataGraph, AllOpenTabType, AllOpenTabData)
				EnvSeasLengthCalcPlot$notebookTab.dataGraph <- retNBTab$notebookTab
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
		EnvSeasLengthCalcPlot$graph$lonLOC <- tclVar()
		EnvSeasLengthCalcPlot$graph$latLOC <- tclVar()
		EnvSeasLengthCalcPlot$graph$stnIDTSp <- tclVar()

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

		##############################################

		tkgrid(frameDataTS, row = 0, column = 0, sticky = 'we', pady = 1)
		tkgrid(frameSTNCrds, row = 1, column = 0, sticky = 'we', pady = 3)

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

		frameSHP <- ttklabelframe(subfr4, text = "Boundaries", relief = 'groove')

		EnvSeasLengthCalcPlot$shp$add.shp <- tclVar(FALSE)
		file.plotShp <- tclVar()
		stateSHP <- "disabled"

		chk.addshp <- tkcheckbutton(frameSHP, variable = EnvSeasLengthCalcPlot$shp$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
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
				if(is.null(shpofile)) EnvSeasLengthCalcPlot$shp$ocrds <- NULL
				EnvSeasLengthCalcPlot$shp$ocrds <- getBoundaries(shpofile[[2]])
			}else return(NULL)
		})

		########
		EnvSeasLengthCalcPlot$SHPOp <- list(col = "black", lwd = 1.5)

		tkconfigure(bt.addshpOpt, command = function(){
			EnvSeasLengthCalcPlot$SHPOp <- climatoAnalysis.GraphOptions.LineSHP(main.win, EnvSeasLengthCalcPlot$SHPOp)
		})

		########
		tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
		tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		#################
		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile)) EnvSeasLengthCalcPlot$shp$ocrds <- NULL
			EnvSeasLengthCalcPlot$shp$ocrds <- getBoundaries(shpofile[[2]])
		})

		tkbind(chk.addshp, "<Button-1>", function(){
			stateSHP <- if(tclvalue(EnvSeasLengthCalcPlot$shp$add.shp) == "1") "disabled" else "normal"
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

		if(EnvSeasLengthCalcPlot$output$params$data.type == "cdtstation"){
			stnIDTSPLOT <- EnvSeasLengthCalcPlot$output$data$id
			txt.stnSel <- tklabel(frTS2, text = "Select a station to plot", anchor = 'w', justify = 'left')
			txt.stnID <- tklabel(frTS2, text = "Station", anchor = 'e', justify = 'right')
			cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = EnvSeasLengthCalcPlot$graph$stnIDTSp, width = largeur2)
			tclvalue(EnvSeasLengthCalcPlot$graph$stnIDTSp) <- stnIDTSPLOT[1]

			tkgrid(txt.stnSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.stnID, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}else{
			txt.crdSel <- tklabel(frTS2, text = "Enter longitude and latitude to plot", anchor = 'w', justify = 'left')
			txt.lonLoc <- tklabel(frTS2, text = "Longitude", anchor = 'e', justify = 'right')
			en.lonLoc <- tkentry(frTS2, textvariable = EnvSeasLengthCalcPlot$graph$lonLOC, width = 8)
			txt.latLoc <- tklabel(frTS2, text = "Latitude", anchor = 'e', justify = 'right')
			en.latLoc <- tkentry(frTS2, textvariable = EnvSeasLengthCalcPlot$graph$latLOC, width = 8)
			stnIDTSPLOT <- ""
			tclvalue(EnvSeasLengthCalcPlot$graph$stnIDTSp) <- ""

			tkgrid(txt.crdSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.lonLoc, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.lonLoc, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.latLoc, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.latLoc, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)
		return(0)
	}

	set.Data.Dates <- function(){
		donDates <- format(EnvSeasLengthCalcPlot$output$start.date, "%Y")
		tkconfigure(cb.data.Index, values = donDates)
		tclvalue(EnvSeasLengthCalcPlot$donDate) <- donDates[length(donDates)]
		return(0)
	}

	#######################################################################################################

	EnvSeasLengthCalcPlot$read.Data.Map <- function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		this.daty <- str_trim(tclvalue(EnvSeasLengthCalcPlot$donDate))
		idt <- which(format(EnvSeasLengthCalcPlot$output$start.date, "%Y") == this.daty)

		if(EnvSeasLengthCalcPlot$output$params$data.type == "cdtstation"){
			filePathData <- file.path(EnvSeasLengthCalcPlot$PathData, "CDTDATASET/SEASONLENGTH.rds")
			if(!file.exists(filePathData)){
				InsertMessagesTxt(main.txt.out, paste(filePathData, 'not found'), format = TRUE)
				return(NULL)
			}

			readVarData <- TRUE
			if(!is.null(EnvSeasLengthCalcPlot$varData))
				if(!is.null(EnvSeasLengthCalcPlot$filePathData))
					if(EnvSeasLengthCalcPlot$filePathData == filePathData) readVarData <- FALSE

			if(readVarData){
				EnvSeasLengthCalcPlot$varData$data <- readRDS(filePathData)
				EnvSeasLengthCalcPlot$filePathData <- filePathData
			}

			########
			rasterVarData <- TRUE
			if(!rasterVarData)
				if(!is.null(EnvSeasLengthCalcPlot$varData$rasterDate))
					if(EnvSeasLengthCalcPlot$filePathData == filePathData)
						if(EnvSeasLengthCalcPlot$varData$rasterDate == this.daty) rasterVarData <- FALSE

			if(rasterVarData){
				nx <- nx_ny_as.image(diff(range(EnvSeasLengthCalcPlot$output$data$lon)))
				ny <- nx_ny_as.image(diff(range(EnvSeasLengthCalcPlot$output$data$lat)))
				tmp <- as.numeric(EnvSeasLengthCalcPlot$varData$data[idt, ])

				tmp <- cdt.as.image(tmp, nx = nx, ny = ny,
								pts.xy = cbind(EnvSeasLengthCalcPlot$output$data$lon, EnvSeasLengthCalcPlot$output$data$lat))
				EnvSeasLengthCalcPlot$varData$map$x <- tmp$x
				EnvSeasLengthCalcPlot$varData$map$y <- tmp$y
				EnvSeasLengthCalcPlot$varData$map$z <- tmp$z
				EnvSeasLengthCalcPlot$varData$rasterDate <- this.daty
				rm(tmp)
			}
		}else{
			filePathData <- file.path(EnvSeasLengthCalcPlot$PathData, "DATA_NetCDF",
							paste0("seasLen_", format(EnvSeasLengthCalcPlot$output$start.date[idt], "%Y%m%d"), ".nc"))
			if(!file.exists(filePathData)){
				InsertMessagesTxt(main.txt.out, paste(filePathData, 'not found'), format = TRUE)
				return(NULL)
			}

			readVarData <- TRUE
			if(!is.null(EnvSeasLengthCalcPlot$varData))
				if(!is.null(EnvSeasLengthCalcPlot$filePathData))
					if(EnvSeasLengthCalcPlot$filePathData == filePathData) readVarData <- FALSE

			if(readVarData){
				nc <- nc_open(filePathData)
				EnvSeasLengthCalcPlot$varData$map$x <- nc$dim[[1]]$vals
				EnvSeasLengthCalcPlot$varData$map$y <- nc$dim[[2]]$vals
				EnvSeasLengthCalcPlot$varData$map$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
				nc_close(nc)
				EnvSeasLengthCalcPlot$filePathData <- filePathData
			}

			###################

			file.CDT.Idx <- file.path(EnvSeasLengthCalcPlot$PathData, "CDTDATASET/CDTDATASET.rds")

			read.cdt.dataIdx<- TRUE
			if(!is.null(EnvSeasLengthCalcPlot$cdtdataset))
				if(!is.null(EnvSeasLengthCalcPlot$file.CDT.Idx))
					if(EnvSeasLengthCalcPlot$file.CDT.Idx == file.CDT.Idx) read.cdt.dataIdx <- FALSE
			if(read.cdt.dataIdx){
				EnvSeasLengthCalcPlot$cdtdataset <- readRDS(file.CDT.Idx)
				EnvSeasLengthCalcPlot$cdtdataset$fileInfo <- file.CDT.Idx
				EnvSeasLengthCalcPlot$file.CDT.Idx <- file.CDT.Idx
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



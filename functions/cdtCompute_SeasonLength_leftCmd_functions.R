

SeasonLengthCalcPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(45)
		largeur0 <- as.integer(w.scale(29)/sfont0)

		# largeur0 <- as.integer(w.scale(22)/sfont0)
		# largeur1 <- as.integer(w.scale(29)/sfont0)
		# largeur2 <- as.integer(w.scale(31)/sfont0)

		# largeur3 <- 45
		# largeur4 <- largeur1-5
		# largeur5 <- 30
		# largeur6 <- 22
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(50)
		largeur0 <- as.integer(w.scale(23)/sfont0)

		# largeur0 <- as.integer(w.scale(20)/sfont0)
		# largeur1 <- as.integer(w.scale(21)/sfont0)
		# largeur2 <- as.integer(w.scale(23)/sfont0)

		# largeur3 <- 35
		# largeur4 <- largeur1
		# largeur5 <- 22
		# largeur6 <- 14
	}

	# GeneralParameters <- fromJSON(file.path(apps.dir, 'init_params', 'ClimatoAnalysis.json'))
	GeneralParameters <- list(onset = "", cessation = "", output = "")

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Season Length")
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

					# load.ClimatoAnalysis.Data()

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

		frameOnsetDat <- ttklabelframe(subfr2, text = "Season Length data", relief = 'groove')

		EnvSeasLengthCalcPlot$DirExist <- tclVar(0)
		file.OnsetIndex <- tclVar()

		stateOnsetDat <- if(tclvalue(EnvSeasLengthCalcPlot$DirExist) == "1") "normal" else "disabled"

		chk.OnsetIdx <- tkcheckbutton(frameOnsetDat, variable = EnvSeasLengthCalcPlot$DirExist, text = "Season length data already computed", anchor = 'w', justify = 'left')
		en.OnsetIdx <- tkentry(frameOnsetDat, textvariable = file.OnsetIndex, width = largeur0, state = stateOnsetDat)
		bt.OnsetIdx <- tkbutton(frameOnsetDat, text = "...", state = stateOnsetDat)

		tkconfigure(bt.OnsetIdx, command = function(){

		})


		tkgrid(chk.OnsetIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.OnsetIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.OnsetIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(chk.OnsetIdx, "<Button-1>", function(){
			stateOnsetDat <- if(tclvalue(EnvSeasLengthCalcPlot$DirExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.OnsetIdx, state = stateOnsetDat)
			tkconfigure(bt.OnsetIdx, state = stateOnsetDat)
			stateCaclBut <- if(tclvalue(EnvSeasLengthCalcPlot$DirExist) == '1') 'normal' else 'disabled'
			tkconfigure(bt.CalcOnset, state = stateCaclBut)
		})

		##############################################

		frameOnsetMap <- ttklabelframe(subfr2, text = "Season Length Map", relief = 'groove')



		##############################################

		frameOnsetTS <- ttklabelframe(subfr2, text = "Season Length Graph", relief = 'groove')



		##############################################

		tkgrid(frameOnsetDat, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameOnsetMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameOnsetTS, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)


	#######################################################################################################
	tcl('update')
	tkgrid(cmd.frame, sticky = '', pady = 1)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)
	######
	return(cmd.frame)
}



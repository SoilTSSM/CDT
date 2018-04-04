
dailyRainAnalysisPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(46)
		hscrlwin1 <- h.scale(27)
		largeur0 <- as.integer(w.scale(22)/sfont0)
		largeur1 <- as.integer(w.scale(27)/sfont0)
		largeur2 <- as.integer(w.scale(29)/sfont0)

		largeur3 <- 24
		# largeur4 <- largeur1-5
		# largeur5 <- 30
		# largeur6 <- 22
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(47)
		hscrlwin1 <- h.scale(28)
		largeur0 <- as.integer(w.scale(20)/sfont0)
		largeur1 <- as.integer(w.scale(21)/sfont0)
		largeur2 <- as.integer(w.scale(23)/sfont0)

		largeur3 <- 24
		# largeur4 <- largeur1
		# largeur5 <- 22
		# largeur6 <- 14
	}

	# GeneralParameters <- fromJSON(file.path(apps.dir, 'init_params', 'ClimatoAnalysis.json'))
	MOIS <- format(ISOdate(2014, 1:12, 1), "%b")
	GeneralParameters <- list(data.type = "cdtstation", cdtstation = "", cdtdataset = "",
							min.frac = 0.95, output = "")

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Analysis")
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

		############################################

		frameInData <- ttklabelframe(subfr1, text = "Input Data", relief = 'groove')

		DataType <- tclVar()
		CbdatatypeVAL <- c('CDT stations data format', 'CDT dataset format (gridded)')
		tclvalue(DataType) <- switch(GeneralParameters$data.type,
									'cdtstation' = CbdatatypeVAL[1],
									'cdtdataset' = CbdatatypeVAL[2])

		if(GeneralParameters$data.type == 'cdtstation'){
			input.Prec <- tclVar(GeneralParameters$cdtstation)
			txt.INPrec <- 'File containing stations daily Precip data'
		}else{
			input.Prec <- tclVar(GeneralParameters$cdtdataset)
			txt.INPrec <- 'Index file (*.rds) for daily Precip data'
		}
		txt.INPrec.var <- tclVar(txt.INPrec)

		txt.datatype <- tklabel(frameInData, text = "Format", anchor = 'w', justify = 'left')
		cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)

		txt.INPrec <- tklabel(frameInData, text = tclvalue(txt.INPrec.var), textvariable = txt.INPrec.var, anchor = 'w', justify = 'left')
		if(GeneralParameters$data.type == 'cdtstation'){
			cb.en.INPrec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur1)
		}else{
			cb.en.INPrec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2)
		}
		bt.INPrec <- tkbutton(frameInData, text = "...")

		############

		tkconfigure(bt.INPrec, command = function(){
			if(GeneralParameters$data.type == 'cdtstation'){
				dat.opfiles <- getOpenFiles(main.win, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(input.Prec) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.en.INPrec, values = unlist(listOpenFiles), textvariable = input.Prec)
				}else return(NULL)
			}else{
				filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
				tclvalue(input.Prec) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
			}
		})

		############
		tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.INPrec, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.en.INPrec, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.INPrec, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		############
		infobulle(cb.datatype, 'Select the format of the input data')
		status.bar.display(cb.datatype, TextOutputVar, 'Select the format of the input data')

		if(GeneralParameters$data.type == 'cdtstation'){
			infobulle(cb.en.INPrec, 'Select the file containing the daily precipitation')
			status.bar.display(cb.en.INPrec, TextOutputVar, 'Select the file containing the daily precipitation')

			infobulle(bt.INPrec, 'Browse file if not listed')
			status.bar.display(bt.INPrec, TextOutputVar, 'Browse file if not listed')
		}else{
			infobulle(cb.en.INPrec, 'Enter the full path to the file <daily precipitation dataset name>.rds')
			status.bar.display(cb.en.INPrec, TextOutputVar, 'Enter the full path to the file <daily precipitation dataset name>.rds')

			infobulle(bt.INPrec, 'or browse here')
			status.bar.display(bt.INPrec, TextOutputVar, 'or browse here')
		}

		############

		tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
			tkdestroy(cb.en.INPrec)
			tclvalue(input.Prec) <- ''

			###
			if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
				tclvalue(txt.INPrec.var) <- 'File containing stations daily Precip data'

				cb.en.INPrec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur1)

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
					}else return(NULL)
				})

				######
				infobulle(cb.en.INPrec, 'Select the file containing the daily precipitation')
				status.bar.display(cb.en.INPrec, TextOutputVar, 'Select the file containing the daily precipitation')

				infobulle(bt.INPrec, 'Browse file if not listed')
				status.bar.display(bt.INPrec, TextOutputVar, 'Browse file if not listed')
			}

			###
			if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)'){
				tclvalue(txt.INPrec.var) <- 'Index file (*.rds) for daily Precip data'

				cb.en.INPrec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2)

				######
				tkconfigure(bt.INPrec, command = function(){
					filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
					path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
					tclvalue(input.Prec) <- if(path.rds%in%c("", "NA")) "" else path.rds
				})

				######
				infobulle(cb.en.INPrec, 'Enter the full path to the file <daily precipitation dataset name>.rds')
				status.bar.display(cb.en.INPrec, TextOutputVar, 'Enter the full path to the file <daily precipitation dataset name>.rds')

				infobulle(bt.INPrec, 'or browse here')
				status.bar.display(bt.INPrec, TextOutputVar, 'or browse here')
			}

			#######
			tkgrid(cb.en.INPrec, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		})

		############################################

		GeneralParameters$seas$all.years <- TRUE
		GeneralParameters$seas$startYear <- 1981
		GeneralParameters$seas$endYear <- 2017
		GeneralParameters$seas$startMon <- 9
		GeneralParameters$seas$startDay <- 1
		GeneralParameters$seas$endMon <- 11
		GeneralParameters$seas$endDay <- 30

		########

		frameSeas <- ttklabelframe(subfr1, text = "Season", relief = 'groove')

		allYears <- tclVar(GeneralParameters$seas$all.years)
		startYear <- tclVar(GeneralParameters$seas$startYear)
		endYear <- tclVar(GeneralParameters$seas$endYear)

		mon1 <- as.numeric(str_trim(GeneralParameters$seas$startMon))
		startMon <- tclVar(MOIS[mon1])
		mon2 <- as.numeric(str_trim(GeneralParameters$seas$endMon))
		endMon <- tclVar(MOIS[mon2])

		min.frac <- tclVar(GeneralParameters$min.frac)

		stateYear <- if(GeneralParameters$seas$all.years) 'disabled' else 'normal'

		chk.allYears <- tkcheckbutton(frameSeas, variable = allYears, text =  "Use all years from the input data", anchor = 'w', justify = 'left')
		txt.startYear <- tklabel(frameSeas, text = "Start Year", anchor = 'e', justify = 'right')
		en.startYear <- tkentry(frameSeas, textvariable = startYear, width = 6, state = stateYear)
		txt.endYear <- tklabel(frameSeas, text = "End Year", anchor = 'e', justify = 'right')
		en.endYear <- tkentry(frameSeas, textvariable = endYear, width = 6, state = stateYear)

		frMonDay <- tkframe(frameSeas)
		txt.startMon <- tklabel(frMonDay, text = "From")
		cb.startMon <- ttkcombobox(frMonDay, values = MOIS, textvariable = startMon, width = 6)
		spin.startDay <- ttkspinbox(frMonDay, from = 1, to = 31, increment = 1, justify = 'center', width = 2)
		tkset(spin.startDay, GeneralParameters$seas$startDay)

		txt.endMon <- tklabel(frMonDay, text = "to")
		cb.endMon <- ttkcombobox(frMonDay, values = MOIS, textvariable = endMon, width = 6)
		spin.endDay <- ttkspinbox(frMonDay, from = 1, to = 31, increment = 1, justify = 'center', width = 2)
		tkset(spin.endDay, GeneralParameters$seas$endDay)

		tkgrid(txt.startMon, cb.startMon, spin.startDay, txt.endMon, cb.endMon, spin.endDay)
		tkgrid.configure(txt.endMon, sticky = "we", padx = 3)

		frMinFrac <- tkframe(frameSeas)
		txt.minfrac <- tklabel(frMinFrac, text = "Minimum fraction of available data", anchor = 'w', justify = 'left')
		en.minfrac <- tkentry(frMinFrac, textvariable = min.frac, width = 5)

		tkgrid(txt.minfrac, en.minfrac)

		tkgrid(chk.allYears, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.startYear, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.startYear, row = 1, column = 2, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.endYear, row = 1, column = 3, sticky = 'e', rowspan = 1, columnspan = 3, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.endYear, row = 1, column = 6, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frMonDay, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frMinFrac, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 3, ipadx = 1, ipady = 1)

		############
		tkbind(chk.allYears, "<Button-1>", function(){
			stateYear <- if(tclvalue(allYears) == '1') 'normal' else 'disabled'
			tkconfigure(en.startYear, state = stateYear)
			tkconfigure(en.endYear, state = stateYear)
		})

		############################################

		frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		dir.save <- tclVar(GeneralParameters$output)

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

		############################################

		tkgrid(frameInData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameSeas, row = 1, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

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

		GeneralParameters$stats$daily <- 'tot.rain'
		GeneralParameters$stats$yearly <- 'mean'

		frameStats <- ttklabelframe(subfr2, text = "Statistics", relief = 'groove')

		daily.Stats <- tclVar()
		CbDailyStatsVAL <- c('Total Rainfall', 'Rainfall Intensity', 'Number of Wet Days',
							'Number of Dry Days', 'Number of Wet Spells', 'Number of Dry Spells')
		tclvalue(daily.Stats) <- switch(GeneralParameters$stats$daily,
										'tot.rain' = CbDailyStatsVAL[1],
										'rain.int' = CbDailyStatsVAL[2],
										'nb.wet.day' = CbDailyStatsVAL[3],
										'nb.dry.day' = CbDailyStatsVAL[4],
										'nb.wet.spell' = CbDailyStatsVAL[5],
										'nb.dry.spell' = CbDailyStatsVAL[6])

		yearly.Stats <- tclVar()
		CbYearlyStatsVAL <- c('Mean', 'Standard deviation', 'Coefficient of variation', 'Probability of exceeding')
		tclvalue(yearly.Stats) <- switch(GeneralParameters$stats$yearly,
										'mean' = CbYearlyStatsVAL[1],
										'stdev' = CbYearlyStatsVAL[2],
										'coefvar' = CbYearlyStatsVAL[3],
										'proba' = CbYearlyStatsVAL[4])

		txt.StatDay <- tklabel(frameStats, text = 'Seasonal daily statistics', anchor = 'w', justify = 'left')
		cb.StatDay <- ttkcombobox(frameStats, values = CbDailyStatsVAL, textvariable = daily.Stats, width = largeur3)

		txt.StatYear <- tklabel(frameStats, text = 'Yearly seasonal statistics', anchor = 'w', justify = 'left')
		cb.StatYear <- ttkcombobox(frameStats, values = CbYearlyStatsVAL, textvariable = yearly.Stats, width = largeur3)

		########
		tkgrid(txt.StatDay, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.StatDay, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.StatYear, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(tklabel(frameStats, width = 5), row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.StatYear, row = 3, column = 3, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		##############################################

		GeneralParameters$def$drywet.day <- 0.85

		frameDryDay <- ttklabelframe(subfr2, text = "Wet/Dry Day definition", relief = 'groove')

		drywet.day <- tclVar(GeneralParameters$def$drywet.day)

		txt.DryDay1 <- tklabel(frameDryDay, text = 'Rainfall amount above/below', anchor = 'w', justify = 'left')
		en.DryDay <- tkentry(frameDryDay, textvariable = drywet.day, width = 5)
		txt.DryDay2 <- tklabel(frameDryDay, text = 'mm/day', anchor = 'w', justify = 'left')

		tkgrid(txt.DryDay1, en.DryDay, txt.DryDay2)

		##############################################

		GeneralParameters$def$drywet.spell <- 7

		frameDrySpell <- ttklabelframe(subfr2, text = "Wet/Dry Spell definition", relief = 'groove')

		drywet.spell <- tclVar(GeneralParameters$def$drywet.spell)

		txt.DrySpell1 <- tklabel(frameDrySpell, text = 'Defined as', anchor = 'w', justify = 'left')
		en.DrySpell <- tkentry(frameDrySpell, textvariable = drywet.spell, width = 2)
		txt.DrySpell2 <- tklabel(frameDrySpell, text = 'continuous wet/dry days', anchor = 'w', justify = 'left')

		tkgrid(txt.DrySpell1, en.DrySpell, txt.DrySpell2)

		##############################################

		GeneralParameters$def$proba.thres <- switch(GeneralParameters$stats$daily,
													'tot.rain' = 400,
													'rain.int' = 10,
													'nb.wet.day' = 30,
													'nb.dry.day' = 30,
													'nb.wet.spell' = 5,
													'nb.dry.spell' = 5)
		txt.units.thres <- switch(GeneralParameters$stats$daily,
										'tot.rain' = 'mm',
										'rain.int' = 'mm/day',
										'nb.wet.day' = 'days',
										'nb.dry.day' = 'days',
										'nb.wet.spell' = 'spells',
										'nb.dry.spell' = 'spells')

		frameProba <- tkframe(subfr2)
		# frameProba <- ttklabelframe(subfr2, text = "Probability of exceeding", relief = 'groove')

		proba.thres <- tclVar(GeneralParameters$def$proba.thres)
		units.thres <- tclVar(txt.units.thres)
		stateProba <- if(GeneralParameters$stats$yearly == 'proba') 'normal' else 'disabled'

		txt.Proba1 <- tklabel(frameProba, text = 'Probability of exceeding', anchor = 'w', justify = 'left')
		en.Proba <- tkentry(frameProba, textvariable = proba.thres, width = 4, state = stateProba)
		txt.Proba2 <- tklabel(frameProba, text = tclvalue(units.thres), textvariable = units.thres, anchor = 'w', justify = 'left')

		tkgrid(txt.Proba1, en.Proba, txt.Proba2)

		###################

		tkbind(cb.StatYear, "<<ComboboxSelected>>", function(){
			stateProba <- if(str_trim(tclvalue(yearly.Stats)) == 'Probability of exceeding') 'normal' else 'disabled'
			tkconfigure(en.Proba, state = stateProba)

			if(str_trim(tclvalue(yearly.Stats)) == 'Probability of exceeding'){
				tclvalue(units.thres) <- switch(str_trim(tclvalue(daily.Stats)),
												'Total Rainfall' = 'mm',
												'Rainfall Intensity' = 'mm/day',
												'Number of Wet Days' = 'days',
												'Number of Dry Days' = 'days',
												'Number of Wet Spells' = 'spells',
												'Number of Dry Spells' = 'spells')

				tclvalue(proba.thres) <- switch(str_trim(tclvalue(daily.Stats)),
												'Total Rainfall' = 400,
												'Rainfall Intensity' = 10,
												'Number of Wet Days' = 30,
												'Number of Dry Days' = 30,
												'Number of Wet Spells' = 5,
												'Number of Dry Spells' = 5)
			}
		})

		tkbind(cb.StatDay, "<<ComboboxSelected>>", function(){
			if(str_trim(tclvalue(yearly.Stats)) == 'Probability of exceeding'){
				tclvalue(units.thres) <- switch(str_trim(tclvalue(daily.Stats)),
												'Total Rainfall' = 'mm',
												'Rainfall Intensity' = 'mm/day',
												'Number of Wet Days' = 'days',
												'Number of Dry Days' = 'days',
												'Number of Wet Spells' = 'spells',
												'Number of Dry Spells' = 'spells')

				tclvalue(proba.thres) <- switch(str_trim(tclvalue(daily.Stats)),
												'Total Rainfall' = 400,
												'Rainfall Intensity' = 10,
												'Number of Wet Days' = 30,
												'Number of Dry Days' = 30,
												'Number of Wet Spells' = 5,
												'Number of Dry Spells' = 5)
			}
		})

		##############################################

		frameCalc <- tkframe(subfr2)

		if(!is.null(EnvDailyRainAnalysisplot$DirExist)){
			stateCaclBut <- if(tclvalue(EnvDailyRainAnalysisplot$DirExist) == "1") "normal" else "disabled"
		}else stateCaclBut <- "normal"

		bt.CalcDaily <- ttkbutton(frameCalc, text = 'Calculate', state = stateCaclBut)

		tkconfigure(bt.CalcDaily, command = function(){
			GeneralParameters$data.type <- switch(str_trim(tclvalue(DataType)),
												'CDT stations data format' = 'cdtstation',
												'CDT dataset format (gridded)' = 'cdtdataset')

			if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
				GeneralParameters$cdtstation <- str_trim(tclvalue(input.Prec))
			}

			if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)'){
				GeneralParameters$cdtdataset <- str_trim(tclvalue(input.Prec))
			}

			GeneralParameters$min.frac <- as.numeric(str_trim(tclvalue(min.frac)))
			GeneralParameters$output <- str_trim(tclvalue(dir.save))

			GeneralParameters$seas$all.years <- switch(tclvalue(allYears), '0' = FALSE, '1' = TRUE)
			GeneralParameters$seas$startYear <- as.numeric(str_trim(tclvalue(startYear)))
			GeneralParameters$seas$endYear <- as.numeric(str_trim(tclvalue(endYear)))
			GeneralParameters$seas$startMon <- which(MOIS%in%str_trim(tclvalue(startMon)))
			GeneralParameters$seas$startDay <- as.numeric(str_trim(tclvalue(tkget(spin.startDay))))
			GeneralParameters$seas$endMon <- which(MOIS%in%str_trim(tclvalue(endMon)))
			GeneralParameters$seas$endDay <- as.numeric(str_trim(tclvalue(tkget(spin.endDay))))

			GeneralParameters$stats$daily <- switch(str_trim(tclvalue(daily.Stats)),
												'Total Rainfall' = 'tot.rain',
												'Rainfall Intensity' = 'rain.int',
												'Number of Wet Days' = 'nb.wet.day',
												'Number of Dry Days' = 'nb.dry.day',
												'Number of Wet Spells' = 'nb.wet.spell',
												'Number of Dry Spells' = 'nb.dry.spell')

			GeneralParameters$stats$yearly <- switch(str_trim(tclvalue(yearly.Stats)),
													'Mean' = 'mean',
													'Standard deviation' = 'stdev',
													'Coefficient of variation' = 'coefvar',
													'Probability of exceeding' = 'proba')

			GeneralParameters$def$drywet.day <- as.numeric(str_trim(tclvalue(drywet.day)))
			GeneralParameters$def$drywet.spell <- as.numeric(str_trim(tclvalue(drywet.spell)))
			GeneralParameters$def$proba.thres <- as.numeric(str_trim(tclvalue(proba.thres)))

			# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

			tkconfigure(main.win, cursor = 'watch')
			analysis.method <- paste(str_trim(tclvalue(daily.Stats)), ":", str_trim(tclvalue(yearly.Stats)))
			InsertMessagesTxt(main.txt.out, paste("Calculating", analysis.method, "......."))
			ret <- tryCatch(
				dailyRainAnalysisCalcProcs(GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = tkconfigure(main.win, cursor = '')
			)

			msg0 <- paste(analysis.method, "calculation finished successfully")
			msg1 <- paste(analysis.method, "calculation failed")

			if(!is.null(ret)){
				if(ret == 0){
					InsertMessagesTxt(main.txt.out, msg0)

					###################

					# load.ClimatoAnalysis.Data()

				}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
			}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
		})

		####################

		tkgrid(bt.CalcDaily, row = 0, column = 0, sticky = 'we', pady = 1)

		############################################

		tkgrid(frameStats, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameDryDay, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 2)
		tkgrid(frameDrySpell, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 2)
		tkgrid(frameProba, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 2)
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

		frameOnsetDat <- ttklabelframe(subfr3, text = "Analysis data", relief = 'groove')

		EnvDailyRainAnalysisplot$DirExist <- tclVar(0)
		file.OnsetIndex <- tclVar()

		stateOnsetDat <- if(tclvalue(EnvDailyRainAnalysisplot$DirExist) == "1") "normal" else "disabled"

		chk.OnsetIdx <- tkcheckbutton(frameOnsetDat, variable = EnvDailyRainAnalysisplot$DirExist, text = "Analysis data already computed", anchor = 'w', justify = 'left')
		en.OnsetIdx <- tkentry(frameOnsetDat, textvariable = file.OnsetIndex, width = largeur2, state = stateOnsetDat)
		bt.OnsetIdx <- tkbutton(frameOnsetDat, text = "...", state = stateOnsetDat)

		tkconfigure(bt.OnsetIdx, command = function(){

		})


		tkgrid(chk.OnsetIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.OnsetIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.OnsetIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(chk.OnsetIdx, "<Button-1>", function(){
			stateOnsetDat <- if(tclvalue(EnvDailyRainAnalysisplot$DirExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.OnsetIdx, state = stateOnsetDat)
			tkconfigure(bt.OnsetIdx, state = stateOnsetDat)
			stateCaclBut <- if(tclvalue(EnvDailyRainAnalysisplot$DirExist) == '1') 'normal' else 'disabled'
			tkconfigure(bt.CalcDaily, state = stateCaclBut)
		})

		##############################################

		frameOnsetMap <- ttklabelframe(subfr3, text = "Analysis Map", relief = 'groove')



		##############################################

		frameOnsetTS <- ttklabelframe(subfr3, text = "Analysis Graph", relief = 'groove')



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


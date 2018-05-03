
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
		largeur4 <- 28
		largeur5 <- 21
		# largeur6 <- 22
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(47)
		hscrlwin1 <- h.scale(28)
		largeur0 <- as.integer(w.scale(20)/sfont0)
		largeur1 <- as.integer(w.scale(21)/sfont0)
		largeur2 <- as.integer(w.scale(23)/sfont0)
		largeur3 <- 24
		largeur4 <- 22
		largeur5 <- 14
		# largeur6 <- 14
	}

	# GeneralParameters <- fromJSON(file.path(apps.dir, 'init_params', 'ClimatoAnalysis.json'))
	MOIS <- format(ISOdate(2014, 1:12, 1), "%b")
	varsname <- list(name = c("TOTALRAIN", "RAININT", "WETDAY", "DRYDAY", "WETSPELL", "DRYSPELL"),
					longname = c('Total Rainfall', 'Rainfall Intensity', 'Number of Wet Days',
					'Number of Dry Days', 'Number of Wet Spells', 'Number of Dry Spells'))
	statsname <- list(name = c('mean', 'stdev', 'coefvar', 'proba'),
					longname = c('Mean', 'Standard deviation', 'Coefficient of variation',
								'Probability of exceeding'))

	GeneralParameters <- list(data.type = "cdtstation", cdtstation = "", cdtdataset = "",
							min.frac = 0.95, output = "")

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Analysis")
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
					set.Data.VarStat.Dates_1st()
					widgets.Station.Pixel()
					res1 <- EnvDailyRainAnalysisplot$read.Data.MapVarStat()
					if(inherits(res1, "try-error") | is.null(res1)) return(NULL)
					res2 <- EnvDailyRainAnalysisplot$read.Data.MapVarTS()
					if(inherits(res2, "try-error") | is.null(res2)) return(NULL)
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

		frameDataExist <- ttklabelframe(subfr3, text = "Analysis data", relief = 'groove')

		EnvDailyRainAnalysisplot$DirExist <- tclVar(0)
		file.dataIndex <- tclVar()

		stateExistData <- if(tclvalue(EnvDailyRainAnalysisplot$DirExist) == "1") "normal" else "disabled"

		chk.dataIdx <- tkcheckbutton(frameDataExist, variable = EnvDailyRainAnalysisplot$DirExist, text = "Analysis data already computed", anchor = 'w', justify = 'left')
		en.dataIdx <- tkentry(frameDataExist, textvariable = file.dataIndex, width = largeur2, state = stateExistData)
		bt.dataIdx <- tkbutton(frameDataExist, text = "...", state = stateExistData)

		tkconfigure(bt.dataIdx, command = function(){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.dataIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			if(path.dataIdx%in%c("", "NA") | is.na(path.dataIdx)) return(NULL)
			tclvalue(file.dataIndex) <- path.dataIdx

			if(file.exists(str_trim(tclvalue(file.dataIndex)))){
				OutIndexdata <- try(readRDS(str_trim(tclvalue(file.dataIndex))), silent = TRUE)
				if(inherits(OutIndexdata, "try-error")){
					InsertMessagesTxt(main.txt.out, 'Unable to load daily rainfall analysis data', format = TRUE)
					InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', OutIndexdata[1]), format = TRUE)

					tkconfigure(cb.varstat.var, values = "")
					tclvalue(EnvDailyRainAnalysisplot$anaVars) <- ""
					tkconfigure(cb.varstat.stat, values = "")
					tclvalue(EnvDailyRainAnalysisplot$anaStat) <- ""
					tkconfigure(cb.data.Index, values = "")
					tclvalue(EnvDailyRainAnalysisplot$donDate) <- ""
					return(NULL)
				}

				EnvDailyRainAnalysisplot$output <- OutIndexdata
				EnvDailyRainAnalysisplot$PathData <- dirname(str_trim(tclvalue(file.dataIndex)))

				###################
				set.Data.VarStat.Dates_1st()
				widgets.Station.Pixel()
				ret1 <- EnvDailyRainAnalysisplot$read.Data.MapVarStat()
				if(inherits(ret1, "try-error") | is.null(ret1)) return(NULL)
				ret2 <- EnvDailyRainAnalysisplot$read.Data.MapVarTS()
				if(inherits(ret2, "try-error") | is.null(ret2)) return(NULL)
			}
		})

		tkgrid(chk.dataIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.dataIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.dataIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(chk.dataIdx, "<Button-1>", function(){
			stateExistData <- if(tclvalue(EnvDailyRainAnalysisplot$DirExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.dataIdx, state = stateExistData)
			tkconfigure(bt.dataIdx, state = stateExistData)
			stateCaclBut <- if(tclvalue(EnvDailyRainAnalysisplot$DirExist) == '1') 'normal' else 'disabled'
			tkconfigure(bt.CalcDaily, state = stateCaclBut)
		})

		##############################################

		frameDataStatMap <- ttklabelframe(subfr3, text = "Statistics Maps", relief = 'groove')

		EnvDailyRainAnalysisplot$anaVars <- tclVar()
		EnvDailyRainAnalysisplot$anaStat <- tclVar()

		cb.varstat.var <- ttkcombobox(frameDataStatMap, values = "", textvariable = EnvDailyRainAnalysisplot$anaVars, width = largeur4)
		bt.varstat.maps <- ttkbutton(frameDataStatMap, text = "PLOT")
		cb.varstat.stat <- ttkcombobox(frameDataStatMap, values = "", textvariable = EnvDailyRainAnalysisplot$anaStat, width = largeur4)
		bt.varstat.MapOpt <- ttkbutton(frameDataStatMap, text = "Options")

		###################

		EnvDailyRainAnalysisplot$varstatMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
												userCol = list(custom = FALSE, color = NULL),
												userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
												title = list(user = FALSE, title = ''),
												colkeyLab = list(user = FALSE, label = ''),
												scalebar = list(add = FALSE, pos = 'bottomleft'))

		tkconfigure(bt.varstat.MapOpt, command = function(){
			if(!is.null(EnvDailyRainAnalysisplot$varData$map)){
				atlevel <- pretty(EnvDailyRainAnalysisplot$varData$map$z, n = 10, min.n = 7)
				if(is.null(EnvDailyRainAnalysisplot$dataMapOp$userLvl$levels)){
					EnvDailyRainAnalysisplot$dataMapOp$userLvl$levels <- atlevel
				}else{
					if(!EnvDailyRainAnalysisplot$dataMapOp$userLvl$custom)
						EnvDailyRainAnalysisplot$dataMapOp$userLvl$levels <- atlevel
				}
			}
			EnvDailyRainAnalysisplot$varstatMapOp <- climatoAnalysis.MapOptions(main.win, EnvDailyRainAnalysisplot$varstatMapOp)
		})

		###################

		EnvDailyRainAnalysisplot$notebookTab.dataMapStat <- NULL

		tkconfigure(bt.varstat.maps, command = function(){
			if(str_trim(tclvalue(EnvDailyRainAnalysisplot$anaVars)) != "" &
				str_trim(tclvalue(EnvDailyRainAnalysisplot$anaStat)) != "")
			{
				ret <- try(EnvDailyRainAnalysisplot$read.Data.MapVarStat(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				imgContainer <- dailyRainAnalysis.Display.MapsVarStats(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvDailyRainAnalysisplot$notebookTab.dataMapStat, AllOpenTabType, AllOpenTabData)
				EnvDailyRainAnalysisplot$notebookTab.dataMapStat <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		###################

		tkgrid(cb.varstat.var, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.varstat.maps, row = 0, column = 4, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.varstat.stat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.varstat.MapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		###################

		tkbind(cb.varstat.var, "<<ComboboxSelected>>", function(){
			vars <- str_trim(tclvalue(EnvDailyRainAnalysisplot$anaVars))
			if(vars == "") return(NULL)
			varstats <- EnvDailyRainAnalysisplot$output$exist.vars.dates
			statsval <- varstats[[varsname$name[which(varsname$longname == vars)]]]

			STATSVAL <- statsname$longname[statsname$name%in%names(statsval)]
			if(length(STATSVAL) == 1) STATSVAL <- c(STATSVAL, "")
			tkconfigure(cb.varstat.stat, values = STATSVAL)
			tclvalue(EnvDailyRainAnalysisplot$anaStat) <- STATSVAL[1]

			tkconfigure(cb.data.Index, values = statsval$date)
			tclvalue(EnvDailyRainAnalysisplot$donDate) <- statsval$date[length(statsval$date)]
			return(0)
		})

		##############################################

		frameDataMap <- ttklabelframe(subfr3, text = "Seasonal Maps", relief = 'groove')

		EnvDailyRainAnalysisplot$donDate <- tclVar()

		cb.data.Index <- ttkcombobox(frameDataMap, values = "", textvariable = EnvDailyRainAnalysisplot$donDate, width = largeur5)
		bt.data.Index.prev <- ttkbutton(frameDataMap, text = "<<", width = 3)
		bt.data.Index.next <- ttkbutton(frameDataMap, text = ">>", width = 3)
		bt.data.maps <- ttkbutton(frameDataMap, text = "PLOT", width = 7)
		bt.data.MapOpt <- ttkbutton(frameDataMap, text = "Options", width = 7)

		###############

		EnvDailyRainAnalysisplot$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
												userCol = list(custom = FALSE, color = NULL),
												userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
												title = list(user = FALSE, title = ''),
												colkeyLab = list(user = FALSE, label = ''),
												scalebar = list(add = FALSE, pos = 'bottomleft'))

		tkconfigure(bt.data.MapOpt, command = function(){
			if(!is.null(EnvDailyRainAnalysisplot$varData$map)){
				atlevel <- pretty(EnvDailyRainAnalysisplot$varData$map$z, n = 10, min.n = 7)
				if(is.null(EnvDailyRainAnalysisplot$dataMapOp$userLvl$levels)){
					EnvDailyRainAnalysisplot$dataMapOp$userLvl$levels <- atlevel
				}else{
					if(!EnvDailyRainAnalysisplot$dataMapOp$userLvl$custom)
						EnvDailyRainAnalysisplot$dataMapOp$userLvl$levels <- atlevel
				}
			}
			EnvDailyRainAnalysisplot$dataMapOp <- climatoAnalysis.MapOptions(main.win, EnvDailyRainAnalysisplot$dataMapOp)
		})

		###############

		EnvDailyRainAnalysisplot$notebookTab.dataMapTS <- NULL

		tkconfigure(bt.data.maps, command = function(){
			if(str_trim(tclvalue(EnvDailyRainAnalysisplot$donDate)) != "" &
				!is.null(EnvDailyRainAnalysisplot$tsData))
			{
				imgContainer <- dailyRainAnalysis.Display.MapVarTS(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvDailyRainAnalysisplot$notebookTab.dataMapTS, AllOpenTabType, AllOpenTabData)
				EnvDailyRainAnalysisplot$notebookTab.dataMapTS <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.data.Index.prev, command = function(){
			if(str_trim(tclvalue(EnvDailyRainAnalysisplot$donDate)) != ""){
				vars <- str_trim(tclvalue(EnvDailyRainAnalysisplot$anaVars))
				this.vars <- varsname$name[which(varsname$longname == vars)]
				donDates <- EnvDailyRainAnalysisplot$output$exist.vars.dates[[this.vars]]$date
				idaty <- which(donDates == str_trim(tclvalue(EnvDailyRainAnalysisplot$donDate)))
				idaty <- idaty-1
				if(idaty < 1) idaty <- length(donDates)
				tclvalue(EnvDailyRainAnalysisplot$donDate) <- donDates[idaty]

				ret <- try(EnvDailyRainAnalysisplot$read.Data.MapVarTS(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				imgContainer <- dailyRainAnalysis.Display.MapVarTS(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvDailyRainAnalysisplot$notebookTab.dataMapTS, AllOpenTabType, AllOpenTabData)
				EnvDailyRainAnalysisplot$notebookTab.dataMapTS <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.data.Index.next, command = function(){
			if(str_trim(tclvalue(EnvDailyRainAnalysisplot$donDate)) != ""){
				vars <- str_trim(tclvalue(EnvDailyRainAnalysisplot$anaVars))
				this.vars <- varsname$name[which(varsname$longname == vars)]
				donDates <- EnvDailyRainAnalysisplot$output$exist.vars.dates[[this.vars]]$date
				idaty <- which(donDates == str_trim(tclvalue(EnvDailyRainAnalysisplot$donDate)))
				idaty <- idaty+1
				if(idaty > length(donDates)) idaty <- 1
				tclvalue(EnvDailyRainAnalysisplot$donDate) <- donDates[idaty]

				ret <- try(EnvDailyRainAnalysisplot$read.Data.MapVarTS(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				imgContainer <- dailyRainAnalysis.Display.MapVarTS(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvDailyRainAnalysisplot$notebookTab.dataMapTS, AllOpenTabType, AllOpenTabData)
				EnvDailyRainAnalysisplot$notebookTab.dataMapTS <- retNBTab$notebookTab
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
			if(!is.null(EnvDailyRainAnalysisplot$tsData)){
				ret <- try(EnvDailyRainAnalysisplot$read.Data.MapVarTS(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		##############################################

		tkgrid(frameDataExist, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameDataStatMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameDataMap, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

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

		frameDataTS <- ttklabelframe(subfr4, text = "Analysis Graph", relief = 'groove')

		typeTSPLOT <- c("Line", "Barplot", "Probability", "Anomaly")
		EnvDailyRainAnalysisplot$graph$typeTSp <- tclVar("Line")
		EnvDailyRainAnalysisplot$graph$averageTSp <- tclVar(FALSE)
		EnvDailyRainAnalysisplot$graph$tercileTSp <- tclVar(FALSE)
		EnvDailyRainAnalysisplot$graph$trendTSp <- tclVar(FALSE)

		stateType <- if(str_trim(tclvalue(EnvDailyRainAnalysisplot$graph$typeTSp)) == "Line") "normal" else "disabled"

		cb.typeTSp <- ttkcombobox(frameDataTS, values = typeTSPLOT, textvariable = EnvDailyRainAnalysisplot$graph$typeTSp, width = largeur5)
		bt.TsGraph.plot <- ttkbutton(frameDataTS, text = "PLOT", width = 7)
		bt.TSGraphOpt <- ttkbutton(frameDataTS, text = "Options", width = 8)

		frTS1 <- tkframe(frameDataTS)
		chk.meanTSp <- tkcheckbutton(frTS1, variable = EnvDailyRainAnalysisplot$graph$averageTSp, text = "Add Mean", anchor = 'w', justify = 'left', state = stateType)
		chk.tercTSp <- tkcheckbutton(frTS1, variable = EnvDailyRainAnalysisplot$graph$tercileTSp, text = "Add Terciles", anchor = 'w', justify = 'left', state = stateType)
		chk.trendTSp <- tkcheckbutton(frTS1, variable = EnvDailyRainAnalysisplot$graph$trendTSp, text = "Add Trend", anchor = 'w', justify = 'left', state = stateType)
		tkgrid(chk.meanTSp, chk.tercTSp, chk.trendTSp)

		#################

		EnvDailyRainAnalysisplot$TSGraphOp <- list(
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
						proba = list(theoretical = FALSE, col = 'black', lwd = 2)
					)
				)

		tkconfigure(bt.TSGraphOpt, command = function(){
			suffix.fun <- switch(tclvalue(EnvDailyRainAnalysisplot$graph$typeTSp),
									"Anomaly" = "Anomaly",
									"Barplot" = "Bar",
									"Line" = "Line",
									"Probability" = "Proba")
			plot.fun <- match.fun(paste0("climatoAnalysis.GraphOptions.", suffix.fun))
			EnvDailyRainAnalysisplot$TSGraphOp <- plot.fun(main.win, EnvDailyRainAnalysisplot$TSGraphOp)
		})

		#################

		EnvDailyRainAnalysisplot$notebookTab.dataGraph <- NULL

		tkconfigure(bt.TsGraph.plot, command = function(){
			if(!is.null(EnvDailyRainAnalysisplot$tsData)){
				imgContainer <- dailyRainAnalysis.Display.VarGraph(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvDailyRainAnalysisplot$notebookTab.dataGraph, AllOpenTabType, AllOpenTabData)
				EnvDailyRainAnalysisplot$notebookTab.dataGraph <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		#################

		tkgrid(cb.typeTSp, row = 0, column = 0, sticky = 'we', pady = 3, columnspan = 1)
		tkgrid(bt.TSGraphOpt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
		tkgrid(bt.TsGraph.plot, row = 0, column = 2, sticky = 'we', pady = 3, columnspan = 1)
		tkgrid(frTS1, row = 1, column = 0, sticky = 'we', pady = 3, columnspan = 3)

		#################

		tkbind(cb.typeTSp, "<<ComboboxSelected>>", function(){
			stateType <- if(str_trim(tclvalue(EnvDailyRainAnalysisplot$graph$typeTSp)) == "Line") "normal" else "disabled"
			tkconfigure(chk.meanTSp, state = stateType)
			tkconfigure(chk.tercTSp, state = stateType)
			tkconfigure(chk.trendTSp, state = stateType)
		})

		tkbind(chk.meanTSp, "<Button-1>", function(){
			EnvDailyRainAnalysisplot$TSGraphOp$line$legend$add$mean <- 
						if(tclvalue(EnvDailyRainAnalysisplot$graph$averageTSp) == '0') TRUE else FALSE
			EnvDailyRainAnalysisplot$TSGraphOp$line.enso$legend$add$mean <- 
						if(tclvalue(EnvDailyRainAnalysisplot$graph$averageTSp) == '0') TRUE else FALSE
		})

		tkbind(chk.tercTSp, "<Button-1>", function(){
			EnvDailyRainAnalysisplot$TSGraphOp$line$legend$add$tercile <- 
						if(tclvalue(EnvDailyRainAnalysisplot$graph$tercileTSp) == '0') TRUE else FALSE
			EnvDailyRainAnalysisplot$TSGraphOp$line.enso$legend$add$tercile <- 
						if(tclvalue(EnvDailyRainAnalysisplot$graph$tercileTSp) == '0') TRUE else FALSE
		})

		tkbind(chk.trendTSp, "<Button-1>", function(){
			EnvDailyRainAnalysisplot$TSGraphOp$line$legend$add$linear <- 
						if(tclvalue(EnvDailyRainAnalysisplot$graph$trendTSp) == '0') TRUE else FALSE
			EnvDailyRainAnalysisplot$TSGraphOp$line.enso$legend$add$linear <- 
						if(tclvalue(EnvDailyRainAnalysisplot$graph$trendTSp) == '0') TRUE else FALSE
		})

		##############################################

		frameSTNCrds <- ttklabelframe(subfr4, text = "Station/Coordinates", relief = 'groove')

		frTS2 <- tkframe(frameSTNCrds)
		EnvDailyRainAnalysisplot$graph$lonLOC <- tclVar()
		EnvDailyRainAnalysisplot$graph$latLOC <- tclVar()
		EnvDailyRainAnalysisplot$graph$stnIDTSp <- tclVar()

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

		##############################################

		tkgrid(frameDataTS, row = 0, column = 0, sticky = 'we', pady = 1)
		tkgrid(frameSTNCrds, row = 1, column = 0, sticky = 'we', pady = 3)

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

		EnvDailyRainAnalysisplot$shp$add.shp <- tclVar(FALSE)
		file.plotShp <- tclVar()
		stateSHP <- "disabled"

		chk.addshp <- tkcheckbutton(frameSHP, variable = EnvDailyRainAnalysisplot$shp$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
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
				if(is.null(shpofile)) EnvDailyRainAnalysisplot$shp$ocrds <- NULL
				EnvDailyRainAnalysisplot$shp$ocrds <- getBoundaries(shpofile[[2]])
			}else return(NULL)
		})

		########
		EnvDailyRainAnalysisplot$SHPOp <- list(col = "black", lwd = 1.5)

		tkconfigure(bt.addshpOpt, command = function(){
			EnvDailyRainAnalysisplot$SHPOp <- climatoAnalysis.GraphOptions.LineSHP(main.win, EnvDailyRainAnalysisplot$SHPOp)
		})

		########
		tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
		tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		#################
		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile)) EnvDailyRainAnalysisplot$shp$ocrds <- NULL
			EnvDailyRainAnalysisplot$shp$ocrds <- getBoundaries(shpofile[[2]])
		})

		tkbind(chk.addshp, "<Button-1>", function(){
			stateSHP <- if(tclvalue(EnvDailyRainAnalysisplot$shp$add.shp) == "1") "disabled" else "normal"
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

		if(EnvDailyRainAnalysisplot$output$params$data.type == "cdtstation"){
			stnIDTSPLOT <- EnvDailyRainAnalysisplot$output$data$id
			txt.stnSel <- tklabel(frTS2, text = "Select a station to plot", anchor = 'w', justify = 'left')
			txt.stnID <- tklabel(frTS2, text = "Station", anchor = 'e', justify = 'right')
			cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = EnvDailyRainAnalysisplot$graph$stnIDTSp, width = largeur4)
			tclvalue(EnvDailyRainAnalysisplot$graph$stnIDTSp) <- stnIDTSPLOT[1]

			tkgrid(txt.stnSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.stnID, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}else{
			txt.crdSel <- tklabel(frTS2, text = "Enter longitude and latitude to plot", anchor = 'w', justify = 'left')
			txt.lonLoc <- tklabel(frTS2, text = "Longitude", anchor = 'e', justify = 'right')
			en.lonLoc <- tkentry(frTS2, textvariable = EnvDailyRainAnalysisplot$graph$lonLOC, width = 8)
			txt.latLoc <- tklabel(frTS2, text = "Latitude", anchor = 'e', justify = 'right')
			en.latLoc <- tkentry(frTS2, textvariable = EnvDailyRainAnalysisplot$graph$latLOC, width = 8)
			stnIDTSPLOT <- ""
			tclvalue(EnvDailyRainAnalysisplot$graph$stnIDTSp) <- ""

			tkgrid(txt.crdSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.lonLoc, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.lonLoc, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.latLoc, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.latLoc, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)
		return(0)
	}

	###################

	set.Data.VarStat.Dates_1st <- function(){
		varstats <- EnvDailyRainAnalysisplot$output$exist.vars.dates
		if(length(names(varstats)) == 0) return(NULL)

		VARSVAL <- varsname$longname[varsname$name%in%names(varstats)]
		if(length(VARSVAL) == 1) VARSVAL <- c(VARSVAL, "")
		tkconfigure(cb.varstat.var, values = VARSVAL)
		last.vars <- varsname$longname[which(varsname$name == EnvDailyRainAnalysisplot$output$last[1])]
		tclvalue(EnvDailyRainAnalysisplot$anaVars) <- last.vars

		statsval <- varstats[[EnvDailyRainAnalysisplot$output$last[1]]]
		STATSVAL <- statsname$longname[statsname$name%in%names(statsval)]
		if(length(STATSVAL) == 1) STATSVAL <- c(STATSVAL, "")
		tkconfigure(cb.varstat.stat, values = STATSVAL)
		last.stats <- statsname$longname[which(statsname$name == EnvDailyRainAnalysisplot$output$last[2])]
		tclvalue(EnvDailyRainAnalysisplot$anaStat) <- last.stats

		tkconfigure(cb.data.Index, values = statsval$date)
		tclvalue(EnvDailyRainAnalysisplot$donDate) <- statsval$date[length(statsval$date)]

		return(0)
	}

	#######################################################################################################

	EnvDailyRainAnalysisplot$read.Data.MapVarStat <- function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		vars <- str_trim(tclvalue(EnvDailyRainAnalysisplot$anaVars))
		stats <- str_trim(tclvalue(EnvDailyRainAnalysisplot$anaStat))
		this.vars <- varsname$name[which(varsname$longname == vars)]
		this.stats <- statsname$name[which(statsname$longname == stats)]
		
		if(vars == "" | stats == "") return(NULL)

		if(EnvDailyRainAnalysisplot$output$params$data.type == "cdtstation"){
			filePathData <- file.path(EnvDailyRainAnalysisplot$PathData, "CDTDATASET", paste0(this.vars, "_", this.stats, ".rds"))
			if(!file.exists(filePathData)){
				InsertMessagesTxt(main.txt.out, paste(filePathData, 'not found'), format = TRUE)
				return(NULL)
			}

			readVarData <- TRUE
			if(!is.null(EnvDailyRainAnalysisplot$statData))
				if(!is.null(EnvDailyRainAnalysisplot$statData$filePathData))
					if(EnvDailyRainAnalysisplot$statData$filePathData == filePathData) readVarData <- FALSE

			if(readVarData){
				EnvDailyRainAnalysisplot$statData$data <- readRDS(filePathData)
				nx <- nx_ny_as.image(diff(range(EnvDailyRainAnalysisplot$output$data$lon)))
				ny <- nx_ny_as.image(diff(range(EnvDailyRainAnalysisplot$output$data$lat)))
				tmp <- cdt.as.image(EnvDailyRainAnalysisplot$statData$data, nx = nx, ny = ny,
								pts.xy = cbind(EnvDailyRainAnalysisplot$output$data$lon, EnvDailyRainAnalysisplot$output$data$lat))
				EnvDailyRainAnalysisplot$statData$map$x <- tmp$x
				EnvDailyRainAnalysisplot$statData$map$y <- tmp$y
				EnvDailyRainAnalysisplot$statData$map$z <- tmp$z
				rm(tmp)

				EnvDailyRainAnalysisplot$statData$filePathData <- filePathData
			}
		}else{
			filePathData <- file.path(EnvDailyRainAnalysisplot$PathData, "DATA_NetCDF_STATS", paste0(this.vars, "_", this.stats, ".nc"))
			if(!file.exists(filePathData)){
				InsertMessagesTxt(main.txt.out, paste(filePathData, 'not found'), format = TRUE)
				return(NULL)
			}

			readVarData <- TRUE
			if(!is.null(EnvDailyRainAnalysisplot$statData))
				if(!is.null(EnvDailyRainAnalysisplot$statData$filePathData))
					if(EnvDailyRainAnalysisplot$statData$filePathData == filePathData) readVarData <- FALSE

			if(readVarData){
				nc <- nc_open(filePathData)
				EnvDailyRainAnalysisplot$statData$map$x <- nc$dim[[1]]$vals
				EnvDailyRainAnalysisplot$statData$map$y <- nc$dim[[2]]$vals
				EnvDailyRainAnalysisplot$statData$map$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
				nc_close(nc)
				EnvDailyRainAnalysisplot$statData$filePathData <- filePathData
			}
		}

		EnvDailyRainAnalysisplot$now$this.vars <- this.vars
		EnvDailyRainAnalysisplot$now$this.stats <- this.stats

		return(0)
	}

	###################

	EnvDailyRainAnalysisplot$read.Data.MapVarTS <- function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		vars <- str_trim(tclvalue(EnvDailyRainAnalysisplot$anaVars))
		this.vars <- varsname$name[which(varsname$longname == vars)]
		this.daty <- str_trim(tclvalue(EnvDailyRainAnalysisplot$donDate))

		if(vars == "" | this.daty == "") return(NULL)

		if(EnvDailyRainAnalysisplot$output$params$data.type == "cdtstation"){
			filePathData <- file.path(EnvDailyRainAnalysisplot$PathData, this.vars, paste0(this.vars, ".rds"))
			if(!file.exists(filePathData)){
				InsertMessagesTxt(main.txt.out, paste(filePathData, 'not found'), format = TRUE)
				return(NULL)
			}

			readVarData <- TRUE
			if(!is.null(EnvDailyRainAnalysisplot$tsData))
				if(!is.null(EnvDailyRainAnalysisplot$tsData$filePathData))
					if(EnvDailyRainAnalysisplot$tsData$filePathData == filePathData) readVarData <- FALSE

			if(readVarData){
				EnvDailyRainAnalysisplot$tsData$data <- readRDS(filePathData)
				EnvDailyRainAnalysisplot$tsData$filePathData <- filePathData
			}

			########
			rasterVarData <- TRUE
			if(!rasterVarData)
				if(!is.null(EnvDailyRainAnalysisplot$tsData$rasterDate))
					if(EnvDailyRainAnalysisplot$tsData$filePathData == filePathData)
						if(EnvDailyRainAnalysisplot$tsData$rasterDate == this.daty) rasterVarData <- FALSE

			if(rasterVarData){
				idt <- which(EnvDailyRainAnalysisplot$output$exist.vars.dates[[this.vars]]$date == this.daty)
				nx <- nx_ny_as.image(diff(range(EnvDailyRainAnalysisplot$output$data$lon)))
				ny <- nx_ny_as.image(diff(range(EnvDailyRainAnalysisplot$output$data$lat)))
				tmp <- cdt.as.image(as.numeric(EnvDailyRainAnalysisplot$tsData$data[idt, ]), nx = nx, ny = ny,
								pts.xy = cbind(EnvDailyRainAnalysisplot$output$data$lon, EnvDailyRainAnalysisplot$output$data$lat))
				EnvDailyRainAnalysisplot$tsData$map$x <- tmp$x
				EnvDailyRainAnalysisplot$tsData$map$y <- tmp$y
				EnvDailyRainAnalysisplot$tsData$map$z <- tmp$z
				EnvDailyRainAnalysisplot$tsData$rasterDate <- this.daty
				rm(tmp)
			}
		}else{
			filePathData <- file.path(EnvDailyRainAnalysisplot$PathData, this.vars, "DATA_NetCDF", paste0("Seas_", this.daty, ".nc"))
			if(!file.exists(filePathData)){
				InsertMessagesTxt(main.txt.out, paste(filePathData, 'not found'), format = TRUE)
				return(NULL)
			}

			readVarData <- TRUE
			if(!is.null(EnvDailyRainAnalysisplot$tsData))
				if(!is.null(EnvDailyRainAnalysisplot$tsData$filePathData))
					if(EnvDailyRainAnalysisplot$tsData$filePathData == filePathData) readVarData <- FALSE

			if(readVarData){
				nc <- nc_open(filePathData)
				EnvDailyRainAnalysisplot$tsData$map$x <- nc$dim[[1]]$vals
				EnvDailyRainAnalysisplot$tsData$map$y <- nc$dim[[2]]$vals
				EnvDailyRainAnalysisplot$tsData$map$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
				nc_close(nc)
				EnvDailyRainAnalysisplot$tsData$filePathData <- filePathData
			}

			###################

			file.CDT.Idx <- file.path(EnvDailyRainAnalysisplot$PathData, this.vars, paste0(this.vars, ".rds"))

			read.cdt.dataIdx<- TRUE
			if(!is.null(EnvDailyRainAnalysisplot$cdtdataset))
				if(!is.null(EnvDailyRainAnalysisplot$file.CDT.Idx))
					if(EnvDailyRainAnalysisplot$file.CDT.Idx == file.CDT.Idx) read.cdt.dataIdx <- FALSE

			if(read.cdt.dataIdx){
				EnvDailyRainAnalysisplot$cdtdataset <- readRDS(file.CDT.Idx)
				EnvDailyRainAnalysisplot$cdtdataset$fileInfo <- file.CDT.Idx
				EnvDailyRainAnalysisplot$file.CDT.Idx <- file.CDT.Idx
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


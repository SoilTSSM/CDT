
climatoAnalysisPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(46)
		largeur0 <- as.integer(w.scale(22)/sfont0)
		largeur1 <- as.integer(w.scale(27)/sfont0)
		largeur2 <- as.integer(w.scale(29)/sfont0)
		largeur3 <- largeur2-10
		largeur4 <- largeur1-5
		largeur5 <- 28
		largeur6 <- 21
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(47)
		largeur0 <- as.integer(w.scale(20)/sfont0)
		largeur1 <- as.integer(w.scale(21)/sfont0)
		largeur2 <- as.integer(w.scale(23)/sfont0)
		largeur3 <- largeur2+2
		largeur4 <- largeur1
		largeur5 <- 22
		largeur6 <- 14
	}

	GeneralParameters <- fromJSON(file.path(apps.dir, 'init_params', 'ClimatoAnalysis.json'))
	MOIS <- format(ISOdate(2014, 1:12, 1), "%B")

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

		#######################

		frameTimeS <- ttklabelframe(subfr1, text = "Time step of input data", relief = 'groove')

		timeSteps <- tclVar()
		CbperiodVAL <- c('Daily data', 'Pentad data', 'Dekadal data', 'Monthly data')
		tclvalue(timeSteps) <- switch(GeneralParameters$in.tstep, 
										'daily' = CbperiodVAL[1],
										'pentad' = CbperiodVAL[2],
										'dekadal' = CbperiodVAL[3],
										'monthly' = CbperiodVAL[4])

		cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = timeSteps, width = largeur1)

		tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.fperiod, 'Select the time step of the data')
		status.bar.display(cb.fperiod, TextOutputVar, 'Select the time step of the data')

		#######################

		frameData <- ttklabelframe(subfr1, text = "Input Data", relief = 'groove')

		DataType <- tclVar()
		CbdatatypeVAL <- c('CDT stations data format', 'CDT dataset format (gridded)')
		tclvalue(DataType) <- switch(GeneralParameters$data.type,
									'cdtstation' = CbdatatypeVAL[1], 
									'cdtdataset' = CbdatatypeVAL[2])
		file.stnfl <- tclVar(GeneralParameters$in.file)
		fileINdir <- tclVar('File containing stations input data')

		txt.datatype <- tklabel(frameData, text = "Format", anchor = 'e', justify = 'right')
		cb.datatype <- ttkcombobox(frameData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)
		txt.stnfl <- tklabel(frameData, text = tclvalue(fileINdir), textvariable = fileINdir, anchor = 'w', justify = 'left')
		cb.stnfl <- ttkcombobox(frameData, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
		bt.stnfl <- tkbutton(frameData, text = "...")

		###############
		tkconfigure(bt.stnfl, command = function(){
			dat.opfiles <- getOpenFiles(main.win, all.opfiles)
			if(!is.null(dat.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'ascii'
				AllOpenFilesData[[nopf+1]] <<- dat.opfiles

				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
				tclvalue(file.stnfl) <- AllOpenFilesData[[nopf+1]][[1]]

				tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
				tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = file.plotShp)
			}else return(NULL)
		})

		###############
		tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.datatype, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.stnfl, row = 2, column = 6, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		infobulle(cb.datatype, 'Select the type of input data')
		status.bar.display(cb.datatype, TextOutputVar, 'Select the type of input data')
		infobulle(cb.stnfl, 'Select the file in the list')
		status.bar.display(cb.stnfl, TextOutputVar, 'Select the file containing the input data')
		infobulle(bt.stnfl, 'Browse file if not listed')
		status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')

		###############

		tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
			tkdestroy(cb.stnfl)
			tclvalue(file.stnfl) <- ''

			if(tclvalue(DataType) == 'CDT stations data format'){
				tclvalue(fileINdir) <- 'File containing stations input data'

				cb.stnfl <- ttkcombobox(frameData, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)

				#######
				tkconfigure(bt.stnfl, command = function(){
					dat.opfiles <- getOpenFiles(main.win, all.opfiles)
					if(!is.null(dat.opfiles)){
						nopf <- length(AllOpenFilesType)
						AllOpenFilesType[[nopf+1]] <<- 'ascii'
						AllOpenFilesData[[nopf+1]] <<- dat.opfiles

						listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
						tclvalue(file.stnfl) <- AllOpenFilesData[[nopf+1]][[1]]
						tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
					}else return(NULL)
				})

				infobulle(cb.stnfl, 'Select the file in the list')
				status.bar.display(cb.stnfl, TextOutputVar, 'Select the file containing the input data')
			}

			if(tclvalue(DataType) == 'CDT dataset format (gridded)'){
				tclvalue(fileINdir) <- 'Index file (*.rds) of the dataset'

				cb.stnfl <- tkentry(frameData, textvariable = file.stnfl, width = largeur2)

				#######
				tkconfigure(bt.stnfl, command = function(){
					filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
					path.dataset <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
					if(path.dataset == "") return(NULL)
					tclvalue(file.stnfl) <- path.dataset
				})

				infobulle(cb.stnfl, 'Enter the full path to the index file <dataset name>.rds')
				status.bar.display(cb.stnfl, TextOutputVar, 'Enter the full path to the index file <dataset name>.rds')
			}
			
			#######
			tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		})

		#############################

		frameAggr <- ttklabelframe(subfr1, text = "Time series aggregation", relief = 'groove')

		aggr.fun <- tclVar(GeneralParameters$aggr.series$aggr.fun)
		min.frac <- tclVar(GeneralParameters$aggr.series$min.frac)
		opr.fun <- tclVar(GeneralParameters$aggr.series$opr.fun)
		opr.thres <- tclVar(GeneralParameters$aggr.series$opr.thres)

		stateo1 <- if(str_trim(GeneralParameters$aggr.series$aggr.fun) == "count") 'readonly' else 'disabled'
		stateo2 <- if(str_trim(GeneralParameters$aggr.series$aggr.fun) == "count") 'normal' else 'disabled'

		txt.aggfun <- tklabel(frameAggr, text = 'Function', anchor = 'w', justify = 'left')
		cb.aggfun <- ttkcombobox(frameAggr, values = c("mean", "sum", "count"), textvariable = aggr.fun, width = 6, state = 'readonly')
		txt.minfrac <- tklabel(frameAggr, text = 'Min.Frac', anchor = 'w', justify = 'left')
		en.minfrac <- tkentry(frameAggr, textvariable = min.frac, width = 6)
		txt.opfun <- tklabel(frameAggr, text = 'Operator', anchor = 'w', justify = 'left')
		cb.opfun <- ttkcombobox(frameAggr, values = c(">=", ">", "<=", "<"), textvariable = opr.fun, width = 6, state = stateo1)
		txt.opthres <- tklabel(frameAggr, text = 'Threshold', anchor = 'w', justify = 'left')
		en.opthres <- tkentry(frameAggr, textvariable = opr.thres, width = 6, width = 6, state = stateo2)

		tkgrid(txt.aggfun, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.aggfun, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.minfrac, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.minfrac, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.opfun, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.opfun, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.opthres, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.opthres, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.aggfun, 'Function that have to be applied for aggregating from daily/dekadal/monthly into\na higher time step (e.g., for precipitation FUN=sum and for temperature FUN=mean)')
		status.bar.display(cb.aggfun, TextOutputVar, 'Function that have to be applied for aggregating from daily/dekadal/monthly into\na higher time step (e.g., for precipitation FUN=sum and for temperature FUN=mean)')
		infobulle(en.minfrac, 'Minimum fraction of available data that must be present within each output time step')
		status.bar.display(en.minfrac, TextOutputVar, 'Minimum fraction of available data that must be present within each output time step')
		infobulle(cb.opfun, 'Select the comparison operator to be used to match event')
		status.bar.display(cb.opfun, TextOutputVar, 'Select the comparison operator to be used to match event')
		infobulle(en.opthres, 'User defined threshold applied to count event')
		status.bar.display(en.opthres, TextOutputVar, 'User defined threshold applied to count event')

		##############
		tkbind(cb.aggfun, "<<ComboboxSelected>>", function(){
			stateo1 <- if(tclvalue(aggr.fun) == "count") "readonly" else "disabled"
			stateo2 <- if(tclvalue(aggr.fun) == "count") "normal" else "disabled"
			tkconfigure(cb.opfun, state = stateo1)
			tkconfigure(en.opthres, state = stateo2)
		})

		#############################
		frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		file.save1 <- tclVar(GeneralParameters$out.dir)

		txt.file.save <- tklabel(frameDirSav, text = "Directory to save results",  anchor = 'w', justify = 'left')
		en.file.save <- tkentry(frameDirSav, textvariable = file.save1, width = largeur2)
		bt.file.save <- tkbutton(frameDirSav, text = "...")

		##############
		tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save1, isFile = FALSE))

		##############
		tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		infobulle(en.file.save, 'Enter the full path to directory to save outputs')
		status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save outputs')
		infobulle(bt.file.save, 'or browse here')
		status.bar.display(bt.file.save, TextOutputVar, 'or browse here')

		#############################
		tkgrid(frameTimeS, row = 0, column = 0, sticky = '', pady = 1)
		tkgrid(frameData, row = 1, column = 0, sticky = 'we', pady = 3)
		tkgrid(frameAggr, row = 2, column = 0, sticky = 'we', pady = 3)
		tkgrid(frameDirSav, row = 3, column = 0, sticky = 'we', pady = 3)

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

		#######################

		frameYear <- ttklabelframe(subfr2, text = "Years to analyze", relief = 'groove')

		allYears <- tclVar(GeneralParameters$time.series$all.years)
		startYear <- tclVar(GeneralParameters$time.series$start.year)
		endYear <- tclVar(GeneralParameters$time.series$end.year)
		nseqYears <- tclVar(GeneralParameters$time.series$nseq.years)

		if(GeneralParameters$time.series$all.years){
			state0 <- 'disabled'
			state1 <- 'disabled'
			state2 <- 'disabled'
		}else{
			state0 <- if(!GeneralParameters$time.series$nseq.years) 'normal' else 'disabled'
			state1 <- 'normal'
			state2 <- if(GeneralParameters$time.series$nseq.years) 'normal' else 'disabled'
		}

		chk.allYears <- tkcheckbutton(frameYear, variable = allYears, text =  "Use all years from the input data", anchor = 'w', justify = 'left') #, width = largeur3
		txt.startYear <- tklabel(frameYear, text = "Start Year", anchor = 'e', justify = 'right')
		en.startYear <- tkentry(frameYear, textvariable = startYear, width = 6, state = state0)
		txt.endYear <- tklabel(frameYear, text = "End Year", anchor = 'e', justify = 'right')
		en.endYear <- tkentry(frameYear, textvariable = endYear, width = 6, state = state0)
		chk.customYear <- tkcheckbutton(frameYear, variable = nseqYears, text =  "Customized Years", anchor = 'w', justify = 'left', state = state1)
		bt.customYear <- tkbutton(frameYear, text = "Custom", state = state2)

		tkconfigure(bt.customYear, command = function(){
			titre <- 'Years to analyze'
			help <- "Edit the years to analyze. The years need to be separated by commas. E.g., 1983, 1984, 1991, 1997, 1998, 2002, 2003, ..."
			years <- GeneralParameters$time.series$custom.years[[1]]
			if(length(years) == 1 & is.na(years)) years <- ''
			years <- paste0(years, collapse = ', ')
			GeneralParameters$time.series$custom.years[[1]] <<- climatoAnalysisEditYrsMon(main.win, years, titre, help, TRUE)
		})

		tkgrid(chk.allYears, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.startYear, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.startYear, row = 1, column = 2, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.endYear, row = 1, column = 3, sticky = 'e', rowspan = 1, columnspan = 3, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.endYear, row = 1, column = 6, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(chk.customYear, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.customYear, row = 2, column = 5, sticky = 'w', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(chk.allYears, "Check this box to use all years from the input data")
		status.bar.display(chk.allYears, TextOutputVar, "Check this box to use all years from the input data")
		infobulle(en.startYear, "Enter the start year of the period to analyze")
		status.bar.display(en.startYear, TextOutputVar, "Enter the start year of the period to analyze")
		infobulle(en.endYear, "Enter the end year of the period to analyze")
		status.bar.display(en.endYear, TextOutputVar, "Enter the end year of the period to analyze")
		infobulle(chk.customYear, "Check this box if the years to analyze are not continuous")
		status.bar.display(chk.customYear, TextOutputVar, "Check this box if the years to analyze are not continuous")
		infobulle(bt.customYear, "Edit the years to analyze")
		status.bar.display(bt.customYear, TextOutputVar, "Edit the years to analyze")

		############
		tkbind(chk.allYears, "<Button-1>", function(){
			if(tclvalue(allYears) == '1'){
				state0 <- if(tclvalue(nseqYears) == '1') 'disabled' else 'normal'
				state1 <- 'normal'
				state2 <- if(tclvalue(nseqYears) == '1') 'normal' else 'disabled'
			}else{
				state0 <- 'disabled'
				state1 <- 'disabled'
				state2 <- if(tclvalue(nseqYears) == '1' & tclvalue(allYears) == '1') 'normal' else 'disabled'
			}

			tkconfigure(en.startYear, state = state0)
			tkconfigure(en.endYear, state = state0)
			tkconfigure(chk.customYear, state = state1)
			tkconfigure(bt.customYear, state = state2)
		})

		tkbind(chk.customYear, "<Button-1>", function(){
			if(tclvalue(allYears) == '0'){
				state0 <- if(tclvalue(nseqYears) == '1') 'normal' else 'disabled'
				state2 <- if(tclvalue(nseqYears) == '1') 'disabled' else 'normal'
				tkconfigure(en.startYear, state = state0)
				tkconfigure(en.endYear, state = state0)
				tkconfigure(bt.customYear, state = state2)
			}
		})

		#######################

		frameOut <- ttklabelframe(subfr2, text = "Output Time series", relief = 'groove')

		OUTSeries <- c('Monthly', 'Seasonal', 'Annual')
		out.series <- tclVar()
		tclvalue(out.series) <- switch(GeneralParameters$time.series$out.series, 
										'monthly' = OUTSeries[1],
										'seasonal' = OUTSeries[2], 
										'annual' = OUTSeries[3])

		mon1 <- as.numeric(str_trim(GeneralParameters$time.series$start.month))
		mon2 <- as.numeric(str_trim(GeneralParameters$time.series$end.month))
		startMonth <- tclVar(MOIS[mon1])
		nseqMonths <- tclVar(GeneralParameters$time.series$nseq.months)


		stateOut <- if(GeneralParameters$time.series$out.series == 'seasonal') "normal" else "disabled"

		if(GeneralParameters$time.series$out.series == 'monthly'){
			stateMon1 <- "normal"
			stateMon2 <- "normal"
			stateMon3 <- "normal"
			stateMon4 <- if(GeneralParameters$time.series$nseq.months) "normal" else "disabled"
		}else{
			stateMon1 <- "normal"
			stateMon2 <- "disabled"
			stateMon3 <- "disabled"
			stateMon4 <- "disabled"
			len <- if(GeneralParameters$time.series$out.series == 'seasonal') GeneralParameters$time.series$len.seas else 12
			mon2 <- (mon1 + len - 1)%%12
			mon2[mon2 == 0] <- 12
		}
		endMonth <- tclVar(MOIS[mon2])

		txt.outTS <- tklabel(frameOut, text = "Time step",  anchor = 'e', justify = 'right')
		cb.outTS <- ttkcombobox(frameOut, values = OUTSeries, textvariable = out.series, width = 10)
		txt.lenSeas <- tklabel(frameOut, text = 'Width')
		spin.lenSeas <- ttkspinbox(frameOut, from = 1, to = 12, increment = 1, justify = 'center', width = 2, state = stateOut)
		tkset(spin.lenSeas, GeneralParameters$time.series$len.seas)

		txt.Month <- tklabel(frameOut, text = "Months to process",  anchor = 'w', justify = 'left')
		fr.Month <- tkframe(frameOut)

		txt.startMonth <- tklabel(fr.Month, text = "From",  anchor = 'e', justify = 'right')
		cb.startMonth <- ttkcombobox(fr.Month, values = MOIS, textvariable = startMonth, width = 10, state = stateMon1)
		txt.endMonth <- tklabel(fr.Month, text = "to")
		cb.endMonth <- ttkcombobox(fr.Month, values = MOIS, textvariable = endMonth, width = 10, state = stateMon2)
		chk.customMonth <- tkcheckbutton(fr.Month, variable = nseqMonths, text =  "Customized Months", anchor = 'w', justify = 'left', state = stateMon3)
		bt.customMonth <- tkbutton(fr.Month, text = "Custom", state = stateMon4)

		tkconfigure(bt.customMonth, command = function(){
			titre <- 'Months to process'
			help <- "Edit the months to be included in the analysis. A month number must be between 1 and 12. The months need to be separated by commas. E.g., 1, 2, 3, 7, 8, 9"
			months <- GeneralParameters$time.series$custom.months[[1]]
			months <- paste0(months, collapse = ', ')
			GeneralParameters$time.series$custom.months[[1]] <<- climatoAnalysisEditYrsMon(main.win, months, titre, help, FALSE)
		})

		tkgrid(txt.startMonth, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.startMonth, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.endMonth, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.endMonth, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(chk.customMonth, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.customMonth, row = 1, column = 6, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(txt.outTS, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.outTS, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.lenSeas, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(spin.lenSeas, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.Month, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(fr.Month, row = 2, column = 0, sticky = '', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.outTS, "Select the time step of the output time series")
		status.bar.display(cb.outTS, TextOutputVar, "Select the time step of the output time series")
		infobulle(spin.lenSeas, 'Select the width of the season [in month] (e.g., 3 for three-month season)')
		status.bar.display(spin.lenSeas, TextOutputVar, 'Select the width of the season [in month] (e.g., 3 for three-month season)')

		infobulle(cb.startMonth, 'Select the start month')
		status.bar.display(cb.startMonth, TextOutputVar, 'Select the start month')

		infobulle(chk.customMonth, "Check this box if the months to process are not consecutive")
		status.bar.display(chk.customMonth, TextOutputVar, "Check this box if the months to process are not consecutive")
		infobulle(bt.customMonth, "Edit the months to process")
		status.bar.display(bt.customMonth, TextOutputVar, "Edit the months to process")

		##############
		tkbind(cb.outTS, "<<ComboboxSelected>>", function(){
			stateOut <- if(tclvalue(out.series) == 'Seasonal') "normal" else "disabled"
			tkconfigure(spin.lenSeas, state = stateOut)

			if(tclvalue(out.series) == 'Monthly'){
				stateMon1 <- "normal"
				stateMon2 <- "normal"
				stateMon3 <- "normal"
				stateMon4 <- if(tclvalue(nseqMonths) == '1') "normal" else "disabled"
			}else{
				stateMon1 <- "normal"
				stateMon2 <- "disabled"
				stateMon3 <- "disabled"
				stateMon4 <- "disabled"
			}
			tkconfigure(cb.startMonth, state = stateMon1)
			tkconfigure(cb.endMonth, state = stateMon2)
			tkconfigure(chk.customMonth, state = stateMon3)
			tkconfigure(bt.customMonth, state = stateMon4)

			if(tclvalue(out.series)%in%c('Seasonal', 'Annual')){
				len <- if(tclvalue(out.series) == 'Seasonal') as.numeric(str_trim(tclvalue(tkget(spin.lenSeas)))) else 12
				mon1 <- which(MOIS%in%str_trim(tclvalue(startMonth)))
				mon2 <- (mon1 + len - 1)%%12
				mon2[mon2 == 0] <- 12
				tclvalue(endMonth) <- MOIS[mon2]
			}
		})

		tkbind(spin.lenSeas, "<ButtonRelease-1>", function(){
			if(tclvalue(out.series) == 'Seasonal'){
				len <- as.numeric(str_trim(tclvalue(tkget(spin.lenSeas))))
				mon1 <- which(MOIS%in%str_trim(tclvalue(startMonth)))
				mon2 <- (mon1 + len - 1)%%12
				mon2[mon2 == 0] <- 12
				tclvalue(endMonth) <- MOIS[mon2]
			}
		})

		tkbind(spin.lenSeas, "<FocusOut>", function(){
			if(tclvalue(out.series) == 'Seasonal'){
				len <- as.numeric(str_trim(tclvalue(tkget(spin.lenSeas))))
				mon1 <- which(MOIS%in%str_trim(tclvalue(startMonth)))
				mon2 <- (mon1 + len - 1)%%12
				mon2[mon2 == 0] <- 12
				tclvalue(endMonth) <- MOIS[mon2]
			}
		})

		tkbind(chk.customMonth, "<Button-1>", function(){
			if(tclvalue(out.series) == 'Monthly'){
				stateMon <- if(tclvalue(nseqMonths) == '1') 'normal' else 'disabled'
				stateMon4 <- if(tclvalue(nseqMonths) == '1') 'disabled' else 'normal'
				tkconfigure(cb.startMonth, state = stateMon)
				tkconfigure(cb.endMonth, state = stateMon)
				tkconfigure(bt.customMonth, state = stateMon4)
			}
		})

		tkbind(cb.startMonth, "<<ComboboxSelected>>", function(){
			if(tclvalue(out.series)%in%c('Seasonal', 'Annual')){
				len <- if(tclvalue(out.series) == 'Seasonal') as.numeric(str_trim(tclvalue(tkget(spin.lenSeas)))) else 12
				mon1 <- which(MOIS%in%str_trim(tclvalue(startMonth)))
				mon2 <- (mon1 + len - 1)%%12
				mon2[mon2 == 0] <- 12
				tclvalue(endMonth) <- MOIS[mon2]
			}
		})

		#######################

		frameAnalysis <- ttklabelframe(subfr2, text = "Analysis Method", relief = 'groove')

		ANALYSIS <- c('Mean', 'Median', 'Standard deviation', 'Coefficient of variation',
						'Trend', 'Percentiles', 'Frequency', 'Anomaly')
		analysis.method <- tclVar()
		tclvalue(analysis.method) <- switch(GeneralParameters$analysis.method$mth.fun, 
										'mean' = ANALYSIS[1],
										'median' = ANALYSIS[2], 
										'std' = ANALYSIS[3],
										'cv' = ANALYSIS[4], 
										'trend' = ANALYSIS[5],
										'percentile' = ANALYSIS[6], 
										'frequency' = ANALYSIS[7],
										'anomaly' = ANALYSIS[8])

		cb.anMthd <- ttkcombobox(frameAnalysis, values = ANALYSIS, textvariable = analysis.method, width = largeur4)
		fr.anMthd <- tkframe(frameAnalysis)

		trend.unit <- tclVar(GeneralParameters$analysis.method$trend.unit)
		if(tclvalue(analysis.method) == 'Trend'){
			rd.trend1 <- tkradiobutton(fr.anMthd, text = "change (trend) / year", variable = trend.unit, value = "1", anchor = 'w', justify = 'left')
			rd.trend2 <- tkradiobutton(fr.anMthd, text = "change (trend) over the period", variable = trend.unit, value = "2", anchor = 'w', justify = 'left')
			rd.trend3 <- tkradiobutton(fr.anMthd, text = "change (trend) / average (in %)", variable = trend.unit, value = "3", anchor = 'w', justify = 'left')

			tkgrid(rd.trend1, sticky = 'we')
			tkgrid(rd.trend2, sticky = 'we')
			tkgrid(rd.trend3, sticky = 'we')
		}

		mth.perc <- tclVar(GeneralParameters$analysis.method$mth.perc)
		if(tclvalue(analysis.method) == 'Percentiles'){
			txt.Percent <- tklabel(fr.anMthd, text = "Percentile",  anchor = 'e', justify = 'right')
			en.Percent <- tkentry(fr.anMthd, textvariable = mth.perc, width = 4)
			th.Percent <- tklabel(fr.anMthd, text = "th",  anchor = 'w', justify = 'left')

			tkgrid(txt.Percent, en.Percent, th.Percent)

			infobulle(en.Percent, "Enter the nth percentile to be calculated")
			status.bar.display(en.Percent, TextOutputVar, "Enter the nth percentile to be calculated")
		}

		low.thres <- tclVar(GeneralParameters$analysis.method$low.thres)
		up.thres <- tclVar(GeneralParameters$analysis.method$up.thres)
		if(tclvalue(analysis.method) == 'Frequency'){
			txt.Freq0 <- tklabel(fr.anMthd, text = "Frequency",  anchor = 'w', justify = 'left')
			txt.Freq1 <- tklabel(fr.anMthd, text = "Between",  anchor = 'e', justify = 'right')
			en.Freq1 <- tkentry(fr.anMthd, textvariable = low.thres, width = 5)
			txt.Freq2 <- tklabel(fr.anMthd, text = "And")
			en.Freq2 <- tkentry(fr.anMthd, textvariable = up.thres, width = 5)

			tkgrid(txt.Freq0, sticky = 'we', columnspan = 4)
			tkgrid(txt.Freq1, en.Freq1, txt.Freq2, en.Freq2)

			infobulle(en.Freq1, "Enter the lower bound of the interval to count the number of occurrences")
			status.bar.display(en.Freq1, TextOutputVar, "Enter the lower bound of the interval to count the number of occurrences")
			infobulle(en.Freq2, "Enter the upper bound of the interval to count the number of occurrences")
			status.bar.display(en.Freq2, TextOutputVar, "Enter the upper bound of the interval to count the number of occurrences")
		}

		perc.anom <- tclVar(GeneralParameters$analysis.method$perc.anom)
		startYr.anom <- tclVar(GeneralParameters$analysis.method$startYr.anom)
		endYr.anom <- tclVar(GeneralParameters$analysis.method$endYr.anom)
		if(tclvalue(analysis.method) == 'Anomaly'){
			txt.Anom0 <- tklabel(fr.anMthd, text = "Anomaly",  anchor = 'w', justify = 'left')
			chk.Anom <- tkcheckbutton(fr.anMthd, variable = perc.anom, text = "Percentage of mean", anchor = 'w', justify = 'left')

			fr.Anom <- ttklabelframe(fr.anMthd, text = "Base period", relief = 'groove')
			txt.Anom1 <- tklabel(fr.Anom, text = "Start Year",  anchor = 'e', justify = 'right')
			txt.Anom2 <- tklabel(fr.Anom, text = "End Year",  anchor = 'e', justify = 'right')
			en.Anom1 <- tkentry(fr.Anom, textvariable = startYr.anom, width = 5)
			en.Anom2 <- tkentry(fr.Anom, textvariable = endYr.anom, width = 5)

			tkgrid(txt.Anom0, chk.Anom)
			tkgrid(txt.Anom1, en.Anom1, txt.Anom2, en.Anom2)
			tkgrid(fr.Anom, sticky = 'we', columnspan = 2)

			infobulle(chk.Anom, "Check this box to calculate the anomaly as percentage of mean")
			status.bar.display(chk.Anom, TextOutputVar, "Check this box to calculate the anomaly as percentage of mean")
			infobulle(fr.Anom, "Enter the start and end year to be used to calculate the climatology")
			status.bar.display(fr.Anom, TextOutputVar, "Enter the start and end year to be used to calculate the climatology")
		}

		tkgrid(cb.anMthd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(fr.anMthd, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.anMthd, "Select the analysis method")
		status.bar.display(cb.anMthd, TextOutputVar, "Select the analysis method")

		#################
		tkbind(cb.anMthd, "<<ComboboxSelected>>", function(){
			tkdestroy(fr.anMthd)
			fr.anMthd <<- tkframe(frameAnalysis)

			if(tclvalue(analysis.method) == 'Trend'){
				rd.trend1 <- tkradiobutton(fr.anMthd, text = "change (trend) / year", variable = trend.unit, value = "1", anchor = 'w', justify = 'left')
				rd.trend2 <- tkradiobutton(fr.anMthd, text = "change (trend) over the period", variable = trend.unit, value = "2", anchor = 'w', justify = 'left')
				rd.trend3 <- tkradiobutton(fr.anMthd, text = "change (trend) / average (in %)", variable = trend.unit, value = "3", anchor = 'w', justify = 'left')

				tkgrid(rd.trend1, sticky = 'we')
				tkgrid(rd.trend2, sticky = 'we')
				tkgrid(rd.trend3, sticky = 'we')
			}

			if(tclvalue(analysis.method) == 'Percentiles'){
				txt.Percent <- tklabel(fr.anMthd, text = "Percentile",  anchor = 'e', justify = 'right')
				en.Percent <- tkentry(fr.anMthd, textvariable = mth.perc, width = 4)
				th.Percent <- tklabel(fr.anMthd, text = "th",  anchor = 'w', justify = 'left')

				tkgrid(txt.Percent, en.Percent, th.Percent)

				infobulle(en.Percent, "Enter the nth percentile to be calculated")
				status.bar.display(en.Percent, TextOutputVar, "Enter the nth percentile to be calculated")
			}
			if(tclvalue(analysis.method) == 'Frequency'){
				txt.Freq0 <- tklabel(fr.anMthd, text = "Frequency",  anchor = 'w', justify = 'left')
				txt.Freq1 <- tklabel(fr.anMthd, text = "Between",  anchor = 'e', justify = 'right')
				en.Freq1 <- tkentry(fr.anMthd, textvariable = low.thres, width = 5)
				txt.Freq2 <- tklabel(fr.anMthd, text = "And")
				en.Freq2 <- tkentry(fr.anMthd, textvariable = up.thres, width = 5)

				tkgrid(txt.Freq0, sticky = 'we', columnspan = 4)
				tkgrid(txt.Freq1, en.Freq1, txt.Freq2, en.Freq2)

				infobulle(en.Freq1, "Enter the lower bound of the interval to count the number of occurrences")
				status.bar.display(en.Freq1, TextOutputVar, "Enter the lower bound of the interval to count the number of occurrences")
				infobulle(en.Freq2, "Enter the upper bound of the interval to count the number of occurrences")
				status.bar.display(en.Freq2, TextOutputVar, "Enter the upper bound of the interval to count the number of occurrences")
			}
			if(tclvalue(analysis.method) == 'Anomaly'){
				txt.Anom0 <- tklabel(fr.anMthd, text = "Anomaly",  anchor = 'w', justify = 'left')
				chk.Anom <- tkcheckbutton(fr.anMthd, variable = perc.anom, text = "Percentage of mean", anchor = 'w', justify = 'left')

				fr.Anom <- ttklabelframe(fr.anMthd, text = "Base period", relief = 'groove')
				txt.Anom1 <- tklabel(fr.Anom, text = "Start Year",  anchor = 'e', justify = 'right')
				txt.Anom2 <- tklabel(fr.Anom, text = "End Year",  anchor = 'e', justify = 'right')
				en.Anom1 <- tkentry(fr.Anom, textvariable = startYr.anom, width = 5)
				en.Anom2 <- tkentry(fr.Anom, textvariable = endYr.anom, width = 5)

				tkgrid(txt.Anom0, chk.Anom)
				tkgrid(txt.Anom1, en.Anom1, txt.Anom2, en.Anom2)
				tkgrid(fr.Anom, sticky = 'we', columnspan = 2)

				infobulle(chk.Anom, "Check this box to calculate the anomaly as percentage of mean")
				status.bar.display(chk.Anom, TextOutputVar, "Check this box to calculate the anomaly as percentage of mean")
				infobulle(fr.Anom, "Enter the start and end year to be used to calculate the climatology")
				status.bar.display(fr.Anom, TextOutputVar, "Enter the start and end year to be used to calculate the climatology")
			}

			tkgrid(fr.anMthd, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		})

		#############################

		if(!is.null(EnvClimatoAnalysisplot$DirExist)){
			stateAnaBut <- if(tclvalue(EnvClimatoAnalysisplot$DirExist) == "1") "normal" else "disabled"
		}else stateAnaBut <- "normal"

		AnalyzeBut <- ttkbutton(subfr2, text = "Calculate", state = stateAnaBut)

		#################
		tkconfigure(AnalyzeBut, command = function(){
			GeneralParameters$data.type <- switch(str_trim(tclvalue(DataType)),
												'CDT stations data format' = 'cdtstation',
												'CDT dataset format (gridded)' = 'cdtdataset')

			GeneralParameters$in.tstep <- switch(str_trim(tclvalue(timeSteps)), 
			 									'Daily data' = 'daily',
			 									'Pentad data' = 'pentad',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')
			GeneralParameters$in.file <- str_trim(tclvalue(file.stnfl))
			GeneralParameters$out.dir <- str_trim(tclvalue(file.save1))

			GeneralParameters$aggr.series$aggr.fun <- str_trim(tclvalue(aggr.fun))
			GeneralParameters$aggr.series$min.frac <- as.numeric(str_trim(tclvalue(min.frac)))
			GeneralParameters$aggr.series$opr.fun <- str_trim(tclvalue(opr.fun))
			GeneralParameters$aggr.series$opr.thres <- as.numeric(str_trim(tclvalue(opr.thres)))

			GeneralParameters$time.series$all.years <- switch(tclvalue(allYears), '0' = FALSE, '1' = TRUE)
			if(!GeneralParameters$time.series$all.years){
				GeneralParameters$time.series$nseq.years <- switch(tclvalue(nseqYears), '0' = FALSE, '1' = TRUE)
				if(GeneralParameters$time.series$nseq.years){
					if(length(GeneralParameters$time.series$custom.years[[1]]) < 1){
						InsertMessagesTxt(main.txt.out, "No years selected.", format = TRUE)
						return(NULL)
					}
					if(any(is.na(GeneralParameters$time.series$custom.years[[1]]))){
						InsertMessagesTxt(main.txt.out, "The edited years contain missing values.", format = TRUE)
						return(NULL)
					}
				}else{
					GeneralParameters$time.series$start.year <- as.numeric(str_trim(tclvalue(startYear)))
					GeneralParameters$time.series$end.year <- as.numeric(str_trim(tclvalue(endYear)))
					if(is.na(GeneralParameters$time.series$start.year) | is.na(GeneralParameters$time.series$end.year)){
						InsertMessagesTxt(main.txt.out, "Start or end year are missing.", format = TRUE)
						return(NULL)
					}
				}
			}

			GeneralParameters$time.series$out.series <- switch(str_trim(tclvalue(out.series)), 
						 									'Monthly' = 'monthly',
															'Seasonal' =  'seasonal',
															'Annual' = 'annual')
			GeneralParameters$time.series$len.seas <- as.numeric(str_trim(tclvalue(tkget(spin.lenSeas))))
			GeneralParameters$time.series$start.month <- which(MOIS%in%str_trim(tclvalue(startMonth)))
			GeneralParameters$time.series$end.month <- which(MOIS%in%str_trim(tclvalue(endMonth)))
			GeneralParameters$time.series$nseq.months <- switch(tclvalue(nseqMonths), '0' = FALSE, '1' = TRUE)

			if(GeneralParameters$time.series$out.series == 'monthly'){
				if(GeneralParameters$time.series$nseq.months){
					if(length(GeneralParameters$time.series$custom.months[[1]]) < 1){
						InsertMessagesTxt(main.txt.out, "No months selected.", format = TRUE)
						return(NULL)
					}
					if(any(is.na(GeneralParameters$time.series$custom.months[[1]]))){
						InsertMessagesTxt(main.txt.out, "The edited months contain missing values.", format = TRUE)
						return(NULL)
					}
				}
			}

			GeneralParameters$analysis.method$mth.fun <- switch(str_trim(tclvalue(analysis.method)),
																'Mean' = 'mean',
																'Median' = 'median',
																'Standard deviation' = 'std',
																'Coefficient of variation' = 'cv',
																'Trend' = 'trend',
																'Percentiles' = 'percentile',
																'Frequency' = 'frequency',
																'Anomaly' = 'anomaly')

			if(GeneralParameters$analysis.method$mth.fun == 'trend'){
				GeneralParameters$analysis.method$trend.unit <- as.numeric(tclvalue(trend.unit))
			}

			if(GeneralParameters$analysis.method$mth.fun == 'percentile'){
				GeneralParameters$analysis.method$mth.perc <- as.numeric(str_trim(tclvalue(mth.perc)))
				if(is.na(GeneralParameters$analysis.method$mth.perc)){
					InsertMessagesTxt(main.txt.out, "Percentile is missing.", format = TRUE)
					return(NULL)
				}
			}

			if(GeneralParameters$analysis.method$mth.fun == 'frequency'){
				GeneralParameters$analysis.method$low.thres <- as.numeric(str_trim(tclvalue(low.thres)))
				GeneralParameters$analysis.method$up.thres <- as.numeric(str_trim(tclvalue(up.thres)))
				if(is.na(GeneralParameters$analysis.method$low.thres) | is.na(GeneralParameters$analysis.method$up.thres)){
					InsertMessagesTxt(main.txt.out, "Lower or upper bounds of the interval for frequency are missing.", format = TRUE)
					return(NULL)
				}
			}

			if(GeneralParameters$analysis.method$mth.fun == 'anomaly'){
				GeneralParameters$analysis.method$perc.anom <- switch(tclvalue(perc.anom), '0' = FALSE, '1' = TRUE)
				GeneralParameters$analysis.method$startYr.anom <- as.numeric(str_trim(tclvalue(startYr.anom)))
				GeneralParameters$analysis.method$endYr.anom <- as.numeric(str_trim(tclvalue(endYr.anom)))
				if(is.na(GeneralParameters$analysis.method$startYr.anom) | is.na(GeneralParameters$analysis.method$endYr.anom)){
					InsertMessagesTxt(main.txt.out, "Base period to compute climatology for anomaly is missing.", format = TRUE)
					return(NULL)
				}
			}

			# assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)

			tkconfigure(main.win, cursor = 'watch')
			InsertMessagesTxt(main.txt.out, paste("Calculating", tclvalue(analysis.method), "......."))
			ret <- tryCatch(
				climatoAnalysisProcs(GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = tkconfigure(main.win, cursor = '')
			)

			msg0 <- paste(tclvalue(analysis.method), "calculation finished successfully")
			msg1 <- paste(tclvalue(analysis.method), "calculation failed")

			if(!is.null(ret)){
				if(ret == 0){
					InsertMessagesTxt(main.txt.out, msg0)

					###################

					load.ClimatoAnalysis.Data()

				}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
			}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
		})

		##############################################
		tkgrid(frameYear, row = 0, column = 0, sticky = '')
		tkgrid(frameOut, row = 1, column = 0, sticky = 'we', pady = 3)
		tkgrid(frameAnalysis, row = 2, column = 0, sticky = 'we', pady = 3)
		tkgrid(AnalyzeBut, row = 3, column = 0, sticky = '', pady = 3)

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

		frameAnDat <- ttklabelframe(subfr3, text = "Analysis data", relief = 'groove')

		EnvClimatoAnalysisplot$DirExist <- tclVar(0)
		file.Stat <- tclVar()

		statedirStat <- if(tclvalue(EnvClimatoAnalysisplot$DirExist) == "1") "normal" else "disabled"

		chk.dirStat <- tkcheckbutton(frameAnDat, variable = EnvClimatoAnalysisplot$DirExist, text = "Analysis already computed", anchor = 'w', justify = 'left')
		en.dirStat <- tkentry(frameAnDat, textvariable = file.Stat, width = largeur2, state = statedirStat)
		bt.dirStat <- tkbutton(frameAnDat, text = "...", state = statedirStat)

		tkconfigure(bt.dirStat, command = function(){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.Stat <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			if(path.Stat%in%c("", "NA") | is.na(path.Stat)) return(NULL)
			tclvalue(file.Stat) <- path.Stat

			if(file.exists(str_trim(tclvalue(file.Stat)))){
				dirAnalysis <- try(readRDS(str_trim(tclvalue(file.Stat))), silent = TRUE)
				if(inherits(dirAnalysis, "try-error")){
					InsertMessagesTxt(main.txt.out, 'Unable to load Climate Analysis data', format = TRUE)
					InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', dirAnalysis[1]), format = TRUE)
					tkconfigure(cb.climato.maps, values = "")
					tclvalue(EnvClimatoAnalysisplot$climStat) <- ""
					return(NULL)
				}

				EnvClimatoAnalysisplot$DirStat <- dirAnalysis
				EnvClimatoAnalysisplot$PathStat <- dirname(str_trim(tclvalue(file.Stat)))

				###################

				load.ClimatoAnalysis.Data()
			}
		})

		tkgrid(chk.dirStat, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.dirStat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.dirStat, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(chk.dirStat, "<Button-1>", function(){
			statedirStat <- if(tclvalue(EnvClimatoAnalysisplot$DirExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.dirStat, state = statedirStat)
			tkconfigure(bt.dirStat, state = statedirStat)
			stateAnaBut <- if(tclvalue(EnvClimatoAnalysisplot$DirExist) == '1') 'normal' else 'disabled'
			tkconfigure(AnalyzeBut, state = stateAnaBut)
		})

		##############################################

		frameClimatoMap <- ttklabelframe(subfr3, text = "Statistics Maps", relief = 'groove')

		EnvClimatoAnalysisplot$climStat <- tclVar()
		EnvClimatoAnalysisplot$climDate <- tclVar()

		cb.climato.maps <- ttkcombobox(frameClimatoMap, values = "", textvariable = EnvClimatoAnalysisplot$climStat, width = largeur5)
		bt.climato.maps <- ttkbutton(frameClimatoMap, text = "PLOT")
		cb.climDate <- ttkcombobox(frameClimatoMap, values = "", textvariable = EnvClimatoAnalysisplot$climDate, width = largeur6)
		bt.climMapOpt <- ttkbutton(frameClimatoMap, text = "Options")

		###################

		EnvClimatoAnalysisplot$climMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
												userCol = list(custom = FALSE, color = NULL),
												userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
												title = list(user = FALSE, title = ''),
												colkeyLab = list(user = FALSE, label = ''),
												scalebar = list(add = FALSE, pos = 'bottomleft'))

		tkconfigure(bt.climMapOpt, command = function(){
			if(!is.null(EnvClimatoAnalysisplot$don)){
				atlevel <- pretty(EnvClimatoAnalysisplot$don$z, n = 10, min.n = 7)
				if(is.null(EnvClimatoAnalysisplot$climMapOp$userLvl$levels)){
					EnvClimatoAnalysisplot$climMapOp$userLvl$levels <- atlevel
				}else{
					if(!EnvClimatoAnalysisplot$climMapOp$userLvl$custom)
						EnvClimatoAnalysisplot$climMapOp$userLvl$levels <- atlevel
				}
			}
			EnvClimatoAnalysisplot$climMapOp <- climatoAnalysis.MapOptions(main.win, EnvClimatoAnalysisplot$climMapOp)
		})

		EnvClimatoAnalysisplot$notebookTab.climMap <- NULL
		tkconfigure(bt.climato.maps, command = function(){
			if(!tclvalue(EnvClimatoAnalysisplot$climStat)%in%c("", "Anomaly")){
				imgContainer <- climatoAnalysis.DisplayStatMaps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvClimatoAnalysisplot$notebookTab.climMap, AllOpenTabType, AllOpenTabData)
				EnvClimatoAnalysisplot$notebookTab.climMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		###################
		tkgrid(cb.climato.maps, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.climato.maps, row = 0, column = 4, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.climDate, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(bt.climMapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		###############

		tkbind(cb.climato.maps, "<<ComboboxSelected>>", function(){
			analysis.path <- file.path(EnvClimatoAnalysisplot$PathStat, tclvalue(EnvClimatoAnalysisplot$climStat))
			params <- try(readRDS(file.path(analysis.path, "params.rds")), silent = TRUE)
			if(inherits(params, "try-error")){
				InsertMessagesTxt(main.txt.out, 'Unable to load Climate Analysis data', format = TRUE)
				InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', params[1]), format = TRUE)
				tkconfigure(cb.climDate, values = "")
				tclvalue(EnvClimatoAnalysisplot$climDate) <- ""
				return(NULL)
			}
			tkconfigure(cb.climDate, values = params$stats)
			tclvalue(EnvClimatoAnalysisplot$climDate) <- params$stats[1]
			stateclimDate <- if(params$params$time.series$out.series == "monthly") "normal" else "disabled"
			tkconfigure(cb.climDate, state = stateclimDate)

			EnvClimatoAnalysisplot$statpars <- params

			###################
			ret <- EnvClimatoAnalysisplot$read.ClimStat()
			if(is.null(ret)) return(NULL)

			###################
			tkconfigure(cb.TSDate, values = params$timeseries[[1]][[2]])
			tclvalue(EnvClimatoAnalysisplot$TSDate) <- params$timeseries[[1]][[2]][1]

			###################
			if(!is.null(EnvClimatoAnalysisplot$don)){
				atlevel <- pretty(EnvClimatoAnalysisplot$don$z, n = 10, min.n = 7)
				EnvClimatoAnalysisplot$climMapOp$userLvl$levels <- atlevel
			}

			###################
			ret1 <- EnvClimatoAnalysisplot$read.ClimTSData()
			if(is.null(ret1)) return(NULL)
		})

		###############

		tkbind(cb.climDate, "<<ComboboxSelected>>", function(){
			ret <- EnvClimatoAnalysisplot$read.ClimStat()
			if(is.null(ret)) return(NULL)

			###################
			ipos <- which(EnvClimatoAnalysisplot$statpars$stats == tclvalue(EnvClimatoAnalysisplot$climDate))
			tkconfigure(cb.TSDate, values = EnvClimatoAnalysisplot$statpars$timeseries[[ipos]][[2]])
			tclvalue(EnvClimatoAnalysisplot$TSDate) <- EnvClimatoAnalysisplot$statpars$timeseries[[ipos]][[2]][1]

			###################
			ret1 <- EnvClimatoAnalysisplot$read.ClimTSData()
			if(is.null(ret1)) return(NULL)
		})

		##############################################

		frameTSMaps <- ttklabelframe(subfr3, text = "Monthly/Seasonal/Annual Maps", relief = 'groove')

		EnvClimatoAnalysisplot$TSDate <- tclVar()
		EnvClimatoAnalysisplot$TSData <- tclVar("Data")

		cb.TSDate <- ttkcombobox(frameTSMaps, values = "", textvariable = EnvClimatoAnalysisplot$TSDate, width = largeur6)
		bt.TSDate.prev <- ttkbutton(frameTSMaps, text = "<<", width = 3)
		bt.TSDate.next <- ttkbutton(frameTSMaps, text = ">>", width = 3)
		bt.TSDate.plot <- ttkbutton(frameTSMaps, text = "PLOT", width = 7)

		cb.TSData <- ttkcombobox(frameTSMaps, values = "Data", textvariable = EnvClimatoAnalysisplot$TSData, width = largeur6)
		bt.TSMapOpt <- ttkbutton(frameTSMaps, text = "Options", width = 7)

		###################

		EnvClimatoAnalysisplot$TSMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
												userCol = list(custom = FALSE, color = NULL),
												userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
												title = list(user = FALSE, title = ''),
												colkeyLab = list(user = FALSE, label = ''),
												scalebar = list(add = FALSE, pos = 'bottomleft'))

		tkconfigure(bt.TSMapOpt, command = function(){
			if(!is.null(EnvClimatoAnalysisplot$tsdata)){
				if(tclvalue(EnvClimatoAnalysisplot$TSData) == "Data")
					atlevel <- pretty(EnvClimatoAnalysisplot$tsdata$z, n = 10, min.n = 7)
				if(tclvalue(EnvClimatoAnalysisplot$TSData) == "Anomaly")
					atlevel <- pretty(EnvClimatoAnalysisplot$anomData$z, n = 10, min.n = 7)
				if(is.null(EnvClimatoAnalysisplot$TSMapOp$userLvl$levels)){
					EnvClimatoAnalysisplot$TSMapOp$userLvl$levels <- atlevel
				}else{
					if(!EnvClimatoAnalysisplot$TSMapOp$userLvl$custom)
						EnvClimatoAnalysisplot$TSMapOp$userLvl$levels <- atlevel
				}
			}
			EnvClimatoAnalysisplot$TSMapOp <- climatoAnalysis.MapOptions(main.win, EnvClimatoAnalysisplot$TSMapOp)
		})


		EnvClimatoAnalysisplot$notebookTab.TSMap <- NULL
		tkconfigure(bt.TSDate.plot, command = function(){
			if(tclvalue(EnvClimatoAnalysisplot$TSDate) != ""){
				imgContainer <- climatoAnalysis.DisplayTSMaps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvClimatoAnalysisplot$notebookTab.TSMap, AllOpenTabType, AllOpenTabData)
				EnvClimatoAnalysisplot$notebookTab.TSMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.TSDate.prev, command = function(){
			if(tclvalue(EnvClimatoAnalysisplot$TSDate) != ""){
				ipos <- which(EnvClimatoAnalysisplot$statpars$stats == tclvalue(EnvClimatoAnalysisplot$climDate))
				idaty <- which(EnvClimatoAnalysisplot$statpars$timeseries[[ipos]][[2]] == tclvalue(EnvClimatoAnalysisplot$TSDate))
				idaty <- idaty-1
				if(idaty < 1) idaty <- length(EnvClimatoAnalysisplot$statpars$timeseries[[ipos]][[2]])
				tclvalue(EnvClimatoAnalysisplot$TSDate) <- EnvClimatoAnalysisplot$statpars$timeseries[[ipos]][[2]][idaty]
				ret1 <- EnvClimatoAnalysisplot$read.ClimTSData()
				if(is.null(ret1)) return(NULL)

				imgContainer <- climatoAnalysis.DisplayTSMaps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvClimatoAnalysisplot$notebookTab.TSMap, AllOpenTabType, AllOpenTabData)
				EnvClimatoAnalysisplot$notebookTab.TSMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.TSDate.next, command = function(){
			if(tclvalue(EnvClimatoAnalysisplot$TSDate) != ""){
				ipos <- which(EnvClimatoAnalysisplot$statpars$stats == tclvalue(EnvClimatoAnalysisplot$climDate))
				idaty <- which(EnvClimatoAnalysisplot$statpars$timeseries[[ipos]][[2]] == tclvalue(EnvClimatoAnalysisplot$TSDate))
				idaty <- idaty+1
				if(idaty > length(EnvClimatoAnalysisplot$statpars$timeseries[[ipos]][[2]])) idaty <- 1
				tclvalue(EnvClimatoAnalysisplot$TSDate) <- EnvClimatoAnalysisplot$statpars$timeseries[[ipos]][[2]][idaty]
				ret1 <- EnvClimatoAnalysisplot$read.ClimTSData()
				if(is.null(ret1)) return(NULL)

				imgContainer <- climatoAnalysis.DisplayTSMaps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvClimatoAnalysisplot$notebookTab.TSMap, AllOpenTabType, AllOpenTabData)
				EnvClimatoAnalysisplot$notebookTab.TSMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		###################
		tkgrid(bt.TSDate.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.TSDate, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.TSDate.next, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.TSDate.plot, row = 0, column = 3, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(cb.TSData, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.TSMapOpt, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		###############

		tkbind(cb.TSDate, "<<ComboboxSelected>>", function(){
			ret1 <- EnvClimatoAnalysisplot$read.ClimTSData()
			if(is.null(ret1)) return(NULL)
		})

		tkbind(cb.TSData, "<<ComboboxSelected>>", function(){
			if(!is.null(EnvClimatoAnalysisplot$tsdata)){
				if(tclvalue(EnvClimatoAnalysisplot$TSData) == "Data")
					atlevel <- pretty(EnvClimatoAnalysisplot$tsdata$z, n = 10, min.n = 7)
				if(tclvalue(EnvClimatoAnalysisplot$TSData) == "Anomaly")
					atlevel <- pretty(EnvClimatoAnalysisplot$anomData$z, n = 10, min.n = 7)
				EnvClimatoAnalysisplot$TSMapOp$userLvl$levels <- atlevel
			}
		})

		##############################################

		tkgrid(frameAnDat, row = 0, column = 0, sticky = 'we')
		tkgrid(frameClimatoMap, row = 1, column = 0, sticky = 'we')
		tkgrid(frameTSMaps, row = 2, column = 0, sticky = 'we', pady = 1)

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

		frameTSPlot <- ttklabelframe(subfr4, text = "Time Series Graph", relief = 'groove')

		typeTSPLOT <- c("Line", "Barplot", "Probability", "ENSO-Line", "ENSO-Barplot", "ENSO-Proba", "Anomaly")
		EnvClimatoAnalysisplot$graph$typeTSp <- tclVar("Line")
		EnvClimatoAnalysisplot$graph$averageTSp <- tclVar(FALSE)
		EnvClimatoAnalysisplot$graph$tercileTSp <- tclVar(FALSE)
		EnvClimatoAnalysisplot$graph$trendTSp <- tclVar(FALSE)

		stateType <- if(tclvalue(EnvClimatoAnalysisplot$graph$typeTSp)%in%c("Line", "ENSO-Line")) "normal" else "disabled"

		cb.typeTSp <- ttkcombobox(frameTSPlot, values = typeTSPLOT, textvariable = EnvClimatoAnalysisplot$graph$typeTSp, width = largeur6)
		bt.TsGraph.plot <- ttkbutton(frameTSPlot, text = "PLOT", width = 7)
		bt.TSGraphOpt <- ttkbutton(frameTSPlot, text = "Options", width = 8)

		frTS1 <- tkframe(frameTSPlot)
		chk.meanTSp <- tkcheckbutton(frTS1, variable = EnvClimatoAnalysisplot$graph$averageTSp, text = "Add Mean", anchor = 'w', justify = 'left', state = stateType)
		chk.tercTSp <- tkcheckbutton(frTS1, variable = EnvClimatoAnalysisplot$graph$tercileTSp, text = "Add Terciles", anchor = 'w', justify = 'left', state = stateType)
		chk.trendTSp <- tkcheckbutton(frTS1, variable = EnvClimatoAnalysisplot$graph$trendTSp, text = "Add Trend", anchor = 'w', justify = 'left', state = stateType)
		tkgrid(chk.meanTSp, chk.tercTSp, chk.trendTSp)

		#################

		EnvClimatoAnalysisplot$TSGraphOp <- list(
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
						proba = list(theoretical = TRUE, col = 'black', lwd = 2)
						),
					line.enso = list(
						xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
						ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
						axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
						title = list(is.title = FALSE, title = '', position = 'top'),
						plot = list(lwd = 2, cex = 2, col = list(line = "black",
									points = c("blue", "gray", "red"))),
						legend = list(
							is = list(mean = FALSE, tercile = FALSE, linear = FALSE),
							add = list(mean = FALSE, tercile = FALSE, linear = FALSE),
							col = list(mean = "darkblue", tercile1 = "chartreuse4", tercile2 = "darkgoldenrod4", linear = "purple3"),
							text = list(mean = "Average", tercile1 = "Tercile 0.33333", tercile2 = "Tercile 0.66666", linear = "Trend line"),
							lwd = list(mean = 2, tercile = 2, linear = 2))
						),
					bar.enso = list(
						xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
						ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
						axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
						title = list(is.title = FALSE, title = '', position = 'top'),
						colors = list(col = c("blue", "gray", "red"))
						),
					proba.enso = list(
						xlim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
						ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
						axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
						title = list(is.title = FALSE, title = '', position = 'top'),
						plot = list(type = 'both', lwd = 2, cex = 1.4,
							all = list(line = "black", points = "lightgray"),
							nina = list(line = "blue", points = "lightblue"),
							neutre = list(line = "gray", points = "lightgray"),
							nino = list(line = "red", points = "lightpink"))
						)
					)

		tkconfigure(bt.TSGraphOpt, command = function(){
			suffix.fun <- switch(tclvalue(EnvClimatoAnalysisplot$graph$typeTSp),
									"Anomaly" = "Anomaly",
									"Barplot" = "Bar",
									"Line" = "Line",
									"Probability" = "Proba",
									"ENSO-Line" = "LineENSO",
									"ENSO-Barplot" = "BarENSO",
									"ENSO-Proba" = "ProbaENSO")
			plot.fun <- match.fun(paste0("climatoAnalysis.GraphOptions.", suffix.fun))
			EnvClimatoAnalysisplot$TSGraphOp <- plot.fun(main.win, EnvClimatoAnalysisplot$TSGraphOp)
		})

		EnvClimatoAnalysisplot$notebookTab.tsplot <- NULL
		tkconfigure(bt.TsGraph.plot, command = function(){
			if(!is.null(EnvClimatoAnalysisplot$tsdata)){
				imgContainer <- climatoAnalysis.DisplayTSPlot(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvClimatoAnalysisplot$notebookTab.tsplot, AllOpenTabType, AllOpenTabData)
				EnvClimatoAnalysisplot$notebookTab.tsplot <- retNBTab$notebookTab
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
			stateType <- if(tclvalue(EnvClimatoAnalysisplot$graph$typeTSp)%in%c("Line", "ENSO-Line")) "normal" else "disabled"
			tkconfigure(chk.meanTSp, state = stateType)
			tkconfigure(chk.tercTSp, state = stateType)
			tkconfigure(chk.trendTSp, state = stateType)
		})

		tkbind(chk.meanTSp, "<Button-1>", function(){
			EnvClimatoAnalysisplot$TSGraphOp$line$legend$add$mean <- 
						if(tclvalue(EnvClimatoAnalysisplot$graph$averageTSp) == '0') TRUE else FALSE
			EnvClimatoAnalysisplot$TSGraphOp$line.enso$legend$add$mean <- 
						if(tclvalue(EnvClimatoAnalysisplot$graph$averageTSp) == '0') TRUE else FALSE
		})

		tkbind(chk.tercTSp, "<Button-1>", function(){
			EnvClimatoAnalysisplot$TSGraphOp$line$legend$add$tercile <- 
						if(tclvalue(EnvClimatoAnalysisplot$graph$tercileTSp) == '0') TRUE else FALSE
			EnvClimatoAnalysisplot$TSGraphOp$line.enso$legend$add$tercile <- 
						if(tclvalue(EnvClimatoAnalysisplot$graph$tercileTSp) == '0') TRUE else FALSE
		})

		tkbind(chk.trendTSp, "<Button-1>", function(){
			EnvClimatoAnalysisplot$TSGraphOp$line$legend$add$linear <- 
						if(tclvalue(EnvClimatoAnalysisplot$graph$trendTSp) == '0') TRUE else FALSE
			EnvClimatoAnalysisplot$TSGraphOp$line.enso$legend$add$linear <- 
						if(tclvalue(EnvClimatoAnalysisplot$graph$trendTSp) == '0') TRUE else FALSE
		})

		##############################################

		frameSTNCrds <- ttklabelframe(subfr4, text = "Station/Coordinates", relief = 'groove')

		frTS2 <- tkframe(frameSTNCrds)
		EnvClimatoAnalysisplot$graph$lonLOC <- tclVar()
		EnvClimatoAnalysisplot$graph$latLOC <- tclVar()
		EnvClimatoAnalysisplot$graph$stnIDTSp <- tclVar()

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

		##############################################

		tkgrid(frameTSPlot, row = 0, column = 0, sticky = 'we', pady = 1)
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

		EnvClimatoAnalysisplot$shp$add.shp <- tclVar(FALSE)
		file.plotShp <- tclVar()
		stateSHP <- "disabled"

		chk.addshp <- tkcheckbutton(frameSHP, variable = EnvClimatoAnalysisplot$shp$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
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

				tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
				tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = file.plotShp)

				shpofile <- getShpOpenData(file.plotShp)
				if(is.null(shpofile)) EnvClimatoAnalysisplot$shp$ocrds <- NULL
				EnvClimatoAnalysisplot$shp$ocrds <- getBoundaries(shpofile[[2]])
			}else return(NULL)
		})

		########
		EnvClimatoAnalysisplot$SHPOp <- list(col = "black", lwd = 1.5)

		tkconfigure(bt.addshpOpt, command = function(){
			EnvClimatoAnalysisplot$SHPOp <- climatoAnalysis.GraphOptions.LineSHP(main.win, EnvClimatoAnalysisplot$SHPOp)
		})

		########
		tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
		tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		#################
		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile)) EnvClimatoAnalysisplot$shp$ocrds <- NULL
			EnvClimatoAnalysisplot$shp$ocrds <- getBoundaries(shpofile[[2]])
		})

		tkbind(chk.addshp, "<Button-1>", function(){
			stateSHP <- if(tclvalue(EnvClimatoAnalysisplot$shp$add.shp) == "1") "disabled" else "normal"
			tkconfigure(cb.addshp, state = stateSHP)
			tkconfigure(bt.addshp, state = stateSHP)
			tkconfigure(bt.addshpOpt, state = stateSHP)
		})

		##############################################

		tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', pady = 1)

	#######################################################################################################

	load.ClimatoAnalysis.Data <- function(){
		if("Anomaly"%in%EnvClimatoAnalysisplot$DirStat$Stats){
			climato.maps.Values <- EnvClimatoAnalysisplot$DirStat$Stats[!EnvClimatoAnalysisplot$DirStat$Stats%in%"Anomaly"]
			if(length(climato.maps.Values) == 0){
				tkconfigure(cb.climato.maps, values = "", state = 'disabled')
				tclvalue(EnvClimatoAnalysisplot$climStat) <- "Anomaly"
			}else{
				tkconfigure(cb.climato.maps, values = climato.maps.Values, state = 'normal')
				lastVal <- if(EnvClimatoAnalysisplot$DirStat$last == "Anomaly") climato.maps.Values[1] else EnvClimatoAnalysisplot$DirStat$last
				tclvalue(EnvClimatoAnalysisplot$climStat) <- lastVal
			}
		}else{
			tkconfigure(cb.climato.maps, values = EnvClimatoAnalysisplot$DirStat$Stats, state = 'normal')
			tclvalue(EnvClimatoAnalysisplot$climStat) <- EnvClimatoAnalysisplot$DirStat$last
		}

		###################
		TSDATA <- if("Anomaly"%in%EnvClimatoAnalysisplot$DirStat$Stats) c("Data", "Anomaly") else "Data"
		tkconfigure(cb.TSData, values = TSDATA)
		tclvalue(EnvClimatoAnalysisplot$TSData) <- "Data"

		###################
		analysis.path <- file.path(EnvClimatoAnalysisplot$PathStat, tclvalue(EnvClimatoAnalysisplot$climStat))
		params <- try(readRDS(file.path(analysis.path, "params.rds")), silent = TRUE)
		if(inherits(params, "try-error")){
			InsertMessagesTxt(main.txt.out, 'Unable to load Climate Analysis data', format = TRUE)
			InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', params[1]), format = TRUE)
			tkconfigure(cb.climDate, values = "")
			tclvalue(EnvClimatoAnalysisplot$climDate) <- ""
			return(NULL)
		}
		tkconfigure(cb.climDate, values = params$stats)
		tclvalue(EnvClimatoAnalysisplot$climDate) <- params$stats[1]
		stateclimDate <- if(params$params$time.series$out.series == "monthly") "normal" else "disabled"
		tkconfigure(cb.climDate, state = stateclimDate)

		EnvClimatoAnalysisplot$statpars <- params

		###################
		ret <- EnvClimatoAnalysisplot$read.ClimStat()
		if(is.null(ret)) return(NULL)

		###################
		tkconfigure(cb.TSDate, values = params$timeseries[[1]][[2]])
		tclvalue(EnvClimatoAnalysisplot$TSDate) <- params$timeseries[[1]][[2]][1]

		###################
		ret1 <- EnvClimatoAnalysisplot$read.ClimTSData()
		if(is.null(ret1)) return(NULL)

		###################
		xlim.years <- substr(EnvClimatoAnalysisplot$statpars$stats[1], 1, 9)
		xlim.year1 <- as.numeric(substr(xlim.years, 1, 4))
		xlim.year2 <- as.numeric(substr(xlim.years, 6, 9))
		plotCHOIX <- c("anomaly", "bar", "line", "line.enso", "bar.enso")
		for(pp in plotCHOIX){
			EnvClimatoAnalysisplot$TSGraphOp[[pp]]$xlim$min <- xlim.year1
			EnvClimatoAnalysisplot$TSGraphOp[[pp]]$xlim$max <- xlim.year2
		}

		###################

		tkdestroy(frTS2)
		frTS2 <<- tkframe(frameSTNCrds)

		if(EnvClimatoAnalysisplot$statpars$params$data.type == "cdtstation"){
			stnIDTSPLOT <- EnvClimatoAnalysisplot$tsdata$id
			txt.stnSel <- tklabel(frTS2, text = "Select a station to plot", anchor = 'w', justify = 'left')
			txt.stnID <- tklabel(frTS2, text = "Station", anchor = 'e', justify = 'right')
			cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = EnvClimatoAnalysisplot$graph$stnIDTSp, width = largeur6)
			tclvalue(EnvClimatoAnalysisplot$graph$stnIDTSp) <- stnIDTSPLOT[1]

			tkgrid(txt.stnSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.stnID, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}else{
			txt.crdSel <- tklabel(frTS2, text = "Enter longitude and latitude to plot", anchor = 'w', justify = 'left')
			txt.lonLoc <- tklabel(frTS2, text = "Longitude", anchor = 'e', justify = 'right')
			en.lonLoc <- tkentry(frTS2, textvariable = EnvClimatoAnalysisplot$graph$lonLOC, width = 8)
			txt.latLoc <- tklabel(frTS2, text = "Latitude", anchor = 'e', justify = 'right')
			en.latLoc <- tkentry(frTS2, textvariable = EnvClimatoAnalysisplot$graph$latLOC, width = 8)
			stnIDTSPLOT <- ""
			tclvalue(EnvClimatoAnalysisplot$graph$stnIDTSp) <- ""

			tkgrid(txt.crdSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.lonLoc, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.lonLoc, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.latLoc, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.latLoc, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}
		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)
	}

	#######################################################################################################

	EnvClimatoAnalysisplot$read.ClimStat <- function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		if(tclvalue(EnvClimatoAnalysisplot$climStat) == "Anomaly") return(0)

		analysis.path <- file.path(EnvClimatoAnalysisplot$PathStat, tclvalue(EnvClimatoAnalysisplot$climStat))
		extFile <- if(EnvClimatoAnalysisplot$statpars$params$data.type == "cdtstation") ".csv" else ".nc"

		filestat <- file.path(analysis.path, paste0(EnvClimatoAnalysisplot$statpars$params$analysis.method$mth.fun,
							"_", tclvalue(EnvClimatoAnalysisplot$climDate), extFile))
		if(!file.exists(filestat)){
			InsertMessagesTxt(main.txt.out, paste(filestat, 'not found'), format = TRUE)
			return(NULL)
		}

		readClimData <- TRUE
		if(!is.null(EnvClimatoAnalysisplot$don))
			if(!is.null(EnvClimatoAnalysisplot$filestat))
				if(EnvClimatoAnalysisplot$filestat == filestat) readClimData <- FALSE

		if(EnvClimatoAnalysisplot$statpars$params$data.type == "cdtstation"){
			if(readClimData){
				don <- data.table::fread(filestat, header = FALSE, sep = ",", data.table = FALSE,
											stringsAsFactors = FALSE, colClasses = "character")

				if(tclvalue(EnvClimatoAnalysisplot$climStat) == "Trend"){
					EnvClimatoAnalysisplot$don <- list(id = as.character(don[1, -1]), 
														x0 = as.numeric(don[2, -1]),
														y0 = as.numeric(don[3, -1]), 
														var = as.numeric(don[4, -1]),
														# std.slope = as.numeric(don[5, -1]),
														p.value = as.numeric(don[6, -1]),
														# r2 = as.numeric(don[7, -1]),
														na = as.numeric(don[8, -1]))
					nx <- nx_ny_as.image(diff(range(EnvClimatoAnalysisplot$don$x0)))
					ny <- nx_ny_as.image(diff(range(EnvClimatoAnalysisplot$don$y0)))
					tmp <- as.image(EnvClimatoAnalysisplot$don$var, nx = nx, ny = ny,
									x = cbind(EnvClimatoAnalysisplot$don$x0, EnvClimatoAnalysisplot$don$y0))
					EnvClimatoAnalysisplot$don$x <- tmp$x
					EnvClimatoAnalysisplot$don$y <- tmp$y
					EnvClimatoAnalysisplot$don$z <- tmp$z
					# tmp <- as.image(EnvClimatoAnalysisplot$don$std.slope, nx = nx, ny = ny,
					# 				x = cbind(EnvClimatoAnalysisplot$don$x0, EnvClimatoAnalysisplot$don$y0))
					# EnvClimatoAnalysisplot$don$std <- tmp$z
					tmp <- as.image(EnvClimatoAnalysisplot$don$p.value, nx = nx, ny = ny,
									x = cbind(EnvClimatoAnalysisplot$don$x0, EnvClimatoAnalysisplot$don$y0))
					EnvClimatoAnalysisplot$don$pval <- tmp$z
					# tmp <- as.image(EnvClimatoAnalysisplot$don$r2, nx = nx, ny = ny,
					# 				x = cbind(EnvClimatoAnalysisplot$don$x0, EnvClimatoAnalysisplot$don$y0))
					# EnvClimatoAnalysisplot$don$r2 <- tmp$z
					tmp <- as.image(EnvClimatoAnalysisplot$don$na, nx = nx, ny = ny,
									x = cbind(EnvClimatoAnalysisplot$don$x0, EnvClimatoAnalysisplot$don$y0))
					EnvClimatoAnalysisplot$don$na <- tmp$z
				}else{
					EnvClimatoAnalysisplot$don <- list(id = as.character(don[1, -1]), 
														x0 = as.numeric(don[2, -1]),
														y0 = as.numeric(don[3, -1]), 
														var = as.numeric(don[4, -1]),
														na = as.numeric(don[5, -1]))
					nx <- nx_ny_as.image(diff(range(EnvClimatoAnalysisplot$don$x0)))
					ny <- nx_ny_as.image(diff(range(EnvClimatoAnalysisplot$don$y0)))
					tmp <- as.image(EnvClimatoAnalysisplot$don$var, nx = nx, ny = ny,
									x = cbind(EnvClimatoAnalysisplot$don$x0, EnvClimatoAnalysisplot$don$y0))
					EnvClimatoAnalysisplot$don$x <- tmp$x
					EnvClimatoAnalysisplot$don$y <- tmp$y
					EnvClimatoAnalysisplot$don$z <- tmp$z
					tmp <- as.image(EnvClimatoAnalysisplot$don$na, nx = nx, ny = ny,
									x = cbind(EnvClimatoAnalysisplot$don$x0, EnvClimatoAnalysisplot$don$y0))
					EnvClimatoAnalysisplot$don$na <- tmp$z
				}
				EnvClimatoAnalysisplot$filestat <- filestat
				rm(don, tmp)
			}
		}else{
			if(readClimData){
				if(tclvalue(EnvClimatoAnalysisplot$climStat) == "Trend"){
					nc <- nc_open(filestat)
					EnvClimatoAnalysisplot$don$x <- nc$dim[[1]]$vals
					EnvClimatoAnalysisplot$don$y <- nc$dim[[2]]$vals
					EnvClimatoAnalysisplot$don$z <- ncvar_get(nc, varid = "trend")
					# EnvClimatoAnalysisplot$don$std <- ncvar_get(nc, varid = "std.slope")
					EnvClimatoAnalysisplot$don$pval <- ncvar_get(nc, varid = "pvalue")
					# EnvClimatoAnalysisplot$don$r2 <- ncvar_get(nc, varid = "r2")
					EnvClimatoAnalysisplot$don$na <- ncvar_get(nc, varid = "nonNA")
					nc_close(nc)
				}else{
					nc <- nc_open(filestat)
					EnvClimatoAnalysisplot$don$x <- nc$dim[[1]]$vals
					EnvClimatoAnalysisplot$don$y <- nc$dim[[2]]$vals
					EnvClimatoAnalysisplot$don$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
					EnvClimatoAnalysisplot$don$na <- ncvar_get(nc, varid = "nonNA")
					nc_close(nc)
				}
				EnvClimatoAnalysisplot$filestat <- filestat
			}
		}
		return(0)
	}

	##############################################

	EnvClimatoAnalysisplot$read.ClimTSData <- function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		tsdata.path <- file.path(EnvClimatoAnalysisplot$PathStat, "Aggregated_TimeSeries")

		if(EnvClimatoAnalysisplot$statpars$params$data.type == "cdtstation"){
			filetsdata <- file.path(tsdata.path, paste0("outTS", "_", tclvalue(EnvClimatoAnalysisplot$climDate), ".csv"))
			if(!file.exists(filetsdata)){
				InsertMessagesTxt(main.txt.out, paste(filetsdata, 'not found'), format = TRUE)
				return(NULL)
			}

			readTsData <- TRUE
			if(!is.null(EnvClimatoAnalysisplot$tsdata))
				if(!is.null(EnvClimatoAnalysisplot$filetsdata))
					if(EnvClimatoAnalysisplot$filetsdata == filetsdata) readTsData <- FALSE

			if(readTsData){
				don <- data.table::fread(filetsdata, header = FALSE, sep = ",", data.table = FALSE,
											stringsAsFactors = FALSE, colClasses = "character")

				EnvClimatoAnalysisplot$tsdata <- list(id = as.character(don[1, -1]), 
													x0 = as.numeric(don[2, -1]),
													y0 = as.numeric(don[3, -1]), 
													date = as.character(don[-(1:3), 1]),
													data = apply(don[-(1:3), -1], 2, as.numeric))
				EnvClimatoAnalysisplot$filetsdata <- filetsdata
				rm(don)
			}

			########
			rasterTsData <- TRUE
			if(!readTsData)
				if(!is.null(EnvClimatoAnalysisplot$rasterTsData))
					if(EnvClimatoAnalysisplot$filetsdata == filetsdata)
						if(EnvClimatoAnalysisplot$rasterTsData == tclvalue(EnvClimatoAnalysisplot$TSDate)) rasterTsData <- FALSE

			if(rasterTsData){
				idt <- which(EnvClimatoAnalysisplot$tsdata$date == tclvalue(EnvClimatoAnalysisplot$TSDate))
				nx <- nx_ny_as.image(diff(range(EnvClimatoAnalysisplot$tsdata$x0)))
				ny <- nx_ny_as.image(diff(range(EnvClimatoAnalysisplot$tsdata$y0)))
				tmp <- as.image(as.numeric(EnvClimatoAnalysisplot$tsdata$data[idt, ]), nx = nx, ny = ny,
								x = cbind(EnvClimatoAnalysisplot$tsdata$x0, EnvClimatoAnalysisplot$tsdata$y0))
				EnvClimatoAnalysisplot$tsdata$x <- tmp$x
				EnvClimatoAnalysisplot$tsdata$y <- tmp$y
				EnvClimatoAnalysisplot$tsdata$z <- tmp$z
				EnvClimatoAnalysisplot$rasterTsData <- tclvalue(EnvClimatoAnalysisplot$TSDate)
				rm(tmp)
			}

			if("Anomaly"%in%EnvClimatoAnalysisplot$DirStat$Stats){
				anom.path <- file.path(EnvClimatoAnalysisplot$PathStat, "Anomaly")
				file.anom <- file.path(anom.path, paste0("anomaly", "_", tclvalue(EnvClimatoAnalysisplot$climDate), ".csv"))
				if(!file.exists(file.anom)){
					InsertMessagesTxt(main.txt.out, paste(file.anom, 'not found'), format = TRUE)
					return(NULL)
				}

				readAnomData <- TRUE
				if(!is.null(EnvClimatoAnalysisplot$anomData))
					if(!is.null(EnvClimatoAnalysisplot$file.anom))
						if(EnvClimatoAnalysisplot$file.anom == file.anom) readAnomData <- FALSE

				if(readAnomData){
					don <- data.table::fread(file.anom, header = FALSE, sep = ",", data.table = FALSE,
												stringsAsFactors = FALSE, colClasses = "character")

					EnvClimatoAnalysisplot$anomData <- list(id = as.character(don[1, -1]), 
														x0 = as.numeric(don[2, -1]),
														y0 = as.numeric(don[3, -1]), 
														date = as.character(don[-(1:3), 1]),
														data = apply(don[-(1:3), -1], 2, as.numeric))
					EnvClimatoAnalysisplot$file.anom <- file.anom
					params <- readRDS(file.path(anom.path, "params.rds"))
					EnvClimatoAnalysisplot$anomData$params <- params$params
					rm(don)
				}

				########
				rasterAnomData <- TRUE
				if(!readAnomData)
					if(!is.null(EnvClimatoAnalysisplot$rasterAnomData))
						if(EnvClimatoAnalysisplot$file.anom == file.anom)
							if(EnvClimatoAnalysisplot$rasterAnomData == tclvalue(EnvClimatoAnalysisplot$TSDate)) rasterAnomData <- FALSE

				if(rasterAnomData){
					idt <- which(EnvClimatoAnalysisplot$anomData$date == tclvalue(EnvClimatoAnalysisplot$TSDate))
					nx <- nx_ny_as.image(diff(range(EnvClimatoAnalysisplot$anomData$x0)))
					ny <- nx_ny_as.image(diff(range(EnvClimatoAnalysisplot$anomData$y0)))
					tmp <- as.image(as.numeric(EnvClimatoAnalysisplot$anomData$data[idt, ]), nx = nx, ny = ny,
									x = cbind(EnvClimatoAnalysisplot$anomData$x0, EnvClimatoAnalysisplot$anomData$y0))
					EnvClimatoAnalysisplot$anomData$x <- tmp$x
					EnvClimatoAnalysisplot$anomData$y <- tmp$y
					EnvClimatoAnalysisplot$anomData$z <- tmp$z
					EnvClimatoAnalysisplot$rasterAnomData <- tclvalue(EnvClimatoAnalysisplot$TSDate)
					rm(tmp)
				}
			}
		}else{
			filetsdata <- file.path(tsdata.path, paste0("outTS", "_", tclvalue(EnvClimatoAnalysisplot$TSDate), ".nc"))
			if(!file.exists(filetsdata)){
				InsertMessagesTxt(main.txt.out, paste(filetsdata, 'not found'), format = TRUE)
				return(NULL)
			}

			readTsData <- TRUE
			if(!is.null(EnvClimatoAnalysisplot$tsdata))
				if(!is.null(EnvClimatoAnalysisplot$filetsdata))
					if(EnvClimatoAnalysisplot$filetsdata == filetsdata) readTsData <- FALSE

			if(readTsData){
				nc <- nc_open(filetsdata)
				EnvClimatoAnalysisplot$tsdata$x <- nc$dim[[1]]$vals
				EnvClimatoAnalysisplot$tsdata$y <- nc$dim[[2]]$vals
				EnvClimatoAnalysisplot$tsdata$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
				nc_close(nc)
				EnvClimatoAnalysisplot$filetsdata <- filetsdata
			}

			if(is.null(EnvClimatoAnalysisplot$cdtdataset)){
				fldataset <- basename(EnvClimatoAnalysisplot$statpars$params$in.file)
				fldataset <- file.path(EnvClimatoAnalysisplot$PathStat, paste0("Aggregated_", getf.no.ext(fldataset)), fldataset)
				EnvClimatoAnalysisplot$cdtdataset <- readRDS(fldataset)
				EnvClimatoAnalysisplot$cdtdataset$fileInfo <- fldataset
			}

			if("Anomaly"%in%EnvClimatoAnalysisplot$DirStat$Stats){
				anom.path <- file.path(EnvClimatoAnalysisplot$PathStat, "Anomaly")
				file.anom <- file.path(anom.path, paste0("anomaly", "_", tclvalue(EnvClimatoAnalysisplot$TSDate), ".nc"))
				if(!file.exists(file.anom)){
					InsertMessagesTxt(main.txt.out, paste(file.anom, 'not found'), format = TRUE)
					return(NULL)
				}

				readAnom <- TRUE
				if(!is.null(EnvClimatoAnalysisplot$anomData))
					if(!is.null(EnvClimatoAnalysisplot$file.anom))
						if(EnvClimatoAnalysisplot$file.anom == file.anom) readAnom <- FALSE

				if(readAnom){
					nc <- nc_open(file.anom)
					EnvClimatoAnalysisplot$anomData$x <- nc$dim[[1]]$vals
					EnvClimatoAnalysisplot$anomData$y <- nc$dim[[2]]$vals
					EnvClimatoAnalysisplot$anomData$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
					nc_close(nc)
					EnvClimatoAnalysisplot$file.anom <- file.anom
					params <- readRDS(file.path(anom.path, "params.rds"))
					EnvClimatoAnalysisplot$anomData$params <- params$params
				}
			}
		}

		if(is.null(EnvClimatoAnalysisplot$ONI)){
			load(file.path(apps.dir, 'data', 'ONI_50-2017.RData'))
			EnvClimatoAnalysisplot$ONI$date <- format(seq(as.Date('1950-1-15'), as.Date('2017-12-15'), "month"), "%Y%m")
			EnvClimatoAnalysisplot$ONI$data <- ONI$ts[, 3]
		}

		return(0)
	}

	#######################################################################################################
	tcl('update')
	tkgrid(cmd.frame, sticky = '', pady = 1) #nswe
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)
	######
	return(cmd.frame)
}


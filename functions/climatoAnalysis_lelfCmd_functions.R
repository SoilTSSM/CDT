
climatoAnalysisPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(45)
		largeur0 <- as.integer(w.scale(22)/sfont0)
		largeur1 <- as.integer(w.scale(30)/sfont0)
		largeur2 <- as.integer(w.scale(28)/sfont0)
	}else{
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(47)
		largeur0 <- as.integer(w.scale(18)/sfont0)
		largeur1 <- as.integer(w.scale(22)/sfont0)
		largeur2 <- as.integer(w.scale(23)/sfont0)
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

	file.period <- tclVar()
	CbperiodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(GeneralParameters$in.series, 
									'daily' = CbperiodVAL[1], 
									'dekadal' = CbperiodVAL[2],
									'monthly' = CbperiodVAL[3])

	cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = file.period, width = largeur1)

	tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.fperiod, 'Select the time step of the data')
	status.bar.display(cb.fperiod, TextOutputVar, 'Select the time step of the data')

	#######################

	frameData <- ttklabelframe(subfr1, text = "Type of Data", relief = 'groove')

	DataType <- tclVar()
	CbdatatypeVAL <- c('CDT data format', 'NetCDF gridded data')
	tclvalue(DataType) <- switch(GeneralParameters$data.type,
								'cdt' = CbdatatypeVAL[1], 
								'netcdf' = CbdatatypeVAL[2])

	cb.datatype <- ttkcombobox(frameData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)
	bt.datatype <- ttkbutton(frameData, text = "Settings")

	###############
	if(tclvalue(DataType) == 'CDT data format'){
		tkconfigure(bt.datatype, state = 'disabled')
	}
	if(tclvalue(DataType) == 'NetCDF gridded data'){
		tkconfigure(bt.datatype, state = 'normal', command = function(){
			GeneralParameters <<- climatoAnalysisNetcdfData(main.win, GeneralParameters,
															str_trim(tclvalue(file.stnfl)),
															tclvalue(file.period))
		})
	}

	###############

	tkgrid(cb.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(bt.datatype, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

	infobulle(cb.datatype, 'Select the type of input data')
	status.bar.display(cb.datatype, TextOutputVar, 'Select the type of input data')
	infobulle(bt.datatype, 'Setting netcdf data options')
	status.bar.display(bt.datatype, TextOutputVar, 'Setting netcdf data options')

	###############

	tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
		tkdestroy(cb.stnfl)
		tclvalue(file.stnfl) <- ''

		if(tclvalue(DataType) == 'CDT data format'){
			tclvalue(fileINdir) <- 'File containing stations input data'

			cb.stnfl <- ttkcombobox(frameInd, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)

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

			tkconfigure(bt.datatype, state = 'disabled')

			infobulle(cb.stnfl, 'Select the file in the list')
			status.bar.display(cb.stnfl, TextOutputVar, 'Select the file containing the input data')
		}

		if(tclvalue(DataType) == 'NetCDF gridded data'){
			tclvalue(fileINdir) <- 'Directory containing the NetCDF data'

			cb.stnfl <- tkentry(frameInd, textvariable = file.stnfl, width = largeur2)

			#######
			tkconfigure(bt.stnfl, command = function(){
				file2convert <- tk_choose.dir(getwd(), "")
				tclvalue(file.stnfl) <- if(!is.na(file2convert)) file2convert else ""
			})

			tkconfigure(bt.datatype, state = 'normal', command = function(){
				GeneralParameters <<- climatoAnalysisNetcdfData(main.win, GeneralParameters,
																str_trim(tclvalue(file.stnfl)),
																tclvalue(file.period))
			})

			infobulle(cb.stnfl, 'Enter the full path to directory containing the NetCDF data')
			status.bar.display(cb.stnfl, TextOutputVar, 'Enter the full path to directory containing the NetCDF data')
		}
		
		#######
		tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	})

	#############################

	frameInd <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

	file.stnfl <- tclVar(GeneralParameters$IO.files$In.dir.file)
	fileINdir <- tclVar('File containing stations input data')

	txt.stnfl <- tklabel(frameInd, text = tclvalue(fileINdir), textvariable = fileINdir, anchor = 'w', justify = 'left')
	cb.stnfl <- ttkcombobox(frameInd, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
	bt.stnfl <- tkbutton(frameInd, text = "...")

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
		}else return(NULL)
	})

	###############
	tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.stnfl, 'Select the file in the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'Select the file containing the input data')
	infobulle(bt.stnfl, 'Browse file if not listed')
	status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')

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

	file.save1 <- tclVar(GeneralParameters$IO.files$Out.dir.file)

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
	tkgrid(frameTimeS, row = 0, column = 0, sticky = 'we')
	tkgrid(frameData, row = 1, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameInd, row = 2, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameAggr, row = 3, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameDirSav, row = 4, column = 0, sticky = 'we', pady = 3)

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

	chk.allYears <- tkcheckbutton(frameYear, variable = allYears, text =  "Use all years from the input data", anchor = 'w', justify = 'left', width = largeur2+2)
	txt.startYear <- tklabel(frameYear, text = "Start Year",  anchor = 'e', justify = 'right')
	en.startYear <- tkentry(frameYear, textvariable = startYear, width = 6, state = state0)
	txt.endYear <- tklabel(frameYear, text = "End Year",  anchor = 'e', justify = 'right')
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
	tclvalue(out.series) <- switch(GeneralParameters$out.series$out.series, 
									'monthly' = OUTSeries[1],
									'seasonal' = OUTSeries[2], 
									'annual' = OUTSeries[3])

	mons <- as.numeric(str_trim(GeneralParameters$out.series$start.seas))
	start.seas <- tclVar(MOIS[mons])
	length.seas  <- tclVar(GeneralParameters$out.series$len.seas)

	stateOut <- if(GeneralParameters$out.series$out.series == 'seasonal') "normal" else "disabled"

	txt.outTS <- tklabel(frameOut, text = "Time step",  anchor = 'e', justify = 'right')
	cb.outTS <- ttkcombobox(frameOut, values = OUTSeries, textvariable = out.series, width = 10)
	txt.startSeas <- tklabel(frameOut, text = 'Start Month', anchor = 'e', justify = 'right')
	cb.startSeas <- ttkcombobox(frameOut, values = MOIS, textvariable = start.seas, width = 10, state = stateOut)
	txt.lenSeas <- tklabel(frameOut, text = 'Width')
	en.lenSeas <- tkentry(frameOut, textvariable = length.seas, width = 3, state = stateOut)

	tkgrid(txt.outTS, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.outTS, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.startSeas, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.startSeas, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.lenSeas, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.lenSeas, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.outTS, "Select the time step of the output time series")
	status.bar.display(cb.outTS, TextOutputVar, "Select the time step of the output time series")
	infobulle(cb.startSeas, 'Select the start month of the season')
	status.bar.display(cb.startSeas, TextOutputVar, 'Select the start month of the season')
	infobulle(en.lenSeas, 'Enter the width of the season [in month] (e.g., 3 for three-month season)')
	status.bar.display(en.lenSeas, TextOutputVar, 'Enter the width of the season [in month] (e.g., 3 for three-month season)')

	##############
	tkbind(cb.outTS, "<<ComboboxSelected>>", function(){
		stateOut <- if(tclvalue(out.series) == 'Seasonal') "normal" else "disabled"
		if(tclvalue(out.series) == 'Monthly'){
			state3a <- state3b <- if(tclvalue(nseqMonths) == '1') 'disabled' else 'normal'
			state4 <- if(tclvalue(nseqMonths) == '1') 'normal' else 'disabled'
			state5 <- 'normal'
		}else if(tclvalue(out.series) == 'Annual'){
			state3a <- 'normal'
			state3b <- 'disabled'
			state4 <- 'disabled'
			state5 <- 'disabled'
		}else{
			state3a <- state3b <- 'disabled'
			state4 <- 'disabled'
			state5 <- 'disabled'
		}
		tkconfigure(cb.startSeas, state = stateOut)
		tkconfigure(en.lenSeas, state = stateOut)
		tkconfigure(cb.startMonth, state = state3a)
		tkconfigure(cb.endMonth, state = state3b)
		tkconfigure(bt.customMonth, state = state4)
		tkconfigure(chk.customMonth, state = state5)
	})

	#######################

	frameMonth <- ttklabelframe(subfr2, text = "Months to process", relief = 'groove')

	mon1 <- as.numeric(str_trim(GeneralParameters$time.series$start.month))
	mon2 <- as.numeric(str_trim(GeneralParameters$time.series$end.month))

	startMonth <- tclVar(MOIS[mon1])
	endMonth <- tclVar(MOIS[mon2])
	nseqMonths <- tclVar(GeneralParameters$time.series$nseq.months)

	if(GeneralParameters$out.series$out.series == 'monthly'){
		state3a <- state3b <- if(GeneralParameters$time.series$nseq.months) "disabled" else "normal"
		state4 <- if(GeneralParameters$time.series$nseq.months) "normal" else "disabled"
	}else if(GeneralParameters$out.series$out.series == 'annual'){
		state3a <- 'normal'
		state3b <- 'disabled'
		state4 <- 'disabled'
	}else{
		state3a <- state3b <- 'disabled'
		state4 <- 'disabled'
	}

	txt.startMonth <- tklabel(frameMonth, text = "From",  anchor = 'e', justify = 'right')
	cb.startMonth <- ttkcombobox(frameMonth, values = MOIS, textvariable = startMonth, width = 10, state = state3a)
	txt.endMonth <- tklabel(frameMonth, text = "to")
	cb.endMonth <- ttkcombobox(frameMonth, values = MOIS, textvariable = endMonth, width = 10, state = state3b)
	chk.customMonth <- tkcheckbutton(frameMonth, variable = nseqMonths, text =  "Customized Months", anchor = 'w', justify = 'left')
	bt.customMonth <- tkbutton(frameMonth, text = "Custom", state = state4)

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

	infobulle(bt.customYear, "Edit the years to analyze")
	status.bar.display(bt.customYear, TextOutputVar, "Edit the years to analyze")
	infobulle(bt.customYear, "Edit the years to analyze")
	status.bar.display(bt.customYear, TextOutputVar, "Edit the years to analyze")
	infobulle(chk.customMonth, "Check this box if the months to process are not consecutive")
	status.bar.display(chk.customMonth, TextOutputVar, "Check this box if the months to process are not consecutive")
	infobulle(bt.customMonth, "Edit the months to process")
	status.bar.display(bt.customMonth, TextOutputVar, "Edit the months to process")

	#################

	tkbind(cb.startMonth, "<<ComboboxSelected>>", function(){
		if(tclvalue(out.series) == 'Annual'){
			mon1 <- which(MOIS%in%str_trim(tclvalue(startMonth)))
			mon1 <- (mon1+11)%%12
			mon1[mon1 == 0] <- 12
			tclvalue(endMonth) <- MOIS[mon1]
		}
	})

	tkbind(chk.customMonth, "<Button-1>", function(){
		if(tclvalue(out.series) == 'Monthly'){
			state3 <- if(tclvalue(nseqMonths) == '1') 'normal' else 'disabled'
			state4 <- if(tclvalue(nseqMonths) == '1') 'disabled' else 'normal'
			tkconfigure(cb.startMonth, state = state3)
			tkconfigure(cb.endMonth, state = state3)
			tkconfigure(bt.customMonth, state = state4)
		}
	})

	#######################

	frameAnalysis <- ttklabelframe(subfr2, text = "Analysis Method", relief = 'groove')

	ANALYSIS <- c('Average', 'Median', 'Standard deviation', 'Coefficient of variation',
					'Trend', 'Percentiles', 'Frequency')
	analysis.method <- tclVar()
	tclvalue(analysis.method) <- switch(GeneralParameters$analysis.method$mth.fun, 
									'average' = ANALYSIS[1],
									'median' = ANALYSIS[2], 
									'std' = ANALYSIS[3],
									'cv' = ANALYSIS[4], 
									'trend' = ANALYSIS[5],
									'percentile' = ANALYSIS[6], 
									'frequency' = ANALYSIS[7])
	mth.perc <- tclVar(GeneralParameters$analysis.method$mth.perc)
	low.thres <- tclVar(GeneralParameters$analysis.method$low.thres)
	up.thres <- tclVar(GeneralParameters$analysis.method$up.thres)

	statePrc <- if(GeneralParameters$analysis.method$mth.fun == 'percentile') 'normal' else 'disabled'
	stateFrq <- if(GeneralParameters$analysis.method$mth.fun == 'frequency') 'normal' else 'disabled'

	cb.anMthd <- ttkcombobox(frameAnalysis, values = ANALYSIS, textvariable = analysis.method, width = largeur1)
	txt.Percent <- tklabel(frameAnalysis, text = "Percentile",  anchor = 'e', justify = 'right')
	en.Percent <- tkentry(frameAnalysis, textvariable = mth.perc, width = 6, state = statePrc)
	txt.Freq1 <- tklabel(frameAnalysis, text = "Between",  anchor = 'e', justify = 'right')
	en.Freq1 <- tkentry(frameAnalysis, textvariable = low.thres, width = 6, state = stateFrq)
	txt.Freq2 <- tklabel(frameAnalysis, text = "And")
	en.Freq2 <- tkentry(frameAnalysis, textvariable = up.thres, width = 6, state = stateFrq)

	tkgrid(cb.anMthd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.Percent, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.Percent, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.Freq1, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.Freq1, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.Freq2, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.Freq2, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.anMthd, "Select the analysis method")
	status.bar.display(cb.anMthd, TextOutputVar, "Select the analysis method")
	infobulle(en.Percent, "Enter the nth percentile to be calculated")
	status.bar.display(en.Percent, TextOutputVar, "Enter the nth percentile to be calculated")
	infobulle(en.Freq1, "Enter the lower bound of the interval to count the number of occurrences")
	status.bar.display(en.Freq1, TextOutputVar, "Enter the lower bound of the interval to count the number of occurrences")
	infobulle(en.Freq2, "Enter the upper bound of the interval to count the number of occurrences")
	status.bar.display(en.Freq2, TextOutputVar, "Enter the upper bound of the interval to count the number of occurrences")

	#################
	tkbind(cb.anMthd, "<<ComboboxSelected>>", function(){
		statePrc <- if(tclvalue(analysis.method) == 'Percentiles') 'normal' else 'disabled'
		stateFrq <- if(tclvalue(analysis.method) == 'Frequency') 'normal' else 'disabled'
		tkconfigure(en.Percent, state = statePrc)
		tkconfigure(en.Freq1, state = stateFrq)
		tkconfigure(en.Freq2, state = stateFrq)
	})

	#############################
	tkgrid(frameYear, row = 0, column = 0, sticky = 'we')
	tkgrid(frameOut, row = 1, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameMonth, row = 2, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameAnalysis, row = 3, column = 0, sticky = 'we', pady = 3)

	#############################
	AnalyzeBut <- ttkbutton(subfr2, text = "Calculate")
	tkgrid(AnalyzeBut)

	tkconfigure(AnalyzeBut, command = function(){
		GeneralParameters$data.type <- switch(tclvalue(DataType),
												'CDT data format' = 'cdt',
												'NetCDF gridded data' = 'netcdf')
		GeneralParameters$in.series <- switch(tclvalue(file.period), 
		 									'Daily data' = 'daily',
											'Dekadal data' =  'dekadal',
											'Monthly data' = 'monthly')
		GeneralParameters$IO.files$In.dir.file <- str_trim(tclvalue(file.stnfl))
		GeneralParameters$IO.files$Out.dir.file <- str_trim(tclvalue(file.save1))

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

		GeneralParameters$out.series$out.series <- switch(tclvalue(out.series), 
					 									'Monthly' = 'monthly',
														'Seasonal' =  'seasonal',
														'Annual' = 'annual')
		GeneralParameters$out.series$start.seas <- which(MOIS%in%str_trim(tclvalue(start.seas)))
		GeneralParameters$out.series$len.seas <- as.numeric(str_trim(tclvalue(length.seas)))

		if(GeneralParameters$out.series$out.series == 'monthly'){
			GeneralParameters$time.series$nseq.months <- switch(tclvalue(nseqMonths), '0' = FALSE, '1' = TRUE)
			if(GeneralParameters$time.series$nseq.months){
				if(length(GeneralParameters$time.series$custom.months[[1]]) < 1){
					InsertMessagesTxt(main.txt.out, "No months selected.", format = TRUE)
					return(NULL)
				}
				if(any(is.na(GeneralParameters$time.series$custom.months[[1]]))){
					InsertMessagesTxt(main.txt.out, "The edited months contain missing values.", format = TRUE)
					return(NULL)
				}
			}else{
				GeneralParameters$time.series$start.month <- which(MOIS%in%str_trim(tclvalue(startMonth)))
				GeneralParameters$time.series$end.month <- which(MOIS%in%str_trim(tclvalue(endMonth)))
				if(GeneralParameters$time.series$end.month < GeneralParameters$time.series$start.month){
					InsertMessagesTxt(main.txt.out, "The starting month must be less than the ending month.", format = TRUE)
					return(NULL)
				}
			}
		}

		if(GeneralParameters$out.series$out.series == 'annual'){
			GeneralParameters$time.series$start.month <- which(MOIS%in%str_trim(tclvalue(startMonth)))
			GeneralParameters$time.series$end.month <- which(MOIS%in%str_trim(tclvalue(endMonth)))
		}

		GeneralParameters$analysis.method$mth.fun <- switch(tclvalue(analysis.method),
															'Average' = 'average',
															'Median' = 'median',
															'Standard deviation' = 'std',
															'Coefficient of variation' = 'cv',
															'Trend' = 'trend',
															'Percentiles' = 'percentile',
															'Frequency' = 'frequency')
		if(GeneralParameters$analysis.method$mth.fun == 'percentile'){
			GeneralParameters$analysis.method$mth.perc <- as.numeric(str_trim(tclvalue(mth.perc)))
			if(is.na(GeneralParameters$analysis.method$mth.perc)){
				InsertMessagesTxt(main.txt.out, "Percentile is misssing.", format = TRUE)
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

		# assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)

		tkconfigure(main.win, cursor = 'watch')
		InsertMessagesTxt(main.txt.out, paste("Calculating", tclvalue(analysis.method), "......."))
		ret <- tryCatch(
			climatoAnalysisProcs(GeneralParameters),
			#warning = function(w) warningFun(w),
			error = function(e){
				errorFun(e)
			},
			finally = {
				tkconfigure(main.win, cursor = '')
			}
		)

		if(!is.null(ret)){
			if(ret == 0) InsertMessagesTxt(main.txt.out, paste(tclvalue(analysis.method), "calculation finished successfully"))
			else InsertMessagesTxt(main.txt.out, paste(tclvalue(analysis.method), "calculation failed"), format = TRUE)
		}else{
			InsertMessagesTxt(main.txt.out, paste(tclvalue(analysis.method), "calculation failed"), format = TRUE)
		}
	})


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


	#######################################################################################################
	tcl('update')
	tkgrid(cmd.frame, sticky = 'nswe', pady = 5)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)
	######
	return(cmd.frame)
}


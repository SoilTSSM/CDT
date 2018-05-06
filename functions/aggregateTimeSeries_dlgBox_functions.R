
AggregateTS_GetInfo <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur <- 47
		largeur1 <- 44
		largeur2 <- 34
		wtkcombo <- 20
	}else{
		largeur <- 33
		largeur1 <- 32
		largeur2 <- 24
		wtkcombo <- 16
	}

	###################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frAGGRTS <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	############################################

	frConvTS <- ttklabelframe(frAGGRTS, text = 'Time Series Aggregation', labelanchor = "nw", relief = "groove", borderwidth = 2)

	OriginData <- tclVar()
	Cbperiod0VAL <- c('Daily data', 'Pentad data', 'Dekadal data', 'Monthly data')
	tclvalue(OriginData) <- switch(GeneralParameters$in.tstep, 
									'daily' = Cbperiod0VAL[1],
									'pentad' = Cbperiod0VAL[2],
									'dekadal' = Cbperiod0VAL[3],
									'monthly' = Cbperiod0VAL[4])
	ConvertData <- tclVar()
	Cbperiod1VAL <- c('Pentad data', 'Dekadal data', 'Monthly data', 'Annual data',
						'Seasonal data', 'Rolling Seasonal data')
	tclvalue(ConvertData) <- switch(GeneralParameters$out.tstep, 
									'pentad' = Cbperiod1VAL[1],
									'dekadal' = Cbperiod1VAL[2],
									'monthly' = Cbperiod1VAL[3],
									'annual' = Cbperiod1VAL[4],
									'seasonal' = Cbperiod1VAL[5], 
									'roll.seas' = Cbperiod1VAL[6])

	start.mon <- tclVar(GeneralParameters$Seasonal$start.mon)
	length.mon <- tclVar(GeneralParameters$Seasonal$length.mon)

	state.enSeasS <- if(tclvalue(ConvertData) == 'Seasonal data') "normal" else "disabled"
	state.enSeasL <- if(tclvalue(ConvertData)%in%c('Seasonal data', 'Rolling Seasonal data')) "normal" else "disabled"

	cb.intstep <- ttkcombobox(frConvTS, values = Cbperiod0VAL, textvariable = OriginData, width = wtkcombo)
	cb.outstep <- ttkcombobox(frConvTS, values = Cbperiod1VAL, textvariable = ConvertData, width = wtkcombo)
	txt.convTs <- tklabel(frConvTS, text = 'TO')
	txt.seasS <- tklabel(frConvTS, text = 'Start Month', anchor = 'e', justify = 'right')
	en.seasS <- tkentry(frConvTS, textvariable = start.mon, width = 3, state = state.enSeasS)
	txt.seasL <- tklabel(frConvTS, text = 'Length')
	en.seasL <- tkentry(frConvTS, textvariable = length.mon, width = 3, state = state.enSeasL)

	tkgrid(cb.intstep, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)
	tkgrid(txt.convTs, row = 0, column = 8, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(cb.outstep, row = 0, column = 9, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)
	tkgrid(txt.seasS, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
	tkgrid(en.seasS, row = 1, column = 11, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(txt.seasL, row = 1, column = 12, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1)
	tkgrid(en.seasL, row = 1, column = 16, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)


	infobulle(cb.intstep, 'Select the time step of the input data to aggregate')
	status.bar.display(cb.intstep, TextOutputVar, 'Select the time step of the input data to aggregate')
	infobulle(cb.outstep, 'Select the time step of the aggregated data')
	status.bar.display(cb.outstep, TextOutputVar, 'Select the time step of the aggregated data')
	infobulle(en.seasS, 'Enter the start month of the season (1 to 12)')
	status.bar.display(en.seasS, TextOutputVar, 'Enter the start month of the season (1 to 12)')
	infobulle(en.seasL, 'Enter the width of the season (e.g., 3 for three-month season)')
	status.bar.display(en.seasL, TextOutputVar, 'Enter the width of the season (e.g., 3 for three-month season)')

	##############
	tkbind(cb.intstep, "<<ComboboxSelected>>", function(){
		if(tclvalue(OriginData) == 'Daily data'){
			tkconfigure(cb.outstep, values = Cbperiod1VAL)
			if(tclvalue(ConvertData) == Cbperiod0VAL[1]) tclvalue(ConvertData) <- 'Pentad data'
		}else if(tclvalue(OriginData) == 'Pentad data'){
			tkconfigure(cb.outstep, values = Cbperiod1VAL[-1])
			if(tclvalue(ConvertData)%in%Cbperiod0VAL[1:2]) tclvalue(ConvertData) <- 'Dekadal data'
		}else if(tclvalue(OriginData) == 'Dekadal data'){
			tkconfigure(cb.outstep, values = Cbperiod1VAL[-(1:2)])
			if(tclvalue(ConvertData)%in%Cbperiod0VAL[1:3]) tclvalue(ConvertData) <- 'Monthly data'
		}else if(tclvalue(OriginData) == 'Monthly data'){
			tkconfigure(cb.outstep, values = Cbperiod1VAL[-(1:3)])
			if(tclvalue(ConvertData)%in%Cbperiod0VAL[1:4]) tclvalue(ConvertData) <- 'Annual data'
		}

		state.enSeasS <- if(tclvalue(ConvertData) == 'Seasonal data') "normal" else "disabled"
		state.enSeasL <- if(tclvalue(ConvertData)%in%c('Seasonal data', 'Rolling Seasonal data')) "normal" else "disabled"
		tkconfigure(en.seasS, state = state.enSeasS)
		tkconfigure(en.seasL, state = state.enSeasL)

		AGGRFUN <- c("mean", "sum", "max", "min")
		if(tclvalue(OriginData) == 'Daily data') AGGRFUN <- c("mean", "sum", "max", "min", "count")
		tkconfigure(cb.aggfun, values = AGGRFUN)
		if(tclvalue(aggr.fun) == "count") tclvalue(aggr.fun) <- "sum"
		stateCount <- if(tclvalue(aggr.fun) == "count") "normal" else "disabled"
		tkconfigure(cb.opfun, state = stateCount)
		tkconfigure(en.opthres, state = stateCount)
	})

	tkbind(cb.outstep, "<<ComboboxSelected>>", function(){
		state.enSeasS <- if(tclvalue(ConvertData) == 'Seasonal data') "normal" else "disabled"
		state.enSeasL <- if(tclvalue(ConvertData)%in%c('Seasonal data', 'Rolling Seasonal data')) "normal" else "disabled"
		tkconfigure(en.seasS, state = state.enSeasS)
		tkconfigure(en.seasL, state = state.enSeasL)
	})

	############################################

	frDataType <- ttklabelframe(frAGGRTS, text = 'Type of Data', labelanchor = "nw", relief = "groove", borderwidth = 2)

	DataType <- tclVar()
	CbdatatypeVAL <- c('CDT stations data format', 'CDT dataset format (gridded)', 'NetCDF gridded data')
	tclvalue(DataType) <- switch(GeneralParameters$data.type,
								'cdtstation' = CbdatatypeVAL[1],
								'cdtdataset' = CbdatatypeVAL[2],
								'cdtnetcdf' = CbdatatypeVAL[3])

	if(GeneralParameters$data.type == 'cdtstation'){
		file.stnfl <- tclVar(GeneralParameters$cdtstation)
		txtFileDir <- 'File containing stations input data'
		stateSetData <- "disabled"
	}else if(GeneralParameters$data.type == 'cdtdataset'){
		file.stnfl <- tclVar(GeneralParameters$cdtdataset)
		txtFileDir <- 'Index file (*.rds) of the dataset'
		stateSetData <- "disabled"
	}else{
		file.stnfl <- tclVar(GeneralParameters$cdtnetcdf$dir)
		txtFileDir <- 'Directory containing the NetCDF data'
		stateSetData <- "normal"
	}
	fileINdir <- tclVar(txtFileDir)

	##############
	cb.datatype <- ttkcombobox(frDataType, values = CbdatatypeVAL, textvariable = DataType, width = largeur2)
	set.datatype <- ttkbutton(frDataType, text = "Settings", state = stateSetData)

	txt.stnfl <- tklabel(frDataType, text = tclvalue(fileINdir), textvariable = fileINdir, anchor = 'w', justify = 'left')
	if(GeneralParameters$data.type == 'cdtstation'){
		cb.stnfl <- ttkcombobox(frDataType, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
	}else{
		cb.stnfl <- tkentry(frDataType, textvariable = file.stnfl, width = largeur)
	}
	bt.stnfl <- tkbutton(frDataType, text = "...")

	##############

	settingdone <- GeneralParameters$settingdone
	tkconfigure(set.datatype, command = function(){
		GeneralParameters <<- AggregateTS_ncdfData(tt, GeneralParameters,
												str_trim(tclvalue(file.stnfl)),
												tclvalue(OriginData))
		settingdone <<- 1
	})

	tkconfigure(bt.stnfl, command = function(){
		if(GeneralParameters$data.type == 'cdtstation'){
			dat.opfiles <- getOpenFiles(tt, all.opfiles)
			if(!is.null(dat.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'ascii'
				AllOpenFilesData[[nopf+1]] <<- dat.opfiles

				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
				tclvalue(file.stnfl) <- AllOpenFilesData[[nopf+1]][[1]]
				tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			}else return(NULL)
		}else if(GeneralParameters$data.type == 'cdtdataset'){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			tclvalue(file.stnfl) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
		}else{
			dirnc <- tk_choose.dir(getwd(), "")
			tclvalue(file.stnfl) <- if(dirnc%in%c("", "NA") | is.na(dirnc)) "" else dirnc
		}
	})

	##############
	tkgrid(cb.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(set.datatype, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	##############
	if(GeneralParameters$data.type == 'cdtstation'){
		infobulle(cb.stnfl, 'Select the file containing the data to aggregate')
		status.bar.display(cb.stnfl, TextOutputVar, 'Select the file containing the data to aggregate')
		infobulle(bt.stnfl, 'Browse file if not listed')
		status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')
	}else if(GeneralParameters$data.type == 'cdtdataset'){
		infobulle(cb.stnfl, 'Enter the full path to the file <dataset name>.rds')
		status.bar.display(cb.stnfl, TextOutputVar, 'Enter the full path to the file <dataset name>.rds')
		infobulle(bt.stnfl, 'or browse here')
		status.bar.display(bt.stnfl, TextOutputVar, 'or browse here')
	}else{
		infobulle(cb.stnfl, 'Enter the full path to directory containing the NetCDF data')
		status.bar.display(cb.stnfl, TextOutputVar, 'Enter the full path to directory containing the NetCDF data')
		infobulle(bt.stnfl, 'or browse here')
		status.bar.display(bt.stnfl, TextOutputVar, 'or browse here')
	}

	##############

	tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
		tkdestroy(cb.stnfl)
		tclvalue(file.stnfl) <- ''

		####
		if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
			tclvalue(fileINdir) <- 'File containing stations input data'
			tclvalue(fileORdir) <- 'File to save aggregated data'

			cb.stnfl <- ttkcombobox(frDataType, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)

			#######
			tkconfigure(bt.stnfl, command = function(){
				dat.opfiles <- getOpenFiles(tt, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(file.stnfl) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
				}else return(NULL)
			})
			tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save, isFile = TRUE))
			tkconfigure(set.datatype, state = 'disabled')

			infobulle(cb.stnfl, 'Select the file in the list')
			status.bar.display(cb.stnfl, TextOutputVar, 'Select the file containing the data to aggregate')
			infobulle(bt.stnfl, 'Browse file if not listed')
			status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')
			infobulle(en.file.save, 'Enter the full path of the file to save result')
			status.bar.display(en.file.save, TextOutputVar, 'Enter the full path of the file to save aggregated data')
		}

		####
		if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)'){
			tclvalue(fileINdir) <- 'Index file (*.rds) of the dataset'
			tclvalue(fileORdir) <- 'Directory to save result'

			cb.stnfl <- tkentry(frDataType, textvariable = file.stnfl, width = largeur)

			#######
			tkconfigure(bt.stnfl, command = function(){
				filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
				tclvalue(file.stnfl) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
			})

			tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save, isFile = FALSE))
			tkconfigure(set.datatype, state = 'disabled')

			#######
			infobulle(cb.stnfl, 'Enter the full path to the file <dataset name>.rds')
			status.bar.display(cb.stnfl, TextOutputVar, 'Enter the full path to the file <dataset name>.rds')
			infobulle(bt.stnfl, 'or browse here')
			status.bar.display(bt.stnfl, TextOutputVar, 'or browse here')
			infobulle(en.file.save, 'Enter the full path to the directory to save result')
			status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to the directory to save result')
		}

		####
		if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data'){
			tclvalue(fileINdir) <- 'Directory containing the NetCDF data'
			tclvalue(fileORdir) <- 'Directory to save result'

			cb.stnfl <- tkentry(frDataType, textvariable = file.stnfl, width = largeur)

			#######
			tkconfigure(bt.stnfl, command = function(){
				dirnc <- tk_choose.dir(getwd(), "")
				tclvalue(file.stnfl) <- if(dirnc%in%c("", "NA") | is.na(dirnc)) "" else dirnc
			})

			tkconfigure(set.datatype, command = function(){
				GeneralParameters <<- AggregateTS_ncdfData(tt, GeneralParameters,
														str_trim(tclvalue(file.stnfl)),
														tclvalue(OriginData))
				settingdone <<- 1
			})

			tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save, isFile = FALSE))
			tkconfigure(set.datatype, state = 'normal')

			#######
			infobulle(cb.stnfl, 'Enter the full path to directory containing the NetCDF data')
			status.bar.display(cb.stnfl, TextOutputVar, 'Enter the full path to directory containing the NetCDF data')
			infobulle(bt.stnfl, 'or browse here')
			status.bar.display(bt.stnfl, TextOutputVar, 'or browse here')
			infobulle(en.file.save, 'Enter the full path to the directory to save result')
			status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to the directory to save result')
		}
		
		#######
		tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkfocus(tt)
	})

	############################################
	frameAggr <- ttklabelframe(frAGGRTS, text = 'Aggregation Function', labelanchor = "nw", relief = "groove", borderwidth = 2)

	aggr.fun <- tclVar(GeneralParameters$aggr.series$aggr.fun)
	min.frac <- tclVar(GeneralParameters$aggr.series$min.frac)
	opr.fun <- tclVar(GeneralParameters$aggr.series$opr.fun)
	opr.thres <- tclVar(GeneralParameters$aggr.series$opr.thres)

	AGGRFUN <- c("mean", "sum", "max", "min")
	if(str_trim(GeneralParameters$in.tstep) == "daily") AGGRFUN <- c("mean", "sum", "max", "min", "count")
	stateCount <- if(str_trim(GeneralParameters$aggr.series$aggr.fun) == "count") 'normal' else 'disabled'

	txt.aggfun <- tklabel(frameAggr, text = 'Function', anchor = 'w', justify = 'left')
	cb.aggfun <- ttkcombobox(frameAggr, values = AGGRFUN, textvariable = aggr.fun, width = 6)
	txt.minfrac <- tklabel(frameAggr, text = 'Min.Frac', anchor = 'w', justify = 'left')
	en.minfrac <- tkentry(frameAggr, textvariable = min.frac, width = 6)
	txt.opfun <- tklabel(frameAggr, text = 'Operator', anchor = 'w', justify = 'left')
	cb.opfun <- ttkcombobox(frameAggr, values = c(">=", ">", "<=", "<"), textvariable = opr.fun, width = 6, state = stateCount)
	txt.opthres <- tklabel(frameAggr, text = 'Threshold', anchor = 'w', justify = 'left')
	en.opthres <- tkentry(frameAggr, textvariable = opr.thres, width = 6, width = 6, state = stateCount)

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
		stateCount <- if(tclvalue(aggr.fun) == "count") "normal" else "disabled"
		tkconfigure(cb.opfun, state = stateCount)
		tkconfigure(en.opthres, state = stateCount)
	})

	############################################
	frSave <- tkframe(frAGGRTS, relief = 'groove', borderwidth = 2)

	file.save <- tclVar(GeneralParameters$output)

	if(GeneralParameters$data.type == 'cdtstation'){
		txtSaveDir <- 'File to save aggregated data'
		isFile <- TRUE
	}else{
		txtSaveDir <- 'Directory to save aggregated data'
		isFile <- FALSE
	}
	fileORdir <- tclVar(txtSaveDir)

	txt.file.save <- tklabel(frSave, text = tclvalue(fileORdir), textvariable = fileORdir, anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frSave, textvariable = file.save, width = largeur)
	bt.file.save <- tkbutton(frSave, text = "...")

	tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save, isFile = isFile))

	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	###################
	if(GeneralParameters$data.type == 'cdtstation'){
		txtSaveHelp <- 'Enter the full path of the file to save aggregated data'
	}else{
		txtSaveHelp <- 'Directory to save aggregated data'
	}

	infobulle(en.file.save, txtSaveHelp)
	status.bar.display(en.file.save, TextOutputVar, txtSaveHelp)
	infobulle(bt.file.save, 'or browse here')
	status.bar.display(bt.file.save, TextOutputVar, 'or browse here')

	############################################
	tkgrid(frConvTS, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frDataType, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frameAggr, row = 3, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################

	tkgrid(frAGGRTS, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	####
	tkconfigure(bt.prm.OK, command = function(){
		if(tclvalue(file.stnfl) == ""){
			tkmessageBox(message = "No input file", icon = "warning", type = "ok")
		}else if(tclvalue(file.save)%in%c("", "NA")){
			tkmessageBox(message = "Choose a directory or enter the file to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data' & is.null(settingdone)){
				tkmessageBox(message = "You have to set the NetCDF files parameters", icon = "warning", type = "ok")
				tkwait.window(tt)
		}else{
			GeneralParameters$in.tstep <<- switch(tclvalue(OriginData), 
			 									'Daily data' = 'daily',
			 									'Pentad data' = 'pentad',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')
			GeneralParameters$out.tstep <<- switch(tclvalue(ConvertData),
												'Pentad data' = 'pentad',
												'Dekadal data' = 'dekadal',
												'Monthly data' = 'monthly',
												'Annual data' = 'annual',
												'Seasonal data' = 'seasonal',
												'Rolling Seasonal data' = 'roll.seas')
			GeneralParameters$Seasonal$start.mon <<- as.numeric(str_trim(tclvalue(start.mon)))
			GeneralParameters$Seasonal$length.mon <<- as.numeric(str_trim(tclvalue(length.mon)))
			GeneralParameters$data.type <<- switch(str_trim(tclvalue(DataType)),
													'CDT stations data format' = 'cdtstation',
													'CDT dataset format (gridded)' = 'cdtdataset',
													'NetCDF gridded data' = 'cdtnetcdf')
			if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
				GeneralParameters$cdtstation <<- str_trim(tclvalue(file.stnfl))
			}else if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)'){
				GeneralParameters$cdtdataset <<- str_trim(tclvalue(file.stnfl))
			}else{
				GeneralParameters$cdtnetcdf$dir <<- str_trim(tclvalue(file.stnfl))
			}

			GeneralParameters$output <<- str_trim(tclvalue(file.save))

			GeneralParameters$aggr.series$aggr.fun <<- str_trim(tclvalue(aggr.fun))
			GeneralParameters$aggr.series$min.frac <<- as.numeric(str_trim(tclvalue(min.frac)))
			GeneralParameters$aggr.series$opr.fun <<- str_trim(tclvalue(opr.fun))
			GeneralParameters$aggr.series$opr.thres <<- as.numeric(str_trim(tclvalue(opr.thres)))

			GeneralParameters$settingdone <<- settingdone

			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
		}
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	####
	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################
	
	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###########################
	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt)
	tkwm.title(tt, 'Time Series - Aggregation')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {
		tkgrab.release(tt)
		tkfocus(parent.win)
	})
	tkwait.window(tt)
	return(GeneralParameters)
}

###########################################################################################################

AggregateTS_ncdfData <- function(tt, GeneralParameters, ncDIR, tstep = 'Daily data'){
	listOpenFiles <- openFile_ttkcomboList()

	largeur1 <- if(Sys.info()["sysname"] == "Windows")  32 else 25
	###################

	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt1)

	###################

	frDate <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

	istart.yrs <- tclVar(GeneralParameters$Date.Range$start.year)
	istart.mon <- tclVar(GeneralParameters$Date.Range$start.mon)
	istart.day <- tclVar(GeneralParameters$Date.Range$start.day)
	iend.yrs <- tclVar(GeneralParameters$Date.Range$end.year)
	iend.mon <- tclVar(GeneralParameters$Date.Range$end.mon)
	iend.day <- tclVar(GeneralParameters$Date.Range$end.day)

	txtdek <- switch(tstep, 'Dekadal data' = 'Dek', 'Pentad data' = 'Pen', 'Day')
	day.txtVar <- tclVar(txtdek)
	stateDay <- if(tstep == 'Monthly data') "disabled" else "normal"

	frtxtDate <- ttklabelframe(frDate, text = "Date Range", relief = 'groove')

	txt.deb <- tklabel(frtxtDate, text = 'Start date', anchor = 'e', justify = 'right')
	txt.fin <- tklabel(frtxtDate, text = 'End date', anchor = 'e', justify = 'right')
	txt.yrs <- tklabel(frtxtDate, text = 'Year')
	txt.mon <- tklabel(frtxtDate, text = 'Month')
	txt.day <- tklabel(frtxtDate, text = tclvalue(day.txtVar), textvariable = day.txtVar)
	en.yrs1 <- tkentry(frtxtDate, width = 4, textvariable = istart.yrs, justify = "right")
	en.mon1 <- tkentry(frtxtDate, width = 4, textvariable = istart.mon, justify = "right")
	en.day1 <- tkentry(frtxtDate, width = 4, textvariable = istart.day, justify = "right", state = stateDay)
	en.yrs2 <- tkentry(frtxtDate, width = 4, textvariable = iend.yrs, justify = "right")
	en.mon2 <- tkentry(frtxtDate, width = 4, textvariable = iend.mon, justify = "right")
	en.day2 <- tkentry(frtxtDate, width = 4, textvariable = iend.day, justify = "right", state = stateDay)

	tkgrid(txt.deb, row = 1, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.fin, row = 2, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.yrs, row = 0, column = 1, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.mon, row = 0, column = 2, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.day, row = 0, column = 3, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.yrs1, row = 1, column = 1, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.mon1, row = 1, column = 2, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.day1, row = 1, column = 3, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.yrs2, row = 2, column = 1, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.mon2, row = 2, column = 2, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.day2, row = 2, column = 3, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(frtxtDate, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

	infobulle(frtxtDate, 'Start and end date of the netcdf data to be aggregated')
	status.bar.display(frtxtDate, TextOutputVar, 'Start and end date of the netcdf data to be aggregated')

	###################

	frFF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	inrfeff <- tclVar(GeneralParameters$cdtnetcdf$format)
	rfesample <- tclVar(GeneralParameters$cdtnetcdf$sample)

	txt.ncsample <- tklabel(frFF, text = "Netcdf data sample file", anchor = 'w', justify = 'left')
	cb.ncsample <- ttkcombobox(frFF, values = unlist(listOpenFiles), textvariable = rfesample, width = largeur1)
	bt.ncsample <- tkbutton(frFF, text = "...")
	txt.inrfeff <- tklabel(frFF, text = 'Netcdf data filenames format', anchor = 'w', justify = 'left')
	en.inrfeff <- tkentry(frFF, textvariable = inrfeff, width = largeur1)

	###################

	tkconfigure(bt.ncsample, command = function(){
		initialdir <- if(file.exists(ncDIR)) ncDIR else getwd()
		nc.opfiles <- getOpenNetcdf(main.win, all.opfiles, initialdir = initialdir)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(rfesample) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.ncsample, values = unlist(listOpenFiles), textvariable = rfesample)
		}else return(NULL)
	})

	###################

	tkgrid(txt.ncsample, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.ncsample, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.ncsample, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.inrfeff, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.inrfeff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	ddk <- if(tstep%in%c('Pentad data', 'Dekadal data')) 1 else '01'
	example <- do.call(sprintf, c(list(fmt = GeneralParameters$cdtnetcdf$format),
				as.list(c(1981, '01', ddk)[seq(length(gregexpr('%s', GeneralParameters$cdtnetcdf$format)[[1]]))])))

	status.bar.display(cb.ncsample, TextOutputVar, 'File containing a sample of the data in netcdf')
	infobulle(bt.ncsample, 'Browse file if not listed')
	infobulle(en.inrfeff, paste('Enter the filename format of netcdf data, example:', example))
	status.bar.display(en.inrfeff, TextOutputVar, paste('Enter the filename format of netcdf data, example:', example))

	###################

	tkgrid(frDate, row = 0, column = 0, sticky = '', padx = 1, pady = 1)
	tkgrid(frFF, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)

	################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(rfesample)) == ""){
			tkmessageBox(message = "You have to provide a sample file", icon = "warning", type = "ok")
			tkwait.window(tt1)
		}else{
			GeneralParameters$cdtnetcdf$format <<- str_trim(tclvalue(inrfeff))
			GeneralParameters$cdtnetcdf$sample <<- str_trim(tclvalue(rfesample))

			GeneralParameters$Date.Range$start.year <<- as.numeric(str_trim(tclvalue(istart.yrs)))
			GeneralParameters$Date.Range$start.mon <<- as.numeric(str_trim(tclvalue(istart.mon)))
			GeneralParameters$Date.Range$start.day <<- as.numeric(str_trim(tclvalue(istart.day)))
			GeneralParameters$Date.Range$end.year <<- as.numeric(str_trim(tclvalue(iend.yrs)))
			GeneralParameters$Date.Range$end.mon <<- as.numeric(str_trim(tclvalue(iend.mon)))
			GeneralParameters$Date.Range$end.day <<- as.numeric(str_trim(tclvalue(iend.day)))

			lenS <- length(gregexpr('%s', GeneralParameters$cdtnetcdf$format)[[1]])
			if((tstep%in%c('Daily data', 'Pentad data', 'Dekadal data') & lenS != 3) |
				(tstep == 'Monthly data' & lenS != 2))
			{
				tkmessageBox(message = "Wrong filename format", icon = "warning", type = "ok")
				tkwait.window(tt1)
			}

			tkgrab.release(tt1)
			tkdestroy(tt1)
			tkfocus(tt)
		}
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################
	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'NetCDF Data - Settings')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function(){
		tkgrab.release(tt1)
		tkfocus(tt)
	})
	tkwait.window(tt1)
	return(GeneralParameters)
}

mainDialogAggTs <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	largeur <- if (Sys.info()["sysname"] == "Windows") 34 else 32
	wtkcombo <- 30
	wtkcombo1 <- 15

	###################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	############################################

	frParams <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

	OriginData <- tclVar()
	ConvertData <- tclVar()
	Cbperiod0VAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(OriginData) <- switch(GeneralParameters$period, 
									'daily' = Cbperiod0VAL[1], 
									'dekadal' = Cbperiod0VAL[2],
									'monthly' = Cbperiod0VAL[3])
	Cbperiod1VAL <- c('Dekadal data', 'Monthly data', 'Yearly data', 'Seasonal data', 'Rolling Seasonal data')
	tclvalue(ConvertData) <- switch(GeneralParameters$output.time, 
									'dekadal' = Cbperiod1VAL[1],
									'monthly' = Cbperiod1VAL[2],
									'yearly' = Cbperiod1VAL[3],
									'seasonal' = Cbperiod1VAL[4], 
									'seasonal1' = Cbperiod1VAL[5])

	start.mon <- tclVar(GeneralParameters$Seasonal$start.mon)
	length.mon <- tclVar(GeneralParameters$Seasonal$length.mon)

	DataType <- tclVar()
	CbdatatypeVAL <- c('One station series', 'CDT data format', 'NetCDF gridded data')
	tclvalue(DataType) <- switch(GeneralParameters$data.type, 
								'series' = CbdatatypeVAL[1],
								'cdt' = CbdatatypeVAL[2], 
								'netcdf' = CbdatatypeVAL[3])

	aggfun <- tclVar(GeneralParameters$compute.var$Function)
	MissFrac <- tclVar(GeneralParameters$compute.var$miss.frac)

	####
	Cbperiod0 <- ttkcombobox(frParams, values = Cbperiod0VAL, textvariable = OriginData, width = wtkcombo1)
	Cbperiod1 <- ttkcombobox(frParams, values = Cbperiod1VAL, textvariable = ConvertData, width = wtkcombo1)
	labconv2 <- tklabel(frParams, text=' TO ')
	labSeasS <- tklabel(frParams, text='Start Month', anchor = 'e', justify = 'right')
	enSeasS <- tkentry(frParams, textvariable = start.mon, width = 3, state = 'disabled')
	labSeasL <- tklabel(frParams, text='Width')
	enSeasL <- tkentry(frParams, textvariable = length.mon, width = 3, state = 'disabled')
	labdatype <- ttklabelframe(frParams, text = 'Type of Data', labelanchor = "nw", relief = "groove", borderwidth = 2)
	frcompVar <- tkframe(frParams)

	######
	Cbdatatype <- ttkcombobox(labdatype, values = CbdatatypeVAL, textvariable = DataType)
	bt.opt.set <- tkbutton(labdatype, text = "Settings")

	######

	AggreFunlab <- tklabel(frcompVar, text = 'Aggregation Fun')
	CbAggreFun <- ttkcombobox(frcompVar, values = c("mean", "sum"), textvariable = aggfun, width = 8)
	MissFraclab <- tklabel(frcompVar, text = 'Min.frac')
	MissFracentr <- tkentry(frcompVar, textvariable = MissFrac, width = 6)

	#######

	settingdone <- NULL
	if(tclvalue(DataType) == 'One station series'){
		tkconfigure(bt.opt.set, state = 'normal', command = function(){
			GeneralParameters <<- oneStnDataAgg(main.win, GeneralParameters, tclvalue(OriginData))
			settingdone <<- 1
		})
	}else if(tclvalue(DataType) == 'CDT data format'){
		tkconfigure(bt.opt.set, state = 'disabled')
	}else if(tclvalue(DataType) == 'NetCDF gridded data'){
		tkconfigure(bt.opt.set, state = 'normal', command = function(){
			GeneralParameters <<- netcdfDataAgg(tt, GeneralParameters, tclvalue(OriginData))
			settingdone <<- 1
		})
	}

	#######

	tkgrid(Cbdatatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(bt.opt.set, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	#######
	tkgrid(AggreFunlab, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(CbAggreFun, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(MissFraclab, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(MissFracentr, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

	########

	tkgrid(Cbperiod0, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)
	tkgrid(labconv2, row = 0, column = 8, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(Cbperiod1, row = 0, column = 9, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)
	tkgrid(labSeasS, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
	tkgrid(enSeasS, row = 1, column = 11, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(labSeasL, row = 1, column = 12, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1)
	tkgrid(enSeasL, row = 1, column = 16, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(labdatype, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 17, padx = 1, pady = 1, ipady = 5)
	tkgrid(frcompVar, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 17, padx = 1, pady = 1)

	#########################

	infobulle(Cbperiod0, 'Choose the time step of the data to aggregate')
	status.bar.display(Cbperiod0, TextOutputVar, 'Choose the time step of the data to aggregate')
	infobulle(Cbperiod1, 'Aggregate data to')
	status.bar.display(Cbperiod1, TextOutputVar, 'Aggregate data to')

	infobulle(bt.opt.set, 'Setting data type options')
	status.bar.display(bt.opt.set, TextOutputVar, 'Setting data type options')

	infobulle(CbAggreFun, 'Function to be used to aggregate dekadal, monthly and yearly series')
	status.bar.display(CbAggreFun, TextOutputVar, 'Function to be used to aggregate dekadal, monthly and yearly series')
	infobulle(MissFracentr, 'Minimum fraction of available data\nthat must be present for the time period\nto compute')
	status.bar.display(MissFracentr, TextOutputVar, 'Minimum fraction of available data that must be present for the time period to compute')

	#########################

	tkbind(Cbperiod0, "<<ComboboxSelected>>", function(){
		if(tclvalue(OriginData) == 'Daily data'){
			tkconfigure(Cbperiod1, values = Cbperiod1VAL)
			if(tclvalue(ConvertData) == 'Daily data') tclvalue(ConvertData) <- 'Dekadal data'
		}else if(tclvalue(OriginData) == 'Dekadal data'){
			tkconfigure(Cbperiod1, values = Cbperiod1VAL[-1])
			if(tclvalue(ConvertData)%in%c('Daily data', 'Dekadal data')) tclvalue(ConvertData) <- 'Monthly data'
		}else if(tclvalue(OriginData) == 'Monthly data'){
			tkconfigure(Cbperiod1, values = Cbperiod1VAL[-(1:2)])
			if(tclvalue(ConvertData)%in%c('Daily data', 'Dekadal data', 'Monthly data')) tclvalue(ConvertData) <- 'Yearly data'
		}
		if(tclvalue(ConvertData) == 'Seasonal data'){
			tkconfigure(enSeasS, state = 'normal')
			tkconfigure(enSeasL, state = 'normal')
		}else if(tclvalue(ConvertData) == 'Rolling Seasonal data'){
			tkconfigure(enSeasS, state = 'disabled')
			tkconfigure(enSeasL, state = 'normal')
		}else{
			tkconfigure(enSeasS, state = 'disabled')
			tkconfigure(enSeasL, state = 'disabled')
		}
	})

	tkbind(Cbperiod1, "<<ComboboxSelected>>", function(){
		if(tclvalue(ConvertData) == 'Seasonal data'){
			tkconfigure(enSeasS, state = 'normal')
			tkconfigure(enSeasL, state = 'normal')
		}else if(tclvalue(ConvertData) == 'Rolling Seasonal data'){
			tkconfigure(enSeasS, state = 'disabled')
			tkconfigure(enSeasL, state = 'normal')
		}else{
			tkconfigure(enSeasS, state = 'disabled')
			tkconfigure(enSeasL, state = 'disabled')
		}
	})

	############################################

	frIO <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

	file.stnfl <- tclVar(GeneralParameters$IO.files$In.dir.file)
	file.save1 <- tclVar(GeneralParameters$IO.files$Out.dir.file)
	fileINdir <- tclVar('Input data')
	fileORdir <- tclVar('File to save computed data')

	inDirFlile <- tklabel(frIO, text = tclvalue(fileINdir), textvariable = fileINdir, anchor = 'w', justify = 'left')
	cb.stnfl <- ttkcombobox(frIO, values = unlist(listOpenFiles), textvariable = file.stnfl, width = wtkcombo)
	bt.stnfl <- tkbutton(frIO, text = "...")
	outDirFlile <- tklabel(frIO, text = tclvalue(fileORdir), textvariable = fileORdir, anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frIO, textvariable = file.save1, width = largeur)
	bt.file.save <- tkbutton(frIO, text = "...")

	#########################

	tkconfigure(bt.stnfl, command = function(){
		dat.opfiles <- getOpenFiles(tt, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.stnfl) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
		}else{
			return(NULL)
		}
	})

	tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save1, isFile = TRUE))

	#########################

	tkgrid(inDirFlile, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(outDirFlile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 3, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.stnfl, 'Choose the file in the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the data to aggregate')
	infobulle(bt.stnfl, 'Browse file if not listed')
	status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')

	infobulle(en.file.save, 'Enter the full path of the file to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path of the file to save aggregated data')
	infobulle(bt.file.save, 'or browse here')
	status.bar.display(bt.file.save, TextOutputVar, 'or browse here')

	############################################

	tkbind(Cbdatatype,"<<ComboboxSelected>>", function(){
		tkdestroy(cb.stnfl)
		tclvalue(file.stnfl) <- ''

		if(tclvalue(DataType) == 'One station series'){
			tclvalue(fileINdir) <- 'Input data'
			tclvalue(fileORdir) <- 'File to save aggregated data'

			cb.stnfl <- ttkcombobox(frIO, values = unlist(listOpenFiles), textvariable = file.stnfl, width = wtkcombo)

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
				}else{
					return(NULL)
				}
			})

			tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save1, isFile = TRUE))
			tkconfigure(bt.opt.set, state = 'normal', command = function(){
				GeneralParameters <<- oneStnDataAgg(main.win, GeneralParameters, tclvalue(OriginData))
				settingdone <<- 1
			})

			#######
			infobulle(cb.stnfl, 'Choose the file in the list')
			status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the data to aggregate')
			infobulle(en.file.save, 'Enter the full path of the file to save result')
			status.bar.display(en.file.save, TextOutputVar, 'Enter the full path of the file to save aggregated data')
		}else if(tclvalue(DataType) == 'CDT data format'){
			tclvalue(fileINdir) <- 'Input data'
			tclvalue(fileORdir) <- 'File to save aggregated data'

			cb.stnfl <- ttkcombobox(frIO, values = unlist(listOpenFiles), textvariable = file.stnfl, width = wtkcombo)

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
				}else{
					return(NULL)
				}
			})

			tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save1, isFile = TRUE))
			tkconfigure(bt.opt.set, state = 'disabled')

			#######
			infobulle(cb.stnfl, 'Choose the file in the list')
			status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the data to aggregate')
			infobulle(en.file.save, 'Enter the full path of the file to save result')
			status.bar.display(en.file.save, TextOutputVar, 'Enter the full path of the file to save aggregated data')
		}else if(tclvalue(DataType) == 'NetCDF gridded data'){
			tclvalue(fileINdir) <- 'Directory containing the NetCDF data'
			tclvalue(fileORdir) <- 'Directory to save result'

			cb.stnfl <- tkentry(frIO, textvariable = file.stnfl, width = largeur)

			#######
			tkconfigure(bt.stnfl, command = function(){
				file2convert <- tk_choose.dir(getwd(), "")
				tclvalue(file.stnfl) <- if(!is.na(file2convert)) file2convert else ""
			})

			tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save1, isFile = FALSE))
			tkconfigure(bt.opt.set, state = 'normal', command = function(){
				GeneralParameters <<- netcdfDataAgg(tt, GeneralParameters, tclvalue(OriginData))
				settingdone <<- 1
			})

			#######
			infobulle(cb.stnfl, 'Enter the full path to directory containing the NetCDF data')
			status.bar.display(cb.stnfl, TextOutputVar, 'Enter the full path to directory containing the NetCDF data')
			infobulle(en.file.save, 'Enter the full path to directory to save result')
			status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save result')
		}
		
		#######
		tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	})

	############################################

	tkgrid(frParams, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frIO, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	####
	tkconfigure(bt.prm.OK, command = function(){
		if(tclvalue(file.stnfl) == ""){
			tkmessageBox(message = "No input file", icon = "warning", type = "ok")
		}else if(tclvalue(file.save1) == "" | tclvalue(file.save1) == "NA"){
			tkmessageBox(message = "Choose a directory or enter the file to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(DataType) != 'CDT data format' & is.null(settingdone)){
				tkmessageBox(message = "You have to set the options", icon = "warning", type = "ok")
				tkwait.window(tt)
		}else{
			GeneralParameters$period <<- switch(tclvalue(OriginData), 
			 									'Daily data' = 'daily',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')
			GeneralParameters$output.time <<- switch(tclvalue(ConvertData),
												'Dekadal data' = 'dekadal',
												'Monthly data' = 'monthly',
												'Yearly data' = 'yearly',
												'Seasonal data' = 'seasonal',
												'Rolling Seasonal data' = 'seasonal1')
			GeneralParameters$Seasonal$start.mon <<- as.numeric(str_trim(tclvalue(start.mon)))
			GeneralParameters$Seasonal$length.mon <<- as.numeric(str_trim(tclvalue(length.mon)))
			GeneralParameters$data.type <<- switch(tclvalue(DataType),
													'One station series' = 'series',
													'CDT data format' = 'cdt',
													'NetCDF gridded data' = 'netcdf')
			GeneralParameters$compute.var$Function <<- str_trim(tclvalue(aggfun))
			GeneralParameters$compute.var$miss.frac <<- as.numeric(str_trim(tclvalue(MissFrac)))
			GeneralParameters$IO.files$In.dir.file <<- str_trim(tclvalue(file.stnfl))
			GeneralParameters$IO.files$Out.dir.file <<- str_trim(tclvalue(file.save1))
			
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
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
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

netcdfDataAgg <- function(tt, GeneralParameters, daydek){
	largeur1 <- if(Sys.info()["sysname"] == "Windows")  27 else 25
	###################

	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt1)

	###################

	frDate <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	day.val0 <- as.numeric(GeneralParameters$Date.Range$start.day)
	day.val1 <- as.numeric(GeneralParameters$Date.Range$end.day)
	if(daydek == 'Dekadal data'){
		day.val0 <- if(day.val0 > 3) 3 else day.val0
		day.val1 <- if(day.val1 > 3) 3 else day.val1
	}
	istart.yrs <- tclVar(GeneralParameters$Date.Range$start.year)
	istart.mon <- tclVar(GeneralParameters$Date.Range$start.mon)
	istart.day <- tclVar(day.val0)
	iend.yrs <- tclVar(GeneralParameters$Date.Range$end.year)
	iend.mon <- tclVar(GeneralParameters$Date.Range$end.mon)
	iend.day <- tclVar(day.val1)

	stdaydek <- if(daydek == 'Monthly data') 'disabled' else 'normal'

	deb.txt <- tklabel(frDate, text = 'Start date', anchor = 'e', justify = 'right')
	fin.txt <- tklabel(frDate, text = 'End date', anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(frDate, text = 'Year')
	mon.txt <- tklabel(frDate, text = 'Month')
	if(daydek == 'Daily data') day.txt <- tklabel(frDate, text = 'Day')
	if(daydek == 'Dekadal data') day.txt <- tklabel(frDate, text = 'Dek')
	if(daydek == 'Monthly data') day.txt <- tklabel(frDate, text = 'Day')

	yrs1.v <- tkentry(frDate, width = 4, textvariable = istart.yrs, justify = "right")
	mon1.v <- tkentry(frDate, width = 4, textvariable = istart.mon, justify = "right")
	day1.v <- tkentry(frDate, width = 4, textvariable = istart.day, justify = "right", state = stdaydek)
	yrs2.v <- tkentry(frDate, width = 4, textvariable = iend.yrs, justify = "right")
	mon2.v <- tkentry(frDate, width = 4, textvariable = iend.mon, justify = "right")
	day2.v <- tkentry(frDate, width = 4, textvariable = iend.day, justify = "right", state = stdaydek)

	tkgrid(deb.txt, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(fin.txt, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs.txt, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon.txt, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day.txt, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs1.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon1.v, row = 1, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day1.v, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs2.v, row = 2, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon2.v, row = 2, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day2.v, row = 2, column = 3, sticky = 'ew', padx = 1, pady = 1)

	infobulle(frDate, 'Start and end date of data to aggregate')
	status.bar.display(frDate, TextOutputVar, 'Start and end date of data to aggregate')

	###################

	frFF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	inrfeff <- tclVar(GeneralParameters$ncdf.file.format$input)
	outmrgff <- tclVar(GeneralParameters$ncdf.file.format$output)

	txt.inrfeff <- tklabel(frFF, text = 'Input NetCDF filenames format', anchor = 'w', justify = 'left')
	en.inrfeff <- tkentry(frFF, textvariable = inrfeff, width = largeur1)
	txt.outmrgff <- tklabel(frFF, text = 'Output NetCDF filenames format', anchor = 'w', justify = 'left')
	en.outmrgff <- tkentry(frFF, textvariable = outmrgff, width = largeur1)

	tkgrid(txt.inrfeff, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(en.inrfeff, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(txt.outmrgff, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(en.outmrgff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

	infobulle(en.inrfeff, 'Enter the filename format of the input NetCDF,\nexample: rfe_19830125.nc')
	status.bar.display(en.inrfeff, TextOutputVar, 'Enter the filename format of the input NetCDF, example: rfe_1983013.nc')
	infobulle(en.outmrgff, 'Enter the filename format  of the output aggregated data')
	status.bar.display(en.outmrgff, TextOutputVar, 'Enter the filename format of the output aggregated data')

	###################

	tkgrid(frDate, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 5, ipady = 6)
	tkgrid(frFF, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)

	###################

	################################
	bt.prm.OK <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		GeneralParameters$Date.Range$start.year <<- as.numeric(str_trim(tclvalue(istart.yrs)))
		GeneralParameters$Date.Range$start.mon <<- as.numeric(str_trim(tclvalue(istart.mon)))
		GeneralParameters$Date.Range$start.day <<- as.numeric(str_trim(tclvalue(istart.day)))
		GeneralParameters$Date.Range$end.year <<- as.numeric(str_trim(tclvalue(iend.yrs)))
		GeneralParameters$Date.Range$end.mon <<- as.numeric(str_trim(tclvalue(iend.mon)))
		GeneralParameters$Date.Range$end.day <<- as.numeric(str_trim(tclvalue(iend.day)))

		GeneralParameters$ncdf.file.format$input <<- str_trim(tclvalue(inrfeff))
		GeneralParameters$ncdf.file.format$output <<- str_trim(tclvalue(outmrgff))

		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})


	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 5, ipady = 1)
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
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'NetCDF Data Settings')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(GeneralParameters)
}

###########################################################################################################

oneStnDataAgg <- function(top.win, GeneralParameters, speriod){
	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt1)

	###################

	rbffrmt <- tclVar(GeneralParameters$One.series$file.format)
 
	fr.fileformat1 <- ttklabelframe(frMRG0, text = "File Format", labelanchor = "nw", relief = "groove", borderwidth = 2)
	ffrmt1 <- tkradiobutton(fr.fileformat1, text = "One variable", anchor = 'w', justify = 'left')
	ffrmt2 <- tkradiobutton(fr.fileformat1, text = "Rain Tmax Tmin", anchor = 'w', justify = 'left')

	tkconfigure(ffrmt1, variable = rbffrmt, value = "1")
	tkconfigure(ffrmt2, variable = rbffrmt, value = "0")

	tkgrid(ffrmt1, row = 0, column = 0, sticky = "we")
	tkgrid(ffrmt2, row = 1, column = 0, sticky = "we")

	infobulle(ffrmt1, 'In case of single serie: The file contains 1 variable')
	status.bar.display(ffrmt1, TextOutputVar, 'In case of single serie: The file contains 1 variable')
	infobulle(ffrmt2, 'In case of single serie: The file contains\nRain, Tmax and Tmin in this order')
	status.bar.display(ffrmt2, TextOutputVar, 'In case of single serie:The file contains Rain, Tmax and Tmin in this order')

	#####
	tkgrid(fr.fileformat1, row = 0, column = 0, sticky = 'nswe', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###################

	rbdtfrmt <- tclVar(GeneralParameters$One.series$date.format)
	if(speriod == 'Daily data') {txtdtfrmt1 <- "YYYYMMDD"; txtdtfrmt2 <- "YYYY MM DD"}
	if(speriod == 'Dekadal data') {txtdtfrmt1 <- "YYYYMMD"; txtdtfrmt2 <- "YYYY MM D"}
	if(speriod == 'Monthly data') {txtdtfrmt1 <- "YYYYMM"; txtdtfrmt2 <- "YYYY MM"}

	fr.fileformat2 <- ttklabelframe(frMRG0, text = "Dates Format", labelanchor = "nw", relief = "groove", borderwidth = 2)
	dtfrmt1 <- tkradiobutton(fr.fileformat2, text = txtdtfrmt1, anchor = 'w', justify = 'left')
	dtfrmt2 <- tkradiobutton(fr.fileformat2, text = txtdtfrmt2, anchor = 'w', justify = 'left')
	dtfrmt3 <- tklabel(fr.fileformat2, text = '')

	tkconfigure(dtfrmt1, variable = rbdtfrmt, value = "1")
	tkconfigure(dtfrmt2, variable = rbdtfrmt, value = "0")

	tkgrid(dtfrmt1, row = 0, column = 0, sticky = "we")
	tkgrid(dtfrmt2, row = 1, column = 0, sticky = "we")
	tkgrid(dtfrmt3, row = 3, column = 0, sticky = "we")

	infobulle(dtfrmt1, 'In case of single serie:\n dates are merged')
	status.bar.display(dtfrmt1, TextOutputVar, 'In case of single serie: dates are merged')
	infobulle(dtfrmt2, 'In case of single serie:\ndates are separated by space,\ntabulation or CSV format')
	status.bar.display(dtfrmt2, TextOutputVar, 'In case of single serie: dates are separated by space, tabulation or CSV format')

	#####

	tkgrid(fr.fileformat2, row = 0, column = 1, sticky = 'nswe', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	################################

	bt.fileformat <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.fileformat, command = function(){
		GeneralParameters$One.series$file.format <<- tclvalue(rbffrmt)
		GeneralParameters$One.series$date.format <<- tclvalue(rbdtfrmt)

		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkgrid(bt.fileformat, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 5, ipady = 1)
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
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, '')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(top.win)})
	tkwait.window(tt1)
	return(GeneralParameters)
}


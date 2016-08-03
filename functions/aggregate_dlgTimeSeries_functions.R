mainDialogAggTs <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	###tkentry width
	if (Sys.info()["sysname"] == "Windows") largeur <- 34
	else largeur <- 32
	##tkcombo width, list.file
	wtkcombo <- 30
	##tkcombo width, freq, data source
	wtkcombo1 <- 15

	###################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frButt <- tkframe(tt)

	##############################
	fr.A <- tkframe(frDialog, relief = 'sunken', borderwidth = 2)
	fr.B <- tkframe(frDialog, relief = 'sunken', borderwidth = 2)

	tkgrid(fr.A, sticky = 'w', padx = 5, pady = 5)
	tkgrid(fr.B, sticky = 'w', padx = 5, pady = 5)

	############################
	for(i in 0:2) assign(paste('fr.A', i, sep = ''), tkframe(fr.A))
	for(i in 0:2) tkgrid(get(paste('fr.A', i, sep = '')))
	for(i in 0:2) tkgrid.configure(get(paste('fr.A', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 1)

	OriginData <- tclVar()
	tclvalue(OriginData) <- ifelse(as.character(GeneralParameters$period) == 'daily',
	'Daily data', ifelse(as.character(GeneralParameters$period) == 'dekadal', 'Dekadal data', 'Monthly data'))

	Cbperiod0 <- ttkcombobox(fr.A0, values = c('Daily data', 'Dekadal data', 'Monthly data'), textvariable = OriginData, width = wtkcombo1)
	infobulle(Cbperiod0, 'Choose the time step of the data to convert')
	status.bar.display(Cbperiod0, TextOutputVar, 'Choose the time step of the data to convert')

	ConvertData <- tclVar()
	tclvalue(ConvertData) <- ifelse(as.character(GeneralParameters$period1) == 'dekadal',
	'Dekadal data', ifelse(as.character(GeneralParameters$period1) == 'monthly', 'Monthly data', 'Yearly data'))
	Cbperiod1 <- ttkcombobox(fr.A0, values = c('Dekadal data', 'Monthly data', 'Yearly data'), textvariable = ConvertData, width = wtkcombo1)
	infobulle(Cbperiod1, 'Aggregate data to')
	status.bar.display(Cbperiod1, TextOutputVar, 'Aggregate data to')

	tkbind(Cbperiod0,"<<ComboboxSelected>>", function(){
		if(tclvalue(OriginData) == 'Daily data'){
			tkconfigure(Cbperiod1, values = c('Dekadal data', 'Monthly data', 'Yearly data'))
			tclvalue(ConvertData) <-'Dekadal data'
		}else if(tclvalue(OriginData) == 'Dekadal data'){
			tkconfigure(Cbperiod1, values = c('Monthly data', 'Yearly data'))
			tclvalue(ConvertData) <-'Monthly data'
		}else if(tclvalue(OriginData) == 'Monthly data'){
			tkconfigure(Cbperiod1, values = c('Yearly data',''))
			tclvalue(ConvertData) <-'Yearly data'
		}
	})

	labconv2 <- tklabel(fr.A0, text=' TO ')
	tkgrid(Cbperiod0, labconv2, Cbperiod1)

	##########

	labdatype <- ttklabelframe(fr.A1, text = 'Type of Data', labelanchor = "nw", relief = "groove", borderwidth = 2)
	tkgrid(labdatype, sticky = 'we', ipadx = 5, ipady = 5)

	datatyp <- as.character(GeneralParameters$data.type)
	DataType <- tclVar()
	tclvalue(DataType) <- ifelse(datatyp == 'series', 'One station series', ifelse(datatyp == 'cdt', 'CDT data format', 'NetCDF gridded data'))
	Cbdatatype <- ttkcombobox(labdatype, values = c('One station series', 'CDT data format', 'NetCDF gridded data'), textvariable = DataType)
	bt.opt.set <- tkbutton.h(labdatype, text = "Settings", TextOutputVar, 'Options setting for data', 'Options setting for data')
	tkgrid(Cbdatatype, bt.opt.set)

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
			GeneralParameters <<- netcdfDataAgg(tt, GeneralParameters, tclvalue(OriginData), tclvalue(ConvertData))
			settingdone <<- 1
		})
	}

	##################

	AggreFunlab <- tklabel.h(fr.A2, text = 'Aggregation Fun', TextOutputVar,
	'Function to be used to compute\ndekadal, monthly and yearly series',
	'Function to be used to compute dekadal, monthly and yearly series')

	Aggregatefun <- c("mean", "sum")
	aggfun <- tclVar(as.character(GeneralParameters$compute.var$Values[1]))
	CbAggreFun <- ttkcombobox(fr.A2, values = Aggregatefun, textvariable = aggfun, width = 8)
	infobulle(CbAggreFun, 'Function to be used to compute\ndekadal, monthly and yearly series')
	status.bar.display(CbAggreFun, TextOutputVar, 'Function to be used to convert dekadal, monthly and yearly series')

	MissFraclab <- tklabel.h(fr.A2, text = 'Min.frac', TextOutputVar,
	'Minimum fraction of available data\nthat must be present for the time period\nto compute',
	'Minimum fraction of available data that must be present for the time period to compute')

	MissFracentr <- tkentry.h(fr.A2, TextOutputVar,
	'Minimum fraction of available data\nthat must be present for the time period\nto compute',
	'Minimum fraction of available data that must be present for the time period to compute')

	MissFrac <- tclVar(as.character(GeneralParameters$compute.var$Values[2]))
	tkconfigure(MissFracentr, width = 6, textvariable = MissFrac)

	tkgrid(AggreFunlab, CbAggreFun, MissFraclab, MissFracentr)

	#############################
	for(i in 0:3) assign(paste('fr.B', i, sep = ''), tkframe(fr.B))
	for(i in 0:3) tkgrid(get(paste('fr.B', i, sep = '')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.B', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	file.stnfl <- tclVar()
	tclvalue(file.stnfl) <- as.character(GeneralParameters$file.io$Values[1])

	fileINdir <- tclVar('Input data')

	frB0.txt <- tklabel(fr.B0, text = tclvalue(fileINdir), textvariable = fileINdir)
	tkgrid(frB0.txt)

	cb.stnfl <- ttkcombobox(fr.B1, values = unlist(listOpenFiles), textvariable = file.stnfl, width = wtkcombo)
	infobulle(cb.stnfl, 'Choose the file in the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the data to convert')

	bt.stnfl <- tkbutton.h(fr.B1, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
	tkgrid(cb.stnfl, row = 0, column = 0, sticky = 'w')
	tkgrid(bt.stnfl, row = 0, column = 1, sticky = 'e')
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

	################################################

	fileORdir <- tclVar('File to save computed data')

	frB2.txt <- tklabel(fr.B2, text = tclvalue(fileORdir), textvariable = fileORdir)
	tkgrid(frB2.txt)

	file.save1 <- tclVar(as.character(GeneralParameters$file.io$Values[2]))
	en.file.save <- tkentry(fr.B3, textvariable = file.save1, width = largeur)
	infobulle(en.file.save, 'Enter the full path of the file to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path of the file to save converted data')
	bt.file.save <- tkbutton.h(fr.B3, text = "...", TextOutputVar, 'or browse here', 'or browse here')
	tkgrid(en.file.save, row = 0, column = 0, sticky = 'w')
	tkgrid(bt.file.save, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.file.save, command = function(){
		filetypes  <-  "{{Text Files} {.txt .TXT}} {{CSV Files} {.csv .CSV}} {{All files} *}"
		if (Sys.info()["sysname"] == "Windows") file2save1 <- tkgetSaveFile(initialdir = getwd(), initialfile = "", filetypes = filetypes, defaultextension = TRUE)
		else file2save1 <- tkgetSaveFile(initialdir = getwd(), initialfile = "", filetypes = filetypes)
		if(is.na(file2save1)) tclvalue(file.save1) <- ""
		else tclvalue(file.save1) <- file2save1
	})
	#####################################################
	tkbind(Cbdatatype,"<<ComboboxSelected>>", function(){
		if(tclvalue(DataType) == 'One station series'){
			tkdestroy(cb.stnfl)
			cb.stnfl <- ttkcombobox(fr.B1, values = unlist(listOpenFiles), textvariable = file.stnfl, width = wtkcombo)
			tkgrid(cb.stnfl, row = 0, column = 0, sticky = 'w')
			infobulle(cb.stnfl, 'Choose the file in the list')
			status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the data to convert')

			tclvalue(fileINdir) <- 'Input data'
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

			infobulle(en.file.save, 'Enter the full path of the file to save result')
			status.bar.display(en.file.save, TextOutputVar, 'Enter the full path of the file to save converted data')
			tclvalue(fileORdir) <- 'File to save converted data'
			tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save1, isFile = TRUE))

			tkconfigure(bt.opt.set, state = 'normal', command = function(){
				GeneralParameters <<- oneStnDataAgg(main.win, GeneralParameters, tclvalue(OriginData))
				settingdone <<- 1
			})
		}else if(tclvalue(DataType) == 'CDT data format'){
			tkdestroy(cb.stnfl)
			cb.stnfl <- ttkcombobox(fr.B1, values = unlist(listOpenFiles), textvariable = file.stnfl, width = wtkcombo)
			tkgrid(cb.stnfl, row = 0, column = 0, sticky = 'w')
			infobulle(cb.stnfl, 'Choose the file in the list')
			status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the data to convert')

			tclvalue(fileINdir) <- 'Input data'
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

			infobulle(en.file.save, 'Enter the full path of the file to save result')
			status.bar.display(en.file.save, TextOutputVar, 'Enter the full path of the file to save converted data')
			tclvalue(fileORdir) <- 'File to save converted data'
			tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save1, isFile = TRUE))

			tkconfigure(bt.opt.set, state = 'disabled')
		}else if(tclvalue(DataType) == 'NetCDF gridded data'){
			tkdestroy(cb.stnfl)
			cb.stnfl <- tkentry(fr.B1, textvariable = file.stnfl, width = largeur)
			tkgrid(cb.stnfl, row = 0, column = 0, sticky = 'w')
			infobulle(cb.stnfl, 'Enter the full path to directory containing the NetCDF data')
			status.bar.display(cb.stnfl, TextOutputVar, 'Enter the full path to directory containing the NetCDF data')

			tclvalue(fileINdir) <- 'Directory containing the NetCDF data'
			tkconfigure(bt.stnfl, command = function(){
				file2convert <- tk_choose.dir(getwd(), "")
				if(is.na(file2convert)) tclvalue(file.stnfl)<-""
				else tclvalue(file.stnfl) <- file2convert
			})

			infobulle(en.file.save, 'Enter the full path to\ndirectory to save result')
			status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save result')
			tclvalue(fileORdir) <- 'Directory to save result'
			tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save1, isFile = FALSE))

			tkconfigure(bt.opt.set, state = 'normal', command = function(){
				GeneralParameters <<- netcdfDataAgg(tt, GeneralParameters, tclvalue(OriginData), tclvalue(ConvertData))
				settingdone <<- 1
			})
		}
	})

	#####################################################
	bt.prm.OK <- tkbutton(frButt, text=" OK ")
	bt.prm.CA <- tkbutton(frButt, text = "Cancel")
	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 5, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

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
			if(tclvalue(DataType) == 'NetCDF gridded data'){
				informat <- as.character(GeneralParameters$IO.file.format$Values[1])
				istart.yrs <- as.numeric(as.character(GeneralParameters$datesSE$Values[1]))
				istart.mon <- as.numeric(as.character(GeneralParameters$datesSE$Values[2]))
				istart.day <- as.numeric(as.character(GeneralParameters$datesSE$Values[3]))
				daty <- format(as.Date(paste(istart.yrs, istart.mon, istart.day, sep = '-')),'%Y%m%d')
				yrs <- substr(daty, 1,4)
				mon <- substr(daty, 5,6)
				day <- substr(daty, 7,8)
				if(tclvalue(OriginData) == 'Monthly data'){
					filein <- sprintf(informat, yrs, mon)
				}else if(tclvalue(OriginData) == 'Dekadal data'){
					filein <- sprintf(informat, yrs, mon, as.numeric(day))
				}else if(tclvalue(OriginData) == 'Daily data'){
					filein <- sprintf(informat, yrs, mon, day)
				}
				ncfiles <- file.path(tclvalue(file.stnfl), filein, fsep = .Platform$file.sep)
				if(!file.exists(ncfiles)){
					tkmessageBox(message = "The NetCDF's:\n -file format\n -or directory\n -or date range\nare wrong", icon = "warning", type = "ok")
					tkwait.window(tt)
				}
			}
			GeneralParameters$period <<- ifelse(tclvalue(OriginData) == 'Daily data', 'daily',
			ifelse(tclvalue(OriginData) == 'Dekadal data', 'dekadal', 'monthly'))
			GeneralParameters$period1 <<- ifelse(tclvalue(ConvertData) == 'Dekadal data', 'dekadal',
			ifelse(tclvalue(ConvertData) == 'Monthly data', 'monthly', 'yearly'))

			GeneralParameters$data.type <<- ifelse(tclvalue(DataType) == 'One station series', 'series',
			ifelse(tclvalue(DataType) == 'CDT data format', 'cdt', 'netcdf'))
			GeneralParameters$compute.var$Values <<- c(tclvalue(aggfun), tclvalue(MissFrac))
			GeneralParameters$file.io$Values <<- c(tclvalue(file.stnfl), tclvalue(file.save1))

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

	######################
	tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frButt, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###########################
	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+',tt.x,'+',tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Time Series Data Aggregation')
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

netcdfDataAgg <- function(tt, GeneralParameters, daydek, daydek1){
	if (Sys.info()["sysname"] == "Windows") largeur1 <- 27
	else largeur1 <- 25
	if (Sys.info()["sysname"] == "Windows") largeur2 <- 42
	else largeur2 <- 40
	###################

	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt1)

	fr.A <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)
	fr.B <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)
	fr.C <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	tkgrid(fr.A, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 5, ipady = 6)
	tkgrid(fr.B, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(fr.C, row = 1, column = 0, columnspan = 2, sticky = 'ew', padx = 1, pady = 1)

	###################

	infobulle(fr.A, 'Start and end date of data to convert')
	status.bar.display(fr.A, TextOutputVar, 'Start and end date of data to convert')

	deb.txt <- tklabel(fr.A, text = 'Start date', anchor = 'e', justify = 'right')
	fin.txt <- tklabel(fr.A, text = 'End date', anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(fr.A, text = 'Year')
	mon.txt <- tklabel(fr.A, text = 'Month')
	if(daydek == 'Daily data'){
		day.txt <- tklabel(fr.A, text = 'Day')
		day.val0 <- tclVar(as.character(GeneralParameters$datesSE$Values[3]))
		day.val1 <- tclVar(as.character(GeneralParameters$datesSE$Values[6]))
		stdaydek <- 'normal'
	}else if(daydek == 'Dekadal data'){
		day.txt <- tklabel(fr.A, text = 'Dek')
		day.vl0 <- as.numeric(as.character(GeneralParameters$datesSE$Values[3]))
		day.val0 <- if(day.vl0 > 3) tclVar('3') else tclVar(day.vl0)
		day.vl1 <- as.numeric(as.character(GeneralParameters$datesSE$Values[6]))
		day.val1 <- if(day.vl1 > 3) tclVar('3') else tclVar(day.vl1)
		stdaydek <- 'normal'
	}else if(daydek == 'Monthly data'){
		day.txt <- tklabel(fr.A, text = 'Day')
		day.val0 <- tclVar(as.character(GeneralParameters$datesSE$Values[3]))
		day.val1 <- tclVar(as.character(GeneralParameters$datesSE$Values[6]))
		stdaydek <- 'disabled'
	}

	istart.yrs <- tclVar(as.character(GeneralParameters$datesSE$Values[1]))
	istart.mon <- tclVar(as.character(GeneralParameters$datesSE$Values[2]))
	istart.day <- day.val0
	iend.yrs <- tclVar(as.character(GeneralParameters$datesSE$Values[4]))
	iend.mon <- tclVar(as.character(GeneralParameters$datesSE$Values[5]))
	iend.day <- day.val1

	yrs1.v <- tkentry(fr.A, width = 4, textvariable = istart.yrs, justify = "right")
	mon1.v <- tkentry(fr.A, width = 4, textvariable = istart.mon, justify = "right")
	day1.v <- tkentry(fr.A, width = 4, textvariable = istart.day, justify = "right", state = stdaydek)
	yrs2.v <- tkentry(fr.A, width = 4, textvariable = iend.yrs, justify = "right")
	mon2.v <- tkentry(fr.A, width = 4, textvariable = iend.mon, justify = "right")
	day2.v <- tkentry(fr.A, width = 4, textvariable = iend.day, justify = "right", state = stdaydek)

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

	##############################
	for(i in 0:3) assign(paste('fr.B', i, sep = ''), tkframe(fr.B))
	for(i in 0:3) tkgrid(get(paste('fr.B', i, sep = '')))
	for(i in 0:3) tkgrid.configure(get(paste('fr.B', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 0, ipadx = 1, ipady = 0)

	frB0.txt <- tklabel(fr.B0, text = 'Input NetCDF files names format')
	tkgrid(frB0.txt)

	inrfeff <- tclVar(as.character(GeneralParameters$IO.file.format$Values[1]))
	en.inrfeff <- tkentry(fr.B1, textvariable = inrfeff, width = largeur1)
	infobulle(en.inrfeff, 'Enter the format of the input NetCDF files names,\nexample: daily_19830101.nc')
	status.bar.display(en.inrfeff, TextOutputVar, 'Enter the format of the input NetCDF files names, example: daily_19830101.nc')
	tkgrid(en.inrfeff, row = 0, column = 0, sticky = 'w')

	frB2.txt <- tklabel(fr.B2, text = 'Output NetCDF files names format')
	tkgrid(frB2.txt)

	outmrgff <- tclVar(as.character(GeneralParameters$IO.file.format$Values[2]))
	en.outmrgff <- tkentry(fr.B3, textvariable = outmrgff, width = largeur1)
	infobulle(en.outmrgff, 'Enter the format of the output NetCDF files names,\nexample: dek_19830101.nc')
	status.bar.display(en.outmrgff, TextOutputVar, 'Enter the format of the output NetCDF files names, example: dek_19830101.nc')
	tkgrid(en.outmrgff, row = 0, column = 0, sticky = 'w')

		if(daydek == 'Daily data'){
			tclvalue(inrfeff) <- "daily_%s%s%s.nc"
			if(daydek1 == 'Dekadal data'){
				tclvalue(outmrgff) <- "dek_%s%s%s.nc"
			}else if(daydek1 == 'Monthly data'){
				tclvalue(outmrgff) <- "mon_%s%s.nc"
			}else if(daydek1 == 'Yearly data'){
				tclvalue(outmrgff) <- "year_%s.nc"
			}
			infobulle(en.inrfeff, 'Enter the format of the input NetCDF files names,\nexample: daily_19830101.nc')
			status.bar.display(en.inrfeff, TextOutputVar, 'Enter the format of the input NetCDF files names, example: daily_19830101.nc')
			infobulle(en.outmrgff, 'Enter the format of the output NetCDF files names,\nexample: dek_1983011.nc')
			status.bar.display(en.outmrgff, TextOutputVar, 'Enter the format of the output NetCDF files names, example: dek_1983011.nc')
		}else if(daydek == 'Dekadal data'){
			tclvalue(inrfeff) <- "dek_%s%s%s.nc"
			if(daydek1 == 'Monthly data'){
				tclvalue(outmrgff) <- "mon_%s%s.nc"
			}else if(daydek1 == 'Yearly data'){
				tclvalue(outmrgff) <- "year_%s.nc"
			}
			infobulle(en.inrfeff, 'Enter the format of the input NetCDF files names,\nexample: dek_1983011.nc')
			status.bar.display(en.inrfeff, TextOutputVar, 'Enter the format of the input NetCDF files names, example: dek_1983011.nc')
			infobulle(en.outmrgff, 'Enter the format of the output NetCDF files names,\nexample: mon_198301.nc')
			status.bar.display(en.outmrgff, TextOutputVar, 'Enter the format of the output NetCDF files names, example: mon_198301.nc')
		}else if(daydek == 'Monthly data'){
			tclvalue(inrfeff) <- "mon_%s%s.nc"
			tclvalue(outmrgff) <- "year_%s.nc"
			infobulle(en.inrfeff, 'Enter the format of the input NetCDF files names,\nexample: mon_198301.nc')
			status.bar.display(en.inrfeff, TextOutputVar, 'Enter the format of the input NetCDF files names, example: mon_198301.nc')
			infobulle(en.outmrgff, 'Enter the format of the output NetCDF files names,\nexample: year_1983.nc')
			status.bar.display(en.outmrgff, TextOutputVar, 'Enter the format of the output NetCDF files names, example: year_1983.nc')
		}


	################
	fr.C0 <- tkframe(fr.C)
	fr.C1 <- tkframe(fr.C)
	tkgrid(fr.C0)
	tkgrid(fr.C1)


	netcdf.out.txt <- tklabel(fr.C0, text = 'Defines NetCDF output variable', anchor = 'e', justify = 'left')
	tkgrid(netcdf.out.txt, sticky = 'ew', padx = 1, pady = 1)

	varid.out.txt <- tklabel(fr.C1, text = 'Varid', anchor = 'e', justify = 'right')
	infobulle(varid.out.txt, 'Name of the variable to be created for netcdf output')
	status.bar.display(varid.out.txt, TextOutputVar, 'Name of the variable to be created for netcdf output')

	varid.out <- tclVar(as.character(GeneralParameters$netcdf.var$Values[1]))
	varid.out.v <- tkentry(fr.C1, width = largeur2, textvariable = varid.out, justify = "left")
	infobulle(varid.out.v, 'Name of the variable to be created for netcdf output')
	status.bar.display(varid.out.v, TextOutputVar, 'Name of the variable to be created for netcdf output')

	longname.out.txt <- tklabel(fr.C1, text = 'Longname', anchor = 'e', justify = 'right')
	infobulle(longname.out.txt, 'Longer name of the variable to be created for netcdf output')
	status.bar.display(longname.out.txt, TextOutputVar, 'Longer name of the variable to be created for netcdf output')

	longname.out <- tclVar(as.character(GeneralParameters$netcdf.var$Values[2]))
	longname.out.v <- tkentry(fr.C1, width = largeur2, textvariable = longname.out, justify = "left")
	infobulle(longname.out.v, 'Longer name of the variable to be created for netcdf output')
	status.bar.display(longname.out.v, TextOutputVar, 'Longer name of the variable to be created for netcdf output')

	tkgrid(varid.out.txt, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(varid.out.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(longname.out.txt, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(longname.out.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)


	################################
	bt.prm.OK <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")
	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 5, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	tkconfigure(bt.prm.OK, command = function(){
		GeneralParameters$datesSE$Values <<- c(tclvalue(istart.yrs), tclvalue(istart.mon), tclvalue(istart.day),
		tclvalue(iend.yrs), tclvalue(iend.mon), tclvalue(iend.day))
		GeneralParameters$IO.file.format$Values <<- c(tclvalue(inrfeff), tclvalue(outmrgff))
		GeneralParameters$netcdf.var$Values <<- c(tclvalue(varid.out), tclvalue(longname.out))
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+',tt.x,'+',tt.y, sep = ''))
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

	fdf1 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	fdf2 <- tkframe(tt1)

	fr.fileformat1 <- ttklabelframe(fdf1, text = "File Format", labelanchor = "nw", relief = "groove", borderwidth = 2)
	fr.fileformat2 <- ttklabelframe(fdf1, text = "Dates Format", labelanchor = "nw", relief = "groove", borderwidth = 2)
	tkgrid(fr.fileformat1, row = 0, column = 0, sticky = 'nswe', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.fileformat2, row = 0, column = 1, sticky = 'nswe', padx = 1, pady = 1, ipadx = 1, ipady = 1)


	ffrmt1 <- tkradiobutton(fr.fileformat1, text = "One variable", anchor = 'w', justify = 'left')
	infobulle(ffrmt1, 'In case of single serie:\nThe file contains 1 variable')
	status.bar.display(ffrmt1, TextOutputVar, 'In case of single serie: The file contains 1 variable')
	ffrmt2 <- tkradiobutton(fr.fileformat1, text = "Rain Tmax Tmin", anchor = 'w', justify = 'left')
	infobulle(ffrmt2, 'In case of single serie:\nThe file contains Rain, Tmax\nand Tmin in this order')
	status.bar.display(ffrmt2, TextOutputVar, 'In case of single serie:The file contains Rain, Tmax and Tmin in this order')
	tkgrid(ffrmt1, row = 0, column = 0, sticky = "we")
	tkgrid(ffrmt2, row = 1, column = 0, sticky = "we")
	rbffrmt <- tclVar(as.character(GeneralParameters$file.date.format$Values[1]))
	tkconfigure(ffrmt1, variable = rbffrmt, value = "1")
	tkconfigure(ffrmt2, variable = rbffrmt, value = "0")

	if(speriod == 'Daily data') {txtdtfrmt1 <- "YYYYMMDD";txtdtfrmt2 <- "YYYY MM DD"}
	if(speriod == 'Dekadal data') {txtdtfrmt1 <- "YYYYMMD";txtdtfrmt2 <- "YYYY MM D"}
	if(speriod == 'Monthly data') {txtdtfrmt1 <- "YYYYMM";txtdtfrmt2 <- "YYYY MM"}

	dtfrmt1 <- tkradiobutton(fr.fileformat2, text = txtdtfrmt1, anchor = 'w', justify = 'left')
	infobulle(dtfrmt1, 'In case of single serie:\n dates are merged')
	status.bar.display(dtfrmt1, TextOutputVar, 'In case of single serie: dates are merged')
	dtfrmt2 <- tkradiobutton(fr.fileformat2, text = txtdtfrmt2, anchor = 'w', justify = 'left')
	infobulle(dtfrmt2, 'In case of single serie:\ndates are separated by space,\ntabulation or CSV format')
	status.bar.display(dtfrmt2, TextOutputVar, 'In case of single serie: dates are separated by space, tabulation or CSV format')
	tkgrid(dtfrmt1, row = 0, column = 0, sticky = "we")
	tkgrid(dtfrmt2, row = 1, column = 0, sticky = "we")
	tkgrid(tklabel(fr.fileformat2, text=''), row = 3, column = 0, sticky = "we")

	rbdtfrmt <- tclVar(as.character(GeneralParameters$file.date.format$Values[2]))
	tkconfigure(dtfrmt1, variable = rbdtfrmt, value = "1")
	tkconfigure(dtfrmt2, variable = rbdtfrmt, value = "0")

	bt.fileformat <- tkbutton(fdf2, text=" OK ")
	bt.prm.CA <- tkbutton(fdf2, text = "Cancel")
	tkgrid(bt.fileformat, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 5, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	tkconfigure(bt.fileformat, command = function(){
		GeneralParameters$file.date.format$Values <<- c(tclvalue(rbffrmt), tclvalue(rbdtfrmt))
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkgrid(fdf1, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fdf2, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+',tt.x,'+',tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1,'')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(top.win)})
	tkwait.window(tt1)
	return(GeneralParameters)
}


cdtDataset_getParams <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur1 <- 30
		largeur2 <- 45
		largeur3 <- 28
	}else{
		largeur1 <- 26
		largeur2 <- 35
		largeur3 <- 25
	}

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################

	frtimestep <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Pentad data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(GeneralParameters$Tstep,
									'daily' = cb.periodVAL[1],
									'pentad' = cb.periodVAL[2],
									'dekadal' = cb.periodVAL[3],
									'monthly' = cb.periodVAL[4])

	cb.period <- ttkcombobox(frtimestep, values = cb.periodVAL, textvariable = file.period, width = largeur1)

	tkgrid(cb.period, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)

	infobulle(cb.period, 'Select the time step of the data')
	status.bar.display(cb.period, TextOutputVar, 'Select the time step of the data')

	###########
	tkbind(cb.period, "<<ComboboxSelected>>", function(){
		tclvalue(day.txtVar) <- switch(tclvalue(file.period), 'Dekadal data' = 'Dek', 'Pentad data' = 'Pen', 'Day')
		stateday <- if(tclvalue(file.period) == 'Monthly data') 'disabled' else 'normal'
		tkconfigure(en.day1, state = stateday)
		tkconfigure(en.day2, state = stateday)
	})

	############################################

	frameNCDF <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	dir.NCDF <- tclVar(GeneralParameters$NCDF$dir)

	txt.NCDF <- tklabel(frameNCDF, text = 'Directory containing the NetCDF data', anchor = 'w', justify = 'left')
	set.NCDF <- tkbutton(frameNCDF, text = "Settings")
	en.NCDF <- tkentry(frameNCDF, textvariable = dir.NCDF, width = largeur2)
	bt.NCDF <- tkbutton(frameNCDF, text = "...")

	######
	tkconfigure(set.NCDF, command = function(){
		GeneralParameters[["NCDF"]] <<- getInfoNetcdfData(tt, GeneralParameters[["NCDF"]], str_trim(tclvalue(dir.NCDF)), tclvalue(file.period))
	})

	tkconfigure(bt.NCDF, command = function(){
		dirnc <- tk_choose.dir(getwd(), "")
		tclvalue(dir.NCDF) <- if(!is.na(dirnc)) dirnc else ""
	})

	######
	tkgrid(txt.NCDF, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.NCDF, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.NCDF, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.NCDF, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.NCDF, 'Enter the full path to the directory containing the NetCDF files')
	status.bar.display(en.NCDF, TextOutputVar, 'Enter the full path to the directory containing the NetCDF files')
	infobulle(bt.NCDF, 'Or browse here')
	status.bar.display(bt.NCDF, TextOutputVar, 'Or browse here')
	infobulle(set.NCDF, 'Setting netcdf data options')
	status.bar.display(set.NCDF, TextOutputVar, 'Setting netcdf data options')

	############################################

	frDate <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	istart.yrs <- tclVar(GeneralParameters$date.range$start.year)
	istart.mon <- tclVar(GeneralParameters$date.range$start.mon)
	istart.day <- tclVar(GeneralParameters$date.range$start.dek)
	iend.yrs <- tclVar(GeneralParameters$date.range$end.year)
	iend.mon <- tclVar(GeneralParameters$date.range$end.mon)
	iend.day <- tclVar(GeneralParameters$date.range$end.dek)

	txtdek <- switch(GeneralParameters$Tstep, 'dekadal' = 'Dek', 'pentad' = 'Pen', 'Day')
	day.txtVar <- tclVar(txtdek)
	statedate <- if(GeneralParameters$Tstep == 'monthly') 'disabled' else 'normal'

	frtxtDate <- ttklabelframe(frDate, text = "Date range to import", relief = 'groove')

	txt.deb <- tklabel(frtxtDate, text = 'Start date', anchor = 'e', justify = 'right')
	txt.fin <- tklabel(frtxtDate, text = 'End date', anchor = 'e', justify = 'right')
	txt.yrs <- tklabel(frtxtDate, text = 'Year')
	txt.mon <- tklabel(frtxtDate, text = 'Month')
	txt.day <- tklabel(frtxtDate, text = tclvalue(day.txtVar), textvariable = day.txtVar)
	en.yrs1 <- tkentry(frtxtDate, width = 4, textvariable = istart.yrs, justify = "right")
	en.mon1 <- tkentry(frtxtDate, width = 4, textvariable = istart.mon, justify = "right")
	en.day1 <- tkentry(frtxtDate, width = 4, textvariable = istart.day, justify = "right", state = statedate)
	en.yrs2 <- tkentry(frtxtDate, width = 4, textvariable = iend.yrs, justify = "right")
	en.mon2 <- tkentry(frtxtDate, width = 4, textvariable = iend.mon, justify = "right")
	en.day2 <- tkentry(frtxtDate, width = 4, textvariable = iend.day, justify = "right", state = statedate)

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

	tkgrid(frtxtDate, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

	infobulle(frtxtDate, 'Start and end date to import into the data')
	status.bar.display(frtxtDate, TextOutputVar, 'Start and end date to import into the data')

	############################################

	frUpdate <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	update.data <- tclVar(GeneralParameters$Update)
	file.dataRDS <- tclVar()

	stateUp <- if(tclvalue(update.data) == "1") "normal" else "disabled"

	chk.update <- tkcheckbutton(frUpdate, variable = update.data, text = "Add new data to an existing dataset", anchor = 'w', justify = 'left')
	en.update <- tkentry(frUpdate, textvariable = file.dataRDS, width = largeur2, state = stateUp)
	bt.update <- tkbutton(frUpdate, text = "...", state = stateUp)

	tkconfigure(bt.update, command = function(){
		filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
		path.update <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
		if(path.update == "") return(NULL)
		tclvalue(file.dataRDS) <- path.update

		if(file.exists(tclvalue(file.dataRDS))){
			tclvalue(dir2save) <- dirname(dirname(tclvalue(file.dataRDS)))
			tclvalue(nom.data) <- basename(dirname(tclvalue(file.dataRDS)))
		} 
	})

	tkgrid(chk.update, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.update, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.update, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(chk.update, 'Check this box to add new data to an existing dataset')
	status.bar.display(chk.update, TextOutputVar, 'Check this box to add new data to an existing dataset')
	infobulle(en.update, 'Enter the full path to the file <dataset name>.rds')
	status.bar.display(en.update, TextOutputVar, 'Enter the full path to the file <dataset name>.rds')
	infobulle(bt.update, 'or browse here')
	status.bar.display(bt.update, TextOutputVar, 'or browse here')

	###############
	tkbind(chk.update, "<Button-1>", function(){
		stateUp <- if(tclvalue(update.data) == '1') 'disabled' else 'normal'
		tkconfigure(en.update, state = stateUp)
		tkconfigure(bt.update, state = stateUp)
		stateOUT <- if(tclvalue(update.data) == '1') 'normal' else 'disabled'
		tkconfigure(en.dir2save, state = stateOUT)
		tkconfigure(bt.dir2save, state = stateOUT)
		tkconfigure(en.nomdata, state = stateOUT)
	})

	############################################

	frOutput <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	dir2save <- tclVar(GeneralParameters$output$dir)
	nom.data <- tclVar(GeneralParameters$output$data.name)

	stateOUT <- if(tclvalue(update.data) == "0") "normal" else "disabled"

	txt.dir2save <- tklabel(frOutput, text = 'Directory to put the dataset', anchor = 'w', justify = 'left')
	en.dir2save <- tkentry(frOutput, textvariable = dir2save, width = largeur2, state = stateOUT)
	bt.dir2save <- tkbutton(frOutput, text = "...", state = stateOUT)
	txt.nomdata <- tklabel(frOutput, text = 'Dataset name', anchor = 'e', justify = 'right')
	en.nomdata <- tkentry(frOutput, textvariable = nom.data, width = largeur3, state = stateOUT)

	#####

	tkconfigure(bt.dir2save, command = function(){
		dir2savepth <- tk_choose.dir(GeneralParameters$output$dir, "")
		if(is.na(dir2savepth)) tclvalue(dir2save) <- GeneralParameters$output$dir
		else{
			dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
			tclvalue(dir2save) <- dir2savepth
		}
	})

	#####

	tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.dir2save, row = 1, column = 6, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.nomdata, row = 2, column = 0, sticky = 'e', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.nomdata, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(en.dir2save, 'Enter the full path to directory to put the dataset')
	status.bar.display(en.dir2save, TextOutputVar, 'Enter the full path to directory to put the dataset')
	infobulle(bt.dir2save, 'or browse here')
	status.bar.display(bt.dir2save, TextOutputVar, 'or browse here')
	infobulle(en.nomdata, 'Specify the dataset name')
	status.bar.display(en.nomdata, TextOutputVar, 'Specify the dataset name')

	############################################
	tkgrid(frtimestep, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frameNCDF, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frDate, row = 2, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frUpdate, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frOutput, row = 4, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	############################################
	
	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(dir.NCDF))%in%c("", "NA")){
			tkmessageBox(message = "Select or enter the  directory containing the NetCDF files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if((tclvalue(update.data) == "1") & str_trim(tclvalue(file.dataRDS))%in%c("", "NA")){
			tkmessageBox(message = "Select or enter the path to the file <dataset name>.rds", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if((tclvalue(update.data) == "0") & str_trim(tclvalue(dir2save))%in%c("", "NA")){
			tkmessageBox(message = "Browse or enter the path to directory to put the dataset", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$Tstep <<- switch(str_trim(tclvalue(file.period)), 
												'Daily data' = 'daily',
												'Pentad data' = 'pentad',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')
			GeneralParameters$NCDF$dir <<- str_trim(tclvalue(dir.NCDF))

			GeneralParameters$date.range$start.year <<- as.numeric(str_trim(tclvalue(istart.yrs)))
			GeneralParameters$date.range$start.mon <<- as.numeric(str_trim(tclvalue(istart.mon)))
			GeneralParameters$date.range$start.dek <<- as.numeric(str_trim(tclvalue(istart.day)))
			GeneralParameters$date.range$end.year <<- as.numeric(str_trim(tclvalue(iend.yrs)))
			GeneralParameters$date.range$end.mon <<- as.numeric(str_trim(tclvalue(iend.mon)))
			GeneralParameters$date.range$end.dek <<- as.numeric(str_trim(tclvalue(iend.day)))

			GeneralParameters$Update <<- switch(tclvalue(update.data), '0' = FALSE, '1' = TRUE)
			GeneralParameters$cdtDataSet <<- str_trim(tclvalue(file.dataRDS))
			GeneralParameters$output$dir <<- str_trim(tclvalue(dir2save))
			GeneralParameters$output$data.name <<- str_trim(tclvalue(nom.data))

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

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################3
	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Import NetCDF files into CDT Dataset')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}


Merging_ScaleDataInfo <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur1 <- 27
		largeur2 <- 34
	}else{
		largeur1 <- 26
		largeur2 <- 32
	}

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frOUT <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################


	frDate <- tkframe(frOUT, relief = 'sunken', borderwidth = 2)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Pentad data', 'Dekadal data')
	tclvalue(file.period) <- switch(GeneralParameters$mrg.data$tstep,
									'daily' = cb.periodVAL[1],
									'pentad' = cb.periodVAL[2],
									'dekadal' = cb.periodVAL[3])

	istart.yrs <- tclVar(GeneralParameters$Scaling.Date$start.year)
	istart.mon <- tclVar(GeneralParameters$Scaling.Date$start.mon)
	istart.day <- tclVar(GeneralParameters$Scaling.Date$start.dek)
	iend.yrs <- tclVar(GeneralParameters$Scaling.Date$end.year)
	iend.mon <- tclVar(GeneralParameters$Scaling.Date$end.mon)
	iend.day <- tclVar(GeneralParameters$Scaling.Date$end.dek)

	txtdek <- switch(GeneralParameters$mrg.data$tstep, 'dekadal' = 'Dek', 'pentad' = 'Pen', 'Day')
	day.txtVar <- tclVar(txtdek)

	cb.period <- ttkcombobox(frDate, values = cb.periodVAL, textvariable = file.period, width = largeur1)
	frtxtDate <- ttklabelframe(frDate, text = "Date Range", relief = 'groove')

	txt.deb <- tklabel(frtxtDate, text = 'Start date', anchor = 'e', justify = 'right')
	txt.fin <- tklabel(frtxtDate, text = 'End date', anchor = 'e', justify = 'right')
	txt.yrs <- tklabel(frtxtDate, text = 'Year')
	txt.mon <- tklabel(frtxtDate, text = 'Month')
	txt.day <- tklabel(frtxtDate, text = tclvalue(day.txtVar), textvariable = day.txtVar)
	en.yrs1 <- tkentry(frtxtDate, width = 4, textvariable = istart.yrs, justify = "right")
	en.mon1 <- tkentry(frtxtDate, width = 4, textvariable = istart.mon, justify = "right")
	en.day1 <- tkentry(frtxtDate, width = 4, textvariable = istart.day, justify = "right")
	en.yrs2 <- tkentry(frtxtDate, width = 4, textvariable = iend.yrs, justify = "right")
	en.mon2 <- tkentry(frtxtDate, width = 4, textvariable = iend.mon, justify = "right")
	en.day2 <- tkentry(frtxtDate, width = 4, textvariable = iend.day, justify = "right")

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

	tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(frtxtDate, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

	infobulle(cb.period, 'Choose the time step of the merged data to be scaled')
	status.bar.display(cb.period, TextOutputVar, 'Choose the time step of the merged data to be scaled')
	infobulle(frtxtDate, 'Start and end date of the merged data to be scaled')
	status.bar.display(frtxtDate, TextOutputVar, 'Start and end date of the merged data to be scaled')

	###########
	tkbind(cb.period, "<<ComboboxSelected>>", function(){
		tclvalue(day.txtVar) <- switch(tclvalue(file.period), 'Dekadal data' = 'Dek', 'Pentad data' = 'Pen', 'Day')
	})

	############################################

	frmrgData <- tkframe(frOUT, relief = 'sunken', borderwidth = 2)

	dir.mrgData <- tclVar(GeneralParameters$mrg.data$dir)

	txt.mrgData <- tklabel(frmrgData, text = 'Directory containing merged data', anchor = 'w', justify = 'left')
	set.mrgData <- tkbutton(frmrgData, text = "Settings")
	en.mrgData <- tkentry(frmrgData, textvariable = dir.mrgData, width = largeur2)
	bt.mrgData <- tkbutton(frmrgData, text = "...")


	tkconfigure(set.mrgData, command = function(){
		GeneralParameters[["mrg.data"]] <<- getInfoNetcdfData(tt, GeneralParameters[["mrg.data"]], str_trim(tclvalue(dir.mrgData)), tclvalue(file.period))
	})

	tkconfigure(bt.mrgData, command = function(){
		dirmrg <- tk_choose.dir(getwd(), "")
		tclvalue(dir.mrgData) <- if(!is.na(dirmrg)) dirmrg else ""
	})

	tkgrid(txt.mrgData, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(set.mrgData, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.mrgData, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.mrgData, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(en.mrgData, 'Enter the full path to the directory containing the merged data to be scaled')
	status.bar.display(en.mrgData, TextOutputVar, 'Enter the full path to the directory containing the merged data to be scaled')
	infobulle(bt.mrgData, 'Or browse here')
	status.bar.display(bt.mrgData, TextOutputVar, 'Or browse here')
	infobulle(set.mrgData, 'Setting netcdf data options')
	status.bar.display(set.mrgData, TextOutputVar, 'Setting netcdf data options')

	############################################

	frScaleData <- tkframe(frOUT, relief = 'sunken', borderwidth = 2)

	scale.fun <- tclVar(GeneralParameters$scale.data$fun)
	scale.dir <- tclVar(GeneralParameters$scale.data$dir)

	txt.scaledata <- tklabel(frScaleData, text = 'Directory of data to use for scaling', anchor = 'w', justify = 'left')
	set.scaledata <- tkbutton(frScaleData, text = "Settings")
	en.scaledata <- tkentry(frScaleData, textvariable = scale.dir, width = largeur2)
	bt.scaledata <- tkbutton(frScaleData, text = "...")
	txt.scalefun <- tklabel(frScaleData, text = 'Function to use for scaling', anchor = 'w', justify = 'left')
	cb.scalefun <- ttkcombobox(frScaleData, values = c("sum", "mean"), textvariable = scale.fun, width = 5)

	tkconfigure(set.scaledata, command = function(){
		GeneralParameters[["scale.data"]] <<- getInfoNetcdfData(tt, GeneralParameters[["scale.data"]], str_trim(tclvalue(scale.dir)), tclvalue(file.period), scale = TRUE)
	})

	tkconfigure(bt.scaledata, command = function(){
		dirscale <- tk_choose.dir(getwd(), "")
		tclvalue(scale.dir) <- if(!is.na(dirscale)) dirscale else ""
	})

	tkgrid(txt.scaledata, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(set.scaledata, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.scaledata, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.scaledata, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.scalefun, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.scalefun, row = 2, column = 2, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(en.scaledata, 'Enter the full path to the directory containing the merged data to be used for scaling')
	status.bar.display(en.scaledata, TextOutputVar, 'Enter the full path to the directory containing the merged data to be used for scaling')
	infobulle(bt.scaledata, 'Or browse here')
	status.bar.display(bt.scaledata, TextOutputVar, 'Or browse here')
	infobulle(set.scaledata, 'Setting netcdf data options')
	status.bar.display(set.scaledata, TextOutputVar, 'Setting netcdf data options')

	############################################

	frSave <- tkframe(frOUT, relief = 'sunken', borderwidth = 2)

	dir2save <- tclVar(GeneralParameters$outdir)

	txt.dir2save <- tklabel(frSave, text = 'Directory to save result', anchor = 'w', justify = 'left')
	en.dir2save <- tkentry(frSave, textvariable = dir2save, width = largeur2)
	bt.dir2save <- tkbutton(frSave, text = "...")

	#####

	tkconfigure(bt.dir2save, command = function(){
		dir2savepth <- tk_choose.dir(GeneralParameters$outdir, "")
		if(is.na(dir2savepth)) tclvalue(dir2save) <- GeneralParameters$outdir
		else{
			dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
			tclvalue(dir2save) <- dir2savepth
		}
	})

	#####

	tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.dir2save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(en.dir2save, 'Enter the full path to directory to save result')
	status.bar.display(en.dir2save, TextOutputVar, 'Enter the full path to directory to save result')
	infobulle(bt.dir2save, 'or browse here')
	status.bar.display(bt.dir2save, TextOutputVar, 'or browse here')

	############################################
	tkgrid(frDate, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frmrgData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frScaleData, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################

	tkgrid(frOUT, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	#######

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(dir.mrgData))%in%c("", "NA")){
			tkmessageBox(message = "Choose or enter the  directory containing the merged files to be scaled", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(scale.dir))%in%c("", "NA")){
			tkmessageBox(message = "Choose or enter the  directory containing the netcdf files to be used for scaling", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(dir2save))%in%c("", "NA")){
			tkmessageBox(message = "Choose or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$mrg.data$tstep <<- switch(str_trim(tclvalue(file.period)), 
														'Daily data' = 'daily',
														'Pentad data' = 'pentad',
														'Dekadal data' =  'dekadal')
			GeneralParameters$Scaling.Date$start.year <<- as.numeric(str_trim(tclvalue(istart.yrs)))
			GeneralParameters$Scaling.Date$start.mon <<- as.numeric(str_trim(tclvalue(istart.mon)))
			GeneralParameters$Scaling.Date$start.dek <<- as.numeric(str_trim(tclvalue(istart.day)))
			GeneralParameters$Scaling.Date$end.year <<- as.numeric(str_trim(tclvalue(iend.yrs)))
			GeneralParameters$Scaling.Date$end.mon <<- as.numeric(str_trim(tclvalue(iend.mon)))
			GeneralParameters$Scaling.Date$end.dek <<- as.numeric(str_trim(tclvalue(iend.day)))

			GeneralParameters$mrg.data$dir <<- str_trim(tclvalue(dir.mrgData))
			GeneralParameters$scale.data$dir <<- str_trim(tclvalue(scale.dir))
			GeneralParameters$scale.data$fun <<- str_trim(tclvalue(scale.fun))
			GeneralParameters$outdir <<- str_trim(tclvalue(dir2save))

			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
		}
	})

	#######

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

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Scale merged data - Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}


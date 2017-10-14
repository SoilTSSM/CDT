
Temp_coefDownGetInfo <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur <- 38
		largeur1 <- 35
	}else{
		largeur <- 34
		largeur1 <- 33
	}

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################
	frSTN <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.stnfl <- tclVar(GeneralParameters$IO.files$STN.file)

	txt.stnfl <- tklabel(frSTN, text = 'Station data file', anchor = 'w', justify = 'left')
	cb.stnfl <- ttkcombobox(frSTN, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
	bt.stnfl <- tkbutton(frSTN, text = "...")

	######
	tkconfigure(bt.stnfl, command = function(){
		dat.opfiles <- getOpenFiles(tt, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.stnfl) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
		}else return(NULL)
	})

	tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.stnfl, 'Choose the file in the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the gauge data')
	infobulle(bt.stnfl, 'Browse file if not listed')
	status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')

	############################################

	frDEM <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.grddem <- tclVar(GeneralParameters$IO.files$DEM.file)

	txt.grddem <- tklabel(frDEM, text = "Elevation data(NetCDF)", anchor = 'w', justify = 'left')
	cb.grddem <- ttkcombobox(frDEM, values = unlist(listOpenFiles), textvariable = file.grddem, width = largeur1)
	bt.grddem <- tkbutton(frDEM, text = "...")

	####
	tkconfigure(bt.grddem, command = function(){
		nc.opfiles <- getOpenNetcdf(tt, all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grddem) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
		}else return(NULL)
	})

	#####
	tkgrid(txt.grddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.grddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.grddem, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.grddem, 'Select the file from the list')
	status.bar.display(cb.grddem, TextOutputVar, 'File containing the elevation data in netcdf')
	infobulle(bt.grddem, 'Browse file if not listed')
	status.bar.display(bt.grddem, TextOutputVar, 'Browse file if not listed')

	############################################

	frDate <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Pentad data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(GeneralParameters$period,
									'daily' = cb.periodVAL[1],
									'pentad' = cb.periodVAL[2],
									'dekadal' = cb.periodVAL[3],
									'monthly' = cb.periodVAL[4])
	year1 <- tclVar(GeneralParameters$Down.Date.Range$start.year)
	year2 <- tclVar(GeneralParameters$Down.Date.Range$end.year)

	cb.period <- ttkcombobox(frDate, values = cb.periodVAL, textvariable = file.period, width = largeur1)

	fr.basePeriod <- ttklabelframe(frDate, text = "Base period", relief = 'groove', labelanchor = "n")
	txt.years1 <- tklabel(fr.basePeriod, text = 'Start Year', anchor = 'e', justify = 'right')
	txt.years2 <- tklabel(fr.basePeriod, text = 'End Year', anchor = 'e', justify = 'right')
	en.years1 <- tkentry(fr.basePeriod, width = 6, textvariable = year1, justify = 'right')
	en.years2 <- tkentry(fr.basePeriod, width = 6, textvariable = year2, justify = 'right')

	tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(fr.basePeriod, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 5, ipadx = 1, ipady = 1)

	tkgrid(txt.years1, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.years1, row = 0, column = 1, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.years2, row = 0, column = 3, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.years2, row = 0, column = 4, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.period, 'Select the time step of the data')
	status.bar.display(cb.period, TextOutputVar, 'Select the time step of the data')
	infobulle(en.years1, 'Start year to be used to compute regression parameters\nbetween station temperature and elevation')
	status.bar.display(en.years1, TextOutputVar, 'Start year to be used to compute regression parameters\nbetween station temperature and elevation')
	infobulle(en.years2, 'End year to be used to compute regression parameters\nbetween station temperature and elevation')
	status.bar.display(en.years2, TextOutputVar, 'End year to be used to compute regression parameters\nbetween station temperature and elevation')

	############################################

	frSave <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.save1 <- tclVar(GeneralParameters$IO.files$dir2save)

	txt.file.save <- tklabel(frSave, text = 'Directory to save result', anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frSave, textvariable = file.save1, width = largeur)
	bt.file.save <- tkbutton(frSave, text = "...")

	#####

	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(GeneralParameters$IO.files$dir2save, "")
		if(is.na(file2save1)) tclvalue(file.save1) <- GeneralParameters$IO.files$dir2save
		else{
			dir.create(file2save1, showWarnings = FALSE, recursive = TRUE)
			tclvalue(file.save1) <- file2save1
		}
	})

	#####

	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.file.save, 'Enter the full path to directory to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save result')
	infobulle(bt.file.save, 'or browse here')
	status.bar.display(bt.file.save, TextOutputVar, 'or browse here')

	############################################
	tkgrid(frSTN, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frDEM, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frDate, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################
	
	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(file.stnfl)) == ""){
			tkmessageBox(message = "Select the file containing the gauge data", icon = "warning", type = "ok")
			#tkwait.window(tt)
		}else if(str_trim(tclvalue(file.grddem)) == "" ){
			tkmessageBox(message = "You have to choose DEM data in NetCDF format", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.save1)) == "" | str_trim(tclvalue(file.save1)) == "NA"){
			tkmessageBox(message = "Select or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$period <<- switch(str_trim(tclvalue(file.period)), 
												'Daily data' = 'daily',
												'Pentad data' = 'pentad',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')
			GeneralParameters$Down.Date.Range$start.year <<- as.numeric(str_trim(tclvalue(year1)))
			GeneralParameters$Down.Date.Range$end.year <<- as.numeric(str_trim(tclvalue(year2)))

			GeneralParameters$IO.files$STN.file <<- str_trim(tclvalue(file.stnfl))
			GeneralParameters$IO.files$DEM.file <<- str_trim(tclvalue(file.grddem))
			GeneralParameters$IO.files$dir2save <<- str_trim(tclvalue(file.save1))

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
	tkwm.title(tt, 'Coefficients Downscaling-Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

#######################################################################################################################################

Temp_reanalDownGetInfo <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur <- 38
		largeur1 <- 35
	}else{
		largeur <- 28
		largeur1 <- 27
	}

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	frRight <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################

	frCoef <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.coef <- tclVar(str_trim(GeneralParameters$DownCoef.file))

	txt.coeffl <- tklabel(frCoef, text = 'Downscaling Coefficients file', anchor = 'w', justify = 'left')
	en.coeffl <- tkentry(frCoef, textvariable = file.coef, width = largeur)
	bt.coeffl <- tkbutton(frCoef, text = "...")

	tkconfigure(bt.coeffl, command = function(){
		file2coef <- tkgetOpenFile(initialdir = getwd(), initialfile = "",
						filetypes = "{{Text Files} {.txt .TXT}} {{CSV Files} {.csv .CSV}} {{All files} *}")
		tclvalue(file.coef) <- if(!is.na(file2coef)) file2coef else ""
	})

	tkgrid(txt.coeffl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.coeffl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.coeffl, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.coeffl, 'Enter the full path of the file containing the regression coef for downscaling')
	status.bar.display(en.coeffl, TextOutputVar, 'Enter the full path of the file containing the regression coef for downscaling')
	infobulle(bt.coeffl, 'or browse here')
	status.bar.display(bt.coeffl, TextOutputVar, 'or browse here')

	############################################

	frReanal <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	dir.REANAL <- tclVar(GeneralParameters$REANAL$dir)

	txt.REANAL <- tklabel(frReanal, text = 'Directory of Reanalysis files', anchor = 'w', justify = 'left')
	set.REANAL <- tkbutton(frReanal, text = "Settings")
	en.REANAL <- tkentry(frReanal, textvariable = dir.REANAL, width = largeur)
	bt.REANAL <- tkbutton(frReanal, text = "...")

	tkconfigure(set.REANAL, command = function(){
		GeneralParameters[["REANAL"]] <<- getInfoNetcdfData(tt, GeneralParameters[["REANAL"]], str_trim(tclvalue(dir.REANAL)), tclvalue(file.period))
	})

	tkconfigure(bt.REANAL, command = function(){
		dirreanal <- tk_choose.dir(getwd(), "")
		tclvalue(dir.REANAL) <- if(!is.na(dirreanal)) dirreanal else ""
	})

	tkgrid(txt.REANAL, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.REANAL, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.REANAL, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.REANAL, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.REANAL, 'Enter the full path to directory containing the Reanalysis files')
	status.bar.display(en.REANAL, TextOutputVar, 'Enter the full path to directory containing the Reanalysis files')
	infobulle(bt.REANAL, 'Or browse here')
	status.bar.display(bt.REANAL, TextOutputVar, 'Or browse here')
	infobulle(set.REANAL, 'Setting netcdf data options')
	status.bar.display(set.REANAL, TextOutputVar, 'Setting netcdf data options')

	############################################
	frDEM <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.grddem <- tclVar(GeneralParameters$DEM.file)

	txt.grddem <- tklabel(frDEM, text = "Elevation data(NetCDF)", anchor = 'w', justify = 'left')
	cb.grddem <- ttkcombobox(frDEM, values = unlist(listOpenFiles), textvariable = file.grddem, width = largeur1)
	bt.grddem <- tkbutton(frDEM, text = "...")

	####
	tkconfigure(bt.grddem, command = function(){
		nc.opfiles <- getOpenNetcdf(tt, all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grddem) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
		}else return(NULL)
	})

	#####
	tkgrid(txt.grddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.grddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.grddem, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.grddem, 'Select the file from the list')
	status.bar.display(cb.grddem, TextOutputVar, 'File containing the elevation data in netcdf')
	infobulle(bt.grddem, 'Browse file if not listed')
	status.bar.display(bt.grddem, TextOutputVar, 'Browse file if not listed')

	############################################

	frGrid <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	varCreateGrd <- tclVar(GeneralParameters$Grid.Creation$grid)
	stategrd <- if(str_trim(GeneralParameters$Grid.Creation$grid) == '2') 'normal' else 'disabled'

	txt.CreateGrd <- tklabel(frGrid, text = 'Create grid for interpolation', anchor = 'w', justify = 'left')
	rbt.CreateGrd1 <- tkradiobutton(frGrid, text = "From DEM", anchor = 'w', justify = 'left')
	rbt.CreateGrd2 <- tkradiobutton(frGrid, text = "New Grid", anchor = 'w', justify = 'left')
	bt.CreateGrd <- tkbutton(frGrid, text = "Create", state = stategrd)
	bt.down.interp <- ttkbutton(frGrid, text = "Downscaling Interpolations Parameters", width = largeur)

	####
	tkconfigure(rbt.CreateGrd1, variable = varCreateGrd, value = "1")
	tkconfigure(rbt.CreateGrd2, variable = varCreateGrd, value = "2")

	tkconfigure(bt.CreateGrd, command = function(){
		GeneralParameters[["Grid.Creation"]] <<- getNewGridParams(tt, GeneralParameters[["Grid.Creation"]])
	})

	tkconfigure(bt.down.interp, command = function(){
		GeneralParameters[["Interpolation"]] <<- getInterpolationPars(tt, GeneralParameters[["Interpolation"]], interpChoix = 2)
	})

	#####

	tkgrid(txt.CreateGrd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(rbt.CreateGrd1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(rbt.CreateGrd2, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.CreateGrd, row = 1, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.down.interp, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(bt.CreateGrd, 'Set the new grid')
	status.bar.display(bt.CreateGrd, TextOutputVar, 'Set the new grid')
	infobulle(frGrid, 'Create the grid to interpolate the downscaled data')
	status.bar.display(frGrid, TextOutputVar, 'Create the grid to interpolate the downscaled data')

	###########
	tkbind(rbt.CreateGrd1, "<Button-1>", function(){
		tkconfigure(bt.CreateGrd, state = 'disabled')
	})
	tkbind(rbt.CreateGrd2, "<Button-1>", function(){
		tkconfigure(bt.CreateGrd, state = 'normal')
	})

	############################################
	tkgrid(frCoef, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frReanal, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frDEM, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frGrid, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	##############################################  RIGHT   ############################################

	frtimestep <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Pentad data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(GeneralParameters$period,
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

	frDate <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	istart.yrs <- tclVar(GeneralParameters$Down.Date.Range$start.year)
	istart.mon <- tclVar(GeneralParameters$Down.Date.Range$start.mon)
	istart.day <- tclVar(GeneralParameters$Down.Date.Range$start.dek)
	iend.yrs <- tclVar(GeneralParameters$Down.Date.Range$end.year)
	iend.mon <- tclVar(GeneralParameters$Down.Date.Range$end.mon)
	iend.day <- tclVar(GeneralParameters$Down.Date.Range$end.dek)

	txtdek <- switch(GeneralParameters$period, 'dekadal' = 'Dek', 'pentad' = 'Pen', 'Day')
	day.txtVar <- tclVar(txtdek)
	statedate <- if(GeneralParameters$period == 'monthly') 'disabled' else 'normal'

	frtxtDate <- ttklabelframe(frDate, text = "Downscaling Date Range", relief = 'groove')

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

	infobulle(frtxtDate, 'Start and end date for downscaling reanalysis data')
	status.bar.display(frtxtDate, TextOutputVar, 'Start and end date for downscaling reanalysis data')

	############################################

	frSave <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	dir2save <- tclVar(GeneralParameters$output$dir)
	outdownff <- tclVar(GeneralParameters$output$format)

	txt.dir2save <- tklabel(frSave, text = 'Directory to save result', anchor = 'w', justify = 'left')
	en.dir2save <- tkentry(frSave, textvariable = dir2save, width = largeur)
	bt.dir2save <- tkbutton(frSave, text = "...")
	txt.outdownff <- tklabel(frSave, text = 'Downscaled data filename format', anchor = 'w', justify = 'left')
	en.outdownff <- tkentry(frSave, textvariable = outdownff, width = largeur)

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

	tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.dir2save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.outdownff, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.outdownff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.dir2save, 'Enter the full path to directory to save result')
	status.bar.display(en.dir2save, TextOutputVar, 'Enter the full path to directory to save result')
	infobulle(bt.dir2save, 'or browse here')
	status.bar.display(bt.dir2save, TextOutputVar, 'or browse here')
	infobulle(en.outdownff, 'Format of the downscaled data files names in NetCDF,\nexample: tmax_down_1981011.nc')
	status.bar.display(en.outdownff, TextOutputVar, 'Format of the downscaled data files names in NetCDF,\nexample: tmax_down_1981011.nc')

	############################################

	tkgrid(frtimestep, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 10, ipady = 5)
	tkgrid(frDate, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################

	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = '', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	#########

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(file.coef)) == ""){
			tkmessageBox(message = "Provide the file containing the coefficients to used for downscaling", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(dir.REANAL)) == "" | str_trim(tclvalue(dir.REANAL)) == "NA"){
			tkmessageBox(message = "Select or enter the path to directory containing the Reanalysis files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.grddem)) == ""){
			tkmessageBox(message = "You have to provide DEM data in NetCDF format", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(dir2save)) == "" | str_trim(tclvalue(dir2save)) == "NA"){
			tkmessageBox(message = "Select or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$DownCoef.file <<- str_trim(tclvalue(file.coef))
			GeneralParameters$DEM.file <<- str_trim(tclvalue(file.grddem))
			GeneralParameters$REANAL$dir <<- str_trim(tclvalue(dir.REANAL))
			GeneralParameters$output$dir <<- str_trim(tclvalue(dir2save))
			GeneralParameters$output$format <<- str_trim(tclvalue(outdownff))

			GeneralParameters$period <<- switch(str_trim(tclvalue(file.period)), 
												'Daily data' = 'daily',
												'Pentad data' = 'pentad',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')
			GeneralParameters$Down.Date.Range$start.year <<- as.numeric(str_trim(tclvalue(istart.yrs)))
			GeneralParameters$Down.Date.Range$start.mon <<- as.numeric(str_trim(tclvalue(istart.mon)))
			GeneralParameters$Down.Date.Range$start.dek <<- as.numeric(str_trim(tclvalue(istart.day)))
			GeneralParameters$Down.Date.Range$end.year <<- as.numeric(str_trim(tclvalue(iend.yrs)))
			GeneralParameters$Down.Date.Range$end.mon <<- as.numeric(str_trim(tclvalue(iend.mon)))
			GeneralParameters$Down.Date.Range$end.dek <<- as.numeric(str_trim(tclvalue(iend.day)))

			GeneralParameters$Grid.Creation$grid <<- str_trim(tclvalue(varCreateGrd))

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

	############################
	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Reanalysis Downscaling - Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}


#######################################################################################################################################


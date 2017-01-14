
mergeGetInfoRain <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur <- 28
		largeur1 <- 25
	}else{
		largeur <- 25
		largeur1 <- 24
	}

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	frRight <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	frRight1 <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################

	frMrg <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	mrg.method <- tclVar(str_trim(GeneralParameters$Mrg.Method))
	cb.MrgMthd <- c("Regression Kriging", "Spatio-Temporal LM")

	txt.mrg <- tklabel(frMrg, text = 'Merging method', anchor = 'w', justify = 'left')
	cb.mrg <- ttkcombobox(frMrg, values = cb.MrgMthd, textvariable = mrg.method, width = largeur1)

	tkgrid(txt.mrg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.mrg, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.mrg, 'Method to be used to perform merging')
	status.bar.display(cb.mrg, TextOutputVar, 'Method to be used to perform merging')

	tkbind(cb.mrg, "<<ComboboxSelected>>", function(){
		stateLM <- if(tclvalue(mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'
		tkconfigure(en.dir.LM, state = stateLM)
		tkconfigure(bt.dir.LM, state = stateLM)
	})

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
			tkconfigure(cb.grdrfe, values = unlist(listOpenFiles), textvariable = file.grdrfe)
			tkconfigure(cb.blkshp, values = unlist(listOpenFiles), textvariable = file.blkshp)
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

	frRFE <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	dir.rfe <- tclVar(GeneralParameters$IO.files$RFE.dir)
	file.grdrfe <- tclVar(GeneralParameters$IO.files$RFE.file)
	inrfeff <- tclVar(GeneralParameters$FileFormat$RFE.File.Format)
	newGrd <- tclVar(GeneralParameters$Create.Grid)

	statenewGrd <- if(tclvalue(newGrd) == '0') 'disabled' else 'normal'

	txt.dir.rfe <- tklabel(frRFE, text = 'Directory of RFE or ADJ-RFE files', anchor = 'w', justify = 'left')
	en.dir.rfe <- tkentry(frRFE, textvariable = dir.rfe, width = largeur)
	bt.dir.rfe <- tkbutton(frRFE, text = "...")
	txt.grdrfe <- tklabel(frRFE, text = "RFE or ADJ-RFE sample file", anchor = 'w', justify = 'left')
	cb.grdrfe <- ttkcombobox(frRFE, values = unlist(listOpenFiles), textvariable = file.grdrfe, width = largeur1)
	bt.grdrfe <- tkbutton(frRFE, text = "...")
	txt.inrfeff <- tklabel(frRFE, text = 'Input RFE or ADJ-RFE filename format', anchor = 'w', justify = 'left')
	en.inrfeff <- tkentry(frRFE, textvariable = inrfeff, width = largeur)
	cb.newGrd <- tkcheckbutton(frRFE, variable = newGrd, text = 'Create Grid', anchor = 'w', justify = 'left')
	bt.newGrd <- tkbutton(frRFE, text = "Set New Grid", state = statenewGrd)

	######
	tkconfigure(bt.dir.rfe, command = function(){
		dir4rfe <- tk_choose.dir(GeneralParameters$IO.files$RFE.dir, "")
		tclvalue(dir.rfe) <- if(!is.na(dir4rfe)) dir4rfe else ""
	})

	tkconfigure(bt.grdrfe, command = function(){
		nc.opfiles <- getOpenNetcdf(tt, all.opfiles, initialdir = tclvalue(dir.rfe))
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grdrfe) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
			tkconfigure(cb.grdrfe, values = unlist(listOpenFiles), textvariable = file.grdrfe)
			tkconfigure(cb.blkshp, values = unlist(listOpenFiles), textvariable = file.blkshp)
		}else return(NULL)
	})

	tkconfigure(bt.newGrd, command = function(){
		GeneralParameters <<- createNewGrid2Merge(tt, GeneralParameters)
		if(GeneralParameters$Grid.From == '2'){
			tkconfigure(cb.grddem, state = 'normal')
			tkconfigure(bt.grddem, state = 'normal')
		}
	})

	######

	tkgrid(txt.dir.rfe, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.dir.rfe, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.rfe, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.grdrfe, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.grdrfe, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.grdrfe, row = 3, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.inrfeff, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.inrfeff, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.newGrd, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 0, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(bt.newGrd, row = 6, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 0, pady = 3, ipadx = 1, ipady = 1)

	infobulle(en.dir.rfe, 'Enter the full path to directory containing\nthe RFE or Adjusted RFE files')
	status.bar.display(en.dir.rfe, TextOutputVar, 'Enter the full path to directory containing the RFE or Adjusted RFE files')
	infobulle(bt.dir.rfe, 'or browse here')
	status.bar.display(bt.dir.rfe, TextOutputVar, 'or browse here')
	infobulle(cb.grdrfe, 'Choose the file in the list')
	status.bar.display(cb.grdrfe, TextOutputVar, 'File containing a sample of RFE or Adjusted RFE data in netcdf')
	infobulle(bt.grdrfe, 'Browse file if not listed')
	status.bar.display(bt.grdrfe, TextOutputVar, 'Browse file if not listed')
	infobulle(en.inrfeff, 'Enter the format of the RFE or Adjusted RFE files names in NetCDF,\nexample: rfe1983_01-dk2.nc or rr_adj_%s%s%s.nc')
	status.bar.display(en.inrfeff, TextOutputVar, 'Enter the format of the RFE or Adjusted RFE files names in NetCDF,\nexample: rfe1983_01-dk2.nc or rr_adj_%s%s%s.nc')
	infobulle(cb.newGrd, 'Set the new grid to merge data in case using no adjusted RFE data')
	status.bar.display(cb.newGrd, TextOutputVar, 'Set the new grid to merge data in case using no adjusted RFE data')
	infobulle(bt.newGrd, 'Set the new grid to merge data in case using no adjusted RFE data')
	status.bar.display(bt.newGrd, TextOutputVar, 'Set the new grid to merge data in case using no adjusted RFE data')

	######

	tkbind(cb.newGrd, "<Button-1>", function(){
		statenewGrd <- if(tclvalue(newGrd) == '0') 'normal' else 'disabled'
		tkconfigure(bt.newGrd, state = statenewGrd)

		newgrd <- tclvalue(newGrd) == '0' & GeneralParameters$Grid.From == '2'
		auxvar <- tclvalue(dem.auxvar) == '1' | tclvalue(slope.auxvar) == '1' | tclvalue(aspect.auxvar) == '1'
		blank <- tclvalue(blankGrd) == 'Use DEM'
		statedem <- if(auxvar | newgrd | blank) 'normal' else 'disabled'
		tkconfigure(cb.grddem, state = statedem)
		tkconfigure(bt.grddem, state = statedem)
	})

	############################################

	frDirLM <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	dir.LMCoef <- tclVar(GeneralParameters$IO.files$LMCoef.dir)
	stateLMCoef <- if(str_trim(GeneralParameters$Mrg.Method) == "Regression Kriging") 'disabled' else 'normal'
	
	txt.dir.LM <- tklabel(frDirLM, text = "Directory of LMCoef files", anchor = 'w', justify = 'left')
	en.dir.LM <- tkentry(frDirLM, textvariable = dir.LMCoef, width = largeur, state = stateLMCoef)
	bt.dir.LM <- tkbutton(frDirLM, text = "...", state = stateLMCoef)

	#####
	tkconfigure(bt.dir.LM, command = function(){
		dir4LM <- tk_choose.dir(GeneralParameters$IO.files$LMCoef.dir, "")
		tclvalue(dir.LMCoef) <- if(!is.na(dir4LM)) dir4LM else ""
	})

	#####

	tkgrid(txt.dir.LM, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.dir.LM, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.LM, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.dir.LM, 'Enter the full path to directory containing the LM coefficients files')
	status.bar.display(en.dir.LM, TextOutputVar, 'Enter the full path to directory containing the LM coefficients files')
	infobulle(bt.dir.LM, 'or browse here')
	status.bar.display(bt.dir.LM, TextOutputVar, 'or browse here')

	############################################
	tkgrid(frMrg, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSTN, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRFE, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frDirLM, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################  RIGHT   #####################

	frDate <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(GeneralParameters$period, 
									'daily' = cb.periodVAL[1], 
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3])
	istart.yrs <- tclVar(GeneralParameters$Mrg.Date.Range$start.year)
	istart.mon <- tclVar(GeneralParameters$Mrg.Date.Range$start.mon)
	istart.day <- tclVar(GeneralParameters$Mrg.Date.Range$start.dek)
	iend.yrs <- tclVar(GeneralParameters$Mrg.Date.Range$end.year)
	iend.mon <- tclVar(GeneralParameters$Mrg.Date.Range$end.mon)
	iend.day <- tclVar(GeneralParameters$Mrg.Date.Range$end.dek)
	day.txtVar <- if(GeneralParameters$period == 'dekadal') tclVar('Dek') else tclVar('Day')
	statedate <- if(GeneralParameters$period == 'monthly') 'disabled' else 'normal'
	# use.months <- tclVar(paste(GeneralParameters$Mrg.Months, collapse=' '))

	cb.period <- ttkcombobox(frDate, values = cb.periodVAL, textvariable = file.period, width = largeur1)
	frtxtDate <- ttklabelframe(frDate, text = "Date Range", relief = 'groove')

	deb.txt <- tklabel(frtxtDate, text = 'Start date', anchor = 'e', justify = 'right')
	fin.txt <- tklabel(frtxtDate, text = 'End date', anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(frtxtDate, text = 'Year')
	mon.txt <- tklabel(frtxtDate, text = 'Month')
	day.txt <- tklabel(frtxtDate, text = tclvalue(day.txtVar), textvariable = day.txtVar)
	yrs1.v <- tkentry(frtxtDate, width = 4, textvariable = istart.yrs, justify = "right")
	mon1.v <- tkentry(frtxtDate, width = 4, textvariable = istart.mon, justify = "right")
	day1.v <- tkentry(frtxtDate, width = 4, textvariable = istart.day, justify = "right", state = statedate)
	yrs2.v <- tkentry(frtxtDate, width = 4, textvariable = iend.yrs, justify = "right")
	mon2.v <- tkentry(frtxtDate, width = 4, textvariable = iend.mon, justify = "right")
	day2.v <- tkentry(frtxtDate, width = 4, textvariable = iend.day, justify = "right", state = statedate)

	# txt.months <- tklabel(frDate, text = 'Months', anchor = 'w', justify = 'left')
	# en.months <- tkentry(frDate, width = 20, textvariable = use.months, justify = "left")

	tkgrid(deb.txt, row = 1, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fin.txt, row = 2, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrs.txt, row = 0, column = 1, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon.txt, row = 0, column = 2, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(day.txt, row = 0, column = 3, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrs1.v, row = 1, column = 1, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon1.v, row = 1, column = 2, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(day1.v, row = 1, column = 3, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrs2.v, row = 2, column = 1, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon2.v, row = 2, column = 2, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(day2.v, row = 2, column = 3, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frtxtDate, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
	# tkgrid(txt.months, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	# tkgrid(en.months, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.period, 'Choose the time step of the data')
	status.bar.display(cb.period, TextOutputVar, 'Choose the time step of the data')
	infobulle(frtxtDate, 'Start and end date to merge RFE or ADJ-RFE data')
	status.bar.display(frtxtDate, TextOutputVar, 'Start and end date to merge RFE or ADJ-RFE data')
	# infobulle(en.months, 'Months to be adjusted')
	# status.bar.display(en.months, TextOutputVar, 'Months to be merged')

	###########
	tkbind(cb.period, "<<ComboboxSelected>>", function(){
		tclvalue(day.txtVar) <- if(tclvalue(file.period) == 'Dekadal data') "Dek" else "Day"
		stateday <- if(tclvalue(file.period) == 'Monthly data') 'disabled' else 'normal'
		tkconfigure(day1.v, state = stateday)
		tkconfigure(day2.v, state = stateday)
		if(tclvalue(file.period) == 'Daily data'){
			tkconfigure(chk.scale.day, state = 'normal')
			scaleday <- if(tclvalue(scale.day) == '1') 'normal' else 'disabled'
			tkconfigure(bt.scale.day, state = scaleday)
		}else{
			tkconfigure(chk.scale.day, state = 'disabled')
			tkconfigure(bt.scale.day, state = 'disabled')
		}
	})

	############################################

	frInterp <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	interp.method <- tclVar()
	cb.InterpVAL <- c('Ordinary Kriging', 'Inverse Distance Weighted')
	tclvalue(interp.method) <- switch(GeneralParameters$Interpolation.pars$interp.method, 
										'Kriging' = cb.InterpVAL[1], 
										'IDW' = cb.InterpVAL[2])
	nmin <- tclVar(GeneralParameters$Interpolation.pars$nmin)
	nmax <- tclVar(GeneralParameters$Interpolation.pars$nmax)
	maxdist <- tclVar(GeneralParameters$Interpolation.pars$maxdist)
	# res.coarse <- tclVar(GeneralParameters$Interpolation.pars$res.coarse)

	txt.Interp <- tklabel(frInterp, text = 'Interpolation method', anchor = 'w', justify = 'left')
	cb.Interp <- ttkcombobox(frInterp, values = cb.InterpVAL, textvariable = interp.method, width = largeur1)
	frIDW <- ttklabelframe(frInterp, text = "Interpolation parameters", relief = 'groove')
	# frCoarse <- tkframe(frInterp)

	########
	min.nbrs.l <- tklabel(frIDW, text = 'nmin', anchor = 'e', justify = 'right')
	max.nbrs.l <- tklabel(frIDW, text = 'nmax', anchor = 'e', justify = 'right')
	max.dst.l <- tklabel(frIDW, text = 'maxdist', anchor = 'e', justify = 'right')
	min.nbrs.v <- tkentry(frIDW, width = 4, textvariable = nmin, justify = 'right')
	max.nbrs.v <- tkentry(frIDW, width = 4, textvariable = nmax, justify = 'right')
	max.dst.v <- tkentry(frIDW, width = 4, textvariable = maxdist, justify = 'right')

	########
	# res.coarse.l <- tklabel(frCoarse, text = 'Coarse Grid', anchor = 'e', justify = 'right')
	# res.coarse.v <- tkentry(frCoarse, width = 4, textvariable = res.coarse, justify = 'right')

	########

	tkgrid(min.nbrs.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.nbrs.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.nbrs.l, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.nbrs.v, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dst.l, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dst.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)

	########
	# tkgrid(res.coarse.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	# tkgrid(res.coarse.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)

	########

	tkgrid(txt.Interp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.Interp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frIDW, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
	# tkgrid(frCoarse, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	########

	infobulle(min.nbrs.v, 'Minimum number of neighbors to be used to interpolate data')
	status.bar.display(min.nbrs.v, TextOutputVar, 'Minimum number of neighbors to be used to interpolate data')
	infobulle(max.nbrs.v, 'Maximum number of neighbors to be used to interpolate data')
	status.bar.display(max.nbrs.v, TextOutputVar, 'Maximum number of neighbors to be used to interpolate data')
	infobulle(max.dst.v, 'Maximum distance (in  decimal degree) to be used to interpolate data')
	status.bar.display(max.dst.v, TextOutputVar, 'Maximum distance (in  decimal degree) to be used to interpolate data')

	# infobulle(frCoarse, 'Coarse resolution from gridded data to complete points data\nwhen interpolating (in decimal degree)')
	# status.bar.display(frCoarse, TextOutputVar, 'Coarse resolution from gridded data to complete points data\nwhen interpolating (in decimal degree)')

	############################################

	frMrgPars <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	min.stn <- tclVar(GeneralParameters$Mrg.set$min.stn)
	min.non.zero <- tclVar(GeneralParameters$Mrg.set$min.non.zero)
	# max.rnr.dst <- tclVar(GeneralParameters$Mrg.set$maxdist.RnoR)
	use.RnoR <- tclVar(GeneralParameters$Mrg.set$use.RnoR)
	smooth.RnoR <- tclVar(GeneralParameters$Mrg.set$smooth.RnoR)

	########
	txt.mrg.pars <- tklabel(frMrgPars, text = 'Merging parameters', anchor = 'w', justify = 'left')
	min.nbrs.stn.l <- tklabel(frMrgPars, text = 'Min.Nb.Stn', anchor = 'e', justify = 'right')
	min.non.zero.l <- tklabel(frMrgPars, text = 'Min.No.Zero', anchor = 'e', justify = 'right')
	# max.rnr.dst.l <- tklabel(frMrgPars, text = 'Max.RnR.Dist', anchor = 'e', justify = 'right')
	min.nbrs.stn.v <- tkentry(frMrgPars, width = 4, textvariable = min.stn, justify = 'right')
	min.non.zero.v <- tkentry(frMrgPars, width = 4, textvariable = min.non.zero, justify = 'right')
	# max.rnr.dst.v <- tkentry(frMrgPars, width = 4, textvariable = max.rnr.dst, justify = 'right')
	cb.RnoR <- tkcheckbutton(frMrgPars, variable = use.RnoR, text = 'Apply Rain-no-Rain mask', anchor = 'w', justify = 'left')
	cb.RnoRs <- tkcheckbutton(frMrgPars, variable = smooth.RnoR, text = 'Smooth Rain-no-Rain mask', anchor = 'w', justify = 'left')

	tkgrid(txt.mrg.pars, row = 0, column = 0, sticky = 'ew', rowspan = 1, columnspan = 4, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(min.nbrs.stn.l, row = 1, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(min.nbrs.stn.v, row = 1, column = 1, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(min.non.zero.l, row = 2, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(min.non.zero.v, row = 2, column = 1, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	# tkgrid(max.rnr.dst.l, row = 3, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	# tkgrid(max.rnr.dst.v, row = 3, column = 1, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.RnoR, row = 3, column = 0, sticky = 'ew', rowspan = 1, columnspan = 4, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.RnoRs, row = 4, column = 0, sticky = 'ew', rowspan = 1, columnspan = 4, padx = 1, pady = 0, ipadx = 1, ipady = 1)

	infobulle(min.nbrs.stn.v, 'Minimum number of gauges with data to be used to do the merging')
	status.bar.display(min.nbrs.stn.v, TextOutputVar, 'Minimum number of gauges with data to be used to do the merging')
	infobulle(min.non.zero.v, 'Minimum number of non-zero gauge values to perform the merging')
	status.bar.display(min.non.zero.v, TextOutputVar, 'Minimum number of non-zero gauge values to perform the merging')
	# infobulle(max.rnr.dst.v, 'Maximum distance (in decimal degrees) for interpolating Rain-noRain mask')
	# status.bar.display(max.rnr.dst.v, TextOutputVar, 'Maximum distance (in decimal degrees) for interpolating Rain-noRain mask')

	############################################
	tkgrid(frDate, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frInterp, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMrgPars, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################  RIGHT 1 #####################

	frSave <- tkframe(frRight1, relief = 'sunken', borderwidth = 2)

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

	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 2, ipadx = 1, ipady = 1)

	infobulle(en.file.save, 'Enter the full path to directory to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save result')
	infobulle(bt.file.save, 'or browse here')
	status.bar.display(bt.file.save, TextOutputVar, 'or browse here')

	############################################
	frOutmrg <- tkframe(frRight1, relief = 'sunken', borderwidth = 2)

	outmrgff <- tclVar(GeneralParameters$FileFormat$Mrg.file.format)

	txt.outmrgff <- tklabel(frOutmrg, text = 'Merged data filename format', anchor = 'w', justify = 'left')
	en.outmrgff <- tkentry(frOutmrg, textvariable = outmrgff, width = largeur)

	#####

	tkgrid(txt.outmrgff, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.outmrgff, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.outmrgff, 'Format of the merged data files names in NetCDF,\nexample: rr_mrg_1983012_ALL.nc')
	status.bar.display(en.outmrgff, TextOutputVar, 'Format of the merged data files names in NetCDF,\nexample: rr_mrg_1983012_ALL.nc')

	############################################
	frblank <- tkframe(frRight1, relief = 'sunken', borderwidth = 2)

	blankGrd <- tclVar()
	cb.blankVAL <- c("None", "Use DEM", "Use ESRI shapefile")
	tclvalue(blankGrd) <- switch(str_trim(GeneralParameters$Blank.Grid), 
									'1' = cb.blankVAL[1], 
									'2' = cb.blankVAL[2],
									'3' = cb.blankVAL[3])

	file.grddem <- tclVar(GeneralParameters$IO.files$DEM.file)
	file.blkshp <- tclVar(GeneralParameters$IO.files$SHP.file)

	isdemGrd <- str_trim(GeneralParameters$Blank.Grid) == '2' | (GeneralParameters$Create.Grid & str_trim(GeneralParameters$Grid.From) == '2')
	isdemAux <- GeneralParameters$auxvar$dem | GeneralParameters$auxvar$slope | GeneralParameters$auxvar$aspect 
	statedem <- if(isdemGrd | isdemAux) 'normal' else 'disabled'
	stateshp <- if(str_trim(GeneralParameters$Blank.Grid) == '3') 'normal' else 'disabled'

	txt.blankGrd <- tklabel(frblank, text = 'Blank merged data', anchor = 'w', justify = 'left')
	cb.blankGrd <- ttkcombobox(frblank, values = cb.blankVAL, textvariable = blankGrd, width = largeur1)
	txt.grddem <- tklabel(frblank, text = "Elevation data(NetCDF)", anchor = 'w', justify = 'left')
	cb.grddem <- ttkcombobox(frblank, values = unlist(listOpenFiles), textvariable = file.grddem, state = statedem, width = largeur1)
	bt.grddem <- tkbutton(frblank, text = "...", state = statedem)
	txt.blkshp <- tklabel(frblank, text = "ESRI shapefiles for blanking", anchor = 'w', justify = 'left')
	cb.blkshp <- ttkcombobox(frblank, values = unlist(listOpenFiles), textvariable = file.blkshp, state = stateshp, width = largeur1)
	bt.blkshp <- tkbutton(frblank, text = "...", state = stateshp)

	########
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
			tkconfigure(cb.grdrfe, values = unlist(listOpenFiles), textvariable = file.grdrfe)
			tkconfigure(cb.blkshp, values = unlist(listOpenFiles), textvariable = file.blkshp)
		}else return(NULL)
	})

	tkconfigure(bt.blkshp, command = function(){
		shp.opfiles <- getOpenShp(tt, all.opfiles)
		if(!is.null(shp.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'shp'
			AllOpenFilesData[[nopf+1]] <<- shp.opfiles
			tclvalue(file.blkshp) <- AllOpenFilesData[[nopf+1]][[1]]

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.blkshp) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
			tkconfigure(cb.grdrfe, values = unlist(listOpenFiles), textvariable = file.grdrfe)
			tkconfigure(cb.blkshp, values = unlist(listOpenFiles), textvariable = file.blkshp)
		}else return(NULL)
	})

	#####
	tkgrid(txt.blankGrd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.blankGrd, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.grddem, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.grddem, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.grddem, row = 3, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.blkshp, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.blkshp, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.blkshp, row = 5, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.blankGrd, 'Blank grid outside the country boundaries or over ocean')
	status.bar.display(cb.blankGrd, TextOutputVar,'Blank grid outside the country boundaries  or over ocean\ngiven by the DEM mask or the shapefile')
	infobulle(cb.grddem, 'Choose the file in the list')
	status.bar.display(cb.grddem, TextOutputVar, 'File containing the elevation data in netcdf')
	infobulle(bt.grddem, 'Browse file if not listed')
	status.bar.display(bt.grddem, TextOutputVar, 'Browse file if not listed')
	infobulle(cb.blkshp, 'Choose the file in the list')
	status.bar.display(cb.blkshp, TextOutputVar, 'Choose the file containing the ESRI shapefiles')
	infobulle(bt.blkshp, 'Browse file if not listed')
	status.bar.display(bt.blkshp, TextOutputVar, 'Browse file if not listed')

	############################################

	tkbind(cb.blankGrd, "<<ComboboxSelected>>", function(){
		if(tclvalue(blankGrd) == 'None'){
			tkconfigure(cb.blkshp, state = 'disabled')
			tkconfigure(bt.blkshp, state = 'disabled')
			auxvar <- tclvalue(dem.auxvar) == '1' | tclvalue(slope.auxvar) == '1' | tclvalue(aspect.auxvar) == '1'
			newgrd <- tclvalue(newGrd) == '1' & GeneralParameters$Grid.From == '2'
			statedem <- if(auxvar | newgrd) 'normal' else 'disabled'
			tkconfigure(cb.grddem, state = statedem)
			tkconfigure(bt.grddem, state = statedem)
		}
		if(tclvalue(blankGrd) == 'Use DEM'){
			tkconfigure(cb.blkshp, state = 'disabled')
			tkconfigure(bt.blkshp, state = 'disabled')
			tkconfigure(cb.grddem, state = 'normal')
			tkconfigure(bt.grddem, state = 'normal')
		}
		if(tclvalue(blankGrd) == 'Use ESRI shapefile'){
			tkconfigure(cb.blkshp, state = 'normal')
			tkconfigure(bt.blkshp, state = 'normal')
			auxvar <- tclvalue(dem.auxvar) == '1' | tclvalue(slope.auxvar) == '1' | tclvalue(aspect.auxvar) == '1'
			newgrd <- tclvalue(newGrd) == '1' & GeneralParameters$Grid.From == '2'
			statedem <- if(auxvar | newgrd) 'normal' else 'disabled'
			tkconfigure(cb.grddem, state = statedem)
			tkconfigure(bt.grddem, state = statedem)
		}
	})

	############################################

	frauxvar <- tkframe(frRight1, relief = 'sunken', borderwidth = 2)

	dem.auxvar <- tclVar(GeneralParameters$auxvar$dem)
	slope.auxvar <- tclVar(GeneralParameters$auxvar$slope)
	aspect.auxvar <- tclVar(GeneralParameters$auxvar$aspect)

	txt.auxvar <- tklabel(frauxvar, text = 'Include auxiliary variables', anchor = 'w', justify = 'left')
	dem.chk.auxvar <- tkcheckbutton(frauxvar, variable = dem.auxvar, text = 'DEM', anchor = 'w', justify = 'left')
	slope.chk.auxvar <- tkcheckbutton(frauxvar, variable = slope.auxvar, text = 'Slope', anchor = 'w', justify = 'left')
	aspect.chk.auxvar <- tkcheckbutton(frauxvar, variable = aspect.auxvar, text = 'Aspect', anchor = 'w', justify = 'left')

	tkgrid(txt.auxvar, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(dem.chk.auxvar, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(slope.chk.auxvar, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(aspect.chk.auxvar, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(dem.chk.auxvar, 'Include elevation data as auxiliary variable')
	status.bar.display(dem.chk.auxvar, TextOutputVar, 'Include elevation data as auxiliary variable')
	infobulle(slope.chk.auxvar, 'Include slope data as auxiliary variable')
	status.bar.display(slope.chk.auxvar, TextOutputVar, 'Include slope data as auxiliary variable')
	infobulle(aspect.chk.auxvar, 'Include aspect data as auxiliary variable')
	status.bar.display(aspect.chk.auxvar, TextOutputVar, 'Include aspect data as auxiliary variable')

	############################################

	tkbind(dem.chk.auxvar, "<Button-1>", function(){
		auxvar <- tclvalue(dem.auxvar) == '0' | tclvalue(slope.auxvar) == '1' | tclvalue(aspect.auxvar) == '1'
		newgrd <- tclvalue(newGrd) == '1' & GeneralParameters$Grid.From == '2'
		blank <- tclvalue(blankGrd) == 'Use DEM'
		statedem <- if(auxvar | newgrd | blank) 'normal' else 'disabled'
		tkconfigure(cb.grddem, state = statedem)
		tkconfigure(bt.grddem, state = statedem)
	})

	tkbind(slope.chk.auxvar, "<Button-1>", function(){
		auxvar <- tclvalue(dem.auxvar) == '1' | tclvalue(slope.auxvar) == '0' | tclvalue(aspect.auxvar) == '1'
		newgrd <- tclvalue(newGrd) == '1' & GeneralParameters$Grid.From == '2'
		blank <- tclvalue(blankGrd) == 'Use DEM'
		statedem <- if(auxvar | newgrd | blank) 'normal' else 'disabled'
		tkconfigure(cb.grddem, state = statedem)
		tkconfigure(bt.grddem, state = statedem)
	})

	tkbind(aspect.chk.auxvar, "<Button-1>", function(){
		auxvar <- tclvalue(dem.auxvar) == '1' | tclvalue(slope.auxvar) == '1' | tclvalue(aspect.auxvar) == '0'
		newgrd <- tclvalue(newGrd) == '1' & GeneralParameters$Grid.From == '2'
		blank <- tclvalue(blankGrd) == 'Use DEM'
		statedem <- if(auxvar | newgrd | blank) 'normal' else 'disabled'
		tkconfigure(cb.grddem, state = statedem)
		tkconfigure(bt.grddem, state = statedem)
	})

	############################################

	frscaleday <- tkframe(frRight1, relief = 'sunken', borderwidth = 2)

	scale.day <- tclVar(GeneralParameters$Scale.daily)
	statescale.chk <- if(GeneralParameters$period == 'daily') 'normal' else 'disabled'
	statescale.bt <- if(GeneralParameters$period == 'daily' & GeneralParameters$Scale.daily) 'normal' else 'disabled'

	chk.scale.day <- tkcheckbutton(frscaleday, variable = scale.day, text = 'Scale daily data', anchor = 'w', justify = 'left', state = statescale.chk)
	bt.scale.day <- ttkbutton(frscaleday, text = 'Dekadal data', state = statescale.bt)

	tkconfigure(bt.scale.day, command = function(){
		GeneralParameters <<- getDekadalData2ScaleDaily(tt, GeneralParameters)
	})

	tkgrid(chk.scale.day, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.scale.day, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(frscaleday, 'Scaling up daily merged data to match dekadal totals\nfrom dekadal merged data')
	status.bar.display(frscaleday, TextOutputVar, 'Scaling up daily merged data to match dekadal totals from dekadally merged data')

	############################################

	tkbind(chk.scale.day, "<Button-1>", function(){
		statescale.bt <- if(tclvalue(scale.day) == '0') 'normal' else 'disabled'
		tkconfigure(bt.scale.day, state = statescale.bt)
	})

	############################################
	tkgrid(frSave, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frOutmrg, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frblank, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frauxvar, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frscaleday, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################
	
	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight1, row = 0, column = 2, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	#######

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(file.stnfl)) == ""){
			tkmessageBox(message = "Select the file containing the gauge data", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(dir.rfe)) == "" | str_trim(tclvalue(dir.rfe)) == "NA"){
			tkmessageBox(message = "Choose or enter the  directory containing the RFE files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.grdrfe)) == ""){
			tkmessageBox(message = "You have to provide a sample file", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.grddem)) == "" & 
			((tclvalue(dem.auxvar) == '1' | tclvalue(slope.auxvar) == '1' | tclvalue(aspect.auxvar) == '1') | 
			(tclvalue(newGrd) == '1' & GeneralParameters$Grid.From == '2') | 
			str_trim(tclvalue(blankGrd)) == "Use DEM")){
			tkmessageBox(message = "You have to provide DEM data in NetCDF format", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.blkshp)) == "" & str_trim(tclvalue(blankGrd)) == "Use ESRI shapefile"){
			tkmessageBox(message = "You have to provide the shapefile", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(mrg.method))  ==  "Spatio-Temporal LM" & 
			(str_trim(tclvalue(dir.LMCoef)) == "" | str_trim(tclvalue(dir.LMCoef)) == "NA")){
			tkmessageBox(message = "Enter the path to directory containing the LM Coefficients", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.save1)) == "" | str_trim(tclvalue(file.save1)) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			# months <- strsplit(str_trim(tclvalue(use.months)), ' ')[[1]]
			# months <- months[months != ""]
			# if(length(months) == 0){
			# 	tkmessageBox(message = "Need at least one month", icon = "warning", type = "ok")
			# 	tkwait.window(tt)
			# }else{
				GeneralParameters$Mrg.Method <<- str_trim(tclvalue(mrg.method))
				GeneralParameters$IO.files$STN.file <<- str_trim(tclvalue(file.stnfl))
				GeneralParameters$IO.files$RFE.dir <<- str_trim(tclvalue(dir.rfe))
				GeneralParameters$IO.files$RFE.file <<- str_trim(tclvalue(file.grdrfe))
				GeneralParameters$FileFormat$RFE.File.Format <<- str_trim(tclvalue(inrfeff))
				GeneralParameters$Create.Grid <<- switch(tclvalue(newGrd), '0' = FALSE, '1' = TRUE)
				GeneralParameters$IO.files$LMCoef.dir <<- str_trim(tclvalue(dir.LMCoef))
				GeneralParameters$IO.files$dir2save <<- str_trim(tclvalue(file.save1))
				GeneralParameters$IO.files$DEM.file <<- str_trim(tclvalue(file.grddem))
				GeneralParameters$IO.files$SHP.file <<- str_trim(tclvalue(file.blkshp))
				GeneralParameters$period <<- switch(str_trim(tclvalue(file.period)), 
													'Daily data' = 'daily',
													'Dekadal data' =  'dekadal',
													'Monthly data' = 'monthly')
				GeneralParameters$Mrg.Date.Range$start.year <<- as.numeric(str_trim(tclvalue(istart.yrs)))
				GeneralParameters$Mrg.Date.Range$start.mon <<- as.numeric(str_trim(tclvalue(istart.mon)))
				GeneralParameters$Mrg.Date.Range$start.dek <<- as.numeric(str_trim(tclvalue(istart.day)))
				GeneralParameters$Mrg.Date.Range$end.year <<- as.numeric(str_trim(tclvalue(iend.yrs)))
				GeneralParameters$Mrg.Date.Range$end.mon <<- as.numeric(str_trim(tclvalue(iend.mon)))
				GeneralParameters$Mrg.Date.Range$end.dek <<- as.numeric(str_trim(tclvalue(iend.day)))
				# GeneralParameters$Mrg.Months <<- sort(as.numeric(months))

				GeneralParameters$Interpolation.pars$interp.method <<- switch(str_trim(tclvalue(interp.method)),
																			'Inverse Distance Weighted' = 'IDW',
																			'Ordinary Kriging' = 'Kriging')
				GeneralParameters$Interpolation.pars$nmin <<- as.numeric(str_trim(tclvalue(nmin)))
				GeneralParameters$Interpolation.pars$nmax <<- as.numeric(str_trim(tclvalue(nmax)))
				GeneralParameters$Interpolation.pars$maxdist <<- as.numeric(str_trim(tclvalue(maxdist)))
				# GeneralParameters$Interpolation.pars$res.coarse <<- as.numeric(str_trim(tclvalue(res.coarse)))
				# GeneralParameters$Interpolation.pars$vgm.model[[1]] <<- 

				GeneralParameters$Mrg.set$min.stn <<- as.numeric(str_trim(tclvalue(min.stn)))
				GeneralParameters$Mrg.set$min.non.zero <<- as.numeric(str_trim(tclvalue(min.non.zero)))
				# GeneralParameters$Mrg.set$maxdist.RnoR <<- as.numeric(str_trim(tclvalue(max.rnr.dst)))
				GeneralParameters$Mrg.set$use.RnoR <<- switch(tclvalue(use.RnoR), '0' = FALSE, '1' = TRUE)
				GeneralParameters$Mrg.set$smooth.RnoR <<- switch(tclvalue(smooth.RnoR), '0' = FALSE, '1' = TRUE)

				GeneralParameters$FileFormat$Mrg.file.format <<- str_trim(tclvalue(outmrgff))
				GeneralParameters$Blank.Grid <<- switch(str_trim(tclvalue(blankGrd)),
														"None" = '1', "Use DEM" = '2',
														"Use ESRI shapefile" = '3')
				GeneralParameters$auxvar$dem <<- switch(tclvalue(dem.auxvar), '0' = FALSE, '1' = TRUE)
				GeneralParameters$auxvar$slope <<- switch(tclvalue(slope.auxvar), '0' = FALSE, '1' = TRUE)
				GeneralParameters$auxvar$aspect <<- switch(tclvalue(aspect.auxvar), '0' = FALSE, '1' = TRUE)
				GeneralParameters$Scale.daily <<- switch(tclvalue(scale.day), '0' = FALSE, '1' = TRUE)

				tkgrab.release(tt)
				tkdestroy(tt)
				tkfocus(parent.win)
			# }
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
	tkwm.title(tt, 'Merging data - Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

#############################################################################################

coefLMGetInfoRain <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur <- 28
		largeur1 <- 25
	}else{
		largeur <- 25
		largeur1 <- 24
	}

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	frRight <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

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
			tkconfigure(cb.grdrfe, values = unlist(listOpenFiles), textvariable = file.grdrfe)
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

	frRFE <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	dir.rfe <- tclVar(GeneralParameters$IO.files$RFE.dir)
	file.grdrfe <- tclVar(GeneralParameters$IO.files$RFE.file)
	inrfeff <- tclVar(GeneralParameters$Prefix$RFE.File.Format)

	txt.dir.rfe <- tklabel(frRFE, text = 'Directory of RFE or ADJ-RFE files', anchor = 'w', justify = 'left')
	en.dir.rfe <- tkentry(frRFE, textvariable = dir.rfe, width = largeur)
	bt.dir.rfe <- tkbutton(frRFE, text = "...")
	txt.grdrfe <- tklabel(frRFE, text = "RFE or ADJ-RFE sample file", anchor = 'w', justify = 'left')
	cb.grdrfe <- ttkcombobox(frRFE, values = unlist(listOpenFiles), textvariable = file.grdrfe, width = largeur1)
	bt.grdrfe <- tkbutton(frRFE, text = "...")
	txt.inrfeff <- tklabel(frRFE, text = 'Input RFE or ADJ-RFE file format', anchor = 'w', justify = 'left')
	en.inrfeff <- tkentry(frRFE, textvariable = inrfeff, width = largeur)

	######
	tkconfigure(bt.dir.rfe, command = function(){
		dir4rfe <- tk_choose.dir(GeneralParameters$IO.files$RFE.dir, "")
		tclvalue(dir.rfe) <- if(!is.na(dir4rfe)) dir4rfe else ""
	})

	tkconfigure(bt.grdrfe, command = function(){
		nc.opfiles <- getOpenNetcdf(tt, all.opfiles, initialdir = tclvalue(dir.rfe))
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grdrfe) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.grdrfe, values = unlist(listOpenFiles), textvariable = file.grdrfe)
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
		}else return(NULL)
	})
	######

	tkgrid(txt.dir.rfe, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.dir.rfe, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.rfe, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.grdrfe, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.grdrfe, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.grdrfe, row = 3, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.inrfeff, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.inrfeff, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.dir.rfe, 'Enter the full path to directory containing\nthe RFE or Adjusted RFE files')
	status.bar.display(en.dir.rfe, TextOutputVar, 'Enter the full path to directory containing\nthe RFE or Adjusted RFE files')
	infobulle(bt.dir.rfe, 'or browse here')
	status.bar.display(bt.dir.rfe, TextOutputVar, 'or browse here')
	infobulle(cb.grdrfe, 'Choose the file in the list')
	status.bar.display(cb.grdrfe, TextOutputVar, 'File containing a sample of RFE or Adjusted RFE data in netcdf')
	infobulle(bt.grdrfe, 'Browse file if not listed')
	status.bar.display(bt.grdrfe, TextOutputVar, 'Browse file if not listed')
	infobulle(en.inrfeff, 'Enter the format of the RFE or Adjusted RFE files names in NetCDF,\nexample: rfe1983_01-dk2.nc or rr_adj_1983012.nc')
	status.bar.display(en.inrfeff, TextOutputVar, 'Enter the format of the RFE or Adjusted RFE files names in NetCDF,\nexample: rfe1983_01-dk2.nc or rr_adj_1983012.nc')

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
			tkconfigure(cb.grdrfe, values = unlist(listOpenFiles), textvariable = file.grdrfe)
		}else return(NULL)
	})

	#####
	tkgrid(txt.grddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.grddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.grddem, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.grddem, 'Choose the file in the list')
	status.bar.display(cb.grddem, TextOutputVar, 'File containing the elevation data in netcdf')
	infobulle(bt.grddem, 'Browse file if not listed')
	status.bar.display(bt.grddem, TextOutputVar, 'Browse file if not listed')


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

	frauxvar <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	dem.auxvar <- tclVar(GeneralParameters$auxvar$dem)
	slope.auxvar <- tclVar(GeneralParameters$auxvar$slope)
	aspect.auxvar <- tclVar(GeneralParameters$auxvar$aspect)

	txt.auxvar <- tklabel(frauxvar, text = 'Include auxiliary variables', anchor = 'w', justify = 'left')
	dem.chk.auxvar <- tkcheckbutton(frauxvar, variable = dem.auxvar, text = 'DEM', anchor = 'w', justify = 'left')
	slope.chk.auxvar <- tkcheckbutton(frauxvar, variable = slope.auxvar, text = 'Slope', anchor = 'w', justify = 'left')
	aspect.chk.auxvar <- tkcheckbutton(frauxvar, variable = aspect.auxvar, text = 'Aspect', anchor = 'w', justify = 'left')

	tkgrid(txt.auxvar, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(dem.chk.auxvar, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(slope.chk.auxvar, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(aspect.chk.auxvar, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(dem.chk.auxvar, 'Include elevation data as auxiliary variable')
	status.bar.display(dem.chk.auxvar, TextOutputVar, 'Include elevation data as auxiliary variable')
	infobulle(slope.chk.auxvar, 'Include slope data as auxiliary variable')
	status.bar.display(slope.chk.auxvar, TextOutputVar, 'Include slope data as auxiliary variable')
	infobulle(aspect.chk.auxvar, 'Include aspect data as auxiliary variable')
	status.bar.display(aspect.chk.auxvar, TextOutputVar, 'Include aspect data as auxiliary variable')

	############################################
	tkgrid(frSTN, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRFE, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frDEM, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frauxvar, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################  RIGHT   #####################

	frDate <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(GeneralParameters$period, 
									'daily' = cb.periodVAL[1], 
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3])
	year1 <- tclVar(GeneralParameters$LM.Date.Range$start.year)
	year2 <- tclVar(GeneralParameters$LM.Date.Range$end.year)
	# use.months <- tclVar(paste(GeneralParameters$LM.Months, collapse=' '))

	cb.period <- ttkcombobox(frDate, values = cb.periodVAL, textvariable = file.period, width = largeur1)
	frtxtDate <- ttklabelframe(frDate, text = "Year Range", relief = 'groove')

	years1.l <- tklabel(frtxtDate, text = 'Start', anchor = 'e', justify = 'right')
	years2.l <- tklabel(frtxtDate, text = 'End', anchor = 'e', justify = 'right')
	years1.v <- tkentry(frtxtDate, width = 6, textvariable = year1, justify = 'right')
	years2.v <- tkentry(frtxtDate, width = 6, textvariable = year2, justify = 'right')

	# txt.months <- tklabel(frDate, text = 'Months', anchor = 'w', justify = 'left')
	# en.months <- tkentry(frDate, width = 20, textvariable = use.months, justify = "left")

	tkgrid(years1.l, row = 0, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(years1.v, row = 0, column = 1, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(years2.l, row = 0, column = 2, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(years2.v, row = 0, column = 3, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frtxtDate, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
	# tkgrid(txt.months, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	# tkgrid(en.months, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.period, 'Choose the time step of the data')
	status.bar.display(cb.period, TextOutputVar, 'Choose the time step of the data')
	infobulle(years1.v, 'Start year to be used to compute LM coefficients')
	status.bar.display(years1.v, TextOutputVar, 'Start year to be used to compute LM coefficients')
	infobulle(years2.v, 'End year to be used to compute LM coefficients')
	status.bar.display(years2.v, TextOutputVar, 'End year to be used to compute LM coefficients')
	# infobulle(en.months, 'Months to compute bias')
	# status.bar.display(en.months, TextOutputVar, 'Months to compute bias')

	############################################

	frGrid <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	varCreateGrd <- tclVar(GeneralParameters$Create.Grid)
	stategrd <- if(str_trim(GeneralParameters$Create.Grid) == '3') 'normal' else 'disabled'

	txt.CreateGrd <- tklabel(frGrid, text = 'Create grid for interpolation', anchor = 'w', justify = 'left')
	grdRFE.rbt <- tkradiobutton(frGrid, text = "From RFE", anchor = 'w', justify = 'left')
	grdDEM.rbt <- tkradiobutton(frGrid, text = "From DEM", anchor = 'w', justify = 'left')
	grdNEW.rbt <- tkradiobutton(frGrid, text = "New Grid", anchor = 'w', justify = 'left')
	bt.getNewgrid <- tkbutton(frGrid, text = "Create", state = stategrd)

	####
	tkconfigure(grdRFE.rbt, variable = varCreateGrd, value = "1")
	tkconfigure(grdDEM.rbt, variable = varCreateGrd, value = "2")
	tkconfigure(grdNEW.rbt, variable = varCreateGrd, value = "3")

	tkconfigure(bt.getNewgrid, command = function(){
		GeneralParameters <<- getParamNewGrid(tt, GeneralParameters)
	})
	#####

	tkgrid(txt.CreateGrd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grdRFE.rbt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grdDEM.rbt, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grdNEW.rbt, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.getNewgrid, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(bt.getNewgrid, 'Set the new grid')
	status.bar.display(bt.getNewgrid, TextOutputVar, 'Set the new grid')
	infobulle(frGrid, 'Create the grid to interpolate the LM coef')
	status.bar.display(frGrid, TextOutputVar, 'Create the grid to interpolate the LM coef')

	###########
	tkbind(grdRFE.rbt, "<Button-1>", function(){
		tkconfigure(bt.getNewgrid, state = 'disabled')
	})
	tkbind(grdDEM.rbt, "<Button-1>", function(){
		tkconfigure(bt.getNewgrid, state = 'disabled')
	})
	tkbind(grdNEW.rbt, "<Button-1>", function(){
		tkconfigure(bt.getNewgrid, state = 'normal')
	})

	############################################

	frInterp <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	interp.method <- tclVar()
	cb.InterpVAL <- c('Nearest Neighbor', 'Inverse Distance Weighted', 'Kriging')
	tclvalue(interp.method) <- switch(GeneralParameters$Interpolation.pars$interp.method, 
										'NN' = cb.InterpVAL[1], 
										'IDW' = cb.InterpVAL[2],
										'Kriging' = cb.InterpVAL[3])
	nmin <- tclVar(GeneralParameters$Interpolation.pars$nmin)
	nmax <- tclVar(GeneralParameters$Interpolation.pars$nmax)
	maxdist <- tclVar(GeneralParameters$Interpolation.pars$maxdist)
	rad.lon <- tclVar(GeneralParameters$Interpolation.pars$rad.lon)
	rad.lat <- tclVar(GeneralParameters$Interpolation.pars$rad.lat)
	rad.elv <- tclVar(GeneralParameters$Interpolation.pars$rad.elv)
	res.coarse <- tclVar(GeneralParameters$Interpolation.pars$res.coarse)

	if(GeneralParameters$Interpolation.pars$interp.method == 'NN'){
		statenn <- 'normal'
		stateidw <- 'disabled'
	}else{
		statenn <- 'disabled'
		stateidw <- 'normal'
	}

	txt.Interp <- tklabel(frInterp, text = 'Interpolation method', anchor = 'w', justify = 'left')
	cb.Interp <- ttkcombobox(frInterp, values = cb.InterpVAL, textvariable = interp.method, width = largeur1)
	frNN <- ttklabelframe(frInterp, text = "Nearest Neighbor", relief = 'groove')
	frIDW <- ttklabelframe(frInterp, text = "Inverse Distance & Kriging", relief = 'groove')
	# frCoarse <- tkframe(frInterp)

	#######
	mul.lon.l <- tklabel(frNN, text = 'Multi.Lon', anchor = 'e', justify = 'right')
	mul.lat.l <- tklabel(frNN, text = 'Multi.Lat', anchor = 'e', justify = 'right')
	mul.elv.l <- tklabel(frNN, text = 'Multi.Elv', anchor = 'e', justify = 'right')
	mul.lon.v <- tkentry(frNN, width = 4, textvariable = rad.lon, justify = 'right', state = statenn)
	mul.lat.v <- tkentry(frNN, width = 4, textvariable = rad.lat, justify = 'right', state = statenn)
	mul.elv.v <- tkentry(frNN, width = 4, textvariable = rad.elv, justify = 'right', state = statenn)

	########
	min.nbrs.l <- tklabel(frIDW, text = 'nmin', anchor = 'e', justify = 'right')
	max.nbrs.l <- tklabel(frIDW, text = 'nmax', anchor = 'e', justify = 'right')
	max.dst.l <- tklabel(frIDW, text = 'maxdist', anchor = 'e', justify = 'right')
	min.nbrs.v <- tkentry(frIDW, width = 4, textvariable = nmin, justify = 'right', state = stateidw)
	max.nbrs.v <- tkentry(frIDW, width = 4, textvariable = nmax, justify = 'right', state = stateidw)
	max.dst.v <- tkentry(frIDW, width = 4, textvariable = maxdist, justify = 'right', state = stateidw)

	########
	# res.coarse.l <- tklabel(frCoarse, text = 'Coarse Grid', anchor = 'e', justify = 'right')
	# res.coarse.v <- tkentry(frCoarse, width = 4, textvariable = res.coarse, justify = 'right')

	########

	tkgrid(mul.lon.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mul.lon.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mul.lat.l, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mul.lat.v, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mul.elv.l, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mul.elv.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)

	########

	tkgrid(min.nbrs.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.nbrs.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.nbrs.l, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.nbrs.v, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dst.l, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dst.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)

	########
	# tkgrid(res.coarse.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	# tkgrid(res.coarse.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)

	########

	tkgrid(txt.Interp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.Interp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frNN, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(frIDW, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
	# tkgrid(frCoarse, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	########

	infobulle(mul.lon.v, 'Maximum distance belong longitude, n times of grid interpolation resolution')
	status.bar.display(mul.lon.v, TextOutputVar, 'Maximum distance belong longitude, n times of grid interpolation resolution')
	infobulle(mul.lat.v, 'Maximum distance belong latitude, n times of grid interpolation resolution')
	status.bar.display(mul.lat.v, TextOutputVar, 'Maximum distance belong latitude, n times of grid interpolation resolution')
	infobulle(mul.elv.v, 'Maximum height for elevation, n times of elevation resolution\n(elevation is discretized by 100 m)')
	status.bar.display(mul.elv.v, TextOutputVar, 'Maximum height for elevation, n times of elevation resolution\n(elevation is discretized by 100 m)')

	infobulle(min.nbrs.v, 'Minimum number of neighbours to be used to interpolate the LM coef')
	status.bar.display(min.nbrs.v, TextOutputVar, 'Minimum number of neighbours to be used to interpolate the LM coef')
	infobulle(max.nbrs.v, 'Maximum number of neighbours to be used to interpolate the LM coef')
	status.bar.display(max.nbrs.v, TextOutputVar, 'Maximum number of neighbours to be used to interpolate the LM coef')
	infobulle(max.dst.v, 'Maximum distance (in  decimal degree) to be used to interpolate the LM coef')
	status.bar.display(max.dst.v, TextOutputVar, 'Maximum distance (in  decimal degree) to be used to interpolate the LM coef')

	# infobulle(frCoarse, 'Coarse resolution from gridded data to complete points data\nwhen interpolating (in decimal degree)')
	# status.bar.display(frCoarse, TextOutputVar, 'Coarse resolution from gridded data to complete points data\nwhen interpolating (in decimal degree)')

	tkbind(cb.Interp, "<<ComboboxSelected>>", function(){
		if(tclvalue(interp.method) == 'Nearest Neighbor'){
			tkconfigure(mul.lon.v, state = 'normal')
			tkconfigure(mul.lat.v, state = 'normal')
			tkconfigure(mul.elv.v, state = 'normal')
			tkconfigure(min.nbrs.v, state = 'disabled')
			tkconfigure(max.nbrs.v, state = 'disabled')
			tkconfigure(max.dst.v, state = 'disabled')
		}else{
			tkconfigure(mul.lon.v, state = 'disabled')
			tkconfigure(mul.lat.v, state = 'disabled')
			tkconfigure(mul.elv.v, state = 'disabled')
			tkconfigure(min.nbrs.v, state = 'normal')
			tkconfigure(max.nbrs.v, state = 'normal')
			tkconfigure(max.dst.v, state = 'normal')
		}
	})

	############################################

	tkgrid(frDate, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frGrid, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frInterp, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################
	
	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	#######

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(file.stnfl)) == ""){
			tkmessageBox(message = "Select the file containing the gauge data", icon = "warning", type = "ok")
			#tkwait.window(tt)
		}else if(str_trim(tclvalue(dir.rfe)) == "" | str_trim(tclvalue(dir.rfe)) == "NA"){
			tkmessageBox(message = "Choose or enter the  directory containing the RFE files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.grdrfe)) == ""){
			tkmessageBox(message = "You have to provide a sample file", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.grddem)) == "" ){
			tkmessageBox(message = "You have to provide DEM data in NetCDF format", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.save1)) == "" | str_trim(tclvalue(file.save1)) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			# months <- strsplit(str_trim(tclvalue(use.months)), ' ')[[1]]
			# months <- months[months != ""]
			# if(length(months) == 0){
			# 	tkmessageBox(message = "Need at least one month", icon = "warning", type = "ok")
			# 	tkwait.window(tt)
			# }else{
				GeneralParameters$period <<- switch(str_trim(tclvalue(file.period)), 
				 									'Daily data' = 'daily',
													'Dekadal data' =  'dekadal',
													'Monthly data' = 'monthly')
				GeneralParameters$LM.Date.Range$start.year <<- as.numeric(str_trim(tclvalue(year1)))
				GeneralParameters$LM.Date.Range$end.year <<- as.numeric(str_trim(tclvalue(year2)))
				# GeneralParameters$LM.Months <<- sort(as.numeric(months))

				GeneralParameters$IO.files$STN.file <<- str_trim(tclvalue(file.stnfl))
				GeneralParameters$IO.files$RFE.dir <<- str_trim(tclvalue(dir.rfe))
				GeneralParameters$IO.files$RFE.file <<- str_trim(tclvalue(file.grdrfe))
				GeneralParameters$IO.files$DEM.file <<- str_trim(tclvalue(file.grddem))
				GeneralParameters$IO.files$dir2save <<- str_trim(tclvalue(file.save1))
				GeneralParameters$Prefix$RFE.File.Format <<- str_trim(tclvalue(inrfeff))

				GeneralParameters$Interpolation.pars$interp.method <<- switch(str_trim(tclvalue(interp.method)),
									 									'Nearest Neighbor' = 'NN',
																		'Inverse Distance Weighted' = 'IDW',
																		'Kriging' = 'Kriging')
				GeneralParameters$Interpolation.pars$rad.lon <<- as.numeric(str_trim(tclvalue(rad.lon)))
				GeneralParameters$Interpolation.pars$rad.lat <<- as.numeric(str_trim(tclvalue(rad.lat)))
				GeneralParameters$Interpolation.pars$rad.elv <<- as.numeric(str_trim(tclvalue(rad.elv)))
				# GeneralParameters$Interpolation.pars$res.coarse <<- as.numeric(str_trim(tclvalue(res.coarse)))
				GeneralParameters$Interpolation.pars$nmin <<- as.numeric(str_trim(tclvalue(nmin)))
				GeneralParameters$Interpolation.pars$nmax <<- as.numeric(str_trim(tclvalue(nmax)))
				GeneralParameters$Interpolation.pars$maxdist <<- as.numeric(str_trim(tclvalue(maxdist)))
				GeneralParameters$Create.Grid <<- str_trim(tclvalue(varCreateGrd))

				GeneralParameters$auxvar$dem <<- switch(tclvalue(dem.auxvar), '0' = FALSE, '1' = TRUE)
				GeneralParameters$auxvar$slope <<- switch(tclvalue(slope.auxvar), '0' = FALSE, '1' = TRUE)
				GeneralParameters$auxvar$aspect <<- switch(tclvalue(aspect.auxvar), '0' = FALSE, '1' = TRUE)

				tkgrab.release(tt)
				tkdestroy(tt)
				tkfocus(parent.win)
			# }
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

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Spatio-temporal Trend coef - Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

#############################################################################################

coefBiasGetInfoRain <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur <- 28
		largeur1 <- 25
	}else{
		largeur <- 25
		largeur1 <- 24
	}

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	frRight <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################

	frBias <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	bias.method <- tclVar(str_trim(GeneralParameters$Bias.Method))
	cb.biasMthd <- c("Quantile.Mapping", "Multiplicative.Bias.Var", "Multiplicative.Bias.Mon")

	txt.bias <- tklabel(frBias, text = 'Bias method', anchor = 'w', justify = 'left')
	cb.bias <- ttkcombobox(frBias, values = cb.biasMthd, textvariable = bias.method, width = largeur1)

	tkgrid(txt.bias, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.bias, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.bias, 'Method to be used to calculate Bias Factors or Parameters')
	status.bar.display(cb.bias, TextOutputVar, 'Method to be used to calculate Bias Factors or Parameters')

	######

	tkbind(cb.bias, "<<ComboboxSelected>>", function(){
		stateBSM <- if(tclvalue(bias.method) == "Quantile.Mapping") 'disabled' else 'normal'
		tkconfigure(en.outbiasff, state = stateBSM)
	})

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
			tkconfigure(cb.grdrfe, values = unlist(listOpenFiles), textvariable = file.grdrfe)
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

	frRFE <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	dir.rfe <- tclVar(GeneralParameters$IO.files$RFE.dir)
	file.grdrfe <- tclVar(GeneralParameters$IO.files$RFE.file)
	inrfeff <- tclVar(GeneralParameters$Prefix$RFE.File.Format)

	txt.dir.rfe <- tklabel(frRFE, text = 'Directory of RFE files', anchor = 'w', justify = 'left')
	en.dir.rfe <- tkentry(frRFE, textvariable = dir.rfe, width = largeur)
	bt.dir.rfe <- tkbutton(frRFE, text = "...")
	txt.grdrfe <- tklabel(frRFE, text = "RFE's sample file", anchor = 'w', justify = 'left')
	cb.grdrfe <- ttkcombobox(frRFE, values = unlist(listOpenFiles), textvariable = file.grdrfe, width = largeur1)
	bt.grdrfe <- tkbutton(frRFE, text = "...")
	txt.inrfeff <- tklabel(frRFE, text = 'Input RFE file format', anchor = 'w', justify = 'left')
	en.inrfeff <- tkentry(frRFE, textvariable = inrfeff, width = largeur)

	######
	tkconfigure(bt.dir.rfe, command = function(){
		dir4rfe <- tk_choose.dir(GeneralParameters$IO.files$RFE.dir, "")
		tclvalue(dir.rfe) <- if(!is.na(dir4rfe)) dir4rfe else ""
	})

	tkconfigure(bt.grdrfe, command = function(){
		nc.opfiles <- getOpenNetcdf(tt, all.opfiles, initialdir = tclvalue(dir.rfe))
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grdrfe) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.grdrfe, values = unlist(listOpenFiles), textvariable = file.grdrfe)
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
		}else return(NULL)
	})
	######

	tkgrid(txt.dir.rfe, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.dir.rfe, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.rfe, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.grdrfe, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.grdrfe, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.grdrfe, row = 3, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.inrfeff, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.inrfeff, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.dir.rfe, 'Enter the full path to directory containing the RFE files')
	status.bar.display(en.dir.rfe, TextOutputVar, 'Enter the full path to directory containing the RFE files')
	infobulle(bt.dir.rfe, 'or browse here')
	status.bar.display(bt.dir.rfe, TextOutputVar, 'or browse here')
	infobulle(cb.grdrfe, 'Choose the file in the list')
	status.bar.display(cb.grdrfe, TextOutputVar, 'File containing a sample of RFE data in netcdf')
	infobulle(bt.grdrfe, 'Browse file if not listed')
	status.bar.display(bt.grdrfe, TextOutputVar, 'Browse file if not listed')
	infobulle(en.inrfeff, 'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01-dk2.nc')
	status.bar.display(en.inrfeff, TextOutputVar, 'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01-dk2.nc')

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
			tkconfigure(cb.grdrfe, values = unlist(listOpenFiles), textvariable = file.grdrfe)
		}else return(NULL)
	})

	#####
	tkgrid(txt.grddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.grddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.grddem, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.grddem, 'Choose the file in the list')
	status.bar.display(cb.grddem, TextOutputVar, 'File containing the elevation data in netcdf')
	infobulle(bt.grddem, 'Browse file if not listed')
	status.bar.display(bt.grddem, TextOutputVar, 'Browse file if not listed')

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

	# frPfxBias <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	# outbiasff <- tclVar(GeneralParameters$Prefix$Mean.Bias.Prefix)
	# if(str_trim(GeneralParameters$Bias.Method) == "Quantile.Mapping") statePfxBias <- 'disabled'
	# else statePfxBias <- 'normal'

	# txt.outbiasff <- tklabel(frPfxBias, text = 'Mean bias filename prefix', anchor = 'w', justify = 'left')
	# en.outbiasff <- tkentry(frPfxBias, textvariable = outbiasff, width = largeur, state = statePfxBias)

	# tkgrid(txt.outbiasff, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	# tkgrid(en.outbiasff, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	# infobulle(en.outbiasff, 'Prefix for the file name of the mean bias coefficient')
	# status.bar.display(en.outbiasff, TextOutputVar, 'Prefix for the file name of the mean bias coefficient')

	############################################

	frauxvar <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	dem.auxvar <- tclVar(GeneralParameters$auxvar$dem)
	slope.auxvar <- tclVar(GeneralParameters$auxvar$slope)
	aspect.auxvar <- tclVar(GeneralParameters$auxvar$aspect)

	txt.auxvar <- tklabel(frauxvar, text = 'Include auxiliary variables', anchor = 'w', justify = 'left')
	dem.chk.auxvar <- tkcheckbutton(frauxvar, variable = dem.auxvar, text = 'DEM', anchor = 'w', justify = 'left')
	slope.chk.auxvar <- tkcheckbutton(frauxvar, variable = slope.auxvar, text = 'Slope', anchor = 'w', justify = 'left')
	aspect.chk.auxvar <- tkcheckbutton(frauxvar, variable = aspect.auxvar, text = 'Aspect', anchor = 'w', justify = 'left')

	tkgrid(txt.auxvar, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(dem.chk.auxvar, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(slope.chk.auxvar, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(aspect.chk.auxvar, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(dem.chk.auxvar, 'Include elevation data as auxiliary variable')
	status.bar.display(dem.chk.auxvar, TextOutputVar, 'Include elevation data as auxiliary variable')
	infobulle(slope.chk.auxvar, 'Include slope data as auxiliary variable')
	status.bar.display(slope.chk.auxvar, TextOutputVar, 'Include slope data as auxiliary variable')
	infobulle(aspect.chk.auxvar, 'Include aspect data as auxiliary variable')
	status.bar.display(aspect.chk.auxvar, TextOutputVar, 'Include aspect data as auxiliary variable')

	############################################
	tkgrid(frBias, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSTN, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRFE, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frDEM, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	# tkgrid(frPfxBias, row = 5, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frauxvar, row = 5, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################  RIGHT   #####################

	frDate <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(GeneralParameters$period, 
									'daily' = cb.periodVAL[1], 
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3])
	year1 <- tclVar(GeneralParameters$Bias.Date.Range$start.year)
	year2 <- tclVar(GeneralParameters$Bias.Date.Range$end.year)
	# use.months <- tclVar(paste(GeneralParameters$Bias.Months, collapse=' '))

	cb.period <- ttkcombobox(frDate, values = cb.periodVAL, textvariable = file.period, width = largeur1)
	frtxtDate <- ttklabelframe(frDate, text = "Year Range", relief = 'groove')

	years1.l <- tklabel(frtxtDate, text = 'Start', anchor = 'e', justify = 'right')
	years2.l <- tklabel(frtxtDate, text = 'End', anchor = 'e', justify = 'right')
	years1.v <- tkentry(frtxtDate, width = 6, textvariable = year1, justify = 'right')
	years2.v <- tkentry(frtxtDate, width = 6, textvariable = year2, justify = 'right')

	# txt.months <- tklabel(frDate, text = 'Months', anchor = 'w', justify = 'left')
	# en.months <- tkentry(frDate, width = 20, textvariable = use.months, justify = "left")

	tkgrid(years1.l, row = 0, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(years1.v, row = 0, column = 1, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(years2.l, row = 0, column = 2, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(years2.v, row = 0, column = 3, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frtxtDate, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
	# tkgrid(txt.months, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	# tkgrid(en.months, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.period, 'Choose the time step of the data')
	status.bar.display(cb.period, TextOutputVar, 'Choose the time step of the data')
	infobulle(years1.v, 'Start year to be used to compute mean Gauge-RFE bias')
	status.bar.display(years1.v, TextOutputVar, 'Start year to be used to compute mean Gauge-RFE bias')
	infobulle(years2.v, 'End year to be used to compute mean Gauge-RFE bias')
	status.bar.display(years2.v, TextOutputVar, 'End year to be used to compute mean Gauge-RFE bias')
	# infobulle(en.months, 'Months to compute bias')
	# status.bar.display(en.months, TextOutputVar, 'Months to compute bias')


	############################################

	frGrid <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	varCreateGrd <- tclVar(GeneralParameters$Create.Grid)
	stategrd <- if(str_trim(GeneralParameters$Create.Grid) == '3') 'normal' else 'disabled'

	txt.CreateGrd <- tklabel(frGrid, text = 'Create grid for interpolation', anchor = 'w', justify = 'left')
	grdRFE.rbt <- tkradiobutton(frGrid, text = "From RFE", anchor = 'w', justify = 'left')
	grdDEM.rbt <- tkradiobutton(frGrid, text = "From DEM", anchor = 'w', justify = 'left')
	grdNEW.rbt <- tkradiobutton(frGrid, text = "New Grid", anchor = 'w', justify = 'left')
	bt.getNewgrid <- tkbutton(frGrid, text = "Create", state = stategrd)

	####
	tkconfigure(grdRFE.rbt, variable = varCreateGrd, value = "1")
	tkconfigure(grdDEM.rbt, variable = varCreateGrd, value = "2")
	tkconfigure(grdNEW.rbt, variable = varCreateGrd, value = "3")

	tkconfigure(bt.getNewgrid, command = function(){
		GeneralParameters <<- getParamNewGrid(tt, GeneralParameters)
	})
	#####

	tkgrid(txt.CreateGrd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grdRFE.rbt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grdDEM.rbt, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grdNEW.rbt, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.getNewgrid, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(bt.getNewgrid, 'Set the new grid')
	status.bar.display(bt.getNewgrid, TextOutputVar, 'Set the new grid')
	infobulle(frGrid, 'Create the grid to interpolate the bias factor or distr parameters')
	status.bar.display(frGrid, TextOutputVar, 'Create the grid to interpolate the bias factor or distr parameters')

	###########
	tkbind(grdRFE.rbt, "<Button-1>", function(){
		tkconfigure(bt.getNewgrid, state = 'disabled')
	})
	tkbind(grdDEM.rbt, "<Button-1>", function(){
		tkconfigure(bt.getNewgrid, state = 'disabled')
	})
	tkbind(grdNEW.rbt, "<Button-1>", function(){
		tkconfigure(bt.getNewgrid, state = 'normal')
	})

	############################################

	frInterp <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	interp.method <- tclVar()
	cb.InterpVAL <- c('Nearest Neighbor', 'Inverse Distance Weighted', 'Kriging')
	tclvalue(interp.method) <- switch(GeneralParameters$Interpolation.pars$interp.method, 
										'NN' = cb.InterpVAL[1], 
										'IDW' = cb.InterpVAL[2],
										'Kriging' = cb.InterpVAL[3])
	nmin <- tclVar(GeneralParameters$Interpolation.pars$nmin)
	nmax <- tclVar(GeneralParameters$Interpolation.pars$nmax)
	maxdist <- tclVar(GeneralParameters$Interpolation.pars$maxdist)
	rad.lon <- tclVar(GeneralParameters$Interpolation.pars$rad.lon)
	rad.lat <- tclVar(GeneralParameters$Interpolation.pars$rad.lat)
	rad.elv <- tclVar(GeneralParameters$Interpolation.pars$rad.elv)
	res.coarse <- tclVar(GeneralParameters$Interpolation.pars$res.coarse)

	if(GeneralParameters$Interpolation.pars$interp.method == 'NN'){
		statenn <- 'normal'
		stateidw <- 'disabled'
	}else{
		statenn <- 'disabled'
		stateidw <- 'normal'
	}

	txt.Interp <- tklabel(frInterp, text = 'Interpolation method', anchor = 'w', justify = 'left')
	cb.Interp <- ttkcombobox(frInterp, values = cb.InterpVAL, textvariable = interp.method, width = largeur1)
	frNN <- ttklabelframe(frInterp, text = "Nearest Neighbor", relief = 'groove')
	frIDW <- ttklabelframe(frInterp, text = "Inverse Distance & Kriging", relief = 'groove')
	# frCoarse <- tkframe(frInterp)

	#######
	mul.lon.l <- tklabel(frNN, text = 'Multi.Lon', anchor = 'e', justify = 'right')
	mul.lat.l <- tklabel(frNN, text = 'Multi.Lat', anchor = 'e', justify = 'right')
	mul.elv.l <- tklabel(frNN, text = 'Multi.Elv', anchor = 'e', justify = 'right')
	mul.lon.v <- tkentry(frNN, width = 4, textvariable = rad.lon, justify = 'right', state = statenn)
	mul.lat.v <- tkentry(frNN, width = 4, textvariable = rad.lat, justify = 'right', state = statenn)
	mul.elv.v <- tkentry(frNN, width = 4, textvariable = rad.elv, justify = 'right', state = statenn)

	########
	min.nbrs.l <- tklabel(frIDW, text = 'nmin', anchor = 'e', justify = 'right')
	max.nbrs.l <- tklabel(frIDW, text = 'nmax', anchor = 'e', justify = 'right')
	max.dst.l <- tklabel(frIDW, text = 'maxdist', anchor = 'e', justify = 'right')
	min.nbrs.v <- tkentry(frIDW, width = 4, textvariable = nmin, justify = 'right', state = stateidw)
	max.nbrs.v <- tkentry(frIDW, width = 4, textvariable = nmax, justify = 'right', state = stateidw)
	max.dst.v <- tkentry(frIDW, width = 4, textvariable = maxdist, justify = 'right', state = stateidw)

	########
	# res.coarse.l <- tklabel(frCoarse, text = 'Coarse Grid', anchor = 'e', justify = 'right')
	# res.coarse.v <- tkentry(frCoarse, width = 4, textvariable = res.coarse, justify = 'right')

	########

	tkgrid(mul.lon.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mul.lon.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mul.lat.l, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mul.lat.v, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mul.elv.l, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mul.elv.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)

	########

	tkgrid(min.nbrs.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.nbrs.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.nbrs.l, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.nbrs.v, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dst.l, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dst.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)

	########
	# tkgrid(res.coarse.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	# tkgrid(res.coarse.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)

	########

	tkgrid(txt.Interp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.Interp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frNN, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(frIDW, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
	# tkgrid(frCoarse, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	########

	infobulle(mul.lon.v, 'Maximum distance belong longitude, n times of grid interpolation resolution')
	status.bar.display(mul.lon.v, TextOutputVar, 'Maximum distance belong longitude, n times of grid interpolation resolution')
	infobulle(mul.lat.v, 'Maximum distance belong latitude, n times of grid interpolation resolution')
	status.bar.display(mul.lat.v, TextOutputVar, 'Maximum distance belong latitude, n times of grid interpolation resolution')
	infobulle(mul.elv.v, 'Maximum height for elevation, n times of elevation resolution\n(elevation is discretized by 100 m)')
	status.bar.display(mul.elv.v, TextOutputVar, 'Maximum height for elevation, n times of elevation resolution\n(elevation is discretized by 100 m)')

	infobulle(min.nbrs.v, 'Minimum number of neighbours to be used to interpolate the bias')
	status.bar.display(min.nbrs.v, TextOutputVar, 'Minimum number of neighbours to be used to interpolate the bias')
	infobulle(max.nbrs.v, 'Maximum number of neighbours to be used to interpolate the bias')
	status.bar.display(max.nbrs.v, TextOutputVar, 'Maximum number of neighbours to be used to interpolate the bias')
	infobulle(max.dst.v, 'Maximum distance (in  decimal degree) to be used to interpolate the bias')
	status.bar.display(max.dst.v, TextOutputVar, 'Maximum distance (in  decimal degree) to be used to interpolate the bias')

	# infobulle(frCoarse, 'Coarse resolution from gridded data to complete points data\nwhen interpolating (in decimal degree)')
	# status.bar.display(frCoarse, TextOutputVar, 'Coarse resolution from gridded data to complete points data\nwhen interpolating (in decimal degree)')

	tkbind(cb.Interp, "<<ComboboxSelected>>", function(){
		if(tclvalue(interp.method) == 'Nearest Neighbor'){
			tkconfigure(mul.lon.v, state = 'normal')
			tkconfigure(mul.lat.v, state = 'normal')
			tkconfigure(mul.elv.v, state = 'normal')
			tkconfigure(min.nbrs.v, state = 'disabled')
			tkconfigure(max.nbrs.v, state = 'disabled')
			tkconfigure(max.dst.v, state = 'disabled')
		}else{
			tkconfigure(mul.lon.v, state = 'disabled')
			tkconfigure(mul.lat.v, state = 'disabled')
			tkconfigure(mul.elv.v, state = 'disabled')
			tkconfigure(min.nbrs.v, state = 'normal')
			tkconfigure(max.nbrs.v, state = 'normal')
			tkconfigure(max.dst.v, state = 'normal')
		}
	})

	############################################

	frPfxBias <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	outbiasff <- tclVar(GeneralParameters$Prefix$Mean.Bias.Prefix)
	statePfxBias <- if(str_trim(GeneralParameters$Bias.Method) == "Quantile.Mapping") 'disabled' else 'normal'

	txt.outbiasff <- tklabel(frPfxBias, text = 'Mean bias filename prefix', anchor = 'w', justify = 'left')
	en.outbiasff <- tkentry(frPfxBias, textvariable = outbiasff, width = largeur, state = statePfxBias)

	tkgrid(txt.outbiasff, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.outbiasff, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.outbiasff, 'Prefix for the file name of the mean bias coefficient')
	status.bar.display(en.outbiasff, TextOutputVar, 'Prefix for the file name of the mean bias coefficient')

	############################################
	tkgrid(frDate, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frGrid, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frInterp, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frPfxBias, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################
	
	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	#######

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(file.stnfl)) == ""){
			tkmessageBox(message = "Choose the file containing the gauge data", icon = "warning", type = "ok")
			#tkwait.window(tt)
		}else if(str_trim(tclvalue(dir.rfe)) == "" | str_trim(tclvalue(dir.rfe)) == "NA"){
			tkmessageBox(message = "Choose or enter the  directory containing the RFE files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.grdrfe)) == ""){
			tkmessageBox(message = "You have to provide a RFE's sample file", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.grddem)) == "" ){
			tkmessageBox(message = "You have to choose DEM data in NetCDF format", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.save1)) == "" | str_trim(tclvalue(file.save1)) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			# months <- strsplit(str_trim(tclvalue(use.months)), ' ')[[1]]
			# months <- months[months != ""]
			# if(length(months) == 0){
			# 	tkmessageBox(message = "Need at least one month", icon = "warning", type = "ok")
			# 	tkwait.window(tt)
			# }else{
				GeneralParameters$period <<- switch(str_trim(tclvalue(file.period)), 
				 									'Daily data' = 'daily',
													'Dekadal data' =  'dekadal',
													'Monthly data' = 'monthly')
				GeneralParameters$Bias.Method <<- str_trim(tclvalue(bias.method))
				GeneralParameters$Bias.Date.Range$start.year <<- str_trim(tclvalue(year1))
				GeneralParameters$Bias.Date.Range$end.year <<- str_trim(tclvalue(year2))
				# GeneralParameters$Bias.Months <<- sort(as.numeric(months))

				GeneralParameters$IO.files$STN.file <<- str_trim(tclvalue(file.stnfl))
				GeneralParameters$IO.files$RFE.dir <<- str_trim(tclvalue(dir.rfe))
				GeneralParameters$IO.files$RFE.file <<- str_trim(tclvalue(file.grdrfe))
				GeneralParameters$IO.files$DEM.file <<- str_trim(tclvalue(file.grddem))
				GeneralParameters$IO.files$dir2save <<- str_trim(tclvalue(file.save1))

				GeneralParameters$Create.Grid <<- str_trim(tclvalue(varCreateGrd))

				GeneralParameters$Prefix$RFE.File.Format <<- str_trim(tclvalue(inrfeff))
				GeneralParameters$Prefix$Mean.Bias.Prefix <<- str_trim(tclvalue(outbiasff))

				GeneralParameters$Interpolation.pars$interp.method <<- switch(str_trim(tclvalue(interp.method)),
									 									'Nearest Neighbor' = 'NN',
																		'Inverse Distance Weighted' = 'IDW',
																		'Kriging' = 'Kriging')
				GeneralParameters$Interpolation.pars$rad.lon <<- as.numeric(str_trim(tclvalue(rad.lon)))
				GeneralParameters$Interpolation.pars$rad.lat <<- as.numeric(str_trim(tclvalue(rad.lat)))
				GeneralParameters$Interpolation.pars$rad.elv <<- as.numeric(str_trim(tclvalue(rad.elv)))
				# GeneralParameters$Interpolation.pars$res.coarse <<- as.numeric(str_trim(tclvalue(res.coarse)))
				GeneralParameters$Interpolation.pars$nmin <<- as.numeric(str_trim(tclvalue(nmin)))
				GeneralParameters$Interpolation.pars$nmax <<- as.numeric(str_trim(tclvalue(nmax)))
				GeneralParameters$Interpolation.pars$maxdist <<- as.numeric(str_trim(tclvalue(maxdist)))

				GeneralParameters$auxvar$dem <<- switch(tclvalue(dem.auxvar), '0' = FALSE, '1' = TRUE)
				GeneralParameters$auxvar$slope <<- switch(tclvalue(slope.auxvar), '0' = FALSE, '1' = TRUE)
				GeneralParameters$auxvar$aspect <<- switch(tclvalue(aspect.auxvar), '0' = FALSE, '1' = TRUE)

				tkgrab.release(tt)
				tkdestroy(tt)
				tkfocus(parent.win)
			# }
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

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Mean Bias computation - Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

##############################################################################################

rmvBiasGetInfoRain <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur <- 28
		largeur1 <- 25
	}else{
		largeur <- 25
		largeur1 <- 24
	}


	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	frRight <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################

	frBias <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	bias.method <- tclVar(str_trim(GeneralParameters$Bias.Method))
	cb.biasMthd <- c("Quantile.Mapping", "Multiplicative.Bias.Var", "Multiplicative.Bias.Mon")

	txt.bias <- tklabel(frBias, text = 'Bias method', anchor = 'w', justify = 'left')
	cb.bias <- ttkcombobox(frBias, values = cb.biasMthd, textvariable = bias.method, width = largeur1)

	tkgrid(txt.bias, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.bias, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.bias, 'Method used to calculate Bias Factors or Parameters')
	status.bar.display(cb.bias, TextOutputVar, 'Method used to calculate Bias Factors or Parameters')

	######

	tkbind(cb.bias, "<<ComboboxSelected>>", function(){
		stateBSM <- if(tclvalue(bias.method) == "Quantile.Mapping") 'disabled' else 'normal'
		tkconfigure(en.outbiasff, state = stateBSM)
	})

	############################################

	frRFE <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	dir.rfe <- tclVar(GeneralParameters$IO.files$RFE.dir)
	file.grdrfe <- tclVar(GeneralParameters$IO.files$RFE.file)
	inrfeff <- tclVar(GeneralParameters$Prefix$RFE.File.Format)

	txt.dir.rfe <- tklabel(frRFE, text = 'Directory of RFE files', anchor = 'w', justify = 'left')
	en.dir.rfe <- tkentry(frRFE, textvariable = dir.rfe, width = largeur)
	bt.dir.rfe <- tkbutton(frRFE, text = "...")
	txt.grdrfe <- tklabel(frRFE, text = "RFE's sample file", anchor = 'w', justify = 'left')
	cb.grdrfe <- ttkcombobox(frRFE, values = unlist(listOpenFiles), textvariable = file.grdrfe, width = largeur1)
	bt.grdrfe <- tkbutton(frRFE, text = "...")
	txt.inrfeff <- tklabel(frRFE, text = 'Input RFE filename format', anchor = 'w', justify = 'left')
	en.inrfeff <- tkentry(frRFE, textvariable = inrfeff, width = largeur)

	######
	tkconfigure(bt.dir.rfe, command = function(){
		dir4rfe <- tk_choose.dir(GeneralParameters$IO.files$RFE.dir, "")
		tclvalue(dir.rfe) <- if(!is.na(dir4rfe)) dir4rfe else ""
	})

	tkconfigure(bt.grdrfe, command = function(){
		nc.opfiles <- getOpenNetcdf(tt, all.opfiles, initialdir = tclvalue(dir.rfe))
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grdrfe) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.grdrfe, values = unlist(listOpenFiles), textvariable = file.grdrfe)
		}else return(NULL)
	})

	######

	tkgrid(txt.dir.rfe, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.dir.rfe, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.rfe, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.grdrfe, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.grdrfe, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.grdrfe, row = 3, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.inrfeff, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.inrfeff, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.dir.rfe, 'Enter the full path to directory containing the RFE files')
	status.bar.display(en.dir.rfe, TextOutputVar, 'Enter the full path to directory containing the RFE files')
	infobulle(bt.dir.rfe, 'or browse here')
	status.bar.display(bt.dir.rfe, TextOutputVar, 'or browse here')
	infobulle(cb.grdrfe, 'Choose the file in the list')
	status.bar.display(cb.grdrfe, TextOutputVar, 'File containing a sample of RFE data in netcdf')
	infobulle(bt.grdrfe, 'Browse file if not listed')
	status.bar.display(bt.grdrfe, TextOutputVar, 'Browse file if not listed')
	infobulle(en.inrfeff, 'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01-dk2.nc')
	status.bar.display(en.inrfeff, TextOutputVar, 'Enter the format of the RFE files names in NetCDF,\nexample: rfe1983_01-dk2.nc')

	############################################

	frDirBias <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	dir.bias <- tclVar(GeneralParameters$IO.files$Bias.dir)
	outbiasff <- tclVar(GeneralParameters$Prefix$Mean.Bias.Prefix)
	statePfxBias <- if(str_trim(GeneralParameters$Bias.Method) == "Quantile.Mapping") 'disabled' else 'normal'

	txt.dir.bias <- tklabel(frDirBias, text = "Directory of mean bias files", anchor = 'w', justify = 'left')
	en.dir.bias <- tkentry(frDirBias, textvariable = dir.bias, width = largeur)
	bt.dir.bias <- tkbutton(frDirBias, text = "...")
	txt.outbiasff <- tklabel(frDirBias, text = 'Mean bias filename prefix', anchor = 'w', justify = 'left')
	en.outbiasff <- tkentry(frDirBias, textvariable = outbiasff, width = largeur, state = statePfxBias)

	#####
	tkconfigure(bt.dir.bias, command = function(){
		dir4bias <- tk_choose.dir(GeneralParameters$IO.files$Bias.dir, "")
		tclvalue(dir.bias) <- if(is.na(dir4bias)) "" else dir4bias
	})

	#####

	tkgrid(txt.dir.bias, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.dir.bias, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.bias, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.outbiasff, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.outbiasff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.dir.bias, 'Enter the full path to directory containing the mean bias files')
	status.bar.display(en.dir.bias, TextOutputVar, 'Enter the full path to directory containing the mean bias files')
	infobulle(bt.dir.bias, 'or browse here')
	status.bar.display(bt.dir.bias, TextOutputVar, 'or browse here')
	infobulle(en.outbiasff, 'Prefix for the file name of the mean bias coefficient')
	status.bar.display(en.outbiasff, TextOutputVar, 'Prefix for the file name of the mean bias coefficient')

	############################################
	tkgrid(frBias, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRFE, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frDirBias, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################  RIGHT   #####################

	frDate <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(GeneralParameters$period, 
									'daily' = cb.periodVAL[1], 
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3])
	istart.yrs <- tclVar(GeneralParameters$Adjust.Date.Range$start.year)
	istart.mon <- tclVar(GeneralParameters$Adjust.Date.Range$start.mon)
	istart.day <- tclVar(GeneralParameters$Adjust.Date.Range$start.dek)
	iend.yrs <- tclVar(GeneralParameters$Adjust.Date.Range$end.year)
	iend.mon <- tclVar(GeneralParameters$Adjust.Date.Range$end.mon)
	iend.day <- tclVar(GeneralParameters$Adjust.Date.Range$end.dek)
	day.txtVar <- if(GeneralParameters$period == 'dekadal') tclVar('Dek') else tclVar('Day')
	statedate <- if(GeneralParameters$period == 'monthly') 'disabled' else 'normal'
	
	# use.months <- tclVar(paste(GeneralParameters$Adjust.Months, collapse=' '))

	cb.period <- ttkcombobox(frDate, values = cb.periodVAL, textvariable = file.period, width = largeur1)
	frtxtDate <- ttklabelframe(frDate, text = "Date Range", relief = 'groove')

	deb.txt <- tklabel(frtxtDate, text = 'Start date', anchor = 'e', justify = 'right')
	fin.txt <- tklabel(frtxtDate, text = 'End date', anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(frtxtDate, text = 'Year')
	mon.txt <- tklabel(frtxtDate, text = 'Month')
	day.txt <- tklabel(frtxtDate, text = tclvalue(day.txtVar), textvariable = day.txtVar)
	yrs1.v <- tkentry(frtxtDate, width = 4, textvariable = istart.yrs, justify = "right")
	mon1.v <- tkentry(frtxtDate, width = 4, textvariable = istart.mon, justify = "right")
	day1.v <- tkentry(frtxtDate, width = 4, textvariable = istart.day, justify = "right", state = statedate)
	yrs2.v <- tkentry(frtxtDate, width = 4, textvariable = iend.yrs, justify = "right")
	mon2.v <- tkentry(frtxtDate, width = 4, textvariable = iend.mon, justify = "right")
	day2.v <- tkentry(frtxtDate, width = 4, textvariable = iend.day, justify = "right", state = statedate)

	# txt.months <- tklabel(frDate, text = 'Months', anchor = 'w', justify = 'left')
	# en.months <- tkentry(frDate, width = 20, textvariable = use.months, justify = "left")

	tkgrid(deb.txt, row = 1, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fin.txt, row = 2, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrs.txt, row = 0, column = 1, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon.txt, row = 0, column = 2, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(day.txt, row = 0, column = 3, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrs1.v, row = 1, column = 1, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon1.v, row = 1, column = 2, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(day1.v, row = 1, column = 3, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrs2.v, row = 2, column = 1, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon2.v, row = 2, column = 2, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(day2.v, row = 2, column = 3, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frtxtDate, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
	# tkgrid(txt.months, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	# tkgrid(en.months, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.period, 'Choose the time step of the data')
	status.bar.display(cb.period, TextOutputVar, 'Choose the time step of the data')
	infobulle(frtxtDate, 'Start and end date for adjusting RFE data')
	status.bar.display(frtxtDate, TextOutputVar, 'Start and end date for adjusting RFE data')
	# infobulle(en.months, 'Months to be adjusted')
	# status.bar.display(en.months, TextOutputVar, 'Months to be adjusted')

	###########
	tkbind(cb.period, "<<ComboboxSelected>>", function(){
		if(tclvalue(file.period) == 'Daily data'){
			tclvalue(day.txtVar) <- "Day"
			tkconfigure(day1.v, state = 'normal')
			tkconfigure(day2.v, state = 'normal')
		}
		if(tclvalue(file.period) == 'Dekadal data'){
			tclvalue(day.txtVar) <- "Dek"
			tkconfigure(day1.v, state = 'normal')
			tkconfigure(day2.v, state = 'normal')
		}
		if(tclvalue(file.period) == 'Monthly data'){
			tclvalue(day.txtVar) <- "Day"
			tkconfigure(day1.v, state = 'disabled')
			tkconfigure(day2.v, state = 'disabled')
		}
	})

	############################################

	frAdjFF <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	adjrfeff <- tclVar(GeneralParameters$Prefix$Adj.File.Format)

	txt.adjrfeff <- tklabel(frAdjFF, text = 'Adjusted data filename format', anchor = 'w', justify = 'left')
	en.adjrfeff <- tkentry(frAdjFF, textvariable = adjrfeff, width = largeur)

	tkgrid(txt.adjrfeff, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.adjrfeff, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.adjrfeff, 'Format of the adjusted RFE files names in NetCDF,\nexample: rr_adj_1983011.nc')
	status.bar.display(en.adjrfeff, TextOutputVar, 'Format of the adjusted RFE files names in NetCDF,\nexample: rr_adj_1983011.nc')

	############################################

	frSave <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	file.save1 <- tclVar(GeneralParameters$IO.files$dir2save)

	txt.file.save <- tklabel(frSave, text = 'Directory to save result', anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frSave, textvariable = file.save1, width = largeur-3)
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
	tkgrid(frDate, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frAdjFF, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################
	
	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	#######
	tkconfigure(bt.prm.OK, command = function(){
		if(tclvalue(dir.rfe) == "" | tclvalue(dir.rfe) == "NA"){
			tkmessageBox(message = "Choose or enter the  directory containing the RFE files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.grdrfe) == ""){
			tkmessageBox(message = "You have to provide a RFE's sample file", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(dir.bias) == "" | tclvalue(dir.bias) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory or file containing mean bias files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save1) == "" | tclvalue(file.save1) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			# months <- strsplit(str_trim(tclvalue(use.months)), ' ')[[1]]
			# months <- months[months != ""]
			# if(length(months) == 0){
			# 	tkmessageBox(message = "Need at least one month", icon = "warning", type = "ok")
			# 	tkwait.window(tt)
			# }else{
				GeneralParameters$period <<- switch(str_trim(tclvalue(file.period)), 
				 									'Daily data' = 'daily',
													'Dekadal data' =  'dekadal',
													'Monthly data' = 'monthly')
				GeneralParameters$IO.files$Bias.dir <<- str_trim(tclvalue(dir.bias))
				GeneralParameters$IO.files$RFE.dir <<- str_trim(tclvalue(dir.rfe))
				GeneralParameters$IO.files$RFE.file <<- str_trim(tclvalue(file.grdrfe))
				GeneralParameters$IO.files$dir2save <<- str_trim(tclvalue(file.save1))
				GeneralParameters$Prefix$RFE.File.Format <<- str_trim(tclvalue(inrfeff))
				GeneralParameters$Prefix$Mean.Bias.Prefix <<- str_trim(tclvalue(outbiasff))
				GeneralParameters$Prefix$Adj.File.Format <<- str_trim(tclvalue(adjrfeff))
				GeneralParameters$Adjust.Date.Range$start.year <<- str_trim(tclvalue(istart.yrs))
				GeneralParameters$Adjust.Date.Range$start.mon <<- str_trim(tclvalue(istart.mon))
				GeneralParameters$Adjust.Date.Range$start.dek <<- str_trim(tclvalue(istart.day))
				GeneralParameters$Adjust.Date.Range$end.year <<- str_trim(tclvalue(iend.yrs))
				GeneralParameters$Adjust.Date.Range$end.mon <<- str_trim(tclvalue(iend.mon))
				GeneralParameters$Adjust.Date.Range$end.dek <<- str_trim(tclvalue(iend.day))
				GeneralParameters$Bias.Method <<- str_trim(tclvalue(bias.method))
				# GeneralParameters$Adjust.Months <<- sort(as.numeric(months))

				tkgrab.release(tt)
				tkdestroy(tt)
				tkfocus(parent.win)
			# }
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

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Bias Correction - Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

##############################################################################################

##############################################################################################

createNewGrid2Merge <- function(parent.win, GeneralParameters){
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	frRight <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################
	frGrid <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	varCreateGrd <- tclVar(GeneralParameters$Grid.From)

	txt.CreateGrd <- tklabel(frGrid, text = 'Create grid for interpolation', anchor = 'w', justify = 'left')
	grdRFE.rbt <- tkradiobutton(frGrid, text = "From RFE", anchor = 'w', justify = 'left')
	grdDEM.rbt <- tkradiobutton(frGrid, text = "From DEM", anchor = 'w', justify = 'left')
	grdNEW.rbt <- tkradiobutton(frGrid, text = "New Grid", anchor = 'w', justify = 'left')

	####
	tkconfigure(grdRFE.rbt, variable = varCreateGrd, value = "1")
	tkconfigure(grdDEM.rbt, variable = varCreateGrd, value = "2")
	tkconfigure(grdNEW.rbt, variable = varCreateGrd, value = "3")

	#####

	tkgrid(txt.CreateGrd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grdRFE.rbt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grdDEM.rbt, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grdNEW.rbt, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(frGrid, 'Create the grid to interpolate the merged data')
	status.bar.display(frGrid, TextOutputVar, 'Create the grid to interpolate the merged data')

	############################################
	tkgrid(frGrid, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################

	frNewGrid <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	minLon <- tclVar(GeneralParameters$New.Grid.Def$minlon)
	maxLon <- tclVar(GeneralParameters$New.Grid.Def$maxlon)
	resLon <- tclVar(GeneralParameters$New.Grid.Def$reslon)
	minLat <- tclVar(GeneralParameters$New.Grid.Def$minlat)
	maxLat <- tclVar(GeneralParameters$New.Grid.Def$maxlat)
	resLat <- tclVar(GeneralParameters$New.Grid.Def$reslat)

	grd_txt <- tklabel(frNewGrid, text = "Create new grid", anchor = 'w', justify = 'left')
	grd_llon <- tklabel(frNewGrid, text = "Longitude", anchor = 'e', justify = 'right')
	grd_llat <- tklabel(frNewGrid, text = "Latitude", anchor = 'e', justify = 'right')
	grd_lb1 <- tklabel(frNewGrid, text = "Min")
	grd_lb2 <- tklabel(frNewGrid, text = "Max")
	grd_lb3 <- tklabel(frNewGrid, text = "Res")

	grd_vlon1 <- tkentry(frNewGrid, width = 5, justify = "right", textvariable = minLon)
	grd_vlon2 <- tkentry(frNewGrid, width = 5, justify = "right", textvariable = maxLon)
	grd_vlon3 <- tkentry(frNewGrid, width = 6, justify = "right", textvariable = resLon)
	grd_vlat1 <- tkentry(frNewGrid, width = 5, justify = "right", textvariable = minLat)
	grd_vlat2 <- tkentry(frNewGrid, width = 5, justify = "right", textvariable = maxLat)
	grd_vlat3 <- tkentry(frNewGrid, width = 6, justify = "right", textvariable = resLat)

	tkgrid(grd_txt, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_lb1, row = 1, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_lb2, row = 1, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_lb3, row = 1, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_llon, row = 2, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon1, row = 2, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon2, row = 2, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon3, row = 2, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_llat, row = 3, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat1, row = 3, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat2, row = 3, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat3, row = 3, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(grd_vlon1, 'Minimum longitude in degree decimal')
	status.bar.display(grd_vlon1, TextOutputVar, 'Minimum longitude in degree decimal')
	infobulle(grd_vlon2, 'Maximum longitude in degree decimal')
	status.bar.display(grd_vlon2, TextOutputVar, 'Maximum longitude in degree decimal')
	infobulle(grd_vlon3, 'Resolution in degree decimal')
	status.bar.display(grd_vlon3, TextOutputVar, 'Resolution in degree decimal')
	infobulle(grd_vlat1, 'Minimum latitude in degree decimal')
	status.bar.display(grd_vlat1, TextOutputVar, 'Minimum latitude in degree decimal')
	infobulle(grd_vlat2, 'Maximum latitude in degree decimal')
	status.bar.display(grd_vlat2, TextOutputVar, 'Maximum latitude in degree decimal')
	infobulle(grd_vlat3, 'Resolution in degree decimal')
	status.bar.display(grd_vlat3, TextOutputVar, 'Resolution in degree decimal')

	############################################
	tkgrid(frNewGrid, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################

	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		GeneralParameters$Grid.From <<- str_trim(tclvalue(varCreateGrd))
		GeneralParameters$New.Grid.Def$minlon <<- as.numeric(str_trim(tclvalue(minLon)))
		GeneralParameters$New.Grid.Def$maxlon <<- as.numeric(str_trim(tclvalue(maxLon)))
		GeneralParameters$New.Grid.Def$reslon <<- as.numeric(str_trim(tclvalue(resLon)))
		GeneralParameters$New.Grid.Def$minlat <<- as.numeric(str_trim(tclvalue(minLat)))
		GeneralParameters$New.Grid.Def$maxlat <<- as.numeric(str_trim(tclvalue(maxLat)))
		GeneralParameters$New.Grid.Def$reslat <<- as.numeric(str_trim(tclvalue(resLat)))

		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
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

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'New grid - Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}


##############################################################################################

getParamNewGrid <- function(tt, GeneralParameters){
	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frGrd0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frGrd1 <- tkframe(tt1)

	################################

	fr_grd <- ttklabelframe(frGrd0, text = "Create new grid", relief = "groove", borderwidth = 2)

	minLon <- tclVar(GeneralParameters$New.Grid.Def$minlon)
	maxLon <- tclVar(GeneralParameters$New.Grid.Def$maxlon)
	resLon <- tclVar(GeneralParameters$New.Grid.Def$reslon)
	minLat <- tclVar(GeneralParameters$New.Grid.Def$minlat)
	maxLat <- tclVar(GeneralParameters$New.Grid.Def$maxlat)
	resLat <- tclVar(GeneralParameters$New.Grid.Def$reslat)

	grd_llon <- tklabel(fr_grd, text = "Longitude", anchor = 'e', justify = 'right')
	grd_llat <- tklabel(fr_grd, text = "Latitude", anchor = 'e', justify = 'right')
	grd_lb1 <- tklabel(fr_grd, text = "Min")
	grd_lb2 <- tklabel(fr_grd, text = "Max")
	grd_lb3 <- tklabel(fr_grd, text = "Res")

	grd_vlon1 <- tkentry(fr_grd, width = 5, justify = "right", textvariable = minLon)
	grd_vlon2 <- tkentry(fr_grd, width = 5, justify = "right", textvariable = maxLon)
	grd_vlon3 <- tkentry(fr_grd, width = 6, justify = "right", textvariable = resLon)
	grd_vlat1 <- tkentry(fr_grd, width = 5, justify = "right", textvariable = minLat)
	grd_vlat2 <- tkentry(fr_grd, width = 5, justify = "right", textvariable = maxLat)
	grd_vlat3 <- tkentry(fr_grd, width = 6, justify = "right", textvariable = resLat)

	tkgrid(grd_lb1, row = 0, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_lb2, row = 0, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_lb3, row = 0, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_llon, row = 1, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon1, row = 1, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon2, row = 1, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon3, row = 1, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_llat, row = 2, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat1, row = 2, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat2, row = 2, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat3, row = 2, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(grd_vlon1, 'Minimum longitude in degree decimal')
	status.bar.display(grd_vlon1, TextOutputVar, 'Minimum longitude in degree decimal')
	infobulle(grd_vlon2, 'Maximum longitude in degree decimal')
	status.bar.display(grd_vlon2, TextOutputVar, 'Maximum longitude in degree decimal')
	infobulle(grd_vlon3, 'Resolution in degree decimal')
	status.bar.display(grd_vlon3, TextOutputVar, 'Resolution in degree decimal')
	infobulle(grd_vlat1, 'Minimum latitude in degree decimal')
	status.bar.display(grd_vlat1, TextOutputVar, 'Minimum latitude in degree decimal')
	infobulle(grd_vlat2, 'Maximum latitude in degree decimal')
	status.bar.display(grd_vlat2, TextOutputVar, 'Maximum latitude in degree decimal')
	infobulle(grd_vlat3, 'Resolution in degree decimal')
	status.bar.display(grd_vlat3, TextOutputVar, 'Resolution in degree decimal')

	################################

	tkgrid(fr_grd, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	################################

	bt.prm.OK <- tkbutton(frGrd1, text=" OK ")
	bt.prm.CA <- tkbutton(frGrd1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		GeneralParameters$New.Grid.Def$minlon <<- as.numeric(str_trim(tclvalue(minLon)))
		GeneralParameters$New.Grid.Def$maxlon <<- as.numeric(str_trim(tclvalue(maxLon)))
		GeneralParameters$New.Grid.Def$reslon <<- as.numeric(str_trim(tclvalue(resLon)))
		GeneralParameters$New.Grid.Def$minlat <<- as.numeric(str_trim(tclvalue(minLat)))
		GeneralParameters$New.Grid.Def$maxlat <<- as.numeric(str_trim(tclvalue(maxLat)))
		GeneralParameters$New.Grid.Def$reslat <<- as.numeric(str_trim(tclvalue(resLat)))

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

	tkgrid(frGrd0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frGrd1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'Grid Parameters')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(GeneralParameters)
}

##############################################################################################

getDekadalData2ScaleDaily <- function(tt, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur <- 28
		largeur1 <- 25
	}else{
		largeur <- 25
		largeur1 <- 24
	}

	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frGrd0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frGrd1 <- tkframe(tt1)

	################################

	frRFE <- tkframe(frGrd0, relief = 'sunken', borderwidth = 2)

	dir.rfe <- tclVar(GeneralParameters$IO.files$DEK.dir)
	file.grdrfe <- tclVar(GeneralParameters$IO.files$DEK.file)
	inrfeff <- tclVar(GeneralParameters$FileFormat$DEK.File.Format)

	txt.dir.rfe <- tklabel(frRFE, text = 'Directory of merged dekadal files', anchor = 'w', justify = 'left')
	en.dir.rfe <- tkentry(frRFE, textvariable = dir.rfe, width = largeur)
	bt.dir.rfe <- tkbutton(frRFE, text = "...")
	txt.grdrfe <- tklabel(frRFE, text = "Merged dekadal sample file", anchor = 'w', justify = 'left')
	cb.grdrfe <- ttkcombobox(frRFE, values = unlist(listOpenFiles), textvariable = file.grdrfe, width = largeur1)
	bt.grdrfe <- tkbutton(frRFE, text = "...")
	txt.inrfeff <- tklabel(frRFE, text = 'Merged dekadal filename format', anchor = 'w', justify = 'left')
	en.inrfeff <- tkentry(frRFE, textvariable = inrfeff, width = largeur)

	######
	tkconfigure(bt.dir.rfe, command = function(){
		dir4rfe <- tk_choose.dir(GeneralParameters$IO.files$DEK.dir, "")
		tclvalue(dir.rfe) <- if(!is.na(dir4rfe)) dir4rfe else ""
	})

	tkconfigure(bt.grdrfe, command = function(){
		nc.opfiles <- getOpenNetcdf(tt, all.opfiles, initialdir = tclvalue(dir.rfe))
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grdrfe) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.grdrfe, values = unlist(listOpenFiles), textvariable = file.grdrfe)
		}else return(NULL)
	})

	######

	tkgrid(txt.dir.rfe, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.dir.rfe, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.rfe, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.grdrfe, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.grdrfe, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.grdrfe, row = 3, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.inrfeff, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.inrfeff, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.dir.rfe, 'Enter the full path to directory containing\nthe merged dekadal files')
	status.bar.display(en.dir.rfe, TextOutputVar, 'Enter the full path to directory containing the merged dekadal files')
	infobulle(bt.dir.rfe, 'or browse here')
	status.bar.display(bt.dir.rfe, TextOutputVar, 'or browse here')
	infobulle(cb.grdrfe, 'Choose the file in the list')
	status.bar.display(cb.grdrfe, TextOutputVar, 'File containing a sample of merged dekadal data in netcdf')
	infobulle(bt.grdrfe, 'Browse file if not listed')
	status.bar.display(bt.grdrfe, TextOutputVar, 'Browse file if not listed')
	infobulle(en.inrfeff, 'Enter the format of the merged dekadal files names in NetCDF,\nexample: rfe1983_01-dk2.nc or rr_adj_%s%s%s.nc')
	status.bar.display(en.inrfeff, TextOutputVar, 'Enter the format of the merged dekadal files names in NetCDF,\nexample: rfe1983_01-dk2.nc or rr_adj_%s%s%s.nc')

	################################

	tkgrid(frRFE, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	################################

	bt.prm.OK <- tkbutton(frGrd1, text=" OK ")
	bt.prm.CA <- tkbutton(frGrd1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(dir.rfe)) == "" | str_trim(tclvalue(dir.rfe)) == "NA"){
			tkmessageBox(message = "Choose or enter the  directory containing the merged dekadal files", icon = "warning", type = "ok")
			tkwait.window(tt1)
		}else if(str_trim(tclvalue(file.grdrfe)) == ""){
			tkmessageBox(message = "You have to provide a sample file", icon = "warning", type = "ok")
			tkwait.window(tt1)
		}else{
			GeneralParameters$IO.files$DEK.dir <<- str_trim(tclvalue(dir.rfe))
			GeneralParameters$IO.files$DEK.file <<- str_trim(tclvalue(file.grdrfe))
			GeneralParameters$FileFormat$DEK.File.Format <<- str_trim(tclvalue(inrfeff))

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

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 5, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################

	tkgrid(frGrd0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frGrd1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'Merged dekadal data')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(GeneralParameters)
}

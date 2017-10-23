
Validation.LOOCV.PanelCmd <- function(clim.var){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(30)
		hscrlwin <- h.scale(45.5)
		wttkcombo <- as.integer(w.scale(28)/sfont0)
		largeur <- as.integer(w.scale(27)/sfont0)
		largeur1 <- as.integer(w.scale(29)/sfont0)
		largeur2 <- 28
		largeur3 <- 28
		largeur4 <- 24
		largeur5 <- as.integer(w.scale(25)/sfont0)
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(48)
		wttkcombo <- as.integer(w.scale(26)/sfont0)
		largeur <- as.integer(w.scale(22)/sfont0)
		largeur1 <- as.integer(w.scale(23)/sfont0)
		largeur2 <- 22
		largeur3 <- 20
		largeur4 <- 17
		largeur5 <- as.integer(w.scale(24)/sfont0)
	}

	GeneralParameters <- fromJSON(file.path(apps.dir, 'init_params', 'Validation_LOOCV.json'))
	MOIS <- format(ISOdate(2014, 1:12, 1), "%b")

	CHXSTATS0 <- c('Correlation', 'Nash-Sutcliffe Efficiency', 'Bias', 'Mean Absolute Error', 'Mean Error', 'Root Mean Square Error')
	CHXSTATS1 <- c('Probability Of Detection', 'False Alarm Ratio', 'Frequency Bias', 'Critical Success Index', 'Heidke Skill Score')
	CHXSTATS2 <- c('Volumetric Hit Index', 'Quantile Probability of Detection', 'Volumetric False Alarm Ratio',
					'Quantile False Alarm Ratio', 'Volumetric Miss Index', 'Volumetric Critical Success Index',
					'Quantile Critical Success Index')
	CHXSTATS <- c(CHXSTATS0, CHXSTATS1, CHXSTATS2)

	##############
	EnvZoomPars$xx1 <- tclVar()
	EnvZoomPars$xx2 <- tclVar()
	EnvZoomPars$yy1 <- tclVar()
	EnvZoomPars$yy2 <- tclVar()

	EnvZoomPars$pressButP <- tclVar(0)
	EnvZoomPars$pressButM <- tclVar(0)
	EnvZoomPars$pressButRect <- tclVar(0)
	EnvZoomPars$pressButDrag <- tclVar(0)

	EnvLOOCValidationplot$pressGetCoords <- tclVar(0)

	ZoomXYval0 <- NULL

	notebookTab <- NULL

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Merging 1")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Merging 2")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Validation")
	cmd.tab4 <- bwAddTab(tknote.cmd, text = "Plot")

	bwRaiseTab(tknote.cmd, cmd.tab1)
	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab3, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab4, 0, weight = 1)

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

	##############################################

	frInputData <- ttklabelframe(subfr1, text = "Input data", relief = 'groove')

	file.period <- tclVar()
	CbperiodVAL <- c('Daily data', 'Pentad data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(GeneralParameters$Tstep, 
									'daily' = CbperiodVAL[1], 
									'pentad' = cb.periodVAL[2],
									'dekadal' = CbperiodVAL[3],
									'monthly' = CbperiodVAL[4])
	file.stnfl <- tclVar(GeneralParameters$STN.file)
	dir.NCDF <- tclVar(GeneralParameters$NCDF$dir)


	cb.tstep <- ttkcombobox(frInputData, values = CbperiodVAL, textvariable = file.period)
	txt.stnfl <- tklabel(frInputData, text = 'Station data file', anchor = 'w', justify = 'left')
	cb.stnfl <- ttkcombobox(frInputData, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur)
	bt.stnfl <- tkbutton(frInputData, text = "...")

	txt.NCDF <- tklabel(frInputData, text = 'Directory of NetCDF files', anchor = 'w', justify = 'left')
	set.NCDF <- tkbutton(frInputData, text = "Settings")
	en.NCDF <- tkentry(frInputData, textvariable = dir.NCDF, width = largeur1)
	bt.NCDF <- tkbutton(frInputData, text = "...")

	#######################

	tkconfigure(bt.stnfl, command = function(){
		dat.opfiles <- getOpenFiles(main.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.stnfl) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
			tkconfigure(cb.adddem, values = unlist(listOpenFiles), textvariable = file.grddem1)
			tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = file.plotShp)
		}else return(NULL)
	})

	tkconfigure(set.NCDF, command = function(){
		GeneralParameters[["NCDF"]] <<- getInfoNetcdfData(main.win, GeneralParameters[["NCDF"]], str_trim(tclvalue(dir.NCDF)), tclvalue(file.period))
	})

	tkconfigure(bt.NCDF, command = function(){
		dirnc <- tk_choose.dir(getwd(), "")
		tclvalue(dir.NCDF) <- if(!is.na(dirnc)) dirnc else ""
	})

	#######################

	tkgrid(cb.tstep, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 5, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(txt.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	tkgrid(txt.NCDF, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.NCDF, row = 3, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.NCDF, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.NCDF, row = 4, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.tstep, 'Select the time step of the data')
	status.bar.display(cb.tstep, TextOutputVar, 'Select the time step of the data')
	infobulle(cb.stnfl, 'Select the station data from the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'Select the file containing the station data in CDT format')
	infobulle(bt.stnfl, 'Browse file if not listed')
	status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')

	infobulle(en.NCDF, 'Enter the full path to the directory containing the NetCDF files')
	status.bar.display(en.NCDF, TextOutputVar, 'Enter the full path to the directory containing the NetCDF files')
	infobulle(bt.NCDF, 'Or browse here')
	status.bar.display(bt.NCDF, TextOutputVar, 'Or browse here')
	infobulle(set.NCDF, 'Setting netcdf data options')
	status.bar.display(set.NCDF, TextOutputVar, 'Setting netcdf data options')

	#######################

	tkbind(cb.tstep, "<<ComboboxSelected>>", function(){
		tclvalue(day.txtVar) <- switch(tclvalue(file.period), 'Dekadal data' = 'Dek', 'Pentad data' = 'Pen', 'Day')
		statedate <- if(tclvalue(file.period) == 'Monthly data') 'disabled' else 'normal'
		tkconfigure(en.day1, state = statedate)
		tkconfigure(en.day2, state = statedate)

		AGGREGFUN <- c("mean", "sum", "count")
		if(tclvalue(aggr.data) == "0"){
			stateo0a <- "disabled"
			stateo0b <- "disabled"
			stateo1 <- "disabled"
			stateo2 <- "disabled"
		}else{
			if(tclvalue(file.period) != 'Daily data'){
				AGGREGFUN <- AGGREGFUN[-3]
				tclvalue(aggr.fun) <- if(tclvalue(aggr.fun) == "count") "sum" else tclvalue(aggr.fun)
			}
			stateo0a <- "readonly"
			stateo0b <- "normal"
			stateo1 <- if(tclvalue(aggr.fun) == "count") "readonly" else "disabled"
			stateo2 <- if(tclvalue(aggr.fun) == "count") "normal" else "disabled"
		}

		tkconfigure(cb.aggfun, values = AGGREGFUN, state = stateo0a)
		# tkconfigure(en.minfrac, state = stateo0b)
		tkconfigure(cb.opfun, state = stateo1)
		tkconfigure(en.opthres, state = stateo2)
		CHXSTATS <- c('Correlation', 'Nash-Sutcliffe Efficiency', 'Bias', 'Mean Absolute Error', 'Root Mean Square Error')
		CHXSTATS1 <- c('Probability Of Detection', 'False Alarm Ratio', 'Frequency Bias', 'Critical Success Index', 'Heidke Skill Score')
		CHXSTATS <- c(CHXSTATS, CHXSTATS1)
		tkconfigure(cb.stats.maps, values = CHXSTATS)
	})

	##############################################

	frMrg <- ttklabelframe(subfr1, text = "Merging", relief = 'groove')

	cb.MrgMthd <- c("Regression Kriging", "Spatio-Temporal LM", "Simple Bias Adjustment")
	mrg.method <- tclVar(str_trim(GeneralParameters$Merging$mrg.method))

	txt.mrg <- tklabel(frMrg, text = 'Merging method', anchor = 'w', justify = 'left')
	cb.mrg <- ttkcombobox(frMrg, values = cb.MrgMthd, textvariable = mrg.method, width = largeur4)
	bt.mrg.interp <- ttkbutton(frMrg, text = "Merging Interpolations Parameters")

	tkconfigure(bt.mrg.interp, command = function(){
		GeneralParameters[["Merging"]] <<- getInterpolationPars(main.win, GeneralParameters[["Merging"]], interpChoix = 0)
	})

	tkgrid(txt.mrg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.mrg, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.mrg.interp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 5, ipadx = 1, ipady = 1)

	infobulle(cb.mrg, 'Method to be used to perform merging')
	status.bar.display(cb.mrg, TextOutputVar, 'Method to be used to perform merging')

	infobulle(bt.mrg.interp, 'Set parameters for interpolation')
	status.bar.display(bt.mrg.interp, TextOutputVar, 'Set parameters for interpolation')

	###############

	tkbind(cb.mrg, "<<ComboboxSelected>>", function(){
		stateLMCoef <- if(tclvalue(mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'
		tkconfigure(en.LMCoef.dir, state = stateLMCoef)
		tkconfigure(bt.LMCoef.dir, state = stateLMCoef)
	})

	##############################################

	frLMCoef <- ttklabelframe(subfr1, text = "Directory of LMCoef files", relief = 'groove')

	LMCoef.dir <- tclVar(GeneralParameters$LMCOEF$dir.LMCoef)

	stateLMCoef <- if(str_trim(GeneralParameters$Merging$mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'

	en.LMCoef.dir <- tkentry(frLMCoef, textvariable = LMCoef.dir, state = stateLMCoef, width = largeur1)
	bt.LMCoef.dir <- tkbutton(frLMCoef, text = "...", state = stateLMCoef)

	tkconfigure(bt.LMCoef.dir, command = function(){
		dirLM <- tk_choose.dir(getwd(), "")
		tclvalue(LMCoef.dir) <- if(!is.na(dirLM)) dirLM else ""
	})

	tkgrid(en.LMCoef.dir, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.LMCoef.dir, row = 0, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(en.LMCoef.dir, 'Enter the full path to directory containing the LM coefficients files')
	status.bar.display(en.LMCoef.dir, TextOutputVar, 'Enter the full path to directory containing the LM coefficients files')
	infobulle(bt.LMCoef.dir, 'or browse here')
	status.bar.display(bt.LMCoef.dir, TextOutputVar, 'or browse here')

	##############################################

	frauxvar <- ttklabelframe(subfr1, text = 'Include auxiliary variables', relief = 'groove')

	dem.auxvar <- tclVar(GeneralParameters$auxvar$dem)
	slope.auxvar <- tclVar(GeneralParameters$auxvar$slope)
	aspect.auxvar <- tclVar(GeneralParameters$auxvar$aspect)
	lon.auxvar <- tclVar(GeneralParameters$auxvar$lon)
	lat.auxvar <- tclVar(GeneralParameters$auxvar$lat)

	dem.chk.auxvar <- tkcheckbutton(frauxvar, variable = dem.auxvar, text = 'DEM', anchor = 'w', justify = 'left')
	slope.chk.auxvar <- tkcheckbutton(frauxvar, variable = slope.auxvar, text = 'Slope', anchor = 'w', justify = 'left')
	aspect.chk.auxvar <- tkcheckbutton(frauxvar, variable = aspect.auxvar, text = 'Aspect', anchor = 'w', justify = 'left')
	lon.chk.auxvar <- tkcheckbutton(frauxvar, variable = lon.auxvar, text = 'Lon', anchor = 'w', justify = 'left')
	lat.chk.auxvar <- tkcheckbutton(frauxvar, variable = lat.auxvar, text = 'Lat', anchor = 'w', justify = 'left')

	tkgrid(dem.chk.auxvar, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 0, ipady = 1)
	tkgrid(slope.chk.auxvar, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 0, ipady = 1)
	tkgrid(aspect.chk.auxvar, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 0, ipady = 1)
	tkgrid(lon.chk.auxvar, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 0, ipady = 1)
	tkgrid(lat.chk.auxvar, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 0, ipady = 1)

	infobulle(dem.chk.auxvar, 'Include elevation data as auxiliary variable')
	status.bar.display(dem.chk.auxvar, TextOutputVar, 'Include elevation data as auxiliary variable')
	infobulle(slope.chk.auxvar, 'Include slope data as auxiliary variable')
	status.bar.display(slope.chk.auxvar, TextOutputVar, 'Include slope data as auxiliary variable')
	infobulle(aspect.chk.auxvar, 'Include aspect data as auxiliary variable')
	status.bar.display(aspect.chk.auxvar, TextOutputVar, 'Include aspect data as auxiliary variable')
	infobulle(lon.chk.auxvar, 'Include longitude as auxiliary variable')
	status.bar.display(lon.chk.auxvar, TextOutputVar, 'Include longitude as auxiliary variable')
	infobulle(lat.chk.auxvar, 'Include latitude as auxiliary variable')
	status.bar.display(lat.chk.auxvar, TextOutputVar, 'Include latitude as auxiliary variable')

	###########

	tkbind(dem.chk.auxvar, "<Button-1>", function(){
		statedem <- if(tclvalue(dem.auxvar) == "0" | tclvalue(slope.auxvar) == "1"  |
						tclvalue(aspect.auxvar) == "1") 'normal' else 'disabled'
		tkconfigure(cb.grddem, state = statedem)
		tkconfigure(bt.grddem, state = statedem)
	})

	tkbind(slope.chk.auxvar, "<Button-1>", function(){
		statedem <- if(tclvalue(slope.auxvar) == "0" | tclvalue(dem.auxvar) == "1" |
						tclvalue(aspect.auxvar) == "1") 'normal' else 'disabled'
		tkconfigure(cb.grddem, state = statedem)
		tkconfigure(bt.grddem, state = statedem)
	})

	tkbind(aspect.chk.auxvar, "<Button-1>", function(){
		statedem <- if(tclvalue(aspect.auxvar) == "0" | tclvalue(slope.auxvar) == "1" |
						 tclvalue(dem.auxvar) == "1") 'normal' else 'disabled'
		tkconfigure(cb.grddem, state = statedem)
		tkconfigure(bt.grddem, state = statedem)
	})

	#############################
	tkgrid(frInputData, row = 0, column = 0, sticky = 'we')
	tkgrid(frMrg, row = 1, column = 0, sticky = 'we', pady = 1)
	tkgrid(frLMCoef, row = 2, column = 0, sticky = 'we', pady = 1)
	tkgrid(frauxvar, row = 3, column = 0, sticky = 'we', pady = 1)

	#######################################################################################################

	#Tab2
	frTab2 <- tkframe(cmd.tab2)
	tkgrid(frTab2, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab2, 0, weight = 1)

	scrw2 <- bwScrolledWindow(frTab2)
	tkgrid(scrw2)
	tkgrid.columnconfigure(scrw2, 0, weight = 1)
	subfr2 <- bwScrollableFrame(scrw2, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr2, 0, weight = 1)

	##############################################

	frMrgPars <- ttklabelframe(subfr2, text = "Merging Parameters", relief = 'groove')

	mrg.min.stn <- tclVar(GeneralParameters$Merging$min.stn)
	mrg.min.non.zero <- tclVar(GeneralParameters$Merging$min.non.zero)
	use.RnoR <- tclVar(GeneralParameters$RnoR$use.RnoR)
	maxdist.RnoR <- tclVar(GeneralParameters$RnoR$maxdist.RnoR)

	if(clim.var == 'RR') stateClimVar <- "normal"
	if(clim.var == 'TT') stateClimVar <- "disabled"
	stateRnoR <- if(GeneralParameters$RnoR$use.RnoR & clim.var == 'RR') 'normal' else 'disabled'

	txt.min.nbrs.stn <- tklabel(frMrgPars, text = 'Min.Nb.Stn', anchor = 'e', justify = 'right')
	en.min.nbrs.stn <- tkentry(frMrgPars, width = 4, textvariable = mrg.min.stn, justify = 'right')
	txt.min.non.zero <- tklabel(frMrgPars, text = 'Min.No.Zero', anchor = 'e', justify = 'right')
	en.min.non.zero <- tkentry(frMrgPars, width = 4, textvariable = mrg.min.non.zero, justify = 'right', state = stateClimVar)

	chk.use.rnr <- tkcheckbutton(frMrgPars, variable = use.RnoR, text = 'Apply Rain-no-Rain mask', width = largeur5, anchor = 'w', justify = 'left', state = stateClimVar)
	txt.maxdist.rnr <- tklabel(frMrgPars, text = 'maxdist.RnoR', anchor = 'e', justify = 'right')
	en.maxdist.rnr <- tkentry(frMrgPars, width = 4, textvariable = maxdist.RnoR, justify = 'right', state = stateRnoR)

	tkgrid(txt.min.nbrs.stn, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 0)
	tkgrid(en.min.nbrs.stn, row = 0, column = 2, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 0)
	tkgrid(txt.min.non.zero, row = 0, column = 3, sticky = 'e', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 0)
	tkgrid(en.min.non.zero, row = 0, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 0)

	tkgrid(chk.use.rnr, row = 1, column = 0, sticky = 'ew', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.maxdist.rnr, row = 2, column = 3, sticky = 'e', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 0)
	tkgrid(en.maxdist.rnr, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 0)


	infobulle(en.min.nbrs.stn, 'Minimum number of gauges with data to be used to do the merging')
	status.bar.display(en.min.nbrs.stn, TextOutputVar, 'Minimum number of gauges with data to be used to do the merging')
	infobulle(en.min.non.zero, 'Minimum number of non-zero gauge values to perform the merging')
	status.bar.display(en.min.non.zero, TextOutputVar, 'Minimum number of non-zero gauge values to perform the merging')

	infobulle(chk.use.rnr, 'Check this box to apply a mask over no rain area')
	status.bar.display(chk.use.rnr, TextOutputVar, 'Check this box to apply a mask over no rain area')
	infobulle(en.maxdist.rnr, 'Maximum distance (in decimal degrees) to be used to interpolate Rain-noRain mask')
	status.bar.display(en.maxdist.rnr, TextOutputVar, 'Maximum distance (in decimal degrees) to be used to interpolate Rain-noRain mask')

	###############
	tkbind(chk.use.rnr, "<Button-1>", function(){
		stateRnoR <- if(tclvalue(use.RnoR) == '0' & clim.var == 'RR') 'normal' else 'disabled'
		tkconfigure(en.maxdist.rnr, state = stateRnoR)
	})

	##############################################

	frtxtDate <- ttklabelframe(subfr2, text = "Merging Date Range", relief = 'groove')

	istart.yrs <- tclVar(GeneralParameters$Merging.Date$start.year)
	istart.mon <- tclVar(GeneralParameters$Merging.Date$start.mon)
	istart.day <- tclVar(GeneralParameters$Merging.Date$start.dek)
	iend.yrs <- tclVar(GeneralParameters$Merging.Date$end.year)
	iend.mon <- tclVar(GeneralParameters$Merging.Date$end.mon)
	iend.day <- tclVar(GeneralParameters$Merging.Date$end.dek)

	txtdek <- switch(GeneralParameters$Tstep, 'dekadal' = 'Dek', 'pentad' = 'Pen', 'Day')
	day.txtVar <- tclVar(txtdek)
	statedate <- if(GeneralParameters$Tstep == 'monthly') 'disabled' else 'normal'

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

	infobulle(frtxtDate, 'Start and end date to perform the merging')
	status.bar.display(frtxtDate, TextOutputVar, 'Start and end date to perform the merging')

	##############################################

	frDEM <- ttklabelframe(subfr2, text = "Elevation data (NetCDF)", relief = 'groove')

	file.grddem <- tclVar(GeneralParameters$DEM.file)

	statedem <- if(GeneralParameters$auxvar$dem |
					GeneralParameters$auxvar$slope |
					GeneralParameters$auxvar$aspect) 'normal' else 'disabled'

	cb.grddem <- ttkcombobox(frDEM, values = unlist(listOpenFiles), textvariable = file.grddem, state = statedem, width = largeur)
	bt.grddem <- tkbutton(frDEM, text = "...", state = statedem)

	tkconfigure(bt.grddem, command = function(){
		nc.opfiles <- getOpenNetcdf(main.win, all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grddem) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
			tkconfigure(cb.adddem, values = unlist(listOpenFiles), textvariable = file.grddem1)
			tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = file.plotShp)
		}else return(NULL)
	})

	tkgrid(cb.grddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.grddem, row = 0, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.grddem, 'Choose the file in the list')
	status.bar.display(cb.grddem, TextOutputVar, 'File containing the elevation data in netcdf')
	infobulle(bt.grddem, 'Browse file if not listed')
	status.bar.display(bt.grddem, TextOutputVar, 'Browse file if not listed')

	##############################################

	frameDirSav <- ttklabelframe(subfr2, text = "Directory to save result", relief = 'groove')

	file.save1 <- tclVar(GeneralParameters$outdir)

	en.dir.save <- tkentry(frameDirSav, textvariable = file.save1, width = largeur1)
	bt.dir.save <- tkbutton(frameDirSav, text = "...")

	tkconfigure(bt.dir.save, command = function() fileORdir2Save(file.save1, isFile = FALSE))

	###########

	tkgrid(en.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.save, row = 0, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(en.dir.save, 'Enter the full path to the directory  to save result')
	status.bar.display(en.dir.save, TextOutputVar, 'Enter the full path to the directory to save result')
	infobulle(bt.dir.save, 'Browse here the full path to the directory to save result')
	status.bar.display(bt.dir.save, TextOutputVar, 'Browse here the full path to the directory to save result')

	##############################################

	if(!is.null(EnvLOOCValidationplot$loocv)){
		stateBTCV <- if(tclvalue(EnvLOOCValidationplot$loocv) == "1") "normal" else "disabled"
	}else stateBTCV <- "normal"
	
	bt.cross.valid <- ttkbutton(subfr2, text = "Leave-One-Out Cross-Validation", state = stateBTCV)

	tkconfigure(bt.cross.valid, command = function(){
		GeneralParameters$clim.var <- clim.var
		GeneralParameters$Tstep <- switch(tclvalue(file.period),
		 									'Daily data' = 'daily',
		 									'Pentad data' = 'pentad',
											'Dekadal data' =  'dekadal',
											'Monthly data' = 'monthly')
		GeneralParameters$STN.file <- str_trim(tclvalue(file.stnfl))
		GeneralParameters$NCDF$dir <- str_trim(tclvalue(dir.NCDF))

		GeneralParameters$Merging$mrg.method <- str_trim(tclvalue(mrg.method))
		GeneralParameters$LMCOEF$dir.LMCoef <- str_trim(tclvalue(LMCoef.dir))

		GeneralParameters$Merging$min.stn <- as.numeric(str_trim(tclvalue(mrg.min.stn)))
		GeneralParameters$Merging$min.non.zero <- as.numeric(str_trim(tclvalue(mrg.min.non.zero)))

		GeneralParameters$RnoR$use.RnoR <- switch(tclvalue(use.RnoR), '0' = FALSE, '1' = TRUE)
		GeneralParameters$RnoR$maxdist.RnoR <- as.numeric(str_trim(tclvalue(maxdist.RnoR)))

		GeneralParameters$DEM.file <- str_trim(tclvalue(file.grddem))
		GeneralParameters$outdir <- str_trim(tclvalue(file.save1))

		GeneralParameters$auxvar$dem <- switch(tclvalue(dem.auxvar), '0' = FALSE, '1' = TRUE)
		GeneralParameters$auxvar$slope <- switch(tclvalue(slope.auxvar), '0' = FALSE, '1' = TRUE)
		GeneralParameters$auxvar$aspect <- switch(tclvalue(aspect.auxvar), '0' = FALSE, '1' = TRUE)
		GeneralParameters$auxvar$lon <- switch(tclvalue(lon.auxvar), '0' = FALSE, '1' = TRUE)
		GeneralParameters$auxvar$lat <- switch(tclvalue(lat.auxvar), '0' = FALSE, '1' = TRUE)

		GeneralParameters$Merging.Date$start.year <- as.numeric(str_trim(tclvalue(istart.yrs)))
		GeneralParameters$Merging.Date$start.mon <- as.numeric(str_trim(tclvalue(istart.mon)))
		GeneralParameters$Merging.Date$start.dek <- as.numeric(str_trim(tclvalue(istart.day)))
		GeneralParameters$Merging.Date$end.year <- as.numeric(str_trim(tclvalue(iend.yrs)))
		GeneralParameters$Merging.Date$end.mon <- as.numeric(str_trim(tclvalue(iend.mon)))
		GeneralParameters$Merging.Date$end.dek <- as.numeric(str_trim(tclvalue(iend.day)))

		# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

		tkconfigure(main.win, cursor = 'watch')
		InsertMessagesTxt(main.txt.out, "Validation .................")
		tcl('update')
		ret <- tryCatch(
			LOOCV_MergingDataExec(GeneralParameters),
			#warning = function(w) warningFun(w),
			error = function(e){
				 errorFun(e)
			},
			finally = {
				tkconfigure(main.win, cursor = '')
			}
		)

		msg0 <- "Calculation of the data for validation finished successfully"
		msg1 <- "Cross-Validation failed"
		if(!is.null(ret)){
			if(ret == 0){
				InsertMessagesTxt(main.txt.out, msg0)
			}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
		}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
	})

	#############################
	tkgrid(frMrgPars, row = 0, column = 0, sticky = 'we')
	tkgrid(frtxtDate, row = 1, column = 0, sticky = '', pady = 1)
	tkgrid(frDEM, row = 2, column = 0, sticky = 'we', pady = 1)
	tkgrid(frameDirSav, row = 3, column = 0, sticky = 'we', pady = 3)
	tkgrid(bt.cross.valid, row = 4, column = 0, sticky = 'we', pady = 3)


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

	frameLOOCV <- ttklabelframe(subfr3, text = "Leave-One-Out Cross-Validation data", relief = 'groove')

	EnvLOOCValidationplot$loocv <- tclVar(0)
	file.loocv <- tclVar()

	stateLOOCV <- if(tclvalue(EnvLOOCValidationplot$loocv) == "1") "normal" else "disabled"

	chk.loocv <- tkcheckbutton(frameLOOCV, variable = EnvLOOCValidationplot$loocv, text = "LOOCV already performed", anchor = 'w', justify = 'left')
	en.loocv <- tkentry(frameLOOCV, textvariable = file.loocv, width = largeur1, state = stateLOOCV)
	bt.loocv <- tkbutton(frameLOOCV, text = "...", state = stateLOOCV)

	tkconfigure(bt.loocv, command = function(){
		filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
		path.loocv <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
		if(path.loocv == "") return(NULL)
		tclvalue(file.loocv) <- path.loocv

		if(file.exists(tclvalue(file.loocv))){
			loocv.data <- try(readRDS(tclvalue(file.loocv)), silent = TRUE)
			if(inherits(loocv.data, "try-error")){
				InsertMessagesTxt(main.txt.out, 'Unable to load Leave-One-Out Cross-Validation data', format = TRUE)
				InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', loocv.data[1]), format = TRUE)
				return(NULL)
			}
			EnvLOOCValidation <<- loocv.data
			EnvLOOCValidationplot$file.loocv <- tclvalue(file.loocv)
		} 
	})

	tkgrid(chk.loocv, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.loocv, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.loocv, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	###############
	tkbind(chk.loocv, "<Button-1>", function(){
		stateLOOCV <- if(tclvalue(EnvLOOCValidationplot$loocv) == '1') 'disabled' else 'normal'
		tkconfigure(en.loocv, state = stateLOOCV)
		tkconfigure(bt.loocv, state = stateLOOCV)
		stateBTCV <- if(tclvalue(EnvLOOCValidationplot$loocv) == '1') 'normal' else 'disabled'
		tkconfigure(bt.cross.valid, state = stateBTCV)
	})

	##############################################

	frameSeason <- ttklabelframe(subfr3, text = "Season & Period", relief = 'groove')

	mon1 <- as.numeric(str_trim(GeneralParameters$date.range$start.month))
	mon2 <- as.numeric(str_trim(GeneralParameters$date.range$end.month))
	start.mois <- tclVar(MOIS[mon1])
	end.mois <- tclVar(MOIS[mon2])
	start.year <- tclVar(GeneralParameters$date.range$start.year)
	end.year <- tclVar(GeneralParameters$date.range$end.year)

	fr.seas <- ttklabelframe(frameSeason, text = 'Season', relief = 'sunken', labelanchor = "n", borderwidth = 2)
	fr.year <- ttklabelframe(frameSeason, text = 'Years', relief = 'sunken', labelanchor = "n", borderwidth = 2)

	txt.to1 <- tklabel(fr.year, text = '-to-')
	en.years1 <- tkentry(fr.year, width = 5, textvariable = start.year, justify = 'right')
	en.years2 <- tkentry(fr.year, width = 5, textvariable = end.year, justify = 'right')

	txt.to2 <- tklabel(fr.seas, text = '-to-')
	cb.month1 <- ttkcombobox(fr.seas, values = MOIS, textvariable = start.mois, width = 4)
	cb.month2 <- ttkcombobox(fr.seas, values = MOIS, textvariable = end.mois, width = 4)

	tkgrid(en.years1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(txt.to1, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(en.years2, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

	tkgrid(cb.month1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(txt.to2, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(cb.month2, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

	tkgrid(fr.seas, row = 0, column = 0, sticky = 'ns', rowspan = 1, columnspan = 1, padx = 3, pady = 1)
	tkgrid(fr.year, row = 0, column = 1, sticky = 'ns', rowspan = 1, columnspan = 1, padx = 3, pady = 1)

	infobulle(en.years1, 'Start year of the period to calculate the statistics')
	status.bar.display(en.years1, TextOutputVar, 'Start year of the period to calculate the statistics')
	infobulle(en.years2, 'End year of the period to calculate the statistics')
	status.bar.display(en.years2, TextOutputVar, 'End year of the period to calculate the statistics')
	infobulle(cb.month1, 'Start month of the period to calculate the statistics')
	status.bar.display(cb.month1, TextOutputVar, 'Start month of the period to calculate the statistics')
	infobulle(cb.month2, 'End month of the season to calculate the statistics')
	status.bar.display(cb.month2, TextOutputVar, 'End month of the season to calculate the statistics')

	##############################################

	frameAggr <- ttklabelframe(subfr3, text = "Data aggregation", relief = 'groove')

	aggr.data <- tclVar(GeneralParameters$aggr.series$aggr.data)
	aggr.fun <- tclVar(GeneralParameters$aggr.series$aggr.fun)
	# min.frac <- tclVar(GeneralParameters$aggr.series$min.frac)
	opr.fun <- tclVar(GeneralParameters$aggr.series$opr.fun)
	opr.thres <- tclVar(GeneralParameters$aggr.series$opr.thres)

	AGGREGFUN <- c("mean", "sum", "count")
	if(GeneralParameters$Tstep != 'daily' & !GeneralParameters$aggr.series$aggr.data) AGGREGFUN <- AGGREGFUN[-3]
	if(!GeneralParameters$aggr.series$aggr.data){
		stateo0a <- 'disabled'
		stateo0b <- 'disabled'
		stateo1 <- 'disabled'
		stateo2 <- 'disabled'
	}else{
		stateo0a <- 'readonly'
		stateo0b <- 'normal'
		stateo1 <- if(str_trim(GeneralParameters$aggr.series$aggr.fun) == "count") 'readonly' else 'disabled'
		stateo2 <- if(str_trim(GeneralParameters$aggr.series$aggr.fun) == "count") 'normal' else 'disabled'
	}

	chk.aggrdata <- tkcheckbutton(frameAggr, variable = aggr.data, text = "Aggregate data", anchor = 'w', justify = 'left')
	txt.aggfun <- tklabel(frameAggr, text = 'Function', anchor = 'w', justify = 'left')
	cb.aggfun <- ttkcombobox(frameAggr, values = AGGREGFUN, textvariable = aggr.fun, width = 6, state = stateo0a)
	# txt.minfrac <- tklabel(frameAggr, text = 'Min.Frac', anchor = 'w', justify = 'left')
	# en.minfrac <- tkentry(frameAggr, textvariable = min.frac, width = 6, state = stateo0b)
	txt.opfun <- tklabel(frameAggr, text = 'Operator', anchor = 'w', justify = 'left')
	cb.opfun <- ttkcombobox(frameAggr, values = c(">=", ">", "<=", "<"), textvariable = opr.fun, width = 6, state = stateo1)
	txt.opthres <- tklabel(frameAggr, text = 'Threshold', anchor = 'w', justify = 'left')
	en.opthres <- tkentry(frameAggr, textvariable = opr.thres, width = 6, state = stateo2)

	tkgrid(chk.aggrdata, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.aggfun, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.aggfun, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	# tkgrid(txt.minfrac, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	# tkgrid(en.minfrac, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.opfun, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.opfun, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.opthres, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.opthres, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.aggfun, 'Function that have to be applied for aggregating from daily/dekadal/monthly into\na higher time step (e.g., for precipitation FUN=sum and for temperature FUN=mean)')
	status.bar.display(cb.aggfun, TextOutputVar, 'Function that have to be applied for aggregating from daily/dekadal/monthly into\na higher time step (e.g., for precipitation FUN=sum and for temperature FUN=mean)')
	# infobulle(en.minfrac, 'Minimum fraction of available data that must be present within each output time step')
	# status.bar.display(en.minfrac, TextOutputVar, 'Minimum fraction of available data that must be present within each output time step')
	infobulle(cb.opfun, 'Select the comparison operator to be used to match event')
	status.bar.display(cb.opfun, TextOutputVar, 'Select the comparison operator to be used to match event')
	infobulle(en.opthres, 'User defined threshold applied to count event')
	status.bar.display(en.opthres, TextOutputVar, 'User defined threshold applied to count event')

	#################
	tkbind(cb.aggfun, "<<ComboboxSelected>>", function(){
		stateo1 <- if(tclvalue(aggr.fun) == "count") "readonly" else "disabled"
		stateo2 <- if(tclvalue(aggr.fun) == "count") "normal" else "disabled"
		tkconfigure(cb.opfun, state = stateo1)
		tkconfigure(en.opthres, state = stateo2)
	})

	tkbind(chk.aggrdata, "<Button-1>", function(){
		if(tclvalue(aggr.data) == "1"){
			stateo0a <- 'disabled'
			stateo0b <- 'disabled'
			stateo1 <- 'disabled'
			stateo2 <- 'disabled'
		}else{
			stateo0a <- 'readonly'
			stateo0b <- 'normal'
			stateo1 <- if(tclvalue(aggr.fun) == "count") 'readonly' else 'disabled'
			stateo2 <- if(tclvalue(aggr.fun) == "count") 'normal' else 'disabled'
		}

		tkconfigure(cb.aggfun, state = stateo0a)
		# tkconfigure(en.minfrac, state = stateo0b)
		tkconfigure(cb.opfun, state = stateo1)
		tkconfigure(en.opthres, state = stateo2)
		tkconfigure(cb.stats.maps, values = CHXSTATS)
	})

	##############################################

	STATDATATYPE <- c('All Data', 'Spatial Average', 'Per station')
	stat.data <- tclVar()
	tclvalue(stat.data) <- switch(GeneralParameters$stat.data, 
									'all' = STATDATATYPE[1], 
									'avg' = STATDATATYPE[2],
									'stn' = STATDATATYPE[3])

	cb.stat.data <- ttkcombobox(subfr3, values = STATDATATYPE, textvariable = stat.data, width = wttkcombo)

	infobulle(cb.stat.data, 'Use all data or a spatial average or station by station to calculate the statistics')
	status.bar.display(cb.stat.data, TextOutputVar, 'Use all data or a spatial average or station by station to calculate the statistics')

	#################
	tkbind(cb.stat.data, "<<ComboboxSelected>>", function(){
		stateDispSTN <- if(tclvalue(stat.data) == 'Per station') 'normal' else 'disabled'
		tkconfigure(bt.stat.prev, state = stateDispSTN)
		tkconfigure(cb.stat.sel, state = stateDispSTN)
		tkconfigure(bt.stat.next, state = stateDispSTN)
		stateMaps <- if(tclvalue(stat.data) == 'Per station') 'normal' else 'disabled'
		tkconfigure(cb.stats.maps, state = stateMaps)
		tkconfigure(bt.stats.maps, state = stateMaps)
		stateStnID <- if(tclvalue(stat.data) == 'Per station') 'normal' else 'disabled'
		tkconfigure(cb.stn.graph, state = stateStnID)

		TYPEGRAPH <- c("Scatter", "CDF", "Lines")
		if(tclvalue(stat.data) == 'All Data'){
			TYPEGRAPH <- c("Scatter", "CDF")
			if(tclvalue(EnvLOOCValidationplot$type.graph) == "Lines") tclvalue(EnvLOOCValidationplot$type.graph) <- "Scatter"
		}
		tkconfigure(cb.stats.graph, values = TYPEGRAPH)
	})

	##############################################

	frameDicho <- ttklabelframe(subfr3, text = "Dichotomous validation", relief = 'groove')

	if(clim.var == 'RR') trhesVal <- 1
	if(clim.var == 'TT') trhesVal <- 20
	dicho.thres <- tclVar(trhesVal)
	# dicho.thres <- tclVar(GeneralParameters$dicho.fcst$opr.thres)
	dicho.opr <- tclVar(GeneralParameters$dicho.fcst$opr.fun)

	txt.dicho <- tklabel(frameDicho, text = 'Threshold', anchor = 'w', justify = 'left')
	cb.dicho <- ttkcombobox(frameDicho, values = c(">=", ">", "<=", "<"), textvariable = dicho.opr, width = 4, state = 'readonly')
	en.dicho <- tkentry(frameDicho, textvariable = dicho.thres, width = 6)

	tkgrid(txt.dicho, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.dicho, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.dicho, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(en.dicho, 'Threshold to be specified to separate "yes" and "no" events')
	status.bar.display(en.dicho, TextOutputVar, 'Threshold to be specified to separate "yes" and "no" events')

	##############################################

	bt.calc.stat <- ttkbutton(subfr3, text = "Calculate Statistics")

	tkconfigure(bt.calc.stat, command = function(){
		GeneralParameters$date.range$start.month <- which(MOIS%in%str_trim(tclvalue(start.mois)))
		GeneralParameters$date.range$end.month <- which(MOIS%in%str_trim(tclvalue(end.mois)))
		GeneralParameters$date.range$start.year <- as.numeric(str_trim(tclvalue(start.year)))
		GeneralParameters$date.range$end.year <- as.numeric(str_trim(tclvalue(end.year)))

		GeneralParameters$aggr.series$aggr.data <- switch(tclvalue(aggr.data), '0' = FALSE, '1' = TRUE)
		GeneralParameters$aggr.series$aggr.fun <- str_trim(tclvalue(aggr.fun))
		# GeneralParameters$aggr.series$min.frac <- as.numeric(str_trim(tclvalue(min.frac)))
		GeneralParameters$aggr.series$opr.fun <- str_trim(tclvalue(opr.fun))
		GeneralParameters$aggr.series$opr.thres <- as.numeric(str_trim(tclvalue(opr.thres)))

		GeneralParameters$stat.data <- switch(tclvalue(stat.data),
											 'All Data' = 'all',
											 'Spatial Average' = 'avg',
											 'Per station' = 'stn')

		GeneralParameters$dicho.fcst$opr.thres <- as.numeric(str_trim(tclvalue(dicho.thres)))
		GeneralParameters$dicho.fcst$opr.fun <- str_trim(tclvalue(dicho.opr))

		########
		GeneralParameters$STN.file <- str_trim(tclvalue(file.stnfl))
		GeneralParameters$outdir <- str_trim(tclvalue(file.save1))

		# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

		tkconfigure(main.win, cursor = 'watch')
		InsertMessagesTxt(main.txt.out, "Validation .................")
		tcl('update')
		ret <- tryCatch(
			LOOCValidationDataProcs(GeneralParameters),
			#warning = function(w) warningFun(w),
			error = function(e){
				 errorFun(e)
			},
			finally = {
				tkconfigure(main.win, cursor = '')
			}
		)

		msg0 <- "Statistics calculation finished successfully"
		msg1 <- "Validation failed"
		if(!is.null(ret)){
			if(ret == 0){
				InsertMessagesTxt(main.txt.out, msg0)

				if(tclvalue(stat.data) == 'Per station'){
					tkconfigure(cb.stat.sel, values = EnvLOOCValidation$opDATA$id)
					tclvalue(stn.stat.tab) <- EnvLOOCValidation$opDATA$id[1]

					tkconfigure(cb.stn.graph, values = EnvLOOCValidation$opDATA$id, state = 'normal')
					tclvalue(EnvLOOCValidationplot$stnIDGraph) <- EnvLOOCValidation$opDATA$id[1]
				}
			}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
		}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
	})

	#############################
	tkgrid(frameLOOCV, row = 0, column = 0, sticky = 'we')
	tkgrid(frameSeason, row = 1, column = 0, sticky = 'we', pady = 1)
	tkgrid(frameAggr, row = 2, column = 0, sticky = 'we', pady = 1)
	tkgrid(cb.stat.data, row = 3, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameDicho, row = 4, column = 0, sticky = '', pady = 3)
	tkgrid(bt.calc.stat, row = 5, column = 0, sticky = 'we', pady = 3)

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

	frameStatTab <- ttklabelframe(subfr4, text = "Display Statistics Table", relief = 'groove')

	STATIONIDS <- ''
	stn.stat.tab <- tclVar()
	stateDispSTN <- if(GeneralParameters$stat.data == 'stn') 'normal' else 'disabled'

	bt.stat.disp <- ttkbutton(frameStatTab, text = "Display Table")
	bt.stat.prev <- ttkbutton(frameStatTab, text = "<<", state = stateDispSTN, width = 4)
	bt.stat.next <- ttkbutton(frameStatTab, text = ">>", state = stateDispSTN, width = 4)
	cb.stat.sel <- ttkcombobox(frameStatTab, values = STATIONIDS, textvariable = stn.stat.tab, width = largeur3, state = stateDispSTN)

	################
	validStatTab <- NULL

	tkconfigure(bt.stat.disp, command = function(){
		if(!is.null(EnvLOOCValidation$Statistics)){
			if(tclvalue(stat.data) == 'All Data'){
				don <- EnvLOOCValidation$Statistics$ALL
				dat2disp <- data.frame(Name = rownames(don$statistics), Statistics = don$statistics, Description = don$description)
				titleTab <- 'All-Data Statistics'
			}
			if(tclvalue(stat.data) == 'Spatial Average'){
				don <- EnvLOOCValidation$Statistics$AVG
				dat2disp <- data.frame(Name = rownames(don$statistics), Statistics = don$statistics, Description = don$description)
				titleTab <- 'Spatial-Average Statistics'
			}
			if(tclvalue(stat.data) == 'Per station'){
				don <- EnvLOOCValidation$Statistics$STN
				istn <- which(EnvLOOCValidation$opDATA$id == tclvalue(stn.stat.tab))
				dat2disp <- data.frame(Name = rownames(don$statistics), Statistics = don$statistics[, istn], Description = don$description)
				titleTab <- paste(tclvalue(stn.stat.tab), 'Statistics')
			}
			retNBTab <- tableValidationNotebookTab_unik(tknotes, dat2disp, titleTab, validStatTab, AllOpenTabType, AllOpenTabData)
			validStatTab <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	tkconfigure(bt.stat.prev, command = function(){
		if(!is.null(EnvLOOCValidation$Statistics)){
			don <- EnvLOOCValidation$Statistics$STN
			istn <- which(EnvLOOCValidation$opDATA$id == tclvalue(stn.stat.tab))
			istn <- istn-1
			if(istn < 1) istn <- length(EnvLOOCValidation$opDATA$id)
			tclvalue(stn.stat.tab) <- EnvLOOCValidation$opDATA$id[istn]
			dat2disp <- data.frame(Name = rownames(don$statistics), Statistics = don$statistics[, istn], Description = don$description)
			titleTab <- paste(tclvalue(stn.stat.tab), 'Statistics')
			retNBTab <- tableValidationNotebookTab_unik(tknotes, dat2disp, titleTab, validStatTab, AllOpenTabType, AllOpenTabData)
			validStatTab <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	tkconfigure(bt.stat.next, command = function(){
		if(!is.null(EnvLOOCValidation$Statistics)){
			don <- EnvLOOCValidation$Statistics$STN
			istn <- which(EnvLOOCValidation$opDATA$id == tclvalue(stn.stat.tab))
			istn <- istn+1
			if(istn > length(EnvLOOCValidation$opDATA$id)) istn <- 1
			tclvalue(stn.stat.tab) <- EnvLOOCValidation$opDATA$id[istn]
			dat2disp <- data.frame(Name = rownames(don$statistics), Statistics = don$statistics[, istn], Description = don$description)
			titleTab <- paste(tclvalue(stn.stat.tab), 'Statistics')
			retNBTab <- tableValidationNotebookTab_unik(tknotes, dat2disp, titleTab, validStatTab, AllOpenTabType, AllOpenTabData)
			validStatTab <<- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	tkgrid(bt.stat.disp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.stat.prev, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.stat.sel, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.stat.next, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	##############################################

	frameMap <- ttklabelframe(subfr4, text = "Statistics Maps", relief = 'groove')

	EnvLOOCValidationplot$statistics <- tclVar('Correlation')

	stateMaps <- if(GeneralParameters$stat.data == 'stn') 'normal' else 'disabled'

	cb.stats.maps <- ttkcombobox(frameMap, values = CHXSTATS, textvariable = EnvLOOCValidationplot$statistics, width = largeur2, state = stateMaps)
	bt.stats.maps <- ttkbutton(frameMap, text = "PLOT", state = stateMaps)

	EnvLOOCValidationplot$notebookTab.maps <- NULL
	tkconfigure(bt.stats.maps, command = function(){
		if(!is.null(EnvLOOCValidation$Statistics)){
			EnvLOOCValidationplot$xlim.maps <- range(EnvLOOCValidation$opDATA$lon, na.rm = TRUE)
			EnvLOOCValidationplot$ylim.maps <- range(EnvLOOCValidation$opDATA$lat, na.rm = TRUE)

			imgContainer <- LOOCValidation.DisplayStatMaps(tknotes)

			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvLOOCValidationplot$notebookTab.maps, AllOpenTabType, AllOpenTabData)
			EnvLOOCValidationplot$notebookTab.maps <- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	tkgrid(cb.stats.maps, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(bt.stats.maps, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	##############################################

	frameGraph <- ttklabelframe(subfr4, text = "Graphs", relief = 'groove')

	TYPEGRAPH <- c("Scatter", "CDF", 'Lines')
	if(GeneralParameters$stat.data == 'all') TYPEGRAPH <- c("Scatter", "CDF")

	EnvLOOCValidationplot$type.graph <- tclVar("Scatter")
	STNIDGRAPH <- ""
	EnvLOOCValidationplot$stnIDGraph <- tclVar()
	stateStnID <- "disabled"

	cb.stats.graph <- ttkcombobox(frameGraph, values = TYPEGRAPH, textvariable = EnvLOOCValidationplot$type.graph, width = largeur2)
	bt.stats.graph <- ttkbutton(frameGraph, text = "PLOT")
	txt.stn.graph <- tklabel(frameGraph, text = "Station",  anchor = 'e', justify = 'right')
	cb.stn.graph <- ttkcombobox(frameGraph, values = STNIDGRAPH, textvariable = EnvLOOCValidationplot$stnIDGraph, state = stateStnID)

	EnvLOOCValidationplot$notebookTab.graph <- NULL
	tkconfigure(bt.stats.graph, command = function(){
		if(!is.null(EnvLOOCValidation$Statistics)){
			imgContainer <- LOOCValidation.DisplayGraph(tknotes)

			retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvLOOCValidationplot$notebookTab.graph, AllOpenTabType, AllOpenTabData)
			EnvLOOCValidationplot$notebookTab.graph <- retNBTab$notebookTab
			AllOpenTabType <<- retNBTab$AllOpenTabType
			AllOpenTabData <<- retNBTab$AllOpenTabData
		}
	})

	tkgrid(cb.stats.graph, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 12, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(bt.stats.graph, row = 0, column = 12, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(txt.stn.graph, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(cb.stn.graph, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 15, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	##############################################

	frameSHP <- ttklabelframe(subfr4, text = "Boundaries", relief = 'groove')

	EnvLOOCValidationplot$add.shp <- tclVar(GeneralParameters$add.to.plot$add.shp)
	file.plotShp <- tclVar(GeneralParameters$add.to.plot$shp.file)

	stateSHP <- if(GeneralParameters$add.to.plot$add.shp) "normal" else "disabled"

	chk.addshp <- tkcheckbutton(frameSHP, variable = EnvLOOCValidationplot$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
	cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur, state = stateSHP)
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
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
			tkconfigure(cb.adddem, values = unlist(listOpenFiles), textvariable = file.grddem1)
			tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = file.plotShp)

			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile)) EnvLOOCValidationplot$shp <- NULL
			EnvLOOCValidationplot$shp <- getBoundaries(shpofile[[2]])
		}else return(NULL)
	})

	tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.addshp, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	#################
	tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
		shpofile <- getShpOpenData(file.plotShp)
		if(is.null(shpofile)) EnvLOOCValidationplot$shp <- NULL
		EnvLOOCValidationplot$shp <- getBoundaries(shpofile[[2]])
	})

	tkbind(chk.addshp, "<Button-1>", function(){
		stateSHP <- if(tclvalue(EnvLOOCValidationplot$add.shp) == "1") "disabled" else "normal"
		tkconfigure(cb.addshp, state = stateSHP)
		tkconfigure(bt.addshp, state = stateSHP)
	})


	##############################################

	frameDEM <- ttklabelframe(subfr4, text = "Elevation", relief = 'groove')

	EnvLOOCValidationplot$add.dem <- tclVar(GeneralParameters$add.to.plot$add.dem)
	file.grddem1 <- tclVar(GeneralParameters$add.to.plot$dem.file)

	stateDEM <- if(GeneralParameters$add.to.plot$add.dem) "normal" else "disabled"

	chk.adddem <- tkcheckbutton(frameDEM, variable = EnvLOOCValidationplot$add.dem, text = "Add DEM to Map", anchor = 'w', justify = 'left')
	cb.adddem <- ttkcombobox(frameDEM, values = unlist(listOpenFiles), textvariable = file.grddem1, width = largeur, state = stateDEM)
	bt.adddem <- tkbutton(frameDEM, text = "...", state = stateDEM)

	tkconfigure(bt.adddem, command = function(){
		nc.opfiles <- getOpenNetcdf(main.win, all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grddem1) <- AllOpenFilesData[[nopf+1]][[1]]

			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
			tkconfigure(cb.adddem, values = unlist(listOpenFiles), textvariable = file.grddem1)
			tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = file.plotShp)

			demData <- getDemOpenData(str_trim(tclvalue(file.grddem1)), convertNeg2NA = TRUE)
			if(is.null(demData)) EnvLOOCValidationplot$dem <- NULL
			names(demData) <- c('x', 'y', 'z')
			EnvLOOCValidationplot$dem$elv <- demData
			######
			demr <- raster(demData)
			slope <- terrain(demr, opt = 'slope')
			aspect <- terrain(demr, opt = 'aspect')
			hill <- hillShade(slope, aspect, angle = 40, direction = 270)
			hill <- t(as.matrix(hill))
			hill <- hill[, rev(seq(ncol(hill)))]
			EnvLOOCValidationplot$dem$hill <- list(x = demData$x, y = demData$y, z = hill)
			rm(demData, demr, slope, aspect, hill)
		}else return(NULL)
	})

	tkgrid(chk.adddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.adddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.adddem, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	#################
	tkbind(cb.adddem, "<<ComboboxSelected>>", function(){
		demData <- getDemOpenData(str_trim(tclvalue(file.grddem1)), convertNeg2NA = TRUE)
		if(is.null(demData)) EnvLOOCValidationplot$dem <- NULL
		names(demData) <- c('x', 'y', 'z')
		EnvLOOCValidationplot$dem$elv <- demData
		######
		demr <- raster(demData)
		slope <- terrain(demr, opt = 'slope')
		aspect <- terrain(demr, opt = 'aspect')
		hill <- hillShade(slope, aspect, angle = 40, direction = 270)
		hill <- t(as.matrix(hill))
		hill <- hill[, rev(seq(ncol(hill)))]
		EnvLOOCValidationplot$dem$hill <- list(x = demData$x, y = demData$y, z = hill)
		rm(demData, demr, slope, aspect, hill)
	})

	tkbind(chk.adddem, "<Button-1>", function(){
		stateDEM <- if(tclvalue(EnvLOOCValidationplot$add.dem) == "1") "disabled" else "normal"
		tkconfigure(cb.adddem, state = stateDEM)
		tkconfigure(bt.adddem, state = stateDEM)
	})

	#############################
	tkgrid(frameStatTab, row = 0, column = 0, sticky = 'we')
	tkgrid(frameMap, row = 1, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameGraph, row = 2, column = 0, sticky = 'we', pady = 1)
	tkgrid(frameSHP, row = 3, column = 0, sticky = 'we', pady = 1)
	tkgrid(frameDEM, row = 4, column = 0, sticky = 'we', pady = 1)

	#######################################################################################################
	tcl('update')
	tkgrid(cmd.frame, sticky = 'nswe', pady = 5)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)
	######
	return(cmd.frame)
}


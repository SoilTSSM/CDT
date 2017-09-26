Precip_mergeGetInfoALL <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur1 <- 42
		largeur2 <- 45
		largeur3 <- 27
		largeur4 <- 28
	}else{
		largeur1 <- 26
		largeur2 <- 28
		largeur3 <- 21
		largeur4 <- 17
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

	frInputData <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.stnfl <- tclVar(GeneralParameters$STN.file)
	dir.RFE <- tclVar(GeneralParameters$RFE$dir)

	txt.stnfl <- tklabel(frInputData, text = 'Station data file', anchor = 'w', justify = 'left')
	cb.stnfl <- ttkcombobox(frInputData, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
	bt.stnfl <- tkbutton(frInputData, text = "...")
	txt.RFE <- tklabel(frInputData, text = 'Directory containing RFE data', anchor = 'w', justify = 'left')
	set.RFE <- tkbutton(frInputData, text = "Settings")
	en.RFE <- tkentry(frInputData, textvariable = dir.RFE, width = largeur2)
	bt.RFE <- tkbutton(frInputData, text = "...")

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
			tkconfigure(cb.blkshp, values = unlist(listOpenFiles), textvariable = file.blkshp)
		}else return(NULL)
	})

	tkconfigure(set.RFE, command = function(){
		GeneralParameters[["RFE"]] <<- getInfoNetcdfData(tt, GeneralParameters[["RFE"]], str_trim(tclvalue(dir.RFE)), tclvalue(file.period))
	})

	tkconfigure(bt.RFE, command = function(){
		dirrfe <- tk_choose.dir(getwd(), "")
		tclvalue(dir.RFE) <- if(!is.na(dirrfe)) dirrfe else ""
	})

	######
	tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	tkgrid(txt.RFE, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.RFE, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.RFE, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.RFE, row = 3, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.stnfl, 'Select the file from the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'Select the file containing the gauge data')
	infobulle(bt.stnfl, 'Browse file if not listed')
	status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')
	infobulle(en.RFE, 'Enter the full path to the directory containing the RFE data')
	status.bar.display(en.RFE, TextOutputVar, 'Enter the full path to the directory containing the RFE data')
	infobulle(bt.RFE, 'Or browse here')
	status.bar.display(bt.RFE, TextOutputVar, 'Or browse here')
	infobulle(set.RFE, 'Setting netcdf data options')
	status.bar.display(set.RFE, TextOutputVar, 'Setting netcdf data options')

	############################################

	frameBias <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	cb.biasMthd <- c("Quantile.Mapping", "Multiplicative.Bias.Var", "Multiplicative.Bias.Mon")
	bias.method <- tclVar(str_trim(GeneralParameters$BIAS$bias.method))

	bias.calc <- tclVar(GeneralParameters$BIAS$deja.calc)
	bias.dir <- tclVar(GeneralParameters$BIAS$dir.Bias)
	bias.year1 <- tclVar(GeneralParameters$BIAS$start.year)
	bias.year2 <- tclVar(GeneralParameters$BIAS$end.year)

	statebias1 <- if(GeneralParameters$BIAS$deja.calc) 'disabled' else 'normal'
	statebias2 <- if(GeneralParameters$BIAS$deja.calc) 'normal' else 'disabled'

	txt.bias <- tklabel(frameBias, text = 'Bias method', anchor = 'w', justify = 'left')
	cb.bias <- ttkcombobox(frameBias, values = cb.biasMthd, textvariable = bias.method, width = largeur3)
	chk.bias <- tkcheckbutton(frameBias, variable = bias.calc, text =  "Bias factors are already calculated", anchor = 'w', justify = 'left', background = 'lightblue')

	fr.baseBias <- ttklabelframe(frameBias, text = "Base period", relief = 'groove', labelanchor = "n")
	txt.bias.years1 <- tklabel(fr.baseBias, text = 'Start Year', anchor = 'e', justify = 'right')
	txt.bias.years2 <- tklabel(fr.baseBias, text = 'End Year', anchor = 'e', justify = 'right')
	en.bias.years1 <- tkentry(fr.baseBias, width = 6, textvariable = bias.year1, state = statebias1, justify = 'right')
	en.bias.years2 <- tkentry(fr.baseBias, width = 6, textvariable = bias.year2, state = statebias1, justify = 'right')

	bt.bias.interp <- ttkbutton(frameBias, text = "Bias Interpolations Parameters", state = statebias1)

	txt.bias.dir <- tklabel(frameBias, text = "Directory of bias files", anchor = 'w', justify = 'left')
	en.bias.dir <- tkentry(frameBias, textvariable = bias.dir, state = statebias2, width = largeur2)
	bt.bias.dir <- tkbutton(frameBias, text = "...", state = statebias2)

	tkconfigure(bt.bias.dir, command = function(){
		dirbias <- tk_choose.dir(getwd(), "")
		tclvalue(bias.dir) <- if(!is.na(dirbias)) dirbias else ""
	})

	tkconfigure(bt.bias.interp, command = function(){
		GeneralParameters[["BIAS"]] <<- getInterpolationPars(tt, GeneralParameters[["BIAS"]], interpChoix = 1)

		statedem <- if((tclvalue(bias.calc) == "0" &
						GeneralParameters$BIAS$interp.method == "NN") |
						(tclvalue(mrg.method) == "Spatio-Temporal LM" &
						tclvalue(lmcoef.calc) == "0" &
						GeneralParameters$LMCOEF$interp.method == "NN") |
						tclvalue(blankGrd) == "Use DEM") 'normal' else 'disabled'

		tkconfigure(cb.grddem, state = statedem)
		tkconfigure(bt.grddem, state = statedem)
	})

	tkgrid(txt.bias, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.bias, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.bias, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.baseBias, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(bt.bias.interp, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.bias.dir, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.bias.dir, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.bias.dir, row = 5, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.bias.years1, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.bias.years1, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.bias.years2, row = 0, column = 3, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.bias.years2, row = 0, column = 4, sticky = 'w', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.bias, 'Select the method to be used to calculate the Bias Factors or Parameters')
	status.bar.display(cb.bias, TextOutputVar, 'Select the method to be used to calculate the Bias Factors or Parameters')
	infobulle(chk.bias, 'Check this box if the bias factors or parameters are already calculated')
	status.bar.display(chk.bias, TextOutputVar, 'Check this box if the bias factors or parameters are already calculated')

	infobulle(en.bias.years1, 'Start year to be used to compute bias factors')
	status.bar.display(en.bias.years1, TextOutputVar, 'Start year to be used to compute bias factors')
	infobulle(en.bias.years2, 'End year to be used to compute bias factors')
	status.bar.display(en.bias.years2, TextOutputVar, 'End year to be used to compute bias factors')

	infobulle(en.bias.dir, 'Enter the full path to directory containing the bias files')
	status.bar.display(en.bias.dir, TextOutputVar, 'Enter the full path to directory containing the bias files')

	###############
	tkbind(chk.bias, "<Button-1>", function(){
		statebias1 <- if(tclvalue(bias.calc) == '1') 'normal' else 'disabled'
		statebias2 <- if(tclvalue(bias.calc) == '0') 'normal' else 'disabled'
		tkconfigure(en.bias.years1, state = statebias1)
		tkconfigure(en.bias.years2, state = statebias1)
		tkconfigure(bt.bias.interp, state = statebias1)
		tkconfigure(en.bias.dir, state = statebias2)
		tkconfigure(bt.bias.dir, state = statebias2)

		statedem <- if((tclvalue(bias.calc) == "1" &
						GeneralParameters$BIAS$interp.method == "NN") |
						(tclvalue(mrg.method) == "Spatio-Temporal LM" &
						tclvalue(lmcoef.calc) == "0" &
						GeneralParameters$LMCOEF$interp.method == "NN") |
						tclvalue(blankGrd) == "Use DEM") 'normal' else 'disabled'

		tkconfigure(cb.grddem, state = statedem)
		tkconfigure(bt.grddem, state = statedem)
	})

	############################################

	frMrg <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	cb.MrgMthd <- c("Regression Kriging", "Spatio-Temporal LM", "Simple Bias Adjustment")
	mrg.method <- tclVar(str_trim(GeneralParameters$Merging$mrg.method))
	mrg.min.stn <- tclVar(GeneralParameters$Merging$min.stn)
	mrg.min.non.zero <- tclVar(GeneralParameters$Merging$min.non.zero)

	txt.mrg <- tklabel(frMrg, text = 'Merging method', anchor = 'w', justify = 'left')
	cb.mrg <- ttkcombobox(frMrg, values = cb.MrgMthd, textvariable = mrg.method, width = largeur4)
	bt.mrg.interp <- ttkbutton(frMrg, text = "Merging Interpolations Parameters")

	txt.min.nbrs.stn <- tklabel(frMrg, text = 'Min.Nb.Stn', anchor = 'e', justify = 'right')
	en.min.nbrs.stn <- tkentry(frMrg, width = 4, textvariable = mrg.min.stn, justify = 'right')
	txt.min.non.zero <- tklabel(frMrg, text = 'Min.No.Zero', anchor = 'e', justify = 'right')
	en.min.non.zero <- tkentry(frMrg, width = 4, textvariable = mrg.min.non.zero, justify = 'right')

	tkconfigure(bt.mrg.interp, command = function(){
		GeneralParameters[["Merging"]] <<- getInterpolationPars(tt, GeneralParameters[["Merging"]], interpChoix = 0)
	})

	tkgrid(txt.mrg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.mrg, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.mrg.interp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.min.nbrs.stn, row = 2, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.min.nbrs.stn, row = 2, column = 2, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.min.non.zero, row = 2, column = 3, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.min.non.zero, row = 2, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.mrg, 'Method to be used to perform merging')
	status.bar.display(cb.mrg, TextOutputVar, 'Method to be used to perform merging')

	infobulle(en.min.nbrs.stn, 'Minimum number of gauges with data to be used to do the merging')
	status.bar.display(en.min.nbrs.stn, TextOutputVar, 'Minimum number of gauges with data to be used to do the merging')
	infobulle(en.min.non.zero, 'Minimum number of non-zero gauge values to perform the merging')
	status.bar.display(en.min.non.zero, TextOutputVar, 'Minimum number of non-zero gauge values to perform the merging')

	###############
	tkbind(cb.mrg, "<<ComboboxSelected>>", function(){
		stateLMCoef1 <- if(tclvalue(mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'
		stateLMCoef2 <- if(tclvalue(mrg.method) == "Spatio-Temporal LM" & tclvalue(lmcoef.calc) == "0") 'normal' else 'disabled'
		stateLMCoef3 <- if(tclvalue(mrg.method) == "Spatio-Temporal LM" & tclvalue(lmcoef.calc) == "1") 'normal' else 'disabled'
		tkconfigure(chk.LMCoef, state = stateLMCoef1)
		tkconfigure(en.LMCoef.years1, state = stateLMCoef2)
		tkconfigure(en.LMCoef.years2, state = stateLMCoef2)
		tkconfigure(bt.LMCoef.interp, state = stateLMCoef2)
		tkconfigure(en.LMCoef.dir, state = stateLMCoef3)
		tkconfigure(bt.LMCoef.dir, state = stateLMCoef3)

		statedem <- if((tclvalue(bias.calc) == "0" &
						GeneralParameters$BIAS$interp.method == "NN") |
						(tclvalue(mrg.method) == "Spatio-Temporal LM" &
						tclvalue(lmcoef.calc) == "0" &
						GeneralParameters$LMCOEF$interp.method == "NN") |
						tclvalue(blankGrd) == "Use DEM") 'normal' else 'disabled'

		tkconfigure(cb.grddem, state = statedem)
		tkconfigure(bt.grddem, state = statedem)
	})


	############################################
	tkgrid(frInputData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frameBias, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMrg, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	##############################################  RIGHT   ############################################

	frDate <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Pentad data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(GeneralParameters$period,
									'daily' = cb.periodVAL[1],
									'pentad' = cb.periodVAL[2],
									'dekadal' = cb.periodVAL[3],
									'monthly' = cb.periodVAL[4])

	istart.yrs <- tclVar(GeneralParameters$Merging.Date$start.year)
	istart.mon <- tclVar(GeneralParameters$Merging.Date$start.mon)
	istart.day <- tclVar(GeneralParameters$Merging.Date$start.dek)
	iend.yrs <- tclVar(GeneralParameters$Merging.Date$end.year)
	iend.mon <- tclVar(GeneralParameters$Merging.Date$end.mon)
	iend.day <- tclVar(GeneralParameters$Merging.Date$end.dek)

	txtdek <- switch(GeneralParameters$period, 'dekadal' = 'Dek', 'pentad' = 'Pen', 'Day')
	day.txtVar <- tclVar(txtdek)
	statedate <- if(GeneralParameters$period == 'monthly') 'disabled' else 'normal'

	cb.period <- ttkcombobox(frDate, values = cb.periodVAL, textvariable = file.period, width = largeur1)
	frtxtDate <- ttklabelframe(frDate, text = "Merging Date Range", relief = 'groove')

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

	tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(frtxtDate, row = 1, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

	infobulle(cb.period, 'Select the time step of the data')
	status.bar.display(cb.period, TextOutputVar, 'Select the time step of the data')
	infobulle(frtxtDate, 'Start and end date to merge RFE data')
	status.bar.display(frtxtDate, TextOutputVar, 'Start and end date to merge RFE data')

	###########
	tkbind(cb.period, "<<ComboboxSelected>>", function(){
		tclvalue(day.txtVar) <- switch(tclvalue(file.period), 'Dekadal data' = 'Dek', 'Pentad data' = 'Pen', 'Day')
		stateday <- if(tclvalue(file.period) == 'Monthly data') 'disabled' else 'normal'
		tkconfigure(en.day1, state = stateday)
		tkconfigure(en.day2, state = stateday)

		stateScaleData1 <- if(tclvalue(file.period) == 'Monthly data') 'disabled' else 'normal'
		tkconfigure(chk.scaledata, state = stateScaleData1)
		stateScaleData2 <- if(tclvalue(scale.data) == '1' & tclvalue(file.period) != 'Monthly data') 'normal' else 'disabled'
		tkconfigure(set.scaledata, state = stateScaleData2)
		tkconfigure(en.scaledata, state = stateScaleData2)
		tkconfigure(bt.scaledata, state = stateScaleData2)
	})

	############################################

	frLMCoef <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	lmcoef.calc <- tclVar(GeneralParameters$LMCOEF$deja.calc)
	LMCoef.dir <- tclVar(GeneralParameters$LMCOEF$dir.LMCoef)

	LMCoef.year1 <- tclVar(GeneralParameters$LMCOEF$start.year)
	LMCoef.year2 <- tclVar(GeneralParameters$LMCOEF$end.year)

	stateLMCoef1 <- if(str_trim(GeneralParameters$Merging$mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'
	stateLMCoef2 <- if(str_trim(GeneralParameters$Merging$mrg.method) == "Spatio-Temporal LM" & !GeneralParameters$LMCOEF$deja.calc) 'normal' else 'disabled'
	stateLMCoef3 <- if(str_trim(GeneralParameters$Merging$mrg.method) == "Spatio-Temporal LM" & GeneralParameters$LMCOEF$deja.calc) 'normal' else 'disabled'

	chk.LMCoef <- tkcheckbutton(frLMCoef, variable = lmcoef.calc, text =  "LMCoef are already calculated", state = stateLMCoef1, anchor = 'w', justify = 'left', background = 'lightblue')

	fr.baseLM <- ttklabelframe(frLMCoef, text = "Base period", relief = 'groove', labelanchor = "n")
	txt.LMCoef.years1 <- tklabel(fr.baseLM, text = 'Start Year', anchor = 'e', justify = 'right')
	txt.LMCoef.years2 <- tklabel(fr.baseLM, text = 'End Year', anchor = 'e', justify = 'right')
	en.LMCoef.years1 <- tkentry(fr.baseLM, width = 6, textvariable = LMCoef.year1, state = stateLMCoef2, justify = 'right')
	en.LMCoef.years2 <- tkentry(fr.baseLM, width = 6, textvariable = LMCoef.year2, state = stateLMCoef2, justify = 'right')

	bt.LMCoef.interp <- ttkbutton(frLMCoef, text = "LMCoef Interpolations Parameters", state = stateLMCoef2)

	txt.LMCoef.dir <- tklabel(frLMCoef, text = "Directory of LMCoef files", anchor = 'w', justify = 'left')
	en.LMCoef.dir <- tkentry(frLMCoef, textvariable = LMCoef.dir, state = stateLMCoef3, width = largeur2)
	bt.LMCoef.dir <- tkbutton(frLMCoef, text = "...", state = stateLMCoef3)

	tkconfigure(bt.LMCoef.dir, command = function(){
		dirLM <- tk_choose.dir(getwd(), "")
		tclvalue(LMCoef.dir) <- if(!is.na(dirLM)) dirLM else ""
	})

	tkconfigure(bt.LMCoef.interp, command = function(){
		GeneralParameters[["LMCOEF"]] <<- getInterpolationPars(tt, GeneralParameters[["LMCOEF"]], interpChoix = 1)

		statedem <- if((tclvalue(bias.calc) == "0" &
						GeneralParameters$BIAS$interp.method == "NN") |
						(tclvalue(mrg.method) == "Spatio-Temporal LM" &
						tclvalue(lmcoef.calc) == "0" &
						GeneralParameters$LMCOEF$interp.method == "NN") |
						tclvalue(blankGrd) == "Use DEM") 'normal' else 'disabled'

		tkconfigure(cb.grddem, state = statedem)
		tkconfigure(bt.grddem, state = statedem)
	})

	tkgrid(chk.LMCoef, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.baseLM, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(bt.LMCoef.interp, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.LMCoef.dir, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.LMCoef.dir, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.LMCoef.dir, row = 4, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.LMCoef.years1, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.LMCoef.years1, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.LMCoef.years2, row = 0, column = 3, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.LMCoef.years2, row = 0, column = 4, sticky = 'w', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(chk.LMCoef, 'Check this box if the linear model coefficients are already calculated')
	status.bar.display(chk.LMCoef, TextOutputVar, 'Check this box if the linear model coefficients are already calculated')

	infobulle(en.LMCoef.years1, 'Start year to be used to compute LM coefficients')
	status.bar.display(en.LMCoef.years1, TextOutputVar, 'Start year to be used to compute LM coefficients')
	infobulle(en.LMCoef.years2, 'End year to be used to compute LM coefficients')
	status.bar.display(en.LMCoef.years2, TextOutputVar, 'End year to be used to compute LM coefficients')

	infobulle(en.LMCoef.dir, 'Enter the full path to directory containing the LM coefficients files')
	status.bar.display(en.LMCoef.dir, TextOutputVar, 'Enter the full path to directory containing the LM coefficients files')
	infobulle(bt.LMCoef.dir, 'or browse here')
	status.bar.display(bt.LMCoef.dir, TextOutputVar, 'or browse here')

	###############
	tkbind(chk.LMCoef, "<Button-1>", function(){
		stateLMCoef2 <- if(tclvalue(lmcoef.calc) == '1' & tclvalue(mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'
		stateLMCoef3 <- if(tclvalue(lmcoef.calc) == '0' & tclvalue(mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'
		tkconfigure(en.LMCoef.years1, state = stateLMCoef2)
		tkconfigure(en.LMCoef.years2, state = stateLMCoef2)
		tkconfigure(bt.LMCoef.interp, state = stateLMCoef2)
		tkconfigure(en.LMCoef.dir, state = stateLMCoef3)
		tkconfigure(bt.LMCoef.dir, state = stateLMCoef3)

		statedem <- if((tclvalue(bias.calc) == "0" &
						GeneralParameters$BIAS$interp.method == "NN") |
						(tclvalue(mrg.method) == "Spatio-Temporal LM" &
						tclvalue(lmcoef.calc) == "1" &
						GeneralParameters$LMCOEF$interp.method == "NN") |
						tclvalue(blankGrd) == "Use DEM") 'normal' else 'disabled'

		tkconfigure(cb.grddem, state = statedem)
		tkconfigure(bt.grddem, state = statedem)
	})

	############################################

	frRnoR <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	use.RnoR <- tclVar(GeneralParameters$RnoR$use.RnoR)
	maxdist.RnoR <- tclVar(GeneralParameters$RnoR$maxdist.RnoR)
	smooth.RnoR <- tclVar(GeneralParameters$RnoR$smooth.RnoR)

	stateRnoR <- if(GeneralParameters$RnoR$use.RnoR) 'normal' else 'disabled'

	########
	txt.mrg.pars <- tklabel(frRnoR, text = 'Rain-no-Rain mask', anchor = 'w', justify = 'left')
	chk.use.rnr <- tkcheckbutton(frRnoR, variable = use.RnoR, text = 'Apply Rain-no-Rain mask', anchor = 'w', justify = 'left')
	txt.maxdist.rnr <- tklabel(frRnoR, text = 'maxdist.RnoR', anchor = 'e', justify = 'right')
	en.maxdist.rnr <- tkentry(frRnoR, width = 4, textvariable = maxdist.RnoR, justify = 'right', state = stateRnoR)
	chk.smooth.rnr <- tkcheckbutton(frRnoR, variable = smooth.RnoR, text = 'Smooth Rain-no-Rain mask', anchor = 'w', justify = 'left', state = stateRnoR)

	tkgrid(txt.mrg.pars, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.use.rnr, row = 1, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.maxdist.rnr, row = 2, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.maxdist.rnr, row = 2, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.smooth.rnr, row = 3, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(chk.use.rnr, 'Check this box to apply a mask over no rain area')
	status.bar.display(chk.use.rnr, TextOutputVar, 'Check this box to apply a mask over no rain area')
	infobulle(en.maxdist.rnr, 'Maximum distance (in decimal degrees) to be used to interpolate Rain-noRain mask')
	status.bar.display(en.maxdist.rnr, TextOutputVar, 'Maximum distance (in decimal degrees) to be used to interpolate Rain-noRain mask')
	infobulle(chk.smooth.rnr, 'Check this box to smooth the gradient between high value and no rain area')
	status.bar.display(chk.smooth.rnr, TextOutputVar, 'Check this box to smooth the gradient between high value and no rain area')

	tkbind(chk.use.rnr, "<Button-1>", function(){
		stateRnoR <- if(tclvalue(use.RnoR) == '0') 'normal' else 'disabled'
		tkconfigure(en.maxdist.rnr, state = stateRnoR)
		tkconfigure(chk.smooth.rnr, state = stateRnoR)
	})

	############################################
	tkgrid(frDate, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frLMCoef, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRnoR, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	##############################################  RIGHT 1 ############################################

	frDEM <- tkframe(frRight1, relief = 'sunken', borderwidth = 2)

	file.grddem <- tclVar(GeneralParameters$DEM.file)

	statedem <- if((!GeneralParameters$BIAS$deja.calc &
					GeneralParameters$BIAS$interp.method == "NN") |
					(GeneralParameters$Merging$mrg.method == "Spatio-Temporal LM" &
					!GeneralParameters$LMCOEF$deja.calc &
					GeneralParameters$LMCOEF$interp.method == "NN") |
					GeneralParameters$blank$blank == "2") 'normal' else 'disabled'

	txt.grddem <- tklabel(frDEM, text = "Elevation data (NetCDF)", anchor = 'w', justify = 'left')
	cb.grddem <- ttkcombobox(frDEM, values = unlist(listOpenFiles), textvariable = file.grddem, state = statedem, width = largeur1)
	bt.grddem <- tkbutton(frDEM, text = "...", state = statedem)

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
			tkconfigure(cb.blkshp, values = unlist(listOpenFiles), textvariable = file.blkshp)
		}else return(NULL)
	})

	tkgrid(txt.grddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.grddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.grddem, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.grddem, 'Choose the file in the list')
	status.bar.display(cb.grddem, TextOutputVar, 'File containing the elevation data in netcdf')
	infobulle(bt.grddem, 'Browse file if not listed')
	status.bar.display(bt.grddem, TextOutputVar, 'Browse file if not listed')

	############################################

	frSave <- tkframe(frRight1, relief = 'sunken', borderwidth = 2)

	dir2save <- tclVar(GeneralParameters$output$dir)
	outmrgff <- tclVar(GeneralParameters$output$format)

	txt.dir2save <- tklabel(frSave, text = 'Directory to save result', anchor = 'w', justify = 'left')
	en.dir2save <- tkentry(frSave, textvariable = dir2save, width = largeur2)
	bt.dir2save <- tkbutton(frSave, text = "...")
	txt.outmrgff <- tklabel(frSave, text = 'Merged data filename format', anchor = 'w', justify = 'left')
	en.outmrgff <- tkentry(frSave, textvariable = outmrgff, width = largeur2)

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

	tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.dir2save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.outmrgff, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.outmrgff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(en.dir2save, 'Enter the full path to directory to save result')
	status.bar.display(en.dir2save, TextOutputVar, 'Enter the full path to directory to save result')
	infobulle(bt.dir2save, 'or browse here')
	status.bar.display(bt.dir2save, TextOutputVar, 'or browse here')
	infobulle(en.outmrgff, 'Format of the merged data files names in NetCDF, example: rr_mrg_1981011_ALL.nc')
	status.bar.display(en.outmrgff, TextOutputVar, 'Format of the merged data files names in NetCDF, example: rr_mrg_1981011_ALL.nc')

	############################################

	frblank <- tkframe(frRight1, relief = 'sunken', borderwidth = 2)

	blankGrd <- tclVar()
	cb.blankVAL <- c("None", "Use DEM", "Use ESRI shapefile")
	tclvalue(blankGrd) <- switch(str_trim(GeneralParameters$blank$blank), 
									'1' = cb.blankVAL[1], 
									'2' = cb.blankVAL[2],
									'3' = cb.blankVAL[3])

	file.blkshp <- tclVar(GeneralParameters$blank$SHP.file)

	stateshp <- if(str_trim(GeneralParameters$blank$blank) == '3') 'normal' else 'disabled'

	txt.blankGrd <- tklabel(frblank, text = 'Blank merged data', anchor = 'w', justify = 'left')
	cb.blankGrd <- ttkcombobox(frblank, values = cb.blankVAL, textvariable = blankGrd, width = largeur1)

	txt.blkshp <- tklabel(frblank, text = "ESRI shapefiles for blanking", anchor = 'w', justify = 'left')
	cb.blkshp <- ttkcombobox(frblank, values = unlist(listOpenFiles), textvariable = file.blkshp, state = stateshp, width = largeur1)
	bt.blkshp <- tkbutton(frblank, text = "...", state = stateshp)

	########

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
			tkconfigure(cb.blkshp, values = unlist(listOpenFiles), textvariable = file.blkshp)
		}else return(NULL)
	})

	#####
	tkgrid(txt.blankGrd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.blankGrd, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.blkshp, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.blkshp, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.blkshp, row = 3, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.blankGrd, 'Blank grid outside the country boundaries or over ocean')
	status.bar.display(cb.blankGrd, TextOutputVar,'Blank grid outside the country boundaries  or over ocean\ngiven by the DEM mask or the shapefile')

	infobulle(cb.blkshp, 'Choose the file in the list')
	status.bar.display(cb.blkshp, TextOutputVar, 'Choose the file containing the ESRI shapefiles')
	infobulle(bt.blkshp, 'Browse file if not listed')
	status.bar.display(bt.blkshp, TextOutputVar, 'Browse file if not listed')

	############################################

	tkbind(cb.blankGrd, "<<ComboboxSelected>>", function(){
		stateshp <- if(tclvalue(blankGrd) == 'Use ESRI shapefile') 'normal' else 'disabled'
		tkconfigure(cb.blkshp, state = stateshp)
		tkconfigure(bt.blkshp, state = stateshp)

		statedem <- if(tclvalue(blankGrd) == "Use DEM" |
						(tclvalue(bias.calc) == "0" &
						GeneralParameters$BIAS$interp.method == "NN") |
						(tclvalue(mrg.method) == "Spatio-Temporal LM" &
						tclvalue(lmcoef.calc) == "0" &
						GeneralParameters$LMCOEF$interp.method == "NN")) 'normal' else 'disabled'
		tkconfigure(cb.grddem, state = statedem)
		tkconfigure(bt.grddem, state = statedem)
	})

	############################################

	frScaleData <- tkframe(frRight1, relief = 'sunken', borderwidth = 2)

	scale.data <- tclVar(GeneralParameters$scale.data$scale)
	dir.RRScale <- tclVar(GeneralParameters$scale.data$dir)

	stateScaleData1 <- if(GeneralParameters$period == 'monthly') 'disabled' else 'normal'
	stateScaleData2 <- if(GeneralParameters$scale.data$scale & GeneralParameters$period != 'monthly') 'normal' else 'disabled'

	chk.scaledata <- tkcheckbutton(frScaleData, variable = scale.data, text = 'Scale the output merged data', state = stateScaleData1, anchor = 'w', justify = 'left')
	txt.scaledata <- tklabel(frScaleData, text = 'Directory of netcdf data', anchor = 'w', justify = 'left')
	set.scaledata <- tkbutton(frScaleData, text = "Settings", state = stateScaleData2)
	en.scaledata <- tkentry(frScaleData, textvariable = dir.RRScale, width = largeur2, state = stateScaleData2)
	bt.scaledata <- tkbutton(frScaleData, text = "...", state = stateScaleData2)

	tkconfigure(set.scaledata, command = function(){
		GeneralParameters[["scale.data"]] <<- getInfoNetcdfData(tt, GeneralParameters[["scale.data"]], str_trim(tclvalue(dir.RRScale)), tclvalue(file.period), scale = TRUE)
	})

	tkconfigure(bt.scaledata, command = function(){
		dirscale <- tk_choose.dir(getwd(), "")
		tclvalue(dir.RRScale) <- if(!is.na(dirscale)) dirscale else ""
	})

	tkgrid(chk.scaledata, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.scaledata, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(set.scaledata, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.scaledata, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.scaledata, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(chk.scaledata, 'Check this box to scale up the output merged data to match\nthe totals of another aggregated merged data')
	status.bar.display(chk.scaledata, TextOutputVar, 'Check this box to scale up the output merged data to match\nthe totals of another aggregated merged data')
	infobulle(en.scaledata, 'Enter the full path to the directory containing the merged data to be used for scaling')
	status.bar.display(en.scaledata, TextOutputVar, 'Enter the full path to the directory containing the merged data to be used for scaling')
	infobulle(bt.scaledata, 'Or browse here')
	status.bar.display(bt.scaledata, TextOutputVar, 'Or browse here')
	infobulle(set.scaledata, 'Setting netcdf data options')
	status.bar.display(set.scaledata, TextOutputVar, 'Setting netcdf data options')

	############################################

	tkbind(chk.scaledata, "<Button-1>", function(){
		stateScaleData2 <- if(tclvalue(scale.data) == '0' & tclvalue(file.period) != 'Monthly data') 'normal' else 'disabled'
		tkconfigure(set.scaledata, state = stateScaleData2)
		tkconfigure(en.scaledata, state = stateScaleData2)
		tkconfigure(bt.scaledata, state = stateScaleData2)
	})

	############################################
	tkgrid(frDEM, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frblank, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frScaleData, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################

	tkgrid(frLeft, row = 0, column = 0, sticky = 'we', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = 'we', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight1, row = 0, column = 2, sticky = 'we', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	#######

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(file.stnfl)) == ""){
			tkmessageBox(message = "Select the file containing the station data", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(dir.RFE))%in%c("", "NA")){
			tkmessageBox(message = "Browse or enter the  directory containing the RFE files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(bias.calc) == '1' & str_trim(tclvalue(bias.dir))%in%c("", "NA"))
		{
			tkmessageBox(message = "Enter the path to directory containing the Bias factors", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(mrg.method) == "Spatio-Temporal LM" & tclvalue(lmcoef.calc) == '1' &
			str_trim(tclvalue(LMCoef.dir))%in%c("", "NA"))
		{
			tkmessageBox(message = "Enter the path to directory containing the lm coefficients", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(((tclvalue(bias.calc) == '0' & GeneralParameters$BIAS$interp.method == "NN") |
				(tclvalue(mrg.method) == "Spatio-Temporal LM" & tclvalue(lmcoef.calc) == '0' &
				GeneralParameters$LMCOEF$interp.method == "NN") | tclvalue(blankGrd) == "Use DEM") &
				(str_trim(tclvalue(file.grddem)) == ""))
		{
			tkmessageBox(message = "You have to provide DEM data in NetCDF format", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.blkshp)) == "" & str_trim(tclvalue(blankGrd)) == "Use ESRI shapefile"){
			tkmessageBox(message = "You have to provide the shapefile", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(dir2save))%in%c("", "NA")){
			tkmessageBox(message = "Browse or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(dir.RRScale))%in%c("", "NA") & tclvalue(scale.data) == '1'){
			tkmessageBox(message = "Enter the path to directory containing the netcdf data to be use for scaling", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$STN.file <<- str_trim(tclvalue(file.stnfl))
			GeneralParameters$RFE$dir <<- str_trim(tclvalue(dir.RFE))

			GeneralParameters$BIAS$bias.method <<- str_trim(tclvalue(bias.method))
			GeneralParameters$BIAS$deja.calc <<- switch(tclvalue(bias.calc), '0' = FALSE, '1' = TRUE)
			GeneralParameters$BIAS$dir.Bias <<- str_trim(tclvalue(bias.dir))
			GeneralParameters$BIAS$start.year <<- as.numeric(str_trim(tclvalue(bias.year1)))
			GeneralParameters$BIAS$end.year <<- as.numeric(str_trim(tclvalue(bias.year2)))

			GeneralParameters$Merging$mrg.method <<- str_trim(tclvalue(mrg.method))
			GeneralParameters$Merging$min.stn <<- as.numeric(str_trim(tclvalue(mrg.min.stn)))
			GeneralParameters$Merging$min.non.zero <<- as.numeric(str_trim(tclvalue(mrg.min.non.zero)))

			GeneralParameters$period <<- switch(str_trim(tclvalue(file.period)), 
												'Daily data' = 'daily',
												'Pentad data' = 'pentad',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')
			GeneralParameters$Merging.Date$start.year <<- as.numeric(str_trim(tclvalue(istart.yrs)))
			GeneralParameters$Merging.Date$start.mon <<- as.numeric(str_trim(tclvalue(istart.mon)))
			GeneralParameters$Merging.Date$start.dek <<- as.numeric(str_trim(tclvalue(istart.day)))
			GeneralParameters$Merging.Date$end.year <<- as.numeric(str_trim(tclvalue(iend.yrs)))
			GeneralParameters$Merging.Date$end.mon <<- as.numeric(str_trim(tclvalue(iend.mon)))
			GeneralParameters$Merging.Date$end.dek <<- as.numeric(str_trim(tclvalue(iend.day)))

			GeneralParameters$LMCOEF$deja.calc <<- switch(tclvalue(lmcoef.calc), '0' = FALSE, '1' = TRUE)
			GeneralParameters$LMCOEF$dir.LMCoef <<- str_trim(tclvalue(LMCoef.dir))
			GeneralParameters$LMCOEF$start.year <<- as.numeric(str_trim(tclvalue(LMCoef.year1)))
			GeneralParameters$LMCOEF$end.year <<- as.numeric(str_trim(tclvalue(LMCoef.year2)))

			GeneralParameters$RnoR$use.RnoR <<- switch(tclvalue(use.RnoR), '0' = FALSE, '1' = TRUE)
			GeneralParameters$RnoR$maxdist.RnoR <<- as.numeric(str_trim(tclvalue(maxdist.RnoR)))
			GeneralParameters$RnoR$smooth.RnoR <<- switch(tclvalue(smooth.RnoR), '0' = FALSE, '1' = TRUE)

			GeneralParameters$DEM.file <<- str_trim(tclvalue(file.grddem))

			GeneralParameters$output$dir <<- str_trim(tclvalue(dir2save))
			GeneralParameters$output$format <<- str_trim(tclvalue(outmrgff))

			GeneralParameters$blank$blank <<- switch(str_trim(tclvalue(blankGrd)),
													"None" = '1', "Use DEM" = '2',
													"Use ESRI shapefile" = '3')
			GeneralParameters$blank$SHP.file <<- str_trim(tclvalue(file.blkshp))

			GeneralParameters$scale.data$scale <<- switch(tclvalue(scale.data), '0' = FALSE, '1' = TRUE)
			GeneralParameters$scale.data$dir <<- str_trim(tclvalue(dir.RRScale))

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
	tkwm.title(tt, 'Merging data - Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}


###################################################################################################

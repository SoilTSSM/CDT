mergeGetInfoRain.simple <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur1 <- 27
		largeur2 <- 32
		largeur3 <- 25
		largeur4 <- 21
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
		GeneralParameters <<- getInfoNetcdfRFE.simple(tt, GeneralParameters, str_trim(tclvalue(dir.RFE)))
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

	infobulle(cb.stnfl, 'Choose the file in the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the gauge data')
	infobulle(bt.stnfl, 'Browse file if not listed')
	status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')
	infobulle(en.RFE, 'Select the file containing the RFE data')
	status.bar.display(en.RFE, TextOutputVar, 'Select the file containing the RFE data')
	infobulle(bt.RFE, 'Browse file if not listed')
	status.bar.display(bt.RFE, TextOutputVar, 'Browse file if not listed')
	infobulle(set.RFE, 'Setting netcdf data options')
	status.bar.display(set.RFE, TextOutputVar, 'Setting netcdf data options')

	############################################

	frameBias <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	cb.biasMthd <- c("Quantile.Mapping", "Multiplicative.Bias.Var", "Multiplicative.Bias.Mon")
	bias.method <- tclVar(str_trim(GeneralParameters$BIAS$method))
	bias.calc <- tclVar(GeneralParameters$BIAS$deja.calc)
	bias.dir <- tclVar(GeneralParameters$BIAS$dir.Bias)
	year1 <- tclVar(GeneralParameters$BIAS$start.year)
	year2 <- tclVar(GeneralParameters$BIAS$end.year)
	statebias1 <- if(GeneralParameters$BIAS$deja.calc) 'disabled' else 'normal'
	statebias2 <- if(GeneralParameters$BIAS$deja.calc) 'normal' else 'disabled'

	txt.bias <- tklabel(frameBias, text = 'Bias method', anchor = 'w', justify = 'left')
	cb.bias <- ttkcombobox(frameBias, values = cb.biasMthd, textvariable = bias.method, width = largeur3)

	txt.years1 <- tklabel(frameBias, text = 'Start Year', anchor = 'e', justify = 'right')
	txt.years2 <- tklabel(frameBias, text = 'End Year', anchor = 'e', justify = 'right')
	en.years1 <- tkentry(frameBias, width = 6, textvariable = year1, state = statebias1, justify = 'right')
	en.years2 <- tkentry(frameBias, width = 6, textvariable = year2, state = statebias1, justify = 'right')

	chk.bias <- tkcheckbutton(frameBias, variable = bias.calc, text =  "Bias factors are already calculated", anchor = 'w', justify = 'left', background = 'lightblue')
	txt.bias.dir <- tklabel(frameBias, text = "Directory of bias files", anchor = 'w', justify = 'left')
	en.bias <- tkentry(frameBias, textvariable = bias.dir, state = statebias2, width = largeur2)
	bt.bias <- tkbutton(frameBias, text = "...", state = statebias2)

	tkconfigure(bt.bias, command = function(){
		dirbias <- tk_choose.dir(getwd(), "")
		tclvalue(bias.dir) <- if(!is.na(dirbias)) dirbias else ""
	})

	tkgrid(txt.bias, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.bias, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.years1, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.years1, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.years2, row = 1, column = 3, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.years2, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(chk.bias, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.bias.dir, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.bias, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.bias, row = 4, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)


	infobulle(cb.bias, 'Select the method to be used to calculate the Bias Factors or Parameters')
	status.bar.display(cb.bias, TextOutputVar, 'Select the method to be used to calculate the Bias Factors or Parameters')
	infobulle(chk.bias, 'Check this box if the bias factors or parameters are already calculated')
	status.bar.display(chk.bias, TextOutputVar, 'Check this box if the bias factors or parameters are already calculated')
	infobulle(en.years1, 'Start year to be used to compute bias factors')
	status.bar.display(en.years1, TextOutputVar, 'Start year to be used to compute bias factors')
	infobulle(en.years2, 'End year to be used to compute bias factors')
	status.bar.display(en.years2, TextOutputVar, 'End year to be used to compute bias factors')

	status.bar.display(chk.bias, TextOutputVar, 'Check this box if the bias factors or parameters are already calculated')
	infobulle(en.bias, 'Enter the full path to directory containing the bias files')
	status.bar.display(en.bias, TextOutputVar, 'Enter the full path to directory containing the bias files')

	###############
	tkbind(chk.bias, "<Button-1>", function(){
		statebias1 <- if(tclvalue(bias.calc) == '1') 'normal' else 'disabled'
		statebias2 <- if(tclvalue(bias.calc) == '0') 'normal' else 'disabled'
		tkconfigure(en.years1, state = statebias1)
		tkconfigure(en.years2, state = statebias1)
		tkconfigure(en.bias, state = statebias2)
		tkconfigure(bt.bias, state = statebias2)
	})

	############################################

	frMrg <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	cb.MrgMthd <- c("Regression Kriging", "Spatio-Temporal LM", "Simple Bias Adjustment")
	mrg.method <- tclVar(str_trim(GeneralParameters$Merging$method))
	min.stn <- tclVar(GeneralParameters$Merging$min.stn)
	min.non.zero <- tclVar(GeneralParameters$Merging$min.non.zero)
	lmcoef.calc <- tclVar(GeneralParameters$Merging$deja.calc)
	dir.LMCoef <- tclVar(GeneralParameters$Merging$dir.LMCoef)

	stateLMCoef1 <- if(str_trim(GeneralParameters$Merging$method) == "Spatio-Temporal LM") 'normal' else 'disabled'
	stateLMCoef2 <- if(str_trim(GeneralParameters$Merging$method) == "Spatio-Temporal LM" & GeneralParameters$Merging$deja.calc) 'normal' else 'disabled'

	txt.mrg <- tklabel(frMrg, text = 'Merging method', anchor = 'w', justify = 'left')
	cb.mrg <- ttkcombobox(frMrg, values = cb.MrgMthd, textvariable = mrg.method, width = largeur4)

	txt.min.nbrs.stn <- tklabel(frMrg, text = 'Min.Nb.Stn', anchor = 'e', justify = 'right')
	en.min.nbrs.stn <- tkentry(frMrg, width = 4, textvariable = min.stn, justify = 'right')
	txt.min.non.zero <- tklabel(frMrg, text = 'Min.No.Zero', anchor = 'e', justify = 'right')
	en.min.non.zero <- tkentry(frMrg, width = 4, textvariable = min.non.zero, justify = 'right')

	chk.mrg <- tkcheckbutton(frMrg, variable = lmcoef.calc, text =  "LMCoef are already calculated", state = stateLMCoef1, anchor = 'w', justify = 'left', background = 'lightblue')
	txt.dir.LM <- tklabel(frMrg, text = "Directory of LMCoef files", anchor = 'w', justify = 'left')
	en.dir.LM <- tkentry(frMrg, textvariable = dir.LMCoef, state = stateLMCoef2, width = largeur2)
	bt.dir.LM <- tkbutton(frMrg, text = "...", state = stateLMCoef2)

	tkconfigure(bt.dir.LM, command = function(){
		dirLM <- tk_choose.dir(getwd(), "")
		tclvalue(dir.LMCoef) <- if(!is.na(dirLM)) dirLM else ""
	})

	tkgrid(txt.mrg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.mrg, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.min.nbrs.stn, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.min.nbrs.stn, row = 1, column = 2, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.min.non.zero, row = 1, column = 3, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.min.non.zero, row = 1, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(chk.mrg, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.dir.LM, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.dir.LM, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.LM, row = 4, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.mrg, 'Method to be used to perform merging')
	status.bar.display(cb.mrg, TextOutputVar, 'Method to be used to perform merging')

	infobulle(en.min.nbrs.stn, 'Minimum number of gauges with data to be used to do the merging')
	status.bar.display(en.min.nbrs.stn, TextOutputVar, 'Minimum number of gauges with data to be used to do the merging')
	infobulle(en.min.non.zero, 'Minimum number of non-zero gauge values to perform the merging')
	status.bar.display(en.min.non.zero, TextOutputVar, 'Minimum number of non-zero gauge values to perform the merging')

	infobulle(chk.mrg, 'Check this box if the linear model coefficients are already calculated')
	status.bar.display(chk.mrg, TextOutputVar, 'Check this box if the linear model coefficients are already calculated')
	infobulle(en.dir.LM, 'Enter the full path to directory containing the LM coefficients files')
	status.bar.display(en.dir.LM, TextOutputVar, 'Enter the full path to directory containing the LM coefficients files')
	infobulle(bt.dir.LM, 'or browse here')
	status.bar.display(bt.dir.LM, TextOutputVar, 'or browse here')

	###############
	tkbind(cb.mrg, "<<ComboboxSelected>>", function(){
		stateLM1 <- if(tclvalue(mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'
		stateLM2 <- if(tclvalue(mrg.method) == "Spatio-Temporal LM" & tclvalue(lmcoef.calc) == "1") 'normal' else 'disabled'
		tkconfigure(chk.mrg, state = stateLM1)
		tkconfigure(en.dir.LM, state = stateLM2)
		tkconfigure(bt.dir.LM, state = stateLM2)
	})

	tkbind(chk.mrg, "<Button-1>", function(){
		stateLM2 <- if(tclvalue(lmcoef.calc) == '0' & tclvalue(mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'
		tkconfigure(en.dir.LM, state = stateLM2)
		tkconfigure(bt.dir.LM, state = stateLM2)
	})

	############################################
	tkgrid(frInputData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frameBias, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMrg, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################  RIGHT   #####################

	frDate <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(GeneralParameters$period, 
									'daily' = cb.periodVAL[1], 
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3])
	istart.yrs <- tclVar(GeneralParameters$Date$start.year)
	istart.mon <- tclVar(GeneralParameters$Date$start.mon)
	istart.day <- tclVar(GeneralParameters$Date$start.dek)
	iend.yrs <- tclVar(GeneralParameters$Date$end.year)
	iend.mon <- tclVar(GeneralParameters$Date$end.mon)
	iend.day <- tclVar(GeneralParameters$Date$end.dek)
	day.txtVar <- if(GeneralParameters$period == 'dekadal') tclVar('Dek') else tclVar('Day')
	statedate <- if(GeneralParameters$period == 'monthly') 'disabled' else 'normal'

	cb.period <- ttkcombobox(frDate, values = cb.periodVAL, textvariable = file.period, width = largeur1)
	frtxtDate <- ttklabelframe(frDate, text = "Date Range", relief = 'groove')

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

	tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frtxtDate, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)

	infobulle(cb.period, 'Choose the time step of the data')
	status.bar.display(cb.period, TextOutputVar, 'Choose the time step of the data')
	infobulle(frtxtDate, 'Start and end date to merge RFE data')
	status.bar.display(frtxtDate, TextOutputVar, 'Start and end date to merge RFE data')

	###########
	tkbind(cb.period, "<<ComboboxSelected>>", function(){
		tclvalue(day.txtVar) <- if(tclvalue(file.period) == 'Dekadal data') "Dek" else "Day"
		stateday <- if(tclvalue(file.period) == 'Monthly data') 'disabled' else 'normal'
		tkconfigure(en.day1, state = stateday)
		tkconfigure(en.day2, state = stateday)
	})

	############################################

	frInterp <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	interp.method <- tclVar()
	cb.InterpVAL <- c('Kriging', 'Inverse Distance Weighted')
	tclvalue(interp.method) <- switch(GeneralParameters$Interp$method, 
										'Kriging' = cb.InterpVAL[1], 
										'IDW' = cb.InterpVAL[2])
	nmin <- tclVar(GeneralParameters$Interp$nmin)
	nmax <- tclVar(GeneralParameters$Interp$nmax)
	maxdist <- tclVar(GeneralParameters$Interp$maxdist)

	txt.Interp <- tklabel(frInterp, text = 'Interpolation method', anchor = 'w', justify = 'left')
	cb.Interp <- ttkcombobox(frInterp, values = cb.InterpVAL, textvariable = interp.method, width = largeur1)
	frInterpPars <- ttklabelframe(frInterp, text = "Interpolation parameters", relief = 'groove')

	txt.nmin <- tklabel(frInterpPars, text = 'nmin', anchor = 'e', justify = 'right')
	txt.nmax <- tklabel(frInterpPars, text = 'nmax', anchor = 'e', justify = 'right')
	txt.maxdist <- tklabel(frInterpPars, text = 'maxdist', anchor = 'e', justify = 'right')
	en.nmin <- tkentry(frInterpPars, width = 4, textvariable = nmin, justify = 'right')
	en.nmax <- tkentry(frInterpPars, width = 4, textvariable = nmax, justify = 'right')
	en.maxdist <- tkentry(frInterpPars, width = 4, textvariable = maxdist, justify = 'right')

	########

	tkgrid(txt.Interp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.Interp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frInterpPars, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.nmin, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.nmin, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.nmax, row = 0, column = 2, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.nmax, row = 0, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.maxdist, row = 1, column = 3, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.maxdist, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	########

	infobulle(en.nmin, 'Minimum number of neighbors to be used to interpolate data')
	status.bar.display(en.nmin, TextOutputVar, 'Minimum number of neighbors to be used to interpolate data')
	infobulle(en.nmax, 'Maximum number of neighbors to be used to interpolate data')
	status.bar.display(en.nmax, TextOutputVar, 'Maximum number of neighbors to be used to interpolate data')
	infobulle(en.maxdist, 'Maximum distance (in  decimal degree) to be used to interpolate data')
	status.bar.display(en.maxdist, TextOutputVar, 'Maximum distance (in  decimal degree) to be used to interpolate data')

	############################################

	frRnoR <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	use.RnoR <- tclVar(GeneralParameters$RnoR$use.RnoR)
	maxdist.RnoR <- tclVar(GeneralParameters$RnoR$maxdist.RnoR)
	smooth.RnoR <- tclVar(GeneralParameters$RnoR$smooth.RnoR)

	########
	txt.mrg.pars <- tklabel(frRnoR, text = 'Rain-no-Rain mask', anchor = 'w', justify = 'left')
	chk.use.rnr <- tkcheckbutton(frRnoR, variable = use.RnoR, text = 'Apply Rain-no-Rain mask', anchor = 'w', justify = 'left')
	txt.maxdist.rnr <- tklabel(frRnoR, text = 'maxdist.RnoR', anchor = 'e', justify = 'right')
	en.maxdist.rnr <- tkentry(frRnoR, width = 4, textvariable = maxdist.RnoR, justify = 'right')
	chk.smooth.rnr <- tkcheckbutton(frRnoR, variable = smooth.RnoR, text = 'Smooth Rain-no-Rain mask', anchor = 'w', justify = 'left')

	tkgrid(txt.mrg.pars, row = 0, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
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

	############################################
	tkgrid(frDate, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frInterp, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRnoR, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################  RIGHT 1 #####################

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
	infobulle(en.outmrgff, 'Format of the merged data files names in NetCDF, example: rr_mrg_1983012_ALL.nc')
	status.bar.display(en.outmrgff, TextOutputVar, 'Format of the merged data files names in NetCDF, example: rr_mrg_1983012_ALL.nc')

	############################################

	frblank <- tkframe(frRight1, relief = 'sunken', borderwidth = 2)

	blankGrd <- tclVar()
	cb.blankVAL <- c("None", "Use DEM", "Use ESRI shapefile")
	tclvalue(blankGrd) <- switch(str_trim(GeneralParameters$blank$blank), 
									'1' = cb.blankVAL[1], 
									'2' = cb.blankVAL[2],
									'3' = cb.blankVAL[3])

	file.grddem <- tclVar(GeneralParameters$blank$DEM.file)
	file.blkshp <- tclVar(GeneralParameters$blank$SHP.file)

	statedem <- if(str_trim(GeneralParameters$blank$blank) == '2') 'normal' else 'disabled'
	stateshp <- if(str_trim(GeneralParameters$blank$blank) == '3') 'normal' else 'disabled'

	txt.blankGrd <- tklabel(frblank, text = 'Blank merged data', anchor = 'w', justify = 'left')
	cb.blankGrd <- ttkcombobox(frblank, values = cb.blankVAL, textvariable = blankGrd, width = largeur1)
	txt.grddem <- tklabel(frblank, text = "Elevation data (NetCDF)", anchor = 'w', justify = 'left')
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
			tkconfigure(cb.blkshp, values = unlist(listOpenFiles), textvariable = file.blkshp)
		}else return(NULL)
	})

	#####
	tkgrid(txt.blankGrd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.blankGrd, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.grddem, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.grddem, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.grddem, row = 3, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.blkshp, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.blkshp, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.blkshp, row = 5, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

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
		statedem <- if(tclvalue(blankGrd) == 'Use DEM') 'normal' else 'disabled'
		stateshp <- if(tclvalue(blankGrd) == 'Use ESRI shapefile') 'normal' else 'disabled'
		tkconfigure(cb.blkshp, state = stateshp)
		tkconfigure(bt.blkshp, state = stateshp)
		tkconfigure(cb.grddem, state = statedem)
		tkconfigure(bt.grddem, state = statedem)
	})

	############################################
	tkgrid(frSave, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frblank, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	# tkgrid(frRnoR, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)



	############################################
	
	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight1, row = 0, column = 2, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	#######

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

getInfoNetcdfRFE.simple <- function(tt, GeneralParameters, ncDIR){
	listOpenFiles <- openFile_ttkcomboList()

	largeur1 <- if(Sys.info()["sysname"] == "Windows")  27 else 25
	###################

	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt1)

	###################

	frFF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	inrfeff <- tclVar(GeneralParameters$RFE$format)
	rfesample <- tclVar(GeneralParameters$RFE$sample)

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

	tkgrid(txt.ncsample, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.ncsample, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.ncsample, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.inrfeff, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.inrfeff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	status.bar.display(cb.ncsample, TextOutputVar, 'File containing a sample of the data in netcdf')
	infobulle(bt.ncsample, 'Browse file if not listed')
	infobulle(en.inrfeff, 'Enter the filename format of netcdf data,\nexample: rfe1983_01-dk3.nc')
	status.bar.display(en.inrfeff, TextOutputVar, 'Enter the filename format of netcdf data,\nexample: rfe1983_01-dk3.nc')

	###################

	tkgrid(frFF, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)

	################################

	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		GeneralParameters$RFE$format <<- str_trim(tclvalue(inrfeff))
		GeneralParameters$RFE$sample <<- str_trim(tclvalue(rfesample))

		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
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
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'NetCDF Data - Settings')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(GeneralParameters)
}



###################################################################################################

# getInfoBiasRFE.simple <- function(tt, GeneralParameters, bias.method){
# 	largeur1 <- if(Sys.info()["sysname"] == "Windows")  27 else 25
# 	###################

# 	tt1 <- tktoplevel()
# 	tkgrab.set(tt1)
# 	tkfocus(tt1)

# 	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
# 	frMRG1 <- tkframe(tt1)

# 	###################

# 	frFF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

# 	bias.dir <- tclVar(GeneralParameters$BIAS$dir.Bias)
# 	bias.prefix <- tclVar(GeneralParameters$BIAS$prefix)
# 	statePrefix <- if(bias.method == "Quantile.Mapping") "disabled" else "normal"

# 	txt.biasDir <- tklabel(frFF, text = "Directory of bias files", anchor = 'w', justify = 'left')
# 	en.biasDir <- tkentry(frFF, textvariable = bias.dir, width = largeur1)
# 	bt.biasDir <- tkbutton(frFF, text = "...")
# 	txt.biasPfx <- tklabel(frFF, text = 'Bias filename prefix', anchor = 'w', justify = 'left')
# 	en.biasPfx <- tkentry(frFF, textvariable = bias.prefix, width = largeur1, state = statePrefix)

# 	###################

# 	tkconfigure(bt.biasDir, command = function(){
# 		dirbias <- tk_choose.dir(getwd(), "")
# 		tclvalue(bias.dir) <- if(!is.na(dirbias)) dirbias else ""
# 	})

# 	###################

# 	tkgrid(txt.biasDir, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
# 	tkgrid(en.biasDir, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
# 	tkgrid(bt.biasDir, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
# 	tkgrid(txt.biasPfx, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
# 	tkgrid(en.biasPfx, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

# 	infobulle(en.biasDir, 'Enter the full path to directory containing the bias files')
# 	status.bar.display(en.biasDir, TextOutputVar, 'Enter the full path to directory containing the bias files')
# 	infobulle(en.biasPfx, 'Filename prefix of the bias files')
# 	status.bar.display(en.biasPfx, TextOutputVar, 'Filename prefix of the bias files')

# 	###################

# 	tkgrid(frFF, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)

# 	################################

# 	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
# 	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

# 	tkconfigure(bt.prm.OK, command = function(){
# 		GeneralParameters$BIAS$dir.Bias <<- str_trim(tclvalue(bias.dir))
# 		GeneralParameters$BIAS$prefix <<- str_trim(tclvalue(bias.prefix))

# 		tkgrab.release(tt1)
# 		tkdestroy(tt1)
# 		tkfocus(tt)
# 	})

# 	tkconfigure(bt.prm.CA, command = function(){
# 		tkgrab.release(tt1)
# 		tkdestroy(tt1)
# 		tkfocus(tt)
# 	})

# 	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
# 	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

# 	################################
# 	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
# 	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

# 	tkwm.withdraw(tt1)
# 	tcl('update')
# 	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
# 	tt.h <- as.integer(tkwinfo("reqheight", tt1))
# 	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
# 	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
# 	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
# 	tkwm.transient(tt1)
# 	tkwm.title(tt1, 'Bias files - Settings')
# 	tkwm.deiconify(tt1)

# 	tkfocus(tt1)
# 	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
# 	tkwait.window(tt1)
# 	return(GeneralParameters)
# }

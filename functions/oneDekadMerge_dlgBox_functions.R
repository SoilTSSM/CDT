mergeDekadInfoRain <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur0 <- 42
		largeur1 <- 39
		largeur2 <- 25
	}else{
		largeur0 <- 30
		largeur1 <- 29
		largeur2 <- 20
	}

	##
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	frRight <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	frRight1 <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################

	frDate <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	istart.yrs <- tclVar(str_trim(GeneralParameters$Merging.Date$year))
	istart.mon <- tclVar(str_trim(GeneralParameters$Merging.Date$month))
	istart.day <- tclVar(str_trim(GeneralParameters$Merging.Date$dekad))

	date.txt <- tklabel(frDate, text = 'Date', anchor = 'w', justify = 'left')
	yrs.txt <- tklabel(frDate, text = 'Year')
	mon.txt <- tklabel(frDate, text = 'Month')
	day.txt <- tklabel(frDate, text = 'Dekad')

	yrs1.v <- tkentry(frDate, width = 5, textvariable = istart.yrs, justify = "right")
	mon1.v <- tkentry(frDate, width = 5, textvariable = istart.mon, justify = "right")
	day1.v <- tkentry(frDate, width = 5, textvariable = istart.day, justify = "right", state = 'normal')

	tkgrid(yrs.txt, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon.txt, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day.txt, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(date.txt, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs1.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon1.v, row = 1, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day1.v, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)

	infobulle(frDate, 'Date of merging')
	status.bar.display(frDate, TextOutputVar, 'Date of merging')

	###########################################

	frStnfl <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.stnfl <- tclVar(str_trim(GeneralParameters$STN$file))
	no.stnfl <- tclVar(GeneralParameters$STN$No.Stn.Data)

	if(tclvalue(no.stnfl) == '0'){
		state.stnfl <- 'normal'
		stateMrg <- 'normal'
	}else{
		state.stnfl <- 'disabled'
		stateMrg <- 'disabled'
	}

	chk.stnfl <- tkcheckbutton(frStnfl, variable = no.stnfl, text = 'No station data available', anchor = 'w', justify = 'left')
	txt.stnfl <- tklabel(frStnfl, text = 'Station data file', anchor = 'w', justify = 'left')
	cb.stnfl <- ttkcombobox(frStnfl, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1, state = state.stnfl)
	bt.stnfl <- tkbutton(frStnfl, text = "...", state = state.stnfl)

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

	tkgrid(chk.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl, row = 2, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(chk.stnfl, 'Check if there is no station data available')
	status.bar.display(chk.stnfl, TextOutputVar, 'Check if there is no station data available')
	infobulle(cb.stnfl, 'Choose the file in the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the gauge data')
	infobulle(bt.stnfl, 'Browse file if not listed')
	status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')

	tkbind(chk.stnfl, "<Button-1>", function(){
		stateSTN <- if(tclvalue(no.stnfl) == '0') 'disabled' else 'normal'
		tkconfigure(cb.stnfl, state = stateSTN)
		tkconfigure(bt.stnfl, state = stateSTN)

		stateMRG <- if(tclvalue(no.stnfl) == '0') 'disabled' else 'normal'
		tkconfigure(cb.mrg, state = stateMRG)
		tkconfigure(bt.mrg.interp, state = stateMRG)
		tkconfigure(en.min.nbrs.stn, state = stateMRG)
		tkconfigure(en.min.non.zero, state = stateMRG)
		tkconfigure(cb.blankGrd, state = stateMRG)
		tkconfigure(chk.use.rnr, state = stateMRG)

		stateRnR <- if(tclvalue(no.stnfl) == '0') 'disabled' else {if(tclvalue(use.RnoR) == '1') 'normal' else 'disabled'}
		tkconfigure(en.maxdist.rnr, state = stateRnR)
		tkconfigure(chk.smooth.rnr, state = stateRnR)

		stateLMC <- if(tclvalue(no.stnfl) == '0') 'disabled' else {if(tclvalue(mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'}
		tkconfigure(en.dir.LM, state = stateLMC)
		tkconfigure(bt.dir.LM, state = stateLMC)

		if(tclvalue(no.stnfl) == '0'){
			stateDEM <- 'disabled'
			stateSHP <- 'disabled'
		}else{
			if(tclvalue(blankGrd) == "None"){
				stateDEM <- 'disabled'
				stateSHP <- 'disabled'
			}
			if(tclvalue(blankGrd) == "Use DEM"){
				stateDEM <- 'normal'
				stateSHP <- 'disabled'
			}
			if(tclvalue(blankGrd) == "Use ESRI shapefile"){
				stateDEM <- 'disabled'
				stateSHP <- 'normal'
			}
		}

		tkconfigure(cb.grddem, state = stateDEM)
		tkconfigure(bt.grddem, state = stateDEM)
		tkconfigure(cb.blkshp, state = stateSHP)
		tkconfigure(bt.blkshp, state = stateSHP)
	})

	############################################
	
	frRfe <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	down.rfe <- tclVar(GeneralParameters$RFE$downloaded)
	file.grdrfe <- tclVar(GeneralParameters$RFE$file)

	data.rfe <- tclVar(GeneralParameters$RFE$source)
	minLon <- tclVar(str_trim(GeneralParameters$RFE.bbox$minlon))
	maxLon <- tclVar(str_trim(GeneralParameters$RFE.bbox$maxlon))
	minLat <- tclVar(str_trim(GeneralParameters$RFE.bbox$minlat))
	maxLat <- tclVar(str_trim(GeneralParameters$RFE.bbox$maxlat))

	if(tclvalue(down.rfe) == '1'){
		stateRFE1 <- 'normal'
		stateRFE2 <- 'disabled'
	}else{
		stateRFE1 <- 'disabled'
		stateRFE2 <- 'normal'
	}

	chk.rfe <- tkcheckbutton(frRfe, variable = down.rfe, text = 'RFE data already downloaded', anchor = 'w', justify = 'left')
	txt.grdrfe <- tklabel(frRfe, text = "RFE file", anchor = 'w', justify = 'left')
	cb.grdrfe <- ttkcombobox(frRfe, values = unlist(listOpenFiles), textvariable = file.grdrfe, width = largeur1, state = stateRFE1)
	bt.grdrfe <- tkbutton(frRfe, text = "...", state = stateRFE1)

	frDown <- ttklabelframe(frRfe, text = "Download RFE", relief = 'groove')
	cb.down.rfe <- ttkcombobox(frDown, values = c('TAMSATv2', 'TAMSATv3', 'CHIRP'), textvariable = data.rfe, width = largeur1, state = stateRFE2)

	frbbox <- tkframe(frDown)
	txt.lon <- tklabel(frbbox, text = "Longitude", anchor = 'e', justify = 'right')
	txt.lat <- tklabel(frbbox, text = "Latitude", anchor = 'e', justify = 'right')
	txt.min <- tklabel(frbbox, text = "Min")
	txt.max <- tklabel(frbbox, text = "Max")
	grd_vlon1 <- tkentry(frbbox, width = 5, justify = "right", textvariable = minLon, state = stateRFE2)
	grd_vlon2 <- tkentry(frbbox, width = 5, justify = "right", textvariable = maxLon, state = stateRFE2)
	grd_vlat1 <- tkentry(frbbox, width = 5, justify = "right", textvariable = minLat, state = stateRFE2)
	grd_vlat2 <- tkentry(frbbox, width = 5, justify = "right", textvariable = maxLat, state = stateRFE2)

	tkconfigure(bt.grdrfe, command = function(){
		nc.opfiles <- getOpenNetcdf(tt, all.opfiles)
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

	tkgrid(chk.rfe, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.grdrfe, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.grdrfe, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.grdrfe, row = 2, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frDown, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(cb.down.rfe, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frbbox, row = 1, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.min, row = 0, column = 2, sticky = "we", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.max, row = 0, column = 3, sticky = "we", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.lon, row = 1, column = 0, sticky = "e",  rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon1, row = 1, column = 2, sticky = "w", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon2, row = 1, column = 3, sticky = "w", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.lat, row = 2, column = 0, sticky = "e", rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat1, row = 2, column = 2, sticky = "w", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat2, row = 2, column = 3, sticky = "w", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(chk.rfe, 'Check if you have already downloaded RFE data')
	status.bar.display(chk.rfe, TextOutputVar, 'Check if you have already downloaded RFE data')
	infobulle(cb.grdrfe, 'File containing the RFE data in netcdf')
	status.bar.display(cb.grdrfe, TextOutputVar, 'File containing the RFE data in netcdf')
	infobulle(bt.grdrfe, 'Browse file if not listed')
	status.bar.display(bt.grdrfe, TextOutputVar, 'Browse file if not listed')

	infobulle(cb.down.rfe, 'Select the data source')
	status.bar.display(cb.down.rfe, TextOutputVar, 'Select the data source')
	infobulle(grd_vlon1, 'Minimum longitude in degree')
	status.bar.display(grd_vlon1, TextOutputVar, 'Minimum longitude in degree')
	infobulle(grd_vlon2, 'Maximum longitude in degree')
	status.bar.display(grd_vlon2, TextOutputVar, 'Maximum longitude in degree')
	infobulle(grd_vlat1, 'Minimum latitude in degree')
	status.bar.display(grd_vlat1, TextOutputVar, 'Minimum latitude in degree')
	infobulle(grd_vlat2, 'Maximum latitude in degree')
	status.bar.display(grd_vlat2, TextOutputVar, 'Maximum latitude in degree')

	tkbind(chk.rfe, "<Button-1>", function(){
		stateRFE <- if(tclvalue(down.rfe) == '1') 'disabled' else 'normal'
		tkconfigure(cb.grdrfe, state = stateRFE)
		tkconfigure(bt.grdrfe, state = stateRFE)

		stateGRD <- if(tclvalue(down.rfe) == '1') 'normal' else 'disabled'
		tkconfigure(cb.down.rfe, state = stateGRD)
		tkconfigure(grd_vlon1, state = stateGRD)
		tkconfigure(grd_vlon2, state = stateGRD)
		tkconfigure(grd_vlat1, state = stateGRD)
		tkconfigure(grd_vlat2, state = stateGRD)
	})

	############################################
	tkgrid(frDate, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frStnfl, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRfe, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################  RIGHT   #####################

	frBias <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	adj.bias <- tclVar(GeneralParameters$BIAS$Adjust)
	dir.bias <- tclVar(str_trim(GeneralParameters$BIAS$Dir))
	bias.method <- tclVar(str_trim(GeneralParameters$BIAS$method))
	cb.biasMthd <- c("Multiplicative.Bias.Var", "Multiplicative.Bias.Mon", "Quantile.Mapping")

	stateBiasdir <- if(tclvalue(adj.bias) == '1') 'normal' else 'disabled'
	chk.bias <- tkcheckbutton(frBias, variable = adj.bias, text = 'Perform bias correction', anchor = 'w', justify = 'left')
	txt.mth.bias <- tklabel(frBias, text = 'Bias method', anchor = 'w', justify = 'left')
	cb.mth.bias <- ttkcombobox(frBias, values = cb.biasMthd, textvariable = bias.method, state = stateBiasdir, width = largeur2)
	txt.dir.bias <- tklabel(frBias, text = "Directory of bias files", anchor = 'w', justify = 'left')
	en.dir.bias <- tkentry(frBias, textvariable = dir.bias, state = stateBiasdir, width = largeur0)
	bt.dir.bias <- tkbutton(frBias, text = "...", state = stateBiasdir)

	tkconfigure(bt.dir.bias, command = function(){
		dir4bias <- tk_choose.dir(str_trim(GeneralParameters$BIAS$Dir), "")
		tclvalue(dir.bias) <- if(!is.na(dir4bias)) dir4bias else ""
	})

	tkgrid(chk.bias, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.mth.bias, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.mth.bias, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.dir.bias, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.dir.bias, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.bias, row = 3, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(chk.bias, 'Check to perform a bias correction of RFE data')
	status.bar.display(chk.bias, TextOutputVar, 'Check to perform a bias correction of RFE data')
	infobulle(en.dir.bias, 'Enter the full path to directory containing the mean bias files')
	status.bar.display(en.dir.bias, TextOutputVar, 'Enter the full path to directory containing the mean bias files')
	infobulle(bt.dir.bias, 'or browse here')
	infobulle(cb.mth.bias, 'Method used to calculate Bias Factors or Parameters')
	status.bar.display(cb.mth.bias, TextOutputVar, 'Method used to calculate Bias Factors or Parameters')

	tkbind(chk.bias, "<Button-1>", function(){
		stateBS <- if(tclvalue(adj.bias) == '1') 'disabled' else 'normal'
		tkconfigure(cb.mth.bias, state = stateBS)
		tkconfigure(en.dir.bias, state = stateBS)
		tkconfigure(bt.dir.bias, state = stateBS)

	})

	############################################
	frMrg <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	cb.MrgMthd <- c("Regression Kriging", "Spatio-Temporal LM", "Simple Bias Adjustment")

	mrg.method <- tclVar(str_trim(GeneralParameters$Merging$mrg.method))
	mrg.min.stn <- tclVar(GeneralParameters$Merging$min.stn)
	mrg.min.non.zero <- tclVar(GeneralParameters$Merging$min.non.zero)

	dir.LMCoef <- tclVar(GeneralParameters$LMCOEF$dir.LMCoef)

	if(tclvalue(no.stnfl) == '0'){
		stateLMCoef <- if(str_trim(tclvalue(mrg.method)) == "Spatio-Temporal LM") 'normal' else 'disabled'
	}else stateLMCoef <- 'disabled'

	txt.mrg <- tklabel(frMrg, text = 'Merging method', anchor = 'w', justify = 'left')
	cb.mrg <- ttkcombobox(frMrg, values = cb.MrgMthd, textvariable = mrg.method, width = largeur2)
	bt.mrg.interp <- ttkbutton(frMrg, text = "Merging Interpolations Parameters")

	txt.min.nbrs.stn <- tklabel(frMrg, text = 'Min.Nb.Stn', anchor = 'e', justify = 'right')
	en.min.nbrs.stn <- tkentry(frMrg, width = 4, textvariable = mrg.min.stn, justify = 'right')
	txt.min.non.zero <- tklabel(frMrg, text = 'Min.No.Zero', anchor = 'e', justify = 'right')
	en.min.non.zero <- tkentry(frMrg, width = 4, textvariable = mrg.min.non.zero, justify = 'right')

	txt.dir.LM <- tklabel(frMrg, text = "Directory of LMCoef files", anchor = 'w', justify = 'left')
	en.dir.LM <- tkentry(frMrg, textvariable = dir.LMCoef, state = stateLMCoef, width = largeur0)
	bt.dir.LM <- tkbutton(frMrg, text = "...", state = stateLMCoef)

	tkconfigure(bt.mrg.interp, command = function(){
		GeneralParameters[["Merging"]] <<- getInterpolationPars(tt, GeneralParameters[["Merging"]], interpChoix = 0)
	})

	tkconfigure(bt.dir.LM, command = function(){
		dirLM <- tk_choose.dir(getwd(), "")
		tclvalue(dir.LMCoef) <- if(!is.na(dirLM)) dirLM else ""
	})

	tkgrid(txt.mrg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.mrg, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.mrg.interp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.min.nbrs.stn, row = 2, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.min.nbrs.stn, row = 2, column = 2, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.min.non.zero, row = 2, column = 3, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.min.non.zero, row = 2, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.dir.LM, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.dir.LM, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.LM, row = 4, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.mrg, 'Method to be used to perform merging')
	status.bar.display(cb.mrg, TextOutputVar, 'Method to be used to perform merging')

	infobulle(en.min.nbrs.stn, 'Minimum number of gauges with data to be used to do the merging')
	status.bar.display(en.min.nbrs.stn, TextOutputVar, 'Minimum number of gauges with data to be used to do the merging')
	infobulle(en.min.non.zero, 'Minimum number of non-zero gauge values to perform the merging')
	status.bar.display(en.min.non.zero, TextOutputVar, 'Minimum number of non-zero gauge values to perform the merging')

	infobulle(en.dir.LM, 'Enter the full path to directory containing the LM coefficients files')
	status.bar.display(en.dir.LM, TextOutputVar, 'Enter the full path to directory containing the LM coefficients files')
	infobulle(bt.dir.LM, 'or browse here')
	status.bar.display(bt.dir.LM, TextOutputVar, 'or browse here')

	###############
	tkbind(cb.mrg, "<<ComboboxSelected>>", function(){
		if(tclvalue(no.stnfl) == '0'){
			stateLM <- if(tclvalue(mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'
		}else stateLM <- 'disabled'
		tkconfigure(en.dir.LM, state = stateLM)
		tkconfigure(bt.dir.LM, state = stateLM)
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

	tkbind(chk.use.rnr, "<Button-1>", function(){
		stateRnoR <- if(tclvalue(use.RnoR) == '0') 'normal' else 'disabled'
		tkconfigure(en.maxdist.rnr, state = stateRnoR)
		tkconfigure(chk.smooth.rnr, state = stateRnoR)
	})

	######
	tkgrid(frBias, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMrg, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRnoR, row = 2, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################  RIGHT1   #####################

	frBlank <- tkframe(frRight1, relief = 'sunken', borderwidth = 2)

	file.grddem <- tclVar(str_trim(GeneralParameters$blank$DEM.file))
	file.blkshp <- tclVar(str_trim(GeneralParameters$blank$SHP.file))

	blankChx <- c("None", "Use DEM", "Use ESRI shapefile")
	blankGrd <- tclVar()
	tclvalue(blankGrd) <- switch(str_trim(GeneralParameters$blank$blank),
									'1' = blankChx[1],
									'2' = blankChx[2],
									'3' = blankChx[3])

	if(str_trim(GeneralParameters$blank$blank) == '2'){
		statedem <- if(tclvalue(no.stnfl) == '0') 'normal' else 'disabled'
	}else statedem <- 'disabled'
	if(str_trim(GeneralParameters$blank$blank) == '3'){
		stateshp <- if(tclvalue(no.stnfl) == '0') 'normal' else 'disabled'
	}else stateshp <- 'disabled'

	txt.blankGrd <- tklabel(frBlank, text = 'Blank', anchor = 'w', justify = 'left')
	cb.blankGrd <- ttkcombobox(frBlank, values = blankChx, textvariable = blankGrd, state = stateMrg, width = largeur2)
	txt.grddem <- tklabel(frBlank, text = "Elevation data(NetCDF)", anchor = 'w', justify = 'left')
	cb.grddem <- ttkcombobox(frBlank, values = unlist(listOpenFiles), textvariable = file.grddem, state = statedem, width = largeur1)
	bt.grddem <- tkbutton(frBlank, text = "...")
	txt.blkshp <- tklabel(frBlank, text = "ESRI shapefiles for blanking", anchor = 'w', justify = 'left')
	cb.blkshp <- ttkcombobox(frBlank, values = unlist(listOpenFiles), textvariable = file.blkshp, state = stateshp, width = largeur1)
	bt.blkshp <- tkbutton(frBlank, text = "...")

	tkconfigure(bt.grddem, state = statedem, command = function(){
		nc.opfiles <- getOpenNetcdf(tt, all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles
			tclvalue(file.grddem) <- AllOpenFilesData[[nopf+1]][[1]]
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]

			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
			tkconfigure(cb.grdrfe, values = unlist(listOpenFiles), textvariable = file.grdrfe)
			tkconfigure(cb.blkshp, values = unlist(listOpenFiles), textvariable = file.blkshp)
		}else return(NULL)
	})

	tkconfigure(bt.blkshp, state = stateshp, command = function(){
		shp.opfiles <- getOpenShp(tt, all.opfiles)
		if(!is.null(shp.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'shp'
			AllOpenFilesData[[nopf+1]] <<- shp.opfiles
			tclvalue(file.blkshp) <- AllOpenFilesData[[nopf+1]][[1]]
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]

			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
			tkconfigure(cb.grdrfe, values = unlist(listOpenFiles), textvariable = file.grdrfe)
			tkconfigure(cb.blkshp, values = unlist(listOpenFiles), textvariable = file.blkshp)
		}else return(NULL)
	})

	tkgrid(txt.blankGrd, row = 0, column = 0, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(cb.blankGrd, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 3, ipadx = 1, ipady = 1)

	tkgrid(txt.grddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.grddem, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.grddem, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.blkshp, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.blkshp, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.blkshp, row = 4, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.blankGrd, 'Blank grid outside the country boundaries or over ocean')
	status.bar.display(cb.blankGrd, TextOutputVar, 'Blank grid outside the country boundaries  or\nover ocean given by the DEM mask or the shapefile')
	infobulle(cb.grddem, 'Choose the file in the list')
	status.bar.display(cb.grddem, TextOutputVar, 'Choose the file containing the elevation data in netcdf')
	infobulle(bt.grddem, 'Browse file if not listed')
	status.bar.display(bt.grddem, TextOutputVar, 'Browse file if not listed')
	infobulle(cb.blkshp, 'Choose the file in the list')
	status.bar.display(cb.blkshp, TextOutputVar, 'Choose the file containing the ESRI shapefiles')
	infobulle(bt.blkshp, 'Browse file if not listed')
	status.bar.display(bt.blkshp, TextOutputVar, 'Browse file if not listed')

	tkbind(cb.blankGrd, "<<ComboboxSelected>>", function(){
		if(tclvalue(blankGrd) == "None"){
			tkconfigure(cb.grddem, state = 'disabled')
			tkconfigure(bt.grddem, state = 'disabled')
			tkconfigure(cb.blkshp, state = 'disabled')
			tkconfigure(bt.blkshp, state = 'disabled')
		}
		if(tclvalue(blankGrd) == "Use DEM"){
			tkconfigure(cb.grddem, state = 'normal')
			tkconfigure(bt.grddem, state = 'normal')
			tkconfigure(cb.blkshp, state = 'disabled')
			tkconfigure(bt.blkshp, state = 'disabled')
		}
		if(tclvalue(blankGrd) == "Use ESRI shapefile"){
			tkconfigure(cb.grddem, state = 'disabled')
			tkconfigure(bt.grddem, state = 'disabled')
			tkconfigure(cb.blkshp, state = 'normal')
			tkconfigure(bt.blkshp, state = 'normal')
		}
	})

	############################################
	
	frSavedir <- tkframe(frRight1, relief = 'sunken', borderwidth = 2)

	file.save1 <- tclVar(str_trim(GeneralParameters$output$dir))

	txt.file.save <- tklabel(frSavedir, text = 'Directory to save result', anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frSavedir, textvariable = file.save1, width = largeur0)
	bt.file.save <- tkbutton(frSavedir, text = "...")

	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(str_trim(GeneralParameters$output$dir), "")
		if(is.na(file2save1)) tclvalue(file.save1) <- str_trim(GeneralParameters$output$dir)
		else{
			dir.create(file2save1, showWarnings = FALSE, recursive = TRUE)
			tclvalue(file.save1) <- file2save1
		}
	})

	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(en.file.save, 'Enter the full path to directory to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save result')
	infobulle(bt.file.save, 'or browse here')

	############################################
	tkgrid(frBlank, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSavedir, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################

	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = 'news', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight1, row = 0, column = 2, sticky = 'news', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################
	
	bt.prm.OK <- tkbutton(frMRG1, text = "OK")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(file.stnfl)) == "" & str_trim(tclvalue(no.stnfl)) == "0"){
			tkmessageBox(message = "Choose the file containing the gauge data", icon = "warning", type = "ok")
		}else if(str_trim(tclvalue(file.grdrfe)) == "" & str_trim(tclvalue(down.rfe)) == "1"){
			tkmessageBox(message = "Choose RFE file", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(dir.bias))%in%c("", "NA") & tclvalue(adj.bias) == "1"){
			tkmessageBox(message = "Choose or enter the directory containing the Mean Bias files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if((tclvalue(mrg.method) == "Spatio-Temporal LM") & (tclvalue(no.stnfl) == "0") &
			str_trim(tclvalue(dir.LMCoef))%in%c("", "NA")){
			tkmessageBox(message = "Enter the path to directory containing the LM Coefficients", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(blankGrd)) == "Use DEM" & str_trim(tclvalue(file.grddem)) == ""){
			tkmessageBox(message = "You have to provide DEM data in NetCDF format", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(blankGrd)) == "Use ESRI shapefile" & str_trim(tclvalue(file.blkshp)) == ""){
			tkmessageBox(message = "You have to provide the shapefile", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.save1))%in%c("", "NA")){
			tkmessageBox(message = "Choose or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$Merging.Date$year <<- str_trim(tclvalue(istart.yrs))
			GeneralParameters$Merging.Date$month <<- str_trim(tclvalue(istart.mon))
			GeneralParameters$Merging.Date$dekad <<- str_trim(tclvalue(istart.day))

			GeneralParameters$STN$file <<- str_trim(tclvalue(file.stnfl))
			GeneralParameters$STN$No.Stn.Data <<- switch(str_trim(tclvalue(no.stnfl)), '0' = FALSE, '1' = TRUE)

			GeneralParameters$RFE$downloaded <<- switch(str_trim(tclvalue(down.rfe)), '0' = FALSE, '1' = TRUE)
			GeneralParameters$RFE$file <<- str_trim(tclvalue(file.grdrfe))
			GeneralParameters$RFE$source <<- str_trim(tclvalue(data.rfe))

			GeneralParameters$RFE.bbox$minlon <<- as.numeric(str_trim(tclvalue(minLon)))
			GeneralParameters$RFE.bbox$maxlon <<- as.numeric(str_trim(tclvalue(maxLon)))
			GeneralParameters$RFE.bbox$minlat <<- as.numeric(str_trim(tclvalue(minLat)))
			GeneralParameters$RFE.bbox$maxlat <<- as.numeric(str_trim(tclvalue(maxLat)))

			GeneralParameters$BIAS$Adjust <<- switch(str_trim(tclvalue(adj.bias)), '0' = FALSE, '1' = TRUE)
			GeneralParameters$BIAS$Dir <<- str_trim(tclvalue(dir.bias))
			GeneralParameters$BIAS$method <<- str_trim(tclvalue(bias.method))

			GeneralParameters$Merging$mrg.method <<- str_trim(tclvalue(mrg.method))
			GeneralParameters$Merging$min.stn <<- as.numeric(str_trim(tclvalue(mrg.min.stn)))
			GeneralParameters$Merging$min.non.zero <<- as.numeric(str_trim(tclvalue(mrg.min.non.zero)))
			GeneralParameters$LMCOEF$dir.LMCoef <<- str_trim(tclvalue(dir.LMCoef))

			GeneralParameters$RnoR$use.RnoR <<- switch(str_trim(tclvalue(use.RnoR)), '0' = FALSE, '1' = TRUE)
			GeneralParameters$RnoR$maxdist.RnoR <<- as.numeric(str_trim(tclvalue(maxdist.RnoR)))
			GeneralParameters$RnoR$smooth.RnoR <<- switch(str_trim(tclvalue(smooth.RnoR)), '0' = FALSE, '1' = TRUE)

			GeneralParameters$blank$blank <<- switch(str_trim(tclvalue(blankGrd)),
														"None" = '1', "Use DEM" = '2',
														"Use ESRI shapefile" = '3')
			GeneralParameters$blank$DEM.file <<- str_trim(tclvalue(file.grddem))
			GeneralParameters$blank$SHP.file <<- str_trim(tclvalue(file.blkshp))

			GeneralParameters$output$dir <<- str_trim(tclvalue(file.save1))

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

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5 - tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5 - tt.h*0.5)
	tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt)
	tkwm.title(tt, 'Merging data - Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

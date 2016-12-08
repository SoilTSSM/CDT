mergeDekadInfoRain <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows") largeur <- 29
	else largeur <- 27

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

	file.stnfl <- tclVar(str_trim(GeneralParameters$IO.files$STN.file))
	no.stnfl <- tclVar(GeneralParameters$No.Stn.Data)

	if(tclvalue(no.stnfl) == '0'){
		state.stnfl <- 'normal'
		stateMrg <- 'normal'
	}else{
		state.stnfl <- 'disabled'
		stateMrg <- 'disabled'
	}

	chk.stnfl <- tkcheckbutton(frStnfl, variable = no.stnfl, text = 'No station data available', anchor = 'w', justify = 'left')
	txt.stnfl <- tklabel(frStnfl, text = 'Station data file', anchor = 'w', justify = 'left')
	cb.stnfl <- ttkcombobox(frStnfl, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur, state = state.stnfl)
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
		}else{
			return(NULL)
		}
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
		if(tclvalue(no.stnfl) == '0'){
			tkconfigure(cb.stnfl, state = 'disabled')
			tkconfigure(bt.stnfl, state = 'disabled')
			##
			tkconfigure(cb.mrg, state = 'disabled')
			tkconfigure(en.dir.LM, state = 'disabled')
			tkconfigure(bt.dir.LM, state = 'disabled')
			tkconfigure(min.nbrs.stn.v, state = 'disabled')
			tkconfigure(min.non.zero.v, state = 'disabled')
			tkconfigure(cb.RnoR, state = 'disabled')
			tkconfigure(cb.RnoRs, state = 'disabled')
			tkconfigure(cb.Interp, state = 'disabled')
			tkconfigure(min.nbrs.v, state = 'disabled')
			tkconfigure(max.nbrs.v, state = 'disabled')
			tkconfigure(max.dst.v, state = 'disabled')
			tkconfigure(cb.blankGrd, state = 'disabled')
			tkconfigure(cb.grddem, state = 'disabled')
			tkconfigure(bt.grddem, state = 'disabled')
			tkconfigure(cb.blkshp, state = 'disabled')
			tkconfigure(bt.blkshp, state = 'disabled')
		}else{
			tkconfigure(cb.stnfl, state = 'normal')
			tkconfigure(bt.stnfl, state = 'normal')
			##
			tkconfigure(cb.mrg, state = 'normal')
			if(tclvalue(mrg.method) == "Spatio-Temporal LM"){
				tkconfigure(en.dir.LM, state = 'normal')
				tkconfigure(bt.dir.LM, state = 'normal')
			}else{
				tkconfigure(en.dir.LM, state = 'disabled')
				tkconfigure(bt.dir.LM, state = 'disabled')
			}
			tkconfigure(min.nbrs.stn.v, state = 'normal')
			tkconfigure(min.non.zero.v, state = 'normal')
			tkconfigure(cb.RnoR, state = 'normal')
			tkconfigure(cb.RnoRs, state = 'normal')
			tkconfigure(cb.Interp, state = 'normal')
			tkconfigure(min.nbrs.v, state = 'normal')
			tkconfigure(max.nbrs.v, state = 'normal')
			tkconfigure(max.dst.v, state = 'normal')
			tkconfigure(cb.blankGrd, state = 'normal')
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
		}
	})

	############################################
	
	frRfe <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	down.rfe <- tclVar(GeneralParameters$Downloaded.RFE)
	file.grdrfe <- tclVar(GeneralParameters$IO.files$RFE.file)

	data.rfe <- tclVar(GeneralParameters$RFE.data)
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
	cb.grdrfe <- ttkcombobox(frRfe, values = unlist(listOpenFiles), textvariable = file.grdrfe, width = largeur, state = stateRFE1)
	bt.grdrfe <- tkbutton(frRfe, text = "...", state = stateRFE1)

	frDown <- ttklabelframe(frRfe, text = "Download RFE", relief = 'groove')
	cb.down.rfe <- ttkcombobox(frDown, values = c('TAMSAT', 'CHIRP'), textvariable = data.rfe, state = stateRFE2)

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
		fileopen <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "",
								filetypes="{{NetCDF Files} {.nc .NC .cdf .CDF}} {{All files} *}"))
		if(fileopen == "" | is.na(fileopen)) return(NULL)
		nc.opfiles1 <- preview.data.nc(tt, fileopen, "")
		nc.opfiles <- list(basename(fileopen), nc.opfiles1, fileopen)
		if(!is.null(nc.opfiles1)){
			tkinsert(all.opfiles, "end", basename(fileopen))
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grdrfe) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
			tkconfigure(cb.grdrfe, values = unlist(listOpenFiles), textvariable = file.grdrfe)
			tkconfigure(cb.blkshp, values = unlist(listOpenFiles), textvariable = file.blkshp)
		}else{
			return(NULL)
		}
	})

	tkgrid(chk.rfe, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.grdrfe, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.grdrfe, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.grdrfe, row = 2, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frDown, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(cb.down.rfe, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frbbox, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

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
		if(tclvalue(down.rfe) == '1'){
			tkconfigure(cb.grdrfe, state = 'disabled')
			tkconfigure(bt.grdrfe, state = 'disabled')
			tkconfigure(cb.down.rfe, state = 'normal')
			tkconfigure(grd_vlon1, state = 'normal')
			tkconfigure(grd_vlon2, state = 'normal')
			tkconfigure(grd_vlat1, state = 'normal')
			tkconfigure(grd_vlat2, state = 'normal')
		}else{
			tkconfigure(cb.grdrfe, state = 'normal')
			tkconfigure(bt.grdrfe, state = 'normal')
			tkconfigure(cb.down.rfe, state = 'disabled')
			tkconfigure(grd_vlon1, state = 'disabled')
			tkconfigure(grd_vlon2, state = 'disabled')
			tkconfigure(grd_vlat1, state = 'disabled')
			tkconfigure(grd_vlat2, state = 'disabled')
		}
	})

	#######################  RIGHT   #####################

	frBias <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	adj.bias <- tclVar(GeneralParameters$Adjust.Bias)
	dir.bias <- tclVar(str_trim(GeneralParameters$IO.files$BIAS.dir))
	meanbsprefix <- tclVar(str_trim(GeneralParameters$Prefix$Mean.Bias.Prefix))
	bias.method <- tclVar(str_trim(GeneralParameters$Bias.Method))
	cb.biasMthd <- c("Quantile.Mapping", "Multiplicative.Bias.Var", "Multiplicative.Bias.Mon")

	if(tclvalue(adj.bias) == '1'){
		stateBiasdir <- 'normal'
		if(tclvalue(bias.method) == "Quantile.Mapping") stateBiaspfx <- 'disabled'
		else stateBiaspfx <- 'normal'
	}else{
		stateBiasdir <- 'disabled'
		stateBiaspfx <- 'disabled'
	}

	chk.bias <- tkcheckbutton(frBias, variable = adj.bias, text = 'Perform bias correction', anchor = 'w', justify = 'left')
	txt.mth.bias <- tklabel(frBias, text = 'Bias method', anchor = 'w', justify = 'left')
	cb.mth.bias <- ttkcombobox(frBias, values = cb.biasMthd, textvariable = bias.method, width = 12, state = stateBiasdir)
	txt.dir.bias <- tklabel(frBias, text = "Directory of bias files", anchor = 'w', justify = 'left')
	en.dir.bias <- tkentry(frBias, textvariable = dir.bias, width = largeur, state = stateBiasdir)
	bt.dir.bias <- tkbutton(frBias, text = "...", state = stateBiasdir)
	txt.prf.bias <- tklabel(frBias, text = 'Mean bias filename prefix', anchor = 'w', justify = 'left')
	en.prf.bias <- tkentry(frBias, textvariable = meanbsprefix, width = largeur, state = stateBiaspfx)

	tkconfigure(bt.dir.bias, command = function(){
		dir4bias <- tk_choose.dir(str_trim(GeneralParameters$IO.files$BIAS.dir), "")
		tclvalue(dir.bias) <- if(!is.na(dir4bias)) dir4bias else ""
	})

	tkgrid(chk.bias, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.mth.bias, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.mth.bias, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 3, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.dir.bias, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.dir.bias, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.bias, row = 3, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.prf.bias, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.prf.bias, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(chk.bias, 'Check to perform a bias correction of RFE data')
	status.bar.display(chk.bias, TextOutputVar, 'Check to perform a bias correction of RFE data')
	infobulle(en.dir.bias, 'Enter the full path to directory containing the mean bias files')
	status.bar.display(en.dir.bias, TextOutputVar, 'Enter the full path to directory containing the mean bias files')
	infobulle(bt.dir.bias, 'or browse here')
	infobulle(en.prf.bias, 'Prefix for the file name of the mean bias coefficient')
	status.bar.display(en.prf.bias, TextOutputVar, 'Prefix for the file name of the mean bias coefficient')
	infobulle(cb.mth.bias, 'Method used to calculate Bias Factors or Parameters')
	status.bar.display(cb.mth.bias, TextOutputVar, 'Method used to calculate Bias Factors or Parameters')

	tkbind(chk.bias, "<Button-1>", function(){
		if(tclvalue(adj.bias) == '1'){
			tkconfigure(cb.mth.bias, state = 'disabled')
			tkconfigure(en.dir.bias, state = 'disabled')
			tkconfigure(bt.dir.bias, state = 'disabled')
			tkconfigure(en.prf.bias, state = 'disabled')
		}else{
			tkconfigure(cb.mth.bias, state = 'normal')
			tkconfigure(en.dir.bias, state = 'normal')
			tkconfigure(bt.dir.bias, state = 'normal')
			if(tclvalue(bias.method) == "Quantile.Mapping") tkconfigure(en.prf.bias, state = 'disabled')
			else tkconfigure(en.prf.bias, state = 'normal')
		}
	})

	tkbind(cb.mth.bias,"<<ComboboxSelected>>", function(){
		if(tclvalue(adj.bias) == '1'){
			if(tclvalue(bias.method) == "Quantile.Mapping") tkconfigure(en.prf.bias, state = 'disabled')
			else tkconfigure(en.prf.bias, state = 'normal')
		}else tkconfigure(en.prf.bias, state = 'disabled')
	})

	############################################
	frMrg <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	min.stn <- tclVar(str_trim(GeneralParameters$Merging.pars$min.stn))
	min.non.zero <- tclVar(str_trim(GeneralParameters$Merging.pars$min.non.zero))
	use.RnoR <- tclVar(GeneralParameters$Merging.pars$use.RnoR)
	smooth.RnoR <- tclVar(GeneralParameters$Merging.pars$smooth.RnoR)

	mrg.method <- tclVar(str_trim(GeneralParameters$Mrg.Method))
	cb.MrgMthd <- c("Regression Kriging", "Spatio-Temporal LM")

	dir.LMCoef <- tclVar(GeneralParameters$IO.files$LMCoef.dir)
	if(str_trim(GeneralParameters$Mrg.Method) == "Regression Kriging") stateLMCoef <- 'disabled'
	else{
		if(tclvalue(no.stnfl) == '0') stateLMCoef <- 'normal'
		else stateLMCoef <- 'disabled'
	}

	txt.mrg <- tklabel(frMrg, text = 'Mering method', anchor = 'w', justify = 'left')
	cb.mrg <- ttkcombobox(frMrg, values = cb.MrgMthd, textvariable = mrg.method, state = stateMrg)
	txt.dir.LM <- tklabel(frMrg, text = "Directory of LMCoef files", anchor = 'w', justify = 'left')
	en.dir.LM <- tkentry(frMrg, textvariable = dir.LMCoef, width = largeur, state = stateLMCoef)
	bt.dir.LM <- tkbutton(frMrg, text = "...", state = stateLMCoef)
	frMrgPars <- ttklabelframe(frMrg, text = "Merging parameters", relief = 'groove')

	min.nbrs.stn.l <- tklabel(frMrgPars, text = 'Min.Nb.Stn', anchor = 'w', justify = 'left')
	min.nbrs.stn.v <- tkentry(frMrgPars, width = 4, textvariable = min.stn, justify = 'right', state = stateMrg)
	min.non.zero.l <- tklabel(frMrgPars, text = 'Min.No.Zero', anchor = 'w', justify = 'left')
	min.non.zero.v <- tkentry(frMrgPars, width = 4, textvariable = min.non.zero, justify = 'right', state = stateMrg)
	cb.RnoR <- tkcheckbutton(frMrgPars, variable = use.RnoR, text = 'Apply Rain-no-Rain mask', anchor = 'w', justify = 'left', state = stateMrg)
	cb.RnoRs <- tkcheckbutton(frMrgPars, variable = smooth.RnoR, text = 'Smooth Rain-no-Rain mask', anchor = 'w', justify = 'left', state = stateMrg)

	#####
	tkconfigure(bt.dir.LM, command = function(){
		dir4LM <- tk_choose.dir(GeneralParameters$IO.files$LMCoef.dir, "")
		tclvalue(dir.LMCoef) <- if(!is.na(dir4LM)) dir4LM else ""
	})
	#####

	tkgrid(txt.mrg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.mrg, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.dir.LM, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.dir.LM, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.LM, row = 2, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMrgPars, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(min.nbrs.stn.l, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(min.nbrs.stn.v, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(min.non.zero.l, row = 0, column = 2, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(min.non.zero.v, row = 0, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.RnoR, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.RnoRs, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.mrg, 'Method to be used to perform merging')
	status.bar.display(cb.mrg, TextOutputVar, 'Method to be used to perform merging')
	infobulle(en.dir.LM, 'Enter the full path to directory containing the LM coefficients files')
	status.bar.display(en.dir.LM, TextOutputVar, 'Enter the full path to directory containing the LM coefficients files')
	infobulle(bt.dir.LM, 'or browse here')
	status.bar.display(bt.dir.LM, TextOutputVar, 'or browse here')

	infobulle(min.nbrs.stn.l, 'Minimum number of gauges with data to be used to do the merging')
	status.bar.display(min.nbrs.stn.l, TextOutputVar, 'Minimum number of gauges with data to be used to do the merging')
	infobulle(min.nbrs.stn.v, 'Minimum number of gauges with data to be used to do the merging')
	status.bar.display(min.nbrs.stn.v, TextOutputVar, 'Minimum number of gauges with data to be used to do the merging')
	infobulle(min.non.zero.l, 'Minimum number of non-zero gauge values to perform the merging')
	status.bar.display(min.non.zero.l, TextOutputVar, 'Minimum number of non-zero gauge values to perform the merging')
	infobulle(min.non.zero.v, 'Minimum number of non-zero gauge values to perform the merging')
	status.bar.display(min.non.zero.v, TextOutputVar, 'Minimum number of non-zero gauge values to perform the merging')
	#####

	tkbind(cb.mrg, "<<ComboboxSelected>>", function(){
		if(tclvalue(mrg.method) == "Spatio-Temporal LM"){
			tkconfigure(en.dir.LM, state = 'normal')
			tkconfigure(bt.dir.LM, state = 'normal')
		}else{
			tkconfigure(en.dir.LM, state = 'disabled')
			tkconfigure(bt.dir.LM, state = 'disabled')
		}
	})


	#######################  RIGHT1   #####################

	frInterp <- tkframe(frRight1, relief = 'sunken', borderwidth = 2)

	interp.method <- tclVar()
	cb.InterpVAL <- c('Ordinary Kriging', 'Inverse Distance Weighted')
	tclvalue(interp.method) <- switch(GeneralParameters$Interpolation.pars$interp.method, 
										'Kriging' = cb.InterpVAL[1], 
										'IDW' = cb.InterpVAL[2])
	nmin <- tclVar(GeneralParameters$Interpolation.pars$nmin)
	nmax <- tclVar(GeneralParameters$Interpolation.pars$nmax)
	maxdist <- tclVar(GeneralParameters$Interpolation.pars$maxdist)

	txt.Interp <- tklabel(frInterp, text = 'Interpolation method', anchor = 'w', justify = 'left')
	cb.Interp <- ttkcombobox(frInterp, values = cb.InterpVAL, textvariable = interp.method, state = stateMrg)
	frIDW <- ttklabelframe(frInterp, text = "Interpolation parameters", relief = 'groove')

	########
	min.nbrs.l <- tklabel(frIDW, text = 'nmin', anchor = 'e', justify = 'right')
	max.nbrs.l <- tklabel(frIDW, text = 'nmax', anchor = 'e', justify = 'right')
	max.dst.l <- tklabel(frIDW, text = 'maxdist', anchor = 'e', justify = 'right')
	min.nbrs.v <- tkentry(frIDW, width = 4, textvariable = nmin, justify = 'right', state = stateMrg)
	max.nbrs.v <- tkentry(frIDW, width = 4, textvariable = nmax, justify = 'right', state = stateMrg)
	max.dst.v <- tkentry(frIDW, width = 4, textvariable = maxdist, justify = 'right', state = stateMrg)

	########

	tkgrid(min.nbrs.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.nbrs.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.nbrs.l, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.nbrs.v, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dst.l, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dst.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)

	########

	tkgrid(txt.Interp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.Interp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frIDW, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)

	########

	infobulle(min.nbrs.v, 'Minimum number of neighbors to be used to interpolate data')
	status.bar.display(min.nbrs.v, TextOutputVar, 'Minimum number of neighbors to be used to interpolate data')
	infobulle(max.nbrs.v, 'Maximum number of neighbors to be used to interpolate data')
	status.bar.display(max.nbrs.v, TextOutputVar, 'Maximum number of neighbors to be used to interpolate data')
	infobulle(max.dst.v, 'Maximum distance (in  decimal degree) to be used to interpolate data')
	status.bar.display(max.dst.v, TextOutputVar, 'Maximum distance (in  decimal degree) to be used to interpolate data')

	###########################################

	frBlank <- tkframe(frRight1, relief = 'sunken', borderwidth = 2)

	blankGrd <- tclVar(str_trim(GeneralParameters$Blank.Grid))
	file.grddem <- tclVar(str_trim(GeneralParameters$IO.files$DEM.file))
	file.blkshp <- tclVar(str_trim(GeneralParameters$IO.files$SHP.file))

	blankChx <- c("None", "Use DEM", "Use ESRI shapefile")
	tclvalue(blankGrd) <- switch(str_trim(GeneralParameters$Blank.Grid),
									'1' = blankChx[1],
									'2' = blankChx[2],
									'3' = blankChx[3])

	if(str_trim(GeneralParameters$Blank.Grid) == '2'){
		if(tclvalue(no.stnfl) == '0') statedem <- 'normal'
		else statedem <- 'disabled'
	}else statedem <- 'disabled'
	if(str_trim(GeneralParameters$Blank.Grid) == '3'){
		if(tclvalue(no.stnfl) == '0') stateshp <- 'normal'
		else stateshp <- 'disabled'
	}else stateshp <- 'disabled'

	txt.blankGrd <- tklabel(frBlank, text = 'Blank', anchor = 'w', justify = 'left')
	cb.blankGrd <- ttkcombobox(frBlank, values = blankChx, textvariable = blankGrd, state = stateMrg)
	txt.grddem <- tklabel(frBlank, text = "Elevation data(NetCDF)", anchor = 'w', justify = 'left')
	cb.grddem <- ttkcombobox(frBlank, values = unlist(listOpenFiles), textvariable = file.grddem, state = statedem, width = largeur-1)
	bt.grddem <- tkbutton(frBlank, text = "...")
	txt.blkshp <- tklabel(frBlank, text = "ESRI shapefiles for blanking", anchor = 'w', justify = 'left')
	cb.blkshp <- ttkcombobox(frBlank, values = unlist(listOpenFiles), textvariable = file.blkshp, state = stateshp, width = largeur-1)
	bt.blkshp <- tkbutton(frBlank, text = "...")

	tkconfigure(bt.grddem, state = statedem, command = function(){
		nc.opfiles <- getOpenNetcdf(tt, all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles
			tclvalue(file.grddem) <- AllOpenFilesData[[nopf+1]][[1]]

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.grddem) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
			tkconfigure(cb.grddem, values = unlist(listOpenFiles), textvariable = file.grddem)
			tkconfigure(cb.grdrfe, values = unlist(listOpenFiles), textvariable = file.grdrfe)
			tkconfigure(cb.blkshp, values = unlist(listOpenFiles), textvariable = file.blkshp)
		}else{
			return(NULL)
		}
	})

	tkconfigure(bt.blkshp, state = stateshp, command = function(){
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
		}else{
			return(NULL)
		}
	})

	tkgrid(txt.blankGrd, row = 0, column = 0, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(cb.blankGrd, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

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

	tkbind(cb.blankGrd,"<<ComboboxSelected>>", function(){
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

	file.save1 <- tclVar(str_trim(GeneralParameters$IO.files$dir2save))

	txt.file.save <- tklabel(frSavedir, text = 'Directory to save result', anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frSavedir, textvariable = file.save1, width = largeur)
	bt.file.save <- tkbutton(frSavedir, text = "...")

	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(str_trim(GeneralParameters$IO.files$dir2save), "")
			if(is.na(file2save1)) tclvalue(file.save1) <- str_trim(GeneralParameters$IO.files$dir2save)
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
	tkgrid(frDate, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frStnfl, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRfe, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	######
	tkgrid(frBias, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMrg, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	######
	tkgrid(frInterp, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frBlank, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSavedir, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################

	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = 'news', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight1, row = 0, column = 2, sticky = 'news', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################
	
	bt.prm.OK <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(file.stnfl)) == "" & str_trim(tclvalue(no.stnfl)) == "0"){
			tkmessageBox(message = "Choose the file containing the gauge data", icon = "warning", type = "ok")
		}else if(str_trim(tclvalue(file.grdrfe)) == ""  & str_trim(tclvalue(down.rfe)) == "1"){
			tkmessageBox(message = "Choose RFE file", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if((str_trim(tclvalue(dir.bias)) == "" | str_trim(tclvalue(dir.bias)) == "NA") & (tclvalue(adj.bias) == "1")){
			tkmessageBox(message = "Choose or enter the  directory containing the Mean Bias files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if((tclvalue(mrg.method)  ==  "Spatio-Temporal LM") & (tclvalue(no.stnfl) == "0") &
			(str_trim(tclvalue(dir.LMCoef)) == "" | str_trim(tclvalue(dir.LMCoef)) == "NA")){
			tkmessageBox(message = "Enter the path to directory containing the LM Coefficients", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(blankGrd)) == "Use DEM" & str_trim(tclvalue(file.grddem)) == ""){
			tkmessageBox(message = "You have to provide DEM data in NetCDF format", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(blankGrd)) == "Use ESRI shapefile" & str_trim(tclvalue(file.blkshp)) == ""){
			tkmessageBox(message = "You have to provide the shapefile", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.save1)) == "" | str_trim(tclvalue(file.save1)) == "NA"){
			tkmessageBox(message = "Choose or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			GeneralParameters$Merging.Date$year <<- str_trim(tclvalue(istart.yrs))
			GeneralParameters$Merging.Date$month <<- str_trim(tclvalue(istart.mon))
			GeneralParameters$Merging.Date$dekad <<- str_trim(tclvalue(istart.day))

			GeneralParameters$IO.files$STN.file <<- str_trim(tclvalue(file.stnfl))
			GeneralParameters$IO.files$RFE.file <<- str_trim(tclvalue(file.grdrfe))
			GeneralParameters$IO.files$dir2save <<- str_trim(tclvalue(file.save1))
			GeneralParameters$IO.files$BIAS.dir <<- str_trim(tclvalue(dir.bias))
			GeneralParameters$IO.files$DEM.file <<- str_trim(tclvalue(file.grddem))
			GeneralParameters$IO.files$SHP.file <<- str_trim(tclvalue(file.blkshp))
			GeneralParameters$IO.files$LMCoef.dir <<- str_trim(tclvalue(dir.LMCoef))

			GeneralParameters$Bias.Method <<- str_trim(tclvalue(bias.method))
			GeneralParameters$Prefix$Mean.Bias.Prefix <<- str_trim(tclvalue(meanbsprefix))

			GeneralParameters$RFE.data <<- str_trim(tclvalue(data.rfe))
			GeneralParameters$No.Stn.Data <<- switch(str_trim(tclvalue(no.stnfl)), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Downloaded.RFE <<- switch(str_trim(tclvalue(down.rfe)), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Adjust.Bias <<- switch(str_trim(tclvalue(adj.bias)), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Blank.Grid <<- switch(str_trim(tclvalue(blankGrd)), 
																"None" =  '1',
																"Use DEM" = '2',
																"Use ESRI shapefile" = '3')

			GeneralParameters$RFE.bbox$minlon <<- as.numeric(str_trim(tclvalue(minLon)))
			GeneralParameters$RFE.bbox$maxlon <<- as.numeric(str_trim(tclvalue(maxLon)))
			GeneralParameters$RFE.bbox$minlat <<- as.numeric(str_trim(tclvalue(minLat)))
			GeneralParameters$RFE.bbox$maxlat <<- as.numeric(str_trim(tclvalue(maxLat)))

			GeneralParameters$Interpolation.pars$interp.method <<- switch(str_trim(tclvalue(interp.method)),
																		'Inverse Distance Weighted' = 'IDW',
																		'Ordinary Kriging' = 'Kriging')
			GeneralParameters$Interpolation.pars$maxdist <<- as.numeric(str_trim(tclvalue(maxdist)))
			GeneralParameters$Interpolation.pars$nmin <<- as.numeric(str_trim(tclvalue(nmin)))
			GeneralParameters$Interpolation.pars$nmax <<- as.numeric(str_trim(tclvalue(nmax)))

			GeneralParameters$Mrg.Method <<- str_trim(tclvalue(mrg.method))
			GeneralParameters$Merging.pars$min.stn <<- as.numeric(str_trim(tclvalue(min.stn)))
			GeneralParameters$Merging.pars$min.non.zero <<- as.numeric(str_trim(tclvalue(min.non.zero)))
			GeneralParameters$Merging.pars$use.RnoR <<- switch(str_trim(tclvalue(use.RnoR)), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Merging.pars$smooth.RnoR <<- switch(str_trim(tclvalue(smooth.RnoR)), '0' = FALSE, '1' = TRUE)

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

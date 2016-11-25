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

	if(tclvalue(no.stnfl) == '0') state.stnfl <- 'normal'
	else state.stnfl <- 'disabled'

	txt.stnfl <- tklabel(frStnfl, text = 'Station data file', anchor = 'w', justify = 'left')
	cb.stnfl <- ttkcombobox(frStnfl, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur-1, state = state.stnfl)
	bt.stnfl <- tkbutton(frStnfl, text = "...", state = state.stnfl)
	chk.stnfl <- tkcheckbutton(frStnfl, variable = no.stnfl, text = 'No station data', anchor = 'w', justify = 'left')
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
		}else{
			return(NULL)
		}
	})

	tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.stnfl, 'Choose the file in the list')
	status.bar.display(cb.stnfl, TextOutputVar, 'Choose the file containing the gauge data')
	infobulle(bt.stnfl, 'Browse file if not listed')
	status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')
	infobulle(chk.stnfl, 'Check if there is no station data available')
	status.bar.display(chk.stnfl, TextOutputVar, 'Check if there is no station data available')

	tkbind(chk.stnfl, "<Button-1>", function(){
		if(tclvalue(no.stnfl) == '0'){
			tkconfigure(cb.stnfl, state = 'disabled')
			tkconfigure(bt.stnfl, state = 'disabled')
		}else{
			tkconfigure(cb.stnfl, state = 'normal')
			tkconfigure(bt.stnfl, state = 'normal')
		}
	})

	############################################
	
	frRfe <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	down.rfe <- tclVar(GeneralParameters$Downloaded.RFE)
	dir.rfe <- tclVar(str_trim(GeneralParameters$IO.files$RFE.dir))
	rfeflformat <- tclVar(str_trim(GeneralParameters$Prefix$RFE.File.Format))
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
	txt.dir.rfe <- tklabel(frRfe, text = 'Directory of RFE files', anchor = 'w', justify = 'left')
	en.dir.rfe <- tkentry(frRfe, textvariable = dir.rfe, width = largeur, state = stateRFE1)
	bt.dir.rfe <- tkbutton(frRfe, text = "...", state = stateRFE1)
	txt.flfrt.rfe <- tklabel(frRfe, text = 'Input RFE filename format', anchor = 'w', justify = 'left')
	en.flfrt.rfe <- tkentry(frRfe, textvariable = rfeflformat, width = largeur, state = stateRFE1)

	txt.down.rfe <- tklabel(frRfe, text = 'Download RFE', anchor = 'w', justify = 'left')
	cb.down.rfe <- ttkcombobox(frRfe, values = c('TAMSAT', 'CHIRP'), textvariable = data.rfe, state = stateRFE2)
	txt.lon <- tklabel(frRfe, text = "Longitude", anchor = 'e', justify = 'right')
	txt.lat <- tklabel(frRfe, text = "Latitude", anchor = 'e', justify = 'right')
	txt.min <- tklabel(frRfe, text = "Minimum")
	txt.max <- tklabel(frRfe, text = "Maximum")

	grd_vlon1 <- tkentry(frRfe, width = 8, justify = "right", textvariable = minLon, state = stateRFE2)
	grd_vlon2 <- tkentry(frRfe, width = 8, justify = "right", textvariable = maxLon, state = stateRFE2)
	grd_vlat1 <- tkentry(frRfe, width = 8, justify = "right", textvariable = minLat, state = stateRFE2)
	grd_vlat2 <- tkentry(frRfe, width = 8, justify = "right", textvariable = maxLat, state = stateRFE2)

	tkconfigure(bt.dir.rfe, command = function(){
		dir4rfe <- tk_choose.dir(str_trim(GeneralParameters$IO.files$RFE.dir), "")
		if(is.na(dir4rfe)) tclvalue(dir.rfe) <- ""
		else tclvalue(dir.rfe) <- dir4rfe
	})

	sep4 <- ttkseparator(frRfe)
	sep5 <- ttkseparator(frRfe)

	tkgrid(chk.rfe, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.dir.rfe, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.dir.rfe, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.rfe, row = 2, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.flfrt.rfe, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.flfrt.rfe, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep4, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 5)
	tkgrid(txt.down.rfe, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.down.rfe, row = 7, column = 0, sticky = 'w', rowspan = 1, columnspan = 3, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(sep5, row = 8, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, pady = 5)
	tkgrid(txt.min, row = 9, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.max, row = 9, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.lon, row = 10, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon1, row = 10, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon2, row = 10, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.lat, row = 11, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat1, row = 11, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat2, row = 11, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(chk.rfe, 'Check if you have already downloaded RFE data')
	status.bar.display(chk.rfe, TextOutputVar, 'Check if you have already downloaded RFE data')
	infobulle(en.dir.rfe, 'Enter the full path to\ndirectory containing the RFE file\n(already extracted over the area\nof interest)')
	status.bar.display(en.dir.rfe, TextOutputVar, 'Full path to directory of RFE file,  already extracted over the region')
	infobulle(bt.dir.rfe, 'or browse here')
	infobulle(en.flfrt.rfe, 'Enter the format of the RFE files names in NetCDF,\nexample: rfe2016_01-dk2.nc')
	status.bar.display(en.flfrt.rfe, TextOutputVar, 'Enter the format of the RFE files names in NetCDF, example: rfe1983_01-dk2.nc')
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
			tkconfigure(en.dir.rfe, state = 'disabled')
			tkconfigure(bt.dir.rfe, state = 'disabled')
			tkconfigure(en.flfrt.rfe, state = 'disabled')
			tkconfigure(cb.down.rfe, state = 'normal')
			tkconfigure(grd_vlon1, state = 'normal')
			tkconfigure(grd_vlon2, state = 'normal')
			tkconfigure(grd_vlat1, state = 'normal')
			tkconfigure(grd_vlat2, state = 'normal')
		}else{
			tkconfigure(en.dir.rfe, state = 'normal')
			tkconfigure(bt.dir.rfe, state = 'normal')
			tkconfigure(en.flfrt.rfe, state = 'normal')
			tkconfigure(cb.down.rfe, state = 'disabled')
			tkconfigure(grd_vlon1, state = 'disabled')
			tkconfigure(grd_vlon2, state = 'disabled')
			tkconfigure(grd_vlat1, state = 'disabled')
			tkconfigure(grd_vlat2, state = 'disabled')
		}
	})

	############################################
	
	frSavedir <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

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

	#######################  RIGHT   #####################

	frBias <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	adj.bias <- tclVar(GeneralParameters$Adjust.Bias)
	dir.bias <- tclVar(str_trim(GeneralParameters$IO.files$BIAS.dir))
	meanbsprefix <- tclVar(str_trim(GeneralParameters$Prefix$Mean.Bias.Prefix))

	if(tclvalue(adj.bias) == '1') stateBias <- 'normal'
	else stateBias <- 'disabled'

	chk.bias <- tkcheckbutton(frBias, variable = adj.bias, text = 'Perform bias correction', anchor = 'w', justify = 'left')
	txt.dir.bias <- tklabel(frBias, text = "Directory of mean bias files", anchor = 'w', justify = 'left')
	en.dir.bias <- tkentry(frBias, textvariable = dir.bias, width = largeur, state = stateBias)
	bt.dir.bias <- tkbutton(frBias, text = "...", state = stateBias)
	txt.prf.bias <- tklabel(frBias, text = 'Mean bias filename prefix', anchor = 'w', justify = 'left')
	en.prf.bias <- tkentry(frBias, textvariable = meanbsprefix, width = largeur, state = stateBias)

	tkconfigure(bt.dir.bias, command = function(){
		dir4bias <- tk_choose.dir(str_trim(GeneralParameters$IO.files$BIAS.dir), "")
		if(is.na(dir4bias)) tclvalue(dir.bias) <- ""
		else tclvalue(dir.bias) <- dir4bias
	})

	tkgrid(chk.bias, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.dir.bias, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.dir.bias, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.bias, row = 2, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.prf.bias, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.prf.bias, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(chk.bias, 'Check to perform a bias correction of RFE data')
	status.bar.display(chk.bias, TextOutputVar, 'Check to perform a bias correction of RFE data')
	infobulle(en.dir.bias, 'Enter the full path to directory containing the mean bias files')
	status.bar.display(en.dir.bias, TextOutputVar, 'Enter the full path to directory containing the mean bias files')
	infobulle(bt.dir.bias, 'or browse here')
	infobulle(en.prf.bias, 'Prefix for the file name of the mean bias coefficient')
	status.bar.display(en.prf.bias, TextOutputVar, 'Prefix for the file name of the mean bias coefficient')

	tkbind(chk.bias, "<Button-1>", function(){
		if(tclvalue(adj.bias) == '1'){
			tkconfigure(en.dir.bias, state = 'disabled')
			tkconfigure(bt.dir.bias, state = 'disabled')
			tkconfigure(en.prf.bias, state = 'disabled')
		}else{
			tkconfigure(en.dir.bias, state = 'normal')
			tkconfigure(bt.dir.bias, state = 'normal')
			tkconfigure(en.prf.bias, state = 'normal')
		}
	})
	############################################

	frIntrp <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	interpMethod <- tclVar(str_trim(GeneralParameters$Interpolation.pars$Interp.Method))
	RainNoRain <- tclVar(str_trim(GeneralParameters$Merging.pars$Rain.no.Rain))
	nmin <- tclVar(str_trim(GeneralParameters$Merging.pars$min.Stn))
	min.non.zero <- tclVar(str_trim(GeneralParameters$Merging.pars$min.Stn.non0))
	max.rnr.dst <- tclVar(str_trim(GeneralParameters$Merging.pars$max.RnR.dist))
	max.dst <- tclVar(str_trim(GeneralParameters$Interpolation.pars$maxdist))
	min.nbrs <- tclVar(str_trim(GeneralParameters$Interpolation.pars$nmin))
	max.nbrs <- tclVar(str_trim(GeneralParameters$Interpolation.pars$nmax))

	nmin.l <- tklabel(frIntrp, text = 'NminStn', anchor = 'e', justify = 'right')
	nmin.v <- tkentry(frIntrp, width = 4, textvariable = nmin, justify = 'right')
	min.non.zero.l <- tklabel(frIntrp, text = 'NminNo0', anchor = 'e', justify = 'right')
	min.non.zero.v <- tkentry(frIntrp, width = 4, textvariable = min.non.zero, justify = 'right')

	txt.RainNoRain <- tklabel(frIntrp, text = 'Rain-no-Rain mask', anchor = 'w', justify = 'left')
	cb.RainNoRain <- ttkcombobox(frIntrp, values = c('None', 'Gauge', 'Satellite', 'GaugeSatellite'), textvariable = RainNoRain)
	max.rnr.dst.l <- tklabel(frIntrp, text = 'MaxRnRDist', anchor = 'e', justify = 'right')
	max.rnr.dst.v <- tkentry(frIntrp, width = 4, textvariable = max.rnr.dst, justify = 'right')

	txt.interpMethod <- tklabel(frIntrp, text = 'Interpolation method', anchor = 'w', justify = 'left')
	cb.interpMethod <- ttkcombobox(frIntrp, values = c('IDW', 'Kriging'), textvariable = interpMethod)

	min.nbrs.l <- tklabel(frIntrp, text = 'MinStn', anchor = 'e', justify = 'right')
	min.nbrs.v <- tkentry(frIntrp, width = 4, textvariable = min.nbrs, justify = 'right')
	max.nbrs.l <- tklabel(frIntrp, text = 'MaxStn', anchor = 'e', justify = 'right')
	max.nbrs.v <- tkentry(frIntrp, width = 4, textvariable = max.nbrs, justify = 'right')
	max.dst.l <- tklabel(frIntrp, text = 'MaxDist', anchor = 'e', justify = 'right')
	max.dst.v <- tkentry(frIntrp, width = 4, textvariable = max.dst, justify = 'right')

	sep1 <- ttkseparator(frIntrp)
	sep2 <- ttkseparator(frIntrp)
	sep3 <- ttkseparator(frIntrp)

	tkgrid(txt.RainNoRain, row = 0, column = 0, sticky = 'w', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.RainNoRain, row = 1, column = 0, sticky = 'w', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(max.rnr.dst.l, row = 0, column = 3, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(max.rnr.dst.v, row = 1, column = 3, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(sep1, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, pady = 2)

	tkgrid(nmin.l, row = 3, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(nmin.v, row = 3, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(min.non.zero.l, row = 3, column = 2, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(min.non.zero.v, row = 3, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep2, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, pady = 2)

	tkgrid(txt.interpMethod, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.interpMethod, row = 6, column = 0, sticky = 'w', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(sep3, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, pady = 2)

	tkgrid(min.nbrs.l, row = 8, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(min.nbrs.v, row = 8, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(max.nbrs.l, row = 8, column = 2, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(max.nbrs.v, row = 8, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(max.dst.l, row = 9, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(max.dst.v, row = 9, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.interpMethod, 'Interpolation techniques: Kriging or Inverse Distance Weighted')
	status.bar.display(cb.interpMethod, TextOutputVar, 'Interpolation techniques: Kriging or Inverse Distance Weighted')
	infobulle(cb.RainNoRain, 'Mask applied to handle no rain')
	status.bar.display(cb.RainNoRain, TextOutputVar, 'Mask applied to handle no rain')
	infobulle(nmin.l, 'Minimum number of gauges with data to be used to do the merging')
	status.bar.display(nmin.l, TextOutputVar, 'Minimum number of gauges with data to be used to do the merging')
	infobulle(nmin.v, 'Minimum number of gauges with data to be used to do the merging')
	status.bar.display(nmin.v, TextOutputVar, 'Minimum number of gauges with data to be used to do the merging')
	infobulle(min.non.zero.l, 'Minimum number of non-zero gauge values to perform the merging')
	status.bar.display(min.non.zero.l, TextOutputVar, 'Minimum number of non-zero gauge values to perform the merging')
	infobulle(min.non.zero.v, 'Minimum number of non-zero gauge values to perform the merging')
	status.bar.display(min.non.zero.v, TextOutputVar, 'Minimum number of non-zero gauge values to perform the merging')
	infobulle(max.rnr.dst.l, 'Maximum distance (in decimal degrees) for interpolating Rain-noRain mask')
	status.bar.display(max.rnr.dst.l, TextOutputVar, 'Maximum distance (in decimal degrees) for interpolating Rain-noRain mask')
	infobulle(max.rnr.dst.v, 'Maximum distance (in decimal degrees) for interpolating Rain-noRain mask')
	status.bar.display(max.rnr.dst.v, TextOutputVar, 'Maximum distance (in decimal degrees) for interpolating Rain-noRain mask')
	infobulle(max.dst.l, 'Maximum distance (in  decimal degree) to be used to interpolate data')
	status.bar.display(max.dst.l, TextOutputVar, 'Maximum distance (in  decimal degree) to be used to interpolate data')
	infobulle(max.dst.v, 'Maximum distance (in  decimal degree) to be used to interpolate data')
	status.bar.display(max.dst.v, TextOutputVar, 'Maximum distance (in  decimal degree) to be used to interpolate data')
	infobulle(min.nbrs.l, 'Minimum number of neighbours to be used to interpolate data')
	status.bar.display(min.nbrs.l, TextOutputVar, 'Minimum number of neighbours to be used to interpolate data')
	infobulle(min.nbrs.v, 'Minimum number of neighbours to be used to interpolate data')
	status.bar.display(min.nbrs.v, TextOutputVar, 'Minimum number of neighbours to be used to interpolate data')
	infobulle(max.nbrs.l, 'Maximum number of neighbours to be used to interpolate data')
	status.bar.display(max.nbrs.l, TextOutputVar, 'Maximum number of neighbours to be used to interpolate data')
	infobulle(max.nbrs.v, 'Maximum number of neighbours to be used to interpolate data')
	status.bar.display(max.nbrs.v, TextOutputVar, 'Maximum number of neighbours to be used to interpolate data')

	###########################################

	frBlank <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	blankGrd <- tclVar(str_trim(GeneralParameters$Blank.Grid))
	file.grddem <- tclVar(str_trim(GeneralParameters$IO.files$DEM.file))
	file.blkshp <- tclVar(str_trim(GeneralParameters$IO.files$SHP.file))

	blankChx <- c("None", "Use DEM", "Use ESRI shapefile")
	tclvalue(blankGrd) <- switch(str_trim(GeneralParameters$Blank.Grid),
									'1' = blankChx[1],
									'2' = blankChx[2],
									'3' = blankChx[3])

	if(str_trim(GeneralParameters$Blank.Grid) == '2') statedem <- 'normal'
	else statedem <- 'disabled'
	if(str_trim(GeneralParameters$Blank.Grid) == '3') stateshp <- 'normal'
	else stateshp <- 'disabled'

	txt.blankGrd <- tklabel(frBlank, text = 'Blank', anchor = 'w', justify = 'left')
	cb.blankGrd <- ttkcombobox(frBlank, values = blankChx, textvariable = blankGrd)
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
	status.bar.display(cb.blankGrd, TextOutputVar, 'Blank grid outside the country boundaries  or over ocean given by the DEM mask or the shapefile')
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
	tkgrid(frDate, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frStnfl, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRfe, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSavedir, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	######
	tkgrid(frBias, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frIntrp, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frBlank, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################

	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################
	
	bt.prm.OK <- tkbutton(frMRG1, text=" OK ")
	bt.prm.CA <- tkbutton(frMRG1, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(file.stnfl)) == "" & str_trim(tclvalue(no.stnfl)) == "0"){
			tkmessageBox(message = "Choose the file containing the gauge data", icon = "warning", type = "ok")
		}else if((str_trim(tclvalue(dir.rfe)) == "" | str_trim(tclvalue(dir.rfe)) == "NA") & (str_trim(tclvalue(down.rfe)) == "1")){
			tkmessageBox(message = "Choose or enter the  directory containing the RFE files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if((str_trim(tclvalue(dir.bias)) == "" | str_trim(tclvalue(dir.bias)) == "NA") & (str_trim(tclvalue(adj.bias)) == "1")){
			tkmessageBox(message = "Choose or enter the  directory containing the Mean Bias files", icon = "warning", type = "ok")
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
			GeneralParameters$IO.files$RFE.dir <<- str_trim(tclvalue(dir.rfe))
			GeneralParameters$IO.files$dir2save <<- str_trim(tclvalue(file.save1))
			GeneralParameters$IO.files$BIAS.dir <<- str_trim(tclvalue(dir.bias))
			GeneralParameters$IO.files$DEM.file <<- str_trim(tclvalue(file.grddem))
			GeneralParameters$IO.files$SHP.file <<- str_trim(tclvalue(file.blkshp))

			GeneralParameters$Prefix$RFE.File.Format <<- str_trim(tclvalue(rfeflformat))
			GeneralParameters$Prefix$Mean.Bias.Prefix <<- str_trim(tclvalue(meanbsprefix))

			GeneralParameters$No.Stn.Data <<- str_trim(tclvalue(no.stnfl))
			GeneralParameters$Downloaded.RFE <<- str_trim(tclvalue(down.rfe))
			GeneralParameters$RFE.data <<- str_trim(tclvalue(data.rfe))
			GeneralParameters$Adjust.Bias <<- str_trim(tclvalue(adj.bias))
			GeneralParameters$Blank.Grid <<- switch(str_trim(tclvalue(blankGrd)), 
																"None" =  '1',
																"Use DEM" = '2',
																"Use ESRI shapefile" = '3')

			GeneralParameters$RFE.bbox$minlon <<- str_trim(tclvalue(minLon))
			GeneralParameters$RFE.bbox$maxlon <<- str_trim(tclvalue(maxLon))
			GeneralParameters$RFE.bbox$minlat <<- str_trim(tclvalue(minLat))
			GeneralParameters$RFE.bbox$maxlat <<- str_trim(tclvalue(maxLat))

			GeneralParameters$Interpolation.pars$Interp.Method <<- str_trim(tclvalue(interpMethod))
			GeneralParameters$Interpolation.pars$maxdist <<- str_trim(tclvalue(max.dst))
			GeneralParameters$Interpolation.pars$nmin <<- str_trim(tclvalue(min.nbrs))
			GeneralParameters$Interpolation.pars$nmax <<- str_trim(tclvalue(max.nbrs))

			GeneralParameters$Merging.pars$Rain.no.Rain <<- str_trim(tclvalue(RainNoRain))
			GeneralParameters$Merging.pars$min.Stn <<- str_trim(tclvalue(nmin))
			GeneralParameters$Merging.pars$min.Stn.non0 <<- str_trim(tclvalue(min.non.zero))
			GeneralParameters$Merging.pars$max.RnR.dist <<- str_trim(tclvalue(max.rnr.dst))

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

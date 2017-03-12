qc.get.info.txtn <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	##tkentry width, file path
	if (Sys.info()["sysname"] == "Windows"){
		largeur <- 28
		largeur1 <- 26
		spady <- 0
	}else{
		largeur <- 26
		largeur1 <- 25
		spady <- 1
	}

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	################################
	frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frButt <- tkframe(tt)

	frLeft <- tkframe(frDialog, relief = "groove", borderwidth = 2)
	frRight <- tkframe(frDialog, relief = "groove", borderwidth = 2)

	################################

	frIO <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(GeneralParameters$period, 
									'daily' = cb.periodVAL[1], 
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3])

	file.choix1a <- tclVar(GeneralParameters$IO.files$STN.file1)
	file.choix1b <- tclVar(GeneralParameters$IO.files$STN.file2)
	file.choix2 <- tclVar(GeneralParameters$IO.files$DEM.file)
	file.save1 <- tclVar(GeneralParameters$IO.files$dir2save)
	vartxtn <- tclVar(GeneralParameters$is.TX)

	txtntxt1 <- tclVar("Tmax")
	txtntxt2 <- tclVar("Tmin")

	################

	temp.state <- if(GeneralParameters$consist.check) 'normal' else 'disabled'

	if(!GeneralParameters$stn.type$single.series & !GeneralParameters$dem$use.elv){
		state <- c('disabled', 'normal', 'disabled')
		state1 <- 'disabled'
	}else if(GeneralParameters$stn.type$single.series){
		state <- c('disabled', 'normal', 'disabled')
		state1 <- 'normal'
	}else if(GeneralParameters$dem$use.elv){
		state <- if(GeneralParameters$dem$interp.dem == '0') c('normal', 'normal', 'normal') else c('disabled', 'normal', 'normal')
		state1 <- 'disabled'
	}

	################

	cb.period <- ttkcombobox(frIO, values = cb.periodVAL, textvariable = file.period, width = largeur1)

	txt.vartxtn <- tklabel(frIO, text = 'Data to control', anchor = 'w', justify = 'left')
	rb.vartxtn1 <- tkradiobutton(frIO, text = "Tmax", anchor = 'w', justify = 'left', background = 'lightblue')
	rb.vartxtn2 <- tkradiobutton(frIO, text = "Tmin", anchor = 'w', justify = 'left', background = 'lightblue')
	tkconfigure(rb.vartxtn1, variable = vartxtn, value = "1")
	tkconfigure(rb.vartxtn2, variable = vartxtn, value = "0")

	txt.file.stn1 <- tklabel(frIO, text = tclvalue(txtntxt1), textvariable = txtntxt1)
	cb.file.stn1 <- ttkcombobox(frIO, values = unlist(listOpenFiles), textvariable = file.choix1a)
	bt.file.stn1 <- tkbutton(frIO, text = "...") 

	txt.file.stn2 <- tklabel(frIO, text = tclvalue(txtntxt2), textvariable = txtntxt2)
	cb.file.stn2 <- ttkcombobox(frIO, values = unlist(listOpenFiles), textvariable = file.choix1b, state = temp.state)
	bt.file.stn2 <- tkbutton(frIO, text = "...", state = temp.state) 

	txt.file.elv <- tklabel(frIO, text = 'Elevation Data (NetCDF)', anchor = 'w', justify = 'left')
	cb.file.elv <- ttkcombobox(frIO, values = unlist(listOpenFiles), textvariable = file.choix2, width = largeur1, state = state[1])
	bt.file.elv <- tkbutton(frIO, text = "...") 

	txt.file.save <- tklabel(frIO, text = 'Directory to save result', anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frIO, textvariable = file.save1, width = largeur) 
	bt.file.save <- tkbutton(frIO, text = "...")

	################

	tkconfigure(bt.file.stn1, command = function(){
		dat.opfiles <- getOpenFiles(parent.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]] 
			tclvalue(file.choix1a) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.file.stn1, values = unlist(listOpenFiles), textvariable = file.choix1a)
			tkconfigure(cb.file.stn2, values = unlist(listOpenFiles), textvariable = file.choix1b)
			tkconfigure(cb.file.elv, values = unlist(listOpenFiles), textvariable = file.choix2)
		}else{
			return(NULL)
		}
	})

	tkconfigure(bt.file.stn2, command = function(){
		dat.opfiles <- getOpenFiles(parent.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]] 
			tclvalue(file.choix1b) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.file.stn1, values = unlist(listOpenFiles), textvariable = file.choix1a)
			tkconfigure(cb.file.stn2, values = unlist(listOpenFiles), textvariable = file.choix1b)
			tkconfigure(cb.file.elv, values = unlist(listOpenFiles), textvariable = file.choix2)
		}else{
			return(NULL)
		}
	})

	tkconfigure(bt.file.elv, state = state[1], command = function(){
		nc.opfiles <- getOpenNetcdf(parent.win, all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles
			tclvalue(file.choix2) <- AllOpenFilesData[[nopf+1]][[1]]
		
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.choix2) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.file.elv, values = unlist(listOpenFiles), textvariable = file.choix2)
			tkconfigure(cb.file.stn1, values = unlist(listOpenFiles), textvariable = file.choix1a)
			tkconfigure(cb.file.stn2, values = unlist(listOpenFiles), textvariable = file.choix1b)
		}else{
			return(NULL)
		}
	})

	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(str_trim(GeneralParameters$IO.files$dir2save), "")
		if(!file.exists(file2save1)){
			tkmessageBox(message = paste(file2save1, 'does not exist. It will be created.'), icon = "warning", type = "ok")
			dir.create(file2save1, recursive = TRUE)
			tclvalue(file.save1) <- file2save1
		}else tclvalue(file.save1) <- file2save1
	})

	################
	tkgrid(cb.period, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 5, ipadx = 1, ipady = 1)

	tkgrid(txt.vartxtn, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(rb.vartxtn1, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(rb.vartxtn2, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.file.stn1, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.file.stn1, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 3, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.stn1, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	tkgrid(txt.file.stn2, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.file.stn2, row = 3, column = 1, sticky = 'we', rowspan = 1, columnspan = 3, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.stn2, row = 3, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	tkgrid(txt.file.elv, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.file.elv, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.elv, row = 5, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	tkgrid(txt.file.save, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 7, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)


	infobulle(cb.period, 'Select the time step of the data')
	status.bar.display(cb.period, TextOutputVar, 'Select the time step of the data')
	infobulle(cb.file.stn1, 'Select the file containing the data to control')
	status.bar.display(cb.file.stn1, TextOutputVar, 'Select the file containing the data to control')
	infobulle(bt.file.stn1, 'Browse file if not listed')
	status.bar.display(bt.file.stn1, TextOutputVar, 'Browse file if not listed')
	infobulle(cb.file.stn2, 'Select the file containing the data to control')
	status.bar.display(cb.file.stn2, TextOutputVar, 'Select the file containing the data to control')
	infobulle(bt.file.stn2, 'Browse file if not listed')
	status.bar.display(bt.file.stn2, TextOutputVar, 'Browse file if not listed')

	infobulle(cb.file.elv, 'Select the file containing the elevation data')
	status.bar.display(cb.file.elv, TextOutputVar, 'Select the file containing the elevation data')
	infobulle(bt.file.elv, 'Browse file if not listed')
	status.bar.display(bt.file.elv, TextOutputVar, 'Browse file if not listed')

	infobulle(en.file.save, 'Enter the full path of the directory to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path of the directory to save result')
	infobulle(bt.file.save, 'or browse here')

	################

	tkbind(cb.period, "<<ComboboxSelected>>", function(){
		if(tclvalue(file.period) == 'Daily data'){
			tclvalue(vdtfrmt1) <- "YYYYMMDD"
			tclvalue(vdtfrmt2) <- "YYYY MM DD"
		}
		if(tclvalue(file.period) == 'Dekadal data'){
			tclvalue(vdtfrmt1) <- "YYYYMMD"
			tclvalue(vdtfrmt2) <- "YYYY MM D"
		}
		if(tclvalue(file.period) == 'Monthly data'){
			tclvalue(vdtfrmt1) <- "YYYYMM"
			tclvalue(vdtfrmt2) <- "YYYY MM"
		}
	})

	tkbind(rb.vartxtn1, "<Button-1>", function(){
		tclvalue(txtntxt1) <- 'Tmax'
		tclvalue(txtntxt2) <- 'Tmin'
	})

	tkbind(rb.vartxtn2, "<Button-1>", function(){
		tclvalue(txtntxt1) <- 'Tmin'
		tclvalue(txtntxt2) <- 'Tmax'
	})


	##################
	frSetOpt <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	bt.opt.set <- ttkbutton(frSetOpt, text = "Options - Settings")

	####
	tkconfigure(bt.opt.set, command = function(){
		state.parm <- if(tclvalue(cb.1series.val) == "0") 'normal' else 'disabled'
		GeneralParameters <<- get.param.temperature(tt, GeneralParameters, state.parm)
	})

	####
	tkgrid(bt.opt.set, sticky = 'we', padx = 10, pady = 2, ipadx = 1, ipady = 1)

	infobulle(bt.opt.set, 'Set general options for QC')
	status.bar.display(bt.opt.set, TextOutputVar, 'Set general options for QC')

	############################################

	tkgrid(frIO, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSetOpt, row = 1, column = 0, padx = 1, pady = 5, ipadx = 1, ipady = 1)

	#######################  RIGHT   #####################

	frSeries <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	cb.1series.val <- tclVar(GeneralParameters$stn.type$single.series)
	cb.const.val <- tclVar(GeneralParameters$consist.check)
	cb.1uselv.val <- tclVar(GeneralParameters$dem$use.elv)
	uselv.ch <- tclVar(GeneralParameters$dem$interp.dem)

	state1s <- if(GeneralParameters$AllOrOne == 'one') 'normal' else 'disabled'

	cb.1series <- tkcheckbutton(frSeries, variable = cb.1series.val, text = 'Temperature series from one station', anchor = 'w', justify = 'left', state = state1s)
	cb.const.chk <- tkcheckbutton(frSeries, variable = cb.const.val, text = 'Compare Tmax & Tim', anchor = 'w', justify = 'left')
	cb.1uselv <- tkcheckbutton(frSeries, variable = cb.1uselv.val, text = 'Use Elevation', state = state[2], anchor = 'w', justify = 'left')
	cb.1intelv <- tkradiobutton(frSeries, text = "Elevation from DEM", anchor = 'w', justify = 'left', state = state[3])
	cb.1datelv <- tkradiobutton(frSeries, text = "Elevation from STN data", anchor = 'w', justify = 'left', state = state[3])
	tkconfigure(cb.1intelv, variable = uselv.ch, value = "0")
	tkconfigure(cb.1datelv, variable = uselv.ch, value = "1")

	tkgrid(cb.1series, row = 0, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = spady)
	tkgrid(cb.const.chk, row = 1, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = spady)
	tkgrid(cb.1uselv, row = 2, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = spady)
	tkgrid(cb.1intelv, row = 3, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = spady)
	tkgrid(cb.1datelv, row = 4, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = spady)

	infobulle(cb.1series, 'Check this box if the data is a time series from one station')
	status.bar.display(cb.1series, TextOutputVar, 'Check this box if the data is a time series from one station')
	infobulle(cb.const.chk, 'Check this box to verify if Tmin is greater than Tmax for the same observation period')
	status.bar.display(cb.const.chk, TextOutputVar, 'Check this box to verify if Tmin is greater than Tmax for the same observation period')
	infobulle(cb.1uselv, 'Check this box if you want to use elevation data to choose neighbors stations')
	status.bar.display(cb.1uselv, TextOutputVar, 'Check this box if you want to use elevation data to choose neighbors stations')
	infobulle(cb.1intelv, 'Tick this box if the elevation data will be extracted from DEM')
	status.bar.display(cb.1intelv, TextOutputVar, 'Tick this box if the elevation data will be extracted from DEM')
	infobulle(cb.1datelv, 'Tick this box if the elevation data from CDT station data')
	status.bar.display(cb.1datelv, TextOutputVar, 'Tick this box if the elevation data from CDT station data')

	##################

	tkbind(cb.1series, "<Button-1>", function(){
		if(GeneralParameters$AllOrOne == 'one'){
			tkconfigure(cb.file.elv, state = 'disabled')
			tkconfigure(bt.file.elv, state = 'disabled')
			tkconfigure(cb.1uselv, state = 'normal')
			tkconfigure(cb.1intelv, state = 'disabled')
			tkconfigure(cb.1datelv, state = 'disabled')
			tclvalue(cb.1uselv.val) <- "0"
			if(tclvalue(cb.1series.val) == "0"){
				tkconfigure(ffrmt1, state = 'normal')
				tkconfigure(ffrmt2, state = 'normal')
				tkconfigure(dtfrmt1, state = 'normal')
				tkconfigure(dtfrmt2, state = 'normal')
			}else{
				tkconfigure(ffrmt1, state = 'disabled')
				tkconfigure(ffrmt2, state = 'disabled')
				tkconfigure(dtfrmt1, state = 'disabled')
				tkconfigure(dtfrmt2, state = 'disabled')
			}
		}
	})

	####
	tkbind(cb.1uselv, "<Button-1>", function(){
		tkconfigure(ffrmt1, state = 'disabled')
		tkconfigure(ffrmt2, state = 'disabled')
		tkconfigure(dtfrmt1, state = 'disabled')
		tkconfigure(dtfrmt2, state = 'disabled')
		tclvalue(cb.1series.val) <- "0"
		if(tclvalue(cb.1uselv.val) == "0"){
			state1 <- if(tclvalue(uselv.ch) == "0") 'normal' else 'disabled'
			tkconfigure(cb.file.elv, state = state1)
			tkconfigure(bt.file.elv, state = state1)
			tkconfigure(cb.1uselv, state = 'normal')
			tkconfigure(cb.1intelv, state = 'normal')
			tkconfigure(cb.1datelv, state = 'normal')
		}else{
			tkconfigure(cb.file.elv, state = 'disabled')
			tkconfigure(bt.file.elv, state = 'disabled')
			tkconfigure(cb.1uselv, state = 'normal')
			tkconfigure(cb.1intelv, state = 'disabled')
			tkconfigure(cb.1datelv, state = 'disabled')
		}
	})

	###
	tkbind(cb.1intelv, "<Button-1>", function(){
		if(tclvalue(cb.1uselv.val) == "1"){
			tkconfigure(cb.file.elv, state = 'normal')
			tkconfigure(bt.file.elv, state = 'normal')
		}
	})

	tkbind(cb.1datelv, "<Button-1>", function(){
		if(tclvalue(cb.1uselv.val) == "1"){
			tkconfigure(cb.file.elv, state = 'disabled')
			tkconfigure(bt.file.elv, state = 'disabled')
		}
	})

	###
	tkbind(cb.const.chk, "<Button-1>", function(){
		if(tclvalue(cb.const.val) == "1"){
			tkconfigure(cb.file.stn2, state = 'disabled')
			tkconfigure(bt.file.stn2, state = 'disabled')
		}else{
			tkconfigure(cb.file.stn2, state = 'normal')
			tkconfigure(bt.file.stn2, state = 'normal')
		}
	})

	##################

	frflfrmt <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	rbffrmt <- tclVar(GeneralParameters$stn.type$file.format)

	txtffrmt <- ttklabel(frflfrmt, text = "File Format", anchor = 'w', justify = 'left')
	ffrmt1 <- tkradiobutton(frflfrmt, text = "One variable", anchor = 'w', justify = 'left', state = state1)
	ffrmt2 <- tkradiobutton(frflfrmt, text = "Rain Tmax Tmin", anchor = 'w', justify = 'left', state = state1)
	tkconfigure(ffrmt1, variable = rbffrmt, value = "1")
	tkconfigure(ffrmt2, variable = rbffrmt, value = "0")

	tkgrid(txtffrmt, row = 0, column = 0, sticky = "we", padx = 1, ipadx = 1, pady = spady)
	tkgrid(ffrmt1, row = 1, column = 0, sticky = "we", padx = 1, ipadx = 1, pady = spady)
	tkgrid(ffrmt2, row = 2, column = 0, sticky = "we", padx = 1, ipadx = 1, pady = spady)

	infobulle(ffrmt1, 'In case of single series: The file contains 1 variable')
	status.bar.display(ffrmt1, TextOutputVar, 'In case of single series: The file contains 1 variable')
	infobulle(ffrmt2, 'In case of single series: The file contains Rain, Tmax and Tmin in this order')
	status.bar.display(ffrmt2, TextOutputVar, 'In case of single series: The file contains Rain, Tmax and Tmin in this order')

	##################

	frdtfrmt <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	vdtfrmt1 <- tclVar("YYYYMMDD")
	vdtfrmt2 <- tclVar("YYYY MM DD")
	rbdtfrmt <- tclVar(GeneralParameters$stn.type$date.format)

	txtdtfrmt <- ttklabel(frdtfrmt, text = "Date Format", anchor = 'w', justify = 'left')
	dtfrmt1 <- tkradiobutton(frdtfrmt, text = tclvalue(vdtfrmt1), textvariable = vdtfrmt1, anchor = 'w', justify = 'left', state = state1)
	dtfrmt2 <- tkradiobutton(frdtfrmt, text = tclvalue(vdtfrmt2), textvariable = vdtfrmt2, anchor = 'w', justify = 'left', state = state1)
	tkconfigure(dtfrmt1, variable = rbdtfrmt, value = "1")
	tkconfigure(dtfrmt2, variable = rbdtfrmt, value = "0")

	tkgrid(txtdtfrmt, row = 0, column = 0, sticky = "we", padx = 1, ipadx = 1, pady = spady)
	tkgrid(dtfrmt1, row = 1, column = 0, sticky = "we", padx = 1, ipadx = 1, pady = spady)
	tkgrid(dtfrmt2, row = 2, column = 0, sticky = "we", padx = 1, ipadx = 1, pady = spady)

	infobulle(dtfrmt1, 'In case of single series: dates are merged')
	status.bar.display(dtfrmt1, TextOutputVar, 'In case of single series: dates are merged')
	infobulle(dtfrmt2, 'In case of single series: dates are separated by space or tabulation')
	status.bar.display(dtfrmt2, TextOutputVar, 'In case of single series: dates are separated by space or tabulation')

	############################################
	tkgrid(frSeries, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frflfrmt, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frdtfrmt, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################
	
	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	###################################################

	bt.opt.OK <- tkbutton(frButt, text = "OK") 
	bt.opt.CA <- tkbutton(frButt, text = "Cancel")

	tkconfigure(bt.opt.OK, command = function(){
		if(tclvalue(file.choix1a) == ""){
			tkmessageBox(message = "Choose the file to control", icon = "warning", type = "ok")
		}else if(tclvalue(cb.1series.val) == "1" & tclvalue(rbffrmt) == "1" &
					tclvalue(cb.const.val) == "1" & tclvalue(file.choix1b) == ""){
			msgtxtn <- if(tclvalue(vartxtn) == "1") 'Tmin' else  'Tmax'
			tkmessageBox(message = paste("Provide the file contaning", msgtxtn, 'for the consistency check'),
						icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(cb.1series.val) == "0" & tclvalue(cb.const.val) == "1" & tclvalue(file.choix1b) == ""){
			msgtxtn <- if(tclvalue(vartxtn) == "1") 'Tmin' else  'Tmax'
			tkmessageBox(message = paste("Provide the file contaning", msgtxtn, 'for the consistency check'),
						icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(cb.1uselv.val) == "1" & tclvalue(uselv.ch) == "0" & tclvalue(file.choix2) == ""){
			tkmessageBox(message = "You have to choose DEM data in NetCDF format if you want to extract elevation from DEM",
						icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save1) == "" | tclvalue(file.save1) == "NA"){
			tkmessageBox(message = "Choose a directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			donne <- getStnOpenData(file.choix1a)
			if(is.null(donne)){
				tkmessageBox(message = "Station data not found or in the wrong format", icon = "warning", type = "ok")
				tkwait.window(tt)
			}
			infoDonne <- getStnOpenDataInfo(file.choix1a)[2:3]

			if(tclvalue(cb.const.val) == "1"){
				if(tclvalue(cb.1series.val) == "0" | (tclvalue(cb.1series.val) == "1" & tclvalue(rbffrmt) == "1")){
					donne1 <- getStnOpenData(file.choix1b)
					if(is.null(donne1)){
						tkmessageBox(message = "Station data not found or in the wrong format", icon = "warning", type = "ok")
						tkwait.window(tt)
					}
					infoDonne1 <- getStnOpenDataInfo(file.choix1b)[2:3]
				}
			}

			##
			dirQcTemp <- file.path(tclvalue(file.save1), paste('QcTemp', getf.no.ext(tclvalue(file.choix1a)), sep = '_'))
			dirparams <- file.path(dirQcTemp, 'OriginalData')
			if(!file.exists(dirparams)) dir.create(dirparams, showWarnings = FALSE, recursive = TRUE)
			fileparams <- file.path(dirparams, 'Parameters.RData')

			GeneralParameters$period <<- switch(tclvalue(file.period), 
			 									'Daily data' = 'daily',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')
			GeneralParameters$IO.files$STN.file1 <<- str_trim(tclvalue(file.choix1a))
			GeneralParameters$IO.files$STN.file2 <<- str_trim(tclvalue(file.choix1b))
			GeneralParameters$IO.files$DEM.file <<- str_trim(tclvalue(file.choix2))
			GeneralParameters$IO.files$dir2save <<- str_trim(tclvalue(file.save1))

			GeneralParameters$is.TX <<- switch(tclvalue(vartxtn), '0' = FALSE, '1' = TRUE)
			GeneralParameters$consist.check <<- switch(tclvalue(cb.const.val), '0' = FALSE, '1' = TRUE)

			GeneralParameters$dem$use.elv <<- switch(tclvalue(cb.1uselv.val), '0' = FALSE, '1' = TRUE)
			GeneralParameters$dem$interp.dem <<- tclvalue(uselv.ch)

			GeneralParameters$stn.type$single.series <<- switch(tclvalue(cb.1series.val), '0' = FALSE, '1' = TRUE)
			GeneralParameters$stn.type$file.format <<- tclvalue(rbffrmt)
			GeneralParameters$stn.type$date.format <<- tclvalue(rbdtfrmt)

			######
			getInitDataParams <- function(GeneralParameters){
				if(tclvalue(cb.1series.val) == "0"){
					donstn <-  getCDTdataAndDisplayMsg(donne, GeneralParameters$period)
					donOut <- list(donstn)
					parsFile <- list(infoDonne)
					if(tclvalue(cb.const.val) == "1"){
						donstn1 <-  getCDTdataAndDisplayMsg(donne1, GeneralParameters$period)
						donOut <- list(donstn, donstn1)
						parsFile <- list(infoDonne, infoDonne1)
					}
					xycrds <- NULL
					if(!is.null(donstn)){
						limControl <- data.frame(donstn$id, GeneralParameters$limits$Lower.Bounds,
												GeneralParameters$limits$Upper.Bounds, donstn$lon, donstn$lat)
						names(limControl) <- c('Station.ID', 'Lower.Bounds', 'Upper.Bounds', 'Lon', 'Lat')
						GeneralParameters$stnInfo <- limControl
						# GeneralParameters$stnInfo <- getTempInitParams(donstn)
						lchoixStnFr$env$stn.choix <- as.character(donstn$id)
						xycrds <- paste(c(as.character(donstn$lon), as.character(donstn$lat)), sep = '', collapse = ' ')
					}else tkwait.window(tt)
				}else{
					donstn <- getCDTTSdataAndDisplayMsg(donne, GeneralParameters$period, tclvalue(rbffrmt), tclvalue(rbdtfrmt))
					donOut <- list(donstn)
					parsFile <- list(infoDonne)
					if(tclvalue(cb.const.val) == "1" & tclvalue(rbffrmt) == "1"){
						donstn1 <- getCDTTSdataAndDisplayMsg(donne1, GeneralParameters$period, tclvalue(rbffrmt), tclvalue(rbdtfrmt))
						donOut <- list(donstn, donstn1)
						parsFile <- list(infoDonne, infoDonne1)
					} 
					if(!is.null(donstn)){
						lchoixStnFr$env$stn.choix <- getf.no.ext(tclvalue(file.choix1a))
						limControl <- data.frame(lchoixStnFr$env$stn.choix, GeneralParameters$limits$Lower.Bounds,
												GeneralParameters$limits$Upper.Bounds)
						names(limControl) <- c('Station.ID', 'Lower.Bounds', 'Upper.Bounds')
						GeneralParameters$stnInfo <- limControl
					}else tkwait.window(tt)
					xycrds <- NULL
				}
				paramsGAL <- list(inputPars = GeneralParameters, dataPars = parsFile, data = donOut)
				save(paramsGAL, file = fileparams)
				return(list(paramsGAL, lchoixStnFr$env$stn.choix, xycrds))
			}

			#####
			getConsistData <- function(){
				if(tclvalue(cb.1series.val) == "0") donstn1 <- getCDTdataAndDisplayMsg(donne, GeneralParameters$period)
				else{
					donstn1 <- getCDTTSdataAndDisplayMsg(donne1, GeneralParameters$period, tclvalue(rbffrmt), tclvalue(rbdtfrmt))
				} 
				if(is.null(donstn1)){
					tkmessageBox(message = "Data to be used for the consistency check not found or in the wrong format", icon = "warning", type = "ok")
					tkwait.window(tt)	
				}
				load(fileparams)
				paramsGAL$data[[2]] <- donstn1
				paramsGAL$dataPars[[2]] <- infoDonne1
				save(paramsGAL, file = fileparams)
				return(paramsGAL)
			}

			######
			xtest1 <- tclvalue(cb.1series.val) == "0" & tclvalue(cb.const.val) == "1"
			xtest2 <- tclvalue(cb.1series.val) == "1" & tclvalue(cb.const.val) == "1" & tclvalue(rbffrmt) == "1"

			if(file.exists(fileparams)){
				load(fileparams)
				intest1 <- paramsGAL$inputPars$period == GeneralParameters$period & all(infoDonne%in%paramsGAL$dataPars[[1]])
				if(intest1){
					donstn <- paramsGAL$data[[1]]
					if(xtest1 | xtest2){
						if(length(paramsGAL$dataPars) == 1){
							paramsGAL <- getConsistData()
						}else{
							ctest1 <- all(infoDonne1%in%paramsGAL$dataPars[[2]])
							if(!ctest1) paramsGAL <- getConsistData()
						} 
						donstn1 <- paramsGAL$data[[2]]
					}
					GeneralParameters$stnInfo <<- paramsGAL$inputPars$stnInfo
					lchoixStnFr$env$stn.choix <<- as.character(GeneralParameters$stnInfo$Station.ID)
					if(tclvalue(cb.1series.val) == "0"){ 
						tclvalue(XYCoordinates) <<- paste(c(GeneralParameters$stnInfo$Lon, GeneralParameters$stnInfo$Lat), sep = '', collapse = ' ')
					}
					rm(paramsGAL)
				}else{
					retDonPar <- getInitDataParams(GeneralParameters)
					donstn <- retDonPar[[1]]$data[[1]]
					if(xtest1 | xtest2) donstn1 <- retDonPar[[1]]$data[[2]]
					GeneralParameters <<- retDonPar[[1]]$inputPars
					lchoixStnFr$env$stn.choix <<- retDonPar[[2]]
					if(tclvalue(cb.1series.val) == "0") tclvalue(XYCoordinates) <<- retDonPar[[3]]
					rm(retDonPar)
				}
			}else{
				retDonPar <- getInitDataParams(GeneralParameters)
				donstn <- retDonPar[[1]]$data[[1]]
				if(xtest1 | xtest2) donstn1 <- retDonPar[[1]]$data[[2]]
				GeneralParameters <<- retDonPar[[1]]$inputPars
				lchoixStnFr$env$stn.choix <<- retDonPar[[2]]
				if(tclvalue(cb.1series.val) == "0") tclvalue(XYCoordinates) <<- retDonPar[[3]]
				rm(retDonPar)
			} 
			assign('donnees1', donstn, envir = EnvQcOutlierData)
			if(xtest1 | xtest2 ) assign('donnees2', donstn1, envir = EnvQcOutlierData)
			assign('baseDir', dirQcTemp, envir = EnvQcOutlierData)

			##################
			##set choix stn

			if(GeneralParameters$retpar == 0){
				ik <- if(lchoixStnFr$env$stn.choix[1] != '') 1 else 2
				tclvalue(lchoixStnFr$env$stn.choix.val) <- lchoixStnFr$env$stn.choix[ik]
			}else{
				istn <- as.numeric(tclvalue(tcl(lchoixStnFr$env$stn.choix.cb, "current")))+1
				istn <- if(istn > 0) istn else 1
				tclvalue(lchoixStnFr$env$stn.choix.val) <- lchoixStnFr$env$stn.choix[istn]
			}
			
			tkconfigure(lchoixStnFr$env$stn.choix.cb, values = lchoixStnFr$env$stn.choix, textvariable = lchoixStnFr$env$stn.choix.val)
			if(GeneralParameters$AllOrOne == 'one'){
				tkconfigure(lchoixStnFr$env$setting.button, state = 'normal')
				stateReplaceAll <- 'disabled'
			} 
			if(GeneralParameters$AllOrOne == 'all'){
				tkconfigure(lchoixStnFr$env$setting.button, state = 'disabled')
				stateReplaceAll <- 'normal'
			} 
			if(tclvalue(cb.1series.val) == '0'){
				tkconfigure(lchoixStnFr$env$stn.choix.prev, state = 'normal')
				tkconfigure(lchoixStnFr$env$stn.choix.next, state = 'normal')
			}
			tkconfigure(spinH, state = 'normal')
			tkconfigure(spinV, state = 'normal')
			
			####button command
			if(is.null(lcmd.frame_qc)){
				lcmd.frame <<- QcCmdBut(stateReplaceAll)
				lcmd.frame_qc <<- 1
			}
			GeneralParameters$retpar <<- GeneralParameters$retpar+1
			
			######
			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
		}
	})

	tkconfigure(bt.opt.CA, command = function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkgrid(bt.opt.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10, ipady = 1)
	tkgrid(bt.opt.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	###############################################################

	tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frButt, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Quality Control - Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

###################################################################################################
##Edit parameter temperature

get.param.temperature <- function(tt, GeneralParameters, state.parm){
	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frDialog <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frButt <- tkframe(tt1)

	##################

	frOpt <- tkframe(frDialog, relief = "sunken", borderwidth = 2)

	min.stn <- tclVar(GeneralParameters$select.pars$min.stn)
	max.stn <- tclVar(GeneralParameters$select.pars$max.stn)
	win.len <- tclVar(GeneralParameters$select.pars$win.len)
	CInt <- tclVar(GeneralParameters$select.pars$conf.lev)
	max.dist <- tclVar(GeneralParameters$select.pars$max.dist)
	elv.diff <- tclVar(GeneralParameters$select.pars$elv.diff)
	limInf <- tclVar(GeneralParameters$limits$Lower.Bounds)
	limSup <- tclVar(GeneralParameters$limits$Upper.Bounds)

	file.period <- get("file.period", parent.frame())
	xper <- switch(tclvalue(file.period),
					'Daily data' = 'days',
					'Dekadal data' = 'dekad',
					'Monthly data' = 'months')
	xperHelp <- paste('Time window: number of', xper, 'to be used for regression')

	min.stn.l <- tklabel(frOpt, text = 'Min.stn', anchor = 'e', justify = 'right')
	max.stn.l <- tklabel(frOpt, text = 'Max.stn', anchor = 'e', justify = 'right')
	win.len.l <- tklabel(frOpt, text = 'Win.len', anchor = 'e', justify = 'right')
	max.dist.l <- tklabel(frOpt, text = 'Max.dist(km)', anchor = 'e', justify = 'right')
	elv.diff.l <- tklabel(frOpt, text = 'Elv.diff(m)', anchor = 'e', justify = 'right')
	CInt.l <- tklabel(frOpt, text = 'Conf.lev(%)', anchor = 'e', justify = 'right')
	lim.inf.l <- tklabel(frOpt, text = 'Temp.min', anchor = 'e', justify = 'right')
	lim.sup.l <- tklabel(frOpt, text = 'Temp.max', anchor = 'e', justify = 'right')

	min.stn.v <- tkentry(frOpt, width = 6, textvariable = min.stn, state = state.parm)
	max.stn.v <- tkentry(frOpt, width = 6, textvariable = max.stn, state = state.parm)
	win.len.v <- tkentry(frOpt, width = 6, textvariable = win.len, state = state.parm)
	max.dist.v <- tkentry(frOpt, width = 6, textvariable = max.dist, state = state.parm)
	elv.diff.v <- tkentry(frOpt, width = 6, textvariable = elv.diff, state = state.parm)
	CInt.v <- tkentry(frOpt, width = 6, textvariable = CInt)
	lim.inf.v <- tkentry(frOpt, width = 6, textvariable = limInf)
	lim.sup.v <- tkentry(frOpt, width = 6, textvariable = limSup)

	tkgrid(min.stn.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.stn.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.stn.l, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.stn.v, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dist.l, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dist.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(elv.diff.l, row = 1, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(elv.diff.v, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(win.len.l, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(win.len.v, row = 2, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(CInt.l, row = 2, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(CInt.v, row = 2, column = 3, sticky = 'ew', padx = 1, pady = 1)

	tkgrid(lim.inf.l, row = 3, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(lim.inf.v, row = 3, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(lim.sup.l, row = 3, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(lim.sup.v, row = 3, column = 3, sticky = 'ew', padx = 1, pady = 1)

	infobulle(min.stn.v, 'Minimum number of neighbor stations to use')
	status.bar.display(min.stn.v, TextOutputVar, 'Minimum number of neighbor stations to use')
	infobulle(max.stn.v, 'Maximum number of neighbor stations to use')
	status.bar.display(max.stn.v, TextOutputVar, 'Maximum number of neighbor stations to use')
	infobulle(win.len.v, xperHelp)
	status.bar.display(win.len.v, TextOutputVar, xperHelp)
	infobulle(CInt.v, 'Confidence level  (%)')
	status.bar.display(CInt.v, TextOutputVar, 'Confidence level  (%)')
	infobulle(max.dist.v, 'Maximum distance of neighbor stations to use (km)')
	status.bar.display(max.dist.v, TextOutputVar, 'Maximum distance of neighbor stations to use (km)')
	infobulle(elv.diff.v, 'Maximum altitude difference of neighbor stations to use (m)')
	status.bar.display(elv.diff.v, TextOutputVar, 'Maximum altitude difference of neighbor stations to use (m)')

	infobulle(lim.inf.v, 'Minimum value of temperature to be considered (degC)')
	status.bar.display(lim.inf.v, TextOutputVar, 'Minimum value of temperature to be considered (degC),\nabove that value, corresponding temperature values are set to missing')
	infobulle(lim.sup.v, 'Maximum value of temperature to be considered (degC)')
	status.bar.display(lim.sup.v, TextOutputVar, 'Maximum value of temperature to be considered (degC),\nabove that value, corresponding temperature values are set to missing')

	##############################

	tkgrid(frOpt, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	####################################################

	bt.prm.OK <- tkbutton(frButt, text = "OK")
	bt.prm.CA <- tkbutton(frButt, text = "Cancel")

	######################
	ret.param.temp1 <<- NULL

	tkconfigure(bt.prm.OK, command = function(){
		GeneralParameters$select.pars$min.stn <<- as.numeric(str_trim(tclvalue(min.stn)))
		GeneralParameters$select.pars$max.stn <<- as.numeric(str_trim(tclvalue(max.stn)))
		GeneralParameters$select.pars$win.len <<- as.numeric(str_trim(tclvalue(win.len)))
		GeneralParameters$select.pars$max.dist <<- as.numeric(str_trim(tclvalue(max.dist)))
		GeneralParameters$select.pars$elv.diff <<- as.numeric(str_trim(tclvalue(elv.diff)))
		GeneralParameters$select.pars$conf.lev <<- as.numeric(str_trim(tclvalue(CInt)))

		GeneralParameters$limits$Lower.Bounds <<- as.numeric(str_trim(tclvalue(limInf)))
		GeneralParameters$limits$Upper.Bounds <<- as.numeric(str_trim(tclvalue(limSup)))

		ret.param.temp1 <<- 0
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

	tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frButt, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'Options- Settings')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(tt)})
	tkwait.window(tt1)
	return(GeneralParameters)
}



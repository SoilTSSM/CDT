qc.get.info.rain <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur <- 34
		largeur1 <- 32
		spady <- 0
	}else{
		largeur <- 25
		largeur1 <- 24
		spady <- 1
	}

	################################
	
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frButt <- tkframe(tt)

	frLeft <- tkframe(frDialog, relief = "groove", borderwidth = 2)
	frRight <- tkframe(frDialog, relief = "groove", borderwidth = 2)

	################################

	frIO <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(as.character(GeneralParameters$period), 
									'daily' = cb.periodVAL[1], 
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3])
	file.choix1 <- tclVar(as.character(GeneralParameters$file.io$Values[1]))
	file.choix2 <- tclVar(as.character(GeneralParameters$file.io$Values[2]))
	file.save1 <- tclVar(as.character(GeneralParameters$file.io$Values[3]))

	if(as.character(GeneralParameters$use.method$Values[1]) == '0' & as.character(GeneralParameters$use.method$Values[2]) == '0'){
		state <- c('disabled', 'normal', 'disabled')
		state1 <- 'disabled'
	}else if(as.character(GeneralParameters$use.method$Values[1]) == '1'){
		state <- c('disabled', 'normal', 'disabled')
		state1 <- 'normal'
	}else if(as.character(GeneralParameters$use.method$Values[2]) == '1'){
		state <- if(as.character(GeneralParameters$use.method$Values[3]) == '0') c('normal', 'normal', 'normal') else c('disabled', 'normal', 'normal')
		state1 <- 'disabled'
	}

	############

	cb.period <- ttkcombobox(frIO, values = cb.periodVAL, textvariable = file.period)

	txt.file.stn <- tklabel(frIO, text = 'Input data to control', anchor = 'w', justify = 'left')
	cb.file.stn <- ttkcombobox(frIO, values = unlist(listOpenFiles), textvariable = file.choix1, width = largeur1)
	bt.file.stn <- tkbutton(frIO, text = "...") 

	txt.file.elv <- tklabel(frIO, text = 'Elevation Data (NetCDF)', anchor = 'w', justify = 'left')
	cb.file.elv <- ttkcombobox(frIO, values = unlist(listOpenFiles), textvariable = file.choix2, width = largeur1, state = state[1])
	bt.file.elv <- tkbutton(frIO, text = "...", state = state[1]) 

	txt.file.save <- tklabel(frIO, text = 'Directory to save result', anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frIO, textvariable = file.save1, width = largeur) 
	bt.file.save <- tkbutton(frIO, text = "...")

	####
	tkconfigure(bt.file.stn, command = function(){
		dat.opfiles <- getOpenFiles(parent.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]] 
			tclvalue(file.choix1) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.file.stn, values = unlist(listOpenFiles), textvariable = file.choix1)
			tkconfigure(cb.file.elv, values = unlist(listOpenFiles), textvariable = file.choix2)
		}else{
			return(NULL)
		}
	})

	tkconfigure(bt.file.elv, command = function(){
		nc.opfiles <- getOpenNetcdf(parent.win, all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles
			tclvalue(file.choix2) <- AllOpenFilesData[[nopf+1]][[1]]
		
			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.choix2) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.file.elv, values = unlist(listOpenFiles), textvariable = file.choix2)
			tkconfigure(cb.file.stn, values = unlist(listOpenFiles), textvariable = file.choix1)
		}else{
			return(NULL)
		}
	})

	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[3]), "")
		if(!file.exists(file2save1)){
			tkmessageBox(message = paste(file2save1, 'does not exist.\n It will be created.', sep = ' '), icon = "warning", type = "ok")
			dir.create(file2save1, recursive = TRUE)
			tclvalue(file.save1) <- file2save1
		}else tclvalue(file.save1) <- file2save1
	})

	####

	tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(txt.file.stn, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.file.stn, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.file.stn, row = 2, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.file.elv, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.file.elv, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.file.elv, row = 4, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.file.save, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 6, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.period, 'Choose the time step of the data')
	status.bar.display(cb.period, TextOutputVar, 'Choose the time step of the data')
	infobulle(cb.file.stn, 'Choose the file in the list')
	status.bar.display(cb.file.stn, TextOutputVar, 'Choose the file containing the data to control')
	infobulle(bt.file.stn, 'Browse file if not listed')
	status.bar.display(bt.file.stn, TextOutputVar, 'Browse file if not listed')
	infobulle(cb.file.elv, 'Choose the file in the list')
	status.bar.display(cb.file.elv, TextOutputVar, 'Choose the file containing the elevation data')
	infobulle(bt.file.elv, 'Browse file if not listed')
	status.bar.display(bt.file.elv, TextOutputVar, 'Browse file if not listed')
	infobulle(en.file.save, 'Enter the full path to directory to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save result')
	infobulle(bt.file.save, 'or browse here')

	##################
	frSetOpt <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	bt.opt.set <- ttkbutton(frSetOpt, text = "Options - Settings")

	####
	tkconfigure(bt.opt.set, command = function(){
		state.parm <- if(tclvalue(cb.1series.val) == "0") 'normal' else 'disabled'
		GeneralParameters <<- get.param.rainfall(tt, GeneralParameters, state.parm)
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

	cb.1series.val <- tclVar(as.character(GeneralParameters$use.method$Values[1]))
	cb.1uselv.val <- tclVar(as.character(GeneralParameters$use.method$Values[2]))
	uselv.ch <- tclVar(as.character(GeneralParameters$use.method$Values[3]))

	state1s <- if(GeneralParameters$AllOrOne == 'one') 'normal' else 'disabled'

	cb.1series <- tkcheckbutton(frSeries, variable = cb.1series.val, text = 'Rainfall series from one station', anchor = 'w', justify = 'left', state = state1s)
	cb.1uselv <- tkcheckbutton(frSeries, variable = cb.1uselv.val, text = 'Use Elevation', anchor = 'w', justify = 'left', state = state[2])
	cb.1intelv <- tkradiobutton(frSeries, text = "Elevation from DEM", anchor = 'w', justify = 'left', state = state[3])
	cb.1datelv <- tkradiobutton(frSeries, text = "Elevation from STN data", anchor = 'w', justify = 'left', state = state[3])
	tkconfigure(cb.1intelv, variable = uselv.ch, value = "0")
	tkconfigure(cb.1datelv, variable = uselv.ch, value = "1")

	tkgrid(cb.1series, row = 0, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = spady)
	tkgrid(cb.1uselv, row = 1, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = spady)
	tkgrid(cb.1intelv, row = 2, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = spady)
	tkgrid(cb.1datelv, row = 3, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = spady)

	infobulle(cb.1series, 'Check this box if the data is a time series from one station')
	status.bar.display(cb.1series, TextOutputVar, 'Check this box if the data is a time series from one station')
	infobulle(cb.1uselv, 'Check this box if you want to use elevation data to choose neighbors stations')
	status.bar.display(cb.1uselv, TextOutputVar, 'Check this box if you want to use elevation data to choose neighbors stations')
	infobulle(cb.1intelv, 'Tick this box if the elevation data will be extracted from DEM')
	status.bar.display(cb.1intelv, TextOutputVar, 'Tick this box if the elevation data will be extracted from DEM')
	infobulle(cb.1datelv, 'Tick this box if the elevation data from CDT station data')
	status.bar.display(cb.1datelv, TextOutputVar, 'Tick this box if the elevation data from CDT station data')

	##################
	frflfrmt <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	rbffrmt <- tclVar(as.character(GeneralParameters$file.date.format$Values[1]))

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
	rbdtfrmt <- tclVar(as.character(GeneralParameters$file.date.format$Values[2]))

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
	status.bar.display(dtfrmt2, TextOutputVar, 'In case of single series:dates are separated by space or tabulation')

	############################################
	tkgrid(frSeries, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frflfrmt, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frdtfrmt, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################
	
	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	###################################################

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

	####
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

	####
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

	###################################################

	bt.opt.OK <- tkbutton(frButt, text = "OK") 
	bt.opt.CA <- tkbutton(frButt, text = "Cancel") 	
	
	tkconfigure(bt.opt.OK, command = function(){
		if(tclvalue(file.choix1) == ""){
			tkmessageBox(message = "Choose the file to control", icon = "warning", type = "ok")
		}else if(tclvalue(cb.1uselv.val) == "1" & tclvalue(uselv.ch) == "0" & tclvalue(file.choix2) == ""){
			tkmessageBox(message = "You have to choose DEM data in NetCDF format if you want to extract elevation from DEM",
			icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save1) == "" | tclvalue(file.save1) == "NA"){
			tkmessageBox(message = "Choose a directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			donne <- getStnOpenData(file.choix1)
			if(is.null(donne)){
				tkmessageBox(message = "Station data not found or in the wrong format", icon = "warning", type = "ok")
				tkwait.window(tt)
			}
			infoDonne <- getStnOpenDataInfo(file.choix1)[2:3]

			##
			dirQcRain <- file.path(tclvalue(file.save1), paste('QcRainfall', getf.no.ext(tclvalue(file.choix1)), sep = '_'))
			dirparams <- file.path(dirQcRain, 'OriginalData')
			if(!file.exists(dirparams)) dir.create(dirparams, showWarnings = FALSE, recursive = TRUE)
			fileparams <- file.path(dirparams, 'Parameters.RData')
			
			##
			GeneralParameters$file.io$Values <<- c(tclvalue(file.choix1), tclvalue(file.choix2), tclvalue(file.save1))
			GeneralParameters$file.date.format$Values <<- c(tclvalue(rbffrmt), tclvalue(rbdtfrmt))
			GeneralParameters$use.method$Values <<- c(tclvalue(cb.1series.val), tclvalue(cb.1uselv.val), tclvalue(uselv.ch))
			GeneralParameters$period <<- switch(tclvalue(file.period), 
			 									'Daily data' = 'daily',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')

			######
			getInitDataParams <- function(GeneralParameters){
				if(tclvalue(cb.1series.val) == "0"){
					donstn <- getCDTdataAndDisplayMsg(donne, GeneralParameters$period)

					xycrds <- NULL
					if(!is.null(donstn)){
						if(GeneralParameters$period == "daily") limUp <- 300
						if(GeneralParameters$period == "dekadal") limUp <- 1000
						if(GeneralParameters$period == "monthly") limUp <- 3000
						limControl <- data.frame(donstn$id, limUp, donstn$lon, donstn$lat)
						names(limControl) <- c('Station ID', 'Upper Bounds', 'Lon', 'Lat')
						GeneralParameters$parameter[[2]] <- limControl
						#GeneralParameters$parameter[[2]] <- getRainInitParams0(donstn, GeneralParameters$period)
						lchoixStnFr$env$stn.choix <- as.character(donstn$id)
						xycrds <- paste(c(as.character(donstn$lon), as.character(donstn$lat)), sep = '', collapse = ' ')
					}else tkwait.window(tt)
				}else{
					donstn <- getCDTTSdataAndDisplayMsg(donne, GeneralParameters$period, tclvalue(rbffrmt), tclvalue(rbdtfrmt))
					if(!is.null(donstn)){
						if(GeneralParameters$period == "daily") valup <- 300
						if(GeneralParameters$period == "dekadal") valup <- 1000
						if(GeneralParameters$period == "monthly") valup <- 3000
						lchoixStnFr$env$stn.choix <- getf.no.ext(tclvalue(file.choix1))
						limControl <- data.frame(lchoixStnFr$env$stn.choix, valup)
						names(limControl) <- c('Station ID', 'Upper Bounds')
						GeneralParameters$parameter[[2]] <- limControl
					}else tkwait.window(tt)
					xycrds <- NULL
				}
				paramsGAL <- list(inputPars = GeneralParameters, dataPars = infoDonne, data = donstn)
				save(paramsGAL, file = fileparams)
				return(list(paramsGAL, lchoixStnFr$env$stn.choix, xycrds))
			}

			######
			if(file.exists(fileparams)){
				load(fileparams)
				if(paramsGAL$inputPars$period == GeneralParameters$period & all(infoDonne %in% paramsGAL$dataPars)){
					donstn <- paramsGAL$data
					GeneralParameters$parameter[[2]] <<- paramsGAL$inputPars$parameter[[2]]
					lchoixStnFr$env$stn.choix <<- as.character(GeneralParameters$parameter[[2]][, 1])
					if(tclvalue(cb.1series.val) == "0"){
						tclvalue(XYCoordinates) <<- paste(c(as.character(GeneralParameters$parameter[[2]][, 3]),
													as.character(GeneralParameters$parameter[[2]][, 4])), sep = '', collapse = ' ')
					}
					rm(paramsGAL)
				}else{
					retDonPar <- getInitDataParams(GeneralParameters)
					donstn <- retDonPar[[1]]$data
					GeneralParameters <<- retDonPar[[1]]$inputPars
					lchoixStnFr$env$stn.choix <<- retDonPar[[2]]
					if(tclvalue(cb.1series.val) == "0") tclvalue(XYCoordinates) <<- retDonPar[[3]]
					rm(retDonPar)
				}
			}else{
				retDonPar <- getInitDataParams(GeneralParameters)
				donstn <- retDonPar[[1]]$data
				GeneralParameters <<- retDonPar[[1]]$inputPars
				lchoixStnFr$env$stn.choix <<- retDonPar[[2]]
				if(tclvalue(cb.1series.val) == "0") tclvalue(XYCoordinates) <<- retDonPar[[3]]
				rm(retDonPar)
			} 
			assign('donnees', donstn, envir = EnvQcOutlierData)
			assign('baseDir', dirQcRain, envir = EnvQcOutlierData)

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

################################################################################################################################
##Edit parameter rainfall

get.param.rainfall <- function(tt, GeneralParameters, state.parm){
	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frDialog <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frButt <- tkframe(tt1)

	##################
	frOpt <- tkframe(frDialog, relief = "sunken", borderwidth = 2)

	min.stn <- tclVar(as.character(GeneralParameters$parameter[[1]]$Values[1]))
	CInt <- tclVar(as.character(GeneralParameters$parameter[[1]]$Values[2]))
	max.dist <- tclVar(as.character(GeneralParameters$parameter[[1]]$Values[3]))
	elv.diff <- tclVar(as.character(GeneralParameters$parameter[[1]]$Values[4]))
	limSup <- tclVar(300)

	min.stn.l <- tklabel(frOpt, text = 'Min.stn', anchor = 'e', justify = 'right')
	max.dist.l <- tklabel(frOpt, text = 'Max.dist(km)', anchor = 'e', justify = 'right')
	elv.diff.l <- tklabel(frOpt, text = 'Elv.diff(m)', anchor = 'e', justify = 'right')
	CInt.l <- tklabel(frOpt, text = 'Conf.lev(%)', anchor = 'e', justify = 'right')
	max.precip.l <- tklabel(frOpt, text = 'Precip.max', anchor = 'e', justify = 'right')

	min.stn.v <- tkentry(frOpt, width = 5, textvariable = min.stn, state = state.parm)
	max.dist.v <- tkentry(frOpt, width = 5, textvariable = max.dist, state = state.parm)
	elv.diff.v <- tkentry(frOpt, width = 8, textvariable = elv.diff, state = state.parm)
	CInt.v <- tkentry(frOpt, width = 8, textvariable = CInt)
	max.precip.v <- tkentry(frOpt, width = 8, textvariable =limSup)

	tkgrid(min.stn.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.stn.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(CInt.l, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(CInt.v, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dist.l, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dist.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)

	tkgrid(max.precip.l, row = 1, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.precip.v, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)

	tkgrid(elv.diff.l, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(elv.diff.v, row = 2, column = 1, sticky = 'ew', padx = 1, pady = 1)

	infobulle(min.stn.v, 'Minimum number of neighbor stations to use')
	status.bar.display(min.stn.v, TextOutputVar, 'Minimum number of neighbor stations to use')
	infobulle(max.dist.v, 'Maximum distance of neighbor stations to use (km)')
	status.bar.display(max.dist.v, TextOutputVar, 'Maximum distance of neighbor stations to use (km)')
	infobulle(elv.diff.v, 'Maximum altitude difference of neighbor stations to use (m)')
	status.bar.display(elv.diff.v, TextOutputVar, 'Maximum altitude difference of neighbor stations to use (m)')
	infobulle(CInt.v, 'Confidence level (%)')
	status.bar.display(CInt.v, TextOutputVar, 'Confidence level (%)')

	infobulle(max.precip.v, 'Maximum value of precipitation to be considered (mm)')
	status.bar.display(max.precip.v, TextOutputVar, 'Maximum value of precipitation to be considered (mm),\nabove that value, corresponding precipitation values are set to missing')

	##################
	frRain <- tkframe(frDialog, relief = "sunken", borderwidth = 2)

	ispmax <- tclVar(as.character(GeneralParameters$parameter[[3]]$ispmax))
	ispobs <- tclVar(as.character(GeneralParameters$parameter[[3]]$ispobs))
	isdmin <- tclVar(as.character(GeneralParameters$parameter[[3]]$isdmin))
	isdobs <- tclVar(as.character(GeneralParameters$parameter[[3]]$isdobs))
	isdq1 <- tclVar(as.character(GeneralParameters$parameter[[3]]$isdq1))
	ftldev <- tclVar(as.character(GeneralParameters$parameter[[3]]$ftldev))

	ispmax.l <- tklabel(frRain, text = 'ispmax', anchor = 'e', justify = 'right')
	ispobs.l <- tklabel(frRain, text = 'ispobs', anchor = 'e', justify = 'right')
	isdmin.l <- tklabel(frRain, text = 'isdmin', anchor = 'e', justify = 'right')
	isdobs.l <- tklabel(frRain, text = 'isdobs', anchor = 'e', justify = 'right')
	isdq1.l <- tklabel(frRain, text = 'isdq1', anchor = 'e', justify = 'right')
	ftldev.l <- tklabel(frRain, text = 'ftldev', anchor = 'e', justify = 'right')

	ispmax.v <- tkentry(frRain, width = 8, textvariable = ispmax, state = state.parm)
	ispobs.v <- tkentry(frRain, width = 8, textvariable = ispobs, state = state.parm)
	isdmin.v <- tkentry(frRain, width = 8, textvariable = isdmin, state = state.parm)
	isdobs.v <- tkentry(frRain, width = 8, textvariable = isdobs, state = state.parm)
	isdq1.v <- tkentry(frRain, width = 8, textvariable = isdq1, state = state.parm)
	ftldev.v <- tkentry(frRain, width = 8, textvariable = ftldev, state = state.parm)

	sep.col <- tklabel(frRain, text = '', width = 8)

	tkgrid(ispmax.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(ispmax.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)

	tkgrid(sep.col, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)

	tkgrid(ispobs.l, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(ispobs.v, row = 0, column = 4, sticky = 'ew', padx = 1, pady = 1)

	tkgrid(isdmin.l, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(isdmin.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(isdobs.l, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(isdobs.v, row = 1, column = 4, sticky = 'ew', padx = 1, pady = 1)

	tkgrid(isdq1.l, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(isdq1.v, row = 2, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(ftldev.l, row = 2, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(ftldev.v, row = 2, column = 4, sticky = 'ew', padx = 1, pady = 1)

	infobulle(ispmax.v, 'Maximum value of the neighbors stations is less than ispmax')
	status.bar.display(ispmax.v, TextOutputVar, 'Maximum value of the neighbors stations is less than ispmax')
	infobulle(ispobs.v, 'Value of target station is greater ispobs')
	status.bar.display(ispobs.v, TextOutputVar, 'Value of target station is greater ispobs')
	infobulle(isdmin.v, 'Minimum value of the  neighbors stations is greater isdmin')
	status.bar.display(isdmin.v, TextOutputVar, 'Minimum value of the  neighbors stations is greater isdmin')
	infobulle(isdobs.v, 'Value of target station is less than isdobs')
	status.bar.display(isdobs.v, TextOutputVar, 'Value of target station is less than isdobs')
	infobulle(isdq1.v, 'The first quartile value of the neighbors stations is greater than isdq1')
	status.bar.display(isdq1.v, TextOutputVar, 'The first quartile value of the neighbors stations is greater than isdq1')
	infobulle(ftldev.v, 'Outliers factor value between 2 and 4, multiplier of IQR')
	status.bar.display(ftldev.v, TextOutputVar, 'Outliers factor value between 2 and 4, multiplier of IQR')

	##############################

	tkgrid(frOpt, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRain, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	####################################

	bt.prm.OK <- tkbutton(frButt, text="OK") 
	bt.prm.CA <- tkbutton(frButt, text = "Cancel") 

	###############
	ret.param.rain1 <<- NULL

	tkconfigure(bt.prm.OK, command = function(){
		GeneralParameters$parameter[[1]][,2] <<- c(tclvalue(min.stn), tclvalue(CInt), tclvalue(max.dist), tclvalue(elv.diff))
		GeneralParameters$parameter[[3]]$ispmax <<- tclvalue(ispmax)
		GeneralParameters$parameter[[3]]$ispobs <<- tclvalue(ispobs)
		GeneralParameters$parameter[[3]]$isdmin <<- tclvalue(isdmin)
		GeneralParameters$parameter[[3]]$isdobs <<- tclvalue(isdobs)
		GeneralParameters$parameter[[3]]$isdq1 <<- tclvalue(isdq1)
		GeneralParameters$parameter[[3]]$ftldev <<- tclvalue(ftldev)
		GeneralParameters$parameter$limSup <<- tclvalue(limSup)
		
		ret.param.rain1 <<- 0
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

	###############################################################	

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




homogen.get.info <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows"){
		largeur <- 35
		largeur1 <- 32
		largeur2 <- 29
	}else{
		largeur <- 25
		largeur1 <- 24
		largeur2 <- 24
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
	tclvalue(file.period) <- switch(GeneralParameters$period, 
									'daily' = cb.periodVAL[1], 
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3])
	file.choix1a <- tclVar(GeneralParameters$IO.files$Cand.file)
	file.choix1b <- tclVar(GeneralParameters$IO.files$Ref.file)

	ref.state <- if(GeneralParameters$stn.type$single.series & GeneralParameters$use.refSeries) 'normal' else 'disabled'

	cb.period <- ttkcombobox(frIO, values = cb.periodVAL, textvariable = file.period, width = largeur1)
	txt.file.stn1 <- tklabel(frIO, text = 'Candidate series data', anchor = 'w', justify = 'left')
	cb.file.stn1 <- ttkcombobox(frIO, values = unlist(listOpenFiles), textvariable = file.choix1a, width = largeur1)
	bt.file.stn1 <- tkbutton(frIO, text = "...")
	txt.file.stn2 <- tklabel(frIO, text = 'Reference series data', anchor = 'w', justify = 'left')
	cb.file.stn2 <- ttkcombobox(frIO, values = unlist(listOpenFiles), textvariable = file.choix1b, width = largeur1, state = ref.state)
	bt.file.stn2 <- tkbutton(frIO, text = "...", state = ref.state)

	####
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
		}else return(NULL)
	})

	####
	tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(txt.file.stn1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.file.stn1, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.file.stn1, row = 2, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.file.stn2, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.file.stn2, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.file.stn2, row = 4, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.period, 'Select the time step of the data')
	status.bar.display(cb.period, TextOutputVar, 'Select the time step of the data')
	infobulle(cb.file.stn1, 'Select the file in the list')
	status.bar.display(cb.file.stn1, TextOutputVar, 'Select the file containing the candidate series')
	infobulle(bt.file.stn1, 'Browse file if not listed')
	status.bar.display(bt.file.stn1, TextOutputVar, 'Browse file if not listed')
	infobulle(cb.file.stn2, 'Select the reference series in the list')
	status.bar.display(cb.file.stn2, TextOutputVar, 'Select the file containing the reference series')
	infobulle(bt.file.stn2, 'Browse file if not listed')
	status.bar.display(bt.file.stn2, TextOutputVar, 'Browse file if not listed')

	############################################

	frSave <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.save1 <- tclVar(GeneralParameters$IO.files$dir2save)

	txt.file.save <- tklabel(frSave, text = 'Directory to save result', anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frSave, textvariable = file.save1, width = largeur)
	bt.file.save <- tkbutton(frSave, text = "...")

	####
	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(str_trim(GeneralParameters$IO.files$dir2save), "")
		if(!file.exists(file2save1)){
			tkmessageBox(message = paste(file2save1, 'does not exist. It will be created.'), icon = "warning", type = "ok")
			dir.create(file2save1, recursive = TRUE)
			tclvalue(file.save1) <- file2save1
		}else tclvalue(file.save1) <- file2save1
	})

	####
	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(en.file.save, 'Enter the full path of the directory to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path of the directory to save result')
	infobulle(bt.file.save, 'or browse here')

	############################################
	tkgrid(frIO, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################  RIGHT   #####################

	frStat <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	hmg.mthd <- tclVar()
	homog.mthd <- c("Pettitt Test", "SNHT (Alexandersson & Moberg, 1997)",
					"CUSUM-type (Gallagher et al., 2013)",
					"CUSUM-type with Trend (Gallagher et al., 2013)")
	tclvalue(hmg.mthd) <- switch(GeneralParameters$hom.stats, 
									'Pettitt' = homog.mthd[1], 
									'SNHT' = homog.mthd[2],
									'CUSUM' = homog.mthd[3],
									'CUSUMtr' = homog.mthd[4])

	txt.homog.mthd <- tklabel(frStat, text = 'Detection statistic', anchor = 'w', justify = 'left')
	cb.homog.mthd <- ttkcombobox(frStat, values = homog.mthd, textvariable = hmg.mthd, width = largeur, state = 'readonly')
	bt.homog.mthd <- tkbutton(frStat, text = "Settings")

	####
	tkconfigure(bt.homog.mthd, command = function(){
		if(tclvalue(hmg.mthd) == homog.mthd[2]) Clev.list <- c('90.0', '92.0', '94.0', '95.0', '97.5', '99.0')
		else Clev.list <- c('90.0', '92.0', '95.0', '97.5', '99.0', '99.9')
		GeneralParameters <<- homogenization.opts(tt, GeneralParameters, Clev.list)
	})

	####
	tkgrid(txt.homog.mthd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.homog.mthd, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.homog.mthd, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.homog.mthd, 'Select a change points detection method')
	status.bar.display(cb.homog.mthd, TextOutputVar, 'Select a change points detection method')
	infobulle(bt.homog.mthd, 'Set options for homogenization procedures')
	status.bar.display(bt.homog.mthd, TextOutputVar, 'Set options for homogenization procedures')

	############################################

	frRefSeries <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	cb.1series.val <- tclVar(GeneralParameters$stn.type$single.series)
	cb.rfseries.val <- tclVar(GeneralParameters$use.refSeries)

	state1ser <- if(GeneralParameters$stn.type$single.series) 'normal' else 'disabled'
	staterf <- if(GeneralParameters$use.refSeries) 'normal' else 'disabled'

	cb.1series <- tkcheckbutton(frRefSeries, variable = cb.1series.val, text = 'Single Series', anchor = 'w', justify = 'left', width = largeur2)
	bt.1series <- tkbutton(frRefSeries, text = "Settings", state = state1ser)
	cb.rfseries <- tkcheckbutton(frRefSeries, variable = cb.rfseries.val, text = 'Use reference series', anchor = 'w', justify = 'left', width = largeur2)
	bt.rfseries <- tkbutton(frRefSeries, text = "Settings", state = staterf)
	txt.adjust <- tklabel(frRefSeries, text = 'Series adjustment parameters', anchor = 'w', justify = 'left', width = largeur2)
	bt.adjust <- tkbutton(frRefSeries, text = "Settings")

	####
	tkconfigure(bt.1series, command = function(){
		GeneralParameters <<- filedateformat(tt, GeneralParameters, tclvalue(file.period))
	})

	tkconfigure(bt.rfseries, command = function(){
		GeneralParameters <<- referenceseries(tt, GeneralParameters, tclvalue(file.choix1a), tclvalue(file.period))
	})

	tkconfigure(bt.adjust, command = function(){
		GeneralParameters <<- getAdjustparams(tt, GeneralParameters, tclvalue(file.period))
	})

	tkgrid(cb.1series, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.1series, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.rfseries, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.rfseries, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.adjust, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.adjust, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	####
	infobulle(cb.1series, 'Homogenization for one station series')
	status.bar.display(cb.1series, TextOutputVar, 'The data is a series of one station')
	infobulle(bt.1series, 'Set options for the file and date format')
	status.bar.display(bt.1series, TextOutputVar, 'Set options for the file and date format')
	infobulle(cb.rfseries, 'Using a reference series to perform the homogenization test')
	status.bar.display(cb.rfseries, TextOutputVar, 'Using a reference series to perform the homogenization test')
	infobulle(bt.rfseries, 'Set options to create the reference series')
	status.bar.display(bt.rfseries, TextOutputVar, 'Set options to create the reference series')
	infobulle(bt.adjust, 'Set options to adjust the series')
	status.bar.display(bt.adjust, TextOutputVar, 'Set options to adjust the series')

	####
	tkbind(cb.rfseries, "<Button-1>", function(){
	if(tclvalue(cb.rfseries.val) == "0" & tclvalue(cb.1series.val) == "1"){
			tkconfigure(cb.file.stn2, state = 'normal')
			tkconfigure(bt.file.stn2, state = 'normal')
		}else if(tclvalue(cb.1series.val) == "1"){
			tkconfigure(cb.file.stn2, state = 'disabled')
			tkconfigure(bt.file.stn2, state = 'disabled')
		}else{
			tkconfigure(cb.file.stn2, state = 'disabled')
			tkconfigure(bt.file.stn2, state = 'disabled')
		}

		if(tclvalue(cb.rfseries.val) == "0") tkconfigure(bt.rfseries, state = 'normal')
		else tkconfigure(bt.rfseries, state = 'disabled')
	})

	tkbind(cb.1series, "<Button-1>", function(){
		if(tclvalue(cb.1series.val) == "0" & tclvalue(cb.rfseries.val) == "1"){
			tkconfigure(cb.file.stn2, state = 'normal')
			tkconfigure(bt.file.stn2, state = 'normal')
		}else{
			tkconfigure(cb.file.stn2, state = 'disabled')
			tkconfigure(bt.file.stn2, state = 'disabled')
		}

		if(tclvalue(cb.1series.val) == "0") tkconfigure(bt.1series, state = 'normal')
		else tkconfigure(bt.1series, state = 'disabled')
	})

	############################################

	frAggr <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	cmptfun <- tclVar(str_trim(GeneralParameters$aggr.var$fonction))
	miss.frac <- tclVar(GeneralParameters$aggr.var$miss.frac)

	txt.cmptfun <- tklabel(frAggr, text = 'Aggregation')
	cb.cmptfun <- ttkcombobox(frAggr, values = c("mean", "sum"), textvariable = cmptfun, width = 8)
	missfrac.l <- tklabel(frAggr, text = 'Min.frac')
	missfrac.v <- tkentry(frAggr, textvariable = miss.frac, width = 6)

	tkgrid(txt.cmptfun, row = 0, column = 0, sticky = 'we', padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(cb.cmptfun, row = 0, column = 1, sticky = 'we', padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(missfrac.l, row = 0, column = 2, sticky = 'we', padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(missfrac.v, row = 0, column = 3, sticky = 'we', padx = 1, pady = 2, ipadx = 1, ipady = 1)

	infobulle(cb.cmptfun, 'Function to be used to aggregate data to dekadal and monthly series')
	status.bar.display(cb.cmptfun, TextOutputVar, 'Function to be used to aggregate data to dekadal and monthly series')
	infobulle(cb.cmptfun, 'Minimum fraction of available data that must be\npresent for the time period to compute')
	status.bar.display(cb.cmptfun, TextOutputVar, 'Minimum fraction of available data that must be\npresent for the time period to compute')

	############################################
	tkgrid(frStat, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRefSeries, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frAggr, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################

	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	###################################################

	bt.opt.OK <- tkbutton(frButt, text = "OK")
	bt.opt.CA <- tkbutton(frButt, text = "Cancel")

	tkconfigure(bt.opt.OK, command = function(){
		if(tclvalue(file.choix1a) == ""){
			tkmessageBox(message = "Provide the file to test", icon = "warning", type = "ok")
		}else if(tclvalue(cb.1series.val) == '1' & tclvalue(cb.rfseries.val) == '1' & tclvalue(file.choix1b) == ""){
			tkmessageBox(message = "Provide the file containing the reference series", icon = "warning", type = "ok")
		}else{
			donne <- getStnOpenData(file.choix1a)
			if(is.null(donne)){
				tkmessageBox(message = "Station data not found or in the wrong format", icon = "warning", type = "ok")
				tkwait.window(tt)
			}
			infoDonne <- getStnOpenDataInfo(file.choix1a)[2:3]

			if(tclvalue(cb.1series.val) == "1" & tclvalue(cb.rfseries.val) == "1"){
				donne1 <- getStnOpenData(file.choix1b)
				if(is.null(donne1)){
					tkmessageBox(message = "Data to be used for reference series not found or in the wrong format", icon = "warning", type = "ok")
					tkwait.window(tt)
				}
				infoDonne1 <- getStnOpenDataInfo(file.choix1b)[2:3]
			}

			##
			dirHomog <- file.path(tclvalue(file.save1), paste('HomogTest', getf.no.ext(tclvalue(file.choix1a)), sep = '_'))
			dirparams <- file.path(dirHomog, 'OriginalData')
			if(!file.exists(dirparams)) dir.create(dirparams, showWarnings = FALSE, recursive = TRUE)
			fileparams <- file.path(dirparams, 'Parameters.RData')

			assign('baseDir', dirHomog, envir = EnvHomogzData)

			GeneralParameters$period <<- switch(tclvalue(file.period), 
			 									'Daily data' = 'daily',
												'Dekadal data' = 'dekadal',
												'Monthly data' = 'monthly')
			GeneralParameters$IO.files$Cand.file <<- str_trim(tclvalue(file.choix1a))
			GeneralParameters$IO.files$Ref.file <<- str_trim(tclvalue(file.choix1b))
			GeneralParameters$IO.files$dir2save <<- str_trim(tclvalue(file.save1))

			GeneralParameters$hom.stats <<- switch(tclvalue(hmg.mthd),
													"Pettitt Test" = 'Pettitt', 
													"SNHT (Alexandersson & Moberg, 1997)" = 'SNHT',
													"CUSUM-type (Gallagher et al., 2013)" = 'CUSUM',
													"CUSUM-type with Trend (Gallagher et al., 2013)" = 'CUSUMtr')
			GeneralParameters$stn.type$single.series <<- switch(tclvalue(cb.1series.val), '0' = FALSE, '1' = TRUE)
			GeneralParameters$use.refSeries <<- switch(tclvalue(cb.rfseries.val), '0' = FALSE, '1' = TRUE)

			GeneralParameters$aggr.var$fonction <<- str_trim(tclvalue(cmptfun))
			GeneralParameters$aggr.var$miss.frac <<- as.numeric(str_trim(tclvalue(miss.frac)))

			######

			getInitDataParams <- function(GeneralParameters){
				if(tclvalue(cb.1series.val) == "0"){
					donstn <- getCDTdataAndDisplayMsg(donne, GeneralParameters$period)
					donOut <- list(donstn)
					parsFile <- list(infoDonne)
					if(!is.null(donstn)) lchoixStnFr$env$stn.choix <- as.character(donstn$id)
					else tkwait.window(tt)
				}else{
					donstn <- getCDTTSdataAndDisplayMsg(donne, GeneralParameters$period,
												GeneralParameters$stn.type$file.format,
												GeneralParameters$stn.type$date.format)
					donOut <- list(donstn)
					parsFile <- list(infoDonne)
					if(tclvalue(cb.rfseries.val) == "1"){
						donstn1 <- getCDTTSdataAndDisplayMsg(donne1, GeneralParameters$period,
													GeneralParameters$stn.type$file.format,
													GeneralParameters$stn.type$date.format)
						donOut <- list(donstn, donstn1)
						parsFile <- list(infoDonne, infoDonne1)
					}
					if(!is.null(donstn)) lchoixStnFr$env$stn.choix <- getf.no.ext(tclvalue(file.choix1a))
					else tkwait.window(tt)
				}
				paramsGAL <- list(inputPars = GeneralParameters, dataPars = parsFile, data = donOut)
				save(paramsGAL, file = fileparams)
				return(list(paramsGAL, lchoixStnFr$env$stn.choix))
			}

			#####
			getRefSrData <- function(){
				donstn1 <- getCDTTSdataAndDisplayMsg(donne1, GeneralParameters$period,
											GeneralParameters$stn.type$file.format,
											GeneralParameters$stn.type$date.format)
				if(is.null(donstn1)){
					tkmessageBox(message = "Data to be used for reference series not found or in the wrong format", icon = "warning", type = "ok")
					tkwait.window(tt)
				}
				load(fileparams)
				paramsGAL$data[[2]] <- donstn1
				paramsGAL$dataPars[[2]] <- infoDonne1
				save(paramsGAL, file = fileparams)
				return(paramsGAL)
			}

			######
			xtest <- tclvalue(cb.1series.val) == "1" & tclvalue(cb.rfseries.val) == "1"
			if(file.exists(fileparams)){
				load(fileparams)
				intest1 <- paramsGAL$inputPars$period == GeneralParameters$period & all(infoDonne%in%paramsGAL$dataPars[[1]])
				if(intest1){
					assign('donnees1', paramsGAL$data[[1]], envir = EnvHomogzData)
					assign('dly_data', paramsGAL$data1$dly_data, envir = EnvHomogzData)
					assign('dek_data', paramsGAL$data1$dek_data, envir = EnvHomogzData)
					assign('mon_data', paramsGAL$data1$mon_data, envir = EnvHomogzData)
					if(xtest){
						assign('donnees2', paramsGAL$data[[2]], envir = EnvHomogzData)
						if(length(paramsGAL$dataPars) == 1){
							paramsGAL <- getRefSrData()
							assign('donnees2', paramsGAL$data[[2]], envir = EnvHomogzData)
							##recalculate
							computeHomogData(GeneralParameters)
						}else{
							ctest1 <- all(infoDonne1%in%paramsGAL$dataPars[[2]])
							if(!ctest1){
								paramsGAL <- getRefSrData()
								assign('donnees2', paramsGAL$data[[2]], envir = EnvHomogzData)
								##recalculate
								computeHomogData(GeneralParameters)
							}
						}
					}
					if(tclvalue(cb.1series.val) == "0") lchoixStnFr$env$stn.choix <<- as.character(paramsGAL$data[[1]]$id)
					else lchoixStnFr$env$stn.choix <<- getf.no.ext(tclvalue(file.choix1a))
					paramsGAL$inputPars <- GeneralParameters
					save(paramsGAL, file = fileparams)
					rm(paramsGAL)
				}else{
					retDonPar <- getInitDataParams(GeneralParameters)
					assign('donnees1', retDonPar[[1]]$data[[1]], envir = EnvHomogzData)
					if(xtest) assign('donnees2', retDonPar[[1]]$data[[2]], envir = EnvHomogzData)
					GeneralParameters <<- retDonPar[[1]]$inputPars
					lchoixStnFr$env$stn.choix <<- retDonPar[[2]]
					#calculate mon/dek
					computeHomogData(GeneralParameters)
					paramsGAL <- retDonPar[[1]]
					paramsGAL$data1 <- list(dly_data = EnvHomogzData$dly_data, dek_data = EnvHomogzData$dek_data, mon_data = EnvHomogzData$mon_data)
					save(paramsGAL, file = fileparams)
					rm(retDonPar, paramsGAL)
				}
			}else{
				retDonPar <- getInitDataParams(GeneralParameters)
				assign('donnees1', retDonPar[[1]]$data[[1]], envir = EnvHomogzData)
				if(xtest) assign('donnees2', retDonPar[[1]]$data[[2]], envir = EnvHomogzData)
				GeneralParameters <<- retDonPar[[1]]$inputPars
				lchoixStnFr$env$stn.choix <<- retDonPar[[2]]
				#calculate mon/dek
				computeHomogData(GeneralParameters)
				paramsGAL <- retDonPar[[1]]
				paramsGAL$data1 <- list(dly_data = EnvHomogzData$dly_data, dek_data = EnvHomogzData$dek_data, mon_data = EnvHomogzData$mon_data)
				save(paramsGAL, file = fileparams)
				rm(retDonPar, paramsGAL)
			}

			##################
			##set choix stn

			if(GeneralParameters$retpar == 0){
				if(lchoixStnFr$env$stn.choix[1] != '') tclvalue(lchoixStnFr$env$stn.choix.val) <- lchoixStnFr$env$stn.choix[1]
				else tclvalue(lchoixStnFr$env$stn.choix.val) <- lchoixStnFr$env$stn.choix[2]
			}else{
				istn <- as.numeric(tclvalue(tcl(lchoixStnFr$env$stn.choix.cb, "current")))+1
				if(istn > 0) istn <- istn
				else istn <- 1
				tclvalue(lchoixStnFr$env$stn.choix.val) <- lchoixStnFr$env$stn.choix[istn]
			}

			tkconfigure(lchoixStnFr$env$stn.choix.cb, values = lchoixStnFr$env$stn.choix, textvariable = lchoixStnFr$env$stn.choix.val)
			tkconfigure(lchoixStnFr$env$setting.button, state = 'normal')
			if(tclvalue(cb.1series.val) == '0'){
				tkconfigure(lchoixStnFr$env$stn.choix.prev, state = 'normal')
				tkconfigure(lchoixStnFr$env$stn.choix.next, state = 'normal')
			}
			tkconfigure(spinH, state = 'normal')
			tkconfigure(spinV, state = 'normal')

			####button command
			if(is.null(lcmd.frame_homo)){
				retcmdpars <- HomogCmdBut(GeneralParameters)
				lcmd.frame <<- retcmdpars[[1]]
				lcmd.frame_homo <<- 1
				GeneralParameters <<- retcmdpars[[2]]
			}
			GeneralParameters$retpar <<- GeneralParameters$retpar+1

			########
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
	tkwm.title(tt, 'Data Homogenization- Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {tkgrab.release(tt); tkfocus(parent.win)})
	tkwait.window(tt)
	return(GeneralParameters)
}

############################################################################################

filedateformat <- function(top.win, GeneralParameters, speriod){
	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	fdf1 <- tkframe(tt1)
	fdf2 <- tkframe(tt1)

	#################################################
	fr.fileformat1 <- ttklabelframe(fdf1, text = "File Format", labelanchor = "nw", relief = "groove", borderwidth = 2)

	rbffrmt <- tclVar(GeneralParameters$stn.type$file.format)
	varcat <- tclVar(GeneralParameters$stn.type$vars)

	ffrmt1 <- tkradiobutton(fr.fileformat1, text = "One variable", anchor = 'w', justify = 'left')
	ffrmt2 <- tkradiobutton(fr.fileformat1, text = "Rain Tmax Tmin", anchor = 'w', justify = 'left')
	tkconfigure(ffrmt1, variable = rbffrmt, value = "1")
	tkconfigure(ffrmt2, variable = rbffrmt, value = "0")
	varframe <- tkframe(fr.fileformat1)
	var2test1 <- tkradiobutton(varframe, variable = varcat, value = "1")
	var2test2 <- tkradiobutton(varframe, variable = varcat, value = "2")
	var2test3 <- tkradiobutton(varframe, variable = varcat, value = "3")

	tkgrid(var2test1, row = 0, column = 0, padx = 2)
	tkgrid(var2test2, row = 0, column = 1, padx = 2)
	tkgrid(var2test3, row = 0, column = 2, padx = 2)

	tkgrid(ffrmt1, row = 0, column = 0, sticky = "we")
	tkgrid(ffrmt2, row = 1, column = 0, sticky = "we")
	tkgrid(varframe, row = 3, column = 0, sticky = "e")

	infobulle(ffrmt1, 'In case of single series: the file contains 1 variable')
	status.bar.display(ffrmt1, TextOutputVar, 'In case of single series: the file contains 1 variable')
	infobulle(ffrmt2, 'In case of single series: the file contains Rain, Tmax and Tmin in this order')
	status.bar.display(ffrmt2, TextOutputVar, 'In case of single series: the file contains Rain, Tmax and Tmin in this order')
	infobulle(varframe, 'Choose the variable to test')
	status.bar.display(varframe, TextOutputVar, 'Choose the variable to test')

	###########
	fr.fileformat2 <- ttklabelframe(fdf1, text = "Dates Format", labelanchor = "nw", relief = "groove", borderwidth = 2)

	rbdtfrmt <- tclVar(GeneralParameters$stn.type$date.format)

	if(speriod == 'Daily data'){
		txtdtfrmt1 <- "YYYYMMDD"
		txtdtfrmt2 <- "YYYY MM DD"
	}
	if(speriod == 'Dekadal data'){
		txtdtfrmt1 <- "YYYYMMD"
		txtdtfrmt2 <- "YYYY MM D"
	}
	if(speriod == 'Monthly data'){
		txtdtfrmt1 <- "YYYYMM"
		txtdtfrmt2 <- "YYYY MM"
	}

	dtfrmt1 <- tkradiobutton(fr.fileformat2, text = txtdtfrmt1, anchor = 'w', justify = 'left')
	dtfrmt2 <- tkradiobutton(fr.fileformat2, text = txtdtfrmt2, anchor = 'w', justify = 'left')
	tkconfigure(dtfrmt1, variable = rbdtfrmt, value = "1")
	tkconfigure(dtfrmt2, variable = rbdtfrmt, value = "0")
	txtblank <- tklabel(fr.fileformat2, text = '')

	tkgrid(dtfrmt1, row = 0, column = 0, sticky = "we")
	tkgrid(dtfrmt2, row = 1, column = 0, sticky = "we")
	tkgrid(txtblank, row = 3, column = 0, pady = 1, sticky = "we")

	infobulle(dtfrmt1, 'In case of single series: dates are merged')
	status.bar.display(dtfrmt1, TextOutputVar, 'In case of single series: dates are merged')
	infobulle(dtfrmt2, 'In case of single series: dates are separated by space, tabulation or CSV format')
	status.bar.display(dtfrmt2, TextOutputVar, 'In case of single series: dates are separated by space, tabulation or CSV format')

	#################################################

	tkgrid(fr.fileformat1, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.fileformat2, row = 0, column = 1, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	#################################################
	bt.fileformat <- tkbutton(fdf2, text = "OK")

	tkconfigure(bt.fileformat, command = function(){
		GeneralParameters$stn.type$file.format <<- tclvalue(rbffrmt)
		GeneralParameters$stn.type$vars <<- tclvalue(varcat)
		GeneralParameters$stn.type$date.format <<- tclvalue(rbdtfrmt)

		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkgrid(bt.fileformat, ipadx = 10)

	#################################################
	tkgrid(fdf1, sticky = 'we', padx = 5, pady = 5)
	tkgrid(fdf2, padx = 5, pady = 5)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'Series Format')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(top.win)})
	tkwait.window(tt1)
	return(GeneralParameters)
}

############################################################################################

referenceseries <- function(top.win, GeneralParameters, file2test, speriod){
	listOpenFiles <- openFile_ttkcomboList()
	largeur <- if (Sys.info()["sysname"] == "Windows") 28 else 24

	################################
	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frcont <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frok <- tkframe(tt1)

	frLeft <- tkframe(frcont)
	frRight <- tkframe(frcont)

	################################

	frRefS <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	frTestS <- ttklabelframe(frRefS, text = "Test series constitution", labelanchor = "nw", relief = "groove", borderwidth = 2)

	diff.ratio <- tclVar(GeneralParameters$refSeries.choix$diff.ratio)

	dif.rat1 <- tkradiobutton(frTestS, text = "Difference", anchor = 'w', justify = 'left')
	dif.rat2 <- tkradiobutton(frTestS, text = "Ratio", anchor = 'w', justify = 'left')
	dif.rat3 <- tkradiobutton(frTestS, text = "LogRatio", anchor = 'w', justify = 'left')
	tkconfigure(dif.rat1, variable = diff.ratio, value = "1")
	tkconfigure(dif.rat2, variable = diff.ratio, value = "2")
	tkconfigure(dif.rat3, variable = diff.ratio, value = "3")

	tkgrid(dif.rat1, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(dif.rat2, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(dif.rat3, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)

	infobulle(dif.rat1, 'Constitution of relative comparison series: Candidate-Reference')
	status.bar.display(dif.rat1, TextOutputVar, 'Constitution of relative comparison series: Candidate-Reference')
	infobulle(dif.rat2, 'Constitution of relative comparison series: Candidate/Reference')
	status.bar.display(dif.rat2, TextOutputVar, 'Constitution of relative comparison series: Candidate/Reference')
	infobulle(dif.rat3, 'Constitution of relative comparison series: log(Candidate/Reference)')
	status.bar.display(dif.rat3, TextOutputVar, 'Constitution of relative comparison series: log(Candidate/Reference)')

	################################

	frWeig <- ttklabelframe(frRefS, text = "Weighting factors", labelanchor = "nw", relief = "groove", borderwidth = 2)

	weight.fac <- tclVar(GeneralParameters$refSeries.choix$weight.mean)

	wmean1 <- tkradiobutton(frWeig, text = "Correlation", anchor = 'w', justify = 'left')
	wmean2 <- tkradiobutton(frWeig, text = "Distance", anchor = 'w', justify = 'left')
	wmean3 <- tkradiobutton(frWeig, text = "Optimal", anchor = 'w', justify = 'left')
	tkconfigure(wmean1, variable = weight.fac, value = "1")
	tkconfigure(wmean2, variable = weight.fac, value = "2")
	tkconfigure(wmean3, variable = weight.fac, value = "3")

	tkgrid(wmean1, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(wmean2, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(wmean3, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)

	infobulle(wmean1, 'Use the square of the correlation coefficient as the weight factor')
	status.bar.display(wmean1, TextOutputVar, 'Use the square of the correlation coefficient as the weight factor')
	infobulle(wmean2, 'Use the square of the inverse of distance as the weight factor')
	status.bar.display(wmean2, TextOutputVar, 'Use the square of the inverse of distance as the weight factor')
	infobulle(wmean3, 'Optimal weighting using covariance matrix (ordinary kriging method)')
	status.bar.display(wmean3, TextOutputVar, 'Optimal weighting using covariance matrix (ordinary kriging method)')

	################################

	tkgrid(frTestS, row = 0, column = 0, sticky = 'nswe', padx = 10, pady = 1)
	tkgrid(frWeig, row = 0, column = 1, sticky = 'nswe', padx = 10, pady = 1)

	################################

	frOpts <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)
	frOptsL <- ttklabelframe(frOpts, text = "Reference series selection options", labelanchor = "nw", relief = "groove", borderwidth = 2)

	min.stn <- tclVar(GeneralParameters$refSeries.choix$min.stn)
	max.stn <- tclVar(GeneralParameters$refSeries.choix$max.stn)
	max.dist <- tclVar(GeneralParameters$refSeries.choix$max.dist)
	elv.diff <- tclVar(GeneralParameters$refSeries.choix$elv.diff)
	min.rho <- tclVar(GeneralParameters$refSeries.choix$min.rho)

	min.stn.l <- tklabel(frOptsL, text = 'Min.stn', anchor = 'e', justify = 'right')
	min.stn.v <- tkentry(frOptsL, textvariable = min.stn, width = 4)
	max.stn.l <- tklabel(frOptsL, text = 'Max.stn', anchor = 'e', justify = 'right')
	max.stn.v <- tkentry(frOptsL, textvariable = max.stn, width = 4)
	min.rho.l <- tklabel(frOptsL, text = 'Min.rho', anchor = 'e', justify = 'right')
	min.rho.v <- tkentry(frOptsL, textvariable = min.rho, width = 4)
	max.dist.l <- tklabel(frOptsL, text = 'Max.dist(km)', anchor = 'e', justify = 'right')
	max.dist.v <- tkentry(frOptsL, textvariable = max.dist, width = 4)
	elv.diff.l <- tklabel(frOptsL, text = 'Elv.diff(m)', anchor = 'e', justify = 'right')
	elv.diff.v <- tkentry(frOptsL, textvariable = elv.diff, width = 4)

	tkgrid(min.stn.l, row = 0, column = 0, sticky = 'ew', pady = 1)
	tkgrid(min.stn.v, row = 0, column = 1, sticky = 'ew', pady = 1)
	tkgrid(max.stn.l, row = 0, column = 2, sticky = 'ew', pady = 1)
	tkgrid(max.stn.v, row = 0, column = 3, sticky = 'ew', pady = 1)

	tkgrid(min.rho.l, row = 0, column = 4, sticky = 'ew', pady = 1)
	tkgrid(min.rho.v, row = 0, column = 5, sticky = 'ew', pady = 1)

	tkgrid(max.dist.l, row = 1, column = 0, sticky = 'ew', pady = 1)
	tkgrid(max.dist.v, row = 1, column = 1, sticky = 'ew', pady = 1)
	tkgrid(elv.diff.l, row = 1, column = 2, sticky = 'ew', pady = 1)
	tkgrid(elv.diff.v, row = 1, column = 3, sticky = 'ew', pady = 1)

	tkgrid(frOptsL, sticky = 'ew', padx = 5, pady = 2)

	################################
	infobulle(min.stn.v, 'Minimum number of neighbor stations to use')
	status.bar.display(min.stn.v, TextOutputVar, 'Minimum number of neighbor stations to use')
	infobulle(max.stn.v, 'Maximum number of neighbor stations to use')
	status.bar.display(max.stn.v, TextOutputVar, 'Maximum number of neighbor stations to use')
	infobulle(min.rho.v, 'Minimum correlation coefficient between candidate and neighbor series')
	status.bar.display(min.rho.v, TextOutputVar, 'Minimum correlation coefficient between candidate and neighbor series')
	infobulle(elv.diff.v, 'Maximum altitude difference of neighbor stations to use (m)')
	status.bar.display(elv.diff.v, TextOutputVar, 'Maximum altitude difference of neighbor stations to use (m)')
	infobulle(max.dist.v, 'Maximum distance of neighbor stations to use (km)')
	status.bar.display(max.dist.v, TextOutputVar, 'Maximum distance of neighbor stations to use (km)')

	################################
	tkgrid(frRefS, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frOpts, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################  RIGHT   #####################

	frURef <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	usr.rfseries.val <- tclVar(GeneralParameters$refSeries.by.user)
	state0 <- if(GeneralParameters$refSeries.by.user) 'normal' else 'disabled'

	ch.usr.rfseries <- tkcheckbutton(frURef, variable = usr.rfseries.val, text = "Stations selected by User", anchor = 'w', justify = 'left')
	bt.usr.rfseries <- tkbutton(frURef, text = "Select", state = state0)

	######
	tkconfigure(bt.usr.rfseries, command = function(){
		if(file2test != ""){
			donstn <- getStnOpenData(file2test)
			if(is.null(donstn)){
				tkmessageBox(message = "File not found or in the wrong format", icon = "warning", type = "ok")
				return(NULL)
			}else{
				speriod <- switch(speriod,
							'Daily data' = 'daily',
							'Dekadal data' = 'dekadal',
							'Monthly data' = 'monthly')
				donstn <- getCDTdataAndDisplayMsg(donstn, speriod)
				GeneralParameters <<- refSeriesUsersChoice(tt1, donstn$id, GeneralParameters)
			}
		}else{
			tkmessageBox(message = "Provide the file to test", icon = "warning", type = "ok")
			tkgrab.release(tt1)
			tkdestroy(tt1)
			tkfocus(top.win)
		}
	})

	######
	tkgrid(ch.usr.rfseries, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.usr.rfseries, row = 0, column = 1, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(ch.usr.rfseries, 'The reference series will be created from stations chosen by user')
	status.bar.display(ch.usr.rfseries, TextOutputVar, "The reference series will be created from stations chosen by user")
	infobulle(bt.usr.rfseries, 'Select the stations to create the reference series')
	status.bar.display(bt.usr.rfseries, TextOutputVar, 'Select the stations to create the reference series')

	######
	tkbind(ch.usr.rfseries, "<Button-1>", function(){
		if(tclvalue(usr.rfseries.val) == "0") tkconfigure(bt.usr.rfseries, state = 'normal')
		else tkconfigure(bt.usr.rfseries, state = 'disabled')
	})

	################################

	frElvSelct <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	uselv.val <- tclVar(GeneralParameters$refSeries.choix$use.elv)
	uselv.ch <- tclVar(GeneralParameters$refSeries.choix$interp.dem)

	if(GeneralParameters$refSeries.choix$use.elv){
		if(str_trim(GeneralParameters$refSeries.choix$interp.dem) == '0'){
			state <- c('normal', 'normal')
		}else state <- c('normal', 'disabled')
	}else state <- c('disabled', 'disabled')

	cb.uselv <- tkcheckbutton(frElvSelct, variable = uselv.val, text = 'Use Elevation', anchor = 'w', justify = 'left')
	uselv.dem <- tkradiobutton(frElvSelct, text = "Elevation from DEM", anchor = 'w', justify = 'left', state = state[1])
	uselv.dat <- tkradiobutton(frElvSelct, text = "Elevation from STN data", anchor = 'w', justify = 'left', state = state[1])
	tkconfigure(uselv.dem, variable = uselv.ch, value = "0")
	tkconfigure(uselv.dat, variable = uselv.ch, value = "1")

	tkgrid(cb.uselv, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(uselv.dem, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(uselv.dat, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)

	infobulle(cb.uselv, 'Check to use elevation data to select neighbors stations')
	status.bar.display(cb.uselv, TextOutputVar, 'Check for using elevation data to select neighbor stations')
	infobulle(uselv.dem, 'Check if the elevation data will be extracted from DEM')
	status.bar.display(uselv.dem, TextOutputVar, 'Check if the elevation data will be extracted from DEM')
	infobulle(uselv.dat, 'Check to use elevation data from the stations data')
	status.bar.display(uselv.dat, TextOutputVar, 'Check to use elevation data from the stations data')

	###########
	tkbind(cb.uselv, "<Button-1>", function(){
		if(tclvalue(uselv.val) == "0" & tclvalue(uselv.ch) == "1"){
			tkconfigure(uselv.dem, state = 'normal')
			tkconfigure(uselv.dat, state = 'normal')
			tkconfigure(cb.file.elv, state = 'disabled')
			tkconfigure(bt.file.elv, state = 'disabled')
		}else if(tclvalue(uselv.val) == "0" & tclvalue(uselv.ch) == "0"){
			tkconfigure(uselv.dem, state = 'normal')
			tkconfigure(uselv.dat, state = 'normal')
			tkconfigure(cb.file.elv, state = 'normal')
			tkconfigure(bt.file.elv, state = 'normal')
		}else{
			tkconfigure(uselv.dem, state = 'disabled')
			tkconfigure(uselv.dat, state = 'disabled')
			tkconfigure(cb.file.elv, state = 'disabled')
			tkconfigure(bt.file.elv, state = 'disabled')
		}
	})

	tkbind(uselv.dat, "<Button-1>", function(){
		if(tclvalue(uselv.ch) == "0" & tclvalue(uselv.val) == "1"){
			tkconfigure(uselv.dem, state = 'normal')
			tkconfigure(uselv.dat, state = 'normal')
			tkconfigure(cb.file.elv, state = 'disabled')
			tkconfigure(bt.file.elv, state = 'disabled')
		}
	})

	tkbind(uselv.dem, "<Button-1>", function(){
		if(tclvalue(uselv.ch) == "1" & tclvalue(uselv.val) == "1"){
			tkconfigure(uselv.dem, state = 'normal')
			tkconfigure(uselv.dat, state = 'normal')
			tkconfigure(cb.file.elv, state = 'normal')
			tkconfigure(bt.file.elv, state = 'normal')
		}
	})

	################################

	frDem <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	file.choix2 <- tclVar(GeneralParameters$IO.files$DEM.file)

	txt.file.elv <- tklabel(frDem, text = 'Elevation Data (NetCDF)',anchor = 'w', justify = 'left')
	cb.file.elv <- ttkcombobox(frDem, values = unlist(listOpenFiles), textvariable = file.choix2, state = state[2], width = largeur)
	bt.file.elv <- tkbutton(frDem, text = '...', state = state[2])

	####
	tkconfigure(bt.file.elv, command = function(){
		nc.opfiles <- getOpenNetcdf(top.win, all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles
			tclvalue(file.choix2) <- AllOpenFilesData[[nopf+1]][[1]]

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.choix2) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.file.elv, values = unlist(listOpenFiles), textvariable = file.choix2)
		}else return(NULL)
	})

	####
	tkgrid(txt.file.elv, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.file.elv, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.file.elv, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.file.elv, 'Choose the file in the list')
	status.bar.display(cb.file.elv, TextOutputVar, 'Choose the file containing the elevation data')
	infobulle(bt.file.elv, 'Browse file if not listed')
	status.bar.display(bt.file.elv, TextOutputVar, 'Browse file if not listed')

	################################
	tkgrid(frURef, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frElvSelct, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frDem, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	################################

	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################

	bt.prm.OK <- tkbutton(frok, text = "OK")
	bt.prm.CA <- tkbutton(frok, text = "Cancel")

	tkconfigure(bt.prm.OK, command = function(){
		if(tclvalue(uselv.val) == "1" & tclvalue(uselv.ch) == "0" & tclvalue(file.choix2) == ""){
			tkmessageBox(message = "Provide the NetCDF file containing the elevation data", icon = "warning", type = "ok")
		}else{
			GeneralParameters$refSeries.choix$diff.ratio <<- tclvalue(diff.ratio)
			GeneralParameters$refSeries.choix$weight.mean <<- tclvalue(weight.fac)
			GeneralParameters$refSeries.choix$min.stn <<- as.numeric(str_trim(tclvalue(min.stn)))
			GeneralParameters$refSeries.choix$max.stn <<- as.numeric(str_trim(tclvalue(max.stn)))
			GeneralParameters$refSeries.choix$max.dist <<- as.numeric(str_trim(tclvalue(max.dist)))
			GeneralParameters$refSeries.choix$elv.diff <<- as.numeric(str_trim(tclvalue(elv.diff)))
			GeneralParameters$refSeries.choix$min.rho <<- as.numeric(str_trim(tclvalue(min.rho)))

			GeneralParameters$refSeries.by.user <<- switch(tclvalue(usr.rfseries.val), '0' = FALSE, '1' = TRUE)
			GeneralParameters$refSeries.choix$use.elv <<- switch(tclvalue(uselv.val), '0' = FALSE, '1' = TRUE)
			GeneralParameters$refSeries.choix$interp.dem <<- tclvalue(uselv.ch)
			GeneralParameters$IO.files$DEM.file <<- str_trim(tclvalue(file.choix2))

			tkgrab.release(tt1)
			tkdestroy(tt1)
			tkfocus(top.win)
		}
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 10)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1)

	###############################################################

	tkgrid(frcont, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frok, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'Reference series creation')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(top.win)})
	tkwait.window(tt1)
	return(GeneralParameters)
}


############################################################################################

homogenization.opts <- function(top.win, GeneralParameters, Clev.list){
	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frE1 <- tkframe(tt1, relief = 'sunken', borderwidth = 2)
	frE2 <- tkframe(tt1, relief = 'sunken', borderwidth = 2)
	frE3 <- tkframe(tt1)

	#################################################

	cb.bounds.val <- tclVar(GeneralParameters$hom.stats.opts$crop.bounds)

	crop.bounds.list <- c('0.010', '0.025', '0.050', '0.100')
	crop.bounds.val <- tclVar(sprintf("%.3f", GeneralParameters$hom.stats.opts$h))
	Clev.val <- tclVar(sprintf("%.1f", GeneralParameters$hom.stats.opts$conf.lev))
	Kmax <- tclVar(GeneralParameters$hom.stats.opts$Kmax)
	min.int <- tclVar(GeneralParameters$hom.stats.opts$min.int)

	###############
	cb.bounds <- tkcheckbutton(frE1, variable = cb.bounds.val, text = 'Crop bounds', anchor = 'w', justify = 'left')

	h.l <- tklabel(frE2, text = 'h', anchor = 'e', justify = 'right')
	h.v <- ttkcombobox(frE2, values = crop.bounds.list, textvariable = crop.bounds.val, width = 6)
	conf.lev.l <- tklabel(frE2, text = 'Conf.lev(%)', anchor = 'e', justify = 'right')
	conf.lev.v <- ttkcombobox(frE2, values = Clev.list, textvariable = Clev.val, width = 6)
	Kmax.l <- tklabel(frE2, text = 'Kmax', anchor = 'e', justify = 'right')
	Kmax.v <- tkentry(frE2, textvariable = Kmax, width = 6)
	min.int.l <- tklabel(frE2, text = 'Min.len(months)', anchor = 'e', justify = 'right')
	min.int.v <- tkentry(frE2, textvariable = min.int, width = 6)

	###############
	tkgrid(cb.bounds, pady = 2)

	tkgrid(h.l, row = 0, column = 0, sticky = 'we', padx = 1, pady = 2)
	tkgrid(h.v, row = 0, column = 1, sticky = 'we', padx = 1, pady = 2)
	tkgrid(conf.lev.l, row = 0, column = 2, sticky = 'we', padx = 1, pady = 2)
	tkgrid(conf.lev.v, row = 0, column = 3, sticky = 'we', padx = 1, pady = 2)
	tkgrid(Kmax.l, row = 1, column = 0, sticky = 'we', padx = 1, pady = 2)
	tkgrid(Kmax.v, row = 1, column = 1, sticky = 'we', padx = 1, pady = 2)
	tkgrid(min.int.l, row = 1, column = 2, sticky = 'we', padx = 1, pady = 2)
	tkgrid(min.int.v, row = 1, column = 3, sticky = 'we', padx = 1, pady = 2)

	###############
	infobulle(cb.bounds, 'Cropping the first and last [h x 100%] percent of the series')
	status.bar.display(cb.bounds, TextOutputVar, 'Cropping the first and last [h x 100%] percent of the series')

	infobulle(h.v, 'Cropping the first and last [h x 100%] percent of the series')
	status.bar.display(h.v, TextOutputVar, 'Cropping the first and last [h x 100%] percent of the series')
	infobulle(conf.lev.v, 'Confidence level (%)')
	status.bar.display(conf.lev.v, TextOutputVar, 'Confidence level (%)')
	infobulle(Kmax.v, 'Maximum number of change-points to be detected')
	status.bar.display(Kmax.v, TextOutputVar, 'Maximum number of change-points to be detected')
	infobulle(min.int.v, 'Minimum segment length to carry out the test')
	status.bar.display(min.int.v, TextOutputVar, 'Minimum segment length to carry out the test')


	#################################################
	bt.prm.OK <- tkbutton(frE3, text = "OK")

	tkconfigure(bt.prm.OK, command = function(){
		GeneralParameters$hom.stats.opts$crop.bounds <<- switch(tclvalue(cb.bounds.val), '0' = FALSE, '1' = TRUE)
		GeneralParameters$hom.stats.opts$h <<- as.numeric(str_trim(tclvalue(crop.bounds.val)))
		GeneralParameters$hom.stats.opts$conf.lev <<- as.numeric(str_trim(tclvalue(Clev.val)))
		GeneralParameters$hom.stats.opts$Kmax <<- as.numeric(str_trim(tclvalue(Kmax)))
		GeneralParameters$hom.stats.opts$min.int <<- as.numeric(str_trim(tclvalue(min.int)))

		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkgrid(bt.prm.OK, ipadx = 10)

	#################################################
	tkgrid(frE1, sticky = 'we', padx = 5, pady = 5)
	tkgrid(frE2, sticky = 'we', padx = 5, pady = 5)
	tkgrid(frE3, padx = 5, pady = 5)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'Statistics Test option')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(top.win)})
	tkwait.window(tt1)
	return(GeneralParameters)
}

############################################################################################

getAdjustparams <- function(top.win, GeneralParameters, speriod){
	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	fdf1 <- tkframe(tt1, relief = "groove", borderwidth = 2)
	fdf2 <- tkframe(tt1)

	################################

	minadjmo <- tclVar(GeneralParameters$adjust.pars$minAdjmo)
	minadjdk <- tclVar(GeneralParameters$adjust.pars$minAdjdk)
	minadjdy <- tclVar(GeneralParameters$adjust.pars$minAdjdy)
	segadjmo <- tclVar(GeneralParameters$adjust.pars$SegAdjmo)
	segadjdk <- tclVar(GeneralParameters$adjust.pars$SegAdjdk)
	segadjdy <- tclVar(GeneralParameters$adjust.pars$SegAdjdy)

	if(speriod == 'Daily data') state <- c('normal', 'normal')
	if(speriod == 'Dekadal data') state <- c('normal', 'disabled')
	if(speriod == 'Monthly data') state <- c('disabled', 'disabled')

	minadj.l <- tklabel(fdf1, text = 'Min.Adj (in month)', anchor = 'e', justify = 'right')
	segadj.l <- tklabel(fdf1, text = 'Segment to Adjust', anchor = 'e', justify = 'right')
	monadj.l <- tklabel(fdf1, text = 'Month')
	dekadj.l <- tklabel(fdf1, text = 'Dekad')
	dlyadj.l <- tklabel(fdf1, text = 'Day')
	minadjdy.v <- tkentry(fdf1, textvariable = minadjdy, state = state[2], width = 3)
	segadjdy.v <- tkentry(fdf1, textvariable = segadjdy, state = state[2], width = 3)
	minadjdk.v <- tkentry(fdf1, textvariable = minadjdk, state = state[1], width = 3)
	segadjdk.v <- tkentry(fdf1, textvariable = segadjdk, state = state[1], width = 3)
	minadjmo.v <- tkentry(fdf1, textvariable = minadjmo, width = 3)
	segadjmo.v <- tkentry(fdf1, textvariable = segadjmo, width = 3)

	tkgrid(monadj.l, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(dekadj.l, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(dlyadj.l, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(minadj.l, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(minadjmo.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(minadjdk.v, row = 1, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(minadjdy.v, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(segadj.l, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(segadjmo.v, row = 2, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(segadjdk.v, row = 2, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(segadjdy.v, row = 2, column = 3, sticky = 'ew', padx = 1, pady = 1)

	infobulle(minadjdy.v, 'Minimum number of non-missing values to be used to adjust the series (in month)')
	status.bar.display(minadjdy.v, TextOutputVar, 'Minimum number of non-missing values to be used to adjust the series (in month)')
	infobulle(segadjdy.v, 'The segment to which the series is to be adjusted (0:last segment)')
	status.bar.display(segadjdy.v, TextOutputVar, 'The segment to which the series is to be adjusted (0:last segment)')
	infobulle(minadjdk.v, 'Minimum number of non-missing values to be used to adjust the series (in month)')
	status.bar.display(minadjdk.v, TextOutputVar, 'Minimum number of non-missing values to be used to adjust the series (in month)')
	infobulle(segadjdk.v, 'The segment to which the series is to be adjusted (0:last segment)')
	status.bar.display(segadjdk.v, TextOutputVar, 'The segment to which the series is to be adjusted (0:last segment)')
	infobulle(minadjmo.v, 'Minimum number of non-missing values to be used to adjust the series (in month)')
	status.bar.display(minadjmo.v, TextOutputVar, 'Minimum number of non-missing values to be used to adjust the series (in month)')
	infobulle(segadjmo.v, 'The segment to which the series is to be adjusted (0:last segment)')
	status.bar.display(segadjmo.v, TextOutputVar, 'The segment to which the series is to be adjusted (0:last segment)')

	################################

	btgetadj <- tkbutton(fdf2, text = "OK")

	tkconfigure(btgetadj, command = function(){
		GeneralParameters$adjust.pars$minAdjmo <<- as.numeric(str_trim(tclvalue(minadjmo)))
		GeneralParameters$adjust.pars$minAdjdk <<- as.numeric(str_trim(tclvalue(minadjdk)))
		GeneralParameters$adjust.pars$minAdjdy <<- as.numeric(str_trim(tclvalue(minadjdy)))
		GeneralParameters$adjust.pars$SegAdjmo <<- as.numeric(str_trim(tclvalue(segadjmo)))
		GeneralParameters$adjust.pars$SegAdjdk <<- as.numeric(str_trim(tclvalue(segadjdk)))
		GeneralParameters$adjust.pars$SegAdjdy <<- as.numeric(str_trim(tclvalue(segadjdy)))

		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkgrid(btgetadj, ipadx = 10)

	################################

	tkgrid(fdf1, sticky = 'we', padx = 5, pady = 5)
	tkgrid(fdf2, padx = 5, pady = 5)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'Adjustment- Settings')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(top.win)})
	tkwait.window(tt1)
	return(GeneralParameters)
}

############################################################################################

refSeriesUsersChoice <- function(parent, stnId, GeneralParameters){
	w.scr.frm <- 190
	h.scr.frm <- 160

	nl <- length(stnId)
	tocheck <- rep("0", nl)
	id2check <- as.numeric(GeneralParameters$stn.user.choice)
	if(length(id2check) > 0){
		tocheck[id2check] <- "1"
		xstation <- stnId[id2check]
	}

	################################

	tt2 <- tktoplevel()
	tkgrab.set(tt2)
	tkfocus(tt2)

	frcont <- tkframe(tt2, relief = 'raised', borderwidth = 2)
	frok <- tkframe(tt2)

	################################
	frchklist <- tkframe(frcont)
	frtransbt <- tkframe(frcont)
	frlistbx <- tkframe(frcont, width = w.scr.frm, relief = "sunken", borderwidth = 2)
	scr.win <- tkwidget(frchklist, "ScrolledWindow", relief = "sunken", borderwidth = 2)

	scr.frm <- tkwidget(scr.win, "ScrollableFrame", width = w.scr.frm, height = h.scr.frm, background = "white")
	tcl(scr.win, "setwidget", scr.frm)
	sub.fram <- tclvalue(tcl(scr.frm, "getframe"))
	checkList <- vector(mode = 'list', length = 0)
	for(i in 1:nl){
		assign(paste("cbval", i, sep = ""), tclVar(tocheck[i]))
		checkList[[i]] <- tcl("checkbutton", paste(sub.fram, i, sep = "."), text = stnId[i],
								anchor = 'w', justify = 'left', background = "white", width = 20)
		tkconfigure(checkList[[i]], variable = paste("cbval", i, sep = ""))
		tkgrid(checkList[[i]], row = i, column = 0, sticky = 'ew', padx = 2)
		if(tocheck[i] == "0") tkdeselect(checkList[[i]])
		if(tocheck[i] == "1") tkselect(checkList[[i]])
	}

	################################

	selbut <- tkbutton(frtransbt, text = '>>')
	delbut <- tkbutton(frtransbt, text = '<<')

	###############
	selectedSTN <- NULL

	tkconfigure(selbut, command = function(){
		for(i in 1:nl) selectedSTN[i] <- tclvalue(paste("cbval", i, sep = ""))
		xselected <- stnId[which(selectedSTN == '1')]
		dejaselect <- as.character(tkget(choose.stn, "0", "end"))
		if(length(dejaselect) > 0){
			xselect <- xselected[!xselected%in%dejaselect]
			if(length(xselect) > 0) for(j in 1:length(xselect)) tkinsert(choose.stn, "end", xselect[j])
		}else{
			if(length(xselected) > 0) for(j in 1:length(xselected)) tkinsert(choose.stn, "end", xselected[j])
		}
	})

	tkconfigure(delbut, command = function(){
		iselect <- as.character(tkcurselection(choose.stn))
		nsel <- length(iselect)
		if(nsel > 0){
			dejaselect <- as.character(tkget(choose.stn, "0", "end"))
			for(i in which(stnId%in%dejaselect[as.numeric(iselect)+1])) tkdeselect(checkList[[i]])
		}
		while(nsel > 0){
			tkdelete(choose.stn, iselect[nsel])
			nsel <- nsel-1
		}
	})

	###############
	tkgrid(selbut, ipadx = 10)
	tkgrid(delbut, ipadx = 10)

	################################
	scr.lstbx <- tkscrollbar(frlistbx, repeatinterval = 5, 
							command = function(...) tkyview(choose.stn, ...))
	choose.stn <- tklistbox(frlistbx, selectbackground = "yellow", selectforeground = "blue",
							selectmode = "multiple", background = "white", width = 20, height = 10,
							yscrollcommand = function(...) tkset(scr.lstbx, ...))
	tkgrid(choose.stn, scr.lstbx)
	tkgrid.configure(scr.lstbx, sticky = "nswe")
	if(length(id2check) > 0) for(i in 1:length(xstation))  tkinsert(choose.stn, "end", xstation[i])

	################################

	tkgrid(scr.win)
	tkgrid(frchklist, frtransbt, frlistbx)

	################################
	okbut <- tkbutton(frok, text = 'OK')
	cabut <- tkbutton(frok, text = 'Cancel')

	tkconfigure(okbut, command = function(){
		stnSelected <- as.character(tkget(choose.stn, "0", "end"))
		idStnSel <- which(stnId%in%stnSelected)
		GeneralParameters$stn.user.choice <<- idStnSel

		tkgrab.release(tt2)
		tkdestroy(tt2)
		tkfocus(parent)
	})

	tkconfigure(cabut, command = function(){
		tkgrab.release(tt2)
		tkdestroy(tt2)
		tkfocus(parent)
	})

	tkgrid(okbut, row = 0, column = 0, padx = 5, pady = 1, ipadx = 10, sticky = 'w')
	tkgrid(cabut, row = 0, column = 1, padx = 5, pady = 1, sticky = 'e')

	###############################################################

	tkgrid(frcont, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frok, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt2)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt2))
	tt.h <- as.integer(tkwinfo("reqheight", tt2))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt2, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt2)
	tkwm.title(tt2, 'Reference series user choice')
	tkwm.deiconify(tt2)

	tkfocus(tt2)
	tkbind(tt2, "<Destroy>", function() {tkgrab.release(tt2); tkfocus(parent)})
	tkwait.window(tt2)
	return(GeneralParameters)
}


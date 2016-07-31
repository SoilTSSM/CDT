homogen.get.info <- function(parent.win, GeneralParameters){

	listOpenFiles <- openFile_ttkcomboList()

	#tkentry width, file path
	if (Sys.info()["sysname"] == "Windows") largeur <- 28
	else largeur <- 26
	##tkcombo width, homg method choice
	if (Sys.info()["sysname"] == "Windows") largeur1 <- 23
	else largeur1 <- 21
	## tkcombo width, listOpenFiles
	wtkcombo <- 25
	################
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frButt <- tkframe(tt)

	fr.A <- tkframe(frDialog, relief = "groove", borderwidth = 2)
	fr.B <- tkframe(frDialog, relief = "groove", borderwidth = 2)
	tkgrid(fr.A, fr.B)
	tkgrid.configure(fr.A, row = 0, column = 0, sticky = 'news', padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid.configure(fr.B, row = 0, column = 1, sticky = 'news', padx = 5, pady = 5, ipadx = 1, ipady = 1)

	for(i in 0:1) assign(paste('fr.A', i, sep = ''), tkframe(fr.A, relief = 'sunken', borderwidth = 2))
	for(i in 0:1) tkgrid(get(paste('fr.A', i, sep = '')))
	for(i in 0:1) tkgrid.configure(get(paste('fr.A', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	##################################

	if(as.character(GeneralParameters$use.method$Values[2]) == '1' & as.character(GeneralParameters$use.method$Values[3]) == '1'){
		homo.state <- 'normal'
	}else{
		homo.state <- 'disabled'
	}
	#####################
	for(i in 0:4) assign(paste('fr.A0', i, sep = ''), tkframe(fr.A0))
	for(i in 0:4) tkgrid(get(paste('fr.A0', i, sep = '')))
	for(i in 0:4) tkgrid.configure(get(paste('fr.A0', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	file.period <- tclVar()
	tclvalue(file.period) <- ifelse(as.character(GeneralParameters$period) == 'daily', 'Daily data', ifelse(as.character(GeneralParameters$period) == 'dekadal', 'Dekadal data', 'Monthly data'))

	cb.period <- ttkcombobox(fr.A00, values = c('Daily data', 'Dekadal data', 'Monthly data'), textvariable = file.period, width = wtkcombo)
	infobulle(cb.period, 'Choose the time step of the data')
	status.bar.display(cb.period, TextOutputVar, 'Choose the time step of the data')
	tkgrid(cb.period)

	#########
	homo.file.stn1 <- tklabel(fr.A01, text = 'Data containing the canditate series')
	tkgrid(homo.file.stn1)

	file.choix1a <- tclVar()
	tclvalue(file.choix1a) <- as.character(GeneralParameters$file.io$Values[1])
	file.choix1b <- tclVar()
	tclvalue(file.choix1b) <- as.character(GeneralParameters$file.io$Values[2])

	cb.file.stn1 <- ttkcombobox(fr.A02, values = unlist(listOpenFiles), textvariable = file.choix1a, width = wtkcombo)
	infobulle(cb.file.stn1, 'Choose the file in the list')
	status.bar.display(cb.file.stn1, TextOutputVar, 'Choose the file containing the candidate series')

	bt.file.stn1 <- tkbutton.h(fr.A02, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
	tkgrid(cb.file.stn1, bt.file.stn1)
	tkgrid.configure(cb.file.stn1, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.file.stn1, row = 0, column = 1, sticky = 'e')

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

	homo.file.stn2 <- tklabel(fr.A03, text = 'Reference series')
	tkgrid(homo.file.stn2)

	cb.file.stn2 <- ttkcombobox(fr.A04, values = unlist(listOpenFiles), textvariable = file.choix1b, width = wtkcombo)
	infobulle(cb.file.stn2, 'Choose the reference series in the list')
	status.bar.display(cb.file.stn2, TextOutputVar, 'Choose the file containing the reference series')

	bt.file.stn2 <- tkbutton.h(fr.A04, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
	tkgrid(cb.file.stn2, bt.file.stn2)

	tkgrid.configure(cb.file.stn2, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.file.stn2, row = 0, column = 1, sticky = 'e')
	tkconfigure(cb.file.stn2, state = homo.state)
	tkconfigure(bt.file.stn2, state = homo.state)

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
		}else{
			return(NULL)
		}
	})

	##########
	fr.A11 <- tkframe(fr.A1)
	fr.A12 <- tkframe(fr.A1)
	tkgrid(fr.A11, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.A12, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	frA11.txt1 <- tklabel(fr.A11, text = 'Directory to save result')
	tkgrid(frA11.txt1)

	file.save1 <- tclVar(as.character(GeneralParameters$file.io$Values[4]))
	en.file.save <- tkentry(fr.A12, textvariable = file.save1, width = largeur)
	infobulle(en.file.save, 'Enter the full path of the\ndirectory to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path of the directory to save result')
	bt.file.save <- tkbutton.h(fr.A12, text = "...", TextOutputVar, 'or browse here','')
	tkgrid(en.file.save, bt.file.save)
	tkgrid.configure(en.file.save, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.file.save, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[4]), "")
		if(!file.exists(file2save1)){
			tkmessageBox(message = paste(file2save1, 'does not exist.\n It will be created.',sep = ' '), icon = "warning", type = "ok")
			dir.create(file2save1, recursive = TRUE)
			tclvalue(file.save1) <- file2save1
		}else tclvalue(file.save1) <- file2save1
	})
	#################################################

	for(i in 0:4) assign(paste('fr.B', i, sep = ''), tkframe(fr.B, relief = 'sunken', borderwidth = 2))
	for(i in 0:4) tkgrid(get(paste('fr.B', i, sep = '')))
	for(i in 0:4) tkgrid.configure(get(paste('fr.B', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)


	frB0.txt1 <- tklabel(fr.B0, text = 'Detection statistic', anchor = 'w', justify = 'left')

	homog.mthd <- c("Pettitt Test", "SNHT(Alexandersson & Moberg, 1997)","CUSUM-type (Gallagher et al.,2013)", "CUSUM-type with Trend(Gallagher et al.,2013)")
	hmg.mthd <- tclVar(as.character(GeneralParameters$use.method$Values[1]))
	cb.homog.mthd <- ttkcombobox(fr.B0, values = homog.mthd, textvariable = hmg.mthd, width = largeur1)
	bt.homog.mthd <- tkbutton.h(fr.B0, text = "Settings", TextOutputVar, 'Set options for homogenization procedures', 'Set options for homogenization procedures')


	tkgrid(frB0.txt1, sticky = 'w')
	tkgrid(cb.homog.mthd, bt.homog.mthd)

	Clev.fun <- function(cb.bt){
		if(tclvalue(tkget(cb.bt)) == "SNHT") Clev.list <- c('90.0', '92.0', '94.0', '95.0', '97.5', '99.0')
		else Clev.list <- c('90.0', '92.0', '95.0', '97.5', '99.0', '99.9')
		return(Clev.list)
	}

	tkconfigure(bt.homog.mthd, command = function(){
		GeneralParameters <<- homogenization.opts(tt, GeneralParameters, Clev.fun(cb.homog.mthd))
	})
	#######################################
	if(as.character(GeneralParameters$use.method$Values[2]) == '0') state1ser <- 'disabled'
	if(as.character(GeneralParameters$use.method$Values[2]) == '1') state1ser <- 'normal'

	cb.1series.val <- tclVar(as.character(GeneralParameters$use.method$Values[2]))
	cb.1series <- tkcheckbutton(fr.B1, variable = cb.1series.val, text = 'Single Series', anchor = 'w', justify = 'left', width = largeur1-1)
	infobulle(cb.1series, 'Homogenization for one station series')
	status.bar.display(cb.1series, TextOutputVar, 'The data is a series of one station')
	bt.1series <- tkbutton.h(fr.B1, text = "Settings", TextOutputVar, 'Set options for the file and date format', 'Set options for the file and date format')
	tkgrid(cb.1series, bt.1series)
	tkconfigure(bt.1series, state = state1ser, command = function(){
		GeneralParameters <<- filedateformat(tt, GeneralParameters, tclvalue(file.period))
	})

	#######################################
	if(as.character(GeneralParameters$use.method$Values[3]) == '0') staterf <- 'disabled'
	if(as.character(GeneralParameters$use.method$Values[3]) == '1') staterf <- 'normal'

	cb.rfseries.val <- tclVar(as.character(GeneralParameters$use.method$Values[3]))
	cb.rfseries <- tkcheckbutton(fr.B2, variable = cb.rfseries.val, text = 'Use reference series', anchor = 'w', justify = 'left', width = largeur1-1)
	infobulle(cb.rfseries, 'Using a reference series to\nperform the homogenization test')
	status.bar.display(cb.rfseries, TextOutputVar, 'Using a reference series to perform the homogenization test')
	bt.rfseries <- tkbutton.h(fr.B2, text = "Settings", TextOutputVar, 'Set options to create the reference series', 'Set options to create the reference series')
	tkgrid(cb.rfseries, bt.rfseries)
	tkconfigure(bt.rfseries, state = staterf, command = function(){
		GeneralParameters <<- referenceseries(tt, GeneralParameters, tclvalue(file.choix1a))
	})

	#######################################
	tkbind(cb.rfseries,"<Button-1>", function(){
	if(tclvalue(cb.rfseries.val) == "0" & tclvalue(cb.1series.val) == "1"){
			homo.state <- 'normal'
			tkconfigure(cb.file.stn2, state = homo.state)
			tkconfigure(bt.file.stn2, state = homo.state)
		}else if(tclvalue(cb.1series.val) == "1"){
			homo.state <- 'disabled'
			tkconfigure(cb.file.stn2, state = homo.state)
			tkconfigure(bt.file.stn2, state = homo.state)
		}else{
			homo.state <- 'disabled'
			tkconfigure(cb.file.stn2, state = homo.state)
			tkconfigure(bt.file.stn2, state = homo.state)
		}
		###
		if(tclvalue(cb.rfseries.val) == "0"){
			staterf <- 'normal'
			tkconfigure(bt.rfseries, state = staterf)
		}else{
			staterf <- 'disabled'
			tkconfigure(bt.rfseries, state = staterf)
		}
	})

	tkbind(cb.1series,"<Button-1>", function(){
		if(tclvalue(cb.1series.val) == "0" & tclvalue(cb.rfseries.val) == "1"){
			homo.state <- 'normal'
			tkconfigure(cb.file.stn2, state = homo.state)
			tkconfigure(bt.file.stn2, state = homo.state)
		}else{
			homo.state <- 'disabled'
			tkconfigure(cb.file.stn2, state = homo.state)
			tkconfigure(bt.file.stn2, state = homo.state)
		}
		###
		if(tclvalue(cb.1series.val) == "0"){
			state1ser <- 'normal'
			tkconfigure(bt.1series, state = state1ser)
		}else{
			state1ser <- 'disabled'
			tkconfigure(bt.1series, state = state1ser)
		}
	})
	#################################
	bt.adjust <- tkbutton.h(fr.B3, text = "Adjust-Settings", TextOutputVar, 'Set options to adjust the series',
	'Set options to adjust the series')
	tkgrid(bt.adjust, sticky = 'we', padx = 25, pady = 1, ipadx = 1, ipady = 1)
	tkconfigure(bt.adjust, command = function(){
		GeneralParameters <<- getAdjustparams(tt, GeneralParameters, tclvalue(file.period))
	})
	###################################
	computefun.l <- tklabel.h(fr.B4, text = 'Aggregation', TextOutputVar, 'Function to be used to compute\ndekadal and monthly series', 'Function to be used to compute dekadal and monthly series')
	computefun <- c("mean", "sum")
	cmptfun <- tclVar(as.character(GeneralParameters$compute.var$Values[1]))
	cb.cmptfun <- ttkcombobox(fr.B4, values = computefun, textvariable = cmptfun, width = 8)
	infobulle(cb.cmptfun, 'Function to be used to compute\ndekadal and monthly series')
	status.bar.display(cb.cmptfun, TextOutputVar, 'Function to be used to compute dekadal and monthly series')

	missfrac.l <- tklabel.h(fr.B4, text='   Min.frac', TextOutputVar,
	'Minimum fraction of available data\nthat must be present for the time period\nto compute',
	'Minimum fraction of available data that must be present for the time period to compute')
	missfrac.v <- tkentry.h(fr.B4, TextOutputVar,
	'Minimum fraction of available data\nthat must be present for the time period\nto compute',
	'Minimum fraction of available data that must be present for the time period to compute')
	miss.frac <- tclVar(as.character(GeneralParameters$compute.var$Values[2]))
	tkconfigure(missfrac.v, width = 6, textvariable = miss.frac)

	tkgrid(computefun.l, cb.cmptfun, missfrac.l, missfrac.v)

	#################################
	bt.opt.OK <- tkbutton(frButt, text = "OK")
	bt.opt.CA <- tkbutton(frButt, text = "Cancel")
	tkgrid(bt.opt.OK, row = 0, column = 0, padx = 5, pady = 5, ipadx = 5, sticky = 'w')
	tkgrid(bt.opt.CA, row = 0, column = 1, padx = 5, pady = 5, sticky = 'e')

	tkconfigure(bt.opt.OK, command = function(){
		if(tclvalue(file.choix1a) == ""){
			tkmessageBox(message = "Provide the file to test", icon = "warning", type = "ok")
		}else if(tclvalue(cb.1series.val) == '1' & tclvalue(cb.rfseries.val) == '1' & tclvalue(file.choix1b) == ""){
			tkmessageBox(message = "Provide the file containing\nthe reference series", icon = "warning", type = "ok")
		}else{

			all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))
			jfile <- which(all.open.file == tclvalue(file.choix1a))
			if(length(jfile) == 0){
				tkmessageBox(message = "Station data not found or in the wrong format", icon = "warning", type = "ok")
				tkwait.window(tt)
			}

			if(tclvalue(cb.1series.val) == "1" & tclvalue(cb.rfseries.val) == "1"){
				jfile1 <- which(all.open.file == tclvalue(file.choix1b))
				if(length(jfile1) == 0){
					tkmessageBox(message = "Data to be used for reference series not found or in the wrong format", icon = "warning", type = "ok")
					tkwait.window(tt)
				}
			}

			##
			dirHomog <- file.path(tclvalue(file.save1), paste('HomogTest', getf.no.ext(tclvalue(file.choix1a)), sep = '_'), fsep = .Platform$file.sep)
			dirparams <- file.path(dirHomog, 'OriginalData', fsep = .Platform$file.sep)
			if(!file.exists(dirparams)) dir.create(dirparams, showWarnings = FALSE, recursive = TRUE)
			fileparams <- file.path(dirparams, 'Parameters.RData', fsep = .Platform$file.sep)
			assign('baseDir', dirHomog, envir = EnvHomogzData)

			GeneralParameters$period <<- ifelse(tclvalue(file.period) == 'Daily data', 'daily', ifelse(tclvalue(file.period) == 'Dekadal data', 'dekadal', 'monthly'))
			GeneralParameters$file.io$Values <<- c(tclvalue(file.choix1a), tclvalue(file.choix1b), as.character(GeneralParameters$file.io$Values[3]), tclvalue(file.save1))
			GeneralParameters$use.method$Values <<- c(tclvalue(hmg.mthd), tclvalue(cb.1series.val), tclvalue(cb.rfseries.val))
			GeneralParameters$compute.var$Values <<- c(tclvalue(cmptfun), tclvalue(miss.frac))

			######
			getInitDataParams <- function(GeneralParameters){
				if(tclvalue(cb.1series.val) == "0"){
					donstn <- splitCDTData(AllOpenFilesData[[jfile]][[2]], GeneralParameters$period)
					donOut <- list(donstn)
					parsFile <- list(AllOpenFilesData[[jfile]][3:4])
					if(!is.null(donstn)) stn.choix <- as.character(donstn$id)
					else tkwait.window(tt)
				}else{
					donstn <- splitTsData(AllOpenFilesData[[jfile]][[2]], GeneralParameters$period, as.character(GeneralParameters$file.date.format$Values[1]), as.character(GeneralParameters$file.date.format$Values[2]))
					donOut <- list(donstn)
					parsFile <- list(AllOpenFilesData[[jfile]][3:4])
					if(tclvalue(cb.rfseries.val) == "1"){
						donstn1 <- splitTsData(AllOpenFilesData[[jfile1]][[2]], GeneralParameters$period, as.character(GeneralParameters$file.date.format$Values[1]), as.character(GeneralParameters$file.date.format$Values[2]))
						donOut <- list(donstn, donstn1)
						parsFile <- list(AllOpenFilesData[[jfile]][3:4], AllOpenFilesData[[jfile1]][3:4])
					}
					if(!is.null(donstn)) stn.choix <- getf.no.ext(tclvalue(file.choix1a))
					else tkwait.window(tt)
				}
				paramsGAL <- list(inputPars = GeneralParameters, dataPars = parsFile, data = donOut)
				save(paramsGAL, file = fileparams)
				return(list(paramsGAL, stn.choix))
			}

			#####
			getRefSrData <- function(){
				donstn1 <- splitTsData(AllOpenFilesData[[jfile1]][[2]], GeneralParameters$period, as.character(GeneralParameters$file.date.format$Values[1]), as.character(GeneralParameters$file.date.format$Values[2]))
				if(is.null(donstn1)){
					tkmessageBox(message = "Data to be used for reference series not found or in the wrong format", icon = "warning", type = "ok")
					tkwait.window(tt)
				}
				load(fileparams)
				paramsGAL$data[[2]] <- donstn1
				paramsGAL$dataPars[[2]] <- AllOpenFilesData[[jfile1]][3:4]
				save(paramsGAL, file = fileparams)
				return(paramsGAL)
			}

			######
			xtest <- tclvalue(cb.1series.val) == "1" & tclvalue(cb.rfseries.val) == "1"

			if(file.exists(fileparams)){
				load(fileparams)

				intest1 <- paramsGAL$inputPars$period == GeneralParameters$period & all(AllOpenFilesData[[jfile]][3:4]%in%paramsGAL$dataPars[[1]])
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
							ctest1 <- all(AllOpenFilesData[[jfile1]][3:4]%in%paramsGAL$dataPars[[2]])
							if(!ctest1){
								paramsGAL <- getRefSrData(GeneralParameters)
								assign('donnees2', paramsGAL$data[[2]], envir = EnvHomogzData)
								##recalculate
								computeHomogData(GeneralParameters)
							}
						}
					}
					if(tclvalue(cb.1series.val) == "0") stn.choix <<- as.character(paramsGAL$data[[1]]$id)
					else stn.choix <<- getf.no.ext(tclvalue(file.choix1a))
					paramsGAL$inputPars <- GeneralParameters
					save(paramsGAL, file = fileparams)
					rm(paramsGAL)
				}else{
					retDonPar <- getInitDataParams(GeneralParameters)

					assign('donnees1', retDonPar[[1]]$data[[1]], envir = EnvHomogzData)
					if(xtest) assign('donnees2', retDonPar[[1]]$data[[2]], envir = EnvHomogzData)
					GeneralParameters <<- retDonPar[[1]]$inputPars
					stn.choix <<- retDonPar[[2]]
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
				stn.choix <<- retDonPar[[2]]
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
				if(stn.choix[1] != '') tclvalue(stn.choix.val) <- stn.choix[1]
				else tclvalue(stn.choix.val) <- stn.choix[2]
			}else{
				istn <- as.numeric(tclvalue(tcl(stn.choix.cb, "current")))+1
				if(istn > 0) istn <- istn
				else istn <- 1
				tclvalue(stn.choix.val) <- stn.choix[istn]
			}

			tkconfigure(stn.choix.cb, values = stn.choix, textvariable = stn.choix.val)
			tkconfigure(setting.button, state = 'normal')
			if(tclvalue(cb.1series.val) == '0'){
				tkconfigure(stn.choix.prev, state = 'normal')
				tkconfigure(stn.choix.next, state = 'normal')
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

	tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frButt, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###############################################################
	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt, paste('+',tt.x,'+',tt.y, sep = ''))
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
	tkgrid(fdf1)
	tkgrid(fdf2)
	fr.fileformat1 <- ttklabelframe(fdf1, text = "File Format", labelanchor = "nw", relief = "groove", borderwidth = 2)
	fr.fileformat2 <- ttklabelframe(fdf1, text = "Dates Format", labelanchor = "nw", relief = "groove", borderwidth = 2)
	tkgrid(fr.fileformat1, fr.fileformat2)
	bt.fileformat <- tkbutton(fdf2, text=" OK ")
	tkgrid(bt.fileformat, ipadx = 5)

	ffrmt1 <- tkradiobutton(fr.fileformat1, text = "One variable", anchor = 'w', justify = 'left')
	infobulle(ffrmt1, 'In case of single serie:\nThe file contains 1 variable')
	status.bar.display(ffrmt1, TextOutputVar, 'In case of single serie: The file contains 1 variable')
	ffrmt2 <- tkradiobutton(fr.fileformat1, text = "Rain Tmax Tmin", anchor = 'w', justify = 'left')
	infobulle(ffrmt2, 'In case of single serie:\nThe file contains Rain, Tmax\nand Tmin in this order')
	status.bar.display(ffrmt2, TextOutputVar, 'In case of single serie:The file contains Rain, Tmax and Tmin in this order')
	tkgrid(ffrmt1, row = 0, column = 0, sticky = "we")
	tkgrid(ffrmt2, row = 1, column = 0, sticky = "we")
	rbffrmt <- tclVar(as.character(GeneralParameters$file.date.format$Values[1]))
	tkconfigure(ffrmt1, variable = rbffrmt, value = "1")
	tkconfigure(ffrmt2, variable = rbffrmt, value = "0")

	varframe <- tkframe(fr.fileformat1)
	tkgrid(varframe, row = 3, column = 0, sticky = "e")
	infobulle(varframe, 'Choose the variable to test')
	status.bar.display(varframe, TextOutputVar, 'Choose the variable to test')

	var2test1 <- tkradiobutton(varframe)
	var2test2 <- tkradiobutton(varframe)
	var2test3 <- tkradiobutton(varframe)

	tkgrid(var2test1, row = 0, column = 0, padx = 2)
	tkgrid(var2test2, row = 0, column = 1, padx = 2)
	tkgrid(var2test3, row = 0, column = 2, padx = 2)
	varcat <- tclVar(as.character(GeneralParameters$file.date.format$Values[3]))
	tkconfigure(var2test1, variable = varcat, value = "1")
	tkconfigure(var2test2, variable = varcat, value = "2")
	tkconfigure(var2test3, variable = varcat, value = "3")

	if(speriod == 'Daily data') {txtdtfrmt1 <- "YYYYMMDD";txtdtfrmt2 <- "YYYY MM DD"}
	if(speriod == 'Dekadal data') {txtdtfrmt1 <- "YYYYMMD";txtdtfrmt2 <- "YYYY MM D"}
	if(speriod == 'Monthly data') {txtdtfrmt1 <- "YYYYMM";txtdtfrmt2 <- "YYYY MM"}

	dtfrmt1 <- tkradiobutton(fr.fileformat2, text = txtdtfrmt1, anchor = 'w', justify = 'left')
	infobulle(dtfrmt1, 'In case of single serie:\n dates are merged')
	status.bar.display(dtfrmt1, TextOutputVar, 'In case of single serie: dates are merged')
	dtfrmt2 <- tkradiobutton(fr.fileformat2, text = txtdtfrmt2, anchor = 'w', justify = 'left')
	infobulle(dtfrmt2, 'In case of single serie:\ndates are separated by space,\ntabulation or CSV format')
	status.bar.display(dtfrmt2, TextOutputVar, 'In case of single serie: dates are separated by space, tabulation or CSV format')
	tkgrid(dtfrmt1, row = 0, column = 0, sticky = "we")
	tkgrid(dtfrmt2, row = 1, column = 0, sticky = "we")
	tkgrid(tklabel(fr.fileformat2, text=''), row = 3, column = 0, sticky = "we")

	rbdtfrmt <- tclVar(as.character(GeneralParameters$file.date.format$Values[2]))
	tkconfigure(dtfrmt1, variable = rbdtfrmt, value = "1")
	tkconfigure(dtfrmt2, variable = rbdtfrmt, value = "0")

	tkconfigure(bt.fileformat, command = function(){
		GeneralParameters$file.date.format$Values <<- c(tclvalue(rbffrmt), tclvalue(rbdtfrmt), tclvalue(varcat))
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+',tt.x,'+',tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'Series Format')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function() {tkgrab.release(tt1); tkfocus(top.win)})
	tkwait.window(tt1)
	return(GeneralParameters)
}

############################################################################################

referenceseries <- function(top.win, GeneralParameters, file2test){
	listOpenFiles <- openFile_ttkcomboList()
	if (Sys.info()["sysname"] == "Windows") largeur2 <- 26
	else largeur2 <- 24

	############
	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frcont <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frok <- tkframe(tt1)

	frE1 <- tkframe(frcont)
	frE2 <- tkframe(frcont)
	tkgrid(frE1, row = 0, column = 0, sticky = 'ns', padx = 5, pady = 5)
	tkgrid(frE2, row = 0, column = 1, sticky = 'ns', padx = 5, pady = 5)

	frE11 <- tkframe(frE1, relief = 'sunken', borderwidth = 2)
	frE12 <- tkframe(frE1, relief = 'sunken', borderwidth = 2)
	tkgrid(frE11, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1)
	tkgrid(frE12, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1)

	frE20 <- tkframe(frE2, relief = 'sunken', borderwidth = 2)
	frE21 <- tkframe(frE2, relief = 'sunken', borderwidth = 2)
	frE22 <- tkframe(frE2, relief = 'sunken', borderwidth = 2)
	tkgrid(frE20, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1)
	tkgrid(frE21, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1)
	tkgrid(frE22, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1)

	frE11a <- ttklabelframe(frE11, text = "Test series constitution", labelanchor = "nw", relief = "groove", borderwidth = 2)
	frE11b <- ttklabelframe(frE11, text = "Weighting factors", labelanchor = "nw", relief = "groove", borderwidth = 2)
	tkgrid(frE11a, row = 0, column = 0, sticky = 'w', padx = 1, pady = 1)
	tkgrid(frE11b, row = 0, column = 1, sticky = 'e', padx = 1, pady = 1)

	dif.rat1 <- tkradiobutton(frE11a, text = "Difference", anchor = 'w', justify = 'left')
	infobulle(dif.rat1, 'Constitution of relative comparison series:\nCandidate-Reference')
	status.bar.display(dif.rat1, TextOutputVar, 'Constitution of relative comparison series: Candidate-Reference')
	dif.rat2 <- tkradiobutton(frE11a, text = "Ratio", anchor = 'w', justify = 'left')
	infobulle(dif.rat2, 'Constitution of relative comparison series:\nCandidate/Reference')
	status.bar.display(dif.rat2, TextOutputVar, 'Constitution of relative comparison series: Candidate/Reference')
	dif.rat3 <- tkradiobutton(frE11a, text = "LogRatio", anchor = 'w', justify = 'left')
	infobulle(dif.rat3, 'Constitution of relative comparison series:\nlog(Candidate/Reference)')
	status.bar.display(dif.rat3, TextOutputVar, 'Constitution of relative comparison series: log(Candidate/Reference)')
	tkgrid(dif.rat1, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(dif.rat2, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(dif.rat3, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)
	diff.ratio <- tclVar(as.character(GeneralParameters$ref.series.choix$Values[1]))
	tkconfigure(dif.rat1, variable = diff.ratio, value = "1")
	tkconfigure(dif.rat2, variable = diff.ratio, value = "2")
	tkconfigure(dif.rat3, variable = diff.ratio, value = "3")

	wmean1 <- tkradiobutton(frE11b, text = "Correlation", anchor = 'w', justify = 'left')
	infobulle(wmean1, 'Use the square of the correlation coef\nas the weight factor')
	status.bar.display(wmean1, TextOutputVar, 'Use the square of the correlation coef as the weight factor')
	wmean2 <- tkradiobutton(frE11b, text = "Distance", anchor = 'w', justify = 'left')
	infobulle(wmean2, 'Use the square of the inverse of distance\nas the weight factor')
	status.bar.display(wmean2, TextOutputVar, 'Use the square of the inverse of distance as the weight factor')
	wmean3 <- tkradiobutton(frE11b, text = "Optimal", anchor = 'w', justify = 'left')
	infobulle(wmean3, 'Optimal weighting using covariance matrix\n(ordinary kriging method)')
	status.bar.display(wmean3, TextOutputVar, 'Optimal weighting using covariance matrix (ordinary kriging method)')

	tkgrid(wmean1, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(wmean2, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(wmean3, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)
	weight.fac <- tclVar(as.character(GeneralParameters$ref.series.choix$Values[2]))
	tkconfigure(wmean1, variable = weight.fac, value = "1")
	tkconfigure(wmean2, variable = weight.fac, value = "2")
	tkconfigure(wmean3, variable = weight.fac, value = "3")

	frE12a <- ttklabelframe(frE12, text = "Options", labelanchor = "nw", relief = "groove", borderwidth = 2)
	tkgrid(frE12a, sticky = 'ew')

	min.stn.l <- tklabel.h(frE12a, text = 'Min.stn', TextOutputVar, 'Minimum number of\nneighbor stations to use',
	'Minimum number of neighbor stations to use')
	max.stn.l <- tklabel.h(frE12a, text = 'Max.stn', TextOutputVar, 'Maximum number of\nneighbor stations to use',
	'Maximum number of neighbor stations to use')
	max.dist.l <- tklabel.h(frE12a, text = 'Max.dist(km)',TextOutputVar, 'Maximum distance of \nneighbor stations to use (km)',
	'Maximum distance of neighbor stations to use (km)')
	elv.diff.l <- tklabel.h(frE12a, text = 'Elv.diff(m)',TextOutputVar, 'Maximum altitude difference of \n neighbor stations to use (m)',
	'Maximum altitude difference of neighbor stations to use (m)')
	min.rho.l <- tklabel.h(frE12a, text = 'Min.rho', TextOutputVar, 'Minimum correlation coef between candidate\nand neighbor series',
	'Minimum correlation coef between candidate and neighbor series')

	min.stn.v <- tkentry.h(frE12a, TextOutputVar, 'Minimum number of \nneighbor stations to use', 'Minimum number of neighbor stations to use')
	max.stn.v <- tkentry.h(frE12a, TextOutputVar, 'Maximum number of\nneighbor stations to use', 'Maximum number of neighbor stations to use')
	max.dist.v <- tkentry.h(frE12a, TextOutputVar, 'Maximum distance of\nneighbor stations to use (km)',
	'Maximum distance of neighbor stations to use (km)')
	elv.diff.v <- tkentry.h(frE12a, TextOutputVar, 'Maximum altitude difference of \nneighbor stations to use (m)',
	'Maximum altitude difference of neighbor stations to use (m)')
	min.rho.v <- tkentry.h(frE12a, TextOutputVar, 'Minimum correlation coef between candidate\nand neighbor series',
	'Minimum correlation coef between candidate and neighbor series')

	tkgrid(min.stn.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.stn.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.stn.l, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.stn.v, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dist.l, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dist.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(elv.diff.l, row = 1, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(elv.diff.v, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.rho.l, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.rho.v, row = 2, column = 1, sticky = 'ew', padx = 1, pady = 1)

	tkconfigure(min.stn.l, anchor = 'e', justify = 'right')
	tkconfigure(max.stn.l, anchor = 'e', justify = 'right')
	tkconfigure(max.dist.l, anchor = 'e', justify = 'right')
	tkconfigure(elv.diff.l, anchor = 'e', justify = 'right')
	tkconfigure(min.rho.l, anchor = 'e', justify = 'right')

	min.stn <- tclVar(as.character(GeneralParameters$ref.series.choix$Values[5]))
	max.stn <- tclVar(as.character(GeneralParameters$ref.series.choix$Values[6]))
	max.dist <- tclVar(as.character(GeneralParameters$ref.series.choix$Values[7]))
	elv.diff <- tclVar(as.character(GeneralParameters$ref.series.choix$Values[8]))
	min.rho <- tclVar(as.character(GeneralParameters$ref.series.choix$Values[9]))

	tkconfigure(min.stn.v, width = 4, textvariable = min.stn)
	tkconfigure(max.stn.v, width = 4, textvariable = max.stn)
	tkconfigure(max.dist.v, width = 4, textvariable = max.dist)
	tkconfigure(elv.diff.v, width = 4, textvariable = elv.diff)
	tkconfigure(min.rho.v, width = 4, textvariable = min.rho)
	########################################

	if(as.character(GeneralParameters$ref.series.user) == '0') state0 <- 'disabled'
	if(as.character(GeneralParameters$ref.series.user) == '1') state0 <- 'normal'

	usr.rfseries.val <- tclVar(as.character(GeneralParameters$ref.series.user))
	usr.rfseries <- tkcheckbutton(frE20, variable = usr.rfseries.val, text = "Stations'Choice by User", anchor = 'w', justify = 'left', width = 19)
	infobulle(usr.rfseries, 'The reference series will be created\nfrom stations chosen by user')
	status.bar.display(usr.rfseries, TextOutputVar, "The reference series will be created from stations chosen by user")
	usrbt.rfseries <- tkbutton.h(frE20, text = "Select", TextOutputVar, 'Select the stations to create the reference series',
	'Select the stations to create the reference series')
	tkgrid(usr.rfseries, usrbt.rfseries)

	tkconfigure(usrbt.rfseries, state = state0, command = function(){
		if(file2test != ""){
			idsstn <- which(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])) == file2test)
			if(length(idsstn) == 0){
				tkmessageBox(message = "File not found or in the wrong format", icon = "warning", type = "ok")
				#tkwait.window(tt1)
				return(NULL)
			}else{
				donstn <- AllOpenFilesData[[idsstn]][[2]]
				stnId <- as.character(donstn[1,-1])
				GeneralParameters <<- refSeriesUsersChoice(tt1, stnId, GeneralParameters)
			}
		}else{
			tkmessageBox(message = "Provide the file to test", icon = "warning", type = "ok")
			tkgrab.release(tt1)
			tkdestroy(tt1)
			tkfocus(top.win)
		}
	})

	tkbind(usr.rfseries,"<Button-1>", function(){
		if(tclvalue(usr.rfseries.val) == "0"){
			state0 <- 'normal'
			tkconfigure(usrbt.rfseries, state = state0)
		}else{
			state0 <- 'disabled'
			tkconfigure(usrbt.rfseries, state = state0)
		}
	})

	###############
	if(as.character(GeneralParameters$ref.series.choix$Values[3]) == '1'){
		if(as.character(GeneralParameters$ref.series.choix$Values[4]) == '0'){
			state <- c('normal', 'normal', 'normal')
		}else{
			state <- c('normal', 'normal', 'disabled')
		}
	}else{
		state <- c('disabled', 'disabled', 'disabled')
	}
	###
	uselv.val <- tclVar(as.character(GeneralParameters$ref.series.choix$Values[3]))
	cb.uselv <- tkcheckbutton(frE21, variable = uselv.val, text = 'Use Elevation', anchor = 'w', justify = 'left')
	infobulle(cb.uselv, 'Check to use elevation data\n to choose neighbors stations')
	status.bar.display(cb.uselv, TextOutputVar, 'Check for using elevation data to choose neighbor stations')
	uselv.dem <- tkradiobutton(frE21, text = "Elevation from DEM", anchor = 'w', justify = 'left', state = state[1])
	infobulle(uselv.dem, 'Choose to extract\nelevation data from DEM')
	status.bar.display(uselv.dem, TextOutputVar, 'If no elevation data are provided for each station, must be choosen')
	uselv.dat <- tkradiobutton(frE21, text = "Elevation from STN data", anchor = 'w', justify = 'left', state = state[2])
	infobulle(uselv.dat, 'Choose to use elevation data\nfrom the data to be homogenized')
	status.bar.display(uselv.dat, TextOutputVar, 'Check to use elevation data from the data to be homogenized')
	uselv.ch <- tclVar(as.character(GeneralParameters$ref.series.choix$Values[4]))
	tkconfigure(uselv.dem, variable = uselv.ch, value = "0")
	tkconfigure(uselv.dat, variable = uselv.ch, value = "1")
	tkgrid(cb.uselv, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(uselv.dem, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(uselv.dat, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)

	frE22a <- tkframe(frE22)
	frE22b <- tkframe(frE22)
	tkgrid(frE22a, row = 0, column = 0, sticky = 'w')
	tkgrid(frE22b, row = 1, column = 0, sticky = 'w')
	frE22a.txt <- tklabel(frE22a, text = 'Elevation Data (NetCDF)',anchor = 'w', justify = 'left')
	tkgrid(frE22a.txt, row = 0, column = 0, sticky = 'w')

	file.choix2 <- tclVar()
	tclvalue(file.choix2) <- as.character(GeneralParameters$file.io$Values[3])
	cb.file.elv <- ttkcombobox(frE22b, values = unlist(listOpenFiles), textvariable = file.choix2, state = state[3], width = largeur2)
	infobulle(cb.file.elv, 'Choose the file in the list')
	status.bar.display(cb.file.elv, TextOutputVar, 'Choose the file containing the elevation data')
	bt.file.elv <- tkbutton.h(frE22b, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed')
	tkgrid(cb.file.elv, bt.file.elv)
	tkgrid.configure(cb.file.elv, row = 0, column = 1, sticky = 'we')
	tkgrid.configure(bt.file.elv, row = 0, column = 2, sticky = 'e')
	tkconfigure(bt.file.elv, state = state[3], command = function(){
		nc.opfiles <- getOpenNetcdf(top.win, all.opfiles)
		if(!is.null(nc.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'netcdf'
			AllOpenFilesData[[nopf+1]] <<- nc.opfiles
			tclvalue(file.choix2) <- AllOpenFilesData[[nopf+1]][[1]]

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.choix2) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.file.elv, values = unlist(listOpenFiles), textvariable = file.choix2)
		}else{
			return(NULL)
		}
	})

	###########
	tkbind(cb.uselv,"<Button-1>", function(){
		if(tclvalue(uselv.val) == "0" & tclvalue(uselv.ch) == "1"){
			state <- c('normal', 'normal', 'disabled')
			tkconfigure(uselv.dem, state = state[1])
			tkconfigure(uselv.dat, state = state[2])
			tkconfigure(cb.file.elv, state = state[3])
			tkconfigure(bt.file.elv, state = state[3])
		}else if(tclvalue(uselv.val) == "0" & tclvalue(uselv.ch) == "0"){
			state <- c('normal', 'normal', 'normal')
			tkconfigure(uselv.dem, state = state[1])
			tkconfigure(uselv.dat, state = state[2])
			tkconfigure(cb.file.elv, state = state[3])
			tkconfigure(bt.file.elv, state = state[3])
		}else{
			state <- c('disabled', 'disabled', 'disabled')
			tkconfigure(uselv.dem, state = state[1])
			tkconfigure(uselv.dat, state = state[2])
			tkconfigure(cb.file.elv, state = state[3])
			tkconfigure(bt.file.elv, state = state[3])
		}
	})

	tkbind(uselv.dat,"<Button-1>", function(){
		if(tclvalue(uselv.ch) == "0" & tclvalue(uselv.val) == "1"){
			state <- c('normal', 'normal', 'disabled')
			tkconfigure(uselv.dem, state = state[1])
			tkconfigure(uselv.dat, state = state[2])
			tkconfigure(cb.file.elv, state = state[3])
			tkconfigure(bt.file.elv, state = state[3])
		}
	})

	tkbind(uselv.dem,"<Button-1>", function(){
		if(tclvalue(uselv.ch) == "1" & tclvalue(uselv.val) == "1"){
			state <- c('normal', 'normal', 'normal')
			tkconfigure(uselv.dem, state = state[1])
			tkconfigure(uselv.dat, state = state[2])
			tkconfigure(cb.file.elv, state = state[3])
			tkconfigure(bt.file.elv, state = state[3])
		}
	})

	bt.prm.OK <- tkbutton(frok, text=" OK ")
	bt.prm.CA <- tkbutton(frok, text = "Cancel")
	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 5, ipadx = 5)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 5)

	tkconfigure(bt.prm.OK, command = function(){
		if(tclvalue(uselv.val) == "1" & tclvalue(uselv.ch) == "0" & tclvalue(file.choix2) == ""){
			tkmessageBox(message = "Provide the NetCDF file containing\nthe elevation data", icon = "warning", type = "ok")
		}else{
			GeneralParameters$ref.series.choix$Values <<- c(tclvalue(diff.ratio), tclvalue(weight.fac), tclvalue(uselv.val), tclvalue(uselv.ch),
			tclvalue(min.stn), tclvalue(max.stn), tclvalue(max.dist), tclvalue(elv.diff), tclvalue(min.rho))
			GeneralParameters$file.io$Values <<- c(as.character(GeneralParameters$file.io$Values[1]), as.character(GeneralParameters$file.io$Values[2]),
			tclvalue(file.choix2), as.character(GeneralParameters$file.io$Values[4]))
			GeneralParameters$ref.series.user <<- tclvalue(usr.rfseries.val)
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

	tkgrid(frcont, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frok, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###############################################################
	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+',tt.x,'+',tt.y, sep = ''))
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
	tkgrid(frE1, sticky = 'we', padx = 5, pady = 5)
	tkgrid(frE2, sticky = 'we', padx = 5, pady = 5)
	tkgrid(frE3, padx = 5, pady = 5)

	cb.bounds.val <- tclVar(as.character(GeneralParameters$hom.opts$Values[1]))
	cb.bounds <- tkcheckbutton(frE1, variable = cb.bounds.val, text = 'Crop bounds', anchor = 'w', justify = 'left')
	tkgrid(cb.bounds, pady = 2)
	infobulle(cb.bounds, 'Cropping the first and last\n[h x 100%] percent of the series')
	status.bar.display(cb.bounds, TextOutputVar, 'Cropping the first and last [h x 100%] percent of the series')


	crop.bounds.list <- c('0.010', '0.025', '0.050', '0.100')
	crop.bounds.val <- tclVar(as.character(GeneralParameters$hom.opts$Values[2]))
	Clev.val <- tclVar(as.character(GeneralParameters$hom.opts$Values[3]))

	h.l <- tklabel.h(frE2, text = 'h', TextOutputVar, 'Cropping the first and last\n[h x 100%] percent of the series', 'Cropping the first and last [h x 100%] percent of the series')
	conf.lev.l <- tklabel.h(frE2, text = 'Conf.lev(%)',TextOutputVar, 'Confidence level (%)','Confidence level  (%)')
	h.v <- ttkcombobox(frE2, values = crop.bounds.list, textvariable = crop.bounds.val)
	infobulle(h.v, 'Cropping the first and last\n[h x 100%] percent of the series')
	status.bar.display(h.v, TextOutputVar, 'Cropping the first and last [h x 100%] percent of the series')
	conf.lev.v <- ttkcombobox(frE2, values = Clev.list, textvariable = Clev.val)
	infobulle(conf.lev.v, 'Confidence level (%)')
	status.bar.display(conf.lev.v, TextOutputVar, 'Confidence level (%)')

	Kmax <- tclVar(as.character(GeneralParameters$hom.opts$Values[4]))
	min.int <- tclVar(as.character(GeneralParameters$hom.opts$Values[5]))

	Kmax.l <- tklabel.h(frE2, text = 'Kmax', TextOutputVar, 'Maximum number of change-points to be detected',
	'Maximum number of change-points to be detected')
	min.int.l <- tklabel.h(frE2, text = 'Min.len(months)',TextOutputVar, 'Minimum segment length to carry out the test',
	'Minimum segment length  to carry out the test')
	Kmax.v <- tkentry.h(frE2, TextOutputVar, 'Maximum number of change-points to be detected', 'Maximum number of change-points to be detected')
	min.int.v <- tkentry.h(frE2, TextOutputVar, 'Minimum segment length', 'Minimum segment length')

	tkgrid(h.l, row = 0, column = 0, sticky = 'we', padx = 1, pady = 2)
	tkgrid(h.v, row = 0, column = 1, sticky = 'we', padx = 1, pady = 2)
	tkgrid(conf.lev.l, row = 0, column = 2, sticky = 'we', padx = 1, pady = 2)
	tkgrid(conf.lev.v, row = 0, column = 3, sticky = 'we', padx = 1, pady = 2)
	tkgrid(Kmax.l, row = 1, column = 0, sticky = 'we', padx = 1, pady = 2)
	tkgrid(Kmax.v, row = 1, column = 1, sticky = 'we', padx = 1, pady = 2)
	tkgrid(min.int.l, row = 1, column = 2, sticky = 'we', padx = 1, pady = 2)
	tkgrid(min.int.v, row = 1, column = 3, sticky = 'we', padx = 1, pady = 2)

	tkconfigure(h.v, width = 6)
	tkconfigure(conf.lev.v, width = 6)
	tkconfigure(Kmax.v, width = 6, textvariable = Kmax)
	tkconfigure(min.int.v, width = 6, textvariable = min.int)

	bt.prm.OK <- tkbutton(frE3, text=" OK ")
	tkgrid(bt.prm.OK, ipadx = 5)

	tkconfigure(bt.prm.OK, command = function(){
		GeneralParameters$hom.opts$Values <<- c(tclvalue(cb.bounds.val), tclvalue(crop.bounds.val), tclvalue(Clev.val), tclvalue(Kmax), tclvalue(min.int))
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+',tt.x,'+',tt.y, sep = ''))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'Test option')
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

	fdf1 <- tkframe(tt1)
	fdf2 <- tkframe(tt1)
	tkgrid(fdf1)
	tkgrid(fdf2)

	fr2 <- tkframe(fdf1, relief = "groove", borderwidth = 2)
	tkgrid(fr2, sticky = 'ew')
	btgetadj <- tkbutton(fdf2, text=" OK ")
	tkgrid(btgetadj, ipadx = 5)

	minadj.l <- tklabel.h(fr2, text = 'Min.Adj (in month)',TextOutputVar, 'Minimum number of non-missing values \nto be used to adjust the series (in month)','Minimum number of non-missing values to be used to adjust the series (in month)')
	segadj.l <- tklabel.h(fr2, text = 'Segment to Adjust', TextOutputVar, 'The segment to which the series is to be adjusted\n (0:last segment)','The segment to which the series is to be adjusted (0:last segment)')
	monadj.l <- tklabel.h(fr2, text = 'Month', TextOutputVar, 'Monthly series', 'Monthly series')
	dekadj.l <- tklabel.h(fr2, text = 'Dekad', TextOutputVar, 'Dekadal series', 'Dekadal series')
	dlyadj.l <- tklabel.h(fr2, text = 'Day', TextOutputVar, 'Daily series', 'Daily series')

	minadjdy.v <- tkentry.h(fr2, TextOutputVar, 'Minimum number of non-missing values \nto be used to adjust the series (in month)','Minimum number of non-missing values to be used to adjust the series (in month)')
	segadjdy.v <- tkentry.h(fr2, TextOutputVar, 'The segment to which the series is to be adjusted\n (0:last segment)','The segment to which the series is to be adjusted (0:last segment)')
	minadjdk.v <- tkentry.h(fr2, TextOutputVar, 'Minimum number of non-missing values \nto be used to adjust the series (in month)','Minimum number of non-missing values to be used to adjust the series (in month)')
	segadjdk.v <- tkentry.h(fr2, TextOutputVar, 'The segment to which the series is to be adjusted\n (0:last segment)','The segment to which the series is to be adjusted (0:last segment)')
	minadjmo.v <- tkentry.h(fr2, TextOutputVar, 'Minimum number of non-missing values \nto be used to adjust the series (in month)','Minimum number of non-missing values to be used to adjust the series (in month)')
	segadjmo.v <- tkentry.h(fr2, TextOutputVar, 'The segment to which the series is to be adjusted\n (0:last segment)','The segment to which the series is to be adjusted (0:last segment)')

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

	tkconfigure(minadj.l, anchor = 'e', justify = 'right')
	tkconfigure(segadj.l, anchor = 'e', justify = 'right')

	minadjmo <- tclVar(as.character(GeneralParameters$Adjust.method$Values[1]))
	minadjdk <- tclVar(as.character(GeneralParameters$Adjust.method$Values[2]))
	minadjdy <- tclVar(as.character(GeneralParameters$Adjust.method$Values[3]))

	segadjmo <- tclVar(as.character(GeneralParameters$Adjust.method$Values[4]))
	segadjdk <- tclVar(as.character(GeneralParameters$Adjust.method$Values[5]))
	segadjdy <- tclVar(as.character(GeneralParameters$Adjust.method$Values[6]))

	if(speriod == 'Daily data') state = c('normal', 'normal', 'normal')
	if(speriod == 'Dekadal data') state = c('normal', 'normal', 'disabled')
	if(speriod == 'Monthly data') state = c('normal', 'disabled', 'disabled')

	tkconfigure(minadjmo.v, width = 3, textvariable = minadjmo, state = state[1])
	tkconfigure(minadjdk.v, width = 3, textvariable = minadjdk, state = state[2])
	tkconfigure(minadjdy.v, width = 3, textvariable = minadjdy, state = state[3])
	tkconfigure(segadjmo.v, width = 3, textvariable = segadjmo, state = state[1])
	tkconfigure(segadjdk.v, width = 3, textvariable = segadjdk, state = state[2])
	tkconfigure(segadjdy.v, width = 3, textvariable = segadjdy, state = state[3])

	tkconfigure(btgetadj, command = function(){
		GeneralParameters$Adjust.method$Values <<- c(tclvalue(minadjmo), tclvalue(minadjdk),
		tclvalue(minadjdy), tclvalue(segadjmo), tclvalue(segadjdk), tclvalue(segadjdy))
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(top.win)
	})

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt1, paste('+',tt.x,'+',tt.y, sep = ''))
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
	#scroll frame width, height
	w.scr.frm <- 190
	h.scr.frm <- 160

	nl <- length(stnId)
	tocheck <- rep("0", nl)
	id2check <- as.numeric(GeneralParameters$stn.user.choice)
	if(length(id2check) > 0){
		tocheck[id2check] <- "1"
		xstation <- stnId[id2check]
	}

	tt2 <- tktoplevel()
	tkgrab.set(tt2)
	tkfocus(tt2)

	frcont <- tkframe(tt2, relief = 'raised', borderwidth = 2)
	frok <- tkframe(tt2)

	frchklist <- tkframe(frcont)
	frtransbt <- tkframe(frcont)
	frlistbx <- tkframe(frcont, width = w.scr.frm, relief = "sunken", borderwidth = 2)
	tkgrid(frchklist, frtransbt, frlistbx)

	scr.win <- tkwidget(frchklist, "ScrolledWindow", relief = "sunken", borderwidth = 2)
	tkgrid(scr.win)
	scr.frm <- tkwidget(scr.win, "ScrollableFrame", width = w.scr.frm, height = h.scr.frm, background = "white")
	tcl(scr.win, "setwidget", scr.frm)
	sub.fram <- tclvalue(tcl(scr.frm, "getframe"))
	checkList <- vector(mode = 'list', length = 0)
	for (i in 1:nl){
		assign(paste("cbval", i, sep = ""), tclVar(tocheck[i]))
		checkList[[i]] <- tcl("checkbutton", paste(sub.fram, i, sep = "."), text = stnId[i], anchor = 'w', justify = 'left', background = "white", width = 20)
		tkconfigure(checkList[[i]], variable = paste("cbval", i, sep = ""))
		tkgrid(checkList[[i]], row = i, column = 0, sticky = 'ew', padx = 2)
		if(tocheck[i] == "0") tkdeselect(checkList[[i]])
		if(tocheck[i] == "1") tkselect(checkList[[i]])
	}

	selbut <- tkbutton(frtransbt, text=' >> ')
	tkgrid(selbut)
	delbut <- tkbutton(frtransbt, text=' << ')
	tkgrid(delbut)

	scr.lstbx <- tkscrollbar(frlistbx, repeatinterval = 5, command = function(...)tkyview(choose.stn,...))
	choose.stn <- tklistbox(frlistbx, selectbackground = "yellow", selectforeground = "blue",
	yscrollcommand = function(...)tkset(scr.lstbx,...), selectmode = "multiple", background = "white", width = 20, height = 10)
	tkgrid(choose.stn, scr.lstbx)
	tkgrid.configure(scr.lstbx, sticky = "nswe")
	if(length(id2check) > 0) for (i in 1:length(xstation))  tkinsert(choose.stn, "end", xstation[i])

	okbut <- tkbutton(frok, text=' OK ')
	cabut <- tkbutton(frok, text=' Cancel')
	tkgrid(okbut, row = 0, column = 0, padx = 5, pady = 5, ipadx = 5, sticky = 'w')
	tkgrid(cabut, row = 0, column = 1, padx = 5, pady = 5, sticky = 'e')

	###############
	selectedSTN <- NULL

	tkconfigure(selbut, command = function(){
		for (i in 1:nl) selectedSTN[i] <- tclvalue(paste("cbval", i, sep = ""))
		xselected <- stnId[which(selectedSTN == '1')]
		dejaselect <- as.character(tkget(choose.stn, "0", "end"))
		if(length(dejaselect) > 0){
			xselect <- xselected[!xselected%in%dejaselect]
			if(length(xselect) > 0) for(j in 1:length(xselect))  tkinsert(choose.stn, "end", xselect[j])
		}else{
			if(length(xselected) > 0) for(j in 1:length(xselected))  tkinsert(choose.stn, "end", xselected[j])
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


	tkgrid(frcont, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frok, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###############################################################
	tkwm.withdraw(tt2)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt2))
	tt.h <- as.integer(tkwinfo("reqheight", tt2))
	tt.x <- as.integer(width.scr*0.5-tt.w*0.5)
	tt.y <- as.integer(height.scr*0.5-tt.h*0.5)
	tkwm.geometry(tt2, paste('+',tt.x,'+',tt.y, sep = ''))
	tkwm.transient(tt2)
	tkwm.title(tt2, 'Reference series user choice')
	tkwm.deiconify(tt2)

	tkfocus(tt2)
	tkbind(tt2, "<Destroy>", function() {tkgrab.release(tt2); tkfocus(parent)})
	tkwait.window(tt2)
	return(GeneralParameters)
}


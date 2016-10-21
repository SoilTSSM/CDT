qc.get.info.rain <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	#tkentry width, file path
	if (Sys.info()["sysname"] == "Windows") largeur <- 23
	else largeur <- 21
	
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
	
	pr.relief.set <- c('sunken', 'sunken', 'sunken', 'sunken', 'sunken')
	for(i in 0:4) assign(paste('fr.A', i, sep = ''), tkframe(fr.A, relief = pr.relief.set[i+1], borderwidth = 2))
	for(i in 0:4) tkgrid(get(paste('fr.A', i, sep = '')))
	for(i in 0:4) tkgrid.configure(get(paste('fr.A', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	################################
	fr.A01 <- tkframe(fr.A0)
	fr.A02 <- tkframe(fr.A0)
	tkgrid(fr.A01, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.A02, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	file.period <- tclVar()
	cb.periodVAL <- c('Daily data', 'Dekadal data', 'Monthly data')
	tclvalue(file.period) <- switch(as.character(GeneralParameters$period), 
									'daily' = cb.periodVAL[1], 
									'dekadal' = cb.periodVAL[2],
									'monthly' = cb.periodVAL[3])
	cb.period <- ttkcombobox(fr.A02, values = cb.periodVAL, textvariable = file.period)
	infobulle(cb.period, 'Choose the time step of the data')
	status.bar.display(cb.period, TextOutputVar, 'Choose the time step of the data')
	tkgrid(cb.period)
	
	###########################
	fr.A11 <- tkframe(fr.A1)
	fr.A12 <- tkframe(fr.A1)
	tkgrid(fr.A11, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.A12, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	file.choix1 <- tclVar()
	tclvalue(file.choix1) <- as.character(GeneralParameters$file.io$Values[1])
	file.choix2 <- tclVar()
	tclvalue(file.choix2) <- as.character(GeneralParameters$file.io$Values[2])

	frA11.txt1 <- tklabel(fr.A11, text = 'Input data to control')
	tkgrid(frA11.txt1)

	cb.file.stn <- ttkcombobox(fr.A12, values = unlist(listOpenFiles), textvariable = file.choix1)
	infobulle(cb.file.stn, 'Choose the file in the list')
	status.bar.display(cb.file.stn, TextOutputVar, 'Choose the file containing the data to control')

	bt.file.stn <- tkbutton.h(fr.A12, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed') 
	tkgrid(cb.file.stn, bt.file.stn) 
	tkgrid.configure(cb.file.stn, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.file.stn, row = 0, column = 1, sticky = 'e')
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

	################################
	if(GeneralParameters$AllOrOne == 'one') state1s <- 'normal'
	if(GeneralParameters$AllOrOne == 'all') state1s <- 'disabled'

	if(as.character(GeneralParameters$use.method$Values[1]) == '0' & as.character(GeneralParameters$use.method$Values[2]) == '0'){
		state <- c('disabled', 'normal', 'disabled')
		state1 <- 'disabled'
	}else if(as.character(GeneralParameters$use.method$Values[1]) == '1'){
		state <- c('disabled', 'normal', 'disabled')
		state1 <- 'normal'
	}else if(as.character(GeneralParameters$use.method$Values[2]) == '1'){
		state1 <- 'disabled'
		if(as.character(GeneralParameters$use.method$Values[3]) == '0') state <- c('normal', 'normal', 'normal')
		else state <- c('disabled', 'normal', 'normal')	
	}
		
	#################################
	fr.A21 <- tkframe(fr.A2)
	fr.A22 <- tkframe(fr.A2)
	tkgrid(fr.A21, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.A22, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	frA21.txt1 <- tklabel(fr.A21, text = 'Elevation Data (NetCDF)')
	tkgrid(frA21.txt1)

	cb.file.elv <- ttkcombobox(fr.A22, values = unlist(listOpenFiles), textvariable = file.choix2, state = state[1])
	infobulle(cb.file.elv, 'Choose the file in the list')
	status.bar.display(cb.file.elv, TextOutputVar, 'Choose the file containing the elevation data')
	bt.file.elv <- tkbutton.h(fr.A22, text = "...", TextOutputVar, 'Browse file if not listed', 'Browse file if not listed') 
	tkgrid(cb.file.elv, bt.file.elv) 
	tkgrid.configure(cb.file.elv, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.file.elv, row = 0, column = 1, sticky = 'e')
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
			tkconfigure(cb.file.stn, values = unlist(listOpenFiles), textvariable = file.choix1)
		}else{
			return(NULL)
		}
	})

	################################
	fr.A31 <- tkframe(fr.A3)
	fr.A32 <- tkframe(fr.A3)
	tkgrid(fr.A31, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.A32, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	frA31.txt1 <- tklabel(fr.A31, text = 'Directory to save result')
	tkgrid(frA31.txt1)

	file.save1 <- tclVar(as.character(GeneralParameters$file.io$Values[3]))
	en.file.save <- tkentry(fr.A32, textvariable = file.save1, width = largeur) 
	infobulle(en.file.save, 'Enter the full path to\ndirectory to save result')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save result')
	bt.file.save <- tkbutton.h(fr.A32, text = "...", TextOutputVar, 'or browse here', 'or browse here')
	tkgrid(en.file.save, bt.file.save) 
	tkgrid.configure(en.file.save, row = 0, column = 0, sticky = 'w')
	tkgrid.configure(bt.file.save, row = 0, column = 1, sticky = 'e')
	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(as.character(GeneralParameters$file.io$Values[3]), "")
		if(!file.exists(file2save1)){
			tkmessageBox(message = paste(file2save1, 'does not exist.\n It will be created.',sep = ' '), icon = "warning", type = "ok")
			dir.create(file2save1, recursive = TRUE)
			tclvalue(file.save1) <- file2save1
		}else tclvalue(file.save1) <- file2save1
	})

	################################
	bt.opt.set <- tkbutton.h(fr.A4, text = "Options - Settings", TextOutputVar, 'Set general options for QC', 'Set general options for QC') 
	tkgrid(bt.opt.set, sticky = 'we', padx = 25, pady = 5, ipadx = 1, ipady = 1)

	tkconfigure(bt.opt.set, command = function(){
		if(tclvalue(cb.1series.val) == "0") state.parm <- c('normal', 'normal')
		else state.parm <- c('disabled', 'normal')
		GeneralParameters <<- get.param.rainfall(tt, GeneralParameters, state.parm)
	})

	################################
	pr.relief.set1 <- c('sunken', 'sunken', 'sunken', 'flat')
	for(i in 0:2) assign(paste('fr.B', i, sep = ''), tkframe(fr.B, relief = pr.relief.set1[i+1], borderwidth = 2))
	for(i in 0:2) tkgrid(get(paste('fr.B', i, sep = '')))
	for(i in 0:2) tkgrid.configure(get(paste('fr.B', i, sep = '')), row = i, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	cb.1series.val <- tclVar(as.character(GeneralParameters$use.method$Values[1]))
	cb.1series <- tkcheckbutton(fr.B0, variable = cb.1series.val, text = 'Single Series', anchor = 'w', justify = 'left')
	infobulle(cb.1series, 'Check for one station series')
	status.bar.display(cb.1series, TextOutputVar, 'Check if the data is a series of one station')
	tkgrid(cb.1series, row = 0, column = 0, sticky = 'w', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkconfigure(cb.1series, state = state1s)
	
	cb.1uselv.val <- tclVar(as.character(GeneralParameters$use.method$Values[2]))
	cb.1uselv <- tkcheckbutton(fr.B0, variable = cb.1uselv.val, text = 'Use Elevation', state = state[2], anchor = 'w', justify = 'left')
	infobulle(cb.1uselv, 'Check to use elevation data\n to choose neighbors stations')
	status.bar.display(cb.1uselv, TextOutputVar, 'Check for using elevation data to choose neighbor stations')
	tkgrid(cb.1uselv, row = 1, column = 0, sticky = 'w', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	
	cb.1intelv <- tkradiobutton(fr.B0, text = "Elevation from DEM", anchor = 'w', justify = 'left', state = state[3])
	infobulle(cb.1intelv, 'Check to extract\nelevation data from DEM')
	status.bar.display(cb.1intelv, TextOutputVar, 'If no elevation data are provided for each station, must be checked')
	cb.1datelv <- tkradiobutton(fr.B0, text = "Elevation from STN data", anchor = 'w', justify = 'left', state = state[3])
	infobulle(cb.1datelv, 'Check to use elevation data\nfrom the data to be controled')
	status.bar.display(cb.1datelv, TextOutputVar, 'Check to use elevation data from the data to be controled')
	uselv.ch <- tclVar(as.character(GeneralParameters$use.method$Values[3]))
	tkconfigure(cb.1intelv, variable = uselv.ch, value = "0")
	tkconfigure(cb.1datelv, variable = uselv.ch, value = "1")	
	tkgrid(cb.1intelv, row = 2, column = 0, sticky = 'w', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.1datelv, row = 3, column = 0, sticky = 'w', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	
	##########

	fr.B11 <- ttklabelframe(fr.B1, text = "File Format", labelanchor = "nw", relief = "flat", borderwidth = 2)
	tkgrid(fr.B11)
	ffrmt1 <- tkradiobutton(fr.B11, text = "One variable", state = state1, anchor = 'w', justify = 'left')
	infobulle(ffrmt1, 'In case of single serie:\nThe file contains 1 variable')
	status.bar.display(ffrmt1, TextOutputVar, 'In case of single serie: The file contains 1 variable')
	ffrmt2 <- tkradiobutton(fr.B11, text = "Rain Tmax Tmin", state = state1, anchor = 'w', justify = 'left')
	infobulle(ffrmt2, 'In case of single serie:\nThe file contains Rain, Tmax\nand Tmin in this order')
	status.bar.display(ffrmt2, TextOutputVar, 'In case of single serie:The file contains Rain, Tmax and Tmin in this order')		
	tkgrid(ffrmt1, row = 0, column = 0, sticky = "we")
	tkgrid(ffrmt2, row = 1, column = 0, sticky = "we")
	rbffrmt <- tclVar(as.character(GeneralParameters$file.date.format$Values[1]))
	tkconfigure(ffrmt1, variable = rbffrmt, value = "1")
	tkconfigure(ffrmt2, variable = rbffrmt, value = "0")

	fr.B21 <- ttklabelframe(fr.B2, text = "Dates Format", labelanchor = "nw", relief = "flat", borderwidth = 2)
	tkgrid(fr.B21)
	
	vdtfrmt1 <- tclVar("YYYYMMDD")
	dtfrmt1 <- tkradiobutton(fr.B21, text = tclvalue(vdtfrmt1), textvariable = vdtfrmt1, state = state1, anchor = 'w', justify = 'left')
	infobulle(dtfrmt1, 'In case of single serie:\n dates are merged')
	status.bar.display(dtfrmt1, TextOutputVar, 'In case of single serie: dates are merged')
	vdtfrmt2 <- tclVar("YYYY MM DD")
	dtfrmt2 <- tkradiobutton(fr.B21, text = tclvalue(vdtfrmt2), textvariable = vdtfrmt2, state = state1, anchor = 'w', justify = 'left')
	infobulle(dtfrmt2, 'In case of single serie:\ndates are separated by space\nor tabulation')
	status.bar.display(dtfrmt2, TextOutputVar, 'In case of single serie:dates are separated by space or tabulation')
	tkgrid(dtfrmt1, row = 0, column = 0, sticky = "we")
	tkgrid(dtfrmt2, row = 1, column = 0, sticky = "we")	
	rbdtfrmt <- tclVar(as.character(GeneralParameters$file.date.format$Values[2]))
	tkconfigure(dtfrmt1, variable = rbdtfrmt, value = "1")
	tkconfigure(dtfrmt2, variable = rbdtfrmt, value = "0")
	
	tkbind(cb.period,"<<ComboboxSelected>>", function(){
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
	##################################
	tkbind(cb.1series,"<Button-1>", function(){
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
	tkbind(cb.1uselv,"<Button-1>", function(){
		tkconfigure(ffrmt1, state = 'disabled')
		tkconfigure(ffrmt2, state = 'disabled')
		tkconfigure(dtfrmt1, state = 'disabled')
		tkconfigure(dtfrmt2, state = 'disabled')
		tclvalue(cb.1series.val) <- "0"
		if(tclvalue(cb.1uselv.val) == "0"){
			if(tclvalue(uselv.ch) == "0") state1 <- 'normal'
			else state1 <- 'disabled'
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
	tkbind(cb.1intelv,"<Button-1>", function(){
		if(tclvalue(cb.1uselv.val) == "1"){
			tkconfigure(cb.file.elv, state = 'normal')
			tkconfigure(bt.file.elv, state = 'normal')
		}
	})	
	tkbind(cb.1datelv,"<Button-1>", function(){
		if(tclvalue(cb.1uselv.val) == "1"){
			tkconfigure(cb.file.elv, state = 'disabled')
			tkconfigure(bt.file.elv, state = 'disabled')
		}
	})	

	###################################################

	bt.opt.OK <- tkbutton(frButt, text = "OK") 
	bt.opt.CA <- tkbutton(frButt, text = "Cancel") 	
	tkgrid(bt.opt.OK, row = 0, column = 0, padx = 5, pady = 5, ipadx = 5, sticky = 'w')
	tkgrid(bt.opt.CA, row = 0, column = 1, padx = 5, pady = 5, sticky = 'e')
	
	tkconfigure(bt.opt.OK, command = function(){
		if(tclvalue(file.choix1) == ""){
			tkmessageBox(message = "Choose the file to control", icon = "warning", type = "ok")
		}else if(tclvalue(cb.1uselv.val) == "1" & tclvalue(uselv.ch) == "0" & tclvalue(file.choix2) == ""){
			tkmessageBox(message = "You have to choose DEM data in NetCDF format\n if you want to extract elevation from DEM",
			icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save1) == "" | tclvalue(file.save1) == "NA"){
			tkmessageBox(message = "Choose a directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			
			all.open.file <- as.character(unlist(lapply(1:length(AllOpenFilesData), function(j) AllOpenFilesData[[j]][[1]])))
			jfile <- which(all.open.file == tclvalue(file.choix1))
			if(length(jfile) == 0){
				tkmessageBox(message = "Station data not found or in the wrong format", icon = "warning", type = "ok")
				tkwait.window(tt)
			}
			
			##
			dirQcRain <- file.path(tclvalue(file.save1), paste('QcRainfall', getf.no.ext(tclvalue(file.choix1)), sep = '_'), fsep = .Platform$file.sep)
			dirparams <- file.path(dirQcRain, 'OriginalData', fsep = .Platform$file.sep)
			if(!file.exists(dirparams)) dir.create(dirparams, showWarnings = FALSE, recursive = TRUE)
			fileparams <- file.path(dirparams, 'Parameters.RData', fsep = .Platform$file.sep)
			
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
					donstn <- getCDTdataAndDisplayMsg(AllOpenFilesData[[jfile]][[2]], GeneralParameters$period)

					xycrds <- NULL
					if(!is.null(donstn)){
						# limUp <- apply(donstn$data, 2, function(x){
						# 	x <- x[!is.na(x) & x > 0]
						# 	Q <- quantile(x, prob = c(0.25,0.75,0.9973), names = F)
						# 	round(Q[3]+3*(Q[2]-Q[1]))
						# })
						# limUp <- as.vector(limUp)
						# limUp <- ifelse(is.na(limUp), max(limUp, na.rm = T), limUp)
						# limControl <- data.frame(donstn$id, limUp, donstn$lon, donstn$lat)

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
					donstn <- getCDTTSdataAndDisplayMsg(AllOpenFilesData[[jfile]][[2]], GeneralParameters$period, tclvalue(rbffrmt), tclvalue(rbdtfrmt))
					if(!is.null(donstn)){
						# if(donstn$nbvar == 1) xval <- donstn$var$var
						# else xval <- donstn$var$rr
						# xval <- xval[!is.na(xval) & xval > 0]
						# Quant <- quantile(xval, prob = c(0.25,0.75,0.9973), names = F)
						# valup <- round(Quant[3]+3*(Quant[2]-Quant[1]))
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
				paramsGAL <- list(inputPars = GeneralParameters, dataPars = AllOpenFilesData[[jfile]][3:4], data = donstn)
				save(paramsGAL, file = fileparams)
				return(list(paramsGAL, lchoixStnFr$env$stn.choix, xycrds))
			}

			######
			if(file.exists(fileparams)){
				load(fileparams)
				if(paramsGAL$inputPars$period == GeneralParameters$period & all(AllOpenFilesData[[jfile]][3:4] %in% paramsGAL$dataPars)){
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
				if(lchoixStnFr$env$stn.choix[1] != '') tclvalue(lchoixStnFr$env$stn.choix.val) <- lchoixStnFr$env$stn.choix[1]
				else tclvalue(lchoixStnFr$env$stn.choix.val) <- lchoixStnFr$env$stn.choix[2]
			}else{
				istn <- as.numeric(tclvalue(tcl(lchoixStnFr$env$stn.choix.cb, "current")))+1
				if(istn > 0) istn <- istn
				else istn <- 1
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

	############
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
	#tkwm.geometry(tt, paste(tt.w, 'x', tt.h,'+',tt.x,'+',tt.y, sep = ''))
	tkwm.geometry(tt, paste('+', tt.x, '+', tt.y, sep = ''))
	tkwm.transient(tt)
	tkwm.title(tt, 'Quality Control - Settings')
	tkwm.deiconify(tt)
	#tkwm.iconify(tt)
	
	##################################################################	
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
	
	fr.C <- tkframe(tt1, relief = "sunken", borderwidth = 2)
	fr.E <- tkframe(tt1, relief = "sunken", borderwidth = 2)
	fr.F <- tkframe(tt1)
	tkgrid(fr.C, row = 0, column = 0, sticky = 'ew', padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(fr.E, row = 1, column = 0, sticky = 'ew', padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(fr.F, row = 3, column = 0, sticky = 'ew', padx = 15, pady = 2, ipadx = 1, ipady = 1)

	min.stn.l <- tklabel.h(fr.C, 'Min.stn', TextOutputVar, 'Minimum number of \n neighbor stations to use', 'Minimum number of neighbor stations to use')
	CInt.l <- tklabel.h(fr.C, 'Conf.lev(%)',TextOutputVar, 'Confidence level (%)','Confidence level  (%)')
	max.dist.l <- tklabel.h(fr.C, 'Max.dist(km)',TextOutputVar, 'Maximum distance of \n neighbor stations to use (km)','Maximum distance of neighbor stations to use (km)')
	elv.diff.l <- tklabel.h(fr.C, 'Elv.diff(m)',TextOutputVar, 'Maximum altitude difference of \n neighbor stations to use (m)','Maximum altitude difference of neighbor stations to use (m)')

	min.stn.v <- tkentry.h(fr.C, TextOutputVar, 'Minimum number of \n neighbor stations to use', 'Minimum number of neighbor stations to use')
	CInt.v <- tkentry.h(fr.C, TextOutputVar, 'Confidence level (%)','Confidence level (%)')
	max.dist.v <- tkentry.h(fr.C, TextOutputVar, 'Maximum distance of\n neighbor stations to use (km)','Maximum distance of neighbor stations to use (km)')
	elv.diff.v <- tkentry.h(fr.C, TextOutputVar, 'Maximum altitude difference of \n neighbor stations to use (m)','Maximum altitude difference of neighbor stations to use (m)')

	tkgrid(min.stn.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(min.stn.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(CInt.l, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(CInt.v, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dist.l, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(max.dist.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(elv.diff.l, row = 1, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(elv.diff.v, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)

	tkconfigure(min.stn.l, anchor = 'e', justify = 'right')
	tkconfigure(CInt.l, anchor = 'e', justify = 'right')
	tkconfigure(max.dist.l, anchor = 'e', justify = 'right')
	tkconfigure(elv.diff.l, anchor = 'e', justify = 'right')

	min.stn <- tclVar(as.character(GeneralParameters$parameter[[1]]$Values[1]))
	CInt <- tclVar(as.character(GeneralParameters$parameter[[1]]$Values[2]))
	max.dist <- tclVar(as.character(GeneralParameters$parameter[[1]]$Values[3]))
	elv.diff <- tclVar(as.character(GeneralParameters$parameter[[1]]$Values[4]))
	tkconfigure(min.stn.v, width = 4, textvariable = min.stn, state = state.parm[1])
	tkconfigure(CInt.v, width = 4, textvariable = CInt, state = state.parm[2])
	tkconfigure(max.dist.v, width = 4, textvariable = max.dist, state = state.parm[1])
	tkconfigure(elv.diff.v, width = 4, textvariable = elv.diff, state = state.parm[1])

	###########
	ispmax.l <- tklabel.h(fr.E, 'ispmax', TextOutputVar, 'Test for isolated precipitation:\nMaximum value of the neighbors stations\nis less than ispmax', 'Maximum value of the neighbors stations is less than ispmax')
	ispobs.l <- tklabel.h(fr.E, 'ispobs', TextOutputVar, 'Test for isolated precipitation:\nValue of target station is greater ispobs', 'Value of target station is greater ispobs')
	isdmin.l <- tklabel.h(fr.E, 'isdmin', TextOutputVar, 'Test for isolated dryness:\nMinimum value of the  neighbors stations\nis greater isdmin', 'Minimum value of the  neighbors stations is greater isdmin')
	isdobs.l <- tklabel.h(fr.E, 'isdobs', TextOutputVar, 'Test for isolated dryness:\nValue of target station is less than isdobs', 'Value of target station is less than isdobs')
	isdq1.l <- tklabel.h(fr.E, 'isdq1', TextOutputVar, 'Test for isolated dryness:\nThe 1st quartile value of the neighbors stations\nis greater than isdq1', 'The 1st quartile value of the neighbors stations is greater than isdq1')
	ftldev.l <- tklabel.h(fr.E, 'ftldev', TextOutputVar, 'Test for too large deviations:\nOutlier factor value between 2 and 4', 'Outlier factor value between 2 and 4, multiplier of IQR')

	ispmax.v <- tkentry.h(fr.E, TextOutputVar, 'Test for isolated precipitation:\nMaximum value of the neighbors stations\nis less than ispmax', 'Maximum value of the neighbors stations is less than ispmax')
	ispobs.v <- tkentry.h(fr.E, TextOutputVar, 'Test for isolated precipitation:\nValue of target station is greater ispobs', 'Value of target station is greater ispobs')
	isdmin.v <- tkentry.h(fr.E, TextOutputVar, 'Test for isolated dryness:\nMinimum value of the  neighbors stations\nis greater isdmin', 'Minimum value of the  neighbors stations is greater isdmin')
	isdobs.v <- tkentry.h(fr.E, TextOutputVar, 'Test for isolated dryness:\nValue of target station is less than isdobs', 'Value of target station is less than isdobs')
	isdq1.v <- tkentry.h(fr.E, TextOutputVar, 'Test for isolated dryness:\nThe 1st quartile value of the neighbors stations\nis greater than isdq1', 'The 1st quartile value of the neighbors stations is greater than isdq1')
	ftldev.v <- tkentry.h(fr.E, TextOutputVar, 'Test for too large deviations:\nOutlier factor value between 2 and 4', 'Outlier factor value between 2 and 4, multiplier of IQR')

	tkgrid(ispmax.l, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(ispmax.v, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(ispobs.l, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(ispobs.v, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(isdmin.l, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(isdmin.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(isdobs.l, row = 1, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(isdobs.v, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(isdq1.l, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(isdq1.v, row = 2, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(ftldev.l, row = 2, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(ftldev.v, row = 2, column = 3, sticky = 'ew', padx = 1, pady = 1)

	tkconfigure(ispmax.l, anchor = 'e', justify = 'right')
	tkconfigure(ispobs.l, anchor = 'e', justify = 'right')
	tkconfigure(isdmin.l, anchor = 'e', justify = 'right')
	tkconfigure(isdobs.l, anchor = 'e', justify = 'right')
	tkconfigure(isdq1.l, anchor = 'e', justify = 'right')
	tkconfigure(ftldev.l, anchor = 'e', justify = 'right')

	ispmax <- tclVar(as.character(GeneralParameters$parameter[[3]]$ispmax))
	ispobs <- tclVar(as.character(GeneralParameters$parameter[[3]]$ispobs))
	isdmin <- tclVar(as.character(GeneralParameters$parameter[[3]]$isdmin))
	isdobs <- tclVar(as.character(GeneralParameters$parameter[[3]]$isdobs))
	isdq1 <- tclVar(as.character(GeneralParameters$parameter[[3]]$isdq1))
	ftldev <- tclVar(as.character(GeneralParameters$parameter[[3]]$ftldev))

	tkconfigure(ispmax.v, width = 8, textvariable = ispmax, state = state.parm[1])
	tkconfigure(ispobs.v, width = 8, textvariable = ispobs, state = state.parm[1])
	tkconfigure(isdmin.v, width = 8, textvariable = isdmin, state = state.parm[1])
	tkconfigure(isdobs.v, width = 8, textvariable = isdobs, state = state.parm[1])
	tkconfigure(isdq1.v, width = 8, textvariable = isdq1, state = state.parm[1])
	tkconfigure(ftldev.v, width = 8, textvariable = ftldev, state = state.parm[1])


	################################
	ret.param.rain1 <<- NULL
	bt.prm.OK <- tkbutton(fr.F, text=" OK ") 
	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 25, pady = 1, ipadx = 1, ipady = 1)
	tkconfigure(bt.prm.OK, command = function(){
		GeneralParameters$parameter[[1]][,2] <<- c(tclvalue(min.stn), tclvalue(CInt), tclvalue(max.dist), tclvalue(elv.diff))
		GeneralParameters$parameter[[3]]$ispmax <<- tclvalue(ispmax)
		GeneralParameters$parameter[[3]]$ispobs <<- tclvalue(ispobs)
		GeneralParameters$parameter[[3]]$isdmin <<- tclvalue(isdmin)
		GeneralParameters$parameter[[3]]$isdobs <<- tclvalue(isdobs)
		GeneralParameters$parameter[[3]]$isdq1 <<- tclvalue(isdq1)
		GeneralParameters$parameter[[3]]$ftldev <<- tclvalue(ftldev)
		ret.param.rain1 <<- 0
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)	
	})

	bt.prm.CA <- tkbutton(fr.F, text = "Cancel") 
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 25, pady = 1, ipadx = 1, ipady = 1)
	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})
	
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




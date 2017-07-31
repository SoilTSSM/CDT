
climdexPanelCmd.RR <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(45)
		hauteur2 <- hscrlwin-40
		largeur0 <- as.integer(w.scale(22)/sfont0)
		largeur1 <- as.integer(w.scale(29)/sfont0)
		largeur2 <- as.integer(w.scale(31)/sfont0)
	}else{
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(46)
		hauteur2 <- hscrlwin-50
		largeur0 <- as.integer(w.scale(18)/sfont0)
		largeur1 <- as.integer(w.scale(22)/sfont0)
		largeur2 <- as.integer(w.scale(23.5)/sfont0)
	}

	GeneralParameters <- fromJSON(file.path(apps.dir, 'init_params', 'Climdex_RR.json'))

	GeneralParameters$One.series$id <- "Station"
	GeneralParameters$One.series$lon <- 0
	GeneralParameters$One.series$lat <- 0

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Indices")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Plot")

	bwRaiseTab(tknote.cmd, cmd.tab1)
	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab3, 0, weight = 1)

	#######################################################################################################

	#Tab1
	frTab1 <- tkframe(cmd.tab1)
	tkgrid(frTab1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab1, 0, weight = 1)

	scrw1 <- bwScrolledWindow(frTab1)
	tkgrid(scrw1)
	tkgrid.columnconfigure(scrw1, 0, weight = 1)
	subfr1 <- bwScrollableFrame(scrw1, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr1, 0, weight = 1)

	#######################

	frameData <- ttklabelframe(subfr1, text = "Type of Data", relief = 'groove')

	DataType <- tclVar()
	CbdatatypeVAL <- c('One station series', 'CDT data format', 'NetCDF gridded data')
	tclvalue(DataType) <- switch(GeneralParameters$data.type, 
								'series' = CbdatatypeVAL[1],
								'cdt' = CbdatatypeVAL[2], 
								'netcdf' = CbdatatypeVAL[3])

	cb.datatype <- ttkcombobox(frameData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)
	bt.datatype <- ttkbutton(frameData, text = "Settings")

	###############
	# settingdone <- NULL
	if(tclvalue(DataType) == 'One station series'){
		tkconfigure(bt.datatype, state = 'normal', command = function(){
			GeneralParameters <<- oneStnDataClimdex(main.win, GeneralParameters)
			# settingdone <<- 1
		})
	}
	if(tclvalue(DataType) == 'CDT data format'){
		tkconfigure(bt.datatype, state = 'disabled')
	}
	if(tclvalue(DataType) == 'NetCDF gridded data'){
		tkconfigure(bt.datatype, state = 'normal', command = function(){
			GeneralParameters <<- netcdfDataClimdexRR(main.win, GeneralParameters, str_trim(tclvalue(file.stnfl)))
			# settingdone <<- 1
		})
	}

	###############

	tkgrid(cb.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(bt.datatype, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

	infobulle(cb.datatype, 'Select the type of input data')
	status.bar.display(cb.datatype, TextOutputVar, 'Select the type of input data')
	infobulle(bt.datatype, 'Setting data type options')
	status.bar.display(bt.datatype, TextOutputVar, 'Setting data type options')

	###############

	tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
		tkdestroy(cb.stnfl)
		tclvalue(file.stnfl) <- ''

		if(tclvalue(DataType) %in% c('One station series', 'CDT data format')){
			tclvalue(fileINdir) <- 'File containing stations input data'

			cb.stnfl <- ttkcombobox(frameInd, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)

			#######
			tkconfigure(bt.stnfl, command = function(){
				dat.opfiles <- getOpenFiles(main.win, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(file.stnfl) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
				}else return(NULL)
			})

			if(tclvalue(DataType) == 'One station series'){
				tkconfigure(bt.datatype, state = 'normal', command = function(){
					GeneralParameters <<- oneStnDataClimdex(main.win, GeneralParameters)
					# settingdone <<- 1
				})
			}

			if(tclvalue(DataType) == 'CDT data format'){
				tkconfigure(bt.datatype, state = 'disabled')
			}

			infobulle(cb.stnfl, 'Select the file in the list')
			status.bar.display(cb.stnfl, TextOutputVar, 'Select the file containing the daily rainfall data')
		}

		if(tclvalue(DataType) == 'NetCDF gridded data'){
			tclvalue(fileINdir) <- 'Directory containing the NetCDF data'

			cb.stnfl <- tkentry(frameInd, textvariable = file.stnfl, width = largeur2)

			#######
			tkconfigure(bt.stnfl, command = function(){
				file2convert <- tk_choose.dir(getwd(), "")
				tclvalue(file.stnfl) <- if(!is.na(file2convert)) file2convert else ""
			})

			tkconfigure(bt.datatype, state = 'normal', command = function(){
				GeneralParameters <<- netcdfDataClimdexRR(main.win, GeneralParameters, str_trim(tclvalue(file.stnfl)))
				# settingdone <<- 1
			})

			infobulle(cb.stnfl, 'Enter the full path to directory containing the NetCDF data')
			status.bar.display(cb.stnfl, TextOutputVar, 'Enter the full path to directory containing the NetCDF data')
		}
		
		#######
		tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	})

	#############################

	frameInd <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

	file.stnfl <- tclVar(GeneralParameters$IO.files$In.dir.file)
	fileINdir <- tclVar('File containing stations input data')

	txt.stnfl <- tklabel(frameInd, text = tclvalue(fileINdir), textvariable = fileINdir, anchor = 'w', justify = 'left')
	cb.stnfl <- ttkcombobox(frameInd, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
	bt.stnfl <- tkbutton(frameInd, text = "...")

	###############
	tkconfigure(bt.stnfl, command = function(){
		dat.opfiles <- getOpenFiles(main.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.stnfl) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles), textvariable = file.stnfl)
		}else return(NULL)
	})

	###############
	tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.stnfl, 'Select the file containing the daily rainfall data')
	status.bar.display(cb.stnfl, TextOutputVar, 'Select the file containing the daily rainfall data')
	infobulle(bt.stnfl, 'Browse file if not listed')
	status.bar.display(bt.stnfl, TextOutputVar, 'Browse file if not listed')

	#############################
	frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

	file.save1 <- tclVar(GeneralParameters$IO.files$Out.dir.file)

	txt.file.save <- tklabel(frameDirSav, text = "Directory to save indices",  anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frameDirSav, textvariable = file.save1, width = largeur2)
	bt.file.save <- tkbutton(frameDirSav, text = "...")

	##############
	tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save1, isFile = FALSE))

	##############
	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.file.save, 'Enter the full path to directory to save the calculated indices')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to save the calculated indices')
	infobulle(bt.file.save, 'or browse here')
	status.bar.display(bt.file.save, TextOutputVar, 'or browse here')

	#############################

	frameBaseyear <- ttklabelframe(subfr1, text = "Base period", relief = 'groove')

	startYear <- tclVar(GeneralParameters$baseYear$start.year)
	endYear <- tclVar(GeneralParameters$baseYear$end.year)

	txt.startYear <- tklabel(frameBaseyear, text = "Start year of base period",  anchor = 'e', justify = 'right')
	en.startYear <- tkentry(frameBaseyear, textvariable = startYear, width = 6)

	txt.endYear <- tklabel(frameBaseyear, text = "End year of base period",  anchor = 'e', justify = 'right')
	en.endYear <- tkentry(frameBaseyear, textvariable = endYear, width = 6)

	tkgrid(txt.startYear, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.startYear, row = 0, column = 3, sticky = 'w', rowspan = 1, columnspan = 2, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.endYear, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.endYear, row = 1, column = 3, sticky = 'w', rowspan = 1, columnspan = 2, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	#############################
	tkgrid(frameData, row = 0, column = 0, sticky = 'we')
	tkgrid(frameInd, row = 1, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameBaseyear, row = 3, column = 0, sticky = '', pady = 3)

	#######################################################################################################

	#Tab2
	frTab2 <- tkframe(cmd.tab2)
	tkgrid(frTab2, padx = 5, pady = 5, ipadx = 2, ipady = 2)
	tkgrid.columnconfigure(frTab2, 0, weight = 1)

	scrw2 <- bwScrolledWindow(frTab2)
	tkgrid(scrw2)
	tkgrid.columnconfigure(scrw2, 0, weight = 1)
	subfr2 <- bwScrollableFrame(scrw2, width = wscrlwin, height = hauteur2)
	tkgrid.columnconfigure(subfr2, 0, weight = 1)

	#######################
	frameIndex <- tkframe(subfr2, relief = 'sunken', borderwidth = 2)

	is.Rx1day <- tclVar(GeneralParameters$Indices$Rx1day)
	is.Rx5day <- tclVar(GeneralParameters$Indices$Rx5day)
	is.SDII <- tclVar(GeneralParameters$Indices$SDII)
	is.R10mm <- tclVar(GeneralParameters$Indices$R10mm)
	is.R20mm <- tclVar(GeneralParameters$Indices$R20mm)
	is.Rnnmm <- tclVar(GeneralParameters$Indices$Rnnmm)
	val.Rnnmm <- tclVar(GeneralParameters$Indices$thres.Rnnmm)

	is.CDD <- tclVar(GeneralParameters$Indices$CDD)
	is.CWD <- tclVar(GeneralParameters$Indices$CWD)
	is.R95pTOT <- tclVar(GeneralParameters$Indices$R95pTOT)
	is.R99pTOT <- tclVar(GeneralParameters$Indices$R99pTOT)
	is.PRCPTOT <- tclVar(GeneralParameters$Indices$PRCPTOT)

	chk.Rx1day <- tkcheckbutton(frameIndex, variable = is.Rx1day, text = 'Rx1day: Monthly maximum 1-day precipitation', anchor = 'w', justify = 'left')
	chk.Rx5day <- tkcheckbutton(frameIndex, variable = is.Rx5day, text = 'Rx5day: Monthly maximum consecutive 5-day precipitation', anchor = 'w', justify = 'left')
	chk.SDII <- tkcheckbutton(frameIndex, variable = is.SDII, text = 'SDII: Simple precipitation intensity index', anchor = 'w', justify = 'left')
	chk.R10mm <- tkcheckbutton(frameIndex, variable = is.R10mm, text = 'R10mm: Annual count of days when PRCP >= 10mm', anchor = 'w', justify = 'left')
	chk.R20mm <- tkcheckbutton(frameIndex, variable = is.R20mm, text = 'R20mm: Annual count of days when PRCP >= 20mm', anchor = 'w', justify = 'left')
	chk.Rnnmm <- tkcheckbutton(frameIndex, variable = is.Rnnmm, text = 'Rnnmm: Annual count of days when PRCP >= nnmm', anchor = 'w', justify = 'left')

	frameRnnmm <- tkframe(frameIndex)

	chk.CDD <- tkcheckbutton(frameIndex, variable = is.CDD, text = 'CDD: Maximum length of dry spell', anchor = 'w', justify = 'left')
	chk.CWD <- tkcheckbutton(frameIndex, variable = is.CWD, text = 'CWD: Maximum length of wet spell', anchor = 'w', justify = 'left')
	chk.R95pTOT <- tkcheckbutton(frameIndex, variable = is.R95pTOT, text = 'R95pTOT: Annual total PRCP when RR > 95th percentile', anchor = 'w', justify = 'left')
	chk.R99pTOT <- tkcheckbutton(frameIndex, variable = is.R99pTOT, text = 'R99pTOT: Annual total PRCP when RR > 99th percentile', anchor = 'w', justify = 'left')
	chk.PRCPTOT <- tkcheckbutton(frameIndex, variable = is.PRCPTOT, text = 'PRCPTOT: Annual total precipitation in wet days', anchor = 'w', justify = 'left')

	################

	txt.Rnnmm <- tklabel(frameRnnmm, text = 'User defined threshold (mm)', anchor = 'w', justify = 'left')
	en.Rnnmm <- tkentry(frameRnnmm, width = 4, textvariable = val.Rnnmm, justify = "left")
	tkgrid(txt.Rnnmm, en.Rnnmm)

	################

	tkgrid(chk.Rx1day, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.Rx5day, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.SDII, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.R10mm, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.R20mm, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.Rnnmm, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(frameRnnmm, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(chk.CDD, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.CWD, row = 8, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.R95pTOT, row = 9, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.R99pTOT, row = 10, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.PRCPTOT, row = 11, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(frameIndex, "Check desired indices to calculate")
	status.bar.display(frameIndex, TextOutputVar, "Check desired indices to calculate")

	#######################
	tkgrid(frameIndex, row = 0, column = 0, sticky = 'we')
	tcl('update')

	#############################
	frameCalc <- tkframe(frTab2)

	CalcBut <- ttkbutton(frameCalc, text = "Calculate")
	tkgrid(CalcBut, padx = 5, pady = 5)

	tkconfigure(CalcBut, command = function(){
		GeneralParameters$data.type <- switch(tclvalue(DataType),
												'One station series' = 'series',
												'CDT data format' = 'cdt',
												'NetCDF gridded data' = 'netcdf')

		GeneralParameters$IO.files$In.dir.file <- str_trim(tclvalue(file.stnfl))
		GeneralParameters$IO.files$Out.dir.file <- str_trim(tclvalue(file.save1))
				
		GeneralParameters$baseYear$start.year <- as.numeric(str_trim(tclvalue(startYear)))
		GeneralParameters$baseYear$end.year <- as.numeric(str_trim(tclvalue(endYear)))

		GeneralParameters$Indices$Rx1day <- switch(tclvalue(is.Rx1day), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$Rx5day <- switch(tclvalue(is.Rx5day), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$SDII <- switch(tclvalue(is.SDII), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$R10mm <- switch(tclvalue(is.R10mm), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$R20mm <- switch(tclvalue(is.R20mm), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$Rnnmm <- switch(tclvalue(is.Rnnmm), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$thres.Rnnmm <- as.numeric(str_trim(tclvalue(val.Rnnmm)))
		GeneralParameters$Indices$CDD <- switch(tclvalue(is.CDD), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$CWD <- switch(tclvalue(is.CWD), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$R95pTOT <- switch(tclvalue(is.R95pTOT), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$R99pTOT <- switch(tclvalue(is.R99pTOT), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$PRCPTOT <- switch(tclvalue(is.PRCPTOT), '0' = FALSE, '1' = TRUE)

		# assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)

		tkconfigure(main.win, cursor = 'watch')
		InsertMessagesTxt(main.txt.out, "Calculating Indices (this may take some time) .......")
		ret <- tryCatch(
			climdexCalc.RR(GeneralParameters),
			#warning = function(w) warningFun(w),
			error = function(e){
				errorFun(e)
			},
			finally = {
				tkconfigure(main.win, cursor = '')
			}
		)

		if(!is.null(ret)){
			if(ret == 0) InsertMessagesTxt(main.txt.out, "Indices calculation finished successfully")
			else InsertMessagesTxt(main.txt.out, "Indices calculation failed", format = TRUE)
		}else{
			InsertMessagesTxt(main.txt.out, "Indices calculation failed", format = TRUE)
		}
	})

	#######################
	tkgrid(frameCalc)

	#######################################################################################################

	#Tab3
	frTab3 <- tkframe(cmd.tab3)
	tkgrid(frTab3, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab3, 0, weight = 1)

	scrw3 <- bwScrolledWindow(frTab3)
	tkgrid(scrw3)
	tkgrid.columnconfigure(scrw3, 0, weight = 1)
	subfr3 <- bwScrollableFrame(scrw3, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr3, 0, weight = 1)


	#######################################################################################################
	tcl('update')
	tkgrid(cmd.frame, sticky = 'nswe', pady = 5)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)
	######
	return(cmd.frame)
}






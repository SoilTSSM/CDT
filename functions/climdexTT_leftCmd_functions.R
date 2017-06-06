
climdexPanelCmd.TT <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(45)
		largeur0 <- as.integer(w.scale(22)/sfont0)
		largeur1 <- as.integer(w.scale(30)/sfont0)
		largeur2 <- as.integer(w.scale(28)/sfont0)
	}else{
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(46)
		largeur0 <- as.integer(w.scale(18)/sfont0)
		largeur1 <- as.integer(w.scale(22)/sfont0)
		largeur2 <- as.integer(w.scale(23.5)/sfont0)
	}

	GeneralParameters <- fromJSON(file.path(apps.dir, 'init_params', 'Climdex_TT.json'))

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
			GeneralParameters <<- netcdfDataClimdexTT(main.win, GeneralParameters,
													str_trim(tclvalue(file.stnfl1)),
													str_trim(tclvalue(file.stnfl2)))
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
		tkdestroy(cb.stnfl1)
		tkdestroy(cb.stnfl2)
		tclvalue(file.stnfl1) <- ''
		tclvalue(file.stnfl2) <- ''

		if(tclvalue(DataType) %in% c('One station series', 'CDT data format')){
			tclvalue(fileINdir1) <- 'File containing daily TX data'
			tclvalue(fileINdir2) <- 'File containing daily TN data'

			cb.stnfl1 <- ttkcombobox(frameInd, values = unlist(listOpenFiles), textvariable = file.stnfl1, width = largeur1)
			cb.stnfl2 <- ttkcombobox(frameInd, values = unlist(listOpenFiles), textvariable = file.stnfl2, width = largeur1)

			#######
			tkconfigure(bt.stnfl1, command = function(){
				dat.opfiles <- getOpenFiles(main.win, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(file.stnfl1) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.stnfl1, values = unlist(listOpenFiles), textvariable = file.stnfl1)
					tkconfigure(cb.stnfl2, values = unlist(listOpenFiles), textvariable = file.stnfl2)
				}else return(NULL)
			})

			tkconfigure(bt.stnfl2, command = function(){
				dat.opfiles <- getOpenFiles(main.win, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(file.stnfl2) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.stnfl1, values = unlist(listOpenFiles), textvariable = file.stnfl1)
					tkconfigure(cb.stnfl2, values = unlist(listOpenFiles), textvariable = file.stnfl2)
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

			infobulle(cb.stnfl1, 'Select the file in the list')
			status.bar.display(cb.stnfl1, TextOutputVar, 'Select the file containing the daily maximum temperature data')
			infobulle(cb.stnfl2, 'Select the file in the list')
			status.bar.display(cb.stnfl2, TextOutputVar, 'Select the file containing the daily minimum temperature data')
		}

		if(tclvalue(DataType) == 'NetCDF gridded data'){
			tclvalue(fileINdir1) <- 'Directory containing the TX NetCDF data'
			tclvalue(fileINdir2) <- 'Directory containing the TN NetCDF data'

			cb.stnfl1 <- tkentry(frameInd, textvariable = file.stnfl1, width = largeur2)
			cb.stnfl2 <- tkentry(frameInd, textvariable = file.stnfl2, width = largeur2)

			#######
			tkconfigure(bt.stnfl1, command = function(){
				file2convert <- tk_choose.dir(getwd(), "")
				tclvalue(file.stnfl1) <- if(!is.na(file2convert)) file2convert else ""
			})

			tkconfigure(bt.stnfl2, command = function(){
				file2convert <- tk_choose.dir(getwd(), "")
				tclvalue(file.stnfl2) <- if(!is.na(file2convert)) file2convert else ""
			})

			tkconfigure(bt.datatype, state = 'normal', command = function(){
				GeneralParameters <<- netcdfDataClimdexTT(main.win, GeneralParameters, 
														str_trim(tclvalue(file.stnfl1)),
														str_trim(tclvalue(file.stnfl2)))
				# settingdone <<- 1
			})

			infobulle(cb.stnfl1, 'Enter the full path to directory containing the TX NetCDF data')
			status.bar.display(cb.stnfl1, TextOutputVar, 'Enter the full path to directory containing the TX  NetCDF data')
			infobulle(cb.stnfl2, 'Enter the full path to directory containing the TN NetCDF data')
			status.bar.display(cb.stnfl2, TextOutputVar, 'Enter the full path to directory containing the TN NetCDF data')
		}
		
		#######
		tkgrid(cb.stnfl1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(cb.stnfl2, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	})

	#############################
	frameInd <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

	file.stnfl1 <- tclVar(GeneralParameters$IO.files$TX.dir.file)
	file.stnfl2 <- tclVar(GeneralParameters$IO.files$TN.dir.file)
	fileINdir1 <- tclVar('File containing daily TX data')
	fileINdir2 <- tclVar('File containing daily TN data')

	txt.stnfl1 <- tklabel(frameInd, text = tclvalue(fileINdir1), textvariable = fileINdir1, anchor = 'w', justify = 'left')
	cb.stnfl1 <- ttkcombobox(frameInd, values = unlist(listOpenFiles), textvariable = file.stnfl1, width = largeur1)
	bt.stnfl1 <- tkbutton(frameInd, text = "...")

	txt.stnfl2 <- tklabel(frameInd, text = tclvalue(fileINdir2), textvariable = fileINdir2, anchor = 'w', justify = 'left')
	cb.stnfl2 <- ttkcombobox(frameInd, values = unlist(listOpenFiles), textvariable = file.stnfl2, width = largeur1)
	bt.stnfl2 <- tkbutton(frameInd, text = "...")

	###############
	tkconfigure(bt.stnfl1, command = function(){
		dat.opfiles <- getOpenFiles(main.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.stnfl1) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl1, values = unlist(listOpenFiles), textvariable = file.stnfl1)
			tkconfigure(cb.stnfl2, values = unlist(listOpenFiles), textvariable = file.stnfl2)
		}else return(NULL)
	})

	tkconfigure(bt.stnfl2, command = function(){
		dat.opfiles <- getOpenFiles(main.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.stnfl2) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.stnfl1, values = unlist(listOpenFiles), textvariable = file.stnfl1)
			tkconfigure(cb.stnfl2, values = unlist(listOpenFiles), textvariable = file.stnfl2)
		}else return(NULL)
	})

	###############
	tkgrid(txt.stnfl1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl1, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.stnfl2, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl2, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl2, row = 3, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.stnfl1, 'Select the file in the list')
	status.bar.display(cb.stnfl1, TextOutputVar, 'Select the file containing the daily maximum temperature data')
	infobulle(bt.stnfl1, 'Browse file if not listed')
	status.bar.display(bt.stnfl1, TextOutputVar, 'Browse file if not listed')
	infobulle(cb.stnfl2, 'Select the file in the list')
	status.bar.display(cb.stnfl2, TextOutputVar, 'Select the file containing the daily minimum temperature data')
	infobulle(bt.stnfl2, 'Browse file if not listed')
	status.bar.display(bt.stnfl2, TextOutputVar, 'Browse file if not listed')

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
	tkgrid(frameBaseyear, row = 3, column = 0, sticky = 'we', pady = 3)

	#######################################################################################################

	#Tab2
	frTab2 <- tkframe(cmd.tab2)
	tkgrid(frTab2, padx = 5, pady = 5, ipadx = 2, ipady = 2)
	tkgrid.columnconfigure(frTab2, 0, weight = 1)

	scrw2 <- bwScrolledWindow(frTab2)
	tkgrid(scrw2)
	tkgrid.columnconfigure(scrw2, 0, weight = 1)
	subfr2 <- bwScrollableFrame(scrw2, width = wscrlwin, height = hscrlwin-50)
	tkgrid.columnconfigure(subfr2, 0, weight = 1)

	frameCalc <- tkframe(frTab2)
	tkgrid(frameCalc)

	#######################
	frameIndex <- tkframe(subfr2, relief = 'sunken', borderwidth = 2)

	is.TXx <- tclVar(GeneralParameters$Indices$TXx)
	is.TXn <- tclVar(GeneralParameters$Indices$TXn)
	is.TX10p <- tclVar(GeneralParameters$Indices$TX10p)
	is.TX90p <- tclVar(GeneralParameters$Indices$TX90p)
	is.WSDI <- tclVar(GeneralParameters$Indices$WSDI)
	is.SU <- tclVar(GeneralParameters$Indices$SU)
	val.upTX <- tclVar(GeneralParameters$Indices$upTX)
	is.ID <- tclVar(GeneralParameters$Indices$ID)
	val.loTX <- tclVar(GeneralParameters$Indices$loTX)

	is.TNx <- tclVar(GeneralParameters$Indices$TNx)
	is.TNn <- tclVar(GeneralParameters$Indices$TNn)
	is.TN10p <- tclVar(GeneralParameters$Indices$TN10p)
	is.TN90p <- tclVar(GeneralParameters$Indices$TN90p)
	is.CSDI <- tclVar(GeneralParameters$Indices$CSDI)
	is.TR <- tclVar(GeneralParameters$Indices$TR)
	val.upTN <- tclVar(GeneralParameters$Indices$upTN)
	is.FD <- tclVar(GeneralParameters$Indices$FD)
	val.loTN <- tclVar(GeneralParameters$Indices$loTN)

	is.DTR <- tclVar(GeneralParameters$Indices$DTR)
	is.GSL <- tclVar(GeneralParameters$Indices$GSL)

	chk.TXn <- tkcheckbutton(frameIndex, variable = is.TXn, text = 'TXn: Monthly minimum value of TX', anchor = 'w', justify = 'left')
	chk.TXx <- tkcheckbutton(frameIndex, variable = is.TXx, text = 'TXx: Monthly maximum value of TX', anchor = 'w', justify = 'left')
	chk.TX10p <- tkcheckbutton(frameIndex, variable = is.TX10p, text = 'TX10p: Percentage of days when TX < 10th percentile', anchor = 'w', justify = 'left')
	chk.TX90p <- tkcheckbutton(frameIndex, variable = is.TX90p, text = 'TX90p: Percentage of days when TX > 90th percentile', anchor = 'w', justify = 'left')
	chk.WSDI <- tkcheckbutton(frameIndex, variable = is.WSDI, text = 'WSDI: Warm spell duration index', anchor = 'w', justify = 'left')

	chk.ID <- tkcheckbutton(frameIndex, variable = is.ID, text = 'ID: Number of icing days when TX < 0C (or user defined threshold)', anchor = 'w', justify = 'left')
	frameID <- tkframe(frameIndex)

	chk.SU <- tkcheckbutton(frameIndex, variable = is.SU, text = 'SU: Number of summer days when TX > 25C (or user defined threshold)', anchor = 'w', justify = 'left')
	frameSU <- tkframe(frameIndex)

	chk.TNn <- tkcheckbutton(frameIndex, variable = is.TNn, text = 'TNn: Monthly minimum value of TN', anchor = 'w', justify = 'left')
	chk.TNx <- tkcheckbutton(frameIndex, variable = is.TNx, text = 'TNx: Monthly maximum value of TN', anchor = 'w', justify = 'left')
	chk.TN10p <- tkcheckbutton(frameIndex, variable = is.TN10p, text = 'TN10p: Percentage of days when TN < 10th percentile', anchor = 'w', justify = 'left')
	chk.TN90p <- tkcheckbutton(frameIndex, variable = is.TN90p, text = 'TN90p: Percentage of days when TN > 90th percentile', anchor = 'w', justify = 'left')
	chk.CSDI <- tkcheckbutton(frameIndex, variable = is.CSDI, text = 'CSDI: Cold spell duration index', anchor = 'w', justify = 'left')

	chk.FD <- tkcheckbutton(frameIndex, variable = is.FD, text = 'FD: Number of frost days when TN < 0C (or user defined threshold)', anchor = 'w', justify = 'left')
	frameFD <- tkframe(frameIndex)

	chk.TR <- tkcheckbutton(frameIndex, variable = is.TR, text = 'TR: Number of tropical nights when TN > 20C (or user defined threshold)', anchor = 'w', justify = 'left')
	frameTR <- tkframe(frameIndex)

	chk.DTR <- tkcheckbutton(frameIndex, variable = is.DTR, text = 'DTR: Daily temperature range', anchor = 'w', justify = 'left')
	chk.GSL <- tkcheckbutton(frameIndex, variable = is.GSL, text = 'GSL: Growing season length', anchor = 'w', justify = 'left')

	################

	txt.ID <- tklabel(frameID, text = 'User defined lower threshold of TX', anchor = 'w', justify = 'left')
	en.ID <- tkentry(frameID, width = 4, textvariable = val.loTX, justify = "left")
	tkgrid(txt.ID, en.ID)

	txt.SU <- tklabel(frameSU, text = 'User defined upper threshold of TX', anchor = 'w', justify = 'left')
	en.SU <- tkentry(frameSU, width = 4, textvariable = val.upTX, justify = "left")
	tkgrid(txt.SU, en.SU)

	txt.FD <- tklabel(frameFD, text = 'User defined lower threshold of TN', anchor = 'w', justify = 'left')
	en.FD <- tkentry(frameFD, width = 4, textvariable = val.loTN, justify = "left")
	tkgrid(txt.FD, en.FD)

	txt.TR <- tklabel(frameTR, text = 'User defined upper threshold of TN', anchor = 'w', justify = 'left')
	en.TR <- tkentry(frameTR, width = 4, textvariable = val.upTN, justify = "left")
	tkgrid(txt.TR, en.TR)

	################

	sep_indx1 <- ttkseparator(frameIndex)
	sep_indx2 <- ttkseparator(frameIndex)

	################

	tkgrid(chk.TXn, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.TXx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.TX10p, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.TX90p, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.WSDI, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(chk.ID, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frameID, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(chk.SU, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frameSU, row = 8, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(sep_indx1, row = 9, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)

	tkgrid(chk.TNn, row = 10, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.TNx, row = 11, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.TN10p, row = 12, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.TN90p, row = 13, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.CSDI, row = 14, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(chk.FD, row = 15, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frameFD, row = 16, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(chk.TR, row = 17, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frameTR, row = 18, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(sep_indx2, row = 19, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)

	tkgrid(chk.DTR, row = 20, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.GSL, row = 21, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)


	infobulle(frameIndex, "Check desired indices to calculate")
	status.bar.display(frameIndex, TextOutputVar, "Check desired indices to calculate")

	#############################
	tkgrid(frameIndex, row = 0, column = 0, sticky = 'we')

	#######################
	CalcBut <- ttkbutton(frameCalc, text = "Calculate")
	tkgrid(CalcBut, padx = 5, pady = 5)

	tkconfigure(CalcBut, command = function(){

		GeneralParameters$data.type <- switch(tclvalue(DataType),
												'One station series' = 'series',
												'CDT data format' = 'cdt',
												'NetCDF gridded data' = 'netcdf')

		GeneralParameters$IO.files$TX.dir.file <- str_trim(tclvalue(file.stnfl1))
		GeneralParameters$IO.files$TN.dir.file <- str_trim(tclvalue(file.stnfl2))
		GeneralParameters$IO.files$Out.dir.file <- str_trim(tclvalue(file.save1))
				
		GeneralParameters$baseYear$start.year <- as.numeric(str_trim(tclvalue(startYear)))
		GeneralParameters$baseYear$end.year <- as.numeric(str_trim(tclvalue(endYear)))

		GeneralParameters$Indices$TXx <- switch(tclvalue(is.TXx), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$TXn <- switch(tclvalue(is.TXn), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$TX10p <- switch(tclvalue(is.TX10p), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$TX90p <- switch(tclvalue(is.TX90p), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$WSDI <- switch(tclvalue(is.WSDI), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$SU <- switch(tclvalue(is.SU), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$upTX <- as.numeric(str_trim(tclvalue(val.upTX)))
		GeneralParameters$Indices$ID <- switch(tclvalue(is.ID), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$loTX <- as.numeric(str_trim(tclvalue(val.loTX)))

		GeneralParameters$Indices$TNx <- switch(tclvalue(is.TNx), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$TNn <- switch(tclvalue(is.TNn), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$TN10p <- switch(tclvalue(is.TN10p), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$TN90p <- switch(tclvalue(is.TN90p), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$CSDI <- switch(tclvalue(is.CSDI), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$TR <- switch(tclvalue(is.TR), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$upTN <- as.numeric(str_trim(tclvalue(val.upTN)))
		GeneralParameters$Indices$FD <- switch(tclvalue(is.FD), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$loTN <- as.numeric(str_trim(tclvalue(val.loTN)))

		GeneralParameters$Indices$DTR <- switch(tclvalue(is.DTR), '0' = FALSE, '1' = TRUE)
		GeneralParameters$Indices$GSL <- switch(tclvalue(is.GSL), '0' = FALSE, '1' = TRUE)

		# assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)

		tkconfigure(main.win, cursor = 'watch')
		InsertMessagesTxt(main.txt.out, "Calculating Indices (this may take some time) .......")
		ret <- tryCatch(
			climdexCalc.TT(GeneralParameters),
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






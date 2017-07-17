
PICSAPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(45)
		largeur0 <- as.integer(w.scale(32)/sfont0)
		largeur1 <- as.integer(w.scale(30)/sfont0)
		largeur2 <- as.integer(w.scale(28)/sfont0)
	}else{
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(50)
		largeur0 <- as.integer(w.scale(25)/sfont0)
		largeur1 <- as.integer(w.scale(22)/sfont0)
		largeur2 <- as.integer(w.scale(23)/sfont0)
	}

	GeneralParameters <- fromJSON(file.path(apps.dir, 'init_params', 'PICSA.json'))
	MOIS <- format(ISOdate(2014, 1:12, 1), "%B")

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Output")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Rainy Season")
	cmd.tab4 <- bwAddTab(tknote.cmd, text = "Plot")
	cmd.tab5 <- bwAddTab(tknote.cmd, text = "Options")

	bwRaiseTab(tknote.cmd, cmd.tab1)
	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab3, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab4, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab5, 0, weight = 1)

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
	CbdatatypeVAL <- c('CDT data format', 'NetCDF gridded data')
	tclvalue(DataType) <- switch(GeneralParameters$data.type,
								'cdt' = CbdatatypeVAL[1], 
								'netcdf' = CbdatatypeVAL[2])

	cb.datatype <- ttkcombobox(frameData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)

	tkgrid(cb.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

	infobulle(cb.datatype, 'Select the type of input data')
	status.bar.display(cb.datatype, TextOutputVar, 'Select the type of input data')

	###############

	tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
		tkdestroy(cb.rain)
		tkdestroy(cb.tmax)
		tkdestroy(cb.tmin)
		tkdestroy(cb.etp)
		tkdestroy(cb.raindk)

		#######
		tclvalue(file.rain) <- ''
		tclvalue(file.tmax) <- ''
		tclvalue(file.tmin) <- ''
		tclvalue(file.etp) <- ''
		tclvalue(file.raindk) <- ''

		#######
		statedk <- if(tclvalue(use.dekmon) == '1') 'normal' else 'disabled'
		stateEtp <- if(tclvalue(ETPCalc) == 'From temperature data') 'disabled' else 'normal'
		stateTemp <- if(tclvalue(ETPCalc) == 'From temperature data') 'normal' else 'disabled'

		if(tclvalue(DataType) == 'CDT data format'){
			tclvalue(fileINdir.rain) <- 'File containing Rain data'
			tclvalue(fileINdir.tmax) <- 'File containing Tmax data'
			tclvalue(fileINdir.tmin) <- 'File containing Tmin data'
			tclvalue(fileINdir.etp) <- 'File containing PET data'
			tclvalue(fileINdir.raindk) <- 'File containing Rain data'

			#######
			cb.rain <- ttkcombobox(frameRain, values = unlist(listOpenFiles), textvariable = file.rain, width = largeur1)
			cb.tmax <- ttkcombobox(frameTemp, values = unlist(listOpenFiles), textvariable = file.tmax, width = largeur1, state = stateTemp)
			cb.tmin <- ttkcombobox(frameTemp, values = unlist(listOpenFiles), textvariable = file.tmin, width = largeur1, state = stateTemp)
			cb.etp <- ttkcombobox(frameETP, values = unlist(listOpenFiles), textvariable = file.etp, width = largeur1, state = stateEtp)
			cb.raindk <- ttkcombobox(frameRaindk, values = unlist(listOpenFiles), textvariable = file.raindk, width = largeur1, state = statedk)

			#######
			tkconfigure(bt.rain, command = function(){
				dat.opfiles <- getOpenFiles(main.win, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(file.rain) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.rain, values = unlist(listOpenFiles), textvariable = file.rain)
					tkconfigure(cb.tmax, values = unlist(listOpenFiles), textvariable = file.tmax)
					tkconfigure(cb.tmin, values = unlist(listOpenFiles), textvariable = file.tmin)
					tkconfigure(cb.etp, values = unlist(listOpenFiles), textvariable = file.etp)
					tkconfigure(cb.raindk, values = unlist(listOpenFiles), textvariable = file.raindk)
					tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = file.shp)
				}else return(NULL)
			})

			tkconfigure(bt.tmax, state = stateTemp, command = function(){
				dat.opfiles <- getOpenFiles(main.win, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(file.tmax) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.rain, values = unlist(listOpenFiles), textvariable = file.rain)
					tkconfigure(cb.tmax, values = unlist(listOpenFiles), textvariable = file.tmax)
					tkconfigure(cb.tmin, values = unlist(listOpenFiles), textvariable = file.tmin)
					tkconfigure(cb.etp, values = unlist(listOpenFiles), textvariable = file.etp)
					tkconfigure(cb.raindk, values = unlist(listOpenFiles), textvariable = file.raindk)
					tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = file.shp)
				}else return(NULL)
			})

			tkconfigure(bt.tmin, state = stateTemp, command = function(){
				dat.opfiles <- getOpenFiles(main.win, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(file.tmin) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.rain, values = unlist(listOpenFiles), textvariable = file.rain)
					tkconfigure(cb.tmax, values = unlist(listOpenFiles), textvariable = file.tmax)
					tkconfigure(cb.tmin, values = unlist(listOpenFiles), textvariable = file.tmin)
					tkconfigure(cb.etp, values = unlist(listOpenFiles), textvariable = file.etp)
					tkconfigure(cb.raindk, values = unlist(listOpenFiles), textvariable = file.raindk)
					tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = file.shp)
				}else return(NULL)
			})

			tkconfigure(bt.etp, state = stateEtp, command = function(){
				dat.opfiles <- getOpenFiles(main.win, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(file.etp) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.rain, values = unlist(listOpenFiles), textvariable = file.rain)
					tkconfigure(cb.tmax, values = unlist(listOpenFiles), textvariable = file.tmax)
					tkconfigure(cb.tmin, values = unlist(listOpenFiles), textvariable = file.tmin)
					tkconfigure(cb.etp, values = unlist(listOpenFiles), textvariable = file.etp)
					tkconfigure(cb.raindk, values = unlist(listOpenFiles), textvariable = file.raindk)
					tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = file.shp)
				}else return(NULL)
			})

			tkconfigure(bt.raindk, state = statedk, command = function(){
				dat.opfiles <- getOpenFiles(main.win, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(file.raindk) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.rain, values = unlist(listOpenFiles), textvariable = file.rain)
					tkconfigure(cb.tmax, values = unlist(listOpenFiles), textvariable = file.tmax)
					tkconfigure(cb.tmin, values = unlist(listOpenFiles), textvariable = file.tmin)
					tkconfigure(cb.etp, values = unlist(listOpenFiles), textvariable = file.etp)
					tkconfigure(cb.raindk, values = unlist(listOpenFiles), textvariable = file.raindk)
					tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = file.shp)
				}else return(NULL)
			})

			#######
			tkconfigure(set.rain, state = 'disabled')
			tkconfigure(set.tmax, state = 'disabled')
			tkconfigure(set.tmin, state = 'disabled')
			tkconfigure(set.etp, state = 'disabled')
			tkconfigure(set.raindk, state = 'disabled')

			#######
			infobulle(cb.rain, 'Select the file containing the daily rainfall data')
			status.bar.display(cb.rain, TextOutputVar, 'Select the file containing the daily rainfall data')
			infobulle(cb.tmax, 'Select the file containing the daily maximum temperature data')
			status.bar.display(cb.tmax, TextOutputVar, 'Select the file containing the daily maximum temperature data')
			infobulle(cb.tmin, 'Select the file containing the daily minimum temperature data')
			status.bar.display(cb.tmin, TextOutputVar, 'Select the file containing the daily minimum temperature data')
			infobulle(cb.etp, 'Select the file containing the daily potential evapotranspiration data')
			status.bar.display(cb.etp, TextOutputVar, 'Select the file containing the daily potential evapotranspiration data')
			infobulle(cb.raindk, 'Select the file containing the dekadal/monthly rainfall data')
			status.bar.display(cb.raindk, TextOutputVar, 'Select the file containing the dekadal/monthly rainfall data')
		}

		if(tclvalue(DataType) == 'NetCDF gridded data'){
			tclvalue(fileINdir.rain) <- 'Directory containing Rain data'
			tclvalue(fileINdir.tmax) <- 'Directory containing Tmax data'
			tclvalue(fileINdir.tmin) <- 'Directory containing Tmin data'
			tclvalue(fileINdir.etp) <- 'Directory containing PET data'
			tclvalue(fileINdir.raindk) <- 'Directory containing Rain data'

			#######
			cb.rain <- tkentry(frameRain, textvariable = file.rain, width = largeur2)
			cb.tmax <- tkentry(frameTemp, textvariable = file.tmax, width = largeur2, state = stateTemp)
			cb.tmin <- tkentry(frameTemp, textvariable = file.tmin, width = largeur2, state = stateTemp)
			cb.etp <- tkentry(frameETP, textvariable = file.etp, width = largeur2, state = stateEtp)
			cb.raindk <- tkentry(frameRaindk, textvariable = file.raindk, width = largeur2, state = statedk)

			#######
			tkconfigure(bt.rain, command = function(){
				file2convert <- tk_choose.dir(getwd(), "")
				tclvalue(file.rain) <- if(!is.na(file2convert)) file2convert else ""
			})
			tkconfigure(bt.tmax, state = stateTemp, command = function(){
				file2convert <- tk_choose.dir(getwd(), "")
				tclvalue(file.tmax) <- if(!is.na(file2convert)) file2convert else ""
			})
			tkconfigure(bt.tmin, state = stateTemp, command = function(){
				file2convert <- tk_choose.dir(getwd(), "")
				tclvalue(file.tmin) <- if(!is.na(file2convert)) file2convert else ""
			})
			tkconfigure(bt.etp, state = stateEtp, command = function(){
				file2convert <- tk_choose.dir(getwd(), "")
				tclvalue(file.etp) <- if(!is.na(file2convert)) file2convert else ""
			})
			tkconfigure(bt.raindk, state = statedk, command = function(){
				file2convert <- tk_choose.dir(getwd(), "")
				tclvalue(file.raindk) <- if(!is.na(file2convert)) file2convert else ""
			})

			#######

			tkconfigure(set.rain, state = 'normal', command = function(){
				GeneralParameters <<- PICSANetcdfData(main.win, GeneralParameters, str_trim(tclvalue(file.rain)), "rain")
			})

			stateTemp1 <- if(tclvalue(ETPCalc) == 'From temperature data') 'normal' else 'disabled'
			tkconfigure(set.tmax, state = stateTemp1, command = function(){
				GeneralParameters <<- PICSANetcdfData(main.win, GeneralParameters, str_trim(tclvalue(file.tmax)), "tmax")
			})
			tkconfigure(set.tmin, state = stateTemp1, command = function(){
				GeneralParameters <<- PICSANetcdfData(main.win, GeneralParameters, str_trim(tclvalue(file.tmin)), "tmin")
			})

			stateEtp1 <- if(tclvalue(ETPCalc) == 'From temperature data') 'disabled' else 'normal'
			# stateEtp1 <- if(tclvalue(use.etp) == '1') 'normal' else 'disabled'
			tkconfigure(set.etp, state = stateEtp1, command = function(){
				GeneralParameters <<- PICSANetcdfData(main.win, GeneralParameters, str_trim(tclvalue(file.etp)), "etp")
			})

			statedk1 <- if(tclvalue(use.dekmon) == '1') 'normal' else 'disabled'
			tkconfigure(set.raindk, state = statedk1, command = function(){
				GeneralParameters <<- PICSANetcdfData(main.win, GeneralParameters, str_trim(tclvalue(file.raindk)), "raindekmon")
			})

			#######
			infobulle(cb.rain, 'Enter the full path to directory containing the daily rainfall data')
			status.bar.display(cb.rain, TextOutputVar, 'Enter the full path to directory containing the daily rainfall data')
			infobulle(cb.tmax, 'Enter the full path to directory containing the daily maximum temperature data')
			status.bar.display(cb.tmax, TextOutputVar, 'Enter the full path to directory containing the daily maximum temperature data')
			infobulle(cb.tmin, 'Enter the full path to directory containing the daily minimum temperature data')
			status.bar.display(cb.tmin, TextOutputVar, 'Enter the full path to directory containing the daily minimum temperature data')
			infobulle(cb.etp, 'Enter the full path to directory containing the daily potential evapotranspiration data')
			status.bar.display(cb.etp, TextOutputVar, 'Enter the full path to directory containing the daily potential evapotranspiration data')
			infobulle(cb.raindk, 'Enter the full path to directory containing the dekadal/monthly rainfall data')
			status.bar.display(cb.raindk, TextOutputVar, 'Enter the full path to directory containing the dekadal/monthly rainfall data')
		}
		
		#######
		tkgrid(cb.rain, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(cb.tmax, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(cb.tmin, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(cb.etp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(cb.raindk, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		###############

		tkbind(chk.raindk, "<Button-1>", function(){
			statedk <- if(tclvalue(use.dekmon) == '0') 'normal' else 'disabled'
			if(tclvalue(DataType) == 'NetCDF gridded data'){
				statedk1 <- if(tclvalue(use.dekmon) == '0') 'normal' else 'disabled'
			}else statedk1 <- 'disabled'
			tkconfigure(cb.raindk, state = statedk)
			tkconfigure(bt.raindk, state = statedk)
			tkconfigure(set.raindk, state = statedk1)
		})

		tkbind(cb.etpcalc, "<<ComboboxSelected>>", function(){
			if(tclvalue(ETPCalc) == 'From temperature data'){
				stateTemp <- 'normal'
				stateTemp1 <- if(tclvalue(DataType) == 'NetCDF gridded data') 'normal' else 'disabled'
				stateEtp <- 'disabled'
				stateEtp1 <- 'disabled'
			}else{
				stateTemp <- 'disabled'
				stateTemp1 <- 'disabled'
				stateEtp <- 'normal'
				stateEtp1 <- if(tclvalue(DataType) == 'NetCDF gridded data') 'normal' else 'disabled'
			}

			tkconfigure(cb.tmax, state = stateTemp)
			tkconfigure(bt.tmax, state = stateTemp)
			tkconfigure(set.tmax, state = stateTemp1)

			tkconfigure(cb.tmin, state = stateTemp)
			tkconfigure(bt.tmin, state = stateTemp)
			tkconfigure(set.tmin, state = stateTemp1)

			tkconfigure(cb.etp, state = stateEtp)
			tkconfigure(bt.etp, state = stateEtp)
			tkconfigure(set.etp, state = stateEtp1)
		})
	})

	#############################

	frameRain <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

	file.rain <- tclVar(GeneralParameters$RAIN$dirORfile)
	fileINdir.rain <- tclVar('File containing Rain data')

	txt.rain <- tklabel(frameRain, text = tclvalue(fileINdir.rain), textvariable = fileINdir.rain, anchor = 'w', justify = 'left')
	set.rain <- tkbutton(frameRain, text = "Settings")
	cb.rain <- ttkcombobox(frameRain, values = unlist(listOpenFiles), textvariable = file.rain, width = largeur1)
	bt.rain <- tkbutton(frameRain, text = "...")

	###############
	if(tclvalue(DataType) == 'CDT data format'){
		tkconfigure(set.rain, state = 'disabled')
	}
	if(tclvalue(DataType) == 'NetCDF gridded data'){
		tkconfigure(set.rain, state = 'normal', command = function(){
			GeneralParameters <<- PICSANetcdfData(main.win, GeneralParameters, str_trim(tclvalue(file.rain)), "rain")
		})
	}

	tkconfigure(bt.rain, command = function(){
		dat.opfiles <- getOpenFiles(main.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.rain) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.rain, values = unlist(listOpenFiles), textvariable = file.rain)
			tkconfigure(cb.tmax, values = unlist(listOpenFiles), textvariable = file.tmax)
			tkconfigure(cb.tmin, values = unlist(listOpenFiles), textvariable = file.tmin)
			tkconfigure(cb.etp, values = unlist(listOpenFiles), textvariable = file.etp)
			tkconfigure(cb.raindk, values = unlist(listOpenFiles), textvariable = file.raindk)
			tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = file.shp)
		}else return(NULL)
	})

	tkgrid(txt.rain, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.rain, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.rain, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.rain, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.rain, 'Select the file containing the daily rainfall data')
	status.bar.display(cb.rain, TextOutputVar, 'Select the file containing the daily rainfall data')
	infobulle(bt.rain, 'Browse file if not listed')
	status.bar.display(bt.rain, TextOutputVar, 'Browse file if not listed')
	infobulle(set.rain, 'Setting netcdf data options')
	status.bar.display(set.rain, TextOutputVar, 'Setting netcdf data options')

	#############################

	frameETPChoix <- ttklabelframe(subfr1, text = "Compute potential evapotranspiration", relief = 'groove')

	ETPCalc <- tclVar()
	ETPChoix <- c('From temperature data', 'Already calculated')
	tclvalue(ETPCalc) <- switch(GeneralParameters$compute.ETP,
								'temp' = ETPChoix[1], 
								'etp' = ETPChoix[2])

	cb.etpcalc <- ttkcombobox(frameETPChoix, values = ETPChoix, textvariable = ETPCalc, width = largeur0)

	tkgrid(cb.etpcalc, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

	infobulle(cb.etpcalc, 'Compute potential evapotranspiration from temperature data\nor import if it has already been calculated from another sources')
	status.bar.display(cb.etpcalc, TextOutputVar, 'Compute potential evapotranspiration from temperature data\nor import if it has already been calculated from another sources')

	###############

	tkbind(cb.etpcalc, "<<ComboboxSelected>>", function(){
		varPICSA.val <- c("Onset", "Cessation", "Season Length", "Seasonal Rainfall Amounts",
						"Dry Spells", "Longest Dry Spell",
						"Number of rain day", "Maximum daily rain",
						"Total rain when RR>95thPerc", "Nb of day when RR>95thPerc")

		if(tclvalue(ETPCalc) == 'From temperature data'){
			stateTemp <- 'normal'
			stateTemp1 <- if(tclvalue(DataType) == 'NetCDF gridded data') 'normal' else 'disabled'
			stateEtp <- 'disabled'
			stateEtp1 <- 'disabled'
			varPICSA.val <- c(varPICSA.val, "Maximum temperature", "Minimum temperature")
		}else{
			stateTemp <- 'disabled'
			stateTemp1 <- 'disabled'
			stateEtp <- 'normal'
			stateEtp1 <- if(tclvalue(DataType) == 'NetCDF gridded data') 'normal' else 'disabled'
			if(tclvalue(EnvPICSAplot$varPICSA) %in% c("Maximum temperature", "Minimum temperature")) tclvalue(EnvPICSAplot$varPICSA) <- "Onset"
		}

		tkconfigure(cb.TsMap.picsavar, values = varPICSA.val)

		tkconfigure(cb.tmax, state = stateTemp)
		tkconfigure(bt.tmax, state = stateTemp)
		tkconfigure(set.tmax, state = stateTemp1)

		tkconfigure(cb.tmin, state = stateTemp)
		tkconfigure(bt.tmin, state = stateTemp)
		tkconfigure(set.tmin, state = stateTemp1)

		tkconfigure(cb.etp, state = stateEtp)
		tkconfigure(bt.etp, state = stateEtp)
		tkconfigure(set.etp, state = stateEtp1)
	})

	#############################

	frameTemp <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

	file.tmax <- tclVar(GeneralParameters$TMAX$dirORfile)
	fileINdir.tmax <- tclVar('File containing Tmax data')
	file.tmin <- tclVar(GeneralParameters$TMIN$dirORfile)
	fileINdir.tmin <- tclVar('File containing Tmin data')

	if(GeneralParameters$compute.ETP == 'temp'){
		stateTemp <- 'normal'
		stateTemp1 <- if(GeneralParameters$data.type == "netcdf") 'normal' else 'disabled'
	}else{
		stateTemp <- 'disabled'
		stateTemp1 <- 'disabled'
	}

	txt.tmax <- tklabel(frameTemp, text = tclvalue(fileINdir.tmax), textvariable = fileINdir.tmax, anchor = 'w', justify = 'left')
	set.tmax <- tkbutton(frameTemp, text = "Settings", state = stateTemp1)
	cb.tmax <- ttkcombobox(frameTemp, values = unlist(listOpenFiles), textvariable = file.tmax, width = largeur1, state = stateTemp)
	bt.tmax <- tkbutton(frameTemp, text = "...", state = stateTemp)

	txt.tmin <- tklabel(frameTemp, text = tclvalue(fileINdir.tmin), textvariable = fileINdir.tmin, anchor = 'w', justify = 'left')
	set.tmin <- tkbutton(frameTemp, text = "Settings", state = stateTemp1)
	cb.tmin <- ttkcombobox(frameTemp, values = unlist(listOpenFiles), textvariable = file.tmin, width = largeur1, state = stateTemp)
	bt.tmin <- tkbutton(frameTemp, text = "...", state = stateTemp)

	###############

	if(tclvalue(DataType) == 'NetCDF gridded data'){
		tkconfigure(set.tmax, command = function(){
			GeneralParameters <<- PICSANetcdfData(main.win, GeneralParameters, str_trim(tclvalue(file.tmax)), "tmax")
		})
	}

	tkconfigure(bt.tmax, command = function(){
		dat.opfiles <- getOpenFiles(main.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.tmax) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.rain, values = unlist(listOpenFiles), textvariable = file.rain)
			tkconfigure(cb.tmax, values = unlist(listOpenFiles), textvariable = file.tmax)
			tkconfigure(cb.tmin, values = unlist(listOpenFiles), textvariable = file.tmin)
			tkconfigure(cb.etp, values = unlist(listOpenFiles), textvariable = file.etp)
			tkconfigure(cb.raindk, values = unlist(listOpenFiles), textvariable = file.raindk)
			tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = file.shp)
		}else return(NULL)
	})

	if(tclvalue(DataType) == 'NetCDF gridded data'){
		tkconfigure(set.tmin, command = function(){
			GeneralParameters <<- PICSANetcdfData(main.win, GeneralParameters, str_trim(tclvalue(file.tmin)), "tmin")
		})
	}

	tkconfigure(bt.tmin, command = function(){
		dat.opfiles <- getOpenFiles(main.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.tmin) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.rain, values = unlist(listOpenFiles), textvariable = file.rain)
			tkconfigure(cb.tmax, values = unlist(listOpenFiles), textvariable = file.tmax)
			tkconfigure(cb.tmin, values = unlist(listOpenFiles), textvariable = file.tmin)
			tkconfigure(cb.etp, values = unlist(listOpenFiles), textvariable = file.etp)
			tkconfigure(cb.raindk, values = unlist(listOpenFiles), textvariable = file.raindk)
			tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = file.shp)
		}else return(NULL)
	})

	tkgrid(txt.tmax, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.tmax, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.tmax, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.tmax, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	tkgrid(txt.tmin, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.tmin, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.tmin, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.tmin, row = 3, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.tmax, 'Select the file containing the daily maximum temperature data')
	status.bar.display(cb.tmax, TextOutputVar, 'Select the file containing the daily maximum temperature data')
	infobulle(bt.tmax, 'Browse file if not listed')
	status.bar.display(bt.tmax, TextOutputVar, 'Browse file if not listed')
	infobulle(set.tmax, 'Setting netcdf data options')
	status.bar.display(set.tmax, TextOutputVar, 'Setting netcdf data options')

	infobulle(cb.tmin, 'Select the file containing the daily minimum temperature data')
	status.bar.display(cb.tmin, TextOutputVar, 'Select the file containing the daily minimum temperature data')
	infobulle(bt.tmin, 'Browse file if not listed')
	status.bar.display(bt.tmin, TextOutputVar, 'Browse file if not listed')
	infobulle(set.tmin, 'Setting netcdf data options')
	status.bar.display(set.tmin, TextOutputVar, 'Setting netcdf data options')

	#############################

	frameETP <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

	file.etp <- tclVar(GeneralParameters$ETP$dirORfile)
	fileINdir.etp <- tclVar('File containing PET data')

	if(GeneralParameters$compute.ETP == 'etp'){
		stateEtp <- 'normal'
		stateEtp1 <- if(GeneralParameters$data.type == "netcdf") 'normal' else 'disabled'
	}else{
		stateEtp <- 'disabled'
		stateEtp1 <- 'disabled'
	}

	txt.etp <- tklabel(frameETP, text = tclvalue(fileINdir.etp), textvariable = fileINdir.etp, anchor = 'w', justify = 'left')
	set.etp <- tkbutton(frameETP, text = "Settings", state = stateEtp1)
	cb.etp <- ttkcombobox(frameETP, values = unlist(listOpenFiles), textvariable = file.etp, width = largeur1, state = stateEtp)
	bt.etp <- tkbutton(frameETP, text = "...", state = stateEtp)

	###############

	if(tclvalue(DataType) == 'NetCDF gridded data'){
		tkconfigure(set.etp, command = function(){
			GeneralParameters <<- PICSANetcdfData(main.win, GeneralParameters, str_trim(tclvalue(file.etp)), "etp")
		})
	}

	tkconfigure(bt.etp, command = function(){
		dat.opfiles <- getOpenFiles(main.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.etp) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.rain, values = unlist(listOpenFiles), textvariable = file.rain)
			tkconfigure(cb.tmax, values = unlist(listOpenFiles), textvariable = file.tmax)
			tkconfigure(cb.tmin, values = unlist(listOpenFiles), textvariable = file.tmin)
			tkconfigure(cb.etp, values = unlist(listOpenFiles), textvariable = file.etp)
			tkconfigure(cb.raindk, values = unlist(listOpenFiles), textvariable = file.raindk)
			tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = file.shp)
		}else return(NULL)
	})

	tkgrid(txt.etp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.etp, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.etp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.etp, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.etp, 'Select the file containing the daily potential evapotranspiration data')
	status.bar.display(cb.etp, TextOutputVar, 'Select the file containing the daily potential evapotranspiration data')
	infobulle(bt.etp, 'Browse file if not listed')
	status.bar.display(bt.etp, TextOutputVar, 'Browse file if not listed')
	infobulle(set.etp, 'Setting netcdf data options')
	status.bar.display(set.etp, TextOutputVar, 'Setting netcdf data options')

	#############################
	tkgrid(frameData, row = 0, column = 0, sticky = 'we')
	tkgrid(frameRain, row = 1, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameETPChoix, row = 2, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameTemp, row = 3, column = 0, sticky = 'we', pady = 1)
	tkgrid(frameETP, row = 4, column = 0, sticky = 'we', pady = 1)

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

	#######################

	frameYear <- ttklabelframe(subfr2, text = "Years to process", relief = 'groove')

	allYears <- tclVar(GeneralParameters$date.range$all.years)
	startYear <- tclVar(GeneralParameters$date.range$start.year)
	endYear <- tclVar(GeneralParameters$date.range$end.year)

	stateYr <- if(GeneralParameters$date.range$all.years) 'disabled' else 'normal'

	chk.allYears <- tkcheckbutton(frameYear, variable = allYears, text =  "Use all years from the input data", anchor = 'w', justify = 'left', width = largeur2+2)
	txt.startYear <- tklabel(frameYear, text = "Start Year",  anchor = 'e', justify = 'right')
	en.startYear <- tkentry(frameYear, textvariable = startYear, width = 6, state = stateYr)
	txt.endYear <- tklabel(frameYear, text = "End Year",  anchor = 'e', justify = 'right')
	en.endYear <- tkentry(frameYear, textvariable = endYear, width = 6, state = stateYr)

	###############
	tkgrid(chk.allYears, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.startYear, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.startYear, row = 1, column = 2, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.endYear, row = 1, column = 3, sticky = 'e', rowspan = 1, columnspan = 3, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.endYear, row = 1, column = 6, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(chk.allYears, "Check this box to use all years from the input data")
	status.bar.display(chk.allYears, TextOutputVar, "Check this box to use all years from the input data")
	infobulle(en.startYear, "Enter the start year of the period to analyze")
	status.bar.display(en.startYear, TextOutputVar, "Enter the start year of the period to analyze")
	infobulle(en.endYear, "Enter the end year of the period to analyze")
	status.bar.display(en.endYear, TextOutputVar, "Enter the end year of the period to analyze")

	###############
	tkbind(chk.allYears, "<Button-1>", function(){
		stateYr <- if(tclvalue(allYears) == '1') 'normal' else 'disabled'
		tkconfigure(en.startYear, state = stateYr)
		tkconfigure(en.endYear, state = stateYr)
	})

	#############################

	frameRaindk <- ttklabelframe(subfr2, text = "Seasonal rainfall amounts data", relief = 'groove')


	use.dekmon <- tclVar(GeneralParameters$dekmon$use.dekmon)
	file.raindk <- tclVar(GeneralParameters$dekmon$dirORfile)
	fileINdir.raindk <- tclVar('File containing Rain data')

	statedk <- if(GeneralParameters$dekmon$use.dekmon & GeneralParameters$data.type == "netcdf") 'normal' else 'disabled'
	
	if(GeneralParameters$data.type == "netcdf"){
		statedk1 <- if(tclvalue(use.dekmon) == "1") 'normal' else 'disabled'
	}else statedk1 <- 'disabled'

	chk.raindk <- tkcheckbutton(frameRaindk, variable = use.dekmon, text =  "Use DEK/MON data for seasonal total", anchor = 'w', justify = 'left')
	txt.raindk <- tklabel(frameRaindk, text = tclvalue(fileINdir.raindk), textvariable = fileINdir.raindk, anchor = 'w', justify = 'left')
	set.raindk <- tkbutton(frameRaindk, text = "Settings", state = statedk1)
	cb.raindk <- ttkcombobox(frameRaindk, values = unlist(listOpenFiles), textvariable = file.raindk, width = largeur1, state = statedk)
	bt.raindk <- tkbutton(frameRaindk, text = "...", state = statedk)

	###############

	if(tclvalue(DataType) == 'NetCDF gridded data'){
		statedk1 <- if(tclvalue(use.dekmon) == "1") 'normal' else 'disabled'
		tkconfigure(set.raindk, command = function(){
			GeneralParameters <<- PICSANetcdfData(main.win, GeneralParameters, str_trim(tclvalue(file.raindk)), "raindekmon")
		})
	}

	tkconfigure(bt.raindk, command = function(){
		dat.opfiles <- getOpenFiles(main.win, all.opfiles)
		if(!is.null(dat.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'ascii'
			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.raindk) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.rain, values = unlist(listOpenFiles), textvariable = file.rain)
			tkconfigure(cb.tmax, values = unlist(listOpenFiles), textvariable = file.tmax)
			tkconfigure(cb.tmin, values = unlist(listOpenFiles), textvariable = file.tmin)
			tkconfigure(cb.etp, values = unlist(listOpenFiles), textvariable = file.etp)
			tkconfigure(cb.raindk, values = unlist(listOpenFiles), textvariable = file.raindk)
			tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = file.shp)
		}else return(NULL)
	})

	tkgrid(chk.raindk, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.raindk, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.raindk, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.raindk, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.raindk, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(chk.raindk, 'Check this box to use dekadal or monthly rainfall data\nto calculate seasonal amounts')
	status.bar.display(chk.raindk, TextOutputVar, 'Check this box to use dekadal or monthly rainfall data\nto calculate seasonal amounts')
	infobulle(cb.raindk, 'Select the file containing the dekadal/monthly rainfall data')
	status.bar.display(cb.raindk, TextOutputVar, 'Select the file containing the dekadal/monthly rainfall data')
	infobulle(bt.raindk, 'Browse file if not listed')
	status.bar.display(bt.raindk, TextOutputVar, 'Browse file if not listed')
	infobulle(set.raindk, 'Setting netcdf data options')
	status.bar.display(set.raindk, TextOutputVar, 'Setting netcdf data options')

	###############
	tkbind(chk.raindk, "<Button-1>", function(){
		statedk <- if(tclvalue(use.dekmon) == '0') 'normal' else 'disabled'
		if(tclvalue(DataType) == 'NetCDF gridded data'){
			statedk1 <- if(tclvalue(use.dekmon) == '0') 'normal' else 'disabled'
		}else statedk1 <- 'disabled'
		tkconfigure(cb.raindk, state = statedk)
		tkconfigure(bt.raindk, state = statedk)
		tkconfigure(set.raindk, state = statedk1)
	})


	#############################

	frameDirSav <- tkframe(subfr2, relief = 'groove', borderwidth = 2)

	file.save1 <- tclVar(GeneralParameters$Outdir)

	txt.file.save <- tklabel(frameDirSav, text = "Directory to save outputs",  anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frameDirSav, textvariable = file.save1, width = largeur2)
	bt.file.save <- tkbutton(frameDirSav, text = "...")

	##############
	tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save1, isFile = FALSE))

	##############
	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.file.save, 'Enter the full path to directory to put the outputs')
	status.bar.display(en.file.save, TextOutputVar, 'Enter the full path to directory to put the outputs')
	infobulle(bt.file.save, 'or browse here')
	status.bar.display(bt.file.save, TextOutputVar, 'or browse here')

	#############################
	tkgrid(frameYear, row = 0, column = 0, sticky = 'we')
	tkgrid(frameRaindk, row = 1, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', pady = 3)
	# tkgrid(frameAggr, row = 2, column = 0, sticky = 'we', pady = 3)


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

	#######################

	frameOnset <- ttklabelframe(subfr3, text = "Onset definition", relief = 'groove')

	mon1 <- as.numeric(str_trim(GeneralParameters$onset$early.month))
	onset.start.mon <- tclVar(MOIS[mon1])
	onset.start.day <- tclVar(GeneralParameters$onset$early.day)
	thres.rain.day <- tclVar(GeneralParameters$onset$thres.rain.day)
	total.rain <- tclVar(GeneralParameters$onset$rain.total)
	# onset.nbday.search <- GeneralParameters$onset$first.win.search
	# dry.spell <- GeneralParameters$onset$dry.spell
	# dry.spell.win <- GeneralParameters$onset$dry.spell.win.search
	mon2 <- as.numeric(str_trim(GeneralParameters$onset$late.month))
	onset.late.mon <- tclVar(MOIS[mon2])
	onset.late.day <- tclVar(GeneralParameters$onset$late.day)

	frameOnset1 <- tkframe(frameOnset)
	txt.onset1a <- tklabel(frameOnset1, text = "Earliest onset date", anchor = 'w', justify = 'left')
	cb.onset1a <- ttkcombobox(frameOnset1, values = MOIS, textvariable = onset.start.mon, width = 9)
	txt.onset1b <- tklabel(frameOnset1, text = "Day", anchor = 'w', justify = 'left')
	cb.onset1b <- ttkcombobox(frameOnset1, values = 1:31, textvariable = onset.start.day, width = 2)

	frameOnset2 <- tkframe(frameOnset)
	txt.onset2a <- tklabel(frameOnset2, text = "Threshold for a rainy day", anchor = 'w', justify = 'left')
	en.onset2a <- tkentry(frameOnset2, textvariable = thres.rain.day, width = 4)
	txt.onset2b <- tklabel(frameOnset2, text = "mm", anchor = 'w', justify = 'left')

	frameOnset3a <- tkframe(frameOnset)
	txt.onset3a <- tklabel(frameOnset3a, text = "Rainfall total", anchor = 'w', justify = 'left')
	en.onset3a <- tkentry(frameOnset3a, textvariable = total.rain, width = 4)
	txt.onset3b <- tklabel(frameOnset3a, text = "mm", anchor = 'w', justify = 'left')
	txt.onset3c <- tklabel(frameOnset3a, text = "over", anchor = 'w', justify = 'left')
	spin.onset3a <- ttkspinbox(frameOnset3a, from = 1, to = 30, increment = 1, justify = 'center', width = 2)
	txt.onset3d <- tklabel(frameOnset3a, text = "days", anchor = 'w', justify = 'left')
	tkset(spin.onset3a, GeneralParameters$onset$first.win.search)

	frameOnset3b <- tkframe(frameOnset)
	txt.onset3e <- tklabel(frameOnset3b, text = "with at least", anchor = 'w', justify = 'left')
	spin.onset3b <- ttkspinbox(frameOnset3b, from = 1, to = 30, increment = 1, justify = 'center', width = 2)
	txt.onset3f <- tklabel(frameOnset3b, text = "rain days", anchor = 'w', justify = 'left')
	tkset(spin.onset3b, GeneralParameters$onset$min.rain.day)

	frameOnset4a <- tkframe(frameOnset)
	txt.onset4a <- tklabel(frameOnset4a, text = "Dry Spell not exceeding", anchor = 'w', justify = 'left')
	spin.onset4a <- ttkspinbox(frameOnset4a, from = 1, to = 60, increment = 1, justify = 'center', width = 2)
	txt.onset4b <- tklabel(frameOnset4a, text = "days", anchor = 'w', justify = 'left')
	tkset(spin.onset4a, GeneralParameters$onset$dry.spell)

	frameOnset4b <- tkframe(frameOnset)
	txt.onset4c <- tklabel(frameOnset4b, text = "in the next", anchor = 'w', justify = 'left')
	spin.onset4b <- ttkspinbox(frameOnset4b, from = 1, to = 60, increment = 1, justify = 'center', width = 2)
	txt.onset4d <- tklabel(frameOnset4b, text = "days", anchor = 'w', justify = 'left')
	tkset(spin.onset4b, GeneralParameters$onset$dry.spell.win.search)

	frameOnset5 <- tkframe(frameOnset)
	txt.onset5a <- tklabel(frameOnset5, text = "Latest possible date", anchor = 'w', justify = 'left')
	cb.onset5a <- ttkcombobox(frameOnset5, values = MOIS, textvariable = onset.late.mon, width = 9)
	txt.onset5b <- tklabel(frameOnset5, text = "Day", anchor = 'w', justify = 'left')
	cb.onset5b <- ttkcombobox(frameOnset5, values = 1:31, textvariable = onset.late.day, width = 2)


	tkgrid(txt.onset1a, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.onset1a, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.onset1b, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.onset1b, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.onset2a, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.onset2a, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.onset2b, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.onset3a, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.onset3a, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.onset3b, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.onset3c, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(spin.onset3a, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.onset3d, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.onset3e, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(spin.onset3b, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.onset3f, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.onset4a, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(spin.onset4a, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.onset4b, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.onset4c, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(spin.onset4b, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.onset4d, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.onset5a, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.onset5a, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.onset5b, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.onset5b, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(frameOnset1, row = 0, sticky = 'we')
	tkgrid(frameOnset2, row = 1, sticky = 'we')
	tkgrid(frameOnset3a, row = 2, sticky = 'we')
	tkgrid(frameOnset3b, row = 3, sticky = 'we')
	tkgrid(frameOnset4a, row = 4, sticky = 'we')
	tkgrid(frameOnset4b, row = 5, sticky = 'we')
	tkgrid(frameOnset5, row = 6, sticky = 'we')

	#######################

	frameCessat <- ttklabelframe(subfr3, text = "Cessation definition", relief = 'groove')

	mon1a <- as.numeric(str_trim(GeneralParameters$cessation$early.month))
	cess.start.mon <- tclVar(MOIS[mon1a])
	cess.start.day <- tclVar(GeneralParameters$cessation$early.day)
	swh.capacity <- tclVar(GeneralParameters$cessation$swh.capacity)
	min.waterb <- tclVar(GeneralParameters$cessation$min.water.balance)
	# cess.nbday.search <- GeneralParameters$cessation$first.win.search
	mon2a <- as.numeric(str_trim(GeneralParameters$cessation$late.month))
	cess.late.mon <- tclVar(MOIS[mon2a])
	cess.late.day <- tclVar(GeneralParameters$cessation$late.day)

	frameCessat1 <- tkframe(frameCessat)
	txt.cessat1a <- tklabel(frameCessat1, text = "Earliest cessation date", anchor = 'w', justify = 'left')
	cb.cessat1a <- ttkcombobox(frameCessat1, values = MOIS, textvariable = cess.start.mon, width = 8)
	txt.cessat1b <- tklabel(frameCessat1, text = "Day", anchor = 'w', justify = 'left')
	cb.cessat1b <- ttkcombobox(frameCessat1, values = 1:31, textvariable = cess.start.day, width = 2)

	frameCessat2 <- tkframe(frameCessat)
	txt.cessat2a <- tklabel(frameCessat2, text = "Soil water holding capacity", anchor = 'w', justify = 'left')
	en.cessat2a <- tkentry(frameCessat2, textvariable = swh.capacity, width = 4)
	txt.cessat2b <- tklabel(frameCessat2, text = "mm", anchor = 'w', justify = 'left')

	frameCessat3a <- tkframe(frameCessat)
	txt.cessat3a <- tklabel(frameCessat3a, text = "Water Balance drops below", anchor = 'w', justify = 'left')
	en.cessat3a <- tkentry(frameCessat3a, textvariable = min.waterb, width = 4)
	txt.cessat3b <- tklabel(frameCessat3a, text = "mm", anchor = 'w', justify = 'left')

	frameCessat3b <- tkframe(frameCessat)
	txt.cessat3c <- tklabel(frameCessat3b, text = "For a period of", anchor = 'w', justify = 'left')
	spin.cessat3a <- ttkspinbox(frameCessat3b, from = 0, to = 30, increment = 1, justify = 'center', width = 2)
	txt.cessat3d <- tklabel(frameCessat3b, text = "days", anchor = 'w', justify = 'left')
	tkset(spin.cessat3a, GeneralParameters$cessation$first.win.search)

	frameCessat4 <- tkframe(frameCessat)
	txt.cessat4a <- tklabel(frameCessat4, text = "Latest possible date", anchor = 'w', justify = 'left')
	cb.cessat4a <- ttkcombobox(frameCessat4, values = MOIS, textvariable = cess.late.mon, width = 9)
	txt.cessat4b <- tklabel(frameCessat4, text = "Day", anchor = 'w', justify = 'left')
	cb.cessat4b <- ttkcombobox(frameCessat4, values = 1:31, textvariable = cess.late.day, width = 2)

	tkgrid(txt.cessat1a, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.cessat1a, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.cessat1b, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.cessat1b, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.cessat2a, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.cessat2a, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.cessat2b, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.cessat3a, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.cessat3a, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.cessat3b, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.cessat3c, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(spin.cessat3a, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.cessat3d, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.cessat4a, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.cessat4a, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.cessat4b, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.cessat4b, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)


	tkgrid(frameCessat1, row = 0, sticky = 'we')
	tkgrid(frameCessat2, row = 1, sticky = 'we')
	tkgrid(frameCessat3a, row = 2, sticky = 'we')
	tkgrid(frameCessat3b, row = 3, sticky = 'we')
	tkgrid(frameCessat4, row = 4, sticky = 'we')

	#######################

	stateCalc <- if(GeneralParameters$picsa.out$deja.calc) 'disabled' else 'normal'

	bt.CalculateOnset <- ttkbutton(subfr3, text = "Calculate", state = stateCalc)

	####
	tkconfigure(bt.CalculateOnset, command = function(){
		GeneralParameters$data.type <- switch(tclvalue(DataType),
												'CDT data format' = 'cdt',
												'NetCDF gridded data' = 'netcdf')
		GeneralParameters$compute.ETP <- switch(tclvalue(ETPCalc),
												'From temperature data' = 'temp',
												'Already calculated' = 'etp')

		GeneralParameters$RAIN$dirORfile <- str_trim(tclvalue(file.rain))
		GeneralParameters$TMAX$dirORfile <- str_trim(tclvalue(file.tmax))
		GeneralParameters$TMIN$dirORfile <- str_trim(tclvalue(file.tmin))
		# GeneralParameters$ETP$use.etp <- switch(tclvalue(use.etp), '0' = FALSE, '1' = TRUE)
		GeneralParameters$ETP$dirORfile <- str_trim(tclvalue(file.etp))
		GeneralParameters$Outdir <- str_trim(tclvalue(file.save1))

		GeneralParameters$date.range$all.years <- switch(tclvalue(allYears), '0' = FALSE, '1' = TRUE)
		GeneralParameters$date.range$start.year <- as.numeric(str_trim(tclvalue(startYear)))
		GeneralParameters$date.range$end.year <- as.numeric(str_trim(tclvalue(endYear)))

		GeneralParameters$dekmon$use.dekmon <- switch(tclvalue(use.dekmon), '0' = FALSE, '1' = TRUE)
		GeneralParameters$dekmon$dirORfile <- str_trim(tclvalue(file.raindk))

		GeneralParameters$onset$early.month <- which(MOIS%in%str_trim(tclvalue(onset.start.mon)))
		GeneralParameters$onset$early.day <- as.numeric(str_trim(tclvalue(onset.start.day)))
		GeneralParameters$onset$thres.rain.day <- as.numeric(str_trim(tclvalue(thres.rain.day)))
		GeneralParameters$onset$rain.total <- as.numeric(str_trim(tclvalue(total.rain)))
		GeneralParameters$onset$first.win.search <- as.numeric(str_trim(tclvalue(tkget(spin.onset3a))))
		GeneralParameters$onset$min.rain.day <- as.numeric(str_trim(tclvalue(tkget(spin.onset3b))))

		GeneralParameters$onset$dry.spell <- as.numeric(str_trim(tclvalue(tkget(spin.onset4a))))
		GeneralParameters$onset$dry.spell.win.search <- as.numeric(str_trim(tclvalue(tkget(spin.onset4b))))
		GeneralParameters$onset$late.month <- which(MOIS%in%str_trim(tclvalue(onset.late.mon)))
		GeneralParameters$onset$late.day <- as.numeric(str_trim(tclvalue(onset.late.day)))

		GeneralParameters$cessation$early.month <- which(MOIS%in%str_trim(tclvalue(cess.start.mon)))
		GeneralParameters$cessation$early.day <- as.numeric(str_trim(tclvalue(cess.start.day)))
		GeneralParameters$cessation$swh.capacity <- as.numeric(str_trim(tclvalue(swh.capacity)))
		GeneralParameters$cessation$min.water.balance <- as.numeric(str_trim(tclvalue(min.waterb)))
		GeneralParameters$cessation$first.win.search <- as.numeric(str_trim(tclvalue(tkget(spin.cessat3a))))
		GeneralParameters$cessation$late.month <- which(MOIS%in%str_trim(tclvalue(cess.late.mon)))
		GeneralParameters$cessation$late.day <- as.numeric(str_trim(tclvalue(cess.late.day)))

		# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

		tkconfigure(main.win, cursor = 'watch')
		InsertMessagesTxt(main.txt.out, paste("Calculating onset & cessation......."))
		ret <- tryCatch(
			PICSAProcs(GeneralParameters),
			#warning = function(w) warningFun(w),
			error = function(e){
				errorFun(e)
			},
			finally = {
				tkconfigure(main.win, cursor = '')
			}
		)

		if(!is.null(ret)){
			if(ret == 0){
				InsertMessagesTxt(main.txt.out, "Calculation finished successfully")

				PICSADATA <<- TRUE
				EnvPICSAplot$directory <- file.path(GeneralParameters$Outdir, "PICSA.OUT.Data")

				statexyLoc <- if(tclvalue(DataType) == 'NetCDF gridded data') "normal" else "disabled"
				stateStnID <- if(tclvalue(DataType) == 'CDT data format') "normal" else "disabled"
				tkconfigure(cb.stnID, state = stateStnID)
				tkconfigure(en.lonLoc, state = statexyLoc)
				tkconfigure(en.latLoc, state = statexyLoc)

				range.TsMap.year <- range(EnvPICSA$cdtONSET$years)
				tkconfigure(EnvPICSAplot$spin.TsMap.year, from = range.TsMap.year[1], to = range.TsMap.year[2])
				tkset(EnvPICSAplot$spin.TsMap.year, range.TsMap.year[2])

				stnIDTSPLOT <- if(tclvalue(DataType) == 'CDT data format') EnvPICSA$PICSA.Coords$id else ""
				tkconfigure(cb.stnID, values = stnIDTSPLOT)
				tclvalue(EnvPICSAplot$stnIDTSp) <- stnIDTSPLOT[1]

			}else{
				InsertMessagesTxt(main.txt.out, "Calculation failed", format = TRUE)
			}
		}else{
			InsertMessagesTxt(main.txt.out, "Calculation failed", format = TRUE)
		}
	})

	#############################
	tkgrid(frameOnset, row = 0, column = 0, sticky = 'we')
	tkgrid(frameCessat, row = 1, column = 0, sticky = 'we', pady = 3)
	tkgrid(bt.CalculateOnset, row = 2, column = 0, sticky = 'we', pady = 3)

	#######################################################################################################

	#Tab4
	frTab4 <- tkframe(cmd.tab4)
	tkgrid(frTab4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab4, 0, weight = 1)

	scrw4 <- bwScrolledWindow(frTab4)
	tkgrid(scrw4)
	tkgrid.columnconfigure(scrw4, 0, weight = 1)
	subfr4 <- bwScrollableFrame(scrw4, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr4, 0, weight = 1)

	#######################

	framePicsaOut <- ttklabelframe(subfr4, text = "Onset & Cessation Data", relief = 'groove')

	dejaCalc <- tclVar(GeneralParameters$picsa.out$deja.calc)
	picsaData <- tclVar(GeneralParameters$picsa.out$picsa.data)

	statePicOut <- if(GeneralParameters$picsa.out$deja.calc) 'normal' else 'disabled'

	chk.picsaout <- tkcheckbutton(framePicsaOut, variable = dejaCalc, text =  "Onset & Cessation are already calculated", anchor = 'w', justify = 'left')
	en.picsaout <- tkentry(framePicsaOut, textvariable = picsaData, width = largeur2, state = statePicOut)
	bt.picsaout <- tkbutton(framePicsaOut, text = "...", state = statePicOut)

	tkconfigure(bt.picsaout, command = function(){
		filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
		loadPiscaData <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
		if(loadPiscaData == "") return(NULL)
		tclvalue(picsaData) <- loadPiscaData

		if(!PICSADATA){
			picsa <- try(loadPICSA.Data(), silent = TRUE)
			if(inherits(picsa, "try-error")){
				InsertMessagesTxt(main.txt.out, 'Unable to load PICSA outputs data', format = TRUE)
				InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', picsa[1]), format = TRUE)
				return(NULL)
			}
			PICSADATA <<- picsa
		} 
	})

	tkgrid(chk.picsaout, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.picsaout, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.picsaout, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(chk.picsaout, "Check this box if the onset and cessation data are already calculated")
	status.bar.display(chk.picsaout, TextOutputVar, "Check this box if the onset and cessation data are already calculated")
	infobulle(en.picsaout, "Enter the full path to the file 'PICSA.DATA.RData' containing the PICSA outputs data")
	status.bar.display(en.picsaout, TextOutputVar, "Enter the full path to the file 'PICSA.DATA.RData' containing the PICSA outputs data")
	infobulle(bt.picsaout, "Enter the full path to the file 'PICSA.DATA.RData' containing the PICSA outputs data")
	status.bar.display(bt.picsaout, TextOutputVar, "Enter the full path to the file 'PICSA.DATA.RData' containing the PICSA outputs data")

	###############
	tkbind(chk.picsaout, "<Button-1>", function(){
		statePicOut <- if(tclvalue(dejaCalc) == '1') 'disabled' else 'normal'
		tkconfigure(en.picsaout, state = statePicOut)
		tkconfigure(bt.picsaout, state = statePicOut)
		stateCalc <- if(tclvalue(dejaCalc) == '1') 'normal' else 'disabled'
		tkconfigure(bt.CalculateOnset, state = stateCalc)
	})

	#######################

	frameTSMaps <- ttklabelframe(subfr4, text = "Maps", relief = 'groove')

	EnvPICSAplot$varPICSA <- tclVar("Onset")
	varPICSA.val <- c("Onset", "Cessation", "Season Length", "Seasonal Rainfall Amounts",
					"Dry Spells", "Longest Dry Spell", 
					"Number of rain day", "Maximum daily rain",
					"Total rain when RR>95thPerc", "Nb of day when RR>95thPerc")

	if(GeneralParameters$compute.ETP == 'temp') varPICSA.val <- c(varPICSA.val, "Maximum temperature", "Minimum temperature")
	stateDrySpl <- 'disabled'

	cb.TsMap.picsavar <- ttkcombobox(frameTSMaps, values = varPICSA.val, textvariable = EnvPICSAplot$varPICSA, width = 21)
	txt.TsMap.dryspell <- tklabel(frameTSMaps, text = "DrySpell",  anchor = 'w', justify = 'left')
	EnvPICSAplot$spin.TsMap.dryspell <- ttkspinbox(frameTSMaps, from = 1, to = 40, increment = 1, justify = 'center', width = 2, state = stateDrySpl)
	tkset(EnvPICSAplot$spin.TsMap.dryspell, 5)

	bt.TsMap.prev <- ttkbutton(frameTSMaps, text = "<<", width = 4)
	bt.TsMap.next <- ttkbutton(frameTSMaps, text = ">>", width = 4)
	EnvPICSAplot$spin.TsMap.year <- ttkspinbox(frameTSMaps, from = 1800, to = 2200, increment = 1, justify = 'center', width = 4)
	tkset(EnvPICSAplot$spin.TsMap.year, 2015)
	bt.TsMap.plot <- ttkbutton(frameTSMaps, text = "PLOT")

	tkgrid(cb.TsMap.picsavar, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.TsMap.dryspell, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(EnvPICSAplot$spin.TsMap.dryspell, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(bt.TsMap.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(EnvPICSAplot$spin.TsMap.year, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.TsMap.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.TsMap.plot, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.TsMap.picsavar, "Select the variable to plot")
	status.bar.display(cb.TsMap.picsavar, TextOutputVar, "Select the variable to plot")
	infobulle(EnvPICSAplot$spin.TsMap.dryspell, "Dry spell definition (continuous dry days)")
	status.bar.display(EnvPICSAplot$spin.TsMap.dryspell, TextOutputVar, "Dry spell definition (continuous dry days)")
	infobulle(bt.TsMap.prev, "Plot the previous year")
	status.bar.display(bt.TsMap.prev, TextOutputVar, "Plot the previous year")
	infobulle(bt.TsMap.next, "Plot the next year")
	status.bar.display(bt.TsMap.next, TextOutputVar, "Plot the next year")
	infobulle(EnvPICSAplot$spin.TsMap.year, "Select the year to plot")
	status.bar.display(EnvPICSAplot$spin.TsMap.year, TextOutputVar, "Select the year to plot")

	#################
	tkbind(cb.TsMap.picsavar, "<<ComboboxSelected>>", function(){
		stateDrySpl <- if(tclvalue(EnvPICSAplot$varPICSA) == "Dry Spells") "normal" else "disabled"
		tkconfigure(EnvPICSAplot$spin.TsMap.dryspell, state = stateDrySpl)
	})

	#######################

	frameClimMaps <- ttklabelframe(subfr4, text = "Climatological Analysis", relief = 'groove')

	ANALYSIS <- c('Average', 'Median', 'Standard deviation', 'Trend', 'Percentiles', 'Frequency')
	EnvPICSAplot$analysis.method <- tclVar('Average')
	EnvPICSAplot$mth.perc <- tclVar(95)
	EnvPICSAplot$low.thres <- tclVar("09-15")
	EnvPICSAplot$up.thres <- tclVar("11-30")

	statePrc <- if(tclvalue(EnvPICSAplot$analysis.method) == 'Percentiles') 'normal' else 'disabled'
	stateFrq <- if(tclvalue(EnvPICSAplot$analysis.method) == 'Frequency') 'normal' else 'disabled'

	cb.anMthd <- ttkcombobox(frameClimMaps, values = ANALYSIS, textvariable = EnvPICSAplot$analysis.method, width = 21)
	txt.Percent <- tklabel(frameClimMaps, text = "Percentile",  anchor = 'w', justify = 'left')
	en.Percent <- tkentry(frameClimMaps, textvariable = EnvPICSAplot$mth.perc, width = 3, state = statePrc)

	txt.Freq1 <- tklabel(frameClimMaps, text = "Between",  anchor = 'w', justify = 'left')
	en.Freq1 <- tkentry(frameClimMaps, textvariable = EnvPICSAplot$low.thres, width = 5, state = stateFrq)
	txt.Freq2 <- tklabel(frameClimMaps, text = "And",  anchor = 'w', justify = 'left')
	en.Freq2 <- tkentry(frameClimMaps, textvariable = EnvPICSAplot$up.thres, width = 5, state = stateFrq)
	bt.ClimMap.plot <- ttkbutton(frameClimMaps, text = "PLOT")


	tkgrid(cb.anMthd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.Percent, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.Percent, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.Freq1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.Freq1, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.Freq2, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.Freq2, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.ClimMap.plot, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.anMthd, "Select the analysis method")
	status.bar.display(cb.anMthd, TextOutputVar, "Select the analysis method")
	infobulle(en.Percent, "Enter the nth percentile to be calculated")
	status.bar.display(en.Percent, TextOutputVar, "Enter the nth percentile to be calculated")
	infobulle(en.Freq1, "Enter the lower bound of the interval to count the number of occurrences.\nIn the case of Onset and Cessation, the limit should be of the form Month-Day,\nnumber otherwise")
	status.bar.display(en.Freq1, TextOutputVar, "Enter the lower bound of the interval to count the number of occurrences.\nIn the case of Onset and Cessation, the limit should be of the form Month-Day,\nnumber otherwise")
	infobulle(en.Freq2, "Enter the upper bound of the interval to count the number of occurrences.\nIn the case of Onset and Cessation, the limit should be of the form Month-Day,\nnumber otherwise")
	status.bar.display(en.Freq2, TextOutputVar, "Enter the upper bound of the interval to count the number of occurrences.\nIn the case of Onset and Cessation, the limit should be of the form Month-Day,\nnumber otherwise")

	#################
	tkbind(cb.anMthd, "<<ComboboxSelected>>", function(){
		statePrc <- if(tclvalue(EnvPICSAplot$analysis.method) == 'Percentiles') 'normal' else 'disabled'
		stateFrq <- if(tclvalue(EnvPICSAplot$analysis.method) == 'Frequency') 'normal' else 'disabled'
		tkconfigure(en.Percent, state = statePrc)
		tkconfigure(en.Freq1, state = stateFrq)
		tkconfigure(en.Freq2, state = stateFrq)
	})

	#######################

	frameTSPlot <- ttklabelframe(subfr4, text = "Time Series Graph", relief = 'groove')

	varTSPLOT <- c("From Maps", "Daily Rainfall")
	EnvPICSAplot$varTSp <- tclVar("From Maps")

	typeTSPLOT <- c("Line", "Barplot", "Probability", "ENSO-Line", "ENSO-Barplot", "ENSO-Proba")
	EnvPICSAplot$typeTSp <- tclVar("Line")
	EnvPICSAplot$averageTSp <- tclVar(FALSE)
	EnvPICSAplot$tercileTSp <- tclVar(FALSE)
	EnvPICSAplot$trendTSp <- tclVar(FALSE)

	EnvPICSAplot$lonLOC <- tclVar()
	EnvPICSAplot$latLOC <- tclVar()

	# update when load data
	stnIDTSPLOT <- c("")
	EnvPICSAplot$stnIDTSp <- tclVar()

	stateTsp <- if(tclvalue(EnvPICSAplot$varTSp) == "From Maps") "normal" else "disabled"
	stateType <- if(tclvalue(EnvPICSAplot$typeTSp)%in%c("Line", "ENSO-Line") && tclvalue(EnvPICSAplot$varTSp) == "From Maps") "normal" else "disabled"

	statexyLoc <- "disabled"
	stateStnID <- "disabled"

	cb.varTSp <- ttkcombobox(frameTSPlot, values = varTSPLOT, textvariable = EnvPICSAplot$varTSp, width = 21)
	cb.typeTSp <- ttkcombobox(frameTSPlot, values = typeTSPLOT, textvariable = EnvPICSAplot$typeTSp, width = 10, state = stateTsp)

	chk.meanTSp <- tkcheckbutton(frameTSPlot, variable = EnvPICSAplot$averageTSp, text =  "Add Mean", anchor = 'w', justify = 'left', state = stateType)
	chk.tercTSp <- tkcheckbutton(frameTSPlot, variable = EnvPICSAplot$tercileTSp, text =  "Add Terciles", anchor = 'w', justify = 'left', state = stateType)
	chk.trendTSp <- tkcheckbutton(frameTSPlot, variable = EnvPICSAplot$trendTSp, text =  "Add Trend", anchor = 'w', justify = 'left', state = stateType)

	txt.lonLoc <- tklabel(frameTSPlot, text = "Longitude",  anchor = 'e', justify = 'right')
	en.lonLoc <- tkentry(frameTSPlot, textvariable = EnvPICSAplot$lonLOC, width = 8, state = statexyLoc)
	txt.latLoc <- tklabel(frameTSPlot, text = "Latitude",  anchor = 'e', justify = 'right')
	en.latLoc <- tkentry(frameTSPlot, textvariable = EnvPICSAplot$latLOC, width = 8, state = statexyLoc)

	txt.stnID <- tklabel(frameTSPlot, text = "Station",  anchor = 'e', justify = 'right')
	cb.stnID <- ttkcombobox(frameTSPlot, values = stnIDTSPLOT, textvariable = EnvPICSAplot$stnIDTSp, width = 10, state = stateStnID)
	bt.TsGraph.plot <- ttkbutton(frameTSPlot, text = "PLOT")

	tkgrid(cb.varTSp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 12, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.typeTSp, row = 0, column = 12, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(chk.meanTSp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.tercTSp, row = 1, column = 6, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.trendTSp, row = 1, column = 12, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.lonLoc, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.lonLoc, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.latLoc, row = 2, column = 9, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.latLoc, row = 2, column = 13, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.stnID, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.stnID, row = 3, column = 3, sticky = 'we', rowspan = 1, columnspan = 9, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.TsGraph.plot, row = 3, column = 12, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#################
	tkbind(cb.varTSp, "<<ComboboxSelected>>", function(){
		stateTsp <- if(tclvalue(EnvPICSAplot$varTSp) == "From Maps") "normal" else "disabled"
		tkconfigure(cb.typeTSp, state = stateTsp)

		stateType <- if(tclvalue(EnvPICSAplot$typeTSp)%in%c("Line", "ENSO-Line") && tclvalue(EnvPICSAplot$varTSp) == "From Maps") "normal" else "disabled"
		tkconfigure(chk.meanTSp, state = stateType)
		tkconfigure(chk.tercTSp, state = stateType)
		tkconfigure(chk.trendTSp, state = stateType)
	})

	tkbind(cb.typeTSp, "<<ComboboxSelected>>", function(){
		stateType <- if(tclvalue(EnvPICSAplot$typeTSp)%in%c("Line", "ENSO-Line") && tclvalue(EnvPICSAplot$varTSp) == "From Maps") "normal" else "disabled"
		tkconfigure(chk.meanTSp, state = stateType)
		tkconfigure(chk.tercTSp, state = stateType)
		tkconfigure(chk.trendTSp, state = stateType)
	})


	#######################
	tkgrid(framePicsaOut, row = 0, column = 0, sticky = 'we')
	tkgrid(frameTSMaps, row = 1, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameClimMaps, row = 2, column = 0, sticky = 'we', pady = 3)
	tkgrid(frameTSPlot, row = 3, column = 0, sticky = 'we', pady = 3)

	#######################################################################################################

	#Tab5
	frTab5 <- tkframe(cmd.tab5)
	tkgrid(frTab5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab5, 0, weight = 1)

	scrw5 <- bwScrolledWindow(frTab5)
	tkgrid(scrw5)
	tkgrid.columnconfigure(scrw5, 0, weight = 1)
	subfr5 <- bwScrollableFrame(scrw5, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr5, 0, weight = 1)

	#######################

	frameSHP <- ttklabelframe(subfr5, text = "Shapefile for Administrative Boundaries", relief = 'groove')

	file.shp <- tclVar()

	cb.shpF <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.shp, width = largeur1)
	bt.shpF <- tkbutton(frameSHP, text = "...")

	tkconfigure(bt.shpF, command = function(){
		shp.opfiles <- getOpenShp(main.win, all.opfiles)
		if(!is.null(shp.opfiles)){
			nopf <- length(AllOpenFilesType)
			AllOpenFilesType[[nopf+1]] <<- 'shp'
			AllOpenFilesData[[nopf+1]] <<- shp.opfiles

			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
			tclvalue(file.shp) <- AllOpenFilesData[[nopf+1]][[1]]
			tkconfigure(cb.rain, values = unlist(listOpenFiles), textvariable = file.rain)
			tkconfigure(cb.tmax, values = unlist(listOpenFiles), textvariable = file.tmax)
			tkconfigure(cb.tmin, values = unlist(listOpenFiles), textvariable = file.tmin)
			tkconfigure(cb.etp, values = unlist(listOpenFiles), textvariable = file.etp)
			tkconfigure(cb.raindk, values = unlist(listOpenFiles), textvariable = file.raindk)
			tkconfigure(cb.shpF, values = unlist(listOpenFiles), textvariable = file.shp)
		}else return(NULL)
	})

	tkgrid(cb.shpF, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.shpF, row = 0, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	#######################
	tkgrid(frameSHP, row = 0, column = 0, sticky = 'we')

	#######################################################################################################

	PICSADATA <- FALSE

	loadPICSA.Data <- function(){
		fileRData <- tclvalue(picsaData)
		if(file.exists(fileRData)){
			tkconfigure(main.win, cursor = 'watch')
			tcl("update")
			EnvPICSA.load <- readRDS(fileRData)
			ret <- lapply(ls(envir = EnvPICSA.load), function(x) {assign(x, get(x, envir = EnvPICSA.load), envir = EnvPICSA); return(NULL)})
			rm(EnvPICSA.load)
			EnvPICSAplot$directory <- dirname(fileRData)

			statexyLoc <- if(EnvPICSA$Pars$data.type == 'netcdf') "normal" else "disabled"
			stateStnID <- if(EnvPICSA$Pars$data.type == 'cdt') "normal" else "disabled"
			tkconfigure(cb.stnID, state = stateStnID)
			tkconfigure(en.lonLoc, state = statexyLoc)
			tkconfigure(en.latLoc, state = statexyLoc)

			varPICSA.val <- c("Onset", "Cessation", "Season Length", "Seasonal Rainfall Amounts",
							"Dry Spells", "Longest Dry Spell",
							"Number of rain day", "Maximum daily rain",
							"Total rain when RR>95thPerc", "Nb of day when RR>95thPerc")
			if(EnvPICSA$Pars$compute.ETP == 'temp'){
				varPICSA.val <- c(varPICSA.val, "Maximum temperature", "Minimum temperature")
			}else{
				if(tclvalue(EnvPICSAplot$varPICSA) %in% c("Maximum temperature", "Minimum temperature")) tclvalue(EnvPICSAplot$varPICSA) <- "Onset"
			}
			tkconfigure(cb.TsMap.picsavar, values = varPICSA.val)

			range.TsMap.year <- range(EnvPICSA$cdtONSET$years)
			tkconfigure(EnvPICSAplot$spin.TsMap.year, from = range.TsMap.year[1], to = range.TsMap.year[2])
			tkset(EnvPICSAplot$spin.TsMap.year, range.TsMap.year[2])

			stnIDTSPLOT <- if(EnvPICSA$Pars$data.type == 'cdt') EnvPICSA$PICSA.Coords$id else ""
			tkconfigure(cb.stnID, values = stnIDTSPLOT)
			tclvalue(EnvPICSAplot$stnIDTSp) <- stnIDTSPLOT[1]

			tkconfigure(main.win, cursor = '')
			tcl("update")
			loaded <- TRUE
		}else{
			InsertMessagesTxt(main.txt.out, 'No PICSA outputs data found', format = TRUE)
			loaded <- FALSE
		}
		return(loaded)
	}

	getSHPLines <- function(file.shp){
		if(!is.null(EnvPICSAplot$ocrds)){
			if(!isTRUE(all.equal(EnvPICSAplot$shpInfo, tclvalue(file.shp)))){
				getdfShp <- TRUE
				EnvPICSAplot$ocrds <- NULL
			}else getdfShp <- FALSE
		}else getdfShp <- TRUE

		if(getdfShp){
			shpofile <- getShpOpenData(file.shp)
			ocrds <- getBoundaries(shpofile[[2]])
			EnvPICSAplot$ocrds <- ocrds
			EnvPICSAplot$shpInfo <- tclvalue(file.shp)
		}else ocrds <- EnvPICSAplot$ocrds
		return(ocrds)
	}

	#######################

	EnvPICSAplot$notebookTab.maps <- NULL

	tkconfigure(bt.TsMap.prev, command = function(){
		if(!PICSADATA){
			picsa <- try(loadPICSA.Data(), silent = TRUE)
			if(inherits(picsa, "try-error")){
				InsertMessagesTxt(main.txt.out, 'Unable to load PICSA outputs data', format = TRUE)
				InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', picsa[1]), format = TRUE)
				return(NULL)
			}
			PICSADATA <<- picsa
		} 

		range.TsMap.year <- range(EnvPICSA$cdtONSET$years)
		iyear <- as.numeric(str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.year))))
		iyear <- iyear-1
		if(iyear < range.TsMap.year[1]) iyear <- range.TsMap.year[2]
		tkset(EnvPICSAplot$spin.TsMap.year, iyear)

		ocrds <- getSHPLines(file.shp)
		EnvPICSAplot$xlim.maps <- range(c(ocrds[, 1], EnvPICSA$PICSA.Coords$lon), na.rm = TRUE)
		EnvPICSAplot$ylim.maps <- range(c(ocrds[, 2], EnvPICSA$PICSA.Coords$lat), na.rm = TRUE)

		imgContainer <- PICSA.DisplayMaps(tknotes, ocrds)

		retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvPICSAplot$notebookTab.maps, AllOpenTabType, AllOpenTabData)
		EnvPICSAplot$notebookTab.maps <- retNBTab$notebookTab
		AllOpenTabType <<- retNBTab$AllOpenTabType
		AllOpenTabData <<- retNBTab$AllOpenTabData
	})

	#######################

	tkconfigure(bt.TsMap.next, command = function(){
		if(!PICSADATA){
			picsa <- try(loadPICSA.Data(), silent = TRUE)
			if(inherits(picsa, "try-error")){
				InsertMessagesTxt(main.txt.out, 'Unable to load PICSA outputs data', format = TRUE)
				InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', picsa[1]), format = TRUE)
				return(NULL)
			}
			PICSADATA <<- picsa
		} 

		range.TsMap.year <- range(EnvPICSA$cdtONSET$years)
		iyear <- as.numeric(str_trim(tclvalue(tkget(EnvPICSAplot$spin.TsMap.year))))
		iyear <- iyear+1
		if(iyear > range.TsMap.year[2]) iyear <- range.TsMap.year[1]
		tkset(EnvPICSAplot$spin.TsMap.year, iyear)

		ocrds <- getSHPLines(file.shp)
		EnvPICSAplot$xlim.maps <- range(c(ocrds[, 1], EnvPICSA$PICSA.Coords$lon), na.rm = TRUE)
		EnvPICSAplot$ylim.maps <- range(c(ocrds[, 2], EnvPICSA$PICSA.Coords$lat), na.rm = TRUE)

		imgContainer <- PICSA.DisplayMaps(tknotes, ocrds)

		retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvPICSAplot$notebookTab.maps, AllOpenTabType, AllOpenTabData)
		EnvPICSAplot$notebookTab.maps <- retNBTab$notebookTab
		AllOpenTabType <<- retNBTab$AllOpenTabType
		AllOpenTabData <<- retNBTab$AllOpenTabData
	})

	#######################

	tkconfigure(bt.TsMap.plot, command = function(){
		if(!PICSADATA){
			picsa <- try(loadPICSA.Data(), silent = TRUE)
			if(inherits(picsa, "try-error")){
				InsertMessagesTxt(main.txt.out, 'Unable to load PICSA outputs data', format = TRUE)
				InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', picsa[1]), format = TRUE)
				return(NULL)
			}
			PICSADATA <<- picsa
		} 

		ocrds <- getSHPLines(file.shp)
		EnvPICSAplot$xlim.maps <- range(c(ocrds[, 1], EnvPICSA$PICSA.Coords$lon), na.rm = TRUE)
		EnvPICSAplot$ylim.maps <- range(c(ocrds[, 2], EnvPICSA$PICSA.Coords$lat), na.rm = TRUE)

		imgContainer <- PICSA.DisplayMaps(tknotes, ocrds)

		retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvPICSAplot$notebookTab.maps, AllOpenTabType, AllOpenTabData)
		EnvPICSAplot$notebookTab.maps <- retNBTab$notebookTab
		AllOpenTabType <<- retNBTab$AllOpenTabType
		AllOpenTabData <<- retNBTab$AllOpenTabData
	})

	#######################

	EnvPICSAplot$notebookTab.clmaps <- NULL

	tkconfigure(bt.ClimMap.plot, command = function(){
		if(!PICSADATA){
			picsa <- try(loadPICSA.Data(), silent = TRUE)
			if(inherits(picsa, "try-error")){
				InsertMessagesTxt(main.txt.out, 'Unable to load PICSA outputs data', format = TRUE)
				InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', picsa[1]), format = TRUE)
				return(NULL)
			}
			PICSADATA <<- picsa
		} 

		ocrds <- getSHPLines(file.shp)
		EnvPICSAplot$xlim.maps <- range(c(ocrds[, 1], EnvPICSA$PICSA.Coords$lon), na.rm = TRUE)
		EnvPICSAplot$ylim.maps <- range(c(ocrds[, 2], EnvPICSA$PICSA.Coords$lat), na.rm = TRUE)

		imgContainer <- PICSA.DisplayClimMaps(tknotes, ocrds)

		retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvPICSAplot$notebookTab.clmaps, AllOpenTabType, AllOpenTabData)
		EnvPICSAplot$notebookTab.clmaps <- retNBTab$notebookTab
		AllOpenTabType <<- retNBTab$AllOpenTabType
		AllOpenTabData <<- retNBTab$AllOpenTabData
	})

	#######################

	EnvPICSAplot$notebookTab.tsplot <- NULL

	tkconfigure(bt.TsGraph.plot, command = function(){
		if(!PICSADATA){
			picsa <- try(loadPICSA.Data(), silent = TRUE)
			if(inherits(picsa, "try-error")){
				InsertMessagesTxt(main.txt.out, 'Unable to load PICSA outputs data', format = TRUE)
				InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', picsa[1]), format = TRUE)
				return(NULL)
			}
			PICSADATA <<- picsa
		}

		imgContainer <- PICSA.DisplayTSPlot(tknotes)

		retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvPICSAplot$notebookTab.tsplot, AllOpenTabType, AllOpenTabData)
		EnvPICSAplot$notebookTab.tsplot <- retNBTab$notebookTab
		AllOpenTabType <<- retNBTab$AllOpenTabType
		AllOpenTabData <<- retNBTab$AllOpenTabData
	})

	#######################################################################################################
	tcl('update')
	tkgrid(cmd.frame, sticky = 'nswe', pady = 5)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)
	######
	return(cmd.frame)
}




SPICalcPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(46)
		largeur0 <- as.integer(w.scale(22)/sfont0)
		largeur1 <- as.integer(w.scale(27)/sfont0)
		largeur2 <- as.integer(w.scale(29)/sfont0)
		largeur3 <- 20
		largeur4 <- 26
		largeur5 <- 20
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(48.5)
		largeur0 <- as.integer(w.scale(16)/sfont0)
		largeur1 <- as.integer(w.scale(21)/sfont0)
		largeur2 <- as.integer(w.scale(22)/sfont0)
		largeur3 <- 15
		largeur4 <- 20
		largeur5 <- 14
	}

	GeneralParameters <- list(intstep = "dekadal", data.type = "cdtstation", 
							cdtstation = "", cdtdataset = "",
							outfreq = "month", tscale = 3, distr = 'Gamma',
							monitoring = FALSE,
							dates = list(year1 = 2018, mon1 = 1, dek1 = 1, year2 = 2018, mon2 = 2, dek2 = 3),
							outdir = "")

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "SPI")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Maps")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Graphs")
	cmd.tab4 <- bwAddTab(tknote.cmd, text = "Boundaries")

	bwRaiseTab(tknote.cmd, cmd.tab1)
	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab3, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab4, 0, weight = 1)

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

		frameTimeS <- ttklabelframe(subfr1, text = "Time step of input data", relief = 'groove')

		timeSteps <- tclVar()
		CbperiodVAL <- c('Daily data', 'Pentad data', 'Dekadal data', 'Monthly data')
		tclvalue(timeSteps) <- switch(GeneralParameters$intstep, 
										'daily' = CbperiodVAL[1],
										'pentad' = CbperiodVAL[2],
										'dekadal' = CbperiodVAL[3],
										'monthly' = CbperiodVAL[4])

		cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = timeSteps, width = largeur1)

		tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.fperiod, 'Select the time step of the data')
		status.bar.display(cb.fperiod, TextOutputVar, 'Select the time step of the data')

		############

		tkbind(cb.fperiod, "<<ComboboxSelected>>", function(){
			valSPIfreq <- if(str_trim(tclvalue(timeSteps)) == 'Monthly data') "month" else c("dekad", "month")
			tkconfigure(cb.SPIfreq, values = valSPIfreq)
			if(str_trim(tclvalue(timeSteps)) == 'Monthly data'){
				tclvalue(out.spifreq) <- "month"
				tclvalue(txt.suffix.var) <- '-month'
			}
			stateTscale <- if(str_trim(tclvalue(out.spifreq)) == 'month') "normal" else "disabled"
			tkconfigure(spin.Tscale, state = stateTscale)
		})

		#######################

		frameInData <- ttklabelframe(subfr1, text = "Input Data", relief = 'groove')

		DataType <- tclVar()
		CbdatatypeVAL <- c('CDT stations data format', 'CDT dataset format (gridded)')
		tclvalue(DataType) <- switch(GeneralParameters$data.type,
									'cdtstation' = CbdatatypeVAL[1],
									'cdtdataset' = CbdatatypeVAL[2])

		if(GeneralParameters$data.type == 'cdtstation'){
			input.file <- tclVar(GeneralParameters$cdtstation)
			txt.INData <- 'File containing stations Precip data'
		}else{
			input.file <- tclVar(GeneralParameters$cdtdataset)
			txt.INData <- 'Index file (*.rds) for Precip dataset'
		}

		txt.INData.var <- tclVar(txt.INData)

		txt.datatype <- tklabel(frameInData, text = "Format", anchor = 'w', justify = 'left')
		cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)

		txt.infile <- tklabel(frameInData, text = tclvalue(txt.INData.var), textvariable = txt.INData.var, anchor = 'w', justify = 'left')

		if(GeneralParameters$data.type == 'cdtstation'){
			cb.en.infile <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)
		}else{
			cb.en.infile <- tkentry(frameInData, textvariable = input.file, width = largeur2)
		}
		bt.infile <- tkbutton(frameInData, text = "...")

		############

		tkconfigure(bt.infile, command = function(){
			if(GeneralParameters$data.type == 'cdtstation'){
				dat.opfiles <- getOpenFiles(main.win, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(input.file) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.en.infile, values = unlist(listOpenFiles), textvariable = input.file)
				}else return(NULL)
			}else{
				filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
				tclvalue(input.file) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
			}
		})

		############

		tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.infile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.en.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.infile, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		############
		infobulle(cb.datatype, 'Select the format of the input data')
		status.bar.display(cb.datatype, TextOutputVar, 'Select the format of the input data')

		if(GeneralParameters$data.type == 'cdtstation'){
			infobulle(cb.en.infile, 'Select the file containing the input data')
			status.bar.display(cb.en.infile, TextOutputVar, 'Select the file containing the input data')
			infobulle(bt.infile, 'Browse file if not listed')
			status.bar.display(bt.infile, TextOutputVar, 'Browse file if not listed')
		}else{
			infobulle(cb.en.infile, 'Enter the full path to the file <dataset name>.rds')
			status.bar.display(cb.en.infile, TextOutputVar, 'Enter the full path to the file <dataset name>.rds')
			infobulle(bt.infile, 'or browse here')
			status.bar.display(bt.infile, TextOutputVar, 'or browse here')
		}

		############

		tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
			tkdestroy(cb.en.infile)
			tclvalue(input.file) <- ''

			###
			if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
				tclvalue(txt.INData.var) <- 'File containing stations Precip data'

				cb.en.infile <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)

				tkconfigure(bt.infile, command = function(){
					dat.opfiles <- getOpenFiles(main.win, all.opfiles)
					if(!is.null(dat.opfiles)){
						nopf <- length(AllOpenFilesType)
						AllOpenFilesType[[nopf+1]] <<- 'ascii'
						AllOpenFilesData[[nopf+1]] <<- dat.opfiles

						listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
						tclvalue(input.file) <- AllOpenFilesData[[nopf+1]][[1]]
						tkconfigure(cb.en.infile, values = unlist(listOpenFiles), textvariable = input.file)
					}else return(NULL)
				})

				infobulle(cb.en.infile, 'Select the file containing the input data')
				status.bar.display(cb.en.infile, TextOutputVar, 'Select the file containing the input data')
				infobulle(bt.infile, 'Browse file if not listed')
				status.bar.display(bt.infile, TextOutputVar, 'Browse file if not listed')
			}

			###
			if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)'){
				tclvalue(txt.INData.var) <- 'Index file (*.rds) for Precip dataset'

				cb.en.infile <- tkentry(frameInData, textvariable = input.file, width = largeur2)

				tkconfigure(bt.infile, command = function(){
					filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
					path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
					tclvalue(input.file) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
				})

				infobulle(cb.en.infile, 'Enter the full path to the file <dataset name>.rds')
				status.bar.display(cb.en.infile, TextOutputVar, 'Enter the full path to the file <dataset name>.rds')
				infobulle(bt.infile, 'or browse here')
				status.bar.display(bt.infile, TextOutputVar, 'or browse here')
			}

			tkgrid(cb.en.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		})

		#############################

		frameMoni <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		monitoring <- tclVar(GeneralParameters$monitoring)

		istart.yrs <- tclVar(GeneralParameters$dates$year1)
		istart.mon <- tclVar(GeneralParameters$dates$mon1)
		istart.dek <- tclVar(GeneralParameters$dates$dek1)
		iend.yrs <- tclVar(GeneralParameters$dates$year2)
		iend.mon <- tclVar(GeneralParameters$dates$mon2)
		iend.dek <- tclVar(GeneralParameters$dates$dek2)

		if(GeneralParameters$monitoring){
			statedates <- 'normal'
			statedatedek <- if(GeneralParameters$outfreq == 'month') 'disabled' else 'normal'
		}else{
			statedates <- 'disabled'
			statedatedek <- 'disabled'
		}

		chk.Moni <- tkcheckbutton(frameMoni, variable = monitoring, text = "Monitoring: update SPI dataset", anchor = 'w', justify = 'left')
		fr.Moni <- tkframe(frameMoni)

		txt.deb.Moni <- tklabel(fr.Moni, text = 'Start date', anchor = 'e', justify = 'right')
		txt.fin.Moni <- tklabel(fr.Moni, text = 'End date', anchor = 'e', justify = 'right')
		txt.yrs.Moni <- tklabel(fr.Moni, text = 'Year')
		txt.mon.Moni <- tklabel(fr.Moni, text = 'Month')
		txt.dek.Moni <- tklabel(fr.Moni, text = 'Dekad')
		en.yrs1.Moni <- tkentry(fr.Moni, width = 4, textvariable = istart.yrs, justify = "right", state = statedates)
		en.mon1.Moni <- tkentry(fr.Moni, width = 4, textvariable = istart.mon, justify = "right", state = statedates)
		en.dek1.Moni <- tkentry(fr.Moni, width = 4, textvariable = istart.dek, justify = "right", state = statedatedek)
		en.yrs2.Moni <- tkentry(fr.Moni, width = 4, textvariable = iend.yrs, justify = "right", state = statedates)
		en.mon2.Moni <- tkentry(fr.Moni, width = 4, textvariable = iend.mon, justify = "right", state = statedates)
		en.dek2.Moni <- tkentry(fr.Moni, width = 4, textvariable = iend.dek, justify = "right", state = statedatedek)

		tkgrid(txt.deb.Moni, row = 1, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)
		tkgrid(txt.fin.Moni, row = 2, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)
		tkgrid(txt.yrs.Moni, row = 0, column = 1, rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)
		tkgrid(txt.mon.Moni, row = 0, column = 2, rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)
		tkgrid(txt.dek.Moni, row = 0, column = 3, rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)
		tkgrid(en.yrs1.Moni, row = 1, column = 1, rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)
		tkgrid(en.mon1.Moni, row = 1, column = 2, rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)
		tkgrid(en.dek1.Moni, row = 1, column = 3, rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)
		tkgrid(en.yrs2.Moni, row = 2, column = 1, rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)
		tkgrid(en.mon2.Moni, row = 2, column = 2, rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)
		tkgrid(en.dek2.Moni, row = 2, column = 3, rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)

		tkgrid(chk.Moni, row = 0, column = 0, rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)
		tkgrid(fr.Moni, row = 1, column = 0, rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)

		###############

		tkbind(chk.Moni, "<Button-1>", function(){
			if(tclvalue(monitoring) == "0"){
				statedates <- 'normal'
				statedatedek <- if(str_trim(tclvalue(out.spifreq)) == 'month') 'disabled' else 'normal'
				stateDistr <- 'disabled'
				tclvalue(txt.save.var) <- "Index file (SPI.rds) for SPI data"

				tkconfigure(bt.outSPI, command = function(){
					filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
					path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
					tclvalue(outSPIdir) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
				})
			}else{
				statedates <- 'disabled'
				statedatedek <- 'disabled'
				stateDistr <- 'normal'
				tclvalue(txt.save.var) <- "Directory to save the outputs"
				tkconfigure(bt.outSPI, command = function(){
					dirSPI <- tk_choose.dir(getwd(), "")
					tclvalue(outSPIdir) <- if(dirSPI%in%c("", "NA") | is.na(dirSPI)) "" else dirSPI
				})
			}

			tkconfigure(en.yrs1.Moni, state = statedates)
			tkconfigure(en.mon1.Moni, state = statedates)
			tkconfigure(en.dek1.Moni, state = statedatedek)
			tkconfigure(en.yrs2.Moni, state = statedates)
			tkconfigure(en.mon2.Moni, state = statedates)
			tkconfigure(en.dek2.Moni, state = statedatedek)

			tkconfigure(cb.Distrb, state = stateDistr)
		})

		#############################

		frameParams <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		out.spifreq <- tclVar(GeneralParameters$outfreq)

		if(GeneralParameters$outfreq == 'dekad'){
			txt.suffix <- '-dekad'
			stateTscale <- "disabled"
			up.tscale <- 1
			val.tscale <- 1
		}else{
			txt.suffix <- '-month'
			stateTscale <- "normal"
			up.tscale <- 60
			val.tscale <- GeneralParameters$tscale
		}
		txt.suffix.var <- tclVar(txt.suffix)

		frameTscale <- tkframe(frameParams)
		txt.SPIfreq <- tklabel(frameTscale, text = "SPI", anchor = 'e', justify = 'right')
		cb.SPIfreq <- ttkcombobox(frameTscale, values = c("dekad", "month"), textvariable = out.spifreq, width = 8)
		txt.Tscale1 <- tklabel(frameTscale, text = "Timescale", anchor = 'e', justify = 'right')
		spin.Tscale <- ttkspinbox(frameTscale, from = 1, to = up.tscale, increment = 1, justify = 'center', width = 2, state = stateTscale)
		tkset(spin.Tscale, val.tscale)
		txt.Tscale2 <- tklabel(frameTscale, text = tclvalue(txt.suffix.var), textvariable = txt.suffix.var, anchor = 'w', justify = 'left')

		tkgrid(txt.SPIfreq, cb.SPIfreq, txt.Tscale1, spin.Tscale, txt.Tscale2)

		########
		tkbind(cb.SPIfreq, "<<ComboboxSelected>>", function(){
			if(str_trim(tclvalue(out.spifreq)) == 'dekad'){
				stateTscale <- "disabled"
				tclvalue(txt.suffix.var) <- '-dekad'
				tkset(spin.Tscale, 1)
				statedatedek <- if(tclvalue(monitoring) == "1") "normal" else "disabled"
			}
			if(str_trim(tclvalue(out.spifreq)) == 'month'){
				stateTscale <- "normal"
				tclvalue(txt.suffix.var) <- '-month'
				tkconfigure(spin.Tscale, to = 60)
				statedatedek <- "disabled"
			}
			tkconfigure(spin.Tscale, state = stateTscale)
			tkconfigure(en.dek1.Moni, state = statedatedek)
			tkconfigure(en.dek2.Moni, state = statedatedek)
		})

		########
		frameDistrb <- tkframe(frameParams)

		DistrbVAL <- c("Gamma", "Pearson Type III", "log-Logistic", "Z-Score")
		DistrbFun <- tclVar(GeneralParameters$distr)
		stateDistr <- if(GeneralParameters$monitoring) 'disabled' else 'normal'

		txt.Distrb <- tklabel(frameDistrb, text = "Distribution function", anchor = 'e', justify = 'right')
		cb.Distrb <- ttkcombobox(frameDistrb, values = DistrbVAL, textvariable = DistrbFun, width = largeur3, state = stateDistr)

		tkgrid(txt.Distrb, cb.Distrb)

		########
		tkgrid(frameTscale, row = 0, column = 0, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameDistrb, row = 1, column = 0, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		#############################

		frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		outSPIdir <- tclVar(GeneralParameters$outdir)

		if(GeneralParameters$monitoring){
			text.save <- "Index file (SPI.rds) for SPI data"
		}else{
			text.save <- "Directory to save the outputs"
		}
		txt.save.var <- tclVar(text.save)

		txt.outSPI <- tklabel(frameDirSav, text = tclvalue(txt.save.var), textvariable = txt.save.var, anchor = 'w', justify = 'left')
		en.outSPI <- tkentry(frameDirSav, textvariable = outSPIdir, width = largeur2)
		bt.outSPI <- tkbutton(frameDirSav, text = "...")

		######

		tkconfigure(bt.outSPI, command = function(){
			if(GeneralParameters$monitoring){
				filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
				tclvalue(outSPIdir) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
			}else{
				dirSPI <- tk_choose.dir(getwd(), "")
				tclvalue(outSPIdir) <- if(dirSPI%in%c("", "NA") | is.na(dirSPI)) "" else dirSPI
			}
		})

		######
		tkgrid(txt.outSPI, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.outSPI, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.outSPI, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		infobulle(en.outSPI, 'Enter the full path to directory to save outputs')
		status.bar.display(en.outSPI, TextOutputVar, 'Enter the full path to directory to save outputs')
		infobulle(bt.outSPI, 'or browse here')
		status.bar.display(bt.outSPI, TextOutputVar, 'or browse here')

		#############################

		if(!is.null(EnvSPICalcPlot$DirExist)){
			stateCaclBut <- if(tclvalue(EnvSPICalcPlot$DirExist) == "0") "normal" else "disabled"
		}else stateCaclBut <- "normal"

		calculateBut <- ttkbutton(subfr1, text = "Calculate", state = stateCaclBut)

		#################

		tkconfigure(calculateBut, command = function(){
			GeneralParameters$intstep <- switch(str_trim(tclvalue(timeSteps)), 
			 									'Daily data' = 'daily',
			 									'Pentad data' = 'pentad',
												'Dekadal data' =  'dekadal',
												'Monthly data' = 'monthly')

			GeneralParameters$data.type <- switch(str_trim(tclvalue(DataType)),
												'CDT stations data format' = 'cdtstation',
												'CDT dataset format (gridded)' = 'cdtdataset')

			if(str_trim(tclvalue(DataType)) == 'CDT stations data format')
				GeneralParameters$cdtstation <- str_trim(tclvalue(input.file))
			if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)')
				GeneralParameters$cdtdataset <- str_trim(tclvalue(input.file))

			GeneralParameters$monitoring <- switch(tclvalue(monitoring), '0' = FALSE, '1' = TRUE)

			GeneralParameters$dates$year1 <- as.numeric(str_trim(tclvalue(istart.yrs)))
			GeneralParameters$dates$mon1 <- as.numeric(str_trim(tclvalue(istart.mon)))
			GeneralParameters$dates$dek1 <- as.numeric(str_trim(tclvalue(istart.dek)))
			GeneralParameters$dates$year2 <- as.numeric(str_trim(tclvalue(iend.yrs)))
			GeneralParameters$dates$mon2 <- as.numeric(str_trim(tclvalue(iend.mon)))
			GeneralParameters$dates$dek2 <- as.numeric(str_trim(tclvalue(iend.dek)))

			GeneralParameters$outfreq <- str_trim(tclvalue(out.spifreq))
			GeneralParameters$tscale <- as.numeric(str_trim(tclvalue(tkget(spin.Tscale))))
			GeneralParameters$distr <- str_trim(tclvalue(DistrbFun))

			GeneralParameters$outdir <- str_trim(tclvalue(outSPIdir))

			# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

			tkconfigure(main.win, cursor = 'watch')
			InsertMessagesTxt(main.txt.out, "Calculate SPI ......")

			ret <- tryCatch(
				computeSPIProcs(GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = tkconfigure(main.win, cursor = '')
			)

			msg0 <- "SPI calculation finished successfully"
			msg1 <- "SPI calculation failed"

			if(!is.null(ret)){
				if(ret == 0){
					InsertMessagesTxt(main.txt.out, msg0)

					###################

					widgets.Station.Pixel()
					ret <- try(set.Data.Scales(), silent = TRUE)
					if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

					ret <- try(set.Data.Dates(), silent = TRUE)
					if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
				}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
			}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
		})

		############################################

		tkgrid(frameTimeS, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameMoni, row = 2, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameParams, row = 3, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(calculateBut, row = 5, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab2
	frTab2 <- tkframe(cmd.tab2)
	tkgrid(frTab2, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab2, 0, weight = 1)

	scrw2 <- bwScrolledWindow(frTab2)
	tkgrid(scrw2)
	tkgrid.columnconfigure(scrw2, 0, weight = 1)
	subfr2 <- bwScrollableFrame(scrw2, width = wscrlwin, height = hscrlwin)
	tkgrid.columnconfigure(subfr2, 0, weight = 1)

		##############################################

		frameDataExist <- ttklabelframe(subfr2, text = "SPI data", relief = 'groove')

		EnvSPICalcPlot$DirExist <- tclVar(0)
		file.dataIndex <- tclVar()

		stateExistData <- if(tclvalue(EnvSPICalcPlot$DirExist) == "1") "normal" else "disabled"

		chk.dataIdx <- tkcheckbutton(frameDataExist, variable = EnvSPICalcPlot$DirExist, text = "SPI data already computed", anchor = 'w', justify = 'left')
		en.dataIdx <- tkentry(frameDataExist, textvariable = file.dataIndex, width = largeur2, state = stateExistData)
		bt.dataIdx <- tkbutton(frameDataExist, text = "...", state = stateExistData)

		tkconfigure(bt.dataIdx, command = function(){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.Stat <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			if(path.Stat%in%c("", "NA") | is.na(path.Stat)) return(NULL)
			tclvalue(file.dataIndex) <- path.Stat

			if(file.exists(str_trim(tclvalue(file.dataIndex)))){
				OutSPIdata <- try(readRDS(str_trim(tclvalue(file.dataIndex))), silent = TRUE)
				if(inherits(OutSPIdata, "try-error")){
					InsertMessagesTxt(main.txt.out, 'Unable to load SPI data', format = TRUE)
					InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', OutSPIdata[1]), format = TRUE)
					tkconfigure(cb.spi.maps, values = "")
					tclvalue(EnvSPICalcPlot$spi.tscale) <- ""
					tkconfigure(cb.spi.Date, values = "")
					tclvalue(EnvSPICalcPlot$spi.date) <- ""
					return(NULL)
				}

				EnvSPICalcPlot$output <- OutSPIdata
				EnvSPICalcPlot$PathData <- dirname(str_trim(tclvalue(file.dataIndex)))

				###################

				widgets.Station.Pixel()
				ret <- try(set.Data.Scales(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				ret <- try(set.Data.Dates(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})


		tkgrid(chk.dataIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.dataIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.dataIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############

		tkbind(chk.dataIdx, "<Button-1>", function(){
			stateExistData <- if(tclvalue(EnvSPICalcPlot$DirExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.dataIdx, state = stateExistData)
			tkconfigure(bt.dataIdx, state = stateExistData)
			stateCaclBut <- if(tclvalue(EnvSPICalcPlot$DirExist) == '1') 'normal' else 'disabled'
			tkconfigure(calculateBut, state = stateCaclBut)
		})

		##############################################

		frameSPIMap <- ttklabelframe(subfr2, text = "SPI Map", relief = 'groove')

		EnvSPICalcPlot$spi.tscale <- tclVar()
		EnvSPICalcPlot$spi.date <- tclVar()

		cb.spi.maps <- ttkcombobox(frameSPIMap, values = "", textvariable = EnvSPICalcPlot$spi.tscale, width = largeur4)
		bt.spi.maps <- ttkbutton(frameSPIMap, text = "PLOT", width = 7)
		cb.spi.Date <- ttkcombobox(frameSPIMap, values = "", textvariable = EnvSPICalcPlot$spi.date, width = largeur5)
		bt.spi.Date.prev <- ttkbutton(frameSPIMap, text = "<<", width = 3)
		bt.spi.Date.next <- ttkbutton(frameSPIMap, text = ">>", width = 3)
		bt.spi.MapOpt <- ttkbutton(frameSPIMap, text = "Options", width = 7)

		###############

		EnvSPICalcPlot$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = TRUE),
											userCol = list(custom = FALSE, color = NULL),
											userLvl = list(custom = TRUE, levels = c(-2, -1.5, -1, 0, 1, 1.5, 2), equidist = TRUE),
											title = list(user = FALSE, title = ''),
											colkeyLab = list(user = FALSE, label = ''),
											scalebar = list(add = FALSE, pos = 'bottomleft'))

		tkconfigure(bt.spi.MapOpt, command = function(){
			if(!is.null(EnvSPICalcPlot$varData$map)){
				atlevel <- pretty(EnvSPICalcPlot$varData$map$z, n = 10, min.n = 7)
				if(is.null(EnvSPICalcPlot$dataMapOp$userLvl$levels)){
					EnvSPICalcPlot$dataMapOp$userLvl$levels <- atlevel
				}else{
					if(!EnvSPICalcPlot$dataMapOp$userLvl$custom)
						EnvSPICalcPlot$dataMapOp$userLvl$levels <- atlevel
				}
			}
			EnvSPICalcPlot$dataMapOp <- MapGraph.MapOptions(main.win, EnvSPICalcPlot$dataMapOp)
		})

		###############

		EnvSPICalcPlot$notebookTab.dataMap <- NULL

		tkconfigure(bt.spi.maps, command = function(){
			if(str_trim(tclvalue(EnvSPICalcPlot$spi.date)) != "" &
				!is.null(EnvSPICalcPlot$varData))
			{
				get.Data.Map()

				imgContainer <- SPICalc.Display.Maps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvSPICalcPlot$notebookTab.dataMap, AllOpenTabType, AllOpenTabData)
				EnvSPICalcPlot$notebookTab.dataMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.spi.Date.prev, command = function(){
			if(str_trim(tclvalue(EnvSPICalcPlot$spi.date)) != ""){
				donDates <- EnvSPICalcPlot$varData$ts$dates
				idaty <- which(donDates == str_trim(tclvalue(EnvSPICalcPlot$spi.date)))
				idaty <- idaty-1
				if(idaty < 1) idaty <- length(donDates)
				tclvalue(EnvSPICalcPlot$spi.date) <- donDates[idaty]
				get.Data.Map()

				imgContainer <- SPICalc.Display.Maps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvSPICalcPlot$notebookTab.dataMap, AllOpenTabType, AllOpenTabData)
				EnvSPICalcPlot$notebookTab.dataMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.spi.Date.next, command = function(){
			if(str_trim(tclvalue(EnvSPICalcPlot$spi.date)) != ""){
				donDates <- EnvSPICalcPlot$varData$ts$dates
				idaty <- which(donDates == str_trim(tclvalue(EnvSPICalcPlot$spi.date)))
				idaty <- idaty+1
				if(idaty > length(donDates)) idaty <- 1
				tclvalue(EnvSPICalcPlot$spi.date) <- donDates[idaty]
				get.Data.Map()

				imgContainer <- SPICalc.Display.Maps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvSPICalcPlot$notebookTab.dataMap, AllOpenTabType, AllOpenTabData)
				EnvSPICalcPlot$notebookTab.dataMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		###############
		tkgrid(cb.spi.maps, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.spi.maps, row = 0, column = 4, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.spi.Date.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.spi.Date, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.spi.Date.next, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.spi.MapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(cb.spi.maps, "<<ComboboxSelected>>", function(){
			ret <- try(set.Data.Dates(), silent = TRUE)
			if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
		})

		##############################################

		tkgrid(frameDataExist, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameSPIMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

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

		##############################################

		frameDataTS <- ttklabelframe(subfr3, text = "SPI Graph", relief = 'groove')

		typeTSPLOT <- c("Bar-Line", "Polygon")
		EnvSPICalcPlot$graph$typeTSp <- tclVar("Bar-Line")

		cb.typeTSp <- ttkcombobox(frameDataTS, values = typeTSPLOT, textvariable = EnvSPICalcPlot$graph$typeTSp, width = largeur5)
		bt.TsGraph.plot <- ttkbutton(frameDataTS, text = "PLOT", width = 7)
		bt.TSGraphOpt <- ttkbutton(frameDataTS, text = "Options", width = 8)

		#################

		EnvSPICalcPlot$TSGraphOp <- list(
							bar.line = list(
								xlim = list(is.min = FALSE, min = "1981-1-1", is.max = FALSE, max = "2017-12-3"),
								ylim = list(is.min = FALSE, min = -10, is.max = FALSE, max = 10),
								userYTcks = list(custom = TRUE, ticks = c(-2, -1.5, -1, 0, 1, 1.5, 2)),
								axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
								title = list(is.title = FALSE, title = '', position = 'top'),
								colors = list(y0 = 0, negative = "#CF661C", positive = "#157040"),
								line = list(plot = FALSE, col = "black", lwd = 1.5)
							)
						)

		tkconfigure(bt.TSGraphOpt, command = function(){
			suffix.fun <- switch(str_trim(tclvalue(EnvSPICalcPlot$graph$typeTSp)),
									"Bar-Line" = "Bar.Line",
									"Polygon" = "Bar.Line")
			plot.fun <- match.fun(paste0("MapGraph.GraphOptions.", suffix.fun))
			EnvSPICalcPlot$TSGraphOp <- plot.fun(main.win, EnvSPICalcPlot$TSGraphOp)
		})

		#########
		EnvSPICalcPlot$notebookTab.dataGraph <- NULL

		tkconfigure(bt.TsGraph.plot, command = function(){
			if(!is.null(EnvSPICalcPlot$varData)){
				imgContainer <- SPICalc.Display.Graph(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvSPICalcPlot$notebookTab.dataGraph, AllOpenTabType, AllOpenTabData)
				EnvSPICalcPlot$notebookTab.dataGraph <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		#################

		tkgrid(cb.typeTSp, row = 0, column = 0, sticky = 'we', pady = 1, columnspan = 1)
		tkgrid(bt.TSGraphOpt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
		tkgrid(bt.TsGraph.plot, row = 0, column = 2, sticky = 'we', pady = 1, columnspan = 1)

		##############################################

		frameSTNCrds <- ttklabelframe(subfr3, text = "Station/Coordinates", relief = 'groove')

		frTS2 <- tkframe(frameSTNCrds)
		EnvSPICalcPlot$graph$lonLOC <- tclVar()
		EnvSPICalcPlot$graph$latLOC <- tclVar()
		EnvSPICalcPlot$graph$stnIDTSp <- tclVar()

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

		##############################################

		frameVizTS <- tkframe(subfr3, relief = 'groove', borderwidth = 2)

		EnvSPICalcPlot$spiViz$max.tscale <- tclVar(12)

		bt.VizTS <- ttkbutton(frameVizTS, text = "Visualizing time-scales")
		bt.VizOpt <- ttkbutton(frameVizTS, text = "Options")
		txt.VizTS <- tklabel(frameVizTS, text = "Maximum time-scale", anchor = 'e', justify = 'right')
		en.VizTS <- tkentry(frameVizTS, textvariable = EnvSPICalcPlot$spiViz$max.tscale, width = 3)

		###############

		EnvSPICalcPlot$spiVizOp <- list(presetCol = list(color = 'spi.colors', reverse = FALSE),
										userCol = list(custom = FALSE, color = NULL),
										userLvl = list(custom = TRUE, levels = c(-2, -1.5, -1, 0, 1, 1.5, 2), equidist = TRUE),
										title = list(user = FALSE, title = ''),
										colkeyLab = list(user = FALSE, label = ''),
										axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = TRUE, ylab = 'Time-scale (months)'))

		tkconfigure(bt.VizOpt, command = function(){
			EnvSPICalcPlot$spiVizOp <- MapGraph.SpiVizOptions(main.win, EnvSPICalcPlot$spiVizOp)
		})

		###############

		EnvSPICalcPlot$notebookTab.spiViz <- NULL

		tkconfigure(bt.VizTS, command = function(){
			if(!is.null(EnvSPICalcPlot$varData)){
				ret <- try(get.Data.spiViz(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				imgContainer <- SPICalc.Display.VizTS(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvSPICalcPlot$notebookTab.spiViz, AllOpenTabType, AllOpenTabData)
				EnvSPICalcPlot$notebookTab.spiViz <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		###############

		tkgrid(bt.VizTS, row = 0, column = 0, sticky = 'we', padx = 3, ipadx = 1, pady = 1)
		tkgrid(bt.VizOpt, row = 0, column = 1, sticky = 'we', padx = 3, ipadx = 1, pady = 1)
		tkgrid(txt.VizTS, row = 1, column = 0, sticky = 'e', padx = 3, ipadx = 1, pady = 1)
		tkgrid(en.VizTS, row = 1, column = 1, sticky = 'w', padx = 3, ipadx = 1, pady = 1)

		##############################################

		tkgrid(frameDataTS, row = 0, column = 0, sticky = 'we', pady = 1)
		tkgrid(frameSTNCrds, row = 1, column = 0, sticky = '', pady = 3)
		tkgrid(frameVizTS, row = 2, column = 0, sticky = '', pady = 3)

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

		##############################################

		frameSHP <- ttklabelframe(subfr4, text = "Boundaries", relief = 'groove')

		EnvSPICalcPlot$shp$add.shp <- tclVar(FALSE)
		file.plotShp <- tclVar()
		stateSHP <- "disabled"

		chk.addshp <- tkcheckbutton(frameSHP, variable = EnvSPICalcPlot$shp$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
		bt.addshpOpt <- ttkbutton(frameSHP, text = "Options", state = stateSHP)
		cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur1, state = stateSHP)
		bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

		########
		tkconfigure(bt.addshp, command = function(){
			shp.opfiles <- getOpenShp(main.win, all.opfiles)
			if(!is.null(shp.opfiles)){
				nopf <- length(AllOpenFilesType)
				AllOpenFilesType[[nopf+1]] <<- 'shp'
				AllOpenFilesData[[nopf+1]] <<- shp.opfiles
				tclvalue(file.plotShp) <- AllOpenFilesData[[nopf+1]][[1]]
				listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]

				tkconfigure(cb.addshp, values = unlist(listOpenFiles), textvariable = file.plotShp)

				shpofile <- getShpOpenData(file.plotShp)
				if(is.null(shpofile)) EnvSPICalcPlot$shp$ocrds <- NULL
				EnvSPICalcPlot$shp$ocrds <- getBoundaries(shpofile[[2]])
			}else return(NULL)
		})

		########
		EnvSPICalcPlot$SHPOp <- list(col = "black", lwd = 1.5)

		tkconfigure(bt.addshpOpt, command = function(){
			EnvSPICalcPlot$SHPOp <- MapGraph.GraphOptions.LineSHP(main.win, EnvSPICalcPlot$SHPOp)
		})

		########
		tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
		tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		#################
		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile)) EnvSPICalcPlot$shp$ocrds <- NULL
			EnvSPICalcPlot$shp$ocrds <- getBoundaries(shpofile[[2]])
		})

		tkbind(chk.addshp, "<Button-1>", function(){
			stateSHP <- if(tclvalue(EnvSPICalcPlot$shp$add.shp) == "1") "disabled" else "normal"
			tkconfigure(cb.addshp, state = stateSHP)
			tkconfigure(bt.addshp, state = stateSHP)
			tkconfigure(bt.addshpOpt, state = stateSHP)
		})

		##############################################

		tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', pady = 1)

	#######################################################################################################

	widgets.Station.Pixel <- function(){
		tkdestroy(frTS2)
		frTS2 <<- tkframe(frameSTNCrds)

		if(EnvSPICalcPlot$output$params$data.type == "cdtstation"){
			stnIDTSPLOT <- EnvSPICalcPlot$output$data$id
			txt.stnSel <- tklabel(frTS2, text = "Select a station to plot")
			bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = 6)
			bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = 6)
			cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = EnvSPICalcPlot$graph$stnIDTSp, width = largeur5)
			tclvalue(EnvSPICalcPlot$graph$stnIDTSp) <- stnIDTSPLOT[1]

			tkconfigure(bt.stnID.prev, command = function(){
				if(!is.null(EnvSPICalcPlot$varData)){
					istn <- which(stnIDTSPLOT == str_trim(tclvalue(EnvSPICalcPlot$graph$stnIDTSp)))
					istn <- istn-1
					if(istn < 1) istn <- length(stnIDTSPLOT)
					tclvalue(EnvSPICalcPlot$graph$stnIDTSp) <- stnIDTSPLOT[istn]

					imgContainer <- SPICalc.Display.Graph(tknotes)
					retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvSPICalcPlot$notebookTab.dataGraph, AllOpenTabType, AllOpenTabData)
					EnvSPICalcPlot$notebookTab.dataGraph <- retNBTab$notebookTab
					AllOpenTabType <<- retNBTab$AllOpenTabType
					AllOpenTabData <<- retNBTab$AllOpenTabData
				}
			})

			tkconfigure(bt.stnID.next, command = function(){
				if(!is.null(EnvSPICalcPlot$varData)){
					istn <- which(stnIDTSPLOT == str_trim(tclvalue(EnvSPICalcPlot$graph$stnIDTSp)))
					istn <- istn+1
					if(istn > length(stnIDTSPLOT)) istn <- 1
					tclvalue(EnvSPICalcPlot$graph$stnIDTSp) <- stnIDTSPLOT[istn]

					imgContainer <- SPICalc.Display.Graph(tknotes)
					retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvSPICalcPlot$notebookTab.dataGraph, AllOpenTabType, AllOpenTabData)
					EnvSPICalcPlot$notebookTab.dataGraph <- retNBTab$notebookTab
					AllOpenTabType <<- retNBTab$AllOpenTabType
					AllOpenTabData <<- retNBTab$AllOpenTabData
				}
			})

			tkgrid(txt.stnSel, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}else{
			txt.crdSel <- tklabel(frTS2, text = "Enter longitude and latitude to plot", anchor = 'w', justify = 'left')
			txt.lonLoc <- tklabel(frTS2, text = "Longitude", anchor = 'e', justify = 'right')
			en.lonLoc <- tkentry(frTS2, textvariable = EnvSPICalcPlot$graph$lonLOC, width = 8)
			txt.latLoc <- tklabel(frTS2, text = "Latitude", anchor = 'e', justify = 'right')
			en.latLoc <- tkentry(frTS2, textvariable = EnvSPICalcPlot$graph$latLOC, width = 8)
			stnIDTSPLOT <- ""
			tclvalue(EnvSPICalcPlot$graph$stnIDTSp) <- ""

			tkgrid(txt.crdSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.lonLoc, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.lonLoc, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.latLoc, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.latLoc, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)
		return(0)
	}

	#################

	set.Data.Scales <- function(){
		path.data <- file.path(EnvSPICalcPlot$PathData, "CDTDATASET")
		spi.tscales <- list.files(path.data, "SPI_.+")
		if(length(spi.tscales) == 0){
			InsertMessagesTxt(main.txt.out, 'No SPI data found', format = TRUE)
			return(NULL)
		}
		if(EnvSPICalcPlot$output$params$data.type == "cdtstation")
			spi.tscales <- file_path_sans_ext(spi.tscales)

		nch <- nchar(spi.tscales)
		tsc <- str_pad(substr(spi.tscales, 5, nch-3), 2, pad = "0")
		scales <- substr(spi.tscales, nch-2, nch)

		spi.tscalesF <- spi.tscales[order(paste0(scales, tsc))]
		spi.tscales <- paste0("SPI-", as.numeric(tsc), "-", ifelse(scales == "dek", "Dekad", "Month"))
		spi.tscales <- spi.tscales[order(paste0(scales, tsc))]

		EnvSPICalcPlot$varData$spi$disp <- spi.tscales
		EnvSPICalcPlot$varData$spi$dataF <- spi.tscalesF

		tkconfigure(cb.spi.maps, values = spi.tscales)
		tclvalue(EnvSPICalcPlot$spi.tscale) <- spi.tscales[1]
		return(0)
	}

	#################

	set.Data.Dates <- function(){
		path.data <- file.path(EnvSPICalcPlot$PathData, "CDTDATASET")
		spi_scale <- str_trim(tclvalue(EnvSPICalcPlot$spi.tscale))

		ipos <- which(EnvSPICalcPlot$varData$spi$disp %in% spi_scale)
		tscale.data <- EnvSPICalcPlot$varData$spi$dataF[ipos]
		file.index <- if(EnvSPICalcPlot$output$params$data.type == "cdtstation")
			file.path(path.data, paste0(tscale.data, ".rds"))
			else file.path(path.data, tscale.data, paste0(tscale.data, ".rds"))

		if(!file.exists(file.index)){
			InsertMessagesTxt(main.txt.out, paste(file.index, 'not found'), format = TRUE)
			return(NULL)
		}

		read.cdt.dataIdx <- TRUE
		if(!is.null(EnvSPICalcPlot$cdtdataset))
			if(!is.null(EnvSPICalcPlot$file.index))
				if(EnvSPICalcPlot$file.index == file.index) read.cdt.dataIdx <- FALSE
		if(read.cdt.dataIdx){
			cdtdataset <- readRDS(file.index)
			daty <- if(EnvSPICalcPlot$output$params$data.type == "cdtstation") cdtdataset$date else cdtdataset$dateInfo$date

			tkconfigure(cb.spi.Date, values = daty)
			tclvalue(EnvSPICalcPlot$spi.date) <- daty[length(daty)]

			EnvSPICalcPlot$varData$ts$step <- strsplit(spi_scale, "-")[[1]][3]
			EnvSPICalcPlot$varData$ts$dates <- daty
			EnvSPICalcPlot$cdtdataset <- cdtdataset
			EnvSPICalcPlot$cdtdataset$fileInfo <- file.index
			EnvSPICalcPlot$file.index <- file.index
		}

		return(0)
	}

	#################

	get.Data.Map <- function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		this.daty <- str_trim(tclvalue(EnvSPICalcPlot$spi.date))

		readVarData <- TRUE
		if(!is.null(EnvSPICalcPlot$varData))
			if(!is.null(EnvSPICalcPlot$varData$spi$this.daty))
				if(EnvSPICalcPlot$varData$spi$this.daty == this.daty) readVarData <- FALSE

		if(readVarData){
			if(EnvSPICalcPlot$output$params$data.type == "cdtstation"){
				idt <- which(EnvSPICalcPlot$cdtdataset$date == this.daty)
				x <- EnvSPICalcPlot$output$data$lon
				y <- EnvSPICalcPlot$output$data$lat
				tmp <- as.numeric(EnvSPICalcPlot$cdtdataset$spi[idt, ])

				nx <- nx_ny_as.image(diff(range(x)))
				ny <- nx_ny_as.image(diff(range(y)))

				tmp <- cdt.as.image(tmp, nx = nx, ny = ny, pts.xy = cbind(x, y))
				EnvSPICalcPlot$varData$map$x <- tmp$x
				EnvSPICalcPlot$varData$map$y <- tmp$y
				EnvSPICalcPlot$varData$map$z <- tmp$z
			}else{
				ipos <- which(EnvSPICalcPlot$varData$spi$disp %in% str_trim(tclvalue(EnvSPICalcPlot$spi.tscale)))
				tscale.data <- EnvSPICalcPlot$varData$spi$dataF[ipos]

				nc.file <- file.path(EnvSPICalcPlot$PathData, "DATA_NetCDF", tscale.data, paste0("spi_", this.daty, ".nc"))
				nc <- nc_open(nc.file)
				EnvSPICalcPlot$varData$map$x <- nc$dim[[1]]$vals
				EnvSPICalcPlot$varData$map$y <- nc$dim[[2]]$vals
				EnvSPICalcPlot$varData$map$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
				nc_close(nc)
			}

			EnvSPICalcPlot$varData$spi$this.daty <- this.daty
		}
	}

	#################

	get.Data.spiViz <- function(){
		file.mon <- file.path(EnvSPICalcPlot$PathData, "MONTHLY_data")
		file.dek <- file.path(EnvSPICalcPlot$PathData, "DEKADAL_data")

		if(file.exists(file.mon)){
			file.index <- file.mon
			viztstep <- "monthly"
		}else{
			if(file.exists(file.dek)){
				file.index <- file.dek
				viztstep <- "dekadal"
			}else{
				InsertMessagesTxt(main.txt.out, 'No dekadal or monthly data found', format = TRUE)
				return(NULL)
			}
		}

		readspiVizData <- TRUE
		if(!is.null(EnvSPICalcPlot$spiViz))
			if(!is.null(EnvSPICalcPlot$spiViz$tstep))
				if(EnvSPICalcPlot$spiViz$tstep == viztstep) readspiVizData <- FALSE

		if(readspiVizData){
			file.index <- file.path(file.index, paste0(basename(file.index), ".rds"))
			EnvSPICalcPlot$spiViz$cdtdataset <- readRDS(file.index)
			EnvSPICalcPlot$spiViz$cdtdataset$fileInfo <- file.index
			EnvSPICalcPlot$spiViz$tstep <- viztstep
		}

		return(0)
	}

	#######################################################################################################

	tcl('update')
	tkgrid(cmd.frame, sticky = '', pady = 1)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)

	######
	return(cmd.frame)
}

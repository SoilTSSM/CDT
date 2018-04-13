
CessationCalcPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(45)
		hscrlwin1 <- h.scale(27)
		largeur0 <- as.integer(w.scale(22)/sfont0)
		largeur1 <- as.integer(w.scale(27)/sfont0)
		largeur2 <- as.integer(w.scale(29)/sfont0)

		largeur3 <- 36
		largeur4 <- 21
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(50)
		hscrlwin1 <- h.scale(28)
		largeur0 <- as.integer(w.scale(20)/sfont0)
		largeur1 <- as.integer(w.scale(21)/sfont0)
		largeur2 <- as.integer(w.scale(23)/sfont0)

		largeur3 <- 33
		largeur4 <- 14
	}

	# GeneralParameters <- fromJSON(file.path(apps.dir, 'init_params', 'ClimatoAnalysis.json'))
	MOIS <- format(ISOdate(2014, 1:12, 1), "%b")
	GeneralParameters <- list(method = 1, data.type = "cdtstation", 
							cdtstation = list(wb = "", prec = ""),
							cdtdataset = list(wb = "", prec = ""),
							onset.def = list(min.wb = 5, total.days = 3,
											thres.rain.day = 0.85, 
											earliest = list(month = 12, day = 15),
											latest = list(month = 2, day = 15)),
							min.frac = 0.95,
							onset.reg = list(region = "One", subdiv = "Latitude",
											lat = list(nb = 2, div = list(8)),
											shp = list(file = "", attr = "")),
							output = "")

	###################

	cmd.frame <- tkframe(panel.left)

	tknote.cmd <- bwNoteBook(cmd.frame)
	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Rainy Season")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Maps")
	cmd.tab4 <- bwAddTab(tknote.cmd, text = "Graphs")
	cmd.tab5 <- bwAddTab(tknote.cmd, text = "Boundaries")

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

		############################################

		# frameMthd <- ttklabelframe(subfr1, text = "Cessation Method", relief = 'groove')

		# onset.method <- tclVar(GeneralParameters$method)
		# critere.method <- switch(GeneralParameters$method,
		# 						"1" = "Water Balance",
		# 						"2" = "Precipitation",
		# 						"3" = "Automatic (Dunning et al. (2016))")
		# txt.method <- tclVar(critere.method)

		# txt.mthd1 <- tklabel(frameMthd, text = "Cessation method", anchor = 'e', justify = 'right')
		# cb.mthd <- ttkcombobox(frameMthd, values = 1:3, textvariable = onset.method, width = 1)
		# txt.mthd2 <- tklabel(frameMthd, text = "criterion based on", anchor = 'w', justify = 'left')
		# txt.mthd3 <- tklabel(frameMthd, text = tclvalue(txt.method), textvariable = txt.method, anchor = 'w', justify = 'left', width = largeur3)

		# tkgrid(txt.mthd1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		# tkgrid(cb.mthd, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		# tkgrid(txt.mthd2, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		# tkgrid(txt.mthd3, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		# ############

		# tkbind(cb.mthd, "<<ComboboxSelected>>", function(){
		# 	tclvalue(txt.method) <- switch(str_trim(tclvalue(onset.method)),
		# 									"1" = "Water Balance",
		# 									"2" = "Precipitation",
		# 									"3" = "Automatic (Dunning et al. (2016))")

		# 	statePrec <- if(str_trim(tclvalue(onset.method)) == '1') 'disabled' else 'normal'
		# 	tkconfigure(cb.en.INPrec, state = statePrec)
		# 	tkconfigure(bt.INPrec, state = statePrec)
		# })

		############################################

		frameInData <- ttklabelframe(subfr1, text = "Input Data", relief = 'groove')

		DataType <- tclVar()
		CbdatatypeVAL <- c('CDT stations data format', 'CDT dataset format (gridded)')
		tclvalue(DataType) <- switch(GeneralParameters$data.type,
									'cdtstation' = CbdatatypeVAL[1],
									'cdtdataset' = CbdatatypeVAL[2])

		if(GeneralParameters$data.type == 'cdtstation'){
			input.WB <- tclVar(GeneralParameters$cdtstation$wb)
			# input.Prec <- tclVar(GeneralParameters$cdtstation$prec)
			txt.INWB <- 'File containing stations daily water balance'
			# txt.INPrec <- 'File containing stations daily Precip data'
		}else{
			input.WB <- tclVar(GeneralParameters$cdtdataset$wb)
			# input.Prec <- tclVar(GeneralParameters$cdtdataset$prec)
			txt.INWB <- 'Index file (*.rds) for daily water balance'
			# txt.INPrec <- 'Index file (*.rds) for daily Precip data'
		}
		txt.INWB.var <- tclVar(txt.INWB)
		# txt.INPrec.var <- tclVar(txt.INPrec)

		# statePrec <- if(GeneralParameters$method == 1) 'disabled' else 'normal'

		txt.datatype <- tklabel(frameInData, text = "Format", anchor = 'w', justify = 'left')
		cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)

		txt.INWB <- tklabel(frameInData, text = tclvalue(txt.INWB.var), textvariable = txt.INWB.var, anchor = 'w', justify = 'left')
		if(GeneralParameters$data.type == 'cdtstation'){
			cb.en.INWB <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.WB, width = largeur1)
		}else{
			cb.en.INWB <- tkentry(frameInData, textvariable = input.WB, width = largeur2)
		}
		bt.INWB <- tkbutton(frameInData, text = "...")

		# txt.INPrec <- tklabel(frameInData, text = tclvalue(txt.INPrec.var), textvariable = txt.INPrec.var, anchor = 'w', justify = 'left')
		# if(GeneralParameters$data.type == 'cdtstation'){
		# 	cb.en.INPrec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur1, state = statePrec)
		# }else{
		# 	cb.en.INPrec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2, state = statePrec)
		# }
		# bt.INPrec <- tkbutton(frameInData, text = "...", state = statePrec)

		############

		tkconfigure(bt.INWB, command = function(){
			if(GeneralParameters$data.type == 'cdtstation'){
				dat.opfiles <- getOpenFiles(main.win, all.opfiles)
				if(!is.null(dat.opfiles)){
					nopf <- length(AllOpenFilesType)
					AllOpenFilesType[[nopf+1]] <<- 'ascii'
					AllOpenFilesData[[nopf+1]] <<- dat.opfiles

					listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
					tclvalue(input.WB) <- AllOpenFilesData[[nopf+1]][[1]]
					tkconfigure(cb.en.INWB, values = unlist(listOpenFiles), textvariable = input.WB)
					# tkconfigure(cb.en.INPrec, values = unlist(listOpenFiles), textvariable = input.Prec)
				}else return(NULL)
			}else{
				filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
				tclvalue(input.WB) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
			}
		})

		# tkconfigure(bt.INPrec, command = function(){
		# 	if(GeneralParameters$data.type == 'cdtstation'){
		# 		dat.opfiles <- getOpenFiles(main.win, all.opfiles)
		# 		if(!is.null(dat.opfiles)){
		# 			nopf <- length(AllOpenFilesType)
		# 			AllOpenFilesType[[nopf+1]] <<- 'ascii'
		# 			AllOpenFilesData[[nopf+1]] <<- dat.opfiles

		# 			listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
		# 			tclvalue(input.Prec) <- AllOpenFilesData[[nopf+1]][[1]]
		# 			tkconfigure(cb.en.INWB, values = unlist(listOpenFiles), textvariable = input.WB)
		# 			tkconfigure(cb.en.INPrec, values = unlist(listOpenFiles), textvariable = input.Prec)
		# 		}else return(NULL)
		# 	}else{
		# 		filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
		# 		path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
		# 		tclvalue(input.Prec) <- if(path.rds%in%c("", "NA") | is.na(path.rds)) "" else path.rds
		# 	}
		# })

		############
		tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.INWB, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.en.INWB, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.INWB, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		# tkgrid(txt.INPrec, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		# tkgrid(cb.en.INPrec, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		# tkgrid(bt.INPrec, row = 4, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		############
		infobulle(cb.datatype, 'Select the format of the input data')
		status.bar.display(cb.datatype, TextOutputVar, 'Select the format of the input data')

		if(GeneralParameters$data.type == 'cdtstation'){
			infobulle(cb.en.INWB, 'Select the file containing the daily water balance')
			status.bar.display(cb.en.INWB, TextOutputVar, 'Select the file containing the daily water balance')
			# infobulle(cb.en.INPrec, 'Select the file containing the daily precipitation')
			# status.bar.display(cb.en.INPrec, TextOutputVar, 'Select the file containing the daily precipitation')

			infobulle(bt.INWB, 'Browse file if not listed')
			status.bar.display(bt.INWB, TextOutputVar, 'Browse file if not listed')
			# infobulle(bt.INPrec, 'Browse file if not listed')
			# status.bar.display(bt.INPrec, TextOutputVar, 'Browse file if not listed')
		}else{
			infobulle(cb.en.INWB, 'Enter the full path to the file <daily water balance dataset name>.rds')
			status.bar.display(cb.en.INWB, TextOutputVar, 'Enter the full path to the file <daily water balance dataset name>.rds')
			# infobulle(cb.en.INPrec, 'Enter the full path to the file <daily precipitation dataset name>.rds')
			# status.bar.display(cb.en.INPrec, TextOutputVar, 'Enter the full path to the file <daily precipitation dataset name>.rds')

			infobulle(bt.INWB, 'or browse here')
			status.bar.display(bt.INWB, TextOutputVar, 'or browse here')
			# infobulle(bt.INPrec, 'or browse here')
			# status.bar.display(bt.INPrec, TextOutputVar, 'or browse here')
		}

		############

		tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
			tkdestroy(cb.en.INWB)
			tclvalue(input.WB) <- ''

			# tkdestroy(cb.en.INPrec)
			# tclvalue(input.Prec) <- ''

			# statePrec <- if(str_trim(tclvalue(onset.method)) == '1') 'disabled' else 'normal'

			###
			if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
				tclvalue(txt.INWB.var) <- 'File containing stations daily water balance'
				# tclvalue(txt.INPrec.var) <- 'File containing stations daily Precip data'

				cb.en.INWB <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.WB, width = largeur1)
				# cb.en.INPrec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur1, state = statePrec)

				######
				tkconfigure(bt.INWB, command = function(){
					dat.opfiles <- getOpenFiles(tt, all.opfiles)
					if(!is.null(dat.opfiles)){
						nopf <- length(AllOpenFilesType)
						AllOpenFilesType[[nopf+1]] <<- 'ascii'
						AllOpenFilesData[[nopf+1]] <<- dat.opfiles

						listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
						tclvalue(input.WB) <- AllOpenFilesData[[nopf+1]][[1]]
						tkconfigure(cb.en.INWB, values = unlist(listOpenFiles), textvariable = input.WB)
						# tkconfigure(cb.en.INPrec, values = unlist(listOpenFiles), textvariable = input.Prec)
					}else return(NULL)
				})

				# tkconfigure(bt.INPrec, state = statePrec, command = function(){
				# 	dat.opfiles <- getOpenFiles(tt, all.opfiles)
				# 	if(!is.null(dat.opfiles)){
				# 		nopf <- length(AllOpenFilesType)
				# 		AllOpenFilesType[[nopf+1]] <<- 'ascii'
				# 		AllOpenFilesData[[nopf+1]] <<- dat.opfiles

				# 		listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
				# 		tclvalue(input.Prec) <- AllOpenFilesData[[nopf+1]][[1]]
				# 		tkconfigure(cb.en.INWB, values = unlist(listOpenFiles), textvariable = input.WB)
				# 		tkconfigure(cb.en.INPrec, values = unlist(listOpenFiles), textvariable = input.Prec)
				# 	}else return(NULL)
				# })

				######
				infobulle(cb.en.INWB, 'Select the file containing the daily water balance')
				status.bar.display(cb.en.INWB, TextOutputVar, 'Select the file containing the daily water balance')
				# infobulle(cb.en.INPrec, 'Select the file containing the daily precipitation')
				# status.bar.display(cb.en.INPrec, TextOutputVar, 'Select the file containing the daily precipitation')

				infobulle(bt.INWB, 'Browse file if not listed')
				status.bar.display(bt.INWB, TextOutputVar, 'Browse file if not listed')
				# infobulle(bt.INPrec, 'Browse file if not listed')
				# status.bar.display(bt.INPrec, TextOutputVar, 'Browse file if not listed')
			}

			###
			if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)'){
				tclvalue(txt.INWB.var) <- 'Index file (*.rds) for daily water balance'
				# tclvalue(txt.INPrec.var) <- 'Index file (*.rds) for daily Precip data'

				cb.en.INWB <- tkentry(frameInData, textvariable = input.WB, width = largeur2)
				# cb.en.INPrec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2, state = statePrec)

				######
				tkconfigure(bt.INWB, command = function(){
					filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
					path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
					tclvalue(input.WB) <- if(path.rds%in%c("", "NA")) "" else path.rds
				})

				# tkconfigure(bt.INPrec, state = statePrec, command = function(){
				# 	filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
				# 	path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
				# 	tclvalue(input.Prec) <- if(path.rds%in%c("", "NA")) "" else path.rds
				# })

				######
				infobulle(cb.en.INWB, 'Enter the full path to the file <daily water balance dataset name>.rds')
				status.bar.display(cb.en.INWB, TextOutputVar, 'Enter the full path to the file <daily water balance dataset name>.rds')
				# infobulle(cb.en.INPrec, 'Enter the full path to the file <daily precipitation dataset name>.rds')
				# status.bar.display(cb.en.INPrec, TextOutputVar, 'Enter the full path to the file <daily precipitation dataset name>.rds')

				infobulle(bt.INWB, 'or browse here')
				status.bar.display(bt.INWB, TextOutputVar, 'or browse here')
				# infobulle(bt.INPrec, 'or browse here')
				# status.bar.display(bt.INPrec, TextOutputVar, 'or browse here')
			}

			#######
			tkgrid(cb.en.INWB, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
			# tkgrid(cb.en.INPrec, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)

			#######
			# tkbind(cb.mthd, "<<ComboboxSelected>>", function(){
			# 	statePrec <- if(str_trim(tclvalue(onset.method)) == '1') 'disabled' else 'normal'
			# 	tkconfigure(cb.en.INWB, state = statePrec)
			# 	tkconfigure(bt.INWB, state = statePrec)
			# })
		})

		############################################

		frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		dir.save <- tclVar(GeneralParameters$output)

		txt.dir.save <- tklabel(frameDirSav, text = "Directory to save results",  anchor = 'w', justify = 'left')
		en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur2)
		bt.dir.save <- tkbutton(frameDirSav, text = "...")

		######
		tkconfigure(bt.dir.save, command = function() fileORdir2Save(dir.save, isFile = FALSE))

		######
		tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.dir.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		infobulle(en.dir.save, 'Enter the full path to directory to save outputs')
		status.bar.display(en.dir.save, TextOutputVar, 'Enter the full path to directory to save outputs')
		infobulle(bt.dir.save, 'or browse here')
		status.bar.display(bt.dir.save, TextOutputVar, 'or browse here')

		############################################

		# tkgrid(frameMthd, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab2
	frTab2 <- tkframe(cmd.tab2)
	tkgrid(frTab2, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab2, 0, weight = 1)

	#####
	calcOnset.frame1 <- tkframe(frTab2)
	scrw2 <- bwScrolledWindow(frTab2)
	calcOnset.frame2 <- tkframe(frTab2)

	tkgrid(calcOnset.frame1, row = 0, column = 0, sticky = 'nwe', rowspan = 1, columnspan = 1)
	tkgrid(scrw2, row = 1, column = 0, sticky = '', rowspan = 1, columnspan = 1)
	tkgrid(calcOnset.frame2, row = 2, column = 0, sticky = 'swe', rowspan = 1, columnspan = 1)

	tkgrid.columnconfigure(calcOnset.frame1, 0, weight = 1)
	tkgrid.columnconfigure(scrw2, 0, weight = 1)
	tkgrid.columnconfigure(calcOnset.frame2, 0, weight = 1)

	subfr2 <- bwScrollableFrame(scrw2, width = wscrlwin, height = hscrlwin1)
	tkgrid.columnconfigure(subfr2, 0, weight = 1)

		##############################################

		ONSET.vars <- NULL
		ONSET.vars[[1]]$frame <- tkframe(subfr2)
		tkgrid(ONSET.vars[[1]]$frame)

		##############################################

		OnsetDefinitionInput <- function(){
			########
			tkdestroy(innerRegDef)
			for(i in seq_along(ONSET.vars)) tkdestroy(ONSET.vars[[i]]$frame)
			tcl('update')
			ONSET.vars <<- NULL

			########

			innerRegDef <<- tkframe(frameRegDef)
			if(str_trim(tclvalue(onset.region)) == "Multiple"){
				if(str_trim(tclvalue(onset.subdiv)) == "Latitude"){
					txt.lat.sub <- tklabel(innerRegDef, text = "Number of subdivisions belong latitude", anchor = 'w', justify = 'left')
					cb.lat.sub <- ttkcombobox(innerRegDef, values = 2:7, textvariable = lat.nbdiv, width = 2)

					fr.lat.sub <- tkframe(innerRegDef)
					txt.lat.div <- tklabel(fr.lat.sub, text = "Latitudes:", anchor = 'w', justify = 'left')
					en.lat.div <- NULL
					for(i in seq_along(lat.subdiv)) en.lat.div[[i]] <- tkentry(fr.lat.sub, textvariable = lat.subdiv[[i]], width = 4)
					tkgrid(txt.lat.div, row = 0, column = 0, padx = 1)
					for(i in seq_along(lat.subdiv)){
						j <- if(i < 5) 0 else 1
						k <- if(i < 5) i else i-4
						tkgrid(en.lat.div[[i]], row = j, column = k, padx = 1)
					}

					tkgrid(txt.lat.sub, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, ipadx = 1)
					tkgrid(cb.lat.sub, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, ipadx = 1)
					tkgrid(fr.lat.sub, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, ipadx = 1)

					tkbind(cb.lat.sub, "<<ComboboxSelected>>", function(){
						for(i in seq_along(lat.subdiv)) tkdestroy(en.lat.div[[i]])
						en.lat.div <<- NULL
						lat.subdiv  <<- vector('list', length = as.numeric(str_trim(tclvalue(lat.nbdiv)))-1)
						for(i in seq_along(lat.subdiv)) lat.subdiv[[i]] <<- tclVar()
						for(i in seq_along(lat.subdiv)) en.lat.div[[i]] <<- tkentry(fr.lat.sub, textvariable = lat.subdiv[[i]], width = 4)
						for(i in seq_along(lat.subdiv)){
							j <- if(i < 5) 0 else 1
							k <- if(i < 5) i else i-4
							tkgrid(en.lat.div[[i]], row = j, column = k, padx = 1)
						}
					})
				}else{
					txt.shp.file <- tklabel(innerRegDef, text = "Shapefile for subdivision of regions", anchor = 'w', justify = 'left')
					cb.shp.file <- ttkcombobox(innerRegDef, values = unlist(listOpenFiles), textvariable = shp.file, width = largeur1)
					bt.shp.file <- tkbutton(innerRegDef, text = "...")

					txt.shp.attr <- tklabel(innerRegDef, text = "Attribute to be used to split the region", anchor = 'w', justify = 'left')
					cb.shp.attr <- ttkcombobox(innerRegDef, values = "", textvariable = shp.attr, width = largeur1)

					SHPDATA <- NULL
					tkconfigure(bt.shp.file, command = function(){
						shp.opfiles <- getOpenShp(main.win, all.opfiles)
						if(!is.null(shp.opfiles)){
							nopf <- length(AllOpenFilesType)
							AllOpenFilesType[[nopf+1]] <<- 'shp'
							AllOpenFilesData[[nopf+1]] <<- shp.opfiles

							listOpenFiles[[length(listOpenFiles)+1]] <<- AllOpenFilesData[[nopf+1]][[1]]
							tclvalue(shp.file) <- AllOpenFilesData[[nopf+1]][[1]]
							
							tkconfigure(cb.shp.file, values = unlist(listOpenFiles), textvariable = shp.file)
							tkconfigure(cb.en.INWB, values = unlist(listOpenFiles), textvariable = input.WB)
							# tkconfigure(cb.en.INPrec, values = unlist(listOpenFiles), textvariable = input.Prec)

							shpf <- getShpOpenData(shp.file)
							SHPDATA <<- shpf[[2]]@data
							AttrTable <- names(SHPDATA)
							tkconfigure(cb.shp.attr, values = AttrTable, textvariable = shp.attr)
							tclvalue(shp.attr) <- AttrTable[1]
							tclvalue(shp.AttrTable) <- paste0(as.character(SHPDATA[, AttrTable == tclvalue(shp.attr)]), collapse = '|')
							tcl('update')
						}
					})

					tkgrid(txt.shp.file, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, ipadx = 1)
					tkgrid(cb.shp.file, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, ipadx = 1)
					tkgrid(bt.shp.file, row = 1, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, ipadx = 1)
					tkgrid(txt.shp.attr, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, ipadx = 1)
					tkgrid(cb.shp.attr, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, ipadx = 1)

					######
					tkbind(cb.shp.file, "<<ComboboxSelected>>", function(){
						shpf <- getShpOpenData(shp.file)
						SHPDATA <<- shpf[[2]]@data
						AttrTable <- names(SHPDATA)
						tkconfigure(cb.shp.attr, values = AttrTable, textvariable = shp.attr)
						tclvalue(shp.attr) <- AttrTable[1]
						tclvalue(shp.AttrTable) <- paste0(as.character(SHPDATA[, AttrTable == tclvalue(shp.attr)]), collapse = '|')
					})

					tkbind(cb.shp.attr, "<<ComboboxSelected>>", function(){
						if(!is.null(SHPDATA)){
							AttrTable <- names(SHPDATA)
							tclvalue(shp.AttrTable) <- paste0(as.character(SHPDATA[, AttrTable == tclvalue(shp.attr)]), collapse = '|')
						}
					})
				}

				bt.create.subdv <- tkbutton(innerRegDef, text = "Create Cessation Criteria")

				######
				tkconfigure(bt.create.subdv, command = function(){
					if(str_trim(tclvalue(onset.subdiv)) == "Latitude"){
						lat <- rep(NA, length(lat.subdiv))
						for(i in seq_along(lat.subdiv)) lat[i] <- as.numeric(str_trim(tclvalue(lat.subdiv[[i]])))
						lat <- ifelse(lat > 60 | lat < -60, NA, lat)
						if(any(is.na(lat))){
							lat0 <- sapply(lat.subdiv, tclvalue)
							InsertMessagesTxt(main.txt.out, paste("Invalid latitude", paste0(lat0[is.na(lat)], collapse = " ")), format = TRUE)
							subdiv <- NULL
						}else{
							subdiv <- vector('character', as.numeric(str_trim(tclvalue(lat.nbdiv))))
							if(length(lat) == 1){
								subdiv[1] <- paste('Lat <=', lat[[1]])
								subdiv[2] <- paste('Lat >', lat[[1]])
							}else{
								subdiv[1] <- paste('Lat <=', lat[[1]])
								for(i in 1:(length(lat)-1)) subdiv[i+1] <- paste(lat[[i+1]], '>= Lat >', lat[[i]])
								subdiv[length(subdiv)] <- paste('Lat >', lat[[length(lat)]])
							}
						}
					}else{
						subdiv <- strsplit(tclvalue(shp.AttrTable), '\\|')[[1]]
						subdiv <- substr(subdiv, 1, 20)
						subdiv <- if(length(subdiv) > 0) subdiv else NULL
					}
					if(!is.null(subdiv)){
						tcl("update", "idletasks")
						for(i in seq_along(ONSET.vars)) tkdestroy(ONSET.vars[[i]]$frame)
						ONSET.vars <<- NULL
						tcl("update", "idletasks")
						for(i in seq_along(subdiv))
							ONSET.vars[[i]] <<- OnsetDefinitionCriteria(subfr2, GeneralParameters$onset.def, subdiv = subdiv[i])
					}
				})

				######
				tkgrid(bt.create.subdv, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			}else{
				tcl("update", "idletasks")
				ONSET.vars[[1]] <<- OnsetDefinitionCriteria(subfr2, GeneralParameters$onset.def)
			}

			######
			tkgrid(innerRegDef)
		}

		OnsetDefinitionCriteria <- function(containerFrame, Parameters, subdiv = NA){
			labelReg <- if(is.na(subdiv)) "Cessation Definition" else paste("Cessation Definition - [", subdiv, "]")

			frameOnset <- ttklabelframe(containerFrame, text = labelReg, relief = 'sunken')

			#########
			mon1 <- as.numeric(str_trim(Parameters$earliest$month))
			onset.start.mon <- tclVar(MOIS[mon1])
			onset.start.day <- tclVar(Parameters$earliest$day)

			min.wb <- tclVar(Parameters$min.wb)
			total.days <- tclVar(Parameters$total.days)
			thres.rain.day <- tclVar(Parameters$thres.rain.day)

			mon2 <- as.numeric(str_trim(Parameters$latest$month))
			onset.late.mon <- tclVar(MOIS[mon2])
			onset.late.day <- tclVar(Parameters$latest$day)

			##########

			frEarliest <- tkframe(frameOnset)
			txt.early1 <- tklabel(frEarliest, text = "Earliest cessation date", anchor = 'w', justify = 'left')
			cb.early1 <- ttkcombobox(frEarliest, values = MOIS, textvariable = onset.start.mon, width = 4)
			txt.early2 <- tklabel(frEarliest, text = "Day", anchor = 'w', justify = 'left')
			cb.early2 <- ttkcombobox(frEarliest, values = 1:31, textvariable = onset.start.day, width = 2)
			tkgrid(txt.early1, cb.early1, txt.early2, cb.early2)

			frMinWB <- tkframe(frameOnset)
			txt.minwb1 <- tklabel(frMinWB, text = "Water balance drops below", anchor = 'w', justify = 'left')
			en.minwb <- tkentry(frMinWB, textvariable = min.wb, width = 4)
			txt.minwb2 <- tklabel(frMinWB, text = "mm", anchor = 'w', justify = 'left')
			tkgrid(txt.minwb1, en.minwb, txt.minwb2)

			frWinDay <- tkframe(frameOnset)
			txt.winday1 <- tklabel(frWinDay, text = "for a period of", anchor = 'w', justify = 'left')
			en.winday <- tkentry(frWinDay, textvariable = total.days, width = 3)
			txt.winday2 <- tklabel(frWinDay, text = "days", anchor = 'w', justify = 'left')
			tkgrid(txt.winday1, en.winday, txt.winday2)

			frLastest <- tkframe(frameOnset)
			txt.late1 <- tklabel(frLastest, text = "Latest possible date", anchor = 'w', justify = 'left')
			cb.late1 <- ttkcombobox(frLastest, values = MOIS, textvariable = onset.late.mon, width = 4)
			txt.late2 <- tklabel(frLastest, text = "Day", anchor = 'w', justify = 'left')
			cb.late2 <- ttkcombobox(frLastest, values = 1:31, textvariable = onset.late.day, width = 2)
			tkgrid(txt.late1, cb.late1, txt.late2, cb.late2)

			########
			tkgrid(frEarliest, row = 0, sticky = 'we')
			tkgrid(frMinWB, row = 1, sticky = 'we')
			tkgrid(frWinDay, row = 2, sticky = 'we')
			tkgrid(frLastest, row = 3, sticky = 'we')

			########
			tkgrid(frameOnset, pady = 5)
			tcl('update')

			########
			return(list(frame = frameOnset,
						onset.def = list(min.wb = min.wb, total.days = total.days,
										thres.rain.day = thres.rain.day,
										earliest = list(month = onset.start.mon, day = onset.start.day),
										latest = list(month = onset.late.mon, day = onset.late.day))
				))
		}

		##############################################

		frameRegion <- tkframe(calcOnset.frame1)

		onset.region <- tclVar(GeneralParameters$onset.reg$region)
		onset.subdiv <- tclVar(GeneralParameters$onset.reg$subdiv)

		stateSubdv <- if(GeneralParameters$onset.reg$region == "One") "disabled" else "normal"

		txt.reg <- tklabel(frameRegion, text = "Region", anchor = 'w', justify = 'left')
		cb.reg <- ttkcombobox(frameRegion, values = c("One", "Multiple"), textvariable = onset.region, width = 6)
		txt.subdv <- tklabel(frameRegion, text = "Subdivision from", anchor = 'w', justify = 'left')
		cb.subdv <- ttkcombobox(frameRegion, values = c("Latitude", "Shapefile"), textvariable = onset.subdiv, width = 8, state = stateSubdv)

		tkgrid(txt.reg, cb.reg, txt.subdv, cb.subdv)

		######
		tkbind(cb.reg, "<<ComboboxSelected>>", function(){
			stateSubdv <- if(str_trim(tclvalue(onset.region)) == "One") "disabled" else "normal"
			tkconfigure(cb.subdv, state = stateSubdv)

			OnsetDefinitionInput()
			tcl('update')
		})

		tkbind(cb.subdv, "<<ComboboxSelected>>", function(){
			OnsetDefinitionInput()
			tcl('update')
		})

		########################

		frameRegDef <- tkframe(calcOnset.frame1)

		shp.file <- tclVar(GeneralParameters$onset.reg$shp$file)
		shp.attr <- tclVar(GeneralParameters$onset.reg$shp$attr)
		shp.AttrTable <- tclVar()
		lat.nbdiv <- tclVar(GeneralParameters$onset.reg$lat$nb)
		lat.subdiv  <- vector('list', length = GeneralParameters$onset.reg$lat$nb-1)
		for(i in seq_along(lat.subdiv)) lat.subdiv[[i]] <- tclVar(GeneralParameters$onset.reg$lat$div[[i]])

		innerRegDef <- tkframe(frameRegDef)
		OnsetDefinitionInput()
		tcl('update')

		########################
		tkgrid(frameRegion, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameRegDef, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

		##############################################

		if(!is.null(EnvCessationCalcPlot$DirExist)){
			stateCaclBut <- if(tclvalue(EnvCessationCalcPlot$DirExist) == "1") "normal" else "disabled"
		}else stateCaclBut <- "normal"

		bt.CalcOnset <- tkbutton(calcOnset.frame2, text = 'Calculate Cessation', state = stateCaclBut, bg = 'lightgreen')

		tkconfigure(bt.CalcOnset, command = function(){
			# GeneralParameters$method <- as.numeric(str_trim(tclvalue(onset.method)))
			GeneralParameters$data.type <- switch(str_trim(tclvalue(DataType)),
												'CDT stations data format' = 'cdtstation',
												'CDT dataset format (gridded)' = 'cdtdataset')

			if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
				GeneralParameters$cdtstation$wb <- str_trim(tclvalue(input.WB))
				# GeneralParameters$cdtstation$prec <- str_trim(tclvalue(input.Prec))
			}

			if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)'){
				GeneralParameters$cdtdataset$wb <- str_trim(tclvalue(input.WB))
				# GeneralParameters$cdtdataset$prec <- str_trim(tclvalue(input.Prec))
			}

			GeneralParameters$output <- str_trim(tclvalue(dir.save))

			GeneralParameters$onset.reg$region <- str_trim(tclvalue(onset.region))
			GeneralParameters$onset.reg$subdiv <- str_trim(tclvalue(onset.subdiv))

			if(str_trim(tclvalue(onset.region)) == "Multiple"){
				if(str_trim(tclvalue(onset.subdiv)) == "Latitude"){
					GeneralParameters$onset.reg$lat$nb <- as.numeric(str_trim(tclvalue(lat.nbdiv)))
					GeneralParameters$onset.reg$lat$div <- lapply(lat.subdiv, function(x) as.numeric(str_trim(tclvalue(x))))
				}

				if(str_trim(tclvalue(onset.subdiv)) == "Shapefile"){
					GeneralParameters$onset.reg$shp$file <- str_trim(tclvalue(shp.file))
					GeneralParameters$onset.reg$shp$attr <- str_trim(tclvalue(shp.attr))
				}
			}

			GeneralParameters$onset.criteria <- lapply(ONSET.vars, function(x){
														x <- x$onset.def
														list(
															min.wb = as.numeric(str_trim(tclvalue(x$min.wb))),
															total.days = as.numeric(str_trim(tclvalue(x$total.days))),
															thres.rain.day = as.numeric(str_trim(tclvalue(x$thres.rain.day))),
															earliest = list(month = which(MOIS%in%str_trim(tclvalue(x$earliest$month))),
																			day = as.numeric(str_trim(tclvalue(x$earliest$day)))),
															latest = list(month = which(MOIS%in%str_trim(tclvalue(x$latest$month))),
																			day = as.numeric(str_trim(tclvalue(x$latest$day))))
														)
													})

			nbcriteria <- length(unlist(GeneralParameters$onset.def)) * length(GeneralParameters$onset.criteria)
			criteria <- unlist(GeneralParameters$onset.criteria)
			if(any(is.na(criteria)) | length(criteria) != nbcriteria){
				InsertMessagesTxt(main.txt.out, "Invalid input found on cessation definition criteria", format = TRUE)
				return(NULL)
			}

			# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

			tkconfigure(main.win, cursor = 'watch')
			InsertMessagesTxt(main.txt.out, "Calculate Season Cessation ......")

			ret <- tryCatch(
				compute_SeasonCessation_Procs(GeneralParameters),
				#warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = tkconfigure(main.win, cursor = '')
			)

			msg0 <- "Season Cessation calculation finished successfully"
			msg1 <- "Season Cessation calculation failed"

			if(!is.null(ret)){
				if(ret == 0){
					InsertMessagesTxt(main.txt.out, msg0)

					###################
					set.Data.Dates()
					widgets.Station.Pixel()
					res <- EnvCessationCalcPlot$read.Data.Map()
					if(inherits(res, "try-error") | is.null(res)) return(NULL)
				}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
			}else InsertMessagesTxt(main.txt.out, msg1, format = TRUE)
		})

		####################

		tkgrid(bt.CalcOnset, row = 0, column = 0, sticky = 'we', pady = 1)

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

		frameDataExist <- ttklabelframe(subfr3, text = "Cessation data", relief = 'groove')

		EnvCessationCalcPlot$DirExist <- tclVar(0)
		file.dataIndex <- tclVar()

		stateExistData <- if(tclvalue(EnvCessationCalcPlot$DirExist) == "1") "normal" else "disabled"

		chk.dataIdx <- tkcheckbutton(frameDataExist, variable = EnvCessationCalcPlot$DirExist, text = "Cessation data already computed", anchor = 'w', justify = 'left')
		en.dataIdx <- tkentry(frameDataExist, textvariable = file.dataIndex, width = largeur2, state = stateExistData)
		bt.dataIdx <- tkbutton(frameDataExist, text = "...", state = stateExistData)

		tkconfigure(bt.dataIdx, command = function(){
			filetypes <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"
			path.dataIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
			if(path.dataIdx%in%c("", "NA") | is.na(path.dataIdx)) return(NULL)
			tclvalue(file.dataIndex) <- path.dataIdx

			if(file.exists(str_trim(tclvalue(file.dataIndex)))){
				OutIndexdata <- try(readRDS(str_trim(tclvalue(file.dataIndex))), silent = TRUE)
				if(inherits(OutIndexdata, "try-error")){
					InsertMessagesTxt(main.txt.out, 'Unable to load cessation data', format = TRUE)
					InsertMessagesTxt(main.txt.out, gsub('[\r\n]', '', OutIndexdata[1]), format = TRUE)
					tkconfigure(cb.data.Index, values = "")
					tclvalue(EnvCessationCalcPlot$donDate) <- ""
					return(NULL)
				}

				EnvCessationCalcPlot$output <- OutIndexdata
				EnvCessationCalcPlot$PathData <- dirname(str_trim(tclvalue(file.dataIndex)))

				###################
				set.Data.Dates()
				widgets.Station.Pixel()
				ret <- EnvCessationCalcPlot$read.Data.Map()
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		tkgrid(chk.dataIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.dataIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.dataIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(chk.dataIdx, "<Button-1>", function(){
			stateExistData <- if(tclvalue(EnvCessationCalcPlot$DirExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.dataIdx, state = stateExistData)
			tkconfigure(bt.dataIdx, state = stateExistData)
			stateCaclBut <- if(tclvalue(EnvCessationCalcPlot$DirExist) == '1') 'normal' else 'disabled'
			tkconfigure(bt.CalcOnset, state = stateCaclBut)
		})

		##############################################

		frameDataMap <- ttklabelframe(subfr3, text = "Cessation Map", relief = 'groove')

		EnvCessationCalcPlot$donDate <- tclVar()

		cb.data.Index <- ttkcombobox(frameDataMap, values = "", textvariable = EnvCessationCalcPlot$donDate, width = largeur4)
		bt.data.Index.prev <- ttkbutton(frameDataMap, text = "<<", width = 3)
		bt.data.Index.next <- ttkbutton(frameDataMap, text = ">>", width = 3)
		bt.data.maps <- ttkbutton(frameDataMap, text = "PLOT", width = 7)
		bt.data.MapOpt <- ttkbutton(frameDataMap, text = "Options", width = 7)

		###############

		EnvCessationCalcPlot$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
												userCol = list(custom = FALSE, color = NULL),
												userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
												title = list(user = FALSE, title = ''),
												colkeyLab = list(user = FALSE, label = ''),
												scalebar = list(add = FALSE, pos = 'bottomleft'))

		tkconfigure(bt.data.MapOpt, command = function(){
			if(!is.null(EnvCessationCalcPlot$varData$map)){
				atlevel <- pretty(EnvCessationCalcPlot$varData$map$z, n = 10, min.n = 7)
				if(is.null(EnvCessationCalcPlot$dataMapOp$userLvl$levels)){
					EnvCessationCalcPlot$dataMapOp$userLvl$levels <- atlevel
				}else{
					if(!EnvCessationCalcPlot$dataMapOp$userLvl$custom)
						EnvCessationCalcPlot$dataMapOp$userLvl$levels <- atlevel
				}
			}
			EnvCessationCalcPlot$dataMapOp <- climatoAnalysis.MapOptions(main.win, EnvCessationCalcPlot$dataMapOp)
		})

		#########
		EnvCessationCalcPlot$notebookTab.dataMap <- NULL

		tkconfigure(bt.data.maps, command = function(){
			if(str_trim(tclvalue(EnvCessationCalcPlot$donDate)) != "" &
				!is.null(EnvCessationCalcPlot$varData))
			{
				imgContainer <- CessationCalc.Display.Maps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvCessationCalcPlot$notebookTab.dataMap, AllOpenTabType, AllOpenTabData)
				EnvCessationCalcPlot$notebookTab.dataMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.data.Index.prev, command = function(){
			if(str_trim(tclvalue(EnvCessationCalcPlot$donDate)) != ""){
				donDates <- format(EnvCessationCalcPlot$output$start.date, "%Y")
				idaty <- which(donDates == str_trim(tclvalue(EnvCessationCalcPlot$donDate)))
				idaty <- idaty-1
				if(idaty < 1) idaty <- length(donDates)
				tclvalue(EnvCessationCalcPlot$donDate) <- donDates[idaty]

				ret <- try(EnvCessationCalcPlot$read.Data.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				imgContainer <- CessationCalc.Display.Maps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvCessationCalcPlot$notebookTab.dataMap, AllOpenTabType, AllOpenTabData)
				EnvCessationCalcPlot$notebookTab.dataMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		tkconfigure(bt.data.Index.next, command = function(){
			if(str_trim(tclvalue(EnvCessationCalcPlot$donDate)) != ""){
				donDates <- format(EnvCessationCalcPlot$output$start.date, "%Y")
				idaty <- which(donDates == str_trim(tclvalue(EnvCessationCalcPlot$donDate)))
				idaty <- idaty+1
				if(idaty > length(donDates)) idaty <- 1
				tclvalue(EnvCessationCalcPlot$donDate) <- donDates[idaty]

				ret <- try(EnvCessationCalcPlot$read.Data.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				imgContainer <- CessationCalc.Display.Maps(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvCessationCalcPlot$notebookTab.dataMap, AllOpenTabType, AllOpenTabData)
				EnvCessationCalcPlot$notebookTab.dataMap <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		###############

		tkgrid(bt.data.Index.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.data.Index, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.data.Index.next, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.data.maps, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.data.MapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		###############

		tkbind(cb.data.Index, "<<ComboboxSelected>>", function(){
			if(!is.null(EnvCessationCalcPlot$varData)){
				ret <- try(EnvCessationCalcPlot$read.Data.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		##############################################

		tkgrid(frameDataExist, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameDataMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

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

		frameDataTS <- ttklabelframe(subfr4, text = "Cessation Graph", relief = 'groove')

		typeTSPLOT <- c("Line", "Barplot")
		EnvCessationCalcPlot$graph$typeTSp <- tclVar("Line")

		cb.typeTSp <- ttkcombobox(frameDataTS, values = typeTSPLOT, textvariable = EnvCessationCalcPlot$graph$typeTSp, width = largeur4)
		bt.TsGraph.plot <- ttkbutton(frameDataTS, text = "PLOT", width = 7)
		bt.TSGraphOpt <- ttkbutton(frameDataTS, text = "Options", width = 8)

		#################

		EnvCessationCalcPlot$TSGraphOp <- list(
					bar = list(
							xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
							ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
							axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
							title = list(is.title = FALSE, title = '', position = 'top'),
							colors = list(col = "darkblue")
						),
					line = list(
						xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
						ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
						axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
						title = list(is.title = FALSE, title = '', position = 'top'),
						plot = list(type = 'both',
							col = list(line = "red", points = "blue"),
							lwd = 2, cex = 1.4),
						legend = NULL)
					)

		tkconfigure(bt.TSGraphOpt, command = function(){
			suffix.fun <- switch(str_trim(tclvalue(EnvCessationCalcPlot$graph$typeTSp)),
									"Barplot" = "Bar",
									"Line" = "Line")
			plot.fun <- match.fun(paste0("climatoAnalysis.GraphOptions.", suffix.fun))
			EnvCessationCalcPlot$TSGraphOp <- plot.fun(main.win, EnvCessationCalcPlot$TSGraphOp)
		})

		#########
		EnvCessationCalcPlot$notebookTab.dataGraph <- NULL

		tkconfigure(bt.TsGraph.plot, command = function(){
			if(!is.null(EnvCessationCalcPlot$varData)){
				imgContainer <- CessationCalc.Display.Graph(tknotes)
				retNBTab <- imageNotebookTab_unik(tknotes, imgContainer, EnvCessationCalcPlot$notebookTab.dataGraph, AllOpenTabType, AllOpenTabData)
				EnvCessationCalcPlot$notebookTab.dataGraph <- retNBTab$notebookTab
				AllOpenTabType <<- retNBTab$AllOpenTabType
				AllOpenTabData <<- retNBTab$AllOpenTabData
			}
		})

		#################

		tkgrid(cb.typeTSp, row = 0, column = 0, sticky = 'we', pady = 1, columnspan = 1)
		tkgrid(bt.TSGraphOpt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
		tkgrid(bt.TsGraph.plot, row = 0, column = 2, sticky = 'we', pady = 1, columnspan = 1)

		##############################################

		frameSTNCrds <- ttklabelframe(subfr4, text = "Station/Coordinates", relief = 'groove')

		frTS2 <- tkframe(frameSTNCrds)
		EnvCessationCalcPlot$graph$lonLOC <- tclVar()
		EnvCessationCalcPlot$graph$latLOC <- tclVar()
		EnvCessationCalcPlot$graph$stnIDTSp <- tclVar()

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

		##############################################

		tkgrid(frameDataTS, row = 0, column = 0, sticky = 'we', pady = 1)
		tkgrid(frameSTNCrds, row = 1, column = 0, sticky = 'we', pady = 3)

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

		##############################################

		frameSHP <- ttklabelframe(subfr5, text = "Boundaries", relief = 'groove')

		EnvCessationCalcPlot$shp$add.shp <- tclVar(FALSE)
		file.plotShp <- tclVar()
		stateSHP <- "disabled"

		chk.addshp <- tkcheckbutton(frameSHP, variable = EnvCessationCalcPlot$shp$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
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
				if(is.null(shpofile)) EnvCessationCalcPlot$shp$ocrds <- NULL
				EnvCessationCalcPlot$shp$ocrds <- getBoundaries(shpofile[[2]])
			}else return(NULL)
		})

		########
		EnvCessationCalcPlot$SHPOp <- list(col = "black", lwd = 1.5)

		tkconfigure(bt.addshpOpt, command = function(){
			EnvCessationCalcPlot$SHPOp <- climatoAnalysis.GraphOptions.LineSHP(main.win, EnvCessationCalcPlot$SHPOp)
		})

		########
		tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
		tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		#################
		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile)) EnvCessationCalcPlot$shp$ocrds <- NULL
			EnvCessationCalcPlot$shp$ocrds <- getBoundaries(shpofile[[2]])
		})

		tkbind(chk.addshp, "<Button-1>", function(){
			stateSHP <- if(tclvalue(EnvCessationCalcPlot$shp$add.shp) == "1") "disabled" else "normal"
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

		if(EnvCessationCalcPlot$output$params$data.type == "cdtstation"){
			stnIDTSPLOT <- EnvCessationCalcPlot$output$data$id
			txt.stnSel <- tklabel(frTS2, text = "Select a station to plot", anchor = 'w', justify = 'left')
			txt.stnID <- tklabel(frTS2, text = "Station", anchor = 'e', justify = 'right')
			cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = EnvCessationCalcPlot$graph$stnIDTSp, width = largeur4)
			tclvalue(EnvCessationCalcPlot$graph$stnIDTSp) <- stnIDTSPLOT[1]

			tkgrid(txt.stnSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.stnID, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}else{
			txt.crdSel <- tklabel(frTS2, text = "Enter longitude and latitude to plot", anchor = 'w', justify = 'left')
			txt.lonLoc <- tklabel(frTS2, text = "Longitude", anchor = 'e', justify = 'right')
			en.lonLoc <- tkentry(frTS2, textvariable = EnvCessationCalcPlot$graph$lonLOC, width = 8)
			txt.latLoc <- tklabel(frTS2, text = "Latitude", anchor = 'e', justify = 'right')
			en.latLoc <- tkentry(frTS2, textvariable = EnvCessationCalcPlot$graph$latLOC, width = 8)
			stnIDTSPLOT <- ""
			tclvalue(EnvCessationCalcPlot$graph$stnIDTSp) <- ""

			tkgrid(txt.crdSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.lonLoc, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.lonLoc, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.latLoc, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.latLoc, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)
		return(0)
	}

	set.Data.Dates <- function(){
		donDates <- format(EnvCessationCalcPlot$output$start.date, "%Y")
		tkconfigure(cb.data.Index, values = donDates)
		tclvalue(EnvCessationCalcPlot$donDate) <- donDates[length(donDates)]
		return(0)
	}

	#######################################################################################################

	EnvCessationCalcPlot$read.Data.Map <- function(){
		tkconfigure(main.win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(main.win, cursor = '')
			tcl('update')
		})

		this.daty <- str_trim(tclvalue(EnvCessationCalcPlot$donDate))
		idt <- which(format(EnvCessationCalcPlot$output$start.date, "%Y") == this.daty)

		if(EnvCessationCalcPlot$output$params$data.type == "cdtstation"){
			filePathData <- file.path(EnvCessationCalcPlot$PathData, "CDTDATASET/CESSATION.rds")
			if(!file.exists(filePathData)){
				InsertMessagesTxt(main.txt.out, paste(filePathData, 'not found'), format = TRUE)
				return(NULL)
			}

			readVarData <- TRUE
			if(!is.null(EnvCessationCalcPlot$varData))
				if(!is.null(EnvCessationCalcPlot$filePathData))
					if(EnvCessationCalcPlot$filePathData == filePathData) readVarData <- FALSE

			if(readVarData){
				EnvCessationCalcPlot$varData$data <- readRDS(filePathData)
				EnvCessationCalcPlot$filePathData <- filePathData
			}

			########
			rasterVarData <- TRUE
			if(!rasterVarData)
				if(!is.null(EnvCessationCalcPlot$varData$rasterDate))
					if(EnvCessationCalcPlot$filePathData == filePathData)
						if(EnvCessationCalcPlot$varData$rasterDate == this.daty) rasterVarData <- FALSE

			if(rasterVarData){
				nx <- nx_ny_as.image(diff(range(EnvCessationCalcPlot$output$data$lon)))
				ny <- nx_ny_as.image(diff(range(EnvCessationCalcPlot$output$data$lat)))
				tmp <- as.numeric(EnvCessationCalcPlot$varData$data[idt, ] - EnvCessationCalcPlot$output$start.date[idt])

				tmp <- as.image(tmp, nx = nx, ny = ny,
								x = cbind(EnvCessationCalcPlot$output$data$lon, EnvCessationCalcPlot$output$data$lat))
				EnvCessationCalcPlot$varData$map$x <- tmp$x
				EnvCessationCalcPlot$varData$map$y <- tmp$y
				EnvCessationCalcPlot$varData$map$z <- tmp$z
				EnvCessationCalcPlot$varData$rasterDate <- this.daty
				rm(tmp)
			}
		}else{
			filePathData <- file.path(EnvCessationCalcPlot$PathData, "DATA_NetCDF",
							paste0("cessation_", format(EnvCessationCalcPlot$output$start.date[idt], "%Y%m%d"), ".nc"))
			if(!file.exists(filePathData)){
				InsertMessagesTxt(main.txt.out, paste(filePathData, 'not found'), format = TRUE)
				return(NULL)
			}

			readVarData <- TRUE
			if(!is.null(EnvCessationCalcPlot$varData))
				if(!is.null(EnvCessationCalcPlot$filePathData))
					if(EnvCessationCalcPlot$filePathData == filePathData) readVarData <- FALSE

			if(readVarData){
				nc <- nc_open(filePathData)
				EnvCessationCalcPlot$varData$map$x <- nc$dim[[1]]$vals
				EnvCessationCalcPlot$varData$map$y <- nc$dim[[2]]$vals
				EnvCessationCalcPlot$varData$map$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
				nc_close(nc)
				EnvCessationCalcPlot$filePathData <- filePathData
			}

			###################

			file.CDT.Idx <- file.path(EnvCessationCalcPlot$PathData, "CDTDATASET/CDTDATASET.rds")

			read.cdt.dataIdx<- TRUE
			if(!is.null(EnvCessationCalcPlot$cdtdataset))
				if(!is.null(EnvCessationCalcPlot$file.CDT.Idx))
					if(EnvCessationCalcPlot$file.CDT.Idx == file.CDT.Idx) read.cdt.dataIdx <- FALSE
			if(read.cdt.dataIdx){
				EnvCessationCalcPlot$cdtdataset <- readRDS(file.CDT.Idx)
				EnvCessationCalcPlot$cdtdataset$fileInfo <- file.CDT.Idx
				EnvCessationCalcPlot$file.CDT.Idx <- file.CDT.Idx
			}
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



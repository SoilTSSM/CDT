

CessationCalcPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(Sys.info()["sysname"] == "Windows"){
		wscrlwin <- w.scale(26)
		hscrlwin <- h.scale(45)
		hscrlwin1 <- h.scale(27)
		largeur0 <- as.integer(w.scale(22)/sfont0)
		largeur1 <- as.integer(w.scale(29)/sfont0)
		largeur2 <- as.integer(w.scale(31)/sfont0)

		largeur3 <- 45
		# largeur4 <- largeur1-5
		# largeur5 <- 30
		# largeur6 <- 22
	}else{
		wscrlwin <- w.scale(27)
		hscrlwin <- h.scale(50)
		hscrlwin1 <- h.scale(28)
		largeur0 <- as.integer(w.scale(20)/sfont0)
		largeur1 <- as.integer(w.scale(21)/sfont0)
		largeur2 <- as.integer(w.scale(23)/sfont0)

		largeur3 <- 35
		# largeur4 <- largeur1
		# largeur5 <- 22
		# largeur6 <- 14
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

					# load.ClimatoAnalysis.Data()

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

		frameOnsetDat <- ttklabelframe(subfr3, text = "Cessation data", relief = 'groove')

		EnvCessationCalcPlot$DirExist <- tclVar(0)
		file.OnsetIndex <- tclVar()

		stateOnsetDat <- if(tclvalue(EnvCessationCalcPlot$DirExist) == "1") "normal" else "disabled"

		chk.OnsetIdx <- tkcheckbutton(frameOnsetDat, variable = EnvCessationCalcPlot$DirExist, text = "Cessation data already computed", anchor = 'w', justify = 'left')
		en.OnsetIdx <- tkentry(frameOnsetDat, textvariable = file.OnsetIndex, width = largeur2, state = stateOnsetDat)
		bt.OnsetIdx <- tkbutton(frameOnsetDat, text = "...", state = stateOnsetDat)

		tkconfigure(bt.OnsetIdx, command = function(){

		})


		tkgrid(chk.OnsetIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.OnsetIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.OnsetIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(chk.OnsetIdx, "<Button-1>", function(){
			stateOnsetDat <- if(tclvalue(EnvCessationCalcPlot$DirExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.OnsetIdx, state = stateOnsetDat)
			tkconfigure(bt.OnsetIdx, state = stateOnsetDat)
			stateCaclBut <- if(tclvalue(EnvCessationCalcPlot$DirExist) == '1') 'normal' else 'disabled'
			tkconfigure(bt.CalcOnset, state = stateCaclBut)
		})

		##############################################

		frameOnsetMap <- ttklabelframe(subfr3, text = "Cessation Map", relief = 'groove')



		##############################################

		frameOnsetTS <- ttklabelframe(subfr3, text = "Cessation Graph", relief = 'groove')



		##############################################

		tkgrid(frameOnsetDat, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameOnsetMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameOnsetTS, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)


	#######################################################################################################
	tcl('update')
	tkgrid(cmd.frame, sticky = '', pady = 1)
	tkgrid.columnconfigure(cmd.frame, 0, weight = 1)
	######
	return(cmd.frame)
}


